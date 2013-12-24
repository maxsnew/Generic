module Generic.Appendable where

import open Generic.Combinable
import Generic.Combinable as Combinable

import Graphics.Element as Element
import Dict
import String
import Set

-- Appendable laws:
type Appendable r a = Combinable { r | empty : a } a

-- | Instances                
-- Monomorphic
unit : Appendable {} ()
unit = { empty = ()
       , op () () = ()
       }

any : Appendable {} Bool
any = { empty  = False
      , op = (||)
      }

all : Appendable {} Bool
all = { empty  = True
      , op = (&&)
      }

string : Appendable {} String
string = { empty  = ""
         , op = String.append
         }

sum : Appendable {} number
sum = { empty  = 0
      , op = (+)
      }

prod : Appendable {} number
prod = { empty  = 1
       , op = (*)
       }

-- Lexicographic Ordering
ord : Appendable {} Order
ord = { empty = EQ
      , op o1 o2 = case o1 of
        EQ -> o2
        _  -> o1
      }

-- Remove if this gets added to the stdlib
emptyEl : Element.Element
emptyEl = Element.spacer 0 0 

above : Appendable {} Element.Element
above = { empty  = emptyEl
        , op = Element.above
        }

below : Appendable {} Element.Element
below = Combinable.flip above

beside : Appendable {} Element.Element
beside = { empty  = emptyEl
         , op = Element.beside
         }

behind : Appendable {} Element.Element
behind = { empty = emptyEl
         , op e1 e2 = Element.layers [e1, e2]
         }

inFront : Appendable {} Element.Element
inFront = Combinable.flip behind

-- | Polymorphic
trans : Appendable {} (a -> a)
trans = { empty  = id
        , op = (.)
        }

list : Appendable {} [a]
list = { empty  = []
       , op = (++)
       }

dict : Appendable {} (Dict.Dict comparable a)
dict = { empty  = Dict.empty
       , op = Dict.union
       }

first : Appendable {} (Maybe a)
first = addEmpty Combinable.first

last : Appendable {} (Maybe a)
last = addEmpty Combinable.last

-- | Bounded Polymorphic
max : Appendable {} (Maybe comparable)
max = addEmpty Combinable.max

min : Appendable {} (Maybe comparable)
min = addEmpty Combinable.min

set : Appendable {} (Set.Set comparable)
set = { empty = Set.empty
      , op = Set.union
      }

pair : Appendable r1 a -> Appendable r2 b -> Appendable {} (a, b)
pair m1 m2 = let op (x1, y1) (x2, y2) = (m1.op x1 x2, m2.op y1 y2) in
  { empty   = (m1.empty, m2.empty)
  , op = op
  }

sig : Appendable r a -> Appendable {} (Signal a)
sig m = { empty = constant m.empty
        , op = \s1 s2 -> m.op <~ s1 ~ s2
        }

addEmpty : Combinable r a -> Appendable {} (Maybe a)
addEmpty c = { empty = Nothing 
             , op x y = case (x, y) of
               (Nothing, _) -> y
               (_, Nothing) -> x
               (Just x', Just y') -> Just (c.op x' y')
             }
