module Generic.AppendableWithEmpty where

import open Generic.Appendable
import Generic.Appendable as Appendable

import Graphics.Element as Element
import Dict
import String
import Set

-- AppendableWithEmpty laws:
type AppendableWithEmpty r a = Appendable { r | empty : a } a

-- | Instances                
-- Monomorphic
unit : AppendableWithEmpty {} ()
unit = { empty = ()
       , append () () = ()
       }

any : AppendableWithEmpty {} Bool
any = { empty  = False
      , append = (||)
      }

all : AppendableWithEmpty {} Bool
all = { empty  = True
      , append = (&&)
      }

string : AppendableWithEmpty {} String
string = { empty  = ""
         , append = String.append
         }

sum : AppendableWithEmpty {} number
sum = { empty  = 0
      , append = (+)
      }

prod : AppendableWithEmpty {} number
prod = { empty  = 1
       , append = (*)
       }

-- Lexicographic Ordering
ord : AppendableWithEmpty {} Order
ord = { empty = EQ
      , append o1 o2 = case o1 of
        EQ -> o2
        _  -> o1
      }

-- Remove if this gets added to the stdlib
emptyEl : Element.Element
emptyEl = Element.spacer 0 0 

above : AppendableWithEmpty {} Element.Element
above = { empty  = emptyEl
        , append = Element.above
        }

beside : AppendableWithEmpty {} Element.Element
beside = { empty  = emptyEl
         , append = Element.beside
         }

behind : AppendableWithEmpty {} Element.Element
behind = { empty = emptyEl
         , append e1 e2 = Element.layers [e1, e2]
         }

-- | Polymorphic
trans : AppendableWithEmpty {} (a -> a)
trans = { empty  = id
        , append = (.)
        }

list : AppendableWithEmpty {} [a]
list = { empty  = []
       , append = (++)
       }

dict : AppendableWithEmpty {} (Dict.Dict comparable a)
dict = { empty  = Dict.empty
       , append = Dict.union
       }

first : AppendableWithEmpty {} (Maybe a)
first = addEmpty Appendable.first

last : AppendableWithEmpty {} (Maybe a)
last = addEmpty Appendable.last

-- | Bounded Polymorphic
max : AppendableWithEmpty {} (Maybe comparable)
max = addEmpty Appendable.max

min : AppendableWithEmpty {} (Maybe comparable)
min = addEmpty Appendable.min

set : AppendableWithEmpty {} (Set.Set comparable)
set = { empty = Set.empty
      , append = Set.union
      }

pair : AppendableWithEmpty r1 a -> AppendableWithEmpty r2 b -> AppendableWithEmpty {} (a, b)
pair m1 m2 = let append (x1, y1) (x2, y2) = (m1.append x1 x2, m2.append y1 y2) in
  { empty   = (m1.empty, m2.empty)
  , append = append
  }

sig : AppendableWithEmpty r a -> AppendableWithEmpty {} (Signal a)
sig m = { empty = constant m.empty
            , append = \s1 s2 -> m.append <~ s1 ~ s2
            }

addEmpty : Appendable r a -> AppendableWithEmpty {} (Maybe a)
addEmpty a = { empty = Nothing 
             , append x y = case (x, y) of
               (Nothing, _) -> y
               (_, Nothing) -> x
               (Just x', Just y') -> Just (a.append x' y')
             }
