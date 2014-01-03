module Generic.Appendable where

{-| An `Appendable` is a `Combinable` type `a` with an "empty" value
of that type `empty : a`, subject to some rules.

When defining your own `Appendable` `a`, your definition should obey
the following rules:

1. Identity: The `empty` element is the identity for the operation.
```haskell
a.op x a.empty == a.op a.empty x == x
```
2. Associativity: The operation is associative.
```haskell
a.op x (a.op y z) == a.op (a.op x y) z
```

If you are writing a function that uses an `Appendable`, you may
be able to use the above rules to optimize your implementation.

This concept is known in mathematics as a `Monoid`.

# Plain Record Type
@docs Appendable

# Numbers
@docs sum, prod

# Collections
@docs string, list, set, dict

# Elements
@docs above, below, beside, behind, inFront

# Finite Types
@docs unit, or, and, ord

# Transformations
@docs trans, pair, sig

# From Combinable to Appendable
@docs addEmpty, first, last, max, min, maxDefault, minDefault
-}

import open Generic.Combinable
import Generic.Combinable as Combinable

import Graphics.Element as Element
import Dict
import String
import Set

{-| An `Appendable` is also a `Combinable` so it can be used with any function that 
works on `Combinable`s. 
-}
type Appendable r a = Combinable { r | empty : a } a

-- | Instances                
-- Monomorphic

{-| The trivial `Appendable` -}
unit : Appendable {} ()
unit = { empty = ()
       , op () () = ()
       }

{-| `(||)` with empty: `False` -}
or : Appendable {} Bool
or = { empty  = False
     , op = (||)
     }

{-| `(&&)` with empty: True -}
and : Appendable {} Bool
and = { empty  = True
      , op = (&&)
      }

{-| String append with the empty string  -}
string : Appendable {} String
string = { empty  = ""
         , op = String.append
         }

{-| Addition with 0 as empty -}
sum : Appendable {} number
sum = { empty  = 0
      , op = (+)
      }

{-| Multiplication with 1 as empty -}
prod : Appendable {} number
prod = { empty  = 1
       , op = (*)
       }

{-| Lexicographical ordering. If the first is LT (GT) the whole thing
is LT (GT), otherwise look at what the next ordering is. empty is EQ.  
-}
ord : Appendable {} Order
ord = { empty = EQ
      , op o1 o2 = case o1 of
        EQ -> o2
        _  -> o1
      }

-- Remove if this gets added to the stdlib
emptyEl : Element.Element
emptyEl = Element.spacer 0 0 

{-| Place the first element above the second -}
above : Appendable {} Element.Element
above = { empty  = emptyEl
        , op = Element.above
        }

{-| Place the first element below the second -}
below : Appendable {} Element.Element
below = Combinable.flip above

{-| Place the first element to the left of the second -}
beside : Appendable {} Element.Element
beside = { empty  = emptyEl
         , op = Element.beside
         }

{-| Place the first element behind the second -}
behind : Appendable {} Element.Element
behind = { empty = emptyEl
         , op e1 e2 = Element.layers [e1, e2]
         }

{-| Place the first element in front of the second -}
inFront : Appendable {} Element.Element
inFront = Combinable.flip behind

-- Polymorphic

{-| Compose functions with the identity function as empty -}
trans : Appendable {} (a -> a)
trans = { empty  = id
        , op = (.)
        }

{-| Append lists with the empty list as empty -}
list : Appendable {} [a]
list = { empty  = []
       , op = (++)
       }

{-| Union dictionaries with the empty dictionary as empty.  -}
dict : Appendable {} (Dict.Dict comparable a)
dict = { empty  = Dict.empty
       , op = Dict.union
       }

-- dictA : Appendable r a -> Appendable {} (Dict.Dict comparable a)
-- dictA = { empty = Dict.empty
--         , op    = 
--         }

{-| Take the first non-Nothing value with Nothing as empty -}
first : Appendable {} (Maybe a)
first = addEmpty Combinable.first

{-| Take the last non-Nothing value with Nothing as empty -}
last : Appendable {} (Maybe a)
last = addEmpty Combinable.last

-- | Bounded Polymorphic

max : (a -> a -> Order) -> Appendable {} (Maybe a)
max = addEmpty . Combinable.max

maxDefault : Appendable {} (Maybe comparable)
maxDefault = addEmpty Combinable.maxDefault

min : (a -> a -> Order) -> Appendable {} (Maybe a)
min = addEmpty . Combinable.min

minDefault : Appendable {} (Maybe comparable)
minDefault = addEmpty Combinable.minDefault

{-| Sets with Set union and the empty Set as empty. -}
set : Appendable {} (Set.Set comparable)
set = { empty = Set.empty
      , op = Set.union
      }

{-| Append on both sides of a pair -}
pair : Appendable r1 a -> Appendable r2 b -> Appendable {} (a, b)
pair m1 m2 = let op (x1, y1) (x2, y2) = (m1.op x1 x2, m2.op y1 y2) in
  { empty   = (m1.empty, m2.empty)
  , op = op
  }

{-| Append the values of two signals of Appendables -}
sig : Appendable r a -> Appendable {} (Signal a)
sig m = { empty = constant m.empty
        , op = \s1 s2 -> m.op <~ s1 ~ s2
        }

{-| Make any Combinable an Appendable by adding Nothing as the empty element. -}
addEmpty : Combinable r a -> Appendable {} (Maybe a)
addEmpty c = { empty = Nothing 
             , op x y = case (x, y) of
               (Nothing, _) -> y
               (_, Nothing) -> x
               (Just x', Just y') -> Just (c.op x' y')
             }
