module Monoid where

import Dict
import String
import Set

-- Monoid laws:
type Monoid m = { id : m
                , op : m -> m -> m
                }

type Semigroup g = { op : g -> g -> g }

-- | Monoidal Folds
{-| Generalizes tons of common functions:

sum : [number] -> number
sum = fold sumM

product : [number] -> number
product = fold prodM

and : [Bool] -> Bool
and = fold allM

or : [Bool] -> Bool
or = fold anyM

join : [[a]] -> [a]
join = fold listM

unions : [Set comparable] -> Set comparable
unions = fold setM

hashUnions : [Dict comparable v] -> Dict comparable v
hashUnions = fold dictM

combine : [Signal a] -> Signal [a]
combine = fold (sigM listM)

If you write a new data structure and you want to implement a function
that looks like the above functions, see if it's a monoid and you
might just get the implementation for free!

-}
fold : Monoid m -> [m] -> m
fold m xs = foldMap m xs id

{-| A more general version of fold that's more common in  

    If elm gets kinds this could be generalized further.
-}
foldMap : Monoid m -> [a] -> (a -> m) -> m
foldMap m xs f = let e    = m.id
                     (<>) = m.op
                 in foldr (\x m -> f x <> m) e xs

-- | Instances                
-- Monomorphic
unitM : Monoid ()
unitM = { id       = ()
        , op () () = ()
        }

anyM : Monoid Bool
anyM = { id = False
       , op = (||)
       }

allM : Monoid Bool
allM = { id = True
       , op = (&&)
       }

stringM : Monoid String
stringM = { id = ""
          , op = String.append
          }

sumM : Monoid number
sumM = { id = 0
       , op = (+)
       }

prodM : Monoid number
prodM = { id = 1
        , op = (*)
        }

-- | Polymorphic
transM : Monoid (a -> a)
transM = { id = id
         , op = (.)
         }

listM : Monoid [a]
listM = { id = []
        , op = (++)
        }

dictM : Monoid (Dict.Dict comparable a)
dictM = { id = Dict.empty
        , op = Dict.union
        }

firstM : Monoid (Maybe a)
firstM = semiM { op x y = x }

lastM : Monoid (Maybe a)
lastM = semiM { op x y = y }

-- | Bounded Polymorphic
maxM : Monoid (Maybe comparable)
maxM = semiM { op = max }

minM : Monoid (Maybe comparable)
minM = semiM { op = min }

setM : Monoid (Set.Set comparable)
setM = { id = Set.empty
       , op = Set.union
       }

pairM : Monoid a -> Monoid b -> Monoid (a, b)
pairM m1 m2 = let op (x1, y1) (x2, y2) = (m1.op x1 x2, m2.op y1 y2) in
  { id   = (m1.id, m2.id)
  , op = op
  }

sigM : Monoid a -> Monoid (Signal a)
sigM m = { id   = constant m.id
         , op = \s1 s2 -> m.op <~ s1 ~ s2
         }

semiM : Semigroup g -> Monoid (Maybe g)
semiM g = { id = Nothing 
          , op x y = case (x, y) of
            (Nothing, _) -> y
            (_, Nothing) -> x
            (Just x', Just y') -> Just (g.op x' y')
            }
