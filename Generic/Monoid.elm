module Generic.Monoid where

import open Generic.Semigroup

import Graphics.Element as Element
import Dict
import String
import Set

-- Monoid laws:
type Monoid m = { id : m
                , op : m -> m -> m
                }

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

-- Lexicographic Ordering
ordM : Monoid Order
ordM = { id = EQ
       , op o1 o2 = case o1 of
         EQ -> o2
         _  -> o1
       }

-- Remove if this gets added to the stdlib
emptyEl : Element.Element
emptyEl = Element.spacer 0 0 

aboveM : Monoid Element.Element
aboveM = { id = emptyEl
         , op = above
         }

besideM : Monoid Element.Element
besideM = { id = emptyEl
          , op = beside
          }

behindM : Monoid Element.Element
behindM = { id = emptyEl
          , op e1 e2 = Element.layers [e1, e2]
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
firstM = semiM firstSG

lastM : Monoid (Maybe a)
lastM = semiM lastSG

-- | Bounded Polymorphic
maxM : Monoid (Maybe comparable)
maxM = semiM maxSG

minM : Monoid (Maybe comparable)
minM = semiM minSG

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
