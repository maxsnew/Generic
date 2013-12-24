module Generic.AppendableWithEmpty where

import open Generic.Appendable

import Graphics.Element as Element
import Dict
import String
import Set

-- AppendableWithEmpty laws:
type AppendableWithEmpty r m = Appendable { r | id : m } m

-- | Instances                
-- Monomorphic
unitM : AppendableWithEmpty {} ()
unitM = { id       = ()
        , op () () = ()
        }

anyM : AppendableWithEmpty {} Bool
anyM = { id = False
       , op = (||)
       }

allM : AppendableWithEmpty {} Bool
allM = { id = True
       , op = (&&)
       }

stringM : AppendableWithEmpty {} String
stringM = { id = ""
          , op = String.append
          }

sumM : AppendableWithEmpty {} number
sumM = { id = 0
       , op = (+)
       }

prodM : AppendableWithEmpty {} number
prodM = { id = 1
        , op = (*)
        }

-- Lexicographic Ordering
ordM : AppendableWithEmpty {} Order
ordM = { id = EQ
       , op o1 o2 = case o1 of
         EQ -> o2
         _  -> o1
       }

-- Remove if this gets added to the stdlib
emptyEl : Element.Element
emptyEl = Element.spacer 0 0 

aboveM : AppendableWithEmpty {} Element.Element
aboveM = { id = emptyEl
         , op = above
         }

besideM : AppendableWithEmpty {} Element.Element
besideM = { id = emptyEl
          , op = beside
          }

behindM : AppendableWithEmpty {} Element.Element
behindM = { id = emptyEl
          , op e1 e2 = Element.layers [e1, e2]
          }

-- | Polymorphic
transM : AppendableWithEmpty {} (a -> a)
transM = { id = id
         , op = (.)
         }

listM : AppendableWithEmpty {} [a]
listM = { id = []
        , op = (++)
        }

dictM : AppendableWithEmpty {} (Dict.Dict comparable a)
dictM = { id = Dict.empty
        , op = Dict.union
        }

firstM : AppendableWithEmpty {} (Maybe a)
firstM = semiM firstSG

lastM : AppendableWithEmpty {} (Maybe a)
lastM = semiM lastSG

-- | Bounded Polymorphic
maxM : AppendableWithEmpty {} (Maybe comparable)
maxM = semiM maxSG

minM : AppendableWithEmpty {} (Maybe comparable)
minM = semiM minSG

setM : AppendableWithEmpty {} (Set.Set comparable)
setM = { id = Set.empty
       , op = Set.union
       }

pairM : AppendableWithEmpty r1 a -> AppendableWithEmpty r2 b -> AppendableWithEmpty {} (a, b)
pairM m1 m2 = let op (x1, y1) (x2, y2) = (m1.op x1 x2, m2.op y1 y2) in
  { id   = (m1.id, m2.id)
  , op = op
  }

sigM : AppendableWithEmpty r a -> AppendableWithEmpty {} (Signal a)
sigM m = { id = constant m.id
         , op = \s1 s2 -> m.op <~ s1 ~ s2
         }

semiM : Appendable r g -> AppendableWithEmpty {} (Maybe g)
semiM g = { id = Nothing 
          , op x y = case (x, y) of
            (Nothing, _) -> y
            (_, Nothing) -> x
            (Just x', Just y') -> Just (g.op x' y')
            }
