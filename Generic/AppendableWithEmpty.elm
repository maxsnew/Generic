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
unitAppE : AppendableWithEmpty {} ()
unitAppE = { id       = ()
           , op () () = ()
           }

anyAppE : AppendableWithEmpty {} Bool
anyAppE = { id = False
          , op = (||)
          }

allAppE : AppendableWithEmpty {} Bool
allAppE = { id = True
          , op = (&&)
          }

stringAppE : AppendableWithEmpty {} String
stringAppE = { id = ""
             , op = String.append
             }

sumAppE : AppendableWithEmpty {} number
sumAppE = { id = 0
          , op = (+)
          }

prodAppE : AppendableWithEmpty {} number
prodAppE = { id = 1
           , op = (*)
           }

-- Lexicographic Ordering
ordAppE : AppendableWithEmpty {} Order
ordAppE = { id = EQ
          , op o1 o2 = case o1 of
            EQ -> o2
            _  -> o1
          }

-- Remove if this gets added to the stdlib
emptyEl : Element.Element
emptyEl = Element.spacer 0 0 

aboveAppE : AppendableWithEmpty {} Element.Element
aboveAppE = { id = emptyEl
            , op = above
            }

besideAppE : AppendableWithEmpty {} Element.Element
besideAppE = { id = emptyEl
             , op = beside
             }

behindAppE : AppendableWithEmpty {} Element.Element
behindAppE = { id = emptyEl
             , op e1 e2 = Element.layers [e1, e2]
             }

-- | Polymorphic
transAppE : AppendableWithEmpty {} (a -> a)
transAppE = { id = id
            , op = (.)
            }

listAppE : AppendableWithEmpty {} [a]
listAppE = { id = []
           , op = (++)
           }

dictAppE : AppendableWithEmpty {} (Dict.Dict comparable a)
dictAppE = { id = Dict.empty
           , op = Dict.union
           }

firstAppE : AppendableWithEmpty {} (Maybe a)
firstAppE = semiM firstSG

lastAppE : AppendableWithEmpty {} (Maybe a)
lastAppE = semiM lastSG

-- | Bounded Polymorphic
maxAppE : AppendableWithEmpty {} (Maybe comparable)
maxAppE = semiAppE maxSG

minAppE : AppendableWithEmpty {} (Maybe comparable)
minAppE = semiAppE minSG

setAppE : AppendableWithEmpty {} (Set.Set comparable)
setAppE = { id = Set.empty
          , op = Set.union
          }

pairAppE : AppendableWithEmpty r1 a -> AppendableWithEmpty r2 b -> AppendableWithEmpty {} (a, b)
pairAppE m1 m2 = let op (x1, y1) (x2, y2) = (m1.op x1 x2, m2.op y1 y2) in
  { id   = (m1.id, m2.id)
  , op = op
  }

sigAppE : AppendableWithEmpty r a -> AppendableWithEmpty {} (Signal a)
sigAppE m = { id = constant m.id
            , op = \s1 s2 -> m.op <~ s1 ~ s2
            }

semiAppE : Appendable r g -> AppendableWithEmpty {} (Maybe g)
semiAppE g = { id = Nothing 
             , op x y = case (x, y) of
               (Nothing, _) -> y
               (_, Nothing) -> x
               (Just x', Just y') -> Just (g.op x' y')
             }
