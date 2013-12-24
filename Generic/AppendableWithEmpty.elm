module Generic.AppendableWithEmpty where

import open Generic.Appendable

import Graphics.Element as Element
import Dict
import String
import Set

  -- AppendableWithEmpty laws:
type AppendableWithEmpty r m = Appendable { r | empty : m } m

-- | Instances                
-- Monomorphic
unitAppE : AppendableWithEmpty {} ()
unitAppE = { empty = ()
           , append () () = ()
           }

anyAppE : AppendableWithEmpty {} Bool
anyAppE = { empty  = False
          , append = (||)
          }

allAppE : AppendableWithEmpty {} Bool
allAppE = { empty  = True
          , append = (&&)
          }

stringAppE : AppendableWithEmpty {} String
stringAppE = { empty  = ""
             , append = String.append
             }

sumAppE : AppendableWithEmpty {} number
sumAppE = { empty  = 0
          , append = (+)
          }

prodAppE : AppendableWithEmpty {} number
prodAppE = { empty  = 1
           , append = (*)
           }

-- Lexicographic Ordering
ordAppE : AppendableWithEmpty {} Order
ordAppE = { empty = EQ
          , append o1 o2 = case o1 of
            EQ -> o2
            _  -> o1
          }

-- Remove if this gets added to the stdlib
emptyEl : Element.Element
emptyEl = Element.spacer 0 0 

aboveAppE : AppendableWithEmpty {} Element.Element
aboveAppE = { empty  = emptyEl
            , append = above
            }

besideAppE : AppendableWithEmpty {} Element.Element
besideAppE = { empty  = emptyEl
             , append = beside
             }

behindAppE : AppendableWithEmpty {} Element.Element
behindAppE = { empty = emptyEl
             , append e1 e2 = Element.layers [e1, e2]
             }

-- | Polymorphic
transAppE : AppendableWithEmpty {} (a -> a)
transAppE = { empty  = id
            , append = (.)
            }

listAppE : AppendableWithEmpty {} [a]
listAppE = { empty  = []
           , append = (++)
           }

dictAppE : AppendableWithEmpty {} (Dict.Dict comparable a)
dictAppE = { empty  = Dict.empty
           , append = Dict.union
           }

firstAppE : AppendableWithEmpty {} (Maybe a)
firstAppE = semiAppE firstApp

lastAppE : AppendableWithEmpty {} (Maybe a)
lastAppE = semiAppE lastApp

-- | Bounded Polymorphic
maxAppE : AppendableWithEmpty {} (Maybe comparable)
maxAppE = semiAppE maxApp

minAppE : AppendableWithEmpty {} (Maybe comparable)
minAppE = semiAppE minApp

setAppE : AppendableWithEmpty {} (Set.Set comparable)
setAppE = { empty = Set.empty
          , append = Set.union
          }

pairAppE : AppendableWithEmpty r1 a -> AppendableWithEmpty r2 b -> AppendableWithEmpty {} (a, b)
pairAppE m1 m2 = let append (x1, y1) (x2, y2) = (m1.append x1 x2, m2.append y1 y2) in
  { empty   = (m1.empty, m2.empty)
  , append = append
  }

sigAppE : AppendableWithEmpty r a -> AppendableWithEmpty {} (Signal a)
sigAppE m = { empty = constant m.empty
            , append = \s1 s2 -> m.append <~ s1 ~ s2
            }

semiAppE : Appendable r g -> AppendableWithEmpty {} (Maybe g)
semiAppE g = { empty = Nothing 
             , append x y = case (x, y) of
               (Nothing, _) -> y
               (_, Nothing) -> x
               (Just x', Just y') -> Just (g.append x' y')
             }
