import Dict
import String
import Set

-- Monoid laws:
type Monoid m = { id : m
                , op : m -> m -> m
                }

-- | Monoidal Folds
foldMap : Monoid m -> [a] -> (a -> m) -> m
foldMap m xs f = let e    = m.id
                     (<>) = m.op
                 in foldr (\x m -> f x <> m) e xs

fold : Monoid m -> [m] -> m
fold m xs = foldMap m xs id

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

-- | Bounded Polymorphic
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
