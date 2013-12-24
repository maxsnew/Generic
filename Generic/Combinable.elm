module Generic.Combinable where

import Basics
import Set
import open Set
import Dict
import open Dict

{-| A type that has an associative operation for combining values 
  
    Combinable law: 
```
c.op x (c.op y z) = c.op (c.op x y) z
```
-}
type Combinable r c = { r | op : c -> c -> c }

first : Combinable {} a
first = { op x y = x }

last : Combinable {} a
last = flip first

max : Combinable {} comparable
max = { op = Basics.max }

min : Combinable {} comparable
min = { op = Basics.min }

dictInter : Combinabale {} (Dict comparable a)
dictInter = { op = Dict.intersect }

setInter : Combinable {} (Set comparable)
setInter = { op = Set.intersect }

sig : Combinable {} (Signal a)
sig = { op = merge }

-- Make a Combinable whose operation is the flipped version of another
-- Combinable
flip : Combinable r a -> Combinable r a
flip a = { a | op <- Basics.flip a.op }
