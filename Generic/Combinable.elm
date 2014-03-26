module Generic.Combinable where

{-| A `Combinable` is a type `a` with a "combining" operation `op : a -> a -> a`
    that is associative, i.e.,

```haskell
c.op x (c.op y z) = c.op (c.op x y) z
```

This concept is known in mathematics as a `Semigroup`

# Plain Record Type
@docs Combinable

# Choose
@docs first, last, max, min, maxDefault, minDefault

# Merge
@docs dictInter, setInter, sig

# Transform
@docs flip
-}

import Basics
import Set
import Set (..)
import Dict
import Dict (..)

{-| A type with an associative operation. -}
type Combinable r c = { r | op : c -> c -> c }

{-| Take the first of two things
```haskell
first.op x y = x
```
-}
first : Combinable {} a
first = { op x y = x }

{-| Take the second of two things -}
last : Combinable {} a
last = flip first

{-| Take the greater of two things that can be compared or the left, if EQ -}
max : (a -> a -> Order) -> Combinable {} a
max comp = { op x y = case comp x y of
                LT -> y
                _  -> x
           }

{-| Take the greater using the default compare operation -}
maxDefault : Combinable {} comparable
maxDefault = max compare

{-| Take the lesser of two things that can be compared or the left, if EQ -}
min : (a -> a -> Order) -> Combinable {} a
min comp = { op x y = case comp x y of
                GT -> y
                _  -> x
           }

{-| Take the greater using the default compare operation -}
minDefault : Combinable {} comparable
minDefault = min compare

{-| The intersection of two sets -}
setInter : Combinable {} (Set comparable)
setInter = { op = Set.intersect }

{-| The intersection of two dictionaries, keeping 
    the values from the left dictionary.
-}
dictInter : Combinable {} (Dict comparable a)
dictInter = { op = Dict.intersect }

{-| Merge two `Signal`s -}
sig : Combinable {} (Signal a)
sig = { op = merge }

{-| Flip the operation of a combinable. 
```haskell
last = flip first
```
-}
flip : Combinable r a -> Combinable r a
flip a = { a | op <- Basics.flip a.op }
