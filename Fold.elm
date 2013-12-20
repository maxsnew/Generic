module Fold where

import open Monoid
import open Semigroup

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
fold m = foldMap m id

{-| A more general version of fold that's more common in  

    If elm gets kinds this could be generalized further.
-}
foldMap : Monoid m -> (a -> m) -> [a] -> m
foldMap m f = let e    = m.id
                  (<>) = m.op
              in foldr (\x m -> f x <> m) e

-- | Semigroupie Folds
fold1 : Semigroup s -> [s] -> s
fold1 s = foldMap1 s id

foldMap1 : Semigroup s -> (a -> s) -> [a] -> s
foldMap1 s f = foldr1 s.op . map f
