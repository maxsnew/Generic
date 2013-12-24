module Generic.Fold where

import open Generic.AppendableWithEmpty
import open Generic.Appendable

-- | AppendableWithEmptyal Folds
{-| Generalizes tons of common functions:

sum : [number] -> number
sum = fold sumAppE

product : [number] -> number
product = fold prodAppE

and : [Bool] -> Bool
and = fold allAppE

or : [Bool] -> Bool
or = fold anyAppE

flow dir     : [Element] -> Element
flow down    = fold aboveAppE
flow right   = fold besideAppE
flow outward = fold behindAppE

unions : [Set comparable] -> Set comparable
unions = fold setAppE

hashUnions : [Dict comparable v] -> Dict comparable v
hashUnions = fold dictAppE

combine : [Signal a] -> Signal [a]
combine = fold (sigAppE listAppE)

If you write a new data structure and you want to implement a function
that looks like the above functions, see if it's a monoid and you
might just get the implementation for free!

Also, if your data structure has a natural fold operation, consider
implementing foldMap with your corresponding type instead of []. You
don't even need to depend on this library to do so, because
AppendableWithEmptys/Appendables are just records!

-}
fold : AppendableWithEmpty r m -> [m] -> m
fold m = foldMap m id

{-| A more general version of fold that's more common in specific uses.

    If elm gets kinds this could be generalized over things besides
    lists.

-}
foldMap : AppendableWithEmpty r m -> (a -> m) -> [a] -> m
foldMap m f = let e    = m.id
                  (<>) = m.op
              in foldr (\x m -> f x <> m) e

{-| Accumulates the values of a Signal using a monoid.
    
    count : Signal a -> Signal Int    
    count = accumWith sumAppE (\_ -> 1)

    countIf : (a -> Bool) -> Signal a -> Signal Int
    countIf p = accumWith sumAppE (\x -> if p x then 1 else 0)

    remember : Signal a -> Signal [a]
    remember = accumWith listAppE (\x -> [x])

-}
accumWith : AppendableWithEmpty r m -> (a -> m) -> Signal a -> Signal m
accumWith m f = foldp (m.op . f) m.id

-- | Appendableie Folds
{-| Generalizes some other useful functions:
maximum : [comparable] -> comparable
maximum = fold1 maxApp

minimum : [comparable] -> comparable
minimum = fold1 minApp

head : [a] -> a
head = fold1 firstApp

last : [a] -> a
last = fold1 lastApp

merges : [Signal a] -> Signal a
merges = fold1 sigApp

-}
fold1 : Appendable r s -> [s] -> s
fold1 s = foldMap1 s id

foldMap1 : Appendable r s -> (a -> s) -> [a] -> s
foldMap1 s f = foldr1 s.op . map f
