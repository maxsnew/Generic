module Generic.Fold where

import open Generic.Combinable
import open Generic.Appendable

-- | Appendable Folds
{-| Generalizes tons of common functions:

sum : [number] -> number
sum = fold App.sum

product : [number] -> number
product = fold App.prod

and : [Bool] -> Bool
and = fold App.all

or : [Bool] -> Bool
or = fold App.any

flow dir     : [Element] -> Element
flow down    = fold App.above
flow right   = fold App.beside
flow outward = fold App.behind

unions : [Set comparable] -> Set comparable
unions = fold App.set

hashUnions : [Dict comparable v] -> Dict comparable v
hashUnions = fold App.dict

combine : [Signal a] -> Signal [a]
combine = fold (App.sig App.list)

If you write a new data structure and you want to implement a function
that looks like the above functions, see if it's an Appendable and you
might just get the implementation for free!

Also, if your data structure has a polymorphic container type,
consider implementing foldMap with your corresponding type instead of
[]. You don't even need to depend on this library to do so, because
Appendable and Combinable are just records!

-}
fold : Appendable r m -> [m] -> m
fold m = foldMap m id

{-| A more general version of fold that's more common in specific uses.

    If elm gets kinds this could be generalized over things besides
    lists.

-}
foldMap : Appendable r m -> (a -> m) -> [a] -> m
foldMap app f xs = case xs of 
  [] -> app.empty
  _  -> foldMap1 app f xs

{-| Accumulates the values of a Signal using a monoid.
    
    count : Signal a -> Signal Int    
    count = accumWith sumAppE (\_ -> 1)

    countIf : (a -> Bool) -> Signal a -> Signal Int
    countIf p = accumWith sumAppE (\x -> if p x then 1 else 0)

    remember : Signal a -> Signal [a]
    remember = accumWith listAppE (\x -> [x])

-}
accumWith : Appendable r m -> (a -> m) -> Signal a -> Signal m
accumWith a f = foldp (a.op . f) a.empty

-- | Combinable Folds
{-| Generalizes some other useful functions:
maximum : [comparable] -> comparable
maximum = fold1 Comb.max

minimum : [comparable] -> comparable
minimum = fold1 Comb.min

head : [a] -> a
head = fold1 Comb.first

last : [a] -> a
last = fold1 Comb.last

merges : [Signal a] -> Signal a
merges = fold1 Comb.sig

-}
fold1 : Combinable r s -> [s] -> s
fold1 c = foldMap1 c id

foldMap1 : Combinable r s -> (a -> s) -> [a] -> s
foldMap1 c f = let mergeAll xs = case xs of
                     (x :: y :: ys) -> mergeAll (c.op x y :: mergeAll ys)
                     _              -> xs
                   pass1 xs = case xs of
                     (x :: y :: ys) -> mergeAll (c.op (f x) (f y) :: pass1 ys)
                     _              -> mergeAll . map f <| xs
               in head . pass1
