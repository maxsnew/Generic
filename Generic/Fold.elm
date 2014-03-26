module Generic.Fold where

{-| The payoff for all of these `Combinable`s and `Appendable`s: Generic Folds!

# Appendable Folds
@docs fold, foldMap, accumWith

# Combinable Folds
@docs fold1, foldMap1
-}

import Generic.Combinable (..)
import Generic.Appendable (..)

{-| A generic fold over a list, using an `Appendable` operation to
combine and the empty element for an empty list.

The implementation is equivalent to the following but uses the
`Appendable` rules to minimize the calls to `a.op`:

```haskell
fold a [x,y,...,z] = a.op x (a.op y  ... (a.op z a.empty) ...)
```

This generalizes tons of common functions:

```haskell
import Generic.Appendable as App

sum : [number] -> number
sum = fold App.sum

product : [number] -> number
product = fold App.prod

and : [Bool] -> Bool
and = fold App.and

or : [Bool] -> Bool
or = fold App.or

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
```

If you write a new data structure and you want to implement a function
that looks like the above functions, see if it's an Appendable and you
might just get the implementation for free!

-}
fold : Appendable r m -> [m] -> m
fold m = foldMap m id

{-| A more general version of fold that's more common in specific uses.

If elm gets kinds this could be generalized over things besides
lists. For now if you have a data structure that's a polymorphic
container type, consider implementing foldMap with your corresponding
type instead of [].
-}
foldMap : Appendable r m -> (a -> m) -> [a] -> m
foldMap app f xs = case xs of 
  [] -> app.empty
  _  -> foldMap1 app f xs

{-| Accumulates the values of a Signal using an `Appendable`.
    
```haskell
import Generic.Appendable as App

count : Signal a -> Signal Int    
count = accumWith Append.sum (\_ -> 1)

countIf : (a -> Bool) -> Signal a -> Signal Int
countIf p = accumWith Append.sum (\x -> if p x then 1 else 0)
    
remember : Signal a -> Signal [a]  
remember = accumWith Append.list (\x -> [x])
```
-}
accumWith : Appendable r m -> (a -> m) -> Signal a -> Signal m
accumWith a f = foldp (a.op . f) a.empty

{-| Fold over a non-empty list combining with a Combinable.

Generalizes some other useful functions:

```haskell
import Combinable as Comb

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
```
-}
fold1 : Combinable r s -> [s] -> s
fold1 c = foldMap1 c id

{-| foldMap for non-empty lists. -}
foldMap1 : Combinable r s -> (a -> s) -> [a] -> s
foldMap1 c f = let mergeAll xs = case xs of
                     (x :: y :: ys) -> mergeAll (c.op x y :: mergeAll ys)
                     _              -> xs
                   pass1 xs = case xs of
                     (x :: y :: ys) -> mergeAll (c.op (f x) (f y) :: pass1 ys)
                     _              -> mergeAll . map f <| xs
               in head . pass1
