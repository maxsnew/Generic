import Mouse

import Generic.Fold as F
import Generic.Appendable as App

pure x = [x]
remember sig = F.accumWith App.list pure sig

mergeList : App.Appendable {} [comparable]
mergeList = let op xs ys = case (xs, ys) of
                  ([], _)        -> ys
                  (_, [])        -> xs
                  (x::xs', y::ys') -> case compare x y of
                    GT -> y :: op xs ys'
                    _  -> x :: op xs' ys
            in 
             { empty = []
             , op    = op
             }

mySort : [comparable] -> [comparable]
mySort = F.foldMap mergeList pure

main = flow down <~ combine [ asText <~ remember Mouse.clicks
                            , constant . asText . mySort <| [6532,12346,0,123435462346, 99483, 120439]
                            ]
