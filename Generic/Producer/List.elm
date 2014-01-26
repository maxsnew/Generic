module Generic.Producer.List where

import List

map : (a -> b) -> [a] -> [b]
map = List.map

(<~) : (a -> b) -> [a] -> [b]
(<~) = map
