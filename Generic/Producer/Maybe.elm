module Generic.Producer.Maybe where

import Maybe (..)

map : (a -> b) -> Maybe a -> Maybe b
map f = maybe Nothing (Just . f) 

(<~) : (a -> b) -> Maybe a -> Maybe b
(<~) = map