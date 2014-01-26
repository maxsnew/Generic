module Generic.Apply.Maybe where

import open Maybe
import open Generic.Producer.Maybe

pure : a -> Maybe a
pure = Just

ap : Maybe (a -> b) -> Maybe a -> Maybe b
ap m1 m2 = case m1 of
  Nothing -> Nothing
  Just f  -> case m2 of
    Nothing -> Nothing
    Just x -> Just (f x)

(~) : Maybe (a -> b) -> Maybe a -> Maybe b
(~) = ap

seq : Maybe a -> Maybe b -> Maybe b
seq m1 m2 = (\x y -> y) <~ m1 ~ m2

(>>) : Maybe a -> Maybe b -> Maybe b
(>>) = seq

pair : Maybe a -> Maybe b -> Maybe (a, b)
pair m1 m2 = (,) <~ m1 ~ m2
