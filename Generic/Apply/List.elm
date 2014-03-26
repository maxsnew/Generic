module Generic.Apply.List where

import List
import Generic.Producer.List (..)
import Generic.Workflow.List (..)

pure : a -> [a]
pure x = [x]

ap : [a -> b] -> [a] -> [b]
ap m1 m2 = m1 >>= \f ->
           m2 >>= \x ->
           pure (f x)

(~) : [a -> b] -> [a] -> [b]
(~) = ap

seq : [a] -> [b] -> [b]
seq m1 m2 = (\x y -> y) <~ m1 ~ m2

(>>) : [a] -> [b] -> [b]
(>>) = seq

pair : [a] -> [b] -> [(a, b)]
pair m1 m2 = (,) <~ m1 ~ m2
