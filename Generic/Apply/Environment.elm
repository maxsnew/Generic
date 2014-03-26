module Generic.Apply.Environment where

import Generic.Producer.Environment (..)

pure : a -> Env e a
pure x = \_ -> x

ap : Env e (a -> b) -> Env e a -> Env e b
ap e1 e2 = \x -> (e1 x) (e2 x)

(~) : Env e (a -> b) -> Env e a -> Env e b
(~) = ap

seq : Env e a -> Env e b -> Env e b
seq m1 m2 = (\x y -> y) <~ m1 ~ m2

(>>) : Env e a -> Env e b -> Env e b
(>>) = seq

pair : Env e a -> Env e b -> Env e (a, b)
pair m1 m2 = (,) <~ m1 ~ m2
