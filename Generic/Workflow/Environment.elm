module Generic.Workflow.Environment where

import Generic.Producer.Environment (..)

bind : Env e a -> (a -> Env e b) -> Env e b
bind e k = \x -> (k (e x) x)

(>>=) : Env e a -> (a -> Env e b) -> Env e b
(>>=) = bind

join : Env e (Env e a) -> Env e a
join e = \x -> e x x
