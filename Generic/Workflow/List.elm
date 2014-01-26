module Generic.Workflow.List where

import List

bind : [a] -> (a -> [b]) -> [b]
bind = flip List.concatMap

(>>=) : [a] -> (a -> [b]) -> [b]
(>>=) = bind

join : [[a]] -> [a]
join = List.concat
