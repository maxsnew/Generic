module Generic.Workflow.Maybe where

import open Maybe
import open Generic.Producer.Maybe
import open Generic.Apply.Maybe

bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind m k = case m of
  Nothing -> Nothing
  Just x  -> k x

(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) = bind

join : Maybe (Maybe a) -> Maybe a
join = maybe Nothing id
