module Generic.Producer.Environment where

{-| Produces a value when given an environment. Known as `Reader` in Haskell. -}
type Env e a = e -> a

map : (a -> b) -> Env e a -> Env e b
map f e = f . e

(<~) : (a -> b) -> Env e a -> Env e b
(<~) = map