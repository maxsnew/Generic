module Generic.Appendable where

import Basics

{-| AppendableWithEmptys that don't have an empty element -}
type Appendable r a = { r | append : a -> a -> a }

first : Appendable {} a
first = { append x y = x }

last : Appendable {} a
last = { append x y = y }

max : Appendable {} comparable
max = { append = Basics.max }

min : Appendable {} comparable
min = { append = Basics.min }

sig : Appendable {} (Signal a)
sig = { append = merge }


