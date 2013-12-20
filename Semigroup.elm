module Semigroup where

type Semigroup g = { op : g -> g -> g }

{-| Monoids that don't have an empty element -}
firstSG : Semigroup a
firstSG = { op x y = x }

lastSG : Semigroup a
lastSG = { op x y = y }

maxSG : Semigroup comparable
maxSG = { op = max }

minSG : Semigroup comparable
minSG = { op = min }
