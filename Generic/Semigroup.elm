module Generic.Semigroup where

{-| Monoids that don't have an empty element -}
type Semigroup r g = { r | op : g -> g -> g }

firstSG : Semigroup {} a
firstSG = { op x y = x }

lastSG : Semigroup {} a
lastSG = { op x y = y }

maxSG : Semigroup {} comparable
maxSG = { op = max }

minSG : Semigroup {} comparable
minSG = { op = min }

sigSG : Semigroup {} (Signal a)
sigSG = { op = merge }


