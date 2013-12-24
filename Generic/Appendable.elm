module Generic.Appendable where

{-| AppendableWithEmptys that don't have an empty element -}
type Appendable r g = { r | op : g -> g -> g }

firstSG : Appendable {} a
firstSG = { op x y = x }

lastSG : Appendable {} a
lastSG = { op x y = y }

maxSG : Appendable {} comparable
maxSG = { op = max }

minSG : Appendable {} comparable
minSG = { op = min }

sigSG : Appendable {} (Signal a)
sigSG = { op = merge }


