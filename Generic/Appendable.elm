module Generic.Appendable where

{-| AppendableWithEmptys that don't have an empty element -}
type Appendable r g = { r | op : g -> g -> g }

firstApp : Appendable {} a
firstApp = { op x y = x }

lastApp : Appendable {} a
lastApp = { op x y = y }

maxApp : Appendable {} comparable
maxApp = { op = max }

minApp : Appendable {} comparable
minApp = { op = min }

sigApp : Appendable {} (Signal a)
sigApp = { op = merge }


