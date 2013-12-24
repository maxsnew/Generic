module Generic.Appendable where

{-| AppendableWithEmptys that don't have an empty element -}
type Appendable r g = { r | append : g -> g -> g }

firstApp : Appendable {} a
firstApp = { append x y = x }

lastApp : Appendable {} a
lastApp = { append x y = y }

maxApp : Appendable {} comparable
maxApp = { append = max }

minApp : Appendable {} comparable
minApp = { append = min }

sigApp : Appendable {} (Signal a)
sigApp = { append = merge }


