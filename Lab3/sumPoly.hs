type Poly = [Integer]

sumPoly :: Poly -> Poly -> Poly
sumPoly xs [] = xs
sumPoly [] ys = ys
sumPoly (x:xs) (y:ys) = (x+y) : sumPoly xs ys
