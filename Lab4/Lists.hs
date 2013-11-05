myAppend :: [a] -> [a] -> [a]
myAppend [] x = x
myAppend (x:xs) y = x : myAppend xs y

myHead :: [a] -> a
myHead [] = error "Empty List supplied"
myHead (x:_) = x

myLast :: [a] -> a
myLast [] = error "Empty List supplied"
myLast [x] = x
myLast (_:xs) = myLast xs

myInit :: [a] -> [a]
myInit [] = error "Empty List supplied"
myInit [x] = []
myInit (x:y:[]) = [x]
myInit (x:xs) = x : myInit(xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum(xs)

myProduct :: Num a => [a] -> a
myProduct [] = error "Empty List supplied"
myProduct [x] = x
myProduct (x:xs) = x * myProduct(xs)

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "Empty List supplied"
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Empty List supplied"
myMinimum [x] = x
myMinimum (x:xs) = min x (myMinimum xs)

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys)
	| x == y = True
	| otherwise = myElem x ys

myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (y:ys)
	| x==y = myDelete x ys
	| otherwise = y : myDelete x ys

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect _ [] = []
myIntersect [] _ = []
myIntersect (x:xs) ys
	| myElem x ys = x : myIntersect xs ys
	| otherwise = myIntersect xs ys
