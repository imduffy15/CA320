--
-- CA320 Computability and Complexity
--
-- Takes in 3 numbers and checks to see if one of them is the sum 
-- of the other two
--
-- Ian Duffy
--

isSum :: Int -> Int -> Int -> Bool
isSum x y z = (x+y==z) || (x+z==y) || (y+z==x)
