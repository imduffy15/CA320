data Score = Conversion | Try | Goal

convert :: Score -> Int
convert Conversion = 2
convert Try = 5
convert Goal = 3

sumScore :: [Score] -> Int
sumScore [] = 0
sumScore (x:xs) = (convert x) + sumScore(xs)
