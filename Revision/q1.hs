swap :: [a] -> [a]
swap [] = []
swap (x:y:rest) = y:x:(swap rest)
swap [x] = [x]
