data BinTree t = Empty | Root t (BinTree t) (BinTree t)
		 deriving (Eq, Ord, Show)

myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))
		(Root 7 Empty Empty)

insert :: Ord a => a -> BinTree a -> BinTree a
insert item Empty = Root item Empty Empty
insert item (Root n left right)
	| item < n  = Root n (insert item left) right
	| otherwise  = Root n left (insert item right)

maketree :: Ord a => [a] -> BinTree a
maketree [] = Empty
maketree [x] = Root x (Empty) (Empty)
maketree (x:xs) = insert x (maketree xs)

mpsort :: Ord a => [a] -> [a]
mpsort x = inorder(maketree(x))

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Root n left right) = inorder left ++ [n] ++ inorder right

hoInsert :: Ord a => (a -> a -> Bool) -> a -> BinTree a -> BinTree a
hoInsert op item Empty = Root item Empty Empty
hoInsert op item (Root n left right)
	| item `op` n = Root n (hoInsert op item left) right
	| otherwise = Root n left (hoInsert op item right)

hoMakeTree :: Ord a => (a -> a -> Bool) -> [a] -> BinTree a
hoMakeTree _ [] = Empty
hoMakeTree _ [x] = Root x (Empty) (Empty)
hoMakeTree op (x:xs) = hoInsert op x (hoMakeTree op xs)

hosort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
hosort op list = inorder(hoMakeTree op list)
