data BinTree t = Empty | Root t (BinTree t) (BinTree t)
		 deriving (Eq, Ord, Show)

myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))
		(Root 7 Empty Empty)

insert :: Ord a => a -> BinTree a -> BinTree a
insert new Empty = Root new Empty Empty
insert new (Root n left right)
	| new > n  = Root n left (insert new right)
	| new <= n  = Root n (insert new left) right 

maketree :: Ord a => [a] -> BinTree a
maketree (x:[]) = Root x (Empty) (Empty)
maketree (x:xs) = insert x (maketree xs)

preorder :: BinTree a -> [a]
preorder n = preorder' n []
	where preorder' Empty acc = acc
	      preorder' (Root n left right) acc = n : (preorder' left (preorder' right acc))

inorder :: BinTree a -> [a]
inorder n = inorder' n []
	where inorder' Empty acc = acc
	      inorder' (Root n left right) acc = inorder' left(n: (inorder' right acc))

postorder :: BinTree a -> [a]
postorder n = postorder' n []
	where postorder' Empty acc = acc
	      postorder' (Root n left right) acc = postorder' left (postorder' right acc) ++ [n]
