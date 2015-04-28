module Trees where
	
	--binary search tree with values in node, leafs is empty
	data BST = BstLeaf | NodeKey Int (Int) (BST) (BST) deriving (Show, Read, Eq)
		
	bstInsert :: Int -> Int -> BST -> BST
	bstInsert key value BstLeaf = NodeKey key value BstLeaf BstLeaf 
	bstInsert key value (NodeKey a b left right)   
		| key == a = NodeKey key value left right  
		| key < a  = NodeKey a b (bstInsert key value left) right  
		| key > a  = NodeKey a b left (bstInsert key value right)
	
	--height of the BST
	--I dont count leafs in height, so if i should, then just put 1 instead of 0
	bstHeight :: BST -> Int
	bstHeight BstLeaf = 0
	bstHeight (NodeKey _ _ left right) = 1 + max (bstHeight left) (bstHeight right)
	
	--sum of all elements in the BST
	bstSum :: BST -> Int
	bstSum BstLeaf = 0
	bstSum (NodeKey _ value left right) = value + bstSum left + bstSum right
	
	--search of element with given key in the BST. I dont know what to return if there is no
	--such key in the tree, so i desided to return an error :) 
	bstSearch :: Int -> BST -> Int  
	bstSearch _ BstLeaf = error "Not found"  
	bstSearch key (NodeKey a b left right)  
		| key == a = b  
		| key < a  = bstSearch key left  
		| key > a  = bstSearch key right
	
	--list with alternating types of values
	data StrangeList a b = SlNull | SlValue a (StrangeList b a) deriving (Show, Read, Eq)
	
	--length of list
	slLength :: StrangeList a b -> Int
	slLength sls = case sls of
		SlNull -> 0
		SlValue x xs -> slLength xs + 1
		
	--functions "fa" and "fb" should go in the same order as types in list
	dmap :: StrangeList a b -> (a -> a) -> (b -> b) -> StrangeList a b
	dmap sls fa fb = case sls of 
		SlNull -> SlNull
		SlValue x xs -> SlValue (fa x) (dmap xs fb fa)
	
	--  binary search trees with values only in the leafs
	data LBST a = LbstLeaf a | LbstNodeKey Int (LBST a) (LBST a) deriving (Show, Read, Eq)
		
	--i dont count leafs here too cause they have no keys, only values
	lbstHeight :: LBST a -> Int
	lbstHeight (LbstLeaf _) = 0
	lbstHeight (LbstNodeKey _ left right) = 1 + max (lbstHeight left) (lbstHeight right)
	
	tmap :: (a -> b) -> LBST a -> LBST b
	tmap f (LbstLeaf a) = LbstLeaf (f a)
	tmap f (LbstNodeKey n left right) = LbstNodeKey n (tmap f left) (tmap f right)