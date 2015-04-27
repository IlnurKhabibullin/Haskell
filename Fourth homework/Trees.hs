module Trees where
	
	data BST = Leaf | NodeKey Int (Int) (BST) (BST) deriving (Show, Read, Eq)
		
	bstInsert :: Int -> Int -> BST -> BST
	bstInsert key value Leaf = NodeKey key value Leaf Leaf 
	bstInsert key value (NodeKey a b left right)   
		| key == a = NodeKey key value left right  
		| key < a  = NodeKey a b (bstInsert key value left) right  
		| key > a  = NodeKey a b left (bstInsert key value right)
	
	--height of the BST
	bstHeight :: BST -> Int
	bstHeight Leaf = 0
	bstHeight (NodeKey _ _ left right) = 1 + max (bstHeight left) (bstHeight right)
	
	--sum of all elements in the BST
	bstSum :: BST -> Int
	bstSum Leaf = 0
	bstSum (NodeKey _ value left right) = value + bstSum left + bstSum right
	
	--search of element with given key in the BST. I dont know what to return if there is no
	--such key in the tree, so i desided to return an error :) 
	bstSearch :: Int -> BST -> Int  
	bstSearch _ Leaf = error "Not found"  
	bstSearch key (NodeKey a b left right)  
		| key == a = b  
		| key < a  = bstSearch key left  
		| key > a  = bstSearch key right

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