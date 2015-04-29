module Functions3 (
	foldl_map,
	foldr_map,
	r_concatMap
	) where
	
	foldl_map :: (a -> b) -> [a] -> [b]  
	foldl_map f list = foldl (\acc x -> f x : acc) [] list
	
	foldr_map :: (a -> b) -> [a] -> [b]  
	foldr_map f list = foldr (\x acc -> f x : acc) [] list
	
	r_concatMap :: (a -> [b]) -> [a] -> [b]
	r_concatMap f [] = []
	r_concatMap f (a:as) = f a ++ r_concatMap f as
	
	foldl_concatMap :: (a -> [b]) -> [a] -> [b]
	foldl_concatMap f list = foldl (\acc x -> f x ++ acc) [] list
	
	foldr_concatMap :: (a -> [b]) -> [a] -> [b]
	foldr_concatMap f list = foldr (\x acc -> f x ++ acc) [] list