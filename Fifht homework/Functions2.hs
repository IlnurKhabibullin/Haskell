module Functions2 (
	foldl_map,
	foldr_map,
	r_concatMap
	) where
	
	foldl_map :: (a -> b) -> [a] -> [b]  
	foldl_map function list = foldl (\acc x -> function x : acc) [] list
	
	foldr_map :: (a -> b) -> [a] -> [b]  
	foldr_map function list = foldr (\x acc -> function x : acc) [] list
	
	r_concatMap :: (a -> [b]) -> [a] -> [b]
	r_concatMap f [] = []
	r_concatMap function (a:as) = function a ++ r_concatMap function as