module Functions2 where
	
	factorial :: Int -> Int
    factorial n = | n >= 0 = f n 1
        where
            f 0 acc = acc
            f 1 acc = acc
            f n acc = f (n - 1) (acc * n)
			
	
	fib :: Int -> Int
    fib n = f n 1
        where 
            f 0 acc = acc
            f 1 acc = acc
            f 2 acc = 1
            f n acc = f (n - 2) acc + f (n - 1) acc
			
			
	nuton :: Int -> Int -> Int
    nuton n m = div (factorial n) ((factorial m) * factorial (n - m))