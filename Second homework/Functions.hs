module Functions ( 
	factorial,
	fibonacci,
	ackermann
	) where
	
	--generates infinite list of factorial elements
	
	factorialSequence :: [Int]
	factorialSequence = facto 0 1
		where facto n acc = acc : facto (n + 1) (acc * (n + 1))

	--takes required element from this list
	factorial :: Int -> Int
	factorial x = factorialSequence !! x

	--same as factorial
	
	fibonacciSequence :: [Int]
	fibonacciSequence = 1:fib 1 1
		where fib n1 n2 = n2 : fib n2 (n1 + n2)
		
	fibonacci :: Int -> Int
	fibonacci x = fibonacciSequence !! (x - 1)
	
	-----------------------------------------------------------
	
	ackermann :: Int -> Int -> Int
	ackermann m n
		| m == 0 = n + 1
		| m > 0 && n == 0 = ackermann (m - 1) 1
		| m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))
		| otherwise = error "Incorrect input"