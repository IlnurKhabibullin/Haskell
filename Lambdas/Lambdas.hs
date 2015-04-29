module Lambdas where

	data Term = Var Char --variable
			  | Abstr Char Term --abstraction
			  | Applic Term Term --application
			  deriving (Show, Eq)

	replic :: Term -> Term -> Char -> Term --replication
	replic term (Var a) b
		| b == a = term
		| otherwise = Var a
	replic term (Abstr a term1) b
		| b == a = Abstr a term1
		| otherwise = Abstr a (replic term term1 b)
	replic term (Applic term1 term2) b = Applic (replic term term1 b) (replic term term2 b)

	
	eval :: Term -> Term
	eval (Var v)     = Var v
	eval (Abstr v term)   = Abstr v (eval term)
	eval (Applic term1 term2) =  case term1 of
						Var a -> Var a
						Abstr a term -> eval . replic & term2 term a --replication used in this case
						Applic (Var a) term -> Applic term1 term2
						Applic term3 term4 -> eval . Applic & (eval term1) term2