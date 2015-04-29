module Main where

	data Type = BoolType | IntType | WrongType | LType Type Type deriving (Show, Eq, Ord)

	data T = BoolLiteral Bool | IntLiteral Int deriving (Show, Eq, Ord)

	data Exp =  Var String | Lambda String Type Exp | Application Exp Exp | Literal T

	-- Context
	type Context = [(String, Type)]

	-- addition to context
	extend :: Context -> (String, Type) -> Context
	extend cont a = a : cont

	--check expression on valid typed. It isn't valid if we have type "WrongType" else other type.
	checkExp :: Context -> Exp -> Type

	checkExp _ (Literal (IntLiteral a)) = IntType
	checkExp _ (Literal (BoolLiteral a)) = BoolType

	checkExp cont (Application a b) = let 
								  term1 = checkExp cont a
								  term2 = checkExp cont b
								  go = if (term1 == WrongType || term2 == WrongType) 
											then WrongType 
										 else 
											checkOnLType term1 term2
								  in go

	checkExp cont (Lambda a b exp)
		| b == WrongType = b
		| t /= WrongType = LType b t
		| otherwise = WrongType
			where t = (checkExp (extend cont (a, b)) exp)

	checkExp cont (Var x) = case (lookup x cont) of
								Just e -> e
								Nothing -> WrongType

	checkOnLType :: Type -> Type -> Type
	checkOnLType (LType type1 type2) t2
		| type1 == t2 && type1 /= WrongType && type2 /= WrongType = type2
	--"otherwise" guard returns "WrongType", but we can опустить this operator cause then goes pattern matching case with the same result
	checkOnLType _ _ = WrongType

	-- check on valid type expression. If type is "WrongType" - False, else "True" 
	check :: Type -> Bool
	check t
		| t == WrongType = False
		| otherwise True