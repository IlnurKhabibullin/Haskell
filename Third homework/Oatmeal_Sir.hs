module Oatmeal_Sir where

data OatmealTemp = Temp Int deriving (Show, Ord, Eq)

data Adjustment = TurnToLeft | TurnToRight | Good deriving (Show)


oatmeal_temp_to_adjustment :: OatmealTemp -> Adjustment
oatmeal_temp_to_adjustment temp
	| temp > Temp 10 = TurnToLeft
	| temp < Temp 10 = TurnToRight
	| otherwise = Good