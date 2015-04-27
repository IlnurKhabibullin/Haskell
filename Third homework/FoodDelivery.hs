module FoodDelivery where

data DinnerOrder = Chicken | Pasta | NotHungry deriving (Eq, Show)

dinner_ordered_msg :: DinnerOrder -> String
dinner_ordered_msg msg
	| msg == NotHungry = "Passenger is fine"
	| otherwise = "Passenger ordered " ++ show msg