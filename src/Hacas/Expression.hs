module Hacas.Expression where

import Hacas.Utils
data ArithmeticOperation = 
	Add | Subtract | Divide | Multiply | Exponentiate deriving Eq

instance Show ArithmeticOperation where
	show Add = "+"
	show Subtract = "-"
	show Divide = "/"
	show Multiply = "*"
	show Exponentiate = "^"

data Expression = 
	Numeric Double |
	Symbol String |
	Negate Expression | 
	Arithmetic ArithmeticOperation Expression Expression deriving (Eq, Show)

--instance Show Expression where 
--	show (Numeric a) = (show a)
--	show (Symbol a) = trimQuotes a 
--	show (Negate a) = "-" ++ (show a)
--	show (Arithmetic op lhs rhs) = 
--		(show lhs) ++ (show op) ++ (show rhs) 

