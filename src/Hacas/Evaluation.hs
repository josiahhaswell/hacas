module Hacas.Evaluation where
import Hacas.Expression

evaluate :: Expression -> Double
evaluate (Numeric x) = x
evaluate (Negate x) = -(evaluate x)
evaluate (Arithmetic op lhs rhs) = 
	case op of 
		Add -> (evaluate lhs) + (evaluate rhs)
		Subtract -> (evaluate lhs) - (evaluate rhs)
		Multiply -> (evaluate lhs) * (evaluate rhs)
		Exponentiate -> (evaluate lhs) ** (evaluate rhs)
		Divide -> case (evaluate rhs) of
			0 -> error "Cannot fucking divide by zero"
			x -> (evaluate lhs) / x 

setValue :: Expression -> String -> Expression -> Expression
setValue s@(Symbol a) name value = if (a == name) then value else s
setValue n@(Numeric a) _ _ = n
setValue (Arithmetic op lhs rhs) name value = 
	(Arithmetic op (setValue lhs name value) (setValue rhs name value))




