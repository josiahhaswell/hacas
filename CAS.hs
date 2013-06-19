
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.Char

operatorCharacters = "+-*/%"

lexer ::  TokenParser ()
lexer = makeTokenParser $ javaStyle {
	opStart = oneOf operatorCharacters,
	opLetter = oneOf operatorCharacters 
}


trimQuotes a = 
	let trimmed = (show a)
	in (drop 1 $ take (length trimmed - 1) trimmed)

data ArithmeticOperation = 
	Add | Subtract | Divide | Multiply | Exponentiate

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
	Function ([Expression] -> Expression) |
	Arithmetic ArithmeticOperation Expression Expression

instance Show Expression where 
	show (Numeric a) = (show a)
	show (Symbol a) = trimQuotes a 
	show (Function a) = "<function>" 
	show (Negate a) = "-" ++ (show a)
	show (Arithmetic op lhs rhs) = 
		(show lhs) ++ (show op) ++ (show rhs) 


numeric :: Parser Expression 
numeric = do
	value <- naturalOrFloat lexer
	case value of
		Left i -> return $ Numeric (fromIntegral i)
		Right i -> return $ Numeric i

sym :: Parser Expression
sym = 
	do
		value <- many1 $ letter 
		return $ Symbol value  

neg x = Negate x
identity x = x

operation op lhs rhs = Arithmetic op lhs rhs

expressionTable = [[ 
	 Prefix (reservedOp lexer "-" >> return neg) 
	,Prefix (reservedOp lexer "+" >> return identity) 
	],[ 
	 Infix (reservedOp lexer "^" >> return (operation Exponentiate)) AssocLeft
  ,Infix (reservedOp lexer "*" >> return (operation Multiply)) AssocLeft 
	,Infix (reservedOp lexer "/" >> return (operation Divide)) AssocLeft 
	],[ 
	 Infix (reservedOp lexer "+" >> return (operation Add)) AssocLeft 
	,Infix (reservedOp lexer "-" >> return (operation Subtract)) AssocLeft 
	]] 



rawExpression :: Parser Expression 
rawExpression = (flip buildExpressionParser) expression $ expressionTable


expression = parens lexer rawExpression <|> numeric  <|> sym

parseInput = 
	do 
		whiteSpace lexer
		n <- rawExpression 
		eof
		return n


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

simplify b@(Numeric a) = b
simplify b@(Symbol a) = b

simplify (Arithmetic op (Numeric (0)) rhs) = Numeric 0 
simplify (Arithmetic op lhs (Numeric (0))) = Numeric 0


simplify b@(Arithmetic op lhs@(Numeric _) rhs@(Numeric _)) = 
	Numeric (evaluate b)
simplify b@(Arithmetic op lhs@(Numeric _) rhs@(Symbol _)) = 
	Arithmetic op (Numeric (evaluate lhs)) rhs
simplify b@(Arithmetic op lhs@(Symbol _) rhs@(Numeric _)) = 
	Arithmetic op lhs (Numeric (evaluate rhs)) 
simplify (Arithmetic op lhs rhs) = 
	Arithmetic op (simplify lhs) (simplify rhs)

p input = case (parse parseInput "" input) of
	Right x -> x
	Left x -> error (show x)
