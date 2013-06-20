module Hacas.Parser where 
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.Char
import Hacas.Expression

operatorCharacters = "+-*/%"

lexer ::  TokenParser ()
lexer = makeTokenParser $ javaStyle {
	opStart = oneOf operatorCharacters,
	opLetter = oneOf operatorCharacters 
}




numeric :: Parser Expression 
numeric = do
	value <- naturalOrFloat lexer
	case value of
		Left i -> return $ Numeric (fromIntegral i)
		Right i -> return $ Numeric i

sym :: Parser Expression
sym = 
	do
		value <- stringLiteral lexer 
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


