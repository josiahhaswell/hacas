module Hacas where


import qualified Text.Parsec as P
import Hacas.Parser
import Hacas.Simplifications


hacas input = case (P.parse parseInput "" input) of
  Right x -> x
  Left x -> error (show x)

main = do
  p <- getLine
  putStrLn (show (hacas p))
