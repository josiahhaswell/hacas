p input = case (parse parseInput "" input) of
	Right x -> x
	Left x -> error (show x)
