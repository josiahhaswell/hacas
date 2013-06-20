module Hacas.Utils where


trimQuotes a = 
	let trimmed = (show a)
	in (drop 1 $ take (length trimmed - 1) trimmed)
