main = do
  a <- getLine
  b <- getLine
  print ((read a::Integer) + (read b::Integer))
