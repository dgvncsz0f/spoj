main = getLine >>= \n -> interact (show . foldr (+) 0 . filter (>0) . map read . take (read n) . lines)
