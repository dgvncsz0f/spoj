import qualified Char as C

mmass :: String -> Int
mmass = foldr (+) 0 . parse' []
  where
    parse' sk [] = sk
    parse' sk (t:rs) | t=='('      = parse' (0:sk) rs
                     | t==')'      = let (l,r) = break (==0) sk
                                         l1    = foldr (+) 0 l
                                         r1    = tail r
                                     in parse' (l1:r1) rs
                     | t=='C'      = parse' (12:sk) rs
                     | t=='H'      = parse' (1:sk) rs
                     | t=='O'      = parse' (16:sk) rs
                     | C.isDigit t = let n1      = C.digitToInt t
                                         ([l],r) = splitAt 1 sk
                                     in parse' (l*n1:r) rs
                     | otherwise   = parse' sk rs

main = getLine >>= \m -> putStrLn (show . mmass $ m)
