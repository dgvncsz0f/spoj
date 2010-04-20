import Data.List (foldl')

comb :: Integer -> Integer -> Integer
comb n k = truncate (0.5 + foldl' step 1 [1..k'])
  where step acc i = acc * (fromIntegral $ n-k'+i) / (fromIntegral i)

        k' | k>n `div` 2 = (n-k)
           | otherwise   = k 

main :: IO ()
main = interact (unlines . map show . marble . lines)
  where marble (t:ts) = map ((\[n,k] -> comb (n-1) (k-1)) . map (readNum) . words) 
                            (take (readNum t) ts)

readNum :: Num a => String -> a
readNum = fromIntegral . (read :: String -> Int)
