import Data.List (foldl')

fibs :: Integer -> Integer
fibs k = fst $ foldr (\_ (a,b) -> (b,a+b)) (0,1) [1..k]

-- notInTheBox :: Int -> Int -> Int
-- notInTheBox months capacity = fibbonacci months `mod` capacity
-- 
-- main :: IO ()
-- main = interact (unlines . map show . rabbits . lines)
--   where readNM line = let [n,m] = map (read) (words line)
--                       in (n+1,2^m)
--         rabbits (t:ts) = map (uncurry notInTheBox . readNM) $ take (read t) ts
-- 
