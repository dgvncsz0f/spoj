import qualified Data.Map as M
import qualified Data.List as L

type BracketT = M.Map Int Bool

rbracket :: String -> BracketT
rbracket = M.fromList . zip [1..] . map (\c -> if (c=='(') then True else False)

check :: BracketT -> Bool
check m | even . M.size $ m = L.foldl' fold' 0 (M.elems m) == 0
        | otherwise         = False
  where
    fold' r l | r<0       = minBound :: Int
              | l         = r+1
              | otherwise = r-1

flipb :: Int -> BracketT -> BracketT
flipb i = M.adjust not i

brckts :: Int -> [String] -> [Bool]
brckts 0 _  = []
brckts k ls = let (_,ls1)   = splitAt 1 ls
                  ([m],ls2) = splitAt 1 ls1
                  ([t],ls3) = splitAt 1 ls2
                  (tcs,ls4) = splitAt (read t) ls3
              in (brckts' (rbracket m) . map read $ tcs) ++ brckts (k-1) ls4
  where
    brckts' _ []                   = []
    brckts' m (op:ops) | op == 0   = check m : brckts' m ops 
                       | otherwise = brckts' (flipb op m) ops

main = interact (unlines . map (\v -> if v then "YES" else "NO") . brckts 1 . lines)

