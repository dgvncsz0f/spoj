module Main where

import qualified Data.List       as L
import qualified Data.ByteString as B
import qualified Char            as C

-- * The sum algorithm to a base10 expansion
(<+>) :: [Int] -> Int -> [Int]
(<+>) [] n    = [n]
(<+>) (x:xs) n 
  | n+x>9     = 0 : xs <+> 1
  | otherwise = x+n : xs

-- * Split a base10 expansion in a tuple containing (left,right) components.
_split :: [Int] -> ([Int],[Int])
_split xs 
  | even s'   = (\(l,r) -> (l,r))           $ L.splitAt (div s' 2) xs
  | otherwise = (\(l,r) -> (l++[head r],r)) $ L.splitAt (div s' 2) xs
    where
      s' = length xs

-- * Returns true if this is the last palin of this size
_last_palindrome []                 = False
_last_palindrome [x]    | x==9      = True
_last_palindrome (x:xs) | x==9      = _last_palindrome xs
                        | otherwise = False

-- * Returns the next palindrome
next :: [Int] -> [Int]
next xs  | lp'       = [1] ++ init z' ++ [1]
         | rl'>r'    = l' ++ (reduce' rl')
         | otherwise = (reverse rl1') ++ (reduce' rl1')
  where
    (l',rl',r',rl1') = (\(l,r) -> (l,reverse l,r,rl' <+> 1)) $ _split xs
    reduce' | even (length xs)   = id
            | otherwise          = tail
    lp' = _last_palindrome l'
    z'  = map (*0) xs


{- io -}

m_next = do
  line <- B.getLine
  putStrLn $ map C.intToDigit (next_palindrome line)
    where base10_expansion line = map (\x -> fromIntegral(x-48)) (B.unpack line)
          next_palindrome  line = next (base10_expansion line)

m_repeat n f | n>1       = f >> m_repeat (n-1) f
             | otherwise = f

main = do
  n <- getLine
  m_repeat (read n :: Int) m_next
