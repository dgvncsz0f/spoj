module Main where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B

-- List of primes up to ~sqrt(2^31)
primes :: [Int]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701]

-- Returns true if n | p
divides :: Int -> Int -> Bool
divides p n = (mod n p) == 0

-- If n | p then it returns a tuple (k,n'), where
--  'k'  -> 0<=k<n is the number of times p divides n
--  'n'' -> The remainder after the divisions have occurred
div' :: Int -> Int -> (Int,Int)
div' n p = _div' 0 n
  where 
    _div' s t
      | p `divides` t = _div' (s+1) (div t p)
      | otherwise     = (s,t)

-- Factores a number into prime factors.
prime_factors :: Int -> [(Int,Int)]
prime_factors n | n>0 = _prime_factors n primes
  where 
    _prime_factors :: Int -> [Int] -> [(Int,Int)]
    _prime_factors n [] = [(1,n)]
    _prime_factors n (p:ps) 
      | n==1      = []
      | p*p > n   = [(1,n)]
      | otherwise = let (s,n') = n `div'` p in
                      if (s>0)
                      then
                        (s,p) : _prime_factors n' ps
                      else
                        _prime_factors n ps

sigma :: Int -> Int
sigma n = (L.foldl' term' 1 pfactors') - n
  where
    term' m (a,p) = m * (div (p^(a+1) - 1) (p-1))
    pfactors'     = prime_factors n

{- io -}

m_repeat n f | n>1       = f >> m_repeat (n-1) f
             | otherwise = f

main = do
  t <- B.getLine
  m_repeat (readint' t) divsum'
    where
      readint' n = case (B.readInt n) of
                     Just (a,b) -> a
                     Nothing    -> 0
      divsum' = do
        n <- B.getLine
        putStrLn $ show $ sigma (readint' n)
