module Main where

import System.IO

population :: Int -> Int -> Int
population 0 0 = 1
population 0 1 = 2
population 1 0 = 1
population n m = n + m

main = do { [n, m] <- fmap (map read . words) (hGetLine stdin)
          ; print (population n m)
          }
