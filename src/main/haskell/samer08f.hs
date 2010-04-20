module Main where

import Data.List

sqrs_in_grid :: Int -> Int
sqrs_in_grid n = foldl' (+) 0 (map (^2) [1..n])

{- io stuff -}
main = do
  string <- getLine
  let grid = read string :: Int in
    if (grid /= 0)
    then do
      return(sqrs_in_grid grid) >>= print
      main
    else do
      return()
