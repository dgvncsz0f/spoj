module Main (main) where

import Data.List (splitAt)

type Z = Integer
type Frac = (Z,Z)

show_ :: Frac -> String
show_ (p,q) = shows p . showString " " . shows q $ ""

swap :: Frac -> Frac
swap (a,b) = (b,a)

parse :: [String] -> [Z]
parse = parseix . zip [0..]
  where parseix []                      = []
        parseix ((ix,x):xs) | even ix   = parseix xs
                            | otherwise = let num = (takeWhile (/='.') . dropWhile (=='.'))
                                          in read (num x) : parseix xs

unfrac :: [Z] -> Frac
unfrac = swap . foldr unfrac' (1,1)
  where unfrac' x (p,q) = (q,x*q+p)

main :: IO ()
main = interact (unlines . map (show_ . unfrac) . parse' . takeWhile (/= "0 0") . lines)
  where parse' []     = []
        parse' (x:xs) = let [l,_]   = (map read . words) x
                            (as,bs) = splitAt l xs
                        in parse as : parse' bs
