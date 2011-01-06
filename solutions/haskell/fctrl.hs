module Main where

import IO

zeroes_f :: Int -> Int
zeroes_f 1 = 0
zeroes_f n = foldr (+) 0 (quotients 5)
             where quotients k | k>n       = []
                               | otherwise = (div n k) : quotients (5*k)

repeat_n :: (Monad a, Integral b) => b -> a c -> a ()
repeat_n 0 f = return ()
repeat_n n f = do {f; repeat_n (n-1) f;}

m_zeroes_f :: IO Int
m_zeroes_f = do
  s <- getLine
  return (zeroes_f (read s))

main = do
  hSetBuffering stdin LineBuffering
  t <- getLine
  repeat_n (read t) (m_zeroes_f >>= print)
