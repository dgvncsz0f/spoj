module Main where

import IO

factorial :: Integer -> Integer
factorial n = product[1..n]

repeat_n :: (Monad a, Integral b) => b -> a c -> a ()
repeat_n 0 f = return ()
repeat_n n f = do {f; repeat_n (n-1) f;}

m_factorial :: IO Integer
m_factorial = do
  s <- getLine
  return (factorial (read s))

main = do
  hSetBuffering stdin LineBuffering
  t <- getLine
  repeat_n (read t) (m_factorial >>= print)
