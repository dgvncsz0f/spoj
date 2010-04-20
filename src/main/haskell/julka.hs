module Main where

import IO

julka :: Integral a => a -> a -> (a, a)
julka t k = (t-y, y)
            where y = div (t-k) 2

repeat_n :: (Monad a, Integral b) => b -> a c -> a ()
repeat_n 0 f = return ()
repeat_n n f = do {f; repeat_n (n-1) f;}

--m_julka :: IO (Integer,Integer)
m_julka = do
  x <- getLine
  y <- getLine
  return (julka (read x) (read y))

main = do
  hSetBuffering stdin LineBuffering
  repeat_n 10 (m_julka >>= print_julka)
  where print_julka (a,b) = do {print a; print b}
