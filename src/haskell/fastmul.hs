module Main where

import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as B

asInt :: B.ByteString -> Int
asInt = fst . fromJust . B.readInt

asInteger :: B.ByteString -> Integer
asInteger = fst . fromJust . B.readInteger

solve :: [B.ByteString] -> B.ByteString
solve [sa,sb] = B.pack $ show (a*b)
  where a = asInteger sa
        b = asInteger sb

main :: IO ()
main = do { n <- fmap asInt B.getLine
          ; replicateM n B.getLine >>= mapM_ solveM
          }
  where solveM = B.putStrLn . solve . B.words

