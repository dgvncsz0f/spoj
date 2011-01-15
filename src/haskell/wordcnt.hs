import Data.List
import Control.Monad

wordcnt = show . maximum . (0:) . map length . group . map length . words

main = 
  liftM read getLine >>= \n ->
  interact (unlines . map wordcnt . take n . lines)
