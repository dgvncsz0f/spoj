-- module Main (main) where 

import Control.Monad (foldM_)
import Text.Printf (printf)

type Position = (Double,Double)

distance :: (Position,[Double]->[Double]) -> Position -> (Position,[Double]->[Double])
distance ((a,b),acc) (c,d) = let this = sqrt $ (c-a)**2 + (d-b)**2
                             in ((c,d),acc.(this:))

distanceM :: [Position] -> IO ()
distanceM (x:xs) = foldM_ printResult 0 (snd distances [])
  where distances = foldl distance (x,id) xs
        printResult acc s = do printf "The salesman has traveled a total of %.3f kilometers.\n" (acc+s)
                               return (acc+s)

main :: IO ()
main = do ts <- fmap (lines) getContents
          distanceM (map readPair ts)
  where fixzero []           = []
        fixzero ('(':'.':xs) = '(':'0':'.' : fixzero xs
        fixzero ('-':'.':xs) = '-':'0':'.' : fixzero xs
        fixzero (' ':'.':xs) = ' ':'0':'.' : fixzero xs
        fixzero (',':'.':xs) = ',':'0':'.' : fixzero xs
        fixzero (x:xs)       = x : fixzero xs

        readPair = read . fixzero . init . dropWhile (/='(')
