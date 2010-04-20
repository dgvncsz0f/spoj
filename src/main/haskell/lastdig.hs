import Control.Monad
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as B

ldl = listArray (0,9) (map (\l->listArray (0,length l-1) l) [[1],[2,4,8,6],[3,9,7,1],[4,6],[5],[6],[7,9,3,1],[8,4,2,6],[9,1],[0]])

lde b e | e==0 = 1
        | b==0 = 0
        | b>9  = lde (mod b 10) e
        | otherwise = b1 ! e1
  where
    b1 = ldl ! (b-1)
    sz = (snd $ bounds b1) + 1
    e1 = mod (e-1) sz

rint s = 
  case (B.readInt s)
  of (Just (v,_)) -> v

main = do
  i <- B.getContents
  let l = B.lines i
      n = rint $ head l
      t = map (map rint . B.words) (tail l)
  forM (take n t) $ \[b,e] -> putStrLn $ show $ lde b e
  return ()
