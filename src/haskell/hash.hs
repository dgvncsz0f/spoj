
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad as C

h :: Integer -> Integer -> Integer -> Integer -> Integer
h a b m y = mod (a*y + b) m

hash :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer -> Integer
hash hx c d x n = let hlist = (M.toAscList . M.fromListWith (+) . zip [ hx k | k <- [x..x+n] ] . repeat) 1
                  in (L.foldl' (\b (_,z) -> b+z) 0 . filter (\(k,_) -> k>=c && k<=d)) hlist

readint :: B.ByteString -> Integer
readint = fst . M.fromJust . B.readInteger

main :: IO ()
main = do (t:ts) <- fmap B.lines B.getContents
          C.mapM (print . hash') ts
          return ()
  where
    hash' s = let (a:b:x:n:c:d:m:[]) = (map readint . B.words) s
              in hash (h a b m) c d x n
