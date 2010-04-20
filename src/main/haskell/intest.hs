
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Maybe as M
import qualified Data.List as L

main = do
  (l:ls) <- fmap B.lines B.getContents
  let [t,n] = map int (B.words l)
  (print . L.foldl' (intest n) 0 . map int . take t) ls

int :: B.ByteString -> Int
int = fst . M.fromJust . B.readInt

intest :: Int -> Int -> Int -> Int
intest n accum a | mod a n == 0 = accum+1
                 | otherwise    = accum
