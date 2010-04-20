
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad as M
import qualified Data.Maybe as M1


equations :: Int -> Int
equations _ = 0

read_int :: B.ByteString -> Int
read_int = fst . M1.fromJust . B.readInt

main :: IO ()
main = do
  input  <- fmap (takeWhile (/=0) . map read_int . B.lines) B.getContents
  mapM print (map equations input)
  return ()
