
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad as M

reverse_num :: B.ByteString -> B.ByteString
reverse_num n = B.dropWhile (=='0') (B.reverse n)

reverse_int :: Int -> B.ByteString
reverse_int = reverse_num . B.pack . show

main = do
  input <- B.getContents
  let lines  = B.lines input
      n      = readint (head lines)
      tcases = map B.words (tail lines)
  M.forM (take n tcases) $ \tcase -> do
    B.putStrLn $ reverse_int (foldr (+) 0 (map (readint . reverse_num) tcase))
  return ()

  where 
    readint s = case (B.readInt s) of
                  (Just (v1,_)) -> v1
                  Nothing       -> 0
