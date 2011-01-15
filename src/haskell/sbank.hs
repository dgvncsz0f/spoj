import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Control.Monad as M
import Data.Maybe (fromJust)
import Data.Map (toAscList,fromListWith)

read_int :: B.ByteString -> Int
read_int = fst . fromJust . B.readInt

sbank :: [B.ByteString] -> [(B.ByteString,Int)]
sbank = toAscList . fromListWith (+) . flip zip (repeat 1)

main :: IO ()
main = do
  (t:input) <- fmap B.lines B.getContents
  outputs' (sbank' (read_int t) input)
  where
    outputs' (os:oss) = M.mapM output' os >> putChar '\n' >> outputs' oss
    outputs' []       = return ()

    output' (k,v) = B.putStr k >> putChar ' ' >> print v

    sbank' 0 _      = []
    sbank' t (i:is) = let n            = read_int i
                          (input,rest) = L.splitAt n is
                      in sbank input : (sbank' (t-1) . tail) rest
