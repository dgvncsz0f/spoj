
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Maybe as M

type PigBank = (Int,Int)
type Coin = (Int,Int)

read_int :: B.ByteString -> Int
read_int = fst . M.fromJust . B.readInt

read_pigbank :: B.ByteString -> PigBank
read_pigbank s = case (map (read_int) . B.words) s
                 of [a,b] -> (a,b)
                    _     -> error "parse error"

read_coin :: B.ByteString -> Coin
read_coin = read_pigbank

estimate :: PigBank -> [Coin] -> Int
