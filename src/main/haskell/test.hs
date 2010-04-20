import qualified Data.ByteString.Lazy.Char8 as B

main = B.interact (B.unlines . takeWhile (/=B.pack "42") . B.lines)
