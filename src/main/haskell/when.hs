
import qualified Data.Map as M

data Operation

data TokenT = Foo

lexer :: String -> [(TokenT,String)]
lexer = error "unsupported operation"

parser :: [(TokenT,String)] -> [OperationT]
parser = error "unsupported operation"

{-io-}

execute = "unsupported operation"

runscript = execute . parse

main = interact (unlines . runscript)
