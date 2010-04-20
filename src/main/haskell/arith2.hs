-- https://www.spoj.pl/problems/ARITH2/

import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad as M

data InfixT a =   Empty
                | LeftOp a
                | Curried (a -> a)

arith_expr :: [B.ByteString] -> InfixT Integer
arith_expr = L.foldl infixfold' Empty
  where
    infixfold' Empty v       = LeftOp (read' v)
    infixfold' (LeftOp a) v  = Curried (operator' v $ a)
    infixfold' (Curried f) v = LeftOp (f (read' v))
    read' v = case (B.readInteger v)
              of (Just (v1,_)) -> v1
                 Nothing       -> 0 -- will not happen
    operator' v | B.head v=='+' = (+)
                | B.head v=='-' = (-)
                | B.head v=='*' = (*)
                | B.head v=='/' = div
                | B.head v=='=' = const

main = do
  input <- B.getContents
  M.forM (tail $ map (B.words) (B.lines input)) $ \expression -> do
    case (arith_expr expression) of 
      Empty       -> return ()
      -- the expression ends with a = sign
      -- this is implemented using const function (that is why the undefined value)
      (Curried f) -> putStrLn $ show (f undefined)
  return ()
