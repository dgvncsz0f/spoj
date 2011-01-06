import qualified Char as C
import qualified Data.List as L

data ArithmeticT =   Add Integer Integer
                   | Sub Integer Integer
                   | Mul Integer (Integer,[Integer])

alignWith :: a -> [[a]] -> [[a]]
alignWith c ls = let n = maximum . map length $ ls
                 in map (pad' n) ls
  where
    pad' n v | n<=length v = v
             | otherwise   = replicate (n - (length v)) c ++ v

print_sum :: Char -> Integer -> Integer -> [String]
print_sum op l r = let l1    = show l
                       l2    = op:show r
                       f     = case op 
                               of '+' -> (+)
                                  '-' -> (-)
                       l4    = show (f l r)
                       l3    = replicate (maximum . map length $ [l4,l2]) '-'
                   in alignWith ' ' $ [l1,l2,l3,l4]

print_mul :: Integer -> (Integer,[Integer]) -> [String]
print_mul l (r,r1) = let l1  = show l
                         l2  = '*':show r
                         ln  = map mul' . zip [0..] $ r1
                         ln2 = show $ l * r
                         l3  = replicate (maximum . map length $ [head ln,l2]) '-'
                         ln1 = replicate (maximum . map length $ [last ln,ln2]) '-'
                     in if (length r1>1 && l/=0) then
                          alignWith ' ' $ [l1,l2,l3] ++ ln ++ [ln1,ln2]
                        else
                          alignWith ' ' $ [l1,l2,l3,ln2]
  where
    mul' (n,d) = shows (d * l) (replicate n ' ')

prettyprint :: ArithmeticT -> [String]
prettyprint (Add l r) = print_sum '+' l r
prettyprint (Sub l r) = print_sum '-' l r
prettyprint (Mul l r) = print_mul l r

arith :: String -> [String]
arith expr = let (l,r0)  = break (flip elem "+-*") expr
                 (op,r)  = splitAt 1 r0
             in case (head op)
                of '*' -> prettyprint $ Mul (read l) (read r, reverse . map (fromIntegral . C.digitToInt) $ r)
                   '+' -> prettyprint $ Add (read l) (read r)
                   '-' -> prettyprint $ Sub (read l) (read r)

main = getLine >>= \n ->
  interact (unlines . map (unlines . arith) . take (read n) . lines)
