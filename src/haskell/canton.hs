
import Control.Monad

_nth :: (Integral a) => a -> a
_nth n = let n' = fromIntegral n
         in truncate (0.5*n' * (n'+1))

_diagonal :: (Integral a) => a -> a
_diagonal n = exec' n'
  where 
    n' = (truncate . sqrt . fromIntegral . (2*)) n

    exec' k = let y = _nth k
              in if (y>=n)
                 then k
                 else exec' (k+1)

cantor :: (Integral a) => a -> (a,a)
cantor n = let d  = _diagonal n
               z  = fromIntegral $ _nth d - n
               c0 = map (\i -> (i,d+1-i)) [1..d]
               c1 = map (\i -> (d+1-i,i)) [1..d]
           in if (odd d) 
              then (head . drop z) c0
              else (head . drop z) c1

main :: IO ()
main = liftM read getLine >>= \n ->
       interact (unlines . map (cantor' . read) . take n . lines)
  where
    cantor' n = let (n1,d) = cantor n
                in (showString "TERM " . shows n . showString " IS " . shows n1 . showString "/" . shows d) ""
