module Main (main) where

newtype Bitmap = Bitmap [Int]

showbits :: Bitmap -> String
showbits (Bitmap bs) = showbits' bs ""
  where showbits' []     = id
        showbits' (x:xs) = shows x . showbits' xs

negbits :: Int -> Int -> Bitmap
negbits 0 _ = Bitmap [0]
negbits n b = Bitmap (negbits' n [])
  where negbits' x | x==0      = id
                   | otherwise = let (q,r) = x `divMod` b
                                 in if (r<0)
                                    then negbits' (q+1) . (r + (abs b):)
                                    else negbits' q . (r:)

main :: IO ()
main = do x <- fmap read getLine
          putStrLn (showbits (negbits x (-2)))
