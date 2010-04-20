
import qualified Data.List as L
import qualified Data.Char as C

baseb_expansion :: Int -> Int -> [Int]
baseb_expansion b = L.unfoldr (digit' (fromIntegral b))
  where
    digit' b1 d | d==0      = Nothing
                | otherwise = Just (fromIntegral (mod d b1), div d b1)

decimal_notation :: Int -> [Int] -> Int
decimal_notation b = foldr ((+).term') 0 . zip [0..]
  where
    b'          = fromIntegral b

    term' (e,d) = let d' = fromIntegral d
                  in d' * b'^e

read_number :: Int -> String -> Int
read_number b = decimal_notation b . reverse . map (fromIntegral . C.digitToInt)

show_number :: Int -> Int -> Maybe String
show_number b n = let baseb = baseb_expansion b n
                  in if (length baseb > 7)
                     then Nothing
                     else (Just . map (C.toUpper . C.intToDigit) . reverse) baseb

main = interact (unlines . map (base' . words) . lines)
  where
    base' [n,b0,b1] = let input  = read_number (read b0) n
                          output = show_number (read b1) input
                      in case output
                         of Nothing -> "ERROR"
                            Just o  -> o
