import Data.List (foldl')
import Data.Char (chr)

encode :: String -> String
encode = fst . foldl' flatten ([],False) . (++[(eof,0)]) . foldl' group []
  where group [] c         = [(c,1)]
        group ((x,n):xs) c | c==x && n<9 = (x,n+1) : xs
                           | otherwise   = (c,1) : (x,n) : xs

        flatten (acc,v) (_,0) | v         = ('1':acc,False)
                              | otherwise = (acc,False)
        flatten (acc,v) (x,1) | v         = (showC x acc,v)
                              | otherwise = (showC x ('1':acc),True)
        flatten (acc,v) (x,n) | v         = (chr (n+48):x:('1':acc),False)
                              | otherwise = (chr (n+48):x:acc,False)
        
        showC '1' cs = '1':'1':cs
        showC c cs   = c:cs

        eof :: Char
        eof = '\0'

main :: IO ()
main = interact (unlines . map encode . lines)
