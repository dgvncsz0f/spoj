import qualified Data.ByteString.Lazy.Char8 as B

mayadigits :: [(B.ByteString,Int)]
mayadigits = [ (B.pack "S",0),(B.pack ".",1),(B.pack "..",2),(B.pack "...",3),
               (B.pack "....",4),(B.pack "-",5),(B.pack ". -",6),(B.pack ".. -",7),
               (B.pack "... -",8),(B.pack ".... -",9),(B.pack "- -",10),
               (B.pack ". - -",11),(B.pack ".. - -",12),(B.pack "... - -",13),
               (B.pack ".... - -",14),(B.pack "- - -",15),(B.pack ". - - -",16),
               (B.pack ".. - - -",17),(B.pack "... - - -",18),(B.pack ".... - - -",19)
             ]

rmaya :: [B.ByteString] -> [(Int,Int)]
rmaya s1 = parse 0 (length s1 - 1) s1
  where
    parse _ _ []     = []
    parse i n (d:ds) = case (lookup d mayadigits)
                       of (Just v) -> (v,n-i) : parse (i+1) n ds
                          Nothing  -> error "parse error"

maya2dec :: [(Int,Int)] -> Integer
maya2dec = foldr fold 0
  where
    fold (d,e) a | e>1       = (fromIntegral d)*(20^e - 40*20^(e-2)) + a
                 | otherwise = (fromIntegral d)*20^e + a

maya :: [B.ByteString] -> [B.ByteString]
maya []     = []
maya (i:is) | B.null i  = maya is
            | otherwise = let n       = readi i
                              (l,ris) = splitAt n is
                          in if (n>0) then
                               (B.pack . show . maya2dec . rmaya $ l) : maya ris
                             else
                               []
  where
    readi n = case (B.readInt n)
              of (Just (v,_)) -> v

main = B.interact (B.unlines . maya . B.lines)
