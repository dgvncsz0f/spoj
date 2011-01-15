
rotate :: Int -> [(a,b)] -> [(a,b)]
rotate n m = let k     = length m
                 z     = mod n k
                 fm    = map fst m
                 sm    = map snd m
                 (l,r) = splitAt (k-z) sm
             in zip fm (r ++ l)

groupsof :: String -> ([(Int,Char)],[(Int,Char)],[(Int,Char)])
groupsof msg = g' 0 msg
  where
    g' _ []                   = ([],[],[])
    g' i (m:ms)   | m>='a'&&m<='i' = let (a,b,c) = g' (i+1) ms
                                     in ((i,m):a,b,c)
                  | m>='j'&&m<='r' = let (a,b,c) = g' (i+1) ms
                                     in (a,(i,m):b,c)
                  | otherwise      = let (a,b,c) = g' (i+1) ms
                                     in (a,b,(i,m):c)

merge2 :: [(Int,Char)] -> [(Int,Char)] -> String
merge2 [] []                                         = []
merge2 [] m2                                         = map snd m2
merge2 m1 []                                         = map snd m1
merge2 m1@((i1,c1):m1s) m2@((i2,c2):m2s) | i1<i2     = c1 : merge2 m1s m2
                                         | otherwise = c2 : merge2 m1 m2s

merge3 :: [(Int,Char)] -> [(Int,Char)] -> [(Int,Char)] -> String
merge3 m1@(_:_) [] m3@(_:_)                                                        = merge2 m1 m3
merge3 m1@(_:_) m2@(_:_) []                                                        = merge2 m1 m2
merge3 [] m2@(_:_) m3@(_:_)                                                        = merge2 m2 m3
merge3 m1@(_:_) [] []                                                              = map snd m1
merge3 [] m2@(_:_) []                                                              = map snd m2
merge3 [] [] m3@(_:_)                                                              = map snd m3
merge3 [] [] []                                                                    = []
merge3 m1@((i1,c1):m1s) m2@((i2,c2):m2s) m3@((i3,c3):m3s) | minimum [i1,i2,i3]==i1 = c1 : merge3 m1s m2 m3
                                                          | minimum [i1,i2,i3]==i2 = c2 : merge3 m1 m2s m3
                                                          | otherwise              = c3 : merge3 m1 m2 m3s

decode :: Int -> Int -> Int -> String -> String
decode k1 k2 k3 m = let (m1,m2,m3) = groupsof m
                    in merge3 (rotate k1 m1) (rotate k2 m2) (rotate k3 m3)

wschiper :: [String] -> String
wschiper []                   = []
wschipher (l:ls) | l=="0 0 0" = []
                 | otherwise  = let [k1,k2,k3] = map read . words $ l
                                    msg        = head ls
                                in decode k1 k2 k3 msg : wschipher (tail ls)

main = interact (unlines . wschipher . lines)
