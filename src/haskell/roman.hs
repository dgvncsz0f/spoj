import qualified Data.ByteString.Lazy.Char8 as B
import Data.Array
import Control.Monad

nroman = listArray (0,8) [listArray (0,9) [("I",0),("X",0),("C",0),("M",0),("X",1),("C",1),("M",1),("x",0),("c",0),("m",0)],
  listArray (0,9) [("II",0),("XX",0),("CC",0),("MM",0),("XX",1),("CC",1),("MM",1),("xx",0),("cc",0),("mm",0)],
  listArray (0,9) [("III",0),("XXX",0),("CCC",0),("MMM",0),("XXX",1),("CCC",1),("MMM",1),("xxx",0),("ccc",0),("mmm",0)],
  listArray (0,9) [("IV",0),("XL",0),("CD",0),("IV",1),("XL",1),("CD",1),("iv",0),("xl",0),("cd",0),("iv",1)],
  listArray (0,8) [("V",0),("L",0),("D",0),("V",1),("L",1),("D",1),("v",0),("l",0),("d",0)],
  listArray (0,8) [("VI",0),("LX",0),("DC",0),("VI",1),("LX",1),("DC",1),("vi",0),("lx",0),("dc",0)],
  listArray (0,8) [("VII",0),("LXX",0),("DCC",0),("VII",1),("LXX",1),("DCC",1),("vii",0),("lxx",0),("dcc",0)],
  listArray (0,8) [("VIII",0),("LXXX",0),("DCCC",0),("VIII",1),("LXXX",1),("DCCC",1),("viii",0),("lxxx",0),("dccc",0)],
  listArray (0,8) [("IX",0),("XC",0),("CM",0),("IX",1),("XC",1),("CM",1),("ix",0),("xc",0),("cm",0)]]

renglish = zip [0..] . reverse . map readint' . B.words
  where
    ns' = [(B.pack "OH",0),(B.pack "ZERO",0),(B.pack "ONE",1),(B.pack "TWO",2),(B.pack "THREE",3),(B.pack "FOUR",4),(B.pack "FIVE",5),(B.pack "SIX",6),(B.pack "SEVEN",7),(B.pack "EIGHT",8),(B.pack "NINE",9)]
    readint' s = case (lookup s ns')
                 of (Just v) -> v

sroman [] = []
sroman ((e,b):as) |b==0      = sroman as
                  |otherwise = f':sroman as
  where
    f' = let (l,r) = nroman ! (b-1) ! e
         in (l,r==1)

main = do
  input <- B.getContents
  let linez  = B.lines input
      n      = foldr (\(e,b) c -> c+b*10^e) 0 (renglish (head linez))
      tcases = tail linez
  forM (take n tcases) $ \tcase -> do
    let roman = reverse (sroman (renglish tcase))
        rnum  = map fst roman
        over  = map (\(n,o) -> if o then map (\_ -> '_') n else map (\_ -> ' ') n) roman
    putStrLn $ concat over
    putStrLn $ concat rnum
  return ()
