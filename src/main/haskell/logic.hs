import qualified Data.Map as M
import qualified Maybe as M1

type Input = M.Map String (Bool,Bool)
data Circuit = Circuit { runCircuit :: (Input -> (Input,[Bool])) }

feed :: [[String]] -> [Bool] -> Input
feed kss = foldr feed' M.empty . zip kss
  where 
    feed' (ks,v) m = foldr (\k m1 -> addinput m1 (k:[]) v) m ks

addinput :: Input -> [String] -> Bool -> Input
addinput m [] _     = m
addinput m (k:ks) v = case (M.lookup k m)
                      of Nothing       -> addinput (M.insert k (v,False) m) ks v
                         (Just (v1,_)) -> addinput (M.insert k (v1,v) m) ks v

fetch :: Circuit
fetch = Circuit $ \m -> (m,[])

output :: String -> [String] -> Circuit
output k0 k1 = Circuit $ \m -> let v = fst . M1.fromJust . M.lookup k0 $ m
                               in (addinput m (k0:k1) v, [v])

binary :: (Bool -> Bool -> Bool) -> String -> [String] -> Circuit
binary op k0 k1 = Circuit $ \m -> let v0 = M1.fromJust . M.lookup k0 $ m
                                      v1 = fst v0
                                      v2 = snd v0
                                      v3 = op v1 v2
                                  in (addinput m k1 v3, [])

unary :: (Bool -> Bool) -> String -> [String] -> Circuit
unary op k0 k1 = Circuit $ \m -> let v0 = fst . M1.fromJust . M.lookup k0 $ m
                                     v1 = op v0
                                 in (addinput m k1 v1, [])

infixl 1 |>>
(|>>) :: Circuit -> Circuit -> Circuit
(|>>) c1 c2 = Circuit $ \m ->
  let (m',o)   = runCircuit c1 m
      (m'',o') = runCircuit c2 m'
  in (m'',o++o')

rcircuit :: [String] -> (([[String]],Circuit),[String])
rcircuit s = (foldr combine (([],fetch)) . map rline $ l, drop 1 r)
  where
    (l,r) = break (=="end") s

    combine ([],Nothing) (c,d) = (c,d)
    combine (a,Nothing)  (c,d) = (a:c,d)
    combine ([],Just b)  (c,d) = (c,b|>>d)
    combine (a,Just b)   (c,d) = (a:c,b|>>d)
    
    rline (i:j:o:_:cs) | i=='e' = ([],Nothing)
                       | o=='i' = (words . filter (/='.') $ cs, Nothing)
                       | o=='o' = ([],Just $ output [i,j] . words . filter (/='.') $ cs)
                       | o=='&' = ([],Just $ binary (&&) [i,j] . words . filter (/='.') $ cs)
                       | o=='|' = ([],Just $ binary (||) [i,j] . words . filter (/='.') $ cs)
                       | o=='!' = ([],Just $ unary   not [i,j] . words . filter (/='.') $ cs)

rvalue :: String -> [Bool]
rvalue (t:ts) | t=='0' = False : rvalue ts
              | t=='1' = True  : rvalue ts

logic :: Int -> [String] -> [[Bool]]
logic 0 _  = []
logic _ [] = []
logic n i  = let (c0,(l0:ls0)) = rcircuit i
                 t             = read l0
                 (l1,ls1)      = splitAt t ls0
                 vs            = map rvalue l1
                 c             = snd . runCircuit (snd c0) . feed (fst c0)
             in ((map c $ vs) ++ [[]]) ++ logic (n-1) ls1

main = do
  getLine >>= \n ->
    interact (unlines . map showb . logic (read n) . filter (not.null) . lines)
  where 
    showb []                 = []
    showb (t:ts) | t         = '1' : showb ts
                 | otherwise = '0' : showb ts
