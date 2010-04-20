import Data.List (foldl',maximum)

newtype Q = Q { unQ :: (Integer,Integer) }

data Qe = Qe Integer (Integer,[Integer])
  deriving (Eq)

numerator :: Q -> Integer
numerator = fst . unQ

denominator :: Q -> Integer
denominator = snd . unQ

norm :: Q -> Q
norm (Q (n,d)) = Q (n `div` m,d `div` m)
  where m = gcd n d

integral :: Q -> Integer
integral (Q (n,d)) = n `div` d

expansion :: Q -> (Integer,[Integer])
expansion q0 = (prePeriodSize,expansion' 0 q0' alpha)
  where q0'@(Q (_,d0)) = norm q0
        alpha          = q0' - (fromIntegral (integral q0'))
        prePeriodSize  = maximum (primePowers [2,5] d0)
        expansion' k m q | stop      = []
                         | otherwise = c : expansion' (k+1) m' q'
          where stop = numerator q==0 || (k>prePeriodSize && q==m)
                c  = integral (10 * q)
                q' = 10*q - fromIntegral c
                m' | k==prePeriodSize = q
                   | otherwise        = m
 
decimal :: Q -> Maybe Qe
decimal q | denominator q == 0 = Nothing
          | otherwise          = Just $ Qe (integral q) (expansion q)

primePowers :: [Integer] -> Integer -> [Integer]
primePowers es0 = primePowers' [] (map (const 0) es0) es0
  where primePowers' acc [] [] _                     = acc 
        primePowers' acc (e:es) (p:ps) n | r == 0    = primePowers' acc (e+1:es) (p:ps) q
                                         | otherwise = primePowers' (e:acc) es ps n
          where (q,r) = n `divMod` p

instance Num Q where
  (+) = plus
  (-) = minus
  (*) = mult
  abs (Q (n,d)) = Q (abs n,d)
  fromInteger n = Q (fromInteger n,1)
  signum (Q (n,_)) = Q (signum n,1)

instance Eq Q where
  (Q (a,b)) == (Q (c,d)) = (a,b) == (c,d)

instance Show Q where
  showsPrec _ (Q (n,d)) = shows n . showChar ' '
                                  . showChar '%'
                                  . showChar ' '
                                  . shows d

instance Show Qe where
  showsPrec _ (Qe n (sz,p0)) = shows n . showExpansion
    where (pp,p) = split sz id p0
          showPrePeriod = showChar '.' . foldl' (.) id (map shows pp)
          showPeriod    = showChar '(' . foldl' (.) id (map shows p)
                                       . showChar ')'
          showExpansion | null pp && null p = showChar '.' . showChar '0'
                        | null p            = showPrePeriod
                        | otherwise         = showPrePeriod . showPeriod
          
split :: Integer -> ([Integer]->[Integer]) -> [Integer] -> ([Integer],[Integer])
split 0 left (x:xs) = (left [],x:xs)
split _ left []     = (left [],[])
split k left (x:xs) = split (k-1) (left.(x:)) xs

mult :: Q -> Q -> Q
mult (Q (n0,d0)) (Q (n1,d1)) = Q (n0*n1,d0*d1)

plus :: Q -> Q -> Q
plus q (Q (0,_)) = q
plus (Q (0,_)) q = q
plus (Q (n0,d0)) (Q (n1,1))  = Q (n0+d0*n1,d0)
plus (Q (n0,1)) (Q (n1,d1))  = Q (n0*d1+n1,d1)
plus (Q (n0,d0)) (Q (n1,d1)) = Q (d0'*n0 + d1'*n1,d)
  where d   = lcm d0 d1
        d0' = d `div` d0
        d1' = d `div` d1

minus :: Q -> Q -> Q
minus q (Q (n,d)) = q + (Q (-n,d))

main :: IO ()
main = interact (unlines . root . lines)
  where root (t0:ts) = let t = read t0
                       in map (showJust.decimal.buildQ) . take t $ ts
        buildQ s = let [n,d] = map read . words $ s
                   in if (signum d == -1)
                      then Q (-n,abs d)
                      else Q (n,d)

        showJust Nothing  = "Invalid Input!!!"
        showJust (Just v) = show v

