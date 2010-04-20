module Main (main) where

type Z = Integer
type CFrac = [Z]
type Frac = (Z,Z)

-- cfrac -- 
data Document =   Text String Document
                | Line Int Document
                | Space Int Document
                | Nil

instance Show Document where
  showsPrec _ Nil         = showString ""
  showsPrec _ (Space k d) = showString (replicate k '.') . shows d
  showsPrec _ (Line k d)  = showString "\n" . showString (replicate k '.') . shows d
  showsPrec _ (Text s d)  = showString s . shows d

render :: CFrac -> Document
render []  = Nil
render [x] = let integral = shows (x-1) ".+.-"
                 padding  = length integral - 1
             in      space padding (text "1")
                 +++ line (text integral)
                 +++ shift padding (line (text "1"))
render (x:xs) = let integral = shows x ".+."
                    padding  = length integral
                    dem      = render xs
                    dems     = columns dem
                in      space padding (ctext "1" dems)
                    +++ line ((text integral) +++ (chars dems '-'))
                    +++ shift padding (line dem)

chars :: Int -> Char -> Document
chars k c = text (replicate k c)

ctext :: String -> Int -> Document
ctext s n = space m (text s) +++ space (n-m-sn) Nil
  where sn = length s
        m  = max (div (n - sn) 2) 0

text :: String -> Document
text = flip Text Nil

line :: Document -> Document
line = Line 0

space :: Int -> Document -> Document
space m (Space n d) = Space (m+n) d
space m d           = Space m d

shift :: Int -> Document -> Document
shift _ Nil         = Nil
shift m (Text s d)  = Text s (shift m d)
shift m (Line n d)  = Line (m+n) (shift m d)
shift m (Space n d) = Space n (shift m d)

columns :: Document -> Int
columns (Text s d)  = length s + (columns d)
columns (Space m d) = m + (columns d)
columns _           = 0

(+++) :: Document -> Document -> Document
(Text s d) +++ x  = Text s (d +++ x)
(Line n d) +++ x  = Line n (d +++ x)
(Space n d) +++ x = Space n (d +++ x)
Nil +++ x         = x
infixr 9 +++

cfraction :: Z -> Z -> CFrac
cfraction a b = egcd id a b []
  where egcd accum _ 0 = accum
        egcd accum x y = let (q,r) = divMod x y
                         in egcd (accum . (q:)) y r

-- cfrac2 -- 
show_ :: Frac -> String
show_ (p,q) = shows p . showString " " . shows q $ ""

swap :: Frac -> Frac
swap (a,b) = (b,a)

parse :: [String] -> [Z]
parse = parseix . zip [0..]
  where parseix []                      = []
        parseix ((ix,x):xs) | even ix   = parseix xs
                            | otherwise = let num = (takeWhile (/='.') . dropWhile (=='.'))
                                          in read (num x) : parseix xs

unfrac :: [Z] -> Frac
unfrac = swap . foldr unfrac' (1,1)
  where unfrac' x (p,q) = (q,x*q+p)

cfrac :: String -> Document
cfrac s = let [p,q] = map read (words s)
          in render (cfraction p q)

cfrac2 :: [String] -> Frac
cfrac2 = unfrac . parse

main :: IO ()
main = interact (unlines . acfrac 1 . takeWhile (/= "C") . lines)
  where acfrac _  []           = []
        acfrac ix ("A":xs:xss) = case_ ix (show . cfrac $  xs) : acfrac (ix+1) xss
        acfrac ix ("B":xss)    = let (axss,bxss) = break (`elem` ["A","B"]) xss
                                 in case_ ix (show_ . cfrac2 $ axss) : acfrac (ix+1) bxss

        case_ ix = showString "Case " . shows ix . showString ":\n"

