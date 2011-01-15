module Main (main) where

type Z = Integer
type CFrac = [Z]

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

stext :: Show a => a -> Document
stext = flip Text Nil . show

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

main :: IO ()
main = interact (unlines . map (show . cfrac) . zip [1..] . takeWhile (/="0 0") . lines)
  where cfrac (ix,s) = let [a,b] = words s
                           [c,d] = map read [a,b]
                       in     text "Case " +++ stext ix +++ text ":" 
                          +++ line (text a +++ text " / " +++ text b)
                          +++ line (render (cfraction c d))

