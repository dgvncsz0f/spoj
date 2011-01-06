-- https://www.spoj.pl/problems/DNA/

module Main where

import Data.List (intercalate)
import Control.Monad (mapM)

data Trie a = Trie { value    :: Maybe a 
                   , children :: [(Char,Trie a)]
                   }
  deriving (Show)

find :: Trie a -> String -> Maybe a
find t []     = value t
find t (k:ks) = case lookup k (children t)
                of Nothing -> Nothing
                   Just t' -> find t' ks

type DNA     = String
type Codon   = String
type Protein = [Codon]

protdb :: Trie String
protdb = let phe = Trie (Just "Phe") []
             ser = Trie (Just "Ser") []
             tyr = Trie (Just "Tyr") []
             cys = Trie (Just "Cys") []
             leu = Trie (Just "Leu") []
             trp = Trie (Just "Trp") []
             pro = Trie (Just "Pro") []
             his = Trie (Just "His") []
             arg = Trie (Just "Arg") []
             gln = Trie (Just "Gln") []
             ile = Trie (Just "Ile") []
             thr = Trie (Just "Thr") []
             asn = Trie (Just "Asn") []
             lys = Trie (Just "Lys") []
             met = Trie (Just "Met") []
             val = Trie (Just "Val") []
             ala = Trie (Just "Ala") []
             asp = Trie (Just "Asp") []
             gly = Trie (Just "Gly") []
             glu = Trie (Just "Glu") []
         in Trie Nothing [('U',Trie Nothing [('U',Trie Nothing [('U',phe)
                                                               ,('C',phe)
                                                               ,('A',leu)
                                                               ,('G',leu)
                                                               ])
                                            ,('C',Trie Nothing [('U',ser)
                                                               ,('C',ser)
                                                               ,('A',ser)
                                                               ,('G',ser)
                                                               ])
                                            ,('A',Trie Nothing [('U',tyr)
                                                               ,('C',tyr)
                                                               ])
                                            ,('G',Trie Nothing [('U',cys)
                                                               ,('C',cys)
                                                               ,('G',trp)
                                                               ])
                                            ])
                         ,('C',Trie Nothing [('U',Trie Nothing [('U',leu)
                                                               ,('C',leu)
                                                               ,('A',leu)
                                                               ,('G',leu)
                                                               ])
                                            ,('C',Trie Nothing [('U',pro)
                                                               ,('C',pro)
                                                               ,('A',pro)
                                                               ,('G',pro)
                                                               ])
                                            ,('A',Trie Nothing [('U',his)
                                                               ,('C',his)
                                                               ,('A',gln)
                                                               ,('G',gln)
                                                               ])
                                            ,('G',Trie Nothing [('U',arg)
                                                               ,('C',arg)
                                                               ,('A',arg)
                                                               ,('G',arg)
                                                               ])
                                            ])
                         ,('A',Trie Nothing [('U',Trie Nothing [('U',ile)
                                                               ,('C',ile)
                                                               ,('A',ile)
                                                               ,('G',met)
                                                               ])
                                            ,('C',Trie Nothing [('U',thr)
                                                               ,('C',thr)
                                                               ,('A',thr)
                                                               ,('G',thr)
                                                               ])
                                            ,('A',Trie Nothing [('U',asn)
                                                               ,('C',asn)
                                                               ,('A',lys)
                                                               ,('G',lys)
                                                               ])
                                            ,('G',Trie Nothing [('U',ser)
                                                               ,('C',ser)
                                                               ,('A',arg)
                                                               ,('G',arg)
                                                               ])
                                            ])
                         ,('G',Trie Nothing [('U',Trie Nothing [('U',val)
                                                               ,('C',val)
                                                               ,('A',val)
                                                               ,('G',val)
                                                               ])
                                            ,('C',Trie Nothing [('U',ala)
                                                               ,('C',ala)
                                                               ,('A',ala)
                                                               ,('G',ala)
                                                               ])
                                            ,('A',Trie Nothing [('U',asp)
                                                               ,('C',asp)
                                                               ,('A',glu)
                                                               ,('G',glu)
                                                               ])
                                            ,('G',Trie Nothing [('U',gly)
                                                               ,('C',gly)
                                                               ,('A',gly)
                                                               ,('G',gly)
                                                               ])
                                            ])
                         ]

complement :: DNA -> DNA
complement (b:bs) | b=='A'    = 'T' : complement bs
                  | b=='T'    = 'A' : complement bs
                  | b=='C'    = 'G' : complement bs
                  | b=='G'    = 'C' : complement bs
                  | otherwise = b   : complement bs
complement [] = []

transcription :: DNA -> DNA
transcription = transcript
  where transcript (d:ds) | d=='T'    = 'U' : transcript ds
                          | otherwise = d : transcript ds
        transcript []                 = []

codons :: DNA -> [Codon]
codons (a:b:c:ds) = [a,b,c] : codons ds
codons _          = []

codons_prfxs :: DNA -> [(Codon,[Codon])]
codons_prfxs (a:b:c:ds) = ([a,b,c],codons ds) : codons_prfxs (b:c:ds)
codons_prfxs _          = []

proteins :: [(Codon,[Codon])] -> Protein
proteins codns | null codns = []
               | otherwise  = let (_,start)   = break ((=="AUG") . fst) codns 
                                  (found,end) = (break (flip elem ["UAA","UAG","UGA"]) . snd . head) start
                                  trymore     = proteins (drop 1 start)
                              in if (null start || null found || null end)
                                 then trymore
                                 else found

dna2proteins :: String -> Maybe Protein
dna2proteins strand 
  | nnull comptary  = Just comptary
  | nnull rcomptary = Just rcomptary
  | nnull rprimary  = Just rprimary
  | nnull primary   = Just primary
  | otherwise       = Nothing
  where primary   = (proteins . codons_prfxs . transcription . complement) strand

        rprimary  = (proteins . codons_prfxs . transcription . complement . reverse) strand

        comptary  = (proteins . codons_prfxs . transcription) strand

        rcomptary = (proteins . codons_prfxs . transcription . reverse) strand

        nnull     = not . null

main :: IO ()
main = interact (unlines . map (showmaybe . showprot . dna2proteins) . takeWhile (/="*") . lines)
  where showprot prot = prot >>= mapM (find protdb) >>= return . intercalate "-"
        
        showmaybe v = case v
                      of Nothing -> "*** No translatable DNA found ***"
                         Just v' -> v'

