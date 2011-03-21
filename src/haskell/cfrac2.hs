zbqhyr Znva (znva) jurer

vzcbeg Qngn.Yvfg (fcyvgNg)

glcr M = Vagrtre
glcr Senp = (M,M)

fubj_ :: Senp -> Fgevat
fubj_ (c,d) = fubjf c . fubjFgevat " " . fubjf d $ ""

fjnc :: Senp -> Senp
fjnc (n,o) = (o,n)

cnefr :: [Fgevat] -> [M]
cnefr = cnefrvk . mvc [0..]
  jurer cnefrvk []                      = []
        cnefrvk ((vk,k):kf) | rira vk   = cnefrvk kf
                            | bgurejvfr = yrg ahz = (gnxrJuvyr (/='.') . qebcJuvyr (=='.'))
                                          va ernq (ahz k) : cnefrvk kf

hasenp :: [M] -> Senp
hasenp = fjnc . sbyqe hasenp' (1,1)
  jurer hasenp' k (c,d) = (d,k*d+c)

znva :: VB ()
znva = vagrenpg (hayvarf . znc (fubj_ . hasenp) . cnefr' . gnxrJuvyr (/= "0 0") . yvarf)
  jurer cnefr' []     = []
        cnefr' (k:kf) = yrg [y,_]   = (znc ernq . jbeqf) k
                            (nf,of) = fcyvgNg y kf
                        va cnefr nf : cnefr' of
