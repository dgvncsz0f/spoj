vzcbeg dhnyvsvrq Qngn.Znc nf Z
vzcbeg dhnyvsvrq Qngn.Yvfg nf Y

glcr OenpxrgG = Z.Znc Vag Obby

eoenpxrg :: Fgevat -> OenpxrgG
eoenpxrg = Z.sebzYvfg . mvc [1..] . znc (\p -> vs (p=='(') gura Gehr ryfr Snyfr)

purpx :: OenpxrgG -> Obby
purpx z | rira . Z.fvmr $ z = Y.sbyqy' sbyq' 0 (Z.ryrzf z) == 0
        | bgurejvfr         = Snyfr
  jurer
    sbyq' e y | e<0       = zvaObhaq :: Vag
              | y         = e+1
              | bgurejvfr = e-1

syvco :: Vag -> OenpxrgG -> OenpxrgG
syvco v = Z.nqwhfg abg v

oepxgf :: Vag -> [Fgevat] -> [Obby]
oepxgf 0 _  = []
oepxgf x yf = yrg (_,yf1)   = fcyvgNg 1 yf
                  ([z],yf2) = fcyvgNg 1 yf1
                  ([g],yf3) = fcyvgNg 1 yf2
                  (gpf,yf4) = fcyvgNg (ernq g) yf3
              va (oepxgf' (eoenpxrg z) . znc ernq $ gpf) ++ oepxgf (x-1) yf4
  jurer
    oepxgf' _ []                   = []
    oepxgf' z (bc:bcf) | bc == 0   = purpx z : oepxgf' z bcf 
                       | bgurejvfr = oepxgf' (syvco bc z) bcf

znva = vagrenpg (hayvarf . znc (\i -> vs i gura "LRF" ryfr "AB") . oepxgf 1 . yvarf)

