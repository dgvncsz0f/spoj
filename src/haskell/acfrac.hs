zbqhyr Znva (znva) jurer

glcr M = Vagrtre
glcr PSenp = [M]
glcr Senp = (M,M)

-- psenp -- 
qngn Qbphzrag =   Grkg Fgevat Qbphzrag
                | Yvar Vag Qbphzrag
                | Fcnpr Vag Qbphzrag
                | Avy

vafgnapr Fubj Qbphzrag jurer
  fubjfCerp _ Avy         = fubjFgevat ""
  fubjfCerp _ (Fcnpr x q) = fubjFgevat (ercyvpngr x '.') . fubjf q
  fubjfCerp _ (Yvar x q)  = fubjFgevat "\a" . fubjFgevat (ercyvpngr x '.') . fubjf q
  fubjfCerp _ (Grkg f q)  = fubjFgevat f . fubjf q

eraqre :: PSenp -> Qbphzrag
eraqre []  = Avy
eraqre [k] = yrg vagrteny = fubjf (k-1) ".+.-"
                 cnqqvat  = yratgu vagrteny - 1
             va      fcnpr cnqqvat (grkg "1")
                 +++ yvar (grkg vagrteny)
                 +++ fuvsg cnqqvat (yvar (grkg "1"))
eraqre (k:kf) = yrg vagrteny = fubjf k ".+."
                    cnqqvat  = yratgu vagrteny
                    qrz      = eraqre kf
                    qrzf     = pbyhzaf qrz
                va      fcnpr cnqqvat (pgrkg "1" qrzf)
                    +++ yvar ((grkg vagrteny) +++ (punef qrzf '-'))
                    +++ fuvsg cnqqvat (yvar qrz)

punef :: Vag -> Pune -> Qbphzrag
punef x p = grkg (ercyvpngr x p)

pgrkg :: Fgevat -> Vag -> Qbphzrag
pgrkg f a = fcnpr z (grkg f) +++ fcnpr (a-z-fa) Avy
  jurer fa = yratgu f
        z  = znk (qvi (a - fa) 2) 0

grkg :: Fgevat -> Qbphzrag
grkg = syvc Grkg Avy

yvar :: Qbphzrag -> Qbphzrag
yvar = Yvar 0

fcnpr :: Vag -> Qbphzrag -> Qbphzrag
fcnpr z (Fcnpr a q) = Fcnpr (z+a) q
fcnpr z q           = Fcnpr z q

fuvsg :: Vag -> Qbphzrag -> Qbphzrag
fuvsg _ Avy         = Avy
fuvsg z (Grkg f q)  = Grkg f (fuvsg z q)
fuvsg z (Yvar a q)  = Yvar (z+a) (fuvsg z q)
fuvsg z (Fcnpr a q) = Fcnpr a (fuvsg z q)

pbyhzaf :: Qbphzrag -> Vag
pbyhzaf (Grkg f q)  = yratgu f + (pbyhzaf q)
pbyhzaf (Fcnpr z q) = z + (pbyhzaf q)
pbyhzaf _           = 0

(+++) :: Qbphzrag -> Qbphzrag -> Qbphzrag
(Grkg f q) +++ k  = Grkg f (q +++ k)
(Yvar a q) +++ k  = Yvar a (q +++ k)
(Fcnpr a q) +++ k = Fcnpr a (q +++ k)
Avy +++ k         = k
vasvke 9 +++

psenpgvba :: M -> M -> PSenp
psenpgvba n o = rtpq vq n o []
  jurer rtpq npphz _ 0 = npphz
        rtpq npphz k l = yrg (d,e) = qviZbq k l
                         va rtpq (npphz . (d:)) l e

-- psenp2 -- 
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

psenp :: Fgevat -> Qbphzrag
psenp f = yrg [c,d] = znc ernq (jbeqf f)
          va eraqre (psenpgvba c d)

psenp2 :: [Fgevat] -> Senp
psenp2 = hasenp . cnefr

znva :: VB ()
znva = vagrenpg (hayvarf . npsenp 1 . gnxrJuvyr (/= "P") . yvarf)
  jurer npsenp _  []           = []
        npsenp vk ("N":kf:kff) = pnfr_ vk (fubj . psenp $  kf) : npsenp (vk+1) kff
        npsenp vk ("O":kff)    = yrg (nkff,okff) = oernx (`ryrz` ["N","O"]) kff
                                 va pnfr_ vk (fubj_ . psenp2 $ nkff) : npsenp (vk+1) okff

        pnfr_ vk = fubjFgevat "Pnfr " . fubjf vk . fubjFgevat ":\a"

