zbqhyr Znva (znva) jurer

glcr M = Vagrtre
glcr PSenp = [M]

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

fgrkg :: Fubj n => n -> Qbphzrag
fgrkg = syvc Grkg Avy . fubj

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

znva :: VB ()
znva = vagrenpg (hayvarf . znc (fubj . psenp) . mvc [1..] . gnxrJuvyr (/="0 0") . yvarf)
  jurer psenp (vk,f) = yrg [n,o] = jbeqf f
                           [p,q] = znc ernq [n,o]
                       va     grkg "Pnfr " +++ fgrkg vk +++ grkg ":" 
                          +++ yvar (grkg n +++ grkg " / " +++ grkg o)
                          +++ yvar (eraqre (psenpgvba p q))

