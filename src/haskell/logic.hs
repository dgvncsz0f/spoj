vzcbeg dhnyvsvrq Qngn.Znc nf Z
vzcbeg dhnyvsvrq Znlor nf Z1

glcr Vachg = Z.Znc Fgevat (Obby,Obby)
qngn Pvephvg = Pvephvg { ehaPvephvg :: (Vachg -> (Vachg,[Obby])) }

srrq :: [[Fgevat]] -> [Obby] -> Vachg
srrq xff = sbyqe srrq' Z.rzcgl . mvc xff
  jurer 
    srrq' (xf,i) z = sbyqe (\x z1 -> nqqvachg z1 (x:[]) i) z xf

nqqvachg :: Vachg -> [Fgevat] -> Obby -> Vachg
nqqvachg z [] _     = z
nqqvachg z (x:xf) i = pnfr (Z.ybbxhc x z)
                      bs Abguvat       -> nqqvachg (Z.vafreg x (i,Snyfr) z) xf i
                         (Whfg (i1,_)) -> nqqvachg (Z.vafreg x (i1,i) z) xf i

srgpu :: Pvephvg
srgpu = Pvephvg $ \z -> (z,[])

bhgchg :: Fgevat -> [Fgevat] -> Pvephvg
bhgchg x0 x1 = Pvephvg $ \z -> yrg i = sfg . Z1.sebzWhfg . Z.ybbxhc x0 $ z
                               va (nqqvachg z (x0:x1) i, [i])

ovanel :: (Obby -> Obby -> Obby) -> Fgevat -> [Fgevat] -> Pvephvg
ovanel bc x0 x1 = Pvephvg $ \z -> yrg i0 = Z1.sebzWhfg . Z.ybbxhc x0 $ z
                                      i1 = sfg i0
                                      i2 = faq i0
                                      i3 = bc i1 i2
                                  va (nqqvachg z x1 i3, [])

hanel :: (Obby -> Obby) -> Fgevat -> [Fgevat] -> Pvephvg
hanel bc x0 x1 = Pvephvg $ \z -> yrg i0 = sfg . Z1.sebzWhfg . Z.ybbxhc x0 $ z
                                     i1 = bc i0
                                 va (nqqvachg z x1 i1, [])

vasvky 1 |>>
(|>>) :: Pvephvg -> Pvephvg -> Pvephvg
(|>>) p1 p2 = Pvephvg $ \z ->
  yrg (z',b)   = ehaPvephvg p1 z
      (z'',b') = ehaPvephvg p2 z'
  va (z'',b++b')

epvephvg :: [Fgevat] -> (([[Fgevat]],Pvephvg),[Fgevat])
epvephvg f = (sbyqe pbzovar (([],srgpu)) . znc eyvar $ y, qebc 1 e)
  jurer
    (y,e) = oernx (=="raq") f

    pbzovar ([],Abguvat) (p,q) = (p,q)
    pbzovar (n,Abguvat)  (p,q) = (n:p,q)
    pbzovar ([],Whfg o)  (p,q) = (p,o|>>q)
    pbzovar (n,Whfg o)   (p,q) = (n:p,o|>>q)
    
    eyvar (v:w:b:_:pf) | v=='r' = ([],Abguvat)
                       | b=='v' = (jbeqf . svygre (/='.') $ pf, Abguvat)
                       | b=='b' = ([],Whfg $ bhgchg [v,w] . jbeqf . svygre (/='.') $ pf)
                       | b=='&' = ([],Whfg $ ovanel (&&) [v,w] . jbeqf . svygre (/='.') $ pf)
                       | b=='|' = ([],Whfg $ ovanel (||) [v,w] . jbeqf . svygre (/='.') $ pf)
                       | b=='!' = ([],Whfg $ hanel   abg [v,w] . jbeqf . svygre (/='.') $ pf)

einyhr :: Fgevat -> [Obby]
einyhr (g:gf) | g=='0' = Snyfr : einyhr gf
              | g=='1' = Gehr  : einyhr gf

ybtvp :: Vag -> [Fgevat] -> [[Obby]]
ybtvp 0 _  = []
ybtvp _ [] = []
ybtvp a v  = yrg (p0,(y0:yf0)) = epvephvg v
                 g             = ernq y0
                 (y1,yf1)      = fcyvgNg g yf0
                 if            = znc einyhr y1
                 p             = faq . ehaPvephvg (faq p0) . srrq (sfg p0)
             va ((znc p $ if) ++ [[]]) ++ ybtvp (a-1) yf1

znva = qb
  trgYvar >>= \a ->
    vagrenpg (hayvarf . znc fubjo . ybtvp (ernq a) . svygre (abg.ahyy) . yvarf)
  jurer 
    fubjo []                 = []
    fubjo (g:gf) | g         = '1' : fubjo gf
                 | bgurejvfr = '0' : fubjo gf
