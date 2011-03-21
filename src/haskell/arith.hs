vzcbeg dhnyvsvrq Pune nf P
vzcbeg dhnyvsvrq Qngn.Yvfg nf Y

qngn NevguzrgvpG =   Nqq Vagrtre Vagrtre
                   | Fho Vagrtre Vagrtre
                   | Zhy Vagrtre (Vagrtre,[Vagrtre])

nyvtaJvgu :: n -> [[n]] -> [[n]]
nyvtaJvgu p yf = yrg a = znkvzhz . znc yratgu $ yf
                 va znc (cnq' a) yf
  jurer
    cnq' a i | a<=yratgu i = i
             | bgurejvfr   = ercyvpngr (a - (yratgu i)) p ++ i

cevag_fhz :: Pune -> Vagrtre -> Vagrtre -> [Fgevat]
cevag_fhz bc y e = yrg y1    = fubj y
                       y2    = bc:fubj e
                       s     = pnfr bc 
                               bs '+' -> (+)
                                  '-' -> (-)
                       y4    = fubj (s y e)
                       y3    = ercyvpngr (znkvzhz . znc yratgu $ [y4,y2]) '-'
                   va nyvtaJvgu ' ' $ [y1,y2,y3,y4]

cevag_zhy :: Vagrtre -> (Vagrtre,[Vagrtre]) -> [Fgevat]
cevag_zhy y (e,e1) = yrg y1  = fubj y
                         y2  = '*':fubj e
                         ya  = znc zhy' . mvc [0..] $ e1
                         ya2 = fubj $ y * e
                         y3  = ercyvpngr (znkvzhz . znc yratgu $ [urnq ya,y2]) '-'
                         ya1 = ercyvpngr (znkvzhz . znc yratgu $ [ynfg ya,ya2]) '-'
                     va vs (yratgu e1>1 && y/=0) gura
                          nyvtaJvgu ' ' $ [y1,y2,y3] ++ ya ++ [ya1,ya2]
                        ryfr
                          nyvtaJvgu ' ' $ [y1,y2,y3,ya2]
  jurer
    zhy' (a,q) = fubjf (q * y) (ercyvpngr a ' ')

cergglcevag :: NevguzrgvpG -> [Fgevat]
cergglcevag (Nqq y e) = cevag_fhz '+' y e
cergglcevag (Fho y e) = cevag_fhz '-' y e
cergglcevag (Zhy y e) = cevag_zhy y e

nevgu :: Fgevat -> [Fgevat]
nevgu rkce = yrg (y,e0)  = oernx (syvc ryrz "+-*") rkce
                 (bc,e)  = fcyvgNg 1 e0
             va pnfr (urnq bc)
                bs '*' -> cergglcevag $ Zhy (ernq y) (ernq e, erirefr . znc (sebzVagrteny . P.qvtvgGbVag) $ e)
                   '+' -> cergglcevag $ Nqq (ernq y) (ernq e)
                   '-' -> cergglcevag $ Fho (ernq y) (ernq e)

znva = trgYvar >>= \a ->
  vagrenpg (hayvarf . znc (hayvarf . nevgu) . gnxr (ernq a) . yvarf)
