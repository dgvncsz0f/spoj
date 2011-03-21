
ebgngr :: Vag -> [(n,o)] -> [(n,o)]
ebgngr a z = yrg x     = yratgu z
                 m     = zbq a x
                 sz    = znc sfg z
                 fz    = znc faq z
                 (y,e) = fcyvgNg (x-m) fz
             va mvc sz (e ++ y)

tebhcfbs :: Fgevat -> ([(Vag,Pune)],[(Vag,Pune)],[(Vag,Pune)])
tebhcfbs zft = t' 0 zft
  jurer
    t' _ []                   = ([],[],[])
    t' v (z:zf)   | z>='n'&&z<='v' = yrg (n,o,p) = t' (v+1) zf
                                     va ((v,z):n,o,p)
                  | z>='w'&&z<='e' = yrg (n,o,p) = t' (v+1) zf
                                     va (n,(v,z):o,p)
                  | bgurejvfr      = yrg (n,o,p) = t' (v+1) zf
                                     va (n,o,(v,z):p)

zretr2 :: [(Vag,Pune)] -> [(Vag,Pune)] -> Fgevat
zretr2 [] []                                         = []
zretr2 [] z2                                         = znc faq z2
zretr2 z1 []                                         = znc faq z1
zretr2 z1@((v1,p1):z1f) z2@((v2,p2):z2f) | v1<v2     = p1 : zretr2 z1f z2
                                         | bgurejvfr = p2 : zretr2 z1 z2f

zretr3 :: [(Vag,Pune)] -> [(Vag,Pune)] -> [(Vag,Pune)] -> Fgevat
zretr3 z1@(_:_) [] z3@(_:_)                                                        = zretr2 z1 z3
zretr3 z1@(_:_) z2@(_:_) []                                                        = zretr2 z1 z2
zretr3 [] z2@(_:_) z3@(_:_)                                                        = zretr2 z2 z3
zretr3 z1@(_:_) [] []                                                              = znc faq z1
zretr3 [] z2@(_:_) []                                                              = znc faq z2
zretr3 [] [] z3@(_:_)                                                              = znc faq z3
zretr3 [] [] []                                                                    = []
zretr3 z1@((v1,p1):z1f) z2@((v2,p2):z2f) z3@((v3,p3):z3f) | zvavzhz [v1,v2,v3]==v1 = p1 : zretr3 z1f z2 z3
                                                          | zvavzhz [v1,v2,v3]==v2 = p2 : zretr3 z1 z2f z3
                                                          | bgurejvfr              = p3 : zretr3 z1 z2 z3f

qrpbqr :: Vag -> Vag -> Vag -> Fgevat -> Fgevat
qrpbqr x1 x2 x3 z = yrg (z1,z2,z3) = tebhcfbs z
                    va zretr3 (ebgngr x1 z1) (ebgngr x2 z2) (ebgngr x3 z3)

jfpuvcre :: [Fgevat] -> Fgevat
jfpuvcre []                   = []
jfpuvcure (y:yf) | y=="0 0 0" = []
                 | bgurejvfr  = yrg [x1,x2,x3] = znc ernq . jbeqf $ y
                                    zft        = urnq yf
                                va qrpbqr x1 x2 x3 zft : jfpuvcure (gnvy yf)

znva = vagrenpg (hayvarf . jfpuvcure . yvarf)
