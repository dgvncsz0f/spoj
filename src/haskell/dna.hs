-- uggcf://jjj.fcbw.cy/ceboyrzf/QAN/

zbqhyr Znva jurer

vzcbeg Qngn.Yvfg (vagrepnyngr)
vzcbeg Pbageby.Zbanq (zncZ)

qngn Gevr n = Gevr { inyhr    :: Znlor n 
                   , puvyqera :: [(Pune,Gevr n)]
                   }
  qrevivat (Fubj)

svaq :: Gevr n -> Fgevat -> Znlor n
svaq g []     = inyhr g
svaq g (x:xf) = pnfr ybbxhc x (puvyqera g)
                bs Abguvat -> Abguvat
                   Whfg g' -> svaq g' xf

glcr QAN     = Fgevat
glcr Pbqba   = Fgevat
glcr Cebgrva = [Pbqba]

cebgqo :: Gevr Fgevat
cebgqo = yrg cur = Gevr (Whfg "Cur") []
             fre = Gevr (Whfg "Fre") []
             gle = Gevr (Whfg "Gle") []
             plf = Gevr (Whfg "Plf") []
             yrh = Gevr (Whfg "Yrh") []
             gec = Gevr (Whfg "Gec") []
             ceb = Gevr (Whfg "Ceb") []
             uvf = Gevr (Whfg "Uvf") []
             net = Gevr (Whfg "Net") []
             tya = Gevr (Whfg "Tya") []
             vyr = Gevr (Whfg "Vyr") []
             gue = Gevr (Whfg "Gue") []
             nfa = Gevr (Whfg "Nfa") []
             ylf = Gevr (Whfg "Ylf") []
             zrg = Gevr (Whfg "Zrg") []
             iny = Gevr (Whfg "Iny") []
             nyn = Gevr (Whfg "Nyn") []
             nfc = Gevr (Whfg "Nfc") []
             tyl = Gevr (Whfg "Tyl") []
             tyh = Gevr (Whfg "Tyh") []
         va Gevr Abguvat [('H',Gevr Abguvat [('H',Gevr Abguvat [('H',cur)
                                                               ,('P',cur)
                                                               ,('N',yrh)
                                                               ,('T',yrh)
                                                               ])
                                            ,('P',Gevr Abguvat [('H',fre)
                                                               ,('P',fre)
                                                               ,('N',fre)
                                                               ,('T',fre)
                                                               ])
                                            ,('N',Gevr Abguvat [('H',gle)
                                                               ,('P',gle)
                                                               ])
                                            ,('T',Gevr Abguvat [('H',plf)
                                                               ,('P',plf)
                                                               ,('T',gec)
                                                               ])
                                            ])
                         ,('P',Gevr Abguvat [('H',Gevr Abguvat [('H',yrh)
                                                               ,('P',yrh)
                                                               ,('N',yrh)
                                                               ,('T',yrh)
                                                               ])
                                            ,('P',Gevr Abguvat [('H',ceb)
                                                               ,('P',ceb)
                                                               ,('N',ceb)
                                                               ,('T',ceb)
                                                               ])
                                            ,('N',Gevr Abguvat [('H',uvf)
                                                               ,('P',uvf)
                                                               ,('N',tya)
                                                               ,('T',tya)
                                                               ])
                                            ,('T',Gevr Abguvat [('H',net)
                                                               ,('P',net)
                                                               ,('N',net)
                                                               ,('T',net)
                                                               ])
                                            ])
                         ,('N',Gevr Abguvat [('H',Gevr Abguvat [('H',vyr)
                                                               ,('P',vyr)
                                                               ,('N',vyr)
                                                               ,('T',zrg)
                                                               ])
                                            ,('P',Gevr Abguvat [('H',gue)
                                                               ,('P',gue)
                                                               ,('N',gue)
                                                               ,('T',gue)
                                                               ])
                                            ,('N',Gevr Abguvat [('H',nfa)
                                                               ,('P',nfa)
                                                               ,('N',ylf)
                                                               ,('T',ylf)
                                                               ])
                                            ,('T',Gevr Abguvat [('H',fre)
                                                               ,('P',fre)
                                                               ,('N',net)
                                                               ,('T',net)
                                                               ])
                                            ])
                         ,('T',Gevr Abguvat [('H',Gevr Abguvat [('H',iny)
                                                               ,('P',iny)
                                                               ,('N',iny)
                                                               ,('T',iny)
                                                               ])
                                            ,('P',Gevr Abguvat [('H',nyn)
                                                               ,('P',nyn)
                                                               ,('N',nyn)
                                                               ,('T',nyn)
                                                               ])
                                            ,('N',Gevr Abguvat [('H',nfc)
                                                               ,('P',nfc)
                                                               ,('N',tyh)
                                                               ,('T',tyh)
                                                               ])
                                            ,('T',Gevr Abguvat [('H',tyl)
                                                               ,('P',tyl)
                                                               ,('N',tyl)
                                                               ,('T',tyl)
                                                               ])
                                            ])
                         ]

pbzcyrzrag :: QAN -> QAN
pbzcyrzrag (o:of) | o=='N'    = 'G' : pbzcyrzrag of
                  | o=='G'    = 'N' : pbzcyrzrag of
                  | o=='P'    = 'T' : pbzcyrzrag of
                  | o=='T'    = 'P' : pbzcyrzrag of
                  | bgurejvfr = o   : pbzcyrzrag of
pbzcyrzrag [] = []

genafpevcgvba :: QAN -> QAN
genafpevcgvba = genafpevcg
  jurer genafpevcg (q:qf) | q=='G'    = 'H' : genafpevcg qf
                          | bgurejvfr = q : genafpevcg qf
        genafpevcg []                 = []

pbqbaf :: QAN -> [Pbqba]
pbqbaf (n:o:p:qf) = [n,o,p] : pbqbaf qf
pbqbaf _          = []

pbqbaf_ceskf :: QAN -> [(Pbqba,[Pbqba])]
pbqbaf_ceskf (n:o:p:qf) = ([n,o,p],pbqbaf qf) : pbqbaf_ceskf (o:p:qf)
pbqbaf_ceskf _          = []

cebgrvaf :: [(Pbqba,[Pbqba])] -> Cebgrva
cebgrvaf pbqaf | ahyy pbqaf = []
               | bgurejvfr  = yrg (_,fgneg)   = oernx ((=="NHT") . sfg) pbqaf 
                                  (sbhaq,raq) = (oernx (syvc ryrz ["HNN","HNT","HTN"]) . faq . urnq) fgneg
                                  gelzber     = cebgrvaf (qebc 1 fgneg)
                              va vs (ahyy fgneg || ahyy sbhaq || ahyy raq)
                                 gura gelzber
                                 ryfr sbhaq

qan2cebgrvaf :: Fgevat -> Znlor Cebgrva
qan2cebgrvaf fgenaq 
  | aahyy pbzcgnel  = Whfg pbzcgnel
  | aahyy epbzcgnel = Whfg epbzcgnel
  | aahyy ecevznel  = Whfg ecevznel
  | aahyy cevznel   = Whfg cevznel
  | bgurejvfr       = Abguvat
  jurer cevznel   = (cebgrvaf . pbqbaf_ceskf . genafpevcgvba . pbzcyrzrag) fgenaq

        ecevznel  = (cebgrvaf . pbqbaf_ceskf . genafpevcgvba . pbzcyrzrag . erirefr) fgenaq

        pbzcgnel  = (cebgrvaf . pbqbaf_ceskf . genafpevcgvba) fgenaq

        epbzcgnel = (cebgrvaf . pbqbaf_ceskf . genafpevcgvba . erirefr) fgenaq

        aahyy     = abg . ahyy

znva :: VB ()
znva = vagrenpg (hayvarf . znc (fubjznlor . fubjcebg . qan2cebgrvaf) . gnxrJuvyr (/="*") . yvarf)
  jurer fubjcebg cebg = cebg >>= zncZ (svaq cebgqo) >>= erghea . vagrepnyngr "-"
        
        fubjznlor i = pnfr i
                      bs Abguvat -> "*** Ab genafyngnoyr QAN sbhaq ***"
                         Whfg i' -> i'

