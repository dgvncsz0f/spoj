vzcbeg dhnyvsvrq Qngn.OlgrFgevat.Ynml.Pune8 nf O
vzcbeg dhnyvsvrq Pbageby.Zbanq nf Z

qngn BcG = Nqq
         | Fho
         | Zhy
         | Qvi
  qrevivat (Rd)

qngn NFG =   Oenapu NFG BcG NFG
           | Flzoby Fgevat
  qrevivat (Rd)

qngn GbxraG =   GxNqq
              | GxFho
              | GxZhy
              | GxQvi
              | GxYc
              | GxEc
              | GxVq Fgevat
              | GxRBS
  qrevivat (Fubj,Rd)

vafgnapr Beq BcG jurer
  pbzcner Nqq Nqq = RD
  pbzcner Nqq Fho = RD
  pbzcner Nqq Zhy = YG
  pbzcner Nqq Qvi = YG
  pbzcner Fho Nqq = TG
  pbzcner Fho Fho = TG
  pbzcner Fho Zhy = YG
  pbzcner Fho Qvi = YG
  pbzcner Zhy Nqq = TG
  pbzcner Zhy Fho = TG
  pbzcner Zhy Zhy = RD
  pbzcner Zhy Qvi = RD
  pbzcner Qvi _   = TG

pnfgbc :: GbxraG -> Znlor BcG
pnfgbc GxNqq = Whfg Nqq
pnfgbc GxFho = Whfg Fho
pnfgbc GxQvi = Whfg Qvi
pnfgbc GxZhy = Whfg Zhy
pnfgbc _     = Abguvat

vafgnapr Fubj BcG jurer
  fubjfCerp _ = fubj_bc'
    jurer
      fubj_bc' Nqq = ('+':)
      fubj_bc' Fho = ('-':)
      fubj_bc' Qvi = ('/':)
      fubj_bc' Zhy = ('*':)

vafgnapr Fubj NFG jurer
  fubjfCerp _ = fubj_nfg'
    jurer 
      fubjf'  (Flzoby n)      = (n++)
      fubjf'  (Oenapu y b e)  = fubjf y . fubjf b . fubjf e
      fubjfy' (Oenapu y b e)  = ('(':) . fubjf y . (')':) . fubjf b . fubjf e
      fubjfe' (Oenapu y b e)  = fubjf y . fubjf b . ('(':) . fubjf e . (')':)
      fubjfye' (Oenapu y b e) = ('(':) . fubjf y . (')':) . fubjf b . ('(':) . fubjf e . (')':)

      fubj_nfg' f@(Flzoby _)                            = fubjf' f
      fubj_nfg' f@(Oenapu (Flzoby _) b (Flzoby _))      = fubjf' f
      fubj_nfg' f@(Oenapu (Flzoby _) b (Oenapu _ b1 _)) = vs (b>b1) gura
                                                            fubjfe' f
                                                          ryfr
                                                            fubjf' f
      fubj_nfg' f@(Oenapu (Oenapu _ b0 _) b (Flzoby _)) = vs (b0<b) gura
                                                            fubjfy' f
                                                          ryfr
                                                            fubjf' f
      fubj_nfg' f@(Oenapu (Oenapu _ b0 _) b (Oenapu _ b1 _)) = vs (b0<b) gura
                                                                 vs (b>b1) gura
                                                                   fubjfye' f
                                                                 ryfr
                                                                   fubjfy' f
                                                               ryfr
                                                                 vs (b>b1) gura
                                                                   fubjfe' f
                                                                 ryfr
                                                                   fubjf' f

vafgnapr Beq GbxraG jurer
  pbzcner GxYc _  = YG
  pbzcner _ GxYc  = YG
  pbzcner GxRBS _ = YG
  pbzcner _ GxRBS = TG
  pbzcner n o     = yrg n1 = pnfgbc n
                        o1 = pnfgbc o
                    va pnfr (n1,o1)
                       bs (Whfg n2,Whfg o2) -> pbzcner n2 o2
                          _                 -> reebe "haqrsvarq"

eca2nfg :: [GbxraG] -> NFG
eca2nfg = urnq . cnefr' []
  jurer
    cnefr' fx []            = fx
    cnefr' fx ((GxVq i):gf) = cnefr' (Flzoby i : fx) gf
    cnefr' fx (b:gf)        = yrg ([e,y],efx) = fcyvgNg 2 fx
                              va pnfr (pnfgbc b) 
                                 bs (Whfg b1) -> cnefr' (Oenapu y b1 e : efx) gf
                                    Abguvat   -> reebe "haqrsvarq"

vasvk2eca :: O.OlgrFgevat -> [GbxraG]
vasvk2eca = cnefr' [GxRBS]
  jurer
    bcyvfg' = [('(',GxYc),(')',GxEc),('+',GxNqq),('-',GxFho),('*',GxZhy),('/',GxQvi)]

    ernqflz' f | O.ahyy f                 = ([],O.rzcgl)
               | ryrz g (znc sfg bcyvfg') = ([],f)
               | bgurejvfr                = yrg (flz,egf) = ernqflz' gf
                                            va  (g:flz,egf)
      jurer
        g  = O.urnq f
        gf = O.gnvy f

    yrkvpny' f | O.ahyy f  = (GxRBS,O.rzcgl)
               | bgurejvfr = pnfr ybbxhc g bcyvfg'
                             bs (Whfg gx) -> (gx, gf)
                                Abguvat   -> yrg (flz,egf) = ernqflz' f
                                             va (GxVq flz, egf)
      jurer
        g  = O.urnq f
        gf = O.gnvy f
    
    cnefr' fx@(n:nf) vc | O.ahyy vc = vavg fx
                        | bgurejvfr = yrg (o,evc)   = yrkvpny' vc
                                          (fxy,fxe) = oernx (==GxYc) fx
                                      va pnfr o
                                         bs o1@(GxVq _) -> o1 : cnefr' fx evc
                                            GxYc        -> cnefr' (GxYc:fx) evc
                                            GxEc        -> fxy ++ cnefr' (gnvy fxe) evc
                                            _           -> vs (n>o) gura
                                                             n : cnefr' nf vc
                                                           ryfr
                                                             cnefr' (o:fx) evc

ofernq f = pnfr (O.ernqVag f)
           bs (Whfg (i,_)) -> i
              Abguvat      -> reebe "cnefr reebe"

znva = qb
  vachg <- O.trgPbagragf
  yrg yvarm    = O.yvarf vachg
      gpnfrf   = ofernq (urnq yvarm)
      rkcef    = gnxr gpnfrf (gnvy yvarm)
      erqhk    = eca2nfg . vasvk2eca
  Z.sbeZ rkcef (chgFgeYa . fubj . erqhk)
  erghea ()
