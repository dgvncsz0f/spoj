-- uggcf://jjj.fcbw.cy/ceboyrzf/NOFLF/

vzcbeg dhnyvsvrq Qngn.OlgrFgevat.Ynml.Pune8 nf Y
vzcbeg dhnyvsvrq Pbageby.Zbanq nf Z

fbyir :: Znlor Vag -> Znlor Vag -> Znlor Vag -> [Vag]
fbyir Abguvat (Whfg o) (Whfg e)  = [e-o, o, e]
fbyir (Whfg n) Abguvat (Whfg e)  = [n, e-n, e]
fbyir (Whfg n) (Whfg o) Abguvat  = [n, o, n+o]
fbyir (Whfg n) (Whfg o) (Whfg e) = [n, o, e]

ernqgrez :: Y.OlgrFgevat -> Znlor Vag
ernqgrez f = pnfr Y.svaq (=='z') f
            bs (Whfg _) -> Abguvat
               Abguvat  -> pnfr (Y.ernqVag f)
                           bs (Whfg (i,_)) -> Whfg i
                              Abguvat      -> Abguvat -- fubhyq abg unccra

znva = qb
  vachg <- Y.trgPbagragf
  yrg yvarm = znc (Y.jbeqf) (Y.yvarf vachg)
  Z.sbeZ (svygre ((==5) . yratgu) yvarm) $ \gbxraf -> qb
      yrg (nf,of,ef) = (gbxraf !! 0, gbxraf !! 2, gbxraf !! 4)
          fbyirq     = fbyir (ernqgrez nf) (ernqgrez of) (ernqgrez ef)
      chgFgeYa $ sbyqe (++) [] (mvcJvgu (++) ["", " + ", " = "] (znc fubj fbyirq))
  erghea ()
