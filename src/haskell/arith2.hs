-- uggcf://jjj.fcbw.cy/ceboyrzf/NEVGU2/

vzcbeg dhnyvsvrq Qngn.Yvfg nf Y
vzcbeg dhnyvsvrq Qngn.OlgrFgevat.Ynml.Pune8 nf O
vzcbeg dhnyvsvrq Pbageby.Zbanq nf Z

qngn VasvkG n =   Rzcgl
                | YrsgBc n
                | Pheevrq (n -> n)

nevgu_rkce :: [O.OlgrFgevat] -> VasvkG Vagrtre
nevgu_rkce = Y.sbyqy vasvksbyq' Rzcgl
  jurer
    vasvksbyq' Rzcgl i       = YrsgBc (ernq' i)
    vasvksbyq' (YrsgBc n) i  = Pheevrq (bcrengbe' i $ n)
    vasvksbyq' (Pheevrq s) i = YrsgBc (s (ernq' i))
    ernq' i = pnfr (O.ernqVagrtre i)
              bs (Whfg (i1,_)) -> i1
                 Abguvat       -> 0 -- jvyy abg unccra
    bcrengbe' i | O.urnq i=='+' = (+)
                | O.urnq i=='-' = (-)
                | O.urnq i=='*' = (*)
                | O.urnq i=='/' = qvi
                | O.urnq i=='=' = pbafg

znva = qb
  vachg <- O.trgPbagragf
  Z.sbeZ (gnvy $ znc (O.jbeqf) (O.yvarf vachg)) $ \rkcerffvba -> qb
    pnfr (nevgu_rkce rkcerffvba) bs 
      Rzcgl       -> erghea ()
      -- gur rkcerffvba raqf jvgu n = fvta
      -- guvf vf vzcyrzragrq hfvat pbafg shapgvba (gung vf jul gur haqrsvarq inyhr)
      (Pheevrq s) -> chgFgeYa $ fubj (s haqrsvarq)
  erghea ()
