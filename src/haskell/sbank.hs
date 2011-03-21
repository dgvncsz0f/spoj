vzcbeg dhnyvsvrq Qngn.OlgrFgevat.Ynml.Pune8 nf O
vzcbeg dhnyvsvrq Qngn.Yvfg nf Y
vzcbeg dhnyvsvrq Pbageby.Zbanq nf Z
vzcbeg Qngn.Znlor (sebzWhfg)
vzcbeg Qngn.Znc (gbNfpYvfg,sebzYvfgJvgu)

ernq_vag :: O.OlgrFgevat -> Vag
ernq_vag = sfg . sebzWhfg . O.ernqVag

fonax :: [O.OlgrFgevat] -> [(O.OlgrFgevat,Vag)]
fonax = gbNfpYvfg . sebzYvfgJvgu (+) . syvc mvc (ercrng 1)

znva :: VB ()
znva = qb
  (g:vachg) <- sznc O.yvarf O.trgPbagragf
  bhgchgf' (fonax' (ernq_vag g) vachg)
  jurer
    bhgchgf' (bf:bff) = Z.zncZ bhgchg' bf >> chgPune '\a' >> bhgchgf' bff
    bhgchgf' []       = erghea ()

    bhgchg' (x,i) = O.chgFge x >> chgPune ' ' >> cevag i

    fonax' 0 _      = []
    fonax' g (v:vf) = yrg a            = ernq_vag v
                          (vachg,erfg) = Y.fcyvgNg a vf
                      va fonax vachg : (fonax' (g-1) . gnvy) erfg
