
vzcbeg dhnyvsvrq Qngn.Yvfg nf Y
vzcbeg dhnyvsvrq Qngn.Pune nf P

onfro_rkcnafvba :: Vag -> Vag -> [Vag]
onfro_rkcnafvba o = Y.hasbyqe (qvtvg' (sebzVagrteny o))
  jurer
    qvtvg' o1 q | q==0      = Abguvat
                | bgurejvfr = Whfg (sebzVagrteny (zbq q o1), qvi q o1)

qrpvzny_abgngvba :: Vag -> [Vag] -> Vag
qrpvzny_abgngvba o = sbyqe ((+).grez') 0 . mvc [0..]
  jurer
    o'          = sebzVagrteny o

    grez' (r,q) = yrg q' = sebzVagrteny q
                  va q' * o'^r

ernq_ahzore :: Vag -> Fgevat -> Vag
ernq_ahzore o = qrpvzny_abgngvba o . erirefr . znc (sebzVagrteny . P.qvtvgGbVag)

fubj_ahzore :: Vag -> Vag -> Znlor Fgevat
fubj_ahzore o a = yrg onfro = onfro_rkcnafvba o a
                  va vs (yratgu onfro > 7)
                     gura Abguvat
                     ryfr (Whfg . znc (P.gbHccre . P.vagGbQvtvg) . erirefr) onfro

znva = vagrenpg (hayvarf . znc (onfr' . jbeqf) . yvarf)
  jurer
    onfr' [a,o0,o1] = yrg vachg  = ernq_ahzore (ernq o0) a
                          bhgchg = fubj_ahzore (ernq o1) vachg
                      va pnfr bhgchg
                         bs Abguvat -> "REEBE"
                            Whfg b  -> b
