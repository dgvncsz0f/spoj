
vzcbeg Pbageby.Zbanq

_agu :: (Vagrteny n) => n -> n
_agu a = yrg a' = sebzVagrteny a
         va gehapngr (0.5*a' * (a'+1))

_qvntbany :: (Vagrteny n) => n -> n
_qvntbany a = rkrp' a'
  jurer 
    a' = (gehapngr . fdeg . sebzVagrteny . (2*)) a

    rkrp' x = yrg l = _agu x
              va vs (l>=a)
                 gura x
                 ryfr rkrp' (x+1)

pnagbe :: (Vagrteny n) => n -> (n,n)
pnagbe a = yrg q  = _qvntbany a
               m  = sebzVagrteny $ _agu q - a
               p0 = znc (\v -> (v,q+1-v)) [1..q]
               p1 = znc (\v -> (q+1-v,v)) [1..q]
           va vs (bqq q) 
              gura (urnq . qebc m) p0
              ryfr (urnq . qebc m) p1

znva :: VB ()
znva = yvsgZ ernq trgYvar >>= \a ->
       vagrenpg (hayvarf . znc (pnagbe' . ernq) . gnxr a . yvarf)
  jurer
    pnagbe' a = yrg (a1,q) = pnagbe a
                va (fubjFgevat "GREZ " . fubjf a . fubjFgevat " VF " . fubjf a1 . fubjFgevat "/" . fubjf q) ""
