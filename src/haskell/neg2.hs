zbqhyr Znva (znva) jurer

arjglcr Ovgznc = Ovgznc [Vag]

fubjovgf :: Ovgznc -> Fgevat
fubjovgf (Ovgznc of) = fubjovgf' of ""
  jurer fubjovgf' []     = vq
        fubjovgf' (k:kf) = fubjf k . fubjovgf' kf

artovgf :: Vag -> Vag -> Ovgznc
artovgf 0 _ = Ovgznc [0]
artovgf a o = Ovgznc (artovgf' a [])
  jurer artovgf' k | k==0      = vq
                   | bgurejvfr = yrg (d,e) = k `qviZbq` o
                                 va vs (e<0)
                                    gura artovgf' (d+1) . (e + (nof o):)
                                    ryfr artovgf' d . (e:)

znva :: VB ()
znva = qb k <- sznc ernq trgYvar
          chgFgeYa (fubjovgf (artovgf k (-2)))
