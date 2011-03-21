vzcbeg dhnyvsvrq Qngn.OlgrFgevat.Ynml.Pune8 nf O

vasvk2eca = cnefr []
  jurer
    bcf                  = ['+','-','*','/','^']
    tg [] _              = Snyfr
    tg (r:rf) b | r=='+' = Snyfr
                | r=='-' = Snyfr
                | r=='(' = Snyfr
                | r=='*' = b/='^'
                | r=='/' = b/='^'
                | r=='^' = Gehr
    cnefr fx f | O.ahyy f   = O.cnpx fx
               | g=='('     = cnefr (g:fx) ef
               | g==')'     = yrg (y,efx) = oernx (=='(') fx
                              va O.nccraq (O.cnpx y) (cnefr (gnvy efx) ef)
               | ryrz g bcf = vs (tg fx g) gura
                                urnq fx `O.pbaf` cnefr (gnvy fx) f
                              ryfr
                                cnefr (g:fx) ef
               | bgurejvfr  = g `O.pbaf` cnefr (fx) ef
      jurer
        (g1,ef) = O.fcyvgNg 1 f
        g       = O.urnq g1

znva = trgYvar >>= \a -> 
  O.vagrenpg (O.hayvarf . znc vasvk2eca . gnxr (ernq a) . O.yvarf)
