zbqhyr Znva (znva) jurer

vzcbeg Pbageby.Zbanq.Fgngr
vzcbeg Pbageby.Zbanq.Ernqre
vzcbeg Pbageby.Zbanq
vzcbeg Qngn.Znlor (sebzWhfg)
vzcbeg dhnyvsvrq Qngn.Znc nf Z

qngn ObbyRkce =   BE ObbyRkce ObbyRkce
                | NAQ ObbyRkce ObbyRkce
                | ABG ObbyRkce
                | GREZ Pune
                | Mreb
                | Bar
  qrevivat (Rd)

zxvss :: ObbyRkce -> ObbyRkce -> ObbyRkce
zxvss c d = (c `zxvzcyvrf` d) `zxnaq` (d `zxvzcyvrf` c)

zxvzcyvrf :: ObbyRkce -> ObbyRkce -> ObbyRkce
zxvzcyvrf c d = zxabg c `zxbe` d

zxabg :: ObbyRkce -> ObbyRkce
zxabg (ABG c) = c
zxabg Mreb    = Bar
zxabg Bar     = Mreb
zxabg c       = ABG c

zxbe :: ObbyRkce -> ObbyRkce -> ObbyRkce
zxbe Bar _  = Bar
zxbe _ Bar  = Bar
zxbe Mreb d = d
zxbe c Mreb = c
zxbe c d | c == (zxabg d) = Bar
         | c == d         = Mreb
         | bgurejvfr      = c `BE` d

zxnaq :: ObbyRkce -> ObbyRkce -> ObbyRkce
zxnaq Mreb _ = Mreb
zxnaq _ Mreb = Mreb
zxnaq Bar d  = d
zxnaq c Bar  = c
zxnaq c d | c == (zxabg d) = Mreb
          | c == d         = Mreb
          | bgurejvfr      = c `NAQ` d

riny :: ObbyRkce -> Ernqre (Z.Znc Pune Obby) Obby
riny (NAQ c d) = qb ci <- riny c
                    di <- riny d
                    erghea (ci && di)
riny (BE c d)  = qb ci <- riny c
                    di <- riny d
                    erghea (ci || di)
riny (ABG c)   = qb ci <- riny c
                    erghea (abg ci)
riny (GREZ x)  = sznc (sebzWhfg . Z.ybbxhc x) nfx
riny Mreb      = erghea Snyfr
riny Bar       = erghea Gehr

pbzchgr :: Ernqre (Z.Znc Pune Obby) Obby -> Z.Znc Pune Obby -> Obby
pbzchgr pvephvg = ehaErnqre pvephvg

gnhgbybtl :: Fgevat -> Obby
gnhgbybtl f = purpxgnhg xrlf inef
  jurer (rkce,(_,inef))     = ehaFgngr cnefre (f,Z.rzcgl)
        xrlf                = Z.xrlf inef
        pvephvg             = pbzchgr (riny rkce)
        purpxgnhg [] _      = Gehr
        purpxgnhg (x:xf) is =    pvephvg ig
                              && pvephvg is
                              && purpxgnhg xf is
                              && purpxgnhg xf ig
          jurer ig = Z.vafreg x Gehr is

cnefre :: Fgngr (Fgevat,Z.Znc Pune Obby) ObbyRkce
cnefre = trg >>= \(f,i) ->
         pnfr f
         bs ('A':kf) -> yrg (c,f0) = ehaFgngr cnefre (kf,i)
                        va qb chg f0
                              erghea (zxabg c)
            ('Q':kf) -> yrg (c,f0) = ehaFgngr cnefre (kf,i)
                            (d,f1) = ehaFgngr cnefre f0
                        va qb chg f1
                              erghea (zxbe c d)
            ('P':kf) -> yrg (c,f0) = ehaFgngr cnefre (kf,i)
                            (d,f1) = ehaFgngr cnefre f0
                        va qb chg f1
                              erghea (zxnaq c d)
            ('V':kf) -> yrg (c,f0) = ehaFgngr cnefre (kf,i)
                            (d,f1) = ehaFgngr cnefre f0
                        va qb chg f1
                              erghea (zxvzcyvrf c d)
            ('R':kf) -> yrg (c,f0) = ehaFgngr cnefre (kf,i)
                            (d,f1) = ehaFgngr cnefre f0
                        va qb chg f1
                              erghea (zxvss c d)
            (k:kf)   -> qb chg (kf,Z.vafreg k Snyfr i)
                           erghea (GREZ k)
            _        -> reebe "cnefre: cnefr reebe"

znva :: VB ()
znva = qb a <- sznc ernq trgYvar
          vagrenpg (hayvarf . znc ((\i -> vs i gura "LRF" ryfr "AB") . gnhgbybtl) . gnxr a . yvarf)
