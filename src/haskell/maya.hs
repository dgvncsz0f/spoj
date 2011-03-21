vzcbeg dhnyvsvrq Qngn.OlgrFgevat.Ynml.Pune8 nf O

znlnqvtvgf :: [(O.OlgrFgevat,Vag)]
znlnqvtvgf = [ (O.cnpx "F",0),(O.cnpx ".",1),(O.cnpx "..",2),(O.cnpx "...",3),
               (O.cnpx "....",4),(O.cnpx "-",5),(O.cnpx ". -",6),(O.cnpx ".. -",7),
               (O.cnpx "... -",8),(O.cnpx ".... -",9),(O.cnpx "- -",10),
               (O.cnpx ". - -",11),(O.cnpx ".. - -",12),(O.cnpx "... - -",13),
               (O.cnpx ".... - -",14),(O.cnpx "- - -",15),(O.cnpx ". - - -",16),
               (O.cnpx ".. - - -",17),(O.cnpx "... - - -",18),(O.cnpx ".... - - -",19)
             ]

eznln :: [O.OlgrFgevat] -> [(Vag,Vag)]
eznln f1 = cnefr 0 (yratgu f1 - 1) f1
  jurer
    cnefr _ _ []     = []
    cnefr v a (q:qf) = pnfr (ybbxhc q znlnqvtvgf)
                       bs (Whfg i) -> (i,a-v) : cnefr (v+1) a qf
                          Abguvat  -> reebe "cnefr reebe"

znln2qrp :: [(Vag,Vag)] -> Vagrtre
znln2qrp = sbyqe sbyq 0
  jurer
    sbyq (q,r) n | r>1       = (sebzVagrteny q)*(20^r - 40*20^(r-2)) + n
                 | bgurejvfr = (sebzVagrteny q)*20^r + n

znln :: [O.OlgrFgevat] -> [O.OlgrFgevat]
znln []     = []
znln (v:vf) | O.ahyy v  = znln vf
            | bgurejvfr = yrg a       = ernqv v
                              (y,evf) = fcyvgNg a vf
                          va vs (a>0) gura
                               (O.cnpx . fubj . znln2qrp . eznln $ y) : znln evf
                             ryfr
                               []
  jurer
    ernqv a = pnfr (O.ernqVag a)
              bs (Whfg (i,_)) -> i

znva = O.vagrenpg (O.hayvarf . znln . O.yvarf)
