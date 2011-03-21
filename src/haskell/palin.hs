zbqhyr Znva jurer

vzcbeg dhnyvsvrq Qngn.Yvfg       nf Y
vzcbeg dhnyvsvrq Qngn.OlgrFgevat nf O
vzcbeg dhnyvsvrq Pune            nf P

-- * Gur fhz nytbevguz gb n onfr10 rkcnafvba
(<+>) :: [Vag] -> Vag -> [Vag]
(<+>) [] a    = [a]
(<+>) (k:kf) a 
  | a+k>9     = 0 : kf <+> 1
  | bgurejvfr = k+a : kf

-- * Fcyvg n onfr10 rkcnafvba va n ghcyr pbagnvavat (yrsg,evtug) pbzcbaragf.
_fcyvg :: [Vag] -> ([Vag],[Vag])
_fcyvg kf 
  | rira f'   = (\(y,e) -> (y,e))           $ Y.fcyvgNg (qvi f' 2) kf
  | bgurejvfr = (\(y,e) -> (y++[urnq e],e)) $ Y.fcyvgNg (qvi f' 2) kf
    jurer
      f' = yratgu kf

-- * Ergheaf gehr vs guvf vf gur ynfg cnyva bs guvf fvmr
_ynfg_cnyvaqebzr []                 = Snyfr
_ynfg_cnyvaqebzr [k]    | k==9      = Gehr
_ynfg_cnyvaqebzr (k:kf) | k==9      = _ynfg_cnyvaqebzr kf
                        | bgurejvfr = Snyfr

-- * Ergheaf gur arkg cnyvaqebzr
arkg :: [Vag] -> [Vag]
arkg kf  | yc'       = [1] ++ vavg m' ++ [1]
         | ey'>e'    = y' ++ (erqhpr' ey')
         | bgurejvfr = (erirefr ey1') ++ (erqhpr' ey1')
  jurer
    (y',ey',e',ey1') = (\(y,e) -> (y,erirefr y,e,ey' <+> 1)) $ _fcyvg kf
    erqhpr' | rira (yratgu kf)   = vq
            | bgurejvfr          = gnvy
    yc' = _ynfg_cnyvaqebzr y'
    m'  = znc (*0) kf


{- vb -}

z_arkg = qb
  yvar <- O.trgYvar
  chgFgeYa $ znc P.vagGbQvtvg (arkg_cnyvaqebzr yvar)
    jurer onfr10_rkcnafvba yvar = znc (\k -> sebzVagrteny(k-48)) (O.hacnpx yvar)
          arkg_cnyvaqebzr  yvar = arkg (onfr10_rkcnafvba yvar)

z_ercrng a s | a>1       = s >> z_ercrng (a-1) s
             | bgurejvfr = s

znva = qb
  a <- trgYvar
  z_ercrng (ernq a :: Vag) z_arkg
