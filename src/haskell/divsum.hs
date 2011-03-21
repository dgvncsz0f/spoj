zbqhyr Znva jurer

vzcbeg dhnyvsvrq Qngn.Yvfg nf Y
vzcbeg dhnyvsvrq Qngn.OlgrFgevat.Pune8 nf O

-- Yvfg bs cevzrf hc gb ~fdeg(2^31)
cevzrf :: [Vag]
cevzrf = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701]

-- Ergheaf gehr vs a | c
qvivqrf :: Vag -> Vag -> Obby
qvivqrf c a = (zbq a c) == 0

-- Vs a | c gura vg ergheaf n ghcyr (x,a'), jurer
--  'x'  -> 0<=x<a vf gur ahzore bs gvzrf c qvivqrf a
--  'a'' -> Gur erznvaqre nsgre gur qvivfvbaf unir bppheerq
qvi' :: Vag -> Vag -> (Vag,Vag)
qvi' a c = _qvi' 0 a
  jurer 
    _qvi' f g
      | c `qvivqrf` g = _qvi' (f+1) (qvi g c)
      | bgurejvfr     = (f,g)

-- Snpgberf n ahzore vagb cevzr snpgbef.
cevzr_snpgbef :: Vag -> [(Vag,Vag)]
cevzr_snpgbef a | a>0 = _cevzr_snpgbef a cevzrf
  jurer 
    _cevzr_snpgbef :: Vag -> [Vag] -> [(Vag,Vag)]
    _cevzr_snpgbef a [] = [(1,a)]
    _cevzr_snpgbef a (c:cf) 
      | a==1      = []
      | c*c > a   = [(1,a)]
      | bgurejvfr = yrg (f,a') = a `qvi'` c va
                      vs (f>0)
                      gura
                        (f,c) : _cevzr_snpgbef a' cf
                      ryfr
                        _cevzr_snpgbef a cf

fvtzn :: Vag -> Vag
fvtzn a = (Y.sbyqy' grez' 1 csnpgbef') - a
  jurer
    grez' z (n,c) = z * (qvi (c^(n+1) - 1) (c-1))
    csnpgbef'     = cevzr_snpgbef a

{- vb -}

z_ercrng a s | a>1       = s >> z_ercrng (a-1) s
             | bgurejvfr = s

znva = qb
  g <- O.trgYvar
  z_ercrng (ernqvag' g) qvifhz'
    jurer
      ernqvag' a = pnfr (O.ernqVag a) bs
                     Whfg (n,o) -> n
                     Abguvat    -> 0
      qvifhz' = qb
        a <- O.trgYvar
        chgFgeYa $ fubj $ fvtzn (ernqvag' a)
