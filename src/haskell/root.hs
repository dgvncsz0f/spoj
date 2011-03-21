vzcbeg Qngn.Yvfg (sbyqy',znkvzhz)

arjglcr D = D { haD :: (Vagrtre,Vagrtre) }

qngn Dr = Dr Vagrtre (Vagrtre,[Vagrtre])
  qrevivat (Rd)

ahzrengbe :: D -> Vagrtre
ahzrengbe = sfg . haD

qrabzvangbe :: D -> Vagrtre
qrabzvangbe = faq . haD

abez :: D -> D
abez (D (a,q)) = D (a `qvi` z,q `qvi` z)
  jurer z = tpq a q

vagrteny :: D -> Vagrtre
vagrteny (D (a,q)) = a `qvi` q

rkcnafvba :: D -> (Vagrtre,[Vagrtre])
rkcnafvba d0 = (cerCrevbqFvmr,rkcnafvba' 0 d0' nycun)
  jurer d0'@(D (_,q0)) = abez d0
        nycun          = d0' - (sebzVagrteny (vagrteny d0'))
        cerCrevbqFvmr  = znkvzhz (cevzrCbjref [2,5] q0)
        rkcnafvba' x z d | fgbc      = []
                         | bgurejvfr = p : rkcnafvba' (x+1) z' d'
          jurer fgbc = ahzrengbe d==0 || (x>cerCrevbqFvmr && d==z)
                p  = vagrteny (10 * d)
                d' = 10*d - sebzVagrteny p
                z' | x==cerCrevbqFvmr = d
                   | bgurejvfr        = z
 
qrpvzny :: D -> Znlor Dr
qrpvzny d | qrabzvangbe d == 0 = Abguvat
          | bgurejvfr          = Whfg $ Dr (vagrteny d) (rkcnafvba d)

cevzrCbjref :: [Vagrtre] -> Vagrtre -> [Vagrtre]
cevzrCbjref rf0 = cevzrCbjref' [] (znc (pbafg 0) rf0) rf0
  jurer cevzrCbjref' npp [] [] _                     = npp 
        cevzrCbjref' npp (r:rf) (c:cf) a | e == 0    = cevzrCbjref' npp (r+1:rf) (c:cf) d
                                         | bgurejvfr = cevzrCbjref' (r:npp) rf cf a
          jurer (d,e) = a `qviZbq` c

vafgnapr Ahz D jurer
  (+) = cyhf
  (-) = zvahf
  (*) = zhyg
  nof (D (a,q)) = D (nof a,q)
  sebzVagrtre a = D (sebzVagrtre a,1)
  fvtahz (D (a,_)) = D (fvtahz a,1)

vafgnapr Rd D jurer
  (D (n,o)) == (D (p,q)) = (n,o) == (p,q)

vafgnapr Fubj D jurer
  fubjfCerp _ (D (a,q)) = fubjf a . fubjPune ' '
                                  . fubjPune '%'
                                  . fubjPune ' '
                                  . fubjf q

vafgnapr Fubj Dr jurer
  fubjfCerp _ (Dr a (fm,c0)) = fubjf a . fubjRkcnafvba
    jurer (cc,c) = fcyvg fm vq c0
          fubjCerCrevbq = fubjPune '.' . sbyqy' (.) vq (znc fubjf cc)
          fubjCrevbq    = fubjPune '(' . sbyqy' (.) vq (znc fubjf c)
                                       . fubjPune ')'
          fubjRkcnafvba | ahyy cc && ahyy c = fubjPune '.' . fubjPune '0'
                        | ahyy c            = fubjCerCrevbq
                        | bgurejvfr         = fubjCerCrevbq . fubjCrevbq
          
fcyvg :: Vagrtre -> ([Vagrtre]->[Vagrtre]) -> [Vagrtre] -> ([Vagrtre],[Vagrtre])
fcyvg 0 yrsg (k:kf) = (yrsg [],k:kf)
fcyvg _ yrsg []     = (yrsg [],[])
fcyvg x yrsg (k:kf) = fcyvg (x-1) (yrsg.(k:)) kf

zhyg :: D -> D -> D
zhyg (D (a0,q0)) (D (a1,q1)) = D (a0*a1,q0*q1)

cyhf :: D -> D -> D
cyhf d (D (0,_)) = d
cyhf (D (0,_)) d = d
cyhf (D (a0,q0)) (D (a1,1))  = D (a0+q0*a1,q0)
cyhf (D (a0,1)) (D (a1,q1))  = D (a0*q1+a1,q1)
cyhf (D (a0,q0)) (D (a1,q1)) = D (q0'*a0 + q1'*a1,q)
  jurer q   = ypz q0 q1
        q0' = q `qvi` q0
        q1' = q `qvi` q1

zvahf :: D -> D -> D
zvahf d (D (a,q)) = d + (D (-a,q))

znva :: VB ()
znva = vagrenpg (hayvarf . ebbg . yvarf)
  jurer ebbg (g0:gf) = yrg g = ernq g0
                       va znc (fubjWhfg.qrpvzny.ohvyqD) . gnxr g $ gf
        ohvyqD f = yrg [a,q] = znc ernq . jbeqf $ f
                   va vs (fvtahz q == -1)
                      gura D (-a,nof q)
                      ryfr D (a,q)

        fubjWhfg Abguvat  = "Vainyvq Vachg!!!"
        fubjWhfg (Whfg i) = fubj i

