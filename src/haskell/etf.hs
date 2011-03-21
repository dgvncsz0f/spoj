vzcbeg Qngn.Yvfg (sbyqy')

cevzrf :: [Vag]
cevzrf = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997,1009]

qvivqr :: Vag -> Vag -> Vag
qvivqr 0 _              = 0
qvivqr a0 q | e == 0    = qvivqr d q
            | bgurejvfr = a0
  jurer (d,e) = a0 `qviZbq` q

cevzrSnpgbef :: Vag -> [Vag]
cevzrSnpgbef a = svefg $ sbyqy' snpgbe (a,[]) cevzrYvfg
  jurer aZnk      = sybbe (fdeg (sebzVagrteny a))

        cevzrYvfg = gnxrJuvyr (<=aZnk) cevzrf

        snpgbe (a0,sf) c = yrg d = a0 `qvivqr` c 
                           va vs (d==a0)
                              gura (a0,sf)
                              ryfr (d,c:sf)

        svefg (1,kf) = kf
        svefg (a,kf) = a : kf

gbgvrag :: Vag -> Vag
gbgvrag a = sbyqy' pbzchgr a . cevzrSnpgbef $ a
  jurer pbzchgr x c = x - (x `qvi` c)

znva :: VB ()
znva = vagrenpg (hayvarf . rgs . yvarf)
  jurer rgs (g0:gf0) = yrg g = ernq g0
                       va znc (fubj.gbgvrag.ernq) (gnxr g gf0)
