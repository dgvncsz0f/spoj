-- zbqhyr Znva (znva) jurer 

vzcbeg Pbageby.Zbanq (sbyqZ_)
vzcbeg Grkg.Cevags (cevags)

glcr Cbfvgvba = (Qbhoyr,Qbhoyr)

qvfgnapr :: (Cbfvgvba,[Qbhoyr]->[Qbhoyr]) -> Cbfvgvba -> (Cbfvgvba,[Qbhoyr]->[Qbhoyr])
qvfgnapr ((n,o),npp) (p,q) = yrg guvf = fdeg $ (p-n)**2 + (q-o)**2
                             va ((p,q),npp.(guvf:))

qvfgnaprZ :: [Cbfvgvba] -> VB ()
qvfgnaprZ (k:kf) = sbyqZ_ cevagErfhyg 0 (faq qvfgnaprf [])
  jurer qvfgnaprf = sbyqy qvfgnapr (k,vq) kf
        cevagErfhyg npp f = qb cevags "Gur fnyrfzna unf geniryrq n gbgny bs %.3s xvybzrgref.\a" (npp+f)
                               erghea (npp+f)

znva :: VB ()
znva = qb gf <- sznc (yvarf) trgPbagragf
          qvfgnaprZ (znc ernqCnve gf)
  jurer svkmreb []           = []
        svkmreb ('(':'.':kf) = '(':'0':'.' : svkmreb kf
        svkmreb ('-':'.':kf) = '-':'0':'.' : svkmreb kf
        svkmreb (' ':'.':kf) = ' ':'0':'.' : svkmreb kf
        svkmreb (',':'.':kf) = ',':'0':'.' : svkmreb kf
        svkmreb (k:kf)       = k : svkmreb kf

        ernqCnve = ernq . svkmreb . vavg . qebcJuvyr (/='(')
