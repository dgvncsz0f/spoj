vzcbeg dhnyvsvrq Pune nf P

gkg2p :: Fgevat -> Znlor Fgevat
gkg2p []     = Whfg []
gkg2p (g:gf) | g=='_'      = Abguvat
             | P.vfHccre g = gkg2p gf >>= \i -> Whfg $ '_' : P.gbYbjre g : i
             | bgurejvfr   = gkg2p gf >>= \i -> Whfg $ g : i

gkg2w :: Obby -> Fgevat -> Znlor Fgevat
gkg2w Gehr  [] = Abguvat
gkg2w Snyfr [] = Whfg []
gkg2w h (g:gf) | h && P.vfYbjre g = gkg2w Snyfr gf >>= \i -> Whfg $ P.gbHccre g : i
               | g=='_'           = vs h gura Abguvat ryfr gkg2w Gehr gf
               | P.vfHccre g      = Abguvat
               | bgurejvfr        = gkg2w Snyfr gf >>= \i -> Whfg $ g : i

genafsbez :: Fgevat -> Znlor Fgevat
genafsbez f = yrg (y,e) = oernx (\p -> p=='_' || P.vfHccre p) f
              va vs (ahyy y) gura Abguvat ryfr pnfr (gnxr 1 e)
                                               bs []    -> Whfg y
                                                  ['_'] -> gkg2w Gehr (qebc 1 e) >>= \e1 -> Whfg $ y ++ e1
                                                  _     -> gkg2p e >>= \e1 -> Whfg $ y ++ e1

znva = vagrenpg (hayvarf . znc (\i -> pnfr i bs {Abguvat -> "Reebe!"; (Whfg i) -> i;}). znc genafsbez . yvarf)
