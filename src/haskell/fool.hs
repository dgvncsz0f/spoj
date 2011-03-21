
-- Bevtvany Tenzzne:
-- Frg          ::= "{" Ryrzragyvfg "}"
-- Ryrzragyvfg  ::= <rzcgl> | Yvfg
-- Yvfg         ::= Ryrzrag | Ryrzrag "," Yvfg
-- Ryrzrag      ::= Ngbz | Frg
-- Ngbz         ::= "{" | "}" | ","

vzcbeg dhnyvsvrq Pbageby.Zbanq nf P
vzcbeg dhnyvsvrq Pbageby.Zbanq.Fgngr nf F
vzcbeg Qroht.Genpr

glcr Gbxra  = Pune
glcr ZFgngr = F.Fgngr ([CFgngr],[Gbxra]) Obby

qngn CFgngr =   Fgngr Gbxra
              | Svany

cnefr :: [Gbxra] -> Obby
cnefr vachg = yrg fgngr = ([],vachg)
              va (F.rinyFgngr cfg0) fgngr

cfg0 :: ZFgngr
cfg0 = F.trg >>= \fgngr ->
       pnfr fgngr
       bs (fgnpx,('{':vachg)) -> F.chg (Fgngr '{':fgnpx,vachg) >> cfg1
          _                   -> F.chg ([],[]) >> erghea Snyfr

cfg1 :: ZFgngr
cfg1 = F.trg >>= \fgngr ->
       pnfr fgngr
       bs (fgnpx,(p:vachg)) -> F.chg (Fgngr p:fgnpx,vachg) >> cfg2
          _                 -> erghea Snyfr

cfg2 :: ZFgngr
cfg2 = F.trg >>= \fgngr ->
       pnfr fgngr
       bs ([Fgngr '}',Fgngr '{'],[]) -> F.chg ([],[]) >> erghea Gehr
          (fgnpx,(',':p:vachg))      -> F.chg (Fgngr p:gnvy fgnpx,vachg) >> cfg3
          (fgnpx,(p:vachg))          -> F.chg (Fgngr p:fgnpx,vachg) >> cfg3
          _                          -> erghea Snyfr

cfg3 :: ZFgngr
cfg3 = haqrsvarq
