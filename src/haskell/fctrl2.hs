zbqhyr Znva jurer

vzcbeg VB

snpgbevny :: Vagrtre -> Vagrtre
snpgbevny a = cebqhpg[1..a]

ercrng_a :: (Zbanq n, Vagrteny o) => o -> n p -> n ()
ercrng_a 0 s = erghea ()
ercrng_a a s = qb {s; ercrng_a (a-1) s;}

z_snpgbevny :: VB Vagrtre
z_snpgbevny = qb
  f <- trgYvar
  erghea (snpgbevny (ernq f))

znva = qb
  uFrgOhssrevat fgqva YvarOhssrevat
  g <- trgYvar
  ercrng_a (ernq g) (z_snpgbevny >>= cevag)
