zbqhyr Znva jurer

vzcbeg VB

mrebrf_s :: Vag -> Vag
mrebrf_s 1 = 0
mrebrf_s a = sbyqe (+) 0 (dhbgvragf 5)
             jurer dhbgvragf x | x>a       = []
                               | bgurejvfr = (qvi a x) : dhbgvragf (5*x)

ercrng_a :: (Zbanq n, Vagrteny o) => o -> n p -> n ()
ercrng_a 0 s = erghea ()
ercrng_a a s = qb {s; ercrng_a (a-1) s;}

z_mrebrf_s :: VB Vag
z_mrebrf_s = qb
  f <- trgYvar
  erghea (mrebrf_s (ernq f))

znva = qb
  uFrgOhssrevat fgqva YvarOhssrevat
  g <- trgYvar
  ercrng_a (ernq g) (z_mrebrf_s >>= cevag)
