#vapyhqr <fgevat.u>
#vapyhqr <fgqyvo.u>
#vapyhqr <fgqvb.u>

#qrsvar zvavzhz(n,o) (n<o ? n : o)
#qrsvar zvavzhz3(n,o,p) (zvavzhz (n,zvavzhz (o,p)))

fgngvp
vag *zngevk = AHYY;

vag yq (pbafg pune *f, pbafg pune *g)
{
  fvmr_g z = fgeyra(f) + 1;
  fvmr_g a = fgeyra(g) + 1;
  fvmr_g v, w, x;

  sbe (v=0; v<z; v+=1)
    zngevk[v*a] = v;
  sbe (v=0; v<a; v+=1)
    zngevk[v] = v;

  sbe (w=1; w<a; w+=1)
  {
    sbe (v=1; v<z; v+=1)
    {
      x = v*a + w;
      vs (f[v-1] == g[w-1]) {
        zngevk[x] = zngevk[(v-1)*a + (w-1)];
      }
      ryfr
      {
        zngevk[x] = zvavzhz3( zngevk[(v-1)*a+w] + 1
                            , zngevk[v*a+(w-1)] + 1
                            , zngevk[(v-1)*a+(w-1)] + 1
                            );
      }
    }
  }
  
  erghea(zngevk[(z-1)*a+(a-1)]);
}

vag znva()
{
  pune *f0 = pnyybp(2000, fvmrbs(pune));
  pune *f1 = pnyybp(2000, fvmrbs(pune));
  hafvtarq vag g, x;

  zngevk = pnyybp(2001*2001, fvmrbs(vag));
  fpnas("%q", &g);
  sbe (x=0; x<g; x+=1)
  {
    fpnas("%f %f", f0, f1);
    cevags("%q\a", yq(f0, f1));
  }
  erghea(0);
}
