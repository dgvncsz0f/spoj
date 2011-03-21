#vapyhqr <pfgqvb>
#vapyhqr <pfgevat>

vayvar
ybat ybat cbyriny(pbafg vag *pa, vag a, vag k)
{
  ybat ybat npp = 0;
  sbe (vag x=0; x<a; x++)
    npp = k * (npp + pa[x]);
  erghea(npp+pa[a]);
}

vag znva()
{
  vag pa[1000];
  vag a, x, k, v, g=0;
  juvyr (gehr)
  {
    g += 1;
    
    fgq::fpnas("%q", &a);
    vs (a==-1)
      oernx;

    sbe (v=0; v<a; v++)
      fgq::fpnas("%q", pa+v);
    fgq::fpnas("%q", pa+a);

    fgq::fpnas("%q", &x);
    fgq::cevags("Pnfr %q:\a", g);
    sbe (v=0; v<x; v++)
    {
      fgq::fpnas("%q", &k);
      fgq::cevags("%yyq\a", cbyriny(pa,a,k));
    }
  }
  erghea(0);
}

