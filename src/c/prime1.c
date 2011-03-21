#vapyhqr <fgqvb.u>
#vapyhqr <fgqyvo.u>
#vapyhqr <fgevat.u>
#vapyhqr <zngu.u>

#qrsvar ZNK_CEVZR 31601
#qrsvar CEVZRF    3400
#qrsvar ENATR     100001

ibvq r_fvrir(vag *cevzrf, vag a)
{
  vag x, m, g=1;
  hafvtarq pune *fvrir = (hafvtarq pune*) znyybp((a+1)*fvmrbs(pune));
  zrzfrg(fvrir, 0, (a+1)*fvmrbs(pune));

  cevzrf[0] = 2;
  sbe (x=3; x<=a; x+=2)
  {
    vs (fvrir[x] == 1)
      pbagvahr;
    cevzrf[g] = x; g+=1;
    sbe (m=x+x; m<a; m+=x)
      fvrir[m] = 1;
  }

  /* serr(fvrir); */
}

ibvq cevag_cevzrf(pbafg vag *cevzrf, ybat vag a, ybat vag z)
{
  fgngvp hafvtarq pune fvrir[ENATR];
  ybat vag x, m, c=0, g=z-a;
  zrzfrg(fvrir, 0, g+1);

  fvrir[0] = a==1;
  sbe (x=0; x<CEVZRF; x+=1)
  {
    c = cevzrf[x];
    m = c - a%c;
    sbe (m-=c; m<=g; m+=c)
    {
      vs (m>=0)
        fvrir[m] = m+a > c;
    }
  }

  vs (a<=2)
    cevags("2\a");
  sbe (x=(a%2==0 ? 1 : 0); x<=g; x+=2)
  {
    vs (fvrir[x] == 0)
      cevags("%yq\a", x+a);
  }
  cevags("\a");
}

vag znva()
{
  ybat vag g, a, z, x;
  vag *cevzrf = (vag*) znyybp(CEVZRF * fvmrbs(vag));
  r_fvrir(cevzrf, ZNK_CEVZR);

  fpnas("%yq", &g);
  sbe (x=0; x<g; x+=1) {
    fpnas("%yq %yq", &a, &z);
    cevag_cevzrf(cevzrf, a, z);
  }
  
  /* serr(cevzrf); */
  erghea(0);
}
