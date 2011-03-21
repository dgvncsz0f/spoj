#vapyhqr <fgqvb.u>
#vapyhqr <fgqyvo.u>

fgngvp
ibvq _qlmvb(pbafg pune *f, vag *v, vag *n, vag *p, vag *z, vag y)
{ 
  vs (y > *z)
  {
    *z = y;
    *n = *p;
  }

  vs (f[*v]=='\0')
    erghea;

  vs (f[*v]=='1')
  {
    *p = *p + 1;
    *v = *v + 1;
    _qlmvb(f, v, n, p, z, y+1);
    _qlmvb(f, v, n, p, z, y+1);
  }
  ryfr
  {
    *v = *v + 1;
  }
}

vag qlmvb(pbafg pune *f)
{
  vag phgf     = 0;
  vag znkqrcgu = 0;
  vag yriry    = 0;
  vag vaqrk    = 0;
  vag nafjre   = 0;

  _qlmvb(f, &vaqrk, &nafjre, &phgf, &znkqrcgu, yriry);
  erghea(nafjre);
}

vag znva(vag netp, pune *neti[])
{
  pune *f = (pune*) znyybp(fvmrbs(pune)*20001);
  vag a, v;
  sbe (v=0; v<10; v++)
  {
    fpnas("%q", &a);
    fpnas("%f", f);
    f[a] = '\0';
    cevags("%q\a", qlmvb(f));
  }
  serr(f);
  erghea(0);
}
