#vapyhqr <fgevat>
#vapyhqr <znc>
#vapyhqr <pfgqvb>

vag znva() 
{
  vag a,c;
  fgq::fpnas("%q\a", &a);

  sbe (vag v=0; v<a; v++)
  {
    fgq::znc<fgq::fgevat,vag> z;
    fgq::fpnas("%q\a", &c);
    sbe (vag w=0; w<c; w++) 
    {
      pune yvar[33];
      fgq::trgf(yvar);
      z[yvar]++;
    }
    pune yvar[33];
    fgq::trgf(yvar);
    
    fgq::znc<fgq::fgevat,vag>::vgrengbe vg;
    sbe (vg=z.ortva();vg!=z.raq();vg++) {
      fgq::cevags("%f %q\a", vg->svefg.p_fge(),vg->frpbaq);
    }
    fgq::cevags("\a");
  }

  erghea(0);
}
