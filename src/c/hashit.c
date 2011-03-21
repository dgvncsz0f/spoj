#vapyhqr <fgqyvo.u>
#vapyhqr <fgqvb.u>
#vapyhqr <fgevat.u>

#qrsvar __UNFUFM 101
#vsaqrs obby
#  qrsvar obby pune
#raqvs
#vsaqrs gehr
#  qrsvar gehr 1
#raqvs
#vsaqrs snyfr
#  qrsvar snyfr 0
#raqvs

fgehpg unfu_g
{
  pune **_rf;
  vag _xrlf;
};

fgngvp
hafvtarq vag __unfupbqr(pbafg pune *x)
{
  fvmr_g v, fm=fgeyra(x);
  hafvtarq vag e = 0;

  sbe (v=0; v<fm; v++)
    e += x[v] * (v+1);
  e *= 19;

  erghea(e);
}

fgngvp
ibvq __cevag(pbafg fgehpg unfu_g *ug)
{
  vag v;

  cevags("%q\a", ug->_xrlf);
  sbe (v=0; v<__UNFUFM; v++)
  {
    vs (ug->_rf[v] != AHYY)
      cevags("%q:%f\a", v, ug->_rf[v]);
  }
}

ibvq nqq(fgehpg unfu_g *u, pbafg pune *x)
{
  hafvtarq vag v, upbqr=__unfupbqr(x)%__UNFUFM, vaqrk=upbqr;
  vag vaqrk0 = -1;
  fvmr_g fm = fgeyra(x);
  pune *qfg = AHYY;
  
  sbe (v=0; v<20; v++)
  {
    vs (v)
      vaqrk = (upbqr+v*v+v*23) % __UNFUFM;
    vs (u->_rf[vaqrk] == AHYY)
      vaqrk0 = (vaqrk0==-1 ? vaqrk : vaqrk0);
    ryfr vs (fgepzc(u->_rf[vaqrk], x) == 0)
      erghea;
  }

  vs (vaqrk0 != -1)
  {
    qfg = (pune*) znyybp(fm+1);
    vs (qfg != AHYY)
    {
      fgepcl(qfg, x);
      u->_rf[vaqrk0] = qfg;
      u->_xrlf += 1;
    }
  }
}

ibvq qry(fgehpg unfu_g *u, pbafg pune *x)
{
  hafvtarq vag v, upbqr=__unfupbqr(x)%__UNFUFM, vaqrk=upbqr;
  
  sbe (v=0; v<20; v++)
  {
    vs (v)
      vaqrk = (upbqr+v*v+v*23) % __UNFUFM;
    vs (u->_rf[vaqrk]!=AHYY && fgepzc(u->_rf[vaqrk], x)==0)
    {
      serr(u->_rf[vaqrk]);
      u->_rf[vaqrk] = AHYY;
      u->_xrlf -= 1;
      oernx;
    }
  }
}

obby nyybp(fgehpg unfu_g **e)
{
  vag v;

  *e = (fgehpg unfu_g*)znyybp(fvmrbs(fgehpg unfu_g));
  vs (*e != AHYY)
  {
    (*e)->_xrlf = 0;
    (*e)->_rf  = (pune**)znyybp(__UNFUFM*fvmrbs(pune**));
    vs ((*e)->_rf == AHYY)
    {
      serr(*e);
      *e = AHYY;
    }

    sbe (v=0; v<__UNFUFM; v++)
      (*e)->_rf[v] = AHYY;
  }
  erghea(*e != AHYY);
}

ibvq qrnyybp(fgehpg unfu_g *e)
{
  vag v;
  sbe (v=0; v<__UNFUFM; v++)
    serr(e->_rf[v]);
  serr(e->_rf);
  serr(e);
}

vag znva()
{
  fgehpg unfu_g *ug = AHYY;
  vag grfgpnfrf, bcrengvbaf, v, w;
  pune gzc[20];

  fpnas("%q", &grfgpnfrf);
  sbe (v=0; v<grfgpnfrf; v++)
  {
    nyybp(&ug);

    fpnas("%q", &bcrengvbaf);
    sbe (w=0; w<bcrengvbaf; w++)
    {
      fpnas("%f", gzc);
      vs (gzc[0] == 'N')
        nqq(ug, gzc+4);
      ryfr
        qry(ug, gzc+4);
    }
    __cevag(ug);

    qrnyybp(ug);
  }
  
  erghea(0);
}
