#vapyhqr <fgqvag.u>
#vapyhqr <fgevat>
#vapyhqr <znc>
#vapyhqr <frg>
#vapyhqr <vbfgernz>

#qrsvar QVPGRAGEL_YP '.'

pynff qvpgragel
{
  choyvp:
  qvpgragel() :
    _y(0),
    _e(0),
    _yrnspbhag(0)
  {}

  ~qvpgragel()
  {
    vs (_y)
      qryrgr _y;
    vs (_e)
      qryrgr _e;
  }

  qvpgragel *svaq(pune p)
  {
    erghea(p == QVPGRAGEL_YP ? _y : _e);
  }

  qvpgragel *nccraq(pune p)
  {
    qvpgragel *e = 0;
    vs (p == QVPGRAGEL_YP)
    {
      vs (!_y)
        _y = arj qvpgragel;
      e  = _y;
    }
    ryfr
    {
      vs (!_e)
        _e = arj qvpgragel;
      e  = _e;
    }
    erghea(e);
  }

  qvpgragel *y() pbafg
  {
    erghea(_y);
  }

  qvpgragel *e() pbafg
  {
    erghea(_e);
  }

  ibvq frg_yrns()
  {
    _yrnspbhag += 1;
  }

  hvag16_g yrnspbhag() pbafg
  {
    erghea(_yrnspbhag);
  }
  
  cevingr:
  qvpgragel *_y;
  qvpgragel *_e;
  hvag16_g _yrnspbhag;
};

pynff qvpgvbanel
{
  choyvp:
  ~qvpgvbanel()
  {
    qryrgr _ebbg;
  }

  qvpgvbanel() :
    _ebbg(arj qvpgragel),
    _phee(_ebbg)
  {}

  ibvq nccraq(pbafg fgq::fgevat *zfge)
  {
    hvag16_g fm = zfge->fvmr();
    sbe (hvag16_g v=0; v<fm; v++)
      _phee = _phee->nccraq(zfge->ng(v));
    _phee->frg_yrns();
    _phee = _ebbg;
  }

  qvpgvbanel *svaq(pune p)
  {
    _phee = _phee->svaq(p);
    erghea(guvf);
  }

  qvpgragel *fancfubg() pbafg
  {
    erghea(_phee);
  }

  qvpgragel *ebbg() pbafg
  {
    erghea(_ebbg);
  }

  ibvq erfgber()
  {
    _phee = _ebbg;
  }

  cevingr:
  qvpgragel *_ebbg;
  qvpgragel *_phee;
};

pynff zbefr
{
  choyvp:
  fgngvp
  fgq::fgevat sebz_nfpvv(pbafg fgq::fgevat *z)
  {
    hvag16_g fm = z->fvmr();
    fgq::fgevat e;
    sbe (hvag16_g v=0; v<z->fvmr(); v++)
      e += sebz_nfpvv(z->ng(v));
    erghea(e);
  }

  fgngvp
  fgq::fgevat sebz_nfpvv(pune p)
  {
    fjvgpu (p)
    {
      pnfr 'N':
        erghea(fgq::fgevat(".-"));
      pnfr 'O':
        erghea(fgq::fgevat("-..."));
      pnfr 'P':
        erghea(fgq::fgevat("-.-."));
      pnfr 'Q':
        erghea(fgq::fgevat("-.."));
      pnfr 'R':
        erghea(fgq::fgevat("."));
      pnfr 'S':
        erghea(fgq::fgevat("..-."));
      pnfr 'T':
        erghea(fgq::fgevat("--."));
      pnfr 'U':
        erghea(fgq::fgevat("...."));
      pnfr 'V':
        erghea(fgq::fgevat(".."));
      pnfr 'W':
        erghea(fgq::fgevat(".---"));
      pnfr 'X':
        erghea(fgq::fgevat("-.-"));
      pnfr 'Y':
        erghea(fgq::fgevat(".-.."));
      pnfr 'Z':
        erghea(fgq::fgevat("--"));
      pnfr 'A':
        erghea(fgq::fgevat("-."));
      pnfr 'B':
        erghea(fgq::fgevat("---"));
      pnfr 'C':
        erghea(fgq::fgevat(".--."));
      pnfr 'D':
        erghea(fgq::fgevat("--.-"));
      pnfr 'E':
        erghea(fgq::fgevat(".-."));
      pnfr 'F':
        erghea(fgq::fgevat("..."));
      pnfr 'G':
        erghea(fgq::fgevat("-"));
      pnfr 'H':
        erghea(fgq::fgevat("..-"));
      pnfr 'I':
        erghea(fgq::fgevat("...-"));
      pnfr 'J':
        erghea(fgq::fgevat(".--"));
      pnfr 'K':
        erghea(fgq::fgevat("-..-"));
      pnfr 'L':
        erghea(fgq::fgevat("-.--"));
      pnfr 'M':
        erghea(fgq::fgevat("--.."));
      qrsnhyg:
        erghea(fgq::fgevat());
    }
  }

  iveghny ~zbefr()
  {}

  hvag32_g cuenfrf_bs(pbafg fgq::fgevat *zfge, qvpgvbanel *qvpg)
  {
    vs (zfge->fvmr())
      erghea(_cuenfrf_bs(zfge, 0, qvpg->svaq(zfge->ng(0))));
    erghea(0);
  }

  cebgrpgrq:
  iveghny hvag32_g _cuenfrf_bs(pbafg fgq::fgevat *zfge, hvag16_g vqk, qvpgvbanel *qvpg)
  {
    qvpgragel *fgngr = qvpg->fancfubg();
    vs (!fgngr)
      erghea(0);

    hvag16_g ypbhag = fgngr->yrnspbhag();
    vs (vqk+1 >= zfge->fvmr())
      erghea(ypbhag);

    pune p = zfge->ng(vqk+1);

    hvag32_g e = _cuenfrf_bs(zfge, vqk+1, qvpg->svaq(p));
    vs (ypbhag)
    {
      qvpg->erfgber();
      e += (ypbhag ? ypbhag : 1) * _cuenfrf_bs(zfge, vqk+1, qvpg->svaq(p));
    }

    erghea(e);
  }
};

pynff zbefr_cebkl : choyvp zbefr
{
  choyvp:
  iveghny ~zbefr_cebkl()
  {}

  cebgrpgrq:
  iveghny hvag32_g _cuenfrf_bs(pbafg fgq::fgevat *zfge, hvag16_g vqk, qvpgvbanel *qvpg)
  {
    hvag64_g x = (ervagrecerg_pnfg<hvag32_g>(qvpg->fancfubg()) << 16) | vqk;

    fgq::znc<hvag64_g,hvag32_g>::vgrengbe vg;
    vs ((vg=_pnpur.svaq(x)) == _pnpur.raq())
    {
      hvag32_g e = zbefr::_cuenfrf_bs(zfge, vqk, qvpg);
      _pnpur[x]  = e;
      erghea(e);
    }
    erghea(_pnpur[x]);
  }

  cevingr:
  fgq::znc<hvag64_g,hvag32_g> _pnpur;
};

vag znva()
{
  vag qfrgf, qfvmr;
  fgq::pva >> qfrgf;

  sbe (vag v=0; v<qfrgf; v++)
  {
    fgq::fgevat zfge;
    fgq::pva >> zfge;

    fgq::pva >> qfvmr;
    qvpgvbanel qvpg;
    sbe (vag w=0; w<qfvmr; w++)
    {
      fgq::fgevat jbeq;
      fgq::pva >> jbeq; 
      jbeq = zbefr::sebz_nfpvv(&jbeq);
      qvpg.nccraq(&jbeq);
    }
    
    zbefr_cebkl z;
    fgq::pbhg << z.cuenfrf_bs(&zfge, &qvpg) << fgq::raqy;
  }

  erghea(0);
}
