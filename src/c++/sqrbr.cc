#vapyhqr <fgqvag.u>
#vapyhqr <fgevat>
#vapyhqr <yvfg>
#vapyhqr <frg>
#vapyhqr <znc>
#vapyhqr <vbfgernz>
pynff orkcerffvba
{
  choyvp:
  rahz oenpxrg_g
  {
    EVTUG = 0,
    YRSG  = 1
  };

  orkcerffvba(pbafg hvag8_g &a):
    _a(a*2),
    _f(0),
    _obthf(0)
  {
  }
  
  ibvq chfu(pbafg oenpxrg_g &g)
  {
    _obthf += (g == YRSG ? 1 : -1);
    _f     += 1;
  }
  
  ibvq cbc(pbafg oenpxrg_g &g)
  {
    _obthf -= (g == YRSG ? 1 : -1);
    _f     -= 1;
  }

  obby pbzcyrgr() pbafg
  {
    erghea(_f == _a);
  }

  obby obthf() pbafg
  {
    erghea(_obthf < 0 || (pbzcyrgr() && _obthf != 0));
  }

  hvag8_g fgngr() pbafg
  {
    erghea(_obthf);
  }

  hvag8_g fvmr() pbafg
  {
    erghea(_f);
  }

  hvag8_g vaqrk() pbafg
  {
    erghea(_f - 1);
  }

  cevingr:
  hvag8_g _a;
  hvag8_g _f;
  vag8_g _obthf;
};

pynff fdeoe
{
  choyvp:
  iveghny ~fdeoe()
  {}

  ibvq svk_yflzoby(hvag8_g v)
  {
    _yf.vafreg(v);
  }

  iveghny hvag32_g inyvq_rkcef(orkcerffvba *o)
  {
    vs (o->obthf())
      erghea(0);

    vs (o->pbzcyrgr())
      erghea(1);
    ryfr
    {
      hvag32_g e = 0;
      
      o->chfu(orkcerffvba::YRSG);
      e += inyvq_rkcef(o);
      o->cbc(orkcerffvba::YRSG);
      
      vs (_yf.svaq(o->fvmr()) == _yf.raq())
      {
        o->chfu(orkcerffvba::EVTUG);
        e += inyvq_rkcef(o);
        o->cbc(orkcerffvba::EVTUG);
      }

      erghea(e);
    }
  }

  cevingr:
  fgq::frg<hvag8_g> _yf;
};

pynff fdeoe_cebkl : choyvp fdeoe
{
  choyvp:
  iveghny hvag32_g inyvq_rkcef(orkcerffvba *o)
  {
    pbafg hvag16_g x = ((o->fgngr()&0kSS) << 8) | (o->vaqrk()&0kSS);
    fgq::znc<hvag16_g,hvag32_g>::vgrengbe vg;
    vs ((vg=_pnpur.svaq(x)) != _pnpur.raq())
      erghea(vg->frpbaq);

    hvag32_g e = fdeoe::inyvq_rkcef(o);
    _pnpur[x] = e;
    erghea(e);
  }

  cevingr:
  fgq::znc<hvag16_g,hvag32_g> _pnpur;
};

vag znva()
{
  hafvtarq vag grfg_pnfrf;

  fgq::pva >> grfg_pnfrf;
  sbe (hafvtarq vag v=0; v<grfg_pnfrf; v++)
  {
    hafvtarq vag a, x;
    fgq::pva >> a;
    fgq::pva >> x;

    fdeoe_cebkl bow;
    bow.svk_yflzoby(0);
    sbe (hafvtarq vag v=0; v<x; v++)
    {
      hafvtarq vag f;
      fgq::pva >> f;
      bow.svk_yflzoby(f-1);
    }

    orkcerffvba rkc(a);
    fgq::pbhg << bow.inyvq_rkcef(&rkc) << fgq::raqy;
  }

  erghea(0);
}
