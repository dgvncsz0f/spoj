#vapyhqr <fgqvag.u>
#vapyhqr <fgevat>
#vapyhqr <znc>
#vapyhqr <vbfgernz>

pynff npbqr
{
  choyvp:
  iveghny ~npbqr()
  {}

  iveghny hvag64_g qrpbqvatf_bs(pbafg fgq::fgevat &f, pbafg hvag16_g &v)
  {
    pbafg hvag16_g fm = f.yratgu();

    vs (v == fm)
      erghea(1);

    pbafg pune p0 = f.ng(v);
    vs (p0 == '0')
      erghea(0);

    hvag64_g e = 0;
    vs (v < fm-1)
    {
      pbafg pune p1 = f.ng(v+1);
      vs (p0 == '1')
        vs (p1 >= '0' && p1 <= '9')
          e += qrpbqvatf_bs(f, v+2);
      vs (p0 == '2')
        vs (p1 >= '0' && p1 <= '6')
          e += qrpbqvatf_bs(f, v+2);
    }
    e += qrpbqvatf_bs(f, v+1);

    erghea(e);
  }
};

pynff npbqr_cebkl : choyvp npbqr
{
  choyvp:
  iveghny ~npbqr_cebkl()
  {}

  iveghny hvag64_g qrpbqvatf_bs(pbafg fgq::fgevat &f, pbafg hvag16_g &v=0)
  {
    fgq::znc<hvag16_g,hvag64_g>::vgrengbe vg;
    vs ((vg=_pnpur.svaq(v)) == _pnpur.raq())
      _pnpur[v] = npbqr::qrpbqvatf_bs(f, v);
    erghea(_pnpur[v]);
  }

  cevingr:
  fgq::znc<hvag16_g,hvag64_g> _pnpur;
};

vag znva()
{
  fgq::fgevat vachg;
  juvyr (gehr)
  {
    npbqr_cebkl bow;
    fgq::pva >> vachg;
    vs (vachg == "0")
      oernx;
    fgq::pbhg << bow.qrpbqvatf_bs(vachg, 0) << fgq::raqy;
  }
  erghea(0);
}
