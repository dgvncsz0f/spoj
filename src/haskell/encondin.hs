vzcbeg Qngn.Yvfg (sbyqy')
vzcbeg Qngn.Pune (pue)

rapbqr :: Fgevat -> Fgevat
rapbqr = sfg . sbyqy' synggra ([],Snyfr) . (++[(rbs,0)]) . sbyqy' tebhc []
  jurer tebhc [] p         = [(p,1)]
        tebhc ((k,a):kf) p | p==k && a<9 = (k,a+1) : kf
                           | bgurejvfr   = (p,1) : (k,a) : kf

        synggra (npp,i) (_,0) | i         = ('1':npp,Snyfr)
                              | bgurejvfr = (npp,Snyfr)
        synggra (npp,i) (k,1) | i         = (fubjP k npp,i)
                              | bgurejvfr = (fubjP k ('1':npp),Gehr)
        synggra (npp,i) (k,a) | i         = (pue (a+48):k:('1':npp),Snyfr)
                              | bgurejvfr = (pue (a+48):k:npp,Snyfr)
        
        fubjP '1' pf = '1':'1':pf
        fubjP p pf   = p:pf

        rbs :: Pune
        rbs = '\0'

znva :: VB ()
znva = vagrenpg (hayvarf . znc rapbqr . yvarf)
