/*
2/1-99 TN Korrigerer lager og artlag.
*/

def var wAntall as int no-undo.
def var wVerdi  as dec no-undo.

for each butiker NO-LOCK WHERE 
    Butiker.Butik = 174:
for each lager where
  lager.butik = butiker.butik AND
    Lager.ArtikkelNr = 9019805:
  
  find artbas where artbas.artikkelnr = lager.artikkelnr no-lock.
  assign
    lager.antsolgt   = 0
    lager.VerdiSolgt = 0
    lager.KjopAnt    = 0
    lager.KjopVerdi  = 0
    lager.OvAnt      = 0
    lager.OvVerdi    = 0
    lager.ReklAnt    = 0
    lager.ReklVerdi  = 0
    lager.ReklLant   = 0
    lager.ReklLVerdi = 0
    lager.gjenkjopant   = 0
    lager.gjenkjopverdi = 0
    lager.SvinnAnt   = 0
    lager.SvinnVerdi = 0
  .
    
  for each artlag where artlag.butik = butiker.butik and
                        Artlag.ArtikkelNr = Lager.ArtikkelNr:
    /* Fiks av lager. */
    assign
      Lager.LagAnt     = Lager.LagAnt     + ArtLag.LagAnt
      Lager.RetAnt     = Lager.RetAnt     + Artlag.RetAnt

      LAger.AntSolgt   = Lager.AntSolgt   + Artlag.salant
      lager.verdisolgt = lager.verdisolgt + artlag.salsum
      lager.KjopAnt    = lager.kjopant    + artlag.kopant
      lager.KjopVerdi  = lager.kjopverdi  + artlag.kopsum
      lager.OvAnt      = lager.OvAnt      + artlag.overfort
      lager.OvVerdi    = lager.ovverdi    + (artlag.overfort * artlag.netpris)
      lager.ReklAnt    = lager.reklant    + artlag.rekant
      lager.ReklVerdi  = lager.reklverdi  + (artlag.rekant * artlag.netpris)

      lager.gjenkjopant   = lager.gjenkjopant   + artlag.retant
      lager.gjenkjopverdi = lager.gjenkjopverdi + (artlag.retant * artlag.netpris)
      lager.SvinnAnt   = lager.SvinnAnt   + artlag.primo
      lager.SvinnVerdi = lager.SvinnVerdi + (artlag.primo * artlag.netpris)
      
      /*lager.VVareKost  = Artlag.NetPris*/
  end.
  

end.
end.
