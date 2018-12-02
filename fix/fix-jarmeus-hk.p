def stream inn.

input stream inn from value("jarmeus.log") no-echo.

def var wArtikkelNr like ArtBAs.Artikkel no-undo.
def var wBildNR     like ArtBas.BildNR   no-undo.

LOOPEN:
repeat:
  import stream inn
    wArtikkelNr
    wBildNr.
    
    
  if wBildNr = 0 then
    next LOOPEN.
      
  find ArtBas exclusive-lock where
    ArtBas.ArtikkelNr = wArtikkelNR no-error.
    
  if available ArtBas then
    do:
      /* Tar bort gammelt bildedata info */
      for each BildeRegister of ArtBAs:
        for each BildeData of BildeRegister:
          delete BildeData.
        end.
        delete BildeRegister.
      end.
      
      assign
        ArtBas.BildNr = wBildNr.
             
      pause 0.
      /*if wBildNr <> ArtBas.BildNR then */
        display "*"
        wArtikkelNr
        ArtBas.ArtikkelNr when available ArtBas
        wBildNr
        ArtBas.BildNr.

    end.
end. /* LOOPEN */

input stream inn close.
