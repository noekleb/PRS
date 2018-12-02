
message "Skal overføring av dokumentnummer til bilderegister starte?"
        view-as alert-box question buttons YES-NO title "Bekreft" 
        update wOk as log.
        
if wOk <> true then
  return.        

/*
assign
  current-window:width = 180.
*/  
  
for each konvreg no-lock where
  KonvReg.EDB-System = "FLEXICON" and
  KonvReg.Tabell     = "ArtBAs": 
  
  find ArtBAs no-lock where
    ArtBas.ArtikkelNr = int(KonvReg.InterntId) no-error.
  
  if available ArtBas then
    do:
      find bilderegister exclusive-lock where
        bilderegister.bildnr = ArtBas.bildNr no-error.
      if available Bilderegister then
        assign
          Bilderegister.Tekst = KonvReg.EkstId.
      {sww.i}
      /*    
      pause 0.
      display
        ArtBas.ArtikkelNr
        ArtBas.bildNr
        KonvReg.EkstId
        Bilderegister.bildnr when available bilderegister
        Bilderegister.Tekst when available Bilderegister
      with width 178.
      */
      {swn.i}
    end.
end.  
