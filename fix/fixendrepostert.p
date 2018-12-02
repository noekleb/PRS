for each Butiker no-lock:
  for each TransLogg exclusive-lock where
    TransLogg.Vg      = 52 and
    TransLogg.LopNr  <= 1002 and
    TransLogg.Postert = true and
    TransLogg.RegistrertDato >= 01/28/99:
        
    pause 0.    
    display 
      TransLogg.ArtikkelNr
      TransLogg.Vg
      TransLogg.LopNr
      TransLogg.RegistrertDato
      TransLogg.PostertDato.
      
    assign
      TransLogg.Postert     = false
      TransLogg.PostertDato = ?
      TransLogg.PostertTid  = 0.

    
  end.
end.    
