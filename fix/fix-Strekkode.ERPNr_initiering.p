def buffer trgSTrekkode for Strekkode.

for each STrekkode exclusive-lock:

  /* Henter ERPNr fra annen strekkodepost på samme størrelse hvis det finnes fra før. */
  find first trgSTrekkode no-lock where
    trgStrekkode.ArtikkelNr = Strekkode.ArtikkelNr and
    trgSTrekkode.StrKode    = Strekkode.StrKode  and
    recid(trgSTrekkode) <> recid(Strekkode) no-error.
  if available trgStrekkode and
    trim(trgSTrekkode.ERPNr) <> '' and
    Strekkode.ERPNr = '' then
    assign Strekkode.ERPNr = trgSTrekkode.ERPNr.  
  
  /* Initierer ERPNr hvis det er blankt. */  
  if (Strekkode.StrKode <> 0 and 
      Strekkode.ArtikkelNr <> 0 and 
      Strekkode.ERPNr = '') then
    assign Strekkode.ERPNr = string(STrekkode.ArtikkelNr) + trim(string(Strekkode.StrKode,">>999")).

end. 
