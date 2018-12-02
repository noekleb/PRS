for each Translogg where
  Translogg.Dato >= 01/01/2011 and
  translogg.VVareKost = ?:
  display
  Translogg.ArtikkelNR
  Translogg.Dato
  TransLogg.Pris
   TransLogg.VVarekost
  .
  
  if translogg.vvarekost = ? then TransLogg.VVareKost = 0.
end.  
  
FOR EACH Lager EXCLUSIVE-LOCK:
    IF Lager.VVAreKost = ? THEN
        Lager.VVareKost = 0.
END.
