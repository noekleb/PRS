current-window:width = 300.

for each VgKundeGrpRabatt exclusive-lock:
  find first InfoPOS.HgrRab no-lock where
    HgrRab.Hgr = VgKundeGrpRabatt.Vg and
    HgrRab.KundeGr = VgKundeGrpRabatt.GruppeId and
    HgrRab.KundeNr = 0 and
    can-do('1,2,17',STRING(HgrRab.ProfNr)) and
    can-do('1,2,17',STRING(HgrRab.ButNr)) no-error.
    
  if not available HgrRab then 
  do:
      delete VgKundeGrpRabatt.
      next.
  end.
  /*
  display
  VgKundeGrpRabatt
  with width 300.
  */
end.
