current-window:width = 300.

DEF BUFFER bufTransLogg FOR Translogg.

for each TransLogg exclusive-lock where
    TransLogg.BAtchNr = 5798216:

  FIND FIRST buftranslogg EXCLUSIVE-LOCK WHERE
      bufTransLogg.BatchNr = TransLogg.BatchNr AND
      bufTransLogg.ArtikkelNr = Translogg.ArtikkelNr AND
      bufTransLogg.Storl      = Translogg.Storl AND
      bufTransLogg.Antall     = Translogg.Antall * -1 NO-ERROR.

  
  IF AVAILABLE bufTranslogg THEN
  DO:

      display
TransLogg.BatchN
TransLogg.Antal
bufTranslogg.Antall
with width 300.

      DELETE translogg.
      DELETE bufTranslogg.
  END.


  
end.     
  
