DEF VAR iant AS INT NO-UNDO.
DEF VAR iantMedfil AS INT NO-UNDO.
DEF VAR iAntMedBonger AS INT NO-UNDO.

FOR EACH DataSett NO-LOCK WHERE
  DataSett.FilId >= 0 AND
  DataSett.SettStatus >= 2 AND 
  DataSett.SettStatus <= 8 AND
  DataSett.Behandlet  >= 3 AND
  DataSett.Behandlet  <= 4:
  
  
  DISPLAY 
  DataSett.DataSettId
  DataSEtt.ButikkNr
  Datasett.Dato
  Datasett.SettStatus
  DataSEtt.Behandlet
  .
  
    iant = iant + 1.        
    IF CAN-FIND(FIRST Filer OF Datasett) THEN
      iantMedfil = iantMedfil + 1.
    IF CAN-FIND(FIRST BongHode OF Datasett) THEN
        iAntMedBonger = iAntMedBonger + 1.
END.
    
MESSAGE iant iantMedfil iAntMedBonger
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
