TRIGGER PROCEDURE FOR WRITE OF KundeBetTrans.

assign
  KundeBetTrans.EDato    = today
  KundeBetTrans.ETid     = time
  KundeBetTrans.BrukerID = userid("skotex").
IF CAN-FIND(Kunde WHERE Kunde.KundeNr = KundeBetTrans.KundeNr AND
                   Kunde.BetType = 2) THEN DO:
  IF NOT CAN-FIND(ELogg WHERE 
       ELogg.TabellNavn     = "Kunde"  AND
       ELogg.EksterntSystem = "POS"    AND
       ELogg.Verdier        = STRING(KundeBetTrans.KundeNr)) THEN DO:
      CREATE Elogg.
      ASSIGN ELogg.TabellNavn     = "Kunde"
             ELogg.EksterntSystem = "POS"   
             ELogg.Verdier        = STRING(KundeBetTrans.KundeNr)
             ELogg.EndringsType   = 1
             ELogg.Behandlet      = FALSE.
  END.
END.
RELEASE ELogg.


