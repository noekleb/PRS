TRIGGER PROCEDURE FOR WRITE OF KundeTrans.
IF CAN-FIND(Kunde WHERE Kunde.KundeNr = KundeTrans.KundeNr AND
                   Kunde.BetType = 2) THEN DO:
  IF NOT CAN-FIND(ELogg WHERE 
       ELogg.TabellNavn     = "Kunde"  AND
       ELogg.EksterntSystem = "POS"    AND
       ELogg.Verdier        = STRING(KundeTrans.KundeNr)) THEN DO:
      CREATE Elogg.
      ASSIGN ELogg.TabellNavn     = "Kunde"
             ELogg.EksterntSystem = "POS"   
             ELogg.Verdier        = STRING(KundeTrans.KundeNr)
             ELogg.EndringsType   = 1
             ELogg.Behandlet      = FALSE.
  END.
END.
RELEASE ELogg.


