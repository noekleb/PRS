TRIGGER PROCEDURE FOR DELETE OF UtbetType.
    
FIND ELogg WHERE 
     ELogg.TabellNavn     = "UtbetType" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(UtbetType.UtBetTId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "UtbetType"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(UtbetType.UtBetTId).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


