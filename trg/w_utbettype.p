TRIGGER PROCEDURE FOR WRITE OF UtbetType.


{trg\c_w_trg.i &Fil=SkoTex.UtBetType &TYPE=W}

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
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


