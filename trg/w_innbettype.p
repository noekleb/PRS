TRIGGER PROCEDURE FOR WRITE OF InnBetType.

{trg\c_w_trg.i &Fil=SkoTex.InnBetType &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "InnBetType" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(InnBetType.InnBetTId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "InnBetType"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(InnBetType.InnBetTId).
END.
ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
RELEASE ELogg.



