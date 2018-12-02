TRIGGER PROCEDURE FOR WRITE OF PrintLoggType.

{trg\c_w_trg.i &Fil=SkoTex.PrintLoggType &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "PrintLoggType" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = PrintLoggType.LoggType NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "PrintLoggType"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = PrintLoggType.LoggType.
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


