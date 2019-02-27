TRIGGER PROCEDURE FOR WRITE OF Messe.

{trg\c_w_trg.i &Fil=SkoTex.Messe &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Messe" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Messe.MesseNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Messe"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Messe.MesseNr).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.

RELEASE ELogg.


