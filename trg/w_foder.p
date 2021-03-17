TRIGGER PROCEDURE FOR WRITE OF foder.

{trg\c_w_trg.i &Fil=SkoTex.Foder &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "foder" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(foder.foder-id) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "foder"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(foder.foder-id).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


