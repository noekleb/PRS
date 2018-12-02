TRIGGER PROCEDURE FOR WRITE OF Rabatt.


{trg\c_w_trg.i &Fil=SkoTex.Rabatt &TYPE=W}
FIND ELogg WHERE 
     ELogg.TabellNavn     = "Rabatt" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Rabatt.RabKod) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Rabatt"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Rabatt.RabKod).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


