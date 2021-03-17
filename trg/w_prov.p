TRIGGER PROCEDURE FOR WRITE OF Prov.


{trg\c_w_trg.i &Fil=SkoTex.Prov &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Prov" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Prov.ProvKod) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Prov"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Prov.ProvKod).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


