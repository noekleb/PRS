TRIGGER PROCEDURE FOR CREATE OF butikkSelger.

{trg\c_w_trg.i &Fil=SkoTex.butikkSelger &Type=C}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Selger" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(butikkSelger.SelgerNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Selger"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(butikkSelger.SelgerNr).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


