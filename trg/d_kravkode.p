TRIGGER PROCEDURE FOR DELETE OF Kravkode.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "KravKode" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(KravKode.KravKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KravKode"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(KravKode.KravKode).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


