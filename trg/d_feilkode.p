TRIGGER PROCEDURE FOR DELETE OF Feilkode.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "FeilKode" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(FeilKode.FeilKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "FeilKode"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(FeilKode.FeilKode).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


