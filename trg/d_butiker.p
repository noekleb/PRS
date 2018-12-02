TRIGGER PROCEDURE FOR DELETE OF Butiker.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Butiker" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Butiker.Butik) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Butiker"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Butiker.Butik).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


