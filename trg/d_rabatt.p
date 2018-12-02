TRIGGER PROCEDURE FOR DELETE OF Rabatt.

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
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


