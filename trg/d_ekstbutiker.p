TRIGGER PROCEDURE FOR DELETE OF ekstbutiker.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "ekstbutiker" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(ekstbutiker.Butik) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ekstbutiker"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ekstbutiker.Butik).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


