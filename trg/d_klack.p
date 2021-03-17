TRIGGER PROCEDURE FOR DELETE OF Klack.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Klack" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Klack.klack-id) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Klack"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Klack.klack-id).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


