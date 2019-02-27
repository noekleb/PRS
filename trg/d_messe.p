TRIGGER PROCEDURE FOR DELETE OF Messe.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Messe" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Messe.MesseNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Messe"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Messe.MesseNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


