TRIGGER PROCEDURE FOR DELETE OF foder.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "foder" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(foder.foder-id) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "foder"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(foder.foder-id).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


