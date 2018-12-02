TRIGGER PROCEDURE FOR DELETE OF GaveKType.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "GaveKType" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(GaveKType.IdentType) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "GaveKType"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(GaveKType.IdentType).
END.
ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE.
RELEASE ELogg.


