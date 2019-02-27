TRIGGER PROCEDURE FOR DELETE OF LevSAnt.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "LevSort" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(LevSAnt.LevNr) + "," + LevSAnt.SortID NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "LevSort"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(LevSAnt.LevNr) + "," + LevSAnt.SortID.
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.

RELEASE ELogg.


