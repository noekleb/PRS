TRIGGER PROCEDURE FOR DELETE OF KjedeRegion.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "KjedeRegion" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(KjedeRegion.KjedeNr) + "," + STRING(KjedeRegion.RegionNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KjedeRegion"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(KjedeRegion.KjedeNr) + "," + STRING(KjedeRegion.RegionNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.

RELEASE ELogg.


