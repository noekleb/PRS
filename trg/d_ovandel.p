TRIGGER PROCEDURE FOR DELETE OF Ovandel.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Ovandel" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Ovandel.Ov-Id) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Ovandel"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Ovandel.Ov-Id).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Ovandel" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Ovandel.Ov-Id) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Avdeling"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Ovandel.Ov-Id).
    END.
    ASSIGN ELogg.EndringsType = 3 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


