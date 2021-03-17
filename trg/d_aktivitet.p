TRIGGER PROCEDURE FOR DELETE OF Aktivitet.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Aktivitet" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Aktivitet.AktNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Aktivitet"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Aktivitet.AktNr).
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
         ELogg.TabellNavn     = "Aktivitet" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Aktivitet.AktNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Aktivitet"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Aktivitet.AktNr).
    END.
    ASSIGN ELogg.EndringsType = 3 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


