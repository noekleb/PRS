TRIGGER PROCEDURE FOR DELETE OF KundeKort.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Kunde" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.Kortnr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Kunde"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.Kortnr).
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
        ELogg.TabellNavn     = "KundeKort" AND
        ELogg.EksterntSystem = "WEBBUT"    AND
        ELogg.Verdier        = STRING(KundeKort.KortNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN 
            ELogg.TabellNavn     = "KundeKort"
            ELogg.EksterntSystem = "WEBBUT"   
            ELogg.Verdier        = STRING(KundeKort.KortNr).
    END.
    ASSIGN 
        ELogg.EndringsType = 3 
        ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


