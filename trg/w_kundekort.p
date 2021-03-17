TRIGGER PROCEDURE FOR WRITE OF KundeKort OLD BUFFER oldKundeKort.

ASSIGN
  KundeKort.EDato    = TODAY
  KundeKort.ETid     = TIME
  KundeKort.BrukerId = USERID("skotex").
  
ASSIGN 
  KundeKort.KortNr   = LEFT-TRIM(KundeKort.KortNr,'0') NO-ERROR.

IF (oldKundeKort.KundeNr = 0 AND KundeKort.Sperret = TRUE) OR (oldKundeKort.Sperret = TRUE AND KundeKort.Sperret = TRUE) THEN
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Kunde" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Kunde"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr).
    END.
    ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE.
END.

ELSE DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Kunde" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Kunde"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(KundeKort.KundeNr) + ";" + STRING(KundeKort.KortNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
END.

IF AVAILABLE Elogg THEN
    RELEASE ELogg.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
IF AVAILABLE Kunde THEN 
DO:
FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAILABLE trgEkstEDBSystem AND Kunde.WebKunde THEN
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
            ELogg.EndringsType = 1 
            ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
    END. /* WEBBUTIKK */
END.

    

