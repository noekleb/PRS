TRIGGER PROCEDURE FOR DELETE OF InnerSula.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "InnerSula" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(InnerSula.Inner-Id) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "InnerSula"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(InnerSula.Inner-Id).
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
         ELogg.TabellNavn     = "Innersula" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Innersula.Inner-Id) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Innersula"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Innersula.Inner-Id).
    END.
    ASSIGN ELogg.EndringsType = 3 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


