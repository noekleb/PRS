TRIGGER PROCEDURE FOR DELETE OF Regnskapsavdeling.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Regnskapsavdeling" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Regnskapsavdeling.RAvdNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Regnskapsavdeling"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Regnskapsavdeling.RAvdNr).
    END.
    ASSIGN ELogg.EndringsType = 3 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


