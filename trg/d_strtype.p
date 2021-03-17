TRIGGER PROCEDURE FOR DELETE OF StrType.

FOR EACH StrTStr OF StrType:
    DELETE StrTStr.
END.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "StrType" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(StrType.StrTypeID) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "StrType"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(StrType.StrTypeID).
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
        ELogg.TabellNavn     = "StrType" AND
        ELogg.EksterntSystem = "WEBBUT"    AND
        ELogg.Verdier        = STRING(StrType.StrTypeId) NO-ERROR.
    IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN 
            ELogg.TabellNavn     = "StrType"
            ELogg.EksterntSystem = "WEBBUT"   
            ELogg.Verdier        = STRING(StrType.StrTypeId).
    END.
    ASSIGN 
        ELogg.EndringsType = 3 
        ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


