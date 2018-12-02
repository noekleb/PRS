TRIGGER PROCEDURE FOR DELETE OF VarGr.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "VarGr" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VarGr.Vg) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VarGr"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VarGr.Vg).
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
         ELogg.TabellNavn     = "VarGr" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(VarGr.Vg) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "VarGr"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(VarGr.Vg).
    END.
    ASSIGN ELogg.EndringsType = 3 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


