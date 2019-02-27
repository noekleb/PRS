TRIGGER PROCEDURE FOR WRITE OF Varemerke.

{trg\c_w_trg.i &Fil=SkoTex.Varemerke &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Varemerke" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Varemerke.VMId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Varemerke"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Varemerke.VMId).
END.
ASSIGN ELogg.EndringsType = 1
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
         ELogg.TabellNavn     = "Varemerke" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Varemerke.VMId) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Varemerke"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Varemerke.VMId).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


