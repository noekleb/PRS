TRIGGER PROCEDURE FOR WRITE OF Klack.

{trg\c_w_trg.i &Fil=SkoTex.Klack &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Klack" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Klack.klack-id) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Klack"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Klack.klack-id).
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
         ELogg.TabellNavn     = "Klack" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Klack.Klack-Id) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Klack"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Klack.Klack-Id).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */



