TRIGGER PROCEDURE FOR WRITE OF Last-Sko.

{trg\c_w_trg.i &Fil=SkoTex.Last-Sko &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Last-Sko" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Last-Sko.Last-Id) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Last-Sko"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Last-Sko.Last-Id).
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
         ELogg.TabellNavn     = "Last-Sko" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Last-Sko.Last-Id) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Last-Sko"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Last-Sko.Last-Id).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */
