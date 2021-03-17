TRIGGER PROCEDURE FOR WRITE OF SaSong.

{trg\c_w_trg.i &Fil=SkoTex.Sasong &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Sasong" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Sasong.Sasong) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Sasong"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Sasong.Sasong).
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
         ELogg.TabellNavn     = "Sasong" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Sasong.Sasong) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Sasong"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Sasong.Sasong).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


