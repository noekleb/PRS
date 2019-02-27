TRIGGER PROCEDURE FOR WRITE OF HuvGr.

{trg\c_w_trg.i &Fil=SkoTex.HuvGr &TYPE=W}

    FIND ELogg WHERE 
     ELogg.TabellNavn     = "HuvGr" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(HuvGr.Hg) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "HuvGr"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(HuvGr.Hg).
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
         ELogg.TabellNavn     = "HuvGr" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(HuvGr.Hg) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "HuvGr"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(HuvGr.Hg).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


