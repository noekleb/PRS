TRIGGER PROCEDURE FOR WRITE OF Moms.


{trg\c_w_trg.i &Fil=SkoTex.Moms &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Moms" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Moms.MomsKod) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Moms"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Moms.MomsKod).
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
         ELogg.TabellNavn     = "Moms" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Moms.MomsKod) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Moms"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Moms.MomsKod).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


