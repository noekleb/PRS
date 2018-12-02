TRIGGER PROCEDURE FOR WRITE OF VgKat.

{trg\c_w_trg.i &Fil=SkoTex.VgKat &TYPE=W}
    
FIND ELogg WHERE 
     ELogg.TabellNavn     = "VgKat" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VgKat.Vg) + CHR(1) + STRING(VgKat.VgKat)
                                             + CHR(1) + STRING(VgKat.KatNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VgKat"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VgKat.Vg) + CHR(1) + STRING(VgKat.VgKat)
                                             + CHR(1) + STRING(VgKat.KatNr).
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
        ELogg.TabellNavn     = "vgkat" AND
        ELogg.EksterntSystem = "WEBBUT"    AND
        ELogg.Verdier        = STRING(VgKat.Vg) + '|' + STRING(VgKat.VgKat) NO-ERROR.
    IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN 
            ELogg.TabellNavn     = "vgkat"
            ELogg.EksterntSystem = "WEBBUT"   
            ELogg.Verdier        = STRING(VgKat.Vg) + '|' + STRING(VgKat.VgKat).
    END.
    ASSIGN 
        ELogg.EndringsType = 1 
        ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


