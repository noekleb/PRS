TRIGGER PROCEDURE FOR WRITE OF VgAkt.

{trg\c_w_trg.i &Fil=SkoTex.VgAkt &TYPE=W}
    
FIND ELogg WHERE 
     ELogg.TabellNavn     = "VgAkt" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VgAkt.Vg) + CHR(1) + STRING(VgAkt.AktNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VgAkt"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VgAkt.Vg) + CHR(1) + STRING(VgAkt.AktNr).
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
        ELogg.TabellNavn     = "vgakt" AND
        ELogg.EksterntSystem = "WEBBUT"    AND
        ELogg.Verdier        = STRING(VgAkt.Vg) + '|' + STRING(VgAkt.AktNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN 
            ELogg.TabellNavn     = "vgakt"
            ELogg.EksterntSystem = "WEBBUT"   
            ELogg.Verdier        = STRING(VgAkt.Vg) + '|' + STRING(VgAkt.AktNr).
    END.
    ASSIGN 
        ELogg.EndringsType = 1 
        ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


