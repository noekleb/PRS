TRIGGER PROCEDURE FOR WRITE OF VarGr.

{trg\c_w_trg.i &Fil=SkoTex.VarGr &TYPE=W}

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
    /* Endring på varegruppe */
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
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


