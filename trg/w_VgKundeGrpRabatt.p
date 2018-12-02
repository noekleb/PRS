TRIGGER PROCEDURE FOR WRITE OF VgKundeGrpRabatt.

{trg\c_w_trg.i &Fil=SkoTex.VgKundeGrpRabatt &Type="W"}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "VarGr" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VgKundeGrpRabatt.Vg) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VarGr"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VgKundeGrpRabatt.Vg).
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
        ELogg.TabellNavn     = "VgKundeGrpRabatt" AND
        ELogg.EksterntSystem = "WEBBUT"    AND
        ELogg.Verdier        = STRING(VgKundeGrpRabatt.Vg) + '|' + STRING(VgKundeGrpRabatt.GruppeId) NO-ERROR.
    IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN 
            ELogg.TabellNavn     = "VgKundeGrpRabatt"
            ELogg.EksterntSystem = "WEBBUT"   
            ELogg.Verdier        = STRING(VgKundeGrpRabatt.Vg) + '|' + STRING(VgKundeGrpRabatt.GruppeId).
    END.
    ASSIGN 
        ELogg.EndringsType = 1 
        ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


