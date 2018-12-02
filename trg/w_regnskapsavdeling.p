TRIGGER PROCEDURE FOR WRITE OF Regnskapsavdeling.

{trg\c_w_trg.i &Fil=SkoTex.Regnskapsavdeling &Type=W}


DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Regnskapsavdeling" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Regnskapsavdeling.RAvdNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Regnskapsavdeling"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Regnskapsavdeling.RAvdNr).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */
