TRIGGER PROCEDURE FOR WRITE OF Avdeling.

{trg\c_w_trg.i &Fil=SkoTex.Avdeling &Type=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Avdeling" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Avdeling.AvdelingNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Avdeling"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Avdeling.AvdelingNr).
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
         ELogg.TabellNavn     = "Avdeling" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Avdeling.Avdelingnr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Avdeling"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Avdeling.Avdelingnr).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


