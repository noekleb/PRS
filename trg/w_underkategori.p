TRIGGER PROCEDURE FOR WRITE OF Underkategori.

{trg\c_w_trg.i &Fil=SkoTex.Underkategori &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Underkategori" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Underkategori.UnderKatNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Underkategori"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Underkategori.UnderKatNr).
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
         ELogg.TabellNavn     = "Underkategori" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Underkategori.UnderKatNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Underkategori"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Underkategori.UnderKatNr).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */
