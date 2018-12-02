TRIGGER PROCEDURE FOR WRITE OF Kategori.

{trg\c_w_trg.i &Fil=SkoTex.Kategori &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Kategori" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Kategori.KatNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Kategori"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Kategori.KatNr).
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
         ELogg.TabellNavn     = "Kategori" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Kategori.KatNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Kategori"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Kategori.KatNr).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


