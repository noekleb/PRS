TRIGGER PROCEDURE FOR WRITE OF HovedKategori.

{trg\c_w_trg.i &Fil=SkoTex.HovedKategori &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Hovedkategori" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Hovedkategori.HovedKatNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Hovedkategori"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Hovedkategori.HovedKatNr).
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
         ELogg.TabellNavn     = "Hovedkategori" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Hovedkategori.HovedKatNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Hovedkategori"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Hovedkategori.HovedKatNr).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */
