TRIGGER PROCEDURE FOR WRITE OF Handtering.

{trg\c_w_trg.i &Fil=SkoTex.Handtering &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Handtering" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Handtering.HandKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Handtering"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Handtering.HandKode).
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
         ELogg.TabellNavn     = "Handtering" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Handtering.HandKode) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Handtering"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Handtering.HandKode).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


