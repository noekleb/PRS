TRIGGER PROCEDURE FOR DELETE OF Produsent.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Produsent" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Produsent.ProdNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Produsent"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Produsent.ProdNr).
END.
ASSIGN ELogg.EndringsType = 3
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
         ELogg.TabellNavn     = "Produsent" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Produsent.ProdNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Produsent"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Produsent.ProdNr).
    END.
    ASSIGN ELogg.EndringsType = 3 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


