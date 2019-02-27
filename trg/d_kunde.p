TRIGGER PROCEDURE FOR DELETE OF Kunde.

/* för tillfället tar vi omhand sletting genom kundekort
FIND ELogg WHERE 
    ELogg.TabellNavn     = "Kunde" AND
    ELogg.EksterntSystem = "POS"    AND
    ELogg.Verdier        = STRING(Kunde.KundeNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
   CREATE Elogg.
   ASSIGN ELogg.TabellNavn     = "Kunde"
          ELogg.EksterntSystem = "POS"   
          ELogg.Verdier        = STRING(Kunde.KundeNr).
END.
ASSIGN ELogg.EndringsType = 3
      ELogg.Behandlet    = FALSE.
 */

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    FIND ELogg WHERE 
        ELogg.TabellNavn     = "Kunde" AND
        ELogg.EksterntSystem = "WEBBUT"    AND
        ELogg.Verdier        = STRING(Kunde.KundeNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN 
            ELogg.TabellNavn     = "Kunde"
            ELogg.EksterntSystem = "WEBBUT"   
            ELogg.Verdier        = STRING(Kunde.KundeNr).
    END.
    ASSIGN 
        ELogg.EndringsType = 3 
        ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */
  

