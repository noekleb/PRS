TRIGGER PROCEDURE FOR WRITE OF Kunde OLD BUFFER oldKunde.

DEF VAR trgiAnt AS INT.

ASSIGN
  Kunde.EDato    = TODAY
  Kunde.ETid     = TIME
  Kunde.BrukerId = USERID("skotex")
  Kunde.ByNavn   = ''.
  
/*
IF kunde.WebKunde = TRUE THEN 
  ASSIGN
    Kunde.EksterntKundeNr = (IF TRIM(Kunde.EksterntKundeNr) = '' 
                               THEN STRING(Kunde.KundeNr) 
                               ELSE Kunde.EksterntKundeNr 
                             ).
*/

/* Teller opp kundekort. */
FOR EACH KundeKort OF Kunde EXCLUSIVE-LOCK:
    ASSIGN trgIAnt = trgIAnt + 1.
END.
/* Oppdaterer kort med blank innehaver. */
IF trgiAnt = 1 THEN
    FOR EACH KundeKort OF Kunde EXCLUSIVE-LOCK:
        ASSIGN KundeKort.Innehaver = Kunde.Navn NO-ERROR.
    END.
ELSE
    FOR EACH KundeKort OF Kunde EXCLUSIVE-LOCK WHERE (KundeKort.Innehaver = "" OR 
                                                      KundeKort.Innehaver MATCHES "*Ukjent*" OR
                                                      Kundekort.Innehaver = oldKunde.Navn):
        ASSIGN KundeKort.Innehaver = Kunde.Navn NO-ERROR.
    END.
/* IF oldKunde.BetType = 0 OR (oldKunde.BetType <> 2 AND Kunde.BetType <> 2) THEN */
/*     RETURN.                                                                    */
FIND ELogg EXCLUSIVE-LOCK WHERE 
     ELogg.TabellNavn     = "Kunde" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Kunde.KundeNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Kunde"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Kunde.KundeNr).
END.
/* Vi måste behandla en ändring från kreditkund till ikke som en delete */
/* I normala fall hanteras en delete genom att vi tar bort kundekort    */
ASSIGN ELogg.EndringsType = 1 /* IF Kunde.BetType = 2 THEN 1 ELSE 3 */
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem AND Kunde.WebKunde THEN
WEBBUTIKK:
DO:
    FIND ELogg EXCLUSIVE-LOCK WHERE 
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
        ELogg.EndringsType = 1 
        ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


