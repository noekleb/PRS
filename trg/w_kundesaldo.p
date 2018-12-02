TRIGGER PROCEDURE FOR WRITE OF KundeSaldo.

assign
  KundeSaldo.EDato    = today
  KundeSaldo.ETid     = time
  KundeSaldo.BrukerId = userid("skotex").

IF CAN-FIND(Kunde WHERE Kunde.KundeNr = KundeSaldo.KundeNr AND
                   Kunde.BetType = 2) THEN DO:
  IF NOT CAN-FIND(ELogg WHERE 
       ELogg.TabellNavn     = "Kunde"  AND
       ELogg.EksterntSystem = "POS"    AND
       ELogg.Verdier        = STRING(KundeSaldo.KundeNr)) THEN DO:
      CREATE Elogg.
      ASSIGN ELogg.TabellNavn     = "Kunde"
             ELogg.EksterntSystem = "POS"   
             ELogg.Verdier        = STRING(KundeSaldo.KundeNr)
             ELogg.EndringsType   = 1
             ELogg.Behandlet      = FALSE.
  END.
END.
RELEASE ELogg.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
FIND Kunde NO-LOCK WHERE
  Kunde.KundeNr = KundeSaldo.KundeNr NO-ERROR.
IF AVAILABLE Kunde THEN 
DO:
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAILABLE trgEkstEDBSystem AND Kunde.WebKunde THEN
    WEBBUTIKK:
    DO:
        FIND ELogg WHERE 
            ELogg.TabellNavn     = "KundeSaldo" AND
            ELogg.EksterntSystem = "WEBBUT"    AND
            ELogg.Verdier        = STRING(KundeSaldo.KundeNr) + '|' + STRING(KundeSaldo.ButikkNr) NO-ERROR.
        IF NOT AVAIL Elogg THEN 
        DO:
            CREATE Elogg.
            ASSIGN 
                ELogg.TabellNavn     = "KundeSaldo"
                ELogg.EksterntSystem = "WEBBUT"   
                ELogg.Verdier        = STRING(KundeSaldo.KundeNr) + '|' + STRING(KundeSaldo.ButikkNr).
        END.
        ASSIGN 
            ELogg.EndringsType = 1 
            ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
    END. /* WEBBUTIKK */
END.


