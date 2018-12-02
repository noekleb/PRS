DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.

FIND Lager WHERE LAger.Butik = 16 AND
    Lager.ArtikkelNr = 9828631 NO-ERROR.
DO:
  FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
MESSAGE 'test-1'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  IF AVAILABLE trgEkstEDBSystem THEN
  WEBBUTIKK:
  DO:
      MESSAGE 'test-2'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF NOT CAN-FIND(FIRST ELogg WHERE 
         ELogg.TabellNavn     = "Lager" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                + chr(1) + string(Lager.butik) AND 
         ELogg.EndringsType   = 1) THEN 
    DO:
        MESSAGE 'test-3'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Lager"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                + chr(1) + string(Lager.butik)
               ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DELETE ELogg.

        RELEASE ELogg.
    END.
  END. /* WEBBUTIKK */
END.
