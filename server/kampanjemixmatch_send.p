/* Sjekk om det finnes en post i ProduktFamilie
   Opprettet: 29.05.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
  DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
  DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.
  
  DEF VAR fKampId AS DEC NO-UNDO.

  ASSIGN
      obOk    = TRUE
      fKampId = DEC(ENTRY(1,icParam,';'))
      .

  /*Validate*******************************/
  FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = fKampId NO-LOCK NO-ERROR.
  IF NOT AVAIL KampanjeMixMatch THEN
  DO:
    ASSIGN 
      ocReturn = 'Kunne ikke finne kampanjen med kampanjeid ' + STRING(fKampId)
      obOk     = FALSE
    .
    RETURN.
  END.
  /*Need relation to butikk*/
  IF NOT CAN-FIND(FIRST KampanjeButikker OF KampanjeMixMatch) THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') + 'FEIL: Kan ikke sendes, mangler knyttning mot butikk.'
      obOk     = FALSE
    .
  END.
  /*Min.antall need to have a value gt 0*/
  FOR EACH KampanjeTilbArtikkel OF KampanjeMixMatch NO-LOCK:
    IF KampanjeTilbArtikkel.KampTilbArtMinAntall LE 0 THEN LEAVE.
  END.
  IF AVAIL KampanjeTilbArtikkel THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') 
                          + 'FEIL: Kan ikke sendes, poster i tilbudsnr.: ' + STRING(KampanjeTilbArtikkel.KampTilbId) 
                          + ' har varelinjer med min.antall lik 0.'
      obOk     = FALSE
    .
  END.
  IF NOT obOk THEN RETURN.
  /***********************************Validate*/

  FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = fKampId EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL KampanjeMixMatch THEN
  DO:
    ASSIGN 
      KampanjeMixMatch.KampSendtDato = TODAY
      KampanjeMixMatch.KampSendtTid  = TIME
    NO-ERROR.
    FIND Elogg 
      WHERE Elogg.Tabellnavn     = "KampanjeMixMatch" 
        AND ELogg.EksterntSystem = "POS" 
        AND Elogg.Verdier        = STRING(KampanjeMixMatch.KampId) 
      EXCLUSIVE NO-ERROR.
    IF NOT AVAIL Elogg THEN 
    DO:
      CREATE Elogg.
      ASSIGN 
        Elogg.Tabellnavn     = "KampanjeMixMatch"
        ELogg.EksterntSystem = "POS"
        Elogg.Verdier        = STRING(KampanjeMixMatch.KampId)
      .
    END.

    ASSIGN 
      Elogg.Behandlet    = FALSE
      ELogg.EndringsType = IF KampanjeMixMatch.KampKlar = TRUE THEN 1 ELSE 3
    NO-ERROR.
    
    ASSIGN 
      ocReturn = IF ERROR-STATUS:ERROR THEN ERROR-STATUS:GET-MESSAGE(1) ELSE ''
      obOk     = NOT ERROR-STATUS:ERROR
    .
  END.
  ELSE
    ASSIGN 
      ocReturn = 'Klarte ikke å utføre sending, kontakt administrator'
      obOk     = FALSE
    .
