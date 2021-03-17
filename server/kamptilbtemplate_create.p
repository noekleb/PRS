
/* Sjekk om det finnes en post i kamptilbtemplate
   Opprettet: 29.05.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
  DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
  DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.
  
  DEF VAR fNext AS DEC NO-UNDO.

  DEF BUFFER bKTT FOR KampTilbTemplate.
  
  FIND LAST bKTT WHERE bKTT.kampid         = DEC(ENTRY(1,icParam,';'))
                   AND bKTT.kamptilbid     = INT(ENTRY(2,icParam,';'))
                 NO-LOCK NO-ERROR.
  IF AVAIL bKTT THEN
    ASSIGN
      obOk = FALSE
      ocReturn = 'Posten finnes allerede registrert'
    .
  ELSE
  DO:
    
    CREATE KampTilbTemplate.
  
    ASSIGN 
      KampTilbTemplate.KampId         = DEC(ENTRY(1,icParam,';'))
      KampTilbTemplate.KampTilbId     = INT(ENTRY(2,icParam,';'))
    NO-ERROR.
    
    obOk = NOT ERROR-STATUS:ERROR.
    IF NOT obOk THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).
  END.
