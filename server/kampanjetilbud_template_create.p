
/* Sjekk om det finnes en post i KampanjeTilbud
   Opprettet: 29.05.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
  DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
  DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.
  
  DEF VAR iNextKey AS INT NO-UNDO.

  DEF BUFFER bKT  FOR KampanjeTilbud.
  DEF BUFFER bKTA FOR KampanjeTilbArtikkel.
  
  FOR LAST bKT WHERE bKT.kampid         = DEC(ENTRY(1,icParam,';'))
/*                  AND bKT.kamptilbid     = INT(ENTRY(3,icParam,';')) */
                 NO-LOCK: LEAVE. END.
  iNextKey = IF AVAIL bKT THEN bKT.KampTilbId + 1 ELSE 1.

  FIND FIRST bKT WHERE bKT.kampid         = DEC(ENTRY(2,icParam,';'))
                   AND bKT.kamptilbid     = INT(ENTRY(3,icParam,';'))
                 NO-LOCK NO-ERROR.
  IF NOT AVAIL bKT THEN
  DO:
    ASSIGN
      obOk = FALSE
      ocReturn = 'Malen ble ikke funnet'
    .
    UNDO, LEAVE.
  END.

  CREATE KampanjeTilbud.  
  ASSIGN 
    KampanjeTilbud.KampId         = DEC(ENTRY(1,icParam,';'))
    KampanjeTilbud.KampTilbId     = iNextKey
  NO-ERROR.
  BUFFER-COPY bKT EXCEPT KampId KampTilbId TO KampanjeTilbud NO-ERROR.
  obOk = NOT ERROR-STATUS:ERROR.
  IF NOT obOk THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).

  FOR EACH bKT WHERE bKT.KampId = DEC(ENTRY(2,icParam,';')) 
                 AND bKT.KampTilbId = INT(ENTRY(3,icparam,';')) NO-LOCK, 
      EACH bKTA OF bKT NO-LOCK:
    CREATE KampanjeTilbArtikkel.
    ASSIGN 
      KampanjeTilbArtikkel.KampId = KampanjeTilbud.KampId
      KampanjeTilbArtikkel.KampTilbId = KampanjeTilbud.KampTilbId
      KampanjeTilbArtikkel.KampTilbArtSeq = bKTA.KampTilbArtSeq
    NO-ERROR.
    BUFFER-COPY bKTA EXCEPT KampId KampTilbId KampTilbArtSeq TO KampanjeTilbArtikkel NO-ERROR.

  END.
  obOk = NOT ERROR-STATUS:ERROR.
  IF NOT obOk THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).
