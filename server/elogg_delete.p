/* Registrer Slett kundekort record
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST ELogg WHERE
        ELogg.TabellNavn     = STRING(ihBuffer:BUFFER-FIELD('TabellNavn'):BUFFER-VALUE) AND  
        ELogg.EksterntSystem = STRING(ihBuffer:BUFFER-FIELD('EksterntSystem'):BUFFER-VALUE) AND 
        ELogg.Verdier        = STRING(ihBuffer:BUFFER-FIELD('Verdier'):BUFFER-VALUE)
        EXCLUSIVE-LOCK NO-ERROR.
    
      /* DØDEN */
      IF AVAILABLE Elogg THEN
          DELETE Elogg.
      
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
  END.
  IF AVAIL ELogg THEN RELEASE Elogg.
  hQuery:GET-NEXT().
END.

