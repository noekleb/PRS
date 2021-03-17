/* ovbunt_skrivfaktura.p
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE: 
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.
    FIND FIRST Ovbunt WHERE 
        OvBunt.BuntNr = INT(ihBuffer:BUFFER-FIELD('BuntNr'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    IF AVAIL OvBunt AND OvBunt.DatoOppdatert <> ? THEN
    BEHANDLE: 
    DO:
      FOR EACH OvBuffer OF OvBunt:
        DELETE OvBuffer.
      END.
/*      DELETE ovbunt.*/
      ASSIGN 
          obOk     = TRUE
          ocReturn = ''
          .
    END. /* BEHANDLE */
  hQuery:GET-NEXT().
END.

