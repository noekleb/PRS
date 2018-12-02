/* Registrer Slett SIEMoms record
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
    FIND FIRST SIETranstype WHERE
      SIETranstype.ButikkNr = INT(ihBuffer:BUFFER-FIELD('ButikkNr'):BUFFER-VALUE) AND  
      SIETranstype.TTId     = INT(ihBuffer:BUFFER-FIELD('TTId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL SIETranstype THEN
    DO:

      DELETE SIETranstype NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL SIETranstype THEN RELEASE SIETranstype.
  hQuery:GET-NEXT().
END.

