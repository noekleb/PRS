/* Registrer Slett VPIMottak record
   Parameter:  
   Opprettet: 25.11.2007             
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
    FIND FIRST VPIMottak WHERE VPIMottak.vpimottakid       = DEC(ihBuffer:BUFFER-FIELD('VPIMottakId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL VPIMottak THEN
    DO:
      DELETE VPIMottak NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL VPIMottak THEN RELEASE VPIMottak.
  hQuery:GET-NEXT().
END.

