/* Registrer siemoms_kopiermoms.p
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iButikkNr AS INTEGER NO-UNDO.

DEFINE BUFFER bSIETranstype FOR SIETranstype.
DEF VAR hQuery          AS HANDLE NO-UNDO.

ASSIGN
  iButikkNr = INT(ENTRY(1,icParam,'|')).
IF iButikkNr = 0 THEN 
  RETURN.  
  
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
  /* Kopiering til samme butikknr ... ikke tillatt. */
  IF iButikkNr <> INT(ihBuffer:BUFFER-FIELD('ButikkNr'):BUFFER-VALUE) THEN 
  DO TRANSACTION:
    FIND FIRST SIETranstype WHERE
      SIETranstype.ButikkNr = INT(ihBuffer:BUFFER-FIELD('ButikkNr'):BUFFER-VALUE) AND  
      SIETranstype.TTId  = INT(ihBuffer:BUFFER-FIELD('TTId'):BUFFER-VALUE) AND
      SIETransType.TBID  = 1
      EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL SIETranstype THEN
    DO:
      FIND bSIETranstype EXCLUSIVE-LOCK WHERE 
        bSIETranstype.ButikkNr = iButikkNr AND 
        bSIETranstype.TTId  = SIETranstype.TTId AND
        bSIETranstype.TBId  = SIETranstype.TBId
        NO-ERROR.

      IF NOT AVAILABLE bSIETranstype THEN 
        CREATE bSIETranstype.
      BUFFER-COPY SIETranstype EXCEPT ButikkNr 
             TO bSIETranstype
             ASSIGN 
               bSIETranstype.ButikkNr = iButikkNr NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL SIETranstype  THEN RELEASE SIETranstype.   /*..\prg */
  IF AVAIL bSIETranstype THEN RELEASE bSIETranstype.
  hQuery:GET-NEXT().
END.

