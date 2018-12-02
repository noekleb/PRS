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

DEFINE BUFFER bSIEMoms FOR SIEMoms.
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
    FIND FIRST SIEMoms WHERE
      SIEMoms.ButikkNr = INT(ihBuffer:BUFFER-FIELD('ButikkNr'):BUFFER-VALUE) AND  
      SIEMoms.MomsKod  = INT(ihBuffer:BUFFER-FIELD('MomsKod'):BUFFER-VALUE)
      EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL SIEMoms THEN
    DO:
      FIND bSIEMoms EXCLUSIVE-LOCK WHERE 
        bSIEMoms.ButikkNr = iButikkNr AND 
        bSIEMoms.MomsKod  = SIEMoms.Momskod
        NO-ERROR.

      IF NOT AVAILABLE bSIEMoms THEN 
        CREATE bSIEMoms.
      BUFFER-COPY SIEMoms EXCEPT ButikkNr 
             TO bSIEMoms
             ASSIGN 
               bSIEMoms.ButikkNr = iButikkNr NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL SIEMoms  THEN RELEASE SIEMoms.
  IF AVAIL bSIEMoms THEN RELEASE bSIEMoms.
  hQuery:GET-NEXT().
END.

