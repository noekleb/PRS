/* Registrer Slett medrabsjekk record
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cFlagg AS CHAR NO-UNDO.
DEF VAR iCL AS INT NO-UNDO.

{syspara.i 5 1 1 iCL INT}

DEF VAR hQuery          AS HANDLE NO-UNDO.

ASSIGN
    cFlagg = ENTRY(1,icParam,'|').

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
    FIND FIRST MedRabSjekk WHERE MedRabSjekk.RabSjekkId = DEC(ihBuffer:BUFFER-FIELD('RabSjekkId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL MedRabSjekk THEN
    DO:
      CASE cFlagg:
          WHEN 'BRUKT' THEN
              assign
                  MedRabSjekk.DatoBrukt     = TODAY
                  MedRabSjekk.BruktButikkNr = iCL 
                  MedRabSjekk.Brukt         = TRUE
              NO-ERROR.
          WHEN 'UBRUKT' THEN
              assign
                  MedRabSjekk.DatoBrukt     = ?
                  MedRabSjekk.BruktButikkNr = 0 
                  MedRabSjekk.Brukt         = FALSE
              NO-ERROR.
          OTHERWISE .
      END CASE.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL MedRabSjekk THEN RELEASE MedRabSjekk.
  hQuery:GET-NEXT().
END.

