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

DEFINE BUFFER bufNets FOR Nets.

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
    FIND FIRST bufNets WHERE 
       bufNets.iJBoxCompanyId = INT(STRING(ihBuffer:BUFFER-FIELD('iJBoxCompanyId'):BUFFER-VALUE)) AND 
       bufNets.TransactionId  = STRING(ihBuffer:BUFFER-FIELD('TransactionId'):BUFFER-VALUE)
       EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bufNets THEN 
        DELETE bufNets.
    
    obOk = NOT ERROR-STATUS:ERROR.
    IF NOT obOk THEN
    DO:
      ocReturn = ERROR-STATUS:GET-MESSAGE(1).
      LEAVE.
    END.
  END.
  IF AVAIL bufNets THEN RELEASE bufNets.
  hQuery:GET-NEXT().
END.

