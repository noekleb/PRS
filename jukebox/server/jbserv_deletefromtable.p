/* jbserv_deletefromtable.p
   Created:     mar 05 by Bha
   Note:        Use with care! No referential integrity check!
              - Called via runproc. 
              - Params: Character string: <table>|<crit>
-----------------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK         AS LOG NO-UNDO.


DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO.

CREATE BUFFER hBuffer FOR TABLE ENTRY(1,icParam,"|") NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
  ocReturn = "Couldn't create buffer for table " + ENTRY(1,icParam,"|").
  
IF ocReturn = "" THEN DO:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + ENTRY(1,icParam,"|") + " " + ENTRY(2,icParam,"|")) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
    ocReturn = "Error in query preparation for table " + ENTRY(1,icParam,"|") + CHR(10) +
               "Query string: " + ENTRY(2,icParam,"|").
  IF ocReturn = "" THEN DO TRANSACTION:
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      hBuffer:FIND-CURRENT(EXCLUSIVE-LOCK).
      hBuffer:BUFFER-DELETE().
      hQuery:GET-NEXT().
    END.
  END.
END.

DELETE OBJECT hQuery NO-ERROR.
DELETE OBJECT hBuffer NO-ERROR.

IF ocReturn = "" THEN
  obOk = TRUE.

