DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icDbName     AS CHAR NO-UNDO.
DEF INPUT  PARAM icBufferName AS CHAR NO-UNDO.
DEF OUTPUT PARAM obInstalled  AS LOG  NO-UNDO.

DEF VAR ocReturn   AS CHAR   NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO.

{incl/validatesession.i}

IF icDbName NE "" THEN icBufferName = icDbName + "." + icBufferName.

CREATE BUFFER hBuffer FOR TABLE icBufferName NO-ERROR.
IF VALID-HANDLE(hBuffer) THEN 
  obInstalled = YES.

DELETE OBJECT hBuffer NO-ERROR.


