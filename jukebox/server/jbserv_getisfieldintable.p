DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBufferName AS CHAR NO-UNDO.
DEF INPUT  PARAM icColumn     AS CHAR NO-UNDO.
DEF OUTPUT PARAM obItIs       AS LOG NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.
DEF VAR ocReturn   AS CHAR NO-UNDO.

{incl/validatesession.i}

CREATE BUFFER hBuffer FOR TABLE icBufferName NO-ERROR.
IF VALID-HANDLE(hBuffer) THEN DO:
  hField = hBuffer:BUFFER-FIELD(icColumn) NO-ERROR.
  obItIs = VALID-HANDLE(hField).
END.
ELSE obItIs = NO.

DELETE OBJECT hBuffer NO-ERROR.


