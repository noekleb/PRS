DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icDbName     AS CHAR NO-UNDO.
DEF INPUT  PARAM icBufferName AS CHAR NO-UNDO.
DEF INPUT  PARAM icColumn     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocDataType   AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.
DEF VAR ocReturn   AS CHAR NO-UNDO.

{incl/validatesession.i}

IF NUM-ENTRIES(icColumn,".") = 3 THEN
  ASSIGN icDbName     = ENTRY(1,icColumn,".")
         icBufferName = ENTRY(2,icColumn,".")
         icColumn     = ENTRY(3,icColumn,".")
         .
ELSE IF NUM-ENTRIES(icColumn,".") = 2 THEN
  ASSIGN icBufferName = ENTRY(1,icColumn,".")
         icColumn     = ENTRY(2,icColumn,".")
         .

IF icDbName NE "" THEN icBufferName = icDbName + "." + icBufferName.

CREATE BUFFER hBuffer FOR TABLE icBufferName NO-ERROR.
IF VALID-HANDLE(hBuffer) THEN DO:
  hField = hBuffer:BUFFER-FIELD(icColumn) NO-ERROR.
  IF VALID-HANDLE(hField) THEN
    ocDataType = hField:DATA-TYPE.
END.

DELETE OBJECT hBuffer NO-ERROR.


