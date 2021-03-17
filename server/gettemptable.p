/* jbserv_gettemptable.p 
   Purpose: Get data from any db table
   Parameters: entry(1,icParam,"|"): Table name
               entry(2,icParam,"|"): Query string (optional)
               
   11.08.05: Tatt kopi av jbserv_gettemptable.p til skotex server
             Orginalen bør brukes!               
-------------------------------------------------------------------------*/               
   
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR icBufferName AS CHAR NO-UNDO.
DEF VAR icCriteria   AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.

FUNCTION GetQueryFalseCrit RETURNS CHARACTER () FORWARD.

/*{incl/validatesession.i}*/

icBufferName = ENTRY(1,icParam,"|").
IF NUM-ENTRIES(icParam,"|") > 1 THEN 
  icCriteria = ENTRY(2,icParam,"|").

CREATE TEMP-TABLE ohTempTable.
ohTempTable:CREATE-LIKE(icBufferName). 
ohTempTable:ADD-NEW-FIELD("RowIdent1","CHARACTER").
ohTempTable:TEMP-TABLE-PREPARE(icBufferName).

CREATE BUFFER hBuffer FOR TABLE icBufferName.

httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE.

IF icCriteria = "WHERE false" THEN
  icCriteria = GetQueryFalseCrit().

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " NO-LOCK " + icCriteria).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  httBuffer:BUFFER-CREATE.
  httBuffer:BUFFER-COPY(hBuffer).
  httBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = STRING(hBuffer:ROWID).
  hQuery:GET-NEXT().
END.

DELETE OBJECT hBuffer.
DELETE OBJECT hQuery.
DELETE OBJECT ohTempTable.

FUNCTION GetQueryFalseCrit RETURNS CHARACTER ():

DEF VAR cPKfields      AS CHAR NO-UNDO.
DEF VAR idx1           AS INT NO-UNDO.
DEF VAR idx2           AS INT NO-UNDO.
DEF VAR hIdxFld        AS HANDLE NO-UNDO.
DEF VAR cWhereString   AS CHAR NO-UNDO INIT "WHERE ".

DO idx1 = 1 TO 100:
  IF hBuffer:INDEX-INFORMATION(idx1) NE ? THEN DO:
    IF ENTRY(3,hBuffer:INDEX-INFORMATION(idx1)) = "1" THEN DO:
      DO idx2 = 5 TO NUM-ENTRIES(hBuffer:INDEX-INFORMATION(idx1)) BY 2:
        cPKfields = cPKfields + ENTRY(idx2,hBuffer:INDEX-INFORMATION(idx1)) + ",".
      END.
      LEAVE.
    END.
  END.
  ELSE LEAVE.
END.  

IF cPKfields NE "" THEN 
  DO idx1 = 1 TO NUM-ENTRIES(TRIM(cPKfields,",")):
    IF idx1 > 1 THEN cWhereString = cWhereString + " AND ".
    hIdxFld = hBuffer:BUFFER-FIELD(ENTRY(idx1,cPKfields)).
    IF VALID-HANDLE(hIdxFld) THEN DO:
      CASE hIdxFld:DATA-TYPE:
        WHEN "character" THEN cWhereString = cWhereString + hIdxFld:NAME + " = 'zaaabbbcccaaaz'".
        WHEN "date"      THEN cWhereString = cWhereString + hIdxFld:NAME + " = DATE('01/11/1111')".
        WHEN "decimal"   THEN cWhereString = cWhereString + hIdxFld:NAME + " = -9876567654233".
        WHEN "integer"   THEN cWhereString = cWhereString + hIdxFld:NAME + " = 2047483648".
        WHEN "logical"   THEN DO:
          IF idx1 = 1 THEN RETURN "WHERE false".
          ELSE cWhereString = cWhereString + hIdxFld:NAME + " = ?".
        END.
        WHEN "recid"     THEN RETURN "WHERE false".
      END CASE.
    END.
  END.
ELSE RETURN "WHERE false".

RETURN cWhereString.

END FUNCTION.
