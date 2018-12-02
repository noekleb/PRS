/* Get a list of rowids from a buffer. 
   The list can be obtained from one of the buffers in a join, specify which in the "icBufferReturnList" parameter
   (If no return buffer is specified, the last entry in the buffer-list is assumed)
----------------------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBufferList       AS CHAR NO-UNDO.
DEF INPUT  PARAM icBufferReturnList AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryCriteria    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocRowidList        AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO EXTENT 10.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR iy         AS INT    NO-UNDO.

{incl/validatesession.i}

IF icBufferReturnList = "" THEN icBufferReturnList = ENTRY(NUM-ENTRIES(icBufferList),icBufferList).

CREATE QUERY hQuery.

DO ix = 1 TO NUM-ENTRIES(icBufferList):
  CREATE BUFFER hBuffer[ix] FOR TABLE ENTRY(1,ENTRY(ix,icBufferList),";") NO-ERROR.
  IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO: 
    DELETE OBJECT hQuery NO-ERROR. 
    DO iy = 1 TO ix - 1:
      DELETE OBJECT hBuffer[iy] NO-ERROR.
    END.
    IF NOT ENTRY(1,ENTRY(ix,icBufferList),";") BEGINS "JBox" THEN
      ocReturn = "Invalid buffer definition: " + ENTRY(1,ENTRY(ix,icBufferList),";").
    RETURN.
  END.
  hQuery:ADD-BUFFER(hBuffer[ix]).
END.

hQuery:QUERY-PREPARE("FOR EACH " + ENTRY(1,ENTRY(1,icBufferList),";") + " NO-LOCK " + icQueryCriteria) NO-ERROR.
IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
  ocReturn = "FOR EACH " + ENTRY(1,ENTRY(1,icBufferList),";") + " NO-LOCK " + icQueryCriteria + CHR(10) + 
             ERROR-STATUS:GET-MESSAGE(1).
  RETURN.
END.

ix = ix - 1.
hQuery:QUERY-OPEN() NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "FOR EACH " + ENTRY(1,ENTRY(1,icBufferList),";") + " NO-LOCK " + icQueryCriteria + CHR(10) + 
             ERROR-STATUS:GET-MESSAGE(1).
  RETURN.
END.

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  DO iy = 1 TO ix:
    IF hBuffer[iy]:AVAIL AND CAN-DO(icBufferReturnList,hBuffer[iy]:NAME) THEN 
      ocRowIdList = ocRowIdList + STRING(hBuffer[iy]:ROWID) + ",".
  END.
  hQuery:GET-NEXT().
END.
ocRowIdList = TRIM(ocRowIdList,",").

DO ix = 1 TO 100:
  IF VALID-HANDLE(hBuffer[ix]) THEN
    DELETE OBJECT hBuffer[ix].
  ELSE LEAVE.
END.
DELETE OBJECT hQuery.

