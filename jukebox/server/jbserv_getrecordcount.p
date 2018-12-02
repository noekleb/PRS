DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBufferList       AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryCriteria    AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiCount            AS INT  NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO EXTENT 100.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR iy         AS INT    NO-UNDO.

{incl/validatesession.i}

CREATE QUERY hQuery.

DO ix = 1 TO NUM-ENTRIES(icBufferList):
  CREATE BUFFER hBuffer[ix] FOR TABLE ENTRY(1,ENTRY(ix,icBufferList),";") NO-ERROR.
  IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO:
    ocReturn = "Invalid buffer definition: " + ENTRY(1,ENTRY(ix,icBufferList),";") + CHR(10) + PROGRAM-NAME(1).
    RETURN.
  END.
  hQuery:ADD-BUFFER(hBuffer[ix]).
END.

hQuery:QUERY-PREPARE("FOR EACH " + ENTRY(1,ENTRY(1,icBufferList),";") + " NO-LOCK " + icQueryCriteria) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "FOR EACH " + ENTRY(1,ENTRY(1,icBufferList),";") + " NO-LOCK " + icQueryCriteria + CHR(10) + 
             ERROR-STATUS:GET-MESSAGE(1).
  RETURN.
END.

ix = ix - 1.
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  oiCount = oiCount + 1.
  hQuery:GET-NEXT().
END.

FINALLY:
  DO ix = 1 TO 100:
    IF VALID-HANDLE(hBuffer[ix]) THEN
      DELETE OBJECT hBuffer[ix].
    ELSE LEAVE.
  END.
  DELETE OBJECT hQuery.
END.

