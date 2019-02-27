DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBufferList       AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryCriteria    AS CHAR NO-UNDO.
DEF INPUT  PARAM icFieldName        AS CHAR NO-UNDO.
DEF OUTPUT PARAM ofTotal            AS DEC  NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO EXTENT 100.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR iy         AS INT    NO-UNDO.
DEF VAR cDb        AS CHAR   NO-UNDO.
DEF VAR cFldBuffer AS CHAR   NO-UNDO.
DEF VAR cFld       AS CHAR   NO-UNDO.
DEF VAR hFld       AS HANDLE NO-UNDO.

{incl/validatesession.i}

IF NUM-ENTRIES(icBufferList) > 1 AND NUM-ENTRIES(icFieldName,".") < 2 THEN DO:
  ocReturn = "Invalid field name for calculation total " + icFieldName + " when more than one buffer is involved in the query.".
  RETURN.
END.

IF NUM-ENTRIES(icFieldName,".") > 2 THEN
  ASSIGN cFldBuffer = ENTRY(2,icFieldName,".")
         cFld       = ENTRY(3,icFieldName,".").
ELSE IF NUM-ENTRIES(icFieldName,".") > 1 THEN
  ASSIGN cFldBuffer = ENTRY(1,icFieldName,".")
         cFld       = ENTRY(2,icFieldName,".").
ELSE cFld = icFieldName.         

CREATE QUERY hQuery.

DO ix = 1 TO NUM-ENTRIES(icBufferList):
  CREATE BUFFER hBuffer[ix] FOR TABLE ENTRY(1,ENTRY(ix,icBufferList),";") NO-ERROR.
  IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO:
    ocReturn = "Invalid buffer definition: " + ENTRY(1,ENTRY(ix,icBufferList),";") + CHR(10) + PROGRAM-NAME(1).
    RETURN.
  END.
  IF (cFldBuffer NE "" AND cFldBuffer = hBuffer[ix]:NAME) OR (cFldBuffer = "" AND ix = 1) THEN DO:
    hFld = hBuffer[ix]:BUFFER-FIELD(cFld) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      ocReturn = "Invalid field name " + cFld + " for buffer " + hBuffer[ix]:NAME.
      RETURN.
    END.  
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
  IF NOT (hFld:DATA-TYPE BEGINS "INT" OR hFld:DATA-TYPE BEGINS "DEC") THEN DO: 
    ofTotal = ofTotal + DECIMAL(hFld:BUFFER-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      ocReturn = "Field " + icFieldName + " cannot be used in accumulation".
      RETURN.
    END.  
  END.  
  ELSE ofTotal = ofTotal + hFld:BUFFER-VALUE.
  
  hQuery:GET-NEXT().
END.

FINALLY:
  DO ix = 1 TO 100:
    IF VALID-HANDLE(hBuffer[ix]) THEN
      DELETE OBJECT hBuffer[ix].
    ELSE LEAVE.
  END.
  DELETE OBJECT hQuery.
  
  IF ocReturn NE "" THEN ocReturn = ocReturn + CHR(10) + PROGRAM-NAME(1).
END.

