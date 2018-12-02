/* Check for index usage. 
   Created: 22.01.07 By brynjar@chemistry.no
----------------------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBufferList       AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryString      AS CHAR NO-UNDO.
DEF OUTPUT PARAM obIndex            AS LOG  NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

DEF VAR hBuffer      AS HANDLE NO-UNDO EXTENT 10.
DEF VAR bOk          AS LOG    NO-UNDO INIT YES.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.

{incl/validatesession.i}

icQueryString = TRIM(icQueryString).

CREATE QUERY hQuery.
DO ix = 1 TO NUM-ENTRIES(icBufferList):
  CREATE BUFFER hBuffer[ix] FOR TABLE ENTRY(ix,icBufferList) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN ocReturn = "Invalid buffer reference: " + ENTRY(ix,icBufferList) + CHR(10) + PROGRAM-NAME(1)
           obIndex  = ?.
    RETURN.
  END.
  hQuery:ADD-BUFFER(hBuffer[ix]). 
END.

IF NOT icQueryString BEGINS "FOR EACH " THEN DO:
  IF ENTRY(1,icQueryString," ") NE ENTRY(1,icBufferList) THEN
    icQueryString = "FOR EACH " + ENTRY(1,icBufferList) + " " + icQueryString.
  ELSE
    icQueryString = "FOR EACH " + icQueryString.
END.

hQuery:QUERY-PREPARE(icQueryString) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ASSIGN ocReturn = "Invalid query string: " + ENTRY(ix,icBufferList) + CHR(10) + PROGRAM-NAME(1)
         obIndex  = ?.
  RETURN.
END.

obIndex = NOT hQuery:INDEX-INFORMATION(1) MATCHES "*whole-index*".

DELETE OBJECT hQuery.
DO ix = 1 TO NUM-ENTRIES(icBufferList):
  DELETE OBJECT hBuffer[ix].
END.
