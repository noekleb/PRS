/* Roll back a change LOG 
  Created 28.01.08 by brynjar@chemistry.no
-----------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hBuffer       AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iBuffCnt      AS INT    NO-UNDO.
DEF VAR iCurrBuffer   AS INT    NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR cBufferNames  AS CHAR   NO-UNDO.
DEF VAR cFindString   AS CHAR   NO-UNDO.
DEF VAR cField        AS CHAR   NO-UNDO.
DEF VAR cValue        AS CHAR   NO-UNDO.
DEF VAR hField        AS HANDLE NO-UNDO.
DEF VAR bOk           AS LOG    NO-UNDO.

UndoChangeLog:
DO TRANSACTION ON ERROR UNDO, LEAVE.

  bOk = ihBuffer:FIND-FIRST("WHERE true",NO-LOCK) NO-ERROR.
  IF NOT bOk THEN DO:
    ocReturn = "Invalid changelog header".
    RETURN.
  END.

  FIND FIRST JBoxChangeLogHeader EXCLUSIVE-LOCK 
       WHERE JBoxChangeLogHeader.iJBoxChangeLogHeaderId = INT(ihBuffer:BUFFER-FIELD("iJBoxChangeLogHeaderId"):BUFFER-VALUE)
       NO-WAIT NO-ERROR.
  IF NOT AVAIL JBoxChangeLogHeader THEN DO:
    ocReturn = "Changelog not available".
    RETURN.
  END.
  IF JBoxChangeLogHeader.dUndoDate NE ? THEN DO:
    ocReturn = "Changes are already undone".
    RETURN.
  END.

  ASSIGN JBoxChangeLogHeader.dUndoDate   = TODAY
         JBoxChangeLogHeader.iUndoTime   = TIME
         JBoxChangeLogHeader.dModified   = TODAY
         JBoxChangeLogHeader.cModifiedBy = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE).

  FOR EACH JBoxChangeLog NO-LOCK
      OF JBoxChangeLogHeader
      :
    IF NOT CAN-DO(cBufferNames,JBoxChangeLog.cTableChanged) THEN DO:
      ASSIGN iBuffCnt = iBuffCnt + 1
             cBufferNames = cBufferNames + (IF cBufferNames NE "" THEN "," ELSE "") + JBoxChangeLog.cTableChanged.
      CREATE BUFFER hBuffer[iBuffCnt] FOR TABLE JBoxChangeLog.cTableChanged.
      iCurrBuffer = iBuffCnt.
    END.
    ELSE iCurrBuffer = LOOKUP(JBoxChangeLog.cTableChanged,cBufferNames).

   cFindString = "WHERE ".

    DO ix = 1 TO NUM-ENTRIES(JBoxChangeLog.cEntityIdFields):
      ASSIGN cField = ENTRY(ix,JBoxChangeLog.cEntityIdFields)
             cValue = ENTRY(ix,JBoxChangeLog.cEntityIdValues,"|")
             .
      hField = hBuffer[iCurrBuffer]:BUFFER-FIELD(cField) NO-ERROR.
      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
        ocReturn = "Field " + cField + " does not exist in table " + JBoxChangeLog.cTableChanged.
        UNDO, LEAVE UndoChangeLog.
      END.
      CASE hField:DATA-TYPE:
           WHEN "character" THEN cFindString = cFindString + cField + " = '" + cValue + "' AND ".
           WHEN "date"      THEN cFindString = cFindString + cField + " = DATE('" + cValue + "') AND ".
           WHEN "decimal"   THEN cFindString = cFindString + cField + " = DECIMAL('" + cValue + "') AND ".
           WHEN "integer"   THEN cFindString = cFindString + cField + " = INTEGER('" + cValue + "') AND ".
           WHEN "logical"   THEN cFindString = cFindString + cField + " = LOGICAL('" + cValue + "') AND ".
      END CASE.
    END.

    cFindString = cFindString + "true".

    bOk = hBuffer[iCurrBuffer]:FIND-FIRST(cFindString,EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
    IF hBuffer[iCurrBuffer]:LOCKED THEN DO:
      ocReturn = "Record " + cFindString + " in table " + JBoxChangeLog.cTableChanged + " is locked".
      UNDO, LEAVE UndoChangeLog.
    END.
    IF bOk THEN DO ix = 1 TO NUM-ENTRIES(JBoxChangeLog.cFieldsChanged):
      hField = hBuffer[iCurrBuffer]:BUFFER-FIELD(ENTRY(ix,JBoxChangeLog.cFieldsChanged)) NO-ERROR.
      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
        ocReturn = "Field " + ENTRY(ix,JBoxChangeLog.cFieldsChanged) + " does not exist in table " + JBoxChangeLog.cTableChanged.
        UNDO, LEAVE UndoChangeLog.
      END.
      CASE hField:DATA-TYPE:
           WHEN "character" THEN hField:BUFFER-VALUE = ENTRY(ix,JBoxChangeLog.cFromValues,"|").
           WHEN "date"      THEN hField:BUFFER-VALUE = DATE(ENTRY(ix,JBoxChangeLog.cFromValues,"|")).
           WHEN "decimal"   THEN hField:BUFFER-VALUE = DECIMAL(ENTRY(ix,JBoxChangeLog.cFromValues,"|")).
           WHEN "integer"   THEN hField:BUFFER-VALUE = INTEGER(ENTRY(ix,JBoxChangeLog.cFromValues,"|")).
           WHEN "logical"   THEN hField:BUFFER-VALUE = LOGICAL(ENTRY(ix,JBoxChangeLog.cFromValues,"|")).
      END CASE.
    END.
  END.
END.

DO ix = 1 TO 20:
  IF VALID-HANDLE(hBuffer[ix]) THEN DELETE OBJECT hBuffer[ix] NO-ERROR.
END.

obOK = ocReturn = "".
