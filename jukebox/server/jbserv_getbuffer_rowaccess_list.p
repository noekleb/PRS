/* Returns list of buffers with row-access restrictions for query.
   Primary usage: ProcessQuery procedure in jbserv_gettemptablejoin.p
   Created: 11.02.13 by Brynjar@chemistry.no
---------------------------------------------------------------*/
DEF INPUT  PARAM icBufferList       AS CHAR   NO-UNDO.
DEF INPUT  PARAM icUserId           AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocBufferAccessList AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocBufferExprList   AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocBufferKeyFldList AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ohBufferAccess     AS HANDLE NO-UNDO.

DEF VAR ix               AS INT    NO-UNDO.
DEF VAR iy               AS INT    NO-UNDO.
DEF VAR bOk              AS LOG    NO-UNDO.
DEF VAR hRowAccessTable  AS HANDLE NO-UNDO.
DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR hBuffUser        AS HANDLE NO-UNDO.  
DEF VAR hBuffUserGroup   AS HANDLE NO-UNDO.
DEF VAR cBuffer          AS CHAR   NO-UNDO.
DEF VAR cKeyFlds         AS CHAR   NO-UNDO.
DEF VAR cFindString      AS CHAR   NO-UNDO.
DEF VAR bAsLibStarted    AS LOG    NO-UNDO.

CREATE BUFFER hRowAccessTable FOR TABLE "JBoxRowAccessTable" NO-ERROR.
CREATE BUFFER ohBufferAccess  FOR TABLE "JBoxRowAccess" NO-ERROR.

IF NOT VALID-HANDLE(hRowAccessTable) OR NOT VALID-HANDLE(ohBufferAccess) THEN DO:
  DELETE OBJECT hRowAccessTable NO-ERROR. 
  DELETE OBJECT ohBufferAccess NO-ERROR.
  RETURN.
END.

CREATE BUFFER hBuffUser       FOR TABLE "JBoxUser" NO-ERROR.
IF NOT VALID-HANDLE(hBuffUser) THEN RETURN.

bOk = hBuffUser:FIND-FIRST("WHERE cJBoxUserId = '" + icUserId + "'") NO-ERROR.
IF NOT bOk THEN RETURN.

CREATE BUFFER hBuffUserGroup  FOR TABLE "JBoxUserGroupMembers" NO-ERROR.

DO ix = 1 TO NUM-ENTRIES(icBufferList):
  ASSIGN cBuffer     = ENTRY(ix,icBufferList)
         cFindString = ""
         .
  IF cBuffer BEGINS "buf" AND INDEX(cBuffer,"_") = 5 THEN cBuffer = SUBSTR(cBuffer,6).

  bOk = hRowAccessTable:FIND-FIRST("WHERE cTable = '" + cBuffer + "'") NO-ERROR.
  IF bOk THEN DO:
    IF NOT bAsLibStarted THEN bAsLibStarted = DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).

    IF hRowAccessTable::cExpression NE "" THEN DO:
      cFindString = hRowAccessTable::cExpression.
      IF NOT TRIM(cFindString) BEGINS "WHERE " THEN cFindString = "WHERE " + cFindString.
    END.
    ELSE DO:
      CREATE BUFFER hBuffer FOR TABLE cBuffer NO-ERROR.
      IF VALID-HANDLE(hBuffer) THEN DO:
        cKeyFlds = DYNAMIC-FUNCTION("getPrimaryKeyFields",hBuffer).  
        IF cKeyFlds NE "" THEN DO:
          cFindString = "WHERE cKeyValue = '&1".
          DO iy = 2 TO NUM-ENTRIES(cKeyFlds):
            cFindString = cFindString + CHR(1) + "&" + STRING(iy).
          END.
          cFindString = cFindString + "'".
        END.
      END.
    END.
    IF cFindString NE "" THEN DO:
      ASSIGN ocBufferAccessList = ocBufferAccessList + (IF ocBufferAccessList NE "" THEN "," ELSE "") + ENTRY(ix,icBufferList)
             ocBufferExprList   = ocBufferExprList + (IF ocBufferExprList NE "" THEN ";" ELSE "") + cFindString
             ocBufferKeyFldList = ocBufferKeyFldList + (IF ocBufferKeyFldList NE "" THEN ";" ELSE "") + cKeyFlds
             .
    END.
  END.
END.
   
