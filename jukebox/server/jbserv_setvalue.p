/* Generic procedure to assign a column value.
   Typical usage is for ProcessSelectedRows or ProcessQuery:
   
   RUN JBoxDSimpleSelectList.w (cAktIdList,?,OUTPUT cReturn).
   IF cReturn NE ? THEN DO:
     IF NOT DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"jbserv_setvalue.p","iProsjektAktId|" + cReturn) THEN
       DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
   
   Parameters: <field name>|<value>
   
   Created: 14.03.12 by brynjar@chemistry.no
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR cField          AS CHAR   NO-UNDO.
DEF VAR cValue          AS CHAR   NO-UNDO.
DEF VAR cTable          AS CHAR   NO-UNDO.
DEF VAR cRowIdField     AS CHAR   NO-UNDO.
DEF VAR hRowIdField     AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hDbBuffer       AS HANDLE NO-UNDO.
DEF VAR hField          AS HANDLE NO-UNDO.
DEF VAR cUserId         AS CHAR   NO-UNDO. 
DEF VAR hFldDMod        AS HANDLE NO-UNDO.
DEF VAR hFldModBy       AS HANDLE NO-UNDO.
def var bOk             as logical no-undo.

ASSIGN cUserId   = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       cField    = ENTRY(1,icParam,"|")
       cValue    = ENTRY(2,icParam,"|")
       NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Error in parameter: " + icParam + " for " + PROGRAM-NAME(1).
  RETURN.
END.
IF NUM-ENTRIES(icParam,"|") > 3 THEN
  ASSIGN cTable      = ENTRY(3,icParam,"|")
         cRowIdField = ENTRY(4,icParam,"|")
         .
ELSE IF NUM-ENTRIES(icParam,"|") = 2 THEN
  ASSIGN cTable      = ihBuffer:NAME
         cRowIdField = "RowIdent1"
         .
ELSE DO:
  ocReturn = "Error in parameter: " + icParam + " for " + PROGRAM-NAME(1).
  RETURN.
END.

CREATE BUFFER hDbBuffer FOR TABLE cTable NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Invalid buffer name: " + cTable + ". Program " + PROGRAM-NAME(1).
  RETURN.
END.

hField = hDbBuffer:BUFFER-FIELD(cField) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Invalid field name: " + cField + " for buffer " + cTable + ". Program " + PROGRAM-NAME(1).
  RETURN.
END.

hRowIdField = ihBuffer:BUFFER-FIELD(cRowIdField) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Invalid field name for rowid: " + cRowIdField + " for buffer " + ihBuffer:NAME + ". Program " + PROGRAM-NAME(1).
  RETURN.
END.

hFldDMod = hDbBuffer:BUFFER-FIELD("dModified") NO-ERROR.
hFldModBy = hDbBuffer:BUFFER-FIELD("cModifiedBy") NO-ERROR.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
DO TRANSACTION ON ERROR UNDO,LEAVE:
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    bOk = hDbBuffer:FIND-FIRST("WHERE ROWID(" + cTable + ") = TO-ROWID('" + hRowIdField:BUFFER-VALUE + "')",EXCLUSIVE-LOCK) NO-ERROR.
    IF hDbBuffer:AVAIL THEN DO:
      RUN jbserv_assignstringvalue.p(hField,0,cValue,OUTPUT bOk).
      IF not bOk THEN DO:
        ocReturn = "Could not assign value " + cValue + " for field " + cField.
        UNDO, LEAVE.
      END.
      ELSE DO:
        IF VALID-HANDLE(hFldDMod)  THEN hFldDMod:BUFFER-VALUE  = TODAY.
        IF VALID-HANDLE(hFldModBy) THEN hFldModBy:BUFFER-VALUE = cUserId.
      END. 
    END.
    ELSE DO:
      ocReturn = "Could not find record / record not available for update".
      UNDO, LEAVE.
    END.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.
DELETE OBJECT hDbBuffer.

obOK = ocReturn = "".
