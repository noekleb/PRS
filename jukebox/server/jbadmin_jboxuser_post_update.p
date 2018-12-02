/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap | hBrowse,"PostUpdateProc","<procedure>").
   If there's no fieldmap (viewer) set the attribute on the browse object
   
   NOTE: If the action is DELETE the buffer handle is for a temp-table copy of the record that was deleted
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Delete, Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cDbAccessList AS CHAR   NO-UNDO.
DEF VAR hUserBuff     AS HANDLE NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR bOK           AS LOG    NO-UNDO.
DEF VAR cPasswd       AS CHAR   NO-UNDO.

IF icAction = "update" THEN DO:
  cDbAccessList = DYNAMIC-FUNCTION("getInputParam" IN SOURCE-PROCEDURE).  

  DO ix = 1 TO NUM-ENTRIES(cDbAccessList):
    CREATE BUFFER hUserBuff FOR TABLE ENTRY(1,ENTRY(ix,cDbAccessList),";") + "._user".
    bOK = hUserBuff:FIND-FIRST("WHERE _userid = '" + ihBuffer:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "'",NO-LOCK) NO-ERROR.
    IF cPasswd = "" AND bOK THEN cPasswd = hUserBuff:BUFFER-FIELD("_Password"):BUFFER-VALUE.
    DELETE OBJECT hUserBuff.
  END.

  IF cPasswd NE "" THEN 
    DO ix = 1 TO NUM-ENTRIES(cDbAccessList):
      CREATE BUFFER hUserBuff FOR TABLE ENTRY(1,ENTRY(ix,cDbAccessList),";") + "._user".
      bOK = hUserBuff:FIND-FIRST("WHERE _userid = '" + ihBuffer:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "'",EXCLUSIVE-LOCK) NO-ERROR.
      IF bOk AND NOT LOGICAL(ENTRY(2,ENTRY(ix,cDbAccessList),";")) THEN
        hUserBuff:BUFFER-DELETE().
      ELSE IF NOT bOk AND LOGICAL(ENTRY(2,ENTRY(ix,cDbAccessList),";")) THEN DO:
        hUserBuff:BUFFER-CREATE().
        ASSIGN hUserBuff:BUFFER-FIELD("_Userid"):BUFFER-VALUE    = ihBuffer:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE
               hUserBuff:BUFFER-FIELD("_User-Name"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("cUserName"):BUFFER-VALUE
               hUserBuff:BUFFER-FIELD("_Password"):BUFFER-VALUE  = cPasswd
               .
      END.
      DELETE OBJECT hUserBuff.
    END.
END.
