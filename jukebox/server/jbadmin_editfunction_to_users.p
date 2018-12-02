/* Update user to function link
   Parameters:  <functionid>|<useridlist> 
   
   Created: 02.05.07 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT   NO-UNDO.
DEF VAR iFunctionId  AS INT   NO-UNDO.
DEF VAR cUserIdList  AS CHAR  NO-UNDO.

ASSIGN iFunctionId = INT(ENTRY(1,icParam,";"))
       cUserIdList = ENTRY(2,icParam,";").

DO TRANSACTION:
  FOR EACH JBoxFunctionAccess EXCLUSIVE-LOCK
      WHERE JBoxFunctionAccess.iJBoxFunctionId = iFunctionId:
    IF JBoxFunctionAccess.cJBoxUserId NE "" AND
       LOOKUP(STRING(JBoxFunctionAccess.cJBoxUserId),cUserIdList,"|") = 0 THEN
      DELETE JBoxFunctionAccess.
  END.
  DO ix = 1 TO NUM-ENTRIES(cUserIdList,"|"):
    FIND JBoxFunctionAccess 
         WHERE JBoxFunctionAccess.iJBoxFunctionId  = iFunctionId
           AND JBoxFunctionAccess.cJBoxUserId      = ENTRY(ix,cUserIdList,"|")
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxFunctionAccess THEN DO:
      CREATE JBoxFunctionAccess.
      ASSIGN JBoxFunctionAccess.iJBoxFunctionId  = iFunctionId
             JBoxFunctionAccess.cJBoxUserId      = ENTRY(ix,cUserIdList,"|")
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

