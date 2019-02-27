/* Update module to menu link
   Parameters:  <menuid>|<moduleidlist> 
      
   Created: 20.01.11 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT   NO-UNDO.
DEF VAR iMenuId       AS INT   NO-UNDO.
DEF VAR cModuleIdList AS CHAR  NO-UNDO.
DEF VAR cUserId       AS CHAR  NO-UNDO.


ASSIGN iMenuId        = INT(ENTRY(1,icParam,";"))
       cModuleIdList = ENTRY(2,icParam,";")
       cUserId        = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE).

DO TRANSACTION:
  FOR EACH JBoxAppModuleItem EXCLUSIVE-LOCK
      WHERE JBoxAppModuleItem.iJBoxMenuId = iMenuId:
    IF LOOKUP(STRING(JBoxAppModuleItem.iJBoxAppModuleId),cModuleIdList,"|") = 0 THEN
      DELETE JBoxAppModuleItem.
  END.
  DO ix = 1 TO NUM-ENTRIES(cModuleIdList,"|"):
    FIND JBoxAppModuleItem 
         WHERE JBoxAppModuleItem.iJBoxMenuId    = iMenuId
           AND JBoxAppModuleItem.iJBoxAppModuleId = INT(ENTRY(ix,cModuleIdList,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxAppModuleItem THEN DO:
      CREATE JBoxAppModuleItem.
      ASSIGN JBoxAppModuleItem.iJBoxMenuId      = iMenuId
             JBoxAppModuleItem.iJBoxAppModuleId = INT(ENTRY(ix,cModuleIdList,"|"))
             JBoxAppModuleItem.dCreated         = TODAY
             JBoxAppModuleItem.cCreatedBy       = cUserId
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

