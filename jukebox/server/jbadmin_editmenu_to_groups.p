/* Update usergroup to menu link
   Parameters:  <menuid>|<grouplist> 
      
   Created:  01.05.08 by Brynjar Hasle                  
   Modified: 26.06.13 by Brynjar
             Support of parameter value pairs like <id|write access|id|write access|id..>            
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT   NO-UNDO.
DEF VAR iMenuId      AS INT   NO-UNDO.
DEF VAR cGroupIdList AS CHAR  NO-UNDO.
DEF VAR cUserId      AS CHAR  NO-UNDO.
DEF VAR hWriteAccess AS HANDLE NO-UNDO.

ASSIGN iMenuId      = INT(ENTRY(1,icParam,";"))
       cGroupIdList = ENTRY(2,icParam,";")
       cUserId      = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE)
       .

hWriteAccess = BUFFER JBoxUserMenu:BUFFER-FIELD("bWriteAccess") NO-ERROR.

DO TRANSACTION:
  FOR EACH JBoxUserMenu EXCLUSIVE-LOCK
      WHERE JBoxUserMenu.iJBoxMenuId = iMenuId:
    IF JBoxUserMenu.iJboxUserGroupId NE 0 AND
       LOOKUP(STRING(JBoxUserMenu.iJBoxUserGroupId),cGroupIdList,"|") = 0 THEN
      DELETE JBoxUserMenu.
  END.
  IF VALID-HANDLE(hWriteAccess) THEN
    DO ix = 1 TO NUM-ENTRIES(cGroupIdList,"|") BY 2:
      FIND JBoxUserMenu 
           WHERE JBoxUserMenu.iJBoxMenuId      = iMenuId
             AND JBoxUserMenu.iJBoxUserGroupId = INT(ENTRY(ix,cGroupIdList,"|"))
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL JBoxUserMenu THEN DO:
        CREATE JBoxUserMenu.
        ASSIGN JBoxUserMenu.iJBoxMenuId        = iMenuId
               JBoxUserMenu.iJBoxUserGroupId   = INT(ENTRY(ix,cGroupIdList,"|"))
               JBoxUserMenu.dCreated           = TODAY
               JBoxUserMenu.cCreatedBy         = cUserId
               .
      END.
      hWriteAccess:BUFFER-VALUE = LOGICAL(ENTRY(ix + 1,cGroupIdList,"|")).
    END.
  ELSE
    DO ix = 1 TO NUM-ENTRIES(cGroupIdList,"|"):
      FIND JBoxUserMenu 
           WHERE JBoxUserMenu.iJBoxMenuId      = iMenuId
             AND JBoxUserMenu.iJBoxUserGroupId = INT(ENTRY(ix,cGroupIdList,"|"))
           NO-LOCK NO-ERROR.
      IF NOT AVAIL JBoxUserMenu THEN DO:
        CREATE JBoxUserMenu.
        ASSIGN JBoxUserMenu.iJBoxMenuId        = iMenuId
               JBoxUserMenu.iJBoxUserGroupId   = INT(ENTRY(ix,cGroupIdList,"|"))
               JBoxUserMenu.dCreated           = TODAY
               JBoxUserMenu.cCreatedBy         = cUserId
               .
      END.
    END.
END.

IF ocReturn = "" THEN obOk = TRUE.

