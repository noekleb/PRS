/* Update user to menu link
   Parameters:  <menuid>|<useridlist> 
   
   Created:  01.05.08 by Brynjar Hasle                  
   Modified: 26.06.13 by Brynjar
             Support of parameter value pairs like <userid|write access|userid|write access|userid..>            
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT   NO-UNDO.
DEF VAR iMenuId      AS INT   NO-UNDO.
DEF VAR cUserIdList  AS CHAR  NO-UNDO.
DEF VAR hWriteAccess AS HANDLE NO-UNDO.

ASSIGN iMenuId     = INT(ENTRY(1,icParam,";"))
       cUserIdList = ENTRY(2,icParam,";").

hWriteAccess = BUFFER JBoxUserMenu:BUFFER-FIELD("bWriteAccess") NO-ERROR.

DO TRANSACTION:
  FOR EACH JBoxUserMenu EXCLUSIVE-LOCK
      WHERE JBoxUserMenu.iJBoxMenuId = iMenuId:
    IF JBoxUserMenu.cJBoxUserId NE "" AND
       LOOKUP(STRING(JBoxUserMenu.cJBoxUserId),cUserIdList,"|") = 0 THEN
      DELETE JBoxUserMenu.
  END.
  IF VALID-HANDLE(hWriteAccess) THEN
    DO ix = 1 TO NUM-ENTRIES(cUserIdList,"|") BY 2:
      FIND JBoxUserMenu 
           WHERE JBoxUserMenu.iJBoxMenuId  = iMenuId
             AND JBoxUserMenu.cJBoxUserId      = ENTRY(ix,cUserIdList,"|")
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL JBoxUserMenu THEN DO:
        CREATE JBoxUserMenu.
        ASSIGN JBoxUserMenu.iJBoxMenuId  = iMenuId
               JBoxUserMenu.cJBoxUserId  = ENTRY(ix,cUserIdList,"|")
               .
      END.
      hWriteAccess:BUFFER-VALUE = LOGICAL(ENTRY(ix + 1,cUserIdList,"|")).
    END.
  ELSE
    DO ix = 1 TO NUM-ENTRIES(cUserIdList,"|"):
      FIND JBoxUserMenu 
           WHERE JBoxUserMenu.iJBoxMenuId  = iMenuId
             AND JBoxUserMenu.cJBoxUserId      = ENTRY(ix,cUserIdList,"|")
           NO-LOCK NO-ERROR.
      IF NOT AVAIL JBoxUserMenu THEN DO:
        CREATE JBoxUserMenu.
        ASSIGN JBoxUserMenu.iJBoxMenuId  = iMenuId
               JBoxUserMenu.cJBoxUserId  = ENTRY(ix,cUserIdList,"|")
               .
      END.
    END.
END.

IF ocReturn = "" THEN obOk = TRUE.

