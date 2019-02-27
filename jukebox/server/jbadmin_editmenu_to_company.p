/* Update company to menu link
   Parameters:  <menuid>|<companyidlist> 
      
   Created:  01.05.08 by Brynjar Hasle     
   Modified: 26.06.13 by Brynjar
             Support of parameter value pairs like <id|write access|id|write access|id..>            
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR iMenuId         AS INT    NO-UNDO.
DEF VAR cCompanyIdList  AS CHAR   NO-UNDO.
DEF VAR cUserId         AS CHAR   NO-UNDO.
DEF VAR hWriteAccess    AS HANDLE NO-UNDO.

ASSIGN iMenuId         = INT(ENTRY(1,icParam,";"))
       cCompanyIdList  = ENTRY(2,icParam,";")
       cUserId         = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE)
       .
       
hWriteAccess = BUFFER JBoxCompanyMenu:BUFFER-FIELD("bWriteAccess") NO-ERROR.

DO TRANSACTION:
  FOR EACH JBoxCompanyMenu EXCLUSIVE-LOCK
      WHERE JBoxCompanyMenu.iJBoxMenuId = iMenuId:
    IF LOOKUP(STRING(JBoxCompanyMenu.iJBoxCompanyId),cCompanyIdList,"|") = 0 THEN
      DELETE JBoxCompanyMenu.
  END.
  IF VALID-HANDLE(hWriteAccess) THEN
    DO ix = 1 TO NUM-ENTRIES(cCompanyIdList,"|") BY 2:
      FIND JBoxCompanyMenu 
           WHERE JBoxCompanyMenu.iJBoxMenuId    = iMenuId
             AND JBoxCompanyMenu.iJBoxCompanyId = INT(ENTRY(ix,cCompanyIdList,"|"))
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL JBoxCompanyMenu THEN DO:
        CREATE JBoxCompanyMenu.
        ASSIGN JBoxCompanyMenu.iJBoxMenuId      = iMenuId
               JBoxCompanyMenu.iJBoxCompanyId   = INT(ENTRY(ix,cCompanyIdList,"|"))
               JBoxCompanyMenu.dCreated         = TODAY
               JBoxCompanyMenu.cCreatedBy       = cUserId
               .
      END.
      hWriteAccess:BUFFER-VALUE = LOGICAL(ENTRY(ix + 1,cCompanyIdList,"|")).
    END.
  ELSE
    DO ix = 1 TO NUM-ENTRIES(cCompanyIdList,"|"):
      FIND JBoxCompanyMenu 
           WHERE JBoxCompanyMenu.iJBoxMenuId    = iMenuId
             AND JBoxCompanyMenu.iJBoxCompanyId = INT(ENTRY(ix,cCompanyIdList,"|"))
           NO-LOCK NO-ERROR.
      IF NOT AVAIL JBoxCompanyMenu THEN DO:
        CREATE JBoxCompanyMenu.
        ASSIGN JBoxCompanyMenu.iJBoxMenuId      = iMenuId
               JBoxCompanyMenu.iJBoxCompanyId   = INT(ENTRY(ix,cCompanyIdList,"|"))
               JBoxCompanyMenu.dCreated         = TODAY
               JBoxCompanyMenu.cCreatedBy       = cUserId
               .
      END.
    END.
END.

IF ocReturn = "" THEN obOk = TRUE.

