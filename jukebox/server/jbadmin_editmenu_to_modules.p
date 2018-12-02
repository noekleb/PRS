/* Update module-company link
   Parameters:  <module id>|<companylist> 
      
            + <ROWID>|<List of rowid's>
         OR + <List of primary keys >
         
         OR temp-table containing CustNum 
   
   Created: 18.01.11 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT    NO-UNDO.
DEF VAR iModuleId     AS INT    NO-UNDO.
DEF VAR cSubmenuList  AS CHAR   NO-UNDO.
DEF VAR cUserId       AS CHAR   NO-UNDO.

ASSIGN iModuleId    = INTEGER(ENTRY(1,icParam,";"))
       cSubmenuList = ENTRY(2,icParam,";")
       cUserId      = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       .

DO TRANSACTION:
  FOR EACH JBoxAppModuleItem EXCLUSIVE-LOCK
      WHERE JBoxAppModuleItem.iJBoxAppModuleId = iModuleId
        AND JBoxAppModuleItem.iJBoxMenuId > 0
        AND JBoxAppModuleItem.iJBoxCompanyId = 0
      :
    IF LOOKUP(STRING(JBoxAppModuleItem.iJBoxMenuId),cSubmenuList,"|") = 0 THEN
      DELETE JBoxAppModuleItem.
  END.
  DO ix = 1 TO NUM-ENTRIES(cSubmenuList,"|"):
    FIND JBoxAppModuleItem 
         WHERE JBoxAppModuleItem.iJBoxAppModuleId = iModuleId
           AND JBoxAppModuleItem.iJBoxMenuId      = INT(ENTRY(ix,cSubmenuList,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxAppModuleItem THEN DO:
      CREATE JBoxAppModuleItem.
      ASSIGN JBoxAppModuleItem.iJBoxAppModuleId = iModuleId
             JBoxAppModuleItem.iJBoxMenuId      = INT(ENTRY(ix,cSubmenuList,"|"))
             JBoxAppModuleItem.dCreated         = TODAY
             JBoxAppModuleItem.cCreatedBy       = cUserId
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

