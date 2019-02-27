/* Update module-company link
   Parameters:  <module id>|<companylist> 
      
            + <ROWID>|<List of rowid's>
         OR + <List of primary keys >
         
         OR temp-table containing CustNum 
   
   Created: 19.01.11 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iModuleId    AS INT    NO-UNDO.
DEF VAR cProgramList AS CHAR   NO-UNDO.
DEF VAR cUserId      AS CHAR   NO-UNDO.

ASSIGN iModuleId    = INTEGER(ENTRY(1,icParam,";"))
       cProgramList = ENTRY(2,icParam,";")
       cUserId      = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       .

DO TRANSACTION:
  FOR EACH JBoxAppModuleItem EXCLUSIVE-LOCK
      WHERE JBoxAppModuleItem.iJBoxAppModuleId = iModuleId
        AND JBoxAppModuleItem.iJBoxCompanyId   = 0
      :
    IF LOOKUP(STRING(JBoxAppModuleItem.iJBoxAppProgramId),cProgramList,"|") = 0 THEN
      DELETE JBoxAppModuleItem.
  END.
  DO ix = 1 TO NUM-ENTRIES(cProgramList,"|"):
    FIND JBoxAppModuleItem 
         WHERE JBoxAppModuleItem.iJBoxAppModuleId  = iModuleId
           AND JBoxAppModuleItem.iJBoxAppProgramId = INT(ENTRY(ix,cProgramList,"|"))
           AND JBoxAppModuleItem.iJBoxCompanyId    = 0
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxAppModuleItem THEN DO:
      CREATE JBoxAppModuleItem.
      ASSIGN JBoxAppModuleItem.iJBoxAppModuleId  = iModuleId
             JBoxAppModuleItem.iJBoxAppProgramId = INT(ENTRY(ix,cProgramList,"|"))
             JBoxAppModuleItem.dCreated          = TODAY
             JBoxAppModuleItem.cCreatedBy        = cUserId
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

