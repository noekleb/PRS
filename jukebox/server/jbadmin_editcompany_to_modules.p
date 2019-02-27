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
DEF VAR cCompanyList  AS CHAR   NO-UNDO.

ASSIGN iModuleId    = INTEGER(ENTRY(1,icParam,";"))
       cCompanyList = ENTRY(2,icParam,";")
       .

DO TRANSACTION:
  FOR EACH JBoxAppModuleCompany EXCLUSIVE-LOCK
      WHERE JBoxAppModuleCompany.iJBoxAppModuleId = iModuleId:
    IF LOOKUP(STRING(JBoxAppModuleCompany.iJBoxCompanyId),cCompanyList,"|") = 0 THEN
      DELETE JBoxAppModuleCompany.
  END.
  DO ix = 1 TO NUM-ENTRIES(cCompanyList,"|"):
    FIND JBoxAppModuleCompany 
         WHERE JBoxAppModuleCompany.iJBoxAppModuleId = iModuleId
           AND JBoxAppModuleCompany.iJBoxCompanyId   = INT(ENTRY(ix,cCompanyList,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxAppModuleCompany THEN DO:
      CREATE JBoxAppModuleCompany.
      ASSIGN JBoxAppModuleCompany.iJBoxAppModuleId = iModuleId
             JBoxAppModuleCompany.iJBoxCompanyId   = INT(ENTRY(ix,cCompanyList,"|"))
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

