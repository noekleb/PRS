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
DEF VAR iProgramId   AS INT    NO-UNDO.
DEF VAR cModuleList  AS CHAR   NO-UNDO.
DEF VAR cUserId      AS CHAR   NO-UNDO.

ASSIGN iProgramId  = INTEGER(ENTRY(1,icParam,";"))
       cModuleList = ENTRY(2,icParam,";")
       cUserId     = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       .

DO TRANSACTION:
  FOR EACH JBoxAppModuleItem EXCLUSIVE-LOCK
      WHERE JBoxAppModuleItem.iJBoxAppProgramId = iProgramId
        AND JBoxAppModuleItem.iJBoxCompanyId = 0
      :
    IF LOOKUP(STRING(JBoxAppModuleItem.iJBoxAppModuleId),cModuleList,"|") = 0 THEN
      DELETE JBoxAppModuleItem.
  END.
  DO ix = 1 TO NUM-ENTRIES(cModuleList,"|"):
    FIND JBoxAppModuleItem 
         WHERE JBoxAppModuleItem.iJBoxAppProgramId = iProgramId
           AND JBoxAppModuleItem.iJBoxAppModuleId  = INT(ENTRY(ix,cModuleList,"|"))
           AND JBoxAppModuleItem.iJBoxCompanyId    = 0
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxAppModuleItem THEN DO:
      CREATE JBoxAppModuleItem.
      ASSIGN JBoxAppModuleItem.iJBoxAppProgramId = iProgramId
             JBoxAppModuleItem.iJBoxAppModuleId  = INT(ENTRY(ix,cModuleList,"|"))
             JBoxAppModuleItem.dCreated          = TODAY
             JBoxAppModuleItem.cCreatedBy        = cUserId
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

