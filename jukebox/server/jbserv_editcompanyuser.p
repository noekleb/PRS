/* Update user-company link
   Parameters:  <userid>|<companylist> 
      
            + <ROWID>|<List of rowid's>
         OR + <List of primary keys >
         
         OR temp-table containing CustNum 
   
   Created: 21.10.04 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.

cUserId = ENTRY(1,icParam,"|").

DO TRANSACTION:
  FOR EACH JBoxCompanyUser EXCLUSIVE-LOCK
      WHERE JBoxCompanyUser.cJBoxUserId = cUserId:
    IF LOOKUP(STRING(JBoxCompanyUser.iJBoxCompanyId),icParam,"|") = 0 THEN
      DELETE JBoxCompanyUser.
  END.
  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    FIND JBoxCompanyUser 
         WHERE JBoxCompanyUser.cJBoxUserId = cUserId
           AND JBoxCompanyUser.iJBoxCompanyId  = INT(ENTRY(ix,icParam,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxCompanyUser THEN DO:
      CREATE JBoxCompanyUser.
      ASSIGN JBoxCompanyUser.cJBoxUserId    = cUserId
             JBoxCompanyUser.iJBoxCompanyId = INT(ENTRY(ix,icParam,"|"))
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

