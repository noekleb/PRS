/* Update user-group link
   Parameters:  <userid>|<grouplist> 
      
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
  FOR EACH JBoxUserGroupMembers EXCLUSIVE-LOCK
      WHERE JBoxUserGroupMembers.cJBoxUserId = cUserId:
    IF LOOKUP(STRING(JBoxUserGroupMembers.iJBoxUserGroupId),icParam,"|") = 0 THEN
      DELETE JBoxUserGroupMembers.
  END.
  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    FIND JBoxUserGroupMembers 
         WHERE JBoxUserGroupMembers.cJBoxUserId      = cUserId
           AND JBoxUserGroupMembers.iJBoxUserGroupId = INT(ENTRY(ix,icParam,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxUserGroupMembers THEN DO:
      CREATE JBoxUserGroupMembers.
      ASSIGN JBoxUserGroupMembers.cJBoxUserId        = cUserId
             JBoxUserGroupMembers.iJBoxUserGroupId   = INT(ENTRY(ix,icParam,"|"))
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

