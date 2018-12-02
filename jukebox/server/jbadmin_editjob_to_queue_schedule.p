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

DEF VAR ix        AS INT    NO-UNDO.
DEF VAR iQueueId  AS INT    NO-UNDO.
DEF VAR cJobList  AS CHAR   NO-UNDO.
DEF VAR cUserId   AS CHAR   NO-UNDO.

ASSIGN iQueueId = INTEGER(ENTRY(1,icParam,";"))
       cJobList = ENTRY(2,icParam,";")
       cUserId  = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       .

DO TRANSACTION:
  FOR EACH JBoxJobSchedule EXCLUSIVE-LOCK
      WHERE JBoxJobSchedule.iJBoxJobQueueId = iQueueId
      :
    IF LOOKUP(STRING(JBoxJobSchedule.iJBoxJobDefinitionId),cJobList,"|") = 0 THEN
      DELETE JBoxJobSchedule.
  END.
  DO ix = 1 TO NUM-ENTRIES(cJobList,"|"):
    FIND JBoxJobSchedule 
         WHERE JBoxJobSchedule.iJBoxJobQueueId      = iQueueId
           AND JBoxJobSchedule.iJBoxJobDefinitionId = INT(ENTRY(ix,cJobList,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxJobSchedule THEN DO:
      CREATE JBoxJobSchedule.
      ASSIGN JBoxJobSchedule.iJBoxJobQueueId       = iQueueId
             JBoxJobSchedule.iJBoxJobDefinitionId  = INT(ENTRY(ix,cJobList,"|"))
             JBoxJobSchedule.dCreated              = TODAY
             JBoxJobSchedule.cCreatedBy           = cUserId
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

