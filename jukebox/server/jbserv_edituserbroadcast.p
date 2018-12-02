/* Update user-company link
   Parameters:  <broadcastmessageid>|<userlist> 
         
   Created: 30.01.06 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix               AS INT  NO-UNDO.
DEF VAR iJBoxBCmessageId AS INT  NO-UNDO.
DEF VAR cUserIdList      AS CHAR NO-UNDO.

ASSIGN iJBoxBCmessageId = INT(ENTRY(1,icParam,";"))
       cUserIdList      = ENTRY(2,icParam,";").


DO TRANSACTION:
  FOR EACH JBoxUserBroadcast EXCLUSIVE-LOCK
      WHERE JBoxUserBroadcast.iJBoxBroadCastMessageId = iJBoxBCmessageId:
    IF LOOKUP(JBoxUserBroadcast.cJBoxUserId,cUserIdList,"|") = 0 THEN
      DELETE JBoxUserBroadcast.
  END.
  DO ix = 1 TO NUM-ENTRIES(cUserIdList,"|"):
    FIND JBoxUserBroadcast 
         WHERE JBoxUserBroadcast.iJBoxBroadCastMessageId = iJBoxBCmessageId
           AND JBoxUserBroadcast.cJBoxUserId             = ENTRY(ix,cUserIdList,"|")
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxUserBroadcast THEN DO:
      CREATE JBoxUserBroadcast.
      ASSIGN JBoxUserBroadcast.iJBoxBroadCastMessageId = iJBoxBCmessageId
             JBoxUserBroadcast.cJBoxUserId             = ENTRY(ix,cUserIdList,"|")
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

