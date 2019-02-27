/* Check for any new broadcasts to current user
   The link from message to user can either exist (message for specific user(s)) or,
   if the message is public - either within a company or for all - the link is created here.
   
   Parameters:  none 
         
   Created: 31.01.06 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


FIND FIRST JBoxLoginSession NO-LOCK
     WHERE JBoxLoginSession.cSessionId = icSessionId
     NO-ERROR.
IF AVAIL JBoxLoginSession THEN DO:
  FIND FIRST JBoxUserBroadcast NO-LOCK
       WHERE JBoxUserBroadcast.cJBoxUserId = JBoxLoginSession.cJBoxUserId
         AND NOT JBoxUserBroadcast.bReceived
       NO-ERROR.
  IF NOT AVAIL JBoxUserBroadcast THEN
    FIND FIRST JBoxUserBroadcast NO-LOCK
         WHERE JBoxUserBroadcast.cJBoxUserId = JBoxLoginSession.cJBoxUserId
           AND JBoxUserBroadcast.bReceived
           AND NOT JBoxUserBroadcast.bDoNotShowAgain
         NO-ERROR.
  IF AVAIL JBoxUserBroadcast THEN DO:
    FIND FIRST JBoxBroadCastMessage NO-LOCK OF JBoxUserBroadcast
         WHERE JBoxBroadCastMessage.dStartBroadCast     LE TODAY
           AND JBoxBroadCastMessage.iStartBroadCastTime LE TIME
           AND JBoxBroadCastMessage.dEndBroadCast       GE TODAY
           AND (IF JBoxBroadCastMessage.dEndBroadCast = TODAY THEN JBoxBroadCastMessage.iEndBroadCastTime GE TIME ELSE TRUE)
         NO-ERROR.
    IF AVAIL JBoxBroadCastMessage THEN DO:
      IF NOT JBoxUserBroadcast.bReceived THEN DO:
        FIND CURRENT JBoxUserBroadcast EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL JBoxUserBroadcast THEN
          ASSIGN JBoxUserBroadcast.bReceived = YES
                 JBoxUserBroadcast.dReceived = TODAY
                 JBoxUserBroadcast.iReceived = TIME.
      END.
      ocReturn = STRING(JBoxBroadCastMessage.iJBoxBroadCastMessageId).
      obOk = YES.
      RETURN.
    END.
  END.

  FIND FIRST JBoxBroadCastMessage NO-LOCK 
       WHERE JBoxBroadCastMessage.dStartBroadCast     LE TODAY
         AND JBoxBroadCastMessage.iStartBroadCastTime LE TIME
         AND JBoxBroadCastMessage.dEndBroadCast       GE TODAY
         AND (IF JBoxBroadCastMessage.dEndBroadCast = TODAY THEN JBoxBroadCastMessage.iEndBroadCastTime GE TIME ELSE TRUE)
         AND (JBoxBroadCastMessage.bAllUsers OR 
              JBoxBroadCastMessage.iJBoxCompanyId = JBoxLoginSession.iJBoxCompanyId)
       NO-ERROR.
  IF AVAIL JBoxBroadCastMessage THEN DO:
    FIND FIRST JBoxUserBroadcast NO-LOCK
               WHERE JBoxUserBroadcast.iJBoxBroadCastMessageId = JBoxBroadCastMessage.iJBoxBroadCastMessageId
                 AND JBoxUserBroadcast.cJBoxUserId = JBoxLoginSession.cJBoxUserId
         NO-ERROR.
    IF NOT AVAIL JBoxUserBroadcast THEN DO:
      CREATE JBoxUserBroadcast.
      ASSIGN JBoxUserBroadcast.iJBoxBroadCastMessageId = JBoxBroadCastMessage.iJBoxBroadCastMessageId
             JBoxUserBroadcast.cJBoxUserId             = JBoxLoginSession.cJBoxUserId
             JBoxUserBroadcast.bReceived               = YES
             JBoxUserBroadcast.dReceived               = TODAY
             JBoxUserBroadcast.iReceived               = TIME
             JBoxUserBroadcast.dCreated                = TODAY
             JBoxUserBroadcast.cCreatedBy              = JBoxLoginSession.cJBoxUserId
             ocReturn                                  = STRING(JBoxBroadCastMessage.iJBoxBroadCastMessageId).
             .
    END.
    ELSE IF NOT JBoxUserBroadcast.bDoNotShowAgain THEN
      ocReturn = STRING(JBoxBroadCastMessage.iJBoxBroadCastMessageId).
  END.

END.

obOk = ocReturn NE "".
