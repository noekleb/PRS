/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : getClientSessionId.p                          ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */

DEFINE INPUT  PARAMETER ipcUserId    AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER ipcPassword  AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER opcSessionID AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER oplStatus    AS LOGICAL INIT FALSE  NO-UNDO. 


FIND FIRST  ClientUser WHERE 
            ClientUser.ClientUserId = ipcUserid AND 
            ClientUser.ClientUserPassword = ENCODE(ipcPassword)  AND 
            ClientUser.ClientUserActive NO-LOCK NO-ERROR. 

IF AVAIL ClientUser THEN
DO:
      FIND FIRST ClientSession WHERE 
                 ClientSession.ClientUserUID = ClientUser.ClientUserUID  
                 NO-ERROR. 

      IF NOT AVAIL ClientSession THEN 
      DO:
          CREATE ClientSession. 
          ClientSession.ClientSessionUID = GUID(GENERATE-UUID).
          ClientSession.ClientUserUID = ClientUser.ClientUserUID. 
      END. 

      ASSIGN 
          ClientSession.SessionID = GUID(GENERATE-UUID)
          ClientSession.ClientSessionDateTime = NOW 
          ClientSession.iJBoxCompanyId = ClientUser.iJBoxCompanyId
          opcSessionID =  ClientSession.SessionID
          oplStatus = TRUE .
END.
 

