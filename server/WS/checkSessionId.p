/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : checkSessionId.p                              ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-3            
------------------------------------------------------------------------- */

DEFINE INPUT  PARAMETER ipcSessionid AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER ipiCompanyId AS INT NO-UNDO. 
DEFINE OUTPUT PARAMETER oplOK AS LOGICAL INIT FALSE NO-UNDO. 

DEFINE VARIABLE timeout AS DATETIME NO-UNDO. 
DEFINE VARIABLE iTimeOut AS INTEGER NO-UNDO. 
timeout = NOW. 

FIND FIRST ClientSession WHERE 
           ClientSession.Sessionid = ipcSessionid AND 
           ClientSession.iJBoxCompanyId = ipiCompanyId NO-LOCK NO-ERROR. 

IF AVAIL ClientSession THEN 
DO:
     /* Timemout 12 min */ 
    iTimeOut =  TimeOut -  ClientSession.ClientSessionDateTime.
    IF NOT iTimeOut GE 1200000 THEN oplOk = TRUE. 
    ELSE 
    DO:
        FIND FIRST ClientSession WHERE 
                   ClientSession.Sessionid = ipcSessionid AND 
                   ClientSession.iJBoxCompanyId = ipiCompanyId 
                   EXCLUSIVE-LOCK NO-ERROR. 
        DELETE ClientSession. 
    END.
END. 
