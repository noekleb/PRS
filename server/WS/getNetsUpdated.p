/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : getNetsUpdated.p                              ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-15          
------------------------------------------------------------------------- */


DEFINE TEMP-TABLE tt_Updated NO-UNDO
    FIELD transactionid AS CHAR
    FIELD sendtDato AS DATE 
    FIELD sendt AS LOGICAL 
    FIELD sendtTid AS INT. 
                                 
DEFINE INPUT  PARAMETER LoginSessionId AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER CompanyId AS INT NO-UNDO. 
DEFINE INPUT  PARAMETER LastSendtDato AS DATE  INIT ? NO-UNDO. 
DEFINE INPUT  PARAMETER LASTSendtTid  AS INT INIT ? NO-UNDO. 
DEFINE OUTPUT PARAMETER TABLE FOR tt_Updated.
DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL INIT FALSE NO-UNDO. 

DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE iCnt AS INT NO-UNDO. 

RUN checkSessionId.p (LoginSessionId,Companyid, OUTPUT loK). 

LOG-MANAGER:WRITE-MESSAGE("SessionId:" + QUOTER(LoginSessionId) + " Status:" + QUOTER(loK),"getNetsUpdated").
IF NOT lok THEN RETURN. 


LOG-MANAGER:WRITE-MESSAGE("Reading updates from SendtDato:" + QUOTER(LastSendtDato) + QUOTER(LastSendtTid) + " Status:" + QUOTER(AVAIL nets),"getNetsUpdated").

FOR EACH nets WHERE nets.iJBoxCompanyId = companyid  AND 
         nets.sendt     AND 
         nets.SendtDato GE LastSendtDato AND 
         nets.SendtTid  GE LastSendtTid  NO-LOCK : 

    CREATE tt_Updated. 
    ASSIGN 
        tt_Updated.sendtDato     = nets.sendtDato
        tt_Updated.sendtTid      = nets.sendtTid 
        tt_Updated.sendt         = nets.sendt
        tt_Updated.transactionid = nets.transactionid. 
    
    iCnt = iCnt + 1. 
END.

RequestStatus = TRUE. 
LOG-MANAGER:WRITE-MESSAGE("Num records:" + QUOTER(iCnt) + " Status:" + QUOTER(RequestStatus),"getNetsUpdated").
