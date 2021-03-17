/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : WSMainCardObj.p                               ---- */
/* ---- Description  :                                               ---- */
/* ----  Testing against web services from appserver                 ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-10            
------------------------------------------------------------------------- */


PROCEDURE WSLogin : 
    DEFINE INPUT PARAMETER LoginUserId AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER LoginPassword AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER LoginSessionId AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL NO-UNDO. 
    RUN getClientSessionId.p (LoginUserId, LoginPassword,OUTPUT LoginSessionId,OUTPUT RequestStatus).
END. 


PROCEDURE getNetsLastCreatedDateTime : 
    DEFINE INPUT  PARAMETER LoginSessionId AS CHAR NO-UNDO. 
    DEFINE INPUT  PARAMETER CompanyId AS INT NO-UNDO. 
    DEFINE OUTPUT PARAMETER LastRegistrertDato AS DATE  INIT ? NO-UNDO. 
    DEFINE OUTPUT PARAMETER LastRegistrertTid  AS INT INIT ? NO-UNDO. 
    DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL INIT FALSE NO-UNDO. 

    RUN getNetsLastCreatedDateTime.p (LoginSessionId, CompanyId,OUTPUT LastRegistrertDato,OUTPUT LastRegistrertTid,OUTPUT RequestStatus).
END. 


DEFINE TEMP-TABLE tt_Updated NO-UNDO
    FIELD transactionid AS CHAR
    FIELD sendtDato AS DATE 
    FIELD sendt AS LOGICAL 
    FIELD sendtTid AS INT. 

PROCEDURE getNetsUpdated : 
    DEFINE INPUT  PARAMETER LoginSessionId AS CHAR NO-UNDO. 
    DEFINE INPUT  PARAMETER CompanyId AS INT NO-UNDO. 
    DEFINE INPUT  PARAMETER LastSendtDato AS DATE  INIT ? NO-UNDO. 
    DEFINE INPUT  PARAMETER LASTSendtTid  AS INT INIT ? NO-UNDO. 
    DEFINE OUTPUT PARAMETER TABLE FOR tt_Updated.
    DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL INIT FALSE NO-UNDO. 

    RUN getNetsUpdated.p (LoginSessionId, CompanyId,INPUT LastSendtDato,INPUT LASTSendtTid, OUTPUT TABLE tt_Updated, OUTPUT RequestStatus).
END. 





