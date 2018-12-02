/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : WSGetNetsData.p                               ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */


DEFINE INPUT PARAMETER LoginSessionId AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER CompanyId AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER TransactionID AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER NumRows AS INTEGER INIT 0 NO-UNDO.
DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL INIT FALSE NO-UNDO. 

DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 

    RUN checkSessionId.p (LoginSessionId,Companyid, OUTPUT loK). 
    
    LOG-MANAGER:WRITE-MESSAGE("SessionId:" + QUOTER(LoginSessionId) + " Status:" + QUOTER(loK),"WSGetNetsDataNumRows").
    IF NOT lok THEN RETURN. 

    FIND FIRST nets WHERE nets.transactionid  = Transactionid AND nets.iJBoxCompanyId = companyid NO-LOCK NO-ERROR. 

    LOG-MANAGER:WRITE-MESSAGE("TransactionId:" + QUOTER(Transactionid) + " Status:" + QUOTER(AVAIL nets),"WSGetNetsDataNumRows").

    IF NOT AVAIL nets THEN RETURN . 
    
    FIND FIRST bonghode WHERE bonghode.b_id = nets.b_id NO-LOCK NO-ERROR. 
    FOR EACH  bonglinje WHERE bonglinje.b_id = nets.b_id   AND  (bonglinje.ttid = 1   OR bonglinje.ttid = 10  OR  bonglinje.ttid = 3 ) 
          AND  bonglinje.makulert = false NO-LOCK :
          NumRows = NumRows + 1. 
    END.

    RequestStatus = TRUE. 

 
