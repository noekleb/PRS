/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : getNetsLastCreatedDateTime.p                  ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-15          
------------------------------------------------------------------------- */

                                  
DEFINE INPUT  PARAMETER LoginSessionId AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER CompanyId AS INT NO-UNDO. 
DEFINE OUTPUT PARAMETER LastRegistrertDato AS DATE  INIT ? NO-UNDO. 
DEFINE OUTPUT PARAMETER LastRegistrertTid  AS INT INIT ? NO-UNDO. 
DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL INIT FALSE NO-UNDO. 

DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 

RUN checkSessionId.p (LoginSessionId,Companyid, OUTPUT loK). 

LOG-MANAGER:WRITE-MESSAGE("SessionId:" + QUOTER(LoginSessionId) + " Status:" + QUOTER(loK),"getNetsLastCreatedDateTime").
IF NOT lok THEN RETURN. 

FOR EACH nets WHERE nets.iJBoxCompanyId = companyid NO-LOCK 
   BY nets.RegistrertDato DESCENDING BY nets.RegistrertTid DESCENDING: 
  LastRegistrertDato = RegistrertDato.
  LastRegistrertTid  = RegistrertTid. 
  LEAVE.
END.

LOG-MANAGER:WRITE-MESSAGE("LastRegistrertDato:" + QUOTER(LastRegistrertDato) + " Status:" + QUOTER(AVAIL nets),"getNetsLastCreatedDateTime").
LOG-MANAGER:WRITE-MESSAGE("LastRegistrertTid :" + QUOTER(LastRegistrertTid)  + " Status:" + QUOTER(AVAIL nets),"getNetsLastCreatedDateTime").

RequestStatus = AVAIL nets. 

 
