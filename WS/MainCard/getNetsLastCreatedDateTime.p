/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : getNetsLastCreatedDateTime.p                  ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-15          
------------------------------------------------------------------------- */

                                  
DEFINE INPUT  PARAMETER LoginSessionId AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER CompanyId AS INT NO-UNDO. 
DEFINE OUTPUT PARAMETER LastRegistrertDato AS DATE  INIT TODAY NO-UNDO. 
DEFINE OUTPUT PARAMETER LastRegistrertTid  AS INT INIT 0 NO-UNDO. 
DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL INIT FALSE NO-UNDO. 

DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 

RUN checkSessionId.p (LoginSessionId,Companyid, OUTPUT loK). 

LOG-MANAGER:WRITE-MESSAGE("SessionId:" + QUOTER(LoginSessionId) + " Status:" + QUOTER(loK),"getNetsLastCreatedDateTime").
IF NOT lok THEN RETURN. 


FIND LAST nets WHERE nets.iJBoxCompanyId = companyid AND RegistrertDato NE ? USE-INDEX LastRegistrert NO-LOCK NO-ERROR.

IF AVAIL nets THEN 
ASSIGN 
   LastRegistrertDato = nets.RegistrertDato
   LastRegistrertTid  = nets.RegistrertTid. 


/*
FOR EACH nets WHERE nets.iJBoxCompanyId = companyid NO-LOCK 
   BY nets.RegistrertDato DESCENDING BY nets.RegistrertTid DESCENDING: 
  LastRegistrertDato = RegistrertDato.
  LastRegistrertTid  = RegistrertTid. 
  
  IF LastRegistrertDato = ? THEN NEXT. 
  LEAVE.
END.
*/


LOG-MANAGER:WRITE-MESSAGE("LastRegistrertDato:" + QUOTER(LastRegistrertDato) + " Status:" + QUOTER(AVAIL nets),"getNetsLastCreatedDateTime").
LOG-MANAGER:WRITE-MESSAGE("LastRegistrertTid :" + QUOTER(LastRegistrertTid)  + " Status:" + QUOTER(AVAIL nets),"getNetsLastCreatedDateTime").

RequestStatus = AVAIL nets. 

 
