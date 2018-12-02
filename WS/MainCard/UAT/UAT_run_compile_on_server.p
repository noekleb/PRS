/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : UAT_getNetsLastCreatedDateTime.p              ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-10            
------------------------------------------------------------------------- */

DEFINE TEMP-TABLE tt_Updated NO-UNDO
    FIELD transactionid AS CHAR
    FIELD sendtDato AS DATE 
    FIELD sendt AS LOGICAL 
    FIELD sendtTid AS INT. 

DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE hPRSWebObj AS HANDLE NO-UNDO.

DEFINE VARIABLE cUserid    AS CHAR INIT "netstestuser2" NO-UNDO. 
DEFINE VARIABLE cPassword  AS CHAR INIT "testing1234" NO-UNDO. 
DEFINE VARIABLE lStatus    AS LOG NO-UNDO. 
DEFINE VARIABLE cSessionId AS CHAR NO-UNDO. 
DEFINE VARIABLE TransId AS CHAR INIT "426667 003404" NO-UNDO. 
DEFINE VARIABLE iNumRows AS INT NO-UNDO. 
DEFINE VARIABLE iTime AS INT NO-UNDO. 
DEFINE VARIABLE dDate AS DATE INIT TODAY NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHAR NO-UNDO. 
DEFINE VARIABLE lConnected AS LOGICAL NO-UNDO. 

CREATE SERVER hWebService.

cConnectionString = "-WSDL 'T:\PRSTrans\WSDeploy\Test.wsdl'". 
cConnectionString = "-URL http://appfarm.netextend.no/aia/Aia?AppService=PRSTrans -sessionModel session-free".
lConnected = hWebService:CONNECT(cConnectionString)  .

dDate = dDate.

MESSAGE hWebService:SUBTYPE VIEW-AS ALERT-BOX. 

IF  hWebService:SUBTYPE = "WEBSERVICE" THEN
   RUN WSMainCardObj SET hPRSWebObj ON hWebService.
ELSE IF hWebService:SUBTYPE = "APPSERVER" THEN 
    RUN VALUE(LC("COMPILE.p"))  ON hWebService.

hWebService:DISCONNECT(). 

