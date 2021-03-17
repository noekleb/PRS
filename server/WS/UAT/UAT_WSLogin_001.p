/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : UAT_WSLogin_001.p                             ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-10            
------------------------------------------------------------------------- */

DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE hPRSWebObj AS HANDLE NO-UNDO.

DEFINE VARIABLE cUserid    AS CHAR INIT "netstestuser2" NO-UNDO. 
DEFINE VARIABLE cPassword  AS CHAR INIT "testing1234" NO-UNDO. 
DEFINE VARIABLE lStatus    AS LOG NO-UNDO. 
DEFINE VARIABLE cSessionId AS CHAR NO-UNDO. 


CREATE SERVER hWebService.
hWebService:CONNECT("-WSDL 'T:\PRSTrans\WSDeploy\Test.wsdl'").

RUN WSMainCardObj SET hPRSWebObj ON hWebService.

RUN WSLogin IN hPRSWebObj (cUserid,
                           cPassword,
                           OUTPUT cSessionId,
                           OUTPUT lStatus).

MESSAGE  cUserid cSessionid lStatus VIEW-AS ALERT-BOX. 

DELETE OBJECT hPRSWebObj. 
hWebService:DISCONNECT(). 
