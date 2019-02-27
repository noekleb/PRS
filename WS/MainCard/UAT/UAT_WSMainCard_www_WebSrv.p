DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE hPRSWebObj AS HANDLE NO-UNDO.

DEFINE VARIABLE lStatus AS LOG NO-UNDO. 
DEFINE VARIABLE cSessionId AS CHAR NO-UNDO. 

CREATE SERVER hWebService.
hWebService:CONNECT("-WSDL 'T:\PRSTrans\WSDeploy\Test.wsdl'").

RUN WSMainCardObj SET hPRSWebObj ON hWebService.


DEFINE VARIABLE result AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.

RUN wsLogin IN hPRSWebObj  ("netstestuser","testing123",OUTPUT cSessionId, OUTPUT lStatus).

MESSAGE cSessionid lStatus VIEW-AS ALERT-BOX. 

/*
RUN PING IN hPRSWebObj (OUTPUT c).
*/

/*
FUNCTION ping RETURNS CHARACTER (OUTPUT c AS CHARACTER) IN hPRSWebObj.

result = ping(c).



RUN PING IN hPRSWebObj(OUTPUT c).

MESSAGE c VIEW-AS ALERT-BOX. 
*/
/*
Melk 
Druer 
Brød 
Pålegg 
Salami / Skinke 
*/
