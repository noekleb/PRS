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
DEFINE VARIABLE TransId AS CHAR INIT "426667 003404" NO-UNDO. 

DEFINE TEMP-TABLE RowData NO-UNDO
    FIELD ButikkNr AS INT 
    FIELD ButNavn AS CHAR  
    FIELD EkstKundeNr AS CHAR  
    FIELD ArtikkelNr AS CHAR  
    FIELD StrKode AS CHAR  
    FIELD VareTekst AS CHAR  
    FIELD VareGr AS INT  
    FIELD VareGrTekst AS CHAR  
    FIELD HovedGr AS INT  
    FIELD HovedGrTekst AS CHAR  
    FIELD Avdeling AS CHAR  
    FIELD AvdelingTekst AS CHAR  
    FIELD Dato AS DATE  
    FIELD Antall AS DEC  
    FIELD LevNr AS INT  
    FIELD LevNavn AS CHAR  
    FIELD RabKr AS DEC  
    FIELD InnVerdiKr AS DEC  
    FIELD UtVerdiKr AS DEC  
    FIELD MvaKr AS DEC.


CREATE SERVER hWebService.
hWebService:CONNECT("-WSDL 'T:\PRSTrans\WSDeploy\Test.wsdl'").

RUN WSMainCardObj SET hPRSWebObj ON hWebService.

RUN WSLogin IN hPRSWebObj 
    (cUserid,
     cPassword,
     OUTPUT cSessionId,
     OUTPUT lStatus).

MESSAGE  cUserid cSessionid lStatus VIEW-AS ALERT-BOX. 

IF lStatus THEN
   RUN WSGetNetsData IN hPRSWebObj 
    (cSessionid,
     801,
     TransId, 
     OUTPUT TABLE RowData,
     OUTPUT lStatus).

FOR EACH rowData : 
    DISP rowData WITH 1 COL . 
END.

DELETE OBJECT hPRSWebObj. 
hWebService:DISCONNECT(). 
