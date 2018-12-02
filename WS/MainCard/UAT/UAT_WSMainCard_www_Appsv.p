
/*
DEFINE VARIABLE ws AS HANDLE NO-UNDO. 
DEFINE VARIABLE opcSessionID AS CHAR NO-UNDO. 
DEFINE VARIABLE oplStatus AS LOGICAL INIT FALSE  NO-UNDO. 
DEFINE VARIABLE transid AS CHAR INIT "379701 207070" NO-UNDO. 
                                                               

DEFINE VARIABLE cConnectionString AS CHAR NO-UNDO. 
DEFINE VARIABLE hServer AS HANDLE NO-UNDO. 
DEFINE VARIABLE lConnected AS LOGICAL NO-UNDO. 

cConnectionString = "-URL http://appfarm.netextend.no/aia/Aia?AppService=PRSTrans -sessionModel session-free".

CREATE SERVER hServer. 
lConnected = hServer:CONNECT(cConnectionString)  .

IF lConnected THEN
DO:
    MESSAGE "ok" VIEW-AS ALERT-BOX.                                                       
    RUN ping.p  ON hserver. 
    
    MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX. 

    hServer:DISCONNECT() NO-ERROR.
END. 
DELETE OBJECT hServer NO-ERROR. 



*/

RUN wsmaincard.p PERSISTENT SET ws. 

RUN login IN ws ("netstestuser","testing123",OUTPUT opcSessionid,OUTPUT oplStatus).

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

DEFINE VARIABLE RequestStatus AS LOG NO-UNDO. 

RUN GetNetsData IN ws (opcSessionid,801,TransId, OUTPUT TABLE RowData,OUTPUT RequestStatus).

DELETE OBJECT ws. 

RUN checkSessionId.p (opcSessionid,OUTPUT oplstatus). 


FOR EACH rowData : 
    DISP rowData WITH 1 COL . 
END.

MESSAGE opcSessionid oplstatus  RequestStatus VIEW-AS ALERT-BOX. 
