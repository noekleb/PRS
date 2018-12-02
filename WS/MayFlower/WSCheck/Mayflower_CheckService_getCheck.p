DEFINE VARIABLE hCheckService AS HANDLE NO-UNDO.
DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.

DEF VAR lOk AS LOG NO-UNDO.
DEF VAR cUsername AS CHARACTER NO-UNDO.
DEF VAR cPassword AS CHARACTER NO-UNDO.

DEF VAR cMemberId AS CHAR      NO-UNDO.
DEF VAR cBAR_CODE AS CHAR NO-UNDO.

DEF VAR hSoapFaultDetail AS HANDLE NO-UNDO.
DEF VAR lcSoapFault AS LONGCHAR NO-UNDO.
DEF VAR lSoapFault AS LOG NO-UNDO.
DEF VAR lConError AS LOG NO-UNDO.
DEF VAR cErrorMsgs AS CHAR NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEFINE VARIABLE getCheck AS LONGCHAR NO-UNDO.
DEFINE VARIABLE getCheckResponse AS LONGCHAR NO-UNDO.

ASSIGN
  cUsername = "polygon"
  cPassword = "2xkCXjz0ic"
  cMemberId = "M-00019654"
  cBAR_CODE = "1000004406"
  .

CREATE SERVER hWebService.

/* Oppkobling mot WebService */
hWebService:CONNECT("-WSDL C:\Polygon\PRS\WS\MayFlower\WSCheck\wsdl\CheckService.wsdl -nohostverify ") NO-ERROR.

IF NOT hWebService:CONNECTED() THEN 
    cErrorMsgs = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.'.
ELSE 
OPPKOBLET:
DO:
    /* Oppstart av service */
    RUN CheckService SET hCheckService ON hWebService NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        cErrorMsgs = 'Feil ved oppstart av service CheckService.'.
        LEAVE OPPKOBLET.
    END.

    DEF VAR req AS CHAR. 
    req = 
    '<ns0:getCheck xmlns:ns0="http://abalon.se/mfService/">
      <BAR_CODE>' + cBAR_CODE + '</BAR_CODE>
      <MemberId>' + cMemberId + '</MemberId>
      <User>
        <username>' + cUsername + '</username>
        <password>' + cPassword + '</password>
      </User>
    </ns0:getCheck>'.

    getCheck = req. 

    lOk = TRUE.
    RUN getCheck IN hCheckService(INPUT getCheck, OUTPUT getCheckResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:  
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.
        END.
        lOk = FALSE. 
    END.                

    IF lOk THEN /* Her har connect og oppslag gått bra. */
        cReturn = 'OK:' + CHR(10) +  
            REPLACE(STRING(getCheckResponse),'><','>' + CHR(10) + '<').
    ELSE IF lSoapFault THEN /* Her har connect gått bra, men oppslag feilet */
        cErrorMsgs = 'FEIL returnert fra WebService:' + CHR(10) + 
            replace(STRING(lcSoapFault),'><','>' + CHR(10) + '<').
    ELSE /* Her feilet connet */
        cErrorMsgs = 'Connect ERROR: Kan ikke koble mot WebService. Kontroller parametre.'.
END. /* OPPKOBLET */


IF valid-handle(hCheckService) THEN DELETE OBJECT hCheckService. 
hWebService:DISCONNECT() NO-ERROR.
IF VALID-HANDLE(hWebService) THEN DELETE OBJECT hWebService.

IF lOk THEN
    MESSAGE lOk cReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE
    MESSAGE lOk cErrorMsgs
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.


