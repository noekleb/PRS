&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER cMethod    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cFunction  AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cParam     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER obOk       AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocReturn   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER ocRetParam AS CHAR NO-UNDO.
                        
DEF VAR hFunctionService AS HANDLE NO-UNDO.
DEF VAR hWebService      AS HANDLE NO-UNDO.

DEF VAR cMemberId        AS CHAR NO-UNDO.
DEF VAR cBAR_CODE        AS CHAR NO-UNDO.
DEF VAR cWORKPLACE_NBR   AS CHAR NO-UNDO.
DEF VAR cREDEEM_DATE     AS CHAR NO-UNDO.
DEF VAR cMEMBER_SEARCH_TYPE AS CHAR NO-UNDO.

DEF VAR hSoapFaultDetail AS HANDLE NO-UNDO.
DEF VAR lcSoapFault      AS LONGCHAR NO-UNDO.
DEF VAR lSoapFault       AS LOG NO-UNDO.
DEFINE VARIABLE lcSoapErrorMsgs AS CHARACTER NO-UNDO.
DEF VAR lConError        AS LOG NO-UNDO.
DEF VAR cInput           AS LONGCHAR NO-UNDO.
DEF VAR cResponse        AS LONGCHAR NO-UNDO.
DEF VAR lOk              AS LOG NO-UNDO.
DEF VAR cReq             AS CHAR NO-UNDO. 
DEF VAR cWsdlFil         AS CHAR NO-UNDO.
DEF VAR WsdlParam        AS CHAR NO-UNDO.

/* sax */
DEFINE VARIABLE gcCurrentCharacters  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCurrentElementName AS CHARACTER NO-UNDO.
DEFINE VARIABLE resultcode           AS CHARACTER NO-UNDO.

/* Initieres fra systemparametre */
DEF VAR cUsername        AS CHARACTER NO-UNDO.
DEF VAR cPassword        AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 50 201 1 cUsername}
IF cUsername = '' THEN
    cUsername = "polygon".
{syspar2.i 50 201 1 cPassword} 
IF cPassword = '' THEN
    cPassword = "polygon09PRS".

CREATE SERVER hWebService.

CASE cMethod:
    WHEN 'CheckService' THEN
      DO:
        RUN getSysParam (10).
        RUN VALUE('CheckService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            obOk     = FALSE.
            ocReturn = 'Feil funksjonsnavn eller parametre send til CheckService.' + cFunction + '.'. 
        END.
      END.
    WHEN 'transactionService' THEN
      DO:
        RUN getSysParam (20).
        RUN VALUE('transactionService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            obOk     = FALSE.
            ocReturn = 'Feil funksjonsnavn eller parametre send til transactionService.' + cFunction + '.'. 
        END.
      END.
    WHEN 'MemberService' THEN
      DO:
        RUN getSysParam (30).
        RUN VALUE('MemberService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            obOk     = FALSE.
            ocReturn = 'Feil funksjonsnavn eller parametre send til MemberService.' + cFunction + '.'. 
        END.
      END.
    WHEN 'OfferService' THEN
        DO:
            RUN getSysParam (30).
            RUN VALUE('OfferService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                obOk     = FALSE.
                ocReturn = 'Feil funksjonsnavn eller parametre send til OfferService.' + cFunction + '.'. 
            END.
        END.
END CASE.

IF lOk THEN /* Her har connect og oppslag gått bra. */
  DO:
    ASSIGN
    obOk     = TRUE
    ocReturn = 'OK:' + CHR(10) +  
        replace(STRING(cResponse),'><','>' + CHR(10) + '<').
    RUN saxa (cResponse). 
    CASE cFunction:
        WHEN 'getMember' THEN RUN opprettMedlem.
    END CASE.       
  END.
ELSE IF lSoapFault THEN /* Her har connect gått bra, men oppslag feilet */
    ASSIGN
    obOk     = FALSE
    ocReturn = 'FEIL returnert fra WebService:' + CHR(10) + 
        replace(STRING(lcSoapFault),'><','>' + CHR(10) + '<').
ELSE /* Her feilet connet */
    ASSIGN
    obOk     = FALSE
    /*ocReturn = 'ConERROR: Kan ikke koble mot WebService. Kontroller parametre.'*/
    .

IF VALID-HANDLE(hFunctionService) THEN DELETE OBJECT hFunctionService. 
hWebService:DISCONNECT() NO-ERROR.
IF VALID-HANDLE(hWebService) THEN DELETE OBJECT hWebService.

IF lcSoapErrorMsgs <> '' THEN 
  MESSAGE 'lcSoapErrorMsgs:' lcSoapErrorMsgs
  VIEW-AS ALERT-BOX.

IF ocRetParam <> '' THEN 
  MESSAGE 'ocRetParam:' ocRetParam
  VIEW-AS ALERT-BOX.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

 
 
 
 
 
&IF DEFINED(EXCLUDE-OfferService.getAvailableOffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OfferService.getAvailableOffers Procedure
PROCEDURE OfferService.getAvailableOffers:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER obOk     AS LOG  NO-UNDO.
    DEF OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.

    cMemberId = ENTRY(1,icParam,'|').

    hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.

    IF NOT hWebService:CONNECTED() THEN 
        ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + 
            "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
    ELSE 
    OPPKOBLET:
    DO:
        RUN MemberService SET hFunctionService ON hWebService NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            ocReturn = 'Feil ved oppstart av service OfferService.'.
            LEAVE OPPKOBLET.
        END.

        cReq = 
            '<ns0:updateMember xmlns:ns0="http://abalon.se/mfService/">' +
            '<GetAvailableOffersRequest>' +
            '<memberId>' + cMemberId + '</memberId>' +
            '</GetAvailableOffersRequest>' +
            '<User>' +
            '<username>' + cUsername + '</username>' +
            '<password>' + cPassword + '</password>' +
            '<roleCode>wsuser</roleCode>' +  
            '</User>' +
            '</ns0:updateMember>'.    
        cInput = cReq. 

        OUTPUT TO '.\log\asMayflower.xml' APPEND.
        PUT UNFORMATTED SKIP.
        PUT UNFORMATTED 
            '** getAvailableOffers **' SKIP.
        PUT UNFORMATTED REPLACE(cReq,CHR(10),'').
        OUTPUT CLOSE.

        lOk = TRUE.
        RUN getAvailableOffers IN hFunctionService(INPUT cInput, OUTPUT cResponse) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
            DO:  
                hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
                IF VALID-HANDLE(hSoapFaultDetail) THEN
                    lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
                lSoapFault = TRUE.
                RUN saxa (lcSoapFault).
            END.
            lOk = FALSE. 
        END.  
    END. /* OPPKOBLET */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-MemberService.updateMember) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MemberService.updateMember Procedure
PROCEDURE MemberService.updateMember:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER obOk     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.

cMemberId = ENTRY(1,icParam,'|').

hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.

IF NOT hWebService:CONNECTED() THEN 
    ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + 
               "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
ELSE 
OPPKOBLET:
DO:
    RUN MemberService SET hFunctionService ON hWebService NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ocReturn = 'Feil ved oppstart av service MemberService.'.
        LEAVE OPPKOBLET.
    END.

    cReq = 
    '<ns0:updateMember xmlns:ns0="http://abalon.se/memberclub/MemberService">' +
      '<request>' +
        '<memberId>' + cMemberId + '</memberId>' +
        '<firstName>' + ENTRY(4,icParam,'|') + '</firstName>' +
        '<lastName>' + ENTRY(5,icParam,'|') + '</lastName>' +
        '<socialnr>' + (IF LENGTH(ENTRY(2,icParam,'|')) = 10
                          THEN '19'
                          ELSE '') + ENTRY(2,icParam,'|') + '</socialnr>'+
        '<email>' + ENTRY(6,icParam,'|') + '</email>'+
        '<mobile>' + ENTRY(3,icParam,'|') + '</mobile>'+
      '</request>' +
      '<User>' +
        '<username>' + cUsername + '</username>' +
        '<password>' + cPassword + '</password>' +
        '<roleCode>wsuser</roleCode>' +  
      '</User>' +
    '</ns0:updateMember>'.    
    cInput = cReq. 

OUTPUT TO '.\log\asMayflower.xml' APPEND.
PUT UNFORMATTED SKIP.
PUT UNFORMATTED '** updateMember **' SKIP.
PUT UNFORMATTED REPLACE(cReq,CHR(10),'').
OUTPUT CLOSE.

    lOk = TRUE.
    RUN updateMember IN hFunctionService(INPUT cInput, OUTPUT cResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:  
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.
            RUN saxa (lcSoapFault).
        END.
        lOk = FALSE. 
    END.  

END. /* OPPKOBLET */


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-MemberService.insertMember) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MemberService.insertMember Procedure
PROCEDURE MemberService.insertMember:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  		
			    															    RUN asMayflower ('MemberService','insertMember',
                   1  fiPersonNr:SCREEN-VALUE + '|' + 
                   2  fiMobilNr:SCREEN-VALUE + '|' + 
                   3  fiFornavn:SCREEN-VALUE + '|' + 
                   4  fiEtterNavn:SCREEN-VALUE + '|' + 
                   5  fieMail:SCREEN-VALUE + '|' +
                   6  RS-Kjonn:SCREEN-VALUE + '|' +
                   7  fiAdresse:SCREEN-VALUE + '|' +
                   8  fiPostNr:SCREEN-VALUE + '|' +
                   9  fiPostSted:SCREEN-VALUE + '|' +
                   10 fiDatoFodt:SCREEN-VALUE + '|' +
                   11 fiButikkNr:SCREEN-VALUE + '|' 
			    															  
	------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER obOk     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.

hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.

MESSAGE 'icParam' skip
icParam
VIEW-AS ALERT-BOX.

IF NOT hWebService:CONNECTED() THEN 
    ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + 
               "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
ELSE 
OPPKOBLET:
DO:
    RUN MemberService SET hFunctionService ON hWebService NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ocReturn = 'Feil ved oppstart av service MemberService.'.
        LEAVE OPPKOBLET.
    END.

    cReq = 
    '<ns0:insertMember xmlns:ns0="http://abalon.se/memberclub/MemberService">' +
      '<request>' +
        '<firstName>' + ENTRY(3,icParam,'|') + '</firstName>' +
        '<lastName>' + ENTRY(4,icParam,'|') + '</lastName>' +
        (IF TRIM(ENTRY(1,ICParam,'|')) <> '' THEN '<socialnr>' + ENTRY(1,icParam,'|') + '</socialnr>' ELSE '') +
        '<email>' + ENTRY(5,icParam,'|') + '</email>' +
        '<mobile>' + ENTRY(2,icParam,'|') + '</mobile>' + 
        ('<gender>' + IF ENTRY(6,icParam,'|') = '1' THEN 'male'
                      ELSE IF ENTRY(6,icParam,'|') = '2' THEN 'female'
                      ELSE 'unknown' + '</gender>') +
        (IF ENTRY(7,ICParam,'|') <> '' THEN  '<address>' + ENTRY(7,ICParam,'|') + '</address>' ELSE '') +
        (IF ENTRY(8,ICParam,'|') <> '' THEN  '<zip>' + ENTRY(8,ICParam,'|') + '</zip>' ELSE '') +
        (IF ENTRY(9,ICParam,'|') <> '' THEN  '<postarea>' + ENTRY(9,ICParam,'|') + '</postarea>' ELSE '') +
        
        (if ENTRY(11,icParam,'|') <> '' then '<primaryStore>' + ENTRY(11,icParam,'|') + '</primaryStore>' ELSE '') +
      '</request>' +
      '<User>' +
        '<username>' + cUsername + '</username>' +
        '<password>' + cPassword + '</password>' +
        '<roleCode>wsuser</roleCode>' +  
      '</User>' +
    '</ns0:insertMember>'.
    cInput = cReq. 

OUTPUT TO '.\log\asMayflower.xml' APPEND.
PUT UNFORMATTED SKIP.
PUT UNFORMATTED '** insertMember **' SKIP.
PUT UNFORMATTED REPLACE(cReq,CHR(10),'').
OUTPUT CLOSE.

    lOk = TRUE.
    RUN insertMember IN hFunctionService(INPUT cInput, OUTPUT cResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:  
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.
            RUN saxa (lcSoapFault).
        END.
        lOk = FALSE. 
    END.                

END. /* OPPKOBLET */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-MemberService.getMember) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MemberService.getMember Procedure
PROCEDURE MemberService.getMember:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER obOk     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.

cMemberId = ENTRY(1,icParam,'|').
IF NUM-ENTRIES(icParam,'|') >= 2 THEN cBAR_CODE = ENTRY(2,icParam,'|').

hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.

IF NOT hWebService:CONNECTED() THEN 
    ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + 
               "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
ELSE 
OPPKOBLET:
DO:
    RUN MemberService SET hFunctionService ON hWebService NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ocReturn = 'Feil ved oppstart av service MemberService.'.
        LEAVE OPPKOBLET.
    END.

    cReq = 
    '<ns0:getMember xmlns:ns0="http://abalon.se/memberclub/MemberService">' +
      '<request>' +
        '<memberId>' + cMemberId + '</memberId>' +
      '</request>' +
      '<User>' +
        '<username>' + cUsername + '</username>' +
        '<password>' + cPassword + '</password>' +
        '<roleCode>wsuser</roleCode>' +   
      '</User>' +
    '</ns0:getMember>'.
    cInput = cReq. 

OUTPUT TO '.\log\asMayflower.xml' APPEND.
PUT UNFORMATTED SKIP.
PUT UNFORMATTED '** getMember **' SKIP.
PUT UNFORMATTED REPLACE(cReq,CHR(10),'').
OUTPUT CLOSE.

    lOk = TRUE.
    RUN getMember IN hFunctionService(INPUT cInput, OUTPUT cResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:  
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.
            RUN saxa (lcSoapFault).
        END.
        lOk = FALSE. 
    END.  

END. /* OPPKOBLET */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-getSysparam) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSysparam Procedure
PROCEDURE getSysparam:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piParaNr AS INTEGER NO-UNDO.

    {syspara.i 50 201 piParaNr cWsdlFil}
    IF cWsdlFil = '' THEN
    CASE piParaNr:
        WHEN 10 THEN cWsdlFil = 'C:\Polygon\PRS\WS\MayFlower\WSCheck\wsdl\CheckService.wsdl'.
        WHEN 20 THEN cWsdlFil = 'C:\Polygon\PRS\WS\MayFlower\WSTransaction\wsdl\TransactionService.wsdl'.
        WHEN 30 THEN cWsdlFil = 'C:\Polygon\PRS\WS\MayFlower\WSMember\wsdl\MemberService.wsdl'.
    END CASE.
    {syspar2.i 50 201 piParaNr WsdlParam}
    IF WsdlParam = '' THEN
        WsdlParam = '-nohostverify'.
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-MemberService.isMember) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MemberService.isMember Procedure
PROCEDURE MemberService.isMember:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER obOk     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.

cMemberId = ENTRY(1,icParam,'|').
IF NUM-ENTRIES(icParam,'|') >= 2 THEN cBAR_CODE = ENTRY(2,icParam,'|').

hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.

IF NOT hWebService:CONNECTED() THEN 
    ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + 
               "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
ELSE 
OPPKOBLET:
DO:
    RUN MemberService SET hFunctionService ON hWebService NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ocReturn = 'Feil ved oppstart av service MemberService.'.
        LEAVE OPPKOBLET.
    END.

    cReq = 
    '<ns0:isMember xmlns:ns0="http://abalon.se/memberclub/MemberService">' +  
      '<persId>' + cMemberId + '</persId>' +
      '<User>' +
        '<username>' + cUsername + '</username>' +
        '<password>' + cPassword + '</password>' +
        '<roleCode>wsuser</roleCode>' +   
      '</User>' +
    '</ns0:isMember>'.

OUTPUT TO '.\log\asMayflower.xml' APPEND.
PUT UNFORMATTED SKIP.
PUT UNFORMATTED '** isMember **' SKIP.
PUT UNFORMATTED REPLACE(cReq,CHR(10),'').
OUTPUT CLOSE.

    cInput = cReq. 

    lOk = TRUE.
    RUN isMember IN hFunctionService(INPUT cInput, OUTPUT cResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:  
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.
            RUN saxa (lcSoapFault).
        END.
        lOk = FALSE. 
    END.                

END. /* OPPKOBLET */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-Characters) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Characters Procedure
PROCEDURE Characters:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pmCharData AS MEMPTR NO-UNDO.
    DEFINE INPUT PARAMETER piNumChars AS INTEGER NO-UNDO.
    
    ASSIGN gcCurrentCharacters = gcCurrentCharacters + GET-STRING(pmCharData, 1, GET-SIZE(pmCharData)).
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-CheckService.getCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckService.getCheck Procedure 
PROCEDURE CheckService.getCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER obOk     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.

cMemberId = ENTRY(1,icParam,'|').
IF NUM-ENTRIES(icParam,'|') >= 2 THEN cBAR_CODE = ENTRY(2,icParam,'|').

hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.

IF NOT hWebService:CONNECTED() THEN 
    ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + 
               "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
ELSE 
OPPKOBLET:
DO:
    RUN CheckService SET hFunctionService ON hWebService NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ocReturn = 'Feil ved oppstart av service CheckService.'.
        LEAVE OPPKOBLET.
    END.

    cReq = 
    '<ns0:getCheck xmlns:ns0="http://abalon.se/mfService/">' + 
      '<BAR_CODE>' + cBAR_CODE + '</BAR_CODE>' + 
      '<MemberId>' + cMemberId + '</MemberId>' + 
      '<User>' + 
        '<username>' + cUsername + '</username>' + 
        '<password>' + cPassword + '</password>' + 
        '<roleCode>wsuser</roleCode>' +  
      '</User>' + 
    '</ns0:getCheck>'
    .

    cInput = cReq. 

OUTPUT TO '.\log\asMayflower.xml' APPEND.
PUT UNFORMATTED SKIP.
PUT UNFORMATTED '** getCheck **' SKIP.
PUT UNFORMATTED REPLACE(cReq,CHR(10),'').
OUTPUT CLOSE.

    lOk = TRUE.
    RUN getCheck IN hFunctionService(INPUT cInput, OUTPUT cResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:  
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.
            RUN saxa (lcSoapFault).
        END.
        lOk = FALSE. 
    END.                

END. /* OPPKOBLET */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
&IF DEFINED(EXCLUDE-EndElement) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndElement Procedure
PROCEDURE EndElement:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcNameSpaceURI AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcLocalName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcQName AS CHARACTER NO-UNDO.
    
    CASE cFunction:
        WHEN 'getCheck' THEN 
          DO:
            CASE gcCurrentElementName:
                /* getCheck */
                WHEN "IS_VALID"      THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "IS_REDEEMED"   THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "IS_EXPIRED"    THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "VALUE"         THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "VALUE_UNIT"    THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "TYPE"          THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "EXPIRE_DATE"   THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + ENTRY(1,gcCurrentCharacters,'T').
                WHEN "CURRENCY_CODE" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                /* SoapFeil */
                WHEN "message" THEN ASSIGN lcSoapErrorMsgs = gcCurrentCharacters.
            END CASE.        
          END.

        WHEN 'insertTransaction' THEN 
          DO:
            CASE gcCurrentElementName:
                /* InsertTransaction */
                WHEN "REFERENCE_NBR" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "BONUS_BALANCE" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "ACC_BONUS_BALANCE" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "ACC_BONUS_BALANCE_CURRENT_YEAR" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "TRANSACTION_BONUS" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                /* SoapFeil */
                WHEN "message" THEN ASSIGN lcSoapErrorMsgs = gcCurrentCharacters.
            END CASE.        
          END.
                
        WHEN 'isMember' THEN 
          DO:
            CASE gcCurrentElementName:
                /* IsMember */
                WHEN "return" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                /* SoapFeil */
                WHEN "message" THEN ASSIGN lcSoapErrorMsgs = gcCurrentCharacters.
            END CASE.        
          END.

        WHEN 'insertMember' THEN 
          DO:
            CASE gcCurrentElementName:
                /* InsertMember */
                WHEN "memberId" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "cardNumber" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "password" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                WHEN "webcode" THEN ASSIGN ocRetParam = ocRetParam + (IF ocRetParam <> '' THEN '|' ELSE '') + gcCurrentCharacters.
                /* SoapFeil */
                WHEN "message" THEN ASSIGN lcSoapErrorMsgs = gcCurrentCharacters.
            END CASE.        
          END.
          
        WHEN 'getMember' THEN 
          DO:
            IF ocRetParam = '' THEN ocRetParam = FILL('|',40).            
            CASE gcCurrentElementName:
                /* GetMember */
                WHEN "memberId"         THEN ASSIGN ENTRY( 1,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "accountNumber"    THEN ASSIGN ENTRY( 2,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "firstName"        THEN ASSIGN ENTRY( 3,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "lastName"         THEN ASSIGN ENTRY( 4,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "gender"           THEN ASSIGN ENTRY( 5,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "socialnr"         THEN ASSIGN ENTRY( 6,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "password"         THEN ASSIGN ENTRY( 7,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "email"            THEN ASSIGN ENTRY( 8,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "mobile"           THEN ASSIGN ENTRY( 9,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "phone"            THEN ASSIGN ENTRY(10,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "coAddress"        THEN ASSIGN ENTRY(11,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "address"          THEN ASSIGN ENTRY(12,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "zip"              THEN ASSIGN ENTRY(13,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "postarea"         THEN ASSIGN ENTRY(14,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "countryCode"      THEN ASSIGN ENTRY(15,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "code"             THEN ASSIGN ENTRY(16,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "displayName"      THEN ASSIGN ENTRY(17,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "noCommercials"    THEN ASSIGN ENTRY(18,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "primaryStore"     THEN ASSIGN ENTRY(19,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "ack_bonus"        THEN ASSIGN ENTRY(20,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "bonus"            THEN ASSIGN ENTRY(21,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "bonusToNextCheck" THEN ASSIGN ENTRY(22,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "accountType"      THEN ASSIGN ENTRY(23,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "webCode"          THEN ASSIGN ENTRY(24,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "nationalityCode"  THEN ASSIGN ENTRY(25,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "entryDate"        THEN ASSIGN ENTRY(26,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "planExit"         THEN ASSIGN ENTRY(27,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "exitDate"         THEN ASSIGN ENTRY(28,ocRetParam,'|') = gcCurrentCharacters.
                WHEN "currencyCode"     THEN ASSIGN ENTRY(29,ocRetParam,'|') = gcCurrentCharacters.
                /* SoapFeil */
                WHEN "message" THEN ASSIGN lcSoapErrorMsgs = gcCurrentCharacters.
            END CASE.        
          END.
    END CASE.   
    ASSIGN gcCurrentElementName = "".
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-opprettMedlem) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettMedlem Procedure
PROCEDURE opprettMedlem:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  
	------------------------------------------------------------------------------*/
DEFINE VARIABLE cButKlubbListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLengdeListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMKlubbId AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMedlemsNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE cKortNr AS CHARACTER NO-UNDO.

FIND FIRST SysPara NO-LOCK WHERE
    SysPara.SysHId = 14 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr >= 31 AND 
    SysPara.ParaNr <= 39 AND
    CAN-DO(SysPara.Parameter1,ENTRY(2,cParam,'|')) NO-ERROR.
IF AVAILABLE SysPara THEN
  ASSIGN 
      cButKlubbListe = SysPara.Parameter1
      cMKlubbId      = SysPara.Parameter2
      .  
IF NOT AVAILABLE SysPara OR 
  cMKlubbId = ''
THEN DO:
    {syspara.i 14 1  7 cMKlubbId}
END.

ASSIGN
  cKortNr = LEFT-TRIM(REPLACE(TRIM(ENTRY(2,ocRetParam,'|')),'M-',''),'0').
  
FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
FIND FIRST MedlemsType   NO-LOCK NO-ERROR.

ASSIGN lMedlemsNr = DEC(cKortNr) NO-ERROR.
IF lMedlemsNr = 0 OR CAN-FIND(Medlem WHERE Medlem.MedlemsNr = lMedlemsNr) THEN 
DO:
  FIND LAST medlem NO-LOCK NO-ERROR.
  IF AVAILABLE Medlem THEN 
    lMedlemsNr = Medlem.MedlemsNr + 1.
  ELSE 
    lMedlemsNr = 1.
END.
  
IF NOT CAN-FIND(FIRST Medlem WHERE 
                Medlem.EksterntMedlemsNr = TRIM(ENTRY(2,ocRetParam,'|'))) THEN 
DO TRANSACTION:
        FIND FIRST Post WHERE 
          Post.PostNr = TRIM(ENTRY(13,ocRetParam,'|')) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Post THEN 
        DO:
            CREATE Post.
            ASSIGN
                Post.PostNr      = TRIM(ENTRY(13,ocRetParam,'|'))
                Post.Beskrivelse = TRIM(ENTRY(14,ocRetParam,'|'))
                .
            FIND CURRENT Post NO-LOCK.
        END.
        CREATE Medlem.
        ASSIGN 
            Medlem.MedlemsNr = lMedlemsNr
            Medlem.PersonNr  = SUBSTRING(TRIM(ENTRY(6,ocRetParam,'|')),3)
            Medlem.ForNavn   = TRIM(ENTRY(3,ocRetParam,'|'))
            Medlem.EtterNavn = TRIM(ENTRY(4,ocRetParam,'|'))
            Medlem.PostNr    = TRIM(ENTRY(13,ocRetParam,'|'))
            Medlem.Adresse2  = TRIM(ENTRY(11,ocRetParam,'|'))
            Medlem.Adresse1  = TRIM(ENTRY(12,ocRetParam,'|'))
            Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 0
            Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType ELSE 0
            Medlem.MKlubbId  = INT(cMKlubbId)
            Medlem.ButikkNr  = INT(ENTRY(2,cParam,'|'))
            Medlem.Kjonn     = CAN-DO('male,man,m',TRIM(ENTRY(5,ocRetParam,'|')))
            Medlem.EksterntMedlemsNr = TRIM(ENTRY(2,ocRetParam,'|'))
            Medlem.Kilde     = 'MayFlower'
            Medlem.ePostAdresse = TRIM(ENTRY(8,ocRetParam,'|'))
            Medlem.MobilTlf     = TRIM(ENTRY(9,ocRetParam,'|'))
            Medlem.Telefon      = TRIM(ENTRY(10,ocRetParam,'|'))
            . 
            
        CREATE MedlemsKort.
        ASSIGN
            MedlemsKort.MedlemsNr = Medlem.MedlemsNr
            MedlemsKort.KortNr       = cKortNr
            MedlemsKort.AktivertDato = TODAY 
            MedlemsKort.UtgarDato    = TODAY + 999
            MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
            MedlemsKort.KortType     = 1
        .
    IF AVAILABLE Medlem THEN RELEASE medlem.
    IF AVAILABLE Medlemskort THEN RELEASE MedlemsKort.
END. /* TRANSACTION */


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-saxa) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saxa Procedure
PROCEDURE saxa:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER RawXML AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE SAXReader AS HANDLE NO-UNDO.
    DEFINE VARIABLE ResultParser AS HANDLE NO-UNDO.
    DEFINE VARIABLE SearchResult AS CHARACTER NO-UNDO.

    CREATE SAX-READER SAXReader.
    SAXReader:HANDLER = THIS-PROCEDURE.
    SAXReader:SET-INPUT-SOURCE("LONGCHAR", RawXML).
    SAXReader:SAX-PARSE() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        resultcode = "100".
    IF VALID-HANDLE(SAXReader) THEN
        DELETE OBJECT SAXReader.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-StartElement) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartElement Procedure
PROCEDURE StartElement:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcNamespaceURI AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcLocalName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcQName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER phAttributes AS HANDLE NO-UNDO.
    
    ASSIGN gcCurrentCharacters = ""
           gcCurrentElementName = pcQName.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-transactionService.insertTransaction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transactionService.insertTransaction Procedure 
PROCEDURE transactionService.insertTransaction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER obOk     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.

DEF VAR cB_Id AS CHAR NO-UNDO.
DEF VAR cDato AS CHAR NO-UNDO.

ocReturn = ''.
cB_Id = ENTRY(1,icParam,'|').
IF NUM-ENTRIES(icParam,'|') >= 2 THEN cMemberId           = ENTRY(2,icParam,'|').
IF NUM-ENTRIES(icParam,'|') >= 3 THEN cMEMBER_SEARCH_TYPE = ENTRY(3,icParam,'|').

FIND BongHode NO-LOCK WHERE
    BongHode.B_Id = DEC(cB_Id) NO-ERROR.
IF NOT AVAILABLE BongHode THEN
DO:
    obOk     = FALSE.
    ocReturn = 'Ukjent bongreferanse (' + cB_Id + ').'.
    RETURN.
END.
IF BongHode.MedlemsNr > 0 THEN
    FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = BongHode.MedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
DO:
    /*
    obOk     = FALSE.
    ocReturn = 'Ukjent medlem på bong det refereres til (B_Id: ' + cB_Id + ', Medlem: ' + STRING(BongHode.MedlemsNr) + ').'.
    RETURN.
    */
END.


ASSIGN
    /* 2013-04-24T22:08:38 */
    cDato = STRING(YEAR(BongHode.Dato),"9999") + '-' +
            STRING(MONTH(BongHode.Dato),"99") + '-' + 
            STRING(DAY(BongHode.Dato),"99") + 'T' + 
            STRING(TIME,"HH:MM:SS").

hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.

IF NOT hWebService:CONNECTED() THEN 
    ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + CHR(10) + 
    "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
ELSE 
OPPKOBLET:
DO:        
    RUN TransactionService SET hFunctionService ON hWebService NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ocReturn = 'Feil ved oppstart av service TransactionService. '.
        LEAVE OPPKOBLET.
    END.

    /* Bygging av cInput string */
    cReq = '<ns0:insertTransaction xmlns:ns0="http://abalon.se/mfService/">' + CHR(10) +
              '<Transaction>' + CHR(10) +
                (IF cMemberId <> '' 
                    THEN '<MEMBER_ID>' + cMemberId + '</MEMBER_ID>' + CHR(10)
                    ELSE '') +
                /*                (IF cMEMBER_SEARCH_TYPE <> ''                                                            */
                /*                    THEN '<MEMBER_SEARCH_TYPE>' + cMEMBER_SEARCH_TYPE + '</MEMBER_SEARCH_TYPE>' + CHR(10)*/
                /*                    ELSE '') +                                                                           */
                '<AMOUNT>' + TRIM(STRING(BongHode.Belop,">>>>>>>>>>9.99")) + '</AMOUNT>' + CHR(10) +
                '<PURCHASE_DATE>' + cDato + '</PURCHASE_DATE>' + CHR(10) +
                '<RECEIPT_NBR>' + STRING(BongHode.BongNr) + '</RECEIPT_NBR>' + CHR(10) +
                '<TERMINAL_NBR>' + STRING(BongHode.KasseNr) + '</TERMINAL_NBR>' + CHR(10) +
                '<WORKPLACE_NBR>' + STRING(BongHode.ButikkNr) + '</WORKPLACE_NBR>' + CHR(10) +
                '<CURRENCY>NOK</CURRENCY>' + CHR(10).

MESSAGE 'Legger inn rader' cReq
VIEW-AS ALERT-BOX.

    /* Her legger vi på salgstransaksjonene */
    BONGLINJE_BLOKK:
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id  
/*       AND BongLinje.Makulert = FALSE AND           */
/*        CAN-DO('1,3,10',STRING(BongLinje.TTId)) */ :
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) NO-ERROR.
        cReq = cReq + 
        '<rows>' + CHR(10) +
          '<AMOUNT>' + TRIM(STRING(BongLinje.LinjeSum - BongLinje.MvaKr)) + '</AMOUNT>' + CHR(10) + 
          '<AMOUNT_COST>' + TRIM(STRING(BongLinje.VVareKost * ABS(BongLinje.Antall))) +  '</AMOUNT_COST>' + CHR(10) + 
          '<BONUS_BASED>' + (IF AVAILABLE ArtBas 
                               THEN (IF ArtBas.Bonus_Givende THEN 'true' ELSE 'false')
                               ELSE 'false')  + '</BONUS_BASED>' + CHR(10) +
          '<PIECES>' + STRING(BongLinje.Antall) + '</PIECES>' + CHR(10) +
          '<PRODUCT>' +  CHR(10) +
            '<CODE>' + BongLinje.Strekkode + '</CODE>' + CHR(10) + 
            '<DESC>' + BongLinje.BongTekst + '</DESC>' + CHR(10) +
          '</PRODUCT>' +  CHR(10) +
          '<PRODUCT_GROUP>' +  CHR(10) +
            '<CODE>' + STRING(BongLinje.VareGr) + '</CODE>' + CHR(10) +
            '<DESC>' + STRING(BongLinje.VareGruppeNavn) + '</DESC>' +  CHR(10) +
          '</PRODUCT_GROUP>' +  CHR(10) +
          '<DISCOUNT>' + STRING(BongLinje.LinjeRab + BongLinje.SubtotalRab) + '</DISCOUNT>' + CHR(10) + 
        '</rows>' + CHR(10).
        
        MESSAGE 'Rad' BongLinje.LinjeNr
        VIEW-AS ALERT-BOX.
        
    END. /* BONGLINJE_BLOKK */
    
   
    MESSAGE 'Rader ferdig' cReq
        VIEW-AS ALERT-BOX.

    /* Her legger vi på sjekken(e) */
    BONGLINJE_SJEKK_BLOKK:
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND 
        BongLinje.Makulert = FALSE AND
        CAN-DO('98',STRING(BongLinje.TTId)) AND
        BongLinje.BongTekst BEGINS 'MayFlower Sjeck':
        cReq = cReq + 
                '<checks>' + CHR(10) +
                  '<BAR_CODE>' + BongLinje.Strekkode + '</BAR_CODE>' + CHR(10) + 
                  '<WORKPLACE_NBR>' + STRING(BongLinje.ButikkNr) + '</WORKPLACE_NBR>' + CHR(10) + 
                  '<REDEEM_DATE>' + 
                    cDato + 
                  '</REDEEM_DATE>' + CHR(10) + 
                '</checks>' + CHR(10).
    END. /* BONGLINJE_SJEKK_BLOKK */

    cReq = cReq + 
             '</Transaction>' + CHR(10) + 
             '<User>' +  CHR(10) +
               '<username>' + cUsername + '</username>' + CHR(10) + 
               '<password>' + cPassword + '</password>' +  CHR(10) +
               '<roleCode>wsuser</roleCode>' +  CHR(10) + 
             '</User>' +  CHR(10) +
           '</ns0:insertTransaction>' + CHR(10).
    /* Bygging av cInput feridg. */
    cInput = REPLACE(cReq,CHR(10),''). 

OUTPUT TO '.\log\asMayflower.xml' APPEND.
PUT UNFORMATTED SKIP.
PUT UNFORMATTED '** insertTransaction **' SKIP.
PUT UNFORMATTED cReq.
OUTPUT CLOSE.

    lOk = TRUE.
    RUN insertTransaction IN hFunctionService(INPUT cInput, OUTPUT cResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.
            RUN saxa (lcSoapFault).
        END.
        lOk = FALSE.
    END.

END. /* OPPKOBLET */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

