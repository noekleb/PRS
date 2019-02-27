&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : asMayflower.p 
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

DEF INPUT  PARAMETER cMethod     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cFunction   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cSearchType AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cParam      AS CHAR NO-UNDO.
DEF INPUT-OUTPUT  PARAMETER TABLE-HANDLE hParam . 

DEF OUTPUT PARAMETER obOk        AS LOG  NO-UNDO. /* Gikk kall fra POS bra (Bare kallet, ikke svaret)    */
DEF OUTPUT PARAMETER ocReturn    AS CHAR NO-UNDO. /* Eventuelle feilmeldinger                            */
DEF OUTPUT PARAMETER ocRetParam  AS CHAR NO-UNDO. /* Pipe separerte Parameterverdier som kassen skal ha. */
                        
DEF VAR hFunctionService AS HANDLE NO-UNDO.
DEF VAR hWebService      AS HANDLE NO-UNDO.

DEF VAR cMemberId           AS CHAR NO-UNDO. /* Medlemsnr. Kassen sender inn telefonnr.            */
DEF VAR cBAR_CODE           AS CHAR NO-UNDO. /* Varelinjens GTIN kode                              */
DEF VAR cWORKPLACE_NBR      AS CHAR NO-UNDO. /* ButikkNr                                           */
DEF VAR cREDEEM_DATE        AS CHAR NO-UNDO. /* Salgsdato                                          */
DEF VAR cMEMBER_SEARCH_TYPE AS CHAR NO-UNDO. /* Type søk på medlem. Her brukes tlfnr som standard. */

DEF VAR hSoapFaultDetail AS HANDLE NO-UNDO.
DEF VAR lcSoapFault      AS LONGCHAR NO-UNDO.
DEF VAR lSoapFault       AS LOG NO-UNDO.
DEF VAR lcSoapErrorMsgs  AS CHARACTER NO-UNDO.
DEF VAR lConError        AS LOG NO-UNDO.
DEF VAR cInput           AS LONGCHAR NO-UNDO.
DEF VAR cResponse        AS LONGCHAR NO-UNDO.
DEF VAR gcErrorMessage AS CHAR NO-UNDO. 

DEF VAR cReq             AS CHAR NO-UNDO. 
DEF VAR cWsdlFil         AS CHAR NO-UNDO.
DEF VAR WsdlParam        AS CHAR NO-UNDO.
DEF VAR lLogXml          AS LOG NO-UNDO.
DEF VAR lLogReq          AS LOG NO-UNDO.
DEF VAR cTekst           AS CHAR NO-UNDO.
DEF VAR lok              AS LOGICAL NO-UNDO. 
/* Sax */
DEFINE VARIABLE gcCurrentCharacters  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCurrentElementName AS CHARACTER NO-UNDO.
DEFINE VARIABLE resultcode           AS CHARACTER NO-UNDO.

/* Initieres fra systemparametre */
DEF VAR cUsername        AS CHARACTER NO-UNDO.
DEF VAR cPassword        AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE request NO-UNDO
        NAMESPACE-URI "" 
        FIELD memberId AS CHARACTER 
        FIELD memberSearchType AS CHARACTER .

DEFINE TEMP-TABLE User1 NO-UNDO
        NAMESPACE-URI "" 
        XML-NODE-NAME "User" 
        FIELD username AS CHARACTER 
        FIELD password AS CHARACTER 
        FIELD roleCode AS CHARACTER .

DEFINE DATASET getMember NAMESPACE-URI "http://abalon.se/memberclub/MemberService" 
        FOR request, User1.

DEFINE TEMP-TABLE insertMemberResponseReturn NO-UNDO
        NAMESPACE-URI "" 
        XML-NODE-NAME "return" 
        FIELD memberId AS CHARACTER 
        FIELD cardNumber AS CHARACTER 
        FIELD password AS CHARACTER 
        FIELD webcode AS CHARACTER .

DEFINE DATASET insertMemberResponse NAMESPACE-URI "http://abalon.se/memberclub/MemberService" 
        FOR insertMemberResponseReturn.

DEFINE TEMP-TABLE getMemberResponseReturn NO-UNDO
        NAMESPACE-URI "" 
        XML-NODE-NAME "return" 
        FIELD memberId AS CHARACTER FORMAT "x(20)"
        FIELD accountNumber AS CHAR 
        FIELD cardNumber AS CHARACTER 
        FIELD password AS CHARACTER 
        FIELD webcode AS CHARACTER 
        FIELD firstName AS CHAR
        FIELD lastName AS CHAR 
        FIELD gender AS CHAR 
        FIELD mobile AS CHAR     
        FIELD phone AS CHAR 
        FIELD socialnr AS CHAR
        FIELD email  AS CHAR 
        FIELD zip  AS CHAR 
        FIELD postarea AS CHAR
        FIELD countryCode AS CHAR
        FIELD address AS CHAR
        FIELD birthdate AS CHAR. 

DEFINE DATASET getMemberResponse NAMESPACE-URI "http://abalon.se/memberclub/MemberService" 
            FOR getMemberResponseReturn.

DEFINE TEMP-TABLE MemberServiceFault NO-UNDO
        NAMESPACE-URI "http://abalon.se/memberclub/MemberService"
        XML-NODE-NAME "MemberServiceFault" 
        FIELD FaultMessage AS CHARACTER XML-NODE-NAME "message"
        FIELD UserFaultMessage AS CHAR .

DEFINE DATASET detail FOR MemberServiceFault.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getInsertMemberRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInsertMemberRequest Procedure 
FUNCTION getInsertMemberRequest RETURNS LONGCHAR (
         INPUT firstName AS CHAR,
         INPUT lastName AS CHAR,
         INPUT socialnr AS CHAR,
         INPUT birthDate AS DATE, 
         INPUT email AS CHAR, 
         INPUT mobile AS CHAR ,
         INPUT gender AS CHAR,
         INPUT address AS CHAR,
         INPUT zip AS CHAR ,
         INPUT postarea AS CHAR,
         INPUT primaryStore AS INT ,
         INPUT registerStore AS INT, 
         INPUT countrycode AS CHAR,
         INPUT currencyCode AS CHAR,
         INPUT username AS CHAR,
         INPUT password AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 24.57
         WIDTH              = 84.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE hParameterTable AS HANDLE NO-UNDO. 
LOG-MANAGER:LOGGING-LEVEL = 3.

hParameterTable = hParam:DEFAULT-BUFFER-HANDLE.
IF NOT hParameterTable:AVAIL THEN hParameterTable:FIND-FIRST() NO-ERROR.


/* Call main method/Service Member/Check/Transaction/Offer */
RUN VALUE(cMethod) (cFunction,cSearchType,cParam, OUTPUT obok, OUTPUT ocReturn) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Characters) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Characters Procedure 
PROCEDURE Characters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pmCharData AS MEMPTR NO-UNDO.
    DEFINE INPUT PARAMETER piNumChars AS INTEGER NO-UNDO.
    
    ASSIGN gcCurrentCharacters = gcCurrentCharacters + GET-STRING(pmCharData, 1, GET-SIZE(pmCharData)).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndElement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndElement Procedure 
PROCEDURE EndElement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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

&IF DEFINED(EXCLUDE-getSysParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSysParam Procedure 
PROCEDURE getSysParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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

&IF DEFINED(EXCLUDE-POSData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE POSData Procedure 
PROCEDURE POSData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE INPUT  PARAMETER cFunction     AS CHAR NO-UNDO. 
   DEFINE INPUT  PARAMETER cSearchType   AS CHAR NO-UNDO. 
   DEFINE INPUT  PARAMETER cParameter    AS CHAR NO-UNDO. 

   DEFINE OUTPUT PARAMETER obok      AS LOG NO-UNDO.
   DEFINE OUTPUT PARAMETER ocReturn  AS CHAR NO-UNDO. 

   LOG-MANAGER:WRITE-MESSAGE("POSData."  + cFunction + ":Search " + cSearchType + ":" + cParameter ,"WSERROR").
   RUN VALUE('POSData.' + cFunction) (cSearchType,cParameter, OUTPUT obOk, OUTPUT ocReturn) .
   IF ERROR-STATUS:ERROR THEN
       ocReturn = 'Feil funksjonsnavn eller parametre send til MemberService.' + cFunction + '.'. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-POSData.getMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE POSData.getMember Procedure 
PROCEDURE POSData.getMember :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcSearchType  AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER picParam  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pobOk     AS LOG  NO-UNDO.
DEFINE OUTPUT PARAMETER pocReturn AS CHAR NO-UNDO.

DEFINE VARIABLE hbf AS HANDLE NO-UNDO. 
DEFINE VARIABLE wslOk AS LOG NO-UNDO. 
DEFINE VARIABLE wsRet AS CHAR NO-UNDO. 
DEFINE VARIABLE lMedlemsNr AS INT NO-UNDO.
DEFINE VARIABLE cMKlubbId AS CHAR NO-UNDO. 
DEFINE VARIABLE cKortNr AS CHAR NO-UNDO. 
DEFINE VARIABLE cPostSted AS CHAR NO-UNDO. 

{syspara.i 14 1  7 cMKlubbId}
 
hBf = hParam:DEFAULT-BUFFER-HANDLE.

 IF ipcSearchType = "TelephoneNumber" OR ipcSearchType = "" THEN
     FIND FIRST Medlem NO-LOCK WHERE
               Medlem.MobilTlf = picParam AND 
               medlem.EksterntMedlemsNr NE ""  
               /* AND  
               ( Medlem.EksterntMedlemsNr = "" 
               OR Medlem.EksterntMedlemsNr = ?)*/  NO-ERROR.


 ELSE IF ipcSearchType = "MemberNumber" THEN
     FIND FIRST Medlem NO-LOCK WHERE
                Medlem.MedlemsNr = INTEGER(picParam)   
                NO-ERROR.


 IF AVAILABLE Medlem THEN
 DO:
    cPostSted = "". 
    FIND FIRST post WHERE 
               Post.PostNr      = Medlem.PostNr NO-LOCK NO-ERROR. 
    IF AVAIL post THEN
    cPostSted = post.beskrivelse. 

    IF NOT hbf:AVAIL THEN hbf:BUFFER-CREATE().

    hbf::MedlemsNr         = Medlem.MedlemsNr.
    hbf::Adresse1          = Medlem.Adresse1.
    hbf::Adresse2          = Medlem.Adresse2.
    hbf::ePostAdresse      = Medlem.ePostAdresse.
    hbf::ForNavn           = Medlem.ForNavn.
    hbf::EtterNavn         = Medlem.EtterNavn.
    hbf::PostNr            = Medlem.PostNr.
    hbf::MobilTlf          = Medlem.MobilTlf.
    hbf::Kjonn             = Medlem.Kjonn.
    hbf::EksterntMedlemsNr = Medlem.EksterntMedlemsNr.
    hbf::beskrivelse       = cPostSted.
    hbf::gift              = Medlem.Bonus_Berettiget.
    hbf::FodselsDato       = Medlem.FodselsDato . 


    pobOk = TRUE.  
 END. 
 ELSE 

 DO:
      /* check mot mayfolower hvis ikke på bakrom*/ 
    
     RUN WSMemberService('getMember','MemberId',picParam,OUTPUT wslOk,OUTPUT wsRet).
     
     FIND FIRST getMemberResponseReturn NO-LOCK NO-ERROR. 

     IF AVAIL getMemberResponseReturn THEN
     DO TRANSACTION:
            
         ASSIGN
              cKortNr = LEFT-TRIM(REPLACE(TRIM(getMemberResponseReturn.accountNumber),'M-',''),'0').
              
        FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
        FIND FIRST MedlemsType   NO-LOCK NO-ERROR.
        
        ASSIGN lMedlemsNr = DEC(cKortNr) NO-ERROR.
        
         IF lMedlemsNr = 0 OR CAN-FIND(Medlem WHERE Medlem.MedlemsNr = lMedlemsNr) OR lMedlemsNr = ? THEN 
         DO:
              FIND LAST medlem WHERE medlemsnr NE ? NO-LOCK NO-ERROR.
              IF AVAILABLE Medlem THEN 
                lMedlemsNr = Medlem.MedlemsNr + 1.
              ELSE 
                lMedlemsNr = 1.
         END. 

         FIND medlem WHERE medlem.medlemsnr = lMedlemsNr NO-ERROR. 
         IF NOT AVAIL  medlem THEN 
         DO:
             FIND FIRST medlem WHERE Medlem.EksterntMedlemsNr = getMemberResponseReturn.accountNumber NO-ERROR.
             IF NOT AVAIL medlem THEN
                 CREATE medlem. 
         END. 

         FIND FIRST Post WHERE 
           Post.PostNr = TRIM(getMemberResponseReturn.zip) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Post THEN 
         DO:
             CREATE Post.
             ASSIGN
                 Post.PostNr      = TRIM(getMemberResponseReturn.zip)
                 Post.Beskrivelse = TRIM(getMemberResponseReturn.postarea).
             FIND CURRENT Post NO-LOCK.
         END.
        
         ASSIGN 
             Medlem.MedlemsNr = lMedlemsNr
             Medlem.PersonNr  = getMemberResponseReturn.socialnr
             Medlem.ForNavn   = TRIM(getMemberResponseReturn.firstname)
             Medlem.EtterNavn = TRIM(getMemberResponseReturn.lastname)
             Medlem.PostNr    = TRIM(getMemberResponseReturn.zip)
             Medlem.Adresse2  = ""
             Medlem.Adresse1  = getMemberResponseReturn.address
             Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 0
             Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType ELSE 0
             Medlem.MKlubbId  = INT(cMKlubbId)
             Medlem.ButikkNr  = INT(0)
             Medlem.Kjonn     = CAN-DO('male,man,m',TRIM(getMemberResponseReturn.gender))
             Medlem.EksterntMedlemsNr = getMemberResponseReturn.accountNumber
             Medlem.Kilde        = 'MayFlower'
             Medlem.ePostAdresse = getMemberResponseReturn.email
             Medlem.MobilTlf     = 
                 IF LENGTH(getMemberResponseReturn.mobile) = 12 THEN
                 SUBSTRING(getMemberResponseReturn.mobile,5) ELSE getMemberResponseReturn.mobile
                                   
             Medlem.Telefon      = getMemberResponseReturn.phone . 
             Medlem.Bonus_Berettiget = FALSE. 
             /* Medlem.FodselsDato  = hParameterTable::FodselsDato.*/ 
        
         FIND FIRST  medlemskort WHERE 
              MedlemsKort.KortNr = cKortNr 
              NO-LOCK NO-ERROR.

         IF NOT AVAIL MedlemsKort THEN
         DO:

         CREATE MedlemsKort.
         ASSIGN
             MedlemsKort.MedlemsNr    = Medlem.MedlemsNr
             MedlemsKort.KortNr       = cKortNr
             MedlemsKort.AktivertDato = TODAY 
             MedlemsKort.UtgarDato    = TODAY + 999
             MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
             MedlemsKort.KortType     = 1 NO-ERROR.
         END. 
        
         cPostSted = "". 
     FIND FIRST post WHERE 
                Post.PostNr      = Medlem.PostNr NO-LOCK NO-ERROR. 
     IF AVAIL post THEN
     cPostSted = post.beskrivelse. 

     IF NOT hbf:AVAIL THEN hbf:BUFFER-CREATE().
        hbf::MedlemsNr         = Medlem.MedlemsNr.
        hbf::Adresse1          = Medlem.Adresse1.
        hbf::Adresse2          = Medlem.Adresse2.
        hbf::ePostAdresse      = Medlem.ePostAdresse.
        hbf::ForNavn           = Medlem.ForNavn.
        hbf::EtterNavn         = Medlem.EtterNavn.
        hbf::PostNr            = Medlem.PostNr.
        hbf::MobilTlf          = Medlem.MobilTlf.
        hbf::Kjonn             = Medlem.Kjonn.
        hbf::EksterntMedlemsNr = Medlem.EksterntMedlemsNr.
        hbf::beskrivelse       = cPostSted.
        hbf::gift              = Medlem.Bonus_Berettiget.
        hbf::FodselsDato       = Medlem.FodselsDato . 

        IF AVAILABLE Medlem      THEN RELEASE medlem.
        IF AVAILABLE Medlemskort THEN RELEASE MedlemsKort.
        
     END. /* TRANSACTION */

     MESSAGE "WSmemberservice:" wslok. 
     
 END.
 /*
    pobOk = FALSE. 
 */

 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-POSData.insertMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE POSData.insertMember Procedure 
PROCEDURE POSData.insertMember :
/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes:  
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcSearchType  AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER picParam  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pobOk     AS LOG  NO-UNDO.
DEFINE OUTPUT PARAMETER pocReturn AS CHAR NO-UNDO.

DEFINE VARIABLE cButKlubbListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLengdeListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMKlubbId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMedlemsNr AS INT NO-UNDO. 
DEFINE VARIABLE lMedlemsNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE cKortNr AS CHARACTER NO-UNDO.


FIND FIRST  SysPara NO-LOCK WHERE
            SysPara.SysHId = 14 AND
            SysPara.SysGr  = 1 AND
            SysPara.ParaNr >= 31 AND 
            SysPara.ParaNr <= 39 AND
            CAN-DO(SysPara.Parameter1,hParameterTable::EksterntMedlemsNr) NO-ERROR.

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
  cKortNr = LEFT-TRIM(REPLACE(TRIM(hParameterTable::EksterntMedlemsNr),'M-',''),'0').
  
FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
FIND FIRST MedlemsType   NO-LOCK NO-ERROR.

ASSIGN lMedlemsNr = DEC(cKortNr) NO-ERROR.

IF lMedlemsNr = 0 OR CAN-FIND(Medlem WHERE Medlem.MedlemsNr = lMedlemsNr)
   OR lMedlemsNr = ? THEN 
DO:
  FIND LAST medlem WHERE medlemsnr NE ? NO-LOCK NO-ERROR.
  IF AVAILABLE Medlem THEN 
    lMedlemsNr = Medlem.MedlemsNr + 1.
  ELSE 
    lMedlemsNr = 1.
END.
  

/* usikker på denne koden ...curt */
IF NOT CAN-FIND(
    FIRST Medlem WHERE 
          Medlem.EksterntMedlemsNr = TRIM(hParameterTable::EksterntMedlemsNr)
          AND hParameterTable::EksterntMedlemsNr NE ? ) THEN 


DO TRANSACTION:

        FIND FIRST Post WHERE 
          Post.PostNr = TRIM(hParameterTable::PostNr) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Post THEN 
        DO:
            CREATE Post.
            ASSIGN
                Post.PostNr      = TRIM(hParameterTable::PostNr)
                Post.Beskrivelse = TRIM(hParameterTable::Beskrivelse)
                .
            FIND CURRENT Post NO-LOCK.
        END.
        
        
        CREATE Medlem.
        ASSIGN 
            Medlem.MedlemsNr = lMedlemsNr
            Medlem.PersonNr  = SUBSTRING(TRIM(hParameterTable::PersonNr),3)
            Medlem.ForNavn   = TRIM(hParameterTable::ForNavn)
            Medlem.EtterNavn = TRIM(hParameterTable::EtterNavn)
            Medlem.PostNr    = TRIM(hParameterTable::PostNr)
            Medlem.Adresse2  = TRIM(hParameterTable::Adresse2)
            Medlem.Adresse1  = TRIM(hParameterTable::Adresse1)
            Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 0
            Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType ELSE 0
            Medlem.MKlubbId  = INT(cMKlubbId)
            Medlem.ButikkNr  = INT(hParameterTable::ButikkNr)
            Medlem.Kjonn     = CAN-DO('male,man,m,yes,true',TRIM(hParameterTable::Kjonn))
            Medlem.EksterntMedlemsNr = TRIM(hParameterTable::EksterntMedlemsNr)
            Medlem.Kilde        = 'MayFlower'
            Medlem.ePostAdresse = TRIM(hParameterTable::epostAdresse)
            Medlem.MobilTlf     = TRIM(hParameterTable::MobilTlf)
            Medlem.Telefon      = TRIM(hParameterTable::Telefon).
            Medlem.FodselsDato       = hParameterTable::FodselsDato . 
            Medlem.Bonus_Berettiget  = hParameterTable::gift . 
            

        iMedlemsNr = INT(Medlem.MedlemsNr) .
        CREATE MedlemsKort.
        ASSIGN
            MedlemsKort.MedlemsNr    = Medlem.MedlemsNr
            MedlemsKort.KortNr       = cKortNr
            MedlemsKort.AktivertDato = TODAY 
            MedlemsKort.UtgarDato    = TODAY + 999
            MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
            MedlemsKort.KortType     = 1 .


    IF AVAILABLE Medlem      THEN RELEASE medlem.
    IF AVAILABLE Medlemskort THEN RELEASE MedlemsKort.
END. /* TRANSACTION */
                                                                   

        RUN WSMemberService('InsertMember','','',OUTPUT pobOk,OUTPUT pocReturn).

        IF pobOk  THEN
        DO:
            FIND FIRST insertMemberResponseReturn NO-LOCK NO-ERROR. 
            IF AVAIL insertMemberResponseReturn THEN
            DO TRANSACTION:
                FIND medlem WHERE medlem.medlemsnr = iMedlemsNr NO-ERROR. 
                IF AVAIL medlem THEN
                    medlem.EksterntMedlemsNr = insertMemberResponseReturn.memberId. 
            END.
        END. 
        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saxa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saxa Procedure 
PROCEDURE Saxa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER RawXML AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE SAXReader    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE ResultParser AS HANDLE    NO-UNDO.
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
PROCEDURE StartElement :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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

&IF DEFINED(EXCLUDE-WSCheckService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSCheckService Procedure 
PROCEDURE WSCheckService :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cFunction AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER cParam AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER obok AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO. 

    RUN getSysParam (10).
    RUN VALUE('CheckService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        obOk     = FALSE.
        ocReturn = 'Feil funksjonsnavn eller parametre send til CheckService.' + cFunction + '.'. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSMemberService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSMemberService Procedure 
PROCEDURE WSMemberService :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFunction AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER ipcSearchType  AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER cParam AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER obok AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO. 


   {syspara.i 50 201 1 cUsername}
    IF cUsername = '' THEN
        cUsername = "polygon".
    {syspar2.i 50 201 1 cPassword} 
    IF cPassword = '' THEN
        cPassword = "polygon09PRS".
    {syspara.i 50 201 2 cTekst} 
    IF CAN-DO('1,J,Ja,True,Y',cTekst) THEN
        lLogReq = TRUE.
    ELSE
        lLogReq = FALSE.
    {syspar2.i 50 201 2 cTekst} 
    IF CAN-DO('1,J,Ja,True,Y',cTekst) THEN
        lLogXml = TRUE.
    ELSE
        lLogXml = FALSE.
/*
        MESSAGE cUsername cPassword VIEW-AS ALERT-BOX. 

    cUsername = "gantnosite".
    cPassword = "NewHaven1949".
  */

    CREATE SERVER hWebService.
    
    RUN getSysParam (30).
    
    RUN VALUE('WSMemberService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       ocReturn = 'Feil funksjonsnavn eller parametre send til MemberService.' + cFunction + '.'. 
    

    IF obOk THEN /* Her har connect og oppslag gått bra. */
    DO:
        ASSIGN
          ocReturn = ''.
        
        IF lLogXml THEN RUN bibl_loggDbFri.p ('MayflowerXML','NoPrefix' + '|' + STRING(cResponse)).
        RUN Saxa (cResponse). 
        IF lLogReq THEN RUN bibl_loggDbFri.p ('Mayflower','MemberService.' + cFunction + ' Request: ' + cParam + ' Answ: ' + ocRetParam).
    END.
    
    ELSE IF lSoapFault THEN /* Her har connect gått bra, men oppslag feilet */
    DO:
        ASSIGN
          ocReturn = 'FEIL returnert fra WebService: ' + STRING(lcSoapFault).
        IF lLogXml THEN RUN bibl_loggDbFri.p ('MayflowerXML','NoPrefix' + '|' + replace(STRING(lcSoapFault),'><','>' + CHR(10) + '<') + CHR(13)).
        IF lLogReq THEN RUN bibl_loggDbFri.p ('Mayflower','MemberService.' + cFunction + ' Request: ' + cParam + ' Answ: ' + ocReturn).
    END.
    
    ELSE /* Her feilet connet */
    DO:
        ASSIGN
        obOk     = FALSE.
        /*ocReturn = 'ConERROR: Kan ikke koble mot WebService. Kontroller parametre.'*/
        IF lLogReq THEN RUN bibl_loggDbFri.p ('Mayflower','MemberService.' + cFunction + ' Request: ' + cParam + ' Answ: Feil ved oppkolbing.').
    END.
    

    IF VALID-HANDLE(hFunctionService) THEN DELETE OBJECT hFunctionService. 
    hWebService:DISCONNECT() NO-ERROR.
    IF VALID-HANDLE(hWebService) THEN DELETE OBJECT hWebService.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSMemberService.getAvailableOffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSMemberService.getAvailableOffers Procedure 
PROCEDURE WSMemberService.getAvailableOffers :
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

&IF DEFINED(EXCLUDE-WSMemberService.getMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSMemberService.getMember Procedure 
PROCEDURE WSMemberService.getMember :
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

   /*
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
    */

    cReq = 
    '<ns0:getMember xmlns:ns0="http://abalon.se/memberclub/MemberService">' +
      '<request>' +
        '<memberId>' + cMemberId + '</memberId>' +
        '<memberSearchType>mobilephone</memberSearchType>' +
      '</request>' +
      '<User>' +
        '<username>' + cUsername + '</username>' +
        '<password>' + cPassword + '</password>' +
        '<roleCode>wsuser</roleCode>' +   
      '</User>' +
    '</ns0:getMember>'.

    obOk = TRUE.
    
    /*RUN getMember IN hFunctionService(INPUT cReq, OUTPUT cResponse)  .
      MESSAGE string(cResponse) VIEW-AS ALERT-BOX. */
    
    EMPTY TEMP-TABLE getMemberResponseReturn.
    RUN getMember IN hFunctionService(INPUT cReq, OUTPUT DATASET getMemberResponse) NO-ERROR.
           

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
        obOk = FALSE. 
    END.  
    ELSE 
    DO:
        FIND FIRST getMemberResponseReturn NO-LOCK NO-ERROR. 
        MESSAGE getMemberResponseReturn.birthdate VIEW-AS ALERT-BOX. 
        obOk = TRUE.
    END. 

END. /* OPPKOBLET */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSMemberService.insertMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSMemberService.insertMember Procedure 
PROCEDURE WSMemberService.insertMember :
/*------------------------------------------------------------------------------
Purpose:                                                                                                                                          
Notes:                  
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER icParam  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER obOk     AS LOG  INIT TRUE NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO.


DEFINE VARIABLE cInput AS LONGCHAR NO-UNDO.

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
    
    obOk = TRUE.
                                                
    cInput = getInsertMemberRequest
                              ( hParameterTable::ForNavn,
                                hParameterTable::EtterNavn,
                                hParameterTable::PersonNr,
                                hParameterTable::FodselsDato,
                                hParameterTable::ePostAdresse,
                                hParameterTable::MobilTlf,
                                hParameterTable::Kjonn,
                                hParameterTable::Adresse1,
                                hParameterTable::PostNr,
                                hParameterTable::Beskrivelse,
                                hParameterTable::ButikkNr,
                                hParameterTable::RegistrertButikkNr,
                                hParameterTable::land,
                                hParameterTable::Valuta,
                                cUsername,
                                cPassword).

    IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN 
        COPY-LOB cInput TO FILE "WS_MayFlower_" + STRING(ETIME) + "Request_InsertMember.xml". 

    EMPTY TEMP-TABLE insertMemberResponseReturn. 
    RUN insertMember IN hFunctionService(INPUT cInput,OUTPUT DATASET insertMemberResponse) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        IF ERROR-STATUS:ERROR-OBJECT-DETAIL NE ? THEN
        DO:  
            hSoapFaultDetail = ERROR-STATUS:ERROR-OBJECT-DETAIL:SOAP-FAULT-DETAIL.
            IF VALID-HANDLE(hSoapFaultDetail) THEN
                lcSoapFault = hSoapFaultDetail:GET-SERIALIZED().
            lSoapFault = TRUE.

           DATASET detail:READ-XML("longchar",lcSoapFault,"empty","", ?, ?, ?) NO-ERROR.
           FIND FIRST memberservicefault . 
           IF AVAIL memberservicefault THEN 
           DO:
               MemberServiceFault.userfaultmessage = LEFT-TRIM(TRIM(ENTRY(2,MemberServiceFault.faultmessage,":"))). 
               gcErrorMessage =  MemberServiceFault.userfaultmessage. 
               ocReturn = MemberServiceFault.userfaultmessage.
           END. 

            IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN 
            DO:
                LOG-MANAGER:WRITE-MESSAGE(STRING(ocReturn),"WSERROR").
                LOG-MANAGER:WRITE-MESSAGE(STRING(lcSoapFault),"WSERROR").
            END. 
            
        END.
        obOk = FALSE. 
    END.              
    

    FIND FIRST insertMemberResponseReturn NO-LOCK NO-ERROR. 
    IF AVAIL insertMemberResponseReturn THEN
    DO:
        LOG-MANAGER:WRITE-MESSAGE (insertMemberResponseReturn.memberId,"memberId").
        LOG-MANAGER:WRITE-MESSAGE (insertMemberResponseReturn.cardNumber,"cardNumber").
        LOG-MANAGER:WRITE-MESSAGE (insertMemberResponseReturn.password,"password").
        LOG-MANAGER:WRITE-MESSAGE (insertMemberResponseReturn.webcode,"webcode").
    END. 

     /*
    IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN 
        COPY-LOB cResponse TO FILE "WS_MayFlower_" + STRING(ETIME) + "Response_InsertMember.xml". 
    */

END. /* OPPKOBLET */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSMemberService.isMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSMemberService.isMember Procedure 
PROCEDURE WSMemberService.isMember :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER picParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pobOk     AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER pocReturn AS CHAR NO-UNDO.

    IF AVAILABLE Medlem THEN RELEASE Medlem.
    
    ASSIGN
       cMemberId  = ENTRY(1,picParam,'|').
    IF NUM-ENTRIES(picParam,'|') >= 2 THEN cBAR_CODE = ENTRY(2,picParam,'|').
    
    MESSAGE cMemberId VIEW-AS ALERT-BOX. 

    hWebService:CONNECT("-WSDL " + cWsdlFil + ' ' + WsdlParam) NO-ERROR.
    
    IF NOT hWebService:CONNECTED() THEN 
        ocReturn = 'Feil ved lasting av WSDL fil og oppkobling mot webservice. Sjekk systemparameter som peker på WSDL filen.' + 
                   "-WSDL " + cWsdlFil + ' ' + WsdlParam + '.'.
    ELSE 
    OPPKOBLET:
    DO:
        /*
        CASE cSearchType:
            /* Slår på medlem med mobilnr for å finne medlemsnr. */
            WHEN 'mobile' THEN
            DO:
              FIND FIRST Medlem NO-LOCK WHERE
                  Medlem.MobilTlf = cMemberId NO-ERROR.
              IF AVAILABLE Medlem THEN
                  cMemberId = Medlem.EksterntMedlemsNr.
            END.
            WHEN 'eMail' THEN
            DO:
              FIND FIRST Medlem NO-LOCK WHERE
                  Medlem.ePostAdresse = cMemberId NO-ERROR.
              IF AVAILABLE Medlem THEN
                  cMemberId = Medlem.EksterntMedlemsNr.
            END.
            WHEN 'MemberId' THEN
            DO:
              FIND FIRST Medlem NO-LOCK WHERE
                  Medlem.EksterntMedlemsNr = cMemberId NO-ERROR.
            END.
        END CASE.
        */

        /* Det er Mayflower som er master. Derfor gjør vi alltid oppslag. */
        RUN MemberService SET hFunctionService ON hWebService NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            pocReturn = 'Feil ved oppstart av service MemberService.'.
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
    
        IF lLogXml THEN RUN bibl_loggDbFri.p ('MayflowerXML','NoPrefix' + '|' + cReq + CHR(13)).
    
        cInput = cReq. 
    
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
            pobOk = FALSE. 
        END.
        ELSE pobOk = TRUE.
    
        MESSAGE STRING(cResponse) VIEW-AS ALERT-BOX. 

    END. /* OPPKOBLET */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSMemberService.updateMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSMemberService.updateMember Procedure 
PROCEDURE WSMemberService.updateMember :
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

&IF DEFINED(EXCLUDE-WSOfferService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSOfferService Procedure 
PROCEDURE WSOfferService :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cFunction AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER cParam AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER obok AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO. 

    
    RUN getSysParam (30).
    RUN VALUE('OfferService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        obOk     = FALSE.
        ocReturn = 'Feil funksjonsnavn eller parametre send til OfferService.' + cFunction + '.'. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSTransactionService) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSTransactionService Procedure 
PROCEDURE WSTransactionService :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cFunction AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER cParam AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER obok AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER ocReturn AS CHAR NO-UNDO. 

            RUN getSysParam (20).
        RUN VALUE('transactionService.' + cFunction) (cParam, OUTPUT obOk, OUTPUT ocReturn) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            obOk     = FALSE.
            ocReturn = 'Feil funksjonsnavn eller parametre send til transactionService.' + cFunction + '.'. 
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSTransactionService.insertTransaction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSTransactionService.insertTransaction Procedure 
PROCEDURE WSTransactionService.insertTransaction :
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

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getInsertMemberRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInsertMemberRequest Procedure 
FUNCTION getInsertMemberRequest RETURNS LONGCHAR (
         INPUT firstName AS CHAR,
         INPUT lastName AS CHAR,
         INPUT socialnr AS CHAR,
         INPUT birthDate AS DATE, 
         INPUT email AS CHAR, 
         INPUT mobile AS CHAR ,
         INPUT gender AS CHAR,
         INPUT address AS CHAR,
         INPUT zip AS CHAR ,
         INPUT postarea AS CHAR,
         INPUT primaryStore AS INT ,
         INPUT registerStore AS INT, 
         INPUT countrycode AS CHAR,
         INPUT currencyCode AS CHAR,
         INPUT username AS CHAR,
         INPUT password AS CHAR):

    /* default values */ 
    gender = IF gender = "mann"   THEN "male"   ELSE 
             IF gender = "kvinne" THEN "female" ELSE
             IF gender = "1"      THEN "male"   ELSE 
             IF gender = "2"      THEN "female" ELSE 
             IF gender = "m"      THEN "male"   ELSE 
             IF gender = "k"      THEN "female" ELSE 
             IF gender = "f"      THEN "female" ELSE 
             IF gender = "no"     THEN "female" ELSE 
             IF gender = "yes"    THEN "male"   ELSE 
             IF gender = "true"   THEN "male"   ELSE 
             IF gender = "false"  THEN "female" ELSE "unknown". 

    currencyCode = IF currencycode = "" THEN "nok" ELSE currencycode. 
    CountryCode  = IF CountryCode  = "" THEN "no"  ELSE CountryCode. 

    DEFINE VARIABLE hWriter AS HANDLE NO-UNDO.
    DEFINE VARIABLE lcxml AS LONGCHAR NO-UNDO. 
    DEFINE VARIABLE cPrefix AS CHAR INIT "ns0" NO-UNDO.
    DEFINE VARIABLE cReq AS LONGCHAR NO-UNDO. 
    
    CREATE SAX-WRITER hWriter.
    hWriter:FRAGMENT = TRUE. 
    hWriter:FORMATTED = TRUE. 
    
    hWriter:SET-OUTPUT-DESTINATION("LONGCHAR",lcxml).
    hWriter:START-DOCUMENT().
    
    hWriter:START-ELEMENT(cPrefix + ":" + 'insertMember').
    hWriter:DECLARE-NAMESPACE("http://abalon.se/memberclub/MemberService",cPrefix).
    
    /* Request */ 
    hWriter:START-ELEMENT("request").
        IF firstname NE '' THEN
        DO:
            hWriter:START-ELEMENT("firstName").
            hWriter:WRITE-CHARACTERS(firstName).
            hWriter:END-ELEMENT("firstName").
        END. 
        
        IF lastname NE '' THEN
        DO:
            hWriter:START-ELEMENT("lastName").
            hWriter:WRITE-CHARACTERS(lastName).
            hWriter:END-ELEMENT("lastName").
        END.
        
        IF socialnr NE '' THEN
        DO:
            hWriter:START-ELEMENT("socialnr").
            hWriter:WRITE-CHARACTERS(socialnr).
            hWriter:END-ELEMENT("socialnr").
        END. 

        IF birthDate NE ? THEN
        DO:
            hWriter:START-ELEMENT("birthDate").
            hWriter:WRITE-CHARACTERS(string(YEAR(birthDate),"9999") + "-" + string(MONTH(birthDate),"99") + "-" + STRING(DAY(birthDate),"99")).
            hWriter:END-ELEMENT("birthDate").
        END. 

        IF email NE '' THEN
        DO:
            hWriter:START-ELEMENT("email").
            hWriter:WRITE-CHARACTERS(email).
            hWriter:END-ELEMENT("email").
        END. 

        IF mobile NE '' THEN
        DO:
            IF LENGTH(mobile) = 8 THEN mobile = "0047" + mobile.
            hWriter:START-ELEMENT("mobile").
            hWriter:WRITE-CHARACTERS(TRIM(mobile)).
            hWriter:END-ELEMENT("mobile").
        END. 

        IF gender NE '' THEN
        DO:
            hWriter:START-ELEMENT("gender").
            hWriter:WRITE-CHARACTERS(gender).
            hWriter:END-ELEMENT("gender").
        END.

        IF address NE '' THEN
        DO:
            hWriter:START-ELEMENT("address").
            hWriter:WRITE-CHARACTERS(STRING(address)).
            hWriter:END-ELEMENT("address").
        END. 

        IF zip NE '' THEN
        DO:
            hWriter:START-ELEMENT("zip").
            hWriter:WRITE-CHARACTERS(STRING(zip)).
            hWriter:END-ELEMENT("zip").
        END. 

        IF postarea NE '' THEN
        DO:
            hWriter:START-ELEMENT("postarea").
            hWriter:WRITE-CHARACTERS(STRING(postarea)).
            hWriter:END-ELEMENT("postarea").
        END. 
        
        
        IF countryCode NE "" THEN
        DO:
            hWriter:START-ELEMENT("countryCode").
            hWriter:WRITE-CHARACTERS(countryCode).
            hWriter:END-ELEMENT("countryCode").
        END. 

        IF CurrencyCode NE "" THEN
        DO:
            hWriter:START-ELEMENT("CurrencyCode").
            hWriter:WRITE-CHARACTERS(CurrencyCode).
            hWriter:END-ELEMENT("CurrencyCode").
        END. 

        IF primaryStore NE ? THEN
        DO:
            hWriter:START-ELEMENT("primaryStore").
            hWriter:WRITE-CHARACTERS(STRING(primaryStore)).
            hWriter:END-ELEMENT("primaryStore").
        END. 

        IF registerStore NE ? THEN
        DO:
            hWriter:START-ELEMENT("registerStore").
            hWriter:WRITE-CHARACTERS(STRING(registerStore)).
            hWriter:END-ELEMENT("registerStore").
        END. 


    hWriter:END-ELEMENT("request").

    /* User */ 
    hWriter:START-ELEMENT("User").
        hWriter:START-ELEMENT("username").
        hWriter:WRITE-CHARACTERS(UserName).
        hWriter:END-ELEMENT("username").
    
        hWriter:START-ELEMENT("password").
        hWriter:WRITE-CHARACTERS(Password).
        hWriter:END-ELEMENT("password").
    
        hWriter:START-ELEMENT("roleCode").
        hWriter:WRITE-CHARACTERS('wsuser').
        hWriter:END-ELEMENT("roleCode").
    hWriter:END-ELEMENT("User").
    
    hWriter:END-ELEMENT(cPrefix + ":" + 'insertMember').
    hWriter:END-DOCUMENT().
    DELETE OBJECT hWriter. 
    
    cReq = lcxml. 
 
    RETURN cReq. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

