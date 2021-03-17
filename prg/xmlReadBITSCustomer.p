&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xmlReadBITSCustomer
    Purpose     : Leser inn kunder fra BITS nettbutikk ePages.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 13/5-2009
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* DEFINE INPUT  PARAMETER cFilename AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE cFilename AS CHARACTER  NO-UNDO. */

DEFINE INPUT  PARAMETER cFileName AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hDoc            AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev1           AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev1Fields     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev1FieldValue AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev2           AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev2Fields     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev2FieldValue AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev3           AS HANDLE NO-UNDO.
DEFINE VARIABLE hLev3Fields     AS HANDLE NO-UNDO.
DEFINE VARIABLE hLev3FieldValue AS HANDLE NO-UNDO.
DEFINE VARIABLE hLev4           AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev4Fields     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hLev4FieldValue AS HANDLE  NO-UNDO.

DEFINE VARIABLE iCL AS INTEGER NO-UNDO.

CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF  hLev1.
CREATE X-NODEREF  hLev1Fields.
CREATE X-NODEREF  hLev1FieldValue.
CREATE X-NODEREF  hLev2.
CREATE X-NODEREF  hLev2Fields.
CREATE X-NODEREF  hLev2FieldValue.
CREATE X-NODEREF  hLev3.
CREATE X-NODEREF  hLev3Fields.
CREATE X-NODEREF  hLev3FieldValue.
CREATE X-NODEREF  hLev4.
CREATE X-NODEREF  hLev4Fields.
CREATE X-NODEREF  hLev4FieldValue.

{xinnBITSCustomer.i &NEW=NEW}

DEFINE VARIABLE iKndGruppeId AS INTEGER FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE iKndTypeId   AS INTEGER FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE iMedGruppeId AS INTEGER FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE iMedTypeId   AS INTEGER FORMAT ">>>9" NO-UNDO.
DEFINE VARIABLE iStdMKlubbId AS INTEGER NO-UNDO.
DEFINE VARIABLE bSvenskFormat AS LOG NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMKlubbId    AS CHARACTER NO-UNDO. 
 
DEFINE VARIABLE cMKortNr      AS CHAR      FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cBongTekst    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cNavn         AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cStatus       AS CHAR      FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOK           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cMelding      AS CHARACTER FORMAT "x(40)" NO-UNDO.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDato Procedure 
FUNCTION getDato RETURNS DATE
    ( INPUT cYYYYMMDD AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTid Procedure 
FUNCTION getTid RETURNS INTEGER
    ( INPUT cHHMMSS AS CHARACTER )  FORWARD.

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
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.71
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE cRet_val AS CHARACTER NO-UNDO.

/* Returnerer hvis det ikke er en xml fil som skal leses. */
IF NOT ENTRY(NUM-ENTRIES(cFileName,"."),cFileName,".") = "xml" THEN
    RETURN 'ERROR'.
    
{syspara.i 5 1 1 iCL int}

{syspara.i 150 1 16 cTekst}
IF CAN-DO("1,J,Ja,Yes,True",cTekst) THEN 
  bSvenskFormat = TRUE.
      
{syspara.i 150 1 12 iKndGruppeId int}   
{syspar2.i 150 1 12 iKndTypeId int}
    
{syspara.i 150 1 13 iMedGruppeId int}   
{syspar2.i 150 1 13 iMedTypeId int}

{syspara.i 150 1 14 iStdMKlubbId int}

/* Tømmer gamle og oppretter nye tomme temp-tabeller. */
RUN PrepTempTabell.

/* Oppretter temp-tabell for å kunne postere direkte i dem ved innlesning fra xml fil. */
CREATE TT_Customer.
CREATE TT_BillingAddress.
CREATE TT_ShippingAdress.
CREATE TT_User.
CREATE TT_UserBillingAddress.
CREATE TT_UserShippingAdress.      
    
/* Leser inn data fra xml fil og posterer dem i temp-tabell. */
RUN LesInnFil.
/* OUTPUT TO "c:\tmp\cust.txt".                     */
/*     DISPLAY TT_Customer WITH 11 COL SIDE-LABELS. */
/* OUTPUT CLOSE.                                    */

/* Er det kommet inn informasjon på noen av postene, skal det oppdateres */
IF cRet_Val <> 'ERROR' AND 
    (TT_Customer.Oppdatert OR 
    TT_BillingAddress.Oppdatert OR 
    TT_ShippingAdress.Oppdatert OR 
    TT_User.Oppdatert OR 
    TT_UserBillingAddress.Oppdatert OR
    TT_UserShippingAdress.Oppdatert) THEN 
    RUN posterKundeData.           

/* Rydder i memory */
RUN clearMemory.

IF cRet_val = "ERROR" THEN 
    RETURN 'ERROR'.
ELSE 
    RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

 
 
&IF DEFINED(EXCLUDE-clearMemory) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearMemory Procedure
PROCEDURE clearMemory:

    /*------------------------------------------------------------------------------
            Purpose:  																	  
            Notes:  																	  
    ------------------------------------------------------------------------------*/

    /* Rydder i memory */
    DELETE OBJECT hLev1 NO-ERROR.
    DELETE OBJECT hLev1Fields NO-ERROR.
    DELETE OBJECT hLev1FieldValue NO-ERROR.
    DELETE OBJECT hLev2 NO-ERROR.
    DELETE OBJECT hLev2Fields NO-ERROR.
    DELETE OBJECT hLev2FieldValue NO-ERROR.
    DELETE OBJECT hLev3 NO-ERROR.
    DELETE OBJECT hLev3Fields NO-ERROR.
    DELETE OBJECT hLev3FieldValue NO-ERROR.
    DELETE OBJECT hLev4 NO-ERROR.
    DELETE OBJECT hLev4Fields NO-ERROR.
    DELETE OBJECT hLev4FieldValue NO-ERROR.
    DELETE OBJECT hDoc NO-ERROR.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil PRIVATE :
    DEFINE VARIABLE cName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoop1 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLoop2 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLoop3 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLoop4 AS INTEGER   NO-UNDO.
    DEF    VAR      lOk    AS LOGICAL   NO-UNDO.

    /* Leser inn xml filen */
    lOk = hDoc:LOAD ("file",cFileName, FALSE) /*NO-ERROR*/.
    IF lOK = FALSE THEN 
    DO:
        RETURN "ERROR".
    END.
    
    IF NOT hDoc:GET-DOCUMENT-ELEMENT (hLev1) THEN
        RETURN "ERROR".
    
    /* Sjekker at det er riktig nodenavn */    
    cName = hLev1:NAME NO-ERROR.
    IF cName <> "Customer" THEN
        RETURN "ERROR".
        
    /* Her traverseres alle noder og felt. Det ligger en ytre loop, med flere nivåer av looper inni for */
    /* å kunne gå ned i treet og hente ut felt og verdier.                                              */
    /* Under travasering posteres det i temp-tabellene. De oppdaterte temp-tabellene behandles etter    */
    /* repeat loopen.                                                                                   */  
    REPEAT iLoop1 = 1 TO hLev1:NUM-CHILDREN:
        lOK = hLev1:GET-CHILD (hLev1Fields,iLoop1) NO-ERROR. /* Alla element til Customer */
        IF NOT lOK THEN NEXT.
 
        /* -------- Felt i Customer Node -------- */
        IF hLev1Fields:NUM-CHILDREN = 1 THEN
        DO:
            IF CAN-DO('path,alias,customerGroup,taxArea,taxModel,isHtmlEMailAllowed,isDoOrderAllowed,comment',hLev1Fields:NAME) THEN 
            DO:
                lOK = hLev1Fields:GET-CHILD (hLev1FieldValue,1) NO-ERROR.
                tt_Customer.Oppdatert = TRUE. /* Flagger at det har skjedd oppdatering på posten */
                IF lOK THEN 
                CASE hLev1Fields:NAME:
                    WHEN 'path'               THEN tt_Customer.cPath = hLev1FieldValue:NODE-VALUE.
                    WHEN 'ALIAS'              THEN ASSIGN
                                                     tt_Customer.cAlias  = hLev1FieldValue:NODE-VALUE
                                                     tt_Customer.KundeNr = hLev1FieldValue:NODE-VALUE
                                                     TT_BillingAddress.KundeNr = hLev1FieldValue:NODE-VALUE
                                                     TT_ShippingAdress.KundeNr = hLev1FieldValue:NODE-VALUE
                                                     TT_User.KundeNr = hLev1FieldValue:NODE-VALUE
                                                     TT_UserBillingAddress.KundeNr = hLev1FieldValue:NODE-VALUE
                                                     TT_UserShippingAdress.KundeNr = hLev1FieldValue:NODE-VALUE           
                                                     .
                    WHEN 'customerGroup'      THEN tt_Customer.customerGroup = hLev1FieldValue:NODE-VALUE.
                    WHEN 'taxArea'            THEN tt_Customer.taxArea = hLev1FieldValue:NODE-VALUE.
                    WHEN 'taxModel'           THEN tt_Customer.taxModel = hLev1FieldValue:NODE-VALUE.
                    WHEN 'isHtmlEMailAllowed' THEN tt_Customer.isHtmlEMailAllowed = hLev1FieldValue:NODE-VALUE.
                    WHEN 'isDoOrderAllowed'   THEN tt_Customer.isDoOrderAllowed = hLev1FieldValue:NODE-VALUE.
                    WHEN 'comment'            THEN tt_Customer.comment = hLev1FieldValue:NODE-VALUE.
                END CASE.
            END.
        END.
        /* Her håndterer neste nivå av noder. Feltet er nå node. */
        ELSE IF hLev1Fields:NUM-CHILDREN > 1 THEN 
            DO:
                CASE hLev1Fields:NAME:
                    /* -------- Node for faktureringsadresse på kunde --------- */
                    WHEN 'billingAddress' THEN 
                        DO:
                            REPEAT iLoop2 = 1 TO hLev1Fields:NUM-CHILDREN:                        
                                lOK = hLev1Fields:GET-CHILD (hLev2Fields,iLoop2) NO-ERROR.
                                IF NOT lOK THEN NEXT.
                                IF CAN-DO('alias,displayName,street,zipcode,city,state,countryID,EMail,phone,fax,salutation,title,firstName,middleName,lastName,EMailPrivate,EMailBusiness,phonePrivate,phoneBusiness,phoneCell,gender,company,department,jobTitle,birthday,VATID,MottaeMailUtsendelser,MedlemsKlubb,bankCode,bankName,bankAccountNo,URL',hLev2Fields:NAME) THEN 
                                DO:
                                    lOK = hLev2Fields:GET-CHILD (hLev2FieldValue,1) NO-ERROR.
                                    TT_BillingAddress.Oppdatert = TRUE. /* Flagger at det har skjedd oppdatering på posten */
                                    IF lOK THEN 
                                    CASE hLev2Fields:NAME:
                                        WHEN 'alias'         THEN TT_BillingAddress.cAlias = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'displayName'   THEN TT_BillingAddress.displayName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'street'        THEN TT_BillingAddress.street = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'zipcode'       THEN TT_BillingAddress.zipcode = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'city'          THEN TT_BillingAddress.city = hLev2FieldValue:NODE-VALUE.         
                                        WHEN 'state'         THEN TT_BillingAddress.state = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'countryID'     THEN TT_BillingAddress.countryID = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'EMail'         THEN TT_BillingAddress.EMail = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phone'         THEN TT_BillingAddress.phone = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'fax'           THEN TT_BillingAddress.fax = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'salutation'    THEN TT_BillingAddress.salutation = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'TITLE'         THEN TT_BillingAddress.cTITLE = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'firstName'     THEN TT_BillingAddress.firstName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'middleName'    THEN TT_BillingAddress.middleName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'lastName'      THEN TT_BillingAddress.lastName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'EMailPrivate'  THEN TT_BillingAddress.EMailPrivate = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'EMailBusiness' THEN TT_BillingAddress.EMailBusiness = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phonePrivate'  THEN TT_BillingAddress.phonePrivate = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phoneBusiness' THEN TT_BillingAddress.phoneBusiness = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phoneCell'     THEN TT_BillingAddress.phoneCell = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'gender'        THEN TT_BillingAddress.gender = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'company'       THEN TT_BillingAddress.company = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'department'    THEN TT_BillingAddress.department = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'jobTitle'      THEN TT_BillingAddress.jobTitle = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'birthday'      THEN TT_BillingAddress.birthday = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'VATID'         THEN TT_BillingAddress.VATID = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'MottaeMailUtsendelser' THEN TT_BillingAddress.MottaeMailUtsendelser = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'MedlemsKlubb'  THEN TT_BillingAddress.MedlemsKlubb = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'bankCode'      THEN TT_BillingAddress.bankCode = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'bankName'      THEN TT_BillingAddress.bankName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'bankAccountNo' THEN TT_BillingAddress.bankAccountNo = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'URL'           THEN TT_BillingAddress.cURL = hLev2FieldValue:NODE-VALUE.
                                    END CASE.
                                END.
                            END. /* Repeat */
                        END.                                                      
                    /* -------- Node for leveringsadresse på kunde --------- */
                    WHEN 'shippingAddress' THEN 
                        DO:
                            REPEAT iLoop2 = 1 TO hLev1Fields:NUM-CHILDREN:                        
                                lOK = hLev1Fields:GET-CHILD (hLev2Fields,iLoop2) NO-ERROR.
                                IF NOT lOK THEN NEXT.
                                IF CAN-DO('alias,displayName,street,zipcode,city,state,countryID,EMail,phone,fax,salutation,title,firstName,middleName,lastName,EMailPrivate,EMailBusiness,phonePrivate,phoneBusiness,phoneCell,gender,company,department,jobTitle,birthday,VATID,bankCode,bankName,bankAccountNo,URL',hLev2Fields:NAME) THEN 
                                DO:
                                    lOK = hLev2Fields:GET-CHILD (hLev2FieldValue,1) NO-ERROR.
                                    TT_ShippingAdress.Oppdatert = TRUE. /* Flagger at det har skjedd oppdatering på posten */
                                    IF lOK THEN 
                                    CASE hLev2Fields:NAME:
                                        WHEN 'alias'         THEN TT_ShippingAdress.cAlias = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'displayName'   THEN TT_ShippingAdress.displayName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'street'        THEN TT_ShippingAdress.street = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'zipcode'       THEN TT_ShippingAdress.zipcode = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'city'          THEN TT_ShippingAdress.city = hLev2FieldValue:NODE-VALUE.         
                                        WHEN 'state'         THEN TT_ShippingAdress.state = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'countryID'     THEN TT_ShippingAdress.countryID = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'EMail'         THEN TT_ShippingAdress.EMail = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phone'         THEN TT_ShippingAdress.phone = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'fax'           THEN TT_ShippingAdress.fax = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'salutation'    THEN TT_ShippingAdress.salutation = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'TITLE'         THEN TT_ShippingAdress.cTITLE = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'firstName'     THEN TT_ShippingAdress.firstName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'middleName'    THEN TT_ShippingAdress.middleName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'lastName'      THEN TT_ShippingAdress.lastName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'EMailPrivate'  THEN TT_ShippingAdress.EMailPrivate = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'EMailBusiness' THEN TT_ShippingAdress.EMailBusiness = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phonePrivate'  THEN TT_ShippingAdress.phonePrivate = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phoneBusiness' THEN TT_ShippingAdress.phoneBusiness = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'phoneCell'     THEN TT_ShippingAdress.phoneCell = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'gender'        THEN TT_ShippingAdress.gender = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'company'       THEN TT_ShippingAdress.company = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'department'    THEN TT_ShippingAdress.department = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'jobTitle'      THEN TT_ShippingAdress.jobTitle = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'birthday'      THEN TT_ShippingAdress.birthday = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'VATID'         THEN TT_ShippingAdress.VATID = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'bankCode'      THEN TT_ShippingAdress.bankCode = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'bankName'      THEN TT_ShippingAdress.bankName = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'bankAccountNo' THEN TT_ShippingAdress.bankAccountNo = hLev2FieldValue:NODE-VALUE.
                                        WHEN 'URL'           THEN TT_ShippingAdress.URL = hLev2FieldValue:NODE-VALUE.
                                    END CASE.
                                END.
                            END. /* Repeat */        
                        END.
                    /* ---------- Node for brukere hos kunde ---------- */
                    /* Denne noden har ingen felt liggende på nodenivå. */
                    /* Det ligger bare en ny node - 'user'.             */
                    WHEN 'users' THEN 
                        DO:
                            /* Benytter repeat, men det er bare et felt her, som er en node. */
                            REPEAT iLoop2 = 1 TO hLev1Fields:NUM-CHILDREN:                        
                                lOK = hLev1Fields:GET-CHILD (hLev2Fields,iLoop2) NO-ERROR.
                                IF NOT lOK THEN NEXT.
                                IF CAN-DO('user',hLev2Fields:NAME) THEN 
                                DO:
                                    /* Looper igjennom feltene på User */
                                    REPEAT iLoop3 = 1 TO hLev2Fields:NUM-CHILDREN:
                                        lOK = hLev2Fields:GET-CHILD (hLev3Fields,iLoop3) NO-ERROR.
                                        IF NOT lOK THEN NEXT.
                                        /* Her behandles feltene på user. */
                                        IF hLev3Fields:NUM-CHILDREN = 1 THEN 
                                        DO:
                                            IF CAN-DO('path,alias,localeID,languageCode,currencyID,isActivated,deleteConfirmation,isLoginCookieAllowed,isHtmlEMailAllowed,reminderQuestion,challengePhrase,EMail,name',hLev3Fields:NAME) THEN 
                                            DO:
                                                lOK = hLev3Fields:GET-CHILD (hLev3FieldValue,1) NO-ERROR.
                                                TT_User.Oppdatert = TRUE. /* Flagger at det har skjedd oppdatering på posten */
                                                IF lOK THEN 
                                                CASE hLev3Fields:NAME:
                                                    WHEN 'path'                 THEN TT_User.cpath = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'alias'                THEN TT_User.cAlias = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'localeID'             THEN TT_User.localeID = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'languageCode'         THEN TT_User.languageCode = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'currencyID'           THEN TT_User.currencyID = hLev3FieldValue:NODE-VALUE.         
                                                    WHEN 'isActivated'          THEN TT_User.isActivated = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'deleteConfirmation'   THEN TT_User.deleteConfirmation = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'isLoginCookieAllowed' THEN TT_User.isLoginCookieAllowed = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'isHtmlEMailAllowed'   THEN TT_User.isHtmlEMailAllowed = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'reminderQuestion'     THEN TT_User.reminderQuestion = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'challengePhrase'      THEN TT_User.challengePhrase = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'EMail'                THEN TT_User.EMail = hLev3FieldValue:NODE-VALUE.
                                                    WHEN 'name'                 THEN TT_User.UserName = hLev3FieldValue:NODE-VALUE.
                                                END CASE.
                                            END. 
                                        END.
                                        /* Her behandles nodene på user. */
                                        ELSE IF hLev3Fields:NUM-CHILDREN > 1 THEN
                                        DO: 
                                            lOK = hLev3Fields:GET-CHILD (hLev3FieldValue,1) NO-ERROR.
                                            IF lOK THEN 
                                            CASE hLev3Fields:NAME:
                                                WHEN 'billingAddress' THEN 
                                                    DO:
                                                        REPEAT iLoop4 = 1 TO hLev3Fields:NUM-CHILDREN:   
                                                            lOK = hLev3Fields:GET-CHILD (hLev4Fields,iLoop4) NO-ERROR.
                                                            IF NOT lOK THEN NEXT.
                                                            IF CAN-DO('alias,displayName,street,zipcode,city,state,countryID,EMail,phone,fax,salutation,title,firstName,middleName,lastName,EMailPrivate,EMailBusiness,phonePrivate,phoneBusiness,phoneCell,gender,company,department,jobTitle,birthday,VATID,bankCode,bankName,bankAccountNo,URL',hLev4Fields:NAME) THEN 
                                                            DO:
                                                                lOK = hLev4Fields:GET-CHILD (hLev4FieldValue,1) NO-ERROR.
                                                                TT_UserBillingAddress.Oppdatert = TRUE. /* Flagger at det har skjedd oppdatering på posten */
                                                                IF lOk THEN 
                                                                CASE hLev4Fields:NAME:
                                                                    WHEN 'alias'         THEN TT_UserBillingAddress.cAlias = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'displayName'   THEN TT_UserBillingAddress.displayName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'street'        THEN TT_UserBillingAddress.street = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'zipcode'       THEN TT_UserBillingAddress.zipcode = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'city'          THEN TT_UserBillingAddress.city = hLev4FieldValue:NODE-VALUE.         
                                                                    WHEN 'state'         THEN TT_UserBillingAddress.state = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'countryID'     THEN TT_UserBillingAddress.countryID = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'EMail'         THEN TT_UserBillingAddress.EMail = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phone'         THEN TT_UserBillingAddress.phone = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'fax'           THEN TT_UserBillingAddress.fax = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'salutation'    THEN TT_UserBillingAddress.salutation = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'TITLE'         THEN TT_UserBillingAddress.cTITLE = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'firstName'     THEN TT_UserBillingAddress.firstName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'middleName'    THEN TT_UserBillingAddress.middleName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'lastName'      THEN TT_UserBillingAddress.lastName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'EMailPrivate'  THEN TT_UserBillingAddress.EMailPrivate = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'EMailBusiness' THEN TT_UserBillingAddress.EMailBusiness = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phonePrivate'  THEN TT_UserBillingAddress.phonePrivate = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phoneBusiness' THEN TT_UserBillingAddress.phoneBusiness = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phoneCell'     THEN TT_UserBillingAddress.phoneCell = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'gender'        THEN TT_UserBillingAddress.gender = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'company'       THEN TT_UserBillingAddress.company = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'department'    THEN TT_UserBillingAddress.department = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'jobTitle'      THEN TT_UserBillingAddress.jobTitle = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'birthday'      THEN TT_UserBillingAddress.birthday = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'VATID'         THEN TT_UserBillingAddress.VATID = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'bankCode'      THEN TT_UserBillingAddress.bankCode = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'bankName'      THEN TT_UserBillingAddress.bankName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'bankAccountNo' THEN TT_UserBillingAddress.bankAccountNo = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'URL'           THEN TT_UserBillingAddress.cURL = hLev4FieldValue:NODE-VALUE.
                                                                END CASE.
                                                            END.
                                                        END. /* Repeat */
                                                    END.
                                                WHEN 'shippingAddress' THEN
                                                    DO:
                                                        REPEAT iLoop4 = 1 TO hLev3Fields:NUM-CHILDREN:                        
                                                            lOK = hLev3Fields:GET-CHILD (hLev4Fields,iLoop4) NO-ERROR.
                                                            IF NOT lOK THEN NEXT.
                                                            IF CAN-DO('alias,displayName,street,zipcode,city,state,countryID,EMail,phone,fax,salutation,title,firstName,middleName,lastName,EMailPrivate,EMailBusiness,phonePrivate,phoneBusiness,phoneCell,gender,company,department,jobTitle,birthday,VATID,bankCode,bankName,bankAccountNo,URL',hLev4Fields:NAME) THEN 
                                                            DO:
                                                                lOK = hLev4Fields:GET-CHILD (hLev4FieldValue,1) NO-ERROR.
                                                                TT_UserShippingAdress.Oppdatert = TRUE. /* Flagger at det har skjedd oppdatering på posten */
                                                                IF lOK THEN 
                                                                CASE hLev4Fields:NAME:
                                                                    WHEN 'alias'         THEN TT_UserShippingAdress.cAlias = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'displayName'   THEN TT_UserShippingAdress.displayName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'street'        THEN TT_UserShippingAdress.street = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'zipcode'       THEN TT_UserShippingAdress.zipcode = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'city'          THEN TT_UserShippingAdress.city = hLev4FieldValue:NODE-VALUE.         
                                                                    WHEN 'state'         THEN TT_UserShippingAdress.state = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'countryID'     THEN TT_UserShippingAdress.countryID = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'EMail'         THEN TT_UserShippingAdress.EMail = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phone'         THEN TT_UserShippingAdress.phone = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'fax'           THEN TT_UserShippingAdress.fax = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'salutation'    THEN TT_UserShippingAdress.salutation = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'TITLE'         THEN TT_UserShippingAdress.cTITLE = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'firstName'     THEN TT_UserShippingAdress.firstName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'middleName'    THEN TT_UserShippingAdress.middleName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'lastName'      THEN TT_UserShippingAdress.lastName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'EMailPrivate'  THEN TT_UserShippingAdress.EMailPrivate = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'EMailBusiness' THEN TT_UserShippingAdress.EMailBusiness = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phonePrivate'  THEN TT_UserShippingAdress.phonePrivate = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phoneBusiness' THEN TT_UserShippingAdress.phoneBusiness = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'phoneCell'     THEN TT_UserShippingAdress.phoneCell = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'gender'        THEN TT_UserShippingAdress.gender = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'company'       THEN TT_UserShippingAdress.company = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'department'    THEN TT_UserShippingAdress.department = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'jobTitle'      THEN TT_UserShippingAdress.jobTitle = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'birthday'      THEN TT_UserShippingAdress.birthday = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'VATID'         THEN TT_UserShippingAdress.VATID = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'bankCode'      THEN TT_UserShippingAdress.bankCode = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'bankName'      THEN TT_UserShippingAdress.bankName = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'bankAccountNo' THEN TT_UserShippingAdress.bankAccountNo = hLev4FieldValue:NODE-VALUE.
                                                                    WHEN 'URL'           THEN TT_UserShippingAdress.cURL = hLev4FieldValue:NODE-VALUE.
                                                                END CASE.
                                                            END.
                                                        END. /* Repeat */        
                                                    END.
                                                WHEN 'customers' THEN
                                                    DO:
                                                        /* Det er lagt ut en referanse på user tilbake til kunde. Den hentes ut her. */
                                                        REPEAT iLoop4 = 1 TO hLev3Fields:NUM-CHILDREN:                        
                                                            lOK = hLev3Fields:GET-CHILD (hLev4Fields,iLoop4) NO-ERROR.
                                                            IF NOT lOK THEN NEXT.
                                                            IF CAN-DO('string',hLev4Fields:NAME) THEN 
                                                            DO:
                                                                lOK = hLev4Fields:GET-CHILD (hLev4FieldValue,1) NO-ERROR.
                                                                IF lOK THEN 
                                                                CASE hLev4Fields:NAME:
                                                                    WHEN 'string' THEN 
                                                                      DO:
                                                                          TT_User.KundeNr = hLev4FieldValue:NODE-VALUE.
                                                                          IF NUM-ENTRIES(TT_User.KundeNr,'/') > 1 THEN
                                                                              TT_User.KundeNr = ENTRY(NUM-ENTRIES(TT_User.KundeNr,'/'),TT_User.KundeNr,'/').   
                                                                      END.
                                                                END CASE.
                                                            END.
                                                        END. /* Repeat */        
                                                    END.
                                            END CASE.
                                        END.
                                    END. /* REPEAT */  
                                END.
                            END. /* Repeat */
                        END. /* user */
                END CASE.
        
            END.
    END. /* REPEAT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-posterKundeData) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE posterKundeData Procedure
PROCEDURE posterKundeData:

	/*------------------------------------------------------------------------------
			Purpose: Hvis innlesning av xml fil har resultert i at det er kommet inn
			         data, skal dette posteres på kunde. Hvilket gjøres her :). 																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEFINE VARIABLE plKundeNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plKortNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE plMedlemsNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE bOk AS LOG NO-UNDO.
  DEFINE VARIABLE cMsgs AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPersonNr AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE cPostNr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPostSted AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iKundeButik AS INTEGER     NO-UNDO.
    /* Kobler medlem til medlemsklubb ut fra hvilken butikk det kommer inn data fra. */
    IF NUM-ENTRIES(tt_Customer.cPath,";") = 2 AND ENTRY(1,tt_Customer.cPath,";") = "But" THEN DO:
        iKundeButik = INT(ENTRY(2,tt_Customer.cPath,";")) NO-ERROR.
        iKundeButik = IF iKundeButik = ? THEN 0 ELSE iKundeButik.
    END.
    FIND FIRST SysPara NO-LOCK WHERE
        SysPara.SysHId = 14 AND
        SysPara.SysGr  = 1 AND
        SysPara.ParaNr >= 31 AND 
        SysPara.ParaNr <= 39 AND
        CAN-DO(SysPara.Parameter1,STRING(iKundeButik)) NO-ERROR.
/*         CAN-DO(SysPara.Parameter1,STRING(iCl)) NO-ERROR. */
    IF AVAILABLE SysPara THEN
        ASSIGN 
            cMKlubbId      = SysPara.Parameter2
            .  
    IF NOT AVAILABLE SysPara OR 
        cMKlubbId = ''
        THEN 
    DO:
      {syspara.i 14 1 7 cMKlubbId}
    END.
   
   DO TRANSACTION:
       /* Henter eksisterende eller oppretter ny kunde. */
       FIND FIRST Kunde EXCLUSIVE-LOCK WHERE
           Kunde.EksterntKundeNr = tt_Customer.KundeNr AND Kunde.ButikkNr = iKundeButik NO-ERROR.
       /* NB: Kundenummer og kundekort opprettes automatisk av db trigger c_kunde.p */
       /* Her settes de felt som skal initieres ved ny kunde fra nettbutikk.        */
       IF NOT AVAILABLE Kunde THEN
       DO:

           FIND FIRST KundeType NO-LOCK NO-ERROR.
           FIND FIRST KundeGruppe NO-LOCK NO-ERROR.
           CREATE Kunde.
           ASSIGN
               Kunde.EksterntKundeNr     = tt_Customer.KundeNr
               Kunde.Kilde               = 'Nettbutikk'
               Kunde.TilgKilde           = tt_billingAddress.displayName
               Kunde.WebKunde            = TRUE 
               Kunde.Aktiv               = TRUE
               Kunde.TypeId              = IF AVAILABLE KundeType THEN KundeType.TypeId ELSE 0
               Kunde.GruppeId            = IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 0
               Kunde.WebKunde             = TRUE
               Kunde.GruppeId            = iKndGruppeId
               Kunde.TypeId              = iKndTypeId               
               .
       END.
       IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
       IF tt_BillingAddress.Oppdatert THEN
       DO:
         FIND NumLandKode NO-LOCK WHERE
              NumLandKode.NumLandKode = INTEGER(tt_billingAddress.countryID) NO-ERROR.
         IF NOT AVAILABLE NumLandKode THEN  
           FIND AlfaLandKode NO-LOCK WHERE 
                AlfaLandKode.AlfaKode3 = TRIM(tt_billingAddress.countryID) NO-ERROR.
         IF NOT AVAILABLE NumLandKode AND NOT AVAILABLE AlfaLandKode THEN  
           FIND AlfaLandKode NO-LOCK WHERE 
                AlfaLandKode.AlfaKode2 = TRIM(tt_billingAddress.countryID) NO-ERROR.
         IF NOT AVAILABLE NumLandKode AND AVAILABLE AlfaLandKode THEN 
           FIND NumLandKode NO-LOCK WHERE
                NumLandKode.NumLandKode = AlfaLandKode.NumLandKode NO-ERROR.                     
        END.       
    
       /* Oppdaterer kunderecord. */
       /* Her settes de felt som skal oppdateres når kunden har oppdatert sin profil på nettet. */
       IF tt_Billingaddress.Oppdatert THEN
       ASSIGN
           Kunde.Navn         = TRIM(tt_billingAddress.displayName)
           Kunde.KontNavn     = tt_billingAddress.firstName + ' ' +
                                tt_billingAddress.middleName + (IF tt_billingAddress.middleName <> '' THEN ' ' ELSE '') +
                                tt_billingAddress.lastName  
           Kunde.Navn         = IF Kunde.Navn = '' THEN Kunde.KontNavn ELSE Kunde.Navn                                                     
           Kunde.Adresse1     = tt_billingAddress.street
           Kunde.Adresse2     = ''           
           Kunde.PostNr       = REPLACE(tt_billingAddress.zipcode,' ','')
           Kunde.Land         = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
           Kunde.Region       = tt_billingAddress.state
           Kunde.ButikkNr     = IF iKundeButik > 0 THEN iKundeButik ELSE iCL                       
           Kunde.Telefon      = tt_billingAddress.phone
           Kunde.TelefonFirma = tt_billingAddress.phoneBusiness
           Kunde.Telefaks     = tt_billingAddress.fax
           Kunde.MobilTlf     = tt_billingAddress.phoneCell
           Kunde.ePostAdresse = tt_billingAddress.EMail
           Kunde.eMailFirma   = tt_billingAddress.EMailBusiness
           Kunde.KontE-Post   = tt_billingAddress.EMailPrivate
           Kunde.KontTelefon  = tt_billingAddress.phonePrivate
           Kunde.Stilling     = tt_billingAddress.jobTitle
           Kunde.Kjon         = IF INTEGER(tt_billingAddress.Gender) <= 2 THEN INTEGER(tt_billingAddress.Gender) ELSE 0
           Kunde.BankKonto    = tt_billingAddress.bankAccountNo
           Kunde.Postgiro     = ''
           Kunde.OrgNr        = (IF tt_billingAddress.VATID <> '' THEN tt_billingAddress.VATID ELSE Kunde.OrgNr)
           Kunde.OrgNr        = REPLACE(Kunde.OrgNr,'-','')
           Kunde.MottaeMailUtsendelser = (IF tt_billingAddress.MottaeMailUtsendelser = 'yes' THEN TRUE ELSE FALSE)
           Kunde.Kommentar    = tt_Customer.comment
           Kunde.WebKanSendeEMail = IF tt_Customer.isHtmlEMailAllowed = 'true' THEN TRUE ELSE FALSE
           Kunde.WebKanSetteOrdre = IF tt_Customer.isDoOrderAllowed = 'true' THEN TRUE ELSE FALSE
           Kunde.ByNavn       = tt_billingAddress.city
           Kunde.Avdeling     = tt_billingAddress.departmen
           Kunde.Tittel       = tt_billingAddress.ctitle
           Kunde.UrlFirma     = tt_billingAddress.cURL
           Kunde.Hilsen       = tt_billingAddress.salutation
           Kunde.BankNavn     = tt_billingAddress.bankName
           Kunde.BankKode     = tt_billingAddress.bankCode
           /* Fakturaadressen settes til det samme som firmaadressen */
           Kunde.FaktAdresse1 = tt_billingAddress.street
           Kunde.FaktAdresse2 = ''
           Kunde.FaktPostNr   = REPLACE(tt_billingAddress.zipcode,' ','')
           Kunde.FaktLand     = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
       .
       IF TRIM(Kunde.MobilTlf) = '' THEN Kunde.MobilTlf = tt_billingAddress.phone.
       
       IF bSvenskFormat THEN 
       DO:
         IF LENGTH(Kunde.PostNr) = 5 THEN Kunde.PostNr = SUBSTRING(Kunde.PostNr,1,3) + ' ' + SUBSTRING(Kunde.PostNr,4,2).
         IF LENGTH(Kunde.FaktPostNr) = 5 THEN Kunde.FaktPostNr = SUBSTRING(Kunde.FaktPostNr,1,3) + ' ' + SUBSTRING(Kunde.FaktPostNr,4,2).
         IF LENGTH(Kunde.LevPostNr) = 5 THEN Kunde.LevPostNr = SUBSTRING(Kunde.LevPostNr,1,3) + ' ' + SUBSTRING(Kunde.LevPostNr,4,2).
       END.
       
       /* Egen assign for å kunne håndtere feil i datokonvertering. */
       ASSIGN
       Kunde.FodtDato = DATE(REPLACE(ENTRY(1,tt_billingAddress.birthday,' '),'.','/'))
           Kunde.Alder    = YEAR(Kunde.FodtDato) - YEAR(TODAY) + 1
           NO-ERROR.
       IF TT_ShippingAdress.Oppdatert THEN
       ASSIGN
           Kunde.LevAdresse1   = TT_ShippingAdress.street
           Kunde.LevAdresse2   = ''
           Kunde.LevPostNr     = TT_ShippingAdress.zipcode
           Kunde.LevLand       = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
           .
       /* Er det lagt inn en ekstra leveringadresse, skal denne benyttes. */   
       IF TT_UserShippingAdress.Oppdatert THEN
       ASSIGN
           Kunde.LevAdresse1   = TT_UserShippingAdress.street
           Kunde.LevAdresse2   = ''
           Kunde.LevPostNr     = TT_UserShippingAdress.zipcode
           Kunde.LevLand       = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
           .   
       /* Felt som ennå ikke er tildelt. 
       ASSIGN
           Kunde.KontTelefaks        = {2}.KontTelefaks.
           Kunde.FaktTekstNr  = {2}.FaktTekstNr
           Kunde.KontMobilTlf  = {2}.KontMobilTlf
           Kunde.BydelsNr      = {2}.BydelsNr
           Kunde.TotalRabatt%  = {2}.TotalRabatt%
           Kunde.BetBet        = {2}.BetBet
           Kunde.Etablert      = {2}.Etablert
           Kunde.SamleFaktura  = {2}.SamleFaktura.
           Kunde.PrivatTlf    = {2}.PrivatTlf
           Kunde.DeresRef     = {2}.DeresRef
           Kunde.Privat       = {2}.Privat
           Kunde.ValKod       = {2}.ValKod
           Kunde.BetType      = {2}.BetType
           Kunde.Purregebyr   = {2}.Purregebyr
           Kunde.Fakturagebyr = {2}.Fakturagebyr
           Kunde.Hovedkunde          = {2}.Hovedkunde
           Kunde.KobletTilKunde      = {2}.KobletTilKunde
           Kunde.Faktureringsperiode = {2}.Faktureringsperiode
           Kunde.Momskod             = {2}.Momskod.
       */

       FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
       IF AVAILABLE KundeKort AND NOT CAN-FIND(FIRST Medlem WHERE Medlem.KundeNr = Kunde.KundeNr) THEN
       KNDKORT: 
       DO:       
            IF AVAILABLE Medlem THEN RELEASE Medlem.
            
            ASSIGN
            plKortNr  = INT(KundeKort.KortNr)
            plKundeNr = Kunde.KundeNr 
            cPersonNr = (IF tt_billingAddress.VATID <> '' THEN tt_billingAddress.VATID ELSE '')
            cPersonNr = REPLACE(cPersonNr,'-','')
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN LEAVE KNDKORT.
            /* Oppretter medlem via SPAR. */
            IF LENGTH(cPersonNr) = 10 THEN 
            DO:
                RUN asMedlem.p (IF iKundeButik > 0 THEN iKundeButik ELSE iCl,
                                99,
                                cPersonNr, 
                                USERID('SkoTex'),
                                OUTPUT plMedlemsNr,
                                OUTPUT cMKortNr,
                                OUTPUT cBongTekst,
                                OUTPUT cNavn,                                
                                OUTPUT cStatus,
                                OUTPUT bOk,
                                OUTPUT cMelding
                               ).
                FIND Medlem EXCLUSIVE-LOCK WHERE
                    Medlem.MedlemsNr = DEC(cPersonNr + cMKlubbId) NO-ERROR.
                IF AVAILABLE Medlem THEN 
                DO:
                    ASSIGN 
                    Medlem.KundeNr = Kunde.KundeNr
                    plMedlemsNr    = Medlem.MedlemsNr
                    .
                    FIND FIRST MedlemsKort OF Medlem EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE MedlemsKort THEN 
                      MedlemsKort.InterntKKortId = KundeKort.InterntKKortId.
                END. 
            END. 
            
            IF plMedlemsNr = 0 THEN 
            DO:
                /* Bare medlem skal opprettes. */
                RUN genkundeMedlem.p (IF iKundeButik > 0 THEN iKundeButik ELSE iCL,
                                      iKndGruppeId,
                                      INPUT-OUTPUT plKundeNr,
                                      OUTPUT plMedlemsNr,
                                      OUTPUT bOk,
                                      OUTPUT cMsgs).
                /* Kundekortene legges opp på samme kunde, men medlemskortene */
                /* legges på separate medlemmer.                              */
                RUN genkundekort_og_medlem.p (IF iKundeButik > 0 THEN iKundeButik ELSE iCL,
                                              plKundeNr,
                                              plMedlemsNr,
                                              plKortNr,
                                              plKortNr,
                                              999,
                                              OUTPUT bOk,
                                              OUTPUT cMsgs).
            END.
            
            IF plMedlemsNr > 0 THEN 
            FIND Medlem EXCLUSIVE-LOCK WHERE 
              Medlem.MedlemsNr = plMedlemsNr NO-ERROR.
            IF AVAILABLE Medlem THEN
            DO: 
              ASSIGN Medlem.MKlubbId = INT(tt_billingAddress.MedlemsKlubb) NO-ERROR.
              IF Medlem.MKlubbId = 0 THEN 
                Medlem.MKlubbId = iStdMKlubbId.
                
              /* Oppdaterer annen medlemsinfo fra kunde. */
              ASSIGN
              Medlem.ForNavn   = tt_billingAddress.firstName + ' ' +
                                 tt_billingAddress.middleName + (IF tt_billingAddress.middleName <> '' THEN ' ' ELSE '')
              Medlem.Etternavn = tt_billingAddress.lastName
              NO-ERROR.
              
              ASSIGN 
              Medlem.Fodselsdato = DATE(tt_billingAddress.birthday) NO-ERROR.
              IF Medlem.Fodselsdato <> ? THEN Medlem.FodtAr = YEAR(Medlem.Fodselsdato).
              
              ASSIGN 
              Medlem.Personnr          = (IF cPersonNr <> '' THEN cPersonNr ELSE Kunde.OrgNr)
              Medlem.Adresse1          = tt_billingAddress.street
              Medlem.Adresse2          = ''
              Medlem.PostNr            = REPLACE(tt_billingAddress.zipcode,' ','')
              Medlem.Land              = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
              Medlem.ePostAdresse      = tt_billingAddress.EMail
              Medlem.Telefon           = tt_billingAddress.phoneBusiness
              Medlem.MobilTlf          = tt_billingAddress.phoneCell
              Medlem.Telefaks          = tt_billingAddress.fax
              Medlem.AktivertFraWeb    = TODAY 
              Medlem.ButikkNr          = iCL
              Medlem.Kilde             = 'Nettbutikk'
              Medlem.EksterntMedlemsNr = tt_Customer.KundeNr
              NO-ERROR.
              
              IF LENGTH(cPersonNr) = 10 THEN 
                  Medlem.Kjonn = Medlem.Kjonn.
              ELSE Medlem.Kjonn             = IF INTEGER(tt_billingAddress.Gender) = 1 THEN TRUE 
                                         ELSE IF INTEGER(tt_billingAddress.Gender) = 2 THEN FALSE 
                                         ELSE ?.
             
             IF bSvenskFormat AND LENGTH(Medlem.PostNr) = 5 THEN 
               IF LENGTH(Medlem.PostNr) = 5 THEN Medlem.PostNr = SUBSTRING(Medlem.PostNr,1,3) + ' ' + SUBSTRING(Medlem.PostNr,4,2).
              
              FIND FIRST Medlemskort OF Medlem EXCLUSIVE-LOCK NO-ERROR.
              IF AVAILABLE MedlemsKort THEN 
                Medlemskort.Innehaver = Kunde.Navn.
            END.
            IF NOT CAN-FIND(Post WHERE 
                Post.PostNr = Medlem.PostNr) THEN 
                DO:
                    CREATE Post.
                    ASSIGN 
                        Post.PostNr      = Medlem.PostNr
                        Post.Beskrivelse = REPLACE(tt_billingAddress.city,' ','').
                    RELEASE Post.
                END.
       END. /* KNDKORT */ 
       ELSE IF AVAILABLE KundeKort AND CAN-FIND(FIRST Medlem WHERE Medlem.KundeNr = Kunde.KundeNr) THEN
       DO:
           FIND FIRST Medlem EXCLUSIVE-LOCK WHERE 
             Medlem.KundeNr = KundeKort.KundeNr NO-ERROR.
           IF AVAILABLE Medlem THEN 
           DO:
               cPersonNr = REPLACE((IF tt_billingAddress.VATID <> '' THEN tt_billingAddress.VATID ELSE ''),'-','').
               IF Medlem.PersonNr = '' 
                 THEN ASSIGN 
                        Medlem.PersonNr = cPersonNr
                        plMedlemsNr     = Medlem.MedlemsNr.
               ASSIGN Medlem.MKlubbId = INT(tt_billingAddress.MedlemsKlubb).
           END.
       END.       
   END. /* TRANSACTION */
   
    IF plMedlemsNr > 0 THEN 
      RUN konverter_medlemsnr_personnr.p (INPUT-OUTPUT plMedlemsNr).
   
   IF AVAILABLE MedlemsKort THEN RELEASE MedlemsKort.
   IF AVAILABLE Medlem      THEN RELEASE Medlem.
   IF AVAILABLE Kunde       THEN RELEASE Kunde.
   IF AVAILABLE KundeKort   THEN RELEASE KundeKort.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-PrepTempTabell) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepTempTabell Procedure
PROCEDURE PrepTempTabell:
	/*------------------------------------------------------------------------------
			Purpose: Tømmer temp-tabell 																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    /* Det kommer ALLTID bare en kunde pr. fil.         */
    /* Renser opp i temptabell før innlesning begynner. */
    FOR EACH TT_Customer:           DELETE TT_Customer.           END.
    FOR EACH TT_BillingAddress:     DELETE TT_BillingAddress.     END.
    FOR EACH TT_ShippingAdress:     DELETE TT_ShippingAdress.     END.
    FOR EACH TT_User:               DELETE TT_User.               END.
    FOR EACH TT_UserBillingAddress: DELETE TT_UserBillingAddress. END.
    FOR EACH TT_UserShippingAdress: DELETE TT_UserShippingAdress. END.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDato Procedure 
FUNCTION getDato RETURNS DATE
    ( INPUT cYYYYMMDD AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes: cYYYYMMDD är separerad med "-" 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDato AS DATE NO-UNDO.
    dDato = DATE(INT(ENTRY(2,cYYYYMMDD,"-")),INT(ENTRY(3,cYYYYMMDD,"-")),INT(ENTRY(1,cYYYYMMDD,"-"))) NO-ERROR.   /* Function return value. */
    RETURN dDato.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTid Procedure 
FUNCTION getTid RETURNS INTEGER
    ( INPUT cHHMMSS AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes: cHHMMSS är separerad med ":" (kolon) 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTid AS INTEGER INIT ? NO-UNDO.
    iTid = INT(ENTRY(1,cHHMMSS,":")) * 3600 + INT(ENTRY(2,cHHMMSS,":")) * 60 + INT(ENTRY(3,cHHMMSS,":")) NO-ERROR.   /* Function return value. */
    RETURN iTid.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

