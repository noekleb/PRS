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
DEFINE VARIABLE iNettbutikk AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cKOrdre_Id_Lst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSeqNr AS INTEGER NO-UNDO.
DEFINE VARIABLE ocValue AS CHARACTER NO-UNDO.

DEFINE VARIABLE iVgLopNr AS INT NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bSJekkKundeKort AS LOG NO-UNDO.
DEFINE VARIABLE bSvenskFormat AS LOG NO-UNDO.
DEFINE VARIABLE bAutoOverfor AS LOG NO-UNDO.
DEFINE VARIABLE lJFPlock AS LOGICAL     NO-UNDO.


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

{xinnBITSOrder.i &NEW=NEW}

DEFINE BUFFER bNettbutikk FOR Butiker.
DEFINE BUFFER bSentrallager FOR Butiker.

RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p. Ny import startet.').

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

&IF DEFINED(EXCLUDE-hentDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentDato Procedure 
FUNCTION hentDato RETURNS DATE
        ( INPUT cDatoTid AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentSisteEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentSisteEntry Procedure 
FUNCTION hentSisteEntry RETURNS CHARACTER
        ( INPUT cTekst AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentTid Procedure 
FUNCTION hentTid RETURNS INTEGER
        ( INPUT cDatoTid AS CHARACTER ) FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE cRet_val AS CHARACTER NO-UNDO.

/* Returnerer hvis det ikke er en xml fil som skal leses. */
IF NOT ENTRY(NUM-ENTRIES(cFileName,"."),cFileName,".") = "xml" THEN
DO:
  RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p. Mottat fil er ikke en xml fil: ' + cFileName).
  RETURN 'ERROR'.
END.


{syspara.i 150 1 15 cTekst} 
IF CAN-DO('1,J,Ja,Y,YES,true',cTekst) THEN 
  bSJekkKundeKort = TRUE.
    
{syspara.i 150 1 9 cTekst} 
IF cTekst = "3" THEN
    lJFPlock = TRUE.

{syspara.i 150 1 16 cTekst}
IF CAN-DO("1,J,Ja,Yes,True",cTekst) THEN 
    bSvenskFormat = TRUE.
          
{syspara.i 150 1 19 cTekst}
IF CAN-DO("1,J,Ja,Yes,True",cTekst) THEN 
    bAutoOverfor = TRUE.          
          
{syspara.i 5 1 1 iCL int}
{syspara.i 150 1 2 iNettbutikk int}
{syspara.i 19 9 2 iVgLopNr INT} 
   
IF NOT CAN-FIND(Butiker WHERE
                Butiker.Butik = iNettbutikk) THEN
DO:                
  RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p. ** Feil - Nettbutikknr. ikke satt, eller satt til ukjent butikk: ' + STRING(iNettbutikk)).
  RETURN '** Feil - Nettbutikknr. ikke satt, eller satt til ukjent butikk.'.
END.
FIND bNettButikk NO-LOCK WHERE
    bNettbutikk.Butik = iNettbutikk.
FIND bSentrallager NO-LOCK WHERE
    bSentrallager.Butik = iCL.
    
/* Tømmer gamle og oppretter nye tomme temp-tabeller. */
RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p PrepTempTabell.').
RUN PrepTempTabell.

/* Oppretter temp-tabell for å kunne postere direkte i dem ved innlesning fra xml fil. */
CREATE TT_Order.
CREATE TT_lineItemContainer.
CREATE TT_BillingAddress.
CREATE TT_ShippingAdress.
/* Disse blir opprettet under innlesning. Det kan komme flere poster av hver av dem. */
/* CREATE TT_productLineItem. */
/* CREATE TT_taxLineItem. */
/* CREATE TT_paymentLineItem. */
/* CREATE TT_shippingLineItem. */
    
/* Leser inn data fra xml fil og posterer dem i temp-tabell. */
RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p LesInnFil.').
RUN LesInnFil.
    
/* Er det kommet inn informasjon på noen av postene, skal det oppdateres */
IF cRet_Val <> 'ERROR' THEN 
DO:
    RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p posterOrdreData.').
    RUN posterOrdreData.           
END.
    
/*  Legger på plukkbutikk, oppretter overføringsordre og sender eMail. */
IF cKOrdre_Id_Lst <> '' THEN 
  DO:
    RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p Legger på plukkbutikk, oppretter overføringsordre og sender eMail. Ordreliste: ' + cKOrdre_Id_Lst).
    /* Setter plukkbutikk på kundeordrelinjene. */
    IF lJFPlock = TRUE THEN
        RUN sett_plukkbutikk_kundeordreJF.p (cKOrdre_Id_Lst,"",0). /* tredje parameter är rowid, används vid byte av plockbutik från KOrdrelinje.w till ny butik */
    ELSE
        RUN sett_plukkbutikk_kundeordre.p (cKOrdre_Id_Lst).
    /* Opprettelse av overføringsordre og sending av email. En ordre pr. fra/til butikkrelasjon. */
    IF bAutoOverfor THEN DO:
        IF lJFPlock THEN
            RUN opprett_OverforingsordreJF.p (cKOrdre_Id_Lst,TRUE,""). /* tredje parameter är rowid, används efter ändring av plockbutik från KOrdrelinje.w */
        ELSE
            RUN opprett_Overforingsordre.p (cKOrdre_Id_Lst,FALSE).
    END.
  END.

/* Rydder i memory */
RUN clearMemory.

IF cRet_val = "ERROR" THEN 
  DO:
    RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p LesInnFil returnerer feil: ' + cRet_Val).
    RETURN 'ERROR'.
  END.
ELSE DO:
    RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p Innlesning avsluttet med returmelding: ' + cRet_Val).
    RETURN "OK".
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-clearMemory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearMemory Procedure 
PROCEDURE clearMemory :
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
PROCEDURE LesInnFil :
DEFINE VARIABLE cName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoop1 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLoop2 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLoop3 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLoop4 AS INTEGER   NO-UNDO.
    DEF    VAR      lOk    AS LOGICAL   NO-UNDO.

    RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p. Leser ordrefil: ' + cFileName).

    /* Leser inn xml filen */
    lOk = hDoc:LOAD ("file",cFileName, FALSE) NO-ERROR.
    IF lOK = FALSE THEN 
    DO:
        RETURN "ERROR".
    END.
    IF NOT hDoc:GET-DOCUMENT-ELEMENT (hLev1) THEN
        RETURN "ERROR".
    
    /* Sjekker at det er riktig nodenavn */    
    cName = hLev1:NAME NO-ERROR.
    IF cName <> "order" THEN
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
            IF CAN-DO('path,alias,customer,user,creationDate,viewedOn,cancelledOn,inProcessOn,pendingOn,readyForShippingOn,partlyDispatchedOn,dispatchedOn,shippedOn,partlyPaidOn,paidOn,closedOn,archivedOn,partlyInvoicedOn,invoicedOn,internalComment,customerComment',hLev1Fields:NAME) THEN 
            DO:
                lOK = hLev1Fields:GET-CHILD (hLev1FieldValue,1) NO-ERROR.
                IF lOK THEN
                    RUN UpdateTT_tables ("Order",hLev1Fields:NAME,hLev1FieldValue:NODE-VALUE).
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
                                IF CAN-DO('alias,displayName,street,zipcode,city,state,countryID,EMail,phone,fax,salutation,title,firstName,middleName,lastName,EMailPrivate,EMailBusiness,phonePrivate,phoneBusiness,phoneCell,gender,company,department,jobTitle,birthday,VATID,bankCode,bankName,bankAccountNo,URL',hLev2Fields:NAME) THEN 
                                DO:
                                    lOK = hLev2Fields:GET-CHILD (hLev2FieldValue,1) NO-ERROR.
                                    TT_BillingAddress.Oppdatert = TRUE. /* Flagger at det har skjedd oppdatering på posten */
                                    IF lOK THEN 
                                        RUN UpdateTT_tables ("Billing",hLev2Fields:NAME,hLev2FieldValue:NODE-VALUE).
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
                                        RUN UpdateTT_tables ("Shipping",hLev2Fields:NAME,hLev2FieldValue:NODE-VALUE).
                                END.
                            END. /* Repeat */        
                        END.
                    /* ---------- Node for brukere hos kunde ---------- */
                    /* Denne noden har ingen felt liggende på nodenivå. */
                    /* Det ligger bare en ny node - 'lineItemContainer'.             */
                    WHEN 'lineItemContainer' THEN 
                        DO:
                            /* Benytter repeat, men det er bare et felt her, som er en node. */
                            REPEAT iLoop2 = 1 TO hLev1Fields:NUM-CHILDREN:                        
                                lOK = hLev1Fields:GET-CHILD (hLev2Fields,iLoop2) NO-ERROR.
                                IF NOT lOK THEN NEXT.                                                                
                                IF CAN-DO('localeID,languageCode,currencyID,taxArea,taxAreaName,taxModel,grandTotal,totalBeforeTax,totalTax',hLev2Fields:NAME) THEN 
                                DO:
                                    IF hLev2Fields:NUM-CHILDREN = 1 THEN
                                    DO:
                                      lOK = hLev2Fields:GET-CHILD (hLev2FieldValue,1) NO-ERROR.
                                      IF lOK THEN 
                                          RUN UpdateTT_tables ("LineItemContainer",hLev2Fields:NAME,hLev2FieldValue:NODE-VALUE).
                                    END.
                                END.
                                /* Her tar vi nodefeltene på lineItemContainer */ 
                                ELSE DO:
                                    CASE hLev2Fields:NAME:
                                        WHEN 'productLineItems' THEN 
                                            DO:
                                                REPEAT iLoop3 = 1 TO hLev2Fields:NUM-CHILDREN:
                                                    lOK = hLev2Fields:GET-CHILD (hLev3Fields,iLoop3) NO-ERROR.
                                                    IF NOT lOK THEN NEXT.
                                                    IF hLev3Fields:NUM-CHILDREN > 0 THEN
                                                    DO:
                                                        IF hLev3Fields:NUM-CHILDREN > 1 THEN
                                                        DO:
                                                            /* Oppretter post i temp-tabell tt_productLineItem */
                                                            ASSIGN iSeqNr = iSeqNr + 1.
                                                            CREATE TT_productLineItem.
                                                            ASSIGN TT_productLineItem.SeqNr = iSeqNr.
                                                            
                                                            REPEAT iLoop4 = 1 TO hLev3Fields:NUM-CHILDREN:
                                                                lOK = hLev3Fields:GET-CHILD (hLev4Fields,iLoop4) NO-ERROR.
                                                                IF NOT lOK THEN NEXT.                                                
                                                                IF hLev4Fields:NUM-CHILDREN = 1 THEN
                                                                DO:
                                                                    /* ext:artikkel Artikkelnr="9815564" ModellFarge="900523" StrKode="11" /> */                                                                        
                                                                    /* Posterer feltene */
                                                                    lOK = hLev4Fields:GET-CHILD (hLev4FieldValue,1) NO-ERROR.
                                                                    IF lOK THEN 
                                                                        RUN UpdateTT_tables ("productLineItem",hLev4Fields:NAME,hLev4FieldValue:NODE-VALUE).
                                                                END.
                                                                ELSE IF CAN-DO('ext:artikkel,artikkel',hLev4Fields:NAME) THEN 
                                                                DO:
                                                                    RUN UpdateTT_tables ("productLineItem",hLev4Fields:NAME,hLev4FieldValue:NODE-VALUE).
                                                                    ASSIGN 
                                                                    TT_productLineItem.Artikkelnr = hLev4Fields:GET-ATTRIBUTE ("Artikkelnr")
                                                                    TT_productLineItem.ModellFarge = hLev4Fields:GET-ATTRIBUTE ("ModellFarge")
                                                                    TT_productLineItem.StrKode = hLev4Fields:GET-ATTRIBUTE ("StrKode")
                                                                    .
                                                                END.
                                                            END. /* REPEAT */
                                                        END.
                                                    END.
                                                END. 
                                            END. /* productLineItems */
                                        WHEN 'taxLineItems' THEN 
                                            DO:
                                                REPEAT iLoop3 = 1 TO hLev2Fields:NUM-CHILDREN:
                                                    lOK = hLev2Fields:GET-CHILD (hLev3Fields,iLoop3) NO-ERROR.
                                                    IF NOT lOK THEN NEXT.
                                                    IF hLev3Fields:NUM-CHILDREN > 0 THEN
                                                    DO:
                                                        IF hLev3Fields:NUM-CHILDREN > 1 THEN
                                                        DO:
                                                            /* Oppretter post i temp-tabell tt_taxLineItem */
                                                            CREATE TT_taxLineItem.
                                                            
                                                            REPEAT iLoop4 = 1 TO hLev3Fields:NUM-CHILDREN:
                                                                lOK = hLev3Fields:GET-CHILD (hLev4Fields,iLoop4) NO-ERROR.
                                                                IF NOT lOK THEN NEXT.                                                
                                                                IF hLev4Fields:NUM-CHILDREN = 1 THEN
                                                                DO:
                                                                    /* <ext:MomsKod>1</ext:MomsKod> */                                                                        
                                                                    /* Posterer feltene */
                                                                    lOK = hLev4Fields:GET-CHILD (hLev4FieldValue,1) NO-ERROR.
                                                                    IF lOK THEN 
                                                                        RUN UpdateTT_tables ("taxLineItem",hLev4Fields:NAME,hLev4FieldValue:NODE-VALUE).
                                                                END.
                                                            END. /* REPEAT */
                                                        END.
                                                    END.
                                                END. 
                                            END. /* taxLineItems*/ 
                                        WHEN 'paymentLineItem' THEN
                                            DO:
                                                /* Oppretter post i temp-tabell */
                                                CREATE TT_paymentLineItem.
                                                /* Posterer feltene */
                                                REPEAT iLoop3 = 1 TO hLev2Fields:NUM-CHILDREN:
                                                    lOK = hLev2Fields:GET-CHILD (hLev3Fields,iLoop3) NO-ERROR.
                                                    IF NOT lOK THEN NEXT.                                                
                                                    IF hLev3Fields:NUM-CHILDREN = 1 THEN
                                                    DO:
                                                        lOK = hLev3Fields:GET-CHILD (hLev3FieldValue,1) NO-ERROR.
                                                        IF lOK THEN 
                                                            RUN UpdateTT_tables ("paymentLineItem",hLev3Fields:NAME,hLev3FieldValue:NODE-VALUE).
                                                    END.
                                                END. /* REPEAT */
                                            END. /* paymentLineItem */
                                        WHEN 'shippingLineItem' THEN 
                                            DO:
                                                /* Oppretter post i temp-tabell */
                                                CREATE TT_shippingLineItem.
                                                /* Posterer feltene */
                                                REPEAT iLoop3 = 1 TO hLev2Fields:NUM-CHILDREN:
                                                    lOK = hLev2Fields:GET-CHILD (hLev3Fields,iLoop3) NO-ERROR.
                                                    IF NOT lOK THEN NEXT.                                                
                                                    IF hLev3Fields:NUM-CHILDREN = 1 THEN
                                                    DO:
                                                        lOK = hLev3Fields:GET-CHILD (hLev3FieldValue,1) NO-ERROR.
                                                        IF lOK THEN 
                                                            RUN UpdateTT_tables ("shippingLineItem",hLev3Fields:NAME,hLev3FieldValue:NODE-VALUE).
                                                    END.
                                                END. /* REPEAT */
                                            END. /* shippingLineItem */                                            
                                    END CASE.
                                END. 
                            END. /* Repeat */        
                        END. /* lineItemContainer */
                END CASE.
        
            END.
    END. /* REPEAT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-posterOrdreData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE posterOrdreData Procedure 
PROCEDURE posterOrdreData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cFraktMetode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hKordreLinje AS HANDLE NO-UNDO.
DEFINE VARIABLE cKundeId     AS CHARACTER NO-UNDO.
DO TRANSACTION:
     /* Disse temp tabellene finnes det bare en post i */
     FIND FIRST tt_Order NO-ERROR.
     FIND FIRST tt_lineItemContainer NO-ERROR.
     FIND FIRST TT_BillingAddress NO-ERROR.
     FIND FIRST TT_ShippingAdress NO-ERROR.
     IF NUM-ENTRIES(tt_Order.Path,";") = 2 AND ENTRY(1,tt_Order.Path,";") = "But" THEN DO:
         FIND bNettButikk NO-LOCK WHERE
             bNettbutikk.Butik = INT(ENTRY(2,tt_Order.Path,";")) NO-ERROR.
     END.

     /* Sjekker om ordren finnes fra før. <creationDate>24.04.2009 22:30:15</creationDate> */
     IF tt_Order.creationDate <> '' OR tt_Order.cAlias <> '' THEN 
         FIND KOrdreHode NO-LOCK WHERE
              KOrdreHode.RegistrertDato = hentDato(tt_Order.creationDate) AND
              KOrdreHode.EkstOrdreNr    = tt_Order.cAlias NO-ERROR.
     ELSE DO:
       RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: ** Feil - Ingen opprettelsesdato/ordrereferanse på ordre fra nettbutikk. ' + 'Dato: ' + STRING(tt_Order.creationDate) + ' OrdreNr: ' + STRING(tt_Order.cAlias)).
       RETURN '** Feil - Ingen opprettelsesdato/ordrereferanse på ordre fra nettbutikk.'.
     END.
     IF tt_Order.Customer = '' THEN DO: 
       RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: ** Feil - Ingen kundedata angitt på ordren fra nettbutikk. ' + 'Dato: ' + STRING(tt_Order.creationDate) + ' OrdreNr: ' + STRING(tt_Order.cAlias)).
       RETURN '** Feil - Ingen kundedata angitt på ordren fra nettbutikk.'.
     END.
     /* Henter kunde. */
     ASSIGN cKundeId = hentSisteEntry(tt_Order.customer) NO-ERROR.
     IF AVAIL bNettbutikk THEN
        FIND Kunde NO-LOCK WHERE Kunde.EksterntKundeNr = cKundeId AND Kunde.butikknr = bNettbutikk.Butik NO-ERROR.
/*      IF bSJekkKundeKort AND NOT AVAILABLE Kunde THEN                             */
/*      DO:                                                                         */
/*        FIND FIRST KundeKort NO-LOCK WHERE KundeKort.KortNr = cKundeId NO-ERROR.  */
/*        IF AVAILABLE KundeKort THEN                                               */
/*          FIND Kunde OF KundeKort NO-LOCK NO-ERROR.                               */
/*      END.                                                                        */
     IF NOT AVAILABLE Kunde THEN
     DO:
       RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: ** Feil - Ukjent kunde angitt på kundeordre. ' + 'Dato: ' + STRING(tt_Order.creationDate) + ' OrdreNr: ' + STRING(tt_Order.cAlias) + ' Kunde: ' + STRING(tt_Order.customer)).
       RETURN '** Feil - Ukjent kunde angitt på kundeordre ' + tt_Order.cAlias + ' fra nettbutikk.'.
     END.
     IF AVAILABLE KOrdreHode THEN
     OPPDATER_ORDRE:
     DO:
       /* Legg inn oppdatering av aktuelle feilt. Ref. mail med spørsmål som er sendt til Ronny Jordalen. */
       RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: Ordre EKSISTERER FRA FØR. ' + 'Kordre_Id: ' + STRING(KOrdreHode.KOrdre_ID) + ' og EksterntId: ' + STRING(KOrdreHode.EkstOrdreNr) + ' Kunde: ' + STRING(KOrdreHode.KundeNr)).
     END. /* OPPDATER_ORDRE */
     
     ELSE 
     OPPRETTELSE_ORDRE:
     DO:           
         /* Ordrenummer settes ved create i db trigger. */
         CREATE KOrdreHode.
         RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: Ordre opprettet. ' + 'Kordre_Id: ' + STRING(KOrdreHode.KOrdre_ID) + ' og EksterntId: ' + STRING(KOrdreHode.EkstOrdreNr) + ' Kunde: ' + STRING(KOrdreHode.KundeNr)).
         ASSIGN
             KOrdreHode.VerkstedMerknad   = 'Innlest ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS') 
             KOrdreHode.Opphav            = 10 /* Nettbutikk */
             KOrdreHode.EkstOrdreNr       = tt_Order.cAlias
             KOrdreHode.internMerknad     = tt_Order.internalComment 
             KOrdreHode.KundeMerknad      = tt_Order.customerComment.
         KOrdreHode.RegistrertDato  = hentDato(tt_Order.creationDate).
         KOrdreHode.RegistrertTid   = hentTid(tt_Order.creationDate).
         KOrdreHode.RegistrertAv    = hentSisteEntry(tt_Order.cuser). /* /Shops/DemoShop/Users/magbyr*/
         IF KORdreHode.RegistrertDato = ? THEN 
             ASSIGN 
             KOrdreHode.RegistrertDato = TODAY
             KOrdreHode.RegistrertTid  = TIME.
         /* Bygger en liste over de kundeordre som det skal sendes eMail og opprettes overføringsordre på. */
         IF NOT CAN-DO(cKOrdre_Id_Lst,STRING(KOrdreHode.KORdre_Id)) THEN
           ASSIGN cKOrdre_Id_Lst = cKOrdre_Id_Lst + (IF cKOrdre_Id_Lst = '' THEN '' ELSE ',') + STRING(KOrdreHode.KOrdre_Id).
         /* Henter kasse. Kasse 99 legges alltid opp for kundeordre. Brukes også for nettbutikk. */
         FIND LAST Kasse NO-LOCK WHERE Kasse.ButikkNr = bNettbutikk.Butik NO-ERROR.
         /* Henter kasserer. Det skal ligge en kasserer med netbutikk.brukerid = forsalg.brukerid2. */
         FIND FIRST Forsalj NO-LOCK WHERE Forsal.Brukerid2 = hentSisteEntry(tt_Order.cuser) NO-ERROR.
         IF NOT AVAILABLE Kunde THEN
         DO:
           RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: ** Feil - Ukjent kunde angitt på kundeordre(2). ' + 'Dato: ' + STRING(tt_Order.creationDate) + ' OrdreNr: ' + STRING(tt_Order.cAlias) + ' Kunde: ' + STRING(tt_Order.customer)).
           RETURN '** Feil - Ukjent kunde angitt på kundeordre ' + tt_Order.cAlias + ' fra nettbutikk.'.
         END.
         ELSE DO:
            /* Kundenr, butikk, kasserer m.m. */               
            ASSIGN                   
            KOrdreHode.KundeNr              = Kunde.KundeNr
            KOrdreHode.ButikkNr             = bNettbutikk.Butik
            KOrdreHode.KasseNr              = (IF AVAILABLE Kasse THEN Kasse.KasseNr ELSE 0)
            KOrdreHode.ForsNr               = (IF AVAILABLE Forsalj THEN Forsalj.ForsNr ELSE 0)
            KOrdreHode.SelgerNr             = 0 NO-ERROR.
            /* Kontoradresse m.m. */
            ASSIGN     
            KOrdreHode.Navn                 = tt_billingAddress.firstName + ' ' + tt_billingAddress.lastName
            KOrdreHode.KontNavn             = Kunde.KontNavn NO-ERROR.
            /* Kontoradresse skal hentes fra fakturadresse på ordre. Er ikke vedlikeholdt i kunderegister. */
            ASSIGN KOrdreHode.Adresse1      = tt_billingAddress.street NO-ERROR. 
            ASSIGN KOrdreHode.Adresse2      = tt_billingAddress.addressExtension NO-ERROR.
            ASSIGN KOrdreHode.PostNr        = tt_billingAddress.zipcode NO-ERROR.
            ASSIGN KOrdreHode.Poststed      = tt_billingAddress.city NO-ERROR.
            ASSIGN KOrdreHode.ePostAdresse  = tt_billingAddress.EMail NO-ERROR.               
            ASSIGN KOrdreHode.Telefon       = tt_billingAddress.phone NO-ERROR.
            ASSIGN KOrdreHode.MobilTlf      = tt_billingAddress.phoneCell NO-ERROR.
            IF KOrdreHode.MobilTlf = '' THEN ASSIGN KOrdreHode.MobilTlf = KOrdreHode.Telefon NO-ERROR.
            IF bSvenskFormat AND LENGTH(KOrdreHode.PostNr) = 5 THEN 
              IF LENGTH(KOrdreHode.PostNr) = 5 THEN KOrdreHode.PostNr = SUBSTRING(KOrdreHode.PostNr,1,3) + ' ' + SUBSTRING(KOrdreHode.PostNr,4,2).
            
            ASSIGN KOrdreHode.DeresRef = Kunde.DeresRef
                   KOrdreHode.VaarRef  = bNettbutikk.VaarRef no-error.
            ASSIGN
            KOrdreHode.Leveringsdato        = KOrdreHode.RegistrertDato + 7
            KOrdreHode.BetaltDato           = KOrdreHode.RegistrertDato
            KOrdreHode.TotalRabatt%         = 0.0
            KOrdreHode.LevStatus            = '30' /* Bekreftet */
            KOrdreHode.BetBet               = Kunde.BetBet no-error.
            /* Overstyrer elementer i fakturaadresse. */
            ASSIGN KOrdreHode.DeresRef      = tt_billingAddress.firstName + ' ' + tt_billingAddress.lastName NO-ERROR.
            ASSIGN KOrdreHode.FaktAdresse1  = tt_billingAddress.street NO-ERROR. 
            ASSIGN KOrdreHode.FaktAdresse2  = tt_billingAddress.addressExtension NO-ERROR.
            ASSIGN KOrdreHode.FaktPostNr    = tt_billingAddress.zipcode NO-ERROR.
            ASSIGN KOrdreHode.FaktPoststed  = tt_billingAddress.city NO-ERROR.
            ASSIGN KOrdreHode.ePostAdresse  = tt_billingAddress.EMail NO-ERROR.               
            ASSIGN KOrdreHode.Telefon       = tt_billingAddress.phone NO-ERROR.
            ASSIGN KOrdreHode.Telefaks      = tt_billingAddress.phoneCell NO-ERROR.
            ASSIGN KOrdreHode.FirmaNavn     = tt_billingAddress.company NO-ERROR.
            ASSIGN KOrdreHode.FirmaAdresse1 = tt_billingAddress.firstName NO-ERROR.
            ASSIGN KOrdreHode.FirmaAdresse2 = tt_billingAddress.lastName NO-ERROR.
            IF bSvenskFormat AND LENGTH(KOrdreHode.FaktPostNr) = 5 THEN 
              IF LENGTH(KOrdreHode.FaktPostNr) = 5 THEN KOrdreHode.FaktPostNr = SUBSTRING(KOrdreHode.FaktPostNr,1,3) + ' ' + SUBSTRING(KOrdreHode.FaktPostNr,4,2).
            IF TRIM(tt_billingAddress.countryID) <> '' THEN
            DO: 
              IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
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
              /* Fakturaadresse */
              ASSIGN
              KOrdreHode.FaktLand          = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE '')
              NO-ERROR.
            END.
            IF KOrdreHode.FaktAdresse1 = '' AND KOrdreHode.FaktAdresse2 = '' THEN 
              ASSIGN
              KOrdreHode.FaktAdresse1         = KOrdreHode.Adresse1
              KOrdreHode.FaktAdresse2         = KOrdreHode.Adresse2
              KOrdreHode.FaktPostNr           = KOrdreHode.PostNr
              KOrdreHode.FaktLand             = Kunde.Land NO-ERROR.
            /* Leveringsadresse */
            ASSIGN KOrdreHode.LevAdresse1  = tt_shippingAdress.street NO-ERROR. 
            ASSIGN KOrdreHode.LevAdresse2  = tt_shippingAdress.addressExtension NO-ERROR.
            ASSIGN KOrdreHode.LevPostNr    = tt_shippingAdress.zipcode NO-ERROR.
            ASSIGN KOrdreHode.LevPoststed  = tt_shippingAdress.city NO-ERROR.
            /* Land */
            IF bSvenskFormat AND LENGTH(KOrdreHode.LevPostNr) = 5 THEN 
              IF LENGTH(KOrdreHode.LevPostNr) = 5 THEN KOrdreHode.LevPostNr = SUBSTRING(KOrdreHode.LevPostNr,1,3) + ' ' + SUBSTRING(KOrdreHode.LevPostNr,4,2).
            IF TRIM(tt_shippingAdress.countryID) <> '' THEN
            DO: 
              IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
              FIND NumLandKode NO-LOCK WHERE
                   NumLandKode.NumLandKode = INTEGER(tt_shippingAdress.countryID) NO-ERROR.
              IF NOT AVAILABLE NumLandKode THEN  
                FIND AlfaLandKode NO-LOCK WHERE 
                     AlfaLandKode.AlfaKode3 = TRIM(tt_shippingAdress.countryID) NO-ERROR.
              IF NOT AVAILABLE NumLandKode AND NOT AVAILABLE AlfaLandKode THEN  
                FIND AlfaLandKode NO-LOCK WHERE 
                     AlfaLandKode.AlfaKode2 = TRIM(tt_shippingAdress.countryID) NO-ERROR.
              IF NOT AVAILABLE NumLandKode AND AVAILABLE AlfaLandKode THEN 
                FIND NumLandKode NO-LOCK WHERE
                     NumLandKode.NumLandKode = AlfaLandKode.NumLandKode NO-ERROR.                     
              /* Fakturaadresse */
              ASSIGN
              KOrdreHode.LevLand             = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE '')
              NO-ERROR.
            END.
            IF KOrdreHode.LevAdresse1 = '' AND KOrdreHode.LevAdresse2 = '' THEN 
              ASSIGN 
              KOrdreHode.LevAdresse1          = KOrdreHode.FaktAdresse1
              KOrdreHode.LevAdresse2          = KOrdreHode.FaktAdresse2
              KOrdreHode.LevPostNr            = KOrdreHode.FaktPostNr
              KOrdreHode.LevPostSted          = KOrdreHode.FaktPostSted
              KOrdreHode.LevLand              = KOrdreHode.FaktLand NO-ERROR.
         END.
       ASSIGN KOrdreHode.ValKod = tt_lineItemContainer.currencyID. /* Valutakode */

       IF AVAILABLE Kunde THEN DO:
         FIND CURRENT Kunde EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF AVAILABLE Kunde THEN DO:
           ASSIGN
             Kunde.Navn          = tt_billingAddress.displayName
             Kunde.DeresRef      = tt_billingAddress.displayName
             Kunde.Adresse1      = tt_billingAddress.street 
             Kunde.Adresse2      = tt_billingAddress.addressExtension
             Kunde.PostNr        = tt_billingAddress.zipcode 
             Kunde.ePostAdresse  = tt_billingAddress.EMail                
             Kunde.Telefon       = tt_billingAddress.phone 
             Kunde.Telefaks      = tt_billingAddress.phoneCell
             Kunde.MobilTlf      = tt_billingAddress.phoneCell
             Kunde.FaktAdresse1  = tt_billingAddress.street 
             Kunde.FaktAdresse2  = tt_billingAddress.addressExtension
             Kunde.FaktPostNr    = tt_billingAddress.zipcode 
             Kunde.LevAdresse1   = tt_billingAddress.street 
             Kunde.LevAdresse2   = tt_billingAddress.addressExtension
             Kunde.LevPostNr     = tt_billingAddress.zipcode
             Kunde.Butik         = KOrdreHode.ButikkNr.
             IF bSvenskFormat AND LENGTH(Kunde.PostNr) = 5 THEN 
               IF LENGTH(Kunde.PostNr) = 5 THEN Kunde.PostNr = SUBSTRING(Kunde.PostNr,1,3) + ' ' + SUBSTRING(Kunde.PostNr,4,2).
           FIND CURRENT Kunde NO-LOCK.
         END.
         ELSE
          FIND Kunde NO-LOCK WHERE
             Kunde.EksterntKundeNr = hentSisteEntry(tt_Order.customer) NO-ERROR.           
       END.
       IF NEW KOrdreHode THEN 
       VARELINJER:
       FOR EACH tt_ProductLineItem NO-LOCK BREAK BY tt_ProductLineItem.SeqNr:
         FIND ArtBas NO-LOCK WHERE ArtBAs.ArtikkelNr = DECIMAL(tt_ProductLineItem.ArtikkelNr) NO-ERROR.
         IF AVAILABLE ArtBas THEN 
           FIND ArtPris OF ArtBas NO-LOCK WHERE ArtPris.ProfilNr = bNettButikk.ProfilNr NO-ERROR.
         IF NOT AVAILABLE ArtPris AND AVAILABLE ArtBas THEN 
           FIND ArtPris OF ArtBas NO-LOCK WHERE ArtPris.ProfilNr = bSentrallager.ProfilNr NO-ERROR.
         
         FIND FIRST Moms NO-LOCK WHERE Moms.MomsProc = DECIMAL(REPLACE(tt_ProductLineItem.taxRate,',','.')) NO-ERROR.
         FIND StrKonv NO-LOCK WHERE StrKonv.StrKode = INTEGER(TT_productLineItem.StrKode) NO-ERROR.
         FIND FIRST Strekkode OF ArtBas NO-LOCK WHERE Strekkode.STrKode = INTEGER(TT_productLineItem.StrKode) NO-ERROR.
         IF AVAILABLE ArtBas THEN 
           FIND Lager NO-LOCK WHERE
             Lager.ArtikkelNr = DECIMAL(tt_ProductLineItem.ArtikkelNr) AND 
             Lager.Butik      = bNettButikk.Butik NO-ERROR.
         FIND LAST KOrdrelinje NO-LOCK WHERE KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
         IF AVAILABLE KOrdreLinje THEN 
           iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
         ELSE
           iLinjeNr = 1.
         RUN SkapaKOLinje ("Produkt",iLinjeNr).
         
         IF AVAILABLE ArtBas THEN DO:
           CASE iVgLopNr:
             WHEN 1 THEN 
                    ASSIGN KOrdreLinje.VareTekst = IF ArtBas.LopNr <> ? 
                                                      THEN STRING(ArtBas.VG) + '-' + STRING(ArtBas.LopNr) 
                                                      ELSE KOrdreLinje.VareTekst.
             WHEN 2 THEN  
                    ASSIGN KOrdreLinje.VareTekst = ArtBas.Beskr.                 
           END CASE. 
         END.
         /* Oppdaterer sumfelt i KOrdreHode. */
         hKOrdreLinje = BUFFER KOrdreLinje:HANDLE.
         RUN kordrelinje_post_update.p (hKOrdreLinje,'','',OUTPUT ocValue).
           
       END. /* VARELINJER */
       /* Fraktmetode */
       IF NEW KOrdreHode THEN 
       FOR EACH TT_shippingLineItem NO-LOCK WHERE
         DECIMAL(TT_shippingLineItem.lineItemPrice) <> 0:
         ASSIGN cFraktMetode = TRIM(ENTRY(NUM-ENTRIES(TT_shippingLineItem.shippingMethod,'/'),TT_shippingLineItem.shippingMethod,'/')) NO-ERROR.
         FIND FIRST Moms NO-LOCK WHERE
           Moms.MomsProc = DECIMAL(REPLACE(TT_shippingLineItem.taxRate,',','.')) NO-ERROR.
         FIND LAST KOrdrelinje NO-LOCK WHERE
           KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
         IF AVAILABLE KOrdreLinje THEN 
           iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
         ELSE
           iLinjeNr = 1.
         RUN SkapaKOLinje ("Frakt",iLinjeNr).
         ASSIGN KOrdreHode.LevFNr = INTEGER(cFraktMetode) NO-ERROR.
         IF ERROR-STATUS:ERROR AND cFraktMetode <> '' THEN DO:
           FIND FIRST Leveringsform NO-LOCK WHERE
             Leveringsform.LevFormMetode = cFraktMetode NO-ERROR.
           IF AVAILABLE Leveringsform THEN
             KOrdreHode.LevFNr = Leveringsform.LevFNr.
           ELSE DO:
             FIND LAST LeveringsForm NO-LOCK NO-ERROR.
             IF AVAILABLE LeveringsForm THEN
               KOrdreHode.LevFNr = LeveringsFor.LevFNr + 1.
             ELSE
               KOrdreHode.LevFNr = 1.
             CREATE LeveringsForm.
             ASSIGN
               LeveringsForm.LevFNr        = KOrdreHode.LevFNr
               LeveringsForm.LevFormMetode = cFraktMetode.
           END. 
         END.
         IF KOrdreHode.LevFNr > 0 THEN DO:
           FIND Leveringsform EXCLUSIVE-LOCK WHERE Leveringsform.LevFNr = KOrdreHode.LevFNr NO-ERROR.
           IF NOT AVAILABLE Leveringsform THEN DO:
             CREATE Leveringsform.
             ASSIGN Leveringsform.LevFNr = KOrdreHode.LevFNr.
           END.
           IF TT_shippingLineItem.Name <> '' THEN 
             ASSIGN Leveringsform.LevFormMetode = TT_shippingLineItem.Name.
         END.
         IF AVAILABLE LeveringsForm THEN RELEASE Leveringsform.
       END. /* Fraktmetode */.
       
     END. /* OPPRETTELSE_ORDRE */
     RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: Ordre behandling. ' + ' Ny ordre: ' + STRING(NEW KORdreHode) + ' Kordre_Id: ' + STRING(KOrdreHode.KOrdre_ID) + ' og EksterntId: ' + STRING(KOrdreHode.EkstOrdreNr) + ' Kunde: ' + STRING(KOrdreHode.KundeNr)).
     IF NEW KOrdreHode THEN 
     FOR EACH tt_paymentLineItem NO-LOCK:
       ASSIGN cBetType = ENTRY(NUM-ENTRIES(tt_paymentLineItem.paymentType,'/'),tt_paymentLineItem.paymentType,'/') NO-ERROR.
       FIND LAST KOrdrelinje NO-LOCK WHERE KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
       IF AVAILABLE KOrdreLinje THEN 
         iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
       ELSE
         iLinjeNr = 1.
       RUN SkapaKOLinje ("Payment",iLinjeNr).
       RUN SetBetType(cBetType).
       ASSIGN cBetType = tt_paymentLineItem.bbsEpayment.
       CASE cBetType:
         WHEN 'Epayment Pro' THEN 
                           ASSIGN KORdreLinje.Varetekst = 'BBS Betaling'
                                  KORdreLinje.Varetekst = 'BETALT'.
       END CASE.         
     END. /* Betaling */
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrepTempTabell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepTempTabell Procedure 
PROCEDURE PrepTempTabell :
/*------------------------------------------------------------------------------
                        Purpose: Tømmer temp-tabell                                                                                                                                       
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    /* Det kommer ALLTID bare en kunde pr. fil.         */
    /* Renser opp i temptabell før innlesning begynner. */
    FOR EACH TT_Order:             DELETE TT_Order.             END.
    FOR EACH TT_BillingAddress:    DELETE TT_BillingAddress.    END.
    FOR EACH TT_ShippingAdress:    DELETE TT_ShippingAdress.    END.
    FOR EACH TT_lineItemContainer: DELETE TT_lineItemContainer. END.
    FOR EACH TT_productLineItem:   DELETE TT_productLineItem.   END.
    FOR EACH TT_taxLineItem:       DELETE TT_taxLineItem.       END.
    FOR EACH TT_paymentLineItem:   DELETE TT_paymentLineItem.   END.
    FOR EACH TT_shippingLineItem:  DELETE TT_shippingLineItem.  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetBetType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBetType Procedure 
PROCEDURE SetBetType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cBetType AS CHARACTER   NO-UNDO.
         CASE cBetType:
           WHEN 'bbsEpayment' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betalt bbsEPayment'
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1.
           WHEN 'bbsNetaxept' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betalt bbsNetaxept'
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1.
           WHEN 'Klarna' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Faktureras via Klarna (Klarna)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'klarna_checkout' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Faktureras via Klarna (KlarnaCheckOut)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'paypal' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betalt via PayPal' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'zaarpay' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Faktureras via Klarna (zaarPay)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'Invoice' THEN 
                            ASSIGN KORdreLinje.Varetekst     = 'Betales via faktura kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr        = 'KREDIT'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           WHEN 'Faktura' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betales via faktura kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr    = 'KREDIT'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           WHEN 'postoppkrav' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Postoppkrav kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr    = 'POSTOPPKRAV'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           WHEN 'oppkrav' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Postoppkrav kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr    = 'POSTOPPKRAV'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           OTHERWISE 
               ASSIGN KORdreLinje.Varetekst = 'Ukjent betalingsmåte'
                      KORdreLinje.VareNr    = 'BETALT'.
         END CASE.         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaKOLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaKOLinje Procedure 
PROCEDURE SkapaKOLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cTyp AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iLinjeNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER   NO-UNDO.
    IF cTyp = "Produkt" THEN DO:
        IF AVAIL Artbas AND AVAIL StrKonv THEN DO:
            FIND FIRST Strekkode OF artbas WHERE StrekKode.StrKode = strkonv.strkode NO-LOCK NO-ERROR.
            IF AVAIL StrekKode THEN
                cKode = strekkode.kode.
        END.
        CREATE KOrdreLinje.
        ASSIGN
        KOrdreLinje.KOrdre_ID         = KOrdreHode.KOrdre_Id
        KOrdreLinje.KOrdreLinjeNr     = iLinjeNr 
        KOrdreLinje.VareNr            = TRIM(tt_ProductLineItem.ArtikkelNr)
        KOrdreLinje.Kode              = cKode
        KOrdreLinje.Varetekst         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '** Ukjent ' + KOrdreLinje.VareNr)
        KOrdreLinje.Antall            = DECIMAL(tt_ProductLineItem.quantity)
        KOrdreLinje.NettoPris         = ROUND(DECIMAL(tt_ProductLineItem.lineItemPrice) / KOrdreLinje.Antall,2)
        KOrdreLinje.NettoLinjesum     = KOrdreLinje.NettoPris * KOrdreLinje.Antall
        KOrdreLinje.LinjeRabattKr     = DECIMAL(tt_ProductLineItem.discount)
        KOrdreLinje.LinjeRab%         = (KOrdreLinje.LinjeRabattKr / (KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr)) * 100  
        KOrdreLinje.LinjeRab%         = IF KOrdreLinje.LinjeRab% = ? THEN 0 ELSE KOrdreLinje.LinjeRab%  
    
        KOrdreLinje.MomsKod           = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0)
        /*KOrdreLinje.MvaKr             = DECIMAL(tt_ProductLineItem.taxAmount)*/
        KOrdreLinje.MvaKr             = DECIMAL(tt_ProductLineItem.lineItemPrice) - (DECIMAL(tt_ProductLineItem.lineItemPrice) / 1 + DECIMAL(tt_ProductLineItem.taxRate))
        KOrdreLinje.Storl             = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
        KOrdreLinje.Mva%              = DECIMAL(tt_ProductLineItem.taxRate) * 100
        KOrdreLinje.StrKode           = INTEGER(TT_productLineItem.StrKode)
        KOrdreLinje.VareKost          = (IF AVAILABLE Lager THEN Lager.VVareKost ELSE 0)
    
        KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = ? OR KOrdreLinje.VareKost <= 0) THEN 0 ELSE KOrdreLinje.VareKost
        KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = 0 AND AVAILABLE ArtPris) THEN  ArtPris.VareKost[1] ELSE KOrdreLinje.VareKost
    
        KOrdreLinje.BruttoPris        = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall)  
        KOrdreLinje.Pris              = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall) 
        KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr 
        KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
        KOrdreLinje.Db%               = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
        KOrdreLinje.Db%               = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%) 
        KOrdreLinje.RefNr             = 0
        KOrdreLinje.RefTekst          = ''
        KOrdreLinje.Bestillingsnummer = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
        KOrdreLinje.LevFargKod        = IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE ''
        KOrdreLinje.ValKod            = KOrdreHode.ValKod           
        NO-ERROR.
    END.
    ELSE IF cTyp = "Frakt" THEN DO:
        FIND FIRST SysPara NO-LOCK WHERE
          SysPara.SysHId       = 150 AND  
          SysPara.SysGr        = 10 AND  
          SysPara.Beskrivelse  = "Posten" NO-ERROR.

        CREATE KOrdreLinje.
        ASSIGN             
          KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
          KOrdreLinje.KOrdreLinjeNr = iLinjeNr
          KOrdreLinje.Varetekst     = 'FRAKT - ' + TT_shippingLineItem.NAME 
          /* ArtikkelNr */
          KOrdreLinje.VareNr        = (IF AVAILABLE Syspara THEN Syspara.Parameter1 ELSE '')
          /* Betaling */
          KOrdreLinje.MomsKod       = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0)
          KOrdreLinje.MvaKr         = DECIMAL(TT_shippingLineItem.taxAmount)
          KOrdreLinje.Mva%          = DECIMAL(TT_shippingLineItem.taxRate) * 100
          KOrdreLinje.Antall        = DECIMAL(TT_shippingLineItem.quantity)
          KOrdreLinje.NettoPris     = DECIMAL(TT_shippingLineItem.lineItemPrice)
          KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
          KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
          KOrdreLinje.Pris          = KOrdreLinje.NettoPris + KOrdreLinje.MvaKr 
          KOrdreLinje.Linjesum      = KOrdreLinje.NettoLinjesum + KOrdreLinje.MvaKr
          .   

        IF KOrdreLinje.Mva% = 0 THEN 
          DO:
            ASSIGN 
            KOrdreLinje.Mva%          = 25
            KOrdreLinje.MvaKr         = KOrdreLinje.NettoLinjesum - (KOrdreLinje.NettoLinjesum / (1 + (KOrdreLinje.Mva% / 100)))
            .             
          END.         
    END.
    IF cTyp = "Payment" THEN DO:
        FIND FIRST Moms NO-LOCK WHERE
          Moms.MomsProc = DECIMAL(REPLACE(tt_paymentLineItem.taxRate,',','.')) NO-ERROR.

      CREATE KOrdreLinje.
      ASSIGN
        KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
        KOrdreLinje.KOrdreLinjeNr = iLinjeNr 
        /* Betaling */
        KOrdreLinje.MomsKod       = IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0
        KOrdreLinje.Antall        = DECIMAL(tt_paymentLineItem.quantity)
        KOrdreLinje.NettoPris     = DECIMAL(tt_paymentLineItem.basePrice) * -1
        KOrdreLinje.MvaKr         = (DECIMAL(tt_paymentLineItem.basePrice) - 
                                    (
                                     DECIMAL(tt_paymentLineItem.basePrice) / (1 + DECIMAL(tt_paymentLineItem.taxRate))
                                    )) * -1
        KORdreLinje.Mva%          = DECIMAL(tt_paymentLineItem.taxRate) * 100
        KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
        KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
        KOrdreLinje.Pris          = KOrdreLinje.NettoPris 
        KOrdreLinje.Linjesum      = KOrdreLinje.NettoPris
        .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateTT_tables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateTT_tables Procedure 
PROCEDURE UpdateTT_tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cIPtype AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cField AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cValue AS CHARACTER   NO-UNDO.

IF cIPtype = "Order" THEN DO:
    CASE cField:
        WHEN 'path'               THEN TT_Order.Path = cValue.
        WHEN 'ALIAS'              THEN TT_Order.cAlias  = cValue.
        WHEN 'customer'           THEN TT_Order.customer = cValue.
        WHEN 'user'               THEN TT_Order.cuser = cValue.
        WHEN 'creationDate'       THEN TT_Order.creationDate = cValue.
        WHEN 'viewedOn'           THEN TT_Order.viewedOn = cValue.
        WHEN 'cancelledOn'        THEN TT_Order.cancelledOn = cValue.
        WHEN 'inProcessOn'        THEN TT_Order.inProcessOn = cValue.
        WHEN 'pendingOn'          THEN TT_Order.pendingOn = cValue.
        WHEN 'readyForShippingOn' THEN TT_Order.readyForShippingOn = cValue.
        WHEN 'partlyDispatchedOn' THEN TT_Order.partlyDispatchedOn = cValue.
        WHEN 'dispatchedOn'       THEN TT_Order.dispatchedOn = cValue.
        WHEN 'shippedOn'          THEN TT_Order.shippedOn = cValue.
        WHEN 'partlyPaidOn'       THEN TT_Order.partlyPaidOn = cValue.
        WHEN 'paidOn'             THEN TT_Order.paidOn = cValue.
        WHEN 'closedOn'           THEN TT_Order.closedOn = cValue.
        WHEN 'archivedOn'         THEN TT_Order.archivedOn = cValue.
        WHEN 'partlyInvoicedOn'   THEN TT_Order.partlyInvoicedOn = cValue.
        WHEN 'invoicedOn'         THEN TT_Order.invoicedOn = cValue.
        WHEN 'internalComment'    THEN TT_Order.internalComment = cValue.
        WHEN 'customerComment'    THEN TT_Order.customerComment = cValue.
    END CASE.
END.
ELSE IF cIPtype = "Billing" THEN DO:
    CASE cField:
        WHEN 'alias'         THEN TT_BillingAddress.cAlias = cValue.
        WHEN 'displayName'   THEN TT_BillingAddress.displayName = cValue.
        WHEN 'street'        THEN TT_BillingAddress.street = cValue.
        WHEN 'zipcode'       THEN TT_BillingAddress.zipcode = cValue.
        WHEN 'city'          THEN TT_BillingAddress.city = cValue.         
        WHEN 'state'         THEN TT_BillingAddress.state = cValue.
        WHEN 'countryID'     THEN TT_BillingAddress.countryID = cValue.
        WHEN 'EMail'         THEN TT_BillingAddress.EMail = cValue.
        WHEN 'phone'         THEN TT_BillingAddress.phone = cValue.
        WHEN 'fax'           THEN TT_BillingAddress.fax = cValue.
        WHEN 'salutation'    THEN TT_BillingAddress.salutation = cValue.
        WHEN 'TITLE'         THEN TT_BillingAddress.cTITLE = cValue.
        WHEN 'firstName'     THEN TT_BillingAddress.firstName = cValue.
        WHEN 'middleName'    THEN TT_BillingAddress.middleName = cValue.
        WHEN 'lastName'      THEN TT_BillingAddress.lastName = cValue.
        WHEN 'EMailPrivate'  THEN TT_BillingAddress.EMailPrivate = cValue.
        WHEN 'EMailBusiness' THEN TT_BillingAddress.EMailBusiness = cValue.
        WHEN 'phonePrivate'  THEN TT_BillingAddress.phonePrivate = cValue.
        WHEN 'phoneBusiness' THEN TT_BillingAddress.phoneBusiness = cValue.
        WHEN 'phoneCell'     THEN TT_BillingAddress.phoneCell = cValue.
        WHEN 'gender'        THEN TT_BillingAddress.gender = cValue.
        WHEN 'company'       THEN TT_BillingAddress.company = cValue.
        WHEN 'department'    THEN TT_BillingAddress.department = cValue.
        WHEN 'jobTitle'      THEN TT_BillingAddress.jobTitle = cValue.
        WHEN 'birthday'      THEN TT_BillingAddress.birthday = cValue.
        WHEN 'VATID'         THEN TT_BillingAddress.VATID = cValue.
        WHEN 'bankCode'      THEN TT_BillingAddress.bankCode = cValue.
        WHEN 'bankName'      THEN TT_BillingAddress.bankName = cValue.
        WHEN 'bankAccountNo' THEN TT_BillingAddress.bankAccountNo = cValue.
        WHEN 'URL'           THEN TT_BillingAddress.cURL = cValue.
    END CASE.
END.
ELSE IF cIPtype = "Shipping" THEN DO:
    CASE cField:
        WHEN 'alias'         THEN TT_ShippingAdress.cAlias = cValue.
        WHEN 'displayName'   THEN TT_ShippingAdress.displayName = cValue.
        WHEN 'street'        THEN TT_ShippingAdress.street = cValue.
        WHEN 'zipcode'       THEN TT_ShippingAdress.zipcode = cValue.
        WHEN 'city'          THEN TT_ShippingAdress.city = cValue.         
        WHEN 'state'         THEN TT_ShippingAdress.state = cValue.
        WHEN 'countryID'     THEN TT_ShippingAdress.countryID = cValue.
        WHEN 'EMail'         THEN TT_ShippingAdress.EMail = cValue.
        WHEN 'phone'         THEN TT_ShippingAdress.phone = cValue.
        WHEN 'fax'           THEN TT_ShippingAdress.fax = cValue.
        WHEN 'salutation'    THEN TT_ShippingAdress.salutation = cValue.
        WHEN 'TITLE'         THEN TT_ShippingAdress.cTITLE = cValue.
        WHEN 'firstName'     THEN TT_ShippingAdress.firstName = cValue.
        WHEN 'middleName'    THEN TT_ShippingAdress.middleName = cValue.
        WHEN 'lastName'      THEN TT_ShippingAdress.lastName = cValue.
        WHEN 'EMailPrivate'  THEN TT_ShippingAdress.EMailPrivate = cValue.
        WHEN 'EMailBusiness' THEN TT_ShippingAdress.EMailBusiness = cValue.
        WHEN 'phonePrivate'  THEN TT_ShippingAdress.phonePrivate = cValue.
        WHEN 'phoneBusiness' THEN TT_ShippingAdress.phoneBusiness = cValue.
        WHEN 'phoneCell'     THEN TT_ShippingAdress.phoneCell = cValue.
        WHEN 'gender'        THEN TT_ShippingAdress.gender = cValue.
        WHEN 'company'       THEN TT_ShippingAdress.company = cValue.
        WHEN 'department'    THEN TT_ShippingAdress.department = cValue.
        WHEN 'jobTitle'      THEN TT_ShippingAdress.jobTitle = cValue.
        WHEN 'birthday'      THEN TT_ShippingAdress.birthday = cValue.
        WHEN 'VATID'         THEN TT_ShippingAdress.VATID = cValue.
        WHEN 'bankCode'      THEN TT_ShippingAdress.bankCode = cValue.
        WHEN 'bankName'      THEN TT_ShippingAdress.bankName = cValue.
        WHEN 'bankAccountNo' THEN TT_ShippingAdress.bankAccountNo = cValue.
        WHEN 'URL'           THEN TT_ShippingAdress.URL = cValue.
    END CASE.
END.
ELSE IF cIPtype = "LineItemContainer" THEN DO:
    CASE cField:
        WHEN 'localeID'       THEN TT_lineItemContainer.localeID = cValue.
        WHEN 'languageCode'   THEN TT_lineItemContainer.languageCode = cValue.
        WHEN 'currencyID'     THEN TT_lineItemContainer.currencyID = cValue.
        WHEN 'taxArea'        THEN TT_lineItemContainer.taxArea = cValue.
        WHEN 'taxAreaName'    THEN TT_lineItemContainer.taxAreaName = cValue.         
        WHEN 'taxModel'       THEN TT_lineItemContainer.taxModel = cValue.
        WHEN 'grandTotal'     THEN TT_lineItemContainer.grandTotal = cValue.
        WHEN 'totalBeforeTax' THEN TT_lineItemContainer.totalBeforeTax = cValue.
        WHEN 'totalTax'       THEN TT_lineItemContainer.totalTax = cValue.
    END CASE.
END.
ELSE IF cIPtype = "productLineItem" THEN DO:
    CASE cField:
        WHEN 'name'          THEN TT_productLineItem.name = cValue.
        WHEN 'SKU'           THEN TT_productLineItem.SKU = cValue.
        WHEN 'product'       THEN TT_productLineItem.product = cValue.
        WHEN 'taxClass'      THEN TT_productLineItem.taxClass = cValue.
        WHEN 'lineItemPrice' THEN TT_productLineItem.lineItemPrice = cValue.         
        WHEN 'basePrice'     THEN TT_productLineItem.basePrice = cValue.
        WHEN 'quantity'      THEN TT_productLineItem.quantity = cValue.
        WHEN 'discount'      THEN TT_productLineItem.discount = cValue.
        WHEN 'taxRate'       THEN TT_productLineItem.taxRate = cValue.
        WHEN 'taxAmount'     THEN TT_productLineItem.taxAmount = cValue.
    END CASE.
END.
ELSE IF cIPtype = "taxLineItem" THEN DO:
    CASE cField:
        WHEN 'name'          THEN TT_taxLineItem.name = cValue.
        WHEN 'taxArea'       THEN TT_taxLineItem.taxArea = cValue.
        WHEN 'taxMatrix'     THEN TT_taxLineItem.taxMatrix = cValue.
        WHEN 'taxClass'      THEN TT_taxLineItem.taxClass = cValue.
        WHEN 'lineItemPrice' THEN TT_taxLineItem.lineItemPrice = cValue.
        WHEN 'basePrice'     THEN TT_taxLineItem.basePrice = cValue.
        WHEN 'quantity'      THEN TT_taxLineItem.quantity = cValue.
        WHEN 'discount'      THEN TT_taxLineItem.discount = cValue.
        WHEN 'taxRate'       THEN TT_taxLineItem.taxRate = cValue.
        WHEN 'taxAmount'     THEN TT_taxLineItem.taxAmount = cValue.
        WHEN 'ext:MomsKod'   THEN TT_taxLineItem.MomsKod = cValue.
    END CASE.
END.
ELSE IF cIPtype = "paymentLineItem" THEN DO:
    CASE cField:
        WHEN 'name'          THEN TT_paymentLineItem.name = cValue.
        WHEN 'paymentMethod' THEN TT_paymentLineItem.paymentMethod = cValue.
        WHEN 'paymentType'   THEN TT_paymentLineItem.paymentType = cValue.
        WHEN 'taxClass'      THEN TT_paymentLineItem.taxClass = cValue.
        WHEN 'lineItemPrice' THEN TT_paymentLineItem.lineItemPrice = cValue.         
        WHEN 'basePrice'     THEN TT_paymentLineItem.basePrice = cValue.
        WHEN 'quantity'      THEN TT_paymentLineItem.quantity = cValue.
        WHEN 'discount'      THEN TT_paymentLineItem.discount = cValue.
        WHEN 'taxRate'       THEN TT_paymentLineItem.taxRate = cValue.
        WHEN 'taxAmount'     THEN TT_paymentLineItem.taxAmount = cValue.
        WHEN 'bbsEpayment'   THEN TT_paymentLineItem.bbsEpayment = cValue.
    END CASE.
END.
ELSE IF cIPtype = "shippingLineItem" THEN DO:
    CASE cField:
        WHEN 'name'           THEN TT_shippingLineItem.name           = cValue.
        WHEN 'shippingMethod' THEN TT_shippingLineItem.shippingMethod = cValue.
        WHEN 'lineItemPrice'  THEN TT_shippingLineItem.lineItemPrice  = cValue.
        WHEN 'basePrice'      THEN TT_shippingLineItem.basePrice      = cValue.
        WHEN 'quantity'       THEN TT_shippingLineItem.quantity       = cValue.         
        WHEN 'discount'       THEN TT_shippingLineItem.discount       = cValue.
        WHEN 'taxRate'        THEN TT_shippingLineItem.taxRate        = cValue.
        WHEN 'taxAmount'      THEN TT_shippingLineItem.taxAmount      = cValue.
    END CASE.
END.

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

&IF DEFINED(EXCLUDE-hentDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentDato Procedure 
FUNCTION hentDato RETURNS DATE
        ( INPUT cDatoTid AS CHAR ):

        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

                DEFINE VARIABLE dDato AS DATE NO-UNDO.
                
                IF cDatoTid = '' THEN
          dDato = ?.
        ELSE dDato = DATE(REPLACE(ENTRY(1,cDatoTid),'.','/')) NO-ERROR.
                
                RETURN dDato.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentSisteEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentSisteEntry Procedure 
FUNCTION hentSisteEntry RETURNS CHARACTER
        ( INPUT cTekst AS CHARACTER ):

        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

                DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

        IF cTekst = '' AND NUM-ENTRIES(cTekst,'/') = 0 THEN
            cResult = ''.
        ELSE cResult = ENTRY(NUM-ENTRIES(cTekst,'/'),cTekst,'/').    
         
                RETURN cResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentTid Procedure 
FUNCTION hentTid RETURNS INTEGER
        ( INPUT cDatoTid AS CHARACTER ):

        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes: <creationDate>24.04.2009 22:30:15</creationDate>                                                                                                                                           
        ------------------------------------------------------------------------------*/

                DEFINE VARIABLE iTid AS INTEGER NO-UNDO.
                DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.

        IF cDatoTid = '' THEN
            iTid = 0.
        ELSE ASSIGN
             cTid = ENTRY(2,cDatoTid)
             iTid = INT(ENTRY(1,cTid,":")) * 3600 + INT(ENTRY(2,cTid,":")) * 60 + INT(ENTRY(3,cTid,":")) NO-ERROR. 
         
                RETURN iTid.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

