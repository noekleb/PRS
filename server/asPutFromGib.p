&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : asPutFromGib.p
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
DEFINE INPUT  PARAMETER cGibType   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER lcBlobData AS LONGCHAR     NO-UNDO.
DEFINE OUTPUT PARAMETER lOK        AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cReturnMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lWriteOK    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iWebLager   AS INT NO-UNDO.
DEFINE VARIABLE iWebButikk  AS INTEGER NO-UNDO.
DEFINE VARIABLE iMedlKlubb  AS INTEGER NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFraktVareNr AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG       NO-UNDO.

DEF VAR KundeDataSet        AS HANDLE NO-UNDO.
DEF VAR OrderDataSet AS HANDLE NO-UNDO.
DEF VAR CancelKOrdreDataSet AS HANDLE NO-UNDO.

DEFINE VARIABLE hJbAPI AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttt_OvBuffer LIKE OvBuffer.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

{asPutCustomer.i}
{asPutOrder.i}
{asPutCancelOrder.i}

CREATE DATASET KundeDataSet.
KundeDataSet:SERIALIZE-NAME   = "tt_Customer".
KundeDataSet:ADD-BUFFER(TEMP-TABLE tt_Customer:DEFAULT-BUFFER-HANDLE).

CREATE DATASET OrderDataSet.
OrderDataSet:SERIALIZE-NAME   = "Order".
OrderDataSet:ADD-BUFFER(TEMP-TABLE tt_orderHeader:DEFAULT-BUFFER-HANDLE).
OrderDataSet:ADD-BUFFER(TEMP-TABLE tt_orderLine:DEFAULT-BUFFER-HANDLE).
OrderDataSet:ADD-BUFFER(TEMP-TABLE tt_payments:DEFAULT-BUFFER-HANDLE).
OrderDataSet:ADD-RELATION(BUFFER tt_orderHeader:HANDLE, BUFFER tt_orderLine:HANDLE,"internnr,internnr").
OrderDataSet:ADD-RELATION(BUFFER tt_orderHeader:HANDLE, BUFFER tt_payments:HANDLE,"internnr,internnr").

CREATE DATASET CancelKOrdreDataSet.
ASSIGN
CancelKOrdreDataSet:SERIALIZE-NAME = "CancelKOrdre".
CancelKOrdreDataSet:ADD-BUFFER(TEMP-TABLE tt_CancelKOrdreHode:DEFAULT-BUFFER-HANDLE).
CancelKOrdreDataSet:ADD-BUFFER(TEMP-TABLE tt_CancelKOrdreLinje:DEFAULT-BUFFER-HANDLE).
CancelKOrdreDataSet:ADD-RELATION(BUFFER tt_CancelKOrdreHode:HANDLE, BUFFER tt_CancelKOrdreLinje:HANDLE,"EkstOrdreNr,EkstOrdreNr").

DEFINE BUFFER bNettButikk  FOR Butiker.
DEFINE BUFFER bSentallager FOR Butiker.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getFilId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilId Procedure 
FUNCTION getFilId RETURNS CHARACTER
  (  ) FORWARD.

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
         HEIGHT             = 14.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF NOT VALID-HANDLE(hJbAPI) THEN
    RUN jbserv_api_for_server.p PERSIST SET hJbAPI ("validsession").
SESSION:ADD-SUPER-PROCEDURE(hJbAPI).

ASSIGN
    bTest = FALSE  
    cLogg = 'asPutFromGib' + REPLACE(STRING(TODAY),'/','')
    .

IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'Start.'). 

{syspara.i 150 1 3  iWebLager INT}
{syspara.i 150 1 2  iWebButikk INT}
{syspara.i 14  1 20 iMedlKlubb INT}
FIND FIRST SysPara NO-LOCK WHERE
    SysPara.SysHId       = 150 AND  
    SysPara.SysGr        = 10 AND  
    SysPara.Beskrivelse  = "Posten" NO-ERROR.
IF AVAILABLE SysPara THEN
    cFraktVareNr = Syspara.Parameter1.

FIND bNettButikk NO-LOCK WHERE 
    bNettButikk.Butik = iWebButikk NO-ERROR.
IF NOT AVAILABLE bNettButikk THEN 
DO:
    MESSAGE 'Ukjent nettbutikk ' + STRING(iWebButikk) + '.'.
    RETURN.
END.
FIND bSentallager NO-LOCK WHERE 
    bSentallager.Butik = iWebButikk NO-ERROR.
IF NOT AVAILABLE bSentallager THEN 
DO:
    MESSAGE 'Ukjent Sentallager ' + STRING(iWebLager) + '.'.
    RETURN.
END.

IF cGibType <> '' THEN 
CASE cGibType:
    WHEN "ORDER" THEN DO:
        RUN putOrder (lcBlobData,OUTPUT lOK,OUTPUT cReturnMsg).
    END.
    WHEN "CUSTOMER" THEN DO:

        RUN putCustomer (lcBlobData,OUTPUT lOK,OUTPUT cReturnMsg).
    END.
END CASE.

IF VALID-HANDLE(hJbAPI) THEN 
    DELETE PROCEDURE hJbAPI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreUnknownCustomer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreUnknownCustomer Procedure 
PROCEDURE CreUnknownCustomer :
/*------------------------------------------------------------------------------
     Purpose: Kalles fra CreUpdOrder hvis kunde ikke er kjent.
              Tabell tt_orderHeader er tilgjengelig.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE piAntFelt   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pcForNavn   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcEtternavn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piLoop      AS INTEGER   NO-UNDO.
    
    IF NOT AVAILABLE tt_orderHeader THEN 
        RETURN.

    ASSIGN piantFelt = NUM-ENTRIES(tt_orderHeader.sh_name,' ').
    IF piAntFelt = 1 THEN 
        ASSIGN 
        pcFornavn   = tt_orderHeader.sh_name
        pcEtternavn = ''
        .    
    ELSE DO: 
        pcEtternavn = ENTRY(piAntFelt,tt_orderHeader.sh_name,' ').
        DO piLoop = 1 TO piAntFelt - 1:
            pcFornavn = pcFornavn +
                       (IF pcFornavn <> '' THEN ' 'ELSE '') + 
                       ENTRY(piLoop,tt_orderHeader.sh_name,' ').
        END.
    END.
        
    CREATE tt_Customer.
    ASSIGN
        tt_Customer.internnr                     = tt_orderHeader.internnr
        tt_Customer.customerId                   = tt_orderHeader.customerId
        tt_Customer.givenName                    = pcForNavn
        tt_Customer.middleName                   = ""
        tt_Customer.familyName                   = pcEtternavn
        tt_Customer.sh_addressLine               = tt_orderHeader.sh_addressLine
        tt_Customer.sh_cityName                  = tt_orderHeader.sh_cityName
        tt_Customer.sh_countrySubDivisionCode    = tt_orderHeader.sh_countrySubDivisionCode
        tt_Customer.sh_countryCode               = tt_orderHeader.sh_countryCode
        tt_Customer.sh_postalCode                = tt_orderHeader.sh_postalCode
        tt_Customer.bi_addressLine               = tt_orderHeader.bi_addressLine
        tt_Customer.bi_cityName                  = tt_orderHeader.bi_cityName
        tt_Customer.bi_countrySubDivisionCode    = tt_orderHeader.bi_countrySubDivisionCode
        tt_Customer.bi_countryCode               = tt_orderHeader.bi_countryCode
        tt_Customer.bi_postalCode                = tt_orderHeader.bi_postalCode
        tt_Customer.home_countryDialingCode      = ""
        tt_Customer.home_dialNumber              = ""
        tt_Customer.work_countryDialingCode      = ""
        tt_Customer.work_dialNumber              = ""
        tt_Customer.mobile_countryDialingCode    = ""
        tt_Customer.mobile_dialNumber            = ""
        tt_Customer.home_eMailAddress            = ""
        tt_Customer.home_htmlPreferenceIndicator = FALSE 
        tt_Customer.home_newsletterIndicator     = FALSE 
        tt_Customer.work_eMailAddress            = ""
        tt_Customer.work_htmlPreferenceIndicator = FALSE 
        tt_Customer.work_newsletterIndicator     = FALSE 
        tt_Customer.loyaltyProgramMembership     = FALSE 
        tt_Customer.preferredSalutationCode      = ""
        .
    
    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                                        '    Opprett tmpfile ukjent kunde: ' + STRING(tt_orderHeader.customerId) + 
                                        ' for ordre: ' + STRING(tt_orderHeader.internnr) 
                                        ). 
    RUN CreUpdCustomer.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreUpdCustomer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreUpdCustomer Procedure 
PROCEDURE CreUpdCustomer :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lMedlemsNr LIKE Medlem.MedlemsNr NO-UNDO.
    DEFINE VARIABLE bOk        AS LOG NO-UNDO.
    DEFINE VARIABLE cMsgs      AS CHARACTER NO-UNDO.


    FOR EACH tt_Customer NO-LOCK TRANSACTION:
        /* Henter eksisterende eller oppretter ny kunde. */
        FIND FIRST Kunde EXCLUSIVE-LOCK WHERE
            Kunde.EksterntKundeNr =  tt_Customer.customerId AND 
            Kunde.butikkNr        = iWebButikk NO-ERROR.
  
        /* NB: Kundenummer og kundekort opprettes automatisk av db trigger c_kunde.p */
        /* Her settes de felt som skal initieres ved ny kunde fra nettbutikk.        */
        IF NOT AVAILABLE Kunde THEN
        NYKUNDE:
        DO:
            FIND FIRST KundeType NO-LOCK NO-ERROR.
            FIND FIRST KundeGruppe NO-LOCK NO-ERROR.
            CREATE Kunde.                                     
            ASSIGN
                Kunde.EksterntKundeNr = tt_Customer.customerId
                Kunde.Kilde           = "Phønix"
                Kunde.TypeId          = IF AVAILABLE KundeType THEN KundeType.TypeId ELSE 0
                Kunde.GruppeId        = IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 0
                Kunde.RegistrertDato  = TODAY
                Kunde.RegistrertTid   = TIME
                Kunde.Etablert        = TODAY
                .
                
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                    '    CreUpdCustomer - Ny kunde:' +  
                    Kunde.EksterntKundeNr
                    ). 
                
        END. /* NYKUNDE */

        ASSIGN 
            Kunde.Butik        = iWebButikk
            Kunde.WebKunde     = TRUE 
            Kunde.Aktiv        = TRUE
            Kunde.Navn         = tt_Customer.givenName + " " + 
                                       tt_Customer.middleName + (IF tt_Customer.middleName <> '' THEN ' ' ELSE '') +
                                       tt_Customer.familyName
            Kunde.KontNavn     = Kunde.Navn 
                                             
            Kunde.Adresse1     = tt_Customer.sh_addressLine
            Kunde.Adresse2     = tt_Customer.sh_addressLine2            
            Kunde.PostNr       = tt_Customer.sh_postalCode
            Kunde.Land         = tt_Customer.sh_countryCode
            Kunde.ByNavn       = tt_customer.sh_cityName
                        
            Kunde.LevAdresse1  = tt_Customer.sh_addressLine
            Kunde.LevAdresse2  = tt_Customer.sh_addressLine2           
            Kunde.LevPostNr    = tt_Customer.sh_postalCode
            Kunde.LevLand      = tt_Customer.sh_countryCode
            
            Kunde.FaktAdresse1 = (IF Kunde.FaktAdresse1 <> '' THEN Kunde.FaktAdresse1 ELSE tt_Customer.bi_addressLine)
            Kunde.FaktAdresse2 = (IF Kunde.FaktAdresse2 <> '' THEN Kunde.FaktAdresse2 ELSE tt_Customer.bi_addressLine2)           
            Kunde.FaktPostNr   = (IF Kunde.FaktPostNr <> '' THEN Kunde.FaktPostNr ELSE tt_Customer.bi_postalCode)
            Kunde.FaktLand     = tt_Customer.bi_countryCode
            
            Kunde.Telefon      = (IF TRIM(tt_Customer.home_dialNumber) <> '' THEN tt_Customer.home_dialNumber ELSE Kunde.Telefon)
            Kunde.MobilTlf     = (IF TRIM(tt_Customer.mobile_dialNumber) <> '' THEN tt_Customer.mobile_dialNumber ELSE Kunde.MobilTlf)
            Kunde.KontTelefon  = Kunde.Telefon
            Kunde.KontMobilTlf = Kunde.MobilTlf
            Kunde.ePostAdresse = tt_Customer.home_eMailAddress
            Kunde.KontE-Post   = tt_Customer.work_eMailAddress
            
            Kunde.MottaeMailUtsendelser = tt_Customer.work_newsletterIndicator
            Kunde.WebKanSendeEMail      = tt_Customer.work_newsletterIndicator
            /*
            Kunde.FodtDato     = DATE(CustomerNew.dob)
            Kunde.Alder        = YEAR(Kunde.FodtDato) - YEAR(TODAY) + 1 NO-ERROR.
            */
        .
        IF tt_Customer.preferredSalutationCode BEGINS "Mr" THEN
            Kunde.Kjon = 1.
        ELSE IF tt_Customer.preferredSalutationCode BEGINS "Ms" THEN
                Kunde.Kjon = 2.
            ELSE
                Kunde.Kjon = 0.
        IF Kunde.MobilTlf = '' THEN 
            Kunde.MobilTlf = Kunde.Telefon.

        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                '    CreUpdCustomer - Oppdater kunde: ' +
                STRING(Kunde.KundeNr) + ' ' +  
                Kunde.Navn
                ). 
                
        IF tt_Customer.loyaltyProgramMembership THEN 
        DO:
            FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
            IF Kunde.MobilTlf <> '' THEN    
                FIND FIRST Medlem EXCLUSIVE-LOCK WHERE 
                    Medlem.MobilTlf = Kunde.MobilTlf NO-ERROR.
            IF NOT AVAILABLE Medlem AND Kunde.Telefon <> '' THEN 
                FIND FIRST Medlem EXCLUSIVE-LOCK WHERE 
                    Medlem.Telefon = Kunde.Telefon NO-ERROR.
            IF NOT AVAILABLE Medlem THEN 
            DO:
                /* Bare medlem skal opprettes. */
                RUN genkundeMedlem.p (iWebButikk,
                    (IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 1),
                    INPUT-OUTPUT Kunde.KundeNr,
                    OUTPUT lMedlemsNr,
                    OUTPUT bOk,
                    OUTPUT cMsgs).
                /* Kundekortene legges opp på samme kunde, men unike medlemskort */
                /* legges på separate medlemmer.                              */
                RUN genkundekort_og_medlem.p (iWebButikk,
                    Kunde.KundeNr,
                    lMedlemsNr,
                    INT(KundeKort.KortNr),
                    INT(KundeKort.KortNr),
                    999,
                    OUTPUT bOk, 
                    OUTPUT cMsgs).

                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    CreUpdCustomer - Opprettet medlem: ' +
                        STRING(lMedlemsNr) + ' KortNr: ' +  
                        STRING(KundeKort.KortNr)
                        ). 

            END.
            IF AVAILABLE Medlem THEN 
            DO:
                ASSIGN 
                Medlem.MKLubbId              = iMedlKlubb 
                Medlem.Butik                 = IF Medlem.Butik = 0 THEN Kunde.Butik ELSE Medlem.Butik  
                Medlem.MottaeMailUtsendelser = tt_Customer.work_newsletterIndicator
                Medlem.Aktiv                 = TRUE 
                .
                IF Medlem.ForNavn = 'Ukjent' THEN 
                ASSIGN 
                    Medlem.Fornavn   = tt_Customer.givenName + " " + 
                                       tt_Customer.middleName 
                    Medlem.Etternavn = tt_Customer.familyName
                    Medlem.Adresse1  = Kunde.Adresse1
                    Medlem.Adresse2  = ''
                    Medlem.PostNr    = Kunde.PostNr
                    Medlem.Land      = Kunde.Land
                    . 
                    
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    CreUpdCustomer - Oppd.medlemsinfo: ' +
                        Medlem.Fornavn + ' ' +
                        Medlem.Etternavn + ' ' +
                        STRING(Medlem.Butik) + ' KlubbId: ' +  
                        STRING(Medlem.MKLubbId)
                        ). 
            END.
        END.
    END.
    IF AVAILABLE Medlem THEN RELEASE Medlem.
    IF AVAILABLE KundeKort THEN RELEASE KundeKort.
    IF AVAILABLE Kunde THEN RELEASE Kunde.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreUpdOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreUpdOrder Procedure 
PROCEDURE CreUpdOrder :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE piLevFNr  AS INTEGER NO-UNDO.
    DEFINE VARIABLE plDec     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE pcKode    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcTekst   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piant     AS INTEGER NO-UNDO.
    DEFINE VARIABLE pi2ant    AS INTEGER NO-UNDO.
    DEFINE VARIABLE pbForste  AS LOG NO-UNDO.

    TEST_ORDER_BLOKK:
    FOR EACH tt_orderHeader 
        BREAK BY tt_orderHeader.internnr:

        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                '    CreUpdOrder - TEST ordre: ' +
                STRING(tt_orderHeader.orderId)
                ).                

    END.
    
    ORDER_BLOKK:
    FOR EACH tt_orderHeader 
        BREAK BY tt_orderHeader.internnr:

        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                '    CreUpdOrder - Leser ordre: ' +
                STRING(tt_orderHeader.orderId)
                ).                

        pbForste = TRUE.
        FOR EACH ttt_OvBuffer:
            DELETE ttt_OvBuffer.
        END.
 
        /* Ordre kanseleres fra PHX. */
        IF CAN-DO('cancelled',tt_orderHeader.orderStatus)  THEN
        KANSELER_ORDRE: 
        DO TRANSACTION:
            FIND FIRST KOrdreHode EXCLUSIVE-LOCK WHERE 
                KOrdreHode.EkstOrdreNr = tt_orderHeader.orderId NO-ERROR.
            IF AVAILABLE KOrdreHode AND 
                (KOrdreHode.LevStatus = '30' AND KORdreHode.Sendingsnr = '') THEN 
            DO: 
                ASSIGN 
                    KordreHode.VerkstedMerknad = 'Kanselert fra PHX ' + STRING(NOW) + chr(10) + 
                                                 KordreHode.VerkstedMerknad 
                    .
                rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id),
                                                           INPUT 60 ).  
                    
                ELOGGEN:
                DO:
                    FIND ELogg WHERE 
                         ELogg.TabellNavn     = 'CANCELLEDKOrdreHode' AND
                         ELogg.EksterntSystem = "WEBBUT"    AND
                         ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id) NO-ERROR.
                    IF NOT AVAIL Elogg THEN DO:
                        CREATE Elogg.
                        ASSIGN ELogg.TabellNavn     = 'CANCELLEDKOrdreHode'
                               ELogg.EksterntSystem = "WEBBUT"   
                               ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id).
                    END.
                    ASSIGN ELogg.EndringsType = 1 
                           ELogg.Behandlet    = FALSE.
                    RELEASE ELogg.
                END. /* ELOGGEN */
                
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    CreUpdOrder - Kanselert: ' +
                        STRING(KOrdreHode.EkstOrdreNr)
                        ).                
                
                DO ON ERROR UNDO, LEAVE:
                    /* Flytter varer tilbake til lager fra nettbutikk. */
                    RUN opprett_Overforingsordre.p (STRING(KOrdreHode.KOrdre_Id),TRUE) NO-ERROR.
                END.
                IF ERROR-STATUS:ERROR THEN
                DO:
                    cTekst = ''.
                    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                        cTekst = cTekst + 
                            (IF cTekst <> '' THEN CHR(10) ELSE '') +  
                            STRING(ix) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).      
                    END.
                    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                            '    **Feil ved opprettelse av overføringsordre ved kanselering: ' + cTekst
                            ).                 
                END.
                
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    CreUpdOrder - Kanselert (Etter overføring): ' +
                        STRING(KOrdreHode.EkstOrdreNr)
                        ).                

                RELEASE KOrdreHode.
                NEXT ORDER_BLOKK.
            END.
            /* Kanselering av ordre som ikke finnes. */
            ELSE DO:
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    CreUpdOrder - Kanselert ikke eksistrende ordre: ' +
                        tt_orderHeader.orderId
                        ).                
                RELEASE KOrdreHode.
                NEXT ORDER_BLOKK.
            END.
        END. /* TRANSACTION KANSELER_ORDRE */
 
        ELSE IF CAN-DO('new,complete',tt_orderHeader.orderStatus)  THEN 
        ORDRE_OPPSTANDELSE:
        DO TRANSACTION:

            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, ' '). 
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, '    OrdreId: ' + tt_orderHeader.orderId). 
            
            FIND LAST KOrdreHode EXCLUSIVE-LOCK WHERE
                KOrdreHode.EkstOrdreNr = tt_orderHeader.orderId NO-ERROR.
            IF AVAILABLE KOrdreHode THEN 
            DO:
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, '    Funnet ordreId: ' + tt_orderHeader.orderId + 
                                                    ' Interntnr: ' + STRING(KORdreHode.KOrdre_ID)). 
            
                pbForste = FALSE.
                /* Finnes ordren fra før og/eller den har fått en ny status, gjøres ingenting.      */ 
                /* Har ordren fått sendringsnr, er behandlingen av den startet, men ikke avsluttet. */
                /* Og det skal ikke gjøres noe med den.                                             */   
                IF (DEC(KOrdreHode.LevStatus) <= 10) THEN
                DO: 
                    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                                                        '    Ignoreres: ' + 
                                                        tt_orderHeader.orderId + ' ' + 
                                                        'Ordren har status <= 10.'
                                                        ). 
                    
                    NEXT ORDER_BLOKK.
                END.
                /* Er endret dato lik, er det ikke nødvendig å oppdatere ordre. */
                IF KOrdreHode.DatoTidEndret = tt_orderHeader.endretdt THEN
                DO:
                    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                            '    Ignoreres: ' + 
                            tt_orderHeader.orderId + ' ' + 
                            'Endret tid er lik.'
                            ). 
                    NEXT ORDER_BLOKK.
                END.
                /* Er noen av radene på ordren endret (Antall endret), skal heller ikke ordren endres. */
                piAnt = 0.
                FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE
                    KOrdreLinje.Varespesifikasjon <> '':
                    piAnt = piAnt + KOrdreLinje.Antall.
                END.
                /* Teller opp antall på ordren. */
                pi2Ant = 0.
                VARELINJEANT:
                FOR EACH tt_orderLine WHERE 
                    tt_orderLine.internnr = tt_orderHeader.internnr:

                    /* Sjekker at det er en varesalgslinje. */
                    ASSIGN 
                        plDec = DECIMAL(tt_orderLine.upcid) NO-ERROR.
                    IF (ERROR-STATUS:ERROR OR 
                       TRIM(tt_orderLine.upcid) = '' OR 
                       TRIM(tt_orderLine.TYPE) <> 'product' OR 
                       NOT CAN-DO('sales,refund',TRIM(tt_orderLine.transactionType)))  THEN 
                        NEXT VARELINJEANT.
                    pi2Ant = pi2Ant + tt_orderLine.quantity.
                END. /* VARELINJEANT */

                /* ORdren posteres på nytt hvis det ikke er gjort endringer. */
                /* ORdreradene slettes via delete trigger.                   */
                IF piAnt = pi2Ant THEN
                DO:
                    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                            '    Slettes: ' + 
                            tt_orderHeader.orderId + ' ' + 
                            'slik at ordren kan legges inn på nytt.'
                            ). 
                    DELETE KORdreHode. /* Ordren slettes, og blir importert på nytt. */
                END.
                ELSE DO: 
                    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                            '    Ignoreres: ' + 
                            tt_orderHeader.orderId + ' ' + 
                            'Ordren har likt antall enheter. Ingen endring.'
                            ). 
                    NEXT ORDER_BLOKK.  /* Ordreimporten skippes */
                END.
            END.            
                
            FIND FIRST Leveringsform EXCLUSIVE-LOCK WHERE
                Leveringsform.LevFormMetode = tt_orderHeader.shipmentServiceLevelCode NO-ERROR.
            IF NOT AVAILABLE Leveringsform THEN 
            DO:
                FIND LAST Leveringsform NO-LOCK.
                IF AVAILABLE Leveringsform THEN 
                    piLevFnr = Leveringsform.LevFnr + 1.
                ELSE 
                    piLevFNr = 1.
                CREATE Leveringsform.
                ASSIGN
                    Leveringsform.LevFNr             = piLevFNr
                    Leveringsform.LevFormMetode      = tt_orderHeader.shipmentServiceLevelCode
                    Leveringsform.LevFormBeskrivelse = tt_orderHeader.shipmentServiceLevelCode
                    .
                FIND CURRENT Leveringsform NO-LOCK.
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    Ny leveringsform: ' + 
                        Leveringsform.LevFormMetode 
                        ).                 
            END.             
            ELSE piLevFNr = LeveringsForm.LevFNr.
            
            FIND LAST Kasse NO-LOCK WHERE
                Kasse.ButikkNr = iWebButikk NO-ERROR.
            FIND FIRST Forsalj NO-LOCK WHERE
                Forsalj.Brukerid2 = " " NO-ERROR.
            
            FIND FIRST Kunde NO-LOCK WHERE 
                Kunde.EksterntKundeNr = tt_orderHeader.customerId AND 
                Kunde.Butikknr = iWebButikk NO-ERROR.
            IF NOT AVAILABLE Kunde THEN 
                FIND FIRST Kunde NO-LOCK WHERE 
                    Kunde.EksterntKundeNr = tt_orderHeader.customerId NO-ERROR.

            IF NOT AVAILABLE Kunde THEN
            DO: 
                RUN CreUnknownCustomer.
                FIND FIRST Kunde NO-LOCK WHERE 
                    Kunde.EksterntKundeNr = tt_orderHeader.customerId AND 
                    Kunde.Butikknr = iWebButikk NO-ERROR.
                pcTekst = 'Ukjent kunde ' + tt_orderHeader.customerI + ' opprettet'.
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    Ukjent kunde: ' + 
                        STRING(tt_orderHeader.customerId) 
                        ).                 
            END.  
            IF NOT AVAILABLE Kunde THEN 
            DO:
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    Finner ikke kunde. Avbryter: ' + 
                        STRING(tt_orderHeader.customerId) + ' ' 
                        ).                 
                NEXT ORDER_BLOKK. /* NB: Feilmelding ??? */
            END.      
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, '    Oppretter ordreId: ' + tt_orderHeader.orderId). 
            
            CREATE KOrdreHode.
            ASSIGN
                KOrdreHode.VerkstedMerknad = pcTekst + (IF pcTekst <> '' THEN CHR(13) ELSE '') + 
                                             'Innlest ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS') 
                KOrdreHode.Opphav          = 10 /* Nettbutikk */
                KOrdreHode.EkstOrdreNr     = tt_orderHeader.orderId
                KOrdreHode.internMerknad   = " " 
                KOrdreHode.KundeMerknad    = " " 
                KOrdreHode.RegistrertDato  = TODAY
                KOrdreHode.RegistrertTid   = TIME 
                KOrdreHode.DatotidOpprettet = tt_orderHeader.opprettetdt
                KOrdreHode.DatoTidEndret   = tt_orderHeader.endretdt
                KOrdreHode.RegistrertAv    = "Phønix"  /* /Shops/DemoShop/Users/magbyr*/
                KOrdreHode.LevFNr          = piLevFNr 
                KOrdreHode.KundeMerknad    = tt_orderHeader.note
            NO-ERROR.
            
            ASSIGN 
                KOrdreHode.Adresse1     = tt_orderHeader.sh_addressLine
                KOrdreHode.Adresse2     = tt_orderHeader.sh_addressLine2
                KOrdreHode.PostNr       = tt_orderHeader.sh_postalCode
                KOrdreHode.Poststed     = tt_orderHeader.sh_cityName
                /*KOrdreHode.Land         = tt_orderHeader.sh_countryCode */
            NO-ERROR.

            ASSIGN 
                KOrdreHode.FirmaNavn     = tt_orderHeader.bi_name
                KOrdreHode.FirmaAdresse1 = tt_orderHeader.bi_addressLine
                KOrdreHode.FirmaAdresse2 = tt_orderHeader.bi_addressLine2
                KOrdreHode.FirmaPoststed = tt_orderHeader.bi_cityName
                KORdreHode.FirmaLand     = tt_orderHeader.bi_countryCode
            NO-ERROR.

            ASSIGN 
                KOrdreHode.LevAdresse1   = tt_orderHeader.sh_addressLine
                KOrdreHode.LevAdresse2   = tt_orderHeader.sh_addressLine2
                KOrdreHode.LevPostNr     = tt_orderHeader.sh_postalCode
                KOrdreHode.LevPoststed   = tt_orderHeader.sh_cityName
                KOrdreHode.LevLand       = tt_orderHeader.sh_countryCode
            NO-ERROR.

            ASSIGN 
                KOrdreHode.FaktAdresse1 = tt_orderHeader.bi_addressLine
                KOrdreHode.FaktAdresse2 = tt_orderHeader.bi_addressLine2
                KOrdreHode.FaktPostNr   = tt_orderHeader.bi_postalCode
                KOrdreHode.FaktPoststed = tt_orderHeader.bi_cityName
                KOrdreHode.FaktLand     = tt_orderHeader.bi_countryCode
            NO-ERROR.
            IF AVAILABLE Kunde AND (KOrdreHode.FaktAdresse1 = '' AND KOrdreHode.FaktPostNr = '') THEN
            DO:
                IF Kunde.FaktAdresse1 <> '' THEN 
                DO:
                    FIND Post NO-LOCK WHERE 
                        Post.PostNr = Kunde.FaktPostNr NO-ERROR.
                    ASSIGN 
                        KOrdreHode.FaktAdresse1 = Kunde.FaktAdresse1
                        KOrdreHode.FaktAdresse2 = Kunde.FaktAdresse2
                        KOrdreHode.FaktPostNr   = Kunde.FaktPostNr
                        KOrdreHode.FaktPoststed = (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '')
                        KOrdreHode.FaktLand     = Kunde.FaktLand
                        .
                END.
            END. 
            
            ASSIGN                   
                KOrdreHode.KundeNr  = (IF AVAILABLE Kunde THEN Kunde.KundeNr ELSE 0)
                KOrdreHode.ButikkNr = iWebButikk
                KOrdreHode.KasseNr  = (IF AVAILABLE Kasse THEN Kasse.KasseNr ELSE 0)
                KOrdreHode.ForsNr   = (IF AVAILABLE Forsalj THEN Forsalj.ForsNr ELSE 0)
                KOrdreHode.SelgerNr = 0
            NO-ERROR.
            ASSIGN     
                KOrdreHode.Navn     = tt_orderHeader.sh_name
                KOrdreHode.KontNavn = (IF AVAILABLE Kunde THEN Kunde.KontNavn ELSE tt_orderHeader.sh_name)
            NO-ERROR.
            ASSIGN 
                KOrdreHode.ePostAdresse = (IF AVAILABLE Kunde THEN Kunde.ePostAdresse ELSE KOrdreHode.ePostAdresse)               
                KOrdreHode.Telefon      = (IF AVAILABLE kunde THEN Kunde.Telefon ELSE '')
                KOrdreHode.MobilTlf     = (IF AVAILABLE kunde THEN Kunde.MobilTlf ELSE '')
                KOrdreHode.Telefaks     = " "
            NO-ERROR.
            ASSIGN      
                KOrdreHode.DeresRef = (IF AVAILABLE Kunde THEN Kunde.DeresRef ELSE " ")
                KOrdreHode.VaarRef  = " "
            NO-ERROR.
            ASSIGN
                KOrdreHode.Leveringsdato = TODAY + 7
                KOrdreHode.BetaltDato    = TODAY
                KOrdreHode.TotalRabatt%  = 0.0
                KOrdreHode.BetBet        = 2 /* Netto 15 dager */
            NO-ERROR.
            rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id),
                                                       INPUT 30 ).  
            ASSIGN 
                KOrdreHode.DeresRef      = tt_orderHeader.sh_name
                KOrdreHode.ValKod        = ''
                KOrdreHode.cOpt1         = REPLACE(tt_orderheader.giftWrapping,'|',CHR(10))
            NO-ERROR.
                
            FIND CURRENT KORdreHode NO-LOCK.
            
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                    '    Opprettet: ' + 
                    STRING(tt_orderHeader.orderId) + ' med PRSNr: ' + 
                    STRING(KOrdreHode.KORdre_Id) 
                    ).                 
            
        END. /* TRANSACTION ORDRE_OPPSTANDELSE */
  
        VARELINJE:
        FOR EACH tt_orderLine WHERE 
            tt_orderLine.internnr = tt_orderHeader.internnr:

            /* Sjekker at det er en varesalgslinje. */
            ASSIGN 
                plDec = DECIMAL(tt_orderLine.upcid) NO-ERROR.
            IF (ERROR-STATUS:ERROR OR 
               TRIM(tt_orderLine.upcid) = '' OR 
               TRIM(tt_orderLine.TYPE) <> 'product' OR 
               NOT CAN-DO('sales,refund',TRIM(tt_orderLine.transactionType)))  THEN 
                NEXT.

            pcKode = TRIM(STRING(tt_orderLine.upcid)).
            RUN bibl_chkean.p (INPUT-OUTPUT pcKode).
            FIND Strekkode NO-LOCK WHERE 
                Strekkode.Kode = pcKode NO-ERROR.
            IF AVAILABLE Strekkode THEN
            DO: 
                FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
                FIND StrKonv NO-LOCK WHERE
                    StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
            END.  
          
            IF AVAILABLE ArtBas THEN 
                FIND ArtPris OF ArtBas NO-LOCK WHERE
                    ArtPris.ProfilNr = bNettButikk.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris AND AVAILABLE ArtBas THEN 
                FIND ArtPris OF ArtBas NO-LOCK WHERE
                    ArtPris.ProfilNr = bSentallager.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN 
                FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
           
            FIND FIRST Moms NO-LOCK WHERE
                Moms.MomsProc = ROUND(DECIMAL(tt_orderLine.taxRat) / 10000,2) NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                FIND Lager NO-LOCK WHERE
                    Lager.ArtikkelNr = ArtBas.ArtikkelNr AND 
                    Lager.Butik      = bNettButikk.Butik NO-ERROR.
           
            CREATE KOrdreLinje.
            ASSIGN
                KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
                KOrdreLinje.KOrdreLinjeNr = INT(tt_orderLine.lineId) 
                KOrdreLinje.Kode          = pcKode
                KOrdreLinje.VareNr        = IF AVAILABLE ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE ''
                KOrdreLinje.Varetekst     = (IF AVAILABLE ArtBAs THEN ArtBas.Beskr ELSE IF tt_orderLine.DESCRIPTION <> "" THEN tt_orderLine.DESCRIPTION ELSE '** Ukjent ' + TRIM(tt_orderLine.upcid))
                KOrdreLinje.Mva%          = (ROUND(tt_orderLine.taxAmount,2) * 100) / ROUND(tt_orderLine.totalAmount - tt_orderLine.taxAmount,2) 
                KOrdreLinje.Antall        = tt_orderLine.quantity
                KOrdreLinje.BruttoPris    = ROUND(tt_orderLine.Amount,2)  
                KOrdreLinje.NettoPris     = ROUND(tt_orderLine.totalAmount,2) /* - ROUND(tt_orderLine.taxAmount,2)*/
                KOrdreLinje.MvaKr         = ROUND(tt_orderLine.taxAmount,2) 
                KOrdreLinje.NettoLinjesum = (KOrdreLinje.NettoPris * KOrdreLinje.Antall)      
                KOrdreLinje.LinjerabattKr = tt_orderLine.discountAmount
                KOrdreLinje.LinjeRab%     = 0       
                KOrdreLinje.LinjeRab%     = ROUND((KOrdreLinje.LinjeRabattKr * 100) / KOrdreLinje.BruttoPris,2)
                KOrdreLinje.LinjeRab%     = IF KOrdreLinje.LinjeRab% = ? THEN 0 ELSE KOrdreLinje.LinjeRab%   
                KOrdreLinje.MomsKod       = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0)
                KOrdreLinje.Storl         = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
                KOrdreLinje.StrKode       = (IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0)
                KOrdreLinje.Storl         = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
                KOrdreLinje.VareKost      = (IF AVAILABLE Lager THEN Lager.VVareKost ELSE 0)
               
                KOrdreLinje.VareKost      = IF (KOrdreLinje.VareKost = ? OR KOrdreLinje.VareKost <= 0) THEN 0 ELSE KOrdreLinje.VareKost
                KOrdreLinje.VareKost      = IF (KOrdreLinje.VareKost = 0 AND AVAILABLE ArtPris) THEN  ArtPris.VareKost[1] ELSE KOrdreLinje.VareKost
                
                KOrdreLinje.Pris              = KOrdreLinje.NettoPris  
                KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum 
               
                KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
                KOrdreLinje.Db%               = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
                KOrdreLinje.Db%               = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%) 
                KOrdreLinje.RefNr             = 0
                KOrdreLinje.RefTekst          = tt_orderLine.upcid
                KOrdreLinje.Bestillingsnummer = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
                KOrdreLinje.LevFargKod        = IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE ''
                KOrdreLinje.ValKod            = KOrdreHode.ValKod   
                KOrdreLinje.Plukkbutikk       = iWebLager 
                KOrdreLinje.OrdreRabattKr     = 0
                NO-ERROR.
                
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                    '    Opprettet varelinje med Ordreid: ' +
                    tt_orderLine.OrderId + ' Type: ' +
                    tt_orderLine.TYPE + ' LinjeNr: ' +
                    tt_orderLine.lineId + ' ' +  
                    STRING(tt_orderLine.upcid) 
                    ).                 
        END. /* VARELINJE */ 
        
        FRAKT:
        FOR EACH tt_orderLine WHERE 
            tt_orderLine.internnr = tt_orderHeader.internnr AND 
            tt_Orderline.TYPE     = 'shipping' AND 
            tt_OrderLine.totalAmount > 0:
           
            CREATE KOrdreLinje.
            ASSIGN
                KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
                KOrdreLinje.KOrdreLinjeNr = INT(tt_orderLine.lineId) 
                KORdreLinje.Varetekst     = "FRAKT"
                KORdreLinje.VareNr        = cFraktVareNr
                KOrdreLinje.Mva%          = ROUND(DECIMAL(tt_orderLine.taxRat) / 10000,2)
                KOrdreLinje.Antall        = tt_orderLine.quantity
                KOrdreLinje.BruttoPris    = ROUND(tt_orderLine.Amount,2)  
                KOrdreLinje.NettoPris     = ROUND(tt_orderLine.totalAmount,2) - ROUND(tt_orderLine.taxAmount,2)
                KOrdreLinje.MvaKr         = ROUND(tt_orderLine.taxAmount,2) 
                KOrdreLinje.NettoLinjesum = (KOrdreLinje.NettoPris * KOrdreLinje.Antall)      
                KOrdreLinje.MomsKod       = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0)
                KOrdreLinje.VareKost      = IF (KOrdreLinje.VareKost = ? OR KOrdreLinje.VareKost <= 0) THEN 0 ELSE KOrdreLinje.VareKost
                KOrdreLinje.VareKost      = IF (KOrdreLinje.VareKost = 0 AND AVAILABLE ArtPris) THEN  ArtPris.VareKost[1] ELSE KOrdreLinje.VareKost
                KOrdreLinje.Pris          = KOrdreLinje.NettoPris  
                KOrdreLinje.Linjesum      = KOrdreLinje.NettoLinjesum 
                KOrdreLinje.DbKr          = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
                KOrdreLinje.Db%           = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
                KOrdreLinje.Db%           = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%) 
            NO-ERROR.
            
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                    '    Opprettet varelinje med Ordreid: ' +
                    tt_orderLine.OrderId + ' Type: ' +
                    tt_orderLine.TYPE + ' LinjeNr: ' +
                    tt_orderLine.lineId  
                    ).                 
            
        END. /* FRAKT */

        BETALING:
        FOR EACH tt_payments WHERE 
            tt_payments.orderID = tt_orderHeader.orderId:
                
            FIND LAST KOrdrelinje NO-LOCK WHERE
                KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
            IF AVAILABLE KOrdreLinje THEN 
                piLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
            ELSE
                piLinjeNr = 1.
                
            CREATE KOrdreLinje.
            ASSIGN
                KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
                KOrdreLinje.KOrdreLinjeNr = piLinjeNr 
                /* Betaling */
                /*          KOrdreLinje.MomsKod       = IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0*/
                KOrdreLinje.Antall        = 1
                KOrdreLinje.NettoPris     = tt_payments.amount * -1
                /*          KOrdreLinje.MvaKr         = OrdHuvud.vatamount*/
                KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
                KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
                KOrdreLinje.Pris          = KOrdreLinje.NettoPris 
                KOrdreLinje.Linjesum      = KOrdreLinje.NettoPris
                KORdreLinje.Varetekst     = tt_payments.TYPE
                KORdreLinje.VareNr        = "BETALT"
            NO-ERROR.          
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                    '    Opprettet varelinje med Ordreid: ' +
                    tt_payments.OrderId + ' Type: Betaling Beløp: ' +
                    STRING(tt_payments.amount) + ' Referanseid: ' +
                    tt_payments.referenceId + ' Feil? ' + STRING(ERROR-STATUS:ERROR) + 
                    ' bForste: ' + STRING(pbForste)
                    ).
            
            /* Setter utlever butik på ordre som er satt på iPad i butikkene.        */
            /* Dette er en midlertidig løsning inntil PHX kan levere butikknummeret. */
            /* Betaling legges opp helt til slutt. Derfor gjøres det her.            */
            IF CAN-DO('visa,mastercard',KORdreLinje.VareTekst) THEN
            SETT_UTLEVERBUTIKK: 
            DO:
                FOR EACH bufKOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK WHERE
                    bufKOrdreLinje.VareNr <> 'BETALT':
                    ASSIGN
                        bufKOrdreLinje.UtleverButikk = 2 /* Stortingsgaten */
                        .
                END.
            END. /* SETT_UTLEVERBUTIKK */    
        END. /* BETALING */

        /* Flytter varer fra nettbutikk lager til butikken. */
        IF pbForste THEN DO ON ERROR UNDO, LEAVE:
            RUN opprett_Overforingsordre.p (STRING(KOrdreHode.KOrdre_Id),FALSE) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                cTekst = ''.
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    cTekst = cTekst + 
                             (IF cTekst <> '' THEN CHR(10) ELSE '') +  
                             STRING(ix) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).      
                END.
                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    **Feil ved opprettelse av overføringsordre: ' + cTekst
                        ).                 
            END.
            
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                    '    Opprettet overføringsordre.'
                    ).                 
        END.
        
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                '    Ferdig med OrdreId: ' + 
                STRING(tt_orderHeader.orderId) + ' Feil? ' + 
                STRING(ERROR-STATUS:ERROR)).                 
        
    END. /* ORDER_BLOKK */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-putCustomer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putCustomer Procedure 
PROCEDURE putCustomer :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER lcCustomer AS LONGCHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER obOk       AS LOG      NO-UNDO.
    DEFINE OUTPUT PARAMETER ocReturn   AS CHAR     NO-UNDO. 

    /* Tar imot JSon melding og oppretter datasettet. Sletter det som ligger der fra før. */
    TEMP-TABLE tt_Customer:READ-JSON ("longchar", lcCustomer,"EMPTY").
/*     KundeDataSet:READ-JSON ("longchar", lcCustomer,"EMPTY"). */
/*     MESSAGE STRING(lcCustomer). */
    
    /* detta skriver till fil */
    ASSIGN  
        lFormatted  = TRUE 
        cTargetType = "file" 
        cFile       = "log\Customer" + getFilId() + ".json".
    lWriteOK = TEMP-TABLE tt_Customer:WRITE-JSON(cTargetType, cFile, lFormatted).

    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                                        '    PutCustomer - JSon:' + CHR(10) + CHR(13) + 
                                        STRING(cFile)
                                        ). 

    /* Oppretter kunde */
    RUN CreUpdCustomer.
    IF CAN-FIND(FIRST tt_customer) THEN
        obOk = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putOrder Procedure 
PROCEDURE putOrder :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER lcOrder  AS LONGCHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER obOk     AS LOG      NO-UNDO.
    DEFINE OUTPUT PARAMETER ocReturn AS CHAR     NO-UNDO. 

    /* Tar imot JSon melding og oppretter datasettet.  Sletter det som ligger der fra før. */
    OrderDataSet:READ-JSON ("longchar", lcOrder,"EMPTY").
    
    /* detta skriver till fil */
    ASSIGN  
        lFormatted  = TRUE 
        cTargetType = "file" 
        cFile       = "log\Order" + getFilId() + ".json".
    lWriteOK = OrderDataSet:WRITE-JSON(cTargetType, cFile, lFormatted).

    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
            '    PutOrder - JSon:' + CHR(10) + CHR(13) + 
            STRING(lcOrder)
            ) NO-ERROR. 
    
    /* Posterer ordre. */
    RUN CreUpdOrder.
    obOk = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getFilId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilId Procedure 
FUNCTION getFilId RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFilId AS CHARACTER NO-UNDO.

ASSIGN 
    cFilId = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').

RETURN cFilId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
