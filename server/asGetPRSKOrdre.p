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

DEFINE OUTPUT PARAMETER lcShipping  AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER obOk     AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHAR     NO-UNDO. 

DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lWriteOK    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iWebLager   AS INT       NO-UNDO.
DEFINE VARIABLE iWebButikk  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFrabutikkNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iTilbutikkNr AS INTEGER NO-UNDO.

DEF VAR ShippingDataSet AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg  NO-UNDO LIKE ELogg.

{asGetPRSKOrdre.i}

CREATE DATASET ShippingDataSet.
ASSIGN
/*     ShippingDataSet:SERIALIZE-HIDDEN = TRUE */
ShippingDataSet:SERIALIZE-NAME   = "Shipping".
ShippingDataSet:ADD-BUFFER(TEMP-TABLE tt_shippingheader:DEFAULT-BUFFER-HANDLE).
ShippingDataSet:ADD-BUFFER(TEMP-TABLE tt_shippinglines:DEFAULT-BUFFER-HANDLE).
ShippingDataSet:ADD-RELATION(BUFFER tt_shippingheader:HANDLE, BUFFER tt_shippinglines:HANDLE,"orderId,orderId").

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
         HEIGHT             = 14.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{syspara.i 150 1 3 iWebLager INT}
{syspara.i 150 1 2 iWebButikk INT}

RUN kopierELogg.
RUN ByggTmpTabeleShipping.

ASSIGN  
  cTargetType = "longchar" 
  lFormatted  = TRUE. 
/* detta skriver till longchar */
lWriteOK = ShippingDataSet:WRITE-JSON(cTargetType, lcShipping, lFormatted).

/* detta skriver till fil på log katalog under arbeidskatalog. */
ASSIGN  
  cTargetType = "file" 
  cFile       = "log\Shipment" + STRING(TIME) + ".json".
lWriteOK = ShippingDataSet:WRITE-JSON(cTargetType, cFile, lFormatted).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTabeleShipping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabeleShipping Procedure 
PROCEDURE ByggTmpTabeleShipping :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE iRecType AS INT     NO-UNDO.
    DEFINE VARIABLE iAnt     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lDec     AS DECIMAL NO-UNDO.

    FOR EACH tt_shippingheader:
        DELETE tt_shippingheader.
    END.
    FOR EACH tt_shippinglines:
        DELETE tt_shippinglines.
    END.

    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = iWebLager NO-ERROR.

    WEBBUT:
    FOR EACH TT_Elogg WHERE
        tt_ELogg.TabellNavn     = 'KOrdreHode' AND 
        tt_Elogg.EksterntSystem = 'WEBBUT':
        
        IF NUM-ENTRIES(TT_Elogg.Verdier,CHR(1)) < 1 THEN
            NEXT.
        FIND KOrdreHode NO-LOCK WHERE 
            KOrdreHode.KOrdre_Id = DEC(ENTRY(1,TT_Elogg.Verdier,CHR(1))) NO-ERROR.
        IF AVAILABLE KORdreHode THEN
        SENDING: 
        DO:
            ASSIGN
                iFrabutikkNr = KOrdreHode.butikkNr
                iTilbutikkNr = iWebLager 
            .
                
            /* Bare disse utleveringsstatus skal behandles. */            
            /* 50-Utlever, 60-Makuler.                      */
            IF NOT CAN-DO('50,60',STRING(KOrdreHode.LevStatus)) THEN 
                LEAVE SENDING.
                
            /* Det skal ikke sendes shipping melding hvis ikke sendingsnr er fyllt ut. */
            IF TRIM(KOrdreHode.SendingsNr) <> '' THEN
            OPPRETT_SHIPPING: 
            DO:
                /* Er ordren makulert, står det 'MAKULERT30' i sendingsnr og status 60. */
                /* Da skal det sendes shippingmelding med 0 i antall.                   */
                /* Står det 'MAKULERT50', skal det IKKE sendes shippingordre. */
                IF KOrdreHode.LevStatus = '60' AND KOrdreHode.SendingsNr = 'MAKULERT50' THEN 
                    LEAVE OPPRETT_SHIPPING.                

                CREATE tt_shippingheader.
                ASSIGN 
                    tt_shippingheader.OrderId = KOrdreHode.EkstOrdreNr
                    tt_shippingheader.note    = 'Ordren er sendt.'
                    .
                OLINJE:
                FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE 
                    KOrdreLinje.Kode <> '':
                    
                    ASSIGN lDec = DEC(KOrdreLinje.Kode) NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN 
                        NEXT OLINJE.
                        
                    CREATE tt_shippinglines.
                    ASSIGN 
                        tt_shippinglines.orderId    = KOrdreHode.EkstOrdreNr
                        tt_shippinglines.trackingId = KOrdreHode.SendingsNr
                        tt_shippinglines.kode       = KOrdreLinje.Kode
                        tt_shippinglines.antall     = (IF KOrdreHode.LevStatus = '50' 
                                                          THEN KordreLinje.Antall
                                                          ELSE 0)
                        tt_shippinglines.note       = KOrdreLinje.Varetekst 
                    .
                END. /* OLINJE*/            
            END. /* OPPRETT_SHIPPING */     
                
            /* Returnerner varene til sentrallageret. */
            IF KOrdreHode.LevStatus = '60' AND 
               (iWebLager <> 0 AND iWebButikk <> 0) AND 
               (iWebLager <> iWebButikk)
            THEN DO:
                SUBSCRIBE TO "getFraTilbutikkReturKOrdre" ANYWHERE  RUN-PROCEDURE "getFraTilbutikkReturKOrdre".
                RUN opprett_Overforingsordre.p (STRING(KOrdreHode.KOrdre_Id),?) NO-ERROR.
                UNSUBSCRIBE TO "getFraTilbutikkReturKOrdre".
            END.
        END. /* SENDING*/
    END. /* WEBBUT */   

    IF CAN-FIND(FIRST tt_shippingheader) THEN 
        obOk = TRUE.
    ELSE 
        ASSIGN 
            obOk     = FALSE 
            ocReturn = 'Ingen nye eller endrede shippingposter logget.'
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFraTilbutikkReturKOrdre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFraTilbutikkReturKOrdre Procedure
PROCEDURE getFraTilbutikkReturKOrdre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER piFrabutikkNr AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER piTilbutikkNr AS INTEGER NO-UNDO.
    
    ASSIGN
        piFrabutikkNr = iFrabutikkNr
        piTilbutikkNr = piTilbutikkNr 
    .
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-kopierELogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopierELogg Procedure 
PROCEDURE kopierELogg :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR lNow AS DEC FORMAT "->>>>>>>>>>>>>>>9" NO-UNDO.
    DEF VAR lDiff AS DEC FORMAT "->>>>>>>>>>>>>>>9" NO-UNDO.
    
    DEFINE BUFFER bElogg   FOR Elogg.

    ASSIGN 
        lDiff = 60 * 5
        lNow  = dec(
                    STRING(YEAR(TODAY),"9999") +
                    string(MONTH(TODAY),"99") + 
                    string(DAY(TODAY),"99") +
                    string(TIME,"99999")
                   ).

    LOOPEN:
    FOR EACH ELogg NO-LOCK WHERE 
        ELogg.TabellNavn     = "KOrdreHode" AND
        ELogg.EksterntSystem = "WEBBUT":

        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF NOT AVAIL bElogg THEN
            NEXT.

        /* Bare modne poster skal behandles. */
        IF (lNow - ELogg.Opprettet) < lDiff THEN
            NEXT.

        BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
        DELETE bELogg.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END. /* LOOPEN */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

