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
DEFINE VARIABLE iWebLager   AS INT NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg  NO-UNDO LIKE ELogg.

DEFINE TEMP-TABLE tt_shipping NO-UNDO SERIALIZE-NAME "Shipping"
    FIELD orderId    AS CHAR
    FIELD butikknr   AS INTE
    FIELD makulerad  AS LOG
    FIELD trackingid AS CHAR
    FIELD note       AS CHAR
    FIELD Kordre_Id  AS DECI
    INDEX orderId    IS PRIMARY UNIQUE orderId.

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

RUN kopierELogg.
RUN ByggTmpTabeleShipping.

ASSIGN  
  cTargetType = "longchar" 
  lFormatted  = TRUE. 
/* detta skriver till longchar */
lWriteOK = TEMP-TABLE tt_Shipping:WRITE-JSON(cTargetType, lcShipping, lFormatted).

/* detta skriver till fil på log katalog under arbeidskatalog. */
/* ASSIGN                                                                 */
/*   cTargetType = "file"                                                 */
/*   cFile       = "log\Shipment" + STRING(TIME) + ".json".               */
/* lWriteOK = ShippingDataSet:WRITE-JSON(cTargetType, cFile, lFormatted). */

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

    FOR EACH tt_shipping:
        DELETE tt_shipping.
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
            IF KORdreHode.LevStatus <> '50' AND KORdreHode.LevStatus <> '60' THEN 
                LEAVE SENDING.
            CREATE tt_shipping.
            ASSIGN tt_shipping.Kordre_id  = KOrdreHode.Kordre_id
                   tt_shipping.butikknr   = KOrdreHode.Butikknr.
            IF KORdreHode.LevStatus = '50' THEN DO:
                ASSIGN 
                    tt_shipping.OrderId = KOrdreHode.EkstOrdreNr
                    tt_shipping.trackingid = KOrdreHode.SendingsNr
                    tt_shipping.note    = 'Ordern er sänt. Spårningsnummer: ' + REPLACE(REPLACE(KOrdreHode.SendingsNr," ",""),CHR(9),"").
                    .
            END.
            ELSE DO: /* '60' */
                ASSIGN 
                    tt_shipping.OrderId   = KOrdreHode.EkstOrdreNr
                    tt_shipping.makulerad = TRUE
                    tt_shipping.note      = 'Ordern er makulerad.'.
            END.
/*             OLINJE:                                                      */
/*             FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE             */
/*                 KOrdreLinje.Kode <> '':                                  */
/*                                                                          */
/*                 ASSIGN lDec = DEC(KOrdreLinje.Kode) NO-ERROR.            */
/*                 IF ERROR-STATUS:ERROR THEN                               */
/*                     NEXT OLINJE.                                         */
/*                                                                          */
/*                 CREATE tt_shippinglines.                                 */
/*                 ASSIGN                                                   */
/*                     tt_shippinglines.orderId    = KOrdreHode.EkstOrdreNr */
/*                     tt_shippinglines.trackingId = KOrdreHode.SendingsNr  */
/*                     tt_shippinglines.kode       = KOrdreLinje.Kode       */
/*                     tt_shippinglines.antall     = KordreLinje.Antall     */
/*                     tt_shippinglines.note       = KOrdreLinje.Varetekst  */
/*                 .                                                        */
/*             END. /* OLINJE*/                                             */
        END. /* SENDING*/
    END. /* WEBBUT */   

    IF CAN-FIND(FIRST tt_shipping) THEN 
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

&IF DEFINED(EXCLUDE-kopierELogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopierELogg Procedure 
PROCEDURE kopierELogg :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {kopierelogg.i "KOrdreHode"}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

