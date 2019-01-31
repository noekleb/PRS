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
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
DEFINE VARIABLE cTabell     AS CHARACTER INITIAL 'Returkoder' NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG       NO-UNDO.

DEF VAR ReturnDataSet AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg  NO-UNDO LIKE ELogg.

{asGetPRSReturn.i}

CREATE DATASET ReturnDataSet.
ASSIGN
/*     ReturnDataSet:SERIALIZE-HIDDEN = TRUE */
ReturnDataSet:SERIALIZE-NAME = "ReturnRequest".
ReturnDataSet:ADD-BUFFER(TEMP-TABLE tt_returnheader:DEFAULT-BUFFER-HANDLE).
ReturnDataSet:ADD-BUFFER(TEMP-TABLE tt_returnlines:DEFAULT-BUFFER-HANDLE).
ReturnDataSet:ADD-RELATION(BUFFER tt_returnheader:HANDLE, BUFFER tt_returnlines:HANDLE,"orderId,orderId").

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

ASSIGN
    bTest = FALSE 
    cLogg = 'asGetPRSReturn' + REPLACE(STRING(TODAY),'/','')
    .

IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'Start.'). 

RUN kopierELogg.
RUN ByggTmpTabeleReturn.

ASSIGN  
  cTargetType = "longchar" 
  lFormatted  = TRUE. 
/* detta skriver till longchar */
lWriteOK = ReturnDataSet:WRITE-JSON(cTargetType, lcShipping, lFormatted).

/* detta skriver till fil på log katalog under arbeidskatalog. */
ASSIGN  
  cTargetType = "file" 
  cFile       = "log\ReturnRequest" + STRING(TIME) + ".json".
lWriteOK = ReturnDataSet:WRITE-JSON(cTargetType, cFile, lFormatted).

IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                      'JSon msgs: ' + CHR(10) + CHR(13) + STRING(lcShipping)
                      ). 

IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'ferdig.'). 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTabeleShipping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabeleShipping Procedure
PROCEDURE ByggTmpTabeleReturn:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE iRecType AS INT     NO-UNDO.
    DEFINE VARIABLE iAnt     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lDec     AS DECIMAL NO-UNDO.

    FOR EACH tt_returnheader:
        DELETE tt_returnheader.
    END.
    FOR EACH tt_returnlines:
        DELETE tt_returnlines.
    END.

    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = iWebLager NO-ERROR.

    IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 'Leser elogg.'). 

    WEBBUT:
    FOR EACH TT_Elogg WHERE
        tt_ELogg.TabellNavn     = 'RETURKOrdreHode' AND 
        tt_Elogg.EksterntSystem = 'WEBBUT':

        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                                            '    ELogg: ' +
                                            tt_ELogg.TabellNavn + ' ' +
                                            tt_Elogg.EksterntSystem + ' ' +
                                            TT_Elogg.Verdier
                                            ). 
        
        IF NUM-ENTRIES(TT_Elogg.Verdier,CHR(1)) > 1 THEN
            NEXT.
        FIND KOrdreHode NO-LOCK WHERE 
            KOrdreHode.KOrdre_Id = DEC(ENTRY(1,TT_Elogg.Verdier,CHR(1))) NO-ERROR.
        IF AVAILABLE KORdreHode THEN
        SENDING: 
        DO:
            IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                    '    KOrdre levstatus: ' +
                    KORdreHode.LevStatus
                    ). 

            IF KORdreHode.LevStatus <> '50' THEN 
                LEAVE SENDING.
            CREATE tt_returnheader.
            ASSIGN 
                tt_returnheader.OrderId = TRIM(REPLACE(KOrdreHode.EkstOrdreNr, 'RETUR',''))
                .
            OLINJE:
            FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE 
                KORdreLinje.Kode <> '':

                IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                        '    KOrdrelinje EAN: ' +
                        KOrdreLinje.Kode
                        ). 
                                
                ASSIGN lDec = DEC(KOrdreLinje.Kode) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                    NEXT OLINJE.
                    
                FIND FIRST ImpKonv NO-LOCK WHERE 
                    ImpKonv.EDB-System = cEDB-System AND 
                    ImpKonv.Tabell     = cTabell AND 
                    ImpKonv.InterntID = STRING(KOrdreLinje.ReturKodeId) NO-ERROR.
                IF AVAILABLE ImpKonv 
                    THEN ASSIGN cTekst = impKonv.EksterntID.
                ELSE cTekst = ''.                    
                    
                CREATE tt_returnlines.
                ASSIGN 
                    tt_returnlines.orderId    = TRIM(REPLACE(KOrdreHode.EkstOrdreNr, 'RETUR',''))
                    tt_returnlines.kode       = KOrdreLinje.Kode
                    tt_returnlines.antall     = ABS(KordreLinje.Antall)
                    tt_returnlines.orsak      = KOrdreLinje.ReturKodeId 
                    tt_returnlines.phoenix_orsak = cTekst 
                .
            END. /* OLINJE*/
        END. /* SENDING*/
    END. /* WEBBUT */   

    IF CAN-FIND(FIRST tt_returnheader) THEN 
    DO:
               
        /* Tar imot JSon melding og oppretter datasettet.  Sletter det som ligger der fra før. */
        ReturnDataSet:WRITE-JSON ("longchar", lcShipping).
        obOk = TRUE.
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                '    Test JSon: ' +
                STRING(obOk)
                ). 
    END.
    ELSE DO:
        ASSIGN 
            obOk     = FALSE 
            ocReturn = 'Ingen nye retur medlinger logget.'
            .
        IF bTest THEN RUN bibl_loggDbFri.p (cLogg, 
                '    Feil JSon: ' +
                ocReturn + ' ' +
                STRING(obOk)
                ). 
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-kopierELogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopierELogg Procedure
PROCEDURE kopierELogg:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {kopierelogg.i "RETURKOrdreHode"}
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

