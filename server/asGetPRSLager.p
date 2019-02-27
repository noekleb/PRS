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

DEFINE OUTPUT PARAMETER lcStock  AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER obOk     AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHAR     NO-UNDO. 

DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lWriteOK    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iWebLager   AS INT NO-UNDO.

DEF VAR StockDataSet AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg  NO-UNDO LIKE ELogg.

{asGetPRSLager.i}

CREATE DATASET StockDataSet.
ASSIGN
/*     StockDataSet:SERIALIZE-HIDDEN = TRUE */
StockDataSet:SERIALIZE-NAME   = "Stock".
StockDataSet:ADD-BUFFER(TEMP-TABLE tt_stockbutiker:DEFAULT-BUFFER-HANDLE).
StockDataSet:ADD-BUFFER(TEMP-TABLE tt_stock:DEFAULT-BUFFER-HANDLE).
StockDataSet:ADD-RELATION(BUFFER tt_stockbutiker:HANDLE, BUFFER tt_stock:HANDLE,"butik,butik").

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
RUN ByggTmpTabeStoc.

ASSIGN  
  cTargetType = "longchar" 
  lFormatted  = TRUE. 
/* detta skriver till longchar */
lWriteOK = StockDataSet:WRITE-JSON(cTargetType, lcStock, lFormatted).

/* detta skriver till fil på log katalog under arbeidskatalog. */
ASSIGN  
  cTargetType = "file" 
  cFile       = "log\Stock" + STRING(TIME) + ".json".
lWriteOK = StockDataSet:WRITE-JSON(cTargetType, cFile, lFormatted).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTabeStoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabeStoc Procedure 
PROCEDURE ByggTmpTabeStoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE iRecType AS INT  NO-UNDO.
DEFINE VARIABLE iAnt     AS INTEGER NO-UNDO.

FOR EACH tt_stockbutiker:
    DELETE tt_stockbutiker.
END.
FOR EACH tt_stock:
    DELETE tt_stock.
END.

FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = iWebLager NO-ERROR.
IF AVAILABLE Butiker THEN
DO:
    CREATE tt_stockbutiker.
    ASSIGN tt_stockbutiker.butik    = Butiker.Butik
           tt_stockbutiker.butnamn  = Butiker.ButNamn
           tt_stockbutiker.kortnavn = Butiker.KortNavn.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE
    tt_ELogg.TabellNavn     = 'Lager' AND 
    tt_Elogg.EksterntSystem = 'WEBBUT':
        
    IF NUM-ENTRIES(TT_Elogg.Verdier,CHR(1)) < 2 THEN
        NEXT.
        
    /* Kun web butikkens lager skal sendes. */
    IF INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) <> iWebLager THEN 
        NEXT.
    ELSE      
    FOR EACH ArtLag NO-LOCK WHERE
        ArtLag.ArtikkelNr = DECIMAL(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
        ArtLag.Butik      = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND 
        /* Det skal ha vært lagerbevegelser på størrelsen før den sendes. */
        CAN-FIND(FIRST TransLogg WHERE 
                       TransLogg.ArtikkelNr = ArtLag.ArtikkelNr AND 
                       TransLogg.Butik      = ArtLag.butik AND
                       TransLogg.Storl      = ArtLag.storl): 
        FOR EACH Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = ArtLag.ArtikkelNr AND
            Strekkode.StrKode    = ArtLag.StrKode:
            CREATE tt_stock.
            ASSIGN 
                tt_stock.butik  = ArtLag.butik
                tt_stock.kode   = Strekkode.Kode
                tt_stock.lagant = INT(IF (ArtLag.Lagant > 0 AND ArtLag.Lagant <> ?) THEN ArtLag.LagAnt ELSE 0)
                .
        END.
    END.

END. /* WEBBUT */   

IF CAN-FIND(FIRST tt_stock) THEN 
  obOk = TRUE.
ELSE 
  ASSIGN 
  obOk     = FALSE 
  ocReturn = 'Ingen nye eller endrede lagerposter logget.'
  .

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
    {kopierelogg.i "Lager"}
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

