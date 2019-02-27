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

DEFINE OUTPUT PARAMETER lcArtPris  AS LONGCHAR NO-UNDO.
DEFINE OUTPUT PARAMETER obOk     AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHAR     NO-UNDO. 

DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lWriteOK    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iWebLager   AS INT NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.

DEF VAR ArtPrisDataSet AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg  NO-UNDO LIKE ELogg.

{asGetPRSArtPris.i}

CREATE DATASET ArtPrisDataSet.
ASSIGN
/*     ArtPrisDataSet:SERIALIZE-HIDDEN = TRUE */
ArtPrisDataSet:SERIALIZE-NAME   = "ArtPris".
ArtPrisDataSet:ADD-BUFFER(TEMP-TABLE tt_artprisbutiker:DEFAULT-BUFFER-HANDLE).
ArtPrisDataSet:ADD-BUFFER(TEMP-TABLE tt_artpris:DEFAULT-BUFFER-HANDLE).
ArtPrisDataSet:ADD-RELATION(BUFFER tt_artprisbutiker:HANDLE, BUFFER tt_artpris:HANDLE,"butik,butik").

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
RUN ByggTmpTableArtPris.

ASSIGN  
  cTargetType = "longchar" 
  lFormatted  = TRUE. 
/* detta skriver till longchar */
lWriteOK = ArtPrisDataSet:WRITE-JSON(cTargetType, lcArtPris, lFormatted).

/* detta skriver till fil */
ASSIGN  
  cTargetType = "file" 
  cFile       = "log\ArtPrice" + STRING(TIME) + ".json".
/*lWriteOK = ArtPrisDataSet:WRITE-JSON(cTargetType, cFile, lFormatted).*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTableArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTableArtPris Procedure 
PROCEDURE ByggTmpTableArtPris :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iRecType AS INT     NO-UNDO.
    DEFINE VARIABLE iAnt     AS INTEGER NO-UNDO.

    FOR EACH tt_artprisbutiker:
        DELETE tt_artprisbutiker.
    END.
    FOR EACH tt_artpris:
        DELETE tt_artpris.
    END.

    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = iWebLager NO-ERROR.
    IF AVAILABLE Butiker THEN
    DO:
        CREATE tt_artprisbutiker.
        ASSIGN 
            tt_artprisbutiker.butik    = Butiker.Butik
            tt_artprisbutiker.butnamn  = Butiker.ButNamn
            tt_artprisbutiker.kortnavn = Butiker.KortNavn
            tt_artprisbutiker.profilnr = butiker.ProfilNr
            .
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE
        tt_ELogg.TabellNavn     = 'artbas' AND 
        tt_Elogg.EksterntSystem = 'WEBBUT':
        
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = DEC(TT_Elogg.Verdier) NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
            NEXT.
        IF Artbas.WebButikkArtikkel = FALSE THEN
            NEXT.
        FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ArtPris.ProfilNr   = tt_artprisbutiker.profilnr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.

/*         MESSAGE 'ArtPris:'                                */
/*             ArtPris.ArtikkelNr                            */
/*             ArtPris.ProfilNr                              */
/*             ArtPris.Tilbud                                */
/*         tt_artprisbutiker.butik                           */
/*         ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] */
/*         ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]     */
/*         ArtPris.Pris[1]                                   */
/*         ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]     */
/*             .                                             */
                
        FOR EACH Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = ArtBas.ArtikkelNr,
            FIRST StrKonv NO-LOCK WHERE 
                StrKonv.StrKode = StrekKode.StrKode:
                    
            /* Det skal ha vært lagerbevegelser på størrelsen før den sendes. */
            IF CAN-FIND(FIRST TransLogg WHERE 
                        TransLogg.ArtikkelNr = Strekkode.ArtikkelNr AND 
                        TransLogg.Butik      = iWebLager AND
                        TransLogg.Storl      = StrKonv.Storl) THEN
            HAR_TRANS:
            DO:
                cTekst = ''.
                CREATE tt_artpris.
                ASSIGN 
                    tt_artpris.butik    = tt_artprisbutiker.butik
                    tt_artpris.kode     = Strekkode.Kode
                    tt_artpris.Varekost = ArtPris.VareKost[1]
                    tt_artpris.mva%     = ArtPris.Mva%[1]
                    tt_artpris.Pris     = ArtPris.Pris[1]
                    tt_artpris.TilbPris = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                    NO-ERROR.
                    
                IF ERROR-STATUS:ERROR THEN 
                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                    cTekst = STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' '+ 
                        ERROR-STATUS:GET-MESSAGE(ix). 
                END.
                IF ctekst <> '' THEN 
                    MESSAGE 'Feil:' cTekst.    
            END. /* HAR_TRANS */ 
        END.
    END. /* WEBBUT */   

    IF CAN-FIND(FIRST tt_artpris) THEN 
        obOk = TRUE.
    ELSE 
        ASSIGN 
            obOk     = FALSE 
            ocReturn = 'Ingen nye eller endrede prisposter logget.'
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

    {kopierelogg.i "artbas"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

