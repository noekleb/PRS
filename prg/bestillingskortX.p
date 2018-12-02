&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :
    main
        ByggTT_Inleveranser
        SkrivRapport
            listetype = 101:
                SkrivInlevHeader
                SkrivBestilling
                    SkrivBestillInfo
            END.
            ELSE:
                PFooter
                SkrivInlevHeader
                SkrivInleveranser
                    PFooter
                    SkrivBestillInfo
            END.
            PFooter
            
    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER ipBestRecid AS RECID  NO-UNDO.
DEFINE INPUT  PARAMETER iListetype  AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER iSortering  AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER lFolgesedel AS LOGICAL    NO-UNDO. /* används för per butik */

DEFINE VAR ipOrdreNr   AS INTEGER    NO-UNDO.
DEFINE VAR ipcBestNr   AS CHARACTER  NO-UNDO.
DEFINE VAR ipcButikker AS INTEGER    NO-UNDO.

DEFINE VARIABLE iBestNrTst  AS INTEGER  NO-UNDO.
DEFINE VARIABLE iOrdreNr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iBestNr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE ii          AS INTEGER    NO-UNDO.
DEFINE VARIABLE cOrdreListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStatusList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBestStatus AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE iCentralLager AS INTEGER    NO-UNDO.
DEFINE VARIABLE cBildeFil   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iNumCopies AS INTEGER INIT 1   NO-UNDO.
DEFINE TEMP-TABLE TT_Ordre NO-UNDO
    FIELD OrdreNr      AS INTEGER.

DEFINE TEMP-TABLE TT_Bestnr NO-UNDO
    FIELD Bestnr      AS INTEGER
    FIELD Storrelser  AS CHARACTER
    FIELD DirekteLev  AS LOGICAL
    INDEX bestnr bestnr.

DEFINE TEMP-TABLE TT_BestButikk NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD RadAntal     AS INTEGER
    FIELD Butnamn      AS CHARACTER
    INDEX idx1 bestnr butikknr.

DEFINE TEMP-TABLE TT_Bestilt NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD cBestiltantal  AS CHARACTER
    FIELD bestUser     AS CHARACTER
    FIELD cRest        AS CHARACTER
    FIELD totbestillt   AS INTE
    INDEX idx1 bestnr butikknr.

DEFINE TEMP-TABLE TT_Inlevert NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD LeveringsNr  AS INTEGER
    FIELD Dato         AS DATE
    FIELD cInlevantal  AS CHARACTER
    FIELD LevertAv     AS CHARACTER
    FIELD cMakulert    AS CHARACTER
    FIELD totlevert    AS INTE
    INDEX idx1 bestnr butikknr leveringsnr.

DEFINE TEMP-TABLE TT_Rest NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD LeveringsNr AS INTEGER
    FIELD cRest        AS CHARACTER
    INDEX idx1 bestnr butikknr leveringsnr.

DEFINE TEMP-TABLE TT_BestCL NO-UNDO LIKE TT_Bestilt.

DEFINE VARIABLE cLevNamn     LIKE levbas.levnamn  NO-UNDO.
DEFINE VARIABLE cInlevLabel AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cOrdreNr    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cHeaderTxt  AS CHARACTER FORMAT "x(35)"   NO-UNDO.
DEFINE VARIABLE iMin AS INTEGER NO-UNDO.
DEFINE VARIABLE iMax AS INTEGER NO-UNDO.
DEFINE VARIABLE cKundenavn    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikkNr     AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE iInfoCol      AS INTEGER INIT 6   NO-UNDO.
DEFINE VARIABLE iSignCol      AS INTEGER INIT 12   NO-UNDO.
DEFINE VARIABLE iStrCol       AS INTEGER INIT 16   NO-UNDO.
/* DEFINE VARIABLE iStrCol       AS INTEGER INIT 26   NO-UNDO. */

/* DEFINE FRAME PageHeader                                                                                                                                   */
/*    HEADER                                                                                                                                                 */
/*       "<ALIGN=BASE><FArial><R4><P14><B><C6>" STRING(TODAY) "<C20>" cHeaderTxt "</B><C45><P12>Butikk:" cButikkNr "<C75><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
/*       "<R5><C6><FROM><R5><C78><LINE>" SKIP                                                                                                                */
/*       WITH PAGE-TOP STREAM-IO WIDTH 255.                                                                                                                  */

DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><P14><B><C6>" STRING(TODAY) "<C37>" cHeaderTxt "</B><C62><P12>Butikk:" cButikkNr "<B><C109><P14>" PAGE-NUMBER FORMAT ">>" "</B><P10>" SKIP
      "<R4><C6><FROM><R4><C113><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

DEFINE FRAME PageHeaderBest
   HEADER
      "<ALIGN=BASE><FArial><R3><P14><B><C6>" STRING(TODAY) "<C37>" cHeaderTxt "<C109>" PAGE-NUMBER FORMAT ">>" "</B><P10>" SKIP
      "<R4><C6><FROM><R4><C113><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

/* DEFINE FRAME PageHeaderBest                                                                                                 */
/*    HEADER                                                                                                                   */
/*       "<ALIGN=BASE><FArial><R4><P14><B><C6>" STRING(TODAY) "<C37>" cHeaderTxt "</B><C75><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
/*       "<R5><C6><FROM><R5><C78><LINE>" SKIP                                                                                  */
/*       WITH PAGE-TOP STREAM-IO WIDTH 255.                                                                                    */


{xPrint.i}
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getBildeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBildeFil Procedure 
FUNCTION getBildeFil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getString Procedure 
FUNCTION getString RETURNS CHARACTER
  ( INPUT cEntryList AS CHARACTER )  FORWARD.

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
         HEIGHT             = 16.43
         WIDTH              = 77.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* DEFINE INPUT  PARAMETER ipBestRecid AS RECID  NO-UNDO.     */
/* DEFINE INPUT  PARAMETER iListetype  AS INTEGER    NO-UNDO. */
/* DEFINE INPUT  PARAMETER iSortering  AS INTEGER    NO-UNDO. */
/* DEFINE INPUT  PARAMETER lFolgesedel AS LOGICAL    NO-UNDO. */
IF iListeType = 107 THEN DO: /* Innlevrapport baserad på HLev = denna inleveransen */
     FIND BestHlev WHERE RECID(BestHLev) = ipBestRecid NO-LOCK NO-ERROR.
     FIND BestHode WHERE BestHode.BestNr = BestHLev.BestNr NO-LOCK NO-ERROR.
     {syspar2.i 5 4 6 iNumCopies INT}
     ASSIGN iNumCopies = IF iNumCopies < 2 THEN 1 ELSE iNumCopies.
END.
ELSE DO: /* 101 = Bestillingsrapport, 109=Inleveransrapport total */
    FIND BestHode WHERE RECID(BestHode) = ipBestRecid NO-LOCK NO-ERROR.
END.
IF NOT AVAIL BestHode THEN DO:
    MESSAGE "Finner ikke bestillingen"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "FEIL".
END.
FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
IF NOT AVAIL ArtBas THEN DO:
    MESSAGE "Finner ikke artikkelen"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "FEIL".
END.
cBildeFil = getBildeFil(ArtBas.Bildnr).

{syspara.i 5 1 1 iCentralLager INT}

/* IF ipOrdreNr <> 0 THEN DO:                                                                                         */
/*     FIND ordre WHERE ordre.ordrenr = ipOrdreNr NO-LOCK NO-ERROR.                                                   */
/*     IF NOT AVAIL Ordre THEN                                                                                        */
/*         RETURN "FEIL, finner ikke ordre".                                                                          */
/*     CREATE TT_Ordre.                                                                                               */
/*     ASSIGN TT_Ordre.OrdreNr = ipOrdreNr.                                                                           */
/*     IF TRIM(ipcBestNr) = "" THEN DO:                                                                               */
/*         ASSIGN ipcBestNr = "".                                                                                     */
/*         FOR EACH BestHode WHERE BestHode.OrdreNr = ipOrdreNr NO-LOCK:                                              */
/*             ASSIGN ipcBestNr = ipcBestNr + (IF ipcBestNr <> "" THEN "," ELSE "") + STRING(BestHode.BestNr).        */
/*         END.                                                                                                       */
/*     END.                                                                                                           */
/* END.                                                                                                               */
/* ELSE IF TRIM(ipcBestNr) <> "" THEN DO:                                                                             */
/*     DO ii = 1 TO NUM-ENTRIES(ipcBestNr):                                                                           */
/*         ASSIGN iBestNrTst = INT(ENTRY(ii,ipcBestNr)) NO-ERROR.                                                     */
/*         IF ERROR-STATUS:ERROR THEN                                                                                 */
/*             RETURN "FEIL i Bestnrliste".                                                                           */
/*         FIND BestHode WHERE BestHode.Bestnr = iBestNrTst NO-LOCK NO-ERROR.                                         */
/*         IF NOT AVAIL BestHode THEN                                                                                 */
/*             RETURN "FEIL, finner ikke besthode".                                                                   */
/* /*         FIND FIRST BestSort OF Besthode WHERE BestSort.Fri = TRUE NO-LOCK NO-ERROR. */                          */
/*         IF NOT CAN-DO(cOrdreListe,STRING(BestHode.OrdreNr)) THEN                                                   */
/*             ASSIGN cOrdreListe = cOrdreListe + (IF cOrdreListe <> "" THEN "," ELSE "") + STRING(BestHode.OrdreNr). */
/*     END.                                                                                                           */
/*     IF NUM-ENTRIES(cOrdreListe) <> 1 THEN                                                                          */
/*         RETURN "Flere Ordre " + cOrdreListe.                                                                       */
/*     CREATE TT_Ordre.                                                                                               */
/*     ASSIGN TT_Ordre.OrdreNr = INT(cOrdreListe).                                                                    */
/* END.                                                                                                               */
/* ELSE                                                                                                               */
/*     RETURN.                                                                                                        */
/*     {syspara.i 5 2 99 cStatusList}                                                                                 */
RUN ByggTT_Inleveranser.

IF NOT CAN-FIND(FIRST TT_Bestnr) THEN
    RETURN "FEIL".
  {syspara.i 1 1 100 cKundenavn}
  {syspara.i 1 1 101 cPolygon}
RUN SkrivRapport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTT_Inleveranser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT_Inleveranser Procedure 
PROCEDURE ByggTT_Inleveranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIdx AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iMin AS INTEGER INIT 1000 NO-UNDO.
    DEFINE VARIABLE iMax AS INTEGER INIT 0    NO-UNDO.
    DEFINE VARIABLE cEntryList  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTmpString AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTmpEntry  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lFinnsRest AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.

    DO: /* ii definearad i definitions */
/*         FIND BestHode WHERE BestHode.BestNr = INT(ENTRY(ii,ipcBestNr)) NO-LOCK. */
        CREATE TT_Bestnr.
        ASSIGN TT_Bestnr.BestNr     = BestHode.Bestnr
               TT_BestNr.DirekteLev = BestHode.DirekteLev.

        FIND FIRST BestSort OF Besthode WHERE BestSort.Fri = TRUE NO-LOCK NO-ERROR.
        IF AVAIL BestSort THEN
            ASSIGN TT_BestNr.Storrelser = REPLACE(TRIM(BestSort.Storrelser)," ",",")
                   cEntryList  = FILL(",",NUM-ENTRIES(TT_BestNr.Storrelser) - 1).

/*     FIELD cBestiltantal  AS CHARACTER */
        ASSIGN iMin = 1000
               iMax = 0.
        FOR EACH BestStr WHERE BestStr.BestNr = BestHode.BestNr AND BestStr.Beststat = BestHode.BestStat AND BestStr.Bestilt <> 0 NO-LOCK.
            FIND TT_BestButikk WHERE TT_BestButikk.BestNr = BestStr.BestNr AND TT_BestButikk.ButikkNr = BestStr.Butik NO-ERROR.
            IF NOT AVAIL TT_BestButikk THEN DO:
                FIND Butiker WHERE Butiker.butik = BestStr.Butik NO-LOCK NO-ERROR.
                CREATE TT_BestButikk.
                ASSIGN TT_BestButikk.BestNr        = BestStr.BestNr
                       TT_BestButikk.ButikkNr      = BestStr.Butik
                       TT_BestButikk.Butnamn       = IF AVAIL Butiker THEN Butiker.butnamn ELSE "Ukjent"
                       TT_BestButikk.RadAntal      = 8.
            END.
            FIND TT_Bestilt WHERE TT_Bestilt.BestNr = BestStr.BestNr AND TT_Bestilt.ButikkNr = BestStr.Butik NO-ERROR.
            IF NOT AVAIL TT_Bestilt THEN DO:
                CREATE TT_Bestilt.
                ASSIGN TT_Bestilt.BestNr        = BestStr.BestNr
                       TT_Bestilt.ButikkNr      = BestStr.Butik
                       TT_Bestilt.cBestiltantal = cEntryList.
            END.
            ASSIGN TT_Bestilt.totbestillt   = TT_Bestilt.totbestillt + BestStr.Bestilt.
            ASSIGN iIdx = LOOKUP(TRIM(BestStr.Storl),TT_BestNr.Storrelser)
                   ENTRY(iIdx,TT_Bestilt.cBestiltantal) = STRING(BestStr.Bestilt)
                   iMin = IF iIdx < iMin THEN iIdx ELSE iMin
                   iMax = IF iIdx > iMax THEN iIdx ELSE iMax.
            IF BestHode.direktelev = FALSE THEN DO:
                FIND TT_BestCL WHERE TT_BestCL.BestNr = BestStr.BestNr AND TT_BestCL.ButikkNr = iCentrallager NO-ERROR.
                IF NOT AVAIL TT_BestCL THEN DO:
                    CREATE TT_BestCL.
                    ASSIGN TT_BestCL.BestNr        = BestStr.BestNr
                           TT_BestCL.ButikkNr      = iCentrallager
                           TT_BestCL.cBestiltantal = cEntryList.
                END.
                ASSIGN TT_BestCL.totbestillt   = TT_Bestilt.totbestillt + BestStr.Bestilt.
                ASSIGN iIdx = LOOKUP(TRIM(BestStr.Storl),TT_BestNr.Storrelser)
                       ENTRY(iIdx,TT_BestCL.cBestiltantal) = STRING(BestStr.Bestilt)
                       iMin = IF iIdx < iMin THEN iIdx ELSE iMin
                       iMax = IF iIdx > iMax THEN iIdx ELSE iMax.
            END.
        END.
        FOR EACH TT_Bestilt WHERE TT_Bestilt.BestNr = BestHode.BestNr.
            ASSIGN TT_Bestilt.cRest = TT_Bestilt.cBestiltAntal.
        END.
        FOR EACH TT_BestCL WHERE TT_BestCL.BestNr = BestHode.BestNr.
            ASSIGN TT_BestCL.cRest = TT_BestCL.cBestiltAntal.
        END.

        IF iListeType = 107 OR iListeType = 109 THEN DO:
            /* finn störrelser på best */
            FOR EACH BestLevert OF BestHode WHERE 
                (IF AVAIL BestHLev THEN BestLevert.LeveringsNr = BestHLev.LeveringsNr ELSE TRUE)
                NO-LOCK BREAK BY BestLevert.Butik BY BestLevert.STORL BY BestLevert.LeveringsNr:
                FIND TT_Inlevert WHERE TT_Inlevert.BestNr      = BestLevert.BestNr AND
                                       TT_Inlevert.ButikkNr    = BestLevert.Butik AND
                                       TT_Inlevert.LeveringsNr = BestLevert.LeveringsNr NO-ERROR.
                IF NOT AVAIL TT_Inlevert THEN DO:
                    FIND TT_BestButikk WHERE TT_BestButikk.BestNr = BestLevert.BestNr AND TT_BestButikk.ButikkNr = BestLevert.Butik NO-ERROR.
                    CREATE TT_Inlevert.
                    ASSIGN TT_Inlevert.BestNr      = BestLevert.BestNr
                           TT_Inlevert.ButikkNr    = BestLevert.Butik
                           TT_Inlevert.LeveringsNr = BestLevert.LeveringsNr
                           TT_Inlevert.Dato        = BestLevert.levertdato
                           TT_Inlevert.LevertAv    = BestLevert.LevertAv.
                    ASSIGN TT_Inlevert.cInlevAntal = cEntryList
                           TT_Inlevert.cMakulert   = TT_Inlevert.cInlevAntal
                           TT_BestButikk.RadAntal  = TT_BestButikk.RadAntal + 1.
                END.
                ASSIGN iIdx = LOOKUP(TRIM(BestLevert.Storl),TT_BestNr.Storrelser)
                       iMin = IF iIdx < iMin THEN iIdx ELSE iMin
                       iMax = IF iIdx > iMax THEN iIdx ELSE iMax.
                IF iIdx > 0 THEN DO:
                    DO:
                        ASSIGN ENTRY(iIdx,TT_Inlevert.cInlevAntal) = STRING(DECI(ENTRY(iIdx,TT_Inlevert.cInlevAntal)) + BestLevert.Levert)
                               TT_Inlevert.totlevert = TT_Inlevert.totlevert + BestLevert.Levert.
                        FIND TT_Bestilt WHERE TT_Bestilt.BestNr = TT_Inlevert.BestNr AND TT_Bestilt.ButikkNr = TT_Inlevert.ButikkNr.
/*                         ASSIGN ENTRY(iIdx,TT_Bestilt.cRest) = STRING(DECI(ENTRY(iIdx,TT_Bestilt.cRest)) - BestLevert.Levert) */
                        ASSIGN ENTRY(iIdx,TT_Bestilt.cRest) = IF BestLevert.Avskrevet = FALSE THEN STRING(BestLevert.rest) ELSE ""
/*                                                   STRING(DECI(ENTRY(iIdx,TT_Bestilt.cRest)) -                                              */
/*                                                              (IF Bestlevert.avskrevet = TRUE THEN Bestlevert.rest ELSE BestLevert.Levert)) */
                               ENTRY(iIdx,TT_Bestilt.cRest) = IF ENTRY(iIdx,TT_Bestilt.cRest) = "0" THEN "" ELSE ENTRY(iIdx,TT_Bestilt.cRest).
                    END.
                    IF LAST-OF(BestLevert.Storl) THEN DO:
                        IF BestLevert.Avskrevet = TRUE AND Bestlevert.Rest > 0 THEN
                            ASSIGN ENTRY(iIdx,TT_Inlevert.cMakulert) = "M/" + STRING(Bestlevert.Rest).
                        ELSE IF BestLevert.Rest > 0 THEN DO:
/*                             FIND TT_Rest WHERE TT_Rest.BestNr      = BestLevert.BestNr AND                                */
/*                                                TT_Rest.ButikkNr    = BestLevert.Butik AND                                 */
/*                                                TT_Rest.LeveringsNr = 0 NO-ERROR.                                          */
/*                             IF NOT AVAIL TT_Rest THEN DO:                                                                 */
/*                                 CREATE TT_Rest.                                                                           */
/*                                 ASSIGN TT_Rest.BestNr      = BestLevert.BestNr                                            */
/*                                        TT_Rest.ButikkNr    = BestLevert.Butik                                             */
/*                                        TT_Rest.LeveringsNr = 0                                                            */
/*                                        TT_Rest.cRest = cEntryList                                                         */
/*                                        TT_BestButikk.RadAntal  = TT_BestButikk.RadAntal + 1.                              */
/*                             END.                                                                                          */
/*                             ASSIGN ENTRY(iIdx,TT_Rest.cRest) = STRING(DECI(ENTRY(iIdx,TT_Rest.cRest)) + BestLevert.Rest). */
                        END.
                    END.
                END.
            END.
            FOR EACH TT_Bestilt WHERE TT_Bestilt.BestNr = BestHode.BestNr:
                IF TT_Bestilt.cRest = TT_Bestilt.cBestiltAntal THEN
                    ASSIGN TT_Bestilt.cRest = "".
                ELSE DO:
                    ASSIGN lFinnsRest = FALSE.
                    DO iIdx = 1 TO NUM-ENTRIES(TT_Bestilt.cRest):
                        IF INT(ENTRY(iIdx,TT_Bestilt.cRest)) <> 0 THEN DO:
                            ASSIGN lFinnsRest = TRUE.
                            LEAVE.
                        END.
                    END.
                    IF lFinnsRest = TRUE THEN DO:
                        FIND TT_BestButikk WHERE TT_BestButikk.BestNr = TT_Bestilt.BestNr AND TT_BestButikk.ButikkNr = TT_Bestilt.Butik NO-ERROR.
                        ASSIGN TT_BestButikk.RadAntal  = TT_BestButikk.RadAntal + 1.
                    END.
                    ELSE
                        ASSIGN TT_Bestilt.cRest = "".
                END.
            END.
        END.
        ELSE DO:
        END.
        ASSIGN cTmpEntry = FILL(",",iMax - iMin).
               cTmpString = cTmpEntry.
        DO iCount = iMin TO iMax:
            ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_BestNr.Storrelser).
        END.
        ASSIGN TT_BestNr.Storrelser = cTmpString.
        FOR EACH TT_Bestilt WHERE TT_Bestilt.BestNr = TT_BestNr.BestNr:
            ASSIGN cTmpString = cTmpEntry.
            DO iCount = iMin TO iMax:
                 ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Bestilt.cBestiltantal).
            END.
            ASSIGN TT_Bestilt.cBestiltantal = cTmpString.
        END.
        FOR EACH TT_Inlevert WHERE TT_Inlevert.BestNr = TT_BestNr.BestNr:
            DO iCount = iMin TO iMax:
                 ASSIGN ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Inlevert.cInlevantal).
            END.
            ASSIGN TT_Inlevert.cInlevantal = cTmpString.
            ASSIGN cTmpString = cTmpEntry.
            DO iCount = iMin TO iMax:
                 ASSIGN ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Inlevert.cMakulert).
            END.
            ASSIGN TT_Inlevert.cMakulert = cTmpString.
        END.
        FOR EACH TT_Rest WHERE TT_Rest.BestNr = TT_BestNr.BestNr:
            ASSIGN cTmpString = cTmpEntry.
            DO iCount = iMin TO iMax:
                 ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Rest.cRest).
            END.
            ASSIGN TT_Rest.cRest = cTmpString.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PFooter Procedure 
PROCEDURE PFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      PUT UNFORMATTED "<R46><C6><FROM><R46><C113><LINE>" SKIP
                      "<C6>" cKundenavn "<C50>" cPolygon "<C105>" STRING(TODAY) SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PFooterORG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PFooterORG Procedure 
PROCEDURE PFooterORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      PUT UNFORMATTED "<R66><C6><FROM><R66><C80><LINE>" SKIP
                      "<C6>" cKundenavn "<C35>" cPolygon "<C74>" STRING(TODAY) SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivBestillInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivBestillInfo Procedure 
PROCEDURE SkrivBestillInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND farg OF ArtBas NO-LOCK NO-ERROR.
    FIND BestPris WHERE BestPris.Bestnr = BestHode.BestNr AND BestPris.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
    IF LINE-COUNTER > 5 THEN PUT UNFORMATTED "<C6><FROM><C113><LINE>" SKIP.
    PUT UNFORMATTED 
/*         "<C6><FROM><C78><LINE>" SKIP */
/*                     "<C6>Bestillingsnr: <C15>" BestHode.BestNr "<C25>Artikkel: " Artbas.ArtikkelNr "<C50>Direktelevert: " STRING(BestHode.direktelev,"JA/NEI") SKIP                                                                                      */
/*                     "<C6>Levart: <C15>"      ArtBas.Levkod (IF Artbas.beskr <> "" THEN " (" + SUBSTR(ArtBas.beskr,1,20) + ")" ELSE "")   "<C50>Varekost: <C57><RIGHT=C+7>" IF AVAIL BestPris THEN STRING(BestPris.Varekost,"->>,>>9.99") ELSE "0" SKIP   */
/*                     "<C6>Levfarge: <C15>"  ArtBas.LevFargKod (IF AVAIL Farg AND TRIM(Farg.farbeskr) <> "" THEN " (" + Farg.farbeskr + ")" ELSE "") "<C50>Pris: <C57><RIGHT=C+7>" IF AVAIL BestPris THEN STRING(BestPris.Pris,"->>,>>9.99") ELSE "0" SKIP */
/*                     "<C50>Status: <C57>" (IF NUM-ENTRIES(cStatusList) >= BestHode.BestStat THEN ENTRY(BestHode.BestStat,cStatusList) ELSE " ") SKIP                                                                                                      */
                    "<R+1><C6>Størrelser:<C" iStrCol "><U>" REPLACE(getString(TT_BestNr.Storrelser),","," ") "</U>" SKIP(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivBestilling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivBestilling Procedure 
PROCEDURE SkrivBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        DEFINE VARIABLE iAntall  AS INTEGER    NO-UNDO.
        DEFINE VARIABLE cTotList AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE ii       AS INTEGER    NO-UNDO.
        FOR EACH TT_Bestilt:
            ASSIGN iAntall = iAntall + TT_Bestilt.totbestillt.
            IF cTotList = "" THEN
                cTotList = TT_Bestilt.cBestiltantal.
            ELSE DO:
                DO ii = 1 TO NUM-ENTRIES(cTotList).
                    IF INT(ENTRY(ii,TT_Bestilt.cBestiltantal)) > 0 THEN
                        ASSIGN ENTRY(ii,cTotList) = STRING(INT(ENTRY(ii,cTotList)) + INT(ENTRY(ii,TT_Bestilt.cBestiltantal))).
                END.
            END.
        END.
        PUT UNFORMATTED "<R+2><C" iInfoCol ">Bestillt antall:<C13><RIGHT=C+4>" iAntall "<C22>Verdi:<C+2><RIGHT=C+6>" STRING(iAntall * IF AVAIL BestPris THEN
                                                      BestPris.Varekost ELSE 0,"->>>,>>9.99") SKIP.
        RUN SkrivBestillInfo.
/*         PUT UNFORMATTED "<C" iInfoCol "><U>Bestillt</U><C" STRING(iStrCol) ">" REPLACE(getString(TT_Bestilt.cBestiltantal),","," ") SKIP.                                                   */
/*         PUT UNFORMATTED "<R+.5><C" iInfoCol ">Bestillt antall:<C16><RIGHT=C+4>" TT_Bestilt.totbestillt "<C22>Verdi:<C+2><RIGHT=C+6>" STRING(TT_Bestilt.totbestillt * IF AVAIL BestPris THEN */
/*                                                           BestPris.Varekost ELSE 0,"->>>,>>9.99") SKIP.                                                                                     */
        FOR EACH TT_BestButikk BREAK BY TT_BestButikk.ButikkNr BY TT_BestButikk.BestNr.
            FIND TT_BestNr WHERE TT_BestNr.BestNr = TT_BestButikk.BestNr.
            FOR EACH TT_Bestilt WHERE TT_Bestilt.ButikkNr = TT_BestButikk.ButikkNr AND TT_Bestilt.BestNr = TT_BestButikk.BestNr BY TT_Bestilt.BestNr:
/*                 PUT UNFORMATTED "<R+.5><C" iInfoCol ">Bestillt antall:<C16><RIGHT=C+4>" TT_Bestilt.totbestillt "<C22>Verdi:<C+2><RIGHT=C+6>" STRING(TT_Bestilt.totbestillt * IF AVAIL BestPris THEN */
/*                                                               BestPris.Varekost ELSE 0,"->>>,>>9.99") SKIP.                                                                                         */
                PUT UNFORMATTED "<C" iInfoCol "><U>Butikk:<RIGHT=C+3>" STRING(TT_Bestilt.ButikkNr) "</U><C13><RIGHT=C+4>" TT_Bestilt.totbestillt "<C" STRING(iStrCol) ">" REPLACE(getString(TT_Bestilt.cBestiltantal),","," ") SKIP.
            END.
        END.
        PUT UNFORMATTED "<R+1><C" iInfoCol "><U>Tot:</U><C" STRING(iStrCol) ">" REPLACE(getString(cTotList),","," ") SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivInleveranser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivInleveranser Procedure 
PROCEDURE SkrivInleveranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSkrevet AS LOGICAL    NO-UNDO.
    FIND BestHode WHERE bestHode.Bestnr = TT_BestNr.Bestnr NO-LOCK.
    FIND ArtBas OF BestHode.
    IF LINE-COUNTER + TT_BestButikk.RadAntal > 65 THEN DO:
        RUN PFooter.
        PAGE.
        VIEW FRAME PageHeader.
    END.

/*     RUN SkrivBestillInfo. */
    FOR EACH TT_Bestilt WHERE TT_Bestilt.ButikkNr = TT_BestButikk.ButikkNr AND TT_Bestilt.BestNr = TT_BestButikk.BestNr BY TT_Bestilt.BestNr:
        PUT UNFORMATTED "<R+1.5><C" iInfoCol ">Bestillt antall:<C13><RIGHT=C+4>" TT_Bestilt.totbestillt "<C22>Verdi:<C+2><RIGHT=C+6>" STRING(TT_Bestilt.totbestillt * IF AVAIL BestPris THEN
                                                          BestPris.Varekost ELSE 0,"->>>,>>9.99") SKIP.
        IF NOT lSkrevet THEN
            RUN SkrivBestillInfo.
        ASSIGN lSkrevet = TRUE.
        PUT UNFORMATTED "<C" iInfoCol "><U>Bestillt</U><C" STRING(iStrCol) ">" REPLACE(getString(TT_Bestilt.cBestiltantal),","," ") SKIP.
/*         PUT UNFORMATTED "<R+.5><C" iInfoCol ">Bestillt antall:<C16><RIGHT=C+4>" TT_Bestilt.totbestillt "<C22>Verdi:<C+2><RIGHT=C+6>" STRING(TT_Bestilt.totbestillt * IF AVAIL BestPris THEN */
/*                                                           BestPris.Varekost ELSE 0,"->>>,>>9.99") SKIP.                                                                                     */
        FIND FIRST TT_Inlevert WHERE TT_Inlevert.Bestnr = TT_BestNr.bestnr AND TT_Inlevert.ButikkNr = TT_Bestilt.ButikkNr NO-ERROR.
        IF NOT AVAIL TT_Inlevert THEN
            .
/*             PUT UNFORMATTED "<C6>" TT_Bestilt.ButikkNr "<C9>" "Ingen innleveranse" SKIP(2). */
        ELSE IF iListeType <> 101 THEN DO:
/*             PUT UNFORMATTED "<R+.5><C" iInfoCol "><U>" "Innlevert</U><R+.5>" SKIP. */
            FOR EACH TT_Inlevert WHERE TT_Inlevert.Bestnr = TT_Bestnr.Bestnr AND TT_Inlevert.ButikkNr = TT_Bestilt.ButikkNr BREAK BY TT_Inlevert.leveringsnr:
                TT_Inlevert.LevertAv = SUBSTR(TT_Inlevert.LevertAv,1,7).
                
                IF TT_Inlevert.totlevert > 0 THEN DO:
                    PUT UNFORMATTED "<R+.5><C" iInfoCol ">Innlevert antall:<C13><RIGHT=C+4>" (IF BestHode.Beststat = 7 THEN -1 ELSE 1) * TT_Inlevert.totlevert "<C22>Verdi:<C+2><RIGHT=C+6>" 
                            STRING((IF BestHode.Beststat = 7 THEN -1 ELSE 1) * TT_Inlevert.totlevert * IF AVAIL BestPris THEN BestPris.Varekost ELSE 0,"->>>,>>9.99") 
                        (IF BestHode.Beststat = 7 THEN "<C+2>(" + cBestStatus + ")" ELSE "") SKIP.
                    PUT UNFORMATTED "<C" iInfoCol ">" TT_Inlevert.dato "<C" iSignCol ">" TT_Inlevert.LevertAv /* " " TT_Inlevert.leveringsnr */ "<C" iStrCol ">" REPLACE(getString(TT_Inlevert.cInlevantal),","," ") SKIP.
                END.
                IF LAST-OF(TT_Inlevert.leveringsnr) AND TT_Inlevert.cMakulert <> FILL(",",NUM-ENTRIES(TT_Bestnr.storrelser) - 1) THEN
                    PUT UNFORMATTED "<C" iInfoCol ">" TT_Inlevert.dato "<C" iSignCol ">" TT_Inlevert.LevertAv /* " " TT_Inlevert.leveringsnr */ "<C" iStrCol ">" REPLACE(getString(TT_Inlevert.cMakulert),","," ") SKIP.
            END.
/*             FIND TT_Rest WHERE TT_Rest.Bestnr = TT_Bestnr.Bestnr AND TT_Rest.ButikkNr = TT_Bestilt.ButikkNr NO-ERROR. */
/*             IF AVAIL TT_Rest THEN                                                                                     */
            IF TT_Bestilt.cRest <> "" THEN
                PUT UNFORMATTED "<R+1><C" iInfoCol ">" "REST" "<C" iStrCol ">" REPLACE(getString(TT_Bestilt.cRest),","," ") SKIP.
        END.
        PUT SKIP(1).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivInleveranserX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivInleveranserX Procedure 
PROCEDURE SkrivInleveranserX :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    FIND BestHode WHERE bestHode.Bestnr = TT_BestNr.Bestnr NO-LOCK.
    FIND ArtBas OF BestHode.
    IF LINE-COUNTER + TT_BestButikk.RadAntal > 65 THEN DO:
        RUN PFooter.
        PAGE.
        VIEW FRAME PageHeader.
    END.
    RUN SkrivBestillInfo.
    FOR EACH TT_Bestilt WHERE TT_Bestilt.ButikkNr = TT_BestButikk.ButikkNr AND TT_Bestilt.BestNr = TT_BestButikk.BestNr BY TT_Bestilt.BestNr:
        IF iListeType <> 101 THEN DO:
            PUT UNFORMATTED "<R+.5><C" iInfoCol "><U>" "Innlevert</U><R+.5>" SKIP.
            FOR EACH TT_Inlevert WHERE TT_Inlevert.Bestnr = TT_Bestnr.Bestnr AND TT_Inlevert.ButikkNr = TT_Bestilt.ButikkNr BREAK BY TT_Inlevert.leveringsnr:
                PUT UNFORMATTED "<C" iInfoCol ">" TT_Inlevert.dato "<C" iSignCol ">" TT_Inlevert.LevertAv /* " " TT_Inlevert.leveringsnr */ "<C" iStrCol ">" REPLACE(getString(TT_Inlevert.cInlevantal),","," ") SKIP.
                PUT UNFORMATTED "<R+.5><C" iInfoCol ">Innlevert antall:<C16><RIGHT=C+4>" (IF BestHode.Beststat = 7 THEN -1 ELSE 1) * TT_Inlevert.totlevert "<C22>Verdi:<C+2><RIGHT=C+6>" 
                        STRING((IF BestHode.Beststat = 7 THEN -1 ELSE 1) * TT_Inlevert.totlevert * IF AVAIL BestPris THEN BestPris.Varekost ELSE 0,"->>>,>>9.99") 
                    (IF BestHode.Beststat = 7 THEN "<C+2>(" + cBestStatus + ")" ELSE "") SKIP.
                IF LAST-OF(TT_Inlevert.leveringsnr) AND TT_Inlevert.cMakulert <> FILL(",",NUM-ENTRIES(TT_Bestnr.storrelser) - 1) THEN
                    PUT UNFORMATTED "<C" iInfoCol ">" TT_Inlevert.dato "<C" iSignCol ">" TT_Inlevert.LevertAv /* " " TT_Inlevert.leveringsnr */ "<C" iStrCol ">" REPLACE(getString(TT_Inlevert.cMakulert),","," ") SKIP.
            END.
/*             FIND TT_Rest WHERE TT_Rest.Bestnr = TT_Bestnr.Bestnr AND TT_Rest.ButikkNr = TT_Bestilt.ButikkNr NO-ERROR. */
/*             IF AVAIL TT_Rest THEN                                                                                     */
            IF TT_Bestilt.cRest <> "" THEN
                PUT UNFORMATTED "<C" iInfoCol ">" "REST" "<C" iStrCol ">" REPLACE(getString(TT_Bestilt.cRest),","," ") SKIP.
        END.
        PUT SKIP(1).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivInlevHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivInlevHeader Procedure 
PROCEDURE SkrivInlevHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF iListeType <> 101 THEN DO:
        FIND syspara WHERE SysPara.SysHId = 5 AND 
                           SysPara.SysGr  = 2 and
                           SysPara.ParaNr = BestHode.Beststat NO-LOCK NO-ERROR.

        IF AVAIL SysPara THEN
            ASSIGN cBestStatus = SysPara.Beskrivelse.
    END.
    FIND Levbas OF BestHode NO-LOCK NO-ERROR.
    FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
    FIND Farg OF ArtBas NO-LOCK NO-ERROR.
    FIND Material OF ArtBas NO-LOCK NO-ERROR.
    FIND Varemerke OF ArtBAs NO-LOCK NO-ERROR.
    FIND FIRST BestPris OF BestHode WHERE BestPris.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
    IF NOT AVAIL BestPris THEN
        FIND LAST bestPris OF BestHode NO-LOCK NO-ERROR.
    FIND Prisprofil OF bestpris NO-LOCK NO-ERROR.
/*     ASSIGN cLevNamn    = IF AVAIL LevBas THEN LevBas.Levnamn ELSE "Ukjent". */

    IF cBildeFil <> "" THEN 
        PUT UNFORMATTED
           "<TRANSPARENT=false><R" STRING(21)  ",5><C112><#3><R" STRING(5) ",25><C89><IMAGE#3=" 
           cBildeFil  ">".
    PUT UNFORMATTED "<P10>" (IF iListeType = 101 THEN "<R4>" ELSE "<R5><C6><B>Status: <C15>" + cBestStatus + "</B>")
                    "<R+1><C6>Leverandør: <C15>" LevBas.LevNr " " LevBas.levnamn
                    "<R+1><C6>Lev.art.nr: <C15>" ArtBas.Levkod
                    "<R+1><C6>Lev.farge:  <C15>" ArtBas.LevFargkod " " cLevNamn
                    "<R+1><C6>Vg/LpNr/kat:<C15>" ArtBas.Vg "/" ArtBas.lopnr "/" ArtBas.VgKat
                    "<R+1><C6>Sesong:     <C15>" Sasong.Sasong " " Sasong.sasbeskr
                    "<R+1><C6>Farge:      <C15>" Farg.farg " " Farg.farbeskr
                    "<R+1><C6>Material:   <C15>" Material.matkod " " Material.matbeskr
                    "<R+1><C6>Varemerke:  <C15>" Varemerke.VMId " " Varemerke.beskrivelse
                    "<R+1><C6>Lev.tid:    <C15>"
                    "<R+1><C6>Varetekst:  <C15>" ArtBas.Beskr
                    "<R+1><C6>Bongtekst:  <C15>" ArtBas.Bongtekst
                    "<R+1><C6>Artikkelnr: <C15>" ArtBas.Artikkelnr.
    IF iListetype = 108 THEN
        PUT UNFORMATTED "<R5><C50><B>Kroner<C60>Valuta</B>"
                        "<R+1><C40>Valutapris:" 
                        "<R+1><C40>Inkjøpspris:<C60><RIGHT=C+4><B>Prosent</B>"
                        "<R+1><C40>Rabatt 1 (-):" 
                        "<R+1><C40>Rabatt 2 (-):"
                        "<R+1><C40>Frakt (+):"
                        "<R+1><C40>Div kost (+):"
                        "<R+1><C40>Rabatt 3 (-):"
                        "<R+1><C40>Varekost:"
                        "<R+1><C40>DB (+):"
                        "<R+1><C40>Mva (+):"
                        "<R+1><C40>Pris:"
                        "<R+1><C40>Pris (Euro):"
                        "<R+2><C6><FROM><C113><LINE><R-1><P10>".
    ELSE
        PUT UNFORMATTED "<R5><C50><B>Kroner<C60>Valuta</B>"
                        "<R+1><C40>Valutapris:   <C50><RIGHT=C+6>" BestPris.ValPris FORMAT ">>>,>>9.99"
                        "<R+1><C40>Inkjøpspris:  <C50><RIGHT=C+6>" BestPris.InnkjopsPris FORMAT ">>>,>>9.99" "<C60><RIGHT=C+4><B>Prosent</B>"
                        "<R+1><C40>Rabatt 1 (-): <C50><RIGHT=C+6>" BestPris.Rab1Kr  FORMAT ">>>,>>9.99"      "<C60><RIGHT=C+4>" BestPris.Rab1% FORMAT ">>9.99"
                        "<R+1><C40>Rabatt 2 (-): <C50><RIGHT=C+6>" BestPris.Rab2Kr  FORMAT ">>>,>>9.99"      "<C60><RIGHT=C+4>" BestPris.Rab2%  FORMAT ">>9.99"
                        "<R+1><C40>Frakt (+):    <C50><RIGHT=C+6>" BestPris.Frakt   FORMAT ">>>,>>9.99"      "<C60><RIGHT=C+4>" BestPris.Frakt%  FORMAT ">>9.99"
                        "<R+1><C40>Div kost (+): <C50><RIGHT=C+6>" BestPris.DivKostKr FORMAT ">>>,>>9.99"    "<C60><RIGHT=C+4>" BestPris.DivKost%  FORMAT ">>9.99"
                        "<R+1><C40>Rabatt 3 (-): <C50><RIGHT=C+6>" BestPris.Rab3Kr  FORMAT ">>>,>>9.99"      "<C60><RIGHT=C+4>" BestPris.Rab3%  FORMAT ">>9.99"
                        "<R+1><C40>Varekost:     <C50><RIGHT=C+6>" BestPris.VareKost FORMAT ">>>,>>9.99"
                        "<R+1><C40>DB (+):       <C50><RIGHT=C+6>" BestPris.DBKr    FORMAT ">>>,>>9.99"      "<C60><RIGHT=C+4>" BestPris.DB%  FORMAT ">>9.99"
                        "<R+1><C40>Mva (+):      <C50><RIGHT=C+6>" BestPris.MvaKr   FORMAT ">>>,>>9.99"      "<C60><RIGHT=C+4>" BestPris.Mva%  FORMAT ">>9.99"
                        "<R+1><C40>Pris:         <C50><RIGHT=C+6><B>" BestPris.Pris FORMAT ">>>,>>9.99"      "</B>"
                        "<R+1><C40>Pris (Euro):  <C50><RIGHT=C+6>" BestPris.EuroPris FORMAT ">>>,>>9.99"     "<C60><RIGHT=C+4>" STRING(BestPris.EuroManuel,"Manuel/")
                        "<R+2><C6><FROM><C113><LINE><R-1><P10>".
/*     BestPris.BestNr       */
/*         BestPris.ProfilNr */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapport Procedure 
PROCEDURE SkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR pcRappFil      AS CHAR NO-UNDO.
    DEF VAR iRad           AS INTE NO-UNDO.
    DEF VAR cSendtDato     AS CHAR NO-UNDO.
    DEF VAR cUser          AS CHAR NO-UNDO.

/*     FIND Ordre WHERE ordre.ordrenr = ipOrdreNr NO-LOCK. */
    ASSIGN cUser = USERID("skotex")
           cUser = IF cUser = "" THEN "xx" ELSE cUser
           pcRappFil = SESSION:TEMP-DIRECTORY + "Mottak-" + cUser + "-" + STRING(besthode.bestnr) + ".xpr"
           cHeaderTxt = IF iListetype = 101 OR iListetype = 108 THEN "Bestillingskort "ELSE "Innleveranserapport"
               .

    OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(68).
    PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
    PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE><COPIES=' + string(iNumCopies) + '>'.
  /*   PUT CONTROL '<SILENT=TRUE>'. */
  /*   PUT CONTROL '<PRINT=NO>'.    */
    PUT CONTROL '<PREVIEW=ZoomToWidth>'.
  /*   PUT CONTROL '<PREVIEW=PDF>'. */
    /*put control "<PrinterSetup>". */
    IF iListeType = 101 THEN DO:
        VIEW FRAME PageHeaderBest.
        RUN SkrivInlevHeader.
        RUN SkrivBestilling.
    END.
    ELSE DO:
/*     DO: */
        FOR EACH TT_BestButikk BREAK BY TT_BestButikk.ButikkNr BY TT_BestButikk.BestNr.
            FIND TT_BestNr WHERE TT_BestNr.BestNr = TT_BestButikk.BestNr.
/*             IF lFolgesedel = TRUE THEN DO: */
                IF FIRST-OF(TT_BestButikk.ButikkNr) AND NOT FIRST(TT_BestButikk.ButikkNr) THEN DO:
                    RUN PFooter.
                    PAGE.
                    ASSIGN cButikkNr = STRING(TT_BestButikk.ButikkNr) + " " + TT_BestButikk.Butnamn.
                    VIEW FRAME PageHeader.
                    RUN SkrivInlevHeader.
                END.
                ELSE IF FIRST-OF(TT_BestButikk.ButikkNr) THEN DO:
                    ASSIGN cButikkNr = STRING(TT_BestButikk.ButikkNr) + " " + TT_BestButikk.Butnamn.
                    VIEW FRAME PageHeader.
                    IF 1 > 0 THEN
                        RUN SkrivInlevHeader.
                END.
/*             END.                                                */
/* /*             ELSE DO:                                      */ */
/* /*                 IF FIRST(TT_BestButikk.ButikkNr) THEN DO: */ */
/* /*                     VIEW FRAME PageHeader.                */ */
/* /*                     RUN SkrivInlevHeader.                 */ */
/* /*                 END.                                      */ */
/* /*             END.                                          */ */
            RUN SkrivInleveranser.
        END.
    END.
    RUN PFooter.
    OUTPUT CLOSE.
   RUN VisXprint.p (pcRappFil).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getBildeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildeFil Procedure 
FUNCTION getBildeFil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ocBildeFil AS CHARACTER  NO-UNDO.
  FIND BildeRegister NO-LOCK WHERE
    BildeRegister.BildNr = ipBildNr NO-ERROR.
  IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:
    IF VALID-HANDLE(wLibHandle) THEN
      RUN HentBildePeker IN wLibHandle (INPUT ipBildNr, 1, BildeRegister.FilNavn, OUTPUT ocBildeFil).
  END.
  /* cBlanktBilde */
  RETURN ocBildeFil.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getString Procedure 
FUNCTION getString RETURNS CHARACTER
  ( INPUT cEntryList AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iStart AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iStep  AS DECIMAL    NO-UNDO.
  ASSIGN iStart = iStrCol
         iStep  = 3.
  DO iCount = 1 TO NUM-ENTRIES(cEntryList):
      ENTRY(iCount,cEntryList) = "<C" + REPLACE(STRING(iStart + ((iCount - 1) * iStep) + ((iCount - 1) * .8)),",",".") + "><RIGHT=C+4.5>" + ENTRY(iCount,cEntryList).
/*       ENTRY(iCount,cEntryList) = "<C" + STRING(iStart + ((iCount - 1) * iStep)) + "><RIGHT=C+4>" + ENTRY(iCount,cEntryList). */
  END.
  RETURN cEntryList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

