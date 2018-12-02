&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"sdo/dovbuffer.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR iInt         AS INT   NO-UNDO.
DEF VAR lOk          AS LOG   NO-UNDO.
DEF VAR cTekst       AS CHAR  NO-UNDO.
DEF VAR lAddMode     AS INTE  NO-UNDO.
DEF VAR iBuntNr      AS INT   FORMAT "->,>>>,>>9" NO-UNDO.
DEF VAR iButikkNrFra AS INT   NO-UNDO.
DEF VAR iButikkNrTil AS INT   NO-UNDO.
DEF VAR cTilButikk   AS CHAR  NO-UNDO.
DEF VAR cFraButikk   AS CHAR  NO-UNDO.
DEF VAR rArtikkelNr  AS DEC   NO-UNDO.
DEFINE VARIABLE hDataSource  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hNavigation  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hScanner AS HANDLE      NO-UNDO.
DEFINE VARIABLE hContainer AS HANDLE      NO-UNDO.
DEFINE VARIABLE cScannerPara AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSeriellScanner   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lClaimed AS LOGICAL     NO-UNDO.
define variable lWinScan as logical no-undo.
DEF VAR hArtBasSok AS HANDLE NO-UNDO.
DEF VAR i1ButNr AS INT FORMAT ">>>>>9" NO-UNDO.
DEF VAR i2ButNr AS INT FORMAT ">>>>>9" NO-UNDO.
DEF VAR dDatoOppdatert AS DATE NO-UNDO.

{proclib.i}
{functions\dialogboksaktiv.i}

ON "ALT-A":U OF CURRENT-WINDOW ANYWHERE   
DO:
  RUN addRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dovbuffer.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.ButikkNrFra RowObject.ButikkNrTil ~
RowObject.Merknad RowObject.Vg RowObject.LopNr RowObject.Storl ~
RowObject.Antall 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS B-Artikler B-Scanner B-SokButikkFra ~
B-SokButikkTil B-SokLopnr B-SokVg T-Bytt RECT-55 
&Scoped-Define DISPLAYED-FIELDS RowObject.fFraButikk RowObject.ButikkNrFra ~
RowObject.fTilbutikk RowObject.ButikkNrTil RowObject.Merknad RowObject.Vg ~
RowObject.fVgBeskr RowObject.LopNr RowObject.Storl RowObject.TilStorl ~
RowObject.Antall RowObject.VareKost 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS T-Bytt 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Artikler  NO-FOCUS
     LABEL "&Hent artikler" 
     SIZE 23.8 BY 1.14.

DEFINE BUTTON B-Scanner  NO-FOCUS
     LABEL "S&trekkoderegistrering" 
     SIZE 23.8 BY 1.14.

DEFINE BUTTON B-SokButikkFra 
     IMAGE-UP FILE "icon/e-search.bmp":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 4.6 BY 1.14.

DEFINE BUTTON B-SokButikkTil 
     IMAGE-UP FILE "icon/e-search.bmp":U NO-FOCUS
     LABEL "SokTil" 
     SIZE 4.6 BY 1.14.

DEFINE BUTTON B-SokLopnr 
     IMAGE-UP FILE "icon/e-search.bmp":U NO-FOCUS
     LABEL "SokLopnr" 
     SIZE 4.6 BY 1.14.

DEFINE BUTTON B-SokVg 
     IMAGE-UP FILE "icon/e-search.bmp":U NO-FOCUS
     LABEL "SokVg" 
     SIZE 4.6 BY 1.14.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125.8 BY 5.48.

DEFINE VARIABLE T-Bytt AS LOGICAL INITIAL no 
     LABEL "Bytt størrelse" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B-Artikler AT ROW 5.05 COL 44.2 WIDGET-ID 2
     B-Scanner AT ROW 5.05 COL 20.2
     B-SokButikkFra AT ROW 1.95 COL 31.2 NO-TAB-STOP 
     B-SokButikkTil AT ROW 2.95 COL 31.2 NO-TAB-STOP 
     B-SokLopnr AT ROW 3 COL 89 NO-TAB-STOP 
     B-SokVg AT ROW 2 COL 89 NO-TAB-STOP 
     RowObject.fFraButikk AT ROW 2 COL 34.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     T-Bytt AT ROW 4.05 COL 100 NO-TAB-STOP 
     RowObject.ButikkNrFra AT ROW 2 COL 18.2 COLON-ALIGNED HELP
          "Alt-A for å addere en post."
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1 TOOLTIP "Alt-A for å addere en ny post."
     RowObject.fTilbutikk AT ROW 3 COL 34.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.ButikkNrTil AT ROW 3 COL 18.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.Merknad AT ROW 4 COL 18.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     RowObject.Vg AT ROW 2.05 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.fVgBeskr AT ROW 2.05 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.LopNr AT ROW 3.05 COL 76 COLON-ALIGNED FORMAT "->>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.Storl AT ROW 4.05 COL 76 COLON-ALIGNED FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.TilStorl AT ROW 4.05 COL 87 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.Antall AT ROW 5.05 COL 76 COLON-ALIGNED HELP
          "Antall par som skal overføres"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.VareKost AT ROW 5.05 COL 103 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RECT-55 AT ROW 1.24 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dovbuffer.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {sdo/dovbuffer.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 5.76
         WIDTH              = 126.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Antall IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN RowObject.ButikkNrFra IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN RowObject.fFraButikk IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fFraButikk:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fTilbutikk IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fTilbutikk:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fVgBeskr IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fVgBeskr:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.LopNr IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Storl IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.TilStorl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.VareKost IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.Antall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Antall vTableWin
ON TAB OF RowObject.Antall IN FRAME F-Main /* Antall */
OR "RETURN":U OF RowObject.Antall
DO:
    IF INT(SELF:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Registrera antal."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    RUN updateRecord.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikler vTableWin
ON CHOOSE OF B-Artikler IN FRAME F-Main /* Hent artikler */
DO:
    IF NOT VALID-HANDLE(hArtBasSok) THEN 
    DO WITH FRAME DEFAULT-FRAME:
      PUBLISH 'getBuntNr' (OUTPUT iBuntNr).
      ASSIGN
          i1ButNr = INT(RowObject.ButikkNrFra:SCREEN-VALUE)
          i2ButNr = INT(RowObject.ButikkNrTil:SCREEN-VALUE)
          .
      RUN dvelgfratilbutikk (INPUT-OUTPUT i1ButNr, INPUT-OUTPUT i2ButNr).
      IF (i1ButNr = 0 OR i2ButNr = 0) THEN
          RETURN NO-APPLY.
      ELSE DO:
          ASSIGN
              RowObject.ButikkNrFra:SCREEN-VALUE = STRING(i1ButNr) 
              RowObject.ButikkNrTil:SCREEN-VALUE = STRING(i2ButNr)
              .

          RUN ValiderButikk IN hDataSource 
              (INPUT INPUT RowObject.ButikkNrFra).
          IF RETURN-VALUE <> "FALSE" THEN
          DO:
              ASSIGN
                  rowObject.fFraButikk:SCREEN-VALUE = RETURN-VALUE
                  RowObject.fFraButikk:MODIFIED     = FALSE
                  .
          END.
          RUN ValiderButikk IN hDataSource 
              (INPUT INPUT RowObject.ButikkNrTil).
          IF RETURN-VALUE <> "FALSE" THEN
          DO:
              ASSIGN
                  rowObject.fTilButikk:SCREEN-VALUE = RETURN-VALUE
                  RowObject.fTilButikk:MODIFIED     = FALSE
                  .
          END.
      END.
      IF RowObject.ButikkNrFra:SCREEN-VALUE = RowObject.ButikkNrTil:SCREEN-VALUE THEN 
      DO:
          MESSAGE "Fra og til butikk er like"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RUN cancelRecord.
          RETURN NO-APPLY.
      END.
      RUN ArtBasSok.w PERSIST SET hArtBasSok.
      RUN InitializeObject IN hArtBasSok.
      RUN setRunProc IN hArtBasSok (TRUE).
      RUN setLager   IN hArtBasSok (i1ButNr).
    END.

    RUN MoveToTop IN hArtBasSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Scanner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Scanner vTableWin
ON CHOOSE OF B-Scanner IN FRAME F-Main /* Strekkoderegistrering */
DO:
  DO WITH FRAME F-Main:
    IF T-Bytt:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        MESSAGE "Bytt størrelse ikke tillatt"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN cancelRecord.
        RETURN NO-APPLY.
    END.
    APPLY "TAB" TO RowObject.ButikkNrFra.
    IF ERROR-STATUS:ERROR THEN DO:
        APPLY "ENTRY" TO RowObject.ButikkNrFra.
        RETURN NO-APPLY.
    END.
    ELSE
        PROCESS EVENTS.
    APPLY "TAB" TO RowObject.ButikkNrTil.
    IF ERROR-STATUS:ERROR THEN DO:
        APPLY "ENTRY" TO RowObject.ButikkNrTil.
        RETURN NO-APPLY.
    END.
    ELSE
        PROCESS EVENTS.
    IF RowObject.ButikkNrFra:SCREEN-VALUE = RowObject.ButikkNrTil:SCREEN-VALUE THEN DO:
        MESSAGE "Fra og til butikk er like"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN cancelRecord.
        RETURN NO-APPLY.
    END.
    ASSIGN
      iButikkNrFra = INPUT RowObject.ButikkNrFra
      iButikkNrTil = INPUT RowObject.ButikkNrTil
      cFraButikk   = RowObject.fFraButikk:SCREEN-VALUE
      cTilButikk   = RowObject.fTilButikk:SCREEN-VALUE
      .
  END.
  IF VALID-HANDLE(hScanner) THEN DO:
/*       hContainer:SENSITIVE = FALSE. */
      lWinScan = true.
      RUN ClearInput IN hScanner.
      RUN winscannerinput.w (INPUT THIS-PROCEDURE,hScanner).
      lWinScan = false.
/*       hContainer:SENSITIVE = TRUE. */
  END.
  ELSE
      RUN gscannerinput.w (INPUT THIS-PROCEDURE:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikkFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikkFra vTableWin
ON CHOOSE OF B-SokButikkFra IN FRAME F-Main /* Button 1 */
OR F10 OF RowObject.ButikkNrFra
DO:
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gButiker.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    RowObject.ButikkNrFra:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        RowObject.ButikkNrFra:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        RowObject.fFraButikk:SCREEN-VALUE  = ENTRY(3,cTekst,CHR(1))
        RowObject.fFraButikk:MODIFIED      = FALSE
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "VALUE-CHANGED":U TO RowObject.ButikkNrFra.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikkTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikkTil vTableWin
ON CHOOSE OF B-SokButikkTil IN FRAME F-Main /* SokTil */
OR F10 OF RowObject.ButikkNrTil
DO:
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gButiker.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    RowObject.ButikkNrFra:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        RowObject.ButikkNrTil:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        RowObject.fTilButikk:SCREEN-VALUE  = ENTRY(3,cTekst,CHR(1))
        RowObject.fTilButikk:MODIFIED      = FALSE
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "VALUE-CHANGED":U TO RowObject.ButikkNrTil.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLopnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLopnr vTableWin
ON CHOOSE OF B-SokLopnr IN FRAME F-Main /* SokLopnr */
OR F10 OF RowObject.LopNr
DO:
  RUN gartbasvgsok.w (INPUT INPUT RowObject.Vg, INPUT 1, INPUT-OUTPUT iInt).
  IF RETURN-VALUE <> "AVBRYT" THEN
  DO:
      IF iInt <> INT(RowObject.LopNr:SCREEN-VALUE) THEN
        DYNAMIC-FUNCTION('setDataModified':U,
          INPUT true /* LOGICAL */).

      ASSIGN
          RowObject.LopNr:SCREEN-VALUE = STRING(iInt)
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokVg vTableWin
ON CHOOSE OF B-SokVg IN FRAME F-Main /* SokVg */
OR F10 OF RowObject.Vg
DO:
  RUN gvargr.w (INPUT-OUTPUT iInt).
  IF RETURN-VALUE <> "AVBRYT" THEN
  DO:
      ASSIGN
          RowObject.Vg:SCREEN-VALUE       = STRING(iInt)
          RowObject.LopNr:SCREEN-VALUE    = ""
          RowObject.fVgBeskr:SCREEN-VALUE = RETURN-VALUE
          RowObject.fVgBeskr:MODIFIED     = FALSE
          .
      IF iInt <> INT(RowObject.Vg:SCREEN-VALUE) THEN
        DYNAMIC-FUNCTION('setDataModified':U,
          INPUT true /* LOGICAL */).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.ButikkNrFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.ButikkNrFra vTableWin
ON TAB OF RowObject.ButikkNrFra IN FRAME F-Main /* Fra butikknummer */
OR "RETURN":U OF RowObject.ButikkNrFra /*OR "LEAVE":U OF RowObject.ButikkNrFra */
DO:
   RUN ValiderButikk IN hDataSource 
       (INPUT INPUT RowObject.ButikkNrFra).
   IF RETURN-VALUE = "FALSE" THEN
   DO:
       ASSIGN
           rowObject.fFraButikk:SCREEN-VALUE = ""
           RowObject.fFraButikk:MODIFIED     = FALSE
           .
       MESSAGE "Ugyldig butikknummer!"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
   END.
   ELSE DO:
       ASSIGN
           rowObject.fFraButikk:SCREEN-VALUE = RETURN-VALUE
           RowObject.fFraButikk:MODIFIED     = FALSE
           .
       APPLY "ENTRY" TO RowObject.ButikkNrTil.
       RETURN NO-APPLY.
/*        APPLY LASTKEY. */

   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.ButikkNrTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.ButikkNrTil vTableWin
ON TAB OF RowObject.ButikkNrTil IN FRAME F-Main /* Til butikk */
OR "RETURN":U OF RowObject.ButikkNrTil /*OR "LEAVE":U OF RowObject.ButikkNrTil*/
DO:

   RUN ValiderButikk IN hDataSource 
       (INPUT INPUT RowObject.ButikkNrTil).
   IF RETURN-VALUE = "FALSE" THEN
   DO:
       MESSAGE "Ugyldig butikknummer!"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
   END.    
  
   IF INPUT RowObject.ButikkNrTil <> 0 AND
      INPUT RowObject.ButikkNrFra <> 0 THEN
   DO:
       IF INPUT RowObject.ButikkNrTil = INPUT RowObject.ButikkNrFra THEN
       DO:
           MESSAGE "Fra og til butikk kan ikke være like."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN NO-APPLY.
       END.
   END.

   ASSIGN
     RowObject.fTilButikk:SCREEN-VALUE = RETURN-VALUE
     RowObject.fTilButikk:MODIFIED     = FALSE
     T-Bytt:SENSITIVE = RowObject.ButikkNrFra:SCREEN-VALUE =
                            RowObject.ButikkNrTil:SCREEN-VALUE
     .

   APPLY "ENTRY" TO RowObject.Merknad.
   RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.LopNr vTableWin
ON TAB OF RowObject.LopNr IN FRAME F-Main /* LpNr */
OR "RETURN":U OF RowObject.LopNr
DO:
   RUN ValiderArtikkel IN hDataSource 
       (INPUT INPUT RowObject.Vg, INPUT INPUT RowObject.LopNr).
   IF RETURN-VALUE = "FALSE" THEN
   DO:
       MESSAGE "Ugyldig varegruppe og løpenummer!"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
   END.
   ELSE
       APPLY LASTKEY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Storl vTableWin
ON ANY-PRINTABLE OF RowObject.Storl IN FRAME F-Main /* Størrelse */
DO:
    DEF VAR cTkn       AS CHAR NO-UNDO.
    DEF VAR cString    AS CHAR NO-UNDO.
    DEF VAR iTst       AS INTE NO-UNDO.
    DEF VAR iCursor    AS INTE NO-UNDO.
    DEF VAR lChar      AS LOGI NO-UNDO.
    DEF VAR lCharInput AS LOGI NO-UNDO.
    DEF VAR lReturn    AS LOGI NO-UNDO.
    DEF VAR cRest      AS CHAR NO-UNDO.
    ASSIGN iTst = INT(SELF:SCREEN-VALUE) NO-ERROR.
           
    ASSIGN lChar      = ERROR-STATUS:ERROR
           cTkn       = KEYFUNCTION(LASTKEY)
           lCharInput = NOT cTkn = "," AND NOT cTkn = "." AND
                        NOT CAN-DO("0,1,2,3,4,5,6,7,8,9",cTkn).
    IF SELF:CURSOR-OFFSET > 10 THEN
        ASSIGN lReturn = TRUE.
    ELSE IF lCharInput AND NOT CAN-DO("A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z",CAPS(cTkn)) THEN
        ASSIGN lReturn = TRUE.
    ELSE IF lChar AND (cTkn = "." OR cTkn = ",") THEN /* om vi har Char-storlek i fältet */
        ASSIGN lReturn = TRUE.
    ELSE IF LENGTH(SELF:SCREEN-VALUE) > 2 AND 
        (cTkn = "." OR cTkn = ",") THEN
        ASSIGN lReturn = TRUE.
    ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,".") = 2 AND
        ((cTkn = "." OR cTkn = ",") OR lCharInput) THEN
        ASSIGN lReturn = TRUE.
    ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,".") = 2 AND ENTRY(2,SELF:SCREEN-VALUE,".") = "" AND
        NOT cTkn = "5" THEN
        ASSIGN lReturn = TRUE.
    ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,".") = 2 AND ENTRY(2,SELF:SCREEN-VALUE,".") <> "" THEN
        ASSIGN lReturn = TRUE.
    ELSE IF SELF:CURSOR-OFFSET <= LENGTH(SELF:SCREEN-VALUE) AND (cTkn = "," OR cTkn = ".") THEN
        ASSIGN lReturn = TRUE.
    IF lReturn THEN DO:
        BELL.
        RETURN NO-APPLY.
    END.
    IF cTkn = "," THEN DO:
        ASSIGN SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + "."
               SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE) + 1.
        DYNAMIC-FUNCTION('setDataModified':U,
            INPUT TRUE /* LOGICAL */).
        RETURN NO-APPLY.
    END.
    ELSE IF lCharInput THEN DO:
        ASSIGN cTkn = CAPS(cTkn).
        IF SELF:CURSOR-OFFSET > LENGTH(SELF:SCREEN-VALUE) THEN
            ASSIGN SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + cTkn
                   SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE) + 1.
        ELSE IF LENGTH(SELF:SCREEN-VALUE) < 4 AND 
                SELF:CURSOR-OFFSET <= LENGTH(SELF:SCREEN-VALUE) THEN
            ASSIGN iCursor = SELF:CURSOR-OFFSET
                   cRest = SUBSTR(SELF:SCREEN-VALUE,iCursor)
                   SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,iCursor - 1) +
                   cTkn + cRest
                   SELF:CURSOR-OFFSET = iCursor + 1.
        ELSE IF LENGTH(SELF:SCREEN-VALUE) = 4 AND 
            SELF:CURSOR-OFFSET <= LENGTH(SELF:SCREEN-VALUE) THEN DO:
            ASSIGN iCursor = SELF:CURSOR-OFFSET
                   cString = SELF:SCREEN-VALUE.
            OVERLAY(cString, iCursor,1,"CHARACTER") = cTkn.
            ASSIGN SELF:SCREEN-VALUE = cString
                   SELF:CURSOR-OFFSET = iCursor + 1.
        END.
        DYNAMIC-FUNCTION('setDataModified':U,
            INPUT TRUE /* LOGICAL */).
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Storl vTableWin
ON ENTRY OF RowObject.Storl IN FRAME F-Main /* Størrelse */
DO:
    ASSIGN SELF:SCREEN-VALUE = TRIM(SELF:SCREEN-VALUE)
           SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Storl vTableWin
ON TAB OF RowObject.Storl IN FRAME F-Main /* Størrelse */
OR "RETURN":U OF RowObject.Storl
DO:
    IF INPUT RowObject.Storl = "" THEN
    DO:
        MESSAGE "Angi størrelse!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE
        APPLY LASTKEY.
/*     ASSIGN                                           */
/*         cTekst = INPUT RowObject.Storl               */
/*         .                                            */
/*     RUN FixStorl IN h_proclib (INPUT-OUTPUT cTekst). */
/*     DISPLAY                                          */
/*         cTekst @ RowObject.Storl                     */
/*         WITH FRAME F-Main.                           */
/*     IF RowObject.TilStorl:SENSITIVE = TRUE THEN      */
/*       DISPLAY                                        */
/*         cTekst @ RowObject.TilStorl                  */
/*         WITH FRAME F-Main.                           */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Bytt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Bytt vTableWin
ON VALUE-CHANGED OF T-Bytt IN FRAME F-Main /* Bytt størrelse */
DO:
  DO WITH FRAME F-Main:
    IF INPUT T-Bytt = TRUE THEN DO:
        IF  RowObject.ButikkNrFra:SCREEN-VALUE <> RowObject.ButikkNrTil:SCREEN-VALUE THEN DO:
            ASSIGN SELF:CHECKED = FALSE.
            MESSAGE "Fra/Til butikk er ulike"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        ASSIGN RowObject.TilStorl:SENSITIVE = TRUE.
        APPLY "ENTRY":U TO RowObject.TilStorl IN FRAME {&FRAME-NAME}. 
    END.
    ELSE
        ASSIGN RowObject.TilStorl:SENSITIVE = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.TilStorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.TilStorl vTableWin
ON ANY-PRINTABLE OF RowObject.TilStorl IN FRAME F-Main /* TilStorl */
DO:
    DEF VAR cTkn       AS CHAR NO-UNDO.
    DEF VAR cString    AS CHAR NO-UNDO.
    DEF VAR iTst       AS INTE NO-UNDO.
    DEF VAR iCursor    AS INTE NO-UNDO.
    DEF VAR lChar      AS LOGI NO-UNDO.
    DEF VAR lCharInput AS LOGI NO-UNDO.
    DEF VAR lReturn    AS LOGI NO-UNDO.
    DEF VAR cRest      AS CHAR NO-UNDO.
    ASSIGN iTst = INT(SELF:SCREEN-VALUE) NO-ERROR.
           
    ASSIGN lChar      = ERROR-STATUS:ERROR
           cTkn       = KEYFUNCTION(LASTKEY)
           lCharInput = NOT cTkn = "," AND NOT cTkn = "." AND
                        NOT CAN-DO("0,1,2,3,4,5,6,7,8,9",cTkn).
    IF SELF:CURSOR-OFFSET > 4 THEN
        ASSIGN lReturn = TRUE.
    ELSE IF lCharInput AND NOT CAN-DO("A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z",CAPS(cTkn)) THEN
        ASSIGN lReturn = TRUE.
    ELSE IF lChar AND (cTkn = "." OR cTkn = ",") THEN /* om vi har Char-storlek i fältet */
        ASSIGN lReturn = TRUE.
    ELSE IF LENGTH(SELF:SCREEN-VALUE) > 2 AND 
        (cTkn = "." OR cTkn = ",") THEN
        ASSIGN lReturn = TRUE.
    ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,".") = 2 AND
        ((cTkn = "." OR cTkn = ",") OR lCharInput) THEN
        ASSIGN lReturn = TRUE.
    ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,".") = 2 AND ENTRY(2,SELF:SCREEN-VALUE,".") = "" AND
        NOT cTkn = "5" THEN
        ASSIGN lReturn = TRUE.
    ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,".") = 2 AND ENTRY(2,SELF:SCREEN-VALUE,".") <> "" THEN
        ASSIGN lReturn = TRUE.
    ELSE IF SELF:CURSOR-OFFSET <= LENGTH(SELF:SCREEN-VALUE) AND (cTkn = "," OR cTkn = ".") THEN
        ASSIGN lReturn = TRUE.
    IF lReturn THEN DO:
        BELL.
        RETURN NO-APPLY.
    END.
    IF cTkn = "," THEN DO:
        ASSIGN SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + "."
               SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE) + 1.
        DYNAMIC-FUNCTION('setDataModified':U,
            INPUT TRUE /* LOGICAL */).
        RETURN NO-APPLY.
    END.
    ELSE IF lCharInput THEN DO:
        ASSIGN cTkn = CAPS(cTkn).
        IF SELF:CURSOR-OFFSET > LENGTH(SELF:SCREEN-VALUE) THEN
            ASSIGN SELF:SCREEN-VALUE = SELF:SCREEN-VALUE + cTkn
                   SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE) + 1.
        ELSE IF LENGTH(SELF:SCREEN-VALUE) < 4 AND 
                SELF:CURSOR-OFFSET <= LENGTH(SELF:SCREEN-VALUE) THEN
            ASSIGN iCursor = SELF:CURSOR-OFFSET
                   cRest = SUBSTR(SELF:SCREEN-VALUE,iCursor)
                   SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,iCursor - 1) +
                   cTkn + cRest
                   SELF:CURSOR-OFFSET = iCursor + 1.
        ELSE IF LENGTH(SELF:SCREEN-VALUE) = 4 AND 
            SELF:CURSOR-OFFSET <= LENGTH(SELF:SCREEN-VALUE) THEN DO:
            ASSIGN iCursor = SELF:CURSOR-OFFSET
                   cString = SELF:SCREEN-VALUE.
            OVERLAY(cString, iCursor,1,"CHARACTER") = cTkn.
            ASSIGN SELF:SCREEN-VALUE = cString
                   SELF:CURSOR-OFFSET = iCursor + 1.
        END.
        DYNAMIC-FUNCTION('setDataModified':U,
            INPUT TRUE /* LOGICAL */).
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.TilStorl vTableWin
ON ENTRY OF RowObject.TilStorl IN FRAME F-Main /* TilStorl */
DO:
    ASSIGN SELF:SCREEN-VALUE = TRIM(SELF:SCREEN-VALUE)
           SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.TilStorl vTableWin
ON LEAVE OF RowObject.TilStorl IN FRAME F-Main /* TilStorl */
DO:
/*     ASSIGN                                           */
/*         cTekst = INPUT RowObject.TilStorl            */
/*         .                                            */
/*     RUN FixStorl IN h_proclib (INPUT-OUTPUT cTekst). */
/*     DISPLAY                                          */
/*         cTekst @ RowObject.TilStorl                  */
/*         WITH FRAME F-Main.                           */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Vg vTableWin
ON TAB OF RowObject.Vg IN FRAME F-Main /* VgNr */
OR "RETURN":U OF RowObject.Vg
DO:
   RUN ValiderVarGr IN hDataSource
       (INPUT INPUT RowObject.Vg).
   IF RETURN-VALUE = "FALSE" THEN
   DO:
       ASSIGN
         RowObject.fVgBeskr:SCREEN-VALUE = ""
         rowObject.LopNr:SCREEN-VALUE    = ""
         RowObject.fVgBeskr:MODIFIED     = FALSE
         .
       MESSAGE "Ugyldig varegruppenummer!"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
   END.    
   ELSE DO:
     ASSIGN
       RowObject.fVgBeskr:SCREEN-VALUE = RETURN-VALUE
/*        RowObject.LopNr:SCREEN-VALUE    = "" */
       RowObject.fVgBeskr:MODIFIED     = FALSE
       .
     APPLY LASTKEY.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */
  on F2,ALT-S of FRAME {&FRAME-NAME} anywhere 
  do: 
      run d-hsok.w (output rArtikkelNr,"NEI" + CHR(1) + "0"). /* JA = sök mot vpistrekkode */
      if rArtikkelNr = ? then
          RETURN NO-APPLY.
      ELSE DO:
         /* FY FY */
         FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = rArtikkelNr NO-ERROR.
         IF AVAILABLE ArtBas THEN
         DO:
             FIND VarGr OF ArtBas NO-LOCK.
             ASSIGN
                 RowObject.Vg:SCREEN-VALUE       = STRING(ArtBas.Vg)
                 RowObject.LopNr:SCREEN-VALUE    = STRING(ArtBAs.LopNr)
                 RowObject.fVgBeskr:SCREEN-VALUE = VarGr.VgBeskr
                 RowObject.fVgBeskr:MODIFIED     = FALSE
                 .
         END.
         APPLY "ENTRY" TO RowObject.LopNr.
         APPLY "TAB"   TO RowObject.LopNr.
         RETURN NO-APPLY.
      END.
  end.
  on F2,ALT-T of FRAME {&FRAME-NAME} anywhere DO:
      APPLY "CHOOSE" TO B-Scanner.
      RETURN NO-APPLY.
  END.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cVerdier AS CHARACTER  NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hArtBasSok)
      THEN APPLY "close" TO hArtBasSok.

  DO WITH FRAME {&FRAME-NAME}:

      IF CAN-FIND(FIRST OvBuffer WHERE OvBuffer.BuntNr = iBuntNr) THEN
      DO:
          FIND FIRST OvBuffer NO-LOCK WHERE
              OvBuffer.BuntNr = iBuntNr NO-ERROR.
          IF AVAILABLE OvBuffer THEN
              lAddMode = 2.
      END.

      IF lAddMode = 2 THEN 
          ASSIGN cVerdier = RowObject.ButikkNrFra:SCREEN-VALUE + "," +
                            RowObject.fFraButikk:SCREEN-VALUE + "," +
                            RowObject.ButikkNrTil:SCREEN-VALUE + "," +
                            RowObject.fTilButikk:SCREEN-VALUE + "," +
                            /*RowObject.Vg:SCREEN-VALUE +*/ "," +
                            /*RowObject.fVgBeskr:SCREEN-VALUE +*/ "," 
                            /*RowObject.LopNr:SCREEN-VALUE*/.
                            
  END.


  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN enableObject.
  DO WITH FRAME {&FRAME-NAME}:
      PROCESS EVENTS.
      ASSIGN RowObject.ButikkNrFra:SENSITIVE = TRUE
             RowObject.ButikkNrTil:SENSITIVE = TRUE
             RowObject.Vg:SENSITIVE = TRUE
             RowObject.LopNr:SENSITIVE = TRUE
             B-Artikler:SENSITIVE = FALSE.
      IF lAddMode = 2 THEN DO:
          ASSIGN RowObject.ButikkNrFra:SCREEN-VALUE = ENTRY(1,cVerdier)
                 RowObject.fFraButikk:SCREEN-VALUE  = ENTRY(2,cVerdier)
                 RowObject.ButikkNrTil:SCREEN-VALUE = ENTRY(3,cVerdier)
                 RowObject.fTilButikk:SCREEN-VALUE  = ENTRY(4,cVerdier)
                 RowObject.Vg:SCREEN-VALUE          = ENTRY(5,cVerdier)
                 RowObject.fVgBeskr:SCREEN-VALUE    = ENTRY(6,cVerdier)
                 RowObject.LopNr:SCREEN-VALUE       = ENTRY(7,cVerdier)
                 RowObject.fFraButikk:MODIFIED      = FALSE
                 RowObject.fTilButikk:MODIFIED      = FALSE
                 RowObject.fVgBeskr:MODIFIED        = FALSE.
          APPLY "ENTRY" TO RowObject.Storl.
      END.
      ASSIGN lAddMode = 1.
      APPLY "ENTRY" TO RowObject.ButikkNrFra.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddStrProc vTableWin 
PROCEDURE AddStrProc :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ifArtikkelNr AS DEC NO-UNDO.
DEF INPUT PARAMETER icStorl      AS CHAR NO-UNDO.
DEF INPUT PARAMETER ifPlukkAnt   AS DEC NO-UNDO.
DEF INPUT PARAMETER icAction     AS CHAR NO-UNDO.

DEF VAR bReturnFocus AS LOG   NO-UNDO.
DEF VAR cStrList     AS CHAR  NO-UNDO.
DEF VAR bOkStr       AS LOG   NO-UNDO.
DEF VAR iLinjenr     AS INT   NO-UNDO.
DEF VAR cEAN         AS CHAR  NO-UNDO.
DEF VAR iStrKode     AS INT   NO-UNDO.
DEF VAR ocReturn     AS CHAR  NO-UNDO.
DEF VAR iAntall      AS INT   NO-UNDO.
DEF VAR cPkSdlId     AS CHAR  NO-UNDO.
DEF VAR bOk          AS LOG   NO-UNDO.
DEF VAR lvVareKost   AS DEC   NO-UNDO.

DEF BUFFER bufOvBuffer FOR OvBuffer.
DEF BUFFER bufButiker FOR Butiker.

  DO WITH FRAME F-Main:
    run bibl_fixstorl.p (icStorl,?,'',output ocReturn,output bOk).
    icStorl = ocReturn.
    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.Storl = icStorl NO-ERROR.
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = ifArtikkelNr NO-ERROR.
    IF ArtBas.OPris THEN
        RETURN. /* Ingen overføring på åpen pris artikler */

    FIND ArtLag NO-LOCK WHERE
        ArtLag.ArtikkelNr = ifArtikkelNr AND 
        ArtLag.Butik      = i1ButNr AND
        ArtLag.StrKode    = StrKonv.StrKode NO-ERROR.

    IF ArtBas.Lager THEN
    DO:
        FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = ifArtikkelNr AND
            Lager.Butik      = i1butNr NO-ERROR.
        IF AVAILABLE Lager THEN
            ASSIGN
            lVVareKost = Lager.VVAreKost
            lVVAreKost = IF lVVareKost = ? THEN 0 ELSE lVVareKost
            lVVareKost = IF lVVAreKost < 0 THEN 0 ELSE lVVareKost
            .
    END.
    IF lVVareKost = 0 OR ArtBas.Lager = FALSE THEN
    DO:
        FIND bufButiker NO-LOCK WHERE
            bufButiker.Butik = i1ButNr NO-ERROR.
        IF AVAILABLE bufButiker THEN
            FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ifArtikkelNr AND
            ArtPris.ProfilNr   = bufButiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
            FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
        ASSIGN
        lVVareKost = ArtPris.VareKost[1]
        lVVAreKost = IF lVVareKost = ? THEN 0 ELSE lVVareKost
        lVVareKost = IF lVVAreKost < 0 THEN 0 ELSE lVVareKost
        .
    END.
    
    DO TRANSACTION:
        FIND LAST bufOvBuffer WHERE
            bufOvBuffer.BuntNr = iBuntNr USE-INDEX BuntLinjeNr NO-ERROR.
        CREATE OvBuffer.
        ASSIGN
            OvBuffer.BuntNr       = iBuntNr
            OvBuffer.LinjeNr      = IF AVAILABLE bufOvBuffer 
                                      THEN bufOvBuffer.LinjeNr + 1
                                    ELSE 1
            OvBuffer.ArtikkelNr   = ifArtikkelNr
            OvBuffer.Vg           = ArtBas.Vg
            OvBuffer.LopNr        = ArtBas.LopNr
            OvBuffer.ButikkNrFra  = i1ButNr
            OvBuffer.ButikkNrTil  = i2ButNr
            OvBuffer.Antall       = ifPlukkAnt
            OvBuffer.VareKost     = lVVareKost
            OvBuffer.Storl        = icStorl
            OvBuffer.TilStorl     = icStorl
            .
         RELEASE OvBuffer.
         DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
         APPLY 'CTRL-END'.
         /*
         DYNAMIC-FUNCTION('findRow':U IN hDataSource,
                           INPUT pcKeyValues /* CHARACTER */).
         */
    END. /* TRANSACTION */
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

/*   IF NOT RowObject.ButikkNrFra:SENSITIVE IN FRAME {&FRAME-NAME} THEN */
/*       RETURN.                                                        */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN lAddMode = 0.
/*   RUN refreshRow IN hDataSource. */
  IF RowObject.ButikkNrFra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0" THEN
      RUN disableObject.
  ELSE DO:
      PUBLISH 'getDatoOppdatert' (OUTPUT dDatoOppdatert).
      IF dDatoOppdatert = ? THEN
          ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
      ELSE 
          ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

      ASSIGN B-Scanner:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
             T-Bytt:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
             RowObject.ButikkNrFra:SENSITIVE = FALSE
             RowObject.ButikkNrTil:SENSITIVE = FALSE
             RowObject.Vg:SENSITIVE          = FALSE
             RowObject.LopNr:SENSITIVE       = FALSE
             B-SokButikkFra:SENSITIVE        = FALSE
             B-SokButikkTil:SENSITIVE        = FALSE
             B-SokVg:SENSITIVE               = FALSE
             B-SokLopnr:SENSITIVE            = FALSE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN RowObject.TilStorl:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable vTableWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN DisableStorl.

  IF VALID-HANDLE(hDataSource) AND DYNAMIC-FUNCTION('columnValue':U IN hDataSource,
      INPUT "fDatoOppdatert" /* CHARACTER */) <> ? THEN DO:
      RUN disableObject.
  END.
  ELSE IF RowObject.ButikkNrFra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0" THEN
      ASSIGN T-Bytt:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE.
  ELSE IF RowObject.ButikkNrFra:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "0" THEN DO:
      RUN enableObject.
      PUBLISH 'getDatoOppdatert' (OUTPUT dDatoOppdatert).
      IF dDatoOppdatert = ? THEN
          ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
      ELSE 
          ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

      ASSIGN B-Scanner:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
             T-Bytt:SENSITIVE IN FRAME {&FRAME-NAME}    = 
                RowObject.ButikkNrFra:SCREEN-VALUE = RowObject.ButikkNrTil:SCREEN-VALUE
             RowObject.ButikkNrFra:SENSITIVE = FALSE
             RowObject.ButikkNrTil:SENSITIVE = FALSE
             RowObject.Vg:SENSITIVE          = FALSE
             RowObject.LopNr:SENSITIVE       = FALSE
             B-SokButikkFra:SENSITIVE        = FALSE
             B-SokButikkTil:SENSITIVE        = FALSE
             B-SokVg:SENSITIVE               = FALSE
             B-SokLopnr:SENSITIVE            = FALSE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF DYNAMIC-FUNCTION('getRecordState':U) = "NoRecordAvailable" THEN DO:
      DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
       RUN disableObject.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject vTableWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hScanner) THEN DO:
      APPLY "CLOSE" TO hScanner.
  END.
  IF VALID-HANDLE(hArtBasSok)
      THEN APPLY "close" TO hArtBasSok.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableObject vTableWin 
PROCEDURE disableObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  IF DYNAMIC-FUNCTION('getRecordState':U) = "NoRecordAvailable" THEN
  DO:
      DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
          INPUT "Update,Copy,Delete" /* CHARACTER */).
      PUBLISH 'getDatoOppdatert' (OUTPUT dDatoOppdatert).
      IF dDatoOppdatert = ? THEN
          ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
      ELSE 
          ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

  END.
  ELSE
      DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
          INPUT "Add,Update,Copy,Delete" /* CHARACTER */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableStorl vTableWin 
PROCEDURE DisableStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN 
      RowObject.TilStorl:SENSITIVE IN FRAME F-Main = FALSE
      T-Bytt = FALSE
      .
  DISPLAY
      T-Bytt
      WITH FRAME F-Main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  IF VALID-HANDLE(hDataSource) AND DYNAMIC-FUNCTION('columnValue':U IN hDataSource,
      INPUT "fDatoOppdatert" /* CHARACTER */) <> ? THEN
      RETURN.

    RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject vTableWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  PUBLISH "SendWinH" (OUTPUT hContainer).
  {syspara.i 11 6 2 cScannerPara}
  lSeriellScanner = cScannerPara = "1".

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN hDataSource = DYNAMIC-FUNCTION('getDataSource':U).
         hNavigation = DYNAMIC-FUNCTION('getTableIOSource':U).

  PUBLISH 'getBuntNr' (OUTPUT iBuntNr).
  PUBLISH 'getDatoOppdatert' (OUTPUT dDatoOppdatert).

/*  rowObject.Antall:FORMAT IN FRAME {&FRAME-NAME} = ">>>>>9". */
  DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
  rowobject.antall:FORMAT IN FRAME {&FRAME-NAME} = "->>,>>>".

IF lSeriellScanner AND NOT VALID-HANDLE(hScanner) THEN 
DO:
    RUN symbolscanner.w PERSISTENT SET hScanner NO-ERROR.
    IF VALID-HANDLE(hScanner) THEN DO:
        RUN START IN hScanner.
        IF RETURN-VALUE <> "0" THEN DO:
            APPLY "CLOSE" TO hScanner.
            MESSAGE "FEL VID KOPPLING TILL SCANNER" SKIP
                "ANVÄND TANGENTBORDSSCANNER"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        /*
        RUN Claimed IN hScanner (OUTPUT lClaimed).
        IF NOT lClaimed THEN DO:
            MESSAGE "Fel vid start av scanner" SKIP
                    "Undersök felet eller använd keyboardscanner"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            DELETE OBJECT hScanner.
        END.
        */
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendX_Y vTableWin 
PROCEDURE SendX_Y :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER iXcenter AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iYcenter AS INTEGER     NO-UNDO.
    IF VALID-HANDLE(hContainer) THEN DO:
        iXcenter = hContainer:X + hContainer:WIDTH-PIXELS / 2.
        iYcenter = hContainer:Y + hContainer:HEIGHT-PIXELS / 2.
    END.
    ELSE ASSIGN iXcenter = 0
                iYcenter = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFokus vTableWin 
PROCEDURE SetFokus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "ENTRY":U TO RowObject.ButikkNrFra IN FRAME F-Main.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetskjermVerdier vTableWin 
PROCEDURE SetskjermVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Strekkode:  VVVVLLLLXSSS
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER pcStrekkode AS CHAR NO-UNDO.

 DEF VAR piVg       AS INT  NO-UNDO.
 DEF VAR piLopNr    AS INT  NO-UNDO.
 DEF VAR pcStorl    AS CHAR NO-UNDO.
 DEF VAR pcTilStorl AS CHAR NO-UNDO.
 DEF VAR piAntall   AS INT  NO-UNDO.
 DEF VAR piLoop     AS INT  NO-UNDO.
 DEF VAR deTst      AS DECIMAL    NO-UNDO.
 DEF VAR lKunBestNrSok AS LOGICAL    NO-UNDO.
 DEF VAR cStrekKodeSok AS CHARACTER  NO-UNDO.

 ASSIGN deTst = DECI(pcStrekKode) NO-ERROR.
 
 IF ERROR-STATUS:ERROR OR LENGTH(pcStrekKode) > 13 THEN
     ASSIGN lKunBestNrSok = TRUE.
 ELSE DO:
     ASSIGN cStrekKodeSok = FILL("0",13 - LENGTH(pcStrekKode)) + pcStrekKode.
 END.
 IF lKunBestNrSok = FALSE THEN DO:
     FIND StrekKode WHERE StrekKode.Kode = cStrekKodeSok NO-LOCK NO-ERROR.
 END.
 IF NOT AVAIL StrekKode THEN DO:
     FIND StrekKode WHERE StrekKode.Bestillingsnummer = pcStrekkode NO-LOCK NO-ERROR.
 END.

 IF NOT AVAIL StrekKode THEN DO:
     MESSAGE "Finner ikke Artikkelen"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN "AVBRYT".
 END.
 FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.
 IF NOT AVAIL ArtBas THEN DO:
     MESSAGE "Finner ikke Artikkelen"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN "AVBRYT".
 END.
 FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
 IF NOT AVAIL StrKonv THEN DO:
     MESSAGE "Feil størrelse på strekkoden"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN "AVBRYT".
 END.
 ASSIGN piVg    = ArtBas.Vg
        piLopNr = ArtBas.LopNr
        pcStorl = StrKonv.Storl
        piAntall = 1.
 
 DO WITH FRAME F-Main:
   ASSIGN
     RowObject.ButikkNrFra:SCREEN-VALUE = STRING(iButikkNrFra)
     RowObject.ButikkNrTil:SCREEN-VALUE = STRING(iButikkNrTil)
/*      RowObject.fTilButikk:SCREEN-VALUE  = cTilButikk */
/*      RowObject.fFraButikk:SCREEN-VALUE  = cFraButikk */
     RowObject.fTilButikk:MODIFIED      = FALSE
     RowObject.fFraButikk:MODIFIED      = FALSE
     pcTilStorl                         = pcStorl
     RowObject.Vg:SCREEN-VALUE          = STRING(piVg)
     RowObject.LopNr:SCREEN-VALUE       = STRING(piLopNr)
     RowObject.Storl:SCREEN-VALUE       = pcStorl
     RowObject.TilStorl:SCREEN-VALUE    = pcTilStorl
/*  RowObject.Storl:MODIFIED      = TRUE    */
/*  RowObject.TilStorl:MODIFIED      = TRUE */
     RowObject.Antall:SCREEN-VALUE      = STRING(piAntall)
     RowObject.Merknad:SCREEN-VALUE     = 
       RowObject.Merknad:SCREEN-VALUE + pcStrekkode
     .
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fVarekost AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iButFra AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iButTil AS INTEGER    NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN iButFra = INPUT RowObject.ButikkNrFra
             iButTil = INPUT RowObject.ButikkNrTil.
      RUN ValiderButikk IN hDataSource 
          (INPUT INPUT RowObject.ButikkNrFra).
      IF RETURN-VALUE = "FALSE" THEN
          RETURN "Ugyldig butikknummer (Kontroll ved lagring).".
      IF DECI(RowObject.VareKost:SCREEN-VALUE) = 0 THEN DO:
          RUN GetVarekost IN hDataSource (INPUT INPUT RowObject.Vg,
                                          INPUT INPUT RowObject.LopNr,
                                          INPUT INPUT RowObject.ButikkNrFra,
                                          OUTPUT fVarekost).
          IF fVarekost = 0 THEN DO:
              MESSAGE "Inget lager finnes registrert for butikk " + RowObject.ButikkNrFra:SCREEN-VALUE
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RowObject.Merknad:SCREEN-VALUE = "".
              RETURN "AVBRYT".
          END.
          ASSIGN RowObject.VareKost:SCREEN-VALUE = STRING(fVareKost)
                 RowObject.VareKost:MODIFIED = TRUE.
      END.
  END.

  RUN SUPER.
  IF RETURN-VALUE = "ADM-ERROR" THEN
      RETURN.
  IF RETURN-VALUE = "" THEN DO:
      /*RUN disableObject.*/
      IF lAddMode > 0 THEN RUN addRecord.
/*       IF RowObject.ButikkNrFra:SENSITIVE IN FRAME {&FRAME-NAME} THEN */
/*          RUN addRecord.                                              */
/*       ELSE                                                           */
/*       RUN refreshRow IN hDataSource.                                 */
/*       ELSE                                                        */
/*           IF VALID-HANDLE(hNavigation) THEN                       */
/*               DYNAMIC-FUNCTION('disableActions':U IN hNavigation, */
/*                   INPUT "Copy" /* CHARACTER */).                  */
  END.
  ELSE DO:
/*       DYNAMIC-FUNCTION('openQuery':U IN hDataSource). */
      RUN cancelRecord.
      /*RUN disableObject.*/
      IF DialogBoksAktiv() OR lAddMode > 0 THEN
          RUN addRecord.
  END.
  IF NOT DialogBoksAktiv() and not lWinScan THEN DO:
      ASSIGN RowObject.ButikkNrFra:SCREEN-VALUE = STRING(iButFra)
             RowObject.ButikkNrTil:SCREEN-VALUE = STRING(iButTil).
/*              RowObject.fTilButikk:SCREEN-VALUE  = STRING(iButTil)  */
/*              RowObject.fFraButikk:SCREEN-VALUE  = STRING(iButFra). */
      APPLY "TAB" TO RowObject.ButikkNrTil.
      APPLY "TAB" TO RowObject.Merknad.
  END.

  PUBLISH 'getDatoOppdatert' (OUTPUT dDatoOppdatert).
  IF dDatoOppdatert = ? THEN
      ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
  ELSE 
      ASSIGN B-Artikler:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

  /* Code placed here will execute AFTER standard behavior.    */
  
/*   IF NOT DYNAMIC-FUNCTION('getDataModified':U) THEN DO: */
/*       RUN refreshRow IN hDataSource.                    */
/*       IF lAddMode = 2 THEN                              */
/*           RUN addRecord.                                */
/*   END.                                                  */
/*   ELSE                                                  */
/*       ASSIGN lAddMode = 1.                              */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisButiker vTableWin 
PROCEDURE VisButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN RowObject.ButikkNrFra:SCREEN-VALUE = STRING(iButikkNrFra)
             RowObject.ButikkNrTil:SCREEN-VALUE = STRING(iButikkNrTil)
             RowObject.fTilButikk:SCREEN-VALUE  = cTilButikk
             RowObject.fFraButikk:SCREEN-VALUE  = cFraButikk.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

