&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

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
DEFINE VARIABLE cAlle AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for FRAME fMain                                          */
&Scoped-define QUERY-STRING-fMain FOR EACH ArtBas NO-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH ArtBas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-fMain ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 FI-LevNr B-LevNrBlank CB-Koblet ~
FI-Beskr BUTTON-SokVg B-BeskrBlank CB-Hg FI-Bongtekst B-Bongtekst B-VgBlank ~
FI-LevKod B-LevKodBlank CB-Sasong FI-ImpDato1 FI-ImpDato2 B-ImportBlank ~
CB-Annonse FI-Endret1 FI-Endret2 B-ImportBlank-2 FI-Utvidetsok B-UtvidetSok ~
B-SokLevNr 
&Scoped-Define DISPLAYED-OBJECTS FI-LevNr CB-Koblet FI-Beskr CB-Hg ~
FI-Bongtekst FI-Vg FI-LevKod CB-Sasong FI-ImpDato1 FI-ImpDato2 CB-Annonse ~
FI-Endret1 FI-Endret2 FI-Utvidetsok FI-Fltertekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-BeskrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Bongtekst 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-ImportBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-ImportBlank-2 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-LevKodBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-LevNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SokLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON B-UtvidetSok 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON BUTTON-SokVg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Annonse AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Annonse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Hg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Koblet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Koblet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sasong AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ses&ong" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "x(20)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bongtekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Endret1 AS DATE FORMAT "99/99/99":U 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Endret2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Fltertekst AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ImpDato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Importert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ImpDato2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Levartnr" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Utvidetsok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utvidet søk" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE FI-Vg AS CHARACTER FORMAT "X(8)" 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 7.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-LevNr AT ROW 1.52 COL 12 COLON-ALIGNED
     B-LevNrBlank AT ROW 1.52 COL 33
     CB-Koblet AT ROW 1.52 COL 65 COLON-ALIGNED
     FI-Beskr AT ROW 2.52 COL 12 COLON-ALIGNED HELP
          "Artikkelens varetekst"
     BUTTON-SokVg AT ROW 3.57 COL 74.8
     B-BeskrBlank AT ROW 2.52 COL 42.8
     CB-Hg AT ROW 2.52 COL 65 COLON-ALIGNED
     FI-Bongtekst AT ROW 3.52 COL 12 COLON-ALIGNED
     B-Bongtekst AT ROW 3.52 COL 42.8
     FI-Vg AT ROW 3.52 COL 65 COLON-ALIGNED HELP
          "Varegruppe"
     B-VgBlank AT ROW 3.57 COL 79.4
     FI-LevKod AT ROW 4.52 COL 12 COLON-ALIGNED
     B-LevKodBlank AT ROW 4.52 COL 42.8
     CB-Sasong AT ROW 4.52 COL 65 COLON-ALIGNED
     FI-ImpDato1 AT ROW 5.52 COL 12 COLON-ALIGNED
     FI-ImpDato2 AT ROW 5.52 COL 26.4 COLON-ALIGNED NO-LABEL
     B-ImportBlank AT ROW 5.52 COL 42.8
     CB-Annonse AT ROW 5.52 COL 65 COLON-ALIGNED
     FI-Endret1 AT ROW 6.48 COL 12 COLON-ALIGNED
     FI-Endret2 AT ROW 6.48 COL 26.4 COLON-ALIGNED NO-LABEL
     B-ImportBlank-2 AT ROW 6.52 COL 42.8
     FI-Utvidetsok AT ROW 7.57 COL 12 COLON-ALIGNED
     B-UtvidetSok AT ROW 7.57 COL 67.6
     B-SokLevNr AT ROW 1.52 COL 28 NO-TAB-STOP 
     FI-Fltertekst AT ROW 1 COL 1 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.2 BY 7.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 7.71
         WIDTH              = 97.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Fltertekst IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FI-Fltertekst:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN FI-Vg IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FI-Vg:READ-ONLY IN FRAME fMain        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "skotex.ArtBas"
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-BeskrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BeskrBlank fFrameWin
ON CHOOSE OF B-BeskrBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-Beskr:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Beskr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Bongtekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bongtekst fFrameWin
ON CHOOSE OF B-Bongtekst IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-BongTekst:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Bongtekst.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ImportBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ImportBlank fFrameWin
ON CHOOSE OF B-ImportBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN 
    FI-ImpDato1:SCREEN-VALUE = ""
    FI-ImpDato2:SCREEN-VALUE = ""
    .
  APPLY "TAB" TO FI-ImpDato1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ImportBlank-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ImportBlank-2 fFrameWin
ON CHOOSE OF B-ImportBlank-2 IN FRAME fMain /* Blank */
DO:
  ASSIGN 
    FI-Endret1:SCREEN-VALUE = ""
    FI-Endret2:SCREEN-VALUE = ""
    .
  APPLY "TAB" TO FI-Endret1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevKodBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevKodBlank fFrameWin
ON CHOOSE OF B-LevKodBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-LevKod:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-LevKod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank fFrameWin
ON CHOOSE OF B-LevNrBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-LevNr:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLevNr fFrameWin
ON CHOOSE OF B-SokLevNr IN FRAME fMain /* ... */
OR F10 OF FI-LevNr
DO:
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gLevbas.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    FI-LevNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) = 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        FI-LevNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "TAB":U TO FI-LevNr.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-UtvidetSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-UtvidetSok fFrameWin
ON CHOOSE OF B-UtvidetSok IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-Utvidetsok:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Utvidetsok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgBlank fFrameWin
ON CHOOSE OF B-VgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Vg:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Vg:SCREEN-VALUE = cAlle
               FI-Vg:TOOLTIP      = "".
        RUN StartSok.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg fFrameWin
ON CHOOSE OF BUTTON-SokVg IN FRAME fMain /* ... */
OR F10 OF FI-Vg
DO:
    DEFINE VARIABLE iVg AS INTEGER    NO-UNDO.

    RUN gvargr.w (INPUT-OUTPUT iVg).
    IF RETURN-VALUE <> "AVBRYT" THEN
    DO:
        ASSIGN
            FI-Vg:SCREEN-VALUE = STRING(iVg)
            FI-Vg:TOOLTIP      = RETURN-VALUE
            .
        RUN StartSok.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Annonse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Annonse fFrameWin
ON VALUE-CHANGED OF CB-Annonse IN FRAME fMain /* Annonse */
OR "TAB":U OF CB-Annonse DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Hg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Hg fFrameWin
ON VALUE-CHANGED OF CB-Hg IN FRAME fMain /* Hovedgruppe */
OR "TAB":U OF CB-Hg DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Koblet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Koblet fFrameWin
ON VALUE-CHANGED OF CB-Koblet IN FRAME fMain /* Koblet */
OR "TAB":U OF CB-Koblet DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Sasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Sasong fFrameWin
ON VALUE-CHANGED OF CB-Sasong IN FRAME fMain /* Sesong */
DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr fFrameWin
ON RETURN OF FI-Beskr IN FRAME fMain /* Varetekst */
OR "TAB":U OF FI-Beskr DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Bongtekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Bongtekst fFrameWin
ON RETURN OF FI-Bongtekst IN FRAME fMain /* Bongtekst */
OR "TAB":U OF FI-BongTekst DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Endret1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret1 fFrameWin
ON DELETE-CHARACTER OF FI-Endret1 IN FRAME fMain /* Endret */
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret1 fFrameWin
ON RETURN OF FI-Endret1 IN FRAME fMain /* Endret */
OR "TAB":U OF FI-Endret1 DO:
    /*RUN StartSok.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Endret2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret2 fFrameWin
ON DELETE-CHARACTER OF FI-Endret2 IN FRAME fMain
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Endret2 fFrameWin
ON RETURN OF FI-Endret2 IN FRAME fMain
OR "TAB":U OF FI-Endret2 DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ImpDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato1 fFrameWin
ON DELETE-CHARACTER OF FI-ImpDato1 IN FRAME fMain /* Importert */
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato1 fFrameWin
ON RETURN OF FI-ImpDato1 IN FRAME fMain /* Importert */
OR "TAB":U OF FI-ImpDato1 DO:
    /*RUN StartSok.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ImpDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato2 fFrameWin
ON DELETE-CHARACTER OF FI-ImpDato2 IN FRAME fMain
DO:
  ASSIGN
    SELF:SCREEN-VALUE = ""
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ImpDato2 fFrameWin
ON RETURN OF FI-ImpDato2 IN FRAME fMain
OR "TAB":U OF FI-ImpDato2 DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod fFrameWin
ON RETURN OF FI-LevKod IN FRAME fMain /* Levartnr */
OR "TAB":U OF FI-LevKod DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr fFrameWin
ON RETURN OF FI-LevNr IN FRAME fMain /* Levnr */
OR "TAB":U OF FI-LevNr DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Utvidetsok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Utvidetsok fFrameWin
ON RETURN OF FI-Utvidetsok IN FRAME fMain /* Utvidet søk */
OR "TAB":U OF FI-Utvidetsok DO:
    RUN StartSok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FI-LevNr CB-Koblet FI-Beskr CB-Hg FI-Bongtekst FI-Vg FI-LevKod 
          CB-Sasong FI-ImpDato1 FI-ImpDato2 CB-Annonse FI-Endret1 FI-Endret2 
          FI-Utvidetsok FI-Fltertekst 
      WITH FRAME fMain.
  ENABLE RECT-1 FI-LevNr B-LevNrBlank CB-Koblet FI-Beskr BUTTON-SokVg 
         B-BeskrBlank CB-Hg FI-Bongtekst B-Bongtekst B-VgBlank FI-LevKod 
         B-LevKodBlank CB-Sasong FI-ImpDato1 FI-ImpDato2 B-ImportBlank 
         CB-Annonse FI-Endret1 FI-Endret2 B-ImportBlank-2 FI-Utvidetsok 
         B-UtvidetSok B-SokLevNr 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCombos fFrameWin 
PROCEDURE initCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
        ASSIGN cListItemPairs = cAlle + ",".
      FOR EACH HuvGr NO-LOCK:
          ASSIGN cListItemPairs = cListItemPairs + "," + STRING(HuvGr.Hg,"9999") + "   " +
                            REPLACE(HuvGr.HgBeskr,","," ") + "," + STRING(HuvGr.Hg).
      END.
      ASSIGN CB-Hg:LIST-ITEM-PAIRS = cListItemPairs
             CB-Hg:screen-value = " ".

      ASSIGN FI-Vg:SCREEN-VALUE = cAlle.
      
      ASSIGN cListItemPairs = cAlle + ",".
      FOR EACH Sasong NO-LOCK:
          ASSIGN cListItemPairs = cListItemPairs + "," + STRING(SaSong.SaSong,"zzz9") + "   " +
                                                 REPLACE(Sasong.SasBeskr,","," ") + "," + STRING(SaSong.SaSong).
      END.
      ASSIGN CB-Sasong:LIST-ITEM-PAIRS = cListItemPairs
             CB-Sasong:screen-value = " ".

      ASSIGN CB-Koblet:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no" /* ,Pakker,yes,Ikke pakker,no" */
             CB-Koblet:screen-value = " ".

      ASSIGN CB-Annonse:LIST-ITEM-PAIRS = cAlle + ",0,Ja,1,Nei,2" /* */
             CB-Annonse:screen-value = " ".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBeskr fFrameWin 
PROCEDURE SetBeskr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cUtvidetSok AS CHAR NO-UNDO.

  DO WITH FRAME fMain:
      ASSIGN
          FI-Beskr:SCREEN-VALUE = cUtvidetSok
          .
     APPLY "TAB" TO FI-Beskr.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFilter fFrameWin 
PROCEDURE SetFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    CASE ENTRY(1,pcTekst,CHR(1)):
      WHEN "BESKR" THEN
      DO:
        FI-Beskr:SCREEN-VALUE = ENTRY(2,pcTekst,CHR(1)).
        APPLY "TAB" TO FI-Beskr.
      END.
      WHEN "BONGTEKST" THEN
      DO:
        FI-BongTekst:SCREEN-VALUE = ENTRY(2,pcTekst,CHR(1)).
        APPLY "TAB" TO FI-Bongtekst.
      END.
      WHEN "LEVKOD" THEN
      DO:
        FI-LevKod:SCREEN-VALUE = ENTRY(2,pcTekst,CHR(1)).
        APPLY "TAB" TO FI-LevKod.
      END.
      WHEN "UtvidetSok" THEN
      DO:
        FI-UtvidetSok:SCREEN-VALUE = ENTRY(2,pcTekst,CHR(1)).
        APPLY "TAB" TO FI-UtvidetSok.
      END.
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLevKod fFrameWin 
PROCEDURE SetLevKod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cUtvidetSok AS CHAR NO-UNDO.

  DO WITH FRAME fMain:
      ASSIGN
          FI-LevKod:SCREEN-VALUE = cUtvidetSok
          .
     APPLY "TAB" TO FI-LevKod.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetUtvidetsok fFrameWin 
PROCEDURE SetUtvidetsok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cUtvidetSok AS CHAR NO-UNDO.

  DO WITH FRAME fMain:
      ASSIGN
          FI-Utvidetsok:SCREEN-VALUE = cUtvidetSok
          .
     APPLY "TAB" TO FI-Utvidetsok.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok fFrameWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcFields   AS CHAR NO-UNDO.
  DEF VAR pcValues   AS CHAR NO-UNDO.
  DEF VAR pcSort     AS CHAR NO-UNDO.
  DEF VAR pcOperator AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.
  DEF VAR iCount      AS INTE NO-UNDO.
  DEF VAR iTst        AS INTE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN pcFeltListe = "LevNr,Hg,Vg,Sasong,LevKod,Beskr,BongTekst,FinnesLokalt,RegistrertDato,EDato,AnonseArtikkel,Utvidetsok".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "FinnesLokalt" THEN DO:
                IF CB-Koblet:SCREEN-VALUE <> " " THEN
                ASSIGN
                    pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "FinnesLokalt"
                    pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                          (IF CB-Koblet:SCREEN-VALUE = "yes" THEN "yes"
                                           ELSE "no")
                    pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "="
                    .
            END.
            WHEN "LevNr" THEN DO:
                IF FI-LevNr:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LevNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-LevNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Hg" THEN DO:
                IF CB-Hg:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Hg"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Hg:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Vg" THEN DO:
                ASSIGN iTst = INT(FI-Vg:SCREEN-VALUE) NO-ERROR.

                IF NOT ERROR-STATUS:ERROR THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Vg"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-Vg:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Sasong" THEN DO:
                IF CB-Sasong:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Sasong"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Sasong:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Levkod" THEN DO:
                IF FI-Levkod:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Levkod:SCREEN-VALUE = TRIM(FI-Levkod:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Levkod"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                    IF NUM-ENTRIES(FI-Levkod:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Levkod:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Levkod:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-Levkod:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "Beskr" THEN DO:
                IF FI-Beskr:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Beskr:SCREEN-VALUE = TRIM(FI-Beskr:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Beskr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                    IF NUM-ENTRIES(FI-Beskr:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Beskr:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Beskr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-Beskr:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "Utvidetsok" THEN DO:
                IF FI-Utvidetsok:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Utvidetsok:SCREEN-VALUE = TRIM(FI-Utvidetsok:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Utvidetsok"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + FI-Utvidetsok:SCREEN-VALUE
                pcOperator = pcOperator  + (IF pcOperator = "" THEN "" ELSE ",") +
                               "CONTAINS".
            END.
            WHEN "Bongtekst" THEN DO:
                IF FI-Bongtekst:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Bongtekst:SCREEN-VALUE = TRIM(FI-Bongtekst:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BongTekst"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                    IF NUM-ENTRIES(FI-Bongtekst:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Bongtekst:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Bongtekst:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-BongTekst:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "RegistrertDato" THEN DO:
                IF FI-ImpDato1:SCREEN-VALUE <> "" THEN
                ASSIGN
                  pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "RegistrertDato"
                  pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + FI-ImpDato1:SCREEN-VALUE
                  pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">="
                  .
                IF FI-ImpDato2:SCREEN-VALUE <> "" THEN
                ASSIGN
                  pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "RegistrertDato"
                  pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + FI-ImpDato2:SCREEN-VALUE
                  pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<="
                  .
            END.
          WHEN "EDato" THEN DO:
              IF FI-Endret1:SCREEN-VALUE <> "" THEN
              ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + FI-Endret1:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">="
                .
              IF FI-Endret2:SCREEN-VALUE <> "" THEN
              ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + FI-Endret2:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<="
                .
          END.
          WHEN "AnonseArtikkel" THEN DO:
              IF CB-Annonse:SCREEN-VALUE <> "0" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AnonseArtikkel"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                    (IF CB-Annonse:SCREEN-VALUE = "1" THEN "yes"
                                     ELSE "no")
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "="
              .
          END.
        END CASE.
    END.
  END.

/*   MESSAGE "pcFields:"     pcFields SKIP   */
/*           "pcValues:"     pcValues SKIP   */
/*           "pcSort:"       pcSort SKIP     */
/*           "pcOperator:"   pcOperator SKIP */
/*           "pcFeltListe:"  pcFeltListe     */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.  */

  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).

  PUBLISH "SetEntryVPIArtBas".
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

