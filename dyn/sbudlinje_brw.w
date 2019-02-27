&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

update_varebok_from_artbas.p
vareboklinjeDvelgfelter.w           
vareboklinje_refresh_all.p           
           
           
           
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowse-2         AS HANDLE NO-UNDO.
DEF VAR hFilterWindow     AS HANDLE NO-UNDO.

DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.
DEF VAR hDetail           AS HANDLE NO-UNDO.
DEF VAR cRowidList        AS CHAR   NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.
DEF VAR hFlatView              AS HANDLE NO-UNDO.
DEF VAR ocValue           AS CHAR NO-UNDO.

DEF VAR hfiSalgbudsjett   AS HANDLE NO-UNDO.
DEF VAR hfiSalgbudsjett-2 AS HANDLE NO-UNDO.
DEF VAR hfiProsent        AS HANDLE NO-UNDO.
DEF VAR hfiDbBudsjett     AS HANDLE NO-UNDO.
DEF VAR hfiDbBudsjett-2   AS HANDLE NO-UNDO.
DEF VAR hfiDbSnitt        AS HANDLE NO-UNDO.
DEF VAR hfiDbProsent      AS HANDLE NO-UNDO.
DEF VAR hfi2Prosent       AS HANDLE NO-UNDO.
DEF VAR hfi2DbProsent     AS HANDLE NO-UNDO.
DEF VAR hfcMerknad        AS HANDLE NO-UNDO.

DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hBuffer-2         AS HANDLE NO-UNDO.

/*Valg av poster*/
DEF VAR cDato        AS CHAR NO-UNDO.
DEF VAR iReturn      AS INT  NO-UNDO.
DEF VAR cLand        AS CHAR NO-UNDO.
DEF VAR cHelligDager AS CHAR NO-UNDO.
DEF VAR iAar         AS INT NO-UNDO.
DEF VAR cParent      AS CHAR NO-UNDO.

/*Browse stuff*/
DEF VAR hbcDbSnitt    AS HANDLE NO-UNDO.
DEF VAR hbfDbSnitt    AS HANDLE NO-UNDO.
DEF VAR hbcSalgBudsjett AS HANDLE NO-UNDO.
DEF VAR hbfSalgBudsjett AS HANDLE NO-UNDO.
DEF VAR hbcSalgBudsjett-2 AS HANDLE NO-UNDO.
DEF VAR hbfSalgBudsjett-2 AS HANDLE NO-UNDO.
DEF VAR hbcProsent    AS HANDLE NO-UNDO.
DEF VAR hbfProsent    AS HANDLE NO-UNDO.
DEF VAR hbcDbBudsjett AS HANDLE NO-UNDO.
DEF VAR hbfDbBudsjett AS HANDLE NO-UNDO.
DEF VAR hbcDbBudsjett-2 AS HANDLE NO-UNDO.
DEF VAR hbfDbBudsjett-2 AS HANDLE NO-UNDO.
DEF VAR hbcDbProsent  AS HANDLE NO-UNDO.
DEF VAR hbfDbProsent  AS HANDLE NO-UNDO.
DEF VAR hbc2Prosent   AS HANDLE NO-UNDO.
DEF VAR hbf2Prosent   AS HANDLE NO-UNDO.
DEF VAR hbc2DbProsent AS HANDLE NO-UNDO.
DEF VAR hbf2DbProsent AS HANDLE NO-UNDO.
DEF VAR hbclinjeSum   AS HANDLE NO-UNDO.
DEF VAR hbflinjeSum   AS HANDLE NO-UNDO.
DEF VAR hbctmpdMDag   AS HANDLE NO-UNDO.
DEF VAR hbftmpdMDag   AS HANDLE NO-UNDO.

DEF VAR hbctmpcUDag   AS HANDLE NO-UNDO.
DEF VAR hbftmpcUDag   AS HANDLE NO-UNDO.

DEF VAR bFlag  AS LOG NO-UNDO.
DEF VAR bFlag2 AS LOG NO-UNDO.

DEF VAR iGreen    AS INT INIT 10 NO-UNDO.
DEF VAR iRed      AS INT INIT 12 NO-UNDO.
DEF VAR iYellow   AS INT INIT 14 NO-UNDO.
DEF VAR iLtYellow AS INT INIT 16 NO-UNDO.
DEF VAR iLtRed    AS INT INIT 17 NO-UNDO.
DEF VAR iLtGreen  AS INT INIT 18 NO-UNDO.

/* DEF VAR iFontWingdings    AS INT    NO-UNDO.                                                    */
/* iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR. */

{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectBrowse-2 ~
B-OppdBudTot SalgbudsjettTotal FI-Bud ProsentTotal DbBudsjettTotal ~
B-OppdbudMndDag DbSnitt B-OppdbudMnd SalgBudsjettTotal-2 ProsentTotal-2 ~
DbBudsjettTotal-2 DbSnitt-2 
&Scoped-Define DISPLAYED-OBJECTS SalgbudsjettTotal FI-Bud ProsentTotal ~
DbBudsjettTotal DbSnitt SalgBudsjettTotal-2 ProsentTotal-2 ~
DbBudsjettTotal-2 DbSnitt-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-OppdbudMnd 
     LABEL "Oppdater måned fra dag" 
     SIZE 31 BY 1.14 TOOLTIP "Oppdaterer månedens totaler med total fra daglinjene."
     FONT 6.

DEFINE BUTTON B-OppdbudMndDag 
     LABEL "Oppdater dag fra måned" 
     SIZE 31 BY 1.14 TOOLTIP "Oppdaterer månedens totaler med total fra daglinjene."
     FONT 6.

DEFINE BUTTON B-OppdBudTot 
     LABEL "Oppdater totalt fra måned" 
     SIZE 32.2 BY 1.14 TOOLTIP "Oppdaterer budsjettets totaler med summer fra månedene."
     FONT 6.

DEFINE VARIABLE DbBudsjettTotal AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 21 BY 1 TOOLTIP "Sum db kr alle måneder"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE DbBudsjettTotal-2 AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13 BY 1 TOOLTIP "Sum db kr. alle dager i måneden"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE DbSnitt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 9.6 BY 1 TOOLTIP "Snitt db% måned"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE DbSnitt-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13.8 BY 1 TOOLTIP "Snitt db% dag"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Bud AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 105.8 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE ProsentTotal AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 10 BY 1 TOOLTIP "Sum oms%"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE ProsentTotal-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13.8 BY 1 TOOLTIP "Sum oms%."
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE SalgbudsjettTotal AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 19.8 BY 1 TOOLTIP "Sum omsetning"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE SalgBudsjettTotal-2 AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15.2 BY 1 TOOLTIP "Sum omsetning alle dager"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107.6 BY 20.48.

DEFINE RECTANGLE rectBrowse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.6 BY 20.48.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-OppdBudTot AT ROW 2.33 COL 1.8 WIDGET-ID 36
     SalgbudsjettTotal AT ROW 24.24 COL 17 COLON-ALIGNED NO-LABEL
     FI-Bud AT ROW 1.19 COL 30.2 COLON-ALIGNED NO-LABEL
     ProsentTotal AT ROW 24.24 COL 36.6 COLON-ALIGNED NO-LABEL
     DbBudsjettTotal AT ROW 24.24 COL 57 COLON-ALIGNED NO-LABEL
     B-OppdbudMndDag AT ROW 2.33 COL 77.4 WIDGET-ID 40
     DbSnitt AT ROW 24.24 COL 77.6 COLON-ALIGNED NO-LABEL
     B-OppdbudMnd AT ROW 2.33 COL 109.6 WIDGET-ID 38
     SalgBudsjettTotal-2 AT ROW 24.24 COL 126.2 COLON-ALIGNED NO-LABEL
     ProsentTotal-2 AT ROW 24.24 COL 142 COLON-ALIGNED NO-LABEL
     DbBudsjettTotal-2 AT ROW 24.24 COL 156.2 COLON-ALIGNED NO-LABEL
     DbSnitt-2 AT ROW 24.24 COL 169.4 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 3.62 COL 1.4
     rectToolBar AT ROW 1.24 COL 2
     rectBrowse-2 AT ROW 3.62 COL 109.2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 184.8 BY 24.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SBudLinje"
         HEIGHT             = 24.24
         WIDTH              = 184.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       DbBudsjettTotal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       DbBudsjettTotal-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       DbSnitt:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       DbSnitt-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Bud:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ProsentTotal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ProsentTotal-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       SalgbudsjettTotal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       SalgBudsjettTotal-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SBudLinje */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SBudLinje */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OppdbudMnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdbudMnd C-Win
ON CHOOSE OF B-OppdbudMnd IN FRAME DEFAULT-FRAME /* Oppdater måned fra dag */
DO:

  bOk = FALSE.
  MESSAGE 
      'Dagene summeres sammen. Summene oppdateres så månedstotalen.' SKIP
      'Vil du gjøre dette?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk = FALSE THEN
      RETURN.

  DYNAMIC-FUNCTION("setpostUpdProc","sbudmaned_post_update.p").

  IF DYNAMIC-FUNCTION("DoUpdate","SBudManed","ignore",
      "",
      hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
      "SalgBudsjett,DbBudsjett,DbProsent",
      SalgBudsjettTotal-2:SCREEN-VALUE + "|" + DbBudsjettTotal-2:SCREEN-VALUE + "|" + DbSnitt-2:SCREEN-VALUE
     ,YES) THEN
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 

  RUN InvokeMethod(hBrowse,"DisplayRecord").
  RUN fargeTotaler.
  RUN InvokeMethod (hBrowse-2,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OppdbudMndDag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdbudMndDag C-Win
ON CHOOSE OF B-OppdbudMndDag IN FRAME DEFAULT-FRAME /* Oppdater dag fra måned */
DO:
  IF NOT hBuffer:AVAIL THEN
      RETURN NO-APPLY.

  bOk = FALSE.
  MESSAGE 
      'Salg månedstotalen vil bli fordelt ned på dagene i måneden med den omsetnings% som står på dagene.' SKIP(1)
      'Fordelingen av dekningsbidrag kan gjøres på to måter.' SKIP
      '   1 (Ja) DB prosenten på dagnivå beholdes og DbKr regnes om.' SKIP
      '   2 (Nei) DB prosent fra måneden settes inn på ALLE dagene og DbKr regnes om.'
      'Velg alternativ, eller avbryt.'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE bOk.
  IF bOk = ? THEN
      RETURN.

  IF bOk = TRUE THEN
      RUN sbudmaned_post_upd_dag.p
          (
           hBuffer,
           'Update',
           '',
           OUTPUT ocValue
          ).
  ELSE IF bOk = FALSE THEN
      RUN sbudmaned_post_upd_dag2.p
          (
           hBuffer,
           'Update',
           '',
           OUTPUT ocValue
          ).
      .

  RUN fargeTotaler.
  RUN InvokeMethod (hBrowse-2,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OppdBudTot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdBudTot C-Win
ON CHOOSE OF B-OppdBudTot IN FRAME DEFAULT-FRAME /* Oppdater totalt fra måned */
DO:
    /* Kjører en alternativ update rutine... */
    DYNAMIC-FUNCTION("setpostUpdProc","sbudhode_post_update2.p").
    
    bOk = FALSE.
    MESSAGE 
        'Månedene summeres sammen. Summene oppdateres så budsjettets totaler.' SKIP
        'Vil du gjøre dette?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
    IF bOk = FALSE THEN
        RETURN.

    IF DYNAMIC-FUNCTION("DoUpdate","SBudHode","ignore",
            "",
            hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
            "SalgBudsjett,DbBudsjett",
            SalgBudsjettTotal:SCREEN-VALUE + "|" + DbBudsjettTotal:SCREEN-VALUE 
           ,YES) THEN
        DYNAMIC-FUNCTION("RefreshRowids",hParentBrowse,hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
    
    RUN budsjettBeskrivelse.
    /* OpeQuery må gjøres fordi recordene slettes i sbudhode_post_update --> sbudhode_generer.p */
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RUN fargeTotaler.
    hBrowse:SELECT-FOCUSED-ROW().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  PUBLISH "InvalidateHandle".
  IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
  DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.
  
  RUN enable_UI.
  DYNAMIC-FUNCTION("setParent",SOURCE-PROCEDURE).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE budsjettBeskrivelse C-Win 
PROCEDURE budsjettBeskrivelse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  DO WITH FRAME {&FRAME-NAME}:
      FI-Bud:SCREEN-VALUE = "Budsjett: " +
                            STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::SBudId) + " " + 
                            "Butikk: " + 
                            STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::ButikkNr) + " " + 
                            "År: " + 
                            STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::Aar) + " " + 
                            hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::SBudBeskrivelse + " " + 
                            "Oms. kr: " + 
                            TRIM(STRING(DEC(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::SalgBudsjett),'->,>>>,>>>,>>9')) + ",- " + 
                            "Db Kr: " + 
                            TRIM(STRING(DEC(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::DbBudsjett),'->,>>>,>>>,>>9')) + ",-"
      .
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilterRecord C-Win 
PROCEDURE ClearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      /*
      ASSIGN
          FraEDato:SCREEN-VALUE    = ?
          TilEDato:SCREEN-VALUE    = ?
          Sasong:SCREEN-VALUE      = ''
          LevKod:SCREEN-VALUE      = ''
          Beskr:SCREEN-VALUE       = ''
          LevFargKod:SCREEN-VALUE  = ''
          Storl:SCREEN-VALUE       = ''
          LevNr:SCREEN-VALUE       = ''
          ArtikkelNr:SCREEN-VALUE  = ''
          StrekKode:SCREEN-VALUE   = ''
          vg:SCREEN-VALUE          = ''
          fraEtid:SCREEN-VALUE     = ''
          tilEtid:SCREEN-VALUE     = ''
          .
      */    
      DYNAMIC-FUNCTION("setSortString",hBrowse,"").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN    
    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
      RUN ArtikkelKortRecord.
  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "New;&Ny,delete;Slette,Print;S&kriv,rule,excel;Eksporter til E&xcel" 
                     + ",Linjeregistrering;Linjeregistrering,ArtikkelKort;Artikkelkort"
                     + ",HTexport;Varefil til håndterminal,HTsetup;Håndterminal oppsett"
                     + ",HentIkkeTalt;Hent alle størrelser på artiklene,Strekkode;Strekkoder"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */                /*action;label;tooltip;Method;image*/
 "AntTaltLikNull;Sett antall talt = 0,AntTaltLikAntPar;Sett antall talt lik antal par,TaBortTommeLinjer;Ta bort linjer med 0 i lager og antall talt,TaBortPosLinjer;Ta bort linjer med pos. differanse,TaBortNegLinjer;Ta bort linjer med neg. differanse"

------------------------------------------------------------------------------*/
  DEF VAR cDisabledEvents    AS CHAR NO-UNDO.

  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN    
  DO WITH FRAME {&FRAME-NAME}:
    /*
    IF hBuffer:AVAILABLE AND hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    DO:
      DYNAMIC-FUNCTION("CreateParentLink",hBrowse-2,hBrowse,"SBudId;SBudId").
      DYNAMIC-FUNCTION("setAttribute",hBrowse-2,'QueryJoin',
                       " AND SBudDag.AarMnd  = '" + STRING(hBuffer:BUFFER-FIELD('AarMnd'):BUFFER-VALUE) + "'").
    END.
    */
  END.

  RUN SUPER.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY SalgbudsjettTotal FI-Bud ProsentTotal DbBudsjettTotal DbSnitt 
          SalgBudsjettTotal-2 ProsentTotal-2 DbBudsjettTotal-2 DbSnitt-2 
      WITH FRAME DEFAULT-FRAME.
  ENABLE rectBrowse rectToolBar rectBrowse-2 B-OppdBudTot SalgbudsjettTotal 
         FI-Bud ProsentTotal DbBudsjettTotal B-OppdbudMndDag DbSnitt 
         B-OppdbudMnd SalgBudsjettTotal-2 ProsentTotal-2 DbBudsjettTotal-2 
         DbSnitt-2 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  /*
  DYNAMIC-FUNCTION("toExcelViaFile",hBrowse-2,0).         
  */
  
  
  DEFINE VARIABLE iSBudId AS INTEGER NO-UNDO.
  DEF VAR cMsg AS CHAR NO-UNDO.

  ASSIGN 
    iSBudId = INT(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SBudId'):BUFFER-VALUE) NO-ERROR.

  IF ERROR-STATUS:ERROR 
    THEN RETURN.
  ELSE DO: 
      MESSAGE 'Skal budsjettet eksporteres til Excel?'
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Excel eksport'  UPDATE bOk.

      IF bOk THEN DO:

          IF NOT DYNAMIC-FUNCTION("processQuery",hBrowse,"SBud_Til_Excel.p",
                                  STRING(iSBudId)) THEN 
          DO:
            cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
            IF INDEX(cMsg,CHR(10)) > 0 THEN
              DYNAMIC-FUNCTION("DoMessage",0,20,
                               cMsg,"",""). 
          END.

      END.
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelSheetParams C-Win 
PROCEDURE ExcelSheetParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
PUBLISH "ExcelSheetParams" (ihObject,chExcelInstance,chWorkbook,chWorksheet,iCount - 1).
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipObject        AS HANDLE     NO-UNDO.
DEF INPUT PARAMETER ipExcelInstance AS CHAR       NO-UNDO.
DEF INPUT PARAMETER ipWorkbook      AS COM-HANDLE NO-UNDO.
DEF INPUT PARAMETER ipWorksheet     AS COM-HANDLE NO-UNDO.
DEF INPUT PARAMETER ipI             AS INT        NO-UNDO.

  ipWorkSheet:Range("D:F"):NumberFormat = "@".
  ipWorkSheet:Range("P:P"):NumberFormat = "@".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fargeTotaler C-Win 
PROCEDURE fargeTotaler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ipDiff AS DEC NO-UNDO.

  IF hBuffer:AVAIL THEN
  DO:
    DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse,"").
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
    ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueSalgProsent").
    SalgBudsjettTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueSalgBudsjett").
    DbBudsjettTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueDbBudsjett").

    DbSnitt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(
                                                         (DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueDbBudsjett")) / DEC(DYNAMIC-FUNCTION("calkNettoFraBruttoOms",DEC(SalgBudsjettTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}),0))) * 100 
                                                         ).  
    /*
    DbSnitt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueDbProsent")) / hBuffer:BUFFER-FIELD("iAntMndDbGT0"):BUFFER-VALUE).
    IF hBuffer:BUFFER-FIELD("iAntMndDbGT0"):BUFFER-VALUE = 0 THEN
        DbSnitt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
    */
  END.
                                                            
  IF hBuffer-2:AVAIL THEN
  DO:
    DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse-2,"").
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse-2,hBuffer-2:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
    DbSnitt-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse-2,"LocalStatValueDbProsent")) / hBuffer-2:BUFFER-FIELD("iAntDagDbGT0"):BUFFER-VALUE).
    ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse-2,"LocalStatValueSalgProsent").
    SalgBudsjettTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse-2,"LocalStatValueSalgBudsjett").
    DbBudsjettTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse-2,"LocalStatValueDbBudsjett").
    IF hBuffer-2:BUFFER-FIELD("iAntDagDbGT0"):BUFFER-VALUE = 0 THEN
        DbSnitt-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
    /*
    hfiSalgBudsjett:SCREEN-VALUE = SalgBudsjettTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    hfiDbBudsjett:SCREEN-VALUE = DbBudsjettTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    */
  END. 

  ipDiff = ABS(100.00 - DEC(ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
  ProsentTotal:BGCOLOR IN FRAME {&FRAME-NAME} =  IF ipDiff > 0.05 AND 
                                                  0 <> DEC(ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                                 THEN iRed ELSE ?. 
  IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  DO:
      SalgBudsjettTotal:BGCOLOR IN FRAME {&FRAME-NAME} = IF ROUND(DEC(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::SalgBudsjett),0) <> ROUND(DEC(SalgBudsjettTotal:SCREEN-VALUE),0)
                                                           THEN iRed ELSE ?.
      DbBudsjettTotal:BGCOLOR IN FRAME {&FRAME-NAME} = IF ROUND(DEC(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::DbBudsjett),0) <> ROUND(DEC(DbBudsjettTotal:SCREEN-VALUE),0)
                                                           THEN iRed ELSE ?.
  END.

  SalgBudsjettTotal-2:BGCOLOR IN FRAME {&FRAME-NAME} = IF hfiSalgBudsjett:SCREEN-VALUE <> SalgBudsjettTotal-2:SCREEN-VALUE
                                                       THEN iRed ELSE ?.
  DbBudsjettTotal-2:BGCOLOR IN FRAME {&FRAME-NAME} = IF hfiDbBudsjett:SCREEN-VALUE <> DbBudsjettTotal-2:SCREEN-VALUE
                                                       THEN iRed ELSE ?.
  ipDiff = ABS(100.00 - DEC(ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
  ProsentTotal-2:BGCOLOR IN FRAME {&FRAME-NAME} =  IF ipDiff > 0.05 AND 
                                                  0 <> DEC(ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME})  
                                                 THEN iRed ELSE ?. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR hColumn          AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  /* TN 7/1-10 Dette slå ar spørsmål på sesjonsninvå. Skal ikke gjøres. Spørsmål skal heller ikke slås av. */
  /*DYNAMIC-FUNCTION("setAttribute",SESSION,'DropExcelWarning','YES').*/

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "SBudManed"
                               + ";!SBudId|BudId"
                               + ";!AarMnd"
                               + ";+tmpAar|Int|9999|tmpAar(ROWID)|År"
                               + ";+tmpMnd|Char|x(4)|tmpMnd(ROWID)|Mnd"
                               + ";SalgBudsjett|Oms"
                               + ";SalgProsent|Oms%|->>>9.99"
                               + ";+linjeSum|dec|->>>9.99|linjeSum(ROWID)|SOms%"
                               + ";DbBudsjett|Db"
                               + ";DbProsent|Db%|->>>9.99"
                               + ";+DbSnitt|decimal|->>>9.99|DbSnitt(ROWID)|SnDb% Dag%"
                               + ";!+chkProc|logical|*/|chkProc(ROWID)|"
                               + ";EDato"
                               + ";!ETid"
                               + ";+endretTid|character|x(6)|endretTid(ROWID)|Kl"
                               + ";Brukerid"
                               + ";+iAntMndDbGT0|integer|->>>9|iAntMndDbGT0(ROWID)|AntMndDbGT0"
                             ,"WHERE false"
                             ,"").
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"SalgBudsjett").
  hColumn:WIDTH-PIXELS = 100.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"DbBudsjett").
  hColumn:WIDTH-PIXELS = 100.

  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,
                "queryStatFields","SalgBudsjett,SalgProsent,DbBudsjett,DbProsent").
  DYNAMIC-FUNCTION("setSortString",hBrowse,"AarMnd").   

  /* hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse). */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"postUpdateProc","sbudmaned_post_update.p").  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=delval_sbudmaned.p").
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','sbudmaned_brwcalc.p').
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "flatview,refresh,excel;Eksporter til E&xcel" 
                     + ",rule"
                     /*+ ",Linjeregistrering;Linjeregistrering"*/
                    ,"maxborder").                  /* Misc - enable, maxborder.. */                /*action;label;tooltip;Method;image*/

  /*
  DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowse,  /* parent widget */
                  "AntTaltLikNull;Sett antall talt = 0,AntTaltLikAntPar;Sett antall talt lik antal par,TaBortTommeLinjer;Ta bort linjer med 0 i lager og antall talt,TaBortPosLinjer;Ta bort linjer med pos. differanse,TaBortNegLinjer;Ta bort linjer med neg. differanse,OppdVVarekostPaLinjer;Oppdater varekost på linje(r)"
                 ,"").   
  */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
  
/*   fillTime(fraEtid:HANDLE,'00:00','23:59',15).       */
/*   tilETid:LIST-ITEM-PAIRS = fraETid:LIST-ITEM-PAIRS. */

  /* Oppdaterbar felt i browse */
  hfiSalgBudsjett = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "SalgBudsjett",     
                      "Salgbudsjett",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiSalgBudsjett,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiSalgBudsjett,"SalgBudsjett").
  hfiProsent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "SalgProsent",     
                      "SalgProsent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiProsent,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiProsent,"SalgProsent").
  hfiDbBudsjett = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "DbBudsjett",     
                      "DbBudsjett",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiDbBudsjett,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiDbBudsjett,"DbBudsjett").
  hfiDbProsent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "DbProsent",     
                      "DbProsent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiDbProsent,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiDbProsent,"DbProsent").
  

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customUpdateValProc","=updval_SBudManed.p").
    
  /* Oppdaterbar felt i browse */
  /*
  hfcMerknad = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "Merknad",     
                    "Merknad",     
                    "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfcMerknad,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfcMerknad,"Merknad").
  */
  hBrowse-2 = DYNAMIC-FUNCTION("NewBrowse"
              ,rectBrowse-2:HANDLE
              ,100
              ,""
              ,"SBudDag"
               + ";!SBudId"
               + ";!AarMnd"
               + ";!AarMndDag"
               + ";!+tmpMnd|Char|x(4)|tmpMnd(ROWID)|Mnd"
               + ";+tmpdMDag|date|99/99/99|tmpdMDag(ROWID)|Dato"
               + ";+tmpcUDag|Char|x(4)|tmpcUDag(ROWID)|UDag"
               + ";SalgBudsjett|Oms dag"
               + ";SalgProsent|Oms% dag|->>>9.99"
               + ";DbBudsjett|Db dag"
               + ";DbProsent|Db% dag|->>>9.99"
               + ";EDato"
               + ";!ETid"
               + ";+endretTid|character|x(6)|endretTid(ROWID)|Kl"
               + ";Brukerid"                   
               + ";+iAntDagDbGT0|integer|->>>9|iAntDagDbGT0(ROWID)|AntDagDbGT0"
              ,"WHERE false"
              ,"").
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer-2 = hBrowse-2:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse-2,"postUpdateProc","sbuddag_post_update.p").
  DYNAMIC-FUNCTION('setAttribute',hBrowse-2,'calcfieldproc','sbuddag_brwcalc.p').
  DYNAMIC-FUNCTION("setAttribute",hBrowse-2,
                "queryStatFields","SalgBudsjett,SalgProsent,DbBudsjett,DbProsent").
  DYNAMIC-FUNCTION("setSortString",hBrowse-2,"AarMndDag").   

  /* Knytter Dag browser til måned browser. */
  DYNAMIC-FUNCTION("CreateParentLink",hBrowse-2,hBrowse,"SBudId,AarMnd").

  /* Oppdaterbar felt i browse */
  hfiSalgBudsjett-2 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse-2,          
                      "SalgBudsjett",     
                      "Salgbudsjett",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiSalgBudsjett-2,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse-2,hfiSalgBudsjett-2,"SalgBudsjett").
  hfi2Prosent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse-2,          
                      "SalgProsent",     
                      "SalgProsent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfi2Prosent,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse-2,hfi2Prosent,"SalgProsent").
  hfiDbBudsjett-2 = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse-2,          
                      "DbBudsjett",     
                      "DbBudsjett",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiDbBudsjett-2,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse-2,hfiDbBudsjett-2,"DbBudsjett").
  hfi2DbProsent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse-2,          
                      "DbProsent",     
                      "DbProsent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfi2DbProsent,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse-2,hfi2DbProsent,"DbProsent").

  /*DYNAMIC-FUNCTION("setAttribute",hBrowse-2,"customUpdateValProc","=updval_SBudMalDag.p").*/


  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "+," + hBrowse:NAME).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION('setCurrentObject',hBrowse).  
DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
hChild = ?.
  APPLY 'value-changed' TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn C-Win 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN SUPER.

RUN fargeTotaler.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.
  RUN fargeTotaler.
  RUN budsjettBeskrivelse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
IF iReturn = 1 THEN  
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_report.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_report.p",'').
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
  
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
DEF VAR cList   AS CHAR NO-UNDO.

hBrowse:SELECT-FOCUSED-ROW() NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN.
/*
RUN JBoxBrowseSelectMsg.w ("Sende valgte poster til rapport?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn).  /*1=Alle,2=Valgte*/

/* MESSAGE 'Programmerer må ferdigstille listen...' */
/*   VIEW-AS ALERT-BOX INFO BUTTONS OK.             */
/* LEAVE.                                           */
/* MESSAGE hParentBrowse:QUERY:PREPARE-STRING */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
IF iReturn = 1 THEN
  RUN printTellelinjeX.p ('Tellehode|' + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE) + '|Tellelinje|*',"").
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cList = cList + ',' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Rowident1'):BUFFER-VALUE).
  END.
  cList = TRIM(cList,',').
  RUN printTellelinjeX.p ('Tellehode|' + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE) +  '|Tellelinje|' + cList,"").    
END.
ELSE 
  LEAVE.

*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowser C-Win 
PROCEDURE RefreshBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  DUMMY PROCEDURE for å tilfredsstille gammelt program      
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipRowid AS ROWID NO-UNDO.
  DEF VAR iArtNr AS INT NO-UNDO.

  DEF VAR bOk AS LOG NO-UNDO.

  
  DO WITH FRAME Default-Frame:
      /*
      FIND TelleLinje NO-LOCK WHERE
          ROWID(TelleLinje) = to-rowid(STRING(ipRowid)) NO-ERROR.
      */
      /*
      iArtNr = int(DYNAMIC-FUNCTION("getFieldValues","Tellelinje","WHERE rowid(tellelinje) = to-rowid(" + quoter(STRING(ipRowid)) + ')',"ArtikkelNr")).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",' AND ArtikkelNr = ' + STRING(iArtNr)).
      */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",' AND ArtikkelNr = ' + STRING(TelleLinje.ArtikkelNr)). */

      bFlag = TRUE. /*Check openquery*/

      RUN InvokeMethod(hBrowse,"OpenQuery").

      IF hBuffer:AVAIL THEN
          DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  END.

  RETURN "Ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  hbclinjeSum:BGCOLOR =  IF hbflinjeSum:BUFFER-VALUE = 100.00
                           THEN ?
                         ELSE IF hbflinjeSum:BUFFER-VALUE = 0
                           THEN ?
                         ELSE iLtRed.

  hbcProsent:BGCOLOR    =  iLtYellow.
  hbcDbProsent:BGCOLOR  =  iLtYellow.
  hbc2Prosent:BGCOLOR   =  iLtYellow.
  hbc2DbProsent:BGCOLOR =  iLtYellow.

  hbcDbSnitt:BGCOLOR =  IF ABS(hbfDbProsent:BUFFER-VALUE - hbfDbSnitt:BUFFER-VALUE) <= 0.5
                           THEN ?
                         ELSE iLtRed.
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse-2 THEN 
DO: 

    IF iAar <> YEAR(hbftmpdMDag:BUFFER-VALUE) OR cHelligDager = '' THEN
    DO:
        iAar = YEAR(hbftmpdMDag:BUFFER-VALUE).
        RUN bibl_Helligdager.p (iAar, OUTPUT cHelligDager).
    END.

    hbctmpcUDag:BGCOLOR =  IF TRIM(hbftmpcUDag:BUFFER-VALUE) <> 'SØN' 
                             THEN ?
                           ELSE iLtGreen.

    IF CAN-DO(cHelligdager,TRIM(hbftmpdMDag:BUFFER-VALUE)) 
                             THEN hbctmpcUDag:BGCOLOR = iLtRed.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowsToBatchRecord C-Win 
PROCEDURE RowsToBatchRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowsToBatch AS CHAR NO-UNDO.

RUN JBoxDSelectRowsToBatch.w (DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowsToBatch"), OUTPUT cRowsToBatch).

IF cRowsToBatch NE "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"rowsToBatch",cRowsToBatch).
  DYNAMIC-FUNCTION("setCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW,"<StartRowsToBatch>|brwLinje|" + cRowsToBatch + "|<EndRowsToBatch>").
  RUN INVOKEMETHOD(hBrowse,'OpenQuery').
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  IF icBrowseName = 'RectBrowse' THEN
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'DbSnitt' THEN 
        ASSIGN
          hbcDbSnitt = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfDbSnitt = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbSnitt')                                                                            
        .
      WHEN 'SalgBudsjett' THEN 
        ASSIGN
          hbcSalgBudsjett = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfSalgbudsjett = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SalgBudsjett')                                                                            
        .

      WHEN 'DbBudsjett' THEN 
        ASSIGN
          hbcDbBudsjett = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfDbBudsjett = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbBudsjett')                                                                            
        .

      WHEN 'SalgProsent' THEN 
        ASSIGN
          hbcProsent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfProsent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SalgProsent')                                                                            
        .
      
      WHEN 'DbProsent' THEN 
        ASSIGN
          hbcDbProsent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfDbProsent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbProsent')                                                                            
        .

      WHEN 'linjeSum' THEN 
        ASSIGN
          hbclinjeSum = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbflinjeSum = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('linjeSum')
        .
    END CASE.
  END.

  ELSE IF icBrowseName = 'RectBrowse-2' THEN
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'SalgBudsjett' THEN 
        ASSIGN
          hbcSalgBudsjett-2 = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfSalgbudsjett-2 = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SalgBudsjett')                                                                            
        .

      WHEN 'DbBudsjett' THEN 
        ASSIGN
          hbcDbBudsjett-2 = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfDbBudsjett-2 = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbBudsjett')                                                                            
        .
      WHEN 'SalgProsent' THEN 
        ASSIGN
          hbc2Prosent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbf2Prosent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('SalgProsent')                                                                            
        .

      WHEN 'DbProsent' THEN 
        ASSIGN
          hbc2DbProsent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbf2DbProsent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbProsent')                                                                            
        .

      WHEN 'tmpcUDag' THEN 
        ASSIGN
          hbctmpcUDag = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbftmpcUDag = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tmpcUDag')
        .

      WHEN 'tmpdMDag' THEN 
        ASSIGN
          hbctmpdMDag = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbftmpdMDag = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tmpdMDag')                                                                            
        .
    END CASE.
  END.
  
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  /*DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField").*/
  DYNAMIC-FUNCTION("setAddMoveY", 
                   THIS-PROCEDURE:CURRENT-WINDOW, 
                   FRAME {&FRAME-NAME}:HANDLE, 
                   "SalgbudsjettTotal,ProsentTotal,DbBudsjettTotal,DbSnitt,SalgBudsjettTotal-2,ProsentTotal-2,DbBudsjettTotal-2,DbSnitt-2").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse").


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ihQueryObject = hBrowse THEN DO WITH FRAME {&FRAME-NAME}:
  ProsentTotal:SCREEN-VALUE = 
     DYNAMIC-FUNCTION("getAttribute",
     ihQueryObject,"statValueProsent").

END.
ELSE DO:
    ProsentTotal-2:SCREEN-VALUE = 
       DYNAMIC-FUNCTION("getAttribute",
       ihQueryObject,"statValueProsent").
END.
ProsentTotal:BGCOLOR IN FRAME {&FRAME-NAME} =  IF 100 <> INT(ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                                                 THEN iRed ELSE ?. 
ProsentTotal-2:BGCOLOR IN FRAME {&FRAME-NAME} =  IF 100 <> INT(ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                                                 THEN iRed ELSE ?. 


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

