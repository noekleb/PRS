&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

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
DEF INPUT  PARAM icQryList    AS CHAR NO-UNDO.
DEF INPUT  PARAM icTbList     AS CHAR NO-UNDO.
DEF INPUT  PARAM icFmList     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocProcList   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCodeList   AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cFirstQry AS CHAR NO-UNDO.
DEF VAR cFirstFm  AS CHAR NO-UNDO.
DEF VAR cFirstTb  AS CHAR NO-UNDO.
DEF VAR iNumQry   AS INT  NO-UNDO.
DEF VAR iNumTb    AS INT  NO-UNDO.
DEF VAR iNumFm    AS INT  NO-UNDO.
DEF VAR cUser     AS CHAR NO-UNDO.

DEF TEMP-TABLE ttProcs
    FIELD cCat       AS CHAR 
    FIELD bSelect    AS LOG  LABEL "Select" 
    FIELD cName      AS CHAR LABEL "Procedure/function name" FORMAT "x(40)"
    FIELD cType      AS CHAR LABEL "Applies to" FORMAT "x(30)"
    FIELD cDesc      AS CHAR LABEL "Description"    FORMAT "x(30)"
    FIELD cCode      AS CHAR LABEL "Code" FORMAT "x(30)"
    FIELD cClip      AS CHAR LABEL "Clip" FORMAT "x(30)"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttProcs

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 cName cClip cType cDesc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttProcs
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttProcs.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttProcs
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttProcs


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbApplyToQry rsOverrideHook cmbApplyToFm ~
BROWSE-4 cmbApplyToTb edDesc btnSelectCode edCode edClip btnOk btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cmbApplyToQry rsOverrideHook cmbApplyToFm ~
cmbApplyToTb edDesc edCode edClip 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillFromConfig C-Win 
FUNCTION fillFromConfig RETURNS LOGICAL
  ( INPUT icConfigFile AS CHAR,
    INPUT icCat        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ImportConfig C-Win 
FUNCTION ImportConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReplaceWithObject C-Win 
FUNCTION ReplaceWithObject RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOk 
     LABEL "Ok" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSelectCode 
     LABEL "Select code template" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE cmbApplyToFm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Substitute FieldMap object" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cmbApplyToQry AS CHARACTER FORMAT "X(256)":U 
     LABEL "Substitute Browse/Query object" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 35.8 BY 1 NO-UNDO.

DEFINE VARIABLE cmbApplyToTb AS CHARACTER FORMAT "X(256)":U 
     LABEL "Substitute Toolbar object" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE edClip AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 103 BY 3.1 NO-UNDO.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 103 BY 8.29 TOOLTIP "If code doesnt't start with PROCEDURE or FUNCTION it must be copied manually" NO-UNDO.

DEFINE VARIABLE edDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 102.8 BY 6.43 NO-UNDO.

DEFINE VARIABLE rsOverrideHook AS CHARACTER INITIAL "override" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Overrides", "override",
"Hooks", "hook",
"User contr.samples", "usercontrol",
"Other", "other"
     SIZE 55 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttProcs SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      cName WIDTH 25
      cClip
      cType   
cDesc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 23.14 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbApplyToQry AT ROW 1.24 COL 117.8 COLON-ALIGNED WIDGET-ID 4
     rsOverrideHook AT ROW 1.38 COL 3 NO-LABEL WIDGET-ID 12
     cmbApplyToFm AT ROW 2.38 COL 117.6 COLON-ALIGNED WIDGET-ID 10
     BROWSE-4 AT ROW 2.86 COL 3 WIDGET-ID 200
     cmbApplyToTb AT ROW 3.48 COL 117.6 COLON-ALIGNED WIDGET-ID 56
     edDesc AT ROW 4.81 COL 53.2 NO-LABEL WIDGET-ID 16
     btnSelectCode AT ROW 11.29 COL 132.2 WIDGET-ID 54
     edCode AT ROW 12.48 COL 53 NO-LABEL WIDGET-ID 18
     edClip AT ROW 21.86 COL 53 NO-LABEL WIDGET-ID 58
     btnOk AT ROW 25.29 COL 126 WIDGET-ID 6
     btnCancel AT ROW 25.29 COL 141.4 WIDGET-ID 8
     "Clip - additional code automatically added - typically for corresponding widget (using ~"paste~"):" VIEW-AS TEXT
          SIZE 88.8 BY .62 AT ROW 21.1 COL 53.2 WIDGET-ID 60
     "Code:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.71 COL 53.2 WIDGET-ID 22
     "Description:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.95 COL 53.2 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.6 BY 25.62 WIDGET-ID 100.


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
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Select override, hook, sample or clip"
         HEIGHT             = 25.62
         WIDTH              = 156.6
         MAX-HEIGHT         = 25.62
         MAX-WIDTH          = 156.6
         VIRTUAL-HEIGHT     = 25.62
         VIRTUAL-WIDTH      = 156.6
         ALWAYS-ON-TOP      = yes
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-4 cmbApplyToFm DEFAULT-FRAME */
ASSIGN 
       BROWSE-4:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       edCode:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

ASSIGN 
       edDesc:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttProcs.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Select override, hook, sample or clip */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Select override, hook, sample or clip */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON VALUE-CHANGED OF BROWSE-4 IN FRAME DEFAULT-FRAME
DO:
  IF AVAIL ttProcs THEN
    ASSIGN edCode:SCREEN-VALUE = ttProcs.cCode
           edDesc:SCREEN-VALUE = ttProcs.cDesc
           edClip:SCREEN-VALUE = ttProcs.cClip.
  ELSE
    ASSIGN edCode:SCREEN-VALUE = ""
           edDesc:SCREEN-VALUE = ""
           edClip:SCREEN-VALUE = "".

  ReplaceWithObject().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  IF ENTRY(1,edCode:SCREEN-VALUE,":") BEGINS "PROCEDURE " THEN
    ASSIGN ocProcList = TRIM(ENTRY(2,ENTRY(1,edCode:SCREEN-VALUE,":")," "),":")
           ocCodeList = SUBSTR(edCode:SCREEN-VALUE,LENGTH(ENTRY(1,edCode:SCREEN-VALUE,":")) + 2)
           .
  ELSE IF edCode:SCREEN-VALUE NE "" THEN
    ASSIGN ocProcList = ttProcs.cName
           ocCodeList = edCode:SCREEN-VALUE.
  
  IF edClip:SCREEN-VALUE NE "" THEN
    ASSIGN ocProcList = ocProcList + ",CLIPBOARD"
           ocCodeList = ocCodeList + "|" + edClip:SCREEN-VALUE
           .
  
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectCode C-Win
ON CHOOSE OF btnSelectCode IN FRAME DEFAULT-FRAME /* Select code template */
DO:
  DEF VAR bOk AS LOG NO-UNDO.
  DEF VAR cFileName AS CHAR NO-UNDO.
  DEF VAR cTemplateDir AS CHAR NO-UNDO.


  IF NOT AVAIL ttProcs THEN RETURN NO-APPLY.

  cTemplateDir = SEARCH("dictview.w").
  IF cTemplateDir NE ? THEN DO:
    FILE-INFO:FILE-NAME = SEARCH("dictview.w").
    cTemplateDir = FILE-INFO:FULL-PATHNAME.
    ENTRY(NUM-ENTRIES(cTemplateDir,"\"),cTemplateDir,"\") = "template".
  END.

  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Templates for " + ttProcs.cName ttProcs.cName + "*.p",
                        "All templates" "*.p"
                MUST-EXIST
                INITIAL-DIR cTemplateDir
                UPDATE bOk.

  IF cFileName NE "" THEN DO:
    ttProcs.cCode = ImportConfig(cFileName).
    BROWSE {&BROWSE-NAME}:REFRESH().
    BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
    APPLY "value-changed" TO BROWSE {&BROWSE-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbApplyToFm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbApplyToFm C-Win
ON VALUE-CHANGED OF cmbApplyToFm IN FRAME DEFAULT-FRAME /* Substitute FieldMap object */
DO:
  ReplaceWithObject().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbApplyToQry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbApplyToQry C-Win
ON VALUE-CHANGED OF cmbApplyToQry IN FRAME DEFAULT-FRAME /* Substitute Browse/Query object */
DO:
  ReplaceWithObject().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbApplyToTb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbApplyToTb C-Win
ON VALUE-CHANGED OF cmbApplyToTb IN FRAME DEFAULT-FRAME /* Substitute Toolbar object */
DO:
  ReplaceWithObject().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edCode C-Win
ON LEAVE OF edCode IN FRAME DEFAULT-FRAME
DO:
  IF SELF:MODIFIED THEN
    ttProcs.cCode = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsOverrideHook
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsOverrideHook C-Win
ON VALUE-CHANGED OF rsOverrideHook IN FRAME DEFAULT-FRAME
DO:
  BROWSE {&BROWSE-NAME}:QUERY:QUERY-PREPARE("FOR EACH ttProcs WHERE cCat = '" + SELF:SCREEN-VALUE + "' BY ttProcs.cName").
  BROWSE {&BROWSE-NAME}:QUERY:QUERY-OPEN().
  APPLY "value-changed" TO BROWSE {&BROWSE-NAME}.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  DISPLAY cmbApplyToQry rsOverrideHook cmbApplyToFm cmbApplyToTb edDesc edCode 
          edClip 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cmbApplyToQry rsOverrideHook cmbApplyToFm BROWSE-4 cmbApplyToTb edDesc 
         btnSelectCode edCode edClip btnOk btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitHooks C-Win 
PROCEDURE InitHooks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE ttProcs.
ASSIGN ttProcs.cCat    = "hook"
       ttProcs.cName   = "BeforeNavBrowseFillIn"
       ttProcs.cDesc   = "The purpose for this hook is to enable additional options for validation and also to provide a place for committing a new local browse row to the database."                     
                       + CHR(10) + "Applies to the LeaveBrowseFillIn method"
       ttProcs.cCode   = "DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO." + CHR(10)
                       + "DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO." + CHR(10)
                       + "DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO." + CHR(10)
       .

CREATE ttProcs.
ASSIGN ttProcs.cCat    = "hook"
       ttProcs.cName   = "ExtraDeleteRecord"
       ttProcs.cDesc   = "ExtraDeleteRecord is primary to enable additional records to be deleted in the same transaction as the current (or new) record in the FieldMap. If the hook is used the transaction must be committed from the procedure, see the DoCommit Server API."                     
                       + CHR(10) + "Applies to the DeleteRecord method"
       ttProcs.cCode   = "DEF OUTPUT PARAM obOk     AS LOG  NO-UNDO." + CHR(10)
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitOverrides C-Win 
PROCEDURE InitOverrides :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE ttProcs.
ASSIGN ttProcs.cCat = "override"
       ttProcs.cName   = "NewRecord"
       ttProcs.cDesc   = "NewRecord"
       .
CREATE ttProcs.
ASSIGN ttProcs.cCat = "override"
       ttProcs.cName   = "EditRecord"
       ttProcs.cDesc   = "EditRecord"
       .
CREATE ttProcs.
ASSIGN ttProcs.cCat = "override"
       ttProcs.cName   = "UndoRecord"
       ttProcs.cDesc   = "UndoRecord"
       .

CREATE ttProcs.
ASSIGN ttProcs.cCat = "override"
       ttProcs.cName   = "SaveRecord"
       ttProcs.cDesc   = "Typically you'd like to pass additional information to the save operation here."
                       + CHR(10) + "Also you might need to know wether the save operation is for a new (or copy of a) record."   
                       + CHR(10) + "To add additional fields and data use the property bufferExtraFields (comma-sep list)"
                       + CHR(10) + "with corresponding values set using the bufferExtraValues (pipe-separated)."
                       + CHR(10) + CHR(10) + "To determine if it's a new record check the toolbar-state (before RUN SUPER):"
                       + CHR(10) + "cTbState = " + (IF cFirstTb NE "" THEN cFirstTb ELSE "oTb<My toolbar>") + ":objectState".
       .
CREATE ttProcs.
ASSIGN ttProcs.cCat    = "override"
       ttProcs.cName   = "DisplayRecord"
       ttProcs.cDesc   = "Typical action to do before RUN SUPER:" 
                       + CHR(10) + " - enable/disable tools based on data in current row:"
                       + CHR(10) + "   " + (IF cFirstTb NE "" THEN cFirstTb ELSE "oTb<My toolbar>") + ':disabledTools = "new,edit..".' 
                       + CHR(10) + CHR(10) + "Typical action to do after RUN SUPER:"
                       + CHR(10) + " - enable/disable input fields to override default behaviour"
                       .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cFirstQry = ENTRY(1,icQryList)
         cFirstFm  = ENTRY(1,icFmList)
         cFirstTb  = ENTRY(1,icTbList)
         iNumQry   = NUM-ENTRIES(icQryList)
         iNumFm    = NUM-ENTRIES(icFmList)
         iNumTb    = NUM-ENTRIES(icTbList)
         cmbApplyToQry:LIST-ITEMS = "," + icQryList
         cmbApplyToFm:LIST-ITEMS = "," + icFmList
         cmbApplyToTb:LIST-ITEMS = "," + icTbList
         .

  IF cFirstQry NE "" THEN cmbApplyToQry:SCREEN-VALUE = cFirstQry.
  IF cFirstFm NE "" THEN cmbApplyToFm:SCREEN-VALUE = cFirstFm.
  IF cFirstTb NE "" THEN cmbApplyToTb:SCREEN-VALUE = cFirstTb.

  cUser = OS-GETENV("username").

  IF NOT fillFromConfig("template\overrideConfig.txt","override") THEN
    RUN InitOverrides.
  
  IF NOT fillFromConfig("template\hookConfig.txt","hook") THEN
    RUN InitHooks.

  fillFromConfig("template\usercontrolSamplesConfig.txt","usercontrol").

  fillFromConfig("template\otherSamplesConfig.txt","other").


  APPLY "value-changed" TO rsOverrideHook.

  SESSION:SET-WAIT-STATE("").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillFromConfig C-Win 
FUNCTION fillFromConfig RETURNS LOGICAL
  ( INPUT icConfigFile AS CHAR,
    INPUT icCat        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cConfig     AS CHAR NO-UNDO.
DEF VAR cLine       AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR cTag        AS CHAR NO-UNDO.

IF SEARCH(icConfigFile) NE ? THEN DO:
  cConfig = ImportConfig(SEARCH(icConfigFile)).

  IF cConfig NE "" THEN DO ix = 1 TO NUM-ENTRIES(cConfig,CHR(10)):
    cLine = RIGHT-TRIM(ENTRY(ix,cConfig,CHR(10))).

    IF TRIM(cLine) BEGINS "<name>" THEN DO:
      cLine = TRIM(cLine).
      CREATE ttProcs.
      ASSIGN ttProcs.cName  = SUBSTR(cLine,7)
             ttProcs.cCat   = icCat
             .
    END. 
    ELSE IF TRIM(cLine) BEGINS "<desc>" THEN DO:
      cTag = "<desc>".  
      cLine = TRIM(cLine).
      IF AVAIL ttProcs AND SUBSTR(cLine,7) NE "" THEN 
        ttProcs.cDesc = ttProcs.cDesc + (IF ttProcs.cDesc NE "" THEN CHR(10) ELSE "") + SUBSTR(cLine,7).
    END. 
    ELSE IF TRIM(cLine) BEGINS "<code>" THEN DO:
      cTag = "<code>".  
      cLine = TRIM(cLine).
      IF AVAIL ttProcs AND SUBSTR(cLine,7) NE "" THEN 
        ttProcs.cCode = ttProcs.cCode + (IF ttProcs.cCode NE "" THEN CHR(10) ELSE "") + SUBSTR(cLine,7).
    END. 
    ELSE IF cLine BEGINS "<clip>" THEN DO:
      cTag = "<clip>".  
      cLine = TRIM(cLine).
      IF AVAIL ttProcs AND SUBSTR(cLine,7) NE "" THEN 
        ttProcs.cClip = ttProcs.cClip + (IF ttProcs.cClip NE "" THEN CHR(10) ELSE "") + SUBSTR(cLine,7).
    END. 
    ELSE IF TRIM(cLine) BEGINS "<type>" THEN DO:
      cTag = "<type>".  
      cLine = TRIM(cLine).
      IF AVAIL ttProcs AND SUBSTR(cLine,7) NE "" THEN 
        ttProcs.cType = SUBSTR(cLine,7).
    END. 
    ELSE IF cLine BEGINS "<cat>" THEN DO:
      icCat = TRIM(ENTRY(2,cLine,">")).
      IF AVAIL ttProcs THEN ttProcs.cCat = icCat.
    END. 
    ELSE IF AVAIL ttProcs THEN DO:
      IF cTag = "<desc>" THEN
        ttProcs.cDesc = ttProcs.cDesc + (IF ttProcs.cDesc NE "" THEN CHR(10) ELSE "") + cLine.
      ELSE IF cTag = "<code>" THEN
        ttProcs.cCode = ttProcs.cCode + (IF ttProcs.cCode NE "" THEN CHR(10) ELSE "") + cLine.
      ELSE IF cTag = "<clip>" THEN
        ttProcs.cClip = ttProcs.cClip + (IF ttProcs.cClip NE "" THEN CHR(10) ELSE "") + cLine.
    END.
  END.

  RETURN YES.  
END.
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ImportConfig C-Win 
FUNCTION ImportConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput  AS CHAR NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.

INPUT FROM VALUE(icFileName).

REPEAT:
  IMPORT UNFORMATTED cInput.  
  cReturn = cReturn + cInput + CHR(10).
END.
INPUT CLOSE.

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReplaceWithObject C-Win 
FUNCTION ReplaceWithObject RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQryObject AS CHAR NO-UNDO.
DEF VAR cFmObject  AS CHAR NO-UNDO.
DEF VAR cTbObject  AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF cmbApplyToQry:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" AND cmbApplyToQry:SCREEN-VALUE NE ? THEN 
    cQryObject = cmbApplyToQry:SCREEN-VALUE.
  ELSE
    cQryObject = cFirstQry.

  IF cmbApplyToFm:SCREEN-VALUE NE "" AND cmbApplyToFm:SCREEN-VALUE NE ? THEN
    cFmObject = cmbApplyToFm:SCREEN-VALUE.
  ELSE
    cFmObject = cFirstFm.

  IF cmbApplyToTb:SCREEN-VALUE NE "" AND cmbApplyToTb:SCREEN-VALUE NE ? THEN
    cTbObject = cmbApplyToTb:SCREEN-VALUE.
  ELSE
    cTbObject = cFirstTb.

  IF cQryObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<QueryObject>",cQryObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<QueryObject>",cQryObject)
           edClip:SCREEN-VALUE = REPLACE(edClip:SCREEN-VALUE,"<QueryObject>",cQryObject)
           .
  IF cFmObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<FieldMapObject>",cFmObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<FieldMapObject>",cFmObject) 
           edClip:SCREEN-VALUE = REPLACE(edClip:SCREEN-VALUE,"<FieldMapObject>",cFmObject) 
           .
  IF cTbObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<ToolbarObject>",cTbObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<ToolbarObject>",cTbObject) 
           edClip:SCREEN-VALUE = REPLACE(edClip:SCREEN-VALUE,"<ToolbarObject>",cTbObject) 
           .

  edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<userid>",cUser).
  edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<now>",STRING(NOW,"99/99/9999 hh:mm")).
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

