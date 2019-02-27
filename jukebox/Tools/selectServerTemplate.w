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

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE ttProcs
    FIELD cCat    AS CHAR 
    FIELD bSelect AS LOG  LABEL "Select" 
    FIELD cName   AS CHAR LABEL "Name" FORMAT "x(40)"
    FIELD cDesc   AS CHAR LABEL "Description"    FORMAT "x(30)"
    FIELD cCode   AS CHAR LABEL "Code" FORMAT "x(30)"
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
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 cName cDesc   
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
&Scoped-Define ENABLED-OBJECTS rsOverrideHook BROWSE-4 edDesc btnSelectCode ~
btnSaveCode edCode btnCancel 
&Scoped-Define DISPLAYED-OBJECTS rsOverrideHook edDesc edCode 

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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Done" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSaveCode 
     LABEL "Save code as .p" 
     SIZE 24 BY 1.14.

DEFINE BUTTON btnSelectCode 
     LABEL "Select code template" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 113.2 BY 11.62 NO-UNDO.

DEFINE VARIABLE edDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 113 BY 7.95 NO-UNDO.

DEFINE VARIABLE rsOverrideHook AS CHARACTER INITIAL "hook" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Transaction hooks", "hook",
"Business logic procedures", "bl",
"Custom queries", "query",
"Calculated fields", "calc"
     SIZE 95 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttProcs SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      cName   
cDesc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 21.71 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsOverrideHook AT ROW 1.38 COL 3 NO-LABEL WIDGET-ID 12
     BROWSE-4 AT ROW 2.86 COL 3 WIDGET-ID 200
     edDesc AT ROW 3.76 COL 44 NO-LABEL WIDGET-ID 16
     btnSelectCode AT ROW 11.76 COL 108.6 WIDGET-ID 54
     btnSaveCode AT ROW 11.76 COL 133 WIDGET-ID 58
     edCode AT ROW 12.95 COL 43.8 NO-LABEL WIDGET-ID 18
     btnCancel AT ROW 24.81 COL 141.8 WIDGET-ID 8
     "Description:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 2.91 COL 44 WIDGET-ID 20
     "Code:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.19 COL 44 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.6 BY 25.29 WIDGET-ID 100.


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
         TITLE              = "Select server-side template"
         HEIGHT             = 25.29
         WIDTH              = 156.6
         MAX-HEIGHT         = 25.29
         MAX-WIDTH          = 156.6
         VIRTUAL-HEIGHT     = 25.29
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
/* BROWSE-TAB BROWSE-4 rsOverrideHook DEFAULT-FRAME */
ASSIGN 
       BROWSE-4:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

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
ON END-ERROR OF C-Win /* Select server-side template */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Select server-side template */
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
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<cre.date>",STRING(TODAY))
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<cre.user>",OS-GETENV("username"))
           .
  ELSE
    ASSIGN edCode:SCREEN-VALUE = ""
           edDesc:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Done */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveCode C-Win
ON CHOOSE OF btnSaveCode IN FRAME DEFAULT-FRAME /* Save code as .p */
DO:
  
  DEF VAR bOk AS LOG NO-UNDO.
  DEF VAR cFileName AS CHAR NO-UNDO.
  DEF VAR cTemplateDir AS CHAR NO-UNDO.


  IF NOT AVAIL ttProcs THEN RETURN NO-APPLY.

  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Source files" "*.p"
                UPDATE bOk.

  IF cFileName NE "" THEN DO:
    IF NOT cFileName MATCHES "*.p" THEN cFileName = cFileName + ".p".
    bOk = NO.
    IF SEARCH(cFileName) NE ? THEN
      MESSAGE cFileName "already exists. Overwrite?"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE bOk.
    ELSE bOk = YES.
    IF bOk THEN DO:
      OUTPUT TO VALUE(cFileName).
      PUT UNFORMATTED edCode:SCREEN-VALUE.
      OUTPUT CLOSE.
    END.
  END.

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
                FILTERS "Tempates for " + ttProcs.cName ttProcs.cName + "*.p",
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
  DISPLAY rsOverrideHook edDesc edCode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rsOverrideHook BROWSE-4 edDesc btnSelectCode btnSaveCode edCode 
         btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
ASSIGN ttProcs.cCat = "hook"
       ttProcs.cName   = "CustomCreateProc"
       ttProcs.cDesc   = "Invoked on create of record."
                       + CHR(10) + "Reference to procedure from client is created by assigning the customCreateProc property"
                       + CHR(10) + "either for the fieldmap object, or - if that doesn't exist - for the browse object."         
                       + CHR(10) + "Typical usage is for assigning the key value based on some criteria"
                       + CHR(10) + "(find the next cust.num etc)."
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
       .
CREATE ttProcs.
ASSIGN ttProcs.cCat    = "override"
       ttProcs.cName   = "DisplayRecord"
       ttProcs.cDesc   = "Typical action to do before RUN SUPER:" 
                       + CHR(10) + " - enable/disable tools based on data in current row:"
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

  fillFromConfig("template\serverHookConfig.txt","hook").
  
  fillFromConfig("template\serverBlConfig.txt","bl").

  fillFromConfig("template\serverQueryConfig.txt","query").

  fillFromConfig("template\serverCalcConfig.txt","calc").

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
    ELSE IF cLine BEGINS "<cat>" THEN DO:
      icCat = TRIM(ENTRY(2,cLine,">")).
      IF AVAIL ttProcs THEN ttProcs.cCat = icCat.
    END. 
    ELSE IF AVAIL ttProcs THEN DO:
      IF cTag = "<desc>" THEN
        ttProcs.cDesc = ttProcs.cDesc + (IF ttProcs.cDesc NE "" THEN CHR(10) ELSE "") + cLine.
      ELSE IF cTag = "<code>" THEN
        ttProcs.cCode = ttProcs.cCode + (IF ttProcs.cCode NE "" THEN CHR(10) ELSE "") + cLine.
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

