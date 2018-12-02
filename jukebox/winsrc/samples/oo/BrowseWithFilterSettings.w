&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports117        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwJBoxMenu ***/
DEF VAR oBrwJBoxMenu AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxMenu
    FIELD bConfigurable AS logical
    FIELD bMultiple AS logical
    FIELD cAccelerator AS character
    FIELD cCreatedBy AS character
    FIELD cFontStyle AS character
    FIELD cImage AS character
    FIELD cLaunch AS character
    FIELD cLaunchType AS character
    FIELD cMenuLabel AS character
    FIELD cMenuNumber AS character
    FIELD cMenuTooltip AS character
    FIELD cMenuType AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_JBoxMenu FOR TEMP-TABLE JBoxMenu.


FUNCTION getBuffersAndFieldsBrwJBoxMenu RETURNS CHARACTER():
  RETURN
    'JBoxMenu'
     + ';bConfigurable'
     + ';bMultiple'
     + ';cAccelerator'
     + ';cCreatedBy'
     + ';cFontStyle'
     + ';cImage'
     + ';cLaunch'
     + ';cLaunchType'
     + ';cMenuLabel'
     + ';cMenuNumber'
     + ';cMenuTooltip'
     + ';cMenuType'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxMenu RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
DEF VAR otbJBoxMenu AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

&Scoped-define WIDGETID-FILE-NAME 

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwJBoxMenu

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES JBoxMenu

/* Definitions for BROWSE BrwJBoxMenu                                   */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxMenu JBoxMenu.bConfigurable ~
JBoxMenu.bMultiple JBoxMenu.cAccelerator JBoxMenu.cCreatedBy ~
JBoxMenu.cFontStyle JBoxMenu.cImage JBoxMenu.cLaunch JBoxMenu.cLaunchType ~
JBoxMenu.cMenuLabel JBoxMenu.cMenuNumber JBoxMenu.cMenuTooltip ~
JBoxMenu.cMenuType 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxMenu JBoxMenu.bConfigurable 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwJBoxMenu JBoxMenu
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwJBoxMenu JBoxMenu
&Scoped-define QUERY-STRING-BrwJBoxMenu FOR EACH JBoxMenu NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxMenu OPEN QUERY BrwJBoxMenu FOR EACH JBoxMenu NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxMenu JBoxMenu
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxMenu JBoxMenu


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbJBoxMenu filter_tbJBoxMenu BrwJBoxMenu 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON filter_tbJBoxMenu 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 6 BY 1.52 TOOLTIP "Filter (CTRL-F)".

DEFINE RECTANGLE tbJBoxMenu
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 98 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwJBoxMenu FOR 
      JBoxMenu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwJBoxMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxMenu C-Win _STRUCTURED
  QUERY BrwJBoxMenu NO-LOCK DISPLAY
      JBoxMenu.bConfigurable FORMAT "Configurable/Not,configurable":U
      JBoxMenu.bMultiple FORMAT "Multiple/Single":U
      JBoxMenu.cAccelerator FORMAT "x(8)":U
      JBoxMenu.cCreatedBy FORMAT "X(8)":U
      JBoxMenu.cFontStyle FORMAT "x(30)":U
      JBoxMenu.cImage FORMAT "x(30)":U
      JBoxMenu.cLaunch FORMAT "x(40)":U
      JBoxMenu.cLaunchType FORMAT "x(20)":U
      JBoxMenu.cMenuLabel FORMAT "x(40)":U
      JBoxMenu.cMenuNumber FORMAT "xxx":U
      JBoxMenu.cMenuTooltip FORMAT "x(40)":U
      JBoxMenu.cMenuType FORMAT "x(12)":U
  ENABLE
      JBoxMenu.bConfigurable
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 13.57 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     filter_tbJBoxMenu AT ROW 1.33 COL 4.2 WIDGET-ID 4
     BrwJBoxMenu AT ROW 3.14 COL 3 WIDGET-ID 200
     tbJBoxMenu AT ROW 1.24 COL 4 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 101.8 BY 16.43 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Insert window title>"
         HEIGHT             = 16.43
         WIDTH              = 101.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BrwJBoxMenu filter_tbJBoxMenu DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 16.43
       FRAME DEFAULT-FRAME:WIDTH            = 101.8.

ASSIGN 
       tbJBoxMenu:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "filter;Filtermaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxMenu
/* Query rebuild information for BROWSE BrwJBoxMenu
     _TblList          = "sports117.JBoxMenu"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > JBoxMenu.bConfigurable
"JBoxMenu.bConfigurable" "Configurable" "Configurable/Not,configurable" "logical" ? ? ? ? ? ? yes "" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > JBoxMenu.bMultiple
"JBoxMenu.bMultiple" "Multiple" "Multiple/Single" "logical" ? ? ? ? ? ? no "Allow multiple instances of program" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > JBoxMenu.cAccelerator
"JBoxMenu.cAccelerator" "Accelerator" "x(8)" "character" ? ? ? ? ? ? no "" no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > JBoxMenu.cCreatedBy
"JBoxMenu.cCreatedBy" "Av" "X(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > JBoxMenu.cFontStyle
"JBoxMenu.cFontStyle" "Font style" "x(30)" "character" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > JBoxMenu.cImage
"JBoxMenu.cImage" "Image" "x(30)" "character" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > JBoxMenu.cLaunch
"JBoxMenu.cLaunch" "Launch" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > JBoxMenu.cLaunchType
"JBoxMenu.cLaunchType" "Launch type" "x(20)" "character" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > JBoxMenu.cMenuLabel
"JBoxMenu.cMenuLabel" "Menu label" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > JBoxMenu.cMenuNumber
"JBoxMenu.cMenuNumber" "Menu num" "xxx" "character" ? ? ? ? ? ? no "Set to enable go-to menu function" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > JBoxMenu.cMenuTooltip
"JBoxMenu.cMenuTooltip" "Tooltip" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > JBoxMenu.cMenuType
"JBoxMenu.cMenuType" "Menu type" "x(12)" "character" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxMenu */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwJBoxMenu
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}
{incl/conttrigg.i oBrwJBoxMenu:BROWSE-HANDLE}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &IF DEFINED(UIB_is_Running) = 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    SESSION:SET-WAIT-STATE("").
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
    ELSE 
    &ENDIF
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.  
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
  ENABLE tbJBoxMenu filter_tbJBoxMenu BrwJBoxMenu 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:

  oBrwJBoxMenu = NEW JBoxBrowse(brwJBoxMenu:HANDLE).

  otbJBoxMenu = NEW JBoxToolbar(tbJBoxMenu:HANDLE).

  oBrwJBoxMenu:TOOLBAR-OBJECT = otbJBoxMenu.
END.
oBrwJBoxMenu:OpenQuery().


oContainer:initResize().

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

