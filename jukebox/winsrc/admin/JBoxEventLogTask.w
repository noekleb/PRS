&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports115        PROGRESS
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


/*** Start instance property definitions for JBoxBrowse object oBrwJBoxEventLog ***/
DEF VAR oBrwJBoxEventLog AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxEventLog
    FIELD cEventLogTypeText AS character
    FIELD cUserName AS character
    FIELD cEventStatusText AS character
    FIELD dEventDate AS date
    FIELD cEventLogType AS character
    FIELD cEventText AS character
    FIELD cResponseText AS character
    FIELD cEntityTable AS character
    FIELD cEntityIdFields AS character
    FIELD cEntityId AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowIdent4 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3 RowIdent4
    .
DEF BUFFER v_JBoxEventLog FOR JBoxEventLog.


FUNCTION getBuffersAndFieldsBrwJBoxEventLog RETURNS CHARACTER():
  RETURN
    'JBoxEventLog'
     + ';dEventDate'
     + ';cEventLogType'
     + ';cEventText'
     + ';cResponseText'
     + ';cEntityTable'
     + ';cEntityIdFields'
     + ';cEntityId'
  + ',JBoxEventLogStatus'
     + ';cEventStatusText'
  + ',JBoxEventLogType'
     + ';cEventLogTypeText'
  + ',JBoxUser'
     + ';cUserName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxEventLog RETURNS CHARACTER():
  RETURN 'EACH JBoxEventLogStatus OF JBoxEventLog NO-LOCK,EACH JBoxEventLogType OF JBoxEventLog NO-LOCK,EACH JBoxUser OF JBoxEventLogType NO-LOCK'.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

&Scoped-define WIDGETID-FILE-NAME 

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwJBoxEventLog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES JBoxEventLog

/* Definitions for BROWSE BrwJBoxEventLog                               */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxEventLog ~
JBoxEventLog.cEventLogTypeText JBoxEventLog.cUserName ~
JBoxEventLog.cEventStatusText JBoxEventLog.dEventDate ~
JBoxEventLog.cEventLogType JBoxEventLog.cEventText ~
JBoxEventLog.cResponseText JBoxEventLog.cEntityTable ~
JBoxEventLog.cEntityIdFields JBoxEventLog.cEntityId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxEventLog ~
JBoxEventLog.cEventLogTypeText 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwJBoxEventLog JBoxEventLog
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwJBoxEventLog JBoxEventLog
&Scoped-define QUERY-STRING-BrwJBoxEventLog FOR EACH JBoxEventLog NO-LOCK, ~
    EACH JBoxEventLogStatus OF JBoxEventLog NO-LOCK, ~
    EACH JBoxEventLogType OF JBoxEventLog NO-LOCK, ~
    EACH JBoxUser OF JBoxEventLogType NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxEventLog OPEN QUERY BrwJBoxEventLog FOR EACH JBoxEventLog NO-LOCK, ~
    EACH JBoxEventLogStatus OF JBoxEventLog NO-LOCK, ~
    EACH JBoxEventLogType OF JBoxEventLog NO-LOCK, ~
    EACH JBoxUser OF JBoxEventLogType NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxEventLog JBoxEventLog
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxEventLog JBoxEventLog


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwJBoxEventLog 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwJBoxEventLog FOR 
      JBoxEventLog SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwJBoxEventLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxEventLog C-Win _STRUCTURED
  QUERY BrwJBoxEventLog NO-LOCK DISPLAY
      JBoxEventLog.cEventLogTypeText FORMAT "x(40)":U
      JBoxEventLog.cUserName FORMAT "X(40)":U
      JBoxEventLog.cEventStatusText FORMAT "x(30)":U
      JBoxEventLog.dEventDate FORMAT "99/99/9999":U
      JBoxEventLog.cEventLogType FORMAT "x(8)":U
      JBoxEventLog.cEventText FORMAT "x(40)":U
      JBoxEventLog.cResponseText FORMAT "x(40)":U
      JBoxEventLog.cEntityTable FORMAT "x(30)":U
      JBoxEventLog.cEntityIdFields FORMAT "x(30)":U
      JBoxEventLog.cEntityId FORMAT "x(20)":U
  ENABLE
      JBoxEventLog.cEventLogTypeText
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 125 BY 13.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrwJBoxEventLog AT ROW 3.38 COL 2 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 126.6 BY 16.38 WIDGET-ID 100.


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
         HEIGHT             = 16.38
         WIDTH              = 126.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 126.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 126.6
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
/* BROWSE-TAB BrwJBoxEventLog 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 16.38
       FRAME DEFAULT-FRAME:WIDTH            = 126.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxEventLog
/* Query rebuild information for BROWSE BrwJBoxEventLog
     _TblList          = "sports115.JBoxEventLog"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > JBoxEventLog.cEventLogTypeText
"JBoxEventLog.cEventLogTypeText" "Event type desc" "x(40)" "character" ? ? ? ? ? ? yes "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > JBoxEventLog.cUserName
"JBoxEventLog.cUserName" "Navn" "X(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > JBoxEventLog.cEventStatusText
"JBoxEventLog.cEventStatusText" "Status text" "x(30)" "character" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > JBoxEventLog.dEventDate
"JBoxEventLog.dEventDate" "Event date" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > JBoxEventLog.cEventLogType
"JBoxEventLog.cEventLogType" "Event type" "x(8)" "character" ? ? ? ? ? ? no "" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > JBoxEventLog.cEventText
"JBoxEventLog.cEventText" "Event text" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > JBoxEventLog.cResponseText
"JBoxEventLog.cResponseText" "Response text" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > JBoxEventLog.cEntityTable
"JBoxEventLog.cEntityTable" "Entity table" "x(30)" "character" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > JBoxEventLog.cEntityIdFields
"JBoxEventLog.cEntityIdFields" "Entity id fields" "x(30)" "character" ? ? ? ? ? ? no "Comma-separated list of field names for primary key" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > JBoxEventLog.cEntityId
"JBoxEventLog.cEntityId" "Entity id" "x(20)" "character" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxEventLog */
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


&Scoped-define BROWSE-NAME BrwJBoxEventLog
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
/*{incl/conttrigg.i oBrw<>:BROWSE-HANDLE} */

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
  ENABLE BrwJBoxEventLog 
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

  oBrwJBoxEventLog = NEW JBoxBrowse(brwJBoxEventLog:HANDLE).

END.
oBrwJBoxEventLog:OpenQuery().


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

