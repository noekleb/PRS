&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports113        PROGRESS
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



/*** Start instance property definitions for JBoxBrowse object oBrwDepartment ***/
/* DEF VAR oBrwDepartment AS JBoxBrowse NO-UNDO.  */
DEF TEMP-TABLE Department
    FIELD DeptCode AS character
    FIELD DeptName AS character
    FIELD EmpNum AS integer
    FIELD Fullname AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .

FUNCTION getBuffersAndFieldsBrwDepartment RETURNS CHARACTER():
  RETURN
    'Department'
     + ';DeptCode'
     + ';DeptName'
  + ',Employee'
     + ';EmpNum'
     + ';+Fullname|CHARACTER||jb_calc(LastName concatcomma FirstName)'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwDepartment RETURNS CHARACTER():
  RETURN 'EACH Employee OF Department NO-LOCK'.
END FUNCTION.
/*
DEF VAR oFmDepartment AS JBoxFieldMap NO-UNDO.

DEF VAR oTbDepartment AS JBoxToolbar NO-UNDO.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwDepartment

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Department

/* Definitions for BROWSE BrwDepartment                                 */
&Scoped-define FIELDS-IN-QUERY-BrwDepartment Department.DeptCode ~
Department.DeptName Department.EmpNum Department.Fullname 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwDepartment Department.DeptCode 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwDepartment Department
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwDepartment Department
&Scoped-define QUERY-STRING-BrwDepartment FOR EACH Department NO-LOCK, ~
    EACH Employee OF Department NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwDepartment OPEN QUERY BrwDepartment FOR EACH Department NO-LOCK, ~
    EACH Employee OF Department NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwDepartment Department
&Scoped-define FIRST-TABLE-IN-QUERY-BrwDepartment Department


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbDepartment BrwDepartment DeptCode DeptName ~
EmpNum 
&Scoped-Define DISPLAYED-OBJECTS DeptCode DeptName EmpNum 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE DeptCode AS CHARACTER FORMAT "x(3)" 
     LABEL "Dept Code" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Please enter the Dept Code.".

DEFINE VARIABLE DeptName AS CHARACTER FORMAT "x(15)" 
     LABEL "Dept Name" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Please enter the Dept Name".

DEFINE VARIABLE EmpNum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Emp No" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Emp Num".

DEFINE RECTANGLE tbDepartment
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11.8 BY .91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwDepartment FOR 
      Department SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwDepartment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwDepartment C-Win _STRUCTURED
  QUERY BrwDepartment NO-LOCK DISPLAY
      Department.DeptCode COLUMN-LABEL "Dept Code" FORMAT "x(3)":U
      Department.DeptName COLUMN-LABEL "Dept Name" FORMAT "x(15)":U
      Department.EmpNum COLUMN-LABEL "Emp No" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Department.Fullname COLUMN-LABEL "Name" FORMAT "X(40)":U
  ENABLE
      Department.DeptCode HELP "Please enter the Dept Code."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93 BY 9.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrwDepartment AT ROW 2.86 COL 2 WIDGET-ID 200
     DeptCode AT ROW 12.67 COL 18 COLON-ALIGNED
     DeptName AT ROW 13.67 COL 18 COLON-ALIGNED
     EmpNum AT ROW 14.67 COL 18 COLON-ALIGNED
     tbDepartment AT ROW 1.24 COL 1.6 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 95.2 BY 16.33 WIDGET-ID 100.


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
         HEIGHT             = 16.33
         WIDTH              = 95.2
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
/* BROWSE-TAB BrwDepartment tbDepartment DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 16.33
       FRAME DEFAULT-FRAME:WIDTH            = 95.2.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwDepartment
/* Query rebuild information for BROWSE BrwDepartment
     _TblList          = "sports113.Department"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Department.DeptCode
"Department.DeptCode" "Dept Code" "x(3)" "character" ? ? ? ? ? ? yes "Please enter the Dept Code." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Department.DeptName
"Department.DeptName" "Dept Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the Dept Name" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Department.EmpNum
"Department.EmpNum" "Emp No" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the Emp Num" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Department.Fullname" "Name" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "50.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwDepartment */
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


&Scoped-define BROWSE-NAME BrwDepartment
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
  DISPLAY DeptCode DeptName EmpNum 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbDepartment BrwDepartment DeptCode DeptName EmpNum 
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

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
    /*
  oBrwDepartment = NEW JBoxBrowse(brwDepartment:HANDLE).

  oBrwDepartment:OpenQuery().

  oFmDepartment = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmDepartment:updateFields = 'DeptCode,DeptName'.
  oFmDepartment:displayFields = 'EmpNum'.
  oFmDepartment:primaryKeyFields = 'DeptCode'.

  oFmDepartment:BROWSE-OBJECT = oBrwDepartment.
  oTbDepartment = NEW JBoxToolbar(tbDepartment:HANDLE).
  oTbDepartment:AddToolGroup('New,Edit,Undo,Save,Filter,Excel').

  oBrwDepartment:TOOLBAR-OBJECT = oTbDepartment.
  oFmDepartment:TOOLBAR-OBJECT = oTbDepartment.
  */
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

