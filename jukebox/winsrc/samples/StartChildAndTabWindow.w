&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

DEF VAR hChildWin   AS HANDLE NO-UNDO.
DEF VAR hChildTab   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edStartChildWindow btnStartChildWindow ~
tbAlwaysNewChildWin btnStartTabWindow edStartTabWindow tbAlwaysNewTabWin ~
tbKeepTabOpen 
&Scoped-Define DISPLAYED-OBJECTS edStartChildWindow tbAlwaysNewChildWin ~
edStartTabWindow tbAlwaysNewTabWin tbKeepTabOpen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnStartChildWindow 
     LABEL "StartChildWindow" 
     SIZE 33 BY 1.14.

DEFINE BUTTON btnStartTabWindow 
     LABEL "StartTabWindow" 
     SIZE 32 BY 1.14.

DEFINE VARIABLE edStartChildWindow AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 80 BY 4.86 NO-UNDO.

DEFINE VARIABLE edStartTabWindow AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 80 BY 4.86 NO-UNDO.

DEFINE VARIABLE tbAlwaysNewChildWin AS LOGICAL INITIAL no 
     LABEL "Always new child window" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tbAlwaysNewTabWin AS LOGICAL INITIAL no 
     LABEL "Always new tab window" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tbKeepTabOpen AS LOGICAL INITIAL no 
     LABEL "Keep tab open" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     edStartChildWindow AT ROW 2.62 COL 42 NO-LABEL WIDGET-ID 10
     btnStartChildWindow AT ROW 2.67 COL 7 WIDGET-ID 4
     tbAlwaysNewChildWin AT ROW 4.86 COL 8 WIDGET-ID 18
     btnStartTabWindow AT ROW 8.52 COL 7 WIDGET-ID 6
     edStartTabWindow AT ROW 8.52 COL 42 NO-LABEL WIDGET-ID 8
     tbAlwaysNewTabWin AT ROW 10.29 COL 8 WIDGET-ID 16
     tbKeepTabOpen AT ROW 11.43 COL 8 WIDGET-ID 20
     "If the setParent(ihParent) function exists in the child win it will" VIEW-AS TEXT
          SIZE 71 BY .62 AT ROW 14.33 COL 7 WIDGET-ID 12
     "be invoked before InitializeObject" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 14.95 COL 7 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 122.8 BY 14.76 WIDGET-ID 100.


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
         HEIGHT             = 14.76
         WIDTH              = 122.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 122.8
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 122.8
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 14.76
       FRAME DEFAULT-FRAME:WIDTH            = 122.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME btnStartChildWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartChildWindow C-Win
ON CHOOSE OF btnStartChildWindow IN FRAME DEFAULT-FRAME /* StartChildWindow */
DO:
  PUBLISH "StartChildWindow" ("Customer.w",
                              "Customer win title",
                              THIS-PROCEDURE, /* parent */
                              tbAlwaysNewChildWin:CHECKED, /* new instance */
                              OUTPUT hChildWin).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartTabWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartTabWindow C-Win
ON CHOOSE OF btnStartTabWindow IN FRAME DEFAULT-FRAME /* StartTabWindow */
DO:
  PUBLISH "StartTabWindow" ("Customer.w",
                            "Customer tab title", /* used if program not defined in menu */
                            THIS-PROCEDURE, /* parent */
                            tbAlwaysNewTabWin:CHECKED,    /* Multiple instances */
                            tbKeepTabOpen:CHECKED,        /* keep the tab open after close of this window */
                            OUTPUT hChildTab).
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
  DISPLAY edStartChildWindow tbAlwaysNewChildWin edStartTabWindow 
          tbAlwaysNewTabWin tbKeepTabOpen 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE edStartChildWindow btnStartChildWindow tbAlwaysNewChildWin 
         btnStartTabWindow edStartTabWindow tbAlwaysNewTabWin tbKeepTabOpen 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeChild C-Win 
PROCEDURE InitializeChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihChildWin AS HANDLE NO-UNDO.

MESSAGE PROGRAM-NAME(1) SKIP
        "Here you can make a call to the child or child tab window " ihChildWin:FILE-NAME SKIP
        "before it is initialized"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
  ASSIGN edStartChildWindow:SCREEN-VALUE = 
         'PUBLISH "StartChildWindow" ("Customer.w",' + CHR(10)
       + '                         "Customer win title",' + CHR(10)
       + '                         THIS-PROCEDURE, /* parent */' + CHR(10)
       + '                         tbAlwaysNewChildWin:CHECKED,  /* new instance */' + CHR(10)
       + '                        OUTPUT hChildWin)'
         edStartTabWindow:SCREEN-VALUE =
         'PUBLISH "StartTabWindow" ("Customer.w",' + CHR(10)
       + '                     "Customer tab title", /* used if program not defined in menu */' + CHR(10)
       + '                     THIS-PROCEDURE, /* parent */' + CHR(10)
       + '                     tbAlwaysNewTabWin:CHECKED, /* (always) new instance */' + CHR(10)
       + '                     tbKeepTabOpen:CHECKED,     /* keep the tab open after close of this window */' + CHR(10)
       + '                     OUTPUT hChildTab).'
         .
END.

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "edStartChildWindow,edStartTabWindow").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "edStartChildWindow,edStartTabWindow").

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,500,0,0).

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

