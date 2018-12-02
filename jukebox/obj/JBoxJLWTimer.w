&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icMethod    AS CHAR NO-UNDO.
  DEF VAR iiInterval  AS INT  NO-UNDO INIT 3000.
&ELSE
  DEF INPUT PARAM icMethod    AS CHAR NO-UNDO.
  DEF INPUT PARAM iiInterval  AS INT  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOk            AS LOG    NO-UNDO.
DEF VAR cMethod        AS CHAR   NO-UNDO.
DEF VAR hParent        AS HANDLE NO-UNDO.
DEF VAR bSuspend       AS LOG    NO-UNDO.
DEF VAR bFirstSuspend  AS LOG    NO-UNDO.
DEF VAR iInterval      AS INT    NO-UNDO.
DEF VAR hiTimer        AS INT    NO-UNDO.
DEF VAR hiSuccess      AS INT    NO-UNDO.
DEF VAR bNamedSuspend  AS LOG    NO-UNDO.
DEF VAR cTimerName     AS CHAR   NO-UNDO.

/* {CVcontrols.i} */
{JukeBoxTimer.i}
DEF VAR hControlsLibrary AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmTimer

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setInterval C-Win 
FUNCTION setInterval RETURNS LOGICAL
  ( INPUT iiInterval AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMethod C-Win 
FUNCTION setMethod RETURNS LOGICAL
  ( INPUT icMethod AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTimerName C-Win 
FUNCTION setTimerName RETURNS LOGICAL
  ( INPUT icTimerName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmTimer
     "JLW timer proc" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.95 COL 6
    WITH 1 DOWN KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 27.6 BY 2.76.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 2.76
         WIDTH              = 27.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         MESSAGE-AREA       = no
         SENSITIVE          = no.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmTimer
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frmTimer:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmTimer
/* Query rebuild information for FRAME frmTimer
     _Query            is NOT OPENED
*/  /* FRAME frmTimer */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frmTimer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmTimer C-Win
ON A OF FRAME frmTimer
DO:
  RUN WaitForAction.
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
  hParent = SOURCE-PROCEDURE.

  cMethod = icMethod.
  
/*   RUN Controls.p PERSISTENT SET hControlsLibrary.  */

  setInterval(iiInterval).

  RUN TimerCreate(FRAME frmTimer:HWND,iInterval,OUTPUT hiTimer).
  RUN TimerEnable(hiTimer,OUTPUT hiSuccess).

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = YES.
  
  SUBSCRIBE TO "InvalidateHandle"  IN hParent.
  SUBSCRIBE TO "SuspendJBoxTimer"  ANYWHERE.
  SUBSCRIBE TO "SuspendNamedTimer" ANYWHERE.

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
  VIEW FRAME frmTimer IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmTimer}
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
DEF INPUT PARAM ihProcHandle AS HANDLE NO-UNDO.

IF ihProcHandle = hParent THEN
  APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SuspendJBoxTimer C-Win 
PROCEDURE SuspendJBoxTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibSuspend AS LOG NO-UNDO.

IF cTimerName NE "" THEN RETURN.

bSuspend = ibSuspend.

IF ibSuspend THEN DO:
  bFirstSuspend = YES.
  RUN TimerDisable(hiTimer,OUTPUT hiSuccess).
END. 
ELSE
  RUN TimerEnable(hiTimer,OUTPUT hiSuccess).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SuspendNamedTimer C-Win 
PROCEDURE SuspendNamedTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icTimerName AS CHAR NO-UNDO.
DEF INPUT PARAM ibSuspend   AS LOG  NO-UNDO.

IF icTimerName NE cTimerName THEN RETURN.

bNamedSuspend = ibSuspend.
IF ibSuspend THEN 
  RUN TimerDisable(hiTimer,OUTPUT hiSuccess).
ELSE
  RUN TimerEnable(hiTimer,OUTPUT hiSuccess).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WaitForAction C-Win 
PROCEDURE WaitForAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF bNamedSuspend THEN RETURN.

IF NOT bSuspend THEN DO:
  RUN VALUE(cMethod) IN hParent NO-ERROR.
/*   MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP SKIP         */
/*           ERROR-STATUS:ERROR SKIP                       */
/*           "valid-handle(hparent)" VALID-HANDLE(hParent) */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
/*   IF ERROR-STATUS:ERROR THEN */
/*     MESSAGE "Procedure " cMethod " not defined in " hParent:FILE-NAME SKIP  */
/*             ERROR-STATUS:GET-MESSAGE(1)                                     */
/*             VIEW-AS ALERT-BOX ERROR.                                        */
/*     RETURN. */
END.
ELSE IF bFirstSuspend THEN DO:
  bFirstSuspend = NO.
  RUN setFocus IN hParent NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setInterval C-Win 
FUNCTION setInterval RETURNS LOGICAL
  ( INPUT iiInterval AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Set the timer intervall (in milliseconds)
    Notes:  
------------------------------------------------------------------------------*/
IF iiInterval NE 0 THEN
  iInterval = MAX(20,iiInterval).
ELSE
  iInterval = 5000.

IF hiTimer NE 0 THEN
  RUN TimerSetTimeInterval(hiTimer,iInterval,OUTPUT hiSuccess).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMethod C-Win 
FUNCTION setMethod RETURNS LOGICAL
  ( INPUT icMethod AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set the timer method 
    Notes:  
------------------------------------------------------------------------------*/

cMethod = icMethod.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTimerName C-Win 
FUNCTION setTimerName RETURNS LOGICAL
  ( INPUT icTimerName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cTimerName = icTimerName.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

