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
  DEF VAR icCallBackProc    AS CHAR NO-UNDO.
  DEF VAR icFile            AS CHAR NO-UNDO.
&ELSE
  DEF INPUT PARAM icCallBackProc    AS CHAR NO-UNDO.
  DEF INPUT PARAM icFile            AS CHAR  NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOk           AS LOG    NO-UNDO.
DEF VAR cCallBackProc AS CHAR   NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.
DEF VAR bSuspend      AS LOG    NO-UNDO.
DEF VAR iInterval     AS INT    NO-UNDO.
DEF VAR hSocket       AS HANDLE NO-UNDO.
DEF VAR cPort         AS CHAR   NO-UNDO.
DEF VAR oStartInfo    AS System.Diagnostics.ProcessStartInfo NO-UNDO.
DEF VAR oProcess      AS System.Diagnostics.Process NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMethod C-Win 
FUNCTION setCallBackProc RETURNS LOGICAL
  ( INPUT icCallBackProc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPort C-Win 
FUNCTION setPort RETURNS LOGICAL
  ( INPUT icPort AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     "Com event client starts a new session running JBoxComEventServer.w" VIEW-AS TEXT
          SIZE 67 BY .62 AT ROW 1.95 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 68.4 BY 2.76.


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
         WIDTH              = 68.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         SHOW-IN-TASKBAR    = no
         MAX-BUTTON         = no
         RESIZE             = yes
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
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

  cCallBackProc = icCallBackProc.
  
  RUN InitWindow.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = YES.
  
/*  SUBSCRIBE TO "InvalidateHandle" IN hParent.*/
/*  SUBSCRIBE TO "SuspendJBoxTimer" ANYWHERE.  */

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
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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

DEF VAR cProwc      AS CHAR NO-UNDO.
DEF VAR cServer     AS CHAR NO-UNDO.

FILE-INFO:FILE-NAME = IF PROGRESS = "FULL" THEN SEARCH("prowin32.exe") ELSE SEARCH("prowc.exe").
cProwc = '"' + FILE-INFO:FULL-PATHNAME + '"'.
IF cProwc = ? THEN DO:
  APPLY "close" TO THIS-PROCEDURE.
  RETURN.
END.

cServer = IF PROGRESS = "FULL" THEN SEARCH("JBoxComEventServer.w") ELSE SEARCH("JBoxComEventServer.r").

IF cServer = ? THEN DO:
  MESSAGE "Com event server procedure is missing"
  VIEW-AS ALERT-BOX.
  APPLY "close" TO THIS-PROCEDURE.
  RETURN.
END.

IF cPort = "" THEN cPort = "2595". /* "32432". */

/* Courtesy Mike Fechner, "extreme desktop integration": */
oStartInfo = NEW System.Diagnostics.ProcessStartInfo().
oStartInfo:FileName = cProwc.
oStartInfo:Arguments = '-p ' + cServer + ' -wy -param "' + icFile + '" -T ' + SESSION:TEMP-DIR.
oStartInfo:WorkingDirectory = System.Environment:CurrentDirectory.
/* optionally obtain access to stdout of the process 
oStartInfo:RedirectStandardOutput = YES.
*/ 
oProcess = System.Diagnostics.Process:Start (oStartInfo).
oProcess:Exited:Subscribe("ProcessExitedHandler").
/*
OS-COMMAND NO-WAIT VALUE(cProwc + " -nosplash -debugalert -p " + cServer + " -wy -param " + icFile + "," + cPort).

PAUSE 3.

CREATE SOCKET hSocket NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN DO:
  bOk = hSocket:CONNECT("-H localhost -S " + cPort) NO-ERROR.
  IF NOT bOk THEN DO:
    MESSAGE "Failed to connect to socket server"
    VIEW-AS ALERT-BOX.
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.
  MESSAGE "hSocket ok" valid-handle(hsocket) skip "bOK" bOK
  VIEW-AS ALERT-BOX.
  hSocket:SET-READ-RESPONSE-PROCEDURE("WaitForAction").
END.
*/
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessExitedHandler C-Win
PROCEDURE ProcessExitedHandler:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM sender AS System.Object NO-UNDO.
DEF INPUT PARAM e AS System.EventArgs   NO-UNDO.


MESSAGE "ferdig"
VIEW-AS ALERT-BOX.
RUN VALUE(cCallBackProc) IN hParent NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  MESSAGE "Procedure " cCallBackProc " not defined in " hParent:FILE-NAME SKIP
          ERROR-STATUS:GET-MESSAGE(1)
          VIEW-AS ALERT-BOX ERROR.

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

bSuspend = ibSuspend.
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
/* OUTPUT TO .\clientcomm.txt APPEND.                                            */
/* PUT UNFORMATTED STRING(TIME,"HH:MM:SS") " " string(hSocket:CONNECTED()) SKIP. */
/* OUTPUT CLOSE.                                                                 */

IF NOT bSuspend THEN DO:
  RUN VALUE(cCallBackProc) IN hParent NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Procedure " cCallBackProc " not defined in " hParent:FILE-NAME SKIP
            ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX ERROR.
END.

APPLY "close" TO THIS-PROCEDURE.

/*
hSocket:CONNECT("-H localhost -S " + cPort) NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
  hSocket:SET-READ-RESPONSE-PROCEDURE("WaitForAction").
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMethod C-Win 
FUNCTION setCallBackProc RETURNS LOGICAL
  ( INPUT icCallBackProc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set the timer method 
    Notes:  
------------------------------------------------------------------------------*/

cCallBackProc = icCallBackProc.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPort C-Win 
FUNCTION setPort RETURNS LOGICAL
  ( INPUT icPort AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cPort = icPort.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

