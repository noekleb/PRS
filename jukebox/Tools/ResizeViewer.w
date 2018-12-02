&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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

/* Local Variable Definitions ---                                       */

DEF VAR ix                 AS INT NO-UNDO.
DEF VAR bOK                AS LOG NO-UNDO.
DEF VAR hBrowse            AS HANDLE NO-UNDO.
DEF VAR hParent            AS HANDLE NO-UNDO.
DEF VAR cInitResizeLogName AS CHAR NO-UNDO.

DEF STREAM strDbgResize.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwObject btnDebugWidget btnDebugResize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDebugResize 
     LABEL "Debug initial settings when starting a new window" 
     SIZE 52.8 BY 1.14.

DEFINE BUTTON btnDebugWidget 
     LABEL "Debug widget" 
     SIZE 21.8 BY 1.14.

DEFINE RECTANGLE rectBrwObject
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 105 BY 16.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDebugWidget AT ROW 17.76 COL 2.2
     btnDebugResize AT ROW 17.76 COL 24.2
     rectBrwObject AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.4 BY 18.14.


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
         TITLE              = "Resize Viewer"
         HEIGHT             = 17.95
         WIDTH              = 107.6
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 52.38
         VIRTUAL-WIDTH      = 320
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
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Resize Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Resize Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Resize Viewer */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDebugResize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDebugResize C-Win
ON CHOOSE OF btnDebugResize IN FRAME DEFAULT-FRAME /* Debug initial settings when starting a new window */
DO:
  DEF VAR iReturn  AS INT  NO-UNDO.
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hWidget"):BUFFER-VALUE NE ? THEN DO:
    cInitResizeLogName = SESSION:TEMP-DIR + "InitResizeLog.txt".
    RUN JboxAskForValue.w ("Log file name (blank for annoying messages only)",
                           "CHARACTER|x(80)",
                           INPUT-OUTPUT cInitResizeLogName,
                           OUTPUT iReturn).  

    IF iReturn = 2 THEN 
      DYNAMIC-FUNCTION("setDebugResize",YES).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDebugWidget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDebugWidget C-Win
ON CHOOSE OF btnDebugWidget IN FRAME DEFAULT-FRAME /* Debug widget */
DO:
  DEF VAR cLogName AS CHAR NO-UNDO.
  DEF VAR iReturn  AS INT  NO-UNDO.
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hWidget"):BUFFER-VALUE NE ? THEN DO:
    cLogName = SESSION:TEMP-DIR + "resizeLog_" + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cWidgetName"):BUFFER-VALUE + ".txt".
    RUN JboxAskForValue.w ("Log file name (blank for annoying messages only)",
                           "CHARACTER|x(80)",
                           INPUT-OUTPUT cLogName,
                           OUTPUT iReturn).  

    IF iReturn = 2 THEN
      DYNAMIC-FUNCTION("setWidgetWatch",hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hWidget"):BUFFER-VALUE,cLogName).
  END.
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
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  SESSION:SET-WAIT-STATE("general").

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.
  RUN InitWindow.
  SESSION:SET-WAIT-STATE("").
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebugResize C-Win 
PROCEDURE DebugResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceProc AS HANDLE NO-UNDO.

IF cInitResizeLogName = "" THEN RETURN.

OUTPUT STREAM strDbgResize TO VALUE(cInitResizeLogName) APPEND.
PUT STREAM strDbgResize UNFORMATTED STRING(TODAY) " " STRING(TIME,"hh:mm:ss") " ** " PROGRAM-NAME(2) " " PROGRAM-NAME(3) SKIP(1).
OUTPUT STREAM strDbgResize CLOSE.

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
  ENABLE rectBrwObject btnDebugWidget btnDebugResize 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
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
DO WITH FRAM {&FRAME-NAME}:

  SUBSCRIBE TO "DebugResize" ANYWHERE.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                                   rectBrwObject:HANDLE,
                                   1000,
                                   "",
                                   "temp-table"
                                   + ";cSetting|CHARACTER|x(35)||Rule"
                                   + ";cSourceProc|CHARACTER|x(40)||Procedure"
                                   + ";cFrameName|CHARACTER|x(25)||Frame"
                                   + ";cWidgetName|CHARACTER|x(25)||Widget"
                                   + ";hWidget|HANDLE|>>>>>9||Widget hdl"
                                   + ";hFrame|HANDLE|>>>>>9||Frame hdl"
                                   ,
                                   "where false",
                                   "").
  hBrowse:NAME = "brwResize".
  DYNAMIC-FUNCTION("NewMenuBand",hBrowse,"MultiSortBrowse;Sort on multiple columns,Filter,Excel","").

  DYNAMIC-FUNCTION("FillObjectTable" IN hParent,"ttResize",hBrowse:QUERY:GET-BUFFER-HANDLE(1)).
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,550,250,0,0).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowse:QUERY:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().

DYNAMIC-FUNCTION("FillObjectTable" IN hParent,"ttResize",hBrowse:QUERY:GET-BUFFER-HANDLE(1)).
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

