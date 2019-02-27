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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1
&ENDIF  

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE Sheet1   AS INTEGER   NO-UNDO.
DEFINE VARIABLE Sheet2   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDDEmsg  AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EDITOR-1 EDITOR-2 Startprog FILL-IN-1 ~
BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 EDITOR-2 Startprog FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL "Send" 
     SIZE 15 BY 1.15.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "This routine should be invoked by a batch script, f.ex C:~\progress~\10.0b~\bin~\prowin32.exe -T ~"%TEMP%~" -p JBoxJlwDDENotify.w -param ~"start|c:~\prowrk~\Sports2000~\Start Sport2000.lnk~;dde|customer|1~"" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 3.12 NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER INITIAL "In the startup procedure of Sports2000 invoke the DDE server: RUN JBoxJlwDDEserver.w PERSIST SET hDDEserver (~"~",~"DDEevent~"). Where DDEevent is the call-back procedure" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 2.62 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "DDE message" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE Startprog AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\prowrk~\sports2000~\Start Sports2000.lnk" 
     LABEL "Start if not active" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     EDITOR-1 AT ROW 1.96 COL 6 NO-LABEL
     EDITOR-2 AT ROW 5.54 COL 6 NO-LABEL
     Startprog AT ROW 8.85 COL 23 COLON-ALIGNED
     FILL-IN-1 AT ROW 10.92 COL 23 COLON-ALIGNED
     BUTTON-2 AT ROW 12.19 COL 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.8 BY 12.91.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 12.92
         WIDTH              = 81.86
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 93
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 93
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       EDITOR-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       EDITOR-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Send */
DO:
  DDE SEND Sheet1 SOURCE trim(fill-in-1:SCREEN-VALUE) ITEM "R1C1" NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    cDDEmsg = trim(fill-in-1:SCREEN-VALUE).
    RUN StartApp.
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
  
  RUN InitDDE.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  &ELSE
    RUN NotifyApp.
    QUIT.
  &ENDIF
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
  DISPLAY EDITOR-1 EDITOR-2 Startprog FILL-IN-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE EDITOR-1 EDITOR-2 Startprog FILL-IN-1 BUTTON-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitDDE C-Win 
PROCEDURE InitDDE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR hFrame AS HANDLE NO-UNDO.
 hFrame = FRAME default-frame:HANDLE.
 DDE INITIATE Sheet1 FRAME hFrame APPLICATION "prowin32" TOPIC "ProgressTopic" NO-ERROR.

/*  MESSAGE ERROR-STATUS:ERROR SKIP      */
/*          ERROR-STATUS:GET-MESSAGE(1)  */
/*    VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*  DDE SEND Sheet1 SOURCE "James Williams" ITEM "R1C1".  */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NotifyApp C-Win 
PROCEDURE NotifyApp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(SESSION:PARAMETER,";"):
  IF ENTRY(1,ENTRY(ix,SESSION:PARAMETER,";"),"=") = "dde" THEN DO:
    cDDEmsg = ENTRY(ix,SESSION:PARAMETER,";").
    DDE SEND Sheet1 SOURCE cDDEmsg ITEM "R1C1" NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      RUN StartApp.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartApp C-Win 
PROCEDURE StartApp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix   AS INT  NO-UNDO.
DEF VAR cApp AS CHAR NO-UNDO.
DO ix = 1 TO NUM-ENTRIES(SESSION:PARAMETER,";"):
  IF ENTRY(1,ENTRY(ix,SESSION:PARAMETER,";"),"=") = "start" THEN 
    cApp = ENTRY(2,ENTRY(ix,SESSION:PARAMETER,";"),"=").
END.
IF cApp = "" THEN
  cApp = StartProg:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

IF SEARCH(cApp) NE ? THEN DO:
  OUTPUT TO VALUE(SESSION:TEMP-DIR + "ddemsg.txt").
  PUT UNFORMATTED cDDEmsg SKIP.
  OUTPUT CLOSE.
  cApp = '"' + cApp + '"'. /* + " -nosplash".  + ' -param ' + cDDEmsg. */
  OS-COMMAND NO-WAIT VALUE(cApp).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

