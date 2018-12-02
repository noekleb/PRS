&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
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
DEF VAR hParent         AS HANDLE NO-UNDO.
DEF VAR iWebBrowser     AS INT    NO-UNDO.
DEF VAR iSuccess        AS INT    NO-UNDO.
DEF VAR cNavigateAction AS CHAR   NO-UNDO.

{JukeBoxWebBrowser.i}
DEF VAR hControlsLibrary    AS HANDLE NO-UNDO.

DEF STREAM strmFile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frWeb

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandleList C-Win 
FUNCTION getFrameHandleList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NavigateToURL C-Win 
FUNCTION NavigateToURL RETURNS LOGICAL
  ( INPUT icUrl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNavigateAction C-Win 
FUNCTION setNavigateAction RETURNS LOGICAL
  ( INPUT icNavigateAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewText C-Win 
FUNCTION ViewText RETURNS LOGICAL
  ( INPUT icText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frWeb
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 1 ROW 1
         SIZE 26 BY 4.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 4.71
         WIDTH              = 26
         MAX-HEIGHT         = 46.14
         MAX-WIDTH          = 228.6
         VIRTUAL-HEIGHT     = 46.14
         VIRTUAL-WIDTH      = 228.6
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frWeb
   FRAME-NAME UNDERLINE                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frWeb
/* Query rebuild information for FRAME frWeb
     _Query            is NOT OPENED
*/  /* FRAME frWeb */
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


&Scoped-define SELF-NAME frWeb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frWeb C-Win
ON A OF FRAME frWeb
DO:
  DEF VAR iMessage AS INT.
  DEF VAR cString  AS CHAR.
  DEF VAR cString2 AS CHAR.
  DEF VAR cString3 AS CHAR.
  DEF VAR cString4 AS CHAR.
  DEF VAR iParam1  AS INT.
  DEF VAR iParam2  AS INT.
  DEF VAR iSuccess AS INT.

  ASSIGN cString = FILL(" ",255)
         cString2 = FILL(" ",255)
         cString3 = FILL(" ",255)
         cString4 = FILL(" ",255).

  RUN WebBrowserGetMessage(iWebBrowser, 
                           OUTPUT iMessage,
                           OUTPUT cString,
                           OUTPUT cString2,
                           OUTPUT cString3,
                           OUTPUT cString4,
                           OUTPUT iParam1,
                           OUTPUT iParam2,
                           OUTPUT iSuccess).
/*   IF iMessage NE 10 AND iMessage NE 2 THEN */
/*     MESSAGE PROGRAM-NAME(1) SKIP           */
/*             iMessage SKIP cString          */
/*             VIEW-AS ALERT-BOX.             */

  PUBLISH "JlwIeMessage" (iMessage).
  CASE iMessage:
      WHEN {&MSG_BEFORE-NAVIGATE} THEN DO:
        IF cNavigateAction = "setWebDoc" AND NOT cString BEGINS "mailto" THEN DO:
/*           RUN WebBrowserGoBack(iWebBrowser,OUTPUT iSuccess). */
          DYNAMIC-FUNCTION("setWebDoc","",cString).
        END.
        RETURN.  
      END. 
      WHEN {&MSG_COMMAND-STATE-CHANGE} THEN RETURN.
      WHEN {&MSG_DOCUMENT-COMPLETE} THEN RETURN.
/*           DO:                                                               */
/*             ASSIGN editor-1:SCREEN-VALUE IN FRAME default-frame  = cString. */
/*             STATUS DEFAULT "Done".                                          */
/*           END.                                                              */
      WHEN {&MSG_DOWNLOAD-BEGIN} THEN RETURN.
      WHEN {&MSG_DOWNLOAD-COMPLETE} THEN RETURN.
      WHEN {&MSG_FILE-DOWNLOAD} THEN RETURN.
      WHEN {&MSG_NAVIGATE-COMPLETE} THEN DO:
        IF cNavigateAction = "setWebDoc" THEN
          RUN WebBrowserGoBack(iWebBrowser,OUTPUT iSuccess).
        ELSE
          PUBLISH "NavigationComplete" (cString).  
      END. 
      WHEN {&MSG_NAVIGATE-ERROR} THEN RETURN.
      WHEN {&MSG_PROGRESS-CHANGE} THEN DO: 
        PUBLISH "NavigationComplete" (cString).
        RETURN NO-APPLY.
      END.
      WHEN {&MSG_STATUS-TEXT-CHANGE} THEN  STATUS DEFAULT cString.
      WHEN {&MSG_NEW-WINDOW} THEN DO:
        IF cNavigateAction = "setWebDoc" THEN
          DYNAMIC-FUNCTION("setWebDoc","",cString).
        ELSE
          RUN WebBrowserNavigate(iWebBrowser, cString, OUTPUT iSuccess).
      END.
      OTHERWISE DO:
          RETURN.
      END.
  END CASE.  
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
ON CLOSE OF THIS-PROCEDURE DO:    
  PUBLISH "InvalidateHandle".
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN WebBrowserStop (iWebBrowser,OUTPUT iSuccess).
  DELETE PROCEDURE THIS-PROCEDURE.
/*   IF VALID-HANDLE(hParent) AND hParent:FILE-NAME MATCHES "*JBoxJlwDynMenu*"  */
/*      THEN APPLY "close" TO hParent.                                          */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  hParent = SOURCE-PROCEDURE.

  CURRENT-WINDOW = hParent:CURRENT-WINDOW.
/*   RUN Controls.p PERSISTENT SET hControlsLibrary.  */

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteWebBrowser C-Win 
PROCEDURE DeleteWebBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN WebBrowserStop (iWebBrowser,OUTPUT iSuccess).
DELETE PROCEDURE THIS-PROCEDURE.

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
  /* Hide all frames. */
  HIDE FRAME frWeb.
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
  VIEW FRAME frWeb.
  {&OPEN-BROWSERS-IN-QUERY-frWeb}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihRectangle AS HANDLE NO-UNDO.
DEF INPUT PARAM icURL       AS CHAR   NO-UNDO.

CURRENT-WINDOW = hParent:CURRENT-WINDOW.

VIEW FRAME frWeb IN WINDOW hParent:CURRENT-WINDOW.
/* RUN enable_UI. */

ASSIGN FRAME {&FRAME-NAME}:X             = ihRectangle:X 
       FRAME {&FRAME-NAME}:Y             = ihRectangle:Y
       FRAME {&FRAME-NAME}:WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS - 2
       FRAME {&FRAME-NAME}:HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS
       NO-ERROR.

IF ERROR-STATUS:ERROR THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          ERROR-STATUS:ERROR SKIP
          ERROR-STATUS:GET-MESSAGE(1)
          VIEW-AS ALERT-BOX.
  
RUN WebBrowserCreate(FRAME frWeb:HWND,OUTPUT iWebBrowser).


IF icUrl NE "" THEN
  NavigateToURL(icUrl).

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
FRAME frWeb:MOVE-TO-TOP().
FRAME frWeb:SENSITIVE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME frWeb:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandleList C-Win 
FUNCTION getFrameHandleList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN STRING(FRAME frWeb:HANDLE).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NavigateToURL C-Win 
FUNCTION NavigateToURL RETURNS LOGICAL
  ( INPUT icUrl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN WebBrowserNavigate(iWebBrowser, icUrl, OUTPUT iSuccess).
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNavigateAction C-Win 
FUNCTION setNavigateAction RETURNS LOGICAL
  ( INPUT icNavigateAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cNavigateAction = icNavigateAction.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewText C-Win 
FUNCTION ViewText RETURNS LOGICAL
  ( INPUT icText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTmpFile AS CHAR NO-UNDO.

cTmpFile = DYNAMIC-FUNCTION("getUniqueFileName").
cTmpFile = cTmpFile + ".htm".
OUTPUT STREAM strmFile TO VALUE(cTmpFile).    
PUT STREAM strmFile UNFORMATTED icText.
OUTPUT STREAM strmFile CLOSE.

NavigateToURL(cTmpFile).

RUN MoveToTop.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

