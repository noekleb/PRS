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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE bEnableBtnEvents AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bOk              AS LOGICAL     NO-UNDO.
DEFINE VARIABLE btnDefault       AS HANDLE      NO-UNDO.
DEFINE VARIABLE cEventMsgProg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hDynMenu         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hParent          AS HANDLE      NO-UNDO.
DEFINE VARIABLE iBottomSpace     AS INTEGER     NO-UNDO INIT 45. /* INIT 175.  */
DEFINE VARIABLE iMenuId          AS INTEGER     NO-UNDO.
DEFINE VARIABLE ReturnValue      AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBuffer          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery           AS HANDLE      NO-UNDO.
DEF VAR hBtnArray                AS HANDLE      NO-UNDO EXTENT 20.
DEF VAR hPanel                   AS HANDLE      NO-UNDO.
DEF VAR hPanelFrame              AS HANDLE      NO-UNDO.

{JukeBoxGradient.i}

PROCEDURE SendMessageA EXTERNAL "USER32.dll":
  DEFINE INPUT PARAMETER hHWND AS LONG.
  DEFINE INPUT PARAMETER iCmd  AS LONG.
  DEFINE INPUT PARAMETER iChar AS LONG.
  DEFINE INPUT PARAMETER ulParam AS LONG.
END PROCEDURE.

PROCEDURE PostPWChar:
  DEFINE INPUT PARAMETER hHWND AS INT.
  RUN SendMessageA(hHWND, 204, ASC("*"), 0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ButtonPanel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitLocalTranslation C-Win 
FUNCTION InitLocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE ButtonPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 1.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButtonPanel AT ROW 1.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.4 BY 2.29.


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
         TITLE              = "Ventende oppgaver"
         HEIGHT             = 2.33
         WIDTH              = 50.4
         MAX-HEIGHT         = 22.38
         MAX-WIDTH          = 108.6
         VIRTUAL-HEIGHT     = 22.38
         VIRTUAL-WIDTH      = 108.6
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/alarm.ico":U) THEN
    MESSAGE "Unable to load icon: ico/alarm.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ventende oppgaver */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ventende oppgaver */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MAXIMIZED OF C-Win /* Ventende oppgaver */
DO:
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* ON 'return' OF {&WINDOW-NAME} ANYWHERE  */
/*   APPLY "choose" TO btnDefault.         */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildButtons C-Win 
PROCEDURE BuildButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iBtnHeight   AS INT    NO-UNDO INIT 40.
DEF VAR iBtnCnt      AS INT    NO-UNDO.
DEF VAR hMenuObject  AS HANDLE NO-UNDO.
DEF VAR cImage       AS CHAR   NO-UNDO.
DEF VAR cButtonMenu  AS CHAR   NO-UNDO.
DEF VAR hWeb         AS HANDLE NO-UNDO.
DEF VAR cMenuString  AS CHAR   NO-UNDO.
DEF VAR iLabelLength AS INT    NO-UNDO.
DEF VAR iMaxLabel    AS INT    NO-UNDO.

hQuery:GET-FIRST().
IF NOT hBuffer:AVAIL THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Menyen for alarmer er ikke definert","","").
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN.
END. 

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF SEARCH(hBuffer:BUFFER-FIELD("Parameter1"):BUFFER-VALUE) NE ? THEN DO:
    ASSIGN iBtnCnt      = iBtnCnt + 1
           iLabelLength = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(hBuffer:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE,FRAME {&FRAME-NAME}:FONT)
           iMaxLabel    = IF iLabelLength > iMaxLabel THEN iLabelLength ELSE iMaxLabel
           hBuffer:BUFFER-FIELD("ProgramFunnet"):BUFFER-VALUE = YES
           .
    IF DYNAMIC-FUNCTION("runProc",hBuffer:BUFFER-FIELD("Parameter2"):BUFFER-VALUE,"",?) THEN
      hBuffer:BUFFER-FIELD("AlarmForProgram"):BUFFER-VALUE = YES.
  END.
  hQuery:GET-NEXT().
END.

iMaxLabel = MIN(200,iMaxLabel + 50).
IF iMaxLabel < 220 THEN iMaxLabel = 220.

IF iBtnCnt * iBtnHeight + iBottomSpace > SESSION:HEIGHT-PIXELS THEN
  iBtnHeight = (SESSION:HEIGHT-PIXELS - iBottomSpace) / iBtnCnt.

ASSIGN 
       THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS          = SESSION:WIDTH-PIXELS
       THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS         = SESSION:HEIGHT-PIXELS
       THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-WIDTH-PIXELS  = SESSION:WIDTH-PIXELS
       THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS
       FRAME {&FRAME-NAME}:WIDTH-PIXELS                    = SESSION:WIDTH-PIXELS
       FRAME {&FRAME-NAME}:HEIGHT-PIXELS                   = SESSION:HEIGHT-PIXELS
       FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS            = SESSION:WIDTH-PIXELS
       FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS           = SESSION:HEIGHT-PIXELS

       buttonPanel:WIDTH-PIXELS                            = iMaxLabel

       FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS            = iMaxLabel 
       FRAME {&FRAME-NAME}:WIDTH-PIXELS                    = iMaxLabel 
       THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-WIDTH-PIXELS  = iMaxLabel
       THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS          = iMaxLabel
       THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-HEIGHT-PIXELS = (iBtnCnt - 1) * iBtnHeight + iBottomSpace
       THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS         = THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-HEIGHT-PIXELS
       FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS           = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS 
       FRAME {&FRAME-NAME}:HEIGHT-PIXELS                   = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS 
       ButtonPanel:HEIGHT-PIXELS                           = ButtonPanel:HEIGHT-PIXELS * iBtnCnt + 3 * iBtnCnt
       .

IF THIS-PROCEDURE:CURRENT-WINDOW:X < 10 THEN
  DYNAMIC-FUNCTION("setWinStartXpos" IN hParent,THIS-PROCEDURE:CURRENT-WINDOW:X + THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 6) NO-ERROR.

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF hBuffer:BUFFER-FIELD("ProgramFunnet"):BUFFER-VALUE THEN
    ASSIGN cImage      = IF hBuffer:BUFFER-FIELD("AlarmForProgram"):BUFFER-VALUE THEN "bmp\dueledg.bmp" ELSE "bmp\disdueledg.bmp"
           iBtnCnt     = iBtnCnt + 1
           cMenuString = cMenuString + (IF cMenuString NE "" THEN "," ELSE "")
                       + hBuffer:BUFFER-FIELD("Parameter1"):BUFFER-VALUE + ";"
                       + hBuffer:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE + ";"
                       + hBuffer:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE + ";"
                       + "PanelAction"
                       + (IF cImage NE "" AND SEARCH(cImage) NE ? THEN
                           ";" + cImage
                          ELSE "").

  hQuery:GET-NEXT().
END.

hPanel = DYNAMIC-FUNCTION("NewPanel"
        ,ButtonPanel:HANDLE IN FRAME {&FRAME-NAME}
        ,"" /* New sub-menu (blank for none) */ 
        ,cMenuString
        ,0,iBtnHeight     
        ,"").


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
  ENABLE ButtonPanel 
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
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cAlarmMenu AS CHAR NO-UNDO.
DEF VAR iStatus    AS INT  NO-UNDO.

/* RUN GradientFrameCreate(FRAME {&FRAME-NAME}:HWND,"",{&GRAD_FRAME-GRADIENT},15953229,6479746, */
/*                        {&GRAD_FRAME_GRADIENT-BACKWARDDIAGONAL},OUTPUT iStatus).              */

DO WITH FRAME {&FRAME-NAME}:
  /*
  cAlarmMenu = REPLACE(DYNAMIC-FUNCTION("getFieldList","SysGruppe;Beskrivelse","WHERE SysHId = 300"),"|",",").

  IF cAlarmMenu = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Menyen for alarmer er ikke definert","","").
    APPLY CLOSE TO THIS-PROCEDURE.
    RETURN.
  END.
  */

  hQuery = DYNAMIC-FUNCTION("NewQuery"
        ,100
        ,""
        ,"SysGruppe"
        + ";SysHId"
       + ",SysPara"
        + ";Beskrivelse"
        + ";distinct Parameter1"
        + ";Parameter2"
        + ";ParaNr"
        + ";+ProgramFunnet|LOGICAL"
        + ";+AlarmForProgram|LOGICAL"
        + ";+AlarmProgHandle|HANDLE"
        ,"WHERE SysHId = 300 AND NOT Beskrivelse BEGINS 'x'"
       + ",EACH SysPara OF SysGruppe"
        ,"sort|SysGr;BY;ParaNr").

  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  RUN BuildButtons.

  InitLocalTranslation().
END.

/* Resize settings (always needed since it also brings up the help menu): */
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,500,THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS - iBottomSpace,0,0).

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
DEF INPUT PARAMETER ihProgram AS HANDLE NO-UNDO.
  
IF ihProgram = hParent AND VALID-HANDLE(hDynMenu) THEN 
  APPLY "CLOSE" TO hDynMenu.

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
IF VALID-HANDLE(hParent:CURRENT-WINDOW) THEN DO:
  {&WINDOW-NAME}:Y = hParent:CURRENT-WINDOW:Y.
  {&WINDOW-NAME}:X = hParent:CURRENT-WINDOW:X + hParent:CURRENT-WINDOW:WIDTH-PIXELS.
END.
{&WINDOW-NAME}:HIDDEN = NO.
{&WINDOW-NAME}:WINDOW-STATE = 3.
APPLY "entry" TO FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PanelAction C-Win 
PROCEDURE PanelAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hAlarmWin AS HANDLE NO-UNDO.

hBuffer:FIND-FIRST("WHERE Parameter1 = '" + DYNAMIC-FUNCTION("getPanelAction" IN hPanel) + "'") NO-ERROR.
IF hBuffer:AVAIL THEN DO:
  RUN StartChildWindow IN hParent (hBuffer:BUFFER-FIELD("Parameter1"):BUFFER-VALUE,
                                   "",
                                   ?,
                                   YES,
                                   OUTPUT hAlarmWin).
  APPLY "close" TO THIS-PROCEDURE.
/*   hBuffer:BUFFER-FIELD("AlarmProgHandle"):BUFFER-VALUE = hAlarmWin. */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEventMsgProg C-Win 
PROCEDURE setEventMsgProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icEventMsgProg AS CHAR NO-UNDO.
ASSIGN cEventMsgProg    = icEventMsgProg
       bEnableBtnEvents = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitLocalTranslation C-Win 
FUNCTION InitLocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

