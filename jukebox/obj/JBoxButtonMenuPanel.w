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
DEFINE VARIABLE iBottomSpace     AS INTEGER     NO-UNDO INIT 175.
DEFINE VARIABLE iMenuId          AS INTEGER     NO-UNDO.
DEFINE VARIABLE ReturnValue      AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBuffer          AS HANDLE      NO-UNDO.
DEF VAR hBtnArray                AS HANDLE      NO-UNDO EXTENT 20.

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
&Scoped-Define ENABLED-OBJECTS btnEvents rectBottom imgLogo fiPwd 
&Scoped-Define DISPLAYED-OBJECTS fiPwd 

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
DEFINE BUTTON btnEvents 
     IMAGE-UP FILE "bmp/dueledg.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Hendelser" 
     SIZE 3.8 BY 1 TOOLTIP "Vis hendelser".

DEFINE VARIABLE fiPwd AS CHARACTER FORMAT "X(999)":U 
     LABEL "Passord for utvidet meny" 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE IMAGE imgLogo
     FILENAME "adeicon/blank":U
     STRETCH-TO-FIT
     SIZE 47 BY 4.52.

DEFINE RECTANGLE rectBottom
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.4 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnEvents AT ROW 8.81 COL 2
     fiPwd AT ROW 8.76 COL 35 COLON-ALIGNED
     rectBottom AT ROW 8.57 COL 1
     imgLogo AT ROW 4.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 47.4 BY 9.1.


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
         HEIGHT             = 9.1
         WIDTH              = 47.4
         MAX-HEIGHT         = 22.38
         MAX-WIDTH          = 108.6
         VIRTUAL-HEIGHT     = 22.38
         VIRTUAL-WIDTH      = 108.6
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MAXIMIZED OF C-Win /* <insert window title> */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEvents
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEvents C-Win
ON CHOOSE OF btnEvents IN FRAME DEFAULT-FRAME /* Hendelser */
DO:
  DEF VAR ohWinHandle    AS HANDLE NO-UNDO.

  IF cEventMsgProg NE "" THEN
    RUN StartChildWindow IN hParent (cEventMsgProg,"",?,NO,OUTPUT ohWinHandle).
  ELSE 
    MESSAGE PROGRAM-NAME(1) SKIP
            "Program for viewing events not set" SKIP
            VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPwd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPwd C-Win
ON ANY-PRINTABLE OF fiPwd IN FRAME DEFAULT-FRAME /* Passord for utvidet meny */
DO:
  RUN PostPWChar(fiPwd:HWND).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPwd C-Win
ON RETURN OF fiPwd IN FRAME DEFAULT-FRAME /* Passord for utvidet meny */
OR TAB OF fiPwd DO:
  DEF VAR iMenuId AS INT NO-UNDO.

  IF NOT fiPwd:MODIFIED OR fiPwd:SCREEN-VALUE = "" THEN RETURN NO-APPLY.

  iMenuId = INT(DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE cMenuType = 'menu' AND cMenuNumber = '" + fiPwd:SCREEN-VALUE + "'","iJBoxMenuId")).

  IF iMenuId NE ? THEN DO:
    IF VALID-HANDLE(hDynMenu) AND DYNAMIC-FUNCTION("getMainMenuId" IN hDynMenu) = iMenuId THEN
      RUN MoveToTop IN hDynMenu.
    ELSE DO:        
      IF VALID-HANDLE(hDynMenu) THEN 
        APPLY "close" TO hDynMenu.

      RUN JBoxJlwDynMenu.w PERSISTENT SET hDynMenu.
      RUN InitializeObject IN hDynMenu (iMenuId).
      RUN MoveToTop IN hDynMenu.
    END.
  END.
  fiPwd:SCREEN-VALUE = "".
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
  RUN disable_UI.
  APPLY "close" TO hParent.
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
DEF VAR hQuery  AS HANDLE NO-UNDO.

DEF VAR iBtnHeight   AS INT    NO-UNDO INIT 47.
DEF VAR iBtnCnt      AS INT    NO-UNDO.
DEF VAR hMenuObject  AS HANDLE NO-UNDO.
DEF VAR cImage       AS CHAR   NO-UNDO.
DEF VAR cButtonMenu  AS CHAR   NO-UNDO.
DEF VAR hWeb         AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).

/* Buttons are created for the first sub-menu instance: */

hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE cMenuType = 'sub-menu' AND cParentMenuType = 'menubar' BY cMenuNumber").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
IF NOT hBuffer:AVAIL THEN RETURN.

cButtonMenu = hBuffer:BUFFER-FIELD("iNodeIndex"):STRING-VALUE.

hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE cMenuType = 'menu-item' AND iParentNodeIndex = " + cButtonMenu + " BY cMenuNumber").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF CAN-DO("start-window,os-command,procedure,data-browse",hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE) THEN
    iBtnCnt = iBtnCnt + 1.
  hQuery:GET-NEXT().
END.

IF iBtnCnt * iBtnHeight + iBottomSpace > SESSION:HEIGHT-PIXELS THEN
  iBtnHeight = (SESSION:HEIGHT-PIXELS - iBottomSpace) / iBtnCnt.

ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
       THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = (iBtnCnt - 1) * iBtnHeight + iBottomSpace
       FRAME {&FRAME-NAME}:HEIGHT-PIXELS           = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS 
       iBtnCnt                                     = 0
       rectBottom:Y                                = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 33
       btnEvents:Y                                 = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 29
       fiPwd:Y                                     = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 29
       fiPwd:SIDE-LABEL-HANDLE:Y                   = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 29
       imgLogo:Y                                   = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 128
       .

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF CAN-DO("start-window,os-command,procedure,data-browse",hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE) THEN DO:
      
    CREATE BUTTON hMenuObject
      ASSIGN  FRAME = FRAME {&FRAME-NAME}:HANDLE 
              WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 2
              HEIGHT-PIXELS = iBtnHeight - 2
              X = 1
              Y = iBtnCnt * iBtnHeight
              LABEL = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
              TOOLTIP = IF hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE NE "" THEN 
                          hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE
                        ELSE hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
              VISIBLE = YES
              SENSITIVE = YES
              NAME = "btn_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
      TRIGGERS:
        ON CHOOSE PERSISTENT RUN ApplyMenu IN hParent (?,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,"").
      END TRIGGERS.
  
    cImage = hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.
    IF cImage NE "" AND SEARCH(cImage) NE ? THEN 
      hMenuObject:LOAD-IMAGE(cImage).
  
    iBtnCnt = iBtnCnt + 1.
    IF iBtnCnt = 1 THEN
      btnDefault = hMenuObject.
  END.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
/* IF iBtnCnt = 1 THEN                                                                              */
/*   THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS + 2. */
/*                                                                                                  */

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
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY fiPwd 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnEvents rectBottom imgLogo fiPwd 
      WITH FRAME DEFAULT-FRAME.
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
DEF VAR cImage AS CHAR NO-UNDO.

SUBSCRIBE TO "setViewBtnEvents" ANYWHERE.
SUBSCRIBE TO "setEventMsgProg"  ANYWHERE.
SUBSCRIBE TO "InvalidateHandle" IN hParent.

DO WITH FRAME {&FRAME-NAME}:
  btnEvents:HIDDEN = YES.
  
  hBuffer = DYNAMIC-FUNCTION("getMenuBuffer" IN hParent).

  IF VALID-HANDLE(hBuffer) THEN DO:
       
    RUN BuildButtons.
  
    bOK = hBuffer:FIND-FIRST("WHERE cMenuType = 'menu'").
    IF bOK THEN DO:
      cImage = hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.
      IF SEARCH(cImage) NE ? THEN 
        imgLogo:LOAD-IMAGE(cImage).
      ELSE imgLogo:HIDDEN = YES.
    END.
  END.

  InitLocalTranslation().
END.

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"imgLogo,rectBottom").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"imgLogo,rectBottom").

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
APPLY "entry" TO fiPwd IN FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setViewBtnEvents C-Win 
PROCEDURE setViewBtnEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibViewBtnEvents AS LOG NO-UNDO.


IF bEnableBtnEvents THEN DO WITH FRAME {&FRAME-NAME}:
  IF ibViewBtnEvents AND SEARCH("bmp\dueledg.bmp") NE ? THEN
    btnEvents:LOAD-IMAGE("bmp\dueledg.bmp").
  ELSE IF SEARCH("bmp\disdueledg.bmp") NE ? THEN
    btnEvents:LOAD-IMAGE("bmp\disdueledg.bmp").
  btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = NO.
END.
/* btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = NOT ibViewBtnEvents. */
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
  IF NOT DYNAMIC-FUNCTION("Scandinavian") THEN
    ASSIGN btnEvents:TOOLTIP = "View events"
           fiPwd:LABEL = "Password for ext.menu"
           .

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

