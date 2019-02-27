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
DEFINE VARIABLE iBottomSpace     AS INTEGER     NO-UNDO INIT 120. /* INIT 175.  */
DEFINE VARIABLE iMenuId          AS INTEGER     NO-UNDO.
DEFINE VARIABLE ReturnValue      AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBuffer          AS HANDLE      NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS btnEvents imgLogo ButtonPanel 

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

DEFINE IMAGE imgLogo
     FILENAME "bmp/table.bmp":U
     STRETCH-TO-FIT
     SIZE 50 BY 2.76.

DEFINE RECTANGLE ButtonPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnEvents AT ROW 6.52 COL 1.4
     imgLogo AT ROW 4.81 COL 1
     ButtonPanel AT ROW 1.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.4 BY 6.67.


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
         HEIGHT             = 6.67
         WIDTH              = 50.4
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

DEF VAR iBtnHeight   AS INT    NO-UNDO INIT 36.
DEF VAR iBtnCnt      AS INT    NO-UNDO.
DEF VAR hMenuObject  AS HANDLE NO-UNDO.
DEF VAR cImage       AS CHAR   NO-UNDO.
DEF VAR cButtonMenu  AS CHAR   NO-UNDO.
DEF VAR hWeb         AS HANDLE NO-UNDO.
DEF VAR cMenuString  AS CHAR   NO-UNDO.
DEF VAR iLabelLength AS INT    NO-UNDO.
DEF VAR iMaxLabel    AS INT    NO-UNDO.

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
    ASSIGN iBtnCnt      = iBtnCnt + 1
           iLabelLength = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,FRAME {&FRAME-NAME}:FONT)
           iMaxLabel    = IF iLabelLength > iMaxLabel THEN iLabelLength ELSE iMaxLabel.
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
       imgLogo:WIDTH-PIXELS                                = iMaxLabel

       FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS            = iMaxLabel 
       FRAME {&FRAME-NAME}:WIDTH-PIXELS                    = iMaxLabel 
       THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-WIDTH-PIXELS  = iMaxLabel
       THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS          = iMaxLabel
       THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-HEIGHT-PIXELS = (iBtnCnt - 1) * iBtnHeight + iBottomSpace
       THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS         = THIS-PROCEDURE:CURRENT-WINDOW:VIRTUAL-HEIGHT-PIXELS
       FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS           = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS 
       FRAME {&FRAME-NAME}:HEIGHT-PIXELS                   = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS 
       ButtonPanel:HEIGHT-PIXELS                           = ButtonPanel:HEIGHT-PIXELS * iBtnCnt + 3 * iBtnCnt
       btnEvents:Y                                         = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 29
/*        fiPwd:Y                                     = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 29 */
/*        fiPwd:SIDE-LABEL-HANDLE:Y                   = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 29 */
       imgLogo:Y                                          = ButtonPanel:Y + ButtonPanel:HEIGHT-PIXELS + 2
       NO-ERROR.

IF THIS-PROCEDURE:CURRENT-WINDOW:X < 10 THEN
  DYNAMIC-FUNCTION("setWinStartXpos" IN hParent,THIS-PROCEDURE:CURRENT-WINDOW:X + THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 6).

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF CAN-DO("start-window,os-command,procedure,data-browse",hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE) THEN
    ASSIGN cImage      = hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE
           iBtnCnt     = iBtnCnt + 1
           cMenuString = cMenuString + (IF cMenuString NE "" THEN "," ELSE "")
                       + hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE + ";" 
                       + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE + ";"
                       + (IF hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE NE "" THEN 
                            hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE
                          ELSE hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE) 
                       + ";PanelAction"
                       + (IF cImage NE "" AND SEARCH(cImage) NE ? THEN
                           ";" + cImage
                          ELSE "").

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

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
  ENABLE btnEvents imgLogo ButtonPanel 
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
DEF VAR cImage  AS CHAR NO-UNDO.
DEF VAR iStatus AS INT  NO-UNDO.

SUBSCRIBE TO "setViewBtnEvents" ANYWHERE.
SUBSCRIBE TO "setEventMsgProg"  ANYWHERE.
SUBSCRIBE TO "setCompanyLogo"   ANYWHERE.
SUBSCRIBE TO "InvalidateHandle" IN hParent.


/* RUN GradientFrameCreate(FRAME {&FRAME-NAME}:HWND,"",{&GRAD_FRAME-GRADIENT},15953229,6479746,  */
/*                        {&GRAD_FRAME_GRADIENT-BACKWARDDIAGONAL},OUTPUT iStatus).               */
  

DO WITH FRAME {&FRAME-NAME}:
  btnEvents:HIDDEN = YES.
  
  hBuffer = DYNAMIC-FUNCTION("getMenuBuffer" IN hParent).

  IF VALID-HANDLE(hBuffer) THEN DO:
       
    RUN BuildButtons.

    hPanelFrame = DYNAMIC-FUNCTION("getPanelFrame" IN hPanel).
  
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
DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, hPanelFrame,hPanelFrame:NAME).
DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"imgLogo").

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
/* APPLY "entry" TO fiPwd IN FRAME {&FRAME-NAME}.  */
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
RUN ApplyMenu IN hParent (?,DYNAMIC-FUNCTION("getPanelAction" IN hPanel),"").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCompanyLogo C-Win 
PROCEDURE setCompanyLogo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icLogoImg AS CHAR NO-UNDO.

DEF VAR iWidth  AS INT  NO-UNDO.
DEF VAR iHeight AS INT  NO-UNDO.
DEF VAR cSize   AS CHAR NO-UNDO.
DEF VAR iHdiff  AS INT  NO-UNDO.
DEF VAR iWdiff  AS INT  NO-UNDO.

IF SEARCH(icLogoImg) NE ? THEN DO WITH FRAME {&FRAME-NAME}:
  cSize = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE cCodeType = 'CompanyLogo' AND cCodeValue MATCHES '*" + ENTRY(NUM-ENTRIES(icLogoImg,"\"),icLogoImg,"\") + "'","cMisc1,cMisc2").
  IF cSize NE ? THEN
    ASSIGN iWidth  = INT(ENTRY(1,cSize,"|"))
           iHeight = INT(ENTRY(2,cSize,"|"))
           iHdiff  = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - (imgLogo:Y + iHeight)
           iWdiff  = FRAME {&FRAME-NAME}:WIDTH-PIXELS - iWidth
           .
    IF iWidth > 0 AND iHeight > 0 THEN
      ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS - iHdiff
             FRAME {&FRAME-NAME}:HEIGHT-PIXELS           = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS
             FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS   = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS
             imgLogo:HEIGHT-PIXELS = iHeight
             THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS  = MAX(THIS-PROCEDURE:WIDTH-PIXELS,THIS-PROCEDURE:WIDTH-PIXELS - iWdiff)
             FRAME {&FRAME-NAME}:WIDTH-PIXELS            = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS
             FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS    = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS
             imgLogo:WIDTH-PIXELS = iWidth
             imgLogo:X =  (FRAME {&FRAME-NAME}:WIDTH-PIXELS - iWidth) / 2
             FRAME {&FRAME-NAME}:SCROLLABLE = NO
             NO-ERROR.

  imgLogo:LOAD-IMAGE(icLogoImg).
  imgLogo:HIDDEN = NO.
END.
ELSE DO:
  imgLogo:HIDDEN = YES.
  IF icLogoImg NE "" THEN
    MESSAGE "Company logo image not found: " icLogoImg
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setViewBtnEvents C-Win 
PROCEDURE setViewBtnEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibViewBtnEvents AS LOG NO-UNDO.

btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = NOT ibViewBtnEvents.
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
/*            fiPwd:LABEL = "Password for ext.menu"  */
           .

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

