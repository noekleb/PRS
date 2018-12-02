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

{JBoxObjectDef.i}
/* DEF BUFFER bttEvent FOR ttEvent.  */

{JukeBoxControlsGeneral.i}
{JukeBoxPanel.i}

/* DEF VAR hControlsLibrary    AS HANDLE NO-UNDO.  */
DEF VAR iPanelIdx           AS INTEGER NO-UNDO.
DEF VAR iPanel              AS INTEGER NO-UNDO.
DEF VAR hSourceProc         AS HANDLE  NO-UNDO.

DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR iPanelStyle         AS INT    NO-UNDO.
DEF VAR cImageList          AS CHAR   NO-UNDO.
DEF VAR cImagePath          AS CHAR   NO-UNDO.
DEF VAR cLargeImageList     AS CHAR   NO-UNDO.
DEF VAR cLargeImagePath     AS CHAR   NO-UNDO.
DEF VAR iImage              AS INT    NO-UNDO.
DEF VAR iImgSelected        AS INT    NO-UNDO.
DEF VAR iStatus             AS INT    NO-UNDO.
DEF VAR iPanelNodeIndex      AS INT    NO-UNDO.
DEF VAR iNavBarNodeIndex    AS INT    NO-UNDO.
DEF VAR cColorNameList      AS CHAR   NO-UNDO
    INIT "AQUA,BLACK,BLUE,CREAM,DARKGRAY,FUCHSIA,GRAY,GREEN,LIMEGREEN,LIGHTGRAY,MAROON,MEDIUMGRAY,MINTGREEN,NAVYBLUE,OLIVE,PURPLE,RED,SILVER,SKYBLUE,TEAL,WHITE,YELLOW".
DEF VAR cColorNumList       AS CHAR
    INIT "16776960,0,16711680,15793151,8421504,16711935,8421504,32768,65280,12632256,128,10789024,12639424,8388608,32896,8388736,255,12632256,15780518,8421376,16777215,65535".


DEF TEMP-TABLE ttPanel 
    FIELD cAction   AS CHARACTER
    FIELD cLabel    AS CHARACTER
    FIELD cSelLabel AS CHARACTER
    FIELD cTooltip  AS CHARACTER
    FIELD cImage    AS CHARACTER
    FIELD cSelImage AS CHARACTER
    FIELD cMethod   AS CHARACTER
    FIELD iButtonId AS INTEGER
    .
DEF BUFFER bttPanel FOR ttPanel.
DEF VAR httPanel AS HANDLE NO-UNDO.
httPanel = BUFFER ttPanel:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rtf-frame

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteItem C-Win 
FUNCTION DeleteItem RETURNS LOGICAL
  ( INPUT iiBtnIndex AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dumpTT C-Win 
FUNCTION dumpTT RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getImageList C-Win 
FUNCTION getImageList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getImgIndex C-Win 
FUNCTION getImgIndex RETURNS INTEGER
  ( INPUT icImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLargeImageList C-Win 
FUNCTION getLargeImageList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPanelAction C-Win 
FUNCTION getPanelAction RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPanelFrame C-Win 
FUNCTION getPanelFrame RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPanelHandle C-Win 
FUNCTION getPanelHandle RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitPanel C-Win 
FUNCTION InitPanel RETURNS HANDLE
  ( INPUT ihPanelRect    AS HANDLE,
    INPUT iiButtonWidth  AS INT,
    INPUT iiButtonHeight AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitPanelFont C-Win 
FUNCTION InitPanelFont RETURNS LOGICAL
  ( INPUT icFontName  AS CHAR,
    INPUT iiSize      AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButtonImage C-Win 
FUNCTION setButtonImage RETURNS LOGICAL
  ( INPUT iiBtnIndex AS INTEGER,
    INPUT icImage    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setImage C-Win 
FUNCTION setImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setImageList C-Win 
FUNCTION setImageList RETURNS LOGICAL
  ( INPUT icImageList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rtf-frame
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.6 BY 7.86.


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
         HEIGHT             = 10.71
         WIDTH              = 89.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 89.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 89.4
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
/* SETTINGS FOR FRAME rtf-frame
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME rtf-frame
/* Query rebuild information for FRAME rtf-frame
     _Query            is NOT OPENED
*/  /* FRAME rtf-frame */
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
  APPLY "close" TO hParent.
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
  DELETE PROCEDURE THIS-PROCEDURE NO-ERROR.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddItem C-Win 
PROCEDURE AddItem :
DEF INPUT PARAM icAction          AS CHAR NO-UNDO.
DEF INPUT PARAM icLabel           AS CHAR NO-UNDO.
DEF INPUT PARAM icSelLabel        AS CHAR NO-UNDO.
DEF INPUT PARAM icTooltip         AS CHAR NO-UNDO.
DEF INPUT PARAM icImage           AS CHAR NO-UNDO.
DEF INPUT PARAM icSelImage        AS CHAR NO-UNDO.
DEF INPUT PARAM icMethod          AS CHAR NO-UNDO.
/*------------------------------------------------------------------------------*/
DEF VAR iImgIdx    AS INT NO-UNDO.
DEF VAR iSelImgIdx AS INT NO-UNDO.

IF icImage NE "" THEN DO:    
  iImgIdx = getImgIndex(icImage).
  IF iImgIdx < 0 THEN DO:
    MESSAGE "Image " icImage " not in image-list" SKIP
            "Node not created" SKIP(1) 
            "Image path: " cImagePath SKIP(1)
            "Image list: " SKIP 
             cImageList SKIP            
             VIEW-AS ALERT-BOX.
    RETURN.
  END.
END.
ELSE iImgIdx = iImage.

IF icSelImage NE "" THEN DO:
  iSelImgIdx = getImgIndex(icSelImage).
  IF iSelImgIdx < 0 THEN 
    iSelImgIdx  = iImgSelected.
END.
ELSE iSelImgIdx  = iImgSelected.

IF iPanelIdx = 0 THEN DO:
  IF cImageList NE "" THEN
    RUN PanelInitImageList(iPanel,
                           cImagePath,
                           cImageList).
  RUN PanelShow(iPanel).
END.


CREATE ttPanel.
ASSIGN ttPanel.cAction          = icAction
       ttPanel.cLabel           = icLabel
       ttPanel.cSelLabel        = icSelLabel
       ttPanel.cTooltip         = icTooltip
       ttPanel.cImage           = icImage
       ttPanel.cSelImage        = icSelImage
       ttPanel.cMethod          = icMethod
       iPanelIdx                = iPanelIdx + 1
       ttPanel.iButtonId        = iPanelIdx.
  
RUN PanelAddItemEx(iPanel,icLabel,iImgIdx,icTooltip).


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
  HIDE FRAME rtf-frame.
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
  VIEW FRAME rtf-frame.
  {&OPEN-BROWSERS-IN-QUERY-rtf-frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
DEF INPUT PARAM ihRectangle        AS HANDLE NO-UNDO.
DEF INPUT PARAM icMenu             AS CHAR   NO-UNDO.
DEF INPUT PARAM icActionList       AS CHAR   NO-UNDO.
DEF INPUT PARAM iiButtonWidth      AS INT    NO-UNDO.
DEF INPUT PARAM iiButtonHeight     AS INT    NO-UNDO.
DEF INPUT PARAM icOtherProp        AS CHAR   NO-UNDO.
DEF INPUT PARAM ihObjectSourceProc AS HANDLE NO-UNDO.

DEF VAR cAction        AS CHAR   NO-UNDO.
DEF VAR cMethod        AS CHAR   NO-UNDO.
DEF VAR cLabel         AS CHAR   NO-UNDO.
DEF VAR cToolTip       AS CHAR   NO-UNDO.
DEF VAR cImage         AS CHAR   NO-UNDO.
DEF VAR hMenu          AS HANDLE NO-UNDO.
DEF VAR hPlaceHold     AS HANDLE NO-UNDO.
DEF VAR hRule          AS HANDLE NO-UNDO.
DEF VAR cHotKey        AS CHAR   NO-UNDO.
DEF VAR cMenuList      AS CHAR   NO-UNDO.
DEF VAR cPlaceHoldList AS CHAR   NO-UNDO.
DEF VAR cBtnMenu       AS CHAR   NO-UNDO.
DEF VAR iButtonGap     AS INT    NO-UNDO.
DEF VAR iNumPlaceHold  AS INT    NO-UNDO.
DEF VAR bRightAdjust   AS LOG    NO-UNDO.
DEF VAR bNoMenu        AS LOG    NO-UNDO.
DEF VAR cAccelerator   AS CHAR   NO-UNDO.
DEF VAR cButtonNames   AS CHAR   NO-UNDO.
DEF VAR cButtonHandles AS CHAR   NO-UNDO.
DEF VAR cRuleHandles   AS CHAR   NO-UNDO.
DEF VAR hParentSubMenu AS HANDLE NO-UNDO.
DEF VAR hLastBtn       AS HANDLE NO-UNDO.
DEF VAR cActionDef     AS CHAR   NO-UNDO.
DEF VAR cEnabledList   AS CHAR   NO-UNDO.
DEF VAR bMenuOnly      AS LOG    NO-UNDO.
DEF VAR bToggle        AS LOG    NO-UNDO.
DEF VAR hCurrObject    AS HANDLE NO-UNDO.

ASSIGN cCtrlHotkeyActions = DYNAMIC-FUNCTION("getAttribute",SESSION,"CtrlHotkeyActions")
       cCtrlHotkeys       = DYNAMIC-FUNCTION("getAttribute",SESSION,"CtrlHotkeys")
       cAltHotkeyActions  = DYNAMIC-FUNCTION("getAttribute",SESSION,"AltHotkeyActions")
       cAltHotkeys        = DYNAMIC-FUNCTION("getAttribute",SESSION,"AltHotkeys")
       hSourceProc        = ihObjectSourceProc
       .

IF icOtherProp MATCHES "*right*" THEN 
  bRightAdjust = TRUE.

IF NOT VALID-HANDLE(ihObjectSourceProc) THEN ihObjectSourceProc = SOURCE-PROCEDURE.

InitPanel(ihRectangle,iiButtonWidth,iiButtonHeight).

IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"setPanelProperties") THEN
  DYNAMIC-FUNCTION("setPanelProperties" IN hSourceProc,THIS-PROCEDURE,ihRectangle,iPanel).

IF icOtherProp BEGINS "append_toolbar" THEN DO:
  FIND FIRST ttObject WHERE ttObject.hObject = WIDGET-HANDLE(ENTRY(2,icOtherProp)) NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN ?.
END.
ELSE
  FIND FIRST ttObject WHERE ttObject.hObject = THIS-PROCEDURE NO-ERROR.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihRectangle:WINDOW
         ttObject.hObject       = THIS-PROCEDURE
         ttObject.cObjectType   = "panel"
         ttObject.hDesignObject = ihRectangle
         ttObject.cObjectName   = ihRectangle:NAME
         ttObject.hSourceProc   = ihObjectSourceProc
         ttObject.cGenProc      = PROGRAM-NAME(1)
         ttObject.cInitProc     = ihObjectSourceProc:FILE-NAME.
END.
hCurrObject = ttObject.hObject.

PUBLISH "NeverResize" (ihRectangle,"Y").

IF icMenu NE "" THEN DO:
  IF NOT VALID-HANDLE(ihRectangle:WINDOW:MENUBAR) THEN DO:
    CREATE MENU hMenu.
    ihRectangle:WINDOW:MENUBAR = hMenu.
  END.
  ELSE DO:
    hMenu = ihRectangle:WINDOW:MENUBAR.
    hPlaceHold = hMenu:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hPlaceHold):
      IF hPlaceHold:LABEL = icMenu THEN DO:
        IF VALID-HANDLE(hPlaceHold:LAST-CHILD) AND hPlaceHold:LAST-CHILD:SUBTYPE NE "rule" THEN DO:
          CREATE MENU-ITEM hRule
                ASSIGN SUBTYPE = "rule"
                       PARENT  = hPlaceHold.
          CREATE ttEvent.
          ASSIGN ttEvent.hObject      = hCurrObject
                 ttEvent.cAction      = "no action"
                 ttEvent.cName        = "rule"
                 ttEvent.cWidgetType  = "menurule"
                 ttEvent.cMethod      = ""
                 ttEvent.hWidget      = hRule.
        END.
        LEAVE.
      END.
      hPlaceHold = hPlaceHold:NEXT-SIBLING.
    END.
  END.
  IF NOT VALID-HANDLE(hPlaceHold) THEN DO:
    CREATE SUB-MENU hPlaceHold
           ASSIGN PARENT = hMenu
                  LABEL = icMenu
                  NAME  = icMenu.
    CREATE ttEvent.
    ASSIGN ttEvent.hObject      = hCurrObject
           ttEvent.cAction      = "no action"
           ttEvent.cName        = icMenu
           ttEvent.cWidgetType  = "sub-menu"
           ttEvent.cMethod      = ""
           ttEvent.hWidget      = hPlaceHold.
  END.
  ASSIGN cMenuList      = icMenu
         cPlaceHoldList = STRING(hPlaceHold).
END.

DO ix = 1 TO NUM-ENTRIES(icActionList):
  cActionDef   = ENTRY(1,ENTRY(ix,icActionList),"¤").
  IF NUM-ENTRIES(cActionDef,";") = 5 THEN
    cImage   = ENTRY(5,cActionDef,";").
  ELSE 
    cImage = DYNAMIC-FUNCTION("getAttribute",SESSION,"BtnImg_" + ENTRY(1,ENTRY(1,cActionDef,";"),"|")).
  IF cImage NE "" AND (cImage MATCHES "*bmp" OR cImage MATCHES "*ico") THEN
    cImageList = cImageList + (IF cImageList NE "" THEN ";" ELSE "") + cImage.
END.
IF cImageList NE "" THEN
  setImageList(cImageList).


DO ix = 1 TO NUM-ENTRIES(icActionList):
  ASSIGN bNoMenu      = NO
         bMenuOnly    = NO
         cAccelerator = ""    
         cActionDef   = ENTRY(1,ENTRY(ix,icActionList),"¤")
         .
  IF NUM-ENTRIES(cActionDef,";") = 1 THEN 
    ASSIGN cAction      = ENTRY(1,cActionDef,"|")
           cMethod      = cAction + "Record"
           cLabel       = CAPS(SUBSTR(cAction,1,1)) + SUBSTR(cAction,2) 
           cToolTip     = CAPS(SUBSTR(cLabel,1,1)) + SUBSTR(cLabel,2).
  ELSE IF NUM-ENTRIES(cActionDef,";") = 2 THEN DO:
    ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
           cLabel   = ENTRY(2,cActionDef,";")
           cToolTip = cLabel
           cMethod  = cAction + "Record".
  END.
  ELSE IF NUM-ENTRIES(cActionDef,";") = 3 THEN 
    ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
           cLabel   = ENTRY(2,cActionDef,";")
           cToolTip = ENTRY(3,cActionDef,";")
           cMethod  = cAction + "Record"
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") = 4 THEN
    ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
           cLabel   = ENTRY(2,cActionDef,";")
           cToolTip = ENTRY(3,cActionDef,";")
           cMethod  = ENTRY(4,cActionDef,";")
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") = 5 THEN DO:
    ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
           cLabel   = ENTRY(2,cActionDef,";")
           cToolTip = ENTRY(3,cActionDef,";")
           cMethod  = ENTRY(4,cActionDef,";")
           .
    IF cMethod = "" THEN cMethod = cAction + "Record".
  END.
  ELSE DO:
    MESSAGE "Error in TB def for element " cActionDef 
            VIEW-AS ALERT-BOX.
    RETURN ?.
  END.

  ASSIGN bToggle      = NUM-ENTRIES(cActionDef,";") > 2 AND ENTRY(3,cActionDef,";") = "toggle"
         cAccelerator = IF INDEX(cLabel,"ctrl-") > 0 THEN
                          "CTRL-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"ctrl-") + 5,1))
                        ELSE IF INDEX(cLabel,"alt-") > 0 THEN
                          "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"alt-") + 4,1))
                        ELSE IF INDEX(cLabel,"&") > 0 AND R-INDEX(cLabel,"&") NE LENGTH(cLabel) THEN
                         "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"&") + 1,1))
                        ELSE IF INDEX(cLabel,"&") > 0 THEN ""
                        ELSE IF CAN-DO(cCtrlHotkeyActions,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) THEN 
                         "CTRL-" + ENTRY(LOOKUP(cAction,cCtrlHotkeyActions),cCtrlHotkeys) 
                        ELSE IF CAN-DO(cAltHotkeyActions,cAction) THEN 
                         "ALT-" + ENTRY(LOOKUP(cAction,cAltHotkeyActions),cAltHotkeys) 
                        ELSE "ALT-" + CAPS(SUBSTR(cLabel,1,1))
         cTooltip     = RIGHT-TRIM(cTooltip + " (" + cAccelerator," (") + (IF cAccelerator NE "" THEN ")" ELSE "")
         cLabel       = (IF INDEX(cLabel,"ctrl-") > 0 THEN 
                          TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"ctrl-") - 1))
                         ELSE IF INDEX(cLabel,"alt-") > 0 THEN 
                          TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"alt-") - 1))
                         ELSE IF R-INDEX(cLabel,"&") = LENGTH(cLabel) THEN 
                          REPLACE(cLabel,"&","")
                         ELSE cLabel).

  IF NUM-ENTRIES(ENTRY(ix,icActionList),"¤") > 1 THEN DO: 
    IF ENTRY(2,ENTRY(ix,icActionList),"¤") MATCHES "*enable*" THEN
      cEnabledList = cEnabledList + "," + cAction.
    IF ENTRY(2,ENTRY(ix,icActionList),"¤") MATCHES "*menu*" THEN 
      bMenuOnly = YES.
  END.

  IF (cAction = "rule" OR cAction = "-") AND VALID-HANDLE(hPlaceHold) THEN DO:
    CREATE MENU-ITEM hRule
          ASSIGN SUBTYPE = "rule"
                 PARENT  = hPlaceHold
                 .
    NEXT.
  END.

  cBtnMenu = IF NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 THEN ENTRY(2,ENTRY(1,cActionDef,";"),"|") ELSE "".

  IF cBtnMenu NE "" AND cAction NE "sub-menu" AND NOT CAN-DO(cMenuList,cBtnMenu) THEN DO:
    IF NOT VALID-HANDLE(ihRectangle:WINDOW:MENUBAR) THEN DO:
      CREATE MENU hMenu.
      ihRectangle:WINDOW:MENUBAR = hMenu.
    END.
    ELSE DO:
      hMenu = ihRectangle:WINDOW:MENUBAR.
      hPlaceHold = hMenu:FIRST-CHILD.
      REPEAT WHILE VALID-HANDLE(hPlaceHold):
        IF hPlaceHold:LABEL = cBtnMenu THEN DO:
          IF VALID-HANDLE(hPlaceHold:LAST-CHILD) AND hPlaceHold:LAST-CHILD:SUBTYPE NE "rule" THEN DO:
            CREATE MENU-ITEM hRule
                  ASSIGN SUBTYPE = "rule"
                         PARENT  = hPlaceHold.
            DYNAMIC-FUNCTION("AddEvent",hCurrObject,hRule,"no action","rule","menurule","rule","").
          END.
          LEAVE.
        END.
        hPlaceHold = hPlaceHold:NEXT-SIBLING.
      END.
    END.
    IF NOT VALID-HANDLE(hPlaceHold) THEN DO:
      CREATE SUB-MENU hPlaceHold
             ASSIGN PARENT = hMenu
                    LABEL = cBtnMenu
                    NAME  = IF cAction NE "" THEN cAction ELSE REPLACE(cBtnMenu," ","").
      DYNAMIC-FUNCTION("AddEvent",hCurrObject,hPlaceHold,cAction,cBtnMenu,"sub-menu",cBtnMenu,"").
    END.
    ASSIGN cMenuList      = cMenuList + "," + cBtnMenu
           cPlaceHoldList = cPlaceHoldList + "," + STRING(hPlaceHold).
  END.
  ELSE IF cAction = "sub-menu" THEN DO:
    hMenu = ihRectangle:WINDOW:MENUBAR.
    hParentSubMenu = hMenu:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hParentSubMenu):
      IF hParentSubMenu:LABEL = cBtnMenu THEN DO:
        IF VALID-HANDLE(hParentSubMenu:LAST-CHILD) AND hParentSubMenu:LAST-CHILD:SUBTYPE NE "rule" THEN DO:
          CREATE MENU-ITEM hRule
                ASSIGN SUBTYPE = "rule"
                       PARENT  = hParentSubMenu.
          DYNAMIC-FUNCTION("AddEvent",hCurrObject,hRule,"no action","rule","menurule","rule","").
        END.
        CREATE SUB-MENU hPlaceHold
               ASSIGN PARENT = hParentSubMenu
                      LABEL = cLabel
                      NAME  = REPLACE(cLabel," ","").
        DYNAMIC-FUNCTION("AddEvent",hCurrObject,hPlaceHold,cLabel,cLabel,"sub-menu",cLabel,"").
        ASSIGN cMenuList      = cMenuList + "," + cLabel
               cPlaceHoldList = cPlaceHoldList + "," + STRING(hPlaceHold).
        LEAVE.
      END.
      hParentSubMenu = hParentSubMenu:NEXT-SIBLING.
    END.
  END.
  ELSE IF NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 AND cBtnMenu = "" THEN 
    bNoMenu = TRUE. 
  ELSE IF cBtnMenu NE "" AND CAN-DO(cMenuList,cBtnMenu) THEN
    hPlaceHold = WIDGET-HANDLE(ENTRY(LOOKUP(cBtnMenu,cMenuList),cPlaceHoldList)).
  ELSE IF cMenuList NE "" THEN
    hPlaceHold = WIDGET-HANDLE(ENTRY(1,cPlaceHoldList)).

  IF cAction = "" OR cAction = "sub-menu" THEN DO:
    iNumPlaceHold = iNumPlaceHold + 1.
    DYNAMIC-FUNCTION("setAttribute",hCurrObject,"placeholder" + STRING(iNumPlaceHold),STRING(hPlaceHold)).
    NEXT.
  END.

  IF cAction = "commit" THEN
    DYNAMIC-FUNCTION("setAttribute",hCurrObject,"commitstate","off").

  IF cMethod = "CloseRecord" THEN cMethod = "CloseWindow".
  ELSE IF cMethod = "HelpRecord" THEN 
    ASSIGN cMethod = "Help"
           cAccelerator = "F1"
           cTooltip     = cLabel.

  IF NOT bMenuOnly AND NOT bToggle THEN DO:
    CREATE ttEvent.
    ASSIGN ttEvent.hObject        = hCurrObject
           ttEvent.cAction        = cAction
           ttEvent.cName          = "choose" 
           ttEvent.cWidgetType    = "panel-button"
           ttEvent.cMethod        = cMethod
           ttEvent.cLabel         = STRING(ix)
           ttEvent.hWidget        = FRAME panel-frame:HANDLE
           .

    RUN AddItem (cAction,cLabel,"",cToolTip,ENTRY(ix,cImageList,";"),"",cMethod). 

    DYNAMIC-FUNCTION("setAttribute",hCurrObject,"button" + cAction,STRING(ttEvent.hWidget)).
  END.

  IF (icMenu NE "" OR cBtnMenu NE "") AND NOT bNoMenu AND cLabel NE "" THEN DO:

    IF VALID-HANDLE(hPlaceHold:LAST-CHILD) AND hPlaceHold:LAST-CHILD:TYPE = "sub-menu" THEN DO:
      CREATE MENU-ITEM hRule
        ASSIGN SUBTYPE = "rule"
               PARENT  = hPlaceHold
               .
      DYNAMIC-FUNCTION("AddEvent",hCurrObject,hRule,"no action","rule","menurule","rule","").
    END.
    CREATE ttEvent.
    ASSIGN ttEvent.hObject        = hCurrObject
           ttEvent.cAction        = cAction
           ttEvent.cName          = "choose"
           ttEvent.cWidgetType    = "menu-item"
           ttEvent.cMethod        = cMethod
           ttEvent.cLabel         = cLabel
           .
    IF bToggle THEN
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN TOGGLE-BOX = TRUE
                   PARENT     = hPlaceHold
                   LABEL      = cLabel
                   NAME       = cAction 
                   TRIGGERS:
                     ON VALUE-CHANGED PERSISTENT RUN DoProcessEvent (ihObjectSourceProc,"choose").
                   END TRIGGERS.
    ELSE
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN PARENT      = hPlaceHold
                   LABEL       = cLabel
                   NAME        = cAction 
                   ACCELERATOR = TRIM(cAccelerator,")")
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoProcessEvent (ihObjectSourceProc,"choose").
                  END TRIGGERS.
    DYNAMIC-FUNCTION("setAttribute",hCurrObject,"menu-item" + cAction,STRING(ttEvent.hWidget)).
  END.
END.

DYNAMIC-FUNCTION("setAttribute",hCurrObject,"button-names",TRIM(cButtonNames,",")).
DYNAMIC-FUNCTION("setAttribute",hCurrObject,"button-handles",TRIM(cButtonHandles,",")).
DYNAMIC-FUNCTION("setAttribute",hCurrObject,"rule-handles",TRIM(cRuleHandles,",")).
IF cEnabledList NE "" THEN 
  DYNAMIC-FUNCTION("setAttribute",ihRectangle,"configenabledevents",TRIM(cEnabledList,",")).

IF icOtherProp MATCHES "*border*" THEN DO:
  ASSIGN ihRectangle:HIDDEN = FALSE
         ihRectangle:X = IF CAN-DO(icOtherProp,"maxborder") THEN 2 ELSE ihRectangle:X - 2
         ihRectangle:Y = ihRectangle:Y - 3
         ihRectangle:WIDTH-PIXELS = IF CAN-DO(icOtherProp,"maxborder") THEN ihRectangle:FRAME:WIDTH-PIXELS - 2
                                    ELSE IF CAN-DO(icOtherProp,"leftborder") THEN 3
                                    ELSE hLastBtn:X + hLastBtn:WIDTH-PIXELS - ihRectangle:X + 2
         ihRectangle:HEIGHT-PIXELS = (IF icMenu = "" THEN 29 ELSE 26 ).
END.
ELSE 
  ihRectangle:HIDDEN = TRUE.


RUN PanelSetActiveItem(iPanel,{&GEN_NOSELECT}).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartJBoxEvent C-Win 
PROCEDURE StartJBoxEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihObject      AS HANDLE NO-UNDO.
DEF INPUT PARAM ihSourceProc  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihWidget      AS HANDLE NO-UNDO.
DEF INPUT PARAM icMethod      AS CHAR   NO-UNDO.

IF ihObject = THIS-PROCEDURE AND VALID-HANDLE(ihWidget) AND ihWidget:TYPE = "menu-item" THEN 
  FIND FIRST ttPanel
       WHERE ttPanel.cLabel = ihWidget:LABEL
       NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteItem C-Win 
FUNCTION DeleteItem RETURNS LOGICAL
  ( INPUT iiBtnIndex AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iiBtnIndex > 0 THEN
  RUN PanelDeleteItem (iPanel,iiBtnIndex - 1).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dumpTT C-Win 
FUNCTION dumpTT RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",httPanel,0).

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getImageList C-Win 
FUNCTION getImageList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cImageList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getImgIndex C-Win 
FUNCTION getImgIndex RETURNS INTEGER
  ( INPUT icImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iImgIdx AS INT NO-UNDO.

IF icImage NE "" THEN DO:  
  IF cLargeImageList NE "" THEN DO:
    iImgIdx = LOOKUP(icImage,cLargeImageList,";") - 1.
    IF iImgIdx < 0 THEN DO:
      IF INDEX(icImage,"\") > 0 THEN 
        icImage = SUBSTR(icImage,R-INDEX(icImage,"\") + 1).  
      ELSE IF INDEX(icImage,"/") > 0 THEN  
        icImage = SUBSTR(icImage,R-INDEX(icImage,"/") + 1).  
      iImgIdx = LOOKUP(icImage,cLargeImageList,";") - 1.
    END.
  END.
  ELSE iImgIdx = -1.

  IF iImgIdx < 0 THEN DO:
    iImgIdx = LOOKUP(icImage,cImageList,";") - 1.
    IF iImgIdx < 0 THEN DO:
      IF INDEX(icImage,"\") > 0 THEN 
        icImage = SUBSTR(icImage,R-INDEX(icImage,"\") + 1).  
      ELSE IF INDEX(icImage,"/") > 0 THEN  
        icImage = SUBSTR(icImage,R-INDEX(icImage,"/") + 1).  
      iImgIdx = LOOKUP(icImage,cImageList,";") - 1.
    END.
  END.
END.

RETURN iImgIdx.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLargeImageList C-Win 
FUNCTION getLargeImageList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cLargeImageList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPanelAction C-Win 
FUNCTION getPanelAction RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF AVAIL ttPanel THEN
  RETURN ttPanel.cAction.
ELSE
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPanelFrame C-Win 
FUNCTION getPanelFrame RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME panel-frame:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPanelHandle C-Win 
FUNCTION getPanelHandle RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iPanel.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitPanel C-Win 
FUNCTION InitPanel RETURNS HANDLE
  ( INPUT ihPanelRect    AS HANDLE,
    INPUT iiButtonWidth  AS INT,
    INPUT iiButtonHeight AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hParent) THEN hParent = SOURCE-PROCEDURE.

DO WITH FRAME panel-frame:
  ASSIGN FRAME panel-frame:WIDTH-PIXELS  = ihPanelRect:WIDTH-PIXELS
         FRAME panel-frame:HEIGHT-PIXELS = ihPanelRect:HEIGHT-PIXELS
         FRAME panel-frame:VIRTUAL-WIDTH-PIXELS = ihPanelRect:WIDTH-PIXELS
         FRAME panel-frame:VIRTUAL-HEIGHT-PIXELS = ihPanelRect:HEIGHT-PIXELS
         FRAME panel-frame:X = ihPanelRect:X
         FRAME panel-frame:Y = ihPanelRect:Y
         .

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME panel-frame:HANDLE,"panel-frame").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME panel-frame:HANDLE,"panel-frame").

  RUN PanelCreate(FRAME panel-frame:HWND,{&GEN_DISABLE}, OUTPUT iPanel).

  IF iiButtonHeight NE 0 THEN
    RUN PanelSetHeight(iPanel,iiButtonHeight).
/*   ELSE                                                           */
/*     RUN PanelSetHeight(iPanel,FRAME panel-frame:HEIGHT-PIXELS).  */
  IF iiButtonWidth NE 0 THEN
    RUN PanelSetWidth(iPanel,iiButtonWidth).
  ELSE
    RUN PanelSetWidth(iPanel,FRAME panel-frame:WIDTH-PIXELS).
  
/*   InitPanelFont("Arial",8).  */

  RUN PanelSetProperty(iPanel,{&PANEL_TOOLTIPS},{&GEN_ENABLE}).
  RUN PanelSetProperty(iPanel,{&PANEL_FLATBUTTONS},{&GEN_ENABLE}).
  RUN PanelSetProperty(iPanel,{&PANEL_HOTTRACK},{&GEN_ENABLE}).
  
/*   RUN PanelShow(iPanel).  */

  RETURN FRAME panel-frame:HANDLE.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitPanelFont C-Win 
FUNCTION InitPanelFont RETURNS LOGICAL
  ( INPUT icFontName  AS CHAR,
    INPUT iiSize      AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN PanelInitFont (iPanel,icFontName,iiSize).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButtonImage C-Win 
FUNCTION setButtonImage RETURNS LOGICAL
  ( INPUT iiBtnIndex AS INTEGER,
    INPUT icImage    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iImgIdx    AS INT NO-UNDO.

IF icImage NE "" THEN DO:    
  iImgIdx = getImgIndex(icImage).
  IF iImgIdx GE 0 AND iiBtnIndex > 0 THEN DO:
    RUN PanelSetItemImage(iPanel,iiBtnIndex - 1,iImgIdx).
    RETURN YES.
  END.
END.

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setImage C-Win 
FUNCTION setImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iImage = getImgIndex(icImage).
IF iImage < 0 THEN DO:
  MESSAGE "Image " icImage " not found in image-list"
          VIEW-AS ALERT-BOX WARNING.
  iImage = 0.
  RETURN NO.
END.
ELSE RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setImageList C-Win 
FUNCTION setImageList RETURNS LOGICAL
  ( INPUT icImageList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewImgList AS CHAR NO-UNDO.
DEF VAR cImgEntry   AS CHAR NO-UNDO.
DEF VAR cImg        AS CHAR NO-UNDO.

cImageList = icImageList.

IF cImageList NE "" THEN DO ix = 1 TO NUM-ENTRIES(cImageList,";"):
  
  cImgEntry = ENTRY(ix,cImageList,";").

  IF cImgEntry = "" THEN NEXT.

  IF R-INDEX(cImgEntry,"/") > 0 THEN
    cImg = ENTRY(NUM-ENTRIES(cImgEntry,"/"),cImgEntry,"/").
  ELSE IF R-INDEX(cImgEntry,"\") > 0 THEN
    cImg = ENTRY(NUM-ENTRIES(cImgEntry,"\"),cImgEntry,"\").
  ELSE IF SEARCH(cImgEntry) = ? THEN DO:
    IF SEARCH("bmp\" + cImgEntry) NE ? THEN
      cImgEntry = "bmp\" + cImgEntry.
    ELSE IF SEARCH("ico\" + cImgEntry) NE ? THEN
      cImgEntry = "ico\" + cImgEntry.
  END.

  IF SEARCH(SESSION:TEMP-DIR + cImg) = ? AND SEARCH(cImgEntry) NE ? THEN DO:
    FILE-INFO:FILE-NAME = cImgEntry.
    OS-COPY VALUE(FILE-INFO:FULL-PATHNAME) VALUE(SESSION:TEMP-DIR).
  END.
  ELSE IF SEARCH(cImgEntry) = ? THEN
    MESSAGE "Invalid image entry: " cImgEntry SKIP
            "Must be in PROPATH"
            VIEW-AS ALERT-BOX WARNING.

  IF R-INDEX(cImgEntry,"/") > 0 THEN DO:    
    IF LOOKUP(SUBSTR(cImgEntry,R-INDEX(cImgEntry,"/") + 1),cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + SUBSTR(cImgEntry,R-INDEX(cImgEntry,"/") + 1) + ";".
  END.
  ELSE IF R-INDEX(cImgEntry,"\") > 0 THEN DO:
    IF LOOKUP(SUBSTR(cImgEntry,R-INDEX(cImgEntry,"\") + 1),cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + SUBSTR(cImgEntry,R-INDEX(cImgEntry,"\") + 1) + ";".
  END.
  ELSE IF LOOKUP(cImgEntry,cNewImgList,";") = 0 THEN
    cNewImgList = cNewImgList + cImgEntry + ";".

END.
ASSIGN cNewImgList = TRIM(cNewImgList,";")
       cImagePath  = TRIM(SESSION:TEMP-DIR,"\")
       cImageList  = cNewImgList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

