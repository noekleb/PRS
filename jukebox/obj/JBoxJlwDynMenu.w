&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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
&SCOPED-DEFINE PureABLWin 1

DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR iDummy              AS INT    NO-UNDO.
DEF VAR cPanelFile          AS CHAR   NO-UNDO.
DEF VAR hPanel              AS HANDLE NO-UNDO.
DEF VAR hMenuCompany        AS HANDLE NO-UNDO.
DEF VAR hNavBarSettingsMenu AS HANDLE NO-UNDO.
DEF VAR cCurrPanelFile      AS CHAR   NO-UNDO.
DEF VAR iMenuStyle          AS INT    NO-UNDO INIT ?. /* INIT 2.  0: Normal, 1: NavBar, 2: TreeView  */
DEF VAR hTree               AS HANDLE NO-UNDO.
DEF VAR cDefImage           AS CHAR   NO-UNDO.
DEF VAR cDefSelImage        AS CHAR   NO-UNDO.
DEF VAR iMainMenuId         AS INT    NO-UNDO.
DEF VAR hMenu               AS HANDLE NO-UNDO.
DEF VAR cAppImage           AS CHAR   NO-UNDO.
DEF VAR hTreeViewFrame      AS HANDLE NO-UNDO.
DEF VAR cEventMsgProg       AS CHAR   NO-UNDO.
DEF VAR bEnableBtnEvents    AS LOG    NO-UNDO.
DEF VAR bDefStartMax        AS LOG    NO-UNDO.
DEF VAR iWinSequence        AS INT    NO-UNDO.
DEF VAR hMultiple           AS HANDLE NO-UNDO.
DEF VAR hParameterField     AS HANDLE NO-UNDO.
DEF VAR iStartWinXpos       AS INT    NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR iNumCompanies       AS INT    NO-UNDO.
DEF VAR bWinMax             AS LOG    NO-UNDO.
DEF VAR bViewMenuPanel      AS LOG    NO-UNDO INIT YES.
DEF VAR iMenuId             AS INT    NO-UNDO.

DEF TEMP-TABLE ttProgram 
    FIELD cProcName     AS CHAR 
    FIELD cMenuTitle    AS CHAR 
    FIELD hWinProc      AS HANDLE
    FIELD hWinMenuItem  AS HANDLE
    FIELD hParent       AS HANDLE
    FIELD bChildWin     AS LOG
    FIELD iSequence     AS INT
    .

DEF VAR hWinMenu AS HANDLE NO-UNDO.

DEF VAR httMenu           AS HANDLE NO-UNDO.
DEF VAR httMenuBuffer     AS HANDLE NO-UNDO.
DEF VAR hMainMenu         AS HANDLE NO-UNDO.

DEF VAR hTimer            AS HANDLE NO-UNDO.
DEF VAR iTimerInterval    AS INT    NO-UNDO.
DEF VAR cTimerServerProc  AS CHAR   NO-UNDO.
DEF VAR cTimerClientProc  AS CHAR   NO-UNDO.
DEF VAR cLastTimerEvent   AS CHAR   NO-UNDO.
DEF VAR cTimerContext     AS CHAR   NO-UNDO.
DEF VAR iCurrMenuStyle    AS INT    NO-UNDO.
DEF VAR cPanelURL         AS CHAR   NO-UNDO.

DEF STREAM sEventStatus.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnEvents rect-panel cmbGoTo dummyButton 
&Scoped-Define DISPLAYED-OBJECTS cmbGoTo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DragDrop C-Win 
FUNCTION DragDrop RETURNS LOGICAL
  ( INPUT icNodeKeys AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBtnEventsHandle C-Win 
FUNCTION getBtnEventsHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCmbGoToHandle C-Win 
FUNCTION getCmbGoToHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentMenuItem C-Win 
FUNCTION getCurrentMenuItem RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGoToList C-Win 
FUNCTION getGoToList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMainMenuId C-Win 
FUNCTION getMainMenuId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuBuffer C-Win 
FUNCTION getMenuBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuImages C-Win 
FUNCTION getMenuImages RETURNS CHARACTER
  ( INPUT icType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuItem C-Win 
FUNCTION getMenuItem RETURNS HANDLE
  ( INPUT icLaunch   AS CHAR,
    INPUT icLabel    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuStyle C-Win 
FUNCTION getMenuStyle RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParameter C-Win 
FUNCTION getParameter RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProgramBuffer C-Win 
FUNCTION getProgramBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTreeHandle C-Win 
FUNCTION getTreeHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDisabledMenus C-Win 
FUNCTION setDisabledMenus RETURNS LOGICAL
  ( INPUT ihMenu     AS HANDLE,
    INPUT icDisabled AS CHAR   )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuPanel C-Win 
FUNCTION setMenuPanel RETURNS LOGICAL
  ( INPUT icPanel AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuStyle C-Win 
FUNCTION setMenuStyle RETURNS LOGICAL
  ( INPUT iiMenuStyle AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPanelFile C-Win 
FUNCTION setPanelFile RETURNS LOGICAL
  ( INPUT icPanelFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTimerProperties C-Win 
FUNCTION setTimerProperties RETURNS LOGICAL
  ( INPUT iiTimerInterval   AS INT,
    INPUT icTimerContext    AS CHAR,
    INPUT icTimerServerProc AS CHAR,
    INPUT icTimerClientProc AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setViewMenuPanel C-Win 
FUNCTION setViewMenuPanel RETURNS LOGICAL
  ( INPUT ibViewMenuPanel AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWinStartXpos C-Win 
FUNCTION setWinStartXpos RETURNS LOGICAL
  ( INPUT iiStartWinXpos AS INT )  FORWARD.

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

DEFINE BUTTON dummyButton 
     LABEL "Just for focus" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbGoTo AS CHARACTER 
     LABEL "Gå til" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN AUTO-COMPLETION
     SIZE 39 BY 1 NO-UNDO.

DEFINE RECTANGLE rect-panel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 5 BY 3.14.

DEFINE RECTANGLE rect-tree
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 3.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnEvents AT ROW 1.24 COL 47.2
     cmbGoTo AT ROW 1.24 COL 6 COLON-ALIGNED HELP
          "Gå direkte til program"
     dummyButton AT ROW 2.52 COL 25
     rect-tree AT ROW 1 COL 1
     rect-panel AT ROW 1 COL 49
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.4 BY 3.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Hovedmeny"
         HEIGHT             = 3.14
         WIDTH              = 80.4
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/bullet_triangle_red.ico":U) THEN
    MESSAGE "Unable to load icon: ico/bullet_triangle_red.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
/* SETTINGS FOR RECTANGLE rect-tree IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rect-tree:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Hovedmeny */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Hovedmeny */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MAXIMIZED OF C-Win /* Hovedmeny */
DO:
  PROCESS EVENTS.
  IF cPanelURL NE "" AND cPanelURL NE ? THEN
    DYNAMIC-FUNCTION("NavigateToURL" IN hPanel,cPanelURL).

  bWinMax = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MINIMIZED OF C-Win /* Hovedmeny */
DO:
  DEF VAR hMenuItm AS HANDLE NO-UNDO.
  DEF VAR hDelItm  AS HANDLE NO-UNDO.
  IF VALID-HANDLE(hWinMenu) THEN DO:
    hMenuItm = hWinMenu:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hMenuItm):
      hDelItm = ?.
      FIND FIRST ttProgram 
           WHERE ttProgram.hWinMenuItem = hMenuItm
           NO-ERROR.
      IF AVAIL ttProgram AND VALID-HANDLE(ttProgram.hWinProc) THEN 
        ttProgram.hWinProc:CURRENT-WINDOW:WINDOW-STATE = WINDOW-MINIMIZED.
      ELSE hDelItm = hMenuItm.
      hMenuItm = hMenuItm:NEXT-SIBLING.
      IF hDelItm NE ? THEN DELETE OBJECT hDelItm NO-ERROR.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESTORED OF C-Win /* Hovedmeny */
DO:
  DEF VAR hMenuItm  AS HANDLE NO-UNDO.
  DEF VAR hDelItm   AS HANDLE NO-UNDO.
  DEF VAR hWinFrame AS HANDLE NO-UNDO.
  IF VALID-HANDLE(hWinMenu) THEN DO:
    hMenuItm = hWinMenu:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hMenuItm):
      hDelItm = ?.
      IF hMenuItm:CHECKED THEN DO:
        FIND FIRST ttProgram 
             WHERE ttProgram.hWinMenuItem = hMenuItm
             NO-ERROR.
        IF AVAIL ttProgram AND VALID-HANDLE(ttProgram.hWinProc) THEN DO:
          ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
          ttProgram.hWinProc:CURRENT-WINDOW:WINDOW-STATE = 3.
          RUN MoveToTop IN ttProgram.hWinProc NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
            hWinFrame = ttProgram.hWinProc:CURRENT-WINDOW:FIRST-CHILD.
            APPLY "entry" TO hWinFrame.
          END.
        END.
        ELSE hDelItm = hMenuItm.
      END.
      hMenuItm = hMenuItm:NEXT-SIBLING.
      IF hDelItm NE ? THEN DO: 
        DELETE OBJECT hDelItm NO-ERROR.
        hMenuItm:CHECKED = YES.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEvents
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEvents C-Win
ON CHOOSE OF btnEvents IN FRAME DEFAULT-FRAME /* Hendelser */
DO:
  DEF VAR ohWinHandle    AS HANDLE NO-UNDO.

  IF cEventMsgProg NE "" THEN
    RUN StartChildWindow(cEventMsgProg,"",?,NO,OUTPUT ohWinHandle).
  ELSE 
    MESSAGE PROGRAM-NAME(1) SKIP
            "Program for viewing events not set" SKIP
            VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbGoTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbGoTo C-Win
ON ENTRY OF cmbGoTo IN FRAME DEFAULT-FRAME /* Gå til */
DO:
  IF VALID-HANDLE(hWinMenu) THEN
    RUN KeepWindowOnTop IN THIS-PROCEDURE (hWinMenu).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbGoTo C-Win
ON RETURN OF cmbGoTo IN FRAME DEFAULT-FRAME /* Gå til */
DO:
  IF cmbGoTo:SCREEN-VALUE NE ? THEN
    RUN ApplyMenu(?,cmbGoTo:SCREEN-VALUE,"").
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
  FOR EACH ttProgram:
    IF VALID-HANDLE(ttProgram.hWinProc) THEN
      APPLY "close" TO ttProgram.hWinProc.
  END.
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize", THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hTree) THEN
    APPLY "close" TO hTree.
  IF VALID-HANDLE(hPanel) THEN
    APPLY "close" TO hPanel.
  IF VALID-HANDLE(hPanel) THEN DELETE PROCEDURE hPanel.
  RUN disable_UI.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

  ASSIGN btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = YES
/*          btnEventsDis:HIDDEN = YES  */
         cmbGoTo:HIDDEN = YES.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    setPanelFile("JBoxDynMenuPanel.w").
    RUN InitializeObject(0).
  &ENDIF
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}

ON "WINDOW-RESIZED" OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?) NO-ERROR.
  bWinMax = THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 1. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyMenu C-Win 
PROCEDURE ApplyMenu :
/*------------------------------------------------------------------------------
  Purpose:    Apply a menu label programatically, f.ex from the optional button-panel
  Parameters:  ihMenuInstance: Only needed when there are multiple dynamic menu instances in the application
               icLaunch:       Program file to be launched 
               icLabel:        Menu label to be launched (pre-translation)
               
  Notes:      Use primarly program name to get to the menu handle (the label might change).
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihMenuInstance AS HANDLE NO-UNDO.
DEF INPUT PARAM icLaunch       AS CHAR   NO-UNDO.
DEF INPUT PARAM icLabel        AS CHAR   NO-UNDO.

DEF VAR hMenuItem AS HANDLE NO-UNDO.

IF VALID-HANDLE(hMainMenu) AND VALID-HANDLE(ihMenuInstance) AND 
   ihMenuInstance NE THIS-PROCEDURE THEN RETURN.

IF icLaunch NE "" THEN DO:
  bOk = httMenuBuffer:FIND-FIRST("WHERE cLaunch = '" + icLaunch + "'") NO-ERROR.
  IF bOK THEN DO:
    IF httMenuBuffer:BUFFER-FIELD("iTvNavBarStyle"):BUFFER-VALUE > 0 THEN RUN tvNodeEvent ("select",STRING(httMenuBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)).
    ELSE DO:        
      hMenuItem = httMenuBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE.
      APPLY "choose" TO hMenuItem.
    END.
  END.
  ELSE RETURN "not avail".
END.
ELSE DO:
  bOK = httMenuBuffer:FIND-FIRST("WHERE cMenuLabel = '" + icLabel + "'") NO-ERROR.
  IF bOK THEN DO:
    IF httMenuBuffer:BUFFER-FIELD("iTvNavBarStyle"):BUFFER-VALUE > 0 THEN RUN tvNodeEvent ("select",STRING(httMenuBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)).
    ELSE DO:        
      hMenuItem = httMenuBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE.
      APPLY "choose" TO hMenuItem.
    END.
  END.
  ELSE RETURN "not avail".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BringToTop C-Win 
PROCEDURE BringToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWinProc    AS HANDLE NO-UNDO.
DEF INPUT PARAM ihWinMenu   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihMenuItem  AS HANDLE NO-UNDO.

DEF VAR hMenuItm  AS HANDLE NO-UNDO.
DEF VAR hWinFrame AS HANDLE NO-UNDO.

FIND FIRST ttProgram 
     WHERE ttProgram.hWinProc = ihWinProc NO-ERROR.
IF AVAIL ttProgram THEN DO:
  IF VALID-HANDLE(ttProgram.hWinProc) THEN DO:
    ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
    IF ttProgram.hWinProc:CURRENT-WINDOW:WINDOW-STATE NE WINDOW-MAXIMIZED THEN
      ttProgram.hWinProc:CURRENT-WINDOW:WINDOW-STATE = 3.
    RUN MoveToTop IN ttProgram.hWinProc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      hWinFrame = ttProgram.hWinProc:CURRENT-WINDOW:FIRST-CHILD.
      APPLY "entry" TO hWinFrame.
    END.
  END.
  ELSE DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.


/*   hMenuItm = ihWinMenu:FIRST-CHILD.    */
/*   REPEAT WHILE VALID-HANDLE(hMenuItm): */
/*     hMenuItm:CHECKED = NO.             */
/*     hMenuItm = hMenuItm:NEXT-SIBLING.  */
/*   END.                                 */

  hMenuItm = ihWinMenu:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hMenuItm):
    hMenuItm:CHECKED = hMenuItm = ttProgram.hWinMenuItem.
    hMenuItm = hMenuItm:NEXT-SIBLING.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildMenu C-Win 
PROCEDURE BuildMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMenuId     AS INT NO-UNDO.
DEF INPUT PARAM ihParent     AS HANDLE NO-UNDO.
DEF INPUT PARAM iiParentNode AS INT NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hMenuObject   AS HANDLE NO-UNDO.
DEF VAR hBuffParamFld AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE(httMenuBuffer).
hBuffParamFld = hBuffer:BUFFER-FIELD("cParameter") NO-ERROR.

hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH JBoxMenu WHERE JBoxMenu.iParentNodeIndex = " + STRING(iiParentNode) + " BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "MENU-ITEM" 
     AND hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE = "THIS-PROCEDURE" 
     AND NOT CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) 
     THEN DO:
    hQuery:GET-NEXT().
    NEXT.      
  END.

  IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "menubar" THEN DO:      
    iCurrMenuStyle = 0.
    RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                   ihParent,
                   hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).


  END.
  ELSE IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "tv-nav-bar" THEN DO:      
    iCurrMenuStyle = iMenuStyle.

/*     IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN         */
/*       cDefImage = hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.        */
/*     IF hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN      */
/*       cDefSelImage = hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE.  */

    RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                   ihParent,
                   hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
  END.

  IF hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE BEGINS "JBoxTimer" THEN DO:
    iTimerInterval  = INT(hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR OR iTimerInterval = 0 THEN iTimerInterval = 500000.
    cTimerServerProc = ENTRY(1,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
    IF cTimerServerProc = "" THEN cTimerServerProc = "jbserv_checkbroadcastmessage.p".
    IF NUM-ENTRIES(hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) > 1 THEN
      cTimerClientProc = ENTRY(2,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
    ELSE cTimerClientProc = "JBoxGetBroadCast.w".
    
    hQuery:GET-NEXT().
    NEXT.
  END.

  CASE STRING(hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE):
    WHEN "SUB-MENU" THEN DO:
      IF iCurrMenuStyle > 0 THEN 
         RUN addNode IN hTree (
                 STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),
                 STRING(iiMenuId),
                 hBuffer:BUFFER-FIELD("bHasChildSubMenu"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE,

                 IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
                   hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE
                 ELSE cDefImage 
                ,IF hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN
                   hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE
                 ELSE cDefSelImage 
                ,""
                ,hBuffer:BUFFER-FIELD("cFontStyle"):BUFFER-VALUE
                ,hBuffer:BUFFER-FIELD("cTextColor"):BUFFER-VALUE
                ,"enable"
                ,"sub-menu").
      ELSE 
        CREATE SUB-MENU hMenuObject
          ASSIGN PARENT = ihParent
                 LABEL  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 NAME   = "sm_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 .
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                     hMenuObject,
                     hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
    END.
    WHEN "PLACEHOLDER" THEN DO:
      IF iCurrMenuStyle = 0 THEN DO:
        CREATE SUB-MENU hMenuObject
          ASSIGN PARENT = ihParent
                 LABEL  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 NAME   = "ph_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 .
        IF hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE = "winmenu" THEN
          ON MENU-DROP OF hMenuObject PERSISTENT RUN KeepWindowOnTop IN THIS-PROCEDURE (hMenuObject).
      END.
    END.
    WHEN "MENU-ITEM" THEN DO:
      IF iCurrMenuStyle > 0 THEN DO:
        IF NOT CAN-DO("this-procedure,rule",hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE) THEN
          RUN addNode IN hTree (
                  STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),
                  STRING(iiMenuId),
                  NO,
                  hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                  hBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE,
                  IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
                    hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE
                  ELSE (IF iCurrMenuStyle = 1 THEN cDefSelImage ELSE cDefImage)
                 ,IF hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN
                    hBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE
                  ELSE cDefSelImage 
                 ,""
                 ,hBuffer:BUFFER-FIELD("cFontStyle"):BUFFER-VALUE
                 ,hBuffer:BUFFER-FIELD("cTextColor"):BUFFER-VALUE
                 ,"enable"
                 ,"menu-item").
      END.     

      ELSE CASE STRING(hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE):
        WHEN "START-WINDOW" THEN DO:
          IF VALID-HANDLE(hMultiple) THEN
            CREATE MENU-ITEM hMenuObject
              ASSIGN PARENT      = ihParent
                     LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
              TRIGGERS:
                ON CHOOSE PERSISTENT RUN StartWindow IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,
                                                                        hBuffer:BUFFER-FIELD("bMultiple"):BUFFER-VALUE,
                                                                        hMenuObject).
              END TRIGGERS.
          ELSE
            CREATE MENU-ITEM hMenuObject
              ASSIGN PARENT      = ihParent
                     LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
              TRIGGERS:
                ON CHOOSE PERSISTENT RUN StartWindow IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,
                                                                        FALSE,
                                                                        hMenuObject).
              END TRIGGERS.
        END.
        WHEN "THIS-PROCEDURE" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN VALUE(hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) IN THIS-PROCEDURE.
            END TRIGGERS.
        WHEN "DATA-BROWSE" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartDataBrw  IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,hMenuObject,
                                                                        (IF VALID-HANDLE(hBuffParamFld) THEN hBuffParamFld:BUFFER-VALUE ELSE "")).
            END TRIGGERS.
        WHEN "OS-COMMAND" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartOsCommand IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).
            END TRIGGERS.
        WHEN "URL" THEN
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartURL IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
            END TRIGGERS.
        OTHERWISE
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN StartProcedure IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,
                                                                         hMenuObject).
            END TRIGGERS.
      END CASE.
    END.
    WHEN "RULE" THEN DO:
      IF iCurrMenuStyle = 0 THEN        
        CREATE MENU-ITEM hMenuObject
          ASSIGN PARENT      = ihParent
                 SUBTYPE     = "rule"
                 .
    END.
  END CASE.

  IF hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE = "ChangeCompany" THEN 
    hMenuCompany = hMenuObject.

  hBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE = hMenuObject.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iCurrCompany     AS INT   NO-UNDO.
DEF VAR iMenuChoice      AS INT   NO-UNDO.

FIND FIRST ttProgram WHERE ttProgram.hWinMenuItem = SELF NO-ERROR.

iCurrCompany = DYNAMIC-FUNCTION("getCompanyId").

DYNAMIC-FUNCTION("setMenuRestart",YES).
RUN JBoxDSelectCompany.w (TRUE, INPUT-OUTPUT iDummy).
DYNAMIC-FUNCTION("setMenuRestart",NO).

IF DYNAMIC-FUNCTION("getCompanyId") NE iCurrCompany AND
   CAN-DO(hParent:INTERNAL-ENTRIES,"RestartMenu")
   THEN DO:
  RUN SelectRestartMenu (OUTPUT iMenuChoice).
  RUN SetRestart IN hParent (YES) NO-ERROR.
  APPLY "close" TO THIS-PROCEDURE.
  RUN RestartMenu IN hParent (iMenuChoice).
END.

ELSE IF DYNAMIC-FUNCTION("getCompanyId") NE iCurrCompany THEN DO:              
  setDisabledMenus(THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR:FIRST-CHILD,"").
  setMenuPanel("").
  APPLY "entry" TO dummyButton IN FRAME {&FRAME-NAME}.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckTimerEvent C-Win 
PROCEDURE CheckTimerEvent :
/*------------------------------------------------------------------------------
  Purpose:    Check on the server for any published information 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCurrTimerEvent AS CHAR NO-UNDO.
DEF VAR bViewBtnEvents  AS LOG  NO-UNDO.

IF cTimerServerProc NE "" THEN DO:
  DYNAMIC-FUNCTION("setShowHourGlass",FALSE).
  IF DYNAMIC-FUNCTION("runproc",cTimerServerProc,cTimerContext,?) THEN DO:
    cCurrTimerEvent = DYNAMIC-FUNCTION("getTransactionMessage").
    IF cCurrTimerEvent NE cLastTimerEvent THEN DO:
      IF cTimerClientProc NE "" THEN DO:
        IF cTimerClientProc = "setViewBtnEvents" THEN DO:
          bViewBtnEvents = LOGICAL(cCurrTimerEvent) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN
            RUN VALUE(cTimerClientProc) (bViewBtnEvents).
        END.
        ELSE RUN VALUE(cTimerClientProc) (cCurrTimerEvent).
      END.
      cLastTimerEvent = cCurrTimerEvent.
    END.
  END.
  DYNAMIC-FUNCTION("setShowHourGlass",TRUE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseWindow C-Win 
PROCEDURE CloseWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setCurrentSourceProc",THIS-PROCEDURE).
RUN SUPER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmbedInChildForm C-Win 
PROCEDURE EmbedInChildForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAM ihProc  AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAM obEmbed AS LOG NO-UNDO.

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
  DISPLAY cmbGoTo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnEvents rect-panel cmbGoTo dummyButton 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMenuImage C-Win 
PROCEDURE getMenuImage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icLaunch AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocImage  AS CHAR NO-UNDO.
  
bOk = httMenuBuffer:FIND-FIRST("WHERE cLaunch = '" + icLaunch + "'") NO-ERROR.
IF bOK THEN 
  ocImage = httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPlaceholderHandle C-Win 
PROCEDURE getPlaceholderHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icPlaceholderTag AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ohPlaceHolder    AS HANDLE NO-UNDO.

bOK = httMenuBuffer:FIND-FIRST("WHERE cMenuType = 'placeholder' AND cLaunch = '" + icPlaceHolderTag + "'") NO-ERROR.
IF bOk THEN
  ohPlaceHolder = httMenuBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE.
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
DEF INPUT PARAM iiMenuId    AS INT NO-UNDO.

DEF VAR iCompanyId       AS INT    NO-UNDO.
DEF VAR cReadOnlyUsers   AS CHAR   NO-UNDO.
DEF VAR cReadOnlyActions AS CHAR   NO-UNDO.
DEF VAR cImageList       AS CHAR   NO-UNDO.
DEF VAR cLargeImageList  AS CHAR   NO-UNDO.
DEF VAR cGoToList        AS CHAR   NO-UNDO.
DEF VAR hPHnavBarOptions AS HANDLE NO-UNDO.
DEF VAR iNodeIdx         AS INT    NO-UNDO.
DEF VAR cLogoImage       AS CHAR   NO-UNDO.
DEF VAR cAnyCompany      AS CHAR   NO-UNDO.
DEF VAR cViewAllModules  AS CHAR   NO-UNDO.

iCompanyId = DYNAMIC-FUNCTION("getCompanyId").
cAnyCompany = DYNAMIC-FUNCTION("getFieldValues","FIRST JBoxCompany","WHERE iJBoxCompanyId > 0","iJBoxCompanyId").
IF iCompanyId = 0 AND cAnyCompany NE ? AND cAnyCompany NE "" THEN DO:
  RUN JBoxDSelectCompany.w (TRUE,INPUT-OUTPUT iDummy).
  IF DYNAMIC-FUNCTION("getUserLevel") = "super" THEN 
    RUN SelectRestartMenu (OUTPUT iiMenuId).
END.

/* THIS-PROCEDURE:CURRENT-WINDOW:LOAD-ICON("ico\bullet_triangle_red.ico").  */
IF iiMenuId = 0 THEN DO:
  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iiMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
  ELSE 
    iiMenuId = INT(DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE cMenuType = 'menu'","iJBoxMenuId")).
END.

IF iiMenuId = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"No menu definitions found in database" + CHR(10)
                                 + DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  QUIT.
END.
iMenuId = iiMenuId.

/* Normally the session and company gobal objects are already created here.. */
DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
DYNAMIC-FUNCTION("NewObject",SESSION:FIRST-PROCEDURE,SESSION:FIRST-PROCEDURE,"Global variables").

DYNAMIC-FUNCTION("setAttribute",SESSION:FIRST-PROCEDURE,"CountryCode",
                 DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                  "WHERE cSysParamName =  'CountryCode'(Codemaster)",
                                  "cSysParamCharValue")).

SUBSCRIBE TO "RefreshMenu"          ANYWHERE.
SUBSCRIBE TO "getPlaceholderHandle" ANYWHERE.
SUBSCRIBE TO "getMenuImage"         ANYWHERE.
SUBSCRIBE TO "InvalidateHandle"     ANYWHERE.
SUBSCRIBE TO "ApplyMenu"            ANYWHERE.
SUBSCRIBE TO "setCompanyLogo"       ANYWHERE.
hMainMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle")) NO-ERROR.
/* If there is only one menu instance there is no need to set the mainmenuhandle attribute.
   Otherwise only the main menu will be the only place to start child windows: */
IF NOT VALID-HANDLE(hMainMenu) OR hMainMenu = THIS-PROCEDURE THEN DO:
  SUBSCRIBE TO "UserNotification" ANYWHERE.
  SUBSCRIBE TO "setViewBtnEvents" ANYWHERE.
  SUBSCRIBE TO "setMenuPanelURL"  ANYWHERE.
  SUBSCRIBE TO "setEventMsgProg"  ANYWHERE.
  SUBSCRIBE TO "StartChildWindow" ANYWHERE.
END.
  
SUBSCRIBE TO "EmbedInChildForm" ANYWHERE.

iMainMenuId = iiMenuId.
DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

httMenu = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                            ,"WHERE iJBoxMenuId = " + STRING(iMainMenuId) 
                          + "|" + (IF DYNAMIC-FUNCTION("getLanguageCode") NE DYNAMIC-FUNCTION("getBaseLanguageCode") THEN
                                     DYNAMIC-FUNCTION("getLanguageCode")
                                   ELSE "")
                          + "||" + DYNAMIC-FUNCTION("getAttribute",SESSION,"RestrictedUnlessAllowed")
                            ,?).

httMenuBuffer = httMenu:DEFAULT-BUFFER-HANDLE.
hParameterField = httMenuBuffer:BUFFER-FIELD("cParameter") NO-ERROR.
hMultiple = httMenuBuffer:BUFFER-FIELD("bMultiple") NO-ERROR.

IF iMenuStyle = ? THEN DO:
  bOk = httMenuBuffer:FIND-FIRST("WHERE cMenuType = 'tv-nav-bar'") NO-ERROR.
  IF bOk THEN
    iMenuStyle = httMenuBuffer:BUFFER-FIELD("iTvNavBarStyle"):BUFFER-VALUE.
END.

bOk = httMenuBuffer:FIND-FIRST("WHERE cMenuType = 'menu'") NO-ERROR.
IF bOk THEN DO:
  IF httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE MATCHES "*.ico" AND 
     SEARCH(httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE) NE ? THEN DO:
    cAppImage = httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.
    THIS-PROCEDURE:CURRENT-WINDOW:LOAD-ICON(cAppImage).
  END.
  IF httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE NE "" THEN
    THIS-PROCEDURE:CURRENT-WINDOW:TITLE = httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE.
  IF httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE NE "" AND SEARCH(httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE) NE ? THEN
    cLogoImage = httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.

  ASSIGN cPanelURL    = httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE
         cPanelFile   = (IF cPanelURL NE "" AND cPanelFile = "" THEN cPanelURL ELSE cPanelFile)
         bDefStartMax = httMenuBuffer:BUFFER-FIELD("bConfigurable"):BUFFER-VALUE
         iNodeIdx     = httMenuBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE 
         .
END.
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Error in menu definition - no root (menu) node defined","","").
  QUIT.    
END.

CREATE MENU hMenu.
THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR = hMenu.

IF iMenuStyle > 0 THEN DO WITH FRAME {&FRAME-NAME}:    

  cGoToList = getGoToList().
  PUBLISH "getEventMsgProc" (OUTPUT cEventMsgProg).
  IF cEventMsgProg NE "" AND SEARCH(cEventMsgProg) NE ? THEN
    ASSIGN bEnableBtnEvents = YES
           btnEvents:HIDDEN = NO
/*            btnEventsDis:HIDDEN = NO  */
           .
  ELSE cEventMsgProg = "".

  IF cGoToList NE "" THEN 
    ASSIGN cmbGoTo:DELIMITER = "|"
           cmbGoTo:LIST-ITEM-PAIRS = cGoToList
           cmbGoTo:HIDDEN = NO
           cmbGoTo:LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Gå til" ELSE "Go to"
           .
  ELSE cmbGoTo:HIDDEN = YES.

  IF NOT cmbGoTo:HIDDEN OR bEnableBtnEvents THEN
    ASSIGN rect-tree:HEIGHT-PIXELS = rect-tree:HEIGHT-PIXELS - 30
           rect-tree:Y = rect-tree:Y + 30.      
  ELSE IF cmbGoTo:HIDDEN THEN
    ASSIGN btnEvents:X = 2
/*            btnEventsDis:X = 2 */
           .

  ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS  = 250 
         THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = 650
         FRAME {&FRAME-NAME}:SCROLLABLE              = NO
         FRAME {&FRAME-NAME}:WIDTH-PIXELS            = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS 
         FRAME {&FRAME-NAME}:HEIGHT-PIXELS           = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS - 1
         rect-tree:WIDTH-PIXELS                      = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS
         rect-tree:HEIGHT-PIXELS                     = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS - 1
                                                     - (IF cGoToList NE "" OR NOT btnEvents:HIDDEN THEN 30 ELSE 0)
/*                                                      - 25 /* Status-area */  */
         iCurrMenuStyle                              = iMenuStyle
         NO-ERROR.
  RUN JBoxJLWtree.w PERSISTENT SET hTree.
  IF iMenuStyle = 1 THEN DO:
    cImageList = getMenuImages("small").
    DYNAMIC-FUNCTION("setImageList" IN hTree,cImageList).
    cLargeImageList = getMenuImages("large").
    IF cLargeImageList NE "" THEN
      DYNAMIC-FUNCTION("setLargeImageList" IN hTree,cLargeImageList).
    SUBSCRIBE TO "NavBarGroupChanged" IN hTree.
  END.
  ELSE DO:
    cImageList = getMenuImages("tree").
    DYNAMIC-FUNCTION("setImageList" IN hTree,cImageList).
  END.

  DYNAMIC-FUNCTION("setTreeStyle" IN hTree,iMenuStyle).
  DYNAMIC-FUNCTION("InitTreeView" IN hTree,rect-tree:HANDLE).
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "tvNodeEvent" IN hTree.
  hTreeViewFrame = DYNAMIC-FUNCTION("getTreeViewFrame" IN hTree).
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rect-tree").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, hTreeViewFrame,hTreeViewFrame:NAME).
END.

RUN BuildMenu (iMainMenuId,hMenu,iNodeIdx).

RUN getPlaceholderHandle("WinMenu",OUTPUT hWinMenu).

IF iMenuStyle = 2 THEN    
  DYNAMIC-FUNCTION("ExpandTree" IN hTree,"").
ELSE IF iMenuStyle > 0 THEN DO:
  RUN getPlaceholderHandle("NavBarOptions",OUTPUT hPHnavBarOptions).
  IF VALID-HANDLE(hPHnavBarOptions) THEN 
    DYNAMIC-FUNCTION("NewMenuBand",hPHnavBarOptions
              ,"Base;Base View&;NavBarViewType"
            + ",Flat;Flat View&;NavBarViewType"
            + ",Office1;Office 1 View&;NavBarViewType"
            + ",Office2;Office 2 View&;NavBarViewType"
            + ",Office3;Office 3 View&;NavBarViewType"
            + ",VsToolbox;VS Toolbox View&;NavBarViewType"
            + ",XP1;XP 1 View&;NavBarViewType"
            + ",XP2;XP 2 View&;NavBarViewType"
            + ",ExplorerBar;Explorer Bar View&;NavBarViewType"
            + ",UltaFlatExplorer;Ultra flat Explorer View&;NavBarViewType"
            + ",AdvancedExplorerBar;Advanced Explorer Bar View&;NavBarViewType"
            + ",XPExplorerBar;XP Explorer Bar View&;NavBarViewType"
            + ",Office11;Office 11 View&;NavBarViewType"
            + ",Office11NavPane;Office 11 Nav Pane View&;NavBarViewType"
            + ",SkinExplorer;Skin Explorer View&;NavBarViewType"
            + ",Skin;Skin View&;NavBarViewType"
            ,"").

  IF cLogoImage NE "" THEN 
    DYNAMIC-FUNCTION("setNavBarImage" IN hTree,cLogoImage).
END.


{&WINDOW-NAME}:TITLE = DYNAMIC-FUNCTION("getAppTitle").

/* iCompanyId = DYNAMIC-FUNCTION("getCompanyId").                                                                                         */
/* IF iCompanyId = 0 AND DYNAMIC-FUNCTION("getFieldValues","FIRST JBoxCompany","WHERE iJBoxCompanyId > 0","iJBoxCompanyId") NE ? THEN DO: */
/*   IF VALID-HANDLE(hMenuCompany) THEN APPLY "choose" TO hMenuCompany.                                                                   */
/*   ELSE RUN JBoxDSelectCompany.w (TRUE,INPUT-OUTPUT iDummy).                                                                            */
/* END.                                                                                                                                   */
/* ELSE DO:  */
  DYNAMIC-FUNCTION("getFunctionRestrictions").    
  DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW).
/*   cLogoImage = DYNAMIC-FUNCTION("getCompanyLogo") NO-ERROR.                                                                                                  */
/*   IF ERROR-STATUS:ERROR THEN                                                                                                                                 */
/*     cLogoImage = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE cCodeType = 'CompanyLogo' AND iJBoxCompanyId = " + STRING(iCompanyId),"cCodeValue"). */
/*   IF cLogoImage NE ? AND cLogoImage NE "" THEN                                                                                                               */
/*     RUN setCompanyLogo(cLogoImage).                                                                                                                          */
/* END. */


IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","bSuperUser")) THEN
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","super").

ELSE DO:
  bOk = LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxCompanyUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'" +
                                                                    "  AND iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")),"bSuperUserCompany")) NO-ERROR.
  IF bOk THEN DO:
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","companysuper").
    setDisabledMenus(THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR:FIRST-CHILD,DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'SU-menu'")).
  END.
  ELSE DO:
    setDisabledMenus(THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR:FIRST-CHILD,DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType MATCHES '*SU-menu*'")).

    cReadOnlyUsers   = REPLACE(DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'ro-users'"),"|",",").
    cReadOnlyActions = REPLACE(DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'ro-actions'"),"|",",").
    IF cReadOnlyActions = "" THEN cReadOnlyActions = "new,edit,copy,delete,undo,save".
  
    IF CAN-DO(cReadOnlyUsers,DYNAMIC-FUNCTION("getASuserId")) THEN
      DYNAMIC-FUNCTION("setSecDisabledActions",cReadOnlyActions,"").
  END.
END.

DYNAMIC-FUNCTION("setMyEnableSaveSettingsMenu",NO).

IF NOT PROGRAM-NAME(2) BEGINS "TestRecord" AND SEARCH("ThreadProgBar.dll") NE ? THEN
  DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"progress-bar",0,YES,?).

IF iMenuStyle = 0 THEN DO:
  IF NOT setMenuPanel(cPanelFile) THEN 
    DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).
END.
ELSE DO:
  setMenuPanel(cPanelURL).
  IF VALID-HANDLE(hPanel) THEN
    SUBSCRIBE TO "NavigationComplete" IN hPanel NO-ERROR.
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).
END.

cLogoImage = DYNAMIC-FUNCTION("getCompanyLogo") NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  cLogoImage = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE cCodeType = 'CompanyLogo' AND iJBoxCompanyId = " + STRING(iCompanyId),"cCodeValue").
IF cLogoImage NE ? AND cLogoImage NE "" THEN DO:
  IF iMenuStyle > 0 THEN RUN setCompanyLogo(cLogoImage).
  ELSE IF VALID-HANDLE(hPanel) AND CAN-DO(hPanel:INTERNAL-ENTRIES,"setCompanyLogo") THEN
    RUN setCompanyLogo IN hPanel (cLogoImage).
END.

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

/* If the menu contains an entry for message publication (requires JukeBox messaging installed) 
   start the timer here: */
IF (NOT VALID-HANDLE(hMainMenu) OR hMainMenu = THIS-PROCEDURE) AND iTimerInterval > 0 THEN DO:
  RUN StartTimer.
  IF iTimerInterval > 1000 THEN
    RUN CheckTimerEvent.
END.

DO WITH FRAME {&FRAME-NAME}:
  APPLY "entry" TO dummyButton.
  dummyButton:VISIBLE = NO.
END.
IF VALID-HANDLE(hPanel) AND CAN-DO(hPanel:INTERNAL-ENTRIES,"setPanelFocus") THEN
  DYNAMIC-FUNCTION("setPanelFocus" IN hPanel).

IF iMenuStyle > 0 THEN
  DYNAMIC-FUNCTION("showTreeView" IN hTree,YES).

ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS + 1
       THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 1.

DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").

ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:X = 0
       THIS-PROCEDURE:CURRENT-WINDOW:Y = 0
       rect-tree:WIDTH-PIXELS = rect-tree:WIDTH-PIXELS + 2.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

/* RUN setViewBtnEvents (YES). */

PUBLISH "setProgBarProperty" ("text"," ").
PUBLISH "setProgBarProperty" ("position","0").

IF bDefStartMax THEN DO:
  PROCESS EVENTS.
  THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 1.
  bWinMax = YES.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  IF cPanelURL NE "" AND cPanelURL NE ? THEN
    DYNAMIC-FUNCTION("NavigateToURL" IN hPanel,cPanelURL).
  APPLY "entry" TO FRAME {&FRAME-NAME}.
END.

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
DEF INPUT PARAM ihWinProc AS HANDLE NO-UNDO.

FOR EACH ttProgram
    WHERE ttProgram.hParent = ihWinProc:
  IF VALID-HANDLE(ttProgram.hWinProc) THEN 
    APPLY "close" TO ttProgram.hWinProc.
  IF AVAIL ttProgram THEN DO:
    DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.
    DELETE ttProgram.
  END.
END.

FIND FIRST ttProgram 
     WHERE ttProgram.hWinProc = ihWinProc NO-ERROR.
IF AVAIL ttProgram THEN DO:
  DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.
  DELETE ttProgram.
END.

FOR EACH ttProgram BY ttProgram.iSequence DESC:
  IF VALID-HANDLE(ttProgram.hWinMenuItem) AND VALID-HANDLE(ttProgram.hWinProc) THEN DO:
    ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
    ttProgram.hWinMenuItem:CHECKED = YES.
    LEAVE.
  END.
END.

IF ihWinProc = hPanel THEN APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KeepWindowOnTop C-Win 
PROCEDURE KeepWindowOnTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWinMenu AS HANDLE NO-UNDO.

DEF VAR hMenuItm AS HANDLE NO-UNDO.
DEF VAR hDelItm  AS HANDLE NO-UNDO.
DEF VAR bFound   AS LOG    NO-UNDO.
hMenuItm = ihWinMenu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMenuItm):
  hDelItm = ?.
  IF hMenuItm:CHECKED THEN DO:
    FIND ttProgram 
         WHERE ttProgram.hWinMenuItem = hMenuItm
         NO-ERROR.
    IF AVAIL ttProgram AND VALID-HANDLE(ttProgram.hWinProc) AND ttProgram.hWinProc:CURRENT-WINDOW:X > rect-tree:WIDTH-PIXELS IN FRAME {&FRAME-NAME} THEN DO:
      ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
      bFound = YES.
    END.
    ELSE IF NOT AVAIL ttProgram OR NOT VALID-HANDLE(ttProgram.hWinProc) THEN 
      hDelItm = hMenuItm.
  END.
  hMenuItm = hMenuItm:NEXT-SIBLING.
  IF hDelItm NE ? THEN
    DELETE OBJECT hDelItm NO-ERROR.
END.
IF NOT bFound THEN DO:
  FOR EACH ttProgram BY ttProgram.iSequence DESC:
    IF VALID-HANDLE(ttProgram.hWinMenuItem) AND VALID-HANDLE(ttProgram.hWinProc) AND ttProgram.hWinMenuItem:CHECKED THEN DO:
      ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
/*       ttProgram.hWinMenuItem:CHECKED = YES.  */
/*       bFound = YES.                          */
      LEAVE.
    END.
/*     ELSE DELETE ttProgram.  */
  END.
END.
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
/* IF iMenuStyle > 0 AND cPanelURL NE "" AND cPanelURL NE ? THEN  */
/*   DYNAMIC-FUNCTION("NavigateToURL" IN hPanel,cPanelURL).       */
IF NOT cmbGoTo:HIDDEN IN FRAME {&FRAME-NAME} THEN
  APPLY "entry" TO cmbGoTo.
ELSE APPLY "entry" TO FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NavBarGroupChanged C-Win 
PROCEDURE NavBarGroupChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hWinMenu) THEN
  RUN KeepWindowOnTop IN THIS-PROCEDURE (hWinMenu).
IF NOT cmbGoTo:HIDDEN IN FRAME {&FRAME-NAME} THEN
  APPLY "entry" TO cmbGoTo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NavBarViewType C-Win 
PROCEDURE NavBarViewType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cMenuViewList AS CHAR NO-UNDO
    INIT "Base,Flat,Office1,Office2,Office3,VsToolbox,XP1,XP2,ExplorerBar,UltaFlatExplorer,AdvancedExplorerBar,XPExplorerBar,Office11,Office11NavPane,SkinExplorer,Skin".

DYNAMIC-FUNCTION("setViewType" IN hTree,
                 LOOKUP(DYNAMIC-FUNCTION("getCurrentAction"),cMenuViewList)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NavigationComplete C-Win 
PROCEDURE NavigationComplete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icURL AS CHAR NO-UNDO.

icURL = REPLACE(icURL,"\","/").
bOk = httMenuBuffer:FIND-FIRST("WHERE cLaunch BEGINS '" + ENTRY(1,ENTRY(NUM-ENTRIES(icURL,"/"),icURL,"/"),".") + "'") NO-ERROR.

IF bOk AND NOT CAN-DO("menu,tv-nav-bar,sub-menu",httMenuBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE) THEN DO:
  PROCESS EVENTS.
  RUN tvNodeEvent ("select",httMenuBuffer:BUFFER-FIELD("iJBoxMenuId"):STRING-VALUE).
  IF AVAIL ttProgram THEN DO:
    PROCESS EVENTS.
    RUN MoveToTop IN ttProgram.hWinProc NO-ERROR.
    IF ERROR-STATUS:ERROR AND VALID-HANDLE(ttProgram.hWinProc:CURRENT-WINDOW) THEN
      ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshMenu C-Win 
PROCEDURE RefreshMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icSelectNode AS CHAR NO-UNDO.

DEF VAR cLargeImageList AS CHAR NO-UNDO.
DEF VAR cImageList      AS CHAR NO-UNDO.

httMenuBuffer:EMPTY-TEMP-TABLE().
             
DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                  ,"WHERE iJBoxMenuId = " + STRING(iMainMenuId) 
                  ,httMenuBuffer).

DELETE OBJECT hMenu.

CREATE MENU hMenu.
THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR = hMenu.


  IF iMenuStyle = 1 THEN DO:
      cLargeImageList = getMenuImages("large").
    ASSIGN cImageList = getMenuImages(IF cLargeImageList NE "" THEN "small" ELSE "")
           cDefImage  = (IF cDefImage = "" THEN "bullet_ball_blue.ico" ELSE cDefImage)
           .
    IF cLargeImageList NE "" THEN
      DYNAMIC-FUNCTION("setLargeImageList" IN hTree,cLargeImageList).
    cImageList = cImageList + ";" + "ico\bullet_triangle_blue.ico;ico\bullet_ball_blue.ico".
  END.
  ELSE 
    ASSIGN cImageList   = getMenuImages("") + ";bmp\folderclosed.bmp;bmp\folderopen.bmp"
           cDefImage    = "bmp\folderclosed.bmp"
           cDefSelImage = "bmp\folderopen.bmp"
           .
DYNAMIC-FUNCTION("setImageList" IN hTree,cImageList).

/* Call after images have been set cause it re-initiates the tree: */
DYNAMIC-FUNCTION("DeleteNodes" IN hTree,"").

RUN BuildMenu (iMainMenuId,hMenu,0).

IF iMenuStyle = 2 THEN DO:
  DYNAMIC-FUNCTION("ExpandTree" IN hTree,"").
  IF icSelectNode NE "" THEN
    DYNAMIC-FUNCTION("SelectNode" IN hTree,icSelectNode).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RestartMenu C-Win 
PROCEDURE RestartMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SetRestart IN hParent (YES) NO-ERROR.
APPLY "close" TO THIS-PROCEDURE.
RUN RestartMenu IN hParent (iMenuId).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectRestartMenu C-Win 
PROCEDURE SelectRestartMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM oiMenuChoice  AS INT NO-UNDO.

DEF VAR cCompanyMenu AS CHAR NO-UNDO.
DEF VAR cSuperMenu   AS CHAR NO-UNDO.
DEF VAR cReturn      AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getUserLevel") = "super" THEN DO:
  cCompanyMenu = DYNAMIC-FUNCTION("getFieldList",
                                  "JBoxCompanyMenu,JBoxMenu;cMenuLabel;iJBoxMenuId",
                                  "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                + ",FIRST JBoxMenu NO-LOCK OF JBoxCompanyMenu WHERE cMenuType = 'menu'"
                                  ).
  
  IF cCompanyMenu NE "" THEN DO:
    IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
      cSuperMenu = DYNAMIC-FUNCTION("getTransactionMessage").
    
    IF LOOKUP(cSuperMenu,cCompanyMenu,"|") = 0 THEN
      RUN JBoxDSimpleSelectList.w (cCompanyMenu + "|" 
                                 + DYNAMIC-FUNCTION("getFieldValues","JBoxMenu",
                                                    "WHERE iJBoxMenuId = " + cSuperMenu,"cMenuLabel,iJBoxMenuId")
                                  ,?
                                  ,OUTPUT cReturn).
    oiMenuChoice = INTEGER(cReturn).
  END.
END.
ELSE IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
  oiMenuChoice = INT(DYNAMIC-FUNCTION("getTransactionMessage")).



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
DEF INPUT PARAM icLogoFile AS CHAR NO-UNDO.

DEF VAR cFullName AS CHAR NO-UNDO.

IF NOT VALID-HANDLE(hTree) THEN RETURN.

IF SEARCH(icLogoFile) NE ? THEN DO:
  FILE-INFO:FILE-NAME = SEARCH(icLogoFile).
  cFullName = FILE-INFO:FULL-PATHNAME.
  DYNAMIC-FUNCTION("setNavBarImage" IN hTree,cFullName).
END.
ELSE DYNAMIC-FUNCTION("setNavBarImage" IN hTree,"").


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMenuPanelURL C-Win 
PROCEDURE setMenuPanelURL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icURL AS CHAR NO-UNDO.

cPanelURL = icURL.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setViewBtnEvents C-Win 
PROCEDURE setViewBtnEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       For the 1st time in 20 yrs I couldn't trust a Progress variable...
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibViewBtnEvents AS LOG NO-UNDO.

DEF VAR cUserId AS CHAR NO-UNDO.
/*
cUserId = DYNAMIC-FUNCTION("getASuserId").

IF SOURCE-PROCEDURE:FILE-NAME NE THIS-PROCEDURE:FILE-NAME THEN DO:
  OUTPUT STREAM sEventStatus TO VALUE(SESSION:TEMP-DIRECTORY + cUserId + "_eventstat.msg").
  PUT STREAM sEventStatus UNFORMATTED ibViewBtnEvents SKIP.
  OUTPUT STREAM sEventStatus CLOSE.
END.
ELSE IF SEARCH(SESSION:TEMP-DIRECTORY + cUserId + "_eventstat.msg") NE ? THEN DO:
  INPUT STREAM sEventStatus FROM VALUE(SESSION:TEMP-DIRECTORY + cUserId + "_eventstat.msg").
  IMPORT STREAM sEventStatus ibViewBtnEvents.
  INPUT STREAM sEventStatus CLOSE.
END.
*/
IF bEnableBtnEvents THEN DO WITH FRAME {&FRAME-NAME}:
  btnEvents:HIDDEN = NOT ibViewBtnEvents.
  /*
  btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = NO.
  IF ibViewBtnEvents AND SEARCH("bmp\dueledg.bmp") NE ? THEN DO:
/*     btnEventsDis:HIDDEN IN FRAME {&FRAME-NAME} = YES. */
/*     btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = NO. */
/*     btnEventsDis:MOVE-TO-BOTTOM(). */
    btnEvents:LOAD-IMAGE("bmp\dueledg.bmp").
    btnEvents:MOVE-TO-TOP().
  END.
  ELSE IF SEARCH("bmp\disdueledg.bmp") NE ? THEN DO:
/*     btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = YES.   */
/*     btnEventsDis:HIDDEN IN FRAME {&FRAME-NAME} = NO. */
/*     btnEvents:MOVE-TO-BOTTOM().                      */
/*     btnEventsDis:MOVE-TO-TOP(). */
    btnEvents:LOAD-IMAGE("bmp\disdueledg.bmp").
    btnEvents:MOVE-TO-TOP().
  END.
  */
END.
ELSE DO WITH FRAME {&FRAME-NAME}:
  btnEvents:HIDDEN IN FRAME {&FRAME-NAME} = YES.
/*   btnEventsDis:HIDDEN IN FRAME {&FRAME-NAME} = YES. */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartChildWindow C-Win 
PROCEDURE StartChildWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  icProcName:   File name 
               icWinTitle:  If blank the window will inherit the child win title
               ihParent:    Calling procedure
               ibNew:       TRUE if multiple instances allowed
               ohWinHandle: Procedure handle to child
                
  Notes:       Invoke procedure "InitializeChild" in the calling procedure
               to set attributes in the child prior to InitializeObject
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icProcName      AS CHAR   NO-UNDO.
DEF INPUT  PARAM icWinTitle     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.

DEF VAR hMenuItm AS HANDLE NO-UNDO.

IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN
  RETURN.

FIND FIRST ttProgram 
     WHERE ttProgram.cProcName = icProcName NO-ERROR.
IF NOT AVAIL ttProgram OR ibNew OR (AVAIL ttProgram AND NOT ttProgram.bChildWin) THEN DO:
  CREATE ttProgram.
  ASSIGN ttProgram.cProcName     = icProcName
         ttProgram.cMenuTitle   = icWinTitle
         ttProgram.hParent      = ihParent
         ttProgram.bChildWin    = TRUE
         iWinSequence           = iWinSequence + 1
         ttProgram.iSequence    = iWinSequence
         .
  
  RUN VALUE(icProcName) PERSIST SET ttProgram.hWinProc.

  IF NOT VALID-HANDLE(hWinMenu) THEN DO:
    CREATE SUB-MENU hWinMenu
           ASSIGN PARENT = THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR
                  LABEL = "Vindu"
                  NAME  = "m_Vindu"
                  .
    DYNAMIC-FUNCTION("InitTranslation",hWinMenu).
  END.
  CREATE MENU-ITEM ttProgram.hWinMenuItem
         ASSIGN TOGGLE-BOX = TRUE
                PARENT     = hWinMenu
                LABEL      = icWinTitle
                NAME       = "m_" + icWinTitle
                TRIGGERS:
                  ON VALUE-CHANGED PERSISTENT RUN BringToTop IN THIS-PROCEDURE (ttProgram.hWinProc,hWinMenu,ttProgram.hWinMenuItem).
                END TRIGGERS
                .

  IF icWinTitle NE "" THEN
    ttProgram.hWinProc:CURRENT-WINDOW:TITLE = ttProgram.cMenuTitle.
  
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"setParent") THEN 
    DYNAMIC-FUNCTION("setParent" IN ttProgram.hWinProc,ihParent).
  
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
    IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"InitializeChild") THEN
      RUN InitializeChild IN ihParent (ttProgram.hWinProc).
    RUN InitializeObject IN ttProgram.hWinProc.
  END.

  IF ttProgram.hWinMenuItem:LABEL = "" THEN
    ASSIGN ttProgram.cMenuTitle         = ttProgram.hWinProc:CURRENT-WINDOW:TITLE
           ttProgram.hWinMenuItem:LABEL = ttProgram.hWinProc:CURRENT-WINDOW:TITLE
           .

  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"ChangeCompany") THEN
    DYNAMIC-FUNCTION("setCompanyHeader",ttProgram.hWinProc:CURRENT-WINDOW).
END.
ELSE IF AVAIL ttProgram THEN
  ASSIGN ttProgram.cProcName          = icProcName
         ttProgram.cMenuTitle         = icWinTitle
         ttProgram.hParent            = ihParent
         ttProgram.hWinMenuItem:LABEL = IF icWinTitle NE "" THEN icWinTitle ELSE ttProgram.hWinMenuItem:LABEL
         ttProgram.hWinProc:CURRENT-WINDOW:TITLE = IF icWinTitle NE "" THEN icWinTitle ELSE ttProgram.hWinProc:CURRENT-WINDOW:TITLE
         .

IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"MoveToTop") THEN 
  RUN MoveToTop IN ttProgram.hWinProc.
ELSE DO:
  ttProgram.hWinProc:CURRENT-WINDOW:WINDOW-STATE = 3.
  ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
END.
hMenuItm = hWinMenu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMenuItm):
  hMenuItm:CHECKED = IF hMenuItm = ttProgram.hWinMenuItem THEN TRUE ELSE FALSE.
  hMenuItm = hMenuItm:NEXT-SIBLING.
END.

ohWinHandle = ttProgram.hWinProc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartDataBrw C-Win 
PROCEDURE StartDataBrw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icInitProgram AS CHAR   NO-UNDO.
DEF INPUT PARAM ihMenuObject  AS HANDLE NO-UNDO.
DEF INPUT PARAM icParamValue  AS CHAR   NO-UNDO.

DEF VAR hInitProc AS HANDLE NO-UNDO.

IF SEARCH(icInitProgram) NE ? OR SEARCH(SUBSTR(icInitProgram,1,LENGTH(icInitProgram) - 1) + "r") NE ? THEN DO:
  bOK = httMenuBuffer:FIND-FIRST("WHERE cLaunch = '" + icInitProgram + "'"
                               + (IF icParamValue NE "" THEN " AND cParameter = '" + icParamValue + "'" ELSE "")) NO-ERROR.  

  PUBLISH "setProgBarProperty" ("WindowStartup","").

  RUN StartWindow ("JBoxDataBrw.w",YES,ihMenuObject).

  IF VALID-HANDLE(hParameterField) AND hParameterField:BUFFER-VALUE NE "" THEN
    RUN VALUE(icInitProgram) PERSIST SET hInitProc (ttProgram.hWinProc,hParameterField:BUFFER-VALUE).
  ELSE
    RUN VALUE(icInitProgram) PERSIST SET hInitProc (ttProgram.hWinProc).

  IF VALID-HANDLE(hInitProc) AND NOT CAN-DO(hInitProc:INTERNAL-ENTRIES,"InvalidateHandle") THEN DELETE PROCEDURE hInitProc.    
      
  PUBLISH "setProgBarProperty" ("endWindowStartup","").

END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Cannot run program: " + icInitProgram,"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartOsCommand C-Win 
PROCEDURE StartOsCommand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiJboxMenuId AS INT NO-UNDO.

DEF VAR cFileName AS CHAR NO-UNDO.
DEF VAR cLaunch   AS CHAR NO-UNDO.
        
bOK = httMenuBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(iiJBoxMenuId)) NO-ERROR.

IF NOT bOk THEN DO:
  MESSAGE "Could not find menu id " + STRING(iiJBoxMenuId)
          "System configuration error"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

cFileName = SEARCH(httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
IF cFileName NE ? THEN DO:
  FILE-INFO:FILE-NAME = cFileName.  
  IF VALID-HANDLE(hParameterField) AND hParameterField:BUFFER-VALUE NE "" THEN
    OS-COMMAND NO-WAIT VALUE(QUOTER(FILE-INFO:FULL-PATHNAME) + " " + hParameterField:BUFFER-VALUE).
  ELSE
    OS-COMMAND NO-WAIT VALUE(QUOTER(FILE-INFO:FULL-PATHNAME)).
END.
ELSE DO:
  cLaunch = httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE.
  IF VALID-HANDLE(hParameterField) AND hParameterField:BUFFER-VALUE NE "" THEN
    OS-COMMAND NO-WAIT VALUE(QUOTER(cLaunch) + " " + hParameterField:BUFFER-VALUE).
  ELSE
    OS-COMMAND NO-WAIT VALUE(QUOTER(cLaunch)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartProcedure C-Win 
PROCEDURE StartProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icProcName     AS CHAR NO-UNDO.
DEF INPUT PARAM ihSelectedMenu AS HANDLE NO-UNDO.

DEF VAR hWin          AS HANDLE NO-UNDO.
DEF VAR iXpos         AS INT    NO-UNDO.
DEF VAR iYpos         AS INT    NO-UNDO.
DEF VAR hWinFrame     AS HANDLE NO-UNDO.
DEF VAR hMenuItm      AS HANDLE NO-UNDO.
DEF VAR cLabel        AS CHAR   NO-UNDO.
DEF VAR hCurrLastProc AS HANDLE NO-UNDO.

IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN
  RETURN.

SESSION:SET-WAIT-STATE("general").

hCurrLastProc = SESSION:LAST-PROCEDURE.

RUN VALUE(icProcName) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  RUN VALUE(icProcName) ("") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE PROGRAM-NAME(1) SKIP
            "Couldn't run " icProcName SKIP
            VIEW-AS ALERT-BOX.
END.
IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(SESSION:LAST-PROCEDURE:CURRENT-WINDOW) 
   AND SESSION:LAST-PROCEDURE NE hCurrLastProc THEN DO:

  IF NOT VALID-HANDLE(ihSelectedMenu) THEN
    cLabel = httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE.
  ELSE cLabel = ihSelectedMenu:LABEL.

  CREATE ttProgram.
  ASSIGN ttProgram.cProcName    = icProcName
         ttProgram.cMenuTitle   = cLabel
         ttProgram.hWinProc     = SESSION:LAST-PROCEDURE
         iWinSequence           = iWinSequence + 1
         ttProgram.iSequence    = iWinSequence
         hWin                   = ttProgram.hWinProc:CURRENT-WINDOW.
         .

  hWin:TITLE = ttProgram.cMenuTitle.

  IF NOT VALID-HANDLE(hWinMenu) THEN DO:
    CREATE SUB-MENU hWinMenu
           ASSIGN PARENT = THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR
                  LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vindu" ELSE "Window"
                  NAME  = "m_Vindu"
                  TRIGGERS:
                    ON MENU-DROP PERSISTENT RUN KeepWindowOnTop IN THIS-PROCEDURE (hWinMenu).
                  END TRIGGERS
                  .
    DYNAMIC-FUNCTION("InitTranslation",hWinMenu).
  END.
  CREATE MENU-ITEM ttProgram.hWinMenuItem
         ASSIGN TOGGLE-BOX = TRUE
                PARENT     = hWinMenu
                LABEL      = cLabel
                NAME       = "m_" + cLabel
                TRIGGERS:
                  ON VALUE-CHANGED PERSISTENT RUN BringToTop IN THIS-PROCEDURE (ttProgram.hWinProc,hWinMenu,ttProgram.hWinMenuItem).
                END TRIGGERS
                .

  IF iMenuStyle > 0 AND hWin:WINDOW-STATE NE WINDOW-MAXIMIZED THEN DO:
    IF THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS > rect-tree:WIDTH-PIXELS IN FRAME {&FRAME-NAME} + 2 THEN
      iYpos = THIS-PROCEDURE:CURRENT-WINDOW:Y + 48.
    ELSE iYpos = THIS-PROCEDURE:CURRENT-WINDOW:Y.
    iXpos = THIS-PROCEDURE:CURRENT-WINDOW:X + rect-tree:WIDTH-PIXELS + 7 +
            (IF THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = rect-tree:WIDTH-PIXELS IN FRAME {&FRAME-NAME} + 2 THEN 2 ELSE -3).
    IF iXpos + hWin:WIDTH-PIXELS > SESSION:WIDTH-PIXEL THEN
      iXpos = SESSION:WIDTH-PIXEL - hWin:WIDTH-PIXELS.
    DYNAMIC-FUNCTION("DoLockWindow",hWin).  
    ASSIGN hWin:X = iXpos
           hWin:Y = iYpos.
    DYNAMIC-FUNCTION("DoLockWindow",?).  
  END.
  ELSE IF hWin:WINDOW-STATE NE WINDOW-MAXIMIZED THEN DO:
    IF THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS < 250 AND
       hWin:WIDTH-PIXELS LE SESSION:WIDTH-PIXELS - (THIS-PROCEDURE:CURRENT-WINDOW:X + THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 2)
       THEN
      iXpos = THIS-PROCEDURE:CURRENT-WINDOW:X + THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 6.
    ELSE
      iXpos = 0.

    IF hWin:HEIGHT-PIXELS LE SESSION:HEIGHT-PIXELS - THIS-PROCEDURE:CURRENT-WINDOW:Y THEN
      iYpos = THIS-PROCEDURE:CURRENT-WINDOW:Y.
    ELSE
      iYpos = 100.

    DYNAMIC-FUNCTION("DoLockWindow",hWin).  
    ASSIGN hWin:X = iXpos
           hWin:Y = iYpos.
    DYNAMIC-FUNCTION("DoLockWindow",?).  
  END.

  IF httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE MATCHES "*.ico" THEN 
    hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE)).
  ELSE IF httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE MATCHES "*.ico" THEN 
    hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE)).
  ELSE IF hWin:ICON = "" AND cAppImage NE "" THEN 
    hWin:LOAD-ICON(cAppImage).


  IF hWin:WINDOW-STATE NE WINDOW-MAXIMIZED THEN
    hWin:WINDOW-STATE = 3.
  hWin:MOVE-TO-TOP().
  hWinFrame = hWin:FIRST-CHILD.
  APPLY "entry" TO hWinFrame.

  DYNAMIC-FUNCTION("DoLockWindow",?).  
  
  hMenuItm = hWinMenu:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hMenuItm):
    hMenuItm:CHECKED = hMenuItm = ttProgram.hWinMenuItem.
    hMenuItm = hMenuItm:NEXT-SIBLING.
  END.

END.
  
SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartTimer C-Win 
PROCEDURE StartTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH("controls.dll") NE ? AND SEARCH("JBoxJLWTimer.r") NE ? THEN
  RUN JBoxJLWtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
ELSE IF SEARCH("JBoxABLtimer.r") NE ? AND PROPATH MATCHES "*webclient*" THEN
  RUN JBoxABLtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
ELSE IF SEARCH("rstimer.ocx") NE ? AND PROPATH MATCHES "*webclient*" THEN
  RUN JBoxRsTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
ELSE
  RUN JBoxTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartURL C-Win 
PROCEDURE StartURL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icLaunch AS CHAR NO-UNDO.

IF NOT icLaunch BEGINS "mailto:" AND VALID-HANDLE(hPanel) AND CAN-DO(hPanel:INTERNAL-ENTRIES,"NavigateToURL") THEN DO:

  IF NOT THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 1 THEN DO:
    PROCESS EVENTS.
    THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 1.
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  END.
  
  DYNAMIC-FUNCTION("NavigateToURL" IN hPanel,icLaunch).
    
  APPLY "entry" TO FRAME {&FRAME-NAME}.
END.

ELSE DYNAMIC-FUNCTION("setWebDoc","",icLaunch).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartWindow C-Win 
PROCEDURE StartWindow :
/*------------------------------------------------------------------------------
  Purpose:    Excecute a menu command 
  Parameters: icProcName:      File name for selected menu item
              ibNew:          Set to enable more than one instance of program
              ihSelectedMenu: Handle to menu item (to get the menu label for window title)
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icProcName     AS CHAR   NO-UNDO.
DEF INPUT PARAM ibNew          AS LOG    NO-UNDO.
DEF INPUT PARAM ihSelectedMenu AS HANDLE NO-UNDO.

DEF VAR hMenuItm  AS HANDLE NO-UNDO.
DEF VAR cLabel    AS CHAR   NO-UNDO.
DEF VAR cDbLabel  AS CHAR   NO-UNDO.
DEF VAR hWin      AS HANDLE NO-UNDO.
DEF VAR hWinFrame AS HANDLE NO-UNDO.
DEF VAR iXpos     AS INT    NO-UNDO.
DEF VAR iYpos     AS INT    NO-UNDO.

IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN
  RETURN.

IF NOT ibNew THEN DO:
  FIND FIRST ttProgram 
       WHERE ttProgram.cProcName = icProcName NO-ERROR.
  IF AVAIL ttProgram AND NOT VALID-HANDLE(ttProgram.hWinProc) THEN
    DELETE ttProgram.
END.

IF NOT AVAIL ttProgram OR ibNew OR (AVAIL ttProgram AND ttProgram.bChildWin) THEN DO:
  IF NOT VALID-HANDLE(ihSelectedMenu) THEN
    ASSIGN cDbLabel = httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           cLabel   = REPLACE(cDbLabel,"&","").
  ELSE 
    ASSIGN cLabel   = ihSelectedMenu:LABEL
           cDbLabel = cLabel.

  IF icProcName NE "JBoxDataBrw.w" THEN DO:
    bOK = httMenuBuffer:FIND-FIRST("WHERE cLaunch = '" + icProcName + "' AND cMenuLabel = '" + cDbLabel + "'") NO-ERROR.
    PUBLISH "setProgBarProperty" ("WindowStartup","").
  END.

  CREATE ttProgram.
  ASSIGN ttProgram.cProcName    = icProcName
         ttProgram.cMenuTitle   = cLabel
         iWinSequence           = iWinSequence + 1
         ttProgram.iSequence    = iWinSequence
         .
  SESSION:SET-WAIT-STATE("general").


  IF icProcName NE "JBoxDataBrw.w" AND VALID-HANDLE(hParameterField) AND hParameterField:BUFFER-VALUE NE "" THEN
    RUN VALUE(icProcName) PERSIST SET ttProgram.hWinProc (hParameterField:BUFFER-VALUE) NO-ERROR.
  ELSE 
    RUN VALUE(icProcName) PERSIST SET ttProgram.hWinProc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    RUN VALUE(icProcName) PERSIST SET ttProgram.hWinProc ("") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Failed to start " icProcName SKIP 
              ERROR-STATUS:GET-MESSAGE(1)
              VIEW-AS ALERT-BOX ERROR.
      DELETE ttProgram.
      RETURN.
    END.
    ELSE IF VALID-HANDLE(ttProgram.hWinProc:NEXT-SIBLING) THEN
      ttProgram.hWinProc = ttProgram.hWinProc:NEXT-SIBLING.
  END.

  hWin = ttProgram.hWinProc:CURRENT-WINDOW.

  IF NOT VALID-HANDLE(hWinMenu) THEN DO:
    CREATE SUB-MENU hWinMenu
           ASSIGN PARENT = THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR
                  LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vindu" ELSE "Window"
                  NAME  = "m_Vindu"
                  TRIGGERS:
                    ON MENU-DROP PERSISTENT RUN KeepWindowOnTop IN THIS-PROCEDURE (hWinMenu).
                  END TRIGGERS
                  .
    DYNAMIC-FUNCTION("InitTranslation",hWinMenu).
  END.
  CREATE MENU-ITEM ttProgram.hWinMenuItem
         ASSIGN TOGGLE-BOX = TRUE
                PARENT     = hWinMenu
                LABEL      = cLabel
                NAME       = "m_" + cLabel
                TRIGGERS:
                  ON VALUE-CHANGED PERSISTENT RUN BringToTop IN THIS-PROCEDURE (ttProgram.hWinProc,hWinMenu,ttProgram.hWinMenuItem).
                END TRIGGERS
                .
  IF iMenuStyle > 0 THEN DO:
    IF THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS > rect-tree:WIDTH-PIXELS IN FRAME {&FRAME-NAME} + 2 THEN
      iYpos = THIS-PROCEDURE:CURRENT-WINDOW:Y + 46.
    ELSE iYpos = THIS-PROCEDURE:CURRENT-WINDOW:Y.
    iXpos = THIS-PROCEDURE:CURRENT-WINDOW:X + rect-tree:WIDTH-PIXELS + 7 +
            (IF THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = rect-tree:WIDTH-PIXELS IN FRAME {&FRAME-NAME} + 2 THEN 2 ELSE -3).
    IF iXpos + hWin:WIDTH-PIXELS > SESSION:WIDTH-PIXEL THEN
      iXpos = SESSION:WIDTH-PIXEL - hWin:WIDTH-PIXELS.
  END.
  ELSE DO:
    IF iStartWinXpos NE 0 THEN
      iXpos = iStartWinXpos.
    ELSE IF THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS < 250 AND
       hWin:WIDTH-PIXELS LE SESSION:WIDTH-PIXELS - (THIS-PROCEDURE:CURRENT-WINDOW:X + THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 2)
       THEN
      iXpos = THIS-PROCEDURE:CURRENT-WINDOW:X + THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 6.
    ELSE
      iXpos = 0.

    IF hWin:HEIGHT-PIXELS LE SESSION:HEIGHT-PIXELS - THIS-PROCEDURE:CURRENT-WINDOW:Y THEN
      iYpos = THIS-PROCEDURE:CURRENT-WINDOW:Y.
    ELSE
      iYpos = 100.
  END.

  DYNAMIC-FUNCTION("DoLockWindow",hWin).  
  ASSIGN hWin:X = iXpos
         hWin:Y = iYpos.

  hWin:TITLE = REPLACE(ttProgram.cMenuTitle,"&","").
  
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",hWin).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
    RUN InitializeObject IN ttProgram.hWinProc NO-ERROR.
  END.
  ELSE IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"local-initialize") THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",hWin).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
    RUN local-initialize IN ttProgram.hWinProc NO-ERROR.
  END.

  IF NOT VALID-HANDLE(hWin) THEN DO:
    DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.  
    DELETE PROCEDURE ttProgram.hWinProc NO-ERROR.
    IF AVAIL ttProgram THEN DELETE ttProgram.
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
    DYNAMIC-FUNCTION("DoLockWindow",?).  
    RETURN.
  END.
  
  IF httMenuBuffer:AVAIL THEN DO:
    IF httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE MATCHES "*.ico" THEN 
      hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE)).
    ELSE IF httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE MATCHES "*.ico" THEN 
      hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE)).
    ELSE IF hWin:ICON = "" AND cAppImage NE "" THEN 
      hWin:LOAD-ICON(cAppImage).
  END.

  IF iMenuStyle > 0 AND 
     ((hWin:HEIGHT-PIXELS < THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS - iYpos - (IF hWin:STATUS-AREA THEN 23 ELSE 1)
       AND hWin:WIDTH-PIXELS / THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS > 1
       AND DYNAMIC-FUNCTION("getObjectList",hWin) NE "" 
       AND DYNAMIC-FUNCTION("getAttribute",hWin,"keepInitialSize") NE "yes") OR
       httMenuBuffer:BUFFER-FIELD("bConfigurable"):BUFFER-VALUE AND bWinMax)
       THEN DO:
    DYNAMIC-FUNCTION("DoLockWindow",hWin).  
    IF iYpos > 0 THEN iYpos = iYpos + 7.
    hWin:HEIGHT-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS - iYpos 
                       - (IF hWin:STATUS-AREA OR DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"statusBarFrame") NE "" THEN 23 ELSE 1).
    IF httMenuBuffer:BUFFER-FIELD("bConfigurable"):BUFFER-VALUE AND bWinMax THEN
      hWin:WIDTH-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS - hWin:X - 5 NO-ERROR.
    APPLY "window-resized" TO hWin.
  END.

  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"ChangeCompany") OR DYNAMIC-FUNCTION("getCompanyId") > 0 THEN
    DYNAMIC-FUNCTION("setCompanyHeader",hWin).

  SESSION:SET-WAIT-STATE("").

  IF icProcName NE "JBoxDataBrw.w" THEN
    PUBLISH "setProgBarProperty" ("endWindowStartup","0").
END.
ELSE IF AVAIL ttProgram THEN
  hWin = ttProgram.hWinProc:CURRENT-WINDOW.

IF icProcName NE "JBoxDataBrw.w" THEN DO:
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"MoveToTop") THEN DO:
    IF PROVERSION GE "10.2" THEN RUN ShowForm IN ttProgram.hWinProc ("") NO-ERROR.
    RUN MoveToTop IN ttProgram.hWinProc.
  END.
  ELSE DO:
    IF ttProgram.hWinProc:ADM-DATA NE ? THEN DO:
      hWin:VISIBLE = NO.
      hWin:VISIBLE = YES.
    END.
    IF hWin:WINDOW-STATE NE WINDOW-MAXIMIZED THEN
      hWin:WINDOW-STATE = 3.
    hWin:MOVE-TO-TOP().
    hWinFrame = hWin:FIRST-CHILD.
    APPLY "entry" TO hWinFrame.
  END.
END.
    
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).  

hMenuItm = hWinMenu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMenuItm):
  hMenuItm:CHECKED = hMenuItm = ttProgram.hWinMenuItem.
  hMenuItm = hMenuItm:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvNodeEvent C-Win 
PROCEDURE tvNodeEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icEvent       AS CHAR NO-UNDO.
DEF INPUT PARAM icKeyValue    AS CHAR NO-UNDO.

IF CAN-DO("select",icEvent) THEN DO:
  bOK = httMenuBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + icKeyValue) NO-ERROR.
  IF bOK THEN DO:
    CASE STRING(httMenuBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE):
      WHEN "START-WINDOW" THEN 
        RUN StartWindow IN THIS-PROCEDURE (httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,
                                           httMenuBuffer:BUFFER-FIELD("bMultiple"):BUFFER-VALUE,
                                           ?).
      WHEN "THIS-PROCEDURE" THEN
        RUN VALUE(httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) IN THIS-PROCEDURE.
      WHEN "DATA-BROWSE" THEN
        RUN StartDataBrw  IN THIS-PROCEDURE (httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,?,
                                            (IF VALID-HANDLE(hParameterField) THEN hParameterField:BUFFER-VALUE ELSE "")).
      WHEN "OS-COMMAND" THEN
        RUN StartOsCommand  IN THIS-PROCEDURE (httMenuBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).
      WHEN "URL" THEN
        RUN StartURL IN THIS-PROCEDURE (httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
      OTHERWISE
        RUN StartProcedure IN THIS-PROCEDURE (httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,?).
    END CASE.
  END.  
END.
ELSE IF icEvent = "dragdrop" THEN
  DragDrop(icKeyValue).
ELSE IF VALID-HANDLE(hWinMenu) THEN
  RUN KeepWindowOnTop IN THIS-PROCEDURE (hWinMenu).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UserNotification C-Win 
PROCEDURE UserNotification :
/*------------------------------------------------------------------------------
  Purpose:    Display a user notification and return the response to calling program 
  Parameters:  <none>
  Notes:      If a handling proc is given the menu will try to start it 
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icShortMsg      AS CHAR NO-UNDO.
DEF INPUT  PARAM icLongMsg       AS CHAR NO-UNDO.
DEF INPUT  PARAM icHandlingProc  AS CHAR NO-UNDO.
DEF INPUT  PARAM icPublishTo     AS CHAR NO-UNDO.
DEF INPUT  PARAM icContext       AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocResponse      AS CHAR NO-UNDO.

IF VALID-HANDLE(hMainMenu) AND THIS-PROCEDURE NE hMainMenu THEN RETURN.

RUN JBoxNotifyUser.w PERSIST (icShortMsg,icLongMsg,icHandlingProc,icPublishTo,OUTPUT ocResponse).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DragDrop C-Win 
FUNCTION DragDrop RETURNS LOGICAL
  ( INPUT icNodeKeys AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDragId       AS CHAR NO-UNDO.
DEF VAR cDropId       AS CHAR NO-UNDO.
DEF VAR cDragLabel    AS CHAR NO-UNDO.
DEF VAR cDropLabel    AS CHAR NO-UNDO.
DEF VAR cDragType     AS CHAR NO-UNDO.
DEF VAR cDropType     AS CHAR NO-UNDO.
DEF VAR cDragParentId AS CHAR NO-UNDO.
DEF VAR cDropParentId AS CHAR NO-UNDO.
DEF VAR cDropSeq      AS CHAR NO-UNDO.
DEF VAR hMenu         AS HANDLE NO-UNDO.

RETURN NO.
/*
ASSIGN cDragId = ENTRY(1,icNodeKeys,"|")
       cDropId = ENTRY(2,icNodeKeys,"|").

bOK = httMenuBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + cDragId) NO-ERROR.
IF bOk THEN
  ASSIGN cDragLabel    = httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
         cDragType     = httMenuBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE
         cDragParentId = STRING(httMenuBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE)
         .
ELSE RETURN FALSE.

bOK = httMenuBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + cDropId) NO-ERROR.
IF bOk THEN DO:
  ASSIGN cDropLabel    = httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
         cDropType     = httMenuBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE
         cDropParentId = STRING(httMenuBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE)
         cDropSeq      = STRING(httMenuBuffer:BUFFER-FIELD("iSeq"):BUFFER-VALUE)
         .
  IF cDropType = "menu-item" THEN DO:
    cDropId = cDropParentId.
    bOK = httMenuBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + cDropParentId) NO-ERROR.
    IF bOk THEN
      ASSIGN cDropLabel = httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
             cDropSeq   = STRING(INT(cDropSeq) + 1)
             .
    ELSE RETURN NO.
  END.
END.
ELSE RETURN FALSE.

IF cDragId    = cDropId 
   OR cDropId = cDragParentId
   OR CAN-DO("placeholder,rule",cDropType)
   OR NOT httMenuBuffer:BUFFER-FIELD("bConfigurable"):BUFFER-VALUE
   THEN
  RETURN FALSE.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Copy " + CHR(10) + "  " 
                                  + cDragLabel + CHR(10)
                                  + "to " + CHR(10) + "  "
                                  + cDropLabel + CHR(10)
                                  ,"","") NE 1 THEN RETURN FALSE.

DYNAMIC-FUNCTION("DoCreate","JBoxMenuToMenu","ignore",
                 "iToMenuId,iFromMenuId,cJBoxUserId",
                 cDropId + "|" + cDragId + "|" + DYNAMIC-FUNCTION("getASuserId"),
                 TRUE). 
IF DYNAMIC-FUNCTION("getTransactionMessage") = "" THEN DO:

  IF iMenuStyle = 2 THEN DO:
      
    httMenuBuffer:EMPTY-TEMP-TABLE().
  
    httMenu = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                                ,"WHERE iJBoxMenuId = " + STRING(iMainMenuId) 
                                ,?).
  
    httMenuBuffer = httMenu:DEFAULT-BUFFER-HANDLE.
  
    DELETE OBJECT THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR.
  
    CREATE MENU hMenu.
    THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR = hMenu.
  
    DYNAMIC-FUNCTION("DeleteNodes" IN hTree,"").

    RUN BuildMenu (iMainMenuId,hMenu,0).
  
    DYNAMIC-FUNCTION("ExpandTree" IN hTree,"").
  END.

END.
ELSE RETURN FALSE.

RETURN TRUE. 
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBtnEventsHandle C-Win 
FUNCTION getBtnEventsHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN btnEvents:HANDLE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCmbGoToHandle C-Win 
FUNCTION getCmbGoToHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 
RETURN cmbGoTo:HANDLE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentMenuItem C-Win 
FUNCTION getCurrentMenuItem RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Get the handle to the last executed menu item 
    Notes:  
------------------------------------------------------------------------------*/
IF AVAIL ttProgram THEN
  RETURN ttProgram.hWinMenuItem.
ELSE
  RETURN SELF.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGoToList C-Win 
FUNCTION getGoToList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery    AS HANDLE NO-UNDO.
DEF VAR cGoToList AS CHAR   NO-UNDO.

CREATE QUERY hQuery.
hQuery:ADD-BUFFER(httMenuBuffer).
hQuery:QUERY-PREPARE("FOR EACH JBoxMenu WHERE "
                   + "cMenuType = 'menu-item' AND cMenuNumber NE ''"
                   + " BY cMenuNumber").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF LOOKUP(httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,cGoToList,"|") = 0 THEN
    cGoToList = cGoToList + (IF cGoToList NE "" THEN "|" ELSE "") 
              + httMenuBuffer:BUFFER-FIELD("cMenuNumber"):BUFFER-VALUE + " - "
              + SUBSTR(httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,1,R-INDEX(httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE," (")) + "|"
              + httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE
              .

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

RETURN cGoToList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMainMenuId C-Win 
FUNCTION getMainMenuId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iMainMenuId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuBuffer C-Win 
FUNCTION getMenuBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN httMenuBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuImages C-Win 
FUNCTION getMenuImages RETURNS CHARACTER
  ( INPUT icType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery    AS HANDLE NO-UNDO.
DEF VAR cImgList  AS CHAR   NO-UNDO.

CREATE QUERY hQuery.
hQuery:ADD-BUFFER(httMenuBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + httMenuBuffer:NAME + " WHERE "
                   + (IF icType = "large" THEN
                        "(cMenuType = 'sub-menu' AND cParentMenuType NE 'sub-menu') OR cMenuType = 'tv-nav-bar'"
                      ELSE IF icType = "small" THEN
                        "cMenuType = 'menu-item' OR cMenuType = 'tv-nav-bar' OR (cMenuType = 'sub-menu' AND cParentMenuType = 'sub-menu')"
                      ELSE "cMenuType NE 'menu'")
                   + " BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF httMenuBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "tv-nav-bar" THEN DO:
    IF icType = "large" AND httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
      ASSIGN cImgList = cImgList + httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE + ";".
    ELSE IF icType NE "large" THEN DO: 
      IF httMenuBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN 
        ASSIGN cImgList = cImgList + httMenuBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE + ";"
               cDefSelImage = httMenuBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE.
      IF httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN 
        ASSIGN cImgList = cImgList + httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE + ";"
               cDefImage = httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.
    END.
  END.
  ELSE DO:
    IF httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN DO:
      cImgList = cImgList + httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE + ";".
      IF httMenuBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "tv-nav-bar" THEN
        cDefImage = httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.
    END.
    ELSE IF icType NE "large" AND httMenuBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE NE "" THEN DO:
      cImgList = cImgList + httMenuBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE + ";".
      IF httMenuBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "tv-nav-bar" THEN
        cDefSelImage = httMenuBuffer:BUFFER-FIELD("cSelImage"):BUFFER-VALUE.
    END.
  END.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

cImgList = TRIM(cImgList,";").

IF icType = "tree" THEN DO:
  cImgList =  TRIM("tvpics\fold.bmp;tvpics\foldopen.bmp;" + cImgList,";").
  IF cDefImage = "" THEN
  ASSIGN cDefImage     =  "tvpics\fold.bmp"
         cDefSelImage  =  "tvpics\foldopen.bmp".
END.
ELSE DO: 
  IF icType = "large" THEN DO: 
    IF cDefImage = "" THEN
      ASSIGN cDefImage  =  "ico\folder.ico"
             cImgList   =  TRIM("ico\folder.ico;" + cImgList,";").
  END.
  IF icType = "small" OR icType = "" THEN DO: 
    cImgList =  TRIM("tvpics\fold.bmp;tvpics\foldopen.bmp;" + cImgList,";").
    IF cDefSelImage = "" THEN
      ASSIGN cDefSelImage = "bmp\active16.bmp"
             cImgList     = TRIM(cImgList + ";bmp\active16.bmp",";").
  END.
END.


RETURN cImgList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuItem C-Win 
FUNCTION getMenuItem RETURNS HANDLE
  ( INPUT icLaunch   AS CHAR,
    INPUT icLabel    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Get the handle to a menu item 
    Notes: Use either launch file name or menu label (pre-translation) 
------------------------------------------------------------------------------*/
DEF VAR hMenuItem AS HANDLE NO-UNDO.

IF icLaunch NE "" THEN DO:
  bOk = httMenuBuffer:FIND-FIRST("WHERE cLaunch = '" + icLaunch + "'") NO-ERROR.
  IF bOK THEN 
    hMenuItem = httMenuBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE.
END.
ELSE DO:
  bOK = httMenuBuffer:FIND-FIRST("WHERE cMenuLabel = '" + icLabel + "'") NO-ERROR.
  IF bOK THEN 
    hMenuItem = httMenuBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE.
END.
RETURN hMenuItem. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuStyle C-Win 
FUNCTION getMenuStyle RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Ask from menu-panel to decide wether to handle resize or not: 0 handle, > 0 leave up to menu
    Notes:  
------------------------------------------------------------------------------*/
RETURN iMenuStyle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParameter C-Win 
FUNCTION getParameter RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hParameterField) THEN
  RETURN hParameterField:BUFFER-VALUE.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProgramBuffer C-Win 
FUNCTION getProgramBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN BUFFER ttProgram:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTreeHandle C-Win 
FUNCTION getTreeHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hTree.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDisabledMenus C-Win 
FUNCTION setDisabledMenus RETURNS LOGICAL
  ( INPUT ihMenu     AS HANDLE,
    INPUT icDisabled AS CHAR   ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hMenuItm AS HANDLE NO-UNDO.

REPEAT WHILE VALID-HANDLE(ihMenu):
  bOK = httMenuBuffer:FIND-FIRST("WHERE hMenuItem = WIDGET-HANDLE('" + STRING(ihMenu) + "')") NO-ERROR.
  IF bOk THEN DO:
    IF LOOKUP(httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,icDisabled,"|") > 0 THEN
      ihMenu:SENSITIVE = FALSE.  
    ELSE ihMenu:SENSITIVE = DYNAMIC-FUNCTION("getMenuPermission",httMenuBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE).
  END.
  ELSE IF LOOKUP(ihMenu:LABEL,icDisabled,"|") > 0 THEN
    ihMenu:SENSITIVE = FALSE.

  IF CAN-QUERY(ihMenu,"first-child") THEN
    setDisabledMenus(ihMenu:FIRST-CHILD,icDisabled).
  ihMenu = ihMenu:NEXT-SIBLING.
END.
RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuPanel C-Win 
FUNCTION setMenuPanel RETURNS LOGICAL
  ( INPUT icPanel AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Nice buttons for most frequently used menu choices, logo, etc
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cCompanyPanel AS CHAR   NO-UNDO INIT ?.
DEF VAR bRefresh      AS LOG    NO-UNDO.

IF iMenuStyle > 0 THEN 
  ASSIGN rect-panel:X IN FRAME {&FRAME-NAME} = rect-tree:X + rect-tree:WIDTH-PIXELS
         rect-panel:HEIGHT-PIXELS = rect-tree:HEIGHT-PIXELS
         rect-panel:WIDTH-PIXELS = 1
         NO-ERROR.
ELSE
  ASSIGN rect-panel:X = FRAME {&FRAME-NAME}:X
         rect-panel:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
         rect-panel:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS
         rect-tree:HIDDEN = YES
         NO-ERROR.

rect-panel:HIDDEN IN FRAME {&FRAME-NAME} = YES.

IF NOT bViewMenuPanel THEN RETURN NO.

IF icPanel BEGINS "http://" OR icPanel BEGINS "www." OR icPanel BEGINS "ftp://" OR icPanel BEGINS "file://" OR icPanel MATCHES "*.htm" OR icPanel MATCHES ".html" THEN DO:
  IF iMenuStyle = 0 AND NOT bDefStartMax THEN
    ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = 600
           THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = 400
           FRAME {&FRAME-NAME}:WIDTH-PIXELS = 600
           FRAME {&FRAME-NAME}:HEIGHT-PIXELS = 400
           rect-panel:WIDTH-PIXELS = 595
           rect-panel:HEIGHT-PIXELS = 395.

  RUN JBoxJlwSupIe.w PERSIST SET hPanel.
  RUN InitializeObject IN hPanel (rect-panel:HANDLE,"").

  RETURN NO.
END.

ELSE DO:
  IF DYNAMIC-FUNCTION("getCompany") NE "0" AND icPanel = "" THEN DO:
    cCompanyPanel = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany") + " AND cCodeType = 'MenuPanel'","cCodeValue").
    IF cCompanyPanel = ? THEN
      cCompanyPanel = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getParentCompany") + " AND cCodeType = 'MenuPanel'","cCodeValue").
    IF cCompanyPanel = ? THEN
      cCompanyPanel = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCodeMaster") + " AND cCodeType = 'MenuPanel'","cCodeValue").
  END.
  
  ELSE IF (cCompanyPanel = ? OR cCompanyPanel = "") AND cPanelFile NE "" AND cPanelFile NE cCurrPanelFile AND
    (SEARCH(SUBSTR(cPanelFile,1,LENGTH(cPanelFile) - 1) + "w") NE ? OR
     SEARCH(SUBSTR(cPanelFile,1,LENGTH(cPanelFile) - 1) + "r") NE ?)
     THEN DO:
        
    IF VALID-HANDLE(hPanel) THEN DO: 
      RUN disable_UI IN hPanel.
      bRefresh = YES.
    END.
    CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.
    RUN VALUE(cPanelFile) PERSIST SET hPanel.
    IF bRefresh AND CAN-DO(hPanel:INTERNAL-ENTRIES,"setRefresh") THEN
      DYNAMIC-FUNCTION("setRefresh" IN hPanel,YES).
    IF CAN-DO(hPanel:INTERNAL-ENTRIES,"InitializeObject") THEN
      RUN InitializeObject IN hPanel.
    cCurrPanelFile = cCompanyPanel.
  END. 
  ELSE IF icPanel NE "" AND icPanel NE cCurrPanelFile AND 
    (SEARCH(SUBSTR(icPanel,1,LENGTH(icPanel) - 1) + "w") NE ? OR 
     SEARCH(SUBSTR(icPanel,1,LENGTH(icPanel) - 1) + "r") NE ?) 
     THEN DO:
          
    IF VALID-HANDLE(hPanel) THEN DO:
      RUN disable_UI IN hPanel.
      bRefresh = YES.
    END. 
    CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.
    RUN VALUE(icPanel) PERSIST SET hPanel.
    IF bRefresh AND CAN-DO(hPanel:INTERNAL-ENTRIES,"setRefresh") THEN
      DYNAMIC-FUNCTION("setRefresh" IN hPanel,YES).
    IF CAN-DO(hPanel:INTERNAL-ENTRIES,"InitializeObject") THEN
      RUN InitializeObject IN hPanel.
    cCurrPanelFile = icPanel.
  END.
 
  IF cCurrPanelFile NE "" THEN
    RETURN YES.
  ELSE
    RETURN NO.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuStyle C-Win 
FUNCTION setMenuStyle RETURNS LOGICAL
  ( INPUT iiMenuStyle AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iMenuStyle = iiMenuStyle.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPanelFile C-Win 
FUNCTION setPanelFile RETURNS LOGICAL
  ( INPUT icPanelFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Enable a shortcut panel file to be loaded as a suppressed window 
    Notes:  
------------------------------------------------------------------------------*/
cPanelFile = icPanelFile.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTimerProperties C-Win 
FUNCTION setTimerProperties RETURNS LOGICAL
  ( INPUT iiTimerInterval   AS INT,
    INPUT icTimerContext    AS CHAR,
    INPUT icTimerServerProc AS CHAR,
    INPUT icTimerClientProc AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN iTimerInterval    = iiTimerInterval  
       cTimerContext     = icTimerContext   
       cTimerServerProc  = icTimerServerProc
       cTimerClientProc  = icTimerClientProc
       .

IF NOT VALID-HANDLE(hTimer) THEN DO:
  RUN StartTimer.
  IF iTimerInterval > 1000 THEN
    RUN CheckTimerEvent.
END.

DYNAMIC-FUNCTION("setInterval" IN hTimer,iTimerInterval).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setViewMenuPanel C-Win 
FUNCTION setViewMenuPanel RETURNS LOGICAL
  ( INPUT ibViewMenuPanel AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bViewMenuPanel = ibViewMenuPanel.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWinStartXpos C-Win 
FUNCTION setWinStartXpos RETURNS LOGICAL
  ( INPUT iiStartWinXpos AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iStartWinXpos = iiStartWinXpos.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

