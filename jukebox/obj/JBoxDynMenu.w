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

DEF VAR bOk            AS LOG    NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR iDummy         AS INT    NO-UNDO.
DEF VAR cPanelFile     AS CHAR   NO-UNDO.
DEF VAR hPanel         AS HANDLE NO-UNDO.
DEF VAR hMenuCompany   AS HANDLE NO-UNDO.
DEF VAR cCurrPanelFile AS CHAR   NO-UNDO.
DEF VAR hParent        AS HANDLE NO-UNDO.
DEF VAR iMenuId        AS INT    NO-UNDO.

DEF TEMP-TABLE ttWindow 
    FIELD cWinName     AS CHAR 
    FIELD cMenuTitle   AS CHAR 
    FIELD hWindow      AS HANDLE
    FIELD hWinMenuItem AS HANDLE
    FIELD hParent      AS HANDLE
    FIELD bChildWin    AS LOG
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
DEF VAR hParameterField   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dummyButton fiTestIndicator 
&Scoped-Define DISPLAYED-OBJECTS fiTestIndicator 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentMenuItem C-Win 
FUNCTION getCurrentMenuItem RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuItem C-Win 
FUNCTION getMenuItem RETURNS HANDLE
  ( INPUT icLaunch   AS CHAR,
    INPUT icLabel    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParameter C-Win 
FUNCTION getParameter RETURNS CHARACTER
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
  ( /* parameter-definitions */ )  FORWARD.

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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON dummyButton 
     LABEL "Just for focus" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiTestIndicator AS CHARACTER FORMAT "X(256)":U INITIAL "TEST" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     dummyButton AT ROW 1.48 COL 5
     fiTestIndicator AT ROW 2.67 COL 1 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.4 BY 2.52.


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
         HEIGHT             = 2.52
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
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
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
/* SETTINGS FOR FILL-IN fiTestIndicator IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  FOR EACH ttWindow:
    IF VALID-HANDLE(ttWindow.hWindow) THEN
      APPLY "close" TO ttWindow.hWindow.
  END.
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize", THIS-PROCEDURE:CURRENT-WINDOW).
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    setPanelFile("JBoxDynMenuPanel.w").
    RUN InitializeObject(0).
  &ENDIF
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}

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
    hMenuItem = httMenuBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE.
    APPLY "choose" TO hMenuItem.
  END.
END.
ELSE DO:
  bOK = httMenuBuffer:FIND-FIRST("WHERE cMenuLabel = '" + icLabel + "'") NO-ERROR.
  IF bOK THEN DO:
    hMenuItem = httMenuBuffer:BUFFER-FIELD("hMenuItem"):BUFFER-VALUE.
    APPLY "choose" TO hMenuItem.
  END.
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
DEF INPUT PARAM ihWindow    AS HANDLE NO-UNDO.
DEF INPUT PARAM ihWinMenu   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihMenuItem  AS HANDLE NO-UNDO.

DEF VAR hMenuItm AS HANDLE NO-UNDO.

FIND FIRST ttWindow 
     WHERE ttWindow.hWindow = ihWindow NO-ERROR.
IF AVAIL ttWindow THEN DO:
  ttWindow.hWindow:CURRENT-WINDOW:MOVE-TO-TOP().
  ttWindow.hWindow:CURRENT-WINDOW:WINDOW-STATE = 3.
  hMenuItm = ihWinMenu:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hMenuItm):
    hMenuItm:CHECKED = IF hMenuItm = ttWindow.hWinMenuItem THEN TRUE ELSE FALSE.
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
DEF INPUT PARAM iiMenuId AS INT NO-UNDO.
DEF INPUT PARAM ihParent AS HANDLE NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.
DEF VAR hMenuObject AS HANDLE NO-UNDO.
DEF VAR hMultiple   AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE(httMenu).
hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH JBoxMenu WHERE JBoxMenu.iParentMenuId = " + STRING(iiMenuId) + " BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

hMultiple = hBuffer:BUFFER-FIELD("bMultiple") NO-ERROR.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "MENU-ITEM" 
     AND hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE = "THIS-PROCEDURE" 
     AND NOT CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) 
     THEN DO:
    hQuery:GET-NEXT().
    NEXT.      
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
      CREATE SUB-MENU hMenuObject
        ASSIGN PARENT = ihParent
               LABEL  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
               NAME   = "sm_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
               .
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,hMenuObject).
    END.
    WHEN "PLACEHOLDER" THEN 
      IF hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE NE "NavBarOptions" THEN
        CREATE SUB-MENU hMenuObject
          ASSIGN PARENT = ihParent
                 LABEL  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 NAME   = "ph_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                 .
    WHEN "MENU-ITEM" THEN DO:
      CASE STRING(hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE):
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
              ON CHOOSE PERSISTENT RUN StartDataBrw  IN THIS-PROCEDURE (hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,hMenuObject).
            END TRIGGERS.
        OTHERWISE
          CREATE MENU-ITEM hMenuObject
            ASSIGN PARENT      = ihParent
                   LABEL       = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   NAME        = "mi_" + hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ACCELERATOR = hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE
            TRIGGERS:
              ON CHOOSE PERSISTENT RUN VALUE(hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
            END TRIGGERS.
      END CASE.
    END.
    WHEN "RULE" THEN 
      CREATE MENU-ITEM hMenuObject
        ASSIGN PARENT      = ihParent
               SUBTYPE     = "rule"
               .
    WHEN "MENU" THEN 
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,ihParent).
    WHEN "MENUBAR" THEN 
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,ihParent).
    WHEN "TV-NAV-BAR" THEN 
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,ihParent).
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
DEF VAR iCurrCompany AS INT NO-UNDO.
DEF VAR iMenuChoice      AS INT   NO-UNDO.

FIND FIRST ttWindow WHERE ttWindow.hWinMenuItem = SELF NO-ERROR.

iCurrCompany = DYNAMIC-FUNCTION("getCompanyId").

DYNAMIC-FUNCTION("setMenuRestart",YES).
RUN JBoxDSelectCompany.w (TRUE, INPUT-OUTPUT iDummy).
DYNAMIC-FUNCTION("setMenuRestart",NO).

IF DYNAMIC-FUNCTION("getCompanyId") NE iCurrCompany AND
   CAN-DO(hParent:INTERNAL-ENTRIES,"RestartMenu")
   THEN DO:
  RUN SelectRestartMenu (OUTPUT iMenuChoice).
  RUN RestartMenu IN hParent (iMenuChoice).
  APPLY "close" TO THIS-PROCEDURE.
END.

ELSE IF DYNAMIC-FUNCTION("getCompanyId") NE iCurrCompany THEN DO:
  setDisabledMenus(THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR:FIRST-CHILD,"").
  setMenuPanel().
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

IF cTimerServerProc NE "" THEN DO:
  DYNAMIC-FUNCTION("setShowHourGlass",FALSE).
  IF DYNAMIC-FUNCTION("runproc",cTimerServerProc,cTimerContext,?) THEN DO:
    cCurrTimerEvent = DYNAMIC-FUNCTION("getTransactionMessage").
    IF cCurrTimerEvent NE cLastTimerEvent THEN DO:
      IF cTimerClientProc NE "" THEN
        RUN VALUE(cTimerClientProc) (cCurrTimerEvent).
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
  DISPLAY fiTestIndicator 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE dummyButton fiTestIndicator 
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
DEF INPUT PARAM iiMenuId    AS INT NO-UNDO.

DEF VAR hMenu            AS HANDLE NO-UNDO.
DEF VAR iCompanyId       AS INT    NO-UNDO.
DEF VAR cReadOnlyUsers   AS CHAR   NO-UNDO.
DEF VAR cReadOnlyActions AS CHAR   NO-UNDO.

IF iiMenuId = 0 THEN
  iiMenuId = INT(DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE cMenuType = 'menubar'","iJBoxMenuId")).

IF iiMenuId = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"No menu definitions found in database" + CHR(10)
                                 + DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  QUIT.
END.

iMenuId = iiMenuId.

httMenu = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                            ,"WHERE iJBoxMenuId = " + STRING(iiMenuId) 
                            ,?).

httMenuBuffer = httMenu:DEFAULT-BUFFER-HANDLE.

hParameterField = httMenuBuffer:BUFFER-FIELD("cParameter") NO-ERROR.

CREATE MENU hMenu.
THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR = hMenu.

RUN BuildMenu (iiMenuId,hMenu).

{&WINDOW-NAME}:TITLE = DYNAMIC-FUNCTION("getAppTitle").

iCompanyId = DYNAMIC-FUNCTION("getCompanyId").
IF iCompanyId = 0 THEN DO:
  IF VALID-HANDLE(hMenuCompany) THEN APPLY "choose" TO hMenuCompany.
  ELSE RUN JBoxDSelectCompany.w (TRUE,INPUT-OUTPUT iDummy).
END.
ELSE DO:
  DYNAMIC-FUNCTION("getFunctionRestrictions").    
  DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW).
END.

SUBSCRIBE TO "InvalidateHandle"     ANYWHERE.
SUBSCRIBE TO "ApplyMenu"            ANYWHERE.
SUBSCRIBE TO "UserNotification" ANYWHERE.

hMainMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle")) NO-ERROR.
/* If there is only one menu instance there is no need to set the mainmenuhandle attribute.
   Otherwise only the main menu will be the only place to start child windows: */
IF NOT VALID-HANDLE(hMainMenu) OR hMainMenu = THIS-PROCEDURE THEN 
  SUBSCRIBE TO "StartChildWindow" ANYWHERE.
  
/* Normally the session object is already created here.. */
DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").

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
/*
ELSE IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxCompanyUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'" +
                                                                    "  AND iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")),"bSuperUserCompany")) THEN DO:
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
*/

IF iCompanyId NE 0 AND NOT setMenuPanel() THEN DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

/* If the menu contains an entry for message publication (requires JukeBox messaging installed) 
   start the timer here: */
IF iTimerInterval > 0 THEN DO:
  IF SEARCH("controls.dll") NE ? AND 
    (SEARCH("JBoxJLWTimer.r") NE ? OR SEARCH("JBoxJLWTimer.r") NE ?) THEN
    RUN JBoxJLWtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  ELSE IF SEARCH("JBoxABLtimer.r") NE ? AND PROPATH MATCHES "*webclient*" THEN
    RUN JBoxABLtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  ELSE IF SEARCH("rstimer.ocx") NE ? AND PROPATH MATCHES "*webclient*" THEN
    RUN JBoxRsTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  ELSE
    RUN JBoxTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  IF iTimerInterval > 1000 THEN
    RUN CheckTimerEvent.
END.

DO WITH FRAME {&FRAME-NAME}:
  IF NOT SESSION:PARAM MATCHES "*test_database*" THEN
    fiTestIndicator:HIDDEN = YES.
  APPLY "entry" TO dummyButton.
  dummyButton:VISIBLE = NO.
END.
IF VALID-HANDLE(hPanel) AND CAN-DO(hPanel:INTERNAL-ENTRIES,"setFocus") THEN
  DYNAMIC-FUNCTION("setFocus" IN hPanel).

/*
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
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
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.

FOR EACH ttWindow
    WHERE ttWindow.hParent = ihWindow:
  IF VALID-HANDLE(ttWindow.hWindow) THEN 
    APPLY "close" TO ttWindow.hWindow.
  IF AVAIL ttWindow THEN DO:
    DELETE OBJECT ttWindow.hWinMenuItem NO-ERROR.
    DELETE ttWindow.
  END.
END.

FIND FIRST ttWindow 
     WHERE ttWindow.hWindow = ihWindow NO-ERROR.
IF AVAIL ttWindow THEN DO:
  DELETE OBJECT ttWindow.hWinMenuItem.
  DELETE ttWindow.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartChildWindow C-Win 
PROCEDURE StartChildWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  icWinName:   File name 
               icWinTitle:  If blank the window will inherit the child win title
               ihParent:    Calling procedure
               ibNew:       TRUE if multiple instances allowed
               ohWinHandle: Procedure handle to child
                
  Notes:       Invoke procedure "InitializeChild" in the calling procedure
               to set attributes in the child prior to InitializeObject
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icWinName      AS CHAR   NO-UNDO.
DEF INPUT  PARAM icWinTitle     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.

DEF VAR hMenuItm AS HANDLE NO-UNDO.

FIND FIRST ttWindow 
     WHERE ttWindow.cWinName = icWinName NO-ERROR.
IF NOT AVAIL ttWindow OR ibNew OR (AVAIL ttWindow AND NOT ttWindow.bChildWin) THEN DO:
  CREATE ttWindow.
  ASSIGN ttWindow.cWinName     = icWinName
         ttWindow.cMenuTitle   = icWinTitle
         ttWindow.hParent      = ihParent
         ttWindow.bChildWin    = TRUE
         .
  
  RUN VALUE(icWinName) PERSIST SET ttWindow.hWindow.

  IF NOT VALID-HANDLE(hWinMenu) THEN DO:
    CREATE SUB-MENU hWinMenu
           ASSIGN PARENT = THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR
                  LABEL = "Vindu"
                  NAME  = "m_Vindu"
                  .
    DYNAMIC-FUNCTION("InitTranslation",hWinMenu).
  END.
  CREATE MENU-ITEM ttWindow.hWinMenuItem
         ASSIGN TOGGLE-BOX = TRUE
                PARENT     = hWinMenu
                LABEL      = icWinTitle
                NAME       = "m_" + icWinTitle
                TRIGGERS:
                  ON VALUE-CHANGED PERSISTENT RUN BringToTop IN THIS-PROCEDURE (ttWindow.hWindow,hWinMenu,ttWindow.hWinMenuItem).
                END TRIGGERS
                .

  IF icWinTitle NE "" THEN
    ttWindow.hWindow:CURRENT-WINDOW:TITLE = ttWindow.cMenuTitle.
  
  IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"setParent") THEN 
    DYNAMIC-FUNCTION("setParent" IN ttWindow.hWindow,ihParent).
  
  IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
    IF CAN-DO(ihParent:INTERNAL-ENTRIES,"InitializeChild") THEN
      RUN InitializeChild IN ihParent (ttWindow.hWindow).
    RUN InitializeObject IN ttWindow.hWindow.
  END.

  IF ttWindow.hWinMenuItem:LABEL = "" THEN
    ASSIGN ttWindow.cMenuTitle         = ttWindow.hWindow:CURRENT-WINDOW:TITLE
           ttWindow.hWinMenuItem:LABEL = ttWindow.hWindow:CURRENT-WINDOW:TITLE
           .

  DYNAMIC-FUNCTION("setCompanyHeader",ttWindow.hWindow:CURRENT-WINDOW).
END.
ELSE IF AVAIL ttWindow THEN
  ASSIGN ttWindow.cWinName           = icWinName
         ttWindow.cMenuTitle         = icWinTitle
         ttWindow.hParent            = ihParent
         ttWindow.hWinMenuItem:LABEL = IF icWinTitle NE "" THEN icWinTitle ELSE ttWindow.hWinMenuItem:LABEL
         ttWindow.hWindow:CURRENT-WINDOW:TITLE = IF icWinTitle NE "" THEN icWinTitle ELSE ttWindow.hWindow:CURRENT-WINDOW:TITLE
         .

IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"MoveToTop") THEN 
  RUN MoveToTop IN ttWindow.hWindow.
ELSE DO:
  ttWindow.hWindow:CURRENT-WINDOW:WINDOW-STATE = 3.
  ttWindow.hWindow:CURRENT-WINDOW:MOVE-TO-TOP().
END.
hMenuItm = hWinMenu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMenuItm):
  hMenuItm:CHECKED = IF hMenuItm = ttWindow.hWinMenuItem THEN TRUE ELSE FALSE.
  hMenuItm = hMenuItm:NEXT-SIBLING.
END.

ohWinHandle = ttWindow.hWindow.

SESSION:SET-WAIT-STATE("").

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

IF SEARCH(icInitProgram) NE ? OR SEARCH(SUBSTR(icInitProgram,1,LENGTH(icInitProgram) - 1) + "r") NE ? THEN DO:
  RUN StartWindow ("JBoxDataBrw.w",YES,ihMenuObject).
  RUN VALUE(icInitProgram) (ttWindow.hWindow).
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Cannot run program: " + icInitProgram,"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartWindow C-Win 
PROCEDURE StartWindow :
/*------------------------------------------------------------------------------
  Purpose:    Excecute a menu command 
  Parameters: icWinName:      File name for selected menu item
              ibNew:          Set to enable more than one instance of program
              ihSelectedMenu: Handle to menu item (to get the menu label for window title)
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icWinName      AS CHAR   NO-UNDO.
DEF INPUT PARAM ibNew          AS LOG    NO-UNDO.
DEF INPUT PARAM ihSelectedMenu AS HANDLE NO-UNDO.

DEF VAR hMenuItm AS HANDLE NO-UNDO.

IF NOT ibNew THEN
  FIND FIRST ttWindow 
       WHERE ttWindow.cWinName = icWinName NO-ERROR.
IF NOT AVAIL ttWindow OR ibNew OR (AVAIL ttWindow AND ttWindow.bChildWin) THEN DO:
  CREATE ttWindow.
  ASSIGN ttWindow.cWinName     = icWinName
         ttWindow.cMenuTitle   = ihSelectedMenu:LABEL
         .

  bOK = httMenuBuffer:FIND-FIRST("WHERE hMenuItem = WIDGET-HANDLE('" + STRING(ihSelectedMenu) + "')") NO-ERROR.
  IF NOT bOk THEN
    httMenuBuffer:FIND-FIRST("WHERE cLaunch = '" + icWinName + "'") NO-ERROR.
  
  RUN VALUE(icWinName) PERSIST SET ttWindow.hWindow.

  IF NOT AVAIL ttWindow THEN RETURN.
  
  IF NOT VALID-HANDLE(ttWindow.hWindow) THEN DO:
    IF AVAIL ttWindow THEN
      DELETE ttWindow.
    RETURN.
  END.

  IF NOT VALID-HANDLE(hWinMenu) THEN DO:
    CREATE SUB-MENU hWinMenu
           ASSIGN PARENT = THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR
                  LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vindu" ELSE "Window"
                  NAME  = "m_Vindu"
                  .
    DYNAMIC-FUNCTION("InitTranslation",hWinMenu).
  END.
  CREATE MENU-ITEM ttWindow.hWinMenuItem
         ASSIGN TOGGLE-BOX = TRUE
                PARENT     = hWinMenu
                LABEL      = ihSelectedMenu:LABEL
                NAME       = "m_" + ihSelectedMenu:LABEL
                TRIGGERS:
                  ON VALUE-CHANGED PERSISTENT RUN BringToTop IN THIS-PROCEDURE (ttWindow.hWindow,hWinMenu,ttWindow.hWinMenuItem).
                END TRIGGERS
                .

  ttWindow.hWindow:CURRENT-WINDOW:TITLE = ttWindow.cMenuTitle.
  
  IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"InitializeObject") THEN 
    RUN InitializeObject IN ttWindow.hWindow NO-ERROR.
  
  IF NOT AVAIL ttWindow THEN RETURN.

  IF NOT VALID-HANDLE(ttWindow.hWindow) THEN DO:
    IF AVAIL ttWindow THEN DO:
      DELETE OBJECT ttWindow.hWinMenuItem NO-ERROR.
      DELETE ttWindow.
    END.
    RETURN.
  END.

  DYNAMIC-FUNCTION("setCompanyHeader",ttWindow.hWindow:CURRENT-WINDOW).
END.

IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"MoveToTop") THEN 
  RUN MoveToTop IN ttWindow.hWindow.
ELSE DO:
  ttWindow.hWindow:CURRENT-WINDOW:WINDOW-STATE = 3.
  ttWindow.hWindow:CURRENT-WINDOW:MOVE-TO-TOP().
END.
hMenuItm = hWinMenu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMenuItm):
  hMenuItm:CHECKED = IF hMenuItm = ttWindow.hWinMenuItem THEN TRUE ELSE FALSE.
  hMenuItm = hMenuItm:NEXT-SIBLING.
END.

SESSION:SET-WAIT-STATE("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentMenuItem C-Win 
FUNCTION getCurrentMenuItem RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Get the handle to the last executed menu item 
    Notes:  
------------------------------------------------------------------------------*/
IF AVAIL ttWindow THEN
  RETURN ttWindow.hWinMenuItem.
ELSE
  RETURN SELF.   

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
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Nice buttons for most frequently used menu choices, logo, etc
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cCompanyPanel AS CHAR   NO-UNDO.
DEF VAR bRefresh      AS LOG    NO-UNDO.

cCompanyPanel = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany") + " AND cCodeType = 'MenuPanel'","cCodeValue").
IF cCompanyPanel = ? THEN
  cCompanyPanel = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getParentCompany") + " AND cCodeType = 'MenuPanel'","cCodeValue").
IF cCompanyPanel = ? THEN
  cCompanyPanel = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCodeMaster") + " AND cCodeType = 'MenuPanel'","cCodeValue").

IF cCompanyPanel NE ? AND cCompanyPanel NE cCurrPanelFile AND
  (SEARCH(SUBSTR(cCompanyPanel,1,LENGTH(cCompanyPanel) - 1) + "w") NE ? OR 
   SEARCH(SUBSTR(cCompanyPanel,1,LENGTH(cCompanyPanel) - 1) + "r") NE ?) 
   THEN DO:
      
  IF VALID-HANDLE(hPanel) THEN DO: 
    RUN disable_UI IN hPanel.
    bRefresh = YES.
  END.
  CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.
  RUN VALUE(cCompanyPanel) PERSIST SET hPanel.
  IF bRefresh AND CAN-DO(hPanel:INTERNAL-ENTRIES,"setRefresh") THEN
    DYNAMIC-FUNCTION("setRefresh" IN hPanel,YES).
  IF CAN-DO(hPanel:INTERNAL-ENTRIES,"InitializeObject") THEN
    RUN InitializeObject IN hPanel.
  cCurrPanelFile = cCompanyPanel.
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
  cCurrPanelFile = cPanelFile.
END.
 
IF cCurrPanelFile NE "" THEN
  RETURN YES.
ELSE
  RETURN NO.

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

DYNAMIC-FUNCTION("setInterval" IN hTimer,iTimerInterval).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

