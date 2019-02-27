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
DEF VAR bOK              AS LOG    NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR hParent          AS HANDLE NO-UNDO.
DEF VAR hRectangle       AS HANDLE NO-UNDO.
DEF VAR hParentQuery     AS HANDLE NO-UNDO.

DEF VAR iCurrTab         AS INT    NO-UNDO INIT 1.
DEF VAR bStopAtMinMaxTab AS LOG    NO-UNDO.

DEF TEMP-TABLE ttProgram NO-UNDO 
    FIELD hProgram     AS HANDLE  
    FIELD iTab#        AS INT 
    FIELD cProgramName AS CHAR 
    FIELD cLabel       AS CHAR 
    FIELD cImage       AS CHAR 
    FIELD hFrame       AS HANDLE
    INDEX idxTab# IS PRIMARY UNIQUE iTab#
    .

DEF BUFFER bttProgram FOR ttProgram.

DEF TEMP-TABLE ttMoveToTop
    FIELD hMoveToTop AS HANDLE
    FIELD iAreal     AS INT
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmTabFrame

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addFolder C-Win 
FUNCTION addFolder RETURNS HANDLE
  (INPUT iiTab#        AS INT,
   INPUT icLabel       AS CHAR,
   INPUT icProgramName AS CHAR,
   INPUT icImage       AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildFolder C-Win 
FUNCTION buildFolder RETURNS HANDLE () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD closeTabWin C-Win 
FUNCTION closeTabWin RETURNS LOGICAL
  ( INPUT ipiTab# AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentTab C-Win 
FUNCTION getCurrentTab RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFolderLabel C-Win 
FUNCTION getFolderLabel RETURNS CHARACTER
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPageFieldMap C-Win 
FUNCTION getPageFieldMap RETURNS HANDLE
  ( INPUT iiTab# AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPageFrame C-Win 
FUNCTION getPageFrame RETURNS HANDLE
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPageHandle C-Win 
FUNCTION getPageHandle RETURNS HANDLE
  ( INPUT iiTab# AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPageQuery C-Win 
FUNCTION getPageQuery RETURNS HANDLE
  ( INPUT iiTab# AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTabFrame C-Win 
FUNCTION getTabFrame RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTabFrames C-Win 
FUNCTION getTabFrames RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitPages C-Win 
FUNCTION InitPages RETURNS LOGICAL
  ( INPUT icPageList    AS CHAR,
    INPUT ihParentQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MoveTabToTop C-Win 
FUNCTION MoveTabToTop RETURNS LOGICAL
  ( INPUT ihBorderFrame AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoResizeTabY C-Win 
FUNCTION setNoResizeTabY RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setStopAtMinMaxTab C-Win 
FUNCTION setStopAtMinMaxTab RETURNS LOGICAL
  ( INPUT ibStopAtMinMaxTab AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTabFolderPosition C-Win 
FUNCTION setTabFolderPosition RETURNS LOGICAL
  (INPUT ihRectangle AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTabMoveY C-Win 
FUNCTION setTabMoveY RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabFolderChanged C-Win 
FUNCTION TabFolderChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmTabFrame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 36.2 BY 3.52.

DEFINE FRAME frmProgram
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.19
         SCROLLABLE SIZE 34 BY 1.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Folder"
         HEIGHT             = 3.38
         WIDTH              = 36.4
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


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
/* REPARENT FRAME */
ASSIGN FRAME frmProgram:FRAME = FRAME frmTabFrame:HANDLE.

/* SETTINGS FOR FRAME frmProgram
                                                                        */
ASSIGN 
       FRAME frmProgram:HEIGHT           = 1.9
       FRAME frmProgram:WIDTH            = 34
       FRAME frmProgram:RESIZABLE        = TRUE.

/* SETTINGS FOR FRAME frmTabFrame
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmProgram
/* Query rebuild information for FRAME frmProgram
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmProgram */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmTabFrame
/* Query rebuild information for FRAME frmTabFrame
     _Query            is NOT OPENED
*/  /* FRAME frmTabFrame */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME frmTabFrame:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 3.33
       WIDTH           = 36
       HIDDEN          = no
       SENSITIVE       = yes.
      TabStrip:NAME = "TabStrip":U .
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-BEFORE(FRAME frmProgram:HANDLE).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Folder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Folder */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-Win OCX.MouseUp
PROCEDURE TabStrip.TabStrip.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.

IF iCurrTab = INT(COM-SELF:SelectedItem:Index) THEN DO:
  IF AVAIL ttProgram THEN RUN MoveToTop IN ttProgram.hProgram NO-ERROR.
  RETURN NO-APPLY.
END.

TabFolderChanged(INT(COM-SELF:SelectedItem:Index)).

RETURN NO-APPLY.
END PROCEDURE.

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
DO:
  DYNAMIC-FUNCTION("closeTabWin",0). /* 0: all */

/*   if valid-handle(hCurrTabWin) then                                   */
/*     RUN disable_UI IN hCurrTabWin NO-ERROR.                           */
/*   IF VALID-HANDLE(hCurrTabWin) AND hCurrTabWin NE THIS-PROCEDURE THEN */
/*     APPLY "close" TO hCurrTabWin.                                     */
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

  RUN enable_UI.

  SUBSCRIBE TO "ResizeTabFrames" ANYWHERE.
  SUBSCRIBE TO "InitStringTranslation" ANYWHERE.
  SUBSCRIBE TO "NextTab" ANYWHERE.
  SUBSCRIBE TO "PrevTab" ANYWHERE.

  FRAME frmProgram:TAB-STOP = NO.
  FRAME frmTabFrame:TAB-STOP = NO.
  TabStrip:TAB-STOP = NO.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "jboxtabfolder.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "jboxtabfolder.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  HIDE FRAME frmProgram.
  HIDE FRAME frmTabFrame.
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
  RUN control_load.
  VIEW FRAME frmTabFrame.
  {&OPEN-BROWSERS-IN-QUERY-frmTabFrame}
  VIEW FRAME frmProgram.
  {&OPEN-BROWSERS-IN-QUERY-frmProgram}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow            AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList  AS CHAR   NO-UNDO.

DEF VAR cTranslation AS CHAR NO-UNDO.

IF hParent:CURRENT-WINDOW NE ihWindow THEN RETURN.

IF NOT CAN-DO(iocTypeList,"foldertab") THEN
  iocTypeList = iocTypeList + ",FOLDERTAB".

FOR EACH ttProgram:
  cTranslation = DYNAMIC-FUNCTION("getStringTranslation",hParent:FILE-NAME,"foldertab",ttProgram.cLabel).
  IF cTranslation NE "" THEN
    chTabStrip:TabStrip:Tabs:Item(ttProgram.iTab#):Caption = cTranslation.
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
DEF VAR hContainer AS HANDLE NO-UNDO.

IF SOURCE-PROCEDURE:CURRENT-WINDOW NE ? AND SOURCE-PROCEDURE:CURRENT-WINDOW NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN ERROR.

IF VALID-HANDLE(hParent) THEN APPLY "close" TO hParent.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NextTab C-Win 
PROCEDURE NextTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceProc AS HANDLE NO-UNDO.

DEF VAR hChildTabProc      AS HANDLE NO-UNDO.
DEF VAR hChildTabFolder AS HANDLE NO-UNDO.

IF ihSourceProc NE hParent THEN RETURN.

hChildTabProc   = DYNAMIC-FUNCTION("getLinkedObjectByInfo",hParent,"parent","page" + STRING(iCurrTab) + "|to").
IF VALID-HANDLE(hChildTabProc) AND hChildTabProc NE hParent THEN DO:
  hChildTabFolder = DYNAMIC-FUNCTION("GetLinkedObject",hChildTabProc,"TabFolder","from").
            
  IF VALID-HANDLE(hChildTabFolder) THEN DO:
    DYNAMIC-FUNCTION("setStopAtMinMaxTab" IN hChildTabFolder,YES).
    RUN NextTab IN hChildTabFolder (hChildTabProc).
    IF RETURN-VALUE NE "max" THEN
      RETURN.
  END.
END.

FIND FIRST ttProgram
     WHERE ttProgram.iTab# = iCurrTab + 1
     NO-ERROR.
IF NOT AVAIL ttProgram AND NOT bStopAtMinMaxTab THEN
  FIND FIRST ttProgram
       WHERE ttProgram.iTab# = 1
       NO-ERROR.
  
IF AVAIL ttProgram THEN
  TabFolderChanged(ttProgram.iTab#).
ELSE IF bStopAtMinMaxTab THEN DO:
  bStopAtMinMaxTab = NO.
  RETURN "max".  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevTab C-Win 
PROCEDURE PrevTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceProc AS HANDLE NO-UNDO.

DEF VAR hChildTabProc      AS HANDLE NO-UNDO.
DEF VAR hChildTabFolder AS HANDLE NO-UNDO.

IF ihSourceProc NE hParent THEN RETURN.

hChildTabProc   = DYNAMIC-FUNCTION("getLinkedObjectByInfo",hParent,"parent","page" + STRING(iCurrTab) + "|to").
IF VALID-HANDLE(hChildTabProc) AND hChildTabProc NE hParent THEN DO:
  hChildTabFolder = DYNAMIC-FUNCTION("GetLinkedObject",hChildTabProc,"TabFolder","from").
            
  IF VALID-HANDLE(hChildTabFolder) THEN DO:
    DYNAMIC-FUNCTION("setStopAtMinMaxTab" IN hChildTabFolder,YES).
    RUN PrevTab IN hChildTabFolder (hChildTabProc).
    IF RETURN-VALUE NE "min" THEN
      RETURN.
  END.
END.

FIND FIRST ttProgram
     WHERE ttProgram.iTab# = iCurrTab - 1
     NO-ERROR.
IF NOT AVAIL ttProgram AND NOT bStopAtMinMaxTab THEN
  FOR EACH ttProgram
      BY ttProgram.iTab# DESC
      :
    LEAVE.
  END.
  
IF AVAIL ttProgram THEN
  TabFolderChanged(ttProgram.iTab#).
ELSE IF bStopAtMinMaxTab THEN DO:
  bStopAtMinMaxTab = NO.
  RETURN "min".  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResizeTabFrames C-Win 
PROCEDURE ResizeTabFrames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.

IF ihWindow = THIS-PROCEDURE:CURRENT-WINDOW THEN DO:
  ASSIGN 
    FRAME frmTabFrame:X             = DYNAMIC-FUNCTION("getAbsPosition",hRectangle,"X")
    FRAME frmTabFrame:Y             = DYNAMIC-FUNCTION("getAbsPosition",hRectangle,"Y")
    FRAME frmTabFrame:WIDTH-PIXELS  = hRectangle:WIDTH-PIXELS
    FRAME frmTabFrame:HEIGHT-PIXELS = hRectangle:HEIGHT-PIXELS
    FRAME frmTabFrame:VIRTUAL-WIDTH-PIXELS  = hRectangle:WIDTH-PIXELS
    FRAME frmTabFrame:VIRTUAL-HEIGHT-PIXELS = hRectangle:HEIGHT-PIXELS
    TabStrip:WIDTH-PIXELS           = hRectangle:WIDTH-PIXELS
    TabStrip:HEIGHT-PIXELS          = hRectangle:HEIGHT-PIXELS
    TabStrip:VIRTUAL-WIDTH-PIXELS           = hRectangle:WIDTH-PIXELS
    TabStrip:VIRTUAL-HEIGHT-PIXELS          = hRectangle:HEIGHT-PIXELS
    FRAME frmProgram:X              = 5
    FRAME frmProgram:Y              = 25
    FRAME frmProgram:WIDTH-PIXELS   = TabStrip:WIDTH-PIXELS - 10
    FRAME frmProgram:HEIGHT-PIXELS  = TabStrip:HEIGHT-PIXELS - 30
    FRAME frmProgram:VIRTUAL-WIDTH-PIXELS   = TabStrip:WIDTH-PIXELS - 10
    FRAME frmProgram:VIRTUAL-HEIGHT-PIXELS  = TabStrip:HEIGHT-PIXELS - 30
    NO-ERROR.
  FOR EACH bttProgram:
    ASSIGN bttProgram.hFrame:WIDTH-PIXELS  = FRAME frmProgram:WIDTH-PIXELS
           bttProgram.hFrame:HEIGHT-PIXELS = FRAME frmProgram:HEIGHT-PIXELS.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addFolder C-Win 
FUNCTION addFolder RETURNS HANDLE
  (INPUT iiTab#        AS INT,
   INPUT icLabel       AS CHAR,
   INPUT icProgramName AS CHAR,
   INPUT icImage       AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hProg AS HANDLE NO-UNDO.

FIND FIRST ttProgram 
     WHERE ttProgram.iTab# = iiTab#
     NO-ERROR.
IF NOT AVAIL ttProgram THEN DO:
  CREATE ttProgram.
  ttProgram.iTab# = iiTab#.
END.
ELSE DO:
  IF VALID-HANDLE(ttProgram.hProgram) THEN APPLY "close" TO ttProgram.hProgram.
  RETURN ?.
END.


ASSIGN ttProgram.cLabel       = icLabel
       ttProgram.cProgramName = icProgramName
       ttProgram.cImage       = icImage
       .

IF SEARCH(ENTRY(1,ttProgram.cProgramName,".") + ".r") NE ? OR SEARCH(ENTRY(1,ttProgram.cProgramName,".") + ".w") NE ? THEN DO:
  RUN VALUE(ttProgram.cProgramName) PERSIST SET ttProgram.hProgram NO-ERROR.
  SUBSCRIBE TO "InvalidateHandle" IN ttProgram.hProgram.

  hProg = ttProgram.hProgram.

  DYNAMIC-FUNCTION("CreateParentLink",ttProgram.hProgram,hParent,"page" + STRING(iiTab#)).

  IF CAN-DO(ttProgram.hProgram:INTERNAL-ENTRIES,"setParent") THEN
    DYNAMIC-FUNCTION("setParent" IN ttProgram.hProgram,hParent) NO-ERROR.
  IF CAN-DO(ttProgram.hProgram:INTERNAL-ENTRIES,"getFrameHandle") THEN DO:
    ttProgram.hFrame = DYNAMIC-FUNCTION("getFrameHandle" IN ttProgram.hProgram) NO-ERROR.
    DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"pageframe" + STRING(iiTab#),STRING(ttProgram.hFrame)).
  END.

  IF CAN-DO(ttProgram.hProgram:INTERNAL-ENTRIES,"InitializeResize") THEN
    DYNAMIC-FUNCTION("InitializeResize" IN ttProgram.hProgram) NO-ERROR.

  DYNAMIC-FUNCTION("setDeltaX",FRAME frmProgram:WIDTH-PIXELS - ttProgram.hFrame:WIDTH-PIXELS).
  DYNAMIC-FUNCTION("setDeltaY",FRAME frmProgram:HEIGHT-PIXELS - ttProgram.hFrame:HEIGHT-PIXELS).
  ASSIGN 
    ttProgram.hFrame:X = FRAME {&FRAME-NAME}:X + FRAME frmProgram:X
    ttProgram.hFrame:Y = FRAME {&FRAME-NAME}:Y + FRAME frmProgram:Y
    ttProgram.hFrame:WIDTH-PIXELS = FRAME frmProgram:WIDTH-PIXELS
    ttProgram.hFrame:HEIGHT-PIXELS = FRAME frmProgram:HEIGHT-PIXELS
    .

  DYNAMIC-FUNCTION("DoWidgetResize",ttProgram.hFrame:FIRST-CHILD).

  IF VALID-HANDLE(hParentQuery) THEN DO:
    DYNAMIC-FUNCTION("setQuery" IN ttProgram.hProgram,hParentQuery) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      MESSAGE "Function 'setQuery' missing in tab-folder procedure for tab # " + STRING(iiTab#)
              VIEW-AS ALERT-BOX ERROR.
  END.

  RUN InitializeObject IN ttProgram.hProgram NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
    MESSAGE "Procedure 'InitializeObject' missing in tab-folder procedure for tab # " + STRING(iiTab#)
            VIEW-AS ALERT-BOX ERROR.

END.
ELSE 
  MESSAGE PROGRAM-NAME(1) SKIP
          "Couldn't find tab program: " icProgramName 
          VIEW-AS ALERT-BOX.

RETURN hProg.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildFolder C-Win 
FUNCTION buildFolder RETURNS HANDLE ():
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setLockWindowUpdate",TRUE).

FOR EACH ttProgram:
  chTabStrip:TabStrip:Tabs:Remove(ttProgram.iTab#) NO-ERROR.
  chTabStrip:TabStrip:Tabs:add(ttProgram.iTab#).
  chTabStrip:TabStrip:Tabs:Item(ttProgram.iTab#):Caption = ttProgram.cLabel.
END.
DYNAMIC-FUNCTION("setLockWindowUpdate",FALSE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION closeTabWin C-Win 
FUNCTION closeTabWin RETURNS LOGICAL
  ( INPUT ipiTab# AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH ttProgram 
    WHERE (IF ipiTab# NE 0 THEN ttProgram.iTab# = ipiTab# ELSE TRUE):
  IF VALID-HANDLE(ttProgram.hProgram) THEN 
    RUN DISABLE_ui IN ttProgram.hProgram.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentTab C-Win 
FUNCTION getCurrentTab RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCurrTab.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFolderLabel C-Win 
FUNCTION getFolderLabel RETURNS CHARACTER
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTabLabels AS CHAR NO-UNDO.
FOR EACH ttProgram
    WHERE (IF iiTab NE 0 THEN ttProgram.iTab# = iiTab ELSE TRUE)
    :
  cTabLabels = cTabLabels + (IF cTabLabels NE "" THEN "|" ELSE "") + ttProgram.cLabel.
END.

RETURN cTabLabels.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPageFieldMap C-Win 
FUNCTION getPageFieldMap RETURNS HANDLE
  ( INPUT iiTab# AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hPageObject AS HANDLE NO-UNDO.

ASSIGN 
  hPageObject = DYNAMIC-FUNCTION("getPageObject",hParent,STRING(iiTab#))
  hPageObject = DYNAMIC-FUNCTION("getLinkedObject",hPageObject,"fieldMap","from")
  .

RETURN hPageObject.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPageFrame C-Win 
FUNCTION getPageFrame RETURNS HANDLE
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttProgram
     WHERE ttProgram.iTab# = iiTab NO-ERROR.
IF AVAIL ttProgram THEN
  RETURN ttProgram.hFrame.
ELSE
  RETURN ?. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPageHandle C-Win 
FUNCTION getPageHandle RETURNS HANDLE
  ( INPUT iiTab# AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: Returns procedure handle for page object 
    Notes:  
------------------------------------------------------------------------------*/
RETURN DYNAMIC-FUNCTION("getPageObject",hParent,STRING(iiTab#)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPageQuery C-Win 
FUNCTION getPageQuery RETURNS HANDLE
  ( INPUT iiTab# AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hPageObject  AS HANDLE NO-UNDO.
DEF VAR hQueryObject AS HANDLE NO-UNDO.

ASSIGN 
  hPageObject = DYNAMIC-FUNCTION("getPageObject",hParent,STRING(iiTab#))
  hQueryObject = DYNAMIC-FUNCTION("getLinkedObject",hPageObject,"browse","from")
  .
IF hQueryObject = ? THEN
  hQueryObject = DYNAMIC-FUNCTION("getLinkedObject",hPageObject,"query","from").

RETURN hQueryObject.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTabFrame C-Win 
FUNCTION getTabFrame RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME frmTabFrame:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTabFrames C-Win 
FUNCTION getTabFrames RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN STRING(FRAME frmProgram:HANDLE) + ","
     + STRING(FRAME frmTabFrame:HANDLE) + ","
     + STRING(TabStrip:HANDLE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitPages C-Win 
FUNCTION InitPages RETURNS LOGICAL
  ( INPUT icPageList    AS CHAR,
    INPUT ihParentQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Initialize page objects
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.

hParentQuery = ihParentQuery.

DO ix = 1 TO NUM-ENTRIES(icPageList,"|") BY 2:
  IF ix MOD 2 = 1 THEN iy = iy + 1.
  hPageObject = addFolder(iy,ENTRY(ix,icPageList,"|"),ENTRY(ix + 1,icPageList,"|"),"").
END.
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MoveTabToTop C-Win 
FUNCTION MoveTabToTop RETURNS LOGICAL
  ( INPUT ihBorderFrame AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cMoveToTopList AS CHAR NO-UNDO.
DEF VAR hWidget        AS HANDLE NO-UNDO.

IF ihBorderFrame NE ? THEN DO:
  cMoveToTopList = DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"pageframe" + STRING(getCurrentTab())).
  IF cMoveToTopList = "" THEN
    cMoveToTopList = DYNAMIC-FUNCTION("getWidgetsByLasso",ihBorderFrame,0,"frame,control-frame").
  ELSE cMoveToTopList = cMoveToTopList + "," 
                      + STRING(FRAME frmProgram:HANDLE) + ","
                      + STRING(FRAME frmTabFrame:HANDLE) + ","
                      + STRING(TabStrip:HANDLE).
END.
ELSE
  cMoveToTopList = cMoveToTopList + "," 
                 + STRING(FRAME frmProgram:HANDLE) + ","
                 + STRING(FRAME frmTabFrame:HANDLE) + ","
                 + DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"pageframe" + STRING(getCurrentTab()))
                   .

EMPTY TEMP-TABLE ttMoveToTop.

DO ix = 1 TO NUM-ENTRIES(cMoveToTopList):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cMoveToTopList)) NO-ERROR.
  IF VALID-HANDLE(hWidget) THEN DO:
    CREATE ttMoveToTop.
    ASSIGN ttMoveToTop.hMoveToTop = hWidget
           ttMoveToTop.iAreal     = hWidget:WIDTH-PIXELS * hWidget:HEIGHT-PIXELS
           .
  END.
END.
FOR EACH ttMoveToTop
    BY ttMoveToTop.iAreal DESC:
  ttMoveToTop.hMoveToTop:MOVE-TO-TOP().
END.

TabStrip:MOVE-TO-TOP().

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoResizeTabY C-Win 
FUNCTION setNoResizeTabY RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmTabFrame:HANDLE,"frmTabFrame,TabStrip").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmProgram:HANDLE,"frmProgram").
  
RETURN TRUE.

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

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setStopAtMinMaxTab C-Win 
FUNCTION setStopAtMinMaxTab RETURNS LOGICAL
  ( INPUT ibStopAtMinMaxTab AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bStopAtMinMaxTab = ibStopAtMinMaxTab.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTabFolderPosition C-Win 
FUNCTION setTabFolderPosition RETURNS LOGICAL
  (INPUT ihRectangle AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iOrgDefFrameWidth  AS INT NO-UNDO.
DEF VAR iOrgDefFrameHeight AS INT NO-UNDO.

ASSIGN 
  iOrgDefFrameWidth     = FRAME {&FRAME-NAME}:WIDTH-PIXELS
  iOrgDefFrameHeight    = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
  FRAME {&FRAME-NAME}:X = DYNAMIC-FUNCTION("getAbsPosition",ihRectangle,"X")
  FRAME {&FRAME-NAME}:Y = DYNAMIC-FUNCTION("getAbsPosition",ihRectangle,"Y")
  FRAME {&FRAME-NAME}:WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS
  FRAME {&FRAME-NAME}:HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS
  TabStrip:WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS
  TabStrip:HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS
  FRAME frmProgram:WIDTH-PIXELS  = FRAME frmProgram:WIDTH-PIXELS + FRAME {&FRAME-NAME}:WIDTH-PIXELS - iOrgDefFrameWidth
  FRAME frmProgram:HEIGHT-PIXELS = FRAME frmProgram:HEIGHT-PIXELS + FRAME {&FRAME-NAME}:HEIGHT-PIXELS - iOrgDefFrameHeight
  hRectangle             = ihRectangle
  .
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTabMoveY C-Win 
FUNCTION setTabMoveY RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmTabFrame:HANDLE,"frmTabFrame").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmTabFrame:HANDLE,"frmTabFrame").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmProgram:HANDLE,"frmProgram,TabStrip").
DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmProgram:HANDLE,"frmProgram,TabStrip").
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabFolderChanged C-Win 
FUNCTION TabFolderChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hFieldMap  AS HANDLE NO-UNDO.
DEF VAR hToolbar   AS HANDLE NO-UNDO.
DEF VAR cCurrState AS CHAR   NO-UNDO.
DEF VAR iReturn    AS INT    NO-UNDO.

IF NOT DYNAMIC-FUNCTION("SaveModified",getPageQuery(iCurrTab)) 
   AND CAN-DO(hParent:INTERNAL-ENTRIES,"UndoTabChanges") THEN
  DYNAMIC-FUNCTION("UndoTabChanges" IN hParent,iCurrTab).

IF iiTab = 0 THEN iiTab = iCurrTab.

iCurrTab = iiTab.

MoveTabToTop(FRAME {&FRAME-NAME}:HANDLE).

FIND FIRST ttProgram
     WHERE ttProgram.iTab# = iiTab NO-ERROR.
IF AVAIL ttProgram THEN
  ttProgram.hFrame:MOVE-TO-TOP().
ELSE RETURN FALSE.

chTabStrip:TabStrip:Tabs:ITEM(iiTab):SELECTED = TRUE.

IF CAN-DO(hParent:INTERNAL-ENTRIES,"TabChanged") THEN
  DYNAMIC-FUNCTION("TabChanged" IN hParent,iCurrTab).

RUN MoveToTop IN ttProgram.hProgram NO-ERROR.
/* APPLY "entry" TO ttProgram.hFrame. */

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

