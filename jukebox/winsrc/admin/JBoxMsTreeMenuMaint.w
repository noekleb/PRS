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
/* Uncomment to enable use of .Net components: */
 &SCOPED-DEFINE AdvGuiWin

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/*&SCOPED-DEFINE PureABLWin 1*/

DEF VAR bOk               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.

DEF VAR hTabFolder        AS HANDLE NO-UNDO.
DEF VAR iTab              AS INT    NO-UNDO.
DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.

DEF VAR hTempTable        AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.

DEF VAR cTreeDragNode     AS CHAR   NO-UNDO.
DEF VAR hTree             AS HANDLE NO-UNDO.
DEF VAR cSelectNode       AS CHAR   NO-UNDO.
DEF VAR hTreeViewFrame    AS HANDLE NO-UNDO.
DEF VAR iRootNode         AS INT    NO-UNDO.
DEF VAR hTestTree         AS HANDLE NO-UNDO.
DEF VAR bMultiMenu        AS LOG    NO-UNDO.
DEF VAR hTestMenu         AS HANDLE NO-UNDO.
DEF VAR hMenuLinks        AS HANDLE NO-UNDO.
DEF VAR bTranslationInst  AS LOG    NO-UNDO.
DEF VAR hMenuTranslation  AS HANDLE NO-UNDO.

DEF VAR oJBoxMsTreeView AS uc.JBoxMsTreeView NO-UNDO.

DEF TEMP-TABLE ttExpand
    FIELD cNodeIndex AS CHAR
    FIELD bExpanded  AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFolder rectWinToolbar rectToolbar ~
rectTreeView cmbMenu 
&Scoped-Define DISPLAYED-OBJECTS cmbMenu 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextMenuNum C-Win 
FUNCTION getNextMenuNum RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNodeIndex C-Win 
FUNCTION getNodeIndex RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentId C-Win 
FUNCTION getParentId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentNodeIndex C-Win 
FUNCTION getParentNodeIndex RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializePopupMenus C-Win 
FUNCTION InitializePopupMenus RETURNS LOGICAL
  ( INPUT icType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RefreshMenuDropDown C-Win 
FUNCTION RefreshMenuDropDown RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setImgSize C-Win 
FUNCTION setImgSize RETURNS LOGICAL
  ( INPUT icImage      AS CHAR,
    INPUT icImageField AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelectNode C-Win 
FUNCTION setSelectNode RETURNS LOGICAL
  ( INPUT icSelectNode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateImage C-Win 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidDropNode C-Win 
FUNCTION ValidDropNode RETURNS LOGICAL
  ( INPUT icDropId  AS CHAR,
    INPUT icDragId  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbMenu AS CHARACTER FORMAT "X(256)":U 
     LABEL "Menu" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 117 BY 25.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12.2 BY .95.

DEFINE RECTANGLE rectTreeView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 25.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8 BY .95.

DEFINE BUTTON btnSplitBarX  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE .8 BY 25
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbMenu AT ROW 1.33 COL 7 COLON-ALIGNED
     rectFolder AT ROW 2.67 COL 53
     rectWinToolbar AT ROW 1.33 COL 160.8
     rectToolbar AT ROW 1.33 COL 54
     rectTreeView AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 169.6 BY 26.91.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 13.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 38.6 ROW 2.67
         SIZE 18.4 BY 25.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 26.91
         WIDTH              = 169.6
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
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
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


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cmbMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbMenu C-Win
ON VALUE-CHANGED OF cmbMenu IN FRAME DEFAULT-FRAME /* Menu */
DO:
  RUN FillTreeView.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY "cancel".
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  IF VALID-HANDLE(hTestMenu) THEN APPLY "close" TO hTestMenu.
  IF VALID-HANDLE(hMenuLinks) THEN APPLY "CLOSE" TO hMenuLinks.
  IF VALID-HANDLE(hMenuTranslation) THEN APPLY "close" TO hMenuTranslation.
  RUN disable_UI.
  RETURN.
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON "window-resized" OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteLinkRecord C-Win 
PROCEDURE DeleteLinkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN DeleteLink IN hCurrTabProc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN DeleteRecord IN hCurrTabProc.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF bMultiMenu AND hBuffer:AVAIL AND hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE NE "menu" THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","test").
ELSE
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

IF iTab = 1 AND hBuffer:AVAIL THEN 
  DYNAMIC-FUNCTION("setAttribute",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iTab),
                   "queryjoin",
                 ",FIRST JBoxMenuToMenu NO-LOCK OUTER-JOIN"
               + " WHERE JBoxMenuToMenu.iFromMenuId = " + STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
               + " AND JBoxMenuToMenu.iToMenuId = " + getParentId()).

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropNode C-Win 
PROCEDURE DropNode :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDragId AS CHAR NO-UNDO.
DEF INPUT PARAM icDropId AS CHAR NO-UNDO.

DEF VAR iDragLevel      AS INT  NO-UNDO.
DEF VAR iDropLevel      AS INT  NO-UNDO.
DEF VAR cDragLabel      AS CHAR NO-UNDO.
DEF VAR cDropLabel      AS CHAR NO-UNDO.
DEF VAR cDragType       AS CHAR NO-UNDO.
DEF VAR cDropType       AS CHAR NO-UNDO.
DEF VAR cDragParentId   AS CHAR NO-UNDO.
DEF VAR cDragParentIdx  AS CHAR NO-UNDO.
DEF VAR cDropParentIdx  AS CHAR NO-UNDO.
DEF VAR cExtraMsg       AS CHAR NO-UNDO.
DEF VAR cReturn         AS CHAR NO-UNDO.
DEF VAR cDropMenuId     AS CHAR NO-UNDO.
DEF VAR cDragMenuId     AS CHAR NO-UNDO.

bOK = hBuffer:FIND-FIRST("WHERE iNodeIndex = " + icDropId) NO-ERROR.
IF NOT bOK THEN RETURN.

ASSIGN cDropLabel     = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
       iDropLevel     = hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE
       cDropType      = hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE
       cDropMenuId    = STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
       cDropParentIdx = hBuffer:BUFFER-FIELD("iParentNodeIndex"):STRING-VALUE
       .

hBuffer:FIND-FIRST("WHERE iNodeIndex = " + icDragId) NO-ERROR.
IF NOT bOK THEN RETURN.

ASSIGN cDragLabel     = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
       cDragType      = hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE
       cDragParentId  = hBuffer:BUFFER-FIELD("iParentMenuId"):STRING-VALUE
       cDragParentIdx = hBuffer:BUFFER-FIELD("iParentNodeIndex"):STRING-VALUE
       iDragLevel     = hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE
       cDragMenuId    = STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
       .

IF cDragParentIdx = cDropParentIdx AND NOT CAN-DO("sub-menu,menubar,tv-nav-bar",cDropType) THEN DO:
  IF NOT DYNAMIC-FUNCTION("getActionPermission",THIS-PROCEDURE:FILE-NAME,"","drop") THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Sorry, you don't have permission to do this operation","",""). 
    RETURN.  
  END.
  IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_change_menu_sequence.p",cDragMenuId + "|" + cDragParentId + "|" + cDropMenuId + "|" + cReturn,?) THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    cSelectNode = icDropId.  
    RUN FillTreeView.
  END.
  RETURN.
END.

IF icDragId = icDropId 
   OR icDropId    = cDragParentId
   OR CAN-DO("placeholder,rule,menu-item",cDropType)
   OR (NOT CAN-DO("menubar,tv-nav-bar",cDragType) AND cDropType = "menu") 
   OR NOT ValidDropNode(cDropMenuId,cDragMenuId)
   THEN
  RETURN.

RUN JBoxDSimpleSelectList.w ('Move "' + cDragLabel + '" to "' + cDropLabel + "|move|"
                           + (IF cDragParentIdx = cDropParentIdx THEN
                               'Move "' + cDragLabel + '" after "' + cDropLabel + '"|moveafter'
                              ELSE
                               'Copy "' + cDragLabel + '" to "' + cDropLabel + '"|copy')
                             ,?,
                             OUTPUT cReturn).


IF NOT DYNAMIC-FUNCTION("getActionPermission",THIS-PROCEDURE:FILE-NAME,"","drop") THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Sorry, you don't have permission to do this operation","",""). 
  RETURN.  
END.

IF cReturn NE ? THEN DO:
  IF cReturn = "moveafter" THEN DO:
    IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_change_menu_sequence.p",cDragMenuId + "|" + cDragParentId + "|" + cDropMenuId + "|" + cReturn,?) THEN 
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  ELSE DO:
    IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_drop_menu_node.p",cDragMenuId + "|" + cDragParentId + "|" + cDropMenuId + "|" + cReturn,?) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      RETURN.
    END.
  END.
  cSelectNode = icDropId.  
  RUN FillTreeView.
END.

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
  DISPLAY cmbMenu 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectFolder rectWinToolbar rectToolbar rectTreeView cmbMenu 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportRecord C-Win 
PROCEDURE ExportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_exportmenu.p","",?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTreeView C-Win 
PROCEDURE FillTreeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bEntries      AS LOG    NO-UNDO.
DEF VAR bCreTTExpand  AS LOG    NO-UNDO.
DEF VAR iRootIndex    AS INT    NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).

hBuffer:EMPTY-TEMP-TABLE().
oJBoxMsTreeView:clearNodes().
ASSIGN iRootNode  = 0
       ix         = 0
       bMultiMenu = NO.

IF cSelectNode = "" THEN DO:
  bCreTTExpand = YES.  
  EMPTY TEMP-TABLE ttExpand.
END.

/* Here we pass the buffer for the already existing temp-table to for automatic fill. hDummyTT serves just as a placeholder for the return value */
DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                  ,"WHERE " 
                 + (IF cmbMenu:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" AND cmbMenu:SCREEN-VALUE NE ? THEN 
                     "iJBoxMenuId = " + cmbMenu:SCREEN-VALUE
                    ELSE "cMenuType = 'menu'")
                  ,hTempTable:DEFAULT-BUFFER-HANDLE).

/*run toexcelviafile.p(hTempTable:DEFAULT-BUFFER-HANDLE,0).*/

hQuery:QUERY-PREPARE("FOR EACH JBoxMenu BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF cSelectNode = "" THEN
    cSelectNode = STRING(hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
  IF iRootNode = 0 THEN
    iRootNode = hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE.
  IF iRootIndex = 0 THEN
    iRootIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE.
  ELSE IF hBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE = 0 THEN
    bMultiMenu = YES.

  ix = ix + 1.

  IF bCreTTExpand THEN DO:
    CREATE ttExpand.  
    ttExpand.cNodeIndex = STRING(hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
  END.

  oJBoxMsTreeView:AddNode(IF hBuffer::iParentNodeIndex NE 0 THEN STRING(hBuffer::iParentNodeIndex) ELSE "",
                          STRING(hBuffer::iNodeIndex),
                          hBuffer::cMenuLabel 
                 + " (" + hBuffer::cMenuType + ")"
                 + " (" + STRING(hBuffer::iSeq) + ")",
                          hBuffer::cMenuType,
                          -1,
                          -1).

  hQuery:GET-NEXT().
END.

IF cSelectNode NE "" THEN DO:
  oJBoxMsTreeView:SelectNode(cSelectNode).
  oJBoxMsTreeView:ExpandNodes(cSelectNode).
END.


/*
IF ix = 0 THEN InitializePopupMenus("root").
ELSE DO:
   IF NOT bCreTTExpand THEN
     FOR EACH ttExpand
         WHERE ttExpand.bExpanded:
       DYNAMIC-FUNCTION("ExpandNode" IN hTree,ttExpand.cNodeIndex).
     END.
   ELSE IF ix < 30 THEN
     DYNAMIC-FUNCTION("ExpandTree" IN hTree, "").
   ELSE 
     DYNAMIC-FUNCTION("ExpandNode" IN hTree,STRING(iRootIndex)).       
END.

IF cSelectNode NE "" THEN DO:
  DYNAMIC-FUNCTION("SelectNode" IN hTree,cSelectNode).
  RUN tvNodeEvent("select",cSelectNode).
END.
*/
cSelectNode = "".

DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMinMaxImgSize C-Win 
PROCEDURE getMinMaxImgSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM oiMinSize    AS INT  NO-UNDO INIT 100000000.
DEF OUTPUT PARAM ocSmallImg   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocSmallNode  AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiMaxSize    AS INT  NO-UNDO.
DEF OUTPUT PARAM ocBigImg     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocBigNode    AS CHAR NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hBuff  AS HANDLE NO-UNDO.

CREATE BUFFER hBuff FOR TABLE hBuffer.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuff).
hQuery:QUERY-PREPARE("FOR EACH " + hBuff:NAME + " WHERE cMenuType NE 'menu'").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF hBuffer:BUFFER-FIELD("iImageSize"):BUFFER-VALUE > 0 AND hBuff:BUFFER-FIELD("iImageSize"):BUFFER-VALUE < oiMinSize THEN
    ASSIGN oiMinSize   = hBuff:BUFFER-FIELD("iImageSize"):BUFFER-VALUE
           ocSmallImg  = hBuff:BUFFER-FIELD("cImage"):BUFFER-VALUE
           ocSmallNode = hBuff:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           .
  IF hBuff:BUFFER-FIELD("iSelImageSize"):BUFFER-VALUE > 0 AND hBuff:BUFFER-FIELD("iSelImageSize"):BUFFER-VALUE < oiMinSize THEN
    ASSIGN oiMinSize   = hBuff:BUFFER-FIELD("iSelImageSize"):BUFFER-VALUE
           ocSmallImg  = hBuff:BUFFER-FIELD("cSelImage"):BUFFER-VALUE
           ocSmallNode = hBuff:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           .
  IF hBuff:BUFFER-FIELD("iStateImageSize"):BUFFER-VALUE > 0 AND hBuff:BUFFER-FIELD("iStateImageSize"):BUFFER-VALUE < oiMinSize THEN
    ASSIGN oiMinSize   = hBuff:BUFFER-FIELD("iStateImageSize"):BUFFER-VALUE
           ocSmallImg  = hBuff:BUFFER-FIELD("cStateImage"):BUFFER-VALUE
           ocSmallNode = hBuff:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           .

  IF hBuff:BUFFER-FIELD("iImageSize"):BUFFER-VALUE > 0 AND hBuff:BUFFER-FIELD("iImageSize"):BUFFER-VALUE > oiMaxSize THEN
    ASSIGN oiMaxSize = hBuff:BUFFER-FIELD("iImageSize"):BUFFER-VALUE
           ocBigImg  = hBuff:BUFFER-FIELD("cImage"):BUFFER-VALUE
           ocBigNode = hBuff:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           .
  IF hBuff:BUFFER-FIELD("iSelImageSize"):BUFFER-VALUE > 0 AND hBuff:BUFFER-FIELD("iSelImageSize"):BUFFER-VALUE > oiMaxSize THEN
    ASSIGN oiMaxSize = hBuff:BUFFER-FIELD("iSelImageSize"):BUFFER-VALUE
           ocBigImg  = hBuff:BUFFER-FIELD("cSelImage"):BUFFER-VALUE
           ocBigNode = hBuff:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           .
  IF hBuff:BUFFER-FIELD("iStateImageSize"):BUFFER-VALUE > 0 AND hBuff:BUFFER-FIELD("iStateImageSize"):BUFFER-VALUE > oiMaxSize THEN
    ASSIGN oiMaxSize = hBuff:BUFFER-FIELD("iStateImageSize"):BUFFER-VALUE
           ocBigImg  = hBuff:BUFFER-FIELD("cStateImage"):BUFFER-VALUE
           ocBigNode = hBuff:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           .

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuff.

IF oiMinSize = 100000000 THEN oiMinSize = 0.
IF oiMaxSize = 0 THEN oiMaxSize = 100000000.

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
RUN enable_UI.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  DYNAMIC-FUNCTION("runProc","jbsetup_fix_menu.p","",?).

  bTranslationInst = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxMenuTranslation","iJBoxMenuId").

  ASSIGN cmbMenu:DELIMITER = "|"
         cmbMenu:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("getFieldList","JBoxMenu;cMenuLabel;iJBoxMenuId","WHERE cMenuType = 'menu'").

  /*Treeview START*/
  oJBoxMsTreeView = NEW uc.JBoxMsTreeView(THIS-PROCEDURE,rectTreeView:HANDLE,"tvpics\Fold.bmp,tvpics\FoldOpen.bmp,gif\flag_no.gif").
  oJBoxMsTreeView:RegisterWithJukeBox(YES).
  oJBoxMsTreeView:setAllowDrop(YES).
  
/*  RUN JBoxJLWtree.w PERSISTENT SET hTree.                                         */
/*  DYNAMIC-FUNCTION("setImageList" IN hTree,"tvpics\fold.bmp;tvpics\foldopen.bmp").*/
/*  DYNAMIC-FUNCTION("setImage" IN hTree,"fold.bmp").                               */
/*  DYNAMIC-FUNCTION("setSelImage" IN hTree,"tvpics\foldopen.bmp").                 */
/*  DYNAMIC-FUNCTION("setTreeStyle" IN hTree,2).                                    */
/*  DYNAMIC-FUNCTION("InitTreeView" IN hTree,rectTreeView:HANDLE).                  */
/*  hTreeViewFrame = DYNAMIC-FUNCTION("getTreeViewFrame" IN hTree).                 */
/*  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "tvNodeEvent" IN hTree.                   */

  /* Get the menu temp-table def. and create a query for it so the query can be linked to the fieldmap in the folder object.
     Note that the query is assigned the temp-table so it doesn't have to go to the server for the data definitions
     (it would not even be possible to get the same struct directly from the db) */
  hTempTable = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p","where false",?).
  hQuery = DYNAMIC-FUNCTION("NewQuery",0,"","JBoxMenu","where false",
                            "TEMP-TABLE|" + STRING(hTempTable)).
  hBuffer = DYNAMIC-FUNCTION("NewFieldMap"
          ,hQuery
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"",""
          ,"",""
          ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hBuffer,hQuery).

  DYNAMIC-FUNCTION("setAttribute",hQuery,"uselocaldata","").

  /*TreeView END*/


  DYNAMIC-FUNCTION("setAttribute",rectFolder:HANDLE,"tabFolderProg","JBoxJLWtabFolder.w").   /* ny */
  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be provided here and
     they must be exact the same as in setOrwWinSize (under) */
  DYNAMIC-FUNCTION("setMinXYmove",2000,1500). 

  DYNAMIC-FUNCTION("setImageList" IN hTabFolder,"newbmp\regblue.bmp;newbmp\user16.bmp") NO-ERROR.

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,
                   "Menu details|JBoxJlwMenuView.w|User groups|JBoxViewUserGroup.w",hQuery).
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).


  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "File",
                             "Test"
                           + (IF PROVERSION GE "10.2B" THEN ",TestAsRibbon;Test as ribbon menu" ELSE "")
                           + ",excel"
                           + (IF bTranslationInst THEN
                               ",Translate¤menu"
                              ELSE "")
                           + ",MenuLinks;Manage menu links¤MENU"
                           + ",Export¤menu"
                           + ",Search;Search program (launch)¤menu"
                           + ",SearchLabel;Se&arch menu label¤menu"
                            ,"maxborder").

  DYNAMIC-FUNCTION("createObjectLink",hToolbar,hQuery).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBuffer).

  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonTest"))
                  ,"ShowImgList;Show image lists"
                  ,"").

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "File",
                             "close;E&xit",
                             "right|enable").

  InitializeResize().
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

/*   InitializePopupMenus("root").  */

  SESSION:SET-WAIT-STATE("").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuBarRecord C-Win 
PROCEDURE MenuBarRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN NewMenuItem IN hCurrTabProc ("MENUBAR").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuItemRecord C-Win 
PROCEDURE MenuItemRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN NewMenuItem IN hCurrTabProc ("MENU-ITEM").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuLinksRecord C-Win 
PROCEDURE MenuLinksRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hMenuLinks) THEN DO:
  RUN JboxSubMenuMaint.w PERSISTENT SET hMenuLinks.
  RUN InitializeObject IN hMenuLinks.
END.
RUN MoveToTop IN hMenuLinks NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuPlaceHolderRecord C-Win 
PROCEDURE MenuPlaceHolderRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN NewMenuItem IN hCurrTabProc ("PLACEHOLDER").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuRecord C-Win 
PROCEDURE MenuRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN NewMenuItem IN hCurrTabProc ("MENU").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuRuleRecord C-Win 
PROCEDURE MenuRuleRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN NewMenuItem IN hCurrTabProc ("RULE").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuSubRecord C-Win 
PROCEDURE MenuSubRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN NewMenuItem IN hCurrTabProc ("SUB-MENU").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuTvNavBarRecord C-Win 
PROCEDURE MenuTvNavBarRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN NewMenuItem IN hCurrTabProc ("TV-NAV-BAR").

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
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
RUN MoveToTop IN hCurrTabProc NO-ERROR.
APPLY "entry" TO cmbMenu IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunMenuItem C-Win 
PROCEDURE RunMenuItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBuffer:AVAIL THEN RETURN.
  
IF VALID-HANDLE(hTestMenu) THEN DO:
  RUN ApplyMenu IN hTestMenu (?,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,"") NO-ERROR.
  IF RETURN-VALUE = "not avail" THEN
    MESSAGE "Run the testmenu again to have the new node populated"
            VIEW-AS ALERT-BOX INFORMATION.
END.
ELSE DO:
  RUN ApplyMenu IN hParent (?,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE,"") NO-ERROR.
  IF RETURN-VALUE = "not avail" THEN
    MESSAGE "Run the testmenu to have the new node populated"
            VIEW-AS ALERT-BOX INFORMATION.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SearchLabelRecord C-Win 
PROCEDURE SearchLabelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn     AS CHAR   NO-UNDO.
DEF VAR iReturn     AS INT    NO-UNDO.
DEF VAR cNode       AS CHAR   NO-UNDO.
DEF VAR cParent     AS CHAR   NO-UNDO.
DEF VAR cExpandList AS CHAR   NO-UNDO.
DEF VAR hSearchBuff AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR cNodeList   AS CHAR   NO-UNDO.

RUN JBoxAskForValue.w ("Search for menu label","CHARACTER|x(40)",INPUT-OUTPUT cReturn,OUTPUT iReturn).
IF iReturn = 2 AND cReturn NE "" THEN DO:

  CREATE QUERY hQuery.
  CREATE BUFFER hSearchBuff FOR TABLE hBuffer.
  hQuery:SET-BUFFERS(hSearchBuff).
  hQuery:QUERY-PREPARE("FOR EACH " + hSearchBuff:NAME
                     + " WHERE cMenuLabel " 
                     + (IF INDEX(cReturn,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '") 
                     + cReturn + "'").

  hQuery:QUERY-OPEN().
  ix = 0.
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ASSIGN ix        = ix + 1.
           cNodeList = cNodeList + (IF cNodeList NE "" THEN "|" ELSE "")
                     + hSearchBuff:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE 
                     + "|" + hSearchBuff:BUFFER-FIELD("iNodeIndex"):STRING-VALUE
                     + ";" + hBuffer:BUFFER-FIELD("iParentNodeIndex"):STRING-VALUE.
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hSearchBuff.
  DELETE OBJECT hQuery.

  IF ix > 1 THEN 
    RUN JBoxDSimpleSelectList.w (cNodeList,?,OUTPUT cReturn).
  ELSE IF ix = 1 THEN
    cReturn = ENTRY(2,cNodeList,"|").
  ELSE DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Not found","","").
    cReturn = "".
  END.

  IF cReturn NE "" AND cReturn NE ? THEN DO:
    ASSIGN cNode   = ENTRY(1,cReturn,";")
           cParent = ENTRY(2,cReturn,";").

    DO ix = 1 TO 10:
      bOk = hBuffer:FIND-FIRST("WHERE iNodeIndex = " + cParent) NO-ERROR.
      IF bOk THEN DO:
        ASSIGN cExpandList = hBuffer:BUFFER-FIELD("iNodeIndex"):STRING-VALUE + "|" + cExpandList
               cParent = hBuffer:BUFFER-FIELD("iParentNodeIndex"):STRING-VALUE.
      END.
      ELSE LEAVE.
    END.
    
    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    DYNAMIC-FUNCTION("ExpandTree" IN hTree,TRIM(cExpandList,"|")).
    DYNAMIC-FUNCTION("SelectNode" IN hTree,cNode).
    RUN tvNodeEvent("select",cNode).
    DYNAMIC-FUNCTION("DoLockWindow",?).
  END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SearchRecord C-Win 
PROCEDURE SearchRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn     AS CHAR NO-UNDO.
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR cNode       AS CHAR NO-UNDO.
DEF VAR cParent     AS CHAR NO-UNDO.
DEF VAR cExpandList AS CHAR NO-UNDO.
DEF VAR hSearchBuff AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR cNodeList   AS CHAR   NO-UNDO.

RUN JBoxAskForValue.w ("Search for program name","CHARACTER|x(40)",INPUT-OUTPUT cReturn,OUTPUT iReturn).

IF iReturn = 2 AND cReturn NE "" THEN DO:

  CREATE QUERY hQuery.
  CREATE BUFFER hSearchBuff FOR TABLE hBuffer.
  hQuery:SET-BUFFERS(hSearchBuff).
  hQuery:QUERY-PREPARE("FOR EACH " + hSearchBuff:NAME
                     + " WHERE cLaunch " 
                     + (IF INDEX(cReturn,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '") 
                     + cReturn + "'").

  hQuery:QUERY-OPEN().
  ix = 0.
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    IF LOOKUP(hSearchBuff::cLaunch,cNodeList,"|") = 0 THEN
      ASSIGN ix        = ix + 1.
             cNodeList = cNodeList + (IF cNodeList NE "" THEN "|" ELSE "")
                       + hSearchBuff::cLaunch 
                       + "|" + hSearchBuff:BUFFER-FIELD("iNodeIndex"):STRING-VALUE
                       + ";" + hBuffer:BUFFER-FIELD("iParentNodeIndex"):STRING-VALUE.
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hSearchBuff.
  DELETE OBJECT hQuery.

  IF ix > 1 THEN 
    RUN JBoxDSimpleSelectList.w (cNodeList,?,OUTPUT cReturn).
  ELSE IF ix = 1 THEN
    cReturn = ENTRY(2,cNodeList,"|").
  ELSE DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Not found","","").
    cReturn = "".
  END.

  IF cReturn NE "" AND cReturn NE ? THEN DO:
    ASSIGN cNode   = ENTRY(1,cReturn,";")
           cParent = ENTRY(2,cReturn,";").

    DO ix = 1 TO 10:
      bOk = hBuffer:FIND-FIRST("WHERE iNodeIndex = " + cParent) NO-ERROR.
      IF bOk THEN DO:
        ASSIGN cExpandList = hBuffer:BUFFER-FIELD("iNodeIndex"):STRING-VALUE + "|" + cExpandList
               cParent = hBuffer:BUFFER-FIELD("iParentNodeIndex"):STRING-VALUE.
      END.
      ELSE LEAVE.
    END.

   MESSAGE 'TRIM(cExpandList,"|")' TRIM(cExpandList,"|") skip
           'cNode' cNode
   VIEW-AS ALERT-BOX.
    oJBoxMsTreeView:ExpandNodes(TRIM(cExpandList,"|")).
    oJBoxMsTreeView:SelectNode(cNode).
    
    /*

    DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
    DYNAMIC-FUNCTION("ExpandTree" IN hTree,TRIM(cExpandList,"|")).
    DYNAMIC-FUNCTION("SelectNode" IN hTree,cNode).
    RUN tvNodeEvent("select",cNode).
    DYNAMIC-FUNCTION("DoLockWindow",?).
    */
  END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowImgListRecord C-Win 
PROCEDURE ShowImgListRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hTestTree) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,20,
                    "Small images: " + DYNAMIC-FUNCTION("getImageList" IN hTestTree) + CHR(10) +
                    "Large images: " + DYNAMIC-FUNCTION("getLargeImageList" IN hTestTree),"","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TestAsRibbonRecord C-Win 
PROCEDURE TestAsRibbonRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hTestMenu) THEN DO:
  RUN CloseMenu IN hTestMenu.
  DELETE PROCEDURE hTestMenu NO-ERROR.    
END.

RUN SelectAppLanguage.

IF DYNAMIC-FUNCTION("getAttribute",SESSION,"mainMenuType") NE "ribbon" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainMenuType","ribbon").
  RUN RestartMenu IN hParent.
END.
ELSE DO:
  RUN JBoxRibbonMenu.p PERSIST SET hTestMenu.
  
  IF NOT bMultiMenu THEN
    RUN InitializeObject IN hTestMenu (iRootNode).
  ELSE
    RUN InitializeObject IN hTestMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).
  
  hTestTree = DYNAMIC-FUNCTION("getTreeHandle" IN hTestMenu) NO-ERROR.

  DYNAMIC-FUNCTION("setWindowTitle" IN hTestMenu,"Test").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainMenuType","ribbon").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TestRecord C-Win 
PROCEDURE TestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLanguages AS CHAR NO-UNDO.
DEF VAR cSelLang   AS CHAR NO-UNDO.
DEF VAR cReturn    AS CHAR NO-UNDO.

IF VALID-HANDLE(hTestMenu) THEN
  APPLY "close" TO hTestMenu.

RUN SelectAppLanguage.

/* IF DYNAMIC-FUNCTION("getAttribute",SESSION,"mainMenuType") = "ribbon" THEN DO: */
/*   DYNAMIC-FUNCTION("setAttribute",SESSION,"mainMenuType","nav-bar").           */
/*   RUN RestartMenu IN hParent.                                                  */
/* END.                                                                           */
/* ELSE DO:                                                                       */
  RUN JBoxJlwDynMenu.w PERSIST SET hTestMenu.
  
  IF NOT bMultiMenu THEN
    RUN InitializeObject IN hTestMenu (iRootNode).
  ELSE
    RUN InitializeObject IN hTestMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).
  
  hTestTree = DYNAMIC-FUNCTION("getTreeHandle" IN hTestMenu).
/* END. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TranslateRecord C-Win 
PROCEDURE TranslateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hMenuTranslation) THEN DO:
  RUN JBoxMenuTranslation.w PERSISTENT SET hMenuTranslation.
  RUN InitializeObject IN hMenuTranslation.
END.
RUN MoveToTop IN hMenuTranslation NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TreeDropNode C-Win 
PROCEDURE TreeDropNode :
DEF INPUT PARAM icDragNode  AS CHAR NO-UNDO.
DEF INPUT PARAM icDropNode  AS CHAR NO-UNDO.

RUN DropNode (icDragNode,icDropNode).
/* code suggestion.. validate where the node can be dropped:
IF NOT CAN-DO(cRegionList,icDropNode) OR CAN-DO(cRegionList,icDragNode) THEN RETURN.
*/
/* After a successful drop rebuild the tree and expand the dropped node:
IF JBoxServerAPI:Instance:Update("Salesrep","Salesrep",icDragNode,"Region",icDropNode,NO) THEN DO:
  FillTreeView().
  oJBoxMsTreeView:ExpandNodes(icDropNode).
END.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TreeNodeSelectedRecord C-Win 
PROCEDURE TreeNodeSelectedRecord :
  DEF INPUT PARAM icNode AS CHAR NO-UNDO.

MESSAGE "icNode" icNode
VIEW-AS ALERT-BOX.

  bOK = hBuffer:FIND-FIRST("WHERE iNodeIndex  = " + icNode) NO-ERROR.
  IF bOk THEN DO:
    RUN InvokeMethod(hQuery,"DisplayRecord").
/*    InitializePopupMenus(hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE).*/
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
DEF INPUT PARAM icEvent   AS CHAR NO-UNDO.
DEF INPUT PARAM icNodeKey AS CHAR NO-UNDO.

IF CAN-DO("parentselect,select",icEvent) THEN DO:
  bOK = hBuffer:FIND-FIRST("WHERE iNodeIndex   = " + icNodeKey) NO-ERROR.
  IF bOk THEN DO:
    RUN InvokeMethod(hQuery,"DisplayRecord").
    InitializePopupMenus(hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE).
  END.
END.
ELSE IF CAN-DO("expand",icEvent) THEN DO:
  FIND FIRST ttExpand
       WHERE ttExpand.cNodeIndex = icNodeKey
       NO-ERROR.
  IF AVAIL ttExpand THEN 
    ttExpand.bExpanded = NOT ttExpand.bExpanded.
END.
ELSE IF icEvent = "dragdrop" THEN
  RUN DropNode(ENTRY(1,icNodeKey,"|"),ENTRY(2,icNodeKey,"|")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WhereUsedRecord C-Win 
PROCEDURE WhereUsedRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF iTab NE 1 THEN
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
RUN WhereUsed IN hCurrTabProc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextMenuNum C-Win 
FUNCTION getNextMenuNum RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hNextNumBuff AS HANDLE NO-UNDO.
DEF VAR cNextNum     AS CHAR   NO-UNDO.
DEF VAR iNum         AS INT    NO-UNDO.
DEF VAR bNumeric     AS LOG    NO-UNDO INIT YES.

CREATE BUFFER hNextNumBuff FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hNextNumBuff).
hQuery:QUERY-PREPARE("FOR EACH " + hNextNumBuff:NAME + " WHERE cMenuType = 'menu-item'").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iNum = INT(hNextNumBuff:BUFFER-FIELD("cMenuNumber"):BUFFER-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    bNumeric = NO.
    LEAVE.
  END.
  hQuery:GET-NEXT().
END.    

hQuery:QUERY-CLOSE().

hQuery:QUERY-PREPARE("FOR EACH " + hNextNumBuff:NAME 
                   + " WHERE cMenuType = 'menu-item'"
                   + " BY "
                   + (IF bNumeric THEN "INT(" ELSE "")
                   + "cMenuNumber"
                   + (IF bNumeric THEN ")" ELSE "")
                   + " DESC").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
IF hNextNumBuff:AVAIL THEN DO:
  IF bNumeric THEN
    cNextNum = STRING(INT(hNextNumBuff:BUFFER-FIELD("cMenuNumber"):BUFFER-VALUE) + 1).
  ELSE hNextNumBuff:BUFFER-FIELD("cMenuNumber"):BUFFER-VALUE.
END.

DELETE OBJECT hNextNumBuff.
DELETE OBJECT hQuery.

RETURN cNextNum.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNodeIndex C-Win 
FUNCTION getNodeIndex RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hBuffer:AVAIL THEN
  RETURN STRING(hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
ELSE 
  MESSAGE PROGRAM-NAME(1) SKIP
          "Tree node not available!!" SKIP
          VIEW-AS ALERT-BOX.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentId C-Win 
FUNCTION getParentId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hBuffer:AVAIL THEN 
  RETURN STRING(hBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE).
ELSE
  RETURN "0".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentNodeIndex C-Win 
FUNCTION getParentNodeIndex RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hBuffer:AVAIL THEN
  RETURN STRING(hBuffer:BUFFER-FIELD("iParentNodeIndex"):BUFFER-VALUE).
ELSE 
  MESSAGE PROGRAM-NAME(1) SKIP
          "Tree node not available!!" SKIP
          VIEW-AS ALERT-BOX.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializePopupMenus C-Win 
FUNCTION InitializePopupMenus RETURNS LOGICAL
  ( INPUT icType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*
DELETE OBJECT hTreeViewFrame:POPUP-MENU NO-ERROR.

/* If there is no current menu items: */
IF icType = "root" THEN
  hPopupRoot =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "New;New;MenuBarRecord"
                      ,"").     

/* If current menu item is a menubar: */
ELSE IF CAN-DO("menu",icType) THEN DO:
  hPopupMenuBar =  DYNAMIC-FUNCTION("NewMenuBand",
                    hTreeViewFrame,  /* parent widget */
                    "|New"
                    + ",Delete"
                    + ",Test"
                    ,"").     
  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTreeViewFrame,"placeholder1")),  /* parent widget */
                    "Menubar;Menubar;MenuBarRecord"
                    + ",Nav-bar;Treeview/Nav-bar;MenuTvNavBarRecord"
                    + ",Menu;Menu;MenuRecord"
                    ,"").     
END.

ELSE IF CAN-DO("menubar",icType) THEN DO:
  hPopupMenuBar =  DYNAMIC-FUNCTION("NewMenuBand",
                    hTreeViewFrame,  /* parent widget */
                    "|New"
                    + ",Delete"
                    + ",DeleteLink;Delete link to parent"
                    + ",WhereUsed;Where used.."
                    ,"").     
  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTreeViewFrame,"placeholder1")),  /* parent widget */
                    "Sub-menu;Sub-menu;MenuSubRecord"
                    + ",Menu-item;Menu-item;MenuItemRecord"
                    + ",Placeholder;Placeholder;MenuPlaceHolderRecord"
                    ,"").     
END.

ELSE IF CAN-DO("tv-nav-bar",icType) THEN DO:
  hPopupMenuBar =  DYNAMIC-FUNCTION("NewMenuBand",
                    hTreeViewFrame,  /* parent widget */
                    "|New"
                    + ",Delete"
                    + ",DeleteLink;Delete link to parent"
                    + ",WhereUsed;Where used.."
                    ,"").     
  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTreeViewFrame,"placeholder1")),  /* parent widget */
                    "Sub-menu;Sub-menu;MenuSubRecord"
                    ,"").     
END.

/* If current menu is a sub-menu: */
ELSE IF icType = "sub-menu" THEN DO:   
  hPopupSubMenu =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "|New"
                      + ",Delete"
                      + ",DeleteLink;Delete link to parent"
                      + ",WhereUsed;Where used.."
                      ,"").     
  DYNAMIC-FUNCTION("NewMenuBand",
                      WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTreeViewFrame,"placeholder1")),  /* parent widget */
                      "Menu-item;Menu-item;MenuItemRecord"
                      + ",Sub-menu;Sub-menu;MenuSubRecord"
                      + ",Ruler;Rule;MenuRuleRecord"
                      + ",Placeholder;Placeholder;MenuPlaceholderRecord"
                      ,"").     
END.
ELSE IF CAN-DO("menu-item",icType) THEN
  hPopupSubMenu =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "Delete"
                    + ",DeleteLink;Delete link to parent"
                    + ",RunMnuItem;Run;RunMenuItem"
                    + ",WhereUsed;Where used.."
                      ,"").     
ELSE IF CAN-DO("placeholder",icType) THEN
  hPopupSubMenu =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "Delete"
                    + ",DeleteLink;Delete link to parent"
                      ,"").     
ELSE IF CAN-DO("rule",icType) THEN
  hPopupSubMenu =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "Delete"
                      ,"").     
*/
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Note how the search combo (cmbMenu) is set to make it follow the split-bar only
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
/*  DYNAMIC-FUNCTION("setNoResizeX" , THIS-PROCEDURE:CURRENT-WINDOW, hTreeViewFrame,hTreeViewFrame:NAME).*/
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cmbMenu").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cmbMenu").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectTreeView,cmbMenu").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").

  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(cmbMenu:HANDLE) + "," +
                    DYNAMIC-FUNCTION("getToolBarHandles",hToolBar,"button,rule") + "," +
                    STRING(rectTreeView:HANDLE) + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle")
                    ).

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,800,700,0,0).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RefreshMenuDropDown C-Win 
FUNCTION RefreshMenuDropDown RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cmbMenu:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = "||" + DYNAMIC-FUNCTION("getFieldList","JBoxMenu;cMenuLabel;iJBoxMenuId","WHERE cMenuType = 'menu'").
  
cmbMenu:SCREEN-VALUE = "".

APPLY "value-changed" TO cmbMenu.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setImgSize C-Win 
FUNCTION setImgSize RETURNS LOGICAL
  ( INPUT icImage      AS CHAR,
    INPUT icImageField AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icImage = "" THEN RETURN NO.
IF SEARCH(icImage) = ? THEN RETURN NO.

FILE-INFO:FILE-NAME = icImage.

IF hBuffer:AVAIL THEN
  hBuffer:BUFFER-FIELD("icImageField"):BUFFER-VALUE = FILE-INFO:FILE-SIZE.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelectNode C-Win 
FUNCTION setSelectNode RETURNS LOGICAL
  ( INPUT icSelectNode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cSelectNode = icSelectNode.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hCustBrowse AS HANDLE NO-UNDO.

IF iTab > 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hQuery,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iTab)).

/* NB: Since the same menu can appear multiple places in the menu we don't want the automatic sync from child
       to sibling that we get when using the onetoone link: */
IF iiTab = 1 THEN DO:
  DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hQuery,"iJBoxMenuId").
  /* NB: We don't want automatic assignment of the parent id either since this is really a one-to-one relationship: */
  DYNAMIC-FUNCTION("setAttribute",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),"parentlinkfields","").
END.

hCurrTabProc = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab).

iTab = iiTab.

RUN InvokeMethod(hQuery,"DisplayRecord").

RUN MoveToTop IN hCurrTabProc NO-ERROR.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateImage C-Win 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iImageSize      AS INT  NO-UNDO.
DEF VAR iCurrMinSize    AS INT  NO-UNDO.
DEF VAR iCurrMaxSize    AS INT  NO-UNDO.
DEF VAR cCurrSmallImg   AS CHAR NO-UNDO.
DEF VAR cCurrSmallNode  AS CHAR NO-UNDO.
DEF VAR cCurrBigImg     AS CHAR NO-UNDO.
DEF VAR cCurrBigNode    AS CHAR NO-UNDO.

IF icImage = "" THEN RETURN YES.

IF SEARCH(icImage) = ? THEN DO:
  MESSAGE "Invalid image: " icImage SKIP "Not in PROPATH"
          VIEW-AS ALERT-BOX ERROR.
  RETURN NO.
END.

FILE-INFO:FILE-NAME = icImage.

IF NOT icImage MATCHES "*.bmp" AND NOT icImage MATCHES "*.ico" AND NOT icImage MATCHES "*.png" AND NOT icImage MATCHES "*.gif" 
   AND NOT icImage MATCHES "*.jpg"
   THEN DO:
  MESSAGE "Invalid image type for " icImage SKIP "Valid types: bmp,ico,png,gif"
          VIEW-AS ALERT-BOX ERROR.
  RETURN NO.
END.

iImageSize = FILE-INFO:FILE-SIZE.
    
RUN getMinMaxImgSize (OUTPUT iCurrMinSize,OUTPUT cCurrSmallImg,OUTPUT cCurrSmallNode,
                      OUTPUT iCurrMaxSize,OUTPUT cCurrBigImg,OUTPUT cCurrBigNode).

IF iImageSize < iCurrMinSize THEN DO:
  MESSAGE "Image " icImage ", size " iImageSize SKIP  
          "is smaller than the smallest image currently defined in the menu: " SKIP 
          cCurrSmallImg " for node " cCurrSmallNode ", size " iCurrMinSize SKIP(1)
          "If " cCurrSmallImg " is used in a navigation bar this is probably ok," SKIP 
          "otherwise it might prevent the image to load - Continue?"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE bOk.
  RETURN bOk.
END.
IF iImageSize > iCurrMaxSize THEN DO:
  MESSAGE "Image " icImage ", size " iImageSize SKIP  
          "is larger than the largest image currently defined in the menu: " SKIP 
          cCurrBigImg " for node " cCurrBigNode ", size " iCurrMinSize SKIP(1)
          "If " cCurrBigImg " is used in a navigation bar this is probably ok," SKIP 
          "otherwise it might prevent the image to load - Continue?"
          VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE bOk.
  RETURN bOk.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidDropNode C-Win 
FUNCTION ValidDropNode RETURNS LOGICAL
  ( INPUT icDropId  AS CHAR,
    INPUT icDragId  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Check if the target (drop) node is a child of the drag node. If so, return false. 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iDropParent AS INT NO-UNDO.

bOK = hBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + icDropId) NO-ERROR.
IF bOK THEN DO:
  iDropParent = hBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE.
  IF iDropParent = INT(icDragId) THEN RETURN FALSE.
  ELSE ValidDropNode (STRING(iDropParent),icDragId).
END.
ELSE RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

