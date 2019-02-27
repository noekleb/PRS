&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
DEF INPUT  PARAM icQryObjects         AS CHAR NO-UNDO.
DEF INPUT  PARAM icFmObjects          AS CHAR NO-UNDO.
DEF INPUT  PARAM icToolCat            AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocToolbarName AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocToolbarDef  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocToolbarProp AS CHAR NO-UNDO.
DEF OUTPUT PARAM obFlat               AS LOG  NO-UNDO.
DEF OUTPUT PARAM ocTools              AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocQryObject          AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocFmObject           AS CHAR NO-UNDO.
DEF OUTPUT PARAM ofStartRow           AS DEC  NO-UNDO.
DEF OUTPUT PARAM ofStartCol           AS DEC  NO-UNDO.
DEF OUTPUT PARAM ofBtnWidth           AS DEC  NO-UNDO INIT 4.6.
DEF OUTPUT PARAM ofBtnHeight          AS DEC  NO-UNDO INIT 1.1.
DEF OUTPUT PARAM obOk                 AS LOG  NO-UNDO.

/* Local Variable Definitions ---                                       */
{adeuib/uniwidg.i}           /* Universal Widget TEMP-TABLE definition   */
{adeuib/sharvars.i}          /* Define _h_win, _frmx, _next_draw etc.    */
{adeuib/layout.i}            /* Layout temp-table definitions            */  

{jukebox/ttJBoxTool.i SHARED}

DEF VAR fStartCol   AS DEC  NO-UNDO.
DEF VAR cAllBuiltIn AS CHAR NO-UNDO. /* all buttons with built-in corresponding methods */
DEF VAR cToolList   AS CHAR NO-UNDO. /* all standard buttons */
DEF VAR cImgList    AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.

DEF VAR cCtrlHotkeyActions  AS CHAR NO-UNDO.
DEF VAR cCtrlHotkeys        AS CHAR NO-UNDO.
DEF VAR cAltHotkeyActions   AS CHAR NO-UNDO.
DEF VAR cAltHotkeys         AS CHAR NO-UNDO.
DEF VAR cFunKeyActs         AS CHAR NO-UNDO.
DEF VAR cFunKeys            AS CHAR NO-UNDO.
DEF VAR idxGroup            AS INT  NO-UNDO.  
DEF VAR idxToolSeq          AS INT  NO-UNDO.
DEF VAR hSeq                AS HANDLE NO-UNDO.
DEF VAR hBrwMove            AS HANDLE NO-UNDO.
DEF VAR bSelectToolGroup    AS LOG  NO-UNDO.
DEF VAR hToolList           AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hLabelCol           AS HANDLE NO-UNDO.
DEF VAR hParentMenuCol      AS HANDLE NO-UNDO.
DEF VAR hTooltipCol         AS HANDLE NO-UNDO.
DEF VAR hWidthCol           AS HANDLE NO-UNDO.
DEF VAR hAlwaysEnableCol    AS HANDLE NO-UNDO.
DEF VAR hViewAsCol          AS HANDLE NO-UNDO.
DEF VAR hToggleCol          AS HANDLE NO-UNDO.
DEF VAR cCurrRootMenu       AS CHAR NO-UNDO.
DEF VAR iNewSeq             AS INT NO-UNDO.
DEF VAR cMenuParentFilter   AS CHAR NO-UNDO.
DEF VAR hSelectorPanel      AS HANDLE NO-UNDO.
DEF VAR iInsertAfter        AS INT  NO-UNDO.
DEF VAR cAddToParentMenu    AS CHAR NO-UNDO.
DEF VAR iMasterSeq          AS INT  NO-UNDO.
DEF VAR rReposRow           AS ROWID NO-UNDO.
DEF VAR cFirstQry           AS CHAR NO-UNDO.
DEF VAR cFirstFm            AS CHAR NO-UNDO.

DEF BUFFER b_U FOR _U.
DEF BUFFER b_L FOR _L.
DEF BUFFER b_F FOR _F.


DEF TEMP-TABLE ttToolbar
    FIELD cTbName  AS CHAR
    FIELD cActList AS CHAR
    FIELD cBorder  AS CHAR
    .

DEF TEMP-TABLE ttToolGroup
    FIELD iGroupId   AS INT
    FIELD cGroupName AS CHAR
    FIELD cGroup     AS CHAR
    .

DEF TEMP-TABLE ttToolsForGroup
    FIELD iGroupId AS INT
    FIELD cTool    AS CHAR
    FIELD iSeq     AS INT
    .

DEF TEMP-TABLE ttValidateMove
    FIELD iSeq AS INT.

DEF TEMP-TABLE ttToolDef 
    FIELD cTool         AS CHAR 
    FIELD cLabel        AS CHAR
    FIELD cViewAs       AS CHAR
    FIELD cType         AS CHAR
    FIELD cSubMenu      AS CHAR
    FIELD cParentMenu   AS CHAR
    FIELD bAlwaysEnable AS LOG
    FIELD iSeq          AS INT   
    . 

DEF TEMP-TABLE ttMenu 
    FIELD iSeq        AS CHAR
    FIELD cTool       AS CHAR 
    FIELD cParentMenu AS CHAR
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwTool

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttJBoxTool

/* Definitions for BROWSE brwTool                                       */
&Scoped-define FIELDS-IN-QUERY-brwTool iSeq /* cTool */ cViewTool cLabel /* cImage fWidth cViewAs and Menu-item", "Menu-item", "Toggle Menu-item", "Button" DROP-DOWN-LIST */ cParentMenu menu" DROP-DOWN-LIST bAlwaysEnable bToggle /* cToolTip */ bBuiltIn /* cType */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwTool cLabel /* cTooltip fWidth cViewAs */ cParentMenu ~
 bAlwaysEnable ~
bToggle   
&Scoped-define SELF-NAME brwTool
&Scoped-define QUERY-STRING-brwTool FOR EACH ttJBoxTool
&Scoped-define OPEN-QUERY-brwTool OPEN QUERY {&SELF-NAME} FOR EACH ttJBoxTool.
&Scoped-define TABLES-IN-QUERY-brwTool ttJBoxTool
&Scoped-define FIRST-TABLE-IN-QUERY-brwTool ttJBoxTool


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAddTool btnToolGroup btnAddCustomTool ~
btnAddRule btnAddSubMenu btnDelete btnMoveUp btnMoveDown btnSelectCode ~
cmbSubMenu brwTool btnSplitBarY edCode edTip fiToolbarName cmbApplyToQry ~
cmbApplyToFm btnOk btnCancel tbCreateProcs fiCodeLabel fiTipLabel 
&Scoped-Define DISPLAYED-OBJECTS cmbSubMenu edCode edTip fiToolbarName ~
cmbApplyToQry cmbApplyToFm tbCreateProcs fiCodeLabel fiTipLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addMenusFromToolDef C-Win 
FUNCTION addMenusFromToolDef RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD creToolDef C-Win 
FUNCTION creToolDef RETURNS LOGICAL
  ( INPUT icToolList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillTbTemplates C-Win 
FUNCTION fillTbTemplates RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuList C-Win 
FUNCTION getMenuList RETURNS CHARACTER
  ( INPUT ibPairs AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuNodes C-Win 
FUNCTION getMenuNodes RETURNS CHARACTER
  ( INPUT icParentMenu AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ImportConfig C-Win 
FUNCTION ImportConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD leaveFiMenu C-Win 
FUNCTION leaveFiMenu RETURNS LOGICAL
  ( INPUT ihFiMenu    AS HANDLE,
    INPUT ihTbAdd     AS HANDLE,
    INPUT ihBtnFilter AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadToolbarDef C-Win 
FUNCTION LoadToolbarDef RETURNS LOGICAL
  ( INPUT icToolbarDef AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD makeRoomForInsert C-Win 
FUNCTION makeRoomForInsert RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD okMove C-Win 
FUNCTION okMove RETURNS LOGICAL
  ( INPUT icDir AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD reSeqSubMenu C-Win 
FUNCTION reSeqSubMenu RETURNS LOGICAL
  ( INPUT iiRootSeq AS INT,
    INPUT icParent  AS CHAR,
    INPUT iiLevel   AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD reSequenceMenu C-Win 
FUNCTION reSequenceMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCode C-Win 
FUNCTION setCode RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuCombos C-Win 
FUNCTION setMenuCombos RETURNS LOGICAL
  ( INPUT icExclude AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuFilter C-Win 
FUNCTION setMenuFilter RETURNS LOGICAL
  ( INPUT ihBtnFilter AS HANDLE,
    INPUT ihFiMenu    AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD unCheckTbAdd C-Win 
FUNCTION unCheckTbAdd RETURNS LOGICAL
  ( INPUT ihTbAdd  AS HANDLE,
    INPUT ihFillIn AS HANDLE,
    INPUT ihFilter AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateImage C-Win 
FUNCTION ValidateImage RETURNS CHARACTER
  ( INPUT icFileName    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_Save_toolbar_definition LABEL "Save toolbar definition"
       MENU-ITEM m_Load_toolbar_definition LABEL "Load toolbar definition".

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_File         LABEL "File"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddCustomTool 
     LABEL "Add custom tool" 
     SIZE 18 BY 1.14 TOOLTIP "Add custom tool".

DEFINE BUTTON btnAddRule 
     LABEL "Add rule" 
     SIZE 11 BY 1.14 TOOLTIP "Add custom tool".

DEFINE BUTTON btnAddSubMenu 
     LABEL "Add sub-menu" 
     SIZE 16.8 BY 1.14 TOOLTIP "Add custom tool".

DEFINE BUTTON btnAddTool 
     LABEL "Add tools" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Move up" 
     SIZE 4.6 BY 1.14 TOOLTIP "Delete selected tools".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "gif/movedown.gif":U
     LABEL "Move down" 
     SIZE 4.6 BY 1.14 TOOLTIP "When multiple menus / sub-menus filter tools on menu to enable movement".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "gif/moveup.gif":U
     LABEL "Move up" 
     SIZE 4.6 BY 1.14 TOOLTIP "When multiple menus / sub-menus filter tools on menu to enable movement".

DEFINE BUTTON btnOk 
     LABEL "Ok" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSelectCode 
     LABEL "Select code template" 
     SIZE 24 BY 1.14.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U
     LABEL "" 
     SIZE 155.8 BY .48.

DEFINE BUTTON btnToolGroup 
     LABEL "Add tools from group(s)" 
     SIZE 24 BY 1.14 TOOLTIP "Add custom tool".

DEFINE VARIABLE cmbApplyToFm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Assign to fieldmap" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE cmbApplyToQry AS CHARACTER FORMAT "X(256)":U 
     LABEL "Assign to query/browse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE cmbSubMenu AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filter sub-menu" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 146.6 BY 2.86 NO-UNDO.

DEFINE VARIABLE edTip AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 146.6 BY 1.91 NO-UNDO.

DEFINE VARIABLE fiCodeLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Code:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fiTipLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Tip:" 
      VIEW-AS TEXT 
     SIZE 4 BY .62 NO-UNDO.

DEFINE VARIABLE fiToolbarName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Meny name" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE tbCreateProcs AS LOGICAL INITIAL yes 
     LABEL "Create methods for non-built-in tools" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.2 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwTool FOR 
      ttJBoxTool SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwTool
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwTool C-Win _FREEFORM
  QUERY brwTool DISPLAY
      iSeq    WIDTH 5
/* cTool   WIDTH 20  */
cViewTool WIDTH 25
cLabel  WIDTH 20 
/*
cImage  WIDTH 20 
fWidth  LABEL "Width" WIDTH 8
cViewAs FORMAT "x(20)":U WIDTH 23 VIEW-AS COMBO-BOX INNER-LINES 5
                       LIST-ITEMS "Button and Menu-item",
                                  "Menu-item",
                                  "Toggle Menu-item",
                                  "Button"
                       DROP-DOWN-LIST
*/

cParentMenu FORMAT "x(16)":U WIDTH 20 VIEW-AS COMBO-BOX INNER-LINES 25
                       LIST-ITEMS "Root menu"
                       DROP-DOWN-LIST
bAlwaysEnable VIEW-AS TOGGLE-BOX   
bToggle VIEW-AS TOGGLE-BOX   
/* cToolTip WIDTH 25  */
bBuiltIn  VIEW-AS TOGGLE-BOX

/*  cType */

ENABLE cLabel /* cTooltip fWidth cViewAs */ cParentMenu 
       bAlwaysEnable HELP "Check to enable menu-item regardless of record availability"
       bToggle HELP "Check to make a toggle menu-item"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 155.2 BY 13.33 ROW-HEIGHT-CHARS .8 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnAddTool AT ROW 1.24 COL 2 WIDGET-ID 70
     btnToolGroup AT ROW 1.24 COL 15 WIDGET-ID 72
     btnAddCustomTool AT ROW 1.24 COL 39 WIDGET-ID 12
     btnAddRule AT ROW 1.24 COL 57 WIDGET-ID 112
     btnAddSubMenu AT ROW 1.24 COL 68 WIDGET-ID 76
     btnDelete AT ROW 1.24 COL 119 WIDGET-ID 74
     btnMoveUp AT ROW 1.24 COL 123.8 WIDGET-ID 66
     btnMoveDown AT ROW 1.24 COL 128.4 WIDGET-ID 68
     btnSelectCode AT ROW 1.24 COL 133.2 WIDGET-ID 54
     cmbSubMenu AT ROW 1.33 COL 98.8 COLON-ALIGNED WIDGET-ID 110
     brwTool AT ROW 2.67 COL 1.8 WIDGET-ID 200
     btnSplitBarY AT ROW 16 COL 1.4 WIDGET-ID 42
     edCode AT ROW 16.57 COL 10.2 NO-LABEL WIDGET-ID 36
     edTip AT ROW 19.57 COL 10.2 NO-LABEL WIDGET-ID 28
     fiToolbarName AT ROW 21.71 COL 114 COLON-ALIGNED
     cmbApplyToQry AT ROW 22.81 COL 114 COLON-ALIGNED WIDGET-ID 4
     cmbApplyToFm AT ROW 23.91 COL 114 COLON-ALIGNED WIDGET-ID 10
     btnOk AT ROW 25.52 COL 126 WIDGET-ID 6
     btnCancel AT ROW 25.52 COL 142 WIDGET-ID 8
     tbCreateProcs AT ROW 25.76 COL 88 WIDGET-ID 32
     fiCodeLabel AT ROW 16.62 COL 1.8 COLON-ALIGNED NO-LABEL
     fiTipLabel AT ROW 19.67 COL 3.8 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157.2 BY 25.95 WIDGET-ID 100.


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
         TITLE              = "Select tools"
         HEIGHT             = 25.95
         WIDTH              = 157.2
         MAX-HEIGHT         = 25.95
         MAX-WIDTH          = 178.6
         VIRTUAL-HEIGHT     = 25.95
         VIRTUAL-WIDTH      = 178.6
         MIN-BUTTON         = no
         ALWAYS-ON-TOP      = yes
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brwTool cmbSubMenu DEFAULT-FRAME */
ASSIGN 
       brwTool:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwTool
/* Query rebuild information for BROWSE brwTool
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttJBoxTool.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brwTool */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Select tools */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Select tools */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwTool
&Scoped-define SELF-NAME brwTool
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwTool C-Win
ON VALUE-CHANGED OF brwTool IN FRAME DEFAULT-FRAME
DO:
  IF AVAIL ttJboxTool THEN DO:
    ASSIGN edTip:SCREEN-VALUE  = ttJBoxTool.cTip
           edCode:SCREEN-VALUE = ttJBoxTool.cCode
           .


    setMenuCombos(ttJBoxTool.cTool).

    IF hParentMenuCol:VISIBLE THEN
      hParentMenuCol:SCREEN-VALUE = ttJBoxTool.cParentMenu.

    ASSIGN hLabelCol:READ-ONLY        = ttJBoxTool.cType NE "tool"
/*            hTooltipCol:READ-ONLY      = ttJBoxTool.cType NE "tool" */
/*            hWidthCol:READ-ONLY        = ttJBoxTool.cType NE "tool" */
           hAlwaysEnableCol:READ-ONLY = ttJBoxTool.cType NE "tool"
/*            hViewAsCol:READ-ONLY       = ttJBoxTool.cType NE "tool" */
           hToggleCol:READ-ONLY       = ttJBoxTool.cViewAs = "menu-item"
           .

  END.
  ELSE
    ASSIGN edTip:SCREEN-VALUE = ""
           edCode:SCREEN-VALUE = ""
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddCustomTool
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddCustomTool C-Win
ON CHOOSE OF btnAddCustomTool IN FRAME DEFAULT-FRAME /* Add custom tool */
DO:
  DEF VAR cTool        AS CHAR NO-UNDO.
  DEF VAR cLabel       AS CHAR NO-UNDO.
  DEF VAR cImage       AS CHAR NO-UNDO.
  DEF VAR cTooltip     AS CHAR NO-UNDO.
  DEF VAR iSeq         AS INT  NO-UNDO.
  DEF VAR cSelSeq      AS CHAR NO-UNDO.
  DEF VAR cSelParent   AS CHAR NO-UNDO.

  DEF BUFFER ttJBoxTool FOR ttJBoxTool.

  IF hBrowse:NUM-SELECTED-ROWS = 1 AND hBrowse:FETCH-SELECTED-ROW(1) THEN
    ASSIGN cSelSeq = STRING(httJBoxTool::iSeq)
           cSelParent = httJBoxTool::cParentMenu.

  RUN jukebox\newTool.w (getMenuList(NO),
                         cSelSeq,
                         cSelParent,
                         OUTPUT cTool,
                         OUTPUT cLabel,
                         OUTPUT cImage,
                         OUTPUT cTooltip,
                         OUTPUT cAddToParentMenu,
                         OUTPUT iInsertAfter).
  IF cTool NE "" THEN DO:
    FIND FIRST ttJBoxTool 
         WHERE ttJBoxTool.cTool = cTool
           AND NOT ttJBoxTool.bDeleted
           AND ttJBoxTool.bSelect
         NO-ERROR.
    IF AVAIL ttJBoxTool THEN DO:
      MESSAGE "Tool" cTool "already exists"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.  
      RETURN NO-APPLY.
    END.
    ELSE DO:
      FIND FIRST ttJBoxTool 
           WHERE ttJBoxTool.cTool = cTool
             AND ttJBoxTool.bDeleted
           NO-ERROR.
      IF AVAIL ttJBoxTool THEN
        ASSIGN ttJBoxTool.bDeleted = NO
               ttJBoxTool.bSelect  = YES
               rReposRow = ROWID(ttJBoxTool).
      ELSE DO:
        FOR EACH ttJBoxTool WHERE ttJBoxTool.bSelect
            BY ttJBoxTool.iSeq DESC:
          iSeq = ttJboxTool.iSeq.
          LEAVE.
        END.
        IF makeRoomForInsert() THEN
          iSeq = iInsertAfter.

        CREATE ttJBoxTool.
        ASSIGN ttJBoxTool.iSeq        = iSeq + 10
               ttJBoxTool.cGroups     = "Custom"
               ttJBoxTool.cTool       = cTool
               ttJBoxTool.cLabel      = cLabel
               ttJBoxTool.cImage      = cImage
               ttJBoxTool.cTooltip    = cTooltip            
               ttJBoxTool.bCreProc    = tbCreateProcs:CHECKED
               ttJBoxTool.bSelect     = YES
               ttJBoxTool.cType       = "tool"
               ttJBoxTool.cViewAs     = "Button and Menu-item"
               ttJBoxTool.cParentMenu = cAddToParentMenu
               .
        IF cImage = "" THEN
          ttJBoxTool.fWidth = LENGTH(cLabel) + 1.
        FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "custom"
             NO-ERROR.
        IF NOT AVAIL ttToolGroup THEN DO:
          CREATE ttToolGroup.
          ASSIGN idxGroup = idxGroup + 1
                 ttToolGroup.iGroupId = idxGroup
                 ttToolGroup.cGroupName = "Custom tools"
                 ttToolGroup.cGroup = "custom"
                 .
        END.
        CREATE ttToolsForGroup.
        ASSIGN idxToolSeq = idxToolSeq + 1
               ttToolsForGroup.cTool = ttJBoxTool.cTool
               ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
               ttToolsForGroup.iSeq = idxToolSeq
               .
        rReposRow = ROWID(ttJBoxTool).
        RELEASE ttJBoxTool.
      END.
    END.
    RUN OpenQuery.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddRule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddRule C-Win
ON CHOOSE OF btnAddRule IN FRAME DEFAULT-FRAME /* Add rule */
DO:
  DEF VAR cParent      AS CHAR NO-UNDO.
  DEF VAR iSeq         AS INT  NO-UNDO.
  DEF VAR bOk          AS LOG  NO-UNDO.

  DEF BUFFER ttJBoxTool FOR ttJBoxTool.

  RUN jukebox\newMenuRule.w (getMenuList(NO),
                            OUTPUT bOk,
                            OUTPUT cParent,
                            OUTPUT iInsertAfter).
  IF bOk THEN DO:
    iSeq = 10.
    FOR EACH ttJBoxTool 
        WHERE ttJBoxTool.bSelect
          AND NOT ttJBoxTool.bDeleted
          AND ttJBoxTool.cType NE "menu"
        BY ttJBoxTool.iSeq DESC:
      iSeq = ttJboxTool.iSeq.
      LEAVE.
    END.

    IF makeRoomForInsert() THEN
      iSeq = iInsertAfter.

    CREATE ttJBoxTool.

    ASSIGN ttJBoxTool.iSeq        = iSeq + 10
           ttJBoxTool.cGroups     = "Rule"
           ttJBoxTool.cTool       = GUID
           ttJBoxTool.bSelect     = YES
           ttJBoxTool.cType       = "rule" 
           ttJBoxTool.cViewAs     = "Menu-item"
           ttJBoxTool.cParentMenu = cParent
           rReposRow   = ROWID(ttJBoxTool)
           .
    
    RELEASE ttJBoxTool.

    RUN OpenQuery.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddSubMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddSubMenu C-Win
ON CHOOSE OF btnAddSubMenu IN FRAME DEFAULT-FRAME /* Add sub-menu */
DO:
  DEF VAR cParent      AS CHAR NO-UNDO.
  DEF VAR cTool        AS CHAR NO-UNDO.
  DEF VAR iSeq         AS INT  NO-UNDO.
  DEF VAR cSelSeq      AS CHAR NO-UNDO.
  DEF VAR cSelParent   AS CHAR NO-UNDO.

  DEF BUFFER ttJBoxTool FOR ttJBoxTool.

  IF hBrowse:NUM-SELECTED-ROWS = 1 AND hBrowse:FETCH-SELECTED-ROW(1) THEN
    ASSIGN cSelSeq = STRING(httJBoxTool::iSeq)
           cSelParent = httJBoxTool::cParentMenu.

  RUN jukebox\newSubmenu.w (getMenuList(NO),
                            cSelSeq,
                            cSelParent,
                            OUTPUT cTool,
                            OUTPUT cParent,
                            OUTPUT iInsertAfter).
  IF cTool NE "" THEN DO:
    FIND FIRST ttJBoxTool 
         WHERE ttJBoxTool.cTool = cTool
           AND CAN-DO("menu,sub-menu",ttJBoxTool.cType)
           AND NOT ttJBoxTool.bDeleted 
         NO-ERROR.
    IF AVAIL ttJBoxTool THEN DO:
      MESSAGE "Menu" cTool "already exists"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.  
      RETURN NO-APPLY.
    END.
    iSeq = 10.
    FOR EACH ttJBoxTool 
        WHERE ttJBoxTool.bSelect
          AND NOT ttJBoxTool.bDeleted
          AND ttJBoxTool.cType NE "menu"
        BY ttJBoxTool.iSeq DESC:
      iSeq = ttJboxTool.iSeq.
      LEAVE.
    END.

    IF makeRoomForInsert() THEN
      iSeq = iInsertAfter.

    FIND FIRST ttJBoxTool 
         WHERE ttJBoxTool.cTool = cTool
           AND ttJBoxTool.cType = "sub-menu"
           AND ttJBoxTool.bDeleted 
         NO-ERROR.
    IF AVAIL ttJBoxTool THEN ttJBoxTool.bDeleted = NO.
    ELSE CREATE ttJBoxTool.

    IF cParent = ? THEN cParent = "".
    ASSIGN ttJBoxTool.iSeq        = iSeq + 10
           ttJBoxTool.cGroups     = "Menus"
           ttJBoxTool.cTool       = cTool
           ttJBoxTool.bSelect     = YES
           ttJBoxTool.cType       = "sub-menu" 
           ttJBoxTool.cViewAs     = ""
           ttJBoxTool.cParentMenu = cParent
           rReposRow   = ROWID(ttJBoxTool)
           .
    
    RELEASE ttJBoxTool.

    setMenuCombos("").

    RUN OpenQuery.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddTool
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddTool C-Win
ON CHOOSE OF btnAddTool IN FRAME DEFAULT-FRAME /* Add tools */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.
  DEF VAR bOk         AS LOG  NO-UNDO.

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "temp-table"    
                      + ";iSeq|INTEGER|>>>9||Seq"  
                      + ";cTool|CHARACTER|x(30)||Tool"
                      + ";cLabel|CHARACTER|x(30)||Label"
                      + ";cImage|CHARACTER|x(30)||Image"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "cTool",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).

  IF bOk THEN DO:

    FOR EACH ttJBoxTool 
        WHERE NOT CAN-DO("menu,sub-menu",ttJBoxTool.cType):
      IF ttJBoxTool.bSelect AND LOOKUP(ttJBoxTool.cTool,cIdList,"|") = 0 THEN
        ASSIGN ttJBoxTool.bDeleted = YES
               ttJBoxTool.bSelect  = NO.
    END.
    makeRoomForInsert().
    DO ix = 1 TO NUM-ENTRIES(cIdList,"|"):
      FIND FIRST ttJBoxTool WHERE ttJBoxTool.cTool = ENTRY(ix,cIdList,"|")
           NO-ERROR.
      IF AVAIL ttJBoxTool THEN DO:
        IF NOT ttJBoxTool.bSelect AND cAddToParentMenu NE "" THEN
          ttJBoxTool.cParentMenu = cAddToParentMenu.

        ASSIGN ttJBoxTool.iSeq = iInsertAfter + ix * 10
               ttJBoxTool.bSelect = YES
               ttJBoxTool.bDeleted = NO.
      END.
    END.
    RUN OpenQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Move up */
DO:
  DO ix = 1 TO BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS:
    IF BROWSE {&BROWSE-NAME}:FETCH-SELECTED-ROW(ix) THEN 
      ASSIGN ttJboxTool.bSelect = NO
             ttJBoxTool.bDeleted = YES.
  END.

  setMenuCombos("").

  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown C-Win
ON CHOOSE OF btnMoveDown IN FRAME DEFAULT-FRAME /* Move down */
DO:
  hBrwMove = BROWSE {&BROWSE-NAME}:HANDLE.
  RUN MoveDownRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp C-Win
ON CHOOSE OF btnMoveUp IN FRAME DEFAULT-FRAME /* Move up */
DO:
  hBrwMove = BROWSE {&BROWSE-NAME}:HANDLE.
  RUN MoveUpRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  IF NOT CAN-FIND(FIRST ttJBoxTool WHERE ttJBoxTool.bSelect) THEN DO:
    MESSAGE "No selected tools"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.

  reSequenceMenu().

  IF NOT fiToolbarName:READ-ONLY THEN DO:
    IF fiToolbarName:SCREEN-VALUE = "" OR fiToolbarName:SCREEN-VALUE = ? THEN DO:
      MESSAGE "Menu must have a name"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
    iocToolbarName = fiToolbarName:SCREEN-VALUE.      
  END.

  ASSIGN
         iocToolbarDef  = iocToolbarDef
         ocQryObject = cmbApplyToQry:SCREEN-VALUE
         ocFmObject  = cmbApplyToFm:SCREEN-VALUE
         obOk        = YES.


  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectCode C-Win
ON CHOOSE OF btnSelectCode IN FRAME DEFAULT-FRAME /* Select code template */
DO:
  DEF VAR bOk AS LOG NO-UNDO.
  DEF VAR cFileName AS CHAR NO-UNDO.
  DEF VAR cTemplateDir AS CHAR NO-UNDO.


  IF NOT AVAIL ttJBoxTool THEN RETURN NO-APPLY.

  cTemplateDir = SEARCH("dictview.w").
  IF cTemplateDir NE ? THEN DO:
    FILE-INFO:FILE-NAME = SEARCH("dictview.w").
    cTemplateDir = FILE-INFO:FULL-PATHNAME.
    ENTRY(NUM-ENTRIES(cTemplateDir,"\"),cTemplateDir,"\") = "template".
  END.

  IF ttJBoxTool.cGroups NE "custom" THEN 
    SYSTEM-DIALOG GET-FILE cFileName 
                  FILTERS "Tempates for " + ttJBoxTool.cTool ttJBoxTool.cTool + "*.p",
                          "All templates" "*.p"
                  MUST-EXIST
                  INITIAL-DIR cTemplateDir
                  UPDATE bOk.
  ELSE
    SYSTEM-DIALOG GET-FILE cFileName 
                  FILTERS "Tempates files" "*.p"
                  MUST-EXIST
                  INITIAL-DIR cTemplateDir
                  UPDATE bOk.

  IF cFileName NE "" THEN DO:
    ttJBoxTool.cCode = ImportConfig(cFileName).
    APPLY "value-changed" TO BROWSE brwTool.
    BROWSE brwTool:REFRESH().
    BROWSE brwTool:SELECT-FOCUSED-ROW().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
/*
  IF SELF:Y < 120 THEN SELF:Y = 120.
  IF SELF:Y > 350 THEN SELF:Y = 350.
  BROWSE brwTool:HEIGHT-PIXELS = SELF:Y - BROWSE brwTool:Y.
  fiCodeLabel:Y = SELF:Y + 13.
  edCode:HEIGHT-PIXELS = 390 - SELF:Y - 15 NO-ERROR.
  edCode:Y = SELF:Y + 13 NO-ERROR.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToolGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToolGroup C-Win
ON CHOOSE OF btnToolGroup IN FRAME DEFAULT-FRAME /* Add tools from group(s) */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR ix          AS INT  NO-UNDO.
  DEF VAR iy          AS INT  NO-UNDO.
  DEF VAR iSeq        AS INT  NO-UNDO.    
  DEF VAR cToolList   AS CHAR NO-UNDO.
  DEF VAR cTool       AS CHAR NO-UNDO.
  DEF VAR cLabel      AS CHAR NO-UNDO.

  DEF BUFFER ttJBoxTool FOR ttJBoxTool.

  bSelectToolGroup = YES.

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "temp-table"    
                      + ";iSeq|INTEGER|>>>9||Seq"  
                      + ";cTool|CHARACTER|x(20)||Group"
                      + ";cGroupName|CHARACTER|x(30)||Group name"
                      + ";cToolList|CHARACTER|x(256)||Tool-list"
                      + ";!iGroupId|INTEGER|>>9"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "cToolList",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).

  bSelectToolGroup = NO.

  IF bOk THEN DO:
    FOR EACH ttJBoxTool 
        WHERE ttJBoxTool.bSelect
        BY ttJBoxTool.iSeq DESC:
      iSeq = ttJBoxTool.iSeq.
      LEAVE.
    END.
    iSeq = iSeq + 10.

    IF makeRoomForInsert() THEN 
      iSeq = iInsertAfter + 10.

    DO ix = 1 TO NUM-ENTRIES(cIdList,"|"):
      cToolList = ENTRY(ix,cIdList,"|").
      DO iy = 1 TO NUM-ENTRIES(cToolList):
        FIND FIRST ttJBoxTool WHERE ttJBoxTool.cTool = ENTRY(1,ENTRY(iy,cToolList),";")
             NO-ERROR.
        IF AVAIL ttJBoxTool AND NOT ttJBoxTool.bSelect THEN DO:
          IF cAddToParentMenu NE "" THEN
             ttJBoxTool.cParentMenu = cAddToParentMenu.
          ASSIGN ttJBoxTool.bSelect = YES
                 ttJBoxTool.bDeleted = NO
                 ttJBoxTool.iSeq = iSeq
                 iSeq = iSeq + 10.
        END.
        ELSE IF NOT AVAIL ttJBoxTool THEN DO: /* new custom tool */
          cTool = ENTRY(1,ENTRY(iy,cToolList),";").
          IF NUM-ENTRIES(ENTRY(iy,cToolList),";") > 1 THEN
            cLabel = ENTRY(2,ENTRY(iy,cToolList),";").
          ELSE cLabel = cTool.

          RUN adecomm/_valpnam.p (cTool,NO,"_INTERNAL",OUTPUT bOk).

          IF bOk THEN DO:
            CREATE ttJBoxTool.
            ASSIGN ttJBoxTool.iSeq     = iSeq
                   ttJBoxTool.cGroups  = "Custom"
                   ttJBoxTool.cTool    = cTool
                   ttJBoxTool.cLabel   = cLabel
                   ttJBoxTool.bCreProc = tbCreateProcs:CHECKED
                   ttJBoxTool.bSelect  = YES
                   iSeq = iSeq + 10
                   .
            FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "custom"
                 NO-ERROR.
            IF NOT AVAIL ttToolGroup THEN DO:
              CREATE ttToolGroup.
              ASSIGN idxGroup = idxGroup + 1
                     ttToolGroup.iGroupId = idxGroup
                     ttToolGroup.cGroupName = "Custom tools"
                     ttToolGroup.cGroup = "custom"
                     .
            END.
            CREATE ttToolsForGroup.
            ASSIGN idxToolSeq = idxToolSeq + 1
                   ttToolsForGroup.cTool = ttJBoxTool.cTool
                   ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
                   ttToolsForGroup.iSeq = idxToolSeq
                   .
          END.
        END.
      END.
    END.

    RUN OpenQuery.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbSubMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSubMenu C-Win
ON VALUE-CHANGED OF cmbSubMenu IN FRAME DEFAULT-FRAME /* Filter sub-menu */
DO:
  FOR EACH ttJBoxTool 
      WHERE ttJBoxTool.hMenuFilter NE ?
      :

    IF ttJBoxTool.hMenuFilter:PRIVATE-DATA = "checked" THEN DO:
      ttJBoxTool.hMenuFilter:LOAD-IMAGE("gif\filter.gif").
      ttJBoxTool.hMenuFilter:PRIVATE-DATA = "".
    END.
  END.


  cMenuParentFilter = SELF:SCREEN-VALUE.
  IF cMenuParentFilter = ? THEN cMenuParentFilter = "".
/*   IF cMenuParentFilter = "" THEN */
/*     SELF:BGCOLOR = 12.           */
/*   ELSE                           */
/*     SELF:BGCOLOR = ?.            */
  RUN OpenQuery.
  SELF:SCREEN-VALUE = cMenuParentFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edCode C-Win
ON LEAVE OF edCode IN FRAME DEFAULT-FRAME
DO:
  IF SELF:MODIFIED AND AVAIL ttJBoxTool THEN ttJBoxTool.cCode = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCreateProcs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCreateProcs C-Win
ON VALUE-CHANGED OF tbCreateProcs IN FRAME DEFAULT-FRAME /* Create methods for non-built-in tools */
DO:
  FOR EACH ttJBoxTool
      WHERE NOT ttJBoxTool.bBuiltIn:
    ttJBoxTool.bCreProc = SELF:CHECKED.
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
ON CLOSE OF THIS-PROCEDURE DO:
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl\wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ASSIGN hBrowse = BROWSE brwTool:HANDLE
         hLabelCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cLabel")
         hParentMenuCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cParentMenu")
         hTooltipCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cTooltip")
         hWidthCol   = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"fWidth")
         hAlwaysEnableCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"bAlwaysEnable")
         hViewAsCol  = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cViewAs")
         hToggleCol  = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"bToggle") 
         .

  RUN InitWindow.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY cmbSubMenu edCode edTip fiToolbarName cmbApplyToQry cmbApplyToFm 
          tbCreateProcs fiCodeLabel fiTipLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnAddTool btnToolGroup btnAddCustomTool btnAddRule btnAddSubMenu 
         btnDelete btnMoveUp btnMoveDown btnSelectCode cmbSubMenu brwTool 
         btnSplitBarY edCode edTip fiToolbarName cmbApplyToQry cmbApplyToFm 
         btnOk btnCancel tbCreateProcs fiCodeLabel fiTipLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN ToExcelViaFile.p (httJboxTool,-1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSelectorAttributes C-Win 
PROCEDURE getSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihBrwSource AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBrwTarget AS HANDLE NO-UNDO.
DEF INPUT  PARAM icDeSelRows AS CHAR   NO-UNDO.
DEF OUTPUT PARAM oiReturn    AS INT    NO-UNDO.

IF VALID-HANDLE(hSelectorPanel) THEN
  ASSIGN cAddToParentMenu  = DYNAMIC-FUNCTION("getAddToParentMenu" IN hSelectorPanel)
         iInsertAfter      = DYNAMIC-FUNCTION("getInsertAfter" IN hSelectorPanel).
          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR iy          AS INT  NO-UNDO.
DEF VAR cBrowseList AS CHAR NO-UNDO.
DEF VAR cQueryList  AS CHAR NO-UNDO.
DEF VAR cOtherList  AS CHAR NO-UNDO.
DEF VAR cVcrList    AS CHAR NO-UNDO.
DEF VAR iOtherIdx   AS INT  NO-UNDO INIT 200.
DEF VAR iVcrIdx     AS INT  NO-UNDO INIT 400.
DEF VAR hProc       AS HANDLE NO-UNDO.
DEF VAR hPrev       AS HANDLE NO-UNDO.
DEF VAR cAccel      AS CHAR NO-UNDO.
DEF VAR bGrouped    AS LOG  NO-UNDO.
DEF VAR bSelectBrw  AS LOG  NO-UNDO.
DEF VAR bSelectQry  AS LOG  NO-UNDO.
DEF VAR cFirstTblList   AS CHAR NO-UNDO.
DEF VAR cTbObjects      AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  EMPTY TEMP-TABLE ttJboxTool.

  ASSIGN cBrowseList = "new,edit,copy,undo,save,delete,refresh,filter,browseConfig,excel,multiSortBrowse,copyBrowseRows,rowsToBatch"
         cQueryList  = "new,edit,copy,undo,save,delete"
         cOtherList  = "flatview,print,note"
         cVcrList    = "first,prev,next,last"
         cAllBuiltIn = cBrowseList + "," + cOtherList + "," + cVcrList
/*          btnAddSubMenu:HIDDEN = YES */
         .

  cToolList = DYNAMIC-FUNCTION("getAttributeList",SESSION,"btnImg_","",NO).
  ASSIGN cImgList = ENTRY(2,cToolList,"|")
         cToolList = ENTRY(1,cToolList,"|")
         .

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
        ,"Refresh,MoveToMenu;Move to menu,Excel"
        ,""). 

  IF cToolList = "" THEN DO:
    hProc = SESSION:FIRST-PROCEDURE.
    REPEAT WHILE VALID-HANDLE(hProc):
      IF hProc:FILE-NAME BEGINS "JBox" OR hProc:FILE-NAME = "ResizeLib.p" THEN DO:
        hPrev = hProc:PREV-SIBLING.
        DELETE PROCEDURE hProc.
        hProc = hPrev.
      END.
      hProc = hProc:NEXT-SIBLING.
    END.
    RUN JBoxLoadLib.p ("ResizeLib.p,JBoxUIlib.p,JBoxASlib.p,JBoxFUlib.p").
    DYNAMIC-FUNCTION("setUseResizeLibWithJukebox",NO).
    DYNAMIC-FUNCTION("setSessionId","validsession").
    DYNAMIC-FUNCTION("setASUserId",IF USERID(LDBNAME(1)) NE "" THEN USERID(LDBNAME(1)) ELSE "testuser","<username>").  
    DYNAMIC-FUNCTION("setLanguageCode","EN").
    cToolList = DYNAMIC-FUNCTION("getAttributeList",SESSION,"btnImg_","",NO).
    ASSIGN cImgList = ENTRY(2,cToolList,"|")
           cToolList = ENTRY(1,cToolList,"|")
           .
  END.

  ASSIGN cmbApplyToQry:LIST-ITEMS = "," + icQryObjects
         cmbApplyToFm:LIST-ITEMS  = "," + icFmObjects
         cCtrlHotkeyActions       = DYNAMIC-FUNCTION("getAttribute",SESSION,"CtrlHotkeyActions")
         cCtrlHotkeys             = DYNAMIC-FUNCTION("getAttribute",SESSION,"CtrlHotkeys")
         cAltHotkeyActions        = DYNAMIC-FUNCTION("getAttribute",SESSION,"AltHotkeyActions")
         cAltHotkeys              = DYNAMIC-FUNCTION("getAttribute",SESSION,"AltHotkeys")
         cFunKeys                 = DYNAMIC-FUNCTION("getAttribute",SESSION,"FunctionKeys")
         cFunKeyActs              = DYNAMIC-FUNCTION("getAttribute",SESSION,"FunctionKeyActions")
         cFirstQry                = ENTRY(1,icQryObjects)
         cFirstFm                 = ENTRY(1,icFmObjects)
         cImgList                 = cImgList + CHR(1) + ""
         cToolList                = cToolList /* + ",BtnImg_multiSortBrowse,BtnImg_copyBrowseRows" */
         .

  IF NOT fillTbTemplates() AND iocToolbarName = "" THEN DO:
    IF icQryObjects BEGINS "oBrw" THEN
      bSelectBrw = YES.
    ELSE IF icQryObjects BEGINS "oQry" THEN
      bSelectQry = YES.
  END.

  DO ix = 1 TO NUM-ENTRIES(cToolList):
    FIND FIRST ttJBoxTool
         WHERE ttJBoxTool.cTool = SUBSTR(ENTRY(ix,cToolList),8)
         NO-ERROR.
    IF NOT AVAIL ttJBoxTool THEN DO:
      CREATE ttJBoxTool.
      ASSIGN ttJBoxTool.cTool    = SUBSTR(ENTRY(ix,cToolList),8)
             ttJBoxTool.cLabel   = DYNAMIC-FUNCTION("getAttribute",SESSION,"BtnLabel_" + ttJboxTool.cTool)
             ttJBoxTool.cImage   = ENTRY(ix,cImgList,CHR(1))
             ttJBoxTool.bBuiltIn = CAN-DO(cAllBuiltIn,ttJBoxTool.cTool)
             ttJBoxTool.bCreProc = NOT ttJBoxTool.bBuiltIn
             cAccel              = IF INDEX(ttJBoxTool.cLabel,"ctrl-") > 0 THEN
                                      "CTRL-" + CAPS(SUBSTR(ttJBoxTool.cLabel,INDEX(ttJBoxTool.cLabel,"ctrl-") + 5,1))
                                    ELSE IF INDEX(ttJBoxTool.cLabel,"alt-") > 0 THEN
                                      "ALT-" + CAPS(SUBSTR(ttJBoxTool.cLabel,INDEX(ttJBoxTool.cLabel,"alt-") + 4,1))
                                    ELSE IF INDEX(ttJBoxTool.cLabel,"&") > 0 AND R-INDEX(ttJBoxTool.cLabel,"&") NE LENGTH(ttJBoxTool.cLabel) THEN
                                      "ALT-" + CAPS(SUBSTR(ttJBoxTool.cLabel,INDEX(ttJBoxTool.cLabel,"&") + 1,1))
                                    ELSE IF INDEX(ttJBoxTool.cLabel,"&") > 0 THEN ""
                                    ELSE IF CAN-DO(cFunKeyActs,ttJBoxTool.cTool) AND NOT CAN-DO(cAltHotkeyActions,ttJBoxTool.cTool) AND NOT CAN-DO(cAltHotkeyActions,ttJBoxTool.cTool) THEN 
                                      ENTRY(LOOKUP(ttJBoxTool.cTool,cFunKeyActs),cFunKeys)
                                    ELSE IF CAN-DO(cCtrlHotkeyActions,ttJBoxTool.cTool) AND NOT CAN-DO(cAltHotkeyActions,ttJBoxTool.cTool) THEN 
                                      "CTRL-" + ENTRY(LOOKUP(ttJBoxTool.cTool,cCtrlHotkeyActions),cCtrlHotkeys) 
                                    ELSE IF CAN-DO(cAltHotkeyActions,ttJBoxTool.cTool) THEN 
                                      "ALT-" + ENTRY(LOOKUP(ttJBoxTool.cTool,cAltHotkeyActions),cAltHotkeys) 
                                    ELSE "ALT-" + CAPS(SUBSTR(ttJBoxTool.cLabel,1,1))
            ttJBoxTool.cLabel    = IF INDEX(ttJBoxTool.cLabel,"ctrl-") > 0 THEN 
                                      TRIM(SUBSTR(ttJBoxTool.cLabel,1,INDEX(ttJBoxTool.cLabel,"ctrl-") - 1))
                                    ELSE IF INDEX(ttJBoxTool.cLabel,"alt-") > 0 THEN 
                                      TRIM(SUBSTR(ttJBoxTool.cLabel,1,INDEX(ttJBoxTool.cLabel,"alt-") - 1))
                                    ELSE IF R-INDEX(ttJBoxTool.cLabel,"&") = LENGTH(ttJBoxTool.cLabel) THEN 
                                      REPLACE(ttJBoxTool.cLabel,"&","")
                                    ELSE ttJBoxTool.cLabel
            ttJBoxTool.cTooltip  = RIGHT-TRIM(ttJBoxTool.cLabel + " (" + cAccel," (") + (IF cAccel NE "" THEN ")" ELSE "")
            ttJBoxTool.cViewAs   = "Button and Menu-item"
            ttJBoxTool.cType     = "tool"
            .
    END.

    bGrouped = NO.
    IF CAN-DO(cBrowseList,ttJBoxTool.cTool) THEN DO:
      ASSIGN ttJBoxTool.cGroups = ttJBoxTool.cGroups + (IF ttJBoxTool.cGroups NE "" THEN "," ELSE "") + "Browse"
             ttJBoxTool.iSeq = LOOKUP(ttJBoxTool.cTool,cBrowseList) + 9
             ttJBoxTool.bSelect = bSelectBrw
             .

      FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "browse"
           NO-ERROR.
      IF NOT AVAIL ttToolGroup THEN DO:
        CREATE ttToolGroup.
        ASSIGN idxGroup = idxGroup + 1
               ttToolGroup.iGroupId = idxGroup
               ttToolGroup.cGroupName = "Browse/navigate (standard)"
               ttToolGroup.cGroup = "browse"
               .
      END.
      CREATE ttToolsForGroup.
      ASSIGN idxToolSeq = idxToolSeq + 1
             ttToolsForGroup.cTool = ttJBoxTool.cTool
             ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
             ttToolsForGroup.iSeq = idxToolSeq
             bGrouped = YES
             .
    END.

    IF CAN-DO(cQueryList,ttJBoxTool.cTool) THEN DO:
      ASSIGN ttJBoxTool.cGroups = ttJBoxTool.cGroups + ",Query"
             ttJBoxTool.bSelect = bSelectQry
             .

      FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "query"
           NO-ERROR.
      IF NOT AVAIL ttToolGroup THEN DO:
        CREATE ttToolGroup.
        ASSIGN idxGroup = idxGroup + 1
               ttToolGroup.iGroupId = idxGroup
               ttToolGroup.cGroupName = "Details (standard)"
               ttToolGroup.cGroup = "query"
               .
      END.
      CREATE ttToolsForGroup.
      ASSIGN idxToolSeq = idxToolSeq + 1
             ttToolsForGroup.cTool = ttJBoxTool.cTool
             ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
             ttToolsForGroup.iSeq = idxToolSeq
             bGrouped = YES
             .
    END.

    IF CAN-DO(cVcrList,ttJBoxTool.cTool) THEN DO:
      ASSIGN ttJBoxTool.cGroups = ttJBoxTool.cGroups + (IF ttJBoxTool.cGroups NE "" THEN "," ELSE "") + "Vcr"
             ttJBoxTool.iSeq = LOOKUP(ttJBoxTool.cTool,cVcrList) + iVcrIdx
             .
      FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "vcr"
           NO-ERROR.
      IF NOT AVAIL ttToolGroup THEN DO:
        CREATE ttToolGroup.
        ASSIGN idxGroup = idxGroup + 1
               ttToolGroup.iGroupId = idxGroup
               ttToolGroup.cGroupName = "VCR (standard)"
               ttToolGroup.cGroup = "vcr"
               .
      END.
      CREATE ttToolsForGroup.
      ASSIGN idxToolSeq = idxToolSeq + 1
             ttToolsForGroup.cTool = ttJBoxTool.cTool
             ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
             ttToolsForGroup.iSeq = idxToolSeq
             bGrouped = YES
             .
    END.

    IF CAN-DO(cOtherList,ttJBoxTool.cTool) THEN DO:
      ASSIGN ttJBoxTool.cGroups = ttJBoxTool.cGroups + (IF ttJBoxTool.cGroups NE "" THEN "," ELSE "") + "Vcr"
             ttJBoxTool.iSeq = LOOKUP(ttJBoxTool.cTool,cOtherList) + iOtherIdx
             .
      FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "built-in"
           NO-ERROR.
      IF NOT AVAIL ttToolGroup THEN DO:
        CREATE ttToolGroup.
        ASSIGN idxGroup = idxGroup + 1
               ttToolGroup.iGroupId = idxGroup
               ttToolGroup.cGroupName = "Built in (standard)"
               ttToolGroup.cGroup = "built-in"
               .
      END.
      CREATE ttToolsForGroup.
      ASSIGN idxToolSeq = idxToolSeq + 1
             ttToolsForGroup.cTool = ttJBoxTool.cTool
             ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
             ttToolsForGroup.iSeq = idxToolSeq
             bGrouped = YES
             .
    END.

    IF NOT bGrouped THEN DO:
      ASSIGN ttJBoxTool.cGroups = ttJBoxTool.cGroups + (IF ttJBoxTool.cGroups NE "" THEN "," ELSE "") + "Other"
             ttJBoxTool.iSeq = iOtherIdx
             iOtherIdx = iOtherIdx + 1
             .

      FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "other"
           NO-ERROR.
      IF NOT AVAIL ttToolGroup THEN DO:
        CREATE ttToolGroup.
        ASSIGN idxGroup = idxGroup + 1
               ttToolGroup.iGroupId = idxGroup
               ttToolGroup.cGroupName = "Other (standard buttons)"
               ttToolGroup.cGroup = "other"
               .
      END.
      CREATE ttToolsForGroup.
      ASSIGN idxToolSeq = idxToolSeq + 1
             ttToolsForGroup.cTool = ttJBoxTool.cTool
             ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
             ttToolsForGroup.iSeq = idxToolSeq
             bGrouped = YES
             .
    END.
  END.

  IF iocToolbarName NE "" THEN DO:
    cmbApplyToFm:SCREEN-VALUE = DYNAMIC-FUNCTION("getRelatedObject","o" + iocToolbarName,"toolbar","fieldMap") NO-ERROR.
    cmbApplyToQry:SCREEN-VALUE = DYNAMIC-FUNCTION("getRelatedObject","o" + iocToolbarName,"toolbar","browse,query") NO-ERROR.
    ASSIGN fiToolbarName:SCREEN-VALUE = iocToolbarName
           fiToolbarName:READ-ONLY = YES
           cmbApplyToFm:SENSITIVE = NO
           cmbApplyToQry:SENSITIVE = NO
           .

    LoadToolbarDef(iocToolbarDef).

  END.
  ELSE DO:
    ASSIGN cFirstTblList = DYNAMIC-FUNCTION("getFirstTableList")
           cTbObjects    = DYNAMIC-FUNCTION("getTbObjectsInUse")
           .
    DO ix = 1 TO MIN(NUM-ENTRIES(cFirstTblList),NUM-ENTRIES(cTbObjects)):
      IF NOT ENTRY(ix,cTbObjects) MATCHES "*" + ENTRY(ix,cFirstTblList) + "*" THEN DO:
        fiToolbarName:SCREEN-VALUE = "popup" + ENTRY(ix,cFirstTblList).
        DO iy = 1 TO NUM-ENTRIES(icQryObjects):
          IF ENTRY(iy,icQryObjects) MATCHES "*" + ENTRY(ix,cFirstTblList) THEN
            cmbApplyToQry:SCREEN-VALUE = ENTRY(iy,icQryObjects).
        END.
        DO iy = 1 TO NUM-ENTRIES(icFmObjects):
          IF ENTRY(iy,icFmObjects) MATCHES "*" + ENTRY(ix,cFirstTblList) THEN
            cmbApplyToFm:SCREEN-VALUE = ENTRY(iy,icFmObjects).
        END.
      END.
    END.
    IF fiToolbarName:SCREEN-VALUE = "" AND cFirstTblList NE "" THEN 
      ASSIGN fiToolbarName:SCREEN-VALUE = "popup" + ENTRY(1,cFirstTblList)
             cmbApplyToQry:SCREEN-VALUE = ENTRY(1,icQryObjects)
             cmbApplyToFm:SCREEN-VALUE = ENTRY(1,icFmObjects)
             .
  END.
  
/*   RUN toexcelviafile.p (httJboxTool,-1). */

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "brwTool,edTip").
  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "btnDelete,btnMoveUp,tbFlat,tbCreateProcs"). 

  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
                   STRING(brwTool:HANDLE) + "," + STRING(edCode:HANDLE) + "," + STRING(fiCodeLabel:HANDLE)). 

  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},120,180).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,620,370,0,0).
  SESSION:SET-WAIT-STATE("").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveDownRecord C-Win 
PROCEDURE MoveDownRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSelected  AS CHAR   NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR cToolsList AS CHAR   NO-UNDO.
DEF VAR hBuffMove  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR iMaxSeq    AS INT    NO-UNDO.
DEF VAR bMove      AS LOG    NO-UNDO INIT YES.

IF hBrwMove:NUM-SELECTED-ROWS = 0 THEN RETURN.


IF NOT okMove("Down") THEN RETURN.

hBuffMove = hBrwMove:QUERY:GET-BUFFER-HANDLE(1). 

DO ix = 1 TO hBrwMove:NUM-SELECTED-ROWS:
  IF hBrwMove:FETCH-SELECTED-ROW(ix) THEN
    ASSIGN cToolsList = cToolsList + (IF cToolsList NE "" THEN "," ELSE "") + hBuffMove::cTool
           iMaxSeq  = (IF hBuffMove::iSeq > iMaxSeq THEN hBuffMove::iSeq ELSE iMaxSeq)
           . 
END.

hBuffMove:FIND-FIRST("WHERE iSeq > " + STRING(iMaxSeq)) NO-ERROR.
IF NOT hBuffMove:AVAIL THEN RETURN.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffMove).

hQuery:QUERY-PREPARE("FOR EACH " + hBuffMove:NAME 
                   + (IF hBuffMove:NAME = "ttJBoxTool" THEN 
                       " WHERE bSelect" 
                     + (IF cMenuParentFilter NE "" THEN " AND cParentMenu = '" + cMenuParentFilter + "'" ELSE "")
                      ELSE "")
                   + " BY iSeq").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF hBuffMove::iSeq > iMaxSeq AND bMove THEN 
    ASSIGN hBuffMove::iSeq = hBuffMove::iSeq - 10 * NUM-ENTRIES(cToolsList)
           bMove = NO.
  ELSE IF CAN-DO(cToolsList,hBuffMove::cTool) THEN
    hBuffMove::iSeq = hBuffMove::iSeq + 10.
  hQuery:GET-NEXT().
END.

/*
hQuery:QUERY-PREPARE("FOR EACH " + hBuffMove:NAME 
                   + " WHERE iSeq > " + STRING(iMaxSeq)
                   + " AND iSeq LE " + STRING(iMaxSeq + 10 * NUM-ENTRIES(cToolsList))
                   + (IF hBuffMove:NAME = "ttJBoxTool " THEN " AND bSelect" ELSE "")
                   + " BY iSeq").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  hBuffMove::iSeq = hBuffMove::iSeq - 10 * NUM-ENTRIES(cToolsList).
  hQuery:GET-NEXT().
END.

hQuery:QUERY-PREPARE("FOR EACH " + hBuffMove:NAME 
                   + " WHERE CAN-DO('" + cToolsList + "',cTool)").

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  hBuffMove::iSeq = hBuffMove::iSeq + 10 * NUM-ENTRIES(cToolsList).
  hQuery:GET-NEXT().
END.
*/

IF VALID-HANDLE(hBrwMove) AND hBrwMove NE BROWSE brwTool:HANDLE THEN 
  RUN InvokeMethod(hBrwMove,"OpenQuery").
ELSE  
  RUN OpenQuery.

hBrwMove:DESELECT-ROWS().

DO ix = 1 TO NUM-ENTRIES(cToolsList):
  hBuffMove:FIND-FIRST("WHERE cTool = '" + ENTRY(ix,cToolsList) + "'") NO-ERROR.
  IF hBuffMove:AVAIL THEN DO:
    hBrwMove:SET-REPOSITIONED-ROW(hBrwMove:DOWN,"conditional").  
    hBrwMove:QUERY:REPOSITION-TO-ROWID(hBuffMove:ROWID) NO-ERROR.
    hBrwMove:SELECT-ROW(hBrwMove:FOCUSED-ROW) NO-ERROR. 
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToMenuRecord C-Win 
PROCEDURE MoveToMenuRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn     AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.

RUN JBoxDSimpleSelectList.w (getMenuList(YES),?,OUTPUT cReturn).

IF cReturn NE ? AND cReturn NE "" THEN DO WITH FRAME {&FRAME-NAME}: 
  DO ix = 1 TO brwTool:NUM-SELECTED-ROWS:
    IF brwTool:FETCH-SELECTED-ROW(ix) THEN DO:
      ttJBoxTool.cParentMenu = cReturn.
      IF rReposRow = ? THEN rReposRow = ROWID(ttJBoxTool).
    END.
  END.
  RUN OpenQuery.
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveUpRecord C-Win 
PROCEDURE MoveUpRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSelected  AS CHAR   NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR cToolsList AS CHAR   NO-UNDO.
DEF VAR hBuffMove  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR iMinSeq    AS INT    NO-UNDO INIT 100000.
DEF VAR bMove      AS LOG    NO-UNDO INIT YES.

IF hBrwMove:NUM-SELECTED-ROWS = 0 THEN RETURN.

hBuffMove = hBrwMove:QUERY:GET-BUFFER-HANDLE(1). 

IF NOT okMove("Up") THEN RETURN.

DO ix = 1 TO hBrwMove:NUM-SELECTED-ROWS:
  IF hBrwMove:FETCH-SELECTED-ROW(ix) THEN
    ASSIGN cToolsList = cToolsList + (IF cToolsList NE "" THEN "," ELSE "") + hBuffMove::cTool
           iMinSeq  = (IF hBuffMove::iSeq < iMinSeq THEN hBuffMove::iSeq ELSE iMinSeq)
           . 
END.

hBuffMove:FIND-FIRST("WHERE iSeq < " + STRING(iMinSeq)) NO-ERROR.
IF NOT hBuffMove:AVAIL THEN RETURN.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffMove).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffMove:NAME 
                   + (IF hBuffMove:NAME = "ttJBoxTool" THEN " WHERE bSelect" 
                     + (IF cMenuParentFilter NE "" THEN " AND cParentMenu = '" + cMenuParentFilter + "'" ELSE "")
                      ELSE "")
                   + " BY iSeq DESC").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF hBuffMove::iSeq < iMinSeq THEN DO:
    IF bMove THEN
      ASSIGN hBuffMove::iSeq = hBuffMove::iSeq + 10 * NUM-ENTRIES(cToolsList)
             bMove = NO.
  END.
  ELSE IF CAN-DO(cToolsList,hBuffMove::cTool) THEN
    hBuffMove::iSeq = hBuffMove::iSeq - 10.
  ELSE hBuffMove::iSeq = hBuffMove::iSeq + 1000.

  hQuery:GET-NEXT().
END.

IF VALID-HANDLE(hBrwMove) AND hBrwMove NE BROWSE brwTool:HANDLE THEN 
  RUN InvokeMethod(hBrwMove,"OpenQuery").
ELSE  
  RUN OpenQuery.
 
hBrwMove:DESELECT-ROWS().

DO ix = 1 TO NUM-ENTRIES(cToolsList):
  hBuffMove:FIND-FIRST("WHERE cTool = '" + ENTRY(ix,cToolsList) + "'") NO-ERROR.
  IF hBuffMove:AVAIL THEN DO:
    hBrwMove:SET-REPOSITIONED-ROW(1,"conditional").  
    hBrwMove:QUERY:REPOSITION-TO-ROWID(hBuffMove:ROWID) NO-ERROR.
    hBrwMove:SELECT-ROW(hBrwMove:FOCUSED-ROW) NO-ERROR. 
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG NO-UNDO.

ihBuffer::cToolList = ihFillIn:SCREEN-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  IF cMenuParentFilter = "" THEN
    reSequenceMenu().

  hBrowse:QUERY:QUERY-PREPARE("FOR EACH ttJBoxTool WHERE ttJBoxTool.bSelect AND cType NE 'menu' " 
                             + (IF cMenuParentFilter NE "" THEN " AND cParentMenu = '" + cMenuParentFilter + "'" ELSE "")
                             + " BY ttJBoxTool.iSeq").

  hBrowse:QUERY:QUERY-OPEN().

  IF cMenuParentFilter NE "" THEN DO:
    hBrowse:QUERY:GET-FIRST().
    REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
      ix = ix + 10.
      ttJBoxTool.iSeq = ix.
      hBrowse:QUERY:GET-NEXT().
    END.
  END.

  
  IF hBrowse:QUERY:NUM-RESULTS > 0 THEN DO:
    hBrowse:REFRESH().

    IF rReposRow NE ? THEN DO:
      hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN - 2,"always").  
      hBrowse:QUERY:REPOSITION-TO-ROWID(rReposRow) NO-ERROR.
    END.

    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW) NO-ERROR.
  END.

  btnMoveUp:SENSITIVE = NOT (cMenuParentFilter = "" AND
                            (NOT cmbSubMenu:HIDDEN OR CAN-FIND(FIRST bttJBoxTool 
                                        WHERE bttJBoxTool.cType = "menu"
                                          AND bttJBoxTool.hMenuFillin NE ?))).
  btnMoveDown:SENSITIVE = btnMoveUp:SENSITIVE.

  APPLY "value-changed" TO hBrowse.
  
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW) NO-ERROR.

  rReposRow = ?.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectCodeTemplate C-Win 
PROCEDURE SelectCodeTemplate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  DEF VAR cTemplateFile AS CHAR NO-UNDO.
  DEF VAR cText         AS CHAR NO-UNDO.

  cTemplateFile = SELF:SCREEN-VALUE + "LookupTemplate.p".
  IF SEARCH(cTemplateFile) NE ? THEN DO:
    INPUT FROM VALUE(SEARCH(cTemplateFile)).
    REPEAT:
      IMPORT UNFORMATTED cText.
      edCode:SCREEN-VALUE = edCode:SCREEN-VALUE + CHR(10) + cText.
    END.
    INPUT CLOSE.
  END.
*/    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.
DEF VAR hTbTarget      AS HANDLE NO-UNDO.
DEF VAR iSeq           AS INT    NO-UNDO INIT 10.
DEF VAR hSelector      AS HANDLE NO-UNDO.
DEF VAR hSelectorWin   AS HANDLE NO-UNDO.
DEF VAR hPanelFrame    AS HANDLE NO-UNDO.
DEF VAR cMenuList      AS CHAR   NO-UNDO.

hSelector = SOURCE-PROCEDURE.

ASSIGN cAddToParentMenu = ""
       iInsertAfter = 0.

/* Causes finding the highest value of iSeq before new rows are selected.
   Passed on to setSelectorTarget to enable assigning sequence numbers to added rows */
DYNAMIC-FUNCTION("setMaxValueField" IN SOURCE-PROCEDURE,"iSeq").

hTbTarget = DYNAMIC-FUNCTION("NewToolBar"
        ,DYNAMIC-FUNCTION("getTargetToolbar" IN SOURCE-PROCEDURE)
        ,""
        ,"MoveDown,MoveUp" + IF bSelectToolGroup THEN ",Edit" ELSE ""
        ,"right").

DYNAMIC-FUNCTION("CreateObjectLink",hTbTarget,ihTargetBrw).

hBrwMove = ihTargetBrw.

ihSourceBrw:WINDOW:ALWAYS-ON-TOP = YES.

hSourceBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1).
hTargetBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1).

DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"basequery","where true").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"uselocaldata","yes").

ihSourceBrw:GET-BROWSE-COLUMN(1):VISIBLE = NO.

DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"1stDbSortColumn","iSeq").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"1stSortColumn","iSeq").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"localsort","iSeq").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"querysort","iSeq").

ix = 1.
/* Fill temp-table: */

IF bSelectToolGroup THEN DO:
  ihTargetBrw:WINDOW:TITLE = "Add (unique) tools from group(s)".
  FOR EACH ttToolGroup BY ttToolGroup.iGroupId:
    hSourceBuffer:BUFFER-CREATE().
    hSourceBuffer:BUFFER-COPY(BUFFER ttToolGroup:HANDLE).
    ASSIGN hSourceBuffer::cTool = ttToolGroup.cGroup
           hSourceBuffer::iSeq = ix * 10
           hSourceBuffer::RowIdent1 = "rowid" + STRING(ix).
    FOR EACH ttToolsForGroup
        WHERE ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
        BY ttToolsForGroup.iSeq:
      hSourceBuffer::cToolList = hSourceBuffer::cToolList
                               + (IF hSourceBuffer::cToolList NE "" THEN "," ELSE "")
                               + ttToolsForGroup.cTool.
    END.
  ix = ix + 1.
  END.
  hToolList = DYNAMIC-FUNCTION("NewBrowseFillIn",ihTargetBrw,"cToolList","cToolList","","","",""). 
  DYNAMIC-FUNCTION("CreateOverlayLink",ihTargetBrw,hToolList,"cToolList").             
  DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"enableOnDblClick","yes").    
  DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"setReadOnlyOnReturn","yes"). 
END.
ELSE
  FOR EACH bttJBoxTool 
      WHERE NOT CAN-DO("menu,sub-menu",bttJBoxTool.cType)
      BY bttJBoxTool.iSeq:
    hSourceBuffer:BUFFER-CREATE().
    hSourceBuffer:BUFFER-COPY(BUFFER bttJBoxTool:HANDLE).
    hSourceBuffer::RowIdent1 = "rowid" + STRING(ix).
    IF bttJBoxTool.bSelect THEN DO:
      hTargetBuffer:BUFFER-CREATE().
      hTargetBuffer:BUFFER-COPY(BUFFER bttJBoxTool:HANDLE).
      ASSIGN hTargetBuffer::RowIdent1 = "rowid" + STRING(ix)
             hTargetBuffer::iSeq  = iSeq
             iSeq = iSeq + 10
             cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ",".
    END.
    ix = ix + 1.
  END.

DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).

hSelectorPanel = ?.

cMenuList = getMenuList(NO).

IF NUM-ENTRIES(cMenuList) > 1 THEN DO:
  RUN LoadPanel IN hSelector ("JukeBox\toolSelectPanel.w",OUTPUT hSelectorPanel).
  IF VALID-HANDLE(hSelectorPanel) THEN DO:
    
    DYNAMIC-FUNCTION("setMenuList" IN hSelectorPanel,cMenuList).
    DYNAMIC-FUNCTION("setSourceProc" IN hSelectorPanel,THIS-PROCEDURE).

    /* Make room for and position the panel frame: */
  
    ASSIGN hPanelFrame                = DYNAMIC-FUNCTION("getFrameHandle" IN hSelectorPanel)
           hSelectorWin               = ihSourceBrw:WINDOW
           hSelectorWin:HEIGHT-PIXELS = hSelectorWin:HEIGHT-PIXELS + hPanelFrame:HEIGHT-PIXELS
           .
    APPLY "window-resized" TO hSelectorWin.
    DYNAMIC-FUNCTION("adjustBrowseHeight" IN hSelector,hPanelFrame:HEIGHT-PIXELS * -1).
    hPanelFrame:Y = ihSourceBrw:Y + ihSourceBrw:HEIGHT-PIXELS.   
  END.
END.

DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
RUN OpenQuerySource IN SOURCE-PROCEDURE.

DYNAMIC-FUNCTION("setCurrentObject",ihTargetBrw). 
RUN OpenQueryTarget IN SOURCE-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorTarget C-Win 
PROCEDURE setSelectorTarget :
/*------------------------------------------------------------------------------
  Purpose:     Assign sequence numbers according to existing selected rows in JBoxSelector
  Parameters:  <none>
  Notes:       The max value is retrieved based existing selection (befor this) 
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrwTarget  AS HANDLE NO-UNDO.
DEF INPUT PARAM ibSelect     AS LOG    NO-UNDO.
DEF INPUT PARAM icCurrMaxSeq AS CHAR   NO-UNDO.
DEF INPUT PARAM icRowids     AS CHAR   NO-UNDO.

DEF VAR iMaxSeq     AS INT NO-UNDO.
DEF VAR hBuffTarget AS HANDLE NO-UNDO.
DEF VAR hQryTarget  AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.

hBuffTarget = ihBrwTarget:QUERY:GET-BUFFER-HANDLE(1).

IF ibSelect THEN DO:
  iMaxSeq = INT(icCurrMaxSeq) + 10.

  DO ix = 1 TO NUM-ENTRIES(icRowids):
    hBuffTarget:FIND-FIRST("WHERE RowIdent1 = '" + ENTRY(ix,icRowids) + "'") NO-ERROR.
    IF hBuffTarget:AVAIL THEN
      ASSIGN hBuffTarget::iSeq = iMaxSeq
             iMaxSeq = iMaxSeq + 10
             .
  END.
END.
ELSE DO:
  CREATE QUERY hQryTarget.
  hQryTarget:SET-BUFFERS(hBuffTarget).
  hQryTarget:QUERY-PREPARE("FOR EACH " + hBuffTarget:NAME + " BY iSeq").
  hQryTarget:QUERY-OPEN().
  hQryTarget:GET-FIRST().
  REPEAT WHILE NOT hQryTarget:QUERY-OFF-END:
    ASSIGN ix = ix + 10
           hBuffTarget::iSeq = ix.
    hQryTarget:GET-NEXT().
  END.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addMenusFromToolDef C-Win 
FUNCTION addMenusFromToolDef RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  After existing buttons have been added then menus need to be added too from the text definition in private-data
    Notes:  Only called when modifying an existing toolbar
------------------------------------------------------------------------------*/
DEF VAR hNextToggle AS HANDLE NO-UNDO.

/*
/* First add the menu-bar level sub-menus: */
DO WITH FRAME {&FRAME-NAME}:
  FOR EACH ttToolDef
      WHERE ttToolDef.cType = "menu"
      BY ttToolDef.iSeq:
  
  
    CREATE ttJBoxTool.
    BUFFER-COPY ttToolDef TO ttJBoxTool.
  
    CASE ttToolDef.iSeq:
      WHEN 1 THEN
        ASSIGN ttJBoxTool.hMenuToggle = tbAdd1:HANDLE
               ttJBoxTool.hMenuFillIn = fiMenu1:HANDLE
               ttJBoxTool.hMenuFilter = btnFilter1:HANDLE
               ttJBoxTool.hMenuFillIn:SCREEN-VALUE = ttJBoxTool.cTool
               hNextToggle = tbAdd2:HANDLE.
      WHEN 2 THEN
        ASSIGN ttJBoxTool.hMenuToggle = tbAdd2:HANDLE
               ttJBoxTool.hMenuFillIn = fiMenu2:HANDLE
               ttJBoxTool.hMenuFilter = btnFilter2:HANDLE
               ttJBoxTool.hMenuFillIn:SCREEN-VALUE = ttJBoxTool.cTool
               hNextToggle = tbAdd3:HANDLE.
      WHEN 3 THEN
        ASSIGN ttJBoxTool.hMenuToggle = tbAdd3:HANDLE
               ttJBoxTool.hMenuFillIn = fiMenu3:HANDLE
               ttJBoxTool.hMenuFilter = btnFilter3:HANDLE
               ttJBoxTool.hMenuFillIn:SCREEN-VALUE = ttJBoxTool.cTool
               hNextToggle = tbAdd4:HANDLE.
      WHEN 4 THEN
        ASSIGN ttJBoxTool.hMenuToggle = tbAdd4:HANDLE
               ttJBoxTool.hMenuFillIn = fiMenu4:HANDLE
               ttJBoxTool.hMenuFilter = btnFilter4:HANDLE
               ttJBoxTool.hMenuFillIn:SCREEN-VALUE = ttJBoxTool.cTool
               hNextToggle = tbAdd5:HANDLE.
      WHEN 5 THEN
        ASSIGN ttJBoxTool.hMenuToggle = tbAdd5:HANDLE
               ttJBoxTool.hMenuFillIn = fiMenu5:HANDLE
               ttJBoxTool.hMenuFilter = btnFilter5:HANDLE
               ttJBoxTool.hMenuFillIn:SCREEN-VALUE = ttJBoxTool.cTool
               hNextToggle = ?.
    END CASE.
    ASSIGN ttJBoxTool.hMenuToggle:HIDDEN = NO
           ttJBoxTool.hMenuToggle:CHECKED = YES
           ttJBoxTool.hMenuFillIn:HIDDEN = NO
           ttJBoxTool.hMenuFilter:HIDDEN = NO
           .
  END.
  IF VALID-HANDLE(hNextToggle) THEN hNextToggle:HIDDEN = NO.

  FOR EACH ttToolDef
      WHERE ttToolDef.cType NE "menu"
      BY ttToolDef.iSeq:

    IF ttToolDef.cType = "sub-menu" THEN DO:
      CREATE ttJBoxTool.  
      ASSIGN ttJBoxTool.iSeq        = ttToolDef.iSeq
             ttJBoxTool.cGroups     = "Menus"
             ttJBoxTool.cTool       = ttToolDef.cTool
             ttJBoxTool.bSelect     = YES
             ttJBoxTool.cType       = "sub-menu" 
             ttJBoxTool.cViewAs     = ""
             ttJBoxTool.cParentMenu = ttToolDef.cParentMenu
             ttJBoxTool.cViewAs     = ttToolDef.cViewAs
             ttJBoxTool.bAlwaysEnable = ttToolDef.bAlwaysEnable
             .
    END.
    ELSE IF ttToolDef.cType = "tool" THEN DO:
      FIND FIRST ttJBoxTool 
           WHERE ttJBoxTool.cTool = ttToolDef.cTool
             AND ttJBoxTool.cType = ttToolDef.cType
           NO-ERROR.
      IF AVAIL ttJBoxTool THEN
        ASSIGN ttJBoxTool.iSeq = ttToolDef.iSeq
               ttJBoxTool.cParentMenu = ttToolDef.cParentMenu
               ttJBoxTool.cViewAs     = ttToolDef.cViewAs
               ttJBoxTool.bAlwaysEnable = ttToolDef.bAlwaysEnable
               ttJBoxTool.bSelect = YES
               .
      ELSE DO:
        CREATE ttJBoxTool.  
        BUFFER-COPY ttToolDef TO ttJBoxTool.
        ttJBoxTool.bSelect = YES.
      END.
    END.
    ELSE IF ttToolDef.cType = "rule" THEN DO:
      CREATE ttJBoxTool.  
      BUFFER-COPY ttToolDef TO ttJBoxTool.
      ASSIGN ttJBoxTool.bSelect = YES
             ttJBoxTool.cViewTool = "Rule"
             ttJBoxTool.cViewAs = "Menu-item"
             .
    END.
  END.
/*   RUN toexcelviafile.p (httJboxTool,-1).  */
END.
*/

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION creToolDef C-Win 
FUNCTION creToolDef RETURNS LOGICAL
  ( INPUT icToolList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix              AS INT  NO-UNDO.
DEF VAR cTool           AS CHAR NO-UNDO.
DEF VAR cViewAs         AS CHAR NO-UNDO.
DEF VAR cType           AS CHAR NO-UNDO.
DEF VAR cLabel          AS CHAR NO-UNDO.
DEF VAR cParentMenu     AS CHAR NO-UNDO.
DEF VAR cProperties     AS CHAR NO-UNDO.
DEF VAR iMnuBarSubSeq   AS INT  NO-UNDO. /* Sequence of submenus on the menu-bar */
DEF VAR iSeq            AS INT  NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icToolList):
  cTool = ENTRY(1,ENTRY(1,ENTRY(ix,icToolList),";"),"¤").
  IF NUM-ENTRIES(ENTRY(ix,icToolList),";") > 1 THEN
    cLabel = ENTRY(1,ENTRY(2,ENTRY(ix,icToolList),";"),"¤").
  ELSE cLabel = "".
  IF NUM-ENTRIES(ENTRY(ix,icToolList),"¤") > 1 THEN
    cProperties = ENTRY(2,ENTRY(ix,icToolList),"¤").
  ELSE cProperties = "".

  CREATE ttToolDef.

  /* menus */
  IF NUM-ENTRIES(cTool,"|") > 1 AND (ENTRY(1,cTool,"|") = "" OR ENTRY(1,cTool,"|") = "sub-menu") THEN DO:
    IF ENTRY(1,cTool,"|") = "" THEN DO:
      ASSIGN ttToolDef.cTool       = ENTRY(2,cTool,"|")
             ttToolDef.cParentMenu = "menu-bar"
             iMnuBarSubSeq         = iMnuBarSubSeq + 1
             ttToolDef.iSeq        = iMnuBarSubSeq
             ttToolDef.cType       = "menu"
             .
    END.
    ELSE DO:
      ASSIGN ttToolDef.cTool       = ENTRY(2,ENTRY(ix,icToolList),";")
             ttToolDef.cParentMenu = ENTRY(1,ENTRY(2,cTool,"|"),"¤")
             iSeq                  = iSeq + 10
             ttToolDef.iSeq        = iSeq
             ttToolDef.cType       = "sub-menu"             
             . 
      IF cProperties MATCHES "*menu*" THEN
        ttToolDef.cViewAs = "Menu only".
      IF cProperties MATCHES "*enable*" THEN
        ttToolDef.bAlwaysEnable = YES.
    END.
  END.
  /* Tools */
  ELSE IF NOT CAN-DO("rule,-",ENTRY(1,cTool,"|")) THEN DO:
    ASSIGN ttToolDef.cTool  = ENTRY(1,cTool,"|")
           ttToolDef.cLabel = cLabel
           iSeq             = iSeq + 10
           ttToolDef.iSeq   = iSeq
           ttToolDef.cType  = "tool".

    IF NUM-ENTRIES(cTool,"|") > 1 THEN DO:
      ttToolDef.cParentMenu = ENTRY(2,cTool,"|").
      IF ttToolDef.cParentMenu = "" THEN
        ttToolDef.cViewAs = "Button only".
    END.
    IF cProperties MATCHES "*menu*" THEN
      ttToolDef.cViewAs = "Menu-item".
    IF cProperties MATCHES "*enable*" THEN
      ttToolDef.bAlwaysEnable = YES.
  END.
  /* Rule */
  ELSE
    ASSIGN ttToolDef.cTool  = GUID
           ttToolDef.cLabel = cLabel
           iSeq             = iSeq + 10
           ttToolDef.iSeq   = iSeq
           ttToolDef.cType  = "rule"
           .
END.

/*
DEF VAR httToolDef AS HANDLE NO-UNDO.
httToolDef = BUFFER ttToolDef:HANDLE.
RUN toexcelviafile.p(httToolDef,-1).
*/

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillTbTemplates C-Win 
FUNCTION fillTbTemplates RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Import toolbar templates.
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cConfig     AS CHAR NO-UNDO.
DEF VAR cLine       AS CHAR NO-UNDO.
DEF VAR cTbName     AS CHAR NO-UNDO.
DEF VAR cMenuLabel  AS CHAR NO-UNDO.
DEF VAR cBorder     AS CHAR NO-UNDO.
DEF VAR cActList    AS CHAR NO-UNDO.
DEF VAR cAction     AS CHAR NO-UNDO.
DEF VAR cActionDef  AS CHAR NO-UNDO.
DEF VAR bNoMenu     AS LOG  NO-UNDO.
DEF VAR bMenuOnly   AS LOG  NO-UNDO.
DEF VAR bTgl        AS LOG  NO-UNDO.
DEF VAR cImg        AS CHAR NO-UNDO.
DEF VAR cAccel      AS CHAR NO-UNDO.
DEF VAR cLabel      AS CHAR NO-UNDO.
DEF VAR cTTip       AS CHAR NO-UNDO.
DEF VAR cMethod     AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR iAppGrpCnt  AS INT  NO-UNDO INIT 1.

IF SEARCH("template\toolbarConfig.txt") NE ? THEN DO:
  cConfig = ImportConfig(SEARCH("template\toolbarConfig.txt")).

  IF cConfig NE "" THEN DO ix = 1 TO NUM-ENTRIES(cConfig,CHR(10)):
    cLine = TRIM(ENTRY(ix,cConfig,CHR(10))).

    IF cLine BEGINS "<name>" THEN DO:
      IF INDEX(cLine,",") > 0 THEN DO:
        MESSAGE "Invalid toolbar name: " SUBSTR(cLine,7) SKIP
                "Name cannot contain ,"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.  
        NEXT.
      END.
      IF cTbName NE "" THEN DO:
        CREATE ttToolbar.
        ASSIGN ttToolbar.cTbName  = cTbName
               ttToolbar.cActList = cActList
               ttToolbar.cBorder  = cBorder
               .
      END.
      ASSIGN cTbName    = SUBSTR(cLine,7)
             cActList   = ""
             cMenuLabel = ""
             cBorder    = ""
             .
    END. 
    ELSE IF cLine BEGINS "<menu>" THEN
      cMenuLabel = SUBSTR(cLine,7).
    ELSE IF cLine BEGINS "<tools>" THEN
      cActList = cActList + SUBSTR(cLine,8).
    ELSE IF cLine BEGINS "<border>" THEN
      cBorder = SUBSTR(cLine,9).
  END.

  IF cActList NE "" THEN DO:
    CREATE ttToolbar.
    ASSIGN ttToolbar.cTbName  = cTbName
           ttToolbar.cActList = cActList
           ttToolbar.cBorder  = cBorder
           .
  END.

  FOR EACH ttToolbar
      BY ttToolbar.cTbName:

    CREATE ttToolGroup.
    ASSIGN idxGroup = idxGroup + 1
           ttToolGroup.iGroupId = idxGroup
           ttToolGroup.cGroupName = ttToolbar.cTbName
           ttToolGroup.cGroup = "Application" + STRING(iAppGrpCnt)
           iAppGrpCnt = iAppGrpCnt + 1
           .

    DO ix = 1 TO NUM-ENTRIES(ttToolbar.cActList):
      ASSIGN cImg         = ""
             bNoMenu      = NO
             bMenuOnly    = NO
             cAccel       = ""    
             cActionDef   = ENTRY(1,ENTRY(ix,ttToolbar.cActList),"¤")
             cAction      = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
             cLabel       = IF NUM-ENTRIES(cActionDef,";") > 1 AND ENTRY(2,cActionDef,";") NE "" THEN
                              ENTRY(2,cActionDef,";")
                            ELSE IF DYNAMIC-FUNCTION("getAttribute",SESSION,"BtnLabel_" + cAction) NE "" THEN
                              DYNAMIC-FUNCTION("getAttribute",SESSION,"BtnLabel_" + cAction)
                            ELSE
                              CAPS(SUBSTR(cAction,1,1)) + SUBSTR(cAction,2)
             cTTip        = cLabel
             cMethod      = cAction + "Record"
             .

      IF NUM-ENTRIES(cActionDef,";") = 2 THEN 
        ASSIGN cLabel   = ENTRY(2,cActionDef,";")
               cTTip    = cLabel.
      ELSE IF NUM-ENTRIES(cActionDef,";") = 3 THEN 
        ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
               cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
               cTTip    = ENTRY(3,cActionDef,";")
               .
      ELSE IF NUM-ENTRIES(cActionDef,";") = 4 THEN
        ASSIGN cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
               cTTip    = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cTTip
               cMethod  = ENTRY(4,cActionDef,";")
               .
      ELSE IF NUM-ENTRIES(cActionDef,";") = 5 THEN 
        ASSIGN cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
               cTTip    = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cTTip
               cMethod  = IF ENTRY(4,cActionDef,";") NE "" THEN ENTRY(4,cActionDef,";") ELSE cMethod
               cImg     = ENTRY(5,cActionDef,";")
               .
      ELSE IF NUM-ENTRIES(cActionDef,";") > 5 THEN DO:
        MESSAGE "Error in TB def for element " cActionDef 
                VIEW-AS ALERT-BOX.
        RETURN NO.
      END.
  
      ASSIGN cLabel       = REPLACE(cLabel,CHR(1),",")    
             cTTip        = REPLACE(cTTip,CHR(1),",")    
             bTgl         = NUM-ENTRIES(cActionDef,";") > 2 AND ENTRY(3,cActionDef,";") = "toggle"
             cAccel       = IF INDEX(cLabel,"ctrl-") > 0 THEN
                              "CTRL-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"ctrl-") + 5,1))
                            ELSE IF INDEX(cLabel,"alt-") > 0 THEN
                              "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"alt-") + 4,1))
                            ELSE IF INDEX(cLabel,"&") > 0 AND R-INDEX(cLabel,"&") NE LENGTH(cLabel) THEN
                             "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"&") + 1,1))
                            ELSE IF INDEX(cLabel,"&") > 0 THEN ""
                            ELSE IF CAN-DO(cFunKeyActs,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) THEN 
                              ENTRY(LOOKUP(cAction,cFunKeyActs),cFunKeys)
                            ELSE IF CAN-DO(cCtrlHotkeyActions,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) THEN 
                             "CTRL-" + ENTRY(LOOKUP(cAction,cCtrlHotkeyActions),cCtrlHotkeys) 
                            ELSE IF CAN-DO(cAltHotkeyActions,cAction) THEN 
                             "ALT-" + ENTRY(LOOKUP(cAction,cAltHotkeyActions),cAltHotkeys) 
                            ELSE "ALT-" + CAPS(SUBSTR(cLabel,1,1))
             cLabel       = (IF INDEX(cLabel,"ctrl-") > 0 THEN 
                              TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"ctrl-") - 1))
                             ELSE IF INDEX(cLabel,"alt-") > 0 THEN 
                              TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"alt-") - 1))
                             ELSE IF R-INDEX(cLabel,"&") = LENGTH(cLabel) THEN 
                              REPLACE(cLabel,"&","")
                             ELSE cLabel)
             cTTip        = RIGHT-TRIM(cTTip + " (" + cAccel," (") + (IF cAccel NE "" THEN ")" ELSE "")
             .
      
      IF cImg = "" AND LOOKUP("BtnImg_" + cAction,cToolList) > 0 THEN 
        cImg = ENTRY(LOOKUP("BtnImg_" + cAction,cToolList),cImgList,CHR(1)).

      FIND FIRST ttJBoxTool
           WHERE ttJBoxTool.cTool = cAction
           NO-ERROR.
      IF NOT AVAIL ttJBoxTool THEN DO:
        CREATE ttJBoxTool.
        ASSIGN ttJBoxTool.iSeq     = ix * 10
               ttJBoxTool.cLabel   = cLabel
               ttJBoxTool.cMethod  = cMethod
               ttJBoxTool.cTool    = cAction
               ttJBoxTool.cImage   = cImg
               ttJBoxTool.cToolTip = cTTip
               ttJBoxTool.cAccel   = cAccel
               ttJBoxTool.bToggle  = bTgl
               ttJBoxTool.bBuiltIn = CAN-DO(cAllBuiltIn,ttJBoxTool.cTool)
               ttJBoxTool.bCreProc = NOT ttJBoxTool.bBuiltIn
               .
      END.
      ttJBoxTool.cGroups = ttJBoxTool.cGroups + (IF ttJBoxTool.cGroups NE "" THEN "," ELSE "") + ttToolbar.cTbName.

      CREATE ttToolsForGroup.
      ASSIGN ttToolsForGroup.cTool = ttJBoxTool.cTool
             ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
             .
    END.
  END.
  RETURN YES.  
END.
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuList C-Win 
FUNCTION getMenuList RETURNS CHARACTER
  ( INPUT ibPairs AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cMenuList AS CHAR NO-UNDO.

DEF BUFFER ttJBoxTool FOR ttJBoxTool.

DO WITH FRAME {&FRAME-NAME}:
  FOR EACH ttJBoxTool 
      WHERE CAN-DO("sub-menu,menu",ttJBoxTool.cType)
        AND NOT ttJBoxTool.bDeleted
      BY ttJBoxTool.iSeq:
    IF ibPairs THEN
      cMenuList = cMenuList + "|" + ttJBoxTool.cTool + "|" + ttJBoxTool.cTool.
    ELSE
      cMenuList = cMenuList + "," + ttJBoxTool.cTool.
  END.
END.

RETURN cMenuList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuNodes C-Win 
FUNCTION getMenuNodes RETURNS CHARACTER
  ( INPUT icParentMenu AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER ttJBoxTool FOR ttJBoxTool.

DEF VAR cMenuList AS CHAR NO-UNDO.

FOR EACH ttJBoxTool 
    WHERE ttJBoxTool.bSelect
      AND NOT ttJBoxTool.bDeleted
      AND (IF icParentMenu NE "" THEN ttJBoxTool.cParentMenu = icParentMenu ELSE TRUE)
    BY ttJBoxTool.iSeq
    :
  cMenuList = cMenuList + (IF cMenuList NE "" THEN "," ELSE "") + ttJBoxTool.cTool + "," + STRING(ttJBoxTool.iSeq).
END.

RETURN cMenuList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ImportConfig C-Win 
FUNCTION ImportConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput     AS CHAR NO-UNDO.
DEF VAR cReturn    AS CHAR NO-UNDO.
DEF VAR cQryObject AS CHAR NO-UNDO.
DEF VAR cFmObject  AS CHAR NO-UNDO.

INPUT FROM VALUE(icFileName).

REPEAT:
  IMPORT UNFORMATTED cInput.  
  cReturn = cReturn + cInput + CHR(10).
END.
INPUT CLOSE.

cReturn = REPLACE(cReturn,"<ToolbarObject>","o" + fiToolbarName:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

IF cmbApplyToQry:SCREEN-VALUE NE "" AND cmbApplyToQry:SCREEN-VALUE NE ? THEN 
  cQryObject = cmbApplyToQry:SCREEN-VALUE.
ELSE
  cQryObject = cFirstQry.

IF cmbApplyToFm:SCREEN-VALUE NE "" AND cmbApplyToFm:SCREEN-VALUE NE ? THEN 
  cFmObject = cmbApplyToFm:SCREEN-VALUE.
ELSE
  cFmObject = cFirstFm.

IF cQryObject NE "" THEN
  cReturn = REPLACE(cReturn,"<QueryObject>",cQryObject).
IF cFmObject NE "" THEN
  cReturn = REPLACE(cReturn,"<FieldMapObject>",cFmObject).

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION leaveFiMenu C-Win 
FUNCTION leaveFiMenu RETURNS LOGICAL
  ( INPUT ihFiMenu    AS HANDLE,
    INPUT ihTbAdd     AS HANDLE,
    INPUT ihBtnFilter AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF ihFiMenu:SCREEN-VALUE = "" AND ihFiMenu:PRIVATE-DATA NE "" AND ihFiMenu:PRIVATE-DATA NE ? THEN 
    ihFiMenu:SCREEN-VALUE = ihFiMenu:PRIVATE-DATA.
  ELSE IF ihFiMenu:SCREEN-VALUE NE "<label>" THEN DO:
    IF ihFiMenu:SCREEN-VALUE = "" OR ihFiMenu:SCREEN-VALUE = ? THEN DO:
      ihFiMenu:SCREEN-VALUE = "<label>".
      RETURN NO-APPLY.
    END.
    IF ihFiMenu:PRIVATE-DATA NE "" AND ihFiMenu:PRIVATE-DATA NE ? AND ihFiMenu:SCREEN-VALUE NE ihFiMenu:PRIVATE-DATA THEN DO:
      FIND FIRST ttJBoxTool 
           WHERE ttJBoxTool.cTool = ihFiMenu:PRIVATE-DATA
             AND ttJBoxTool.cType = "menu"
           NO-ERROR.
      IF AVAIL ttJBoxTool THEN DO:
        ASSIGN ihFiMenu:PRIVATE-DATA = ihFiMenu:SCREEN-VALUE
               ttJBoxTool.cTool = ihFiMenu:SCREEN-VALUE.
        FOR EACH bttJBoxTool
            WHERE bttJBoxTool.cParentMenu = ihFiMenu:PRIVATE-DATA:
          bttJBoxTool.cParentMenu = ihFiMenu:SCREEN-VALUE.
        END.
      END.
    END.
    ELSE IF ihFiMenu:PRIVATE-DATA = "" OR ihFiMenu:PRIVATE-DATA = ? THEN DO:
      FIND FIRST ttJBoxTool 
           WHERE ttJBoxTool.cTool = ihFiMenu:SCREEN-VALUE
             AND ttJBoxTool.cType = "menu"
             AND ttJBoxTool.bDeleted 
           NO-ERROR.
      IF AVAIL ttJBoxTool THEN DO:
        FIND FIRST ttJBoxTool 
             WHERE bttJBoxTool.cTool = ihFiMenu:SCREEN-VALUE
               AND bttJBoxTool.cType = "menu"
               AND NOT bttJBoxTool.bDeleted 
               AND bttJBoxTool.hMenuFillIn NE ihFiMenu
             NO-ERROR.
        IF AVAIL bttJBoxTool THEN
          ihFiMenu:SCREEN-VALUE = ihFiMenu:SCREEN-VALUE + "-1".
        ttJBoxTool.bDeleted = NO.
      END.
      ELSE DO:
        CREATE ttJBoxTool.
        ASSIGN ttJBoxTool.cTool = ihFiMenu:SCREEN-VALUE
               ttJBoxTool.cType = "menu"
               ttJBoxTool.iSeq  = INT(SUBSTR(ihFiMenu:NAME,LENGTH(ihFiMenu:NAME))) /* * 1000 */
               .
      END.        
      ASSIGN ihFiMenu:PRIVATE-DATA = ihFiMenu:SCREEN-VALUE
             ttJBoxTool.hMenuFillIn = ihFiMenu
             ttJBoxTool.hMenuToggle = ihTbAdd
             ttJBoxTool.hMenuFilter = ihBtnFilter
             .
    END.

    setMenuCombos("").

    reSequenceMenu().
    
    hBrowse:REFRESH().

  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadToolbarDef C-Win 
FUNCTION LoadToolbarDef RETURNS LOGICAL
  ( INPUT icToolbarDef AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cBtnTool    AS CHAR NO-UNDO.
DEF VAR cBtnSize    AS CHAR NO-UNDO.
DEF VAR iNewIdx     AS INT  NO-UNDO INIT 10.

DO WITH FRAME {&FRAME-NAME}:
  IF NUM-ENTRIES(icToolbarDef,CHR(1)) > 1 THEN
    creToolDef(ENTRY(1,icToolbarDef,CHR(1))).

  FOR EACH b_U 
      WHERE b_U._WINDOW-HANDLE = _h_win
        AND b_U._NAME MATCHES "*" + iocToolbarName
        AND b_U._TYPE = "button"
        AND NOT b_U._DELETED
     ,FIRST b_L WHERE RECID(b_L) = b_U._lo-recid
     ,FIRST b_F WHERE RECID(b_F) = b_U._x-recid
      BY b_L._COL
      :
    IF ofStartCol = 0 THEN 
      ASSIGN ofStartCol               = b_L._COL
             ofStartRow               = b_L._ROW
             .
    ASSIGN cBtnTool = SUBSTR(b_U._NAME,1,INDEX(b_U._NAME,iocToolbarName) - 2)
           cBtnSize = STRING(b_L._WIDTH * 100) + "x" + STRING(b_L._HEIGHT * 100)
           .
    FIND FIRST ttJBoxTool
         WHERE ttJBoxTool.cTool = cBtnTool
         NO-ERROR.
    IF NOT AVAIL ttJBoxTool THEN DO:
      CREATE ttJBoxTool.
      ASSIGN ttJBoxTool.cGroups  = "custom"
             ttJBoxTool.cTool    = cBtnTool
             .

      FIND FIRST ttToolGroup WHERE ttToolGroup.cGroup = "custom"
           NO-ERROR.
      IF NOT AVAIL ttToolGroup THEN DO:
        CREATE ttToolGroup.
        ASSIGN idxGroup = idxGroup + 1
               ttToolGroup.iGroupId = idxGroup
               ttToolGroup.cGroupName = "Custom tools"
               ttToolGroup.cGroup = "custom"
               .
      END.
      CREATE ttToolsForGroup.
      ASSIGN idxToolSeq = idxToolSeq + 1
             ttToolsForGroup.cTool = ttJBoxTool.cTool
             ttToolsForGroup.iGroupId = ttToolGroup.iGroupId
             ttToolsForGroup.iSeq = idxToolSeq
             .
    END.
    ASSIGN ttJBoxTool.bSelect = YES
           ttJBoxTool.bExist  = YES
           ttJBoxTool.U_recid = RECID(b_U)
           ttJBoxTool.cLabel  = b_U._LABEL
           ttJBoxTool.cImage  = b_F._IMAGE-FILE
           ttJBoxTool.fCol    = b_L._COL
           ttJBoxTool.iSeq    = iNewIdx 
           iNewIdx            = iNewIdx + 10
           .
    IF b_U._TOOLTIP NE "" AND b_U._TOOLTIP NE ? THEN
      ttJBoxTool.cTooltip = b_U._TOOLTIP.
    IF b_F._IMAGE-FILE = "" THEN
      ttJBoxTool.fWidth = b_L._WIDTH.

    FIND FIRST ttToolDef
         WHERE ttToolDef.cTool = ttJBoxTool.cTool NO-ERROR.
    IF AVAIL ttToolDef THEN
      BUFFER-COPY ttToolDef EXCEPT cTool cLabel TO ttJBoxTool.
  END.

  IF NUM-ENTRIES(icToolbarDef,CHR(1)) > 1 THEN DO:

    addMenusFromToolDef().

  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION makeRoomForInsert C-Win 
FUNCTION makeRoomForInsert RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bInsertFirst AS LOG NO-UNDO.
DEF VAR iFirstInMenu AS INT NO-UNDO.

DEF BUFFER ttJBoxTool FOR ttJBoxTool.

IF iInsertAfter = ? THEN iInsertAfter = 0.

IF iInsertAfter = 0 AND cAddToParentMenu NE "" THEN DO:
  bInsertFirst = YES.
  FOR EACH ttJBoxTool 
      WHERE ttJBoxTool.cParentMenu = cAddToParentMenu
        AND ttJBoxTool.bSelect
      BY ttJBoxTool.iSeq:
    iFirstInMenu = ttJBoxTool.iSeq.
    LEAVE.
  END.
  FOR EACH ttJBoxTool
      WHERE ttJBoxTool.bSelect 
        AND ttJBoxTool.iSeq < iFirstInMenu
      BY ttJBoxTool.iSeq DESC:
    iInsertAfter = ttJBoxTool.iSeq.
    LEAVE.
  END.
END.

IF iInsertAfter NE 0 OR bInsertFirst THEN DO:
  REPEAT PRESELECT EACH ttJBoxTool
     WHERE ttJBoxTool.iSeq > iInsertAfter
     :
    FIND NEXT ttJBoxTool NO-ERROR.
    IF AVAIL ttJBoxTool THEN
      ttJBoxTool.iSeq = ttJBoxTool.iSeq + 1000.
    ELSE LEAVE.
  END.
  RETURN YES.
END.
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION okMove C-Win 
FUNCTION okMove RETURNS LOGICAL
  ( INPUT icDir AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix        AS INT NO-UNDO.
DEF VAR hBuffMove AS HANDLE NO-UNDO.
DEF VAR iMinSeq   AS INT NO-UNDO INIT 100000.
DEF VAR iMaxSeq   AS INT NO-UNDO.
DEF VAR bOkMove   AS LOG NO-UNDO INIT YES.
DEF VAR bMenuBar  AS LOG NO-UNDO.
DEF VAR bOther    AS LOG NO-UNDO.

EMPTY TEMP-TABLE ttValidateMove.

hBuffMove = hBrwMove:QUERY:GET-BUFFER-HANDLE(1). 
DO ix = 1 TO hBrwMove:NUM-SELECTED-ROWS:
  IF hBrwMove:FETCH-SELECTED-ROW(ix) THEN DO:
    CREATE ttValidateMove.
    ttValidateMove.iSeq = hBuffMove::iSeq.
    IF ttValidateMove.iSeq < iMinSeq THEN
      iMinSeq = ttValidateMove.iSeq.
    IF ttValidateMove.iSeq > iMaxSeq THEN
      iMaxSeq = ttValidateMove.iSeq.
    IF ttValidateMove.iSeq < 10 THEN bMenuBar = YES.
    ELSE bOther = YES.
  END.
END.

IF bOther AND bMenuBar THEN DO:
  MESSAGE "Cannot move menu-bar level menus and sub-menus/tools in combination"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.  
  RETURN bOkMove.
END.
IF iMinSeq = 10 AND bOther AND icDir = "Up" THEN bOkMove = NO.

IF bMenuBar AND NOT CAN-FIND(FIRST bttJBoxTool WHERE bttJBoxTool.iSeq < 10 AND bttJBoxTool.iSeq > iMaxSeq) THEN RETURN NO.

ix = iMinSeq - 10.
FOR EACH ttValidateMove BY ttValidateMove.iSeq:
  IF ttValidateMove.iSeq - 10 NE ix AND bOther THEN 
    bOkMove = NO.
  IF ttValidateMove.iSeq - 1 NE ix AND bMenuBar THEN 
    bOkMove = NO.
  ix = ttValidateMove.iSeq.
END.

IF NOT bOkMove THEN
  MESSAGE "Invalid selection for move"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
RETURN bOkMove.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION reSeqSubMenu C-Win 
FUNCTION reSeqSubMenu RETURNS LOGICAL
  ( INPUT iiRootSeq AS INT,
    INPUT icParent  AS CHAR,
    INPUT iiLevel   AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER ttJBoxTool FOR ttJBoxTool.

REPEAT PRESELECT EACH ttJBoxTool
   WHERE (IF icParent NE "" THEN ttJBoxTool.cParentMenu = icParent ELSE TRUE)
     AND ttJBoxTool.bSelect
      BY ttJBoxTool.iSeq:
  FIND NEXT ttJBoxTool NO-ERROR.
  IF AVAIL ttJBoxTool THEN DO:
    ASSIGN iMasterSeq = iMasterSeq + 10
           ttJBoxTool.iSeq = iMasterSeq
           ttJBoxTool.iLevel = iiLevel
           ttJBoxTool.cViewTool = (IF iiLevel > 1 THEN " " + FILL("--",iiLevel - 1) + " " ELSE "") 
                                + (IF ttJBoxTool.cType NE "rule" THEN ttJBoxTool.cTool ELSE "Rule")
           .

    iocToolbarDef = iocToolbarDef + (IF iocToolbarDef NE "" THEN "," ELSE "")
               + (IF ttJBoxTool.cType = "sub-menu" THEN
                   "sub-menu|" + ttJBoxTool.cParentMenu + ";" + ttJBoxTool.cTool
                  ELSE
                    (IF ttJBoxTool.cType = "rule" THEN "Rule" ELSE ttJBoxTool.cTool)
                  + (IF ttJBoxTool.cLabel NE "" THEN ";" + ttJBoxTool.cLabel ELSE "")
                  + (IF ttJboxTool.bToggle THEN ";toggle" ELSE ""))
                  + (IF ttJBoxTool.cViewAs = "menu-item" OR ttJBoxTool.bAlwaysEnable THEN "¤" ELSE "")
                  + (IF ttJBoxTool.cViewAs = "menu-item" THEN "menu" ELSE "")
                  + (IF ttJBoxTool.bAlwaysEnable THEN "enable" ELSE "")
               .
    IF ttJBoxTool.cType = "rule" THEN
      ttJBoxTool.bMenuOnly = YES.

    IF ttJBoxTool.cType = "sub-menu" THEN
      reSeqSubMenu(ttJBoxTool.iSeq,ttJBoxTool.cTool,iiLevel + 1).
  END.
  ELSE LEAVE.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION reSequenceMenu C-Win 
FUNCTION reSequenceMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER ttJBoxTool FOR ttJBoxTool.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iocToolbarDef = ""
         iMasterSeq = 0.

  reSeqSubMenu(0,"",1).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCode C-Win 
FUNCTION setCode RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQueryObj   AS CHAR NO-UNDO.
DEF VAR cFmObj      AS CHAR NO-UNDO.
DEF VAR cTable      AS CHAR NO-UNDO.
DEF VAR cToolbarObj AS CHAR NO-UNDO.

ASSIGN cQueryObj = IF ENTRY(1,icQryObjects) NE "" THEN ENTRY(1,icQryObjects) ELSE "<qryObject>"
       cFmObj    = IF ENTRY(1,icFmObjects)  NE "" THEN ENTRY(1,icFmObjects) ELSE "<fieldMapObject>"
       .
IF iocToolbarName NE "" THEN
  cToolbarObj = "o" + iocToolbarName.
ELSE IF cQueryObj NE "qryObject" THEN
  cToolbarObj = "oTb" + SUBSTR(cQueryObj,5).
ELSE
  cToolbarObj = "toolbarObject".

IF ENTRY(1,icFmObjects) NE "" THEN
  cTable = SUBSTR(ENTRY(1,icFmObjects),4).
ELSE cTable = "<bufferName>".

CASE ttJboxTool.cTool:
  WHEN "documents" THEN
    ASSIGN ttJBoxTool.cCode = 
               "DEF VAR hDocTree AS HANDLE NO-UNDO. /* <- Move to Defintions */" + CHR(10) + CHR(10)     
             + "IF NOT VALID-HANDLE(hDocTree) THEN DO:" + CHR(10)
             + "  RUN JBoxViewDocTree.w PERSIST SET hDocTree." + CHR(10)
             + "  RUN InitializeObject IN hDocTree." + CHR(10)  
             + "END." + CHR(10) + CHR(10)
             + "DYNAMIC-FUNCTION('setDocContext' IN hDocTree," + CHR(10)
             + "     STRING(" + cTable + ".<idField>)," + CHR(10) 
             + "     '" + cTable + "'," + CHR(10) 
             + "     '<default descr for new docs>')." + CHR(10) + CHR(10)
             + "RUN MoveToTop IN hDocTree." + CHR(10)
    ttJboxTool.cTip = "To indicate that there are documents related to the record add this to DisplayRecord (before RUN SUPER):" + CHR(10)
             + "IF AVAIL " + cTable + " THEN " + cToolbarObj + ":docAvailable = " + cTable + ".DocumentsExist." + CHR(10)
             + "where 'DocumentsExist is defined as a calculated field:" + CHR(10)
             + "     + ';+DocumentsExist|LOGICAL||jb_can-find(FIRST JBoxDocRel WHERE JBoxDocRel.cContext = '" + cTable + "' AND JBoxDocRel.cEntityId = STRING(<idField>))" + CHR(10) + CHR(10)
             + "Also remember to set the doc.context in DisplayRecord (after RUN SUPER)" + CHR(10)
             + "and close the doc. window (ON CLOSE OF THIS PROCEDURE):" + CHR(10)
             + "  IF VALID-HANDLE(hDocTree) THEN APPLY 'close' TO hDocTree." + CHR(10)
             .

  WHEN "note" THEN
    ASSIGN ttJboxTool.cCode = 
               "RUN SUPER." + CHR(10)
             + "IF " + cToolbarObj + ":noteCurrentValueChanged THEN DO:" + CHR(10)
             + "  IF DYNAMIC-FUNCTION('DoUpdate','" + cTable + "',''," + CHR(10)
             + "     ''," + cTable + ".RowIdent1," + CHR(10)
             + "     '<noteField>'," + cToolbarObj + ":noteCurrentValue," + CHR(10)
             + "     YES) THEN" + CHR(10)
             + "    " + cQueryObj + ":refreshRow(" + cTable + ".RowIdent1)." + CHR(10)
             + "END." + CHR(10)

           ttJboxTool.cTip = "To assign a field in the query to be edited as note set this propery in DisplayRecord (before RUN SUPER):" + CHR(10)
             + "  " + cToolbarObj + ":noteCurrentValue = " + cTable + ".<noteField>."
             .
  WHEN "www" THEN
    ASSIGN ttJboxTool.cCode = 
               "DEF VAR cURL         AS CHAR NO-UNDO." + CHR(10) + CHR(10)

             + REPLACE("DO WITH FRAME <&FRAME-NAME}:","<",CHR(123)) + CHR(10) 
             + "  cURL = 'http://www.gulesider.no/finn:'" + CHR(10)
             + "       + <namefield>:SCREEN-VALUE" + CHR(10)
             + "       + (IF <addressfield>:SCREEN-VALUE NE '' THEN '+' + <addressfield>:SCREEN-VALUE ELSE '')" + CHR(10)
             + "       + (IF <zipcodefield>:SCREEN-VALUE NE '' THEN '+' + <zipcodefield>:SCREEN-VALUE ELSE '')" + CHR(10)
             + "       ." + CHR(10) + CHR(10)
          
             + "  DYNAMIC-FUNCTION('setWebDoc','',cURL)." + CHR(10)
             + "END." + CHR(10)
             .
END CASE.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuCombos C-Win 
FUNCTION setMenuCombos RETURNS LOGICAL
  ( INPUT icExclude AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cMenuList              AS CHAR NO-UNDO.
DEF VAR cSubMenus              AS CHAR NO-UNDO.
DEF VAR cCmbSubMenuScreenValue AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  FOR EACH bttJBoxTool
      WHERE CAN-DO("menu,sub-menu",bttJBoxTool.cType)
        AND NOT bttJBoxTool.bDeleted
        AND bttJBoxTool.cTool NE icExclude
      BY bttJBoxTool.iSeq
      :
    cMenuList = cMenuList + "," + bttJBoxTool.cTool.
    IF bttJBoxTool.cType = "sub-menu" THEN
      cSubMenus = cSubMenus + "," + bttJBoxTool.cTool.
  END.
  hParentMenuCol:LIST-ITEMS = cMenuList.
  /*
  IF cSubMenus = "" THEN
    cmbSubMenu:HIDDEN = YES.
  ELSE
    ASSIGN cCmbSubMenuScreenValue = cmbSubMenu:SCREEN-VALUE 
           cmbSubMenu:LIST-ITEMS = cSubMenus
           cmbSubMenu:HIDDEN = NO
           cmbSubMenu:SCREEN-VALUE = cCmbSubMenuScreenValue
           .
  */         
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuFilter C-Win 
FUNCTION setMenuFilter RETURNS LOGICAL
  ( INPUT ihBtnFilter AS HANDLE,
    INPUT ihFiMenu    AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF ihBtnFilter:PRIVATE-DATA = ? THEN DO:
    ASSIGN cMenuParentFilter = ihFiMenu:SCREEN-VALUE
           ihBtnFilter:PRIVATE-DATA = "checked"
           .
    ihBtnFilter:LOAD-IMAGE("gif\filterc.gif").

    FOR EACH ttJBoxTool 
        WHERE ttJBoxTool.hMenuFilter NE ?
          AND ttJBoxTool.hMenuFilter NE ihBtnFilter
          :
        
      IF ttJBoxTool.hMenuFilter:PRIVATE-DATA = "checked" THEN DO:
        ttJBoxTool.hMenuFilter:LOAD-IMAGE("gif\filter.gif").
        ttJBoxTool.hMenuFilter:PRIVATE-DATA = "".
      END.
    END.
  END.
  ELSE DO:
    ASSIGN cMenuParentFilter = ""
           ihBtnFilter:PRIVATE-DATA = ?
           .
    ihBtnFilter:LOAD-IMAGE("gif\filter.gif").
  END.

  RUN OpenQuery.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION unCheckTbAdd C-Win 
FUNCTION unCheckTbAdd RETURNS LOGICAL
  ( INPUT ihTbAdd  AS HANDLE,
    INPUT ihFillIn AS HANDLE,
    INPUT ihFilter AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bOk AS LOG NO-UNDO.
DEF VAR hThisMenuFillIn AS HANDLE NO-UNDO.
DEF VAR ix  AS INT NO-UNDO.

/*
DEF BUFFER ttJBoxTool  FOR ttJBoxTool.
DEF BUFFER bttJBoxTool FOR ttJBoxTool.

DO WITH FRAME {&FRAME-NAME}:
  FIND FIRST ttJBoxTool
       WHERE ttJBoxTool.hMenuToggle = SELF
         AND NOT ttJBoxTool.bDeleted
       NO-ERROR.
  IF AVAIL ttJBoxTool THEN DO:
    MESSAGE "Removing the menu node will cause it's child nodes to be" SKIP
            "swapped to the primary menu node: " 
            "Continue?"
            VIEW-AS ALERT-BOX WARNING UPDATE bOk.
    IF bOk THEN DO:
      ASSIGN ttJBoxTool.bDeleted = YES
             ttJBoxTool.hMenuFillin = ?
             ttJBoxTool.hMenuFilter = ?
             .
    
      FOR EACH bttJBoxTool
          WHERE bttJBoxTool.cParentMenu = ihFillIn:SCREEN-VALUE
          :
        bttJBoxTool.cParentMenu = fiMenu:SCREEN-VALUE.
      END.
      setMenuCombos("").
      RUN OpenQuery.
    END.
    ELSE DO:
      ihTbAdd:CHECKED = YES.
      RETURN NO.
    END.
  END.

  cMenuParentFilter     = "".
  IF ihFilter:PRIVATE-DATA = "checked" THEN RUN OpenQuery.

  FOR EACH bttJBoxTool
      WHERE bttJBoxTool.hMenuFillIn NE ?
        AND bttJBoxTool.iSeq > ttJBoxTool.iSeq
      BY bttJBoxTool.iSeq  
      :

    ihTbAdd:CHECKED = YES.

    ASSIGN ihFillIn:SCREEN-VALUE  = bttJBoxTool.hMenuFillIn:SCREEN-VALUE
           ihFillIn:PRIVATE-DATA  = bttJBoxTool.hMenuFillIn:PRIVATE-DATA
           ihFilter:PRIVATE-DATA  = bttJBoxTool.hMenuFilter:PRIVATE-DATA
           ihFillIn = bttJBoxTool.hMenuFillIn
           ihFilter = bttJBoxTool.hMenuFilter
           ihTbAdd  = bttJBoxTool.hMenuToggle
           ix = ix + 1
           .

    ihTbAdd:CHECKED = NO.

  END.

  ASSIGN ihFillIn:SCREEN-VALUE = "<label>"
         ihFillIn:PRIVATE-DATA = ""
         ihFillIn:HIDDEN       = YES
         ihFilter:PRIVATE-DATA = ""
         ihFilter:HIDDEN       = YES
         .

END.
*/

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateImage C-Win 
FUNCTION ValidateImage RETURNS CHARACTER
  ( INPUT icFileName    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix      AS INT NO-UNDO.
DEF VAR cTarget AS CHAR NO-UNDO.

IF icFileName = "" THEN RETURN "".

IF icFileName MATCHES "*bmp" OR icFileName MATCHES "*ico" OR icFileName MATCHES "*.png"  OR icFileName MATCHES "*.gif"
   THEN DO WITH FRAME {&FRAME-NAME}:
  cTarget = ENTRY(NUM-ENTRIES(icFileName,"\"),icFileName,"\").
  IF SEARCH(cTarget) = ? THEN DO ix = 1 TO 4:
    cTarget = ENTRY(NUM-ENTRIES(icFileName,"\") - ix,icFileName,"\") + "\" + cTarget.
    IF SEARCH(cTarget) NE ? THEN LEAVE.
  END.
  IF SEARCH(cTarget) = ? THEN DO:
    MESSAGE "Invalid image: " icFileName SKIP "(must be in PROPATH)"
             VIEW-AS ALERT-BOX ERROR.
    RETURN "".
  END.    
  RETURN cTarget.
END.
ELSE 
  MESSAGE "Invalid image file type: Must be bmp, gif or ico"
          VIEW-AS ALERT-BOX ERROR.
  
RETURN "".
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

