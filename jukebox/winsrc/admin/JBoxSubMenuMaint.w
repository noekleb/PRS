&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEFINE VARIABLE bOk              AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cPrimaryMenu     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBrwPrimary      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBrwPrimaryItems AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBrwSubmenuItems AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBrwSubmenu      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldMap        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hTbPrimary       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hTbSubmenu       AS HANDLE      NO-UNDO.
DEFINE VARIABLE ix               AS INTEGER     NO-UNDO.
DEFINE VARIABLE hParent          AS HANDLE      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwPrimaryMenu brwSubmenu brwPrimaryItems ~
brwSubmenuItems TbPrimaryItems TbSubmenuItems cmbMenu 
&Scoped-Define DISPLAYED-OBJECTS cmbMenu 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbMenu AS CHARACTER FORMAT "X(256)":U 
     LABEL "Link to menu" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "1","2"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE brwPrimaryItems
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 12.52.

DEFINE RECTANGLE brwPrimaryMenu
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 14.76.

DEFINE RECTANGLE brwSubmenu
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 14.76.

DEFINE RECTANGLE brwSubmenuItems
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 12.52.

DEFINE RECTANGLE TbPrimaryItems
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.

DEFINE RECTANGLE TbSubmenuItems
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8.2 BY .95.

DEFINE BUTTON btnSplitBarX  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1 BY 28.81
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbMenu AT ROW 1.38 COL 108.8 COLON-ALIGNED
     "Primary menu:" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 1.57 COL 4
     brwPrimaryMenu AT ROW 2.57 COL 1.6
     brwSubmenu AT ROW 2.57 COL 95.8
     brwPrimaryItems AT ROW 19 COL 1.6
     brwSubmenuItems AT ROW 19 COL 95.6
     TbPrimaryItems AT ROW 17.67 COL 2
     TbSubmenuItems AT ROW 17.67 COL 177.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 187.2 BY 30.62.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 56.2 ROW 2.62
         SIZE 75 BY 29.


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
         TITLE              = "Manage links from primary menu to other menus"
         HEIGHT             = 30.62
         WIDTH              = 187.2
         MAX-HEIGHT         = 31.48
         MAX-WIDTH          = 187.2
         VIRTUAL-HEIGHT     = 31.48
         VIRTUAL-WIDTH      = 187.2
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
ON END-ERROR OF C-Win /* Manage links from primary menu to other menus */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Manage links from primary menu to other menus */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cmbMenu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbMenu C-Win
ON VALUE-CHANGED OF cmbMenu IN FRAME DEFAULT-FRAME /* Link to menu */
DO:
  DYNAMIC-FUNCTION("setAttribute",hBrwSubmenu,"calcParamMenuBranch",SELF:SCREEN-VALUE).
  RUN InvokeMethod(hBrwSubmenu,"OpenQuery").
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
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN FillTreeView IN hParent NO-ERROR.
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  hParent = SOURCE-PROCEDURE.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeNavBrowseFillIn C-Win 
PROCEDURE BeforeNavBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

IF CAN-DO("cMenuNumber,cMenuLabel",ihFillIn:NAME) THEN
  obOk = DYNAMIC-FUNCTION("DoUpdate","JBoxMenu","Ignore","",
      hBrwPrimaryItems:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE,
      ihFillIn:NAME,
      ihFillIn:INPUT-VALUE
     ,YES).

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
  DISPLAY cmbMenu 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwPrimaryMenu brwSubmenu brwPrimaryItems brwSubmenuItems 
         TbPrimaryItems TbSubmenuItems cmbMenu 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR hSubMenuItemSeq AS HANDLE NO-UNDO.
DEF VAR hPrimaryItemSeq AS HANDLE NO-UNDO.
DEF VAR hPrimaryItemNum AS HANDLE NO-UNDO.
DEF VAR hPrimaryItemLab AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DEF VAR cMenuList AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN cmbMenu:DELIMITER = "|"
         cmbMenu:LIST-ITEM-PAIRS = "|"
         cMenuList = DYNAMIC-FUNCTION("getFieldList","JBoxMenu;cMenuLabel;iJBoxMenuId","WHERE cMenuType = 'menu' BY iJBoxMenuId")
         .
  IF NUM-ENTRIES(cMenuList,"|") > 2 THEN DO:
    DO ix = 3 TO NUM-ENTRIES(cMenuList,"|") BY 2:
      cmbMenu:ADD-LAST(ENTRY(ix,cMenuList,"|"),ENTRY(ix + 1,cMenuList,"|")).
    END.
    cmbMenu:DELETE(1).
    cPrimaryMenu = ENTRY(2,cMenuList,"|").
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"The program isn't relevant WHEN there IS only one main MENU","","").
    APPLY "CLOSE" TO THIS-PROCEDURE.
    RETURN.
  END.


  hBrwPrimary = DYNAMIC-FUNCTION("NewBrowse"
          ,brwPrimaryMenu:HANDLE
          ,100
          ,""
          ,"JBoxMenu"
           + ";+MenuBranch|CHARACTER|X(150)|menubranch(ROWID)|Submenu-list primary menu"
           + ";!iJBoxMenuId"
          ,"WHERE false"
          ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrwPrimary,"calcFieldProc","jbadmin_jboxmenu_browsecalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrwPrimary,"baseQuery","WHERE cMenuType = 'SUB-MENU'").
  DYNAMIC-FUNCTION("setAttribute",hBrwPrimary,"calcParamMenuBranch",cPrimaryMenu).

  hBrwPrimaryItems = DYNAMIC-FUNCTION("NewBrowse"
          ,brwPrimaryItems:HANDLE
          ,100
          ,"MULTIPLE"
          ,"JBoxMenuToMenu"
           + ";iSeq"
           + ";!iToMenuId;!iFromMenuId"
         + ",JBoxMenu"
           + ";cMenuType@1"
           + ";cMenuLabel@2"
           + ";cLaunch@3"
           + ";cMenuNumber"
          ,"WHERE false"
         + ",FIRST JBoxMenu NO-LOCK WHERE iJBoxMenuId = iFromMenuId"
          ,"").

  hBrwPrimaryItems:TOOLTIP = "Doubleclick to edit".
  DYNAMIC-FUNCTION("setSortString",hBrwPrimaryItems,"iSeq").
  DYNAMIC-FUNCTION("CreateParentLink",hBrwPrimaryItems,hBrwPrimary,"iToMenuId;iJBoxMenuId").
  DYNAMIC-FUNCTION("setAttribute",hBrwPrimaryItems,"enableOnDblClick","YES").

  hPrimaryItemSeq = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwPrimaryItems,"iSeq","iSeq"
                  ,"","",""
                  ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwPrimaryItems,hPrimaryItemSeq,"iSeq").

  hPrimaryItemNum = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwPrimaryItems,"cMenuNumber","cMenuNumber"
                  ,"","",""
                  ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwPrimaryItems,hPrimaryItemNum,"cMenuNumber").

  hPrimaryItemLab = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwPrimaryItems,"cMenuLabel","cMenuLabel"
                  ,"","",""
                  ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwPrimaryItems,hPrimaryItemLab,"cMenuLabel").

  hBrwSubmenu = DYNAMIC-FUNCTION("NewBrowse"
          ,brwSubmenu:HANDLE
          ,100
          ,""
          ,"JBoxMenu"
           + ";+MenuBranch|CHARACTER|X(150)|menubranch(ROWID)|Submenu-list"
           + ";!iJBoxMenuId"
          ,"WHERE false"
          ,"").
  DYNAMIC-FUNCTION("setAttribute",hBrwSubmenu,"calcFieldProc","jbadmin_jboxmenu_browsecalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrwSubmenu,"baseQuery","WHERE cMenuType = 'SUB-MENU'").

  hBrwSubmenuItems = DYNAMIC-FUNCTION("NewBrowse"
          ,brwSubmenuItems:HANDLE
          ,100
          ,"MULTIPLE"
          ,"JBoxMenuToMenu"
           + ";iSeq"
           + ";!iToMenuId;!iFromMenuId"
         + ",JBoxMenu"
           + ";cMenuType@1"
           + ";cMenuLabel@2"
           + ";cLaunch@3"
          ,"WHERE false"
         + ",FIRST JBoxMenu NO-LOCK WHERE iJBoxMenuId = iFromMenuId"
          ,"").

  DYNAMIC-FUNCTION("setSortString",hBrwSubmenuItems,"iSeq").
  DYNAMIC-FUNCTION("CreateParentLink",hBrwSubmenuItems,hBrwSubmenu,"iToMenuId;iJBoxMenuId").

  hSubMenuItemSeq = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwSubMenuItems,"iSeq","iSeq"
                  ,"","",""
                  ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwSubMenuItems,hSubMenuItemSeq,"iSeq").

  hTbPrimary = DYNAMIC-FUNCTION("NewToolBar"
          ,TbPrimaryItems:HANDLE
          ,"File"
          ,"Link;Link items to sub-menu;Link items to submenu;;bmp/links.bmp"
         + ",excel;Eksporter til E&xcel"
          ,"border").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrwPrimaryItems,hTbPrimary).

  hTbSubmenu = DYNAMIC-FUNCTION("NewToolBar"
          ,TbSubmenuItems:HANDLE
          ,"File"
          ,"UnLink;Delete link for selected items to sub-menu;Delete link for selected items to sub-menu;;gif/remove.gif"
         + ",excel;Eksporter til E&xcel"
          ,"border").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrwSubmenuItems,hTbSubmenu).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "brwPrimaryMenu,brwPrimaryItems,TbSubmenuItems," + hBrwPrimary:NAME + "," + hBrwPrimaryItems:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "brwPrimaryMenu,brwSubMenu," + hBrwSubmenu:NAME + "," + hBrwPrimary:NAME).

  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,
                  STRING(brwPrimaryMenu:HANDLE) + "," + STRING(brwSubmenu:HANDLE) + "," + STRING(cmbMenu:HANDLE) + ","
                  + STRING(hBrwPrimary) + "," + STRING(hBrwSubmenu) + ","
                  + STRING(hBrwPrimaryItems) + "," + STRING(brwPrimaryItems:HANDLE) + ","
                  + STRING(hBrwSubmenuItems) + "," + STRING(brwSubmenuItems:HANDLE)
                  ). 
END.

RUN InvokeMethod(hBrwPrimary,"OpenQuery").

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,500,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkRecord C-Win 
PROCEDURE LinkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrwSubmenu:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Confirm linking selected menu-items to " 
                    + hBrwSubmenu:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("MenuBranch"):BUFFER-VALUE,"","") = 1 THEN DO:
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrwPrimaryItems,"jbadmin_jboxmenu_copy_links.p",
                     STRING(hBrwSubmenu:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)).
    APPLY "VALUE-CHANGED" TO hBrwSubmenu.
  END.
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Target sub-menu not selected","","").

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnLinkRecord C-Win 
PROCEDURE UnLinkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrwSubmenu:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Confirm removal of link for selected menu-items from " 
                    + hBrwSubmenu:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("MenuBranch"):BUFFER-VALUE,"","") = 1 THEN DO:
    DYNAMIC-FUNCTION("ProcessSelectedRows",hBrwSubmenuItems,"jbadmin_jboxmenu_remove_links.p",
                     STRING(hBrwSubmenu:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)).
    APPLY "VALUE-CHANGED" TO hBrwSubmenu.
  END.
END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Sub-menu not selected","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF CAN-DO("brwPrimaryItems,brwSubmenuItems",icBrowseName) THEN
  ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 60
         ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 120
         ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 120
         .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

