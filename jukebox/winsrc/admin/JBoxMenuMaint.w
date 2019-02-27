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

DEF VAR bOk               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.

DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.

DEF VAR hTempTable        AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.

DEF VAR hPopupRoot        AS HANDLE NO-UNDO.
DEF VAR hPopupMenuBar     AS HANDLE NO-UNDO.
DEF VAR hPopupSubMenu     AS HANDLE NO-UNDO.
DEF VAR cNewType          AS CHAR   NO-UNDO.
DEF VAR hMenuTypeWidget   AS HANDLE NO-UNDO.
DEF VAR hSequenceWidget   AS HANDLE NO-UNDO.
DEF VAR hProgramIdWidget  AS HANDLE NO-UNDO.
DEF VAR cMenuWidgetList   AS CHAR NO-UNDO.

DEF VAR cTreeDragNode     AS CHAR   NO-UNDO.
DEF VAR hPageObject       AS HANDLE NO-UNDO.

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
rectTreeView cmbMenuType 
&Scoped-Define DISPLAYED-OBJECTS cmbMenuType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DropNode C-Win 
FUNCTION DropNode RETURNS LOGICAL
  ( INPUT icKey AS CHAR )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEnabledFields C-Win 
FUNCTION setEnabledFields RETURNS LOGICAL
  ( INPUT icMenuType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidDropNode C-Win 
FUNCTION ValidDropNode RETURNS LOGICAL
  ( INPUT icDropId  AS CHAR,
    INPUT icDragId  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewCurrentTab C-Win 
FUNCTION ViewCurrentTab RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbMenuType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Menu type" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","MENUBAR","SUB-MENU","MENU-ITEM","RULE","PLACEHOLDER" 
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 99.6 BY 19.05.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12.2 BY .95.

DEFINE RECTANGLE rectTreeView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 19.05.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8 BY .95.

DEFINE BUTTON btnSplitBarX  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1 BY 19.05
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbMenuType AT ROW 1.33 COL 11 COLON-ALIGNED
     rectFolder AT ROW 2.67 COL 47.4
     rectWinToolbar AT ROW 1.33 COL 138.6
     rectToolbar AT ROW 1.33 COL 48.4
     rectTreeView AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146.8 BY 20.91.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 38.2 ROW 2.67
         SIZE 16.6 BY 19.05.


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
         HEIGHT             = 20.81
         WIDTH              = 146.8
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/menumain16.ico":U) THEN
    MESSAGE "Unable to load icon: ico/menumain16.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}
{JBoxTreeView.i}

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
  ViewCurrentTab().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cmbMenuType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbMenuType C-Win
ON VALUE-CHANGED OF cmbMenuType IN FRAME DEFAULT-FRAME /* Menu type */
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
    RETURN NO-APPLY.
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
  
  RUN enable_UI.

/*   &IF DEFINED(UIB_is_Running) NE 0 &THEN */
/*     RUN InitializeObject.                */
/*   &ENDIF                                 */
  RUN InitWindow.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON "window-resized" OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  ViewCurrentTab().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBuffer:AVAIL THEN RETURN.

IF DYNAMIC-FUNCTION("getCurrentTab" IN hTabFolder) = 1 THEN DO:
  setExpandNode(STRING(hBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE)).

  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Delete menu (branch)?","","") = 1 THEN DO:
    IF NOT DYNAMIC-FUNCTION("runproc","jbsetup_delmenustruct.p",STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      RETURN.
    END.
  END.
END.
ELSE RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentTab" IN hTabFolder) = 1 THEN 
  RUN FillTreeView.
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
RUN SUPER.

IF hBuffer:AVAIL THEN
  setEnabledFields(hMenuTypeWidget:SCREEN-VALUE).

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
  DISPLAY cmbMenuType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectFolder rectWinToolbar rectToolbar rectTreeView cmbMenuType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
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
DEF VAR hDummyTT AS HANDLE NO-UNDO.
DEF VAR bEntries AS LOG    NO-UNDO.

hTreeTableBuffer:EMPTY-TEMP-TABLE().
RUN emptyTree in hDynTreeView.
hTempTable:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().
EMPTY TEMP-TABLE ttNodes.

/* Here we pass the buffer for the already existing temp-table to for automatic fill. hDummyTT serves just as a placeholder for the return value */
hDummyTT = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                            ,"WHERE " 
                           + (IF cmbMenuType:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ? THEN 
                               "cMenuType = '" + cmbMenuType:SCREEN-VALUE + "'"
                              ELSE "true")
                            ,hTempTable:DEFAULT-BUFFER-HANDLE).
DELETE OBJECT hDummyTT NO-ERROR.

hQuery:QUERY-PREPARE("FOR EACH JBoxMenu BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF cExpandNode = "" THEN
    cExpandNode = STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).

  bEntries = YES.

  RUN CreateNodes(
             hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE,
             STRING(hBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE),
             hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE 
             + " (" + hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE + ")"
/*              + " (" + STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + ")" */
             + " (" + STRING(hBuffer:BUFFER-FIELD("iSeq"):BUFFER-VALUE) + ")"
/*              + " (" + STRING(hBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE) + ")" */
             ,
             TRUE,
             FALSE,
             STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
             ).
  hQuery:GET-NEXT().
END.
RUN BuildTree ("0",""). /* Level, parent */
DYNAMIC-FUNCTION('setAutoSort' in hDynTreeView,FALSE).
RUN populateTree IN hDynTreeView (hTreeTable,"").
RUN OpenTreeNode.

IF NOT bEntries THEN InitializePopupMenus("root").

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
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  /*Treeview START*/
  RUN dyntreeview.w PERSIST SET hDynTreeView.
  THIS-PROCEDURE:ADD-SUPER-PROC(hDynTreeView).
  hTreeViewFrame = DYNAMIC-FUNCTION("InitTreeView",rectTreeView:HANDLE,THIS-PROCEDURE).

  hTreeTable = DYNAMIC-FUNCTION("getTreeDataTable").
  hTreeTableBuffer = hTreeTable:DEFAULT-BUFFER-HANDLE.
  
  CREATE QUERY hTreeTableQuery.
  hTreeTableQuery:SET-BUFFERS(hTreeTableBuffer).
  hTreeTableQuery:QUERY-PREPARE("FOR EACH " + hTreeTableBuffer:NAME).

  ASSIGN  
    cImageOpen   = "bmp/folderopen.bmp"
    cImageClosed = "bmp/folderclosed.bmp"
    .
  
  DYNAMIC-FUNCTION('setOLEDrag',TRUE).
  DYNAMIC-FUNCTION('setOLEDrop',TRUE).

  SUBSCRIBE TO "tvOLEDrag"  IN hDynTreeView.
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "tvNodeEvent" IN hdynTreeView.

  /* Get the menu temp-table def. and create a query for it so the query can be linked to the fieldmap in the folder object.
     Note that the query is assigned the temp-table so it doesn't have to go to the server for the data definitions
     (it would not even be possible to get the same struct directly from the db) */
  hTempTable = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p","where false",?).
  hQuery = DYNAMIC-FUNCTION("NewQuery",0,"","JBoxMenu","where false",
                            "TEMP-TABLE|" + STRING(hTempTable)).
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hQuery,"uselocaldata","").

  /*TreeView END*/


  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be provided here and
     they must be exact the same as in setOrwWinSize (under) */
  DYNAMIC-FUNCTION("setMinXYmove",600,300). 

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,
                   "Menu details|JBoxMenuView.w",hQuery).
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  hPageObject = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,1).

/*   hPageObject = DYNAMIC-FUNCTION("addFolder" IN hTabFolder,1,"Menu details","JBoxMenuView.w",""). */
/*   DYNAMIC-FUNCTION("setQuery" IN hPageObject,hQuery).                                             */
/*   RUN InitializeObject IN hPageObject.                                                            */

  /* Grab field handles from the fieldmap that are not handled in default update (see SaveRecord).
     Remember that the buffer for hTempTable contains fields from the link table as well: */
  ASSIGN 
    hMenuTypeWidget  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getFieldMapWidgets",hBuffer,"cMenuType"))      /* Assigned by type of "new" operation */   
    hSequenceWidget  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getFieldMapWidgets",hBuffer,"iSeq"))           /* Belongs in the link table */
    hProgramIdWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getFieldMapWidgets",hBuffer,"iJBoxProgramId")) /* Show the program id if a program is linked */
    cMenuWidgetList  = DYNAMIC-FUNCTION("getWidgetsByLasso",DYNAMIC-FUNCTION("getFrameHandle" IN hPageObject),0,"") /* Used in setEnabledFields */
    .

  DYNAMIC-FUNCTION("setAttribute",hBuffer,"bufferextrafields","cMenuType,iJBoxProgramId").

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,DYNAMIC-FUNCTION("getPageFieldMap" IN hTabFolder,1)).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "File",
                             "delete,undo,save,excel",
                             "maxborder").

  DYNAMIC-FUNCTION("createObjectLink",hToolbar,hQuery).
  DYNAMIC-FUNCTION("createObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageFieldMap" IN hTabFolder,1)).


  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "File",
                             "close;E&xit",
                             "right|enable").

  InitializeResize().
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
  hQuery:GET-FIRST().
  DYNAMIC-FUNCTION("setCurrentObject",hQuery).
  RUN DisplayRecord.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
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
RUN InvokeMethod(hBuffer,"NewRecord").
hMenuTypeWidget:SCREEN-VALUE = "MENUBAR".
setEnabledFields(hMenuTypeWidget:SCREEN-VALUE).


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
RUN InvokeMethod(hBuffer,"NewRecord").

hMenuTypeWidget:SCREEN-VALUE = "MENU-ITEM".
setEnabledFields(hMenuTypeWidget:SCREEN-VALUE).

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
DYNAMIC-FUNCTION("setCurrentObject",hToolbar).
RUN NewRecord.
hMenuTypeWidget:SCREEN-VALUE = "PLACEHOLDER".
setEnabledFields(hMenuTypeWidget:SCREEN-VALUE).

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
RUN InvokeMethod(hBuffer,"NewRecord").
hMenuTypeWidget:SCREEN-VALUE = "RULE".
setEnabledFields(hMenuTypeWidget:SCREEN-VALUE).

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
RUN InvokeMethod(hBuffer,"NewRecord").
hMenuTypeWidget:SCREEN-VALUE = "SUB-MENU".
setEnabledFields(hMenuTypeWidget:SCREEN-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTBstate    AS CHAR NO-UNDO.
DEF VAR cCurrMenuId AS CHAR NO-UNDO.
DEF VAR cSeq        AS CHAR NO-UNDO.

cTBstate = DYNAMIC-FUNCTION("getToolbarState",hToolbar).

/* Grab the sequence value for the structure file before save (which does an automatic re-display). 
   Also grab the non-updateable fields that should be assigned to the buffer: */
IF DYNAMIC-FUNCTION("getCurrentTab" IN hTabFolder) = 1 THEN DO:
  cSeq       = hSequenceWidget:SCREEN-VALUE.
  DYNAMIC-FUNCTION("setAttribute",hBuffer,"bufferextravalues",
                    hMenuTypeWidget:SCREEN-VALUE + "|" + hProgramIdWidget:SCREEN-VALUE).
  IF hBuffer:AVAIL THEN
    cCurrMenuId = STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).
END.

RUN SUPER.

/* Creation and update of the link table (structure table) is done here to demonstrate
   usage of update functionality from the clien. Normally the whole save operation
   would be done by calling a .p on the server, using the 'runproc' function in as-lib (see DeleteRecord proc) */
IF DYNAMIC-FUNCTION("getCurrentTab" IN hTabFolder) = 1 THEN DO:
  setExpandNode(STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)).

  /* Add parent link to current menu if adding anything but a menubar: */
  IF cTBstate = "new" AND hMenuTypeWidget:SCREEN-VALUE NE "MENUBAR" THEN DO:
    IF NOT DYNAMIC-FUNCTION("DoCreate","JBoxMenuToMenu" /* buffer */
                               ,""               /* validation / bl parameter  */
                               ,"iToMenuId,iFromMenuId,iSeq"  /* update fields */
                               ,cCurrMenuId + "|" 
                              + STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + "|" 
                              + cSeq /* Update values */
                               ,TRUE) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      RETURN.
    END.
  END.
  ELSE IF hMenuTypeWidget:SCREEN-VALUE NE "MENUBAR" THEN DO:
    /* Update the sequence number stored in the structure file: */
    IF NOT DYNAMIC-FUNCTION("DoUpdate","JBoxMenuToMenu"  /* buffer */
                               ,"" /* validation / bl parameter */
                               ,"iToMenuId,iFromMenuId" /* Key fileds - for retrieval */
                               ,STRING(hBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE) + "|" + 
                                STRING(hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)  /* Key values */
                               ,"iSeq"      /* Update fields */
                               ,cSeq        /* Update values */
                               ,TRUE) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      RETURN.
    END.
  END.
  RUN FillTreeView.
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
DEF VAR hDynMenu AS HANDLE NO-UNDO.

RUN JBoxDynMenu.w PERSIST SET hDynMenu.
RUN InitializeObject IN hDynMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).

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

IF CAN-DO("CLICK,MYCLICK",icEvent) THEN DO:

  cCurrNode = icNodeKey.
  bOK = hTreeTableBuffer:FIND-FIRST("WHERE node_key = '" + icNodeKey + "'") NO-ERROR.
  IF bOk THEN DO:
    bOK = hQuery:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE iJBoxMenuId = "
                                               + hTreeTableBuffer:BUFFER-FIELD('private_data'):BUFFER-VALUE) NO-ERROR.
    IF bOk THEN DO:
/*       hQuery:REPOSITION-TO-ROWID(hQuery:GET-BUFFER-HANDLE(1):ROWID).  */
/*       hQuery:GET-NEXT().                                              */
      DYNAMIC-FUNCTION("setCurrentObject",hQuery).
      RUN DisplayRecord.
    END.
  END.

  /* myclick: Programmers click: */
  IF icEvent NE "myclick" THEN  
    DYNAMIC-FUNCTION('ExpandNode',icNodeKey,TRUE).

END.
ELSE IF icEvent = "RIGHTCLICK" THEN DO:
  IF hBuffer:AVAIL THEN DO:
    InitializePopupMenus(hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE).
/*     IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "MENUBAR" THEN       */
/*       hTreeViewFrame:POPUP-MENU = hPopupMenuBar.                             */
/*     ELSE IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "SUB-MENU" THEN */
/*       hTreeViewFrame:POPUP-MENU = hPopupSubMenu.                             */
/*     ELSE hTreeViewFrame:POPUP-MENU = ?.                                      */
  END.
  ELSE IF cmbMenuType:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ? THEN 
    InitializePopupMenus("root").
/*     hTreeViewFrame:POPUP-MENU = hPopupRoot. */
  ELSE
    InitializePopupMenus("").
/*     hTreeViewFrame:POPUP-MENU = ?. */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvOLEDrag C-Win 
PROCEDURE tvOLEDrag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icMode AS CHAR NO-UNDO.
DEF INPUT PARAM icKey  AS CHAR NO-UNDO.
DEF INPUT PARAM icDill AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iiAllowedEffects AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAM iiState AS INT NO-UNDO.

IF icMode = "start" THEN 
  cTreeDragNode = icKey.
ELSE IF icMode = "drop" THEN 
  DropNode(icKey).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DropNode C-Win 
FUNCTION DropNode RETURNS LOGICAL
  ( INPUT icKey AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDragId       AS CHAR NO-UNDO.
DEF VAR cDropId       AS CHAR NO-UNDO.
DEF VAR iDragLevel    AS INT  NO-UNDO.
DEF VAR iDropLevel    AS INT  NO-UNDO.
DEF VAR cDragLabel    AS CHAR NO-UNDO.
DEF VAR cDropLabel    AS CHAR NO-UNDO.
DEF VAR cDragType     AS CHAR NO-UNDO.
DEF VAR cDropType     AS CHAR NO-UNDO.
DEF VAR cDragParentId AS CHAR NO-UNDO.
DEF VAR cExtraMsg     AS CHAR NO-UNDO.


bOK = hTreeTableBuffer:FIND-FIRST("WHERE node_key = '" + icKey + "'") NO-ERROR.
IF bOK THEN DO:

  cDropId = ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|").
  bOK = hBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")) NO-ERROR.
  IF bOk THEN 
    ASSIGN cDropLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           iDropLevel = hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE
           cDropType  = hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE
           .
  ELSE RETURN FALSE.

  hTreeTableBuffer:FIND-FIRST("WHERE node_key = '" + cTreeDragNode + "'") NO-ERROR.
  bOK = hBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")) NO-ERROR.
  IF NOT bOK THEN RETURN FALSE.

  ASSIGN cDragId       = ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")
         cDragLabel    = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
         cDragType     = hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE
         cDragParentId = hBuffer:BUFFER-FIELD("iParentMenuId"):STRING-VALUE
         iDragLevel    = hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE
         .

  IF cDragId = cDropId 
     OR cDropId    = cDragParentId
     OR CAN-DO("placeholder,rule,menu-item",cDropType)
     OR NOT ValidDropNode(cDropId,cDragId)
     THEN
    RETURN FALSE.

  hBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")) NO-ERROR.

  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Move " + CHR(10) + "  " 
                                    + cDragLabel + CHR(10)
                                    + "to " + CHR(10) + "  "
                                    + cDropLabel + CHR(10)
                                    + cExtraMsg,"","") NE 1 THEN RETURN FALSE.

  DYNAMIC-FUNCTION("DoUpdate","JBoxMenuToMenu","",
                   "iToMenuId,iFromMenuId",
                   cDragParentId + "|" + cDragId,
                   "iToMenuId",
                   cDropId,
                   TRUE).

  cExpandNode = icKey.

  RUN FillTreeView.

END.

RETURN TRUE. 

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
DELETE OBJECT hTreeViewFrame:POPUP-MENU NO-ERROR.

/* If there is no current menu items: */
IF icType = "root" THEN
  hPopupRoot =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "New;New;MenuBarRecord"
                      ,"").     

/* If current menu item is a menubar: */
ELSE IF icType = "menubar" THEN DO:
  hPopupMenuBar =  DYNAMIC-FUNCTION("NewMenuBand",
                    hTreeViewFrame,  /* parent widget */
                    "|New"
                    + ",Delete"
                    + ",Test"
                    ,"").     
  DYNAMIC-FUNCTION("NewMenuBand",
                    WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTreeViewFrame,"placeholder1")),  /* parent widget */
                    "Sub-menu;Sub-menu;MenuSubRecord"
                    + ",Menu-item;Menu-item;MenuItemRecord"
                    + ",Menubar;Menubar;MenuBarRecord"
                    + ",Rule"
                    + ",Placeholder;Placeholder;MenuPlaceHolderRecord"
                    ,"").     
END.

/* If current menu is a sub-menu: */
ELSE IF icType = "sub-menu" THEN DO:   
  hPopupSubMenu =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "|New"
                      + ",Delete"
                      ,"").     
  DYNAMIC-FUNCTION("NewMenuBand",
                      WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTreeViewFrame,"placeholder1")),  /* parent widget */
                      "Menu-item;Menu-item;MenuItemRecord"
                      + ",Sub-menu;Sub-menu;MenuSubRecord"
                      + ",Ruler;Rule;MenuRuleRecord"
                      ,"").     
END.
ELSE IF CAN-DO("menu-item,rule",icType) THEN
  hPopupSubMenu =  DYNAMIC-FUNCTION("NewMenuBand",
                      hTreeViewFrame,  /* parent widget */
                      "Delete"
                      ,"").     

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Note how the search combo (cmbMenuType) is set to make it follow the split-bar only
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setNoResizeX" , THIS-PROCEDURE:CURRENT-WINDOW, hTreeViewFrame, "hTreeView,frTreeView").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cmbMenuType").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cmbMenuType").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectTreeView,cmbMenuType").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").

  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(cmbMenuType:HANDLE) + "," +
                    DYNAMIC-FUNCTION("getToolBarHandles",hToolBar,"button,rule") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectTreeView:HANDLE,0,"frame,control-frame") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle")
                    ).

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,600,300,0,0).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEnabledFields C-Win 
FUNCTION setEnabledFields RETURNS LOGICAL
  ( INPUT icMenuType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget AS HANDLE NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(cMenuWidgetList):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cMenuWidgetList)).
  hWidget:SENSITIVE = TRUE.

  IF (hWidget:NAME = "cMenuType" OR hWidget:NAME = "iJBoxProgramId")
     OR (icMenuType = "MENUBAR" AND hWidget:NAME = "iSeq")
     OR (icMenuType NE "MENU-ITEM" AND hWidget:NAME MATCHES "*Launch*")
     OR (icMenuType NE "MENU-ITEM" AND hWidget:NAME = "cAccelerator")
     OR (icMenuType = "RULE" AND hWidget:NAME = "cMenuLabel")
     OR hWidget:NAME = "iJBoxMenuId"
     THEN 
    hWidget:SENSITIVE = FALSE.
END.
  
RETURN TRUE. 

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

/* IF iiTab = 3 THEN DO:                                                                                                                               */
/*   hCustBrowse = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1).                                                                                   */
/*   DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,3),DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1),"CustNum"). */
/*   APPLY "value-changed" TO hCustBrowse.                                                                                                             */
/* END.                                                                                                                                                */
/* ELSE                                                                                                                                                */
/*   DYNAMIC-FUNCTION("DeleteObjectLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,3),DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1)).           */

RETURN TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewCurrentTab C-Win 
FUNCTION ViewCurrentTab RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,0).
RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

