&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        JukeBox sample program for viewing orders pr salesrep.
                      Check the winsrc\admin\JboxMenuMaint.w for more advanced usage
                      with drag/drop and popup-menus

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:             Brynjar Hasle, brynjar@chemistry.no

  Created:            11.nov.2005

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
&SCOPED-DEFINE AdvGuiWin
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.

DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hWinToolBar       AS HANDLE NO-UNDO.

DEF VAR hTempTable        AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT    NO-UNDO.

DEF VAR bRefresh          AS LOG    NO-UNDO.

DEF VAR h_pure4gltv       AS HANDLE NO-UNDO.

DEF VAR cExpandList       AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnFromDate OrderFolder rectTreeView ~
cmbRegion dFrom dTo btnToDate 
&Scoped-Define DISPLAYED-OBJECTS cmbRegion dFrom dTo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

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
DEFINE BUTTON btnFromDate 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btnToDate 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE VARIABLE cmbRegion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Region" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 30.2 BY 1 NO-UNDO.

DEFINE VARIABLE dFrom AS DATE FORMAT "99/99/9999":U 
     LABEL "Ordered from" 
     VIEW-AS FILL-IN 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE dTo AS DATE FORMAT "99/99/9999":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE RECTANGLE OrderFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118.6 BY 23.1.

DEFINE RECTANGLE rectTreeView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 23.1.

DEFINE BUTTON btnSplitBarX  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1 BY 23
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnFromDate AT ROW 1.48 COL 72.8
     cmbRegion AT ROW 1.33 COL 7.8 COLON-ALIGNED
     dFrom AT ROW 1.38 COL 54 COLON-ALIGNED
     dTo AT ROW 1.38 COL 78.4 COLON-ALIGNED
     btnToDate AT ROW 1.48 COL 97
     OrderFolder AT ROW 2.67 COL 41.4
     rectTreeView AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 159.8 BY 24.86.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 33 ROW 2.67
         SIZE 16.57 BY 23.08.


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
         HEIGHT             = 24.91
         WIDTH              = 159.8
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.88
       FRAME DEFAULT-FRAME:WIDTH            = 159.86.

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


&Scoped-define SELF-NAME btnFromDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFromDate C-Win
ON CHOOSE OF btnFromDate IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (dFrom:HANDLE).
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
  RUN ResizeObject IN h_pure4gltv (rectTreeView:HEIGHT IN FRAME DEFAULT-FRAME,rectTreeView:WIDTH).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnToDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToDate C-Win
ON CHOOSE OF btnToDate IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (dTo:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbRegion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbRegion C-Win
ON VALUE-CHANGED OF cmbRegion IN FRAME DEFAULT-FRAME /* Region */
OR RETURN OF cmbRegion DO:
  RUN FillTreeView.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom C-Win
ON RETURN OF dFrom IN FRAME DEFAULT-FRAME /* Ordered from */
DO:
  RUN FillTreeView.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dTo C-Win
ON RETURN OF dTo IN FRAME DEFAULT-FRAME /* To */
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
ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").

  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  RUN ResizeObject IN h_pure4gltv (rectTreeView:HEIGHT IN FRAME {&FRAME-NAME},rectTreeView:WIDTH).
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalenderAction C-Win 
PROCEDURE CalenderAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF TRIM(dFrom:SCREEN-VALUE,"/") NE "" AND TRIM(dTo:SCREEN-VALUE,"/") NE "" THEN
    RUN FillTreeView.
END.

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
  DISPLAY cmbRegion dFrom dTo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnFromDate OrderFolder rectTreeView cmbRegion dFrom dTo btnToDate 
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

RUN emptyTree in h_pure4gltv.
hBuffer:EMPTY-TEMP-TABLE().

/* Here we pass the buffer for the already existing temp-table to for automatic fill. hDummyTT serves just as a placeholder for the return value */
hDummyTT = DYNAMIC-FUNCTION("getTempTable","salesrep_tree.p",
                             (IF cmbRegion:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ? THEN 
                                cmbRegion:SCREEN-VALUE
                              ELSE "") + "|" +
                             dFrom:SCREEN-VALUE + "|" + dTo:SCREEN-VALUE,
                             hTempTable:DEFAULT-BUFFER-HANDLE).
DELETE OBJECT hDummyTT NO-ERROR.

hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  RUN addNode IN h_pure4gltv (STRING(hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE),
                              IF hBuffer:BUFFER-FIELD("iParentIdx"):BUFFER-VALUE NE 0 THEN STRING(hBuffer:BUFFER-FIELD("iParentIdx"):BUFFER-VALUE) ELSE "",
                              hBuffer:BUFFER-FIELD("cNodeLabel"):BUFFER-VALUE,
                              "",
                              IF CAN-DO(cExpandList,STRING(hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE)) THEN "expanded" ELSE "") NO-ERROR.

  hQuery:GET-NEXT().
END.

DYNAMIC-FUNCTION("setAttribute",hCurrTabQuery,"basequery","where false").
DYNAMIC-FUNCTION("setCurrentObject",hCurrTabQuery).
RUN OpenQuery.

DYNAMIC-FUNCTION("setAttribute",hCurrTabQuery,"queryfilter",
                 (IF TRIM(dFrom:SCREEN-VALUE,"/") NE "" THEN
                    " AND OrderDate GE DATE('" + dFrom:SCREEN-VALUE + "')"
                  ELSE "") +
                 (IF TRIM(dTo:SCREEN-VALUE,"/") NE "" THEN
                    " AND OrderDate LE DATE('" + dTo:SCREEN-VALUE + "')"
                  ELSE "")).

DYNAMIC-FUNCTION('tvRefresh':U IN h_pure4gltv).

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

  /* There's no region table in sports so we need to extract them from Salesrep.
     Check the calc-procedure distinct_regions.p.. */
  ASSIGN cmbRegion:DELIMITER = "|"
         cmbRegion:LIST-ITEMS = " |" + DYNAMIC-FUNCTION("getFieldList","Salesrep"
                                                    + ";+CalcRegion|distinct_regions.p(ROWID)",
                                                      "where true").

  /*Treeview START*/
  RUN Pure4GLTv.w PERSIST SET h_pure4gltv.
  THIS-PROCEDURE:ADD-SUPER-PROC(h_pure4gltv).
  hTreeViewFrame = DYNAMIC-FUNCTION("InitTreeView",rectTreeView:HANDLE,THIS-PROCEDURE).
  
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "tvNodeEvent" IN h_pure4gltv.

  /* Get the menu temp-table def. and create a query for it so the query can be linked to the fieldmap in the folder object.
     Note that the query is assigned the temp-table so it doesn't have to go to the server for the data definitions
     (it would not even be possible to get the same struct directly from the db) */
  hTempTable = DYNAMIC-FUNCTION("getTempTable","salesrep_tree.p","nobody||",?).
  hQuery = DYNAMIC-FUNCTION("NewQuery",0,"","Salesrep","where false",
                            "TEMP-TABLE|" + STRING(hTempTable)).
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  /*TreeView END*/


  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",OrderFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Orders|OrderBrw.w",hQuery).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  InitializeResize().
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
  
  SUBSCRIBE TO "NewOrderCount" ANYWHERE.

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
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm ("") NO-ERROR.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewOrderCount C-Win 
PROCEDURE NewOrderCount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.

DEF VAR cCurrStatus   AS CHAR NO-UNDO.
DEF VAR cCurrSalesrep AS CHAR NO-UNDO.

ASSIGN cCurrStatus   = hBuffer:BUFFER-FIELD("OrderStatus"):BUFFER-VALUE
       cCurrSalesrep = hBuffer:BUFFER-FIELD("Salesrep"):BUFFER-VALUE
       bRefresh      = TRUE
       .
RUN FillTreeView.

bOk = hBuffer:FIND-FIRST("WHERE OrderStatus = '" + cCurrStatus + "' AND Salesrep = '" + cCurrSalesrep + "'") NO-ERROR.
IF bOk THEN
  DYNAMIC-FUNCTION("selectNode" IN h_pure4gltv,STRING(hBuffer:BUFFER-FIELD('iNodeIndex'):BUFFER-VALUE)).


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


IF CAN-DO("select",icEvent) THEN DO:

  cCurrNode = icNodeKey.
  bOK = hBuffer:FIND-FIRST("WHERE iNodeIndex = " + icNodeKey) NO-ERROR.
  IF bOk THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hQuery).
    RUN DisplayRecord.
  END.

  /* myclick: Programmers click: */
/*   IF icEvent NE "myclick" THEN                      */
/*     DYNAMIC-FUNCTION('ExpandNode',icNodeKey,TRUE).  */

END.
ELSE IF icEvent = "expand" THEN
  cExpandList = cExpandList + icNodeKey + ",".
ELSE IF icEvent = "collapse" THEN
  cExpandList = REPLACE(cExpandList,icNodeKey + ",","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Note how the search combo (cmbRegion) is set to make it follow the split-bar only
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setNoResizeX" , THIS-PROCEDURE:CURRENT-WINDOW, hTreeViewFrame, "hTreeView,frTreeView").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cmbRegion").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cmbRegion").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectTreeView,cmbRegion").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").

  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(cmbRegion:HANDLE) + "," +
                    STRING(dFrom:HANDLE) + "," +
                    STRING(btnFromDate:HANDLE) + "," +
                    STRING(dTo:HANDLE) + "," +
                    STRING(btnToDate:HANDLE) + "," +
                    STRING(rectTreeView:HANDLE) + "," +
/*                     DYNAMIC-FUNCTION("getWidgetsByLasso",rectTreeView:HANDLE,0,"frame,editor") + "," + */
                    DYNAMIC-FUNCTION("getWidgetsByLasso",OrderFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle")
                    ).

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,2000,1200,0,0).
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
IF iCurrTab NE 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hQuery,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab).

DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hQuery,"Salesrep,OrderStatus").

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

DYNAMIC-FUNCTION("setCurrentObject",hQuery).
RUN DisplayRecord.

RETURN TRUE.

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

