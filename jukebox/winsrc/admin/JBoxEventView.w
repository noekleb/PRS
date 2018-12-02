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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.

DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hWinToolbar       AS HANDLE NO-UNDO.
DEF VAR hBtnClose         AS HANDLE NO-UNDO.
DEF VAR hOpenDocBtn       AS HANDLE NO-UNDO.
DEF VAR hDocListMenu      AS HANDLE NO-UNDO.

DEF VAR hTempTable        AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT    NO-UNDO.

DEF VAR bRefresh          AS LOG    NO-UNDO.
DEF VAR cFilter           AS CHAR   NO-UNDO.
DEF VAR hVisSaksbeh       AS HANDLE NO-UNDO.

DEF VAR h_pure4gltv       AS HANDLE NO-UNDO.

DEF VAR iSelectNode       AS INT    NO-UNDO.

DEF VAR cLastCheckTime    AS CHAR   NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.

DEF VAR hDocList          AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Folder rectTreeView MeldingToolbar ~
rectWinToolbar 

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
DEFINE RECTANGLE Folder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 117.4 BY 23.1.

DEFINE RECTANGLE MeldingToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20.8 BY 1.

DEFINE RECTANGLE rectTreeView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31.6 BY 23.1.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8 BY .91.

DEFINE BUTTON btnSplitBarX  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1 BY 23
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Folder AT ROW 2.67 COL 34.6
     rectTreeView AT ROW 2.67 COL 2
     MeldingToolbar AT ROW 1.33 COL 34.8
     rectWinToolbar AT ROW 1.29 COL 143.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 151.6 BY 24.86.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 25.4 ROW 2.67
         SIZE 16.57 BY 23.08.


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
         TITLE              = "Hendelseslogg"
         HEIGHT             = 24.91
         WIDTH              = 151.6
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
IF NOT C-Win:LOAD-ICON("ico/bullet_triangle_blue.ico":U) THEN
    MESSAGE "Unable to load icon: ico/bullet_triangle_blue.ico"
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.86
       FRAME DEFAULT-FRAME:WIDTH            = 151.6.

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
ON END-ERROR OF C-Win /* Hendelseslogg */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Hendelseslogg */
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
  RUN MoveToTop IN hCurrTabProc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","super").
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/conttrigg.i hToolbar}

ON 'F5':U OF {&WINDOW-NAME} ANYWHERE DO:
  RUN RefreshRecord.
END.

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DynFilterDefined C-Win 
PROCEDURE DynFilterDefined :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.

IF iCurrTab = 1 AND hCurrTabQuery = ihBrowse THEN DO:
  cFilter =  DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryfilter"). 
  RUN FillTreeView.
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"executedynfilter","no").
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
  ENABLE Folder rectTreeView MeldingToolbar rectWinToolbar 
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
DEF VAR cReturn     AS CHAR   NO-UNDO.
DEF VAR cBullet     AS CHAR   NO-UNDO.
DEF VAR iCounter    AS INT    NO-UNDO.

RUN emptyTree in h_pure4gltv.

ASSIGN 
  iCounter    = 0
  iSelectNode = 0
  cReturn     = DYNAMIC-FUNCTION("getTransactionMessage").

IF cReturn NE "" THEN 
DO:
  IF cReturn BEGINS "FEIL" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,SUBSTR(cReturn,5),"","").
  ELSE 
    cLastCheckTime = cReturn.
END.

hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCounter = iCounter + 1.
  IF iSelectNode = 0 THEN iSelectNode = hBuffer:BUFFER-FIELD("EventGroupObj"):BUFFER-VALUE.
  RUN addNode IN h_pure4gltv (STRING(hBuffer:BUFFER-FIELD("EventGroupObj"):BUFFER-VALUE),
/*                               STRING(iCounter), */
                              "",
                              hBuffer:BUFFER-FIELD("EventGroupName"):BUFFER-VALUE,
                              cBullet,
                              "expanded") NO-ERROR.
  hQuery:GET-NEXT().
END.

DYNAMIC-FUNCTION('tvRefresh':U IN h_pure4gltv).

IF NOT bRefresh THEN
  DYNAMIC-FUNCTION("selectNode" IN h_pure4gltv,STRING(iSelectNode)).

IF iSelectNode = 0 AND iCurrTab = 1 THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).
  RUN OpenQuery.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDocListXYpos C-Win 
PROCEDURE getDocListXYpos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM oiXpos AS INT NO-UNDO.
DEF OUTPUT PARAM oiYpos AS INT NO-UNDO.

ASSIGN oiXpos = THIS-PROCEDURE:CURRENT-WINDOW:X + hOpenDocBtn:X + hOpenDocBtn:WIDTH-PIXELS
       oiYpos = THIS-PROCEDURE:CURRENT-WINDOW:Y + hOpenDocBtn:Y.

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
DEF VAR hSubMenuInnst AS HANDLE NO-UNDO.
DEF VAR cWinSettings  AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"containerwindow").
  
  /*Treeview START*/
  RUN Pure4GLTv.w PERSIST SET h_pure4gltv.
  THIS-PROCEDURE:ADD-SUPER-PROC(h_pure4gltv).
  hTreeViewFrame = DYNAMIC-FUNCTION("InitTreeView",rectTreeView:HANDLE,THIS-PROCEDURE).
  
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "tvNodeEvent" IN h_pure4gltv.

  hQuery = DYNAMIC-FUNCTION("NewQuery",0,"","JBoxEventGroup","where TRUE",
                            "").
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  /*TreeView END*/
  
  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",Folder:HANDLE).

  /* X and Y limits for move of widget are not yet set for the window.
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function)
     Here the values are set to not give any automatic move of widgets */
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200).
  
  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Hendelser|JBoxEventLineBrw.w",hQuery).
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    MeldingToolbar:HANDLE,
                    "Fil",
                    "filter;Vis &alle,Excel,refresh;;F5"
/*                   + ",print"  */
                    ,"maxborder").
  ASSIGN MeldingToolbar:Y = MeldingToolbar:Y - 1
         MeldingToolbar:HEIGHT-PIXELS = MeldingToolbar:HEIGHT-PIXELS + 2.
  
  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "Fil",
                             "Close;Avslutt&",
                             "right,enable").  
  hBtnClose = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hWinToolbar,"buttonClose")).
  InitializeResize().
  
  SUBSCRIBE TO "NewOrderCount"    ANYWHERE.
  SUBSCRIBE TO "DynFilterDefined" ANYWHERE.

  cWinSettings = DYNAMIC-FUNCTION("getCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN FillTreeView.
  /*  RUN JBoxTimer.w PERSIST ("RefreshTree",15000).*/

  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
/* hBrowse = DYNAMIC-FUNCTION("getLinkedObject",hToolbar,"browse","from"). */
/* APPLY "entry" TO hBrowse.                                               */
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
RUN FillTreeView.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshTree C-Win 
PROCEDURE RefreshTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("setShowHourGlass",FALSE).
bOk = DYNAMIC-FUNCTION("runproc","melding_sjekk_for_ny.p",cLastCheckTime,?).
DYNAMIC-FUNCTION("setShowHourGlass",TRUE).
IF bOk THEN
  PUBLISH "UserNotification" ("Ny melding i Message Collector",
                                  ENTRY(2,DYNAMIC-FUNCTION("getTransactionMessage"),"|"),
                                  THIS-PROCEDURE:FILE-NAME,
                                  "NewOrderCount",
                                  "",
                                  OUTPUT cReturn).

cLastCheckTime = ENTRY(1,DYNAMIC-FUNCTION("getTransactionMessage"),"|").

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
  bOK = hBuffer:FIND-FIRST("WHERE EventGroupObj = " + icNodeKey) NO-ERROR.
  IF bOk THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hQuery).
    RUN DisplayRecord.
  END.

  /* myclick: Programmers click: */
/*   IF icEvent NE "myclick" THEN                      */
/*     DYNAMIC-FUNCTION('ExpandNode',icNodeKey,TRUE).  */

END.
/* ELSE IF icEvent = "expand" THEN                          */
/*   cExpandList = cExpandList + icNodeKey + ",".           */
/* ELSE IF icEvent = "collapse" THEN                        */
/*   cExpandList = REPLACE(cExpandList,icNodeKey + ",",""). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Note how the search combo (cmbStatus) is set to make it follow the split-bar only
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setNoResizeX" , THIS-PROCEDURE:CURRENT-WINDOW, hTreeViewFrame, "hTreeView,frTreeView").
  DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,hBtnClose:NAME).
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectTreeView").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "MeldingToolbar").

  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    DYNAMIC-FUNCTION("getToolBarHandles",hToolbar,"") + "," +
                    STRING(rectTreeView:HANDLE)
                    + "," +
/*                     DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"behkomm_handle") + "," +                             */
                    DYNAMIC-FUNCTION("getWidgetsByLasso",Folder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor")
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
DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hQuery,"EventGroupObj").

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

DYNAMIC-FUNCTION("DeleteLinksFrom",hToolbar).
/* Denne fins ikke, så vidt jeg kan se: */
/* DYNAMIC-FUNCTION("setToolbarLink" IN hCurrTabProc,hToolbar). */
DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)).
  
DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION('getLinkedObject',DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),'FieldMap','From')).

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

