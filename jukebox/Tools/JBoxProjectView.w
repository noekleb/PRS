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

DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.

DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR cTreeDragNode     AS CHAR   NO-UNDO.

DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.

DEF VAR hTempTable        AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.

DEF VAR hButQuery         AS HANDLE NO-UNDO. /* Ekstra query for å finne butikker til CL uten å måtte gå til DB */
DEF VAR hButBuffer        AS HANDLE NO-UNDO.

DEF VAR hPopupRoot        AS HANDLE NO-UNDO.
DEF VAR hPopupMenuBar     AS HANDLE NO-UNDO.
DEF VAR hPopupSubMenu     AS HANDLE NO-UNDO.
DEF VAR cNewType          AS CHAR   NO-UNDO.
DEF VAR hTreeViewPopup    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectTreeView 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitTreeMenu C-Win 
FUNCTION InitTreeMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectTreeView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectTreeView AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 60 BY 11.81.


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
         TITLE              = "JukeBox Project View"
         HEIGHT             = 11.76
         WIDTH              = 60.4
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
{JBoxTreeView.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.81
       FRAME DEFAULT-FRAME:WIDTH            = 60.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeBox Project View */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeBox Project View */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* JukeBox Project View */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
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

  DELETE OBJECT hQuery NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyFileName C-Win 
PROCEDURE CopyFileName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBuffer:AVAIL THEN
  CLIPBOARD:VALUE = hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE.
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
  ENABLE rectTreeView 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).

hTreeTableBuffer:EMPTY-TEMP-TABLE().
RUN emptyTree in hDynTreeView.
EMPTY TEMP-TABLE ttNodes.

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().


REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF cExpandNode = "" THEN
    cExpandNode = STRING(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE).

  RUN CreateNodes(
             hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE,                      /* Level */
             hBuffer:BUFFER-FIELD("cParentId"):BUFFER-VALUE,                   /* Parent */
             hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,                   /* Label */
             TRUE,
             FALSE,
             hBuffer:BUFFER-FIELD("cId"):BUFFER-VALUE                          /* Key value */
             ).
  hQuery:GET-NEXT().
END.
RUN BuildTree ("",""). /* Level, parent */
DYNAMIC-FUNCTION('setAutoSort' in hDynTreeView,FALSE).
RUN populateTree IN hDynTreeView (hTreeTable,"").
RUN OpenTreeNode.

DYNAMIC-FUNCTION("DoLockWindow",?).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      All linking is done in the TabChanged function 
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBuffer AS HANDLE NO-UNDO.

DEF VAR hPageObject AS HANDLE NO-UNDO.

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

  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "tvNodeEvent" IN hdynTreeView.
  SUBSCRIBE TO "tvOLEDrag"  IN hDynTreeView.

  hBuffer = ihBuffer.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH ttProjectFiles").

  InitTreeMenu().

  /*TreeView END*/

/*   hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).                                        */
/*                                                                                                           */
/*   /* X and Y limits for move of widget are not yet set for the window.                                    */
/*      Since we want the tabs to resize according to the window size these values must be provided here and */
/*      they must be exact the same as in setOrwWinSize (under) */                                           */
/*   DYNAMIC-FUNCTION("setMinXYmove",600,350).                                                               */
/*                                                                                                           */
/*   hPageObject = DYNAMIC-FUNCTION("addFolder" IN hTabFolder,1,"Butikk","ButikerView.w","").                */
/*   DYNAMIC-FUNCTION("setQuery" IN hPageObject,hQuery).                                                     */
/*   RUN InitializeObject IN hPageObject.                                                                    */
/*                                                                                                           */
/* /*   hPageObject = DYNAMIC-FUNCTION("addFolder" IN hTabFolder,2,"Programs","JBoxProgramBrw.w",""). */     */
/* /*   RUN InitializeObject IN hPageObject.                                                          */     */
/*                                                                                                           */
/*   DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).                                                          */


/*   hToolbar = DYNAMIC-FUNCTION("NewToolbar",       */
/*                              rectToolbar:HANDLE,  */
/*                              "File",              */
/*                              "undo,save,excel",   */
/*                              "maxborder").        */


/*   hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",      */
/*                              rectWinToolbar:HANDLE, */
/*                              "File",                */
/*                              "close;E&xit",         */
/*                              "right|enable").       */

/*   InitializePopupMenus(). */
  InitializeResize().
/*   DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).  */
END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
  bOK = hTreeTableBuffer:FIND-FIRST("WHERE node_key = '" + icNodeKey + "'") NO-ERROR.

  cCurrNode = icNodeKey.
  IF bOk THEN 
    hBuffer:FIND-FIRST("WHERE cId = '" + hTreeTableBuffer:BUFFER-FIELD('private_data'):BUFFER-VALUE + "'") NO-ERROR.

  /* myclick: Programmers click: */
  IF icEvent NE "myclick" THEN
    DYNAMIC-FUNCTION('ExpandNode',icNodeKey,TRUE).

END.
ELSE IF icEvent = "RIGHTCLICK" THEN DO:
  IF hBuffer:AVAIL THEN 
    hTreeViewFrame:POPUP-MENU = hTreeViewPopup.
  ELSE 
    hTreeViewFrame:POPUP-MENU = ?.
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
DEF VAR cDragCL       AS CHAR NO-UNDO.
DEF VAR cDropCL       AS CHAR NO-UNDO.
DEF VAR iDragLevel    AS INT NO-UNDO.
DEF VAR iDropLevel    AS INT NO-UNDO.
DEF VAR cDragButNamn  AS CHAR NO-UNDO.
DEF VAR cDropButNamn  AS CHAR NO-UNDO.
DEF VAR cDragParentId AS CHAR NO-UNDO.
DEF VAR cExtraMsg     AS CHAR NO-UNDO.


bOK = hTreeTableBuffer:FIND-FIRST("WHERE node_key = '" + icKey + "'") NO-ERROR.
IF bOK THEN DO:

  cDropCL = ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|").
  bOK = hBuffer:FIND-FIRST("WHERE Butik = " + ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")) NO-ERROR.
  IF bOk THEN 
    ASSIGN cDropButNamn = hBuffer:BUFFER-FIELD("ButNamn"):BUFFER-VALUE
           iDropLevel   = hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE
           .
  ELSE 
    ASSIGN cDropButNamn = hTreeTableBuffer:BUFFER-FIELD("node_label"):BUFFER-VALUE
           cDropCL      = "-999"
           .

  hTreeTableBuffer:FIND-FIRST("WHERE node_key = '" + cTreeDragNode + "'") NO-ERROR.
  bOK = hBuffer:FIND-FIRST("WHERE Butik = " + ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")) NO-ERROR.
  IF NOT bOK THEN RETURN FALSE.

  ASSIGN cDragCL       = ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")
         cDragButNamn  = hBuffer:BUFFER-FIELD("ButNamn"):BUFFER-VALUE
         cDragParentId = hBuffer:BUFFER-FIELD("iParentButik"):STRING-VALUE
         iDragLevel    = hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE
         .

  IF cDragCL = cDropCL 
     OR iDropLevel GE iDragLevel      /* Kan ikke flytte sentrallager under butikk */
     OR cDropCL    = cDragParentId
     OR (cDropCL   = "-999" AND INT(cDragParentId) = 0)
     THEN
    RETURN FALSE.

  hBuffer:FIND-FIRST("WHERE Butik = " + ENTRY(1,hTreeTableBuffer:BUFFER-FIELD("private_data"):BUFFER-VALUE,"|")) NO-ERROR.
  IF cDropCL = "-999" THEN
    cExtraMsg = "(Butikken blir endret til sentrallager)".

  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Bekreft flytting av " + CHR(10) 
                                    + cDragButNamn + CHR(10)
                                    + "til " + CHR(10)
                                    + cDropButNamn + CHR(10)
                                    + cExtraMsg,"","") NE 1 THEN RETURN FALSE.

  IF INT(cDragParentId) = 0 THEN  /* Butikken flyttes fra å være sentrallager til vanlig butikk */
    DYNAMIC-FUNCTION("DoUpdate","Butiker","",
                     "Butik",
                     cDragCL,
                     "clButikkNr,Sentrallager",
                     cDropCL + "|false",
                     TRUE).
  ELSE IF cDropCL = "-999" THEN   /* Butikken opprettes som nytt sentrallager */
    DYNAMIC-FUNCTION("DoUpdate","Butiker","",
                     "Butik",
                     cDragCL,
                     "clButikkNr,Sentrallager",
                     cDragCL + "|true",
                     TRUE).
  ELSE
    DYNAMIC-FUNCTION("DoUpdate","Butiker","",
                     "Butik",
                     cDragCl,
                     "clButikkNr",
                     cDropCL,
                     TRUE).

  cExpandNode = icKey.

  RUN FillTreeView.

END.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,600,350,0,0).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitTreeMenu C-Win 
FUNCTION InitTreeMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hMenuItem AS HANDLE NO-UNDO.

CREATE MENU hTreeViewPopup
ASSIGN POPUP-ONLY = TRUE
       TITLE      = "Button State".

CREATE MENU-ITEM hMenuItem
ASSIGN PARENT = hTreeViewPopup
       LABEL = "Copy file name"
       TRIGGERS:
         ON CHOOSE PERSISTENT RUN CopyFileName IN THIS-PROCEDURE.
        END TRIGGERS.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

