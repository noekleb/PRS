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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFolder rectWinToolbar rectToolbar ~
rectTreeView fiSokCL btnSokCL tbRealCL 
&Scoped-Define DISPLAYED-OBJECTS fiSokCL tbRealCL 

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
DEFINE BUTTON btnSokCL 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE fiSokCL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Søk sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 99 BY 19.05.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12.2 BY .95.

DEFINE RECTANGLE rectTreeView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 18.33.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8 BY .95.

DEFINE VARIABLE tbRealCL AS LOGICAL INITIAL yes 
     LABEL "Kun sentr.lagere med tilh.butikker" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 2 BY 19.05
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiSokCL AT ROW 1.29 COL 18 COLON-ALIGNED
     btnSokCL AT ROW 1.29 COL 41.2
     tbRealCL AT ROW 2.43 COL 2
     rectFolder AT ROW 2.67 COL 48
     rectWinToolbar AT ROW 1.33 COL 138.6
     rectToolbar AT ROW 1.33 COL 48.4
     rectTreeView AT ROW 3.38 COL 2
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
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Vedlikehold distribusjonsenheter"
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
ON END-ERROR OF C-Win /* Vedlikehold distribusjonsenheter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold distribusjonsenheter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold distribusjonsenheter */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokCL C-Win
ON CHOOSE OF btnSokCL IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  cLookupValue = "ButNamn".

  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn;BuAdr;BuPonr;BuPadr;BuKon", "where Sentrallager", INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    fiSokCL:SCREEN-VALUE = cLookupValue.
    APPLY "return" TO fiSokCL.
  END.
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
&Scoped-define SELF-NAME fiSokCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSokCL C-Win
ON RETURN OF fiSokCL IN FRAME DEFAULT-FRAME /* Søk sentrallager */
DO:
  RUN FillTreeView.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbRealCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbRealCL C-Win
ON VALUE-CHANGED OF tbRealCL IN FRAME DEFAULT-FRAME /* Kun sentr.lagere med tilh.butikker */
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

  DELETE OBJECT hButQuery NO-ERROR.
  DELETE OBJECT hButBuffer NO-ERROR.

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
  {lng.i}
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hTabFolder) THEN RETURN.

RUN DisplayRecord IN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,DYNAMIC-FUNCTION("getCurrentTab" IN hTabFolder)) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditButikkListe C-Win 
PROCEDURE EditButikkListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cButikerRowIdList AS CHAR NO-UNDO.
DEF VAR cCurrRowIdList    AS CHAR NO-UNDO.
DEF VAR cCurrButik        AS CHAR NO-UNDO.
DEF VAR cDummy            AS CHAR NO-UNDO.

cCurrButik = STRING(hBuffer:BUFFER-FIELD("Butik"):BUFFER-VALUE).

hButQuery:QUERY-PREPARE("FOR EACH Butiker WHERE clButikkNr = " + cCurrButik).
hButQuery:QUERY-OPEN().
hButQuery:GET-FIRST().
REPEAT WHILE NOT hButQuery:QUERY-OFF-END:
  cButikerRowIdList = cButikerRowIdList + hButBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
  hButQuery:GET-NEXT().
END.
ASSIGN cButikerRowIdList = TRIM(cButikerRowIdList,",")
       cCurrRowIdList    = cButikerRowIdList
       .

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "Butiker;Butik;ButNamn;BuAdr;BuPonr;BuPadr;BuKon;!clButikkNr;!Sentrallager",
                    "where NOT Sentrallager",
                    INPUT-OUTPUT cButikerRowIdList,
                    "Butik",
                    INPUT-OUTPUT cDummy,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cButikerRowIdList):
    DYNAMIC-FUNCTION("DoUpdate","Butiker","",
                     "",                         /* Nøkkelfelt blank betyr ROWID! */
                     ENTRY(ix,cButikerRowIdList),
                     "clButikkNr",
                     cCurrButik,
                     FALSE). /* Vent med commit */
  END.
  DO ix = 1 TO NUM-ENTRIES(cCurrRowIdList):
    IF NOT CAN-DO(cButikerRowIdList,ENTRY(ix,cCurrRowIdList)) THEN
      DYNAMIC-FUNCTION("DoUpdate","Butiker","",
                       "",                         /* Nøkkelfelt blank betyr ROWID! */
                       ENTRY(ix,cCurrRowIdList),
                       "clButikkNr",
                       "0",
                       FALSE). /* Vent med commit */
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).

  RUN FillTreeView.
  cExpandNode = cCurrButik.
  RUN OpenTreeNode.
/*   RUN tvNodeEvent("myclick",cCurrButik). */
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
  DISPLAY fiSokCL tbRealCL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectFolder rectWinToolbar rectToolbar rectTreeView fiSokCL btnSokCL 
         tbRealCL 
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
DEF VAR hDummyTT   AS HANDLE NO-UNDO.
DEF VAR iButnr     AS INT NO-UNDO.
DEF VAR cQueryCrit AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).

hTreeTableBuffer:EMPTY-TEMP-TABLE().
RUN emptyTree in hDynTreeView.
hTempTable:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().
EMPTY TEMP-TABLE ttNodes.

DO WITH FRAME {&FRAME-NAME}:
  IF fiSokCL:SCREEN-VALUE NE "" THEN DO:
    iButNr = INT(fiSokCL:SCREEN-VALUE) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      cQueryCrit = "AND Butik = " + fiSokCL:SCREEN-VALUE.
    ELSE
      cQueryCrit = "AND ButNamn MATCHES '*" + fiSokCL:SCREEN-VALUE + "*'".
  END.
  /* Here we pass the buffer for the already existing temp-table to for automatic fill. hDummyTT serves just as a placeholder for the return value */
  hDummyTT = DYNAMIC-FUNCTION("getTempTable","butiker_getstruct.p"
                              ,"WHERE Sentrallager " 
                               + cQueryCrit
                               + " BY Butik"
                               + (IF tbRealCL:CHECKED THEN ";CL" ELSE ";")
                              ,hTempTable:DEFAULT-BUFFER-HANDLE).
  DELETE OBJECT hDummyTT NO-ERROR.
END.

RUN CreateNodes(
           0,
           "",
           "Sentrallagere",
           TRUE,
           FALSE,
           "0"
           ).


hQuery:QUERY-PREPARE("FOR EACH Butiker").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().


REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF cExpandNode = "" THEN
    cExpandNode = STRING(hBuffer:BUFFER-FIELD("Butik"):BUFFER-VALUE).

  RUN CreateNodes(
             hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE,                       /* Level */
             STRING(hBuffer:BUFFER-FIELD("iParentButik"):BUFFER-VALUE),                  /* Parent */
             STRING(hBuffer:BUFFER-FIELD("Butik"):BUFFER-VALUE)
             + " " + hBuffer:BUFFER-FIELD("ButNamn"):BUFFER-VALUE, /* Label */
             TRUE,
             FALSE,
             STRING(hBuffer:BUFFER-FIELD("Butik"):BUFFER-VALUE) 
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
/*     cImageOpen   = "bmp/folderopen.bmp"    */
/*     cImageClosed = "bmp/folderclosed.bmp"  */
    .
  
  DYNAMIC-FUNCTION('setOLEDrag',TRUE).
  DYNAMIC-FUNCTION('setOLEDrop',TRUE).

  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "tvNodeEvent" IN hdynTreeView.
  SUBSCRIBE TO "tvOLEDrag"  IN hDynTreeView.

  /* Get the menu temp-table def. and create a query for it so the query can be linked to the fieldmap in the folder object.
     Note that the query is assigned the temp-table so it doesn't have to go to the server for the data definitions
     (it would not even be possible to get the same struct directly from the db) */
  hTempTable = DYNAMIC-FUNCTION("getTempTable","butiker_getstruct.p","where false;",?).
  hQuery = DYNAMIC-FUNCTION("NewQuery",0,"","Butiker","where false",
                            "TEMP-TABLE|" + STRING(hTempTable)).
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  /* Brukes i prosedyre EditButikkListe: */
  CREATE QUERY hButQuery.
  CREATE BUFFER hButBuffer FOR TABLE hBuffer.
  hButQuery:SET-BUFFERS(hButBuffer).

  InitTreeMenu().
  RUN FillTreeView.

  /*TreeView END*/
  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be provided here and
     they must be exact the same as in setOrwWinSize (under) */
  DYNAMIC-FUNCTION("setMinXYmove",600,350). 

  /* TN 6/10-08 Denne gir feilmelding */
  /*hPageObject = DYNAMIC-FUNCTION("addFolder" IN hTabFolder,1,"Butikk","ButikerView.w","").*/
  /* Dette er korrekt oppsett. */
  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Butikk|ButikerView.w",hQuery).
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).


  /* TN 7/10-08 Skal ikke lenger kjøres.                */
  /*DYNAMIC-FUNCTION("setQuery" IN hPageObject,hQuery). */
  /*RUN InitializeObject IN hPageObject.                */

/*   hPageObject = DYNAMIC-FUNCTION("addFolder" IN hTabFolder,2,"Programs","JBoxProgramBrw.w",""). */
/*   RUN InitializeObject IN hPageObject.                                                          */

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).


  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "File",
                             "new,delete,undo,save,excel",
                             "maxborder").


  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "File",
                             "close;E&xit",
                             "right|enable").

/*   InitializePopupMenus(). */
  InitializeResize().
  hQuery:GET-FIRST().
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
  DYNAMIC-FUNCTION('initTranslation',THIS-PROCEDURE:CURRENT-WINDOW).
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
  IF bOk THEN DO:
    bOK = hBuffer:FIND-FIRST("WHERE Butik = "
                            + hTreeTableBuffer:BUFFER-FIELD('private_data'):BUFFER-VALUE) NO-ERROR.
    IF bOk THEN DO:
      DYNAMIC-FUNCTION("setCurrentObject",hQuery).
      RUN DisplayRecord.
    END.
  END.

  /* myclick: Programmers click: */
  IF icEvent NE "myclick" THEN
    DYNAMIC-FUNCTION('ExpandNode',icNodeKey,TRUE).

END.
ELSE IF icEvent = "RIGHTCLICK" THEN DO:
  IF hBuffer:BUFFER-FIELD("iParentButik"):BUFFER-VALUE = 0 THEN 
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
    Notes:  Note how the search combo (cmbMenuType) is set to make it follow the split-bar only
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setNoResizeX" , THIS-PROCEDURE:CURRENT-WINDOW, hTreeViewFrame, "hTreeView,frTreeView").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectTreeView,fiSokCL").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectTreeView,fiSokCL").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").

  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    DYNAMIC-FUNCTION("getToolBarHandles",hToolBar,"button,rule") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectTreeView:HANDLE,0,"frame,control-frame") + "," +
                    STRING(fiSokCL:HANDLE) + "," +
                    STRING(btnSokCL:HANDLE) + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle")
                    ).

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
       LABEL = "Rediger butikker"
       TRIGGERS:
         ON CHOOSE PERSISTENT RUN EditButikkListe IN THIS-PROCEDURE.
        END TRIGGERS.

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
DYNAMIC-FUNCTION("DeleteLinksFrom",hToolbar).
DYNAMIC-FUNCTION("DeleteLinksFrom",hQuery).

CASE iiTab:
  WHEN 1 THEN DO:
    DYNAMIC-FUNCTION("CreateObjectLink",hQuery,DYNAMIC-FUNCTION("getPageFieldMap" IN hTabFolder,1)).
    DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hQuery).
    DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageFieldMap" IN hTabFolder,1)).
  END.
/*   WHEN 2 THEN DO:                                                                                                  */
/*     DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2),hQuery,"iJBoxModuleId").  */
/*     DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2)).                */
/*     DYNAMIC-FUNCTION("TabChanged" IN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,2),0).                         */
/*   END.                                                                                                             */
END CASE.

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

