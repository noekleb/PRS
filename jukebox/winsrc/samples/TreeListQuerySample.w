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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/*&SCOPED-DEFINE AdvGuiWin*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{JukeBoxControlsGeneral.i}
{JukeBoxTreeList.i}
DEF VAR hControlsLibrary    AS HANDLE NO-UNDO.
DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.
DEF VAR hTreeList   AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PopulateGrid C-Win 
FUNCTION PopulateGrid RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbCustomer AT ROW 1.24 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.2 BY 11.38.

DEFINE FRAME frTreeList
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.71
         SIZE 109 BY 9.29.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 11.33
         WIDTH              = 112.4
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 138
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 138
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
ASSIGN FRAME frTreeList:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME frTreeList
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RUN ReleaseDll IN hControlsLibrary.
  DELETE PROCEDURE hControlsLibrary.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frTreeList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frTreeList C-Win
ON / OF FRAME frTreeList
DO:
/*     DEF VAR c AS CHAR.                                                      */
/*     DEF VAR m AS MEMPTR.                                                    */
/*     DEF VAR mb AS MEMPTR.                                                   */
/*     /*This DLL procedure returns a MEMPTR type that can contain             */
/*       any type of data.  */                                                 */
/*     RUN TreeListGetSelSubItemValue(hTreeAdv,3 /*First column*/, OUTPUT m).  */
/*                                                                             */
/*   /*  IF GET-LONG(M,1) = 1 THEN DO:                                         */
/*       SET-POINTER-VALUE(MB) = GET-LONG(M,5).                                */
/*       MESSAGE GET-STRING(mB,1).                                             */
/*     END.                                                                    */
/*                                                                             */
/*     ELSE  */                                                                */
/*       MESSAGE STRING(GET-LONG(M,1)).                                        */
/*                                                                             */

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
  RUN Controls.p PERSISTENT SET hControlsLibrary.
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
  ENABLE tbCustomer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME frTreeList IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frTreeList}
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
DEFINE VAR cViewFields AS CHAR NO-UNDO.
DEFINE VARIABLE hColumn AS INTEGER NO-UNDO.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  hQuery = DYNAMIC-FUNCTION("NewQuery"
        ,100
        ,""
        ,"Customer"
         + ";Custnum"
         + ";Name"
         + ";CreditLimit"
       + ",SalesRep"
         + ";SalesRep"
        ,"WHERE FALSE,FIRST SalesRep OF Customer NO-LOCK"
        ,"").

  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

  /*
  cViewFields = DYNAMIC-FUNCTION("getAttribute",hQuery,"allviewfields").
  DO ix = 1 TO NUM-ENTRIES(cViewFields):
    MESSAGE hBuffer:BUFFER-FIELD(ENTRY(ix,cViewFields)):NAME SKIP
            hBuffer:BUFFER-FIELD(ENTRY(ix,cViewFields)):LABEL SKIP
            hBuffer:BUFFER-FIELD(ENTRY(ix,cViewFields)):FORMAT SKIP
            hBuffer:BUFFER-FIELD(ENTRY(ix,cViewFields)):DATA-TYPE
/*             hBuffer:BUFFER-FIELD(ix):BUFFER-VALUE */
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  */
  /*First CREATE treelist control. */
  RUN TreeListCreate(FRAME frTreeList:HWND, OUTPUT hTreeList).
  DO ix = 1 TO hBuffer:NUM-FIELDS:
    RUN TreeListAddColumn(hTreeList,hBuffer:BUFFER-FIELD(ix):NAME,
                       {&GEN_NOIMAGE},
                       (IF hBuffer:BUFFER-FIELD(ix):NAME BEGINS "RowIdent" 
                           OR CAN-DO("RowCount,jbCountDistinct,jbAverage",hBuffer:BUFFER-FIELD(ix):NAME) 
                           THEN {&GEN_HIDDEN} ELSE {&GEN_VISIBLE}),
                       {&GEN_STRING-TYPE},
                       {&GEN_LEFT-JUSTIFY},
                       OUTPUT hColumn).
/*     MESSAGE hBuffer:BUFFER-FIELD(ix):NAME SKIP   */
/*              SKIP                                */
/*             hBuffer:BUFFER-FIELD(ix):FORMAT SKIP */
/*             hBuffer:BUFFER-FIELD(ix):DATA-TYPE   */
/*             hBuffer:ROWID                        */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.   */
  END.


  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
      ,tbCustomer:HANDLE
      ,"File"
      ,"Filter,Excel"
      ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hQuery).

  RUN InvokeMethod(hQuery,"OpenQuery").

/*   PopulateGrid().  */
  
  /* From JBoxObjLib.o, CreateBrowseAndEvents PROCEDURE 
  
CREATE BROWSE ohBrowse
  ASSIGN ROW-MARKERS      = FALSE
         SEPARATORS       = TRUE
         NAME             = ihRectangle:NAME + "_" + STRING(ohBrowse)
         FRAME            = ihRectangle:FRAME
         QUERY            = httTableQuery
         X                = ihRectangle:X + 1
         Y                = ihRectangle:Y + 1
         WIDTH-PIXELS     = ihRectangle:WIDTH-PIXELS - 2
         HEIGHT-PIXELS    = ihRectangle:HEIGHT-PIXELS - 2
         MULTIPLE         = IF CAN-DO(icProperties,"MULTIPLE") THEN TRUE ELSE FALSE
         ROW-MARKERS      = IF CAN-DO(icProperties,"ROW-MARKERS") THEN TRUE ELSE FALSE
         VISIBLE          = YES
         SENSITIVE        = TRUE
         COLUMN-RESIZABLE = TRUE
         READ-ONLY        = YES
         TRIGGERS:
           ON START-SEARCH      PERSISTENT RUN DoProcessEvent (ihProcedure,"start-search").
           ON OFF-END           PERSISTENT RUN DoProcessEvent (ihProcedure,"off-end").
           ON OFF-HOME          PERSISTENT RUN DoProcessEvent (ihProcedure,"off-home").
           ON VALUE-CHANGED     PERSISTENT RUN DoProcessEvent (ihProcedure,"value-changed").
           ON mouse-select-up   PERSISTENT RUN DoProcessEvent (ihProcedure,"row-entry").
           ON ROW-LEAVE         PERSISTENT RUN DoProcessEvent (ihProcedure,"row-leave").
           ON ROW-DISPLAY       PERSISTENT RUN DoProcessEvent (ihProcedure,"row-display").
           ON END-MOVE          PERSISTENT RUN DoProcessEvent (ihProcedure,"view-overlay").
           ON SCROLL-NOTIFY     PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
           ON CURSOR-RIGHT      PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
           ON CURSOR-LEFT       PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
           ON CURSOR-UP         PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
           ON CURSOR-DOWN       PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
           ON shift-CURSOR-UP   PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-cursor-up").
           ON shift-CURSOR-DOWN PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-cursor-down").
           ON PAGE-UP           PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
           ON PAGE-DOWN         PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
           ON shift-PAGE-UP     PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-page-up").
           ON shift-PAGE-DOWN   PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-page-down").
           ON HOME              PERSISTENT RUN DoProcessEvent (ihProcedure,"first-record").
           ON END               PERSISTENT RUN DoProcessEvent (ihProcedure,"last-record").
           ON shift-HOME        PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-home").
           ON shift-END         PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-end").
           ON ANY-PRINTABLE     PERSISTENT RUN DoProcessEvent (ihProcedure,"any-printable").
           ON DEFAULT-ACTION    PERSISTENT RUN DoProcessEvent (ihProcedure,"default-action").
           ON INSERT-MODE       PERSISTENT RUN DoProcessEvent (ihProcedure,"new-record").
           ON DELETE-CHARACTER  PERSISTENT RUN DoProcessEvent (ihProcedure,"delete-record").
           ON F5                PERSISTENT RUN DoProcessEvent (ihProcedure,"refresh-record"). 
           ON TAB               PERSISTENT RUN DoProcessEvent (ihProcedure,"tab").
           ON BACK-TAB          PERSISTENT RUN DoProcessEvent (ihProcedure,"back-tab").
           ON DROP-FILE-NOTIFY  PERSISTENT RUN DoProcessEvent (ihProcedure,"drop-file-notify").
         END TRIGGERS.
  */

END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iCount AS INT NO-UNDO.
DEF VAR hNode  AS INT NO-UNDO.
DEF VAR iSuccess AS INT NO-UNDO.

RUN SUPER.

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

   RUN TreeListAddLastString(hTreeList,STRING(hBuffer:buffer-field("CustNum"):BUFFER-VALUE),
                           {&GEN_NOIMAGE},
                           {&GEN_NOIMAGE},OUTPUT hNode).
    RUN TreeListSetSubItemString(hTreeList,hNode,
                              1,            /*Column Index.*/
                              STRING(hBuffer:buffer-field("NAME"):BUFFER-VALUE),   /*String data*/
                              {&GEN_NOIMAGE},OUTPUT iSuccess).
    RUN TreeListSetSubItemString(hTreeList,hNode,
                              2,            /*Column Index.*/
                              STRING(hBuffer:buffer-field("CreditLimit"):BUFFER-VALUE),   /*String data*/
                              {&GEN_NOIMAGE},OUTPUT iSuccess).
    RUN TreeListSetSubItemString(hTreeList,hNode,
                           3,       /*Column Index.*/
                           STRING(hBuffer:buffer-field("RowIdent1"):BUFFER-VALUE),    /*Integer Data*/
                           {&GEN_NOIMAGE},OUTPUT iSuccess).
/*   DO ix = 1 TO hBuffer:NUM-FIELDS:                */
/*     MESSAGE "Record: " iCount SKIP(1)             */
/*             hBuffer:BUFFER-FIELD(ix):NAME SKIP    */
/*             hBuffer:BUFFER-FIELD(ix):LABEL SKIP   */
/*             hBuffer:BUFFER-FIELD(ix):FORMAT SKIP  */
/*             hBuffer:BUFFER-FIELD(ix):BUFFER-VALUE */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.    */
/*   END.                                            */
  hQuery:GET-NEXT().
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PopulateGrid C-Win 
FUNCTION PopulateGrid RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix     AS INT NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.
DYNAMIC-FUNCTION("setAttribute",hQuery,"queryFilter","WHERE NAME BEGINS 'ab'").
RUN InvokeMethod(hQuery,"OpenQuery").

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  DO ix = 1 TO hBuffer:NUM-FIELDS:
    MESSAGE "Record: " iCount SKIP(1)
            hBuffer:BUFFER-FIELD(ix):NAME SKIP
            hBuffer:BUFFER-FIELD(ix):LABEL SKIP
            hBuffer:BUFFER-FIELD(ix):FORMAT SKIP
            hBuffer:BUFFER-FIELD(ix):BUFFER-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  hQuery:GET-NEXT().
END.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* To create the browse column (from JBoxObjLib, NewBrowse)
    hField = hBrowse:ADD-LIKE-COLUMN(httTableBuffer:BUFFER-FIELD(ix)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      MESSAGE PROGRAM-NAME(1) SKIP
              ERROR-STATUS:GET-MESSAGE(1) 
              VIEW-AS ALERT-BOX ERROR.

    ON END-RESIZE    OF hField PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"end-resize").

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

