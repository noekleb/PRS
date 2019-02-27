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
DEF INPUT PARAM ihParent       AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrowse       AS HANDLE NO-UNDO. /* browse or query */
DEF INPUT PARAM ihAccumButton  AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR bOk              AS LOG    NO-UNDO.

DEF VAR hBrowse          AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hWinToolbar      AS HANDLE NO-UNDO.
DEF VAR hSaveFilterMenu  AS HANDLE NO-UNDO.

DEF VAR hDistinctOverlay  AS HANDLE NO-UNDO.
DEF VAR hAccumOverlay     AS HANDLE NO-UNDO.
DEF VAR hDistinct         AS HANDLE NO-UNDO.
DEF VAR hAccum            AS HANDLE NO-UNDO.
DEF VAR hPinButton        AS HANDLE NO-UNDO.
DEF VAR bValChng          AS LOG    NO-UNDO.

DEF VAR rRepos            AS ROWID  NO-UNDO.
DEF VAR iMaxSeq           AS INT    NO-UNDO INIT -1000.

DEF VAR cBrowseColumn     AS CHAR   NO-UNDO.
DEF VAR cSelAccumFlds     AS CHAR   NO-UNDO.

DEF VAR cAccumDataTypes       AS CHAR   NO-UNDO INIT "DECIMAL".
DEF VAR cDistinctDataTypes    AS CHAR   NO-UNDO INIT "CHARACTER,DATE,LOGICAL".
DEF VAR cAvailAccumFields     AS CHAR   NO-UNDO.
DEF VAR cAvailDistinctColumns AS CHAR   NO-UNDO.
DEF VAR cActivePinButton      AS CHAR   NO-UNDO.
DEF VAR cPassivePinButton     AS CHAR   NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectWinToolbar rectToolbar rectBrowse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewTotals C-Win 
FUNCTION ViewTotals RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58.2 BY 18.1.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectWinToolbar AT ROW 1.33 COL 45.6
     rectToolbar AT ROW 1.33 COL 2
     rectBrowse AT ROW 2.67 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60 BY 20.


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
         TITLE              = "Akkumuler"
         HEIGHT             = 20
         WIDTH              = 60.2
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 142.6
         VIRTUAL-HEIGHT     = 52.38
         VIRTUAL-WIDTH      = 142.6
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Akkumuler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Akkumuler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.

  PUBLISH "EndJBoxEvent" (?,ihParent,?,"JBoxWindowClose").
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
  RUN InitializeObject.

  IF THIS-PROCEDURE:CURRENT-WINDOW:ICON = "" THEN
    THIS-PROCEDURE:CURRENT-WINDOW:LOAD-ICON("ico\comp%.ico") NO-ERROR.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DEF VAR hColumn          AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").

  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearAccumRecord C-Win 
PROCEDURE ClearAccumRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE    = NO
         hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE = NO
         hBuffer:BUFFER-FIELD("SortType"):BUFFER-VALUE = NO         
         hBuffer:BUFFER-FIELD("Total"):BUFFER-VALUE    = ""  
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE      = hBuffer:BUFFER-FIELD("OrgSeq"):BUFFER-VALUE
         .
  IF R-INDEX(hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE," (") > 0 THEN
    hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE = SUBSTR(hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE,1,
                                                              R-INDEX(hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE," (")).
  hQuery:GET-NEXT().
END.
hQuery:GET-FIRST().

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

/* PUBLISH "DynAccumDone" (ihBrowse,"clear").  */

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
DEF VAR hColumn          AS HANDLE NO-UNDO.
DEF VAR cCurrToggle      AS CHAR   NO-UNDO.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
IF NOT bValChng THEN
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
DYNAMIC-FUNCTION("setAttribute",hAccumOverlay,"hideframewhendeleteobject","yes").
DYNAMIC-FUNCTION("DeleteObject",hAccumOverlay).
DYNAMIC-FUNCTION("setAttribute",hDistinctOverlay,"hideframewhendeleteobject","yes").
DYNAMIC-FUNCTION("DeleteObject",hDistinctOverlay).

RUN SUPER.

IF hBuffer:AVAIL THEN DO:
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hDistinctOverlay).
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hAccumOverlay).

  IF cAvailAccumFields NE "" OR cAvailDistinctColumns NE "" THEN DO:
    IF CAN-DO(cAvailAccumFields,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN DO:
      hAccumOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                        hBrowse,                    
                        "Accum",                   
                        "Accum",                   
                        "").
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hAccumOverlay,"Accum"). /* Link the dropdown to the browse with column-name info */
      ASSIGN hAccumOverlay:CHECKED  = hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE 
             hAccumOverlay:MODIFIED = FALSE
             hAccumOverlay:BGCOLOR  = hAccum:BGCOLOR
             cCurrToggle            = STRING(hAccumOverlay)
             NO-ERROR.
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hAccumOverlay)).
    END.
    ELSE DO:
      hDistinctOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                        hBrowse,                    
                        "Distinct",                   
                        "Distinct",                   
                        "").
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hDistinctOverlay,"Distinct"). /* Link the dropdown to the browse with column-name info */
      ASSIGN hDistinctOverlay:CHECKED  = hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE
             hDistinctOverlay:MODIFIED = FALSE
             hDistinctOverlay:BGCOLOR  = hDistinct:BGCOLOR
             cCurrToggle               = STRING(hDistinctOverlay)
             NO-ERROR.
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hDistinctOverlay)).
    END.
  END.
  ELSE DO:
    IF CAN-DO(cAccumDataTypes,STRING(hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE)) THEN DO:
      hAccumOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                        hBrowse,                    
                        "Accum",                   
                        "Accum",                   
                        "").
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hAccumOverlay,"Accum"). /* Link the dropdown to the browse with column-name info */
      ASSIGN hAccumOverlay:CHECKED  = hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE 
             hAccumOverlay:MODIFIED = FALSE
             hAccumOverlay:BGCOLOR  = hAccum:BGCOLOR
             cCurrToggle            = STRING(hAccumOverlay)
             NO-ERROR.
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hAccumOverlay)).
    END.
    ELSE DO:
      hDistinctOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                        hBrowse,                    
                        "Distinct",                   
                        "Distinct",                   
                        "").
      DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hDistinctOverlay,"Distinct"). /* Link the dropdown to the browse with column-name info */
      ASSIGN hDistinctOverlay:CHECKED  = hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE
             hDistinctOverlay:MODIFIED = FALSE
             hDistinctOverlay:BGCOLOR  = hDistinct:BGCOLOR
             cCurrToggle               = STRING(hDistinctOverlay)
             NO-ERROR.
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hDistinctOverlay)).
    END.
  END.

  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"currbrowsetoggles",cCurrToggle).

IF NOT bValChng THEN DO:
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
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
  ENABLE rectWinToolbar rectToolbar rectBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DEF VAR hSubMenuAdv           AS HANDLE NO-UNDO.
DEF VAR cAccumFields          AS CHAR   NO-UNDO.
DEF VAR cDistinctFields       AS CHAR   NO-UNDO.
DEF VAR cAccumSortFields      AS CHAR   NO-UNDO.
DEF VAR cDescAccumSortFields  AS CHAR   NO-UNDO.
DEF VAR cAllAccumFields       AS CHAR   NO-UNDO.
DEF VAR cDataType             AS CHAR   NO-UNDO.
DEF VAR iy                    AS INT    NO-UNDO.
DEF VAR cKeepAccumOpen        AS CHAR   NO-UNDO.
DEF VAR iNumViewRows          AS INT    NO-UNDO.
DEF VAR bKeep                 AS LOG    NO-UNDO.
DEF VAR cCurrViewFields       AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cActivePinButton  = DYNAMIC-FUNCTION("getAttribute",SESSION,"ActivePinButton")
         cPassivePinButton = DYNAMIC-FUNCTION("getAttribute",SESSION,"PassivePinButton").
  IF cActivePinButton  = "" THEN cActivePinButton = "gif\pushout.gif".
  IF cPassivePinButton = "" THEN cPassivePinButton = "gif\pushin.gif".

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             "MoveUp;"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Flytt &opp" ELSE "Move &up")
                           + ",MoveDown;"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Flytt &ned" ELSE "Move &down")
                           + ",ClearAccum;"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "&Blank" ELSE "Clear")
                           + ",StartAccum;&OK"
                             ,"maxborder,enable").
    
  cKeepAccumOpen = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepaccumopen").
  DYNAMIC-FUNCTION("DeleteObject",hBrowse).
  DYNAMIC-FUNCTION("DeleteObject",hDistinctOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hAccumOverlay).
  rRepos = ?.

  IF NOT DYNAMIC-FUNCTION("Scandinavian") THEN THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Accumulate".

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",rectBrowse:HANDLE,1,"",
                             "temp-table"
                           + ";+ColumnLabel|CHARACTER|x(50)||" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Felt"   ELSE "Field") 
                           + ";+Distinct|LOGICAL|*/||"         + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Nivå"   ELSE "Distinct") 
                           + ";+Accum|LOGICAL|*/||"            + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Summér" ELSE "Accumulate")
                           + ";+Total|CHARACTER|x(25)||Total"
                           + ";+!ColumnName|CHARACTER" 
                           + ";+!DataType|CHARACTER" 
                           + ";+!Format|CHARACTER" 
                           + ";!+Seq|INTEGER" 
                           + ";+!OrgSeq|INTEGER" 
                           + ";+!SortType|INTEGER"  /* 0: No sorting, 1: ASC, 2: DESC */
                            ,"where false"
                            ,"").

  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"ColumnLabel,Distinct,Accum,Total").

  DYNAMIC-FUNCTION("NewMenuBand",
                   hBrowse,
                   "Excel;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Eksporter til Excel" ELSE "Export to Excel"),
                   "").

  ASSIGN hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 160
         hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 40
         hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 45
         hBrowse:GET-BROWSE-COLUMN(4):VISIBLE      = NO
         hDistinct                                 = hBrowse:GET-BROWSE-COLUMN(2)
         hAccum                                    = hBrowse:GET-BROWSE-COLUMN(3)
         hBuffer                                   = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
         hQuery                                    = hBrowse:QUERY
         cDistinctFields                           = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctcolumns")
         cAccumFields                              = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumfields")
         cSelAccumFlds                             = cAccumFields
         cAllAccumFields                           = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"allaccumfields")
         cAccumSortFields                          = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumsortfields")
         cDescAccumSortFields                      = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"descaccumsortfields")
         cAvailAccumFields                         = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"availaccumfields")
         cAvailDistinctColumns                     = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"availdistinctcolumns")
         cCurrViewFields                           = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"currviewfields")
         .
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumdatatypes") NE "" THEN
    cAccumDataTypes       = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumdatatypes").
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctdatatypes") NE "" THEN
    cDistinctDataTypes    = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctdatatypes").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF NOT CAN-DO(cCurrViewFields,ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN NEXT.

    ASSIGN cBrowseColumn = ihBrowse:GET-BROWSE-COLUMN(ix):NAME
           cDataType     = ihBrowse:GET-BROWSE-COLUMN(ix):DATA-TYPE
           bKeep         = NO.

    IF cAvailAccumFields NE "" AND CAN-DO(cAvailAccumFields,cBrowseColumn) THEN
      bKeep = YES.
    ELSE IF cAvailDistinctColumns NE "" AND CAN-DO(cAvailDistinctColumns,cBrowseColumn) THEN 
      bKeep = YES.
    ELSE IF cAvailAccumFields = "" AND CAN-DO(cAccumDataTypes,cDataType) THEN
      bKeep = YES.
    ELSE IF cAvailDistinctColumns = "" AND CAN-DO(cDistinctDataTypes,cDataType) THEN 
      bKeep = YES.

    IF NOT bKeep THEN NEXT.

    iNumViewRows = iNumViewRows + 1.

    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE  = cBrowseColumn
           hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE    = CAN-DO(cDistinctFields,cBrowseColumn)
           hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE       = CAN-DO(cAccumFields,cBrowseColumn)
           hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cBrowseColumn):DATA-TYPE
           hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cBrowseColumn):FORMAT
           hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cBrowseColumn):LABEL
/*                                                             + (IF CAN-DO(cAccumSortFields,cBrowseColumn) THEN                           */
/*                                                                 (IF CAN-DO(cDescAccumSortFields,cBrowseColumn) THEN " (v)" ELSE " (^)") */
/*                                                                ELSE "")                                                                 */
           hBuffer:BUFFER-FIELD("SortType"):BUFFER-VALUE    = IF CAN-DO(cDescAccumSortFields,cBrowseColumn) THEN 2
                                                              ELSE IF CAN-DO(cAccumSortFields,cBrowseColumn) THEN 1
                                                              ELSE 0
           .
    IF CAN-DO(cAllAccumFields,cBrowseColumn) THEN
      hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE            = LOOKUP(cBrowseColumn,cAllAccumFields) - 200.
    ELSE hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE         = iNumViewRows.

    hBuffer:BUFFER-FIELD("OrgSeq"):BUFFER-VALUE           = iNumViewRows.

    IF rRepos = ? AND hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE NE "" THEN 
      rRepos = hBuffer:ROWID.
  END.

  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepaccumopen",cKeepAccumOpen).

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querysort","Seq").
  RUN OpenQuery.

  IF rRepos NE ? THEN DO:
    hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
    hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
    APPLY "value-changed" TO hBrowse.
  END.

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             "Close;Exit,Pin;;;pinWindow;" + cPassivePinButton
                            ,"right,enable").  

  hPinButton     = DYNAMIC-FUNCTION("getEventWidget",hWinToolbar,"Pin","").
END.

SUBSCRIBE TO "RefreshAccumTotals" ANYWHERE.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,250,150).

THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = MIN(SESSION:HEIGHT-PIXELS - 105,70 + iNumViewRows * (hBrowse:ROW-HEIGHT-PIXELS + 4)) + 5.
APPLY "window-resized" TO {&WINDOW-NAME}.

APPLY "value-changed" TO hBrowse.
DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

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
DEF VAR rBuffer  AS ROWID NO-UNDO.
DEF VAR iSeq     AS INT   NO-UNDO.
DEF VAR iNextSeq AS INT   NO-UNDO.

IF NOT hBuffer:AVAIL THEN RETURN.

ASSIGN rBuffer = hBuffer:ROWID
       iSeq    = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE.

IF hQuery:GET-NEXT() THEN DO:
  ASSIGN iNextSeq = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
         .
  hQuery:GET-PREV().
  hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iNextSeq.

  hQuery:QUERY-OPEN().
  hQuery:REPOSITION-TO-ROWID(rBuffer).
  APPLY "value-changed" TO hBrowse.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO hBrowse.

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
DEF VAR rBuffer  AS ROWID NO-UNDO.
DEF VAR iSeq     AS INT   NO-UNDO.
DEF VAR iPrevSeq AS INT   NO-UNDO.

IF NOT hBuffer:AVAIL THEN RETURN.

ASSIGN rBuffer = hBuffer:ROWID
       iSeq    = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE.

IF hQuery:GET-PREV() THEN DO:
  ASSIGN iPrevSeq = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
         .
  hQuery:GET-NEXT().
  hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iPrevSeq.

  hQuery:QUERY-OPEN().
  hQuery:REPOSITION-TO-ROWID(rBuffer).
  APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PinWindow C-Win 
PROCEDURE PinWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepaccumopen") = "yes" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepaccumopen","").
  hPinButton:LOAD-IMAGE(cPassivePinButton).
  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = NO.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"keepaccumopen","yes").
  hPinButton:LOAD-IMAGE(cActivePinButton).
  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = YES.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshAccumTotals C-Win 
PROCEDURE RefreshAccumTotals :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrowse AS HANDLE NO-UNDO.

DEF VAR cField           AS CHAR NO-UNDO.
DEF VAR cFormat          AS CHAR NO-UNDO.
DEF VAR cDataType        AS CHAR NO-UNDO.

IF ihSourceBrowse NE ihBrowse THEN RETURN.

IF cSelAccumFlds NE "" THEN DO:
  IF NOT hBrowse:GET-BROWSE-COLUMN(4):VISIBLE THEN
    ASSIGN hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS - 80
           hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS - 120
           hBrowse:GET-BROWSE-COLUMN(4):VISIBLE = YES.

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    IF hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE THEN DO:
      ASSIGN cField    = hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE
             cFormat   = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cField):FORMAT
             cDataType = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cField):DATA-TYPE
             .
      IF INDEX(cFormat,">") > 1 THEN
        cFormat = SUBSTR(cFormat,1,INDEX(cFormat,">") - 1) + ">>>" + SUBSTR(cFormat,INDEX(cFormat,">")).
      ELSE IF INDEX(cFormat,"9") > 1 THEN
        cFormat = SUBSTR(cFormat,1,INDEX(cFormat,"9") - 1) + ">>>" + SUBSTR(cFormat,INDEX(cFormat,"9")).
      ELSE IF NOT cFormat BEGINS "-" THEN 
        cFormat = "->>" + cFormat.
      ELSE  
        cFormat = SUBSTR(cFormat,1,1) + ">>>" + SUBSTR(cFormat,2).

      hBuffer:BUFFER-FIELD("Total"):BUFFER-VALUE = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"statvalue" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)),cFormat) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
        hBuffer:BUFFER-FIELD("Total"):BUFFER-VALUE = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"statvalue" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE))) NO-ERROR.
    END.

    hQuery:GET-NEXT().
  END.
  hQuery:GET-FIRST().
  hBrowse:REFRESH().
  APPLY "value-changed" TO hBrowse.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE THEN
  hDistinct:BGCOLOR = 10.
IF hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE THEN
  hAccum:BGCOLOR = 12.

IF cAvailAccumFields NE "" AND CAN-DO(cAvailAccumFields,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN 
  ASSIGN 
    hAccum:FONT      = iFontWingdings
    hAccum:FORMAT    = CHR(254) + "/"  + CHR(168)
    hDistinct:FONT   = ?
    hDistinct:FORMAT = "*/".
ELSE IF cAvailDistinctColumns NE "" AND CAN-DO(cAvailDistinctColumns,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN 
  ASSIGN 
    hDistinct:FONT   = iFontWingdings
    hDistinct:FORMAT = CHR(254) + "/"  + CHR(168)
    hAccum:FONT      = ?
    hAccum:FORMAT    = "*/".
ELSE IF CAN-DO(cAccumDataTypes,STRING(hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE)) THEN 
  ASSIGN 
    hAccum:FONT      = iFontWingdings
    hAccum:FORMAT    = CHR(254) + "/"  + CHR(168)
    hDistinct:FONT   = ?
    hDistinct:FORMAT = "*/".
ELSE 
  ASSIGN
    hDistinct:FONT   = iFontWingdings
    hDistinct:FORMAT = CHR(254) + "/"  + CHR(168)
    hAccum:FONT      = ?
    hAccum:FORMAT    = "*/".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartAccumRecord C-Win 
PROCEDURE StartAccumRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      01.06.06: Sorting on multiple columns doesn't apply no more.   
------------------------------------------------------------------------------*/
DEF VAR cDistinctFields    AS CHAR   NO-UNDO.
DEF VAR cDescFields        AS CHAR   NO-UNDO.
DEF VAR cAccumSortFields   AS CHAR   NO-UNDO.
DEF VAR cAllSelectedFields AS CHAR   NO-UNDO.
DEF VAR cSortPrefix        AS CHAR   NO-UNDO.
DEF VAR iy                 AS INT    NO-UNDO.
DEF VAR cButton            AS CHAR   NO-UNDO.
DEF VAR hSortColumn        AS HANDLE NO-UNDO.
DEF VAR hCurrSearchField   AS HANDLE NO-UNDO.
DEF VAR hColumn            AS HANDLE NO-UNDO.

cSelAccumFlds = "".

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE THEN DO:
    cDistinctFields = cDistinctFields + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE + ",".
    IF NUM-ENTRIES(cAccumSortFields) < 4 THEN
      cAccumSortFields = cAccumSortFields + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE + ",".
  END.
  IF hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE THEN
    cSelAccumFlds = cSelAccumFlds + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE + ",".
  IF hBuffer:BUFFER-FIELD("Distinct"):BUFFER-VALUE OR hBuffer:BUFFER-FIELD("Accum"):BUFFER-VALUE THEN
    cAllSelectedFields = cAllSelectedFields + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE + ",".

  hBuffer:BUFFER-FIELD("Total"):BUFFER-VALUE = "".

  hQuery:GET-NEXT().
END.

ASSIGN cDistinctFields    = TRIM(cDistinctFields,",")
       cSelAccumFlds      = TRIM(cSelAccumFlds,",")
       cAccumSortFields   = TRIM(cAccumSortFields,",")
       cAllSelectedFields = TRIM(cAllSelectedFields,",")
       .

IF cAllSelectedFields = "" THEN DO:
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"tempTableHandle") = "" THEN
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"uselocaldata","").
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querysort") = "jbCountDistinct" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"querysort","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"querydesc","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stDbSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stSortColumn","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"1stSortColumnDesc","").
  END.
END.

DYNAMIC-FUNCTION("setAttribute",ihBrowse,"distinctcolumns",cDistinctFields).
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"accumfields",cSelAccumFlds).
/* IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"uselocaldata") = "yes" THEN  */
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"querystatfields",cSelAccumFlds).
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"accumsortfields",cAccumSortFields).
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"allaccumfields",cAllSelectedFields).

DYNAMIC-FUNCTION("setSortString",ihBrowse,cAccumSortFields).

IF cAllSelectedFields NE "" THEN
  PUBLISH "BeforeDynAccum" (ihBrowse,"accum").
ELSE 
  PUBLISH "BeforeDynAccum" (ihBrowse,"clear").

DYNAMIC-FUNCTION("setCurrentObject",ihBrowse).
RUN OpenQuery.

IF cAllSelectedFields NE "" THEN DO:
  ihBrowse:FIT-LAST-COLUMN = NO.
  DYNAMIC-FUNCTION("ViewCountDistinctColumn",ihBrowse,YES).
  IF NUM-ENTRIES(cSelAccumFlds) = 1 THEN DO:
    DYNAMIC-FUNCTION("ViewAverageColumn",ihBrowse,YES).
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"accumviewfields",cAllSelectedFields + ",jbCountDistinct,jbAverage").
  END.
  ELSE DYNAMIC-FUNCTION("setAttribute",ihBrowse,"accumviewfields",cAllSelectedFields + ",jbCountDistinct").

  DYNAMIC-FUNCTION("setBrowseColumns",ihBrowse,cAllSelectedFields + ",jbCountDistinct,jbAverage",YES).
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF NUM-ENTRIES(cAccumSortFields) = 1 AND cAccumSortFields = cBrowseColumn THEN
      hSortColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
    IF ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS > 400 THEN
      ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = 100.
  END.
  IF cAccumSortFields NE "" THEN
    DYNAMIC-FUNCTION("setBrowseSearchField",ihBrowse,hSortColumn).
  cButton = DYNAMIC-FUNCTION("getAttribute",SESSION,"activeaccumbutton").
  
  DYNAMIC-FUNCTION("ViewAverageColumn",ihBrowse,NUM-ENTRIES(cSelAccumFlds) = 1).

  PUBLISH "DynAccumDone" (ihBrowse,"accum").
  hColumn = ihBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.
ELSE DO:
  cButton = DYNAMIC-FUNCTION("getAttribute",SESSION,"passiveaccumbutton").
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"tempTableHandle") = "" THEN
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"uselocaldata","").
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"accumviewfields","").
  DYNAMIC-FUNCTION("setBrowseSearchField",ihBrowse,?).
  PUBLISH "DynAccumDone" (ihBrowse,"clear").
  DYNAMIC-FUNCTION("ViewCountDistinctColumn",ihBrowse,NO).
  DYNAMIC-FUNCTION("ViewAverageColumn",ihBrowse,NO).
  DYNAMIC-FUNCTION("setBrowseColumns",ihBrowse,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"currviewfields"),NO).
END.

IF VALID-HANDLE(ihAccumButton) AND ihAccumButton:TYPE = "button" THEN 
  ihAccumButton:LOAD-IMAGE(cButton).

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepaccumopen") = "yes" 
   OR cSelAccumFlds NE "" THEN DO:
  
  RUN RefreshAccumTotals (ihBrowse).
  APPLY "entry" TO hBrowse.
END.
ELSE DO:
  APPLY "close" TO THIS-PROCEDURE.
  APPLY "entry" TO ihBrowse.
END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseToggle C-Win 
PROCEDURE ValChngBrowseToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iFocusedRow AS INT NO-UNDO.
DEF VAR iSeq        AS INT NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

bValChng = YES.

RUN SUPER.

IF (VALID-HANDLE(hDistinctOverlay) AND hDistinctOverlay:CHECKED) OR 
   (VALID-HANDLE(hAccumOverlay) AND hAccumOverlay:CHECKED) THEN DO:
  ASSIGN iSeq         = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE
         iFocusedRow  = hBrowse:FOCUSED-ROW
         iMaxSeq      = iMaxSeq + 1
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iMaxSeq.
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
  bOk = hBuffer:FIND-FIRST("WHERE Seq GE " + STRING(iSeq - 1)) NO-ERROR.
  IF NOT bOk THEN
    bOk = hBuffer:FIND-FIRST("WHERE Seq GE " + STRING(iSeq - 2)) NO-ERROR.
  IF NOT bOk THEN
    bOk = hBuffer:FIND-LAST() NO-ERROR.

  IF bOk THEN DO:
    hBrowse:SET-REPOSITIONED-ROW(iFocusedRow,"always").
    hQuery:REPOSITION-TO-ROWID(hBuffer:ROWID).
    RUN DisplayRecord.
  END.
END.
ELSE IF (VALID-HANDLE(hDistinctOverlay) AND NOT hDistinctOverlay:CHECKED) OR 
        (VALID-HANDLE(hAccumOverlay) AND NOT hAccumOverlay:CHECKED) THEN
       hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE + 200.

IF VALID-HANDLE(hDistinctOverlay) THEN
  APPLY "entry" TO hDistinctOverlay.
ELSE IF VALID-HANDLE(hAccumOverlay) THEN
  APPLY "entry" TO hAccumOverlay.


bValChng = NO.
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewTotals C-Win 
FUNCTION ViewTotals RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  hBuffer:BUFFER-FIELD("Total"):BUFFER-VALUE = 
    DEC(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"statvalue" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)).

  hQuery:GET-NEXT().
END.
RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

