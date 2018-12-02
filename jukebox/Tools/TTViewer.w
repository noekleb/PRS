&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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

DEF VAR ix                AS INT NO-UNDO.
DEF VAR bOK               AS LOG NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwTT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SearchForWidget C-Win 
FUNCTION SearchForWidget RETURNS HANDLE
  ( INPUT icTargetType  AS CHAR,
    INPUT ihRelated     AS HANDLE,
    INPUT icRelatedType AS CHAR,
    INPUT ihStart       AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrwTT
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 105 BY 17.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrwTT AT ROW 1.23 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.4 BY 18.14.


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
         TITLE              = "Local temp-tables and buffers"
         HEIGHT             = 17.96
         WIDTH              = 107.57
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Local temp-tables and buffers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Local temp-tables and buffers */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Local temp-tables and buffers */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
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
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  SESSION:SET-WAIT-STATE("general").

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.
  RUN InitWindow.
  SESSION:SET-WAIT-STATE("").
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContentToExcelRecord C-Win 
PROCEDURE ContentToExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",hBuffer:BUFFER-FIELD("hTTbuffer"):BUFFER-VALUE,0).

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
  ENABLE rectBrwTT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DO WITH FRAM {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                                   rectBrwTT:HANDLE,
                                   1000,
                                   "",
                                   "temp-table"
                                   + ";+cName|CHARACTER|x(35)||Name"
                                   + ";+cType|CHARACTER|x(12)||Type"
                                   + ";+iRecordCount|INTEGER|>>>>>9||Record count"
                                   + ";+cContainer|CHARACTER|x(25)||Container"
                                   + ";+cProcedure|CHARACTER|x(25)||Procedure"
                                   + ";!hTTbuffer|HANDLE"
                                  ,"where false",
                                   "sort|cName").
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse,
                   "MultiSortBrowse;Sort on multiple columns"
                 + ",Excel"
                 + ",ContentToExcel;Content to Excel"
                  ,"").

  RUN RefreshRecord.

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,550,250,0,0).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowseRecord C-Win 
PROCEDURE RefreshBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RefreshRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Very much the same as Peter V Dam's getappsmeminfo2.p     
------------------------------------------------------------------------------*/
DEF VAR hBufferTT  AS HANDLE NO-UNDO.
DEF VAR hTT        AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hTTbuff    AS HANDLE NO-UNDO.
DEF VAR iRecords   AS INT    NO-UNDO.
DEF VAR hContainer AS HANDLE NO-UNDO.
DEF VAR cContName  AS CHAR   NO-UNDO.
DEF VAR cProc      AS CHAR   NO-UNDO.
DEF VAR hBrw       AS HANDLE NO-UNDO.

hBuffer:EMPTY-TEMP-TABLE().

hBufferTT = SESSION:FIRST-BUFFER.
DO WHILE VALID-HANDLE(hBufferTT):

  hBuffer:BUFFER-CREATE().
  ASSIGN hBuffer:BUFFER-FIELD("cName"):BUFFER-VALUE     = hBufferTT:NAME
         hBuffer:BUFFER-FIELD("hTTbuffer"):BUFFER-VALUE = hBufferTT
         .

  IF VALID-HANDLE(hBufferTT:TABLE-HANDLE) THEN DO:
    hBuffer:BUFFER-FIELD("cType"):BUFFER-VALUE = "Dyn TT".
    hTT = hBufferTT:TABLE-HANDLE.
    hContainer = DYNAMIC-FUNCTION("getObjectContainer",hTT).
    IF NOT VALID-HANDLE(hContainer) THEN DO:
      hContainer = DYNAMIC-FUNCTION("getObjectContainer",hBufferTT).
      IF NOT VALID-HANDLE(hContainer) THEN DO:
        hBrw = SearchForWidget("browse",hBufferTT,"buffer",SESSION).
        IF VALID-HANDLE(hBrw) THEN
          hContainer = DYNAMIC-FUNCTION("getObjectContainer",hBrw).
      END.
    END.
  END.
  ELSE DO:
    hBuffer:BUFFER-FIELD("cType"):BUFFER-VALUE = "Buffer".
    hContainer = DYNAMIC-FUNCTION("getObjectContainer",hBufferTT).
    IF NOT VALID-HANDLE(hContainer) THEN DO:
      hBrw = SearchForWidget("query",hBufferTT,"buffer",SESSION).
      IF VALID-HANDLE(hBrw) THEN
        hContainer = DYNAMIC-FUNCTION("getObjectContainer",hBrw).
    END.
  END.

  ASSIGN cContName = ""
         cProc  = "".

  IF CAN-QUERY(hContainer,"TITLE") THEN
    ASSIGN cContName = hContainer:TITLE
           cProc     = DYNAMIC-FUNCTION("getProcNameForWindow" IN hParent,hContainer).
  ELSE IF VALID-HANDLE(hContainer) THEN 
    ASSIGN cContName = DYNAMIC-FUNCTION("getObjectName",hContainer)
           cProc     = "".
  ELSE IF DYNAMIC-FUNCTION("getCacheBufferHandle",hBufferTT:NAME) = hBufferTT THEN
    ASSIGN cContName = "Cache"
           cProc     = "JBoxASlib.p".

  ASSIGN hBuffer:BUFFER-FIELD("cContainer"):BUFFER-VALUE = cContName
         hBuffer:BUFFER-FIELD("cProcedure"):BUFFER-VALUE = cProc
         iRecords = 0.

  CREATE QUERY hQuery.

  CREATE BUFFER hTTbuff FOR TABLE hBufferTT.

  hQuery:SET-BUFFERS(hTTbuff).
  hQuery:QUERY-PREPARE("for each " + hTTbuff:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    iRecords = iRecords + 1.

    hQuery:GET-NEXT().
  END.

  hBuffer:BUFFER-FIELD("iRecordCount"):BUFFER-VALUE = iRecords.

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
  DELETE OBJECT hTTbuff.

  hBufferTT = hBufferTT:NEXT-SIBLING.
END.


DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.


THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SearchForWidget C-Win 
FUNCTION SearchForWidget RETURNS HANDLE
  ( INPUT icTargetType  AS CHAR,
    INPUT ihRelated     AS HANDLE,
    INPUT icRelatedType AS CHAR,
    INPUT ihStart       AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget AS HANDLE NO-UNDO.

REPEAT WHILE VALID-HANDLE(ihStart):
  IF CAN-QUERY(ihStart,"first-child") AND ihStart:FIRST-CHILD NE ? THEN DO:
    hWidget = SearchForWidget(icTargetType,ihRelated,icRelatedType,ihStart:FIRST-CHILD).
    IF hWidget NE ? THEN RETURN hWidget.
  END.
  IF ihStart:TYPE = icTargetType THEN DO:
    CASE icTargetType:
      WHEN "browse" THEN DO:
        IF icRelatedType = "buffer" AND ihStart:QUERY:GET-BUFFER-HANDLE(1) = ihRelated THEN RETURN ihStart.
      END.
    END CASE.
  END.
  ihStart = ihStart:NEXT-SIBLING NO-ERROR.
END.

RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

