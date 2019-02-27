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
DEF INPUT PARAM ihQueryObject  AS HANDLE NO-UNDO. /* browse or query */
DEF INPUT PARAM ihFilterButton AS HANDLE NO-UNDO.
DEF INPUT PARAM ibDialog       AS LOG    NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR ix                    AS INT NO-UNDO.
DEF VAR bOk                   AS LOG NO-UNDO.
                              
DEF VAR hFilter               AS HANDLE NO-UNDO.
DEF VAR hBrowse               AS HANDLE NO-UNDO.
DEF VAR hBuffer               AS HANDLE NO-UNDO.
DEF VAR hToolbar              AS HANDLE NO-UNDO.
DEF VAR hWinToolbar           AS HANDLE NO-UNDO.
DEF VAR hSaveFilterMenu       AS HANDLE NO-UNDO.
                              
DEF VAR hOperatorOverlay      AS HANDLE NO-UNDO.
DEF VAR hValueOverlay         AS HANDLE NO-UNDO.
DEF VAR hFieldOpOverlay       AS HANDLE NO-UNDO.
DEF VAR hNewBuff              AS HANDLE NO-UNDO.
DEF VAR httCopy               AS HANDLE NO-UNDO.
DEF VAR hPinButton            AS HANDLE NO-UNDO.
DEF VAR cActivePinButton      AS CHAR   NO-UNDO.
DEF VAR cPassivePinButton     AS CHAR   NO-UNDO.
                              
DEF VAR hMenuItemAdv          AS HANDLE NO-UNDO.
DEF VAR hMenuItemViewQ        AS HANDLE NO-UNDO.
DEF VAR bAdv                  AS LOG    NO-UNDO.
DEF VAR rRepos                AS ROWID  NO-UNDO.
DEF VAR hFieldGroup           AS HANDLE NO-UNDO.
DEF VAR cBaseTable            AS CHAR   NO-UNDO.
                              
DEF VAR cDuplicateLabel       AS CHAR   NO-UNDO.
DEF VAR cGroupSelectedLabel   AS CHAR   NO-UNDO.
DEF VAR cDeGroupSelectedLabel AS CHAR   NO-UNDO.
DEF VAR cAllowCanDoOperator   AS CHAR   NO-UNDO.
DEF VAR cCurrOperator         AS CHAR   NO-UNDO.

DEF VAR cLogicalDropDown   AS CHAR   NO-UNDO.
DEF VAR cLogDropDownVal    AS CHAR   NO-UNDO INIT "||yes|yes|no|no".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillFromCopy C-Win 
FUNCTION FillFromCopy RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReduceFilter C-Win 
FUNCTION ReduceFilter RETURNS CHARACTER
  ( INPUT icFilterFields AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDuplicateGroups C-Win 
FUNCTION setDuplicateGroups RETURNS LOGICAL
  ( INPUT icAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 18.1.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectWinToolbar AT ROW 1.33 COL 57.2
     rectToolbar AT ROW 1.33 COL 2
     rectBrowse AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 71.6 BY 19.81.


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
         TITLE              = "Filter"
         HEIGHT             = 19.81
         WIDTH              = 71.8
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.81
       FRAME DEFAULT-FRAME:WIDTH            = 71.6.

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
ON END-ERROR OF C-Win /* Filter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Filter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Filter */
DO:
  DEF VAR hColumn          AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").

  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
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
  DELETE OBJECT hNewBuff.
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
  IF ibDialog THEN
    ihParent:CURRENT-WINDOW:SENSITIVE = YES.
  
  PUBLISH "EndJBoxEvent" (?,ihParent,?,"JBoxWindowClose").

  IF VALID-HANDLE(ihQueryObject) AND ihQueryObject:TYPE = "browse" THEN
    APPLY "entry" TO ihQueryObject.
  ELSE
    ihParent:CURRENT-WINDOW:MOVE-TO-TOP().
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
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN InitializeObject.
  IF ibDialog THEN
    ihParent:CURRENT-WINDOW:SENSITIVE = NO.

  IF THIS-PROCEDURE:CURRENT-WINDOW:ICON = "" THEN
    THIS-PROCEDURE:CURRENT-WINDOW:LOAD-ICON("ico\prospy9.ico") NO-ERROR.
    
  DYNAMIC-FUNCTION("DoLockWindow",?).
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdvancedFilterRecord C-Win 
PROCEDURE AdvancedFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"advancedfilter") = "on" THEN
  DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"advancedfilter","").
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"advancedfilter","on").
  IF THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS LE 405 THEN
    THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = 510.
END.

httCopy:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().
hBrowse:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
  httCopy:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE().
  httCopy:DEFAULT-BUFFER-HANDLE:BUFFER-COPY(hBuffer).
  hBrowse:QUERY:GET-NEXT().
END.
RUN InitFilter.

CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.
APPLY "window-resized" TO CURRENT-WINDOW.

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeNavBrowseFillIn C-Win 
PROCEDURE BeforeNavBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK    AS LOG NO-UNDO INIT TRUE.

DEF VAR cFilterOperator  AS CHAR NO-UNDO.
DEF VAR ix               AS INT  NO-UNDO.
DEF VAR cContElement     AS CHAR NO-UNDO.
DEF VAR bContElementCorr AS LOG  NO-UNDO.
DEF VAR cContString      AS CHAR NO-UNDO.

IF hOperatorOverlay:SCREEN-VALUE = ? OR hOperatorOverlay:SCREEN-VALUE = "" THEN DO:
  CASE ihFillIn:DATA-TYPE:
    WHEN "character" THEN
      IF ihFillIn:INPUT-VALUE NE "" THEN DO:
        cFilterOperator = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE).
        IF cFilterOperator NE "" THEN 
          hOperatorOverlay:SCREEN-VALUE = cFilterOperator.
        ELSE IF INDEX(ihFillIn:INPUT-VALUE,"*") > 0 THEN
          hOperatorOverlay:SCREEN-VALUE = "MATCHES".
        ELSE
          hOperatorOverlay:SCREEN-VALUE = "BEGINS".
      END.
    WHEN "date" THEN
      IF DATE(ihFillIn:SCREEN-VALUE) = ? THEN ihBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "".
      ELSE hOperatorOverlay:SCREEN-VALUE = "=".
    WHEN "decimal" THEN
      IF DEC(ihFillIn:SCREEN-VALUE) = 0 THEN ihBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "".
      ELSE hOperatorOverlay:SCREEN-VALUE = "=".
    WHEN "integer" THEN
      IF INT(ihFillIn:SCREEN-VALUE) = 0 THEN ihBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "".
      ELSE hOperatorOverlay:SCREEN-VALUE = "=".
    WHEN "logical" THEN
      hOperatorOverlay:SCREEN-VALUE = "=".
  END CASE.  
  IF hOperatorOverlay:SCREEN-VALUE NE ? THEN
    ihBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = hOperatorOverlay:SCREEN-VALUE.
  hOperatorOverlay:MODIFIED = FALSE.
END.
ELSE IF ihFillIn:DATA-TYPE = "DATE" AND 
        DATE(ihFillIn:SCREEN-VALUE) = ? THEN 
  ihBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "".
  
IF hOperatorOverlay:SCREEN-VALUE = "contains" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(ihFillIn:SCREEN-VALUE," "):
    cContElement = ENTRY(ix,ihFillIn:SCREEN-VALUE," ").
    IF cContElement BEGINS "*" THEN
      ASSIGN bContElementCorr = YES
             cContElement = LEFT-TRIM(cContElement,"*").
    cContString = cContString + " " + cContElement.          
  END.      
  ASSIGN cContString = TRIM(cContString)
         ihFillIn:SCREEN-VALUE = cContString
         ihBuffer::ColumnValue = cContString.
  IF bContElementCorr THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION ("Scandinavian") THEN "Søkestreng ble korrigert for ord-søk" ELSE "Search-string was corrected for word-index match","","").
END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseColumnLookup C-Win 
PROCEDURE BrowseColumnLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF hOperatorOverlay:SCREEN-VALUE = "IN" AND DYNAMIC-FUNCTION("getAttribute",hValueOverlay,"lastlookupreturnvalues") NE "" THEN DO:
  hValueOverlay:SCREEN-VALUE = REPLACE(DYNAMIC-FUNCTION("getAttribute",hValueOverlay,"lastlookupreturnvalues"),"¤",","). 

  DYNAMIC-FUNCTION("setAttribute",hValueOverlay,"last-event","tab").
  APPLY "tab" TO hValueOverlay.  
  APPLY "entry" TO hValueOverlay.
END.
THIS-PROCEDURE:CURRENT-WINDOW:TOP-ONLY = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseDateLookup C-Win 
PROCEDURE BrowseDateLookup :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
RUN SUPER.
THIS-PROCEDURE:CURRENT-WINDOW:TOP-ONLY = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilterRecord C-Win 
PROCEDURE ClearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowse:QUERY:GET-FIRST().

REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
  IF hBuffer:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE THEN hBuffer:BUFFER-DELETE().
  ELSE
    ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE = "AND"
           hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 0
           .
  hBrowse:QUERY:GET-NEXT().
END.
hBrowse:QUERY:GET-FIRST().

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeGroupSelectedRecord C-Win 
PROCEDURE DeGroupSelectedRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    ASSIGN hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 0
           hBuffer:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = ""
           .
  END.
END.
hBrowse:REFRESH().

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
DEF VAR hOldValueOverlay AS HANDLE NO-UNDO.

DEF VAR cLookupDef       AS CHAR NO-UNDO.
DEF VAR cLookupFields    AS CHAR NO-UNDO.
DEF VAR cLookupFilter    AS CHAR NO-UNDO.
DEF VAR cLookupTable     AS CHAR NO-UNDO.
DEF VAR cLookupReturnFld AS CHAR NO-UNDO.
DEF VAR iMousePos        AS INT  NO-UNDO.
DEF VAR cFilterOperator  AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

IF VALID-HANDLE(hFieldOpOverlay) THEN DYNAMIC-FUNCTION("DeleteObject",hFieldOpOverlay).
DYNAMIC-FUNCTION("DoLockWindow",?).
IF bAdv AND 
  (NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allcalcfields"),
              hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) 
   OR DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"uselocaldata") = "yes") THEN DO:
  hFieldOpOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                    hBrowse,                    
                    "FieldOperator",            
                    "FieldOperator",            
                    "",                         
                    "",                         
                    "AND|AND|OR|OR").  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFieldOpOverlay,"FieldOperator"). 
END.

IF hBuffer:AVAIL AND cCurrOperator = "IN" AND hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE NE "IN" THEN
  hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = ENTRY(1,hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) NO-ERROR.

DYNAMIC-FUNCTION ("setAttribute",hBrowse,"disabledActions",
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) = "CONTAINS" THEN "duplicate" ELSE "").

RUN SUPER.

IF hBuffer:AVAIL THEN DO:

  hOldValueOverlay = hValueOverlay.

  IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE NE "logical" THEN DO:
    IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" OR
       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN DO:
      hValueOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,
                      "ColumnValue",
                      "ColumnValue",
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                          /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                    /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)                    /* Query to get drop-down values */
                      ).   
      hOperatorOverlay:LIST-ITEM-PAIRS = "||=|=|>|>|<|<|>=|>=|<=|<=|<>|<>".
      hValueOverlay:LIST-ITEM-PAIRS = "||" + hValueOverlay:LIST-ITEM-PAIRS.
      hOperatorOverlay:LIST-ITEM-PAIRS = REPLACE(hOperatorOverlay:LIST-ITEM-PAIRS,"|BEGINS|BEGINS|MATCHES|MATCHES","").
    END.
    ELSE DO:
      hValueOverlay = DYNAMIC-FUNCTION("NewPropertyFillIn",
                      hBrowse,
                      "ColumnValue",
                      "ColumnValue",
                      IF hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "IN" THEN
                         "CHARACTER"
                      ELSE
                        hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE,
                      IF hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "IN" THEN
                        "x(256)"
                      ELSE
                        hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE,
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                          /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                    /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupreturnfield_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                    /* Query to get drop-down values */
                      "").   
      cFilterOperator = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE).
      IF cFilterOperator NE "" THEN
        hOperatorOverlay:LIST-ITEM-PAIRS = cFilterOperator + "|" + cFilterOperator.
      ELSE IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "character" THEN
        hOperatorOverlay:LIST-ITEM-PAIRS = "=|=|>|>|<|<|>=|>=|<=|<=|<>|<>|BEGINS|BEGINS|MATCHES|MATCHES".
      ELSE hOperatorOverlay:LIST-ITEM-PAIRS = "=|=|>|>|<|<|>=|>=|<=|<=|<>|<>".
      IF CAN-DO(cAllowCanDoOperator,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN
        hOperatorOverlay:LIST-ITEM-PAIRS = "||IN|IN|" + hOperatorOverlay:LIST-ITEM-PAIRS.
      ELSE
        hOperatorOverlay:LIST-ITEM-PAIRS = "||" + hOperatorOverlay:LIST-ITEM-PAIRS.

    END.
    IF DYNAMIC-FUNCTION("Scandinavian") THEN
      hOperatorOverlay:TOOLTIP = "> - større enn" + CHR(10) + "< - mindre enn".
  END.
  ELSE DO:
    hOperatorOverlay:LIST-ITEM-PAIRS = "||=|=|<>|<>".
    IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" OR
       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN DO:
      hValueOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,
                      "ColumnValue",
                      "ColumnValue",
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                          /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                    /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)                    /* Query to get drop-down values */
                      ).   
      hValueOverlay:LIST-ITEM-PAIRS = "||" + hValueOverlay:LIST-ITEM-PAIRS.
    END.
    ELSE
      hValueOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,
                      "ColumnValue",
                      "ColumnValue",
                      "",    
                      "",  
                      IF hBuffer::CharValueTrue NE "" THEN 
                        "||" + hBuffer::CharValueTrue + "|" + hBuffer::CharValueTrue
                      + "|" + hBuffer::CharValueFalse + "|" + hBuffer::CharValueFalse
                      ELSE cLogicalDropDown
                      ).   
    
    /*
      hValueOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                        hBrowse,
                        "ColumnValue",
                        "ColumnValue",
                        "").
                        */
  END.
  hOperatorOverlay:SCREEN-VALUE = hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE.

  DYNAMIC-FUNCTION("DeleteObject",hOldValueOverlay).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hValueOverlay)).

  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hValueOverlay,"ColumnValue").
    
  hValueOverlay:SCREEN-VALUE = hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE NO-ERROR.
  
  ASSIGN hValueOverlay:MODIFIED = FALSE
         hOperatorOverlay:MODIFIED = FALSE.

  hValueOverlay:TAB-STOP = YES.

  hColumn = hBrowse:GET-BROWSE-COLUMN(1) NO-ERROR.
  IF VALID-HANDLE(hColumn) THEN
    APPLY "end-resize" TO hColumn.

  iMousePos = DYNAMIC-FUNCTION("getMousePosition",FRAME {&FRAME-NAME}:HANDLE,"x").

  IF iMousePos > hValueOverlay:X THEN
    DYNAMIC-FUNCTION("setWidgetEnter",hValueOverlay).
  ELSE IF iMousePos > hOperatorOverlay:X THEN
    DYNAMIC-FUNCTION("setWidgetEnter",hOperatorOverlay).

  IF hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "IN" THEN
    DYNAMIC-FUNCTION("setAttribute",hValueOverlay,"lookup_multiselect","yes").
  ELSE 
    DYNAMIC-FUNCTION("setAttribute",hValueOverlay,"lookup_multiselect","").

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).

END.
THIS-PROCEDURE:CURRENT-WINDOW:TOP-ONLY = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DuplicateRecord C-Win 
PROCEDURE DuplicateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR rRepos        AS ROWID NO-UNDO.
DEF VAR iCurrRow      AS INT   NO-UNDO.


DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","yes").

hNewBuff:BUFFER-CREATE().
hNewBuff:BUFFER-COPY(hBrowse:QUERY:GET-BUFFER-HANDLE(1),"GroupOperator").
ASSIGN hNewBuff:BUFFER-FIELD("Seq"):BUFFER-VALUE = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Seq"):BUFFER-VALUE + 10
       hNewBuff:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE = YES.

ASSIGN rRepos   = hNewBuff:ROWID
       iCurrRow = hBrowse:FOCUSED-ROW.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

IF rRepos NE ? THEN DO:
  hBrowse:SET-REPOSITIONED-ROW(iCurrRow + 1,"conditional").
  hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
  hBrowse:SELECT-ROW(iCurrRow + 1) NO-ERROR.
  APPLY "value-changed" TO hBrowse.
END.

DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FetchFilterRecord C-Win 
PROCEDURE FetchFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
RUN ClearFilterRecord.
DYNAMIC-FUNCTION("LoadUserFilter",ihQueryObject,?).
RUN InitFilter.
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GroupSelectedRecord C-Win 
PROCEDURE GroupSelectedRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cOper         AS CHAR NO-UNDO.
DEF VAR iMaxGroup     AS INT  NO-UNDO.
DEF VAR iCurrGroup    AS INT  NO-UNDO.
DEF VAR cActiveGroups AS CHAR NO-UNDO.
DEF VAR cSelected     AS CHAR NO-UNDO.
DEF VAR cFirstBuffer  AS CHAR NO-UNDO.

cOper = IF CAN-DO("AND,OR",hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE) THEN
          hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE
        ELSE "".

hBrowse:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
  IF NOT CAN-DO(cActiveGroups,STRING(hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE)) THEN
    cActiveGroups = cActiveGroups + STRING(hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE) + ",".
  IF hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE > iMaxGroup THEN
    iMaxGroup = hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE.
  hBrowse:QUERY:GET-NEXT().
END.
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    IF hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE > 0 THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                         "Feltet " + hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE + " er allerede med i en gruppe"
                        ELSE 
                         "Column " + hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE + " is already part of a group")
                       ,"","").
      RUN MoveToTop.
      RETURN.
    END.
    IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"uselocaldata") NE "yes" THEN DO:
      IF cFirstBuffer = "" THEN cFirstBuffer = hBuffer:BUFFER-FIELD("BufferName"):BUFFER-VALUE.
      ELSE IF hBuffer:BUFFER-FIELD("BufferName"):BUFFER-VALUE NE cFirstBuffer THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,
                         (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                           "En gruppe kan ikke angis på tvers av tabeller i spørring"
                          ELSE 
                           "Grouping of criteria cannot cross tables"),"","").
        RUN MoveToTop.
        RETURN.
      END.

      IF CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allcalcfields"),hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,
                         (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                           "Kalkulerte verdier kan ikke inngå i en gruppe"
                          ELSE 
                           "Calculated fields cannot be part of a group"),"","").
        RUN MoveToTop.
        RETURN.
      END.
    END.

    IF hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE NE 0 THEN
      iCurrGroup = hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE.
  END.
END.

RUN JBoxDFilterGroupOperator.w (INPUT-OUTPUT cOper,
                                OUTPUT bOK).

IF iCurrGroup = 0 THEN iCurrGroup = iMaxGroup + 1.
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = iMaxGroup + 1.
    hBuffer:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE = cOper.
    IF ix = 1 THEN
      hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = cOper.
  END.
END.
hBrowse:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFilter C-Win 
PROCEDURE InitFilter :
/*------------------------------------------------------------------------------
  Purpose:     This is where the programmer makes the change
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFilterFields   AS CHAR   NO-UNDO.
DEF VAR iy              AS INT    NO-UNDO.
DEF VAR cKeepFilterOpen AS CHAR   NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR cExtFilter      AS CHAR   NO-UNDO.
DEF VAR cExtBuffers     AS CHAR   NO-UNDO.
DEF VAR cExtFields      AS CHAR   NO-UNDO.
DEF VAR cExtFieldDef    AS CHAR   NO-UNDO.
DEF VAR cExtFieldLab  AS CHAR   NO-UNDO.
DEF VAR cExtFieldForm AS CHAR   NO-UNDO.
DEF VAR cExtFieldType AS CHAR   NO-UNDO.
DEF VAR cExtFieldAttr AS CHAR   NO-UNDO.
DEF VAR cExtDbFldAttr AS CHAR   NO-UNDO.
DEF VAR cFormat       AS CHAR   NO-UNDO.
DEF VAR hColumn       AS HANDLE NO-UNDO.
DEF VAR cValue        AS CHAR   NO-UNDO. 
DEF VAR bValue        AS LOG    NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cKeepFilterOpen = DYNAMIC-FUNCTION("getAttribute",hFilter,"keepfilteropen").
  DYNAMIC-FUNCTION("DeleteObject",hBrowse).
  DYNAMIC-FUNCTION("DeleteObject",hOperatorOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hFilter).
  DYNAMIC-FUNCTION("DeleteObject",hValueOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hFieldOpOverlay).
  DELETE OBJECT hNewBuff NO-ERROR.
  rRepos = ?.

  bAdv = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"advancedfilter") = "on".

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",rectBrowse:HANDLE,1,(IF bAdv THEN "MULTIPLE" ELSE ""),
                             "temp-table"
                           + ";+BufferName|CHARACTER|x(30)||"      + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Tabell" ELSE "Table") 
                           + ";+GroupOperator|CHARACTER|x(5)||"    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "GrpOp" ELSE "GroupOp") 
                           + ";+FieldGroup|INTEGER|>9||"           + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "FeltGrp" ELSE "FieldGr")
                           + ";+FieldOperator|CHARACTER|x(5)||"    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "FeltOp" ELSE "FieldOp") 
                           + ";+ColumnLabel|CHARACTER|x(50)||"     + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Felt" ELSE "Field") 
                           + ";+Operator|CHARACTER|x(8)||Operator" 
                           + ";+ColumnValue|CHARACTER|x(256)||"     + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Verdi" ELSE "Value")  
                           + ";+!ColumnName|CHARACTER" 
                           + ";+!DataType|CHARACTER" 
                           + ";+!Format|CHARACTER" 
                           + ";+!Seq|INTEGER" 
                           + ";+!HiddenGroupOperator|CHARACTER" 
                           + ";+!DuplicateRow|LOGICAL"
                           + ";+!CharValueTrue|CHARACTER"
                           + ";+!CharValueFalse|CHARACTER"
                             ,
                             "where false",
                             "").
  ASSIGN hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 40
         hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 40
         hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 40
         hBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 45
         hFieldGroup = hBrowse:GET-BROWSE-COLUMN(3)
         hBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 100
         .

  IF NOT bAdv THEN
    ASSIGN hBrowse:GET-BROWSE-COLUMN(1):VISIBLE = FALSE
           hBrowse:GET-BROWSE-COLUMN(2):VISIBLE = FALSE
           hBrowse:GET-BROWSE-COLUMN(3):VISIBLE = FALSE
           hBrowse:GET-BROWSE-COLUMN(4):VISIBLE = FALSE
           .

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  IF NOT VALID-HANDLE(httCopy) THEN DO:
    CREATE TEMP-TABLE httCopy.
    DO ix = 1 TO hBuffer:NUM-FIELDS:
      httCopy:ADD-LIKE-FIELD(hBuffer:BUFFER-FIELD(ix):NAME,hBuffer:BUFFER-FIELD(ix)).
    END.
    httCopy:TEMP-TABLE-PREPARE("ttCopy").
  END.

  CREATE BUFFER hNewBuff FOR TABLE hBuffer.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").

  cFilterFields = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterfields"). 
  IF cFilterFields = "" AND ihQueryObject:TYPE = "browse" THEN DO:
    IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"uselocaldata") = "yes" AND
       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GoLocalWhenSmallDistinctCount") NE "yes" THEN
      cFilterFields = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allviewfields").
    ELSE
      cFilterFields = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allviewfields").
  END.
  ELSE IF cFilterFields = "" THEN DO:
    hFieldMap = DYNAMIC-FUNCTION("GetLinkedObject",ihQueryObject,"fieldMap","from").
    IF VALID-HANDLE(hFieldMap) THEN
      cFilterFields = DYNAMIC-FUNCTION("getAttribute",hFieldMap,"screenUpdateFields") + "," 
                    + DYNAMIC-FUNCTION("getAttribute",hFieldMap,"screenDisplayFields").
    ELSE 
      cFilterFields = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allViewFields").
  END.

  IF NOT FillFromCopy() THEN DO:
    cFilterFields = ReduceFilter(cFilterFields).

    cExtFilter = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"extraFilterFields").
    DO ix = 1 TO NUM-ENTRIES(cExtFilter):
      cExtDbFldAttr = "".
      IF NUM-ENTRIES(ENTRY(1,ENTRY(ix,cExtFilter),"|"),".") NE 2 THEN DO:
        MESSAGE "Error in definition of extra filter fields. Must be defined as <table>.<field>"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
        LEAVE.
      END.
      cExtFieldDef = SUBSTR(ENTRY(ix,cExtFilter),INDEX(ENTRY(ix,cExtFilter),".") + 1).
      IF NUM-ENTRIES(cExtFieldDef,"|") > 1 THEN DO:
        cExtFieldLab = ENTRY(2,cExtFieldDef,"|").
        IF cExtFieldLab = "" THEN DO:
          MESSAGE "Error in definition of extra filter fields. No label defined for field: " cExtFieldDef SKIP
                  "Complete attr.setting for entry in extraFilterFields with label override: <table>.<field>|<label>"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
          LEAVE.
        END.
      END.
      ELSE cExtFieldLab = "".
      IF NUM-ENTRIES(cExtFieldDef,"|") > 2 THEN DO:
        cExtFieldForm = ENTRY(3,cExtFieldDef,"|").
        IF cExtFieldForm = "" THEN DO:
          MESSAGE "Error in definition of extra filter fields. No format defined for field: " cExtFieldDef SKIP
                  "Complete attr.setting for entry in extraFilterFields with label and format override: <table>.<field>|<label>|<format>"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
          LEAVE.
        END.
      END.
      ELSE cExtFieldForm = "".
      IF NUM-ENTRIES(cExtFieldDef,"|") > 3 THEN DO:
        cExtFieldType = ENTRY(4,cExtFieldDef,"|").
        IF cExtFieldType = "" THEN DO:
          MESSAGE "Error in definition of extra filter fields. No type defined for field: " cExtFieldDef SKIP
                  "Complete attr.setting for entry in extraFilterFields with label, format and type override: <table>.<field>|<label>|<format>|<type>"
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
          LEAVE.
        END.
        ELSE cExtDbFldAttr = cExtFieldType + "|" + cExtFieldForm + "|" + cExtFieldLab.
      END.
      ELSE cExtFieldType = "".

      cExtFieldDef = ENTRY(1,cExtFieldDef,"|").
      IF CAN-DO(cFilterFields,cExtFieldDef) THEN DO:
        MESSAGE "Extra filter fields cannot have the same name as fields in the query: " cExtFieldDef
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        LEAVE.
      END.

      DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"fieldBuffer" + cExtFieldDef,ENTRY(1,ENTRY(ix,cExtFilter),".")).
      DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"orgdbfield" + cExtFieldDef,cExtFieldDef).
      cExtFields    = cExtFields + (IF cExtFields NE "" THEN "," ELSE "") + cExtFieldDef.
      IF cExtDbFldAttr = "" THEN DO:
        cExtDbFldAttr = DYNAMIC-FUNCTION("getFieldList","_file;,_Field;_data-type;_format;_label" 
                         ,"WHERE _file-name = '" + ENTRY(1,ENTRY(ix,cExtFilter),".") + "'"
                        + ",FIRST _Field OF _File NO-LOCK WHERE _field-name = '" + cExtFieldDef + "'").
        IF cExtDbFldAttr = ? THEN
          cExtDbFldAttr = DYNAMIC-FUNCTION("getFieldList","_file;,_Field;_data-type;_format" 
                         ,"WHERE _file-name = '" + ENTRY(1,ENTRY(ix,cExtFilter),".") + "'"
                        + ",FIRST _Field OF _File NO-LOCK WHERE _field-name = '" + cExtFieldDef + "'").

        IF cExtDbFldAttr = "" THEN
          MESSAGE "Could not obtain information from database on extra filter field " cExtFieldDef SKIP
                  "Either the db/field names are wrong or it could be that the field comes from another db" SKIP
                  "You could try to add type as last parameter to the definition:" SKIP
                  "<table>.<field>|<label>|<format>|<type>"
                  VIEW-AS ALERT-BOX WARNING.
        ELSE DO:
          IF cExtFieldForm NE "" AND NUM-ENTRIES(cExtDbFldAttr,"|") > 1 THEN
            ENTRY(2,cExtDbFldAttr,"|") = cExtFieldForm.
          IF cExtFieldLab NE "" AND NUM-ENTRIES(cExtDbFldAttr,"|") > 2 THEN
            ENTRY(3,cExtDbFldAttr,"|") = cExtFieldLab.
          ELSE IF cExtFieldLab NE "" THEN
            cExtDbFldAttr = cExtDbFldAttr + "|" + cExtFieldLab.
          ELSE 
            cExtDbFldAttr = cExtDbFldAttr + "|".
        END.
      END.

      IF cExtDbFldAttr NE "" THEN DO:
        ASSIGN cFilterFields = cFilterFields + (IF cFilterFields NE "" THEN "," ELSE "") + cExtFieldDef
               cExtFieldAttr = cExtFieldAttr + (IF cExtFieldAttr NE "" THEN ";" ELSE "") + cExtDbFldAttr.
        IF NOT CAN-DO(cExtBuffers,ENTRY(1,ENTRY(ix,cExtFilter),".")) THEN
          cExtBuffers = cExtBuffers + (IF cExtBuffers NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,cExtFilter),".").
      END.
    END.
    IF cExtBuffers NE "" THEN
      DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"extraFilterBuffers",cExtBuffers).

    DO ix = 1 TO NUM-ENTRIES(cFilterFields):
      IF CAN-DO(cExtFields,ENTRY(ix,cFilterFields)) THEN
        cFormat = ENTRY(2,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtFields),cExtFieldAttr,";"),"|").
      ELSE DO:  
        IF ihQueryObject:TYPE = "browse" THEN DO:
          hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihQueryObject,ENTRY(ix,cFilterFields)).
          IF NOT VALID-HANDLE(hColumn) THEN
            hColumn = ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)) NO-ERROR. 
        END.  
        ELSE
          hColumn = ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)) NO-ERROR.         
        IF VALID-HANDLE(hColumn) THEN
          cFormat = hColumn:FORMAT.
        ELSE NEXT.
      END.  
            
      hBuffer:BUFFER-CREATE().
      ASSIGN hBuffer:BUFFER-FIELD("BufferName"):BUFFER-VALUE = 
                     IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"uselocaldata") = "yes" THEN
                       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"basetable")
                     ELSE DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"fieldbuffer" + ENTRY(ix,cFilterFields))
             hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE =            
                     IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GroupOperator_" + ENTRY(ix,cFilterFields)) NE "" THEN
                       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GroupOperator_" + ENTRY(ix,cFilterFields))
                     ELSE ""
             hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 
                     IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldGroup_" + ENTRY(ix,cFilterFields)) NE "" THEN
                       INT(DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldGroup_" + ENTRY(ix,cFilterFields)))
                     ELSE 0
             hBuffer:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE =            
                     IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + ENTRY(ix,cFilterFields)) NE "" THEN
                       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + ENTRY(ix,cFilterFields))
                     ELSE "AND"
             hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE = ENTRY(ix,cFilterFields)
             hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = 
                     DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"operatorInUse_" + ENTRY(ix,cFilterFields))           
             hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = 
                     DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + ENTRY(ix,cFilterFields))
             hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = 
                     IF CAN-DO(cExtFields,ENTRY(ix,cFilterFields)) THEN
                       ENTRY(1,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtFields),cExtFieldAttr,";"),"|")
                     ELSE IF ihQueryObject:TYPE = "browse" THEN
                       ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):DATA-TYPE
                     ELSE
                       ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):DATA-TYPE
             hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE = cFormat 
/*                     IF CAN-DO(cExtFields,ENTRY(ix,cFilterFields)) THEN                                     */
/*                       ENTRY(2,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtFields),cExtFieldAttr,";"),"|")     */
/*                     ELSE IF ihQueryObject:TYPE = "browse" THEN                                             */
/*                       ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):FORMAT*/
/*                     ELSE                                                                                   */
/*                       ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):FORMAT      */
             hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE = 
                     IF CAN-DO(cExtFields,ENTRY(ix,cFilterFields)) THEN
                       ENTRY(3,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtFields),cExtFieldAttr,";"),"|")
                     ELSE IF ihQueryObject:TYPE = "browse" THEN
                       DYNAMIC-FUNCTION("getBrowseColumnLabel",ihQueryObject,ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):NAME)
                     ELSE
                       ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):LABEL
             hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = ix + 100 * ix
             hBuffer:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE =            
                     IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"HiddenGroupOperator_" + ENTRY(ix,cFilterFields)) NE "" THEN
                       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"HiddenGroupOperator_" + ENTRY(ix,cFilterFields))
                     ELSE ""
             .

/*      IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "LOGICAL" AND TRIM(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) = "true"  THEN      */
/*        hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "yes".                                                                                 */
/*      ELSE IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "LOGICAL" AND TRIM(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) = "false"  THEN*/
/*        hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "no".                                                                                  */

      IF hBuffer::DataType = "LOGICAL" THEN DO:
        cValue = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterDropdownValuelist_" + ENTRY(ix,cFilterFields)).
        IF cValue NE "" THEN
          ASSIGN hBuffer::CharValueTrue  = ENTRY(NUM-ENTRIES(cValue,"|") - 2,cValue,"|")
                 hBuffer::CharValueFalse = ENTRY(NUM-ENTRIES(cValue,"|"),cValue,"|")
                 . 
        ELSE IF NUM-ENTRIES(cFormat,"/") = 2 AND ENTRY(1,cFormat,"/") NE "" AND ENTRY(2,cFormat,"/") NE "" AND ASC(ENTRY(1,cFormat,"/")) NE 254 THEN        
          ASSIGN hBuffer::CharValueTrue  = ENTRY(1,cFormat,"/")
                 hBuffer::CharValueFalse = ENTRY(2,cFormat,"/")
                 . 
        ELSE         
          ASSIGN hBuffer::CharValueTrue  = ENTRY(NUM-ENTRIES(cLogicalDropDown,"|") - 2,cLogicalDropDown,"|")
                 hBuffer::CharValueFalse = ENTRY(NUM-ENTRIES(cLogicalDropDown,"|"),cLogicalDropDown,"|")
                 . 
        IF hBuffer::ColumnValue NE "" THEN DO:        
          bValue = LOGICAL(hBuffer::ColumnValue) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR AND bValue NE ? THEN DO:
            IF bValue THEN
              hBuffer::ColumnValue = hBuffer::CharValueTrue.
            ELSE         
              hBuffer::ColumnValue = hBuffer::CharValueFalse.
          END.
        END.
      END.  


      DO iy = 1 TO 100:
        IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields)) NE "" THEN DO:
          hNewBuff:BUFFER-CREATE().
          hNewBuff:BUFFER-COPY(hBuffer).
          ASSIGN hNewBuff:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = 
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GroupOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hNewBuff:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldGroup_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hNewBuff:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE = 
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hNewBuff:BUFFER-FIELD("Operator"):BUFFER-VALUE = 
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hNewBuff:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE = 
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hNewBuff:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = 
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hNewBuff:BUFFER-FIELD("Seq"):BUFFER-VALUE = ix + 100 * ix + iy
                 hNewBuff:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE = 
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"HiddenGroupOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hNewBuff:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE = YES
                 .
        END.
        ELSE IF iy > 1 OR (iy = 1 AND DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_2_" + ENTRY(ix,cFilterFields)) = "") THEN
          LEAVE.
      END.
  
      IF rRepos = ? AND hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE NE "" THEN 
        rRepos = hBuffer:ROWID.
    END.
  END.

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowse,  /* parent widget */
                    "Duplicate;" + cDuplicateLabel
                  + (IF bAdv THEN 
                      ",GroupSelected;"   + cGroupSelectedLabel 
                    + ",DeGroupSelected;" + cDeGroupSelectedLabel
                     ELSE "")
                    ,"").

  hOperatorOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                    hBrowse,                      /* Handle to browse */
                    "Operator",                   /* Browse column (display) */
                    "Operator",                   /* Buffer column (to update - foreign key. Maps to value - under) */
                    "",                           /* DB buffers and fields for drop-down values: Label,value */
                    "",                           /* Query to get drop-down values */
                    "|").   /* I've got the values. Don't go to the database */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hOperatorOverlay,"Operator"). /* Link the dropdown to the browse with column-name info */

/*   IF bAdv THEN DO:                                                                                                                              */
/*     hFieldOpOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",                                                                                     */
/*                       hBrowse,                                                                                                                  */
/*                       "FieldOperator",                                                                                                          */
/*                       "FieldOperator",                                                                                                          */
/*                       "",                                                                                                                       */
/*                       "",                                                                                                                       */
/*                       "AND|AND|OR|OR").                                                                                                         */
/*     DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFieldOpOverlay,"FieldOperator"). /* Link the dropdown to the browse with column-name info */  */
/*   END.                                                                                                                                          */

  hFilter = DYNAMIC-FUNCTION("NewDynFilter",
                             hBrowse,
                             ihFilterButton,                                                             /* Open filter window button */
                             "").
  DYNAMIC-FUNCTION("setAttribute",hFilter,"keepfilteropen",cKeepFilterOpen).

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFilter).
  DYNAMIC-FUNCTION("CreateObjectLink",ihQueryObject,hFilter).

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querysort","Seq").
  RUN OpenQuery.

  IF rRepos NE ? THEN DO:
    hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
    hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
    APPLY "value-changed" TO hBrowse.
  END.

END.
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
DEF VAR hSubMenuAdv        AS HANDLE NO-UNDO.
DEF VAR cDefaultFrameFont  AS CHAR   NO-UNDO.

cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont").
IF cDefaultFrameFont NE "" THEN
  FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.

IF DYNAMIC-FUNCTION("Scandinavian") THEN
  cLogicalDropDown = "||Ja|Ja|Nei|Nei".
ELSE 
  cLogicalDropDown = "||Yes|Yes|No|No".

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cActivePinButton      = DYNAMIC-FUNCTION("getAttribute",SESSION,"ActivePinButton")
         cPassivePinButton     = DYNAMIC-FUNCTION("getAttribute",SESSION,"PassivePinButton")
         cAllowCanDoOperator   = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allowCan-DoFilterOperator")
         cDuplicateLabel       = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Dupliser rad" ELSE "Duplicate"
         cGroupSelectedLabel   = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Gruppér markerte felter" ELSE "Group selected fields"
         cDeGroupSelectedLabel = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fjern markerte felter fra gruppe" ELSE "Remove selected fields from group"
         .
  IF cActivePinButton  = "" THEN cActivePinButton = "gif\pushout.gif".
  IF cPassivePinButton = "" THEN cPassivePinButton = "gif\pushin.gif".

  SUBSCRIBE TO "InitStringTranslation" ANYWHERE.

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             (IF NOT DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"disablesavefilter") = "yes"
                                 AND DYNAMIC-FUNCTION("getIsUserSettingInstalled") THEN 
                               "SaveFilter;"
                               + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Lagre filter" ELSE "Save filter") + "¤menu,"
                              ELSE "")
                           + (IF DYNAMIC-FUNCTION("getUserSetting",
                                                  DYNAMIC-FUNCTION("getObjectSourceFile",ihQueryObject),
                                                  DYNAMIC-FUNCTION("getObjectName",ihQueryObject),
                                                  DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"usersettingcontext"),
                                                  "queryfilter"
                                                  ) NE "" OR
                                 DYNAMIC-FUNCTION("getUserSetting",
                                                 DYNAMIC-FUNCTION("getObjectSourceFile",ihQueryObject),
                                                 DYNAMIC-FUNCTION("getObjectName",ihQueryObject),
                                                 DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"usersettingcontext"),
                                                 "prescanqueryfilter"
                                                 ) NE "" OR
                                 DYNAMIC-FUNCTION("getUserSetting",
                                                  DYNAMIC-FUNCTION("getObjectSourceFile",ihQueryObject),
                                                  DYNAMIC-FUNCTION("getObjectName",ihQueryObject),
                                                  DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"usersettingcontext"),
                                                  "calcfieldfilter"
                                                  ) NE ""
                                 THEN
                                "FetchFilter;" +
                                (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Hent lagret filter&" ELSE "Fetch saved filter&") + "¤menu,"
                              ELSE "")
                           + "ClearFilter;"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "&Blank" ELSE "Clear")
                           + ",StartDynFilter;&OK,|"
                             + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Innstillinger" ELSE "Settings"),
                             "maxborder,enable").
    
  RUN InitFilter.

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             "Close;E&xit"
                           + (IF NOT ibDialog THEN ",Pin;;;pinWindow;" + cPassivePinButton
                              ELSE "")
                            ,"right,enable").  

  hSubMenuAdv = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"placeholder1")).
  DYNAMIC-FUNCTION("NewMenuBand",
                   hSubMenuAdv,
                   "AdvancedFilter;"
                   + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Avansert" ELSE "Advanced")
                   + ";AdvancedFilterRecord;toggle"
                 + ",ViewQuery;"
                   + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vis spørring" ELSE "View query")
                   + ";ViewQueryRecord;toggle"
                   ,"").
  ASSIGN hMenuItemAdv   = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"AdvancedFilter","menu-item")
         hMenuItemViewQ = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"ViewQuery","menu-item")
         hPinButton     = DYNAMIC-FUNCTION("getEventWidget",hWinToolbar,"Pin","").

  IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"advancedfilter") = "on" THEN
    hMenuItemAdv:CHECKED = TRUE.
  IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"viewqueryfilter") = "on" THEN 
    hMenuItemViewQ:CHECKED = TRUE.

  IF ihQueryObject:TYPE = "query" THEN
    cBaseTable = ihQueryObject:GET-BUFFER-HANDLE(1):NAME.
  ELSE
    cBaseTable = ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):NAME.
END.

DYNAMIC-FUNCTION("setMyEnableSaveSettingsMenu",NO) NO-ERROR.
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,250,150).

APPLY "value-changed" TO hBrowse.
DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).


IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"advancedfilter") = "on" THEN 
  THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = 510.
THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = MIN(SESSION:HEIGHT-PIXELS - 100,40 + ix * 21).
APPLY "window-resized" TO {&WINDOW-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

/* For translation: Init special translation type and corresponding values: */
IF NOT CAN-DO(iocTypeList,"Filter-duplicate") THEN
  iocTypeList = iocTypeList + ",Filter-duplicate".
IF NOT CAN-DO(iocTypeList,"Filter-group-selected") THEN
  iocTypeList = iocTypeList + ",Filter-group-selected".
IF NOT CAN-DO(iocTypeList,"Filter-degroup-selected") THEN
  iocTypeList = iocTypeList + ",Filter-degroup-selected".
IF NOT CAN-DO(iocTypeList,"LogicalCombo") THEN
  iocTypeList = iocTypeList + ",LogicalCombo".


ASSIGN 
  cDuplicateLabel = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,
                                         "Filter-duplicate",cDuplicateLabel)
  cGroupSelectedLabel  = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,
                                         "Filter-group-selected",cGroupSelectedLabel)
  cDeGroupSelectedLabel = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,
                                         "Filter-degroup-selected",cDeGroupSelectedLabel)
  cLogicalDropDown = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,
                      "LogicalCombo",cLogicalDropDown)
  .
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

IF VALID-HANDLE(hValueOverlay) THEN APPLY "entry" TO hValueOverlay.

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

IF DYNAMIC-FUNCTION("getAttribute",hFilter,"keepfilteropen") = "yes" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hFilter,"keepfilteropen","").
  hPinButton:LOAD-IMAGE(cPassivePinButton).
  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = NO.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hFilter,"keepfilteropen","yes").
  hPinButton:LOAD-IMAGE(cActivePinButton).
  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = YES.
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

IF CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allcalcfields"),
          hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN
 hFieldGroup:BGCOLOR = 8.

ELSE IF hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE > 0 AND VALID-HANDLE(hFieldGroup) THEN
  hFieldGroup:BGCOLOR = hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveFilterRecord C-Win 
PROCEDURE SaveFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hLastWidget AS HANDLE NO-UNDO.

hLastWidget = FOCUS.

IF VALID-HANDLE(hLastWidget) AND hLastWidget:TYPE = "fill-in" THEN
  APPLY "return" TO hLastWidget.

DYNAMIC-FUNCTION("SetDynFilter",
                 "Save", 
                 hFilter,
                 ihQueryObject,
                 ihParent:FILE-NAME,
                 "").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartDynFilterRecord C-Win 
PROCEDURE StartDynFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
setDuplicateGroups("set").

RUN SUPER.

setDuplicateGroups("clear").

PUBLISH "RefreshAccumTotals" (ihQueryObject).

IF NOT DYNAMIC-FUNCTION("getAttribute",hFilter,"keepfilteropen") = "yes" OR NOT ihParent:CURRENT-WINDOW:SENSITIVE THEN DO:    
  APPLY "close" TO THIS-PROCEDURE.
  IF ihQueryObject:TYPE = "browse" THEN
    APPLY "entry" TO ihQueryObject.
END.
ELSE APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabFromBrowse C-Win 
PROCEDURE TabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hValueOverlay) THEN
  APPLY "entry" TO hValueOverlay.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseDropDown C-Win 
PROCEDURE ValChngBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
cCurrOperator = hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hValueOverlay THEN DO:
  IF hValueOverlay:TYPE = "combo-box" THEN DO: 
    IF hValueOverlay:SCREEN-VALUE = ? OR hValueOverlay:SCREEN-VALUE = "" THEN
      hOperatorOverlay:SCREEN-VALUE = " ".
    ELSE IF hOperatorOverlay:SCREEN-VALUE = ? OR hOperatorOverlay:SCREEN-VALUE = "" THEN
      hOperatorOverlay:SCREEN-VALUE = "=".
    APPLY "value-changed" TO hOperatorOverlay.
  END.
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hOperatorOverlay THEN DO: 
  IF hOperatorOverlay:SCREEN-VALUE = ? OR hOperatorOverlay:SCREEN-VALUE = "" THEN DO:
    IF hBuffer:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE THEN DO:
      hBuffer:BUFFER-DELETE().
      DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
      RUN OpenQuery.
    END.
    ELSE DO:
      ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = ""
             hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = ""
             hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = ""
             hBuffer:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE = "AND"
             hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 0
             .
      DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
      RUN DisplayRecord.
    END.
  END.

  IF CAN-DO(cAllowCanDoOperator,hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) THEN
/*   IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allowCAN-DOfilterOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) = "yes" THEN */
    RUN InvokeMethod(hBrowse,"DisplayRecord").
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
RUN SUPER.

IF hOperatorOverlay:SCREEN-VALUE = ? OR hOperatorOverlay:SCREEN-VALUE = "" THEN
  ASSIGN hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "="
         hOperatorOverlay:SCREEN-VALUE = "=".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewQueryRecord C-Win 
PROCEDURE ViewQueryRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"viewqueryfilter") = "on" THEN 
  DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"viewqueryfilter","").
ELSE 
  DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"viewqueryfilter","on").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillFromCopy C-Win 
FUNCTION FillFromCopy RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hCopyQuery AS HANDLE NO-UNDO.
DEF VAR bFilled    AS LOG NO-UNDO.

CREATE QUERY hCopyQuery.
hCopyQuery:SET-BUFFERS(httCopy:DEFAULT-BUFFER-HANDLE).
hCopyQuery:QUERY-PREPARE("FOR EACH " + httCopy:DEFAULT-BUFFER-HANDLE:NAME).
hCopyQuery:QUERY-OPEN().
hCopyQuery:GET-FIRST().
REPEAT WHILE NOT hCopyQuery:QUERY-OFF-END:
  hBuffer:BUFFER-CREATE().
  hBuffer:BUFFER-COPY(hCopyQuery:GET-BUFFER-HANDLE(1)).
  hCopyQuery:GET-NEXT().
  bFilled = TRUE.
  IF rRepos = ? AND hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE NE "" THEN 
    rRepos = hBuffer:ROWID.
END.

DELETE OBJECT hCopyQuery.

RETURN bFilled.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReduceFilter C-Win 
FUNCTION ReduceFilter RETURNS CHARACTER
  ( INPUT icFilterFields AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewList     AS CHAR NO-UNDO.
DEF VAR cExcludeList AS CHAR NO-UNDO.
DEF VAR hColumn      AS HANDLE NO-UNDO.

cExcludeList = TRIM(REPLACE(DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterexcludefields"),"+","") 
                    + ",iJBoxCompanyId"
                    ,",")
                    .

IF ihQueryObject:TYPE = "browse" THEN DO ix = 1 TO ihQueryObject:NUM-COLUMNS:
  IF CAN-DO(icFilterFields,ihQueryObject:GET-BROWSE-COLUMN(ix):NAME)
     AND NOT CAN-DO(cExcludeList,ihQueryObject:GET-BROWSE-COLUMN(ix):NAME) THEN 
    cNewList = cNewList + ihQueryObject:GET-BROWSE-COLUMN(ix):NAME + ",".
END.
ELSE DO ix = 1 TO NUM-ENTRIES(icFilterFields):
  hColumn = ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,icFilterFields)) NO-ERROR.           
  IF NOT CAN-DO(cExcludeList,ENTRY(ix,icFilterFields)) AND VALID-HANDLE(hColumn) THEN 
    cNewList = cNewList + ENTRY(ix,icFilterFields) + ",".
END.
RETURN TRIM(cNewList,","). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDuplicateGroups C-Win 
FUNCTION setDuplicateGroups RETURNS LOGICAL
  ( INPUT icAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hDuplQuery AS HANDLE NO-UNDO.
DEF VAR hDuplBuff  AS HANDLE NO-UNDO.
DEF VAR iGrp       AS INT    NO-UNDO INIT 50.
DEF VAR cPrevCol   AS CHAR   NO-UNDO.
DEF VAR cSetRows   AS CHAR   NO-UNDO EXTENT 100.
DEF VAR iSetIdx    AS INT    NO-UNDO INIT 1.

CREATE BUFFER hDuplBuff FOR TABLE hBuffer.
CREATE QUERY hDuplQuery.
hDuplQuery:SET-BUFFERS(hDuplBuff).
hDuplQuery:QUERY-PREPARE("FOR EACH " + hDuplBuff:NAME
                       + (IF icAction = "clear" THEN " WHERE FieldGroup GE 50" ELSE "")
                       + " BY ColumnName BY DuplicateRow DESC").
hDuplQuery:QUERY-OPEN().
hDuplQuery:GET-FIRST().
REPEAT WHILE NOT hDuplQuery:QUERY-OFF-END:
  IF icAction = "set" THEN DO:
    IF hDuplBuff:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE OR cPrevCol = hDuplBuff:BUFFER-FIELD("ColumnName"):BUFFER-VALUE THEN
      cSetRows[iSetIdx] = cSetRows[iSetIdx] + (IF cSetRows[iSetIdx] NE "" THEN "," ELSE "") 
                        + (IF hDuplBuff:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE > 0 THEN "n/a" ELSE STRING(hDuplBuff:ROWID)).
    ELSE iSetIdx = iSetIdx + 1.
  END.
  ELSE DO: 
    ASSIGN hDuplBuff:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = ""
           hDuplBuff:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE    = 0.
    DYNAMIC-FUNCTION("msetAttribute",ihQueryObject,"GroupOperator_*" + hDuplBuff:BUFFER-FIELD("ColumnName"):BUFFER-VALUE,"").
    DYNAMIC-FUNCTION("msetAttribute",ihQueryObject,"FieldGroup_*" + hDuplBuff:BUFFER-FIELD("ColumnName"):BUFFER-VALUE,"0").
  END.
    
  cPrevCol = hDuplBuff:BUFFER-FIELD("ColumnName"):BUFFER-VALUE.
  hDuplQuery:GET-NEXT().
END.

IF icAction = "set" THEN
  DO ix = 1 TO 100:
    IF NUM-ENTRIES(cSetRows[ix]) > 1 AND NOT CAN-DO(cSetRows[ix],"n/a") THEN DO:
      DO iSetIdx = 1 TO NUM-ENTRIES(cSetRows[ix]):
        hDuplBuff:FIND-BY-ROWID(TO-ROWID(ENTRY(iSetIdx,cSetRows[ix]))).
        ASSIGN hDuplBuff:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = "AND"
               hDuplBuff:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = iGrp.
      END.
      iGrp = iGrp + 1.
    END.
  END.

DELETE OBJECT hDuplQuery.
DELETE OBJECT hDuplBuff.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

