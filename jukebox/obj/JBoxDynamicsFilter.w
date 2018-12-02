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
DEF VAR ix                 AS INT NO-UNDO.
DEF VAR bOk                AS LOG NO-UNDO.
                           
DEF VAR hFilter            AS HANDLE NO-UNDO.
DEF VAR hBrowse            AS HANDLE NO-UNDO.
DEF VAR hBuffer            AS HANDLE NO-UNDO.
DEF VAR hToolbar           AS HANDLE NO-UNDO.
DEF VAR hWinToolbar        AS HANDLE NO-UNDO.
DEF VAR hSaveFilterMenu    AS HANDLE NO-UNDO.
                           
DEF VAR hColumnTextOverlay AS HANDLE NO-UNDO.
DEF VAR hValueOverlay      AS HANDLE NO-UNDO.
DEF VAR hValueOverlay1     AS HANDLE NO-UNDO.
DEF VAR hValueOverlay2     AS HANDLE NO-UNDO.
DEF VAR hNewBuff           AS HANDLE NO-UNDO.
DEF VAR httCopy            AS HANDLE NO-UNDO.
DEF VAR hPinButton         AS HANDLE NO-UNDO.
                           
DEF VAR hMenuItemAdv       AS HANDLE NO-UNDO.
DEF VAR hMenuItemViewQ     AS HANDLE NO-UNDO.
DEF VAR bAdv               AS LOG    NO-UNDO.
DEF VAR rRepos             AS ROWID  NO-UNDO.
DEF VAR hSearchColumn      AS HANDLE NO-UNDO.
DEF VAR cBaseTable         AS CHAR   NO-UNDO.
DEF VAR cLastOverlay       AS CHAR   NO-UNDO.
DEF VAR hOverlayRectangle  AS HANDLE NO-UNDO.
DEF VAR bApply             AS LOG    NO-UNDO.
DEF VAR cCurrOverlayValue  AS CHAR   NO-UNDO.
DEF VAR cCurrOverlay1Value AS CHAR   NO-UNDO.
DEF VAR bDelVal2           AS LOG    NO-UNDO.
DEF VAR bSetVal2           AS LOG    NO-UNDO.
DEF VAR hLastWidget        AS HANDLE NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS rectToolbar rectBrowse tbSaveFilter 
&Scoped-Define DISPLAYED-OBJECTS tbSaveFilter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustRowViewValues C-Win 
FUNCTION AdjustRowViewValues RETURNS LOGICAL
  ( INPUT icFieldName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReduceFilter C-Win 
FUNCTION ReduceFilter RETURNS CHARACTER
  ( INPUT icFilterFields AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReshapeFilter C-Win 
FUNCTION ReshapeFilter RETURNS LOGICAL
  ( INPUT icShapeTo AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SavedFilterEqualsFilter C-Win 
FUNCTION SavedFilterEqualsFilter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveModifiedOverlay C-Win 
FUNCTION SaveModifiedOverlay RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91.2 BY 19.76.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.

DEFINE VARIABLE tbSaveFilter AS LOGICAL INITIAL no 
     LABEL "Permanent" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbSaveFilter AT ROW 1.33 COL 3.4
     rectToolbar AT ROW 1.33 COL 48.2
     rectBrowse AT ROW 2.67 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 93.4 BY 21.57.


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
         HEIGHT             = 21.57
         WIDTH              = 93.4
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/prospy9.ico":U) THEN
    MESSAGE "Unable to load icon: ico/prospy9.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 21.57
       FRAME DEFAULT-FRAME:WIDTH            = 93.4.

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

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).

  IF ibDialog THEN
    ihParent:CURRENT-WINDOW:SENSITIVE = YES.
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
  RUN InitializeObject.
  IF ibDialog THEN
    ihParent:CURRENT-WINDOW:SENSITIVE = NO.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyRecord C-Win 
PROCEDURE ApplyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
bApply = YES.
RUN StartDynFilterRecord.
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

cLastOverlay = ihFillIn:NAME.

IF (ihFillIn:DATA-TYPE = "DATE" AND
    DATE(ihFillIn:SCREEN-VALUE) = ?) OR ihFillIn:SCREEN-VALUE = ? OR ihFillIn:SCREEN-VALUE = "?" THEN DO:
  IF ihFillIn = hValueOverlay THEN
    ASSIGN ihBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = ""
           ihBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "".
  ELSE IF ihFillIn = hValueOverlay1 THEN
    ihBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = "".
  ELSE IF ihFillIn = hValueOverlay2 THEN
    ihBuffer:BUFFER-FIELD("ColumnValue_2"):BUFFER-VALUE = "".
END.
IF bDelVal2 THEN 
  ASSIGN hValueOverlay1:SCREEN-VALUE = "?"
         hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = "".
IF bSetVal2 THEN 
  ASSIGN hValueOverlay1:SCREEN-VALUE = hValueOverlay:SCREEN-VALUE
         hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE.

ASSIGN cCurrOverlayValue  = hValueOverlay:SCREEN-VALUE
       cCurrOverlay1Value = hValueOverlay1:SCREEN-VALUE.
 
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
DEF VAR cReturnValue AS CHAR NO-UNDO.

RUN SUPER.
IF DYNAMIC-FUNCTION("getCurrentObject") = hValueOverlay AND 
   DYNAMIC-FUNCTION("getAttribute",hValueOverlay,"lastlookupreturnvalues") NE "" THEN DO:
  cReturnValue = DYNAMIC-FUNCTION("getAttribute",hValueOverlay,"lastlookupreturnvalues").
  DO ix = 1 TO MIN(2,NUM-ENTRIES(cReturnValue,"¤")):
    IF ix = 1 THEN 
      ASSIGN hValueOverlay:SCREEN-VALUE = ENTRY(ix,cReturnValue,"¤")
             hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = hValueOverlay:SCREEN-VALUE.
    ELSE 
      ASSIGN hValueOverlay1:SCREEN-VALUE = ENTRY(ix,cReturnValue,"¤")
             hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = hValueOverlay1:SCREEN-VALUE.
  END.
END.

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
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("ColumnValue_2"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE = "AND"
           hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 0
           .
  hBrowse:QUERY:GET-NEXT().
END.

DYNAMIC-FUNCTION("msetAttribute",ihQueryObject,"operatorInUse","").
DYNAMIC-FUNCTION("msetAttribute",ihQueryObject,"filtervalue","").

hBrowse:QUERY:GET-FIRST().

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

APPLY "value-changed" TO hBrowse.
APPLY "entry" TO hValueOverlay.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeletePropertyOverlayValue C-Win 
PROCEDURE DeletePropertyOverlayValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrOverlay AS HANDLE NO-UNDO.

hCurrOverlay = DYNAMIC-FUNCTION("getCurrentObject").

IF CAN-DO("integer,decimal",hCurrOverlay:DATA-TYPE) THEN DO:
  IF hCurrOverlay:SELECTION-TEXT = hCurrOverlay:SCREEN-VALUE OR LENGTH(hCurrOverlay:SCREEN-VALUE) = 1 THEN
    hCurrOverlay:SCREEN-VALUE = ?.
END.
RUN SUPER.

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
DEF VAR hColumn           AS HANDLE NO-UNDO.
DEF VAR hOldValueOverlay  AS HANDLE NO-UNDO.
DEF VAR hOldValueOverlay1 AS HANDLE NO-UNDO.
DEF VAR hOldValueOverlay2 AS HANDLE NO-UNDO.
DEF VAR iMousePos         AS INT    NO-UNDO.
DEF VAR bDropDown         AS LOG    NO-UNDO.
                          
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","yes").

RUN SUPER.

IF hBuffer:AVAIL THEN DO:

  ASSIGN hOldValueOverlay  = hValueOverlay
         hOldValueOverlay1 = hValueOverlay1
         hOldValueOverlay2 = hValueOverlay2
         .

  IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE NE "logical" THEN DO:
    IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" OR
       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN DO:
      hValueOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,
                      "ColumnValue",
                      "ColumnValue",
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),    
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),  
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)
                      ).   
      hValueOverlay:LIST-ITEM-PAIRS = "||" + hValueOverlay:LIST-ITEM-PAIRS.
      hValueOverlay1 = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,
                      "ColumnValue",
                      "ColumnValue",
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE), 
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),  
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)
                      ).   
      hValueOverlay1:LIST-ITEM-PAIRS = "||" + hValueOverlay1:LIST-ITEM-PAIRS.
      bDropDown = YES.
    END.
    ELSE DO:
      hValueOverlay = DYNAMIC-FUNCTION("NewPropertyFillIn",
                      hBrowse,
                      "ColumnValue",
                      "ColumnValue",
                      hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE,
                      hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE,
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),  
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),   
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupreturnfield_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),
                      "").   
      DYNAMIC-FUNCTION("setAttribute",hValueOverlay,"buffersandfields",DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)).
      hValueOverlay1 = DYNAMIC-FUNCTION("NewPropertyFillIn",
                      hBrowse,
                      "ColumnValue_1",
                      "ColumnValue_1",
                      hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE,
                      hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE,
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),  
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),   
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupreturnfield_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE), 
                      "").   
        
      DYNAMIC-FUNCTION("setAttribute",hValueOverlay,"lookup_multiselect","yes").
      DYNAMIC-FUNCTION("setAttribute",hValueOverlay,"lookup_numreturnrows","2").
      DYNAMIC-FUNCTION("setAttribute",hValueOverlay,"lookup_sortedselect","yes").
    END.
  END.
  ELSE DO:
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

      hValueOverlay1 = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,
                      "ColumnValue_1",
                      "ColumnValue_1",
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                          /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),                    /* Query to get drop-down values */
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)                    /* Query to get drop-down values */
                      ).   
      hValueOverlay:LIST-ITEM-PAIRS = "||" + hValueOverlay:LIST-ITEM-PAIRS.
    END.
    ELSE DO:
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
                         
      hValueOverlay1 = DYNAMIC-FUNCTION("NewBrowseDropDown",
                      hBrowse,
                      "ColumnValue_1",
                      "ColumnValue_1",
                      "",    
                      "",  
                      IF hBuffer::CharValueTrue NE "" THEN 
                        "||" + hBuffer::CharValueTrue + "|" + hBuffer::CharValueTrue
                      + "|" + hBuffer::CharValueFalse + "|" + hBuffer::CharValueFalse
                      ELSE cLogicalDropDown
                      ).   
/*    
    hValueOverlay = DYNAMIC-FUNCTION("NewBrowseDropDown",
                    hBrowse,
                    "ColumnValue",
                    "ColumnValue",
                    "",    
                    "",  
                    cLogicalDropDown
                    ).   
    hValueOverlay1 = DYNAMIC-FUNCTION("NewBrowseDropDown",
                    hBrowse,
                    "ColumnValue_1",
                    "ColumnValue_1",
                    "",    
                    "",  
                    cLogicalDropDown
                    ).
                       */
    END.                   
    ASSIGN bDropDown = YES
           hValueOverlay1:SENSITIVE = NO.

/*     hValueOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",  */
/*                       hBrowse,                           */
/*                       "ColumnValue",                     */
/*                       "ColumnValue",                     */
/*                       "").                               */
/*     hValueOverlay1 = DYNAMIC-FUNCTION("NewBrowseToggle", */
/*                       hBrowse,                           */
/*                       "ColumnValue_1",                   */
/*                       "ColumnValue_1",                   */
/*                       "").                               */
  END.

  IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "character" AND NOT bDropDown THEN
    hValueOverlay2 = DYNAMIC-FUNCTION("NewPropertyFillIn",
                     hBrowse,
                    "ColumnValue_2",
                     "ColumnValue_2",
                     "character",
                      hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE,
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),    
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupquery_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),     
                      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterlookupreturnfield_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE),
                     ""). 
  ELSE DO:
    hValueOverlay2 = DYNAMIC-FUNCTION("NewPropertyFillIn",
                     hBrowse,
                    "ColumnValue_2",
                     "ColumnValue_2",
                     "character",
                     IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "character" THEN hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE
                     ELSE "x(256)",
                     "",
                     "",
                     "",
                     ""). 
    ASSIGN hValueOverlay2:SENSITIVE   = NO
           hValueOverlay2:BGCOLOR     = 8.
  END.


  DYNAMIC-FUNCTION("DeleteObject",hOldValueOverlay).
  DYNAMIC-FUNCTION("DeleteObject",hOldValueOverlay1).
  DYNAMIC-FUNCTION("DeleteObject",hOldValueOverlay2).

  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hValueOverlay,"ColumnValue"). 
  hValueOverlay:SCREEN-VALUE = hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE NO-ERROR.
  ASSIGN hValueOverlay:MODIFIED = FALSE
         hValueOverlay:TAB-STOP = YES.
  IF VALID-HANDLE(hValueOverlay1) THEN DO:
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hValueOverlay1,"ColumnValue_1"). 
    hValueOverlay1:SCREEN-VALUE = hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE NO-ERROR.
    ASSIGN hValueOverlay1:MODIFIED = FALSE
           hValueOverlay1:TAB-STOP = YES.
  END.
  IF VALID-HANDLE(hValueOverlay2) THEN DO:
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hValueOverlay2,"ColumnValue_2"). 
    IF hValueOverlay2:SENSITIVE THEN
      ASSIGN hValueOverlay2:SCREEN-VALUE = hBuffer:BUFFER-FIELD("ColumnValue_2"):BUFFER-VALUE
             hValueOverlay2:MODIFIED = FALSE
             hValueOverlay2:TAB-STOP = YES.
  END.

  IF cLastOverlay = "ColumnValue_2" AND hValueOverlay2:SENSITIVE THEN 
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hValueOverlay2)).
  ELSE IF cLastOverlay = "ColumnValue_1" AND VALID-HANDLE(hValueOverlay1) THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hValueOverlay1)).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hValueOverlay)).

  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.

/*   ASSIGN hColumnTextOverlay:Y = hColumnTextOverlay:Y + 1                           */
/*          hColumnTextOverlay:SCREEN-VALUE = hColumnTextOverlay:SCREEN-VALUE + " >>" */
/*          hColumnTextOverlay:BGCOLOR = 15.                                          */

  IF NOT hValueOverlay2:SENSITIVE THEN DO:
    hValueOverlay2:SCREEN-VALUE = "".
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"lastenabledoverlay",STRING(hValueOverlay1)).
  END.

  ASSIGN cCurrOverlayValue  = hValueOverlay:SCREEN-VALUE
         cCurrOverlay1Value = hValueOverlay1:SCREEN-VALUE.

  DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).

  IF LAST-EVENT:LABEL = "enter" THEN DO:
    DYNAMIC-FUNCTION("setWidgetEnter",hValueOverlay).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"currentoverlaywidget",STRING(hValueOverlay)).
  END.
  ELSE DO:
    iMousePos = DYNAMIC-FUNCTION("getMousePosition",FRAME {&FRAME-NAME}:HANDLE,"x").
  
    IF VALID-HANDLE(hValueOverlay2) AND hValueOverlay2:SENSITIVE AND iMousePos > hValueOverlay2:X THEN
      DYNAMIC-FUNCTION("setWidgetEnter",hValueOverlay2).
    ELSE IF iMousePos > hValueOverlay1:X THEN
      DYNAMIC-FUNCTION("setWidgetEnter",hValueOverlay1).
    ELSE
      DYNAMIC-FUNCTION("setWidgetEnter",hValueOverlay).
  END.
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
  DISPLAY tbSaveFilter 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolbar rectBrowse tbSaveFilter 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnterBrowseFillIn C-Win 
PROCEDURE EnterBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFillIn AS HANDLE NO-UNDO.

RUN SUPER.

hFillIn = DYNAMIC-FUNCTION("getCurrentObject").
IF hFillIn = hValueOverlay1 AND hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = "" AND
   hFillIn:SCREEN-VALUE = "?" THEN DO:
  ASSIGN hFillIn:SCREEN-VALUE = ""
         hFillIn:MODIFIED = NO.
  IF hValueOverlay:SCREEN-VALUE = "?" THEN
    ASSIGN hValueOverlay:SCREEN-VALUE = ""
           hValueOverlay:MODIFIED = NO.
END.

hLastWidget = hFillIn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExitRecord C-Win 
PROCEDURE ExitRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "close" TO THIS-PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HjelpRecord C-Win 
PROCEDURE HjelpRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
DEF VAR bDropDown       AS LOG    NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR cExtFilter      AS CHAR   NO-UNDO.
DEF VAR cExtBuffers     AS CHAR   NO-UNDO.
DEF VAR cExtFields      AS CHAR   NO-UNDO.
DEF VAR cExtFieldDef    AS CHAR   NO-UNDO.
DEF VAR cExtFieldLab    AS CHAR   NO-UNDO.
DEF VAR cExtFieldForm   AS CHAR   NO-UNDO.
DEF VAR cExtFieldType   AS CHAR   NO-UNDO.
DEF VAR cExtFieldAttr   AS CHAR   NO-UNDO.
DEF VAR cExtDbFldAttr   AS CHAR   NO-UNDO.
DEF VAR bAdjRowViewVal  AS LOG    NO-UNDO.
DEF VAR cFormat         AS CHAR   NO-UNDO.
DEF VAR hColumn         AS HANDLE NO-UNDO. 
DEF VAR cValue          AS CHAR   NO-UNDO.
DEF VAR bValue          AS LOG    NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cKeepFilterOpen = DYNAMIC-FUNCTION("getAttribute",hFilter,"keepfilteropen").
  DYNAMIC-FUNCTION("DeleteObject",hBrowse).
  DYNAMIC-FUNCTION("DeleteObject",hFilter).
  DYNAMIC-FUNCTION("DeleteObject",hValueOverlay).
  DELETE OBJECT hNewBuff NO-ERROR.
  rRepos = ?.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",rectBrowse:HANDLE,1,"",
                             "temp-table"
                           + ";!+BufferName|CHARACTER|x(30)||"      + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Tabell" ELSE "Table") 
                           + ";!+GroupOperator|CHARACTER|x(5)||"    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "GrpOp" ELSE "GroupOp") 
                           + ";!+FieldGroup|INTEGER|>9||"           + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "FeltGrp" ELSE "FieldGr")
                           + ";!+FieldOperator|CHARACTER|x(5)||"    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "FeltOp" ELSE "FieldOp") 
                           + ";+ColumnLabel|CHARACTER|x(50)||"      + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kolonne" ELSE "Column") 
                           + ";!+Operator|CHARACTER|x(8)||Operator" 
                           + ";+ColumnValue|CHARACTER|x(20)||"      + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fra" ELSE "From")  
                           + ";!+Operator_1|CHARACTER|x(8)||Operator" 
                           + ";+ColumnValue_1|CHARACTER|x(20)||"     + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Til" ELSE "To")  
                           + ";!+Operator_2|CHARACTER|x(8)||Operator" 
                           + ";+ColumnValue_2|CHARACTER|x(20)||"     + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Søkekriterium" ELSE "Criteria")  
                           + ";+!ColumnName|CHARACTER" 
                           + ";+!DataType|CHARACTER" 
                           + ";+!Format|CHARACTER" 
                           + ";+!Seq|INTEGER" 
                           + ";+!HiddenGroupOperator|CHARACTER" 
                           + ";+!DuplicateRow|LOGICAL"
                           + ";+!NumInUse|INTEGER"
                           + ";+!CharValueTrue|CHARACTER"
                           + ";+!CharValueFalse|CHARACTER"
                             ,
                             "where false",
                             "").
  ASSIGN hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 120
         hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 90
         hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 90
         hSearchColumn = hBrowse:GET-BROWSE-COLUMN(4)
         hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
         .

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

  tbSaveFilter:CHECKED = SavedFilterEqualsFilter().

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
/*                   IF CAN-DO(cExtFields,ENTRY(ix,cFilterFields)) THEN                                     */
/*                     ENTRY(2,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtFields),cExtFieldAttr,";"),"|")     */
/*                   ELSE IF ihQueryObject:TYPE = "browse" THEN                                             */
/*                     ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):FORMAT*/
/*                   ELSE                                                                                   */
/*                     ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):FORMAT      */
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
    bAdjRowViewVal = AdjustRowViewValues(ENTRY(ix,cFilterFields)).

/*    IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "LOGICAL" AND TRIM(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) = "true"  THEN      */
/*      hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "yes".                                                                                 */
/*    ELSE IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "LOGICAL" AND TRIM(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) = "false"  THEN*/
/*      hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "no".                                                                                  */

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

    IF NOT bAdjRowViewVal THEN
      DO iy = 1 TO 2:
        IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields)) NE "" THEN DO:
          ASSIGN hBuffer:BUFFER-FIELD("Operator_" + STRING(iy)):BUFFER-VALUE =
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 hBuffer:BUFFER-FIELD("ColumnValue_" + STRING(iy)):BUFFER-VALUE =
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
                 .
        END.
        ELSE IF iy = 1 AND hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE NE "" THEN
          hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE.
      END.

    IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "logical" OR
       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownfields_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" OR
       DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterdropdownvaluelist_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN
      bDropDown = YES.

    IF rRepos = ? AND hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE NE "" THEN 
      rRepos = hBuffer:ROWID.
  END.

  ReshapeFilter("View").

  IF bDropDown THEN
    hBrowse:ROW-HEIGHT-CHARS = .75.

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
DEF VAR hSubMenuAdv     AS HANDLE NO-UNDO.
DEF VAR cDefaultFrameFont  AS CHAR   NO-UNDO.

cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont").
IF cDefaultFrameFont NE "" THEN
  FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             "StartDynFilter;&OK"
                           + ",Apply;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "&Bruk" ELSE "&Apply")
                           + (IF DYNAMIC-FUNCTION("Scandinavian") THEN ",|Innstillinger" ELSE ",|Settings")
                           + ",ClearFilter" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN ";B&lank" ELSE ";C&lear")
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
                                ",FetchFilter;" +
                                (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Hent lagret filter&" ELSE "Fetch saved filter&") + "¤menu"
                              ELSE "")
                           + ",Exit;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "&Avbryt" ELSE "&Cancel")
                           + (IF DYNAMIC-FUNCTION("Scandinavian") THEN ",|Hjelp" ELSE ",|Help")
                           + ",Hjelp" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "|Hjelp;&Hjelp" ELSE "|Help;&Help")
                           + ",ViewQuery" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "|Innstillinger;Vis spørring" ELSE "|Settings;View query")
                              + ";toggle¤menu"
                             ,"maxborder,enable").
    
  IF DYNAMIC-FUNCTION("Scandinavian") THEN
    cLogicalDropDown = "||Ja|Ja|Nei|Nei".
  ELSE 
    cLogicalDropDown = "||Yes|Yes|No|No".

  RUN InitFilter.

  hMenuItemViewQ = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"ViewQuery","menu-item").

  IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"viewqueryfilter") = "on" THEN
    hMenuItemViewQ:CHECKED = TRUE.

  IF ihQueryObject:TYPE = "query" THEN
    cBaseTable = ihQueryObject:GET-BUFFER-HANDLE(1):NAME.
  ELSE
    cBaseTable = ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):NAME.

  IF NOT DYNAMIC-FUNCTION("getIsUserSettingInstalled") OR DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"disableSaveFilter") = "yes" THEN
    tbSaveFilter:HIDDEN =  YES.
END.

DYNAMIC-FUNCTION("setMyEnableSaveSettingsMenu",NO) NO-ERROR.
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,250,150).

SUBSCRIBE TO "InitStringTranslation" ANYWHERE.

DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = MIN(SESSION:HEIGHT-PIXELS - 100,40 + ix * 21).
APPLY "window-resized" TO {&WINDOW-NAME}.

APPLY "value-changed" TO hBrowse.
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

DEF VAR cCurrValue AS CHAR NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

IF NOT CAN-DO(iocTypeList,"LogicalCombo") THEN
  iocTypeList = iocTypeList + ",LogicalCombo".

cLogicalDropDown = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"LogicalCombo",cLogicalDropDown).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn C-Win 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDataType AS CHAR NO-UNDO.
IF DYNAMIC-FUNCTION("getCurrentObject") = hValueOverlay THEN DO: 
  IF hValueOverlay:SCREEN-VALUE = "?" AND
     cCurrOverlayValue = cCurrOverlay1Value THEN
    bDelVal2 = YES.
  ELSE DO:
    ASSIGN bDelVal2 = NO
           cDataType = hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE.
    CASE cDataType:
      WHEN "character" THEN bSetVal2 = hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE < hValueOverlay:SCREEN-VALUE.
      WHEN "date"      THEN bSetVal2 = hValueOverlay:INPUT-VALUE NE ? AND 
                                       (DATE(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) = ? OR 
                                        DATE(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) < DATE(hValueOverlay:SCREEN-VALUE)).
      WHEN "decimal"   THEN bSetVal2 = DECIMAL(hValueOverlay:SCREEN-VALUE) NE 0 AND DECIMAL(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) < DECIMAL(hValueOverlay:SCREEN-VALUE).
      WHEN "integer"   THEN bSetVal2 = INT(hValueOverlay:SCREEN-VALUE) NE 0 AND INT(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) < INT(hValueOverlay:SCREEN-VALUE).
      WHEN "logical"   THEN bSetVal2 = hValueOverlay:MODIFIED.
    END.
  END.
END.

RUN SUPER.

bSetVal2 = NO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
ASSIGN hBrowse:GET-BROWSE-COLUMN(2):LABEL-BGCOLOR = 7
       hBrowse:GET-BROWSE-COLUMN(3):LABEL-BGCOLOR = 7
       hBrowse:GET-BROWSE-COLUMN(4):LABEL-BGCOLOR = 7
       .

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
  hPinButton:LOAD-IMAGE("gif/pushout.gif").
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hFilter,"keepfilteropen","yes").
  hPinButton:LOAD-IMAGE("gif/pushin.gif").
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

IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE NE "character" THEN
 hSearchColumn:BGCOLOR = 8.

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
DYNAMIC-FUNCTION("SetDynFilter",
                 "Save", 
                 hFilter,
                 ihQueryObject,
                 ihParent:FILE-NAME,
                 "").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFilter C-Win 
PROCEDURE SetFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iy         AS INT NO-UNDO.
DEF VAR cOperator  AS CHAR NO-UNDO.


hBrowse:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:

  ASSIGN hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE =            
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GroupOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GroupOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)
                 ELSE ""
         hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldGroup_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN
                   INT(DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldGroup_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE))
                 ELSE 0
         hBuffer:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE =            
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)
                 ELSE "AND"
         hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = 
                 DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"operatorInUse_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)           
         hBuffer:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE =            
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"HiddenGroupOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"HiddenGroupOperator_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)
                 ELSE ""
         .

  IF CAN-DO("begins,matches",hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE) THEN
    hBuffer:BUFFER-FIELD("ColumnValue_2"):BUFFER-VALUE = 
        DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE).
  ELSE DO:
    hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = 
      DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE).

    IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_1_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE) NE "" THEN DO:
      ASSIGN hBuffer:BUFFER-FIELD("Operator_1"):BUFFER-VALUE =
               DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_1_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)
             hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE =
               DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_1_" + hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE)
             .
    END.
    ELSE IF hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE NE "" AND hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = "" THEN
      hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE.

  END.

  IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "LOGICAL" AND TRIM(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) = "true"  THEN
    hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "yes".
  ELSE IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "LOGICAL" AND TRIM(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) = "false"  THEN
    hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = "no".


  hBrowse:QUERY:GET-NEXT().

END.

RUN InvokeMethod(hBrowse,"OpenQuery").

RUN ApplyRecord.

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
DEF VAR bFilterSet AS LOG NO-UNDO.

SaveModifiedOverlay().

ReshapeFilter("save").

bFilterSet = hBuffer:FIND-FIRST("WHERE Operator NE '' OR Operator_1 NE '' OR Operator_2 NE ''") NO-ERROR.

DYNAMIC-FUNCTION("setCurrentObject",hToolbar).

RUN SUPER.

PUBLISH "RefreshAccumTotals" (ihQueryObject).

IF tbSaveFilter:CHECKED IN FRAME {&FRAME-NAME} THEN
  RUN SaveFilterRecord.

IF NOT bApply OR NOT ihParent:CURRENT-WINDOW:SENSITIVE THEN 
  APPLY "close" TO THIS-PROCEDURE.
ELSE DO:
  ReshapeFilter("view").
  APPLY "entry" TO FRAME {&FRAME-NAME}.
END.

IF NOT bFilterSet THEN PUBLISH "FilterCleared" (ihQueryObject).

bApply = NO.

DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

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
DEF VAR cDataType AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hValueOverlay THEN DO: 
  IF (hValueOverlay:SCREEN-VALUE = "?" OR hValueOverlay:SCREEN-VALUE = "") AND
     cCurrOverlayValue = cCurrOverlay1Value THEN
    bDelVal2 = YES.
  ELSE DO:
    ASSIGN bDelVal2 = NO
           cDataType = hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE.
    CASE cDataType:
      WHEN "character" THEN bSetVal2 = hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE < hValueOverlay:SCREEN-VALUE.
      WHEN "date"      THEN bSetVal2 = hValueOverlay:INPUT-VALUE NE ? AND 
                                       (DATE(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) = ? OR 
                                        DATE(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) < DATE(hValueOverlay:SCREEN-VALUE)).
      WHEN "decimal"   THEN bSetVal2 = INTEGER(hValueOverlay:SCREEN-VALUE) NE 0 AND INT(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) < INT(hValueOverlay:SCREEN-VALUE).
      WHEN "integer"   THEN bSetVal2 = DECIMAL(hValueOverlay:SCREEN-VALUE) NE 0 AND DECIMAL(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE) < DECIMAL(hValueOverlay:SCREEN-VALUE).
      WHEN "logical"   THEN bSetVal2 = hValueOverlay:MODIFIED.
    END.
  END.

END.
ELSE DO:
  hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = IF hValueOverlay1:SCREEN-VALUE = ? THEN "" ELSE hValueOverlay1:SCREEN-VALUE.
  RETURN.
END.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hValueOverlay THEN DO:
  IF bDelVal2 THEN 
    ASSIGN hValueOverlay1:SCREEN-VALUE = "?"
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = "".
  IF bSetVal2 THEN 
    ASSIGN hValueOverlay1:SCREEN-VALUE = hValueOverlay:SCREEN-VALUE
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE.

END.
ELSE
  ASSIGN hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = hValueOverlay1:SCREEN-VALUE.

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

/* IF hOperatorOverlay:SCREEN-VALUE = ? THEN                    */
/*   ASSIGN hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "=" */
/*          hOperatorOverlay:SCREEN-VALUE = "=".                */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustRowViewValues C-Win 
FUNCTION AdjustRowViewValues RETURNS LOGICAL
  ( INPUT icFieldName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cVal   AS CHAR NO-UNDO.
DEF VAR cVal0  AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_0_" + icFieldName) NE "" THEN DO:
  ASSIGN cVal0 = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_0_" + icFieldName)
         cVal  = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + icFieldName)
         .
  IF cVal0 LT cVal THEN
    ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = cVal0
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = cVal
           .
  ELSE IF cVal LT cVal0 THEN
    ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = cVal
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = cVal0
           .
  ELSE
    ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE   = cVal0
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = cVal0
           .
  RETURN YES.  
END.
ELSE RETURN NO.

/*
DEF VAR cVal1 AS CHAR NO-UNDO.
DEF VAR cVal2  AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_0_" + icFieldName) NE "" THEN 
  ASSIGN cVal1 = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_0_" + icFieldName)
         cVal2  = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + icFieldName)
         .
ELSE IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_1_" + icFieldName) NE "" THEN 
  ASSIGN cVal1 = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_1_" + icFieldName)
         cVal2  = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + icFieldName)
         .
IF cVal1 NE "" THEN DO:
  IF cVal1 LT cVal2 THEN
    ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = cVal1
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = cVal2
           .
  ELSE IF cVal2 LT cVal1 THEN
    ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = cVal2
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = cVal1
           .
  ELSE
    ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE   = cVal1
           hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = cVal1
           .
  RETURN YES.  
END.
ELSE RETURN NO.
*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReshapeFilter C-Win 
FUNCTION ReshapeFilter RETURNS LOGICAL
  ( INPUT icShapeTo AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: icShapeTo = "save": 
           Reshape the filter setting to match the JukeBox standard filter by adding rows for the two extra value columns
           icShapeTo = "view": 
           Reshape the filter setting to match this filter user interface by duplicate rows for the same buffer column
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQry    AS HANDLE NO-UNDO.
DEF VAR hBuf2   AS HANDLE NO-UNDO.
DEF VAR rRepos  AS ROWID  NO-UNDO.

IF icShapeTo = "save" THEN DO:
  DYNAMIC-FUNCTION("msetAttribute",ihQueryObject,"operatorInUse","").
  DYNAMIC-FUNCTION("msetAttribute",ihQueryObject,"filtervalue","").
END.

IF hBuffer:AVAIL THEN rRepos = hBuffer:ROWID.

CREATE BUFFER hBuf2 FOR TABLE hBuffer.
CREATE QUERY hQry.
hQry:SET-BUFFERS(hBuffer).
IF icShapeTo = "save" THEN DO:
  hQry:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
  hQry:QUERY-OPEN().
  hQry:GET-FIRST().
  REPEAT WHILE NOT hQry:QUERY-OFF-END:
    hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "".
    hQry:GET-NEXT().
  END.
  hQry:QUERY-CLOSE().
END.
hQry:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + IF icShapeTo = "save" THEN " WHERE DuplicateRow = NO" ELSE "").
hQry:QUERY-OPEN().
hQry:GET-FIRST().
REPEAT WHILE NOT hQry:QUERY-OFF-END:
  IF icShapeTo = "save" AND 
    (hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE NE "" OR
     hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE NE "" OR
     hBuffer:BUFFER-FIELD("ColumnValue_2"):BUFFER-VALUE NE "")
     THEN DO:
    IF hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE AND 
       hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE NE "" THEN
      hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "=".
    ELSE DO:
      IF hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE NE "" THEN DO:
        IF hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE NE "" THEN
          hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = ">=".
        ELSE IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "character" THEN
          hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "BEGINS".
        ELSE
          hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = "=".
      END.
      IF hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE NE "" THEN DO:
        hBuf2:BUFFER-CREATE().
        hBuf2:BUFFER-COPY(hBuffer,"Seq").
        ASSIGN hBuf2:BUFFER-FIELD("Seq"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE + 1
               hBuf2:BUFFER-FIELD("Operator"):BUFFER-VALUE = "<="
               hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE
               hBuf2:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE = YES
               hBuf2:BUFFER-FIELD("NumInUse"):BUFFER-VALUE = 1
               . 
      END.
    END.
    IF hBuffer:BUFFER-FIELD("ColumnValue_2"):BUFFER-VALUE NE "" THEN DO:
      hBuf2:BUFFER-CREATE().
      hBuf2:BUFFER-COPY(hBuffer,"Seq").
      ASSIGN hBuf2:BUFFER-FIELD("Seq"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE + 2
             hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("ColumnValue_2"):BUFFER-VALUE
             hBuf2:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE = YES
             hBuf2:BUFFER-FIELD("NumInUse"):BUFFER-VALUE = 2
             .
      IF INDEX(hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE,"*") = LENGTH(hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) THEN
        hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = SUBSTR(hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE,1,LENGTH(hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE) - 1).
      IF INDEX(hBuf2:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE,"*") > 0 THEN
        hBuf2:BUFFER-FIELD("Operator"):BUFFER-VALUE = "MATCHES".
      ELSE
        hBuf2:BUFFER-FIELD("Operator"):BUFFER-VALUE = "BEGINS".
    END.

    IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "logical" THEN
      ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = ENTRY(LOOKUP(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE,cLogicalDropDown,"|"),cLogDropDownVal,"|")
             hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = ENTRY(LOOKUP(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE,cLogicalDropDown,"|"),cLogDropDownVal,"|")
             .
  END.
  ELSE IF icShapeTo = "view" THEN DO: 

    IF hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE = "logical" THEN
      ASSIGN hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = ENTRY(LOOKUP(hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE,cLogDropDownVal,"|"),cLogicalDropDown,"|")
             hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE = ENTRY(LOOKUP(hBuffer:BUFFER-FIELD("ColumnValue_1"):BUFFER-VALUE,cLogDropDownVal,"|"),cLogicalDropDown,"|")
             .
    IF hBuffer:BUFFER-FIELD("DuplicateRow"):BUFFER-VALUE THEN
      hBuffer:BUFFER-DELETE().
  END.

  hQry:GET-NEXT().
END.

DELETE OBJECT hQry.
DELETE OBJECT hBuf2.

IF rRepos NE ? THEN
  hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SavedFilterEqualsFilter C-Win 
FUNCTION SavedFilterEqualsFilter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSavedQueryFilter     AS CHAR   NO-UNDO.
DEF VAR cSavedPrescanFilter   AS CHAR   NO-UNDO.
DEF VAR cSavedCalcFieldFilter AS CHAR   NO-UNDO.
DEF VAR cCurrQueryFilter      AS CHAR   NO-UNDO.
DEF VAR cCurrPrescanFilter    AS CHAR   NO-UNDO.
DEF VAR cCurrCalcFieldFilter  AS CHAR   NO-UNDO.
DEF VAR cObjectName           AS CHAR   NO-UNDO.
DEF VAR cContext              AS CHAR   NO-UNDO.
DEF VAR hSourceProc           AS HANDLE NO-UNDO.

ASSIGN hSourceProc           = DYNAMIC-FUNCTION("getObjectSourceFileHandle",ihQueryObject)
       cObjectName           = DYNAMIC-FUNCTION("getObjectName",ihQueryObject)
       cContext              = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"usersettingcontext")

       cSavedQueryFilter     = DYNAMIC-FUNCTION("getUserSetting",
                                hSourceProc:FILE-NAME,
                                cObjectName,
                                cContext,
                                "queryFilter")
       cSavedPrescanFilter   = DYNAMIC-FUNCTION("getUserSetting",
                                hSourceProc:FILE-NAME,
                                cObjectName,
                                cContext,
                                "prescanQueryFilter")
       cSavedCalcFieldFilter = DYNAMIC-FUNCTION("getUserSetting",
                                hSourceProc:FILE-NAME,
                                cObjectName,
                                cContext,
                                "calcFieldFilter")

       cCurrQueryFilter      = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"queryFilter")
       cCurrPrescanFilter    = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"prescanQueryFilter")
       cCurrCalcFieldFilter  = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"calcFieldFilter")

       cSavedQueryFilter     = RIGHT-TRIM(cSavedQueryFilter,"|")
       cSavedQueryFilter     = TRIM(cSavedQueryFilter)
       cSavedPrescanFilter   = RIGHT-TRIM(cSavedPrescanFilter,"|")
       cSavedPrescanFilter   = TRIM(cSavedPrescanFilter)
       cSavedCalcFieldFilter = RIGHT-TRIM(cSavedCalcFieldFilter,"|")
       cSavedCalcFieldFilter = TRIM(cSavedCalcFieldFilter)

       cCurrQueryFilter      = RIGHT-TRIM(cCurrQueryFilter,"|")
       cCurrQueryFilter      = TRIM(cCurrQueryFilter)
       cCurrPrescanFilter    = RIGHT-TRIM(cCurrPrescanFilter,"|")
       cCurrPrescanFilter    = TRIM(cCurrPrescanFilter)
       cCurrCalcFieldFilter  = RIGHT-TRIM(cCurrCalcFieldFilter,"|")
       cCurrCalcFieldFilter  = TRIM(cCurrCalcFieldFilter)
       .

IF cSavedQueryFilter = "" AND cSavedPrescanFilter = "" AND cSavedCalcFieldFilter = "" THEN
  RETURN NO.

RETURN cSavedQueryFilter     = cCurrQueryFilter AND
       cSavedPrescanFilter   = cCurrPrescanFilter AND
       cSavedCalcFieldFilter = cCurrCalcFieldFilter
       .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveModifiedOverlay C-Win 
FUNCTION SaveModifiedOverlay RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hLastWidget) AND hLastWidget:TYPE = "fill-in" AND hLastWidget:MODIFIED THEN
  APPLY "return" TO hLastWidget.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

