&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
DEF INPUT  PARAM icQryList    AS CHAR NO-UNDO.
DEF INPUT  PARAM icTbList     AS CHAR NO-UNDO.
DEF INPUT  PARAM icFmList     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocControl   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCode   AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cFirstQry AS CHAR NO-UNDO.
DEF VAR cFirstFm  AS CHAR NO-UNDO.
DEF VAR cFirstTb  AS CHAR NO-UNDO.
DEF VAR iNumQry   AS INT  NO-UNDO.
DEF VAR iNumTb    AS INT  NO-UNDO.
DEF VAR iNumFm    AS INT  NO-UNDO.

DEF TEMP-TABLE ttControl
    FIELD cCat     AS CHAR 
    FIELD cName    AS CHAR LABEL "Procedure name" FORMAT "x(40)"
    FIELD cDesc    AS CHAR LABEL "Description"    FORMAT "x(30)"
    FIELD cCode    AS CHAR LABEL "Code" FORMAT "x(30)"
    FIELD cOrgCode AS CHAR 
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttControl

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 cName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttControl
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttControl.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttControl
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttControl


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbApplyToQry cmbApplyToFm BROWSE-4 edDesc ~
edCode btnOk btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cmbApplyToQry cmbApplyToFm edDesc edCode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillFromConfig C-Win 
FUNCTION fillFromConfig RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD importConfig C-Win 
FUNCTION importConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReplaceWithObject C-Win 
FUNCTION ReplaceWithObject RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOk 
     LABEL "Ok" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbApplyToFm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Substitute FieldMap object" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cmbApplyToQry AS CHARACTER FORMAT "X(256)":U 
     LABEL "Substitute Browse/Query object" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 35.8 BY 1 NO-UNDO.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 107 BY 12.81 NO-UNDO.

DEFINE VARIABLE edDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 106.8 BY 7.38 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttControl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      cName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 22.43 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbApplyToQry AT ROW 1.24 COL 122 COLON-ALIGNED WIDGET-ID 4
     cmbApplyToFm AT ROW 2.38 COL 122 COLON-ALIGNED WIDGET-ID 10
     BROWSE-4 AT ROW 2.86 COL 3 WIDGET-ID 200
     edDesc AT ROW 3.86 COL 53.2 NO-LABEL WIDGET-ID 16
     edCode AT ROW 12.48 COL 53 NO-LABEL WIDGET-ID 18
     btnOk AT ROW 25.48 COL 128.8 WIDGET-ID 6
     btnCancel AT ROW 25.48 COL 144.8 WIDGET-ID 8
     "Description:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3 COL 53.6 WIDGET-ID 20
     "Code:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.71 COL 53.2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.2 BY 25.81 WIDGET-ID 100.


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
         TITLE              = "Select .Net user-control"
         HEIGHT             = 25.76
         WIDTH              = 160.2
         MAX-HEIGHT         = 26.14
         MAX-WIDTH          = 162
         VIRTUAL-HEIGHT     = 26.14
         VIRTUAL-WIDTH      = 162
         ALWAYS-ON-TOP      = yes
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-4 cmbApplyToFm DEFAULT-FRAME */
ASSIGN 
       edDesc:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttControl.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Select .Net user-control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Select .Net user-control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON VALUE-CHANGED OF BROWSE-4 IN FRAME DEFAULT-FRAME
DO:
  ASSIGN edCode:SCREEN-VALUE = ttControl.cCode
         edDesc:SCREEN-VALUE = ttControl.cDesc.
  ReplaceWithObject().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  IF AVAIL ttControl THEN
    ASSIGN ocControl = ttControl.cName
           ocCode = edCode:SCREEN-VALUE.
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbApplyToFm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbApplyToFm C-Win
ON VALUE-CHANGED OF cmbApplyToFm IN FRAME DEFAULT-FRAME /* Substitute FieldMap object */
DO:
  ReplaceWithObject().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbApplyToQry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbApplyToQry C-Win
ON VALUE-CHANGED OF cmbApplyToQry IN FRAME DEFAULT-FRAME /* Substitute Browse/Query object */
DO:
  ReplaceWithObject().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edCode C-Win
ON LEAVE OF edCode IN FRAME DEFAULT-FRAME
DO:
  IF SELF:MODIFIED THEN
    ttControl.cCode = SELF:SCREEN-VALUE.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
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
  DISPLAY cmbApplyToQry cmbApplyToFm edDesc edCode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cmbApplyToQry cmbApplyToFm BROWSE-4 edDesc edCode btnOk btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitControls C-Win 
PROCEDURE InitControls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT fillFromConfig() THEN DO:
  CREATE ttControl.
  ASSIGN ttControl.cCat  = "override"
         ttControl.cName = "JBoxFpSpread"
         ttControl.cDesc = "FarPoint spread sheet."
                         + CHR(10) + "Parameter for handle may be for temp-table, buffer, browse or query."
                         + CHR(10) + "Second parameter controls if labels should be added."                        
         ttControl.cCode = "  oJBoxFpSpread = NEW JBoxFpSpread(THIS-PROCEDURE,JBoxFpSpread:HANDLE)."
                         + CHR(10) + "  oJBoxFpSpread:RegisterWithJukeBox(YES)."
                         + (IF cFirstQry NE "" THEN
                              CHR(10) + "  " + cFirstQry + ":OpenQuery()."
                            + CHR(10) + "  oJBoxFpSpread:InitializeSpreadSheetFromHandle(" + cFirstQry + ":BUFFER-HANDLE,YES)."
                            ELSE "") 
         .
       
  CREATE ttControl.
  ASSIGN ttControl.cCat  = "override"
         ttControl.cName = "JBoxDevExEdit"
         ttControl.cDesc = "Developer Express rtf editor"
         ttControl.cCode = "  oJBoxDevExEdit = NEW JBoxDevExEdit(THIS-PROCEDURE,Comments:HANDLE)."
                         + CHR(10) + "  oJBoxDevExEdit:RegisterWithJukeBox(YES)."
                         + CHR(10) + "  oJBoxDevExEdit:CreateDisplayLink(hFieldMap,'Comments')."
                         + CHR(10) + "  oJBoxDevExEdit:cDocLoadContext = 'Customer.Comments'."
                         + CHR(10) + "  oJBoxDevExEdit:cDocLoadIdFields = 'CustNum'."
                         + CHR(10) + "  oJBoxDevExEdit:cDocLoadFormat = 'html'."
         .

  CREATE ttControl.
  ASSIGN ttControl.cCat  = "override"
         ttControl.cName = "JBoxUltraGantt"
         ttControl.cDesc = "Infragistics Gantt chart"
         ttControl.cCode = "  oJBoxUltraGantt = NEW JBoxUltraGantt(THIS-PROCEDURE,JBoxUltraGantt:HANDLE)."
                         + CHR(10) + "  oJBoxUltraGantt:RegisterWithJukeBox(YES)."
                         + CHR(10) + "  oJBoxUltraGantt:iDeltaHeight = -40."
                         + CHR(10) + "  oJBoxUltraGantt:addColumn('Status','CHARACTER',4,YES)."
         .
END.

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
  ASSIGN cFirstQry = ENTRY(1,icQryList)
         cFirstFm  = ENTRY(1,icFmList)
         cFirstTb  = ENTRY(1,icTbList)
         iNumQry   = NUM-ENTRIES(icQryList)
         iNumFm    = NUM-ENTRIES(icFmList)
         iNumTb    = NUM-ENTRIES(icTbList)
         cmbApplyToQry:LIST-ITEMS = "," + icQryList
         cmbApplyToFm:LIST-ITEMS = "," + icFmList
         .
  RUN InitControls.

  IF cFirstQry NE "" THEN cmbApplyToQry:SCREEN-VALUE = cFirstQry.
  IF cFirstFm NE "" THEN cmbApplyToFm:SCREEN-VALUE = cFirstFm.

  BROWSE {&BROWSE-NAME}:QUERY:QUERY-PREPARE("FOR EACH ttControl BY ttControl.cName").
  BROWSE {&BROWSE-NAME}:QUERY:QUERY-OPEN().
  APPLY "value-changed" TO BROWSE {&BROWSE-NAME}.

  SESSION:SET-WAIT-STATE("").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillFromConfig C-Win 
FUNCTION fillFromConfig RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cConfig     AS CHAR NO-UNDO.
DEF VAR cLine       AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.

IF SEARCH("template\userControlConfig.txt") NE ? THEN DO:
  cConfig = ImportConfig(SEARCH("template\userControlConfig.txt")).

  IF cConfig NE "" THEN DO ix = 1 TO NUM-ENTRIES(cConfig,CHR(10)):
    cLine = TRIM(ENTRY(ix,cConfig,CHR(10))).

    IF cLine BEGINS "<name>" THEN DO:
      CREATE ttControl.
      ttControl.cName  = SUBSTR(cLine,7).
    END. 
    ELSE IF cLine BEGINS "<desc>" AND AVAIL ttControl THEN 
      ttControl.cDesc = ttControl.cDesc + (IF ttControl.cDesc NE "" THEN CHR(10) ELSE "") + SUBSTR(cLine,7).
    ELSE IF cLine BEGINS "<code>" AND AVAIL ttControl THEN 
      ttControl.cCode = ttControl.cCode + (IF ttControl.cCode NE "" THEN CHR(10) ELSE "") + SUBSTR(cLine,7).
  END.
  RETURN YES.  
END.
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION importConfig C-Win 
FUNCTION importConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput     AS CHAR NO-UNDO.
DEF VAR cReturn    AS CHAR NO-UNDO.

INPUT FROM VALUE(icFileName).

REPEAT:
  IMPORT UNFORMATTED cInput.  
  cReturn = cReturn + cInput + CHR(10).
END.
INPUT CLOSE.

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReplaceWithObject C-Win 
FUNCTION ReplaceWithObject RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQryObject AS CHAR NO-UNDO.
DEF VAR cFmObject  AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF cmbApplyToQry:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" AND cmbApplyToQry:SCREEN-VALUE NE ? THEN 
    cQryObject = cmbApplyToQry:SCREEN-VALUE.
  ELSE
    cQryObject = cFirstQry.

  IF cmbApplyToFm:SCREEN-VALUE NE "" AND cmbApplyToFm:SCREEN-VALUE NE ? THEN
    cFmObject = cmbApplyToFm:SCREEN-VALUE.
  ELSE
    cFmObject = cFirstFm.

  IF cQryObject NE "" THEN
    edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<QueryObject>",cQryObject).
  IF cFmObject NE "" THEN
    edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<FieldMapObject>",cFmObject).

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

