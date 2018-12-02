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
{adeuib/uniwidg.i}              /* Universal widget definition              */

/* Parameters Definitions ---                                           */
DEF INPUT  PARAM ir_U          AS RECID NO-UNDO.
DEF OUTPUT PARAM ocControl     AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocControlType AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocCode        AS CHAR  NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cFirstQry AS CHAR NO-UNDO.
DEF VAR cFirstFm  AS CHAR NO-UNDO.
DEF VAR cFirstTb  AS CHAR NO-UNDO.
DEF VAR iNumQry   AS INT  NO-UNDO.
DEF VAR iNumTb    AS INT  NO-UNDO.
DEF VAR iNumFm    AS INT  NO-UNDO.
DEF VAR cUser     AS CHAR NO-UNDO.

DEF TEMP-TABLE ttProp
    FIELD cCat      AS CHAR 
    FIELD bSelect   AS LOG  LABEL "Select" 
    FIELD cName     AS CHAR LABEL "Property/method" FORMAT "x(50)"
    FIELD cMP       AS CHAR LABEL "P/M"             FORMAT "x"
    FIELD cType     AS CHAR LABEL "(Return) Type"   FORMAT "x(10)"
    FIELD cDesc     AS CHAR LABEL "Description"     FORMAT "x(80)"
    FIELD cCode     AS CHAR LABEL "Code"            FORMAT "x(30)"
    FIELD bReadOnly AS LOG  LABEL "Read-only"       FORMAT "Yes/No"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwProperty

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttProp

/* Definitions for BROWSE brwProperty                                   */
&Scoped-define FIELDS-IN-QUERY-brwProperty cName cMP cType bReadOnly cDesc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwProperty   
&Scoped-define SELF-NAME brwProperty
&Scoped-define QUERY-STRING-brwProperty FOR EACH ttProp
&Scoped-define OPEN-QUERY-brwProperty OPEN QUERY {&SELF-NAME} FOR EACH ttProp.
&Scoped-define TABLES-IN-QUERY-brwProperty ttProp
&Scoped-define FIRST-TABLE-IN-QUERY-brwProperty ttProp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwProperty}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsPropGroup brwProperty edDesc edCode btnOk ~
btnCancel 
&Scoped-Define DISPLAYED-OBJECTS rsPropGroup edDesc edCode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillTT C-Win 
FUNCTION FillTT RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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
     SIZE 17 BY 1.14.

DEFINE BUTTON btnOk 
     LABEL "Ok" 
     SIZE 17 BY 1.14.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 117.8 BY 8.62 NO-UNDO.

DEFINE VARIABLE edDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 117.6 BY 4.1 NO-UNDO.

DEFINE VARIABLE rsPropGroup AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Base query", 1,
"Filter", 2,
"Resize", 3,
"Sorting", 4,
"Statistics", 5,
"Other", 6,
"All", 9
     SIZE 116.8 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwProperty FOR 
      ttProp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwProperty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwProperty C-Win _FREEFORM
  QUERY brwProperty DISPLAY
      cName   
      cMP
      cType
      bReadOnly
cDesc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 117.6 BY 8.81
         TITLE "Property or Method" ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN TOOLTIP "Doubleclick to select property or method".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsPropGroup AT ROW 1.33 COL 3 NO-LABEL WIDGET-ID 24
     brwProperty AT ROW 2.48 COL 2.4 WIDGET-ID 200
     edDesc AT ROW 12.33 COL 2.2 NO-LABEL WIDGET-ID 16
     edCode AT ROW 17.24 COL 2.2 NO-LABEL WIDGET-ID 18
     btnOk AT ROW 26 COL 86 WIDGET-ID 6
     btnCancel AT ROW 26 COL 103.2 WIDGET-ID 8
     "Description:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.62 COL 2.4 WIDGET-ID 20
     "Code:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 16.48 COL 2.4 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.2 BY 26.38 WIDGET-ID 100.


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
         TITLE              = "Properties and methods for browse initialization"
         HEIGHT             = 26.38
         WIDTH              = 120.2
         MAX-HEIGHT         = 26.38
         MAX-WIDTH          = 125.4
         VIRTUAL-HEIGHT     = 26.38
         VIRTUAL-WIDTH      = 125.4
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
/* BROWSE-TAB brwProperty rsPropGroup DEFAULT-FRAME */
ASSIGN 
       brwProperty:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       edDesc:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwProperty
/* Query rebuild information for BROWSE brwProperty
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttProp.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwProperty */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Properties and methods for browse initialization */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Properties and methods for browse initialization */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwProperty
&Scoped-define SELF-NAME brwProperty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwProperty C-Win
ON DEFAULT-ACTION OF brwProperty IN FRAME DEFAULT-FRAME /* Property or Method */
DO:
  IF NOT edCode:SCREEN-VALUE MATCHES "*" + ttProp.cName + "*" THEN
    edCode:SCREEN-VALUE = edCode:SCREEN-VALUE + CHR(10) + ttProp.cCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwProperty C-Win
ON VALUE-CHANGED OF brwProperty IN FRAME DEFAULT-FRAME /* Property or Method */
DO:
  IF AVAIL ttProp THEN
    ASSIGN /* edCode:SCREEN-VALUE = ttProp.cCode */
           edDesc:SCREEN-VALUE = ttProp.cDesc.
  ELSE
    ASSIGN /* edCode:SCREEN-VALUE = "" */
           edDesc:SCREEN-VALUE = "".

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
  ocCode = edCode:SCREEN-VALUE.
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsPropGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsPropGroup C-Win
ON VALUE-CHANGED OF rsPropGroup IN FRAME DEFAULT-FRAME
DO:
  BROWSE {&BROWSE-NAME}:QUERY:QUERY-PREPARE("FOR EACH ttProp" 
                                          + (IF SELF:SCREEN-VALUE NE "9" THEN 
                                              " WHERE cCat = '" + SELF:SCREEN-VALUE + "'"
                                             ELSE "")
                                          + " BY ttProp.cName")
                                            .
  BROWSE {&BROWSE-NAME}:QUERY:QUERY-OPEN().
  APPLY "value-changed" TO BROWSE {&BROWSE-NAME}.
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
  DISPLAY rsPropGroup edDesc edCode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rsPropGroup brwProperty edDesc edCode btnOk btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
DEF VAR cFm     AS CHAR NO-UNDO.
DEF VAR cTable  AS CHAR NO-UNDO.
DEF VAR cOBS    AS CHAR NO-UNDO.

DEF VAR cDefaultFrameFont  AS CHAR   NO-UNDO.

cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont").
IF cDefaultFrameFont NE "" THEN
  FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.

FIND _U WHERE RECID(_U) = ir_U NO-ERROR.

IF AVAIL _U THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN ocControl = "o" + _U._NAME
         cFm = DYNAMIC-FUNCTION("getFmObjectsInUse")
         cTable = DYNAMIC-FUNCTION("getFirstTableList")
         .

  cUser = OS-GETENV("username").

  fillTT().

  edCode:SCREEN-VALUE = DYNAMIC-FUNCTION("getJBoxUserContrProperties",ir_U,ocControl).

  APPLY "value-changed" TO rsPropGroup.

/*   IF NUM-ENTRIES(cFm) > 1 THEN                                     */
/*     MESSAGE "More than one fieldMap object detected: " + cFm SKIP  */
/*             "Please make sure that the references are correct"     */
/*             VIEW-AS ALERT-BOX INFORMATION.                         */
/*   ELSE IF NUM-ENTRIES(cTable) > 1 THEN                             */
/*     MESSAGE "More than one primary table detected: " + cTable SKIP */
/*             "Please make sure that the references are correct"     */
/*             VIEW-AS ALERT-BOX INFORMATION.                         */

  ASSIGN cFm = ENTRY(1,cFm)
         cTable = ENTRY(1,cTable).


  SESSION:SET-WAIT-STATE("").
END.
ELSE MESSAGE "_U record not available"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillTT C-Win 
FUNCTION FillTT RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput        AS CHAR NO-UNDO.
DEF VAR cExceptList   AS CHAR NO-UNDO INIT "localSort,querySort,".
DEF VAR bProtectedSet AS LOG  NO-UNDO.    
DEF VAR bReadParam    AS LOG  NO-UNDO.
DEF VAR cDesc         AS CHAR NO-UNDO.
DEF VAR cCat          AS CHAR NO-UNDO.

IF SEARCH("JBoxBrowseQueryProp.i") NE ? THEN DO:
  INPUT FROM VALUE(SEARCH("JBoxBrowseQueryProp.i")).
  REPEAT:
    IMPORT UNFORMATTED cInput.
    cInput = TRIM(cInput).
    REPEAT WHILE INDEX(cInput,"  ") > 0:
      cInput = REPLACE(cInput,"  "," ").
    END.
    IF cInput MATCHES "*PUBLIC PROPERTY*" AND NOT CAN-DO(cExceptList,ENTRY(4,cInput," ")) THEN DO:
      CREATE ttProp.
      ASSIGN ttProp.cCat   = "6"
             ttProp.cName  = ENTRY(4,cInput," ")
             ttProp.cMP    = "P"
             ttProp.cType  = ENTRY(6,cInput," ")
             ttProp.cCode  = "  " + ocControl + ":" + ttProp.cName + " = "  
             ttProp.cDesc  = (IF INDEX(cInput,"/") > 0 THEN SUBSTR(cInput,INDEX(cInput,"/")) + CHR(10) + CHR(10) ELSE "") 
/*                            + ttProp.cCode */
             ttProp.bReadOnly = bProtectedSet
             bProtectedSet = NO
             .

      IF ttProp.cName MATCHES "*stat*" OR ttProp.cName MATCHES "*flatview*" THEN
        ttProp.cCat = "5".
      ELSE IF ttProp.cName MATCHES "*sort*" THEN
        ttProp.cCat = "4".
      ELSE IF ttProp.cName MATCHES "*query*" THEN
        ttProp.cCat = "1".
      ELSE IF ttProp.cName MATCHES "*filter*" THEN
        ttProp.cCat = "2".

      IF ttProp.cType BEGINS "CHAR" THEN
        ttProp.cCode = ttProp.cCode + '"".'.
      ELSE IF ttProp.cType BEGINS "LOG" THEN
        ttProp.cCode = ttProp.cCode + "YES. /* or NO */".
      ELSE
        ttProp.cCode = ttProp.cCode + ".".
    END.
    ELSE IF cInput MATCHES "*protected set*" THEN
      bProtectedSet = YES.
  END.
  INPUT CLOSE.
END.

IF SEARCH("JBoxBrowse.cls") NE ? THEN DO:
  INPUT FROM VALUE(SEARCH("JBoxBrowse.cls")).
  REPEAT:
    IMPORT UNFORMATTED cInput.
    cInput = TRIM(cInput).
    REPEAT WHILE INDEX(cInput,"  ") > 0:
      cInput = REPLACE(cInput,"  "," ").
    END.
    cDesc = (IF INDEX(cInput,"/") > 0 THEN SUBSTR(cInput,INDEX(cInput,"/")) + CHR(10) + CHR(10) ELSE "").
    cInput = TRIM(ENTRY(1,cInput,"/")).
    cCat   = "".
    IF cInput MATCHES "*Method*set*Resize*" OR cInput MATCHES "*Method*set*move*" THEN 
      cCat = "3".
    ELSE IF cInput MATCHES "*method*set*QuerySort*" THEN 
      cCat = "4".
    ELSE IF cInput MATCHES "*method*set*Filter*" OR cInput MATCHES "*method*setPreScanQuery*" THEN 
      cCat = "2".

    IF cCat NE "" OR bReadParam THEN DO:
      IF bReadParam THEN
        ASSIGN ttProp.cCode = ttProp.cCode + TRIM(ENTRY(1,cInput," "))
               ttProp.cDesc = ttProp.cDesc + TRIM(cInput)
               .
      ELSE DO:
        CREATE ttProp.
        ASSIGN ttProp.cCat   = cCat
               ttProp.cName  = ENTRY(1,ENTRY(4,cInput," "),"(")
               ttProp.cMP    = "M"
               ttProp.cType  = ENTRY(3,cInput," ")
               ttProp.cCode  = "  " + ocControl + ":" + ttProp.cName + "(" + REPLACE(ENTRY(2,ENTRY(4,cInput," "),"("),"):","")
               ttProp.cDesc  = cDesc + (IF cDesc NE "" THEN CHR(10) ELSE "") + ocControl + ":" + ttProp.cName + "(" + ENTRY(2,cInput,"(")
               ttProp.bReadOnly = ?
               .
      END.
      bReadParam    = NOT cInput MATCHES "*).".
      IF NOT bReadParam THEN
        ttProp.cCode = ttProp.cCode + ").".
/*       MESSAGE cInput SKIP(1)                                                      */
/*               NOT cInput MATCHES "*)." SKIP(1)                                    */
/*               'ENTRY(2,ENTRY(4,cInput," "),"(")' ENTRY(2,ENTRY(4,cInput," "),"(") */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                      */
    END.
  END.
  INPUT CLOSE.
END.

RETURN YES.

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
/* DEF VAR ocControl AS CHAR NO-UNDO.  */
DEF VAR cFmObject  AS CHAR NO-UNDO.
DEF VAR cTbObject  AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  IF ocControl NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<QueryObject>",ocControl)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<QueryObject>",ocControl)
           .
  IF cFmObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<FieldMapObject>",cFmObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<FieldMapObject>",cFmObject) 
           .
  IF cTbObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<ToolbarObject>",cTbObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<ToolbarObject>",cTbObject) 
           .

  edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<userid>",cUser).
  edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<now>",STRING(NOW,"99/99/99 hh:mm")).

END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

