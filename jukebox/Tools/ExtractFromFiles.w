&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CompareCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS CompareCat 
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
DEF VAR ix            AS INT NO-UNDO.
DEF VAR bOk           AS LOG NO-UNDO.

DEF VAR bResizeLibLoaded AS LOG NO-UNDO.
DEF VAR hResizeLib   AS HANDLE NO-UNDO.

DEF VAR cSortColumn1  AS CHAR NO-UNDO.
DEF VAR bDesc1        AS LOG NO-UNDO.

DEF VAR cSortColumn2  AS CHAR NO-UNDO.
DEF VAR bDesc2        AS LOG NO-UNDO.

DEF VAR cSortColumn3  AS CHAR NO-UNDO.
DEF VAR bDesc3        AS LOG NO-UNDO.

DEF VAR ghMainAppBuilderWindow AS HANDLE NO-UNDO.
DEF VAR hAppComp               AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttFiles
    FIELD cFileName   AS CHAR 
    FIELD cType       AS CHAR
    FIELD cExt        AS CHAR
    FIELD fSize       AS DEC
    FIELD dModified   AS DATE
    FIELD bShow       AS LOG
    FIELD cFullName   AS CHAR
    INDEX cFileName   cFileName
    .

DEF TEMP-TABLE ttLine
    FIELD cFileName   AS CHAR
    FIELD iLineNo     AS INT
    FIELD cLineText   AS CHAR
    FIELD cBlock      AS CHAR
    FIELD cFullName   AS CHAR
    INDEX idxLine     cFileName iLineNo 
    INDEX cFileName   cFileName
    .

DEF TEMP-TABLE ttUniqueLine
    FIELD cFileName   AS CHAR
    FIELD iLineNo     AS INT
    FIELD cLineText   AS CHAR
    FIELD cBlock      AS CHAR
    FIELD cFullName   AS CHAR
    INDEX idxLine     cFileName iLineNo 
    INDEX cFileName   cFileName
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwFiles

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttFiles ttLine ttUniqueLine

/* Definitions for BROWSE brwFiles                                      */
&Scoped-define FIELDS-IN-QUERY-brwFiles ttFiles.cFileName ttFiles.cExt /* ttFiles.cType */ ttFiles.dModified ttFiles.fSize   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwFiles ttFiles.cFileName   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwFiles ttFiles
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwFiles ttFiles
&Scoped-define SELF-NAME brwFiles
&Scoped-define QUERY-STRING-brwFiles FOR EACH ttFiles
&Scoped-define OPEN-QUERY-brwFiles OPEN QUERY {&SELF-NAME} FOR EACH ttFiles.
&Scoped-define TABLES-IN-QUERY-brwFiles ttFiles
&Scoped-define FIRST-TABLE-IN-QUERY-brwFiles ttFiles


/* Definitions for BROWSE brwText                                       */
&Scoped-define FIELDS-IN-QUERY-brwText ttLine.cFileName ttLine.iLineNo ttLine.cLineText ttLine.cBlock   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwText ttLine.cFileName   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwText ttLine
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwText ttLine
&Scoped-define SELF-NAME brwText
&Scoped-define QUERY-STRING-brwText FOR EACH ttLine
&Scoped-define OPEN-QUERY-brwText OPEN QUERY {&SELF-NAME} FOR EACH ttLine.
&Scoped-define TABLES-IN-QUERY-brwText ttLine
&Scoped-define FIRST-TABLE-IN-QUERY-brwText ttLine


/* Definitions for BROWSE brwUniqueText                                 */
&Scoped-define FIELDS-IN-QUERY-brwUniqueText ttUniqueLine.cFileName ttUniqueLine.iLineNo ttUniqueLine.cLineText ttUniqueLine.cBlock   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwUniqueText ttUniqueLine.cFileName   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwUniqueText ttUniqueLine
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwUniqueText ttUniqueLine
&Scoped-define SELF-NAME brwUniqueText
&Scoped-define QUERY-STRING-brwUniqueText FOR EACH ttUniqueLine
&Scoped-define OPEN-QUERY-brwUniqueText OPEN QUERY {&SELF-NAME} FOR EACH ttUniqueLine.
&Scoped-define TABLES-IN-QUERY-brwUniqueText ttUniqueLine
&Scoped-define FIRST-TABLE-IN-QUERY-brwUniqueText ttUniqueLine


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwFiles}~
    ~{&OPEN-QUERY-brwText}~
    ~{&OPEN-QUERY-brwUniqueText}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cPattern fi-cFilePattern fi-cCat1 ~
btnSearch tbSearchSub tbUniqeFiles tbAllText brwFiles btnExcel ~
brwUniqueText brwText 
&Scoped-Define DISPLAYED-OBJECTS fi-cPattern fi-cFilePattern fi-cCat1 ~
tbSearchSub tbUniqeFiles tbAllText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkForResizeLib CompareCat 
FUNCTION checkForResizeLib RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR CompareCat AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-btnExcel 
       MENU-ITEM m_Tables_to_Excel LABEL "Tables to Excel".


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExcel 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Button 1" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnSearch 
     LABEL "Start search" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi-cCat1 AS CHARACTER FORMAT "X(256)":U INITIAL "." 
     LABEL "Catalog" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cFilePattern AS CHARACTER FORMAT "X(256)":U INITIAL "*.w *.p *.i" 
     LABEL "File pattern" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cPattern AS CHARACTER FORMAT "X(256)":U INITIAL "setAttribute getAttribute" 
     LABEL "Search pattern" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE tbAllText AS LOGICAL INITIAL yes 
     LABEL "View all text" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 TOOLTIP "View all text regardless of selected files in file browser" NO-UNDO.

DEFINE VARIABLE tbSearchSub AS LOGICAL INITIAL no 
     LABEL "Search sub-catalogs" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tbUniqeFiles AS LOGICAL INITIAL no 
     LABEL "Unique files" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "View one line pr file-name" NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 5" 
     SIZE 1 BY 14.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwFiles FOR 
      ttFiles SCROLLING.

DEFINE QUERY brwText FOR 
      ttLine SCROLLING.

DEFINE QUERY brwUniqueText FOR 
      ttUniqueLine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwFiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwFiles CompareCat _FREEFORM
  QUERY brwFiles DISPLAY
      ttFiles.cFileName  FORMAT "x(60)" WIDTH 25  COLUMN-LABEL "FileName"
ttFiles.cExt       FORMAT "x(30)"  WIDTH 7   COLUMN-LABEL "Ext"
/* ttFiles.cType      FORMAT "x(30)" WIDTH 4   COLUMN-LABEL "Type" */
ttFiles.dModified  FORMAT "99/99/9999"      COLUMN-LABEL "Modified"
ttFiles.fSize      FORMAT "->>,>>>,>>9.99"  COLUMN-LABEL "Size"
ENABLE ttFiles.cFileName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 49.2 BY 14.14 FIT-LAST-COLUMN.

DEFINE BROWSE brwText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwText CompareCat _FREEFORM
  QUERY brwText DISPLAY
      ttLine.cFileName        FORMAT "x(60)"  WIDTH 25  COLUMN-LABEL "FileName"
ttLine.iLineNo          FORMAT ">>>>9"            COLUMN-LABEL "LineNo"
ttLine.cLineText        FORMAT "x(256)" WIDTH 40  COLUMN-LABEL "Text"
ttLine.cBlock           FORMAT "x(256)" WIDTH 40  COLUMN-LABEL "Block"
ENABLE ttLine.cFileName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 108 BY 14.14 FIT-LAST-COLUMN.

DEFINE BROWSE brwUniqueText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwUniqueText CompareCat _FREEFORM
  QUERY brwUniqueText DISPLAY
      ttUniqueLine.cFileName        FORMAT "x(60)"  WIDTH 25  COLUMN-LABEL "FileName"
ttUniqueLine.iLineNo          FORMAT ">>>>9"            COLUMN-LABEL "LineNo"
ttUniqueLine.cLineText        FORMAT "x(256)" WIDTH 40  COLUMN-LABEL "Text"
ttUniqueLine.cBlock           FORMAT "x(256)" WIDTH 40  COLUMN-LABEL "Block"
ENABLE ttUniqueLine.cFileName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 108 BY 14.14 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-cPattern AT ROW 1.24 COL 17 COLON-ALIGNED
     fi-cFilePattern AT ROW 2.48 COL 17 COLON-ALIGNED
     fi-cCat1 AT ROW 3.67 COL 17 COLON-ALIGNED
     btnSearch AT ROW 2.43 COL 64 WIDGET-ID 2
     tbSearchSub AT ROW 3.81 COL 58
     tbUniqeFiles AT ROW 3.86 COL 124 WIDGET-ID 4
     tbAllText AT ROW 3.86 COL 144
     brwFiles AT ROW 5 COL 1.6
     btnExcel AT ROW 19.43 COL 155.8
     brwUniqueText AT ROW 5 COL 53 WIDGET-ID 100
     brwText AT ROW 5 COL 53
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 160.8 BY 19.71.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 34.6 ROW 5
         SIZE 42 BY 14.12.


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
  CREATE WINDOW CompareCat ASSIGN
         HIDDEN             = YES
         TITLE              = "Find in files"
         HEIGHT             = 19.67
         WIDTH              = 160.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW CompareCat
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frSplitBarX:MOVE-AFTER-TAB-ITEM (btnExcel:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME frSplitBarX:MOVE-BEFORE-TAB-ITEM (brwUniqueText:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB brwFiles tbAllText DEFAULT-FRAME */
/* BROWSE-TAB brwUniqueText frSplitBarX DEFAULT-FRAME */
/* BROWSE-TAB brwText brwUniqueText DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.71
       FRAME DEFAULT-FRAME:WIDTH            = 160.8.

ASSIGN 
       brwFiles:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       brwText:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       brwUniqueText:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       brwUniqueText:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       btnExcel:POPUP-MENU IN FRAME DEFAULT-FRAME       = MENU POPUP-MENU-btnExcel:HANDLE.

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CompareCat)
THEN CompareCat:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwFiles
/* Query rebuild information for BROWSE brwFiles
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttFiles.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwFiles */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwText
/* Query rebuild information for BROWSE brwText
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttLine.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwText */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwUniqueText
/* Query rebuild information for BROWSE brwUniqueText
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUniqueLine.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwUniqueText */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CompareCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON END-ERROR OF CompareCat /* Find in files */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON WINDOW-CLOSE OF CompareCat /* Find in files */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON WINDOW-RESIZED OF CompareCat /* Find in files */
DO: 
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwFiles
&Scoped-define SELF-NAME brwFiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwFiles CompareCat
ON START-SEARCH OF brwFiles IN FRAME DEFAULT-FRAME
DO:
  IF cSortColumn1 = brwFiles:CURRENT-COLUMN:NAME THEN
    bDesc1 = NOT bDesc1.
  cSortColumn1 = brwFiles:CURRENT-COLUMN:NAME.

  RUN OpenQueryFiles.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwFiles CompareCat
ON VALUE-CHANGED OF brwFiles IN FRAME DEFAULT-FRAME
DO:
  IF NOT tbAllText THEN DO:
    RUN OpenQueryText.
    RUN OpenQueryUniqueText.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwText
&Scoped-define SELF-NAME brwText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwText CompareCat
ON START-SEARCH OF brwText IN FRAME DEFAULT-FRAME
DO: 
  IF cSortColumn2 = brwText:CURRENT-COLUMN:NAME THEN
    bDesc2 = NOT bDesc2.
  cSortColumn2 = brwText:CURRENT-COLUMN:NAME.

  RUN OpenQueryText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwUniqueText
&Scoped-define SELF-NAME brwUniqueText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwUniqueText CompareCat
ON START-SEARCH OF brwUniqueText IN FRAME DEFAULT-FRAME
DO: 
  IF cSortColumn3 = brwUniqueText:CURRENT-COLUMN:NAME THEN
    bDesc3 = NOT bDesc3.
  cSortColumn3 = brwUniqueText:CURRENT-COLUMN:NAME.

  RUN OpenQueryUniqueText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel CompareCat
ON CHOOSE OF btnExcel IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  RUN ToExcelViaFile.p (brwText:HANDLE IN FRAME {&FRAME-NAME},0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch CompareCat
ON CHOOSE OF btnSearch IN FRAME DEFAULT-FRAME /* Start search */
DO:
  RUN StartSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX CompareCat
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX /* Button 5 */
DO:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME fi-cCat1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cCat1 CompareCat
ON RETURN OF fi-cCat1 IN FRAME DEFAULT-FRAME /* Catalog */
DO:
  RUN StartSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Tables_to_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Tables_to_Excel CompareCat
ON CHOOSE OF MENU-ITEM m_Tables_to_Excel /* Tables to Excel */
DO:
  RUN ToExcelViaFile.p (brwText:HANDLE IN FRAME {&FRAME-NAME},0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAllText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAllText CompareCat
ON VALUE-CHANGED OF tbAllText IN FRAME DEFAULT-FRAME /* View all text */
DO:
  ASSIGN tbAllText.
  RUN OpenQueryText.
  RUN OpenQueryUniqueText.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbSearchSub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbSearchSub CompareCat
ON VALUE-CHANGED OF tbSearchSub IN FRAME DEFAULT-FRAME /* Search sub-catalogs */
DO:
  RUN StartSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbUniqeFiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUniqeFiles CompareCat
ON VALUE-CHANGED OF tbUniqeFiles IN FRAME DEFAULT-FRAME /* Unique files */
DO:
  BROWSE brwUniqueText:HIDDEN = NOT SELF:CHECKED.
  BROWSE brwText:HIDDEN = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwFiles
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK CompareCat 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppCompileRecord CompareCat 
PROCEDURE AppCompileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hAppComp) THEN
  RUN AppComp.w PERSIST SET hAppComp.


DO WITH FRAME {&FRAME-NAME}:
  DO ix = 1 TO BROWSE brwFiles:NUM-SELECTED-ROWS:
    IF BROWSE brwFiles:FETCH-SELECTED-ROW(ix) THEN DO:
      DYNAMIC-FUNCTION("LoadFiles" IN hAppComp,BROWSE brwFiles:HANDLE:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFullName"):BUFFER-VALUE).
    END.
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppCompileTextRecord CompareCat 
PROCEDURE AppCompileTextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hAppComp) THEN
  RUN AppComp.w PERSIST SET hAppComp.


DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("LoadFiles" IN hAppComp,BROWSE brwText:HANDLE:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFullName"):BUFFER-VALUE, ghMainAppBuilderWindow).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppCompileUniqueTextRecord CompareCat 
PROCEDURE AppCompileUniqueTextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hAppComp) THEN
  RUN AppComp.w PERSIST SET hAppComp.

DO WITH FRAME {&FRAME-NAME}:
  DO ix = 1 TO BROWSE brwUniqueText:NUM-SELECTED-ROWS:
    IF BROWSE brwUniqueText:FETCH-SELECTED-ROW(ix) THEN DO:
      DYNAMIC-FUNCTION("LoadFiles" IN hAppComp,BROWSE brwUniqueText:HANDLE:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFullName"):BUFFER-VALUE).
    END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI CompareCat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CompareCat)
  THEN DELETE WIDGET CompareCat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI CompareCat  _DEFAULT-ENABLE
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
  DISPLAY fi-cPattern fi-cFilePattern fi-cCat1 tbSearchSub tbUniqeFiles 
          tbAllText 
      WITH FRAME DEFAULT-FRAME IN WINDOW CompareCat.
  ENABLE fi-cPattern fi-cFilePattern fi-cCat1 btnSearch tbSearchSub 
         tbUniqeFiles tbAllText brwFiles btnExcel brwUniqueText brwText 
      WITH FRAME DEFAULT-FRAME IN WINDOW CompareCat.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW CompareCat.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findAppBuilder CompareCat 
PROCEDURE findAppBuilder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:   14-SEP-2007 sla: more reliable proof to catch the AppBuilder Window
------------------------------------------------------------------------------*/

/* 05-JAN-2007 sla: We will probably need this guy soon to add a penguin button on it */
DEFINE VARIABLE hCheckFirstMenuItem    AS HANDLE     NO-UNDO.
ghMainAppBuilderWindow = SESSION:FIRST-CHILD.

DO WHILE ghMainAppBuilderWindow <> ?:
    /* 14-SEP-2007 sla: as asked by Jan Keirse, some people change the title of the AppBuilder, so let's be more flexible' */
    /* IF ghMainAppBuilderWindow:TITLE = "AppBuilder" THEN DO: */
    IF ghMainAppBuilderWindow:TITLE MATCHES "*AppBuilder" THEN DO:
        
        /* 14-SEP-2007 sla: safer test afterward */
        /* hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR. */
        /*IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:FIRST-CHILD NO-ERROR. */
        /*IF   VALID-HANDLE(hCheckFirstMenuItem)                                                                    */
        /* AND hCheckFirstMenuItem:LABEL = "&File"                                                                  */
        /* THEN LEAVE.                                                                                              */
    
        hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR.
        IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.
        
        /* 06-DEC-2007 bot: Make sure we have the "&Help" menu, since this is not always the last one with modified appbuilders */
        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL <> "&Help":U THEN DO WHILE hCheckFirstMenuItem <> ?:
           ASSIGN hCheckFirstMenuItem = hCheckFirstMenuItem:PREV-SIBLING NO-ERROR. /*Walk back from the last menuitem*/
           IF   VALID-HANDLE(hCheckFirstMenuItem)
            AND hCheckFirstMenuItem:LABEL = "&Help":U
              THEN LEAVE. /* We found the correct menuitem*/
         END.
        /* 06-DEC-2007 bot: now return to the "About AppBuilder" check*/
        
        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL = "&Help" THEN DO:
            hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.
            
            IF VALID-HANDLE(hCheckFirstMenuItem)
             AND hCheckFirstMenuItem:TYPE = "MENU-ITEM"
             AND hCheckFirstMenuItem:LABEL = "About AppBuilder"
             THEN LEAVE.  /* OK , found a reliable way */
         END.
    
    END.
    ghMainAppBuilderWindow = ghMainAppBuilderWindow:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow CompareCat 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSessfile  AS CHAR   NO-UNDO.
DEF VAR cLine      AS CHAR   NO-UNDO.
DEF VAR cSessId    AS CHAR   NO-UNDO.
RUN JBoxLoadLib.p ("JBoxUIlib.p,JBoxAsLib.p,ResizeLib.p"
                   + (IF PROVERSION BEGINS "1" THEN ",JBoxFUlib.p" ELSE "")).
/* RUN JBoxLoadLib.p ("JBoxUIlib.p,ResizeLib.p"). */


cSessfile = SEARCH("incl/custdevmode.i").
IF cSessfile NE ? THEN DO:
  INPUT FROM VALUE(cSessfile).
  REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine MATCHES '*setSessionId*' THEN
      cSessId = REPLACE(REPLACE(REPLACE(ENTRY(NUM-ENTRIES(cLine),cLine),'"',""),".",""),")","").
  END.
END.
IF cSessId = "" OR cSessId = "ocSessionId" THEN cSessId = "validsession".

DYNAMIC-FUNCTION("setSessionId",cSessId).

RUN findAppBuilder.

DO WITH FRAME {&FRAME-NAME}:
  ttFiles.cFileName:READ-ONLY IN BROWSE brwFiles = TRUE.
  ttLine.cFileName:READ-ONLY IN BROWSE brwText = TRUE.
  ttUniqueLine.cFileName:READ-ONLY IN BROWSE brwUniqueText = TRUE.

  IF VALID-HANDLE(ghMainAppBuilderWindow) AND SEARCH("protools/abhack/openFileInAB.p") NE ? THEN DO:
    DYNAMIC-FUNCTION("NewMenuBand",BROWSE brwFiles:HANDLE,
                     "OpenFile;Open file in AppBuilder using ABhack method (doesn't need ABhack to run)"
                   + ",AppCompile;Open in JBox AppCompiler"
                     ,"").
    DYNAMIC-FUNCTION("NewMenuBand",BROWSE brwText:HANDLE,
                     "OpenFileText;Open file in AppBuilder using ABhack method (doesn't need ABhack to run)"
                     ,"").
    DYNAMIC-FUNCTION("NewMenuBand",BROWSE brwUniqueText:HANDLE,
                     "OpenFileUniqueText;Open file in AppBuilder using ABhack method (doesn't need ABhack to run)"
                     ,"").
  END.
  BROWSE brwUniqueText:HIDDEN = YES.

  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwFiles").
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(brwFiles:HANDLE) + "," +
                    STRING(brwText:HANDLE) + "," +
                    STRING(brwUniqueText:HANDLE)
                   ).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,380,300,0,0).
  APPLY "window-resized" TO {&WINDOW-NAME}.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFiles CompareCat 
PROCEDURE LoadFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icCat AS CHAR NO-UNDO.

DEF VAR cTmpFileName     AS CHAR NO-UNDO.
DEF VAR cTmpFileFullName AS CHAR NO-UNDO.
DEF VAR iLineIx          AS INT  NO-UNDO.
DEF VAR cImpLine         AS CHAR NO-UNDO.
DEF VAR cPattern         AS CHAR NO-UNDO.
DEF VAR cFilePattern     AS CHAR NO-UNDO.
DEF VAR cTmpBlock        AS CHAR NO-UNDO.
DEF VAR cDirList         AS CHAR NO-UNDO.
DEF VAR ix               AS INT  NO-UNDO.
DEF VAR bTextMatch       AS LOG  NO-UNDO.

FILE-INFO:FILE-NAME = icCat.
IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN fi-cPattern fi-cFilePattern
         cPattern     = TRIM(REPLACE(fi-cPattern," ",","),",")
         cFilePattern = REPLACE(fi-cFilePattern," ",",")
         .

  INPUT FROM OS-DIR(icCat).
  REPEAT:
    IMPORT cTmpFileName.
    FILE-INFO:FILE-NAME = RIGHT-TRIM(icCat,"\") + "\" + cTmpFileName.
    cTmpFileFullName = FILE-INFO:FULL-PATHNAME.
    IF cTmpFileName BEGINS "." THEN NEXT.

    IF FILE-INFO:FILE-TYPE = "drw" THEN DO:
      IF tbSearchSub:CHECKED THEN
        cDirList = cDirList + (IF cDirList NE "" THEN "," ELSE "") + cTmpFileFullName.
      NEXT.
    END.

    bOk = FALSE.
    DO ix = 1 TO NUM-ENTRIES(cFilePattern):
      IF cTmpFileName MATCHES ENTRY(ix,cFilePattern) THEN
        bOK = TRUE.
    END.
    IF NOT bOk THEN NEXT.

    CREATE ttFiles.
    ASSIGN ttFiles.cFileName = LEFT-TRIM(RIGHT-TRIM(SUBSTR(icCat,LENGTH(fi-cCat1:SCREEN-VALUE) + 1),"\") + "\" + cTmpFileName,"\")
           ttFiles.cFullName = cTmpFileFullName
           ttFiles.cType     = FILE-INFO:FILE-TYPE
           ttFiles.cExt      = SUBSTR(cTmpFileName,R-INDEX(cTmpFileName,".") + 1)
           ttFiles.fSize     = FILE-INFO:FILE-SIZE
           ttFiles.dModified = FILE-INFO:FILE-MOD-DATE
           ttFiles.bShow     = TRUE
           .
  END.
  INPUT CLOSE.

  FOR EACH ttFiles WHERE ttFiles.cType NE "DRW":
    INPUT FROM VALUE(ttFiles.cFullName).
    ASSIGN iLineIx = 0
           bTextMatch = NO.
    REPEAT:
      IMPORT UNFORMATTED cImpLine.
      iLineIx = iLineIx + 1.
      DO ix = 1 TO NUM-ENTRIES(cPattern):
        IF cImpLine MATCHES "*" + ENTRY(ix,cPattern) + "*" THEN DO:
          bTextMatch = YES.
          FIND FIRST ttLine
               WHERE ttLine.cFileName = ttFiles.cFileName
                 AND ttLine.iLineNo   = iLineIx
               NO-ERROR.
          IF NOT AVAIL ttLine THEN DO:
            CREATE ttLine.
            ASSIGN ttLine.cFileName = ttFiles.cFileName
                   ttLine.iLineNo   = iLineIx
                   ttLine.cLineText = TRIM(cImpLine)
                   ttLine.cBlock    = cTmpBlock
                   ttLine.cFullName = ttFiles.cFullName
                   .
          END.
          FIND FIRST ttUniqueLine
               WHERE ttUniqueLine.cFileName = ttFiles.cFileName
               NO-ERROR.
          IF NOT AVAIL ttUniqueLine THEN DO:
            CREATE ttUniqueLine.
            ASSIGN ttUniqueLine.cFileName = ttFiles.cFileName
                   ttUniqueLine.iLineNo   = iLineIx
                   ttUniqueLine.cLineText = TRIM(cImpLine)
                   ttUniqueLine.cBlock    = cTmpBlock
                   ttUniqueLine.cFullName = ttFiles.cFullName
                   .
          END.
        END.
      END.
      IF cImpLine MATCHES "&Scoped-define SELF-NAME*" OR cImpLine MATCHES "procedure*" OR cImpLine MATCHES "*function*returns*" THEN
        cTmpBlock = cImpLine.
    END.
    INPUT CLOSE.
    IF cPattern NE "" AND NOT bTextMatch THEN DELETE ttFiles.
  END.

  RUN OpenQueryFiles.
  RUN OpenQueryText.
  RUN OpenQueryUniqueText.
END.

/* MESSAGE "icCat: " icCat SKIP                                        */
/*         "fi-cCat1: " fi-cCat1:SCREEN-VALUE SKIP                     */
/*         "trim: " LEFT-TRIM(icCat,fi-cCat1:SCREEN-VALUE) SKIP        */
/*         "substr: " SUBSTR(icCat,LENGTH(fi-cCat1:SCREEN-VALUE) + 1)  */
/*   VIEW-AS ALERT-BOX INFO BUTTONS OK.                                */
DO ix = 1 TO NUM-ENTRIES(cDirList):
  RUN LoadFiles(ENTRY(ix,cDirList)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenFileRecord CompareCat 
PROCEDURE OpenFileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hOpenFile AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DO ix = 1 TO BROWSE brwFiles:NUM-SELECTED-ROWS:
    IF BROWSE brwFiles:FETCH-SELECTED-ROW(ix) THEN DO:
      RUN protools/abhack/openFileInAB.p 
           PERSISTENT SET hOpenFile
          (BROWSE brwFiles:HANDLE:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFullName"):BUFFER-VALUE, ghMainAppBuilderWindow).
    
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenFileTextRecord CompareCat 
PROCEDURE OpenFileTextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hOpenFile AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  RUN protools/abhack/openFileInAB.p 
       PERSISTENT SET hOpenFile
      (BROWSE brwText:HANDLE:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFullName"):BUFFER-VALUE, ghMainAppBuilderWindow).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenFileUniqueTextRecord CompareCat 
PROCEDURE OpenFileUniqueTextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hOpenFile AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DO ix = 1 TO BROWSE brwUniqueText:NUM-SELECTED-ROWS:
    IF BROWSE brwUniqueText:FETCH-SELECTED-ROW(ix) THEN DO:
      RUN protools/abhack/openFileInAB.p 
           PERSISTENT SET hOpenFile
          (BROWSE brwUniqueText:HANDLE:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFullName"):BUFFER-VALUE, ghMainAppBuilderWindow).
    
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryFiles CompareCat 
PROCEDURE OpenQueryFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cQueryString = "FOR EACH ttFiles WHERE ttFiles.bShow AND ttFiles.cType NE 'DRW'" +
                 (IF cSortColumn1 NE "" THEN  
                    " BY " + cSortColumn1 + (IF bDesc1 THEN " DESC." ELSE ".")
                  ELSE ".") +
                 (IF cSortColumn1 NE "" THEN 
                   " BY " + cSortColumn1 + (IF bDesc1 THEN " DESC" ELSE "")
                  ELSE "")
                 .
  brwFiles:QUERY:QUERY-PREPARE(cQueryString).
  brwFiles:QUERY:QUERY-OPEN().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryText CompareCat 
PROCEDURE OpenQueryText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cQueryString = "FOR EACH ttLine WHERE " +
                 (IF tbAllText THEN 
                   "true"
                  ELSE "ttLine.cFileName = '" + ttFiles.cFileName + "'") +
                 (IF cSortColumn2 NE "" THEN 
                   " BY " + cSortColumn2 + (IF bDesc2 THEN " DESC" ELSE "")
                  ELSE "")
                 .
  
  brwText:QUERY:QUERY-PREPARE(cQueryString).
  brwText:QUERY:QUERY-OPEN().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryUniqueText CompareCat 
PROCEDURE OpenQueryUniqueText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cQueryString = "FOR EACH ttUniqueLine WHERE " +
                 (IF tbAllText THEN 
                   "true"
                  ELSE "ttUniqueLine.cFileName = '" + ttFiles.cFileName + "'") +
                 (IF cSortColumn2 NE "" THEN 
                   " BY " + cSortColumn2 + (IF bDesc2 THEN " DESC" ELSE "")
                  ELSE "")
                 .
  
  brwUniqueText:QUERY:QUERY-PREPARE(cQueryString).
  brwUniqueText:QUERY:QUERY-OPEN().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch CompareCat 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE ttFiles.
EMPTY TEMP-TABLE ttLine.
EMPTY TEMP-TABLE ttUniqueLine.
SESSION:SET-WAIT-STATE("general").
RUN LoadFiles (fi-cCat1:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkForResizeLib CompareCat 
FUNCTION checkForResizeLib RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF ihProc:FILE-NAME = "resizelib.p" THEN DO:
    bResizeLibLoaded = TRUE.
    RETURN TRUE.
  END.
  ELSE IF ihProc:NEXT-SIBLING NE ? THEN
    checkForResizeLib (ihProc:NEXT-SIBLING).
   
  RETURN FALSE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

