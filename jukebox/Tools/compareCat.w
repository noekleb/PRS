&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          wr               PROGRESS
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

DEF VAR cSortColumn1  AS CHAR NO-UNDO.
DEF VAR bDesc1        AS LOG NO-UNDO.

DEF VAR cSortColumn2  AS CHAR NO-UNDO.
DEF VAR bDesc2        AS LOG NO-UNDO.

DEF TEMP-TABLE ttCat1
    FIELD cFileName   AS CHAR 
    FIELD cType       AS CHAR
    FIELD fSize       AS DEC
    FIELD dModified   AS DATE
    FIELD bShow       AS LOG
    INDEX cFileName   cFileName
    .

DEF TEMP-TABLE ttCat2
    FIELD cFileName   AS CHAR 
    FIELD cType       AS CHAR
    FIELD fSize       AS DEC
    FIELD dModified   AS DATE
    FIELD bShow       AS LOG
    INDEX cFileName   cFileName
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwCat1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCat1 ttCat2

/* Definitions for BROWSE brwCat1                                       */
&Scoped-define FIELDS-IN-QUERY-brwCat1 ttCat1.cFileName ttCat1.cType ttCat1.dModified ttCat1.fSize   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCat1 ttCat1.cFileName   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwCat1 ttCat1
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwCat1 ttCat1
&Scoped-define SELF-NAME brwCat1
&Scoped-define QUERY-STRING-brwCat1 FOR EACH ttCat1
&Scoped-define OPEN-QUERY-brwCat1 OPEN QUERY {&SELF-NAME} FOR EACH ttCat1.
&Scoped-define TABLES-IN-QUERY-brwCat1 ttCat1
&Scoped-define FIRST-TABLE-IN-QUERY-brwCat1 ttCat1


/* Definitions for BROWSE brwCat2                                       */
&Scoped-define FIELDS-IN-QUERY-brwCat2 ttCat2.cFileName ttCat2.cType ttCat2.dModified ttCat2.fSize   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCat2 ttCat2.cFileName   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwCat2 ttCat2
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwCat2 ttCat2
&Scoped-define SELF-NAME brwCat2
&Scoped-define QUERY-STRING-brwCat2 FOR EACH ttCat2
&Scoped-define OPEN-QUERY-brwCat2 OPEN QUERY {&SELF-NAME} FOR EACH ttCat2.
&Scoped-define TABLES-IN-QUERY-brwCat2 ttCat2
&Scoped-define FIRST-TABLE-IN-QUERY-brwCat2 ttCat2


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwCat1}~
    ~{&OPEN-QUERY-brwCat2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cCat1 fi-cCat2 brwCat1 brwCat2 ~
fi-dModFrom tbOnlyDiff 
&Scoped-Define DISPLAYED-OBJECTS fi-cCat1 fi-cCat2 fi-dModFrom tbOnlyDiff 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR CompareCat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-cCat1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cat 1" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cCat2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cat 2" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dModFrom AS DATE FORMAT "99/99/9999":U 
     LABEL "Mod from" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE tbOnlyDiff AS LOGICAL INITIAL no 
     LABEL "Show Differences" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .86 NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U
     LABEL "Button 5" 
     SIZE 1 BY 14.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwCat1 FOR 
      ttCat1 SCROLLING.

DEFINE QUERY brwCat2 FOR 
      ttCat2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwCat1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCat1 CompareCat _FREEFORM
  QUERY brwCat1 DISPLAY
      ttCat1.cFileName  FORMAT "x(40)" WIDTH 25  COLUMN-LABEL "FileName"
ttCat1.cType      FORMAT "x(30)" WIDTH 4   COLUMN-LABEL "Type"
ttCat1.dModified  FORMAT "99/99/9999"      COLUMN-LABEL "Modified"
ttCat1.fSize      FORMAT "->>,>>>,>>9.99"  COLUMN-LABEL "Size"
ENABLE ttCat1.cFileName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38.2 BY 13.95 EXPANDABLE.

DEFINE BROWSE brwCat2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCat2 CompareCat _FREEFORM
  QUERY brwCat2 DISPLAY
      ttCat2.cFileName  FORMAT "x(40)" WIDTH 25  COLUMN-LABEL "FileName"
ttCat2.cType      FORMAT "x(30)" WIDTH 4   COLUMN-LABEL "Type"
ttCat2.dModified  FORMAT "99/99/9999"      COLUMN-LABEL "Modified"
ttCat2.fSize      FORMAT "->>,>>>,>>9.99"  COLUMN-LABEL "Size"
ENABLE ttCat2.cFileName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38.6 BY 13.95 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-cCat1 AT ROW 1.52 COL 6 COLON-ALIGNED
     fi-cCat2 AT ROW 1.52 COL 45 COLON-ALIGNED
     brwCat1 AT ROW 2.76 COL 1.8
     brwCat2 AT ROW 2.76 COL 41.6
     fi-dModFrom AT ROW 16.95 COL 33.6 COLON-ALIGNED
     tbOnlyDiff AT ROW 17.05 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 80.8 BY 17.29.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 2.69
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
         TITLE              = "Compare Catalogs"
         HEIGHT             = 17.29
         WIDTH              = 80.8
         MAX-HEIGHT         = 42.67
         MAX-WIDTH          = 182.8
         VIRTUAL-HEIGHT     = 42.67
         VIRTUAL-WIDTH      = 182.8
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frSplitBarX:MOVE-AFTER-TAB-ITEM (fi-cCat2:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME frSplitBarX:MOVE-BEFORE-TAB-ITEM (brwCat1:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB brwCat1 frSplitBarX DEFAULT-FRAME */
/* BROWSE-TAB brwCat2 brwCat1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 17.29
       FRAME DEFAULT-FRAME:WIDTH            = 80.8.

ASSIGN 
       brwCat1:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       brwCat2:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CompareCat)
THEN CompareCat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCat1
/* Query rebuild information for BROWSE brwCat1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCat1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwCat1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCat2
/* Query rebuild information for BROWSE brwCat2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCat2.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwCat2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CompareCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON END-ERROR OF CompareCat /* Compare Catalogs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON WINDOW-CLOSE OF CompareCat /* Compare Catalogs */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON WINDOW-RESIZED OF CompareCat /* Compare Catalogs */
DO: 
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwCat1
&Scoped-define SELF-NAME brwCat1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwCat1 CompareCat
ON START-SEARCH OF brwCat1 IN FRAME DEFAULT-FRAME
DO:
  IF cSortColumn1 = brwCat1:CURRENT-COLUMN:NAME THEN
    bDesc1 = NOT bDesc1.
  ELSE cSortColumn1 = brwCat1:CURRENT-COLUMN:NAME.

  RUN OpenQuery1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwCat2
&Scoped-define SELF-NAME brwCat2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwCat2 CompareCat
ON START-SEARCH OF brwCat2 IN FRAME DEFAULT-FRAME
DO: 
  IF cSortColumn2 = brwCat2:CURRENT-COLUMN:NAME THEN
    bDesc2 = NOT bDesc2.
  ELSE cSortColumn2 = brwCat2:CURRENT-COLUMN:NAME.

  RUN OpenQuery2.
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
ON RETURN OF fi-cCat1 IN FRAME DEFAULT-FRAME /* Cat 1 */
DO:
  RUN LoadFiles1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cCat2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cCat2 CompareCat
ON RETURN OF fi-cCat2 IN FRAME DEFAULT-FRAME /* Cat 2 */
DO:
  RUN LoadFiles2.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dModFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dModFrom CompareCat
ON RETURN OF fi-dModFrom IN FRAME DEFAULT-FRAME /* Mod from */
DO:
  ASSIGN fi-dModFrom.
  RUN OpenQuery1.
  RUN OpenQuery2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbOnlyDiff
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbOnlyDiff CompareCat
ON VALUE-CHANGED OF tbOnlyDiff IN FRAME DEFAULT-FRAME /* Show Differences */
DO:
  ASSIGN tbOnlyDiff.
  IF tbOnlyDiff THEN
    RUN FindDiff.
  ELSE DO:
    FOR EACH ttCat1: ttCat1.bShow = TRUE. END.
    FOR EACH ttCat2: ttCat2.bShow = TRUE. END.
  END.
  cSortColumn1 = "".
  cSortColumn2 = "".
  RUN OpenQuery1.
  RUN OpenQuery2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwCat1
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
  DISPLAY fi-cCat1 fi-cCat2 fi-dModFrom tbOnlyDiff 
      WITH FRAME DEFAULT-FRAME IN WINDOW CompareCat.
  ENABLE fi-cCat1 fi-cCat2 brwCat1 brwCat2 fi-dModFrom tbOnlyDiff 
      WITH FRAME DEFAULT-FRAME IN WINDOW CompareCat.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW CompareCat.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
  VIEW CompareCat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindDiff CompareCat 
PROCEDURE FindDiff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttCat1:
  FIND FIRST ttCat2
       WHERE ttCat2.cFileName = ttCat1.cFileName
       NO-ERROR.
  IF AVAIL ttCat2
     AND ttCat2.cType     = ttCat1.cType
     AND ttCat2.dModified = ttCat1.dModified
     AND ttCat2.fSize     = ttCat1.fSize
     THEN
    ASSIGN ttCat1.bShow = FALSE
           ttCat2.bShow = FALSE.
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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR hResizeLib AS HANDLE NO-UNDO.
  RUN resizeLib.p PERSIST SET hResizeLib.
  SESSION:ADD-SUPER-PROC(hResizeLib).
&ENDIF

DO WITH FRAME {&FRAME-NAME}:
  ttCat1.cFileName:READ-ONLY IN BROWSE brwCat1 = TRUE.
  ttCat2.cFileName:READ-ONLY IN BROWSE brwCat2 = TRUE.

  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwCat1").
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(brwCat1:HANDLE) + "," + STRING(BrwCat2:HANDLE)
                    ).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,300,300,0,0).
  APPLY "window-resized" TO CURRENT-WINDOW.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFiles1 CompareCat 
PROCEDURE LoadFiles1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTmpFileName AS CHAR NO-UNDO.
DEF VAR cTmpFileFullName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  EMPTY TEMP-TABLE ttCat1.
  ASSIGN fi-cCat1 fi-cCat2.

  INPUT FROM OS-DIR(fi-cCat1).
  REPEAT:
    IMPORT cTmpFileName.
    IF cTmpFileName BEGINS "." THEN NEXT.
    cTmpFileFullName = TRIM(fi-cCat1, "\") + "\" + cTmpFileName.
    FILE-INFO:FILE-NAME = cTmpFileFullName.
    CREATE ttCat1.
    ASSIGN ttCat1.cFileName = cTmpFileName
           ttCat1.cType     = FILE-INFO:FILE-TYPE
           ttCat1.fSize     = FILE-INFO:FILE-SIZE
           ttCat1.dModified = FILE-INFO:FILE-MOD-DATE
           ttCat1.bShow     = TRUE
           .
  END.
  INPUT CLOSE.

  RUN OpenQuery1.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFiles2 CompareCat 
PROCEDURE LoadFiles2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTmpFileName AS CHAR NO-UNDO.
DEF VAR cTmpFileFullName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    
  EMPTY TEMP-TABLE ttCat2.
  ASSIGN fi-cCat2.

  INPUT FROM OS-DIR(fi-cCat2).
  REPEAT:
    IMPORT cTmpFileName.
    IF cTmpFileName BEGINS "." THEN NEXT.
    cTmpFileFullName = TRIM(fi-cCat2, "\") + "\" + cTmpFileName.
    FILE-INFO:FILE-NAME = cTmpFileFullName.
    CREATE ttCat2.
    ASSIGN ttCat2.cFileName = cTmpFileName
           ttCat2.cType     = FILE-INFO:FILE-TYPE
           ttCat2.fSize     = FILE-INFO:FILE-SIZE
           ttCat2.dModified = FILE-INFO:FILE-MOD-DATE
           ttCat2.bShow     = TRUE
           .
  END.
  INPUT CLOSE.

  RUN OpenQuery2.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery1 CompareCat 
PROCEDURE OpenQuery1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cQueryString = "FOR EACH ttCat1 WHERE ttCat1.bShow " +
                 (IF fi-dModFrom NE ? THEN 
                    " AND ttCat1.dModified GE " + STRING(fi-dModFrom) 
                  ELSE "") +
                 (IF cSortColumn1 NE "" THEN 
                    " BY " + cSortColumn1 + (IF bDesc1 THEN " DESC." ELSE ".")
                  ELSE ".").
  brwCat1:QUERY:QUERY-PREPARE(cQueryString).
  brwCat1:QUERY:QUERY-OPEN().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery2 CompareCat 
PROCEDURE OpenQuery2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cQueryString = "FOR EACH ttCat2 WHERE ttCat2.bShow " +
                 (IF fi-dModFrom NE ? THEN 
                    " AND ttCat2.dModified GE " + STRING(fi-dModFrom) 
                  ELSE "") +
                 (IF cSortColumn2 NE "" THEN 
                    " BY " + cSortColumn2 + (IF bDesc2 THEN " DESC." ELSE ".")
                  ELSE ".").
  
  brwCat2:QUERY:QUERY-PREPARE(cQueryString).
  brwCat2:QUERY:QUERY-OPEN().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

