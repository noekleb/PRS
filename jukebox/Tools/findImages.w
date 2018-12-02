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

DEF VAR cSortColumn1  AS CHAR NO-UNDO.
DEF VAR bDesc1        AS LOG NO-UNDO.

DEF VAR cSortColumn2  AS CHAR NO-UNDO.
DEF VAR bDesc2        AS LOG NO-UNDO.

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
    INDEX idxLine     cFileName iLineNo 
    INDEX cFileName   cFileName
    .

DEF TEMP-TABLE ttImages
    FIELD cFileName   AS CHAR
    FIELD cType       AS CHAR
    FIELD cImgName    AS CHAR
    FIELD cFullName   AS CHAR 
    INDEX cFileName   cFileName
    .

DEF BUFFER bttImages FOR ttImages.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwFiles

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttFiles ttImages

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


/* Definitions for BROWSE brwImages                                     */
&Scoped-define FIELDS-IN-QUERY-brwImages ttImages.cFileName ttImages.cType ttImages.cImgName ttImages.cFullName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwImages ttImages.cFileName ~
ttImages.cImgName ~
ttImages.cFullName   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwImages ttImages
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwImages ttImages
&Scoped-define SELF-NAME brwImages
&Scoped-define QUERY-STRING-brwImages FOR EACH ttImages
&Scoped-define OPEN-QUERY-brwImages OPEN QUERY {&SELF-NAME} FOR EACH ttImages.
&Scoped-define TABLES-IN-QUERY-brwImages ttImages
&Scoped-define FIRST-TABLE-IN-QUERY-brwImages ttImages


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwFiles}~
    ~{&OPEN-QUERY-brwImages}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cPattern fi-cFilePattern fi-cCat1 ~
btnSource tbAllImg brwFiles brwImages btnDeployDir btnDeploy fi-cDeployTo ~
tbUseRelativePath 
&Scoped-Define DISPLAYED-OBJECTS fi-cPattern fi-cFilePattern fi-cCat1 ~
tbAllImg fi-cDeployTo tbUseRelativePath 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR CompareCat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDeploy 
     LABEL "Deploy" 
     SIZE 8 BY 1.14.

DEFINE BUTTON btnDeployDir 
     LABEL "..." 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnSource 
     LABEL "..." 
     SIZE 5 BY 1.14.

DEFINE VARIABLE fi-cCat1 AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\progress~\jukebox~\lib" 
     LABEL "Catalog" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cDeployTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Deploy to catalog" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cFilePattern AS CHARACTER FORMAT "X(256)":U INITIAL "*.i *.p *.w" 
     LABEL "File pattern" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cPattern AS CHARACTER FORMAT "X(256)":U INITIAL ".bmp .gif .ico .jpg" 
     LABEL "Search pattern" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE tbAllImg AS LOGICAL INITIAL no 
     LABEL "View all images" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.6 BY .81 NO-UNDO.

DEFINE VARIABLE tbUseRelativePath AS LOGICAL INITIAL yes 
     LABEL "Use relative path" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.2 BY .81 NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 5" 
     SIZE 1 BY 14.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwFiles FOR 
      ttFiles SCROLLING.

DEFINE QUERY brwImages FOR 
      ttImages SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwFiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwFiles CompareCat _FREEFORM
  QUERY brwFiles DISPLAY
      ttFiles.cFileName  FORMAT "x(40)" WIDTH 25  COLUMN-LABEL "FileName"
ttFiles.cExt       FORMAT "x(30)"  WIDTH 7   COLUMN-LABEL "Ext"
/* ttFiles.cType      FORMAT "x(30)" WIDTH 4   COLUMN-LABEL "Type" */
ttFiles.dModified  FORMAT "99/99/9999"      COLUMN-LABEL "Modified"
ttFiles.fSize      FORMAT "->>,>>>,>>9.99"  COLUMN-LABEL "Size"
ENABLE ttFiles.cFileName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49.2 BY 14.14 FIT-LAST-COLUMN.

DEFINE BROWSE brwImages
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwImages CompareCat _FREEFORM
  QUERY brwImages DISPLAY
      ttImages.cFileName  FORMAT "x(40)" WIDTH 25  COLUMN-LABEL "FileName"
ttImages.cType      FORMAT "x(30)" WIDTH 4   COLUMN-LABEL "Type"
ttImages.cImgName   FORMAT "x(30)"      COLUMN-LABEL "Image"
ttImages.cFullName  FORMAT "x(80)"      COLUMN-LABEL "Full name"
ENABLE ttImages.cFileName
       ttImages.cImgName
       ttImages.cFullName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 48 BY 14.14 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-cPattern AT ROW 1.24 COL 17 COLON-ALIGNED
     fi-cFilePattern AT ROW 2.48 COL 17 COLON-ALIGNED
     fi-cCat1 AT ROW 3.71 COL 17 COLON-ALIGNED
     btnSource AT ROW 3.71 COL 57
     tbAllImg AT ROW 3.95 COL 79.8
     brwFiles AT ROW 5 COL 1.6
     brwImages AT ROW 5 COL 53
     btnDeployDir AT ROW 19.52 COL 64.4
     btnDeploy AT ROW 19.52 COL 70
     fi-cDeployTo AT ROW 19.57 COL 17 COLON-ALIGNED
     tbUseRelativePath AT ROW 19.71 COL 78.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 100.2 BY 19.71.

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
         TITLE              = "Extract images"
         HEIGHT             = 19.71
         WIDTH              = 100.2
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
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME frSplitBarX:MOVE-AFTER-TAB-ITEM (brwFiles:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME frSplitBarX:MOVE-BEFORE-TAB-ITEM (brwImages:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB brwFiles tbAllImg DEFAULT-FRAME */
/* BROWSE-TAB brwImages frSplitBarX DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.71
       FRAME DEFAULT-FRAME:WIDTH            = 100.2.

ASSIGN 
       brwFiles:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       brwImages:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwImages
/* Query rebuild information for BROWSE brwImages
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttImages.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwImages */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CompareCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON END-ERROR OF CompareCat /* Extract images */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON WINDOW-CLOSE OF CompareCat /* Extract images */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompareCat CompareCat
ON WINDOW-RESIZED OF CompareCat /* Extract images */
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
  IF NOT tbAllImg THEN
    RUN OpenQueryImages.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwImages
&Scoped-define SELF-NAME brwImages
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwImages CompareCat
ON START-SEARCH OF brwImages IN FRAME DEFAULT-FRAME
DO: 
  IF cSortColumn2 = brwImages:CURRENT-COLUMN:NAME THEN
    bDesc2 = NOT bDesc2.
  cSortColumn2 = brwImages:CURRENT-COLUMN:NAME.

  RUN OpenQueryImages.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeploy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeploy CompareCat
ON CHOOSE OF btnDeploy IN FRAME DEFAULT-FRAME /* Deploy */
DO:
  ASSIGN fi-cDeployTo.
  FILE-INFO:FILE-NAME = fi-cDeployTo.
  IF fi-cDeployTo = "" OR FILE-INFO:FULL-PATHNAME = ? THEN DO:
    MESSAGE "Invalid target catalog"
            VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.
  ELSE RUN DeployFiles.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeployDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeployDir CompareCat
ON CHOOSE OF btnDeployDir IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cDir AS CHAR NO-UNDO.

  SYSTEM-DIALOG GET-DIR cDir.

  IF cDir NE "" THEN fi-cDeployTo:SCREEN-VALUE = cDir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSource CompareCat
ON CHOOSE OF btnSource IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cDir AS CHAR NO-UNDO.

  SYSTEM-DIALOG GET-DIR cDir.

  IF cDir NE "" THEN DO:
    fi-cCat1:SCREEN-VALUE = cDir.
    APPLY "return" TO fi-cCat1.
  END.
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
  SESSION:SET-WAIT-STATE("general").
  RUN LoadFiles.
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAllImg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAllImg CompareCat
ON VALUE-CHANGED OF tbAllImg IN FRAME DEFAULT-FRAME /* View all images */
DO:
  ASSIGN tbAllImg.
  RUN OpenQueryImages.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeployFiles CompareCat 
PROCEDURE DeployFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFile AS CHAR NO-UNDO.
DEF VAR iy    AS INT  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN tbUseRelativePath.
  IF brwImages:NUM-SELECTED-ROWS > 0 THEN
    DO ix = 1 TO brwImages:NUM-SELECTED-ROWS:
      IF brwImages:FETCH-SELECTED-ROW(ix) THEN DO:
        IF NOT tbUseRelativePath THEN DO:
          IF R-INDEX(ttImages.cImgName,"\") > 0 THEN
            cFile = SUBSTR(ttImages.cImgName,R-INDEX(ttImages.cImgName,"\") + 1).
          ELSE IF R-INDEX(ttImages.cImgName,"/") > 0 THEN
            cFile = SUBSTR(ttImages.cImgName,R-INDEX(ttImages.cImgName,"/") + 1).
          ELSE cFile = ttImages.cImgName.
        END.
        cFile = REPLACE(ttImages.cImgName,"/","\").
        DO iy = 1 TO NUM-ENTRIES(cFile,"\") - 1:
          FILE-INFO:FILE-NAME = TRIM(fi-cDeployTo,"\") + "\" + ENTRY(iy,cFile,"\").
          IF FILE-INFO:FULL-PATHNAME = ? THEN
            OS-COMMAND SILENT mkdir VALUE(TRIM(fi-cDeployTo,"\") + "\" + ENTRY(iy,cFile,"\")).
        END.
        OS-COPY VALUE(ttImages.cFullName) VALUE(TRIM(fi-cDeployTo,"\") + "\" + cFile).
      END.
    END.
  ELSE DO:
    brwImages:QUERY:GET-FIRST().
    REPEAT WHILE NOT brwImages:QUERY:QUERY-OFF-END:
      IF NOT tbUseRelativePath THEN DO:
        IF R-INDEX(ttImages.cImgName,"\") > 0 THEN
          cFile = SUBSTR(ttImages.cImgName,R-INDEX(ttImages.cImgName,"\") + 1).
        ELSE IF R-INDEX(ttImages.cImgName,"/") > 0 THEN
          cFile = SUBSTR(ttImages.cImgName,R-INDEX(ttImages.cImgName,"/") + 1).
        ELSE cFile = ttImages.cImgName.
      END.
      ELSE DO:
        cFile = REPLACE(ttImages.cImgName,"/","\").
        DO iy = 1 TO NUM-ENTRIES(cFile,"\") - 1:
          FILE-INFO:FILE-NAME = TRIM(fi-cDeployTo,"\") + "\" + ENTRY(iy,cFile,"\").
          IF FILE-INFO:FULL-PATHNAME = ? THEN
            OS-COMMAND SILENT mkdir VALUE(TRIM(fi-cDeployTo,"\") + "\" + ENTRY(iy,cFile,"\")).
        END.
      END. 
      OS-COPY VALUE(ttImages.cFullName) VALUE(TRIM(fi-cDeployTo,"\") + "\" + cFile).
      brwImages:QUERY:GET-NEXT().
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
  DISPLAY fi-cPattern fi-cFilePattern fi-cCat1 tbAllImg fi-cDeployTo 
          tbUseRelativePath 
      WITH FRAME DEFAULT-FRAME IN WINDOW CompareCat.
  ENABLE fi-cPattern fi-cFilePattern fi-cCat1 btnSource tbAllImg brwFiles 
         brwImages btnDeployDir btnDeploy fi-cDeployTo tbUseRelativePath 
      WITH FRAME DEFAULT-FRAME IN WINDOW CompareCat.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW CompareCat.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
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
  ttFiles.cFileName:READ-ONLY IN BROWSE brwFiles = TRUE.
  ttImages.cFileName:READ-ONLY IN BROWSE brwImages = TRUE.

  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwFiles").
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(brwFiles:HANDLE) + "," +
                    STRING(brwImages:HANDLE)
                   ).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,340,300,0,0).

  BROWSE brwFiles:MOVE-TO-TOP().
  BROWSE brwImages:MOVE-TO-TOP().
END.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.

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
DEF VAR cTmpFileName     AS CHAR NO-UNDO.
DEF VAR cTmpFileFullName AS CHAR NO-UNDO.
DEF VAR iLineIx          AS INT NO-UNDO.
DEF VAR cImpLine         AS CHAR NO-UNDO.
DEF VAR iPrevBlank       AS INT NO-UNDO.
DEF VAR iPrevSC          AS INT NO-UNDO.
DEF VAR cPattern         AS CHAR NO-UNDO.
DEF VAR cFilePattern     AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  EMPTY TEMP-TABLE ttFiles.
  EMPTY TEMP-TABLE ttImages.
  EMPTY TEMP-TABLE ttLine.

  ASSIGN fi-cCat1 fi-cPattern fi-cFilePattern
         cPattern     = REPLACE(fi-cPattern," ",",")
         cFilePattern = REPLACE(fi-cFilePattern," ",",")
         .

  INPUT FROM OS-DIR(fi-cCat1).
  REPEAT:
    IMPORT cTmpFileName.
    IF cTmpFileName BEGINS "." THEN NEXT.
    bOk = FALSE.
    DO ix = 1 TO NUM-ENTRIES(cFilePattern):
      IF cTmpFileName MATCHES ENTRY(ix,cFilePattern) THEN
        bOK = TRUE.
    END.
    IF NOT bOk THEN NEXT.

    cTmpFileFullName = TRIM(fi-cCat1, "\") + "\" + cTmpFileName.
    FILE-INFO:FILE-NAME = cTmpFileFullName.
    CREATE ttFiles.
    ASSIGN ttFiles.cFileName = cTmpFileName
           ttFiles.cFullName = FILE-INFO:FULL-PATHNAME
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
    iLineIx = 0.
    REPEAT:
      IMPORT UNFORMATTED cImpLine.
      iLineIx = iLineIx + 1.
      DO ix = 1 TO NUM-ENTRIES(fi-cPattern," "):
        IF cImpLine MATCHES "*" + ENTRY(ix,cPattern) + "*" THEN DO:
          FIND FIRST ttLine
               WHERE ttLine.cFileName = ttFiles.cFileName
                 AND ttLine.iLineNo   = iLineIx
               NO-ERROR.
          IF NOT AVAIL ttLine THEN DO:
            CREATE ttLine.
            ASSIGN ttLine.cFileName = ttFiles.cFileName
                   ttLine.iLineNo   = iLineIx
                   ttLine.cLineText = cImpLine.
          END.
        END.
      END.
    END.
    INPUT CLOSE.
  END.

  FOR EACH ttLine:
    DO iLineIx = 1 TO LENGTH(cLineText):
      DO ix = 1 TO NUM-ENTRIES(cPattern):
        IF SUBSTR(cLineText,iLineIx,LENGTH(ENTRY(ix,cPattern))) = ENTRY(ix,cPattern) THEN DO:
          CREATE ttImages.
          ASSIGN ttImages.cFileName = ttLine.cFileName
                 ttImages.cType     = SUBSTR(ENTRY(ix,cPattern),2)
                 ttImages.cImgName  = TRIM(TRIM(TRIM(SUBSTR(cLine,iPrevBlank + 1,iLineIx + LENGTH(ENTRY(ix,cPattern)) - iPrevBlank),","),'"'),",")
                 iPrevBlank         = iLineIx + LENGTH(ENTRY(ix,cPattern))
                 ttImages.cFullName = SEARCH(ttImages.cImgName)
                 NO-ERROR.
          IF ttImages.cFullName = ? THEN DO:
            ASSIGN ttImages.cImgName  = TRIM(TRIM(TRIM(SUBSTR(cLine,iPrevSC + 1,iLineIx + LENGTH(ENTRY(ix,cPattern)) - iPrevSC),","),'"'),",")
                   iPrevSC            = iLineIx + LENGTH(ENTRY(ix,cPattern))
                   ttImages.cFullName = SEARCH(ttImages.cImgName)
                   NO-ERROR.
            IF ttImages.cFullName = ? THEN
              DELETE ttImages.
          END.
          FIND bttImages 
               WHERE bttImages.cFileName = ttImages.cFileName
                 AND bttImages.cImgName  = ttImages.cImgName
                 AND ROWID(bttImages)   NE ROWID(ttImages)
               NO-ERROR.
          IF AVAIL bttImages THEN
            DELETE ttImages.
        END.
        ELSE IF SUBSTR(cLineText,iLineIx,1) = "" THEN
          iPrevBlank = iLineIx.
        ELSE IF SUBSTR(cLineText,iLineIx,1) = ";" THEN
          iPrevSC = iLineIx.
      END.
    END.
  END.

  RUN OpenQueryFiles.
  RUN OpenQueryImages.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryImages CompareCat 
PROCEDURE OpenQueryImages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cQueryString = "FOR EACH ttImages WHERE ttImages.cFullName NE ? AND " +
                 (IF tbAllImg THEN 
                   "true"
                  ELSE "ttImages.cFileName = '" + ttFiles.cFileName + "'") +
                 (IF cSortColumn2 NE "" THEN 
                   " BY " + cSortColumn2 + (IF bDesc2 THEN " DESC" ELSE "")
                  ELSE "")
                 .
  
  brwImages:QUERY:QUERY-PREPARE(cQueryString).
  brwImages:QUERY:QUERY-OPEN().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

