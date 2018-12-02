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

/* Local Variable Definitions ---                                       */

DEF VAR bOk                AS LOG    NO-UNDO.
DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR hBrwPackage        AS HANDLE NO-UNDO.
DEF VAR hBrwFiles          AS HANDLE NO-UNDO.
DEF VAR hFMPackage         AS HANDLE NO-UNDO.
DEF VAR hTBPackage         AS HANDLE NO-UNDO.
DEF VAR hTBFiles           AS HANDLE NO-UNDO.
DEF VAR hWinToolbar        AS HANDLE NO-UNDO.
DEF VAR hSearchField       AS HANDLE NO-UNDO.
DEF VAR bEntryToPackage    AS LOG    NO-UNDO.
DEF VAR hBtnLoadDoc        AS HANDLE NO-UNDO.
DEF VAR hBuffer            AS HANDLE NO-UNDO.
DEF VAR bEnglish           AS LOG    NO-UNDO INIT YES.
DEF VAR cEditKey           AS CHAR   NO-UNDO.
DEF VAR cEditDesc          AS CHAR   NO-UNDO.
DEF VAR cEditType          AS CHAR   NO-UNDO.
DEF VAR cEditFile          AS CHAR   NO-UNDO.
DEF VAR dLastModDate       AS DATE   NO-UNDO.
DEF VAR iLastModTime       AS INT    NO-UNDO.
DEF VAR hFldDescription    AS HANDLE NO-UNDO.
DEF VAR hColRevCreBy       AS HANDLE NO-UNDO.
DEF VAR hBrwColName        AS HANDLE NO-UNDO.
DEF VAR cCheckOutFileTypes AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY rectBrwPackage rectTBpackage ~
RectBrowseSearch rectBrwFiles rectTBFiles cPackageName cRelativePath ~
cVersion bProPath bCompressUpload bComplete bDownload dCreated bRestart ~
cCreatedBy 
&Scoped-Define DISPLAYED-OBJECTS cPackageName cRelativePath cVersion ~
bProPath bCompressUpload bComplete bDownload dCreated bRestart cCreatedBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadFiles C-Win 
FUNCTION LoadFiles RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRevisionsMenu C-Win 
FUNCTION setRevisionsMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 129 BY .43.

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE cPackageName AS CHARACTER FORMAT "x(40)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cRelativePath AS CHARACTER FORMAT "x(50)" 
     LABEL "Rel.path" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE cVersion AS CHARACTER FORMAT "x(8)" 
     LABEL "Version" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Created" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY .95.

DEFINE RECTANGLE rectBrwFiles
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 4.91.

DEFINE RECTANGLE rectBrwPackage
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 7.76.

DEFINE RECTANGLE rectTBFiles
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY .95.

DEFINE RECTANGLE rectTBpackage
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY .95.

DEFINE VARIABLE bComplete AS LOGICAL INITIAL yes 
     LABEL "Complete" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE bCompressUpload AS LOGICAL INITIAL yes 
     LABEL "Compress upload" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.6 BY .81 NO-UNDO.

DEFINE VARIABLE bDownload AS LOGICAL INITIAL yes 
     LABEL "Download" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE bProPath AS LOGICAL INITIAL yes 
     LABEL "In propath" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE bRestart AS LOGICAL INITIAL no 
     LABEL "Req.restart" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .71 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 13.71 COL 2
     cPackageName AT ROW 10.43 COL 9 COLON-ALIGNED
     cRelativePath AT ROW 11.52 COL 9 COLON-ALIGNED
     cVersion AT ROW 12.57 COL 9 COLON-ALIGNED
     bProPath AT ROW 10.57 COL 68.8
     bCompressUpload AT ROW 11.71 COL 68.8
     bComplete AT ROW 10.57 COL 84.8
     bDownload AT ROW 10.57 COL 99.4
     dCreated AT ROW 12.57 COL 95 COLON-ALIGNED
     bRestart AT ROW 10.57 COL 114.8
     cCreatedBy AT ROW 12.57 COL 116 COLON-ALIGNED
     rectBrwPackage AT ROW 2.52 COL 2
     rectTBpackage AT ROW 1.24 COL 19.8
     RectBrowseSearch AT ROW 1.19 COL 2.2
     rectBrwFiles AT ROW 15.62 COL 1.8
     rectTBFiles AT ROW 14.43 COL 2.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.6 BY 19.71.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 19.71
         WIDTH              = 130.6
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
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
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* <insert window title> */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* ON 'drop-file-notify' OF hBtnDropDoc DO:                        */
/*   DEF VAR cFileNames AS CHAR   NO-UNDO.                         */
/*                                                                 */
/*   DO ix = 1 TO SELF:NUM-DROPPED-FILES:                          */
/*     cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".  */
/*   END.                                                          */
/*   IF cFileNames NE "" THEN                                      */
/*     LoadFiles(TRIM(cFileNames,";")).                            */
/* END.                                                            */


/* ON 'drop-file-notify' OF hBrwFiles DO:                          */
/*   DEF VAR cFileNames AS CHAR   NO-UNDO.                         */
/*                                                                 */
/*   DO ix = 1 TO SELF:NUM-DROPPED-FILES:                          */
/*     cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".  */
/*   END.                                                          */
/*   IF cFileNames NE "" THEN                                      */
/*     LoadFiles(TRIM(cFileNames,";")).                            */
/* END.                                                            */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId"))).
DYNAMIC-FUNCTION("setCurrentObject",hBrwPackage).
RUN OpenQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckOutRecord C-Win 
PROCEDURE CheckOutRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDir         AS CHAR NO-UNDO.
DEF VAR cRowIdList   AS CHAR NO-UNDO.

IF NOT hFMPackage:AVAIL OR hBrwFiles:NUM-SELECTED-ROWS = 0 THEN RETURN.

DO ix = 1 TO hBrwFiles:NUM-SELECTED-ROWS:
  IF hBrwFiles:FETCH-SELECTED-ROW(ix) THEN DO:
    IF hBuffer::cRevCreatedBy NE "" AND hBuffer::cRevCreatedBy NE DYNAMIC-FUNCTION("getASuserId") THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"File " + hBuffer::cFileName + " is already checed out","","").
      RETURN.
    END.
  END.
END.

SYSTEM-DIALOG GET-DIR cDir.

IF cDir NE "" THEN DO ix = 1 TO hBrwFiles:NUM-SELECTED-ROWS:
  IF hBrwFiles:FETCH-SELECTED-ROW(ix) THEN DO:
    IF NOT CAN-DO(cCheckOutFileTypes,hBuffer::cFileType) THEN 
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       hBuffer::cFileName + " cannot be checked out."
                      + CHR(10) + "Filetype not defined as check-out type"
                      ,"","").
    ELSE DO:
      cRowIdList = cRowIdList + (IF cRowIdList NE "" THEN "," ELSE "") + hBuffer::RowIdent1.
      DYNAMIC-FUNCTION("ViewDocs",hFMPackage:NAME,
                       STRING(hFMPackage:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE) + "|" +
                       hBrwFiles:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFileName"):BUFFER-VALUE
                      ,FALSE,cDir).
      DYNAMIC-FUNCTION("DoCreate","JBoxDocRev","ignore",
                       "iJBoxDocumentId,iDocRevNo,cRevCreatedBy,dRevCreated,iRevCreTime,cRevDescription",
                       STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + "|-1|"
                     + DYNAMIC-FUNCTION("getASuserId") + "|" + STRING(TODAY) + "|" + STRING(TIME) + "|" + cDir
                      ,YES).
    END.
  END.
END.
DYNAMIC-FUNCTION("RefreshRowids",hBrwFiles,cRowIdList).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF DYNAMIC-FUNCTION("GetCurrentObject") = hTBFiles THEN 
  APPLY "value-changed" TO hBrwPackage.

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
IF hFMPackage:AVAIL AND DYNAMIC-FUNCTION("getCurrentObject") = hBrwPackage THEN DO:
  bEntryToPackage = YES.
  DYNAMIC-FUNCTION("setAttribute",hBrwFiles,"parentlink",
                   "WHERE cContext = '" + hFMPackage:NAME + "' AND cEntityId = '" + STRING(hFMPackage:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE) + "'").
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwPackage THEN
  DYNAMIC-FUNCTION("setAttribute",hBrwFiles,"parentlink",
                   "WHERE false").

RUN SUPER.

IF hFMPackage:AVAIL THEN
  ASSIGN hBtnLoadDoc:SENSITIVE = TRUE
/*          hBtnDropDoc:SENSITIVE = TRUE  */
         .
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwPackage THEN DO:
  DYNAMIC-FUNCTION("setFileCompression",bCompressUpload:CHECKED IN FRAME {&FRAME-NAME}).
  IF bEntryToPackage THEN
    APPLY "entry" TO hBrwPackage.
  bEntryToPackage = NO.
END.
ELSE setRevisionsMenu().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileNames AS CHAR   NO-UNDO.

IF CAN-DO("new,modified",DYNAMIC-FUNCTION("getToolbarState",hTBPackage)) THEN DO:
  MESSAGE "Save changes before loading files"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

DO ix = 1 TO hBrwFiles:NUM-DROPPED-FILES:
  cFileNames = cFileNames + hBrwFiles:GET-DROPPED-FILE(ix) + ";".
END.
IF cFileNames NE "" THEN 
  LoadFiles(TRIM(cFileNames,";")).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditDocRecord C-Win 
PROCEDURE EditDocRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName     AS CHAR NO-UNDO.

IF hBrwFiles:NUM-SELECTED-ROWS > 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   IF bEnglish THEN "You may only edit one document at a time" ELSE "Du kan bare editere et dokument om gangen",
                   "","").
  RETURN.
END.

ASSIGN cEditKey     = STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE)
       cEditType    = hBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE
       cEditDesc    = hBuffer:BUFFER-FIELD("cDescription"):BUFFER-VALUE
       .

DYNAMIC-FUNCTION("ViewDocs","JBoxDocument",cEditKey,NO,"").

cFileName = DYNAMIC-FUNCTION("getTmpDocFileNames").
FILE-INFO:FILE-NAME = cFileName.

ASSIGN cEditFile    = FILE-INFO:FULL-PATHNAME
       dLastModDate = FILE-INFO:FILE-MOD-DATE
       iLastModTime = FILE-INFO:FILE-MOD-TIME
       .

/*
IF cEditType MATCHES "*doc*" THEN DO:
  CREATE "Word.Application" chEditWord.
  
  chEditWord:ENABLE-EVENTS("WordEvents").
  
  chEditWord:Documents:OPEN(cFileName,,0).
  ASSIGN chEditWord:WindowState = 0
         chEditWord:Visible     = TRUE
         chActiveDoc            = chEditWord:ActiveDocument
         chActiveWin            = chEditWord:ActiveWindow
         .
END.
ELSE DO:
  CREATE "Excel.Application" chEditWord.
  
  chEditWord:ENABLE-EVENTS("ExcelEvents").
  
  chEditWord:Workbooks:OpenText(cFileName,2,,,,,TRUE).
  ASSIGN 
/*          chEditWord:WindowState = 0 */
         chEditWord:Visible     = TRUE
/*          chActiveDoc            = chEditWord:ActiveDocument */
         chActiveWin            = chEditWord:ActiveWindow
         .
END.
*/

IF DYNAMIC-FUNCTION("getFieldValues","JBoxDocRev",
                    "WHERE iJBoxDocumentId = " + cEditKey
                  + " AND iDocRevNo = -1","cRevCreatedBy") = ? THEN
  DYNAMIC-FUNCTION("DoCreate","JBoxDocRev","ignore",
                   "iJBoxDocumentId,iDocRevNo,cRevCreatedBy,dRevCreated,iRevCreTime",
                   cEditKey + "|-1|" + DYNAMIC-FUNCTION("getASuserId") + "|" + STRING(TODAY) + "|" + STRING(TIME),
                   YES).

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
  DISPLAY cPackageName cRelativePath cVersion bProPath bCompressUpload bComplete 
          bDownload dCreated bRestart cCreatedBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY rectBrwPackage rectTBpackage RectBrowseSearch 
         rectBrwFiles rectTBFiles cPackageName cRelativePath cVersion bProPath 
         bCompressUpload bComplete bDownload dCreated bRestart cCreatedBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraDeleteRecord C-Win 
PROCEDURE ExtraDeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obOk AS LOG NO-UNDO INIT TRUE.

IF DYNAMIC-FUNCTION("getCurrentObject") = hFMPackage THEN DO:
  IF hBrwFiles:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    IF DYNAMIC-FUNCTION("runproc","jbappfarm_package_files_delete.p",
                        STRING(hFMPackage:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE),?) THEN
      DYNAMIC-FUNCTION("DoCommit",TRUE).
    ELSE obOk = FALSE.
  END.
  ELSE DYNAMIC-FUNCTION("DoCommit",TRUE).
END.
ELSE DO:
  DO ix = 1 TO hBrwFiles:NUM-SELECTED-ROWS: 
    IF hBrwFiles:FETCH-SELECTED-ROW(ix) THEN DO:
      IF NOT DYNAMIC-FUNCTION("runproc","jbdoc_delete_docrel_and_doc.p",
                          STRING(hBrwFiles:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocRelId"):BUFFER-VALUE),?) THEN
        obOk = FALSE.
    END.
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
RUN enable_UI.  

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  cCheckOutFileTypes = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE cCodeType = 'checkOutFileTypes'(company)","cCodeValue").
  IF cCheckOutFileTypes = ? THEN
    cCheckOutFileTypes = "p,i,w,cls,r,df,doc,docx,xls,xlsx,html,htm".

  hBrwPackage = DYNAMIC-FUNCTION("NewBrowse",
                                 rectBrwPackage:HANDLE,
                                 100,
                                 "",
                                 "JBoxPackage"
                               + ";cPackageName"
                               + ";cVersion"
                               + ";cRelativePath"
                               + ";bComplete"
                               + ";bDownLoad"
                               + ";bProPath"
                               + ";bRestart"
                               + ";bCompressUpload"
                               + ";dCreated|Created"
                               + ";cCreatedBy|By"
                               + ";dModified|Modified"
                               + ";cModifiedBy|By"
                               + ";iJBoxPackageId"
                                ,"WHERE false"
                                ,"").

  hBrwPackage:NAME = "brwPackage".

  DYNAMIC-FUNCTION("setSortString",hBrwPackage,"cPackageName"). 
  DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"viewRecordCount","no").

  DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"extraFilterFields",
                   "JBoxApplication.cAppName|Used in Application").
  DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"prescanqueryJBoxApplication","EACH JBoxAppPackage OF JBoxApplication NO-LOCK,FIRST JBoxPackage OF JBoxAppPackage NO-LOCK"). 

  DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"filterlookupfields_cAppName","JBoxApplication;cAppName").
  DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"filterlookupquery_cAppName","where iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")).
  DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"filterlookupreturnfield_cAppName","cAppName").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrwPackage,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrwPackage,hSearchField).

  hFMPackage = DYNAMIC-FUNCTION("NewFieldMap",
                                hBrwPackage:QUERY,
                                FRAME {&FRAME-NAME}:HANDLE,
                                "cPackageName,cRelativePath,cVersion,bProPath,bComplete,bDownload,bRestart,bCompressUpload","",
                                "dCreated,cCreatedBy","",
                                "").
  DYNAMIC-FUNCTION("createObjectLink",hBrwPackage,hFMPackage).
  DYNAMIC-FUNCTION("setAttribute",hFMPackage,"bufferextrafields","iJBoxCompanyId").

  hTBPackage = DYNAMIC-FUNCTION("NewToolbar",
                                rectTBPackage:HANDLE,
                                "Package",
                                "New,Copy,Undo,Delete,Save,Filter,Excel"
                                ,"maxborder").
  DYNAMIC-FUNCTION("createObjectLink",hBrwPackage,hTBPackage).
  DYNAMIC-FUNCTION("createObjectLink",hFMPackage,hTBPackage).


  hBrwFiles = DYNAMIC-FUNCTION("NewBrowse",
                                 rectBrwFiles:HANDLE,
                                 1000,
                                 "MULTIPLE",
                                 "JBoxDocRel"
                                 + ";!iJBoxDocRelId"
                                 + ";!iJBoxDocumentId"
                               + ",JBoxDocument"
                                 + ";cFileName|File name"
                                 + ";cFileType"
                                 + ";cDescription|Version"
                                 + ";dFileCreateDate"
                                 + ";+cFileCreateTime|CHARACTER|x(5)|jb_hhmm(iFileCreateTime)|Cre.time"
                                 + ";!iFileCreateTime"
                                 + ";dFileModDate"
                                 + ";+cFileModTime|CHARACTER|x(5)|jb_hhmm(iFileModTime)|Mod.time"
                                 + ";!iFileModTime"
                                 + ";iDocSize|Size"
                                 + ";cFullPathName|Org.path"
                                 + ";dCreated|Loaded"
                                 + ";cCreatedBy|By"
                               + ",JBoxDocRev"
                                 + ";dRevCreated|Checked out"
                                 + ";+cRevCreTime|CHARACTER|x(5)|jb_hhmm(iRevCreTime)|Check out time"
                                 + ";cRevCreatedBy|Checked out by"
                                 + ";cRevDescription|Checked out to"
                                ,"WHERE FALSE"
                               + ",FIRST JBoxDocument OF JBoxDocRel NO-LOCK"
                               + ",FIRST JBoxDocRev OF JBoxDocument NO-LOCK WHERE JBoxDocRev.iDocRevNo < 0 OUTER-JOIN"
                                ,"").

  hFldDescription  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwFiles,"cDescription","cDescription","","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwFiles,hFldDescription,"cDescription").

  hBrwColName = DYNAMIC-FUNCTION("getBrowseColumn",hBrwFiles,"cFileName").

  hBrwFiles:DROP-TARGET = TRUE.
  hBrwFiles:TOOLTIP = "Drop files here. If a file exists it will automatically be replaced in the package".
  DYNAMIC-FUNCTION("setAttribute",hBrwFiles,"sortmap","cFileCreateTime;iFileCreateTime,cFileModTime;iFileModTime").
  DYNAMIC-FUNCTION("setAttribute",hBrwFiles,"getrecordcount","yes").
  DYNAMIC-FUNCTION("setSortString",hBrwFiles,"dFileModDate;desc,cFileModTime;desc").
  DYNAMIC-FUNCTION("CreateParentLink",hBrwFiles,hBrwPackage,"parentlink").
  DYNAMIC-FUNCTION("setAttribute",hBrwFiles,"enableOnDblClick","yes").

  hBuffer = hBrwFiles:QUERY:GET-BUFFER-HANDLE(1).
  hColRevCreBy = hBuffer:BUFFER-FIELD("cRevCreatedBy").

  DYNAMIC-FUNCTION("NewMenuBand",
                   hBrwFiles,
                   "MultiSortBrowse;Sort on multiple columns"
                  ,"").

  hTBFiles = DYNAMIC-FUNCTION("NewToolBar",
                    rectTBFiles:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Files",                          /* Corresponding menu label - no menu if blank */
                    "LoadFromDisc;LoadFiles;Load files;LoadFromDisc;bmp/open16e.bmp"
                  + ",delete"
                  + ",SaveToDisc;SaveFiles;Save selected files to disc;SaveToDisc;bmp/save16e.bmp"                                                    
                    ,"maxborder").       

  DYNAMIC-FUNCTION("CreateObjectLink",hBrwFiles,hTBFiles).

  hBtnLoadDoc = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTBFiles,"buttonLoadFromDisc")).


  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"RectBrowseSearch").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectBrwPackage,brwPackage,rectTBpackage,rectTBFiles").
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).

  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME}, 
                   STRING(cCreatedBy:HANDLE) + "," + STRING(cPackageName:HANDLE) + "," + STRING(cRelativePath:HANDLE) + "," + STRING(cVersion:HANDLE) + "," + STRING(dCreated:HANDLE) + "," + STRING(bComplete:HANDLE) + "," + STRING(bCompressUpload:HANDLE) + "," + STRING(bDownload:HANDLE) + "," + STRING(bProPath:HANDLE) + "," + STRING(bRestart:HANDLE)
                 + "," + STRING(hBrwPackage) + "," + STRING(rectBrwPackage:HANDLE) + "," + STRING(hBrwFiles) + "," + STRING(rectBrwFiles:HANDLE) + "," + STRING(rectTbFiles:HANDLE)
                 + "," + DYNAMIC-FUNCTION("getToolbarHandles",hTbFiles,"")
/*                     DYNAMIC-FUNCTION("getWidgetsByLasso",rectSplitBar:HANDLE,        */
/*                                      0,"rectangle,browse,button,fill-in,toggle-box") */
                    ).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE,220,150).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,330,1000,0,0).

  SUBSCRIBE TO "ChangeCompany" ANYWHERE.
  RUN ChangeCompany.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFromDisc C-Win 
PROCEDURE LoadFromDisc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileNames AS CHAR NO-UNDO.

cFileNames = DYNAMIC-FUNCTION("SelectFileNames","All files|*.*",?,"Select files").
IF cFileNames NE "" THEN LoadFiles(cFileNames).
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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
APPLY "entry" TO hBrwPackage.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MyDeleteMessage C-Win 
PROCEDURE MyDeleteMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obOk AS LOG NO-UNDO.

obOk = 1 = DYNAMIC-FUNCTION("DoMessage",0,1,"Delete?","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

IF ihFillIn = hFldDescription THEN 
  obOk = DYNAMIC-FUNCTION("DoUpdate","JBoxDocument","ignore",
                          "",
                          hBuffer:BUFFER-FIELD("RowIdent2"):BUFFER-VALUE,
                          "cDescription",
                          ihFillIn:SCREEN-VALUE,
                          YES).  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleaseLockRecord C-Win 
PROCEDURE ReleaseLockRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBuffer:AVAIL AND hBrwFiles:NUM-SELECTED-ROWS = 1 THEN DO:
  IF DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
                      "iJBoxDocumentId,iDocRevNo",
                      STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + "|-1",
                      YES) THEN
    RUN InvokeMethod(hBrwFiles,"DisplayRecord").
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

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwFiles THEN DO:
  IF hColRevCreBy:BUFFER-VALUE NE "" THEN
    hBrwColName:BGCOLOR = 12.
  ELSE
    hBrwColName:BGCOLOR = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hFMPackage,"bufferextravalues",STRING(DYNAMIC-FUNCTION("getCompanyId"))).
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveToDisc C-Win 
PROCEDURE SaveToDisc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDir AS CHAR NO-UNDO.

IF NOT hFMPackage:AVAIL THEN RETURN.

SYSTEM-DIALOG GET-DIR cDir.

IF cDir NE "" THEN DO ix = 1 TO hBrwFiles:NUM-SELECTED-ROWS:
  IF hBrwFiles:FETCH-SELECTED-ROW(ix) THEN
    DYNAMIC-FUNCTION("ViewDocs",hFMPackage:NAME,
                     STRING(hFMPackage:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE) + "|" +
                     hBrwFiles:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFileName"):BUFFER-VALUE
                    ,FALSE,cDir).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewOlderVersionRecord C-Win 
PROCEDURE ViewOlderVersionRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cVersionLabel AS CHAR   NO-UNDO.
DEF VAR hMenuItem     AS HANDLE NO-UNDO.

ASSIGN hMenuItem     = DYNAMIC-FUNCTION("getCurrentWidget")
       cVersionLabel = hMenuItem:LABEL
       .

DYNAMIC-FUNCTION("ViewDocs","JBoxDocRev",
                 STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + "|" + ENTRY(1,cVersionLabel,"-"),
                 TRUE,"").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.

IF icBrowseName = "rectBrwFiles" THEN DO:
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cFileName").
  IF VALID-HANDLE(hColumn) THEN hColumn:WIDTH-PIXELS = 150.

  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cDescription").
  IF VALID-HANDLE(hColumn) THEN hColumn:WIDTH-PIXELS = 150.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadFiles C-Win 
FUNCTION LoadFiles RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DO ix = 1 TO NUM-ENTRIES(icFileNames,";"):
  bOk = hBuffer:FIND-FIRST("WHERE cFileName = '" + ENTRY(NUM-ENTRIES(ENTRY(ix,icFileNames,";"),"\"),ENTRY(ix,icFileNames,";"),"\") + "'") NO-ERROR. 
  IF NOT bOK THEN
    bOk = hBuffer:FIND-FIRST("WHERE cFileName = '" + ENTRY(NUM-ENTRIES(ENTRY(ix,icFileNames,";"),"\"),ENTRY(ix,icFileNames,";"),"\") + ".gz'") NO-ERROR. 
  IF bOk THEN DO:
    IF hBuffer::cRevCreatedBy NE "" AND hBuffer::cRevCreatedBy NE DYNAMIC-FUNCTION("getASuserId") THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,"Cannot load / check in " + hBuffer::cFileName + CHR(10) + "The file is checked out by another user","","").
    ELSE DO:
      DYNAMIC-FUNCTION("setDocLoadParam","replacedoc" + (IF CAN-DO(cCheckOutFileTypes,hBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE) THEN ",saveoldversion" ELSE "")).
      DYNAMIC-FUNCTION("LoadDocs",ENTRY(ix,icFileNames,";"),"",
                       STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE),
                       (IF hFmPackage:BUFFER-FIELD("cVersion"):BUFFER-VALUE NE "" THEN
                          hFmPackage:BUFFER-FIELD("cVersion"):BUFFER-VALUE
                        ELSE
                          hBuffer:BUFFER-FIELD("cDescription"):BUFFER-VALUE)).  
      IF hBuffer::RowIdent3 NE "" THEN
        DYNAMIC-FUNCTION("DoDelete","JBoxDocRev","ignore",
                          "",
                          hBuffer::RowIdent3,
                          YES).
    END.
  END.
  ELSE DYNAMIC-FUNCTION("LoadDocs",ENTRY(ix,icFileNames,";"),
                        hFMPackage:NAME,STRING(hFMPackage:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE),
                        hFmPackage:BUFFER-FIELD("cVersion"):BUFFER-VALUE).

/*     DYNAMIC-FUNCTION("runproc","jbdoc_delete_docrel_and_doc.p",                                                   */
/*                       STRING(hBuffer:BUFFER-FIELD("iJBoxDocRelId"):BUFFER-VALUE),?). */
END.
/* DYNAMIC-FUNCTION("LoadDocs",icFileNames,                                                                                                               */
/*                  hFMPackage:NAME,STRING(hFMPackage:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE),hFMPackage:BUFFER-FIELD("cPackageName"):BUFFER-VALUE). */
APPLY "value-changed" TO hBrwPackage.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRevisionsMenu C-Win 
FUNCTION setRevisionsMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cVersionMenu  AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR iy            AS INT  NO-UNDO.
DEF VAR hOlderVerMenu AS HANDLE NO-UNDO.

DEF VAR cLinkLabel    AS CHAR   NO-UNDO.
DEF VAR bAddMenu      AS LOG    NO-UNDO.
DEF VAR cDocRevList   AS CHAR   NO-UNDO.
DEF VAR cLockInfo     AS CHAR   NO-UNDO.

DELETE OBJECT hBrwFiles:POPUP-MENU NO-ERROR.

IF NOT hBuffer:AVAIL THEN RETURN NO.

IF hBrwFiles:NUM-SELECTED-ROWS = 1 AND DYNAMIC-FUNCTION("runProc","jbdoc_getdocrevisions.p",STRING(hBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE),?) THEN
  ASSIGN cDocRevList = DYNAMIC-FUNCTION("getTransactionMessage")
         cLockInfo   = ENTRY(2,cDocRevList,"|")
         cDocRevList = ENTRY(1,cDocRevList,"|")
         .

DO ix = 1 TO NUM-ENTRIES(cDocRevList):
  cVersionMenu = cVersionMenu 
               + (IF cVersionMenu NE "" THEN "," ELSE "")
               + "ViewOlderVersion;"
               + ENTRY(ix,cDocRevList)
               .
END.

DYNAMIC-FUNCTION("NewMenuBand",hBrwFiles
                ,"MultiSortBrowse;" + (IF bEnglish THEN "Sort on multiple columns" ELSE "Sorter på flere kolonner")
              + ",CheckOut;" + (IF bEnglish THEN "Check out file(s)" ELSE "Sjekk ut fil(er)")
              + (IF cVersionMenu NE "" THEN 
                  ",|" + (IF bEnglish THEN "Older versions" ELSE "Tidligere versjoner")
                 ELSE "")
                ,"").

IF cVersionMenu NE "" THEN DO:
  hOlderVerMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrwFiles,"placeholder1")) NO-ERROR.
  IF VALID-HANDLE(hOlderVerMenu) THEN
    DYNAMIC-FUNCTION("NewMenuBand",hOlderVerMenu
    ,cVersionMenu
    ,"").
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

