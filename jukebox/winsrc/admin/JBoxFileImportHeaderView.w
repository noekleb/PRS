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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
                          
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hTbActivate       AS HANDLE NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR cMenuCatParam     AS CHAR   NO-UNDO.
DEF VAR cTargetDb         AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExportCat tbActivate cDescription ~
cImportType cDelimiter iHeaderLines cImportProg cRunTimeParameterList ~
cTargetDBtable bIgnoreDuplicates cDataSourceCatalog cMoveImportedFilesTo ~
bLogFilesInDB bAllowRecordDeleteFromImportLog cComment btnExportCat-2 ~
fiCommentLabel 
&Scoped-Define DISPLAYED-OBJECTS cDescription cImportType cDelimiter ~
iHeaderLines cImportProg cRunTimeParameterList cTargetDBtable ~
bIgnoreDuplicates cDataSourceCatalog cMoveImportedFilesTo bLogFilesInDB ~
bAllowRecordDeleteFromImportLog cComment fiCommentLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoMyPublish C-Win 
FUNCTION DoMyPublish RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExportCat  NO-FOCUS
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnExportCat-2  NO-FOCUS
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE cImportType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "FLAT FILE","XML","PROGRESS" 
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cTargetDBtable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import to db-table" 
     VIEW-AS COMBO-BOX INNER-LINES 50
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 43.2 BY 1 NO-UNDO.

DEFINE VARIABLE cComment AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 54.8 BY 2.67.

DEFINE VARIABLE cDataSourceCatalog AS CHARACTER FORMAT "x(256)" 
     LABEL "Data source catalog" 
     VIEW-AS FILL-IN 
     SIZE 50.2 BY 1.

DEFINE VARIABLE cDelimiter AS CHARACTER FORMAT "x(6)" 
     LABEL "Delimiter" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1.

DEFINE VARIABLE cDescription AS CHARACTER FORMAT "x(50)" 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.

DEFINE VARIABLE cImportProg AS CHARACTER FORMAT "x(40)" 
     LABEL "Import program" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.

DEFINE VARIABLE cMoveImportedFilesTo AS CHARACTER FORMAT "x(256)" 
     LABEL "Move files to" 
     VIEW-AS FILL-IN 
     SIZE 50.2 BY 1.

DEFINE VARIABLE cRunTimeParameterList AS CHARACTER FORMAT "x(60)" 
     LABEL "Runtime parameters" 
     VIEW-AS FILL-IN 
     SIZE 43.2 BY 1 TOOLTIP "Comma-sep. list of params for runtime selection or program-name for parm.input".

DEFINE VARIABLE fiCommentLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Comment:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE iHeaderLines AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Header lines" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE RECTANGLE tbActivate
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 5.4 BY 1.

DEFINE VARIABLE bAllowRecordDeleteFromImportLog AS LOGICAL INITIAL no 
     LABEL "Allow record delete when deleting import log row" 
     VIEW-AS TOGGLE-BOX
     SIZE 50.4 BY 1 TOOLTIP "Allow record delete when deleting import log row".

DEFINE VARIABLE bIgnoreDuplicates AS LOGICAL INITIAL no 
     LABEL "Ignore duplicates" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE bLogFilesInDB AS LOGICAL INITIAL no 
     LABEL "Log source files in database" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnExportCat AT ROW 9.67 COL 73 NO-TAB-STOP 
     cDescription AT ROW 1.19 COL 21 COLON-ALIGNED
     cImportType AT ROW 2.24 COL 21 COLON-ALIGNED
     cDelimiter AT ROW 3.29 COL 20.8 COLON-ALIGNED
     iHeaderLines AT ROW 3.29 COL 57.8 COLON-ALIGNED
     cImportProg AT ROW 4.33 COL 20.8 COLON-ALIGNED
     cRunTimeParameterList AT ROW 5.38 COL 2.8 HELP
          "Comma-sep. list of params for runtime selection or program-name"
     cTargetDBtable AT ROW 6.48 COL 20.8 COLON-ALIGNED
     bIgnoreDuplicates AT ROW 7.62 COL 22.8
     cDataSourceCatalog AT ROW 8.57 COL 2.4
     cMoveImportedFilesTo AT ROW 9.67 COL 9.6
     bLogFilesInDB AT ROW 10.91 COL 22.6
     bAllowRecordDeleteFromImportLog AT ROW 11.71 COL 22.6
     cComment AT ROW 14 COL 22.2 NO-LABEL
     btnExportCat-2 AT ROW 8.62 COL 73 NO-TAB-STOP 
     fiCommentLabel AT ROW 14.1 COL 9.8 COLON-ALIGNED NO-LABEL
     tbActivate AT ROW 12.71 COL 21.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.4 BY 16.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer"
         HEIGHT             = 16.29
         WIDTH              = 88.4
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


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
/* SETTINGS FOR FILL-IN cDataSourceCatalog IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cMoveImportedFilesTo IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cRunTimeParameterList IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
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
ON END-ERROR OF C-Win /* Customer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportCat C-Win
ON CHOOSE OF btnExportCat IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cOutputCat    AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-DIR cOutputCat RETURN-TO-START-DIR.
  IF cOutputCat NE "" THEN DO:
    cMoveImportedFilesTo:SCREEN-VALUE = cOutputCat.
    APPLY "any-printable" TO cMoveImportedFilesTo.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportCat-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportCat-2 C-Win
ON CHOOSE OF btnExportCat-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cOutputCat    AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-DIR cOutputCat RETURN-TO-START-DIR.
  IF cOutputCat NE "" THEN DO:
    cDataSourceCatalog:SCREEN-VALUE = cOutputCat.
    APPLY "any-printable" TO cDataSourceCatalog.
  END.
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
  PUBLISH "InvalidateHandle".
  RUN disable_UI.
END.

{incl/supptrigg.i hQuery}
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActivateRecord C-Win 
PROCEDURE ActivateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileNames      AS CHAR NO-UNDO.
DEF VAR cTmpFileName    AS CHAR NO-UNDO.
DEF VAR ocReturn        AS CHAR NO-UNDO.
DEF VAR cImportFiles    AS CHAR NO-UNDO.
DEF VAR cConvFile       AS CHAR NO-UNDO.
DEF VAR cParam          AS CHAR NO-UNDO.
DEF VAR cTmp            AS CHAR NO-UNDO.
DEF VAR cParamField     AS CHAR NO-UNDO.
DEF VAR cParamProg      AS CHAR NO-UNDO.
DEF VAR cParamTargetFld AS CHAR NO-UNDO. 
DEF VAR bTest           AS LOG  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF cDataSourceCatalog:SCREEN-VALUE = "" THEN DO:
    cFileNames = DYNAMIC-FUNCTION("SelectFileNames","Alle filer|*.*",?,"Velg fil(er) for opplasting").
    IF cFileNames = "" THEN RETURN.
    DO ix = 1 TO NUM-ENTRIES(cFileNames,";"):
      cTmpFileName = ENTRY(ix,cFileNames,";").
      IF cTmpFileName MATCHES "*.xls*" THEN
         cConvFile = DYNAMIC-FUNCTION("ConvXlsToCsv",cTmpFileName).
      ELSE cConvFile = cTmpFileName.
      cImportFiles = cImportFiles + cConvFile + ";".
    END.
  END.
  ELSE DO:
    INPUT FROM OS-DIR(cDataSourceCatalog:SCREEN-VALUE) NO-ECHO.
    REPEAT:
      IMPORT cTmpFileName. 
      FILE-INFO:FILE-NAME = RIGHT-TRIM(cDataSourceCatalog:SCREEN-VALUE,"\") + "\" + cTmpFileName.
      IF FILE-INFO:FILE-TYPE NE "FRW" THEN NEXT.

      IF cTmpFileName MATCHES "*.xls*" THEN
         cConvFile = DYNAMIC-FUNCTION("ConvXlsToCsv",FILE-INFO:FULL-PATHNAME).
      ELSE cConvFile = FILE-INFO:FULL-PATHNAME.

      ASSIGN cFileNames   = cFileNames + FILE-INFO:FULL-PATHNAME + ";"
             cImportFiles = cImportFiles + cConvFile + ";".
    END.
    INPUT CLOSE.
  END.
  IF hFieldMap:BUFFER-FIELD("cRunTimeParameterList"):BUFFER-VALUE NE "" THEN DO:
    ASSIGN cParamField = hFieldMap:BUFFER-FIELD("cRunTimeParameterList"):BUFFER-VALUE
           cParamProg  = ENTRY(1,cParamField).
    IF SEARCH(cParamProg) NE ? OR
       SEARCH(SUBSTR(cParamProg,1,LENGTH(cParamProg) - 2) + ".r") NE ?  THEN DO:
      IF NUM-ENTRIES(cParamField) > 1 THEN
        cParamField = SUBSTR(cParamField,INDEX(cParamField,",") + 1).
      ELSE cParamField = "".
      RUN VALUE(cParamProg) (cParamField,OUTPUT cParam).     
    END.
    ELSE DO:
      IF NUM-ENTRIES(cParamField,";") > 1 THEN
        ASSIGN cParamTargetFld = ENTRY(1,cParamField,";")
               cParamField     = ENTRY(2,cParamField,";")
               .
      DO ix = 1 TO NUM-ENTRIES(cParamField):
        cTmp = cTmp + (IF cTmp NE "" THEN "|" ELSE "") 
             + ENTRY(ix,cParamField) + "|"
             + ENTRY(ix,cParamField).
      END.
      RUN JBoxDSimpleSelectList.w (cTmp,?,OUTPUT cParam).
    END.  
    IF cParam = ? THEN RETURN.
  END.
  IF cParamTargetFld NE "" THEN
    cParam = cParamTargetFld + ";" + cParam.

  IF DYNAMIC-FUNCTION("DoMessage",0,4,"Run as test (no database update)","","") = 6 THEN DO:
    bTest = YES.  
    DYNAMIC-FUNCTION("setDocLoadParam",cParam + "|test").
  END.
  ELSE IF cParam NE "" THEN
    DYNAMIC-FUNCTION("setDocLoadParam",cParam).

  ASSIGN cFileNames = TRIM(cFileNames,";")
         cImportFiles = TRIM(cImportFiles,";").
  DYNAMIC-FUNCTION("setFileCompression",NO).
  IF NOT DYNAMIC-FUNCTION("LoadDocs"
                   ,cImportFiles
                   ,hFieldMap:NAME
                   ,STRING(hFieldMap:BUFFER-FIELD("iJBoxFileImportHeaderId"):BUFFER-VALUE)
                   ,"") THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getOutParam"),"","").
  ELSE IF cMoveImportedFilesTo:SCREEN-VALUE NE "" AND NOT bTest THEN DO:
    RUN jbutil_movefiles.p (cFileNames,cMoveImportedFilesTo:SCREEN-VALUE,OUTPUT ocReturn).
    IF ocReturn NE "" THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,ocReturn,"","").
  END.
END.

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY cDescription cImportType cDelimiter iHeaderLines cImportProg 
          cRunTimeParameterList cTargetDBtable bIgnoreDuplicates 
          cDataSourceCatalog cMoveImportedFilesTo bLogFilesInDB 
          bAllowRecordDeleteFromImportLog cComment fiCommentLabel 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnExportCat tbActivate cDescription cImportType cDelimiter 
         iHeaderLines cImportProg cRunTimeParameterList cTargetDBtable 
         bIgnoreDuplicates cDataSourceCatalog cMoveImportedFilesTo 
         bLogFilesInDB bAllowRecordDeleteFromImportLog cComment btnExportCat-2 
         fiCommentLabel 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       If multiple databases connected the menu cat. parameter can be used to select database by issuing "db_" as prefix to the database name  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  cMenuCatParam = DYNAMIC-FUNCTION("getMenuCatParameter" IN hParent) NO-ERROR.

  cTargetDb = (IF cMenuCatParam BEGINS "db_" THEN SUBSTR(cMenuCatParam,4) + "." ELSE "").

  ASSIGN cTargetDBtable:DELIMITER = "|"
         cTargetDBtable:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("getFieldList",cTargetDb
                                                                 + "_file;_file-name;_file-name",
                                                                  "WHERE NOT _file-name BEGINS 'JBox' AND _tbl-type = 'T'")
         .

  hQuery = DYNAMIC-FUNCTION("NewQuery"
          ,100
          ,""
          ,"JBoxFileImportHeader"
          ,"WHERE false"
          ,"").

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME {&FRAME-NAME}:HANDLE,
                             "cDescription,cImportProg,cDelimiter,iHeaderLines,cImportType,cTargetDBtable,bIgnoreDuplicates,cDataSourceCatalog,cMoveImportedFilesTo,bLogFilesInDB,cRunTimeParameterList,bAllowRecordDeleteFromImportLog,cComment","",
                             "","",
                             ""). 

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","iJBoxCompanyId,cMenuCategory").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,THIS-PROCEDURE).

  hTbActivate = DYNAMIC-FUNCTION("NewToolbar",tbActivate:HANDLE,"",
                                 "activate",
                                 "flat|enable").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"recordavailwidgets",
                    DYNAMIC-FUNCTION("getToolbarHandles",hTbActivate,"")).


  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"tbActivate,cComment,fiCommentLabel,"
                 + DYNAMIC-FUNCTION("getToolbarNames",hTbActivate,"")).
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
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
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",DYNAMIC-FUNCTION("getCompany") + "|" + cMenuCatParam).
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoMyPublish C-Win 
FUNCTION DoMyPublish RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
PUBLISH "FileImportToTable" (cTargetDb,cTargetDBtable:HANDLE:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

RETURN TRUE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "tbActivate,cComment,fiCommentLabel").
DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "cRunTimeParameterList").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

