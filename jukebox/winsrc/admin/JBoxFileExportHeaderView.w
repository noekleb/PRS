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
DEF VAR hTbFilter         AS HANDLE NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hExportQuery      AS HANDLE NO-UNDO.

DEF STREAM sExport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbActivate tbFilter cDescription cExportType ~
cDelimiter btnFilterFields bHeaderLines cExportProg cSourceDBtable ~
cFilterFields cFilter cExportFilesTo bLogFilesInDB btnExportCat 
&Scoped-Define DISPLAYED-OBJECTS cDescription cExportType cDelimiter ~
bHeaderLines cExportProg cSourceDBtable cFilterFields cFilter ~
cExportFilesTo bLogFilesInDB 

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

DEFINE BUTTON btnFilterFields  NO-FOCUS
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE cExportType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Export type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "FLAT FILE","XML","PROGRESS" 
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cSourceDBtable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Export from db-table" 
     VIEW-AS COMBO-BOX INNER-LINES 50
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 43.2 BY 1 NO-UNDO.

DEFINE VARIABLE cFilter AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 43 BY 1.76 NO-UNDO.

DEFINE VARIABLE cDelimiter AS CHARACTER FORMAT "x(6)" 
     LABEL "Delimiter" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1.

DEFINE VARIABLE cDescription AS CHARACTER FORMAT "x(50)" 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.

DEFINE VARIABLE cExportFilesTo AS CHARACTER FORMAT "x(50)" 
     LABEL "Export files to" 
     VIEW-AS FILL-IN 
     SIZE 43.2 BY 1.

DEFINE VARIABLE cExportProg AS CHARACTER FORMAT "x(40)" 
     LABEL "Export program" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.

DEFINE VARIABLE cFilterFields AS CHARACTER FORMAT "x(50)" 
     LABEL "Filter on" 
     VIEW-AS FILL-IN 
     SIZE 43.2 BY 1.

DEFINE RECTANGLE tbActivate
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4.2 BY 1.

DEFINE RECTANGLE tbFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 5 BY .95.

DEFINE VARIABLE bHeaderLines AS LOGICAL INITIAL no 
     LABEL "Header lines" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE bLogFilesInDB AS LOGICAL INITIAL no 
     LABEL "Log exported files in database" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cDescription AT ROW 1.19 COL 21 COLON-ALIGNED
     cExportType AT ROW 2.24 COL 21 COLON-ALIGNED
     cDelimiter AT ROW 3.29 COL 20.8 COLON-ALIGNED
     btnFilterFields AT ROW 6.52 COL 66 NO-TAB-STOP 
     bHeaderLines AT ROW 3.43 COL 48.8
     cExportProg AT ROW 4.33 COL 20.8 COLON-ALIGNED
     cSourceDBtable AT ROW 5.38 COL 20.8 COLON-ALIGNED
     cFilterFields AT ROW 6.48 COL 20.8 COLON-ALIGNED HELP
          "Comma-separated list of filter fields"
     cFilter AT ROW 7.62 COL 22.8 NO-LABEL
     cExportFilesTo AT ROW 9.48 COL 9
     bLogFilesInDB AT ROW 10.67 COL 22.8
     btnExportCat AT ROW 9.52 COL 66 NO-TAB-STOP 
     tbActivate AT ROW 11.67 COL 21.8
     tbFilter AT ROW 7.71 COL 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73.6 BY 11.91.


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
         HEIGHT             = 11.91
         WIDTH              = 73.6
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
                                                                        */
/* SETTINGS FOR FILL-IN cExportFilesTo IN FRAME DEFAULT-FRAME
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
    cExportFilesTo:SCREEN-VALUE = cOutputCat.
    APPLY "any-printable" TO cExportFilesTo.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilterFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilterFields C-Win
ON CHOOSE OF btnFilterFields IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList    AS CHAR NO-UNDO.
  DEF VAR cNameList     AS CHAR NO-UNDO.

  IF cFilterFields:SCREEN-VALUE NE "" THEN
    ASSIGN cRowIdList = DYNAMIC-FUNCTION("getRowIdList","_file,_field","_field",
                                         "WHERE _file-name = '" + cSourceDBtable:SCREEN-VALUE + "',EACH _field OF _file NO-LOCK WHERE CAN-DO('" + cFilterFields:SCREEN-VALUE + "',_field-name)")
           cNameList    = REPLACE(cFilterFields:SCREEN-VALUE,",","|").
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "_field;_field-name;_label,_file;!_file-name",
                      "WHERE true,FIRST _file OF _field NO-LOCK WHERE _file-name = '" + cSourceDBtable:SCREEN-VALUE + "'",
                      INPUT-OUTPUT cRowIdList,
                      "_field-name",
                      INPUT-OUTPUT cNameList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cFilterFields:SCREEN-VALUE):
      IF NOT CAN-DO(REPLACE(cNameList,"|",","),ENTRY(ix,cFilterFields:SCREEN-VALUE)) THEN 
        bOK = FALSE.
    END.
    cFilterFields:SCREEN-VALUE = REPLACE(cNameList,"|",",").
    APPLY "any-printable" TO cDescription.
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

{incl/wintrigg.i}
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

{incl/supptrigg.i hFieldMap}

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
DEF VAR cOutputCat   AS CHAR   NO-UNDO.
DEF VAR cTmpFileName AS CHAR   NO-UNDO.
DEF VAR ocReturn     AS CHAR   NO-UNDO.
DEF VAR httExport    AS HANDLE NO-UNDO.
DEF VAR hExpQuery    AS HANDLE NO-UNDO.
DEF VAR hExpBuffer   AS HANDLE NO-UNDO.
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR cDelimiter   AS CHAR   NO-UNDO.
DEF VAR cTmpDelim    AS CHAR   NO-UNDO.
DEF VAR cTransMsg    AS CHAR   NO-UNDO.
DEF VAR cTmp         AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  httExport = DYNAMIC-FUNCTION("getTempTable","jbdoc_getexportfile.p",
                      STRING(hFieldMap:BUFFER-FIELD("iJBoxFileExportHeaderId"):BUFFER-VALUE)
                      ,?).
 cTransMsg = DYNAMIC-FUNCTION("getTransactionMessage").
 IF NUM-ENTRIES(cTransMsg,"|") = 2 THEN DO:
   IF cExportFilesTo:SCREEN-VALUE = "" THEN
     DYNAMIC-FUNCTION("ToExcelViaFile",httExport:DEFAULT-BUFFER-HANDLE,0).
   ELSE DO:
     hExpBuffer = httExport:DEFAULT-BUFFER-HANDLE.
     CREATE QUERY hExpQuery.
     hExpQuery:SET-BUFFERS(hExpBuffer).
     hExpQuery:QUERY-PREPARE("FOR EACH " + hExpBuffer:NAME).
     hExpQuery:QUERY-OPEN().

     DO ix = 1 TO LENGTH(cExportFilesTo:SCREEN-VALUE):
       IF SUBSTR(cExportFilesTo:SCREEN-VALUE,ix,1) = "." THEN DO:
         cFileName = cExportFilesTo:SCREEN-VALUE.
         LEAVE.
       END.
     END.
     IF cFileName = "" THEN
       cFileName = TRIM(cExportFilesTo:SCREEN-VALUE,"\") + "\" + cSourceDBtable:SCREEN-VALUE + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + STRING(TIME) + ".txt".
     cDelimiter = hFieldMap:BUFFER-FIELD("cDelimiter"):BUFFER-VALUE.

     OUTPUT STREAM sExport TO VALUE(cFileName).
     IF hFieldMap:BUFFER-FIELD("bHeaderLines"):BUFFER-VALUE THEN DO:
       DO ix = 1 TO hExpBuffer:NUM-FIELDS:
         IF ix = hExpBuffer:NUM-FIELDS THEN
           cTmpDelim = "".
         ELSE 
           cTmpDelim = cDelimiter.
         PUT STREAM sExport UNFORMATTED hExpBuffer:BUFFER-FIELD(ix):NAME cTmpDelim.
       END.
       PUT STREAM sExport SKIP.
     END.

     hExpQuery:GET-FIRST().
     REPEAT WHILE NOT hExpQuery:QUERY-OFF-END:
       DO ix = 1 TO hExpBuffer:NUM-FIELDS:
         IF ix = hExpBuffer:NUM-FIELDS THEN
           cTmpDelim = "".
         ELSE 
           cTmpDelim = cDelimiter.
         cTmp = STRING(hExpBuffer:BUFFER-FIELD(ix):BUFFER-VALUE).
         cTmp = REPLACE(REPLACE(cTmp,CHR(13),""),CHR(10),"").
         PUT STREAM sExport UNFORMATTED cTmp cTmpDelim. 
/*          PUT STREAM sExport UNFORMATTED hExpBuffer:BUFFER-FIELD(ix):BUFFER-VALUE cTmpDelim. */
       END.
       PUT STREAM sExport SKIP.
       hExpQuery:GET-NEXT().
     END.
     DELETE OBJECT hExpQuery.
     OUTPUT STREAM sExport CLOSE.

     DYNAMIC-FUNCTION("DoUpdate","JBoxFileExportLog","",
                      "",
                      ENTRY(2,cTransMsg,"|"),
                      "cFileName,dFileCreateDate",
                      cFileName + "|" + STRING(TODAY),
                      TRUE).
   END.
 END.
 ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DynFilterDefined C-Win 
PROCEDURE DynFilterDefined :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihQueryObject AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cFilter:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hExportQuery,"queryfilter").
  APPLY "any-printable" TO cDescription.
  DYNAMIC-FUNCTION("setAttribute",hExportQuery,"executedynfilter","no").
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
  DISPLAY cDescription cExportType cDelimiter bHeaderLines cExportProg 
          cSourceDBtable cFilterFields cFilter cExportFilesTo bLogFilesInDB 
      WITH FRAME DEFAULT-FRAME.
  ENABLE tbActivate tbFilter cDescription cExportType cDelimiter 
         btnFilterFields bHeaderLines cExportProg cSourceDBtable cFilterFields 
         cFilter cExportFilesTo bLogFilesInDB btnExportCat 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilterRecord C-Win 
PROCEDURE FilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DeleteObject",hExportQuery).

DO WITH FRAME {&FRAME-NAME}:
  hExportQuery = DYNAMIC-FUNCTION("NewQuery",1,"",
                                  cSourceDBtable:SCREEN-VALUE,
                                  "where false",
                                  "").
  DYNAMIC-FUNCTION("CreateObjectLink",hExportQuery,hTbFilter).

  DYNAMIC-FUNCTION("setAttribute",hExportQuery,"filterfields",cFilterFields:SCREEN-VALUE).

  DYNAMIC-FUNCTION("setCurrentObject",hTbFilter).

  RUN SUPER.

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
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  cSourceDBtable:DELIMITER = "|".
  cSourceDBtable:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("getFieldList","_file;_file-name;_file-name",
                                                           "WHERE NOT _file-name BEGINS 'JBox' AND _tbl-type = 'T'").

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME {&FRAME-NAME}:HANDLE,
                             "cDescription,cExportProg,cDelimiter,bHeaderLines,cExportType,cSourceDBtable,cFilter,cExportFilesTo,bLogFilesInDB","",
                                  /* Updateable buffer fields and their correspondign input fields (blank if the same) */
                             "cFilterFields","",
                                  /* Fields for display and corresponding display widgets */
                             ""). /* If any other fields are used for input these are listed here */

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","iJBoxCompanyId,cFilterFields").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,THIS-PROCEDURE).

  hTbActivate = DYNAMIC-FUNCTION("NewToolbar",tbActivate:HANDLE,"",
                                 "activate",
                                 "flat|enable").

  hTbFilter = DYNAMIC-FUNCTION("NewToolbar",tbFilter:HANDLE,"",
                                 "filter",
                                 "flat|enable").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"recordavailwidgets",
                    DYNAMIC-FUNCTION("getToolbarHandles",hTbActivate,"")).

  SUBSCRIBE TO "DynFilterDefined" ANYWHERE.
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
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",STRING(DYNAMIC-FUNCTION("getCompanyId")) + "|" + cFilterFields:SCREEN-VALUE  IN FRAME {&FRAME-NAME}).
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
PUBLISH "FileExportFromTable" (cSourceDBtable:HANDLE:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

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

