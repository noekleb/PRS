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
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hBrowseLogFile    AS HANDLE NO-UNDO.
DEF VAR hBuffLogFile      AS HANDLE NO-UNDO.

DEF VAR cInitSort         AS CHAR   NO-UNDO.

DEF STREAM strLogFile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwLog brwLogFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse  AS HANDLE,
    INPUT icBrwName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInitSort C-Win 
FUNCTION getInitSort RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQueryString C-Win 
FUNCTION getQueryString RETURNS CHARACTER
  ( INPUT icTable AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBrowse C-Win 
FUNCTION setBrowse RETURNS LOGICAL
  (INPUT iphBrowse AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwLog
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 9.24.

DEFINE RECTANGLE brwLogFile
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 5.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwLog AT ROW 1.05 COL 1
     brwLogFile AT ROW 10.52 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.2 BY 15.71.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 15.71
         WIDTH              = 84.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseLogFile THEN
  RUN OpenLogFileRecord.
ELSE 
  RUN ViewDocumentRecord.

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
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFirstKeyString"):BUFFER-VALUE NE "" AND
   hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cLastKeyString"):BUFFER-VALUE NE "" AND
   hParentBuffer:BUFFER-FIELD("bAllowRecordDeleteFromImportLog"):BUFFER-VALUE
   THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,1,"Delete imported rows using this criteria:" + CHR(10) + getQueryString(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cTargetDBtable"):BUFFER-VALUE)
                      ,"","") = 1 THEN DO:
    IF DYNAMIC-FUNCTION("runproc","jbserv_deletefromtable.p",
                     hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cTargetDBtable"):BUFFER-VALUE + "|" +
                     getQueryString(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cTargetDBtable"):BUFFER-VALUE),
                     ?) THEN
      RUN SUPER.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  ELSE RUN SUPER.
END.
ELSE RUN SUPER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDisabledMenuItems AS CHAR NO-UNDO.

RUN SUPER.

IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE = 0 THEN
    cDisabledMenuItems = "ViewDocument".
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFirstKeyString"):BUFFER-VALUE = "" THEN
    cDisabledMenuItems = cDisabledMenuItems + ",ViewImported".
END.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledevents",TRIM(cDisabledMenuItems,",")).
DYNAMIC-FUNCTION("setToolbar",hBrowse,"").
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
  ENABLE brwLog brwLogFile 
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
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  /* Create the browse: */
   hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           brwLog:HANDLE,   
                           100,                 
                           "",                  
                           "JBoxFileImportLog"  
                           + ";dCreated|Created"
                           + ";+cCreTime|CHARACTER|x(5)|jb_hhmm(iCreTime)|Time"
                           + ";cCreatedBy|By"
                           + ";cFileName"
                           + ";dFileCreateDate"
                           + ";cErrorMessage|Message|x(256)"
                           + ";cFirstKeyString;cLastKeyString;cTargetDBtable"
                           + ";iJBoxFileImportLogId;!iCreTime"
                           + ",JBoxDocRel;iJBoxDocumentId;cEntityId"
                          ,"WHERE false"
                         + ",FIRST JBoxDocRel WHERE cContext = 'JBoxFileImportLog' AND JBoxDocRel.cEntityId = STRING(iJBoxFileImportLogId) OUTER-JOIN",  
                           ""). 
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"sortmap","cCreTime;iCreTime").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"nodeletewarning","yes").
  DYNAMIC-FUNCTION("setSortString",hBrowse,"dCreated;desc,cCreTime;desc").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"CustomDeleteValProc","=jbdoc_importlogdelete.p").

  hBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 200.

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowse,  /* parent widget */
                    "MultiSortBrowse;Sort on multiple columns"
                  + ",ViewImported;View imported data"  
                  + ",ViewDocument;View import-file"  
                    ,"").

  hBrowseLogFile = DYNAMIC-FUNCTION("NewBrowse"
                    ,brwLogFile:HANDLE
                    ,100
                    ,""
                    ,"JBoxFileImportLogFileHeader"
                    + ";cImportLogFileType"
                    + ";cImportLogFileName"
                    + ";cImportLogFileDesc|Log file description|x(256)"
                    + ";!iJBoxFileImportHeaderId"
                    + ";!iJBoxFileImportLogFileHeaderId"
                    + ";!iJBoxFileImportLogId"
                    ,"WHERE false"
                    ,"").
  hBuffLogFile = hBrowseLogFile:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowseLogFile,"viewRecordCount","no").

  DYNAMIC-FUNCTION("CreateParentLink",hBrowseLogFile,hBrowse,"iJBoxFileImportLogId").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowseLogFile
                  ,"OpenLogFile;Open logfile"
                  ,"").

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "brwLogFile," + hBrowseLogFile:NAME).
  DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "brwLogFile," + hBrowseLogFile:NAME).
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
DEF VAR hColumn AS HANDLE NO-UNDO.
hColumn = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hColumn.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenLogFileRecord C-Win 
PROCEDURE OpenLogFileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hTempTable  AS HANDLE NO-UNDO.
DEF VAR hTTbuffer   AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR cFileType   AS CHAR   NO-UNDO.
DEF VAR cFileName   AS CHAR   NO-UNDO.
DEF VAR bSourceData AS LOG    NO-UNDO.
DEF VAR cDelimiter  AS CHAR   NO-UNDO INIT "~t".

IF hBuffLogFile:BUFFER-FIELD("cImportLogFileType"):BUFFER-VALUE = "sourcedata" THEN DO:
  bSourceData = YES.
  IF NUM-ENTRIES(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,".") > 1 THEN
    cFileType = SUBSTR(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,R-INDEX(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,".")).
  ELSE cFileType = ".txt".
END.
ELSE DO:
  IF NUM-ENTRIES(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,".") > 1 THEN
    ASSIGN cFileType  = SUBSTR(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,R-INDEX(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,"."))
           cDelimiter = hParentBuffer:BUFFER-FIELD("cDelimiter"):BUFFER-VALUE.
  ELSE cFileType = ".txt".
END. 
     
cFileName = SESSION:TEMP-DIRECTORY + hBuffLogFile:BUFFER-FIELD("cImportLogFileName"):BUFFER-VALUE 
          + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + "_" + STRING(TIME) + cFileType.

OUTPUT STREAM strLogFile TO VALUE(cFileName).

hTempTable = DYNAMIC-FUNCTION("getTempTable",""
            ,"JBoxFileImportLogFile|WHERE iJBoxFileImportLogFileHeaderId = " 
             + STRING(hBuffLogFile:BUFFER-FIELD("iJBoxFileImportLogFileHeaderId"):BUFFER-VALUE),?).

hTTbuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hTTbuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hTTbuffer:NAME + " BY iImportLogLineNum").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF bSourceData THEN
    PUT STREAM strLogFile UNFORMATTED hTTbuffer:BUFFER-FIELD("cImportLogText"):BUFFER-VALUE SKIP.
  ELSE
    PUT STREAM strLogFile UNFORMATTED hTTbuffer:BUFFER-FIELD("iImportLogLineNum"):BUFFER-VALUE cDelimiter
                    hTTbuffer:BUFFER-FIELD("cImportLogText"):BUFFER-VALUE SKIP.
  hQuery:GET-NEXT().
END.

OUTPUT STREAM strLogFile CLOSE.

DELETE OBJECT hQuery.
DELETE OBJECT hTempTable NO-ERROR.

DYNAMIC-FUNCTION("setWebDoc","",cFileName).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewDocumentRecord C-Win 
PROCEDURE ViewDocumentRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE NE 0 THEN
  DYNAMIC-FUNCTION("ViewDocs",
                    hBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME,
                    STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cEntityId"):BUFFER-VALUE),
                    TRUE,"").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewImportedRecord C-Win 
PROCEDURE ViewImportedRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFileImportData   AS HANDLE NO-UNDO.
DEF VAR hBrwData          AS HANDLE NO-UNDO.

IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cTargetDBtable"):BUFFER-VALUE = "" THEN RETURN.

RUN JBoxDataBrw.w PERSIST SET hFileImportData.

RUN InitializeObject IN hFileImportData (hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cTargetDBtable"):BUFFER-VALUE,
                                         getQueryString(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cTargetDBtable"):BUFFER-VALUE),
                                         getInitSort(),
                                         TRUE).

hFileImportData:CURRENT-WINDOW:TITLE = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cTargetDBtable"):BUFFER-VALUE
                                     + " [importert " + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("dCreated"):BUFFER-VALUE) + "]"
                                       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse  AS HANDLE,
    INPUT icBrwName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icBrwName = "brwLog" THEN
  ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 150.

RETURN YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInitSort C-Win 
FUNCTION getInitSort RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cInitSort.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQueryString C-Win 
FUNCTION getQueryString RETURNS CHARACTER
  ( INPUT icTable AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR htTarget          AS HANDLE NO-UNDO.
DEF VAR hbTarget          AS HANDLE NO-UNDO.
DEF VAR cQueryString      AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR cKeyString        AS CHAR   NO-UNDO.

htTarget = DYNAMIC-FUNCTION("getTempTable","jbserv_gettemptable.p",
                             icTable + "|where false",
                             ?).
hbTarget = htTarget:DEFAULT-BUFFER-HANDLE.                             

cKeyString = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cFirstKeyString"):BUFFER-VALUE.
DO ix = 1 TO NUM-ENTRIES(cKeyString,"|") BY 2:   
  IF ix > 1 THEN cQueryString = cQueryString + " AND ".    
  ELSE cInitSort = ENTRY(ix,cKeyString,"|").
  CASE hbTarget:BUFFER-FIELD(ENTRY(ix,cKeyString,"|")):DATA-TYPE:
    WHEN "character" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " GE '" + ENTRY(ix + 1,cKeyString,"|") + "'".
    WHEN "date" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " GE DATE('" + ENTRY(ix + 1,cKeyString,"|") + "')".
    WHEN "decimal" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " GE DEC('" + ENTRY(ix + 1,cKeyString,"|") + "')".
    WHEN "integer" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " GE INT('" + ENTRY(ix + 1,cKeyString,"|") + "')".
    WHEN "logical" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " = LOGICAL('" + ENTRY(ix + 1,cKeyString,"|") + "')".
  END CASE.
END.
cKeyString = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cLastKeyString"):BUFFER-VALUE.
DO ix = 1 TO NUM-ENTRIES(cKeyString,"|") BY 2:   
  cQueryString = cQueryString + " AND ".    
  CASE hbTarget:BUFFER-FIELD(ENTRY(ix,cKeyString,"|")):DATA-TYPE:
    WHEN "character" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " LE '" + ENTRY(ix + 1,cKeyString,"|") + "'".
    WHEN "date" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " LE DATE('" + ENTRY(ix + 1,cKeyString,"|") + "')".
    WHEN "decimal" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " LE DEC('" + ENTRY(ix + 1,cKeyString,"|") + "')".
    WHEN "integer" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " LE INT('" + ENTRY(ix + 1,cKeyString,"|") + "')".
    WHEN "logical" THEN 
      cQueryString = cQueryString + ENTRY(ix,cKeyString,"|") + " = LOGICAL('" + ENTRY(ix + 1,cKeyString,"|") + "')".
  END CASE.
END.

DELETE OBJECT htTarget.
RETURN cQueryString.

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
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                  "brwLogFile").
DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                  "brwLogFile").  

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBrowse C-Win 
FUNCTION setBrowse RETURNS LOGICAL
  (INPUT iphBrowse AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        
        VIEW-AS ALERT-BOX.
  hBrowse = iphBrowse.
  RETURN TRUE.

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

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBuffer = ihParentQuery:QUERY:GET-BUFFER-HANDLE(1).  

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

