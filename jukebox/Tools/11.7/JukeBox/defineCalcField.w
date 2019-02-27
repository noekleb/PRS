&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM        icSourceTables  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocSourceTable  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocParamField   AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocFieldName    AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocDataType     AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocFormat       AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocLabel        AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocCalcCont     AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocCalcProc     AS CHAR NO-UNDO.
DEF OUTPUT       PARAM obOK            AS LOG  NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF NEW SHARED STREAM P_4GL.
{adeuib/sharvars.i}
{adecomm/adefext.i}

DEF VAR cNewProcList     AS CHAR NO-UNDO.
DEF VAR ix               AS INT  NO-UNDO.
DEF VAR cFieldDataType   AS CHAR NO-UNDO INIT "*".
DEF VAR cFieldAddFirst   AS CHAR NO-UNDO.
DEF VAR bNew             AS LOG  NO-UNDO.
DEF VAR bOk              AS LOG  NO-UNDO.
DEF VAR cBuiltInWithCode AS CHAR NO-UNDO INIT "jb_calc,jb_total,jb_can-find,jb_max,jb_min,jb_random".

DEF TEMP-TABLE ttFields NO-UNDO
    FIELD cFieldName  AS CHAR 
    FIELD cDataType   AS CHAR 
    FIELD cLabel      AS CHAR
    .

DEF TEMP-TABLE ttProcs NO-UNDO
    FIELD cProcName   AS CHAR                            
    FIELD cSignature  AS CHAR
    FIELD cParamField AS CHAR
    FIELD cParFldType AS CHAR 
    FIELD bRunTimePar AS LOG
    FIELD bNew        AS LOG 
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsType btnContainer fiContainer cmbCalcProc ~
fiNewProcedure cmbSourceTable cmbParamField tbRuntimeParam fiFieldName ~
cmbDataType fiFormat fiLabel edHint btnRescan Btn_Help Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS rsType fiContainer edContainer cmbCalcProc ~
fiNewProcedure cmbSourceTable cmbParamField tbRuntimeParam fiFieldName ~
cmbDataType fiFormat fiLabel edHint 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD adjustFrame Dialog-Frame 
FUNCTION adjustFrame RETURNS LOGICAL
  ( INPUT icSize AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckSyntax Dialog-Frame 
FUNCTION CheckSyntax RETURNS LOGICAL
  ( INPUT pCheck AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD importContainer Dialog-Frame 
FUNCTION importContainer RETURNS LOGICAL
  ( INPUT icFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initContainer Dialog-Frame 
FUNCTION initContainer RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD scanContainer Dialog-Frame 
FUNCTION scanContainer RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParamFieldList Dialog-Frame 
FUNCTION setParamFieldList RETURNS LOGICAL
  ( INPUT icScreenValue AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSignature Dialog-Frame 
FUNCTION setSignature RETURNS CHARACTER
  ( INPUT icProcName AS CHAR,
    INPUT ibReplace  AS LOG  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCheckSyntax 
     LABEL "Check syntax" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnContainer 
     LABEL "Choose/new container.." 
     SIZE 27 BY 1.14 TOOLTIP "Server procedure containing one or more procedures for calc.fields".

DEFINE BUTTON btnRescan 
     LABEL "Rescan proc." 
     SIZE 15 BY 1.14 TOOLTIP "After change of procedure name you can rescan the procedure for proc.names".

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cmbCalcProc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Procedure" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 55.6 BY 1 TOOLTIP "Procedure for calculation of value" NO-UNDO.

DEFINE VARIABLE cmbDataType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data type" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "CHARACTER","DATE","DATETIME","DATETIME-TZ","DECIMAL","INTEGER","INT64","LOGICAL" 
     DROP-DOWN-LIST
     SIZE 40.6 BY 1 NO-UNDO.

DEFINE VARIABLE cmbDummy AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dummy" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE cmbParamField AS CHARACTER FORMAT "X(256)":U 
     LABEL "Param.field" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 55.6 BY 1 TOOLTIP "If not selected the rowid for the source table is passed as parameter" NO-UNDO.

DEFINE VARIABLE cmbSourceTable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Source table" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 55.6 BY 1 NO-UNDO.

DEFINE VARIABLE edContainer AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 99 BY 21.67 NO-UNDO.

DEFINE VARIABLE edHint AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 68.4 BY 11.14 NO-UNDO.

DEFINE VARIABLE fiContainer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 NO-UNDO.

DEFINE VARIABLE fiFieldName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field name" 
     VIEW-AS FILL-IN 
     SIZE 40.6 BY 1 TOOLTIP "Field name in client temp-table (view)" NO-UNDO.

DEFINE VARIABLE fiFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 40.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Label" 
     VIEW-AS FILL-IN 
     SIZE 40.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiNewProcedure AS CHARACTER FORMAT "X(256)":U 
     LABEL "Proc.name" 
     VIEW-AS FILL-IN 
     SIZE 55.6 BY 1 NO-UNDO.

DEFINE VARIABLE rsType AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Built in", 1,
"Custom", 2
     SIZE 28 BY 1.19 NO-UNDO.

DEFINE VARIABLE tbRuntimeParam AS LOGICAL INITIAL no 
     LABEL "Enable runtime parameter" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 TOOLTIP "Use calcParam property on query to set parameter in runtime" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rsType AT ROW 1.14 COL 15 NO-LABEL WIDGET-ID 2
     btnContainer AT ROW 1.14 COL 43.6 WIDGET-ID 22
     fiContainer AT ROW 1.24 COL 69 COLON-ALIGNED NO-LABEL
     edContainer AT ROW 2.67 COL 71 NO-LABEL WIDGET-ID 18
     cmbCalcProc AT ROW 2.71 COL 12.8 COLON-ALIGNED WIDGET-ID 24
     fiNewProcedure AT ROW 3.76 COL 12.8 COLON-ALIGNED
     cmbSourceTable AT ROW 4.95 COL 12.8 COLON-ALIGNED WIDGET-ID 6
     cmbParamField AT ROW 6 COL 12.8 COLON-ALIGNED WIDGET-ID 8
     tbRuntimeParam AT ROW 7.05 COL 15 WIDGET-ID 34
     fiFieldName AT ROW 8.29 COL 13 COLON-ALIGNED
     cmbDataType AT ROW 9.33 COL 13 COLON-ALIGNED WIDGET-ID 12
     fiFormat AT ROW 10.38 COL 13 COLON-ALIGNED
     fiLabel AT ROW 11.43 COL 13 COLON-ALIGNED
     edHint AT ROW 13.19 COL 2 NO-LABEL WIDGET-ID 28
     btnCheckSyntax AT ROW 24.71 COL 71 WIDGET-ID 20
     btnRescan AT ROW 24.71 COL 86.4 WIDGET-ID 38
     Btn_Help AT ROW 24.71 COL 123.8
     Btn_Cancel AT ROW 24.71 COL 139.4
     Btn_OK AT ROW 24.71 COL 155.2
     cmbDummy AT ROW 24.95 COL 11 COLON-ALIGNED WIDGET-ID 36
     "Hint:" VIEW-AS TEXT
          SIZE 5.8 BY .62 AT ROW 12.48 COL 2.4 WIDGET-ID 30
     SPACE(163.19) SKIP(12.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Define calculated field for server query"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCheckSyntax IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbDummy IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cmbDummy:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR EDITOR edContainer IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       edContainer:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

ASSIGN 
       edHint:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       fiContainer:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Define calculated field for server query */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCheckSyntax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCheckSyntax Dialog-Frame
ON CHOOSE OF btnCheckSyntax IN FRAME Dialog-Frame /* Check syntax */
DO:
  CheckSyntax(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnContainer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnContainer Dialog-Frame
ON CHOOSE OF btnContainer IN FRAME Dialog-Frame /* Choose/new container.. */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  DEF VAR bOk       AS LOG  NO-UNDO.
  DEF VAR cFileCont AS CHAR NO-UNDO.

  SYSTEM-DIALOG GET-FILE cFileName 
                TITLE "Choose existing or type name for new server procedure.." 
                FILTERS "Source Files (*.p)"   "*.p",
                        "Source Files (*.i)"   "*.i"
                UPDATE bOk.
  IF bOk THEN DO:
    cFileName = REPLACE(cFileName,"\","/").
    IF INDEX(cFileName,".") = 0 THEN cFileName = cFileName + ".p".

    IF SEARCH(cFileName) NE ? THEN DO:
      fiContainer:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cFileName,"/"),cFileName,"/").
      IF SEARCH(fiContainer:SCREEN-VALUE) = ? THEN DO ix = 1 TO 4:
        fiContainer:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cFileName,"/") - ix,cFileName,"/") + "/" + fiContainer:SCREEN-VALUE.
        IF SEARCH(fiContainer:SCREEN-VALUE) NE ? THEN LEAVE.
      END.
      IF SEARCH(fiContainer:SCREEN-VALUE) = ? THEN fiContainer:SCREEN-VALUE = cFileName.

      fiContainer:READ-ONLY = YES.

      importContainer(cFileName).
      edContainer:MODIFIED = NO.
    END.
    ELSE DO:
      ASSIGN fiContainer:SCREEN-VALUE    = cFileName
             fiContainer:READ-ONLY       = NO
             cmbCalcProc:LIST-ITEM-PAIRS = "<new>,new"
             cmbCalcProc:SCREEN-VALUE    = "new"
             .
      initContainer().
      APPLY "value-changed" TO cmbCalcProc.
    END.
    ASSIGN cmbCalcProc:SENSITIVE = YES
           edContainer:SENSITIVE = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRescan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRescan Dialog-Frame
ON CHOOSE OF btnRescan IN FRAME Dialog-Frame /* Rescan proc. */
DO:
  DEF VAR cCurrProcName AS CHAR NO-UNDO.
  cCurrProcName = cmbCalcProc:SCREEN-VALUE.

  cmbCalcProc:LIST-ITEM-PAIRS = scanContainer().

  cmbCalcProc:SCREEN-VALUE = cCurrProcName NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    cmbCalcProc:SCREEN-VALUE = cmbCalcProc:ENTRY(1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  IF rsType:SCREEN-VALUE = "1" THEN
    OS-COMMAND NO-WAIT VALUE("start http://aia.appfarm.no/chemistry/jukebox/JukeBox_tmphtm/built-in-dynamic-calculations-for-queries.htm").
  ELSE
    OS-COMMAND NO-WAIT VALUE("start http://aia.appfarm.no/chemistry/jukebox/JukeBox_tmphtm/calculatedfields.htm").
  
/*   MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  
  IF fiFieldName:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Field must have a name"
            VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.
  IF fiFormat:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Format is missing"
            VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.
  IF fiLabel:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Label is not assignend. Continue?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE bOk.
    IF NOT bOk THEN
      RETURN NO-APPLY.
  END.
  IF cmbCalcProc:SCREEN-VALUE = ? OR cmbCalcProc:SCREEN-VALUE = "" THEN DO:
    MESSAGE "Procedure name is missing"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE bOk.
    RETURN NO-APPLY.
  END.
  
  IF NOT CheckSyntax(NO) THEN DO:
    MESSAGE "Syntax check failed. Continue?"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE bOk.
    IF NOT bOk THEN RETURN NO-APPLY.
  END.
  IF rsType:SCREEN-VALUE = "2" THEN DO:
    IF edContainer:MODIFIED THEN DO:
      cFileName = fiContainer:SCREEN-VALUE. 
      cFileName = REPLACE(cFileName,"\","/").
      IF SEARCH(fiContainer:SCREEN-VALUE) NE ? THEN
        OUTPUT TO VALUE(SEARCH(fiContainer:SCREEN-VALUE)).
      ELSE
        OUTPUT TO VALUE(fiContainer:SCREEN-VALUE).
      PUT UNFORMATTED edContainer:SCREEN-VALUE.
      OUTPUT CLOSE.
  
      IF SUBSTR(fiContainer:SCREEN-VALUE,2,1) = ":" OR fiContainer:SCREEN-VALUE BEGINS "//" THEN DO:
        fiContainer:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cFileName,"/"),cFileName,"/").
        IF SEARCH(fiContainer:SCREEN-VALUE) = ? THEN DO:
          DO ix = 1 TO 4:
            fiContainer:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cFileName,"/") - ix,cFileName,"/") + "/" + fiContainer:SCREEN-VALUE.
            IF SEARCH(fiContainer:SCREEN-VALUE) NE ? THEN LEAVE.
          END.
          IF SEARCH(fiContainer:SCREEN-VALUE) = ? THEN fiContainer:SCREEN-VALUE = cFileName.
        END.        
      END.
    END.
    iocCalcCont = fiContainer:SCREEN-VALUE.
    IF cmbParamField:SCREEN-VALUE NE "ROWID" OR (NOT tbRuntimeParam:HIDDEN AND tbRuntimeParam:CHECKED) THEN
      iocParamField  = "(" + cmbParamField:SCREEN-VALUE + ")".
    ELSE iocParamField = "".
  END.

  ELSE DO:
    iocCalcCont = "".
    IF edContainer:SCREEN-VALUE NE "" THEN DO:
      iocParamField = edContainer:SCREEN-VALUE.

      IF CAN-DO("jb_min,jb_max",cmbCalcProc:SCREEN-VALUE) THEN DO:
        IF NUM-ENTRIES(iocParamField) > 1 THEN DO:
          MESSAGE "Field names must be separated by space"
                  VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
        END.
        IF NUM-ENTRIES(iocParamField," ") > 12 THEN DO:
          MESSAGE "Max 12 fields for comparison"
                  VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
        END.
      END.
      IF cmbCalcProc:SCREEN-VALUE = "jb_total" THEN
        iocParamField = REPLACE(iocParamField,CHR(10),"¤").
      ELSE
        iocParamField = REPLACE(iocParamField,CHR(10)," ").
    END.
    ELSE
      iocParamField  = cmbParamField:SCREEN-VALUE.
    iocParamField  = "(" + iocParamField + ")".
  END.

  ASSIGN iocSourceTable = cmbSourceTable:SCREEN-VALUE
         iocFieldName   = fiFieldName:SCREEN-VALUE 
         iocDataType    = cmbDataType:SCREEN-VALUE 
         iocFormat      = fiFormat:SCREEN-VALUE 
         iocLabel       = fiLabel:SCREEN-VALUE 
         iocCalcProc    = cmbCalcProc:SCREEN-VALUE 
         obOk           = YES
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCalcProc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCalcProc Dialog-Frame
ON VALUE-CHANGED OF cmbCalcProc IN FRAME Dialog-Frame /* Procedure */
DO:
  ASSIGN cFieldDataType = "*"
         cFieldAddFirst = ""
         cmbParamField:SENSITIVE  = YES
         cmbSourceTable:SENSITIVE = YES
         edContainer:SENSITIVE    = YES
         edHint:SCREEN-VALUE      = ""
         fiNewProcedure:HIDDEN    = YES
         .
  IF rsType:SCREEN-VALUE = "1" THEN DO:      
    IF CAN-DO(cBuiltInWithCode,cmbCalcProc:SCREEN-VALUE) AND FRAME {&FRAME-NAME}:WIDTH-CHARS < 100 THEN
      adjustFrame("normal").
    ELSE IF NOT CAN-DO(cBuiltInWithCode,cmbCalcProc:SCREEN-VALUE) AND FRAME {&FRAME-NAME}:WIDTH-CHARS > 100 THEN
      adjustFrame("small").

    edContainer:SCREEN-VALUE = "".
  END.
  ELSE
    edHint:SCREEN-VALUE = "A custom procedure will always return a character value. You may however specify your column in the result set"
                        + " using a different data type."
                        + CHR(10) + "For easy reuse of the calculation for different query buffers you should if possible use a field value as input"
                        + " parameter. If your key is complex or you want to pass an additional parameter at runtime (using the setCalcParam method)"
                        + " you must however specify ROWID as the parameter field. You can still reuse the procedure but that will require that you"
                        + " ask the caller (the standard query procedure) for the active query buffer handle using" 
                        + CHR(10) + "DYNAMIC-FUNCTION('getActiveQueryBuffer' IN SOURCE-PROCEDURE)." + CHR(10)
                        + CHR(10) 
                        + "If you create dynamic objects in the container you should delete them in the procedure CleanUp which is called before"
                        + " the container is deleted."
                        .

  CASE SELF:SCREEN-VALUE:
    WHEN "jb_void" THEN 
      ASSIGN edHint:SCREEN-VALUE = "Add a field in the local temp-table"
             edContainer:SENSITIVE      = NO
             cFieldAddFirst             = "N/A"
             cmbParamField:SENSITIVE    = NO
             cmbSourceTable:SENSITIVE   = NO
             .
    WHEN "jb_calc" THEN 
      ASSIGN edHint:SCREEN-VALUE = "(Simple) calculation - fields can be referenced from any buffer in the query and even be other calculated fields, f.ex:" + CHR(10) 
                                 + CHR(10) + "Qty * Item.Price"
                                 + CHR(10) + "The field would here be defined for the OrderLine buffer where Qty belongs. Hence Qty doesn't require a table qualifier." 
                                 + CHR(10) + "Valid operators are: * % / + - MOD CONCAT COND"
                                 + CHR(10) + "CONCAT will contatinate two strings with the default separator of ' '. The fields may themselves be of other data types. To use a different separator append it to the operator, f.ex like this: CONCAT/ or CONCAT /. The first will result in just the / while the other will produce ' / '."
                                 + CHR(10) + "COND checks if <field2> has value (ie NE 0, NE '', NE ? or TRUE) and if so use it rather than <field1>."
                                 + CHR(10) + "You can also check if TODAY is between date field values: <datefld1> LT TODAY GE <datefld2>."
             .
    WHEN "jb_date_dt" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(8)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(8)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "datetime" /* View only date fields */
             .
    WHEN "jb_weeknum" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(6)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(6)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "date" /* View only date fields */
             .
    WHEN "jb_weeknum" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(6)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(6)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "datetime" /* View only date fields */
             .
    WHEN "jb_month" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(6)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(6)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "date" /* View only date fields */
             .
    WHEN "jb_month_dt" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(6)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(6)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "datetime" /* View only date fields */
             .
    WHEN "jb_quarter" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(6)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(6)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "date" /* View only date fields */
             .
    WHEN "jb_quarter_dt" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(6)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(6)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "datetime" /* View only date fields */
             . 
    WHEN "jb_year" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(4)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(4)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "date" /* View only date fields */
             .
    WHEN "jb_year_dt" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value will best be viewed as a character value with format x(4)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(4)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "datetime" /* View only date fields */
             .
    WHEN "jb_hhmmss" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value should be a character value with format x(8)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(8)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "integer"
             .
    WHEN "jb_hhmm" THEN
      ASSIGN edHint:SCREEN-VALUE = "The resulting value should be a character value with format x(6)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(6)"
             edContainer:SENSITIVE = NO
             cFieldDataType = "integer"
             .
    WHEN "jb_total" THEN 
      ASSIGN edHint:SCREEN-VALUE = "Calculate totals from related (child) records or count records" + CHR(10)
                                 + CHR(10) + "You can also do simple calculations for child records"
                                 + CHR(10) + "Valid operators are: * % / + - MOD" + CHR(10)
                                 + CHR(10) + "Examples ('Order' is part of the query):" 
                                 + CHR(10) + CHR(10) + "EACH OrderLine OF Order" + CHR(10) + "Price * Qty"
                                 + CHR(10) + CHR(10) + "EACH OrderLine WHERE Orderline.OrderNum = Order.OrderNum" + CHR(10) + "COUNT"
                                 + CHR(10) + CHR(10) + "Use linefeed or ¤ as separator between query and calc field/expression" 
             cmbDataType:SCREEN-VALUE = "DECIMAL"
             cFieldAddFirst = "N/A"
             cmbParamField:SENSITIVE = NO
             fiFormat:SCREEN-VALUE = "->>,>>>,>>9.99"
             .
    WHEN "jb_can-find" THEN 
      ASSIGN edHint:SCREEN-VALUE = "Check existance of related records (CAN-FIND)"
                                 + CHR(10) + "F.ex are there any lines for 'Fins' in the order?:" + CHR(10)
                                 + CHR(10) + "FIRST OrderLine OF Order WHERE OrderLine.ItemNum = 1" + CHR(10)
                                 + CHR(10) + "To implement 'NOT CAN-FIND' much more efficient than by filtering on the resulting value of the CAN-FIND "
                                 + "you can use the NOT EXIST option in the view definition."
                                 + "F.ex to query items without sale last year:" + CHR(10)
                                 + CHR(10) + "Item"
                                 + CHR(10) + "  + ;DISTINCT Itemnum"
                                 + CHR(10) + "  + ;ItemName"
                                 + CHR(10) + ",OrderLine"
                                 + CHR(10) + ",NOT EXIST Order"
                                 + CHR(10) + ",WHERE false"
                                 + CHR(10) + ",EACH orderline NO-LOCK OF Item"
                                 + CHR(10) + ",FIRST Order NO-LOCK OF Orderline WHERE order.OrderDate > STRING(TODAY - 365)"
             cmbDataType:SCREEN-VALUE = "LOGICAL"
             cFieldAddFirst = "N/A"
             fiFormat:SCREEN-VALUE = "yes/no"
             cmbParamField:SENSITIVE = NO
             .
    WHEN "jb_max" THEN
      ASSIGN edHint:SCREEN-VALUE = "Uses the MAX function to determine max value for fields with the same datatype within the same query buffer"
                                 + CHR(10) + "Note that the field delimiter is space (not comma)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(8)"
             edContainer:SENSITIVE = YES
             cmbParamField:SENSITIVE = NO
             .
    WHEN "jb_min" THEN
      ASSIGN edHint:SCREEN-VALUE = "Uses the MAX function to determine max value for fields with the same datatype within the same query buffer"
                                 + CHR(10) + "Note that the field delimiter is space (not comma)"
             cmbDataType:SCREEN-VALUE = "CHARACTER"
             fiFormat:SCREEN-VALUE = "x(8)"
             edContainer:SENSITIVE = YES
             cmbParamField:SENSITIVE = NO
             .
    WHEN "new" THEN DO:        
      fiNewProcedure:HIDDEN = NO.
      APPLY "entry" TO fiNewProcedure.
      RETURN NO-APPLY.
    END.
    OTHERWISE DO:
      IF rsType:SCREEN-VALUE = "1" THEN
        cFieldAddFirst = "N/A".
      ELSE DO:
        cFieldAddFirst = "ROWID".
        FIND FIRST ttProcs 
             WHERE ttProcs.cProcName = cmbCalcProc:SCREEN-VALUE
             NO-ERROR.
        IF AVAIL ttProcs THEN DO:
          IF ttProcs.cParFldType NE "" THEN
            cFieldDataType = ttProcs.cParFldType.
          IF bNew OR ttProcs.bNew THEN
            ASSIGN fiFieldName:SCREEN-VALUE = ttProcs.cProcName
                   fiLabel:SCREEN-VALUE = ttProcs.cProcName
                   .
        END.
      END.
    END.
  END CASE.

  APPLY "value-changed" TO cmbSourceTable.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbDataType Dialog-Frame
ON VALUE-CHANGED OF cmbDataType IN FRAME Dialog-Frame /* Data type */
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "Logical"       THEN fiFormat:SCREEN-VALUE = "Yes/No":U.
    WHEN "Character":U   THEN fiFormat:SCREEN-VALUE = "X(8)":U.
    WHEN "Integer":U     THEN fiFormat:SCREEN-VALUE = "->,>>>,>>9":U.
    WHEN "INT64":U       THEN fiFormat:SCREEN-VALUE = "->,>>>,>>>,>>9":U.
    WHEN "Decimal":U     THEN fiFormat:SCREEN-VALUE = "->>,>>9.99":U.
    WHEN "Date":U        THEN fiFormat:SCREEN-VALUE = "99/99/99":U.
    WHEN "DateTime":U    THEN fiFormat:SCREEN-VALUE = "99/99/9999 HH:MM:SS.SSS":U.
    WHEN "DateTime-Tz":U THEN fiFormat:SCREEN-VALUE = "99/99/9999 HH:MM:SS.SSS+HH:MM":U.

      /*
    WHEN "CHARACTER" THEN 
      fiFormat:SCREEN-VALUE = "X(8)".
    WHEN "DECIMAL" THEN 
      fiFormat:SCREEN-VALUE = "->,>>>,>>9.99".
    WHEN "DATE" THEN 
      fiFormat:SCREEN-VALUE = "99/99/9999".
    WHEN "DATE-TIME" THEN
      fiFormat:SCREEN-VALUE = "99/99/9999 HH:MM".
    WHEN "DATE-TIME-TZ" THEN 
      fiFormat:SCREEN-VALUE = "99/99/9999 HH:MM".
    WHEN "INTEGER" THEN 
      fiFormat:SCREEN-VALUE = "->,>>>,>>9".
    WHEN "LOGICAL" THEN 
      fiFormat:SCREEN-VALUE = "yes/no".
      */
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbParamField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbParamField Dialog-Frame
ON VALUE-CHANGED OF cmbParamField IN FRAME Dialog-Frame /* Param.field */
DO:
  tbRuntimeParam:HIDDEN = SELF:SCREEN-VALUE NE "ROWID".   
  FIND FIRST ttFields 
       WHERE ttFields.cFieldName = SELF:SCREEN-VALUE
       NO-ERROR.
  IF AVAIL ttFields AND rsType:SCREEN-VALUE = "1" /* AND (fiFieldName:SCREEN-VALUE = "" OR bNew) */ THEN
    CASE cmbCalcProc:SCREEN-VALUE:
      WHEN "jb_void" THEN
        ASSIGN fiFieldName:SCREEN-VALUE = "MyAddnField"
               fiLabel:SCREEN-VALUE = "AddnFldLabel"
               .
      WHEN "jb_weeknum" THEN
        ASSIGN fiFieldName:SCREEN-VALUE = SELF:SCREEN-VALUE + "_week"
               fiLabel:SCREEN-VALUE = "Week(" + ttFields.cLabel + ")"
               .
      WHEN "jb_month" THEN
        ASSIGN fiFieldName:SCREEN-VALUE = SELF:SCREEN-VALUE + "_month"
               fiLabel:SCREEN-VALUE = "Month(" + ttFields.cLabel + ")"
               .
      WHEN "jb_quarter" THEN
        ASSIGN fiFieldName:SCREEN-VALUE = SELF:SCREEN-VALUE + "_q"
               fiLabel:SCREEN-VALUE = "Q(" + ttFields.cLabel + ")"
               .
      WHEN "jb_year" THEN
        ASSIGN fiFieldName:SCREEN-VALUE = SELF:SCREEN-VALUE + "_year"
               fiLabel:SCREEN-VALUE = "Year(" + ttFields.cLabel + ")"
               .
      WHEN "jb_hhmmss" OR WHEN "jb_hhmm" THEN
        ASSIGN fiFieldName:SCREEN-VALUE = SELF:SCREEN-VALUE + "_time"
               fiLabel:SCREEN-VALUE = ttFields.cLabel
               .
      WHEN "jb_calc" THEN DO:
        IF INDEX(edContainer:SCREEN-VALUE,"<field>") > 0 THEN
          edContainer:SCREEN-VALUE = REPLACE(edContainer:SCREEN-VALUE,"<field>",SELF:SCREEN-VALUE).
        ELSE IF INDEX(edContainer:SCREEN-VALUE,"<field2>") > 0 THEN
          edContainer:SCREEN-VALUE = REPLACE(edContainer:SCREEN-VALUE,"<field2>",SELF:SCREEN-VALUE).
      END.
    END CASE.

  ELSE IF rsType:SCREEN-VALUE = "2" AND CAN-DO(cNewProcList,cmbCalcProc:SCREEN-VALUE) THEN DO:
    setSignature(cmbCalcProc:SCREEN-VALUE,YES).
    scanContainer().
    APPLY "ctrl-end" TO edContainer.
    RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbSourceTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSourceTable Dialog-Frame
ON VALUE-CHANGED OF cmbSourceTable IN FRAME Dialog-Frame /* Source table */
DO:
  setParamFieldList("").
  CASE cmbCalcProc:SCREEN-VALUE:
    WHEN "jb_void" THEN
      cmbParamField:SCREEN-VALUE = "N/A".
    WHEN "jb_calc" THEN
      IF bNew OR edContainer:SCREEN-VALUE = "" THEN
        edContainer:SCREEN-VALUE = "<field> [operator <field2>]".
    WHEN "jb_total" THEN
      IF bNew OR edContainer:SCREEN-VALUE = "" THEN
        edContainer:SCREEN-VALUE = "EACH <table> OF " + cmbSourceTable:SCREEN-VALUE + " [WHERE ..]" 
                                 + CHR(10) + "<field> [operator <field2>] | COUNT".
    WHEN "jb_can-find" THEN
      IF bNew OR edContainer:SCREEN-VALUE = "" THEN
        edContainer:SCREEN-VALUE = "FIRST <table> OF " + cmbSourceTable:SCREEN-VALUE + " [WHERE ..]".
    WHEN "jb_max" THEN DO:
      cmbParamField:SCREEN-VALUE = "N/A".
      IF bNew OR edContainer:SCREEN-VALUE = "" THEN
        edContainer:SCREEN-VALUE = "<field1> <field2>[ <field3> ..]".
    END.
    OTHERWISE DO:
      IF rsType:SCREEN-VALUE = "2" THEN DO: /* custom */
        FIND FIRST ttProcs 
             WHERE ttProcs.cProcName = cmbCalcProc:SCREEN-VALUE
             NO-ERROR.
        IF AVAIL ttProcs THEN DO:
          IF ttProcs.bRunTimePar THEN
            ASSIGN tbRuntimeParam:HIDDEN = NO 
                   tbRuntimeParam:CHECKED = YES
                   cmbParamField:SCREEN-VALUE = "ROWID"
                   .
          ELSE IF ttProcs.cParamField NE "" THEN DO ix = 2 TO NUM-ENTRIES(cmbParamField:LIST-ITEM-PAIRS) BY 2:
            IF ttProcs.cParamField MATCHES "*" + ENTRY(ix,cmbParamField:LIST-ITEM-PAIRS) THEN
              cmbParamField:SCREEN-VALUE = ENTRY(ix,cmbParamField:LIST-ITEM-PAIRS).
            ELSE IF ttProcs.cParamField MATCHES "*" + ENTRY(ix,cmbParamField:LIST-ITEM-PAIRS) + "*" THEN
              cmbParamField:SCREEN-VALUE = ENTRY(ix,cmbParamField:LIST-ITEM-PAIRS).
          END.
        END.
      END.
      APPLY "value-changed" TO cmbParamField.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNewProcedure
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNewProcedure Dialog-Frame
ON LEAVE OF fiNewProcedure IN FRAME Dialog-Frame /* Proc.name */
DO:
  IF SELF:SCREEN-VALUE NE "" AND NOT SELF:HIDDEN THEN DO:
    IF NOT CAN-DO(cmbCalcProc:LIST-ITEM-PAIRS,SELF:SCREEN-VALUE) THEN DO:
      CREATE ttProcs.
      ASSIGN ttProcs.cProcName = SELF:SCREEN-VALUE
             cNewProcList = cNewProcList + (IF cNewProcList NE "" THEN "," ELSE "") + SELF:SCREEN-VALUE
             edContainer:SCREEN-VALUE = edContainer:SCREEN-VALUE + CHR(10)
                             + CHR(10) + setSignature(SELF:SCREEN-VALUE,NO) + CHR(10)
                             + CHR(10) + '  ocReturn = "". /* Always string value. Use SKIPROW to exclude row from result set */'
                             + CHR(10) + "END PROCEDURE."
             cmbCalcProc:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = scanContainer()
             .
      cmbCalcProc:SCREEN-VALUE = SELF:SCREEN-VALUE NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
        APPLY "value-changed" TO cmbCalcProc.
        APPLY "ctrl-end" TO edContainer.
        RETURN NO-APPLY.
      END.
      ELSE MESSAGE "Invalid procedure name" VIEW-AS ALERT-BOX ERROR.
    END.   
    ELSE DO:
      MESSAGE "Procedure already exists"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsType Dialog-Frame
ON VALUE-CHANGED OF rsType IN FRAME Dialog-Frame
DO:
  DEFINE VAR cCalcProcCont AS CHAR NO-UNDO.

  edContainer:SCREEN-VALUE = "".
  IF SELF:SCREEN-VALUE = "1" THEN DO: /* built in procs */
    
    ASSIGN btnContainer:HIDDEN      = YES
           fiContainer:HIDDEN       = YES
           fiNewProcedure:HIDDEN    = YES
           tbRuntimeParam:HIDDEN    = YES
           btnCheckSyntax:SENSITIVE = NO
           btnRescan:SENSITIVE      = NO
           cmbCalcProc:SENSITIVE    = YES 
           cmbCalcProc:LIST-ITEM-PAIRS = "Integer time value to HH:MM:SS,jb_hhmmss"
                                       + ",Integer time value to HH:MM,jb_hhmm"
                                       + ",Simple calculation,jb_calc"
                                       + ",Convert (extract) date from datetime field,jb_date_dt"
                                       + ",Convert (extract) week from date field,jb_weeknum"
                                       + ",Convert (extract) week from datetime field,jb_weeknum_dt"
                                       + ",Convert (extract) month from date field,jb_month"
                                       + ",Convert (extract) month from datetime field,jb_month_dt"
                                       + ",Convert (extract) quarter from date field,jb_quarter"
                                       + ",Convert (extract) quarter from datetime field,jb_quarter_dt"
                                       + ",Convert (extract) year from date field,jb_year"
                                       + ",Convert (extract) year from datetime field,jb_year_dt"
                                       + ",Calc. totals from/count rel. records not incl. in the query,jb_total"
                                       + ",Check existence of records not included in the query,jb_can-find"
                                       + ",Additional field in the local temp-table,jb_void"
                                       + ",Max,jb_max"     
                                       + ",Min,jb_min"     
/*                                       + ",Random,jb_random"     */
           edHint:SCREEN-VALUE      = "Built in procedures are embedded in the standard server query procedure (jbserv_gettemptablejoin.p)"
           .
  
    IF NOT CAN-DO(cBuiltInWithCode,cmbCalcProc:SCREEN-VALUE) OR cmbCalcProc:SCREEN-VALUE = ? THEN
      adjustFrame("small").
  END.
  ELSE DO:
    cCalcProcCont = DYNAMIC-FUNCTION("getObjectCalcProcs","").

    IF cCalcProcCont NE "" AND fiContainer:SCREEN-VALUE = "" THEN DO:
      IF NUM-ENTRIES(cCalcProcCont) = 1 AND SEARCH(cCalcProcCont) NE ? THEN DO:
        MESSAGE "The container " cCalcProcCont " is already used for another field" SKIP
                "Add this procedure to the same container?"
                VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOK.  
        IF bOk THEN 
          fiContainer:SCREEN-VALUE = cCalcProcCont.
      END.
      ELSE DO:
        JBoxSession:Instance:SimpleSelectList(cCalcProcCont,",").
        IF JBoxSession:Instance:SelectListOk THEN
          fiContainer:SCREEN-VALUE = JBoxSession:Instance:SelectListValue.
      END.

    END.
    
    adjustFrame("normal").
    ASSIGN btnContainer:HIDDEN      = NO
           fiContainer:HIDDEN       = NO    
           btnCheckSyntax:SENSITIVE = YES
           btnRescan:SENSITIVE      = YES
           edHint:SCREEN-VALUE      = "Custom procedures are normally internal to a 'container' procedure"
                                    + " (.p) that is invoked persistent from the standard query procedure."
                                    + " They could however also be added to the include file incl/custom_calcfieldprocs.i"
                                    + " - this could be helpful for commonly used expressions, f.ex 'LastName + FirstName'."
                                    + CHR(10)
                                    + "A third option is to use a single .p for a single field. This approach is not recommended"
                                    + " due to poor performance for larger result sets."
                                    + CHR(10)
                                    + "Note that you exclude the row from the result set by returning 'SKIPROW'."
                                    + CHR(10)
                                    + "More than one container procedure may be used for the same query."
           .
    importContainer(fiContainer:SCREEN-VALUE).
    cmbCalcProc:SENSITIVE = fiContainer:SCREEN-VALUE NE "".
        
  END.
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbRuntimeParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbRuntimeParam Dialog-Frame
ON VALUE-CHANGED OF tbRuntimeParam IN FRAME Dialog-Frame /* Enable runtime parameter */
DO:
  setSignature(cmbCalcProc:SCREEN-VALUE,YES).
  scanContainer().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY rsType fiContainer edContainer cmbCalcProc fiNewProcedure 
          cmbSourceTable cmbParamField tbRuntimeParam fiFieldName cmbDataType 
          fiFormat fiLabel edHint 
      WITH FRAME Dialog-Frame.
  ENABLE rsType btnContainer fiContainer cmbCalcProc fiNewProcedure 
         cmbSourceTable cmbParamField tbRuntimeParam fiFieldName cmbDataType 
         fiFormat fiLabel edHint btnRescan Btn_Help Btn_Cancel Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTable             AS CHAR NO-UNDO.
DEF VAR cDefaultFrameFont  AS CHAR   NO-UNDO.

cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont").
IF cDefaultFrameFont NE "" THEN
  ASSIGN {&WINDOW-NAME}:FONT = INTEGER(cDefaultFrameFont)
         FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fiContainer:SCREEN-VALUE    = iocCalcCont
         fiFieldName:SCREEN-VALUE    = iocFieldName 
         fiFormat:SCREEN-VALUE       = iocFormat
         fiLabel:SCREEN-VALUE        = iocLabel
         bNew                        = iocFieldName = ""
         .
  DO ix = 1 TO NUM-ENTRIES(icSourceTables):
    cTable = ENTRY(ix,icSourceTables).
    cTable = ENTRY(NUM-ENTRIES(cTable,"."),cTable,".").
    IF LOOKUP(cTable,cmbSourceTable:LIST-ITEMS) = ? OR LOOKUP(cTable,cmbSourceTable:LIST-ITEMS) = 0 THEN 
      cmbSourceTable:ADD-LAST(cTable).
  END.
  IF iocDataType NE "" THEN 
    cmbDataType:SCREEN-VALUE = iocDataType.
  ELSE 
    cmbDataType:SCREEN-VALUE = cmbDataType:ENTRY(1).
  APPLY "value-changed" TO cmbDataType.

  fiContainer = iocCalcCont.
  IF fiContainer NE "" THEN
    rsType:SCREEN-VALUE = "2".

  IF iocSourceTable NE "" THEN DO:
    cmbSourceTable:SCREEN-VALUE = iocSourceTable NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      cmbSourceTable:SCREEN-VALUE = cmbSourceTable:ENTRY(1) NO-ERROR.
  END. 
  ELSE cmbSourceTable:SCREEN-VALUE = cmbSourceTable:ENTRY(1) NO-ERROR.

  APPLY "value-changed" TO rsType.
  IF iocCalcProc NE "" THEN 
    cmbCalcProc:SCREEN-VALUE = iocCalcProc NO-ERROR.

  APPLY "value-changed" TO cmbCalcProc. 

  IF iocParamField NE "" THEN DO:
    IF CAN-DO(cBuiltInWithCode,iocCalcProc) THEN
      edContainer:SCREEN-VALUE = iocParamField.
    ELSE
      cmbParamField:SCREEN-VALUE = iocParamField NO-ERROR.
  END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION adjustFrame Dialog-Frame 
FUNCTION adjustFrame RETURNS LOGICAL
  ( INPUT icSize AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iCurrX AS INT NO-UNDO.
DEF VAR iCurrY AS INT NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iCurrX = FRAME {&FRAME-NAME}:X
         iCurrY = FRAME {&FRAME-NAME}:Y
         .

  IF icSize = "small" THEN
    ASSIGN btnCheckSyntax:HIDDEN = YES
           btnRescan:HIDDEN = YES
           edContainer:HIDDEN = YES
           edHint:HEIGHT-PIXELS = 50
           btn_Help:X   = 114
           btn_Cancel:X = 192
           btn_Ok:X     = 270
           btn_Help:Y   = 310
           btn_Cancel:Y = 310
           btn_Ok:Y     = 310
           FRAME {&FRAME-NAME}:WIDTH-PIXELS = 355
           FRAME {&FRAME-NAME}:HEIGHT-PIXELS = 370
           .
  ELSE
    ASSIGN FRAME {&FRAME-NAME}:WIDTH-PIXELS = 855
           FRAME {&FRAME-NAME}:HEIGHT-PIXELS = 560
           edHint:HEIGHT-PIXELS = 234
           btnCheckSyntax:HIDDEN = NO
           btnRescan:HIDDEN = NO
           edContainer:HIDDEN = NO
           btn_Help:X   = 614
           btn_Cancel:X = 692
           btn_Ok:X     = 770
           btn_Help:Y   = 495
           btn_Cancel:Y = 495
           btn_Ok:Y     = 495
           btnCheckSyntax:Y = 495
           btnRescan:Y      = 495
           .

  ASSIGN FRAME {&FRAME-NAME}:X = iCurrX
         FRAME {&FRAME-NAME}:Y = iCurrY.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckSyntax Dialog-Frame 
FUNCTION CheckSyntax RETURNS LOGICAL
  ( INPUT pCheck AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF rsType:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2" THEN DO WITH FRAME {&frame-name}:
  DEF VAR TempString         AS CHAR    NO-UNDO.
  DEF VAR StreamLength       AS INT     NO-UNDO.
  DEF VAR StreamDiff         AS INT     NO-UNDO. 
  DEF VAR ThisTbl            AS CHAR    NO-UNDO.
  DEF VAR ThisDB             AS CHAR    NO-UNDO.

  RUN adecomm/_setcurs.p ("WAIT":U).
  IF _comp_temp_file = ? THEN
    RUN adecomm/_tmpfile.p ({&STD_TYP_UIB_COMPILE}, {&STD_EXT_UIB},OUTPUT _comp_temp_file).
  OUTPUT STREAM P_4GL TO VALUE(_comp_temp_file) NO-ECHO.
  ASSIGN SESSION:NUMERIC-FORMAT = "AMERICAN":U.
  
/*   RUN adeuib/_writedf.p. Obs - nasty bugger: Changes _Q._TblList */

  TempString = edContainer:SCREEN-VALUE.

  PUT STREAM P_4GL UNFORMATTED TempString.
  StreamLength = SEEK (P_4GL).
  OUTPUT STREAM P_4GL CLOSE. 

  /*  Need to set session numeric format back to user's setting after setting 
      it to American above  */
  SESSION:SET-NUMERIC-FORMAT(_numeric_separator,_numeric_decimal).
  
  COMPILE VALUE(_comp_temp_file) NO-ERROR.
  IF NOT COMPILER:ERROR THEN DO:
    /*
    ** Only report a valid syntax message if pCheck is True 
    */
    IF pCheck THEN
    do: 
       &if DEFINED(IDE-IS-RUNNING) <> 0 &then
       ShowMessageInIDE("Syntax is correct.",
                        "Information",?,"OK",yes).
       &else 
       MESSAGE "Syntax is correct." VIEW-AS ALERT-BOX INFORMATION.
       &endif
    end.   
    bOK = YES.
  END.
  ELSE DO:
    StreamDiff = StreamLength - INTEGER(COMPILER:FILE-OFFSET).
    &if DEFINED(IDE-IS-RUNNING) <> 0 &then
       ShowMessageInIDE(string(error-status:get-message(1)) + " in column " + string(StreamDiff) + ".",
                        "ERROR",?,"OK",yes).
    &else
    MESSAGE error-status:get-message(1) SKIP "in column" StreamDiff "." VIEW-AS ALERT-BOX ERROR.
    &endif
    bOK = NO.
  END.

  OS-DELETE VALUE(_comp_temp_file).  
  RUN adecomm/_setcurs.p ("":U).
END.
ELSE RETURN YES.

RETURN bOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION importContainer Dialog-Frame 
FUNCTION importContainer RETURNS LOGICAL
  ( INPUT icFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileCont AS CHAR NO-UNDO.
DEF VAR cLine     AS CHAR NO-UNDO FORMAT "X(255)".
DEF VAR cFileLC   AS LONGCHAR NO-UNDO.

IF icFileName NE "" AND SEARCH(icFileName) = ? THEN DO:
  MESSAGE "Invalid file name for container." SKIP "Is this a new file?"
           VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE bOk.
  IF bOk THEN initContainer().
END.

ELSE IF SEARCH(icFileName) NE ? THEN DO WITH FRAME {&FRAME-NAME}:
  COPY-LOB FROM FILE SEARCH(icFileName) TO cFileLC.
  DO ix = 1 TO NUM-ENTRIES(cFileLC,"~n"):
    cFileCont = cFileCont + ENTRY(ix,cFileLC,"~n") + CHR(10).
  END.
  DO ix = LENGTH(cFileCont) TO LENGTH(cFileCont) - 10:
    IF SUBSTR(cFileCont,ix,1) NE CHR(10) THEN LEAVE.
  END.
  IF ix < LENGTH(cFileCont) - 2 THEN
    cFileCont = SUBSTR(cFileCont,1,ix + 1).

  edContainer:SCREEN-VALUE = cFileCont.      

END.

cmbCalcProc:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = scanContainer().


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initContainer Dialog-Frame 
FUNCTION initContainer RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  edContainer:SCREEN-VALUE = "/********************************************************************************"
                           + CHR(10) + "Created...." + STRING(TODAY,"99/99/9999") + " by " + OS-GETENV("username")
                           + CHR(10) + "Purpose...."
                           + CHR(10) + "Used by....  <list of_ client procedures>"
                           + CHR(10) + "Modified..." 
                           + CHR(10) + "********************************************************************************/" + CHR(10)
                           + CHR(10) + "/* DYNAMIC-FUNCTION('startASlib' IN SOURCE-PROCEDURE). /* Optionally enable appserver library functions on server */ */" + CHR(10)
                           + CHR(10) + "PROCEDURE CleanUp:"
                           + CHR(10) + "  /* Delete any dynamic objects here (avoid creating them inside the internal procedure(s)) */"
                           + CHR(10) + "END PROCEDURE." + CHR(10)
                           .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION scanContainer Dialog-Frame 
FUNCTION scanContainer RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO INIT "<new>,new".

DEF VAR cFileCont    AS CHAR NO-UNDO.
DEF VAR cOrgLine     AS CHAR NO-UNDO.
DEF VAR cLine        AS CHAR NO-UNDO.
DEF VAR cProc        AS CHAR NO-UNDO.
DEF VAR bReadSign    AS LOG  NO-UNDO.
DEF VAR iSignLineCnt AS INT  NO-UNDO.

EMPTY TEMP-TABLE ttProcs.

DO WITH FRAME {&FRAME-NAME}:
  cFileCont = edContainer:SCREEN-VALUE.
  DO ix = 1 TO NUM-ENTRIES(cFileCont,CHR(10)):
    ASSIGN cOrgLine = ENTRY(ix,cFileCont,CHR(10))
           cLine = TRIM(ENTRY(ix,cFileCont,CHR(10))).

    REPEAT WHILE INDEX(cLine,"  ") > 0:
      cLine = REPLACE(cLine,"  "," ").
    END.

    IF bReadSign AND AVAIL ttProcs AND cLine BEGINS "DEF" AND cLine MATCHES "* PARAM*" THEN DO:
      ASSIGN ttProcs.cSignature = ttProcs.cSignature + CHR(10) + cOrgLine
             iSignLineCnt = iSignLineCnt + 1.
      IF cLine MATCHES "* AS ROWID *" THEN
        ttProcs.cParamField = "ROWID".
      ELSE IF iSignLineCnt = 2 THEN DO:
        ASSIGN ttProcs.cParamField = ENTRY(4,cLine," ")
               ttProcs.cParFldType = ENTRY(6,cLine," ").
        IF ttProcs.cParFldType BEGINS "int" THEN ttProcs.cParFldType = "INTEGER".
        ELSE IF ttProcs.cParFldType BEGINS "dec" THEN ttProcs.cParFldType = "DECIMAL".
        ELSE IF ttProcs.cParFldType BEGINS "log" THEN ttProcs.cParFldType = "LOGICAL".
        ELSE IF ttProcs.cParFldType BEGINS "char" THEN ttProcs.cParFldType = "CHARACTER".
      END.

      IF iSignLineCnt = 5 THEN
        ttProcs.bRunTimePar = YES.
    END.
    ELSE IF bReadSign AND (cLine BEGINS "END." OR cLine BEGINS "END PROCEDURE.") THEN
      bReadSign = NO.

    IF cLine BEGINS "procedure" AND NOT cLine MATCHES "* CleanUp:*" THEN DO:
      ASSIGN cProc = TRIM(ENTRY(2,cLine," "),":")
             cReturn = cReturn + "," + cProc + "," + cProc
             .
      CREATE ttProcs.
      ASSIGN ttProcs.cProcName  = cProc
             ttProcs.cSignature = cOrgLine
             ttProcs.bNew       = CAN-DO(cNewProcList,cProc)
             bReadSign = YES
             iSignLineCnt = 1
             .
    END.
  END.
END.

RETURN cReturn. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParamFieldList Dialog-Frame 
FUNCTION setParamFieldList RETURNS LOGICAL
  ( INPUT icScreenValue AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR Stat        AS LOG  NO-UNDO.
DEF VAR cLabel      AS CHAR NO-UNDO.
DEF VAR cLabelAttr  AS CHAR NO-UNDO.
DEF VAR cFormat     AS CHAR NO-UNDO.
DEF VAR cFormatAttr AS CHAR NO-UNDO.
DEF VAR cDataType   AS CHAR NO-UNDO.
DEF VAR cHelp       AS CHAR NO-UNDO.
DEF VAR cHelpAttr   AS CHAR NO-UNDO.
DEF VAR cDesc       AS CHAR NO-UNDO.
DEF VAR cValExp     AS CHAR NO-UNDO.
DEF VAR extnt       AS INT  NO-UNDO.
DEF VAR intl        AS CHAR NO-UNDO.
DEF VAR valmsg      AS CHAR NO-UNDO.
DEF VAR valmsg-sa   AS CHAR NO-UNDO.
DEF VAR bMandatory  AS LOG  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  
  ASSIGN cmbDummy:LIST-ITEMS = "" 
         cmbParamField:LIST-ITEM-PAIRS = ",".

  EMPTY TEMP-TABLE ttFields.

  RUN adecomm/_mfldlst.p (cmbDummy:HANDLE,
                          cmbSourceTable:SCREEN-VALUE,
                          "",
                          yes,
                          "",
                          1,
                          "",
                          OUTPUT Stat).
  IF Stat THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cmbDummy:LIST-ITEMS):
      RUN adeuib/_fldinfo.p (?, /* INPUT LDBNAME("dictdb"), */
                             INPUT SELF:SCREEN-VALUE,
                             INPUT cmbDummy:ENTRY(ix),
                             OUTPUT cLabel,
                             OUTPUT cLabelAttr,
                             OUTPUT cFormat,
                             OUTPUT cFormatAttr,
                             OUTPUT cDataType,
                             OUTPUT cHelp,
                             OUTPUT cHelpAttr,
                             OUTPUT extnt,
                             OUTPUT intl,
                             OUTPUT cDesc,
                             OUTPUT cValExp,
                             OUTPUT valmsg,
                             OUTPUT valmsg-sa, 
                             OUTPUT bMandatory).
      IF CAN-DO(cFieldDataType,cDataType) THEN DO:
        cmbParamField:ADD-LAST(cmbDummy:ENTRY(ix) + " (" + cDataType + ")",cmbDummy:ENTRY(ix)).
        CREATE ttFields.
        ASSIGN ttFields.cFieldName = cmbDummy:ENTRY(ix)
               ttFields.cDataType  = cDataType
               ttFields.cLabel     = cLabel
               .
      END.
    END.
    
    IF NUM-ENTRIES(cmbParamField:LIST-ITEM-PAIRS) > 2 THEN
      cmbParamField:DELETE(1).

    IF cFieldAddFirst NE "" THEN
      cmbParamField:ADD-FIRST(cFieldAddFirst,cFieldAddFirst).

    IF icScreenValue NE "" THEN
      cmbParamField:SCREEN-VALUE = icScreenValue NO-ERROR.
    ELSE
      cmbParamField:SCREEN-VALUE = cmbParamField:ENTRY(1).
  END.

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSignature Dialog-Frame 
FUNCTION setSignature RETURNS CHARACTER
  ( INPUT icProcName AS CHAR,
    INPUT ibReplace  AS LOG  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLine1 AS CHAR NO-UNDO.
DEF VAR cLine2 AS CHAR NO-UNDO.
DEF VAR cLine3 AS CHAR NO-UNDO.
DEF VAR cLine4 AS CHAR NO-UNDO.
DEF VAR cLine5 AS CHAR NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

FIND FIRST ttProcs 
     WHERE ttProcs.cProcName = icProcName
     NO-ERROR.
IF AVAIL ttProcs THEN DO WITH FRAME {&FRAME-NAME}:
  cLine1 = "PROCEDURE " + icProcName + ":".
  IF cmbParamField:SCREEN-VALUE = "ROWID" OR NOT ibReplace THEN DO:
    cLine2 = CHR(10) + "  DEF INPUT  PARAM ir" + cmbSourceTable:SCREEN-VALUE + " AS ROWID NO-UNDO.".
    IF NOT tbRuntimeParam:HIDDEN AND tbRuntimeParam:CHECKED THEN
      cLine3 = CHR(10) + "  DEF INPUT  PARAM icParam AS CHAR NO-UNDO.".
  END.
  ELSE DO:
    FIND FIRST ttFields 
         WHERE ttFields.cFieldName = cmbParamField:SCREEN-VALUE
         NO-ERROR.
    cLine2 = CHR(10) + "  DEF INPUT  PARAM ip" + cmbParamField:SCREEN-VALUE + " AS "
                     + (IF AVAIL ttFields THEN ttFields.cDataType ELSE "") + " NO-UNDO.".
  END.
  ASSIGN cLine4 = CHR(10) + "  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO."
         cLine5 = CHR(10) + "  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO."
         .
  cReturn = cLine1 + cLine2 + cLine3 + cLine4 + cLine5.

  IF ibReplace THEN DO:
    IF INDEX(edContainer:SCREEN-VALUE,ttProcs.cSignature) = 0 THEN
      MESSAGE ttProcs.cSignature
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    edContainer:SCREEN-VALUE = REPLACE(edContainer:SCREEN-VALUE,ttProcs.cSignature,cReturn).
  END.
END.
ELSE DO:
  MESSAGE icProcName " not avail"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

