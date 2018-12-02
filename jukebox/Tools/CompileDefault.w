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

DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR cFileTypes        AS CHAR   NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwFiles rectTBFiles DefaultName ~
FileTypes SaveRcode cmbDefaults CompileSubDir 
&Scoped-Define DISPLAYED-OBJECTS DefaultName FileTypes SaveRcode ~
cmbDefaults CompileSubDir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteSetting C-Win 
FUNCTION DeleteSetting RETURNS LOGICAL
  ( INPUT icKey AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadDefaultValues C-Win 
FUNCTION LoadDefaultValues RETURNS LOGICAL
  ( INPUT icKey AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadFiles C-Win 
FUNCTION LoadFiles RETURNS CHARACTER
  ( INPUT icFileNames AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveFiles C-Win 
FUNCTION SaveFiles RETURNS LOGICAL
  ( INPUT icKey AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbDefaults AS CHARACTER FORMAT "X(256)":U 
     LABEL "Defaults" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30.57 BY 1 NO-UNDO.

DEFINE VARIABLE DefaultName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FileTypes AS CHARACTER FORMAT "X(256)":U INITIAL "*.p *.w" 
     LABEL "File types" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrwFiles
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59.14 BY 6.81.

DEFINE RECTANGLE rectTBFiles
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY .96.

DEFINE VARIABLE CompileSubDir AS LOGICAL INITIAL yes 
     LABEL "Compile sub-folders" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE SaveRcode AS LOGICAL INITIAL yes 
     LABEL "Save r-code" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.43 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     DefaultName AT ROW 2.88 COL 11 COLON-ALIGNED
     FileTypes AT ROW 3.96 COL 11 COLON-ALIGNED
     SaveRcode AT ROW 5.04 COL 13
     cmbDefaults AT ROW 1.38 COL 28 COLON-ALIGNED
     CompileSubDir AT ROW 5.04 COL 37
     rectBrwFiles AT ROW 6.12 COL 2
     rectTBFiles AT ROW 1.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 61.14 BY 12.15.


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
         TITLE              = "Build compiler defaults"
         HEIGHT             = 12.12
         WIDTH              = 61.14
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Build compiler defaults */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Build compiler defaults */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Build compiler defaults */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbDefaults C-Win
ON VALUE-CHANGED OF cmbDefaults IN FRAME DEFAULT-FRAME /* Defaults */
DO:
  LoadDefaultValues(cmbDefaults:SCREEN-VALUE).
  DYNAMIC-FUNCTION("setAttribute",hQuery,"basequery",
                   "WHERE " + (IF cmbDefaults:SCREEN-VALUE = ? THEN "false" ELSE "DefKey = '" + cmbDefaults:SCREEN-VALUE + "'")).
  DYNAMIC-FUNCTION("setCurrentObject",hQuery).
  RUN OpenQuery.
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
  PUBLISH "InvalidateHandle".
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.  
  RUN InitWindow.
  SUBSCRIBE PROCEDURE hParent TO "InvalidateHandle" IN THIS-PROCEDURE.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON 'drop-file-notify' OF hBrowse DO:
  DEF VAR cFileNames AS CHAR   NO-UNDO.

  DO ix = 1 TO SELF:NUM-DROPPED-FILES:
    cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".
  END.
  IF cFileNames NE "" THEN 
    LoadFiles(TRIM(cFileNames,";")).
END.


ON 'window-resized' OF {&WINDOW-NAME} DO:
  DEF VAR hColumn AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  hColumn = hBrowse:GET-BROWSE-COLUMN(2).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeNavBrowseFillIn C-Win 
PROCEDURE BeforeNavBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

IF ihFillIn:MODIFIED THEN
  PUT-KEY-VALUE SECTION "JBoxCompiler" 
                KEY cmbDefaults:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "_" + STRING(hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE) 
                VALUE ihBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE + "|" + ihBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE
                .
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN
  PUT-KEY-VALUE SECTION "JBoxCompiler" 
                KEY cmbDefaults:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "_" + STRING(hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE) 
                VALUE ?
                .
RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar THEN
  DeleteSetting(cmbDefaults:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectRowRecord C-Win 
PROCEDURE DeselectRowRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hBrowse:DESELECT-ROWS().
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
  DISPLAY DefaultName FileTypes SaveRcode cmbDefaults CompileSubDir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrwFiles rectTBFiles DefaultName FileTypes SaveRcode cmbDefaults 
         CompileSubDir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DEF VAR hSaveInto     AS HANDLE NO-UNDO.
DEF VAR hSearchField  AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbDefaults:DELIMITER = "|".

  cmbDefaults:LIST-ITEM-PAIRS = RIGHT-TRIM("||" + DYNAMIC-FUNCTION("LoadDefaults" IN hParent),"|") NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
    cmbDefaults:LIST-ITEM-PAIRS = "|".

  hQuery = DYNAMIC-FUNCTION("NewQuery",
                                 100,
                                 "",
                                 "temp-table"
                               + ";DefaultName|CHARACTER|x(256)"
                               + ";FileTypes|CHARACTER|x(256)"
                               + ";SaveRcode|LOGICAL|yes/no"
                               + ";CompileSubDir|LOGICAL"
                               + ";DefKey|CHARACTER|x(256)"
                                ,"WHERE false"
                                ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                               hQuery,
                               FRAME {&FRAME-NAME}:HANDLE,
                               "DefaultName,FileTypes,SaveRcode,CompileSubDir","",
                               "","",
                               "").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                                rectTBFiles:HANDLE,
                                "File",
                                "New,Delete,Save"
                              + ",rule¤menu"
                              + ",Close;Exit¤menu enable"
                                ,"maxborder").
  DYNAMIC-FUNCTION("createObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,hToolbar).


  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                                 rectBrwFiles:HANDLE,
                                 100,
                                 "multiple",
                                 "temp-table"
                               + ";!Seq|INTEGER|>>>>>>9||Seq"
                               + ";FileName|CHARACTER|x(256)||File"
                               + ";!FileType|CHARACTER|x(10)||Type"
                               + ";SaveInto|CHARACTER|x(256)||Save into"
                               + ";!DefKey|CHARACTER|x(256)"
                                ,"WHERE false"
                                ,"SORT|Seq").
  hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 200.
  hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 200.
  hBrowse:TOOLTIP = "Drag directories here".

  DYNAMIC-FUNCTION("CreateParentLink",hBrowse,hQuery,"DefKey").

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hBrowse:DROP-TARGET = TRUE.
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"copytoolbartobrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"nodeletewarning","yes").

  hSaveInto = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "SaveInto",     
                    "SaveInto",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSaveInto,"SaveInto").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse,
                   "Delete"
                  ,"").

  DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"cmbDefaults").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectTBFiles").

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,330,200,0,0).

  APPLY "value-changed" TO cmbDefaults.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
FileTypes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "*.p *.w".
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
DEF VAR bNew       AS LOG NO-UNDO.
DEF VAR iDefNr     AS INT  NO-UNDO INIT 1.
DEF VAR cDefKey    AS CHAR NO-UNDO.
DEF VAR bDefExists AS LOG  NO-UNDO.

bNew = DYNAMIC-FUNCTION("getToolbarState",hToolbar) = "new".

RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:

  IF NOT bNew THEN 
    DeleteSetting(cmbDefaults:SCREEN-VALUE).
  ELSE 
    hBuffer:EMPTY-TEMP-TABLE().

  IF LOOKUP(DefaultName:SCREEN-VALUE,cmbDefaults:LIST-ITEM-PAIRS,"|") > 0 THEN
    iDefNr = LOOKUP(DefaultName:SCREEN-VALUE,cmbDefaults:LIST-ITEM-PAIRS,"|") / 2.
  ELSE iDefNr = NUM-ENTRIES(cmbDefaults:LIST-ITEM-PAIRS,"|") / 2 + 1.
  
  cDefKey = "Default" + STRING(iDefNr).
  
  IF hFieldMap:AVAIL THEN
    hFieldMap:BUFFER-FIELD("DefKey"):BUFFER-VALUE = cDefKey.
  
  IF NOT bDefExists THEN DO:
    cmbDefaults:ADD-LAST(DefaultName:SCREEN-VALUE,cDefKey).
    cmbDefaults:SCREEN-VALUE = cDefKey.
  END.
  
  PUT-KEY-VALUE SECTION "JBoxCompiler" 
                KEY  cDefKey
                VALUE DefaultName:SCREEN-VALUE + "|" 
                    + FileTypes:SCREEN-VALUE + "|"
                    + SaveRcode:SCREEN-VALUE + "|"
                    + CompileSubDir:SCREEN-VALUE
                .
  
  IF bNew THEN 
    APPLY "value-changed" TO cmbDefaults.
  ELSE DO:  
    ix = 0.
    hBrowse:QUERY:GET-FIRST().
    REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
      ix = ix + 1.
      PUT-KEY-VALUE SECTION "JBoxCompiler" 
                    KEY cDefKey + "_" + STRING(ix)
                    VALUE hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE + "|" + hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE.
      hBrowse:QUERY:GET-NEXT().
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteSetting C-Win 
FUNCTION DeleteSetting RETURNS LOGICAL
  ( INPUT icKey AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
PUT-KEY-VALUE SECTION "JBoxCompiler" KEY icKey VALUE ?.

DO ix = 1 TO 50:
  PUT-KEY-VALUE SECTION "JBoxCompiler" KEY icKey + "_" + STRING(ix) VALUE ?.
END.

cmbDefaults:DELETE(cmbDefaults:SCREEN-VALUE) IN FRAME {&FRAME-NAME}.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadDefaultValues C-Win 
FUNCTION LoadDefaultValues RETURNS LOGICAL
  ( INPUT icKey AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cValue AS CHAR NO-UNDO.
DEF VAR cKey   AS CHAR NO-UNDO.
DEF VAR iSeq   AS INT  NO-UNDO.
  
hBuffer:EMPTY-TEMP-TABLE().

IF icKey = "" OR icKey = ? THEN 
  RETURN NO.


bOk = hFieldMap:FIND-FIRST("WHERE DefKey = '" + icKey + "'") NO-ERROR.

IF NOT bOk THEN DO:
  hFieldMap:BUFFER-CREATE().

  GET-KEY-VALUE SECTION "JBoxCompiler" KEY icKey VALUE cValue. 

  ASSIGN hFieldMap:BUFFER-FIELD("DefaultName"):BUFFER-VALUE = ENTRY(1,cValue,"|")
         hFieldMap:BUFFER-FIELD("DefKey"):BUFFER-VALUE = icKey
         .

  DO ix = 2 TO NUM-ENTRIES(cValue,"|") WITH FRAME {&FRAME-NAME}:
    IF ix = 2 THEN
      hFieldMap:BUFFER-FIELD("FileTypes"):BUFFER-VALUE = ENTRY(ix,cValue,"|").
    ELSE IF ix = 3 THEN
      hFieldMap:BUFFER-FIELD("SaveRcode"):BUFFER-VALUE = LOGICAL(ENTRY(ix,cValue,"|")).
    ELSE IF ix = 4 THEN
      hFieldMap:BUFFER-FIELD("CompileSubDir"):BUFFER-VALUE = LOGICAL(ENTRY(ix,cValue,"|")).
  END.
END.
  
DO ix = 1 TO 50:
  cKey = icKey + "_" + STRING(ix).
  GET-KEY-VALUE SECTION "JBoxCompiler" KEY cKey VALUE cValue. 
  IF cValue NE ? THEN DO:
    hBuffer:BUFFER-CREATE().
    ASSIGN iSeq = iSeq + 1
           hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
           hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = ENTRY(1,cValue,"|")
           hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE = ENTRY(2,cValue,"|")
           hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = "DRW"
           hBuffer:BUFFER-FIELD("DefKey"):BUFFER-VALUE   = icKey
           .
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadFiles C-Win 
FUNCTION LoadFiles RETURNS CHARACTER
  ( INPUT icFileNames AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iSeq      AS INT  NO-UNDO.
DEF VAR bMatch    AS LOG  NO-UNDO.
DEF VAR iy        AS INT  NO-UNDO.
DEF VAR cCompList AS CHAR NO-UNDO.

IF cmbDefaults:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" OR cmbDefaults:SCREEN-VALUE = ? THEN DO:
  MESSAGE "Select a default (or create a new) before loading directories" 
          VIEW-AS ALERT-BOX.
  RETURN "".
END.

hBuffer:FIND-LAST() NO-ERROR.
IF hBuffer:AVAIL THEN
  iSeq = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE.
DO ix = 1 TO NUM-ENTRIES(icFileNames,";"):
  FILE-INFO:FILE-NAME = ENTRY(ix,icFileNames,";").
 
  IF FILE-INFO:FILE-TYPE BEGINS "DR" THEN DO:
    bOk = hBuffer:FIND-FIRST("WHERE FileName = '" + FILE-INFO:FULL-PATHNAME + "'") NO-ERROR.
    IF NOT bOK THEN DO:
      hBuffer:BUFFER-CREATE().
      ASSIGN iSeq = iSeq + 1
             hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = iSeq
             hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE = FILE-INFO:FULL-PATHNAME
             hBuffer:BUFFER-FIELD("FileType"):BUFFER-VALUE = FILE-INFO:FILE-TYPE
             hBuffer:BUFFER-FIELD("DefKey"):BUFFER-VALUE = cmbDefaults:SCREEN-VALUE
             .
    END.
    cCompList = cCompList + STRING(hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE) + ",".
  END.
END.

DYNAMIC-FUNCTION("setCurrentObject",hQuery).
RUN OpenQuery.

SaveFiles(cmbDefaults:SCREEN-VALUE).

RETURN cCompList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveFiles C-Win 
FUNCTION SaveFiles RETURNS LOGICAL
  ( INPUT icKey AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT hBrowse:QUERY:IS-OPEN THEN RETURN NO.

DO ix = 1 TO 50:
  PUT-KEY-VALUE SECTION "JBoxCompiler" KEY icKey + "_" + STRING(ix) VALUE ?.
END.

hBrowse:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
  PUT-KEY-VALUE SECTION "JBoxCompiler" 
                KEY icKey + "_" + STRING(hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE) 
                VALUE hBuffer:BUFFER-FIELD("FileName"):BUFFER-VALUE + "|" + hBuffer:BUFFER-FIELD("SaveInto"):BUFFER-VALUE
                .
  hBrowse:QUERY:GET-NEXT().
END.


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

