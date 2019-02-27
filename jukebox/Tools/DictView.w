&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File.............: vis-felt.w
  Description......: Display databasetabeller/felter, med muligighet for
                     seleksjon av felter til clipboar for bruk i editorer
  Input Parameters : <none>
  Output Parameters: <none>
  Author...........: Arvid Soll›s
  Created..........: mai 1996 
  Modified.........: Brynjar Hasle, CreditView as, des -98:
                     - Added table browse
                     Brynjar Hasle, Chemistry as, sep -00:
                     - Added multiple DB
                     Brynjar Hasle, Chemistry as, may -02:
                     - Added resize and sorting
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

/*DEF VAR w-file AS CHARACTER                   NO-UNDO.*/

DEF VAR ix           AS INT NO-UNDO.
DEF VAR bOK          AS LOG NO-UNDO.
DEF VAR cDbTableName AS CHAR NO-UNDO.
DEF VAR iFormatLgth  AS INT NO-UNDO.
DEF VAR iMaxLgChar   AS INT INIT 70 NO-UNDO.
DEF VAR bEditMode    AS LOG NO-UNDO.

DEF VAR hResizeLib   AS HANDLE NO-UNDO.
DEF VAR bResizeLibLoaded AS LOG NO-UNDO.

DEF VAR cSortColumn1 AS CHAR NO-UNDO.
DEF VAR cSortColumn2 AS CHAR NO-UNDO.
DEF VAR bDesc1       AS LOG  NO-UNDO.
DEF VAR bDesc2       AS LOG  NO-UNDO.

DEF VAR cEnable      AS CHAR NO-UNDO.
DEF VAR cSeparator   AS CHAR NO-UNDO.
DEF VAR bWithoutLabels AS LOG NO-UNDO.

DEFINE TEMP-TABLE idx                             NO-UNDO
       FIELD Tbl-navn   AS CHAR
       FIELD Ix-navn    AS CHAR
       FIELD Ix-felt    AS CHAR 
       FIELD ix-sq      AS INTEGER
       FIELD ix-asc     AS CHAR
       FIELD ix-kode    AS CHAR
       FIELD cData-type AS CHAR
       FIELD bSearchOn  AS LOG FORMAT "Y/N".

DEF VAR hbIdx AS HANDLE NO-UNDO.
hbIdx = BUFFER idx:HANDLE.

DEF NEW SHARED TEMP-TABLE tt_file LIKE _file
    FIELD cLDBname   AS CHAR
    FIELD iFileNum   AS INT
    INDEX FileNum IS UNIQUE iFileNum.
    
DEF NEW SHARED TEMP-TABLE tt_field LIKE _field
    FIELD iFileNum AS INT
    FIELD iFieldNum AS INT
    INDEX FileNum iFileNum
    INDEX FieldNum IS UNIQUE iFieldNum.
DEF NEW SHARED TEMP-TABLE tt_index LIKE _index
    FIELD iFileNum AS INT
    FIELD iIdxNum  AS INT
    FIELD cCode    AS CHAR
    INDEX FileNum iFileNum
    INDEX IdxNum IS UNIQUE iIdxNum.
DEF NEW SHARED TEMP-TABLE tt_index-field LIKE _index-field
    FIELD iFileNum   AS INT
    FIELD iIdxNum    AS INT
    FIELD iFieldNum  AS INT
    FIELD cFieldName AS CHAR
    INDEX FileNum iFileNum
    INDEX FieldNum iFieldNum
    INDEX IdxNum iIdxNum.

DEF BUFFER btt_field  FOR tt_field.
DEF BUFFER bbtt_field FOR tt_field.

DEF TEMP-TABLE ttSearch
    FIELD iFileNum   AS INT.
    
DEF VAR hbttSearch AS HANDLE NO-UNDO.
hbttSearch = BUFFER ttSearch:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSearch tt_file tt_field idx

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt_file._file-name tt_file._file-label tt_file._dump-name tt_file.iFileNum tt_file._desc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt_file._file-name   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt_file
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt_file
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttSearch, ~
       EACH tt_file NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1      FOR EACH ttSearch, ~
       EACH tt_file NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttSearch tt_file
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttSearch
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 tt_file


/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt_field._field-name tt_field._data-type tt_field._label tt_field._order tt_field._format tt_field._mandatory tt_field._initial tt_field._extent tt_field._width tt_field._view-as tt_field._col-label tt_field._help tt_field._desc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt_field._mandatory   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt_field
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt_field
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt_field
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH tt_field.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt_field
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt_field


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 idx.ix-navn idx.ix-felt idx.ix-sq idx.ix-asc idx.ix-kode idx.bSearchOn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 idx.bSearchOn   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 idx
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 idx
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH idx
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3      FOR EACH idx.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 idx
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 idx


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-65 RECT-66 RECT-67 BtnSearch fi-cTable ~
cb-cDB fi-cSearch tb-bShowHidden BROWSE-1 w-format w-label w-col-label ~
btnSave w-help w-descr w-view-as BROWSE-3 BROWSE-2 tb-bLC b-kopier ~
b-innhold tbOnlyMandatory t-kvalifiser cb-cType v-ant-pr-linje ~
tbRrowseSelected fi-cDescLab fi-cViewAsLab fi-cIdxLab 
&Scoped-Define DISPLAYED-OBJECTS fi-cTable cb-cDB fi-cSearch tb-bShowHidden ~
w-data-type w-order w-format w-decimals w-extent w-label w-initial ~
w-col-label tbMandatory w-help w-descr w-view-as tb-bLC tbOnlyMandatory ~
t-kvalifiser cb-cType v-ant-pr-linje tbRrowseSelected fi-cHelpLab ~
fi-cDescLab fi-cViewAsLab fi-cIdxLab 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkForResizeLib C-Win 
FUNCTION checkForResizeLib RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BROWSE-1 
       MENU-ITEM m_Copy2        LABEL "Copy table name"
       MENU-ITEM m_Browse       LABEL "Browse table contents"
       MENU-ITEM m_Browse_selected_fields LABEL "Browse selected fields"
       MENU-ITEM m_Export_tables_to_Excel LABEL "Export tables to Excel"
       MENU-ITEM m_Export_indexes_to_Excel LABEL "Export indexes to Excel".

DEFINE MENU POPUP-MENU-BROWSE-2 
       MENU-ITEM m_Copy         LABEL "Copy field name(s)"
       MENU-ITEM m_Copy_field_names_with_comma LABEL "Copy field names with comma separator"
       MENU-ITEM m_Copy_field_names_with_semic LABEL "Copy field names with semicolon separator"
       MENU-ITEM m_Copy_field_names_with__sepa LABEL "Copy field names with !~; separator"
       MENU-ITEM m_Copy_field_names_with_table LABEL "Copy field name(s) with table prefix"
       MENU-ITEM m_Copy_field_names_with_LF_pref LABEL "Copy field name(s) with  < LF + ~;> prefix"
       MENU-ITEM m_Copy_field_names_with_LF_2_pre LABEL "Copy field name(s) with < LF + ~;!> prefix"
       MENU-ITEM m_Browse_selected_fields2 LABEL "Browse selected fields"
       MENU-ITEM m_Export_fields_to_Excel LABEL "Export fields to Excel".


/* Definitions of the field level widgets                               */
DEFINE BUTTON b-innhold 
     LABEL "&Browse" 
     SIZE 14 BY .95.

DEFINE BUTTON b-kopier 
     LABEL "&Copy" 
     SIZE 9 BY .91.

DEFINE BUTTON btnSave 
     LABEL "Save temporary changes" 
     SIZE 33.4 BY 1 TOOLTIP "..to display format and label".

DEFINE BUTTON BtnSearch 
     LABEL "&Search" 
     SIZE 9.6 BY 1.19 TOOLTIP "Search (ALT-S)"
     BGCOLOR 8 .

DEFINE VARIABLE cb-cDB AS CHARACTER FORMAT "X(256)":U 
     LABEL "DB" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 13.8 BY 1 NO-UNDO.

DEFINE VARIABLE cb-cType AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 9
     LIST-ITEMS "All","Character","Date","Decimal","Integer","Logical","Raw","Recid","Rowid" 
     DROP-DOWN-LIST
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE w-descr AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 44.4 BY 1.24
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE w-help AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 44.4 BY 1.48
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE w-view-as AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 44.4 BY 1.33
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fi-cDescLab AS CHARACTER FORMAT "X(256)":U INITIAL "Desc:" 
      VIEW-AS TEXT 
     SIZE 7.2 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cHelpLab AS CHARACTER FORMAT "X(256)":U INITIAL "Help:" 
      VIEW-AS TEXT 
     SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cIdxLab AS CHARACTER FORMAT "X(256)":U INITIAL "Idx:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .91 TOOLTIP "Search file-, field- or index name for term (free text)" NO-UNDO.

DEFINE VARIABLE fi-cTable AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .91 NO-UNDO.

DEFINE VARIABLE fi-cViewAsLab AS CHARACTER FORMAT "X(256)":U INITIAL "V-as:" 
      VIEW-AS TEXT 
     SIZE 7.4 BY .62 NO-UNDO.

DEFINE VARIABLE v-ant-pr-linje AS INTEGER FORMAT "z9":U INITIAL 9 
     LABEL "Nb. pr line" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE w-col-label AS CHARACTER FORMAT "X(256)":U 
     LABEL "C-lab" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE w-data-type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE w-decimals AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Dec." 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81 NO-UNDO.

DEFINE VARIABLE w-extent AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Extent" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81 NO-UNDO.

DEFINE VARIABLE w-format AS CHARACTER FORMAT "X(256)":U 
     LABEL "Form" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE w-initial AS CHARACTER FORMAT "X(256)":U 
     LABEL "Init" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81 NO-UNDO.

DEFINE VARIABLE w-label AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lab" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE w-order AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Order" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 1.52.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24.6 BY 2.52.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.2 BY 2.52.

DEFINE VARIABLE t-kvalifiser AS LOGICAL INITIAL no 
     LABEL "Prefix table" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.8 BY .62 NO-UNDO.

DEFINE VARIABLE tb-bLC AS LOGICAL INITIAL no 
     LABEL "LC" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .57 NO-UNDO.

DEFINE VARIABLE tb-bShowHidden AS LOGICAL INITIAL no 
     LABEL "Show hidden" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tbMandatory AS LOGICAL INITIAL no 
     LABEL "Mand" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.8 BY .81 NO-UNDO.

DEFINE VARIABLE tbOnlyMandatory AS LOGICAL INITIAL no 
     LABEL "Only mand." 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .57 NO-UNDO.

DEFINE VARIABLE tbRrowseSelected AS LOGICAL INITIAL no 
     LABEL "Only sel. flds" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 1.8 BY 16.48
     BGCOLOR 8 .

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 44.2 BY .48
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttSearch, 
      tt_file SCROLLING.

DEFINE QUERY BROWSE-2 FOR 
      tt_field SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      idx SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt_file._file-name  WIDTH 30  FORMAT "x(100)"
      tt_file._file-label WIDTH 30  FORMAT "x(100)"
      tt_file._dump-name  WIDTH 30
      tt_file.iFileNum  
      tt_file._desc                 FORMAT "x(3000)"

ENABLE tt_file._file-name
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44 BY 9.14
         FONT 4 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      tt_field._field-name   WIDTH 20
      tt_field._data-type
      tt_field._label        WIDTH 10
      tt_field._order        FORMAT "ZZZ9"
      tt_field._format        
      tt_field._mandatory
      tt_field._initial
      tt_field._extent
      tt_field._width
      tt_field._view-as
      tt_field._col-label    WIDTH 30
      tt_field._help
      tt_field._desc
            
ENABLE tt_field._mandatory
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 44 BY 6.67
         FONT 4 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _FREEFORM
  QUERY BROWSE-3 DISPLAY
      idx.ix-navn     COLUMN-LABEL 'Ix-name' WIDTH 13 FORMAT 'X(50)'
  idx.ix-felt     COLUMN-LABEL 'Field'   WIDTH 14 FORMAT 'X(50)'
  idx.ix-sq       COLUMN-LABEL 'Sq'      FORMAT 'ZZ9'
  idx.ix-asc      COLUMN-LABEL "Asc"     FORMAT "X(3)"
  idx.ix-kode     COLUMN-LABEL "PU"      FORMAT "X(3)"
  idx.bSearchOn   COLUMN-LABEL "Sea" 

 ENABLE idx.bSearchOn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44.4 BY 6.81
         BGCOLOR 8 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN TOOLTIP "Dblclick to set index as search criteria when browsing then table (max 2 crit)".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnSearch AT ROW 1.19 COL 88.8
     fi-cTable AT ROW 1.33 COL 9.6 COLON-ALIGNED
     cb-cDB AT ROW 1.33 COL 30 COLON-ALIGNED
     fi-cSearch AT ROW 1.33 COL 66 COLON-ALIGNED NO-LABEL
     tb-bShowHidden AT ROW 1.43 COL 47
     BROWSE-1 AT ROW 2.62 COL 1
     w-data-type AT ROW 2.76 COL 56 COLON-ALIGNED
     w-order AT ROW 2.76 COL 87 COLON-ALIGNED
     w-format AT ROW 3.67 COL 56 COLON-ALIGNED
     w-decimals AT ROW 3.67 COL 87 COLON-ALIGNED
     w-extent AT ROW 4.62 COL 87 COLON-ALIGNED
     w-label AT ROW 4.67 COL 56 COLON-ALIGNED
     w-initial AT ROW 5.52 COL 87 COLON-ALIGNED
     w-col-label AT ROW 5.67 COL 56 COLON-ALIGNED
     btnSave AT ROW 6.76 COL 55.6
     tbMandatory AT ROW 6.81 COL 90.2
     w-help AT ROW 7.95 COL 55.6 NO-LABEL
     w-descr AT ROW 9.33 COL 55.6 NO-LABEL
     w-view-as AT ROW 10.52 COL 55.6 NO-LABEL
     BROWSE-3 AT ROW 12.05 COL 55.6
     BROWSE-2 AT ROW 12.33 COL 1
     tb-bLC AT ROW 19.33 COL 60.2
     b-kopier AT ROW 19.43 COL 50.2
     b-innhold AT ROW 19.43 COL 79.6
     tbOnlyMandatory AT ROW 19.57 COL 29
     t-kvalifiser AT ROW 19.91 COL 60.2
     cb-cType AT ROW 20.52 COL 27 COLON-ALIGNED
     v-ant-pr-linje AT ROW 20.57 COL 65.8 COLON-ALIGNED
     tbRrowseSelected AT ROW 20.67 COL 74.8
     fi-cHelpLab AT ROW 7.95 COL 46.6 COLON-ALIGNED NO-LABEL
     fi-cDescLab AT ROW 9.33 COL 46.4 COLON-ALIGNED NO-LABEL
     fi-cViewAsLab AT ROW 10.62 COL 46.2 COLON-ALIGNED NO-LABEL
     fi-cIdxLab AT ROW 12.19 COL 47.6 COLON-ALIGNED NO-LABEL
     RECT-65 AT ROW 1 COL 1
     RECT-66 AT ROW 19.24 COL 48.8
     RECT-67 AT ROW 19.24 COL 73.2
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 101.6 BY 21.05
         FONT 3.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 8.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS THREE-D 
         AT COL 38.14 ROW 2.58
         SIZE 13.57 BY 16.5.

DEFINE FRAME frSplitBarY
     btnSplitBarY AT ROW 1.95 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 1 ROW 10.81
         SIZE 44.14 BY 4.73.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Compile into: .\app
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dict. viewer"
         HEIGHT             = 21.05
         WIDTH              = 101.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/dict%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/dict%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frSplitBarY:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 frSplitBarX DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-3 frSplitBarY DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-2 BROWSE-3 DEFAULT-FRAME */
ASSIGN 
       BROWSE-1:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-BROWSE-1:HANDLE
       BROWSE-1:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BROWSE-2:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-BROWSE-2:HANDLE
       BROWSE-2:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BROWSE-3:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FILL-IN fi-cHelpLab IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tbMandatory IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-data-type IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-decimals IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-extent IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-initial IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-order IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frSplitBarX
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

/* SETTINGS FOR FRAME frSplitBarY
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frSplitBarY          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY BROWSE-1
     FOR EACH ttSearch, EACH tt_file NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY BROWSE-2 FOR EACH tt_field.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY BROWSE-3
     FOR EACH idx
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarY
/* Query rebuild information for FRAME frSplitBarY
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarY */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dict. viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dict. viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Dict. viewer */
DO:
  DYNAMIC-FUNCTION("setWidgetResize", {&WINDOW-NAME}:HANDLE,{&WINDOW-NAME}:HANDLE,"Resize","").
/*   DYNAMIC-FUNCTION("setWidgetResize" IN hResizeLib, {&WINDOW-NAME}:HANDLE,{&WINDOW-NAME}:HANDLE,"Resize",""). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-innhold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-innhold C-Win
ON CHOOSE OF b-innhold IN FRAME DEFAULT-FRAME /* Browse */
DO:
  RUN Browse-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-kopier C-Win
ON CHOOSE OF b-kopier IN FRAME DEFAULT-FRAME /* Copy */
DO:
  ASSIGN t-kvalifiser   = INPUT FRAME {&FRAME-NAME} t-kvalifiser
         cSeparator     = " "
         v-ant-pr-linje = INPUT FRAME {&FRAME-NAME} v-ant-pr-linje.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  DEF VAR v-i  AS INTEGER.
  DEF VAR v-i1  AS INTEGER.
  DEF VAR v-ok AS LOGICAL.
  
  ASSIGN tb-bLC.

  CLIPBOARD:MULTIPLE = FALSE.
  CLIPBOARD:ITEMS-PER-ROW = 1.

  DO v-i = 1 TO SELF:NUM-SELECTED-ROWS:
     v-ok = SELF:FETCH-SELECTED-ROW(v-i).
     IF v-ok THEN 
       CLIPBOARD:VALUE = TRIM(tt_file._file-name).
     ASSIGN v-i1 = v-i.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON START-SEARCH OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  DEF VAR hColumn AS HANDLE NO-UNDO.

  hColumn = browse-1:CURRENT-COLUMN.

  IF hColumn:NAME = cSortColumn1 THEN bDesc1 = NOT bDesc1.

  cSortColumn1 = hColumn:NAME.

  RUN OpenQueryFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON VALUE-CHANGED OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  RUN ChangeFileRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
  DEF VAR v-i  AS INTEGER.
  DEF VAR v-i1  AS INTEGER.
  DEF VAR v-ok AS LOGICAL.
  DEF VAR cList AS CHAR NO-UNDO.
  
  ASSIGN tb-bLC.

  CLIPBOARD:MULTIPLE = FALSE.
  DO v-i = 1 TO SELF:NUM-SELECTED-ROWS:
     v-ok = SELF:FETCH-SELECTED-ROW(v-i).
     IF v-ok THEN DO:
        IF t-kvalifiser THEN
          cList = cList + (IF tb-bLC THEN LC(TRIM(tt_file._file-name + "." + tt_field._field-name))
                           ELSE TRIM(tt_file._file-name + "." + tt_field._field-name)) + cSeparator.
        ELSE
           cList = cList + (IF tb-bLC THEN LC(TRIM(tt_field._field-name)) 
                            ELSE TRIM(tt_field._field-name)) + cSeparator.
     END.
     ASSIGN v-i1 = v-i.
  END.
  CLIPBOARD:VALUE = TRIM(cList,cSeparator).

/*   CLIPBOARD:MULTIPLE = TRUE. */
/*   CLIPBOARD:ITEMS-PER-ROW = v-ant-pr-linje. */

/*   DO v-i = 1 TO SELF:NUM-SELECTED-ROWS:                                                                     */
/*      v-ok = SELF:FETCH-SELECTED-ROW(v-i).                                                                   */
/*      IF v-ok THEN DO:                                                                                       */
/*         IF t-kvalifiser THEN                                                                                */
/*            CLIPBOARD:VALUE = IF tb-bLC THEN LC(TRIM(tt_file._file-name + "." + tt_field._field-name))       */
/*                              ELSE TRIM(tt_file._file-name + "." + tt_field._field-name).                    */
/*         ELSE                                                                                                */
/*            CLIPBOARD:VALUE = IF tb-bLC THEN LC(TRIM(tt_field._field-name)) ELSE TRIM(tt_field._field-name). */
/*      END.                                                                                                   */
/*      ASSIGN v-i1 = v-i.                                                                                     */
/*   END.                                                                                                      */
/*                                                                                                             */
/*   CLIPBOARD:MULTIPLE = FALSE.                                                                               */
/*   DO v-i = 1 TO v-i1:                      */
/*      v-ok = SELF:DESELECT-SELECTED-ROW(1). */
/*   END.                                     */
/*                                            */
/*   DO WITH FRAME {&FRAME-NAME}:             */
/*      ASSIGN w-help    = ""                 */
/*             w-view-as = ""                 */
/*             w-descr   = "".                */
/*      DISPLAY  "" @ w-data-type             */
/*               "" @ w-order                 */
/*               "" @ w-format                */
/*               "" @ w-decimals              */
/*               "" @ w-label                 */
/*               "" @ w-extent                */
/*               "" @ w-col-label             */
/*               "" @ w-initial               */
/*                w-help w-view-as w-descr.   */
/*   END.                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON START-SEARCH OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
  DEF VAR hColumn AS HANDLE NO-UNDO.

  hColumn = browse-2:CURRENT-COLUMN.

  IF hColumn:NAME = cSortColumn2 THEN bDesc2 = NOT bDesc2.

  cSortColumn2 = hColumn:NAME.

  RUN OpenQueryField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN w-help      = tt_field._help
            w-view-as   = tt_field._view-as
            w-descr     = tt_field._desc
            tbMandatory = tt_field._mandatory.
     DISPLAY  tt_field._data-type @ w-data-type
              tt_field._order     @ w-order
              tt_field._format    @ w-format
              tt_field._decimals  @ w-decimals
              tt_field._label     @ w-label
              tt_field._extent    @ w-extent
              tt_field._col-label @ w-col-label
              tt_field._initial   @ w-initial
              tbMandatory
              w-help w-view-as w-descr.    
    IF browse-2:NUM-SELECTED-ROWS = 0 THEN DO:
       tbRrowseSelected = FALSE.
       DISP tbRrowseSelected.
    END.
    ELSE DO:
       tbRrowseSelected = TRUE.
       DISP tbRrowseSelected.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-3 IN FRAME DEFAULT-FRAME
DO:
  FIND CURRENT idx.
  IF idx.Ix-navn NE "" THEN idx.bSearchOn = NOT idx.bSearchOn.
  bOK = browse-3:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save temporary changes */
DO:
  FIND CURRENT tt_field.
  IF AVAIL tt_field THEN
    ASSIGN tt_field._format    = w-format:SCREEN-VALUE
           tt_field._label     = w-label:SCREEN-VALUE
           tt_field._col-label = w-col-label:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnSearch C-Win
ON CHOOSE OF BtnSearch IN FRAME DEFAULT-FRAME /* Search */
DO:
  RUN SearchAll.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarY
&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME frSplitBarY
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frSplitBarY,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cb-cDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cDB C-Win
ON VALUE-CHANGED OF cb-cDB IN FRAME DEFAULT-FRAME /* DB */
DO:
  ASSIGN cb-cDB.  
  IF fi-cSearch:SCREEN-VALUE NE "" THEN
    APPLY "choose" TO BtnSearch.
  ELSE  
    RUN OpenQueryFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cType C-Win
ON VALUE-CHANGED OF cb-cType IN FRAME DEFAULT-FRAME /* Type */
DO:
  RUN OpenQueryField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cSearch C-Win
ON RETURN OF fi-cSearch IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO BtnSearch.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cTable C-Win
ON ANY-PRINTABLE OF fi-cTable IN FRAME DEFAULT-FRAME /* Table */
DO:
  APPLY LASTKEY.
  RUN TableSearch.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cTable C-Win
ON BACKSPACE OF fi-cTable IN FRAME DEFAULT-FRAME /* Table */
DO:
  APPLY LASTKEY.
  RUN TableSearch.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Browse C-Win
ON CHOOSE OF MENU-ITEM m_Browse /* Browse table contents */
DO:
  tbRrowseSelected = FALSE.
  DISP tbRrowseSelected WITH FRAME {&FRAME-NAME}.
  RUN Browse-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Browse_selected_fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Browse_selected_fields C-Win
ON CHOOSE OF MENU-ITEM m_Browse_selected_fields /* Browse selected fields */
DO:
  tbRrowseSelected = TRUE.
  DISP tbRrowseSelected WITH FRAME {&FRAME-NAME}.
  RUN Browse-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Browse_selected_fields2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Browse_selected_fields2 C-Win
ON CHOOSE OF MENU-ITEM m_Browse_selected_fields2 /* Browse selected fields */
DO:
  tbRrowseSelected = TRUE.
  DISP tbRrowseSelected WITH FRAME {&FRAME-NAME}.
  RUN Browse-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy C-Win
ON CHOOSE OF MENU-ITEM m_Copy /* Copy field name(s) */
DO:
  ASSIGN t-kvalifiser   = FALSE
         cSeparator     = " ".
  DISP t-kvalifiser WITH FRAME {&FRAME-NAME}.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy2 C-Win
ON CHOOSE OF MENU-ITEM m_Copy2 /* Copy table name */
DO:
  APPLY "DEFAULT-ACTION":U TO BROWSE-1 IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy_field_names_with_comma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy_field_names_with_comma C-Win
ON CHOOSE OF MENU-ITEM m_Copy_field_names_with_comma /* Copy field names with comma separator */
DO:
  ASSIGN t-kvalifiser   = FALSE
         cSeparator     = ",".
  DISP t-kvalifiser WITH FRAME {&FRAME-NAME}.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy_field_names_with_LF_2_pre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy_field_names_with_LF_2_pre C-Win
ON CHOOSE OF MENU-ITEM m_Copy_field_names_with_LF_2_pre /* Copy field name(s) with < LF + ;!> prefix */
DO:
  ASSIGN t-kvalifiser   = FALSE
         cSeparator     = '"' + CHR(10) + '~t+ ";!'.
  DISP t-kvalifiser WITH FRAME {&FRAME-NAME}.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy_field_names_with_LF_pref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy_field_names_with_LF_pref C-Win
ON CHOOSE OF MENU-ITEM m_Copy_field_names_with_LF_pref /* Copy field name(s) with  < LF + ;> prefix */
DO:
  ASSIGN t-kvalifiser   = FALSE
         cSeparator     = '"' + CHR(10) + '~t+ ";'.
  DISP t-kvalifiser WITH FRAME {&FRAME-NAME}.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy_field_names_with_semic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy_field_names_with_semic C-Win
ON CHOOSE OF MENU-ITEM m_Copy_field_names_with_semic /* Copy field names with semicolon separator */
DO:
  ASSIGN t-kvalifiser   = FALSE
         cSeparator     = ";".
  DISP t-kvalifiser WITH FRAME {&FRAME-NAME}.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy_field_names_with_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy_field_names_with_table C-Win
ON CHOOSE OF MENU-ITEM m_Copy_field_names_with_table /* Copy field name(s) with table prefix */
DO:
  ASSIGN t-kvalifiser   = YES
         cSeparator     = " ".
  DISP t-kvalifiser WITH FRAME {&FRAME-NAME}.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copy_field_names_with__sepa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copy_field_names_with__sepa C-Win
ON CHOOSE OF MENU-ITEM m_Copy_field_names_with__sepa /* Copy field names with !; separator */
DO:
  ASSIGN t-kvalifiser   = FALSE
         cSeparator     = ";!".
  DISP t-kvalifiser WITH FRAME {&FRAME-NAME}.
  APPLY "DEFAULT-ACTION":U TO BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Export_fields_to_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Export_fields_to_Excel C-Win
ON CHOOSE OF MENU-ITEM m_Export_fields_to_Excel /* Export fields to Excel */
DO:
  RUN ToExcelViaFile.p (BROWSE browse-2:HANDLE,0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Export_indexes_to_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Export_indexes_to_Excel C-Win
ON CHOOSE OF MENU-ITEM m_Export_indexes_to_Excel /* Export indexes to Excel */
DO:
  RUN IndexesToExcel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Export_tables_to_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Export_tables_to_Excel C-Win
ON CHOOSE OF MENU-ITEM m_Export_tables_to_Excel /* Export tables to Excel */
DO:
  RUN ToExcelViaFile.p (BROWSE browse-1:HANDLE,0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb-bShowHidden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb-bShowHidden C-Win
ON VALUE-CHANGED OF tb-bShowHidden IN FRAME DEFAULT-FRAME /* Show hidden */
DO:
  ASSIGN tb-bShowHidden.
  APPLY "VALUE-CHANGED" TO cb-cDB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbOnlyMandatory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbOnlyMandatory C-Win
ON VALUE-CHANGED OF tbOnlyMandatory IN FRAME DEFAULT-FRAME /* Only mand. */
DO:
  RUN OpenQueryField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
   RUN disable_UI.
   IF NOT bResizeLibLoaded THEN
     DELETE PROCEDURE hResizeLib.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  RUN InitWin.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Browse-table C-Win 
PROCEDURE Browse-table :
DEF VAR cBrowseProg  AS CHAR NO-UNDO.
DEF VAR cSearchF1    AS CHAR NO-UNDO.
DEF VAR cSearchF2    AS CHAR NO-UNDO.
DEF VAR cSFtype1     AS CHAR NO-UNDO.
DEF VAR cSFtype2     AS CHAR NO-UNDO.
DEF VAR cSFlabel1    AS CHAR NO-UNDO.
DEF VAR cSFlabel2    AS CHAR NO-UNDO.
DEF VAR cDelim       AS CHAR INIT '";"' NO-UNDO.

IF AVAIL tt_file THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN tbRrowseSelected.
  
  cDbTableName = cb-cDB:SCREEN-VALUE + "." + tt_file._file-name.
  MESSAGE "Do you want to be able to edit the records" 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bEditMode.
           
  ASSIGN cBrowseProg = SESSION:TEMP-DIR + cDbTableName + ".w".

  RUN Find-searchcrit(
      OUTPUT cSearchF1,
      OUTPUT cSearchF2,
      OUTPUT cSFtype1 ,
      OUTPUT cSFtype2 ,
      OUTPUT cSFlabel1,
      OUTPUT cSFlabel2).
    
  DO:
    OUTPUT TO VALUE(cBrowseProg).

    PUT UNFORMATTED "/* Variable definitions: */" SKIP(1).
    PUT UNFORMATTED "DEF VAR hnd-win      AS WIDGET-HANDLE NO-UNDO." SKIP.
    PUT UNFORMATTED "DEF VAR bOK          AS LOGICAL NO-UNDO." SKIP.
    PUT UNFORMATTED "DEF VAR cExpFile     AS CHAR FORMAT 'X(40)' NO-UNDO INIT 'c:\temp\" REPLACE(cDbTableName,".","-") ".csv'." SKIP(1).
    PUT UNFORMATTED "DEF VAR hResizeLib   AS HANDLE NO-UNDO." SKIP.
    PUT UNFORMATTED "DEF VAR cSortColumn  AS CHAR NO-UNDO." SKIP.
    PUT UNFORMATTED "DEF VAR bDesc        AS LOG NO-UNDO." SKIP.

    PUT UNFORMATTED "/* Window definition: */" SKIP(1).
    PUT UNFORMATTED "CREATE WINDOW hnd-win" SKIP.
    PUT UNFORMATTED "       ASSIGN HEIGHT             = 15"  SKIP.
    PUT UNFORMATTED "              WIDTH              = 100"  SKIP.
/*     PUT UNFORMATTED "              MAX-HEIGHT         = 30"  SKIP. */
/*     PUT UNFORMATTED "              MAX-WIDTH          = 146" SKIP. */
    PUT UNFORMATTED "              MAX-HEIGHT         = 90"  SKIP.
    PUT UNFORMATTED "              MAX-WIDTH          = 246" SKIP.
    PUT UNFORMATTED "              VIRTUAL-HEIGHT     = 30"  SKIP.
    PUT UNFORMATTED "              VIRTUAL-WIDTH      = 146" SKIP.
    PUT UNFORMATTED "              RESIZE             = yes" SKIP.
    PUT UNFORMATTED "              SCROLL-BARS        = no"  SKIP.
    PUT UNFORMATTED "              STATUS-AREA        = no"  SKIP.
    PUT UNFORMATTED "              KEEP-FRAME-Z-ORDER = yes" SKIP.
    PUT UNFORMATTED "              THREE-D            = yes" SKIP.
    PUT UNFORMATTED "              SENSITIVE          = yes" SKIP.
    PUT UNFORMATTED "              MESSAGE-AREA       = no"  SKIP.
    PUT UNFORMATTED "              TITLE              = ".
    PUT UNFORMATTED "'".
    PUT UNFORMATTED "Contents of ".    
    PUT UNFORMATTED cDbTableName " (" tt_file._file-label ")".
    PUT UNFORMATTED "'." SKIP(1).
    PUT UNFORMATTED "bOK = hnd-win:LOAD-ICON('adeicon/dict%.ico')." SKIP.
    
    PUT UNFORMATTED "ASSIGN CURRENT-WINDOW = hnd-win." SKIP(1).

    PUT UNFORMATTED "/* Widget definitions: */" SKIP(1).
    IF cSearchF1 NE "" THEN 
      PUT UNFORMATTED "DEFINE BUTTON Btn_Done" SKIP.
    ELSE
      PUT UNFORMATTED "DEFINE BUTTON Btn_Done DEFAULT" SKIP.
    PUT UNFORMATTED "       LABEL '&Close'" SKIP.
    PUT UNFORMATTED "       SIZE 9 BY 1.17" SKIP.
    PUT UNFORMATTED "       BGCOLOR 8." SKIP(1).

    IF cSearchF1 NE "" THEN DO:
      PUT UNFORMATTED "DEFINE BUTTON Btn_Search DEFAULT" SKIP.
      PUT UNFORMATTED "       LABEL '&Search'" SKIP.
      PUT UNFORMATTED "       SIZE 9 BY 1.17" SKIP.
      PUT UNFORMATTED "       BGCOLOR 8." SKIP(1).
    END.

    PUT UNFORMATTED "DEFINE BUTTON Btn_Export" SKIP.
    PUT UNFORMATTED "       LABEL '&Export'" SKIP.
    PUT UNFORMATTED "       SIZE 9 BY 1.17" SKIP.
    PUT UNFORMATTED "       BGCOLOR 8." SKIP(1).

    IF cSearchF1 NE "" THEN DO:
      IF cSFtype1 = "character" THEN
        PUT UNFORMATTED "DEFINE VARIABLE fi-search1 AS CHARACTER FORMAT 'X(256)':U" SKIP.
      ELSE IF cSFtype1 = "date" THEN
        PUT UNFORMATTED "DEFINE VARIABLE fi-search1 AS DATE FORMAT '99/99/9999':U" SKIP.
      ELSE IF cSFtype1 = "logical" THEN
        PUT UNFORMATTED "DEFINE VARIABLE fi-search1 AS LOGICAL FORMAT 'Y/N':U" SKIP.
      ELSE
        PUT UNFORMATTED "DEFINE VARIABLE fi-search1 AS INT FORMAT '>>>>>>>>>>>9':U" SKIP.
      PUT UNFORMATTED "       LABEL '" cSearchF1 "'" SKIP. 
      PUT UNFORMATTED "       VIEW-AS FILL-IN" SKIP. 
      PUT UNFORMATTED "       SIZE 10 BY .88 NO-UNDO." SKIP(1).
      
    END.
    IF cSearchF2 NE "" THEN DO:
      IF cSFtype2 = "character" THEN
        PUT UNFORMATTED "DEFINE VARIABLE fi-search2 AS CHARACTER FORMAT 'X(256)':U" SKIP.
      ELSE IF cSFtype2 = "date" THEN
        PUT UNFORMATTED "DEFINE VARIABLE fi-search2 AS DATE FORMAT '99/99/9999':U" SKIP.
      ELSE IF cSFtype2 = "logical" THEN
        PUT UNFORMATTED "DEFINE VARIABLE fi-search2 AS LOGICAL FORMAT 'Y/N':U" SKIP.
      ELSE
        PUT UNFORMATTED "DEFINE VARIABLE fi-search2 AS INT FORMAT '>>>>>>>>>>>9':U" SKIP.
      PUT UNFORMATTED "       LABEL '" cSearchF2 "'" SKIP. 
      PUT UNFORMATTED "       VIEW-AS FILL-IN" SKIP. 
      PUT UNFORMATTED "       SIZE 10 BY .88 NO-UNDO." SKIP(1).

    END.

    RUN QueryForBrowse.        

    PUT UNFORMATTED "/* Frame definition: */" SKIP(1).

    PUT UNFORMATTED "DEFINE FRAME f1" SKIP.
    PUT UNFORMATTED "       b1 AT ROW 1 COL 1 HELP 'Browse'" SKIP.
    IF cSearchF1 NE "" THEN DO:
      PUT UNFORMATTED "       fi-search1  AT ROW 14.3 COL 5 LABEL '" cSFLabel1 "' HELP 'Search for '" SKIP.
      PUT UNFORMATTED "       Btn_Search  AT ROW 14.3 COL 72 HELP 'Search'" SKIP.
    END.
    IF cSearchF2 NE "" THEN DO:
      PUT UNFORMATTED "       fi-search2  AT ROW 14.3 COL 30  LABEL '" cSFLabel2 "' HELP 'Search for '" SKIP.
    END.
    PUT UNFORMATTED "       Btn_Export AT ROW 14.3 COL 82 HELP 'Export rows as semicolon separated values'" SKIP.
    PUT UNFORMATTED "       Btn_Done AT ROW 14.3 COL 92 HELP 'Close window'" SKIP.
    PUT UNFORMATTED "       WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY" SKIP.
    PUT UNFORMATTED "            SIDE-LABELS NO-UNDERLINE THREE-D" SKIP.
    PUT UNFORMATTED "            AT COL 1 ROW 1" SKIP.
    IF cSearchF1 NE "" THEN 
      PUT UNFORMATTED "            DEFAULT-BUTTON btn_search" SKIP.
    PUT UNFORMATTED "            SIZE 120.43 BY 35.71 FONT 3." SKIP(1).

    PUT UNFORMATTED "ASSIGN  b1:COLUMN-RESIZABLE IN FRAME f1       = TRUE." SKIP(1).

    /* Generate control triggers: */

    PUT UNFORMATTED "/* Control Triggers: */" SKIP(1).

    PUT UNFORMATTED "ON 'START-SEARCH' OF b1 DO:" SKIP.
    PUT UNFORMATTED "  IF b1:CURRENT-COLUMN:NAME = cSortColumn THEN bDesc = NOT bDesc." SKIP.
    PUT UNFORMATTED "  cSortColumn = b1:CURRENT-COLUMN:NAME." SKIP.
    PUT UNFORMATTED "  RUN Open-query2." SKIP.
    PUT UNFORMATTED "END." SKIP.
    
    IF cSearchF1 NE "" THEN DO:
      PUT UNFORMATTED "ON 'CHOOSE' OF btn_search DO:" SKIP.
      PUT UNFORMATTED "  ASSIGN fi-search1." SKIP.
      IF cSearchF2 NE "" THEN 
        PUT UNFORMATTED "  ASSIGN fi-search2." SKIP.
      PUT UNFORMATTED "  RUN Open-query." SKIP.
      PUT UNFORMATTED "END." SKIP.
    END.

    PUT UNFORMATTED "ON 'CHOOSE' OF btn_Export DO:" SKIP.
      IF cSearchF1 NE "" THEN 
        PUT UNFORMATTED "  ASSIGN fi-search1." SKIP.
      IF cSearchF2 NE "" THEN 
        PUT UNFORMATTED "  ASSIGN fi-search2." SKIP.
    PUT UNFORMATTED "  RUN ToExcelViaFile.p (b1:HANDLE,20000)." SKIP.
/*     PUT UNFORMATTED "  RUN Export-data." SKIP. */
    PUT UNFORMATTED "END." SKIP.

    PUT UNFORMATTED "ON END-ERROR OF hnd-win OR ENDKEY OF hnd-win ANYWHERE DO:" SKIP.
    PUT UNFORMATTED "  RETURN NO-APPLY." SKIP.
    PUT UNFORMATTED "END." SKIP.
    PUT UNFORMATTED "ON WINDOW-CLOSE OF hnd-win DO:" SKIP.
    PUT UNFORMATTED "  APPLY 'CLOSE' TO THIS-PROCEDURE." SKIP.
    PUT UNFORMATTED "  RETURN NO-APPLY." SKIP.
    PUT UNFORMATTED "END." SKIP.
    PUT UNFORMATTED "ON 'CHOOSE' OF Btn_Done IN FRAME f1 APPLY 'CLOSE' TO THIS-PROCEDURE." SKIP(1).


    PUT UNFORMATTED SKIP(1) "/* Procedures: */" SKIP(1).

    /* Generate procedure for open-query: */
    PUT UNFORMATTED "PROCEDURE Open-query:" SKIP.
    PUT UNFORMATTED "DO WITH FRAME f1:" SKIP.
    PUT UNFORMATTED "  bOK = SESSION:SET-WAIT-STATE('General')." SKIP.
    IF cSearchF1 NE "" THEN DO:
      PUT UNFORMATTED "  ASSIGN fi-search1" + (IF cSearchF2 NE "" THEN " fi-search2" ELSE "") + "." SKIP.
      PUT UNFORMATTED "  IF fi-search1 NE ? AND fi-search1:SCREEN-VALUE NE '' THEN DO:" SKIP.      
      PUT UNFORMATTED "    OPEN QUERY q1 FOR EACH " cDbTableName " WHERE" SKIP.
      IF cSFtype1 = "character" THEN DO:
        PUT UNFORMATTED "         " cSearchF1 " BEGINS fi-search1 ".
      END.
      ELSE
        PUT UNFORMATTED "         " cSearchF1 " = fi-search1 ".
      IF cSearchF2 NE "" THEN DO:
        PUT SKIP.
        IF cSFtype2 = "character" THEN
          PUT UNFORMATTED "         AND IF fi-search2:SCREEN-VALUE NE '' THEN " cSearchF2 " BEGINS fi-search2 ELSE TRUE".
        ELSE 
          PUT UNFORMATTED "         AND IF fi-search2 NE ? AND fi-search2:SCREEN-VALUE NE '' THEN " cSearchF2 " = fi-search2 ELSE TRUE".
      END.
      IF NOT bEditMode THEN
        PUT UNFORMATTED " NO-LOCK." SKIP.
      ELSE
        PUT UNFORMATTED "." SKIP.
      PUT UNFORMATTED "  END." SKIP.
    END.
    IF cSearchF2 NE "" THEN DO:
      PUT UNFORMATTED "  ELSE IF fi-search2 NE ? AND fi-search2:SCREEN-VALUE NE '' THEN DO: " SKIP.
      PUT UNFORMATTED "    OPEN QUERY q1 FOR EACH " cDbTableName " WHERE" SKIP.
      IF cSFtype2 = "character" THEN
        PUT UNFORMATTED "         " cSearchF2 " BEGINS fi-search2".
      ELSE 
        PUT UNFORMATTED "         " cSearchF2 " = fi-search2".
      IF NOT bEditMode THEN
        PUT UNFORMATTED " NO-LOCK." SKIP.
      ELSE
        PUT UNFORMATTED "." SKIP.
      PUT UNFORMATTED "  END." SKIP.
    END.

    IF cSearchF1 NE "" THEN PUT UNFORMATTED "  ELSE". 

    PUT UNFORMATTED "  OPEN QUERY q1 FOR EACH " cDbTableName.
    IF NOT bEditMode THEN
      PUT UNFORMATTED " NO-LOCK." SKIP.
    ELSE
      PUT UNFORMATTED "." SKIP.

    PUT UNFORMATTED "  bOK = SESSION:SET-WAIT-STATE('')." SKIP.
    PUT UNFORMATTED "END." SKIP(1).
    PUT UNFORMATTED "END PROCEDURE." SKIP(1).        
    
    
    PUT UNFORMATTED "PROCEDURE Open-query2:" SKIP.
    PUT UNFORMATTED "DEF VAR hQuery       AS HANDLE NO-UNDO." SKIP.
    PUT UNFORMATTED "DEF VAR cQueryString AS CHAR NO-UNDO." SKIP.
    PUT UNFORMATTED "bOk = SESSION:SET-WAIT-STATE('General')." SKIP. 
    PUT UNFORMATTED "DO WITH FRAME f1:" SKIP.
    PUT UNFORMATTED "  hQuery       = b1:QUERY." SKIP.
    PUT UNFORMATTED "  cQueryString = 'FOR EACH " + cDbTableName + " NO-LOCK BY ' + cSortColumn + IF bDesc THEN ' DESC.' ELSE '.'." SKIP.
    PUT UNFORMATTED "  hQuery:QUERY-PREPARE(cQueryString)." SKIP.
    PUT UNFORMATTED "  hQuery:QUERY-OPEN." SKIP.
    PUT UNFORMATTED "END." SKIP(1).
    PUT UNFORMATTED "bOk = SESSION:SET-WAIT-STATE('')." SKIP. 
    PUT UNFORMATTED "END PROCEDURE." SKIP(1).        

    RUN GenProcForExport.                              

    IF cSearchF1 NE "" THEN DO:
      PUT UNFORMATTED "  IF fi-search1:SCREEN-VALUE NE '' THEN DO:" SKIP.      
      PUT UNFORMATTED "    FOR EACH " cDbTableName " WHERE" SKIP.
      IF cSFtype1 = "character" THEN 
        PUT UNFORMATTED "        " cSearchF1 " BEGINS fi-search1 ".
      ELSE
        PUT UNFORMATTED "        " cSearchF1 " = fi-search1 ".
      IF cSearchF2 NE "" THEN DO:
        PUT SKIP.
        IF cSFtype2 = "character" THEN
          PUT UNFORMATTED "        AND IF fi-search2:SCREEN-VALUE NE '' THEN " cSearchF2 " BEGINS fi-search2 ELSE TRUE".
        ELSE 
          PUT UNFORMATTED "        AND IF fi-search2:SCREEN-VALUE NE '' THEN " cSearchF2 " = fi-search2 ELSE TRUE".
      END.
      PUT UNFORMATTED "        NO-LOCK:" SKIP.
      PUT UNFORMATTED "      EXPORT DELIMITER " cDelim " " _file-name "." SKIP. 
      PUT UNFORMATTED "    END." SKIP.
      PUT UNFORMATTED "  END." SKIP.
    END.
    IF cSearchF2 NE "" THEN DO:
      PUT UNFORMATTED "  ELSE IF fi-search2:SCREEN-VALUE NE '' THEN DO: " SKIP.
      PUT UNFORMATTED "     FOR EACH " cDbTableName " WHERE" SKIP.
      IF cSFtype2 = "character" THEN
        PUT UNFORMATTED "         " cSearchF2 " BEGINS fi-search2".
      ELSE 
        PUT UNFORMATTED "         " cSearchF2 " = fi-search2".
      PUT UNFORMATTED "        NO-LOCK:" SKIP.
      PUT UNFORMATTED "      EXPORT DELIMITER " cDelim " " _file-name "." SKIP. 
      PUT UNFORMATTED "    END." SKIP.
      PUT UNFORMATTED "  END." SKIP.
    END.
    IF cSearchF1 NE "" THEN DO:
      PUT UNFORMATTED "  ELSE  FOR EACH " cDbTableName.
      PUT UNFORMATTED "        NO-LOCK:" SKIP.
      PUT UNFORMATTED "      EXPORT DELIMITER " cDelim " " _file-name "." SKIP. 
      PUT UNFORMATTED "    END." SKIP.
    END.
    ELSE DO:
      PUT UNFORMATTED "  FOR EACH " cDbTableName.
      PUT UNFORMATTED "        NO-LOCK:" SKIP.
      PUT UNFORMATTED "    EXPORT DELIMITER " cDelim " " _file-name "." SKIP. 
      PUT UNFORMATTED "  END." SKIP.
    END.
    PUT UNFORMATTED "  OUTPUT CLOSE." SKIP(1).
    PUT UNFORMATTED "  bOK = SESSION:SET-WAIT-STATE('')." SKIP.
    PUT UNFORMATTED "END." SKIP(1).
    PUT UNFORMATTED "END PROCEDURE." SKIP(1).        

    /* Generate Main Block: */        
    PUT UNFORMATTED "/* Main Block: */" SKIP(1).
    PUT UNFORMATTED "ASSIGN CURRENT-WINDOW = hnd-win" SKIP.
    PUT UNFORMATTED "       THIS-PROCEDURE:CURRENT-WINDOW = hnd-win." SKIP.

    PUT UNFORMATTED "ON 'CLOSE' OF THIS-PROCEDURE DO:" SKIP.
    PUT UNFORMATTED "  DELETE WIDGET hnd-win." SKIP.
    PUT UNFORMATTED "  DELETE PROCEDURE THIS-PROCEDURE." SKIP.
    PUT UNFORMATTED "  DELETE PROCEDURE hResizeLib." SKIP.
    PUT UNFORMATTED "END." SKIP(1).

    PUT UNFORMATTED "ON 'WINDOW-RESIZED' OF CURRENT-WINDOW DO:" SKIP.
    PUT UNFORMATTED "  bOk = DYNAMIC-FUNCTION('setWidgetResize' IN hResizeLib, CURRENT-WINDOW,CURRENT-WINDOW,'Resize','')." SKIP.
    PUT UNFORMATTED "END." SKIP(1).

    PUT UNFORMATTED "ENABLE b1 WITH FRAME f1." SKIP.
    IF cSearchF1 NE "" THEN 
      PUT UNFORMATTED "ENABLE fi-search1 WITH FRAME f1." SKIP.
    IF cSearchF2 NE "" THEN 
      PUT UNFORMATTED "ENABLE fi-search2 WITH FRAME f1." SKIP.
    IF cSearchF1 NE "" THEN 
      PUT UNFORMATTED "ENABLE Btn_Search WITH FRAME f1." SKIP.
    PUT UNFORMATTED "ENABLE Btn_Export Btn_Done WITH FRAME f1." SKIP.

    IF NOT bEditMode THEN
      PUT UNFORMATTED cEnable ":READ-ONLY IN BROWSE b1 = TRUE." SKIP.

    PUT UNFORMATTED "RUN ResizeLib.p PERSISTE SET hResizeLib." SKIP.
    PUT UNFORMATTED "DYNAMIC-FUNCTION('setOrgWinSize' IN hResizeLib, THIS-PROCEDURE:CURRENT-WINDOW,300,150,500,250)." SKIP.

    PUT UNFORMATTED "RUN Open-query." SKIP.
        
    OUTPUT CLOSE.  
  END.  

  RUN VALUE(cBrowseProg) PERSIST. 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeFileRecord C-Win 
PROCEDURE ChangeFileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
      
  IF AVAIL tt_file THEN DO:
    tbRrowseSelected = FALSE.
    DISP tbRrowseSelected.
    
    RUN OpenQueryField.
    FOR EACH idx:
        DELETE idx.
    END.
     
    FOR EACH tt_index OF tt_file NO-LOCK,
        EACH tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field WHERE tt_field.iFieldNum = tt_index-field.iFieldNum NO-LOCK
        BREAK BY  tt_index._index-name
        BY tt_index-field._index-seq:
     
      CREATE idx.
      IF FIRST-OF( tt_index._index-name) THEN
        ASSIGN idx.ix-navn = tt_index._index-name.
      
      ASSIGN idx.ix-felt    = tt_index-field.cFieldName
             idx.cData-type = tt_field._Data-type
             idx.ix-sq      = tt_index-field._index-seq
             idx.ix-asc     = IF tt_index-field._ascending THEN "A" ELSE " "
             idx.ix-kode    = tt_index.cCode
/*              IF RECID(tt_index) = tt_file._prime-index THEN "P"    */
/*                              ELSE " "                              */
/*             idx.ix-kode    = idx.ix-kode +                         */
/*                              IF  tt_index._UNIQUE THEN "U" ELSE " "*/
             .
    END.
    RUN OpenQueryIndex.
  
  END.
  ELSE MESSAGE "What??".
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
  DISPLAY fi-cTable cb-cDB fi-cSearch tb-bShowHidden w-data-type w-order 
          w-format w-decimals w-extent w-label w-initial w-col-label tbMandatory 
          w-help w-descr w-view-as tb-bLC tbOnlyMandatory t-kvalifiser cb-cType 
          v-ant-pr-linje tbRrowseSelected fi-cHelpLab fi-cDescLab fi-cViewAsLab 
          fi-cIdxLab 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-65 RECT-66 RECT-67 BtnSearch fi-cTable cb-cDB fi-cSearch 
         tb-bShowHidden BROWSE-1 w-format w-label w-col-label btnSave w-help 
         w-descr w-view-as BROWSE-3 BROWSE-2 tb-bLC b-kopier b-innhold 
         tbOnlyMandatory t-kvalifiser cb-cType v-ant-pr-linje tbRrowseSelected 
         fi-cDescLab fi-cViewAsLab fi-cIdxLab 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
  ENABLE btnSplitBarY 
      WITH FRAME frSplitBarY IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarY}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Find-searchcrit C-Win 
PROCEDURE Find-searchcrit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER cSearchF1    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cSearchF2    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cSFtype1     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cSFtype2     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cSFlabel1    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cSFlabel2    AS CHAR NO-UNDO.

DEF VAR bPreset AS LOG NO-UNDO.                     

  /* Find searchcrit: 
     Priority, 1st field:
        1: Char field containing "na" (name, navn)
        2: Unique char field 
        3: Char field 
        4: Unique other field
        5: Other field 
     Priority, 2nd field (must be different from 1st field)
        1: Unique char field 
        2: Char field 
        3: Other field */
        
  bPreset = FALSE.
  FOR EACH idx WHERE idx.bSearchOn NO-LOCK,
      FIRST tt_field WHERE :
    IF bPreset THEN DO:
      ASSIGN cSearchF2 = idx.ix-felt
             cSFtype2  = idx.cData-type
             csFlabel2 = idx.ix-felt.
      LEAVE.
    END.
    ELSE
      ASSIGN cSearchF1 = idx.ix-felt
             cSFtype1  = idx.cData-type
             csFlabel1 = idx.ix-felt
             bPreset   = TRUE.
  END.
  IF bPreset THEN RETURN.

  FOR EACH tt_index OF tt_file WHERE tt_index._active NO-LOCK,
      FIRST tt_index-field OF tt_index NO-LOCK,
      FIRST tt_field OF tt_index-field WHERE tt_field._data-type = "character" AND
            tt_field._field-name MATCHES "*na*" NO-LOCK:
      
    ASSIGN cSearchF1 = tt_field._field-name
           cSFtype1  = tt_field._data-type.
    IF tt_field._label NE ? AND tt_field._label NE "" THEN 
      ASSIGN cSFlabel1 = tt_field._label.
    ELSE 
      ASSIGN cSFlabel1 = tt_field._field-name.
  END.
  IF cSearchF1 = "" THEN
    FOR EACH tt_index OF tt_file WHERE tt_index._unique AND tt_index._active NO-LOCK,
        FIRST tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field OF tt_index-field WHERE tt_field._data-type = "character" NO-LOCK:
      
      ASSIGN cSearchF1 = tt_field._field-name
             cSFtype1  = tt_field._data-type.
      IF tt_field._label NE ? AND tt_field._label NE "" THEN 
        ASSIGN cSFlabel1 = tt_field._label.
      ELSE 
        ASSIGN cSFlabel1 = tt_field._field-name.
    END.
  IF cSearchF1 = "" THEN
    FOR EACH tt_index OF tt_file WHERE tt_index._active NO-LOCK,
        FIRST tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field OF tt_index-field WHERE tt_field._data-type = "character" NO-LOCK:
      
      ASSIGN cSearchF1 = tt_field._field-name
             cSFtype1  = tt_field._data-type.
      IF tt_field._label NE ? AND tt_field._label NE "" THEN 
        ASSIGN cSFlabel1 = tt_field._label.
      ELSE 
        ASSIGN cSFlabel1 = tt_field._field-name.
    END.
  IF cSearchF1 = "" THEN
    FOR EACH tt_index OF tt_file WHERE tt_index._unique AND tt_index._active NO-LOCK,
        FIRST tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field OF tt_index-field NO-LOCK:
      
      ASSIGN cSearchF1 = tt_field._field-name
             cSFtype1  = tt_field._data-type.
      IF tt_field._label NE ? AND tt_field._label NE "" THEN 
        ASSIGN cSFlabel1 = tt_field._label.
      ELSE 
        ASSIGN cSFlabel1 = tt_field._field-name.
    END.
  IF cSearchF1 = "" THEN
    FOR EACH tt_index OF tt_file WHERE tt_index._active NO-LOCK,
        FIRST tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field OF tt_index-field NO-LOCK:
      
      ASSIGN cSearchF1 = tt_field._field-name
             cSFtype1  = tt_field._data-type.
      IF tt_field._label NE ? AND tt_field._label NE "" THEN 
        ASSIGN cSFlabel1 = tt_field._label.
      ELSE 
        ASSIGN cSFlabel1 = tt_field._field-name.
    END.

  /* 2nd searchcrit: */
  FOR EACH tt_index OF tt_file WHERE tt_index._unique AND tt_index._active NO-LOCK,
      FIRST tt_index-field OF tt_index NO-LOCK,
      FIRST tt_field OF tt_index-field WHERE tt_field._field-name NE cSearchF1 AND
            tt_field._data-type = "character" NO-LOCK:
      
    ASSIGN cSearchF2 = tt_field._field-name
           cSFtype2  = tt_field._data-type.
    IF tt_field._label NE ? AND tt_field._label NE "" THEN 
      ASSIGN cSFlabel2 = tt_field._label.
    ELSE 
      ASSIGN cSFlabel2 = tt_field._field-name.
  END.
  IF cSearchF2 = "" THEN
    FOR EACH tt_index OF tt_file WHERE tt_index._active NO-LOCK,
        FIRST tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field OF tt_index-field WHERE tt_field._field-name NE cSearchF1 AND
              tt_field._data-type = "character" NO-LOCK:
      
      ASSIGN cSearchF2 = tt_field._field-name
             cSFtype2  = tt_field._data-type.
      IF tt_field._label NE ? AND tt_field._label NE "" THEN 
        ASSIGN cSFlabel2 = tt_field._label.
      ELSE 
        ASSIGN cSFlabel2 = tt_field._field-name.
    END.
  IF cSearchF2 = "" THEN
    FOR EACH tt_index OF tt_file WHERE tt_index._active NO-LOCK,
        FIRST tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field OF tt_index-field WHERE tt_field._field-name NE cSearchF1 NO-LOCK:
      
      ASSIGN cSearchF2 = tt_field._field-name
             cSFtype2  = tt_field._data-type.
      IF tt_field._label NE ? AND tt_field._label NE "" THEN 
        ASSIGN cSFlabel2 = tt_field._label.
      ELSE 
        ASSIGN cSFlabel2 = tt_field._field-name.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenProcForExport C-Win 
PROCEDURE GenProcForExport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
/* Generate procedure for export: */

   
DEF VAR ix          AS INT NO-UNDO.
DEF VAR ix1         AS INT NO-UNDO.

PUT UNFORMATTED "PROCEDURE Export-data:" SKIP.
PUT UNFORMATTED "DO WITH FRAME f1:" SKIP.
PUT UNFORMATTED "  MESSAGE 'File name: ' UPDATE cExpFile." SKIP.
PUT UNFORMATTED "  IF cExpFile NE '' THEN OUTPUT TO VALUE(cExpFile)."  SKIP.
PUT UNFORMATTED "  ELSE RETURN."  SKIP.
PUT UNFORMATTED "  bOK = SESSION:SET-WAIT-STATE('General')." SKIP.    
/* 
IF tbRrowseSelected THEN DO WITH FRAME {&FRAME-NAME}:
  PUT UNFORMATTED "  FIND FIRST _file WHERE _file._file-name = '" tt_file._file-name "' NO-LOCK NO-ERROR." SKIP.
  PUT UNFORMATTED "  IF AVAIL _file THEN DO:" SKIP.
  DO ix = 1 TO browse-2:NUM-SELECTED-ROWS:
    bOK = browse-2:FETCH-SELECTED-ROW(ix).
    IF bOK THEN DO:
      PUT UNFORMATTED "      PUT UNFORMATTED IF _field._label NE '' AND _field._label NE ? THEN _field._label ELSE _field._field-name." SKIP.
      PUT UNFORMATTED "      PUT "";""." SKIP.
    END.
    ASSIGN ix1 = ix.
  END.
END.
ELSE DO:    
*/  
  PUT UNFORMATTED "  FIND FIRST _file WHERE _file._file-name = '" tt_file._file-name "' NO-LOCK NO-ERROR." SKIP.
  PUT UNFORMATTED "  IF AVAIL _file THEN DO:" SKIP.
  PUT UNFORMATTED "    FOR EACH _field OF _file NO-LOCK BY _field._order:" SKIP.
  PUT UNFORMATTED "      PUT UNFORMATTED IF _field._label NE '' AND _field._label NE ? THEN _field._label ELSE _field._field-name." SKIP.
  PUT UNFORMATTED "      PUT "";""." SKIP.
  PUT UNFORMATTED "    END." SKIP.
  PUT UNFORMATTED "    PUT SKIP." SKIP.
  PUT UNFORMATTED "    FOR EACH _field OF _file NO-LOCK BY _field._order:" SKIP.
  PUT UNFORMATTED "      PUT UNFORMATTED '(' + _field._field-name + ')'." SKIP.
  PUT UNFORMATTED "      PUT "";""." SKIP.
  PUT UNFORMATTED "    END." SKIP.
  PUT UNFORMATTED "  END." SKIP.
  PUT UNFORMATTED "  bOK = SESSION:SET-WAIT-STATE('')." SKIP.    
/*   PUT UNFORMATTED "  ELSE RETURN."  SKIP. */
  PUT UNFORMATTED "  PUT SKIP(1)." SKIP.
/*
END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IndexesToExcel C-Win 
PROCEDURE IndexesToExcel :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    FOR EACH idx:
        DELETE idx.
    END.
    
    FOR EACH tt_file NO-LOCK
        WHERE (IF tb-bShowHidden:CHECKED IN FRAME {&FRAME-NAME} THEN
                 tt_file._hidden
                ELSE
                 NOT tt_file._hidden)
       ,EACH tt_index OF tt_file NO-LOCK,
        EACH tt_index-field OF tt_index NO-LOCK,
        FIRST tt_field WHERE tt_field.iFieldNum = tt_index-field.iFieldNum NO-LOCK
        BY tt_file._file-name BY tt_index._index-name
        BY  tt_index-field._index-seq:
    
      CREATE idx.
      ASSIGN idx.Tbl-navn   = tt_file._file-name
             idx.ix-navn    = tt_index._index-name
             idx.ix-felt    = tt_index-field.cFieldName
             idx.cData-type = tt_field._Data-type
             idx.ix-sq      = tt_index-field._index-seq
             idx.ix-asc     = IF tt_index-field._ascending THEN "A" ELSE " "
             idx.ix-kode    = tt_index.cCode
/*              IF RECID(tt_index) = tt_file._prime-index THEN "P"    */
/*                              ELSE " "                              */
/*             idx.ix-kode    = idx.ix-kode +                         */
/*                              IF  tt_index._UNIQUE THEN "U" ELSE " "*/
             .
    END.
   
  RUN ToExcelViaFile.p (hbIdx,0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWin C-Win 
PROCEDURE InitWin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ixFileNum  AS INT NO-UNDO.
DEF VAR ixIdxNum   AS INT NO-UNDO.
DEF VAR ixFieldNum AS INT NO-UNDO.

checkForResizeLib(SESSION:FIRST-PROCEDURE).

IF NOT bResizeLibLoaded THEN DO:
  RUN ResizeLib.p PERSIST SET hResizeLib.
  SESSION:ADD-SUPER-PROC(hResizeLib).
END.

DYNAMIC-FUNCTION("setSaveSettingName","dictview").
DYNAMIC-FUNCTION("setNoMoveX"  ,THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "fi-cTable,cb-cDB,tb-bShowHidden,RECT-65,t-feltrekkef,tbOnlyMandatory,cb-cType").
DYNAMIC-FUNCTION("setAddMoveX" ,THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-66,RECT-67,b-kopier,tb-bLC,t-kvalifiser,v-ant-pr-linje,b-innhold,tbRrowseSelected,fi-cSearch,btnSearch,tbMandatory,w-decimals,w-extent,w-initial,w-order").
DYNAMIC-FUNCTION("setNoMoveY"  ,THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "fi-cHelpLab,fi-cDescLab,fi-cViewAsLab,fi-cIdxLab,w-help,w-descr,w-view-as,browse-2,BROWSE-3,TEXT-5,TEXT-4,TEXT-3,TEXT-2,fi-cTable,fi-cDB,tb-bShowHidden,RECT-65").
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "browse-1,browse-2,RECT-66,RECT-67").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "browse-1,w-help,w-descr,w-view-as,RECT-65,RECT-66,RECT-67").
DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,
                  STRING(FRAME frSplitBarY:HANDLE) + "," +
                  STRING(browse-1:HANDLE) + "," + 
                  STRING(browse-2:HANDLE) + "," + 
                  STRING(browse-3:HANDLE) + "," + 
                  STRING(btnSave:HANDLE) + "," + 
                  STRING(fi-cDescLab:HANDLE) + "," + 
                  STRING(fi-cHelpLab:HANDLE) + "," + 
                  STRING(fi-cIdxLab:HANDLE) + "," + 
                  STRING(fi-cViewAsLab:HANDLE) + "," + 
                  STRING(w-col-label:HANDLE) + "," + 
                  STRING(w-data-type:HANDLE) + "," + 
                  STRING(w-descr:HANDLE) + "," + 
                  STRING(w-format:HANDLE) + "," + 
                  STRING(w-help:HANDLE) + "," + 
                  STRING(w-label:HANDLE) + "," + 
                  STRING(w-view-as:HANDLE)
/*                   STRING(tbMandatory:HANDLE) + "," + */
/*                   STRING(w-decimals:HANDLE) + "," +  */
/*                   STRING(w-extent:HANDLE) + "," +    */
/*                   STRING(w-initial:HANDLE) + "," +   */
/*                   STRING(w-order:HANDLE)             */
                  ).
DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frSplitBarY,NO).
DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frSplitBarY,
                  STRING(browse-1:HANDLE) + "," + STRING(browse-2:HANDLE)).
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,800,152,0,0).

DO WITH FRAME {&FRAME-NAME}:
  bOK = SESSION:SET-WAIT-STATE("General").
  REPEAT ix = 1 TO NUM-DBS:
    cb-cDB:ADD-LAST(LDBNAME(ix)).
    DELETE ALIAS dictdb.
    CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(ix)).
    RUN CreTempTableFile.p (LDBNAME(ix), 
                            INPUT-OUTPUT ixFileNum,
                            INPUT-OUTPUT ixIdxNum,
                            INPUT-OUTPUT ixFieldNum).
  END.
  cb-cDB:SCREEN-VALUE = ENTRY (1, cb-cDB:LIST-ITEMS).
  CREATE ttSearch.
  APPLY "VALUE-CHANGED" TO cb-cDB.  
  bOK = SESSION:SET-WAIT-STATE("").
  tt_field._mandatory:READ-ONLY IN BROWSE browse-2 = TRUE.
  tt_file._file-name:READ-ONLY  IN BROWSE browse-1 = TRUE.
END.

APPLY "window-resized" TO {&WINDOW-NAME}.

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
{&WINDOW-NAME}:WINDOW-STATE = 3.
bOK = {&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "ENTRY" TO fi-cTable IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryField C-Win 
PROCEDURE OpenQueryField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cb-cType tbOnlyMandatory.

  hQuery = browse-2:QUERY.
  cQueryString = "FOR EACH tt_field WHERE tt_field.iFileNum = " + STRING(tt_file.iFileNum) + 
                 (IF cb-cType NE "All" THEN 
                    " AND tt_field._Data-type = '" + LC(cb-cType) + "'"
                  ELSE " AND TRUE ") +
                 (IF tbOnlyMandatory THEN 
                    " AND tt_field._mandatory"
                  ELSE "") + 
                 (IF cSortColumn2 NE "" THEN
                   " BY " + cSortColumn2 + (IF bDesc2 THEN " DESC." ELSE ".")
                  ELSE ".").

   hQuery:QUERY-PREPARE(cQueryString).
   hQuery:QUERY-OPEN().

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryFile C-Win 
PROCEDURE OpenQueryFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR cQueryString AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN fi-cSearch.

  hQuery = browse-1:QUERY.
  cQueryString = "FOR EACH ttSearch, " +
                  "   EACH tt_file " +
                  (IF fi-cSearch:SCREEN-VALUE NE "" THEN
                    "  WHERE tt_file.iFileNum = ttSearch.iFileNum "
                   ELSE
                     "  WHERE tt_file.cLDBname = '" + cb-cDB + "'") +
                  (IF tb-bShowHidden THEN
                    "    AND tt_file._hidden "
                   ELSE
                    "    AND NOT tt_file._hidden ") +
                  (IF cSortColumn1 NE "" THEN
                    " BY " + cSortColumn1 + (IF bDesc1 THEN " DESC." ELSE ".")
                    ELSE ".").


  hQuery:QUERY-PREPARE(cQueryString).
  hQuery:QUERY-OPEN().
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-1.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryIndex C-Win 
PROCEDURE OpenQueryIndex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OPEN QUERY BROWSE-3
     FOR EACH idx NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryForBrowse C-Win 
PROCEDURE QueryForBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iNbColumns  AS INT  NO-UNDO.
DEF VAR bFieldNames AS LOG  NO-UNDO INIT TRUE.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR ix1         AS INT  NO-UNDO.
DEF VAR cOutput     AS CHAR NO-UNDO.
DEF VAR cAllOutput  AS CHAR NO-UNDO.
DEF VAR iNumChars   AS INT  NO-UNDO.

IF NOT tbRrowseSelected THEN DO: 
  FOR EACH btt_field OF tt_file NO-LOCK:
    iNbColumns = iNbColumns + 1.
  END.
  IF iNbColumns > 55 THEN bFieldNames = FALSE.
END.

IF NOT bWithoutLabels THEN DO:
  PUT UNFORMATTED "/* Query and Browse definition: */" SKIP(1).
  PUT UNFORMATTED "DEFINE QUERY q1 FOR " cDbTableName "." SKIP(1).
  PUT UNFORMATTED "DEFINE BROWSE b1 QUERY q1" SKIP.
  PUT UNFORMATTED "  DISPLAY" SKIP.
END.

IF tbRrowseSelected THEN DO WITH FRAME {&FRAME-NAME}:
  DO ix = 1 TO browse-2:NUM-SELECTED-ROWS:
    bOK = browse-2:FETCH-SELECTED-ROW(ix).
    IF bOK THEN DO:
      ASSIGN iFormatLgth = 0
             cEnable     = tt_field._field-name.    
      IF tt_field._data-type = "character" THEN DO: 
        ASSIGN iFormatLgth = INT(SUBSTR(tt_field._format,3,LENGTH(tt_field._format) - 3)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND iFormatLgth > iMaxLgChar THEN 
          ASSIGN iFormatLgth = iMaxLgChar.
      END.

      PUT UNFORMATTED "      " cDbTableName 
                      "." tt_field._field-name 
                      IF iFormatLgth = iMaxLgChar THEN " FORMAT 'x(" + STRING(iMaxLgChar) + ")'" ELSE ' FORMAT "' + tt_field._format + '"'
                      IF bFieldNames THEN " COLUMN-LABEL " ELSE " LABEL " '"'
                      IF tt_field._col-label NE "" THEN tt_field._col-label ELSE tt_field._label
                      IF bFieldNames THEN "!" ELSE ""
                      IF bFieldNames THEN tt_field._field-name ELSE ""
                      '"'.
    END.
    ASSIGN ix1 = ix.
  END.
END.
ELSE DO:
  FOR EACH btt_field OF tt_file NO-LOCK BY btt_field._order:
    ASSIGN iFormatLgth = 0
           cEnable     = btt_field._field-name.    
    IF btt_field._data-type = "character" THEN DO: 
      ASSIGN iFormatLgth = INT(SUBSTR(btt_field._format,3,LENGTH(btt_field._format) - 3)) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND iFormatLgth > iMaxLgChar THEN 
        ASSIGN iFormatLgth = iMaxLgChar.
    END.
    cOutput =  "      " + cDbTableName 
             +  "." + btt_field._field-name 
             +  (IF bFieldNames THEN " COLUMN-LABEL " ELSE " LABEL ") + '"'
             +  (IF NOT bWithoutLabels THEN
                   (IF btt_field._col-label NE "" AND btt_field._col-label NE ? THEN btt_field._col-label 
                    ELSE IF btt_field._label NE ? THEN btt_field._label
                    ELSE "")
                 + (IF bFieldNames THEN "!" ELSE "")
                ELSE "")
             +  btt_field._field-name
             +  '"'.
  
    IF iFormatLgth = iMaxLgChar THEN 
      cOutput = cOutput + " FORMAT 'x(" + STRING(iMaxLgChar) + ")'" + CHR(10).
    ELSE cOutput = cOutput + " FORMAT '" + btt_field._format + "'" + CHR(10).
/*    ELSE cOutput = cOutput + CHR(10).*/

    ASSIGN iNumChars  = iNumChars + LENGTH(cOutput)
           cAllOutput = cAllOutput + cOutput.
    IF iNumChars > 3900 THEN DO:
      IF bWithoutLabels THEN DO:
        MESSAGE "Max length reach for one statement reached" SKIP
                "Not all fields will be visible in browse" SKIP
                "(Use 'Browse selected fields' to get to fields not visible)"
                VIEW-AS ALERT-BOX WARNING.
        bWithoutLabels = FALSE.
        LEAVE.
      END.
      ELSE DO:
        bFieldNames = TRUE.
        bWithoutLabels = TRUE.
        LEAVE.
      END.
    END.
  END.
  IF NOT bWithoutLabels THEN PUT UNFORMATTED cAllOutput.
END.
   
IF NOT bWithoutLabels THEN DO:
  IF bEditMode THEN 
    PUT UNFORMATTED "  ENABLE ALL" SKIP. 
  ELSE   
    PUT UNFORMATTED "  ENABLE " cEnable SKIP. 
  PUT UNFORMATTED "  WITH NO-ROW-MARKERS SEPARATORS SCROLLABLE" SKIP.
  PUT UNFORMATTED "       SIZE 100 BY 13." SKIP(1).
END.
ELSE RUN QueryForBrowse.

bWithoutLabels = FALSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SearchAll C-Win 
PROCEDURE SearchAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSearch AS CHAR NO-UNDO.
DEF VAR bMatch  AS LOG  NO-UNDO.

FOR EACH ttSearch: DELETE ttSearch. END.

IF fi-cSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
    
  ASSIGN cSearch = "*" + fi-cSearch:SCREEN-VALUE + "*".
  
  FOR EACH tt_index WHERE 
      tt_index._index-name MATCHES cSearch 
      NO-LOCK:
    IF NOT CAN-FIND(FIRST ttSearch WHERE ttSearch.iFileNum = tt_index.iFileNum) THEN DO:      
      CREATE ttSearch.
      ASSIGN ttSearch.iFileNum = tt_index.iFileNum
             bMatch = TRUE.
    END.         
  END.
  FOR EACH tt_field WHERE 
      tt_field._field-name MATCHES cSearch 
      NO-LOCK:
    IF NOT CAN-FIND(FIRST ttSearch WHERE ttSearch.iFileNum = tt_field.iFileNum) THEN DO:      
      CREATE ttSearch.
      ASSIGN ttSearch.iFileNum = tt_field.iFileNum
             bMatch = TRUE.
    END.         
  END.
  FOR EACH tt_file WHERE 
      tt_file._file-name MATCHES cSearch 
      NO-LOCK:
    IF NOT CAN-FIND(FIRST ttSearch WHERE ttSearch.iFileNum = tt_file.iFileNum) THEN DO:      
      CREATE ttSearch.
      ASSIGN ttSearch.iFileNum = tt_file.iFileNum
             bMatch = TRUE.
    END.         
  END.
  
END.

IF NOT bMatch THEN CREATE ttSearch.

RUN OpenQueryFile.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TableSearch C-Win 
PROCEDURE TableSearch :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF BUFFER tt_file FOR tt_file.
DEF BUFFER ttSearch FOR ttSearch.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST ttSearch WHERE ttSearch.iFileNum > 0 NO-ERROR.
    IF NOT AVAIL ttSearch THEN DO:
      FIND FIRST ttSearch.
      FIND FIRST tt_file WHERE tt_file._file-name BEGINS fi-cTable:SCREEN-VALUE AND
           tt_file.cLDBname = cb-cDB:SCREEN-VALUE
           NO-LOCK NO-ERROR.
      IF AVAIL tt_file THEN DO:
        REPOSITION browse-1 TO ROWID ROWID(ttSearch), ROWID(tt_file) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          APPLY "VALUE-CHANGED" TO BROWSE BROWSE-1.
      END.
    END.  
    ELSE FOR EACH tt_file 
             WHERE tt_file._file-name BEGINS fi-cTable:SCREEN-VALUE
            ,FIRST ttSearch WHERE ttSearch.iFileNum = tt_file.iFileNum
             :
      REPOSITION browse-1 TO ROWID ROWID(ttSearch), ROWID(tt_file) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        APPLY "VALUE-CHANGED" TO BROWSE BROWSE-1.
      LEAVE.  
    END.              
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkForResizeLib C-Win 
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

