&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports115        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwJBoxEventLogPattern ***/
DEF VAR oBrwJBoxEventLogPattern AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxEventLogPattern
    FIELD cPatternId AS character
    FIELD cPatternDesc AS character
    FIELD cPatternType AS character
    FIELD dCreated AS date
    FIELD cCreatedBy AS character
    FIELD dModified AS date
    FIELD cModifiedBy AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_JBoxEventLogPattern FOR JBoxEventLogPattern.


FUNCTION getBuffersAndFieldsBrwJBoxEventLogPattern RETURNS CHARACTER():
  RETURN
    'JBoxEventLogPattern'
     + ';cPatternId'
     + ';cPatternDesc'
     + ';cPatternType'
     + ';dCreated'
     + ';cCreatedBy'
     + ';dModified'
     + ';cModifiedBy'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxEventLogPattern RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
DEF VAR oFmJBoxEventLogPattern AS JBoxFieldMap NO-UNDO.
DEF VAR otbJBoxEventLogPattern AS JBoxToolbar NO-UNDO.
/*** Start instance property definitions for JBoxBrowse object oBrwJBoxEventLogPatternType ***/
DEF VAR oBrwJBoxEventLogPatternType AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxEventLogPatternType
    FIELD iSeq AS integer
    FIELD cEventLogType AS character
    FIELD cEventLogTypeText AS character
    FIELD cJBoxUserId AS character
    FIELD cUserName AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3
    .
DEF BUFFER v_JBoxEventLogPatternType FOR JBoxEventLogPatternType.


FUNCTION getBuffersAndFieldsBrwJBoxEventLogPatternType RETURNS CHARACTER():
  RETURN
    'JBoxEventLogPatternType'
     + ';iSeq'
  + ',JBoxEventLogType'
     + ';cEventLogType'
     + ';cEventLogTypeText'
     + ';cJBoxUserId'
  + ',JBoxUser'
     + ';cUserName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxEventLogPatternType RETURNS CHARACTER():
  RETURN 'EACH JBoxEventLogType OF JBoxEventLogPatternType NO-LOCK,EACH JBoxUser OF JBoxEventLogType OUTER-JOIN NO-LOCK'.
END FUNCTION.


/*** Start instance property definitions for JBoxBrowse object oBrwJBoxEventLogPatternStatus ***/
DEF VAR oBrwJBoxEventLogPatternStatus AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxEventLogPatternStatus
    FIELD iSeq AS integer
    FIELD cEventStatus AS character
    FIELD cEventStatusText AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_JBoxEventLogPatternStatus FOR JBoxEventLogPatternStatus.


FUNCTION getBuffersAndFieldsBrwJBoxEventLogPatternStatus RETURNS CHARACTER():
  RETURN
    'JBoxEventLogPatternStatus'
     + ';iSeq'
     + ';cEventStatus'
  + ',JBoxEventLogStatus'
     + ';cEventStatusText'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxEventLogPatternStatus RETURNS CHARACTER():
  RETURN 'EACH JBoxEventLogStatus OF JBoxEventLogPatternStatus NO-LOCK'.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwJBoxEventLogPattern

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES JBoxEventLogPattern ~
JBoxEventLogPatternStatus JBoxEventLogPatternType

/* Definitions for BROWSE BrwJBoxEventLogPattern                        */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxEventLogPattern ~
JBoxEventLogPattern.cPatternId JBoxEventLogPattern.cPatternDesc ~
JBoxEventLogPattern.cPatternType JBoxEventLogPattern.dCreated ~
JBoxEventLogPattern.cCreatedBy JBoxEventLogPattern.dModified ~
JBoxEventLogPattern.cModifiedBy 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxEventLogPattern ~
JBoxEventLogPattern.cPatternId 
&Scoped-define QUERY-STRING-BrwJBoxEventLogPattern FOR EACH JBoxEventLogPattern NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxEventLogPattern OPEN QUERY BrwJBoxEventLogPattern FOR EACH JBoxEventLogPattern NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxEventLogPattern JBoxEventLogPattern
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxEventLogPattern JBoxEventLogPattern


/* Definitions for BROWSE BrwJBoxEventLogPatternStatus                  */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxEventLogPatternStatus ~
JBoxEventLogPatternStatus.iSeq JBoxEventLogPatternStatus.cEventStatus ~
JBoxEventLogPatternStatus.cEventStatusText 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxEventLogPatternStatus ~
JBoxEventLogPatternStatus.iSeq 
&Scoped-define QUERY-STRING-BrwJBoxEventLogPatternStatus FOR EACH JBoxEventLogPatternStatus NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxEventLogPatternStatus OPEN QUERY BrwJBoxEventLogPatternStatus FOR EACH JBoxEventLogPatternStatus NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxEventLogPatternStatus ~
JBoxEventLogPatternStatus
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxEventLogPatternStatus JBoxEventLogPatternStatus


/* Definitions for BROWSE BrwJBoxEventLogPatternType                    */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxEventLogPatternType ~
JBoxEventLogPatternType.iSeq JBoxEventLogPatternType.cEventLogType ~
JBoxEventLogPatternType.cEventLogTypeText ~
JBoxEventLogPatternType.cJBoxUserId JBoxEventLogPatternType.cUserName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxEventLogPatternType ~
JBoxEventLogPatternType.iSeq JBoxEventLogPatternType.cEventLogType 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwJBoxEventLogPatternType ~
JBoxEventLogPatternType
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwJBoxEventLogPatternType JBoxEventLogPatternType
&Scoped-define QUERY-STRING-BrwJBoxEventLogPatternType FOR EACH JBoxEventLogPatternType NO-LOCK, ~
    EACH JBoxEventLogType OF JBoxEventLogPatternType NO-LOCK, ~
    EACH JBoxUser OF JBoxEventLogType OUTER-JOIN NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxEventLogPatternType OPEN QUERY BrwJBoxEventLogPatternType FOR EACH JBoxEventLogPatternType NO-LOCK, ~
    EACH JBoxEventLogType OF JBoxEventLogPatternType NO-LOCK, ~
    EACH JBoxUser OF JBoxEventLogType OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxEventLogPatternType ~
JBoxEventLogPatternType
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxEventLogPatternType JBoxEventLogPatternType


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbJBoxEventLogPattern ~
new_tbJBoxEventLogPattern edit_tbJBoxEventLogPattern ~
copy_tbJBoxEventLogPattern undo_tbJBoxEventLogPattern ~
delete_tbJBoxEventLogPattern save_tbJBoxEventLogPattern ~
excel_tbJBoxEventLogPattern selectType_tbJBoxEventLogPattern ~
selectStat_tbJBoxEventLogPattern BrwJBoxEventLogPattern cPatternId ~
cPatternDesc cPatternType BrwJBoxEventLogPatternType ~
BrwJBoxEventLogPatternStatus 
&Scoped-Define DISPLAYED-OBJECTS cPatternId cPatternDesc cPatternType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON copy_tbJBoxEventLogPattern 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbJBoxEventLogPattern 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbJBoxEventLogPattern 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 6 BY 1.52 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON excel_tbJBoxEventLogPattern 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON new_tbJBoxEventLogPattern 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbJBoxEventLogPattern 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON selectStat_tbJBoxEventLogPattern 
     LABEL "Velg status" 
     SIZE 12 BY 1.52.

DEFINE BUTTON selectType_tbJBoxEventLogPattern 
     LABEL "Velg typer" 
     SIZE 11 BY 1.52.

DEFINE BUTTON undo_tbJBoxEventLogPattern 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE cPatternType AS CHARACTER FORMAT "x(12)" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Oppgave","Hendelse" 
     DROP-DOWN-LIST
     SIZE 17 BY 1.

DEFINE VARIABLE cPatternDesc AS CHARACTER FORMAT "x(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1.

DEFINE VARIABLE cPatternId AS CHARACTER FORMAT "x(15)" 
     LABEL "Mønster Id" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE tbJBoxEventLogPattern
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 135.2 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwJBoxEventLogPattern FOR 
      JBoxEventLogPattern SCROLLING.

DEFINE QUERY BrwJBoxEventLogPatternStatus FOR 
      JBoxEventLogPatternStatus SCROLLING.

DEFINE QUERY BrwJBoxEventLogPatternType FOR 
      JBoxEventLogPatternType SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwJBoxEventLogPattern
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxEventLogPattern C-Win _STRUCTURED
  QUERY BrwJBoxEventLogPattern NO-LOCK DISPLAY
      JBoxEventLogPattern.cPatternId COLUMN-LABEL "Mønster id" FORMAT "x(15)":U
      JBoxEventLogPattern.cPatternDesc COLUMN-LABEL "Beskrivelse" FORMAT "x(40)":U
      JBoxEventLogPattern.cPatternType COLUMN-LABEL "Type" FORMAT "x(12)":U
      JBoxEventLogPattern.dCreated COLUMN-LABEL "Opprettet" FORMAT "99/99/9999":U
      JBoxEventLogPattern.cCreatedBy COLUMN-LABEL "Av" FORMAT "X(8)":U
      JBoxEventLogPattern.dModified COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      JBoxEventLogPattern.cModifiedBy COLUMN-LABEL "Av" FORMAT "X(8)":U
  ENABLE
      JBoxEventLogPattern.cPatternId
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 7.71 FIT-LAST-COLUMN.

DEFINE BROWSE BrwJBoxEventLogPatternStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxEventLogPatternStatus C-Win _STRUCTURED
  QUERY BrwJBoxEventLogPatternStatus NO-LOCK DISPLAY
      JBoxEventLogPatternStatus.iSeq COLUMN-LABEL "Sekv" FORMAT "->>9":U
            WIDTH 4.2
      JBoxEventLogPatternStatus.cEventStatus COLUMN-LABEL "Status" FORMAT "x(15)":U
      JBoxEventLogPatternStatus.cEventStatusText COLUMN-LABEL "Status tekst" FORMAT "x(30)":U
            WIDTH 36.6
  ENABLE
      JBoxEventLogPatternStatus.iSeq
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59 BY 6.43
         TITLE "Oppgave/hendelse status i mønster" FIT-LAST-COLUMN.

DEFINE BROWSE BrwJBoxEventLogPatternType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxEventLogPatternType C-Win _STRUCTURED
  QUERY BrwJBoxEventLogPatternType NO-LOCK DISPLAY
      JBoxEventLogPatternType.iSeq COLUMN-LABEL "Sekv" FORMAT "->>9":U
            WIDTH 4.2
      JBoxEventLogPatternType.cEventLogType COLUMN-LABEL "Hendelsestype" FORMAT "x(15)":U
            WIDTH 10.2
      JBoxEventLogPatternType.cEventLogTypeText COLUMN-LABEL "Beskrivelse" FORMAT "x(40)":U
      JBoxEventLogPatternType.cJBoxUserId COLUMN-LABEL "Std. saksbeh" FORMAT "x(30)":U
            WIDTH 32.6
      JBoxEventLogPatternType.cUserName COLUMN-LABEL "Navn" FORMAT "X(40)":U
            WIDTH 4.2
  ENABLE
      JBoxEventLogPatternType.iSeq
      JBoxEventLogPatternType.cEventLogType
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 73 BY 6.43
         TITLE "Oppgave/hendelsestyper for mønster" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbJBoxEventLogPattern AT ROW 1.24 COL 2 WIDGET-ID 8
     edit_tbJBoxEventLogPattern AT ROW 1.24 COL 8.2 WIDGET-ID 22
     copy_tbJBoxEventLogPattern AT ROW 1.24 COL 14.2 WIDGET-ID 10
     undo_tbJBoxEventLogPattern AT ROW 1.24 COL 20.2 WIDGET-ID 12
     delete_tbJBoxEventLogPattern AT ROW 1.24 COL 26.2 WIDGET-ID 14
     save_tbJBoxEventLogPattern AT ROW 1.24 COL 32.2 WIDGET-ID 16
     excel_tbJBoxEventLogPattern AT ROW 1.24 COL 38.2 WIDGET-ID 18
     selectType_tbJBoxEventLogPattern AT ROW 1.24 COL 44.2 WIDGET-ID 20
     selectStat_tbJBoxEventLogPattern AT ROW 1.24 COL 55.2 WIDGET-ID 24
     BrwJBoxEventLogPattern AT ROW 3.05 COL 2 WIDGET-ID 200
     cPatternId AT ROW 3.14 COL 73 COLON-ALIGNED
     cPatternDesc AT ROW 4.24 COL 73 COLON-ALIGNED
     cPatternType AT ROW 5.33 COL 73 COLON-ALIGNED WIDGET-ID 28
     BrwJBoxEventLogPatternType AT ROW 11.14 COL 2 WIDGET-ID 300
     BrwJBoxEventLogPatternStatus AT ROW 11.14 COL 77 WIDGET-ID 400
     tbJBoxEventLogPattern AT ROW 1.14 COL 1.8 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136 BY 16.95 WIDGET-ID 100.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 16.86
         WIDTH              = 136
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 136
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 136
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BrwJBoxEventLogPattern selectStat_tbJBoxEventLogPattern DEFAULT-FRAME */
/* BROWSE-TAB BrwJBoxEventLogPatternType cPatternType DEFAULT-FRAME */
/* BROWSE-TAB BrwJBoxEventLogPatternStatus BrwJBoxEventLogPatternType DEFAULT-FRAME */
ASSIGN 
       tbJBoxEventLogPattern:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel,selectType;Velg typer,selectStat;Velg statusmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxEventLogPattern
/* Query rebuild information for BROWSE BrwJBoxEventLogPattern
     _TblList          = "sports115.JBoxEventLogPattern"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"JBoxEventLogPattern.cPatternId" "Mønster id" "x(15)" "character" ? ? ? ? ? ? yes "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"JBoxEventLogPattern.cPatternDesc" "Beskrivelse" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"JBoxEventLogPattern.cPatternType" "Type" "x(12)" "character" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"JBoxEventLogPattern.dCreated" "Opprettet" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"JBoxEventLogPattern.cCreatedBy" "Av" "X(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"JBoxEventLogPattern.dModified" "Endret" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"JBoxEventLogPattern.cModifiedBy" "Av" "X(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxEventLogPattern */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxEventLogPatternStatus
/* Query rebuild information for BROWSE BrwJBoxEventLogPatternStatus
     _TblList          = "sports115.JBoxEventLogPatternStatus"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"JBoxEventLogPatternStatus.iSeq" "Sekv" "->>9" "integer" ? ? ? ? ? ? yes "" no no "4.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"JBoxEventLogPatternStatus.cEventStatus" "Status" "x(15)" "character" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"JBoxEventLogPatternStatus.cEventStatusText" "Status tekst" "x(30)" "character" ? ? ? ? ? ? no "" no no "36.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxEventLogPatternStatus */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxEventLogPatternType
/* Query rebuild information for BROWSE BrwJBoxEventLogPatternType
     _TblList          = "sports115.JBoxEventLogPatternType"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",, OUTER"
     _FldNameList[1]   > JBoxEventLogPatternType.iSeq
"JBoxEventLogPatternType.iSeq" "Sekv" "->>9" "integer" ? ? ? ? ? ? yes "" no no "4.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > JBoxEventLogPatternType.cEventLogType
"JBoxEventLogPatternType.cEventLogType" "Hendelsestype" "x(15)" "character" ? ? ? ? ? ? yes "" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > JBoxEventLogPatternType.cEventLogTypeText
"JBoxEventLogPatternType.cEventLogTypeText" "Beskrivelse" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > JBoxEventLogPatternType.cJBoxUserId
"JBoxEventLogPatternType.cJBoxUserId" "Std. saksbeh" "x(30)" "character" ? ? ? ? ? ? no "" no no "32.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > JBoxEventLogPatternType.cUserName
"JBoxEventLogPatternType.cUserName" "Navn" "X(40)" "character" ? ? ? ? ? ? no "" no no "4.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxEventLogPatternType */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwJBoxEventLogPattern
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}
{incl/conttrigg.i oBrwJBoxEventLogPattern:BROWSE-HANDLE}  

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
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &IF DEFINED(UIB_is_Running) = 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    SESSION:SET-WAIT-STATE("").
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
    ELSE 
    &ENDIF
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY cPatternId cPatternDesc cPatternType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbJBoxEventLogPattern new_tbJBoxEventLogPattern 
         edit_tbJBoxEventLogPattern copy_tbJBoxEventLogPattern 
         undo_tbJBoxEventLogPattern delete_tbJBoxEventLogPattern 
         save_tbJBoxEventLogPattern excel_tbJBoxEventLogPattern 
         selectType_tbJBoxEventLogPattern selectStat_tbJBoxEventLogPattern 
         BrwJBoxEventLogPattern cPatternId cPatternDesc cPatternType 
         BrwJBoxEventLogPatternType BrwJBoxEventLogPatternStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:

  oBrwJBoxEventLogPattern = NEW JBoxBrowse(brwJBoxEventLogPattern:HANDLE).
  oBrwJBoxEventLogPattern:setNoResizeY().
  oBrwJBoxEventLogPattern:setQuerySort("cPatternId").

  oFmJBoxEventLogPattern = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmJBoxEventLogPattern:updateFields = 'cPatternId,cPatternDesc,cPatternType'.
  oFmJBoxEventLogPattern:primaryKeyFields = 'cPatternId'.   

  oFmJBoxEventLogPattern:BROWSE-OBJECT = oBrwJBoxEventLogPattern.
  otbJBoxEventLogPattern = NEW JBoxToolbar(tbJBoxEventLogPattern:HANDLE).

  oBrwJBoxEventLogPattern:TOOLBAR-OBJECT = otbJBoxEventLogPattern.
  oFmJBoxEventLogPattern:TOOLBAR-OBJECT = otbJBoxEventLogPattern.
  
  oBrwJBoxEventLogPatternType = NEW JBoxBrowse(brwJBoxEventLogPatternType:HANDLE).
  oBrwJBoxEventLogPatternType:setParentBrowseObject(oBrwJBoxEventLogPattern,"cPatternId").
  oBrwJBoxEventLogPatternType:enabledColumns = "iSeq".
  oBrwJBoxEventLogPatternType:enableOnDblClick = yes.
  oBrwJBoxEventLogPatternType:setQuerySort("iSeq").
  
  oBrwJBoxEventLogPatternStatus = NEW JBoxBrowse(brwJBoxEventLogPatternStatus:HANDLE).
  oBrwJBoxEventLogPatternStatus:setParentBrowseObject(oBrwJBoxEventLogPattern,"cPatternId").
  oBrwJBoxEventLogPatternStatus:enabledColumns = "iSeq".
  oBrwJBoxEventLogPatternStatus:enableOnDblClick = yes.
  oBrwJBoxEventLogPatternStatus:setQuerySort("iSeq").

END.
oBrwJBoxEventLogPattern:OpenQuery().

oContainer:setResizeXgroup(50,"brwJBoxEventLogPatternType,brwJBoxEventLogPatternStatus").
oContainer:setMoveXgroup(50,"BrwJBoxEventLogPatternStatus").
oContainer:initResize(300,500).

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectStatRecord C-Win 
PROCEDURE selectStatRecord :
DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList   AS CHAR NO-UNDO.  
  DEF VAR bOk       AS LOG  NO-UNDO.

  /* Uncomment and modify to fetch pre-selected rows from database: */
  cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                "JBoxEventLogPatternStatus,JBoxEventLogStatus",       /* Buffer(list) for query */
                                "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                "WHERE cPatternId = '" + JBoxEventLogPattern.cPatternId + "',FIRST JBoxEventLogStatus NO-LOCK OF JBoxEventLogPatternStatus").  

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "JBoxEventLogStatus"      
                      + ";cEventStatus|Status"  
                      + ";cEventStatusText|Beskrivelse"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "cEventStatus", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF JBoxServerAPI:Instance:CallServerProc("jbadmin_edit_status_pattern.p",JBoxEventLogPattern.cPatternId + "|" + cIdList) THEN
      oBrwJBoxEventLogPattern:displayRecord().
    ELSE JBoxServerAPI:Instance:viewCallMessage().
  END.  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectTypeRecord C-Win 
PROCEDURE selectTypeRecord :
DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  /* Uncomment and modify to fetch pre-selected rows from database: */
  cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                "JBoxEventLogPatternType,JBoxEventLogType",       /* Buffer(list) for query */
                                "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                "WHERE cPatternId = '" + JBoxEventLogPattern.cPatternId + "',FIRST JBoxEventLogType NO-LOCK OF JBoxEventLogPatternType").  

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "JBoxEventLogType"      
                      + ";cEventLogType|Hendelsestype"  
                      + ";cEventLogTypeText|Beskrivelse"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "cEventLogType", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF JBoxServerAPI:Instance:CallServerProc("jbadmin_edit_eventtype_pattern.p",JBoxEventLogPattern.cPatternId + "|" + cIdList) THEN
      oBrwJBoxEventLogPattern:displayRecord().
    ELSE JBoxServerAPI:Instance:viewCallMessage().
  END.  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

