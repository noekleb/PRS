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

DEF VAR bOk                       AS LOG  NO-UNDO.
DEF VAR ix                        AS INT  NO-UNDO.
DEF VAR iReturn                   AS INT  NO-UNDO.
DEF VAR hParent                   AS HANDLE NO-UNDO.
                                  
DEF VAR hParentBuffer             AS HANDLE NO-UNDO.
DEF VAR hParentBrowse             AS HANDLE NO-UNDO.
DEF VAR hBrowse                   AS HANDLE NO-UNDO.
DEF VAR hBuffer                   AS HANDLE NO-UNDO.
DEF VAR hToolbar                  AS HANDLE NO-UNDO.
DEF VAR hToolbarPris              AS HANDLE NO-UNDO.
DEF VAR hViewer                   AS HANDLE NO-UNDO.
DEF VAR hFieldMap                 AS HANDLE NO-UNDO.
DEF VAR hGyldigKodeCol            AS HANDLE NO-UNDO.
DEF VAR hFeilkobletStrekkodeCol   AS HANDLE NO-UNDO.
DEF VAR hKodeCol                  AS HANDLE NO-UNDO.
DEF VAR hArtBilde                 AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame            AS HANDLE NO-UNDO.
DEF VAR iDummy                    AS INT    NO-UNDO.
DEFINE VARIABLE cButikkNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
                                  
DEF VAR hCurrTabProc              AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame             AS HANDLE NO-UNDO.
DEF VAR iCurrTab                  AS INT    NO-UNDO.
DEF VAR iCurrBildNr               AS INT    NO-UNDO.
                                  
DEF VAR hLevAnt                   AS HANDLE NO-UNDO.
DEF VAR hAntall                   AS HANDLE NO-UNDO.
DEF VAR hLinjenr                  AS HANDLE NO-UNDO.
DEF VAR hStrekkode                AS HANDLE NO-UNDO.
DEF VAR fCurrPkSdlId              AS DEC    NO-UNDO.
DEF VAR hButikkNrCol              AS HANDLE NO-UNDO.
DEF VAR hAntallCol                AS HANDLE NO-UNDO.
DEF VAR hGyldigStrekkodeCol       AS HANDLE NO-UNDO.
DEF VAR hPakkeCol                 AS HANDLE NO-UNDO.
DEF VAR bOverstyrKol              AS LOG    NO-UNDO.
DEF VAR bPrevNextInvalidStrekkode AS LOG    NO-UNDO.
DEF VAR bGenEAN                   AS LOG    NO-UNDO.
DEF VAR cTekst                    AS CHAR   NO-UNDO.
DEF VAR bEtikettBekreft           AS LOG    NO-UNDO.
DEF VAR hInstance                 AS INT    NO-UNDO.   
DEF VAR cCL                       AS CHAR   NO-UNDO.
DEF VAR iBrukerType               AS INT    NO-UNDO.
DEF VAR iBrukerButikkNr           AS INT    NO-UNDO.
DEF VAR iHTType                   AS INT    NO-UNDO.
DEF VAR iButikkNr                 AS INT    NO-UNDO.
DEF VAR cOutletLst                AS CHAR   NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

DEF VAR bAllowCreate              AS LOG    NO-UNDO.
DEF VAR hArtBasSok                AS HANDLE NO-UNDO.
DEF VAR hScanProc                 AS HANDLE NO-UNDO.

DEF STREAM Inn.
DEF TEMP-TABLE ttStrekkode NO-UNDO LIKE Strekkode
    FIELD Antall AS DEC
    FIELD OkArt AS LOG
    INDEX Kode Kode.

{windows.i}

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.

{overforing.i &NEW=NEW &SHARED="Shared"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 tbVisFilter rsMottatt tbKorrigerte ~
cmbButikkNr tbLeverte sokLevNr sokLevNamn btnLev sokLevKod sokBeskr ~
btnBlankFilter BrwPkSdlLinje TBPkSdlLinje PrisViewer 
&Scoped-Define DISPLAYED-OBJECTS FI-InfoBox tbVisFilter rsMottatt ~
tbKorrigerte cmbButikkNr tbLeverte sokLevNr sokLevNamn sokLevKod sokBeskr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnLev 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcTotals C-Win 
FUNCTION CalcTotals RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFilterFields C-Win 
FUNCTION setFilterFields RETURNS LOGICAL
  ( INPUT icMode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPrevNextStrekkodeMode C-Win 
FUNCTION setPrevNextStrekkodeMode RETURNS LOGICAL
  ( INPUT ibPrevNextInvalidStrekkode AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlankFilter 
     LABEL "Blank" 
     SIZE 7 BY 1.14.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk hele leverandør-register".

DEFINE BUTTON BUTTON-1 
     LABEL "Overstyr inn/utpris..." 
     SIZE 20.4 BY 1.

DEFINE VARIABLE cmbButikkNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InfoBox AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 61 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTotAntall AS DECIMAL FORMAT "->>>>9":U INITIAL 0 
     LABEL "Tot antall" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotLevert AS DECIMAL FORMAT "->>>>9":U INITIAL 0 
     LABEL "Tot levert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotRest AS DECIMAL FORMAT "->>>>9":U INITIAL 0 
     LABEL "Tot rest" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sokBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskr" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 6.8 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Lev" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE rsMottatt AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alle", 1,
"Kun mottatte", 2,
"Kun ikke-mottatte", 3
     SIZE 44.8 BY .71 NO-UNDO.

DEFINE RECTANGLE BrwPkSdlLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 188.4 BY 15.95.

DEFINE RECTANGLE PrisViewer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 151.4 BY 6.19.

DEFINE RECTANGLE TBPkSdlLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE tbKorrigerte AS LOGICAL INITIAL NO 
     LABEL "Kun med korrigert antall" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tbLeverte AS LOGICAL INITIAL NO 
     LABEL "Kun leverte" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.6 BY .81 NO-UNDO.

DEFINE VARIABLE tbVisFilter AS LOGICAL INITIAL NO 
     LABEL "Vis filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-1 AT ROW 2.52 COL 1.8
     FI-InfoBox AT ROW 2.48 COL 23 NO-LABEL
     tbVisFilter AT ROW 2.57 COL 178
     rsMottatt AT ROW 2.62 COL 131.8 NO-LABEL
     fiTotAntall AT ROW 22.43 COL 162.2 COLON-ALIGNED NO-TAB-STOP 
     tbKorrigerte AT ROW 2.57 COL 104
     cmbButikkNr AT ROW 1.29 COL 124 COLON-ALIGNED
     tbLeverte AT ROW 2.57 COL 87.4
     sokLevNr AT ROW 1.24 COL 162 COLON-ALIGNED HELP
          "Leverandør - trykk F3 for å søke blant alle leverandører"
     sokLevNamn AT ROW 1.24 COL 170.2 COLON-ALIGNED NO-LABEL
     btnLev AT ROW 1.24 COL 178.2 NO-TAB-STOP 
     sokLevKod AT ROW 1.29 COL 171.2 COLON-ALIGNED
     sokBeskr AT ROW 1.29 COL 175.8 COLON-ALIGNED
     btnBlankFilter AT ROW 1.24 COL 184
     fiTotLevert AT ROW 23.48 COL 162.2 COLON-ALIGNED NO-TAB-STOP 
     fiTotRest AT ROW 24.52 COL 162.2 COLON-ALIGNED NO-TAB-STOP 
     BrwPkSdlLinje AT ROW 3.62 COL 1.6
     TBPkSdlLinje AT ROW 1.24 COL 1.8
     PrisViewer AT ROW 19.81 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 190.6 BY 25.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 25.14
         WIDTH              = 190.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON btnLev IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-InfoBox IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiTotAntall IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiTotAntall:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       fiTotAntall:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiTotLevert IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiTotLevert:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       fiTotLevert:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiTotRest IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiTotRest:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       fiTotRest:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       sokLevNamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       tbVisFilter:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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


&Scoped-define SELF-NAME btnBlankFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlankFilter C-Win
ON CHOOSE OF btnBlankFilter IN FRAME DEFAULT-FRAME /* Blank */
DO:
  setFilterFields("blank").
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLevBasId AS CHAR NO-UNDO.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn,PkSdlLinje;!PkSdlId",
                      "where true,FIRST PkSdlLinje OF LevBas NO-LOCK WHERE PkSdlId = " + hParentBuffer:BUFFER-FIELD("PkSdlId"):STRING-VALUE,
                      "",
                      "Levnr",
                      OUTPUT cLevBasId,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    sokLevNr:SCREEN-VALUE   = cLevBasId.
    APPLY "Return" TO sokLevnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Overstyr inn/utpris... */
DO:
  RUN d-pksdlparameter.w.

  FIND syspara NO-LOCK WHERE
    syspara.syshid = 22 AND
    syspara.sysgr  = 1 AND
    syspara.paranr = 2 NO-ERROR.
  IF AVAILABLE SysPara THEN
      ASSIGN FI-InfoBox = (IF CAN-DO('1,j,ja,y,yes,true',SysPara.Parameter1) 
                             THEN 'Innpris = lokal'
                             ELSE '').
  FIND syspara NO-LOCK WHERE
    syspara.syshid = 22 AND
    syspara.sysgr  = 1 AND
    syspara.paranr = 1 NO-ERROR.
  IF AVAILABLE SysPara THEN
      ASSIGN FI-InfoBox = FI-InfoBox + 
                          (IF FI-InfoBox = '' THEN '' ELSE ' ') + 
                          (IF CAN-DO('1,j,ja,y,yes,true',SysPara.Parameter1) 
                             THEN 'Utpris = lokal'
                             ELSE '').
  IF FI-InfoBox <> '' THEN FI-InfoBox = 'Overstyring ved import: ' + FI-InfoBox + '.'.
  DISPLAY FI-InfoBox WITH FRAME Default-frame.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbButikkNr C-Win
ON VALUE-CHANGED OF cmbButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  DYNAMIC-FUNCTION("setButikkNr" IN hViewer,cmbButikkNr:SCREEN-VALUE).
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsMottatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsMottatt C-Win
ON VALUE-CHANGED OF rsMottatt IN FRAME DEFAULT-FRAME
DO:
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBeskr C-Win
ON RETURN OF sokBeskr IN FRAME DEFAULT-FRAME /* Beskr */
DO:
  IF sokBeskr:MODIFIED THEN
    RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevKod C-Win
ON RETURN OF sokLevKod IN FRAME DEFAULT-FRAME /* Lev.art.nr */
DO:
  IF sokLevKod:MODIFIED THEN
    RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON F3 OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
DO:
  APPLY "choose" TO btnLev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    RUN SetQueryFilter(YES).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbKorrigerte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbKorrigerte C-Win
ON VALUE-CHANGED OF tbKorrigerte IN FRAME DEFAULT-FRAME /* Kun med korrigert antall */
DO:
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbLeverte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbLeverte C-Win
ON VALUE-CHANGED OF tbLeverte IN FRAME DEFAULT-FRAME /* Kun leverte */
DO:
  RUN SetQueryFilter(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbVisFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbVisFilter C-Win
ON VALUE-CHANGED OF tbVisFilter IN FRAME DEFAULT-FRAME /* Vis filter */
DO:
  setFilterFields(IF tbVisFilter:CHECKED THEN "view" ELSE "hide").
  IF NOT tbVisFilter:CHECKED AND NOT PROGRAM-NAME(2) BEGINS "InitializeObject" THEN
    RUN SetQueryFilter(YES).
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
  IF VALID-HANDLE(hStrekkode)    THEN APPLY "close" TO hStrekkode.
  IF VALID-HANDLE(hArtBasSok)    THEN APPLY "close" TO hArtBasSok.
  IF VALID-HANDLE(hScanProc)     THEN APPLY "close" TO hScanProc.
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
{incl/supptrigg.i hBrowse}

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
DEF INPUT  PARAM ihField  AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

IF ihField:NAME NE "LinjeNr" THEN DO WITH FRAME {&FRAME-NAME}:
/*   CalcTotals(hBrowse,"Antall,AntLevert,AntRest").                                                       */
/*   ASSIGN fiTotAntall:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"localstatvalueAntall")     */
/*          fiTotLevert:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"localstatvalueAntLevert")  */
/*          fiTotRest:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"localstatvalueAntRest")      */
/*          .                                                                                              */
  DYNAMIC-FUNCTION("CalcTotals" IN hViewer).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord C-Win 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN EditRecord.

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

  bOk = FALSE.
  MESSAGE 'Er du sikker på at du vil slette disse varelinjene fra pakkseddelen?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO   TITLE 'Slette pakkseddellinjer' UPDATE bOk.
  IF NOT bOk THEN
      RETURN NO-APPLY.

  RUN SUPER.


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
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

ASSIGN 
    tbVisFilter:HIDDEN IN FRAME Default-Frame = TRUE
    tbVisFilter:SENSITIVE IN FRAME Default-Frame = FALSE
    .

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:     
  IF hParentBuffer:AVAIL AND hBuffer:AVAIL THEN DO:
/*     IF hParentBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE LE 15 THEN DO: */
    
    /* Ikke mottatte pakkseddler. */
    IF hBuffer:BUFFER-FIELD("MottaksId"):BUFFER-VALUE = 0 AND 
       NOT hBuffer:BUFFER-FIELD("Pakke"):BUFFER-VALUE THEN 
    DO:
      IF DYNAMIC-FUNCTION("getLinkedObject",hLevAnt,"browseoverlay","from") = ? THEN
        DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hLevAnt,"AntLevert").
      
      IF VALID-HANDLE(hLinjenr) AND DYNAMIC-FUNCTION("getLinkedObject",hLinjenr,"browseoverlay","from") = ? THEN
        DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hLinjenr,"Linjenr").
      IF DYNAMIC-FUNCTION("getLinkedObject",hLevAnt,"browseoverlay","from") = ? THEN
        DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hLevAnt,"AntLevert").
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledevents","").
    END.
    /* Mottatte pakkseddler */
    ELSE DO:      
      IF DYNAMIC-FUNCTION("getLinkedObject",hLevAnt,"browse","from") NE ? THEN DO:
        DYNAMIC-FUNCTION("DeleteObjectLink",hLevAnt,hBrowse).
        hLevAnt:HIDDEN = YES.
      END.
      IF DYNAMIC-FUNCTION("getLinkedObject",hLinjenr,"browse","from") NE ? THEN DO:
        DYNAMIC-FUNCTION("DeleteObjectLink",hLinjenr,hBrowse).
        hLinjenr:HIDDEN = YES.
      END.
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledevents","NullstillInnlev,SettLevAntTilAnt").
    END.
  END.
END.

RUN SUPER.

IF hParentBuffer:AVAIL THEN
  fCurrPkSdlId = hParentBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE.
ELSE fCurrPkSdlId = 0.
     
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editRecord C-Win 
PROCEDURE editRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN ArtikkelKort IN hViewer.

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
  DISPLAY FI-InfoBox tbVisFilter rsMottatt tbKorrigerte cmbButikkNr tbLeverte 
          sokLevNr sokLevNamn sokLevKod sokBeskr 
      WITH FRAME DEFAULT-FRAME.
  ENABLE BUTTON-1 tbVisFilter rsMottatt tbKorrigerte cmbButikkNr tbLeverte 
         sokLevNr sokLevNamn btnLev sokLevKod sokBeskr btnBlankFilter 
         BrwPkSdlLinje TBPkSdlLinje PrisViewer 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLog C-Win 
PROCEDURE ErrorLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cFileName AS CHAR NO-UNDO.

  DEF VAR pcTekst   AS CHARACTER NO-UNDO.
  DEF VAR cErrorFil AS CHAR      NO-UNDO.

  IF NOT CAN-FIND(FIRST ttStrekkode WHERE ttSTrekkode.ArtikkelNr = 0)
    THEN RETURN.
  ELSE 
  UTSKRIFT_AV_ERR_Logg:
  DO:
    pcTekst = "  Ukjente strekkoder i fil " +  cFileName + '.'. 

    ASSIGN
      cErrorFil = OS-GETENV('TMP') + '\' 
                 + "Error_" 
                 + ENTRY(NUM-ENTRIES(cFileName,'\'),cFileName,'\')
                 + ".Txt".

    OUTPUT TO VALUE(cErrorFil).
    
    PUT UNFORMATTED
        "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
        "Feil i fil: " + cFileName SKIP
        "  " pcTekst SKIP(1)
        .
    FOR EACH ttStrekkode WHERE ttSTrekkode.ArtikkelNr = 0:
      PUT UNFORMATTED ttStrekkode.Kode SKIP.
    END.
  
    OUTPUT CLOSE.
    
    IF SEARCH(cErrorFil) <> ? THEN
    DO:
      RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cErrorFil),
                                  "",
                                  1,                                  OUTPUT hInstance).
    END.    


  END. /* UTSKRIFT_AV_ERR_Logg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtikettRecord C-Win 
PROCEDURE EtikettRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.

bOk = hBuffer:FIND-FIRST("WHERE NOT GyldigStrekkode") NO-ERROR.
IF bOk AND bEtikettBekreft THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
                      "Det fins artikler i pakklisten som ikke har gyldige strekkoder" + CHR(10)
                    + "Vil du registrere disse først?"
                      ,"","").
                      
  IF iReturn = 6 THEN DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
    RUN StrekKodeRecord.
    RETURN.
  END.
  ELSE IF iReturn = 2 THEN RETURN.
END.

bEtikettBekreft = TRUE.

NY_KODE:    
DO:    
    iReturn = 0.
    RUN JBoxBrowseSelectMsg.w ("Etikettutskrift valgte poster?",
        hBrowse:NUM-SELECTED-ROWS,
        IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
        INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
        ELSE 99999,
        OUTPUT iReturn).  /*1=Alle,2=Valgte*/

    IF iReturn = 0 THEN RETURN NO-APPLY.

    IF iReturn = 1 OR hBrowse:NUM-SELECTED-ROWS = 0 THEN /* Alle poster */
      DO:
        IF DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pksdl_etiketter.p","") THEN
           DYNAMIC-FUNCTION("SkrivEtikett" IN hParent, DYNAMIC-FUNCTION("getTransactionMessage")).
      END.
    ELSE DO: /* Valgte poster */
        IF DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pksdl_etiketter.p","") THEN
            DYNAMIC-FUNCTION("SkrivEtikett" IN hParent, DYNAMIC-FUNCTION("getTransactionMessage")).
    END.
END. /* NY_KODE */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelSheetParams C-Win 
PROCEDURE ExcelSheetParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse           AS HANDLE     NO-UNDO.
DEF INPUT PARAM chExcelApplication AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorkbook         AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorksheet        AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM iCount             AS INT        NO-UNDO.

DEF VAR chChart              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheetRange     AS COM-HANDLE NO-UNDO.
DEF VAR cRange               AS CHAR       NO-UNDO.
DEF VAR cAccumFields         AS CHAR       NO-UNDO.
DEF VAR cDistinctFields      AS CHAR       NO-UNDO.
DEF VAR cYtext               AS CHAR       NO-UNDO.
DEF VAR iRange               AS INT        NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

/* chWorkSheet:PageSetup:Zoom = 80.  */
chWorkSheet:PageSetup:CenterHeader = "Pakkseddel " + hParentBuffer:BUFFER-FIELD("PkSdlNr"):STRING-VALUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnStrekkode C-Win 
PROCEDURE FinnStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icStrekkode AS CHAR NO-UNDO.

DEF VAR cArtNrStr AS CHAR NO-UNDO.

cArtNrStr = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Kode = '" + icStrekkode + "'","ArtikkelNr,StrKode").
IF cArtNrStr NE "" THEN DO:
  bOK = hBuffer:FIND-FIRST("WHERE ArtikkelNr = " + ENTRY(1,cArtNrStr,"|") + " AND StrKode = " + ENTRY(2,cArtNrStr,"|")) NO-ERROR.
  IF bOk THEN DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      APPLY "value-changed" TO hBrowse.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPDAFilRecord C-Win 
PROCEDURE getPDAFilRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.
DEF VAR cLookupValue AS CHAR NO-UNDO.

DEF VAR iReturn     AS INT    NO-UNDO.
DEF VAR cVareNr     AS CHAR   NO-UNDO.
DEF VAR cDummy      AS CHAR   NO-UNDO.
DEF VAR cButList    AS CHAR   NO-UNDO.
DEF VAR cFileName   AS CHAR   NO-UNDO.
DEF VAR cKatalog    AS CHAR   NO-UNDO.
DEF VAR cFilter     AS CHAR   NO-UNDO.
DEF VAR cFilNavn AS CHAR   NO-UNDO.
DEF VAR cPrefix     AS CHAR NO-UNDO.

DO  WITH FRAME DEFAULT-FRAME:
    bOk = hBuffer:FIND-FIRST("WHERE MottaksId > '0'") NO-ERROR.
    IF bOk THEN
    DO:
        MESSAGE 'Pakkseddelen er allerede innlevert.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    IF cmbButikkNr:SCREEN-VALUE = ? OR 
       INT(cmbButikkNr:SCREEN-VALUE) = 0 THEN 
    DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke importere fil hvis ikke butikk er valgt","","").
      RETURN NO-APPLY.
    END.

    RUN JBoxDLookup.w ("HT-Type;TypeId;Betegnelse", 
                         "WHERE HTAktiv = TRUE",
                         INPUT-OUTPUT cLookupValue).
    IF cLookupValue NE "" THEN 
      ASSIGN 
        iHTType = INT(ENTRY(1,cLookupValue,"|")).
    ELSE DO:
        bOk = FALSE.
        RETURN.
    END.
    
    IF NOT CAN-DO("6,7,10,12",STRING(iHTType)) THEN
    DO:
        MESSAGE 'Import mot pakkseddel kan bare gjøres for håndterminaltype 6 (Symbol PPT8800),  7 (BxMobile Gml),10 (BxMobile) og 12 (CubComm).' SKIP
            'Valgt håndterminaltype er ' iHtType '.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        bOk = FALSE.
        RETURN.
    END.

    cFileName = DYNAMIC-FUNCTION("getFieldValues","HT-Type","WHERE TypeId = '" + STRING(iHTType) + "'","FilPrefix").
    cPreFix   = cFileName.
    IF cFileName = '' THEN
    DO:
        MESSAGE 'Filnavn ikke satt opp i håndterminalregisteret.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        bOk = FALSE.
        RETURN.
    END.
    cKatalog = DYNAMIC-FUNCTION("getFieldValues","HT-Type","WHERE TypeId = '" + STRING(iHTType) + "'","ImportKatalog").
    IF cKatalog = '' THEN
    DO:
        MESSAGE 'Importkatalog ikke satt opp i håndterminalregisteret.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        bOk = FALSE.
        RETURN.
    END.


END. /* DEFAULT-FRAME */


/* Setter opp filmaske m.m. */
ASSIGN
    cKatalog = RIGHT-TRIM(cKatalog,'\')
    cFilter  = cFilNavn + '*.' + cmbButikkNr:SCREEN-VALUE
    .
/* Sikrer at backup katalog finnes. */
OS-CREATE-DIR value(cKatalog + "~\bku").

/* Hvis det ligger en Inv.Dat, skal denne først konverteres til PPT880 formatet.            */
/* vpiartbas_importer_pricat_fil.p kaller opp xbxmobileinvinnles.p som gjør konverteringen. */
bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_importer_pricat_fil.p"
                      ,'917;' + cKatalog + '\inv.dat').

bOk = FALSE.
/* Henter VPI fil fra disk */
SYSTEM-DIALOG GET-FILE cFileName 
              FILTERS "Håndterminalfiler: " + cFilter cFilter 
              INITIAL-DIR cKatalog
              RETURN-TO-START-DIR
              MUST-EXIST
              UPDATE bOk.

IF bOk = FALSE THEN
    RETURN NO-APPLY.
ELSE 
    MESSAGE "Skal import av håndterminalfil starte?" SKIP
            "Fil som skal importeres:" cFileName
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
IF bOk = FALSE THEN
    RETURN NO-APPLY.

RUN PDAFilImport (cFileName,INT(cmbButikkNr:SCREEN-VALUE),iHTType, OUTPUT bOk).
  

IF NOT bOK THEN
    MESSAGE 'Import av fil feilet. Ingen linjer er innlest.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE DO:           
  /* Flytter filen til backup katalog. */
  IF SEARCH(cFileName) <> ? THEN
      OS-COPY VALUE(cFileName) 
              VALUE(REPLACE(cFileName,cPreFix,'bku\' + cPrefix)).
  IF SEARCH(REPLACE(cFileName,cPreFix,'bku\' + cPrefix)) <> ? THEN
      OS-DELETE VALUE(cFileName).

  RUN InvokeMethod (hBrowse,'OpenQuery'). 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getStrekkodeStat C-Win 
PROCEDURE getStrekkodeStat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM oiTotAnt      AS INT NO-UNDO.
DEF OUTPUT PARAM oiTotUtenKode AS INT NO-UNDO.

DEF VAR hStatBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR iPrevArtNr   AS INT    NO-UNDO.

CREATE BUFFER hStatBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hStatBuffer).

hQuery:QUERY-PREPARE("FOR EACH " + hStatBuffer:NAME + " BY ArtikkelNr").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF hStatBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NE iPrevArtNr THEN DO:
    oiTotAnt = oiTotAnt + 1.
    IF NOT hStatBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE THEN
      oiTotUtenKode = oiTotUtenKode + 1.
  END.

  iPrevArtNr = hStatBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hStatBuffer.
DELETE OBJECT hQuery.

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
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.
DEF VAR hTabFrame   AS HANDLE NO-UNDO.

iBrukerType = int(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType")).
iBrukerButikkNr = int(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")).
IF iBrukerButikkNr = ? THEN
    iBrukerButikkNr = 0.
cCL = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").
cTekst = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                           "WHERE SysHId = 2 and SysGr = 4 and ParaNr = 26","Parameter1").
IF CAN-DO('1,y,j,yes,ja,true',cTekst) THEN
    bGenEAN = TRUE.
ELSE
    bGenEAN = FALSE.

cOutletLst = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                           "WHERE SysHId = 22 and SysGr = 5 and ParaNr = 2","Parameter1").
    
bAllowCreate = DYNAMIC-FUNCTION("getAttribute",SESSION,"allowPkSdlCreate") = "yes".

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN FI-InfoBox = (IF CAN-DO('1,j,ja,y,yes,true',DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 22 and SysGr = 1 and ParaNr = 2","Parameter1")) 
                             THEN 'Innpris = lokal'
                             ELSE '').
  ASSIGN FI-InfoBox = FI-InfoBox + 
                      (IF FI-InfoBox = '' THEN '' ELSE ' ') + 
                      (IF CAN-DO('1,j,ja,y,yes,true',DYNAMIC-FUNCTION("getFieldValues","SysPara",
                         "WHERE SysHId = 22 and SysGr = 1 and ParaNr = 1","Parameter1")) 
                         THEN 'Utpris = lokal'
                         ELSE '').
  IF FI-InfoBox <> '' THEN FI-InfoBox = 'Overstyring ved import: ' + FI-InfoBox + '.'.
  
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN cmbButikkNr:DELIMITER = "|"
         bOverstyrKol = DYNAMIC-FUNCTION("getOverstyrKol" IN hParent).

  APPLY "value-changed" TO tbVisFilter.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           BrwPkSdlLinje:HANDLE,      
                           1000,                      
                           "multiple",                       
                           "PkSdlLinje"   
                           + ";ButikkNr|Butikk"
                           + ";Linjenr|Lnr|>>9"
                           + ";Pakke|PK|*/"
                           + ";ArtikkelNr"
                           + ";LevKod"
                           + ";Beskr"
                           + ";LevFargKod"
                           + ";Antall|Antall|->>>>9"
                           /*
                           + ";!+AntBest|DECIMAL|->>>>9|pksdllinje_ant_bestilt|Ant.best"
                           + ";!+AntBestRest|DECIMAL|->>>>9|pksdllinje_ant_Antall|Antall"
                           */
                           + ";AntLevert|Levert|->>>>9"
                           + ";AntRest|Rest|->>>9"
                           + ";+GyldigStrekkode|LOGICAL|*/|pksdllinje_gyldig_kode|Gyldig"
                           + ";Kode|Strekkode"
                           + ";+FeilkobletStrekkode|LOGICAL|Rydd/|pksdllinje_feilkoblet_kode|Rydd"
                           + ";!LevNr"                           
                           + ";!BrukerId"
                           + ";!PkSdlId"
                           + ";!PkSdlLinjeId"
                           + ";!StrKode"
                           + ";OrdreNr|OrdreNr|->>>>>>>>>>>>9"
                           + ";BestNr|BestNr|->>>>>>>>>>>>9"
                           + ";EDato"
                           + ";!+EndrTid|CHARACTER|x(5)|jbhhmm_time(ETid)|Tid"
                           + ";!ETid"
                           + ";+PkSdlOpphav|CHAR|x(5)|pksdllinje_opphav|Opphav"
                         + ",LevBas"
                           + ";LevNamn|Lev.navn@15"
                         + ",StrKonv"
                           + ";+Str|CHARACTER|x(12)|strkonv_storl@8"
                         + ",buf1_ArtBas;!ArtikkelNr;!BildNr"
                         + ",PkSdlMottak"
                           + ";!MottaksId"
                           + ";!MottattDato"
                           + ";!+iMottattTid|CHARACTER|x(5)|jb_hhmm(MottattTid)|Kl"
                           + ";!BrukerId|Av"
                          ,"WHERE false"
                         + ",FIRST LevBas OF PkSdlLinje NO-LOCK OUTER-JOIN"
                         + ",FIRST StrKonv OF PkSdlLinje NO-LOCK OUTER-JOIN"
                         + ",FIRST buf1_ArtBas OF PkSdlLinje NO-LOCK OUTER-JOIN"
                         + ",FIRST PkSdlMottak OF PkSdlLinje NO-LOCK OUTER-JOIN"
                           ,"sort|PkSdlLinjeId").         
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"sortmap","EndrTid;ETid").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","pksdllinje_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"PostUpdateProc","pksdllinje_post_update.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customUpdateValProc","=pksdllinje_update.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"viewErrorsFromProcessRows","no").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                  ,"MultiSortBrowse;Sorter på flere kolonner"
                 + ",NullstillInnlev;Sett levert antall til 0 for varer"
                 + ",SettLevAntTilAnt;Sett levert antall til antall på pakkseddel for varer"
                 + (IF bAllowCreate THEN ",Delete;Slett" ELSE "")
                  ,"").

  IF bOverstyrKol THEN
    hLinjenr = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"Linjenr","Linjenr"
                              ,"","","","").
  /*
  hAntall = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"Antall","Antall"
                            ,"","","","").
  DYNAMIC-FUNCTION("setAttribute",hAntall,"refreshRow","yes").
  */
  hLevAnt = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"AntLevert","AntLevert"
                            ,"","","","").
  DYNAMIC-FUNCTION("setAttribute",hLevAnt,"refreshRow","yes").

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            TBPkSdlLinje:HANDLE,
                            "",
                            "Delete"
                          + ",edit;A&rtikkelkort"
                          + ",excel"
                          + (IF bOverstyrKol THEN ",browseconfig" ELSE "")
                          + ",Etikett;Eti&kett;Utskrift av etiketter for mottatte varer"
                          + ",Innlev;Innlever"
                          + ",Strekkode"
                          + ",Rule"
                          + ",Saner;&Rydd;Rydd opp"
                          + ",Rule"
                          + ",NyArt;Ny artikkel;Opprett ny artikkel¤enable"
                          + ",NewArt;Hent artikkel;Legg til artikkel¤enable"
                          + ",Scan¤enable" 
                          /*+ ",Rule"
                          + ",getPDAFil;Hent PDA fil¤enable"*/
                           ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  IF bAllowCreate THEN DO:
    cmbButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" +
            DYNAMIC-FUNCTION("getFieldList","Butiker;Butik|Butnamn;Butik","WHERE true"),"|").
    cmbButikkNr:SCREEN-VALUE = (IF iBrukerButikkNr > 0 THEN STRING(iBrukerButikkNr) ELSE cCL).
    cmbButikkNr:SENSITIVE = (IF (iBrukerButikkNr > 0 AND iBrukerType = 2) THEN FALSE ELSE TRUE).
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"allowDeleteKey","yes").
  END.

  hViewer = DYNAMIC-FUNCTION("NewViewer",PrisViewer:HANDLE,hBrowse,"PkSdlPrisView.w").
  FI-InfoBox:SCREEN-VALUE = FI-InfoBox.

  SUBSCRIBE TO "FinnStrekkode" IN hParent.
/*   SUBSCRIBE TO "InnlevRecord"  IN hParent.  */
  SUBSCRIBE TO "setPkslButikkListe" ANYWHERE.
  SUBSCRIBE TO "ExcelSheetParams" ANYWHERE.

END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlevRecord C-Win 
PROCEDURE InnlevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cMsg    AS CHAR NO-UNDO.
DEF VAR piLinjeNr AS INT NO-UNDO.
DEF VAR iFrabut AS INT NO-UNDO.
DEF VAR iBuntNr AS INT NO-UNDO.

IF hLevAnt:MODIFIED THEN
  APPLY "return" TO hLevAnt.

/* Tvang på registrering av EAN koder. */
bOk = hBuffer:FIND-FIRST("WHERE MottaksId > '0'") NO-ERROR.
IF bOk THEN
DO:
    MESSAGE 'Pakkseddelen er allerede innlevert.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Sjekker ordren og oppretter eventuelle manglende ordre og bestillinger. */
bOk = hBuffer:FIND-FIRST("WHERE AntLevert > 0") NO-ERROR.
IF bOk THEN DO:
    DYNAMIC-FUNCTION("ProcessQuery",
                           hBrowse,
                           "pksdl_opprett_ordre.p",
                           STRING(hBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)).
    /* Er det generert bestillinger, må hele spørringen friskes opp. */
    IF DYNAMIC-FUNCTION("getTransactionMessage") = 'KORR' THEN 
    DO:
      DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
      RUN OpenQuery.
    END.
END.

/* Tvang på registrering av EAN koder. */
bOk = hBuffer:FIND-FIRST("WHERE NOT GyldigStrekkode and Pakke = false and AntLevert > 0") NO-ERROR.
IF bOk AND bGenEAN THEN DO:
  MESSAGE "Det fins artikler i pakklisten som ikke har gyldige strekkoder," + CHR(10)
          + "Det må registreres gyldige EAN koder på disse først."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RUN StrekKodeRecord.
  RUN InvokeMethod(hBrowse,"OpenQuery").
  bOk = hBuffer:FIND-FIRST("WHERE NOT GyldigStrekkode and pakke = false and AntLevert > 0") NO-ERROR.
  IF bOk THEN DO:
      MESSAGE "Det ligger fortsatt artikler i pakklisten som ikke har gyldige strekkoder" + CHR(10) 
              + "Varemottak avbrudt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
END.
bOk = FALSE.

IF NUM-ENTRIES(cmbButikkNr:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME},"|") > 4 AND 
   (cmbButikkNr:SCREEN-VALUE = "" OR cmbButikkNr:SCREEN-VALUE = ?) THEN
  cMsg = "NB! Du har ikke valgt butikk og dermed registrers innleveranse og prisoppdatering for alle butikker (i utvalget)" + CHR(10) + CHR(10).
  
cMsg = cMsg + "Skal oppdatering av varemottak startes (husk kontroll av antall og pris først)?".

IF hBrowse:NUM-SELECTED-ROWS > 1 THEN
  cMsg = cMsg + CHR(10) + CHR(10) + "NB! Innleveranse skjer på alle varer i søkekriteriet - uavhengig av markering av rader".

IF DYNAMIC-FUNCTION("DoMessage",0,4,cMsg,"Oppdatering av varemottak","") = 6 THEN 
OPPDATER_VAREMOTTAK:
DO:
  bOk = hBuffer:FIND-FIRST("WHERE NOT GyldigStrekkode") NO-ERROR.
  
  /* Strekkoder må registreres før varemottaket oppdateres - frivillig. */
  IF bOk AND bGenEAN = FALSE THEN DO:
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
                      "Det fins artikler i pakklisten som ikke har gyldige strekkoder" + CHR(10)
                    + "Vil du registrere disse først?"
                      ,"","").
    IF iReturn = 6 THEN DO:
      hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
      RUN StrekKodeRecord.
    END.
  END.
  
  /* Er pakkseddelen kommet fra sentrallageret, skal lageret trekkes ned på sentrallageret når det gjøres varemottak i outlet. */ 
  /* Slike pakksedler har opphav 6.                                                                                            */
  bOk = hBuffer:FIND-FIRST("WHERE TRUE") NO-ERROR.
  IF bOk THEN
  BUFFERAVAIL: 
  DO:
    FIND PkSdlHode NO-LOCK WHERE 
        PkSdlHode.PkSdlId = hBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE NO-ERROR.
    IF AVAILABLE PkSdlHode THEN
        FIND FIRST PkSdlLinje OF PkSdlHode NO-ERROR.

    IF AVAILABLE PkSdlHode AND PkSdlHode.PkSdlStatus <> 10 THEN
    DO:
        DYNAMIC-FUNCTION("DoMessage",0,20,
                         "Pakkseddel har feil status. Kan ikke gjøre varemottak på denne pakkseddel.","",""). 
        RETURN.
    END.
    
    /* TN 21/12-17 Utfør priskontroll på utpris. Gjøres ikke for Outlet butikkene. */
    IF AVAILABLE PkSdlLinje AND NOT CAN-DO(cOutletLst,STRING(PkSdlLinje.butikkNr)) THEN 
    DO:
        RUN PkSdlUtPrisKontroll.p (PkSdlHode.PkSdlId).
    END.
    
    /* Finner fra butikken hvis det er en overføring. Hvis ikke kommer varene fra lager 20. */
    IF AVAILABLE PkSdlHode AND 
        NUM-ENTRIES(PkSdlHode.Merknad,CHR(13)) > 1 AND 
        ENTRY(2,PkSdlHode.Merknad,CHR(13)) BEGINS 'Overført fra butikk ' AND 
        PkSdlHode.PkSdlOpphav = 4 THEN 
    DO:
        cTekst  = ENTRY(2,PkSdlHode.Merknad,CHR(13)).
        cTekst  = ENTRY(1,cTekst,'.').
        iFraBut = INT(ENTRY(4,cTekst,' ')).
    END.
    ELSE iFraBut = INT(cCl).

    /* Spesiel behandling av OUTLET og ventelager */
    IF AVAILABLE PkSdlHode AND 
        PkSdlHode.PkSdlStatus = 10 AND 
        CAN-DO(cOutletLst,STRING(PkSdlLinje.butikkNr))  THEN
    OUTLET_BEHANDLING:
    DO:
        IF CAN-DO('4',STRING(PkSdlHode.PkSdlOpphav)) THEN
        OUTLET_MIKS: 
        DO:
            EMPTY TEMP-TABLE TT_Ovbuffer.
            piLinjeNr = 1.
            FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
                FIND StrKonv NO-LOCK WHERE 
                    StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
                FIND PkSdlPris OF PkSdlHode NO-LOCK WHERE 
                     PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
                FIND ArtBas NO-LOCK WHERE
                  ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
                /* Logger overføringstransaksjonen */
                CREATE TT_OvBuffer.
                ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
                       TT_OvBuffer.LinjeNr     = piLinjeNr
                       TT_OvBuffer.ArtikkelNr  = PkSdlLinje.ArtikkelNr
                       TT_OvBuffer.Vg          = ArtBas.Vg   
                       TT_OvBuffer.LopNr       = (IF ArtBas.LopNr = ? THEN 0 ELSE ArtBas.LopNr)
                       TT_OvBuffer.Antall      = PkSdlLinje.AntLevert
                       TT_OvBuffer.Merknad     = "Varemottak pakkseddel"
                       TT_OvBuffer.Storl       = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
                       TT_OvBuffer.TilStorl    = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
                       TT_OvBuffer.Varekost    = PkSdlPris.NyVarekost
                       piLinjeNr               = piLinjeNr + 1
                       /* Setter datoinfo i registrert dato og tid. */
                       TT_OvBuffer.RegistrertDato = TODAY 
                       TT_OvBuffer.RegistrertTid  = TIME
                       TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
                       TT_OvBuffer.ButikkNrFra = iFrabut
                       TT_OvBuffer.ButikkNrTil = PkSdlLinje.ButikkNr        
                       .
            END.
            
            ASSIGN iBuntNr = -2. /* -2 = En overføringsordre pr. bong. Og de markeres som oppdatert. */
            RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                                 0,
                                 "N" + CHR(1) + "Varemottak outlet " + STRING(TODAY) + STRING(TIME,"HH:MM") + CHR(1) + "N",
                                 '',
                                 '',
                                 7).
        END. /* OUTLET_MIKS */
        ELSE iBuntNr = 0.
        
        EMPTY TEMP-TABLE ttPkSdlLinje.
        OPPRETT_TMP:
        FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
          CREATE ttpkSdlLinje.              
          BUFFER-COPY pkSdlLinje TO ttpkSdlLinje.
          cButikkNr = STRING(PkSdlLinje.ButikkNr).
          
          FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
          FIND StrKonv NO-LOCK WHERE 
              StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
            
          /* For å kunne opprette faktura. */    
          IF CAN-DO(cOutletLst,cButikkNr) AND CAN-DO('4',STRING(PkSdlHode.PkSdlOpphav)) THEN 
          DO:
              CREATE tmpOverfor.
              ASSIGN
                tmpOverfor.ArtikkelNr = DEC(PkSdlLinje.ArtikkelNr)
                tmpOverfor.Vg         = ArtBas.Vg
                tmpOverfor.LopNr      = ArtBas.LopNr
                tmpOverfor.FraBut     = iFrabut
                tmpOverfor.TilBut     = PkSdlLinje.ButikkNr
                tmpOverfor.FraStorl   = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
                tmpOverfor.TilStorl   = tmpOverfor.FraStorl 
                tmpOverfor.Antall     = PkSdlLinje.AntLevert
                tmpOverfor.BuntNr     = iBuntNr
                tmpOverfor.OrdreNr    = ''
                tmpOverFor.Rab%       = 0
                tmpOverfor.Kode       = PkSdlLinje.Kode 
                 .
           END.        
        END. /* OPPRETT_TMP */

        /* Er pakkseddelen kommet fra sentrallageret, skal lageret trekkes ned på sentrallageret når det gjøres varemottak i outlet. */  
        /* Det utstedes da samtidig faktura for varemottaket.                                                                        */
        IF CAN-DO('5,6',STRING(PkSdlHode.PkSdlOpphav)) THEN
            DYNAMIC-FUNCTION("processQuery",hBrowse,"pksdl_internsalg.p",
                              DYNAMIC-FUNCTION("getASuserId")).
        /* Er det overført fra en annen butikk til outlet, skal det bare utstedes faktura. 'Fra' butikkens lager skal da ikke røres her. Det er gjort tidligere. */
        ELSE IF CAN-DO(cOutletLst,cButikkNr) AND CAN-DO('4',STRING(PkSdlHode.PkSdlOpphav)) THEN 
            RUN opprettfakturaoverfor.p (OUTPUT iDummy, OUTPUT cTekst).
      /* 4/8-17 Er det mottak av forward eller Stock ordre skal bare faktura utstedes fra butikk 20 til Outlet. */
    END. /* OUTLET_BEHANDLING */

    VENTELAGER_BEHANDLING:
    DO:
      /* Det skal ikke utstedes faktura utstedes da samtidig faktura for varemottaket.                                                           */
      IF CAN-DO('6,7',STRING(PkSdlHode.PkSdlOpphav)) THEN
      DO: 
          EMPTY TEMP-TABLE ttPkSdlLinje.
          FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
            CREATE ttpkSdlLinje.              
            BUFFER-COPY pkSdlLinje TO ttpkSdlLinje.
          END. /* OPPRETT_TMP */
          
          ihBuffer = BUFFER ttpkSdlLinje:HANDLE.              
          RUN pksdl_internsalg.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).
      END.          
    END. /* VENTELAGER_BEHANDLING */
  END. /* BUFFERAVAIL */

  IF NOT DYNAMIC-FUNCTION("processQuery",hBrowse,"pksdl_innlever.p",
                          DYNAMIC-FUNCTION("getASuserId")) THEN 
  DO:
    cMsg = DYNAMIC-FUNCTION("getTransactionMessage").
    IF INDEX(cMsg,CHR(10)) > 0 THEN
      DYNAMIC-FUNCTION("DoMessage",0,20,
                       cMsg,"",""). 
  END.
  ELSE 
      RUN InvokeMethod(hBrowse,"OpenQuery").

END. /* OPPDATER_VAREMOTTAK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn C-Win 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF RETURN-VALUE = "error" THEN
  DYNAMIC-FUNCTION("setWidgetEnter",hBrowse).
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
DEF VAR hWidget AS HANDLE NO-UNDO.
hWidget = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hWidget.

FRAME {&FRAME-NAME}:MOVE-TO-TOP().

RUN MoveToTop IN hViewer.

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewArtRecord C-Win 
PROCEDURE NewArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hArtBasSok) THEN DO WITH FRAME DEFAULT-FRAME:
    /* Tvang på registrering av EAN koder. */
    bOk = hBuffer:FIND-FIRST("WHERE MottaksId > '0'") NO-ERROR.
    IF bOk THEN
    DO:
        MESSAGE 'Pakkseddelen er allerede innlevert.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.


  IF cmbButikkNr:SCREEN-VALUE = ? OR cmbButikkNr:SCREEN-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke legge til artikkel hvis ikke butikk er valgt","","").
    RETURN NO-APPLY.
  END.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  RUN InitializeObject IN hArtBasSok.

  /*DYNAMIC-FUNCTION("setLevNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("LevNr"):BUFFER-VALUE,NO).*/

/*     DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hParentBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).  */
/*     DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hParentBuffer:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE). */
/*     DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).                                             */
/*     DYNAMIC-FUNCTION("setUpdateCurrentRow" IN hArtBasSok,YES).                                          */

END.

RUN MoveToTop IN hArtBasSok.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextRow C-Win 
PROCEDURE nextRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).

IF hBrowse:SELECT-NEXT-ROW() THEN
  APPLY "value-changed" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillInnlevRecord C-Win 
PROCEDURE NullstillInnlevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT  NO-UNDO.
DEF VAR ocValue AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ("Sett levert antall til 0 for varer",
                              hBrowse:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"),
                              "",
/*                               "LOGICAL|Ja/Nei|yes",  */
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn = 2 THEN DO:
  IF NOT DYNAMIC-FUNCTION("processSelectedRows",hBrowse,"pksdl_sett_levant.p","0") THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE 
    RUN InvokeMethod(hBrowse,"OpenQuery").
END.
ELSE IF iReturn = 1 THEN DO:
  IF NOT DYNAMIC-FUNCTION("processQuery",hBrowse,"pksdl_sett_levant.p","0") THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE 
    RUN InvokeMethod(hBrowse,"OpenQuery").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtBas C-Win 
PROCEDURE NyArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER piArtikkelNr AS DEC NO-UNDO.
              
DO  WITH FRAME DEFAULT-FRAME:
    RUN ArtBasSok.w PERSIST SET hArtBasSok.
    RUN InitializeObject IN hArtBasSok.
    RUN settArtikkelNr IN hArtBasSok (STRING(piArtikkelNr)).
    RUN MoveToTop  IN hArtBasSok.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtRecord C-Win 
PROCEDURE NyArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:
      /* Tvang på registrering av EAN koder. */
      bOk = hBuffer:FIND-FIRST("WHERE MottaksId > '0'") NO-ERROR.
      IF bOk THEN
      DO:
          MESSAGE 'Pakkseddelen er allerede innlevert.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.

      IF cmbButikkNr:SCREEN-VALUE = ? OR cmbButikkNr:SCREEN-VALUE = "" THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke legge til artikkel hvis ikke butikk er valgt","","").
        RETURN NO-APPLY.
      END.
      bOk = FALSE.
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN ArtBasVedlikehold.w (THIS-PROCEDURE,"new",OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
      IF VALID-HANDLE(hArtBasSok) THEN RUN MoveToTop  IN hArtBasSok.
      /*THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:
/*   CalcTotals(hBrowse,"Antall,AntLevert,AntRest").                                                       */
/*   ASSIGN fiTotAntall:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"localstatvalueAntall")     */
/*          fiTotLevert:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"localstatvalueAntLevert")  */
/*          fiTotRest:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"localstatvalueAntRest")      */
/*          .                                                                                              */

/*   ASSIGN fiTotAntall:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"statvalueAntall")     */
/*          fiTotLevert:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"statvalueAntLevert")  */
/*          fiTotRest:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",hBrowse,"statvalueAntRest")      */
         .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDAFilImport C-Win 
PROCEDURE PDAFilImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER cFileName AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iButikkNr AS INT  NO-UNDO.
DEF INPUT  PARAMETER iHTType   AS INT  NO-UNDO.  
DEF OUTPUT PARAMETER bOk       AS LOG  NO-UNDO.

DEF VAR piAnt  AS INT  NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.

/* Tømmer tabellen */
FOR EACH ttStrekkode:
    DELETE ttSTrekkode.
END.
                         
DO WITH FRAME DEFAULT-FRAME:
  ASSIGN
      bOk = TRUE.

  INPUT STREAM Inn FROM VALUE(cFileName) NO-ECHO.
  INNLES:
  REPEAT:
      piAnt = piAnt + 1.
      IMPORT STREAM Inn UNFORMATTED cLinje NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
          MESSAGE 'Feil i importfil. Innlesning avbrudt.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          bOk = FALSE.
          INPUT STREAM Inn CLOSE.
          RETURN.
      END.
      CASE iHTType:
        WHEN 6 OR WHEN 10 THEN /*SymbolPPT8800 og BxSentral */ DO:
            FIND ttSTrekkode NO-LOCK WHERE 
                ttSTrekkode.Kode = TRIM(ENTRY(2,cLinje,' ')) NO-ERROR.
            IF NOT AVAILABLE ttStrekkode THEN
            DO:
                CREATE ttStrekkode.
                ASSIGN
                    ttStrekkode.Kode   = TRIM(ENTRY(2,cLinje,' '))
                    ttStrekkode.Antall = INT(ENTRY(9,cLinje,' ')).
                FIND Strekkode NO-LOCK WHERE
                    Strekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                IF AVAILABLE Strekkode THEN
                    ttSTrekkode.ArtikkelNr = StreKkode.ArtikkelNr.
                IF NOT AVAILABLE Strekkode THEN
                DO:
                    FIND FIRST VPIStrekkode NO-LOCK WHERE
                        VPISTrekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                    IF AVAILABLE VPIStrekkode THEN
                        ttStrekkode.ArtikkelNr = DEC(VPIStrekkode.VareNr).
                END.
            END.
            ELSE IF AVAILABL ttStrekkode THEN
                ASSIGN
                  ttSTrekkode.Antall = ttSTrekkode.Antall + INT(ENTRY(9,cLinje,' ')) NO-ERROR.
        END. /* SymbolPPT8800 */
      WHEN 12 THEN /*CubComm*/ DO:
          IF TRIM(ENTRY(1,cLinje,';')) = '1' THEN
          DO:
              FIND ttStrekkode NO-LOCK WHERE 
                  ttStrekkode.Kode = TRIM(ENTRY(3,cLinje,';')) NO-ERROR.
              IF NOT AVAILABLE ttStrekkode THEN
              DO:
                  CREATE ttStrekkode.
                  ASSIGN
                      ttStrekkode.Kode = TRIM(ENTRY(3,cLinje,';')).
                  FIND Strekkode NO-LOCK WHERE
                      Strekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                  IF AVAILABLE Strekkode THEN
                      ttSTrekkode.ArtikkelNr = StreKkode.ArtikkelNr.
                  IF NOT AVAILABLE Strekkode THEN
                  DO:
                      FIND FIRST VPIStrekkode NO-LOCK WHERE
                          VPISTrekkode.Kode = ttSTrekkode.Kode NO-ERROR.
                      IF AVAILABLE VPIStrekkode THEN
                          ttSTrekkode.ArtikkelNr = DEC(VPIStrekkode.VareNr).
                  END.
              END.
              IF AVAILABL ttStrekkode THEN
                  ASSIGN
                    ttSTrekkode.Antall = ttSTrekkode.Antall + INT(ENTRY(10,cLinje,';')) NO-ERROR.
          END.
          ELSE IF TRIM(ENTRY(1,cLinje,';')) = '99' THEN
          DO:
              IF piAnt <> INT(ENTRY(2,cLinje,';')) THEN
              DO:
                  MESSAGE 'Det er innlest ' + STRING(piAnt) + ' linjer. Men filen skal inneholde ' + ENTRY(2,cLinje,';') + '.' + CHR(10) +
                          'Filens innhold samsvarer ikke med sjekksiffer. Filen kan være ødelagt og bør sjekkes.'
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
              END.
          END.
      END. /* CubComm */
      END CASE.

      IF AVAILABLE ttSTrekkode AND ttStrekkode.ArtikkelNr > 0 THEN
      DO:
          RUN SetskjermVerdier (ttStrekkode.Kode).
          DELETE ttStrekkode.
      END.
  END. /* INNLES */
  INPUT STREAM Inn CLOSE.

  /*
  IF CAN-FIND(FIRST ttStrekkode WHERE ttStrekkode.ArtikkelNr > 0) THEN
  DO:
      BYGG_PAKKSEDDEL:
      FOR EACH ttStrekkode WHERE
          ttStrekkode.ArtikkelNr > 0:
          RUN SetskjermVerdier (ttStrekkode.Kode).
      END. /* BYGG_PAKKSEDDEL */
  END.
  */
  IF CAN-FIND(FIRST ttStrekkode WHERE 
              ttStrekkode.ArtikkelNr = 0) THEN
      RUN ErrorLog (cFileName).
END. /* DEFAULT-FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icDir AS CHARACTER  NO-UNDO.

DEF VAR fCurrArtikkelNr AS DEC NO-UNDO.

IF CAN-DO("Prev,Next",icDir) THEN DO:
  IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).

  IF icDir = "Prev" THEN DO:
    fCurrArtikkelNr = hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.
    REPEAT WHILE hBuffer:AVAIL:
      bOk = hBrowse:SELECT-PREV-ROW().
      IF NOT bOk OR 
         (hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NE fCurrArtikkelNr AND
          (IF bPrevNextInvalidStrekkode THEN NOT hBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE ELSE TRUE)
          ) THEN
        LEAVE.
    END.
  END.
  ELSE DO:
    fCurrArtikkelNr = hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.
    REPEAT:
      bOk = hBrowse:SELECT-NEXT-ROW().
      IF NOT bOk OR 
        (hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NE fCurrArtikkelNr AND
         (IF bPrevNextInvalidStrekkode THEN NOT hBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE ELSE TRUE)
         ) THEN
        LEAVE.
    END.
  END.
  APPLY "value-changed" TO hBrowse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prevRow C-Win 
PROCEDURE prevRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
IF hBrowse:SELECT-PREV-ROW() THEN
  APPLY "value-changed" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshQuery C-Win 
PROCEDURE RefreshQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR crRepos     AS CHAR NO-UNDO.
DEF VAR iReposRow   AS INT  NO-UNDO.
DEF VAR iAntUgyldig AS INT  NO-UNDO.
DEF VAR iTotAnt     AS INT  NO-UNDO.

IF NOT hBuffer:AVAIL THEN RETURN.

ASSIGN crRepos = hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
       iReposRow = hBrowse:FOCUSED-ROW.


RUN InvokeMethod(hBrowse,"OpenQuery").
bOk = hBuffer:FIND-FIRST("WHERE RowIdent1 = '" + crRepos + "'") NO-ERROR.
IF bOk THEN DO:
  hBrowse:SET-REPOSITIONED-ROW(iReposRow,"always").  
  hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
  APPLY "value-changed" TO hBrowse.
  IF VALID-HANDLE(hStrekkode) THEN DO:
    RUN getStrekkodeStat(OUTPUT iTotAnt,OUTPUT iAntUgyldig).
    DYNAMIC-FUNCTION("ViewStrekkodeStat" IN hStrekkode,iTotAnt,iAntUgyldig).
  END.
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
/* ASSIGN hGyldigKodeCol:FONT         = iFontWingdings              */
/*        hGyldigKodeCol:FORMAT       = CHR(254) + "/"  + CHR(168)  */
/*        .                                                         */
IF hBuffer:BUFFER-FIELD("MottaksId"):BUFFER-VALUE > 0 THEN
  hAntallCol:BGCOLOR = 11.
ELSE hAntallCol:BGCOLOR = ?.

IF TRIM(hBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE) = 'No' AND 
    TRIM(hBuffer:BUFFER-FIELD("Pakke"):BUFFER-VALUE) = 'No' THEN
  hGyldigStrekkodeCol:BGCOLOR = 12.
ELSE hGyldigStrekkodeCol:BGCOLOR = ?.

IF TRIM(hBuffer:BUFFER-FIELD("FeilkobletStrekkode"):BUFFER-VALUE) = 'Yes' AND 
    TRIM(hBuffer:BUFFER-FIELD("Pakke"):BUFFER-VALUE) = 'No' THEN
  hFeilkobletStrekkodeCol:BGCOLOR = 12.
ELSE hFeilkobletStrekkodeCol:BGCOLOR = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SanerRecord C-Win 
PROCEDURE SanerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.


IF hBuffer:AVAIL THEN DO:
  /* Sjekker ordren og oppretter eventuelle manglende ordre og bestillinger.              */
  /* Er det artikler som er slettet, hentes disse ut fra VPI register hvis de finnes der. */
  DYNAMIC-FUNCTION("ProcessQuery",
                         hBrowse,
                         "pksdl_opprett_ordre.p",
                         STRING(hBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)).
  
  IF hBuffer:BUFFER-FIELD("MottaksId"):BUFFER-VALUE > 0 THEN
  DO:
      MESSAGE 'Varelinjen er innlevert. Kan ikke endres.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

  ELSE IF TRIM(STRING(hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE)) = '' THEN
  DO:
      MESSAGE 'Varelinjen du har markert har ikke strekkode. Kan ikke saneres.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE IF hBuffer:BUFFER-FIELD("FeilkobletSTrekkode"):BUFFER-VALUE = 'Yes' THEN 
  DO:
      bOk = FALSE.
      MESSAGE 'Strekkoden ligger på en annen artikkel. Vil du sanere?' + CHR(10) + CHR(10) + 
              'Sanering rydder og flytter all informasjon (Salg,  lager m.m.) fra gammelt til nytt artikkelkort.' + CHR(10) + CHR(10) +
              'Hvis du ikke sanerer, må antall levert settes til 0 på pakkseddelen og varen leveres inn via direkte varemottak.'
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Behandling av feilkoblet strekkode' UPDATE bOk.
      IF bOk THEN
      DO:
          /* Sanerer artikkelen. */
          RUN SanerArtikkel.p (DYNAMIC-FUNCTION("getFieldList","Strekkode;ArtikkelNr","WHERE Kode = '" + STRING(hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE) + "'"),
                               STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),
                               OUTPUT bOk
                               ).
          DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
          /* Sjekker om også størrelsen på strekkoden må flytte. */
          IF hBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE = 'No' AND hBuffer:BUFFER-FIELD("FeilkobletSTrekkode"):BUFFER-VALUE = 'No' THEN
          DO:
              RUN bibl_flytt_lager_str.p (hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE, INT(hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE)).
              DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
          END.
      END.
  END.
  ELSE IF hBuffer:BUFFER-FIELD("GyldigStrekkode"):BUFFER-VALUE = 'No' AND hBuffer:BUFFER-FIELD("FeilkobletSTrekkode"):BUFFER-VALUE = 'No' THEN 
  DO: 
      bOk = FALSE.
      /* Strekkoden finnes ikke på artikkelen fra før. */
      IF TRIM(DYNAMIC-FUNCTION("getFieldList","Strekkode;ArtikkelNr","WHERE Kode = '" + STRING(hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE) + "'")) = '' THEN 
      DO TRANSACTION:
          /* Strekkoden ligger på samme artikkel, men på en annen størrelse */
          MESSAGE 'Det ligger en ukjent strekkode på pakkseddelen. Denne er nå opprettet på artikkelen.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE 'Ukjent strekkode opprettet'.
          CREATE Strekkode.
          ASSIGN
               Strekkode.kode       = hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE
               Strekkode.kodetype   = 1
               Strekkode.Artikkelnr = hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
               Strekkode.StrKode    = hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE
               NO-ERROR.
          IF ERROR-STATUS:ERROR AND AVAIL Strekkode THEN
              DELETE Strekkode.
          ELSE RELEASE Strekkode.
          DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      END. /* TRANSACTION */
      ELSE DO:
          /* Strekkoden ligger på samme artikkel, men på en annen størrelse */
          MESSAGE 'Strekkoden ligger på en annen størrelse på artikkelen. Vil du flytte strekkode til riktig størrelse?' + CHR(10) + CHR(10) + 
                  'Når strekkoden flyttes til riktig størrelse flyttes ogs salg og lagerverdier med.' + CHR(10) + CHR(10) +
                  'Hvis du ikke flytter, må antall levert settes til 0 på pakkseddelen og varen leveres inn via direkte varemottak.'
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Behandling av strekkode som ligger på feil størrelse' UPDATE bOk.
          IF bOk THEN
          DO: 
              RUN bibl_flytt_lager_str.p (hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE, INT(hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE)).
              DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
          END.
      END.
  END.
  ELSE DO:
      MESSAGE 'Strekkoden på varelinjen du har markert, er ikke feilkoblet. Kan ikke saneres.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanRecord C-Win 
PROCEDURE ScanRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO  WITH FRAME DEFAULT-FRAME:
    /* Tvang på registrering av EAN koder. */
    bOk = hBuffer:FIND-FIRST("WHERE MottaksId > '0'") NO-ERROR.
    IF bOk THEN
    DO:
        MESSAGE 'Pakkseddelen er allerede innlevert.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    IF cmbButikkNr:SCREEN-VALUE = ? OR cmbButikkNr:SCREEN-VALUE = "" THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke legge til artikkel hvis ikke butikk er valgt","","").
      RETURN NO-APPLY.
    END.

    RUN gscannerinput.w (THIS-PROCEDURE).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPkslButikkListe C-Win 
PROCEDURE setPkslButikkListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ifPkSldId AS DEC NO-UNDO.

DEF VAR cTekst AS CHAR NO-UNDO.

IF bAllowCreate AND ifPkSldId NE fCurrPkSdlId THEN
DO WITH FRAME {&FRAME-NAME}:
    cTekst = DYNAMIC-FUNCTION("getFieldList","Butiker;Butik|Butnamn;Butik,PkSdlLinje;",
                              "WHERE true"
                              + ",FIRST PkSdlLinje WHERE PkSdlLinje.ButikkNr = Butiker.Butik AND PkSdlLinje.PkSdlId = " + STRING(ifPkSldId)
                              ).
    IF NUM-ENTRIES(cTekst,'|') > 2 THEN 
        cmbButikkNr:SCREEN-VALUE = '0'.
    ELSE 
        cmbButikkNr:SCREEN-VALUE = (IF iBrukerButikkNr > 0 THEN STRING(iBrukerButikkNr) ELSE cCL).
END.

ELSE IF NOT bAllowCreate AND ifPkSldId NE fCurrPkSdlId THEN 
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" +
            DYNAMIC-FUNCTION("getFieldList","Butiker;Butik|Butnamn;Butik,PkSdlLinje;",
                             "WHERE true"
                           + ",FIRST PkSdlLinje WHERE PkSdlLinje.ButikkNr = Butiker.Butik AND PkSdlLinje.PkSdlId = " + STRING(ifPkSldId))
                             ,"|")
         cmbButikkNr:SCREEN-VALUE = cmbButikkNr:ENTRY(1).

  IF bOverstyrKol THEN DO:
    IF NUM-ENTRIES(cmbButikkNr:LIST-ITEM-PAIRS,"|") < 5 THEN
      hButikkNrCol:VISIBLE = NO NO-ERROR.
    ELSE DO:
      hButikkNrCol:VISIBLE = YES NO-ERROR.
      cmbButikkNr:HIDDEN   = NO.
    END.
  END.
  ELSE DO:
    hButikkNrCol:VISIBLE = YES NO-ERROR.
    cmbButikkNr:HIDDEN   = NO.
  END.

  RUN SetQueryFilter(NO).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetQueryFilter C-Win 
PROCEDURE SetQueryFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibExecute AS LOG NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN rsMottatt.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter", 
                   (IF tbLeverte:CHECKED THEN
                     " AND PkSdlLinje.AntLevert > 0"
                    ELSE "")
                 + (IF tbKorrigerte:CHECKED THEN
                     " AND PkSdlLinje.AntLevert NE PkSdlLinje.Antall"
                    ELSE "")
                 + (IF cmbButikkNr:SCREEN-VALUE NE "0" AND cmbButikkNr:SCREEN-VALUE NE ? THEN
                     " AND PkSdlLinje.ButikkNr = " + cmbButikkNr:SCREEN-VALUE
                    ELSE "")
                 + (IF sokLevKod:SCREEN-VALUE NE "" THEN
                     " AND PkSdlLinje.LevKod BEGINS '" + sokLevKod:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF sokLevNr:SCREEN-VALUE NE "0" THEN
                     " AND PkSdlLinje.LevNr = " + sokLevNr:SCREEN-VALUE
                    ELSE "")
                 + (IF sokBeskr:SCREEN-VALUE NE "" THEN 
                      " AND PkSdlLinje.beskr " 
                      + (IF INDEX(sokBeskr:SCREEN-VALUE,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '")
                      + sokBeskr:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF rsMottatt = 2 THEN
                     " AND PkSdlLinje.MottaksId > 0"
                    ELSE IF rsMottatt = 3 THEN
                     " AND PkSdlLinje.MottaksId = 0"
                    ELSE "")
                   ).

  IF ibExecute THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetskjermVerdier C-Win 
PROCEDURE SetskjermVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM icStrekkode AS CHAR NO-UNDO.

DEF VAR cArtStorl   AS CHAR NO-UNDO.
DEF VAR lArtikkelNr AS DEC  NO-UNDO.
DEF VAR iAntall AS INT NO-UNDO.

iAntall = 1.

FIND FIRST ttStrekkode NO-LOCK WHERE 
     ttSTrekkode.Kode = icStrekkode NO-ERROR.
IF AVAILABLE ttStrekkode THEN 
    iAntall = ttStrekkode.Antall.

FIND Strekkode NO-LOCK WHERE
    Strekkode.Kode = icStrekkode NO-ERROR.
IF NOT AVAILABLE Strekkode THEN
    FIND FIRST VPIStrekkode NO-LOCK WHERE 
    VPIStrekkode.Kode = icStrekkode NO-ERROR.
IF AVAILABLE Strekkode THEN
    lArtikkelNr = Strekkode.ArtikkelNr.
ELSE IF AVAILABLE VPIStrekkode THEN
    lArtikkelNr = DEC(VPIStrekkode.VareNr).

IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = lArtikkelNr) AND 
       CAN-FIND(FIRST VPIArtBas WHERE VPIArtBas.ArtikkelNr = lArtikkelNr) THEN 
       DO:
           FIND FIRST VPIArtBas NO-LOCK WHERE VPIArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
           cTekst = {tbchooseAll.i}.
           RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cTekst + ';' + STRING(lArtikkelNr), 
                             ?, 
                             '', 
                             OUTPUT cTekst, 
                             OUTPUT bOk).      
       END.


cArtStorl = DYNAMIC-FUNCTION("getFieldList","Strekkode;ArtikkelNr,StrKonv;Storl",
                             "WHERE kode = '" + icStrekkode + "',FIRST StrKonv NO-LOCK OF Strekkode").

IF cArtStorl NE ? AND cArtStorl NE '' THEN
  AddStr(DECIMAL(ENTRY(1,cArtStorl,"|")),
         ENTRY(2,cArtStorl,"|"),
         DEC(iAntall),
         "add").
ELSE
  DYNAMIC-FUNCTION("DoMessage",0,0,"Finner ikke strekkode","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setStrekkode C-Win 
PROCEDURE setStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ihBuffer AS HANDLE NO-UNDO.
 
  DEF VAR cParam AS CHAR NO-UNDO.

  ASSIGN
      cParam = STRING(hBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) + '|' + 
               STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE). 
               
  DYNAMIC-FUNCTION("runproc","strekkode_pksdl.p",cParam,ihBuffer).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLevAntTilAntRecord C-Win 
PROCEDURE SettLevAntTilAntRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT  NO-UNDO.
DEF VAR ocValue AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ("Sett levert antall til antall på pakkseddel for varer",
                              hBrowse:NUM-SELECTED-ROWS,
                              DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"),
                              "",
/*                               "LOGICAL|Ja/Nei|yes",  */
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn = 2 THEN DO:
  IF NOT DYNAMIC-FUNCTION("processSelectedRows",hBrowse,"pksdl_sett_levant.p","antall") THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE 
    RUN InvokeMethod(hBrowse,"OpenQuery").
END.
ELSE IF iReturn = 1 THEN DO:
  IF NOT DYNAMIC-FUNCTION("processQuery",hBrowse,"pksdl_sett_levant.p","antall") THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE 
    RUN InvokeMethod(hBrowse,"OpenQuery").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkodeRecord C-Win 
PROCEDURE StrekkodeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iTotAnt     AS INT NO-UNDO.
DEF VAR iAntUgyldig AS INT NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",hParent:CURRENT-WINDOW).
RUN Strekkode.w (THIS-PROCEDURE).
DYNAMIC-FUNCTION("DoLockWindow",?).

/*
IF NOT VALID-HANDLE(hStrekkode) THEN
  RUN Strekkode.w PERSIST SET hStrekkode (THIS-PROCEDURE).

DYNAMIC-FUNCTION("ViewTbPrevNextMode" IN hStrekkode,YES).

RUN getStrekkodeStat(OUTPUT iTotAnt,OUTPUT iAntUgyldig).
DYNAMIC-FUNCTION("ViewStrekkodeStat" IN hStrekkode,iTotAnt,iAntUgyldig).

RUN MoveToTop IN hStrekkode.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateRecord C-Win 
PROCEDURE UpdateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bReturnFocus AS LOG   NO-UNDO.
DEF VAR cStrList     AS CHAR  NO-UNDO.
DEF VAR bOkStr       AS LOG   NO-UNDO.
DEF VAR iLinjenr     AS INT   NO-UNDO.
DEF VAR cEAN         AS CHAR  NO-UNDO.
DEF VAR iStrKode     AS INT   NO-UNDO.
DEF VAR ocReturn     AS CHAR  NO-UNDO.
DEF VAR iAntall      AS INT   NO-UNDO.
DEF VAR cPkSdlId     AS CHAR  NO-UNDO.

DYNAMIC-FUNCTION("setCurrentObject",hToolbar).

bOk = hBuffer:FIND-FIRST("WHERE ArtikkelNr = " + STRING(ifArtikkelNr)
                         + "  AND Str  = '" + TRIM(icStorl) + "'") NO-ERROR.
IF bOk THEN DO:
    hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).

/*   IF ifPlukkAnt NE ? AND ifPlukkAnt > 1 THEN   */
/*     hLevAnt:SCREEN-VALUE = STRING(ifPlukkAnt). */
/*   ELSE                                         */
  
    /*
    MESSAGE 'Start:' SKIP
        'ifArtikkelNr' ifArtikkelNr SKIP
        'icStorl'      icStorl      SKIP
        'ifPlukkAnt'   ifPlukkAnt   SKIP
        'icAction'     icAction     SKIP
        'hBuffer-field:'  hBuffer:BUFFER-FIELD("AntLevert"):BUFFER-VALUE SKIP
        'hLevAnt:screen-value:' hLevAnt:SCREEN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */
  /*hLevAnt:SCREEN-VALUE = STRING(DEC(hLevAnt:SCREEN-VALUE) + ifPlukkAnt).*/
  hLevAnt:SCREEN-VALUE = STRING(DEC(hBuffer:BUFFER-FIELD("AntLevert"):BUFFER-VALUE) + ifPlukkAnt).

  RUN InvokeMethod(hLevAnt,"LeaveBrowseFillIn").
/*   DYNAMIC-FUNCTION("setCurrentObject",hLevAnt). */
/*   RUN LeaveBrowseFillIn.                        */

  RETURN YES.
END.
  
IF icAction = "add" THEN DO WITH FRAME {&FRAME-NAME}:

  IF ifPlukkAnt = 0 OR ifPlukkAnt = ? THEN ifPlukkAnt = 1.

  IF cmbButikkNr:SCREEN-VALUE = ? OR cmbButikkNr:SCREEN-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke legge til artikkel hvis ikke butikk er valgt","","").
    RETURN NO.
  END.
  
  cPkSdlId = STRING(hParentBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE).

  RUN bibl_fixstorl.p (icStorl,?,'',OUTPUT ocReturn,OUTPUT bOk).
  icStorl = ocReturn.
  
  iStrKode = INTEGER(DYNAMIC-FUNCTION("getFieldValues",
                                      "FIRST StrKonv",
                                      "WHERE StrKonv.Storl = '" + icStorl + "'",
                                      "StrKode")).

  cEAN = DYNAMIC-FUNCTION("getFieldValues",
                             "FIRST Strekkode",
                             "WHERE Strekkode.ArtikkelNr = '" + 
                                STRING(ifArtikkelNr) + 
                                "' AND Strekkode.StrKode = '" + STRING(iStrKode) + "'" +
                                " AND NOT Strekkode.Kode BEGINS '02'",  
                             "Kode").
  IF cEAN = ? OR TRIM(cEAN) = '' THEN
      cEAN = DYNAMIC-FUNCTION("getFieldValues",
                              "FIRST Strekkode",
                              "WHERE Strekkode.ArtikkelNr = '" + 
                                     STRING(ifArtikkelNr) + 
                                     "' AND Strekkode.StrKode = '" + STRING(iStrKode) + "'",
                              "Kode").

  IF cEAN = ? THEN cEAN = ''.
  IF cEAN = '' AND bGenEAN THEN
      RUN hentEAN.p (13,2,OUTPUT cEAN).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraFields","ArtikkelNr,PkSdlLinjeId,Storl,AntLevert,Kode,ButikkNr").
  iLinjenr = INTEGER(DYNAMIC-FUNCTION("getFieldValues",
                                      "LAST PkSdlLinje",
                                      "WHERE PkSdlId = '" + cPkSdlId + "'",
                                      "PkSdlLinjeId")).
  IF iLinjenr = ? THEN iLinjenr = 1.
  ELSE iLinjenr = iLinjenr + 1.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraValues",
                   STRING(ifArtikkelNr) + "|"
                 + STRING(iLinjenr) + "|"
                 + icStorl + "|"
                 + STRING(ifPlukkAnt + iAntall) + "|"
                 + STRING(cEAN) + "|"
                 + cmbButikkNr:SCREEN-VALUE
                   ).
  RUN InvokeMethod(hBrowse,"NewRecord").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraFields","").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferExtraValues","").
END.
ELSE 
  APPLY "value-changed" TO hBrowse.
    
RETURN bReturnFocus.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.

IF bOverstyrKol THEN DO:
  ASSIGN hButikkNrCol              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"ButikkNr")
         hButikkNrCol:WIDTH-PIXELS = 30
         .
END.

ASSIGN hColumn              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"LinjeNr")
       hColumn:WIDTH-PIXELS = 30
       hColumn              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"Pakke")
       hColumn:WIDTH-PIXELS = 20
       hColumn:COLUMN-FONT  = 12
       hColumn              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"ArtikkelNr")
       hColumn:WIDTH-PIXELS = 60
       hColumn              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"LevKod")
       hColumn:WIDTH-PIXELS = 60
       hColumn              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"GyldigStrekkode")
       hColumn:COLUMN-FONT  = 12
       hColumn:WIDTH-PIXELS = 30
       hGyldigKodeCol       = hColumn
       hColumn              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"FeilkobletStrekkode")
       hColumn:WIDTH-PIXELS = 40
       hFeilkobletStrekkodeCol = hColumn
       /*hAntallCol           = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"AntBestRest")*/
       hAntallCol           = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"AntLevert")
       hGyldigStrekkodeCol  = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"GyldigStrekkode")
       hPakkeCol            = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"Pakke")
       hKodeCol             = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"Kode")
       hColumn              = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"AntLevert")
       hColumn:COLUMN-FONT  = 14
       .
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcTotals C-Win 
FUNCTION CalcTotals RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR ) : 
/*------------------------------------------------------------------------------
  Purpose:  Sum up listed fields in a local query
    Notes:  If no fields are specified go and get them from the querystatfields attribute
            and then store the result back in the querystatfieldvalues
------------------------------------------------------------------------------*/
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hQueryBuffer  AS HANDLE NO-UNDO.
DEF VAR cStatFields   AS CHAR   NO-UNDO.
DEF VAR fStatValues   AS DEC    NO-UNDO EXTENT 100.
DEF VAR cReturnString AS CHAR   NO-UNDO.
DEF VAR iCount        AS INT    NO-UNDO.

IF icStatFields = "" THEN
  cStatFields = DYNAMIC-FUNCTION("getAttribute",ihBrowseOrQuery,"querystatfields").
ELSE cStatFields = icStatFields.

IF ihBrowseOrQuery:TYPE = "browse" THEN
  hBuffer = ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hBuffer = ihBrowseOrQuery:GET-BUFFER-HANDLE(1).

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  DO ix = 1 TO NUM-ENTRIES(cStatFields):
    fStatValues[ix] = fStatValues[ix] + hQueryBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
  END.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  cReturnString = cReturnString + ENTRY(ix,cStatFields) + "|" + STRING(fStatValues[ix]) + ";".
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"localstatvalue" + ENTRY(ix,cStatFields),STRING(fStatValues[ix])).
END.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF hBuffer:AVAIL THEN
      RETURN DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE). /* Dummy ved opprettelse av ny artikkel */
  ELSE 
      RETURN 0.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN REPLACE(
       REPLACE(
       DYNAMIC-FUNCTION("getFieldList","PkSdlLinje;,StrKonv;Storl",
                        "WHERE ArtikkelNr = " + STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                      + ",FIRST StrKonv NO-LOCK WHERE StrKonv.StrKode = PkSdlLinje.StrKode")
      ,"|",",")
      ," ","").

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
DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"fiTotAntall,fiTotLevert,fiTotRest").
DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer,fiTotAntall,fiTotLevert,fiTotRest").
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer,TBPkSdlLinje").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"PrisViewer,TBPkSdlLinje").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFilterFields C-Win 
FUNCTION setFilterFields RETURNS LOGICAL
  ( INPUT icMode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF CAN-DO("blank,hide",icMode) THEN
    ASSIGN cmbButikknr:SCREEN-VALUE = "0"
           sokLevNr:SCREEN-VALUE    = ""
           sokLevKod:SCREEN-VALUE   = ""
           tbLeverte:CHECKED        = NO
           tbKorrigerte:CHECKED     = NO
           sokBeskr:SCREEN-VALUE    = ""
           rsMottatt:SCREEN-VALUE   = "1"
           NO-ERROR.
  ASSIGN cmbButikknr:HIDDEN    = icMode = "hide" AND NUM-ENTRIES(cmbButikkNr:LIST-ITEM-PAIRS,"|") < 5 AND NOT bAllowCreate
         sokLevNr:HIDDEN       = icMode = "hide"
         sokLevKod:HIDDEN      = icMode = "hide"
         tbLeverte:HIDDEN      = icMode = "hide"
         tbKorrigerte:HIDDEN   = icMode = "hide"
         sokBeskr:HIDDEN       = icMode = "hide"
         rsMottatt:HIDDEN      = icMode = "hide"
         btnBlankFilter:HIDDEN = icMode = "hide"
         btnLev:HIDDEN         = icMode = "hide"
         sokLevNamn:HIDDEN     = icMode = "hide"
         .
END.

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

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParentQuery C-Win 
FUNCTION setParentQuery RETURNS LOGICAL
  ( INPUT ihParentQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  hParentBrowse = ihParentQuery.
  hParentBuffer = hParentBrowse:QUERY:GET-BUFFER-HANDLE(1).

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPrevNextStrekkodeMode C-Win 
FUNCTION setPrevNextStrekkodeMode RETURNS LOGICAL
  ( INPUT ibPrevNextInvalidStrekkode AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bPrevNextInvalidStrekkode = ibPrevNextInvalidStrekkode.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hParentBrowse = ihQuery
       hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

