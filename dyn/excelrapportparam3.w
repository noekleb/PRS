&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR iX          AS INT    NO-UNDO.
DEF VAR iY          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.

DEF VAR hUpdate     AS HANDLE NO-UNDO.
DEF VAR hAdHoc      AS HANDLE NO-UNDO.
DEF VAR tth         AS HANDLE NO-UNDO.

DEF VAR gsysHId     AS INT INIT 228 NO-UNDO.
DEF VAR gSysGr      AS INT NO-UNDO.

DEF VAR mpXML       AS MEMPTR NO-UNDO.
DEF VAR hTeamCombo  AS HANDLE NO-UNDO.
DEF VAR hSourceBrw  AS HANDLE NO-UNDO.

DEF VAR bButikk     AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolbar RECT-1 RECT-2 hjelpetekst ~
btnbutnr btnAvdelingNr advRapportNavn rptEmailAddr advSendMail BtnOK BtnNew 
&Scoped-Define DISPLAYED-OBJECTS sysGr Beskrivelse hjelpetekst butik ~
advFilePath avdelingnr advFileExtent stTypeId advRapportNavn rptEmailAddr ~
advSendMail lblAdvanced lblButikk lblAvdeling lblButtons 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 advFilePath advFileExtent stTypeId advRapportNavn ~
rptEmailAddr 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doGetFrameField C-Win 
FUNCTION doGetFrameField RETURNS HANDLE
  ( INPUT ihFrame      AS HANDLE,
    INPUT icFrameField AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initValues C-Win 
FUNCTION initValues RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD receiveParameterTable C-Win 
FUNCTION receiveParameterTable RETURNS HANDLE
  (INPUT iptth AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sendParameterTable C-Win 
FUNCTION sendParameterTable RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSysGr C-Win 
FUNCTION setSysGr RETURNS LOGICAL
  ( INPUT iiSysgr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD validateFields C-Win 
FUNCTION validateFields RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAvdelingNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnbutnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON BtnDelete DEFAULT 
     LABEL "Slett" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON BtnNew DEFAULT 
     LABEL "Ny" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Ad-Hoc rapport" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON BtnUpdate DEFAULT 
     LABEL "Oppdater" 
     SIZE 18 BY 1.67
     BGCOLOR 8 .

DEFINE VARIABLE avdelingnr AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE butik AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP LARGE
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE hjelpetekst AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 3.33 NO-UNDO.

DEFINE VARIABLE advFileExtent AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil type" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE advFilePath AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil sti" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE advRapportNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapport navn" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62.6 BY 1 NO-UNDO.

DEFINE VARIABLE lblAdvanced AS CHARACTER FORMAT "X(256)":U INITIAL "Avanserte parametre" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 NO-UNDO.

DEFINE VARIABLE lblAvdeling AS CHARACTER FORMAT "X(256)":U INITIAL "Avdeling:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE lblButikk AS CHARACTER FORMAT "X(256)":U INITIAL "Butikk:" 
      VIEW-AS TEXT 
     SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE lblButtons AS CHARACTER FORMAT "X(256)":U INITIAL "Faste rapporter" 
      VIEW-AS TEXT 
     SIZE 15 BY .62 NO-UNDO.

DEFINE VARIABLE rptEmailAddr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE stTypeId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rapport type" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE sysGr AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "RapportId" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.4 BY 2.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 7.14.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 1.19.

DEFINE VARIABLE advSendMail AS LOGICAL INITIAL no 
     LABEL "Send ePost" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Send splittrapport som epost til butikk ansvarlig" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     sysGr AT ROW 2.67 COL 16 COLON-ALIGNED
     Beskrivelse AT ROW 2.67 COL 23.4 COLON-ALIGNED NO-LABEL
     hjelpetekst AT ROW 3.86 COL 18 NO-LABEL
     butik AT ROW 8.14 COL 18 NO-LABEL
     btnbutnr AT ROW 8.14 COL 39.8 NO-TAB-STOP 
     advFilePath AT ROW 9.1 COL 60 COLON-ALIGNED
     avdelingnr AT ROW 9.14 COL 18 NO-LABEL
     btnAvdelingNr AT ROW 9.14 COL 39.8 NO-TAB-STOP 
     advFileExtent AT ROW 10.05 COL 60 COLON-ALIGNED
     stTypeId AT ROW 11 COL 60 COLON-ALIGNED
     advRapportNavn AT ROW 11.95 COL 60 COLON-ALIGNED
     rptEmailAddr AT ROW 12.91 COL 60 COLON-ALIGNED
     advSendMail AT ROW 14.1 COL 62
     BtnOK AT ROW 16.29 COL 10
     BtnNew AT ROW 16.29 COL 30
     BtnUpdate AT ROW 16.29 COL 48
     BtnDelete AT ROW 16.29 COL 66
     lblAdvanced AT ROW 7.91 COL 46 COLON-ALIGNED NO-LABEL
     lblButikk AT ROW 8.38 COL 8 COLON-ALIGNED NO-LABEL
     lblAvdeling AT ROW 9.33 COL 6 COLON-ALIGNED NO-LABEL
     lblButtons AT ROW 15.43 COL 27.6 COLON-ALIGNED NO-LABEL
     rectToolbar AT ROW 1.24 COL 2
     RECT-1 AT ROW 15.81 COL 28.6
     RECT-2 AT ROW 8.14 COL 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 92.6 BY 18.43.


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
         TITLE              = "Tempo Rapport"
         HEIGHT             = 18.14
         WIDTH              = 95
         MAX-HEIGHT         = 46.1
         MAX-WIDTH          = 336
         VIRTUAL-HEIGHT     = 46.1
         VIRTUAL-WIDTH      = 336
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/app16.ico":U) THEN
    MESSAGE "Unable to load icon: ico/app16.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 18.43
       FRAME DEFAULT-FRAME:WIDTH            = 92.6.

/* SETTINGS FOR FILL-IN advFileExtent IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN advFilePath IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN advRapportNavn IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       advRapportNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR avdelingnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Beskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR BUTTON BtnDelete IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnUpdate IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR butik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       hjelpetekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblAdvanced IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblAdvanced:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblAvdeling IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblAvdeling:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblButikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblButikk:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblButtons IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblButtons:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN rptEmailAddr IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       rptEmailAddr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN stTypeId IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN sysGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Tempo Rapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tempo Rapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdelingNr C-Win
ON CHOOSE OF btnAvdelingNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.
  DEF VAR cTmp        AS CHAR NO-UNDO.

  cTmp = REPLACE(avdelingnr:SCREEN-VALUE,'|',',').
  IF avdelingnr:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Avdeling",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(Avdelingnr))").
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Avdeling"      
                      + ";avdelingnr"  
                      + ";AvdelingNavn"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "AvdelingNr", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
  DO:
    ASSIGN 
      Avdelingnr:SCREEN-VALUE = cIdList
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnbutnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnbutnr C-Win
ON CHOOSE OF btnbutnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  DEF VAR cTmp        AS CHAR NO-UNDO.

  ASSIGN 
    bButikk = TRUE
    cTmp = REPLACE(butik:SCREEN-VALUE,'|',',').
  IF butik:SCREEN-VALUE NE '' THEN
    cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                  "Butiker",       /* Buffer(list) for query */
                                  "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                  "WHERE CAN-DO(" + QUOTER(cTmp) + ",STRING(butik))").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn;+!TeamList|character|x(255)|butikkteam_all.p(rowid)"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "butik", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
    butik:SCREEN-VALUE = cIdList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDelete C-Win
ON CHOOSE OF BtnDelete IN FRAME DEFAULT-FRAME /* Slett */
DO:
  RUN updateSysGruppe.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNew
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNew C-Win
ON CHOOSE OF BtnNew IN FRAME DEFAULT-FRAME /* Ny */
DO:
  RUN updateSysGruppe.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Ad-Hoc rapport */
DO:
  validateFields().
  RUN writeAdHoc.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnUpdate C-Win
ON CHOOSE OF BtnUpdate IN FRAME DEFAULT-FRAME /* Oppdater */
DO:
  RUN updateSysGruppe.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hUpdate) THEN APPLY 'close' TO hUpdate.
  mpXML = ?.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankRecord C-Win 
PROCEDURE BlankRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR qh         AS HANDLE NO-UNDO.
  DEF VAR hObj       AS HANDLE NO-UNDO.
  DEF VAR cDroppList AS CHAR   NO-UNDO.
  ASSIGN 
    hObj = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
    hObj = hObj:FIRST-CHILD
    cDroppList = REPLACE("{&List-2}",' ',',') 
 .
  DO WHILE VALID-HANDLE(hObj):
    IF NOT CAN-DO(cDroppList,hObj:NAME) THEN
      CASE hObj:TYPE:
        WHEN 'FILL-IN' OR WHEN 'EDITOR'  THEN hObj:SCREEN-VALUE = ''.
        WHEN 'COMBO-BOX' THEN 
        DO:
          /*IF hObj:NAME = 'opris' THEN hObj:SCREEN-VALUE = 'Alle'.*/
          hObj:SCREEN-VALUE = hObj:ENTRY(1).
          
        END.
        WHEN 'TOGGLE-BOX' THEN
          hObj:CHECKED = FALSE.
      END CASE.
    hObj = hObj:NEXT-SIBLING.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      btnUpdate:SENSITIVE = FALSE
      btnDelete:SENSITIVE = FALSE
    .
  END.
  DYNAMIC-FUNCTION('initValues':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildParameterTable C-Win 
PROCEDURE BuildParameterTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hObj          AS HANDLE    NO-UNDO.
  DEF VAR bh            AS HANDLE    NO-UNDO.
  DEF VAR cFieldName    AS CHAR      NO-UNDO.
  DEF VAR icnt          AS INT       NO-UNDO.
  DEF VAR artbasFields  AS CHARACTER NO-UNDO.
  DEF VAR StLinjeFields AS CHARACTER NO-UNDO.    
  
  DO WITH FRAME {&FRAME-NAME}:
    tth:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().

    /*Dette bør byttes ut med oppslag mot _file slik at en kan legge inn nye felter uten å måtte vedlikeholde
     disse listene*/
    ASSIGN 
      artbasFields  = 'vg,sasong,matkod,vmid,farg,levnr,opris'
      StLinjeFields = 'perid,butik,stTypeId'
      hObj          = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
      hObj          = hObj:FIRST-CHILD
      bh            = tth:DEFAULT-BUFFER-HANDLE
      iCnt          = 0
    .
    /*Løp igjennom widgettreet og lag parametre av alle feltene (fill-in og combo), ved NEW vil sysGr være
    0 så den må fikses i oppdateringsprogrammet*/
    DO WHILE VALID-HANDLE(hObj):
      IF CAN-DO('FILL-IN,EDITOR,COMBO-BOX,TOGGLE-BOX',hObj:TYPE) THEN
      myBlock: 
      DO ON ERROR UNDO, LEAVE:
        IF hObj:NAME BEGINS 'lbl' THEN UNDO myBlock, LEAVE myBlock.
        IF hObj:NAME = 'Beskrivelse' THEN UNDO myBlock, LEAVE myBlock. /*Kan ikke lagre beskrivelse i sysPara, den vedlikeholdes i sysGruppe*/
        cFieldName = IF CAN-DO(artbasfields,hObj:NAME) THEN 'artbas.' + hObj:NAME 
                     ELSE 
                       IF CAN-DO(StLinjeFields,hObj:NAME) THEN 'stlinje.' + hObj:NAME 
                         ELSE hObj:NAME.
        bh:FIND-FIRST('WHERE parameter1 = ' + QUOTER(cFieldName)) NO-ERROR.
        IF bh:AVAILABLE THEN
        DO:
          /*Delete parameter if screen-value has no value or combo-box has value of "ALLE" */
          IF hObj:SCREEN-VALUE = '' OR hObj:SCREEN-VALUE = ? OR (hObj:TYPE = "COMBO-BOX" AND hObj:SCREEN-VALUE = "Alle") THEN
          DO:
            bh:BUFFER-DELETE().
            UNDO myBlock, LEAVE myBlock.
          END.
          bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = hObj:SCREEN-VALUE.
        END.
        ELSE
        DO:
          /*Do not add if screen-value has no value*/
          IF hObj:SCREEN-VALUE = '' OR hObj:SCREEN-VALUE = ? 
            OR (hObj:NAME = "opris" AND hObj:SCREEN-VALUE = "Alle") THEN 
            UNDO myBlock, LEAVE myBlock.
          bh:BUFFER-CREATE().                                     
          ASSIGN 
            iCnt = iCnt + 1
            bh:BUFFER-FIELD('sysHid'):BUFFER-VALUE     = STRING(gsysHid)
            bh:BUFFER-FIELD('sysGr'):BUFFER-VALUE      = (IF sysGr:SCREEN-VALUE = "0" THEN "999" ELSE sysGr:SCREEN-VALUE)
            bh:BUFFER-FIELD('paranr'):BUFFER-VALUE     = STRING(iCnt)
            bh:BUFFER-FIELD('parameter1'):BUFFER-VALUE = cFieldName
            bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = hObj:SCREEN-VALUE
          .
          IF cFieldName = 'artbas.opris' THEN /*Må bytte ja til YES og nei til NO*/
          DO:
            bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE = IF hObj:SCREEN-VALUE = 'Ja' THEN 'Yes' ELSE 'No'.
          END.
        END.
      END.
      hObj = hObj:NEXT-SIBLING.
    END.
                                         /*
      tth:WRITE-XML('file','slettme.xml').*/
  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")). */
/*   DYNAMIC-FUNCTION("setCurrentObject",hBrowse).                                                                             */
/*   RUN OpenQuery.                                                                                                            */

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
  DISPLAY sysGr Beskrivelse hjelpetekst butik advFilePath avdelingnr 
          advFileExtent stTypeId advRapportNavn rptEmailAddr advSendMail 
          lblAdvanced lblButikk lblAvdeling lblButtons 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolbar RECT-1 RECT-2 hjelpetekst btnbutnr btnAvdelingNr 
         advRapportNavn rptEmailAddr advSendMail BtnOK BtnNew 
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
  DO WITH FRAME {&FRAME-NAME}:
    hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                                  rectToolbar:HANDLE,
                                  "File",
                                  "Blank¤enabled,rule,Report;Hent Rapport¤enabled"
                                 ,"maxborder").

    tth = DYNAMIC-FUNCTION("getTempTable","","sysPara|where sysHid = " + STRING(gSysHid) + " AND SysGr=" + STRING(gsysGr),?).

    initValues().
    APPLY 'entry' TO butik.
  END.
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,500,200,200).
  DYNAMIC-FUNCTION("setNoResizex", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rect-1,rect-2").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"hjelpetekst,rect-1,rect-2").
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
APPLY 'entry' TO butik IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE myDisplayRecord C-Win 
PROCEDURE myDisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR qh         AS HANDLE NO-UNDO.
  DEF VAR hObj       AS HANDLE NO-UNDO.
  DEF VAR bh         AS HANDLE NO-UNDO.
  DEF VAR cReturn    AS CHAR   NO-UNDO.

  DEF VAR cFieldName AS CHAR NO-UNDO.
  
DO WITH FRAME {&FRAME-NAME}:
  IF VALID-HANDLE(tth) THEN DELETE OBJECT tth NO-ERROR.
  ASSIGN 
    cReturn                  = DYNAMIC-FUNCTION("getFieldValues","sysGruppe","WHERE sysHid = " + STRING(gSysHid) + ' AND sysGr =' + STRING(gSysGr),"sysGr,Beskrivelse,HjelpeTekst")
    sysGr:SCREEN-VALUE = ENTRY(1,cReturn,'|')           /*blir overskrevet om det finnes i parametertabell*/
    beskrivelse:SCREEN-VALUE = ENTRY(2,cReturn,'|')
    hjelpetekst:SCREEN-VALUE = ENTRY(3,cReturn,'|')
    advRapportNavn:SCREEN-VALUE = beskrivelse:SCREEN-VALUE /*blir overskrevet om det finnes i parametertabell*/
    hObj                     = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
    hObj                     = hObj:FIRST-CHILD
    gSysGr                   = INT(sysGr:SCREEN-VALUE)
    tth                      = DYNAMIC-FUNCTION("getTempTable","","sysPara|where sysHid = " + STRING(gSysHid) + " AND SysGr =" + STRING(gSysGr),?)
    bh                       = tth:DEFAULT-BUFFER-HANDLE
  .
  
  DO WHILE VALID-HANDLE(hObj):
    IF (hObj:TYPE = "FILL-IN" AND NOT hObj:NAME BEGINS 'lbl') OR CAN-DO('COMBO-BOX,EDITOR,TOGGLE-BOX',hObj:TYPE) THEN
    DO:
      cFieldName = 'artbas.' + hObj:NAME.
      bh:FIND-FIRST('where sysHid = ' + STRING(gsysHId) 
                                                + ' AND sysGr      = ' + STRING(sysgr:SCREEN-VALUE) 
                                                + ' AND parameter1 = ' + QUOTER(cFieldName),NO-LOCK) NO-ERROR.
      IF NOT bh:AVAILABLE THEN
      DO:
        cFieldName = 'stlinje.' + hObj:NAME.
        bh:FIND-FIRST('where sysHid = ' + STRING(gsysHId) 
                                                  + ' AND sysGr      = ' + STRING(sysgr:SCREEN-VALUE) 
                                                  + ' AND parameter1 = ' + QUOTER(cFieldName),NO-LOCK) NO-ERROR.
      END.
      IF NOT bh:AVAILABLE THEN
      DO:
        cFieldName = hObj:NAME.
        bh:FIND-FIRST('where sysHid = ' + STRING(gsysHId) 
                                                  + ' AND sysGr      = ' + STRING(sysgr:SCREEN-VALUE) 
                                                  + ' AND parameter1 = ' + QUOTER(cFieldName),NO-LOCK) NO-ERROR.
      END.
      IF bh:AVAILABLE THEN
      DO:
        hObj:SCREEN-VALUE = bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE.
      END.
    END.
    hObj = hObj:NEXT-SIBLING.
  END.
  initValues().  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE myTeam C-Win 
PROCEDURE myTeam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDropDown  AS HANDLE NO-UNDO.

IF ihDropDown:SCREEN-VALUE = "" THEN
  DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"QueryFilter","").
ELSE
  DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"QueryFilter"," AND CAN-DO(teamlist," + QUOTER(ihDropDown:SCREEN-VALUE) + ')').
DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
RUN openQuery.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReportRecord C-Win 
PROCEDURE ReportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cReturnValues  AS CHAR NO-UNDO.
  DEF VAR bOk            AS LOG  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "sysGruppe"
                      + ";SysGr|Rapp.nr"
                      + ";!SysHid"
                      + ";Beskrivelse"
                      + ";Hjelpetekst"
                     ,"WHERE sysHid = " + STRING(gSysHid)
                      ,""                                                  
                      ,"sysgr,hjelpetekst,beskrivelse",   /* <- return values for these fields */
                        OUTPUT cReturnValues,
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
    IF bOk THEN
    DO:
      RUN BlankRecord.
      ASSIGN 
        sysGr:SCREEN-VALUE        = ENTRY(1,cReturnValues,'|')
        hjelpetekst:SCREEN-VALUE  = ENTRY(2,cReturnValues,'|')
        Beskrivelse:SCREEN-VALUE  = ENTRY(3,cReturnValues,'|')
        btnUpdate:SENSITIVE       = TRUE
        btnDelete:SENSITIVE       = TRUE
        gSysGr                    = INT(sysGr:SCREEN-VALUE)
      .
      RUN myDisplayRecord.
      APPLY 'entry' TO butik.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR   cList AS CHAR NO-UNDO.
DEF VAR   hFilterToolbar AS HANDLE NO-UNDO.
DEF VAR   hFilterButton  AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer   AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer   AS HANDLE NO-UNDO.
DEF VAR hSelectorWin    AS HANDLE NO-UNDO.
DEF VAR hSelectorSplitB AS HANDLE NO-UNDO.

DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.

IF NOT bButikk THEN RETURN.

IF TRIM(ENTRY(1,PROGRAM-NAME(3)," ")) = "resendRecord" THEN .
ELSE 
DO:
  cList = " | |" + DYNAMIC-FUNCTION("getFieldList",'ButikkTeam;TeamNr|Beskrivelse;+rButikkTeam|butikkteam_rowid.p',
                           'WHERE TeamTypeId = "2"' /*Hardkodet tallet 2*/
                           ).
  CREATE COMBO-BOX hTeamCombo
    ASSIGN DELIMITER        = "|"
           DATA-TYPE        = "CHARACTER"
           FORMAT           = "x(256)"
           NAME             = "cbTeam"
           SUBTYPE          = "DROP-DOWN-LIST"
           LIST-ITEM-PAIRS  = cList
           INNER-LINES      = 50
           FRAME            = ihSourceBrw:FRAME
           X                = 90
           Y                = 5
           WIDTH-PIXELS     = 250
           VISIBLE          = TRUE
           SENSITIVE        = TRUE
           TRIGGERS:
             ON VALUE-CHANGED PERSISTENT RUN myTeam IN THIS-PROCEDURE (INPUT ihSourceBrw, INPUT hTeamCombo).
  /*            ON TAB           PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON BACK-TAB      PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON RETURN        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave"). */
  /*            ON ENTRY         PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"entry"). */
             ON END-ERROR     PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"end-error").
           END TRIGGERS.
   ASSIGN 
     bButikk    = FALSE
     hSourceBrw = ihSourceBrw
   .
END.
  /*Fiks størrelse*/ 
  ASSIGN hSelectorWin               = ihSourceBrw:WINDOW
         hSelectorWin:WIDTH-PIXELS  = hSelectorWin:WIDTH-PIXELS + 400
         hSelectorWin:HEIGHT-PIXELS = hSelectorWin:HEIGHT-PIXELS + 200
         hSelectorSplitB            = DYNAMIC-FUNCTION("getSplitBarHandle",hSelectorWin,"x")
         .
  APPLY "window-resized" TO hSelectorWin.
  hSelectorSplitB:X = hSelectorSplitB:FRAME:WIDTH-PIXELS / 2 - hSelectorSplitB:WIDTH-PIXELS / 2.

  APPLY "end-move" TO hSelectorSplitB.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateSysGruppe C-Win 
PROCEDURE updateSysGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    Ved ny, kjøres først en generering av parametre (buildParameterTable),
    for så å kjøre selve oppdateringsprogrammet.

------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DEF VAR cAction AS CHAR NO-UNDO.
  cAction = SUBSTRING(SELF:NAME,4).
  
  IF Butik:SCREEN-VALUE = '' AND cAction NE 'Delete' THEN
  DO:
    MESSAGE 'Feltet butikk MÅ ha verdi, vennligst prøv igjen'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY 'entry' TO btnbutnr.
    RETURN NO-APPLY.
  END.
  
  RUN BuildParameterTable.  
  ASSIGN 
    iX = INT((CURRENT-WINDOW:WIDTH-PIXEL / 2 + CURRENT-WINDOW:X))
    iY = INT((CURRENT-WINDOW:HEIGHT-PIXEL / 2 + CURRENT-WINDOW:Y))  
  .
  RUN excelrapportUpdate.w PERSISTENT SET hUpdate.
  IF VALID-HANDLE(hUpdate) THEN 
  DO:
    ASSIGN 
      hUpdate:CURRENT-WINDOW:X =  iX
      hUpdate:CURRENT-WINDOW:Y =  iY
      . 
    RUN initValues IN hUpdate (INPUT TABLE-HANDLE tth,INPUT cAction,INPUT STRING(gSysHid),INPUT sysGr:SCREEN-VALUE).
    RUN initializeObject IN hUpdate.
    RUN MoveToTop IN hUpdate.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeAdHoc C-Win 
PROCEDURE writeAdHoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   advfilePath:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SESSION:TEMP-DIRECTORY. */
  
    DEFINE VARIABLE cTempDirectory AS CHAR INIT ? NO-UNDO.
    cTempDirectory = IF cTempDirectory = ? THEN OS-GETENV("TEMP") ELSE cTempDirectory.
    cTempDirectory = IF cTempDirectory = ? THEN OS-GETENV("TMP") ELSE cTempDirectory.
    advfilePath:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cTempDirectory.

  
  
  RUN BuildParameterTable.  
  ASSIGN 
    iX = INT((CURRENT-WINDOW:WIDTH-PIXEL / 2 + CURRENT-WINDOW:X))
    iY = INT((CURRENT-WINDOW:HEIGHT-PIXEL / 2 + CURRENT-WINDOW:Y))  
  .
  RUN excelrapportadhoc3.w PERSISTENT SET hAdHoc.
  
/*   MESSAGE 'Write-xml'   'c:\temp\slettme.xml'  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
/*   tth:WRITE-XML('file','c:\temp\slettme.xml'). */
  IF VALID-HANDLE(hAdHoc) THEN 
  DO:
    ASSIGN 
      hAdHoc:CURRENT-WINDOW:X =  iX
      hAdHoc:CURRENT-WINDOW:Y =  iY
      . 
    RUN initializeObject IN hAdHoc.
    DYNAMIC-FUNCTION('setReportFile' IN hAdHoc,'excelrapport3.p').
    RUN MoveToTop IN hAdHoc.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doGetFrameField C-Win 
FUNCTION doGetFrameField RETURNS HANDLE
  ( INPUT ihFrame      AS HANDLE,
    INPUT icFrameField AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR hObj AS HANDLE NO-UNDO.

  hObj = ihFrame:FIRST-CHILD:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hObj):
    IF hObj:NAME = icFrameField THEN
      RETURN hObj.
    hObj = hObj:NEXT-SIBLING.
  END.
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initValues C-Win 
FUNCTION initValues RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcFilePath   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcFileExtent AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcEmailAddr  AS CHARACTER NO-UNDO. 

  pcEmailAddr = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 227 and SysGr = 1 and ParaNr = 3","Parameter2").

  pcFilePath = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 227 and SysGr = 1 and ParaNr = 1","Parameter2").
  IF pcFilePath = ? OR pcFilePath = '' THEN pcFilePath = SESSION:TEMP-DIRECTORY.
  pcFileExtent = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 227 and SysGr = 1 and ParaNr = 2","Parameter2").
  IF pcFileExtent = ? OR pcFileExtent = '' THEN pcFileExtent = '.xls'.

  OS-CREATE-DIR VALUE(RIGHT-TRIM(pcFilePath,'\')). /* Sikrer at filkatalogen finnes. */

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      advFilePath:SCREEN-VALUE    = IF advFilePath:SCREEN-VALUE    = '' THEN pcFilePath ELSE advFilePath:SCREEN-VALUE
      advFileExtent:SCREEN-VALUE  = IF advFileExtent:SCREEN-VALUE  = '' THEN pcFileExtent ELSE advFileExtent:SCREEN-VALUE
      stTypeId:SCREEN-VALUE    = IF stTypeId:SCREEN-VALUE    = '' THEN 'Avdeling' ELSE stTypeId:SCREEN-VALUE
      rptEmailAddr:SCREEN-VALUE = IF rptEmailAddr:SCREEN-VALUE  = '' THEN pcEmailAddr ELSE rptEmailAddr:SCREEN-VALUE
      advRapportNavn:SCREEN-VALUE = IF advRapportNavn:SCREEN-VALUE = '' THEN 'Tempo Rapport - ' + STRING(TIME,"HH:MM:SS") + ':' + STRING(RANDOM(1,99)) ELSE advRapportNavn:SCREEN-VALUE
      
      advRapportNavn:SCREEN-VALUE = REPLACE(advRapportNavn:SCREEN-VALUE,':','')
    .    
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION receiveParameterTable C-Win 
FUNCTION receiveParameterTable RETURNS HANDLE
  (INPUT iptth AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  tth = iptth.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sendParameterTable C-Win 
FUNCTION sendParameterTable RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN tth.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSysGr C-Win 
FUNCTION setSysGr RETURNS LOGICAL
  ( INPUT iiSysgr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  gSysGr = iiSysGr.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION validateFields C-Win 
FUNCTION validateFields RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR hField AS HANDLE NO-UNDO.
  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

