&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
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
DEFINE VARIABLE cTypeListItems AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iArtikelnrCol  AS INTEGER INIT 1    NO-UNDO.
DEFINE VARIABLE cTypeList AS CHARACTER   NO-UNDO.
ASSIGN cTypeList = "Kunder,Butiksfsg/kund,Drivmedel,Artikel,Kampanj,Vg".


DEFINE TEMP-TABLE tt_typ NO-UNDO
    FIELD iType AS INTEGER
    FIELD cNamn AS CHARACTER
    INDEX TYPE IS PRIMARY UNIQUE iType.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-Analyse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES preemanalyse preemanalysembr preemanalyserad

/* Definitions for BROWSE BR-Analyse                                    */
&Scoped-define FIELDS-IN-QUERY-BR-Analyse preemanalyse.AnalyseId ~
preemanalyse.Navn preemanalyse.StartDato preemanalyse.SluttDato ~
preemanalyse.Aktiv 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Analyse 
&Scoped-define QUERY-STRING-BR-Analyse FOR EACH preemanalyse NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Analyse OPEN QUERY BR-Analyse FOR EACH preemanalyse NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Analyse preemanalyse
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Analyse preemanalyse


/* Definitions for BROWSE BR-analysembr                                 */
&Scoped-define FIELDS-IN-QUERY-BR-analysembr preemanalysembr.Artikkelnr ~
preemanalysembr.KampId preemanalysembr.KampTilbId getBeskr() 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-analysembr 
&Scoped-define QUERY-STRING-BR-analysembr FOR EACH preemanalysembr ~
      WHERE preemanalysembr.AnalyseId = preemanalyserad.Analyseid ~
 AND preemanalysembr.radnr = preemanalyserad.radnr NO-LOCK ~
    BY preemanalysembr.Artikkelnr ~
       BY preemanalysembr.KampId ~
        BY preemanalysembr.KampTilbId INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-analysembr OPEN QUERY BR-analysembr FOR EACH preemanalysembr ~
      WHERE preemanalysembr.AnalyseId = preemanalyserad.Analyseid ~
 AND preemanalysembr.radnr = preemanalyserad.radnr NO-LOCK ~
    BY preemanalysembr.Artikkelnr ~
       BY preemanalysembr.KampId ~
        BY preemanalysembr.KampTilbId INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-analysembr preemanalysembr
&Scoped-define FIRST-TABLE-IN-QUERY-BR-analysembr preemanalysembr


/* Definitions for BROWSE BR-AnalyseRad                                 */
&Scoped-define FIELDS-IN-QUERY-BR-AnalyseRad preemanalyserad.AnalyseId ~
preemanalyserad.beskr preemanalyserad.radnr preemanalyserad.typ 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-AnalyseRad 
&Scoped-define QUERY-STRING-BR-AnalyseRad FOR EACH preemanalyserad ~
      WHERE preemanalyserad.AnalyseId = preemanalyse.AnalyseId NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-AnalyseRad OPEN QUERY BR-AnalyseRad FOR EACH preemanalyserad ~
      WHERE preemanalyserad.AnalyseId = preemanalyse.AnalyseId NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-AnalyseRad preemanalyserad
&Scoped-define FIRST-TABLE-IN-QUERY-BR-AnalyseRad preemanalyserad


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-Analyse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-EndreRad-2 RECT-1 RECT-2 RECT-3 BR-Analyse ~
BR-AnalyseRad BR-analysembr B-NyRadMbr-2 FI3_Element FI3_KampTilbId ~
B-NyRadMbr B-Endre B-EndreRad B-Ny B-NyRad 
&Scoped-Define DISPLAYED-OBJECTS FI_AnalyseId FI_Navn FI_teamnr FI_Teamnamn ~
FI_StartDato FI_SluttDato FI_Aktiv FI2_AnalyseId FI2_radnr FI2_ctyp ~
FI2_beskr FI3_Element FI3_KampTilbId FI3_beskr FI-Rapporttxt ~
FI-Rapportradtxt FI-Rapportradmbrtxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBeskr C-Win 
FUNCTION getBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Endre 
     IMAGE-UP FILE "icon/e-detail.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ändra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ändra".

DEFINE BUTTON B-EndreRad 
     IMAGE-UP FILE "icon/e-detail.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Än&dra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ändra".

DEFINE BUTTON B-EndreRad-2 
     IMAGE-UP FILE "icon/e-detail.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Än&dra" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ändra".

DEFINE BUTTON B-Ny 
     IMAGE-UP FILE "icon/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ny post".

DEFINE BUTTON B-NyRad 
     IMAGE-UP FILE "icon/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "N&y" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ny post".

DEFINE BUTTON B-NyRadMbr 
     IMAGE-UP FILE "icon/add.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Ny" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ny post".

DEFINE BUTTON B-NyRadMbr-2 
     IMAGE-UP FILE "icon/deleterec.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Ny" 
     SIZE 4.57 BY 1.04 TOOLTIP "Ny post".

DEFINE VARIABLE FI-Rapportradmbrtxt AS CHARACTER FORMAT "X(256)":U INITIAL "Medlem rapportrad" 
      VIEW-AS TEXT 
     SIZE 41 BY .96
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Rapportradtxt AS CHARACTER FORMAT "X(256)":U INITIAL "Rapportrad" 
      VIEW-AS TEXT 
     SIZE 41 BY .96
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Rapporttxt AS CHARACTER FORMAT "X(256)":U INITIAL "Rapport" 
      VIEW-AS TEXT 
     SIZE 41 BY .96
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI2_AnalyseId AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Analysid" 
     VIEW-AS FILL-IN 
     SIZE 10.43 BY 1.

DEFINE VARIABLE FI2_beskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Beskrivning" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI2_ctyp AS CHARACTER FORMAT "X(256)" 
     LABEL "Typ" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI2_radnr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Radnr" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI3_beskr AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskriving" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI3_Element AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Artikkelnr" 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY 1.

DEFINE VARIABLE FI3_KampTilbId AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "KampTilbId" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY 1.

DEFINE VARIABLE FI_Aktiv AS LOGICAL FORMAT "J/N" INITIAL NO 
     LABEL "Aktiv" 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI_AnalyseId AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Analysid" 
     VIEW-AS FILL-IN 
     SIZE 10.43 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI_Navn AS CHARACTER FORMAT "X(30)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI_SluttDato AS DATE FORMAT "99/99/99" 
     LABEL "Slut datum" 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI_StartDato AS DATE FORMAT "99/99/99" 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI_Teamnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_teamnr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Teamnr" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.57 BY 8.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.43 BY 5.69.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.43 BY 6.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-Analyse FOR 
      preemanalyse SCROLLING.

DEFINE QUERY BR-analysembr FOR 
      preemanalysembr SCROLLING.

DEFINE QUERY BR-AnalyseRad FOR 
      preemanalyserad SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-Analyse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Analyse C-Win _STRUCTURED
  QUERY BR-Analyse NO-LOCK DISPLAY
      preemanalyse.AnalyseId FORMAT ">>>>>9":U
      preemanalyse.Navn FORMAT "X(30)":U
      preemanalyse.StartDato FORMAT "99/99/99":U
      preemanalyse.SluttDato FORMAT "99/99/99":U
      preemanalyse.Aktiv FORMAT "J/N":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 72 BY 11.42 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BR-analysembr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-analysembr C-Win _STRUCTURED
  QUERY BR-analysembr NO-LOCK DISPLAY
      preemanalysembr.Artikkelnr FORMAT ">>>>>>>>>>>>9":U
      preemanalysembr.KampId FORMAT ">>>>>>>>>>>>9":U
      preemanalysembr.KampTilbId FORMAT ">>>>>>>>9":U
      getBeskr() COLUMN-LABEL "Beskrivning" FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 72 BY 7.42 FIT-LAST-COLUMN.

DEFINE BROWSE BR-AnalyseRad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-AnalyseRad C-Win _STRUCTURED
  QUERY BR-AnalyseRad NO-LOCK DISPLAY
      preemanalyserad.AnalyseId FORMAT ">>>>>9":U
      preemanalyserad.beskr FORMAT "x(30)":U
      preemanalyserad.radnr FORMAT ">>>9":U
      preemanalyserad.typ FORMAT ">9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 72.57 BY 10.65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-EndreRad-2 AT ROW 28.46 COL 83 NO-TAB-STOP 
     BR-Analyse AT ROW 2.62 COL 4
     FI_AnalyseId AT ROW 3.85 COL 93.14 COLON-ALIGNED HELP
          "Unikt nummer på analysen."
     FI_Navn AT ROW 4.85 COL 93.14 COLON-ALIGNED HELP
          "Navn på analysen"
     FI_teamnr AT ROW 5.85 COL 93.14 COLON-ALIGNED
     FI_Teamnamn AT ROW 5.85 COL 101 COLON-ALIGNED NO-LABEL
     FI_StartDato AT ROW 6.85 COL 93.14 COLON-ALIGNED HELP
          "Dato for start av analysen."
     FI_SluttDato AT ROW 7.85 COL 93.14 COLON-ALIGNED HELP
          "Siste dag analysen skal kjøres."
     FI_Aktiv AT ROW 8.85 COL 93.14 COLON-ALIGNED HELP
          "Analysen er aktiv."
     BR-AnalyseRad AT ROW 15.58 COL 4.43
     FI2_AnalyseId AT ROW 16.81 COL 93.57 COLON-ALIGNED HELP
          "Unikt nummer på analysen."
     FI2_radnr AT ROW 17.81 COL 93.57 COLON-ALIGNED
     FI2_ctyp AT ROW 18.81 COL 93.57 COLON-ALIGNED
     FI2_beskr AT ROW 19.81 COL 93.57 COLON-ALIGNED
     BR-analysembr AT ROW 28.35 COL 5
     B-NyRadMbr-2 AT ROW 28.46 COL 88 NO-TAB-STOP 
     FI3_Element AT ROW 30.58 COL 93.43 COLON-ALIGNED
     FI3_KampTilbId AT ROW 31.62 COL 93.43 COLON-ALIGNED
     FI3_beskr AT ROW 32.62 COL 93.43 COLON-ALIGNED HELP
          "Navn på analysen"
     B-NyRadMbr AT ROW 28.46 COL 78 NO-TAB-STOP 
     B-Endre AT ROW 2.42 COL 83 NO-TAB-STOP 
     B-EndreRad AT ROW 15.69 COL 83 NO-TAB-STOP 
     B-Ny AT ROW 2.42 COL 78 NO-TAB-STOP 
     B-NyRad AT ROW 15.69 COL 78 NO-TAB-STOP 
     FI-Rapporttxt AT ROW 1.23 COL 2.43 COLON-ALIGNED NO-LABEL
     FI-Rapportradtxt AT ROW 14.35 COL 2.57 COLON-ALIGNED NO-LABEL
     FI-Rapportradmbrtxt AT ROW 26.85 COL 3 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 2.23 COL 77.43
     RECT-2 AT ROW 15.58 COL 77.57
     RECT-3 AT ROW 28.35 COL 77.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 148.8 BY 37.95.


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
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Konfigurering säljmålsuppfyllning"
         HEIGHT             = 35.46
         WIDTH              = 131.43
         MAX-HEIGHT         = 47.54
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 47.54
         VIRTUAL-WIDTH      = 256
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BR-Analyse RECT-3 DEFAULT-FRAME */
/* BROWSE-TAB BR-AnalyseRad FI_Aktiv DEFAULT-FRAME */
/* BROWSE-TAB BR-analysembr FI2_beskr DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Rapportradmbrtxt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rapportradtxt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rapporttxt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI2_AnalyseId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI2_beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI2_ctyp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI2_radnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI3_beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_Aktiv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_AnalyseId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_Navn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_SluttDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_StartDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_Teamnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_teamnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Analyse
/* Query rebuild information for BROWSE BR-Analyse
     _TblList          = "data.preemanalyse"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = data.preemanalyse.AnalyseId
     _FldNameList[2]   = data.preemanalyse.Navn
     _FldNameList[3]   = data.preemanalyse.StartDato
     _FldNameList[4]   = data.preemanalyse.SluttDato
     _FldNameList[5]   > data.preemanalyse.Aktiv
"preemanalyse.Aktiv" ? "J/N" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BR-Analyse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-analysembr
/* Query rebuild information for BROWSE BR-analysembr
     _TblList          = "data.preemanalysembr"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "data.preemanalysembr.Artikkelnr|yes,data.preemanalysembr.KampId|yes,data.preemanalysembr.KampTilbId|yes"
     _Where[1]         = "data.preemanalysembr.AnalyseId = data.preemanalyserad.Analyseid
 AND data.preemanalysembr.radnr = data.preemanalyserad.radnr"
     _FldNameList[1]   = data.preemanalysembr.Artikkelnr
     _FldNameList[2]   = data.preemanalysembr.KampId
     _FldNameList[3]   = data.preemanalysembr.KampTilbId
     _FldNameList[4]   > "_<CALC>"
"getBeskr()" "Beskrivning" "x(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BR-analysembr */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-AnalyseRad
/* Query rebuild information for BROWSE BR-AnalyseRad
     _TblList          = "data.preemanalyserad"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "data.preemanalyserad.AnalyseId = data.preemanalyse.AnalyseId"
     _FldNameList[1]   = data.preemanalyserad.AnalyseId
     _FldNameList[2]   = data.preemanalyserad.beskr
     _FldNameList[3]   = data.preemanalyserad.radnr
     _FldNameList[4]   = data.preemanalyserad.typ
     _Query            is NOT OPENED
*/  /* BROWSE BR-AnalyseRad */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Konfigurering säljmålsuppfyllning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Konfigurering säljmålsuppfyllning */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Endre C-Win
ON CHOOSE OF B-Endre IN FRAME DEFAULT-FRAME /* Ändra */
DO:
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    IF NOT AVAIL preemanalyse THEN
        RETURN NO-APPLY.
    rRowId = ROWID(preemanalyse).
    RUN d-preemanalyse.w ("Endre",INPUT-OUTPUT rRowId).
    IF RETURN-VALUE = "OK" THEN
        BROWSE BR-Analyse:REFRESH().
    APPLY "VALUE-CHANGED" TO BROWSE BR-Analyse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EndreRad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EndreRad C-Win
ON CHOOSE OF B-EndreRad IN FRAME DEFAULT-FRAME /* Ändra */
DO:
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    DEFINE VARIABLE cTypelistItems AS CHARACTER   NO-UNDO.
    IF NOT AVAIL preemanalyserad THEN
        RETURN NO-APPLY.
    FIND TT_Typ WHERE TT_Typ.iType = preemanalyserad.typ.
    cTypelistItems = tt_typ.cNamn + "," + STRING(preemanalyserad.typ).
    rRowId = ROWID(preemanalyserad).
    RUN d-preemanalyserad.w ("Endre",preemanalyserad.AnalyseId,cTypelistItems,INPUT-OUTPUT rRowId).
    IF RETURN-VALUE = "OK" THEN
        BROWSE BR-AnalyseRad:REFRESH().
    APPLY "VALUE-CHANGED" TO BROWSE BR-AnalyseRad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EndreRad-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EndreRad-2 C-Win
ON CHOOSE OF B-EndreRad-2 IN FRAME DEFAULT-FRAME /* Ändra */
DO:
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    IF NOT AVAIL preemanalyserad OR preemanalyserad.typ = 4 THEN
        RETURN NO-APPLY.
    rRowId = ROWID(preemanalysembr).
    RUN d-preemanalyseradmbr.w ("Endre",
                                preemanalyserad.analyseid,
                                FI2_radnr,
                                preemanalyserad.typ,
                                FI2_ctyp,
                                0, /* vi hämtar posten i anropad rutin */
                                "",
                                INPUT-OUTPUT rRowId).

    IF RETURN-VALUE = "OK" THEN DO:
        {&OPEN-QUERY-BR-Analysembr}
        IF rRowId <> ? THEN
            REPOSITION BR-Analysembr TO ROWID rRowId.
    END.
    APPLY "VALUE-CHANGED" TO BROWSE BR-Analysembr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny C-Win
ON CHOOSE OF B-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    RUN d-preemanalyse.w ("Ny",INPUT-OUTPUT rRowId).
    IF RETURN-VALUE = "OK" THEN DO:
        {&OPEN-QUERY-BR-Analyse}
        IF rRowId <> ? THEN
            REPOSITION BR-Analyse TO ROWID rRowId.
        RUN SkapaFasta.
    END.
    APPLY "VALUE-CHANGED" TO BROWSE BR-Analyse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-NyRad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NyRad C-Win
ON CHOOSE OF B-NyRad IN FRAME DEFAULT-FRAME /* Ny */
DO:
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    DEFINE BUFFER bRad FOR preemanalyserad.
    IF NOT AVAIL preemanalyse THEN
        RETURN NO-APPLY.
    cTypelistItems = "".
    FOR EACH TT_Typ.
        IF TT_typ.iTyp < 4 AND CAN-FIND(preemanalyserad OF preemanalyse WHERE preemanalyserad.typ = TT_typ.iTyp) THEN
            NEXT.
        cTypelistItems = cTypelistItems + (IF cTypelistItems <> "" THEN "," ELSE "") +
                             TT_Typ.cNamn + "," + STRING(TT_Typ.ityp).
    END.
    RUN d-preemanalyserad.w ("Ny",preemanalyse.analyseid,cTypelistItems,INPUT-OUTPUT rRowId).
    IF RETURN-VALUE = "OK" THEN DO:
        {&OPEN-QUERY-BR-Analyserad}
        IF rRowId <> ? THEN
            REPOSITION BR-AnalyseRad TO ROWID rRowId.
    END.
    APPLY "VALUE-CHANGED" TO BROWSE BR-AnalyseRad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-NyRadMbr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NyRadMbr C-Win
ON CHOOSE OF B-NyRadMbr IN FRAME DEFAULT-FRAME /* Ny */
DO:
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    IF NOT AVAIL preemanalyserad OR preemanalyserad.typ < 4 THEN
        RETURN NO-APPLY.
    RUN d-preemanalyseradmbr.w ("Ny",preemanalyserad.analyseid,FI2_radnr,preemanalyserad.typ,FI2_ctyp,0,"",INPUT-OUTPUT rRowId).
    IF RETURN-VALUE = "OK" THEN DO:
        {&OPEN-QUERY-BR-Analysembr}
        IF rRowId <> ? THEN
            REPOSITION BR-Analysembr TO ROWID rRowId.
    END.
    APPLY "VALUE-CHANGED" TO BROWSE BR-Analysembr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-NyRadMbr-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NyRadMbr-2 C-Win
ON CHOOSE OF B-NyRadMbr-2 IN FRAME DEFAULT-FRAME /* Ny */
DO:
    DEFINE VARIABLE rRowid AS ROWID       NO-UNDO.
    IF NOT AVAIL preemanalysembr THEN
        RETURN NO-APPLY.
    MESSAGE "Skall vald rad tas bort?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOK AS LOGICAL.
    IF lOK = TRUE THEN DO:
        FIND CURRENT preemanalysembr EXCLUSIVE.
        DELETE preemanalysembr.
        {&OPEN-QUERY-BR-Analysembr}
    END.
    APPLY "VALUE-CHANGED" TO BROWSE BR-Analysembr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Analyse
&Scoped-define SELF-NAME BR-Analyse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Analyse C-Win
ON VALUE-CHANGED OF BR-Analyse IN FRAME DEFAULT-FRAME
DO:
    IF NOT AVAIL preemanalyse THEN
        RETURN.
    FIND Butikkteam NO-LOCK WHERE Butikkteam.BrGrpNr    = 1 AND
                                  Butikkteam.TeamTypeId = 2 AND
                                  Butikkteam.TeamNr     = preemanalyse.teamnr NO-ERROR.
    IF NOT AVAIL Butikkteam THEN DO:
        MESSAGE "Felaktigt rapportteam"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        FI_teamnamn = "".
    END.
    ASSIGN FI_AnalyseId = preemanalyse.AnalyseId 
           FI_Navn      = preemanalyse.Navn 
           FI_SluttDato = preemanalyse.SluttDato 
           FI_StartDato = preemanalyse.StartDato
           FI_Aktiv     = preemanalyse.Aktiv
           FI_teamnr    = preemanalyse.Teamnr
           FI_teamnamn  = butikkteam.beskrivelse.
    DISPLAY
      FI_AnalyseId 
      FI_Navn 
      FI_SluttDato 
      FI_StartDato
      FI_Aktiv
      FI_teamnr
      FI_teamnamn   WITH FRAME {&FRAME-NAME}.
{&OPEN-QUERY-BR-AnalyseRad}
    APPLY "VALUE-CHANGED" TO BROWSE BR-Analyserad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-AnalyseRad
&Scoped-define SELF-NAME BR-AnalyseRad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-AnalyseRad C-Win
ON VALUE-CHANGED OF BR-AnalyseRad IN FRAME DEFAULT-FRAME
DO:
    IF NOT AVAIL preemanalyseRad THEN DO:
        ASSIGN FI2_AnalyseId = IF AVAIL preemanalyse then preemanalyse.AnalyseId ELSE 0 
               FI2_beskr     = ""
               FI2_radnr     = 0
               FI2_ctyp      = "".
    END.
    ELSE DO:
        FIND TT_Typ WHERE TT_Typ.iType = preemanalyserad.typ.

        ASSIGN FI2_AnalyseId = preemanalyserad.AnalyseId
               FI2_beskr     = preemanalyserad.beskr
               FI2_radnr     = preemanalyserad.radnr
               FI2_ctyp      = tt_typ.cNamn.
    END.
    DISPLAY FI2_AnalyseId 
            FI2_beskr    
            FI2_radnr     
            FI2_ctyp  WITH FRAME {&FRAME-NAME}.
    IF preemanalyserad.typ = 4 THEN DO:
        FI3_Element:LABEL = "Artikelnr".
        FI3_KampTilbId:HIDDEN = TRUE.
    END.
    ELSE IF preemanalyserad.typ = 5 THEN DO:
        FI3_Element:LABEL = "Kampid".
        FI3_KampTilbId:HIDDEN = FALSE.
    END.
/*     IF preemanalyserad.typ = 4 OR preemanalyserad.typ = 5 THEN DO:     */
/*         IF preemanalyserad.typ = 5 AND iArtikelnrCol = 1 THEN DO:      */
/*             BR-AnalyseMbr:MOVE-COLUMN(2,4).                            */
/*             BR-AnalyseMbr:MOVE-COLUMN(1,4).                            */
/*             iArtikelnrCol = 4.                                         */
/*         END.                                                           */
/*         ELSE IF preemanalyserad.typ = 4 AND iArtikelnrCol = 4 THEN DO: */
/*             BR-AnalyseMbr:MOVE-COLUMN(4,1).                            */
/*             BR-AnalyseMbr:MOVE-COLUMN(4,2).                            */
/*             iArtikelnrCol = 1.                                         */
/*         END.                                                           */
/*     END.                                                               */
{&OPEN-QUERY-BR-AnalyseMbr}
    APPLY "VALUE-CHANGED" TO BROWSE BR-AnalyseMbr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Analyse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitType.
  RUN enable_UI.
  RUN Autoresize.
  APPLY "VALUE-CHANGED" TO BR-Analyse.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Autoresize C-Win 
PROCEDURE Autoresize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE wW AS HANDLE     NO-UNDO.
/*     ASSIGN wW        = BROWSE BR-analysembr:FIRST-COLUMN. */
/*     REPEAT WHILE VALID-HANDLE(wW):                        */
/*         ASSIGN wW:AUTO-RESIZE = TRUE                      */
/*               wW:LABEL        = TRIM(wW:LABEL)            */
/*                wW             = wW:NEXT-COLUMN.           */
/*     END.                                                  */
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
  DISPLAY FI_AnalyseId FI_Navn FI_teamnr FI_Teamnamn FI_StartDato FI_SluttDato 
          FI_Aktiv FI2_AnalyseId FI2_radnr FI2_ctyp FI2_beskr FI3_Element 
          FI3_KampTilbId FI3_beskr FI-Rapporttxt FI-Rapportradtxt 
          FI-Rapportradmbrtxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-EndreRad-2 RECT-1 RECT-2 RECT-3 BR-Analyse BR-AnalyseRad 
         BR-analysembr B-NyRadMbr-2 FI3_Element FI3_KampTilbId B-NyRadMbr 
         B-Endre B-EndreRad B-Ny B-NyRad 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitType C-Win 
PROCEDURE InitType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii    AS INTEGER     NO-UNDO.
DO ii = 1 TO NUM-ENTRIES(cTypeList).
    CREATE TT_Typ.
    ASSIGN iTyp = ii
           cNamn = ENTRY(ii,cTypeList).
    cTypelistItems = cTypelistItems + (IF cTypelistItems <> "" THEN "," ELSE "") +
                         TT_Typ.cNamn + "," + STRING(ii).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaFasta C-Win 
PROCEDURE SkapaFasta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DO ii = 1 TO 3:
        CREATE preemanalyserad.
        ASSIGN preemanalyserad.Analyseid = preemanalyse.AnalyseId
               preemanalyserad.radnr     = ii
               preemanalyserad.typ       = ii
               preemanalyserad.beskr     = ENTRY(ii,cTypeList).   
    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBeskr C-Win 
FUNCTION getBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF preemanalysembr.artikkelnr <> 0 THEN DO:
      FIND preemanalyserad OF preemanalysembr NO-LOCK.
      IF preemanalyserad.typ = 4 THEN DO:
          FIND artbas WHERE artbas.artikkelnr = preemanalysembr.artikkelnr NO-LOCK NO-ERROR.
          RETURN IF avail artbas THEN artbas.beskr ELSE "OKÄND".
      END.
      ELSE IF preemanalyserad.typ = 6 THEN DO:
          FIND vargr WHERE vargr.vg = INT(preemanalysembr.artikkelnr) NO-LOCK NO-ERROR.
          RETURN IF avail vargr THEN vargr.vgbeskr ELSE "OKÄND".
      END.
  END.
  ELSE DO:
      FIND Kampanjetilbud WHERE kampanjetilbud.Kampid = preemanalysembr.kampid AND
                                kampanjetilbud.KamptilbId = preemanalysembr.kamptilbid NO-LOCK NO-ERROR.
      RETURN IF AVAIL kampanjetilbud THEN KampanjeTilbud.KampTilbNavn ELSE "OKÄND".
  END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

