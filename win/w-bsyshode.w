&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  Forfatter:
  Beskrivelse:  
  Parametere:
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR wSysHId LIKE SysHode.SysHId NO-UNDO.
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER wSysHId LIKE SysHode.SysHId NO-UNDO.
&ENDIF


/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell  SysHode
&scoped-define DataType   INT /* INT STRING DATE  Konvertering av key-felt*/
&scoped-define OptWhere 
&scoped-define BROWSE-NAME-1 BROWSE-{&br-tabell}

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  SysHId
&scoped-define sok-field1-def  INT
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} /* DESCENDING */
&scoped-define index-col1  SysHode
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE ~
               {&OptWhere} ~
                b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Kolonne 2 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field2  Beskrivelse
&scoped-define sok-field2-def  CHAR
&scoped-define sortby-col2 {&br-tabell}.{&sok-field2}
&scoped-define sortby-phrase2 {&br-tabell}.{&sok-field2} /* DESCENDING */
&scoped-define index-col2  Beskrivelse
&scoped-define sok-phrase2 FIND FIRST b{&br-tabell} WHERE ~
               {&OptWhere} ~
               b{&br-tabell}.{&sok-field2} >= ~
               INPUT FILL-IN-SOK-{&sok-field2-def} USE-INDEX {&index-col2} NO-LOCK NO-ERROR.

/* Om du vill sortera på fler kolonner:
   Kopiera kolonnedefinition 1 ovan och ændra talet 1 till næsta ikke anvænda.
   Ændra sedan alla fælt.
   Ændra ant-sok-field nedan till det antall som du vill sortera på.
   Ændra init-datatype nedan till datatyp før den sorteringskolonne du startar på
   Ændra æven alla andra scoped-define før att passa ditt program
   Ændra wAktivCol nedan till INIT ønskad startkolonne.
*/

&scoped-define ip-variabel wSysHId
&scoped-define return-ip   wSysHId = if available {&br-tabell}  ~
                                       then {&br-tabell}.{&sok-field1} ~
                                       else  {&DataType}("").
&scoped-define assign-retur-verdi retur-verdi = if available {&br-tabell} ~
                                                  then string(recid({&br-tabell})) ~
                                                  else "".
&scoped-define SletteMelding "Skal parameterhode slettes? "
&scoped-define KanSlettes  ~
  if can-find(first SysGruppe of SysHode) then ~
    do: ~
      message "Det ligger parametergrupper under dette hode!" skip ~
              "Hode kan ikke slettes." ~
              view-as alert-box message title "Melding". ~
      return no-apply. ~
    end.

&scoped-define ant-sok-field   2
&scoped-define init-datatype   INT /* CHAR, INT, DATE */
&scoped-define VedlikeholdsRutine d-v{&br-tabell}.w

/* Om du ønskar en startup-record annars fjern. */
&scoped-define init-phrase  FIND b{&br-tabell} WHERE ~
                           {&OptWhere} ~
                           b{&br-tabell}.{&sok-field1} = ~
                           {&ip-variabel} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Denna måste finnas før riktig sortering vid uppstart */
&scoped-define init-sortby-phrase b{&br-tabell}.{&sok-field1} /* DESCENDING */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.

/* Lokale variabler ---                                                 */

DEF VAR retur-verdi AS CHAR INITIAL "AVBRYT" NO-UNDO.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEFINE VAR wRecid            AS RECID       NO-UNDO.
DEFINE VAR wOk               AS LOG         NO-UNDO.

{runlib.i}

DEF VAR cFileName        AS CHAR  NO-UNDO. 
DEF VAR cExcEkstent      AS CHAR  NO-UNDO.
DEF VAR cKunde           AS CHAR  NO-UNDO.
DEF VAR cPOS             AS CHAR  NO-UNDO.

DEF STREAM sExportFile.
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-SysGruppe

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES SysGruppe SysHode SysPara

/* Definitions for BROWSE BROWSE-SysGruppe                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-SysGruppe SysGruppe.SysGr ~
SysGruppe.Beskrivelse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-SysGruppe 
&Scoped-define QUERY-STRING-BROWSE-SysGruppe FOR EACH SysGruppe ~
      WHERE SysGruppe.SysHId = SysHode.SysHId NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-SysGruppe OPEN QUERY BROWSE-SysGruppe FOR EACH SysGruppe ~
      WHERE SysGruppe.SysHId = SysHode.SysHId NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-SysGruppe SysGruppe
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-SysGruppe SysGruppe


/* Definitions for BROWSE BROWSE-SysHode                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-SysHode SysHode.SysHId ~
SysHode.Beskrivelse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-SysHode SysHode.SysHId ~
SysHode.Beskrivelse 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-SysHode SysHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-SysHode SysHode
&Scoped-define QUERY-STRING-BROWSE-SysHode FOR EACH SysHode NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-SysHode OPEN QUERY BROWSE-SysHode FOR EACH SysHode NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-SysHode SysHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-SysHode SysHode


/* Definitions for BROWSE BROWSE-SysPara                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-SysPara SysPara.ParaNr ~
SysPara.Beskrivelse SysPara.Parameter1 SysPara.Parameter2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-SysPara 
&Scoped-define QUERY-STRING-BROWSE-SysPara FOR EACH SysPara ~
      WHERE SysPara.SysHId = SysGruppe.SysHId and ~
SysPara.SysGr  = SysGruppe.SysGr NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-SysPara OPEN QUERY BROWSE-SysPara FOR EACH SysPara ~
      WHERE SysPara.SysHId = SysGruppe.SysHId and ~
SysPara.SysGr  = SysGruppe.SysGr NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-SysPara SysPara
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-SysPara SysPara


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-SysGruppe}~
    ~{&OPEN-QUERY-BROWSE-SysHode}~
    ~{&OPEN-QUERY-BROWSE-SysPara}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-SysPara BROWSE-SysHode ~
BROWSE-SysGruppe Btn-help FILL-IN-SOK-INT FILL-IN-SOK-CHAR FILL-IN-SOK-DATE ~
BUTTON-Sok BUTTON-Ny BUTTON-Ny-2 BUTTON-Endre BUTTON-Endre-2 BUTTON-Slett ~
BUTTON-Slett-2 BUTTON-Ny-3 BUTTON-Endre-3 BUTTON-Slett-3 Btn_Avslutt ~
BUTTON-Eksport RECT-3 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-INT FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DATE FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-4 FI-Parameter1 ~
FI-Parameter2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-help 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS
     LABEL "Hjelp" 
     SIZE 4.6 BY 1.05 TOOLTIP "Hjelp".

DEFINE BUTTON Btn_Avslutt 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lukk vinduet".

DEFINE BUTTON BUTTON-Eksport 
     IMAGE-UP FILE "icon/excel.bmp":U
     LABEL "&Eksport" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett".

DEFINE BUTTON BUTTON-Endre 
     LABEL "Endre..." 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Endre-2 
     LABEL "En&dre..." 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Endre-3 
     LABEL "&Endre..." 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "Ny..." 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Ny-2 
     LABEL "N&y..." 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Ny-3 
     LABEL "&Ny..." 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "Slett" 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Slett-2 
     LABEL "S&lett" 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Slett-3 
     LABEL "&Slett" 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1.

DEFINE VARIABLE FI-Parameter1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 139 BY .95 NO-UNDO.

DEFINE VARIABLE FI-Parameter2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 139 BY .95 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Parameterhode" 
     VIEW-AS FILL-IN 
     SIZE 53 BY .95
     BGCOLOR 1 FGCOLOR 15 FONT 3 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Parametergruppe" 
     VIEW-AS FILL-IN 
     SIZE 71 BY .95
     BGCOLOR 1 FGCOLOR 15 FONT 3 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Parameter" 
     VIEW-AS FILL-IN 
     SIZE 139 BY .95
     BGCOLOR 1 FGCOLOR 15 FONT 3 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Parameterverdi" 
     VIEW-AS FILL-IN 
     SIZE 139 BY .95
     BGCOLOR 1 FGCOLOR 15 FONT 3 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INT AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 154 BY .1.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 154 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-SysGruppe FOR 
      SysGruppe SCROLLING.

DEFINE QUERY BROWSE-SysHode FOR 
      SysHode SCROLLING.

DEFINE QUERY BROWSE-SysPara FOR 
      SysPara SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-SysGruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-SysGruppe C-Win _STRUCTURED
  QUERY BROWSE-SysGruppe NO-LOCK DISPLAY
      SysGruppe.SysGr COLUMN-LABEL "Nr *" FORMAT ">>>9":U WIDTH 4.2
      SysGruppe.Beskrivelse COLUMN-LABEL "Beskrivelse *" FORMAT "X(61)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 71 BY 7.38 ROW-HEIGHT-CHARS .62.

DEFINE BROWSE BROWSE-SysHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-SysHode C-Win _STRUCTURED
  QUERY BROWSE-SysHode NO-LOCK DISPLAY
      SysHode.SysHId COLUMN-LABEL "SysHId *" FORMAT ">>>9":U
      SysHode.Beskrivelse COLUMN-LABEL "Beskrivelse *" FORMAT "X(30)":U
            WIDTH 39.4
  ENABLE
      SysHode.SysHId
      SysHode.Beskrivelse
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 53 BY 7.38.

DEFINE BROWSE BROWSE-SysPara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-SysPara C-Win _STRUCTURED
  QUERY BROWSE-SysPara NO-LOCK DISPLAY
      SysPara.ParaNr COLUMN-LABEL "Nr *" FORMAT ">>>>>9":U
      SysPara.Beskrivelse FORMAT "X(60)":U WIDTH 42.4
      SysPara.Parameter1 FORMAT "X(80)":U WIDTH 43.4
      SysPara.Parameter2 FORMAT "X(80)":U WIDTH 41
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 139 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-SysPara AT ROW 12.43 COL 2
     BROWSE-SysHode AT ROW 3.86 COL 2
     BROWSE-SysGruppe AT ROW 3.86 COL 70
     Btn-help AT ROW 1.29 COL 145 HELP
          "Felthjelp"
     FILL-IN-SOK-INT AT ROW 1.33 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.33 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.33 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 1.33 COL 21.6
     FILL-IN-1 AT ROW 2.67 COL 2 NO-LABEL
     FILL-IN-2 AT ROW 2.67 COL 70 NO-LABEL
     BUTTON-Ny AT ROW 3.86 COL 56
     BUTTON-Ny-2 AT ROW 3.86 COL 142
     BUTTON-Endre AT ROW 5.05 COL 56
     BUTTON-Endre-2 AT ROW 5.05 COL 142
     BUTTON-Slett AT ROW 6.24 COL 56
     BUTTON-Slett-2 AT ROW 6.24 COL 142
     FILL-IN-3 AT ROW 11.48 COL 2 NO-LABEL
     BUTTON-Ny-3 AT ROW 12.43 COL 142
     BUTTON-Endre-3 AT ROW 13.62 COL 142
     BUTTON-Slett-3 AT ROW 14.81 COL 142
     FILL-IN-4 AT ROW 23.86 COL 2 NO-LABEL
     FI-Parameter1 AT ROW 24.81 COL 2 NO-LABEL
     FI-Parameter2 AT ROW 25.76 COL 2 NO-LABEL
     Btn_Avslutt AT ROW 1.29 COL 150
     BUTTON-Eksport AT ROW 1.33 COL 33
     RECT-3 AT ROW 1.1 COL 1
     RECT-4 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154.6 BY 25.91.


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
         TITLE              = "Systemparametre"
         HEIGHT             = 25.91
         WIDTH              = 154.6
         MAX-HEIGHT         = 25.91
         MAX-WIDTH          = 154.6
         VIRTUAL-HEIGHT     = 25.91
         VIRTUAL-WIDTH      = 154.6
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-SysPara 1 DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-SysHode BROWSE-SysPara DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-SysGruppe BROWSE-SysHode DEFAULT-FRAME */
ASSIGN 
       BROWSE-SysPara:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1.

/* SETTINGS FOR FILL-IN FI-Parameter1 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Parameter2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-SysGruppe
/* Query rebuild information for BROWSE BROWSE-SysGruppe
     _TblList          = "skotex.SysGruppe"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "SysGruppe.SysHId = SysHode.SysHId"
     _FldNameList[1]   > skotex.SysGruppe.SysGr
"SysGruppe.SysGr" "Nr *" ? "integer" ? ? ? ? ? ? no ? no no "4.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.SysGruppe.Beskrivelse
"SysGruppe.Beskrivelse" "Beskrivelse *" "X(61)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-SysGruppe */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-SysHode
/* Query rebuild information for BROWSE BROWSE-SysHode
     _TblList          = "skotex.SysHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > skotex.SysHode.SysHId
"SysHode.SysHId" "SysHId *" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.SysHode.Beskrivelse
"SysHode.Beskrivelse" "Beskrivelse *" ? "character" ? ? ? ? ? ? yes ? no no "39.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-SysHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-SysPara
/* Query rebuild information for BROWSE BROWSE-SysPara
     _TblList          = "skotex.SysPara"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "SysPara.SysHId = SysGruppe.SysHId and
SysPara.SysGr  = SysGruppe.SysGr"
     _FldNameList[1]   > skotex.SysPara.ParaNr
"SysPara.ParaNr" "Nr *" ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.SysPara.Beskrivelse
"SysPara.Beskrivelse" ? "X(60)" "character" ? ? ? ? ? ? no ? no no "42.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > skotex.SysPara.Parameter1
"SysPara.Parameter1" ? "X(80)" "character" ? ? ? ? ? ? no ? no no "43.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > skotex.SysPara.Parameter2
"SysPara.Parameter2" ? "X(80)" "character" ? ? ? ? ? ? no ? no no "41" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-SysPara */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Systemparametre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Systemparametre */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-SysGruppe
&Scoped-define SELF-NAME BROWSE-SysGruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysGruppe C-Win
ON DEFAULT-ACTION OF BROWSE-SysGruppe IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO BUTTON-Endre-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysGruppe C-Win
ON VALUE-CHANGED OF BROWSE-SysGruppe IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BROWSE-SysPara}  
  IF AVAILABLE SysGruppe THEN
  FIND FIRST SysPara OF SysGruppe NO-LOCK NO-ERROR.
  IF AVAILABLE SysPara THEN
    DISPLAY
      SysPara.Parameter1 @ FI-Parameter1 
      SysPara.Parameter2 @ FI-Parameter2
    WITH FRAME DEFAULT-FRAME.
  ELSE
    DISPLAY
      "" @ FI-Parameter1 
      "" @ FI-Parameter2
    WITH FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-SysHode
&Scoped-define SELF-NAME BROWSE-SysHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysHode C-Win
ON ANY-PRINTABLE OF BROWSE-SysHode IN FRAME DEFAULT-FRAME
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysHode C-Win
ON CURSOR-LEFT OF BROWSE-SysHode IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysHode C-Win
ON CURSOR-RIGHT OF BROWSE-SysHode IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysHode C-Win
ON DEFAULT-ACTION OF BROWSE-SysHode IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO BROWSE-SysHode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysHode C-Win
ON START-SEARCH OF BROWSE-SysHode IN FRAME DEFAULT-FRAME
DO:
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysHode C-Win
ON VALUE-CHANGED OF BROWSE-SysHode IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BROWSE-SysGruppe}
  IF AVAILABLE {&br-tabell} THEN
    FIND FIRST SysGruppe OF {&br-tabell} NO-LOCK NO-ERROR.

  IF AVAILABLE SysGruppe THEN
    FIND FIRST SysPara OF SysGruppe NO-LOCK NO-ERROR.

  {&OPEN-QUERY-BROWSE-SysPara}
  IF AVAILABLE SysPara THEN
    DO:
      DISPLAY
        SysPara.Parameter1 @ FI-Parameter1 
        SysPara.Parameter2 @ FI-Parameter2
      WITH FRAME DEFAULT-FRAME.
    END.
  ELSE
    DISPLAY
      "" @ FI-Parameter1 
      "" @ FI-Parameter2
    WITH FRAME DEFAULT-FRAME.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-SysPara
&Scoped-define SELF-NAME BROWSE-SysPara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysPara C-Win
ON DEFAULT-ACTION OF BROWSE-SysPara IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO BUTTON-Endre-3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysPara C-Win
ON TAB OF BROWSE-SysPara IN FRAME DEFAULT-FRAME
DO:
  APPLY "entry":U TO BROWSE-SysHode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SysPara C-Win
ON VALUE-CHANGED OF BROWSE-SysPara IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE SysPara THEN
    DISPLAY
      SysPara.Parameter1 @ FI-Parameter1 
      SysPara.Parameter2 @ FI-Parameter2
    WITH FRAME DEFAULT-FRAME.
  ELSE
  DISPLAY
    "" @ FI-Parameter1 
    "" @ FI-Parameter2
  WITH FRAME DEFAULT-FRAME.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-help C-Win
ON CHOOSE OF Btn-help IN FRAME DEFAULT-FRAME /* Hjelp */
DO:
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Avslutt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Avslutt C-Win
ON CHOOSE OF Btn_Avslutt IN FRAME DEFAULT-FRAME
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Eksport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Eksport C-Win
ON CHOOSE OF BUTTON-Eksport IN FRAME DEFAULT-FRAME /* Eksport */
DO:
  RUN Utskrift.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME DEFAULT-FRAME /* Endre... */
DO:
  ASSIGN
    wRecid = RECID({&br-tabell}).

  IF SEARCH("{&VedlikeholdsRutine}") <> ? THEN
    DO:
      RUN {&VedlikeholdsRutine} (INPUT-OUTPUT wRecid,"Endre").
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
      wOk = BROWSE-{&br-tabell}:REFRESH().
    END.
  ELSE DO:
    MESSAGE "Ukjent program!" "{&VedlikeholdsRutine}"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre-2 C-Win
ON CHOOSE OF BUTTON-Endre-2 IN FRAME DEFAULT-FRAME /* Endre... */
DO:
  IF NOT AVAILABLE SysHode THEN
    RETURN.
  IF NOT AVAILABLE SysGruppe THEN
    RETURN NO-APPLY.
  ASSIGN
    wRecid = RECID(SysGruppe).

  IF SEARCH("d-vsysgruppe.r") <> ? THEN
    DO:
      RUN d-vsysgruppe.w (INPUT-OUTPUT wRecid,"Endre",RECID(SysHode)).
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
      wOk = BROWSE-SysGruppe:REFRESH().
    END.
  ELSE DO:
    MESSAGE "Ukjent program!" "d-vsysgruppe.r"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre-3 C-Win
ON CHOOSE OF BUTTON-Endre-3 IN FRAME DEFAULT-FRAME /* Endre... */
DO:
  IF NOT AVAILABLE SysHode THEN
    RETURN NO-APPLY.
  IF NOT AVAILABLE SysGruppe THEN
    RETURN NO-APPLY.
  IF NOT AVAILABLE SysPara THEN
    RETURN NO-APPLY.

  ASSIGN
    wRecid = RECID(SysPara).

  IF SEARCH("d-vsyspara.r") <> ? THEN
    DO:
      RUN d-vsyspara.w (INPUT-OUTPUT wRecid,"Endre",RECID(SysGruppe)).
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
      wOk = BROWSE-SysPara:REFRESH().
    END.
  ELSE DO:
    MESSAGE "Ukjent program!" "d-vsyspara.r"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny... */
DO:
  ASSIGN 
    wRecid = ?.
  IF SEARCH("{&VedlikeholdsRutine}") <> ? THEN
    DO:
      RUN {&VedlikeholdsRutine} (INPUT-OUTPUT wRecid,"Ny").
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    END.
  ELSE DO:
    MESSAGE "Ukjent program!" "{&VedlikeholdsRutine}"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.

  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  FIND b{&br-tabell} NO-LOCK WHERE
    RECID(b{&br-tabell}) = wRecid NO-ERROR.
  RUN SD-Reposition.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny-2 C-Win
ON CHOOSE OF BUTTON-Ny-2 IN FRAME DEFAULT-FRAME /* Ny... */
DO:
  IF NOT AVAILABLE SysHode THEN
    RETURN NO-APPLY.

  ASSIGN 
    wRecid = ?.
  IF SEARCH("d-vsysgruppe.r") <> ? THEN
    DO:
      RUN d-vsysgruppe.w (INPUT-OUTPUT wRecid,"Ny",RECID(SysHode)).
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    END.
  ELSE DO:
    MESSAGE "Ukjent program!" "d-vsysgruppe.r"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.


  {&OPEN-QUERY-BROWSE-SysGruppe}
  FIND SysGruppe NO-LOCK WHERE
    RECID(SysGruppe) = wRecid NO-ERROR.
   REPOSITION BROWSE-SysGruppe TO ROWID ROWID(SysGruppe) NO-ERROR.
   APPLY "ENTRY" TO BROWSE BROWSE-SysGruppe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny-3 C-Win
ON CHOOSE OF BUTTON-Ny-3 IN FRAME DEFAULT-FRAME /* Ny... */
DO:
  IF NOT AVAILABLE SysHode THEN
    RETURN NO-APPLY.
  IF NOT AVAILABLE SysGruppe THEN
    RETURN NO-APPLY.

  ASSIGN 
    wRecid = ?.
  IF SEARCH("d-vsyspara.r") <> ? THEN
    DO:
      RUN d-vsyspara.w (INPUT-OUTPUT wRecid,"Ny",RECID(SysGruppe)).
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    END.
  ELSE DO:
    MESSAGE "Ukjent program!" "d-vsyspara.r"
      VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.


  {&OPEN-QUERY-BROWSE-SysPara}
  FIND SysPara NO-LOCK WHERE
    RECID(SysPara) = wRecid NO-ERROR.
   REPOSITION BROWSE-SysPara TO ROWID ROWID(SysPara) NO-ERROR.
   APPLY "ENTRY" TO BROWSE BROWSE-SysPara.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
  IF NOT AVAILABLE SysHode THEN
    RETURN.
    
  MESSAGE {&SletteMelding}
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wOk.
  IF wOk = TRUE THEN
    DO TRANSACTION:
      {&KanSlettes}
      FIND b{&br-tabell} EXCLUSIVE-LOCK WHERE
        RECID(b{&br-tabell}) = recid({&br-tabell}) NO-ERROR.
      IF AVAILABLE b{&br-tabell} THEN
        DO:
          DELETE b{&br-tabell}.
          wOk = BROWSE-{&br-tabell}:DELETE-CURRENT-ROW( ).
        END.
    END.
  ELSE RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett-2 C-Win
ON CHOOSE OF BUTTON-Slett-2 IN FRAME DEFAULT-FRAME /* Slett */
DO:
  IF NOT AVAILABLE SysGruppe THEN
    RETURN.
    
  MESSAGE "Skal parametergruppen slettes?"
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wOk.
  IF wOk = TRUE THEN
    DO TRANSACTION:
      IF CAN-FIND(FIRST SysPara OF SysGruppe) THEN
        DO:
          MESSAGE "Det finnes parametre under denne parametergruppen!" SKIP
                  "Kan ikke slettes." VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
          RETURN NO-APPLY.
        END.
      
      FIND CURRENT SysGruppe EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE SysGruppe THEN
        DO:
          DELETE SysGruppe.
          wOk = BROWSE-SysGruppe:DELETE-CURRENT-ROW( ).
        END.
    END.
  ELSE RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett-3 C-Win
ON CHOOSE OF BUTTON-Slett-3 IN FRAME DEFAULT-FRAME /* Slett */
DO:
  IF NOT AVAILABLE SysHode THEN
    RETURN NO-APPLY.
  IF NOT AVAILABLE SysGruppe THEN
    RETURN NO-APPLY.
  IF NOT AVAILABLE SysPAra THEN
    RETURN NO-APPLY.

  MESSAGE "Skal parameteret slettes?"
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wOk.
  IF wOk = TRUE THEN
    DO TRANSACTION:
      FIND CURRENT SysPara EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE SysPara THEN
        DO:
          DELETE SysPara.
          wOk = BROWSE-SysPara:DELETE-CURRENT-ROW( ).
        END.
    END.
  ELSE RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok C-Win
ON CHOOSE OF BUTTON-Sok IN FRAME DEFAULT-FRAME /* Søk */
DO:
   DEFINE VAR wBlank AS LOG NO-UNDO.
   CASE wAktivCol:
       WHEN 1 THEN DO:
           IF FILL-IN-SOK-{&sok-field1-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase1}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field1-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &IF DEFINED(sok-phrase2) &THEN 
       WHEN 2 THEN DO:
           IF FILL-IN-SOK-{&sok-field2-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase2}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field2-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &ENDIF
       &IF DEFINED(sok-phrase3) &THEN 
       WHEN 3 THEN DO:
           IF FILL-IN-SOK-{&sok-field3-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase3}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field3-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &ENDIF
       &IF DEFINED(sok-phrase4) &THEN 
       WHEN 4 THEN DO:
           IF FILL-IN-SOK-{&sok-field4-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase4}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field4-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &ENDIF
   END CASE.
   IF wBlank THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME-1}.
   ELSE
       RUN SD-Reposition.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-CHAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-CHAR C-Win
ON RETURN OF FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-DATE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE C-Win
ON LEAVE OF FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR wDate AS DATE.
    wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feilaktigt tastet dato." VIEW-AS ALERT-BOX ERROR TITLE "Feil dato".
        APPLY "ENTRY" TO FILL-IN-SOK-DATE.
        RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE C-Win
ON RETURN OF FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR wDate AS DATE.
    wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feilaktigt tastet dato." VIEW-AS ALERT-BOX ERROR TITLE "Feil dato".
        APPLY "ENTRY" TO FILL-IN-SOK-DATE.
        RETURN NO-APPLY.
    END.
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-INT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-INT C-Win
ON RETURN OF FILL-IN-SOK-INT IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-SysGruppe
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"EmbedMe") THEN
  DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE).

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  DO:
    {inutmld.i &Modus = "Slett"} /* Melder fra at programmet har stopper. */
    {stopplib.i} /* Starter prosedurebibloteket */
    RUN disable_UI.
  END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  wOrgBgCol = {&sortby-col1}:Label-bgcolor in browse {&BROWSE-NAME-1}.
  RUN LabelColor.
  RUN Move-Fill-To-Top.
  RUN enable_UI.
  {lng.i} 

  IF {&ant-sok-field} > 1 THEN
      RUN Init-Read-Only.
  &IF DEFINED(init-phrase) &THEN
  {&init-phrase}
  IF NOT AVAILABLE b{&br-tabell} THEN
    FIND FIRST b{&br-tabell} NO-ERROR.
  IF AVAILABLE b{&br-tabell} THEN DO:
      RUN SD-Reposition.
     FIND FIRST SysGruppe OF b{&br-tabell} NO-ERROR.
     IF AVAILABLE SysGruppe THEN
       DO:
         {&OPEN-QUERY-BROWSE-SysGruppe}
         FIND FIRST SysPara OF SysGruppe NO-LOCK NO-ERROR.
       END.
     IF AVAILABLE SysPara THEN
       DO:
         {&OPEN-QUERY-BROWSE-SysPara}
         DISPLAY
           SysPara.Parameter1 @ FI-Parameter1 
           SysPara.Parameter2 @ FI-Parameter2
         WITH FRAME DEFAULT-FRAME.
       END.
  END.
  &ENDIF

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                    "BROWSE-SysHode").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                    "BROWSE-SysHode,BROWSE-SysGruppe,RECT-3,RECT-4").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                    "FILL-IN-2,FILL-IN-3,FILL-IN-4").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,400,400,0,0).

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.

  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME-1}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl\wintrigg.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytSortering C-Win 
PROCEDURE BytSortering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wNyCol AS INT NO-UNDO.
  DEFINE VAR             wDataType AS CHAR NO-UNDO.
  IF wNyCol = wAktivCol THEN
      RETURN NO-APPLY.
  ASSIGN wAktivCol = wNyCol.
  CASE wNyCol:
    WHEN 1 THEN DO:
        &scope SORTBY-PHRASE BY {&sortby-phrase1}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col1}:DATA-TYPE IN BROWSE {&BROWSE-NAME-1}.
    END.
    &IF DEFINED(sortby-phrase2) &THEN
    WHEN 2 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase2}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col2}:DATA-TYPE IN BROWSE {&BROWSE-NAME-1}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase3) &THEN
    WHEN 3 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase3}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col3}:DATA-TYPE IN BROWSE {&BROWSE-NAME-1}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase4) &THEN
    WHEN 4 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase4}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col4}:DATA-TYPE IN BROWSE {&BROWSE-NAME-1}.
     END.
     &ENDIF
  END CASE.
  CASE wDataType:
      WHEN "INTEGER" THEN
          wAktivDataType = "INT".
      WHEN "CHARACTER" THEN
          wAktivDataType = "CHAR".
      WHEN "DATE" THEN
          wAktivDataType = "DATE".
  END CASE.
  &scope SORTBY-PHRASE BY {&sortby-phrase1}
  RUN Move-Fill-To-Top.
  RUN LabelColor.
  RETURN NO-APPLY.
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
  DISPLAY FILL-IN-SOK-INT FILL-IN-SOK-CHAR FILL-IN-SOK-DATE FILL-IN-1 FILL-IN-2 
          FILL-IN-3 FILL-IN-4 FI-Parameter1 FI-Parameter2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BROWSE-SysPara BROWSE-SysHode BROWSE-SysGruppe Btn-help 
         FILL-IN-SOK-INT FILL-IN-SOK-CHAR FILL-IN-SOK-DATE BUTTON-Sok BUTTON-Ny 
         BUTTON-Ny-2 BUTTON-Endre BUTTON-Endre-2 BUTTON-Slett BUTTON-Slett-2 
         BUTTON-Ny-3 BUTTON-Endre-3 BUTTON-Slett-3 Btn_Avslutt BUTTON-Eksport 
         RECT-3 RECT-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Read-Only C-Win 
PROCEDURE Init-Read-Only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
    {&sortby-col1}:READ-ONLY IN BROWSE {&BROWSE-NAME-1} = YES
    &IF DEFINED(sortby-col2) &THEN
    {&sortby-col2}:READ-ONLY IN BROWSE {&BROWSE-NAME-1} = YES
    &ENDIF
    &IF DEFINED(sortby-col3) &THEN
    {&sortby-col3}:READ-ONLY IN BROWSE {&BROWSE-NAME-1} = YES
    &ENDIF
    &IF DEFINED(sortby-col4) &THEN
    {&sortby-col4}:READ-ONLY IN BROWSE {&BROWSE-NAME-1} = YES
    &ENDIF
    .
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LabelColor C-Win 
PROCEDURE LabelColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
   {&sortby-col1}:Label-bgcolor IN BROWSE {&BROWSE-NAME-1} = 
                                   IF wAktivCol = 1 THEN wSortBgCol ELSE wOrgBgCol
   &IF DEFINED(sortby-col2) &THEN
   {&sortby-col2}:Label-bgcolor IN BROWSE {&BROWSE-NAME-1} = 
                                   IF wAktivCol = 2 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col3) &THEN
   {&sortby-col3}:Label-bgcolor IN BROWSE {&BROWSE-NAME-1} = 
                                   IF wAktivCol = 3 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col4) &THEN
   {&sortby-col4}:Label-bgcolor IN BROWSE {&BROWSE-NAME-1} = 
                                   IF wAktivCol = 4 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   .
   RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Fill-To-Top C-Win 
PROCEDURE Move-Fill-To-Top :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        CASE wAktivDataType:
             WHEN "INT" THEN
                 IF FILL-IN-SOK-INT:MOVE-TO-TOP() THEN.
             WHEN "CHAR" THEN
                 IF FILL-IN-SOK-CHAR:MOVE-TO-TOP() THEN.
             WHEN "DATE" THEN
                 IF FILL-IN-SOK-DATE:MOVE-TO-TOP() THEN.
        END CASE.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-AnyPrintable C-Win 
PROCEDURE SD-AnyPrintable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CASE wAktivDataType:
        WHEN "INT" OR WHEN "DATE" THEN DO:
            IF key-function(LASTKEY) < "0" OR key-function(LASTKEY) > "9" THEN
                RETURN NO-APPLY.
        END.
    END CASE.
    CASE wAktivDataType:
        WHEN "INT" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-INT.
            APPLY LASTKEY. 
            FILL-IN-SOK-INT:CURSOR-OFFSET = 2.
        END.
        WHEN "CHAR" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-CHAR.
            APPLY LASTKEY.
        END.
        WHEN "DATE" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-DATE.
            APPLY LASTKEY.
        END.
    END CASE.
  END.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Cursor C-Win 
PROCEDURE SD-Cursor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wLeft-Right AS CHAR NO-UNDO.
    CASE wLeft-Right:
        WHEN "LEFT" THEN
            IF wAktivCol = 1 THEN
                RUN SortNyCol({&ant-sok-field}).
            ELSE
                RUN SortNyCol(wAktivCol - 1).
        WHEN "RIGHT" THEN
            IF wAktivCol = {&ant-sok-field} THEN
                RUN SortNyCol(1).
            ELSE
                RUN SortNyCol(wAktivCol + 1).
    END CASE.
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME-1}.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Reposition C-Win 
PROCEDURE SD-Reposition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        REPOSITION {&BROWSE-NAME-1} TO ROWID ROWID(b{&br-tabell}) NO-ERROR.
        CASE wAktivDataType:
            WHEN "INT" THEN
                ASSIGN FILL-IN-SOK-INT:SCREEN-VALUE = "".
            WHEN "CHAR" THEN
                ASSIGN FILL-IN-SOK-CHAR:SCREEN-VALUE = "".
            WHEN "DATE" THEN
                ASSIGN FILL-IN-SOK-DATE:SCREEN-VALUE = "".
        END CASE.
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME-1}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-START-SEARCH C-Win 
PROCEDURE SD-START-SEARCH :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR h-Curr-Col AS widget.
    h-Curr-Col = BROWSE {&BROWSE-NAME-1}:CURRENT-COLUMN.
    APPLY "end-search" TO {&BROWSE-NAME-1} IN FRAME {&FRAME-NAME}.  
    CASE h-Curr-Col:NAME:
        WHEN "{&sok-field1}" THEN
            IF wAktivCol <> 1 THEN DO:
                RUN SortNyCol(1).
                RETURN NO-APPLY.
            END.
        &IF DEFINED(sok-field2) &THEN 
        WHEN "{&sok-field2}" THEN
            IF wAktivCol <> 2 THEN DO:
                RUN SortNyCol(2).
                RETURN NO-APPLY.
            END.
        &ENDIF
        &IF DEFINED(sok-field3) &THEN 
        WHEN "{&sok-field3}" THEN
            IF wAktivCol <> 3 THEN DO:
                RUN SortNyCol(3).
                RETURN NO-APPLY.
            END.
        &ENDIF
        &IF DEFINED(sok-field4) &THEN 
        WHEN "{&sok-field4}" THEN
            IF wAktivCol <> 4 THEN DO:
                RUN SortNyCol(4).
                RETURN NO-APPLY.
            END.
        &ENDIF
    END CASE.
   
    RUN SD-Reposition.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortNyCol C-Win 
PROCEDURE SortNyCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wNyCol     AS INT NO-UNDO.
    DEFINE VAR             wRowID     AS ROWID NO-UNDO.
    DEFINE VAR             wBrowseRow AS INT   NO-UNDO.
    wRowId = ROWID({&br-tabell}).
    wBrowseRow = {&BROWSE-NAME-1}:FOCUSED-ROW IN FRAME {&FRAME-NAME}.
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = wRowId NO-LOCK.
    RUN BytSortering (wNyCol).
    RUN SD-Reposition.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Win 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR piAntPoster AS INT  NO-UNDO.
  DEF VAR pcChr10     AS CHAR NO-UNDO.
  DEF VAR pcTekst     AS CHAR NO-UNDO.

  ASSIGN
    pcChr10 = CHR(10)
    .

  {sww.i}
  
  /* Finner temporært filnavn. */
  IF VALID-HANDLE(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle ("Para", cExcEkstent, OUTPUT cFileName). 
  
MESSAGE cFileName
VIEW-AS ALERT-BOX.  
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(cFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "SYSTEMPARAMETRE"
    ""
    ""
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    "SysHId"
    "Parameterhode"
    "SysGr"
    "Parametergruppe"
    "ParaNr"
    "Bskrivelse"
    "Parameter1"
    "Parameter2"
    "Hjelpetekst1"
    "HjelpeTekst"
    SKIP.                                 
                                  
  /* Eksporterer data */
  EKSPORT:
  FOR EACH SysPara NO-LOCK WHERE
      BREAK BY SysPara.SysHId
            BY SysPara.SysGr
            BY SysPara.ParaNr:

    FIND SysHode NO-LOCK WHERE
      SysHode.SysHId   = SysPara.SysHId NO-ERROR.
    
    FIND SysGruppe NO-LOCK WHERE
      SysGruppe.SysHId   = SysPara.SysHId AND
      SysGruppe.SysGr    = SysPara.SysGr NO-ERROR.

    ASSIGN
        piAntPoster = piAntPoster + 1
        .

    EXPORT STREAM sExportFile DELIMITER ";"
      SysPara.SysHId
      (IF AVAILABLE SysHode
         THEN SysHode.Beskrivelse
         ELSE "")
      SysPara.SysGr
      (IF AVAILABLE SysGruppe
         THEN SysGruppe.Beskrivelse
         ELSE "")
      SysPara.ParaNr
      SysPara.Beskrivelse
      SysPara.Parameter1
      SysPara.Parameter2
      SysPara.Hjelpetekst1
      SysPara.HjelpeTekst2
      .
                                
  END. /* EKSPORT */
                                  
  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:AI2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:AI2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("B:B"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/

  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("A:A"):NumberFormat = "# ##0".
  /*
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  */

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:F1"):Merge().
  chWorkSheets:Range("A1:F1"):HorizontalAlignment = 3.
  /*      
  chWorkSheets:Range("C2:C2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F2:F2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("H2:H2"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("J2:J2"):HorizontalAlignment = 4.   
  */

  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:AI"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:AI2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier - <Blank>".
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(piAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = cKunde.
  chWorkSheets:PageSetup:RightFooter    = cPOS.
  chWorksheets:PageSetup:PrintArea      = "A:AI".
  chWorkSheets:PageSetup:Orientation    = 2. /*LAndscape */
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("C3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = TRUE.
  
  /* Legger inn sumlinjer. */                                        
  /* Excel macro som gjør jobben.
  Range("A4").Select
    Selection.Subtotal GroupBy:=1, Function:=xlSum, TotalList:=Array(5, 7, 9), _
        Replace:=True, PageBreaks:=True, SummaryBelowData:=True  
  */
  STATUS DEFAULT "Setter summeringer...".
  /*chWorkSheets:Range("E4:M50"):Subtotal(1 ,1 ,"5 , 7, 9" ,TRUE ,TRUE ,TRUE ).*/   
  
  chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */

  STATUS DEFAULT "".

  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

