&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  Forfatter:
  Beskrivelse:  Template før søkprogram
  Parametere:
  Endringer:

  STEP 1: Definiera din QUERY
          Kryssa av Indexed-Reposition
          Kryssa av Sort-ByPhrase under Option
  STEP 2: Definiera de fælt du ønskar i din browser
          Kryssa av enabled på de fælt du ønskar sortering

   Programmet ær førberett før 4 søkkolonner.
   Gør de andringar som behøvs i alla scoped-define och følj
   beskrivning nedan.

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

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

DEFINE VAR wTelleNr LIKE TelleHode.TelleNr NO-UNDO.

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell   TelleHode
&scoped-define OptWhere 

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  TelleNr
&scoped-define sok-field1-def  INT /* INT STRING DATE */
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} DESCENDING 
&scoped-define index-col1  TelleHode
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Kolonne 2 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field2  Beskrivelse
&scoped-define sok-field2-def  STRING
&scoped-define sortby-col2 {&br-tabell}.{&sok-field2}
&scoped-define sortby-phrase2 {&br-tabell}.{&sok-field2} /* DESCENDING */
&scoped-define index-col2  Beskrivelse
&scoped-define sok-phrase2 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field2} >= ~
               INPUT FILL-IN-SOK-{&sok-field2-def} USE-INDEX {&index-col2} NO-LOCK NO-ERROR.

&scoped-define sok-field3 TTId
&scoped-define sok-field3-def  INT
&scoped-define sortby-col3 {&br-tabell}.{&sok-field3}
&scoped-define sortby-phrase3 {&br-tabell}.{&sok-field3} /* DESCENDING */
&scoped-define index-col3  TransType
&scoped-define sok-phrase3 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field3} >= ~
               INPUT FILL-IN-SOK-{&sok-field3-def} USE-INDEX {&index-col3} NO-LOCK NO-ERROR.

&scoped-define sok-field4 StartDato
&scoped-define sok-field4-def  DATE
&scoped-define sortby-col4 {&br-tabell}.{&sok-field4}
&scoped-define sortby-phrase4 {&br-tabell}.{&sok-field4} /* DESCENDING */
&scoped-define index-col4  StartDato
&scoped-define sok-phrase4 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field4} >= ~
               INPUT FILL-IN-SOK-{&sok-field4-def} USE-INDEX {&index-col4} NO-LOCK NO-ERROR.

&scoped-define sok-field5 OrdreNr
&scoped-define sok-field5-def  INT
&scoped-define sortby-col5 {&br-tabell}.{&sok-field5}
&scoped-define sortby-phrase5 {&br-tabell}.{&sok-field5} /* DESCENDING */
&scoped-define index-col5  Ordre
&scoped-define sok-phrase5 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field5} >= ~
               INPUT FILL-IN-SOK-{&sok-field5-def} USE-INDEX {&index-col5} NO-LOCK NO-ERROR.

&scoped-define sok-field6 PkSdlNr
&scoped-define sok-field6-def  INT
&scoped-define sortby-col6 {&br-tabell}.{&sok-field6}
&scoped-define sortby-phrase6 {&br-tabell}.{&sok-field6} /* DESCENDING */
&scoped-define index-col6  Pakkseddel
&scoped-define sok-phrase6 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field6} >= ~
               INPUT FILL-IN-SOK-{&sok-field6-def} USE-INDEX {&index-col6} NO-LOCK NO-ERROR.


/* Om du vill sortera på fler kolonner:
   Kopiera kolonnedefinition 1 ovan och ændra talet 1 till næsta ikke anvænda.
   Ændra sedan alla fælt.
   Ændra ant-sok-field nedan till det antall som du vill sortera på.
   Ændra init-datatype nedan till datatyp før den sorteringskolonne du startar på
   Ændra æven alla andra scoped-define før att passa ditt program
   Ændra wAktivCol nedan till INIT ønskad startkolonne.
*/

&scoped-define ip-variabel wTelleNr
&scoped-define return-ip   wTelleNr = if available {&br-tabell}  ~
                                       then {&br-tabell}.{&sok-field1} ~
                                       else  {&sok-field1-def}("").
&scoped-define assign-retur-verdi retur-verdi = if available {&br-tabell} ~
                                                  then string(recid({&br-tabell})) ~
                                                  else "".
&scoped-define ant-sok-field   6
&scoped-define init-datatype   INT /* CHAR, INT, DATE */
&scoped-define VedlikeholdsRutine d-v{&br-tabell}.w

/* Om du ønskar en startup-record annars fjern. */
&scoped-define init-phrase  FIND b{&br-tabell} WHERE ~
                           {&OptWhere} ~
                           b{&br-tabell}.{&sok-field1} = ~
                           {&ip-variabel} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Denna måste finnas før riktig sortering vid uppstart */
&scoped-define init-sortby-phrase {&br-tabell}.{&sok-field1} /* DESCENDING */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.

/* Lokale variabler ---                                                 */
DEF VAR wRecid       AS RECID                 NO-UNDO.
DEF VAR wOk          AS LOG                   NO-UNDO.
DEF VAR retur-verdi  AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEF VAR wCl          AS INT                   NO-UNDO.
DEF VAR wTransNr AS  INT                      NO-UNDO.
DEF VAR wBatchNr AS  INT                      NO-UNDO.
DEF VAR wNedSkriv    AS LOG                   NO-UNDO.
DEF VAR wTekst       AS CHAR                  NO-UNDO.
DEF VAR wEDB-System  AS CHAR                  NO-UNDO.
DEF VAR wTabell      AS CHAR                  NO-UNDO.  
DEF VAR wLinjeHandle AS HANDLE                NO-UNDO.
DEF VAR hInstance   AS INT NO-UNDO.
DEF VAR wExcEkstent       AS CHAR        NO-UNDO.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEF    VAR idags_moms        AS INT  NO-UNDO.
DEF    VAR cTekst            AS CHAR NO-UNDO.
DEF    VAR plDbFaktorNetto   AS DEC  NO-UNDO.
DEF    VAR plDbFaktorBrutto  AS DEC  NO-UNDO.
DEF    VAR plMva%            AS DEC  NO-UNDO.
DEF    VAR cButikkerBgrp     AS CHAR NO-UNDO.

DEF    VAR hUtvalg           AS HANDLE NO-UNDO.
DEF    VAR bUtvalgIsMaster   AS LOG NO-UNDO.
DEFINE VARIABLE cBruker AS CHARACTER  NO-UNDO.
{runlib.i}
{windows.i}

DEF TEMP-TABLE tmpChild
  FIELD wChild AS HANDLE.

DEF BUFFER clButiker FOR Butiker.

DEF STREAM Eksport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TelleHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TelleHode

/* Definitions for BROWSE BROWSE-TelleHode                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TelleHode TelleHode.TelleNr ~
TelleHode.TTId getTranstype() TelleHode.OrdreNr TelleHode.PkSdlNr ~
TelleHode.Beskrivelse TelleHode.BatchNr TelleHode.StartDato ~
TelleHode.Oppdatert TelleHode.AntallPar TelleHode.AntallTalt ~
TelleHode.AntallDiff TelleHode.OpprVerdi TelleHode.OpptVerdi ~
TelleHode.VerdiDiff TelleHode.AntLinjer TelleHode.ButikkListe ~
TelleHode.RegistrertDato TelleHode.RegistrertAv TelleHode.EDato ~
TelleHode.BrukerID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TelleHode TelleHode.TelleNr ~
TelleHode.TTId TelleHode.OrdreNr TelleHode.PkSdlNr TelleHode.Beskrivelse ~
TelleHode.StartDato 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-TelleHode TelleHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-TelleHode TelleHode
&Scoped-define QUERY-STRING-BROWSE-TelleHode FOR EACH TelleHode ~
      WHERE TelleHode.ButikkListe = FI-Butiker NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TelleHode OPEN QUERY BROWSE-TelleHode FOR EACH TelleHode ~
      WHERE TelleHode.ButikkListe = FI-Butiker NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TelleHode TelleHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TelleHode TelleHode


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TelleHode}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Etiketter RECT-50 RECT-51 Btn_Help Btn_OK ~
BUTTON-Ny BUTTON-Endre BUTTON-Slett B-SkrivUt B-Linjer BtnUtvalg BUTTON-Sok ~
FILL-IN-SOK-DATE FILL-IN-SOK-STRING FILL-IN-SOK-INT BROWSE-TelleHode ~
B-SokButikker 
&Scoped-Define DISPLAYED-OBJECTS FI-Butiker FILL-IN-SOK-DATE ~
FILL-IN-SOK-STRING FILL-IN-SOK-INT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu C-Win 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTranstype C-Win 
FUNCTION getTranstype RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVareTellingNr C-Win 
FUNCTION getVareTellingNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitUtvalgToTelling C-Win 
FUNCTION InitUtvalgToTelling RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UtvalgIsMaster C-Win 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE ProgressBar AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chProgressBar AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Etiketter 
     IMAGE-UP FILE "icon/ean13.jpg":U NO-FOCUS
     LABEL "Etiketter..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Etiketter".

DEFINE BUTTON B-Linjer 
     LABEL "&Tellelinjer..." 
     SIZE 22 BY 1.1 TOOLTIP "Starter program for vedlikehold av telleliste (linjer).".

DEFINE BUTTON B-SkrivUt 
     IMAGE-UP FILE "icon/e-print":U
     LABEL "Button 32" 
     SIZE 4.6 BY 1.1 TOOLTIP "Skriv ut merket telleliste.".

DEFINE BUTTON B-SokButikker 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BtnHentFraUtvalg 
     LABEL "Hent artikler fra utvalg" 
     SIZE 30.8 BY 1.1 TOOLTIP "Åpne artikkelutvalg".

DEFINE BUTTON BtnUtvalg 
     LABEL "Utvalg" 
     SIZE 16.2 BY 1.1 TOOLTIP "Åpne artikkelutvalg".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U
     LABEL "&Help" 
     SIZE 4.4 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "icon/e-exit":U
     LABEL "OK" 
     SIZE 4.4 BY 1.1 TOOLTIP "Avslutt"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Endre 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Endre..." 
     SIZE 4.4 BY 1.1 TOOLTIP "Vise/endre tellelistens hode. Alt-E.".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon/e-ny":U
     LABEL "&Ny..." 
     SIZE 4.4 BY 1.1 TOOLTIP "Opprette ny telleliste.".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slett" 
     SIZE 4.4 BY 1.1 TOOLTIP "Tar bort merket telleliste.".

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1.

DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INT AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-STRING AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.4 BY .1.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.4 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TelleHode FOR 
      TelleHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TelleHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TelleHode C-Win _STRUCTURED
  QUERY BROWSE-TelleHode NO-LOCK DISPLAY
      TelleHode.TelleNr COLUMN-LABEL "Telling *" FORMAT "zzzzzzz9":U
      TelleHode.TTId COLUMN-LABEL "TTId *" FORMAT "zz9":U WIDTH 5
      getTranstype() COLUMN-LABEL "Telletype" FORMAT "x(15)":U
      TelleHode.OrdreNr COLUMN-LABEL "OrdreNr *" FORMAT "->>>>>>9":U
      TelleHode.PkSdlNr COLUMN-LABEL "Pk.sdl *" FORMAT "->>>>>>9":U
      TelleHode.Beskrivelse COLUMN-LABEL "Beskrivelse *" FORMAT "X(27)":U
            WIDTH 33
      TelleHode.BatchNr FORMAT "zzzzzzzz9":U
      TelleHode.StartDato COLUMN-LABEL "StartDato *" FORMAT "99/99/9999":U
      TelleHode.Oppdatert FORMAT "99/99/9999":U
      TelleHode.AntallPar FORMAT "-z,zzz,zz9":U
      TelleHode.AntallTalt FORMAT "-z,zzz,zz9":U
      TelleHode.AntallDiff FORMAT "-z,zzz,zz9":U
      TelleHode.OpprVerdi FORMAT "-zzz,zzz,zz9":U
      TelleHode.OpptVerdi FORMAT "-zzz,zzz,zz9":U WIDTH 12.2
      TelleHode.VerdiDiff FORMAT "-zzz,zzz,zz9":U WIDTH 11.8
      TelleHode.AntLinjer FORMAT "->>,>>>,>>9":U
      TelleHode.ButikkListe FORMAT "X(40)":U
      TelleHode.RegistrertDato FORMAT "99/99/9999":U
      TelleHode.RegistrertAv FORMAT "X(10)":U
      TelleHode.EDato FORMAT "99/99/9999":U
      TelleHode.BrukerID FORMAT "X(10)":U
  ENABLE
      TelleHode.TelleNr
      TelleHode.TTId
      TelleHode.OrdreNr
      TelleHode.PkSdlNr
      TelleHode.Beskrivelse
      TelleHode.StartDato
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 160 BY 23.81 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Etiketter AT ROW 1.33 COL 22.4
     Btn_Help AT ROW 1.29 COL 150
     Btn_OK AT ROW 1.29 COL 155
     BUTTON-Ny AT ROW 1.33 COL 2
     BUTTON-Endre AT ROW 1.33 COL 7
     BUTTON-Slett AT ROW 1.33 COL 12
     B-SkrivUt AT ROW 1.33 COL 17
     B-Linjer AT ROW 1.33 COL 70.2
     BtnUtvalg AT ROW 1.33 COL 92.8
     BtnHentFraUtvalg AT ROW 1.33 COL 109.2
     FI-Butiker AT ROW 1.38 COL 36 COLON-ALIGNED
     BUTTON-Sok AT ROW 2.86 COL 22
     FILL-IN-SOK-DATE AT ROW 2.91 COL 2 NO-LABEL
     FILL-IN-SOK-STRING AT ROW 2.91 COL 2 NO-LABEL
     FILL-IN-SOK-INT AT ROW 2.91 COL 2.2 NO-LABEL
     BROWSE-TelleHode AT ROW 4.33 COL 2
     B-SokButikker AT ROW 1.38 COL 55.6
     RECT-50 AT ROW 2.48 COL 1
     RECT-51 AT ROW 1.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 162.2 BY 27.38.

DEFINE FRAME FRAME-Progress
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 32.8 ROW 2.67
         SIZE 68.2 BY 1.29.


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
         TITLE              = "Telling"
         HEIGHT             = 27.38
         WIDTH              = 162.2
         MAX-HEIGHT         = 31.67
         MAX-WIDTH          = 188
         VIRTUAL-HEIGHT     = 31.67
         VIRTUAL-WIDTH      = 188
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Progress:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-TelleHode FILL-IN-SOK-INT DEFAULT-FRAME */
ASSIGN 
       BROWSE-TelleHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 6.

ASSIGN 
       TelleHode.TelleNr:COLUMN-READ-ONLY IN BROWSE BROWSE-TelleHode = TRUE
       TelleHode.TTId:COLUMN-READ-ONLY IN BROWSE BROWSE-TelleHode = TRUE
       TelleHode.OrdreNr:COLUMN-READ-ONLY IN BROWSE BROWSE-TelleHode = TRUE
       TelleHode.PkSdlNr:COLUMN-READ-ONLY IN BROWSE BROWSE-TelleHode = TRUE
       TelleHode.Beskrivelse:COLUMN-READ-ONLY IN BROWSE BROWSE-TelleHode = TRUE
       TelleHode.StartDato:COLUMN-READ-ONLY IN BROWSE BROWSE-TelleHode = TRUE.

/* SETTINGS FOR BUTTON BtnHentFraUtvalg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BtnHentFraUtvalg:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FI-Butiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-STRING IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-Progress
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       FRAME FRAME-Progress:HIDDEN           = TRUE
       FRAME FRAME-Progress:SENSITIVE        = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TelleHode
/* Query rebuild information for BROWSE BROWSE-TelleHode
     _TblList          = "skotex.TelleHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _OrdList          = "skotex.TelleHode.TelleNr|no"
     _Where[1]         = "skotex.TelleHode.ButikkListe = FI-Butiker"
     _FldNameList[1]   > skotex.TelleHode.TelleNr
"TelleHode.TelleNr" "Telling *" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.TelleHode.TTId
"TelleHode.TTId" "TTId *" ? "integer" ? ? ? ? ? ? yes ? no no "5" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"getTranstype()" "Telletype" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > skotex.TelleHode.OrdreNr
"TelleHode.OrdreNr" "OrdreNr *" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > skotex.TelleHode.PkSdlNr
"TelleHode.PkSdlNr" "Pk.sdl *" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > skotex.TelleHode.Beskrivelse
"TelleHode.Beskrivelse" "Beskrivelse *" "X(27)" "character" ? ? ? ? ? ? yes ? no no "33" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = skotex.TelleHode.BatchNr
     _FldNameList[8]   > skotex.TelleHode.StartDato
"TelleHode.StartDato" "StartDato *" ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = skotex.TelleHode.Oppdatert
     _FldNameList[10]   > skotex.TelleHode.AntallPar
"TelleHode.AntallPar" ? "-z,zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > skotex.TelleHode.AntallTalt
"TelleHode.AntallTalt" ? "-z,zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > skotex.TelleHode.AntallDiff
"TelleHode.AntallDiff" ? "-z,zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > skotex.TelleHode.OpprVerdi
"TelleHode.OpprVerdi" ? "-zzz,zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > skotex.TelleHode.OpptVerdi
"TelleHode.OpptVerdi" ? "-zzz,zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > skotex.TelleHode.VerdiDiff
"TelleHode.VerdiDiff" ? "-zzz,zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no "11.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = skotex.TelleHode.AntLinjer
     _FldNameList[17]   = skotex.TelleHode.ButikkListe
     _FldNameList[18]   = skotex.TelleHode.RegistrertDato
     _FldNameList[19]   = skotex.TelleHode.RegistrertAv
     _FldNameList[20]   = skotex.TelleHode.EDato
     _FldNameList[21]   = skotex.TelleHode.BrukerID
     _Query            is OPENED
*/  /* BROWSE BROWSE-TelleHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Progress
/* Query rebuild information for FRAME FRAME-Progress
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Progress */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME ProgressBar ASSIGN
       FRAME           = FRAME FRAME-Progress:HANDLE
       ROW             = 1.38
       COLUMN          = 5.2
       HEIGHT          = .86
       WIDTH           = 59
       HIDDEN          = no
       SENSITIVE       = yes.
/* ProgressBar OCXINFO:CREATE-CONTROL from: {35053A22-8589-11D1-B16A-00C0F0283628} type: ProgressBar */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Telling */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Telling */
DO:
   /* This event will close the window and terminate the procedure.  */
 
  IF bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN 
    RUN InvalidateHandle IN hUtvalg (THIS-PROCEDURE).
  ELSE IF NOT bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN
    APPLY "close" TO hUtvalg.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Telling */
DO:
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Etiketter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Etiketter C-Win
ON CHOOSE OF B-Etiketter IN FRAME DEFAULT-FRAME /* Etiketter... */
DO:
    DEFINE VARIABLE iLoop  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lOk    AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iRetur AS INTEGER    NO-UNDO.
    IF NOT CAN-FIND(FIRST TelleLinje OF TelleHode) THEN
        RETURN NO-APPLY.
    RUN Etiketter.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Linjer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Linjer C-Win
ON CHOOSE OF B-Linjer IN FRAME DEFAULT-FRAME /* Tellelinjer... */
DO:
  IF AVAILABLE TelleHode THEN
      RUN TelleLinje.    
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SkrivUt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SkrivUt C-Win
ON CHOOSE OF B-SkrivUt IN FRAME DEFAULT-FRAME /* Button 32 */
DO:
  IF NOT AVAILABLE {&br-tabell} THEN
    RETURN NO-APPLY.
  RUN printTellelisteX.p (ROWID({&br-tabell}),"").
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikker C-Win
ON CHOOSE OF B-SokButikker IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-ArtInfo:
  /* Kaller søkerutine */
    RUN gbutikerBgrp.w (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "", /* Feltliste avgrensningsfelt (kommaseparert) */
      "", /* Feltverdier (chr(1) sep) */ 
      FI-Butiker:SCREEN-VALUE /* Post markøren skal stå på */
      ).
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN FI-Butiker = ENTRY(2,cTekst,CHR(1))
          FI-Butiker:SCREEN-VALUE = FI-Butiker.
          .
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TelleHode
&Scoped-define SELF-NAME BROWSE-TelleHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleHode C-Win
ON ANY-PRINTABLE OF BROWSE-TelleHode IN FRAME DEFAULT-FRAME
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleHode C-Win
ON CURSOR-LEFT OF BROWSE-TelleHode IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleHode C-Win
ON CURSOR-RIGHT OF BROWSE-TelleHode IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleHode C-Win
ON DEFAULT-ACTION OF BROWSE-TelleHode IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO B-Linjer.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleHode C-Win
ON ROW-DISPLAY OF BROWSE-TelleHode IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE TelleHode THEN
    DO:
      IF TelleHode.Oppdatert = ? THEN
        ASSIGN        
          TelleHode.Beskrivelse:bgcolor IN BROWSE BROWSE-TelleHode = 11.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleHode C-Win
ON START-SEARCH OF BROWSE-TelleHode IN FRAME DEFAULT-FRAME
DO:
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TelleHode C-Win
ON VALUE-CHANGED OF BROWSE-TelleHode IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE TelleHode THEN
    RUN SettSensitive.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnHentFraUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnHentFraUtvalg C-Win
ON CHOOSE OF BtnHentFraUtvalg IN FRAME DEFAULT-FRAME /* Hent artikler fra utvalg */
DO:
  DEF VAR bOK AS LOG NO-UNDO.
  IF AVAIL TelleHode AND TelleHode.oppdatert = ? AND VALID-HANDLE(hUtvalg) THEN DO:
    RUN OverfUtvalgTilTelling IN hUtvalg (TelleHode.TelleNr, OUTPUT bOK).
    IF bOK THEN
      APPLY "choose" TO B-Linjer IN FRAME {&FRAME-NAME}.
  END.
  ELSE IF NOT VALID-HANDLE(hUtvalg) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,"Utvalg ikke tilgjengelig","","").
  ELSE IF AVAIL TelleHode THEN 
    DYNAMIC-FUNCTION("DoMessage",0,0,"Telling &1 er oppdatert og kan ikke tilføres flere artikler","",STRING(TelleHode.TelleNr)).
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen varetelling valgt","","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnUtvalg C-Win
ON CHOOSE OF BtnUtvalg IN FRAME DEFAULT-FRAME /* Utvalg */
DO:
  SESSION:SET-WAIT-STATE("general").
  IF NOT VALID-HANDLE(hUtvalg) THEN DO:
    RUN wtmpartbas.w PERSIST SET hUtvalg.
    DYNAMIC-FUNCTION("setTellingIsMaster" IN hUtvalg,TRUE).
    RUN InitializeObject IN hUtvalg.
/*     InitUtvalgToTelling(hUtvalg,NO). /* NO: Utvalg er IKKE master */  */
  END.
  DYNAMIC-FUNCTION("InitFromTelling" IN hUtvalg, THIS-PROCEDURE).
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  {&return-ip}
  
  IF bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN 
    RUN InvalidateHandle IN hUtvalg (THIS-PROCEDURE).
  ELSE IF NOT bUtvalgIsMaster AND VALID-HANDLE(hUtvalg) THEN
    APPLY "close" TO hUtvalg.

  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
  &ELSE
      retur-verdi = "OK".
  &ENDIF
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME DEFAULT-FRAME /* Endre... */
DO:
  IF NOT AVAILABLE {&br-tabell} THEN
    RETURN NO-APPLY.
  ASSIGN
    wRecid = RECID({&br-tabell}).
  RUN {&VedlikeholdsRutine} (INPUT-OUTPUT wRecid,"Endre",FI-Butiker:SCREEN-VALUE).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  wOk = BROWSE-{&br-tabell}:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny... */
DO:
  ASSIGN 
    wRecid = ?.
  RUN {&VedlikeholdsRutine} (INPUT-OUTPUT wRecid,"Ny",FI-Butiker:SCREEN-VALUE).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  {&OPEN-QUERY-BROWSE-TelleHode}
/*   {&OPEN-BROWSERS-IN-QUERY} */
  FIND b{&br-tabell} NO-LOCK WHERE
    RECID(b{&br-tabell}) = wRecid NO-ERROR.
  RUN SD-Reposition.
  
  APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
/*   APPLY "CHOOSE" TO B-Linjer.  */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Slett */
DO:
  DEF VAR wSvar AS LOG NO-UNDO.
  
  ASSIGN wSvar = FALSE.
  
  IF NOT AVAILABLE {&br-tabell} THEN
    RETURN NO-APPLY.

  MESSAGE "Skal tellingen tas bort?"
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY.

  RUN SlettLinjer.
  IF RETURN-VALUE <> "AVBRYT" THEN
    wOk = BROWSE-{&br-tabell}:DELETE-CURRENT-ROW( ).
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
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
   ELSE
       RUN SD-Reposition.
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


&Scoped-define SELF-NAME FILL-IN-SOK-STRING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-STRING C-Win
ON RETURN OF FILL-IN-SOK-STRING IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
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

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall  = "Nei"
  &WindowName = "TelleHode"
  &PostIClose = "for each tmpChild:
                   if valid-handle(tmpChild.wChild) then
                     delete procedure tmpChild.wChild. 
                 end.
                 IF VALID-HANDLE(chProgressBar) THEN
                     RELEASE OBJECT chProgressBar NO-ERROR.
                 IF VALID-HANDLE(ProgressBar) THEN
                     DELETE OBJECT ProgressBar NO-ERROR.
                 ASSIGN ProgressBar   = ?
                        chProgressBar = ?.
                     "
}

/* Avgjør om dagsrapporten skal posteres inklusive eller eksklusive mva */
idags_moms = 0.
{syspara.i 6 4 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    idags_moms = 1. /* Poster inkl. Mva */
ELSE
    iDags_Moms = 0. /* Poster eks.  Mva  */

/* Faktor for beregning av DB på grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes på artikkelen. */
plDbFaktorNetto = 0.
{syspara.i 6 4 4 plDbFaktorNetto DEC}
IF plDbFaktorNetto = 0 THEN
    plDbFaktorNetto = 0.30.

/* Faktor for beregning av DB på grunnlag av oms inkl. mva, hvis ikke kalkyle finnes på artikkelen. */
plDbFaktorBrutto = 0.
{syspara.i 6 4 3 plDbFaktorBrutto DEC}
IF plDbFaktorBrutto = 0 THEN
    plDbFaktorBrutto = 0.24.
/* Faktor for beregning av DB på grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes på artikkelen. */
plMva% = 0.
{syspara.i 6 4 2 plMva% DEC}
IF plMva% = 0 THEN
    plMva% = 0.30.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Sentrallageret */
{syspara.i 5 1 1 wCl INT}
{syspara.i 1 2 4 wEDB-System}
ASSIGN wTabell = "ArtBas".
{syspara.i 1 4 1 wExcEkstent}
wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.   
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = wCl NO-ERROR.

/* HotKey */
ON ALT-N OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Ny IN FRAME DEFAULT-FRAME.
  END.
ON ALT-E OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Endre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-D OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Slett IN FRAME DEFAULT-FRAME.
  END.
ON ALT-P OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-SkrivUt IN FRAME DEFAULT-FRAME.
  END.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  wOrgBgCol = {&sortby-col1}:Label-bgcolor IN BROWSE {&BROWSE-NAME}.
  RUN InitButikker.
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  IF AVAIL bruker AND CAN-DO(cButikkerBgrp,STRING(Bruker.butikknr)) THEN
      ASSIGN FI-Butiker = STRING(Bruker.ButikkNr).
  ELSE
      ASSIGN FI-Butiker = ENTRY(1,cButikkerBgrp).
  RUN LabelColor.
  RUN Move-Fill-To-Top.

  RUN InitResize.

  RUN enable_UI.
  {lng.i} 
  VIEW FRAME DEFAULT-FRAME.
  
  IF {&ant-sok-field} > 1 THEN
      RUN Init-Read-Only.
  &IF DEFINED(init-phrase) &THEN
  {&init-phrase}
  
  IF AVAILABLE b{&br-tabell} THEN DO:
      RUN SD-Reposition.
  END.
  &ENDIF

  ASSIGN
      C-Win:HIDDEN = FALSE.

  APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 RETURN retur-verdi.
&else
 MESSAGE retur-verdi VIEW-AS ALERT-BOX.
&endif

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
        {&OPEN-QUERY-{&BROWSE-NAME}}
        .
        wDataType = {&sortby-col1}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
    END.
    &IF DEFINED(sortby-phrase2) &THEN
    WHEN 2 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase2}
        {&OPEN-QUERY-{&BROWSE-NAME}}
        .
        wDataType = {&sortby-col2}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase3) &THEN
    WHEN 3 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase3}
        {&OPEN-QUERY-{&BROWSE-NAME}}
        .
        wDataType = {&sortby-col3}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase4) &THEN
    WHEN 4 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase4}
        {&OPEN-QUERY-{&BROWSE-NAME}}
/*         {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame} */
        .
        wDataType = {&sortby-col4}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase5) &THEN
    WHEN 5 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase5}
        {&OPEN-QUERY-{&BROWSE-NAME}}
        .
        wDataType = {&sortby-col5}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase6) &THEN
    WHEN 6 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase6}
        {&OPEN-QUERY-{&BROWSE-NAME}}
        .
        wDataType = {&sortby-col6}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-btellehode.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chProgressBar = ProgressBar:COM-HANDLE
    UIB_S = chProgressBar:LoadControls( OCXFile, "ProgressBar":U)
    ProgressBar:NAME = "ProgressBar":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-btellehode.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY FI-Butiker FILL-IN-SOK-DATE FILL-IN-SOK-STRING FILL-IN-SOK-INT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-Etiketter RECT-50 RECT-51 Btn_Help Btn_OK BUTTON-Ny BUTTON-Endre 
         BUTTON-Slett B-SkrivUt B-Linjer BtnUtvalg BUTTON-Sok FILL-IN-SOK-DATE 
         FILL-IN-SOK-STRING FILL-IN-SOK-INT BROWSE-TelleHode B-SokButikker 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Progress}
  FRAME FRAME-Progress:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Etiketter C-Win 
PROCEDURE Etiketter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iStartEtikett AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLayoutValg   AS INTEGER    NO-UNDO.
  RUN d-skrivervalg.w (OUTPUT iLayoutValg,OUTPUT iStartEtikett).
  IF iLayoutValg = 0 THEN
      RETURN.
  RUN batchEtikettTelling (TelleHode.TelleNr,iLayoutValg,"",FALSE,"TELLING").
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
    {&sortby-col1}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &IF DEFINED(sortby-col2) &THEN
    {&sortby-col2}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    &IF DEFINED(sortby-col3) &THEN
    {&sortby-col3}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    &IF DEFINED(sortby-col4) &THEN
    {&sortby-col4}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    &IF DEFINED(sortby-col5) &THEN
    {&sortby-col5}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    &IF DEFINED(sortby-col6) &THEN
    {&sortby-col6}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    .
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButikker C-Win 
PROCEDURE InitButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND Bruker WHERE Bruker.BrukerId = USERID("skotex") NO-LOCK.
    FOR EACH Butikktilgang WHERE Butikktilgang.BrGrpNr = Bruker.BrGrpNr NO-LOCK:
        ASSIGN cButikkerBgrp = cButikkerBgrp + 
                               (IF cButikkerBgrp <> "" THEN "," ELSE "") + 
                               STRING(Butikktilgang.butik).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       ASSIGN chProgressBar = chProgressBar:ProgressBar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitResize C-Win 
PROCEDURE InitResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME DEFAULT-FRAME:HANDLE,
    "RECT-50,RECT-51").
    
    DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,600,380,0,0).
    
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
   {&sortby-col1}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 1 THEN wSortBgCol ELSE wOrgBgCol
   &IF DEFINED(sortby-col2) &THEN
   {&sortby-col2}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 2 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col3) &THEN
   {&sortby-col3}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 3 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col4) &THEN
   {&sortby-col4}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 4 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col5) &THEN
   {&sortby-col5}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 5 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col6) &THEN
   {&sortby-col6}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 6 THEN wSortBgCol ELSE wOrgBgCol
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
                 IF FILL-IN-SOK-STRING:MOVE-TO-TOP() THEN.
             WHEN "DATE" THEN
                 IF FILL-IN-SOK-DATE:MOVE-TO-TOP() THEN.
        END CASE.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterDagsrapport C-Win 
PROCEDURE OppdaterDagsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{dagsrapport.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterTelling C-Win 
PROCEDURE OppdaterTelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wAntLinjer AS INT NO-UNDO.

  IF NOT AVAILABLE TelleHode THEN
    RETURN.
  
  {syspara.i 4 1 2 wTekst}
  IF {&br-tabell}.TTID = int(wTekst) 
    THEN wNedskriv = TRUE.
  ELSE wNedskriv = FALSE.

  FIND TransType NO-LOCK WHERE
    TransType.TTId = TelleHode.TTId NO-ERROR.
  FIND TransBeskr NO-LOCK WHERE
    TransBeskr.TTID = TelleHode.TTId AND
    TransBeskr.TBId = TelleHode.TBID NO-ERROR.
      
  {sww.i}
  RUN batchlogg.w (PROGRAM-NAME(1), 
                   "Varetelling - Type: " + TransType.Beskrivelse + "/" + TransBeskr.Beskrivelse,
                    OUTPUT wBatchNr).

  IF TelleHode.AntLinjer >= 5 THEN
    DO:
      ASSIGN
        chProgressBar:Min   = 1
        chProgressBar:Max   = TelleHode.AntLinjer
        chProgressBar:Value = 1.
      VIEW FRAME FRAME-Progress.
    END.

  /* Leser alle tellerader. */
  TELLELINJE:
  FOR EACH TelleLinje OF TelleHode EXCLUSIVE-LOCK WHERE
    TelleLinje.Oppdatert = FALSE
    BREAK
    BY TelleLinje.TelleNr
    BY TelleLinje.Vg
    BY TelleLinje.LopNr
    BY TelleLinje.Butik 
    BY TelleLinje.Storl TRANSACTION:
    
    IF FIRST-OF(TelleLinje.Butik) THEN
      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = TelleLinje.Butik NO-ERROR.
    
    ASSIGN
      wAntLinjer = wAntLinjer + 1
      TelleLinje.Oppdatert = TRUE.      
      
    IF wAntLinjer >= 5 THEN
      DO:
        IF wAntLinjer MODULO 10 = 0 THEN
          chProgressBar:Value = wAntLinjer.
      END.

    /* Tar bort tellelås på artikkel/butikk. */
    IF LAST-OF(TelleLinje.Butik) THEN
      DO:
        FIND FIRST KonvReg EXCLUSIVE-LOCK WHERE
          KonvReg.EDB-System = wEDB-System AND
          KonvReg.Tabell     = wTabell     AND
          KonvReg.EkstId     = STRING(TelleLinje.ArtikkelNr) + "," + 
                               string(TelleLinje.Butik) AND
          KonvReg.InterntId  = STRING(TelleLinje.ArtikkelNr) + "," + 
                               string(TelleLinje.Butik) NO-ERROR.
        IF AVAILABLE KonvReg THEN
          DELETE KonvReg.      
      END.
    
    /* Er diff lik 0, er et ingenting å oppdatere på en vanlig telling. */
    IF (TelleLinje.AntallDiff = 0 AND wNedskriv = FALSE) THEN
      DO:
        ASSIGN
          TelleLinje.Oppdatert = TRUE.
        NEXT TELLELINJE.
      END.
      
    /* Er det nedskrivning, skal det kun opprettes transaksjoner der hvor det er gjordt nedskrivning . */
    /* Dvs der hvor Nedskrevet er forsjkellig fra VVareKost.                                           */
    IF CAN-DO("8",STRING(TelleHode.TTId)) THEN
      DO:
        IF TelleLinje.Nedskrevet = TelleLinje.VVareKost THEN
          NEXT TELLELINJE.
      END.

    /* Oppretter transaksjoner */
    IF wNedskriv THEN
      RUN OppdatNedskriv.
    ELSE
      RUN OppdatTrans.
  END. /* TELLELINJE */

  HIDE FRAME FRAME-Progress NO-PAUSE.     
  
  RUN OppdatTelleHode (INPUT TODAY).

  /* Oppdaterer dagsrapporten for varesalg. */
  IF TelleHode.TTId = 1 THEN
      RUN OppdaterDagsrapport (INPUT wBatchNr).

  RUN batchstatus.p (wBatchNr, 2).
  {swn.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatNedSkriv C-Win 
PROCEDURE OppdatNedSkriv :
/*------------------------------------------------------------------------------
  Purpose:     Oppretter en transaksjon pr. butikk/vg og løpenummer.
               Denne transaksjonen posterer mot lager. I LAger påvirkes feltet
               VVAreKost som er den vektede varekost for butikken.
               Det er nok med en transaksjon for å oppnå det ønskede
               resultat.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  POSTER_BLOKK:
  DO:
    /* Transaksjonsnummer for butikken. */
    FIND LAST TransLogg NO-LOCK WHERE
      TransLogg.Butik = TelleLinje.Butik USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE TransLogg THEN
      wTransNr = TransLogg.TransNr + 1.
    ELSE 
      wTransNr = 1.

    /* Oppretter transaksjon */
    LAG_TRANS:
    DO:
      /* Sjekker at transnr er ledig */
      IF CAN-FIND(TransLogg WHERE
                  TransLogg.Butik   = TelleLinje.Butik AND
                  TransLogg.TransNr = wTransNr) THEN
      NESTE_NR:
      DO WHILE TRUE:
        wTransNr = wTransNr + 1.
        IF CAN-FIND(TransLogg WHERE
                    TransLogg.Butik   = TelleLinje.Butik AND
                    TransLogg.TransNr = wTransNr) THEN
          NEXT NESTE_NR.
       ELSE
          LEAVE NESTE_NR.
      END. /* NESTE_NR */

      CREATE TransLogg.
      ASSIGN TransLogg.Butik        = TelleLinje.Butik
             TransLogg.TransNr      = wTransNr
             TransLogg.SeqNr        = 1.
      ASSIGN TransLogg.BatchNr      = wBatchNr
             TransLogg.KundNr       = 0
             TransLogg.TTId         = TelleHode.TTId
             TransLogg.TBId         = TelleHode.TBId
             TransLogg.ArtikkelNr   = TelleLinje.ArtikkelNr
             TransLogg.LevNr        = TelleLinje.LevNr
             TransLogg.BongId       = 0
             TransLogg.BongLinjeNr  = 0
             TransLogg.KassaNr      = 0
             TransLogg.Vg           = TelleLinje.Vg
             TransLogg.LopNr        = TelleLinje.LopNr
             TransLogg.Storl        = ""
             TransLogg.Dato         = TODAY
             TransLogg.Tid          = TIME
             TransLogg.BestNr       = 0
             TransLogg.Postert      = FALSE                                             
             TransLogg.Plukket      = TRUE /* Skal ikke ut på plukkliste */
             TransLogg.Antall       = IF TelleLinje.AntallPar <= 0 /* For at transene skal kunne bakkes ut */
                                        THEN 1
                                        ELSE TelleLinje.AntallPar
             TransLogg.Pris         = TelleLinje.VVareKost - TelleLinje.Nedskrevet
             TransLogg.RabKr        = 0
             TransLogg.Mva          = 0.
/*             
message "Fra OppdatNedskriv i w-bTelleHode:" skip(1)
        "TransLogg.Antall:" TransLogg.Antall skip
        "TransLogg.Pris:" TransLogg.Pris skip
        "TelleLinje.VVareKost:" TelleLinje.VVareKost skip
        "TelleLinje.Nedskrevet:" TelleLinje.Nedskrevet
        view-as alert-box.
*/        

                     
    END. /* LAG_TRANS */
    
  END. /* POSTER_BLOKK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatRad C-Win 
PROCEDURE OppdatRad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatTelleHode C-Win 
PROCEDURE OppdatTelleHode :
/*------------------------------------------------------------------------------
  Purpose:     DUMMY
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wToday AS DATE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatTrans C-Win 
PROCEDURE OppdatTrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bufTransLogg FOR TransLogg.

DO FOR bufTransLogg:
    /* Henter artpris */
    IF AVAILABLE Butiker THEN
      DO:
        FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = TelleLinje.ArtikkelNr AND
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = TelleLinje.ArtikkelNr AND
            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      END.
    ELSE 
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = TelleLinje.ArtikkelNr AND
        ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.

    /* Henter lager */
    FIND Lager NO-LOCK WHERE
         Lager.ArtikkelNr = ArtPris.ArtikkelNr AND
        Lager.Butik = TelleLinje.Butik NO-ERROR.
            
    /* Transaksjonsnummer for butikken. */
    FIND LAST bufTransLogg NO-LOCK WHERE
      bufTransLogg.Butik = TelleLinje.Butik USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE bufTransLogg THEN
      wTransNr = bufTransLogg.TransNr + 1.
    ELSE 
      wTransNr = 1.

    /* Oppretter transaksjon */
    LAG_TRANS:
    DO:
      /* Sjekker at transnr er ledig */
      IF CAN-FIND(bufTransLogg WHERE
                  bufTransLogg.Butik   = TelleLinje.Butik AND
                  bufTransLogg.TransNr = wTransNr) THEN
      NESTE_NR:
      DO WHILE TRUE:
        wTransNr = wTransNr + 1.
        IF CAN-FIND(bufTransLogg WHERE
                    bufTransLogg.Butik   = TelleLinje.Butik AND
                    bufTransLogg.TransNr = wTransNr) THEN
          NEXT NESTE_NR.
       ELSE
          LEAVE NESTE_NR.
      END. /* NESTE_NR */

      CREATE bufTransLogg.
      ASSIGN bufTransLogg.Butik        = TelleLinje.Butik
             bufTransLogg.TransNr      = wTransNr
             bufTransLogg.SeqNr        = 1.
      ASSIGN bufTransLogg.BatchNr      = wBatchNr
             bufTransLogg.KundNr       = 0
             bufTransLogg.TTId         = TelleHode.TTId
             bufTransLogg.TBId         = TelleHode.TBId
             bufTransLogg.ArtikkelNr   = TelleLinje.ArtikkelNr
             bufTransLogg.LevNr        = TelleLinje.LevNr
             bufTransLogg.BongId       = 0
             bufTransLogg.BongLinjeNr  = 0
             bufTransLogg.KassaNr      = 0
             bufTransLogg.Vg           = TelleLinje.Vg
             bufTransLogg.LopNr        = TelleLinje.LopNr
             bufTransLogg.Storl        = TelleLinje.Storl
             bufTransLogg.Dato         = TelleHode.StartDato
             bufTransLogg.Tid          = TIME
             bufTransLogg.BestNr       = 0
             bufTransLogg.Postert      = FALSE.                                             
             
      /* Tillatte transaksjonstyper 1,2,3,5,7,8,9,10,11 */       
      CASE TelleHode.TTId:
        WHEN 1  /* Varesalg        */ THEN
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff
            bufTransLogg.Pris   = TelleLinje.VVareKost 
            bufTransLogg.RabKr  = TelleLinje.RabKr
            bufTransLogg.Mva    = (TelleLinje.VVareKost - TelleLinje.RabKr) - 
                                  ((TelleLinje.VVAreKost - TelleLinje.RabKr) / (1 + (ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1] / 100))).
        WHEN 2  /* Brekkasje       */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
        WHEN 3  /* Kundereklamasjon */ THEN
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall  = TelleLinje.AntallDiff
            bufTransLogg.Pris    = TelleLinje.VVareKost 
            bufTransLogg.RabKr   = TelleLinje.RabKr
            bufTransLogg.Mva     = (TelleLinje.VVareKost - TelleLinje.RabKr) - 
                                  ((TelleLinje.VVAreKost - TelleLinje.RabKr) / (1 + (ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1] / 100))).
        WHEN 5  /* Varekjøp        */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
        WHEN 7  /* Lagerjustering  */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
        WHEN 8  /* Nedskriving     */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff
            bufTransLogg.Pris   = TelleLinje.Nedskrevet
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
        WHEN 9  /* Svinn           */ THEN 
        DO:
            ASSIGN
              bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
              bufTransLogg.Antall = TelleLinje.AntallDiff
              bufTransLogg.Pris   = TelleLinje.VVareKost
              bufTransLogg.RabKr  = 0
              bufTransLogg.Mva    = 0.
            IF AVAILABLE Lager THEN
            DO:
                IF Lager.VVareKost = 0 THEN
                DO:
                    FIND CURRENT Lager EXCLUSIVE-LOCK.
                    ASSIGN
                        Lager.VVareKost = TelleLinje.VVareKost
                        .
                    RELEASE Lager.
                END.
            END.

        END.
        WHEN 10  /* Gjennkjøp */ THEN
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall  = TelleLinje.AntallDiff * -1
            bufTransLogg.Pris    = TelleLinje.VVareKost 
            bufTransLogg.RabKr   = TelleLinje.RabKr
            bufTransLogg.Mva     = (TelleLinje.VVareKost - TelleLinje.RabKr) - 
                                  ((TelleLinje.VVAreKost - TelleLinje.RabKr) / (1 + (ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1] / 100))).
        WHEN 11 /* Internt forbruk */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
      END CASE.
    END. /* LAG_TRANS */
    
  IF AVAILABLE bufTransLogg THEN
    RELEASE bufTransLogg.    
END. /* TRANSLOGG */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Any-Printable C-Win 
PROCEDURE SD-Any-Printable :
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
            APPLY "ENTRY" TO FILL-IN-SOK-STRING.
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
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
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
        REPOSITION {&BROWSE-NAME} TO ROWID ROWID(b{&br-tabell}) NO-ERROR.
        CASE wAktivDataType:
            WHEN "INT" THEN
                ASSIGN FILL-IN-SOK-INT:SCREEN-VALUE = "".
            WHEN "CHAR" THEN
                ASSIGN FILL-IN-SOK-STRING:SCREEN-VALUE = "".
            WHEN "DATE" THEN
                ASSIGN FILL-IN-SOK-DATE:SCREEN-VALUE = "".
        END CASE.
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Start-Search C-Win 
PROCEDURE SD-Start-Search :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR h-Curr-Col AS widget.
    h-Curr-Col = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
    APPLY "end-search" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.  
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
        &IF DEFINED(sok-field5) &THEN 
        WHEN "{&sok-field5}" THEN
            IF wAktivCol <> 5 THEN DO:
                RUN SortNyCol(5).
                RETURN NO-APPLY.
            END.
        &ENDIF
        &IF DEFINED(sok-field6) &THEN 
        WHEN "{&sok-field6}" THEN
            IF wAktivCol <> 6 THEN DO:
                RUN SortNyCol(6).
                RETURN NO-APPLY.
            END.
        &ENDIF
    END CASE.
   
    RUN SD-Reposition.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettSensitive C-Win 
PROCEDURE SettSensitive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:
/*     if TelleHode.Oppdatert <> ? then */
/*       assign                         */
/*         B-bygg:sensitive   = false   */
/*         B-Oppdat:sensitive = false   */
/*         B-Fil:sensitive    = false.  */
/*     else                             */
/*       assign                         */
/*         B-bygg:sensitive   = true    */
/*         B-Oppdat:sensitive = true    */
/*         B-Fil:sensitive    = true.   */
    IF NOT BtnHentFraUtvalg:HIDDEN THEN
      IF TelleHode.oppdatert NE ? THEN
        BtnHentFraUtvalg:SENSITIVE = FALSE.
      ELSE BtnHentFraUtvalg:SENSITIVE = TRUE.

    IF TelleHode.oppdatert NE ? THEN DO:
      BtnUtvalg:SENSITIVE = FALSE.
      IF VALID-HANDLE(hUtvalg) THEN 
        DYNAMIC-FUNCTION("setEnableBtnSendUtvalg" IN hUtvalg,FALSE).
    END.
    ELSE DO:
      BtnUtvalg:SENSITIVE = TRUE.
      IF VALID-HANDLE(hUtvalg) THEN 
        DYNAMIC-FUNCTION("setEnableBtnSendUtvalg" IN hUtvalg,TRUE).
    END.
    ASSIGN B-Etiketter:SENSITIVE = TelleHode.TTid = 5 AND CAN-FIND(FIRST TelleLinje OF Tellehode).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettLinjer C-Win 
PROCEDURE SlettLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wArtikkelNr   LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEF VAR wButik        AS INT  NO-UNDO. 
  DEF VAR wReturn-Value AS CHAR INITIAL "AVBRYT" NO-UNDO.
  DEF VAR wAntLinjer    AS INT NO-UNDO.

  DEF BUFFER bTelleLinje FOR TelleLinje.
  DEF BUFFER bKonvReg    FOR KonvReg.

  IF TelleHode.AntLinjer > 15 THEN
    DO:
      ASSIGN
        chProgressBar:Min   = 1
        chProgressBar:Max   = TelleHode.AntLinjer
        chProgressBar:Value = 1.
      VIEW FRAME FRAME-Progress.
    END.
  ELSE 
    wReturn-Value = "OK".
    
SLETT_LINJER:
FOR EACH bTelleLinje OF TelleHode TRANSACTION:

  /* Viser fremdrift til bruker. */
  ASSIGN
    wAntLinjer = wAntLinjer + 1.
    
  IF TelleHode.AntLinjer > 15 THEN
    DO:
      IF wAntLinjer MODULO 10 = 0 THEN
        chProgressBar:Value = wAntLinjer.
    END.

  /* Leser av verdiene for aktiv tellelinje */
  ASSIGN
    wArtikkelNr   = bTelleLinje.ArtikkelNr
    wButik        = bTelleLinje.Butik
    wReturn-Value = "AVBRYT".

  /* Døden */
  DELETE bTelleLinje.
  
  /* Sjekker om låsen skal slettes for andre typer av tellinger.      */
  /* Finnes det ikke flere tellelinjer igjen på artikkelen og butikk, */
  /* skal tellelåsen slettes.                                         */
  IF wNedskriv = FALSE THEN
    DO:
      IF NOT CAN-FIND(FIRST bTelleLinje WHERE
        bTelleLinje.TelleNr    = TelleHode.TelleNr AND
        bTelleLinje.ArtikkelNr = wArtikkelNr AND
        bTelleLinje.Butik      = wButik) THEN
        DO:
          FIND FIRST bKonvReg EXCLUSIVE-LOCK WHERE
            bKonvReg.EDB-System = wEDB-System AND
            bKonvReg.Tabell     = wTabell     AND
            bKonvReg.EkstId     = STRING(wArtikkelNr) + "," + 
                                 string(wButik) AND
            bKonvReg.InterntId  = STRING(wArtikkelNr) + "," + 
                                 string(wButik) NO-ERROR.
          IF AVAILABLE bKonvReg THEN
            DELETE bKonvReg.
        END.     
    END.      
  /* Sletter låsen for nedskrivning. */
  /* Her finnes kun en linje pr. artikkel og butikk. */
  ELSE DO:
      FIND FIRST bKonvReg EXCLUSIVE-LOCK WHERE
        bKonvReg.EDB-System = wEDB-System AND
        bKonvReg.Tabell     = wTabell     AND
        bKonvReg.EkstId     = STRING(wArtikkelNr) + "," + 
                             string(wButik) AND
        bKonvReg.InterntId  = STRING(wArtikkelNr) + "," + 
                             string(wButik) NO-ERROR.
      IF AVAILABLE bKonvReg THEN
        DELETE bKonvReg.
  END.

  wReturn-Value = "OK".
  
END. /* SLETT_LINJER TRANSACTION */

IF TelleHode.AntLinjer > 15 THEN
  HIDE FRAME FRAME-Progress NO-PAUSE.     

IF wReturn-Value <> "AVBRYT" THEN
  DO TRANSACTION:
    wReturn-Value = "AVBRYT".
    FOR EACH HT-FilHode OF TelleHode EXCLUSIVE-LOCK:
      FOR EACH HT-FilLinje OF HT-FilHode:
        DELETE HT-FilLinje.
      END.
      DELETE HT-FilHode.
    END.
    FIND CURRENT TelleHode EXCLUSIVE-LOCK.
    DELETE TelleHode.
    wReturn-Value = "OK".
  END.

RETURN wReturn-Value.

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
    wBrowseRow = {&BROWSE-NAME}:FOCUSED-ROW IN FRAME {&FRAME-NAME}.
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = wRowId NO-LOCK.
    IF AVAILABLE b{&br-tabell} THEN
    DO:
        RUN BytSortering (wNyCol).
        RUN SD-Reposition.
    END.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tellelinje C-Win 
PROCEDURE Tellelinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  fLockvindu(TRUE).
  RUN w-btellelinje (INPUT RECID(TelleHode)).
  fLockvindu(FALSE).
  BROWSE BROWSE-Tellehode:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu C-Win 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTranstype C-Win 
FUNCTION getTranstype RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Transtype WHERE Transtype.TTId = TelleHode.TTId NO-LOCK NO-ERROR.
  RETURN IF AVAIL Transtype THEN Transtype.Beskrivelse ELSE "-Ukjent-".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVareTellingNr C-Win 
FUNCTION getVareTellingNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF AVAIL TelleHode THEN
  RETURN TelleHode.TelleNr.
ELSE RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitUtvalgToTelling C-Win 
FUNCTION InitUtvalgToTelling RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Enable / disable buttons according to who is master
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN hUtvalg                    = ihUtvalg
         bUtvalgIsMaster            = ibUtvalgIsMaster
         btnUtvalg:HIDDEN           = IF bUtvalgIsMaster THEN TRUE ELSE FALSE
         btnUtvalg:SENSITIVE        = IF bUtvalgIsMaster THEN FALSE ELSE TRUE
         btnHentFraUtvalg:HIDDEN    = IF VALID-HANDLE(hUtvalg) THEN FALSE ELSE TRUE
         btnHentFraUtvalg:SENSITIVE = IF VALID-HANDLE(hUtvalg) THEN TRUE ELSE FALSE
         .

END.
  
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UtvalgIsMaster C-Win 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN bUtvalgIsMaster.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

