&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          wr               PROGRESS
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

DEFINE INPUT PARAMETER wTelleNr LIKE TelleHode.TelleNr NO-UNDO.
DEFINE INPUT PARAMETER wTypeId  AS   INT               NO-UNDO.
DEFINE INPUT PARAMETER wHtFilId AS   INT               NO-UNDO.

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell   HT-FilHode
&scoped-define OptWhere 

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  HTFilId
&scoped-define sok-field1-def  INT /* INT STRING DATE */
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} /* DESCENDING */
&scoped-define index-col1  TelleNrHTFilId
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Kolonne 2 som det kan sorteres på */
&scoped-define sok-field2  FilNavn
&scoped-define sok-field2-def  STRING /* INT STRING DATE */
&scoped-define sortby-col2 {&br-tabell}.{&sok-field2}
&scoped-define sortby-phrase2 {&br-tabell}.{&sok-field2} /* DESCENDING */
&scoped-define index-col2  FilNavn
&scoped-define sok-phrase2 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field2} >= ~
                INPUT FILL-IN-SOK-{&sok-field2-def} USE-INDEX {&index-col2} NO-LOCK NO-ERROR.

/* Kolonne 3 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field3  FilDato
&scoped-define sok-field3-def  Date
&scoped-define sortby-col3 {&br-tabell}.{&sok-field3}
&scoped-define sortby-phrase3 {&br-tabell}.{&sok-field3} /* DESCENDING */
&scoped-define index-col3  FilDato
&scoped-define sok-phrase3 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field3} >= ~
               INPUT FILL-IN-SOK-{&sok-field3-def} USE-INDEX {&index-col3} NO-LOCK NO-ERROR.

/* Om du vill sortera på fler kolonner:
   Kopiera kolonnedefinition 1 ovan och ændra talet 1 till næsta ikke anvænda.
   Ændra sedan alla fælt.
   Ændra ant-sok-field nedan till det antall som du vill sortera på.
   Ændra init-datatype nedan till datatyp før den sorteringskolonne du startar på
   Ændra æven alla andra scoped-define før att passa ditt program
   Ændra wAktivCol nedan till INIT ønskad startkolonne.
*/

&scoped-define ip-variabel wHTFilId
&scoped-define return-ip   wHTFilId = if available {&br-tabell}  ~
                                       then {&br-tabell}.{&sok-field1} ~
                                       else  {&sok-field1-def}("").
&scoped-define assign-retur-verdi retur-verdi = if available {&br-tabell} ~
                                                  then string(recid({&br-tabell})) ~
                                                  else "".
&scoped-define SletteMelding "Skal filen(e) tas bort?"
&scoped-define KanSlettes 
&scoped-define ant-sok-field   3
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
DEF VAR wRecid      AS RECID                 NO-UNDO.
DEF VAR wOk         AS LOG                   NO-UNDO.
DEF VAR retur-verdi AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEF VAR wKatalog    AS CHAR                  NO-UNDO.
DEF VAR wFilPrefix  AS CHAR                  NO-UNDO.
DEF VAR wFilEkstent AS CHAR                  NO-UNDO.
DEF VAR wEDB-System AS CHAR                  NO-UNDO.
DEF VAR wTabell     AS CHAR                  NO-UNDO.
DEF VAR wAntTyper   AS INT                   NO-UNDO.
DEF VAR wCl         AS INT                   NO-UNDO.
DEF VAR wButikk     AS INT                   NO-UNDO.
DEF VAR iAntFiler   AS INT                   NO-UNDO.
DEF VAR wSvar       AS LOG                   NO-UNDO.

DEF VAR iButikkNr AS INT  NO-UNDO.
DEF VAR pcEAN     AS CHAR NO-UNDO.
DEF VAR pfEAN     AS DEC  NO-UNDO.
DEF VAR pcDato    AS CHAR NO-UNDO.
DEF VAR pcTid     AS CHAR NO-UNDO.
DEF VAR piTid     AS INT  NO-UNDO. 
DEF VAR pdDato    AS DATE NO-UNDO.
DEF VAR cTekst    AS CHAR NO-UNDO.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEFINE VAR wImpProgram       AS CHAR        NO-UNDO.
DEFINE VAR wArtikkelNr       AS DEC         NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

DEF STREAM InnFraFil.

DEF TEMP-TABLE tmpVaretran NO-UNDO
    FIELD ButikkNr     AS INT FORMAT ">>>>>9"
    FIELD Strekkode    AS CHAR FORMAT "x(30)"
    FIELD Dato         AS DATE
    FIELD tid          AS INT FORMAT ">>>>9"
    FIELD LoggNr       AS INT FORMAT ">>>>>9"
    FIELD TransType    AS INT FORMAT ">9"
    FIELD Transtekst   AS CHAR FORMAT "x(30)"
    FIELD BrukerId     AS CHAR FORMAT "x(15)"
    FIELD Antall       AS DEC FORMAT ">>,>>9.999-"
    FIELD Kostpris     AS DEC FORMAT ">>>,>>9.999-"
    FIELD Salgssum     AS DEC FORMAT ">,>>>,>>9.999-"
    FIELD NyLagAnt     AS DEC FORMAT ">>>,>>9.999-"
    FIELD GmlLagAnt    AS DEC FORMAT ">>>,>>9.999-"
    FIELD Varetekst    AS CHAR FORMAT "x(40)"
    FIELD Storl        AS CHAR FORMAT "x(10)"
    FIELD ArtikkelNr   AS DEC FORMAT ">>>>>>>>>>>>9"
    .

{runlib.i}
{tmpfilliste.i &New="New"}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-HT-FilHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES HT-FilHode

/* Definitions for BROWSE BROWSE-HT-FilHode                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-HT-FilHode HT-FilHode.HTFilId ~
HT-FilHode.FilNavn HT-FilHode.FilEkst HT-FilHode.Butik HT-FilHode.FilDato ~
HT-FilHode.AntLinjer HT-FilHode.FilStorrelse HT-FilHode.Oppdatert ~
HT-FilHode.OppdatertAv HT-FilHode.AnnulertDato HT-FilHode.AnnulertAv ~
HT-FilHode.InnlestFra HT-FilHode.TypeId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-HT-FilHode HT-FilHode.HTFilId ~
HT-FilHode.FilNavn HT-FilHode.FilDato 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-HT-FilHode HT-FilHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-HT-FilHode HT-FilHode
&Scoped-define QUERY-STRING-BROWSE-HT-FilHode FOR EACH HT-FilHode ~
      WHERE HT-FilHode.TelleNr = wTelleNr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-HT-FilHode OPEN QUERY BROWSE-HT-FilHode FOR EACH HT-FilHode ~
      WHERE HT-FilHode.TelleNr = wTelleNr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-HT-FilHode HT-FilHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-HT-FilHode HT-FilHode


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-HT-FilHode}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-52 RECT-53 BUTTON-Detalj BUTTON-Slett-2 ~
B-NyeFiler B-Koblebutikk BUTTON-KobleFra B-Annuller Btn_Help Btn_OK ~
FILL-IN-SOK-STRING FILL-IN-SOK-DATE FILL-IN-SOK-INT BUTTON-Sok ~
BROWSE-HT-FilHode 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-STRING FILL-IN-SOK-DATE ~
FILL-IN-SOK-INT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Annuller 
     LABEL "Annuller oppdatering" 
     SIZE 24.2 BY 1.1 TOOLTIP "Trekker ~"ut~" tellelinjene fra tellelisten.".

DEFINE BUTTON B-Koblebutikk 
     LABEL "Oppdatere mot telleliste" 
     SIZE 27 BY 1.1 TOOLTIP "Leser inn og oppdaterer fil mot telleliste.".

DEFINE BUTTON B-NyeFiler 
     LABEL "Se etter nye filer" 
     SIZE 19 BY 1.1 TOOLTIP "Scanner filkatalog og lager liste over tilgjengelige tellefiler.".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U
     LABEL "&Help" 
     SIZE 4.4 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "icon/e-exit":U
     LABEL "OK" 
     SIZE 4.4 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Detalj 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Detalj" 
     SIZE 4.4 BY 1.1 TOOLTIP "Visning av innlest fil. Data fra håndterminal.".

DEFINE BUTTON BUTTON-KobleFra 
     LABEL "K&oble fra butikk" 
     SIZE 19 BY 1.1 TOOLTIP "Kobler den innleste filen fra butikk/telleliste.".

DEFINE BUTTON BUTTON-Slett-2 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "K&oble fra butikk" 
     SIZE 4.6 BY 1.1 TOOLTIP "Sletter håndterminalfilen fra database (Ikke fra filsystem).".

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INT AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-STRING AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174 BY .1.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174 BY .1.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-HT-FilHode FOR 
      HT-FilHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-HT-FilHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-HT-FilHode C-Win _STRUCTURED
  QUERY BROWSE-HT-FilHode NO-LOCK DISPLAY
      HT-FilHode.HTFilId COLUMN-LABEL "HTFilId *" FORMAT ">>>>>>>9":U
      HT-FilHode.FilNavn COLUMN-LABEL "FilNavn *" FORMAT "X(40)":U
      HT-FilHode.FilEkst FORMAT "x(8)":U
      HT-FilHode.Butik FORMAT "zzzzz9":U
      HT-FilHode.FilDato COLUMN-LABEL "FilDato *" FORMAT "99/99/9999":U
      HT-FilHode.AntLinjer FORMAT "zzzzzzz9":U
      HT-FilHode.FilStorrelse FORMAT "x(8)":U
      HT-FilHode.Oppdatert FORMAT "99/99/9999":U
      HT-FilHode.OppdatertAv FORMAT "x(8)":U
      HT-FilHode.AnnulertDato FORMAT "99/99/9999":U
      HT-FilHode.AnnulertAv FORMAT "X(15)":U WIDTH 14.2
      HT-FilHode.InnlestFra FORMAT "X(30)":U
      HT-FilHode.TypeId FORMAT "zz9":U
  ENABLE
      HT-FilHode.HTFilId
      HT-FilHode.FilNavn
      HT-FilHode.FilDato
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 174 BY 18.81 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-Detalj AT ROW 1.43 COL 1.8
     BUTTON-Slett-2 AT ROW 1.43 COL 7
     B-NyeFiler AT ROW 1.43 COL 15.2
     B-Koblebutikk AT ROW 1.43 COL 35
     BUTTON-KobleFra AT ROW 1.43 COL 87.2
     B-Annuller AT ROW 1.43 COL 107
     Btn_Help AT ROW 1.43 COL 165.2
     Btn_OK AT ROW 1.43 COL 170.2
     FILL-IN-SOK-STRING AT ROW 3.38 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 3.38 COL 2 NO-LABEL
     FILL-IN-SOK-INT AT ROW 3.38 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 3.38 COL 21.6
     BROWSE-HT-FilHode AT ROW 4.81 COL 2
     RECT-52 AT ROW 1.19 COL 1
     RECT-53 AT ROW 2.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 175 BY 22.86.

DEFINE FRAME FRAME-Progress
     FI-Info AT ROW 1.48 COL 1 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 10.52
         SIZE 63 BY 3.57
         TITLE "Behandler fil".


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
         TITLE              = "Import av filer fra håndterminal"
         HEIGHT             = 22.86
         WIDTH              = 175
         MAX-HEIGHT         = 22.86
         MAX-WIDTH          = 175
         VIRTUAL-HEIGHT     = 22.86
         VIRTUAL-WIDTH      = 175
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

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Progress:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-HT-FilHode BUTTON-Sok DEFAULT-FRAME */
ASSIGN 
       BROWSE-HT-FilHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 2.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-STRING IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-Progress
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-Progress:HIDDEN           = TRUE
       FRAME FRAME-Progress:SENSITIVE        = FALSE.

/* SETTINGS FOR FILL-IN FI-Info IN FRAME FRAME-Progress
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-HT-FilHode
/* Query rebuild information for BROWSE BROWSE-HT-FilHode
     _TblList          = "wr.HT-FilHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "HT-FilHode.TelleNr = wTelleNr"
     _FldNameList[1]   > wr.HT-FilHode.HTFilId
"HT-FilHode.HTFilId" "HTFilId *" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > wr.HT-FilHode.FilNavn
"HT-FilHode.FilNavn" "FilNavn *" "X(40)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = wr.HT-FilHode.FilEkst
     _FldNameList[4]   = wr.HT-FilHode.Butik
     _FldNameList[5]   > wr.HT-FilHode.FilDato
"HT-FilHode.FilDato" "FilDato *" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = wr.HT-FilHode.AntLinjer
     _FldNameList[7]   = wr.HT-FilHode.FilStorrelse
     _FldNameList[8]   = wr.HT-FilHode.Oppdatert
     _FldNameList[9]   = wr.HT-FilHode.OppdatertAv
     _FldNameList[10]   = wr.HT-FilHode.AnnulertDato
     _FldNameList[11]   > wr.HT-FilHode.AnnulertAv
"HT-FilHode.AnnulertAv" ? ? "character" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = wr.HT-FilHode.InnlestFra
     _FldNameList[13]   = wr.HT-FilHode.TypeId
     _Query            is OPENED
*/  /* BROWSE BROWSE-HT-FilHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Progress
/* Query rebuild information for FRAME FRAME-Progress
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Progress */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME FRAME-Progress:HANDLE
       ROW             = 2.67
       COLUMN          = 3
       HEIGHT          = .71
       WIDTH           = 60
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {35053A22-8589-11D1-B16A-00C0F0283628} type: ProgressBar */
      CtrlFrame:MOVE-AFTER(FI-Info:HANDLE IN FRAME FRAME-Progress).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import av filer fra håndterminal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import av filer fra håndterminal */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Annuller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Annuller C-Win
ON CHOOSE OF B-Annuller IN FRAME DEFAULT-FRAME /* Annuller oppdatering */
DO:
  FIND TelleHode NO-LOCK WHERE
    TelleHode.TelleNr = wTelleNr NO-ERROR.
  IF NOT AVAILABLE TelleHode THEN
    RETURN.
  
  IF TelleHode.Oppdatert <> ? THEN
    DO:
      MESSAGE "Telling er oppdatert, annullering kan ikke gjøres."
              VIEW-AS ALERT-BOX MESSAGE TITLE "Medling".
      RETURN NO-APPLY.
    END.
    
  RUN Annuller.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Koblebutikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koblebutikk C-Win
ON CHOOSE OF B-Koblebutikk IN FRAME DEFAULT-FRAME /* Oppdatere mot telleliste */
DO:
  RUN Koblebutikk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-NyeFiler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NyeFiler C-Win
ON CHOOSE OF B-NyeFiler IN FRAME DEFAULT-FRAME /* Se etter nye filer */
DO:
  DEF VAR wOkStatus AS INT NO-UNDO.
  ASSIGN wAntTyper = 0.
  /* Teller antall registrerte hådnterminaltyper */
  FOR EACH HT-Type NO-LOCK:
    ASSIGN wAntTyper = wAntTyper + 1.
  END.
  /* Ber bruker velge hvis det er mer enn en. */
  IF wAntTyper > 1 THEN
    DO:
      RUN d-bht-type.w (INPUT-OUTPUT wTypeId).      
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
      FIND HT-Type NO-LOCK WHERE
        HT-Type.TypeId = wTypeId NO-ERROR.
    END.
  ELSE DO:
    FIND FIRST HT-Type NO-LOCK NO-ERROR.
    IF NOT AVAILABLE HT-Type THEN
      DO:
        MESSAGE "Det er ikke satt opp noen håndterminalsdefinisjoner!"
                VIEW-AS ALERT-BOX WARNING TITLE "Advarsel".
        RETURN NO-APPLY.
      END.
    ELSE
      wTypeId = HT-Type.TypeId.
  END.       

  IF wTypeId = 7 THEN
  DO:
      MESSAGE 'Kun automatisk innlesning via batch server av denne filtypen'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  ASSIGN
    wKatalog    = HT-Type.ImportKatalog
    wFilPrefix  = HT-Type.FilPrefix
    wFilPrefix  = SUBSTITUTE(wFilPrefix,trim(string(TelleHode.Butik)))
    wFilekstent = HT-Type.FilEkstent
    wImpProgram = HT-Type.ImportProg
    .  

  /* Overstyrer for PDA */
  IF can-do("5,6,7,8,10,12",string(wTypeId)) AND HT-Type.FilEkstent = "<ButNr>" THEN
      wFilekstent = TelleHode.ButikkListe.

  RUN byggfilliste.w (INPUT wKatalog, 
                      INPUT wFilPrefix,
                      INPUT wFilekstent,
                      OUTPUT wOkstatus).
    
  IF wOkStatus = 1 THEN
    DO:
      MESSAGE "Ingen nye filer å importere" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  ELSE IF wOkStatus = 2 THEN
    DO:
      MESSAGE "Ukjent filkatalog satt opp i systemparameter - kontakt systemansvarlig" VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
  RUN AddRecords.  
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  APPLY "ENTRY":U TO BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-HT-FilHode
&Scoped-define SELF-NAME BROWSE-HT-FilHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HT-FilHode C-Win
ON ANY-PRINTABLE OF BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HT-FilHode C-Win
ON CURSOR-LEFT OF BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HT-FilHode C-Win
ON CURSOR-RIGHT OF BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HT-FilHode C-Win
ON DEFAULT-ACTION OF BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME
DO:
  /*
  APPLY "CHOOSE" TO Btn_OK.
  RETURN NO-APPLY.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HT-FilHode C-Win
ON ROW-DISPLAY OF BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME
DO:
  IF HT-FilHode.Oppdatert <> ? THEN
   ASSIGN        
      HT-FilHode.FilNavn:bgcolor IN BROWSE BROWSE-HT-FilHode = ?.        
  ELSE IF HT-FilHode.Butik <> 0 THEN
   ASSIGN        
      HT-FilHode.FilNavn:bgcolor IN BROWSE BROWSE-HT-FilHode = 10.        
  ELSE
   ASSIGN        
      HT-FilHode.FilNavn:bgcolor IN BROWSE BROWSE-HT-FilHode = 11.        
      
  IF HT-FilHode.Butik <> 0 THEN
    ASSIGN
      HT-FilHode.Butik:bgcolor IN BROWSE BROWSE-HT-FilHode = 10.        
      
      
  /* Flagger at det er feil i filen. */
  IF CAN-FIND(FIRST HT-FilLinje OF HT-FilHode WHERE
     HT-FilLinje.Ok = FALSE) THEN
   ASSIGN        
      HT-FilHode.HTFilId:bgcolor IN BROWSE BROWSE-HT-FilHode = 12.        
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-HT-FilHode C-Win
ON START-SEARCH OF BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME
DO:
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
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
  
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
  &ELSE
      retur-verdi = "OK".
  &ENDIF
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Detalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Detalj C-Win
ON CHOOSE OF BUTTON-Detalj IN FRAME DEFAULT-FRAME /* Detalj */
DO:
  DEF VAR wLinjeId AS INT NO-UNDO.
  
  IF NOT AVAILABLE HT-FilHode THEN
    RETURN.
    
  RUN d-bht-fillinje.w (INPUT-OUTPUT wLinjeId, INPUT HT-FilHode.HTFilId).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  APPLY "ENTRY":U TO BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-KobleFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-KobleFra C-Win
ON CHOOSE OF BUTTON-KobleFra IN FRAME DEFAULT-FRAME /* Koble fra butikk */
DO:
  DEF VAR wLoop AS INT.
  DEF VAR wAntLinjer AS INT NO-UNDO.  
  
  IF BROWSE-HT-FilHode:NUM-SELECTED-ROWS = 0 THEN
    DO:
      MESSAGE "Det er ingen merkede rader å ta bort!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
  MESSAGE "Skal filen(e) kobles fra?"
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY.
  
  {sww.i}
  SLETTE_LOOP:
  DO wLoop = BROWSE-HT-FilHode:NUM-SELECTED-ROWS TO 1 BY -1:
    wOk = BROWSE-HT-FilHode:FETCH-SELECTED-ROW(wLoop).

    /* er postene oppdatert, skal de ikke kunne slettes */
    IF HT-FilHode.Oppdatert <> ? THEN
      NEXT SLETTE_LOOP.

    ASSIGN
      wAntLinjer = 0
      FI-Info    = "Kobler fra: " + HT-FilHode.InnlestFra.
    IF HT-FilHode.AntLinjer > 0 THEN
      DO:  
        ASSIGN
          chCtrlFrame:ProgressBar:Min   = 1
          chCtrlFrame:ProgressBar:Max   = HT-FilHode.AntLinjer
          chCtrlFrame:ProgressBar:Value = 1.
        VIEW FRAME FRAME-Progress.
        DISPLAY FI-Info WITH FRAME FRAME-Progress.
      END.

    /* Sender posten til den siste hvile. */
    IF AVAILABLE {&br-tabell} THEN
      DO:
        FOR EACH HT-FilLinje OF HT-FilHode TRANSACTION:

          ASSIGN
            wAntLinjer = wAntLinjer + 1.
        
          IF wAntLinjer MODULO 10 = 0 AND HT-FilHode.AntLinjer > 0 THEN
            chCtrlFrame:ProgressBar:Value = wAntLinjer.


          DELETE HT-FilLinje.
        END. /* TRANSACTION */
        
        DO TRANSACTION:
          FIND CURRENT {&br-tabell} EXCLUSIVE-LOCK.
          ASSIGN
            HT-FilHode.FeilIFil  = FALSE
            HT-FilHode.AntLinjer = 0
            HT-FilHode.Butik     = 0.
          RELEASE HT-FilHode.
        END. /* TRANSACTION */
        wOk = BROWSE-HT-FilHode:DESELECT-SELECTED-ROW(wLoop).
      END.

    wOk = BROWSE-HT-FilHode:REFRESH( ).
  END. /* SLETTE_LOOP */  
  HIDE FRAME FRAME-Progress NO-PAUSE.
  {swn.i}
  
 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett-2 C-Win
ON CHOOSE OF BUTTON-Slett-2 IN FRAME DEFAULT-FRAME /* Koble fra butikk */
DO:
  DEF VAR wLoop AS INT.
  DEF VAR wAntLinjer AS INT NO-UNDO.  
  
  IF BROWSE-HT-FilHode:NUM-SELECTED-ROWS = 0 THEN
    DO:
      MESSAGE "Det er ingen merkede rader å ta bort!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
  MESSAGE {&SletteMelding}
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY.
  
  {sww.i}
  SLETTE_LOOP:
  DO wLoop = BROWSE-HT-FilHode:NUM-SELECTED-ROWS TO 1 BY -1 TRANSACTION:
    wOk = BROWSE-HT-FilHode:FETCH-SELECTED-ROW(wLoop).

    ASSIGN
      wAntLinjer = 0
      FI-Info    = "Tar bort: " + HT-FilHode.InnlestFra.

    /* Sender posten til den siste hvile. */
    IF AVAILABLE {&br-tabell} THEN
      DO:
        FIND CURRENT {&br-tabell} EXCLUSIVE-LOCK.
        FOR EACH HT-FilLinje OF HT-FilHode:

          ASSIGN
            wAntLinjer = wAntLinjer + 1.
        
          DELETE HT-FilLinje.
        END.
        DELETE {&br-tabell}.
        wOk = BROWSE-HT-FilHode:DELETE-SELECTED-ROW(wLoop).
      END.

  END. /* SLETTE_LOOP */  
  {swn.i}
  
  APPLY "ENTRY":U TO BROWSE-HT-FilHode IN FRAME DEFAULT-FRAME.
 
  
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

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall  = "Nei"
  &WindowName = "ImportFilListeHT"
  &PreIClose  = "RUN TaBortIkkeKobledeFiler."
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 1 2 4 wEDB-System}
ASSIGN wTabell = "ArtBas".
{syspara.i 5 1 1 wCl INT}
FIND clButiker WHERE
  clbutiker.Butik = wCl.

FIND TelleHode NO-LOCK WHERE
  TelleHode.TelleNr = wTelleNr NO-ERROR.
IF NOT AVAILABLE TelleHode THEN
  DO:
    MESSAGE "Ukjent tellehode!"
            VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN "AVBRYT".
  END.  

FOR EACH tmpVareTran:
    DELETE tmpVareTran.
END.

/* Finner butikk som listen er koblet mot. */
IF Tellehode.ButikkListe = "" OR NUM-ENTRIES(TelleHode.ButikkListe) > 1 THEN
DO:
    RUN d-bbutike2.w (INPUT-OUTPUT wButikk, INPUT TelleHode.ButikkListe).
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY "AVBRYT".
    FIND Butiker NO-LOCK WHERE
      recid(Butiker) = int(RETURN-VALUE) NO-ERROR.
    wButikk = Butiker.Butik.
END.
ELSE DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INT(TelleHode.ButikkListe) NO-ERROR.
    IF AVAILABLE Butiker THEN
        wButikk = Butiker.Butik.
    ELSE
        RETURN "AVBRYT".
END.

/* Tømmer fillisten for filer som ikke er oppdatert */
IF wHtFilId = 0 THEN /* Er wHtFilId > 0, kommer filhode fra kallende program */
FOR EACH HT-FilHode EXCLUSIVE-LOCK WHERE
    HT-FilHode.TelleNr = TelleHode.TelleNr AND
    HT-FilHode.Oppdatert = ?:
    DELETE HT-FilHode.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  wOrgBgCol = {&sortby-col1}:Label-bgcolor IN BROWSE {&BROWSE-NAME}.
  RUN LabelColor.
  RUN Move-Fill-To-Top.
  RUN enable_UI.
  {lng.i} 
  ASSIGN
    C-Win:title = C-Win:title + " for telling " + string(TelleHode.TelleNr) + 
    " " + TelleHode.Beskrivelse.
    
  IF {&ant-sok-field} > 1 THEN
      RUN Init-Read-Only.
  &IF DEFINED(init-phrase) &THEN
  {&init-phrase}
  
  IF AVAILABLE b{&br-tabell} THEN DO:
      RUN SD-Reposition.
  END.
  &ENDIF

  /* Scanner katalog for nye filer. */
  IF wHtFilId = 0 THEN /* Er wHtFilId > 0, kommer filhode fra kallende program */
      APPLY "CHOOSE" TO B-NyeFiler.

  /* Tell opp antall filer. Er det bare en fil, start oppdatering direkte */
  iAntFiler = 0.
  FOR EACH HT-FilHode NO-LOCK WHERE
      HT-FilHode.TelleNr = TelleHode.TelleNr AND 
      HT-FilHode.Oppdatert = ?:
      ASSIGN
          iAntFiler = iAntFiler + 1
          .
  END.
  IF iAntFiler >= 1 THEN
  DO:
      wSvar = FALSE.
      IF wHtFilId = 0 THEN
      DO:
          MESSAGE "Det ligger en eller flere filer (Ant filer = " + STRING(iAntFiler) + ") fra håndterminalen klar for innlesning." SKIP
                  "Skal innlesning og oppdatering mot telleliste starte?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Bekreft"
                  UPDATE wSvar.
      END.
      ELSE wSvar = TRUE.
      IF wSvar = FALSE THEN 
      DO:
          retur-verdi = "AVBRYT".
          LEAVE MAIN-BLOCK.
      END.
      ELSE IF wSvar = ? THEN
      DO:
          APPLY "ENTRY"  TO BROWSE {&BROWSE-NAME}.

          IF NOT THIS-PROCEDURE:PERSISTENT THEN
            WAIT-FOR CLOSE OF THIS-PROCEDURE.
      END.
      ELSE DO:
          FOR EACH HT-FilHode NO-LOCK WHERE
              HT-FilHode.TelleNr = TelleHode.TelleNr AND
              HT-FilHode.Oppdatert = ?:
              RUN KobleButikk2 (HT-FilHode.HTFilId).
          END.
          APPLY "CHOOSE" TO Btn_Ok.
      END.
  END.
  ELSE DO:
      APPLY "ENTRY"  TO BROWSE {&BROWSE-NAME}.

      IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.

END. /* MAIN-BLOCK */

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 RETURN retur-verdi.
&else
 MESSAGE retur-verdi VIEW-AS ALERT-BOX.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2Denso2000v1-0 C-Win 
PROCEDURE 2Denso2000v1-0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

Min kommentar: Denso 2000 v. 1.0
1-4  : Säljare
5-3  : VG 
8-4  : Löpnr
12-1 : Prisgrupp
13-4 : Storlek
17-10: Datum/tid ÅÅMMDDTTMM

123456789012345678901234567890

Eksempel:
00010040023000909812200854
00010040023000809812200854
00010040025000659812200854
00010040023001009812200854
00010040023000909812200854
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipLinjeData AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipStorl     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipAntall    AS INT NO-UNDO.

  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,5,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,8,4))
    ipStorl  = substring(HT-FilLinje.LinjeData,13,4)
    ipAntall = 1. /* En linje pr. par. */

  FIND ArtBas NO-LOCK WHERE
      ArtBas.Vg = ipVg AND
      ArtBas.LopNr = ipLopNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
      wArtikkelNr = ArtBas.ArtikkelNr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2DensoBHT5000GEN2 C-Win 
PROCEDURE 2DensoBHT5000GEN2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

Min kommentar: Denso BHT 5000 GEN2

4-3  : VG 
7-4  : Löpnr
11-1  : Prisgrupp
12-4 : Storlek

123456789012345678901234567890

Eksempel:
MDIMP,19991109,STD1
IH,5,
IC,030102300360,1
IC,030102300365,1
IC,030102300370,1
IC,030102300375,1
IC,030102300380,1
IC,030102300410,1
IC,030108900365,1

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipLinjeData AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipStorl     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipAntall    AS INT NO-UNDO.

  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,4,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,7,4))
    ipStorl  = substring(HT-FilLinje.LinjeData,12,4)
    ipAntall = int(entry(3,HT-FilLinje.LinjeData)).

  FIND ArtBas NO-LOCK WHERE
      ArtBas.Vg = ipVg AND
      ArtBas.LopNr = ipLopNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
      wArtikkelNr = ArtBas.ArtikkelNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2DT900 C-Win 
PROCEDURE 2DT900 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipLinjeData AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipStorl     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipAntall    AS INT NO-UNDO.
  DEF OUTPUT PARAMETER pcEAN       AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipSeqNr     AS INT  NO-UNDO.

  DEF VAR cStrekkode AS CHAR NO-UNDO.

  IF AVAILABLE ArtBas THEN
      RELEASE ArtBAs.

  cStrekKode = string(dec(ENTRY(6,ipLinjeData)),">>>>>>>>>>>>9").
  cStrekKode = IF LENGTH(cStrekKode) < 6 
               THEN STRING(cStrekKode) 
               ELSE IF LENGTH(cStrekKode) = 6 
                    THEN DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib, 
                         INPUT DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT STRING(cStrekKode)))
                    ELSE STRING(dec(ENTRY(6,ipLinjeData)),"9999999999999").
  /* Sjekksifferkontroll */
  IF (LENGTH(cStrekkode) = 13 OR 
  LENGTH(cStrekkode) = 8) THEN
  ASSIGN cStrekkode = DYNAMIC-FUNCTION('EANprefixKonv':U IN h_dproclib, INPUT cStrekkode).

  /* Sjekker med nullutfylling. */
  FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cStrekkode NO-ERROR.
  /* Sjekker uten nullutfylling. */
  IF NOT AVAILABLE Strekkode THEN
  DO:
    FIND Strekkode NO-LOCK WHERE Strekkode.Kode = LEFT-TRIM(cStrekkode,"0") NO-ERROR.
    IF AVAILABLE Strekkode THEN
      cStrekkode = LEFT-TRIM(cStrekkode,"0").         
  END.

  ASSIGN
      pcEan = cStrekkode
      .
  IF AVAILABLE Strekkode AND Strekkode.ArtikkelNr > 0 THEN
    FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
  IF AVAILABLE Strekkode THEN
  DO:
      /* Strekkode med størrelseskode = 0, skal alltid legges på den første størrelsen */
      /* i aktuell størrelsestype for artikkelen.                                      */
      IF StrekKode.StrKode = 0 THEN
      DO:
          IF AVAILABLE ArtBas THEN
              FIND StrType OF ArtBas NO-LOCK NO-ERROR.
          IF AVAILABLE StrType THEN 
          DO:
              FIND FIRST StrTStr OF StrType NO-LOCK NO-ERROR.
              IF AVAILABLE StrTStr THEN
              DO:
                  FIND FIRST StrKonv WHERE
                      StrKonv.Storl = StrTStr.SoStorl NO-ERROR.
              END.
          END.
      END.
      /* Standard strekkode. */
      ELSE
          FIND StrKonv NO-LOCK WHERE
              StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
  END.

  IF AVAILABLE ArtBas AND ArtBas.ArtikkelNr > 0 THEN
    ASSIGN
    ipVg     = ArtBas.Vg
    ipLopNr  = ArtBas.LopNr
    ipStorl  = IF AVAILABLE StrKonv
                 THEN StrKonv.Storl
                 ELSE ""
    ipAntall = INT(ENTRY(7,ipLinjeData))
    wArtikkelNr = ArtBAs.ArtikkelNr
    .
  /* plukker ut sekvensnummer på størrelsen */
  IF num-entries(ipLinjeData) = 9 THEN
      ipSeqNr = int(ENTRY(9,ipLinjeData)).
  ELSE
      ipSeqNr = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2FsgGen C-Win 
PROCEDURE 2FsgGen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipLinjeData AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipStorl     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipAntall    AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipPris      AS DECI NO-UNDO.

  ASSIGN
    ipVg     = INT(ENTRY(1,ENTRY(1,HT-FilLinje.LinjeData,";"),"-"))
    ipLopNr  = INT(ENTRY(2,ENTRY(1,HT-FilLinje.LinjeData,";"),"-"))
    ipStorl  = ENTRY(2,HT-FilLinje.LinjeData,";")
    ipAntall = INT(ENTRY(3,HT-FilLinje.LinjeData,";"))
    ipPris   = DECI(ENTRY(4,HT-FilLinje.LinjeData,";")).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2Memor128-Ean C-Win 
PROCEDURE 2Memor128-Ean :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
Memor 128
---------------  
1-1  : Postid 
2-4  : VG
5-4  : Löpnr
9-1  : Prisgrupp
10-4: Storlek
14-3: Antal 3 siffror vänsterjusterat spaceutfyllt
17-10: Datum/tid ÅÅMMDDHHMM

12345678901234567890123456789

Här börjar filen:
--------------------------
00183063000101  9604161018
00181040004501  9604161019
00183063000101  9604161019
00183063000101  9604161019
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipLinjeData AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipStorl     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipAntall    AS INT NO-UNDO.

  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,2,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,5,4))
    ipStorl  = substring(HT-FilLinje.LinjeData,9,4)
    ipAntall = int(substring(HT-FilLinje.LinjeData,14,3)).

  FIND ArtBas NO-LOCK WHERE
      ArtBas.Vg = ipVg AND
      ArtBas.LopNr = ipLopNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
      wArtikkelNr = ArtBas.ArtikkelNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2Memor128v1-0 C-Win 
PROCEDURE 2Memor128v1-0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
Memor 128
---------------  
1-1  : Postid 
2-4  : VG
5-4  : Löpnr
9-1  : Prisgrupp
10-4: Storlek
14-3: Antal 3 siffror vänsterjusterat spaceutfyllt
17-10: Datum/tid ÅÅMMDDHHMM

12345678901234567890123456789

Här börjar filen:
--------------------------
00183063000101  9604161018
00181040004501  9604161019
00183063000101  9604161019
00183063000101  9604161019
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipLinjeData AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT NO-UNDO.
  DEF OUTPUT PARAMETER ipStorl     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipAntall    AS INT NO-UNDO.

  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,2,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,5,4))
    ipStorl  = substring(HT-FilLinje.LinjeData,10,4)
    ipAntall = int(substring(HT-FilLinje.LinjeData,14,3)).

  FIND ArtBas NO-LOCK WHERE
      ArtBas.Vg = ipVg AND
      ArtBas.LopNr = ipLopNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
      wArtikkelNr = ArtBas.ArtikkelNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2Varetran C-Win 
PROCEDURE 2Varetran :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
      HT-FilLinje.LinjeData  = string(tmpVareTran.ButikkNr)    + CHR(1) + 
                               string(tmpVareTran.Strekkode)   + CHR(1) +   
                               string(tmpVareTran.Dato)        + CHR(1) +   
                               string(tmpVareTran.tid)         + CHR(1) +   
                               string(tmpVareTran.LoggNr)      + CHR(1) +   
                               string(tmpVareTran.TransType)   + CHR(1) +  
                               string(tmpVareTran.Transtekst)  + CHR(1) +   
                               string(tmpVareTran.BrukerId)    + CHR(1) +   
                               string(tmpVareTran.Antall)      + CHR(1) +   
                               string(tmpVareTran.Kostpris)    + CHR(1) +   
                               string(tmpVareTran.Salgssum)    + CHR(1) +   
                               string(tmpVareTran.NyLagAnt)    + CHR(1) +   
                               string(tmpVareTran.GmlLagAnt)   + CHR(1) +   
                               string(tmpVareTran.ArtikkelNr)  

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipLinjeData AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER lArtikkelNr AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER ipStorl     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipAntall    AS INT NO-UNDO.
  DEF OUTPUT PARAMETER pcEAN       AS CHAR NO-UNDO.

  DEF VAR cStrekkode AS CHAR NO-UNDO.

  cStrekKode = string(dec(ENTRY(2,ipLinjeData,CHR(1))),">>>>>>>>>>>>9").
  cStrekKode = IF LENGTH(cStrekKode) < 6 
               THEN STRING(cStrekKode) 
               ELSE IF LENGTH(cStrekKode) = 6 
                    THEN DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib, 
                         INPUT DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT STRING(cStrekKode)))
                    ELSE STRING(dec(ENTRY(2,ipLinjeData,CHR(1))),"9999999999999").
  /* Sjekksifferkontroll */
  IF (LENGTH(cStrekkode) = 13 OR 
  LENGTH(cStrekkode) = 8) THEN
  ASSIGN cStrekkode = DYNAMIC-FUNCTION('EANprefixKonv':U IN h_dproclib, INPUT cStrekkode).

  /* Sjekker med nullutfylling. */
  FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cStrekkode NO-ERROR.
  /* Sjekker uten nullutfylling. */
  IF NOT AVAILABLE Strekkode THEN
  DO:
    FIND Strekkode NO-LOCK WHERE Strekkode.Kode = LEFT-TRIM(cStrekkode,"0") NO-ERROR.
    IF AVAILABLE Strekkode THEN
      cStrekkode = LEFT-TRIM(cStrekkode,"0").         
  END.

  ASSIGN
      pcEan = cStrekkode
      .
  IF AVAILABLE Strekkode THEN
  DO:
      FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
      /* Strekkode med størrelseskode = 0, skal alltid legges på den første størrelsen */
      /* i aktuell størrelsestype for artikkelen.                                      */
      IF StrekKode.StrKode = 0 THEN
      DO:
          IF AVAILABLE ArtBas THEN
              FIND StrType OF ArtBas NO-LOCK NO-ERROR.
          IF AVAILABLE StrType THEN 
          DO:
              FIND FIRST StrTStr OF StrType NO-LOCK NO-ERROR.
              IF AVAILABLE StrTStr THEN
              DO:
                  FIND FIRST StrKonv WHERE
                      StrKonv.Storl = StrTStr.SoStorl NO-ERROR.
              END.
          END.
      END.
      /* Standard strekkode. */
      ELSE
          FIND StrKonv NO-LOCK WHERE
              StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
  END.

  IF AVAILABLE ArtBas THEN
    ASSIGN
    lArtikkelNr = ArtBas.ArtikkelNr
    ipStorl  = IF AVAILABLE StrKonv
                 THEN StrKonv.Storl
                 ELSE " 1"
    ipAntall = INT(ENTRY(9,ipLinjeData,CHR(1)))
    .
  ELSE
      ASSIGN
          lArtikkelNr = ?
          ipAntall    = INT(ENTRY(9,ipLinjeData,CHR(1)))
          ipStorl     = ' 1'.
          .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddRecords C-Win 
PROCEDURE AddRecords :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wHTFilId AS INT NO-UNDO.

  DO TRANSACTION:
    FIL-LISTE:
    FOR EACH tmpFilListe:

      IF wFilPrefix <> "" THEN
        DO:
          IF NOT tmpFilListe.FilNavn BEGINS wFilPrefix THEN
            NEXT FIL-LISTE.
        END.
        
      IF wFilekstent <> "" THEN
        DO:
          IF tmpFilListe.Ekstent <> wFilEkstent THEN
            NEXT FIL-LISTE.
        END.

      /* Logger filen i listen hvis den ikke har vært lastet før dette året. */
      IF NOT CAN-FIND(FIRST HT-FilHode WHERE
        HT-FilHode.FilNavn  = tmpFilListe.FilNavn AND
        HT-FilHode.FilEkst  = tmpFilListe.Ekstent AND
        HT-FilHode.FilDato  = tmpFilListe.FilDato AND
        HT-FilHode.FilTid   = tmpFilListe.FilTid) THEN
        DO:                
          /* Finner ID */
          FIND LAST HT-FilHode NO-LOCK USE-INDEX HTFilId NO-ERROR.
          IF AVAILABLE HT-FilHode THEN
            wHTFilId = HT-FilHode.HTFilId + 1.
          ELSE
            wHTFilId = 1.
            
          CREATE HT-FilHode.
          ASSIGN
            HT-FilHode.HTFilId = wHTFilId.
            
          ASSIGN
            HT-FilHode.FilNavn      = tmpFilListe.FilNavn 
            HT-FilHode.FilEkst      = tmpFilListe.Ekstent 
            HT-FilHode.FilDato      = tmpFilListe.FilDato
            HT-FilHode.FilTid       = tmpFilListe.FilTid
            HT-FilHode.InnlestFra   = tmpFilListe.FullPath
            HT-FilHode.FilStorrelse = string(tmpFilListe.File-size)
            HT-FilHode.TelleNr      = wTelleNr
            HT-FilHode.TypeId       = wTypeId.
        END.
       
    END. /* FIL-LISTE */
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Annuller C-Win 
PROCEDURE Annuller :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wLoop      AS INT.
  DEF VAR wAntLinjer AS INT  NO-UNDO.  
  DEF VAR wVg        AS INT  NO-UNDO.
  DEF VAR wLopNr     AS INT  NO-UNDO.
  DEF VAR wStorl     AS CHAR NO-UNDO.
  DEF VAR wAntall    AS INT  NO-UNDO.
  DEF VAR wVVareKost AS DEC  NO-UNDO.
  DEF VAR wEAN        AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
  
  IF BROWSE-HT-FilHode:NUM-SELECTED-ROWS = 0 THEN
    DO:
      MESSAGE "Det er ingen merkede filer å annullere!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  
  MESSAGE "Skal annullering av HT-Fil(er) mot telleliste starte?"
  VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Bekreftelse" 
  UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY.
  
  {sww.i}
  OPPDAT_LOOP:
  DO wLoop = BROWSE-HT-FilHode:NUM-SELECTED-ROWS TO 1 BY -1:
    wOk = BROWSE-HT-FilHode:FETCH-SELECTED-ROW(wLoop).

    /* er postene ikke oppdatert, skal de ikke annulleres */
    IF HT-FilHode.Oppdatert = ? THEN
      NEXT OPPDAT_LOOP.
      
    /* er postene ikke koblet mot butikk de ikke annulleres. */
    IF HT-FilHode.Butik = 0 THEN
      NEXT OPPDAT_LOOP.    

    ASSIGN
      wAntLinjer = 0
      FI-Info    = "Annullerer fil: " + HT-FilHode.InnlestFra.
    ASSIGN
      chCtrlFrame:ProgressBar:Min   = 1
      chCtrlFrame:ProgressBar:Max   = (IF HT-FilHode.AntLinjer < 10
                                         THEN 10
                                         ELSE HT-FilHode.AntLinjer)
      chCtrlFrame:ProgressBar:Value = 1.
    VIEW FRAME FRAME-Progress.
    DISPLAY FI-Info WITH FRAME FRAME-Progress.

    /* Oppdaterer filen. */
    TRANS_BLOKK:
    DO:
      IF AVAILABLE {&br-tabell} THEN
        DO:
          
          LES_LINJE:
          FOR EACH HT-FilLinje OF HT-FilHode EXCLUSIVE-LOCK WHERE
            HT-FilLinje.Ok = TRUE TRANSACTION:

            ASSIGN
              wAntLinjer = wAntLinjer + 1.
        
            IF wAntLinjer MODULO 10 = 0 THEN
              chCtrlFrame:ProgressBar:Value = wAntLinjer.
              
            /* Legger data inn i HT-FilLinje. */
            CASE HT-FilHode.TypeId:
              WHEN 1 THEN  /* DensoBHT5000GEN2 */
                DO:
                  /* Den første posten er alltid en datolinje. */
                  IF NOT HT-FilLinje.LinjeData BEGINS wFilPrefix THEN
                    NEXT LES_LINJE.
                  RUN 2DensoBHT5000GEN2 (HT-FilLinje.LinjeData, 
                                         OUTPUT wVg,
                                         OUTPUT wLopNr,
                                         OUTPUT wStorl,
                                         OUTPUT wAntall).
                END.
              WHEN 2 THEN RUN 2Denso2000v1-0 (HT-FilLinje.LinjeData, 
                                              OUTPUT wVg,
                                              OUTPUT wLopNr,
                                              OUTPUT wStorl,
                                              OUTPUT wAntall).
              WHEN 3 THEN RUN 2Memor128v1-0 (HT-FilLinje.LinjeData, 
                                             OUTPUT wVg,
                                             OUTPUT wLopNr,
                                             OUTPUT wStorl,
                                             OUTPUT wAntall).
              WHEN 4 THEN RUN 2Memor128-Ean (HT-FilLinje.LinjeData, 
                                             OUTPUT wVg,
                                             OUTPUT wLopNr,
                                             OUTPUT wStorl,
                                             OUTPUT wAntall).
              WHEN 5 THEN RUN 2DT900 (HT-FilLinje.LinjeData, 
                                      OUTPUT wVg,
                                      OUTPUT wLopNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
              WHEN 6 THEN RUN 2Varetran (HT-FilLinje.LinjeData, /* SymbPPT8800 */
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
              WHEN 7 THEN RUN 2Varetran (HT-FilLinje.LinjeData, 
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
              WHEN 8 THEN RUN 2Varetran (HT-FilLinje.LinjeData, 
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
            END CASE. 

/*             /* Prepper størrelsen og konverterer den. */ */
/*             wStorl = string(dec(wStorl) / 10).           */
/*             wStorl = FiksStorl(wStorl).                  */

            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = wEAN NO-ERROR.
            IF AVAILABLE Strekkode THEN
                FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
            
            IF NOT AVAILABLE ArtBas THEN
                FIND ArtBas NO-LOCK WHERE
                  ArtBas.Vg    = wVg AND
                  ArtBas.LopNr = wLopNr NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN
              DO:
                HT-FilLinje.Ok = FALSE.
                NEXT LES_LINJE.
              END.

            /* Henter TelleLinjen om den finnes. */
            FIND TelleLinje EXCLUSIVE-LOCK WHERE
              TelleLinje.TelleNr    = wTelleNr AND
              TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
              TelleLinje.Butik      = HT-FilHode.Butik AND
              TelleLinje.Storl      = wStorl NO-ERROR.            
              
            /* Finnes ikke linjen, skippes posten. */
            IF NOT AVAILABLE TelleLinje THEN DO:
                NEXT LES_LINJE.
            END.
              
            /* Øvrig informasjon. */
            ASSIGN
              TelleLinje.AntallTalt = TelleLinje.AntallTalt - wAntall
              TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
              TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
              TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost.
    
            /* Flagger linjen som ikke oppdatert */
            ASSIGN
              HT-FilLinje.Ok = ?.
          END. /* LES_LINJE TRANSACTION */
          
          DO TRANSACTION:
            FIND CURRENT {&br-tabell} EXCLUSIVE-LOCK.
            ASSIGN
              HT-FilHode.Oppdatert    = ?
              HT-FilHode.OppdatertTid = 0
              HT-FilHode.OppdatertAv  = ""
              HT-FilHode.AnnulertDato = TODAY
              HT-FilHode.AnnulertAv   = userid("dictdb").
          END.
          FIND CURRENT {&br-tabell} NO-LOCK.
        END.
    END. /* TRANS_BLOKK TRANSACTION */

    wOk = BROWSE-HT-FilHode:DESELECT-SELECTED-ROW(wLoop).

  END. /* OPPDAT_LOOP */  
  HIDE FRAME FRAME-Progress NO-PAUSE.
  {swn.i}
  
  wOk = BROWSE-HT-FilHode:REFRESH( ).
  
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
        wDataType = {&sortby-col1}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
    END.
    &IF DEFINED(sortby-phrase2) &THEN
    WHEN 2 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase2}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col2}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase3) &THEN
    WHEN 3 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase3}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col3}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase4) &THEN
    WHEN 4 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase4}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col4}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
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

OCXFile = SEARCH( "w-bht-filhode.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-bht-filhode.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Denso2000v1-0 C-Win 
PROCEDURE Denso2000v1-0 :
/*------------------------------------------------------------------------------
Purpose:     
Parameters:  <none>

Min kommentar: Denso 2000 v. 1.0

1-4  : Säljare
5-7  : VG 
8-11 : Löpnr
12-12: Prisgrupp
13-16: Storlek
17-26: Datum/tid ÅÅMMDDTTMM

123456789012345678901234567890

Eksempel:
00010040023000909812200854
00010040023000809812200854
00010040025000659812200854
00010040023001009812200854
00010040023000909812200854

Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipAntLinjer AS INT  NO-UNDO.
  DEF INPUT  PARAMETER ipDataLinje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipFeilIFil  AS LOG  NO-UNDO.

  CREATE HT-FilLinje.
  ASSIGN
    HT-FilLinje.HTFilId   = HT-FilHode.HTFilId
    HT-FilLinje.LinjeId   = ipAntLinjer
    HT-FilLinje.LinjeData = ipDataLinje
    HT-FilLinje.Ok        = ?.
        
  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,5,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,8,4)).
  IF NOT CAN-FIND(ArtBas WHERE
    ArtBas.Vg    = ipVg AND
    ArtBas.LopNr = ipLopNr) THEN
     ASSIGN 
       ipFeilIFil      = TRUE
       HT-FilLinje.Ok = FALSE.
  IF ipAntLinjer = 1 THEN
    HT-FilLinje.Ok = ?.               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DensoBHT5000GEN2 C-Win 
PROCEDURE DensoBHT5000GEN2 :
/*------------------------------------------------------------------------------
Purpose:     
Parameters:  <none>

Min kommentar: Denso BHT 5000 GEN2

1-3  : VG 
4-7  : Löpnr
8-8  : Prisgrupp
9-12 : Storlek

123456789012345678901234567890

Eksempel:
MDIMP,19991109,STD1
IH,5,
IC,030102300360,1
IC,030102300365,1
IC,030102300370,1
IC,030102300375,1
       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipAntLinjer AS INT  NO-UNDO.
  DEF INPUT  PARAMETER ipDataLinje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipFeilIFil  AS LOG  NO-UNDO.

  CREATE HT-FilLinje.
  ASSIGN
    HT-FilLinje.HTFilId   = HT-FilHode.HTFilId
    HT-FilLinje.LinjeId   = ipAntLinjer
    HT-FilLinje.LinjeData = ipDataLinje
    HT-FilLinje.Ok        = ?.
        
  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,4,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,7,4)).
    
  IF NOT CAN-FIND(ArtBas WHERE
    ArtBas.Vg    = ipVg AND
    ArtBas.LopNr = ipLopNr) THEN
     ASSIGN 
       ipFeilIFil      = TRUE
       HT-FilLinje.Ok = FALSE.
  IF ipAntLinjer = 1 THEN
    HT-FilLinje.Ok = ?.               
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DT900 C-Win 
PROCEDURE DT900 :
/*------------------------------------------------------------------------------
Purpose:     
Parameters:  <none>

Min kommentar: Sport - Håndterminal brukt ved oppstart av pilot Sport1

Entry:
1 - Ikke i bruk
2 - Ikke i bruk
3 - Ikke i bruk 
4 - ikke i bruk
5 - Ikke i bruk
6 - EAN kode
7 - Antall

Eksempel:
VAL,,,,E,4015268152497,00001,
VAL,,,,E,4015268152503,00001,

Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipAntLinjer AS INT  NO-UNDO.
  DEF INPUT  PARAMETER ipDataLinje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipFeilIFil  AS LOG  NO-UNDO.
  DEF OUTPUT PARAMETER ipSeqNr     AS INT  NO-UNDO.

  DEF VAR pcEAN AS CHAR NO-UNDO.

  CREATE HT-FilLinje.
  ASSIGN
    HT-FilLinje.HTFilId   = HT-FilHode.HTFilId
    HT-FilLinje.LinjeId   = ipAntLinjer
    HT-FilLinje.LinjeData = ipDataLinje
    HT-FilLinje.Ok        = ?
    ipSeqNr               = 0.
        
  ASSIGN
    pcEAN = entry(6,ipDataLinje)
    .
  /* plukker ut sekvensnummeret. */
  IF NUM-ENTRIES(ipDatalinje) = 9 THEN
      ipSeqNr = int(ENTRY(9,ipDataLinje)).

  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = pcEAN NO-ERROR.
  IF AVAILABLE Strekkode THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
  IF AVAILABLE Strekkode AND AVAILABLE ArtBas THEN
      ASSIGN
      ipVg       = ArtBas.Vg
      ipLopNr    = ArtBas.LopNr
      ipFeilIFil = FALSE 
      HT-Fillinje.OK = ? 
      .
  ELSE
      ASSIGN
          ipVg       = 0
          ipLopNr    = 0
          ipFeilIFil = TRUE
          HT-Fillinje.OK = FALSE
          .

  RELEASE Strekkode.
  RELEASE ArtBas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dummy C-Win 
PROCEDURE Dummy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Nullstiller error flag. */
  
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
  DISPLAY FILL-IN-SOK-STRING FILL-IN-SOK-DATE FILL-IN-SOK-INT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-52 RECT-53 BUTTON-Detalj BUTTON-Slett-2 B-NyeFiler B-Koblebutikk 
         BUTTON-KobleFra B-Annuller Btn_Help Btn_OK FILL-IN-SOK-STRING 
         FILL-IN-SOK-DATE FILL-IN-SOK-INT BUTTON-Sok BROWSE-HT-FilHode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Info 
      WITH FRAME FRAME-Progress IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Progress}
  FRAME FRAME-Progress:SENSITIVE = NO.
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FsgGen C-Win 
PROCEDURE FsgGen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipAntLinjer AS INT  NO-UNDO.
  DEF INPUT  PARAMETER ipDataLinje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipFeilIFil  AS LOG  NO-UNDO.

  CREATE HT-FilLinje.
  ASSIGN
    HT-FilLinje.HTFilId   = HT-FilHode.HTFilId
    HT-FilLinje.LinjeId   = ipAntLinjer
    HT-FilLinje.LinjeData = ipDataLinje
    HT-FilLinje.Ok        = ?.
        
  ASSIGN
    ipVg     = INT(ENTRY(1,ENTRY(1,HT-FilLinje.LinjeData,";"),"-"))
    ipLopNr  = INT(ENTRY(2,ENTRY(1,HT-FilLinje.LinjeData,";"),"-")) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      ASSIGN 
        ipFeilIFil      = TRUE
        HT-FilLinje.Ok = FALSE.
  ELSE IF NOT CAN-FIND(ArtBas WHERE
    ArtBas.Vg    = ipVg AND
    ArtBas.LopNr = ipLopNr) THEN
     ASSIGN 
       ipFeilIFil      = TRUE
       HT-FilLinje.Ok = FALSE.
  IF ipAntLinjer = 1 THEN
    HT-FilLinje.Ok = ?.               

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HTuppslag C-Win 
PROCEDURE HTuppslag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER lUppslag AS LOGICAL NO-UNDO.
  lUppslag = TRUE.
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
    .
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initProgram C-Win 
PROCEDURE initProgram :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleButikk C-Win 
PROCEDURE KobleButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSvar      AS LOG NO-UNDO.
  DEF VAR wLoop      AS INT NO-UNDO.
  DEF VAR wAntLinjer AS INT.
  DEF VAR wDataLinje AS CHAR.
  DEF VAR wTotLinjer AS INT NO-UNDO.
  DEF VAR wVg        AS INT NO-UNDO.
  DEF VAR wLopNr     AS INT NO-UNDO.
  DEF VAR wFeilIFil  AS LOG NO-UNDO.
  DEF VAR wOppdater  AS LOG NO-UNDO.
  DEF VAR wBkupDir   AS CHAR NO-UNDO.
  DEF VAR wFilensNavn AS CHAR NO-UNDO.
  DEF BUFFER bHT-FilHode FOR HT-FilHode.
  
  wOppdater = TRUE.

  DO WITH FRAME DEFAULT-FRAME: 
    IF wHtFilId = 0 THEN
    DO:
        /* Sjekker at det er minst en valgt butikk i listen */
        IF BROWSE-HT-FilHode:NUM-SELECTED-ROWS = 0 THEN
        DO:
            MESSAGE "Der er ingen filer valgt for oppdatering." SKIP
                    "Minst en fil må velges (markeres) før oppdatering mot telleliste kan starte."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "AVBRYT".
        END.

        wSvar = FALSE.
        MESSAGE "Skal oppdatering mot telleliste starte?" SKIP
                STRING(BROWSE-HT-FilHode:NUM-SELECTED-ROWS) + " file(r) valgt for oppdatering."
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
                UPDATE wSvar.
        IF wSvar = FALSE THEN 
          RETURN NO-APPLY "AVBRYT".
    END.
    ELSE
        wSvar = TRUE.

    FIX_OPP_LINJER:
    DO wLoop = 1 TO BROWSE-HT-FilHode:NUM-SELECTED-ROWS:
      wOk = BROWSE-HT-FilHode:FETCH-SELECTED-ROW(wLoop).
      /* Disse er lest inn i work base tidlingere. */
      IF NOT CAN-FIND(FIRST HT-FilLinje OF HT-FilHode WHERE
                      HT-FilLinje.OK = FALSE) THEN
        NEXT FIX_OPP_LINJER.

      /* Leser inn filen */
      RUN KobleButikk2 (HT-FilHode.HTFilId).

    END. /* FIX_OPP_LINJER TRANSACTION */
  END. 

  wOk = BROWSE-HT-FilHode:REFRESH( ).
  
  IF AVAILABLE HT-FilHode THEN
    RELEASE HT-FilHode.          
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleButikk2 C-Win 
PROCEDURE KobleButikk2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER iHTFilId AS INT NO-UNDO.
  
  DEF VAR wAntLinjer AS INT.
  DEF VAR wDataLinje AS CHAR.
  DEF VAR wTotLinjer AS INT NO-UNDO.
  DEF VAR wVg        AS INT NO-UNDO.
  DEF VAR wLopNr     AS INT NO-UNDO.
  DEF VAR wFeilIFil  AS LOG NO-UNDO.
  DEF VAR wSeqNr     AS INT NO-UNDO.
  DEF VAR wBkupDir   AS CHAR NO-UNDO.
  DEF VAR wFilensNavn AS CHAR NO-UNDO.
  DEF VAR cLinje     AS CHAR NO-UNDO.

  DEF BUFFER bHT-FilHode FOR HT-FilHode.
  
  FIND HT-FilHode NO-LOCK WHERE
      HT-FilHode.HTFilID = iHTFilID NO-ERROR.

  IF NOT AVAILABLE HT-FilHode THEN
      RETURN "AVBRYT".

  /* Kobler filen til butikk */
  IF HT-FilHode.Butik = 0 THEN DO TRANSACTION:
      /* Setter lås på filhode. */
      FIND CURRENT HT-FilHode EXCLUSIVE-LOCK.
        /* Setter butikknummer på posten. */
      ASSIGN
        HT-FilHode.Butik = wbutikk
        .          
  END. /* TRANSACTION */
  FIND CURRENT HT-FilHode NO-LOCK.        


  /* lESER INN OG OPPDATERER FILEN */
  IF (SEARCH(HT-FilHode.InnlestFra) <> ? AND NOT CAN-FIND(FIRST HT-FilLinje OF HT-FilHode)) THEN
  DO:  
      /* Teller opp antall linjer i datafilen. */
      IF HT-FilHode.AntLinjer = 0 THEN
        RUN TellLinjer (INPUT HT-FilHode.InnlestFra, OUTPUT wTotLinjer).  

      FI-Info = "Leser fra: " + HT-FilHode.InnlestFra.
      /*IF wTotLinjer > 10 THEN*/
      DO:
          VIEW FRAME FRAME-Progress.
          ASSIGN
            wTotLinjer                    = IF wTotLinjer < 11
                                              THEN 11
                                              ELSE wTotLinjer
            chCtrlFrame:ProgressBar:Min   = 1
            chCtrlFrame:ProgressBar:Max   = wTotLinjer
            chCtrlFrame:ProgressBar:Value = 1.
          DISPLAY FI-Info WITH FRAME FRAME-Progress.
      END.

      /* For sikkerhetsskyld, slettes allt someventuelt skulle være der fra før */
      FOR EACH HT-FilLinje OF HT-FilHode TRANSACTION:
        DELETE HT-FilLinje.
      END. /* TRANSACTION */ 
        
      /* Åpner stream fra fil og leser inn linjene. */
      ASSIGN
        wAntLinjer = 0
        wFeilIFil  = FALSE
        FILE-INFO:FILE-NAME = HT-FilHode.InnlestFra
        wFilensNavn = ENTRY(NUM-ENTRIES(FILE-INFO:FULL-PATHNAME,"\"),FILE-INFO:FULL-PATHNAME,"\")
        wBkupDir   = REPLACE(FILE-INFO:FULL-PATHNAME,wFilensNavn,"") + "bku"
        FILE-INFO:FILE-NAME = wBkupDir.
      IF FILE-INFO:FILE-TYPE = ? THEN
          OS-CREATE-DIR VALUE(wBkupDir).
      INPUT stream InnFraFil from value(HT-FilHode.InnlestFra).
      
      LES_FRA_FIL:
      REPEAT TRANSACTION:

        /* Importerer fra filen. */
        IF CAN-DO('6,7,8',string(HT-FilHode.TypeId)) THEN /* SymbPPT8800 */
        DO:
            CREATE tmpVaretran.
            IMPORT STREAM InnFraFil tmpVaretran.
            IF tmpVareTran.TransType <> 7 THEN
                NEXT LES_FRA_FIL.
        END.
        ELSE IF CAN-DO('10',string(HT-FilHode.TypeId)) THEN /* BxMobile */
        DO:
            IMPORT STREAM InnFraFil UNFORMATTED cLinje .
            IF NUM-ENTRIES(cLinje,';') >= 10 THEN
            DO:
                /* Sjekker butikk */
                ASSIGN iButikkNr = INT(ENTRY(10,cLinje,";")) NO-ERROR.
                IF ERROR-STATUS:ERROR OR NOT CAN-FIND(Butiker WHERE Butiker.Butik = iButikkNr) THEN
                    NEXT LES_FRA_FIL.
                FIND Butiker NO-LOCK WHERE Butiker.Butik = iButikkNr NO-ERROR.

                ASSIGN /* Dato og tid fra fil: '2012-11-4 12:28:00.000' */
                    pcEAN   = TRIM(ENTRY(5,cLinje,";"))
                    cTekst  = RIGHT-TRIM(LEFT-TRIM(TRIM(ENTRY(3,cLinje,";")),"'"),"'")
                    pcDato  = ENTRY(1,cTekst,' ')
                    pcTid   = ENTRY(2,cTekst,' ')
                    pcTid   = ENTRY(1,pcTid,'.')
                    .
            
                /* Legger på ledende nuller i EAN koden */
                IF LENGTH(pcEAN) > 6 AND length(pcEAN) < 13 THEN
                    pcEAN = FILL("0",13 - LENGTH(pcEAN)) + pcEAN.
                    .
                /* Kontroll av EAN kode */
                ASSIGN pfEAN = DEC(pcEAN) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT LES_FRA_FIL.            
                FIND Strekkode NO-LOCK WHERE Strekkode.Kode = pcEAN NO-ERROR.
                IF AVAILABLE Strekkode THEN 
                    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
                ELSE
                    NEXT LES_FRA_FIL.            
                IF AVAILABLE ArtBas THEN
                    cTekst = REPLACE(ArtBas.Beskr,' ','_').
                ELSE
                    NEXT LES_FRA_FIL. 
                FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
                IF NOT AVAILABLE StrKonv THEN 
                    NEXT LES_FRA_FIL. 
                    
                /* Trekker ut dato */
                ASSIGN
                    pdDato = DATE(INT(ENTRY(2,pcDato,'-')),
                                  INT(ENTRY(3,pcDato,'-')),
                                  INT(ENTRY(1,pcDato,'-'))) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT LES_FRA_FIL.            
                /* Trekker ut tid 12:28:00 */
                ASSIGN
                    piTid = (INT(SUBSTRING(pcTid,1,2)) * 60 * 60) + 
                            (INT(SUBSTRING(pcTid,4,2)) * 60) +
                             INT(SUBSTRING(pcTid,7,2)) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT LES_FRA_FIL.            

                CREATE tmpVaretran.
                ASSIGN
                    tmpVareTran.ButikkNr   = iButikkNr  
                    tmpVareTran.Strekkode  = pcEAN  
                    tmpVareTran.Dato       = pdDato  
                    tmpVareTran.tid        = piTid  
                    tmpVareTran.LoggNr     = 0  
                    tmpVareTran.TransType  = 7
                    tmpVareTran.Transtekst = cTekst
                    tmpVareTran.BrukerId   = TRIM(ENTRY(9,cLinje,';'))  
                    tmpVareTran.Antall     = DEC(ENTRY(2,cLinje,';'))  
                    tmpVareTran.Kostpris   = 0
                    tmpVareTran.Salgssum   = 0
                    tmpVareTran.NyLagAnt   = 0
                    tmpVareTran.GmlLagAnt  = 0
                    tmpVareTran.Varetekst  = cTekst
                    tmpVareTran.Storl      = StrKonv.Storl  
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT LES_FRA_FIL.
            END.
            ELSE NEXT LES_FRA_FIL.
        END.
        ELSE IF CAN-DO('12',string(HT-FilHode.TypeId)) THEN  
        DO:
            IMPORT STREAM InnFraFil UNFORMATTED cLinje .
            IF NUM-ENTRIES(cLinje,';') >= 16 THEN
            DO:
                CREATE tmpVaretran.
                ASSIGN
                    tmpVareTran.ButikkNr   = INT(ENTRY(2,cLinje,';'))  
                    tmpVareTran.Strekkode  = ENTRY(3,cLinje,';')  
                    tmpVareTran.Dato       = DATE(ENTRY(4,cLinje,';'))  
                    tmpVareTran.tid        = INT(ENTRY(5,cLinje,';'))  
                    tmpVareTran.LoggNr     = INT(ENTRY(6,cLinje,';'))  
                    tmpVareTran.TransType  = INT(ENTRY(7,cLinje,';'))  
                    tmpVareTran.Transtekst = TRIM(ENTRY(8,cLinje,';'))  
                    tmpVareTran.BrukerId   = TRIM(ENTRY(9,cLinje,';'))  
                    tmpVareTran.Antall     = DEC(ENTRY(10,cLinje,';'))  
                    tmpVareTran.Kostpris   = DEC(ENTRY(11,cLinje,';'))  
                    tmpVareTran.Salgssum   = DEC(ENTRY(12,cLinje,';'))  
                    tmpVareTran.NyLagAnt   = DEC(ENTRY(13,cLinje,';'))  
                    tmpVareTran.GmlLagAnt  = DEC(ENTRY(14,cLinje,';'))  
                    tmpVareTran.Varetekst  = TRIM(ENTRY(15,cLinje,';'),'"')  
                    tmpVareTran.Storl      = TRIM(ENTRY(16,cLinje,';'),'"')  
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT LES_FRA_FIL.
            END.
            ELSE NEXT LES_FRA_FIL.
        END.
        ELSE /* Alle andre */
            IMPORT STREAM InnFraFil UNFORMATTED wDataLinje.
        ASSIGN
          wAntLinjer = wAntLinjer + 1.
        
        IF wAntLinjer >= 10 AND (wAntLinjer MODULO 10 = 0) THEN
          chCtrlFrame:ProgressBar:Value = wAntLinjer.
        /* Legger data inn i HT-FilLinje. */
        CASE HT-FilHode.TypeId:
          WHEN 1 THEN 
            DO:
              IF NOT wDataLinje BEGINS wFilPrefix THEN
                NEXT LES_FRA_FIL.

              RUN DensoBHT5000GEN2 (wAntLinjer, 
                                    wDataLinje, 
                                    OUTPUT wVg,
                                    OUTPUT wLopNr,
                                    OUTPUT wFeilIFil).
            END.
          WHEN 2 THEN RUN Denso2000v1-0 (wAntLinjer, 
                                         wDataLinje, 
                                         OUTPUT wVg,
                                         OUTPUT wLopNr,
                                         OUTPUT wFeilIFil).
          WHEN 3 THEN RUN Memor128v1-0 (wAntLinjer, 
                                        wDataLinje, 
                                        OUTPUT wVg,
                                        OUTPUT wLopNr,
                                        OUTPUT wFeilIFil).
          WHEN 4 THEN RUN Memor128-Ean13 (wAntLinjer, 
                                        wDataLinje, 
                                        OUTPUT wVg,
                                        OUTPUT wLopNr,
                                        OUTPUT wFeilIFil).
          WHEN 5 THEN RUN DT900 (wAntLinjer, 
                                 wDataLinje, 
                                 OUTPUT wVg,
                                 OUTPUT wLopNr,
                                 OUTPUT wFeilIFil,
                                 OUTPUT wSeqNr).
          WHEN 6 THEN RUN Varetran (wAntLinjer, 
                                 wDataLinje, 
                                 OUTPUT wArtikkelNr,
                                 OUTPUT wFeilIFil).
          WHEN 7 THEN RUN Varetran (wAntLinjer, 
                                 wDataLinje, 
                                 OUTPUT wArtikkelNr,
                                 OUTPUT wFeilIFil).
          WHEN 8 THEN RUN Varetran (wAntLinjer, 
                                 wDataLinje, 
                                 OUTPUT wArtikkelNr,
                                 OUTPUT wFeilIFil).
          WHEN 9 THEN DO: /* Anpassning SkoDej */
              /* Den första posten innehåller labler. */
              IF wDataLinje BEGINS "VG" THEN
                  NEXT LES_FRA_FIL.
              RUN FsgGen (wAntLinjer, 
                          wDataLinje, 
                          OUTPUT wVg,
                          OUTPUT wLopNr,
                          OUTPUT wFeilIFil).
          END.
          WHEN 10 THEN RUN Varetran (wAntLinjer, 
                                 wDataLinje, 
                                 OUTPUT wArtikkelNr,
                                 OUTPUT wFeilIFil).      
          WHEN 12 THEN RUN Varetran (wAntLinjer, 
                                 wDataLinje, 
                                 OUTPUT wArtikkelNr,
                                 OUTPUT wFeilIFil).
          OTHERWISE
            MESSAGE "Det finnes ingen innlesningsrutine rutine for import av data for HT-Type " wTypeId
                    VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".      
        END CASE. 
        
      END. /* LES_FRA_FIL TRANSACTION */

      INPUT stream InnFraFil close.
      OS-RENAME VALUE(HT-FilHode.InnlestFra) 
          VALUE(wBkupDir + "\" + ENTRY(1,wFilensNavn,".") + STRING(TODAY,"99999999") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + "." + ENTRY(2,wFilensNavn,".")).
      /*
      IF SEARCH(wBkupDir + "\" + ENTRY(1,wFilensNavn,".") + STRING(TODAY,"99999999") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + "." + ENTRY(2,wFilensNavn,".")) <> ? THEN
          OS-DELETE VALUE(HT-FilHode.InnlestFra).
      */

      HIDE FRAME FRAME-Progress NO-PAUSE.

      DO TRANSACTION:
        FIND bHT-FilHode EXCLUSIVE-LOCK WHERE
          bHT-FilHode.HTFilId = HT-FilHode.HTFilId NO-ERROR.
        IF AVAILABLE bHT-FilHode THEN
          DO:
            ASSIGN
              bHT-FilHode.FeilIFil  = wFeilIFil
              bHT-FilHode.AntLinjer = wAntLinjer.
            RELEASE bHT-FilHode.
          END.
      END. /* TRANSACTION */

  END. 

  IF AVAILABLE HT-FilHode THEN
    RELEASE HT-FilHode.          
    
  RUN OppdaterTelleListe2 (iHTFilId).
    
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
   .
   RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Memor128-Ean13 C-Win 
PROCEDURE Memor128-Ean13 :
/*------------------------------------------------------------------------------
Purpose:     
Parameters:  <none>

Min kommentar: Memor 128 v. 1.0

1-1  : Postid 
2-4  : VG
5-8  : Löpnr
9-9  : Prisgrupp
10-13: Storlek
14-16: Antal 3 siffror vänsterjusterat spaceutfyllt
17-26: Datum/tid ÅÅMMDDHHMM

12345678901234567890123456789

Här börjar filen:
--------------------------
00183063000101  9604161018
00181040004501  9604161019
00183063000101  9604161019
00183063000101  9604161019

Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipAntLinjer AS INT  NO-UNDO.
  DEF INPUT  PARAMETER ipDataLinje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipFeilIFil  AS LOG  NO-UNDO.

  CREATE HT-FilLinje.
  ASSIGN
    HT-FilLinje.HTFilId   = HT-FilHode.HTFilId
    HT-FilLinje.LinjeId   = ipAntLinjer
    HT-FilLinje.LinjeData = ipDataLinje
    HT-FilLinje.Ok        = ?.
        
  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,2,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,5,4)).
  IF NOT CAN-FIND(ArtBas WHERE
    ArtBas.Vg    = ipVg AND
    ArtBas.LopNr = ipLopNr) THEN
     ASSIGN 
       ipFeilIFil      = TRUE
       HT-FilLinje.Ok = FALSE.
  IF ipAntLinjer = 1 THEN
    HT-FilLinje.Ok = ?.               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Memor128v1-0 C-Win 
PROCEDURE Memor128v1-0 :
/*------------------------------------------------------------------------------
Purpose:     
Parameters:  <none>

Min kommentar: Memor 128 v. 1.0

1-1  : Postid 
2-4  : VG
5-8  : Löpnr
9-9  : Prisgrupp
10-13: Storlek
14-16: Antal 3 siffror vänsterjusterat spaceutfyllt
17-26: Datum/tid ÅÅMMDDHHMM

12345678901234567890123456789

Här börjar filen:
--------------------------
00183063000101  9604161018
00181040004501  9604161019
00183063000101  9604161019
00183063000101  9604161019

Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipAntLinjer AS INT  NO-UNDO.
  DEF INPUT  PARAMETER ipDataLinje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER ipVg        AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipLopNr     AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER ipFeilIFil  AS LOG  NO-UNDO.

  CREATE HT-FilLinje.
  ASSIGN
    HT-FilLinje.HTFilId   = HT-FilHode.HTFilId
    HT-FilLinje.LinjeId   = ipAntLinjer
    HT-FilLinje.LinjeData = ipDataLinje
    HT-FilLinje.Ok        = ?.
        
  ASSIGN
    ipVg     = int(substring(HT-FilLinje.LinjeData,2,3))
    ipLopNr  = int(substring(HT-FilLinje.LinjeData,5,4)).
  IF NOT CAN-FIND(ArtBas WHERE
    ArtBas.Vg    = ipVg AND
    ArtBas.LopNr = ipLopNr) THEN
     ASSIGN 
       ipFeilIFil      = TRUE
       HT-FilLinje.Ok = FALSE.
  IF ipAntLinjer = 1 THEN
    HT-FilLinje.Ok = ?.               
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterTelleListe C-Win 
PROCEDURE OppdaterTelleListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBekreft AS INT NO-UNDO.

  MESSAGE 
  program-name(1) 
  SKIP 'OppdaterTelleListe' SKIP
      "Denne koden er koblet ut. Kontakt systemansvarlig "
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
    
  /*
  def var wLoop      as int.
  def var wAntLinjer as int  no-undo.  
  def var wVg        as int  no-undo.
  def var wLopNr     as int  no-undo.
  def var wStorl     as char no-undo.
  def var wAntall    as int  no-undo.
  def var wVVareKost as dec  no-undo.
  DEF VAR wEAN       AS CHAR NO-UNDO.
  
  do with frame DEFAULT-FRAME:
  
  if BROWSE-HT-FilHode:NUM-SELECTED-ROWS = 0 then
    do:
      message "Det er ingen merkede filer å oppdatere!"
              view-as alert-box message title "Melding".
      return no-apply.
    end.
  
  if wBekreft = 1 then
    do:
      message "Skal oppdatering av HT-Fil(er) mot telleliste starte?"
              view-as alert-box buttons yes-no title "Bekreftelse" 
      update wOk.
      if wOk = false then
        return no-apply.
    end.
  else
    wOk = true.
  
  {sww.i}
  OPPDAT_LOOP:
  do wLoop = BROWSE-HT-FilHode:NUM-SELECTED-ROWS to 1 by -1:
    wOk = BROWSE-HT-FilHode:FETCH-SELECTED-ROW(wLoop).

    /* Jo - Dette skal kunne gjøres. Men kun på ikke oppdaterte poster.
    /* er postene oppdatert, skal de ikke oppdateres */
    if HT-FilHode.Oppdatert <> ? then
      next OPPDAT_LOOP.
    */
      
    /* er postene ikke koblet mot butikk de ikke oppdateres. */
    if HT-FilHode.Butik = 0 then
      next OPPDAT_LOOP.    

    assign
      wAntLinjer = 0
      FI-Info    = "Oppdaterer fil: " + HT-FilHode.InnlestFra.
    assign
      chCtrlFrame:ProgressBar:Min   = 1
      chCtrlFrame:ProgressBar:Max   = HT-FilHode.AntLinjer
      chCtrlFrame:ProgressBar:Value = 1 NO-ERROR.
    view frame FRAME-Progress.
    display FI-Info with frame FRAME-Progress.

    RUN dummy.

    /* Oppdaterer filen. */
    TRANS_BLOKK:
    do:
      if available {&br-tabell} then
        do:
          LES_LINJE:
          for each HT-FilLinje of HT-FilHode exclusive-lock where
            HT-FilLinje.Ok <> true TRANSACTION:

            assign
              wEAN       = ""
              wAntLinjer = wAntLinjer + 1.
        
            if wAntLinjer modulo 10 = 0 then
              chCtrlFrame:ProgressBar:Value = wAntLinjer.
              
            IF AVAILABLE ArtBas THEN
                RELEASE ArtBAs.

            /* Legger data inn i HT-FilLinje. */
            case HT-FilHode.TypeId:
              when 1 then  /* DensoBHT5000GEN2 */
                do:
                  /* De to første postene er dato og butikk. De skal ikke leses. */
                  if not HT-FilLinje.LinjeData begins wFilPrefix then
                    do:
                      HT-FilLinje.Ok = true.
                      next LES_Linje.
                    end.
                  run 2DensoBHT5000GEN2 (HT-FilLinje.LinjeData, 
                                         output wVg,
                                         output wLopNr,
                                         output wStorl,
                                         output wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
                end.
              when 2 then 
              DO:
                  run 2Denso2000v1-0 (HT-FilLinje.LinjeData, 
                                              output wVg,
                                              output wLopNr,
                                              output wStorl,
                                              output wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
              END.
              when 3 then 
              DO:
                  run 2Memor128v1-0 (HT-FilLinje.LinjeData, 
                                             output wVg,
                                             output wLopNr,
                                             output wStorl,
                                             output wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
              END.
              when 4 then 
              DO:
                  run 2Memor128-Ean (HT-FilLinje.LinjeData, 
                                             output wVg,
                                             output wLopNr,
                                             output wStorl,
                                             output wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
              END.
              when 5 then run 2DT900 (HT-FilLinje.LinjeData, 
                                      output wVg,
                                      output wLopNr,
                                      output wStorl,
                                      output wAntall,
                                      OUTPUT wEAN).
            end case. 
            
            find ArtBas no-lock where
              ArtBas.Vg    = wVg and
              ArtBas.LopNr = wLopNr no-error.
            if not available ArtBas then
              do:
                HT-FilLinje.Ok = false.
                next LES_LINJE.
              end.

            /* Henter TelleLinjen om den finnes. */
            find TelleLinje exclusive-lock where
              TelleLinje.TelleNr    = wTelleNr and
              TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr and
              TelleLinje.Butik      = HT-FilHode.Butik and
              TelleLinje.Storl      = wStorl no-error.            
                            
            find ArtPris no-lock where
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
              ArtPris.ProfilNr   = Butiker.ProfilNr no-error.

            /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
            if not available ArtPris then
              do:
                find ArtPris no-lock where
                  ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
                  ArtPris.ProfilNr   = clButiker.ProfilNr no-error.
              end.

            /* Skal det oppdateres som varesalg, må pris hentes. */
            if TelleHode.TTId = 1 then
              do:
                find Butiker no-lock where
                  Butiker.Butik = HT-FilHode.Butik.
          
                if available ArtPris then
                  wVVAreKost = ArtPris.Pris[if ArtPris.Tilbud then 2 else 1].
              end.
              
            RUN dummy.

            /* Finnes ikke linjen, legges den opp. */
            if not available TelleLinje then
              NY-LINJE:
              do:
                /* Henter lagerkost */
                find Lager of ArtBas no-lock where
                     Lager.Butik = HT-FilHode.Butik no-error.
                if TelleHode.TTId <> 1 then
                  do:
                    if available Lager then
                      wVVareKost = Lager.VVareKost.
                    else 
                      wVVareKost = 0.
                  end.
                /* Ukjent varekost ? */
                IF wVVarekost = 0 OR ArtBas.Lager = FALSE THEN
                DO:
                    find Butiker no-lock where
                      Butiker.Butik = HT-FilHode.Butik.

                    find ArtPris no-lock where
                      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
                      ArtPris.ProfilNr   = Butiker.ProfilNr no-error.

                    /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
                    if not available ArtPris then
                      do:
                        find ArtPris no-lock where
                          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
                          ArtPris.ProfilNr   = clButiker.ProfilNr no-error.
                      end.
                    if available ArtPris then
                      wVVAreKost = ArtPris.Varekost[if ArtPris.Tilbud then 2 else 1].
                END.

                create TelleLinje.
                assign
                  TelleLinje.TelleNr    = TelleHode.TelleNr 
                  TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
                  Tellelinje.Beskr      = ArtBas.Beskr
                  TelleLinje.Butik      = HT-FilHode.Butik
                  TelleLinje.Storl      = wStorl
                  TelleLinje.VVareKost  = wVVAreKost
                  TelleLinje.Vg         = wVg
                  TelleLinje.LopNr      = wLopNr
                  TelleLinje.Kode       = wEAN
                  .

                find ArtLag no-lock where
                  ArtLag.Butik = TelleLinje.Butik and
                  ArtLag.Vg    = TelleLinje.Vg and
                  ArtLag.LopNr = TelleLinje.LopNr and
                  ArtLag.Storl = TelleLinje.Storl no-error.
                if available ArtLag then
                  assign
                    TelleLinje.AntallPar = ArtLag.LagAnt
                    TelleLinje.OpprVerdi = ArtLag.LagAnt * wVVareKost.

              end. /* NY-LINJE */
  

            /* Øvrig informasjon. */
            assign
              TelleLinje.LevKod     = ArtBas.LevKod
              TelleLinje.NedSkrevet = TelleLinje.VVareKost
              TelleLinje.AntallTalt = TelleLinje.AntallTalt + wAntall
              TelleLinje.AntallDiff = TelleLinje.AntallPAr  - TelleLinje.AntallTalt
              TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
              TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
              TelleLinje.LevNr      = ArtBas.LevNr
              TelleLinje.Sasong     = ArtBas.SaSong
              TelleLinje.Farg       = ArtBas.Farg
              TelleLinje.MatKod     = ArtBas.MatKod
              TelleLinje.VgLopNr    = trim(string(ArtBas.Vg,">>>>>9")) + "/" + trim(string(ArtBas.LopNr,">>>>>9")).
    
            if not can-find(first KonvReg where
              KonvReg.EDB-System = wEDB-System and
              KonvReg.Tabell     = wTabell     and
              KonvReg.EkstId     = string(ArtBas.ArtikkelNr) + "," + 
                                   string(HT-FilHode.Butik) and 
              KonvReg.InterntId  = string(ArtBas.ArtikkelNr) + "," + 
                                   string(HT-FilHode.Butik)) then
            do:
              create KonvReg.
              assign
                KonvReg.EDB-System = wEDB-System 
                KonvReg.Tabell     = wTabell     
                KonvReg.EkstId     = string(ArtBas.ArtikkelNr) + "," + 
                                     string(HT-FilHode.Butik)
                KonvReg.InterntId  = string(ArtBas.ArtikkelNr) + "," + 
                                     string(HT-FilHode.Butik)
                KonvReg.DivTekst   = string(TelleHode.TelleNr).
            end. 

            /* Flagger linjen som oppdatert */
            assign
              HT-FilLinje.Ok = true.
            RELEASE HT-Fillinje.
          end. /* LES_LINJE TRANSACTION*/
          
          do TRANSACTION:
            find current {&br-tabell} exclusive-lock.
          
            assign
              HT-FilHode.Oppdatert    = today
              HT-FilHode.OppdatertTid = time
              HT-FilHode.OppdatertAv  = userid("dictdb")
              HT-FilHode.AnnulertDato = ?
              HT-FilHode.AnnulertAv   = "".
          end. /* TRANSACTION */
          find current {&br-tabell} no-lock.
        end.
    end. /* TRANS_BLOKK */

    wOk = BROWSE-HT-FilHode:DESELECT-SELECTED-ROW(wLoop).
  end. /* OPPDAT_LOOP */  
  hide frame FRAME-Progress no-pause.
  {swn.i}
  
  wOk = BROWSE-HT-FilHode:REFRESH( ).
  
  end.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterTelleliste2 C-Win 
PROCEDURE OppdaterTelleliste2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piHTFilId AS INT NO-UNDO.
  
  DEF VAR wAntLinjer  AS INT  NO-UNDO.  
  DEF VAR wArtikkelNr AS DEC NO-UNDO.
  DEF VAR wVg         AS INT  NO-UNDO.
  DEF VAR wLopNr      AS INT  NO-UNDO.
  DEF VAR wStorl      AS CHAR NO-UNDO.
  DEF VAR wAntall     AS INT  NO-UNDO.
  DEF VAR wVVareKost  AS DEC  NO-UNDO.
  DEF VAR wEAN        AS CHAR NO-UNDO.
  DEF VAR wSeqNr      AS INT  NO-UNDO.
  DEF VAR wPrisFsGen  AS DEC  NO-UNDO.


  FIND HT-FilHode NO-LOCK WHERE
      HT-FilHode.HTFilId = piHTFilId NO-ERROR.
  IF NOT AVAILABLE HT-FilHode THEN
      RETURN "AVBRYT".
  /* er postene ikke koblet mot butikk de ikke oppdateres. */
  IF HT-FilHode.Butik = 0 THEN
    RETURN "AVBRYT".    
  

  DO WITH FRAME DEFAULT-FRAME:
  
  {sww.i}
  OPPDAT_LOOP:
  DO:
    ASSIGN
      wAntLinjer = 0
      FI-Info    = "Oppdaterer fil: " + HT-FilHode.InnlestFra.
    ASSIGN
      chCtrlFrame:ProgressBar:Min   = 1
      chCtrlFrame:ProgressBar:Max   = HT-FilHode.AntLinjer
      chCtrlFrame:ProgressBar:Value = 1 NO-ERROR.
    VIEW FRAME FRAME-Progress.
    DISPLAY FI-Info WITH FRAME FRAME-Progress.

    RUN dummy.

    /* Oppdaterer filen. */
    TRANS_BLOKK:
    DO:
      IF AVAILABLE {&br-tabell} THEN
        DO:
          LES_LINJE:
          FOR EACH HT-FilLinje OF HT-FilHode EXCLUSIVE-LOCK WHERE
            HT-FilLinje.Ok <> TRUE TRANSACTION:

            ASSIGN
              wEAN       = ""
              wAntLinjer = wAntLinjer + 1.
        
            IF wAntLinjer MODULO 10 = 0 THEN
              chCtrlFrame:ProgressBar:Value = wAntLinjer NO-ERROR.
              
            IF AVAILABLE ArtBas THEN
                RELEASE ArtBAs.

            /* Legger data inn i HT-FilLinje. */
            CASE HT-FilHode.TypeId:
              WHEN 1 THEN  /* DensoBHT5000GEN2 */
                DO:
                  /* De to første postene er dato og butikk. De skal ikke leses. */
                  IF NOT HT-FilLinje.LinjeData BEGINS wFilPrefix THEN
                    DO:
                      HT-FilLinje.Ok = TRUE.
                      NEXT LES_Linje.
                    END.
                  RUN 2DensoBHT5000GEN2 (HT-FilLinje.LinjeData, 
                                         OUTPUT wVg,
                                         OUTPUT wLopNr,
                                         OUTPUT wStorl,
                                         OUTPUT wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
                END.
              WHEN 2 THEN 
              DO:
                  RUN 2Denso2000v1-0 (HT-FilLinje.LinjeData, 
                                              OUTPUT wVg,
                                              OUTPUT wLopNr,
                                              OUTPUT wStorl,
                                              OUTPUT wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
              END.
              WHEN 3 THEN 
              DO:
                  RUN 2Memor128v1-0 (HT-FilLinje.LinjeData, 
                                             OUTPUT wVg,
                                             OUTPUT wLopNr,
                                             OUTPUT wStorl,
                                             OUTPUT wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
              END.
              WHEN 4 THEN 
              DO:
                  RUN 2Memor128-Ean (HT-FilLinje.LinjeData, 
                                             OUTPUT wVg,
                                             OUTPUT wLopNr,
                                             OUTPUT wStorl,
                                             OUTPUT wAntall).
                  /* Prepper størrelsen og konverterer den. */
                  wStorl = string(dec(wStorl) / 10).
                  wStorl = FiksStorl(wStorl). 
              END.
              WHEN 5 THEN RUN 2DT900 (HT-FilLinje.LinjeData, 
                                      OUTPUT wVg,
                                      OUTPUT wLopNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN,
                                      OUTPUT wSeqNr).
              WHEN 6 THEN RUN 2Varetran (HT-FilLinje.LinjeData, /* SymbPPT8800 */
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
              WHEN 7 THEN RUN 2Varetran (HT-FilLinje.LinjeData, 
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
              WHEN 8 THEN RUN 2Varetran (HT-FilLinje.LinjeData, 
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
              WHEN 9 THEN DO: /* Anpassning SkoDej */
                  /* Den första posten innehåller lablar. */
                  IF HT-FilLinje.LinjeData BEGINS "VG" THEN
                    DO:
                      HT-FilLinje.Ok = TRUE.
                      NEXT LES_Linje.
                    END.
                  wPrisFsGen = 0.
                  RUN 2FsgGen (HT-FilLinje.LinjeData, 
                                               OUTPUT wVg,
                                               OUTPUT wLopNr,
                                               OUTPUT wStorl,
                                               OUTPUT wAntall,
                                               OUTPUT wPrisFsGen).
              END.
              WHEN 10 THEN RUN 2Varetran (HT-FilLinje.LinjeData, /* BxMobile */
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
              WHEN 12 THEN RUN 2Varetran (HT-FilLinje.LinjeData, /* SymbPPT8800 */
                                      OUTPUT wArtikkelNr,
                                      OUTPUT wStorl,
                                      OUTPUT wAntall,
                                      OUTPUT wEAN).
            END CASE. 
            
            /* Sikrer at størrelsen er formatert riktig. */
            DYNAMIC-FUNCTION("runproc","bibl_fixstorl.p",wStorl,?).
            wStorl = caps(DYNAMIC-FUNCTION("getTransactionMessage")).

            IF AVAILABLE ArtBas THEN
                RELEASE ArtBas.
            IF wArtikkelNr > 0 THEN
                FIND ArtBas NO-LOCK WHERE
                Artbas.ArtikkelNr = wArtikkelNr NO-ERROR.
            IF NOT AVAILABLE ArtBas AND wVg > 0 AND wLopNr <> 0 AND wLopNr <> ? THEN
                FIND ArtBas NO-LOCK WHERE
                ArtBas.Vg    = wVg AND
                ArtBas.LopNr = wLopNr NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN
              DO:
                HT-FilLinje.Ok = FALSE.
                NEXT LES_LINJE.
              END.
            ELSE
                ASSIGN
                    wVg    = ArtBas.Vg
                    wLopNr = ArtBAs.LopNr.
            FIND Farg OF ArtBas NO-LOCK NO-ERROR.

            /* Henter TelleLinjen om den finnes. */
            FIND TelleLinje EXCLUSIVE-LOCK WHERE
              TelleLinje.TelleNr    = wTelleNr AND
              TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
              TelleLinje.Butik      = HT-FilHode.Butik AND
              TelleLinje.Storl      = wStorl
              NO-ERROR.            
                            
            FIND ArtPris NO-LOCK WHERE
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
              ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

            /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
            IF NOT AVAILABLE ArtPris THEN
              DO:
                FIND ArtPris NO-LOCK WHERE
                  ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                  ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
              END.

            /* Skal det oppdateres som varesalg, må pris hentes. */
            IF TelleHode.TTId = 1 THEN
              DO:
                FIND Butiker NO-LOCK WHERE
                  Butiker.Butik = HT-FilHode.Butik.
          
                IF AVAILABLE ArtPris THEN
                  wVVAreKost = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1].
              END.
              
            RUN dummy.

            /* Finnes ikke linjen, legges den opp. */
            IF NOT AVAILABLE TelleLinje THEN
              NY-LINJE:
              DO:
                /* Henter lagerkost */
                FIND Lager OF ArtBas NO-LOCK WHERE
                     Lager.Butik = HT-FilHode.Butik NO-ERROR.
                /*if TelleHode.TTId <> 1 THEN*/
                  DO:
                    IF AVAILABLE Lager THEN
                      wVVareKost = Lager.VVareKost.
                    ELSE 
                      wVVareKost = 0.
                  END.
                /* Ukjent varekost ? */
                IF wVVarekost = 0 OR wVVarekost = ? OR ArtBas.Lager = FALSE OR TelleHode.TTId = 1 THEN
                DO:
                    FIND Butiker NO-LOCK WHERE
                      Butiker.Butik = HT-FilHode.Butik.

                    FIND Lager NO-LOCK WHERE
                      Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                      Lager.Butik      = Butiker.Butik NO-ERROR.
                    IF AVAILABLE Lager THEN DO:
                      wVVareKost = Lager.VVarekost.
                    END.
                    IF wVVareKost < 0 OR wVVareKost = ? THEN 
                      wVVareKost = 0. 

                    FIND ArtPris NO-LOCK WHERE
                      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                      ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

                    /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
                    IF NOT AVAILABLE ArtPris THEN
                      DO:
                        FIND ArtPris NO-LOCK WHERE
                          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                      END.
                    /* Er det feil på varekost, skal den hentes fra kalkylen */
                    IF (wVVareKost = 0 OR TelleHode.TTId = 1) AND AVAILABLE ArtPris THEN
                      wVVAreKost = IF TelleHode.TTId = 1
                                     THEN ArtPris.Pris[1]
                                     ELSE ArtPris.Varekost[1].
                END.

                /* for JF - pris lest ut fra håndterminalen. */
                IF TelleHode.TTID = 1 AND wPrisFsGen > 0 THEN
                    wVvareKost = wPrisFsGen.

                CREATE TelleLinje.
                ASSIGN
                  TelleLinje.TelleNr    = TelleHode.TelleNr 
                  TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
                  TelleLinje.Butik      = HT-FilHode.Butik
                  TelleLinje.Storl      = wStorl
                  TelleLinje.VVareKost  = wVVAreKost
                  TelleLinje.Vg         = wVg
                  TelleLinje.LopNr      = wLopNr
                  TelleLinje.Kode       = wEAN
                  Tellelinje.Beskr      = ArtBas.Beskr
                  TelleLinje.SeqNr      = wSeqNr
                  .

                FIND ArtLag NO-LOCK WHERE
                  ArtLag.artikkelnr = TelleLinje.Artikkelnr AND
                  ArtLag.Butik = TelleLinje.Butik AND
                  ArtLag.Storl = TelleLinje.Storl NO-ERROR.
                IF AVAILABLE ArtLag THEN
                  ASSIGN
                    TelleLinje.AntallPar = ArtLag.LagAnt
                    TelleLinje.OpprVerdi = ArtLag.LagAnt * wVVareKost.

              END. /* NY-LINJE */
  
            /* Logger strekkoder */
            IF NOT CAN-DO(TelleLinje.Merknad,wEAN) THEN
                TelleLinje.Merknad = TelleLinje.Merknad + ',' + wEAN.
            TelleLinje.Merknad = LEFT-TRIM(TelleLinje.Merknad,',').

            /* Øvrig informasjon. */
            ASSIGN
              TelleLinje.LevKod        = ArtBas.LevKod
              TelleLinje.LevFargKod    = IF ArtBas.LevFargKod = ""
                                           THEN ArtBas.LevFargKod
                                         ELSE IF AVAILABLE Farg
                                           THEN Farg.FarBeskr
                                         ELSE ""
              TelleLinje.NedSkrevet    = TelleLinje.VVareKost
              TelleLinje.AntallTalt    = TelleLinje.AntallTalt + wAntall
              Tellelinje.OpprAntalTalt = TelleLinje.OpprAntalTalt + wAntall
              TelleLinje.AntallDiff    = TelleLinje.AntallPAr  - TelleLinje.AntallTalt
              TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost
              TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * TelleLinje.VVareKost
              TelleLinje.LevNr         = ArtBas.LevNr
              TelleLinje.Sasong        = ArtBas.SaSong
              TelleLinje.Farg          = ArtBas.Farg
              TelleLinje.MatKod        = ArtBas.MatKod
              TelleLinje.VgLopNr       = trim(string(ArtBas.Vg,">>>>>9")) + "/" + trim(string(ArtBas.LopNr,">>>>>9")).
    
            IF NOT CAN-FIND(FIRST KonvReg WHERE
              KonvReg.EDB-System = wEDB-System AND
              KonvReg.Tabell     = wTabell     AND
              KonvReg.EkstId     = string(ArtBas.ArtikkelNr) + "," + 
                                   string(HT-FilHode.Butik) AND 
              KonvReg.InterntId  = string(ArtBas.ArtikkelNr) + "," + 
                                   string(HT-FilHode.Butik)) THEN
            DO:
              CREATE KonvReg.
              ASSIGN
                KonvReg.EDB-System = wEDB-System 
                KonvReg.Tabell     = wTabell     
                KonvReg.EkstId     = string(ArtBas.ArtikkelNr) + "," + 
                                     string(HT-FilHode.Butik)
                KonvReg.InterntId  = string(ArtBas.ArtikkelNr) + "," + 
                                     string(HT-FilHode.Butik)
                KonvReg.DivTekst   = string(TelleHode.TelleNr).
            END. 

            /* Flagger linjen som oppdatert */
            ASSIGN
              HT-FilLinje.Ok = TRUE.
            RELEASE HT-Fillinje.
          END. /* LES_LINJE TRANSACTION*/
          
          DO TRANSACTION:
            FIND CURRENT {&br-tabell} EXCLUSIVE-LOCK.
          
            ASSIGN
              HT-FilHode.Oppdatert    = TODAY
              HT-FilHode.OppdatertTid = TIME
              HT-FilHode.OppdatertAv  = userid("dictdb")
              HT-FilHode.AnnulertDato = ?
              HT-FilHode.AnnulertAv   = "".
          END. /* TRANSACTION */
          FIND CURRENT {&br-tabell} NO-LOCK.
        END.
    END. /* TRANS_BLOKK */
  END. /* OPPDAT_LOOP */  
  HIDE FRAME FRAME-Progress NO-PAUSE.
  {swn.i}
  
  END.
  
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
        REPOSITION {&BROWSE-NAME} TO ROWID rowid(b{&br-tabell}) NO-ERROR.
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
    wBrowseRow = {&BROWSE-NAME}:FOCUSED-ROW IN FRAME {&FRAME-NAME}.
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = wRowId NO-LOCK.
    RUN BytSortering (wNyCol).
    RUN SD-Reposition.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortIkkeKobledeFiler C-Win 
PROCEDURE TaBortIkkeKobledeFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Tar bort alle ikke koblede filer fra tellingen. */
  DO TRANSACTION:
  FOR EACH HT-FilHode WHERE
    HT-FilHode.TelleNr = wTelleNr AND
    HT-FilHode.Butik   = 0:

    DELETE HT-FilHode.
  END.
  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellLinjer C-Win 
PROCEDURE TellLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wFilNavn AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wTotLinjer AS INT.
  
  DEF VAR wLinje AS CHAR FORMAT "x(70)" NO-UNDO.
  
  INPUT stream InnFraFil from value (wFilNavn).

    REPEAT:
      IMPORT STREAM InnFraFil UNFORMATTED wLinje.
      wTotLinjer = wTotLinjer + 1.
    END.  

  INPUT stream InnFraFil close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Varetran C-Win 
PROCEDURE Varetran :
/*------------------------------------------------------------------------------
Purpose:     
Parameters:  <none>

Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipAntLinjer AS INT  NO-UNDO.
  DEF INPUT  PARAMETER ipDataLinje AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER lArtikkelNr AS DEC  NO-UNDO.
  DEF OUTPUT PARAMETER ipFeilIFil  AS LOG  NO-UNDO.

  CREATE HT-FilLinje.
  ASSIGN
    HT-FilLinje.HTFilId   = HT-FilHode.HTFilId
    HT-FilLinje.LinjeId   = ipAntLinjer
    HT-FilLinje.Ok        = ?.
        
  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = tmpVareTran.Strekkode NO-ERROR.
  IF AVAILABLE Strekkode THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
  IF AVAILABLE Strekkode AND AVAILABLE ArtBas THEN
      ASSIGN
      lArtikkelNr    = ArtBas.ArtikkelNr
      ipFeilIFil     = FALSE 
      HT-Fillinje.OK = ? 
      .
  ELSE
      ASSIGN
          lArtikkelNr = 0
          ipFeilIFil = TRUE
          HT-Fillinje.OK = FALSE
          .
  ASSIGN
      tmpVareTran.ArtikkelNr = (IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0)
      HT-FilLinje.LinjeData  = string(tmpVareTran.ButikkNr)    + CHR(1) +  /*  1 */
                               string(tmpVareTran.Strekkode)   + CHR(1) +  /*  2 */   
                               string(tmpVareTran.Dato)        + CHR(1) +  /*  3 */   
                               string(tmpVareTran.tid)         + CHR(1) +  /*  4 */   
                               string(tmpVareTran.LoggNr)      + CHR(1) +  /*  5 */   
                               string(tmpVareTran.TransType)   + CHR(1) +  /*  6 */  
                               string(tmpVareTran.Transtekst)  + CHR(1) +  /*  7 */   
                               string(tmpVareTran.BrukerId)    + CHR(1) +  /*  8 */   
                               string(tmpVareTran.Antall)      + CHR(1) +  /*  9 */   
                               string(tmpVareTran.Kostpris)    + CHR(1) +  /* 10 */   
                               string(tmpVareTran.Salgssum)    + CHR(1) +  /* 11 */   
                               string(tmpVareTran.NyLagAnt)    + CHR(1) +  /* 12 */   
                               string(tmpVareTran.GmlLagAnt)   + CHR(1) +  /* 13 */   
                               string(tmpVareTran.ArtikkelNr)  
      .

  RELEASE Strekkode.
  RELEASE ArtBas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl C-Win 
FUNCTION FiksStorl RETURNS CHARACTER
  ( INPUT wStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/

 ASSIGN
    wStorl = trim(wStorl)
    wStorl = caps(wStorl)
    wStorl = if (length(wStorl) = 1 OR 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF index(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

