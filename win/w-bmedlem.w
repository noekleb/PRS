&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 
  STEP 1: Välj tabell i browsern
  STEP 2: Definiera de fælt du ønskar i din browser
          Kryssa av enabled på de fælt du ønskar sortering
  STEP 3: Gør de andringar som behøvs i alla scope.
          Sorttype skall ha lika många entries som antal enablade fält.
              - Tillåtna värden = "" (blank -> BY = default)
              - 

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
&scope br-tabell   Medlem
&scope Sorttype    ,,,,,,,,
&scope BrowseIdx   Medlem,ForNavn,EtterNavn,Personnr,Kunde,ePostAdresse,FodselsDato,FodtAr,MobilTlf
&scope Sokvillkor  >=,>=,>=,>=,>=,>=,>=,>=,>=
&scope InitIdx     Medlem
&scope ip-felt     MedlemsNr
/* Om du önskar input parameter. Ger en startup-record */ 
/*
&scope ip-variabel w{&ip-felt}
*/

&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.MedlemsNr) ~
                                                else "".

&scope BrowseQ     FOR EACH {&br-tabell} NO-LOCK XSORTTYPE XSORT INDEXED-REPOSITION
&scope BrowseSQ    FOR EACH b{&br-tabell} NO-LOCK WHERE b{&br-tabell}.XFIELD XSOKV XFILL ~
                           USE-INDEX XIDX MAX-ROWS 1

/* Parameter Definisjoner ---                                           */
&IF LENGTH("{&ip-variabel}") > 0 &THEN

  &scope return-ip   ASSIGN {&ip-variabel} = {&br-tabell}.{&ip-felt}
  &scope init-phrase FIND b{&br-tabell} WHERE b{&br-tabell}.{&ip-felt} = ~
                        {&ip-variabel} USE-INDEX {&InitIdx} NO-LOCK NO-ERROR.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} INIT 6 NO-UNDO.
  &ELSE
    DEFINE INPUT-OUTPUT PARAMETER {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} NO-UNDO.
  &ENDIF

&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.
DEFINE QUERY wSQ FOR b{&br-tabell} SCROLLING.
DEFINE VARIABLE hQexp AS HANDLE NO-UNDO. /**/
define temp-table tmpChild
  field wChild as handle.

/* Lokale variabler ---                                                 */

DEFINE VAR retur-verdi       AS CHAR INIT "AVBRYT" NO-UNDO.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wSearchCols       AS CHAR       NO-UNDO.
DEFINE VAR wSearchColsH      AS WIDGET EXTENT 10 NO-UNDO.
DEFINE VAR wQ                AS WIDGET      NO-UNDO.
DEFINE VAR wSortCol          AS WIDGET      NO-UNDO.
DEFINE VAR wAntSortCol       AS INTE        NO-UNDO.
DEFINE VAR wAktivFillIn      AS WIDGET      NO-UNDO.
DEFINE VAR wSorttype         AS CHAR   INIT "{&Sorttype}"   NO-UNDO.
DEFINE VAR wSokvillkor       AS CHAR   INIT "{&Sokvillkor}" NO-UNDO.
DEFINE VAR wBrowseIdx        AS CHAR   INIT "{&BrowseIdx}"  NO-UNDO.
define var wOk               as log         no-undo.
define var wAlle             as char        no-undo.
define var wAktivQString     as char        no-undo.
define var wBekreft          as log         no-undo.
DEFINE VAR wBlank            AS LOG         NO-UNDO.
define var wRetStatus        as log         no-undo.
def    var wTypeBeskr        as char format "x(20)" no-undo.
def    var wGruppeBeskr      as char format "x(20)" no-undo.
define var wExcEkstent       as char        no-undo.
DEFINE VAR wAntPoster        AS INT         NO-UNDO.
DEFINE VAR wSkoTex           AS CHAR        NO-UNDO.
DEFINE VAR wKunde            AS CHAR        NO-UNDO.
DEFINE VAR wKriterier        AS CHAR        NO-UNDO.
DEFINE VAR wHarKort          AS LOG  FORMAT "K/ " NO-UNDO.
DEFINE VAR wMedlemsklubb     AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR iValg AS IN NO-UNDO.
DEF VAR lDec AS DEC NO-UNDO.
DEF VAR bOk  AS LOG NO-UNDO.
def var hTTexpbuffer as handle no-undo.

{runlib.i}
{medlem.i &New="New"}
DEFINE TEMP-TABLE tmpMedlem2 NO-UNDO LIKE Medlem  /* tmpMedlem opptatt i Medlemliste.i */
    FIELD Ordning AS INTE.
DEF TEMP-TABLE expMedlem NO-UNDO LIKE Medlem.

def stream Eksport.
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
&Scoped-define BROWSE-NAME BROWSE-Medlem
&Scoped-define QUERY-NAME QUERY-Alle

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Medlem bMedlem tmpMedlem2

/* Definitions for BROWSE BROWSE-Medlem                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Medlem Medlem.MedlemsNr Medlem.Aktiv ~
Medlem.ButikkNr Medlem.ForNavn Medlem.EtterNavn Medlem.PersonNr ~
wMedlemsklubb Medlem.Kjonn Medlem.HovedMedlemFlagg wHarKort Medlem.KundeNr ~
Medlem.ePostAdresse Medlem.FodselsDato wTypeBeskr Medlem.FodtAr ~
Medlem.Telefon Medlem.MobilTlf Medlem.Telefaks Medlem.EDato ~
Medlem.RegistrertDato wGruppeBeskr Medlem.Bonus_Berettiget ~
Medlem.Bonus_Forsendelse Medlem.Bonus_varsel Medlem.MKlubbId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Medlem Medlem.MedlemsNr ~
Medlem.ForNavn Medlem.EtterNavn Medlem.PersonNr Medlem.KundeNr ~
Medlem.ePostAdresse Medlem.FodselsDato Medlem.FodtAr Medlem.MobilTlf 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Medlem Medlem
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Medlem Medlem
&Scoped-define QUERY-STRING-BROWSE-Medlem FOR EACH Medlem NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Medlem OPEN QUERY BROWSE-Medlem FOR EACH Medlem NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Medlem Medlem
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Medlem Medlem


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for QUERY QUERY-Alle                                     */
&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define QUERY-STRING-QUERY-Alle FOR EACH bMedlem NO-LOCK BY MedlemsNr
&Scoped-define OPEN-QUERY-QUERY-Alle OPEN QUERY {&SELF-NAME} FOR EACH bMedlem NO-LOCK BY MedlemsNr.
&Scoped-define TABLES-IN-QUERY-QUERY-Alle bMedlem
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Alle bMedlem


/* Definitions for QUERY QUERY-tmp                                      */
&Scoped-define SELF-NAME QUERY-tmp
&Scoped-define QUERY-STRING-QUERY-tmp FOR EACH tmpMedlem2 NO-LOCK BY tmpMedlem2.Ordning
&Scoped-define OPEN-QUERY-QUERY-tmp OPEN QUERY {&SELF-NAME} FOR EACH tmpMedlem2 NO-LOCK BY tmpMedlem2.Ordning.
&Scoped-define TABLES-IN-QUERY-QUERY-tmp tmpMedlem2
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-tmp tmpMedlem2


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 RECT-52 FILL-IN-SOK-DATE ~
FILL-IN-SOK-INTE FILL-IN-SOK-CHAR FILL-IN-SOK-DECI BUTTON-Ny BUTTON-Endre ~
BUTTON-Slett B-Print B-Excel B-Rapp B-Saldo B-Statistikk B-Oppdater ~
B-KonvMedlem Btn_Help Btn_OK BUTTON-Sok CB-Type CB-Butikk CB_MKlubbId ~
CB-Gruppe CB-Aktiv FI-Navn FI-Personnr B-Blank-3 B-Blank2 BROWSE-Medlem 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-DATE FILL-IN-SOK-INTE ~
FILL-IN-SOK-CHAR FILL-IN-SOK-DECI CB-Type CB-Butikk CB_MKlubbId CB-Gruppe ~
CB-Aktiv FI-Navn FI-Personnr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Prep-Private-Data C-Win 
FUNCTION Prep-Private-Data RETURNS CHARACTER
  ( INPUT wQueryCol AS WIDGET,INPUT wQueryCol# AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Blank-3 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Blank2 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel":U
     LABEL "Excel..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter alle eller merkede tellelinjer til Excel. Alt-X.".

DEFINE BUTTON B-KonvMedlem 
     LABEL "&Konverter medlemsnummer" 
     SIZE 34.8 BY 1.14.

DEFINE BUTTON B-Oppdater 
     LABEL "&Oppdater browser" 
     SIZE 26 BY 1.14.

DEFINE BUTTON B-Print 
     IMAGE-UP FILE "icon/e-print":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av adresseetiketter".

DEFINE BUTTON B-Rapp 
     LABEL "Htm" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-Saldo 
     LABEL "Oppdater saldo ..." 
     SIZE 20 BY 1.14.

DEFINE BUTTON B-Statistikk 
     LABEL "Oppdater statistikk ..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "icon/e-exit":U
     LABEL "OK" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Endre 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Endre.." 
     SIZE 4.6 BY 1.1 TOOLTIP "Endre medlem (Alt-E)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon/e-ny":U
     LABEL "&Ny.." 
     SIZE 4.6 BY 1.1 TOOLTIP "Opprette nytt medlem (Alt-N)".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort medlem (Alt-D)".

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1 TOOLTIP "Søk i AKTIV kollonne".

DEFINE VARIABLE CB-Aktiv AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Aktiv" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Butikk AS CHARACTER FORMAT "X(256)":U INITIAL "[Alle]" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Gruppe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Medlemsgruppe" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Medlemstype" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE CB_MKlubbId AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Klubbnr" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 30 BY 1.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Personnr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Personnr" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DECI AS DECIMAL FORMAT ">>>>>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INTE AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 206 BY .1.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 206 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Medlem FOR 
      Medlem SCROLLING.

DEFINE QUERY QUERY-Alle FOR 
      bMedlem SCROLLING.

DEFINE QUERY QUERY-tmp FOR 
      tmpMedlem2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Medlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Medlem C-Win _STRUCTURED
  QUERY BROWSE-Medlem NO-LOCK DISPLAY
      Medlem.MedlemsNr FORMAT ">>>>>>>>>>>>9":U
      Medlem.Aktiv FORMAT "*/":U
      Medlem.ButikkNr FORMAT ">>>>>9":U
      Medlem.ForNavn COLUMN-LABEL "Fornavn" FORMAT "X(40)":U WIDTH 25
      Medlem.EtterNavn FORMAT "X(40)":U WIDTH 25
      Medlem.PersonNr FORMAT "X(15)":U
      wMedlemsklubb COLUMN-LABEL "Medlemsklubb" FORMAT "x(15)":U
      Medlem.Kjonn FORMAT "M/K":U
      Medlem.HovedMedlemFlagg COLUMN-LABEL "HMedl" FORMAT "*/":U
            WIDTH 6
      wHarKort COLUMN-LABEL "Kort" FORMAT "*/":U
      Medlem.KundeNr FORMAT ">>>>>>>>>>>>9":U
      Medlem.ePostAdresse FORMAT "X(60)":U WIDTH 30
      Medlem.FodselsDato FORMAT "99/99/9999":U
      wTypeBeskr COLUMN-LABEL "MedType" FORMAT "x(10)":U
      Medlem.FodtAr COLUMN-LABEL "FødtÅr" FORMAT "9999":U
      Medlem.Telefon FORMAT "X(15)":U
      Medlem.MobilTlf FORMAT "X(15)":U
      Medlem.Telefaks FORMAT "X(15)":U
      Medlem.EDato FORMAT "99/99/9999":U
      Medlem.RegistrertDato COLUMN-LABEL "Registrert" FORMAT "99/99/9999":U
      wGruppeBeskr COLUMN-LABEL "MedGrp" FORMAT "x(10)":U WIDTH 8.6
      Medlem.Bonus_Berettiget FORMAT "yes/no":U
      Medlem.Bonus_Forsendelse FORMAT "9":U
      Medlem.Bonus_varsel FORMAT "9":U
      Medlem.MKlubbId FORMAT ">>9":U
  ENABLE
      Medlem.MedlemsNr
      Medlem.ForNavn
      Medlem.EtterNavn
      Medlem.PersonNr
      Medlem.KundeNr
      Medlem.ePostAdresse
      Medlem.FodselsDato
      Medlem.FodtAr
      Medlem.MobilTlf
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN MULTIPLE SIZE 205 BY 25.48 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-INTE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DECI AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Ny AT ROW 1.48 COL 33
     BUTTON-Endre AT ROW 1.48 COL 38
     BUTTON-Slett AT ROW 1.48 COL 43
     B-Print AT ROW 1.48 COL 48
     B-Excel AT ROW 1.48 COL 53
     B-Rapp AT ROW 1.48 COL 58.2
     B-Saldo AT ROW 1.48 COL 91.6
     B-Statistikk AT ROW 1.48 COL 112.2
     B-Oppdater AT ROW 1.48 COL 134.4
     B-KonvMedlem AT ROW 1.48 COL 160.6 WIDGET-ID 12
     Btn_Help AT ROW 1.48 COL 197
     Btn_OK AT ROW 1.48 COL 202
     BUTTON-Sok AT ROW 1.52 COL 21.4
     CB-Type AT ROW 3.14 COL 16 COLON-ALIGNED
     CB-Butikk AT ROW 3.14 COL 99 COLON-ALIGNED
     CB_MKlubbId AT ROW 3.14 COL 172 COLON-ALIGNED WIDGET-ID 6
     CB-Gruppe AT ROW 4.33 COL 16 COLON-ALIGNED
     CB-Aktiv AT ROW 4.33 COL 99 COLON-ALIGNED
     FI-Navn AT ROW 5.52 COL 16 COLON-ALIGNED
     FI-Personnr AT ROW 5.52 COL 99.4 COLON-ALIGNED
     B-Blank-3 AT ROW 5.52 COL 164.6 WIDGET-ID 10
     B-Blank2 AT ROW 5.57 COL 81.8
     BROWSE-Medlem AT ROW 6.95 COL 2
     RECT-51 AT ROW 1.24 COL 1
     RECT-52 AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 206.4 BY 31.62.


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
         TITLE              = "Søkeliste medlemmer"
         HEIGHT             = 31.62
         WIDTH              = 204.8
         MAX-HEIGHT         = 39.19
         MAX-WIDTH          = 230.4
         VIRTUAL-HEIGHT     = 39.19
         VIRTUAL-WIDTH      = 230.4
         MAX-BUTTON         = no
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-Medlem B-Blank2 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

ASSIGN 
       B-Print:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       B-Rapp:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BROWSE-Medlem:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 3
       BROWSE-Medlem:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 481
       BROWSE-Medlem:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Medlem
/* Query rebuild information for BROWSE BROWSE-Medlem
     _TblList          = "SkoTex.Medlem"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > SkoTex.Medlem.MedlemsNr
"MedlemsNr" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > SkoTex.Medlem.Aktiv
"Aktiv" ? "*~~/" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = SkoTex.Medlem.ButikkNr
     _FldNameList[4]   > SkoTex.Medlem.ForNavn
"ForNavn" "Fornavn" ? "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > SkoTex.Medlem.EtterNavn
"EtterNavn" ? ? "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > SkoTex.Medlem.PersonNr
"PersonNr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"wMedlemsklubb" "Medlemsklubb" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = SkoTex.Medlem.Kjonn
     _FldNameList[9]   > SkoTex.Medlem.HovedMedlemFlagg
"HovedMedlemFlagg" "HMedl" "*~~/" "logical" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"wHarKort" "Kort" "*~~/" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > SkoTex.Medlem.KundeNr
"KundeNr" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > SkoTex.Medlem.ePostAdresse
"ePostAdresse" ? "X(60)" "character" ? ? ? ? ? ? yes ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > SkoTex.Medlem.FodselsDato
"FodselsDato" ? "99/99/9999" "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"wTypeBeskr" "MedType" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > SkoTex.Medlem.FodtAr
"FodtAr" "FødtÅr" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = SkoTex.Medlem.Telefon
     _FldNameList[17]   > SkoTex.Medlem.MobilTlf
"MobilTlf" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   = SkoTex.Medlem.Telefaks
     _FldNameList[19]   = SkoTex.Medlem.EDato
     _FldNameList[20]   > SkoTex.Medlem.RegistrertDato
"RegistrertDato" "Registrert" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"wGruppeBeskr" "MedGrp" "x(10)" ? ? ? ? ? ? ? no ? no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   = SkoTex.Medlem.Bonus_Berettiget
     _FldNameList[23]   = SkoTex.Medlem.Bonus_Forsendelse
     _FldNameList[24]   = SkoTex.Medlem.Bonus_varsel
     _FldNameList[25]   = SkoTex.Medlem.MKlubbId
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-Medlem */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Alle
/* Query rebuild information for QUERY QUERY-Alle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH bMedlem NO-LOCK BY MedlemsNr.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 4.1 , 153 )
*/  /* QUERY QUERY-Alle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-tmp
/* Query rebuild information for QUERY QUERY-tmp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tmpMedlem2 NO-LOCK BY tmpMedlem2.Ordning.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 2.91 , 153 )
*/  /* QUERY QUERY-tmp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søkeliste medlemmer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søkeliste medlemmer */
DO:
  if can-find(first tmpChild where
               valid-handle(tmpChild.wChild)) then
    do:
      wBekreft = false.
      message 'Det er startet andre programmer fra dette vinduet.' skip
              'Avsluttes dette vinduet, vil alle underliggende programmer' skip
              'også bli avsluttet.'
              view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
              update wBekreft
              .
    end.
  else wBekreft = true.
  if wBekreft <> true then
  return no-apply.
                       
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-3 C-Win
ON CHOOSE OF B-Blank-3 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-Personnr = "".
  display FI-Personnr with frame DEFAULT-FRAME.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank2 C-Win
ON CHOOSE OF B-Blank2 IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FI-Navn = "*".
  display FI-Navn with frame DEFAULT-FRAME.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Excel... */
DO:
    /* RUN ExHtmRapp ("EX"). */
    iValg = 1.
    RUN EksportMedlemsData.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KonvMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvMedlem C-Win
ON CHOOSE OF B-KonvMedlem IN FRAME DEFAULT-FRAME /* Konverter medlemsnummer */
DO:

  iValg = 2.
  RUN konverterMedlemsNr.

  IF bOk AND wAntPoster > 0 THEN
      RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-Win
ON CHOOSE OF B-Oppdater IN FRAME DEFAULT-FRAME /* Oppdater browser */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Print C-Win
ON CHOOSE OF B-Print IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  def var wSvar as log no-undo.
  def var wLoop as int no-undo.
  
  /* Nullstiller logg */
  for each tmpMedlem:
    delete tmpMedlem.
  end.
  
  wSvar = false.
  /* Bruker har valgt en eller flere linjer. */
  if BROWSE-Medlem:NUM-SELECTED-ROWS > 0 then
  do TRANSACTION:  
    message "Skal utskrift startes for de merkede medlemmene?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    if wSvar = false then 
      return no-apply.  

    {sww.i}
    do wLoop = 1 to BROWSE-Medlem:NUM-SELECTED-ROWS:
      wOk = BROWSE-Medlem:FETCH-SELECTED-ROW(wLoop).
      if not can-find(first tmpMedlem where
        tmpMedlem.MedlemsRecid = recid(Medlem)) then
        do:
          create tmpMedlem.
          assign
            tmpMedlem.MedlemsRecid = recid(Medlem).
        end.
    end.
    {swn.i}
  end.
  
  /* Ingen linjer er valgt. Alle linjer skal behandles. Bruker må bekrefte */
  else do:
    message "Skal utskrift startes for ALLE medlemmer?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    if wSvar = false then 
      return no-apply.  
    {sww.i}
    do:
      for each Medlem no-lock:
        if not can-find(first tmpMedlem where
          tmpMedlem.MedlemsRecid = recid(Medlem)) then
          do:
            create tmpMedlem.
            assign
              tmpMedlem.MedlemsRecid = recid(Medlem).
          end.
      end.  
    end.
    {swn.i}
  end. 
  
  if can-find(first tmpMedlem) then
    run medlemsliste.p (820,3).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapp C-Win
ON CHOOSE OF B-Rapp IN FRAME DEFAULT-FRAME /* Htm */
DO:
    RUN ExHtmRapp ("HTM").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Saldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Saldo C-Win
ON CHOOSE OF B-Saldo IN FRAME DEFAULT-FRAME /* Oppdater saldo ... */
DO:
  RUN OppdaterSaldo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Statistikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Statistikk C-Win
ON CHOOSE OF B-Statistikk IN FRAME DEFAULT-FRAME /* Oppdater statistikk ... */
DO:
  RUN OppdaterStatistikk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Medlem
&Scoped-define SELF-NAME BROWSE-Medlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Medlem C-Win
ON ANY-PRINTABLE OF BROWSE-Medlem IN FRAME DEFAULT-FRAME
DO:
  if lastkey <> 32 then
    do:
      RUN SD-ANY-PRINTABLE.
      RETURN NO-APPLY.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Medlem C-Win
ON CURSOR-LEFT OF BROWSE-Medlem IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Medlem C-Win
ON CURSOR-RIGHT OF BROWSE-Medlem IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Medlem C-Win
ON DEFAULT-ACTION OF BROWSE-Medlem IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Endre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Medlem C-Win
ON HOME OF BROWSE-Medlem IN FRAME DEFAULT-FRAME
DO:
  APPLY "ENTRY" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Medlem C-Win
ON ROW-DISPLAY OF BROWSE-Medlem IN FRAME DEFAULT-FRAME
DO:
  wHarKort = FALSE.

  if available Medlem then
    do:
      find MedlemsGruppe OF Medlem NO-LOCK no-error.
      find MedlemsType   of Medlem no-lock no-error.
      FIND MedlemsKlubb  OF Medlem NO-LOCK NO-ERROR.
      assign
        wTypeBeskr   = if available MedlemsType   then MedlemsType.Beskrivelse else ""
        wGruppeBeskr = if available MedlemsGruppe then MedlemsGruppe.Beskrivelse else "".
      IF CAN-FIND(FIRST MEdlemsKort OF Medlem) THEN
          ASSIGN
            wHarKort = TRUE.
      IF AVAILABLE MedlemsKlubb THEN
          wMedlemsklubb = MedlemsKlubb.MKlubbBeskrivelse.
      ELSE 
          wMedlemsklubb = ''.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Medlem C-Win
ON START-SEARCH OF BROWSE-Medlem IN FRAME DEFAULT-FRAME
DO:
  RUN lockwindowupdate(FRAME {&FRAME-NAME}:HWND).
  DEF VAR wSearchCol  AS WIDGET NO-UNDO.
  DEF VAR wQString    AS CHAR   NO-UNDO.
  DEF VAR wSortColIdx AS INTE   NO-UNDO.
  DEF VAR bDesc       AS LOG NO-UNDO.
  DEF VAR cTekst AS CHAR NO-UNDO.

  ASSIGN wSearchCol = SELF:CURRENT-COLUMN.

  IF wSortCol <> SELF:CURRENT-COLUMN AND
                   LOOKUP(wSearchCol:NAME,wSearchCols) > 0 THEN DO:
      ASSIGN wSortCol = SELF:CURRENT-COLUMN.
      RUN SortNyCol.
  END.

  ELSE IF LOOKUP(wSearchCol:NAME,wSearchCols) > 0 AND
          LOOKUP("USE-INDEX",wQ:PREPARE-STRING," ") = 0 THEN DO:

      ASSIGN wQString = wQ:PREPARE-STRING
             cTekst   = wQString.

      IF wQString MATCHES '*DESCENDING*' 
          THEN bDesc = TRUE.
      ELSE bDesc = FALSE.

      ASSIGN
             wQString = IF bDesc THEN
                 REPLACE(wQString,"BY {&br-tabell}." + wSearchCol:NAME + " DESCENDING","BY {&br-tabell}." + wSearchCol:NAME)
                        ELSE
                 REPLACE(wQString,"BY {&br-tabell}." + wSearchCol:NAME, 
                           "BY {&br-tabell}." + wSearchCol:NAME + " DESCENDING").
      IF LOOKUP("NO-LOCK",wQString," ") = 0 THEN
               wQString = REPLACE(wQString,'BY','NO-LOCK BY').

      ASSIGN
             wSearchCol:PRIVATE-DATA = wQString.


      wQ:QUERY-PREPARE(wQString).
      FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK.

      wQ:QUERY-OPEN().
      RUN SD-Reposition.
  END.
  APPLY "LEAVE" TO SELF. /* annars fungerar inte "ENTRY" ?? */
  APPLY "ENTRY" TO SELF.
  APPLY "END-SEARCH" TO SELF.
  RUN lockwindowupdate(0).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
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
  if can-find(first tmpChild where
               valid-handle(tmpChild.wChild)) then
    do:
      wBekreft = false.
      message 'Det er startet andre programmer fra dette vinduet.' skip
              'Avsluttes dette vinduet, vil alle underliggende programmer' skip
              'også bli avsluttet.'
              view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
              update wBekreft
              .
    end.
  else wBekreft = true.
  if wBekreft <> true then
  return no-apply.
                     
  {&return-ip}
  .
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
      .
  &ELSE
      ASSIGN retur-verdi = "OK".
  &ENDIF
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME DEFAULT-FRAME /* Endre.. */
DO:
  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY.
  DEF VAR wTmpChild AS HANDLE NO-UNDO.
  run w-vmedlem PERSISTENT SET wTmpChild (recid({&br-tabell}),"ENDRE").
  CREATE tmpChild.
  ASSIGN tmpChild.wChild = wTmpChild.
  RELEASE tmpChild.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny.. */
DO:
  DEF VAR wTmpChild AS HANDLE NO-UNDO.
  run w-vmedlem.w PERSISTENT SET wTmpChild (?,"Ny").
  CREATE tmpChild.
  ASSIGN tmpChild.wChild = wTmpChild.
  RELEASE tmpChild.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Slette */
DO:
  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY.
  def var wCurrRow as inte no-undo.
  IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS > 0 AND 
     BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN DO:
      BROWSE {&BROWSE-NAME}:SCROLL-TO-SELECTED-ROW(1).
  END.
  IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS <> 1 THEN DO:
     IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 0 THEN
         MESSAGE "Du må velge et medlem." VIEW-AS ALERT-BOX INFORMATION
                      TITLE "Feil".
     ELSE DO:
         MESSAGE "Du kan bare velge et medlem." SKIP
                 "Vil du rense ditt utvalg?" VIEW-AS ALERT-BOX QUESTION
                 BUTTONS YES-NO TITLE "Feil" UPDATE wJa AS LOGI.
         IF wJa THEN
             BROWSE {&BROWSE-NAME}:DESELECT-ROWS().
         REPOSITION {&BROWSE-NAME} TO ROW 1.
     END.
     RETURN NO-APPLY.
  END.
  ASSIGN wCurrRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW.
  run w-vmedlem.w (recid({&br-tabell}),"SLETT").
  if return-value = "AVBRYT" then
    return no-apply.
  wOk = BROWSE-{&br-tabell}:REFRESH().
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok C-Win
ON CHOOSE OF BUTTON-Sok IN FRAME DEFAULT-FRAME /* Søk */
DO:
   /* DEFINE VAR wBlank AS LOG  NO-UNDO.*/
   DEFINE VAR wSQStr AS CHAR NO-UNDO. 
   DEFINE VAR wChar AS  CHAR NO-UNDO.
   DEFINE VAR wXSokv AS CHAR NO-UNDO.
   
   wBlank = false.
   
   IF wAktivFillIn:SCREEN-VALUE = "" THEN
       wBlank = TRUE.
   ELSE DO:
       ASSIGN wChar  = IF wAktivFillIn:DATA-TYPE BEGINS "CHAR" THEN '"' ELSE ''
              wSQStr = REPLACE("{&BrowseSQ}","XFIELD",ENTRY(wAktivCol,wSearchCols))
              wSQStr = REPLACE(wSQStr,"XSOKV",ENTRY(wAktivCol,wSokvillkor))
              wSQStr = IF ENTRY(wAktivCol,wBrowseIdx) <> "" THEN
                         REPLACE(wSQStr,"XIDX",ENTRY(wAktivCol,wBrowseIdx)) 
                       ELSE 
                           REPLACE(wSQStr,"USE-INDEX XIDX ","") 
              wSQStr = REPLACE(wSQStr,"XFILL",wChar + wAktivFillIn:SCREEN-VALUE + wChar).
       QUERY wSQ:QUERY-PREPARE(wSQStr).
       QUERY wSQ:QUERY-OPEN().
       GET FIRST wSQ.
       IF NOT AVAIL b{&br-tabell} THEN DO:
           APPLY "ENTRY" TO wAktivFillIn.
           RETURN NO-APPLY.         
       END.
   END.

   IF wBlank THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
   ELSE do:
     wBlank = ?.
     RUN SD-Reposition.
     wBlank = false.
   end.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aktiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aktiv C-Win
ON VALUE-CHANGED OF CB-Aktiv IN FRAME DEFAULT-FRAME /* Aktiv */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butikk C-Win
ON VALUE-CHANGED OF CB-Butikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Gruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Gruppe C-Win
ON VALUE-CHANGED OF CB-Gruppe IN FRAME DEFAULT-FRAME /* Medlemsgruppe */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Type C-Win
ON VALUE-CHANGED OF CB-Type IN FRAME DEFAULT-FRAME /* Medlemstype */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB_MKlubbId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB_MKlubbId C-Win
ON VALUE-CHANGED OF CB_MKlubbId IN FRAME DEFAULT-FRAME /* Klubbnr */
DO:
    RUN SD-CURSOR (" ").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Navn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Navn C-Win
ON TAB OF FI-Navn IN FRAME DEFAULT-FRAME /* Etternavn */
or "RETURN" of FI-Navn
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Personnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Personnr C-Win
ON TAB OF FI-Personnr IN FRAME DEFAULT-FRAME /* Personnr */
or "RETURN" of FI-Personnr
DO:
  RUN SD-CURSOR (" ").
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
    ASSIGN wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
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


&Scoped-define SELF-NAME FILL-IN-SOK-DECI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DECI C-Win
ON RETURN OF FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-INTE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-INTE C-Win
ON RETURN OF FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define SELF-NAME QUERY-tmp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Søkeliste medlemmer"
  &PreIClose      = " "
  &PostIClose     = " "
  &PostDisable_ui = "for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end.
                     &IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
                       return retur-verdi.
                     &else
                       message {&ip-variabel} retur-verdi view-as alert-box.
                     &endif"
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 1 100 1 wAlle}
{syspara.i 1 4 1 wExcEkstent}
wExcEkstent = if wExcEkstent = "" then "sdv" else wExcEkstent.   

{syspara.i 1 1 100 wKunde}
{syspara.i 1 1 101 wSkoTex}

/* HotKeySøk - DYYYYRT */
on ALT-N of frame DEFAULT-FRAME anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Ny in frame DEFAULT-FRAME.
  end.
on ALT-E of frame DEFAULT-FRAME anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Endre in frame DEFAULT-FRAME.
  end.
on ALT-D of frame DEFAULT-FRAME anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Slett in frame DEFAULT-FRAME.
  end.
on ALT-P of frame DEFAULT-FRAME anywhere 
  do:
    apply "CHOOSE":U to B-Print in frame DEFAULT-FRAME.
  end.

  on ALT-S of {&WINDOW-NAME} anywhere 
    do: /* SOKTRIGGER */
      RUN d-medlemssok.w.
      IF RETURN-VALUE = "AVBRYT" THEN 
          RETURN NO-APPLY.
      ELSE DO:
        FIND bMedlem NO-LOCK WHERE
          bMedlem.MedlemsNr = DEC(RETURN-VALUE) NO-ERROR.
        if available bMedlem then
        do:
          wBlank = ?.
          RUN SD-Reposition.
          wBlank = false.
        end.
      END.
    end. /* SOKTRIGGER */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN InitVars. /* inkl open-query */
    IF RETURN-VALUE = "FEIL" THEN
       RETURN.
/*    IF "Komnavn" <> wSortCol:NAME THEN
        RUN QueryCustomSettings ("Komnavn"). */
    RUN SD-QUERY-OPEN.

    IF RETURN-VALUE = "FEIL" THEN
        LEAVE MAIN-BLOCK.

    RUN enable_UI.
    {lng.i} 

    /* {browsesettings.i {&BROWSE-NAME}}  Konflikt med filter. */
    IF BROWSE {&BROWSE-NAME}:CURRENT-COLUMN <> wSortCol THEN DO:
        wSortCol = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
        RUN QueryCustomSettings (wSortCol:NAME).
    END.

    RUN InitType.
    run InitGruppe.
    RUN InitButikk.
    RUN InitCB.
    RUN InitAktiv.
    
    RUN LabelColor.
    RUN Move-Fill-To-Top. 
    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    &ENDIF 
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END. 
    ELSE IF AVAILABLE {&br-tabell} THEN
      do:
        find MedlemsType of Medlem no-lock no-error.
        find MedlemsGruppe of Medlem no-lock no-error.
        REPOSITION {&BROWSE-NAME} TO ROW 1.
      end.
    ASSIGN {&WINDOW-NAME}:HIDDEN = FALSE.      
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggExpMedlem C-Win 
PROCEDURE ByggExpMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wSvar as log no-undo.
  def var wLoop as int no-undo.
  DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.
  EMPTY TEMP-TABLE expMedlem. /* [ NO-ERROR ] */
  wSvar = false.
  /* Kun merkede medlemmer skal ut. */
  if BROWSE BROWSE-Medlem:NUM-SELECTED-ROWS > 0 then do: 
    IF iValg = 1 THEN
      MESSAGE "Skal eksporten startes for de merkede medlemmene?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    ELSE wSvar = TRUE.
    
    IF wSvar = TRUE THEN DO:
        ASSIGN
          wKriterier = "Export av merkede medlemmer".
        do wLoop = 1 to BROWSE BROWSE-Medlem:NUM-SELECTED-ROWS:
            IF BROWSE BROWSE-Medlem:FETCH-SELECTED-ROW(wLoop) THEN DO:
                create expMedlem.
                BUFFER-COPY Medlem TO expMedlem.
                RELEASE expMedlem.
            END.
        end.
        RETURN "TMP".
    END.
    ELSE
      return "AVBRYT".  
  end.
  /* Alle medlemmer skal ut */
  else do:
      message "Skal utskrift startes for ALLE medlemmer?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.

      IF wSvar = FALSE THEN
          RETURN "AVBRYT".  
                                              
      ASSIGN
        wKriterier = "Export av alle medlemmer".
        hTTexpbuffer  = TEMP-TABLE expMedlem:DEFAULT-BUFFER-HANDLE.
/*         hTTexpbuffer:BUFFER-COPY(BUFFER bonglinje:HANDLE). */
        hBuffer = BUFFER Medlem:HANDLE.
        CREATE QUERY hQexp.
        hQexp:SET-BUFFERS(hBuffer). 
        hQexp:QUERY-PREPARE(wQ:PREPARE-STRING).
        hQexp:QUERY-OPEN.
        hQexp:GET-FIRST().
        REPEAT WHILE NOT hQexp:QUERY-OFF-END:
            hTTexpbuffer:BUFFER-CREATE().
            hTTexpbuffer:BUFFER-COPY(hBuffer).
            hQexp:GET-NEXT().
        END.

/*       FOR EACH Medlem NO-LOCK: */
/*         create expMedlem. */
/*         BUFFER-COPY Medlem TO expMedlem. */
/*         RELEASE expMedlem. */
/*       END. */
      hQexp:QUERY-CLOSE().
      DELETE OBJECT hQexp.
      RETURN "OK".
  end. 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportMedlemsData C-Win 
PROCEDURE EksportMedlemsData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR wFileName AS CHAR NO-UNDO.

  DEF BUFFER b1Medlem FOR expMedlem.
  DEF BUFFER b1Kunde  FOR Kunde.
  
  /* Bygger datasett som skal eksporteres */
  RUN ByggExpMedlem.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN "AVBRYT".

  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("Medlem", wExcEkstent, output wFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(wFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "MEDLEMSDATA"
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */ "MedlemsNr" 
    /* B  */ "Aktiv"
    /* C  */ "ForNavn"          
    /* D  */ "EtterNavn"  
    /* E  */ "Personnr"
    /* F  */ "MedType"          
    /* G  */ "Beskrivelse"
    /* H  */ "MedGruppe"        
    /* I  */ "Beskrivelse"
    /* J  */ "Adresse1"         
    /* K  */ "Adresse2"         
    /* L  */ "PostNr"           
    /* M  */ "Poststed"
    /* N  */ "Land"             
    /* O  */ "Telefon"          
    /* P  */ "Telefaks"         
    /* Q  */ "MobilTlf"         
    /* R  */ "Utsendelse"             
    /* S  */ "Medlemsklubb" 
    /* T  */ "Opphort"          
    /* U  */ "ButikkNr"         
    /* V  */ "BydelsNr"         
    /* W  */ "ePostAdresse"     
    /* X  */ "FodselsDato"      
    /* Y  */ "FodtAr"           
    /* S  */ "Kjonn"            
    /* AA */ "Personnr."
    /* AB */ "RegKode"          
    /* AC */ "Navn"             
    /* AD */ "HovedMedlemFlagg" 
    /* AE */ "HovedMedlemsNr"   
    /* AF */ "Navn"             
    /* AG */ "KundeNr"          
    /* AH */ "Navn"             
    /* AI */ "EDato"            
    /* AJ */ "ETid"             
    /* AK */ "BrukerID"         
    /* AL */ "RegistrertDato"   
    /* AM */ "RegistrertTid"    
    /* AN */ "RegistrertAv"     
    SKIP.                                 
                                  
  /* Nullstiller */
  ASSIGN
    wAntPoster = 0.

  /* Eksporterer data */
  EKSPORT:
  FOR EACH expMedlem no-lock
    BY expMedlem.MedlemsNr:
                    
    /* Henter relaterte data */
    FIND Medlem NO-LOCK WHERE
         Medlem.MedlemsNr = expMedlem.MedlemsNr NO-ERROR.
    FIND Post          OF Medlem NO-LOCK NO-ERROR.
    FIND MedlemsType   OF Medlem NO-LOCK NO-ERROR.
    FIND MedlemsGruppe OF Medlem NO-LOCK NO-ERROR.
    FIND Region        OF Medlem NO-LOCK NO-ERROR.

    FIND Medlemsklubb NO-LOCK WHERE MedlemsKlubb.MKlubbId = Medlem.MKlubbId NO-ERROR.
    IF expMedlem.HovedMedlemsNr <> 0 THEN
        FIND b1Medlem NO-LOCK WHERE
             b1Medlem.MedlemsNr = expMedlem.HovedMedlemsNr NO-ERROR.
    IF expMedlem.KundeNr <> 0 THEN
        FIND Kunde NO-LOCK WHERE
             Kunde.KundeNr = expMedlem.MedlemsNr NO-ERROR.

    EXPORT STREAM sExportFile DELIMITER ";"
        expMedlem.MedlemsNr  
        expMedlem.Aktiv
        expMedlem.ForNavn          
        expMedlem.EtterNavn 
        expMedlem.MedlemInfo
        expMedlem.MedType  
        
        (IF AVAILABLE MedlemsType THEN MedlemsType.Beskrivelse ELSE "")
        expMedlem.MedGruppe        
        (IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.Beskrivelse ELSE "")
        expMedlem.Adresse1         
        expMedlem.Adresse2         
        expMedlem.PostNr           
        (IF AVAILABLE Post THEN Post.Beskrivelse ELSE "")
        expMedlem.Land             
        expMedlem.Telefon          
        expMedlem.Telefaks         
        expMedlem.MobilTlf 
        expMedlem.MottaeMailUtsendelser
        (IF AVAILABLE MedlemsKlubb THEN
            MedlemsKlubb.MKlubbBeskrivelse ELSE "")
        (IF expMedlem.Opphort <> ?
           THEN string(expMedlem.Opphort)
           ELSE "")
        expMedlem.ButikkNr         
        expMedlem.BydelsNr         
        expMedlem.ePostAdresse     
        (IF expMedlem.FodselsDato <> ?
            THEN string(expMedlem.FodselsDato) 
            ELSE "")
        (IF expMedlem.FodtAr <> ?
            THEN string(expMedlem.FodtAr)
            ELSE "")
        expMedlem.Kjonn 
        expMedlem.PersonNr
        expMedlem.RegKode          
        (IF AVAILABLE Region
           THEN Region.Navn
           ELSE "")
        expMedlem.HovedMedlemFlagg 
        expMedlem.HovedMedlemsNr   
        (IF AVAILABLE b1Medlem 
            THEN (b1Medlem.ForNavn + " " + b1Medlem.EtterNavn)
            ELSE "")
        expMedlem.KundeNr
        (IF AVAILABLE Kunde
           THEN Kunde.Navn
           ELSE "")
        expMedlem.EDato            
        string(expMedlem.ETid,"HH:MM:SS")             
        expMedlem.BrukerID         
        expMedlem.RegistrertDato   
        string(expMedlem.RegistrertTid,"HH:MM:SS")    
        expMedlem.RegistrertAv 
        .
                                
    ASSIGN
      wAntPoster = wAntPoster + 1.
  END. /* EKSPORT */
                                  
  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(wFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:AM2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:AM2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("D:D"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("I:I"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("N:N"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("Q:Q"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  /*
  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("E:E"):NumberFormat = "# ##0".
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  chWorkSheets:Range("H:H"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("I:I"):NumberFormat = "# ##0".
  chWorkSheets:Range("J:J"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("K:K"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("L:L"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("M:M"):NumberFormat = "# ##0,00".
  */
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:C1"):Merge().
  chWorkSheets:Range("A1:C1"):HorizontalAlignment = 3.
  /*
  chWorkSheets:Range("C3:C3"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E3:E3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F3:F3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  chWorkSheets:Range("G3:G3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("H3:H3"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  chWorkSheets:Range("I3:I3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("J3:J3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("K3:K3"):HorizontalAlignment = 4.   
  chWorkSheets:Range("L3:L3"):HorizontalAlignment = 4.   
  chWorkSheets:Range("M3:M3"):HorizontalAlignment = 4.   
  */
  
  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:AL"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:AJ2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier: " + wKriterier. 
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(wAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = wKunde.
  chWorkSheets:PageSetup:RightFooter    = wSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:AJ".
  chWorkSheets:PageSetup:Orientation    = 2.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("D3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  
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
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.

  STATUS DEFAULT "".

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
  DISPLAY FILL-IN-SOK-DATE FILL-IN-SOK-INTE FILL-IN-SOK-CHAR FILL-IN-SOK-DECI 
          CB-Type CB-Butikk CB_MKlubbId CB-Gruppe CB-Aktiv FI-Navn FI-Personnr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-51 RECT-52 FILL-IN-SOK-DATE FILL-IN-SOK-INTE FILL-IN-SOK-CHAR 
         FILL-IN-SOK-DECI BUTTON-Ny BUTTON-Endre BUTTON-Slett B-Print B-Excel 
         B-Rapp B-Saldo B-Statistikk B-Oppdater B-KonvMedlem Btn_Help Btn_OK 
         BUTTON-Sok CB-Type CB-Butikk CB_MKlubbId CB-Gruppe CB-Aktiv FI-Navn 
         FI-Personnr B-Blank-3 B-Blank2 BROWSE-Medlem 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExHtmRapp C-Win 
PROCEDURE ExHtmRapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wDokTyp AS CHAR NO-UNDO.
  DEFINE VAR wtmpFileName AS CHAR NO-UNDO.
  DEFINE VAR wHead1Set    AS CHAR NO-UNDO.
  DEFINE VAR wColHead     AS CHAR NO-UNDO.
  DEFINE VAR wFields      AS CHAR NO-UNDO.
  DEFINE VAR wColHeadForm AS CHAR NO-UNDO.
  DEFINE VAR wTabell      AS CHAR NO-UNDO.
  DEFINE VAR wSep         AS CHAR INIT ";" NO-UNDO.
  DEFINE VAR wQY          AS CHAR NO-UNDO.
  DEFINE VAR hQuery       AS HANDLE NO-UNDO.
  DEFINE VAR hMedlem      AS HANDLE NO-UNDO.
  DEFINE VAR wLblHdl      AS HANDLE NO-UNDO.
  DEFINE VAR wTotWidthC   AS DECI NO-UNDO.
  DEFINE VAR wColWidthC   AS CHAR NO-UNDO.
  DEFINE VAR wCount       AS INTE NO-UNDO.
  RUN SkapatmpMedlem.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  {sww.i}
 DO WITH FRAME {&FRAME-NAME}:
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle (wDokTyp, IF wDokTyp = "HTM" THEN "HTM" ELSE wExcEkstent, output wtmpFileName). 
  IF RETURN-VALUE = "TMP" THEN DO:
     ASSIGN hQuery = QUERY QUERY-tmp:HANDLE.
     OPEN QUERY QUERY-tmp FOR EACH tmpMedlem2 NO-LOCK BY tmpMedlem2.Ordning.
  END.
  ELSE DO:
      ASSIGN hQuery = QUERY QUERY-Alle:HANDLE
             hMedlem = {&BROWSE-NAME}:QUERY.
      hQuery:QUERY-PREPARE(REPLACE(hMedlem:PREPARE-STRING," Medlem"," bMedlem")).
      hQuery:QUERY-OPEN().
  END.
  DO:
    ASSIGN wLblHdl = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(wLblHdl).
      IF wLblHdl:label = "" THEN LEAVE.
      ASSIGN wColHead = wColHead + (if wColHead <> "" then wSep else "") + 
                    TRIM(REPLACE(wLblHdl:label,"*",""))
             wFields  = wFields + (if wFields <> "" then wSep else "") + 
                    TRIM(wLblHdl:NAME)
             wTotWidthC = wTotWidthC + wLblHdl:WIDTH-CHARS
             wColHeadForm = wColHeadForm + (if wColHeadForm = "" then "" else wSep) +
                          (IF LENGTH(wLblHdl:FORMAT) = 1 THEN "C" ELSE (IF
                          wLblHdl:DATA-TYPE BEGINS "INTE" OR wLblHdl:DATA-TYPE BEGINS "DECI" THEN "R" ELSE "L"))
             wColWidthC = wColWidthC + (if wColWidthC = "" then "" else wSep) + string(INT(wLblHdl:WIDTH-CHARS))
             wLblHdl = wLblHdl:NEXT-COLUMN.
        
    end.
    DO wCount = 1 TO NUM-ENTRIES(wColHeadForm,wSep):
      ENTRY(wCount,wColHeadForm,wSep) = ENTRY(wCount,wColHeadForm,wSep) + "," + String(INT(DECI(ENTRY(wCount,wColWidthC,wSep)) / wTotWidthC * 100)) + "%".
    END.
  END.
  ASSIGN wTabell = "Medlem"
         wHead1Set    = "100%,,1,0,2," + STRING(NUM-ENTRIES(wFields,wSep))
         wQY          = hQuery:PREPARE-STRING.
  RUN w-bhtml.p(wDokTyp,wSep,wHead1Set,wColHead,wFields,wColHeadForm,wTabell,wQY,hQuery,wtmpFileName).
  if valid-handle(wLibHandle) then DO:
    IF wDokTyp = "HTM" THEN
        RUN OpenWeb in wLibHandle (wtmpFileName).
    ELSE run OpenExcelDocument in wLibHandle (wtmpFileName," ").
  END.
  {swn.i}
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitAktiv C-Win 
PROCEDURE InitAktiv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wAktiv as char no-undo.

  /* Frame SCOOP */
  do with frame DEFAULT-FRAME:
  
  {sww.i}
  wAktiv = "[Alle],Aktiv,Passiv".

  assign
    CB-Aktiv = entry(1,wAktiv)
    CB-Aktiv:List-Items = wAktiv.
  display CB-Aktiv.
  {swn.i}
  
  end. /* Frame SCOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButikk C-Win 
PROCEDURE InitButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
--------------------------------------------------------------------------------*/
  def var wButikkLst as char no-undo.
  
  wButikkLst = wAlle.

  SYSPARA:
  for each Butiker no-lock:
    
    assign
      wButikkLst = wButikkLst + 
                   (if wButikkLst = "" 
                      then ""
                      else ",") +
                   string(Butiker.Butik,"zzzzz9") + ": " + Butiker.ButNamn.    
  end. /* SYSPARA */

  assign
    CB-Butikk = entry(1,wButikkLst)
    CB-Butikk:List-Items in frame DEFAULT-FRAME = wButikkLst.
  
  display CB-Butikk with frame {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-Win 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  ASSIGN
      pcTekst = wAlle + ',0'.
  FOR EACH MedlemsKlubb NO-LOCK:
      ASSIGN
          pcTekst = pcTekst + 
                    (IF pcTekst = '' THEN '' ELSE ',') + 
                    MedlemsKlubb.MKlubbBeskrivelse + ',' + 
                    STRING(MedlemsKlubb.MKlubbId)
                    .
  END.

  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          CB_MKlubbId:LIST-ITEM-PAIRS IN FRAME DEFAULT-FRAME = pcTekst
          CB_MKlubbId:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ENTRY(2,pcTekst)
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitGruppe C-Win 
PROCEDURE InitGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wGrupper as char no-undo.

  /* Frame SCOOP */
  do with frame DEFAULT-FRAME:
  
  {sww.i}
  wGrupper = wAlle.

  for each MedlemsGruppe no-lock where MedlemsGruppe.MedGruppe > 0:
    assign
      wGrupper = wGrupper + 
                      (if wGrupper = ""
                         then ""
                         else ",") + string(MedlemsGruppe.MedGruppe,"9999") + ": " + MedlemsGruppe.Beskrivelse.
  end.
  
  assign
    CB-Gruppe = entry(1,wGrupper)
    CB-Gruppe:List-Items = wGrupper.
  display CB-Gruppe.
  {swn.i}
  
  end. /* Frame SCOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitType C-Win 
PROCEDURE InitType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
--------------------------------------------------------------------------------*/
  def var wTypeLst as char no-undo.
  
  wTypeLst = wAlle.

  SYSPARA:
  for each MedlemsType no-lock:
    
    assign
      wTypeLst = wTypeLst + 
                   (if wTypeLst = "" 
                      then ""
                      else ",") +
                   string(MedlemsType.MedType,"zzz9") + ": " + MedlemsType.Beskrivelse.    
  end. /* SYSPARA */

  assign
    CB-Type = entry(1,wTypeLst)
    CB-Type:List-Items in frame DEFAULT-FRAME = wTypeLst.
  
  display CB-Type with frame {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVars C-Win 
PROCEDURE InitVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wW     AS WIDGET NO-UNDO.
    DEF VAR wCount AS INTE   NO-UNDO.

    ASSIGN wAntSortCol = NUM-ENTRIES("{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," ")
           wAktivCol   = IF wAktivCol > wAntSortCol THEN 1 ELSE wAktivCol.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
    DEF VAR wIdx  AS INTE NO-UNDO.
    DEF VAR wNumE AS INTE NO-UNDO.
    IF wAntSortCol < 1 THEN DO:
        MESSAGE "Du må 'enabla' minst ett felt" view-as alert-box.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSorttype) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sorttype skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," skip
                " kommaseparert med valgfritt BY, BY DESCENDING eller USE-INDEX." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSokvillkor) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sokvillkor skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," skip
                " kommaseparert med valgfritt <=,>= eller =." SKIP
                "Aktuellt värde: " + wSokvillkor
                 VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    DO wIdx = 1 TO wAntSortCol:
        CASE ENTRY(wIdx,wSorttype):
            WHEN "" OR WHEN "BY" OR WHEN "BY DESCENDING" OR WHEN "USE-INDEX" THEN.
            OTHERWISE DO:
                          MESSAGE "Tillåtna entries i 'scope Sort' = ''(tomt),BY,BY DESCENDING OCH USE-INDEX"
                                 VIEW-AS ALERT-BOX ERROR.
                          RETURN "FEIL".
                      END.
        END CASE.
    END.
    ASSIGN wNumE = NUM-ENTRIES(wBrowseIdx).
    IF NOT CAN-DO(wBrowseIdx,"USE-INDEX") AND NUM-ENTRIES(wBrowseIdx) <> wAntSortCol THEN DO:
        MESSAGE "Antall entries i 'scope BrowseIdx' <>" wAntSortCol VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    DO wIdx = 1 TO wAntSortCol:
        IF ENTRY(wIdx,wSorttype) = "USE-INDEX" AND
                (wIdx > wNumE OR ENTRY(wIdx,"{&BrowseIdx}") = "") THEN DO:
            MESSAGE "Entry " wIdx " av 'scope Sorttype' = USE-INDEX och" SKIP
                    "entry " wIdx " saknas i 'scope BrowseIdx'" VIEW-AS ALERT-BOX ERROR.
            RETURN "FEIL".
        END.
    END.
  &ENDIF
    DO wCount = 1 TO wAntSortCol:
        ASSIGN wSearchCols = IF wSearchCols = "" THEN 
               ENTRY(NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),".")
                                                ELSE
               wSearchCols + "," + 
                   ENTRY(NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),".").
    END.
    ASSIGN wW        = BROWSE {&BROWSE-NAME}:FIRST-COLUMN
           wOrgBgCol = wW:Label-bgcolor
           wCount = 1.
    REPEAT WHILE VALID-HANDLE(wW):
        IF LOOKUP(wW:NAME,wSearchCols) > 0 THEN
            ASSIGN wW:PRIVATE-DATA      = PREP-PRIVATE-DATA(wW:HANDLE,wCount)
                   wSearchColsH[wCount] = wW:HANDLE
                   wW:LABEL = wW:LABEL + STRING(LOOKUP("*",wW:LABEL," ") = 0," */")
                   wW:LABEL = wW:LABEL + IF ENTRY(wCount,wSortType) = "USE-INDEX" THEN
                                "" ELSE "*"
                   wCount               = wCount + 1
                   wW:READ-ONLY         = YES.
        ASSIGN wW = wW:NEXT-COLUMN.
    END.
    ASSIGN BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSearchColsH[wAktivCol]
           wSortCol                             = wSearchColsH[wAktivCol]
           wQ                                   = BROWSE {&BROWSE-NAME}:QUERY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konverterMedlemsNr C-Win 
PROCEDURE konverterMedlemsNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR wFileName AS CHAR NO-UNDO.

  DEF BUFFER b1Medlem FOR expMedlem.
  DEF BUFFER b1Kunde  FOR Kunde.
  
  STATUS DEFAULT "Sjekker utvalg for medlemmer som kan konverteres...".
  
  /* Bygger datasett som skal konverteres */
  RUN ByggExpMedlem.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN "AVBRYT".
                                  
  /* Nullstiller */
  ASSIGN
    wAntPoster = 0.

  /* Sjekker hvor mange medlemmer i utvalget som kan konverteres. */
  KONVERTER:
  FOR EACH expMedlem no-lock
    BY expMedlem.MedlemsNr:
                    
    /* Henter relaterte data */
    FIND Medlem NO-LOCK WHERE
         Medlem.MedlemsNr = expMedlem.MedlemsNr NO-ERROR.
    IF Medlem.MedlemsNr >= 9999999999 THEN
        NEXT.
    ASSIGN lDec = DEC(Medlem.PersonNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    IF NOT CAN-DO('10,11',STRING(LENGTH(Medlem.PersonNr))) THEN
        NEXT.

    ASSIGN
      wAntPoster = wAntPoster + 1.
  END. /* KONVERTER */
                                  
  STATUS DEFAULT "".

  IF wAntPoster = 0 THEN
  DO:
      MESSAGE 'Det er ingen poster i utvalget som kan koverteres.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  bOk = FALSE.
  MESSAGE 'Det er ' + STRING(wAntPoster) + ' medlemmer i utvalaget som kan konverteres.' SKIP
          'Skall konvertering starte?' SKIP(1)
          'Medlemmer som har et medlemsnr < 9999999999 og har et norsk eller svensk personnr, kan konverteres.'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk <> TRUE THEN
      RETURN NO-APPLY.

  /* Konverterer medlemmer. */
  wAntPoster = 0.
  KONVERTER:
  FOR EACH expMedlem no-lock
    BY expMedlem.MedlemsNr:
                    
    /* Henter relaterte data */
    FIND Medlem NO-LOCK WHERE
         Medlem.MedlemsNr = expMedlem.MedlemsNr NO-ERROR.
    IF Medlem.MedlemsNr >= 9999999999 THEN
        NEXT.
    ASSIGN lDec = DEC(Medlem.PersonNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    IF NOT CAN-DO('10,11',STRING(LENGTH(Medlem.PersonNr))) THEN
        NEXT.

    ASSIGN
      wAntPoster = wAntPoster + 1.
    lDec = expMedlem.MedlemsNr.
    RUN konverter_medlemsnr_personnr.p (INPUT-OUTPUT lDec).
  END. /* KONVERTER */

  bOk = TRUE.
  MESSAGE STRING(wAntPoster) + ' medlemmer konvertert.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
  DEF VAR wColorCol AS WIDGET NO-UNDO.
  ASSIGN wColorCol = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
  REPEAT WHILE VALID-HANDLE(wColorCol):
      IF LOOKUP("{&br-tabell}." + wColorCol:NAME,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," ") > 0 THEN
          ASSIGN wColorCol:LABEL-BGCOLOR = 
            IF wColorCol:NAME = wSortCol:NAME THEN wSortBgCol ELSE wOrgBgCol.
      ASSIGN wColorCol = wColorCol:NEXT-COLUMN.
  END.
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
        CASE SUBSTR(wSortCol:DATA-TYPE,1,4).
             WHEN "INTE" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-INTE:HANDLE.
             WHEN "CHAR" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-CHAR:HANDLE.
             WHEN "DATE" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-DATE:HANDLE.
             WHEN "DECI" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-DECI:HANDLE.
        END CASE.
    END.
    wAktivFillIn:MOVE-TO-TOP().
    ASSIGN wAktivFillIn:FORMAT                               = 
           IF wSortCol:DATA-TYPE = "INTEGER" THEN FILL(">",LENGTH(wSortCol:FORMAT)) ELSE wSortCol:FORMAT
           FILL-IN-SOK-DATE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DATE"
           FILL-IN-SOK-INTE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "INTE"
           FILL-IN-SOK-CHAR:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "CHAR"
           FILL-IN-SOK-DECI:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DECI".
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterSaldo C-Win 
PROCEDURE OppdaterSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER bMedlem FOR Medlem.

  DEF VAR pbOk AS LOG NO-UNDO.

  pbOk = FALSE.
  MESSAGE "Skal oppdatering av medlemssaldo starte?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
      UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  FOR EACH bMedlem NO-LOCK:
    STATUS DEFAULT "Oppdaterer saldo på medlem " + 
                   STRING(bMedlem.MedlemsNr) + " " + bMedlem.ForNavn + " " + bMEdlem.EtterNavn.
    RUN beregnmedlemsaldo.p(bMedlem.MedlemsNr, 0).
  END.
  STATUS DEFAULT "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterStatistikk C-Win 
PROCEDURE OppdaterStatistikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bMedlem FOR Medlem.

  DEF VAR pbOk AS LOG NO-UNDO.

  pbOk = FALSE.
  MESSAGE "Skal oppdatering av medlemsstatistikk starte?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
      UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  FOR EACH bMedlem NO-LOCK:
    STATUS DEFAULT "Oppdaterer statistikk på medlem " + 
                   STRING(bMedlem.MedlemsNr) + " " + bMedlem.ForNavn + " " + bMEdlem.EtterNavn.
    RUN oppdatmedlemstat.p(bMedlem.MedlemsNr).
  END.
  STATUS DEFAULT "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE orgByggExpMedlem C-Win 
PROCEDURE orgByggExpMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wSvar as log no-undo.
  def var wLoop as int no-undo.
  EMPTY TEMP-TABLE expMedlem. /* [ NO-ERROR ] */
  wSvar = false.
  /* Kun merkede medlemmer skal ut. */
  if BROWSE BROWSE-Medlem:NUM-SELECTED-ROWS > 0 then do: 
    IF iValg = 1 THEN
      MESSAGE "Skal eksporten startes for de merkede medlemmene?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    ELSE wSvar = TRUE.

    IF wSvar = TRUE THEN DO:
        ASSIGN
          wKriterier = "Export av merkede medlemmer".
        do wLoop = 1 to BROWSE BROWSE-Medlem:NUM-SELECTED-ROWS:
            IF BROWSE BROWSE-Medlem:FETCH-SELECTED-ROW(wLoop) THEN DO:
                create expMedlem.
                BUFFER-COPY Medlem TO expMedlem.
                RELEASE expMedlem.
            END.
        end.
        RETURN "TMP".
    END.
    ELSE
      return "AVBRYT".  
  end.
  /* Alle medlemmer skal ut */
  else do:
      message "Skal utskrift startes for ALLE medlemmer?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.

      IF wSvar = FALSE THEN
          RETURN "AVBRYT".  
                                              
      ASSIGN
        wKriterier = "Export av alle medlemmer".
      FOR EACH Medlem NO-LOCK:
        create expMedlem.
        BUFFER-COPY Medlem TO expMedlem.
        RELEASE expMedlem.
      END.
      RETURN "OK".
  end. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryCustomSettings C-Win 
PROCEDURE QueryCustomSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER wInitSortColNavn AS CHAR NO-UNDO. 
    DEF VAR wW            AS WIDGET        NO-UNDO.
    DEF VAR wCount        AS INTE   INIT 1 NO-UNDO.
    DEF VAR wLookUp       AS INTE          NO-UNDO.
    DEF VAR wNySearchCols AS CHAR          NO-UNDO.
    DEF VAR wNySokvillkor AS CHAR          NO-UNDO.
    DEF VAR wNyBrowseIdx  AS CHAR          NO-UNDO.
    ASSIGN wW = BROWSE {&BROWSE-NAME}:FIRST-COLUMN
           wInitSortColNavn = IF wInitSortColNavn = "" THEN
                                  ENTRY(wAktivCol,wSearchCols)
                              ELSE wInitSortColNavn.
    
    REPEAT WHILE VALID-HANDLE(wW):
      ASSIGN wLookUp = LOOKUP(wW:NAME,wSearchCols).
      IF wLookUp /* LOOKUP(wW:NAME,wSearchCols) */ > 0 THEN
            ASSIGN wNySearchCols = IF wNySearchCols = "" THEN wW:NAME ELSE
                                      wNySearchCols + "," + wW:NAME
                   wNySokvillkor = IF wNySokvillkor = "" THEN ENTRY(wLookup,wSokvillkor) ELSE
                                      wNySokvillkor + "," + ENTRY(wLookup,wSokvillkor)
                   wNyBrowseIdx = IF wNyBrowseIdx = "" THEN ENTRY(wLookup,wBrowseIdx) ELSE
                                      wNyBrowseIdx + "," + ENTRY(wLookup,wBrowseIdx)
                   wSearchColsH[wCount] = wW:HANDLE
                   wSortCol             = IF wW:NAME = wInitSortColNavn THEN wW:HANDLE ELSE wSortCol
                   wAktivCol            = IF wW:NAME = wInitSortColNavn THEN wCount ELSE wAktivCol
                   wCount               = wCount + 1.
        ASSIGN wW = wW:NEXT-COLUMN.
    END. 
    ASSIGN wSearchCols = wNySearchCols
           wSokvillkor = wNySokvillkor
           wBrowseIdx     = wNyBrowseIdx.
/*
    RUN SortNyCol. 
    */
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
    CASE SUBSTR(wSortCol:DATA-TYPE,1,4):
        WHEN "INTE" OR WHEN "DATE" THEN DO:
            IF KEY-FUNCTION(LASTKEY) < "0" OR KEY-FUNCTION(LASTKEY) > "9" THEN
                RETURN NO-APPLY.
        END.
    END CASE.
    APPLY "ENTRY" TO wAktivFillIn.
    APPLY LASTKEY.
    IF wSortCol:DATA-TYPE = "INTEGER" THEN
        ASSIGN wAktivFillIn:CURSOR-OFFSET = 2.
    /*
    CASE SUBSTR(wSortCol:DATA-TYPE,1,4):
        WHEN "INTE" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-INTE.
            APPLY LASTKEY. 
            FILL-IN-SOK-INTE:CURSOR-OFFSET = 2.
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
    */
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
    def var wW as widget no-undo.
  
    run lockwindowupdate(frame {&FRAME-NAME}:hwnd).

    CASE wLeft-Right:
        WHEN "LEFT" THEN
            ASSIGN wAktivCol = IF wAktivCol = 1 THEN wAntSortCol ELSE wAktivCol - 1.
        WHEN "RIGHT" THEN
            ASSIGN wAktivCol = IF wAktivCol = wAntSortCol THEN 1 ELSE wAktivCol + 1.
    END CASE.
    ASSIGN wSortCol                             = wSearchColsH[wAktivCol]
           BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSortCol.
    RUN SortNyCol.
    
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    
    run lockwindowupdate(0).
    
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Query-Open C-Win 
PROCEDURE SD-Query-Open :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wWhere as char no-undo.
  
  run SettWhereSats (output wWhere).

  assign
    wAktivQString = wSortCol:PRIVATE-DATA
    wAktivQString = REPLACE(wAktivQString, "NO-LOCK", wWhere).
  
  
/*   MESSAGE 'SD-QUERY-OPEN' SKIP                                 */
/*           "where:" wWHERE SKIP                                 */
/*           "wSortCol:PRIVATE-DATA:" wSortCol:PRIVATE-DATA SKIP  */
/*           "Ny query:" wAktivQString                            */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                       */
/*                                                                */

   /* wQ:QUERY-PREPARE(wSortCol:PRIVATE-DATA). */
   wQ:QUERY-PREPARE(wAktivQString).
   wQ:QUERY-OPEN().
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
        if wBlank = ? then
          REPOSITION {&BROWSE-NAME} TO ROWID rowid(b{&br-tabell}) NO-ERROR.
        ASSIGN wAktivFillIn:SCREEN-VALUE = "".
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWhereSats C-Win 
PROCEDURE SettWhereSats :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter wWhere as char no-undo.

  if frame DEFAULT-FRAME:visible = false then
    return.

  FRAME-SCOOP:
  do with frame DEFAULT-FRAME:
  
  /* Bygger where sats */
  assign
    wWhere = (if input CB-Type <> wAlle
               then (if wWhere = "" then "" else "and ") + "Medlem.MedType = " + entry(1,input CB-Type,":")
               else "") 
    wWhere = wWhere + " " +
             (if input CB-Gruppe <> wAlle
               then (if wWhere = "" then "" else "and ") + "Medlem.MedGruppe = " + entry(1,input CB-Gruppe,":")
               else "")
    wWhere = wWhere + " " +
             (if input CB_MKlubbId <> 0
               then (if wWhere = "" then "" else "and ") + "Medlem.MKlubbId = " + QUOTER(CB_MKlubbId:SCREEN-VALUE)
               else "")
    wWhere = wWhere + " " +
             (if input CB-Butikk <> wAlle
               then (if wWhere = "" then "" else "and ") + "Medlem.ButikkNr = " + entry(1,input CB-Butikk,":")
               else "") 
    wWhere = wWhere + " " +
             (if input FI-Navn <> "*"
               then (if wWhere = "" then "" else "and ") + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "Medlem.EtterNavn matches '"
                       else "Medlem.EtterNavn begins '") + 
                     input FI-Navn + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "*"
                       else "") + "'"
               else "")
    wWhere = wWhere + " " + 
              (if input FI-Personnr <> ''
                then (IF wWhere = "" then "" else "and ") + "Medlem.Personnr begins '" + FI-Personnr:SCREEN-VALUE + "' " 
                else "")   
          
    wWhere = wWhere + " " +
             (if input CB-Aktiv = "Aktiv"
               then (if wWhere = "" then "" else "and ") + "Medlem.Aktiv = true"
               else "") +  
             (if input CB-Aktiv = "Passiv"
               then (if wWhere = "" then "" else "and ") + "Medlem.Aktiv = false"
               else "") 
    
    wWhere = if wWhere <> "" then "NO-LOCK where " + wWhere else "".

    
  end. /* FRAME SCOOP */       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpMedlem C-Win 
PROCEDURE SkapaTmpMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wSvar as log no-undo.
  def var wLoop as int no-undo.
    EMPTY TEMP-TABLE tmpMedlem2. /* [ NO-ERROR ] */
    wSvar = false.
    if BROWSE BROWSE-Medlem:NUM-SELECTED-ROWS > 1 then do:  
      message "Skal rapport startes for de merkede medlemmene?"
              view-as alert-box question buttons yes-no title "Bekreft utskrift"
              update wSvar.
      IF wSvar = TRUE THEN DO:
          do wLoop = 1 to BROWSE BROWSE-Medlem:NUM-SELECTED-ROWS:
              IF BROWSE BROWSE-Medlem:FETCH-SELECTED-ROW(wLoop) THEN DO:
                  create tmpMedlem2.
                  ASSIGN tmpMedlem2.MedlemsNr = Medlem.MedlemsNr
                         tmpMedlem2.Ordning   = wLoop.
                  RELEASE tmpMedlem2.
              END.
          end.
          RETURN "TMP".
      END.
      ELSE
        return "AVBRYT".  
    end.
    else do:
        message "Skal utskrift startes for ALLE medlemmer?"
              view-as alert-box question buttons yes-no title "Bekreft utskrift"
              update wSvar.
        RETURN IF wSvar = TRUE THEN "ALLE" ELSE "AVBRYT".  
    end. 

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
    ASSIGN wAktivCol  =  LOOKUP(wSortCol:NAME,wSearchCols)
           BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSortCol.
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK no-error.

    RUN SD-QUERY-OPEN.
    RUN Move-Fill-To-Top.
    RUN LabelColor.
    RUN SD-Reposition.

    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Prep-Private-Data C-Win 
FUNCTION Prep-Private-Data RETURNS CHARACTER
  ( INPUT wQueryCol AS WIDGET,INPUT wQueryCol# AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VAR wQStr      AS CHAR NO-UNDO.
   DEFINE VAR wXSorttype AS CHAR NO-UNDO. 
   DEFINE VAR wXSort     AS CHAR NO-UNDO. 

   ASSIGN wXSorttype = IF ENTRY(wQueryCol#,wSorttype) = "" THEN "BY" ELSE
                          ENTRY(wQueryCol#,wSorttype)
          wXSort     = IF wXSorttype = "BY" THEN
                           "{&br-tabell}." + wQueryCol:Name
                       ELSE IF wXSorttype = "BY DESCENDING" THEN
                           "{&br-tabell}." + wQueryCol:Name + " DESCENDING" 
                       ELSE
                           ENTRY(wQueryCol#,"{&BrowseIdx}")
          wXSorttype = IF wXSorttype = "BY DESCENDING" THEN "BY" ELSE
                              wXSorttype
          wQStr = REPLACE("{&BrowseQ}","XSORTTYPE",wXSorttype)
          wQStr = REPLACE(wQStr,"XSORT",wXSort).
  RETURN wQStr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

