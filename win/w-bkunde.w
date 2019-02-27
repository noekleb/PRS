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
&scope br-tabell   Kunde
&scope Sorttype    ,,,
&scope BrowseIdx   Kunde,Navn,Kontakt,TotalRabatt%
&scope Sokvillkor  >=,>=,>=,>=
&scope InitIdx     Kunde
&scope ip-felt     KundeNr
/* Om du önskar input parameter. Ger en startup-record */ 
/*
&scope ip-variabel w{&ip-felt}
*/

&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.KundeNr) ~
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
DEFINE VAR wKortNummer       AS CHAR        NO-UNDO.
DEFINE VAR wMedlemsNr        AS DEC         NO-UNDO.
DEFINE VAR wAntPoster        AS INT         NO-UNDO.
DEFINE VAR wSkoTex           AS CHAR        NO-UNDO.
DEFINE VAR wKunde            AS CHAR        NO-UNDO.
DEFINE VAR wKriterier        AS CHAR        NO-UNDO.
DEFINE VAR wCl               AS INT         NO-UNDO.
DEFINE VAR dDato             AS DATE        NO-UNDO.
DEFINE VAR lDatoSaldo        AS DEC         NO-UNDO.
DEFINE VAR lDatoTotKjop      AS DEC         NO-UNDO.
DEFINE VAR iType             AS INT         NO-UNDO.

{runlib.i}
{kunde.i &New="New"}
DEFINE TEMP-TABLE tmpKunde2 LIKE Kunde  /* tmpKund opptatt i kundeliste.i */
       FIELD Ordning AS INTE.
DEF TEMP-TABLE expKunde LIKE Kunde
  FIELD Saldo       AS DEC FORMAT ">>>,>>>,>>9.99"
  FIELD Kjop        AS DEC FORMAT ">>>,>>>,>>9.99"
  FIELD DatoSaldo   AS DEC FORMAT ">>>,>>>,>>9.99"
  FIELD DatoTotKjop AS DEC FORMAT ">>>,>>>,>>9.99"
  .

def stream Eksport.
DEF STREAM sExportFile.
{etikettlogg.i &NEW=NEW}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Kunde
&Scoped-define QUERY-NAME QUERY-Alle

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Kunde bKunde tmpKunde2

/* Definitions for BROWSE BROWSE-Kunde                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Kunde Kunde.KundeNr Kunde.ButikkNr ~
Kunde.Navn Kunde.KontNavn Kunde.TotalRabatt% Kunde.KontE-Post ~
Kunde.KontTelefon Kunde.KontMobilTlf wTypeBeskr Kunde.MaksKredit ~
Kunde.Opphort wGruppeBeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Kunde Kunde.KundeNr ~
Kunde.Navn Kunde.KontNavn Kunde.TotalRabatt% 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Kunde Kunde
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Kunde Kunde
&Scoped-define QUERY-STRING-BROWSE-Kunde FOR EACH Kunde NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Kunde OPEN QUERY BROWSE-Kunde FOR EACH Kunde NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Kunde Kunde
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Kunde Kunde


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for QUERY QUERY-Alle                                     */
&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define QUERY-STRING-QUERY-Alle FOR EACH bKunde NO-LOCK BY KundeNr
&Scoped-define OPEN-QUERY-QUERY-Alle OPEN QUERY {&SELF-NAME} FOR EACH bKunde NO-LOCK BY KundeNr.
&Scoped-define TABLES-IN-QUERY-QUERY-Alle bKunde
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Alle bKunde


/* Definitions for QUERY QUERY-tmp                                      */
&Scoped-define SELF-NAME QUERY-tmp
&Scoped-define QUERY-STRING-QUERY-tmp FOR EACH tmpKunde2 NO-LOCK BY tmpKunde2.Ordning
&Scoped-define OPEN-QUERY-QUERY-tmp OPEN QUERY {&SELF-NAME} FOR EACH tmpKunde2 NO-LOCK BY tmpKunde2.Ordning.
&Scoped-define TABLES-IN-QUERY-QUERY-tmp tmpKunde2
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-tmp tmpKunde2


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 RECT-51 RECT-52 FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DATE FILL-IN-SOK-DECI FILL-IN-SOK-INTE BUTTON-Ny BUTTON-Endre ~
BUTTON-Slett B-Print B-Excel B-Rapp B-OppdatSaldo B-Medlem B-Oppdater ~
Btn_Help Btn_OK BUTTON-Sok CB-Type CB-Butikk CB-Gruppe FI-Navn B-EtiMedlem ~
B-Blank2 BROWSE-Kunde 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-CHAR FILL-IN-SOK-DATE ~
FILL-IN-SOK-DECI FILL-IN-SOK-INTE CB-Type CB-Butikk CB-Gruppe FI-Navn 

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
DEFINE BUTTON B-Blank2 
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-EtiMedlem 
     LABEL "Etikett medlemskort" 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel":U
     LABEL "Excel..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksport av kundedato og saldoliste til excel.".

DEFINE BUTTON B-Medlem 
     LABEL "&Opprett medlem..." 
     SIZE 19.4 BY 1.1.

DEFINE BUTTON B-Oppdater 
     LABEL "&Oppdater browser" 
     SIZE 21.4 BY 1.1.

DEFINE BUTTON B-OppdatSaldo 
     LABEL "Oppdater saldo ..." 
     SIZE 17.2 BY 1.1.

DEFINE BUTTON B-Print 
     IMAGE-UP FILE "icon/e-print":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av adresseetiketter".

DEFINE BUTTON B-Rapp 
     LABEL "Htm" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-Statistikk 
     LABEL "Oppdater statistikk ..." 
     SIZE 22 BY 1.1.

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
     SIZE 4.6 BY 1.1 TOOLTIP "Endre kunde (Alt-E)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon/e-ny":U
     LABEL "&Ny.." 
     SIZE 4.6 BY 1.1 TOOLTIP "Opprette ny kunde (Alt-N)".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort kunde (Alt-D)".

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1 TOOLTIP "Søk i AKTIV kollonne".

DEFINE VARIABLE CB-Butikk AS CHARACTER FORMAT "X(256)":U INITIAL "[Alle]" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Gruppe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundegruppe" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundetype" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "[Alle]" 
     DROP-DOWN-LIST
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Navn" 
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

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Kunde FOR 
      Kunde SCROLLING.

DEFINE QUERY QUERY-Alle FOR 
      bKunde SCROLLING.

DEFINE QUERY QUERY-tmp FOR 
      tmpKunde2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Kunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Kunde C-Win _STRUCTURED
  QUERY BROWSE-Kunde NO-LOCK DISPLAY
      Kunde.KundeNr FORMAT ">>>>>>>>>>>>9":U
      Kunde.ButikkNr FORMAT ">>>>>9":U
      Kunde.Navn FORMAT "X(40)":U
      Kunde.KontNavn FORMAT "X(40)":U
      Kunde.TotalRabatt% FORMAT "->9.99":U
      Kunde.KontE-Post FORMAT "X(40)":U
      Kunde.KontTelefon FORMAT "X(15)":U
      Kunde.KontMobilTlf FORMAT "X(15)":U
      wTypeBeskr COLUMN-LABEL "KndType" FORMAT "x(10)":U
      Kunde.MaksKredit FORMAT "->,>>>,>>9.99":U
      Kunde.Opphort FORMAT "99/99/99":U
      wGruppeBeskr COLUMN-LABEL "KndGrp" FORMAT "x(10)":U WIDTH 8.6
  ENABLE
      Kunde.KundeNr
      Kunde.Navn
      Kunde.KontNavn
      Kunde.TotalRabatt%
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN MULTIPLE SIZE 155 BY 17.86 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DECI AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-INTE AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Ny AT ROW 1.48 COL 31.8
     BUTTON-Endre AT ROW 1.48 COL 36.4
     BUTTON-Slett AT ROW 1.48 COL 41
     B-Print AT ROW 1.48 COL 45.6
     B-Excel AT ROW 1.48 COL 50.2
     B-Rapp AT ROW 1.48 COL 54.8
     B-Statistikk AT ROW 1.48 COL 65.4
     B-OppdatSaldo AT ROW 1.48 COL 87.8
     B-Medlem AT ROW 1.48 COL 105.4
     B-Oppdater AT ROW 1.48 COL 125
     Btn_Help AT ROW 1.48 COL 147
     Btn_OK AT ROW 1.48 COL 152
     BUTTON-Sok AT ROW 1.52 COL 21.4
     CB-Type AT ROW 3.14 COL 16 COLON-ALIGNED
     CB-Butikk AT ROW 3.14 COL 96 COLON-ALIGNED
     CB-Gruppe AT ROW 4.33 COL 16 COLON-ALIGNED
     FI-Navn AT ROW 5.52 COL 16 COLON-ALIGNED
     B-EtiMedlem AT ROW 5.52 COL 94
     B-Blank2 AT ROW 5.57 COL 81.8
     BROWSE-Kunde AT ROW 6.95 COL 2
     RECT-50 AT ROW 1 COL 1
     RECT-51 AT ROW 1.24 COL 1
     RECT-52 AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157 BY 24.


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
         TITLE              = "Søkeliste kunder"
         HEIGHT             = 23.91
         WIDTH              = 157
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

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-Kunde B-Blank2 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Statistikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Statistikk:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BROWSE-Kunde:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 2
       BROWSE-Kunde:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 481
       BROWSE-Kunde:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Kunde
/* Query rebuild information for BROWSE BROWSE-Kunde
     _TblList          = "SkoTex.Kunde"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > SkoTex.Kunde.KundeNr
"Kunde.KundeNr" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = SkoTex.Kunde.ButikkNr
     _FldNameList[3]   > SkoTex.Kunde.Navn
"Kunde.Navn" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > SkoTex.Kunde.KontNavn
"Kunde.KontNavn" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > SkoTex.Kunde.TotalRabatt%
"Kunde.TotalRabatt%" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = SkoTex.Kunde.KontE-Post
     _FldNameList[7]   = SkoTex.Kunde.KontTelefon
     _FldNameList[8]   = SkoTex.Kunde.KontMobilTlf
     _FldNameList[9]   > "_<CALC>"
"wTypeBeskr" "KndType" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   = SkoTex.Kunde.MaksKredit
     _FldNameList[11]   = SkoTex.Kunde.Opphort
     _FldNameList[12]   > "_<CALC>"
"wGruppeBeskr" "KndGrp" "x(10)" ? ? ? ? ? ? ? no ? no no "8.6" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-Kunde */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Alle
/* Query rebuild information for QUERY QUERY-Alle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH bKunde NO-LOCK BY KundeNr.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 2.91 , 152 )
*/  /* QUERY QUERY-Alle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-tmp
/* Query rebuild information for QUERY QUERY-tmp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tmpKunde2 NO-LOCK BY tmpKunde2.Ordning.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 4.1 , 152 )
*/  /* QUERY QUERY-tmp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søkeliste kunder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søkeliste kunder */
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


&Scoped-define SELF-NAME B-EtiMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EtiMedlem C-Win
ON CHOOSE OF B-EtiMedlem IN FRAME DEFAULT-FRAME /* Etikett medlemskort */
DO:
    DEFINE VARIABLE iPrintervalg AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntall AS INTEGER  INIT 1  NO-UNDO.
    DEFINE VARIABLE iAntkort AS INTEGER    NO-UNDO.
    RUN PopulateEtikettLogg (kunde.kundenr,OUTPUT iAntkort).
    IF iAntKort = 0 THEN DO:
        MESSAGE "Inget medlemkort finnes"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    MESSAGE "Utskrift medlemskort for kunde " kunde.navn "(Antall kort: " iAntKort ") 'J/N'"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lUt AS LOGICAL.
    IF NOT lUt THEN
        RETURN NO-APPLY.
    IF CAN-FIND(FIRST Etikettlogg) THEN DO:
        RUN d-skrivervalgmedlem.w (OUTPUT iPrinterValg,INPUT-OUTPUT iAntall).
        IF iPrinterValg > 0 AND iAntall > 0 THEN DO:
            RUN x-etikettsend.w (iPrinterValg).
            EMPTY TEMP-TABLE EtikettLogg.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Excel... */
DO:
  RUN kundeexcelexport.w (OUTPUT iType, OUTPUT dDato).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  /*RUN ExHtmRapp ("EX").*/
  CASE iType:
      WHEN 1 THEN RUN EksportKundeData.
      WHEN 2 THEN RUN EksportKundeDataDato.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Medlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Medlem C-Win
ON CHOOSE OF B-Medlem IN FRAME DEFAULT-FRAME /* Opprett medlem... */
DO:
  IF NOT AVAILABLE Kunde THEN
      RETURN NO-APPLY.
  RUN d-kortnummer.w.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ELSE ASSIGN
      wKortNummer = RETURN-VALUE.

  RUN opprettmedlem.p (Kunde.KundeNr, wKortNummer, wCl, OUTPUT wMedlemsNr).
  IF RETURN-VALUE = "AVBRYT" THEN
  DO:
      MESSAGE "Feil ved opprettelse av medlem. Rutine er avbrudt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Feil ved opprettelse av medlem".
      RETURN NO-APPLY.
  END.
  FIND medlem NO-LOCK WHERE
      Medlem.MedlemsNr = wMedlemsNr.

  CREATE tmpChild.
  run w-vmedlem PERSISTENT SET tmpChild.wChild (recid(Medlem),"ENDRE").
  RELEASE tmpChild.

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


&Scoped-define SELF-NAME B-OppdatSaldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdatSaldo C-Win
ON CHOOSE OF B-OppdatSaldo IN FRAME DEFAULT-FRAME /* Oppdater saldo ... */
DO:
  RUN OppdaterSaldo.
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
  for each tmpKunde:
    delete tmpKunde.
  end.
  
  wSvar = false.
  /* Bruker har valgt en eller flere linjer. */
  if BROWSE-Kunde:NUM-SELECTED-ROWS > 0 then
  do TRANSACTION:  
    message "Skal utskrift startes for de merkede kundene?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    if wSvar = false then 
      return no-apply.  

    {sww.i}
    do wLoop = 1 to BROWSE-Kunde:NUM-SELECTED-ROWS:
      wOk = BROWSE-Kunde:FETCH-SELECTED-ROW(wLoop).
      if not can-find(first tmpKunde where
        tmpKunde.KundeRecid = recid(Kunde)) then
        do:
          create tmpKunde.
          assign
            tmpKunde.KundeRecid = recid(Kunde).
        end.
    end.
    {swn.i}
  end.
  
  /* Ingen linjer er valgt. Alle linjer skal behandles. Bruker må bekrefte */
  else do:
    message "Skal utskrift startes for ALLE kunder?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    if wSvar = false then 
      return no-apply.  
    {sww.i}
    do:
      for each Kunde no-lock:
        if not can-find(first tmpKunde where
          tmpKunde.KundeRecid = recid(Kunde)) then
          do:
            create tmpKunde.
            assign
              tmpKunde.KundeRecid = recid(Kunde).
          end.
      end.  
    end.
    {swn.i}
  end. 
  
  if can-find(first tmpKunde) then
    run kundeliste.p (300,3).
  
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


&Scoped-define SELF-NAME B-Statistikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Statistikk C-Win
ON CHOOSE OF B-Statistikk IN FRAME DEFAULT-FRAME /* Oppdater statistikk ... */
DO:
  RUN OppdaterStatistikk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Kunde
&Scoped-define SELF-NAME BROWSE-Kunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Kunde C-Win
ON ANY-PRINTABLE OF BROWSE-Kunde IN FRAME DEFAULT-FRAME
DO:
  if lastkey <> 32 then
    do:
      RUN SD-ANY-PRINTABLE.
      RETURN NO-APPLY.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Kunde C-Win
ON CURSOR-LEFT OF BROWSE-Kunde IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Kunde C-Win
ON CURSOR-RIGHT OF BROWSE-Kunde IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Kunde C-Win
ON DEFAULT-ACTION OF BROWSE-Kunde IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Endre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Kunde C-Win
ON HOME OF BROWSE-Kunde IN FRAME DEFAULT-FRAME
DO:
  APPLY "ENTRY" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Kunde C-Win
ON ROW-DISPLAY OF BROWSE-Kunde IN FRAME DEFAULT-FRAME
DO:
  if available Kunde then
    do:
      find KundeType of Kunde no-lock no-error.
      find KundeGruppe of Kunde no-lock no-error.
      assign
        wTypeBeskr   = if available KundeType then KundeType.Beskrivelse else ""
        wGruppeBeskr = if available KundeGruppe then KundeGruppe.Beskrivelse else "".
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Kunde C-Win
ON START-SEARCH OF BROWSE-Kunde IN FRAME DEFAULT-FRAME
DO:
  RUN lockwindowupdate(FRAME {&FRAME-NAME}:HWND).
  DEF VAR wSearchCol  AS WIDGET NO-UNDO.
  DEF VAR wQString    AS CHAR   NO-UNDO.
  DEF VAR wSortColIdx AS INTE   NO-UNDO.
  ASSIGN wSearchCol = SELF:CURRENT-COLUMN.
  IF wSortCol <> SELF:CURRENT-COLUMN AND
                   LOOKUP(wSearchCol:NAME,wSearchCols) > 0 THEN DO:
      ASSIGN wSortCol = SELF:CURRENT-COLUMN.
      RUN SortNyCol.
  END.
  ELSE IF LOOKUP(wSearchCol:NAME,wSearchCols) > 0 AND
          LOOKUP("USE-INDEX",wQ:PREPARE-STRING," ") = 0 THEN DO:
      ASSIGN wQString = wQ:PREPARE-STRING
             wSortColIdx = LOOKUP("{&br-tabell}." + wSearchCol:NAME,wQString," ")
             wQString = IF ENTRY(wSortColIdx + 1,wQString," ") = "DESCENDING" THEN
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME + " DESCENDING","{&br-tabell}." + wSearchCol:NAME)
                        ELSE
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME, 
                           "{&br-tabell}." + wSearchCol:NAME + " DESCENDING")
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
  IF NOT AVAILABLE Kunde THEN
      RETURN NO-APPLY.
  DEF VAR wTmpChild AS HANDLE NO-UNDO.
  run w-vkunde PERSISTENT SET wTmpChild (recid({&br-tabell}),"ENDRE").
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
  run w-vkunde PERSISTENT SET wTmpChild (?,"Ny").
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
  IF NOT AVAILABLE Kunde THEN
      RETURN NO-APPLY.
  def var wCurrRow as inte no-undo.
  IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS > 0 AND 
     BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN DO:
      BROWSE {&BROWSE-NAME}:SCROLL-TO-SELECTED-ROW(1).
  END.
  IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS <> 1 THEN DO:
     IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 0 THEN
         MESSAGE "Du må velge 1 kunde." VIEW-AS ALERT-BOX INFORMATION
                      TITLE "Feil".
     ELSE DO:
         MESSAGE "Du kan bare velge 1 kunde." SKIP
                 "Vil du rense ditt utvalg?" VIEW-AS ALERT-BOX QUESTION
                 BUTTONS YES-NO TITLE "Feil" UPDATE wJa AS LOGI.
         IF wJa THEN
             BROWSE {&BROWSE-NAME}:DESELECT-ROWS().
         REPOSITION {&BROWSE-NAME} TO ROW 1.
     END.
     RETURN NO-APPLY.
  END.
  ASSIGN wCurrRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW.
  run w-vkunde (recid({&br-tabell}),"SLETT").
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
ON VALUE-CHANGED OF CB-Gruppe IN FRAME DEFAULT-FRAME /* Kundegruppe */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Type C-Win
ON VALUE-CHANGED OF CB-Type IN FRAME DEFAULT-FRAME /* Kundetype */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Navn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Navn C-Win
ON TAB OF FI-Navn IN FRAME DEFAULT-FRAME /* Navn */
or "RETURN" of FI-Navn
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
  &WindowName     = "Søkeliste Kunder"
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

{syspara.i 5  1 1 wCl INT}     /* Sentrallager. */

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

on ALT-S of {&WINDOW-NAME} ANYWHERE
  do: /* SOKTRIGGER */
    RUN d-kundesok.w.
    IF RETURN-VALUE = "AVBRYT" THEN 
        RETURN NO-APPLY.
    ELSE DO:
      FIND bKunde NO-LOCK WHERE
        bKunde.KundeNr = DEC(RETURN-VALUE) NO-ERROR.
      if available bKunde then
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
        find KundeType of Kunde no-lock no-error.
        find KundeGruppe of Kunde no-lock no-error.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggExpKunde C-Win 
PROCEDURE ByggExpKunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wSvar as log no-undo.
  def var wLoop as int no-undo.

  EMPTY TEMP-TABLE expKunde. /* [ NO-ERROR ] */
  wSvar = false.
  /* Kun merkede kunde skal ut. */
  if BROWSE BROWSE-Kunde:NUM-SELECTED-ROWS > 0 then do:  
    message "Skal eksporten startes for de merkede kundene?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    IF wSvar = TRUE THEN DO:
        ASSIGN
          wKriterier = "Export av merkede kundene".
        do wLoop = 1 to BROWSE BROWSE-Kunde:NUM-SELECTED-ROWS:
            IF BROWSE BROWSE-Kunde:FETCH-SELECTED-ROW(wLoop) THEN DO:
                create expKunde.
                BUFFER-COPY Kunde TO expKunde.

                FOR EACH KundeSaldo NO-LOCK WHERE
                  KundeSaldo.KundeNr = expKunde.KundeNr:
                  ASSIGN
                    expKunde.Kjop  = expKunde.Kjop  + KundeSaldo.TotaltKjop
                    expKunde.Saldo = expKunde.Saldo + KundeSaldo.Saldo
                    .
                END.

                IF dDato <> ? THEN
                DATOSALDO:
                FOR EACH Butiker NO-LOCK:
                    RUN beregnkundesaldopadato.p (Kunde.KundeNr,
                                                  Butiker.Butik,
                                                  dDato,
                                                  OUTPUT lDatoSaldo,
                                                  OUTPUT lDatoTotKjop).
                    ASSIGN
                        expKunde.DatoSaldo   = expKunde.DatoSaldo   + lDatoSaldo
                        expKunde.DatoTotKjop = expKunde.DatoTotKjop + lDatoTotKjop
                        .
                END. /* DATOSALDO */
                RELEASE expKunde.
            END.
        end.
        RETURN "TMP".
    END.
    ELSE
      return "AVBRYT".  
  end.
  /* Alle kunder skal ut */
  else do:
      message "Skal utskrift startes for ALLE kunder?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.

      IF wSvar = FALSE THEN
          RETURN "AVBRYT".  
                                              
      ASSIGN
        wKriterier = "Export av alle kunder".
      FOR EACH Kunde NO-LOCK:
        create expKunde.
        BUFFER-COPY Kunde TO expKunde.
        FOR EACH KundeSaldo NO-LOCK WHERE
          KundeSaldo.KundeNr = expKunde.KundeNr:
          ASSIGN
            expKunde.Kjop  = expKunde.Kjop  + KundeSaldo.TotaltKjop
            expKunde.Saldo = expKunde.Saldo + KundeSaldo.Saldo
            .
        END.
        
        IF dDato <> ? THEN
        DATOSALDO:
        FOR EACH Butiker NO-LOCK:
            RUN beregnkundesaldopadato.p (Kunde.KundeNr,
                                          Butiker.Butik,
                                          dDato,
                                          OUTPUT lDatoSaldo,
                                          OUTPUT lDatoTotKjop).
            ASSIGN
                expKunde.DatoSaldo   = expKunde.DatoSaldo   + lDatoSaldo
                expKunde.DatoTotKjop = expKunde.DatoTotKjop + lDatoTotKjop
                .
        END. /* DATOSALDO */

        RELEASE expKunde.
      END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportKundeData C-Win 
PROCEDURE EksportKundeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR wFileName AS CHAR NO-UNDO.

  DEF BUFFER b1Medlem FOR Medlem.
  DEF BUFFER b1Kunde  FOR Kunde.
  DEF BUFFER b1Post   FOR Post.
  
  {sww.i}
  
  /* Bygger datasett som skal eksporteres */
  RUN ByggExpKunde.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN "AVBRYT".

  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("Kunde", wExcEkstent, output wFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(wFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "KUNDEDATA"
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */  "KundeNr"        
    /* B  */  "Navn"           
    /* C  */  "OrgNr"          
    /* AN */  "TotalRabatt%"
    /* D  */  "TypeId"         
    /* E  */  "Beskrivelse"           
    /* F  */  "GruppeId"       
    /* G  */  "Beskrivelse"           
    /* H  */  "Adresse1"       
    /* I  */  "Adresse2"       
    /* J  */  "PostNr"         
    /* K  */  "Poststed"              
    /* L  */  "Telefon"        
    /* M  */  "Telefaks"       
    /* N  */  "MobilTlf"       
    /* O  */  "KontNavn"       
    /* P  */  "KontTelefon"    
    /* Q  */  "Stilling"       
    /* R  */  "KontTelefaks"   
    /* S  */  "KontMobilTlf"   
    /* T  */  "LevAdresse1"    
    /* U  */  "LevAdresse2"    
    /* V  */  "LevPostNr"      
    /* W  */  "Poststed"              
    /* X  */  "LevLand"        
    /* Y  */  "Land"           
    /* S  */  "MaksKredit"     
    /* AA */  "KreditSperret"  
    /* AB */  "Utestående saldo" 
    /* AC */  "Totalt kjøp" 
    /* AD */  "Opphort"        
    /* AE */  "ButikkNr"       
    /* AF */  "BydelsNr"       
    /* AG */  "ePostAdresse"   
    /* AH */  "KontE-Post"     
    /* AI */  "BetType"        
    /* AJ */  "EDato"          
    /* AK */  "ETid"           
    /* AL */  "BrukerID"       
    /* AM */  "RegistrertDato"
    /* AN */  "RegistrertTid" 
    /* AO */  "RegistrertAv"  

    SKIP.                                 
                                  
  /* Nullstiller */
  ASSIGN
    wAntPoster = 0.

  /* Eksporterer data */
  EKSPORT:
  FOR EACH expKunde no-lock
    BY expKunde.KundeNr:
                    
    /* Henter relaterte data */
    FIND Kunde NO-LOCK WHERE
         Kunde.KundeNr = expKunde.KundeNr NO-ERROR.
    FIND Post          NO-LOCK WHERE Post.PostNr = Kunde.PostNr    NO-ERROR.
    FIND b1Post        NO-LOCK WHERE Post.PostNr = Kunde.LevPostNr NO-ERROR.
    FIND KundeType     OF Kunde NO-LOCK NO-ERROR.
    FIND KundeGruppe   OF Kunde NO-LOCK NO-ERROR.

    EXPORT STREAM sExportFile DELIMITER ";"
        expKunde.KundeNr        
        expKunde.Navn           
        expKunde.OrgNr          
        expKunde.TotalRabatt%
        expKunde.TypeId         
        (IF AVAILABLE KundeType 
           THEN KundeType.Beskrivelse
           ELSE "")           
        expKunde.GruppeId       
        (IF AVAILABLE KundeGruppe
           THEN KundeGruppe.Beskrivelse
           ELSE "")           
        expKunde.Adresse1       
        expKunde.Adresse2       
        expKunde.PostNr         
        (IF AVAILABLE Post
           THEN Post.Beskrivelse
           ELSE "")              
        expKunde.Telefon        
        expKunde.Telefaks       
        expKunde.MobilTlf       
        expKunde.KontNavn       
        expKunde.KontTelefon    
        expKunde.Stilling       
        expKunde.KontTelefaks   
        expKunde.KontMobilTlf   
        expKunde.LevAdresse1    
        expKunde.LevAdresse2    
        expKunde.LevPostNr      
        (IF AVAILABLE b1Post
           THEN b1Post.Beskrivelse
           ELSE "")              
        expKunde.LevLand        
        expKunde.Land           
        expKunde.MaksKredit     
        expKunde.KreditSperret  
        expKunde.Saldo
        expKunde.Kjop
        (IF expKunde.Opphort <> ?
           THEN string(expKunde.Opphort)
           ELSE "")
        expKunde.ButikkNr       
        expKunde.BydelsNr       
        expKunde.ePostAdresse   
        expKunde.KontE-Post     
        expKunde.BetType        
        expKunde.EDato          
        string(expKunde.ETid,"HH:MM:SS")           
        expKunde.BrukerID       
        expKunde.RegistrertDato 
        string(expKunde.RegistrertTid,"HH:MM:SS")  
        expKunde.RegistrertAv   
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
  chWorkSheets:Range("A1:AP2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:AP2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("C:C"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("F:F"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("L:L"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("O:O"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  
  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("AC:AC"):NumberFormat = "# ##0,00".
  chWorkSheets:Range("AD:AD"):NumberFormat = "# ##0,00".
  /*
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  chWorkSheets:Range("H:H"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("I:I"):NumberFormat = "# ##0".
  chWorkSheets:Range("J:J"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("K:K"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("L:L"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("M:M"):NumberFormat = "# ##0,00".
  */
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:AP1"):Merge().
  chWorkSheets:Range("A1:AP1"):HorizontalAlignment = 3.
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
  chWorkSheets:Columns("A:AP"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:AP2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier: " + wKriterier. 
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(wAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = wKunde.
  chWorkSheets:PageSetup:RightFooter    = wSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:AP".
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

  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportKundeDataDato C-Win 
PROCEDURE EksportKundeDataDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR wFileName AS CHAR NO-UNDO.

  DEF BUFFER b1Medlem FOR Medlem.
  DEF BUFFER b1Kunde  FOR Kunde.
  DEF BUFFER b1Post   FOR Post.
  
  {sww.i}
  
  /* Bygger datasett som skal eksporteres */
  RUN ByggExpKunde.
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN "AVBRYT".

  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("Kunde", wExcEkstent, output wFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(wFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "SALDOLISTE"
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */  "KundeNr"        
    /* B  */  "Navn"           
    /* D  */  "Adresse1"       
    /* E  */  "Adresse2"       
    /* F  */  "PostNr"         
    /* G  */  "Poststed"              
    /* H  */  "Telefon"        
    /* I  */  "Telefaks"       
    /* J  */  "MobilTlf"       
    /* K  */  "ePostAdresse"   
    /* L  */  "Saldo pr. " + STRING(dDato) 
    /* M  */  "Totalt kjøp pr. " + STRING(dDato) 

    SKIP.                                 
                                  
  /* Nullstiller */
  ASSIGN
    wAntPoster = 0.

  /* Eksporterer data */
  EKSPORT:
  FOR EACH expKunde no-lock WHERE
       expKunde.DatoSaldo <> 0
    BY expKunde.KundeNr:
                    
    /* Henter relaterte data */
    FIND Kunde NO-LOCK WHERE
         Kunde.KundeNr = expKunde.KundeNr NO-ERROR.
    FIND Post          NO-LOCK WHERE Post.PostNr = Kunde.PostNr    NO-ERROR.
    FIND b1Post        NO-LOCK WHERE Post.PostNr = Kunde.LevPostNr NO-ERROR.
    FIND KundeType     OF Kunde NO-LOCK NO-ERROR.
    FIND KundeGruppe   OF Kunde NO-LOCK NO-ERROR.

    EXPORT STREAM sExportFile DELIMITER ";"
        expKunde.KundeNr        
        expKunde.Navn           
        expKunde.Adresse1       
        expKunde.Adresse2       
        expKunde.PostNr         
        (IF AVAILABLE Post
           THEN Post.Beskrivelse
           ELSE "")              
        expKunde.Telefon        
        expKunde.Telefaks       
        expKunde.MobilTlf       
        expKunde.ePostAdresse   
        expKunde.DatoSaldo
        expKunde.DatoTotKjop
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
  chWorkSheets:Range("A1:M2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:M2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("B:B"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("K:K"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  
  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("K:M"):NumberFormat = "# ##0,00".
  
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:M1"):Merge().
  chWorkSheets:Range("A1:M1"):HorizontalAlignment = 3.
  
  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:M"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:M2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier: " + wKriterier. 
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(wAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = wKunde.
  chWorkSheets:PageSetup:RightFooter    = wSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:M".
  chWorkSheets:PageSetup:Orientation    = 2.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("B3"):Select().
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

  {swn.i}

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
  DISPLAY FILL-IN-SOK-CHAR FILL-IN-SOK-DATE FILL-IN-SOK-DECI FILL-IN-SOK-INTE 
          CB-Type CB-Butikk CB-Gruppe FI-Navn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-50 RECT-51 RECT-52 FILL-IN-SOK-CHAR FILL-IN-SOK-DATE 
         FILL-IN-SOK-DECI FILL-IN-SOK-INTE BUTTON-Ny BUTTON-Endre BUTTON-Slett 
         B-Print B-Excel B-Rapp B-OppdatSaldo B-Medlem B-Oppdater Btn_Help 
         Btn_OK BUTTON-Sok CB-Type CB-Butikk CB-Gruppe FI-Navn B-EtiMedlem 
         B-Blank2 BROWSE-Kunde 
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
  DEFINE VAR hKund        AS HANDLE NO-UNDO.
  DEFINE VAR wLblHdl      AS HANDLE NO-UNDO.
  DEFINE VAR wTotWidthC   AS DECI NO-UNDO.
  DEFINE VAR wColWidthC   AS CHAR NO-UNDO.
  DEFINE VAR wCount       AS INTE NO-UNDO.
  RUN SkapaTmpKunde.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  {sww.i}
 DO WITH FRAME {&FRAME-NAME}:
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle (wDokTyp, IF wDokTyp = "HTM" THEN "HTM" ELSE wExcEkstent, output wtmpFileName). 
  IF RETURN-VALUE = "TMP" THEN DO:
     ASSIGN hQuery = QUERY QUERY-tmp:HANDLE.
     OPEN QUERY QUERY-tmp FOR EACH tmpKunde2 NO-LOCK BY tmpKunde2.Ordning.
  END.
  ELSE DO:
      ASSIGN hQuery = QUERY QUERY-Alle:HANDLE
             hKund = {&BROWSE-NAME}:QUERY.
      hQuery:QUERY-PREPARE(REPLACE(hKund:PREPARE-STRING," Kunde"," bKunde")).
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
  ASSIGN wTabell = "Kund"
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

  for each KundeGruppe no-lock where KundeGruppe.GruppeId > 0:
    assign
      wGrupper = wGrupper + 
                      (if wGrupper = ""
                         then ""
                         else ",") + string(KundeGruppe.GruppeId,"9999") + ": " + KundeGruppe.Beskrivelse.
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
  for each KundeType no-lock:
    
    assign
      wTypeLst = wTypeLst + 
                   (if wTypeLst = "" 
                      then ""
                      else ",") +
                   string(KundeType.TypeId,"zzz9") + ": " + KundeType.Beskrivelse.    
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
  DEF BUFFER bKunde FOR Kunde.

  DEF VAR pbOk AS LOG NO-UNDO.

  pbOk = FALSE.
  MESSAGE "Skal oppdatering av kundesaldo starte?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
      UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  FOR EACH bKunde NO-LOCK:
    STATUS DEFAULT "Oppdaterer saldo på kunde " + 
                   STRING(bKunde.KundeNr) + " " + bKunde.Navn.
    RUN beregnkundesaldo(bKunde.kundeNr, 0).
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
  DEF BUFFER bKunde FOR Kunde.

  DEF VAR pbOk AS LOG NO-UNDO.

  pbOk = FALSE.
  MESSAGE "Skal oppdatering av kundestatistikk starte?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
      UPDATE pbOk.
  IF pbOk = FALSE THEN
      RETURN.

  FOR EACH bKunde NO-LOCK:
    STATUS DEFAULT "Oppdaterer statistikk på kunde " + 
                   STRING(bKunde.KundeNr) + " " + bKunde.Navn.
    RUN oppdatkundestat.p(bKunde.KundeNr).
  END.
  STATUS DEFAULT "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateEtikettLogg C-Win 
PROCEDURE PopulateEtikettLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iKundeNr AS INTEGER    NO-UNDO.
    DEFINE OUTPUT PARAMETER iAntKort AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cEAN AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEANprefix AS CHARACTER INIT "29" NO-UNDO.
    DEFINE VARIABLE dKortNummer AS DECIMAL DECIMALS 0 NO-UNDO.
    DEFINE VARIABLE iAntall AS INTEGER INIT 1   NO-UNDO.
    EMPTY TEMP-TABLE Etikettlogg.
    FOR EACH medlem WHERE Medlem.kundenr = iKundeNr AND Medlem.Opphort = ? OR Medlem.Opphort > TODAY NO-LOCK.
        FIND Butiker WHERE Butiker.butik = Medlem.ButikkNr NO-LOCK NO-ERROR.
        FOR EACH medlemskort OF medlem WHERE MedlemsKort.Sperret = FALSE AND
            (MedlemsKort.UtgarDato = ? OR MedlemsKort.UtgarDato > TODAY):
            ASSIGN dKortNummer = DECI(Medlemskort.Kortnr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            ASSIGN cEAN = DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib,
                     cEANprefix + STRING(dKortNummer,"9999999999")).
           
           CREATE EtikettLogg.
           ASSIGN EtikettLogg.storl = cEan
                  EtikettLogg.bongtekst = "Medlem|" + REPLACE(SUBSTR(TRIM(MedlemsKort.Innehaver),1,25),"|"," ") + "|" +
                    (IF AVAIL Butiker THEN REPLACE(SUBSTR(TRIM(Butiker.butnamn),1,25),"|"," ") ELSE "") + "|" +
                    IF MedlemsKort.UtgarDato = ? THEN "" ELSE STRING(MedlemsKort.UtgarDato,"99/99/99")
                  EtikettLogg.Ant       = iAntall

                  iAntKort = iAntkort + 1.
        END.
    END.

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
  /*
  MESSAGE "where:" wWHERE SKIP
          "wSortCol:PRIVATE-DATA:" wSortCol:PRIVATE-DATA SKIP
          "Ny query:" wAktivQString
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   */

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
               then (if wWhere = "" then "" else "and ") + "Kunde.TypeId = " + entry(1,input CB-Type,":")
               else "") 
    wWhere = wWhere + " " +
             (if input CB-Gruppe <> wAlle
               then (if wWhere = "" then "" else "and ") + "Kunde.GruppeId = " + entry(1,input CB-Gruppe,":")
               else "")
    wWhere = wWhere + " " +
               (if input CB-Butikk <> wAlle
                 then (if wWhere = "" then "" else "and ") + "Kunde.ButikkNr = " + entry(1,input CB-Butikk,":")
                 else "") 
    wWhere = wWhere + " " +
             (if input FI-Navn <> "*"
               then (if wWhere = "" then "" else "and ") + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "Kunde.Navn matches '"
                       else "Kunde.Navn begins '") + 
                     input FI-Navn + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "*"
                       else "") + "'"
               else "")
    wWhere = if wWhere <> "" then "NO-LOCK where " + wWhere else "".
    
  end. /* FRAME SCOOP */       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpKunde C-Win 
PROCEDURE SkapaTmpKunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wSvar as log no-undo.
  def var wLoop as int no-undo.
    EMPTY TEMP-TABLE tmpKunde2. /* [ NO-ERROR ] */
    wSvar = false.
    if BROWSE BROWSE-Kunde:NUM-SELECTED-ROWS > 1 then do:  
      message "Skal rapport startes for de merkede kundene?"
              view-as alert-box question buttons yes-no title "Bekreft utskrift"
              update wSvar.
      IF wSvar = TRUE THEN DO:
          do wLoop = 1 to BROWSE BROWSE-Kunde:NUM-SELECTED-ROWS:
              IF BROWSE BROWSE-Kunde:FETCH-SELECTED-ROW(wLoop) THEN DO:
                  create tmpKunde2.
                  ASSIGN tmpKunde2.Kundenr  = Kunde.KundeNr
                         tmpKunde2.Ordning = wLoop.
                  RELEASE tmpKunde2.
              END.
          end.
          RETURN "TMP".
      END.
      ELSE
        return "AVBRYT".  
    end.
    else do:
        message "Skal utskrift startes for ALLE kunder?"
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

