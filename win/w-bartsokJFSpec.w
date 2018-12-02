&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Artikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Artikkel 
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
def var wVgNr     as int no-undo.
def var wLoop     as int no-undo.
def var wLevNr    as int no-undo.
def var wSasong   as int no-undo.
def var wOk       as log no-undo.
def var wSasBeskr as char no-undo.
def var wBekreft  as log no-undo.
DEFINE VARIABLE hJmfRutine AS HANDLE     NO-UNDO.
/* Buffere */
def buffer bArtBas for ArtBas.
def temp-table tmpChild
  field wChild as handle.

{runlib.i} /* Starter procedurebibloteket. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Artbas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES artbas BestHode

/* Definitions for BROWSE BROWSE-Artbas                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Artbas ArtBas.ArtikkelNr artbas.vg ArtBas.LopNr ArtBas.VgKat wSasBeskr artbas.levnr artbas.levkod ArtBas.Farg ArtBas.MatKod ArtBas.LevFargKod artbas.beskr artbas.bildnr artbas.ny_dato ArtBas.EDato "Endret" artbas.inn_dato   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Artbas ArtBas.Beskr   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Artbas ArtBas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Artbas ArtBas
&Scoped-define SELF-NAME BROWSE-Artbas
&Scoped-define OPEN-QUERY-BROWSE-Artbas assign   wVgNr = (if lookup(COMBO-BOX-Varegruppe:screen-value, ~
      COMBO-BOX-VareGruppe:List-items) = 1             then ?             else int(entry(1, ~
      COMBO-BOX-Varegruppe:screen-value, ~
      " ")))   wSasong = (if lookup(COMBO-BOX-Sasong:screen-value, ~
      COMBO-BOX-Sasong:List-items) = 1               then ?               else int(substring(COMBO-BOX-Sasong:screen-value, ~
      1, ~
      4)))   wLoop =  lookup(COMBO-BOX-Sort:screen-value, ~
      COMBO-BOX-Sort:list-items)   wLevNr = INPUT FI-LevNr   wLevNr = (IF wLevNr = 0              THEN ?              ELSE wLevNr)   T-Annonse.  case wLoop:    when 1 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.Vg       by Artbas.LopNr       by ArtBas.Beskr       by ArtBas.LevNr       INDEXED-REPOSITION.    when 2 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.Vg       by Artbas.Beskr       by ArtBas.LevNr       by ArtBas.LevKod       INDEXED-REPOSITION.    when 3 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.Vg       by Artbas.LevNr       by ArtBas.LevKod       by ArtBas.Beskr       INDEXED-REPOSITION.    when 4 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.LevNr       by Artbas.LevKod       by ArtBas.Vg       by ArtBas.Beskr       INDEXED-REPOSITION.    when 5 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.LevKod       INDEXED-REPOSITION.    when 6 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.ArtikkelNr       INDEXED-REPOSITION.    when 7 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.RegistrertDato       by ArtBas.ArtikkelNr       INDEXED-REPOSITION.    when 8 then       OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where       (if wVgNr = ?          then true        else Artbas.Vg = wVgNr) and       ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and       (if wLevNr = ?          then true        else ArtBas.LevNr = wLevNr) and       (if wSasong = ?          then true        else Artbas.Sasong = wSasong) and       (if T-Annonse = false         then true        else ArtBas.AnonseArtikkel = true)       by Artbas.EDato       by ArtBas.ArtikkelNr       INDEXED-REPOSITION. end case.
&Scoped-define TABLES-IN-QUERY-BROWSE-Artbas artbas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Artbas artbas


/* Definitions for BROWSE BROWSE-Ordre                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Ordre BestHode.BestNr ~
BestHode.LevDato BestHode.BestStat BestHode.BestillingsDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Ordre 
&Scoped-define QUERY-STRING-BROWSE-Ordre FOR EACH BestHode ~
      WHERE BestHode.ArtikkelNr = (if available ArtBas ~
                         then ArtBas.ArtikkelNr ~
                         else -99) NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Ordre OPEN QUERY BROWSE-Ordre FOR EACH BestHode ~
      WHERE BestHode.ArtikkelNr = (if available ArtBas ~
                         then ArtBas.ArtikkelNr ~
                         else -99) NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Ordre BestHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Ordre BestHode


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-Artbas}~
    ~{&OPEN-QUERY-BROWSE-Ordre}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Vg FI-LopNr FI-ArtikkelNr COMBO-BOX-Sort ~
T-Annonse FI-LevNr COMBO-BOX-Varegruppe BUTTON-SokLev COMBO-BOX-Sasong ~
FILL-IN-ArtFilter BUTTON-Blank BROWSE-Artbas BUTTON-ArtKort BROWSE-Ordre ~
B-Hjelp Btn_Done Btn_Cancel FILL-IN-5 FILL-IN-3 FILL-IN-8 RECT-1 RECT-49 
&Scoped-Define DISPLAYED-OBJECTS FI-Vg FI-LopNr FI-ArtikkelNr ~
COMBO-BOX-Sort T-Annonse FI-LevNr FI-LevNamn COMBO-BOX-Varegruppe ~
COMBO-BOX-Sasong FILL-IN-ArtFilter FILL-IN-5 FILL-IN-3 FILL-IN-8 FILL-IN-6 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LopeNr C-Artikkel 
FUNCTION LopeNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Artikkel AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Hjelp 
     LABEL "Hjelp" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Jamfor 
     LABEL "Ja&mfør..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Oppdater 
     LABEL "O&ppdater browser" 
     SIZE 44 BY 1.14.

DEFINE BUTTON B-Slett 
     IMAGE-UP FILE "icon/e-del":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 4.6 BY 1.14 TOOLTIP "Ta bort merkede artikkler".

DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "Ok" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-ArtKort 
     LABEL "Arti&kkelkort..." 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-Blank 
     LABEL "&Blank" 
     SIZE 7.8 BY 1 TOOLTIP "BLANKER filter og †pner s›keliste igjen".

DEFINE BUTTON BUTTON-EndreBest 
     LABEL "En&dre.." 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-Innleveranse 
     LABEL "&Innleveranse..." 
     SIZE 26.8 BY 1.14.

DEFINE BUTTON BUTTON-Kalkyle 
     LABEL "Kalky&le..." 
     SIZE 26.8 BY 1.14.

DEFINE BUTTON BUTTON-NyBest 
     LABEL "N&y.." 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-Overfor 
     LABEL "Over&føringer..." 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-Paste 
     IMAGE-UP FILE "icon\e-paste":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1 TOOLTIP "Henter bilde fra ClipBoard".

DEFINE BUTTON BUTTON-SettLopNr 
     LABEL "Sett løpenummer..." 
     SIZE 27 BY 1.05.

DEFINE BUTTON BUTTON-SlettBest 
     LABEL "Slette" 
     SIZE 27 BY 1.14.

DEFINE BUTTON BUTTON-SokLev 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Søk i leverandørregister".

DEFINE BUTTON BUTTON-Trans 
     LABEL "T&ransaksjoner..." 
     SIZE 27 BY 1.14.

DEFINE VARIABLE COMBO-BOX-Sasong AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ses&ong" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-Sort AS CHARACTER FORMAT "X(256)":U INITIAL "Vg/LpNr" 
     LABEL "Sorterin&g" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Vg/LpNr","Vg/Beskr","Vg/LevArtNr","Lev/LevArtNr","LevArtNr","ArtikkelNummer","Opprettet","Endret" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-Varegruppe AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Varugrupp" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzzz":U INITIAL 0 
     LABEL "ArtikkelNr" 
     VIEW-AS FILL-IN 
     SIZE 28.4 BY 1 TOOLTIP "Artikkelnummer. Alt-A for hurtigsøk." NO-UNDO.

DEFINE VARIABLE FI-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT ">>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Vg/LøpeNr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Varegruppe. Alt-S for hurtigsøk." NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Direkte oppslag artikkel" 
      VIEW-AS TEXT 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U INITIAL "Bestillinger" 
      VIEW-AS TEXT 
     SIZE 26.4 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Velg sorteringsordning" 
      VIEW-AS TEXT 
     SIZE 35 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-ArtFilter AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Sök l&ev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 TOOLTIP "Angi filter p† artikkelbeskrivelse og trykk ENTER." NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 126 BY 5.95.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.4 BY 4.76.

DEFINE VARIABLE T-Annonse AS LOGICAL INITIAL no 
     LABEL "A&nnonse" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .71 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Artbas FOR 
      artbas SCROLLING.

DEFINE QUERY BROWSE-Ordre FOR 
      BestHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Artbas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Artbas C-Artikkel _FREEFORM
  QUERY BROWSE-Artbas NO-LOCK DISPLAY
      ArtBas.ArtikkelNr column-label "ArtikkelNr"
      artbas.vg column-label "* Vg" format "zz9"      
      ArtBas.LopNr column-label "LøpeNr"
      ArtBas.VgKat column-label "Kat" format "z9"
      wSasBeskr column-label "Sesong" format "x(10)"
      artbas.levnr column-label "LevNr" format "zzzzz9"
      artbas.levkod column-label "Lev.Art.Nr"
      ArtBas.Farg format "zzz" column-label "F.Kod"
      ArtBas.MatKod FORMAT "zzz" COLUMN-LABEL "M.Kod"
      ArtBas.LevFargKod column-label "LevFargKod"
      artbas.beskr format "x(30)" column-label "Beskrivelse"
      artbas.bildnr format "zzzzz9" column-label "Bild"
      artbas.ny_dato column-label "Oppretttet"
      ArtBas.EDato column-label   "Endret"
      artbas.inn_dato column-label "S.Inlev"
      enable
      ArtBas.Beskr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 126 BY 16.67 ROW-HEIGHT-CHARS .62.

DEFINE BROWSE BROWSE-Ordre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Ordre C-Artikkel _STRUCTURED
  QUERY BROWSE-Ordre NO-LOCK DISPLAY
      BestHode.BestNr FORMAT ">>>>>>>9":U WIDTH 8
      BestHode.LevDato COLUMN-LABEL "LevDato" FORMAT "99/99/99":U
            WIDTH 9.4
      BestHode.BestStat FORMAT ">9":U WIDTH 3.6
      BestHode.BestillingsDato FORMAT "99/99/99":U WIDTH .6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 27 BY 4.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Slett AT ROW 6.24 COL 129
     BUTTON-Paste AT ROW 6.24 COL 151
     B-Jamfor AT ROW 24.48 COL 101
     FI-Vg AT ROW 2.43 COL 13 COLON-ALIGNED
     FI-LopNr AT ROW 2.43 COL 24 COLON-ALIGNED NO-LABEL
     FI-ArtikkelNr AT ROW 3.62 COL 13 COLON-ALIGNED
     COMBO-BOX-Sort AT ROW 5.95 COL 13 COLON-ALIGNED
     T-Annonse AT ROW 1.57 COL 82.4
     FI-LevNr AT ROW 2.48 COL 64.2 COLON-ALIGNED
     FI-LevNamn AT ROW 2.48 COL 83 COLON-ALIGNED NO-LABEL
     COMBO-BOX-Varegruppe AT ROW 3.62 COL 64 COLON-ALIGNED
     BUTTON-SokLev AT ROW 2.43 COL 80.4
     COMBO-BOX-Sasong AT ROW 4.81 COL 64 COLON-ALIGNED
     FILL-IN-ArtFilter AT ROW 5.95 COL 64 COLON-ALIGNED
     BUTTON-Blank AT ROW 6 COL 116.6
     BROWSE-Artbas AT ROW 7.43 COL 2
     BUTTON-ArtKort AT ROW 7.43 COL 129
     BUTTON-Overfor AT ROW 8.67 COL 129
     BUTTON-SettLopNr AT ROW 9.95 COL 129
     BUTTON-Trans AT ROW 11.1 COL 129
     BROWSE-Ordre AT ROW 13.14 COL 129
     BUTTON-NyBest AT ROW 17.86 COL 129.2
     BUTTON-EndreBest AT ROW 19.14 COL 129.2
     BUTTON-Kalkyle AT ROW 20.43 COL 129.4
     BUTTON-SlettBest AT ROW 21.76 COL 129.2
     BUTTON-Innleveranse AT ROW 23.1 COL 129.2
     B-Hjelp AT ROW 24.43 COL 141
     Btn_Done AT ROW 24.48 COL 2
     Btn_Cancel AT ROW 24.48 COL 18
     B-Oppdater AT ROW 24.48 COL 52
     FILL-IN-5 AT ROW 1.71 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 1.71 COL 64 COLON-ALIGNED NO-LABEL
     FILL-IN-8 AT ROW 5.05 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-6 AT ROW 12.43 COL 127.6 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.24 COL 2
     RECT-49 AT ROW 1.29 COL 128.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.6 BY 24.71
         CANCEL-BUTTON Btn_Cancel.


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
  CREATE WINDOW C-Artikkel ASSIGN
         HIDDEN             = YES
         TITLE              = "Artikelregister"
         HEIGHT             = 24.71
         WIDTH              = 156.6
         MAX-HEIGHT         = 24.71
         MAX-WIDTH          = 156.6
         VIRTUAL-HEIGHT     = 24.71
         VIRTUAL-WIDTH      = 156.6
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Artikkel
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BROWSE-Artbas BUTTON-Blank DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-Ordre BUTTON-Trans DEFAULT-FRAME */
/* SETTINGS FOR BUTTON B-Jamfor IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Jamfor:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Oppdater IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Oppdater:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-Slett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Slett:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BROWSE-Artbas:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 3
       BROWSE-Artbas:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 5000.

/* SETTINGS FOR BUTTON BUTTON-EndreBest IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-EndreBest:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Innleveranse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Innleveranse:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Kalkyle IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Kalkyle:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-NyBest IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-NyBest:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Overfor IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Overfor:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Paste IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Paste:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SettLopNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SettLopNr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SlettBest IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SlettBest:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Trans IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Trans:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FI-LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Artikkel)
THEN C-Artikkel:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Artbas
/* Query rebuild information for BROWSE BROWSE-Artbas
     _START_FREEFORM
assign
  wVgNr = (if lookup(COMBO-BOX-Varegruppe:screen-value,COMBO-BOX-VareGruppe:List-items) = 1
            then ?
            else int(entry(1,COMBO-BOX-Varegruppe:screen-value," ")))
  wSasong = (if lookup(COMBO-BOX-Sasong:screen-value,COMBO-BOX-Sasong:List-items) = 1
              then ?
              else int(substring(COMBO-BOX-Sasong:screen-value,1,4)))
  wLoop =  lookup(COMBO-BOX-Sort:screen-value,COMBO-BOX-Sort:list-items)
  wLevNr = INPUT FI-LevNr
  wLevNr = (IF wLevNr = 0
             THEN ?
             ELSE wLevNr)
  T-Annonse.

case wLoop:
   when 1 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.Vg
      by Artbas.LopNr
      by ArtBas.Beskr
      by ArtBas.LevNr
      INDEXED-REPOSITION.

  when 2 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.Vg
      by Artbas.Beskr
      by ArtBas.LevNr
      by ArtBas.LevKod
      INDEXED-REPOSITION.

  when 3 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.Vg
      by Artbas.LevNr
      by ArtBas.LevKod
      by ArtBas.Beskr
      INDEXED-REPOSITION.

  when 4 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.LevNr
      by Artbas.LevKod
      by ArtBas.Vg
      by ArtBas.Beskr
      INDEXED-REPOSITION.

  when 5 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.LevKod
      INDEXED-REPOSITION.

  when 6 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.ArtikkelNr
      INDEXED-REPOSITION.

  when 7 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.RegistrertDato
      by ArtBas.ArtikkelNr
      INDEXED-REPOSITION.

  when 8 then
      OPEN QUERY {&SELF-NAME} FOR EACH artbas NO-LOCK where
      (if wVgNr = ?
         then true
       else Artbas.Vg = wVgNr) and
      ArtBas.LevKod matches FILL-IN-ArtFilter + "*" and
      (if wLevNr = ?
         then true
       else ArtBas.LevNr = wLevNr) and
      (if wSasong = ?
         then true
       else Artbas.Sasong = wSasong) and
      (if T-Annonse = false
        then true
       else ArtBas.AnonseArtikkel = true)
      by Artbas.EDato
      by ArtBas.ArtikkelNr
      INDEXED-REPOSITION.
end case.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE BROWSE-Artbas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Ordre
/* Query rebuild information for BROWSE BROWSE-Ordre
     _TblList          = "skotex.BestHode"
     _Options          = "NO-LOCK"
     _Where[1]         = "BestHode.ArtikkelNr = (if available ArtBas
                         then ArtBas.ArtikkelNr
                         else -99)"
     _FldNameList[1]   > skotex.BestHode.BestNr
"BestHode.BestNr" ? ? "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.BestHode.LevDato
"BestHode.LevDato" "LevDato" "99/99/99" "date" ? ? ? ? ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > skotex.BestHode.BestStat
"BestHode.BestStat" ? ? "integer" ? ? ? ? ? ? no ? no no "3.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > skotex.BestHode.BestillingsDato
"BestHode.BestillingsDato" ? "99/99/99" "date" ? ? ? ? ? ? no ? no no ".6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-Ordre */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.57
       COLUMN          = 130.6
       HEIGHT          = 4.19
       WIDTH           = 23.4
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Image-Sko:MOVE-AFTER(B-Oppdater:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Artikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Artikkel C-Artikkel
ON END-ERROR OF C-Artikkel /* Artikelregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Artikkel C-Artikkel
ON WINDOW-CLOSE OF C-Artikkel /* Artikelregister */
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


&Scoped-define SELF-NAME B-Jamfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Jamfor C-Artikkel
ON CHOOSE OF B-Jamfor IN FRAME DEFAULT-FRAME /* Jamfør... */
DO:
    IF NOT AVAIL ArtBas THEN
        RETURN.
    IF NOT VALID-HANDLE(hJmfRutine) THEN DO:
        RUN w-bildejmf.w PERSISTENT SET hJmfRutine.
        create tmpChild.
        ASSIGN tmpChild.wChild = hJmfRutine.
    END.
    RUN NyArtBas IN hJmfRutine (ArtBas.ArtikkelNr).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-Artikkel
ON CHOOSE OF B-Oppdater IN FRAME DEFAULT-FRAME /* Oppdater browser */
DO:
  if not available ArtBas then
    return no-apply.
  do:
    {sww.i}
    assign
      FILL-IN-ArtFilter = "*".
    display 
      FILL-IN-ArtFilter
    with frame DEFAULT-FRAME.
      
    {&OPEN-BROWSERS-IN-QUERY-C-Artikkel}
  
    find first bArtBas no-lock where
      bArtBas.ArtikkelNr >= ArtBas.ArtikkelNr no-error.
    if not available bArtBas then
      find last bArtBas no-lock no-error.
    {swn.i}
    if available bArtBas then
      do:
        REPOSITION BROWSE-Artbas TO ROWID rowid(bArtBas) NO-ERROR.
        find ArtBas no-lock where
          recid(ArtBas) = recid(bArtBas) no-error.
        run VisBilde (1).  
        {&OPEN-QUERY-BROWSE-Ordre}
      end.
    APPLY "ENTRY" TO BROWSE BROWSE-Artbas.
    return no-apply.
  end.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett C-Artikkel
ON CHOOSE OF B-Slett IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  run SlettArtikkel.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Artbas
&Scoped-define SELF-NAME BROWSE-Artbas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Artbas C-Artikkel
ON MOUSE-SELECT-DBLCLICK OF BROWSE-Artbas IN FRAME DEFAULT-FRAME
or "RETURN":U of BROWSE-ArtBas
DO:
  if BUTTON-ArtKort:sensitive = true then
    apply "CHOOSE":U to BUTTON-ArtKort in frame {&FRAME-NAME}.
  else
    apply "CHOOSE":U to Btn_Done in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Artbas C-Artikkel
ON RETURN OF BROWSE-Artbas IN FRAME DEFAULT-FRAME
DO:
  if BUTTON-ArtKort:sensitive = true then
    apply "CHOOSE":U to BUTTON-ArtKort in frame {&FRAME-NAME}.
  else
    apply "CHOOSE":U to Btn_Done in frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Artbas C-Artikkel
ON ROW-DISPLAY OF BROWSE-Artbas IN FRAME DEFAULT-FRAME
DO:
  /*
  if ArtBas.LopNr = ? then
    display "" @ ArtBas.LopNr with browse BROWSE-ArtBas .
  */
  find SaSong of ArtBas no-lock no-error.
  if available SaSong then
    wSasBeskr = SaSong.SasBeskr.
  else 
    wSasBeskr = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Artbas C-Artikkel
ON START-SEARCH OF BROWSE-Artbas IN FRAME DEFAULT-FRAME
DO:
  APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Artbas C-Artikkel
ON VALUE-CHANGED OF BROWSE-Artbas IN FRAME DEFAULT-FRAME
DO:
  run VisBilde (1).  
  {&OPEN-QUERY-BROWSE-Ordre}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Artikkel
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Artikkel
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Ok */
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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ArtKort C-Artikkel
ON CHOOSE OF BUTTON-ArtKort IN FRAME DEFAULT-FRAME /* Artikkelkort... */
DO:
  create tmpChild.
  if available ArtBas then
    run w-vartkorJFSpec PERSISTENT set tmpChild.wChild (input recid(ArtBas), "ENDRE").
  else 
    run w-vartkorJFSpec PERSISTENT set tmpChild.wChild (input ?, "ENDRE").

  if entry(1,return-value) = "AVBRYT" then
    return no-apply.
  else if entry(1,return-value) = "SLETTET" then
    do:
      wOk = BROWSE-Artbas:DELETE-SELECTED-ROW(1).
      return no-apply.
    end.
  else if entry(1,return-value) = "NY" then
    do:
      if BROWSE-Artbas:FOCUSED-ROW < BROWSE-Artbas:DOWN then
        assign wLoop = BROWSE-Artbas:FOCUSED-ROW + 1.
      else 
        assign wLoop = BROWSE-Artbas:FOCUSED-ROW.  
      wOk = BROWSE-Artbas:insert-row("AFTER").
      wOk = BROWSE-Artbas:CREATE-RESULT-LIST-ENTRY().
      wOk = BROWSE-Artbas:SELECT-ROW(wLoop).
      find ArtBas no-lock where
        recid(ArtBas) = int(entry(2,return-value)).
    end.
  
  display
      artbas.vg
      artbas.lopnr
      artbas.beskr
      artbas.levnr
      artbas.levkod
      artbas.ny_dato
      artbas.inn_dato
      artbas.bildnr 
  with browse BROWSE-ArtBas.             
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Blank C-Artikkel
ON CHOOSE OF BUTTON-Blank IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FILL-IN-ArtFilter:screen-value in frame {&FRAME-NAME} = "*".
  run StartBrowse.  
  run VisBilde (1).  
  {&OPEN-QUERY-BROWSE-Ordre}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EndreBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EndreBest C-Artikkel
ON CHOOSE OF BUTTON-EndreBest IN FRAME DEFAULT-FRAME /* Endre.. */
DO:
  def var wBestHodeRecid as recid no-undo.
  
  if not available ArtBas then
    return no-apply.
  
  if not available BestHode then
    return no-apply.
    
  assign
    wBestHodeRecid = recid(BestHode).
  create tmpChild.
  run w-gridord.w persistent set tmpChild.wChild (input recid(ArtBas), input-output wBestHodeRecid, "ENDRE").
  /*{&OPEN-QUERY-BROWSE-Ordre}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Innleveranse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Innleveranse C-Artikkel
ON CHOOSE OF BUTTON-Innleveranse IN FRAME DEFAULT-FRAME /* Innleveranse... */
DO:
  def var wBestHodeRecid as recid no-undo.
  
  if not available ArtBAs then
    return no-apply.
    
  if ArtBas.LopNr = 0 or ArtBas.LopNr = ? then
    do:
      message "Artikkelen må tildeles løpenummer før innleveranse kan gjøres!"
        view-as alert-box MESSAGE title "Melding".
      return no-apply.
    end.
    
  if not available BestHode then
    return no-apply.

  if BestHode.BestStat < 2  then
    do:
      message "Bestilling med denne status kan ikke innleveres!"
        view-as alert-box title "Melding".
      return no-apply.
    end.  
  
  assign
    wBestHodeRecid = recid(BestHode).
  create tmpChild.
  run w-gridinnlev.w persistent set tmpChild.wChild (input recid(ArtBas), input-output wBestHodeRecid, "INLEV").
  /*{&OPEN-QUERY-BROWSE-Ordre}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kalkyle C-Artikkel
ON CHOOSE OF BUTTON-Kalkyle IN FRAME DEFAULT-FRAME /* Kalkyle... */
DO:
  def var wBestHodeRecid as recid no-undo.
  
  if not available ArtBas then
    return no-apply.
  if not available BestHode then
    return no-apply.
    
  assign
    wBestHodeRecid = recid(BestHode).
  run d-vbestkalkyle.w (input recid(ArtBas), input wBestHodeRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyBest C-Artikkel
ON CHOOSE OF BUTTON-NyBest IN FRAME DEFAULT-FRAME /* Ny.. */
DO:
  def var wBestHodeRecid as recid no-undo.
  
  if not available ArtBas then
    return no-apply.
  if ArtBas.Utgatt = true then   
    do:
      message "Artikkelen er utgått. Bestilling kan ikke registreres på utgått artikkel."
        view-as alert-box message title "Melding".
      return no-apply.
    end.
  if ArtBas.Lager = false then   
    do:
      message "Artikkelen har ikke lagerstyring. Bestilling kan ikke registreres."
        view-as alert-box message title "Melding".
      return no-apply.
    end.

  assign
    wBestHodeRecid = ?.
  create tmpChild.
  run w-gridord.w  persistent set tmpChild.wChild (input recid(ArtBas), input-output wBestHodeRecid, "NY").  
  /*
  if wBestHodeRecid <> ? then
    do:
      {&OPEN-QUERY-BROWSE-Ordre}
    end.
  */
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Overfor C-Artikkel
ON CHOOSE OF BUTTON-Overfor IN FRAME DEFAULT-FRAME /* Overføringer... */
DO:
  if not available ArtBas then
    return no-apply.
    
  if ArtBas.Lager = false then   
    do:
      message "Artikkelen har ikke lagerstyring."
        view-as alert-box message title "Melding".
      return no-apply.
    end.
  if ArtBas.LopNr = 0 or ArtBas.LopNr = ? then
    do:
      message "Artikkelen er ikke tildelt løpenummer!"
        view-as alert-box MESSAGE title "Melding".
      return no-apply.
    end.
  
  if available ArtBas then
    do:
      create tmpChild.
      run w-gridlager.w PERSISTENT set tmpChild.wChild (input recid(ArtBas), "SOEK").
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Paste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Paste C-Artikkel
ON CHOOSE OF BUTTON-Paste IN FRAME DEFAULT-FRAME
DO:
  def var wFilNAvn     as char no-undo.
  def var wBildNr      like BildeRegister.BildNr no-undo.
  {limfraclipboard.i}
  SELF:PRIVATE-DATA = "".
  BROWSE-Artbas:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Paste C-Artikkel
ON DROP-FILE-NOTIFY OF BUTTON-Paste IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cFilnavn AS CHARACTER  NO-UNDO.
    IF SELF:NUM-DROPPED-FILES = 1 THEN DO:
        ASSIGN cFilNavn = SELF:GET-DROPPED-FILE(1).
        IF NOT CAN-DO("bmp,jpg",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
            MESSAGE "Tillatte filtyper: '.bmp,.jpg'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE IF ENTRY(NUM-ENTRIES(cFilNavn,"\"),cFilNavn,"\") BEGINS "mini" THEN DO:
            MESSAGE "Fil med navn 'mini...' kann ikke leses inn."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        ELSE DO:
            ASSIGN SELF:PRIVATE-DATA = cFilnavn.
            APPLY "CHOOSE" TO SELF.
        END.
    END.
    ELSE DO:
        MESSAGE "Endast 1 fil tillatt!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SettLopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SettLopNr C-Artikkel
ON CHOOSE OF BUTTON-SettLopNr IN FRAME DEFAULT-FRAME /* Sett løpenummer... */
DO:
  run SettLopenummer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SlettBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest C-Artikkel
ON CHOOSE OF BUTTON-SlettBest IN FRAME DEFAULT-FRAME /* Slette */
DO:

  run SlettBestilling (1).
  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev C-Artikkel
ON CHOOSE OF BUTTON-SokLev IN FRAME DEFAULT-FRAME /* ... */
or F10 of ArtBas.LevNr
DO:
  {soek.i
    &Felt       = FI-LevNr
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-blevbas.w 
    &OptDisp    = "LevBas.LevNamn  when available LevBas @ FI-LevNamn"
    &PostRun    = "find LevBas no-lock where
                     recid(LevBas) = int(return-value) no-error."
  }
  RUN StartBrowse.
  apply "ENTRY":U to BROWSE-Artbas.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Trans C-Artikkel
ON CHOOSE OF BUTTON-Trans IN FRAME DEFAULT-FRAME /* Transaksjoner... */
DO:
  if not available ArtBas then
    return no-apply.
  create tmpChild.
  run w-barttranslogg.w PERSISTENT set tmpChild.wChild (ArtBas.ArtikkelNr).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Sasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Sasong C-Artikkel
ON VALUE-CHANGED OF COMBO-BOX-Sasong IN FRAME DEFAULT-FRAME /* Sesong */
DO:
  run StartBrowse.      

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Sort C-Artikkel
ON VALUE-CHANGED OF COMBO-BOX-Sort IN FRAME DEFAULT-FRAME /* Sortering */
DO:
  run StartBrowse.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Varegruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Varegruppe C-Artikkel
ON VALUE-CHANGED OF COMBO-BOX-Varegruppe IN FRAME DEFAULT-FRAME /* Varugrupp */
DO:
run StartBrowse.      

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ArtikkelNr C-Artikkel
ON RETURN OF FI-ArtikkelNr IN FRAME DEFAULT-FRAME /* ArtikkelNr */
DO:
  if (input FI-ArtikkelNr <> 0) then
    do:
      {sww.i}
      assign
        FILL-IN-ArtFilter = "*".
      display 
        FILL-IN-ArtFilter
      with frame DEFAULT-FRAME.
      
      {&OPEN-BROWSERS-IN-QUERY-C-Artikkel}
  
      find first bArtBas no-lock where
        bArtBas.ArtikkelNr >= input FI-ArtikkelNr no-error.
      if not available bArtBas then
        find last bArtBas no-lock no-error.
      {swn.i}
      if available bArtBas then
        do:
          REPOSITION BROWSE-Artbas TO ROWID rowid(bArtBas) NO-ERROR.
          find ArtBas no-lock where
            recid(ArtBas) = recid(bArtBas) no-error.
          run VisBilde (1).  
          {&OPEN-QUERY-BROWSE-Ordre}
        end.
      APPLY "ENTRY" TO BROWSE BROWSE-Artbas.
      return no-apply.
    end.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr C-Artikkel
ON TAB OF FI-LevNr IN FRAME DEFAULT-FRAME /* LevNr */
OR "RETURN" OF FI-LevNr
DO:
  FIND LevBas NO-LOCK WHERE
      LevBas.LEvNr = INPUT FI-LEvNr NO-ERROR.
  IF AVAILABLE LevBas THEN
  DO:
      ASSIGN
          FI-LevNamn = LevBas.LevNamn
          .
      DISPLAY
          FI-LevNamn
          WITH FRAME Default-Frame.
      RUN StartBrowse.
      apply "ENTRY":U to BROWSE-Artbas.

  END.
  ELSE DO:
      MESSAGE "Ugyldig leverandørnummer"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
          FI-LevNamn = ""
          .
      DISPLAY 
          FI-LevNamn
          WITH FRAME Default-Frame.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr C-Artikkel
ON RETURN OF FI-LopNr IN FRAME DEFAULT-FRAME
DO:
  if (input FI-LopNr <> 0 and
      input FI-LopNr <> ?) then
    do:
      {sww.i}
      assign
        FILL-IN-ArtFilter = "*".
      display 
        FILL-IN-ArtFilter
      with frame DEFAULT-FRAME.
      
      {&OPEN-BROWSERS-IN-QUERY-C-Artikkel}
  
      find first bArtBas no-lock where
        bArtBas.Vg      = input FI-Vg and
        bArtBas.LopNr  >= input FI-LopNr no-error.
      {swn.i}
      if available bArtBas then
        do:
          REPOSITION BROWSE-Artbas TO ROWID rowid(bArtBas) NO-ERROR.
          find ArtBas no-lock where
            recid(ArtBas) = recid(bArtBas) no-error.
          run VisBilde (1).  
          {&OPEN-QUERY-BROWSE-Ordre}
        end.
      APPLY "ENTRY" TO BROWSE BROWSE-Artbas.
      return no-apply.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg C-Artikkel
ON RETURN OF FI-Vg IN FRAME DEFAULT-FRAME /* Vg/LøpeNr */
DO:
  if input FI-Vg <> 0 then
    do:
      find VarGr no-lock where
        VarGr.Vg = input FI-Vg no-error.
      if not available VarGr then
        do:
          message "Ukjent varegruppe!" 
            view-as alert-box title "Feil ved søk".
          return no-apply.
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ArtFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ArtFilter C-Artikkel
ON RETURN OF FILL-IN-ArtFilter IN FRAME DEFAULT-FRAME /* Sök lev.art.nr */
DO:
  assign
    COMBO-BOX-Sort:screen-value = entry(5,COMBO-BOX-Sort:list-items).
  run StartBrowse.  

  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Image-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Image-Sko C-Artikkel OCX.DblClick
PROCEDURE Image-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available ArtBAs then
    do:
      find BildeRegister of ArtBas no-error.
      if available BildeRegister then
        run d-visbil.w (input recid(BildeRegister)).
    end.
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Annonse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Annonse C-Artikkel
ON VALUE-CHANGED OF T-Annonse IN FRAME DEFAULT-FRAME /* Annonse */
DO:
  run StartBrowse.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Artikkel 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "SøkelisteArtikkelregister"
  &PreIClose      = " "
  &PostIClose     = " "
  &PostDisable_ui = "for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end."
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
on ALT-S of C-Artikkel anywhere 
  apply "ENTRY":U to FI-Vg in frame DEFAULT-FRAME.
on ALT-A of C-Artikkel anywhere 
  apply "ENTRY":U to FI-ArtikkelNr in frame DEFAULT-FRAME.
run Finn_Vg.      /* Laster varegruppene inn i ComboBox. */
run Finn_Sasong.  /* Laster alle sesonger i Combo-Box */
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  &SCOP PROCEDURE-TYPE FRAME

  RUN enable_UI.
  {lng.i}
  
  /* Kalles s›kerutinen fra artikkelkortet, skal ikke artikkelkortet */
  /* kunne startes herfra.                                           */
  /* ******   Dette må endres- dårlig løsning!!!! **** */
  if  program-name(2) matches "*w-vartkor*" or
      program-name(2) matches "*w-overforrest*" then
    assign 
      BUTTON-ArtKort:sensitive      = false
      BUTTON-EndreBest:sensitive    = false 
      BUTTON-Innleveranse:sensitive = false 
      BUTTON-Kalkyle:sensitive      = false 
      BUTTON-NyBest:sensitive       = false 
      BUTTON-Overfor:sensitive      = false 
      BUTTON-SettLopNr:sensitive    = false 
      BUTTON-SlettBest:sensitive    = false.

  assign
    ArtBas.Beskr:read-only in browse BROWSE-Artbas = TRUE.

  run VisBilde (1).
  {&OPEN-QUERY-BROWSE-Ordre}
  apply "ENTRY":U to FI-Vg.
  
  ASSIGN
    C-Artikkel:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  if available ArtBas
    then return string(recid(ArtBas)).
  else 
    return "AVBRYT".
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Artikkel  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-bartsokJFSpec.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    Image-Sko:NAME = "Image-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-bartsokJFSpec.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Artikkel  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Artikkel)
  THEN DELETE WIDGET C-Artikkel.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Artikkel  _DEFAULT-ENABLE
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
  DISPLAY FI-Vg FI-LopNr FI-ArtikkelNr COMBO-BOX-Sort T-Annonse FI-LevNr 
          FI-LevNamn COMBO-BOX-Varegruppe COMBO-BOX-Sasong FILL-IN-ArtFilter 
          FILL-IN-5 FILL-IN-3 FILL-IN-8 FILL-IN-6 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Artikkel.
  ENABLE FI-Vg FI-LopNr FI-ArtikkelNr COMBO-BOX-Sort T-Annonse FI-LevNr 
         COMBO-BOX-Varegruppe BUTTON-SokLev COMBO-BOX-Sasong FILL-IN-ArtFilter 
         BUTTON-Blank BROWSE-Artbas BUTTON-ArtKort BROWSE-Ordre B-Hjelp 
         Btn_Done Btn_Cancel FILL-IN-5 FILL-IN-3 FILL-IN-8 RECT-1 RECT-49 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Artikkel.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDisButtons C-Artikkel 
PROCEDURE EnaDisButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER lEnable AS LOGICAL    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN B-Oppdater:SENSITIVE          = FALSE
               B-Slett:SENSITIVE             = FALSE
               BUTTON-ArtKort:SENSITIVE      = lEnable
               BUTTON-EndreBest:SENSITIVE    = FALSE
               BUTTON-Innleveranse:SENSITIVE = FALSE
               BUTTON-Kalkyle:SENSITIVE      = FALSE
               BUTTON-NyBest:SENSITIVE       = FALSE
               BUTTON-Overfor:SENSITIVE      = FALSE
               BUTTON-Paste:SENSITIVE        = FALSE
               BUTTON-SettLopNr:SENSITIVE    = FALSE
               BUTTON-SlettBest:SENSITIVE    = FALSE
/*                BUTTON-SokLev:SENSITIVE       = FALSE */
               BUTTON-Trans:SENSITIVE        = FALSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Finn_Sasong C-Artikkel 
PROCEDURE Finn_Sasong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOK as log no-undo.
  
  do with frame {&FRAME-NAME}:
    assign
      COMBO-BOX-Sasong:list-items = "[Alle]"
      COMBO-BOX-Sasong:screen-value = "[Alle]"
      COMBO-BOX-Sasong = "[Alle]".
    for each Sasong no-lock:
      assign
        wOK = COMBO-BOX-Sasong:add-last(string(SaSong.SaSong,"zzz9") + "   " + 
                                               Sasong.SasBeskr).
    end.
  end.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Finn_Vg C-Artikkel 
PROCEDURE Finn_Vg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOK as log no-undo.
  
  do with frame {&FRAME-NAME}:
    assign
      COMBO-BOX-Varegruppe:list-items = "[Alle]"
      COMBO-BOX-Varegruppe:screen-value = "[Alle]"
      COMBO-BOX-Varegruppe = "[Alle]".
    for each VarGr no-lock:
      assign
        wOK = COMBO-BOX-Varegruppe:add-last(string(VarGr.Vg,"999") + "   " + 
                                            VarGr.VgBeskr).
    end.
  end.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Artikkel 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     bläddring från artikelkort
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    BROWSE BROWSE-Artbas:REFRESH().
    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        CASE cRettning:
            WHEN "Prev" THEN
                BROWSE BROWSE-Artbas:SELECT-PREV-ROW( ).
            WHEN "Next" THEN
                BROWSE BROWSE-Artbas:SELECT-NEXT-ROW( ).
        END CASE.
        PUBLISH "ByttArtikkel" (ArtBas.ArtikkelNr).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLopeNummer C-Artikkel 
PROCEDURE SettLopeNummer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame DEFAULT-FRAME:
  if not available ArtBas then
    return no-apply.

  run d-vtildelopnr.w (input recid(ArtBas)).  
  if return-value = "AVBRYT" then
    return no-apply.

  find current ArtBas no-lock no-error.
  wOk = BROWSE-Artbas:REFRESH().

end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettArtbas C-Artikkel 
PROCEDURE SlettArtbas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var i      as int  no-undo.
  def var wTekst as char no-undo.
  def var wSvar  as log  no-undo.

  /* Kontroll mot nytt bestillingsregister. */
  if can-find(first BestHode where
    BestHode.ArtikkelNr = ArtBas.ArtikkelNr) then
  if available BestHode then
    do:
      assign wSvar = false.
      message "Det er registrert bestilling(er) på artikkelen." skip
              "Skal den alikevel tas bort!" view-as alert-box 
              QUESTION buttons yes-no title "Bekreft"
              update wSvar.
      if wSvar <> true then
        return no-apply "AVBRYT".
    end.

  /* Fikser bestillingene */
  for each BestHode of ArtBas no-lock:
    RUN SlettBestilling (2).
    if return-value = "AVBRYT" then 
      return no-apply "AVBRYT".
  end.
    
  {sww.i} /* Session wait staite. */
  run slettartbas.p (recid(artbas)).
  {swn.i} /* Session wait staite. */
  return no-apply "SLETTET".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettArtikkel C-Artikkel 
PROCEDURE SlettArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk as log format "Ja/Nei" no-undo.
  def var wNesteRecid as recid no-undo.
  
  if not available ArtBas then
    return no-apply.
    
  assign wOk = false.
  message "Skal artikkelen slettes?" view-as alert-box 
    QUESTION BUTTONS YES-NO
    title "Bekreftelse"
    update wOk.
  if wOk = false then
    return no-apply "AVBRYT".  

  else do with frame DEFAULT-FRAME:
    run SlettArtBas.
    if return-value = "AVBRYT" then
      return no-apply.
    wOk = BROWSE-ArtBas:DELETE-CURRENT-ROW( ).
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBestilling C-Artikkel 
PROCEDURE SlettBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter wModus as int no-undo. /* 1-Bekreft, 2-Ikke bekreft */

do with frame FRAME-Ordre:
  def var wBestHodeRecid as recid no-undo.
  
  if not available ArtBas then
    return no-apply.
  if not available BestHode then
    return no-apply.
  
  assign
    wBestHodeRecid = recid(BestHode).

  if wModus <> 2 then
    do:
      if can-do(",4,5",string(BestHode.BestStat)) then
        do:
          message "Bestilling " BestHode.BestNr "er sendt til leverandør (eller delhvis levert)!" skip
              "Skal den alikevel tas bort?"
              view-as alert-box buttons yes-no-cancel title "Bekreftelse"
              update wOk.
        end.
      else do:  
        message "Skal bestilling slettes?"
          view-as alert-box buttons yes-no-cancel title "Bekreftelse" 
        update wOk.
      end.
    end.
  else
    wOk = true.

  if wOk = true then
    do transaction:
      {sww.i}
      /* KanSlettes*/      
      run w-gridord.w (input recid(ArtBas), input-output wBestHodeRecid, "SLETT").
      {swn.i}
      {&OPEN-QUERY-BROWSE-Ordre}
      return "OK".
    end.
  else return no-apply "AVBRYT".
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartBrowse C-Artikkel 
PROCEDURE StartBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN FRAME Default-Frame
      FI-LevNr
      .

  RUN VisBilde (0).

  assign
    COMBO-BOX-Varegruppe = COMBO-BOX-Varegruppe:screen-value in frame {&FRAME-NAME}
    COMBO-BOX-Sasong     = COMBO-BOX-Sasong:screen-value in frame {&FRAME-NAME}
    FILL-IN-ArtFilter    = FILL-IN-ArtFilter:screen-value in frame {&FRAME-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  {&OPEN-QUERY-BROWSE-Ordre}

  IF BROWSE BROWSE-Artbas:FOCUSED-ROW = ? THEN
      RUN EnaDisButtons(FALSE).
  ELSE DO:
      RUN EnaDisButtons(TRUE).
      run VisBilde (1).  
      apply "ENTRY":U to BROWSE-Artbas in frame DEFAULT-FRAME.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Artikkel 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if not available ArtBas then
    return.
  {visbilde.i
   &BldOcx = chImage-Sko
   &BildNr = "ArtBas.BildNr"
  }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LopeNr C-Artikkel 
FUNCTION LopeNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wReturn as char.
  
  if ArtBas.LopNr = ?
    then wreturn = "".
  else if ArtBas.LopNr = 0
    then wreturn = "".
  else 
    wreturn = string(artbas.lopnr,"9999").

  return wReturn.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

