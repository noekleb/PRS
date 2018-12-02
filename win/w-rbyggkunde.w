&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE Bydel NO-UNDO LIKE Post.


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
def var wUtskriftProg as handle no-undo.
def var wListeType    as char   no-undo.
def var wModus        as char   no-undo.
def var wListerRecid  as recid  no-undo.
def var wCellNr       as int    no-undo.
def var wAlle         as char   no-undo.

/* Alle av mulige valg */
def var wKunder       as char no-undo.
def var wKundeGruppe  as char no-undo.
def var wKundeType    as char no-undo.
def var wBydel        as char no-undo.
def var wPostNr       as char no-undo.
def var wKommune      as char no-undo.
def var wFylke        as char no-undo.
def var wButiker      as char no-undo.

/* Valgte valg. */
def var wvKunder       as char no-undo.
def var wvKundeGruppe  as char no-undo.
def var wvKundeType    as char no-undo.
def var wvBydel        as char no-undo.
def var wvPostNr       as char no-undo.
def var wvKommune      as char no-undo.
def var wvFylke        as char no-undo.
def var wvButiker      as char no-undo.
def var wAktiv         as int  no-undo.

/* For kobling mot bildegrid */
DEFINE VAR wChild AS HANDLE NO-UNDO.
DEFINE VAR wMenu  AS CHAR NO-UNDO.
DEFINE VAR wScreenSize AS INTE INIT 1 NO-UNDO.
DEFINE VAR wColor AS INTE EXTENT 3 INIT [5197823,65280,16711680] NO-UNDO.
define var wNumRecords as int no-undo.
define var wMenuAction as char no-undo.

/* Buffere */
def buffer bLister for Lister.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-SokTillOmsDato B-SokListe FI-Navn ~
FI-Merknad FI-FraOpprettet FI-TilOpprettet FI-FraEndret FI-TilEndret ~
FI-FraFKjop FI-TilFKjop FI-FraSKjop FI-TilSKjop FI-FraOmsDato FI-TilOmsDato ~
FI-Oms FI-KundeNavn RS-Filter T-HarMedlem T-HarKundeKort T-HarOmsetning ~
T-HarSaldo T-HarKreditSperre T-HarOpphort T-Butikk B-Kunde B-KundeGruppe ~
B-KundeType BUTTON-SokTilSKjop B-Bydel B-PostNr B-Kommune B-Fylke ~
BUTTON-SokTilFKjop BUTTON-SokTilEndret BUTTON-SokTilOpprettet ~
BUTTON-SokFraSKjop BUTTON-SokFraFKjop BUTTON-SokFraEndret ~
BUTTON-SokFraOpprettet B-Exit B-Ny Btn_Help BUTTON-SokFraOmsDato RECT-10 ~
RECT-2 RECT-6 RECT-7 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS FI-ListeNr FI-Navn FI-Merknad FI-Opprettet ~
FI-Endret FI-Eier FI-FraOpprettet FI-TilOpprettet FI-FraEndret FI-TilEndret ~
FI-FraFKjop FI-TilFKjop FI-FraSKjop FI-TilSKjop FI-FraOmsDato FI-TilOmsDato ~
FI-Oms FI-KundeNavn RS-Filter T-HarMedlem T-HarKundeKort T-HarOmsetning ~
T-HarSaldo T-HarKreditSperre T-HarOpphort T-Butikk FI-Butiker FI-Bydel ~
FI-Kunde FI-PostNr FI-Kundegruppe FI-Kommune FI-Kundetype FI-Fylke FI-Info ~
FILL-IN-Tekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Hex2Int C-Win 
FUNCTION Hex2Int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Angre 
     IMAGE-UP FILE "icon/e-undo":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Angre".

DEFINE BUTTON B-Butikker 
     LABEL "&Butikker..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Bydel 
     LABEL "B&ydel..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Bygg 
     IMAGE-UP FILE "icon/e-save":U NO-FOCUS FLAT-BUTTON
     LABEL "By&gg liste..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre - Ved ny oppprettes ny liste, ved endre, oppdateres valgt liste.".

DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON B-Forrige 
     IMAGE-UP FILE "icon/e-pilopp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av valgt liste.".

DEFINE BUTTON B-Fylke 
     LABEL "&Fylke..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Kommune 
     LABEL "Ko&mmune..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Kunde 
     LABEL "&Kunder..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-KundeGruppe 
     LABEL "K&undegrupper..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-KundeType 
     LABEL "Kun&deTyper..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Neste 
     IMAGE-UP FILE "icon/e-pilned":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Neste".

DEFINE BUTTON B-Ny 
     IMAGE-UP FILE "icon/e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny kolleksjonsliste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny liste".

DEFINE BUTTON B-PostNr 
     LABEL "P&ostNummer..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Slett 
     IMAGE-UP FILE "icon/e-del":U NO-FOCUS FLAT-BUTTON
     LABEL "Sl&ett kolleksjonsliste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Sletter valgt liste.".

DEFINE BUTTON B-SokListe 
     IMAGE-UP FILE "icon/e-sokpr":U
     LABEL "" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON B-Utskrift 
     IMAGE-UP FILE "icon/e-print":U NO-FOCUS FLAT-BUTTON
     LABEL "&Utskrift av kolleksjonsliste..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av valgt liste.".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokFraEndret 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokFraFKjop 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokFraOmsDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokFraOpprettet 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokFraSKjop 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokTilEndret 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokTilFKjop 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokTillOmsDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokTilOpprettet 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokTilSKjop 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bydel AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Eier AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eier" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Endret AS CHARACTER FORMAT "X(256)":U 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraEndret AS DATE FORMAT "99/99/99":U 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraFKjop AS DATE FORMAT "99/99/99":U 
     LABEL "1. kjøp" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraOmsDato AS DATE FORMAT "99/99/99":U 
     LABEL "Omsetning" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraOpprettet AS DATE FORMAT "99/99/99":U 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraSKjop AS DATE FORMAT "99/99/99":U 
     LABEL "Siste kjøp" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Fylke AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 133.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kommune AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kunde AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kundegruppe AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KundeNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kundetype AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ListeNr AS INTEGER FORMAT "-zzzzzzzz9":U INITIAL 0 
     LABEL "Liste nr/navn" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Merknad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 131.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 94.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Oms AS DECIMAL FORMAT "-zzz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Opprettet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FI-PostNr AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilEndret AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilFKjop AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilOmsDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilOpprettet AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilSKjop AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Merking" 
      VIEW-AS TEXT 
     SIZE 48 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE RS-Filter AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Begynner med", 1,
"Inneholder", 2
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 134 BY .1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 133.2 BY 6.43.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 134.6 BY 12.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 134 BY .1.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 134 BY .1.

DEFINE VARIABLE T-Butikk AS LOGICAL INITIAL no 
     LABEL "&Pr. butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE T-HarKreditSperre AS LOGICAL INITIAL no 
     LABEL "Har kredittsperre" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE T-HarKundeKort AS LOGICAL INITIAL no 
     LABEL "Har kundekort" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE T-HarMedlem AS LOGICAL INITIAL no 
     LABEL "Har medlemmer" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE T-HarOmsetning AS LOGICAL INITIAL no 
     LABEL "Har omsetning" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-HarOpphort AS LOGICAL INITIAL no 
     LABEL "Har opphørt" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE T-HarSaldo AS LOGICAL INITIAL no 
     LABEL "Har saldo" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokTillOmsDato AT ROW 11.05 COL 68
     FI-ListeNr AT ROW 3.43 COL 15.4 COLON-ALIGNED
     B-Angre AT ROW 1.29 COL 17
     B-SokListe AT ROW 3.38 COL 34.6
     FI-Navn AT ROW 3.43 COL 37.4 COLON-ALIGNED NO-LABEL
     FI-Merknad AT ROW 4.57 COL 2.4 NO-LABEL
     FI-Opprettet AT ROW 6.24 COL 104 COLON-ALIGNED
     FI-Endret AT ROW 7.43 COL 104 COLON-ALIGNED
     FI-Eier AT ROW 8.62 COL 104 COLON-ALIGNED
     FI-FraOpprettet AT ROW 6.24 COL 19 COLON-ALIGNED
     FI-TilOpprettet AT ROW 6.24 COL 45.4 COLON-ALIGNED NO-LABEL
     FI-FraEndret AT ROW 7.43 COL 19 COLON-ALIGNED
     FI-TilEndret AT ROW 7.43 COL 45.4 COLON-ALIGNED NO-LABEL
     FI-FraFKjop AT ROW 8.62 COL 19 COLON-ALIGNED
     FI-TilFKjop AT ROW 8.62 COL 45.4 COLON-ALIGNED NO-LABEL
     FI-FraSKjop AT ROW 9.81 COL 19 COLON-ALIGNED
     FI-TilSKjop AT ROW 9.81 COL 45.4 COLON-ALIGNED NO-LABEL
     FI-FraOmsDato AT ROW 11.05 COL 19 COLON-ALIGNED
     FI-TilOmsDato AT ROW 11.05 COL 45.4 COLON-ALIGNED NO-LABEL
     FI-Oms AT ROW 11 COL 70 COLON-ALIGNED NO-LABEL
     FI-KundeNavn AT ROW 12.19 COL 19 COLON-ALIGNED
     RS-Filter AT ROW 12.19 COL 73 NO-LABEL
     T-HarMedlem AT ROW 13.62 COL 21
     T-HarKundeKort AT ROW 14.57 COL 21
     T-HarOmsetning AT ROW 13.71 COL 51
     T-HarSaldo AT ROW 14.67 COL 51
     T-HarKreditSperre AT ROW 13.71 COL 80
     T-HarOpphort AT ROW 14.67 COL 80
     T-Butikk AT ROW 18.1 COL 6.2
     B-Butikker AT ROW 19.1 COL 5.4
     B-Kunde AT ROW 20.29 COL 5.4
     B-KundeGruppe AT ROW 21.48 COL 5.4
     B-KundeType AT ROW 22.67 COL 5.4
     BUTTON-SokTilSKjop AT ROW 9.81 COL 68
     B-Bydel AT ROW 19.1 COL 55.4
     B-PostNr AT ROW 20.29 COL 55.4
     B-Kommune AT ROW 21.48 COL 55.4
     B-Fylke AT ROW 22.67 COL 55.4
     FI-Butiker AT ROW 19.1 COL 28.4 COLON-ALIGNED NO-LABEL
     FI-Bydel AT ROW 19.1 COL 78.4 COLON-ALIGNED NO-LABEL
     FI-Kunde AT ROW 20.29 COL 28.4 COLON-ALIGNED NO-LABEL
     FI-PostNr AT ROW 20.29 COL 78.4 COLON-ALIGNED NO-LABEL
     FI-Kundegruppe AT ROW 21.48 COL 28.4 COLON-ALIGNED NO-LABEL
     FI-Kommune AT ROW 21.48 COL 78.4 COLON-ALIGNED NO-LABEL
     FI-Kundetype AT ROW 22.67 COL 28.4 COLON-ALIGNED NO-LABEL
     FI-Fylke AT ROW 22.67 COL 78.4 COLON-ALIGNED NO-LABEL
     FI-Info AT ROW 24.43 COL 2 NO-LABEL
     BUTTON-SokTilFKjop AT ROW 8.62 COL 68
     BUTTON-SokTilEndret AT ROW 7.43 COL 68
     BUTTON-SokTilOpprettet AT ROW 6.24 COL 68
     BUTTON-SokFraSKjop AT ROW 9.76 COL 41.4
     BUTTON-SokFraFKjop AT ROW 8.62 COL 41.4
     BUTTON-SokFraEndret AT ROW 7.43 COL 41.4
     BUTTON-SokFraOpprettet AT ROW 6.24 COL 41.4
     B-Bygg AT ROW 1.29 COL 7
     B-Exit AT ROW 1.24 COL 131
     B-Forrige AT ROW 1.29 COL 32
     B-Neste AT ROW 1.29 COL 27
     B-Ny AT ROW 1.29 COL 2
     B-Slett AT ROW 1.29 COL 12
     B-Utskrift AT ROW 1.29 COL 22
     Btn_Help AT ROW 1.24 COL 126
     FILL-IN-Tekst AT ROW 16.91 COL 2.4 NO-LABEL
     BUTTON-SokFraOmsDato AT ROW 11 COL 41
     RECT-10 AT ROW 5.81 COL 1.6
     RECT-2 AT ROW 17.91 COL 1.8
     RECT-6 AT ROW 2.91 COL 1.4
     RECT-7 AT ROW 1.1 COL 1.6
     RECT-8 AT ROW 2.43 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 135.4 BY 24.62.

DEFINE FRAME FRAME-Progress
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 20.4 ROW 16
         SIZE 96.6 BY 1.43.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: Bydel T "NEW SHARED" NO-UNDO skotex Post
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kundelister"
         HEIGHT             = 24.62
         WIDTH              = 135.4
         MAX-HEIGHT         = 34.62
         MAX-WIDTH          = 204.4
         VIRTUAL-HEIGHT     = 34.62
         VIRTUAL-WIDTH      = 204.4
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Progress:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-Progress:MOVE-AFTER-TAB-ITEM (FI-Info:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR BUTTON B-Angre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Butikker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Bygg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Forrige IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Neste IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Slett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Utskrift IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Bydel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Eier IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Endret IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Fylke IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Info IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Kommune IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kunde IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kundegruppe IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kundetype IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ListeNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Merknad IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Opprettet IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PostNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Tekst IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME FRAME-Progress
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       FRAME FRAME-Progress:HIDDEN           = TRUE
       FRAME FRAME-Progress:SENSITIVE        = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

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
       ROW             = 1.67
       COLUMN          = 2.8
       HEIGHT          = .71
       WIDTH           = 93.4
       HIDDEN          = no
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8D2-850A-101B-AFC0-4210102A8DA7} type: ProgressBar */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kundelister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kundelister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Angre C-Win
ON CHOOSE OF B-Angre IN FRAME DEFAULT-FRAME
DO:
  if wModus = "NY" then
    do:
      find last Lister no-lock where
        Lister.ListeType = wListeType and
        Lister.Eier      = userid("dictdb") and
        Lister.ListeNr   > 0 no-error.

      assign
        wListerRecid = if available Lister 
                         then recid(Lister)
                         else ?
        FI-ListeNr:sensitive = false.
        
      if available Lister then
        do:
          run VisListe.
          RUN DefaultKnapper.
        end.
      else do:
        run DefaultVerdier.
        RUN RensSkjerm.
        apply "choose":U to B-Ny in frame DEFAULT-FRAME.
      end.
    end.
    
  else do:
    run VisListe.
    RUN DefaultKnapper.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Butikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Butikker C-Win
ON CHOOSE OF B-Butikker IN FRAME DEFAULT-FRAME /* Butikker... */
DO:
  def var IO-Liste as char no-undo.

  if wButiker = wAlle then
    RUN InitButiker.

  assign
    IO-Liste = if wvButiker = wAlle
                 then ""
                 else wvButiker.
    
  run d-tagbutiker.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvButiker = if IO-Liste = ""
                       then wAlle
                     else if IO-Liste = wButiker
                       then wAlle
                     else IO-Liste
    FI-Butiker    = if wvButiker = wAlle
                      then wAlle
                      else string(num-entries(wvButiker)) + " av " + string(num-entries(wButiker)).
  display 
    FI-Butiker
  with frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Butikker C-Win
ON RETURN OF B-Butikker IN FRAME DEFAULT-FRAME /* Butikker... */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Bydel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bydel C-Win
ON CHOOSE OF B-Bydel IN FRAME DEFAULT-FRAME /* Bydel... */
DO:
  def var IO-Liste as char no-undo.

  if wBydel = wAlle then 
    RUN InitByDel.
    
  assign
    IO-Liste = if wvBydel = wAlle
                 then ""
                 else wvBydel.
  
  run d-tagbydel.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvBydel = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wBydel
                      then wAlle
                    else IO-Liste
    FI-Bydel     = if wvBydel = wAlle
                      then wAlle
                      else string(num-entries(wvBydel)) + " av " + string(num-entries(wBydel)).
  display 
    FI-Bydel
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bydel C-Win
ON RETURN OF B-Bydel IN FRAME DEFAULT-FRAME /* Bydel... */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Bygg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bygg C-Win
ON CHOOSE OF B-Bygg IN FRAME DEFAULT-FRAME /* Bygg liste... */
DO:
  run LagreListe.
  if return-value = "AVBRYT" then
    return no-apply.

  assign
    wModus       = "ENDRE".
  RUN DefaultKnapper.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Win
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Forrige
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Forrige C-Win
ON CHOOSE OF B-Forrige IN FRAME DEFAULT-FRAME /* Forrige */
DO:
 find Lister no-lock where
   recid(Lister) = wListerRecid no-error.
 find prev Lister where
   Lister.ListeType = wListeType no-error.
 if not available Lister then
   find first Lister where
     Lister.ListeType = wListeType no-error.
 if available Lister then
   do:
     assign 
       wListerRecid = recid(Lister).
     RUN DefaultVerdier.
     RUN VisListe.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Fylke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Fylke C-Win
ON CHOOSE OF B-Fylke IN FRAME DEFAULT-FRAME /* Fylke... */
DO:
  def var IO-Liste as char no-undo.

  if wFylke = wAlle then 
    RUN InitFylke.
    
  assign
    IO-Liste = if wvFylke = wAlle
                 then ""
                 else wvFylke.
  
  run d-tagfylke.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvFylke = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wFylke
                      then wAlle
                    else IO-Liste
    FI-Fylke     = if wvFylke = wAlle
                      then wAlle
                      else string(num-entries(wvFylke)) + " av " + string(num-entries(wFylke)).
  display 
    FI-Fylke
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Fylke C-Win
ON RETURN OF B-Fylke IN FRAME DEFAULT-FRAME /* Fylke... */
DO:
  APPLY "ENTRY" TO FI-FraOpprettet.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kommune
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kommune C-Win
ON CHOOSE OF B-Kommune IN FRAME DEFAULT-FRAME /* Kommune... */
DO:
  def var IO-Liste as char no-undo.

  if wKommune = wAlle then 
    RUN InitKommune.
    
  assign
    IO-Liste = if wvKommune = wAlle
                 then ""
                 else wvKommune.
  
  run d-tagkommune.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvKommune = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wKommune
                      then wAlle
                    else IO-Liste
    FI-Kommune     = if wvKommune = wAlle
                      then wAlle
                      else string(num-entries(wvKommune)) + " av " + string(num-entries(wKommune)).
  display 
    FI-Kommune
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kommune C-Win
ON RETURN OF B-Kommune IN FRAME DEFAULT-FRAME /* Kommune... */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kunde C-Win
ON CHOOSE OF B-Kunde IN FRAME DEFAULT-FRAME /* Kunder... */
DO:
  def var IO-Liste as char no-undo.

  if wKunder = wAlle then 
    RUN InitKunder.
    
  assign
    IO-Liste = if wvKunder = wAlle
                 then ""
                 else wvKunder.
  
  run d-tagkunde.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvKunder = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wKunder
                      then wAlle
                    else IO-Liste
    FI-Kunde     = if wvKunder = wAlle
                      then wAlle
                      else string(num-entries(wvKunder)) + " av " + string(num-entries(wKunder)).
  display 
    FI-Kunde
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kunde C-Win
ON RETURN OF B-Kunde IN FRAME DEFAULT-FRAME /* Kunder... */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KundeGruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeGruppe C-Win
ON CHOOSE OF B-KundeGruppe IN FRAME DEFAULT-FRAME /* Kundegrupper... */
DO:
  def var IO-Liste as char no-undo.

  if wKundeGruppe = wAlle then 
    RUN InitKundeGrupper.
    
  assign
    IO-Liste = if wvKundeGruppe = wAlle
                 then ""
                 else wvKundeGruppe.
  
  run d-tagkundegruppe.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvKundeGruppe = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wKundeGruppe
                      then wAlle
                    else IO-Liste
    FI-KundeGruppe     = if wvKundeGruppe = wAlle
                      then wAlle
                      else string(num-entries(wvKundeGruppe)) + " av " + string(num-entries(wKundeGruppe)).
  display 
    FI-KundeGruppe
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeGruppe C-Win
ON RETURN OF B-KundeGruppe IN FRAME DEFAULT-FRAME /* Kundegrupper... */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KundeType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeType C-Win
ON CHOOSE OF B-KundeType IN FRAME DEFAULT-FRAME /* KundeTyper... */
DO:
  def var IO-Liste as char no-undo.

  if wKundeType = wAlle then 
    RUN InitKundeType.
    
  assign
    IO-Liste = if wvKundeType = wAlle
                 then ""
                 else wvKundeType.
  
  run d-tagkundetype.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvKundetype = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wKundeType
                      then wAlle
                    else IO-Liste
    FI-KundeType = if wvKundeType = wAlle
                      then wAlle
                      else string(num-entries(wvKundeType)) + " av " + string(num-entries(wKundeType)).
  display 
    FI-KundeType
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeType C-Win
ON RETURN OF B-KundeType IN FRAME DEFAULT-FRAME /* KundeTyper... */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Neste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Neste C-Win
ON CHOOSE OF B-Neste IN FRAME DEFAULT-FRAME
DO:
 find Lister no-lock where
   recid(Lister) = wListerRecid no-error.
 find next Lister where
   Lister.ListeType = wListeType no-error.
 if not available Lister then
   find last Lister where
     Lister.ListeType = wListeType no-error.
 if available Lister then
   do:
     assign 
       wListerRecid = recid(Lister).
     RUN DefaultVerdier.
     RUN VisListe.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny C-Win
ON CHOOSE OF B-Ny IN FRAME DEFAULT-FRAME /* Ny kolleksjonsliste */
DO:
  run RensSkjerm.
  run DefaultVerdier.

  assign
    wModus               = "NY"
    B-Ny:sensitive       = false
    B-Bygg:sensitive     = true
    B-Slett:sensitive    = false
    B-Angre:sensitive    = true
    B-Utskrift:sensitive = false
    B-butikker:sensitive = false
    FI-ListeNr:sensitive = true
    B-SokListe:sensitive = false
    B-Forrige:sensitive  = false
    B-Neste:sensitive    = false.
    
  /* Finner ledig listenummer */
  find last Lister no-lock where
     Lister.ListeType = wListeType no-error.
  if available Lister then
    FI-ListeNr = Lister.ListeNr + 1.
  else
    FI-ListeNr = 1.
  display FI-ListeNr with frame DEFAULT-FRAME.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-PostNr C-Win
ON CHOOSE OF B-PostNr IN FRAME DEFAULT-FRAME /* PostNummer... */
DO:
  def var IO-Liste as char no-undo.

  if wPostNr = wAlle then 
    RUN InitPost.
    
  assign
    IO-Liste = if wvPostNr = wAlle
                 then ""
                 else wvPostNr.
  
  run d-tagpost.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvPostNr = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wPostNr
                      then wAlle
                    else IO-Liste
    FI-PostNr     = if wvPostNr = wAlle
                      then wAlle
                      else string(num-entries(wvPostNr)) + " av " + string(num-entries(wPostNr)).
  display 
    FI-PostNr
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-PostNr C-Win
ON RETURN OF B-PostNr IN FRAME DEFAULT-FRAME /* PostNummer... */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett C-Win
ON CHOOSE OF B-Slett IN FRAME DEFAULT-FRAME /* Slett kolleksjonsliste */
DO:
  message "Nullstille liste (Ja), ta bort liste (Nei)?"
    view-as alert-box question button YES-NO-CANCEL title "Bekreft"
    update wSvar as log.
  
  if wSvar then
    RUN SlettKunder.
  else if wSvar = false then
    run SlettLister.
  else
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokListe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokListe C-Win
ON CHOOSE OF B-SokListe IN FRAME DEFAULT-FRAME
or F10 of B-SokListe
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-ListeNr
    &Program     = d-bLister.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Lister no-lock where
                    recid(Lister) = int(return-value) no-error."
    &ExtraParam  = "wListeType"
  }   
  if available Lister then
    do:
      assign
        wListerRecid = recid(Lister).
      RUN RensSkjerm.
      RUN VisListe.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Utskrift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Utskrift C-Win
ON CHOOSE OF B-Utskrift IN FRAME DEFAULT-FRAME /* Utskrift av kolleksjonsliste... */
DO:
  if not available Lister then 
    return.
    
  assign frame DEFAULT-FRAME
    FI-ListeNr.
    
  run w-rutskrkolleksjon.w persistent set wUtskriftProg (Lister.ListeType).
  if valid-handle(wUtskriftProg) then
    run InitListe in wUtskriftProg (input wListerRecid).
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


&Scoped-define SELF-NAME BUTTON-SokFraEndret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFraEndret C-Win
ON CHOOSE OF BUTTON-SokFraEndret IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraEndret
DO:

  def var wTittel as char no-undo.
  assign FI-FraEndret = date(FI-FraEndret:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraEndret
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFraFKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFraFKjop C-Win
ON CHOOSE OF BUTTON-SokFraFKjop IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraFKjop
DO:

  def var wTittel as char no-undo.
  assign FI-FraFKjop = date(FI-FraFKjop:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraFKjop
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFraOmsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFraOmsDato C-Win
ON CHOOSE OF BUTTON-SokFraOmsDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraOmsDato
DO:

  def var wTittel as char no-undo.
  assign FI-FraOmsDato = date(FI-FraOmsDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraOmsDato   
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFraOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFraOpprettet C-Win
ON CHOOSE OF BUTTON-SokFraOpprettet IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraOpprettet
DO:

  def var wTittel as char no-undo.
  assign FI-FraOpprettet = date(FI-FraOpprettet:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraOpprettet
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFraSKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFraSKjop C-Win
ON CHOOSE OF BUTTON-SokFraSKjop IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraSKjop
DO:

  def var wTittel as char no-undo.
  assign FI-FraSKjop = date(FI-FraSKjop:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraSKjop
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTilEndret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTilEndret C-Win
ON CHOOSE OF BUTTON-SokTilEndret IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilEndret
DO:

  def var wTittel as char no-undo.
  assign FI-TilEndret = date(FI-TilEndret:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilEndret
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTilFKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTilFKjop C-Win
ON CHOOSE OF BUTTON-SokTilFKjop IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilFKjop
DO:

  def var wTittel as char no-undo.
  assign FI-TilFKjop = date(FI-TilFKjop:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilFKjop
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTillOmsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTillOmsDato C-Win
ON CHOOSE OF BUTTON-SokTillOmsDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilOmsDato
DO:

  def var wTittel as char no-undo.
  assign FI-TilOmsDato = date(FI-TilOmsDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilOmsDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTilOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTilOpprettet C-Win
ON CHOOSE OF BUTTON-SokTilOpprettet IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilOpprettet
DO:

  def var wTittel as char no-undo.
  assign FI-TilOpprettet = date(FI-TilOpprettet:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilOpprettet
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTilSKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTilSKjop C-Win
ON CHOOSE OF BUTTON-SokTilSKjop IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilSKjop
DO:

  def var wTittel as char no-undo.
  assign FI-TilSKjop = date(FI-TilSKjop:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilSKjop
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraEndret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraEndret C-Win
ON DELETE-CHARACTER OF FI-FraEndret IN FRAME DEFAULT-FRAME /* Endret */
DO:
    ASSIGN
        FI-FraEndret:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraFKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraFKjop C-Win
ON DELETE-CHARACTER OF FI-FraFKjop IN FRAME DEFAULT-FRAME /* 1. kjøp */
DO:
    ASSIGN
        FI-FraFKjop:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraOmsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraOmsDato C-Win
ON DELETE-CHARACTER OF FI-FraOmsDato IN FRAME DEFAULT-FRAME /* Omsetning */
DO:
    ASSIGN
        FI-FraOmsDato:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraOpprettet C-Win
ON DELETE-CHARACTER OF FI-FraOpprettet IN FRAME DEFAULT-FRAME /* Opprettet */
DO:
    ASSIGN
        FI-FraOpprettet:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraSKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraSKjop C-Win
ON DELETE-CHARACTER OF FI-FraSKjop IN FRAME DEFAULT-FRAME /* Siste kjøp */
DO:
    ASSIGN
        FI-FraSKjop:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilEndret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilEndret C-Win
ON DELETE-CHARACTER OF FI-TilEndret IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        FI-TilEndret:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilFKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilFKjop C-Win
ON DELETE-CHARACTER OF FI-TilFKjop IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        FI-TilFKjop:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilOmsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilOmsDato C-Win
ON DELETE-CHARACTER OF FI-TilOmsDato IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        FI-TilOmsDato:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilOpprettet C-Win
ON DELETE-CHARACTER OF FI-TilOpprettet IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        FI-TilOpprettet:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilSKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilSKjop C-Win
ON DELETE-CHARACTER OF FI-TilSKjop IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        FI-TilSKjop:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Filter C-Win
ON RETURN OF RS-Filter IN FRAME DEFAULT-FRAME
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Butikk C-Win
ON RETURN OF T-Butikk IN FRAME DEFAULT-FRAME /* Pr. butikk */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Butikk C-Win
ON VALUE-CHANGED OF T-Butikk IN FRAME DEFAULT-FRAME /* Pr. butikk */
DO:
  assign
    B-Butikker:sensitive = input T-Butikk
    wvButiker = if input T-Butikk
                  then wvButiker
                  else wAlle.
  
  assign
    FI-Butiker  = if wvButiker = wAlle
                    then wAlle
                    else string(num-entries(wvButiker))      + " av " + string(num-entries(wButiker)).
  display 
    FI-Butiker
    wAlle when FI-Butiker = "" @ FI-Butiker
  with frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-HarKreditSperre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HarKreditSperre C-Win
ON RETURN OF T-HarKreditSperre IN FRAME DEFAULT-FRAME /* Har kredittsperre */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-HarKundeKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HarKundeKort C-Win
ON RETURN OF T-HarKundeKort IN FRAME DEFAULT-FRAME /* Har kundekort */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-HarMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HarMedlem C-Win
ON RETURN OF T-HarMedlem IN FRAME DEFAULT-FRAME /* Har medlemmer */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-HarOmsetning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HarOmsetning C-Win
ON RETURN OF T-HarOmsetning IN FRAME DEFAULT-FRAME /* Har omsetning */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-HarOpphort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HarOpphort C-Win
ON RETURN OF T-HarOpphort IN FRAME DEFAULT-FRAME /* Har opphørt */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-HarSaldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-HarSaldo C-Win
ON RETURN OF T-HarSaldo IN FRAME DEFAULT-FRAME /* Har saldo */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{syspara.i 7 1 3 wListeType}
{syspara.i 1 100 1 wAlle}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   IF VALID-HANDLE(chCtrlFrame) THEN
       RELEASE OBJECT chCtrlFrame NO-ERROR.
   IF VALID-HANDLE(CtrlFrame) THEN
       DELETE OBJECT CtrlFrame NO-ERROR.
   ASSIGN CtrlFrame   = ?
          chCtrlFrame = ?.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i} 
  assign
    frame DEFAULT-FRAME:hidden = false.
  display
    FI-ListeNr
  with frame DEFAULT-FRAME.

  RUN InitParametre.
    
  find last Lister no-lock where
    Lister.ListeType = wListeType and
    Lister.Eier      = userid("dictdb") and
    Lister.ListeNr   > 0 no-error.

  /* Initierer bydel */
  ASSIGN
      FI-Info:SCREEN-VALUE = "Initiering av bydel pågår. Vent litt...".
  RUN InitBydel.
  ASSIGN
      FI-Info:SCREEN-VALUE = "".

  apply "choose":U to B-Ny. /* Setter NY modus ved oppstart av program.*/   
  if wModus = "NY" then  
    apply "entry":U to FI-ListeNr.
  else
    apply "entry":U to FI-Navn.
   
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

OCXFile = SEARCH( "w-rbyggkunde.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-rbyggkunde.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultKnapper C-Win 
PROCEDURE DefaultKnapper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame {&FRAME-NAME}:

  assign
    B-butikker:sensitive = input T-Butikk
    B-Ny:sensitive       = true
    B-Bygg:sensitive     = true
    B-Slett:sensitive    = true
    B-Angre:sensitive    = true
    B-Utskrift:sensitive = true
    FI-ListeNr:sensitive = false
    B-SokListe:sensitive = true
    B-Forrige:sensitive  = true
    B-Neste:sensitive    = true.
end.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultVerdier C-Win 
PROCEDURE DefaultVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Default er allt valgt. */
  assign
    FI-Eier        = userid("DictDB")
    wAktiv         = 3
    wvKunder       = wAlle
    wvButiker      = wAlle
    wvKundeGruppe  = wAlle
    wvKundeType    = wAlle
    wvBydel        = wAlle
    wvPostNr       = wAlle
    wvKommune      = wAlle
    wvFylke        = wAlle
    FI-Kunde       = if wvKunder = wAlle
                       then wAlle
                       else string(num-entries(wvKunder))  + " av " + string(num-entries(wKunder)) 
    FI-Butiker     = if wvButiker = wAlle
                       then wAlle
                       else string(num-entries(wvButiker)) + " av " + string(num-entries(wButiker))
    FI-KundeGruppe = IF wvKundeGruppe  = wAlle
                       then wAlle
                       else string(num-entries(wvKundeGruppe)) + " av " + string(num-entries(wKundeGruppe))
    FI-KundeType   = IF wvKundeType    = wAlle
                       then wAlle
                       else string(num-entries(wvKundeType)) + " av " + string(num-entries(wKundeType))
    FI-Bydel       = IF wvBydel        = wAlle
                       then wAlle
                       else string(num-entries(wvBydel)) + " av " + string(num-entries(wBydel))
    FI-PostNr      = IF wvPostNr       = wAlle
                       then wAlle
                       else string(num-entries(wvPostNr)) + " av " + string(num-entries(wPostNr))
    FI-Kommune     = IF wvKommune      = wAlle
                       then wAlle
                       else string(num-entries(wvKommune)) + " av " + string(num-entries(wKommune))
    FI-Fylke       = IF wvFylke        = wAlle
                       then wAlle
                       else string(num-entries(wvFylke)) + " av " + string(num-entries(wFylke))
    .

  FI-Info = "".
  display 
    FI-Info
    FI-Eier
    FI-Kunde  
    FI-Butiker
    FI-KundeGruppe 
    FI-KundeType   
    FI-Bydel       
    FI-PostNr      
    FI-Kommune     
    FI-Fylke       
  with frame {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disp-Info C-Win 
PROCEDURE Disp-Info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wInfo as char no-undo.
  
  display wInfo @ FI-Info with frame {&FRAME-NAME}.

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
  DISPLAY FI-ListeNr FI-Navn FI-Merknad FI-Opprettet FI-Endret FI-Eier 
          FI-FraOpprettet FI-TilOpprettet FI-FraEndret FI-TilEndret FI-FraFKjop 
          FI-TilFKjop FI-FraSKjop FI-TilSKjop FI-FraOmsDato FI-TilOmsDato FI-Oms 
          FI-KundeNavn RS-Filter T-HarMedlem T-HarKundeKort T-HarOmsetning 
          T-HarSaldo T-HarKreditSperre T-HarOpphort T-Butikk FI-Butiker FI-Bydel 
          FI-Kunde FI-PostNr FI-Kundegruppe FI-Kommune FI-Kundetype FI-Fylke 
          FI-Info FILL-IN-Tekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-SokTillOmsDato B-SokListe FI-Navn FI-Merknad FI-FraOpprettet 
         FI-TilOpprettet FI-FraEndret FI-TilEndret FI-FraFKjop FI-TilFKjop 
         FI-FraSKjop FI-TilSKjop FI-FraOmsDato FI-TilOmsDato FI-Oms 
         FI-KundeNavn RS-Filter T-HarMedlem T-HarKundeKort T-HarOmsetning 
         T-HarSaldo T-HarKreditSperre T-HarOpphort T-Butikk B-Kunde 
         B-KundeGruppe B-KundeType BUTTON-SokTilSKjop B-Bydel B-PostNr 
         B-Kommune B-Fylke BUTTON-SokTilFKjop BUTTON-SokTilEndret 
         BUTTON-SokTilOpprettet BUTTON-SokFraSKjop BUTTON-SokFraFKjop 
         BUTTON-SokFraEndret BUTTON-SokFraOpprettet B-Exit B-Ny Btn_Help 
         BUTTON-SokFraOmsDato RECT-10 RECT-2 RECT-6 RECT-7 RECT-8 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Progress}
  FRAME FRAME-Progress:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButiker C-Win 
PROCEDURE InitButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FI-Info = "Initiering av butikker pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  /* Butikker */
  wButiker = "".
  for each Butiker no-lock:
    assign
      wButiker = wButiker + 
                 (if wButiker = "" 
                    then ""
                    else ",") +
                 string(Butiker.Butik).
  end.

  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitBydel C-Win 
PROCEDURE InitBydel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER initKunde FOR Kunde.

  FOR EACH initKunde NO-LOCK:
    IF NOT CAN-FIND(Bydel WHERE
                    Bydel.PostNr = initKunde.BydelsNr) THEN
    DO:
        FIND Post NO-LOCK WHERE
            Post.PostNr = initKunde.PostNr NO-ERROR.

        CREATE Bydel.
        ASSIGN
            Bydel.PostNr      = initKunde.BydelsNr
            Bydel.Beskrivelse = Post.Beskrivelse
            .
    END.
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFylke C-Win 
PROCEDURE InitFylke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wBlank AS LOG INITIAL FALSE NO-UNDO.
                     
  FI-Info = "Initiering av fylker pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wFylke = "".

  /* Fylke */
  for each Fylke no-lock:
    assign
      wBlank = IF Fylke.FylkesNr = "" THEN TRUE ELSE wBlank
      wFylke = wFylke + 
                     (if wFylke = "" 
                        then ""
                        else ",") +
                     string(Fylke.FylkesNr).
  end.
  IF wBlank THEN
      wFylke = "," + wFylke.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKommune C-Win 
PROCEDURE InitKommune :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wBlank AS LOG INITIAL FALSE NO-UNDO.

  FI-Info = "Initiering av kommuner pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wKommune = "".

  /* Kunder */
  for each Kommune no-lock:
    assign
      wBlank   = IF Kommune.KommNr = "" THEN TRUE ELSE wBlank
      wKommune = wKommune + 
                     (if wKommune = "" 
                        then ""
                        else ",") +
                     string(Kommune.KommNr).
  end.
  IF wBlank THEN
      wKommune = "," + wKommune.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKundeGrupper C-Win 
PROCEDURE InitKundeGrupper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FI-Info = "Initiering av kundegrupper pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wKundeGruppe = "".

  /* Kunder */
  for each KundeGruppe no-lock:
    assign
      wKundeGruppe = wKundeGruppe + 
                     (if wKundeGruppe = "" 
                        then ""
                        else ",") +
                     string(KundeGruppe.GruppeId).
  end.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKunder C-Win 
PROCEDURE InitKunder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FI-Info = "Initiering av kunder pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wKunder = "".

  /* Kunder */
  for each Kunde no-lock:
    assign
      wKunder = wKunder + 
                     (if wKunder = "" 
                        then ""
                        else ",") +
                     string(Kunde.KundeNr).
  end.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKundeType C-Win 
PROCEDURE InitKundeType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FI-Info = "Initiering av kundetyper pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wKundeType = "".

  /* Kundetyper */
  for each KundeType no-lock:
    assign
      wKundeType = wKundeType + 
                     (if wKundeType = "" 
                        then ""
                        else ",") +
                     string(KundeType.TypeId).
  end.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitParametre C-Win 
PROCEDURE InitParametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {sww.i}
  assign
    wKunder       = wAlle
    wButiker      = wAlle  
    wKundeGruppe  = wAlle
    wKundeType    = wAlle
    wBydel        = wAlle
    wPostNr       = wAlle
    wKommune      = wAlle
    wFylke        = wAlle

    wvKunder      = wAlle
    wvButiker     = wAlle
    wvKundeGruppe = wAlle
    wvKundeType   = wAlle
    wvBydel       = wAlle
    wvPostNr      = wAlle
    wvKommune     = wAlle
    wvFylke       = wAlle
    .   
  
  run DefaultVerdier.
  {swn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitPost C-Win 
PROCEDURE InitPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wFlagg AS LOG INITIAL FALSE NO-UNDO.
                                        
  FI-Info = "Initiering av postnummer pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wPostNr = "".

  /* Kunder */
  for each Post no-lock:
    assign
      wFlagg  = IF Post.PostNR = "" THEN TRUE ELSE wFlagg
      wPostNr = wPostNr + 
                     (if wPostNr = "" 
                        then ""
                        else ",") +
                     string(Post.PostNr).
  end.
  /* Legger på Blankt postnummer */
  IF wFlagg THEN
    ASSIGN
      wPostNr = " " + "," + wPostNr.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreListe C-Win 
PROCEDURE LagreListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wListeNr as int    no-undo.
def var wChild   as handle no-undo.
def var wSvar    as log    no-undo.

do with frame DEFAULT-FRAME:
  /* Bekrefte at lagring skal gjøres */
  message "Skal lagring starte?" 
    view-as alert-box question buttons YES-NO title "Bekreftelse"
    update wSvar.
  if wSvar <> true then
      return no-apply "AVBRYT".

  /* Finnes liste fra før? */
  if wModus = "Ny" then
    do:
      find Lister no-lock where
        Lister.ListeType = wListeType and
        Lister.ListeNr   = input FI-ListeNr  no-error.
      if available Lister then
        do:
          message "Liste med dette nummer finnes fra før!"
            view-as alert-box message title "Melding".
          return no-apply "AVBRYT".
        end.
    end.
  else do:
    find Lister no-lock where
        recid(Lister) = wListerRecid no-error.
    if not available Lister then
      do:
        message "Ukjent liste!"
          view-as alert-box message title "Melding".
        return "AVBRYT".
      end.          
  end.

  /* Sjekker om det er endring i butikk flagget */
  if T-Butikk <> input T-Butikk then
    BUTIKKSJEKK:
    do:
      if not can-find(first ListeLinje of Lister) then 
        leave BUTIKKSJEKK.
      if input T-butikk then
        do:
          wSvar = false.
          message "Listen er tidligere oppdatert for alle butikker." skip
                  "Listen må nullstilles før den kan oppdateres pr. butikk."
                  view-as alert-box question buttons yes-no title "Bekreft"
                  update wSvar.
          if wSvar = false then
            do:
              display T-Butikk with frame DEFAULT-FRAME.
              return "AVBRYT".
            end.
        end.
      else 
        do:
          wSvar = false.
          message "Listen er tidligere oppdatert pr. butikk." skip
                  "Listen må nullstilles før den kan oppdateres for alle butikker."
                  view-as alert-box question buttons yes-no title "Bekreft"
                  update wSvar.
          if wSvar = false then
            do:
              display T-Butikk with frame DEFAULT-FRAME.
              return "AVBRYT".
            end.
        end.
      RUN SlettKunder.
    end. /* BUTIKKSJEKK */
  
  {sww.i}    
  FI-Info = "Lagring av ny liste pågår....".
  run Disp-Info (FI-Info).

  /* Tar vare på skjermverdiene. */
  assign frame {&FRAME-NAME}
    FI-Navn
    FI-Merknad
    FI-Eier
    T-Butikk
    FI-FraOpprettet
    FI-TilOpprettet
    FI-FraEndret
    FI-TilEndret
    FI-FraFKjop
    FI-TilFKjop
    FI-FraSKjop
    FI-TilSKjop
    FI-FraOMsDato
    FI-TilOmsDato
    FI-Oms
    FI-KundeNavn
    RS-Filter
    T-HarMedlem
    T-HarKundeKort
    T-HarOmsetning
    T-HarSaldo
    T-HarKreditSperre
    T-HarOpphort
    . 

  if wModus = "NY" then
    run listehode.p ("", wModus, wListeType, output wListerRecid).
      
  /* Setter kriteriene inn i listenhode. */    
  do TRANSACTION:     
    find bLister exclusive-lock where
      recid(bLister) = wListerRecid.           
    assign
      bLister.Beskrivelse   = FI-Navn
      bLister.Merknad       = FI-Merknad
      bLister.Eier          = FI-Eier
      bLister.Kriterier[ 1] = wvKunder      + "|" + wKunder 
      bLister.Kriterier[ 2] = wvButiker     + "|" + wButiker
      bLister.Kriterier[ 3] = if T-Butikk          then "TRUE" else "FALSE"
      bLister.Kriterier[ 4] = wvKundeGruppe + "|" + wKundeGruppe 
      bLister.Kriterier[ 5] = wvKundeType   + "|" + wKundeType 
      bLister.Kriterier[ 6] = wvBydel       + "|" + wBydel 
      bLister.Kriterier[ 7] = wvPostNr      + "|" + wPostNr 
      bLister.Kriterier[ 8] = wvKommune     + "|" + wKommune 
      bLister.Kriterier[ 9] = wvFylke       + "|" + wFylke 
      bLister.Kriterier[10] = if T-HarMedlem       then "TRUE" else "FALSE"
      bLister.Kriterier[11] = if T-HarKundeKort    then "TRUE" else "FALSE"
      bLister.Kriterier[12] = if T-HarOmsetning    then "TRUE" else "FALSE"
      bLister.Kriterier[13] = if T-HarSaldo        then "TRUE" else "FALSE"
      bLister.Kriterier[14] = if T-HarKreditSperre then "TRUE" else "FALSE"
      bLister.Kriterier[15] = if T-HArOpphort      then "TRUE" else "FALSE"
      bLister.Kriterier[16] = (IF FI-FraOpprettet = ? THEN "?" ELSE string(FI-FraOpprettet)) + "," +
                              (IF FI-TilOpprettet = ? THEN "?" ELSE string(FI-TilOpprettet)) + "," +
                              (IF FI-FraEndret    = ? THEN "?" ELSE string(FI-FraEndret)) + "," +
                              (IF FI-TilEndret    = ? THEN "?" ELSE string(FI-TilEndret)) + "," +
                              (IF FI-FraFKjop     = ? THEN "?" ELSE string(FI-FraFKjop)) + "," +
                              (IF FI-TilFKjop     = ? THEN "?" ELSE string(FI-TilFKjop)) + "," +
                              (IF FI-FraSKjop     = ? THEN "?" ELSE string(FI-FraSKjop)) + "," +
                              (IF FI-TilSKjop     = ? THEN "?" ELSE string(FI-TilSKjop))  + "," +
                              (IF FI-FraOmsDato   = ? THEN "?" ELSE string(FI-FraOmsDato)) + "," +
                              (IF FI-TilOmsDato   = ? THEN "?" ELSE string(FI-TilOmsDato)) + "," +
                              STRING(FI-Oms)
      bLister.Kriterier[17] = string(FI-KundeNavn)
      bLister.Kriterier[18] = string(RS-Filter)
      .
         
      release bLister.
  end. /* TRANSACTION */
    
  find Lister no-lock where
    recid(Lister) = wListerRecid.           
  
  /* Her bygger og oppdaterer vi */
  if wModus = "NY" then
    do:
      run byggkunde.p persistent set wChild
                         (wListerRecid,
                          T-Butikk,
                          wvKunder,
                          wvButiker,
                          wvKundeGruppe,
                          wvKundeType,  
                          wvBydel,      
                          wvPostNr,     
                          wvKommune,    
                          wvFylke,      
                          T-HarMedlem,      
                          T-HarKundeKort,   
                          T-HarOmsetning,   
                          T-HarSaldo,       
                          T-HarKreditSperre,
                          T-HarOpphort,     
                          FI-FraOpprettet, 
                          FI-TilOpprettet, 
                          FI-FraEndret,    
                          FI-TilEndret,    
                          FI-FraFKjop,     
                          FI-TilFKjop,     
                          FI-FraSKjop,     
                          FI-TilSKjop,     
                          FI-FraOmsDato,
                          FI-TilOmsDato,
                          FI-Oms,
                          FI-KundeNavn,
                          RS-Filter,
                          this-procedure:handle).

    end.
  else do:
    run byggkunde.p persistent set wChild
                       (wListerRecid,
                        T-Butikk,
                        wvKunder,
                        wvButiker,
                        wvKundeGruppe,
                        wvKundeType,  
                        wvBydel,      
                        wvPostNr,     
                        wvKommune,    
                        wvFylke,      
                        T-HarMedlem,      
                        T-HarKundeKort,   
                        T-HarOmsetning,   
                        T-HarSaldo,       
                        T-HarKreditSperre,
                        T-HarOpphort,     
                        FI-FraOpprettet, 
                        FI-TilOpprettet, 
                        FI-FraEndret,    
                        FI-TilEndret,    
                        FI-FraFKjop,     
                        FI-TilFKjop,     
                        FI-FraSKjop,     
                        FI-TilSKjop,     
                        FI-FraOmsDato,
                        FI-TilOmsDato,
                        FI-Oms,
                        FI-KundeNavn,
                        RS-Filter,
                        this-procedure:handle).
  end.    

  {swn.i}    
  FI-Info = "".
  run Disp-Info (FI-Info).
  run VisListe.
end. /* FrameScoope */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensSkjerm C-Win 
PROCEDURE RensSkjerm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  assign
    FI-Merknad       = ""
    FI-ListeNr       = 0
    FI-Navn          = ""
    FI-Eier          = ""
    T-Butikk         = false.

  display
    FI-Merknad 
    FI-ListeNr   
    FI-Navn
    FI-Eier
    T-Butikk     
  with frame DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettKunder C-Win 
PROCEDURE SlettKunder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wAntall as int no-undo.

  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".
   
  /* DØDEN */
  for each ListeLinje of Lister exclusive-lock:
    assign
      wAntall = wAntall + 1
      FI-Info = "Antall poster slettet " + string(wAntall).
    display FI-Info with frame {&FRAME-NAME}.
    delete ListeLinje.
  end. /* DØDEN */
  assign
    FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettLister C-Win 
PROCEDURE SlettLister :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".
    
  DOEDEN:
  do transaction:
    RUN SlettKunder.
    find current Lister exclusive-lock.
    delete Lister.
  end. /* TRANSACTION. */
  
  find first Lister no-lock where
    Lister.ListeType = wListeType no-error.
  if available Lister then
    do:
      assign
        wListerRecid = recid(Lister).
      RUN VisListe.
      apply "entry":U to FI-Navn in frame DEFAULT-FRAME.
    end.
  else do:
    apply "choose:":U to B-Ny in frame DEFAULT-FRAME.
  end.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisListe C-Win 
PROCEDURE VisListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".
  
  /* Default er allt valgt. */
  assign
    wModus            = "ENDRE"
    FI-Opprettet      = string(Lister.RegistrertDato) + " " +
                        string(Lister.RegistrertTid,"HH:MM:SS") + " " +
                        Lister.RegistrertAv
    FI-Endret         = string(Lister.EDato) + " " +
                        string(Lister.ETid,"HH:MM:SS") + " " +
                        Lister.BrukerId
    FI-Info           = ""
    FI-Merknad        = Lister.Merknad
    FI-ListeNr        = Lister.ListeNr
    FI-Navn           = Lister.Beskrivelse
    FI-Eier           = Lister.Eier
    wvKunder          = entry(1,Lister.Kriterier[ 1],"|")
    wvButiker         = entry(1,Lister.Kriterier[ 2],"|")
    T-Butikk          = if Lister.Kriterier[3] = "TRUE" then TRUE else false
    wvKundeGruppe     = entry(1,Lister.Kriterier[ 4],"|")    
    wvKundeType       = entry(1,Lister.Kriterier[ 5],"|")
    wvBydel           = entry(1,Lister.Kriterier[ 6],"|")
    wvPostNr          = entry(1,Lister.Kriterier[ 7],"|")
    wvKommune         = entry(1,Lister.Kriterier[ 8],"|")
    wvFylke           = entry(1,Lister.Kriterier[ 9],"|")
    T-HarMedlem       = if Lister.Kriterier[10] = "TRUE" then TRUE else false    
    T-HarKundeKort    = if Lister.Kriterier[11] = "TRUE" then TRUE else false
    T-HarOmsetning    = if Lister.Kriterier[12] = "TRUE" then TRUE else false
    T-HarSaldo        = if Lister.Kriterier[13] = "TRUE" then TRUE else false
    T-HarKreditSperre = if Lister.Kriterier[14] = "TRUE" then TRUE else false 
    T-HarOpphort      = if Lister.Kriterier[15] = "TRUE" then TRUE else false
    FI-KundeNavn      = Lister.Kriterier[17]
    RS-Filter         = INT(Lister.Kriterier[18])
    .
    IF NUM-ENTRIES(Lister.Kriterier[16]) >= 11 THEN
    ASSIGN
      FI-FraOpprettet   = IF ENTRY(1,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(1,Lister.Kriterier[16]))
      FI-TilOpprettet   = IF ENTRY(2,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(2,Lister.Kriterier[16]))
      FI-FraEndret      = IF ENTRY(3,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(3,Lister.Kriterier[16]))
      FI-TilEndret      = IF ENTRY(4,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(4,Lister.Kriterier[16]))
      FI-FraFKjop       = IF ENTRY(5,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(5,Lister.Kriterier[16]))
      FI-TilFKjop       = IF ENTRY(6,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(6,Lister.Kriterier[16]))
      FI-FraSKjop       = IF ENTRY(7,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(7,Lister.Kriterier[16]))
      FI-TilSKjop       = IF ENTRY(8,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(8,Lister.Kriterier[16]))
      FI-FraOmsDato     = IF ENTRY(9,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(9,Lister.Kriterier[16]))
      FI-TilOmsDato     = IF ENTRY(10,Lister.Kriterier[16]) = "?" THEN ? ELSE date(ENTRY(10,Lister.Kriterier[16]))
      FI-Oms            = dec(ENTRY(11,Lister.Kriterier[16]))
      .
    ELSE ASSIGN
      FI-FraOpprettet   = ?
      FI-TilOpprettet   = ?
      FI-FraEndret      = ?
      FI-TilEndret      = ?
      FI-FraFKjop       = ?
      FI-TilFKjop       = ?
      FI-FraSKjop       = ?
      FI-TilSKjop       = ?
      FI-FraOmsDato     = ?
      FI-TilOmsDato     = ?
      .

  /* Initierer lister hvis det er nødvendig */
  if wvKunder <> wAlle and wKunder = wAlle then RUN InitKunder.
  if wvButiker     <> wAlle and wButiker     = wAlle then RUN InitButiker.    

  assign
    FI-Kunde      = if wvKunder = wAlle
                       then wAlle
                       else string(num-entries(wvKunder))  + " av " + string(num-entries(wKunder)) 
    FI-Butiker     = if wvButiker = wAlle
                       then wAlle
                       else string(num-entries(wvButiker))      + " av " + string(num-entries(wButiker))
    FI-KundeGruppe = IF wvKundeGruppe  = wAlle
                       then wAlle
                       else string(num-entries(wvKundeGruppe)) + " av " + string(num-entries(wKundeGruppe))
    FI-KundeType   = IF wvKundeType    = wAlle
                       then wAlle
                       else string(num-entries(wvKundeType)) + " av " + string(num-entries(wKundeType))
    FI-Bydel       = IF wvBydel        = wAlle
                       then wAlle
                       else string(num-entries(wvBydel)) + " av " + string(num-entries(wBydel))
    FI-PostNr      = IF wvPostNr       = wAlle
                       then wAlle
                       else string(num-entries(wvPostNr)) + " av " + string(num-entries(wPostNr))
    FI-Kommune     = IF wvKommune      = wAlle
                       then wAlle
                       else string(num-entries(wvKommune)) + " av " + string(num-entries(wKommune))
    FI-Fylke       = IF wvFylke        = wAlle
                       then wAlle
                       else string(num-entries(wvFylke)) + " av " + string(num-entries(wFylke))
    .

  display
    FI-Opprettet
    FI-Endret   
    FI-Merknad 
    FI-ListeNr   
    FI-Navn
    FI-Eier
    T-Butikk     
    FI-Info
    T-HarMedlem      
    T-HarKundeKort   
    T-HarOmsetning   
    T-HarSaldo       
    T-HarKreditSperre
    T-HarOpphort     
    FI-FraOpprettet  
    FI-TilOpprettet  
    FI-FraEndret     
    FI-TilEndret     
    FI-FraFKjop      
    FI-TilFKjop      
    FI-FraSKjop      
    FI-TilSKjop      
    FI-FraOmsDato
    FI-TilOmsDato
    FI-Oms
    FI-KundeNavn     
    RS-Filter        
    FI-Kunde  
    wAlle when FI-Kunde       = "" @ FI-Kunde
    FI-Butiker
    wAlle when FI-Butiker     = "" @ FI-Butiker
    FI-KundeGruppe 
    wAlle when FI-KundeGruppe = "" @ FI-KundeGruppe
    FI-KundeType   
    wAlle when FI-KundeType   = "" @ FI-KundeType
    FI-Bydel       
    wAlle when FI-Bydel       = "" @ FI-Bydel
    FI-PostNr      
    wAlle when FI-PostNr      = "" @ FI-PostNr
    FI-Kommune     
    wAlle when FI-Kommune     = "" @ FI-Kommune
    FI-Fylke       
    wAlle when FI-Fylke       = "" @ FI-Fylke
  with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Hex2Int C-Win 
FUNCTION Hex2Int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN INT(ENTRY(3,wRGB)) * 65536 +
         INT(ENTRY(2,wRGB)) * 256   +
         INT(ENTRY(1,wRGB)).         
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

