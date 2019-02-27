&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-KundKort


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tStLinje NO-UNDO LIKE StLinje.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-KundKort 
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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  def var wInputRecid as recid no-undo.
  def var wModus       as char  no-undo. /* NY, ENDRE, SLETT */
  assign 
    wModus = "ENDRE". /* Default */
 find first Kunde no-lock no-error.
  if available Kunde then
    assign wInputRecid = recid(Kunde).
&ELSE
  def input parameter wInputRecid as recid no-undo.
  def input parameter wModus       as char  no-undo. /* NY, ENDRE, SLETT */
&ENDIF

/* Local Variable Definitions ---                                       */
def var wOk                as log    no-undo.
def var wOldRecid          as recid  no-undo.
def var hHandle            as handle no-undo.
def var hLabel             as handle no-undo.
def var wSubWin1           as handle no-undo.
def var wSubWin2           as handle no-undo.
def var wSubWin3           as handle no-undo.
def var wSubWin4           as handle no-undo.
def var wSubWin5           as handle no-undo.
def var wLapTop            as log    no-undo.
def var wBekreft           as log    no-undo.
def var wFeil              as log    no-undo.
DEF VAR wLedige            AS INT    NO-UNDO.
DEF VAR wDbId              AS char   NO-UNDO.
DEF VAR wMedlemsNr         LIKE Medlem.MedlemsNr NO-UNDO.
DEF VAR wKortNummer        AS CHAR   NO-UNDO.
DEF VAR wCl                AS INT    NO-UNDO.
DEF VAR cTekst             AS CHAR   NO-UNDO.
DEF VAR wSaldo             AS DEC    NO-UNDO.
DEF VAR wTotal             AS DEC    NO-UNDO.

/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
define var chTab1Side        as com-handle no-undo.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.
DEFINE VAR wTabTekst         AS CHAR INIT "Vedlikehold,Salgstransaksjoner,Betalingstransaksjoner,Medlemmer" NO-UNDO.
DEFINE VAR wTabHnd           AS HANDLE EXTENT 5 NO-UNDO.
DEFINE VAR wBrowseHandle     AS HANDLE NO-UNDO.

/* Buffere */
def buffer bKunde for Kunde.
DEF BUFFER ledKunde FOR Kunde.

def temp-table tmpChild 
  field wChild as handle.
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
&Scoped-Define ENABLED-OBJECTS B-AltS RECT-27 RECT-28 B-Medlem ~
B-OppdatSaldo CB-Sort B-GDPR BUTTON-SokMedlem BUTTON-Angre BUTTON-Kopier ~
BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev BUTTON-Slett Btn_Help ~
BUTTON-Ok 
&Scoped-Define DISPLAYED-OBJECTS CB-Sort FILL-IN-KundeNr FILL-IN-Navn ~
FI-Saldo FI-Totalkjop FI-AntMedlemmer FI-ForsteKjop FI-SisteKjop 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KundeNummer C-KundKort 
FUNCTION KundeNummer RETURNS CHARACTER
  ( ipKundeNr AS dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Modifierad C-KundKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-KundKort AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-ArtKort MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AltS 
     IMAGE-UP FILE "icon/select.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Søk" 
     SIZE 4.6 BY 1.1 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE BUTTON B-GDPR 
     LABEL "GDPR" 
     SIZE 22 BY 1.

DEFINE BUTTON B-Medlem 
     LABEL "&Opprett medlem..." 
     SIZE 22 BY 1.05.

DEFINE BUTTON B-OppdatSaldo 
     LABEL "Oppdater saldo" 
     SIZE 20 BY 1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon\e-undo":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON BUTTON-Kopier 
     IMAGE-UP FILE "icon\e-copy":U NO-FOCUS FLAT-BUTTON
     LABEL "&Kopiera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Kopier post  (Alt-K)".

DEFINE BUTTON BUTTON-Lagre 
     IMAGE-UP FILE "icon\e-save":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon\e-pilned":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon\e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon\e-pilopp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon\e-del":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON BUTTON-SokKund-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE BUTTON BUTTON-SokMedlem 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL " 1: Vg/LpNr" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Velg sorteringsordning som skal gjelde for blaing med pil Opp/Ned." NO-UNDO.

DEFINE VARIABLE FI-AntMedlemmer AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall medlemmer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ForsteKjop AS DATE FORMAT "99/99/99":U 
     LABEL "Første gang kjøpt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Saldo AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Utestående saldo" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SisteKjop AS DATE FORMAT "99/99/99":U 
     LABEL "Siste gang kjøpt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Totalkjop AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Totalt kjøp" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KundeNr AS INTEGER FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "KundeNr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE FILL-IN-Navn AS CHARACTER FORMAT "x(200)" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.2 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.4 BY .1.

DEFINE BUTTON B-Sok 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokType 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Kort 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokeForsteKjop 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Sokeknapp 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Sokeknapp2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokKund 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE FI-KundeKort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundekort" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KundeTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Kundeinformasjon" 
      VIEW-AS TEXT 
     SIZE 38 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-PostSted1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FI-PostSted2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Kontakt" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Leveringsadresse" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-EndretInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Endret informasjon" 
      VIEW-AS TEXT 
     SIZE 77.8 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 149 BY 16.91.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 149 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-AltS AT ROW 1.33 COL 25.6 NO-TAB-STOP 
     B-Medlem AT ROW 1.33 COL 81
     B-OppdatSaldo AT ROW 1.33 COL 127
     CB-Sort AT ROW 1.38 COL 38 COLON-ALIGNED NO-LABEL
     B-GDPR AT ROW 1.38 COL 103.8
     FILL-IN-KundeNr AT ROW 2.86 COL 19.8 COLON-ALIGNED HELP
          "Kundenummer"
     FILL-IN-Navn AT ROW 2.86 COL 44.6 COLON-ALIGNED HELP
          "Kundens navn" NO-LABEL
     FI-Saldo AT ROW 2.86 COL 132 COLON-ALIGNED
     FI-Totalkjop AT ROW 3.86 COL 132 COLON-ALIGNED
     FI-AntMedlemmer AT ROW 4.86 COL 132 COLON-ALIGNED
     FI-ForsteKjop AT ROW 5.86 COL 132 COLON-ALIGNED
     FI-SisteKjop AT ROW 6.81 COL 132 COLON-ALIGNED
     BUTTON-SokMedlem AT ROW 4.86 COL 148
     BUTTON-SokKund-2 AT ROW 2.86 COL 42
     BUTTON-Angre AT ROW 1.33 COL 20.8 NO-TAB-STOP 
     BUTTON-Kopier AT ROW 1.33 COL 11.4 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 1.33 COL 6.8 NO-TAB-STOP 
     BUTTON-Next AT ROW 1.33 COL 34.8 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.33 COL 2 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 30.2 NO-TAB-STOP 
     BUTTON-Slett AT ROW 1.33 COL 16 NO-TAB-STOP 
     Btn_Help AT ROW 1.33 COL 150.4 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 155.4 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.8 BY 26.48.

DEFINE FRAME FRAME-2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-1
     B-Sok AT ROW 11.52 COL 27.6
     B-SokButikk AT ROW 12.52 COL 30.4
     B-SokType AT ROW 10.52 COL 27.6
     FI-KundeKort AT ROW 16.95 COL 96 COLON-ALIGNED
     Kunde.KundeNr AT ROW 1.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Kunde.Navn AT ROW 1.71 COL 42.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.Adresse1 AT ROW 3.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.Adresse2 AT ROW 4.52 COL 18 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.PostNr AT ROW 5.52 COL 18 COLON-ALIGNED
          LABEL "PostNr/Bydels"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Kunde.Land AT ROW 6.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.ePostAdresse AT ROW 7.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.Telefon AT ROW 8.52 COL 18 COLON-ALIGNED
          LABEL "Telefon/Mobil.tlf"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.MobilTlf AT ROW 8.52 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.Telefaks AT ROW 9.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     BUTTON-SokeForsteKjop AT ROW 14.95 COL 112
     Kunde.TypeId AT ROW 10.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     Kunde.GruppeId AT ROW 11.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     Kunde.ButikkNr AT ROW 12.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Kunde.BetType AT ROW 13.86 COL 20 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Kontant", 1,
"Kredit", 2
          SIZE 27 BY 1
     BUTTON-Sokeknapp2 AT ROW 12.52 COL 113.6
     Kunde.KreditSperret AT ROW 13.86 COL 47.2
          LABEL "Sperret"
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY 1
     Kunde.SamleFaktura AT ROW 13.86 COL 73.6
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY 1
     Kunde.MaksKredit AT ROW 14.95 COL 18 COLON-ALIGNED FORMAT "->>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Kunde.TotalRabatt% AT ROW 14.95 COL 71.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1 TOOLTIP "Totalrabatt kundeordre/bong."
     Kunde.Kjon AT ROW 16 COL 18 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Ukjent",0,
                     "Mann",1,
                     "Kvinne",2
          DROP-DOWN-LIST
          SIZE 20 BY 1
     BUTTON-SokKund AT ROW 1.71 COL 40.2
     Kunde.FodtDato AT ROW 16 COL 45 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.Alder AT ROW 16 COL 71.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     KundeType.Beskrivelse AT ROW 10.52 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     KundeGruppe.Beskrivelse AT ROW 11.52 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     Butiker.ButNamn AT ROW 12.52 COL 32.6 COLON-ALIGNED NO-LABEL FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 38.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     Kunde.BydelsNr AT ROW 5.52 COL 38 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     FI-PostSted1 AT ROW 5.52 COL 50.8 COLON-ALIGNED NO-LABEL
     Kunde.Opphort AT ROW 1.71 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Kunde.KontNavn AT ROW 3.52 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.Stilling AT ROW 4.52 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.KontE-Post AT ROW 5.52 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.KontTelefon AT ROW 6.52 COL 96 COLON-ALIGNED
          LABEL "Telefon/mobil.tlf"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.KontMobilTlf AT ROW 6.52 COL 113.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.KontTelefaks AT ROW 7.52 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.LevAdresse1 AT ROW 10.52 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.LevAdresse2 AT ROW 11.52 COL 96 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.LevPostNr AT ROW 12.52 COL 96 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     FI-PostSted2 AT ROW 12.52 COL 116 COLON-ALIGNED NO-LABEL
     Kunde.BankKonto AT ROW 14.91 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Kunde.Postgiro AT ROW 15.91 COL 126 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     BUTTON-Sokeknapp AT ROW 5.52 COL 35.6
     FILL-IN-EndretInfo AT ROW 17.29 COL 3.2 NO-LABEL
     FI-KundeTekst AT ROW 2.91 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 2.91 COL 96 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 9.81 COL 96 COLON-ALIGNED NO-LABEL
     BUTTON-Kort AT ROW 16.95 COL 122
     RECT-46 AT ROW 1.24 COL 2
     RECT-53 AT ROW 13.67 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tStLinje T "NEW SHARED" NO-UNDO skotex StLinje
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-KundKort ASSIGN
         HIDDEN             = YES
         TITLE              = "Kunde"
         HEIGHT             = 26.48
         WIDTH              = 159.8
         MAX-HEIGHT         = 33.71
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.71
         VIRTUAL-WIDTH      = 204.8
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-ArtKort:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-KundKort
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-1:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-2:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-3:FRAME = FRAME FRAME-2:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SokKund-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntMedlemmer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ForsteKjop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Saldo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SisteKjop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Totalkjop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KundeNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Navn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-1
   Custom                                                               */
/* SETTINGS FOR FILL-IN KundeType.Beskrivelse IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN KundeGruppe.Beskrivelse IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Butiker.ButNamn IN FRAME FRAME-1
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Kunde.BydelsNr IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KundeKort IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KundeTekst IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PostSted1 IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PostSted2 IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-EndretInfo IN FRAME FRAME-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Kunde.FodtDato IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Kunde.KontTelefon IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Kunde.KreditSperret IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Kunde.KundeNr IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Kunde.MaksKredit IN FRAME FRAME-1
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Kunde.PostNr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Kunde.Telefon IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Kunde.TotalRabatt% IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-2
                                                                        */
/* SETTINGS FOR FRAME FRAME-3
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KundKort)
THEN C-KundKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-1
/* Query rebuild information for FRAME FRAME-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-2
/* Query rebuild information for FRAME FRAME-2
     _Query            is NOT OPENED
*/  /* FRAME FRAME-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-3
/* Query rebuild information for FRAME FRAME-3
     _Query            is NOT OPENED
*/  /* FRAME FRAME-3 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 7.91
       COLUMN          = 1
       HEIGHT          = 19.52
       WIDTH           = 159
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(FI-SisteKjop:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-KundKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KundKort C-KundKort
ON END-ERROR OF C-KundKort /* Kunde */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KundKort C-KundKort
ON WINDOW-CLOSE OF C-KundKort /* Kunde */
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
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Kunde.Alder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.Alder C-KundKort
ON LEAVE OF Kunde.Alder IN FRAME FRAME-1 /* Alder */
DO:
    IF INPUT Kunde.FodtDato <> ? THEN
      Kunde.Alder:SCREEN-VALUE = STRING(YEAR(TODAY) - YEAR(INPUT Kunde.FodtDato)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-AltS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AltS C-KundKort
ON CHOOSE OF B-AltS IN FRAME DEFAULT-FRAME /* Søk */
DO:
  APPLY "ALT-S" to {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-GDPR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-GDPR C-KundKort
ON CHOOSE OF B-GDPR IN FRAME DEFAULT-FRAME /* GDPR */
DO:
    DEFINE VARIABLE cResultat AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dKundenr AS DECIMAL     NO-UNDO.
  IF NOT AVAILABLE Kunde THEN
      RETURN NO-APPLY.
  IF AVAIL Kunde THEN DO:
      dKundenr = INPUT FILL-IN-KundeNr.
      RUN w-GDPR.w (INPUT dKundenr, "KUNDE",OUTPUT cResultat).
      APPLY "CLOSE" TO THIS-PROCEDURE.
      RETURN NO-APPLY.
  END.
/*                                                                     */
/*   FIND Medlem WHERE Medlem.MedlemsNr = lMedlemsNr NO-LOCK NO-ERROR. */
/*   IF AVAILABLE Medlem THEN                                          */
/*     DO:                                                             */
/*       ASSIGN                                                        */
/*         wInputRecid = RECID(Medlem)                                 */
/*         wModus      = 'ENDRE'.                                      */
/*       RUN VisPost.                                                  */
/*     END.                                                            */
/*   RETURN NO-APPLY.                                                  */
/*                                                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Medlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Medlem C-KundKort
ON CHOOSE OF B-Medlem IN FRAME DEFAULT-FRAME /* Opprett medlem... */
DO:
  IF NOT AVAILABLE Kunde THEN
      RETURN NO-APPLY.

/*   RUN d-kortnummer.w.             */
/*   IF RETURN-VALUE = "AVBRYT" THEN */
/*     RETURN NO-APPLY.              */
/*   ELSE ASSIGN                     */
/*       wKortNummer = RETURN-VALUE. */
  wKortNummer = "".

  RUN opprettmedlem.p (Kunde.KundeNr, wKortNummer, wCl, OUTPUT wMedlemsNr).
  IF RETURN-VALUE = "AVBRYT" THEN
  DO:
      MESSAGE "Feil ved opprettelse av medlem. Rutine er avbrudt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Feil ved opprettelse av medlem".
      RETURN NO-APPLY.
  END.
  RUN AntallMedlemmer.
  FIND medlem NO-LOCK WHERE
      Medlem.MedlemsNr = wMedlemsNr.

  CREATE tmpChild.
  run w-vmedlem PERSISTENT SET tmpChild.wChild (recid(Medlem),"ENDRE").
  RELEASE tmpChild.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OppdatSaldo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OppdatSaldo C-KundKort
ON CHOOSE OF B-OppdatSaldo IN FRAME DEFAULT-FRAME /* Oppdater saldo */
DO:
  IF AVAILABLE Kunde THEN
    RUN beregnkundesaldo(Kunde.kundeNr, 0).
  RUN VisKundeSaldo.
  RUN VisSaldo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok C-KundKort
ON CHOOSE OF B-Sok IN FRAME FRAME-1 /* ... */
or "F10" of Kunde.GruppeId
DO:
  {soek.i
    &Felt        = Kunde.GruppeId
    &Program     = d-bkundegruppe.w
    &Frame       = FRAME-1
    &PostRun     = "find KundeGruppe no-lock where
                    recid(KundeGruppe) = int(return-value) no-error."
    &OptDisp     = "KundeGruppe.Beskrivelse when available KundeGruppe
                    ' ' when not available KundeGruppe @ KundeGruppe.Beskrivelse"
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikk C-KundKort
ON CHOOSE OF B-SokButikk IN FRAME FRAME-1 /* ... */
or "F10" of Kunde.ButikkNr
DO:
  {soek.i
    &Felt        = Kunde.ButikkNr
    &Program     = d-bbutiker.w
    &Frame       = FRAME-1
    &PostRun     = "find Butiker no-lock where
                    recid(Butiker) = int(return-value) no-error."
    &OptDisp     = "Butiker.ButNamn when available Butiker
                    ' ' when not available Butiker @ Butiker.butNamn"
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokType C-KundKort
ON CHOOSE OF B-SokType IN FRAME FRAME-1 /* ... */
or "F10" of Kunde.TypeId
DO:
  {soek.i
    &Felt        = Kunde.TypeId
    &Program     = d-bkundetype.w
    &Frame       = FRAME-1
    &PostRun     = "find KundeType no-lock where
                    recid(KundeType) = int(return-value) no-error."
    &OptDisp     = "KundeType.Beskrivelse when available KundeType
                    ' ' when not available KundeType @ KundeType.Beskrivelse"
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kunde.BetType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.BetType C-KundKort
ON RETURN OF Kunde.BetType IN FRAME FRAME-1 /* BT */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.BetType C-KundKort
ON VALUE-CHANGED OF Kunde.BetType IN FRAME FRAME-1 /* BT */
DO:
  ASSIGN Kunde.MaksKredit:SENSITIVE    = INPUT Kunde.BetType = 2.
  IF INPUT Kunde.BetType = 1 THEN
      ASSIGN Kunde.MaksKredit:SCREEN-VALUE = "0".
  ELSE
      APPLY "ENTRY" TO Kunde.MaksKredit.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-KundKort
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  run WinHlp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Kunde.ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.ButikkNr C-KundKort
ON TAB OF Kunde.ButikkNr IN FRAME FRAME-1 /* Butikk */
OR "RETURN" OF Kunde.ButikkNr
DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INPUT Kunde.butikkNr NO-ERROR.
    IF AVAILABLE Butiker THEN
        ASSIGN
          Kunde.ButikkNr:SCREEN-VALUE = STRING(Butiker.Butik)
          Butiker.ButNamn:SCREEN-VALUE = Butiker.ButNamn
        .
    ELSE 
    ASSIGN
      Kunde.ButikkNr:SCREEN-VALUE  = ""
      Butiker.ButNamn:SCREEN-VALUE = ""
    .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-KundKort
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  IF NOT AVAILABLE Kunde THEN
    APPLY "CLOSE" TO THIS-PROCEDURE.
  
  IF wModus = "NY" THEN
     ASSIGN Kunde.KundeNr:SENSITIVE IN FRAME FRAME-1 = NO.
            wModus = "ENDRE".

  RUN VisPost.
  RUN BUTTONEnaDis.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier C-KundKort
ON CHOOSE OF BUTTON-Kopier IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
  DO WITH FRAME FRAME-1:
    assign 
      wModus   = "NY"
      Kunde.KundeNr:SENSITIVE = FALSE
      Kunde.KundeNr:SCREEN-VALUE = KundeNummer(0.0)
      FI-KundeKort:SCREEN-VALUE = "".
    RUN BUTTONEnaDis.
    apply "ENTRY":U to Kunde.Navn.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME BUTTON-Kort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kort C-KundKort
ON CHOOSE OF BUTTON-Kort IN FRAME FRAME-1 /* ... */
or "F10" of FI-KundeKort
DO:
  IF AVAILABLE Kunde THEN
    RUN d-bkundekort.w (0,Kunde.KundeNr,"V").
  RUN VisKundeKort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-KundKort
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  run LagrePost (0).
  if return-value = "AVBRYT" then
    return no-apply.
  RUN VisEndretInfo.
  RUN BUTTONEnaDis.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-KundKort
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  /*RUN LagrePost (0).*/
  run Bytpost("Next").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-KundKort
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  run BUTTON-Ny.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-KundKort
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
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
  IF Modifierad() THEN DO: 
    run LagrePost (0).
    if return-value <> "OK" then
     do:
       readkey pause 0.
       return no-apply.
    end.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
  return wModus + "," + string(wInputRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-KundKort
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  /*RUN LagrePost (0).*/
  run Bytpost("Prev").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-KundKort
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  run BUTTON-Slett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME BUTTON-SokeForsteKjop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokeForsteKjop C-KundKort
ON CHOOSE OF BUTTON-SokeForsteKjop IN FRAME FRAME-1 /* ... */
DO:
  IF AVAILABLE Kunde THEN
    RUN d-bkundesaldo.w (0,Kunde.KundeNr,"V").
  RUN VisKundeSaldo.
  RUN VisSaldo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sokeknapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp C-KundKort
ON CHOOSE OF BUTTON-Sokeknapp IN FRAME FRAME-1 /* ... */
or "F10" of Kunde.PostNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    Kunde.PostNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        Kunde.PostNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        Kunde.BydelsNr:SCREEN-VALUE = SUBSTRING(TRIM(ENTRY(2,cTekst,CHR(1))),1,3)
        FI-PostSted1:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sokeknapp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp2 C-KundKort
ON CHOOSE OF BUTTON-Sokeknapp2 IN FRAME FRAME-1 /* ... */
or "F10" of Kunde.LevPostNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    Kunde.LevPostNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        Kunde.LevPostNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        FI-PostSted2:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokKund
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokKund C-KundKort
ON CHOOSE OF BUTTON-SokKund IN FRAME FRAME-1 /* ... */
or F10 of Kunde.KundeNr
DO:
  DO WITH FRAME FRAME-1:
     DEF VAR wKundeNr LIKE Kunde.KundeNr NO-UNDO.
     IF wModus = "ENDRE" AND Modifierad() THEN DO:
        MESSAGE "Vill du lagre posten?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO
        TITLE "" UPDATE wChoice AS LOGICAL.
        IF wChoice = TRUE THEN
           RUN LagrePost(0).
           IF RETURN-VALUE = "AVBRYT" THEN DO:
               RETURN NO-APPLY.
        END.
        RUN VisPost.
      END.
      ASSIGN wKundeNr = IF wModus = "ENDRE" THEN Kunde.KundeNr ELSE ?.
      RUN d-bkunde.w (INPUT wKundeNr," ").
      IF wModus = "NY" THEN DO:
         APPLY "ENTRY" TO Kunde.KundeNr IN FRAME FRAME-1.
         RETURN NO-APPLY.
      END.
      IF RETURN-VALUE <> "AVBRYT" THEN DO:
        FIND Kunde WHERE recid(Kunde) = INT(RETURN-VALUE) NO-LOCK.
        ASSIGN
            wKundeNr    = Kunde.KundeNr
            wInputRecid = RECID(Kunde).
        RUN VisPost.
      END.
      ELSE RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokKund-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokKund-2 C-KundKort
ON CHOOSE OF BUTTON-SokKund-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
    APPLY "CHOOSE" TO BUTTON-SokKund IN FRAME FRAME-1.
    IF RETURN-VALUE <> "AVBRYT" AND AVAILABLE Kunde THEN 
    DO:
        RUN LagrePost (0).
        RUN ByttFrame.
    END.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMedlem C-KundKort
ON CHOOSE OF BUTTON-SokMedlem IN FRAME DEFAULT-FRAME /* ... */
DO:
    IF AVAILABLE Kunde THEN
      RUN d-bmedlem2.w (0,"",Kunde.KundeNr).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Kunde.FodtDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.FodtDato C-KundKort
ON LEAVE OF Kunde.FodtDato IN FRAME FRAME-1 /* Født */
DO:

  IF INPUT Kunde.FodtDato <> ? THEN
      Kunde.Alder:SCREEN-VALUE = STRING(YEAR(TODAY) - YEAR(INPUT Kunde.FodtDato)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kunde.GruppeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.GruppeId C-KundKort
ON TAB OF Kunde.GruppeId IN FRAME FRAME-1 /* Kundegruppe */
OR "RETURN" OF Kunde.GruppeId
DO:
    FIND KundeGruppe NO-LOCK WHERE
        KundeGruppe.GruppeId = INPUT Kunde.GruppeId NO-ERROR.
    IF AVAILABLE KundeGruppe THEN
        ASSIGN
          Kunde.GruppeId:SCREEN-VALUE = STRING(KundeGruppe.GruppeId)
          KundeGruppe.Beskrivelse:SCREEN-VALUE = KundeGruppe.Beskrivelse
        .
    ELSE 
    ASSIGN
      Kunde.GruppeId:SCREEN-VALUE  = ""
      KundeGruppe.Beskrivelse:SCREEN-VALUE = ""
    .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kunde.KreditSperret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.KreditSperret C-KundKort
ON RETURN OF Kunde.KreditSperret IN FRAME FRAME-1 /* Sperret */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kunde.LevPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.LevPostNr C-KundKort
ON TAB OF Kunde.LevPostNr IN FRAME FRAME-1 /* PostNr */
OR "RETURN" OF Kunde.LevPostNr
DO:
    FIND Post NO-LOCK WHERE
        Post.PostNr = INPUT Kunde.LevPostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN
        ASSIGN
          FI-PostSted2:SCREEN-VALUE   = "".
    ELSE
        ASSIGN
          FI-PostSted2:SCREEN-VALUE   = Post.Beskrivelse.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kunde.Opphort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.Opphort C-KundKort
ON DELETE-CHARACTER OF Kunde.Opphort IN FRAME FRAME-1 /* Opphørt */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kunde.PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.PostNr C-KundKort
ON TAB OF Kunde.PostNr IN FRAME FRAME-1 /* PostNr/Bydels */
OR "RETURN" OF Kunde.PostNr
DO:
    FIND Post NO-LOCK WHERE
        Post.PostNr = INPUT Kunde.PostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN
        ASSIGN
          FI-PostSted1:SCREEN-VALUE   = ""
          Kunde.BydelsNr:SCREEN-VALUE = "".
    ELSE
        ASSIGN
          FI-PostSted1:SCREEN-VALUE   = Post.Beskrivelse
          Kunde.BydelsNr:SCREEN-VALUE = substring(INPUT Kunde.PostNr,1,3).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-KundKort OCX.Click
PROCEDURE TabStrip.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN DO:
  IF wModus = "NY" THEN DO:
      MESSAGE "Posten må lagres først." VIEW-AS ALERT-BOX INFORMATION TITLE "Feil".
              ASSIGN wFeil = TRUE
                     chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER)
                     chTabStrip:SelectedItem = chTab1Side.
  END.
  ELSE IF wAktivFlip = 1 AND Modifierad() THEN DO:
      MESSAGE "Vill du lagre posten?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO
      TITLE "" UPDATE wChoice AS LOGICAL.
      IF wChoice = TRUE THEN
          RUN LagrePost(0).
          IF RETURN-VALUE = "AVBRYT" THEN DO:
              ASSIGN wFeil = TRUE
                     chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER)
                     chTabStrip:SelectedItem = chTab1Side.
              RETURN NO-APPLY.
      END.
      ELSE DO:
          IF wModus = "NY" THEN
              ASSIGN wModus = "ENDRE".
      END.
      RUN VisPost.
  END.
  IF NOT wFeil THEN DO:
    ASSIGN wAktivFlip = INT(chTabStrip:SelectedItem:Index).
    RUN ByttFrame. /* Bytter tab */
    RUN ButtonEnaDis.
  END.
  IF wAktivFlip = 1 AND NOT wFeil THEN
      APPLY "ENTRY" TO Kunde.Navn IN FRAME FRAME-1.
  ELSE IF wAktivFlip <> 1 THEN
      APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
 END.
 ELSE DO:
    IF NOT wFeil THEN DO:
     IF wAktivFlip = 1 THEN
            APPLY "ENTRY" TO Kunde.Navn IN FRAME FRAME-1.
     ELSE
       APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
    END.
    ELSE
        wFeil = FALSE.
 END.
  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Kunde.TypeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kunde.TypeId C-KundKort
ON TAB OF Kunde.TypeId IN FRAME FRAME-1 /* Kundetype */
OR "RETURN" OF Kunde.TypeId
DO:
    FIND KundeType NO-LOCK WHERE
        KundeType.TypeId = INPUT Kunde.TypeId NO-ERROR.
    IF AVAILABLE KundeType THEN
        ASSIGN
          Kunde.typeId:SCREEN-VALUE = STRING(KundeType.TypeId)
          KundeType.Beskrivelse:SCREEN-VALUE = KundeType.Beskrivelse
        .
    ELSE 
    ASSIGN
      Kunde.TypeId:SCREEN-VALUE  = ""
      KundeType.Beskrivelse:SCREEN-VALUE = ""
    .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-KundKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Kundekort"
  &PostIClose    = "if valid-handle(wSubWin1) then
                         delete procedure wSubWin1 no-error.
                    if valid-handle(wSubWin2) then
                         delete procedure wSubWin2 no-error.
                    if valid-handle(wSubWin3) then
                         delete procedure wSubWin3 no-error.
                    if valid-handle(wSubWin4) then
                    DO:
                       RUN SlettTmpChild IN wSubWin4.
                       delete procedure wSubWin4 no-error.
                    END.
                    if valid-handle(wSubWin5) then
                         delete procedure wSubWin5 no-error.
                    RUN DelTmpChild.
                    IF VALID-HANDLE(chTabStrip) THEN
                        RELEASE OBJECT chTabStrip NO-ERROR.
                    IF VALID-HANDLE(chTabs) THEN
                        RELEASE OBJECT chTabs NO-ERROR.
                    IF VALID-HANDLE(chTab) THEN
                        RELEASE OBJECT chTab NO-ERROR.
                    IF VALID-HANDLE(chTab1Side) THEN
                        RELEASE OBJECT chTab1Side NO-ERROR.
                    IF VALID-HANDLE(TabStrip) THEN
                        DELETE OBJECT TabStrip NO-ERROR.
                    ASSIGN TabStrip    = ?
                           chTabStrip  = ?
                           chTabs      = ?
                           chTab       = ?
                           chTab1Side  = ?.
                    "                     
  &PostDisable_ui = "wModus = 'AVBRYT'." 
}

{syspara.i 1 1 17 wDbId}       /* DatabaseID */
{syspara.i 14 2 4 wLedige INT} /* Føste eller neste ledige kundenummer. */
{syspara.i 5  1 1 wCl INT}     /* Sentrallager. */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
on ALT-N of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Ny in frame DEFAULT-FRAME.
  end.
on ALT-L of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Lagre in frame DEFAULT-FRAME.
  end.
on ALT-K of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Kopier in frame DEFAULT-FRAME.
  end.
on ALT-D of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Slett in frame DEFAULT-FRAME.
  end.
on ALT-A of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-angre in frame DEFAULT-FRAME.
  end.
/*
on ALT-K of FRAME FRAME-1 anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Kort in frame FRAME-1.
  end.
*/
on ALT-CURSOR-UP of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Prev in frame DEFAULT-FRAME.
  end.
on ALT-CURSOR-DOWN of {&WINDOW-NAME} anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Next in frame DEFAULT-FRAME.
  end.
on ALT-S of {&WINDOW-NAME} anywhere 
  do: /* SOKTRIGGER */
    RUN d-kundesok.w.
    IF RETURN-VALUE = "AVBRYT" THEN 
        RETURN NO-APPLY.
    FIND Kunde NO-LOCK WHERE
        Kunde.KundeNr = DEC(RETURN-VALUE) NO-ERROR.
    if available Kunde then
    do:
      assign
        wInputRecid = recid(Kunde).
      run VisPost.
      RUN ByttFrame.
    end.
  end. /* SOKTRIGGER */

on "CTRL-TAB":U anywhere
  do:
    chTab1Side = chTabs:Item ((IF wAktivFlip = NUM-ENTRIES(wTabTekst)
                      THEN 1 ELSE wAktivFlip + 1) BY-VARIANT-POINTER).
    chTabStrip:SelectedItem = chTab1Side.
    RETURN NO-APPLY.
  end.

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
if valid-handle(wLibHandle) then
  run SjekkLapTop in wLibHandle (output wLapTop).

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF wInputRecid = ? AND wModus = "ENDRE" THEN DO:
      MESSAGE "Recid = ? och wModus = 'ENDRE' --> FEL !!" VIEW-AS ALERT-BOX ERROR.
      RETURN "AVBRYT".
  END.
  RUN enable_UI.
  {lng.i} /* Oversettelse */

  RUN InitCB.
  ASSIGN {&WINDOW-NAME}:HIDDEN = NO
         CB-Sort:SCREEN-VALUE = ENTRY(2,CB-Sort:LIST-ITEMS)
         wTabHnd[1] = FRAME FRAME-1:HANDLE
         wTabHnd[2] = FRAME FRAME-2:HANDLE
         wTabHnd[3] = FRAME FRAME-3:HANDLE
         .

  run ByttFrame. /* Legger opp f›rste fane. */

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  assign chTab1Side = chTabStrip. /* COM-Handl 1. side */
  

  if wModus <> "NY" then DO:
      find Kunde where recid(Kunde) = wInputRecid no-lock no-error.
      run VisPost.
  END.
  run ByttFrame. /* Legger opp f›rste fane. */ 

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  assign chTab1Side = chTabStrip. /* COM-Handl 1. side */
  
  if wModus = "NY" then 
    do:
      apply "CHOOSE":U to BUTTON-Ny.
    end.
  else if wModus = "SLETT" THEN DO:
     APPLY "CHOOSE" TO BUTTON-Slett.
     RETURN RETURN-VALUE.
  END.
  /*  */

  APPLY "entry" TO Kunde.Navn IN FRAME FRAME-1.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  if wModus <> "NY" then
    find Kunde no-lock where 
      recid(Kunde) = wInputRecid no-error.
      
  /* Retur verdi */  
  if available Kunde and wModus <> "NY" then
    return string(recid(Kunde)).
  else 
    return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AntallMedlemmer C-KundKort 
PROCEDURE AntallMedlemmer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wLoop AS INT NO-UNDO.

  IF NOT AVAILABLE Kunde THEN
    RETURN.
  DO WITH FRAME Default-Frame:
      ASSIGN
          FI-AntMEdlemmer = 0
          .
      FOR EACH Medlem NO-LOCK WHERE
        Medlem.KundeNr = Kunde.KundeNr:
        ASSIGN
            wLoop = wLoop + 1
            FI-AntMedlemmer = wLoop
            .
      END.
      DISPLAY
          FI-AntMedlemmer
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Ny C-KundKort 
PROCEDURE BUTTON-Ny :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wSort as int no-undo.
  DO WITH FRAME FRAME-1:
     IF Modifierad() THEN DO:
         RUN LagrePost(0).
         IF RETURN-VALUE = "AVBRYT" THEN
             RETURN NO-APPLY.
     END.
     FIND FIRST KundeType NO-LOCK NO-ERROR.
     FIND FIRST KundeGruppe NO-LOCK NO-ERROR.
     CLEAR FRAME FRAME-1.
     ASSIGN wModus = "NY"
            FILL-IN-KundeNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "" 
            FILL-IN-Navn:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
            Kunde.KundeNr:SENSITIVE = FALSE /*TRUE*/
            /* Kunde.KundeNr:SCREEN-VALUE = KundeNummer(0.0) */
            Kunde.TypeId:SCREEN-VALUE   = IF AVAIL KundeType THEN STRING(KundeType.TypeId) ELSE "0"
            Kunde.GruppeId:SCREEN-VALUE = IF AVAIL KundeGruppe THEN STRING(KundeGruppe.GruppeId) ELSE "0"
            FI-AntMedlemmer:SCREEN-VALUE IN FRAME default-frame = "" 
            KundeGruppe.Beskrivelse:SCREEN-VALUE = IF AVAIL KundeGruppe THEN KundeGruppe.Beskrivelse ELSE ""
            KundeType.Beskrivelse:SCREEN-VALUE   = IF AVAIL KundeType THEN KundeType.Beskrivelse ELSE ""
            Kunde.ButikkNr:SCREEN-VALUE = STRING(wCL).
    APPLY "TAB" TO Kunde.ButikkNr.
    RUN BUTTONEnaDis.
    APPLY "ENTRY" TO Kunde.KundeNr.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Slett C-KundKort 
PROCEDURE BUTTON-Slett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk as log format "Ja/Nei" no-undo.
  def var wNesteRecid as recid no-undo.

  IF CAN-FIND(FIRST Medlem WHERE
              Medlem.KundeNr = Kunde.KundeNr) THEN
  DO:
      MESSAGE "Kunden har medlemmer koblet til seg. Disse må kobles fra" SKIP
              "før kunden kan slettes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Validering av sletting".
      RETURN NO-APPLY "AVBRYT".
  END.

  find bKunde no-lock where
    recid(bKunde) = recid(Kunde) no-error.
  find next bKunde no-lock no-error.
  if not available bKunde then
    find first bKunde.
  if available bKunde then
    assign
      wNesteRecid = recid(bKunde).

  assign wOk = false.
  message "Skal kunden slettes?" SKIP(1)
      "Kundesaldo, transaksjoner, kort og statistikker vil også bli slettet."
      view-as alert-box QUESTION BUTTONS YES-NO
    title "Bekreftelse"
    update wOk.
  if wOk = false then
    return no-apply "AVBRYT".  
  else do:
    /* Statistikk */
    FOR EACH stLinje EXCLUSIVE where
             StLinje.StType = "KUNDSTAT" and
             StLinje.DataObjekt = string(Kunde.KundeNr,"9999999999999"):
       DELETE stLinje.
    END.
    /* Kundekort */
    FOR EACH KundeKort OF Kunde EXCLUSIVE-LOCK:
        DELETE KundeKort.
    END.
    /* Kundesaldo */
    FOR EACH KundeSaldo EXCLUSIVE-LOCK WHERE
        KundeSaldo.KundeNr = Kunde.KundeNr:
        DELETE KundeSaldo.
    END.
    /* Kundetransaksjoner */
    FOR EACH KundeTrans OF Kunde EXCLUSIVE-LOCK:
        DELETE KundeTrans.
    END.

    FIND CURRENT Kunde EXCLUSIVE.
    DELETE Kunde.
    IF wModus = "SLETT" THEN
        RETURN "OK".
    assign 
          wInputRecid   = wNesteRecid
          wModus        = "ENDRE".
    FIND Kunde NO-LOCK WHERE RECID(Kunde) = wInputRecid.
    RUN VisPost.
    RUN BUTTONEnaDis.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-KundKort 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame DEFAULT-FRAME:  
  assign
    BUTTON-Kort:SENSITIVE IN FRAME frame-1 = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-Ny:sensitive        = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-Kopier:sensitive    = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-Slett:sensitive     = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-Prev:sensitive      = NOT wModus = "Ny"
    BUTTON-Next:sensitive      = NOT wModus = "Ny"
    BUTTON-Ok:sensitive        = NOT wModus = "Ny"
    BUTTON-Lagre:sensitive     = wAktivFlip = 1
    BUTTON-Angre:sensitive     = wAktivFlip = 1 AND wInputRecid <> ?
    BUTTON-Angre:sensitive     = IF wModus = "Ny"
                                   THEN true
                                   ELSE BUTTON-Angre:SENSITIVE
    BUTTON-SokKund-2:sensitive = wAktivFlip <> 1
    B-AltS:sensitive           = NOT wModus = "Ny"
    .
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bytpost C-KundKort 
PROCEDURE Bytpost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wRettning AS CHAR NO-UNDO.
  def var wSort as int no-undo.
  assign
    wSort = int(entry(1,CB-Sort:screen-value in frame DEFAULT-FRAME,":")).
  IF Modifierad() THEN
    run LagrePost (0).

  if return-value = "AVBRYT" then
    return no-apply.
  else do:
    assign wModus = "ENDRE".
    
    case wSort:
      when 1 then
        do:
          IF wRettning = "Next" THEN DO:
              find Next Kunde no-lock use-index Kunde no-error.    
              if not available Kunde then
                 find last Kunde no-lock use-index Kunde  no-error.
          END.
          ELSE DO:
              find Prev Kunde no-lock use-index Kunde no-error.    
              if not available Kunde then
                 find First Kunde no-lock use-index Kunde  no-error.
          END.
        end.
      when 2 then
        do:
          IF wRettning = "Next" THEN DO:
              find Next Kunde no-lock use-index Navn no-error.    
              if not available Kunde then
                 find last Kunde no-lock use-index Navn  no-error.
          END.
          ELSE DO:
              find Prev Kunde no-lock use-index Navn no-error.    
              if not available Kunde then
                 find First Kunde no-lock use-index Navn  no-error.
          END.
        end.
        when 3 then
          do:
            IF wRettning = "Next" THEN DO:
                find Next Kunde no-lock use-index Kontakt no-error.    
                if not available Kunde then
                   find last Kunde no-lock use-index Kontakt  no-error.
            END.
            ELSE DO:
                find Prev Kunde no-lock use-index Kontakt no-error.    
                if not available Kunde then
                   find First Kunde no-lock use-index Kontakt  no-error.
            END.
          end.
    end case.  
    if available Kunde then
      do:
        assign
          wInputRecid = recid(Kunde)
          .
        run VisPost.
        RUN ByttFrame.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrame C-KundKort 
PROCEDURE ByttFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) or
   INT(chTabStrip:SelectedItem:Index) = 1 THEN
DO:
  FRAME DEFAULT-FRAME:MOVE-TO-TOP().
  wTabHnd[wAktivFlip]:MOVE-TO-TOP().
END.

IF wAktivFlip = 2 THEN 
DO:
    IF VALID-HANDLE(wSubWin2) THEN
    DO:
      run ByttObjekt in wSubWin2 (wInputRecid).  
      RUN MoveToTopp IN wSubWin2.
    END.
    ELSE
      run w-bkundetrans.w persistent set wSubWin2 (wInputRecid,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
END.

IF wAktivFlip = 3 THEN 
DO:
    IF VALID-HANDLE(wSubWin3) THEN
    DO:
      run ByttObjekt in wSubWin3 (wInputRecid).  
      RUN MoveToTopp IN wSubWin3.
    END.
    ELSE
      run w-bkundebettrans.w persistent set wSubWin3 (wInputRecid,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
END.

IF wAktivFlip = 4 THEN 
DO:
    IF VALID-HANDLE(wSubWin4) THEN
    DO:
      run ByttObjekt in wSubWin4 (wInputRecid).  
      RUN MoveToTopp IN wSubWin4.
    END.
    ELSE
      run w-bkundemedlem.w persistent set wSubWin4 (wInputRecid,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).  
END.

RUN VisSaldo.
RUN VisKundeSaldo.
RUN VisKundeKort.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-KundKort  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-vkunde.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
    TabStrip:NAME = "TabStrip":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-vkunde.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-KundKort 
PROCEDURE DelTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    for each tmpChild:

        if valid-handle(tmpChild.wChild) then do:
            RUN DelTmpChild IN tmpChild.wChild NO-ERROR.
            delete procedure tmpChild.wChild.
        end.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-KundKort  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KundKort)
  THEN DELETE WIDGET C-KundKort.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-KundKort  _DEFAULT-ENABLE
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
  DISPLAY CB-Sort FILL-IN-KundeNr FILL-IN-Navn FI-Saldo FI-Totalkjop 
          FI-AntMedlemmer FI-ForsteKjop FI-SisteKjop 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  ENABLE B-AltS RECT-27 RECT-28 B-Medlem B-OppdatSaldo CB-Sort B-GDPR 
         BUTTON-SokMedlem BUTTON-Angre BUTTON-Kopier BUTTON-Lagre BUTTON-Next 
         BUTTON-Ny BUTTON-Prev BUTTON-Slett Btn_Help BUTTON-Ok 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-KundeKort FI-PostSted1 FI-PostSted2 FILL-IN-EndretInfo 
          FI-KundeTekst FILL-IN-2 FILL-IN-3 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE Butiker THEN 
    DISPLAY Butiker.ButNamn 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE Kunde THEN 
    DISPLAY Kunde.KundeNr Kunde.Navn Kunde.Adresse1 Kunde.Adresse2 Kunde.PostNr 
          Kunde.Land Kunde.ePostAdresse Kunde.Telefon Kunde.MobilTlf 
          Kunde.Telefaks Kunde.TypeId Kunde.GruppeId Kunde.ButikkNr 
          Kunde.BetType Kunde.KreditSperret Kunde.SamleFaktura Kunde.MaksKredit 
          Kunde.TotalRabatt% Kunde.Kjon Kunde.FodtDato Kunde.Alder 
          Kunde.BydelsNr Kunde.Opphort Kunde.KontNavn Kunde.Stilling 
          Kunde.KontE-Post Kunde.KontTelefon Kunde.KontMobilTlf 
          Kunde.KontTelefaks Kunde.LevAdresse1 Kunde.LevAdresse2 Kunde.LevPostNr 
          Kunde.BankKonto Kunde.Postgiro 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE KundeGruppe THEN 
    DISPLAY KundeGruppe.Beskrivelse 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  IF AVAILABLE KundeType THEN 
    DISPLAY KundeType.Beskrivelse 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  ENABLE B-Sok B-SokButikk B-SokType Kunde.Navn Kunde.Adresse1 Kunde.Adresse2 
         Kunde.PostNr Kunde.Land Kunde.ePostAdresse Kunde.Telefon 
         Kunde.MobilTlf Kunde.Telefaks BUTTON-SokeForsteKjop Kunde.TypeId 
         Kunde.GruppeId Kunde.ButikkNr Kunde.BetType BUTTON-Sokeknapp2 
         Kunde.KreditSperret Kunde.SamleFaktura Kunde.Kjon BUTTON-SokKund 
         Kunde.FodtDato Kunde.Alder Kunde.Opphort Kunde.KontNavn Kunde.Stilling 
         Kunde.KontE-Post Kunde.KontTelefon Kunde.KontMobilTlf 
         Kunde.KontTelefaks Kunde.LevAdresse1 Kunde.LevAdresse2 Kunde.LevPostNr 
         Kunde.BankKonto Kunde.Postgiro BUTTON-Sokeknapp FILL-IN-EndretInfo 
         FILL-IN-2 FILL-IN-3 BUTTON-Kort RECT-46 RECT-53 
      WITH FRAME FRAME-1 IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-1}
  VIEW FRAME FRAME-2 IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-2}
  VIEW FRAME FRAME-3 IN WINDOW C-KundKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-KundKort 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
    
  assign
      pcTekst = Tx(" 1: Nummer, 2: Navn, 3: Kontaktperson",1)
      .

  DO WITH FRAME Default-Frame:
      ASSIGN
          CB-Sort:LIST-ITEMS = pcTekst
          CB-Sort:SCREEN-VALUE = ENTRY(2,pcTekst)
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-Controls C-KundKort 
PROCEDURE initialize-Controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wCount AS INTE NO-UNDO.
  ASSIGN chTabStrip = chTabStrip:TabStrip
         chTabs     = chTabStrip:Tabs
         chTabs:Item(1):Caption = ENTRY(1,wTabTekst).
  DO wCount = 2 TO NUM-ENTRIES(wTabTekst):
      chTabs:Add(wCount,,ENTRY(wCount,wTabTekst)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-KundKort 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wBekreft as int no-undo.
  def var ipStatus as char INIT "AVBRYT" no-undo.

  DO WITH FRAME FRAME-1:
    IF wModus = "NY" THEN DO:
      /*
      /* Sjekker input */
      if input Kunde.KundeNr = 0 then
        do:
          message "Kundenummer må være større enn 0"
          view-as alert-box title "Lagringsfeil".
          apply "ENTRY":U to Kunde.KundeNr.
          return ipStatus.
        end.
      ELSE 
      if can-find(Kunde where
        Kunde.KundeNr = input Kunde.KundeNr) then
        do:
          message "Det finnes allerede en kunde med dette kundenummer!"
            view-as alert-box message title "Melding".
          apply "ENTRY":U to Kunde.KundeNr.
          return ipStatus.
        end.
      */
    END.
    if not can-find(KundeType where KundeType.TypeId = input Kunde.TypeId) then
    do:
      message "Ukjent kundetype!"
        view-as alert-box message title "Melding".
          apply "ENTRY":U to Kunde.TypeId.
          return ipStatus.
    end.
    if not can-find(KundeGruppe where KundeGruppe.GruppeId = input Kunde.GruppeId) then
    do:
      message "Ukjent kundegruppe!"
        view-as alert-box message title "Melding".
          apply "ENTRY":U to Kunde.GruppeId.
          return ipStatus.
    end.
    if not can-find(Post where Post.PostNr = input Kunde.PostNr) then
    do:
      message "Ukjent postnummer på kunden!"
        view-as alert-box message title "Melding".
          apply "ENTRY":U to Kunde.PostNr.
          return ipStatus.
    end.  
    
  if not can-find(Post where Post.PostNr = input Kunde.LevPostNr) then
    do:
      message "Ukjent postnummer på leveringsadressen!"
        view-as alert-box message title "Melding".
          apply "ENTRY":U to Kunde.LevPostNr.
          return ipStatus.
    end.  
    if not can-find(Butiker where Butiker.Butik = input Kunde.ButikkNr) then
      do:
        message "Ukjent butikknummer på kunden!"
          view-as alert-box message title "Melding".
            apply "ENTRY":U to Kunde.ButikkNr.
            return ipStatus.
      end.  
    ELSE
      FIND Butiker NO-LOCK where
           Butiker.butik = INPUT Kunde.ButikkNr NO-ERROR.
    
    LAGRE_KUND:
      do TRANSACTION:
        if wModus = "NY" then
          do:
            release Kunde. /* Slipper gammel post. */
            CREATE Kunde.
            assign
               wInputRecid = recid(Kunde)
               Kunde.KundeNr.
          end.
        else do:
          FIND CURRENT Kunde EXCLUSIVE-LOCK no-error no-wait.
          if NOT AVAILABLE Kunde then
            do:
                message "Kunden oppdateres fra en annen terminal" skip
                  "Forsök å lagre en gang til" view-as alert-box 
               WARNING title "Lagringsfeil".
           return no-apply ipStatus.
          end.
        end.
        assign
            wModus       = "ENDRE"
            Kunde.KundeNr:SENSITIVE = FALSE
            Kunde.KundeNr
            Kunde.Navn
            Kunde.Adresse1
            kunde.Adresse2
            Kunde.BydelsNr 
            Kunde.PostNr
            Kunde.Land
            Kunde.ePostAdresse
            Kunde.Telefon
            Kunde.Mobil
            Kunde.Telefaks
            Kunde.TypeId
            Kunde.GruppeId 
            Kunde.MaksKredit 
            Kunde.Opphort 
            Kunde.KontNavn 
            Kunde.Stilling
            Kunde.ButikkNr 
            Kunde.KontE-Post 
            Kunde.KontMobilTlf 
            Kunde.KontTelefaks 
            Kunde.KontTelefon 
            Kunde.KreditSperret 
            Kunde.LevAdresse1 
            Kunde.LevAdresse2 
/*             Kunde.LevLand */
            Kunde.LevPostNr 
            Kunde.BetType
            Kunde.TotalRabatt%
            Kunde.Samlefaktura
            Kunde.Kjon
            Kunde.Alder
            Kunde.FodtDato
            .
         FIND CURRENT Kunde NO-LOCK.
         RUN VisPost.
         RUN BUTTONEnaDis.
      end.
  END.
  return "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModified C-KundKort 
PROCEDURE SettModified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wModified AS LOGI NO-UNDO.
  DO WITH FRAME FRAME-1:
    ASSIGN 
      Kunde.KundeNr:MODIFIED = wModified
      Kunde.Navn:MODIFIED = wModified
      Kunde.Adresse1:MODIFIED = wModified
      kunde.Adresse2:MODIFIED = wModified
      Kunde.BydelsNr:MODIFIED = wModified 
      Kunde.PostNr:MODIFIED = wModified
      Kunde.Land:MODIFIED = wModified
      Kunde.ePostAdresse:MODIFIED = wModified
      Kunde.Telefon:MODIFIED = wModified
      Kunde.Mobil:MODIFIED = wModified
      Kunde.Telefaks:MODIFIED = wModified
      Kunde.TypeId:MODIFIED = wModified
      Kunde.GruppeId:MODIFIED = wModified 
      Kunde.MaksKredit:MODIFIED = wModified 
      Kunde.Opphort:MODIFIED = wModified 
      Kunde.KontNavn:MODIFIED = wModified 
      Kunde.Stilling:MODIFIED = wModified
      Kunde.ButikkNr:MODIFIED = wModified 
      Kunde.KontE-Post:MODIFIED = wModified 
      Kunde.KontMobilTlf:MODIFIED = wModified 
      Kunde.KontTelefaks:MODIFIED = wModified 
      Kunde.KontTelefon:MODIFIED = wModified 
      Kunde.KreditSperret:MODIFIED = wModified 
      Kunde.LevAdresse1:MODIFIED = wModified 
      Kunde.LevAdresse2:MODIFIED = wModified 
/*       Kunde.LevLand:MODIFIED = wModified */
      Kunde.BetType:MODIFIED = wModified 
      Kunde.LevPostNr:MODIFIED = wModified 
      Kunde.Samlefaktura:MODIFIED = wModified
      Kunde.Kjon:MODIFIED = wModified
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpChild C-KundKort 
PROCEDURE SkapaTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipChild AS HANDLE NO-UNDO.
    IF VALID-HANDLE(ipChild) THEN DO:
        CREATE tmpChild.
        ASSIGN tmpChild.wChild = ipChild.
        RELEASE tmpChild.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-KundKort 
PROCEDURE SlettProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  apply "CLOSE":U to this-procedure. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisEndretInfo C-KundKort 
PROCEDURE VisEndretInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if available Kunde then
    do:
      assign
        Fill-In-EndretInfo = "Opprettet " + 
                             (if Kunde.RegistrertDato <> ? 
                               then string(Kunde.RegistrertDato)
                               else "        ") + " " +
                             (if Kunde.RegistrertTid <> 0
                               then string(Kunde.RegistrertTid,"HH:MM:SS")
                               else "        ") + " av " + 
                             Kunde.RegistrertAv + "    Endret " +
                             (if Kunde.EDato <> ?
                               then string(Kunde.EDato)
                               else "        ") + " " +
                             (if Kunde.ETid <> 0
                               then string(Kunde.ETid,"HH:MM:SS")
                               else "        ") + " av " +
                             Kunde.BrukerId.
    end.
  else do:
    assign
      FILL-IN-EndretInfo = "".
  end.
  display 
    FILL-IN-EndretInfo
  with frame FRAME-1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisKundeKort C-KundKort 
PROCEDURE VisKundeKort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME FRAME-1:
  IF NOT AVAILABLE Kunde THEN
      RETURN.
  FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
  IF NOT AVAILABLE KundeKort THEN
      ASSIGN
        FI-KundeKort:SCREEN-VALUE = "".
  ELSE
      ASSIGN
        FI-KundeKort:SCREEN-VALUE = KundeKort.KortNr.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisKundeSaldo C-KundKort 
PROCEDURE VisKundeSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME Default-Frame:
  
  IF NOT AVAILABLE Kunde THEN
      ASSIGN
        FI-ForsteKjop:SCREEN-VALUE = ""
        FI-SisteKjop:SCREEN-VALUE = ""
        FI-Saldo:SCREEN-VALUE = ""
        FI-Totalkjop:SCREEN-VALUE = ""
        .
  ELSE DO:
    FIND first KundeSaldo NO-LOCK WHERE
        KundeSaldo.KundeNr = Kunde.KundeNr USE-INDEX Forste NO-ERROR.
    IF AVAILABLE KundeSaldo THEN
        ASSIGN
          FI-ForsteKjop:SCREEN-VALUE = string(KundeSaldo.ForsteDato).
          .
    FIND LAST KundeSaldo NO-LOCK WHERE
        KundeSaldo.KundeNr = Kunde.KundeNr USE-INDEX Siste NO-ERROR.
    IF AVAILABLE KundeSaldo THEN
        ASSIGN
          FI-SisteKjop:SCREEN-VALUE = string(KundeSaldo.DatoSiste).
          .
    wSaldo = 0.
    wTotal = 0.
    FOR EACH KundeSaldo NO-LOCK WHERE
        KundeSaldo.KundeNr = Kunde.KundeNr:
        ASSIGN
            wSaldo = wSaldo + KundeSaldo.Saldo
            wTotal = wTotal + KundeSaldo.Totaltkjop
            .
    END.
    ASSIGN
      FI-Saldo:SCREEN-VALUE = string(wSaldo).
      FI-Totalkjop:SCREEN-VALUE = STRING(wTotal)
      .
  END.
END. /* FrameScoop */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-KundKort 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE Kunde THEN
      RETURN.
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN FILL-IN-KundeNr:SCREEN-VALUE = STRING(Kunde.KundeNr)
           FILL-IN-Navn:SCREEN-VALUE    = Kunde.Navn
           FI-AntMedlemmer              = 0.
  END.
  find Post where Post.PostNr = Kunde.PostNr no-lock no-error.
  ASSIGN FI-PostSted1 = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  find Post where Post.PostNr = Kunde.LevPostNr no-lock no-error.
  ASSIGN FI-PostSted2 = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  FIND KundeType   OF Kunde NO-LOCK NO-ERROR.
  FIND KundeGruppe OF Kunde NO-LOCK NO-ERROR.
  FIND Butiker     NO-LOCK WHERE
      Butiker.Butik = Kunde.ButikkNr NO-ERROR.
  DO WITH FRAME default-frame:
      DISPLAY
          FI-AntMedlemmer
          FI-SisteKjop
          FI-ForsteKjop
          FI-Totalkjop
          FI-Saldo
          .
  END.
  DO WITH FRAME FRAME-1:
      DISPLAY 
          Kunde.KundeNr
          Kunde.Navn
          Kunde.Adresse1
          kunde.Adresse2
          Kunde.BydelsNr 
          Kunde.PostNr
          FI-PostSted1
          Kunde.TotalRabatt%
          Kunde.Land
          Kunde.ePostAdresse
          Kunde.Telefon
          Kunde.Mobil
          Kunde.Telefaks
          Kunde.TypeId
          (IF AVAIL KundeType THEN KundeType.Beskrivelse ELSE "") @ KundeType.Beskrivelse 
          Kunde.GruppeId 
          (IF AVAIL KundeGruppe THEN KundeGruppe.Beskrivelse ELSE "") @ KundeGruppe.Beskrivelse 
              FI-PostSted2
          Butiker.ButNamn WHEN AVAILABLE Butiker 
          Kunde.MaksKredit 
          Kunde.Opphort 
          Kunde.KontNavn 
          Kunde.Stilling
          Kunde.ButikkNr 
          Kunde.KontE-Post 
          Kunde.KontMobilTlf 
          Kunde.KontTelefaks 
          Kunde.KontTelefon 
          Kunde.KreditSperret 
          Kunde.LevAdresse1 
          Kunde.LevAdresse2 
/*           Kunde.LevLand */
          Kunde.LevPostNr
          Kunde.BetType
          Kunde.Samlefaktura
          Kunde.Kjon
          Kunde.FodtDato
          Kunde.Alder
          .
  END.
  ASSIGN Kunde.MaksKredit:SENSITIVE = Kunde.BetType = 2.
  RUN AntallMedlemmer.
  RUN VisEndretInfo.
  RUN SettModified(FALSE).
  RUN VisKundeSaldo.
  RUN VisSaldo.
  RUN VisKundeKort.
  IF wAktivFlip = 1 THEN
      APPLY "ENTRY" TO Kunde.Navn IN FRAME FRAME-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisSaldo C-KundKort 
PROCEDURE VisSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wSaldo   AS DEC FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR wTotKjop AS DEC FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.

    
DO WITH FRAME Default-Frame:
    IF NOT AVAILABLE Kunde THEN
        ASSIGN
          FI-Saldo:SCREEN-VALUE = ""
        .
    ASSIGN
        wSaldo   = 0
        wTotKjop = 0.
    FOR EACH KundeSaldo NO-LOCK WHERE
        KundeSaldo.KundeNr = Kunde.KundeNr:
        ASSIGN
            wSaldo    = wSaldo  + KundeSaldo.Saldo
            wTotKjop = wTotKjop + KundeSaldo.TotaltKjop.
    END.
    ASSIGN
      FI-Saldo:SCREEN-VALUE   = STRING(wSaldo)
      .

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-KundKort 
PROCEDURE WinHlp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {winhlp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KundeNummer C-KundKort 
FUNCTION KundeNummer RETURNS CHARACTER
  ( ipKundeNr AS dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: wLedige er initiert fra systemparameter i MainBlokk.
           Det samme er wDbId. 
           Er nummerserien full, returneres ? i nummerfeltet.
           Loop starter fra og med verdi på ipKundeNr.
------------------------------------------------------------------------------*/

  DEF VAR ipLoop    AS   dec               NO-UNDO.
  DEF VAR ipStart   AS   DEC               NO-UNDO.
                       
  CASE wLedige:
      WHEN 1 THEN /* Første ledige */
      DO:
          ASSIGN
              ipStart = IF ipKundeNr < dec(wDbId)
                         THEN dec(wDbId)
                         ELSE ipKundeNr.
          LOOPEN:
          DO ipLoop = ipStart TO DEC(wDbId) + 9999999.0:
              IF NOT CAN-FIND(Kunde WHERE
                              Kunde.KundeNr = ipLoop) THEN
              DO:
                ASSIGN
                  ipKundeNr = ipLoop.
                LEAVE LOOPEN.
              END.
              ELSE
                  ASSIGN
                      ipKundeNr = ?.
          END. /* LOOPEN */
      END.
      WHEN 2 THEN /* Neste ledige */
      DO:
          FIND LAST ledKunde NO-LOCK NO-ERROR.
          IF AVAILABLE ledKunde THEN
              ipKundeNr = ledKunde.KundeNr + 1.
          IF ipKundeNr < dec(wDbId) THEN
          LOOPEN:
          DO ipLoop = DEC(wDbId) + 1 TO DEC(wDbId) + 9999999.0:
              IF NOT CAN-FIND(Kunde WHERE
                              Kunde.KundeNr = ipLoop) THEN
              DO:
                ASSIGN
                  ipKundeNr = ipLoop.
                LEAVE LOOPEN.
              END.
              ELSE
                  ASSIGN
                      ipKundeNr = ?.
          END. /* LOOPEN */
      END.
      OTHERWISE 
          DO:
              MESSAGE "Tildeling av kundenummer - Parameter (Syspara 14 2 4) er ikke satt."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Initieringsfeil".
          END.
  END CASE.
  
/*  MESSAGE "wLedige" wLedige SKIP
          "ipKundeNr" ipKundeNr
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

  /* Sjekker om nummerserien er full */
  IF ipKundeNr = ? THEN
  DO:
      MESSAGE "Nummerserien er full. Kontakt systemansvarlig eller leverandør."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Initieringsfeil".
      ASSIGN
          ipKundeNr = 0.
  END.

  RETURN string(ipKundeNr).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Modifierad C-KundKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-1:
    RETURN IF
      Kunde.KundeNr:MODIFIED = TRUE OR
      Kunde.Navn:MODIFIED = TRUE OR
      Kunde.Adresse1:MODIFIED = TRUE OR
      kunde.Adresse2:MODIFIED = TRUE OR
      Kunde.BydelsNr:MODIFIED = TRUE OR 
      Kunde.PostNr:MODIFIED = TRUE OR
      Kunde.Land:MODIFIED = TRUE OR
      Kunde.ePostAdresse:MODIFIED = TRUE OR
      Kunde.Telefon:MODIFIED = TRUE OR
      Kunde.Mobil:MODIFIED = TRUE OR
      Kunde.Telefaks:MODIFIED = TRUE OR
      Kunde.TypeId:MODIFIED = TRUE OR
      Kunde.GruppeId:MODIFIED = TRUE OR 
      Kunde.MaksKredit:MODIFIED = TRUE OR 
      Kunde.Opphort:MODIFIED = TRUE OR 
      Kunde.KontNavn:MODIFIED = TRUE OR 
      Kunde.Stilling:MODIFIED = TRUE OR
      Kunde.ButikkNr:MODIFIED = TRUE OR 
      Kunde.KontE-Post:MODIFIED = TRUE OR 
      Kunde.KontMobilTlf:MODIFIED = TRUE OR 
      Kunde.KontTelefaks:MODIFIED = TRUE OR 
      Kunde.KontTelefon:MODIFIED = TRUE OR 
      Kunde.KreditSperret:MODIFIED = TRUE OR 
      Kunde.LevAdresse1:MODIFIED = TRUE OR 
      Kunde.LevAdresse2:MODIFIED = TRUE OR 
/*       Kunde.LevLand:MODIFIED = TRUE OR */
      Kunde.BetType:MODIFIED = TRUE OR 
      Kunde.Samlefaktura:MODIFIED = TRUE OR 
      Kunde.Kjon:MODIFIED = TRUE OR 
      Kunde.LevPostNr:MODIFIED = TRUE THEN TRUE ELSE FALSE 
      .
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

