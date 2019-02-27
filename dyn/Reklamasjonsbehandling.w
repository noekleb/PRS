&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

DEF VAR bOK             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR ocValue         AS CHAR   NO-UNDO.
DEF VAR iCL             AS INT    NO-UNDO.
DEF VAR iProfilNr       AS INT    NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hToolbar-2      AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hFieldMap-2     AS HANDLE NO-UNDO.
DEF VAR hParent         AS HANDLE NO-UNDO.
DEF VAR hParentBrowse   AS HANDLE NO-UNDO.
DEF VAR hArtBasSok      AS HANDLE NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR httRapport      AS HANDLE NO-UNDO.
DEF VAR hHurtigRegister AS HANDLE NO-UNDO.
DEF VAR cReturnValue    AS CHAR NO-UNDO.

{dproclibstart.i}
{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frame-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SearchReklamNr btnPostnr ReklamasjonsNr ~
btnArtikkelNr RegistrertDato ReklamStatus SluttfortAv SluttfortDato ForsNr ~
LevNr Kundenr btnStorl KundeNavn KundeAdresse PostNr KundeE-Mail ~
SluttfortBesluttet KundeTelefon KundeMobil AkseptertKunde AkseptertAv ~
AkseptertDato AkseptertNotat Frist-Dato BetalesAv ReklamVerdi ~
ReklamUtgifter ReklamTotal btnKundeNr BetalesBruker BetalesDato ~
BetalesNotat Butik ArtikkelNr Vg LopNr LevKod Varetekst LevFargKod ~
AkseptertBesluttet Storl Antall Pris RabKr linReklamUtgifter VVarekost TTId ~
FeilKode FeilNotat BetalesAvgjort btnReklamasjonsNr btnFeilkode btnLevNr ~
btnForsNr btnButik rectBrowse rectToolBar rectWinToolbar RECT-2 RECT-4 ~
RECT-5 RECT-3 rectToolBar-2 RECT-1 RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS Fakturert OppdLager SearchReklamNr ~
ReklamasjonsNr RegistrertDato ReklamStatus SluttfortAv SluttfortDato ForsNr ~
LevNr Kundenr KundeNavn KundeAdresse PostNr KundeE-Mail SluttfortBesluttet ~
KundeTelefon KundeMobil AkseptertKunde AkseptertAv lblInterntBruk ~
AkseptertDato AkseptertNotat Frist-Dato FoNamn BetalesAv ReklamVerdi ~
ReklamUtgifter ReklamTotal BetalesBruker BetalesDato BetalesNotat Butik ~
ArtikkelNr Vg LopNr LevKod Varetekst LevFargKod AkseptertBesluttet Storl ~
Antall Pris RabKr linReklamUtgifter VVarekost TTId LevNamn FeilKode ~
FeilkodeBeskrivelse FeilNotat OppdLagerAv BetalesAvgjort OppdLagerDato ~
FakturertAv FakturertDato lblKunde lblReklamasjon Poststed ~
lblBehandlingKunde lblLeverandor lblKundeInfo ButNamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuery C-Win 
FUNCTION getQuery RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBlankValue C-Win 
FUNCTION setBlankValue RETURNS LOGICAL
  (INPUT bModified  AS LOG,
   INPUT cFieldList AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setModified C-Win 
FUNCTION setModified RETURNS LOGICAL
  (INPUT bModified  AS LOG,
   INPUT cFieldList AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnArtikkelNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnButik  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnFeilkode  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnForsNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnKundeNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnPostnr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnReklamasjonsNr  NO-FOCUS
     LABEL ".." 
     SIZE 5 BY 1.

DEFINE BUTTON btnStorl  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE AkseptertKunde AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Avgjørelse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE BetalesAv AS CHARACTER FORMAT "X(256)":U 
     LABEL "Betales av" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE ReklamStatus AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE TTId AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Reklam.type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",0
     DROP-DOWN-LIST
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE AkseptertNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 41 BY 4.52 NO-UNDO.

DEFINE VARIABLE BetalesNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 37 BY 4.52 NO-UNDO.

DEFINE VARIABLE FeilNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 59 BY 8.1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AkseptertAv AS CHARACTER FORMAT "X(256)":U 
     LABEL "BrukerId" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE AkseptertDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Antall AS DECIMAL FORMAT "-zz,zzz,zz9" INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE BetalesBruker AS CHARACTER FORMAT "X(15)" 
     LABEL "BrukerId" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE BetalesDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "x(50)":U 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FakturertAv AS CHARACTER FORMAT "X(15)" 
     LABEL "Fakt. av" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE FakturertDato AS DATE FORMAT "99/99/99" 
     LABEL "Fakt. dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE FeilKode AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Feilkode" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FeilkodeBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 35 BY .86 NO-UNDO.

DEFINE VARIABLE FoNamn AS CHARACTER FORMAT "x(50)":U 
      VIEW-AS TEXT 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE ForsNr AS DECIMAL FORMAT "zzzzz9" INITIAL 0 
     LABEL "Kasserernr" 
     VIEW-AS FILL-IN 
     SIZE 14.8 BY 1.

DEFINE VARIABLE Frist-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Svarfrist" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE KundeAdresse AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE KundeE-Mail AS CHARACTER FORMAT "X(30)" 
     LABEL "E-Mail" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE KundeMobil AS CHARACTER FORMAT "X(15)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE KundeNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE Kundenr AS INTEGER FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE KundeTelefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE lblBehandlingKunde AS CHARACTER FORMAT "X(256)":U INITIAL "Behandling kunde" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lblInterntBruk AS CHARACTER FORMAT "X(256)":U INITIAL "Internt bruk:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE lblKunde AS CHARACTER FORMAT "X(256)":U INITIAL "Kunde" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lblKundeInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Internt bruk:" 
      VIEW-AS TEXT 
     SIZE 18 BY .62 NO-UNDO.

DEFINE VARIABLE lblLeverandor AS CHARACTER FORMAT "X(256)":U INITIAL "Behandling leverandør" 
      VIEW-AS TEXT 
     SIZE 30 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lblReklamasjon AS CHARACTER FORMAT "X(256)":U INITIAL "Reklamasjon" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "LevFargKod" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE LevNamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36 BY .95 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE linReklamUtgifter AS DECIMAL FORMAT "-zz,zzz,zz9.99" INITIAL 0 
     LABEL "Utgifter" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE LopNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1 NO-UNDO.

DEFINE VARIABLE OppdLagerAv AS CHARACTER FORMAT "X(15)" 
     LABEL "Oppd. av" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE OppdLagerDato AS DATE FORMAT "99/99/99" 
     LABEL "Opp. dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE Poststed AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "-zz,zzz,zz9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE RabKr AS DECIMAL FORMAT "-zz,zzz,zz9.99" INITIAL 0 
     LABEL "Rabatt" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13.4 BY 1.

DEFINE VARIABLE ReklamasjonsNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Reklam.nr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE ReklamTotal AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Tot.kostnad" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE ReklamUtgifter AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Tot.utgifter" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE ReklamVerdi AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Tot.reklamert" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE SearchReklamNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Søk reklamasjonsnr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE SluttfortAv AS CHARACTER FORMAT "X(15)" 
     LABEL "BrukerId" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE SluttfortDato AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Storl AS CHARACTER FORMAT "x(4)" 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.

DEFINE VARIABLE Varetekst AS CHARACTER FORMAT "X(30)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Vg AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Vg/Løpenr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE VVarekost AS DECIMAL FORMAT "-z,zzz,zz9.99" INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.8 BY 1.91.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 6.91.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 6.91.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 6.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 6.19.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 12.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 6.91.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 12.62.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 1.

DEFINE VARIABLE AkseptertBesluttet AS LOGICAL INITIAL no 
     LABEL "Besluttet" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE BetalesAvgjort AS LOGICAL INITIAL no 
     LABEL "Aksept" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE Fakturert AS LOGICAL INITIAL no 
     LABEL "Fakturert" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE OppdLager AS LOGICAL INITIAL no 
     LABEL "Oppd.lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE SluttfortBesluttet AS LOGICAL INITIAL no 
     LABEL "Sluttført" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frame-A
     Fakturert AT ROW 8.43 COL 167 HELP
          "Reklamasjon er fakturert"
     OppdLager AT ROW 5.43 COL 167 HELP
          "Reklamasjon er oppdatert i lager og statistikker"
     SearchReklamNr AT ROW 2.86 COL 21.8 COLON-ALIGNED HELP
          "Unikt nummer på reklamasjonen - automatisk tildelt."
     btnPostnr AT ROW 8.43 COL 113 NO-TAB-STOP 
     ReklamasjonsNr AT ROW 5.43 COL 15 COLON-ALIGNED HELP
          "Unikt nummer på reklamasjonen - automatisk tildelt."
     btnArtikkelNr AT ROW 21.81 COL 129.8 NO-TAB-STOP 
     RegistrertDato AT ROW 5.43 COL 35.6 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret" NO-LABEL
     ReklamStatus AT ROW 6.48 COL 15 COLON-ALIGNED
     SluttfortAv AT ROW 7.52 COL 15 COLON-ALIGNED HELP
          "Markert sluttført av bruker."
     SluttfortDato AT ROW 7.52 COL 35.6 COLON-ALIGNED HELP
          "Sluttført dato" NO-LABEL
     ForsNr AT ROW 8.57 COL 15 COLON-ALIGNED HELP
          "Kasserernummer"
     LevNr AT ROW 9.62 COL 15 COLON-ALIGNED
     Kundenr AT ROW 5.29 COL 91 COLON-ALIGNED
     btnStorl AT ROW 26.91 COL 117.6 NO-TAB-STOP 
     KundeNavn AT ROW 6.33 COL 91 COLON-ALIGNED HELP
          "Navn på kunde som reklamerer varen."
     KundeAdresse AT ROW 7.38 COL 91 COLON-ALIGNED HELP
          "Adresse til kunden som reklamerte varen."
     PostNr AT ROW 8.43 COL 91 COLON-ALIGNED HELP
          "Postnummer til kunden som reklamerte varen."
     KundeE-Mail AT ROW 9.52 COL 91 COLON-ALIGNED HELP
          "E-Mail adresse til kunde."
     SluttfortBesluttet AT ROW 7.62 COL 51.2 NO-TAB-STOP 
     KundeTelefon AT ROW 10.57 COL 91 COLON-ALIGNED
     KundeMobil AT ROW 10.57 COL 119.8 COLON-ALIGNED HELP
          "Mobiltelefonnumer til kunden som har reklamert varen."
     AkseptertKunde AT ROW 13.1 COL 15 COLON-ALIGNED
     AkseptertAv AT ROW 14.14 COL 15 COLON-ALIGNED
     lblInterntBruk AT ROW 13.38 COL 60 COLON-ALIGNED NO-LABEL
     AkseptertDato AT ROW 14.14 COL 29 COLON-ALIGNED NO-LABEL
     AkseptertNotat AT ROW 14.1 COL 62 NO-LABEL
     Frist-Dato AT ROW 15.24 COL 15 COLON-ALIGNED HELP
          "Svarfrist til kunde. Normalt 14dg. En uke + postgang."
     FoNamn AT ROW 8.62 COL 35 COLON-ALIGNED NO-LABEL
     BetalesAv AT ROW 13.14 COL 118 COLON-ALIGNED
     ReklamVerdi AT ROW 14.24 COL 118 COLON-ALIGNED HELP
          "Beløp som kunden krever tilbakebeltalt (Inkl. mva)."
     ReklamUtgifter AT ROW 15.29 COL 118 COLON-ALIGNED HELP
          "Utgifter som er påløpt ved behandling av reklamajson."
     ReklamTotal AT ROW 16.33 COL 118 COLON-ALIGNED HELP
          "Totalt kostnad ved reklamasjonen."
     btnKundeNr AT ROW 5.29 COL 113 NO-TAB-STOP 
     BetalesBruker AT ROW 17.38 COL 118 COLON-ALIGNED HELP
          "Bruker som registrerte beslutning."
     BetalesDato AT ROW 17.38 COL 138.6 COLON-ALIGNED HELP
          "Dato da beslutning ble fattet." NO-LABEL
     BetalesNotat AT ROW 14.1 COL 165 NO-LABEL
     Butik AT ROW 20.76 COL 105.8 COLON-ALIGNED HELP
          "Butikknummer"
     ArtikkelNr AT ROW 21.81 COL 105.8 COLON-ALIGNED
     Vg AT ROW 22.86 COL 105.8 COLON-ALIGNED HELP
          "'varegruppenummer"
     LopNr AT ROW 22.86 COL 118 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.6 BY 32.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME frame-A
     LevKod AT ROW 23.91 COL 105.8 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     Varetekst AT ROW 24.91 COL 105.8 COLON-ALIGNED HELP
          "Varetekst - eller kort beskrivelse av varen."
     LevFargKod AT ROW 25.91 COL 105.8 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     AkseptertBesluttet AT ROW 14.29 COL 46 HELP
          "Besluttning på reklamajsonen er fattet." NO-TAB-STOP 
     Storl AT ROW 26.91 COL 105.8 COLON-ALIGNED HELP
          "Størrelse"
     Antall AT ROW 27.91 COL 105.8 COLON-ALIGNED HELP
          "Antall"
     Pris AT ROW 28.95 COL 105.8 COLON-ALIGNED HELP
          "Pris"
     RabKr AT ROW 29.95 COL 105.8 COLON-ALIGNED HELP
          "Pris"
     linReklamUtgifter AT ROW 30.95 COL 105.8 COLON-ALIGNED HELP
          "Utgifter som er påløpt ved behandling av reklamajson."
     VVarekost AT ROW 31.95 COL 105.8 COLON-ALIGNED HELP
          "Vektet varekost"
     TTId AT ROW 21.81 COL 149.4 COLON-ALIGNED
     LevNamn AT ROW 9.62 COL 35 COLON-ALIGNED NO-LABEL
     FeilKode AT ROW 23.62 COL 149.4 COLON-ALIGNED HELP
          "Feilkode som beskriver hva som er feil med varen."
     FeilkodeBeskrivelse AT ROW 23.62 COL 164 COLON-ALIGNED NO-LABEL
     FeilNotat AT ROW 24.81 COL 142 NO-LABEL
     OppdLagerAv AT ROW 7.33 COL 165 COLON-ALIGNED HELP
          "Bruker som oppdaterte lager og statistikker"
     BetalesAvgjort AT ROW 17.43 COL 154 NO-TAB-STOP 
     OppdLagerDato AT ROW 6.33 COL 165 COLON-ALIGNED HELP
          "Dato oppdatert lager og statistikker"
     FakturertAv AT ROW 10.52 COL 165 COLON-ALIGNED HELP
          "Bruker som fakturerte reklamasjonen"
     FakturertDato AT ROW 9.52 COL 165 COLON-ALIGNED HELP
          "Fakturert dato"
     btnReklamasjonsNr AT ROW 2.86 COL 44 NO-TAB-STOP 
     btnFeilkode AT ROW 23.62 COL 161.4 NO-TAB-STOP 
     btnLevNr AT ROW 9.62 COL 32 NO-TAB-STOP 
     btnForsNr AT ROW 8.57 COL 31.8 NO-TAB-STOP 
     lblKunde AT ROW 4.33 COL 76 NO-LABEL
     lblReklamasjon AT ROW 4.43 COL 2.6 NO-LABEL
     Poststed AT ROW 8.48 COL 115.6 COLON-ALIGNED NO-LABEL
     lblBehandlingKunde AT ROW 12.19 COL 2 NO-LABEL
     lblLeverandor AT ROW 12.19 COL 105 NO-LABEL
     lblKundeInfo AT ROW 13.38 COL 163 COLON-ALIGNED NO-LABEL
     btnButik AT ROW 20.81 COL 120 NO-TAB-STOP 
     ButNamn AT ROW 20.76 COL 123 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 20.52 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 192
     RECT-2 AT ROW 5.05 COL 2
     RECT-4 AT ROW 12.91 COL 2
     RECT-5 AT ROW 12.91 COL 105
     RECT-3 AT ROW 5.05 COL 76
     rectToolBar-2 AT ROW 19.43 COL 2
     RECT-1 AT ROW 2.43 COL 1.8
     RECT-6 AT ROW 20.52 COL 94
     RECT-7 AT ROW 5.05 COL 152
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.6 BY 32.33.


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
         TITLE              = "Reklamasjonsbehandling"
         HEIGHT             = 32.33
         WIDTH              = 202.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frame-A
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN ButNamn IN FRAME frame-A
   NO-ENABLE                                                            */
ASSIGN 
       ButNamn:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR TOGGLE-BOX Fakturert IN FRAME frame-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FakturertAv IN FRAME frame-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FakturertDato IN FRAME frame-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FeilkodeBeskrivelse IN FRAME frame-A
   NO-ENABLE                                                            */
ASSIGN 
       FeilkodeBeskrivelse:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN FoNamn IN FRAME frame-A
   NO-ENABLE                                                            */
ASSIGN 
       FoNamn:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN lblBehandlingKunde IN FRAME frame-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lblBehandlingKunde:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN lblInterntBruk IN FRAME frame-A
   NO-ENABLE                                                            */
ASSIGN 
       lblInterntBruk:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN lblKunde IN FRAME frame-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lblKunde:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN lblKundeInfo IN FRAME frame-A
   NO-ENABLE                                                            */
ASSIGN 
       lblKundeInfo:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN lblLeverandor IN FRAME frame-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lblLeverandor:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN lblReklamasjon IN FRAME frame-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lblReklamasjon:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR FILL-IN LevNamn IN FRAME frame-A
   NO-ENABLE                                                            */
ASSIGN 
       LevNamn:READ-ONLY IN FRAME frame-A        = TRUE.

/* SETTINGS FOR TOGGLE-BOX OppdLager IN FRAME frame-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OppdLagerAv IN FRAME frame-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OppdLagerDato IN FRAME frame-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Poststed IN FRAME frame-A
   NO-ENABLE                                                            */
ASSIGN 
       Poststed:READ-ONLY IN FRAME frame-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-A
/* Query rebuild information for FRAME frame-A
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frame-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Reklamasjonsbehandling */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Reklamasjonsbehandling */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Reklamasjonsbehandling */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnArtikkelNr C-Win
ON CHOOSE OF btnArtikkelNr IN FRAME frame-A /* ... */
OR F10 OF artikkelnr
DO:
  IF VALID-HANDLE(hArtBasSok) THEN 
    APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  RUN InitializeObject IN hArtBasSok.
  DYNAMIC-FUNCTION('setCloseOnSelect' IN hArtBasSok,TRUE).
  RUN MoveToTop IN hArtBasSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButik C-Win
ON CHOOSE OF btnButik IN FRAME frame-A /* ... */
OR F10 OF Forsnr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Butik;ButNamn".

  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      Butik:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
      butNamn:SCREEN-VALUE = ENTRY(2,cLookupValue,'|')
    .
    APPLY "ANY-PRINTABLE" TO Butik.
  END.
  APPLY "entry" TO Butik.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFeilkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFeilkode C-Win
ON CHOOSE OF btnFeilkode IN FRAME frame-A /* ... */
OR F10 OF Feilkode
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "FeilKode;Beskrivelse".

  RUN JBoxDLookup.w ("Feilkode;FeilKode;Beskrivelse", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      FeilKode:SCREEN-VALUE            = ENTRY(1,cLookupValue,"|")
      FeilKodeBeskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
    .
    DYNAMIC-FUNCTION('setModified',FALSE,'FeilKode,FeilKodeBeskrivelse').
    APPLY 'ANY-PRINTABLE' TO FeilKode.
  END.
  APPLY "entry" TO FeilKode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnForsNr C-Win
ON CHOOSE OF btnForsNr IN FRAME frame-A /* ... */
OR F10 OF Forsnr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ForsNr;ButikkNr;FoNamn".

  RUN JBoxDLookup.w ("Forsalj;ForsNr;ButikkNr;FoNamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      ForsNr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
      FoNamn:SCREEN-VALUE     = ENTRY(3,cLookupValue,'|')
    .
    DYNAMIC-FUNCTION('setModified',FALSE,'ForsNr,FoNamn').
    APPLY "ANY-PRINTABLE" TO ForsNr.
  END.
  APPLY "entry" TO ForsNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundeNr C-Win
ON CHOOSE OF btnKundeNr IN FRAME frame-A /* ... */
OR F10 OF kundenr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KundeNr;Navn;Adresse1;PostNr;beskrivelse;ePostAdresse;PrivatTlf;MobilTlf".

  RUN JBoxDLookup.w ("Kunde;Kundenr;Navn;Adresse1;PrivatTlf;MobilTlf;ePostAdresse,Post;postnr;beskrivelse", 
                     "WHERE TRUE" + ',FIRST post OF Kunde NO-LOCK',
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      kundenr:SCREEN-VALUE      = ENTRY(1,cLookupValue,"|")
      kundenavn:SCREEN-VALUE    = ENTRY(2,cLookupValue,"|")
      kundeadresse:SCREEN-VALUE = ENTRY(3,cLookupValue,"|")
      postnr:SCREEN-VALUE       = ENTRY(4,cLookupValue,'|')
      poststed:SCREEN-VALUE     = ENTRY(5,cLookupValue,'|')
      KundeE-Mail:SCREEN-VALUE  = ENTRY(6,cLookupValue,'|')
      KundeTelefon:SCREEN-VALUE = ENTRY(7,cLookupValue,'|')
      KundeMobil:SCREEN-VALUE   = ENTRY(8,cLookupValue,'|')
    .
    APPLY 'ANY-PRINTABLE' TO kundenr.
    DYNAMIC-FUNCTION('setModified',FALSE,'kundenr,kundenavn,kundeadresse,postnr,poststed,kundeE-mail,kundetelefon,kundemobil').
  END.
  APPLY "entry" TO kundenr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevNr C-Win
ON CHOOSE OF btnLevNr IN FRAME frame-A /* ... */
OR F10 OF LevNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "LevNr;LevNamn".

  RUN JBoxDLookup.w ("LevBas;LevNr;LevNamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      LevNr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
    APPLY 'ANY-PRINTABLE' TO LevNr.
/*     IF LevNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
  END.
  APPLY "entry" TO LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME frame-A /* ... */
OR F10 OF postnr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "postnr;Beskrivelse".

  RUN JBoxDLookup.w ("Post;postnr;Beskrivelse", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      postnr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
      poststed:SCREEN-VALUE   = ENTRY(2,cLookupValue,"|")
    .
    APPLY 'ANY-PRINTABLE' TO postnr.
/*     IF postnr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
  END.
  APPLY "entry" TO postnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReklamasjonsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReklamasjonsNr C-Win
ON CHOOSE OF btnReklamasjonsNr IN FRAME frame-A /* .. */
OR F10 OF SearchReklamNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "".

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Reklamasjonslogg;Reklamasjonsnr;Kundenr;Kundenavn;KundeTelefon;KundeMobil;ArtikkelNr"
                    ,"WHERE true"
                    ,""                                                  
                    ,"Reklamasjonsnr,KundeTelefon,KundeMobil",   /* <- return values for these fields */
                      OUTPUT cLookupValue,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      SearchReklamNr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
    IF SearchReklamNr:MODIFIED AND VALID-HANDLE(hQuery) THEN 
    DO:
      DYNAMIC-FUNCTION("setAttribute",hQuery,"QueryFilter",'WHERE reklamasjonsnr = ' + SearchReklamNr:SCREEN-VALUE). 
      RUN InvokeMethod (hQuery,'OpenQuery').   
    END.
  END.
  ELSE APPLY "entry" TO SearchReklamNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStorl C-Win
ON CHOOSE OF btnStorl IN FRAME frame-A /* ... */
OR F10 OF storl
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.
  DEF VAR iStrTypeId    AS INT  NO-UNDO.

  iStrTypeId = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.artikkelnr = DEC(' + ArtikkelNr:SCREEN-VALUE + ')','StrTypeId').
  IF iStrTypeId NE 0 AND iStrTypeId NE ? THEN
  DO:
    cLookupValue = "soStorl".
  
    RUN JBoxDLookup.w ("StrType;StrTypeId;KortNavn;Beskrivelse,strTstr;soStorl", 
                       "WHERE StrType.StrTypeId = " + STRING(iStrTypeId) 
                       + ", EACH strTstr OF strType no-lock",
                       INPUT-OUTPUT cLookupValue).
  
    IF cLookupValue NE "" THEN DO:
      ASSIGN 
        storl:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
      .
      APPLY 'ANY-PRINTABLE' TO storl.
/*       IF kundenr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    END.
    ELSE APPLY "entry" TO storl.
  END.
  ELSE
    MESSAGE 'Ingen størrelsestype angitt for gjeldene artikkel, vennligst rett dette.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butik C-Win
ON TAB OF Butik IN FRAME frame-A /* Butikk */
OR 'RETURN' OF Butik
DO:
    cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'Butik,ButNamn').
    IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
    DO:
        ButNamn:SCREEN-VALUE = "".
        MESSAGE "Ugyldig butikk."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO Butik.
        RETURN NO-APPLY.
    END.
    ELSE
        ButNamn:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SearchReklamNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SearchReklamNr C-Win
ON RETURN OF SearchReklamNr IN FRAME frame-A /* Søk reklamasjonsnr */
DO:
  DEF VAR fReturn AS DEC NO-UNDO.

  fReturn = DYNAMIC-FUNCTION('getFieldValues','Reklamasjonslogg','WHERE Reklamasjonslogg.reklamasjonsnr = DEC(' + SearchReklamNr:SCREEN-VALUE + ')','Reklamasjonsnr').
  IF fReturn NE 0 AND fReturn NE ? THEN
  DO:
    DYNAMIC-FUNCTION("setAttribute",hQuery,"QueryFilter",'WHERE reklamasjonsnr = ' + SearchReklamNr:SCREEN-VALUE). 
    RUN InvokeMethod (hQuery,'OpenQuery').   
    ASSIGN SearchReklamNr:SCREEN-VALUE = "".
  END.
  ELSE DO:
      MESSAGE "Ukjent reklamasjonsnummer."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN SearchReklamNr:SCREEN-VALUE = "".
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TTId C-Win
ON VALUE-CHANGED OF TTId IN FRAME frame-A /* Reklam.type */
DO:
   IF SELF:MODIFIED THEN RUN StartQuery.
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
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hArtBasSok) THEN DELETE OBJECT hArtBasSok.
  IF VALID-HANDLE(hHurtigRegister) THEN DELETE OBJECT hHurtigRegister.
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
  hparent = SOURCE-PROCEDURE.
  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
{incl/supptrigg.i hFieldMap}

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar THEN
  DO:
      bOk = DYNAMIC-FUNCTION("runProc","reklamasjonslogg_delete.p",STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE),?).
  END.


  RUN SUPER.

  IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar-2 THEN
  DO:
    bOk = DYNAMIC-FUNCTION("runProc","reklamasjonslogg_recalc.p",STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE),?).
    DYNAMIC-FUNCTION("RefreshRowids",hQuery,hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE).
    DYNAMIC-FUNCTION('DisplayFieldMap',hFieldMap).
  END.
  ELSE DO:
      RUN startQuery IN hParent NO-ERROR.
  END.
  ASSIGN
      bOk = TRUE
      .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hObject AS HANDLE NO-UNDO.
DEF VAR cTest AS CHAR NO-UNDO.

RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    hObject = WIDGET-HANDLE(DYNAMIC-FUNCTION("getLinkedObject",hQuery,"Parent","from"))
   .
  IF VALID-HANDLE(hObject) THEN
    ASSIGN
      hObject = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonHurtig"))
      hObject:VISIBLE = FALSE
    .

/*   iArtNr = DYNAMIC-FUNCTION('getFieldValues','reklamasjonslinje','WHERE Reklamasjonslinje.reklamasjonsnr = DEC(' + STRING(hFieldMap:BUFFER-FIELD('Reklamasjonsnr'):BUFFER-VALUE) + ')' */
/*                                                                 + ' AND Reklamasjonslinje.artikkelnr = DEC(' + STRING(ifArtikkelNr) + ')','ArtikkelNr').                               */

  ASSIGN 
    SearchReklamNr:SCREEN-VALUE = IF hFieldMap:AVAIL THEN hFieldMap:BUFFER-FIELD('reklamasjonsnr'):BUFFER-VALUE ELSE '0'
    SearchReklamNr:SENSITIVE    = TRUE
    SearchReklamNr:MODIFIED     = FALSE
    btnReklamasjonsnr:SENSITIVE = TRUE
    /*
    Vg:SENSITIVE                = FALSE
    LopNr:SENSITIVE             = FALSE
    */
  .
  IF hFieldMap-2:AVAIL THEN
  DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'Butik,ButNamn').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
          ButNamn:SCREEN-VALUE = "".
      ELSE
          ButNamn:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').
      ASSIGN
          LevKod:SENSITIVE    = FALSE
          Varetekst:SENSITIVE  = FALSE
          LevFargKod:SENSITIVE = FALSE
          .
  END.
  ELSE ButNamn:SCREEN-VALUE = "".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eMailRecord C-Win 
PROCEDURE eMailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cMessage AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF KundeE-Mail:SCREEN-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Epost adresse er ikke utfylt","","").
    RETURN.
  END.
  cMessage = "Hei, din reklamasjon er ferdigbehandlet." + CHR(10) 
           +  "Vennlig hilsen " 
           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + Butik:SCREEN-VALUE,"ButNamn,BuAdr")
                    ,"|",", ").
  RUN SendEmail.w (KundeE-Mail:SCREEN-VALUE,
                 "Reklam.nr: " + ReklamasjonsNr:SCREEN-VALUE,
                 cMessage,
                 OUTPUT bOK,
                 OUTPUT cMessage).
  IF bOK THEN DO:
    AkseptertNotat:SCREEN-VALUE = cMessage + CHR(10) + AkseptertNotat:SCREEN-VALUE.
    RUN InvokeMethod(hFieldMap,"SaveRecord").
  END.
END.  
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
  DISPLAY Fakturert OppdLager SearchReklamNr ReklamasjonsNr RegistrertDato 
          ReklamStatus SluttfortAv SluttfortDato ForsNr LevNr Kundenr KundeNavn 
          KundeAdresse PostNr KundeE-Mail SluttfortBesluttet KundeTelefon 
          KundeMobil AkseptertKunde AkseptertAv lblInterntBruk AkseptertDato 
          AkseptertNotat Frist-Dato FoNamn BetalesAv ReklamVerdi ReklamUtgifter 
          ReklamTotal BetalesBruker BetalesDato BetalesNotat Butik ArtikkelNr Vg 
          LopNr LevKod Varetekst LevFargKod AkseptertBesluttet Storl Antall Pris 
          RabKr linReklamUtgifter VVarekost TTId LevNamn FeilKode 
          FeilkodeBeskrivelse FeilNotat OppdLagerAv BetalesAvgjort OppdLagerDato 
          FakturertAv FakturertDato lblKunde lblReklamasjon Poststed 
          lblBehandlingKunde lblLeverandor lblKundeInfo ButNamn 
      WITH FRAME frame-A IN WINDOW C-Win.
  ENABLE SearchReklamNr btnPostnr ReklamasjonsNr btnArtikkelNr RegistrertDato 
         ReklamStatus SluttfortAv SluttfortDato ForsNr LevNr Kundenr btnStorl 
         KundeNavn KundeAdresse PostNr KundeE-Mail SluttfortBesluttet 
         KundeTelefon KundeMobil AkseptertKunde AkseptertAv AkseptertDato 
         AkseptertNotat Frist-Dato BetalesAv ReklamVerdi ReklamUtgifter 
         ReklamTotal btnKundeNr BetalesBruker BetalesDato BetalesNotat Butik 
         ArtikkelNr Vg LopNr LevKod Varetekst LevFargKod AkseptertBesluttet 
         Storl Antall Pris RabKr linReklamUtgifter VVarekost TTId FeilKode 
         FeilNotat BetalesAvgjort btnReklamasjonsNr btnFeilkode btnLevNr 
         btnForsNr btnButik rectBrowse rectToolBar rectWinToolbar RECT-2 RECT-4 
         RECT-5 RECT-3 rectToolBar-2 RECT-1 RECT-6 RECT-7 
      WITH FRAME frame-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frame-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HurtigRecord C-Win 
PROCEDURE HurtigRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT VALID-HANDLE(hHurtigRegister) THEN 
  DO:
    RUN reklamasjonslogg_hurtig.w PERSISTENT SET hHurtigRegister.
    RUN initializeObject IN hHurtigRegister.
    SUBSCRIBE TO 'refreshByRowid' IN hHurtigRegister.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  /* Sentralllager og profilnr. */
  iCL       = int(DYNAMIC-FUNCTION('getFieldValues','SysPara','where sysHId = 5 and sysGr = 1 and ParaNr = 1','Parameter1')).
  iProfilNr = int(DYNAMIC-FUNCTION('getFieldValues','Butiker','where Butik = ' + string(iCL),'ProfilNr')).

  hQuery = DYNAMIC-FUNCTION("NewQuery",1,""
                             ,"Reklamasjonslogg;SolgtDato;SluttfortDato;SluttfortBesluttet;SluttfortAv;ReklamVerdi;ReklamUtgifter;ReklamTotal;ReklamStatus;ReklamasjonsNr;RegistrertTid;RegistrertDato;RegistrertAv;PostNr;LevNr;LevKod;KundeTelefon;KundeNr;KundeNavn;KundeMobil;KundeE-Mail;KundeAdresse;Frist-Dato;ETid;EDato;BrukerID;BetalesNotat;BetalesDato;BetalesBruker;BetalesAvgjort;BetalesAv;Beskr;ArtikkelNr;AkseptertVerdi;AkseptertNotat;AkseptertKunde;AkseptertDato;AkseptertBesluttet;AkseptertAv;Forsnr;Fakturert;FakturertDato;FakturertAv;OppdLager;OppdLagerDato;OppdLagerAv"
                            + ";!+_Total|decimal|>>><>>9.99|_Total"
                            + ",post;beskrivelse"
                            + ",levbas;levnamn"
                            + ",forsalj;fonamn"
                             ,"WHERE FALSE "
                            + ", FIRST post OUTER-JOIN WHERE post.postnr = Reklamasjonslogg.postnr NO-LOCK"
                            + ", FIRST levbas OUTER-JOIN WHERE levbas.levnr = Reklamasjonslogg.levnr NO-LOCK"
                            + " ,FIRST forsalj OUTER-JOIN WHERE Forsalj.ForsNr = int(Reklamasjonslogg.ForsNr) NO-LOCK"
                             ,"").
 

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hQuery,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "ReklamStatus,SluttfortBesluttet,forsnr,LevNr"
                    + ",KundeNr,KundeNavn,KundeMobil,KundeTelefon,KundeE-Mail,PostNr,KundeAdresse"
                    + ",AkseptertKunde,AkseptertNotat,AkseptertBesluttet,Frist-Dato"                                       
                    + ",BetalesNotat,BetalesAvgjort,BetalesAv",   /* Update columns in buffer */
                      " ",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                    "RegistrertDato,ReklamasjonsNr,beskrivelse,ReklamUtgifter,ReklamVerdi,ReklamTotal,SluttfortAv,SluttfortDato,AkseptertAv,AkseptertDato,BetalesBruker,BetalesDato,levnamn,fonamn,Fakturert,FakturertDato,FakturertAv,OppdLager,OppdLagerDato,OppdLagerAv",        /* Additional buffer and displ.fields - not updateable*/
                    "RegistrertDato,ReklamasjonsNr,Poststed,ReklamUtgifter,ReklamVerdi,ReklamTotal,SluttfortAv,SluttfortDato,AkseptertAv,AkseptertDato,BetalesBruker,BetalesDato,levnamn,fonamn,Fakturert,FakturertDato,FakturertAv,OppdLager,OppdLagerDato,OppdLagerAv",                           /* Corresponding fill-ins */
                    "btnLevNr,btnKundenr,btnPostNr").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).
  
  DYNAMIC-FUNCTION('setAttribute',hQuery,'calcfieldproc','reklamasjonslogg_qrycalc.p').
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","=reklamasjonslogg_update.p"). /*=updval_tabell.p*/
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"PostUpdateProc","reklamasjonslogg_post_update_bl.p").
  
  /*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields",'RegistrertDato'). */
  
  ASSIGN 
    ReklamStatus:DELIMITER = "|"
    ReklamStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 1")
  . 
  ASSIGN 
    AkseptertKunde:DELIMITER = "|"
    AkseptertKunde:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 2")
  . 
  ASSIGN 
    BetalesAv:DELIMITER = "|"
    BetalesAv:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","KravKode;KravKode|Beskrivelse;KravKode","where TRUE")
    TTId:DELIMITER = "|"
    TTId:LIST-ITEM-PAIRS = "|0|" +  DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 12")
  . 
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "first|Navigate¤ENABLE,prev|Navigate¤ENABLE,next|Navigate,last|Navigate,rule,"
                    + "new,undo,delete,save,Print,Hurtig;Hurtig registrering¤ENABLE,Email;Send ep&ost,SMS;S&MS"
                    + "" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  hToolbar-2 = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar-2:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,save,undo,delete"
                    + ",Strekkoderegistrering;Strekkoderegistrering¤Enable,VgLopeNrregistrering;VgLøpeNr registrering¤Enable" /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                  rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                  100,                            /* Rows to batch */
                  "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                  "reklamasjonslinje"
                  + ";LinjeNr|LNr;TTId|Rekl.typ;Vg|Vg;LopNr|Lpnr|>>>>>9;levkod;varetekst;levfargkod;storl;feilkode;!feilnotat;antall;pris;RabKr;Butik;ArtikkelNr;ReklamUtgifter|Utgifter;VVarekost|Varekost"
                  + ",Feilkode;!Beskrivelse"
                  ,"WHERE FALSE"
                  + ",FIRST Feilkode OF Reklamasjonslinje"
                  ,"sort|LinjeNr DESC").             /* Initial sort column */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar-2).
  DYNAMIC-FUNCTION("createParentLink",hBrowse,hQuery,'Reklamasjonsnr').

  hFieldMap-2 = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                     and their corresponding buffer columns return handle equals the buffer handle */
                  hBrowse,
                  FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                  "Butik,artikkelnr,Vg,LopNr,LevKod,varetekst,levfargkod,TTId,feilkode,feilnotat,storl,antall,pris,RabKr,ReklamUtgifter,VVarekost",   /* Update columns in buffer */
                  "Butik,artikkelnr,Vg,LopNr,LevKod,varetekst,levfargkod,TTId,feilkode,feilnotat,storl,antall,pris,RabKr,linReklamUtgifter,VVarekost",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                  "Beskrivelse",            /* Additional buffer and displ.fields - not updateable*/
                    "FeilkodeBeskrivelse",  /* Corresponding fill-ins */
                  "btnArtikkelNr,btnFeilKode,btnStorl").   /* other input widgets and lookup buttons for update fill-ins */
  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap-2,"customUpdateValProc","=reklamasjonslinje_update.p"). /*=table_update.p*/ 
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMap-2,"customDeleteValProc","=reklamasjonslinje_delete.p"). /*=table_delete.p*/ */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap-2,"customCreateProc","reklamasjonslinje_create.p"). /*=table_create.p*/ 
  DYNAMIC-FUNCTION("setAttribute",hFieldMap-2,"PostUpdateProc","reklamasjonslinje_post_update_bl.p").
  
  /*
  DYNAMIC-FUNCTION("setAttribute",hFieldMap-2,'BufferExtraFields','Vg,LopNr'). /* Komma separert. BufferExtraValues må kalles i SaveRecord. */
  */
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap-2,hBrowse).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap-2,hToolbar-2).
  
  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */


  DYNAMIC-FUNCTION("setAttribute",hQuery,"QueryFilter",'WHERE FALSE'). 
  RUN invokemethod(hQuery,'OpenQuery').

END.
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "AkseptertNotat,rect-1,rect-2,rect-4,FeilNotat").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,rect-1,rect-2,rect-3,rect-4,rect-5,AkseptertNotat,betalesNotat,FeilNotat").
DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rect-6,ButNamn" + DYNAMIC-FUNCTION("getWidgetNamesByLasso",rect-6:HANDLE IN FRAME {&FRAME-NAME},0,
                                     "frame,control-frame,rectangle,browse,fill-in,button,editor,COMBO-BOX")).
DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rect-6," + DYNAMIC-FUNCTION("getWidgetNamesByLasso",rect-6:HANDLE IN FRAME {&FRAME-NAME},0,
                                     "frame,control-frame,rectangle,browse,fill-in,button,editor")).

/*   DYNAMIC-FUNCTION("setAnchor",THIS-PROCEDURE:CURRENT-WINDOW,rectBrowse:HANDLE IN FRAME {&FRAME-NAME}, */
/*                    STRING(rectLagerStatus:HANDLE) + ","                                                */
/*                  + STRING(hLagerStatusFrame)                                                           */
/*                   ,"Y").                                                                               */

DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,850,450,0,0).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField C-Win 
PROCEDURE LeaveOfField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DEF VAR cReturnValue AS CHAR NO-UNDO.
DEF VAR fArtNr       AS DEC NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN 'ArtikkelNr' THEN
    DO:
      IF DEC(ArtikkelNr:SCREEN-VALUE) > 0 THEN
      DO:
          cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.ArtikkelNr = DEC(' + SELF:SCREEN-VALUE + ')','ArtikkelNr,LevKod,Beskr,LevFargKod,Vg,LopNr').
          IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
          DO:
            MESSAGE "Ugyldig artikkelnummer."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            READKEY.
            APPLY "ENTRY" TO ArtikkelNr.
            RETURN NO-APPLY.
          END.
          ELSE
          DO:
            ASSIGN 
              LevKod:SCREEN-VALUE     = ENTRY(2,cReturnValue,'|')
              Varetekst:SCREEN-VALUE  = ENTRY(3,cReturnValue,'|')
              LevFargKod:SCREEN-VALUE = ENTRY(4,cReturnValue,'|')
              Vg:SCREEN-VALUE         = ENTRY(5,cReturnValue,'|')
              LopNr:SCREEN-VALUE      = ENTRY(6,cReturnValue,'|')
            .
            APPLY "ENTRY" TO Storl.
            RETURN NO-APPLY.
          END.
      END.
    END.
    WHEN 'vg' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','VarGr','WHERE VarGr.Vg = ' + Vg:SCREEN-VALUE,'Vg,VgBeskr').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        MESSAGE "Ugyldig varegruppe eller varegruppe ikke angitt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO ArtikkelNr.
        RETURN NO-APPLY.
      END.
    END.
    WHEN 'lopnr' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Vg = ' + Vg:SCREEN-VALUE + 
                                        ' and ArtBas.LopNr = ' + LopNr:SCREEN-VALUE,'Vg,LopNr,ArtikkelNr').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        MESSAGE "Ugyldig varegruppe/løpenummer."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO LopNr.
        RETURN NO-APPLY.
      END.
      ELSE DO:
          ASSIGN
              ArtikkelNr:SCREEN-VALUE = ENTRY(3,cReturnValue,"|").
          APPLY "LEAVE" TO ArtikkelNr.
      END.
    END.
    WHEN 'feilkode' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Feilkode','WHERE FeilKode.feilkode = ' + feilkode:SCREEN-VALUE,'Feilkode,beskrivelse').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnFeilkode.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        ASSIGN 
          FeilkodeBeskrivelse:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
        .
      END.

    END.
    WHEN 'storl' THEN 
    DO:
      cReturnValue = storl:SCREEN-VALUE.
      RUN FixStorl IN h_dproclib(INPUT-OUTPUT cReturnValue).
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','strkonv','WHERE strkonv.storl = ' + QUOTER(cReturnValue),'storl').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnStorl.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        storl:SCREEN-VALUE = cReturnValue.
      END.
    END.
    WHEN 'ForsNr' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Forsalj','WHERE Forsalj.ForsNr = ' + ForsNr:SCREEN-VALUE,'ForsNr,FoNamn,ButikkNr').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnForsNr.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        ASSIGN 
          FoNamn:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
          .
      END.
    END.
  END CASE.
END.
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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturnValue AS CHAR NO-UNDO.

RUN SUPER.
IF DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'new' THEN
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    SearchReklamNr:SCREEN-VALUE = STRING(0)
    SearchReklamNr:SENSITIVE    = FALSE
    btnReklamasjonsnr:SENSITIVE = FALSE
    RegistrertDato:SCREEN-VALUE = STRING(TODAY)
    ReklamStatus:SCREEN-VALUE   = STRING(1)
  .
  cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Kunde','WHERE Kunde.Navn = ' + QUOTER('Reklamasjon'),'KundeNr,Navn,Adresse1,PostNr,ePostAdresse,PrivatTlf,MobilTlf').
  
  IF cReturnValue NE '' AND cReturnValue NE ? THEN
  DO:
    ASSIGN 
      kundenr:SCREEN-VALUE      = ENTRY(1,cReturnValue,"|")
      kundenavn:SCREEN-VALUE    = ENTRY(2,cReturnValue,"|")
      kundeadresse:SCREEN-VALUE = ENTRY(3,cReturnValue,"|")
      postnr:SCREEN-VALUE       = ENTRY(4,cReturnValue,'|')
      KundeE-Mail:SCREEN-VALUE  = ENTRY(5,cReturnValue,'|')
      KundeTelefon:SCREEN-VALUE = ENTRY(6,cReturnValue,'|')
      KundeMobil:SCREEN-VALUE   = ENTRY(7,cReturnValue,'|')
    .
    cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Post','WHERE Post.Postnr = ' + QUOTER(postnr:SCREEN-VALUE),'Beskrivelse').
    IF cReturnValue NE '' AND cReturnValue NE ? THEN
      poststed:SCREEN-VALUE = cReturnValue.
  END.

/*   DYNAMIC-FUNCTION('setAttribute',hBrowse,'QueryFilter',' AND FALSE'). */
/*   RUN InvokeMethod (hBrowse,'OpenQuery').                              */

  DYNAMIC-FUNCTION('setBlankValue',FALSE,'Antall,ArtikkelNr,FeilKode,FeilkodeBeskrivelse,FeilNotat,ForsNr,LevFargKod,LevKod,Pris,RabKr,Storl,Varetekst').
  DYNAMIC-FUNCTION('setModified',FALSE,'ReklamStatus,RegistrertDato,SearchReklamNr,kundenr,kundenavn,kundeadresse,postnr,KundeE-Mail,KundeTelefon,Kundemobil,PostSted,sluttfortav,sluttfortdato').
  APPLY "entry" TO Kundenr.
END.
IF DYNAMIC-FUNCTION('getToolbarState',hToolbar-2) = 'new' THEN
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        LevKod:SENSITIVE     = FALSE
        Varetekst:SENSITIVE  = FALSE
        LevFargKod:SENSITIVE = FALSE
        TTID:SCREEN-VALUE    = '4'
        .  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRapport  AS CHAR NO-UNDO.
  DEF VAR pcRapListe AS CHAR NO-UNDO.
  DEF VAR cValg      AS CHAR NO-UNDO.
  
  ASSIGN
    pcRapListe = "Reklamasjon kunde,1," 
               + "Reklamasjon leverandør,2," 
               + "Tilgode lapp,3,"
               + "Reklamasjons liste,4"
  .

  RUN grapportvalg.w (INPUT pcRapListe).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ELSE 
  DO:
    ASSIGN 
      cValg = RETURN-VALUE
    .
    RUN skrivreklamasjonY.p (cValg,hFieldMap:BUFFER-FIELD('Reklamasjonsnr'):BUFFER-VALUE).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshByRowid C-Win 
PROCEDURE refreshByRowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cRowidList AS CHARACTER NO-UNDO. /*Reklamasjonslogg '|' ReklamasjonsLinje*/

  DYNAMIC-FUNCTION("setAttribute",hQuery,"QueryFilter",'WHERE ROWID(ReklamasjonsLogg) = TO-ROWID(' + QUOTER(ENTRY(1,cRowidList,'|')) + ')'). 
  RUN InvokeMethod(hQuery,"OpenQuery").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar-2 THEN
  DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE IN FRAME frame-A,'Butik,ButNamn').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
          MESSAGE "Butikk må registreres på reklamasjonslinjene."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO Butik.
          RETURN NO-APPLY.
      END.
  END.
  
  RUN SUPER.
  
  IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar-2 THEN
  DO:
      /* Her oppdateres sumfelt og leverandørnummer (Hvis det ikke er satt fra før). */
      bOk = DYNAMIC-FUNCTION("runProc","reklamasjonslogg_recalc.p",STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE),?).
      DYNAMIC-FUNCTION('DisplayFieldMap',hFieldMap).
      DYNAMIC-FUNCTION("RefreshRowids",hQuery,hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE).

      /* For oppfriskning av sumfelt og leverandørnr. */
      /* NB: Det burde være en bedre måte å gjøre dette på..... */
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Reklamasjonslogg','WHERE Reklamasjonslogg.Reklamasjonsnr = ' + STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE),'LevNr,ReklamVerdi,ReklamUtgifter,ReklamTotal').
      IF NOT (cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ?) THEN
          ASSIGN
            LevNr:SCREEN-VALUE          = ENTRY(1,cReturnValue,"|")
            ReklamVerdi:SCREEN-VALUE    = ENTRY(2,cReturnValue,"|")
            ReklamUtgifter:SCREEN-VALUE = ENTRY(3,cReturnValue,"|")
            ReklamTotal:SCREEN-VALUE    = ENTRY(4,cReturnValue,"|") 
            .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SMSRecord C-Win 
PROCEDURE SMSRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cMessage AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF KundeMobil:SCREEN-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Mobiltelefon er ikke utfylt","","").
    RETURN.
  END.
  cMessage = "Hei, din reklamasjon er ferdigbehandlet." + CHR(10) 
           +  "Vennlig hilsen " 
           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + Butik:SCREEN-VALUE,"ButNamn,BuAdr")
                    ,"|",", ").
  RUN SendSMS.w (KundeMobil:SCREEN-VALUE,
                 "Reklam.nr: " + ReklamasjonsNr:SCREEN-VALUE,
                 cMessage,
                 OUTPUT bOK,
                 OUTPUT cMessage).
  IF bOK THEN DO:
    AkseptertNotat:SCREEN-VALUE = cMessage + CHR(10) + AkseptertNotat:SCREEN-VALUE.
    RUN InvokeMethod(hFieldMap,"SaveRecord").
  END.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkoderegistreringRecord C-Win 
PROCEDURE StrekkoderegistreringRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cKode      AS CHAR NO-UNDO.
DEF VAR cStrKode   AS CHAR NO-UNDO.
DEF VAR cFeilKode  AS CHAR NO-UNDO.
DEF VAR cFeilNotat AS CHAR NO-UNDO.
DEF VAR cButik     AS CHAR NO-UNDO.
DEF VAR cbutNamn   AS CHAR NO-UNDO.
DEF VAR cTTId      AS CHAR NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

STREKKODELOOPEN:
DO WHILE TRUE WITH FRAME Frame-A:
    /* Sikrer at allt er lagret. */
    RUN InvokeMethod(hToolbar,"SaveRecord").

    IF int(ReklamasjonsNr:SCREEN-VALUE) = 0 THEN
    DO:
        MESSAGE "Reklamasjons må registreres før artikler kan legges inn."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF DEC(ForsNr:SCREEN-VALUE) = 0 THEN
    DO:
        MESSAGE "Kasserer må registreres før artikler kan legges inn."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    
    /* Henter butikknummer hvis det er føste linje. */
    IF int(Butik:SCREEN-VALUE) = 0 THEN 
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Forsalj','WHERE Forsalj.ForsNr = ' + "'" + ForsNr:SCREEN-VALUE  + "'",'ButikkNr').
      IF NOT (cReturnvalue = '0' OR cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ?) THEN
          Butik:SCREEN-VALUE = cReturnValue.
      ELSE
          Butik:SCREEN-VALUE = STRING(iCL).
    END.
    /* Setter default feilkode hvis feilkode ikke er angitt. */
    IF Feilkode:SCREEN-VALUE = ? OR int(Feilkode:SCREEN-VALUE) = 0 THEN 
    DO:
      ASSIGN
        Feilkode:SCREEN-VALUE            = DYNAMIC-FUNCTION("getFieldValues","Feilkode","WHERE FeilKode.FeilKode > '0'","Feilkode")
        FeilkodeBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Feilkode","WHERE FeilKode.FeilKode > '0'","Beskrivelse").
    END.
    
    RUN dReklamasjonStrekkode.w (INPUT Butik:SCREEN-VALUE, 
                                 INPUT (IF FeilKode:SCREEN-VALUE = ? THEN '' ELSE FeilKode:SCREEN-VALUE) + "|" + 
                                       (IF FeilNotat:SCREEN-VALUE = ? THEN '' ELSE FeilNotat:SCREEN-VALUE) + "|" + 
                                       (IF TTId:SCREEN-VALUE = ? THEN '' ELSE TTId:SCREEN-VALUE), 
                                 OUTPUT cReturnValue, 
                                 OUTPUT bOk).
    /* Avbryt eller ingen strekkode angitt. */
    IF bOk = FALSE OR ENTRY(1,cReturnValue,"|") = "" THEN
        LEAVE STREKKODELOOPEN.
    ELSE DO:
        /* Legger opp samme feilkode som ligger på den raden man starter fra. */
        ASSIGN
            cKode                            = ENTRY(1,cReturnValue,"|")
            cFeilKode                        = ENTRY(2,cReturnValue,"|")
            cFeilNotat                       = ENTRY(3,cReturnValue,"|")
            cButik                           = ENTRY(4,cReturnValue,"|")
            cButNamn                         = ENTRY(5,cReturnValue,"|")
            cTTId                            = ENTRY(6,cReturnValue,"|")
            .

        cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Strekkode','WHERE Strekkode.Kode = ' + "'" + cKode  + "'",'ArtikkelNr,StrKode').
        IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
        DO:
          MESSAGE "Ukjent strekkode " SKIP
                  "Strekkode:" cKode SKIP
                  "ReturnValue:" cReturnValue
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT STREKKODELOOPEN.
        END.
        ELSE DO:
            RUN InvokeMethod(hToolbar-2,"NewRecord").
            ASSIGN
                Butik:SCREEN-VALUE   = cButik 
                ButNamn:SCREEN-VALUE = cButNamn 
                .

            cStrKode = ENTRY(2,cReturnValue,'|').
            cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.ArtikkelNr = DEC(' + ENTRY(1,cReturnValue,'|') + ')','ArtikkelNr,LevKod,Beskr,LevFargKod,Vg,LopNr').
            IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
            DO:
                MESSAGE "Ukjent strekkode koblet til strekkode " + cKode + "." SKIP
                    "Kontakt systemansvarlig."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                NEXT STREKKODELOOPEN.
            END.
            ELSE
            DO:
              ASSIGN 
                ArtikkelNr:SCREEN-VALUE = ENTRY(1,cReturnValue,'|')
                LevKod:SCREEN-VALUE     = ENTRY(2,cReturnValue,'|')
                Varetekst:SCREEN-VALUE  = ENTRY(3,cReturnValue,'|')
                LevFargKod:SCREEN-VALUE = ENTRY(4,cReturnValue,'|')
                Vg:SCREEN-VALUE         = ENTRY(5,cReturnValue,'|')
                LopNr:SCREEN-VALUE      = ENTRY(6,cReturnValue,'|')
                TTId:SCREEN-VALUE       = cTTId
              .
              /* Legger opp varekost */
              cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Lager','WHERE Lager.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ') and Lager.Butik = ' + "'" + Butik:SCREEN-VALUE + "'",'ArtikkelNr,VVarekost').
              ASSIGN 
                VVarekost:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').

              cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtPris','WHERE ArtPris.ArtikkelNr = DEC(' + ENTRY(1,cReturnValue,'|') + ') and ProfilNr = ' + string(iProfilNr),'ArtikkelNr,Pris[1],VareKost[1]').
              ASSIGN 
                Pris:SCREEN-VALUE     = ENTRY(2,cReturnValue,'|').
              IF dec(VVarekost:SCREEN-VALUE) = 0 OR VVarekost:SCREEN-VALUE = ?
                  THEN VVarekost:SCREEN-VALUE = ENTRY(3,cReturnValue,'|').
            END.
            cReturnValue = DYNAMIC-FUNCTION('getFieldValues','StrKonv','WHERE StrKonv.StrKode = int(' + cStrKode + ')','Storl').
            IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
            DO:
              MESSAGE "Ukjent størrelse på strekkode "  + cKode + "." SKIP
                  "Kontakt systemansvarlig."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              NEXT STREKKODELOOPEN.
            END.
            ELSE
            DO:
              ASSIGN 
                Storl:SCREEN-VALUE     = ENTRY(1,cReturnValue,'|')
                Antall:SCREEN-VALUE    = '1'
                .
            END.
            
            IF INT(cFeilKode) <> 0 THEN DO:
              ASSIGN
                  FeilKode:SCREEN-VALUE            = cFeilKode
                  FeilNotat:SCREEN-VALUE           = cFeilNotat
                  FeilKodeBeskrivelse:SCREEN-VALUE = ""
                  .
              cReturnValue = DYNAMIC-FUNCTION('getFieldValues','FeilKode','WHERE FeilKode.FeilKode = ' + cFeilKode,'Beskrivelse').
              IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN. /* Gjør ingenting. */
              ELSE FeilKodeBeskrivelse:SCREEN-VALUE = cReturnValue.
              DYNAMIC-FUNCTION('setModified',TRUE,'FeilKode,FeilNotat').
              APPLY 'ANY-PRINTABLE' TO FeilKode.
            END.

            RUN InvokeMethod(hToolbar-2,"SaveRecord").
            /*
            IF DYNAMIC-FUNCTION("runproc","artbas_endre_varefakta.p",STRING(hFieldMapLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + "|" + cVareFakta,?) THEN
              DYNAMIC-FUNCTION("RefreshRowids",hBrowseLinje,hFieldMapLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
            ELSE 
              DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
            */
        END.
    END.
END. /* STREKKODELOOPEN */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VgLopeNrregistreringRecord C-Win 
PROCEDURE VgLopeNrregistreringRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cKode      AS CHAR NO-UNDO.
DEF VAR cStrKode   AS CHAR NO-UNDO.
DEF VAR cFeilKode  AS CHAR NO-UNDO.
DEF VAR cFeilNotat AS CHAR NO-UNDO.
DEF VAR cButik     AS CHAR NO-UNDO.
DEF VAR cbutNamn   AS CHAR NO-UNDO.
DEF VAR cVg        AS CHAR NO-UNDO.
DEF VAR cLopnr     AS CHAR NO-UNDO.
DEF VAR cStorl     AS CHAR NO-UNDO.
DEF VAR cAntall    AS CHAR NO-UNDO.
DEF VAR cTTId      AS CHAR NO-UNDO.

STREKKODELOOPEN:
DO WHILE TRUE WITH FRAME Frame-A:
    /* Sikrer at allt er lagret. */
    RUN InvokeMethod(hToolbar,"SaveRecord").

    IF int(ReklamasjonsNr:SCREEN-VALUE) = 0 THEN
    DO:
        MESSAGE "Reklamasjons må registreres før artikler kan legges inn."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF DEC(ForsNr:SCREEN-VALUE) = 0 THEN
    DO:
        MESSAGE "Kasserer må registreres før artikler kan legges inn."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    
    /* Henter butikknummer hvis det er føste linje. */
    IF int(Butik:SCREEN-VALUE) = 0 THEN 
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Forsalj','WHERE Forsalj.ForsNr = ' + "'" + ForsNr:SCREEN-VALUE  + "'",'ButikkNr').
      IF NOT (cReturnvalue = '0' OR cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ?) THEN
          Butik:SCREEN-VALUE = cReturnValue.
      ELSE
          Butik:SCREEN-VALUE = STRING(iCL).
    END.
    /* Setter default feilkode hvis feilkode ikke er angitt. */
    IF Feilkode:SCREEN-VALUE = ? OR int(Feilkode:SCREEN-VALUE) = 0 THEN 
    DO:
      ASSIGN
        Feilkode:SCREEN-VALUE            = DYNAMIC-FUNCTION("getFieldValues","Feilkode","WHERE FeilKode.FeilKode > '0'","Feilkode")
        FeilkodeBeskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Feilkode","WHERE FeilKode.FeilKode > '0'","Beskrivelse").
    END.

    RUN dReklamasjonVgLopnr.w (INPUT Butik:SCREEN-VALUE,
                               INPUT FeilKode:SCREEN-VALUE + "|" + 
                              (IF FeilNotat:SCREEN-VALUE = ? THEN '' ELSE FeilNotat:SCREEN-VALUE) + "|" + 
                              (IF TTId:SCREEN-VALUE = ? THEN '' ELSE TTId:SCREEN-VALUE), 
                               OUTPUT cReturnValue, 
                               OUTPUT bOk).
    /* Avbryt eller ingen strekkode angitt. */
    IF bOk = FALSE OR ENTRY(1,cReturnValue,"|") = "" THEN
        LEAVE STREKKODELOOPEN.
    ELSE DO:
        /* Legger opp samme feilkode som ligger på den raden man starter fra. */
        ASSIGN
            cVg                              = ENTRY(1,cReturnValue,"|")
            cLopnr                           = ENTRY(2,cReturnValue,"|")
            cStorl                           = ENTRY(3,cReturnValue,"|")
            cAntall                          = ENTRY(4,cReturnValue,"|")
            cFeilKode                        = ENTRY(5,cReturnValue,"|")
            cFeilNotat                       = ENTRY(6,cReturnValue,"|")
            cButik                           = ENTRY(7,cReturnValue,"|")
            cButNamn                         = ENTRY(8,cReturnValue,"|")
            cTTID                            = ENTRY(9,cReturnValue,"|")
            .

        cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Vg = ' + cVg + 
                                          ' and ArtBas.LopNr = ' + cLopNr,'Vg,LopNr,ArtikkelNr,LevKod,Beskr,LevFargKod').
        IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
        DO:
          MESSAGE "Ukjent varegruppe/løpenummer "
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT STREKKODELOOPEN.
        END.
        ELSE DO:
            RUN InvokeMethod(hToolbar-2,"NewRecord").
            ASSIGN
                ArtikkelNr:SCREEN-VALUE = ENTRY(3,cReturnValue,"|")
                LevKod:SCREEN-VALUE     = ENTRY(4,cReturnValue,"|")
                Varetekst:SCREEN-VALUE  = ENTRY(5,cReturnValue,"|")
                LevFargKod:SCREEN-VALUE = ENTRY(6,cReturnValue,"|")

                Vg:SCREEN-VALUE         = cVg
                Lopnr:SCREEN-VALUE      = cLopNr
                Storl:SCREEN-VALUE      = cStorl
                Antall:SCREEN-VALUE     = cAntall
                Butik:SCREEN-VALUE      = cButik 
                ButNamn:SCREEN-VALUE    = cButNamn 
                TTId:SCREEN-VALUE       = cTTId 
                .

            /* Legger opp varekost */
            cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Lager','WHERE Lager.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ') and Lager.Butik = ' + "'" + Butik:SCREEN-VALUE + "'",'ArtikkelNr,VVarekost').
            ASSIGN 
              VVarekost:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').

            cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtPris','WHERE ArtPris.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ') and ProfilNr = ' + string(iProfilNr),'ArtikkelNr,Pris[1],VareKost[1]').
            IF NOT (cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ?) THEN
                ASSIGN Pris:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').
            IF dec(VVarekost:SCREEN-VALUE) = 0 OR VVarekost:SCREEN-VALUE = ?
                THEN VVarekost:SCREEN-VALUE = ENTRY(3,cReturnValue,'|').
            IF INT(cFeilKode) <> 0 THEN DO:
              ASSIGN
                  FeilKode:SCREEN-VALUE            = cFeilKode
                  FeilNotat:SCREEN-VALUE           = cFeilNotat
                  FeilKodeBeskrivelse:SCREEN-VALUE = ""
                  .
              cReturnValue = DYNAMIC-FUNCTION('getFieldValues','FeilKode','WHERE FeilKode.FeilKode = ' + cFeilKode,'Beskrivelse').
              IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN. /* Gjør ingenting. */
              ELSE FeilKodeBeskrivelse:SCREEN-VALUE = cReturnValue.
              DYNAMIC-FUNCTION('setModified',TRUE,'FeilKode,FeilNotat').
              APPLY 'ANY-PRINTABLE' TO FeilKode.
            END.

            RUN InvokeMethod(hToolbar-2,"SaveRecord").
        END.
    END.
END. /* STREKKODELOOPEN */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR bReturnFocus  AS LOG   NO-UNDO.
  DEF VAR cStrList      AS CHAR  NO-UNDO.
  DEF VAR bOkStr        AS LOG   NO-UNDO.
  DEF VAR cReturnValue  AS CHAR  NO-UNDO.
  DEF VAR iArtNr        AS DEC   NO-UNDO.
  DEF VAR fArtikkelPris AS DEC   NO-UNDO.
  DEF VAR bCurrentFlag  AS LOG   NO-UNDO.

  iArtNr = DYNAMIC-FUNCTION('getFieldValues','reklamasjonslinje','WHERE Reklamasjonslinje.reklamasjonsnr = DEC(' + STRING(hFieldMap:BUFFER-FIELD('Reklamasjonsnr'):BUFFER-VALUE) + ')' 
                                                                + ' AND Reklamasjonslinje.artikkelnr = DEC(' + STRING(ifArtikkelNr) + ')','ArtikkelNr').
  IF cReturnValue NE '' AND cReturnValue NE ? THEN
  DO:
    MESSAGE 'allerede registrert !'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    LEAVE.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      cReturnValue= DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Artikkelnr = DEC(' + STRING(ifArtikkelNr) + ')','Beskr,levfargkod,levkod,strkode1,vg')
      ArtikkelNr:SCREEN-VALUE = STRING(ifArtikkelNr)
      varetekst:SCREEN-VALUE  = ENTRY(1,cReturnValue,'|')
      levfargKod:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
      levkod:SCREEN-VALUE     = ENTRY(3,cReturnValue,'|')
      Vg:SCREEN-VALUE         = ENTRY(5,cReturnValue,'|')
      storl:SCREEN-VALUE      = icStorl
      antall:SCREEN-VALUE     = IF ifPlukkAnt LE 0 THEN STRING(1) ELSE STRING(ifPlukkAnt)
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtPris','WHERE ArtPris.ArtikkelNr = DEC(' + STRING(ifArtikkelNr) + ')' 
                                                                 + ' AND ArtPris.ProfilNr   = ' + STRING(1) ,'pris;1,Rab1Kr;1')
      pris:SCREEN-VALUE       = ENTRY(1,cReturnValue,'|')
      RabKr:SCREEN-VALUE      = ENTRY(2,cReturnValue,'|')
    .
    
    cReturnValue= DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Artikkelnr = DEC(' + STRING(ifArtikkelNr) + ')','LopNr').
    ASSIGN
        LopNr:SCREEN-VALUE = ENTRY(1,cReturnValue,'|')
        .

    IF DYNAMIC-FUNCTION('getToolbarState',hToolbar-2) NE 'new' THEN 
      DYNAMIC-FUNCTION("setToolbar",hToolbar-2,"modified").
/*     DYNAMIC-FUNCTION('applyEvent',hQuery,'value-changed').                                                                        */

    RETURN bOk.    
  END. /*FRAME*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuery C-Win 
FUNCTION getQuery RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hQuery.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBlankValue C-Win 
FUNCTION setBlankValue RETURNS LOGICAL
  (INPUT bModified  AS LOG,
   INPUT cFieldList AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR hObject AS HANDLE NO-UNDO.
  ASSIGN 
    hObject = FRAME {&FRAME-NAME}:FIRST-CHILD
    hObject = hObject:FIRST-CHILD
  NO-ERROR.
  REPEAT WHILE VALID-HANDLE(hObject):
    IF CAN-DO(cFieldList,hObject:NAME) THEN
    DO:
      hObject:SCREEN-VALUE = ''.
    END.
    hObject = hObject:NEXT-SIBLING.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setModified C-Win 
FUNCTION setModified RETURNS LOGICAL
  (INPUT bModified  AS LOG,
   INPUT cFieldList AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR hObject AS HANDLE NO-UNDO.
  ASSIGN 
    hObject = FRAME {&FRAME-NAME}:FIRST-CHILD
    hObject = hObject:FIRST-CHILD
  NO-ERROR.
  REPEAT WHILE VALID-HANDLE(hObject):
    IF CAN-DO(cFieldList,hObject:NAME) THEN
    DO:
      hObject:MODIFIED = bModified.
    END.
    hObject = hObject:NEXT-SIBLING.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

