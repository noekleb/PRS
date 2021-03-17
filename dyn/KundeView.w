&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR iCL               AS INT  NO-UNDO.
                          
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hTbUpd            AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hParentQuery      AS HANDLE NO-UNDO.
DEF VAR hIE               AS HANDLE NO-UNDO.
DEF VAR hGenKundekort     AS HANDLE NO-UNDO.

/* DEF VAR hBrwRel           AS HANDLE NO-UNDO.  */
/* DEF VAR hFMrel            AS HANDLE NO-UNDO.  */
/* DEF VAR hTBrel            AS HANDLE NO-UNDO.  */
/* DEF VAR bNewPerson        AS LOG    NO-UNDO.  */
/*                                               */
/* DEF VAR fCurrPersonObj    AS DEC    NO-UNDO.  */
/* DEF VAR cRowIdentPerson   AS CHAR   NO-UNDO.  */
/* DEF VAR fCurrZipCodeObj   AS DEC    NO-UNDO.  */
/*                                               */
/* DEF VAR fCurrZipCodeObj2  AS DEC    NO-UNDO.  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Kunde.Avdeling 
&Scoped-define ENABLED-TABLES Kunde
&Scoped-define FIRST-ENABLED-TABLE Kunde
&Scoped-Define ENABLED-OBJECTS Momskod KundeNr OrgNr Navn Telefon MobilTlf ~
Telefaks ePostAdresse UrlFirma KontNavn KontTelefon KontMobilTlf ~
KontTelefaks PrivatTlf KontE-Post Adresse1 Adresse2 PostNr Beskrivelse Land ~
KortNr btnOrgnr ButikkNr ButNamn GruppeId TypeId Privat Aktiv ~
MottaeMailUtsendelser MvaFri BankKonto BankKode BankNavn ValKod ~
FaktAdresse1 FaktAdresse2 btnTlf FaktPostNr FaktLand LevAdresse1 ~
LevAdresse2 LevPostNr LevLand DeresRef EksterntKundeNr Beskrivelse3 ~
Beskrivelse4 btnButikk Etablert Opphort Kilde TilgKilde BetBet Fakturagebyr ~
Purregebyr SamleFaktura btnPostnr TotalRabatt% BetType MaksKredit ~
KreditSperret FodtDato Alder Kjon Region ByNavn BydelsNr WebKunde ~
WebKanSetteOrdre WebKanSendeEMail btnKundekort KundeSaldo ForsteKjop ~
SisteKjop Hovedkunde KobletTilKunde Navn7 btnFaktPostNr btnLevPostnr ~
btnKundeDetSok btnValuta rectTBupd RectSaldo 
&Scoped-Define DISPLAYED-FIELDS Kunde.Avdeling 
&Scoped-define DISPLAYED-TABLES Kunde
&Scoped-define FIRST-DISPLAYED-TABLE Kunde
&Scoped-Define DISPLAYED-OBJECTS Momskod KundeNr FILL-IN-1 OrgNr Navn ~
Telefon MobilTlf Telefaks ePostAdresse UrlFirma KontNavn KontTelefon ~
KontMobilTlf KontTelefaks PrivatTlf KontE-Post Adresse1 Adresse2 PostNr ~
Beskrivelse Land KortNr ButikkNr ButNamn GruppeId TypeId Privat Aktiv ~
MottaeMailUtsendelser MvaFri BankKonto BankKode BankNavn ValKod ~
FaktAdresse1 FaktAdresse2 FaktPostNr FaktLand LevAdresse1 LevAdresse2 ~
LevPostNr LevLand DeresRef EksterntKundeNr Beskrivelse3 Beskrivelse4 ~
Etablert Opphort Kilde TilgKilde BetBet Fakturagebyr Purregebyr ~
SamleFaktura TotalRabatt% BetType MaksKredit KreditSperret FodtDato Alder ~
Kjon Region ByNavn BydelsNr WebKunde WebKanSetteOrdre WebKanSendeEMail ~
KundeSaldo ForsteKjop SisteKjop Hovedkunde KobletTilKunde Navn7 ~
FI-Kontaktperson FI-Kontoradresse FI-Fakturaadresse FI-BetInfo ~
FI-Leveringsadresse FI-BetBet FI-Kredithandtering FI-Demografiskedata ~
FI-Nettbutikk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKobletTilKunde C-Win 
FUNCTION setKobletTilKunde RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButikk 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnFaktPostNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnKundeDetSok 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnKundekort 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnLevPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnOrgnr 
     IMAGE-UP FILE "gif/afinternet.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "www" 
     SIZE 5 BY 1
     FONT 2.

DEFINE BUTTON btnPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnTlf 
     IMAGE-UP FILE "gif/afinternet.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "www" 
     SIZE 5 BY 1
     FONT 2.

DEFINE BUTTON btnValuta 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE BetBet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bet.bet" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE GruppeId AS CHARACTER FORMAT "X(256)":U 
     LABEL "K.gr" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE Kjon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kjønn" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Ukjent","0",
                     "Mann","1",
                     "Kvinne","2"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE TypeId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE Adresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Adresse2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Alder AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Alder" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE BankKode AS CHARACTER FORMAT "X(30)" 
     LABEL "Bankkode" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE BankKonto AS CHARACTER FORMAT "X(20)" 
     LABEL "Bank-kto" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE BankNavn AS CHARACTER FORMAT "X(30)" 
     LABEL "Banknavn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE Beskrivelse3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18.6 BY 1 NO-UNDO.

DEFINE VARIABLE Beskrivelse4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18.6 BY 1 NO-UNDO.

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Butikk som har opprettet kunden".

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 25.4 BY 1.

DEFINE VARIABLE BydelsNr AS CHARACTER FORMAT "X(8)" 
     LABEL "Bydel" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE ByNavn AS CHARACTER FORMAT "X(30)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE DeresRef AS CHARACTER FORMAT "X(30)" 
     LABEL "Deres referanse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE EksterntKundeNr AS CHARACTER FORMAT "X(20)" 
     LABEL "Ekst.kundenr" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE ePostAdresse AS CHARACTER FORMAT "X(40)" 
     LABEL "E-Post" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE Etablert AS DATE FORMAT "99/99/99" 
     LABEL "Etabl./opphørt" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE FaktAdresse1 AS CHARACTER FORMAT "X(30)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktAdresse2 AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktLand AS CHARACTER FORMAT "X(30)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktPostNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE FI-BetBet AS CHARACTER FORMAT "X(256)":U INITIAL "Betalingsbetingelser" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-BetInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Betalingsinformasjon" 
      VIEW-AS TEXT 
     SIZE 30 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Demografiskedata AS CHARACTER FORMAT "X(256)":U INITIAL "Demografiske data" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Fakturaadresse AS CHARACTER FORMAT "X(256)":U INITIAL "Fakturadresse" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kontaktperson AS CHARACTER FORMAT "X(256)":U INITIAL "Kontaktperson" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kontoradresse AS CHARACTER FORMAT "X(256)":U INITIAL "Kontoradresse" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kredithandtering AS CHARACTER FORMAT "X(256)":U INITIAL "Kredithåndtering" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Leveringsadresse AS CHARACTER FORMAT "X(256)":U INITIAL "Leveringsadresse" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Nettbutikk AS CHARACTER FORMAT "X(256)":U INITIAL "Nettbutikk" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "(for kundeordre)" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 NO-UNDO.

DEFINE VARIABLE FodtDato AS DATE FORMAT "99/99/99" 
     LABEL "Født" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ForsteKjop AS DATE FORMAT "99/99/99" 
     LABEL "Første kjøp" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Kilde AS CHARACTER FORMAT "X(30)" 
     LABEL "Kilde" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE KobletTilKunde AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1.

DEFINE VARIABLE KontE-Post AS CHARACTER FORMAT "X(40)" 
     LABEL "E-Post" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE KontMobilTlf AS CHARACTER FORMAT "X(16)" 
     LABEL "Mob" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE KontNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE KontTelefaks AS CHARACTER FORMAT "X(15)" 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE KontTelefon AS CHARACTER FORMAT "X(16)" 
     LABEL "Tlf" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE KortNr AS CHARACTER FORMAT "X(22)" 
     LABEL "Kundekort" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE KundeSaldo AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Saldo" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE Land AS CHARACTER FORMAT "X(30)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE LevAdresse1 AS CHARACTER FORMAT "X(30)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE LevAdresse2 AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE LevLand AS CHARACTER FORMAT "X(30)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE LevPostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE MaksKredit AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Kred.gr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(16)" 
     LABEL "Mob" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE Navn7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.8 BY 1 NO-UNDO.

DEFINE VARIABLE Opphort AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE OrgNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Orgnr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE PrivatTlf AS CHARACTER FORMAT "X(15)" 
     LABEL "Priv.tlf" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE Region AS CHARACTER FORMAT "X(30)" 
     LABEL "Region" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE SisteKjop AS DATE FORMAT "99/99/99" 
     LABEL "Siste kjøp" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Telefaks AS CHARACTER FORMAT "X(16)" 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(16)" 
     LABEL "Tlf" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE TilgKilde AS CHARACTER FORMAT "X(30)" 
     LABEL "Tilg.kilde" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE TotalRabatt% AS DECIMAL FORMAT "->9.9" INITIAL 0 
     LABEL "Ordrerabatt%" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE UrlFirma AS CHARACTER FORMAT "X(40)" 
     LABEL "URL" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE ValKod AS CHARACTER FORMAT "x(3)" 
     LABEL "Valuta" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1.

DEFINE VARIABLE BetType AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Kontant", 1,
"Kreditt", 2
     SIZE 23 BY .95 NO-UNDO.

DEFINE VARIABLE Momskod AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mva", 0,
"Mva fri", 1
     SIZE 32 BY .91 NO-UNDO.

DEFINE RECTANGLE RectSaldo
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 1.52.

DEFINE RECTANGLE rectTBupd
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE Aktiv AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.8 BY .81 NO-UNDO.

DEFINE VARIABLE Fakturagebyr AS LOGICAL INITIAL yes 
     LABEL "Fakturagebyr" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE Hovedkunde AS LOGICAL INITIAL no 
     LABEL "Hovedkunde" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE KreditSperret AS LOGICAL INITIAL no 
     LABEL "Kredittsperret" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.2 BY .81 NO-UNDO.

DEFINE VARIABLE MottaeMailUtsendelser AS LOGICAL INITIAL no 
     LABEL "Utsendelser" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.2 BY .81 TOOLTIP "Vil motta eMail utsendelser" NO-UNDO.

DEFINE VARIABLE MvaFri AS LOGICAL INITIAL no 
     LABEL "Mva fri" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.8 BY .81 NO-UNDO.

DEFINE VARIABLE Privat AS LOGICAL INITIAL no 
     LABEL "Privat" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE Purregebyr AS LOGICAL INITIAL yes 
     LABEL "Purregebyr" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE SamleFaktura AS LOGICAL INITIAL no 
     LABEL "Samlefaktura" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.4 BY .81 NO-UNDO.

DEFINE VARIABLE WebKanSendeEMail AS LOGICAL INITIAL no 
     LABEL "Kan sende eMail" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81.

DEFINE VARIABLE WebKanSetteOrdre AS LOGICAL INITIAL no 
     LABEL "Kan sette ordre" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81.

DEFINE VARIABLE WebKunde AS LOGICAL INITIAL no 
     LABEL "Kunde" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Momskod AT ROW 11.1 COL 69 HELP
          "Mva kode" NO-LABEL
     KundeNr AT ROW 2.52 COL 13 COLON-ALIGNED HELP
          "Kundenummer"
     FILL-IN-1 AT ROW 9.57 COL 125 COLON-ALIGNED NO-LABEL
     OrgNr AT ROW 2.52 COL 38 COLON-ALIGNED HELP
          "Kundens organisasjonsnummer"
     Navn AT ROW 3.48 COL 13 COLON-ALIGNED HELP
          "Navn eller firmanavn"
     Telefon AT ROW 5.33 COL 13 COLON-ALIGNED HELP
          "Telefon"
     MobilTlf AT ROW 5.29 COL 38 COLON-ALIGNED HELP
          "Mobiltelefon"
     Telefaks AT ROW 6.29 COL 13 COLON-ALIGNED HELP
          "Telefaks"
     ePostAdresse AT ROW 7.24 COL 13 COLON-ALIGNED HELP
          "E-Post adresse"
     Kunde.Avdeling AT ROW 8.19 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     UrlFirma AT ROW 9.14 COL 13 COLON-ALIGNED
     KontNavn AT ROW 12.67 COL 13 COLON-ALIGNED HELP
          "Navn på kontaktperson"
     KontTelefon AT ROW 13.62 COL 13 COLON-ALIGNED HELP
          "Telefon direkte til kontaktperson"
     KontMobilTlf AT ROW 13.62 COL 38 COLON-ALIGNED HELP
          "Kontaktpersonens mobiltelefonnummer"
     KontTelefaks AT ROW 14.57 COL 13 COLON-ALIGNED HELP
          "Kontaktpersonens telefaksnummer"
     PrivatTlf AT ROW 14.62 COL 38 COLON-ALIGNED HELP
          "Privattelefonnummer"
     KontE-Post AT ROW 15.57 COL 13 COLON-ALIGNED HELP
          "E-Post adresse"
     Adresse1 AT ROW 17.24 COL 13 COLON-ALIGNED HELP
          "Kundens adresse"
     Adresse2 AT ROW 18.19 COL 13 COLON-ALIGNED HELP
          "Kundens adresse" NO-LABEL
     PostNr AT ROW 19.14 COL 13 COLON-ALIGNED HELP
          "Postnummer"
     Beskrivelse AT ROW 19.14 COL 32.6 COLON-ALIGNED NO-LABEL
     Land AT ROW 20.1 COL 13 COLON-ALIGNED HELP
          "Land"
     KortNr AT ROW 21.71 COL 13 COLON-ALIGNED HELP
          "Kortnummer"
     btnOrgnr AT ROW 2.52 COL 57
     ButikkNr AT ROW 22.71 COL 13 COLON-ALIGNED HELP
          "Butikk som rekrutterte kunden"
     ButNamn AT ROW 22.71 COL 26.8 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     GruppeId AT ROW 2.52 COL 67 COLON-ALIGNED
     TypeId AT ROW 3.57 COL 67 COLON-ALIGNED
     Privat AT ROW 4.67 COL 68.8
     Aktiv AT ROW 4.67 COL 85.2
     MottaeMailUtsendelser AT ROW 5.52 COL 68.8 HELP
          "Ønsker å motta utsendelser på mail" WIDGET-ID 2
     MvaFri AT ROW 5.52 COL 85.2 HELP
          "Kunden skal ikke faktureres mva" WIDGET-ID 4
     BankKonto AT ROW 7.24 COL 66.8 COLON-ALIGNED HELP
          "Bankkontonummer"
     BankKode AT ROW 8.19 COL 66.8 COLON-ALIGNED
     BankNavn AT ROW 9.1 COL 66.8 COLON-ALIGNED
     ValKod AT ROW 10.05 COL 66.8 COLON-ALIGNED
     FaktAdresse1 AT ROW 12.67 COL 66.8 COLON-ALIGNED HELP
          "Fakturaadresse"
     FaktAdresse2 AT ROW 13.62 COL 66.8 COLON-ALIGNED HELP
          "Fakturaadresse" NO-LABEL
     btnTlf AT ROW 5.29 COL 62
     FaktPostNr AT ROW 14.57 COL 66.8 COLON-ALIGNED HELP
          "Postnr. fakturaadresse."
     FaktLand AT ROW 15.52 COL 66.8 COLON-ALIGNED HELP
          "Land"
     LevAdresse1 AT ROW 17.24 COL 66.8 COLON-ALIGNED HELP
          "Kundens leveringsadresse"
     LevAdresse2 AT ROW 18.19 COL 66.8 COLON-ALIGNED HELP
          "Kundens leveringsadresse" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SCROLLABLE SIZE 142 BY 25.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     LevPostNr AT ROW 19.14 COL 66.8 COLON-ALIGNED HELP
          "Leveringsadressesn postnummer"
     LevLand AT ROW 20.1 COL 66.8 COLON-ALIGNED HELP
          "Land"
     DeresRef AT ROW 21.71 COL 66.8 COLON-ALIGNED
     EksterntKundeNr AT ROW 22.71 COL 66.8 COLON-ALIGNED HELP
          "Eksternt kundenummer (Fra f.eks fakturasystem)"
     Beskrivelse3 AT ROW 14.57 COL 87.8 COLON-ALIGNED NO-LABEL
     Beskrivelse4 AT ROW 19.14 COL 87.8 COLON-ALIGNED NO-LABEL
     btnButikk AT ROW 22.71 COL 25 NO-TAB-STOP 
     Etablert AT ROW 2.52 COL 114 COLON-ALIGNED HELP
          "Opprette dato"
     Opphort AT ROW 2.52 COL 126.8 COLON-ALIGNED HELP
          "Kunden er meldt ut av kunderegisteret" NO-LABEL
     Kilde AT ROW 3.52 COL 114 COLON-ALIGNED HELP
          "Hvor kommer kunden fra."
     TilgKilde AT ROW 4.48 COL 114 COLON-ALIGNED HELP
          "Hvilken tilknyttning har kunden."
     BetBet AT ROW 6.19 COL 114 COLON-ALIGNED
     Fakturagebyr AT ROW 7.14 COL 116 HELP
          "Inn/utkobling av fakturagebyr på kunde."
     Purregebyr AT ROW 7.86 COL 116 HELP
          "Inn/utkobling av purregebyr for kunden."
     SamleFaktura AT ROW 8.57 COL 116
     btnPostnr AT ROW 19.14 COL 30.6 NO-TAB-STOP 
     TotalRabatt% AT ROW 9.33 COL 114 COLON-ALIGNED
     BetType AT ROW 11.1 COL 116 NO-LABEL
     MaksKredit AT ROW 12.05 COL 114 COLON-ALIGNED HELP
          "Kreditgrense"
     KreditSperret AT ROW 13.19 COL 116
     FodtDato AT ROW 14.86 COL 114 COLON-ALIGNED HELP
          "Fødselsdato"
     Alder AT ROW 15.81 COL 114 COLON-ALIGNED HELP
          "Alder"
     Kjon AT ROW 16.76 COL 114 COLON-ALIGNED
     Region AT ROW 17.71 COL 114 COLON-ALIGNED
     ByNavn AT ROW 18.67 COL 114 COLON-ALIGNED
     BydelsNr AT ROW 19.62 COL 114 COLON-ALIGNED
     WebKunde AT ROW 21.71 COL 116
     WebKanSetteOrdre AT ROW 22.43 COL 116
     WebKanSendeEMail AT ROW 23.14 COL 116
     btnKundekort AT ROW 21.71 COL 39 NO-TAB-STOP 
     KundeSaldo AT ROW 24.57 COL 6.6 COLON-ALIGNED HELP
          "Kundens utestående saldo"
     ForsteKjop AT ROW 24.57 COL 38.6 COLON-ALIGNED
     SisteKjop AT ROW 24.52 COL 62.6 COLON-ALIGNED HELP
          "Siste gang kunden kjøpte"
     Hovedkunde AT ROW 24.62 COL 78.6
     KobletTilKunde AT ROW 24.52 COL 93 COLON-ALIGNED HELP
          "Kundenummer" NO-LABEL
     Navn7 AT ROW 24.52 COL 111.8 COLON-ALIGNED NO-LABEL
     btnFaktPostNr AT ROW 14.52 COL 85.8 NO-TAB-STOP 
     btnLevPostnr AT ROW 19.14 COL 85.8 NO-TAB-STOP 
     btnKundeDetSok AT ROW 24.52 COL 109.2 NO-TAB-STOP 
     FI-Kontaktperson AT ROW 12.05 COL 13.4 COLON-ALIGNED NO-LABEL
     FI-Kontoradresse AT ROW 16.62 COL 13.8 COLON-ALIGNED NO-LABEL
     FI-Fakturaadresse AT ROW 12 COL 67.4 COLON-ALIGNED NO-LABEL
     FI-BetInfo AT ROW 6.48 COL 67.8 COLON-ALIGNED NO-LABEL
     FI-Leveringsadresse AT ROW 16.62 COL 67.8 COLON-ALIGNED NO-LABEL
     FI-BetBet AT ROW 5.52 COL 114.8 COLON-ALIGNED NO-LABEL
     FI-Kredithandtering AT ROW 10.52 COL 114.6 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SCROLLABLE SIZE 142 BY 25.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     FI-Demografiskedata AT ROW 14.14 COL 114 COLON-ALIGNED NO-LABEL
     FI-Nettbutikk AT ROW 21 COL 114 COLON-ALIGNED NO-LABEL
     btnValuta AT ROW 10.05 COL 77.4 NO-TAB-STOP 
     rectTBupd AT ROW 1.24 COL 1.8
     RectSaldo AT ROW 24.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SCROLLABLE SIZE 142 BY 25.1.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kunde"
         HEIGHT             = 25.1
         WIDTH              = 142
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 25.1
       FRAME DEFAULT-FRAME:WIDTH            = 142.

/* SETTINGS FOR FILL-IN FI-BetBet IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BetInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Demografiskedata IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Fakturaadresse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kontaktperson IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kontoradresse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kredithandtering IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Leveringsadresse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Nettbutikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kunde */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kunde */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikk C-Win
ON CHOOSE OF btnButikk IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF ButikkNr DO:
  DEF VAR cButikerFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "Butiker"
                     + ";Butik"
                     + ";ButNamn"
                     ,
                   "WHERE true"
                    ,""
                    ,"Butik,ButNamn",
                    OUTPUT cButikerFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cButikerFieldList NE "" THEN DO:
    ASSIGN 
       ButikkNr:SCREEN-VALUE      = ENTRY(1,cButikerFieldList,"|")
       ButNamn:SCREEN-VALUE       = ENTRY(2,cButikerFieldList,"|")
       .
    APPLY "any-printable" TO Postnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFaktPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFaktPostNr C-Win
ON CHOOSE OF btnFaktPostNr IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF FaktPostNr DO:
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Post"
                     + ";Beskrivelse"
                     + ";Postnr"
                     ,
                   "WHERE false"
                    ,""
                    ,"Beskrivelse,Postnr",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       Beskrivelse3:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       FaktPostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO FaktPostnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundeDetSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundeDetSok C-Win
ON CHOOSE OF btnKundeDetSok IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cKundeValueList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Kunde;Kundenr;Navn"
                    ,"WHERE true"
                    ,""
                    ,"KundeNr,Navn"
                    ,OUTPUT cKundeValueList
                    ,OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cKundeValueList NE "" THEN DO:
    ASSIGN KobletTilKunde:SCREEN-VALUE = ENTRY(1,cKundeValueList,"|")
           Navn7:SCREEN-VALUE = ENTRY(2,cKundeValueList,"|").
    APPLY "any-printable" TO KobletTilKunde.
  END.
  APPLY "entry" TO KobletTilKunde.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundekort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundekort C-Win
ON CHOOSE OF btnKundekort IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF Kortnr DO:
  RUN d-bkundekort.w (0,DEC(KundeNr:SCREEN-VALUE),"V").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevPostnr C-Win
ON CHOOSE OF btnLevPostnr IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF LevPostNr DO:
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Post"
                     + ";Beskrivelse"
                     + ";Postnr"
                     ,
                   "WHERE true"
                    ,""
                    ,"Beskrivelse,Postnr",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       Beskrivelse4:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       LevPostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO LevPostnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOrgnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrgnr C-Win
ON CHOOSE OF btnOrgnr IN FRAME DEFAULT-FRAME /* www */
DO:
  RUN StartIE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Post"
                     + ";Beskrivelse"
                     + ";Postnr"
                     ,
                   "WHERE false"
                    ,""
                    ,"Beskrivelse,Postnr",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       Beskrivelse:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       PostNr:SCREEN-VALUE        = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO Postnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTlf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTlf C-Win
ON CHOOSE OF btnTlf IN FRAME DEFAULT-FRAME /* www */
DO:
  RUN StartIE("tlf").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnValuta C-Win
ON CHOOSE OF btnValuta IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF ValKod DO:
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Valuta"
                     + ";ValKod"
                     + ";ValKurs"
                     + ";ValLand"
                     ,
                   "WHERE true"
                    ,""
                    ,"ValKod,ValKurs",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       ValKod:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO ValKod.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON F10 OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  APPLY "choose" TO btnButikk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON RETURN OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
OR TAB OF ButikkNr DO:
  IF ButikkNr:MODIFIED THEN
    ButNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = '" + ButikkNr:SCREEN-VALUE + "'","ButNamn").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FaktPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FaktPostNr C-Win
ON F10 OF FaktPostNr IN FRAME DEFAULT-FRAME /* Postnr */
DO:
  APPLY "choose" TO btnFaktPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FaktPostNr C-Win
ON RETURN OF FaktPostNr IN FRAME DEFAULT-FRAME /* Postnr */
OR TAB OF FaktPostNr DO:
  IF FaktPostNr:MODIFIED THEN
    Beskrivelse3:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + FaktPostNr:SCREEN-VALUE + "'","Beskrivelse").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KobletTilKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KobletTilKunde C-Win
ON F10 OF KobletTilKunde IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO btnKundeDetSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevPostNr C-Win
ON F10 OF LevPostNr IN FRAME DEFAULT-FRAME /* PostNr */
DO:
  APPLY "choose" TO btnLevPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevPostNr C-Win
ON RETURN OF LevPostNr IN FRAME DEFAULT-FRAME /* PostNr */
OR TAB OF LevPostNr DO:
  IF LevPostNr:MODIFIED THEN
    Beskrivelse4:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + LevPostNr:SCREEN-VALUE + "'","Beskrivelse").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON F10 OF PostNr IN FRAME DEFAULT-FRAME /* PostNr */
DO:
  APPLY "choose" TO btnPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON RETURN OF PostNr IN FRAME DEFAULT-FRAME /* PostNr */
OR TAB OF PostNr DO:
  IF PostNr:MODIFIED THEN
    Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'","Beskrivelse").
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
  PUBLISH "InvalidateHandle".
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* Rydder opp hvos programmet er aktivt. */
IF VALID-HANDLE(hGenKundekort) THEN
    DELETE PROCEDURE hGenKundekort.

{incl/supptrigg.i hFieldMap}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "AltSKundeOrdre" (THIS-PROCEDURE:FILE-NAME).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DEF VAR cBrukerTypeBut AS CHAR  NO-UNDO.

  ASSIGN cBrukerTypeBut = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType,ButikkNr").

  RUN SUPER.
  RUN ValueChangedField("BetType").
  RUN ValueChangedField("Privat").
  setKobletTilKunde().

  DO WITH FRAME DEFAULT-FRAME:
      /*
      ASSIGN
        ButikkNr:SENSITIVE = (IF ENTRY(1,cBrukerTypeBut,'|') = '2'
                                THEN FALSE 
                                ELSE TRUE)        
        btnButikk:SENSITIVE = (IF ENTRY(1,cBrukerTypeBut,'|') = '2'
                                THEN FALSE 
                                ELSE TRUE)
        .
      */
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
  DISPLAY Momskod KundeNr FILL-IN-1 OrgNr Navn Telefon MobilTlf Telefaks 
          ePostAdresse UrlFirma KontNavn KontTelefon KontMobilTlf KontTelefaks 
          PrivatTlf KontE-Post Adresse1 Adresse2 PostNr Beskrivelse Land KortNr 
          ButikkNr ButNamn GruppeId TypeId Privat Aktiv MottaeMailUtsendelser 
          MvaFri BankKonto BankKode BankNavn ValKod FaktAdresse1 FaktAdresse2 
          FaktPostNr FaktLand LevAdresse1 LevAdresse2 LevPostNr LevLand DeresRef 
          EksterntKundeNr Beskrivelse3 Beskrivelse4 Etablert Opphort Kilde 
          TilgKilde BetBet Fakturagebyr Purregebyr SamleFaktura TotalRabatt% 
          BetType MaksKredit KreditSperret FodtDato Alder Kjon Region ByNavn 
          BydelsNr WebKunde WebKanSetteOrdre WebKanSendeEMail KundeSaldo 
          ForsteKjop SisteKjop Hovedkunde KobletTilKunde Navn7 FI-Kontaktperson 
          FI-Kontoradresse FI-Fakturaadresse FI-BetInfo FI-Leveringsadresse 
          FI-BetBet FI-Kredithandtering FI-Demografiskedata FI-Nettbutikk 
      WITH FRAME DEFAULT-FRAME.
  IF AVAILABLE Kunde THEN 
    DISPLAY Kunde.Avdeling 
      WITH FRAME DEFAULT-FRAME.
  ENABLE Momskod KundeNr OrgNr Navn Telefon MobilTlf Telefaks ePostAdresse 
         Kunde.Avdeling UrlFirma KontNavn KontTelefon KontMobilTlf KontTelefaks 
         PrivatTlf KontE-Post Adresse1 Adresse2 PostNr Beskrivelse Land KortNr 
         btnOrgnr ButikkNr ButNamn GruppeId TypeId Privat Aktiv 
         MottaeMailUtsendelser MvaFri BankKonto BankKode BankNavn ValKod 
         FaktAdresse1 FaktAdresse2 btnTlf FaktPostNr FaktLand LevAdresse1 
         LevAdresse2 LevPostNr LevLand DeresRef EksterntKundeNr Beskrivelse3 
         Beskrivelse4 btnButikk Etablert Opphort Kilde TilgKilde BetBet 
         Fakturagebyr Purregebyr SamleFaktura btnPostnr TotalRabatt% BetType 
         MaksKredit KreditSperret FodtDato Alder Kjon Region ByNavn BydelsNr 
         WebKunde WebKanSetteOrdre WebKanSendeEMail btnKundekort KundeSaldo 
         ForsteKjop SisteKjop Hovedkunde KobletTilKunde Navn7 btnFaktPostNr 
         btnLevPostnr btnKundeDetSok btnValuta rectTBupd RectSaldo 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraDeleteRecord C-Win 
PROCEDURE ExtraDeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obOk AS LOG NO-UNDO INIT TRUE.

DYNAMIC-FUNCTION("DoCommit",TRUE).
RUN AddUpdRecord IN hParent ("delete",hFieldMap).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLookupAttributes C-Win 
PROCEDURE getLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  ihBrowse AS HANDLE NO-UNDO.
DEF OUTPUT PARAM oiReturn AS INT NO-UNDO.

/* IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "pemPerson" THEN                                   */
/*   cRowIdentPerson = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  {syspara.i 5 1 1 iCl INT}

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,""
                           ,"Kunde"
                          + ",Post;Beskrivelse,buf1_Post;Beskrivelse,buf2_Post;Beskrivelse"
                          + ",Kundekort;KortNr"
                          + ",Butiker;ButNamn"
                          + ",buf3_kunde;Navn"
                          + ",Valuta;ValKod"
                          + ",Moms;MomsKod"
                           ,"WHERE FALSE"
                           + ",FIRST Post OF Kunde NO-LOCK OUTER-JOIN"
                           + ",FIRST buf1_Post WHERE buf1_Post.Postnr = Kunde.FaktPostNr NO-LOCK OUTER-JOIN"
                           + ",FIRST buf2_Post WHERE buf2_Post.Postnr = Kunde.LevPostNr NO-LOCK OUTER-JOIN"
                           + ",FIRST Kundekort OF Kunde NO-LOCK OUTER-JOIN"
                           + ",FIRST Butiker WHERE Butiker.Butik = Kunde.ButikkNr NO-LOCK OUTER-JOIN"
                           + ",FIRST buf3_Kunde WHERE buf3_Kunde.KundeNr = Kunde.KobletTilKunde NO-LOCK OUTER-JOIN"
                           + ",FIRST Valuta of Kunde NO-LOCK OUTER-JOIN"
                           + ",FIRST Moms of Kunde NO-LOCK OUTER-JOIN"
                           ,"").
/*   IF VALID-HANDLE(hParentQuery) THEN                                     */
/*     DYNAMIC-FUNCTION("CreateParentLink",hQuery,hParentQuery,"KundeNr").  */

  ASSIGN GruppeId:DELIMITER = "|"
         GruppeId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                             "KundeGruppe;Beskrivelse;GruppeId",
                                                             "WHERE true")
         BetBet:DELIMITER = "|"
         BetBet:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                            "Betalingsbetingelser;BetTekst;BetBet",
                                                            "WHERE true")
         TypeId:DELIMITER = "|"
         TypeId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                           "KundeType;Beskrivelse;TypeId",
                                                           "WHERE true")
         .

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME {&FRAME-NAME}:HANDLE,
                             "OrgNr,GruppeId,Etablert,TypeId,Navn,FodtDato,Adresse1,Kjon,Alder,SamleFaktura,MaksKredit"
                             + ",Adresse2,BetBet,PostNr,BetType,Land,BankKonto,KreditSperret,Opphort"
                             + ",Telefon,MobilTlf,Telefaks,PrivatTlf,FaktAdresse1,ePostAdresse,FaktAdresse2,FaktPostNr,FaktLand"
                             + ",KontNavn,KontTelefon,KontMobilTlf,LevAdresse1,KontE-Post,LevAdresse2,KontTelefaks,LevPostNr,LevLand"
                             + ",Privat,ButikkNr,Fakturagebyr,Purregebyr,Aktiv,MvaFri,Hovedkunde,KobletTilKunde,Kilde,TilgKilde,EksterntKundeNr"
                             + ",UrlFirma,BankKode,BankNavn,ValKod,MomsKod,Region,ByNavn,bydelsNr,WebKunde,WebKanSetteOrdre,WebKanSendeeMail,Totalrabatt%,MottaeMailUtsendelser",
                             "",
                             "KundeNr,Beskrivelse,Beskrivelse3,Beskrivelse4,KortNr,KundeSaldo,ForsteKjop,SisteKjop,ButNamn,Navn7","",
                             "btnOrgnr,btnPostnr,btnTlf,btnFaktPostnr,btnLevPostnr,btnButikk").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"recordavailwidgets",STRING(btnKundekort:HANDLE)).  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"fieldnamedeletewarning","Navn").  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc",'=kunde_delete.p').
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,THIS-PROCEDURE).

  hTBupd = DYNAMIC-FUNCTION("NewToolbar",
                            rectTBupd:HANDLE,
                            "",
                            "New;Ny,Undo;Angre,Save,Delete;Slett,Medlemskort",
                            "maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hTBupd).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hTBupd).

END.
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

DEF VAR cValueList    AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN 'PostNr' THEN DO:
  
      cValueList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'"
                                           ,"Beskrivelse,PostNr").
      IF cValueList NE ? THEN
        ASSIGN 
           Beskrivelse:SCREEN-VALUE = ENTRY(1,cValueList,"|")
           PostNr:SCREEN-VALUE      = ENTRY(2,cValueList,"|")
           .
      ELSE
        ASSIGN 
           Beskrivelse:SCREEN-VALUE   = ""
           .
  
    END.
    WHEN 'FaktPostNr' THEN DO:
      cValueList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + FaktPostNr:SCREEN-VALUE + "'"
                                           ,"Beskrivelse,PostNr").
      IF cValueList NE ? THEN
        ASSIGN 
           Beskrivelse3:SCREEN-VALUE   = ENTRY(1,cValueList,"|")
           FaktPostNr:SCREEN-VALUE     = ENTRY(2,cValueList,"|")
           .
      ELSE
        ASSIGN 
           Beskrivelse3:SCREEN-VALUE   = ""
           .
    END.
    WHEN 'LevPostNr' THEN DO:
      cValueList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + LevPostNr:SCREEN-VALUE + "'"
                                           ,"Beskrivelse,PostNr").
      IF cValueList NE ? THEN
        ASSIGN 
           Beskrivelse4:SCREEN-VALUE   = ENTRY(1,cValueList,"|")
           LevPostNr:SCREEN-VALUE     = ENTRY(2,cValueList,"|")
           .
      ELSE
        ASSIGN 
           Beskrivelse4:SCREEN-VALUE   = ""
           .
    END.
    WHEN 'FodtDato' THEN DO:
      ASSIGN FodtDato.
      Alder:SCREEN-VALUE = STRING(YEAR(TODAY) - YEAR(FodtDato) + 
                           IF INT(STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")) LT INT(STRING(YEAR(TODAY)) + STRING(MONTH(FodtDato),"99") + STRING(DAY(FodtDato),"99")) THEN
                             -1
                           ELSE 0).
    END.
    WHEN "KobletTilKunde" THEN DO:
      cValueList = DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE KundeNr = '" + KobletTilKunde:SCREEN-VALUE + "'"
                                           ,"KundeNr,Navn").
      IF cValueList NE ? THEN
        ASSIGN KobletTilKunde:SCREEN-VALUE = ENTRY(1,cValueList,"|")
               Navn7:SCREEN-VALUE = ENTRY(2,cValueList,"|").
    END.
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MedlemskortRecord C-Win 
PROCEDURE MedlemskortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME default-frame:
    IF NOT VALID-HANDLE(hGenKundekort) THEN
        RUN VALUE("wgenkundekort.w") PERSISTENT SET hGenKundekort.
    RUN setKunde IN hGenKundeKort (dec(KundeNr:SCREEN-VALUE)).
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO OrgNr IN FRAME {&FRAME-NAME}.

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
  DEF VAR cBrukerTypeBut AS CHAR  NO-UNDO.

  ASSIGN cBrukerTypeBut = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType,ButikkNr").

  RUN SUPER.

  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
        aktiv:CHECKED = TRUE          
        GruppeId:SCREEN-VALUE = "1"
        Makskredit:SCREEN-VALUE = "10000"
        KreditSperret:CHECKED = FALSE
        ButikkNr:SCREEN-VALUE = (IF ENTRY(1,cBrukerTypeBut,'|') = '2'
                                   THEN entry(2,cBrukerTypeBut,'|')
                                   ELSE STRING(iCL))
        /*
        ButikkNr:SENSITIVE = (IF ENTRY(1,cBrukerTypeBut,'|') = '2'
                                THEN FALSE 
                                ELSE TRUE)        
        btnButikk:SENSITIVE = (IF ENTRY(1,cBrukerTypeBut,'|') = '2'
                                THEN FALSE 
                                ELSE TRUE)
        */                        
        .

  END.


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
DEF VAR cTbState    AS CHAR NO-UNDO.
DEF VAR cParentLink AS CHAR NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR icParam     AS CHAR NO-UNDO.

  
DO WITH FRAME DEFAULT-FRAME:
    IF int(ButikkNr:screen-value) = 0 THEN
    DO:
        MESSAGE "Kunden er ikke koblet til butikk." SKIP
                "Butikknummer må angis."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

cTbState = DYNAMIC-FUNCTION("getToolbarState",hTBupd).

IF cTbState = "new" THEN DO:
  cParentLink = DYNAMIC-FUNCTION("getAttribute",hQuery,"parentlinkfields").  
  DYNAMIC-FUNCTION("setAttribute",hQuery,"parentlinkfields","").
END.

RUN SUPER.

IF cParentLink NE "" THEN
  DYNAMIC-FUNCTION("setAttribute",hQuery,"parentlinkfields",cParentLink).


DO WITH FRAME DEFAULT-FRAME:
    icParam = KundeNr:SCREEN-VALUE.
    /* Oppretter medlem */
    IF cTbState = "new" THEN
        /*RUN genMedlem.*/
        DYNAMIC-FUNCTION("runproc","kunde_gen_medlem.p",icParam,?).
END.

IF CAN-DO(hParent:INTERNAL-ENTRIES,"AddUpdRecord") THEN RUN AddUpdRecord IN hParent (cTbState,hFieldMap).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

IF CAN-DO("Post",ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME) THEN
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"basequery","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartIE C-Win 
PROCEDURE StartIE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icTarget AS CHAR NO-UNDO.

DEF VAR cURL         AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  cURL = "http://" + 
         (IF icTarget = "tlf" THEN
            "www.gulesider.no/gsi/" +
           (IF Privat:CHECKED THEN
              "whiteSearch.do?mode=A&linje=1&sort=2&spraak=1" +
              "&etter=" + ENTRY(NUM-ENTRIES(Navn:SCREEN-VALUE," "),Navn:SCREEN-VALUE," ") +
              "&for=" + ENTRY(1,Navn:SCREEN-VALUE," ") +
              "&gate=" +
              "&pn=" + (IF PostNr:SCREEN-VALUE NE "" THEN PostNr:SCREEN-VALUE ELSE "0") +
              "&ps=&kommune=&fylke=00&tel=&private=on&mobile=on&profile=on&x=0&y=0"
            ELSE
              "search.do?sort=1&spraak=1&mode=A&linje=1&bransje=" +
              "&firma=" + Navn:SCREEN-VALUE +
              "&fylke=00&kommune=&by=&bydel=&gate=" +
              "&pn=" + PostNr:SCREEN-VALUE +
              "&ps=&weekDay=&openFrom=&openTo=&cardName=&tel=")
          ELSE 
            "w2.brreg.no/enhet/sok/treffliste.jsp?navn=" + 
            Navn:SCREEN-VALUE + 
            "&orgform=0&fylke=0&kommune=" +
            "0"
/*             (IF AVAIL postreg THEN STRING(postreg.Kom-Nr) ELSE "0") */
            ).
  
  
  IF NOT VALID-HANDLE(hIE) THEN
    RUN ie.w PERSIST SET hIE ("","",23,25,800,600).
/*   RUN ie.w PERSIST SET hIE ("","",23,25,SESSION:WIDTH-PIXELS - 30,SESSION:HEIGHT-PIXELS - 100). */
  RUN UpdateWindow IN hIE (cURL,Navn:SCREEN-VALUE).
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN 'Privat' THEN DO:
  
      IF Privat:CHECKED THEN
        ASSIGN 
           Etablert:SENSITIVE = FALSE
           Opphort:SENSITIVE  = FALSE
           FodtDato:SENSITIVE = TRUE
           Kjon:SENSITIVE     = TRUE
           Alder:SENSITIVE    = TRUE
           .
      ELSE
        ASSIGN 
           Etablert:SENSITIVE = TRUE
           Opphort:SENSITIVE  = TRUE
           FodtDato:SENSITIVE = FALSE
           Kjon:SENSITIVE     = FALSE
           Alder:SENSITIVE    = FALSE
           .
    END.
    WHEN 'BetType' THEN DO:
      ASSIGN BetType.
      IF BetType = 1 THEN
        MaksKredit:SENSITIVE = FALSE.
      ELSE
        MaksKredit:SENSITIVE = TRUE.
    END.
    WHEN "Hovedkunde" THEN DO:
      setKobletTilKunde().
      IF Hovedkunde:CHECKED THEN
        ASSIGN KobletTilKunde:SCREEN-VALUE = "0"
               Navn7:SCREEN-VALUE = "".
    END.
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectTBupd,rectTBrel,GuestAddressName,RectSaldo").
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"PersonAddressName,GuestAddressName").
DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"ZipCodeId,ZipCodeName,btnZipCode").
DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
  "btnButikk,btnKundekort,ButikkNr,ButNamn,EksterntKundeNr,FI-Nettbutikk,KortNr,LevLand,WebKanSendeEMail,WebKanSetteOrdre,WebKunde,DeresRef").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKobletTilKunde C-Win 
FUNCTION setKobletTilKunde RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF Hovedkunde:CHECKED IN FRAME {&FRAME-NAME} THEN
  ASSIGN KobletTilKunde:SENSITIVE = NO
         Hovedkunde:LABEL = TRIM(Hovedkunde:LABEL,":").
ELSE
  ASSIGN KobletTilKunde:SENSITIVE = YES
         Hovedkunde:LABEL = TRIM(Hovedkunde:LABEL,":") + ":".

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

