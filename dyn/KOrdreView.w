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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hParentQuery      AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT    NO-UNDO.
DEF VAR hAdresseProc      AS HANDLE NO-UNDO.
DEF VAR hKundeMerknadProc AS HANDLE NO-UNDO.
DEF VAR hLinjeToolbar     AS HANDLE NO-UNDO.
DEF VAR bOpprettFaktura   AS LOG    NO-UNDO.
DEF VAR cTekst            AS CHAR   NO-UNDO.
DEF VAR cOpphav           AS CHAR   NO-UNDO.
DEF VAR cReturLst         AS CHAR   NO-UNDO.

DEF VAR cKundeFields      AS CHAR NO-UNDO
    INIT "Navn,Adresse1,Adresse2,PostNr,TotalRabatt%,BetBet,ePostAdresse,Telefon,Telefaks,KontNavn,FaktAdresse1,FaktAdresse2,FaktPostNr,FaktLand,LevAdresse1,LevAdresse2,LevPostNr,LevLand,MobilTlf".

DEF VAR cOldTotalRabatt%  AS CHAR NO-UNDO.
DEF VAR iKasseModKOrdre   AS INT  NO-UNDO.
DEF VAR iNettButikk       AS INT  NO-UNDO.
DEF VAR lKOrdre_Id        LIKE KordreHode.KOrdre_Id NO-UNDO.
DEFINE VARIABLE iCL AS INTEGER     NO-UNDO.
DEF VAR cLokTekst AS CHAR NO-UNDO.
DEF VAR cLokRowIdList AS CHAR NO-UNDO.
DEF VAR cLokIdList AS CHAR NO-UNDO.

DEFINE VARIABLE cNettButikkType AS CHARACTER   NO-UNDO.

DEF BUFFER bufKOrdreLinje FOR KOrdreLinje.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnLeveringsDato Mvafri RegistrertAv KasseNr ~
KOrdre_Id ButikkNr KundeNr Navn DeresRef VaarRef Referanse LeveringsDato ~
Utsendelsesdato TotalRabatt% BetBet LevFNr SendingsNr ReturNr LevStatus ~
EkstOrdreNr btnKundeDetSok FakturertDato ProdStatus AnsvVerksted ~
ProduksjonsDato VerkstedMerknad RegistrertDato ForsNr SelgerNr ~
Verkstedordre btnProduksjonsDato rectTBordreHode rectFolder rectVerksted 
&Scoped-Define DISPLAYED-OBJECTS ShipmentSendt AntApnet AntPPEti Mvafri ~
fiInternMerknad Opphav fiKundeMerknad RegistrertAv KasseNr KOrdre_Id ~
ButikkNr KundeNr Navn DeresRef VaarRef Referanse LeveringsDato ~
Utsendelsesdato TotalRabatt% BetBet LevFNr SendingsNr ReturNr LevStatus ~
EkstOrdreNr FakturertDato ProdStatus AnsvVerksted ProduksjonsDato ~
VerkstedMerknad RegistrertDato ForsNr SelgerNr Verkstedordre ~
fiGaveInnpakning 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndreKundeMva C-Win 
FUNCTION EndreKundeMva RETURNS LOGICAL
  ( INPUT icNyttKundenr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldMap C-Win 
FUNCTION getFieldMap RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastTabWidget C-Win 
FUNCTION getLastTabWidget RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrdreLinjeQuery C-Win 
FUNCTION getOrdreLinjeQuery RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAdresseProc C-Win 
FUNCTION setAdresseProc RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKasse C-Win 
FUNCTION setKasse RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKunde C-Win 
FUNCTION setKunde RETURNS LOGICAL
  ( INPUT icKunde AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLinjeToolbar C-Win 
FUNCTION setLinjeToolbar RETURNS LOGICAL
  ( INPUT ihLinjeToolbar AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMinKasse C-Win 
FUNCTION setMinKasse RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnKundeDetSok 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLeveringsDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnProduksjonsDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE AnsvVerksted AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ansv.verksted" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 28.2 BY 1 NO-UNDO.

DEFINE VARIABLE BetBet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bet.bet" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 28.2 BY 1 NO-UNDO.

DEFINE VARIABLE ButikkNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE ForsNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasserer" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE KasseNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE LevFNr AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Leverings måte" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 28.2 BY 1 NO-UNDO.

DEFINE VARIABLE LevStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordrestatus" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE Opphav AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Opphav" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "<Udefiner>",1,
                     "Manuelt reg.",1,
                     "Kasse",5,
                     "Nettbutikk",10
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE ProdStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Verkstedstatus" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 28.2 BY 1 NO-UNDO.

DEFINE VARIABLE SelgerNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Selger" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE VerkstedMerknad AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 41 BY 1.62 TOOLTIP "DblKlikk for å åpne dalog" NO-UNDO.

DEFINE VARIABLE AntApnet AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Ant.PkSdl" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 5 BY 1 TOOLTIP "Antall ganger pakkseddel er srevet ut." NO-UNDO.

DEFINE VARIABLE AntPPEti AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Ant.PPEti" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 5 BY 1 TOOLTIP "Antall ganger postpakke etikett er skrevet ut" NO-UNDO.

DEFINE VARIABLE DeresRef AS CHARACTER FORMAT "X(30)" 
     LABEL "Deres ref" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE EkstOrdreNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Ekst.ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE FakturertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Fakturert dato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fiGaveInnpakning AS CHARACTER FORMAT "X(256)":U INITIAL "GAVEINNPAKKNING" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fiInternMerknad AS CHARACTER FORMAT "X(256)":U INITIAL "Internmerknad" 
      VIEW-AS TEXT 
     SIZE 18.2 BY .62
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fiKundeMerknad AS CHARACTER FORMAT "X(256)":U INITIAL "Kundemerknad" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1.

DEFINE VARIABLE LeveringsDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Ø. lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Ønsket leveringsdato" NO-UNDO.

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.4 BY 1 NO-UNDO.

DEFINE VARIABLE ProduksjonsDato AS DATE FORMAT "99/99/9999" 
     LABEL "Ferdigdato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE Referanse AS CHARACTER FORMAT "X(50)" 
     LABEL "Referanse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Evt. bestillingsnummer".

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Dato for (foreløpig) siste leveranse fra ordre".

DEFINE VARIABLE ReturNr AS CHARACTER FORMAT "X(30)" 
     LABEL "Returnr." 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1.

DEFINE VARIABLE SendingsNr AS CHARACTER FORMAT "X(30)" 
     LABEL "Sendingsnr." 
     VIEW-AS FILL-IN 
     SIZE 26.8 BY 1.

DEFINE VARIABLE ShipmentSendt AS DATETIME FORMAT "99/99/99 HH:MM:SS":U INITIAL ? 
     LABEL "Shipment sendt" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE TotalRabatt% AS DECIMAL FORMAT "->9.9" INITIAL 0 
     LABEL "O.rab%" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE Utsendelsesdato AS DATE FORMAT "99/99/9999" 
     LABEL "Levert dato" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Dato for (foreløpig) siste leveranse fra ordre".

DEFINE VARIABLE VaarRef AS CHARACTER FORMAT "X(30)" 
     LABEL "Vår ref" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 13.1.

DEFINE RECTANGLE rectTBordreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE rectVerksted
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 3.67.

DEFINE VARIABLE Mvafri AS LOGICAL INITIAL NO 
     LABEL "Mva fri" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE Verkstedordre AS LOGICAL INITIAL NO 
     LABEL "Verkstedordre" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnLeveringsDato AT ROW 2.57 COL 90
     ShipmentSendt AT ROW 4.57 COL 71 COLON-ALIGNED WIDGET-ID 10
     AntApnet AT ROW 9.62 COL 136.6 COLON-ALIGNED HELP
          "Antall ganger pakkseddel er srevet ut." WIDGET-ID 8
     AntPPEti AT ROW 9.62 COL 115.2 COLON-ALIGNED HELP
          "Antall ganger postpakke etikett er skrevet ut" WIDGET-ID 6
     Mvafri AT ROW 2.67 COL 31.8
     fiInternMerknad AT ROW 7.76 COL 29.6 COLON-ALIGNED NO-LABEL
     Opphav AT ROW 3.67 COL 115 COLON-ALIGNED HELP
          "Ordre opphav. F.eks. 1-Manuell reg. 2-Nettbutikk."
     fiKundeMerknad AT ROW 7.76 COL 11 COLON-ALIGNED NO-LABEL
     RegistrertAv AT ROW 5.62 COL 132.2 COLON-ALIGNED NO-LABEL
     KasseNr AT ROW 4.62 COL 10.8 COLON-ALIGNED
     KOrdre_Id AT ROW 2.57 COL 11.2 COLON-ALIGNED HELP
          "Internt faktura id. Tildeles autmatisk av systemet."
     ButikkNr AT ROW 3.62 COL 10.8 COLON-ALIGNED HELP
          "Butikk"
     KundeNr AT ROW 8.52 COL 10.8 COLON-ALIGNED HELP
          "Kundenummer"
     Navn AT ROW 8.52 COL 29.4 COLON-ALIGNED NO-LABEL
     DeresRef AT ROW 9.48 COL 10.8 COLON-ALIGNED HELP
          "Deres referanse"
     VaarRef AT ROW 10.43 COL 10.8 COLON-ALIGNED HELP
          "Vår referanse"
     Referanse AT ROW 11.38 COL 10.8 COLON-ALIGNED HELP
          "Referanse til kundens ordre id."
     LeveringsDato AT ROW 2.57 COL 71 COLON-ALIGNED HELP
          "Ønsket leveringsdato"
     Utsendelsesdato AT ROW 3.57 COL 71 COLON-ALIGNED HELP
          "Utsendelsesdato"
     TotalRabatt% AT ROW 2.57 COL 50.6 COLON-ALIGNED HELP
          "Subtotalrabatt på kundeordre/bong."
     BetBet AT ROW 5.62 COL 71 COLON-ALIGNED
     LevFNr AT ROW 6.67 COL 71 COLON-ALIGNED
     SendingsNr AT ROW 7.62 COL 115 COLON-ALIGNED HELP
          "Sendingsnummer"
     ReturNr AT ROW 8.62 COL 115 COLON-ALIGNED HELP
          "Sendingsnummer" WIDGET-ID 4
     LevStatus AT ROW 2.62 COL 115 COLON-ALIGNED
     EkstOrdreNr AT ROW 4.62 COL 115 COLON-ALIGNED HELP
          "Ordrenummer fra eksternt system (Importert ordre)"
     btnKundeDetSok AT ROW 8.52 COL 27 NO-TAB-STOP 
     FakturertDato AT ROW 6.62 COL 115 COLON-ALIGNED HELP
          "Fakturert dato"
     ProdStatus AT ROW 9 COL 70.8 COLON-ALIGNED
     AnsvVerksted AT ROW 10 COL 70.8 COLON-ALIGNED
     ProduksjonsDato AT ROW 11 COL 70.8 COLON-ALIGNED HELP
          "Utsendelsesdato"
     VerkstedMerknad AT ROW 10.76 COL 102.8 HELP
          "DblKlikk for å åpne dialog." NO-LABEL
     RegistrertDato AT ROW 5.62 COL 115 COLON-ALIGNED HELP
          "Utsendelsesdato"
     ForsNr AT ROW 5.62 COL 10.8 COLON-ALIGNED
     SelgerNr AT ROW 6.62 COL 10.8 COLON-ALIGNED
     Verkstedordre AT ROW 8.14 COL 58 NO-TAB-STOP 
     btnProduksjonsDato AT ROW 11 COL 90.2
     fiGaveInnpakning AT ROW 7.81 COL 73.6 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     rectTBordreHode AT ROW 1.24 COL 1.8
     rectFolder AT ROW 12.67 COL 2
     rectVerksted AT ROW 8.71 COL 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 143.6 BY 24.95.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 24.95
         WIDTH              = 143.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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
       FRAME DEFAULT-FRAME:HEIGHT           = 24.95
       FRAME DEFAULT-FRAME:WIDTH            = 143.6.

/* SETTINGS FOR FILL-IN AntApnet IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       AntApnet:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN AntPPEti IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       AntPPEti:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiGaveInnpakning IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiInternMerknad IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiKundeMerknad IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Opphav IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ShipmentSendt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ShipmentSendt:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundeDetSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundeDetSok C-Win
ON CHOOSE OF btnKundeDetSok IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cKundeFieldList    AS CHAR NO-UNDO.
  DEF VAR cKundeValueList    AS CHAR NO-UNDO.
  DEF VAR cDisplay           AS CHAR NO-UNDO.
  DEF VAR cNoDisplay         AS CHAR NO-UNDO.

  ASSIGN cKundeFieldList = "KundeNr," + cKundeFields
         cDisplay = "Navn,Kundenr,Adresse1,Postnr,ePostAdresse,Telefon,MobilTlf,KontNavn".

  DO ix = 1 TO NUM-ENTRIES(cKundeFields):
    IF NOT CAN-DO(cDisplay,ENTRY(ix,cKundeFields)) THEN
      cNoDisplay = cNoDisplay + ";!" + ENTRY(ix,cKundeFields).
  END.
  cDisplay = REPLACE(cDisplay,",",";").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Kunde;" + cDisplay + cNoDisplay
                    ,"WHERE true"
                    ,""
                    ,cKundeFieldList,
                    OUTPUT cKundeValueList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cKundeValueList NE "" AND EndreKundeMva(ENTRY(1,cKundeValueList,"|")) THEN DO:
    KundeNr:SCREEN-VALUE      = ENTRY(1,cKundeValueList,"|").
    setKunde(SUBSTR(cKundeValueList,INDEX(cKundeValueList,"|") + 1)).
    APPLY "any-printable" TO KundeNr.
    APPLY "entry" TO DeresRef.
  END.
  ELSE APPLY "entry" TO Kundenr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLeveringsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLeveringsDato C-Win
ON CHOOSE OF btnLeveringsDato IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (LeveringsDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProduksjonsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProduksjonsDato C-Win
ON CHOOSE OF btnProduksjonsDato IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (ProduksjonsDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON BACK-TAB OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  DYNAMIC-FUNCTION("setFocus" IN hParent).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeNr C-Win
ON F10 OF KundeNr IN FRAME DEFAULT-FRAME /* Kundenr */
DO:
  APPLY "choose" TO btnKundeDetSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ProdStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProdStatus C-Win
ON VALUE-CHANGED OF ProdStatus IN FRAME DEFAULT-FRAME /* Verkstedstatus */
DO:
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ProduksjonsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProduksjonsDato C-Win
ON TAB OF ProduksjonsDato IN FRAME DEFAULT-FRAME /* Ferdigdato */
DO:
/*   IF iCurrTab = 2 THEN DO: */
    RUN MoveToTop IN hCurrTabProc.
    RETURN NO-APPLY.
/*   END. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VerkstedMerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VerkstedMerknad C-Win
ON MOUSE-SELECT-DBLCLICK OF VerkstedMerknad IN FRAME DEFAULT-FRAME
DO:
    RUN NoteRecord.  
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

{incl/supptrigg.i hFieldMap}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF hCurrTabProc:FILE-NAME = "KOrdreLinje.w" THEN
    PUBLISH "AltSKundeOrdre" ("kundeordre").
END.

/* ON 'alt-l':U OF FRAME {&FRAME-NAME} ANYWHERE DO:               */
/*   DEF VAR bNew AS LOG NO-UNDO.                                 */
/*                                                                */
/*   bNew = DYNAMIC-FUNCTION("getToolbarState",hToolbar) = "new". */
/*   DYNAMIC-FUNCTION("setCurrentObject",hToolbar).               */
/*   RUN SaveRecord.                                              */
/* END.                                                           */
/*                                                                */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BekreftRecord C-Win 
PROCEDURE BekreftRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DECIMAL(KundeNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Kunde må registreres før ordre kan bekreftes","","").
  RETURN.
END.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Skriv ut ordrebekreftelse?","","") = 1 THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","kordre_bekreft.p",STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    RUN skrivkundeordre.p (STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + '|FULL',
                           NO,"",1,"",DYNAMIC-FUNCTION("getTransactionMessage")).
    APPLY "value-changed" TO hParentQuery.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

setMinKasse(STRING(hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)).

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",KOrdre_id:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
EkstOrdreNr:SCREEN-VALUE = ''.

RUN SaveRecord.

setMinKasse(STRING(hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord C-Win 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.



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
IF hFieldMap:AVAIL THEN DO WITH FRAME DEFAULT-FRAME:
/*   IF iCurrTab = 1 THEN  */
    PUBLISH "setEnableEditOrderLine" (INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) < 50).

  IF INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) > 30 THEN 
  DO:
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents",
        (IF INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) < 60 THEN ",Delete" ELSE "")
      + (IF INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) > 30 THEN ",Bekreft" ELSE "")
      + (IF INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) > 40 THEN ",Lever" ELSE "")
      + (IF INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) <> 50 THEN ",Retur" ELSE "")
      + (IF hFieldMap:BUFFER-FIELD("SendingsNr"):BUFFER-VALUE = "RETUR" THEN ",Retur" ELSE "")
                     ).
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"ReadOnlyFields",DYNAMIC-FUNCTION("getAttribute",hFieldMap,"ScreenUpdateFields")).
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"DisabledFields",DYNAMIC-FUNCTION("getAttribute",hFieldMap,"ExtraUpdateFields")).
    IF INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) > 40 THEN
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteReadOnly","yes").
    ELSE
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteReadOnly","").
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteReadOnly","").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"ReadOnlyFields","").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"DisabledFields","").
  END.

  setKasse(STRING(hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)).

  DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteCurrentValue",hFieldMap:BUFFER-FIELD("VerkstedMerknad"):BUFFER-VALUE).
  
  IF STRING(hFieldMap:BUFFER-FIELD("InternMerknad"):BUFFER-VALUE) = '' 
    THEN fiInternMerknad:HIDDEN = TRUE.
    ELSE fiInternMerknad:HIDDEN = FALSE. 
  IF STRING(hFieldMap:BUFFER-FIELD("KundeMerknad"):BUFFER-VALUE) = '' 
    THEN fiKundeMerknad:HIDDEN = TRUE.
    ELSE fiKundeMerknad:HIDDEN = FALSE. 
  IF STRING(hFieldMap:BUFFER-FIELD("cOpt1"):BUFFER-VALUE) = '' 
    THEN fiGaveInnpakning:HIDDEN = TRUE.
    ELSE fiGaveInnpakning:HIDDEN = FALSE. 

  IF STRING(hFieldMap:BUFFER-FIELD("KundeMerknad"):BUFFER-VALUE) = '' 
    THEN DYNAMIC-FUNCTION("setTabImage" IN hTabFolder,3,"accep16d.bmp") NO-ERROR.
    ELSE DYNAMIC-FUNCTION("setTabImage" IN hTabFolder,3,"redtick.bmp") NO-ERROR.
  IF STRING(hFieldMap:BUFFER-FIELD("InternMerknad"):BUFFER-VALUE) = '' 
    THEN DYNAMIC-FUNCTION("setTabImage" IN hTabFolder,4,"accep16d.bmp") NO-ERROR.
    ELSE DYNAMIC-FUNCTION("setTabImage" IN hTabFolder,4,"redtick.bmp") NO-ERROR.

END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteCurrentValue","").
  fiInternMerknad:HIDDEN = TRUE.
  fiKundeMerknad:HIDDEN  = TRUE.
  fiGaveinnpakning:HIDDEN = TRUE.
END.

IF VALID-HANDLE(hLinjeToolbar) THEN
  DYNAMIC-FUNCTION("setAttribute",hLinjeToolbar,"enabledevents","hentart").

DYNAMIC-FUNCTION("setCurrentObject",hQuery).
RUN SUPER.

IF hFieldMap:AVAIL AND INT(hFieldMap:BUFFER-FIELD("LevStatus"):BUFFER-VALUE) < 50 THEN
  RUN ValueChangedField ("Verkstedordre").
  
cOldTotalRabatt% = TotalRabatt%:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

ButikkNr:MODIFIED = NO.

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
DEF VAR cMessage   AS CHAR NO-UNDO.
DEF VAR cKundeInfo AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("getKundeInfo" IN hAdresseProc,OUTPUT cKundeInfo).

DO WITH FRAME {&FRAME-NAME}:
  IF ENTRY(7,cKundeInfo,"|") = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Epost adresse er ikke utfylt","","").
    RETURN.
  END.
  cMessage = "Hei, din ordre er klar for henting." + CHR(10) 
           +  "Vennlig hilsen " 
           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + ButikkNr:SCREEN-VALUE,"ButNamn,BuAdr")
                    ,"|",", ").
  RUN SendEmail.w (ENTRY(7,cKundeInfo,"|"),
                 "Ordrenr: " + KOrdre_Id:SCREEN-VALUE,
                 cMessage,
                 OUTPUT bOK,
                 OUTPUT cMessage).
  IF bOK THEN DO:
    VerkstedMerknad:SCREEN-VALUE = cMessage + CHR(10) + VerkstedMerknad:SCREEN-VALUE.
  END.
  

  RUN InvokeMethod(hFieldMap,"SaveRecord").
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
  DISPLAY ShipmentSendt AntApnet AntPPEti Mvafri fiInternMerknad Opphav 
          fiKundeMerknad RegistrertAv KasseNr KOrdre_Id ButikkNr KundeNr Navn 
          DeresRef VaarRef Referanse LeveringsDato Utsendelsesdato TotalRabatt% 
          BetBet LevFNr SendingsNr ReturNr LevStatus EkstOrdreNr FakturertDato 
          ProdStatus AnsvVerksted ProduksjonsDato VerkstedMerknad RegistrertDato 
          ForsNr SelgerNr Verkstedordre fiGaveInnpakning 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnLeveringsDato Mvafri RegistrertAv KasseNr KOrdre_Id ButikkNr 
         KundeNr Navn DeresRef VaarRef Referanse LeveringsDato Utsendelsesdato 
         TotalRabatt% BetBet LevFNr SendingsNr ReturNr LevStatus EkstOrdreNr 
         btnKundeDetSok FakturertDato ProdStatus AnsvVerksted ProduksjonsDato 
         VerkstedMerknad RegistrertDato ForsNr SelgerNr Verkstedordre 
         btnProduksjonsDato rectTBordreHode rectFolder rectVerksted 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtiketterRecord C-Win 
PROCEDURE EtiketterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hFieldMap:AVAIL THEN
DO:
    RUN d-SkrivKOrdreEtikett.w (hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE).

END.
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
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.
DEF VAR hTabFrame   AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  cNettButikkType = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20","Parameter1")).
  iNettButikk = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 2","Parameter1")).

  cReturLst = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 19 and SysGr = 9 and ParaNr = 12","Parameter1")).
  IF cReturLst = '' OR cTekst = '0' THEN cReturLst = ''.
  ELSE cReturLst = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                      "WHERE SysHId = 19 and SysGr = 9 and ParaNr = 12","Parameter2")).
  ASSIGN 
         LevStatus:DELIMITER = "|"
         LevStatus:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 19 and SysGr = 1")

         Opphav:DELIMITER = "|"
         Opphav:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 150 and SysGr = 2")

         ProdStatus:DELIMITER = "|"
         ProdStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 19 and SysGr = 2")

         ButikkNr:DELIMITER = "|"
         ButikkNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE TRUE BY Butik")
        
         KasseNr:DELIMITER = "|"
         ForsNr:DELIMITER = "|"
         SelgerNr:DELIMITER = "|"
    
         BetBet:DELIMITER = "|"
         BetBet:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                            "Betalingsbetingelser;BetTekst;BetBet",
                                                            "WHERE true")
         LevFNr:DELIMITER = "|"
         LevFNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                            "Leveringsform;LevFormBeskrivelse;LevFNr",
                                                            "WHERE true")
         AnsvVerksted:DELIMITER = "|"
         AnsvVerksted:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("GetFieldList","Bruker;Navn;BrukerId","WHERE TRUE BY Navn")        

         iKasseModKOrdre = 20
         .
/*   iKasseModKOrdre = INT(DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 1 and SysGr = 10")) NO-ERROR. */

  /* Create the browse: */
   hQuery = DYNAMIC-FUNCTION("NewQuery",
                           1,
                           "",
                           "KOrdreHode"
                         + ",Kunde"
                           + ";Mvafri"
                          ,"WHERE false"
                         + ",FIRST kunde NO-LOCK OF KOrdreHode OUTER-JOIN"
                          ,"").         
  DYNAMIC-FUNCTION("createObjectLink",hQuery,THIS-PROCEDURE).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                           hQuery,
                           FRAME {&FRAME-NAME}:HANDLE,
                           "ButikkNr,KasseNr,ForsNr,SelgerNr,DeresRef,BetBet,LevFNr,KundeNr,AnsvVerksted,LeveringsDato,Navn,ProdStatus,ProduksjonsDato,Referanse,TotalRabatt%,Verkstedordre,VaarRef,SendingsNr,ReturNr,EkstOrdreNr,VerkstedMerknad","",
                           "KOrdre_Id,FakturertDato,LevStatus,Utsendelsesdato,RegistrertDato,RegistrertAv,Opphav,Mvafri,AntPPEti,AntApnet,ShipmentSendt","",
                           "btnLeveringsDato,btnKundeDetSok,btnProduksjonsDato").


  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customDeleteValProc","=kordre_delete.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","=kordre_update.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"postupdateproc","kordre_post_upd.p").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","source_KOrdre_Id,OldTotalRabatt%").

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectTBOrdreHode:HANDLE,
                            "",
                            "new;Ny,Copy;Kopier,undo;Angre,delete;Slett,save;Lagre,Note;Notat"
                          + ",Print"
/*                           + ",Tilbud;Send tilbud"  */
                          + ",Bekreft;Bekreft ordre"
                          + ",Lever;Lever ordre"
                          + ",Etiketter;Etiketter;Skriv ut etiketter"
                          + ",Postpakke;Postpakke etikett;Skriv ut postpakke etikett"
                          + ",Sendingsnr;Sendingsnr"
                          + ",Email;Send ep&ost"
                          + ",SMS;S&MS"
                          + ",Makuler" 
                          + (IF cReturLst <> '' THEN ',Retur' ELSE '') 
                            ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteAddTimeStamp","yes").
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteSplitEditor","yes").


  IF SEARCH("controls.dll") NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,"tabFolderProg","JBoxJlwTabFolder.w").

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).

/*   DYNAMIC-FUNCTION("setImageList" IN hTabFolder,"bmp\PJINTL_DLL_1134.bmp;bmp\search.bmp;bmp\accep16d.bmp;bmp\accep16d.bmp;bmp\redtick.bmp") NO-ERROR. */

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Linjer|KOrdreLinje.w|Adresse|KOrdreView2.w|Kundemerknad|KOrdreView3.w|Internmerknad|KOrdreView4.w|Gaveinnpakning|KOrdreView5.w",hQuery).

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

END.

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

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
DEF VAR hCurrWidget   AS HANDLE NO-UNDO.

hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
IF NOT hCurrWidget:MODIFIED THEN RETURN.

CASE icFieldName:
  WHEN 'KundeNr' THEN DO WITH FRAME {&FRAME-NAME}:
    cValueList = DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE KundeNr = '" + KundeNr:SCREEN-VALUE + "'"
                                         ,cKundeFields).
    IF cValueList NE ? THEN DO:
      IF EndreKundeMva(KundeNr:SCREEN-VALUE) THEN
        setKunde(cValueList).
      ELSE KundeNr:SCREEN-VALUE = STRING(hFieldMap:BUFFER-FIELD("KundeNr"):BUFFER-VALUE).
    END. 
  END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeverRecord C-Win 
PROCEDURE LeverRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLevVareList AS CHAR NO-UNDO.
DEF VAR cRowIdList   AS CHAR NO-UNDO.
DEF VAR iReturn      AS INT  NO-UNDO.
DEF VAR cStatus      AS CHAR NO-UNDO.
DEF VAR piLevFNr     AS INT NO-UNDO.

IF DECIMAL(KundeNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Kunde må registreres før ordre kan leveres","","").
  RETURN.
END.

bOpprettFaktura = IF DYNAMIC-FUNCTION("getFieldValues","SysPara",
                           "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 8","Parameter1") = '1'
                    THEN TRUE
                    ELSE FALSE.


iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,"Skal ordren full-leveres","","").

IF iReturn = 2 THEN RETURN.
ELSE IF iReturn = 6 THEN DO:
  RUN kordre_sjekkartnettbutikk.p(DEC(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)).

  IF NOT DYNAMIC-FUNCTION("runproc","kordre_levering.p",STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    IF bOpprettFaktura = FALSE THEN 
    DO:
        IF DYNAMIC-FUNCTION("runproc",
                         IF INTEGER(hFieldMap:BUFFER-FIELD("Opphav"):BUFFER-VALUE) = 10 THEN "kordre_kontant.p" ELSE "kordre_fakturer.p",
                         STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),
                         ?) THEN 
           RUN skrivkundeordre.p (STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|utlev",
                                 NO,"",1,"",DYNAMIC-FUNCTION("getTransactionMessage")).
        ELSE
          DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    END.
    ELSE DO:
        IF DYNAMIC-FUNCTION("runproc",
                         "kordre_fakturer.p",
                         STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),
                         ?) THEN 
           RUN skrivkundeordre.p (STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|utlev",
                                 NO,"",1,"",DYNAMIC-FUNCTION("getTransactionMessage")).
        ELSE
          DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    END.
  END.
END.
ELSE IF iReturn = 7 THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.
  
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "_file"                               /* Dummy buffer */
                      + ";!_file-name"                      /* <- must invoke a dummy non-visual field to avoid initial sort since calculated fields normally arn't sortable */
                      + ";+KOrdreLinjeNr|INTEGER|>>9||Lnr"
                      + ";+VareNr|CHARACTER|x(12)||Varenr"
                      + ";+VareTekst|CHARACTER|x(30)||Varetekst"
                      + ";+Antall|DECIMAL|>>>>9 ||Antall"
                      + ";+LevertAntall|DECIMAL|>>>>9||Leverings" + CHR(10) + "antall"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "KOrdreLinjeNr,LevertAntall",
                      INPUT-OUTPUT cLevVareList,
                      "","",
                      OUTPUT bOK).
  
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.
  
  IF bOk THEN DO:
    IF NOT DYNAMIC-FUNCTION("runproc",
                            "kordre_levering.p",
                            STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + ";" + cLevVareList
                           ,?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    ELSE DO: 
      cStatus = DYNAMIC-FUNCTION("getTransactionMessage").
      
      IF bOpprettFaktura = FALSE THEN
      DO:
          IF DYNAMIC-FUNCTION("runproc",
                             IF INTEGER(hFieldMap:BUFFER-FIELD("Opphav"):BUFFER-VALUE) = 10 THEN "kordre_kontant.p" ELSE "kordre_fakturer.p",
                             STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),
                             ?) THEN
            RUN skrivkundeordre.p (STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|utlev",
                                   NO,"",1,"",cStatus).
          ELSE
            DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      END.
      ELSE DO:
        IF DYNAMIC-FUNCTION("runproc",
                           "kordre_fakturer.p",
                            STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),
                           ?) THEN
          RUN skrivkundeordre.p (STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|utlev",
                                 NO,"",1,"",cStatus).
        ELSE
          DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
      END.
    END.
  END.
END.

DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
DYNAMIC-FUNCTION("setCurrentObject",hQuery).
RUN DisplayRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakulerRecord C-Win 
PROCEDURE MakulerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcOldLevStatus AS CHAR NO-UNDO.
DEF VAR pcKordre_Id AS CHAR NO-UNDO.
DEFINE VARIABLE dKordre_Id AS DECIMAL     NO-UNDO.
DEF VAR piOpphav AS INT NO-UNDO.

DEFINE VARIABLE lNekad AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPs12  AS LOGICAL     NO-UNDO.

piOpphav = INT(Opphav:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

IF INT(LevStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 60 THEN RETURN.

/* TN 9/2-17 Dette skal tillates. 
IF INT(LevStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) < 50 AND 
    INT(ButikkNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = iNettButikk THEN 
DO:
    MESSAGE 'Ordre fra nettbutikk kan ikke makuleres.' SKIP 
            'Sett antall til 0 på hver ordrerad, og utlever ordren på vanlig måte.' SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
*/

/* For JF */
IF piOpphav = 10 AND cNettButikkType = "2" /* PRS nettbutikk */ THEN DO:
    dKordre_Id = DECI(KOrdre_id:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    IF CAN-FIND(FIRST kordrelinje WHERE kordrelinje.kordre_id = dKordre_id AND KOrdrelinje.plockstatus > 2) THEN
        lNekad = TRUE.
    IF CAN-FIND(FIRST kordrelinje WHERE kordrelinje.kordre_id = dKordre_id AND KOrdrelinje.plockstatus > 0 AND 
                                                                               KOrdrelinje.plockstatus < 3) THEN
        lPs12 = TRUE.
    IF lNekad AND lPs12 THEN DO:
        MESSAGE "Behandling av order påbörjad. >> Manuell handtering krävs"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END.
/* Også JF funksjon. */
IF piOpphav = 10 AND INT(LevStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <= 50 AND cNettButikkType = "2" AND lNekad THEN DO:
    IF INT(LevStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) < 50 THEN DO:
        IF DYNAMIC-FUNCTION("DoMessage",0,4,'Sett ordrestatus til status "Makulert"',"","") = 6 THEN DO:
          DYNAMIC-FUNCTION("DoUpdate","KOrdreHode","",
                           "",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                           "LevStatus","60",
                           YES).
          DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
          DYNAMIC-FUNCTION("setCurrentObject",hQuery).
          ON WRITE OF kordrelinje OVERRIDE DO: END.
          dKordre_Id = DECI(KOrdre_id:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN DO:
              FOR EACH kordrelinje WHERE kordrelinje.kordre_id = dKordre_id AND KOrdrelinje.plockstatus > 0 USE-INDEX FaktLinje:
                  KOrdrelinje.plockstatus = 0.
              END.
          END.
          RUN DisplayRecord.
        END.
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ordre er levert og kan ikke makuleres","","").
END.

/* Generell håndtering - Gjelder også Gant. */
ELSE IF INT(LevStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <= 50 THEN 
DO:
    /* Tar vare på gammel status. */
    ASSIGN
        pcOldLevStatus = LevStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        piOpphav       = DEC(hFieldMap:BUFFER-FIELD("Opphav"):BUFFER-VALUE)
        .

    /* Makulering av hele ordren for Nettbutikk. */
    IF (pcOldLevStatus = '50' AND piOpphav = 10) THEN 
    DO:
        IF DYNAMIC-FUNCTION("DoMessage",0,4,'Skal ordren makuleres? ' + CHR(10) + 
                                            ' Salget vil bli returnert, og varene flyttet tilbake til lageret.' + CHR(10) + 
                                            (IF piOpphav = 10 THEN 'NB: Retur av betaling til kunde blir ikke håndtert av nettbutikk. Må gjøres på annen måte.' ELSE '')
                                            ,"","") = 6 THEN 
        DO:
    
            DYNAMIC-FUNCTION("DoUpdate","KOrdreHode","",
                           "",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                           "LevStatus,SendingsNr","60|MAKULERT50",
                           YES).
            DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
            DYNAMIC-FUNCTION("setCurrentObject",hQuery).
            RUN DisplayRecord.
    
            IF pcOldLevStatus = '50' THEN 
                RUN kordre_makuler.p(DEC(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE), pcOldLevStatus).
        END.
    END.
    /* Makulering av hele ordren for Nettbutikk. */
    ELSE IF (pcOldLevStatus = '30' AND piOpphav = 10) THEN 
    DO:
        IF DYNAMIC-FUNCTION("DoMessage",0,4,'Skal ordren makuleres? - Varene vil bli flyttet tilbake til lageret.',"","") = 6 THEN 
        DO:
    
            DYNAMIC-FUNCTION("DoUpdate","KOrdreHode","",
                           "",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                           "LevStatus,SendingsNr","60|MAKULER30",
                           YES).
            DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
            DYNAMIC-FUNCTION("setCurrentObject",hQuery).
            RUN DisplayRecord.
    
            /* Ingen retur av salg. Bare tilbakeføre varer til lageret. */
            IF pcOldLevStatus = '30' THEN 
                RUN opprett_overforingsordre.p(STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),TRUE).
            
            dKordre_Id = DECI(KOrdre_id:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
            FOR EACH kordrelinje WHERE kordrelinje.kordre_id = dKordre_id AND KOrdrelinje.plockstatus > 0 USE-INDEX FaktLinje:
                KOrdrelinje.plockstatus = 0.
            END.
        END.
    END.
    /* Vanlig makulering */
    ELSE IF DYNAMIC-FUNCTION("DoMessage",0,4,'Skal ordren makuleres? - status settes til "Makulert"',"","") = 6 THEN 
      DO:
          DYNAMIC-FUNCTION("DoUpdate","KOrdreHode","",
                           "",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                           "LevStatus","60",
                           YES).
          DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
          DYNAMIC-FUNCTION("setCurrentObject",hQuery).
          RUN DisplayRecord.
      END.


END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ordre er levert og kan ikke makuleres","","").
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

DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,rectFolder:HANDLE IN FRAME {&FRAME-NAME}).

RUN MoveToTop IN hCurrTabProc NO-ERROR.

APPLY "entry" TO ButikkNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

IF ihFillIn:INPUT-VALUE > ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE OR ihFillIn:INPUT-VALUE = 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").
  obOK = NO.
  RETURN.
END.
ihBuffer:BUFFER-FIELD("LevertAntall"):BUFFER-VALUE = ihFillIn:INPUT-VALUE.

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
DEF VAR cButikkNr   AS CHAR NO-UNDO.
DEF VAR cKasse AS CHAR NO-UNDO.
    
ButikkNr:MODIFIED IN FRAME {&FRAME-NAME} = NO.

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
DYNAMIC-FUNCTION("setAttribute",hToolbar,"NoteReadOnly","").
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"ReadOnlyFields","").
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"DisabledFields","").

RUN SUPER.

ASSIGN cButikkNr = DYNAMIC-FUNCTION("getButikkNr" IN hParent)
       LeveringsDato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
       fiInternMerknad:HIDDEN = TRUE
       fiKundeMerknad:HIDDEN  = TRUE
       fiGaveinnpakning:HIDDEN = TRUE.

IF cButikkNr NE ? AND cButikkNr NE "0" THEN DO:
  ButikkNr:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = cButikkNr.
  ButikkNr:MODIFIED = NO.
  APPLY "value-changed" TO ButikkNr.
  RUN ValueChangedField ("Verkstedordre").
/*   APPLY "choose" TO btnKundeDetSok.  */
END.


DYNAMIC-FUNCTION("TabFolderChanged" IN hTabfolder,2).
APPLY "entry" TO KundeNr IN FRAME {&FRAME-NAME}.
/* APPLY "entry" TO LeveringsDato IN FRAME {&FRAME-NAME}. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NoteRecord C-Win 
PROCEDURE NoteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
IF DYNAMIC-FUNCTION("getAttribute",hToolbar,"NoteCurrentValueChanged") = "yes" THEN DO:
  DYNAMIC-FUNCTION("DoUpdate",hFieldMap:NAME,"",
                    "",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                    "VerkstedMerknad",DYNAMIC-FUNCTION("getAttribute",hToolbar,"NoteCurrentValue"),
                   YES).
  VerkstedMerknad:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hToolbar,"NoteCurrentValue").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostpakkeRecord C-Win 
PROCEDURE PostpakkeRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
        DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
        DEFINE VARIABLE pbOk   AS LOG     NO-UNDO.
        
      
    IF hFieldMap:AVAIL THEN
    DO WITH FRAME default-frame:
        IF INT(hFieldMap:BUFFER-FIELD("LevFNr"):BUFFER-VALUE) = 8 THEN
        DO:
            DYNAMIC-FUNCTION("DoMessage",0,0,"Ordren har leveringsmåte 8 - Utleveres fra butikk. Den skal pakkes og klargjøres og sendes butikk. Den skal ikke leveres via posten, og derfor ikke ha postpakke etikett.'","","").
            RETURN.
        END.
        /* Sjekker om sendingsnr ligger i filen. */
        IF SendingsNr:SCREEN-VALUE = '' THEN
        DO:
            RUN xWinEDIinnlesdirekte.p (STRING(hFieldMap:BUFFER-FIELD("EkstOrdreNr"):BUFFER-VALUE)). /* Leser inn sendingsnr direkte fra fil. */
            DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
            DYNAMIC-FUNCTION("setCurrentObject",hFieldMap).
            RUN DisplayRecord.      
        END.

      IF INT(AntPPEti:SCREEN-VALUE IN FRAME default-frame) > 0 THEN
        DO:
          MESSAGE 'Det er allerede tatt ut postpakke etikett på denne kundeodre.' SKIP
                  'Vil du ta ut en ny etikett?'
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE pbOk.
          IF pbOk <> TRUE THEN RETURN.
          ASSIGN
            SendingsNr:SCREEN-VALUE = ''
            ReturNr:SCREEN-VALUE = ''.                               
        END.
      RUN d-skrivPostPakkeEtikett.w (DECIMAL(hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE)).
      IF RETURN-VALUE = 'OK' THEN  
      LOOPEN:
      DO WHILE TRUE:
        RUN xWinEDIinnlesdirekte.p (STRING(hFieldMap:BUFFER-FIELD("EkstOrdreNr"):BUFFER-VALUE)). /* Leser inn sendingsnr direkte fra fil. */
        RUN postpakke_dialog.w.
        IF RETURN-VALUE = 'AVBRYT' THEN 
        DO: 
            DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
            DYNAMIC-FUNCTION("setCurrentObject",hFieldMap).
            RUN DisplayRecord.      
            LEAVE LOOPEN.
        END.
        
        DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
        DYNAMIC-FUNCTION("setCurrentObject",hFieldMap).
        RUN DisplayRecord.      
        
        IF SendingsNr:SCREEN-VALUE <> '' THEN LEAVE LOOPEN.
      END. /* LOOPEN */
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
/* DEF VAR iSelection AS INT NO-UNDO.                                                                                  */
/* RUN JBoxDSelectFunction.w ("Ordrebekreftelse|Faktura",OUTPUT iSelection).                                           */
/* IF iSelection NE 0 THEN DO:                                                                                         */
/*   DYNAMIC-FUNCTION("runproc","kordre_produksjon.p",STRING(hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE),?).     */
/*   IF iSelection = 2 THEN                                                                                            */
/*     DYNAMIC-FUNCTION("runproc","faktura_produksjon.p",STRING(hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE),?).  */
/* END.                                                                                                                */

  DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.
  
  RUN skrivkundeordre.p (STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|full",
                         NO,"",1,"","").
  DO TRANSACTION:
    FIND bufKORdreHode EXCLUSIVE-LOCK WHERE 
        bufKOrdreHode.KOrdre_Id = hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE NO-ERROR.
    IF AVAILABLE bufKOrdrEHode THEN 
    DO:
        ASSIGN 
            bufKOrdrEHode.AntApnet = bufKOrdreHode.AntApnet + 1
            .
        DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
        DYNAMIC-FUNCTION("setCurrentObject",hFieldMap).
        RUN DisplayRecord.
        RELEASE bufKOrdreHode.      
    END.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturRecord C-Win 
PROCEDURE ReturRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{syspara.i 5 1 1 iCL INT}
IF NOT hFieldMap:AVAILABLE THEN
    RETURN.
ELSE DO:
    IF NOT CAN-DO(cReturLst,STRING(hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE)) THEN 
    DO:
        MESSAGE 'Retur er ikke tillatt for denne butikken.' SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    ELSE 
    RETURBLOKK:
    DO:
        IF INT(LevStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <> 50 OR
           STRING(SendingsNr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) =  'RETUR'
            THEN RETURN.

        RUN w-returJFweb.w (KOrdre_Id:SCREEN-VALUE,iCL).
        IF RETURN-VALUE <> "Avbryt" THEN
        . /* refresh */

    END. /* RETURBLOKK */
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Hvis ordre er kopiert så ligger det allerede en verdi i bufferextravalues (fra CopyRecord)
              Ny pris på varelinjer skal bare beregnes dersom totalrabatt er endret. Derfor legges den gamle verdien med 
------------------------------------------------------------------------------*/
DEF VAR bNew AS LOG NO-UNDO.

bNew = DYNAMIC-FUNCTION("getToolbarState",hToolbar) = "new".

DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                 DYNAMIC-FUNCTION("getAttribute",hFieldMap,"bufferextravalues") + "|" + cOldTotalRabatt%).

RUN SUPER.

/* For ikke å få flere og flere elementer i lista: */
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues","").


IF bNew THEN DO:

  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
  RUN MoveToTop IN hCurrTabProc.

  RETURN NO-APPLY.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendingsnrRecord C-Win 
PROCEDURE SendingsnrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcSendingsNr AS CHAR NO-UNDO.

    IF hFieldMap:AVAIL THEN
    DO WITH FRAME default-frame:
        pcSendingsNr = (hFieldMap:BUFFER-FIELD("SendingsNr"):BUFFER-VALUE).
        RUN dOppdaterSendingsNr.w (INPUT-OUTPUT pcSendingsNr).
        IF CAN-DO('true,yes',RETURN-VALUE) THEN
        DO:
            SendingsNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = pcSendingsNr.
            RUN InvokeMethod(hFieldMap,"SaveRecord").
            /*
            DYNAMIC-FUNCTION("refreshRowids",hQuery,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
            DYNAMIC-FUNCTION("setCurrentObject",hFieldMap).
            */
        END.
    END.

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
DEF INPUT PARAM hBrwLookup AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2   AS HANDLE NO-UNDO.

IF CAN-DO("Post",hBrwLookup:QUERY:GET-BUFFER-HANDLE(1):NAME) THEN
  DYNAMIC-FUNCTION("setAttribute",hBrwLookup,"basequery","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSearch C-Win 
PROCEDURE setSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icKOrdre_id AS CHAR NO-UNDO.

bOk = hFieldMap:FIND-FIRST("WHERE KOrdre_id = " + icKOrdre_id) NO-ERROR.
IF NOT bOK THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hQuery,"queryfilter","").
  DYNAMIC-FUNCTION("setAttribute",hQuery,"querywhere","WHERE KOrdre_id = " + icKOrdre_id).
  RUN OpenQuery.
END.
ELSE DO:
  hQuery:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID).
  RUN DisplayRecord.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer   AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer   AS HANDLE NO-UNDO.
DEF VAR cVareNrList     AS CHAR   NO-UNDO.
DEF VAR hSelectorWin    AS HANDLE NO-UNDO.
DEF VAR hSelectorSplitB AS HANDLE NO-UNDO.
DEF VAR hAntallOverlay  AS HANDLE NO-UNDO.

hSourceBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1).
hTargetBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1).

DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"basequery","where true").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"querysort","KOrdreLinjeNr").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"querysort","KOrdreLinjeNr").

cVareNrList = DYNAMIC-FUNCTION("getFieldList",
                               "KOrdreLinje;KOrdreLinjeNr;VareNr;Varetekst;Antall",
                               "WHERE KOrdre_id = " + STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)
                              + " AND LeveringsDato = ? BY KOrdreLinjeNr").

DO ix = 1 TO NUM-ENTRIES(cVareNrList,"|") BY 4:

  hSourceBuffer:BUFFER-CREATE().
  ASSIGN hSourceBuffer:BUFFER-FIELD("KOrdreLinjeNr"):BUFFER-VALUE = INT(ENTRY(ix,cVareNrList,"|"))
         hSourceBuffer:BUFFER-FIELD("VareNr"):BUFFER-VALUE        = DEC(ENTRY(ix + 1,cVareNrList,"|"))
         hSourceBuffer:BUFFER-FIELD("VareTekst"):BUFFER-VALUE     = ENTRY(ix + 2,cVareNrList,"|")
         hSourceBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE        = DEC(ENTRY(ix + 3,cVareNrList,"|"))
         hSourceBuffer:BUFFER-FIELD("LevertAntall"):BUFFER-VALUE  = DEC(ENTRY(ix + 3,cVareNrList,"|"))
         hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE     = "rowid" + STRING(ix)
         .
END.

ihSourceBrw:GET-BROWSE-COLUMN(5):VISIBLE = NO.

ASSIGN hSelectorWin = ihSourceBrw:WINDOW
       hSelectorWin:WIDTH-PIXELS = hSelectorWin:WIDTH-PIXELS + 350
       hSelectorSplitB = DYNAMIC-FUNCTION("getSplitBarHandle",hSelectorWin,"x")
       .
APPLY "window-resized" TO hSelectorWin.
hSelectorSplitB:X = hSelectorSplitB:FRAME:WIDTH-PIXELS / 2 - hSelectorSplitB:WIDTH-PIXELS / 2 - 50.
APPLY "end-move" TO hSelectorSplitB.

hAntallOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                                     ihTargetBrw,
                                     "LevertAntall",
                                     "LevertAntall",
                                     "","","","").
DYNAMIC-FUNCTION("CreateOverlayLink",ihTargetBrw,hAntallOverlay,"LevertAntall").

DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
RUN OpenQuerySource IN SOURCE-PROCEDURE.

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
DEF VAR cKundeInfo AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("getKundeInfo" IN hAdresseProc,OUTPUT cKundeInfo).

DO WITH FRAME {&FRAME-NAME}:
  IF ENTRY(19,cKundeInfo,"|") = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Mobiltelefon er ikke utfylt","","").
    RETURN.
  END.
  cMessage = "Hei, din ordre er klar for henting." + CHR(10) 
           +  "Vennlig hilsen " 
           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + ButikkNr:SCREEN-VALUE,"ButNamn,BuAdr")
                    ,"|",", ").
  RUN SendSMS.w (ENTRY(19,cKundeInfo,"|"),
                 "Ordrenr: " + KOrdre_Id:SCREEN-VALUE,
                 cMessage,
                 OUTPUT bOK,
                 OUTPUT cMessage).
  IF bOK THEN DO:
    VerkstedMerknad:SCREEN-VALUE = cMessage + CHR(10) + VerkstedMerknad:SCREEN-VALUE.
    RUN InvokeMethod(hFieldMap,"SaveRecord").
  END.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilbudRecord C-Win 
PROCEDURE TilbudRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        "Utskrift av tilbud og setting av tilbudsfrist" SKIP
        VIEW-AS ALERT-BOX.
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
    WHEN "ButikkNr" THEN DO:
      setKasse(ButikkNr:SCREEN-VALUE).
      ASSIGN KasseNr:SCREEN-VALUE = KasseNr:ENTRY(1)
             ForsNr:SCREEN-VALUE  = ForsNr:ENTRY(1)
             .
      setMinKasse(ButikkNr:SCREEN-VALUE).
    END.
    WHEN "Verkstedordre" THEN DO:
      ASSIGN ProdStatus:SENSITIVE = Verkstedordre:CHECKED
             AnsvVerksted:SENSITIVE = Verkstedordre:CHECKED
             ProduksjonsDato:READ-ONLY = NOT Verkstedordre:CHECKED
             btnProduksjonsDato:SENSITIVE = Verkstedordre:CHECKED
/*              VerkstedMerknad:SENSITIVE = Verkstedordre:CHECKED */

             ProdStatus:MODIFIED = NO
             AnsvVerksted:MODIFIED = NO
             ProduksjonsDato:MODIFIED = NO
/*              VerkstedMerknad:MODIFIED = NO */
             .
      IF Verkstedordre:CHECKED AND ProduksjonsDato:INPUT-VALUE = ? AND NOT PROGRAM-NAME(2) BEGINS "DisplayRecord" THEN
        ProduksjonsDato:SCREEN-VALUE = LeveringsDato:SCREEN-VALUE.
    END.
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndreKundeMva C-Win 
FUNCTION EndreKundeMva RETURNS LOGICAL
  ( INPUT icNyttKundenr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DEC(Kundenr:SCREEN-VALUE) NE 0 AND 
     DYNAMIC-FUNCTION("getFieldValues","FIRST Kordrelinje",
                      "WHERE KOrdre_Id = " + hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE,"KOrdre_Id") NE ? THEN DO:
    IF hFieldMap:BUFFER-FIELD("Mvafri"):BUFFER-VALUE NE LOGICAL(DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE Kundenr = " + icNyttKundenr,"Mvafri")) THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Valgt kunde har ulik håndtering av mva. Du kan ikke endre mva beregning på en ordre som har varelinjer","","").
      RETURN NO.
    END.
  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldMap C-Win 
FUNCTION getFieldMap RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hFieldMap.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastTabWidget C-Win 
FUNCTION getLastTabWidget RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN ProduksjonsDato:HANDLE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrdreLinjeQuery C-Win 
FUNCTION getOrdreLinjeQuery RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hQuery.  

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
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectVerksted").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectTBordreHode,rectOrdreHode,brwOrdreHode,VerkstedMerknad,rectVerksted").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAdresseProc C-Win 
FUNCTION setAdresseProc RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hAdresseProc = SOURCE-PROCEDURE.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKasse C-Win 
FUNCTION setKasse RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Henter også kasserer og selger 
------------------------------------------------------------------------------*/
DEF VAR cKasseList AS CHAR NO-UNDO.
DEF VAR cForsList  AS CHAR NO-UNDO.

cKasseList = DYNAMIC-FUNCTION("getFieldList","kasse;Navn;KasseNr",
                              "WHERE ButikkNr = '" + icButikkNr + "' AND Aktiv AND KasseNr >= '" + "90" + "' AND ModellNr = " + STRING(iKasseModKOrdre)).

IF cKasseList NE "" THEN
  KasseNr:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cKasseList.
ELSE DO:
  cKasseList = DYNAMIC-FUNCTION("getFieldList","kasse;Navn;KasseNr",
                                "WHERE ButikkNr = '" + icButikkNr + "' AND Aktiv AND KasseNr >= '" + "90" + "'").
  IF cKasseList NE "" THEN
    KasseNr:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cKasseList.
  ELSE
    KasseNr:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = "|0".
END.

ASSIGN KasseNr:SCREEN-VALUE     = KasseNr:ENTRY(1)
       KasseNr:MODIFIED         = NO
       cForsList                = DYNAMIC-FUNCTION("getFieldList","butikkforsalj,forsalj;forsnr|Fonamn;forsnr","where butik = " + icButikkNr + ",first forsalj NO-LOCK of butikkforsalj")
       ForsNr:LIST-ITEM-PAIRS   = IF cForsList NE "" THEN cForsList ELSE "|"
       SelgerNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","butikkselger,selger;selgernr|navn;selgernr","where butikknr = " + icButikkNr + ",first selger NO-LOCK where Selger.SelgerNr = butikkselger.SelgerNr"),"|")
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKunde C-Win 
FUNCTION setKunde RETURNS LOGICAL
  ( INPUT icKunde AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN Navn:SCREEN-VALUE         = ENTRY(1,icKunde,"|")
         TotalRabatt%:SCREEN-VALUE = ENTRY(5,icKunde,"|")
         .
  ASSIGN
      BetBet:SCREEN-VALUE       = ENTRY( 6,icKunde,"|") 
      DeresRef:SCREEN-VALUE     = ENTRY(10,icKunde,"|") 
      NO-ERROR.
  DYNAMIC-FUNCTION("setKunde" IN hAdresseProc,icKunde).
END.
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLinjeToolbar C-Win 
FUNCTION setLinjeToolbar RETURNS LOGICAL
  ( INPUT ihLinjeToolbar AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hLinjeToolbar = ihLinjeToolbar.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMinKasse C-Win 
FUNCTION setMinKasse RETURNS LOGICAL
  ( INPUT icButikkNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDefKasse AS CHAR NO-UNDO.

/*
cKasseList = DYNAMIC-FUNCTION("getFieldList","kasse;Navn;KasseNr",
                              "WHERE ButikkNr = " + icButikkNr + " AND Aktiv AND KasseNr = " 
                            + DYNAMIC-FUNCTION("getFieldValues","Bruker",
                                                "WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","KasseNr")).

IF cKasseList NE "" THEN
  KasseNr:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cKasseList.
*/

cDefKasse = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + icButikkNr,"KasseNr").
IF cDefKasse NE ? THEN DO WITH FRAME {&FRAME-NAME}:
  KasseNr:SCREEN-VALUE = cDefKasse NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    KasseNr:SCREEN-VALUE = KasseNr:ENTRY(1).
END.
ELSE KasseNr:SCREEN-VALUE = KasseNr:ENTRY(1).

KasseNr:MODIFIED = NO.

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

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hParentQuery  = ihQuery
       hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

IF iCurrTab = 2 AND iiTab NE 2 THEN DO:
  IF NOT DYNAMIC-FUNCTION("SaveModified",hQuery) THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hParentQuery).
    RUN DisplayRecord.
  END.
END.

IF iCurrTab > 0 THEN 
  DYNAMIC-FUNCTION("DeleteObjectLink",hQuery,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).
ELSE /* Første gang må det lages parent-link fra toolbar i ordrelinje til toolbar i ordrehode: */
  DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getToolbarHandle" IN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)),hToolbar,"toolbar").

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

IF iiTab = 1 THEN 
  DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hQuery,"KOrdre_Id").

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab
       .

RUN MoveToTop.

/* Normalt vil en kjøre "DisplayRecord" her for å åpne query for linjer men det fører også
   til en re-display av ordrehode med sjekk for endringer, etc og det vil vi ikke når vieweren spenner
   over flere framer (taber): */ 
IF iiTab = 1 THEN 
  RUN StartQuery IN hCurrTabProc (IF hFieldMap:AVAIL THEN hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE ELSE 0).

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

