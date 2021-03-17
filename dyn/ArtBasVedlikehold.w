&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR ihParent AS HANDLE NO-UNDO.
  DEF VAR icAction AS CHAR NO-UNDO INIT "NEW".
  DEF VAR obOk     AS LOG NO-UNDO.
&ELSE
  DEF INPUT  PARAM ihParent AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icAction AS CHAR NO-UNDO.
  DEF OUTPUT PARAM obOk     AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR ix                AS INT NO-UNDO.
DEF VAR bOk               AS LOG NO-UNDO.

DEF VAR fArtikkelNr       AS DEC NO-UNDO.
DEF VAR bHovedModellExist AS LOG NO-UNDO.
DEF VAR fMomsProc         AS DEC NO-UNDO.
DEF VAR fValKurs          AS DEC NO-UNDO.
DEF VAR cFieldValues      AS CHAR NO-UNDO.
DEF VAR cModellFarge      AS CHAR NO-UNDO.

DEF VAR wSjekkStreng AS CHAR                  NO-UNDO.

DEF TEMP-TABLE ttVerdier
    FIELD cNavn   AS CHAR
    FIELD cVerdi  AS CHAR
    .
DEF VAR httVerdier  AS HANDLE NO-UNDO.
httVerdier = BUFFER ttVerdier:TABLE-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Valkod btnCancel btnSave ArtSlag LevNr ~
LevKod LevFargKod cmbModell tbModell Beskr Vg VgKat StrTypeID Farg SaSong ~
BUTTON-SokProfil ProdNr matkod FI-ProfilNr FI-ValPris FI-InnkjopsPris ~
FI-Rab1kr FI-Rab1% FI-Rab2kr FI-Rab2% FI-Frakt FI-Frakt% FI-DivKostKr ~
FI-DivKost% FI-Rab3kr FI-Rab3% FI-VareKost FI-DBkr FI-DB% FI-MVAkr FI-Mva% ~
btnFarge FI-AnbefaltPris FI-Pris FI-EuroPris T-Manuel btnSasong btnLev ~
btnVarGr btnStr tbFullKalk FarBeskr SasBeskr levnamn vgbeskr Beskrivelse ~
BongTekst btnProdNr btnModell RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS ModellFarge FI-Valkod ArtSlag LevNr LevKod ~
LevFargKod cmbModell tbModell Beskr Vg VgKat StrTypeID Farg SaSong ProdNr ~
matkod FI-ProfilNr FI-ValPris FI-InnkjopsPris FI-Rab1kr FI-Rab1% FI-Rab2kr ~
FI-Rab2% FI-Frakt FI-Frakt% FI-DivKostKr FI-DivKost% FI-Rab3kr FI-Rab3% ~
FI-VareKost FI-DBkr FI-DB% FI-MVAkr FI-Mva% FI-AnbefaltPris FI-Pris ~
FI-EuroPris T-Manuel tbFullKalk FarBeskr SasBeskr levnamn vgbeskr ~
Beskrivelse FI-Beskrivelse BongTekst ProdBeskrivelse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 FI-InnkjopsPris FI-Rab2kr FI-Rab2% FI-Frakt FI-Frakt% ~
FI-DivKostKr FI-DivKost% FI-Rab3kr FI-Rab3% FI-VareKost FI-DBkr FI-DB% ~
FI-MVAkr FI-Mva% 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng C-Win 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateInput C-Win 
FUNCTION ValidateInput RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnFarge 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnModell 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnProdNr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSasong 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSave  NO-FOCUS
     LABEL "&Lagre og avslutt" 
     SIZE 17.8 BY 1.14.

DEFINE BUTTON btnStr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokProfil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE ArtSlag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vareslag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 30.4 BY 1 NO-UNDO.

DEFINE VARIABLE cmbModell AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mod.farger" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE matkod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Materiale" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE VgKat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kategori" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Beskr" 
     VIEW-AS FILL-IN 
     SIZE 31.4 BY 1.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13.8 BY .62 NO-UNDO.

DEFINE VARIABLE BongTekst AS CHARACTER FORMAT "X(20)" 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 24.4 BY 1.

DEFINE VARIABLE FarBeskr AS CHARACTER FORMAT "x(30)" 
      VIEW-AS TEXT 
     SIZE 15.4 BY .62.

DEFINE VARIABLE Farg AS INTEGER FORMAT "zzzz9" INITIAL 0 
     LABEL "Fargekode" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE FI-AnbefaltPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Veil.pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 18.6 BY .62 NO-UNDO.

DEFINE VARIABLE FI-DB% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DBkr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "DB (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKost% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DivKostKr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Divkost(+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuroPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris(Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnkjopsPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkj.pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Mva% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MVAkr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Mva (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Markedspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ProfilNr AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1kr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rab 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2kr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rab 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3kr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rab 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Valkod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "Lev.farge" 
     VIEW-AS FILL-IN 
     SIZE 22.4 BY 1.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "Lev.artnr" 
     VIEW-AS FILL-IN 
     SIZE 22.4 BY 1.

DEFINE VARIABLE levnamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14.8 BY .62 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE ModellFarge AS DECIMAL FORMAT "->>>>>>>>>>>>>9":U INITIAL 0 
     LABEL "ModellNr" 
     VIEW-AS FILL-IN 
     SIZE 22.4 BY 1 NO-UNDO.

DEFINE VARIABLE ProdBeskrivelse AS CHARACTER FORMAT "x(30)" 
      VIEW-AS TEXT 
     SIZE 16 BY .62.

DEFINE VARIABLE ProdNr AS INTEGER FORMAT "zzzzz9" INITIAL 1 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE SasBeskr AS CHARACTER FORMAT "x(30)" 
      VIEW-AS TEXT 
     SIZE 18.8 BY .62.

DEFINE VARIABLE SaSong AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE StrTypeID AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Str.type" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE vgbeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13.8 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 16.52.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 16.52.

DEFINE VARIABLE T-Manuel AS LOGICAL INITIAL no 
     LABEL "Manuel" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbFullKalk AS LOGICAL INITIAL no 
     LABEL "Rediger alle kalkylefelter" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .71 NO-UNDO.

DEFINE VARIABLE tbModell AS LOGICAL INITIAL yes 
     LABEL "Mod" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .81 TOOLTIP "Skal denne artikkel inngå i samme modell" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ModellFarge AT ROW 5.71 COL 11.6 COLON-ALIGNED
     FI-Valkod AT ROW 3.91 COL 72 COLON-ALIGNED NO-LABEL
     btnCancel AT ROW 17.81 COL 73.2
     btnSave AT ROW 17.81 COL 55.2
     ArtSlag AT ROW 1.48 COL 11.6 COLON-ALIGNED
     LevNr AT ROW 2.57 COL 11.6 COLON-ALIGNED HELP
          "Leverandørnummer"
     LevKod AT ROW 3.62 COL 11.6 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevFargKod AT ROW 4.67 COL 11.6 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     cmbModell AT ROW 6.76 COL 11.6 COLON-ALIGNED
     tbModell AT ROW 6.91 COL 36.4
     Beskr AT ROW 7.81 COL 11.6 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     Vg AT ROW 9.86 COL 11.6 COLON-ALIGNED HELP
          "'varegruppenummer"
     VgKat AT ROW 10.86 COL 11.6 COLON-ALIGNED
     StrTypeID AT ROW 11.91 COL 11.6 COLON-ALIGNED HELP
          "Størrelsestype"
     Farg AT ROW 12.95 COL 11.6 COLON-ALIGNED HELP
          "Fargekode"
     SaSong AT ROW 14.05 COL 11.6 COLON-ALIGNED HELP
          "Sesong"
     BUTTON-SokProfil AT ROW 1.43 COL 63.8
     ProdNr AT ROW 15.1 COL 11.6 COLON-ALIGNED HELP
          "Produsent"
     matkod AT ROW 16.14 COL 11.6 COLON-ALIGNED
     FI-ProfilNr AT ROW 1.43 COL 56 COLON-ALIGNED
     FI-ValPris AT ROW 3.91 COL 56 COLON-ALIGNED
     FI-InnkjopsPris AT ROW 5 COL 56 COLON-ALIGNED
     FI-Rab1kr AT ROW 6.05 COL 56 COLON-ALIGNED
     FI-Rab1% AT ROW 6.05 COL 72 COLON-ALIGNED NO-LABEL
     FI-Rab2kr AT ROW 7.1 COL 56 COLON-ALIGNED
     FI-Rab2% AT ROW 7.1 COL 72 COLON-ALIGNED NO-LABEL
     FI-Frakt AT ROW 8.1 COL 56 COLON-ALIGNED
     FI-Frakt% AT ROW 8.1 COL 72 COLON-ALIGNED NO-LABEL
     FI-DivKostKr AT ROW 9.1 COL 56 COLON-ALIGNED
     FI-DivKost% AT ROW 9.1 COL 72 COLON-ALIGNED NO-LABEL
     FI-Rab3kr AT ROW 10.1 COL 56 COLON-ALIGNED
     FI-Rab3% AT ROW 10.1 COL 72 COLON-ALIGNED NO-LABEL
     FI-VareKost AT ROW 11.14 COL 56 COLON-ALIGNED
     FI-DBkr AT ROW 12.14 COL 56 COLON-ALIGNED
     FI-DB% AT ROW 12.14 COL 72 COLON-ALIGNED NO-LABEL
     FI-MVAkr AT ROW 13.14 COL 56 COLON-ALIGNED
     FI-Mva% AT ROW 13.14 COL 72 COLON-ALIGNED NO-LABEL
     btnFarge AT ROW 12.95 COL 24 NO-TAB-STOP 
     FI-AnbefaltPris AT ROW 14.33 COL 56 COLON-ALIGNED
     FI-Pris AT ROW 15.38 COL 56 COLON-ALIGNED
     FI-EuroPris AT ROW 16.38 COL 56 COLON-ALIGNED
     T-Manuel AT ROW 16.38 COL 74
     btnSasong AT ROW 14.1 COL 20 NO-TAB-STOP 
     btnLev AT ROW 2.57 COL 24.2 NO-TAB-STOP 
     btnVarGr AT ROW 9.81 COL 24.2 NO-TAB-STOP 
     btnStr AT ROW 11.91 COL 24.2 NO-TAB-STOP 
     tbFullKalk AT ROW 2.52 COL 58.2 NO-TAB-STOP 
     FarBeskr AT ROW 12.33 COL 27 COLON-ALIGNED NO-LABEL
     SasBeskr AT ROW 14.33 COL 23.6 COLON-ALIGNED NO-LABEL
     levnamn AT ROW 2.76 COL 27.6 COLON-ALIGNED NO-LABEL
     vgbeskr AT ROW 10.05 COL 27.6 COLON-ALIGNED NO-LABEL
     Beskrivelse AT ROW 12.19 COL 27.6 COLON-ALIGNED NO-LABEL
     FI-Beskrivelse AT ROW 1.67 COL 67.2 COLON-ALIGNED NO-LABEL
     BongTekst AT ROW 8.86 COL 11.6 COLON-ALIGNED HELP
          "Bongtekst - Tekst som vises på kundens kvittering" NO-TAB-STOP 
     btnProdNr AT ROW 15.14 COL 24.2 NO-TAB-STOP 
     ProdBeskrivelse AT ROW 15.38 COL 27 COLON-ALIGNED NO-LABEL
     btnModell AT ROW 5.76 COL 36.2 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.8 BY 18.05
         CANCEL-BUTTON btnCancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "Valuta" VIEW-AS TEXT
          SIZE 13 BY .57 AT ROW 3.24 COL 75
          FONT 6
     "Kroner" VIEW-AS TEXT
          SIZE 14.2 BY .57 AT ROW 3.24 COL 59.8
          FONT 6
     "Prosent" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5.38 COL 75
          FONT 6
     RECT-1 AT ROW 1.14 COL 1.4
     RECT-2 AT ROW 1.14 COL 45
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.8 BY 18.05
         CANCEL-BUTTON btnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopier artikkel"
         HEIGHT             = 18.05
         WIDTH              = 88.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MIN-BUTTON         = no
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       Beskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DB% IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-DBkr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-DivKost% IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-DivKostKr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-Frakt IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-Frakt% IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-InnkjopsPris IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-Mva% IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-MVAkr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-Rab2% IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-Rab2kr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-Rab3% IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-Rab3kr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN FI-VareKost IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       levnamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ModellFarge IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ProdBeskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       vgbeskr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

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
ON END-ERROR OF C-Win /* Kopier artikkel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kopier artikkel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtSlag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtSlag C-Win
ON VALUE-CHANGED OF ArtSlag IN FRAME DEFAULT-FRAME /* Vareslag */
DO:
    IF ArtSlag:SENSITIVE THEN DO:
        IF CAN-DO("0,2",ArtSlag:SCREEN-VALUE) THEN
            ASSIGN StrTypeID:SCREEN-VALUE = "2".
        ELSE IF CAN-DO("1,3",ArtSlag:SCREEN-VALUE) THEN
            ASSIGN StrTypeID:SCREEN-VALUE = "2".
    END.
    APPLY "TAB" TO StrTypeID.
    IF (LevNr:SCREEN-VALUE = "0" OR LevNr:SCREEN-VALUE = ? OR LevNr:SCREEN-VALUE = '?') THEN
        APPLY "ENTRY" TO LevNr.
    ELSE
        APPLY "ENTRY" TO LevKod.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Beskr C-Win
ON LEAVE OF Beskr IN FRAME DEFAULT-FRAME /* Beskr */
DO:
  IF Beskr:MODIFIED AND BongTekst:SCREEN-VALUE = "" THEN
    BongTekst:SCREEN-VALUE = Beskr:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFarge C-Win
ON CHOOSE OF btnFarge IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Farg;FarBeskr;Farg|Farge|>>>>9"
                   ,"WHERE TRUE"
                    ,""                                                  
                    ,"Farg,FarBeskr",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN Farg:SCREEN-VALUE     = ENTRY(1,cReturnValues,"|")
           FarBeskr:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    APPLY "entry" TO SaSong.
  END.
  ELSE APPLY "entry" TO Farg.

/*
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Farg;FarBeskr".

  RUN JBoxDLookup.w ("Farg;FarBeskr;Farg", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:    
    ASSIGN Farg:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
           FarBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|").
    APPLY "entry" TO SaSong.
  END.
  ELSE APPLY "entry" TO Farg.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.
  DEF VAR cLevData AS CHAR NO-UNDO.

  /* See also procedure setLookupAttributes */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "LevBas"
                    + ";LevNamn"
                    + ";LevNr"
                   ,"WHERE TRUE"
                    ,""                                                  
                    ,"LevNr,LevNamn",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN levnr:SCREEN-VALUE     = ENTRY(1,cReturnValues,"|")
           levnamn:SCREEN-VALUE   = ENTRY(2,cReturnValues,"|")
           levnr:MODIFIED         = TRUE
           cLevData               = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn;ValKod","WHERE LevNr = " + LevNr:SCREEN-VALUE)
           FI-ValKod:SCREEN-VALUE = ENTRY(2,cLevData,"|")
           .
    APPLY "ENTRY" TO LevKod.
  END.
  ELSE APPLY "ENTRY" TO LevNr.


/*
  DEF VAR cLookupValue AS CHAR NO-UNDO.
  DEF VAR cLevData AS CHAR NO-UNDO.

  cLookupValue = "levnr;levnamn".

  CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.

  RUN JBoxDLookup.w ("LevBas;levnamn;Levnr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).

  IF cLookupValue NE "" THEN DO:    
    ASSIGN levnr:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
           levnamn:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           levnr:MODIFIED       = TRUE
           .
  END.
  ELSE DO: 
      ASSIGN levnr:MODIFIED = TRUE.
  END.

  IF LevNr:MODIFIED THEN 
    ASSIGN cLevData               = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn;ValKod","WHERE LevNr = " + LevNr:SCREEN-VALUE)
           FI-ValKod:SCREEN-VALUE = ENTRY(2,cLevData,"|")
           .
*/           

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnModell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnModell C-Win
ON CHOOSE OF btnModell IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.
  DEF VAR cLevData AS CHAR NO-UNDO.

  DO WITH FRAME Default-Frame: 
      IF INT(LevNr:SCREEN-VALUE) = 0 OR
         TRIM(LevKod:SCREEN-VALUE) = '' THEN
      DO:
          MESSAGE 'Leverandørnr. og leverandørs artikkelnr. må angis før det kan søkes etter modell.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,50,
                        "ArtBas"
                        + ";ArtikkelNr"
                        + ";LevKod"
                        + ";Beskr"
                        + ";LevFargKod"
                        + ";ModellFarge"
                       ,"WHERE ArtBas.LevNr = '" + LevNr:screen-value + "' AND ArtBas.LevKod = '" + LevKod:SCREEN-VALUE + "'" 
                        ,""                                                  
                        ,"ArtikkelNr,ModellFarge",   /* <- return values for these fields */
                          OUTPUT cReturnValues,
                          OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF bOk AND cReturnValues NE "" THEN DO:
        ASSIGN ModellFarge:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
               ModellFarge:MODIFIED         = TRUE
               .
        /* Init av kalkyle */
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = DEC(ENTRY(2,cReturnValues,"|")) NO-ERROR.
        IF AVAILABLE ArtBas THEN
            FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE ArtPris THEN 
        DO:
            ASSIGN
                FI-ProfilNr:SCREEN-VALUE = STRING(ArtPris.ProfilNr)
                FI-ValPris:SCREEN-VALUE = STRING(ArtPris.ValPris[1])
                FI-InnkjopsPris:SCREEN-VALUE = STRING(ArtPris.InnkjopsPris[1])
                FI-Rab1kr:SCREEN-VALUE = STRING(ArtPris.Rab1Kr[1])
                FI-Rab2kr:SCREEN-VALUE = STRING(ArtPris.Rab2Kr[1])
                FI-Frakt:SCREEN-VALUE = STRING(ArtPris.Frakt[1])
                FI-DivKostKr:SCREEN-VALUE = STRING(ArtPris.DivKostKr[1])
                FI-Rab3kr:SCREEN-VALUE = STRING(ArtPris.Rab3Kr[1])
                FI-VareKost:SCREEN-VALUE = STRING(ArtPris.VareKost[1])
                FI-DBkr:SCREEN-VALUE = STRING(ArtPris.DbKr[1])
                FI-MVAkr:SCREEN-VALUE = STRING(ArtPris.MvaKr[1])
                FI-Rab1%:SCREEN-VALUE = STRING(ArtPris.Rab1%[1])
                FI-Rab2%:SCREEN-VALUE = STRING(ArtPris.Rab2%[1])
                FI-Frakt%:SCREEN-VALUE = STRING(ArtPris.Frakt%[1])
                FI-DivKost%:SCREEN-VALUE = STRING(ArtPris.DivKost%[1])
                FI-Rab3%:SCREEN-VALUE = STRING(ArtPris.Rab3%[1])
                FI-DB%:SCREEN-VALUE = STRING(ArtPris.Db%[1])
                FI-Mva%:SCREEN-VALUE = STRING(ArtPris.Mva%[1])
                FI-Pris:SCREEN-VALUE = STRING(ArtPris.Pris[1])
                FI-EuroPris:SCREEN-VALUE = STRING(ArtPris.EuroPris[1])
                FI-AnbefaltPris:SCREEN-VALUE = STRING(ArtBas.AnbefaltPris)
                Beskr:SCREEN-VALUE = STRING(ArtBas.Beskr)
                BongTekst:SCREEN-VALUE = STRING(ArtBas.BongTekst)
                Vg:SCREEN-VALUE = STRING(ArtBas.Vg)
                VgKat:SCREEN-VALUE = STRING(ArtBas.VgKat)
                StrTypeID:SCREEN-VALUE = STRING(ArtBas.StrTypeId)
                Farg:SCREEN-VALUE = STRING(ArtBas.Farg)
                SaSong:SCREEN-VALUE = STRING(ArtBas.Sasong)
                ProdNr:SCREEN-VALUE = STRING(ArtBas.ProdNr)
                matkod:SCREEN-VALUE = STRING(ArtBas.MatKod)
                .
        END.
        APPLY "ENTRY" TO LevFargKod.
      END.
      ELSE APPLY "ENTRY" TO LevFargKod.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdNr C-Win
ON CHOOSE OF btnProdNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Produsent;ProdNr;Beskrivelse"
                   ,"WHERE TRUE"
                    ,""                                                  
                    ,"ProdNr,Beskrivelse",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN ProdNr:SCREEN-VALUE     = ENTRY(1,cReturnValues,"|")
           ProdBeskrivelse:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    APPLY "entry" TO FI-ProfilNr.
  END.
  ELSE APPLY "entry" TO ProdNr.
/*
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ProdNr;Beskrivelse".

  RUN JBoxDLookup.w ("Produsent;ProdNr;Beskrivelse", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN ProdNr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
           ProdBeskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|").
    APPLY "entry" TO FI-ProfilNr.
  END.
  ELSE APPLY "entry" TO ProdNr.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSasong C-Win
ON CHOOSE OF btnSasong IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Sasong;Sasong;SasBeskr"
                   ,"WHERE TRUE"
                    ,""                                                  
                    ,"Sasong,SasBeskr",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN Sasong:SCREEN-VALUE     = ENTRY(1,cReturnValues,"|")
           SasBeskr:SCREEN-VALUE = ENTRY(2,cReturnValues,"|").
    APPLY "entry" TO matkod.
  END.
  ELSE APPLY "entry" TO SaSong.

/*
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Sasong;SasBeskr".

  RUN JBoxDLookup.w ("Sasong;Sasong;SasBeskr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN Sasong:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
           SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|").
    APPLY "entry" TO matkod.
  END.
  ELSE APPLY "entry" TO SaSong.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Lagre og avslutt */
DO:
  DEF VAR cNyArtikkelNr AS CHAR NO-UNDO.
  DEF VAR cModus AS CHAR NO-UNDO.
  DEF VAR ilVareBehNr AS DEC NO-UNDO.

  APPLY "return" TO FOCUS.
  APPLY "return" TO StrTypeId.
  IF NOT ValidateInput() THEN RETURN NO-APPLY.

  RUN "hentModus"     IN ihParent (OUTPUT cModus)      NO-ERROR.
  RUN "hentVareBehNr" IN ihParent (OUTPUT ilVareBehNr) NO-ERROR.
  
  IF fArtikkelNr = ? THEN
      fArtikkelNr = 0.
  
  IF DYNAMIC-FUNCTION("runproc",
                      "artbas_vedlikehold.p",
                      icAction + "," + STRING(fArtikkelNr) + "," + FI-ProfilNr:SCREEN-VALUE + ',' + STRING(ilVareBehNr) + ',' + cModus,
                      HentVerdier()
                      ) THEN 
  DO:

    IF CAN-DO("new,copy",icAction) THEN DO:
      cNyArtikkelNr = DYNAMIC-FUNCTION("getTransactionMessage").
      RUN "NyArtBas" IN ihParent (DEC(cNyArtikkelNr)).
    END.

    IF tbModell:CHECKED AND 
        bHovedModellExist AND 
        icAction = "copy" THEN 
      DYNAMIC-FUNCTION("DoUpdate","ArtBas","ignore","ArtikkelNr",cNyArtikkelNr,"ModellFarge",cModellFarge,TRUE).

    ELSE IF tbModell:CHECKED AND 
        icAction = "copy" THEN 
    DO:
      DYNAMIC-FUNCTION("DoUpdate","ArtBas","ignore","ArtikkelNr",cNyArtikkelNr,"ModellFarge,HovedModellFarge",STRING(fArtikkelNr) + "|no",TRUE).
      DYNAMIC-FUNCTION("DoUpdate","ArtBas","ignore","ArtikkelNr",STRING(fArtikkelNr),"ModellFarge,HovedModellFarge",STRING(fArtikkelNr) + "|yes",TRUE).
    END.

    ELSE IF NOT tbModell:CHECKED AND icAction = "copy" THEN DO:
        DYNAMIC-FUNCTION("DoUpdate","ArtBas","ignore","ArtikkelNr",cNyArtikkelNr,"ModellFarge,HovedModellFarge","0|no",TRUE).
    END.
    
    obOK = TRUE.
    APPLY "close" TO THIS-PROCEDURE.
  END.
  ELSE
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("GetTransactionMessage"),"Feil","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStr C-Win
ON CHOOSE OF btnStr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR iocColValues AS CHAR NO-UNDO.
  DEFINE VARIABLE iAvd AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHg AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cWhere AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iSaveStrTypeid AS INTEGER    NO-UNDO.
  ASSIGN cWhere = IF CAN-DO("0,2",ArtSlag:SCREEN-VALUE) THEN
                      "strtypeid > 2" 
             ELSE IF CAN-DO("1,3",ArtSlag:SCREEN-VALUE) THEN
                      "strtypeid = 2"
             ELSE "".
  IF NOT CAN-DO("1,3",ArtSlag:SCREEN-VALUE) THEN DO:
      iHg = INT(DYNAMIC-FUNCTION("getFieldList","VarGr;Hg","WHERE Vg = " + Vg:SCREEN-VALUE)).
      IF iHg > 0 THEN
          iAvd = INT(DYNAMIC-FUNCTION("getFieldList","HuvGr;AvdelingNr","WHERE Hg = " + STRING(iHg))).
      IF iAvd > 0 AND iHg > 0 THEN DO:
          IF DYNAMIC-FUNCTION("getFieldValues","StrType","WHERE AvdelingNr = " + STRING(iAvd) + " AND Hg = " + STRING(iHg),"StrTypeId") = ? THEN
              ASSIGN iAvd = 0
                     iHg  = 0.
      END.
  END.
  RUN gstrtype.w (INPUT-OUTPUT iocColValues,
                  "", /* cFelt */
                  "", /* cVerdier */
                  "", /*cStart */
                  iAvd,  /* iAvdelingNr */
                  iHg,   /* iHg */
                  cWhere).


  IF NUM-ENTRIES(RETURN-VALUE) >= 2 THEN
      ASSIGN
      strtypeid:SCREEN-VALUE = ENTRY(2,RETURN-VALUE)
      NO-ERROR.
/*   IF ERROR-STATUS:ERROR THEN                      */
/*       ASSIGN iStrTypeID = INPUT ArtBas.StrTypeID. */
  IF CAN-DO("ENDRE-STRTYPE,NY-STRTYPE",ENTRY(1,RETURN-VALUE)) THEN
  DO:
    iSaveStrTypeid = INPUT strtypeid.
    strtypeid = (IF ENTRY(1,RETURN-VALUE) = "NY-STRTYPE"
                    THEN ?
                    ELSE INPUT strtypeid).
    RUN d-strtype.w (INPUT-OUTPUT strtypeid,
                     iAvd,
                     iHg).
    IF ENTRY(1,RETURN-VALUE) = "AVBRYT" THEN 
    DO:
        strtypeid:SCREEN-VALUE = STRING(isavestrtypeid).
        APPLY "ENTRY" TO strtypeid.
        APPLY "tab" TO strtypeid.
        RETURN NO-APPLY.
    END.
    ELSE
        strtypeid:SCREEN-VALUE = STRING(strtypeid).
  END.
  ELSE DO:
      IF RETURN-VALUE = "AVBRYT" THEN
          .
      ELSE IF NUM-ENTRIES(iocColValues,CHR(1)) >= 3 THEN
      DO:
        ASSIGN strtypeid:SCREEN-VALUE = ENTRY(2,iocColValues,CHR(1)).
      END.
  END.
  APPLY "tab" TO strtypeid.
  RETURN NO-APPLY.
/*   DEF VAR cLookupValue AS CHAR NO-UNDO.                                      */
/*                                                                              */
/*   cLookupValue = "strtypeid;Beskrivelse".                                    */
/*                                                                              */
/*   RUN JBoxDLookup.w ("StrType;strtypeid;Beskrivelse;KortNavn;Alfafordeling", */
/*                      "WHERE strtypeid > 1",                                  */
/*                      INPUT-OUTPUT cLookupValue).                             */
/*                                                                              */
/*   IF cLookupValue NE "" THEN                                                 */
/*     ASSIGN strtypeid:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")              */
/*            Beskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")              */
/*            .                                                                 */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr C-Win
ON CHOOSE OF btnVarGr IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEF VAR cTekst AS CHAR NO-UNDO.
    cTekst = "Vg".
    /*
    RUN JBoxDLookup.w (
                       "VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn",
                       "WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK",
                       INPUT-OUTPUT cTekst).
    */
    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn"
                     ,"WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK"
                      ,""                                                  
                      ,"Vg",   /* <- return values for these fields */
                        OUTPUT cTekst,
                        OUTPUT bOK).

    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    FIND VarGr NO-LOCK WHERE
      VarGr.Vg = INT(cTekst) NO-ERROR.
    IF AVAILABLE VarGr THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          vg:SCREEN-VALUE      = cTekst.
        APPLY "tab" TO vg.
    END.
    ELSE DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          vg:SCREEN-VALUE  = ''.
    END.

  /*
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn",
                    "WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK"
                    ,""
                    ,"VgBeskr,Vg",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       vg:SCREEN-VALUE = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "tab" TO vg.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokProfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokProfil C-Win
ON CHOOSE OF BUTTON-SokProfil IN FRAME DEFAULT-FRAME /* ... */
OR "F10":U OF FI-ProfilNr
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "PrisProfil;ProfilNr;KortNavn;Beskrivelse"
                   ,"WHERE TRUE"
                    ,""                                                  
                    ,"ProfilNr,Beskrivelse",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN FI-ProfilNR:SCREEN-VALUE    = ENTRY(1,cReturnValues,"|")
           FI-Beskrivelse:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           .
    APPLY "return" TO FI-ProfilNr.
  END.
  ELSE APPLY "ENTRY" TO FI-ProfilNr.

/*
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ProfilNr;Beskrivelse".

  RUN JBoxDLookup.w ("PrisProfil;ProfilNr;KortNavn;Beskrivelse", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN FI-ProfilNR:SCREEN-VALUE    = ENTRY(1,cLookupValue,"|")
           FI-Beskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           .
    APPLY "return" TO FI-ProfilNr.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Farg C-Win
ON F10 OF Farg IN FRAME DEFAULT-FRAME /* Fargekode */
DO:
  APPLY "choose" TO btnFarge.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Farg C-Win
ON RETURN OF Farg IN FRAME DEFAULT-FRAME /* Fargekode */
OR TAB OF Farg DO:
  IF Farg:MODIFIED THEN 
    FarBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Farg;FarBeskr","WHERE Farg = " + Farg:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-AnbefaltPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-AnbefaltPris C-Win
ON LEAVE OF FI-AnbefaltPris IN FRAME DEFAULT-FRAME /* Veil.pris */
DO:
  IF FI-AnbefaltPris:MODIFIED AND DEC(FI-Pris:SCREEN-VALUE) = 0 THEN
    FI-Pris:SCREEN-VALUE = FI-AnbefaltPris:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DB%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DB% C-Win
ON LEAVE OF FI-DB% IN FRAME DEFAULT-FRAME
DO:
  IF INPUT FI-DB% > 99.99 THEN
    DO:
      MESSAGE "Du kan ikke ha mer enn 99.99 i DB%!" 
              VIEW-AS ALERT-BOX TITLE "Kalkulasjonsfeil".
      RETURN NO-APPLY.
    END.
  IF ROUND(INPUT FI-DB%,2) <> round(dec(ENTRY(15,wSjekkStreng,";")),2) THEN
    DO:
      RUN Kalkulasjon (1).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DBkr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DBkr C-Win
ON LEAVE OF FI-DBkr IN FRAME DEFAULT-FRAME /* DB (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DivKost%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DivKost% C-Win
ON LEAVE OF FI-DivKost% IN FRAME DEFAULT-FRAME
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DivKostKr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DivKostKr C-Win
ON LEAVE OF FI-DivKostKr IN FRAME DEFAULT-FRAME /* Divkost(+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-EuroPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EuroPris C-Win
ON TAB OF FI-EuroPris IN FRAME DEFAULT-FRAME /* Pris(Euro) */
OR "RETURN":U OF FI-EuroPris
DO:
  APPLY "Entry":U TO FI-ValPris IN FRAME {&FRAME-NAME}.  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Frakt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Frakt C-Win
ON LEAVE OF FI-Frakt IN FRAME DEFAULT-FRAME /* Frakt (+) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Frakt%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Frakt% C-Win
ON LEAVE OF FI-Frakt% IN FRAME DEFAULT-FRAME
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris C-Win
ON LEAVE OF FI-Pris IN FRAME DEFAULT-FRAME /* Markedspris */
DO:
  IF wSjekkStreng = "" OR INPUT FI-Pris <> dec(ENTRY(18,wSjekkStreng,";")) THEN
    DO:
      RUN Kalkulasjon (1).
      /*
      apply "ENTRY":U to FI-DB% in frame Dialog-Frame.
      return no-apply.
      */
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ProfilNr C-Win
ON TAB OF FI-ProfilNr IN FRAME DEFAULT-FRAME /* Prisprofil */
OR "RETURN":U OF FI-ProfilNr 
DO:
  IF FI-ProfilNr:MODIFIED THEN DO:
     FI-Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","PrisProfil;Beskrivelse","WHERE ProfilNr = " + FI-ProfilNr:SCREEN-VALUE).
     RUN Initkalkyle.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab1%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab1% C-Win
ON LEAVE OF FI-Rab1% IN FRAME DEFAULT-FRAME
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab1kr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab1kr C-Win
ON LEAVE OF FI-Rab1kr IN FRAME DEFAULT-FRAME /* Rab 1 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab2%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab2% C-Win
ON LEAVE OF FI-Rab2% IN FRAME DEFAULT-FRAME
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab2kr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab2kr C-Win
ON LEAVE OF FI-Rab2kr IN FRAME DEFAULT-FRAME /* Rab 2 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab3%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab3% C-Win
ON LEAVE OF FI-Rab3% IN FRAME DEFAULT-FRAME
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Rab3kr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Rab3kr C-Win
ON LEAVE OF FI-Rab3kr IN FRAME DEFAULT-FRAME /* Rab 3 (-) */
DO:
  RUN Kalkulasjon (1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ValPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ValPris C-Win
ON LEAVE OF FI-ValPris IN FRAME DEFAULT-FRAME /* Valutapris */
DO:
  RUN Kalkulasjon (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr C-Win
ON ANY-PRINTABLE OF LevNr IN FRAME DEFAULT-FRAME /* Lev.nr */
DO:
  ASSIGN
      LevNr:MODIFIED = TRUE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr C-Win
ON F10 OF LevNr IN FRAME DEFAULT-FRAME /* Lev.nr */
OR F3 OF LevNr DO:
  APPLY "choose" TO btnLev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr C-Win
ON RETURN OF LevNr IN FRAME DEFAULT-FRAME /* Lev.nr */
OR TAB OF LevNr DO:
  DEF VAR cLevData AS CHAR NO-UNDO.
  IF LevNr:MODIFIED THEN 
    ASSIGN cLevData               = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn;ValKod","WHERE LevNr = " + LevNr:SCREEN-VALUE)
           LevNamn:SCREEN-VALUE   = ENTRY(1,cLevData,"|")
           FI-ValKod:SCREEN-VALUE = ENTRY(2,cLevData,"|")
           NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME matkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL matkod C-Win
ON RETURN OF matkod IN FRAME DEFAULT-FRAME /* Materiale */
DO:
  APPLY "tab" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProdNr C-Win
ON F10 OF ProdNr IN FRAME DEFAULT-FRAME /* Produsent */
DO:
  APPLY "choose" TO btnProdNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SaSong C-Win
ON F10 OF SaSong IN FRAME DEFAULT-FRAME /* Sesong */
DO:
  APPLY "choose" TO btnSasong.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SaSong C-Win
ON RETURN OF SaSong IN FRAME DEFAULT-FRAME /* Sesong */
OR TAB OF SaSong DO:
  IF SaSong:MODIFIED THEN 
    SasBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","SaSong;SasBeskr","WHERE SaSong = " + SaSong:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME StrTypeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StrTypeID C-Win
ON F10 OF StrTypeID IN FRAME DEFAULT-FRAME /* Str.type */
DO:
  APPLY "choose" TO btnStr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StrTypeID C-Win
ON RETURN OF StrTypeID IN FRAME DEFAULT-FRAME /* Str.type */
OR TAB OF StrTypeId DO:
    IF SELF:SCREEN-VALUE = "1" THEN DO:
        BELL.
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
    END.
    IF SELF:SCREEN-VALUE = "2" AND NOT CAN-DO("0,1,2,3",ArtSlag:SCREEN-VALUE) THEN
        ArtSlag:SCREEN-VALUE = "0".
    ELSE IF SELF:SCREEN-VALUE <> "0" AND DYNAMIC-FUNCTION("getFieldList","StrType;StrTypeId","WHERE StrTypeId = " + StrTypeId:SCREEN-VALUE) = "" THEN DO:
        BELL.
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
    END.
    ELSE IF INT(SELF:SCREEN-VALUE) > 2 AND NOT CAN-DO("0,2",ArtSlag:SCREEN-VALUE) THEN
        ArtSlag:SCREEN-VALUE = "0".
  IF StrTypeId:MODIFIED THEN 
    Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","StrType;Beskrivelse","WHERE StrTypeId = " + StrTypeId:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Manuel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Manuel C-Win
ON RETURN OF T-Manuel IN FRAME DEFAULT-FRAME /* Manuel */
OR "tab":U OF T-Manuel
DO:
  IF INPUT T-Manuel = FALSE THEN
    APPLY "Entry":U TO FI-ValPris IN FRAME {&FRAME-NAME}.
  ELSE
    APPLY "Entry":U TO FI-EuroPris IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Manuel C-Win
ON VALUE-CHANGED OF T-Manuel IN FRAME DEFAULT-FRAME /* Manuel */
DO:
  IF INPUT T-Manuel = FALSE THEN
    ASSIGN
      FI-EuroPris:sensitive = FALSE.
  ELSE
    ASSIGN
      FI-EuroPris:sensitive = TRUE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbFullKalk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbFullKalk C-Win
ON VALUE-CHANGED OF tbFullKalk IN FRAME DEFAULT-FRAME /* Rediger alle kalkylefelter */
DO:
  IF tbFullKalk:CHECKED THEN 
    ENABLE {&List-1} WITH FRAME {&FRAME-NAME}.
  ELSE DISABLE {&List-1} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Vg C-Win
ON F10 OF Vg IN FRAME DEFAULT-FRAME /* Varegr */
DO:
  APPLY "choose" TO btnVarGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Vg C-Win
ON RETURN OF Vg IN FRAME DEFAULT-FRAME /* Varegr */
OR TAB OF Vg DO:
  DEF VAR cVGdata AS CHAR NO-UNDO.

  DEF VAR cVgKat AS CHAR NO-UNDO.

  IF Vg:MODIFIED THEN DO:
    cVGdata               = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr;MomsKod","WHERE Vg = " + Vg:SCREEN-VALUE).
    IF cVgData = "" THEN DO:
      MESSAGE "Ugyldig varegruppe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.

    FOR EACH VgKat NO-LOCK WHERE
        VgKat.Vg = int(Vg:SCREEN-VALUE),
        EACH Kategori OF VgKat NO-LOCK:

        ASSIGN
            cVgKat = cVgKat + 
                     (IF cVgKat <> "" THEN "|" ELSE "") + 
                     Kategori.Beskrivelse + "|" + string(Kategori.KatNr).
    END.

    ASSIGN VgBeskr:SCREEN-VALUE  = ENTRY(1,cVGdata,"|")
           /*
           cVgKat                = DYNAMIC-FUNCTION("getFieldList","VgKat;!VgKat,Kategori;KatNr|Beskrivelse;KatNr",
                                                    "WHERE VgKat.Vg = " + Vg:SCREEN-VALUE + ",FIRST Kategori OF VgKat NO-LOCK")
           */                                         
           fMomsProc             = DEC(DYNAMIC-FUNCTION("getFieldList","Moms;MomsProc","WHERE MomsKod = " + ENTRY(2,cVGdata,"|")))
           FI-Mva%:SCREEN-VALUE  = STRING(fMomsProc)
           VgKat:LIST-ITEM-PAIRS = cVgKat
           VgKat:SCREEN-VALUE    = ENTRY(2,VgKat:LIST-ITEM-PAIRS,"|")
           .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VgKat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VgKat C-Win
ON RETURN OF VgKat IN FRAME DEFAULT-FRAME /* Kategori */
DO:
  APPLY "tab" TO SELF.
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
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

ON 'alt-l':U OF FRAME {&FRAME-NAME} ANYWHERE
  APPLY "choose" TO btnSave.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  APPLY "VALUE-CHANGED" TO ArtSlag.
  
  CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY ModellFarge FI-Valkod ArtSlag LevNr LevKod LevFargKod cmbModell 
          tbModell Beskr Vg VgKat StrTypeID Farg SaSong ProdNr matkod 
          FI-ProfilNr FI-ValPris FI-InnkjopsPris FI-Rab1kr FI-Rab1% FI-Rab2kr 
          FI-Rab2% FI-Frakt FI-Frakt% FI-DivKostKr FI-DivKost% FI-Rab3kr 
          FI-Rab3% FI-VareKost FI-DBkr FI-DB% FI-MVAkr FI-Mva% FI-AnbefaltPris 
          FI-Pris FI-EuroPris T-Manuel tbFullKalk FarBeskr SasBeskr levnamn 
          vgbeskr Beskrivelse FI-Beskrivelse BongTekst ProdBeskrivelse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-Valkod btnCancel btnSave ArtSlag LevNr LevKod LevFargKod cmbModell 
         tbModell Beskr Vg VgKat StrTypeID Farg SaSong BUTTON-SokProfil ProdNr 
         matkod FI-ProfilNr FI-ValPris FI-InnkjopsPris FI-Rab1kr FI-Rab1% 
         FI-Rab2kr FI-Rab2% FI-Frakt FI-Frakt% FI-DivKostKr FI-DivKost% 
         FI-Rab3kr FI-Rab3% FI-VareKost FI-DBkr FI-DB% FI-MVAkr FI-Mva% 
         btnFarge FI-AnbefaltPris FI-Pris FI-EuroPris T-Manuel btnSasong btnLev 
         btnVarGr btnStr tbFullKalk FarBeskr SasBeskr levnamn vgbeskr 
         Beskrivelse BongTekst btnProdNr btnModell RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKalkyle C-Win 
PROCEDURE InitKalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wFeltNr    AS INT  NO-UNDO.
DEF VAR wSkjerm    AS CHAR NO-UNDO.
DEF VAR wTilbud    AS LOG  INITIAL FALSE NO-UNDO.

IF icAction = "new" THEN RETURN.
ELSE DO WITH FRAME {&FRAME-NAME}: 
  cFieldValues = DYNAMIC-FUNCTION("getFieldValues","ArtPris","WHERE Artikkelnr = " + STRING(fArtikkelnr) + " AND ProfilNr = " + FI-ProfilNr:SCREEN-VALUE,
                                  "ValPris;1"
                                  + ",InnkjopsPris;1"
                                  + ",Rab1Kr;1"
                                  + ",Rab1%;1"
                                  + ",Rab2Kr;1"
                                  + ",Rab2%;1"
                                  + ",Frakt;1"
                                  + ",Frakt%;1"
                                  + ",DivKostKr;1"
                                  + ",DivKost%;1"
                                  + ",Rab3Kr;1"
                                  + ",Rab3%;1"
                                  + ",Varekost;1"
                                  + ",DBkr;1"
                                  + ",DB%;1"
                                  + ",MVAkr;1"
                                  + ",MVA%;1"
                                  + ",Pris;1"
                                  + ",EuroPris;1"
                                  + ",EuroManuel"
                                  ).
  IF cFieldValues NE ? THEN
    ASSIGN FI-ValPris:SCREEN-VALUE      = ENTRY(1,cFieldValues,"|")
           FI-InnkjopsPris:SCREEN-VALUE = ENTRY(2,cFieldValues,"|")
           FI-Rab1kr:SCREEN-VALUE       = ENTRY(3,cFieldValues,"|")
           FI-Rab1%:SCREEN-VALUE        = ENTRY(4,cFieldValues,"|")
           FI-Rab2kr:SCREEN-VALUE       = ENTRY(5,cFieldValues,"|")
           FI-Rab2%:SCREEN-VALUE        = ENTRY(6,cFieldValues,"|")
           FI-Frakt:SCREEN-VALUE        = ENTRY(7,cFieldValues,"|")
           FI-Frakt%:SCREEN-VALUE       = ENTRY(8,cFieldValues,"|")
           FI-DivKostKr:SCREEN-VALUE    = ENTRY(9,cFieldValues,"|")
           FI-DivKost%:SCREEN-VALUE     = ENTRY(10,cFieldValues,"|")
           FI-Rab3kr:SCREEN-VALUE       = ENTRY(11,cFieldValues,"|")
           FI-Rab3%:SCREEN-VALUE        = ENTRY(12,cFieldValues,"|")
           FI-Varekost:SCREEN-VALUE     = ENTRY(13,cFieldValues,"|")
           FI-DBkr:SCREEN-VALUE         = ENTRY(14,cFieldValues,"|")
           FI-DB%:SCREEN-VALUE          = ENTRY(15,cFieldValues,"|")
           FI-MVAkr:SCREEN-VALUE        = ENTRY(16,cFieldValues,"|")
           FI-MVA%:SCREEN-VALUE         = ENTRY(17,cFieldValues,"|")
           FI-Pris:SCREEN-VALUE         = ENTRY(18,cFieldValues,"|")
           FI-EuroPris:SCREEN-VALUE     = ENTRY(19,cFieldValues,"|")
           T-Manuel:SCREEN-VALUE        = ENTRY(20,cFieldValues,"|")
           .
  ELSE
    ASSIGN FI-ValPris:screen-value  = ""
           FI-InnkjopsPris:screen-value  = ""
           FI-Rab1Kr:screen-value     = ""
           FI-Rab1%:screen-value    = ""
           FI-Rab2Kr:screen-value     = ""
           FI-Rab2%:screen-value    = ""
           FI-Frakt:screen-value    = ""
           FI-Frakt%:screen-value   = ""
           FI-DivKostKr:screen-value  = ""
           FI-DivKost%:screen-value = ""
           FI-Rab3Kr:screen-value     = ""
           FI-Rab3%:screen-value    = ""
           FI-VareKost:screen-value = ""
           FI-MvaKr:screen-value      = ""
           FI-Mva%:screen-value     = ""
           FI-DBkr:screen-value       = ""
           FI-DB%:screen-value      = ""
           FI-Pris:screen-value     = ""
           FI-EuroPris:screen-value   = ""
           T-Manuel:SCREEN-VALUE    = STRING(FALSE)
           .

  IF T-Manuel:CHECKED THEN
    FI-EuroPris:SENSITIVE = TRUE.
  ELSE 
    FI-EuroPris:SENSITIVE = FALSE.

END. /* FrameScoop */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCL            AS CHAR NO-UNDO.
DEF VAR cVgKat         AS CHAR NO-UNDO.
DEF VAR cArtSlag       AS CHAR NO-UNDO.
DEF VAR cArtSlagUnntak AS CHAR NO-UNDO.
DEF VAR cArtSlagOK     AS CHAR NO-UNDO.
DEF VAR cTekst         AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  
  ASSIGN fArtikkelNr = DYNAMIC-FUNCTION("getArtNr" IN ihParent) NO-ERROR.
  IF fArtikkelNr = 0 THEN
      ASSIGN fArtikkelNr = DYNAMIC-FUNCTION("getActivArtNr" IN ihParent) NO-ERROR. /* Fra artikkelkortet faghandel */

  ASSIGN ArtSlag:DELIMITER = "|"
         matkod:DELIMITER = "|"
         VgKat:DELIMITER = "|"
         cmbModell:DELIMITER = "|"
         /*FI-ValKod:DELIMITER = "|"*/
         cArtSlagUnntak  = DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1","WHERE SysHId = 2 and SysGr = 8")
         cArtSlag = DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1","WHERE SysHId = 2 and SysGr = 7")
         matkod:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Material;Matkod|MatBeskr;Matkod","WHERE true")
         cCL                    = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                                   "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")
         FI-ProfilNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                                                   "WHERE Butik = " + cCL,"ProfilNr")
         /*
         FI-ValKod:LIST-ITEMS = "|" + DYNAMIC-FUNCTION("getFieldList","Valuta;Valkod","WHERE true")
         */
         .
  DO ix = 2 TO NUM-ENTRIES(cArtSlag,"|") BY 2:
    IF LOOKUP(ENTRY(ix,cArtSlag,"|"),cArtSlagUnntak,"|") = 0 THEN
      cArtSlagOK = cArtSlagOK + ENTRY(ix - 1,cArtSlag,"|") + "|" + ENTRY(ix ,cArtSlag,"|") + "|".
  END.
  ArtSlag:LIST-ITEM-PAIRS = TRIM(cArtSlagOK,"|").

  IF icAction NE "new" THEN DO:
    ASSIGN 
         cFieldValues = DYNAMIC-FUNCTION("getFieldValues","Artbas","WHERE Artikkelnr = " + STRING(fArtikkelnr),
                                         "Levnr,LevKod,LevFargKod,Beskr,Vg,StrTypeID,MatKod,Sasong,StrTypeId,Farg,ValKod,VgKat,ArtSlag")
         Levnr:SCREEN-VALUE = ENTRY(1,cFieldValues,"|")
         LevKod:SCREEN-VALUE = ENTRY(2,cFieldValues,"|")
         LevFargKod:SCREEN-VALUE = ENTRY(3,cFieldValues,"|")
         Beskr:SCREEN-VALUE = ENTRY(4,cFieldValues,"|")
         Vg:SCREEN-VALUE = ENTRY(5,cFieldValues,"|")
         StrTypeId:SCREEN-VALUE = ENTRY(6,cFieldValues,"|")
         MatKod:SCREEN-VALUE = ENTRY(7,cFieldValues,"|")
         Sasong:SCREEN-VALUE = ENTRY(8,cFieldValues,"|")
         StrTypeId:SCREEN-VALUE = ENTRY(9,cFieldValues,"|")
         Farg:SCREEN-VALUE = ENTRY(10,cFieldValues,"|")
         FI-ValKod:SCREEN-VALUE = ENTRY(11,cFieldValues,"|")

         levnamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE levnr = " + Levnr:SCREEN-VALUE,"levnamn")
         vgbeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","VarGr","WHERE Vg = " + Vg:SCREEN-VALUE,"VgBeskr")
         Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","StrType","WHERE StrTypeId = " + StrTypeId:SCREEN-VALUE,"Beskrivelse")
         ArtSlag:SCREEN-VALUE = ENTRY(13,cFieldValues,"|")
         ArtSlag:SENSITIVE = FALSE
         FarBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Farg","WHERE Farg = " + Farg:SCREEN-VALUE,"FarBeskr")
         SasBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Sasong","WHERE Sasong = " + Sasong:SCREEN-VALUE,"SasBeskr")
         .
         
    FOR EACH VgKat NO-LOCK WHERE
        VgKat.Vg = int(Vg:SCREEN-VALUE),
        EACH Kategori OF VgKat NO-LOCK:

        ASSIGN
            cTekst = cTekst + 
                     (IF cTekst <> "" THEN "|" ELSE "") + 
                     Kategori.Beskrivelse + "|" + string(Kategori.KatNr).
    END.
     ASSIGN
         /*
          VgKat:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","VgKat;KatNr,Kategori;Beskrivelse",
                                                   "WHERE VgKat.Vg = " + Vg:SCREEN-VALUE + ",FIRST Kategori OF VgKat NO-LOCK")
         */                                          
          VgKat:LIST-ITEM-PAIRS = cTekst
          VgKat:SCREEN-VALUE = ENTRY(12,cFieldValues,"|")
          cVgKat = ENTRY(12,cFieldValues,"|").

    RUN InitKalkyle.
    IF icAction = "copy" THEN DO:
      cModellFarge = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtikkelNr),"ModellFarge").
      IF cModellFarge = "0" OR cModellFarge = ? THEN
        cmbModell:LIST-ITEM-PAIRS = LevFargKod:SCREEN-VALUE + "|" + STRING(fArtikkelNr).
      ELSE
        ASSIGN cmbModell:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","ArtBas;LevKod|LevFargKod;ArtikkelNr","WHERE ArtBas.ModellFarge = " + cModellFarge)
               bHovedModellExist         = TRUE
               .
    END.
    ELSE ASSIGN cmbModell:SENSITIVE   = FALSE
                tbModell:SCREEN-VALUE = STRING(FALSE)
                tbModell:SENSITIVE    = FALSE
                .
    APPLY "tab" TO Vg.
  END.
  ELSE DO:
    ASSIGN cmbModell:SENSITIVE    = FALSE
           tbModell:SCREEN-VALUE  = STRING(FALSE)
           tbModell:SENSITIVE     = FALSE
           FI-ValKod:SCREEN-VALUE = "  "
           T-Manuel:SCREEN-VALUE  = STRING(TRUE)
           FI-EuroPris:SENSITIVE  = FALSE
           ArtSlag:SCREEN-VALUE   = ENTRY(2,ArtSlag:LIST-ITEM-PAIRS,"|")
           .
    LevNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getLevNr" IN ihParent) NO-ERROR.
    IF LevNr:SCREEN-VALUE NE "0" THEN APPLY "return" TO LevNr.
    LevNr:MODIFIED = FALSE.
  END.

  fValKurs = DEC(DYNAMIC-FUNCTION("getFieldValues","Valuta","WHERE ValKod = '" + 
                                   (IF FI-ValKod:SCREEN-VALUE = ? THEN "" ELSE FI-ValKod:SCREEN-VALUE) + "'","ValKurs")).
  APPLY "tab" TO FI-ProfilNr.
  APPLY "tab" TO Farg.

  CASE icAction:
    WHEN "new" THEN THIS-PROCEDURE:CURRENT-WINDOW:TITLE  = "Registrer ny artikkel".
    WHEN "copy" THEN THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Kopier fra artikkel (" + STRING(fArtikkelNr) + ")".
    WHEN "edit" THEN THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Endre artikkel (" + STRING(fArtikkelNr) + ")".
  END CASE.

  DISABLE {&List-1}.

  ASSIGN
      FI-ValKod:SENSITIVE = FALSE
      ModellFarge:SENSITIVE = FALSE
      .
  IF icAction = "new" THEN
    APPLY "entry" TO ArtSlag.
  ELSE 
    APPLY "entry" TO LevNr.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon C-Win 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wSetNr AS INT NO-UNDO. /* 1 - Kalkulasjonsramme          */
                                             /* 2 - Visningsramme (Høyre side) */

  DEF VAR wFeltListe AS CHAR   NO-UNDO.
  DEF VAR wFraFelt   AS CHAR   NO-UNDO.
  DEF VAR wFeltNr    AS INT    NO-UNDO.
  DEF VAR wSkjerm    AS CHAR   NO-UNDO.
  DEF VAR h_PrisKo   AS HANDLE NO-UNDO.

  ASSIGN
    wFraFelt   = frame-field
    wFeltListe = "ValPris,InnkjopsPris,Rab1Kr,Rab1%,Rab2Kr,Rab2%,Frakt,Frakt%," + 
                 "DivKostKr,DivKost%,Rab3Kr,Rab3%,VareKost,DBkr,DB%," +
                 "FI-Mva,FI-Mva%,Pris,EUro-Pris".
                 
  /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
  CASE wSetNr:
    WHEN 1 THEN ASSIGN 
                   wFraFelt = SUBSTRING(wFraFelt,4).
    WHEN 2 THEN ASSIGN 
                   wFraFelt = SUBSTRING(wFraFelt,4)
                   wFraFelt = SUBSTRING(wFraFelt,LENGTH(wFraFelt) - 2).
  END CASE. 
  ASSIGN
    wFeltNr = LOOKUP(wFraFelt,wFeltListe).
    
  /* Ukjent felt. */  
  IF wFeltNr = 0 THEN
    DO:
      MESSAGE "Ukjent felt!" VIEW-AS ALERT-BOX TITLE "Kalkylefeil".
      RETURN NO-APPLY.  
    END.

  FRAME-SCOOPE:
  DO WITH FRAME {&FRAME-NAME}:

    fValKurs = DEC(DYNAMIC-FUNCTION("getFieldValues","Valuta","WHERE ValKod = '" + 
                                   (IF FI-ValKod:SCREEN-VALUE = ? THEN "" ELSE FI-ValKod:SCREEN-VALUE) + "'","ValKurs")).
  

    /* Pakker ned verdiene som ligger i skjermen. */
    ASSIGN
      wSkjerm = KalkStreng().

/*     MESSAGE PROGRAM-NAME(1) SKIP          */
/*             "wSkjerm: " wSkjerm SKIP      */
/*             "fMomsProc: " fMomsProc SKIP  */
/*             "fValKurs " fValKurs SKIP     */
/*             "wFeltNr: " wFeltNr           */
/*             VIEW-AS ALERT-BOX.            */

    /* Det skal ikke skje noe ved tabbing mellom feltene                 */
    /* Gambler her på at det ikke kommer to artikkler med samme kalkyle. */
    /* Hvis så om atte, så gjør det ikke noe.                            */
    IF (wSjekkStreng <> wSkjerm) THEN
      DO:
        ASSIGN wSjekkStreng = wSkjerm.
      END.
    ELSE 
      RETURN NO-APPLY.

   IF NOT VALID-HANDLE(h_PrisKo) THEN
     RUN prisko.p PERSISTENT SET h_PrisKo.

   /* Starter omkalkulering.                         */
   /* NB: Kalkulasjonen skjer i prosedyrebilboteket. */
   IF VALID-HANDLE(h_PrisKo) THEN
     RUN Omregning IN h_PrisKo
          (INPUT ?, 
           INPUT INT(FI-ProfilNr:SCREEN-VALUE),
           INPUT-OUTPUT wSkjerm,
           INPUT fMomsProc,
           INPUT fValKurs, 
           INPUT wFeltNr,
           INPUT FALSE).
    ELSE 
      MESSAGE "Prosedyrebiblotek er ikke startet!" VIEW-AS ALERT-BOX.
                  
    IF VALID-HANDLE(h_PrisKo) THEN
        DELETE PROCEDURE h_PrisKo.

    /* Legger nye verier opp på skjermen igjen. */
    ASSIGN
      FI-ValPris:screen-value       = ENTRY(1,wSkjerm,";")
      FI-InnkjopsPris:screen-value  = ENTRY(2,wSkjerm,";")
      FI-Rab1Kr:screen-value        = ENTRY(3,wSkjerm,";")
      FI-Rab1%:screen-value         = ENTRY(4,wSkjerm,";")
      FI-Rab2Kr:screen-value        = ENTRY(5,wSkjerm,";")
      FI-Rab2%:screen-value         = ENTRY(6,wSkjerm,";")
      FI-Frakt:screen-value         = ENTRY(7,wSkjerm,";")
      FI-Frakt%:screen-value        = ENTRY(8,wSkjerm,";")
      FI-DivKostKr:screen-value     = ENTRY(9,wSkjerm,";")
      FI-DivKost%:screen-value      = ENTRY(10,wSkjerm,";")
      FI-Rab3Kr:screen-value        = ENTRY(11,wSkjerm,";")
      FI-Rab3%:screen-value         = ENTRY(12,wSkjerm,";")
      FI-VareKost:screen-value      = ENTRY(13,wSkjerm,";")
      FI-MvaKr:screen-value         = ENTRY(14,wSkjerm,";")
      FI-Mva%:screen-value          = ENTRY(15,wSkjerm,";")
      FI-DBkr:screen-value          = ENTRY(16,wSkjerm,";")
      FI-DB%:screen-value           = ENTRY(17,wSkjerm,";")
      FI-Pris:screen-value          = ENTRY(18,wSkjerm,";")
      FI-EUroPris:screen-value      = ENTRY(19,wSkjerm,";").
  END. /* FRAME-SCOOPE */

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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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

DEF VAR hCombo AS HANDLE NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "VarGr" THEN DO:


    CREATE COMBO-BOX hCombo 
      ASSIGN DELIMITER        = "|"
             DATA-TYPE        = "CHARACTER"
             FORMAT           = "x(256)"
             NAME             = "cmbHgr"
             SUBTYPE          = "DROP-DOWN-LIST"
             LIST-ITEM-PAIRS  = "<Velg hovedgruppe>|0|" + DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr;Hg","WHERE TRUE BY HgBeskr")
             INNER-LINES      = 50
             FRAME            = ihBrowse:FRAME
             X                = ihBrowse:X + 150
             Y                = ihBrowse:Y - 26
             WIDTH-PIXELS     = 300
             VISIBLE          = YES
             SENSITIVE        = TRUE
             HELP             = "Velg hovedgruppe"
             TOOLTIP          = "Velg hovedgruppe"
             TRIGGERS:
               ON VALUE-CHANGED PERSISTENT RUN setVgLookupFilter IN THIS-PROCEDURE (ihBrowse,hCombo).
/*                ON TAB           PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
/*                ON BACK-TAB      PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
/*                ON RETURN        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").     */
/*                ON ENTRY         PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"entry").     */
/*                ON END-ERROR     PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"end-error"). */
             END TRIGGERS.
      ASSIGN hCombo:SCREEN-VALUE = '0'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVgLookupFilter C-Win 
PROCEDURE setVgLookupFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCombo  AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",ihBrowse,"queryFilter",
                 IF ihCombo:SCREEN-VALUE NE ? AND ihCombo:SCREEN-VALUE NE "" THEN 
                   " AND Hg = " + ihCombo:SCREEN-VALUE
                 ELSE "").

RUN InvokeMethod(ihBrowse,"OpenQuery").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField AS HANDLE NO-UNDO.

EMPTY TEMP-TABLE ttVerdier.
hField = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.

REPEAT WHILE VALID-HANDLE(hField) WITH FRAME {&FRAME-NAME}:
  IF CAN-DO("combo-box,fill-in",hField:TYPE) AND hField:NAME NE "cmbModell" THEN DO:
    CREATE ttVerdier.
    ASSIGN ttVerdier.cNavn  = IF hField:NAME BEGINS "FI-" THEN SUBSTR(hField:NAME,4)
                              ELSE IF hField:NAME BEGINS "T-" THEN SUBSTR(hField:NAME,3)
                              ELSE hField:NAME
           ttVerdier.cVerdi = hField:INPUT-VALUE
           .
  END.
  ELSE IF hField:NAME = "tbModell" AND tbModell:CHECKED THEN DO:
    CREATE ttVerdier.
    ASSIGN ttVerdier.cNavn  = "ModellFarge"
           ttVerdier.cVerdi = STRING(fArtikkelNr)
           .
  END.

  hField = hField:NEXT-SIBLING.
END.

RETURN httVerdier.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng C-Win 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wTekst AS CHAR NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    wTekst =    STRING(INPUT FI-ValPris) + ";" +
                string(INPUT FI-InnkjopsPris) + ";" +
                string(INPUT FI-Rab1Kr) + ";" +
                string(INPUT FI-Rab1%) + ";" +
                string(INPUT FI-Rab2Kr) + ";" +
                string(INPUT FI-Rab2%) + ";" +
                string(INPUT FI-Frakt) + ";" +
                string(INPUT FI-Frakt%) + ";" +
                string(INPUT FI-DivKostKr) + ";" +
                string(INPUT FI-DivKost%) + ";" +
                string(INPUT FI-Rab3Kr) + ";" +
                string(INPUT FI-Rab3%) + ";" +
                string(INPUT FI-VareKost) + ";" +
                string(INPUT FI-MvaKr) + ";" +
                string(INPUT FI-Mva%) + ";" +
                string(INPUT FI-DBkr) + ";" +
                string(INPUT FI-DB%) + ";" +
                string(INPUT FI-Pris) + ";" +
                string(INPUT FI-EUroPris) + ";" +
                (IF INPUT T-Manuel = TRUE
                   THEN "True"
                   ELSE "False") + ";" +
                string(TODAY) + ";" +
                string(TODAY) + ";" +
                "0"  + ";" +
                "0"  + ";" +
                "false".
  
  END.
  
  RETURN wTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateInput C-Win 
FUNCTION ValidateInput RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF Beskr:SCREEN-VALUE = "" THEN DO: /* Mer kontroll på serveren */
          MESSAGE "Registrer beskrivelse"
                  VIEW-AS ALERT-BOX ERROR.
          APPLY "ENTRY" TO Beskr.
          RETURN FALSE.
        END.
  ELSE IF DYNAMIC-FUNCTION("getFieldList","StrType;StrTypeId","WHERE StrTypeId = " + StrTypeId:SCREEN-VALUE) = "" THEN DO:
      MESSAGE "Ugyldig størrelsetype. Mangler størrelser i størrelsestypen."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO StrTypeId.
      RETURN FALSE.
  END.
  ELSE IF INT(Vg:SCREEN-VALUE) = 0 THEN DO: /* Mer kontroll på serveren */
      MESSAGE "Ugyldig varegruppe"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO Vg.
      RETURN FALSE.
    END.
  ELSE IF INPUT FI-Pris = 0 OR
     INPUT FI-VareKost = 0 OR
     INPUT FI-DBkr <= 0 OR
     INPUT FI-DB% <= 0 OR
     INPUT FI-Valpris = 0 THEN DO:
    bOk = FALSE.
    MESSAGE "Kalkylen er ufulstendig." SKIP
            "Det mangler en eller flere av følgende opplysninger:" SKIP
            "Valutapris, varekost, dekningsbidrag eller utpris." SKIP(1)
            "Skal kalkylen lagres?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
            UPDATE bOK.
    IF NOT bOk THEN
      RETURN FALSE.
  END.
  ELSE IF icAction = "edit" AND DYNAMIC-FUNCTION("getFieldValues","ArtPris",
                                                 "WHERE ArtikkelNr = " + STRING(fArtikkelNr)
                                               + "  AND ProfilNr = " + FI-ProfilNr:SCREEN-VALUE,
                                                 "ProfilNr") = ? THEN DO:
    MESSAGE "Ugyldig prisprofil"
            VIEW-AS ALERT-BOX ERROR.
    RETURN FALSE.
  END.

END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

