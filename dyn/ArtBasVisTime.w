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
DEF INPUT  PARAM ihParent    AS HANDLE NO-UNDO.
DEF INPUT  PARAM dArtikkelNr AS DECIMAL NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE obOk        AS LOG NO-UNDO.
DEFINE VARIABLE icAction AS CHARACTER   NO-UNDO.

DEF VAR ix                AS INT NO-UNDO.
DEF VAR bOk               AS LOG NO-UNDO.
DEF VAR iWinX             AS INTE NO-UNDO.
DEF VAR iWinY             AS INTE NO-UNDO.
DEF VAR iWinWidth         AS INTE NO-UNDO.
DEF VAR iWinHeight        AS INTE NO-UNDO.
DEF VAR ifArtikkelNr      AS DEC NO-UNDO.
DEF VAR bHovedModellExist AS LOG NO-UNDO.
DEF VAR fMomsProc         AS DEC NO-UNDO.
DEF VAR cFieldValues      AS CHAR NO-UNDO.
DEF VAR cFieldValuesBut   AS CHAR NO-UNDO.
DEF VAR cModellFarge      AS CHAR NO-UNDO.
DEF VAR cCL            AS CHAR NO-UNDO.
DEFINE VARIABLE iButProfilNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCLProfilNr AS INTEGER     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS FarBeskr SasBeskr levnamn vgbeskr ~
Beskrivelse BUTTON-Next BUTTON-Prev BUTTON-Ok RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS UtgattDato FI-ButTxt fartikkelnr ArtSlag ~
LevNr LevKod LevFargKod cmbModell Beskr Vg VgKat StrTypeID Farg SaSong ~
matkod ProdNr FI-ProfilNrBut FI-InnkjopsPrisBut FI-VareKostBut FI-DBkrBut ~
FI-DB%But FI-MVAkrBut FI-Mva%But FI-PrisBut FarBeskr SasBeskr levnamn ~
vgbeskr Beskrivelse FI-BeskrivelseBut BongTekst ProdBeskrivelse ~
FI-AnbefaltPrisBut FI-InnkjopsPris FI-VareKost FI-Pris FI-DBkr FI-DB% ~
FI-MVAkr FI-Mva% FI-AnbefaltPris FI-HKTxt FI-ProfilNr FI-Beskrivelse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 FI-InnkjopsPrisBut FI-VareKostBut FI-DBkrBut ~
FI-DB%But FI-MVAkrBut FI-Mva%But FI-InnkjopsPris FI-VareKost FI-DBkr FI-DB% ~
FI-MVAkr FI-Mva% 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
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
DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE VARIABLE ArtSlag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 21.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cmbModell AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mod.farger" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE matkod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Materiale" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE VgKat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kategori" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Beskr" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13.8 BY .62 NO-UNDO.

DEFINE VARIABLE BongTekst AS CHARACTER FORMAT "X(20)" 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 24.4 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FarBeskr AS CHARACTER FORMAT "x(30)" 
      VIEW-AS TEXT 
     SIZE 33 BY .62.

DEFINE VARIABLE Farg AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Fargekode" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE fartikkelnr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 21.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AnbefaltPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Veil.pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AnbefaltPrisBut AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Veil.pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Beskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13.2 BY .62 NO-UNDO.

DEFINE VARIABLE FI-BeskrivelseBut AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE FI-ButTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Butikens kalkyl" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-DB% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-DB%But AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-DBkr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "DB (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-DBkrBut AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "DB (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-HKTxt AS CHARACTER FORMAT "X(256)":U INITIAL "HK's kalkyl" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-InnkjopsPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkj.pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-InnkjopsPrisBut AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkj.pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Mva% AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Mva%But AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-MVAkr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Mva (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-MVAkrBut AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Mva (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-PrisBut AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ProfilNr AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ProfilNrBut AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Prisprofil" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-VareKost AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-VareKostBut AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "Lev.farge" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "Lev.artnr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE levnamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .62 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE ProdBeskrivelse AS CHARACTER FORMAT "x(30)" 
      VIEW-AS TEXT 
     SIZE 29.6 BY .62.

DEFINE VARIABLE ProdNr AS INTEGER FORMAT "zzzzz9" INITIAL 1 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE SasBeskr AS CHARACTER FORMAT "x(30)" 
      VIEW-AS TEXT 
     SIZE 33 BY .62.

DEFINE VARIABLE SaSong AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE StrTypeID AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Str.type" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE UtgattDato AS DATE FORMAT "99/99/99" INITIAL ? 
     LABEL "Utgår dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE vgbeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58.4 BY 17.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 17.86.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 17.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     UtgattDato AT ROW 18.86 COL 12 COLON-ALIGNED HELP
          "Dato når artikkel går ut av kjedens sortiment"
     FI-ButTxt AT ROW 3.38 COL 70.2 COLON-ALIGNED NO-LABEL
     fartikkelnr AT ROW 2.62 COL 12 COLON-ALIGNED
     ArtSlag AT ROW 3.67 COL 12 COLON-ALIGNED
     LevNr AT ROW 4.76 COL 12 COLON-ALIGNED HELP
          "Leverandørnummer"
     LevKod AT ROW 5.81 COL 12 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevFargKod AT ROW 6.86 COL 12 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     cmbModell AT ROW 7.95 COL 12 COLON-ALIGNED
     Beskr AT ROW 9.05 COL 12 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     Vg AT ROW 11.14 COL 12 COLON-ALIGNED HELP
          "'varegruppenummer"
     VgKat AT ROW 12.19 COL 12 COLON-ALIGNED
     StrTypeID AT ROW 13.29 COL 12 COLON-ALIGNED HELP
          "Størrelsestype"
     Farg AT ROW 14.33 COL 12 COLON-ALIGNED HELP
          "Fargekode"
     SaSong AT ROW 15.43 COL 12 COLON-ALIGNED HELP
          "Sesong"
     matkod AT ROW 16.48 COL 12 COLON-ALIGNED
     ProdNr AT ROW 17.62 COL 12 COLON-ALIGNED HELP
          "Produsent"
     FI-ProfilNrBut AT ROW 4.76 COL 71 COLON-ALIGNED
     FI-InnkjopsPrisBut AT ROW 6.81 COL 71 COLON-ALIGNED
     FI-VareKostBut AT ROW 7.91 COL 71 COLON-ALIGNED
     FI-DBkrBut AT ROW 8.95 COL 71 COLON-ALIGNED
     FI-DB%But AT ROW 8.95 COL 87 COLON-ALIGNED NO-LABEL
     FI-MVAkrBut AT ROW 10.05 COL 71 COLON-ALIGNED
     FI-Mva%But AT ROW 10.05 COL 87 COLON-ALIGNED NO-LABEL
     FI-PrisBut AT ROW 12.19 COL 71 COLON-ALIGNED
     btnLev AT ROW 4.76 COL 24.6 NO-TAB-STOP 
     FarBeskr AT ROW 14.52 COL 24 COLON-ALIGNED NO-LABEL
     SasBeskr AT ROW 15.62 COL 23.8 COLON-ALIGNED NO-LABEL
     levnamn AT ROW 4.95 COL 28 COLON-ALIGNED NO-LABEL
     vgbeskr AT ROW 11.33 COL 23.8 COLON-ALIGNED NO-LABEL
     Beskrivelse AT ROW 13.48 COL 23.8 COLON-ALIGNED NO-LABEL
     FI-BeskrivelseBut AT ROW 5 COL 87 COLON-ALIGNED NO-LABEL
     BongTekst AT ROW 10.1 COL 12 COLON-ALIGNED HELP
          "Bongtekst - Tekst som vises på kundens kvittering" NO-TAB-STOP 
     ProdBeskrivelse AT ROW 17.81 COL 23.8 COLON-ALIGNED NO-LABEL
     FI-AnbefaltPrisBut AT ROW 11.1 COL 71 COLON-ALIGNED
     BUTTON-Next AT ROW 1.19 COL 6.8 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.19 COL 2.2 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.24 COL 142 HELP
          "Lagre og avslutt" NO-TAB-STOP 
     FI-InnkjopsPris AT ROW 6.81 COL 114.2 COLON-ALIGNED
     FI-VareKost AT ROW 7.91 COL 114.2 COLON-ALIGNED
     FI-Pris AT ROW 12.19 COL 114.2 COLON-ALIGNED
     FI-DBkr AT ROW 8.95 COL 114.2 COLON-ALIGNED
     FI-DB% AT ROW 8.95 COL 130 COLON-ALIGNED NO-LABEL
     FI-MVAkr AT ROW 10.05 COL 114 COLON-ALIGNED
     FI-Mva% AT ROW 10.05 COL 130 COLON-ALIGNED NO-LABEL
     FI-AnbefaltPris AT ROW 11.1 COL 114.2 COLON-ALIGNED
     FI-HKTxt AT ROW 3.38 COL 112.6 COLON-ALIGNED NO-LABEL
     FI-ProfilNr AT ROW 4.76 COL 114.2 COLON-ALIGNED
     FI-Beskrivelse AT ROW 5 COL 130 COLON-ALIGNED NO-LABEL
     "Kroner" VIEW-AS TEXT
          SIZE 14.2 BY .57 AT ROW 6.05 COL 73
          FONT 6
     "Prosent" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 6.05 COL 89
          FONT 6
     "Kroner" VIEW-AS TEXT
          SIZE 14.2 BY .57 AT ROW 6.05 COL 116.2
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147 BY 19.43.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "Prosent" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 6.05 COL 132.2
          FONT 6
     RECT-1 AT ROW 2.43 COL 1.8
     RECT-2 AT ROW 2.43 COL 59.8
     RECT-3 AT ROW 2.43 COL 103.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147 BY 19.43.


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
         TITLE              = "Vis artikkel"
         HEIGHT             = 19.43
         WIDTH              = 147
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR COMBO-BOX ArtSlag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Beskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN BongTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnLev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cmbModell IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Farg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fartikkelnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fartikkelnr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-AnbefaltPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AnbefaltPrisBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BeskrivelseBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButTxt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DB% IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-DB%But IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-DBkr IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-DBkrBut IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-HKTxt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnkjopsPris IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-InnkjopsPrisBut IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-Mva% IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-Mva%But IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-MVAkr IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-MVAkrBut IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-Pris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PrisBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ProfilNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ProfilNrBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN FI-VareKostBut IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN LevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       levnamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX matkod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ProdBeskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ProdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StrTypeID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN UtgattDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       vgbeskr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR COMBO-BOX VgKat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* Vis artikkel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vis artikkel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtSlag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtSlag C-Win
ON VALUE-CHANGED OF ArtSlag IN FRAME DEFAULT-FRAME /* Varetype */
DO:
    IF ArtSlag:SENSITIVE THEN DO:
        IF CAN-DO("0,2",ArtSlag:SCREEN-VALUE) THEN
            ASSIGN StrTypeID:SCREEN-VALUE = "0".
        ELSE IF CAN-DO("1,3",ArtSlag:SCREEN-VALUE) THEN
            ASSIGN StrTypeID:SCREEN-VALUE = "2".
    END.
    APPLY "TAB" TO StrTypeID.
        IF LevNr:SCREEN-VALUE = "0" THEN
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


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.
  DEF VAR cLevData AS CHAR NO-UNDO.

  cLookupValue = "levnr;levnamn".

  RUN JBoxDLookup.w ("LevBas;levnamn;Levnr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:    
    ASSIGN levnr:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
           levnamn:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           levnr:MODIFIED       = TRUE
           .
  END.
  ELSE DO: 
      ASSIGN levnr:MODIFIED = TRUE.
  END.

/*   IF LevNr:MODIFIED THEN                                                                                                           */
/*     ASSIGN cLevData               = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn;ValKod","WHERE LevNr = " + LevNr:SCREEN-VALUE) */
/*            .                                                                                                                       */
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-Win
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
  IF VALID-HANDLE(ihParent) THEN DO:
      RUN PrevNext IN ihParent ("Next").
  END.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-Win
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
  IF VALID-HANDLE(ihParent) THEN DO:
      RUN PrevNext IN ihParent ("Prev").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Farg C-Win
ON RETURN OF Farg IN FRAME DEFAULT-FRAME /* Fargekode */
OR TAB OF Farg DO:
  IF Farg:MODIFIED THEN 
    FarBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Farg;FarBeskr","WHERE Farg = " + Farg:SCREEN-VALUE).
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
DO:
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
           .
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


&Scoped-define SELF-NAME SaSong
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
ON RETURN OF StrTypeID IN FRAME DEFAULT-FRAME /* Str.type */
OR TAB OF StrTypeId DO:
    IF SELF:SCREEN-VALUE = "1" THEN DO:
        BELL.
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
    END.
    IF SELF:SCREEN-VALUE = "2" AND NOT CAN-DO("1,3",ArtSlag:SCREEN-VALUE) THEN
        ArtSlag:SCREEN-VALUE = "1".
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


&Scoped-define SELF-NAME Vg
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
iWinX      = CURRENT-WINDOW:X.
iWinY      = CURRENT-WINDOW:Y.
/* iWinWidth  = CURRENT-WINDOW:WIDTH.  */
/* iWinHeight = CURRENT-WINDOW:HEIGHT. */
iWinWidth  = CURRENT-WINDOW:WIDTH-PIXELS.
iWinHeight = CURRENT-WINDOW:HEIGHT-PIXELS.
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
/* CURRENT-WINDOW:X = iWinX + 200. */
/* CURRENT-WINDOW:Y = iWinY + 200. */
CURRENT-WINDOW:X = iWinX - CURRENT-WINDOW:WIDTH-PIXELS / 2 + iWinWidth / 2.
CURRENT-WINDOW:Y = iWinY - CURRENT-WINDOW:HEIGHT-PIXELS / 2 + iWinHeight / 2.

/* ON 'alt-l':U OF FRAME {&FRAME-NAME} ANYWHERE */
/*   APPLY "choose" TO btnSave.                 */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  fArtikkelNr = dArtikkelnr.
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  /* Standard profil */
  cCL                    = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").
  FI-Profilnr = INT(DYNAMIC-FUNCTION("getFieldValues","Butiker",
                                            "WHERE Butik = " + cCL,"ProfilNr")).
  /* Butikens profil */
  IF AVAIL bruker THEN DO:
      FIND butiker WHERE butiker.butik = bruker.butikknr NO-LOCK NO-ERROR.
      IF AVAIL butiker THEN
          FI-ProfilNrBut = butiker.profilnr.
  END.
  FI-Beskrivelse    = DYNAMIC-FUNCTION("getFieldList","PrisProfil;Beskrivelse","WHERE ProfilNr = " + STRING(FI-ProfilNr)).
  FI-BeskrivelseBut = DYNAMIC-FUNCTION("getFieldList","PrisProfil;Beskrivelse","WHERE ProfilNr = " + STRING(FI-ProfilNrBut)).

  RUN enable_UI.
  SUBSCRIBE TO "ByttArtikkel" IN ihParent.
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  APPLY "VALUE-CHANGED" TO ArtSlag.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttArtikkel C-Win 
PROCEDURE ByttArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER dArtNr AS DECIMAL     NO-UNDO.
fArtikkelNr = dArtNr.
DISP fArtikkelnr WITH FRAME {&FRAME-NAME}.
RUN Initwindow.
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
  DISPLAY UtgattDato FI-ButTxt fartikkelnr ArtSlag LevNr LevKod LevFargKod 
          cmbModell Beskr Vg VgKat StrTypeID Farg SaSong matkod ProdNr 
          FI-ProfilNrBut FI-InnkjopsPrisBut FI-VareKostBut FI-DBkrBut FI-DB%But 
          FI-MVAkrBut FI-Mva%But FI-PrisBut FarBeskr SasBeskr levnamn vgbeskr 
          Beskrivelse FI-BeskrivelseBut BongTekst ProdBeskrivelse 
          FI-AnbefaltPrisBut FI-InnkjopsPris FI-VareKost FI-Pris FI-DBkr FI-DB% 
          FI-MVAkr FI-Mva% FI-AnbefaltPris FI-HKTxt FI-ProfilNr FI-Beskrivelse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FarBeskr SasBeskr levnamn vgbeskr Beskrivelse BUTTON-Next BUTTON-Prev 
         BUTTON-Ok RECT-1 RECT-2 RECT-3 
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


cFieldValuesBut = ?.

DO WITH FRAME {&FRAME-NAME}: 
    /* om butiken har avvikande profil */
    IF FI-ProfilNrBut <> FI-ProfilNR THEN DO:
        cFieldValuesBut = DYNAMIC-FUNCTION("getFieldValues","ArtPris","WHERE Artikkelnr = " + STRING(fArtikkelnr) + " AND ProfilNr = " + FI-ProfilNrBut:SCREEN-VALUE,
                                        "InnkjopsPris;1"
                                        + ",Varekost;1"
                                        + ",DBkr;1"
                                        + ",DB%;1"
                                        + ",MVAkr;1"
                                        + ",MVA%;1"
                                        + ",Pris;1"
                                        ).
    END.
    cFieldValues = DYNAMIC-FUNCTION("getFieldValues","ArtPris","WHERE Artikkelnr = " + STRING(fArtikkelnr) + " AND ProfilNr = " + FI-ProfilNr:SCREEN-VALUE,
                                  "InnkjopsPris;1"
                                  + ",Varekost;1"
                                  + ",DBkr;1"
                                  + ",DB%;1"
                                  + ",MVAkr;1"
                                  + ",MVA%;1"
                                  + ",Pris;1"
                                  ).
    IF cFieldValuesBut = ? THEN
        cFieldValuesBut = cFieldValues.
    IF cFieldValues NE ? THEN DO:
      ASSIGN FI-InnkjopsPris:SCREEN-VALUE = ENTRY(1,cFieldValues,"|")
             FI-Varekost:SCREEN-VALUE     = ENTRY(2,cFieldValues,"|")
             FI-DBkr:SCREEN-VALUE         = ENTRY(3,cFieldValues,"|")
             FI-DB%:SCREEN-VALUE          = ENTRY(4,cFieldValues,"|")
             FI-MVAkr:SCREEN-VALUE        = ENTRY(5,cFieldValues,"|")
             FI-MVA%:SCREEN-VALUE         = ENTRY(6,cFieldValues,"|")
             FI-Pris:SCREEN-VALUE         = ENTRY(7,cFieldValues,"|").

      ASSIGN FI-InnkjopsPrisBut:SCREEN-VALUE = ENTRY(1,cFieldValuesBut,"|")
             FI-VarekostBut:SCREEN-VALUE     = ENTRY(2,cFieldValuesBut,"|")
             FI-DBkrBut:SCREEN-VALUE         = ENTRY(3,cFieldValuesBut,"|")
             FI-DB%But:SCREEN-VALUE          = ENTRY(4,cFieldValuesBut,"|")
             FI-MVAkrBut:SCREEN-VALUE        = ENTRY(5,cFieldValuesBut,"|")
             FI-MVA%But:SCREEN-VALUE         = ENTRY(6,cFieldValuesBut,"|")
             FI-PrisBut:SCREEN-VALUE         = ENTRY(7,cFieldValuesBut,"|").
      
      FI-DBkrBut:BGCOLOR  = IF FI-DBkrBut:SCREEN-VALUE  <> FI-DBkr:SCREEN-VALUE  THEN 10 ELSE 15.
      FI-DB%But:BGCOLOR   = IF FI-DB%But:SCREEN-VALUE   <> FI-DB%:SCREEN-VALUE   THEN 10 ELSE 15.
      FI-MVAkrBut:BGCOLOR = IF FI-MVAkrBut:SCREEN-VALUE <> FI-MVAkr:SCREEN-VALUE THEN 10 ELSE 15.
      FI-PrisBut:BGCOLOR  = IF FI-PrisBut:SCREEN-VALUE  <> FI-Pris:SCREEN-VALUE  THEN 10 ELSE 15.
    END.
    ELSE DO:
        ASSIGN FI-InnkjopsPris:screen-value  = ""
               FI-VareKost:screen-value = ""
               FI-MvaKr:screen-value      = ""
               FI-Mva%:screen-value     = ""
               FI-DBkr:screen-value       = ""
               FI-DB%:screen-value      = ""
               FI-Pris:screen-value     = ""
               .
        ASSIGN FI-InnkjopsPrisBut:screen-value  = ""
               FI-VareKostBut:screen-value = ""
               FI-MvaKrBut:screen-value      = ""
               FI-Mva%But:screen-value     = ""
               FI-DBkrBut:screen-value       = ""
               FI-DB%But:screen-value      = ""
               FI-PrisBut:screen-value     = ""
               .
    END.

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
DEF VAR cVgKat         AS CHAR NO-UNDO.
DEF VAR cArtSlag       AS CHAR NO-UNDO.
DEF VAR cArtSlagUnntak AS CHAR NO-UNDO.
DEF VAR cArtSlagOK     AS CHAR NO-UNDO.
DEF VAR cTekst         AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
/*          fArtikkelNr = DYNAMIC-FUNCTION("getArtNr" IN ihParent) */
         ArtSlag:DELIMITER = "|"
         matkod:DELIMITER = "|"
         VgKat:DELIMITER = "|"
         cmbModell:DELIMITER = "|"
         /*FI-ValKod:DELIMITER = "|"*/
         /*cArtSlagUnntak  = DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1","WHERE SysHId = 2 and SysGr = 8")*/
         /*cArtSlag = DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1","WHERE SysHId = 2 and SysGr = 7")*/
         matkod:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Material;Matkod|MatBeskr;Matkod","WHERE true")
         /*
         FI-ValKod:LIST-ITEMS = "|" + DYNAMIC-FUNCTION("getFieldList","Valuta;Valkod","WHERE true")
         */
         .

  cArtSlagOk = "Stykkvare(Stk)|0" +
               "|Vektvare(Kg)|1" +
               "|Vektvare(Hg)|2" +
               "|Metervare(m)|3" + 
               "|Kvadratmetervare(m2)|4" + 
               "|Volumvare(l)|5"
               .

  /*
  DO ix = 2 TO NUM-ENTRIES(cArtSlag,"|") BY 2:
    IF LOOKUP(ENTRY(ix,cArtSlag,"|"),cArtSlagUnntak,"|") = 0 THEN
      cArtSlagOK = cArtSlagOK + ENTRY(ix - 1,cArtSlag,"|") + "|" + ENTRY(ix ,cArtSlag,"|") + "|".
  END.
  */
  ArtSlag:LIST-ITEM-PAIRS = TRIM(cArtSlagOK,"|").

  IF icAction NE "new" THEN DO:
    ASSIGN 
         cFieldValues = DYNAMIC-FUNCTION("getFieldValues","Artbas","WHERE Artikkelnr = " + STRING(fArtikkelnr),
                                         "Levnr,LevKod,LevFargKod,Beskr,Vg,StrTypeID,MatKod,Sasong,StrTypeId,Farg,VgKat,ArtSlag,AnbefaltPris,UtgattDato")
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
         FI-AnbefaltPris:SCREEN-VALUE = ENTRY(13,cFieldValues,"|")
         FI-AnbefaltPrisBut:SCREEN-VALUE = ENTRY(13,cFieldValues,"|")
         UtgattDato:SCREEN-VALUE = ENTRY(14,cFieldValues,"|")

         levnamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE levnr = " + Levnr:SCREEN-VALUE,"levnamn")
         vgbeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","VarGr","WHERE Vg = " + Vg:SCREEN-VALUE,"VgBeskr")
         Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","StrType","WHERE StrTypeId = " + StrTypeId:SCREEN-VALUE,"Beskrivelse")
         ArtSlag:SCREEN-VALUE = ENTRY(12,cFieldValues,"|")
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
          VgKat:SCREEN-VALUE = ENTRY(11,cFieldValues,"|")
          cVgKat = ENTRY(11,cFieldValues,"|").

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
                .
    APPLY "tab" TO Vg.
  END.
  ELSE DO:
    ASSIGN ArtSlag:SCREEN-VALUE   = ENTRY(2,ArtSlag:LIST-ITEM-PAIRS,"|")
           .
    LevNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getLevNr" IN ihParent) NO-ERROR.
    IF LevNr:SCREEN-VALUE NE "0" THEN APPLY "return" TO LevNr.
    LevNr:MODIFIED = FALSE.
  END.

  APPLY "tab" TO Farg.

  APPLY "entry" TO LevNr.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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

  hField = hField:NEXT-SIBLING.
END.

RETURN httVerdier.

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

END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

