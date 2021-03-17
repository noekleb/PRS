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
                          
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hFieldMap-2       AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowse-2         AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hQuery-2          AS HANDLE NO-UNDO.

DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR iArtikkelNr       AS INT NO-UNDO.
DEF VAR iArtikkelNrOld    AS INT NO-UNDO.

DEF VAR hbcChkStrekNy     AS HANDLE NO-UNDO.
DEF VAR hbfChkStrekNy     AS HANDLE NO-UNDO.
DEF VAR hbcChkStrekFeil   AS HANDLE NO-UNDO.
DEF VAR hbfChkStrekFeil   AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttWidget NO-UNDO
  FIELD cField AS CHAR
  FIELD hField AS HANDLE
  FIELD cType  AS CHAR
  FIELD bVPI   AS LOG
  INDEX cField IS PRIMARY cField
  INDEX bVPI   bVPI
  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolbar rectWinToolbar RECT-1 RECT-2 ~
RECT-5 
&Scoped-Define DISPLAYED-OBJECTS ArtikkelNr-2 ArtikkelNr Beskr-2 Beskr ~
LevKod-2 LevKod LevFargKod-2 LevFargKod Vg-2 Vg LevNr-2 LevNr SaSong-2 ~
SaSong ProdNr-2 AktiveresDato OrgAktiveresDato VPIDato-2 GyldigTilDato ~
OrgGyldigTilDato Innkjopspris Innkjopspris#1-2 Rab1% Rab1%#1-2 Varekost#1-2 ~
Varekost DB% DB%#1-2 OrgDB% Mva% Mva%#1-2 OrgPris Pris Pris#1-2 ~
txtVPImottak txtArtikkel VgBeskr-2 VgBeskr LevNamn-2 LevNamn Sasbeskr-2 ~
Sasbeskr Beskrivelse-2 lblTekst-3 Tilbud 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildWidgetTable C-Win 
FUNCTION buildWidgetTable RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setColour C-Win 
FUNCTION setColour RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE AktiveresDato AS DATE FORMAT "99/99/99" 
     LABEL "Aktiveres" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE ArtikkelNr-2 AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(20)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE Beskr-2 AS CHARACTER FORMAT "x(20)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE Beskrivelse-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39.8 BY 1 NO-UNDO.

DEFINE VARIABLE DB% AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Dekn.Bid%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE DB%#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Dekn.Bid%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE GyldigTilDato AS DATE FORMAT "99/99/99" 
     LABEL "Gyldig til" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Innkjopspris AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Innkjopspris#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE lblTekst-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Orginal" 
      VIEW-AS TEXT 
     SIZE 8 BY .62 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge kode" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE LevFargKod-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge kode" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE LevKod-2 AS CHARACTER FORMAT "x(20)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE LevNamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40.8 BY 1 NO-UNDO.

DEFINE VARIABLE LevNamn-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39.8 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevNr-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Mva% AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Mva%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Mva%#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Mva%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE OrgAktiveresDato AS DATE FORMAT "99/99/99" 
     LABEL "Aktiveres" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE OrgDB% AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Dekn.bid%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE OrgGyldigTilDato AS DATE FORMAT "99/99/99" 
     LABEL "Gyldig til" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE OrgPris AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Org. pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Pris#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE ProdNr-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "ProdNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Rab1% AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Rabatt%1" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Rab1%#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Rabatt%1" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Sasbeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE Sasbeskr-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE SaSong AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE SaSong-2 AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Tilbud AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26 BY 1.38
     FGCOLOR 12 FONT 8 NO-UNDO.

DEFINE VARIABLE txtArtikkel AS CHARACTER FORMAT "X(256)":U INITIAL "Artikkel" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE txtVPImottak AS CHARACTER FORMAT "X(256)":U INITIAL "VPImottak" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE Varekost AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Varekost#1-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "VgNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Vg-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "VgNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE VgBeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40.6 BY 1 NO-UNDO.

DEFINE VARIABLE VgBeskr-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39.6 BY 1 NO-UNDO.

DEFINE VARIABLE VPIDato-2 AS DATE FORMAT "99/99/99" 
     LABEL "VpiDato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 20.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 20.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 10.71.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13.6 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ArtikkelNr-2 AT ROW 3.62 COL 92 COLON-ALIGNED
     ArtikkelNr AT ROW 3.86 COL 17 COLON-ALIGNED
     Beskr-2 AT ROW 4.81 COL 92 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     Beskr AT ROW 4.95 COL 17 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     LevKod-2 AT ROW 5.86 COL 92 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevKod AT ROW 6 COL 17 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevFargKod-2 AT ROW 6.91 COL 92 COLON-ALIGNED
     LevFargKod AT ROW 7.05 COL 17 COLON-ALIGNED
     Vg-2 AT ROW 8 COL 92 COLON-ALIGNED HELP
          "'varegruppenummer"
     Vg AT ROW 8.14 COL 17 COLON-ALIGNED HELP
          "'varegruppenummer"
     LevNr-2 AT ROW 9.05 COL 92 COLON-ALIGNED HELP
          "Leverandørnummer"
     LevNr AT ROW 9.19 COL 17 COLON-ALIGNED HELP
          "Leverandørnummer"
     SaSong-2 AT ROW 10.05 COL 92 COLON-ALIGNED HELP
          "Sesong"
     SaSong AT ROW 10.29 COL 17 COLON-ALIGNED HELP
          "Sesong"
     ProdNr-2 AT ROW 11.24 COL 92 COLON-ALIGNED HELP
          "Leverandørnummer"
     AktiveresDato AT ROW 12.43 COL 17.2 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     OrgAktiveresDato AT ROW 12.43 COL 51 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     VPIDato-2 AT ROW 12.43 COL 92 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     GyldigTilDato AT ROW 13.48 COL 17 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     OrgGyldigTilDato AT ROW 13.48 COL 51 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     Innkjopspris AT ROW 16 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     Innkjopspris#1-2 AT ROW 16 COL 92 COLON-ALIGNED HELP
          "Anbefalt pris"
     Rab1% AT ROW 17.1 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     Rab1%#1-2 AT ROW 17.1 COL 92 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost#1-2 AT ROW 18.1 COL 92 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost AT ROW 18.24 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     DB% AT ROW 19.24 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     DB%#1-2 AT ROW 19.24 COL 92 COLON-ALIGNED HELP
          "Anbefalt pris"
     OrgDB% AT ROW 19.33 COL 49 COLON-ALIGNED HELP
          "Anbefalt pris"
     Mva% AT ROW 20.38 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     Mva%#1-2 AT ROW 20.38 COL 92 COLON-ALIGNED HELP
          "Anbefalt pris"
     OrgPris AT ROW 21.38 COL 49 COLON-ALIGNED HELP
          "Anbefalt pris"
     Pris AT ROW 21.48 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     Pris#1-2 AT ROW 21.48 COL 92 COLON-ALIGNED HELP
          "Anbefalt pris"
     txtVPImottak AT ROW 2.52 COL 2 COLON-ALIGNED NO-LABEL
     txtArtikkel AT ROW 2.67 COL 78 COLON-ALIGNED NO-LABEL
     VgBeskr-2 AT ROW 7.91 COL 105.4 COLON-ALIGNED NO-LABEL
     VgBeskr AT ROW 8.14 COL 30.4 COLON-ALIGNED NO-LABEL
     LevNamn-2 AT ROW 9.05 COL 105.2 COLON-ALIGNED NO-LABEL
     LevNamn AT ROW 9.29 COL 30.2 COLON-ALIGNED NO-LABEL
     Sasbeskr-2 AT ROW 10.05 COL 105 COLON-ALIGNED NO-LABEL
     Sasbeskr AT ROW 10.29 COL 30 COLON-ALIGNED NO-LABEL
     Beskrivelse-2 AT ROW 11.24 COL 105.2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 149.6 BY 22.1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     lblTekst-3 AT ROW 11.95 COL 38 COLON-ALIGNED NO-LABEL
     Tilbud AT ROW 13.14 COL 114 COLON-ALIGNED NO-LABEL
     rectToolbar AT ROW 1.33 COL 2.4
     rectWinToolbar AT ROW 1.24 COL 142
     RECT-1 AT ROW 2.91 COL 1
     RECT-2 AT ROW 2.91 COL 75
     RECT-5 AT ROW 12.19 COL 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 149.6 BY 22.1.


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
         TITLE              = "Avviks visning"
         HEIGHT             = 22.14
         WIDTH              = 149.8
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN AktiveresDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ArtikkelNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ArtikkelNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ArtikkelNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Beskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Beskr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Beskr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Beskr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Beskrivelse-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN DB% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       DB%:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN DB%#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       DB%#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN GyldigTilDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Innkjopspris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Innkjopspris:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Innkjopspris#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Innkjopspris#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblTekst-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblTekst-3:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevFargKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevFargKod-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevKod IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevKod:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevKod-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevKod-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevNamn-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Mva% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Mva%:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Mva%#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Mva%#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN OrgAktiveresDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OrgDB% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       OrgDB%:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN OrgGyldigTilDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OrgPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       OrgPris:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Pris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Pris:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Pris#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Pris#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ProdNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ProdNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Rab1% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Rab1%:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Rab1%#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Rab1%#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Sasbeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Sasbeskr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       SaSong:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN SaSong-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       SaSong-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Tilbud IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtArtikkel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtArtikkel:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtVPImottak IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtVPImottak:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost#1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost#1-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Vg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Vg-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Vg-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN VgBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VgBeskr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VPIDato-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Avviks visning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Avviks visning */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  PUBLISH "InvalidateHandle".
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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

  hParent = SOURCE-PROCEDURE.

  RUN enable_UI.

  RUN InitializeObject.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkelKortRecord C-Win 
PROCEDURE ArtikkelKortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(hParent) THEN RUN ArtikkelKortRecord IN hParent.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId"))). */
/* DYNAMIC-FUNCTION("setCurrentObject",hBrowse).                                                                              */
/* RUN OpenQuery.                                                                                                             */
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
  run super.
  IF hFieldMap-2:AVAIL THEN Tilbud:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF hFieldMap-2:BUFFER-FIELD('Tilbud'):BUFFER-VALUE THEN 'PÅ TILBUD!' ELSE ''.

  run MoveToTop.
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
  DISPLAY ArtikkelNr-2 ArtikkelNr Beskr-2 Beskr LevKod-2 LevKod LevFargKod-2 
          LevFargKod Vg-2 Vg LevNr-2 LevNr SaSong-2 SaSong ProdNr-2 
          AktiveresDato OrgAktiveresDato VPIDato-2 GyldigTilDato 
          OrgGyldigTilDato Innkjopspris Innkjopspris#1-2 Rab1% Rab1%#1-2 
          Varekost#1-2 Varekost DB% DB%#1-2 OrgDB% Mva% Mva%#1-2 OrgPris Pris 
          Pris#1-2 txtVPImottak txtArtikkel VgBeskr-2 VgBeskr LevNamn-2 LevNamn 
          Sasbeskr-2 Sasbeskr Beskrivelse-2 lblTekst-3 Tilbud 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolbar rectWinToolbar RECT-1 RECT-2 RECT-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FirstRecord C-Win 
PROCEDURE FirstRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').
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
  buildWidgetTable().
  hBrowse = DYNAMIC-FUNCTION("getParentBrowse" IN hParent).                       
  
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,""
                             ,"VPImottak;!AktiveresDato;!AktiveresTid;!ArtikkelNr;!BehStatus;!Beskr;!BrukerID;!EDato;!ETid;!EtikettSkrevet;!GyldigTilDato;!GyldigTilTid;!LevFargKod;!LevKod;!LevNr;!OrgAktiveresDato;!OrgDB%;!OrgGyldigTilDato;!OrgPris;!ProfilNr;!RegistrertAv;!RegistrertDato;!RegistrertTid;!SaSong;!Vg;!VPIMottakId;!VPIType"
                            + ";!innkjopspris;!rab1%;!varekost;!mva%;!pris"
                            + ";!+DB%|decimal|>>>><>>9.99|DB%|DB%"
                             + ",VarGr;Vg;VgBeskr"
                             + ",LevBas;LevNr;LevNamn"
                             + ",SaSong;SaSong;SasBeskr"
                             + ",ArtPris;!ValPris;!TilbudTimeStyrt;!TilbudTilTid;!TilbudTilDato;!TilbudFraTid;!TilbudFraDato;!Tilbud;!RegistrertTid;!RegistrertDato;!RegistrertAv;!Rab3Kr;!Rab3%;!Rab2Kr;!Rab2%;!Rab1Kr;!ProfilNr;!MvaKr;!LevNr;!Frakt%;!Frakt;!EuroPris;!EuroManuel;!ETid;!EDato;!DivKostKr;!DivKost%;!DBKr;!DB%;!BrukerID;!ArtikkelNr;!AktivFraTid;!AktivFraDato"
                             ,"WHERE FALSE "
                             + ", FIRST VarGr outer-join OF VPImottak NO-LOCK"
                             + ", FIRST LevBas outer-join  OF VPImottak NO-LOCK"
                             + ", FIRST SaSong outer-join  OF VPImottak NO-LOCK"
                             + ", FIRST ArtPris outer-join  WHERE ArtPris.ArtikkelNr = VPImottak.ArtikkelNr AND ArtPris.ProfilNr = VPImottak.profilnr NO-LOCK"
                             ,"").
  
  DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hBrowse,"VPImottakId").
  DYNAMIC-FUNCTION('setAttribute',hQuery,'calcfieldproc','VPImottak_artikkel_detailcalc.p').

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,                    
                             FRAME {&FRAME-NAME}:HANDLE,
                             "",
                             "", 
                                  /* Updateable buffer fields and their correspondign input fields (blank if the same) */
                             "ArtikkelNr,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn" 
                             + ",AktiveresDato,SaSong,SasBeskr,Innkjopspris,Rab1%,Varekost,DB%,Mva%,Pris,OrgPris,OrgGyldigTilDato,OrgDB%,OrgAktiveresDato",
                             "ArtikkelNr,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn" 
                               + ",AktiveresDato,SaSong,SasBeskr,Innkjopspris,Rab1%,Varekost,DB%,Mva%,Pris,OrgPris,OrgGyldigTilDato,OrgDB%,OrgAktiveresDato",
                             ""). /* If any other fields are used for input these are listed here */
  /*
,,InnkjopsPris,VareKost,ForhRab%,SupVarekost,SupRab%,Pris,AnbefaltPris,KjedeRab%,KjedeInnkPris  
  */
/*   SUBSCRIBE TO 'ReopenChild' IN hParent. /*For å kjøre hquery, siden jeg havnet i en evig loop ved openquery*/ */
  hQuery-2 = DYNAMIC-FUNCTION("NewQuery",1,""
                             ,"ArtBas;!VPIDato;!VPIBildeKode;!VMId;!VisDivInfo;!VgKat;!Vg;!VareFakta;!valkod;!UtvidetSok;!Tilv-Land;!supRab%;!StrTypeID;!StrKode2;!StrKode1;!Storrelser;!slit-id;!Slasket;!SlaskArtikkelNr;!SentralBestilling;!SattPaKampanje;!SaSong;!SalgsEnhet;!RegistrertTid;!RegistrertDato;!RegistrertAv;!RabKod;!ProvKod;!ProdNr;!PrisGrpNr;!Pant;!Pakkenr;!Pakke;!ov-id;!OPris;!OLLager;!ny_dato;!Notat;!ModellFarge;!Mengde;!Medlemsutbytte;!MatKod;!ManRabIKas;!LopNr;!LokPris;!Lokasjon;!LinkVareNr;!LinjeMerknad;!LevVareTekst;!LevNr;!LevKod;!LevFargKod;!LevDato4;!LevDato3;!LevDato2;!LevDato1;!last-id;!LapTop;!lager;!KundeRabatt;!KonvFaktEtikett;!Kommentar;!Klack;!KjentPaHK;!KjedeVare;!KjedeRab%;!KjedeInnkPris;!KatalogPris;!inn_dato;!inner-id;!IndividType;!IKasse;!HoyLavMva;!HovedModellFarge;!HKVareId;!HkStyrt;!HKArtikkelNr;!Hg;!Gjennomfaktureres;!GarantiKl;!FrittTillegg;!forhRab%;!foder-id;!Farg;!EtiLayout;!Etikettekst2;!Etikettekst1;!Etikett;!ETid;!EDato;!DivInfo;!Depositum;!Dato1gSendtHk;!BrukerID;!BongTekst;!BildNr;!BildeIKasse;!BestForslag;!Beskr;!BehKode;!ArtSlag;!ArtikkelNr;!anv-id;!AntIPakn;!AnonseArtikkel;!AnbefaltPris;!Alder;!Aktivert;!AktivDato;!AktivAv"
                              + ";Katalogpris;forhrab%;supRab%"
                              + ";+VareKost-2|decimal|>>>><>>9.99|VareKost-2|Varekost"
                              + ";+suppVareKost-2|decimal|>>>><>>9.99|suppVareKost-2|Sup.Varekost"
                              + ",VarGr;Vg;VgBeskr"
                              + ",LevBas;LevNr;LevNamn"
                              + ",Produsent;ProdNr;Beskrivelse"
                              + ",SaSong;SaSong;SasBeskr"
                              + ",ArtPris;!ValPris;!TilbudTimeStyrt;!TilbudTilTid;!TilbudTilDato;!TilbudFraTid;!TilbudFraDato;!Tilbud;!RegistrertTid;!RegistrertDato;!RegistrertAv;!Rab3Kr;!Rab3%;!Rab2Kr;!Rab2%;!Rab1Kr;!Rab1%;!ProfilNr;!Pris;!MvaKr;!Mva%;!MomsKod;!LevNr;!InnkjopsPris;!Frakt%;!Frakt;!EuroPris;!EuroManuel;!ETid;!EDato;!DivKostKr;!DivKost%;!DBKr;!DB%;!BrukerID;!ArtikkelNr;!AktivFraTid;!AktivFraDato"
                              + ";+innkjopspris#1-2|decimal|>>>><>>9.99|innkjopspris#1-2|Innkjøpspris"
                              + ";+Rab1%#1-2|decimal|>>>><>>9.99|Rab1%#1-2|Rabatt1%"
                              + ";+Varekost#1-2|decimal|>>>><>>9.99|Varekost#1-2|Varekost"
                              + ";+DB%#1-2|decimal|>>>><>>9.99|DB%#1-2|DB%"
                              + ";+Mva%#1-2|decimal|>>>><>>9.99|Mva%#1-2|MVA"
                              + ";+Pris#1-2|decimal|>>>><>>9.99|Pris#1-2|Pris"
                             ,"WHERE FALSE"
                              + ", FIRST VarGr OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST LevBas OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST Produsent OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST SaSong OUTER-JOIN OF ArtBas NO-LOCK"
                              + ", FIRST ArtPris OUTER-JOIN WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND ArtPris.ProfilNr = 1 NO-LOCK"
                             ,"").
  DYNAMIC-FUNCTION('setAttribute',hQuery-2,'calcfieldproc','VPImottak_artikkel_detailcalc.p').
  
  hFieldMap-2 = DYNAMIC-FUNCTION("NewFieldMap",
                              hQuery-2,                    
                              FRAME {&FRAME-NAME}:HANDLE,
                              "",
                              "",
                              "ArtikkelNr,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn,ProdNr,Beskrivelse"
                              + ",VPIDato,SaSong,SasBeskr,Innkjopspris#1-2,Rab1%#1-2,Varekost#1-2,DB%#1-2,Mva%#1-2,Pris#1-2",
                               "ArtikkelNr-2,Beskr-2,LevKod-2,LevFargKod-2,Vg-2,VgBeskr-2,LevNr-2,LevNamn-2,ProdNr-2,Beskrivelse-2"
                               + ",VPIDato-2,SaSong-2,SasBeskr-2,Innkjopspris#1-2,Rab1%#1-2,Varekost#1-2,DB%#1-2,Mva%#1-2,Pris#1-2",
                              ""). /* If any other fields are used for input these are listed here */

  DYNAMIC-FUNCTION("CreateParentLink",hQuery-2,hQuery,"parentlink").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap-2,hQuery-2).

/*   DYNAMIC-FUNCTION("createParentLink",hQuery,hBrowse,"Varenr"). */
  
  /* Create the browse: */


  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "Fil",
                             "first|Navigate,prev|Navigate,next|Navigate,last|Navigate"
                             + ",rule,SendTilPrisRegister;Til pris kø,ArtikkelKort;Artikkel kort",
                             "maxborder").


  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "Fil",
                             "close;E&xit",
                             "right|enable").

  /* Disable the browse toolbar */
  DYNAMIC-FUNCTION("setToolbar",DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"toolbar","from"),"disable").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
/*   DYNAMIC-FUNCTION("ReplaceObjectLink",hQuery,hToolbar). /* the detail win toolbar should now be linked to the browse */ */

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).
          
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastRecord C-Win 
PROCEDURE LastRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').

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
apply 'entry' to artikkelnr in frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NextRecord C-Win 
PROCEDURE NextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
  iArtikkelNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE
.

 DYNAMIC-FUNCTION("setAttribute",hQuery-2,"parentlink"," WHERE ArtikkelNr = " + STRING(iArtikkelNr)).

 RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevRecord C-Win 
PROCEDURE PrevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  DYNAMIC-FUNCTION('setColour').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTilPrisRegisterRecord C-Win 
PROCEDURE SendTilPrisRegisterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF VALID-HANDLE(hParent) THEN RUN SendTilPrisRegisterRecord IN hParent.
 DYNAMIC-FUNCTION("RefreshRowids",hQuery,STRING(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ASSIGN
    hbcChkStrekNy    = ihBrowse:GET-BROWSE-COLUMN(4)
    hbfChkStrekNy    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrekNy')
    hbcChkStrekFeil  = ihBrowse:GET-BROWSE-COLUMN(5)
    hbfChkStrekFeil  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrekFeil')
    hbcChkStrekNy:COLUMN-FONT   = 8
    hbcChkStrekFeil:COLUMN-FONT = 8
  .
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildWidgetTable C-Win 
FUNCTION buildWidgetTable RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hObject AS HANDLE NO-UNDO.
ASSIGN 
  hObject = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD
  hObject = hObject:FIRST-CHILD
.

DO WHILE VALID-HANDLE(hObject):
  IF hObject:TYPE = 'FILL-IN' THEN
  DO:
    CREATE ttWidget.
    ASSIGN 
      ttWidget.cField = hObject:NAME
      ttWidget.hField = hObject:HANDLE
      ttWidget.bVPI   = INDEX(hObject:NAME,'-2') LE 0 
      ttWidget.cType  = hObject:DATA-TYPE
    .
  END.
  hObject = hObject:NEXT-SIBLING.
END.
/* FOR EACH ttWidget:                                                 */
/*   MESSAGE ttWidget.cField SKIP hField SKIP bVPI VIEW-AS ALERT-BOX. */
/* END.                                                               */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setColour C-Win 
FUNCTION setColour RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bttWidget FOR ttWidget.
  FOR EACH ttWidget WHERE ttWidget.bVPI NO-LOCK:
    FIND FIRST bttWidget WHERE bttWidget.cField = ttWidget.cField + '-2' NO-LOCK NO-ERROR.
    IF AVAIL bttWidget THEN
    DO:
      IF ttWidget.hField:SCREEN-VALUE = bttWidget.hField:SCREEN-VALUE THEN
      DO:
        ttWidget.hField:BGCOLOR = ?.
        NEXT.  /*Like verdier dropper vi å gjøre noe med*/
      END.
      CASE ttWidget.hField:DATA-TYPE:
        WHEN 'INTEGER' OR WHEN 'DECIMAL' THEN
        DO:
          IF INDEX(ttWidget.hField:NAME,'%') GT 0 THEN
          DO: /*Snu uttrykk, da lav verdi er dårlig*/
            ttWidget.hField:BGCOLOR =  IF DEC(ttWidget.hField:SCREEN-VALUE) < DEC(bttWidget.hField:SCREEN-VALUE) THEN
                                         12
                                       ELSE
                                         10.
          END.
          ELSE
          DO: /*Høyere verdi på VPI er gunstig*/
            ttWidget.hField:BGCOLOR =  IF DEC(ttWidget.hField:SCREEN-VALUE) > DEC(bttWidget.hField:SCREEN-VALUE) THEN
                                         12
                                       ELSE
                                         10.
          END.
        END.
        OTHERWISE
        DO:
          ttWidget.hField:BGCOLOR =  14.
        END.
      END CASE.
    END.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

