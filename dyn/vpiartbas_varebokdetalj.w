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
DEF VAR hToolBar-2        AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR iVarebokNr        AS DEC NO-UNDO.
DEF VAR iArtikkelNr       AS DEC NO-UNDO.
DEF VAR iArtikkelNrOld    AS DEC NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rectToolbar rectWinToolbar rectBrowse RECT-1 ~
RECT-2 rectToolbar-2 VPIBildekode-2 VPIBildekode 
&Scoped-Define DISPLAYED-OBJECTS ArtikkelNr-2 ArtikkelNr Beskr-2 Beskr ~
LevKod-2 LevKod LevFargKod-2 LevFargKod Vg-2 Vg LevNr-2 LevNr ProdNr-2 ~
ProdNr LevDato1-2 LevDato1 LevDato2-2 LevDato2 LevDato3-2 LevDato3 ~
LevDato4-2 LevDato4 VPIDato-2 VPIBildekode-2 VPIDato VPIBildekode SaSong-2 ~
SaSong Innkjopspris-2 Katalogpris#1 ForhRab%-2 ForhRab%#1 Varekost-2 ~
Varekost suppRab%-2 suppRab%#1 suppVareKost-2 suppVareKost AnbefaltPris-2 ~
AnbefaltPris txtVPIartbas txtVarebok VgBeskr-2 VgBeskr LevNamn-2 LevNamn ~
Beskrivelse-2 Beskrivelse Sasbeskr-2 Sasbeskr 

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
DEFINE VARIABLE AnbefaltPris AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Veil. pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE AnbefaltPris-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Veil. pris" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

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

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE Beskrivelse-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE ForhRab%#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Forh.rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE ForhRab%-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Forh.rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Innkjopspris-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Engros" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Katalogpris#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Engros" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE LevDato1 AS DATE FORMAT "99/99/99" 
     LABEL "1. Leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato1-2 AS DATE FORMAT "99/99/99" 
     LABEL "1. Leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato2 AS DATE FORMAT "99/99/99" 
     LABEL "2. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato2-2 AS DATE FORMAT "99/99/99" 
     LABEL "2. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato3 AS DATE FORMAT "99/99/99" 
     LABEL "3. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato3-2 AS DATE FORMAT "99/99/99" 
     LABEL "3. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato4 AS DATE FORMAT "99/99/99" 
     LABEL "4. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevDato4-2 AS DATE FORMAT "99/99/99" 
     LABEL "4. leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

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
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE LevNamn-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE LevNr-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "LevNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProdNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE ProdNr-2 AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE Sasbeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE Sasbeskr-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE SaSong AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE SaSong-2 AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE suppRab%#1 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Sup.Rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE suppRab%-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Sup.Rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE suppVareKost AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto. Innpr. supp" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE suppVareKost-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto. innpr. supp" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE txtVarebok AS CHARACTER FORMAT "X(256)":U INITIAL "Varebok" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE txtVPIartbas AS CHARACTER FORMAT "X(256)":U INITIAL "VPI Artbas" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE Varekost AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto Innpr. forh" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE VARIABLE Varekost-2 AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Nto. Innpr. forh" 
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
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE VgBeskr-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE VPIBildekode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bildereferanse" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE VPIBildekode-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bildereferanse" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE VPIDato AS DATE FORMAT "99/99/99" 
     LABEL "VpiDato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE VPIDato-2 AS DATE FORMAT "99/99/99" 
     LABEL "VpiDato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 22.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 22.86.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 149.6 BY 11.86.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.6 BY .95.

DEFINE RECTANGLE rectToolbar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.6 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ArtikkelNr-2 AT ROW 3.62 COL 94 COLON-ALIGNED
     ArtikkelNr AT ROW 3.86 COL 17 COLON-ALIGNED
     Beskr-2 AT ROW 4.81 COL 94 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     Beskr AT ROW 4.95 COL 17 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     LevKod-2 AT ROW 5.86 COL 94 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevKod AT ROW 6 COL 17 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevFargKod-2 AT ROW 6.91 COL 94 COLON-ALIGNED
     LevFargKod AT ROW 7.05 COL 17 COLON-ALIGNED
     Vg-2 AT ROW 8 COL 94 COLON-ALIGNED HELP
          "'varegruppenummer"
     Vg AT ROW 8.14 COL 17 COLON-ALIGNED HELP
          "'varegruppenummer"
     LevNr-2 AT ROW 9.05 COL 94 COLON-ALIGNED HELP
          "Leverandørnummer"
     LevNr AT ROW 9.19 COL 17 COLON-ALIGNED HELP
          "Leverandørnummer"
     ProdNr-2 AT ROW 11.19 COL 94 COLON-ALIGNED HELP
          "Produsent"
     ProdNr AT ROW 11.33 COL 17 COLON-ALIGNED HELP
          "Produsent"
     LevDato1-2 AT ROW 12.24 COL 94 COLON-ALIGNED HELP
          "Første dato da varene forventes levert butikk/lager."
     LevDato1 AT ROW 12.38 COL 17 COLON-ALIGNED HELP
          "Første dato da varene forventes levert butikk/lager."
     LevDato2-2 AT ROW 13.29 COL 94 COLON-ALIGNED HELP
          "Forventet dato for andre leveranse."
     LevDato2 AT ROW 13.43 COL 17 COLON-ALIGNED HELP
          "Forventet dato for andre leveranse."
     LevDato3-2 AT ROW 14.38 COL 94 COLON-ALIGNED HELP
          "Forventet dato for tredje leveranse."
     LevDato3 AT ROW 14.52 COL 17 COLON-ALIGNED HELP
          "Forventet dato for tredje leveranse."
     LevDato4-2 AT ROW 15.43 COL 94 COLON-ALIGNED HELP
          "Forventet dato for fjerde leveranse."
     LevDato4 AT ROW 15.57 COL 17 COLON-ALIGNED HELP
          "Forventet dato for fjerde leveranse."
     VPIDato-2 AT ROW 16.48 COL 94 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     VPIBildekode-2 AT ROW 16.48 COL 123 COLON-ALIGNED
     VPIDato AT ROW 16.62 COL 17 COLON-ALIGNED HELP
          "Dato da vpi ble importert"
     VPIBildekode AT ROW 16.62 COL 47 COLON-ALIGNED
     SaSong-2 AT ROW 17.43 COL 94 COLON-ALIGNED HELP
          "Sesong"
     SaSong AT ROW 17.67 COL 17 COLON-ALIGNED HELP
          "Sesong"
     Innkjopspris-2 AT ROW 18.86 COL 94 COLON-ALIGNED HELP
          "Anbefalt pris"
     Katalogpris#1 AT ROW 19 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     ForhRab%-2 AT ROW 19.81 COL 94 COLON-ALIGNED HELP
          "Anbefalt pris"
     ForhRab%#1 AT ROW 19.95 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost-2 AT ROW 20.95 COL 94 COLON-ALIGNED HELP
          "Anbefalt pris"
     Varekost AT ROW 21.1 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppRab%-2 AT ROW 21.95 COL 94 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppRab%#1 AT ROW 22.1 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppVareKost-2 AT ROW 23.1 COL 94 COLON-ALIGNED HELP
          "Anbefalt pris"
     suppVareKost AT ROW 23.24 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     AnbefaltPris-2 AT ROW 24.1 COL 94 COLON-ALIGNED HELP
          "Anbefalt pris"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 150 BY 37.91.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     AnbefaltPris AT ROW 24.24 COL 17 COLON-ALIGNED HELP
          "Anbefalt pris"
     txtVPIartbas AT ROW 2.52 COL 2 COLON-ALIGNED NO-LABEL
     txtVarebok AT ROW 2.67 COL 80 COLON-ALIGNED NO-LABEL
     VgBeskr-2 AT ROW 7.91 COL 107.4 COLON-ALIGNED NO-LABEL
     VgBeskr AT ROW 8.14 COL 30.4 COLON-ALIGNED NO-LABEL
     LevNamn-2 AT ROW 9.05 COL 107.2 COLON-ALIGNED NO-LABEL
     LevNamn AT ROW 9.29 COL 30.2 COLON-ALIGNED NO-LABEL
     Beskrivelse-2 AT ROW 11.14 COL 107 COLON-ALIGNED NO-LABEL
     Beskrivelse AT ROW 11.38 COL 30 COLON-ALIGNED NO-LABEL
     Sasbeskr-2 AT ROW 17.43 COL 107 COLON-ALIGNED NO-LABEL
     Sasbeskr AT ROW 17.67 COL 30 COLON-ALIGNED NO-LABEL
     rectToolbar AT ROW 1.33 COL 2.4
     rectWinToolbar AT ROW 1.33 COL 142.2
     rectBrowse AT ROW 27 COL 1.4
     RECT-1 AT ROW 2.91 COL 1.2
     RECT-2 AT ROW 2.91 COL 76
     rectToolbar-2 AT ROW 25.91 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 150 BY 37.91.


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
         TITLE              = "Avviks visning"
         HEIGHT             = 37.95
         WIDTH              = 151.4
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
/* SETTINGS FOR FILL-IN AnbefaltPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       AnbefaltPris:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN AnbefaltPris-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       AnbefaltPris-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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

/* SETTINGS FOR FILL-IN Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Beskrivelse-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ForhRab%#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ForhRab%#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ForhRab%-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ForhRab%-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Innkjopspris-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Innkjopspris-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Katalogpris#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Katalogpris#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevDato1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato1-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato2-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato3-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevDato4-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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

/* SETTINGS FOR FILL-IN ProdNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ProdNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN ProdNr-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ProdNr-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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

/* SETTINGS FOR FILL-IN suppRab%#1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppRab%#1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN suppRab%-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppRab%-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN suppVareKost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppVareKost:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN suppVareKost-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       suppVareKost-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtVarebok IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtVarebok:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN txtVPIartbas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       txtVPIartbas:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varekost-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varekost-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
/* SETTINGS FOR FILL-IN VPIDato IN FRAME DEFAULT-FRAME
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
          LevFargKod Vg-2 Vg LevNr-2 LevNr ProdNr-2 ProdNr LevDato1-2 LevDato1 
          LevDato2-2 LevDato2 LevDato3-2 LevDato3 LevDato4-2 LevDato4 VPIDato-2 
          VPIBildekode-2 VPIDato VPIBildekode SaSong-2 SaSong Innkjopspris-2 
          Katalogpris#1 ForhRab%-2 ForhRab%#1 Varekost-2 Varekost suppRab%-2 
          suppRab%#1 suppVareKost-2 suppVareKost AnbefaltPris-2 AnbefaltPris 
          txtVPIartbas txtVarebok VgBeskr-2 VgBeskr LevNamn-2 LevNamn 
          Beskrivelse-2 Beskrivelse Sasbeskr-2 Sasbeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolbar rectWinToolbar rectBrowse RECT-1 RECT-2 rectToolbar-2 
         VPIBildekode-2 VPIBildekode 
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
                             ,"VPIArtBas;!VPIDato;VPIBildeKode;!VMId;!VisDivInfo;!VgKat;!Vg;!VareNr;!ValKod;!Tilv-Land;!suppRab%;!StrTypeID;!StrKode2;!StrKode1;!Storrelser;!Slit-Id;!SentralBestilling;!SattPaKampanje;!SaSong;!SalgsEnhet;!RegistrertTid;!RegistrertDato;!RegistrertAv;!RabKod;!ProvKod;!ProdNr;!PrisGrpNr;!Pant;!Pakkenr;!Pakke;!Ov-Id;!OPris;!Oppdatert;!OLLager;!Notat;!ModellFarge;!Mengde;!MatKod;!ManRabIKas;!LopNr;!LokPris;!LokArtikkelNr;!LinkVareNr;!Linjemerknad;!LevVareTekst;!LevNr;!LevKod;!LevFargKod;!LevDato4;!LevDato3;!LevDato2;!LevDato1;!Last-Id;!Lager;!KundeRabatt;!Kommentar;!Klack;!KjentPaHK;!KjedeVare;!KatalogPris;!Inner-Id;!IndividType;!IKasse;!HovedModellFarge;!HKVareId;!HkStyrt;!Hg;!HandKode;!Gjennomfaktureres;!GarantiKl;!forhRab%;!Foder-Id;!Farg;!Etikett;!ETid;!EkstVPILevNr;!EDato;!DivInfo;!BrukerID;!BongTekst;!BildNr;!BildeIKasse;!BestForslag;!Beskr;!BehKode;!ArtSlag;!ArtikkelNr;!Anv-Id;!AntIPkn;!AnonseArtikkel;!AnbefaltPris;!Alder"
                             + ";+KatalogPris#1|decimal|>>>><>>9.99|KatalogPris#1|Katalogpris"
                             + ";+forhRab%#1|decimal|>>>><>>9.99|forhRab%#1|Forh.rab"
                             + ";+suppRab%#1|decimal|>>>><>>9.99|suppRab%#1|Sup.Rabatt"
                             + ";+VareKost|decimal|>>>><>>9.99|VareKost|Varekost"
                             + ";+suppVareKost|decimal|>>>><>>9.99|suppVareKost|Sup.Varekost"
                             + ",VarGr;Vg;VgBeskr"
                             + ",LevBas;LevNr;LevNamn"
                             + ",Produsent;ProdNr;Beskrivelse"
                             + ",SaSong;SaSong;SasBeskr"
                             ,"WHERE FALSE "
                             + ", FIRST VarGr OF VPIArtBas NO-LOCK"
                             + ", FIRST LevBas OF VPIArtBas NO-LOCK"
                             + ", FIRST Produsent OF VPIArtBas NO-LOCK"
                             + ", FIRST SaSong OF VPIArtBas NO-LOCK"
                             ,"").
  
  DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hBrowse,"EkstVPILevNr,Varenr").
  DYNAMIC-FUNCTION('setAttribute',hQuery,'calcfieldproc','vpiartbas_varebok_detailcalc.p').

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,                    
                             FRAME {&FRAME-NAME}:HANDLE,
                             "",
                             "", 
                                  /* Updateable buffer fields and their correspondign input fields (blank if the same) */
                             "ArtikkelNr,VPIBildeKode,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn,ProdNr,Beskrivelse,LevDato1,LevDato2,LevDato3" 
                             + ",LevDato4,VPIDato,SaSong,SasBeskr,KatalogPris#1,forhRab%#1,suppRab%#1,VareKost,suppVareKost,AnbefaltPris",
                             "ArtikkelNr,VPIBildeKode,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn,ProdNr,Beskrivelse,LevDato1,LevDato2,LevDato3" 
                               + ",LevDato4,VPIDato,SaSong,SasBeskr,KatalogPris#1,forhRab%#1,suppRab%#1,VareKost,suppVareKost,AnbefaltPris",
                             ""). /* If any other fields are used for input these are listed here */
  /*
,,InnkjopsPris,VareKost,ForhRab%,SupVarekost,SupRab%,Pris,AnbefaltPris,KjedeRab%,KjedeInnkPris  
  */
/*   SUBSCRIBE TO 'ReopenChild' IN hParent. /*For å kjøre hquery, siden jeg havnet i en evig loop ved openquery*/ */
  hQuery-2 = DYNAMIC-FUNCTION("NewQuery",1,""
                             ,"Vareboklinje;VPIDato;!VgBeskr;!Vg;!VareKost;!VareBokNr;!supVareKost;!supRab1Kr;!supPris;!supKalkyle;!supInnkjopsPris;!supDBKr;!supDB%;!supAntall;!Sortimentkoder;!Sekv;!SaSong;!RegistrertTid;!RegistrertDato;!RegistrertAv;!Rab1Kr;!ProdusentBeskrivelse;!ProdNr;!Pris;!Mva%;!ModellFarge;!LinjeMerknad;!levnr;!levnamn;!LevKod;!LevFargKod;!LevDato4;!LevDato3;!LevDato2;!LevDato1;!Lagerkoder;!KjedeVare;!KjedeRab%;!KjedeInnkPris;!Kampanjeuker;!Kampanjestotte;!KampanjePris;!InnkjopsPris;!HgBeskr;!Hg;!Gjennomfaktureres;!forhKalkyle;!ETid;!EDato;!DBKr;!DB%;!BrukerID;!Beskr;!AvdelingNr;!AvdelingNavn;!ArtikkelNr;!Antall;!AnbefaltPris"
                              + ";Katalogpris;forhrab%;supRab%"
                              + ";+VareKost-2|decimal|>>>><>>9.99|VareKost-2|Varekost"
                              + ";+suppVareKost-2|decimal|>>>><>>9.99|suppVareKost-2|Sup.Varekost"
                              + ",VarGr;Vg;VgBeskr"
                              + ",LevBas;LevNr;LevNamn"
                              + ",Produsent;ProdNr;Beskrivelse"
                              + ",SaSong;SaSong;SasBeskr"
                              + ",ArtBas;VPIBildekode"
                             ,"WHERE FALSE"
                              + ", FIRST VarGr OF Vareboklinje NO-LOCK"
                              + ", FIRST LevBas OF Vareboklinje NO-LOCK"
                              + ", FIRST Produsent OF Vareboklinje NO-LOCK"
                              + ", FIRST SaSong OF Vareboklinje NO-LOCK"
                              + ", FIRST ArtBas OF Vareboklinje NO-LOCK"
                             ,"").
  DYNAMIC-FUNCTION('setAttribute',hQuery-2,'calcfieldproc','vpiartbas_varebok_detailcalc.p').
  
  hFieldMap-2 = DYNAMIC-FUNCTION("NewFieldMap",
                              hQuery-2,                    
                              FRAME {&FRAME-NAME}:HANDLE,
                              "",
                              "",
                              "ArtikkelNr,VPIBildeKode,Beskr,LevKod,LevFargKod,Vg,VgBeskr,LevNr,LevNamn,ProdNr,Beskrivelse,LevDato1,LevDato2,LevDato3"
                              + ",LevDato4,VPIDato,SaSong,SasBeskr,Innkjopspris,forhRab%,supRab%,VareKost-2,suppVareKost-2,AnbefaltPris",
                               "ArtikkelNr-2,VPIBildeKode-2,Beskr-2,LevKod-2,LevFargKod-2,Vg-2,VgBeskr-2,LevNr-2,LevNamn-2,ProdNr-2,Beskrivelse-2,LevDato1-2,LevDato2-2,LevDato3-2"
                               + ",LevDato4-2,VPIDato-2,SaSong-2,SasBeskr-2,Innkjopspris-2,forhRab%-2,suppRab%-2,VareKost-2,suppVareKost-2,AnbefaltPris-2",
                              ""). /* If any other fields are used for input these are listed here */

  DYNAMIC-FUNCTION("CreateParentLink",hQuery-2,hQuery,"parentlink").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap-2,hQuery-2).

/*   DYNAMIC-FUNCTION("createParentLink",hQuery,hBrowse,"Varenr"). */
  
  /* Create the browse: */

  hBrowse-2 = DYNAMIC-FUNCTION("NewBrowse",
                             rectBrowse:HANDLE,
                             100,
                             "",
                             "VPIStrekKode"
                             + ";strKode;Storl;Kode"
                              + ";+chkStrekNy|logical|*/|chkStrekNy(ROWID)|Ny"
                              + ";+chkStrekFeil|logical|*/|chkStrekFeil(ROWID)|Feil" 
                              + ";Bestillingsnummer|Best.nr|x(15);ERPNr|ERPNr|x(15);!EkstVPILevNr;!VareNr;EDato|Endr.dato;RegistrertDato|Reg.dato"
                             ,"WHERE FALSE "
                             ,"SORT|Storl").
  
  DYNAMIC-FUNCTION("createParentLink",hBrowse-2,hBrowse,"EkstVPILevNr,Varenr").
  DYNAMIC-FUNCTION('setAttribute',hBrowse-2,'calcfieldproc','vpiartbas_varebok_detailcalc.p').

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "Fil",
                             "first|Navigate,prev|Navigate,next|Navigate,last|Navigate"
                             + ",rule,SendVarebok|Send til varebok,ArtikkelKort;Artikkel kort",
                             "maxborder").

  hToolbar-2 = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar-2:HANDLE,
                             "Fil",
                             "Delete;Slett strekkode",
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
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse-2,hToolbar-2).
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
  iVarebokNr  = DYNAMIC-FUNCTION('getVarebokNr' IN hParent)
  iArtikkelNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE
.

 DYNAMIC-FUNCTION("setAttribute",hQuery-2,"parentlink",
                             " WHERE Vareboknr = " + STRING(iVarebokNr)
                            + " AND ArtikkelNr = " + STRING(iArtikkelNr)).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVarebokRecord C-Win 
PROCEDURE SendVarebokRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hParent) THEN do:
    RUN SendVarebokRecord IN hParent.
    DYNAMIC-FUNCTION("RefreshRowids",hQuery,hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
END.

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

