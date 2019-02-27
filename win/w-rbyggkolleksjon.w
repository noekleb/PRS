&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tBild NO-UNDO LIKE Bild.
DEFINE NEW SHARED TEMP-TABLE tmpFarg NO-UNDO LIKE Farg.
DEFINE NEW SHARED TEMP-TABLE tmpLevBas NO-UNDO LIKE LevBas.



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
def var wVareGrupper  as char no-undo.
def var wLeverandorer as char no-undo.
def var wBestStatus   as char no-undo.
def var wSesonger     as char no-undo.
def var wFarger       as char no-undo.
def var wMaterial     as char no-undo.
def var wButiker      as char no-undo.
def var wVmId         as char no-undo.

/* Valgte valg. */
def var wvVareGrupper  as char no-undo.
def var wvLeverandorer as char no-undo.
def var wvBestStatus   as char no-undo.
def var wvSesonger     as char no-undo.
def var wvFarger       as char no-undo.
def var wvMaterial     as char no-undo.
def var wvButiker      as char no-undo.
def var wvVmId         as char no-undo.
DEF VAR wCl            AS INT  NO-UNDO.

/* For kobling mot bildegrid */
DEFINE VAR wChild AS HANDLE NO-UNDO.
DEFINE VAR wMenu  AS CHAR NO-UNDO.
DEFINE VAR wScreenSize AS INTE INIT 1 NO-UNDO.
DEFINE VAR wColor AS INTE EXTENT 3 INIT [5197823,65280,16711680] NO-UNDO.
define var wNumRecords as int no-undo.
define var wMenuAction as char no-undo.

/* Temp-Table. */
/* {tmplevbas.i &New = "New"} */

/* Buffere */
def buffer bLister   for Lister.
DEF BUFFER clButiker FOR Butiker.

DEF VAR cProdusentRowIdList AS CHAR NO-UNDO.
DEF VAR cProdusentIdList    AS CHAR NO-UNDO.
DEF VAR bOk                 AS LOG NO-UNDO.


{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-5 RECT-6 RECT-7 RECT-8 B-Grid ~
BUTTON-SokLevDato COMBO-BOX-WinSize CB-Sortering B-SokListe FI-Navn ~
FI-Merknad FI-FraDato FI-TilDato FI-FraLevDato FI-TilLevDato FI-FraOrdreNr ~
FI-TilOrdreNr FI-FraBestNr FI-TilBestNr cmbBekreftet FI-FraKateg ~
FI-TilKateg T-Annonse T-Butikk B-Varemerker btnUtvalgProdusent B-VarGr ~
B-Sesong B-LevBas B-Farg B-BestStat B-Material BUTTON-SokTilDato B-Rediger ~
B-Exit B-Ny Btn_Help BUTTON-SokDato-2 BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-WinSize CB-Sortering FI-ListeNr ~
FI-Navn FI-Merknad FI-FraDato FI-TilDato FI-Opprettet FI-FraLevDato ~
FI-TilLevDato FI-Endret FI-FraOrdreNr FI-TilOrdreNr FI-Eier FI-FraBestNr ~
FI-TilBestNr cmbBekreftet FI-FraKateg FI-TilKateg T-Annonse T-Butikk ~
FI-Varemerker fi-Produsent FI-Butiker FI-VAreGr FI-Sesong FI-LevBas FI-Farg ~
FI-BestStat FI-Material FI-Info FILL-IN-Tekst 

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

DEFINE BUTTON B-BestStat 
     LABEL "&Bestillingsstatus..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Butikker 
     LABEL "&Butikker..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Bygg 
     IMAGE-UP FILE "icon/e-save":U NO-FOCUS FLAT-BUTTON
     LABEL "By&gg liste..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre - Ved ny oppprettes ny liste, ved endre, oppdateres valgt liste.".

DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON B-Farg 
     LABEL "&Farger..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Forrige 
     IMAGE-UP FILE "icon/e-pilopp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av valgt liste.".

DEFINE BUTTON B-Grid 
     LABEL "&Bildegrid..." 
     SIZE 12.6 BY 1.

DEFINE BUTTON B-LevBas 
     LABEL "&Leverandører..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Material 
     LABEL "&Material..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Neste 
     IMAGE-UP FILE "icon/e-pilned":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Neste".

DEFINE BUTTON B-Ny 
     IMAGE-UP FILE "icon/e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny kolleksjonsliste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny liste".

DEFINE BUTTON B-Rediger  NO-FOCUS
     LABEL "&Liste..." 
     SIZE 8.8 BY 1.

DEFINE BUTTON B-Sesong 
     LABEL "&Sesong..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-ShowRoom 
     LABEL "&ShowRoom..." 
     SIZE 15 BY 1.

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

DEFINE BUTTON B-Varemerker 
     LABEL "V&aremerker..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-VarGr 
     LABEL "&Varegrupper..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON btnUtvalgProdusent 
     LABEL "Produsent..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLevDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokTilDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Sortering AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 37.6 BY 1 NO-UNDO.

DEFINE VARIABLE cmbBekreftet AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Bekreftet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0","1"
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-WinSize AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "800 X 600","1024 X 768","1152 X 864","1280 X 1024" 
     DROP-DOWN-LIST
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BestStat AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(10)":U 
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

DEFINE VARIABLE FI-Farg AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraBestNr AS INTEGER FORMAT ">>>>>>9":U INITIAL 1 
     LABEL "Bestillingsnr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Bestillingsdato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraKateg AS INTEGER FORMAT "9":U INITIAL 1 
     LABEL "Kategori" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraLevDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Leveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraOrdreNr AS INTEGER FORMAT "zzzzzz9":U INITIAL 0 
     LABEL "OrdreNr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 134 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevBas AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ListeNr AS INTEGER FORMAT "-zzzzzzzz9":U INITIAL 0 
     LABEL "Liste nr/navn" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Material AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Merknad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 133 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 96 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Opprettet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Produsent AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilBestNr AS INTEGER FORMAT ">>>>>>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilKateg AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilLevDato AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilOrdreNr AS INTEGER FORMAT "zzzzzzz9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VAreGr AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Varemerker AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Merking" 
      VIEW-AS TEXT 
     SIZE 48 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 136 BY 8.24.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 135.6 BY .1.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 136 BY 10.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 138 BY .1.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 138 BY .1.

DEFINE VARIABLE T-Annonse AS LOGICAL INITIAL no 
     LABEL "Annonsevarer" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE T-Butikk AS LOGICAL INITIAL no 
     LABEL "&Pr. butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-ShowRoom AT ROW 1.33 COL 36.8
     B-Grid AT ROW 1.33 COL 52.2
     BUTTON-SokLevDato AT ROW 7.43 COL 36.2
     COMBO-BOX-WinSize AT ROW 1.33 COL 63.2 COLON-ALIGNED NO-LABEL
     CB-Sortering AT ROW 1.33 COL 80 COLON-ALIGNED NO-LABEL
     B-SokListe AT ROW 3.38 COL 36.2
     FI-ListeNr AT ROW 3.43 COL 17 COLON-ALIGNED
     FI-Navn AT ROW 3.43 COL 39 COLON-ALIGNED NO-LABEL
     FI-Merknad AT ROW 4.57 COL 4 NO-LABEL
     FI-FraDato AT ROW 6.24 COL 18 COLON-ALIGNED
     FI-TilDato AT ROW 6.24 COL 39 COLON-ALIGNED NO-LABEL
     FI-Opprettet AT ROW 6.24 COL 107 COLON-ALIGNED
     FI-FraLevDato AT ROW 7.43 COL 18 COLON-ALIGNED
     FI-TilLevDato AT ROW 7.43 COL 39 COLON-ALIGNED NO-LABEL
     FI-Endret AT ROW 7.43 COL 107 COLON-ALIGNED
     FI-FraOrdreNr AT ROW 8.62 COL 18 COLON-ALIGNED
     FI-TilOrdreNr AT ROW 8.62 COL 39 COLON-ALIGNED NO-LABEL
     FI-Eier AT ROW 8.62 COL 107 COLON-ALIGNED
     FI-FraBestNr AT ROW 9.81 COL 18 COLON-ALIGNED
     FI-TilBestNr AT ROW 9.81 COL 39 COLON-ALIGNED NO-LABEL
     cmbBekreftet AT ROW 9.81 COL 107 COLON-ALIGNED
     FI-FraKateg AT ROW 11 COL 18 COLON-ALIGNED
     FI-TilKateg AT ROW 11 COL 39 COLON-ALIGNED NO-LABEL
     T-Annonse AT ROW 12.19 COL 20
     T-Butikk AT ROW 15.91 COL 6.8
     B-Butikker AT ROW 17.1 COL 6
     B-Varemerker AT ROW 17.1 COL 50
     FI-Varemerker AT ROW 17.1 COL 73 COLON-ALIGNED NO-LABEL
     btnUtvalgProdusent AT ROW 17.1 COL 94 WIDGET-ID 2
     fi-Produsent AT ROW 17.1 COL 116.6 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FI-Butiker AT ROW 17.14 COL 29 COLON-ALIGNED NO-LABEL
     B-VarGr AT ROW 18.29 COL 6
     FI-VAreGr AT ROW 18.29 COL 29 COLON-ALIGNED NO-LABEL
     B-Sesong AT ROW 18.29 COL 50
     FI-Sesong AT ROW 18.29 COL 73 COLON-ALIGNED NO-LABEL
     B-LevBas AT ROW 19.48 COL 6
     FI-LevBas AT ROW 19.48 COL 29 COLON-ALIGNED NO-LABEL
     B-Farg AT ROW 19.48 COL 50
     FI-Farg AT ROW 19.48 COL 73 COLON-ALIGNED NO-LABEL
     B-BestStat AT ROW 20.67 COL 6
     FI-BestStat AT ROW 20.67 COL 29 COLON-ALIGNED NO-LABEL
     B-Material AT ROW 20.67 COL 50
     FI-Material AT ROW 20.67 COL 73 COLON-ALIGNED NO-LABEL
     FI-Info AT ROW 22 COL 3 NO-LABEL
     BUTTON-SokTilDato AT ROW 7.43 COL 57.6
     B-Rediger AT ROW 1.33 COL 119.8
     B-Angre AT ROW 1.29 COL 17
     B-Bygg AT ROW 1.29 COL 7
     B-Exit AT ROW 1.29 COL 133.8
     B-Forrige AT ROW 1.29 COL 32
     B-Neste AT ROW 1.29 COL 27
     B-Ny AT ROW 1.29 COL 2
     B-Slett AT ROW 1.29 COL 12
     B-Utskrift AT ROW 1.29 COL 22
     Btn_Help AT ROW 1.29 COL 129
     BUTTON-SokDato-2 AT ROW 6.24 COL 57.4
     BUTTON-SokDato AT ROW 6.24 COL 36.2
     FILL-IN-Tekst AT ROW 14.57 COL 3 NO-LABEL
     RECT-2 AT ROW 15.29 COL 2
     RECT-5 AT ROW 5.81 COL 2.2
     RECT-6 AT ROW 2.86 COL 2
     RECT-7 AT ROW 1.1 COL 1
     RECT-8 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138 BY 22.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tBild T "NEW SHARED" NO-UNDO Temp-DB Bild
      TABLE: tmpFarg T "NEW SHARED" NO-UNDO skotex Farg
      TABLE: tmpLevBas T "NEW SHARED" NO-UNDO skotex LevBas
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kolleksjonsliste"
         HEIGHT             = 22.81
         WIDTH              = 138
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
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
/* SETTINGS FOR BUTTON B-ShowRoom IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Slett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Utskrift IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BestStat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Eier IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Endret IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Farg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Info IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-LevBas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ListeNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Material IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Merknad IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Opprettet IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-Produsent IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fi-Produsent:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-Sesong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VAreGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Varemerker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Tekst IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 13.62
       COLUMN          = 2
       HEIGHT          = .71
       WIDTH           = 136
       HIDDEN          = no
       SENSITIVE       = no.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {35053A22-8589-11D1-B16A-00C0F0283628} type: ProgressBar */
      CtrlFrame:MOVE-AFTER(T-Annonse:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kolleksjonsliste */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kolleksjonsliste */
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


&Scoped-define SELF-NAME B-BestStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BestStat C-Win
ON CHOOSE OF B-BestStat IN FRAME DEFAULT-FRAME /* Bestillingsstatus... */
DO:
  def var IO-Liste as char no-undo.

  if wBestStatus = wAlle or wBestStatus = "" then
    RUN Initbeststatus.

  assign
    IO-Liste = if wvBestStatus = wAlle
                 then ""
                 else wvBestStatus.
    
  run d-tagbeststat.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvBestStatus = if IO-Liste = ""
                       then wAlle
                     else if IO-Liste = wBestStatus
                       then wAlle
                     else IO-Liste
    FI-BestStat  = if wvBestStatus = wAlle
                     then wAlle
                     else string(num-entries(wvBestStatus)) + " av " + string(num-entries(wBestStatus)).
  display 
    FI-BestStat
  with frame {&FRAME-NAME}.
  
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


&Scoped-define SELF-NAME B-Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Farg C-Win
ON CHOOSE OF B-Farg IN FRAME DEFAULT-FRAME /* Farger... */
DO:
  def var IO-Liste as char no-undo.

  if wFarger = wAlle then
    RUN InitFarg.

  assign
    IO-Liste = if wvFarger = wAlle
                 then ""
                 else wvFarger.
    
  run d-tagfarg.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvFarger = if IO-Liste = ""
                 then wAlle
               else if IO-Liste = wFarger
                 then wAlle
               else IO-Liste
    FI-Farg  = if wvFarger = wAlle
                 then wAlle
                 else string(num-entries(wvFarger)) + " av " + string(num-entries(wFarger)).
  display 
    FI-Farg
  with frame {&FRAME-NAME}.
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


&Scoped-define SELF-NAME B-Grid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Grid C-Win
ON CHOOSE OF B-Grid IN FRAME DEFAULT-FRAME /* Bildegrid... */
DO: 
  /* Utføres kun når liste finnes. */
  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".

  {sww.i}    
  run ByggTempListe.
  {swn.i}    
    
  /* Teller opp antall poster. */
  wNumRecords = 0.
  for each ListeLinje of Lister no-lock:
    wNumRecords = wNumRecords + 1.
  end.
  
  wMenuAction = "INIT".
  run kollgridmeny.p (wMenuAction, "", output wMenu).   
  
  IF VALID-HANDLE(wChild) THEN
      APPLY "CLOSE" TO wChild.

  IF NOT VALID-HANDLE(wChild) THEN  
    RUN w-bildegrid.w PERSISTENT 
        SET wChild (THIS-PROCEDURE:HANDLE,wMenu,wScreenSize,wNumRecords).
  
  IF VALID-HANDLE(wChild) THEN
      RUN SettTittel IN wChild (" Sortering: " + CB-Sortering:SCREEN-VALUE).

  assign
  chCtrlFrame:ProgressBar:Min   = 0
  chCtrlFrame:ProgressBar:Max   = 100
  chCtrlFrame:ProgressBar:Value = 0.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevBas C-Win
ON CHOOSE OF B-LevBas IN FRAME DEFAULT-FRAME /* Leverandører... */
DO:
  def var IO-Liste as char no-undo.

  if wLeverandorer = wAlle then
    RUN InitLeverandor.

  assign
    IO-Liste = if wvLeverandorer = wAlle
                 then ""
                 else wvLeverandorer.
    
  run d-taglevbas.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvLeverandorer = if IO-Liste = ""
                       then wAlle
                     else if IO-Liste = wLeverandorer
                       then wAlle
                     else IO-Liste
    FI-LevBas  = if wvLeverandorer = wAlle
                   then wAlle
                   else string(num-entries(wvLeverandorer)) + " av " + string(num-entries(wLeverandorer)).
    
  display 
    FI-LevBas
  with frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Material
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Material C-Win
ON CHOOSE OF B-Material IN FRAME DEFAULT-FRAME /* Material... */
DO:
  def var IO-Liste as char no-undo.

  if wMaterial = wAlle then
    RUN InitMaterial.

  assign
    IO-Liste = if wvMaterial = wAlle
                 then ""
                 else wvMaterial.
    
  run d-tagmaterial.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvMaterial = if IO-Liste = ""
                   then wAlle
                 else if IO-Liste = wMaterial
                   then wAlle
                 else IO-Liste
    FI-Material  = if wvMaterial = wAlle
                     then wAlle
                     else string(num-entries(wvMaterial)) + " av " + string(num-entries(wMaterial)).
  display 
    FI-Material
  with frame {&FRAME-NAME}.
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


&Scoped-define SELF-NAME B-Rediger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rediger C-Win
ON CHOOSE OF B-Rediger IN FRAME DEFAULT-FRAME /* Liste... */
DO:
  /* Utføres kun når liste finnes. */
  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".

  {sww.i}    
  run ByggTempListe.
  {swn.i}    
  
  run w-bstartbilder.w (wListerRecid, wCellNr, this-procedure:handle). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sesong C-Win
ON CHOOSE OF B-Sesong IN FRAME DEFAULT-FRAME /* Sesong... */
DO:
  def var IO-Liste as char no-undo.

  if wSesonger = wAlle then
    RUN InitSasong.

  assign
    IO-Liste = if wvSesonger = wAlle
                 then ""
                 else wvSesonger.
    
  run d-tagsasong.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvSesonger = if IO-Liste = ""
                       then wAlle
                     else if IO-Liste = wSesonger
                       then wAlle
                     else IO-Liste
    FI-Sesong  = if wvSesonger = wAlle
                   then wAlle 
                   else string(num-entries(wvSesonger)) + " av " + string(num-entries(wSesonger)).
  display 
    FI-Sesong
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ShowRoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ShowRoom C-Win
ON CHOOSE OF B-ShowRoom IN FRAME DEFAULT-FRAME /* ShowRoom... */
DO: 
  /* Utføres kun når liste finnes. */
    RETURN NO-APPLY.
  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".

  /*
  {sww.i}    
  run ByggTempListe.
  {swn.i}    
  */
    
  FIND FIRST ListeLinje OF Lister NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ListeLinje THEN
  DO:
      MESSAGE "Det er ingen artikler i den valgte listen!"
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
  END.

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = dec(entry(1,ListeLinje.DataObjekt)) NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
      MESSAGE "Ukjent artikkel på listeraden. Listen må bygges om!"
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN NO-APPLY.
  END.
  if available ArtBas then
    do:
      find BildeRegister of ArtBas no-error.
      if available BildeRegister then
        run d-showroom (input recid(BildeRegister), wListerRecid).
    end.

  RETURN NO-APPLY.
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
    RUN SlettKoleksjon.
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
    return no-apply.

  assign frame DEFAULT-FRAME
    FI-ListeNr.
    
  run w-rutskrkolleksjon.w persistent set wUtskriftProg (Lister.ListeType).
  if valid-handle(wUtskriftProg) then
    run InitListe in wUtskriftProg (input wListerRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Varemerker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Varemerker C-Win
ON CHOOSE OF B-Varemerker IN FRAME DEFAULT-FRAME /* Varemerker... */
DO:
  def var IO-Liste as char no-undo.

  if wVmId = wAlle then
    RUN InitVareMerker.

  assign
    IO-Liste = if wvVmId = wAlle
                 then ""
                 else wvVmId.
    
  run d-tagvaremerke.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvVmId = if IO-Liste = ""
                       then wAlle
                     else if IO-Liste = wVmId
                       then wAlle
                     else IO-Liste
    FI-Varemerker  = if wvVmId = wAlle
                    then wAlle 
                    else string(num-entries(wvVmId)) + " av " + string(num-entries(wVmId)).
  display 
    FI-Varemerker
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VarGr C-Win
ON CHOOSE OF B-VarGr IN FRAME DEFAULT-FRAME /* Varegrupper... */
DO:
  def var IO-Liste as char no-undo.

  if wVareGrupper = wAlle then 
    RUN InitVaregrupper.
    
  assign
    IO-Liste = if wvVareGrupper = wAlle
                 then ""
                 else wvVareGrupper.
  
  run d-tagvaregr.w (input-output IO-Liste).
  IF RETURN-VALUE = "Avbryt" THEN
        RETURN NO-APPLY.
  assign
    wvVareGrupper = if IO-Liste = ""
                      then wAlle
                    else if IO-Liste = wVareGrupper
                      then wAlle
                    else IO-Liste
    FI-VareGr     = if wvVareGrupper = wAlle
                      then wAlle
                      else string(num-entries(wvVareGrupper)) + " av " + string(num-entries(wVareGrupper)).
  display 
    FI-VareGr
  with frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgProdusent C-Win
ON CHOOSE OF btnUtvalgProdusent IN FRAME DEFAULT-FRAME /* Produsent... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Produsent;ProdNr;Beskrivelse;Adresse1;Land",
                      "where true",
                      INPUT-OUTPUT cProdusentRowIdList,
                      "ProdNr",
                      INPUT-OUTPUT cProdusentIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  FI-Produsent:SCREEN-VALUE = cProdusentIdList.
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


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraDato
DO:

  def var wTittel as char no-undo.
  assign 
      FI-FraDato = date(FI-FraDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-Win
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilDato
DO:

  def var wTittel as char no-undo.
  assign FI-TilDato = date(FI-FraDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLevDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLevDato C-Win
ON CHOOSE OF BUTTON-SokLevDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraLevDato
DO:

  def var wTittel as char no-undo.
  assign 
      FI-FraLevDato = date(FI-FraLevDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraLevDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTilDato C-Win
ON CHOOSE OF BUTTON-SokTilDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-TilLevDato
DO:

  def var wTittel as char no-undo.
  assign FI-TilLevDato = date(FI-FraLevDato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilLevDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-WinSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-WinSize C-Win
ON VALUE-CHANGED OF COMBO-BOX-WinSize IN FRAME DEFAULT-FRAME
DO:
  ASSIGN wScreenSize = LOOKUP(SELF:SCREEN-VALUE,SELF:LIST-ITEMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraDato C-Win
ON DELETE-CHARACTER OF FI-FraDato IN FRAME DEFAULT-FRAME /* Bestillingsdato */
DO:
  ASSIGN
      FI-FraDato:SCREEN-VALUE = " ".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraLevDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraLevDato C-Win
ON DELETE-CHARACTER OF FI-FraLevDato IN FRAME DEFAULT-FRAME /* Leveringsdato */
DO:
  ASSIGN
      FI-FraLevDato:SCREEN-VALUE = " ".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilDato C-Win
ON DELETE-CHARACTER OF FI-TilDato IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        FI-TilDato:SCREEN-VALUE = " ".
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilLevDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilLevDato C-Win
ON DELETE-CHARACTER OF FI-TilLevDato IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
      FI-TilLevDato:SCREEN-VALUE = " ".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Butikk C-Win
ON VALUE-CHANGED OF T-Butikk IN FRAME DEFAULT-FRAME /* Pr. butikk */
DO:
  assign
    B-Butikker:sensitive = input T-Butikk
    B-Rediger:sensitive  = not input T-Butikk
    B-Grid:sensitive     = not input T-Butikk.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{syspara.i 7   1 1 wListeType}
{syspara.i 1 100 1 wAlle}
{syspara.i 5   1 1 wCl INT}

/* Henter sentrallager */
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = wCl NO-ERROR.

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

assign
  COMBO-BOX-WinSize = entry(wScreenSize, COMBO-BOX-WinSize:list-items).

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i} 

  RUN InitParametre.
  RUN InitSortering.  
  
  find last Lister no-lock where
    Lister.ListeType = wListeType and
    Lister.Eier      = userid("dictdb") and
    Lister.ListeNr   > 0 no-error.

  /*
  if available Lister then
    do:
      assign
        wModus       = "ENDRE"
        wListerRecid = recid(Lister).
      RUN VisListe.
      RUN DefaultKnapper.
    end.
  else
    apply "choose":U to B-Ny. /* Setter NY modus */
  if wModus = "NY" then  
    apply "entry":U to FI-ListeNr.
  else
    apply "entry":U to FI-Navn.
  */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseRefresh C-Win 
PROCEDURE BrowseRefresh :
/*------------------------------------------------------------------------------
  Purpose:     Dummy for å kunne håndtere BildeGrid.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTempListe C-Win 
PROCEDURE ByggTempListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {byggtmpliste.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "w-rbyggkolleksjon.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-rbyggkolleksjon.wrx":U SKIP(1)
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
    FI-FraDato     = ?
    FI-TilDato     = ?
    FI-FraLevDato  = ?
    FI-TilLevDato  = ?
    FI-FraBestNr   = 1
    FI-TilBestNr   = 99999999
    FI-FraOrdreNr  = 0
    FI-TilOrdreNr  = 99999999
    FI-Eier        = userid("DictDB")
    wvVareGrupper  = wAlle
    wvLeverandorer = wAlle
    wvBestStatus   = wAlle
    wvSesonger     = wAlle
    wvFarger       = wAlle
    wvMaterial     = wAlle
    wvButiker      = wAlle
    wvVmId         = wAlle
    FI-BestStat    = if wvBestStatus = wAlle
                       then wAlle
                       else string(num-entries(wvBestStatus))   + " av " + string(num-entries(wBestStatus)) 
    FI-Sesong      = if wvSesonger = wAlle
                       then wAlle
                       else string(num-entries(wvSesonger))     + " av " + string(num-entries(wSesonger)) 
    FI-LevBas      = if wvLeverandorer = wAlle
                       then wAlle
                       else string(num-entries(wvLeverandorer)) + " av " + string(num-entries(wLeverandorer)) 
    FI-VareGr      = if wvVareGrupper = wAlle
                       then wAlle
                       else string(num-entries(wvVareGrupper))  + " av " + string(num-entries(wVareGrupper)) 
    FI-Farg        = if wvFarger = wAlle
                       then wAlle
                       else string(num-entries(wvFarger))       + " av " + string(num-entries(wFarger)) 
    FI-Material    = if wvMaterial = wAlle
                       then wAlle
                       else string(num-entries(wvMaterial))     + " av " + string(num-entries(wMaterial)) 
    FI-Butiker     = if wvButiker = wAlle
                       then wAlle
                       else string(num-entries(wvButiker))      + " av " + string(num-entries(wButiker)).
    FI-Varemerker  = if wvVmId = wAlle
                       then wAlle
                       else string(num-entries(wvVmId))      + " av " + string(num-entries(wVmId))
  .
cmbBekreftet:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
ASSIGN cmbBekreftet.

  FI-Info = "".
  display 
    FI-Info
    FI-Eier
    FI-BestStat
    FI-Sesong  
    FI-LevBas  
    FI-VareGr  
    FI-Farg    
    FI-Material
    FI-Butiker
    FI-FraDato     
    FI-TilDato     
    FI-FraLevDato   
    FI-TilLevDato   
    FI-FraBestNr
    FI-TilBestNr
    FI-FraOrdreNr 
    FI-TilOrdreNr 
    FI-Varemerker
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
  DISPLAY COMBO-BOX-WinSize CB-Sortering FI-ListeNr FI-Navn FI-Merknad 
          FI-FraDato FI-TilDato FI-Opprettet FI-FraLevDato FI-TilLevDato 
          FI-Endret FI-FraOrdreNr FI-TilOrdreNr FI-Eier FI-FraBestNr 
          FI-TilBestNr cmbBekreftet FI-FraKateg FI-TilKateg T-Annonse T-Butikk 
          FI-Varemerker fi-Produsent FI-Butiker FI-VAreGr FI-Sesong FI-LevBas 
          FI-Farg FI-BestStat FI-Material FI-Info FILL-IN-Tekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-5 RECT-6 RECT-7 RECT-8 B-Grid BUTTON-SokLevDato 
         COMBO-BOX-WinSize CB-Sortering B-SokListe FI-Navn FI-Merknad 
         FI-FraDato FI-TilDato FI-FraLevDato FI-TilLevDato FI-FraOrdreNr 
         FI-TilOrdreNr FI-FraBestNr FI-TilBestNr cmbBekreftet FI-FraKateg 
         FI-TilKateg T-Annonse T-Butikk B-Varemerker btnUtvalgProdusent B-VarGr 
         B-Sesong B-LevBas B-Farg B-BestStat B-Material BUTTON-SokTilDato 
         B-Rediger B-Exit B-Ny Btn_Help BUTTON-SokDato-2 BUTTON-SokDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initbeststatus C-Win 
PROCEDURE Initbeststatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
--------------------------------------------------------------------------------*/
  FI-Info = "Initiering av bestillingsstatus pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wBestStatus = "".

  /* Bestillingsstatus - Bruker en passende temprærtabell. */
  for each tmpFarg: delete tmpFarg. End.
  SYSPARA:
  for each SysPAra no-lock where
    SysPara.SysHId = 5 and
    SysPara.SysGr  = 2 and
    SysPara.ParaNr < 99:
    
    create tmpFarg.
    assign
      tmpFarg.Farg     = SysPara.ParaNr
      tmpFarg.FarBeskr = SysPara.Parameter1     
      wBestStatus = wBestStatus + 
                   (if wBestStatus = "" 
                      then ""
                      else ",") +
                   string(SysPara.ParaNr).    
  end. /* SYSPARA */

  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFarg C-Win 
PROCEDURE InitFarg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FI-Info = "Initiering av farger pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wFarger = "".

  /* Farger */
  for each Farg no-lock:
    assign
      wFarger = wFarger + 
                (if wFarger = ""
                   then ""
                   else ",") + 
                string(Farg.Farg).
  end.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLeverandor C-Win 
PROCEDURE InitLeverandor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FI-Info = "Initiering av leverandører pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wLeverandorer = "".

  /* Leverandører - Leses fra Bestillingene */
  for each tmpLevBas: delete tmpLevBas. end.
  /*
  for each BestHode no-lock where
    BestHode.BestStat <= 7:
    
    if not can-do(wLeverandorer,string(BestHode.LevNr)) then
      do:
        find LevBas no-lock where
          LevBas.LevNr = BestHode.LevNr.
        assign
          wLeverandorer = wLeverandorer + 
                          (if wLeverandorer = ""
                             then ""
                             else ",") + 
                          string(BestHode.LevNr).    
        create tmpLevBas.
        assign
          tmpLevBas.LevNr   = BestHode.LevNr
          tmpLevBas.LevNamn = LevBas.LeVNamn.          
      end.
  end.
  */
  for each LevBas no-lock where LevBas.LevNr > 0:
    assign
      wLeverandorer = wLeverandorer + 
                      (if wLeverandorer = ""
                         then ""
                         else ",") + string(LevBas.LevNr).
    create tmpLevBas.
    assign
      tmpLevBas.LevNr   = LevBas.LevNr
      tmpLevBas.LevNamn = LevBas.LeVNamn.          
  end.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitMaterial C-Win 
PROCEDURE InitMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FI-Info = "Initiering av material pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wMaterial = "".
  
  /* Material */
  for each Material no-lock:
    assign
      wMaterial = wMaterial + 
                  (if wMaterial = ""
                     then ""
                     else ",") + 
                  string(Material.MatKod).
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
    wVareGrupper  = wAlle
    wLeverandorer = wAlle
    wBestStatus   = wAlle
    wSesonger     = wAlle
    wFarger       = wAlle
    wMaterial     = wAlle
    wButiker      = wAlle 
    wVmId         = wAlle  

    wvVareGrupper  = wAlle
    wvLeverandorer = wAlle
    wvBestStatus   = wAlle
    wvSesonger     = wAlle
    wvFarger       = wAlle
    wvMaterial     = wAlle
    wvButiker      = wAlle   
    wvVmId          = wAlle.
    cmbBekreftet:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = wAlle + (IF wCurrLng = "SE" THEN ",0,Ja,1,Nej,2" ELSE ",0,Ja,1,Nei,2").
  run DefaultVerdier.
  {swn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSasong C-Win 
PROCEDURE InitSasong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FI-Info = "Initiering av sesonger pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wSesonger = "".

  /* Sesonger. */
  for each Sasong no-lock:
    assign
      wSesonger = wSesonger + 
                  (if wSesonger = ""
                     then ""
                     else ",") + 
                  string(Sasong.Sasong).
  end.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSortering C-Win 
PROCEDURE InitSortering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Setter opp frame-scoop. */
  do with frame DEFAULT-FRAME:  
  assign
    CB-Sortering = " "
    CB-Sortering:List-Items = " ".

  for each SysPara no-lock where
    SysPara.SysHId = 9 and
    SysPara.SysGr  = 10:
          
    CB-Sortering:List-items = CB-Sortering:list-items + 
                              (if CB-Sortering:list-items = ""
                                 then ""
                                 else ",") + 
                               string(SysPara.ParaNr,"zzz9") + ": " + 
                               SysPara.Parameter1.
  end.      
  assign
    CB-Sortering = entry(1,CB-Sortering:list-items).
  display
    CB-Sortering
  with frame DEFAULT-FRAME.
  end. /* FRAMESCOOP */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVaregrupper C-Win 
PROCEDURE InitVaregrupper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FI-Info = "Initiering av varegrupper pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wVareGrupper = "".

  /* Varegrupper */
  for each VarGr no-lock where VarGr.Vg > 0:
    assign
      wVareGrupper = wVareGrupper + 
                     (if wVareGrupper = "" 
                        then ""
                        else ",") +
                     string(VarGr.Vg).
  end.
  
  FI-Info = "".
  display FI-Info with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVaremerker C-Win 
PROCEDURE InitVaremerker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FI-Info = "Initiering av varemerker pågår...".
  display FI-Info with frame {&FRAME-NAME}.

  wVmId = "".

  /* Varemerker. */
  for each Varemerke no-lock:
    assign
      wVmId = wVmId + 
                  (if wVmId = ""
                     then ""
                     else ",") + 
                  string(Varemerke.VmId).
  end.
  
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
  /* Tull med dato */
  IF INPUT FI-FraDato <> ? AND INPUT FI-TilDato <> ? THEN
  DO:
    if input FI-FraDato > input FI-TilDato then
      do:
        message "Dato TIL er mindre enn dato FRA!"
          view-as alert-box message title "Melding".
        return no-apply "AVBRYT".
      end.
  END.

  /* Tull med LevTid */
  IF INPUT FI-FraLEvDato <> ? AND FI-FraLEvDato <> ? THEN
  DO:
    if input FI-FraLevDato > input FI-TilLevDato then
      do:
        message "Fra leveringsdato er større enn til leveringsdato!"
          view-as alert-box message title "Melding".
        return no-apply "AVBRYT".
    end.
  END.

  /* Tull med bestillingsnummer */
  if input FI-FraBestNr > input FI-TilBestNr then
    do:
      message "Leveringsuke TIL er mindre enn leveringsuke FRA!"
        view-as alert-box message title "Melding".
      return no-apply "AVBRYT".
    end.

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
      RUN SlettKoleksjon.
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
    FI-FraDato 
    FI-TilDato
    FI-FraLevDato
    FI-TilLevDato
    FI-FraOrdreNr
    FI-TilOrdreNr
    FI-FraBestNr
    FI-TilBestNr
    FI-FraKateg
    FI-TilKateg
    T-Annonse
    fi-Produsent
    cmbBekreftet.

/*
message "TEST LagreListe i pgrogram w-rbyggkolleksjon.w" skip
  "wListerRecid"        wListerRecid skip
  "FI-FraDato"          FI-FraDato skip
  "FI-TilDato"          FI-TilDato skip
  "FI-FraLevUke"        FI-FraLevUke skip
  "FI-TilLevUke"        FI-TilLevUke skip
  "FI-FraBestNr"        FI-FraBestNr skip
  "FI-TilBestNr"        FI-TilBestNr skip
  "FI-FraKateg"         FI-FraKateg skip
  "FI-TilKateg"         FI-TilKateg skip
  "T-Butikk"            T-Butikk skip
  "wvVareGrupper"       wvVareGrupper skip
  "wvLeverandorer"      wvLeverandorer skip
  "wvBestStatus"        wvBestStatus skip
  "wvSesonger"          wvSesonger skip
  "wvFarger"            wvFarger skip
  "wvMaterial"          wvMaterial skip
  "wvButiker"           wvButiker skip
view-as alert-box.
*/

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
      bLister.Kriterier[ 1] = wvVareGrupper + "|" + wVareGrupper 
      bLister.Kriterier[ 2] = wvLeverandorer + "|" + wLeverandorer
      bLister.Kriterier[ 3] = wvBestStatus + "|" + wBestStatus  
      bLister.Kriterier[ 4] = wvSesonger + "|" + wSesonger    
      bLister.Kriterier[ 5] = wvFarger + "|" + wFarger      
      bLister.Kriterier[ 6] = wvMaterial + "|" + wMaterial    
      bLister.Kriterier[ 7] = wvButiker + "|" + wButiker
      bLister.Kriterier[ 8] = if T-Butikk 
                               then "TRUE"
                               else "FALSE"
      bLister.Kriterier[ 9] = IF FI-FraDato = ?
                                   THEN ""
                                   ELSE STRING(FI-FraDato) + ";" +
                              IF FI-TilDato = ? 
                                   THEN ""
                                   ELSE STRING(FI-TilDato)
      bLister.Kriterier[10] = (IF INPUT FI-FraLevDato = ?
                                 THEN ""
                                 ELSE string(INPUT FI-FraLevDato)) + ";" + 
                              (IF INPUT FI-TilLevDato = ?
                                 THEN ""
                                 ELSE string(FI-TilLevDato))
      bLister.Kriterier[11] = string(FI-FraBestNr) + ";" + string(FI-TilBestNr)
      bLister.Kriterier[12] = string(FI-FraKateg) + ";" + string(FI-TilKateg)
      bLister.Kriterier[13] = string(FI-FraOrdreNr) + ";" + string(FI-TilOrdreNr)
      bLister.Kriterier[14] = if T-Annonse 
                               then "TRUE"
                               else "FALSE"
      bLister.Kriterier[15] = wvVmId + "|" + wVmId
      bLister.Kriterier[16] = fi-Produsent
      bLister.Kriterier[17] = cmbBekreftet.

      
      release bLister.
  end. /* TRANSACTION */
    
  find Lister no-lock where
    recid(Lister) = wListerRecid.           
  
  /* Her bygger og oppdaterer vi */
  if wModus = "NY" then
    do:
      run byggkolleksjon.p persistent set wChild
                         (wListerRecid,
                          FI-FraDato,
                          FI-TilDato,
                          FI-FraLevDato,
                          FI-TilLevDato,
                          FI-FraBestNr,
                          FI-TilBestNr,
                          FI-FraOrdreNr,
                          FI-TilOrdreNr,
                          FI-FraKateg,
                          FI-TilKateg,
                          T-Butikk,
                          wvVareGrupper,
                          wvLeverandorer,
                          wvBestStatus,
                          wvSesonger,
                          wvFarger,
                          wvMaterial,
                          wvButiker,
                          wvVmId,
                          fi-Produsent,
                          T-Annonse,
                          cmbBekreftet,
                          this-procedure:handle).
    end.
  else do:
    run byggkolleksjon.p persistent set wChild
                       (wListerRecid,
                        FI-FraDato,
                        FI-TilDato,
                        FI-FraLevDato,
                        FI-TilLevDato,
                        FI-FraBestNr,
                        FI-TilBestNr,
                        FI-FraOrdreNr,
                        FI-TilOrdreNr,
                        FI-FraKateg,
                        FI-TilKateg,
                        T-Butikk,
                        wvVareGrupper,
                        wvLeverandorer,
                        wvBestStatus,
                        wvSesonger,
                        wvFarger,
                        wvMaterial,
                        wvButiker,
                        wvVmId,
                        fi-Produsent,
                        T-Annonse,
                        cmbBekreftet,
                        this-procedure:handle).
  end.    

  {swn.i}    
  FI-Info = "".
  run Disp-Info (FI-Info).
  run VisListe.
  
  return "OK".
end. /* FrameScoope */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenyValg C-Win 
PROCEDURE MenyValg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wRowId      AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER wMenuAction AS CHAR  NO-UNDO.    
    
    def var wTekst as char.
    
    def buffer bbListeLinje for ListeLinje.

    find tBild where
      rowid(tBild) = wRowId no-error.
    if available tBild then
      find bbListeLinje no-lock where
        recid(bbListeLinje) = tBild.ListeLinje no-error.
    if available bbListeLinje then      
      run kollgridmeny.p (wMenuAction, bbListeLinje.DataObjekt, output wTekst).
    
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = tBild.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBAs THEN
    TEKSTRAD:
    DO:
      FIND BestHode NO-LOCK WHERE
          BestHode.BestNr = tBild.BestNr NO-ERROR.
      IF NOT AVAILABLE BestHode THEN
          LEAVE TEKSTRAD.

      RUN TekstLinje (1).
      RUN TekstLinje (2).
    END.
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
    FI-FraDato    = ?
    FI-TilDato    = ?
    FI-FraLevDato = ?
    FI-TilLevDato = ?
    FI-Merknad    = ""
    FI-ListeNr    = 0
    FI-Navn       = ""
    FI-Eier       = ""
    T-Butikk      = false
    FI-FraKateg   = 1
    FI-TilKateg   = 99
    FI-FraBestNr  = 1
    FI-TilBestNr  = 9999999
    FI-FraOrdreNr = 0
    FI-TilORdreNr = 9999999.

  display
    FI-FraDato   
    FI-TilDato   
    FI-FraLevDato 
    FI-TilLevDato 
    FI-Merknad 
    FI-ListeNr   
    FI-Navn
    FI-Eier
    T-Butikk     
    FI-FraKateg  
    FI-TilKateg  
    FI-FraBestNr 
    FI-TilBestNr 
    FI-FraOrdreNr 
    FI-TilORdreNr 
  with frame DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettCelleNr C-Win 
PROCEDURE SettCelleNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  for each tBild exclusive-lock where
    tBild.CellNr <> ?:

    find ListeLinje exclusive-lock where
      recid(ListeLinje) = tBild.ListeLinje no-error.
    if available ListeLinje then
      ListeLinje.CellNr = tBild.CellNr.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettTekstfelt C-Win 
PROCEDURE SettTekstfelt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pRowId AS ROWID NO-UNDO.
                     
  DO TRANSACTION:
    FIND tBild EXCLUSIVE-LOCK WHERE
        ROWID(tBild) = pRowId NO-ERROR.
    IF NOT AVAILABLE tBild THEN
        RETURN.
    FIND BestHode NO-LOCK WHERE
        BestHode.BestNr = tBild.BestNr NO-ERROR.
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = tBild.ArtikkelNr NO-ERROR.
    
    RUN TekstLinje IN THIS-PROCEDURE (1) NO-ERROR.
    RUN TekstLinje IN THIS-PROCEDURE (2) NO-ERROR.
   
  END.
  RELEASE tBild.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkoReposition C-Win 
PROCEDURE SkoReposition :
/*------------------------------------------------------------------------------
  Purpose:     Dummy rutine for å kunne starte w-bildegrid.w fra dette programmet.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wRowId AS ROWID NO-UNDO.
  
  /* Gjør ingenting */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettKoleksjon C-Win 
PROCEDURE SlettKoleksjon :
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
    RUN SlettKoleksjon.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortPoster C-Win 
PROCEDURE TaBortPoster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Ta bort slettede poster.                           */
  /* Poster som tas bort, tas også bort fra ListeLinje. */
  for each tBild exclusive-lock where
    tBild.CellNr = ?:

    find ListeLinje exclusive-lock where
      recid(ListeLinje) = tBild.ListeLinje no-error.
    if available ListeLinje then
      delete ListeLinje.

    delete tBild.    
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TekstLinje C-Win 
PROCEDURE TekstLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
  
  Forutsetter at tBild og ArtBas record er tilgjengelig.
     
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piLinje AS INT NO-UNDO.

  DEF VAR pcRGB AS CHAR NO-UNDO.

  IF NOT AVAILABLE ArtBas THEN
      RETURN.
  IF NOT AVAILABLE BestHode THEN
      RETURN.

  IF piLinje = 1 THEN
  DO:
    ASSIGN
      tBild.TekstRad1  = string(ArtBas.Vg) + "/" +
                           (if ArtBas.LopNr <> ?
                             then string(ArtBas.LopNr)
                             else "?") + " " + 
                         string(ArtBas.LevNr) + "/" + 
                         string(ArtBas.LevKod).
  END.

  IF piLinje = 2 THEN
  DO:
    /* Henter prisinformasjon */
    FIND BestPris NO-LOCK WHERE
        BestPris.BestNr   = BestHode.BestNr AND
        BestPris.BestStat = BestHode.BestStat AND
        BestPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE BestPris THEN
        FIND FIRST BestPris NO-LOCK WHERE
            BestPris.BestNr   = BestHode.BestNr AND
            BestPris.BestStat = BestHode.BestStat NO-ERROR.

    FIND Farg NO-LOCK WHERE
        Farg.Farg = ArtBAs.Farg NO-ERROR.
    ASSIGN
      tBild.TekstRad2 = (IF AVAILABLE BestPris
                           THEN string(int(BestPris.InnkjopsPris))
                           ELSE "0") + " / " +
                         string(BestHode.TotAntPar) + " / " +
                         (IF NOT AVAILABLE Farg
                            THEN ""
                          ELSE IF Farg.KFarge <> ""
                            THEN Farg.KFarge
                          ELSE
                            Farg.FarBeskr).

    /* Henter fargekode for bestillingsstatus. */
    IF BestHode.BestStat = 4 AND BestHode.bekreftetdato <> ? THEN
        {syspar2.i 5 2 8 pcRGB}
    ELSE
        {syspar2.i 5 2 BestHode.BestStat pcRGB}
    assign
      tBild.Farg      = Hex2Int(pcRGB)
      .

  END.


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
    wModus         = "ENDRE"
    FI-Opprettet   = string(Lister.RegistrertDato) + " " +
                     string(Lister.RegistrertTid,"HH:MM:SS") + " " +
                     Lister.RegistrertAv
    FI-Endret      = string(Lister.EDato) + " " +
                     string(Lister.ETid,"HH:MM:SS") + " " +
                     Lister.BrukerId
    FI-Info        = ""
    FI-Merknad     = Lister.Merknad
    FI-ListeNr     = Lister.ListeNr
    FI-Navn        = Lister.Beskrivelse
    FI-Eier        = Lister.Eier
    wvVareGrupper  = entry(1,Lister.Kriterier[ 1],"|")
    wvLeverandorer = entry(1,Lister.Kriterier[ 2],"|")
    wvBestStatus   = entry(1,Lister.Kriterier[ 3],"|")
    wvSesonger     = entry(1,Lister.Kriterier[ 4],"|")
    wvFarger       = entry(1,Lister.Kriterier[ 5],"|")
    wvMaterial     = entry(1,Lister.Kriterier[ 6],"|")
    wvButiker      = entry(1,Lister.Kriterier[ 7],"|")
    T-Butikk       = if Lister.Kriterier[8] = "TRUE"
                       then true
                       else false    
    T-Annonse      = if Lister.Kriterier[14] = "TRUE"
                       then true
                       else false    
    wvVmId         = entry(1,Lister.Kriterier[15],"|")
    fi-Produsent   = Lister.Kriterier[16]
    cmbBekreftet   = Lister.Kriterier[17]
    .

  /* Initierer lister hvis det er nødvendig */
  if wvVareGrupper <> wAlle and wVareGrupper = wAlle then run InitVaregrupper.
  if wvLeverandorer <> wAlle and wLeverandorer = wAlle then RUN InitLeverandor. 
  if wvBestStatus <> wAlle and wBestStatus = wAlle then RUN Initbeststatus.  
  if wvSesonger <> wAlle and wSesonger = wAlle then RUN InitSasong.   
  if wvFarger <> wAlle and wFarger = wAlle then RUN InitFarg.     
  if wvMaterial <> wAlle and wMaterial = wAlle then RUN InitMaterial.   
  if wvButiker <> wAlle and wButiker = wAlle then RUN InitButiker.    
  if wvVmId <> wAlle and wVmId = wAlle then RUN InitVaremerker.   

  /* Hurra hvis det lykkes. */
  DO ON ERROR UNDO,LEAVE:
    IF NUM-ENTRIES(Lister.Kriterier[9],";") = 2 THEN
      ASSIGN
        FI-FraDato = DATE(ENTRY(1,Lister.Kriterier[9],";"))
        FI-TilDato = DATE(ENTRY(2,Lister.Kriterier[9],";"))
        .
    ELSE
      ASSIGN
        FI-FraDato = ?
        FI-TilDato = ?
        .
  END.
  assign
    FI-FraLevDato  = if num-entries(Lister.Kriterier[10],";") <> 2 
                       then ?
                     else date(entry(1,Lister.Kriterier[10],";"))
    FI-TilLevDato  = if num-entries(Lister.Kriterier[10],";") <> 2 
                       then ?
                     else date(entry(2,Lister.Kriterier[10],";"))
    FI-FraBestNr   = if num-entries(Lister.Kriterier[11],";") <> 2 
                       then FI-FraBestNr
                     else int(entry(1,Lister.Kriterier[11],";"))
    FI-TilBestNr   = if num-entries(Lister.Kriterier[11],";") <> 2 
                       then FI-TilBestNr
                     else int(entry(2,Lister.Kriterier[11],";"))
    FI-FraKateg    = if num-entries(Lister.Kriterier[12],";") <> 2 
                       then FI-FraKateg
                     else int(entry(1,Lister.Kriterier[12],";"))
    FI-TilKateg    = if num-entries(Lister.Kriterier[12],";") <> 2 
                       then FI-TilKateg
                     else int(entry(2,Lister.Kriterier[12],";"))                     
    FI-FraOrdreNr   = if num-entries(Lister.Kriterier[13],";") <> 2 
                       then FI-FraOrdreNr
                     else int(entry(1,Lister.Kriterier[13],";"))
    FI-TilOrdreNr   = if num-entries(Lister.Kriterier[13],";") <> 2 
                       then FI-TilOrdreNr
                     else int(entry(2,Lister.Kriterier[13],";"))
    FI-BestStat    = if wvBestStatus = wAlle
                       then wAlle
                       else string(num-entries(wvBestStatus))   + " av " + string(num-entries(wBestStatus)) 
    FI-Sesong      = if wvSesonger = wAlle
                       then wAlle
                       else string(num-entries(wvSesonger))     + " av " + string(num-entries(wSesonger)) 
    FI-LevBas      = if wvLeverandorer = wAlle
                       then wAlle
                       else string(num-entries(wvLeverandorer)) + " av " + string(num-entries(wLeverandorer)) 
    FI-VareGr      = if wvVareGrupper = wAlle
                       then wAlle
                       else string(num-entries(wvVareGrupper))  + " av " + string(num-entries(wVareGrupper)) 
    FI-Farg        = if wvFarger = wAlle
                       then wAlle
                       else string(num-entries(wvFarger))       + " av " + string(num-entries(wFarger)) 
    FI-Material    = if wvMaterial = wAlle
                       then wAlle
                       else string(num-entries(wvMaterial))     + " av " + string(num-entries(wMaterial)) 
    FI-Butiker     = if wvButiker = wAlle
                       then wAlle
                       else string(num-entries(wvButiker))      + " av " + string(num-entries(wButiker))
    FI-Varemerker  = if wvVmId = wAlle
                       then wAlle
                       else string(num-entries(wvVmId))         + " av " + string(num-entries(wVmId)).

  do with frame DEFAULT-FRAME:
    B-Rediger:sensitive = if T-Butikk 
                            then false
                            else true.
    B-Grid:sensitive = if T-Butikk 
                         then false
                         else true.
  end.

  display
    FI-Opprettet
    FI-Endret   
    FI-FraDato
    FI-TilDato
    FI-FraLevDato 
    FI-TilLevDato 
    FI-FraBestNr 
    FI-TilBestNr
    FI-FraKateg
    FI-TilKateg
    FI-Merknad 
    FI-ListeNr   
    FI-Navn
    FI-Eier
    T-Butikk     
    FI-Info
    T-Annonse
    FI-Produsent
    FI-BestStat
    wAlle when FI-BestStat = "" @ FI-BestStat
    FI-Sesong  
    wAlle when FI-Sesong = "" @ FI-Sesong
    FI-LevBas  
    wAlle when FI-LevBas = "" @ FI-LevBas
    FI-VareGr  
    wAlle when FI-VareGr = "" @ FI-VareGr
    FI-Farg    
    wAlle when FI-Farg = "" @ FI-Farg
    FI-Material
    wAlle when FI-Material = "" @ FI-Material
    FI-Butiker
    wAlle when FI-Butiker = "" @ FI-Butiker
    FI-Varemerker  
    wAlle when FI-Varemerker = "" @ FI-Varemerker
  with frame {&FRAME-NAME}.
    cmbBekreftet:SCREEN-VALUE = cmbBekreftet.
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

