&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
def input-output parameter wBatchNr like BatchLogg.BatchNr no-undo.
DEFINE INPUT  PARAMETER    rTransRowid         AS ROWID    NO-UNDO.

/* Local Variable Definitions ---                                       */
def var wOk             as   log                 no-undo.
def var wLoop           as   int                 no-undo.
def var wTTId           like TransLogg.TTId      no-undo.
def var wTBId           like TransLogg.TBId      no-undo.
def var wProfilNr       like PrisProfil.ProfilNr no-undo.
def var wTransLoggRecid as   recid               no-undo.
def var wModus          as   char                no-undo.
def var wAlle           as   char                no-undo.
def var wwTTId          as int initial ?         no-undo.
def var wOldTransRowid  as   rowid               no-undo.
def var wStorl          as   char                no-undo.
DEF VAR wCl             AS   INT                 NO-UNDO.  
DEF    VAR idags_moms        AS INT  NO-UNDO.
DEF    VAR cTekst            AS CHAR NO-UNDO.
DEF    VAR plDbFaktorNetto   AS DEC  NO-UNDO.
DEF    VAR plDbFaktorBrutto  AS DEC  NO-UNDO.
DEF    VAR plMva%            AS DEC  NO-UNDO.
DEF    VAR lBruttoPris       LIKE TransLogg.Pris NO-UNDO.
DEF VAR piTransNr     AS INT  NO-UNDO.


DEFINE VARIABLE cOKStorl     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisStorl    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCB-TTIdAlle AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCB-TTIdNy   AS CHARACTER  NO-UNDO.
def buffer bufTransLogg for TransLogg.

DEF BUFFER clButiker    FOR Butiker.
DEF BUFFER bArtBas      FOR ArtBas.
DEF BUFFER bTransLogg   FOR TransLogg.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TransLogg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TransLogg

/* Definitions for BROWSE BROWSE-TransLogg                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TransLogg TransLogg.Butik ~
TransLogg.TransNr TransLogg.Dato TransLogg.TTId TransLogg.TBId TransLogg.Vg ~
TransLogg.LopNr TransLogg.Storl TransLogg.TilStorl TransLogg.Antall ~
TransLogg.Pris (TransLogg.Pris - TransLogg.RabKr) @ lBruttoPris ~
TransLogg.RabKr TransLogg.Mva TransLogg.FeilKode TransLogg.ArtikkelNr ~
TransLogg.OvButik TransLogg.ForsNr TransLogg.BestNr TransLogg.Kode ~
TransLogg.BongId TransLogg.BongLinjeNr TransLogg.KassaNr TransLogg.KundNr ~
TransLogg.MedlemsNr TransLogg.KortType TransLogg.KortNr TransLogg.LevNr ~
TransLogg.Plukket TransLogg.Postert TransLogg.PostertDato TransLogg.RefNr ~
TransLogg.RefTekst 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TransLogg TransLogg.Butik 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-TransLogg TransLogg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-TransLogg TransLogg
&Scoped-define QUERY-STRING-BROWSE-TransLogg FOR EACH TransLogg ~
      WHERE TransLogg.BatchNr = FI-BatchNr and ~
(if T-VisAlle = true ~
  then true ~
  else TransLogg.Postert = false) and ~
(if wwTTId = ? ~
  then true ~
  else TransLogg.TTId = wwTTId) ~
  ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TransLogg OPEN QUERY BROWSE-TransLogg FOR EACH TransLogg ~
      WHERE TransLogg.BatchNr = FI-BatchNr and ~
(if T-VisAlle = true ~
  then true ~
  else TransLogg.Postert = false) and ~
(if wwTTId = ? ~
  then true ~
  else TransLogg.TTId = wwTTId) ~
  ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TransLogg TransLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TransLogg TransLogg


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TransLogg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Ny B-Godkjenn CB-TTId2 T-VisAlle ~
BROWSE-TransLogg BUTTON-NyTrans BUTTON-EndreTrans BUTTON-Motposter Btn_Help ~
BUTTON-Ok BUTTON-SokBatch FILL-IN-9 B-Kasse BUTTON-KopierTrans RECT-44 ~
RECT-45 RECT-46 RECT-54 
&Scoped-Define DISPLAYED-FIELDS TransLogg.Storl BatchLogg.Beskrivelse ~
TransLogg.ForsNr Forsalj.FoNamn TransLogg.ArtikkelNr TransLogg.Dato ~
TransLogg.Butik TransLogg.KassaNr TransLogg.Vg TransLogg.LopNr ~
TransLogg.TilStorl TransLogg.Antall TransLogg.Pris TransLogg.Mva ~
TransLogg.RabKr TransLogg.MedlemsNr TransLogg.KundNr Kunde.Navn ~
TransLogg.LevNr LevBas.levnamn 
&Scoped-define DISPLAYED-TABLES TransLogg BatchLogg Forsalj Kunde LevBas
&Scoped-define FIRST-DISPLAYED-TABLE TransLogg
&Scoped-define SECOND-DISPLAYED-TABLE BatchLogg
&Scoped-define THIRD-DISPLAYED-TABLE Forsalj
&Scoped-define FOURTH-DISPLAYED-TABLE Kunde
&Scoped-define FIFTH-DISPLAYED-TABLE LevBas
&Scoped-Define DISPLAYED-OBJECTS FI-Medlem FI-BatchNr FI-OppdStatus CB-TTId ~
CB-TBId FI-TilButik CB-TTId2 T-VisAlle FILL-IN-9 FILL-IN-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOKStorl C-Win 
FUNCTION getOKStorl RETURNS CHARACTER
  ( INPUT ipVg AS INTEGER,
    INPUT ipLopNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU MENU-BAR-C-Win MENUBAR
       MENU-ITEM m_Fil          LABEL "&Fil"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Godkjenn 
     LABEL "&Godkjenn batch" 
     SIZE 23 BY 1.05.

DEFINE BUTTON B-Kasse  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     LABEL "&Angre" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-AngreB 
     IMAGE-UP FILE "icon/e-undo":U NO-FOCUS FLAT-BUTTON
     LABEL "&Angre" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON BUTTON-EndreTrans 
     LABEL "&Endre" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-KopierTrans 
     LABEL "&Kopier" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Lagre 
     LABEL "Lagre" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-LagreNy 
     LABEL "LagreNy" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Motposter 
     LABEL "&Motposter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon\e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post".

DEFINE BUTTON BUTTON-NyTrans 
     LABEL "&Ny" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-SokArtikkelNr 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokBatch 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokForsalj 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON BUTTON-SokFraBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokKund 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON BUTTON-SokMedlem 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON BUTTON-SokTilBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-TBId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE CB-TTId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transaksjonstype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE CB-TTId2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transtype" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BatchNr AS INTEGER FORMAT "zzzzzzzz9":U INITIAL 0 
     LABEL "BatchNummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Medlem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OppdStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Oppdstatus" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilButik AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Til butikk" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL " Filter" 
      VIEW-AS TEXT 
     SIZE 10 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Vedlikehold av transaksjonslinjer" 
      VIEW-AS TEXT 
     SIZE 42.6 BY .62
     BGCOLOR 1 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 153.6 BY 1.1
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 153.6 BY .1.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 153.6 BY .1.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 2.62.

DEFINE VARIABLE T-VisAlle AS LOGICAL INITIAL yes 
     LABEL "&Vis alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TransLogg FOR 
      TransLogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TransLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TransLogg C-Win _STRUCTURED
  QUERY BROWSE-TransLogg NO-LOCK DISPLAY
      TransLogg.Butik FORMAT "zzzzz9":U
      TransLogg.TransNr FORMAT "zz,zzz,zz9":U
      TransLogg.Dato FORMAT "99/99/9999":U
      TransLogg.TTId COLUMN-LABEL "TTId" FORMAT "zz9":U WIDTH 7.2
      TransLogg.TBId COLUMN-LABEL "TBId" FORMAT "z9":U WIDTH 8.2
      TransLogg.Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U WIDTH 6.4
      TransLogg.LopNr COLUMN-LABEL "LøpNr" FORMAT "zzz9":U WIDTH 6
      TransLogg.Storl FORMAT "x(10)":U
      TransLogg.TilStorl FORMAT "x(10)":U
      TransLogg.Antall FORMAT "-zz,zzz,zz9":U
      TransLogg.Pris COLUMN-LABEL "Brutto pris" FORMAT "-zz,zzz,zz9.99":U
      (TransLogg.Pris - TransLogg.RabKr) @ lBruttoPris COLUMN-LABEL "Pris" FORMAT "-zz,zzz,zz9.99":U
            WIDTH 14.4
      TransLogg.RabKr FORMAT "->,>>>,>>9.99":U
      TransLogg.Mva FORMAT "->,>>>,>>9.99":U
      TransLogg.FeilKode COLUMN-LABEL "FKode" FORMAT "zzz9":U
      TransLogg.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      TransLogg.OvButik COLUMN-LABEL "OvButikk" FORMAT "zzzzz9":U
      TransLogg.ForsNr FORMAT ">>>>>9":U
      TransLogg.BestNr FORMAT ">>>>>>>9":U
      TransLogg.Kode FORMAT "X(20)":U
      TransLogg.BongId FORMAT "zz,zzz,zz9":U
      TransLogg.BongLinjeNr FORMAT "zzzzz9":U
      TransLogg.KassaNr FORMAT "zzz9":U
      TransLogg.KundNr FORMAT ">>>>>>>>>>>>9":U
      TransLogg.MedlemsNr FORMAT ">>>>>>>>>>>>9":U
      TransLogg.KortType FORMAT "9":U
      TransLogg.KortNr FORMAT "X(22)":U
      TransLogg.LevNr FORMAT "zzzzz9":U
      TransLogg.Plukket FORMAT "Ja/Nei":U
      TransLogg.Postert FORMAT "Ja/Nei":U
      TransLogg.PostertDato FORMAT "99/99/9999":U
      TransLogg.RefNr FORMAT "->,>>>,>>9":U
      TransLogg.RefTekst FORMAT "X(40)":U
  ENABLE
      TransLogg.Butik
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 152 BY 11.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TransLogg.Storl AT ROW 9.52 COL 52 COLON-ALIGNED
          LABEL "Str"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 9.6 BY 1
     BUTTON-AngreB AT ROW 1.24 COL 7 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.24 COL 2 NO-TAB-STOP 
     BUTTON-SokKund AT ROW 6.48 COL 131.2
     BUTTON-SokMedlem AT ROW 5.52 COL 131.2
     FI-Medlem AT ROW 5.52 COL 134 COLON-ALIGNED NO-LABEL
     FI-BatchNr AT ROW 2.91 COL 18 COLON-ALIGNED
     BatchLogg.Beskrivelse AT ROW 2.91 COL 37.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 58 BY 1
     FI-OppdStatus AT ROW 2.91 COL 109 COLON-ALIGNED
     CB-TTId AT ROW 5.38 COL 18 COLON-ALIGNED
     CB-TBId AT ROW 5.38 COL 63.4 COLON-ALIGNED
     TransLogg.ForsNr AT ROW 6.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     B-Godkjenn AT ROW 1.24 COL 12 NO-TAB-STOP 
     Forsalj.FoNamn AT ROW 6.48 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 61.6 BY 1
     TransLogg.ArtikkelNr AT ROW 7.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     TransLogg.Dato AT ROW 8.52 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22.2 BY 1
     TransLogg.Butik AT ROW 9.52 COL 18 COLON-ALIGNED
          LABEL "Butikk/KasseNr" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     TransLogg.KassaNr AT ROW 9.57 COL 32.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     FI-TilButik AT ROW 10.52 COL 18 COLON-ALIGNED
     TransLogg.Vg AT ROW 7.52 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     TransLogg.LopNr AT ROW 8.52 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     TransLogg.TilStorl AT ROW 10.52 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     TransLogg.Antall AT ROW 7.52 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     TransLogg.Pris AT ROW 8.52 COL 74 COLON-ALIGNED HELP
          "Pris pr. enhet"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     TransLogg.Mva AT ROW 9.52 COL 74 COLON-ALIGNED FORMAT "-z,zzz,zz9.99"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     TransLogg.RabKr AT ROW 10.52 COL 74 COLON-ALIGNED HELP
          "Rabatt pr. enhet" FORMAT "-z,zzz,zz9.99"
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     TransLogg.MedlemsNr AT ROW 5.52 COL 109 COLON-ALIGNED
          LABEL "MedlemsNr"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     TransLogg.KundNr AT ROW 6.52 COL 109 COLON-ALIGNED
          LABEL "KundeNr"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Kunde.Navn AT ROW 6.52 COL 134 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     TransLogg.LevNr AT ROW 7.52 COL 109 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     LevBas.levnamn AT ROW 7.52 COL 120 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30.6 BY 1
     CB-TTId2 AT ROW 9.52 COL 109 COLON-ALIGNED
     T-VisAlle AT ROW 10.67 COL 111
     BROWSE-TransLogg AT ROW 11.95 COL 2
     BUTTON-NyTrans AT ROW 23.86 COL 2
     BUTTON-EndreTrans AT ROW 23.86 COL 17.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154.4 BY 24.14.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     BUTTON-LagreNy AT ROW 23.86 COL 49
     BUTTON-Lagre AT ROW 23.86 COL 65
     BUTTON-Angre AT ROW 23.86 COL 81
     BUTTON-Motposter AT ROW 23.86 COL 97
     Btn_Help AT ROW 1.24 COL 143.2 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.24 COL 148.2 NO-TAB-STOP 
     BUTTON-SokBatch AT ROW 2.91 COL 34.4
     BUTTON-SokDato AT ROW 8.52 COL 42.6
     BUTTON-SokForsalj AT ROW 6.48 COL 31
     BUTTON-SokFraBut AT ROW 9.52 COL 30.2
     FILL-IN-9 AT ROW 4.29 COL 2 NO-LABEL
     FILL-IN-10 AT ROW 8.81 COL 98.6 COLON-ALIGNED NO-LABEL
     B-Kasse AT ROW 9.57 COL 42.6 NO-TAB-STOP 
     BUTTON-SokArtikkelNr AT ROW 7.52 COL 42.6
     BUTTON-SokTilBut AT ROW 10.57 COL 30.2
     BUTTON-KopierTrans AT ROW 23.86 COL 33
     RECT-44 AT ROW 4.1 COL 1
     RECT-45 AT ROW 1 COL 1
     RECT-46 AT ROW 2.33 COL 1
     RECT-54 AT ROW 9.1 COL 99
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154.4 BY 24.14.


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
         TITLE              = "Transaksjonsregistrering"
         HEIGHT             = 24.24
         WIDTH              = 154.6
         MAX-HEIGHT         = 24.76
         MAX-WIDTH          = 155.4
         VIRTUAL-HEIGHT     = 24.76
         VIRTUAL-WIDTH      = 155.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-TransLogg T-VisAlle DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN TransLogg.Antall IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.ArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN BatchLogg.Beskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-TransLogg:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 8.

/* SETTINGS FOR FILL-IN TransLogg.Butik IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR BUTTON BUTTON-Angre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-AngreB IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Lagre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-LagreNy IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokArtikkelNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokForsalj IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokFraBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokKund IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokMedlem IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokTilBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX CB-TBId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX CB-TTId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.Dato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BatchNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Medlem IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-OppdStatus IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TilButik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Forsalj.FoNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.ForsNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.KassaNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.KundNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.LopNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.MedlemsNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN TransLogg.Mva IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Kunde.Navn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.Pris IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN TransLogg.RabKr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR COMBO-BOX TransLogg.Storl IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN TransLogg.TilStorl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TransLogg.Vg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TransLogg
/* Query rebuild information for BROWSE BROWSE-TransLogg
     _TblList          = "skotex.TransLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "TransLogg.BatchNr = FI-BatchNr and
(if T-VisAlle = true
  then true
  else TransLogg.Postert = false) and
(if wwTTId = ?
  then true
  else TransLogg.TTId = wwTTId)
 
"
     _FldNameList[1]   > skotex.TransLogg.Butik
"TransLogg.Butik" ? "zzzzz9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = skotex.TransLogg.TransNr
     _FldNameList[3]   = skotex.TransLogg.Dato
     _FldNameList[4]   > skotex.TransLogg.TTId
"TransLogg.TTId" "TTId" ? "integer" ? ? ? ? ? ? no ? no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > skotex.TransLogg.TBId
"TransLogg.TBId" "TBId" ? "integer" ? ? ? ? ? ? no ? no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > skotex.TransLogg.Vg
"TransLogg.Vg" "Vg" ? "integer" ? ? ? ? ? ? no ? no no "6.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > skotex.TransLogg.LopNr
"TransLogg.LopNr" "LøpNr" ? "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = skotex.TransLogg.Storl
     _FldNameList[9]   = skotex.TransLogg.TilStorl
     _FldNameList[10]   = skotex.TransLogg.Antall
     _FldNameList[11]   > skotex.TransLogg.Pris
"TransLogg.Pris" "Brutto pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"(TransLogg.Pris - TransLogg.RabKr) @ lBruttoPris" "Pris" "-zz,zzz,zz9.99" ? ? ? ? ? ? ? no ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = skotex.TransLogg.RabKr
     _FldNameList[14]   = skotex.TransLogg.Mva
     _FldNameList[15]   > skotex.TransLogg.FeilKode
"TransLogg.FeilKode" "FKode" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   = skotex.TransLogg.ArtikkelNr
     _FldNameList[17]   > skotex.TransLogg.OvButik
"TransLogg.OvButik" "OvButikk" "zzzzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   = skotex.TransLogg.ForsNr
     _FldNameList[19]   = skotex.TransLogg.BestNr
     _FldNameList[20]   = skotex.TransLogg.Kode
     _FldNameList[21]   = skotex.TransLogg.BongId
     _FldNameList[22]   = skotex.TransLogg.BongLinjeNr
     _FldNameList[23]   = skotex.TransLogg.KassaNr
     _FldNameList[24]   = skotex.TransLogg.KundNr
     _FldNameList[25]   = skotex.TransLogg.MedlemsNr
     _FldNameList[26]   = skotex.TransLogg.KortType
     _FldNameList[27]   = skotex.TransLogg.KortNr
     _FldNameList[28]   = skotex.TransLogg.LevNr
     _FldNameList[29]   = skotex.TransLogg.Plukket
     _FldNameList[30]   = skotex.TransLogg.Postert
     _FldNameList[31]   = skotex.TransLogg.PostertDato
     _FldNameList[32]   = skotex.TransLogg.RefNr
     _FldNameList[33]   = skotex.TransLogg.RefTekst
     _Query            is OPENED
*/  /* BROWSE BROWSE-TransLogg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Transaksjonsregistrering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Transaksjonsregistrering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Godkjenn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Godkjenn C-Win
ON CHOOSE OF B-Godkjenn IN FRAME DEFAULT-FRAME /* Godkjenn batch */
DO:
  if not available BatchLogg then
    return no-apply.
  wBatchNr = BatchLogg.BatchNr.

  /*if BatchLogg.OppdStatus < 2 then*/
  run batchstatus.p (BatchLogg.BatchNr, 2).
  find BatchLogg no-lock where
    BatchLogg.BatchNr = wBatchNr no-error.
  RUN VisLinje.
  RUN OppdaterDagsrapport (wBatchNr).
  APPLY "choose" TO BUTTON-Ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kasse C-Win
ON CHOOSE OF B-Kasse IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF TransLogg.KassaNr
DO:
    DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
    /* Kaller søkerutine */
    IF NOT CAN-FIND(Butiker  WHERE Butiker.Butik = INPUT TransLogg.Butik) THEN DO:
        MESSAGE "Ukjent butikk"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO TransLogg.Butik.
        RETURN NO-APPLY "AVBRYT".
    END.
    RUN gkasse.w (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "ButikkNr,GruppeNr", /* Feltliste avgrensningsfelt (kommaseparert) */
      TransLogg.Butik:SCREEN-VALUE + chr(1) + "1", /* Feltverdier (chr(1) sep) */ 
      "=,=",
      TransLogg.KassaNr:SCREEN-VALUE /* Post markøren skal stå på */
      ) NO-ERROR.
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
        ASSIGN
          TransLogg.KassaNr:SCREEN-VALUE     = ENTRY(2,cTekst,CHR(1))
          .
/*         APPLY "VALUE-CHANGED":U TO RowObject.KassaNr.      */
/*                                                            */
/*         /* Flagger at det er gjort endringer på posten. */ */
/*         dynamic-function('assignLinkProperty',             */
/*                          'GroupAssign-Target':U,           */
/*                          'DataModified':U, 'yes':U).       */
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TransLogg
&Scoped-define SELF-NAME BROWSE-TransLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON ROW-DISPLAY OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  if TransLogg.SeqNr = 2 then
    TransLogg.TransNr:bgcolor in browse BROWSE-TransLogg = 11.
  if TransLogg.SeqNr = 1 and 
     can-find(bufTransLogg where
              bufTransLogg.Butik   = TransLogg.Butik   and
              bufTransLogg.TransNr = TransLogg.TransNr and
              bufTransLogg.SeqNr   = 2) then
    TransLogg.TransNr:bgcolor in browse BROWSE-TransLogg = 8.
  if TransLogg.Postert then
    TransLogg.TransNr:bgcolor in browse BROWSE-TransLogg = 15.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON VALUE-CHANGED OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  run VisLinje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TransLogg.Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TransLogg.Butik C-Win
ON TAB OF TransLogg.Butik IN FRAME DEFAULT-FRAME /* Butikk/KasseNr */
or "RETURN" of TransLogg.Butik
DO:
  def var wLoop as int no-undo.
  
  find Butiker no-lock where
     Butiker.Butik = int(TransLogg.Butik:screen-value) no-error.
  if available Butiker then
    do:
      display 
        Butiker.Butik @ TransLogg.Butik
      with frame DEFAULT-FRAME. 
      find PrisProfil no-lock where
        PrisProfil.ProfilNr = Butiker.ProfilNr no-error.
      if available PrisProfil then
        wProfilNr = PrisProfil.ProfilNr.
    end.
  else do:
    message "Ukjent butikk!"
      view-as alert-box MESSAGE title "Melding".
    return no-apply.
  end.

  find ArtBas no-lock where
     ArtBas.ArtikkelNr = input TransLogg.ArtikkelNr no-error.
  if available ArtBas then
    do:
      find ArtPris no-lock where
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
        ArtPris.ProfilNr   = wProfilNr no-error.
      if available ArtPris then
        assign wLoop = if ArtPris.Tilbud then 2 else 1.
      display 
        ArtBas.Vg            @ TransLogg.Vg 
        ArtBas.LopNr         @ TransLogg.LopNr
        ArtBas.ArtikkelNr    @ TransLogg.ArtikkelNr
        ArtPris.Pris[wLoop]  @ TransLogg.Pris
        ArtPris.MvaKr[wLoop] @ TransLogg.Mva
        1                    @ Translogg.Antall
      with frame DEFAULT-FRAME. 
    end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-Win
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Angre */
DO:
  IF wModus = "NY" THEN
     ASSIGN CB-TTId:LIST-ITEMS = cCB-TTIdAlle
            CB-TTId:SCREEN-VALUE = ENTRY(1,CB-TTId).
  run EnableFelt (9).
  run VisLinje.
  assign
    B-Godkjenn:sensitive = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AngreB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AngreB C-Win
ON CHOOSE OF BUTTON-AngreB IN FRAME DEFAULT-FRAME /* Angre */
DO:
  run EnableFelt (9).
  run VisLinje.
  assign
    B-Godkjenn:sensitive = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EndreTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EndreTrans C-Win
ON CHOOSE OF BUTTON-EndreTrans IN FRAME DEFAULT-FRAME /* Endre */
DO:
  if not available TransLogg then
    return no-apply.
    
  if TransLogg.Postert then
    do:
      message "Oppdatert! Kan ikke endres."
              view-as alert-box message title "Melding".
      return no-apply.              
    end.

  if BROWSE-TransLogg:num-selected-rows > 1 then
    do:
      message "Det er valgt mer enn en transaksjon." skip
              "Kun en post kan endres ad gangen."
              view-as alert-box message title "Melding".
      return no-apply.              
    end.

  IF NOT CAN-DO("1,2,3,4,6,10,11",STRING(int(substring(CB-TTId:screen-value in frame DEFAULT-FRAME,1,3)))) THEN
  DO:
      MESSAGE "Transaksjonstype " CB-TTId:screen-value in frame DEFAULT-FRAME " kan ikke registreres manuelt." SKIP
              "Kun transaksjonstypene 1, 2, 3, 4, 6, 10 og 11 kan benyttes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  assign
    wTransLoggRecid = recid(TransLogg)
    wModus = "ENDRE".
  ASSIGN TransLogg.Storl:LIST-ITEMS = cOKStorl.  
  run EnableFelt (3).
  apply "Entry":U to TransLogg.ForsNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-KopierTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-KopierTrans C-Win
ON CHOOSE OF BUTTON-KopierTrans IN FRAME DEFAULT-FRAME /* Kopier */
DO:
  if not available TransLogg then
    return no-apply.
    
  if BROWSE-TransLogg:num-selected-rows > 1 then
    do:
      message "Det er valgt mer enn en transaksjon." skip
              "Kun en post kan endres ad gangen."
              view-as alert-box message title "Melding".
      return no-apply.              
    end.

  IF NOT CAN-DO("1,2,3,4,6,10,11",STRING(int(substring(CB-TTId:screen-value in frame DEFAULT-FRAME,1,3)))) THEN
  DO:
      MESSAGE "Transaksjonstype " CB-TTId:screen-value in frame DEFAULT-FRAME " kan ikke registreres manuelt." SKIP
              "Kun transaksjonstypene 1, 2, 3, 4, 6, 10 og 11 kan benyttes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  /* Koperer translogg */
  DO TRANSACTION:
      /* Setter transaksjonsnummer  */
      piTransNr = 0.
      DO:
        FIND LAST bTransLogg WHERE
          bTransLogg.Butik = TransLogg.Butik USE-INDEX TransLogg NO-ERROR.
        IF AVAILABLE bTransLogg THEN
          piTransNr = bTransLogg.TransNr + 1.
        ELSE
          piTransNr = 1.
      END.
     CREATE bTransLogg.
     BUFFER-COPY TransLogg 
         EXCEPT TransNr SeqNr
         TO bTransLogg
         ASSIGN
           bTransLogg.TransNr       = piTransNr
           bTransLogg.SeqNr         = 1
           bTransLogg.Postert       = FALSE
           bTransLogg.PostertDato   = ?
           bTransLogg.PostertTid    = 0
           bTransLogg.SattVVareKost = FALSE
           bTransLogg.Plukket       = FALSE
           bTransLogg.TilStorl      = ''
           .
     FIND CURRENT bTransLogg NO-LOCK NO-ERROR.
     FIND TransLogg NO-LOCK WHERE RECID(TransLogg) = RECID(bTransLogg).
     RELEASE bTranslogg.
  END. /* TRANSACTION */

  assign
    wTransLoggRecid = recid(TransLogg)
    wModus = "ENDRE".
  ASSIGN TransLogg.Storl:LIST-ITEMS = cOKStorl.  
  run EnableFelt (3).
  apply "Entry":U to TransLogg.ForsNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-Win
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagre */
DO:
  run LagreTrans.
  if return-value <> "OK" then
    return no-apply.
  assign
    wModus = "VIS"
    B-Godkjenn:sensitive = true.

  {&OPEN-QUERY-BROWSE-TransLogg}
  find TransLogg no-lock where
    recid(TransLogg) = wTransLoggRecid no-error.
  REPOSITION BROWSE-TransLogg TO ROWID rowid(TransLogg) NO-ERROR.
  
  ASSIGN CB-TTId:LIST-ITEMS   = cCB-TTIdAlle
         CB-TTId:SCREEN-VALUE = ENTRY(1,CB-TTId).

  run BlankLinje.  
  run EnableFelt (9).
  RUN VisLinje.
  apply "ENTRY":U to BROWSE-TransLogg in frame DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-LagreNy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-LagreNy C-Win
ON CHOOSE OF BUTTON-LagreNy IN FRAME DEFAULT-FRAME /* LagreNy */
DO:
  
  run LagreTrans.
  if return-value <> "OK" then
    return no-apply.

  {&OPEN-QUERY-BROWSE-TransLogg}
  find TransLogg no-lock where
    recid(TransLogg) = wTransLoggRecid no-error.
  REPOSITION BROWSE-TransLogg TO ROWID rowid(TransLogg) NO-ERROR.

  /* run BlankLinje. */
  RUN BlankLinje2.
  RUN SokArtBas.
  IF RETURN-VALUE = "AVBRYT" THEN DO:
      APPLY "CHOOSE" TO BUTTON-Angre.
      RETURN NO-APPLY.
  END.
  display today @ TransLogg.Dato with frame DEFAULT-FRAME.
  apply "Entry":U to TransLogg.Storl.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Motposter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Motposter C-Win
ON CHOOSE OF BUTTON-Motposter IN FRAME DEFAULT-FRAME /* Motposter */
DO:
  if not available TransLogg then
    return no-apply.    

  if BROWSE-TransLogg:num-selected-rows = 0 then
    do:
      message "Ingen transaksjoner valgt."
        view-as alert-box MESSAGE title "Melding".
      return no-apply.
    end.
  
  /* Kontroller som utføres hvis det kun er en trans valgt. */
  if BROWSE-TransLogg:num-selected-rows > 1 then
    do: 
      if TransLogg.SeqNr > 1 then    
        do:
          message "Denne transaksjonen er en motpostering. Kan ikke motposteres!"
             view-as alert-box MESSAGE title "Melding".
          return no-apply.
        end.
    
      if TransLogg.SeqNr = 1 and 
         can-find(bufTransLogg where
                  bufTransLogg.Butik   = TransLogg.Butik   and
                  bufTransLogg.TransNr = TransLogg.TransNr and
                  bufTransLogg.SeqNr   = 2) then
        do:
          message "Denne transaksjonen er allerede motpostert!"
            view-as alert-box MESSAGE title "Melding".
          return no-apply.
        end.
    end.

  /* Husker hvor vi var. */
  assign
    wOldTransRowid = ?.
  BROWSE BROWSE-TransLogg:SET-REPOSITIONED-ROW(BROWSE-TransLogg:FOCUSED-ROW - 
             IF BROWSE-TransLogg:FOCUSED-ROW = BROWSE-TransLogg:DOWN THEN 1 ELSE 0).
    
  /* Behandler alle valgte transer. */
  TRANSLOOP:
  do wLoop = 1 to BROWSE-TransLogg:NUM-SELECTED-ROWS:
     wOk = BROWSE-TransLogg:FETCH-SELECTED-ROW(wLoop).
    
    if wOldTransRowId = ? then
      wOldTransRowId = rowid(TransLogg).    
    
    if TransLogg.SeqNr > 1 then    
      next TRANSLOOP.
    if TransLogg.SeqNr = 1 and 
       can-find(bufTransLogg where
                bufTransLogg.Butik   = TransLogg.Butik   and
                bufTransLogg.TransNr = TransLogg.TransNr and
                bufTransLogg.SeqNr   = 2) then
      next TRANSLOOP.

    run MotPosterTrans.
  end. /* TRANSLOOP */
  
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  find TransLogg no-lock where
    rowid(TransLogg) = wOldTransRowid no-error.
  REPOSITION BROWSE-TransLogg TO ROWID rowid(TransLogg) NO-ERROR.  
  apply "entry":U to BROWSE-TransLogg.


  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  def var wTittel as char no-undo.
  
  assign
    wTittel = "Manuell registrering " + 
               string(today) + 
               " " + 
               string(time,"HH:MM") + 
               " " +
               userid("dictdb").
  run batchlogg.p (program-name(1), 
                   wTittel,
                   output FI-BatchNr).  

  assign
    B-Godkjenn:sensitive = false.
  display 
    FI-BatchNr
    wTittel @ BatchLogg.Beskrivelse                   
  with frame DEFAULT-FRAME.
  
  FIND BatchLogg NO-LOCK WHERE
      BatchLogg.BatchNr = FI-BatchNr NO-ERROR.

  run BlankLinje.
  {&OPEN-QUERY-BROWSE-TransLogg}                   
   
  apply "CHOOSE":U to BUTTON-NyTrans in frame DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyTrans C-Win
ON CHOOSE OF BUTTON-NyTrans IN FRAME DEFAULT-FRAME /* Ny */
DO:
  assign
    wModus = "NY".
  run BlankLinje.
  RUN SokArtBas.
  IF RETURN-VALUE = "AVBRYT" THEN DO:
      APPLY "CHOOSE" TO BUTTON-Angre.
      RETURN NO-APPLY.
  END.
  run EnableFelt (1).

  display today @ TransLogg.Dato with frame DEFAULT-FRAME.
  ASSIGN CB-TTId:LIST-ITEMS = cCB-TTIdNy
         CB-TTId:SCREEN-VALUE = ENTRY(1,CB-TTId:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO CB-TTId.
  apply "Entry":U to CB-TTId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokArtikkelNr C-Win
ON CHOOSE OF BUTTON-SokArtikkelNr IN FRAME DEFAULT-FRAME /* ... */
or F10 of TransLogg.ArtikkelNr
DO:
    RUN SokArtBas.
    IF RETURN-VALUE <> "AVBRYT" THEN DO:
        run EnableFelt (2).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBatch C-Win
ON CHOOSE OF BUTTON-SokBatch IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-BatchNr
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-BatchNr
    &Program     = d-bbatchlogg.w
    &Frame       = DEFAULT-FRAME
    &PostRun     = "find BatchLogg no-lock where
                      recid(BatchLogg) = int(return-value).
                    display BatchLogg.Beskrivelse." 
  }   
  assign
    FI-BatchNr
    T-VisAlle         = TRUE
    T-VisAlle:CHECKED = TRUE.
  {&OPEN-QUERY-BROWSE-TransLogg}
    RUN VisLinje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of TransLogg.LopNr
DO:
  def var wTittel as char no-undo.
  
  wTittel = "Transaksjonsdato".
  
  /* Start søkeprogram */
  {soek.i
    &Felt        = TransLogg.Dato
    &Program     = kalender.w
    &Frame       = DEFAULT-FRAME
    &ExtraParam  = "input wTittel"
  }   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokForsalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokForsalj C-Win
ON CHOOSE OF BUTTON-SokForsalj IN FRAME DEFAULT-FRAME /* ... */
or F10 of TransLogg.ForsNr
DO:
  {soek.i
    &Felt       = TransLogg.ForsNr
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-bforsalj.w 
    &PostRun    = "find Forsalj no-lock where recid(Forsalj) = int(return-value) no-error."                   
    &OptDisp    = "Forsalj.ForsNr    when available Forsalj @ TransLogg.ForsNr 
                   Forsalj.FoNamn"
  }
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFraBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFraBut C-Win
ON CHOOSE OF BUTTON-SokFraBut IN FRAME DEFAULT-FRAME /* ... */
or F10 of TransLogg.Butik
DO:
  
  /* Start søkeprogram */
  {soek.i
    &Felt        = Translogg.Butik
    &Program     = d-bbutiker.w
    &PostRun     = "find butiker no-lock where 
                      recid(Butiker) = int(return-value) no-error."
    &Frame       = DEFAULT-FRAME
  } 

  Translogg.Butik:SCREEN-VALUE = STRING(Butiker.Butik).

  if available Butiker then
    do:
      find PrisProfil no-lock where
        PrisProfil.ProfilNr = Butiker.ProfilNr no-error.
      if available PrisProfil then
        wProfilNr = PrisProfil.ProfilNr.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokKund
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokKund C-Win
ON CHOOSE OF BUTTON-SokKund IN FRAME DEFAULT-FRAME /* ... */
or F10 of TransLogg.KundNr
DO:
  {soek.i
    &Felt       = TransLogg.KundNr
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-bkunde.w 
    &ParamType  = "input"
    &ExtraParam = "' '"
    &PostRun    = "find Kunde no-lock where recid(Kunde) = int(return-value) no-error."                   
    &OptDisp    = "Kunde.KundeNr when available Kunde @ TransLogg.KundNr 
                   Kunde.Navn"
  }
  return no-apply.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMedlem C-Win
ON CHOOSE OF BUTTON-SokMedlem IN FRAME DEFAULT-FRAME /* ... */
or F10 of TransLogg.MedlemsNr
DO:
  {soek.i
    &Felt       = TransLogg.MedlemsNr
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-bmedlem.w 
    &ParamType  = "input"
    &ExtraParam = "' '"
    &PostRun    = "find Medlem no-lock where recid(Medlem) = int(return-value) no-error.
                  IF Medlem.KundeNr <> 0 THEN
                  DO:
                      FIND Kunde NO-LOCK WHERE
                          Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
                      IF AVAILABLE Kunde THEN
                          DISPLAY
                            Kunde.KundeNr @ TransLogg.KundNr
                            Kunde.Navn
                          WITH FRAME Default-frame.
                      ELSE 
                        DISPLAY
                          '' @ TransLogg.KundNr
                          '' @ Kunde.Navn
                        WITH FRAME Default-frame.
                  END."                   
    &OptDisp    = "Medlem.MedlemsNr when available Medlem @ TransLogg.MedlemsNr 
                   Medlem.ForNavn + ' ' + Medlem.EtterNavn @ FI-Medlem"
  }
  return no-apply.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokTilBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokTilBut C-Win
ON CHOOSE OF BUTTON-SokTilBut IN FRAME DEFAULT-FRAME /* ... */
or F10 of TransLogg.Butik
DO:
  
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilButik
    &Program     = d-bbutiker.w
    &PostRun     = "find butiker no-lock where 
                      recid(Butiker) = int(return-value) no-error."
    &Frame       = DEFAULT-FRAME
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TBId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TBId C-Win
ON TAB OF CB-TBId IN FRAME DEFAULT-FRAME /* Beskrivelse */
or "RETURN" of CB-TBId
DO:
  apply "ENTRY":U to Translogg.ForsNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId C-Win
ON TAB OF CB-TTId IN FRAME DEFAULT-FRAME /* Transaksjonstype */
or "RETURN":U of CB-TTID
DO:
  apply "ENTRY":U to CB-TBId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId C-Win
ON VALUE-CHANGED OF CB-TTId IN FRAME DEFAULT-FRAME /* Transaksjonstype */
DO:
  /* Kun disse transaksjonstypene kan registreres manuelt. */
  IF NOT CAN-DO("1,2,3,4,10,11",STRING(int(substring(CB-TTId:screen-value in frame DEFAULT-FRAME,1,3)))) THEN
  DO:
      MESSAGE "Transaksjonstype " CB-TTId:screen-value in frame DEFAULT-FRAME " kan ikke registreres manuelt." SKIP
              "Kun transaksjonstypene 1, 2, 3, 4, 10 og 11 kan benyttes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  assign
    wTTId = int(substring(CB-TTId:screen-value in frame DEFAULT-FRAME,1,3)).
  run InitTransBeskr.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TTId2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId2 C-Win
ON VALUE-CHANGED OF CB-TTId2 IN FRAME DEFAULT-FRAME /* Transtype */
DO:
  assign frame DEFAULT-FRAME
    CB-TTId2.
  if CB-TTId2 = wAlle then
    wwTTId = ?.
  else
    wwTTId = int(entry(1,CB-TTId2,":")).
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  apply "entry":U to BROWSE-TransLogg.
  
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilButik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilButik C-Win
ON TAB OF FI-TilButik IN FRAME DEFAULT-FRAME /* Til butikk */
OR "Return" OF FI-TilButik DO:

    find Butiker no-lock where
       Butiker.Butik = int(FI-TilButik:screen-value) no-error.
    if NOT AVAILABLE Butiker then
      do:
        message "Ukjent butikk!"
          view-as alert-box MESSAGE title "Melding".
        return no-apply.
      end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TransLogg.ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TransLogg.ForsNr C-Win
ON TAB OF TransLogg.ForsNr IN FRAME DEFAULT-FRAME /* Kasserernummer */
or "RETURN" of TransLogg.ForsNr
DO:
  find Forsalj no-lock where
     DEC(Forsalj.ForsNr) = input TransLogg.ForsNr no-error.
  if available Forsalj then
    do:
      display 
        forsalj.ForsNr @ TransLogg.ForsNr
        Forsalj.FoNamn 
      with frame DEFAULT-FRAME. 
    end.
  else do:
      display 
        "" @ TransLogg.ForsNr
        "" @ Forsalj.FoNamn 
      with frame DEFAULT-FRAME. 
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TransLogg.KundNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TransLogg.KundNr C-Win
ON TAB OF TransLogg.KundNr IN FRAME DEFAULT-FRAME /* KundeNr */
or "RETURN" of TransLogg.KundNr
DO:
  find Kunde no-lock where
     DEC(Kunde.KundeNr) = input TransLogg.KundNr no-error.
  if available Kunde then
    do:
      display 
        Kunde.KundeNr @ TransLogg.KundNr
        Kunde.Navn 
      with frame DEFAULT-FRAME. 
      IF INPUT TransLogg.MedlemsNr <> 0 THEN
      DO:
          IF NOT CAN-FIND(FIRST Medlem WHERE
                          Medlem.KundeNr   = Kunde.KundeNr AND
                          Medlem.MedlemsNr = INPUT TransLogg.MedlemsNr) THEN
            DISPLAY
              "" @ TransLogg.MedlemsNr
              "" @ FI-Medlem 
            with frame DEFAULT-FRAME. 
      END.
    end.
  
  else do:
      display 
        "" @ TransLogg.KundNr
        "" @ Kunde.Navn 
      with frame DEFAULT-FRAME. 
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TransLogg.MedlemsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TransLogg.MedlemsNr C-Win
ON TAB OF TransLogg.MedlemsNr IN FRAME DEFAULT-FRAME /* MedlemsNr */
or "RETURN" of TransLogg.MedlemsNr
DO:
  find Medlem no-lock where
     Medlem.MedlemsNr = input TransLogg.MedlemsNr no-error.
  if available Medlem then
    do:
      display 
        Medlem.MedlemsNr @ TransLogg.MedlemsNr
        Medlem.ForNavn + " " + MEdlem.EtterNavn @ FI-Medlem 
      with frame DEFAULT-FRAME.
      IF Medlem.KundeNr <> 0 THEN
      DO:
          FIND Kunde NO-LOCK WHERE
              Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
          IF AVAILABLE Kunde THEN
              DISPLAY
                Kunde.KundeNr @ TransLogg.KundNr
                Kunde.Navn
              WITH FRAME Default-frame.
          ELSE 
            DISPLAY
              "" @ TransLogg.KundNr
              "" @ Kunde.Navn
            WITH FRAME Default-frame.
      END.
      ELSE 
        DISPLAY
          "" @ TransLogg.KundNr
          "" @ Kunde.Navn
        WITH FRAME Default-frame.
    end.
  
  else do:
      display 
        "" @ TransLogg.MedlemsNr
        "" @ FI-Medlem 
      with frame DEFAULT-FRAME. 
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TransLogg.Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TransLogg.Pris C-Win
ON LEAVE OF TransLogg.Pris IN FRAME DEFAULT-FRAME /* Pris */
DO:
  def var wMvaKr as dec no-undo.
  def var wMva%  as dec no-undo.
  def var wBelop as dec no-undo.
  DEF VAR wRabKr AS DEC NO-UNDO.
  
  if not available ArtBas then 
    do:
      message "Ingen artikkel tilgjengelig!"
        view-as alert-box MESSAGE title "Melding".
      return no-apply.
    end.
  find VarGr no-lock of ArtBas no-error.
  if available VarGr then
    find Moms no-lock of VarGr no-error.
  if available Moms then
    wMva% = Moms.MomsProc.
  
  assign
    wBelop = dec(TransLogg.Pris:screen-value)
    wRabKr = DEC(TransLogg.RabKr:SCREEN-VALUE).
  
  if valid-handle(wLibHandle) then
    run MvaKalk in wLibHandle (input wMva%, input (wBelop - wRabKr), output wMvaKr).
  display 
    wMvaKr @ TransLogg.Mva
  with frame DEFAULT-FRAME.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TransLogg.RabKr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TransLogg.RabKr C-Win
ON LEAVE OF TransLogg.RabKr IN FRAME DEFAULT-FRAME /* Rabatt */
DO:
    def var wMvaKr as dec no-undo.
    def var wMva%  as dec no-undo.
    def var wBelop as dec no-undo.
    DEF VAR wRabKr AS DEC NO-UNDO.

    if not available ArtBas then 
      do:
        message "Ingen artikkel tilgjengelig!"
          view-as alert-box MESSAGE title "Melding".
        return no-apply.
      end.
    
    find VarGr no-lock of ArtBas no-error.
    if available VarGr then
      find Moms no-lock of VarGr no-error.
    if available Moms then
      wMva% = Moms.MomsProc.

    assign
      wBelop = dec(TransLogg.Pris:screen-value)
      wRabKr = DEC(TransLogg.RabKr:SCREEN-VALUE).

    if valid-handle(wLibHandle) then
      run MvaKalk in wLibHandle (input wMva%, input (wBelop - wRabKr), output wMvaKr).
    display 
      wMvaKr @ TransLogg.Mva
    with frame DEFAULT-FRAME. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-VisAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-VisAlle C-Win
ON VALUE-CHANGED OF T-VisAlle IN FRAME DEFAULT-FRAME /* Vis alle */
DO:
  assign frame DEFAULT-FRAME
    T-VisAlle.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  RUN VisLinje.
  apply "entry":U to BROWSE-TransLogg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Transaksjonsregistrering"
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Avgjør om dagsrapporten skal posteres inklusive eller eksklusive mva */
idags_moms = 0.
{syspara.i 6 4 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    idags_moms = 1. /* Poster inkl. Mva */
ELSE
    iDags_Moms = 0. /* Poster eks.  Mva  */

/* Faktor for beregning av DB på grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes på artikkelen. */
plDbFaktorNetto = 0.
{syspara.i 6 4 4 plDbFaktorNetto DEC}
IF plDbFaktorNetto = 0 THEN
    plDbFaktorNetto = 0.30.

/* Faktor for beregning av DB på grunnlag av oms inkl. mva, hvis ikke kalkyle finnes på artikkelen. */
plDbFaktorBrutto = 0.
{syspara.i 6 4 3 plDbFaktorBrutto DEC}
IF plDbFaktorBrutto = 0 THEN
    plDbFaktorBrutto = 0.24.
/* Faktor for beregning av DB på grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes på artikkelen. */
plMva% = 0.
{syspara.i 6 4 2 plMva% DEC}
IF plMva% = 0 THEN
    plMva% = 0.30.


/* Henter prisprofil fra sentrallager. */
{syspara.i 5 1 1 wProfilNr INT}
find Butiker no-lock where
  Butiker.Butik = wPRofilNr no-error.
if available Butiker then
  wProfilNr = Butiker.ProfilNr.

{syspara.i 1 100 1 wAlle}

/* Henter sentrallager. */
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = wCl.
/* ingen ALT-S p g a dialogen körs bara vid NY-knapp */
/* on F2,ALT-S of FRAME {&FRAME-NAME} anywhere do: */
/*     if CAN-DO("NY,ENDRE",wModus) THEN DO:       */
/*         APPLY "CHOOSE" TO BUTTON-SokArtikkelNr. */
/*         return no-apply.                        */
/*     END.                                        */
/* END.                                            */
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.
  {lng.i}

  find first TransType no-lock.
  if not available TransType then
    do:
      message "Det finnes ingen tansaksjonstyper!" view-as alert-box ERROR.
      return no-apply.
    end.
  find first TransBeskr of TransType no-lock.
  if not available TransBeskr then
    do:
      message "Det finnes ingen tansaksjonsbeskrivelser for transtypen!" view-as alert-box ERROR.
      return no-apply.
    end.
  assign
    wTTId  = TransType.TTId
    wTBId  = TransBeskr.TBId
    wwTTId = ?.
  run InitTransType (1).
  run InitTransBeskr.
  display CB-TTId2 with frame DEFAULT-FRAME.
  
  find BatchLogg no-lock where
    BatchLogg.BatchNr = wBatchNr no-error.
  if not available BatchLogg then
    find last BatchLogg no-lock where 
       BatchLogg.OppdStatus = 3 no-error .
  if not available BatchLogg then
    find last BatchLogg no-error.
  if available BatchLogg then
    do:
/*       assign               */
/*         t-VisAlle = FALSE. */
        FI-BatchNr = BatchLogg.BatchNr.
      display FI-BatchNr BatchLogg.Beskrivelse with frame DEFAULT-FRAME.
      {&OPEN-QUERY-BROWSE-TransLogg}
      IF rTransRowid <> ? THEN DO:
        BROWSE {&BROWSE-NAME}:SET-REPOSITIONED-ROW(5,"CONDITIONAL").
        REPOSITION {&BROWSE-NAME} TO ROWID rTransRowid.
      END.
      IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN DO:
          BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
          APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
      END.
    end.
  assign
    TransLogg.Butik:read-only in browse BROWSE-TransLogg = true.
  apply "ENTRY":U to BROWSE-TransLogg.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankLinje C-Win 
PROCEDURE BlankLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  display 
    "" @ TransLogg.Butik 
    "" @ TransLogg.Dato 
    "" @ TransLogg.ArtikkelNr 
    "" @ TransLogg.Vg 
    "" @ TransLogg.LopNr 
/*     "" @ TransLogg.Storl */
    "" @ TransLogg.Antall 
    "" @ TransLogg.Pris 
    "" @ TransLogg.RabKr 
    "" @ TransLogg.Mva 
    "" @ TransLogg.ForsNr 
    "" @ TransLogg.KassaNr 
    "" @ TransLogg.KundNr 
    "" @ TransLogg.LevNr
    "" @ LevBas.LevNamn
    "" @ Kunde.Navn
    "" @ Forsalj.FoNamn
    "" @ TransLogg.MedlemsNr
    "" @ FI-Medlem
  with frame DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankLinje2 C-Win 
PROCEDURE BlankLinje2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  display 
    /*
    "" @ TransLogg.Butik 
    "" @ TransLogg.Dato 
    "" @ TransLogg.ForsNr 
    "" @ TransLogg.KassaNr 
    "" @ Forsalj.FoNamn
    */    
    "" @ TransLogg.ArtikkelNr 
    "" @ TransLogg.Vg 
    "" @ TransLogg.LopNr 
/*     "" @ TransLogg.Storl */
    "" @ TransLogg.Antall 
    "" @ TransLogg.Pris 
    "" @ TransLogg.RabKr 
    "" @ TransLogg.Mva 
    "" @ TransLogg.KundNr 
    "" @ TransLogg.LevNr
    "" @ LevBas.LevNamn
    "" @ Kunde.Navn
    "" @ TransLogg.MedlemsNr
    "" @ FI-Medlem
  with frame DEFAULT-FRAME.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayBrowsRow C-Win 
PROCEDURE DisplayBrowsRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if available TransLogg then
  display
    TransLogg.Butik     
    TransLogg.TransNr   
    TransLogg.KundNr    
    TransLogg.TTId       
    TransLogg.TBId       
    TransLogg.ArtikkelNr 
    TransLogg.LevNr      
    TransLogg.BongId     
    TransLogg.BongLinjeNr
    TransLogg.KassaNr   
    TransLogg.Vg        
    TransLogg.LopNr     
    TransLogg.Storl     
    TransLogg.Antall    
    TransLogg.Pris      
    TransLogg.RabKr     
    TransLogg.Mva       
    TransLogg.Plukket   
    TransLogg.Dato      
    TransLogg.BestNr    
    TransLogg.Postert   
  with browse BROWSE-TransLogg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableFelt C-Win 
PROCEDURE EnableFelt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter ipModus as int no-undo.

do with frame DEFAULT-FRAME: 
  case ipModus:
    when 1 then /* NY */
      assign
        BUTTON-AngreB:sensitive = true
        BUTTON-Ny:sensitive = false
        BUTTON-Ok:sensitive = false
        BUTTON-EndreTrans:sensitive = false
        CB-TTId:sensitive = true
        CB-TBId:sensitive = true
        CB-TTId2:sensitive = false
        T-VisAlle:Sensitive = false
        TransLogg.Butik:sensitive = true
        TransLogg.Dato:sensitive = true 
        FI-TilButik:SENSITIVE = FALSE
        TransLogg.ArtikkelNr:sensitive = false
        TransLogg.Storl:BGCOLOR      = ?
        /*TransLogg.Storl:LIST-ITEMS   = cOKStorl*/
        TransLogg.Storl:sensitive    = cOKStorl <> "" 
        TransLogg.Storl:SCREEN-VALUE = ENTRY(1,TransLogg.Storl:LIST-ITEMS)
        TransLogg.Antall:sensitive = true 
        TransLogg.Pris:sensitive = true 
        TransLogg.RabKr:sensitive = true 
        TransLogg.Mva:sensitive = false
        TransLogg.ForsNr:sensitive = true 
        TransLogg.KassaNr:sensitive = true 
        TransLogg.KundNr:sensitive = true 
        TransLogg.MedlemsNr:sensitive = true 
/*         TransLogg.LevNr:sensitive = true */
        BROWSE-TransLogg:sensitive = false
        BUTTON-SokArtikkelNr:sensitive = TRUE 
        BUTTON-SokBatch:sensitive = false
        BUTTON-SokDato:sensitive = true 
        BUTTON-SokForsalj:sensitive = true 
        BUTTON-SokFraBut:sensitive = true 
        BUTTON-SokKund:sensitive = true 
        BUTTON-SokMedlem:sensitive = true 
        BUTTON-Angre:sensitive = true
        BUTTON-LagreNy:sensitive = true
        BUTTON-Lagre:sensitive = true
        BUTTON-Motposter:sensitive = false
        BUTTON-NyTrans:sensitive = false      
        BUTTON-SokTilBut:sensitive = false 
        .
    when 2 then /* ENDRE */
      assign
        BUTTON-AngreB:sensitive = true
        BUTTON-Ny:sensitive = false
        BUTTON-Ok:sensitive = false
        BUTTON-EndreTrans:sensitive = false
        CB-TTId:sensitive = false
        CB-TBId:sensitive = false
        CB-TTId2:sensitive = FALSE
        T-VisAlle:Sensitive = false
        FI-TilButik:SENSITIVE = FALSE
        TransLogg.Butik:sensitive = true
        TransLogg.Dato:sensitive = true 
        TransLogg.ArtikkelNr:sensitive = false 
        /*TransLogg.Storl:LIST-ITEMS = cOKStorl*/
        TransLogg.Storl:SENSITIVE  = cOKStorl <> ""
        TransLogg.Storl:BGCOLOR      = ?
        TransLogg.Storl:SCREEN-VALUE = IF CAN-DO(cOKStorl,TransLogg.Storl) THEN TransLogg.Storl
                                            ELSE ENTRY(1,TransLogg.Storl:LIST-ITEMS)
        TransLogg.Antall:sensitive = true 
        TransLogg.Pris:sensitive = true 
        TransLogg.RabKr:sensitive = true 
        TransLogg.Mva:sensitive = false
        TransLogg.ForsNr:sensitive = true 
        TransLogg.KassaNr:sensitive = true 
        TransLogg.KundNr:sensitive = true 
        TransLogg.MedlemsNr:sensitive = true 
/*         TransLogg.LevNr:sensitive = true */
        BROWSE-TransLogg:sensitive = false
        BUTTON-SokArtikkelNr:sensitive = TRUE
        BUTTON-SokBatch:sensitive = false
        BUTTON-SokDato:sensitive = true 
        BUTTON-SokForsalj:sensitive = true 
        BUTTON-SokFraBut:sensitive = true 
        BUTTON-SokKund:sensitive = true 
        BUTTON-SokMedlem:sensitive = true 
        BUTTON-Angre:sensitive = true
        BUTTON-LagreNy:sensitive = false
        BUTTON-Lagre:sensitive = true
        BUTTON-Motposter:sensitive = false
        BUTTON-NyTrans:sensitive = false      
        BUTTON-SokTilBut:sensitive = false 
        .    
      when 3 then /* ENDRE */
        assign
          BUTTON-AngreB:sensitive = true
          BUTTON-Ny:sensitive = false
          BUTTON-Ok:sensitive = false
          BUTTON-EndreTrans:sensitive = false
          CB-TTId:sensitive = false
          CB-TBId:sensitive = false
          CB-TTId2:sensitive = false
          T-VisAlle:Sensitive = false
          FI-TilButik:SENSITIVE = TRUE
          TransLogg.Butik:sensitive = true
          TransLogg.Dato:sensitive = true 
          TransLogg.ArtikkelNr:sensitive = false 
          /*TransLogg.Storl:LIST-ITEMS = cOKStorl*/
          TransLogg.Storl:SENSITIVE  = cOKStorl <> ""
          TransLogg.Storl:BGCOLOR      = ?
          TransLogg.Storl:SCREEN-VALUE = IF CAN-DO(cOKStorl,TransLogg.Storl) THEN TransLogg.Storl
                                              ELSE ENTRY(1,TransLogg.Storl:LIST-ITEMS)
          TransLogg.Antall:sensitive = true 
          TransLogg.Pris:sensitive = true 
          TransLogg.RabKr:sensitive = true 
          TransLogg.Mva:sensitive = false
          TransLogg.ForsNr:sensitive = true 
          TransLogg.KassaNr:sensitive = true 
          TransLogg.KundNr:sensitive = true 
          TransLogg.MedlemsNr:sensitive = true 
  /*         TransLogg.LevNr:sensitive = true */
          BROWSE-TransLogg:sensitive = false
          BUTTON-SokArtikkelNr:sensitive = TRUE
          BUTTON-SokBatch:sensitive = false
          BUTTON-SokDato:sensitive = true 
          BUTTON-SokForsalj:sensitive = true 
          BUTTON-SokFraBut:sensitive = true 
          BUTTON-SokKund:sensitive = true 
          BUTTON-SokMedlem:sensitive = true 
          BUTTON-Angre:sensitive = true
          BUTTON-LagreNy:sensitive = false
          BUTTON-Lagre:sensitive = true
          BUTTON-Motposter:sensitive = false
          BUTTON-NyTrans:sensitive = false      
          BUTTON-SokTilBut:sensitive = true 
          .    
    when 9 then
      do:
      assign
        BUTTON-AngreB:sensitive = false
        BUTTON-Ny:sensitive = true
        BUTTON-Ok:sensitive = true
        BUTTON-EndreTrans:sensitive = AVAIL TransLogg
        FI-TilButik:SENSITIVE = FALSE
        CB-TTId:sensitive = false
        CB-TBId:sensitive = false
        CB-TTId2:sensitive = true
        T-VisAlle:Sensitive = true
        TransLogg.Butik:sensitive = false
        TransLogg.Dato:sensitive = false
        TransLogg.ArtikkelNr:sensitive = false
        TransLogg.Storl:SENSITIVE    = FALSE
        /*TransLogg.Storl:LIST-ITEMS   = IF AVAIL TransLogg THEN TransLogg.Storl ELSE ""*/
        TransLogg.Storl:SCREEN-VALUE = IF AVAIL TransLogg THEN TransLogg.Storl ELSE ""
        TransLogg.Antall:sensitive = false
        TransLogg.Pris:sensitive = false
        TransLogg.RabKr:sensitive = false
        TransLogg.Mva:sensitive = false
        TransLogg.ForsNr:sensitive = false
        TransLogg.KassaNr:sensitive = false
        TransLogg.KundNr:sensitive = false
        TransLogg.MedlemsNr:sensitive = false
/*         TransLogg.LevNr:sensitive = false */
        BROWSE-TransLogg:sensitive = true
        BUTTON-SokArtikkelNr:sensitive = false
        BUTTON-SokBatch:sensitive = true
        BUTTON-SokDato:sensitive = false
        BUTTON-SokForsalj:sensitive = false
        BUTTON-SokFraBut:sensitive = false
        BUTTON-SokKund:sensitive = false
        BUTTON-SokMedlem:sensitive = false
        BUTTON-Angre:sensitive = false
        BUTTON-LagreNy:sensitive = false
        BUTTON-Lagre:sensitive = false
        BUTTON-Motposter:sensitive = true 
        BUTTON-NyTrans:sensitive = true
        BUTTON-SokTilBut:sensitive = false 
        .
        
        /* Disabler endreposten hvis det ikke er noen poster å endre. */
        if not can-find(first TransLogg no-lock where
          TransLogg.BatchNr = int(FI-BatchNr:screen-value)) then
            BUTTON-EndreTrans:sensitive = false.
      end.
  end case.
end.

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
  DISPLAY FI-Medlem FI-BatchNr FI-OppdStatus CB-TTId CB-TBId FI-TilButik 
          CB-TTId2 T-VisAlle FILL-IN-9 FILL-IN-10 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE BatchLogg THEN 
    DISPLAY BatchLogg.Beskrivelse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE Forsalj THEN 
    DISPLAY Forsalj.FoNamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE Kunde THEN 
    DISPLAY Kunde.Navn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE TransLogg THEN 
    DISPLAY TransLogg.Storl TransLogg.ForsNr TransLogg.ArtikkelNr TransLogg.Dato 
          TransLogg.Butik TransLogg.KassaNr TransLogg.Vg TransLogg.LopNr 
          TransLogg.TilStorl TransLogg.Antall TransLogg.Pris TransLogg.Mva 
          TransLogg.RabKr TransLogg.MedlemsNr TransLogg.KundNr TransLogg.LevNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-Ny B-Godkjenn CB-TTId2 T-VisAlle BROWSE-TransLogg 
         BUTTON-NyTrans BUTTON-EndreTrans BUTTON-Motposter Btn_Help BUTTON-Ok 
         BUTTON-SokBatch FILL-IN-9 B-Kasse BUTTON-KopierTrans RECT-44 RECT-45 
         RECT-46 RECT-54 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTransBeskr C-Win 
PROCEDURE InitTransBeskr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign
    CB-TBId:list-items   in frame DEFAULT-FRAME = " "
    CB-TBId:screen-value in frame DEFAULT-FRAME = " ".
  
  for each TransBeskr no-lock where
    TransBeskr.TTId = wTTId:
    assign
      CB-TBId:list-items in frame DEFAULT-FRAME = 
                       CB-TBId:list-items in frame DEFAULT-FRAME +
                       (if CB-TBId:list-items in frame DEFAULT-FRAME = ""
                          then ""
                          else ",") +
                       string(TransBeskr.TBId,"zz9") + ": " + TransBeskr.Beskrivelse.
  end.

  find TransBeskr no-lock where
    TransBeskr.TTId = wTTId and
    TransBeskr.TBId = wTBId no-error.
  if available TransBeskr then
    if wTBId = TransBeskr.TBId then
      CB-TBId:screen-value in frame DEFAULT-FRAME = 
                       string(TransBeskr.TBId,"zz9") + ": " + TransBeskr.Beskrivelse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTransType C-Win 
PROCEDURE InitTransType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipModus as int no-undo.

  if ipModus = 1 then
    do:
      assign
        CB-TTId:list-items    in frame DEFAULT-FRAME = " "
        CB-TTId:screen-value  in frame DEFAULT-FRAME = " "
        CB-TTId2:list-items   in frame DEFAULT-FRAME = wAlle
        CB-TTId2:screen-value in frame DEFAULT-FRAME = wAlle.

      for each TransType NO-LOCK /*WHERE
          CAN-DO("1,2,3,4,10,11",STRING(TransTyp.TTId))*/:
        assign
          CB-TTId:list-items in frame DEFAULT-FRAME = 
                       CB-TTId:list-items in frame DEFAULT-FRAME +
                       (if CB-TTId:list-items in frame DEFAULT-FRAME = ""
                          then ""
                          else ",") +
                       string(TransType.TTId,"zz9") + ": " + TransType.Beskrivelse
          CB-TTId2:list-items in frame DEFAULT-FRAME = 
                     CB-TTId2:list-items in frame DEFAULT-FRAME +
                     (if CB-TTId2:list-items in frame DEFAULT-FRAME = ""
                        then ""
                        else ",") +
                     string(TransType.TTId,"zz9") + ": " + TransType.Beskrivelse.
          IF CAN-DO("1,2,3,4,10,11",STRING(TransTyp.TTId)) THEN
              ASSIGN cCB-TTIdNy = cCB-TTIdNy + (if cCB-TTIdNy = "" then "" else ",") +
                 string(TransType.TTId,"zz9") + ": " + TransType.Beskrivelse.
      end.
      ASSIGN cCB-TTIdAlle = CB-TTId:list-items.
    end.
  find TransType no-lock where
    TransType.TTId = wTTId.
  if available TransType then
    do:
      if TransType.TTId = wTTId /*and
         CAN-DO("1,2,3,4,10,11",STRING(TransTyp.TTId)) */ then
        CB-TTId:screen-value in frame DEFAULT-FRAME = 
                         string(TransType.TTId,"zz9") + ": " + TransType.Beskrivelse.
    end.
  CB-TTId2 = wAlle.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreTrans C-Win 
PROCEDURE LagreTrans :
/*
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOkMsg   as   char              no-undo.
  def var wTransNr like TransLogg.TransNr no-undo.
  def var wOk      as   log               no-undo.
  
  assign wOkMsg = "AVBRYT".
  
  do with frame DEFAULT-FRAME:
  IF NOT AVAIL ArtBas THEN DO:
      message "Ukjent artikkel!"
        view-as alert-box MESSAGE title "Lagringsfeil".
      RUN SokArtBas.
      IF RETURN-VALUE <> "AVBRYT" THEN DO:
          run EnableFelt (2).
      END.
      return no-apply wOkMsg.
  END.
  ELSE IF NOT CAN-FIND(Forsalj WHERE
       DEC(Forsalj.ForsNr) = INPUT TransLogg.ForsNr) THEN DO:
      MESSAGE "Ugyldig kasserernummer"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
      APPLY "entry" TO TransLogg.ForsNr.
      RETURN NO-APPLY.
  END.
  ELSE IF INPUT TransLogg.KundNr <> 0 THEN
  DO:
    /* Gyldig kundenummer. */
    IF NOT CAN-FIND(Kunde WHERE
                    Kunde.KundeNr = INPUT TransLogg.KundNr) THEN
    DO:
        MESSAGE "Ugyldig kundenummer"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
        APPLY "entry" TO TransLogg.KundNr.
        RETURN NO-APPLY.
    END.
    /* Tilhører medlemmet denne kunden */
    IF INPUT TransLogg.MedlemsNr <> 0 THEN
    DO:
      IF NOT CAN-FIND(Medlem WHERE
                      Medlem.MedlemsNr = INPUT TransLogg.MedlemsNr AND
                      MEdlem.KundeNr   = INPUT TransLogg.KundNr) THEN
      DO:
        MESSAGE "Medlemmet tilhører ikke angitt kunde"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
        APPLY "entry" TO TransLogg.MedlemsNr.
        RETURN NO-APPLY.
      END.
    END.
  END.

  IF INPUT TransLogg.MedlemsNr <> 0 THEN
  DO:
    IF NOT CAN-FIND(Medlem WHERE
                    Medlem.MedlemsNr = INPUT TransLogg.MedlemsNr) THEN
    DO:
        MESSAGE "Ugyldig medlemsnummer"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
        APPLY "entry" TO TransLogg.MedlemsNr.
        RETURN NO-APPLY.
    END.

  END.
  find ArtPris no-lock where
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
    ArtPris.ProfilNr   = wProfilNr no-error.
  IF NOT AVAILABLE ArtPris THEN
    FIND ArtPris NO-LOCK where
         ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
         ArtPris.ProfilNr   = clButiker.ProfilNr no-error.

  if not available ArtPris then
    do:
      message "Ukjent artikkel!"
        view-as alert-box MESSAGE title "Lagringsfeil".
      return no-apply wOkMsg.
    end.
  find Butiker no-lock where
     Butiker.Butik = int(TransLogg.Butik:screen-value) no-error.
  if not available Butiker then
    do:
      message "Ukjent butikk!"
        view-as alert-box MESSAGE title "Lagringsfeil".
      return no-apply wOkMsg.
    end.
    IF NOT CAN-FIND(Kasse WHERE Kasse.Butik = INPUT TransLogg.Butik AND
                    Kasse.GruppeNr = 1 AND
                    Kasse.KasseNr = INPUT TransLogg.KassaNr) THEN DO:
        message "Ukjent kasse!"
          view-as alert-box MESSAGE title "Lagringsfeil".
        APPLY "ENTRY" TO TransLogg.KassaNr.
        return no-apply wOkMsg.
   END.
  if FI-TilButik:sensitive = TRUE AND INT(FI-TilButik:SCREEN-VALUE) > 0 then
    do:
      find Butiker no-lock where
         Butiker.Butik = int(FI-TilButik:screen-value) no-error.
      if not available Butiker then
        do:
          message "Ukjent mottagende butikk for overføring!"
            view-as alert-box MESSAGE title "Lagringsfeil".
          APPLY "ENTRY" TO TransLogg.Butik.
          return no-apply wOkMsg.
        end.
    end.
  find VarGr no-lock where
     VarGr.Vg = int(TransLogg.Vg:screen-value) no-error.
  if not available VarGr then
    do:
      message "Ukjent varegruppe!"
        view-as alert-box MESSAGE title "Lagringsfeil".
      return no-apply wOkMsg.
    end.
  find first StrTStr no-lock where
     trim(StrTStr.SoStorl) = trim(input TransLogg.Storl) no-error.
  if not available StrTStr then
    do:
      message "Ukjent størrelse!"
        view-as alert-box MESSAGE title "Lagringsfeil".
      return no-apply wOkMsg.
    end.

  IF NOT CAN-DO("1,2,3,4,6,10,11",STRING(int(substring(CB-TTId:screen-value in frame DEFAULT-FRAME,1,3)))) THEN
  DO:
      MESSAGE "Transaksjonstype " CB-TTId:screen-value in frame DEFAULT-FRAME " kan ikke registreres manuelt." SKIP
              "Kun transaksjonstypene 1, 2, 3, 4, 6, 10 og 11 kan benyttes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY wOkMsg.
  END.

  find TransLogg no-lock where
    recid(TransLogg) = wTransLoggRecid no-error.

  /* Oppretter ny post */
  if wModus = "NY" then
  OPPRETT-LAGRE-TRANS:
  do transaction:
    find last TransLogg NO-LOCK where
      TransLogg.Butik = int(TransLogg.Butik:screen-value) 
      use-index TransLogg no-error.
    if available TransLogg then
      wTransNR = TransLogg.TransNr + 1.
    else
      wTransNr = 1.
      
    create TransLogg.

    assign TransLogg.Butik        = int(TransLogg.Butik:screen-value)
           TransLogg.TransNr      = wTransNr
           TransLogg.SeqNr        = 1
           wTransLoggRecid        = recid(TransLogg).
           
    assign TransLogg.BatchNr      = FI-BatchNr
           TransLogg.KundNr       = dec(TransLogg.KundNr:SCREEN-VALUE)
           TransLogg.MedlemsNr    = dec(TransLogg.MedlemsNr:SCREEN-VALUE)
           TransLogg.ForsNr       = int(TransLogg.ForsNr:screen-value)
           TransLogg.TTId         = wTTId
           TransLogg.TBId         = wTBId
           TransLogg.ArtikkelNr   = ArtBas.ArtikkelNr
           TransLogg.LevNr        = int(TransLogg.LevNr:screen-value)
           TransLogg.BongId       = 0
           TransLogg.BongLinjeNr  = 0
           TransLogg.KassaNr      = INPUT TransLogg.KassaNr
           TransLogg.Vg           = int(TransLogg.Vg:screen-value)
           TransLogg.LopNr        = int(TransLogg.LopNr:screen-value)
           TransLogg.Storl        = TransLogg.Storl:screen-value
           TransLogg.TilStorl     = if wTTId = 6 
                                      then TransLogg.TilStorl:screen-value
                                      else TransLogg.Storl:screen-value
           TransLogg.Antall       = dec(TransLogg.Antall:screen-value)
           TransLogg.Antall       = TransLogg.Antall * (if can-do("3,10",string(wTTId))
                                                         then -1
                                                         else 1)
           TransLogg.Pris         = dec(TransLogg.Pris:screen-value)
           TransLogg.RabKr        = dec(TransLogg.RabKr:screen-value)
           TransLogg.Mva          = dec(TransLogg.Mva:screen-value)
           TransLogg.Plukket      = false /* Skal ikke ut på plukkliste */
           TransLogg.Dato         = INPUT TransLogg.Dato
           TransLogg.Tid          = time
           TransLogg.BestNr       = 0
           TransLogg.Postert      = false
           TransLogg.OvButik      = if wTTId = 6 then int(FI-TilButik:screen-value) else 0
           TransLogg.OvTransNr    = if wTTId = 6 then TransLogg.TransNr else 0.

    RELEASE TransLogg.  
  end. /* OPPRETT-LAGRE-TRANS */
  else if wModus = "ENDRE" then
  do transaction:
    find current TransLogg exclusive-lock.
    assign frame DEFAULT-FRAME
           /* TransLogg.Butik */
           TransLogg.KundNr
           TransLogg.MedlemsNr
           TransLogg.ForsNr
           /*
           TransLogg.TTId  
           TransLogg.TBId  
           */
           TransLogg.ArtikkelNr
           TransLogg.LevNr 
           /*       
           TransLogg.BongId       
           TransLogg.BongLinjeNr  
           */
           TransLogg.KassaNr      
           TransLogg.Vg           
           TransLogg.LopNr        
           TransLogg.Storl        
           TransLogg.TilStorl     = if wTTId = 6 
                                      then TransLogg.TilStorl:screen-value
                                      else TransLogg.Storl:screen-value
           TransLogg.Antall       
           TransLogg.Pris         
           TransLogg.RabKr        
           TransLogg.Mva          
           TransLogg.FeilKode = 0
           /* TransLogg.Plukket */
           TransLogg.Dato
           /* TransLogg.Tid */
           /* TransLogg.BestNr */
           /* TransLogg.Postert */
           TransLogg.OvButik      = if wTTId = 6 then int(FI-TilButik:screen-value) else 0
           TransLogg.OvTransNr    = if wTTId = 6 then TransLogg.TransNr else 0.
    release TransLogg.
  end.

  find TransLogg no-lock where
    recid(TransLogg) = wTransLoggRecid.
    
  if available TransLogg then
    display 
      TransLogg.ArtikkelNr
    with frame DEFAULT-FRAME.

  if BatchLogg.OppdStatus < 2 then
    run batchstatus.p (FI-BatchNr, 1).
  if return-value = "AVBRYT" then
    message "Transaksjon lagret, men klarte ikke å oppdatere status på BatchLogg!" skip
            "Kontroller batchloggens oppdateringsstatus. Den må ha 0, 1 eller 2 for at den nye " skip
            "transaksjonen skal bli behandlet."
            view-as alert-box ERROR title "Lagringsfeil".

  assign wOkMsg = "OK".

  return wOkMsg.
  
  end. /* FRAME blokk */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MotPosterTrans C-Win 
PROCEDURE MotPosterTrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wStatus as char  no-undo.
  def var wRecid  as recid no-undo.
  
  def buffer bufTransLogg for TransLogg.
  
  wStatus = "AVBRYT".
  
  if not available TransLogg then
    return wStatus.
  if TransLogg.SeqNr > 1 then
    return wStatus.

  do TRANSACTION:    
    find current TransLogg exclusive-lock no-wait.
    if locked TransLogg then
      do:
        message "Posten holdes av en annen bruker!" 
          view-as alert-box MESSAGE title "Melding".
        return wStatus.      
      end.
    create BufTransLogg.
  
    {translogg.i bufTransLogg TransLogg " + 1" true}
    
    /* Hvis transaksjonen ikke er oppdatert når den motposteres, flagges den */
    /* direkte som ferdigbehandlet.                                          */
    if TransLogg.Postert = false then
      do:
        assign
          wRecid = recid(TransLogg)
          TransLogg.Postert        = true
          TransLogg.PostertDato    = today
          TransLogg.PostertTid     = time
          TransLogg.Plukket        = true
          bufTransLogg.Postert     = true
          bufTransLogg.PostertDato = today
          bufTransLogg.PostertTid  = time
          bufTransLogg.Plukket     = true.
        release TransLogg.
        find TransLogg no-lock where
          recid(TransLogg) = wRecid.
      end.
  end. /* TRANSACTION */
  
  wStatus = "OK".
  return no-apply wStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterDagsrapport C-Win 
PROCEDURE OppdaterDagsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {dagsrapport.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokArtBas C-Win 
PROCEDURE SokArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wArtikkelNr as dec no-undo.
  def var wLoop        as int   no-undo.

  run d-hsok.w (output wArtikkelNr,"").

  IF wArtikkelNr = ? OR 
      NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = wArtikkelNr) THEN
      RETURN "AVBRYT".
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
  IF ArtBas.Pakke THEN DO:
      MESSAGE "Pakkeartikkel kann ikke velges." 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  IF ArtBas.LopNr = 0 OR ArtBas.LopNr = ? THEN DO:
      MESSAGE "Artikkel mangler løpnr." 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.

  if available ArtBas then
    do:
      ASSIGN 
          TransLogg.Storl:LIST-ITEMS IN FRAME Default-Frame = getOKStorl(ArtBas.Vg,ArtBas.LopNr)
          TransLogg.Storl:SCREEN-VALUE = ENTRY(1,TransLogg.Storl:LIST-ITEMS)
          TransLogg.Dato:SCREEN-VALUE = STRING(TODAY). 
      
      find ArtPris no-lock where
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
        ArtPris.ProfilNr   = wProfilNr no-error.
      if available ArtPris then
        assign wLoop = if ArtPris.Tilbud then 2 else 1.
      find LevBas of ArtBas no-lock no-error.
      
      display 
        ArtBas.Vg            @ TransLogg.Vg 
        ArtBas.LopNr         @ TransLogg.LopNr
        ArtBas.ArtikkelNr    @ TransLogg.ArtikkelNr
        ArtPris.Pris[wLoop]  WHEN wModus <> "ENDRE"  @ TransLogg.Pris   
        ArtPris.MvaKr[wLoop] WHEN wModus <> "ENDRE"  @ TransLogg.Mva    
        1                    WHEN wModus <> "ENDRE"  @ Translogg.Antall 
        ArtBas.LevNr         @ TransLogg.LevNr
        ""                   @ LevBas.LevNamn
        LevBas.LevNamn when available LevBas  
      with frame DEFAULT-FRAME. 
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisLinje C-Win 
PROCEDURE VisLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wStatuskoder AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      IF AVAILABLE TransLogg THEN DO:
          FIND ArtBas WHERE ArtBas.ArtikkelNr =  TransLogg.ArtikkelNr NO-LOCK NO-ERROR.
          find LevBas  NO-LOCK WHERE LevBas.LevNr        = TransLogg.LevNr  NO-ERROR.
          find Forsalj NO-LOCK WHERE DEC(Forsalj.ForsNr) = TransLogg.ForsNr NO-ERROR.
          find Kunde   NO-LOCK WHERE Kunde.KundeNr       = TransLogg.KundNr NO-ERROR.
          FIND Medlem  NO-LOCK WHERE MEdlem.MEdlemsNr    = TransLogg.MEdlemsNr NO-ERROR.
          ASSIGN cOKStorl = getOKStorl(TransLogg.Vg,TransLogg.LopNr)
                 TransLogg.Storl:LIST-ITEMS = TransLogg.Storl
                 TransLogg.Storl:SCREEN-VALUE = TransLogg.Storl.
          IF NOT CAN-DO(cOKStorl,TransLogg.Storl) THEN
              ASSIGN TransLogg.Storl:BGCOLOR = 12.
          ELSE 
              ASSIGN TransLogg.Storl:BGCOLOR = ?.
          IF TransLogg.TTId = 1 AND TransLogg.TBId = 2 THEN DO:
              ASSIGN FI-TilButik:LABEL = "Fra butikk".
              display TransLogg.OvButik @ FI-TilButik.
          END.
          ELSE IF TransLogg.TTId = 6 THEN DO:
              ASSIGN FI-TilButik:LABEL = "Til butikk".
              DISPLAY TransLogg.OvButik @ FI-TilButik.
          END.
          ELSE DO:
              ASSIGN FI-TilButik:LABEL = "Til butikk".
              DISPLAY "" @ FI-TilButik.
          END.
          DISPLAY TransLogg.Butik 
                  TransLogg.Dato 
                  TransLogg.ArtikkelNr 
                  TransLogg.Vg 
                  TransLogg.LopNr 
                  TransLogg.Storl 
                  TransLogg.Antall 
                  TransLogg.Pris 
                  TransLogg.RabKr 
                  TransLogg.Mva 
                  TransLogg.ForsNr 
                  TransLogg.KassaNr 
                  TransLogg.KundNr 
                  TransLogg.MedlemsNr
                  TransLogg.LevNr
                  /* 
                  TransLogg.BongId 
                  TransLogg.BongLinjeNr 
                  TransLogg.BestNr 
                  TransLogg.TransNr 
                  TransLogg.TTId 
                  TransLogg.TBId 
                  TransLogg.Plukket 
                  TransLogg.Postert 
                  TransLogg.PostertDato 
                  */
                  "" @ LevBas.LevNamn
                  LevBas.LevNamn WHEN AVAILABLE LevBas
                  "" @ Kunde.Navn
                  Kunde.Navn WHEN AVAILABLE Kunde
                  "" @ Forsalj.FoNamn
                  Forsalj.FoNamn WHEN AVAILABLE Forsalj
                  "" @ FI-Medlem.
          IF AVAILABLE Medlem THEN
              DISPLAY Medlem.ForNavn + " " + Medlem.EtterNavn @ FI-Medlem.
          ASSIGN wTTId = TransLogg.TTId
                 wTBId = TransLogg.TBId.
          RUN InitTransType (2).
          RUN InitTransBeskr.
          FIND BatchLogg NO-LOCK WHERE BatchLogg.BatchNr = INPUT FI-BatchNr NO-ERROR.
          IF AVAILABLE BatchLogg THEN DO:
              {syspara.i 3 10 20 wStatuskoder}
              IF NUM-ENTRIES(wStatuskoder) >= BatchLogg.OppdStatus AND BatchLogg.OppdStatus > 0 THEN
                  FI-OppdStatus = entry(BatchLogg.OppdStatus,wStatusKoder).
              ELSE
                  FI-OppdStatus = "".    
          END.
          ELSE
              FI-OppdStatus = "".    
          DISPLAY FI-OppdStatus WITH FRAME Default-FRAME.
          ASSIGN cOKStorl = getOKStorl(TransLogg.Vg,TransLogg.LopNr).
      END.
      ELSE DO:
          RELEASE ArtBas.
          RELEASE LevBas. 
          RELEASE Forsalj.
          RELEASE Kunde.  
          RELEASE Medlem. 
          RUN BlankLinje.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOKStorl C-Win 
FUNCTION getOKStorl RETURNS CHARACTER
  ( INPUT ipVg AS INTEGER,
    INPUT ipLopNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.

  /*
  FOR EACH ArtLag WHERE ArtLag.Vg = ipVg AND ArtLag.LopNr = ipLopNr NO-LOCK.
      IF NOT CAN-DO(cString,ArtLag.Storl) THEN
          ASSIGN cString = cString + (IF cString = "" THEN "" ELSE ",") + ArtLag.Storl.
  END.
  */
  FIND bArtBas NO-LOCK WHERE
      bArtBas.Vg    = ipVg AND
      bArtBas.LopNr = ipLopNr NO-ERROR.
  IF AVAILABLE bArtBas THEN
  DO:
      FOR EACH StrTStr WHERE 
          StrTStr.StrTypeId = bArtBas.StrTypeId NO-LOCK:
          IF NOT CAN-DO(cString,StrTStr.SoStorl) THEN
              ASSIGN cString = cString 
                               + (IF cString = "" 
                                     THEN "" 
                                     ELSE ",") 
                               + StrTStr.SoStorl.
      END.
  END.
  RETURN cString.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

