&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_TransLogg NO-UNDO LIKE TransLogg
       FIELD Beskr LIKE ArtBas.Beskr
       FIELD Levkod LIKE ArtBas.Levkod
       FIELD Sasong LIKE SaSong.Sasong
       FIELD SasBeskr LIKE SaSong.SasBeskr
       FIELD ListPris LIKE TransLogg.Pris
       FIELD Profil AS CHARACTER
       FIELD Tabell AS CHARACTER
       FIELD VgLopNr as CHARACTER FORMAT "X(16)"
       field Avvik% as dec format ">>>9.9-"
       .


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
DEFINE VARIABLE iCL      LIKE Butiker.Butik NO-UNDO.
DEFINE VARIABLE cVgLopnr AS CHAR            NO-UNDO.
DEFINE VARIABLE hQuery   AS HANDLE          NO-UNDO.
DEFINE VARIABLE hSortCol AS HANDLE          NO-UNDO.

DEFINE VARIABLE cSortString AS CHAR         NO-UNDO.
DEFINE VARIABLE cColLabels  AS CHAR         NO-UNDO.
DEFINE VARIABLE iProfilNrCL AS INTE  INIT ? NO-UNDO.
DEFINE STREAM Eksport.
&SCOPED-DEFINE BrowseQ FOR EACH TT_TransLogg NO-LOCK &1 INDEXED-REPOSITION
&SCOPED-DEFINE FirstSort Dato
&SCOPED-DEFINE SortBgCol 15

DEF VAR lIsLocked  AS INTEGER NO-UNDO INITIAL 0.

DEF VAR pdcAvvik AS DECIMAL FORMAT "->>>>9.9" NO-UNDO.
DEF VAR hParent  AS HANDLE NO-UNDO.
DEF VAR hvArtKor AS HANDLE NO-UNDO.
DEF VAR cRab%    AS DEC    NO-UNDO.

ASSIGN cSortString = "BY Dato".

DEF BUFFER bufButiker FOR Butiker.

{runlib.i}
{windows.i}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_TransLogg

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 TT_TransLogg.Dato ~
TT_TransLogg.Butik TT_TransLogg.KassaNr TT_TransLogg.ArtikkelNr ~
TT_TransLogg.VgLopnr TT_TransLogg.Beskr TT_TransLogg.LevNr ~
TT_TransLogg.Levkod TT_TransLogg.SasBeskr TT_TransLogg.ListPris ~
TT_TransLogg.Pris TT_TransLogg.RabKr cRab% TT_TransLogg.Avvik% ~
TT_TransLogg.Storl TT_TransLogg.Antall TT_TransLogg.ForsNr ~
TT_TransLogg.Profil TT_TransLogg.Tabell 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 TT_TransLogg.Dato 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 TT_TransLogg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 TT_TransLogg
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH TT_TransLogg NO-LOCK ~
    BY TT_TransLogg.Dato INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH TT_TransLogg NO-LOCK ~
    BY TT_TransLogg.Dato INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 TT_TransLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 TT_TransLogg


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-TTId FI-FraDat Btn_Done FI-TilDat ~
Btn_Help BUTTON-SokDato FI-Avvik BUTTON-SokDato-2 RS-AnalysTyp B-Utfor ~
FI-Urvaltxt BROWSE-1 FI-Filtertxt RECT-47 RECT-48 RECT-53 RECT-54 ~
B-SpinFraUp B-SpinTilUp B-SpinFraDown B-SpinTilDown 
&Scoped-Define DISPLAYED-OBJECTS CB-TTId FI-FraDat FI-TilDat FI-Avvik ~
RS-AnalysTyp FI-LevNr FI-LevNamn FI-Urvaltxt CB-Varegruppe FI-LopNrFra ~
FI-LopNrTil CB-Sasong CB-Butiker CB-Kasserer FI-Filtertxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu C-Win 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProfilStr C-Win 
FUNCTION getProfilStr RETURNS CHARACTER
  ( INPUT iButikProfil AS INTEGER, INPUT iCLProfil AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-ArtKort  NO-FOCUS
     LABEL "&Artikkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel.bmp":U NO-FOCUS
     LABEL "&Excel" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON B-Nullstill 
     LABEL "Nullstill" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SokLev 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Søk i leverandørregister".

DEFINE BUTTON B-SpinFraDown 
     LABEL "-" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-SpinFraUp 
     LABEL "+" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-SpinTilDown 
     LABEL "-" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-SpinTilUp 
     LABEL "+" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-Utfor 
     LABEL "&Utfør" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Done 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.4 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.4 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.05.

DEFINE VARIABLE CB-Butiker AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Butikk" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEMS "          [Alle]" 
     DROP-DOWN-LIST
     SIZE 49.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Kasserer AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Selger" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEMS "          [Alle]" 
     DROP-DOWN-LIST
     SIZE 49.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sasong AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ses&ong" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEMS "          [Alle]" 
     DROP-DOWN-LIST
     SIZE 49.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-TTId AS INTEGER FORMAT "zz9" INITIAL 1 
     LABEL "Transtype" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "",0
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Varegruppe AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Varegruppe" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     LIST-ITEMS "          [Alle]" 
     DROP-DOWN-LIST
     SIZE 49.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avvik AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Avvik %" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filtertxt AS CHARACTER FORMAT "X(256)":U INITIAL " Filter" 
      VIEW-AS TEXT 
     SIZE 7.2 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FraDat AS DATE FORMAT "99/99/99":U 
     LABEL "&Fra" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNrFra AS INTEGER FORMAT "zzz9" INITIAL 0 
     LABEL "Løpenr fra / til" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNrTil AS INTEGER FORMAT "zzz9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDat AS DATE FORMAT "99/99/99":U 
     LABEL "&Til" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Urvaltxt AS CHARACTER FORMAT "X(256)":U INITIAL " Urval" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE RS-AnalysTyp AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Rabattavvik", 1,
"Prisavvik", 2
     SIZE 33 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 6.67.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 6.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      TT_TransLogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      TT_TransLogg.Dato COLUMN-LABEL "Dato **" FORMAT "99/99/9999":U
      TT_TransLogg.Butik COLUMN-LABEL "Butik" FORMAT ">>>>>9":U
      TT_TransLogg.KassaNr COLUMN-LABEL "Kasse" FORMAT "zzz9":U
            WIDTH 6
      TT_TransLogg.ArtikkelNr COLUMN-LABEL "Art.nr *" FORMAT "zzzzzzzzzzzz9":U
      TT_TransLogg.VgLopnr COLUMN-LABEL "Vg/Lopnr *" WIDTH 12.4
      TT_TransLogg.Beskr COLUMN-LABEL "Beskr" WIDTH 12
      TT_TransLogg.LevNr COLUMN-LABEL "Levnr **" FORMAT "zzzzz9":U
      TT_TransLogg.Levkod COLUMN-LABEL "LevKod"
      TT_TransLogg.SasBeskr COLUMN-LABEL "Sesong *" WIDTH 10
      TT_TransLogg.ListPris COLUMN-LABEL "Listpris" WIDTH 10
      TT_TransLogg.Pris FORMAT "-zz,zzz,zz9.99":U
      TT_TransLogg.RabKr FORMAT "->,>>>,>>9.99":U
      cRab% COLUMN-LABEL "Rab%" FORMAT "->>9.9":U
      TT_TransLogg.Avvik% COLUMN-LABEL "Avvik% **"
      TT_TransLogg.Storl FORMAT "x(10)":U
      TT_TransLogg.Antall FORMAT "-zz,zzz,zz9":U
      TT_TransLogg.ForsNr FORMAT ">>>>>9":U
      TT_TransLogg.Profil COLUMN-LABEL "Profiler"
      TT_TransLogg.Tabell COLUMN-LABEL "Tabell"
  ENABLE
      TT_TransLogg.Dato
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 154.8 BY 16.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Excel AT ROW 1.24 COL 19
     CB-TTId AT ROW 3.52 COL 12.6 COLON-ALIGNED HELP
          "TransaksjonstypensID"
     B-ArtKort AT ROW 1.24 COL 3
     FI-FraDat AT ROW 4.67 COL 12.6 COLON-ALIGNED
     Btn_Done AT ROW 1.24 COL 152.6 NO-TAB-STOP 
     FI-TilDat AT ROW 4.67 COL 41.2 COLON-ALIGNED
     Btn_Help AT ROW 1.24 COL 147.6 NO-TAB-STOP 
     BUTTON-SokDato AT ROW 4.67 COL 34.2
     FI-Avvik AT ROW 5.81 COL 12.6 COLON-ALIGNED
     BUTTON-SokDato-2 AT ROW 4.67 COL 62.8
     RS-AnalysTyp AT ROW 6.95 COL 14 NO-LABEL
     B-SokLev AT ROW 3 COL 98.4
     B-Utfor AT ROW 8.19 COL 52
     FI-LevNr AT ROW 3 COL 85.8 COLON-ALIGNED
     FI-LevNamn AT ROW 3 COL 101.4 COLON-ALIGNED NO-LABEL
     FI-Urvaltxt AT ROW 2.67 COL 1 COLON-ALIGNED NO-LABEL
     BROWSE-1 AT ROW 9.81 COL 2.2
     CB-Varegruppe AT ROW 4.05 COL 85.6 COLON-ALIGNED
     FI-LopNrFra AT ROW 5.14 COL 85.6 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen"
     FI-LopNrTil AT ROW 5.14 COL 96.8 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen" NO-LABEL
     CB-Sasong AT ROW 6.24 COL 85.6 COLON-ALIGNED
     CB-Butiker AT ROW 7.24 COL 85.6 COLON-ALIGNED
     CB-Kasserer AT ROW 8.33 COL 85.6 COLON-ALIGNED
     B-Nullstill AT ROW 6.71 COL 139.4
     B-Aktiver AT ROW 8.19 COL 139.4
     FI-Filtertxt AT ROW 2.67 COL 69.4 COLON-ALIGNED NO-LABEL
     B-SpinFraUp AT ROW 4.57 COL 30.4
     B-SpinTilUp AT ROW 4.62 COL 59.2
     B-SpinFraDown AT ROW 5.19 COL 30.4
     B-SpinTilDown AT ROW 5.24 COL 59.2
     RECT-47 AT ROW 2.48 COL 1
     RECT-48 AT ROW 1.05 COL 1
     RECT-53 AT ROW 2.91 COL 2
     RECT-54 AT ROW 2.91 COL 70.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         CANCEL-BUTTON B-SokLev.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_TransLogg T "?" NO-UNDO skotex TransLogg
      ADDITIONAL-FIELDS:
          FIELD Beskr LIKE ArtBas.Beskr
          FIELD Levkod LIKE ArtBas.Levkod
          FIELD Sasong LIKE SaSong.Sasong
          FIELD SasBeskr LIKE SaSong.SasBeskr
          FIELD ListPris LIKE TransLogg.Pris
          FIELD Profil AS CHARACTER
          FIELD Tabell AS CHARACTER
          FIELD VgLopNr as CHARACTER FORMAT "X(16)"
          field Avvik% as dec format ">>>9.9-"
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Alarmliste rabatt/prisavvik"
         HEIGHT             = 25.1
         WIDTH              = 157
         MAX-HEIGHT         = 25.1
         MAX-WIDTH          = 157
         VIRTUAL-HEIGHT     = 25.1
         VIRTUAL-WIDTH      = 157
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE Size-to-Fit Custom                                       */
/* BROWSE-TAB BROWSE-1 FI-Urvaltxt DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON B-Aktiver IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-ArtKort IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Excel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Nullstill IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SokLev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 4.

ASSIGN 
       TT_TransLogg.Dato:COLUMN-READ-ONLY IN BROWSE BROWSE-1 = TRUE.

/* SETTINGS FOR COMBO-BOX CB-Butiker IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX CB-Kasserer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX CB-Sasong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX CB-Varegruppe IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LopNrFra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LopNrTil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "Temp-Tables.TT_TransLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.TT_TransLogg.Dato|yes"
     _FldNameList[1]   > Temp-Tables.TT_TransLogg.Dato
"TT_TransLogg.Dato" "Dato **" ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[2]   > Temp-Tables.TT_TransLogg.Butik
"TT_TransLogg.Butik" "Butik" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.TT_TransLogg.KassaNr
"TT_TransLogg.KassaNr" "Kasse" ? "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.TT_TransLogg.ArtikkelNr
"TT_TransLogg.ArtikkelNr" "Art.nr *" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > "_<CALC>"
"TT_TransLogg.VgLopnr" "Vg/Lopnr *" ? ? ? ? ? ? ? ? no ? no no "12.4" yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"TT_TransLogg.Beskr" "Beskr" ? ? ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.TT_TransLogg.LevNr
"TT_TransLogg.LevNr" "Levnr **" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > "_<CALC>"
"TT_TransLogg.Levkod" "LevKod" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > "_<CALC>"
"TT_TransLogg.SasBeskr" "Sesong *" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[10]   > "_<CALC>"
"TT_TransLogg.ListPris" "Listpris" ? ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[11]   = Temp-Tables.TT_TransLogg.Pris
     _FldNameList[12]   = Temp-Tables.TT_TransLogg.RabKr
     _FldNameList[13]   > "_<CALC>"
"cRab%" "Rab%" "->>9.9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > "_<CALC>"
"TT_TransLogg.Avvik%" "Avvik% **" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[15]   = Temp-Tables.TT_TransLogg.Storl
     _FldNameList[16]   = Temp-Tables.TT_TransLogg.Antall
     _FldNameList[17]   = Temp-Tables.TT_TransLogg.ForsNr
     _FldNameList[18]   > "_<CALC>"
"TT_TransLogg.Profil" "Profiler" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[19]   > "_<CALC>"
"TT_TransLogg.Tabell" "Tabell" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Alarmliste rabatt/prisavvik */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Alarmliste rabatt/prisavvik */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver C-Win
ON CHOOSE OF B-Aktiver IN FRAME DEFAULT-FRAME /* Aktiver */
DO:
    ASSIGN FI-LopNrFra
           FI-LopNrTil.
    IF FI-LopNrFra = ? OR FI-LopNrTil = ? THEN
        ASSIGN FI-LopNrFra:SCREEN-VALUE = "0"
               FI-LopNrTil:SCREEN-VALUE = "0".
    ELSE IF FI-LopNrFra > FI-LopNrTil AND FI-LopNrTil <> 0 THEN DO:
        MESSAGE "Fra > til"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-LopnrFra.
        RETURN NO-APPLY.
    END.
    run QPrepare.
    hQuery:QUERY-OPEN().
    ASSIGN B-ArtKort:SENSITIVE = NOT BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ?
           B-Excel:SENSITIVE   = NOT BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ?.
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ArtKort C-Win
ON CHOOSE OF B-ArtKort IN FRAME DEFAULT-FRAME /* Artikkelkort */
DO:
  IF NOT AVAILABLE TT_Translogg THEN
      RETURN NO-APPLY.

  RUN Artikkel.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Excel */
DO:
  RUN LockWindowUpdate(FRAME Default-Frame:HWND, OUTPUT lIsLocked).
  RUN ExportToExcel.
  RUN LockWindowUpdate(FRAME Default-Frame:HWND, OUTPUT lIsLocked).
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Nullstill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Nullstill C-Win
ON CHOOSE OF B-Nullstill IN FRAME DEFAULT-FRAME /* Nullstill */
DO:
    RUN NullstillFilter.
    APPLY "CHOOSE" TO B-Aktiver.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLev C-Win
ON CHOOSE OF B-SokLev IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-LevNr
DO:
    {soek.i
      &Felt       = FI-LevNr
      &Frame-Name = DEFAULT-FRAME
      &Program    = d-blevbas.w 
      &OptDisp    = "LevBas.LevNr when available LevBas @ FI-LevNr 
                     LevBas.LevNamn  when available LevBas @ FI-LevNamn"
      &PostRun    = "find LevBas no-lock where
                       recid(LevBas) = int(return-value) no-error."
    }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinFraDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinFraDown C-Win
ON CHOOSE OF B-SpinFraDown IN FRAME DEFAULT-FRAME /* - */
DO:
    ASSIGN FI-FraDat = FI-FraDat - 1.
    DISPLAY FI-FraDat with frame {&FRAME-NAME}.
    APPLY "ENTRY" TO FI-FraDat.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinFraUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinFraUp C-Win
ON CHOOSE OF B-SpinFraUp IN FRAME DEFAULT-FRAME /* + */
DO:
    IF FI-FraDat = TODAY /* OR FI-FraDat = FI-TilDat */ THEN
        RETURN NO-APPLY.
    ASSIGN FI-FraDat = FI-FraDat + 1.
    DISPLAY FI-FraDat WITH FRAME {&FRAME-NAME}.
    APPLY "LEAVE" TO FI-FraDat.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinTilDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinTilDown C-Win
ON CHOOSE OF B-SpinTilDown IN FRAME DEFAULT-FRAME /* - */
DO:
    IF FI-TilDat = FI-FraDat THEN
        RETURN NO-APPLY.
    ASSIGN FI-TilDat = FI-TilDat - 1.
    DISPLAY FI-TilDat with frame {&FRAME-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinTilUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinTilUp C-Win
ON CHOOSE OF B-SpinTilUp IN FRAME DEFAULT-FRAME /* + */
DO:
    IF FI-TilDat = TODAY THEN
        RETURN NO-APPLY.
    ASSIGN FI-TilDat = FI-TilDat + 1.
    DISPLAY FI-TilDat with frame {&FRAME-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Utfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Utfor C-Win
ON CHOOSE OF B-Utfor IN FRAME DEFAULT-FRAME /* Utfør */
DO:
  RUN NullstillFilter.
  RUN Utfor.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE":U TO B-ArtKort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON ROW-DISPLAY OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE TT_TransLogg THEN
  DO:
      ASSIGN
          cRab% = (TT_TransLogg.RabKr / TT_TransLogg.Pris) * 100
          .
  END.
  ELSE cRab% = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON START-SEARCH OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  DEF VAR hSearchCol  AS WIDGET NO-UNDO.
  IF SELF:FOCUSED-ROW = ? THEN DO:
      APPLY "END-SEARCH" TO SELF.
      RETURN.
  END.
  ASSIGN hSearchCol = SELF:CURRENT-COLUMN.
    IF CAN-DO("Dato,ArtikkelNr,VgLopnr,LevNr,SasBeskr,Avvik%",hSearchCol:NAME) THEN NYSORT: DO:
        CASE hSearchCol:NAME:
            WHEN "Dato" THEN DO:
                IF hSortCol = hSearchCol THEN
                    ASSIGN cSortString = 
                     STRING(LOOKUP("DESCENDING",hQuery:PREPARE-STRING," ") <> 0,
                            "BY DATO/BY Dato DESCENDING").
                ELSE
                    ASSIGN cSortString = "BY Dato".
            END.
            WHEN "Levnr" THEN DO:
                IF hSortCol = hSearchCol THEN
                    ASSIGN cSortString = 
                     STRING(LOOKUP("DESCENDING",hQuery:PREPARE-STRING," ") <> 0,
                            "BY Levnr BY Dato/BY Levnr DESCENDING BY Dato").
                ELSE
                    ASSIGN cSortString = "BY Levnr BY Dato".
            END.
/*             WHEN "Butik" THEN DO:                        */
/*                 IF hSortCol = hSearchCol THEN            */
/*                     LEAVE NYSORT.                        */
/*                 ASSIGN cSortString = "BY Butik BY Dato". */
/*             END.                                         */
            WHEN "ArtikkelNr" THEN DO:
                IF hSortCol = hSearchCol THEN
                    LEAVE NYSORT.
                ASSIGN cSortString = "BY ArtikkelNr BY Dato".
            END.
            WHEN "VgLopnr" THEN DO:
                IF hSortCol = hSearchCol THEN
                    LEAVE NYSORT.
                ASSIGN cSortString = "BY Vg BY Lopnr BY Dato".
            END.
            WHEN "SasBeskr" THEN DO:
                IF hSortCol = hSearchCol THEN
                    LEAVE NYSORT.
                ASSIGN cSortString = "BY SasBeskr BY Dato".
            END.
            WHEN "Avvik%" THEN DO:
                ASSIGN cSortString = 
                STRING(LOOKUP("DESCENDING",hQuery:PREPARE-STRING," ") <> 0,
                       "BY Avvik% BY VgLopNr/BY Avvik% DESCENDING BY VgLopNr").
            END.
        END CASE.
        ASSIGN hSortCol:LABEL-BGCOLOR = ?
               hSearchCol:LABEL-BGCOLOR = {&SortBgCol}
               hSortCol = hSearchCol.
        RUN QPrepare.
        hQuery:QUERY-OPEN().
    END.
    APPLY "END-SEARCH" TO SELF.
/*     BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW(). */
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Ok */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-FraDat
DO:

  def var wTittel as char no-undo.
  assign FI-FraDat = date(FI-FraDat:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraDat
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
or F10 of FI-TilDat
DO:

  def var wTittel as char no-undo.
  assign FI-TilDat = date(FI-TilDat:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilDat
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butiker C-Win
ON VALUE-CHANGED OF CB-Butiker IN FRAME DEFAULT-FRAME /* Butikk */
DO:
/*   run QPrepare.      */
/* hQuery:QUERY-OPEN(). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Kasserer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Kasserer C-Win
ON VALUE-CHANGED OF CB-Kasserer IN FRAME DEFAULT-FRAME /* Selger */
DO:
/*   run QPrepare.      */
/* hQuery:QUERY-OPEN(). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Sasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Sasong C-Win
ON VALUE-CHANGED OF CB-Sasong IN FRAME DEFAULT-FRAME /* Sesong */
DO:
/*   run QPrepare.      */
/* hQuery:QUERY-OPEN(). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Varegruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Varegruppe C-Win
ON VALUE-CHANGED OF CB-Varegruppe IN FRAME DEFAULT-FRAME /* Varegruppe */
DO:
/* run QPrepare.        */
/* hQuery:QUERY-OPEN(). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraDat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraDat C-Win
ON TAB OF FI-FraDat IN FRAME DEFAULT-FRAME /* Fra */
OR "RETURN" OF FI-FraDat OR "LEAVE" OF FI-FraDat
DO:
    IF INPUT FI-FraDat > TODAY THEN DO:
        MESSAGE "Feil dato (> idag)"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN FI-FraDat:SCREEN-VALUE = STRING(FI-FraDat).
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN INPUT FI-FraDat.
        IF FI-FraDat > FI-TilDat THEN
            ASSIGN FI-TilDat:SCREEN-VALUE = STRING(INPUT FI-FraDat)
                   FI-TilDat.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr C-Win
ON LEAVE OF FI-LevNr IN FRAME DEFAULT-FRAME /* Levnr */
OR "TAB" OF FI-LevNr OR "RETURN" OF FI-LevNr DO:
   ASSIGN FI-LevNr.
   FIND LevBas WHERE LevBas.LevNr = FI-LevNr NO-LOCK NO-ERROR.
   ASSIGN FI-LevNamn:SCREEN-VALUE = IF NOT AVAIL LevBas THEN "?"
                                    ELSE LevBas.LevNamn.
/*    run QPrepare.        */
/*    hQuery:QUERY-OPEN(). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilDat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilDat C-Win
ON TAB OF FI-TilDat IN FRAME DEFAULT-FRAME /* Til */
OR "RETURN" OF FI-TilDat OR "LEAVE" OF FI-TilDat
DO:
    DO WITH FRAME {&FRAME-NAME}:
      IF INPUT FI-TilDat < FI-FraDat OR INPUT FI-TilDat > TODAY THEN DO:
          MESSAGE "Feil dato " + IF INPUT FI-TilDat > TODAY THEN "(> idag)" ELSE
              "(< fra dato)"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN FI-TilDat:SCREEN-VALUE = string(FI-TilDat).
          RETURN NO-APPLY.
      END.
      ELSE DO:
          ASSIGN INPUT FI-TilDat.
      END.
    END.
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
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Søkeliste alarmrabatt"
  &PreIClose      = " "
  &PostIClose     = " "
  &PostDisable_ui = " "
}
  

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    ASSIGN FI-FraDat = TODAY
           FI-TilDat = TODAY.

  {syspara.i 5 1 1 iCL INT}
  RUN enable_UI.
  {lng.i}

  {browsesettings.i {&BROWSE-NAME}}
  ASSIGN CB-Butiker:SCREEN-VALUE = ENTRY(1,CB-Butiker:LIST-ITEMS)
         CB-Kasserer:SCREEN-VALUE = ENTRY(1,CB-Kasserer:LIST-ITEMS)
         CB-Sasong:SCREEN-VALUE = ENTRY(1,CB-Sasong:LIST-ITEMS)
         CB-Varegruppe:SCREEN-VALUE = ENTRY(1,CB-Varegruppe:LIST-ITEMS).
  RUN InitHandles.
   hQuery:QUERY-PREPARE(SUBSTITUTE("{&BrowseQ}",cSortString)).
/*   RUN QPrepare. */
  RUN InitLabels.
  RUN InitCB.
  FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK.
  IF AVAIL Butiker THEN
      ASSIGN iProfilNrCL = Butiker.ProfilNr.

  ASSIGN
      C-Win:HIDDEN = FALSE
      .

/*   APPLY "LEAVE" TO FI-LevNr. */
/*   APPLY "ENTRY" TO FI-LevNr. */

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{swn.i}

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
  DEFINE RETURN PARAMETER IsLocked AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkel C-Win 
PROCEDURE Artikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   IF NOT VALID-HANDLE(hvArtkor) THEN DO: */
      FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_TransLogg.ArtikkelNr NO-LOCK.

      /* RUN w-vartkor.w PERSISTENT SET hvArtkor (RECID(ArtBas),"ENDRE"). */
      fLockvindu(TRUE).
      RUN w-vartkor.w (RECID(ArtBas),"ENDRE" + "," + STRING(THIS-PROCEDURE) + ",2").
      fLockvindu(FALSE).

/*   END.                                                                     */
/*   ELSE DO:                                                                 */
/*     FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_TransLogg.ArtikkelNr NO-LOCK. */
/*     IF AVAILABLE ArtBAs THEN                                               */
/*       RUN ByttArtikkel IN hvArtKor (RECID(ArtBas)).                        */
/*     ASSIGN hParent = hvArtkor:CURRENT-WINDOW.                              */
/*     hParent:MOVE-TO-TOP().                                                 */
/*   END.                                                                     */
/*   IF VALID-HANDLE(hvArtkor) THEN                                           */
/*   DO:                                                                      */
/*     RUN SetAktivFlik IN hvArtKor (2).                                      */
/*   END.                                                                     */

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
  DISPLAY CB-TTId FI-FraDat FI-TilDat FI-Avvik RS-AnalysTyp FI-LevNr FI-LevNamn 
          FI-Urvaltxt CB-Varegruppe FI-LopNrFra FI-LopNrTil CB-Sasong CB-Butiker 
          CB-Kasserer FI-Filtertxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CB-TTId FI-FraDat Btn_Done FI-TilDat Btn_Help BUTTON-SokDato FI-Avvik 
         BUTTON-SokDato-2 RS-AnalysTyp B-Utfor FI-Urvaltxt BROWSE-1 
         FI-Filtertxt RECT-47 RECT-48 RECT-53 RECT-54 B-SpinFraUp B-SpinTilUp 
         B-SpinFraDown B-SpinTilDown 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDis C-Win 
PROCEDURE EnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER lSensitive AS LOGICAL    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN B-ArtKort:SENSITIVE     = lSensitive
           B-Excel:SENSITIVE       = lSensitive
           CB-Butiker:SENSITIVE    = lSensitive
           CB-Kasserer:SENSITIVE   = lSensitive
           CB-Sasong:SENSITIVE     = lSensitive
           CB-Varegruppe:SENSITIVE = lSensitive
           FI-LevNr:SENSITIVE      = lSensitive
           FI-LopNrFra:SENSITIVE   = lSensitive 
           FI-LopNrTil:SENSITIVE   = lSensitive
           B-Soklev:SENSITIVE      = lSensitive
           B-Nullstill:SENSITIVE   = lSensitive
           B-Aktiver:SENSITIVE     = lSensitive.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportToExcel C-Win 
PROCEDURE ExportToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:                                 combo
------------------------------------------------------------------------------*/
  DEF VAR cExcEkstent  AS CHAR NO-UNDO.
  DEF VAR ctmpFileName AS CHAR NO-UNDO.
  DEF VAR cButikRader  AS CHAR NO-UNDO.
  DEF VAR iButikRad    AS INTE NO-UNDO.
  DEF VAR iCount1      AS INTE NO-UNDO.
  {syspara.i 1 4 1 cExcEkstent}
  ASSIGN cExcEkstent = if cExcEkstent = "" then "sdv" else cExcEkstent.
  IF VALID-HANDLE(wLibHandle) then
    RUN GetTempFileName IN wLibHandle (INPUT "hgrapp", INPUT cExcEkstent, OUTPUT ctmpFileName).
  OUTPUT STREAM Eksport TO VALUE(ctmpFileName).
DO WITH FRAME {&FRAME-NAME}:
  {sww.i}
  FIND TransType WHERE TransType.TTId = INT(CB-TTId:SCREEN-VALUE) NO-LOCK NO-ERROR.
  EXPORT STREAM Eksport {&WINDOW-NAME}:TITLE.
  EXPORT STREAM Eksport CB-TTId:LABEL + ": " + IF AVAIL TransType THEN TransType.Beskrivelse ELSE "".
  EXPORT STREAM Eksport FI-LevNr:LABEL + ": " + FI-LevNr:SCREEN-VALUE + " " + FI-LevNamn:SCREEN-VALUE.
  EXPORT STREAM Eksport REPLACE(FI-FraDat:LABEL,"&","") + ": " + FI-FraDat:SCREEN-VALUE + "  " + REPLACE(FI-TilDat:LABEL,"&","") + ": " + FI-TilDat:SCREEN-VALUE.
  EXPORT STREAM Eksport "".
  EXPORT STREAM Eksport DELIMITER ";" ENTRY(1,cColLabels)
                                      ENTRY(2,cColLabels)
                                      ENTRY(3,cColLabels)
                                      ENTRY(4,cColLabels)
                                      ENTRY(5,cColLabels)
                                      ENTRY(6,cColLabels)
                                      ENTRY(7,cColLabels)
                                      ENTRY(8,cColLabels)
                                      ENTRY(9,cColLabels)
                                      ENTRY(10,cColLabels)
                                      ENTRY(11,cColLabels)
                                      ENTRY(12,cColLabels)
                                      ENTRY(13,cColLabels)
                                      ENTRY(14,cColLabels)
                                      ENTRY(15,cColLabels)
                                      ENTRY(16,cColLabels)
                                      .
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  BROWSE {&BROWSE-NAME}:SELECT-ROW(1).
  REPEAT:
      EXPORT STREAM Eksport DELIMITER ";"
          {&FIELDS-IN-QUERY-{&BROWSE-NAME}}.
/*           TT_TransLogg.Dato                         */
/*           TT_TransLogg.Butik                        */
/*           TT_TransLogg.KassaNr                      */
/*           TT_TransLogg.ArtikkelNr                   */
/*           TT_TransLogg.Beskr                        */
/*           TT_TransLogg.Levkod                       */
/*           TT_TransLogg.SasBeskr                     */
/*           TT_TransLogg.VgLopnr                      */
/*           TT_TransLogg.Storl                        */
/*           TT_TransLogg.Antall                       */
/*           TT_TransLogg.Pris FORMAT ">>>>>9.99"      */
/*           TT_TransLogg.RabKr FORMAT ">>>>>9.99"     */
/*           TT_TransLogg.Tabell                       */
/*           TT_TransLogg.Profil                       */
/*           TT_TransLogg.ListPris FORMAT ">>>>>9.99". */
      IF NOT BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW() THEN
          LEAVE.
  END.
  {swn.i}
  output stream Eksport close.
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.
  chExcelApplication:Visible = FALSE.
  chWorkbooks = chExcelApplication:Workbooks:OpenText(ctmpFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:M4"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:M4"):Font:Italic = TRUE.
  chWorkSheets:Range("A6:O6"):Font:Bold = TRUE.

  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("K:L"):NumberFormat = "## ##0,00".
  chWorkSheets:Range("O:O"):NumberFormat = "## ##0,00".

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:O1"):Merge().
  chWorkSheets:Range("A2:O2"):Merge().
  chWorkSheets:Range("A3:O3"):Merge().
  chWorkSheets:Range("A4:O4"):Merge().
/*   DO iCount1 = 1 TO NUM-ENTRIES(cButikRader):                                                                                                      */
/*       chWorkSheets:Range("A" + ENTRY(iCount1,cButikRader) + ":O" + ENTRY(iCount1,cButikRader)):Merge().                                            */
/*       chWorkSheets:Range("A" + ENTRY(iCount1,cButikRader) + ":D" + ENTRY(iCount1,cButikRader)):Font:Bold = TRUE.                                   */
/*       chWorkSheets:Range("A" + STRING(INT(ENTRY(iCount1,cButikRader)) + 1) + ":O" + STRING(INT(ENTRY(iCount1,cButikRader)) + 1)):Font:Bold = TRUE. */
/*   END.                                                                                                                                             */
  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:O"):AutoFit().
  chWorksheets:PageSetup:PrintArea      = "A:O".
  chWorkSheets:PageSetup:Orientation    = 2.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("E7"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
END.

  chExcelApplication:Visible = TRUE.

  RELEASE OBJECT chWorksheets       NO-ERROR. /* release com-handles */
  RELEASE OBJECT chWorkbooks        NO-ERROR. /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR. /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.
  STATUS DEFAULT "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-Win 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcListe AS CHAR NO-UNDO.
DEF VAR piLoop  AS INT  NO-UNDO.
DEF VAR pcTekst AS CHAR NO-UNDO.

ASSIGN
    pcListe = "1,3,10"
    pcTekst = ""
    .

DO piLoop = 1 TO NUM-ENTRIES(pcListe):
    FIND TransType NO-LOCK WHERE
        TransType.TTId = INT(ENTRY(piLoop,pcListe)) NO-ERROR.
    IF AVAILABLE TransType THEN
    ASSIGN
        pcTekst = pcTekst + 
                  (IF pcTekst = ""
                     THEN ""
                     ELSE ",") +
                  TransType.Beskrivelse + "," + string(TransType.TTId)
                  .
                  
END.
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        CB-TTId:LIST-ITEM-PAIRS   = pcTekst
        CB-TTId:SCREEN-VALUE = ENTRY(1,pcListe)
        .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitHandles C-Win 
PROCEDURE InitHandles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN hQuery = BROWSE {&BROWSE-NAME}:QUERY.
  
  ASSIGN hSortCol = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
  REPEAT WHILE VALID-HANDLE(hSortCol):
      IF hSortCol:NAME = "{&FirstSort}" THEN DO:
          ASSIGN hSortCol:LABEL-BGCOLOR = {&SortBgCol}.
          LEAVE.
      END.
      ASSIGN hSortCol = hSortCol:NEXT-COLUMN.
  END.
  hSortCol:READ-ONLY = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLabels C-Win 
PROCEDURE InitLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hCol AS HANDLE NO-UNDO.
  ASSIGN hCol = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
  REPEAT WHILE VALID-HANDLE(hCol):
      ASSIGN cColLabels = cColLabels + (IF cColLabels = "" THEN "" ELSE ",") +
          TRIM(REPLACE(hCol:LABEL,"*",""))
             hCol = hCol:NEXT-COLUMN.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillFilter C-Win 
PROCEDURE NullstillFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        CB-Butiker:SCREEN-VALUE    = ENTRY(1,CB-Butiker:LIST-ITEMS)
        CB-Kasserer:SCREEN-VALUE   = ENTRY(1,CB-Kasserer:LIST-ITEMS)
        CB-Sasong:SCREEN-VALUE     = ENTRY(1,CB-Sasong:LIST-ITEMS)
        CB-Varegruppe:SCREEN-VALUE = ENTRY(1,CB-Varegruppe:LIST-ITEMS)
        FI-LevNr:SCREEN-VALUE      = "0"
        FI-LevNamn:SCREEN-VALUE    = ""
        FI-LopNrFra:SCREEN-VALUE   = "0"
        FI-LopNrTil:SCREEN-VALUE   = "0".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        CASE cRettning:
            WHEN "Prev" THEN
                BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
            WHEN "Next" THEN
                BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
        END CASE.
        PUBLISH "ByttArtikkel" (TT_TransLogg.ArtikkelNr).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QPrepare C-Win 
PROCEDURE QPrepare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQSort AS CHARACTER NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN FI-LevNr
           FI-LopNrFra
           FI-LopNrTil.
    IF FI-LevNr <> 0 THEN
        cWhere = " WHERE TT_TransLogg.LevNr = " + STRING(FI-LevNr) + " ".
    IF CB-Butiker:SCREEN-VALUE <> ENTRY(1,CB-Butiker:LIST-ITEMS) THEN
        ASSIGN cWhere = (IF cWhere <> "" THEN cWhere + "AND" ELSE " WHERE") +
        " TT_Translogg.Butik = " + STRING(ENTRY(1,TRIM(CB-Butiker:SCREEN-VALUE)," ")) + " ".
    IF CB-Varegruppe:SCREEN-VALUE <> ENTRY(1,CB-Varegruppe:LIST-ITEMS) THEN
        ASSIGN cWhere = (IF cWhere <> "" THEN cWhere + "AND" ELSE " WHERE") +
        " TT_Translogg.Vg = " + STRING(ENTRY(1,TRIM(CB-Varegruppe:SCREEN-VALUE)," ")) + " ".
    IF FI-LopNrFra > 0 OR FI-LopNrTil > 0 THEN DO:
        IF FI-LopNrTil = 0 THEN
            ASSIGN cWhere = (IF cWhere <> "" THEN cWhere + "AND" ELSE " WHERE") +
                   " TT_Translogg.LopNr >= " + FI-LopNrFra:SCREEN-VALUE + " ".
        ELSE 
            ASSIGN cWhere = (IF cWhere <> "" THEN cWhere + "AND" ELSE " WHERE") +
                   " TT_Translogg.LopNr >= " + FI-LopNrFra:SCREEN-VALUE + " AND" +
                   " TT_Translogg.LopNr <= " + FI-LopNrTil:SCREEN-VALUE + " ".
    END.
    IF CB-Sasong:SCREEN-VALUE <> ENTRY(1,CB-Sasong:LIST-ITEMS) THEN
        ASSIGN cWhere = (IF cWhere <> "" THEN cWhere + "AND" ELSE " WHERE") +
              " TT_Translogg.Sasong = " + STRING(ENTRY(1,TRIM(CB-Sasong:SCREEN-VALUE)," ")) + " ".
    IF CB-Kasserer:SCREEN-VALUE <> ENTRY(1,CB-Kasserer:LIST-ITEMS) THEN
        ASSIGN cWhere = (IF cWhere <> "" THEN cWhere + "AND" ELSE " WHERE") +
              " TT_Translogg.ForsNr = " + STRING(ENTRY(1,TRIM(CB-Kasserer:SCREEN-VALUE)," ")) + " ".
     ASSIGN cQSort = (IF cWhere = "" THEN "" ELSE cWhere) + cSortString.
  END.
/*   MESSAGE cQSort                         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    hQuery:QUERY-PREPARE(SUBSTITUTE("{&BrowseQ}",cQSort)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utfor C-Win 
PROCEDURE Utfor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR iBruktProfilNr AS INTE  INIT ? NO-UNDO.
  DEF VAR iButikProfilNr AS INTE  INIT ? NO-UNDO.
  DEF VAR rHPrisKo       AS ROWID NO-UNDO.
  DEF VAR dPrisKoPris    AS DECI  NO-UNDO.
  DEF VAR cTabell        AS CHAR  NO-UNDO.
  DEF VAR cProfilStr     AS CHAR  NO-UNDO.
  DEF VAR iCount         AS INTE  NO-UNDO.
  DEF VAR dLoop          AS date  NO-UNDO.
  DEF VAR cCBVg          AS CHAR  INIT "         [Alle]" NO-UNDO.
  DEF VAR cCBSasong      AS CHAR  INIT "         [Alle]" NO-UNDO.
  DEF VAR cCBButiker     AS CHAR  INIT "         [Alle]" NO-UNDO.
  DEF VAR cCBKasserer    AS CHAR  INIT "         [Alle]" NO-UNDO.
  
  DO WITH FRAME Default-Frame:
    /*
    IF FI-LevNamn:SCREEN-VALUE = "?" THEN
        RETURN NO-APPLY.
    */
    {sww.i}
    ASSIGN cCBVg       = FILL(" ",9) + TRIM(ENTRY(1,CB-Varegruppe:LIST-ITEMS))
           cCBSasong   = FILL(" ",9) + TRIM(ENTRY(1,CB-Sasong:LIST-ITEMS))
           cCBButiker  = FILL(" ",9) + TRIM(ENTRY(1,CB-Butiker:LIST-ITEMS))
           cCBKasserer = FILL(" ",9) + TRIM(ENTRY(1,CB-Kasserer:LIST-ITEMS))
           RS-AnalysTyp
           FI-Avvik
           FI-FraDat
           FI-TilDat
           CB-TTId
           B-Excel:SENSITIVE = NO
        .

    EMPTY TEMP-TABLE TT_TransLogg.

    BUTIKKLOOP:
    FOR EACH bufButiker NO-LOCK:
    DATOLOOP:
    DO dLoop = FI-FraDat TO FI-TilDat:
      FOR EACH TransLogg WHERE 
               TransLogg.Dato = dLoop AND
               TransLogg.TTId = CB-TTId AND
               TransLogg.Butik = bufButiker.Butik
               :
        /* Kontroll på leverandør. */
/*         IF FI-LevNr <> 0 THEN                   */
/*         DO:                                     */
/*             IF TransLogg.LevNr <> FI-LevNr THEN */
/*                 NEXT DATOLOOP.                  */
/*         END.                                    */
   
        ASSIGN dPrisKoPris = ?.
   
        /* Rabattanalyse og ingen rabatt er gitt. */
        IF RS-AnalysTyp = 1 AND TransLogg.RabKr = 0 THEN
          NEXT.
        /* Rabattanalyse og rabatt innenfor grensen */
        ELSE IF RS-AnalysTyp = 1 AND NOT TransLogg.RabKr >= TransLogg.Pris * FI-Avvik / 100 THEN
          NEXT.
        ELSE IF RS-AnalysTyp = 2 AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr AND
                                              ArtBas.OPris = TRUE) THEN
            NEXT.
        /* Prisavviksanalyse */
        /*ELSE IF RS-AnalysTyp = 2 THEN */ DO:
/*           FIND Butiker WHERE Butiker.Butik = TransLogg.Butik NO-LOCK. */
/*           IF NOT AVAIL Butiker THEN                                   */
/*             NEXT.                                                     */
/*           ELSE                                                        */
          ASSIGN cProfilStr     = getProfilStr(bufButiker.ProfilNr,iProfilNrCL)
                 iButikProfilNr = bufButiker.ProfilNr
                 rHPrisKo = ?
                 cTabell = ""
                 iBruktProfilNr = ?.
          FINN_HPRISKO:
          DO iCount = 1 TO NUM-ENTRIES(cProfilStr):
            FOR EACH HPrisKo WHERE
                HPrisKo.ArtikkelNr = TransLogg.ArtikkelNr AND
                HPrisKo.ProfilNr   = INT(ENTRY(iCount,cProfilStr)) AND
                HPrisKo.AktiveresDato <= TransLogg.Dato AND
                HPrisKo.GyldigTilDato >= TransLogg.Dato:
              IF HPrisKo.AktiveresDato = TransLogg.Dato AND
                   HPrisKo.AktiveresTid  > TransLogg.Tid THEN
                NEXT.
              IF HPrisKo.GyldigTilDato = TransLogg.Dato AND
                 HPrisKo.GyldigTilTid  < TransLogg.Tid THEN
                NEXT.
              ASSIGN rHPrisKo = ROWID(HPrisKo)
                     iBruktProfilNr = INT(ENTRY(iCount,cProfilStr)).
              LEAVE FINN_HPRISKO.
            END.
            IF rHPrisKo = ? THEN DO:
              FIND FIRST HPrisKo WHERE
                  HPrisKo.ArtikkelNr = TransLogg.ArtikkelNr AND
                  HPrisKo.ProfilNr   = INT(ENTRY(iCount,cProfilStr)) AND
                  HPrisKo.AktiveresDato <= TransLogg.Dato NO-LOCK NO-ERROR.
              IF AVAIL HPrisKo THEN DO:
                ASSIGN rHPrisKo = ROWID(HPrisKo)
                       iBruktProfilNr = HPrisKo.ProfilNr.
                LEAVE FINN_HPRISKO.
              END.
            END.
          END.
          IF rHPrisKo <> ? THEN DO:
            FIND HPrisKo WHERE ROWID(HPrisKo) = rHPrisKo NO-LOCK.
            ASSIGN dPrisKoPris = HPrisKo.Pris
                   cTabell     = "HPrisko".
          END.
          ELSE 
          FINN_ARTPRIS: 
          DO iCount = 1 TO NUM-ENTRIES(cProfilStr):
            FIND ArtPris WHERE ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND
                               ArtPris.ProfilNr = INT(ENTRY(iCount,cProfilStr)) NO-LOCK NO-ERROR.
            IF AVAIL ArtPris THEN DO:
              ASSIGN dPrisKoPris = IF NOT ArtPris.Tilbud THEN ArtPris.Pris[1]
                                   ELSE ArtPris.Pris[2]
                     iBruktProfilNr = ArtPris.Profilnr
                     cTabell        = "Artpris".
              LEAVE FINN_ARTPRIS.
            END.
          END. /* FINN_ARTPRIS */
   
          /* Prissavvik er innenfor grensen */
          IF RS-AnalysTyp = 2 THEN
          DO:
            IF NOT ABS(Translogg.Pris - dPrisKoPris) > FI-Avvik * dPrisKoPris / 100 THEN
              NEXT.
          END.
        END.
   
        /* Sesongkode */
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN
            FIND SaSong WHERE SaSong.Sasong = ArtBas.Sasong NO-LOCK NO-ERROR.
        ELSE
            RELEASE Sasong.
        ASSIGN cCBVg      = cCBVg + IF NOT CAN-DO(cCBVg,STRING(TransLogg.Vg)) THEN "," + STRING(TransLogg.Vg) ELSE ""
               cCBSasong  = cCBSasong + IF AVAIL Sasong AND NOT CAN-DO(cCBSasong,STRING(Sasong.Sasong)) THEN
                                    "," + STRING(Sasong.Sasong) ELSE ""
               cCBButiker = cCBButiker + IF NOT CAN-DO(cCBButiker,STRING(TransLogg.Butik)) THEN "," + STRING(TransLogg.Butik) ELSE ""
               cCBKasserer = cCBKasserer + IF NOT CAN-DO(cCBKasserer,STRING(TransLogg.ForsNr)) THEN "," + STRING(TransLogg.ForsNr) ELSE ""
                   .
        CREATE TT_TransLogg.
        BUFFER-COPY TransLogg TO TT_TransLogg.
        ASSIGN TT_TransLogg.Beskr    = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "Ukjent"
               TT_TransLogg.Levkod   = IF AVAIL ArtBas THEN ArtBas.Levkod ELSE ? 
               TT_TransLogg.Sasong   = IF AVAIL SaSong THEN SaSong.Sasong ELSE ?
               TT_TransLogg.SasBeskr = IF AVAIL SaSong THEN SaSong.SasBeskr ELSE "Ukjent"
               TT_TransLogg.ListPris = dPrisKoPris
               TT_TransLogg.Profil   = STRING(iButikProfilNr) + " / " + STRING(iBruktProfilNr)
               TT_TransLogg.Tabell   = cTabell
               TT_TransLogg.VgLopNr  = STRING(TransLogg.Vg) + " / " + STRING(TransLogg.LopNr)
               /*
               TT_TransLogg.VgLopNr  = FILL(" ", 8 - (2 * LENGTH(STRING(TransLogg.Vg)))) + STRING(TransLogg.Vg) + " / " + 
                                       FILL(" ", 8 - (2 * LENGTH(STRING(TransLogg.Lopnr)))) + STRING(TransLogg.LopNr)
               */
               TT_TransLogg.Avvik%   = IF INPUT RS-AnalysTyp = 1
                                         THEN ((TT_TransLogg.RabKr * 100) / TT_TransLogg.Pris)
                                         ELSE ((TT_TransLogg.Pris * 100) / TT_TransLogg.ListPris)
               .
      RELEASE TT_TransLogg.
      END.
    END. /* DATOLOOP */
    END. /* BUTIKKLOOP */
    DO iCount = 2 TO NUM-ENTRIES(cCBVg):
        FIND VarGr WHERE VarGr.Vg = INT(ENTRY(iCount,cCBVg)) NO-LOCK NO-ERROR.
            ASSIGN ENTRY(iCount,cCBVg) = FILL(" ",8 - (2 * LENGTH(ENTRY(iCount,cCBVg)))) + STRING(INT(ENTRY(iCount,cCBVg))) +
                " " + IF AVAIL VarGr THEN REPLACE(VarGr.VgBeskr,","," ") ELSE "?".
    END.
    DO iCount = 2 TO NUM-ENTRIES(cCBSasong):
        FIND Sasong WHERE Sasong.Sasong = INT(ENTRY(iCount,cCBSasong)) NO-LOCK NO-ERROR.
            ASSIGN ENTRY(iCount,cCBSasong) = FILL(" ",8 - (2 * LENGTH(ENTRY(iCount,cCBSasong)))) + STRING(INT(ENTRY(iCount,cCBSasong))) +
                " " + IF AVAIL Sasong THEN REPLACE(Sasong.SasBeskr,","," ") ELSE "?".
    END.
    DO iCount = 2 TO NUM-ENTRIES(cCBButiker):
        FIND Butiker WHERE Butiker.Butik = INT(ENTRY(iCount,cCBButiker)) NO-LOCK NO-ERROR.
            ASSIGN ENTRY(iCount,cCBButiker) = FILL(" ",8 - (2 * LENGTH(ENTRY(iCount,cCBButiker)))) + STRING(INT(ENTRY(iCount,cCBButiker))) +
                " " + IF AVAIL Butiker THEN REPLACE(Butiker.ButNamn,","," ") ELSE "?".
    END.
    DO iCount = 2 TO NUM-ENTRIES(cCBKasserer):
        FIND Forsalj WHERE Forsalj.ForsNr = INT(ENTRY(iCount,cCBKasserer)) NO-LOCK NO-ERROR.
            ASSIGN ENTRY(iCount,cCBKasserer) = FILL(" ",8 - (2 * LENGTH(ENTRY(iCount,cCBKasserer)))) + STRING(INT(ENTRY(iCount,cCBKasserer))) +
                " " + IF AVAIL Forsalj THEN REPLACE(Forsalj.FoNamn,","," ") ELSE "?".
    END.
    ASSIGN CB-Varegruppe:LIST-ITEMS   = cCBVg
           CB-Varegruppe:SCREEN-VALUE = ENTRY(1,CB-Varegruppe:LIST-ITEMS)
           CB-Sasong:LIST-ITEMS       = cCBSasong
           CB-Sasong:SCREEN-VALUE     = ENTRY(1,CB-Sasong:LIST-ITEMS)
           CB-Butiker:LIST-ITEMS      = cCBButiker
           CB-Butiker:SCREEN-VALUE    = ENTRY(1,CB-Butiker:LIST-ITEMS)
           CB-Kasserer:LIST-ITEMS     = cCBKasserer
           CB-Kasserer:SCREEN-VALUE   = ENTRY(1,CB-Kasserer:LIST-ITEMS)
           FI-LopNrFra:SCREEN-VALUE   = "0"
           FI-LopNrTil:SCREEN-VALUE   = "0"
           .
    RUN QPrepare.
    hQuery:QUERY-OPEN().
    RUN EnaDis (NOT BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ?).
    {swn.i}
  END.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utfor-Old C-Win 
PROCEDURE Utfor-Old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR iBruktProfilNr AS INTE  INIT ? NO-UNDO.
  DEF VAR iProfilNrCL    AS INTE  INIT ? NO-UNDO.
  DEF VAR iButikProfilNr AS INTE  INIT ? NO-UNDO.
  DEF VAR rHPrisKo       AS ROWID NO-UNDO.
  DEF VAR dPrisKoPris    AS DECI  NO-UNDO.
  DEF VAR cTabell        AS CHAR  NO-UNDO.
  DEF VAR cProfilStr     AS CHAR  NO-UNDO.
  DEF VAR iCount         AS INTE  NO-UNDO.
  DEF VAR dLoop          AS date  NO-UNDO.

  DO WITH FRAME Default-Frame:
    /*
    IF FI-LevNamn:SCREEN-VALUE = "?" THEN
        RETURN NO-APPLY.
    */
    {sww.i}
    ASSIGN RS-AnalysTyp
           FI-Avvik
           FI-FraDat
           FI-TilDat
           CB-TTId
           B-Excel:SENSITIVE = NO
        .

    FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK.
    IF AVAIL Butiker THEN
        ASSIGN iProfilNrCL = Butiker.ProfilNr.
    EMPTY TEMP-TABLE TT_TransLogg.

    BUTIKKLOOP:
    FOR EACH bufButiker NO-LOCK:
    DATOLOOP:
    DO dLoop = FI-FraDat TO FI-TilDat:
      FOR EACH TransLogg WHERE 
               TransLogg.Dato = dLoop AND
               TransLogg.TTId = CB-TTId AND
               TransLogg.Butik = bufButiker.Butik
               :
        /* Kontroll på leverandør. */
        IF FI-LevNr <> 0 THEN
        DO:
            IF TransLogg.LevNr <> FI-LevNr THEN
                NEXT DATOLOOP.
        END.
   
        ASSIGN dPrisKoPris = ?.
   
        /* Rabattanalyse og ingen rabatt er gitt. */
        IF RS-AnalysTyp = 1 AND TransLogg.RabKr = 0 THEN
          NEXT.
        /* Rabattanalyse og rabatt innenfor grensen */
        ELSE IF RS-AnalysTyp = 1 AND NOT TransLogg.RabKr >= TransLogg.Pris * FI-Avvik / 100 THEN
          NEXT.
   
        /* Prisavviksanalyse */
        /*ELSE IF RS-AnalysTyp = 2 THEN */ DO:
          FIND Butiker WHERE Butiker.Butik = TransLogg.Butik NO-LOCK.
          IF NOT AVAIL Butiker THEN
            NEXT.
          ELSE
            ASSIGN cProfilStr     = getProfilStr(Butiker.ProfilNr,iProfilNrCL)
                   iButikProfilNr = Butiker.ProfilNr.
          ASSIGN rHPrisKo = ?
                 cTabell = ""
                 iBruktProfilNr = ?.
          FINN_HPRISKO:
          DO iCount = 1 TO NUM-ENTRIES(cProfilStr):
            FOR EACH HPrisKo WHERE
                HPrisKo.ArtikkelNr = TransLogg.ArtikkelNr AND
                HPrisKo.ProfilNr   = INT(ENTRY(iCount,cProfilStr)) AND
                HPrisKo.AktiveresDato <= TransLogg.Dato AND
                HPrisKo.GyldigTilDato >= TransLogg.Dato:
              IF HPrisKo.AktiveresDato = TransLogg.Dato AND
                   HPrisKo.AktiveresTid  > TransLogg.Tid THEN
                NEXT.
              IF HPrisKo.GyldigTilDato = TransLogg.Dato AND
                 HPrisKo.GyldigTilTid  < TransLogg.Tid THEN
                NEXT.
              ASSIGN rHPrisKo = ROWID(HPrisKo)
                     iBruktProfilNr = INT(ENTRY(iCount,cProfilStr)).
              LEAVE FINN_HPRISKO.
            END.
            IF rHPrisKo = ? THEN DO:
              FIND FIRST HPrisKo WHERE
                  HPrisKo.ArtikkelNr = TransLogg.ArtikkelNr AND
                  HPrisKo.ProfilNr   = INT(ENTRY(iCount,cProfilStr)) AND
                  HPrisKo.AktiveresDato <= TransLogg.Dato NO-LOCK NO-ERROR.
              IF AVAIL HPrisKo THEN DO:
                ASSIGN rHPrisKo = ROWID(HPrisKo)
                       iBruktProfilNr = HPrisKo.ProfilNr.
                LEAVE FINN_HPRISKO.
              END.
            END.
          END.
          IF rHPrisKo <> ? THEN DO:
            FIND HPrisKo WHERE ROWID(HPrisKo) = rHPrisKo NO-LOCK.
            ASSIGN dPrisKoPris = HPrisKo.Pris
                   cTabell     = "HPrisko".
          END.
          ELSE 
          FINN_ARTPRIS: 
          DO iCount = 1 TO NUM-ENTRIES(cProfilStr):
            FIND ArtPris WHERE ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND
                               ArtPris.ProfilNr = INT(ENTRY(iCount,cProfilStr)) NO-LOCK NO-ERROR.
            IF AVAIL ArtPris THEN DO:
              ASSIGN dPrisKoPris = IF NOT ArtPris.Tilbud THEN ArtPris.Pris[1]
                                   ELSE ArtPris.Pris[2]
                     iBruktProfilNr = ArtPris.Profilnr
                     cTabell        = "Artpris".
              LEAVE FINN_ARTPRIS.
            END.
          END. /* FINN_ARTPRIS */
   
          /* Prissavvik er innenfor grensen */
          IF RS-AnalysTyp = 2 THEN
          DO:
            IF NOT ABS(Translogg.Pris - dPrisKoPris) > FI-Avvik * dPrisKoPris / 100 THEN
              NEXT.
          END.
        END.
   
        /* Sesongkode */
        FIND ArtBas WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN
            FIND SaSong WHERE SaSong.Sasong = ArtBas.Sasong NO-LOCK NO-ERROR.
   
        CREATE TT_TransLogg.
        BUFFER-COPY TransLogg TO TT_TransLogg.
        ASSIGN TT_TransLogg.Beskr    = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "Ukjent"
               TT_TransLogg.Levkod   = IF AVAIL ArtBas THEN ArtBas.Levkod ELSE ? 
               TT_TransLogg.SasBeskr = IF AVAIL SaSong THEN SaSong.SasBeskr ELSE "Ukjent"
               TT_TransLogg.ListPris = dPrisKoPris
               TT_TransLogg.Profil   = STRING(iButikProfilNr) + " / " + STRING(iBruktProfilNr)
               TT_TransLogg.Tabell   = cTabell
               TT_TransLogg.VgLopNr  = STRING(TransLogg.Vg) + " / " + STRING(TransLogg.LopNr)
               TT_TransLogg.Avvik%   = IF INPUT RS-AnalysTyp = 1
                                         THEN ((TT_TransLogg.RabKr * 100) / TT_TransLogg.Pris)
                                         ELSE ((TT_TransLogg.Pris * 100) / TT_TransLogg.ListPris)
               .
      RELEASE TT_TransLogg.
      END.
    END. /* DATOLOOP */
    END. /* BUTIKKLOOP */
    hQuery:QUERY-OPEN().
    ASSIGN B-Excel:SENSITIVE = NOT BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ?.
    {swn.i}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu C-Win 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProfilStr C-Win 
FUNCTION getProfilStr RETURNS CHARACTER
  ( INPUT iButikProfil AS INTEGER, INPUT iCLProfil AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cInitProfilStr   AS CHAR NO-UNDO.
  DEF VAR cReturnProfilStr AS CHAR NO-UNDO.
  ASSIGN cInitProfilStr = STRING(iButikProfil) + IF iButikProfil <> iCLProfil THEN
                "," + STRING(iCLProfil) ELSE ""
         cReturnProfilStr = cInitProfilStr.
  FOR EACH Prisprofil WHERE NOT CAN-DO(cInitProfilStr,STRING(Prisprofil.ProfilNr)) NO-LOCK:
      ASSIGN cReturnProfilStr = cReturnProfilStr + "," + STRING(Prisprofil.ProfilNr).
  END.
  RETURN cReturnProfilStr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

