&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ArtBas NO-UNDO LIKE ArtBas
       FIELD db% LIKE ArtPris.db%
       FIELD Tilbud LIKE ArtPris.Tilbud
       FIELD DBKr LIKE ArtPris.DBKr
       FIELD Pris LIKE ArtPris.Pris
       FIELD Varekost LIKE ArtPris.Varekost.


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

DEF VAR      wAlle     AS CHAR no-undo.
DEF VAR      cComboStr AS CHAR NO-UNDO.
DEF VAR      iAntTraff AS INTE NO-UNDO.
DEF VAR      cEmpty    AS CHAR LABEL "" NO-UNDO.
DEF VAR      dDB%1      AS DECI NO-UNDO.
DEF VAR      dDB%2      AS DECI NO-UNDO.
DEF VAR      cColLabels  AS CHAR         NO-UNDO.
DEF VAR lIsLocked  AS INTEGER NO-UNDO INITIAL 0.

/* Alle av mulige valg */
def var iAntVarGr     AS INTE NO-UNDO.
def var iAntLevBas    AS INTE NO-UNDO.
def var iAntSaSong    AS INTE NO-UNDO.
def var iAntVaremerke AS INTE NO-UNDO.

/* Valgte valg. */
def var cValgteVarGr     as char INIT "*" no-undo.
def var cValgteLevBas    as char INIT "*" no-undo.
def var cValgteSaSong    as char INIT "*" no-undo.
def var cValgteVaremerke as char INIT "*" no-undo.

DEF VAR hvArtkor         AS HANDLE NO-UNDO.

DEF STREAM Eksport.

{runlib.i}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_ArtBas

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 TT_ArtBas.ArtikkelNr ~
TT_ArtBas.Beskr TT_ArtBas.Vg TT_ArtBas.LopNr TT_ArtBas.LevKod ~
TT_ArtBas.DB%[1] TT_ArtBas.DBKr[1] TT_ArtBas.Pris[1] TT_ArtBas.VareKost[1] ~
TT_ArtBas.DB%[2] TT_ArtBas.DBKr[2] TT_ArtBas.Pris[2] TT_ArtBas.VareKost[2] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4 
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH TT_ArtBas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY BROWSE-4 FOR EACH TT_ArtBas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 TT_ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 TT_ArtBas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Help BROWSE-4 T-AlleVarGr T-AlleLevBas ~
T-AlleVaremerke T-AlleSaSong BUTTON-Ok B-VarGr B-LevBas B-Varemerke ~
B-SaSong T-Godkjente RS-Type CB-PP FI-DB% FI-Avvik B-Utfor FILL-IN-3 ~
FILL-IN-1 FILL-IN-2 RECT-1 RECT-2 RECT-27 RECT-28 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS T-AlleVarGr T-AlleLevBas T-AlleVaremerke ~
T-AlleSaSong FI-VarGr FI-LevBas FI-Varemerke FI-SaSong T-Godkjente RS-Type ~
CB-PP FI-DB% FI-Avvik FILL-IN-3 FILL-IN-1 FILL-IN-2 

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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-ArtKort 
     LABEL "Artikkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Excel 
     LABEL "Excel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-LevBas 
     LABEL "&Leverandører..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-SaSong 
     LABEL "&Sesong..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Utfor 
     LABEL "&Utfør" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Varemerke 
     LABEL "V&aremerker..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-VarGr 
     LABEL "&Varegrupper..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE VARIABLE CB-PP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prisprofil" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avvik AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Avvik +/- %" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DB% AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "DB%" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevBas AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SaSong AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Varemerke AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL " Godkjent DB%" 
      VIEW-AS TEXT 
     SIZE 21 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL " Priskontroll mot" 
      VIEW-AS TEXT 
     SIZE 21 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL " Filter" 
      VIEW-AS TEXT 
     SIZE 10 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE RS-Type AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ordinær", 1,
"Tilbud", 2,
"Aktiv", 3
     SIZE 48.8 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 3.33.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 1.67.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.2 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.4 BY .1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 5.48.

DEFINE VARIABLE T-AlleLevBas AS LOGICAL INITIAL yes 
     LABEL "Toggle 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-AlleSaSong AS LOGICAL INITIAL yes 
     LABEL "Toggle 4" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-AlleVaremerke AS LOGICAL INITIAL yes 
     LABEL "Toggle 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-AlleVarGr AS LOGICAL INITIAL yes 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE T-Godkjente AS LOGICAL INITIAL no 
     LABEL "Vis godkjente" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      TT_ArtBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _STRUCTURED
  QUERY BROWSE-4 NO-LOCK DISPLAY
      TT_ArtBas.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      TT_ArtBas.Beskr FORMAT "x(20)":U
      TT_ArtBas.Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U
      TT_ArtBas.LopNr FORMAT "zzz9":U
      TT_ArtBas.LevKod FORMAT "x(20)":U
      TT_ArtBas.DB%[1] COLUMN-LABEL "Ord. DB%" WIDTH 9.8
      TT_ArtBas.DBKr[1] COLUMN-LABEL "DBKr"
      TT_ArtBas.Pris[1] COLUMN-LABEL "Pris"
      TT_ArtBas.VareKost[1] COLUMN-LABEL "Varekost"
      TT_ArtBas.DB%[2] COLUMN-LABEL "Tilb. DB%" WIDTH 10.4
      TT_ArtBas.DBKr[2] COLUMN-LABEL "DBKr"
      TT_ArtBas.Pris[2] COLUMN-LABEL "Pris"
      TT_ArtBas.VareKost[2] COLUMN-LABEL "Varekost"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 158 BY 17.14 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Help AT ROW 1.33 COL 150.4 NO-TAB-STOP 
     BROWSE-4 AT ROW 10.1 COL 2
     T-AlleVarGr AT ROW 4.76 COL 115.8
     T-AlleLevBas AT ROW 5.91 COL 115.8
     T-AlleVaremerke AT ROW 7.19 COL 115.6
     T-AlleSaSong AT ROW 8.33 COL 115.6
     FI-VarGr AT ROW 4.62 COL 94.2 COLON-ALIGNED NO-LABEL
     BUTTON-Ok AT ROW 1.33 COL 155.4 NO-TAB-STOP 
     FI-LevBas AT ROW 5.81 COL 94.2 COLON-ALIGNED NO-LABEL
     FI-Varemerke AT ROW 7.1 COL 94.2 COLON-ALIGNED NO-LABEL
     FI-SaSong AT ROW 8.29 COL 94.2 COLON-ALIGNED NO-LABEL
     B-VarGr AT ROW 4.52 COL 71.2
     B-LevBas AT ROW 5.76 COL 71.2
     B-Varemerke AT ROW 7 COL 71.2
     B-SaSong AT ROW 8.24 COL 71.2
     B-Excel AT ROW 1.24 COL 36
     T-Godkjente AT ROW 5.24 COL 31
     RS-Type AT ROW 8.43 COL 3.2 NO-LABEL
     CB-PP AT ROW 2.67 COL 13 COLON-ALIGNED
     B-ArtKort AT ROW 1.24 COL 19
     FI-DB% AT ROW 5.1 COL 14.2 COLON-ALIGNED HELP
          "DB%"
     FI-Avvik AT ROW 6.29 COL 14.4 COLON-ALIGNED HELP
          "DB%"
     B-Utfor AT ROW 1.24 COL 2
     FILL-IN-3 AT ROW 3.81 COL 69 COLON-ALIGNED NO-LABEL
     FILL-IN-1 AT ROW 3.95 COL 3 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 7.76 COL 3 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 4.19 COL 2
     RECT-2 AT ROW 8 COL 2
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
     RECT-3 AT ROW 4.19 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_ArtBas T "?" NO-UNDO skotex ArtBas
      ADDITIONAL-FIELDS:
          FIELD db% LIKE ArtPris.db%
          FIELD Tilbud LIKE ArtPris.Tilbud
          FIELD DBKr LIKE ArtPris.DBKr
          FIELD Pris LIKE ArtPris.Pris
          FIELD Varekost LIKE ArtPris.Varekost
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kontroll kalkyle"
         HEIGHT             = 26.57
         WIDTH              = 159.8
         MAX-HEIGHT         = 26.57
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 26.57
         VIRTUAL-WIDTH      = 159.8
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Size-to-Fit R-To-L,COLUMNS                                           */
/* BROWSE-TAB BROWSE-4 Btn_Help DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON B-ArtKort IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Excel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevBas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SaSong IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Varemerke IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _TblList          = "Temp-Tables.TT_ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_ArtBas.ArtikkelNr
     _FldNameList[2]   = Temp-Tables.TT_ArtBas.Beskr
     _FldNameList[3]   > Temp-Tables.TT_ArtBas.Vg
"Vg" "Vg" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.TT_ArtBas.LopNr
     _FldNameList[5]   = Temp-Tables.TT_ArtBas.LevKod
     _FldNameList[6]   > "_<CALC>"
"TT_ArtBas.DB%[1]" "Ord. DB%" ? ? ? ? ? ? ? ? no ? no no "9.8" yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
"TT_ArtBas.DBKr[1]" "DBKr" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > "_<CALC>"
"TT_ArtBas.Pris[1]" "Pris" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > "_<CALC>"
"TT_ArtBas.VareKost[1]" "Varekost" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > "_<CALC>"
"TT_ArtBas.DB%[2]" "Tilb. DB%" ? ? ? ? ? ? ? ? no ? no no "10.4" yes no no "U" "" ""
     _FldNameList[11]   > "_<CALC>"
"TT_ArtBas.DBKr[2]" "DBKr" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > "_<CALC>"
"TT_ArtBas.Pris[2]" "Pris" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > "_<CALC>"
"TT_ArtBas.VareKost[2]" "Varekost" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kontroll kalkyle */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kontroll kalkyle */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ArtKort C-Win
ON CHOOSE OF B-ArtKort IN FRAME DEFAULT-FRAME /* Artikkelkort */
DO:
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevBas C-Win
ON CHOOSE OF B-LevBas IN FRAME DEFAULT-FRAME /* Leverandører... */
DO:
  ASSIGN T-AlleLevBas:CHECKED = FALSE.
  APPLY "VALUE-CHANGED" TO T-AlleLevBas.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SaSong C-Win
ON CHOOSE OF B-SaSong IN FRAME DEFAULT-FRAME /* Sesong... */
DO:
    ASSIGN T-AlleSaSong:CHECKED = FALSE.
    APPLY "VALUE-CHANGED" TO T-AlleSaSong.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Utfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Utfor C-Win
ON CHOOSE OF B-Utfor IN FRAME DEFAULT-FRAME /* Utfør */
DO:
       DEF VAR iCount AS INTE NO-UNDO.
    DEFINE VARIABLE hCol AS WIDGET     NO-UNDO.
    {sww.i}
    ASSIGN iAntTraff = 0
           FI-DB%.
    EMPTY TEMP-TABLE TT_ArtBas NO-ERROR.
    BROWSE {&BROWSE-NAME}:REFRESH() NO-ERROR.
    ASSIGN B-ArtKort:SENSITIVE = FALSE.
    {&OPEN-QUERY-{&BROWSE-NAME}}

    IF NOT T-AlleVarGr:CHECKED AND NUM-ENTRIES(cValgteVarGr) = 1 THEN DO:
        FOR EACH ArtBas NO-LOCK WHERE ArtBas.Vg = INT(cValgteVarGr):
            RUN Analyser.
        END.
    END.
    ELSE IF NOT T-AlleLevBas:CHECKED AND NUM-ENTRIES(cValgteLevBas) = 1 THEN DO:
        FOR EACH ArtBas NO-LOCK WHERE ArtBas.LevNr = INT(cValgteLevBas):
            RUN Analyser.
        END.
    END.
    ELSE IF NOT T-AlleVaremerke:CHECKED AND NUM-ENTRIES(cValgteVaremerke) = 1 THEN DO:
        FOR EACH ArtBas NO-LOCK WHERE ArtBas.VMid = INT(cValgteVaremerke):
            RUN Analyser.
        END.
    END.
    ELSE DO:
        FOR EACH ArtBas NO-LOCK:
            RUN Analyser.
        END.
    END.
    DO icount = 6 TO 9:
        ASSIGN hCol = BROWSE {&BROWSE-NAME}:GET-BROWSE-COL(iCount)
               hCol:VISIBLE = CAN-DO("1,3",RS-Type:SCREEN-VALUE).
    END.
    DO icount = 10 TO 13:
        ASSIGN hCol = BROWSE {&BROWSE-NAME}:GET-BROWSE-COL(iCount)
               hCol:VISIBLE = CAN-DO("2,3",RS-Type:SCREEN-VALUE).
    END.
    ASSIGN BROWSE {&BROWSE-NAME}:MAX-DATA-GUESS = iAntTraff.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    ASSIGN
      B-ArtKort:SENSITIVE = iAntTraff > 0
      B-Excel:SENSITIVE = iAntTraff > 0
      .
    {swn.i}
    RETURN NO-APPLY.
/*
cValgteVarGr    
cValgteLevBas   
cValgteSaSong   
cValgteVaremerke

  DEF VAR iCount AS INTE NO-UNDO.
  {sww.i}
  EMPTY TEMP-TABLE TT_ArtPris NO-ERROR.
  BROWSE {&BROWSE-NAME}:REFRESH() NO-ERROR.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  IF INPUT FI-DB% = 0 THEN DO:
      MESSAGE "Angi > 0"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  FOR EACH Artpris NO-LOCK WHERE ArtPris.ProfilNr = INT(CB-PP:SCREEN-VALUE) AND
      ArtPris.Tilbud = (RS-Type:SCREEN-VALUE = "2") AND
      (ArtPris.DB%[INT(RS-Type:SCREEN-VALUE)] < INPUT FI-DB% - INPUT FI-Avvik OR ArtPris.DB%[INT(RS-Type:SCREEN-VALUE)] > INPUT FI-DB% + INPUT FI-Avvik):
      BUFFER-COPY ArtPris TO TT_ArtPris.
      RELEASE TT_ArtPris.
      ASSIGN iCount = iCount + 1.
      IF iCount MOD 100 = 0 THEN
          ASSIGN FI-Antal:SCREEN-VALUE = STRING(iCount).
  END.
  ASSIGN FI-Antal:SCREEN-VALUE = STRING(iCount).
  {&OPEN-QUERY-{&BROWSE-NAME}}
  {swn.i}
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Varemerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Varemerke C-Win
ON CHOOSE OF B-Varemerke IN FRAME DEFAULT-FRAME /* Varemerker... */
DO:
    ASSIGN T-AlleVaremerke:CHECKED = FALSE.
    APPLY "VALUE-CHANGED" TO T-AlleVaremerke.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VarGr C-Win
ON CHOOSE OF B-VarGr IN FRAME DEFAULT-FRAME /* Varegrupper... */
DO:
    ASSIGN T-AlleVarGr:CHECKED = FALSE.
    APPLY "VALUE-CHANGED" TO T-AlleVarGr.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON DEFAULT-ACTION OF BROWSE-4 IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO B-ArtKort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON ROW-DISPLAY OF BROWSE-4 IN FRAME DEFAULT-FRAME
DO:
    DEF VAR iIdx AS INTE NO-UNDO.
    ASSIGN iIdx = IF RS-Type:SCREEN-VALUE = "2" OR                               
                   (RS-Type:SCREEN-VALUE = "3" AND TT_ArtBas.Tilbud = TRUE) THEN 2 ELSE 1.
    IF iIdx = 1 THEN
        ASSIGN TT_ArtBas.DB%[1]:BGCOLOR IN BROWSE {&BROWSE-NAME} = 11
                TT_ArtBas.DBKr[1]:BGCOLOR IN BROWSE {&BROWSE-NAME} = 
                    IF TT_ArtBas.DBKr[1] < 0 THEN 12 ELSE
                    IF TT_ArtBas.DB%[1] < FI-DB% THEN 14 ELSE 10.
    ELSE
        ASSIGN TT_ArtBas.DB%[2]:BGCOLOR IN BROWSE {&BROWSE-NAME} = 11
                TT_ArtBas.DBKr[2]:BGCOLOR IN BROWSE {&BROWSE-NAME} = 
                    IF TT_ArtBas.DBKr[2] < 0 THEN 12 ELSE
                    IF TT_ArtBas.DB%[2] < FI-DB% THEN 14 ELSE 10.
/*                                                                                                                        */
/* ASSIGN                                                                                                                 */
/*   TT_ArtBas.DB%[1]:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF RS-Type:SCREEN-VALUE = "1" OR                                 */
/*                                               (RS-Type:SCREEN-VALUE = "3" AND TT_ArtBas.Tilbud = FALSE) THEN 11 ELSE ? */
/*   TT_ArtBas.DB%[2]:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF RS-Type:SCREEN-VALUE = "2" OR                                 */
/*                                               (RS-Type:SCREEN-VALUE = "3" AND TT_ArtBas.Tilbud = TRUE) THEN 11 ELSE ?. */
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


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
    IF VALID-HANDLE(hvArtkor) THEN
        APPLY "CLOSE":U TO hvArtkor.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
/*   if can-find(first tmpChild where                                                 */
/*                valid-handle(tmpChild.wChild)) then                                 */
/*     do:                                                                            */
/*       wBekreft = false.                                                            */
/*       message 'Det er startet andre programmer fra dette vinduet.' skip            */
/*               'Avsluttes dette vinduet, vil alle underliggende programmer' skip    */
/*               'også bli avsluttet.'                                                */
/*               view-as alert-box warning buttons yes-no title 'Bekreft avsluttning' */
/*               update wBekreft                                                      */
/*               .                                                                    */
/*     end.                                                                           */
/*   else wBekreft = true.                                                            */
/*   if wBekreft <> true then                                                         */
/*   return no-apply.                                                                 */
/*                                                                                    */
/*   if return-value = "OK" then                                                      */
/*       APPLY "CLOSE":U TO THIS-PROCEDURE.                                           */
/*   else do:                                                                         */
/*     readkey pause 0.                                                               */
/*     return no-apply.                                                               */
/*   end.                                                                             */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-AlleLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-AlleLevBas C-Win
ON VALUE-CHANGED OF T-AlleLevBas IN FRAME DEFAULT-FRAME /* Toggle 2 */
DO:
    IF NOT SELF:CHECKED THEN DO:
        IF cValgteLevBas = "*" THEN
            ASSIGN cValgteLevBas = "".
        RUN d-tag2levbas.w (INPUT-OUTPUT cValgteLevBas).
    END.
    ELSE
        ASSIGN cValgteLevBas = "".
    IF cValgteLevBas = "" THEN
        ASSIGN SELF:CHECKED = TRUE
               cValgteLevBas = "*".
    RUN VisValgte("LevBas").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-AlleSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-AlleSaSong C-Win
ON VALUE-CHANGED OF T-AlleSaSong IN FRAME DEFAULT-FRAME /* Toggle 4 */
DO:
    IF NOT SELF:CHECKED THEN DO:
        IF cValgteSaSong = "*" THEN
            ASSIGN cValgteSaSong = "".
        RUN d-tagsasong.w (INPUT-OUTPUT cValgteSaSong).
    END.
    ELSE
        ASSIGN cValgteSaSong = "".
    IF cValgteSaSong = "" THEN
        ASSIGN SELF:CHECKED = TRUE
               cValgteSaSong = "*".
    RUN VisValgte("SaSong").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-AlleVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-AlleVaremerke C-Win
ON VALUE-CHANGED OF T-AlleVaremerke IN FRAME DEFAULT-FRAME /* Toggle 3 */
DO:
    IF NOT SELF:CHECKED THEN DO:
        IF cValgteVaremerke = "*" THEN
            ASSIGN cValgteVaremerke = "".
        RUN d-tagvaremerke.w (INPUT-OUTPUT cValgteVaremerke).
    END.
    ELSE
        ASSIGN cValgteVaremerke = "".
    IF cValgteVaremerke = "" THEN
        ASSIGN SELF:CHECKED = TRUE
               cValgteVaremerke = "*".
    RUN VisValgte("Varemerke").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-AlleVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-AlleVarGr C-Win
ON VALUE-CHANGED OF T-AlleVarGr IN FRAME DEFAULT-FRAME /* Toggle 1 */
DO:
    IF NOT SELF:CHECKED THEN DO:
        IF cValgteVarGr = "*" THEN
            ASSIGN cValgteVarGr = "".
        RUN d-tagvaregr.w (INPUT-OUTPUT cValgteVarGr).
    END.
    ELSE
        ASSIGN cValgteVarGr = "".
    IF cValgteVarGr = "" THEN
        ASSIGN SELF:CHECKED = TRUE
               cValgteVarGr = "*".
    RUN VisValgte("VarGr").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{syspara.i 1 100 1 wAlle}

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Søkeliste prissavvik"
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
  FOR EACH PrisProfil NO-LOCK:
      ASSIGN cComboStr = cComboStr + (IF cComboStr = "" THEN "" ELSE ",") + 
            Prisprofil.KortNavn + "," + STRING(Prisprofil.ProfilNr).
  END.
  ASSIGN CB-PP:LIST-ITEM-PAIRS = cComboStr
         CB-PP:SCREEN-VALUE    = ENTRY(2,cComboStr)
         T-AlleVarGr:LABEL     = wAlle
         T-AlleLevBas:LABEL    = wAlle
         T-AlleSaSong:LABEL    = wAlle
         T-AlleVaremerke:LABEL = wAlle.
  RUN enable_UI.
  {lng.i}

  {browsesettings.i {&BROWSE-NAME}}

  RUN InitParametre.
  RUN InitLabels.
  APPLY "ENTRY":U TO FI-DB% IN FRAME Default-Frame.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
  DEFINE RETURN PARAMETER IsLocked AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Analyser C-Win 
PROCEDURE Analyser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iDB%idx AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    IF RS-Type:SCREEN-VALUE = "2" AND ArtBas.SattPaKampanje = ? THEN
        RETURN.
    IF CAN-DO(cValgteVarGr,STRING(ArtBas.Vg)) AND
       CAN-DO(cValgteLevBas,STRING(ArtBas.LevNr)) AND
       CAN-DO(cValgteSaSong,STRING(ArtBas.SaSong)) AND
       CAN-DO(cValgteVaremerke,STRING(ArtBas.VMid)) THEN DO:

/*         IF RS-Type:SCREEN-VALUE <> "3" THEN                                             */
/*             FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = INT(CB-PP:SCREEN-VALUE) AND */
/*                    ArtPris.Tilbud = (RS-Type:SCREEN-VALUE = "2") NO-LOCK NO-ERROR.      */
/*         ELSE                                                                            */
        FIND ArtPris OF ArtBas WHERE ArtPris.ProfilNr = INT(CB-PP:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAIL ArtPris THEN DO:
            ASSIGN iDB%idx = IF RS-Type:SCREEN-VALUE <> "3" then
                      INT(RS-Type:SCREEN-VALUE) ELSE (IF NOT ArtPris.Tilbud THEN 1 ELSE 2).
            IF T-Godkjente:CHECKED THEN DO:
                IF NOT (ArtPris.DB%[iDB%idx] >= INPUT FI-DB% - INPUT FI-Avvik AND 
                   ArtPris.DB%[iDB%idx] <= INPUT FI-DB% + INPUT FI-Avvik) THEN
                    NEXT.
            END.
            ELSE DO:
                IF NOT (ArtPris.DB%[iDB%idx] < INPUT FI-DB% - INPUT FI-Avvik OR 
                   ArtPris.DB%[iDB%idx] > INPUT FI-DB% + INPUT FI-Avvik) THEN
                    NEXT.
            END.
            BUFFER-COPY ArtBas TO TT_ArtBas
                ASSIGN TT_ArtBas.DB%[1]      = ArtPris.DB%[1]
                       TT_ArtBas.DB%[2]      = ArtPris.DB%[2]
                       TT_ArtBas.TilBud      = ArtPris.TilBud
                       TT_ArtBas.DBKr[1]     = ArtPris.DBKr[1] 
                       TT_ArtBas.DBKr[2]     = ArtPris.DBKr[2] 
                       TT_ArtBas.Pris[1]     = ArtPris.Pris[1]
                       TT_ArtBas.Pris[2]     = ArtPris.Pris[2]
                       TT_ArtBas.VareKost[1] = ArtPris.VareKost[1]
                       TT_ArtBas.VareKost[2] = ArtPris.VareKost[2]
                       iAntTraff        = iAntTraff + 1.
            RELEASE TT_ArtBas.
        END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkel C-Win 
PROCEDURE Artikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEF VAR hParent AS HANDLE NO-UNDO.     */
/*   IF NOT VALID-HANDLE(hvArtkor) THEN DO: */
      FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtBas.ArtikkelNr NO-LOCK.

      /*RUN w-vartkor.w PERSISTENT SET hvArtkor (RECID(ArtBas),"ENDRE").*/
      fLockvindu(TRUE).
      RUN w-vartkor.w (RECID(ArtBas),"ENDRE" + "," + STRING(THIS-PROCEDURE) + ",2").
      fLockvindu(FALSE).
/*   END.                                                                  */
/*   ELSE DO:                                                              */
/*     FIND ArtBas WHERE ArtBas.ArtikkelNr = TT_ArtBas.ArtikkelNr NO-LOCK. */
/*     IF AVAILABLE ArtBAs THEN                                            */
/*       RUN ByttArtikkel IN hvArtKor (RECID(ArtBas)).                     */
/*     ASSIGN hParent = hvArtkor:CURRENT-WINDOW.                           */
/*     hParent:MOVE-TO-TOP().                                              */
/*   END.                                                                  */
/*   IF VALID-HANDLE(hvArtkor) THEN                                        */
/*   DO:                                                                   */
/*     RUN SetAktivFlik IN hvArtKor (2).                                   */
/*   END.                                                                  */

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
  DISPLAY T-AlleVarGr T-AlleLevBas T-AlleVaremerke T-AlleSaSong FI-VarGr 
          FI-LevBas FI-Varemerke FI-SaSong T-Godkjente RS-Type CB-PP FI-DB% 
          FI-Avvik FILL-IN-3 FILL-IN-1 FILL-IN-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_Help BROWSE-4 T-AlleVarGr T-AlleLevBas T-AlleVaremerke 
         T-AlleSaSong BUTTON-Ok B-VarGr B-LevBas B-Varemerke B-SaSong 
         T-Godkjente RS-Type CB-PP FI-DB% FI-Avvik B-Utfor FILL-IN-3 FILL-IN-1 
         FILL-IN-2 RECT-1 RECT-2 RECT-27 RECT-28 RECT-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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

  ASSIGN 
      cExcEkstent = if cExcEkstent = "" 
                      then "sdv" 
                      else cExcEkstent.

  IF VALID-HANDLE(wLibHandle) then
    RUN GetTempFileName IN wLibHandle (INPUT "hgrapp", INPUT cExcEkstent, OUTPUT ctmpFileName).

  OUTPUT STREAM Eksport TO VALUE(ctmpFileName).

DO WITH FRAME {&FRAME-NAME}:
  {sww.i}
  EXPORT STREAM Eksport "".
  EXPORT STREAM Eksport "".
  EXPORT STREAM Eksport {&WINDOW-NAME}:TITLE.
  EXPORT STREAM Eksport CB-PP:LABEL + ": " + CB-PP:SCREEN-VALUE.
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
                                      ENTRY(13,cColLabels).
                                      
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}

  BROWSE {&BROWSE-NAME}:SELECT-ROW(1).
  REPEAT:
      EXPORT STREAM Eksport DELIMITER ";"
          {&FIELDS-IN-QUERY-{&BROWSE-NAME}}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitParametre C-Win 
PROCEDURE InitParametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
  {sww.i}
  SELECT Count(*) INTO iAntVarGr     FROM VarGr. 
  SELECT Count(*) INTO iAntLevBas    FROM LevBas.
  SELECT Count(*) INTO iAntSaSong    FROM SaSong.    
  SELECT Count(*) INTO iAntVaremerke FROM Varemerke.   
  RUN VisValgte("VarGr").
  RUN VisValgte("LevBas").
  RUN VisValgte("SaSong").
  RUN VisValgte("Varemerke").
  {swn.i}
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
        PUBLISH "ByttArtikkel" (TT_ArtBas.ArtikkelNr).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisValgte C-Win 
PROCEDURE VisValgte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFrom AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    CASE cFrom:
        WHEN "VarGr" THEN
            ASSIGN FI-VarGr:SCREEN-VALUE = (STRING(IF T-AlleVarGr:CHECKED THEN iAntVarGr ELSE
                      NUM-ENTRIES(cValgteVarGr))) + " / " + STRING(iAntVarGr)
                   FI-VarGr:TOOLTIP = IF T-AlleVarGr:CHECKED THEN wAlle ELSE cValgteVarGr.
        WHEN "LevBas" THEN
            ASSIGN FI-LevBas:SCREEN-VALUE = (STRING(IF T-AlleLevBas:CHECKED THEN iAntLevBas ELSE
                        NUM-ENTRIES(cValgteLevBas))) + " / " + STRING(iAntLevBas)
                   FI-LevBas:TOOLTIP = IF T-AlleLevBas:CHECKED THEN wAlle ELSE cValgteLevBas.
    
        WHEN "Varemerke" THEN
            ASSIGN FI-Varemerke:SCREEN-VALUE = (STRING(IF T-AlleVaremerke:CHECKED THEN iAntVaremerke ELSE
                                    NUM-ENTRIES(cValgteVaremerke))) + " / " + STRING(iAntVaremerke)
                   FI-Varemerke:TOOLTIP = IF T-AlleVaremerke:CHECKED THEN wAlle ELSE cValgteVaremerke.
        WHEN "SaSong" THEN
            ASSIGN FI-SaSong:SCREEN-VALUE = (STRING(IF T-AlleSaSong:CHECKED THEN iAntSaSong ELSE
                        NUM-ENTRIES(cValgteSaSong))) + " / " + STRING(iAntSaSong)
                   FI-SaSong:TOOLTIP = IF T-AlleSaSong:CHECKED THEN wAlle ELSE cValgteSaSong.
    END CASE.
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

