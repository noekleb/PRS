&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Butiker NO-UNDO LIKE Butiker.
DEFINE TEMP-TABLE TT_ValgteButiker NO-UNDO LIKE Butiker.


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
     that this procedure"s triggers and internal procedures 
     will execute in this procedure"s storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cSLPluEan AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_RapHodePB
    FIELD b_id      LIKE BongHode.b_id
    FIELD betalkort AS LOGICAL 
    FIELD BongType  AS INTE /* 1 = bara SL 2 = båda */
    FIELD antall    AS DECI EXTENT 12 LABEL "Antal" /*                      */
    FIELD Belop     AS DECI EXTENT 12 LABEL "Belopp" /*                      */
    FIELD sumFsgSL  AS DECI EXTENT 12 LABEL "sumSL" /*                      */
    FIELD sumOvr    AS DECI EXTENT 12 LABEL "sumOvr" /*                      */
    INDEX BID IS PRIMARY UNIQUE b_id.


DEFINE TEMP-TABLE TT_RapLinjePB
    FIELD b_id      LIKE BongHode.b_id
    FIELD grupp     AS INTE
    FIELD ArtikkelNr LIKE Bonglinje.ArtikkelNr
    FIELD Beskrivelse AS CHAR
    FIELD antall    AS DECI EXTENT 12  /*                      */
    FIELD sumFsg    AS DECI EXTENT 12  /*                      */
    FIELD PluEan    AS CHAR
    INDEX BID IS PRIMARY UNIQUE b_id ArtikkelNr
    INDEX grupp grupp.

DEFINE TEMP-TABLE TmpTT_RapLinjePB LIKE TT_RapLinjePB.

DEFINE TEMP-TABLE ButTT_RapHodePB
    FIELD Butik LIKE Butiker.butik
    FIELD antallKont    AS INTE
    FIELD antallKort    AS INTE
    FIELD antallMKort  AS INTE EXTENT 12
    FIELD sumFsgSLKont  AS DECI
    FIELD sumFsgSLKort  AS DECI
    INDEX Butik IS PRIMARY UNIQUE Butik.

DEFINE TEMP-TABLE butTT_RapLinjePB
    FIELD Butik LIKE Butiker.butik
    FIELD antall    AS DECI EXTENT 12  /*                      */
    FIELD sumFsg    AS DECI EXTENT 12  /*                      */
    FIELD PluEan    AS CHAR
    INDEX Butik IS PRIMARY UNIQUE Butik.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-TTButiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Butiker TT_ValgteButiker TT_RapHodePB ~
TT_RapLinjePB Gruppe Kasse

/* Definitions for BROWSE BR-TTButiker                                  */
&Scoped-define FIELDS-IN-QUERY-BR-TTButiker TT_Butiker.Butik 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-TTButiker 
&Scoped-define QUERY-STRING-BR-TTButiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-TTButiker OPEN QUERY BR-TTButiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-TTButiker TT_Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BR-TTButiker TT_Butiker


/* Definitions for BROWSE BR-TTValgteButiker                            */
&Scoped-define FIELDS-IN-QUERY-BR-TTValgteButiker TT_ValgteButiker.Butik 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-TTValgteButiker 
&Scoped-define QUERY-STRING-BR-TTValgteButiker FOR EACH TT_ValgteButiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-TTValgteButiker OPEN QUERY BR-TTValgteButiker FOR EACH TT_ValgteButiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-TTValgteButiker TT_ValgteButiker
&Scoped-define FIRST-TABLE-IN-QUERY-BR-TTValgteButiker TT_ValgteButiker


/* Definitions for BROWSE BROWSE-TT_RapHodePB                           */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_RapHodePB TT_RapHodePB.b_id TT_RapHodePB.betalkort TT_RapHodePB.BongType TT_RapHodePB.antall TT_RapHodePB.belop TT_RapHodePB.sumFsgSL TT_RapHodePB.sumOvr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_RapHodePB   
&Scoped-define SELF-NAME BROWSE-TT_RapHodePB
&Scoped-define QUERY-STRING-BROWSE-TT_RapHodePB FOR EACH TT_RapHodePB
&Scoped-define OPEN-QUERY-BROWSE-TT_RapHodePB OPEN QUERY {&SELF-NAME} FOR EACH TT_RapHodePB.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_RapHodePB TT_RapHodePB
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_RapHodePB TT_RapHodePB


/* Definitions for BROWSE BROWSE-TT_RapLinjePB                          */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_RapLinjePB TT_RapLinjePB.b_id TT_RapLinjePB.grupp TT_RapLinjePB.ArtikkelNr TT_RapLinjePB.Beskrivelse TT_RapLinjePB.antall TT_RapLinjePB.sumFsg   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_RapLinjePB   
&Scoped-define SELF-NAME BROWSE-TT_RapLinjePB
&Scoped-define QUERY-STRING-BROWSE-TT_RapLinjePB FOR EACH TT_RapLinjePB OF TT_RapHodePB
&Scoped-define OPEN-QUERY-BROWSE-TT_RapLinjePB OPEN QUERY {&SELF-NAME} FOR EACH TT_RapLinjePB OF TT_RapHodePB.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_RapLinjePB TT_RapLinjePB
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_RapLinjePB TT_RapLinjePB


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-TTButiker}~
    ~{&OPEN-QUERY-BR-TTValgteButiker}~
    ~{&OPEN-QUERY-BROWSE-TT_RapHodePB}~
    ~{&OPEN-QUERY-BROWSE-TT_RapLinjePB}
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH Gruppe SHARE-LOCK, ~
      EACH Kasse OF Gruppe SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Gruppe SHARE-LOCK, ~
      EACH Kasse OF Gruppe SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Gruppe Kasse
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Gruppe
&Scoped-define SECOND-TABLE-IN-QUERY-DEFAULT-FRAME Kasse


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-56 RECT-57 BROWSE-TT_RapHodePB ~
B-Summera RS-Metode CB-Analyse FI-FraDato FI-TilDato B-Dump BR-TTButiker ~
BR-TTValgteButiker BUTTON-LeggTil BROWSE-TT_RapLinjePB FI-Butiknr ~
S-Bearbetade FI-KasseNr FI-DatoLoop FI-HodeSum FI-LinjeSum FILL-IN-20 ~
FILL-IN-19 FILL-IN-18 FILL-IN-21 FILL-IN-24 FILL-IN-22 FILL-IN-26 
&Scoped-Define DISPLAYED-OBJECTS RS-Metode CB-Analyse FI-FraDato FI-TilDato ~
FI-Butiknr S-Bearbetade FI-KasseNr FI-DatoLoop FI-HodeSum FI-LinjeSum ~
FILL-IN-20 FILL-IN-19 FILL-IN-18 FILL-IN-21 FILL-IN-24 FILL-IN-22 ~
FILL-IN-26 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Dump 
     LABEL "Dump av tabeller" 
     SIZE 25 BY 1.14.

DEFINE BUTTON B-Export 
     LABEL "EXPORT" 
     SIZE 24.8 BY 1.14.

DEFINE BUTTON B-NyRapp 
     LABEL "Ny rapport" 
     SIZE 25 BY 1.14.

DEFINE BUTTON B-Summera 
     LABEL "Summera" 
     SIZE 24.8 BY 1.14.

DEFINE BUTTON BUTTON-AlleFra 
     LABEL "<< &Alla bort" 
     SIZE 15 BY 1.1.

DEFINE BUTTON BUTTON-LeggTil 
     LABEL "Lägg till &>>" 
     SIZE 15 BY 1.1.

DEFINE BUTTON BUTTON-TrekkFra 
     LABEL "&<< Ta bort" 
     SIZE 15 BY 1.1.

DEFINE VARIABLE CB-Analyse AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Analys" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Välj analys",0
     DROP-DOWN-LIST
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butiknr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butiknr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-DatoLoop AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Från dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HodeSum AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-KasseNr AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Kassa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-LinjeSum AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     LABEL "Till dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS CHARACTER FORMAT "X(256)":U INITIAL " Valda butiker" 
      VIEW-AS TEXT 
     SIZE 16 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS CHARACTER FORMAT "X(256)":U INITIAL " Tillgängliga butiker" 
      VIEW-AS TEXT 
     SIZE 19.8 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U INITIAL " Rapportbegränsning" 
      VIEW-AS TEXT 
     SIZE 21 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U INITIAL " Bearbetar" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-22 AS CHARACTER FORMAT "X(256)":U INITIAL " Namn på exportfil summeringsnivå" 
      VIEW-AS TEXT 
     SIZE 34.6 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-24 AS CHARACTER FORMAT "X(256)":U INITIAL " Bearbetade" 
      VIEW-AS TEXT 
     SIZE 12.6 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-26 AS CHARACTER FORMAT "X(256)":U INITIAL " Namn på exportfil artikelnivå" 
      VIEW-AS TEXT 
     SIZE 28.6 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE RS-Metode AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Analyselogg-metod", 1,
"Bonglinje-metod", 2
     SIZE 48 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 6.91.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 13.1.

DEFINE VARIABLE S-Bearbetade AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 23 BY 5.57 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-TTButiker FOR 
      TT_Butiker SCROLLING.

DEFINE QUERY BR-TTValgteButiker FOR 
      TT_ValgteButiker SCROLLING.

DEFINE QUERY BROWSE-TT_RapHodePB FOR 
      TT_RapHodePB SCROLLING.

DEFINE QUERY BROWSE-TT_RapLinjePB FOR 
      TT_RapLinjePB SCROLLING.

DEFINE QUERY DEFAULT-FRAME FOR 
      Gruppe, 
      Kasse SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-TTButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-TTButiker C-Win _STRUCTURED
  QUERY BR-TTButiker NO-LOCK DISPLAY
      TT_Butiker.Butik FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 17 BY 5.81 ROW-HEIGHT-CHARS .62 EXPANDABLE.

DEFINE BROWSE BR-TTValgteButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-TTValgteButiker C-Win _STRUCTURED
  QUERY BR-TTValgteButiker NO-LOCK DISPLAY
      TT_ValgteButiker.Butik FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 16 BY 5.81 ROW-HEIGHT-CHARS .62 EXPANDABLE.

DEFINE BROWSE BROWSE-TT_RapHodePB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_RapHodePB C-Win _FREEFORM
  QUERY BROWSE-TT_RapHodePB DISPLAY
      TT_RapHodePB.b_id     
TT_RapHodePB.betalkort
TT_RapHodePB.BongType 
TT_RapHodePB.antall FORMAT "->>>,>>>,>>>,>>9.99"  
TT_RapHodePB.belop  FORMAT "->>>,>>>,>>>,>>9.99"
TT_RapHodePB.sumFsgSL FORMAT "->>>,>>>,>>>,>>9.99"
TT_RapHodePB.sumOvr  FORMAT "->>>,>>>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 56 BY 9.05 EXPANDABLE.

DEFINE BROWSE BROWSE-TT_RapLinjePB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_RapLinjePB C-Win _FREEFORM
  QUERY BROWSE-TT_RapLinjePB DISPLAY
      TT_RapLinjePB.b_id     
TT_RapLinjePB.grupp    
TT_RapLinjePB.ArtikkelNr
TT_RapLinjePB.Beskrivelse
TT_RapLinjePB.antall  FORMAT "->>>,>>>,>>9.99" 
TT_RapLinjePB.sumFsg FORMAT "->>>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 11.67 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-TT_RapHodePB AT ROW 1.48 COL 3
     B-Summera AT ROW 1.71 COL 132
     RS-Metode AT ROW 2.19 COL 77 NO-LABEL
     CB-Analyse AT ROW 3.38 COL 75 COLON-ALIGNED
     B-Export AT ROW 3.38 COL 132
     FI-FraDato AT ROW 4.57 COL 88.8 COLON-ALIGNED
     B-NyRapp AT ROW 5.05 COL 132
     FI-TilDato AT ROW 5.62 COL 88.8 COLON-ALIGNED
     B-Dump AT ROW 6.71 COL 132
     BR-TTButiker AT ROW 7.86 COL 70.8
     BR-TTValgteButiker AT ROW 7.86 COL 107
     BUTTON-LeggTil AT ROW 8.86 COL 90
     BUTTON-TrekkFra AT ROW 10.05 COL 90
     BUTTON-AlleFra AT ROW 11.24 COL 90
     BROWSE-TT_RapLinjePB AT ROW 15.29 COL 3
     FI-Butiknr AT ROW 16.38 COL 117.4 COLON-ALIGNED
     S-Bearbetade AT ROW 16.38 COL 135 NO-LABEL
     FI-KasseNr AT ROW 17.52 COL 117.4 COLON-ALIGNED HELP
          "Kassenummer"
     FI-DatoLoop AT ROW 18.67 COL 117.4 COLON-ALIGNED
     FI-HodeSum AT ROW 23.38 COL 110.2 COLON-ALIGNED NO-LABEL
     FI-LinjeSum AT ROW 25.76 COL 110.2 COLON-ALIGNED NO-LABEL
     FILL-IN-20 AT ROW 1.19 COL 82 COLON-ALIGNED NO-LABEL
     FILL-IN-19 AT ROW 7.05 COL 68 COLON-ALIGNED NO-LABEL
     FILL-IN-18 AT ROW 7.05 COL 104.8 COLON-ALIGNED NO-LABEL
     FILL-IN-21 AT ROW 15.52 COL 118.8 COLON-ALIGNED NO-LABEL
     FILL-IN-24 AT ROW 15.52 COL 138.4 COLON-ALIGNED NO-LABEL
     FILL-IN-22 AT ROW 22.43 COL 110.4 COLON-ALIGNED NO-LABEL
     FILL-IN-26 AT ROW 24.81 COL 110.4 COLON-ALIGNED NO-LABEL
     RECT-56 AT ROW 15.29 COL 109
     RECT-57 AT ROW 1.48 COL 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 26.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Butiker T "?" NO-UNDO skotex Butiker
      TABLE: TT_ValgteButiker T "?" NO-UNDO skotex Butiker
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Summera PB-SL"
         HEIGHT             = 26.62
         WIDTH              = 160
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-TT_RapHodePB RECT-57 DEFAULT-FRAME */
/* BROWSE-TAB BR-TTButiker B-Dump DEFAULT-FRAME */
/* BROWSE-TAB BR-TTValgteButiker BR-TTButiker DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-TT_RapLinjePB BUTTON-AlleFra DEFAULT-FRAME */
/* SETTINGS FOR BUTTON B-Export IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-NyRapp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-AlleFra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-TrekkFra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-Butiknr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-DatoLoop:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-HodeSum:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-KasseNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-LinjeSum:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-TTButiker
/* Query rebuild information for BROWSE BR-TTButiker
     _TblList          = "Temp-Tables.TT_Butiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_Butiker.Butik
     _Query            is OPENED
*/  /* BROWSE BR-TTButiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-TTValgteButiker
/* Query rebuild information for BROWSE BR-TTValgteButiker
     _TblList          = "Temp-Tables.TT_ValgteButiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_ValgteButiker.Butik
     _Query            is OPENED
*/  /* BROWSE BR-TTValgteButiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_RapHodePB
/* Query rebuild information for BROWSE BROWSE-TT_RapHodePB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_RapHodePB.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_RapHodePB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_RapLinjePB
/* Query rebuild information for BROWSE BROWSE-TT_RapLinjePB
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_RapLinjePB OF TT_RapHodePB.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_RapLinjePB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "skotex.Gruppe,skotex.Kasse OF skotex.Gruppe"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Summera PB-SL */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Summera PB-SL */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Dump
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Dump C-Win
ON CHOOSE OF B-Dump IN FRAME DEFAULT-FRAME /* Dump av tabeller */
DO:
  RUN DumpAvTabeller.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Export C-Win
ON CHOOSE OF B-Export IN FRAME DEFAULT-FRAME /* EXPORT */
DO:
    DEFINE VARIABLE cID     AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE dSumSL  AS DECIMAL EXTENT 12 NO-UNDO.
    DEFINE VARIABLE dSumOv  AS DECIMAL EXTENT 12 NO-UNDO.
    DEFINE VARIABLE dTotSum AS DECIMAL EXTENT 12 NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER            NO-UNDO.
    DEFINE VARIABLE dRSumKontSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumKontSLOv AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumKortSL AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumKortSLOv AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumKontOv AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumKortOv AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumSumSL  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumTotSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dRSumSumOv  AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dArtNr      AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cSL% AS CHARACTER EXTENT 12 NO-UNDO.
    DEFINE VARIABLE cOv% AS CHARACTER EXTENT 12 NO-UNDO.
    ASSIGN cID = STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","").
  ASSIGN FI-HodeSum  = SESSION:TEMP-DIR + "SUM_" + cID + ".skv"
         FI-LinjeSum = SESSION:TEMP-DIR + "ART_" + cID + ".skv"
         FI-HodeSum:SCREEN-VALUE  = FI-HodeSum
         FI-LinjeSum:SCREEN-VALUE = FI-LinjeSum.
  OUTPUT TO VALUE(FI-HodeSum).
  EXPORT "AB Svenska Pressbyrån, preliminär SL-försäljningsanalys".
  PUT " " SKIP.
  EXPORT "BELOPP".
  EXPORT DELIMITER ";"
      "Kontantförsäljning" "Jan" "Feb" "Mar" "Apr" "Maj" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dec" " " "Totaler".
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 1 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumFsgSL[iCount] = ROUND(sumFsgSL[iCount],0)
                 dSumSL[iCount]   = sumFsgSL[iCount]
                 dRSumKontSL      = dRSumKontSL + sumFsgSL[iCount].
      END.
      EXPORT DELIMITER ";" "SL enbart" sumFsgSL " " dRSumKontSL.
  END.
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 2 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumOvr[iCount] = ROUND(sumOvr[iCount],0)
                 dSumSL[iCount] = dSumSL[iCount] + sumFsgSL[iCount]
                 dSumOv[iCount] =  sumOvr[iCount]
                 dRSumKontSLOv  = dRSumKontSLOv + sumFsgSL[iCount]
                 dRSumKontOv    = dRSumKontOv  + sumOvr[iCount].
      END.
      EXPORT DELIMITER ";" "SL m övrigt"  sumFsgSL " " dRSumKontSLOv.
      EXPORT DELIMITER ";" "Övrigt"  sumOvr " " dRSumKontOv.
  END.
  PUT " " SKIP.    
   EXPORT "Kortförsäljning".
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 3 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumFsgSL[iCount] = ROUND(sumFsgSL[iCount],0)
                 dSumSL[iCount] = dSumSL[iCount] + sumFsgSL[iCount]
                 dRSumKortSL    = dRSumKortSL + sumFsgSL[iCount].
      END.
      EXPORT DELIMITER ";" "SL enbart" sumFsgSL " " dRSumKortSL.
  END.
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 4 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumOvr[iCount] = ROUND(sumOvr[iCount],0)
                 dSumSL[iCount] = dSumSL[iCount] + sumFsgSL[iCount]
                 dSumOv[iCount] = dSumOv[iCount] + sumOvr[iCount]
                 dRSumKortSLOv  = dRSumKortSLOv + sumFsgSL[iCount]
                 dRSumKortOv    = dRSumKortOv  + sumOvr[iCount].
      END.
      EXPORT DELIMITER ";" "SL m övrigt"  sumFsgSL " " dRSumKortSLOv.
      EXPORT DELIMITER ";" "Övrigt" sumOvr " " dRSumKortOv.
  END.
  PUT " " SKIP.    
  DO iCount = 1 TO 12:
      ASSIGN dTotSum[iCount] = dSumSL[iCount] + dSumOv[iCount]
             dRSumSumSL      = dRSumSumSL  + dSumSL[iCount]
             dRSumSumOv      = dRSumSumOv  + dSumOv[iCount]
             dRSumTotSum     = dRSumTotSum + dTotSum[iCount].
  END.
  EXPORT DELIMITER ";" "Summa SL-försäljning" dSumSL     " " dRSumSumSL.
  EXPORT DELIMITER ";" "Summa Övrig förs." dSumOv " " dRSumSumOv.
  EXPORT DELIMITER ";" "Total försäljning" dTotSum       " " dRSumTotSum.
  PUT " " SKIP.    
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 3 NO-ERROR.
  DO iCount = 1 TO 12:
      ASSIGN cSL%[iCount] = STRING(ROUND(sumFsgSL[iCount] / dSumSL[iCount] * 100,1)) + "%"
             cSL%[iCount] = IF cSL%[iCount] = ? THEN "" ELSE cSL%[iCount]
             cOV%[iCount] = STRING(ROUND(dSumOv[iCount] / dTotSum[iCount] * 100,1)) + "%"
             cOV%[iCount] = IF cOv%[iCount] = ? THEN "" ELSE cOv%[iCount].
  END.
  EXPORT DELIMITER ";" "%SL bet med kort" cSL% " " STRING(ROUND(dRSumKortSL / dRSumSumSL * 100,1)) + "%".
  EXPORT DELIMITER ";" "%Övrig försäljning" cOV% " " STRING(ROUND(dRSumSumOv / dRSumTotSum * 100,1)) + "%".
  ASSIGN dSumSL  = 0    
         dSumOv  = 0
         dTotSum = 0
         dRSumKontSL = 0
         dRSumKortSL = 0
         dRSumKontOv = 0
         dRSumKortOv = 0
         dRSumSumSL  = 0
         dRSumTotSum = 0
         dRSumSumOv  = 0.
/*  */
  PUT " " SKIP.
  EXPORT "ANTAL KVITTON".
  EXPORT DELIMITER ";"
      "Kontantförsäljning" "Jan" "Feb" "Mar" "Apr" "Maj" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dec" " " "Totaler".
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 1 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumFsgSL[iCount] = ROUND(sumFsgSL[iCount],0)
                 dSumSL[iCount]   = Antall[iCount]
                 dRSumKontSL      = dRSumKontSL + Antall[iCount].
      END.
      EXPORT DELIMITER ";" "SL enbart" Antall " " dRSumKontSL.
  END.
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 2 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumOvr[iCount] = ROUND(sumOvr[iCount],0)
                 dSumOv[iCount] =  Antall[iCount]
                 dRSumKoNtOv      = dRSumKontOv  + Antall[iCount].
      END.
      EXPORT DELIMITER ";" "Övrigt"  Antall " " dRSumKontOv.
  END.
  PUT " " SKIP.    
   EXPORT "Kortförsäljning".
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 3 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumFsgSL[iCount] = ROUND(sumFsgSL[iCount],0)
                 dSumSL[iCount] = dSumSL[iCount] + Antall[iCount]
                 dRSumKortSL    = dRSumKortSL + Antall[iCount].
      END.
      EXPORT DELIMITER ";" "SL enbart" Antall " " dRSumKortSL.
  END.
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 4 NO-ERROR.
  IF AVAIL TT_RapHodePB THEN DO:
      DO iCount = 1 TO 12:
          ASSIGN sumOvr[iCount] = ROUND(sumOvr[iCount],0)
                 dSumOv[iCount] = dSumOv[iCount] + Antall[iCount]
                 dRSumKortOv      = dRSumKortOv  + Antall[iCount].
      END.
      EXPORT DELIMITER ";" "Övrigt" Antall " " dRSumKortOv.
  END.
  PUT " " SKIP.    
  DO iCount = 1 TO 12:
      ASSIGN dTotSum[iCount] = dSumSL[iCount] + dSumOv[iCount]
             dRSumSumSL      = dRSumSumSL  + dSumSL[iCount]
             dRSumSumOv      = dRSumSumOv  + dSumOv[iCount]
             dRSumTotSum     = dRSumTotSum + dTotSum[iCount].
  END.
  EXPORT DELIMITER ";" "Summa SL enbart"     dSumSL   " " dRSumSumSL.
  EXPORT DELIMITER ";" "Summa med övrigt" dSumOv   " " dRSumSumOv.
  EXPORT DELIMITER ";" "Totalt med SL"        dTotSum  " " dRSumTotSum.
  PUT " " SKIP.    
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = 3 NO-ERROR.
  DO iCount = 1 TO 12:
      ASSIGN cSL%[iCount] = STRING(ROUND(Antall[iCount] / dSumSL[iCount] * 100,1)) + "%"
             cSL%[iCount] = IF cSL%[iCount] = ? THEN "" ELSE cSL%[iCount]
             cOV%[iCount] = STRING(ROUND(dSumOv[iCount] / dTotSum[iCount] * 100,1)) + "%"
             cOV%[iCount] = IF cOv%[iCount] = ? THEN "" ELSE cOv%[iCount].
  END.
  EXPORT DELIMITER ";" "%SL bet med kort" cSL% " " STRING(ROUND(dRSumKortSL / dRSumSumSL * 100,1)) + "%".
  EXPORT DELIMITER ";" "%Övrig försäljning" cOV% " " STRING(ROUND(dRSumSumOv / dRSumTotSum * 100,1)) + "%".
  PUT " " SKIP.
  EXPORT DELIMITER ";" "Butik" "SL bet kontant" "SL bet kort" "Belopp kontant" "Belopp kort" SKIP.
  FOR EACH ButTT_RapHodePB:
      EXPORT DELIMITER ";"
          ButTT_RapHodePB.butik ButTT_RapHodePB.antallKont ButTT_RapHodePB.antallKort  ButTT_RapHodePB.sumFsgSLKont ButTT_RapHodePB.sumFsgSLKort.
  END.
  PUT " " SKIP.
  PUT "SL bet kort/butik/månad" SKIP.
  PUT " " SKIP.
  EXPORT DELIMITER ";"
      "Butik" "Jan" "Feb" "Mar" "Apr" "Maj" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dec".
  FOR EACH ButTT_RapHodePB:
      EXPORT DELIMITER ";"
          ButTT_RapHodePB.butik ButTT_RapHodePB.antallMKort.
  END.
  OUTPUT CLOSE.
  /* Sätt PluEan i fält  */
  FOR EACH TT_RapLinjePB WHERE TT_RapLinjePB.grupp = 1:
      ASSIGN dArtNr = DECI(TT_RapLinjePB.ArtikkelNr) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          NEXT.
      FOR EACH StrekKode WHERE StrekKode.ArtikkelNr = dArtNr NO-LOCK.
          ASSIGN TT_RapLinjePB.PluEan = TT_RapLinjePB.PluEan + (IF TT_RapLinjePB.PluEan <> "" THEN ":" ELSE "") + StrekKode.Kode.
      END.
  END.

  OUTPUT TO VALUE(FI-LinjeSum).
  EXPORT DELIMITER ";"
      "Typ" " " "Artikkel" "Beskrivning" "PLU/Ean" "Antal" " " " " " " " " " " " " " " " " " " " " " " " " "Belopp".
  EXPORT DELIMITER ";"
      " " " " " " " " " " "Jan" "Feb" "Mar" "Apr" "Maj" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dec"
      " " "Jan" "Feb" "Mar" "Apr" "Maj" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Dec".
  FOR EACH TT_RapLinjePB:
      EXPORT DELIMITER ";" TT_RapLinjePB.b_id
                           TT_RapLinjePB.grupp
                           TT_RapLinjePB.ArtikkelNr
                           TT_RapLinjePB.Beskrivelse
                           TT_RapLinjePB.PluEan
                           TT_RapLinjePB.antall
                           " "
                           TT_RapLinjePB.sumFsg.
  END.
  PUT " " SKIP.
  FOR EACH butTT_RapLinjePB:
      EXPORT DELIMITER ";" butTT_RapLinjePB.butik
                           " "
                           " "
                           " "
                           " "
                           butTT_RapLinjePB.antall
                           " "
                           butTT_RapLinjePB.sumFsg.
  END.
  OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-NyRapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NyRapp C-Win
ON CHOOSE OF B-NyRapp IN FRAME DEFAULT-FRAME /* Ny rapport */
DO:
  EMPTY TEMP-TABLE TT_RapHodePB.
  EMPTY TEMP-TABLE TT_RapLinjePB.
  EMPTY TEMP-TABLE ButTT_RapHodePB.
  EMPTY TEMP-TABLE butTT_RapLinjePB.
  {&OPEN-QUERY-BROWSE-TT_RapHodePB}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-TT_RapHodePB.
  ASSIGN FI-Butiknr:SCREEN-VALUE = ""
         FI-DatoLoop:SCREEN-VALUE = ""
         FI-KasseNr:SCREEN-VALUE = ""
         FI-HodeSum:SCREEN-VALUE = ""
         FI-LinjeSum:SCREEN-VALUE = ""
         S-Bearbetade:LIST-ITEMS = ""
         S-Bearbetade:SCREEN-VALUE = "".
  APPLY "CHOOSE" TO BUTTON-AlleFra.
  ASSIGN B-Export:SENSITIVE = FALSE
         B-Summera:SENSITIVE = TRUE
         SELF:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Summera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Summera C-Win
ON CHOOSE OF B-Summera IN FRAME DEFAULT-FRAME /* Summera */
DO:
  DEFINE VARIABLE iKasseLoop AS INTEGER    NO-UNDO.
  IF SESSION:SET-WAIT-STATE("GENERAL") THEN.
/*   FOR EACH bonghode /* WHERE flslkort = 1 */ NO-LOCK: */
  IF NOT AVAIL Analyse THEN DO:
      MESSAGE "Välj analys"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ASSIGN INPUT FI-FraDato
         INPUT FI-TilDato.
  IF FI-FraDato = ? THEN DO:
      MESSAGE "Ange från datum"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-FraDato.
      RETURN NO-APPLY.
  END.
  IF FI-TilDato = ? THEN DO:
      MESSAGE "Ange till datum"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-TilDato.
      RETURN NO-APPLY.
  END.
  IF FI-FraDato < Analyse.StartDato THEN DO:
      MESSAGE "Till datum < analysens startdatum"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-FraDato.
      RETURN NO-APPLY.
  END.
  IF FI-FraDato > FI-TilDato THEN DO:
      MESSAGE "Från datum > till datum"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-FraDato.
      RETURN NO-APPLY.
  END.
  IF FI-TilDato > Analyse.SluttDato THEN DO:
      MESSAGE "Till datum > analysens slutdatum"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-FraDato.
      RETURN NO-APPLY.
  END.
  IF NOT CAN-FIND(FIRST TT_ValgteButiker) THEN DO:
      MESSAGE "Välj butiker"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO BR-TTButiker.
      RETURN NO-APPLY.
  END.
  ASSIGN FI-HodeSum:SCREEN-VALUE  = ""
         FI-LinjeSum:SCREEN-VALUE = "".

  FOR EACH TT_ValgteButiker NO-LOCK:
      ASSIGN FI-ButikNr:SCREEN-VALUE = STRING(TT_ValgteButiker.butik).
      S-Bearbetade:ADD-LAST(STRING(TT_ValgteButiker.Butik)).
      S-Bearbetade:SCROLL-TO-ITEM(STRING(TT_ValgteButiker.Butik)).
      PROCESS EVENTS.
          DO iKasseLoop = 1 TO 12: /* Vi tror inte någon butik har mer än 5 kassor */
/*       FOR EACH Kasse WHERE Kasse.Butik = TT_ValgteButiker.Butik NO-LOCK. */
          ASSIGN FI-KasseNr:SCREEN-VALUE = STRING(iKasseLoop).
          DO FI-DatoLoop = FI-FraDato TO FI-TilDato:
              ASSIGN FI-DatoLoop:SCREEN-VALUE = STRING(FI-DatoLoop).
              PROCESS EVENTS.
              IF INPUT RS-Metode = 1 THEN DO:
                  FOR EACH AnalyseLogg WHERE
                           AnalyseLogg.AnalyseId = Analyse.AnalyseId      AND
                           AnalyseLogg.Dato      = FI-DatoLoop            AND
                           AnalyseLogg.ButikkNr  = TT_ValgteButiker.butik AND
                           AnalyseLogg.GruppeNr  = 1                      AND  /* Alla PB har GruppeNr = 1 */
                           AnalyseLogg.KasseNr   = iKasseLoop             NO-LOCK.
                      FIND BongHode WHERE BongHode.b_id = AnalyseLogg.b_id NO-LOCK NO-ERROR.
                      IF NOT AVAIL BongHode THEN
                          NEXT.
                      RUN AnalyserSummerBong.
                  END. /* AnalyseLogg */
              END.
              ELSE DO:
                  FOR EACH BongHode WHERE BongHode.ButikkNr = TT_ValgteButiker.butik AND
                                          BongHode.GruppeNr = 1                      AND
                                          BongHode.KasseNr  = iKasseLoop             AND
                                          BongHode.Dato     = FI-DatoLoop USE-INDEX Bong NO-LOCK:
                      RUN AnalyserSummerBong.
                  END.
              END.
          END. /* Datoloop */
      END. /* Kasse */
  END. /* TT_ValgteButiker */
  {&OPEN-QUERY-BROWSE-TT_RapHodePB}
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-TT_RapHodePB.
  IF SESSION:SET-WAIT-STATE("") THEN.
  ASSIGN B-Export:SENSITIVE = BROWSE-TT_RapHodePB:FOCUSED-ROW <> ?
         B-Summera:SENSITIVE = NOT B-Export:SENSITIVE
         B-NyRapp:SENSITIVE  = TRUE
         BUTTON-AlleFra:SENSITIVE  = B-Summera:SENSITIVE
         BUTTON-LeggTil:SENSITIVE  = B-Summera:SENSITIVE
         BUTTON-TrekkFra:SENSITIVE = B-Summera:SENSITIVE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_RapHodePB
&Scoped-define SELF-NAME BROWSE-TT_RapHodePB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_RapHodePB C-Win
ON VALUE-CHANGED OF BROWSE-TT_RapHodePB IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BROWSE-TT_RapLinjePB}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AlleFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AlleFra C-Win
ON CHOOSE OF BUTTON-AlleFra IN FRAME DEFAULT-FRAME /* << Alla bort */
DO:
    Run TilFra("AlleFra").
    ASSIGN BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} = BROWSE BR-TTButiker:FOCUSED-ROW <> ?
           BUTTON-TrekkFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           SELF:SENSITIVE = NO.
    APPLY "ENTRY" TO BR-TTButiker.
    BR-TTButiker:SELECT-FOCUSED-ROW().
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-LeggTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-LeggTil C-Win
ON CHOOSE OF BUTTON-LeggTil IN FRAME DEFAULT-FRAME /* Lägg till >> */
DO:
     RUN TilFra("Til").
     IF BR-TTButiker:FOCUSED-ROW = ? THEN DO:
         ASSIGN SELF:SENSITIVE = FALSE.
         APPLY "ENTRY" TO BR-TTValgteButiker.
     END.
     ELSE DO:
         BR-TTButiker:SELECT-FOCUSED-ROW().
         APPLY "ENTRY" TO BR-TTButiker.
     END.
     ASSIGN BUTTON-AlleFra:SENSITIVE = BR-TTValgteButiker:FOCUSED-ROW <> ?
            BUTTON-TrekkFra:SENSITIVE = BUTTON-AlleFra:SENSITIVE.
/*      ASSIGN SELF:SENSITIVE = BR-TTButiker:SELECT-NEXT-ROW()       */
/*             BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES */
/*             BUTTON-TrekkFra:SENSITIVE = YES.                      */
/*      IF BR-TTButiker:FOCUSED-ROW <> ? THEN                        */
/*         APPLY "ENTRY" TO BROWSE BR-TTButiker.                     */
/*      ELSE                                                         */
/*          APPLY "ENTRY" TO BROWSE BR-TTValgteButiker.              */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-TrekkFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-TrekkFra C-Win
ON CHOOSE OF BUTTON-TrekkFra IN FRAME DEFAULT-FRAME /* << Ta bort */
DO:
     RUN TilFra("Fra").
     IF BR-TTValgteButiker:FOCUSED-ROW = ? THEN DO:
         ASSIGN SELF:SENSITIVE = FALSE
                BUTTON-AlleFra:SENSITIVE = FALSE.
         APPLY "ENTRY" TO BR-TTButiker.
         BR-TTButiker:SELECT-FOCUSED-ROW().
     END.
     ELSE DO:
         BR-TTValgteButiker:SELECT-FOCUSED-ROW().
         APPLY "ENTRY" TO BR-TTValgteButiker.
     END.
     ASSIGN BUTTON-LeggTil:SENSITIVE = BR-TTButiker:FOCUSED-ROW <> ?.
/*      IF BR-TTValgteButiker:FOCUSED-ROW <> ? THEN DO:                     */
/*          ASSIGN SELF:SENSITIVE = BR-TTValgteButiker:SELECT-FOCUSED-ROW() */
/*                 BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES.   */
/*          APPLY "ENTRY" TO BR-TTValgteButiker.                            */
/*      END.                                                                */
/*      ELSE DO:                                                            */
/*          ASSIGN SELF:SENSITIVE = NO                                      */
/*                 BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO     */
/*                 BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} =        */
/*                                   BR-TTButiker:NUM-SELECTED-ROWS > 0.    */
/*          APPLY "ENTRY" TO BR-TTButiker.                                  */
/*      END.                                                                */
/*     RETURN NO-APPLY.                                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Analyse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Analyse C-Win
ON VALUE-CHANGED OF CB-Analyse IN FRAME DEFAULT-FRAME /* Analys */
DO:
  FIND Analyse WHERE Analyse.AnalyseId = INPUT CB-Analyse NO-LOCK NO-ERROR.
  ASSIGN FI-FraDato = IF AVAIL Analyse THEN Analyse.StartDato ELSE ?
         FI-TilDato = IF AVAIL Analyse THEN Analyse.SluttDato ELSE ?
         FI-FraDato:SCREEN-VALUE = STRING(FI-FraDato)
         FI-TilDato:SCREEN-VALUE = STRING(FI-TilDato).
  FOR EACH AnalyseArtikkel WHERE AnalyseArtikkel.AnalyseId = Analyse.AnalyseId NO-LOCK:
      FOR EACH StrekKode WHERE StrekKode.ArtikkelNr = AnalyseArtikkel.ArtikkelNr NO-LOCK:
          ASSIGN cSLPluEan = cSLPluEan + (IF cSLPluEan <> "" THEN "," ELSE "") + TRIM(StrekKode.Kode).
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-TTButiker
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN SkapaTT_Butiker.
    RUN InitCombo.
  RUN enable_UI.
  BROWSE BR-TTButiker:SET-REPOSITIONED-ROW(BROWSE BR-TTButiker:DOWN).
  BROWSE BR-TTValgteButiker:SET-REPOSITIONED-ROW(BROWSE BR-TTValgteButiker:DOWN).
  BROWSE BR-TTButiker:SELECT-FOCUSED-ROW().
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnalyserSummerBong C-Win 
PROCEDURE AnalyserSummerBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE l_BetKort  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE l_Ovr      AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE i_b_id     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dTmpSum    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSumTstTmp AS DECIMAL    NO-UNDO.
  FOR EACH tmpTT_RapLinjePB: 
      DELETE tmpTT_RapLinjePB.
  END.
  ASSIGN l_BetKort = FALSE
         l_Ovr     = FALSE
         dTmpSum   = 0.
  IF BongHode.flBankkort = TRUE OR BongHode.flBetalingskort = TRUE OR BongHode.flKreditkort = TRUE THEN
      ASSIGN l_BetKort = TRUE.
  IF l_BetKort = FALSE THEN
      ASSIGN l_BetKort =
         CAN-FIND(FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.TTId = 58).

  FOR EACH bonglinje WHERE bonglinje.b_id      = bonghode.b_id AND
                           bonglinje.TTId      < 12 AND /* SP har speciell hantering av Makulerad rad  */
                           bonglinje.StrekKode <> "" AND
                           bonglinje.antall    <> 0 NO-LOCK.
/*       IF AVAIL StrekKode AND CAN-FIND(FIRST AnalyseArtikkel WHERE AnalyseArtikkel.AnalyseId = Analyse.AnalyseId AND AnalyseArtikkel.ArtikkelNr = StrekKode.ArtikkelNr) THEN DO: */
      IF CAN-DO(cSLPluEan,TRIM(BongLinje.StrekKode)) THEN DO:
          FIND StrekKode WHERE StrekKode.Kode = TRIM(BongLinje.StrekKode) NO-LOCK NO-ERROR.
          FIND tmpTT_RapLinjePB WHERE tmpTT_RapLinjePB.ArtikkelNr = STRING(strekKode.ArtikkelNr) NO-ERROR.
          IF NOT AVAIL tmpTT_RapLinjePB THEN DO:
              CREATE tmpTT_RapLinjePB.
              ASSIGN tmpTT_RapLinjePB.ArtikkelNr = STRING(strekkode.ArtikkelNr)
                     tmpTT_RapLinjePB.grupp = 1.
          END.
          IF Bonglinje.Antall = ? OR bonglinje.linjesum = ? THEN  
              ASSIGN Bonglinje.Antall   = 0
                     BongLinje.LinjeSum = 0.
          ASSIGN dSumTstTmp = (((bonglinje.linjesum / ABSOLUTE(bonglinje.antall)) * bonglinje.antall)).
          IF dSumTstTmp = ? THEN
              dSumTstTmp = 0.
          ASSIGN tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] = tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] + bonglinje.antall
                 tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] = tmpTT_RapLinjePB.SumFsg[MONTH(bonghode.dato)] + dSumTstTmp
                 dTmpSum = dTmpSum + dSumTstTmp.
      END.
      ELSE DO:
          ASSIGN l_Ovr = TRUE.
          FIND tmpTT_RapLinjePB WHERE tmpTT_RapLinjePB.ArtikkelNr = "Övrigt" NO-ERROR.
          IF NOT AVAIL tmpTT_RapLinjePB THEN DO:
              CREATE tmpTT_RapLinjePB.
              ASSIGN tmpTT_RapLinjePB.ArtikkelNr = "Övrigt"
                     tmpTT_RapLinjePB.grupp     = 9.
          END.
          IF Bonglinje.Antall = ? OR bonglinje.linjesum = ? THEN  
              ASSIGN Bonglinje.Antall   = 0
                     BongLinje.LinjeSum = 0.
          ASSIGN dSumTstTmp = (((bonglinje.linjesum / ABSOLUTE(bonglinje.antall)) * bonglinje.antall)).
           IF dSumTstTmp = ? THEN
              dSumTstTmp = 0.
          ASSIGN tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] = tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)] + bonglinje.antall
                 tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] = tmpTT_RapLinjePB.SumFsg[MONTH(bonghode.dato)] + dSumTstTmp.
      END.
  END.
  /* här hoppar vi av om SL=0 */
  IF dTmpSum = 0 THEN
      RETURN.
  ASSIGN i_b_id = IF l_BetKort = FALSE AND l_Ovr = FALSE THEN 1 ELSE
                  IF l_BetKort = FALSE AND l_Ovr = TRUE  THEN 2 ELSE
                  IF l_BetKort = TRUE  AND l_Ovr = FALSE THEN 3 ELSE 4.
  FIND TT_RapHodePB WHERE TT_RapHodePB.b_id = i_b_id NO-ERROR.
  IF NOT AVAIL TT_RapHodePB THEN DO:
      CREATE TT_RapHodePB.
      ASSIGN TT_RapHodePB.b_id      = i_b_id
             TT_RapHodePB.betalkort = l_BetKort
             TT_RapHodePB.BongType  = IF l_Ovr = TRUE THEN 2 ELSE 1.
  END.
  
  FIND ButTT_RapHodePB WHERE ButTT_RapHodePB.butik = TT_ValgteButiker.butik NO-ERROR.
  IF NOT AVAIL ButTT_RapHodePB THEN DO:
      CREATE ButTT_RapHodePB.
      ASSIGN ButTT_RapHodePB.butik     = TT_ValgteButiker.butik.
  END.
  ASSIGN TT_RapHodePB.antall[MONTH(bonghode.dato)] = TT_RapHodePB.antall[MONTH(bonghode.dato)] + 1
         TT_RapHodePB.belop[MONTH(bonghode.dato)]  = TT_RapHodePB.belop[MONTH(bonghode.dato)] + BongHode.Belop
         ButTT_RapHodePB.antallKont = ButTT_RapHodePB.antallKont + IF i_b_id < 3 THEN 1 ELSE 0
         ButTT_RapHodePB.antallKort = ButTT_RapHodePB.antallKort + IF i_b_id > 2 THEN 1 ELSE 0
         ButTT_RapHodePB.antallMKort[MONTH(bonghode.dato)] = ButTT_RapHodePB.antallMKort[MONTH(bonghode.dato)] + IF i_b_id > 2 THEN 1 ELSE 0.
  FOR EACH tmpTT_RapLinjePB:
      FIND TT_RapLinjePB WHERE TT_RapLinjePB.b_id       = i_b_id  AND
                               TT_RapLinjePB.ArtikkelNr = tmpTT_RapLinjePB.ArtikkelNr NO-ERROR.
      IF NOT AVAIL TT_RapLinjePB THEN DO:
          IF tmpTT_RapLinjePB.grupp = 1 THEN DO:
              FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(tmpTT_RapLinjePB.ArtikkelNr) NO-LOCK NO-ERROR.
          END.
          BUFFER-COPY tmpTT_RapLinjePB EXCEPT b_id TO TT_RapLinjePB
              ASSIGN TT_RapLinjePB.b_id = TT_RapHodePB.b_id
              TT_RapLinjePB.Beskrivelse = IF tmpTT_RapLinjePB.grupp = 1 AND AVAIL ArtBas THEN ArtBas.Beskr ELSE "".
          RELEASE TT_RapLinjePB.
          RELEASE ArtBas.
      END.
      ELSE DO:
          ASSIGN TT_RapLinjePB.antall[MONTH(bonghode.dato)] = TT_RapLinjePB.antall[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)]
                 TT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] = TT_RapLinjePB.SumFsg[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)].
      END.
      IF tmpTT_RapLinjePB.grupp = 1 THEN DO:
          FIND butTT_RapLinjePB WHERE butTT_RapLinjePB.butik = TT_ValgteButiker.butik NO-ERROR.
          IF NOT AVAIL butTT_RapLinjePB THEN DO:
              CREATE butTT_RapLinjePB.
              ASSIGN butTT_RapLinjePB.butik = TT_ValgteButiker.butik.
          END.
          ASSIGN TT_RapHodePB.sumFsgSL[MONTH(bonghode.dato)] = TT_RapHodePB.sumFsgSL[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)]
                 butTT_RapHodePB.sumFsgSLKont = butTT_RapHodePB.sumFsgSLKont + IF i_b_id < 3 THEN tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] ELSE 0
                 butTT_RapHodePB.sumFsgSLKort = butTT_RapHodePB.sumFsgSLKort + IF i_b_id > 2 THEN tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] ELSE 0
                 butTT_RapLinjePB.antall[MONTH(bonghode.dato)] = butTT_RapLinjePB.antall[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.antall[MONTH(bonghode.dato)]
                 butTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)] = butTT_RapLinjePB.SumFsg[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)].
      END.
      ELSE
          ASSIGN TT_RapHodePB.sumOvr[MONTH(bonghode.dato)] = TT_RapHodePB.sumOvr[MONTH(bonghode.dato)] + tmpTT_RapLinjePB.sumFsg[MONTH(bonghode.dato)].
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpAvTabeller C-Win 
PROCEDURE DumpAvTabeller :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcFilNavn AS CHAR NO-UNDO.

MESSAGE "Start"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

  ASSIGN
      pcFilNavn = "tt_RapHode.csv"
      .
  OUTPUT TO VALUE(pcFilNavn) NO-ECHO.
  FOR EACH tt_RapHodePB:
      EXPORT DELIMITER ";"
          tt_RapHodePB.
  END.
  OUTPUT CLOSE.

  ASSIGN
      pcFilNavn = "tt_RapLinje.csv"
      .
  OUTPUT TO VALUE(pcFilNavn) NO-ECHO.
  FOR EACH tt_RapLinjePB:
      EXPORT DELIMITER ";"
          tt_RapLinjePB.
  END.
  OUTPUT CLOSE.

  ASSIGN
      pcFilNavn = "ButTT_RapHode.csv"
      .
  OUTPUT TO VALUE(pcFilNavn) NO-ECHO.
  FOR EACH ButTT_RapHodePB:
      EXPORT DELIMITER ";"
          ButTT_RapHodePB.
  END.
  OUTPUT CLOSE.

  ASSIGN
      pcFilNavn = "ButTT_RapLinje.csv"
      .
  OUTPUT TO VALUE(pcFilNavn) NO-ECHO.
  FOR EACH ButTT_RapLinjePB:
      EXPORT DELIMITER ";"
          ButTT_RapLinjePB.
  END.
  OUTPUT CLOSE.

MESSAGE "STOPP"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY RS-Metode CB-Analyse FI-FraDato FI-TilDato FI-Butiknr S-Bearbetade 
          FI-KasseNr FI-DatoLoop FI-HodeSum FI-LinjeSum FILL-IN-20 FILL-IN-19 
          FILL-IN-18 FILL-IN-21 FILL-IN-24 FILL-IN-22 FILL-IN-26 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-56 RECT-57 BROWSE-TT_RapHodePB B-Summera RS-Metode CB-Analyse 
         FI-FraDato FI-TilDato B-Dump BR-TTButiker BR-TTValgteButiker 
         BUTTON-LeggTil BROWSE-TT_RapLinjePB FI-Butiknr S-Bearbetade FI-KasseNr 
         FI-DatoLoop FI-HodeSum FI-LinjeSum FILL-IN-20 FILL-IN-19 FILL-IN-18 
         FILL-IN-21 FILL-IN-24 FILL-IN-22 FILL-IN-26 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo C-Win 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN cListItemPairs = CB-Analyse:LIST-ITEM-PAIRS.
        FOR EACH Analyse WHERE Analyse.AnalyseType = 1 AND
                               Analyse.Aktiv = TRUE NO-LOCK.
            ASSIGN cListItemPairs = cListItemPairs + "," + Analyse.Navn + "," +
                                            STRING(Analyse.AnalyseId).
        END.
        ASSIGN CB-Analyse:LIST-ITEM-PAIRS = cListItemPairs.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_Butiker C-Win 
PROCEDURE SkapaTT_Butiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Butiker NO-LOCK.
      CREATE TT_Butiker.
      BUFFER-COPY Butiker USING Butik TO TT_Butiker.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilFra C-Win 
PROCEDURE TilFra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wType       AS CHAR NO-UNDO.
    DEFINE VAR             wAntSel     AS INTE NO-UNDO.
    DEFINE VAR             wOpen-Query AS LOGI NO-UNDO.    
    DEFINE VAR             wNum-Sel    AS INTE NO-UNDO.
    DEFINE VAR             wLoop       as INTE NO-UNDO.
    DEFINE VARIABLE        rRowId      AS ROWID      NO-UNDO.
    CASE wType:
        WHEN "Til" THEN DO:
            DO wAntSel = 1 TO BROWSE BR-TTButiker:NUM-SELECTED-ROWS:
                BROWSE BR-TTButiker:FETCH-SELECTED-ROW (wAntsel).
                   CREATE TT_ValgteButiker.
                   BUFFER-COPY TT_Butiker USING Butik TO TT_ValgteButiker.
                   ASSIGN rRowId = ROWID(TT_ValgteButiker).
                   DELETE TT_Butiker.
            END.
            BROWSE BR-TTButiker:DELETE-SELECTED-ROWS().
            {&OPEN-QUERY-BR-TTValgteButiker}
            REPOSITION BR-TTValgteButiker TO ROWID rRowId NO-ERROR.
        END.
        WHEN "Fra" THEN DO:
            IF BROWSE BR-TTValgteButiker:NUM-SELECTED-ROWS > 1 THEN DO:
                SLETT:
                DO wAntSel = 1 TO BROWSE BR-TTValgteButiker:NUM-SELECTED-ROWS:
                    BROWSE BR-TTValgteButiker:FETCH-SELECTED-ROW (wAntSel).
                    CREATE TT_Butiker.
                    BUFFER-COPY TT_ValgteButiker USING Butik TO TT_Butiker.
                    DELETE TT_ValgteButiker.
                    ASSIGN rRowId = ROWID(TT_Butiker).
                END. /* SLETT */
                BROWSE BR-TTValgteButiker:DELETE-SELECTED-ROWS().
            END.
            ELSE DO:
                IF BROWSE BR-TTValgteButiker:NUM-SELECTED-ROWS = 1 THEN DO:
                     CREATE TT_Butiker.
                     BUFFER-COPY TT_ValgteButiker USING Butik TO TT_Butiker.
                     DELETE TT_ValgteButiker.
                     BROWSE BR-TTValgteButiker:DELETE-SELECTED-ROWS().
                     ASSIGN rRowId = ROWID(TT_Butiker).
                END.
                wOpen-Query = YES.
            END.
        END.
        WHEN "AlleFra" THEN DO:
            FOR EACH TT_ValgteButiker:
                CREATE TT_Butiker.
                BUFFER-COPY TT_ValgteButiker USING Butik TO TT_Butiker.
                DELETE TT_ValgteButiker.
            END.
            {&OPEN-QUERY-BR-TTValgteButiker}
            wOpen-Query = YES.
       END.
    END CASE.
    IF wOpen-Query THEN DO:
        {&OPEN-QUERY-BR-TTButiker}
        IF rRowId <> ? THEN
            REPOSITION BR-TTButiker TO ROWID rRowId NO-ERROR.
/*         REPOSITION BROWSE-Tag TO ROW 1 NO-ERROR. */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

