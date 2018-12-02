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
  STEP 1: Välj tabell i browsern
  STEP 2: Definiera de fælt du ønskar i din browser
          Kryssa av enabled på de fælt du ønskar sortering
  STEP 3: Gør de andringar som behøvs i alla scope.
          Sorttype skall ha lika många entries som antal enablade fält.
              - Tillåtna värden = "" (blank -> BY = default)
              - 

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
&scope RePos       RePos
&scope br-tabell   TransLogg
&scope Sorttype    ,,,,,,
&scope BrowseIdx   BatchLogg,Dato,Kode,BongTekst,Vg,ArtButStr,Antal
&scope Sokvillkor  >=,>=,>=,>=,>=,>=,>=
&scope InitIdx     BatchLogg
&scope ip-felt     BatchNr

/* Om du önskar input parameter. Ger en startup-record */ 
&scope ip-variabel w{&ip-felt}

/*
&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.BestNr) ~
                                                else "".
*/                                                

&scope QWhere      'TransLogg.BatchNr = ' + STRING({&ip-variabel})
&scope BrowseQ     FOR EACH {&br-tabell} NO-LOCK XWHERE XSORTTYPE XSORT INDEXED-REPOSITION
&scope BrowseSQ    FOR EACH b{&br-tabell} NO-LOCK WHERE XWHERE b{&br-tabell}.XFIELD XSOKV XFILL ~
                           USE-INDEX XIDX MAX-ROWS 1

/* Parameter Definisjoner ---                                           */
&IF LENGTH("{&ip-variabel}") > 0 &THEN

  &scope return-ip   if available {&br-tabell} then ASSIGN {&ip-variabel} = {&br-tabell}.{&ip-felt}
  &scope init-phrase FIND b{&br-tabell} WHERE b{&br-tabell}.{&ip-felt} = ~
                        {&ip-variabel} USE-INDEX {&InitIdx} NO-LOCK NO-ERROR.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} INIT 6 NO-UNDO.
    def    var wTitle   as   char              no-undo.
    assign
      wBAtchNr = 8
      wTitle   = "TEST TEST TEST Batch 8".
  &ELSE
    DEFINE INPUT-OUTPUT PARAMETER {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} NO-UNDO.
    def    input        parameter wTitle   as   char              no-undo.
  &ENDIF

&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.
DEFINE QUERY wSQ FOR b{&br-tabell} SCROLLING.
define temp-table tmpChild
  field wChild as handle.

/* Lokale variabler ---                                                 */

DEFINE VAR retur-verdi       AS CHAR INIT "AVBRYT" NO-UNDO.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wSearchCols       AS CHAR       NO-UNDO.
DEFINE VAR wSearchColsH      AS WIDGET EXTENT 10 NO-UNDO.
DEFINE VAR wQ                AS WIDGET      NO-UNDO.
DEFINE VAR wSortCol          AS WIDGET      NO-UNDO.
DEFINE VAR wAntSortCol       AS INTE        NO-UNDO.
DEFINE VAR wAktivFillIn      AS WIDGET      NO-UNDO.
DEFINE VAR wSorttype         AS CHAR   INIT "{&Sorttype}"   NO-UNDO.
DEFINE VAR wSokvillkor       AS CHAR   INIT "{&Sokvillkor}" NO-UNDO.
DEFINE VAR wBrowseIdx        AS CHAR   INIT "{&BrowseIdx}"  NO-UNDO.
define var wOk               as log         no-undo.
define var wAlle             as char        no-undo.
define var wAktivQString     as char        no-undo.
define var wBekreft          as log         no-undo.
DEFINE VAR wBlank AS LOG     NO-UNDO.
def    var wKlokken             as char format "x(8)"    no-undo.
def    var wTransType           as char format "x(15)"   no-undo.
DEF    VAR wDummy               AS INT                   NO-UNDO.
DEF    VAR cFeilmelding         AS CHAR                  NO-UNDO.
DEF    VAR lNettoPris       LIKE TransLogg.Pris          NO-UNDO.

DEF VAR wwBatchNr             AS INT         NO-UNDO.

DEF TEMP-TABLE TransRecid
    FIELD TRecid AS RECID.

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
TransLogg.TransNr wTransType TransLogg.TBId TransLogg.Dato ~
TransLogg.ArtikkelNr TransLogg.Kode TransLogg.BongTekst TransLogg.Vg ~
TransLogg.LopNr TransLogg.Storl TransLogg.Antall TransLogg.Pris ~
TransLogg.RabKr TransLogg.Mva ~
(TransLogg.Pris - Translogg.RabKr) @ lNettoPris TransLogg.VVarekost ~
TransLogg.NegLager TransLogg.Postert TransLogg.PostertDato ~
TransLogg.FeilKode TransLogg.OvButik TransLogg.TilStorl TransLogg.Plukket ~
TransLogg.KassaNr TransLogg.BongId TransLogg.BongLinjeNr TransLogg.BestNr ~
cFeilmelding TransLogg.SelgerNr TransLogg.ForsNr TransLogg.KundNr ~
TransLogg.MedlemsNr TransLogg.KortType TransLogg.KortNr TransLogg.LevNr ~
TransLogg.OvTransNr TransLogg.SattVVareKost TransLogg.RefNr ~
TransLogg.RefTekst TransLogg.individnr TransLogg.KalkylePris TransLogg.Mva% ~
TransLogg.Varekost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TransLogg TransLogg.Butik ~
TransLogg.Dato TransLogg.ArtikkelNr TransLogg.Kode TransLogg.BongTekst ~
TransLogg.Vg TransLogg.Antall 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-TransLogg TransLogg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-TransLogg TransLogg
&Scoped-define QUERY-STRING-BROWSE-TransLogg FOR EACH TransLogg ~
      WHERE TransLogg.BatchNr = wBatchNr NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TransLogg OPEN QUERY BROWSE-TransLogg FOR EACH TransLogg ~
      WHERE TransLogg.BatchNr = wBatchNr NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TransLogg TransLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TransLogg TransLogg


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 RECT-51 RECT-52 FILL-IN-SOK-INTE ~
FILL-IN-SOK-CHAR FILL-IN-SOK-DECI FILL-IN-SOK-DATE B-KorrTrans ~
BUTTON-Motposter Btn_Help Btn_OK BUTTON-Sok B-VisTrans-2 CB-TTId T-VisAlle ~
BROWSE-TransLogg 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-INTE FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DECI FILL-IN-SOK-DATE CB-TTId T-VisAlle 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Klokken C-Win 
FUNCTION Klokken RETURNS CHARACTER
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Prep-Private-Data C-Win 
FUNCTION Prep-Private-Data RETURNS CHARACTER
  ( INPUT wQueryCol AS WIDGET,INPUT wQueryCol# AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-KorrTrans 
     LABEL "&Sett artikkelnummmer" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-VisTrans 
     LABEL "&Vis transaksjon" 
     SIZE 19 BY 1.14.

DEFINE BUTTON B-VisTrans-2 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "icon/e-exit":U
     LABEL "OK" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Motposter 
     LABEL "&Motposter" 
     SIZE 16 BY 1.1.

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1 TOOLTIP "Søk i AKTIV kollonne".

DEFINE VARIABLE CB-TTId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DECI AS DECIMAL FORMAT ">>>>>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INTE AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY .1.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY .1.

DEFINE VARIABLE T-VisAlle AS LOGICAL INITIAL no 
     LABEL "&Vis alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TransLogg FOR 
      TransLogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TransLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TransLogg C-Win _STRUCTURED
  QUERY BROWSE-TransLogg NO-LOCK DISPLAY
      TransLogg.Butik FORMAT ">>>>>9":U
      TransLogg.TransNr COLUMN-LABEL "TransNr" FORMAT "zz,zzz,zz9":U
      wTransType COLUMN-LABEL "TransType" FORMAT "x(15)":U
      TransLogg.TBId COLUMN-LABEL "TBId" FORMAT ">>9":U
      TransLogg.Dato COLUMN-LABEL "Dato" FORMAT "99/99/9999":U
      TransLogg.ArtikkelNr COLUMN-LABEL "Artikkelnr" FORMAT "zzzzzzzzzzzz9":U
      TransLogg.Kode FORMAT "X(20)":U
      TransLogg.BongTekst FORMAT "X(30)":U
      TransLogg.Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U
      TransLogg.LopNr COLUMN-LABEL "LpNr" FORMAT ">>zzz9":U
      TransLogg.Storl COLUMN-LABEL "Str" FORMAT "x(6)":U WIDTH 8
      TransLogg.Antall FORMAT "-zz,zzz,zz9.999":U
      TransLogg.Pris COLUMN-LABEL "Brutto Pris" FORMAT "-zz,zzz,zz9.99":U
      TransLogg.RabKr FORMAT "->,>>>,>>9.99":U
      TransLogg.Mva FORMAT "->,>>>,>>9.99":U
      (TransLogg.Pris - Translogg.RabKr) @ lNettoPris COLUMN-LABEL "Pris" FORMAT "-zz,zzz,zz9.99":U
            WIDTH 14.4
      TransLogg.VVarekost FORMAT "-z,zzz,zz9.99":U
      TransLogg.NegLager FORMAT "9":U
      TransLogg.Postert COLUMN-LABEL "Post" FORMAT "Ja/Nei":U
      TransLogg.PostertDato COLUMN-LABEL "PosttDato" FORMAT "99/99/9999":U
      TransLogg.FeilKode COLUMN-LABEL "Feil" FORMAT "zzz9":U
      TransLogg.OvButik COLUMN-LABEL "TilBut" FORMAT ">>>>>9":U
      TransLogg.TilStorl FORMAT "x(10)":U
      TransLogg.Plukket FORMAT "Ja/Nei":U
      TransLogg.KassaNr COLUMN-LABEL "Kasse" FORMAT "zzz9":U
      TransLogg.BongId FORMAT "zz,zzz,zz9":U
      TransLogg.BongLinjeNr FORMAT "zzzzz9":U
      TransLogg.BestNr FORMAT ">>>>>>>9":U
      cFeilmelding FORMAT "x(40)":U
      TransLogg.SelgerNr FORMAT ">>>>>>>>>>>>9":U
      TransLogg.ForsNr FORMAT ">>>>>9":U
      TransLogg.KundNr FORMAT ">>>>>>>>>>>>9":U
      TransLogg.MedlemsNr FORMAT ">>>>>>>>>>>>9":U
      TransLogg.KortType FORMAT "9":U
      TransLogg.KortNr FORMAT "X(22)":U
      TransLogg.LevNr COLUMN-LABEL "LevNr" FORMAT "zzzzz9":U
      TransLogg.OvTransNr FORMAT "zz,zzz,zz9":U
      TransLogg.SattVVareKost FORMAT "yes/no":U
      TransLogg.RefNr FORMAT "->,>>>,>>9":U
      TransLogg.RefTekst FORMAT "X(40)":U
      TransLogg.individnr FORMAT ">>>>>>>>>>>9":U
      TransLogg.KalkylePris FORMAT "->>,>>>,>>9.99":U
      TransLogg.Mva% FORMAT "->>9.99":U
      TransLogg.Varekost FORMAT "->>,>>9.99":U
  ENABLE
      TransLogg.Butik
      TransLogg.Dato
      TransLogg.ArtikkelNr
      TransLogg.Kode
      TransLogg.BongTekst
      TransLogg.Vg
      TransLogg.Antall
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN MULTIPLE SIZE 155 BY 22.14 ROW-HEIGHT-CHARS .63 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-SOK-INTE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DECI AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     B-VisTrans AT ROW 1.48 COL 37.4
     B-KorrTrans AT ROW 1.48 COL 56.4
     BUTTON-Motposter AT ROW 1.48 COL 130.6
     Btn_Help AT ROW 1.48 COL 147
     Btn_OK AT ROW 1.48 COL 152
     BUTTON-Sok AT ROW 1.52 COL 21.4
     B-VisTrans-2 AT ROW 1.52 COL 32.4
     CB-TTId AT ROW 1.52 COL 77.4 COLON-ALIGNED NO-LABEL
     T-VisAlle AT ROW 1.67 COL 117.4
     BROWSE-TransLogg AT ROW 3.14 COL 2
     RECT-50 AT ROW 1 COL 1
     RECT-51 AT ROW 1.24 COL 1
     RECT-52 AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157 BY 24.62.


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
         TITLE              = "Søkeliste transaksjonslogg"
         HEIGHT             = 24.62
         WIDTH              = 157
         MAX-HEIGHT         = 39.19
         MAX-WIDTH          = 230.4
         VIRTUAL-HEIGHT     = 39.19
         VIRTUAL-WIDTH      = 230.4
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-TransLogg T-VisAlle DEFAULT-FRAME */
/* SETTINGS FOR BUTTON B-VisTrans IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-TransLogg:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 6
       BROWSE-TransLogg:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 481
       BROWSE-TransLogg:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       TransLogg.Dato:COLUMN-READ-ONLY IN BROWSE BROWSE-TransLogg = TRUE
       TransLogg.Kode:COLUMN-READ-ONLY IN BROWSE BROWSE-TransLogg = TRUE
       TransLogg.BongTekst:COLUMN-READ-ONLY IN BROWSE BROWSE-TransLogg = TRUE
       TransLogg.Vg:COLUMN-READ-ONLY IN BROWSE BROWSE-TransLogg = TRUE
       TransLogg.Antall:COLUMN-READ-ONLY IN BROWSE BROWSE-TransLogg = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TransLogg
/* Query rebuild information for BROWSE BROWSE-TransLogg
     _TblList          = "SkoTex.TransLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "TransLogg.BatchNr = wBatchNr"
     _FldNameList[1]   > SkoTex.TransLogg.Butik
"Butik" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > SkoTex.TransLogg.TransNr
"TransNr" "TransNr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"wTransType" "TransType" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.TransLogg.TBId
"TBId" "TBId" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > SkoTex.TransLogg.Dato
"Dato" "Dato" ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > SkoTex.TransLogg.ArtikkelNr
"ArtikkelNr" "Artikkelnr" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > SkoTex.TransLogg.Kode
"Kode" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > SkoTex.TransLogg.BongTekst
"BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > SkoTex.TransLogg.Vg
"Vg" "Vg" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > SkoTex.TransLogg.LopNr
"LopNr" "LpNr" ">>zzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > SkoTex.TransLogg.Storl
"Storl" "Str" "x(6)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > SkoTex.TransLogg.Antall
"Antall" ? "-zz,zzz,zz9.999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > SkoTex.TransLogg.Pris
"Pris" "Brutto Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = SkoTex.TransLogg.RabKr
     _FldNameList[15]   = SkoTex.TransLogg.Mva
     _FldNameList[16]   > "_<CALC>"
"(TransLogg.Pris - Translogg.RabKr) @ lNettoPris" "Pris" "-zz,zzz,zz9.99" ? ? ? ? ? ? ? no ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = SkoTex.TransLogg.VVarekost
     _FldNameList[18]   = SkoTex.TransLogg.NegLager
     _FldNameList[19]   > SkoTex.TransLogg.Postert
"Postert" "Post" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > SkoTex.TransLogg.PostertDato
"PostertDato" "PosttDato" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > SkoTex.TransLogg.FeilKode
"FeilKode" "Feil" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > SkoTex.TransLogg.OvButik
"OvButik" "TilBut" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   = SkoTex.TransLogg.TilStorl
     _FldNameList[24]   = SkoTex.TransLogg.Plukket
     _FldNameList[25]   > SkoTex.TransLogg.KassaNr
"KassaNr" "Kasse" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   = SkoTex.TransLogg.BongId
     _FldNameList[27]   = SkoTex.TransLogg.BongLinjeNr
     _FldNameList[28]   = SkoTex.TransLogg.BestNr
     _FldNameList[29]   > "_<CALC>"
"cFeilmelding" ? "x(40)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   = SkoTex.TransLogg.SelgerNr
     _FldNameList[31]   = SkoTex.TransLogg.ForsNr
     _FldNameList[32]   = SkoTex.TransLogg.KundNr
     _FldNameList[33]   = SkoTex.TransLogg.MedlemsNr
     _FldNameList[34]   = SkoTex.TransLogg.KortType
     _FldNameList[35]   = SkoTex.TransLogg.KortNr
     _FldNameList[36]   > SkoTex.TransLogg.LevNr
"LevNr" "LevNr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   = SkoTex.TransLogg.OvTransNr
     _FldNameList[38]   = SkoTex.TransLogg.SattVVareKost
     _FldNameList[39]   = SkoTex.TransLogg.RefNr
     _FldNameList[40]   = SkoTex.TransLogg.RefTekst
     _FldNameList[41]   = SkoTex.TransLogg.individnr
     _FldNameList[42]   = SkoTex.TransLogg.KalkylePris
     _FldNameList[43]   = SkoTex.TransLogg.Mva%
     _FldNameList[44]   = SkoTex.TransLogg.Varekost
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-TransLogg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søkeliste transaksjonslogg */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søkeliste transaksjonslogg */
DO:
  if can-find(first tmpChild where
               valid-handle(tmpChild.wChild)) then
    do:
      wBekreft = false.
      message 'Det er startet andre programmer fra dette vinduet.' skip
              'Avsluttes dette vinduet, vil alle underliggende programmer' skip
              'også bli avsluttet.'
              view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
              update wBekreft
              .
    end.
  else wBekreft = true.
  if wBekreft <> true then
  return no-apply.
                       
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KorrTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KorrTrans C-Win
ON CHOOSE OF B-KorrTrans IN FRAME DEFAULT-FRAME /* Sett artikkelnummmer */
DO:
  if not available TransLogg then
    return no-apply.
  RUN settArtikkel.  
  browse-Translogg:REFRESH().
  /* Kall til rutine for visning av transaksjon. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans C-Win
ON CHOOSE OF B-VisTrans IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
  if not available TransLogg then
    return no-apply.
    
  /* Kall til rutine for visning av transaksjon. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans-2 C-Win
ON CHOOSE OF B-VisTrans-2 IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
    IF AVAIL Translogg THEN DO:
        RUN gviskvittokopi.w (Translogg.Butik,1,Translogg.Kassanr,Translogg.dato,Translogg.BongId).
    END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TransLogg
&Scoped-define SELF-NAME BROWSE-TransLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON ANY-PRINTABLE OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON CURSOR-LEFT OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON CURSOR-RIGHT OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON DEFAULT-ACTION OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  
  APPLY "CHOOSE" TO B-KorrTrans.
  RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON END-MOVE OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  message "gurre var her " view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON HOME OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  APPLY "ENTRY" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON ROW-DISPLAY OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  cFeilmelding = "".
  if available TransLogg then
    do:
      find TransType no-lock of 
        TransLogg no-error.
      if TransLogg.Postert = false then
        assign        
          TransLogg.Postert:bgcolor in browse BROWSE-TransLogg = 12.   
      IF TransLogg.FeilKode <> 0 THEN
      DO:
          {syspara.i 100 1 TransLogg.FeilKode cFeilmelding}
      END.
    end.
  assign
    wTransType = if available TransType 
                   then TransType.Beskrivelse
                   else ""
    /*wKlokken = Klokken()*/.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON START-SEARCH OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  RUN lockwindowupdate(FRAME {&FRAME-NAME}:HWND).
  DEF VAR wSearchCol  AS WIDGET NO-UNDO.
  DEF VAR wQString    AS CHAR   NO-UNDO.
  DEF VAR wSortColIdx AS INTE   NO-UNDO.
  ASSIGN wSearchCol = SELF:CURRENT-COLUMN.
  IF wSortCol <> SELF:CURRENT-COLUMN AND
                   LOOKUP(wSearchCol:NAME,wSearchCols) > 0 THEN DO:
      ASSIGN wSortCol = SELF:CURRENT-COLUMN.
      RUN SortNyCol.
  END.
  ELSE IF LOOKUP(wSearchCol:NAME,wSearchCols) > 0 AND
          LOOKUP("USE-INDEX",wQ:PREPARE-STRING," ") = 0 THEN DO:
      ASSIGN wQString = wQ:PREPARE-STRING
             wSortColIdx = LOOKUP("{&br-tabell}." + wSearchCol:NAME,wQString," ")
             wQString = IF ENTRY(wSortColIdx + 1,wQString," ") = "DESCENDING" THEN
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME + " DESCENDING","{&br-tabell}." + wSearchCol:NAME)
                        ELSE
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME, 
                           "{&br-tabell}." + wSearchCol:NAME + " DESCENDING")
             wSearchCol:PRIVATE-DATA = wQString.
      wQ:QUERY-PREPARE(wQString).
      FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK.

      wQ:QUERY-OPEN().
      RUN SD-Reposition.
  END.
  APPLY "LEAVE" TO SELF. /* annars fungerar inte "ENTRY" ?? */
  APPLY "ENTRY" TO SELF.
  APPLY "END-SEARCH" TO SELF.
  RUN lockwindowupdate(0).
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  if can-find(first tmpChild where
               valid-handle(tmpChild.wChild)) then
    do:
      wBekreft = false.
      message 'Det er startet andre programmer fra dette vinduet.' skip
              'Avsluttes dette vinduet, vil alle underliggende programmer' skip
              'også bli avsluttet.'
              view-as alert-box warning buttons yes-no title 'Bekreft avsluttning'
              update wBekreft
              .
    end.
  else wBekreft = true.
  if wBekreft <> true then
  return no-apply.
                     
  {&return-ip}
  .
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
      .
  &ELSE
      ASSIGN retur-verdi = "OK".
  &ENDIF
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Motposter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Motposter C-Win
ON CHOOSE OF BUTTON-Motposter IN FRAME DEFAULT-FRAME /* Motposter */
DO:
  ASSIGN
    wDummy = IF INPUT CB-TTID = wAlle 
               THEN ?
               ELSE INT(ENTRY(1,INPUT CB-TTID,":")).
  RUN MotposterTranser (wDummy).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ELSE DO:
      MESSAGE "Motpostering utført (Batch " + STRING(wBatchNr) + ")."
          VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok C-Win
ON CHOOSE OF BUTTON-Sok IN FRAME DEFAULT-FRAME /* Søk */
DO:
   /* DEFINE VAR wBlank AS LOG  NO-UNDO.*/
   DEFINE VAR wSQStr AS CHAR NO-UNDO. 
   DEFINE VAR wChar AS  CHAR NO-UNDO.
   DEFINE VAR wXSokv AS CHAR NO-UNDO.
   DEFINE VAR wQWhere    AS CHAR NO-UNDO. 
   
   wBlank = false.
   
   IF wAktivFillIn:SCREEN-VALUE = "" THEN
       wBlank = TRUE.
   ELSE DO:
       &IF LENGTH("{&QWhere}") > 1 &THEN
           ASSIGN wQWhere = " " + {&QWhere} + " AND".
                  wQWhere = IF LOOKUP(" {&br-tabell}",wQWhere,".") = 0 THEN wQWhere
                            ELSE REPLACE(wQWhere," {&br-tabell}"," b{&br-tabell}").
       &ENDIF
       ASSIGN wChar  = IF wAktivFillIn:DATA-TYPE BEGINS "CHAR" THEN '"' ELSE ''
              wSQStr = REPLACE("{&BrowseSQ}","XFIELD",ENTRY(wAktivCol,wSearchCols))
              wSQStr = REPLACE(wSQStr,"XSOKV",ENTRY(wAktivCol,wSokvillkor))
              wSQStr = IF ENTRY(wAktivCol,wBrowseIdx) <> "" THEN
                         REPLACE(wSQStr,"XIDX",ENTRY(wAktivCol,wBrowseIdx)) 
                       ELSE 
                           REPLACE(wSQStr,"USE-INDEX XIDX ","") 
              wSQStr = REPLACE(wSQStr,"XFILL",wChar + wAktivFillIn:SCREEN-VALUE + wChar)
              wSQStr = REPLACE(wSQStr,"XWHERE",wQWHere).
       QUERY wSQ:QUERY-PREPARE(wSQStr).
       QUERY wSQ:QUERY-OPEN().
       GET FIRST wSQ.
       IF NOT AVAIL b{&br-tabell} THEN DO:
           APPLY "ENTRY" TO wAktivFillIn.
           RETURN NO-APPLY.         
       END.
   END.

   IF wBlank THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
   ELSE do:
     wBlank = ?.
     RUN SD-Reposition.
     wBlank = false.
   end.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId C-Win
ON VALUE-CHANGED OF CB-TTId IN FRAME DEFAULT-FRAME
DO:
  assign frame DEFAULT-FRAME
    CB-TTId.
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-CHAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-CHAR C-Win
ON RETURN OF FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-DATE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE C-Win
ON LEAVE OF FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR wDate AS DATE.
    ASSIGN wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feilaktigt tastet dato." VIEW-AS ALERT-BOX ERROR TITLE "Feil dato".
        APPLY "ENTRY" TO FILL-IN-SOK-DATE.
        RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE C-Win
ON RETURN OF FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR wDate AS DATE.
    wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feilaktigt tastet dato." VIEW-AS ALERT-BOX ERROR TITLE "Feil dato".
        APPLY "ENTRY" TO FILL-IN-SOK-DATE.
        RETURN NO-APPLY.
    END.
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-DECI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DECI C-Win
ON RETURN OF FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-INTE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-INTE C-Win
ON RETURN OF FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-VisAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-VisAlle C-Win
ON VALUE-CHANGED OF T-VisAlle IN FRAME DEFAULT-FRAME /* Vis alle */
DO:
  assign frame DEFAULT-FRAME
    T-VisAlle.
  RUN SD-CURSOR (" ").
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

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Søkeliste VinduTransLogg"
  &PreIClose      = "RUN SaveBrowseSettings."
  &PostIClose     = " "
  &PostDisable_ui = "for each tmpChild:
                       if valid-handle(tmpChild.wChild) then
                         delete procedure tmpChild.wChild.
                     end."
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 1 100 1 wAlle}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN InitVars. /* inkl open-query */
    IF RETURN-VALUE = "FEIL" THEN
       RETURN.
/*    IF "Komnavn" <> wSortCol:NAME THEN
        RUN QueryCustomSettings ("Komnavn"). */
    run InitTransType.

    /*
    RUN SD-QUERY-OPEN.

    IF RETURN-VALUE = "FEIL" THEN
        LEAVE MAIN-BLOCK.
    */

    assign
      {&WINDOW-NAME}:title = "Transaksjoner tilhørende batch " +
                             string(wBatchNr) + " " + 
                             wTitle.
    RUN enable_UI.
    {lng.i} 

    {browsesettings.i {&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:CURRENT-COLUMN <> wSortCol THEN DO:
        wSortCol = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
        RUN QueryCustomSettings (wSortCol:NAME).
    END.

    RUN LabelColor.
    RUN Move-Fill-To-Top. 

    T-VisAlle = FALSE.
    RUN SD-CURSOR (" ").

    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    &ENDIF 
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END. 
    ELSE IF AVAILABLE {&br-tabell} THEN
       REPOSITION {&BROWSE-NAME} TO ROW 1.
    ASSIGN
      C-Win:HIDDEN = FALSE.
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
END PROCEDURE.

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
  DISPLAY FILL-IN-SOK-INTE FILL-IN-SOK-CHAR FILL-IN-SOK-DECI FILL-IN-SOK-DATE 
          CB-TTId T-VisAlle 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-50 RECT-51 RECT-52 FILL-IN-SOK-INTE FILL-IN-SOK-CHAR 
         FILL-IN-SOK-DECI FILL-IN-SOK-DATE B-KorrTrans BUTTON-Motposter 
         Btn_Help Btn_OK BUTTON-Sok B-VisTrans-2 CB-TTId T-VisAlle 
         BROWSE-TransLogg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
  assign
    CB-TTId:list-items   in frame DEFAULT-FRAME = wAlle
    CB-TTId:screen-value in frame DEFAULT-FRAME = wAlle.

  for each TransType no-lock:
    assign
     CB-TTId:list-items in frame DEFAULT-FRAME = 
                   CB-TTId:list-items in frame DEFAULT-FRAME +
                   (if CB-TTId:list-items in frame DEFAULT-FRAME = ""
                      then ""
                      else ",") +
                   string(TransType.TTId,"zz9") + ": " + TransType.Beskrivelse.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVars C-Win 
PROCEDURE InitVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wW     AS WIDGET NO-UNDO.
    DEF VAR wCount AS INTE   NO-UNDO.

    ASSIGN wAntSortCol = NUM-ENTRIES("{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," ")
           wAktivCol   = IF wAktivCol > wAntSortCol THEN 1 ELSE wAktivCol.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
    DEF VAR wIdx  AS INTE NO-UNDO.
    DEF VAR wNumE AS INTE NO-UNDO.
    IF wAntSortCol < 1 THEN DO:
        MESSAGE "Du må 'enabla' minst ett felt" view-as alert-box.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSorttype) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sorttype skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," skip
                " kommaseparert med valgfritt BY, BY DESCENDING eller USE-INDEX." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSokvillkor) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sokvillkor skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," skip
                " kommaseparert med valgfritt <=,>= eller =." SKIP
                "Aktuellt värde: " + wSokvillkor
                 VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    DO wIdx = 1 TO wAntSortCol:
        CASE ENTRY(wIdx,wSorttype):
            WHEN "" OR WHEN "BY" OR WHEN "BY DESCENDING" OR WHEN "USE-INDEX" THEN.
            OTHERWISE DO:
                          MESSAGE "Tillåtna entries i 'scope Sort' = ''(tomt),BY,BY DESCENDING OCH USE-INDEX"
                                 VIEW-AS ALERT-BOX ERROR.
                          RETURN "FEIL".
                      END.
        END CASE.
    END.
    ASSIGN wNumE = NUM-ENTRIES(wBrowseIdx).
    IF NOT CAN-DO(wBrowseIdx,"USE-INDEX") AND NUM-ENTRIES(wBrowseIdx) <> wAntSortCol THEN DO:
        MESSAGE "Antall entries i 'scope BrowseIdx' <>" wAntSortCol VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    DO wIdx = 1 TO wAntSortCol:
        IF ENTRY(wIdx,wSorttype) = "USE-INDEX" AND
                (wIdx > wNumE OR ENTRY(wIdx,"{&BrowseIdx}") = "") THEN DO:
            MESSAGE "Entry " wIdx " av 'scope Sorttype' = USE-INDEX och" SKIP
                    "entry " wIdx " saknas i 'scope BrowseIdx'" VIEW-AS ALERT-BOX ERROR.
            RETURN "FEIL".
        END.
    END.
  &ENDIF
    DO wCount = 1 TO wAntSortCol:
        ASSIGN wSearchCols = IF wSearchCols = "" THEN 
               ENTRY(NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),".")
                                                ELSE
               wSearchCols + "," + 
                   ENTRY(NUM-ENTRIES(ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),"."),ENTRY(wCount,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," "),".").
    END.
    ASSIGN wW        = BROWSE {&BROWSE-NAME}:FIRST-COLUMN
           wOrgBgCol = wW:Label-bgcolor
           wCount = 1.
    REPEAT WHILE VALID-HANDLE(wW):
        IF LOOKUP(wW:NAME,wSearchCols) > 0 THEN
            ASSIGN wW:PRIVATE-DATA      = PREP-PRIVATE-DATA(wW:HANDLE,wCount)
                   wSearchColsH[wCount] = wW:HANDLE
                   wW:LABEL = wW:LABEL + STRING(LOOKUP("*",wW:LABEL," ") = 0," */")
                   wW:LABEL = wW:LABEL + IF ENTRY(wCount,wSortType) = "USE-INDEX" THEN
                                "" ELSE "*"
                   wCount               = wCount + 1
                   wW:READ-ONLY         = YES.
        ASSIGN wW = wW:NEXT-COLUMN.
    END.
    ASSIGN BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSearchColsH[wAktivCol]
           wSortCol                             = wSearchColsH[wAktivCol]
           wQ                                   = BROWSE {&BROWSE-NAME}:QUERY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LabelColor C-Win 
PROCEDURE LabelColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wColorCol AS WIDGET NO-UNDO.
  ASSIGN wColorCol = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
  REPEAT WHILE VALID-HANDLE(wColorCol):
      IF LOOKUP("{&br-tabell}." + wColorCol:NAME,"{&ENABLED-FIELDS-IN-QUERY-{&BROWSE-NAME}}"," ") > 0 THEN
          ASSIGN wColorCol:LABEL-BGCOLOR = 
            IF wColorCol:NAME = wSortCol:NAME THEN wSortBgCol ELSE wOrgBgCol.
      ASSIGN wColorCol = wColorCol:NEXT-COLUMN.
  END.
   RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MotposterTranser C-Win 
PROCEDURE MotposterTranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wTTId LIKE TransLogg.TTId NO-UNDO.

def var wSvar          as log               no-undo.
def var wLoop          as int               no-undo.
DEF VAR wTittel        as CHAR              NO-UNDO.
DEF VAR wAlleTranser   AS LOG INITIAL FALSE NO-UNDO.
DEF VAR wTransNr       as INT INITIAL 0     NO-UNDO.
DEF VAR wPwd           AS CHAR              NO-UNDO.
DEF VAR wTekst         AS CHAR              NO-UNDO.

DEF BUFFER bTransLogg FOR TransLogg.
DEF BUFFER qTransLogg FOR TransLogg.

/* Setter Pwd. */
{syspara.i 2 3 1 wPwd}
IF wPwd = "" THEN
  ASSIGN
    wPwd = "MASTER".

/* Nullstiller temp-file */
FOR EACH TransRecid:
    DELETE TransRecid.
END.

/* Setter frameScoop */
DO with FRAME DEFAULT-FRAME:
/* Alle transer som er merket skal motposteres */
if BROWSE-TransLogg:NUM-SELECTED-ROWS > 0 then
  do:
    wSvar = false.
    message "Skal de merkede transaksjonene motposteres?"
            view-as alert-box question buttons yes-no title "Bekreft motpostering"
            update wSvar.
    if wSvar = false then 
      return NO-APPLY "AVBRYT".
    ASSIGN 
      wAlleTranser = FALSE.
  END.
/* Alle transer i browser skal motposteres. */
else do: 
  message "Skal ALLE transaksjonene i listen motposteres?"
          view-as alert-box question buttons yes-no title "Bekreft motpostering"
          update wSvar.
  if wSvar = false then 
    return no-apply "AVBRYT".
  ASSIGN 
      wAlleTranser = TRUE.
END.

/* Adgangskontroll */
RUN d-adgangskontroll.d (INPUT "Adgangskontroll motpostering", INPUT wPwd).
IF RETURN-VALUE <> "OK" THEN
    RETURN "AVBRYT".

/* Tittel som batchen med motposteringene merkes med. */
assign
  wTittel = "Motpostering: " + STRING(TODAY) + " " +
            STRING(time,"HH:MM") + " " + USERID("dictdb").

/* Henter ledig BatchNr */
run batchlogg.p (program-name(1),
                 wTittel,
                 output wwBatchnr).
{sww.i}
IF wAlleTranser = FALSE THEN DO TRANSACTION:

  MOTPOSTER_LOOP:
  do wLoop = BROWSE-TransLogg:NUM-SELECTED-ROWS to 1 by -1:
    wOk = BROWSE-TransLogg:FETCH-SELECTED-ROW(wLoop).

    FIND TransRecid WHERE
        TransRecid.TRecid = RECID(TransLogg) NO-ERROR.
    IF NOT AVAILABLE TransRecid THEN
    DO:
        CREATE TransRecid.
        ASSIGN
            TransRecid.TRecid = RECID(TransLogg).
    END.
  END. /* MOTPOSTER_LOOP */
  wOk = BROWSE-TransLogg:REFRESH( ).
END.
ELSE do TRANSACTION:
  /* Alle transaksjoner */
  IF wTTID = ? THEN
    for each qTransLogg exclusive-lock where
      qTransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
      FIND TransRecid WHERE
          TransRecid.TRecid = RECID(qTransLogg) NO-ERROR.
      IF NOT AVAILABLE TransRecid THEN
      DO:
          CREATE TransRecid.
          ASSIGN
              TransRecid.TRecid = RECID(qTransLogg).
      END.
    end.  
  /* Kun en type transaksjoner */
  ELSE FOR EACH qTransLogg EXCLUSIVE-LOCK WHERE
    qTransLogg.TTID       = wTTID AND
    qTransLogg.ArtikkelNr = ArtBas.ArtikkelNr:    
    FIND TransRecid WHERE
        TransRecid.TRecid = RECID(qTransLogg) NO-ERROR.
    IF NOT AVAILABLE TransRecid THEN
    DO:
        CREATE TransRecid.
        ASSIGN
            TransRecid.TRecid = RECID(qTransLogg).
    END.
  END.
  wOk = BROWSE-TransLogg:REFRESH( ).
end. /* TRANSACTION */  

/* Motposterer transene. */
FOR EACH TransRecid:
  FIND bTransLogg NO-LOCK WHERE
      RECID(bTransLogg) = TransRecid.TRecid NO-ERROR.
  IF AVAILABLE bTranslogg THEN
  DO:
    /* Setter transaksjonsnummer  */
    find last qTransLogg where
      qTransLogg.Butik = bTransLogg.Butik
      use-index TransLogg no-error.
    if available qTransLogg then
      wTransNr = qTransLogg.TransNr + 1.
    else
      wTransNr = 1.
      
    CREATE qTransLogg.
    BUFFER-COPY bTransLogg TO qTransLogg
      ASSIGN
        qTransLogg.BatchNr   = wwBatchnr
        qTransLogg.TransNr   = wTransNr
        qTransLogg.Antall    = bTransLogg.Antall * -1
        qTransLogg.Postert = FALSE
        qTranslogg.PostertDato = ?
        qTranslogg.PostertTid  = 0
        .
  END.
  DELETE TransRecid.
END.

/* Flagger batchen klar for oppdatering. */
run batchstatus.p (wwBatchnr, 2).

{swn.i}
end. /* Slutt på frameScoop */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Fill-To-Top C-Win 
PROCEDURE Move-Fill-To-Top :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        CASE SUBSTR(wSortCol:DATA-TYPE,1,4).
             WHEN "INTE" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-INTE:HANDLE.
             WHEN "CHAR" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-CHAR:HANDLE.
             WHEN "DATE" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-DATE:HANDLE.
             WHEN "DECI" THEN
                 ASSIGN wAktivFillIn = FILL-IN-SOK-DECI:HANDLE.
        END CASE.
    END.
    wAktivFillIn:MOVE-TO-TOP().
    ASSIGN wAktivFillIn:FORMAT                               = 
           IF wSortCol:DATA-TYPE = "INTEGER" THEN FILL(">",LENGTH(wSortCol:FORMAT)) ELSE wSortCol:FORMAT
           FILL-IN-SOK-DATE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DATE"
           FILL-IN-SOK-INTE:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "INTE"
           FILL-IN-SOK-CHAR:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "CHAR"
           FILL-IN-SOK-DECI:SENSITIVE IN FRAME {&FRAME-NAME} = SUBSTR(wSortCol:DATA-TYPE,1,4) = "DECI".
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryCustomSettings C-Win 
PROCEDURE QueryCustomSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER wInitSortColNavn AS CHAR NO-UNDO. 
    DEF VAR wW            AS WIDGET        NO-UNDO.
    DEF VAR wCount        AS INTE   INIT 1 NO-UNDO.
    DEF VAR wLookUp       AS INTE          NO-UNDO.
    DEF VAR wNySearchCols AS CHAR          NO-UNDO.
    DEF VAR wNySokvillkor AS CHAR          NO-UNDO.
    DEF VAR wNyBrowseIdx  AS CHAR          NO-UNDO.
    ASSIGN wW = BROWSE {&BROWSE-NAME}:FIRST-COLUMN
           wInitSortColNavn = IF wInitSortColNavn = "" THEN
                                  ENTRY(wAktivCol,wSearchCols)
                              ELSE wInitSortColNavn.
    
    REPEAT WHILE VALID-HANDLE(wW):
      ASSIGN wLookUp = LOOKUP(wW:NAME,wSearchCols).
      IF wLookUp /* LOOKUP(wW:NAME,wSearchCols) */ > 0 THEN
            ASSIGN wNySearchCols = IF wNySearchCols = "" THEN wW:NAME ELSE
                                      wNySearchCols + "," + wW:NAME
                   wNySokvillkor = IF wNySokvillkor = "" THEN ENTRY(wLookup,wSokvillkor) ELSE
                                      wNySokvillkor + "," + ENTRY(wLookup,wSokvillkor)
                   wNyBrowseIdx = IF wNyBrowseIdx = "" THEN ENTRY(wLookup,wBrowseIdx) ELSE
                                      wNyBrowseIdx + "," + ENTRY(wLookup,wBrowseIdx)
                   wSearchColsH[wCount] = wW:HANDLE
                   wSortCol             = IF wW:NAME = wInitSortColNavn THEN wW:HANDLE ELSE wSortCol
                   wAktivCol            = IF wW:NAME = wInitSortColNavn THEN wCount ELSE wAktivCol
                   wCount               = wCount + 1.
        ASSIGN wW = wW:NEXT-COLUMN.
    END. 
    ASSIGN wSearchCols = wNySearchCols
           wSokvillkor = wNySokvillkor
           wBrowseIdx     = wNyBrowseIdx.
/*
    RUN SortNyCol. 
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Any-Printable C-Win 
PROCEDURE SD-Any-Printable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CASE SUBSTR(wSortCol:DATA-TYPE,1,4):
        WHEN "INTE" OR WHEN "DATE" THEN DO:
            IF KEY-FUNCTION(LASTKEY) < "0" OR KEY-FUNCTION(LASTKEY) > "9" THEN
                RETURN NO-APPLY.
        END.
    END CASE.
    APPLY "ENTRY" TO wAktivFillIn.
    APPLY LASTKEY.
    IF wSortCol:DATA-TYPE = "INTEGER" THEN
        ASSIGN wAktivFillIn:CURSOR-OFFSET = 2.
    /*
    CASE SUBSTR(wSortCol:DATA-TYPE,1,4):
        WHEN "INTE" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-INTE.
            APPLY LASTKEY. 
            FILL-IN-SOK-INTE:CURSOR-OFFSET = 2.
        END.
        WHEN "CHAR" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-CHAR.
            APPLY LASTKEY.
        END.
        WHEN "DATE" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-DATE.
            APPLY LASTKEY.
        END.
    END CASE.
    */
  END.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Cursor C-Win 
PROCEDURE SD-Cursor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wLeft-Right AS CHAR NO-UNDO.
    def var wW as widget no-undo.
  
    run lockwindowupdate(frame {&FRAME-NAME}:hwnd).

    CASE wLeft-Right:
        WHEN "LEFT" THEN
            ASSIGN wAktivCol = IF wAktivCol = 1 THEN wAntSortCol ELSE wAktivCol - 1.
        WHEN "RIGHT" THEN
            ASSIGN wAktivCol = IF wAktivCol = wAntSortCol THEN 1 ELSE wAktivCol + 1.
    END CASE.
    ASSIGN wSortCol                             = wSearchColsH[wAktivCol]
           BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSortCol.
    RUN SortNyCol.
    
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    
    run lockwindowupdate(0).
    
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Query-Open C-Win 
PROCEDURE SD-Query-Open :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wWhere as char no-undo.
  
  run SettWhereSats (output wWhere).
  assign
    wAktivQString = wSortCol:PRIVATE-DATA
    wAktivQString = if wWhere <> "" and "{&QWhere}" <> "" 
                      then REPLACE(wAktivQString, "WHERE", wWhere)
                    else if wWhere <> ""
                      then REPLACE(wAktivQString, "NO-LOCK", wWhere)
                    else wAktivQString.

   /* wQ:QUERY-PREPARE(wSortCol:PRIVATE-DATA). */
   wQ:QUERY-PREPARE(wAktivQString).
   wQ:QUERY-OPEN().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Reposition C-Win 
PROCEDURE SD-Reposition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        if wBlank = ? then
          REPOSITION {&BROWSE-NAME} TO ROWID rowid(b{&br-tabell}) NO-ERROR.
        ASSIGN wAktivFillIn:SCREEN-VALUE = "".
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settArtikkel C-Win 
PROCEDURE settArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF TransLogg.Postert <> FALSE THEN
  DO:
      MESSAGE "Posten er oppdatert og kan ikke endres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  RUN SokArtBas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWhereSats C-Win 
PROCEDURE SettWhereSats :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter wWhere as char no-undo.

  if frame DEFAULT-FRAME:visible = false then
    return.

  FRAME-SCOOP:
  do with frame DEFAULT-FRAME:
  
  /* Bygger where sats */
  assign
    wWhere = ""
    wWhere = wWhere + " " +
             (if (CB-TTId <> wAlle and CB-TTId <> "")
               then (if wWhere = "" then "" else "and ") + ("TransLogg.TTId = " + entry(1,CB-TTId,':'))
               else "")
    wWhere = wWhere + " " +
             (if T-VisAlle = false
               then (if wWhere = "" then "" else "and ") + "TransLogg.Postert = false"
               else "")
    wWhere = if wWhere <> "" and "{&QWhere}" <> "" 
               then "where " + wWhere + " and "
             else if wWhere <> "" 
               then "NO-LOCK where " + wWhere 
             else if "{&QWhere}" <> ""
               then ""
             else "NO-LOCK".
    
  end. /* FRAME SCOOP */       
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
    DO TRANSACTION:
      FIND CURRENT TransLogg EXCLUSIVE-LOCK.
      ASSIGN
          TransLogg.ArtikkelNr = ArtBas.ArtikkelNr
          TransLogg.Vg         = ArtBas.Vg
          TransLogg.LopNr      = ArtBas.LopNr
          .
      FIND CURRENT Translogg NO-LOCK.
      run batchstatus.p (Translogg.BatchNr, 2).
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortNyCol C-Win 
PROCEDURE SortNyCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN wAktivCol  =  LOOKUP(wSortCol:NAME,wSearchCols)
           BROWSE {&BROWSE-NAME}:CURRENT-COLUMN = wSortCol.
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK no-error.

    RUN SD-QUERY-OPEN.
    RUN Move-Fill-To-Top.
    RUN LabelColor.
    RUN SD-Reposition.

    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Klokken C-Win 
FUNCTION Klokken RETURNS CHARACTER
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wKlokken as char no-undo.
  def var wTid     as int  no-undo.
  
  if available BatchLogg then
    wTid = BatchLogg.RegistrertTid.
  else
    wTid = 0.
  
  
  if wTid <> 0 then
    wKlokken = string(wTid,"HH:MM:SS").
  else
    wKlokken = "".

  RETURN wKlokken.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Prep-Private-Data C-Win 
FUNCTION Prep-Private-Data RETURNS CHARACTER
  ( INPUT wQueryCol AS WIDGET,INPUT wQueryCol# AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VAR wQStr      AS CHAR NO-UNDO.
   DEFINE VAR wXSorttype AS CHAR NO-UNDO. 
   DEFINE VAR wXSort     AS CHAR NO-UNDO. 
   DEFINE VAR wQWhere    AS CHAR NO-UNDO. 
   &IF LENGTH("{&QWhere}") > 1 &THEN
       ASSIGN wQWhere = "WHERE " + {&QWhere}.
   &ENDIF
   ASSIGN wXSorttype = IF ENTRY(wQueryCol#,wSorttype) = "" THEN "BY" ELSE
                          ENTRY(wQueryCol#,wSorttype)
          wXSort     = IF wXSorttype = "BY" THEN
                           "{&br-tabell}." + wQueryCol:Name
                       ELSE IF wXSorttype = "BY DESCENDING" THEN
                           "{&br-tabell}." + wQueryCol:Name + " DESCENDING" 
                       ELSE
                           ENTRY(wQueryCol#,"{&BrowseIdx}")
          wXSorttype = IF wXSorttype = "BY DESCENDING" THEN "BY" ELSE
                              wXSorttype
          wQStr = REPLACE("{&BrowseQ}","XSORTTYPE",wXSorttype)
          wQStr = REPLACE(wQStr,"XWHERE",wQWHere)
          wQStr = REPLACE(wQStr,"XSORT",wXSort).
  RETURN wQStr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

