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
&scope Sorttype    USE-INDEX,USE-INDEX,USE-INDEX,USE-INDEX
/* BrowseIdx entries = antal sortkolonner, kan vara tomma. Om USE-INDEX i Sorttype 
             skall samma entry ha indexnamn */
&scope BrowseIdx   OppslagDatoTid,OppslagStr,OppslagKunde,OppslagSelger
&scope Sokvillkor  >=,>=,>=,>=
&scope InitIdx     OppslagDatoTid
&scope ip-felt     Dato

/* Om du önskar input parameter. Ger en startup-record */ 
/*
&scope ip-variabel w{&ip-felt}
*/

/*
&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.BestNr) ~
                                                else "".
*/                                                

/* NB NB NB */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  define var wArtikkelNr like ArtBas.ArtikkelNr       no-undo.
  ASSIGN
    wArtikkelNr = 2000.
&ELSE
  DEFINE INPUT PARAMETER wArtikkelNr     LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEF    INPUT PARAMETER wCurrent-Window AS HANDLE              NO-UNDO.
  DEF    INPUT PARAMETER wParentHandle   AS HANDLE              NO-UNDO.
&ENDIF

&scope QWhere      'TransLogg.ArtikkelNr = ' + STRING(wArtikkelNr)
&scope BrowseQ     FOR EACH {&br-tabell} NO-LOCK XWHERE XSORTTYPE XSORT INDEXED-REPOSITION
&scope BrowseSQ    FOR EACH b{&br-tabell} NO-LOCK WHERE XWHERE b{&br-tabell}.XFIELD XSOKV XFILL ~
                           USE-INDEX XIDX MAX-ROWS 1

/* ----------------- Definer høyere opp i koden.
/* Parameter Definisjoner ---                                           */
&IF LENGTH("{&ip-variabel}") > 0 &THEN

  &scope return-ip   if available {&br-tabell} then ASSIGN {&ip-variabel} = {&br-tabell}.{&ip-felt}
  &scope init-phrase FIND b{&br-tabell} WHERE b{&br-tabell}.{&ip-felt} = ~
                        {&ip-variabel} USE-INDEX {&InitIdx} NO-LOCK NO-ERROR.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} INIT ? NO-UNDO.
    define var wArtikkelNr    like ArtBas.ArtikkelNr       no-undo.
    assign
      wArtikkelNr = 2000.
  &ELSE
    DEFINE INPUT-OUTPUT PARAMETER {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} NO-UNDO.
    define input        parameter wArtikkelNr    like ArtBas.ArtikkelNr       no-undo.
  &ENDIF

&ENDIF
-------------------- */

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.
DEFINE QUERY wSQ FOR b{&br-tabell} SCROLLING.
DEFINE TEMP-TABLE tmpChild
  FIELD wChild AS HANDLE.

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
DEFINE VAR wOk               AS LOG         NO-UNDO.
DEFINE VAR wAlle             AS CHAR        NO-UNDO.
DEFINE VAR wAktivQString     AS CHAR        NO-UNDO.
DEFINE VAR wBekreft          AS LOG         NO-UNDO.
DEFINE VAR wBlank AS LOG                    NO-UNDO.
DEF VAR wKlokken             AS CHAR FORMAT "x(8)"    NO-UNDO.
DEF VAR wTransType           AS CHAR FORMAT "x(15)"   NO-UNDO.
DEF VAR wDummy               AS INT         NO-UNDO.
DEF VAR wBatchNr             AS INT         NO-UNDO.
DEF VAR lBruttoPris          LIKE TransLogg.Pris NO-UNDO.
DEF VAR h_PrisKo             AS HANDLE      NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-BROWSE-TransLogg TransLogg.BatchNr ~
TransLogg.Butik TransLogg.OvButik wTransType TransLogg.TBId TransLogg.Dato ~
TransLogg.Kode TransLogg.Storl TransLogg.Antall TransLogg.Pris ~
TransLogg.RabKr TransLogg.Mva ~
(TransLogg.Pris - TransLogg.RabKr) @ lBruttoPris TransLogg.VVarekost ~
TransLogg.Postert TransLogg.PostertDato TransLogg.RegistrertAv ~
TransLogg.TilStorl TransLogg.KundNr TransLogg.MedlemsNr TransLogg.KortType ~
TransLogg.KortNr TransLogg.ForsNr TransLogg.Plukket TransLogg.TransNr ~
TransLogg.KassaNr TransLogg.BongId TransLogg.BongLinjeNr TransLogg.BestNr ~
TransLogg.LevNr TransLogg.RefNr TransLogg.RefTekst TransLogg.SattVVareKost ~
TransLogg.BongTekst TransLogg.FeilKode TransLogg.individnr ~
TransLogg.KalkylePris TransLogg.Mva% TransLogg.Varekost ~
TransLogg.ArtikkelNr TransLogg.Vg TransLogg.LopNr TransLogg.RegistrertDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TransLogg TransLogg.Dato ~
TransLogg.Storl TransLogg.KundNr TransLogg.ForsNr 
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
&Scoped-Define ENABLED-OBJECTS B-VisTrans BROWSE-TransLogg FILL-IN-SOK-DATE ~
FILL-IN-SOK-CHAR FILL-IN-SOK-DECI FILL-IN-SOK-INTE BUTTON-Sok B-TransReg ~
FI-ButikkNr FI-OvButik FI-Storl CB-TTId FI-FraDato FI-TilDato T-VisAlle ~
BUTTON-Motposter BUTTON-Motposter-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-DATE FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DECI FILL-IN-SOK-INTE FI-ButikkNr FI-OvButik FI-Storl CB-TTId ~
FI-FraDato FI-TilDato T-VisAlle 

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
DEFINE BUTTON B-TransReg 
     LABEL "&Trans.registrering" 
     SIZE 19.8 BY 1.1.

DEFINE BUTTON B-VisTrans 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON BUTTON-Motposter 
     LABEL "&Motposter" 
     SIZE 15.4 BY 1.1.

DEFINE BUTTON BUTTON-Motposter-2 
     LABEL "&Endre varekost og motposter" 
     SIZE 30 BY 1.1.

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1 TOOLTIP "Søk i AKTIV kollonne".

DEFINE VARIABLE CB-TTId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Trans.type" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato fra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OvButik AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Overf.til butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Storl AS CHARACTER FORMAT "X(6)":U 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     LABEL "til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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

DEFINE VARIABLE T-VisAlle AS LOGICAL INITIAL yes 
     LABEL "&Vis alle" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TransLogg FOR 
      TransLogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TransLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TransLogg C-Win _STRUCTURED
  QUERY BROWSE-TransLogg NO-LOCK DISPLAY
      TransLogg.BatchNr FORMAT "zzzzzzzz9":U
      TransLogg.Butik FORMAT "zzzzz9":U
      TransLogg.OvButik COLUMN-LABEL "TilBut" FORMAT "zzzzz9":U
      wTransType COLUMN-LABEL "TransType" FORMAT "x(15)":U
      TransLogg.TBId COLUMN-LABEL "TBId" FORMAT ">>9":U WIDTH 8
      TransLogg.Dato COLUMN-LABEL "Dato" FORMAT "99/99/9999":U
      TransLogg.Kode FORMAT "X(20)":U
      TransLogg.Storl COLUMN-LABEL "Str" FORMAT "x(5)":U
      TransLogg.Antall FORMAT "-zz,zzz,zz9.999":U
      TransLogg.Pris COLUMN-LABEL "Brutto pris" FORMAT "-zz,zzz,zz9.99":U
      TransLogg.RabKr FORMAT "->,>>>,>>9.99":U
      TransLogg.Mva FORMAT "->,>>>,>>9.99":U
      (TransLogg.Pris - TransLogg.RabKr) @ lBruttoPris COLUMN-LABEL "Pris" FORMAT "-zz,zzz,zz9.99":U
            WIDTH 14.4
      TransLogg.VVarekost FORMAT "-z,zzz,zz9.99":U
      TransLogg.Postert COLUMN-LABEL "Post" FORMAT "Ja/Nei":U
      TransLogg.PostertDato COLUMN-LABEL "PosttDato" FORMAT "99/99/9999":U
      TransLogg.RegistrertAv COLUMN-LABEL "Brukerid" FORMAT "X(10)":U
      TransLogg.TilStorl FORMAT "x(10)":U
      TransLogg.KundNr FORMAT ">>>>>>>>>>>>9":U
      TransLogg.MedlemsNr FORMAT ">>>>>>>>>>>>9":U
      TransLogg.KortType FORMAT "9":U
      TransLogg.KortNr FORMAT "X(22)":U
      TransLogg.ForsNr FORMAT ">>>>>9":U
      TransLogg.Plukket FORMAT "Ja/Nei":U
      TransLogg.TransNr COLUMN-LABEL "TransNr" FORMAT "zz,zzz,zz9":U
      TransLogg.KassaNr COLUMN-LABEL "Kasse" FORMAT "zzz9":U
      TransLogg.BongId FORMAT "zz,zzz,zz9":U
      TransLogg.BongLinjeNr FORMAT "zzzzz9":U
      TransLogg.BestNr FORMAT ">>>>>>>9":U
      TransLogg.LevNr COLUMN-LABEL "LevNr" FORMAT "zzzzz9":U
      TransLogg.RefNr FORMAT "->,>>>,>>9":U
      TransLogg.RefTekst FORMAT "X(40)":U
      TransLogg.SattVVareKost FORMAT "yes/no":U
      TransLogg.BongTekst FORMAT "X(30)":U
      TransLogg.FeilKode FORMAT "zzz9":U
      TransLogg.individnr FORMAT ">>>>>>>>>>>9":U
      TransLogg.KalkylePris FORMAT "->>,>>>,>>9.99":U
      TransLogg.Mva% FORMAT "->>9.99":U
      TransLogg.Varekost FORMAT "->>,>>9.99":U
      TransLogg.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      TransLogg.Vg FORMAT "zzzzz9":U
      TransLogg.LopNr FORMAT "zzzzz9":U
      TransLogg.RegistrertDato FORMAT "99/99/9999":U
  ENABLE
      TransLogg.Dato
      TransLogg.Storl
      TransLogg.KundNr
      TransLogg.ForsNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN MULTIPLE SIZE 206.8 BY 19.24 ROW-HEIGHT-CHARS .63 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-VisTrans AT ROW 1.62 COL 2.6
     BROWSE-TransLogg AT ROW 3.86 COL 1.2
     FILL-IN-SOK-DATE AT ROW 1.62 COL 7.6 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.62 COL 7.6 NO-LABEL
     FILL-IN-SOK-DECI AT ROW 1.62 COL 7.6 NO-LABEL
     FILL-IN-SOK-INTE AT ROW 1.62 COL 7.6 NO-LABEL
     BUTTON-Sok AT ROW 1.62 COL 27
     B-TransReg AT ROW 1.62 COL 37.8
     FI-ButikkNr AT ROW 1.67 COL 64 COLON-ALIGNED
     FI-OvButik AT ROW 2.67 COL 64 COLON-ALIGNED
     FI-Storl AT ROW 1.67 COL 87.8 COLON-ALIGNED
     CB-TTId AT ROW 1.67 COL 108 COLON-ALIGNED
     FI-FraDato AT ROW 2.67 COL 108 COLON-ALIGNED
     FI-TilDato AT ROW 2.67 COL 128 COLON-ALIGNED
     T-VisAlle AT ROW 1.67 COL 144.6
     BUTTON-Motposter AT ROW 1.62 COL 156.6
     BUTTON-Motposter-2 AT ROW 1.62 COL 172
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.66
         SIZE 207.8 BY 23.1.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Søkeliste transaksjonslogg for artikkel"
         HEIGHT             = 31.76
         WIDTH              = 208
         MAX-HEIGHT         = 31.76
         MAX-WIDTH          = 208
         VIRTUAL-HEIGHT     = 31.76
         VIRTUAL-WIDTH      = 208
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME L-To-R,COLUMNS                                */
/* BROWSE-TAB BROWSE-TransLogg B-VisTrans DEFAULT-FRAME */
ASSIGN 
       BROWSE-TransLogg:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 6
       BROWSE-TransLogg:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 481
       BROWSE-TransLogg:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DECI IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INTE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TransLogg
/* Query rebuild information for BROWSE BROWSE-TransLogg
     _TblList          = "SkoTex.TransLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "TransLogg.BatchNr = wBatchNr"
     _FldNameList[1]   = SkoTex.TransLogg.BatchNr
     _FldNameList[2]   > SkoTex.TransLogg.Butik
"Butik" ? "zzzzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.TransLogg.OvButik
"OvButik" "TilBut" "zzzzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"wTransType" "TransType" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > SkoTex.TransLogg.TBId
"TBId" "TBId" ? "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > SkoTex.TransLogg.Dato
"Dato" "Dato" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = SkoTex.TransLogg.Kode
     _FldNameList[8]   > SkoTex.TransLogg.Storl
"Storl" "Str" "x(5)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > SkoTex.TransLogg.Antall
"Antall" ? "-zz,zzz,zz9.999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > SkoTex.TransLogg.Pris
"Pris" "Brutto pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   = SkoTex.TransLogg.RabKr
     _FldNameList[12]   = SkoTex.TransLogg.Mva
     _FldNameList[13]   > "_<CALC>"
"(TransLogg.Pris - TransLogg.RabKr) @ lBruttoPris" "Pris" "-zz,zzz,zz9.99" ? ? ? ? ? ? ? no ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = SkoTex.TransLogg.VVarekost
     _FldNameList[15]   > SkoTex.TransLogg.Postert
"Postert" "Post" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > SkoTex.TransLogg.PostertDato
"PostertDato" "PosttDato" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > SkoTex.TransLogg.RegistrertAv
"RegistrertAv" "Brukerid" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   = SkoTex.TransLogg.TilStorl
     _FldNameList[19]   > SkoTex.TransLogg.KundNr
"KundNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   = SkoTex.TransLogg.MedlemsNr
     _FldNameList[21]   = SkoTex.TransLogg.KortType
     _FldNameList[22]   = SkoTex.TransLogg.KortNr
     _FldNameList[23]   > SkoTex.TransLogg.ForsNr
"ForsNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   = SkoTex.TransLogg.Plukket
     _FldNameList[25]   > SkoTex.TransLogg.TransNr
"TransNr" "TransNr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > SkoTex.TransLogg.KassaNr
"KassaNr" "Kasse" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   = SkoTex.TransLogg.BongId
     _FldNameList[28]   = SkoTex.TransLogg.BongLinjeNr
     _FldNameList[29]   = SkoTex.TransLogg.BestNr
     _FldNameList[30]   > SkoTex.TransLogg.LevNr
"LevNr" "LevNr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   = SkoTex.TransLogg.RefNr
     _FldNameList[32]   = SkoTex.TransLogg.RefTekst
     _FldNameList[33]   = SkoTex.TransLogg.SattVVareKost
     _FldNameList[34]   = SkoTex.TransLogg.BongTekst
     _FldNameList[35]   = SkoTex.TransLogg.FeilKode
     _FldNameList[36]   = SkoTex.TransLogg.individnr
     _FldNameList[37]   = SkoTex.TransLogg.KalkylePris
     _FldNameList[38]   = SkoTex.TransLogg.Mva%
     _FldNameList[39]   = SkoTex.TransLogg.Varekost
     _FldNameList[40]   = SkoTex.TransLogg.ArtikkelNr
     _FldNameList[41]   = SkoTex.TransLogg.Vg
     _FldNameList[42]   > SkoTex.TransLogg.LopNr
"LopNr" ? "zzzzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[43]   = SkoTex.TransLogg.RegistrertDato
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-TransLogg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søkeliste transaksjonslogg for artikkel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søkeliste transaksjonslogg for artikkel */
DO:
  IF CAN-FIND(FIRST tmpChild WHERE
               valid-handle(tmpChild.wChild)) THEN
    DO:
      wBekreft = FALSE.
      MESSAGE 'Det er startet andre programmer fra dette vinduet.' SKIP
              'Avsluttes dette vinduet, vil alle underliggende programmer' SKIP
              'også bli avsluttet.'
              VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE 'Bekreft avsluttning'
              UPDATE wBekreft
              .
    END.
  ELSE wBekreft = TRUE.
  IF wBekreft <> TRUE THEN
  RETURN NO-APPLY.
                       
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TransReg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TransReg C-Win
ON CHOOSE OF B-TransReg IN FRAME DEFAULT-FRAME /* Trans.registrering */
DO:
  DEF VAR wBatchNr AS INT NO-UNDO.
  DEFINE VARIABLE iRow AS INTEGER    NO-UNDO.
  DEFINE VARIABLE rTransRowId AS ROWID      NO-UNDO.
  IF NOT AVAILABLE TransLogg THEN
    RETURN NO-APPLY.
  ASSIGN wBatchNr = TransLogg.BatchNr.
  FIND BatchLogg NO-LOCK WHERE
    BatchLogg.BatchNr = wBatchNr NO-ERROR.
  
  IF NOT AVAILABLE BatchLogg THEN
    RETURN NO-APPLY.
  ASSIGN wCurrent-Window:SENSITIVE = FALSE
         wBatchNr    = BatchLogg.BatchNr
         rTransRowId = ROWID(TransLogg)
         iRow        = BROWSE {&BROWSE-NAME}:FOCUSED-ROW.
  RUN w-vtranslogg (INPUT-OUTPUT wBatchNr,INPUT rTransRowid) NO-ERROR.
  RUN SD-Query-Open.
  {&BROWSE-NAME}:SET-REPOSITIONED-ROW(iRow).
  REPOSITION {&BROWSE-NAME} TO ROWID rTransRowId NO-ERROR.
  BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  ASSIGN wCurrent-Window:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans C-Win
ON CHOOSE OF B-VisTrans IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
  IF NOT AVAILABLE TransLogg THEN
    RETURN NO-APPLY.
    
  /* Kall til rutine for visning av transaksjon. */
/*   RUN gviskvittokopi.w (TransLogg.Butik,1,TransLogg.KassaNr,TransLogg.Dato,TransLogg.BongId). */
  RUN gviskvittokopi2.w (TransLogg.Butik,1,TransLogg.KassaNr,TransLogg.Dato,TransLogg.BongId,THIS-PROCEDURE).
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
  /*
  APPLY "CHOOSE" TO Btn_OK.
  RETURN NO-APPLY.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TransLogg C-Win
ON END-MOVE OF BROWSE-TransLogg IN FRAME DEFAULT-FRAME
DO:
  MESSAGE "gurre var her " VIEW-AS ALERT-BOX.
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
  IF AVAILABLE TransLogg THEN
    DO:
      FIND TransType NO-LOCK OF 
        TransLogg NO-ERROR.
      IF Translogg.Postert = FALSE THEN
        TransLogg.Postert:bgcolor IN BROWSE BROWSE-TransLogg = 12.        
      IF Translogg.TTId = 6 AND
        TransLogg.Storl <> TransLogg.TilStorl THEN
          ASSIGN
            TransLogg.Storl:bgcolor IN BROWSE BROWSE-TransLogg = 10
            TransLogg.TilStorl:bgcolor IN BROWSE BROWSE-TransLogg = 10.
    END.
  ASSIGN
    wTransType = IF AVAILABLE TransType 
                   THEN TransType.Beskrivelse
                   ELSE ""
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


&Scoped-define SELF-NAME BUTTON-Motposter-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Motposter-2 C-Win
ON CHOOSE OF BUTTON-Motposter-2 IN FRAME DEFAULT-FRAME /* Endre varekost og motposter */
DO:
  ASSIGN
    wDummy = IF INPUT CB-TTID = wAlle 
               THEN ?
               ELSE INT(ENTRY(1,INPUT CB-TTID,":")).
  RUN MotposterOgKorrigerTranser (wDummy).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ELSE DO:
      IF wBatchNr > 0 THEN
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
   
   wBlank = FALSE.
   
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
   ELSE DO:
     wBlank = ?.
     RUN SD-Reposition.
     wBlank = FALSE.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId C-Win
ON VALUE-CHANGED OF CB-TTId IN FRAME DEFAULT-FRAME /* Trans.type */
DO:
  ASSIGN FRAME DEFAULT-FRAME
    CB-TTId.
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ButikkNr C-Win
ON TAB OF FI-ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
OR RETURN OF FI-ButikkNr
DO:
    ASSIGN FRAME DEFAULT-FRAME
      CB-TTId.
    RUN SD-CURSOR (" ").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraDato C-Win
ON TAB OF FI-FraDato IN FRAME DEFAULT-FRAME /* Dato fra */
OR RETURN OF FI-FraDato
DO:
    ASSIGN FRAME DEFAULT-FRAME
      CB-TTId.
    RUN SD-CURSOR (" ").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-OvButik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-OvButik C-Win
ON TAB OF FI-OvButik IN FRAME DEFAULT-FRAME /* Overf.til butikk */
OR RETURN OF FI-OvButik
DO:
    ASSIGN FRAME DEFAULT-FRAME
      CB-TTId.
    RUN SD-CURSOR (" ").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl C-Win
ON TAB OF FI-Storl IN FRAME DEFAULT-FRAME /* Størrelse */
OR RETURN OF FI-Storl
DO:
    ASSIGN FRAME DEFAULT-FRAME
      CB-TTId.
    RUN SD-CURSOR (" ").
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilDato C-Win
ON TAB OF FI-TilDato IN FRAME DEFAULT-FRAME /* til */
OR RETURN OF FI-TilDato
DO:
    ASSIGN FRAME DEFAULT-FRAME
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
  ASSIGN FRAME DEFAULT-FRAME
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
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window. 

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
    ON CLOSE OF THIS-PROCEDURE DO:
    /*    RUN SaveBrowseSettings. */
        IF VALID-HANDLE(h_Prisko) THEN
            DELETE OBJECT h_Prisko.
        RUN SlettTmpChild.
        IF VALID-HANDLE(wParentHandle) THEN
            RUN SlettProg IN wParentHandle.
        RUN disable_UI.
    END.
/* {genlib.i                                                    */
/*   &NoLibCall      = "Nei"                                    */
/*   &WindowName     = "Søkeliste VinduTransLogg"               */
/*   &PreIClose      = "RUN SaveBrowseSettings."                */
/*   &PostIClose     = " "                                      */
/*   &PostDisable_ui = "for each tmpChild:                      */
/*                        if valid-handle(tmpChild.wChild) then */
/*                          delete procedure tmpChild.wChild.   */
/*                      end."                                   */
/* }                                                            */

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
    RUN InitTransType.

    RUN SD-QUERY-OPEN.

    IF RETURN-VALUE = "FEIL" THEN
        LEAVE MAIN-BLOCK.

    FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = wArtikkelNr NO-ERROR.
/*     if available ArtBas then                                                         */
/*       assign                                                                         */
/*         {&WINDOW-NAME}:title = {&WINDOW-NAME}:title + "  " +                         */
/*                              string(ArtBas.Vg) + "/" + string(ArtBAs.LopNr) + " (" + */
/*                              string(ArtBAs.ArtikkelNr) + ")".                        */
    RUN enable_UI.
    {lng.i} 
    {browsesettings.i {&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:CURRENT-COLUMN <> wSortCol THEN DO:
        wSortCol = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
        RUN QueryCustomSettings (wSortCol:NAME).
    END.

    RUN LabelColor.
    RUN Move-Fill-To-Top. 
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
    RUN ButtonEnaDis.
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    FRAME DEFAULT-FRAME:SENSITIVE = artbas.sanertdato = ?.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-Win 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN B-VisTrans:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  FRAME DEFAULT-FRAME:SENSITIVE = artbas.sanertdato = ?.
  FOR EACH tmpChild:
      IF VALID-HANDLE(tmpChild.wChild) THEN
          DELETE PROCEDURE tmpChild.wChild.
  END.
   ASSIGN
    wSortCol:PRIVATE-DATA = REPLACE(wSortCol:PRIVATE-DATA,STRING(wArtikkelNr),STRING(dArtikkelNr))
    wArtikkelNr = dArtikkelNr.

  {sww.i}  
   RUN SD-Query-Open.
/*   {&OPEN-QUERY-{&BROWSE-NAME}}                    */
/*   APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}. */
  RUN ButtonEnaDis.
  {swn.i}
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY FILL-IN-SOK-DATE FILL-IN-SOK-CHAR FILL-IN-SOK-DECI FILL-IN-SOK-INTE 
          FI-ButikkNr FI-OvButik FI-Storl CB-TTId FI-FraDato FI-TilDato 
          T-VisAlle 
      WITH FRAME DEFAULT-FRAME.
  ENABLE B-VisTrans BROWSE-TransLogg FILL-IN-SOK-DATE FILL-IN-SOK-CHAR 
         FILL-IN-SOK-DECI FILL-IN-SOK-INTE BUTTON-Sok B-TransReg FI-ButikkNr 
         FI-OvButik FI-Storl CB-TTId FI-FraDato FI-TilDato T-VisAlle 
         BUTTON-Motposter BUTTON-Motposter-2 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRecord C-Win 
PROCEDURE GetRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iButikkNr LIKE BongHode.ButikkNr NO-UNDO.
  DEFINE OUTPUT PARAMETER iGruppeNr LIKE BongHode.GruppeNr NO-UNDO.
  DEFINE OUTPUT PARAMETER iKasseNr  LIKE BongHode.KasseNr  NO-UNDO.
  DEFINE OUTPUT PARAMETER dDato     LIKE BongHode.Dato     NO-UNDO.
  DEFINE OUTPUT PARAMETER iBongNr   LIKE BongHode.BongNr   NO-UNDO.
  DEFINE VARIABLE cVerdier     AS CHARACTER  NO-UNDO.
  IF CAN-DO("Prev,Next",cRettning) THEN DO:
      IF cRettning = "NEXT" THEN
          BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
      ELSE 
          BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
      IF Translogg.BongId <> 0 THEN
          ASSIGN iButikkNr = TransLogg.Butik
                 iGruppeNr = 1
                 iKasseNr  = TransLogg.KassaNr
                 dDato     = TransLogg.Dato
                 iBongNr   = TransLogg.BongId.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Html-Rapport C-Win 
PROCEDURE Html-Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR wTitle       AS CHAR NO-UNDO.
  DEFINE VAR wRapport     AS CHAR NO-UNDO.
  DEFINE VAR wHead1       AS CHAR NO-UNDO.
  DEFINE VAR wHead1Set    AS CHAR NO-UNDO.
  DEFINE VAR wHead2       AS CHAR NO-UNDO.
  DEFINE VAR wColHead     AS CHAR NO-UNDO.
  DEFINE VAR wColHeadForm AS CHAR NO-UNDO.
  DEFINE VAR wTabell      AS CHAR NO-UNDO.
  DEFINE VAR wQY          AS CHAR NO-UNDO.
  DEFINE VAR wFields      AS CHAR NO-UNDO.
  DEFINE VAR wDokTyp      AS CHAR NO-UNDO.

  /* {tmphtmlrapp.i &New = "New"} --- Skal ligge i definisjonsblokken */
  /* ----------
  {sww.i}
  
  /* Tømmer tmp buffer */
  for each tmpHtml: delete tmpHtml. end.

  /* Bygger tmpbuffer */
  for each Overfor:
    create tmpHtml.
    assign
      tmpHtml.Data01 = string(ArtBas.Vg,"zz9")
      tmpHtml.Data02 = string(ArtBas.LopNr,"zzz9")
      tmpHtml.Data03 = string(Overfor.TilButik)
      tmpHtml.Data04 = string(Overfor.FraButik)
      tmpHtml.Data05 = string(Overfor.TilStrl)
      tmpHtml.Data06 = string(Overfor.FraStrl)
      tmpHtml.Data07 = string(Overfor.Antal,"zzz9").
  end.
  
  ASSIGN wTitle       = "Rapportgenerator"
         wRapport     = "Overføringer"
         wHead1       = "Rapport " + wRapport
         wHead1Set    = "80%,,1,0,2," + "7"
         wHead2       = "Bruker: " + USERID("dictdb") + "<Html:BreakLn><Html:BreakLn>" +
                      "<Html:CenterOn><Html:BoldOn>Dato: " + string(today,"99/99/9999")
         wColHead     = "VareGr|LøpneNr|TilBut|FraBut|TilStr|FraStr|Antal"
         wFields      = "Data01,Data02,Data03,Data04,Data05,Data06,Data07"
         wColHeadForm = "L,5%|L,20%|L,20%"
         wTabell      = "tmpHtml"
         wQY          = "for each " + wTabell + " NO-LOCK by Data01 by Data02 by Data03"
         wDokTyp      = "Html".
  
  RUN stdrapphtml2.p (wTitle,wRapport,wHead1,wHead1Set,wHead2,wColHead,wColHeadForm,wTabell,wQY,wFields,wDokTyp).
  if valid-handle(wLibHandle) then
    RUN OpenWeb in wLibHandle ("rapport.htm").
  {swn.i}

  -------------- */
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
  ASSIGN
    CB-TTId:list-items   IN FRAME DEFAULT-FRAME = wAlle
    CB-TTId:screen-value IN FRAME DEFAULT-FRAME = wAlle.

  FOR EACH TransType NO-LOCK:
    ASSIGN
     CB-TTId:list-items IN FRAME DEFAULT-FRAME = 
                   CB-TTId:list-items IN FRAME DEFAULT-FRAME +
                   (IF CB-TTId:list-items IN FRAME DEFAULT-FRAME = ""
                      THEN ""
                      ELSE ",") +
                   string(TransType.TTId,"zz9") + ": " + TransType.Beskrivelse.
  END.
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
        MESSAGE "Du må 'enabla' minst ett felt" VIEW-AS ALERT-BOX.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSorttype) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sorttype skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," SKIP
                " kommaseparert med valgfritt BY, BY DESCENDING eller USE-INDEX." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    IF NUM-ENTRIES(wSokvillkor) <> wAntSortCol THEN DO:
        MESSAGE "&scope Sokvillkor skall ha " + STRING(wAntSortCol) +
                " entries i definitionsblocket," SKIP
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MotposterOgKorrigerTranser C-Win 
PROCEDURE MotposterOgKorrigerTranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wTTId LIKE TransLogg.TTId NO-UNDO.

DEF VAR wSvar          AS LOG               NO-UNDO.
DEF VAR wLoop          AS INT               NO-UNDO.
DEF VAR wTittel        AS CHAR              NO-UNDO.
DEF VAR wAlleTranser   AS LOG INITIAL FALSE NO-UNDO.
DEF VAR wTransNr       AS INT INITIAL 0     NO-UNDO.
DEF VAR wPwd           AS CHAR              NO-UNDO.
DEF VAR wTekst         AS CHAR              NO-UNDO.
DEF VAR wVVarekost     AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR wOldVVarekost  AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.

DEF BUFFER bTransLogg FOR TransLogg.
DEF BUFFER qTransLogg FOR TransLogg.
DEF BUFFER bLager     FOR Lager.

/* Setter Pwd. */
{syspara.i 2 3 1 wPwd}
IF wPwd = "" THEN
  ASSIGN
    wPwd = "MASTER".
ASSIGN
    wBatchNr = 0.

/* Nullstiller temp-file */
FOR EACH TransRecid:
    DELETE TransRecid.
END.

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

/* Setter frameScoop */
DO WITH FRAME DEFAULT-FRAME:
ASSIGN 
    wAlleTranser = FALSE.

/*{sww.i}*/
IF wAlleTranser = FALSE THEN 
DO TRANSACTION:
  MOTPOSTER_LOOP:
  DO wLoop = BROWSE-TransLogg:NUM-SELECTED-ROWS TO 1 BY -1:
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
ELSE DO TRANSACTION:
  /* Alle transaksjoner */
  IF wTTID = ? THEN
    FOR EACH qTransLogg EXCLUSIVE-LOCK WHERE
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
END. /* TRANSACTION */  

IF NOT CAN-FIND(FIRST TransRecid) THEN
DO:
    MESSAGE "Ingen transaksjoner er valgt for motpostering"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

FIND FIRST TransRecid NO-LOCK.
FIND FIRST bTransLogg NO-LOCK WHERE
    RECID(bTransLogg) = TransRecid.TRecid NO-ERROR.
IF AVAILABLE bTransLogg THEN
    wOldvVarekost = bTransLogg.VVareKost.
ELSE
    wOldVVareKost = 0.

FIND bLager NO-LOCK WHERE
    bLager.ArtikkelNr = bTransLogg.ArtikkelNr AND
    bLager.butik      = bTransLogg.Butik NO-ERROR.
IF AVAILABLE bLager THEN
    wVVareKost = IF (bLager.VVareKost <= 0 OR bLager.VVareKost = ? OR ArtBas.OPris OR ArtBas.LAger = FALSE) THEN 0 ELSE bLager.VVareKost.
ELSE
    wVVareKost = 0.

IF (wVVareKost = 0 OR ArtBas.OPris = TRUE OR ArtBas.LAger = FALSE OR bLager.Lagant <= 0 OR wVVareKost = ?) THEN
DO:
    IF VALID-HANDLE(h_PrisKo) THEN
          /* NB: Varekost skal regnes av pris eksklusive rabatter       */
          /* Mva trekkes fra i rutinen som kalles. Fordi hvis det er    */
          /* gitt rabatt, er det feil mva. MvaKr må da beregnes pånytt. */
          RUN HentVareKost IN h_PrisKo (INPUT bLager.ArtikkelNr, 
                                        INPUT bLager.Butik, 
                                          INPUT (Translogg.Pris - (Translogg.Pris - (Translogg.Pris / (1 + (Translogg.Mva% / 100))))), 
                                        OUTPUT wVVareKost).
END.

/* Setting av ny varekost */
CURRENT-WINDOW:SENSITIVE = NO.
RUN settVVarekost.w (bTranslogg.ArtikkelNr, wOldVVareKost, INPUT-OUTPUT wVVareKost).
CURRENT-WINDOW:SENSITIVE = YES.
IF RETURN-VALUE <> 'OK' THEN
    RETURN.
IF wVVarekost <= 0 THEN
    RETURN.
    

/* Adgangskontroll */
RUN d-adgangskontroll.d (INPUT "Adgangskontroll motpostering", INPUT wPwd).
IF RETURN-VALUE <> "OK" THEN
    RETURN "AVBRYT".

/* Tittel som batchen med motposteringene merkes med. */
ASSIGN
  wTittel = "Endre varekost på salg: " + STRING(TODAY) + " " +
            STRING(TIME,"HH:MM") + " " + USERID("dictdb").

/* Henter ledig BatchNr */
RUN batchlogg.p (program-name(1),
                 wTittel,
                 OUTPUT wBatchNr).


/* Motposterer transene. */
FOR EACH TransRecid:
  FIND bTransLogg NO-LOCK WHERE
      RECID(bTransLogg) = TransRecid.TRecid NO-ERROR.
  IF AVAILABLE bTranslogg THEN
  DO:
    /* Setter transaksjonsnummer  */
    FIND LAST qTransLogg WHERE
      qTransLogg.Butik = bTransLogg.Butik
      USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE qTransLogg THEN
      wTransNr = qTransLogg.TransNr + 1.
    ELSE
      wTransNr = 1.
      
    CREATE qTransLogg.
    BUFFER-COPY bTransLogg TO qTransLogg
      ASSIGN
        qTransLogg.BatchNr = wBatchNr
        qTransLogg.TransNr = wTransNr
        qTransLogg.Antall  = bTransLogg.Antall * -1
        qTranslogg.Postert = FALSE
        qTransLogg.PostertDato = ?
        qTransLogg.PostertTid  = 0
        qTranslogg.SattVVarekost = TRUE
        wTransNr               = wTransNr + 1
        .
    CREATE qTransLogg.
    BUFFER-COPY bTransLogg TO qTransLogg
      ASSIGN
        qTransLogg.BatchNr = wBatchNr
        qTransLogg.TransNr = wTransNr
        qTransLogg.Antall  = bTransLogg.Antall
        qTranslogg.Postert = FALSE
        qTransLogg.PostertDato = ?
        qTransLogg.PostertTid  = 0
        qTranslogg.SattVVarekost = TRUE
        qTransLogg.VVareKost   = wVVarekost
        .
  END.
  DELETE TransRecid.
END.

/* Flagger batchen klar for oppdatering. */
RUN batchstatus.p (wBatchNr, 2).

/*{swn.i}*/
END. /* Slutt på frameScoop */
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

DEF VAR wSvar          AS LOG               NO-UNDO.
DEF VAR wLoop          AS INT               NO-UNDO.
DEF VAR wTittel        AS CHAR              NO-UNDO.
DEF VAR wAlleTranser   AS LOG INITIAL FALSE NO-UNDO.
DEF VAR wTransNr       AS INT INITIAL 0     NO-UNDO.
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
DO WITH FRAME DEFAULT-FRAME:
/* Alle transer som er merket skal motposteres */
IF BROWSE-TransLogg:NUM-SELECTED-ROWS > 0 THEN
  DO:
    wSvar = FALSE.
    MESSAGE "Skal de merkede transaksjonene motposteres?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft motpostering"
            UPDATE wSvar.
    IF wSvar = FALSE THEN 
      RETURN NO-APPLY "AVBRYT".
    ASSIGN 
      wAlleTranser = FALSE.
  END.
/* Alle transer i browser skal motposteres. */
ELSE DO: 
  MESSAGE "Skal ALLE transaksjonene i listen motposteres?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft motpostering"
          UPDATE wSvar.
  IF wSvar = FALSE THEN 
    RETURN NO-APPLY "AVBRYT".
  ASSIGN 
      wAlleTranser = TRUE.
END.

/* Adgangskontroll */
RUN d-adgangskontroll.d (INPUT "Adgangskontroll motpostering", INPUT wPwd).
IF RETURN-VALUE <> "OK" THEN
    RETURN "AVBRYT".

/* Tittel som batchen med motposteringene merkes med. */
ASSIGN
  wTittel = "Motpostering: " + STRING(TODAY) + " " +
            STRING(TIME,"HH:MM") + " " + USERID("dictdb").

/* Henter ledig BatchNr */
RUN batchlogg.p (program-name(1),
                 wTittel,
                 OUTPUT wBatchNr).
{sww.i}
IF wAlleTranser = FALSE THEN DO TRANSACTION:

  MOTPOSTER_LOOP:
  DO wLoop = BROWSE-TransLogg:NUM-SELECTED-ROWS TO 1 BY -1:
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
ELSE DO TRANSACTION:
  /* Alle transaksjoner */
  IF wTTID = ? THEN
    FOR EACH qTransLogg EXCLUSIVE-LOCK WHERE
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
END. /* TRANSACTION */  

/* Motposterer transene. */
FOR EACH TransRecid:
  FIND bTransLogg NO-LOCK WHERE
      RECID(bTransLogg) = TransRecid.TRecid NO-ERROR.
  IF AVAILABLE bTranslogg THEN
  DO:
    /* Setter transaksjonsnummer  */
    FIND LAST qTransLogg WHERE
      qTransLogg.Butik = bTransLogg.Butik
      USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE qTransLogg THEN
      wTransNr = qTransLogg.TransNr + 1.
    ELSE
      wTransNr = 1.
      
    CREATE qTransLogg.
    BUFFER-COPY bTransLogg TO qTransLogg
      ASSIGN
        qTransLogg.BatchNr = wBatchNr
        qTransLogg.TransNr = wTransNr
        qTransLogg.Antall  = bTransLogg.Antall * -1
        qTranslogg.Postert = FALSE
        qTransLogg.PostertDato = ?
        qTransLogg.PostertTid  = 0
        .
  END.
  DELETE TransRecid.
END.

/* Flagger batchen klar for oppdatering. */
RUN batchstatus.p (wBatchNr, 2).

{swn.i}
END. /* Slutt på frameScoop */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTopp C-Win 
PROCEDURE MoveToTopp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FRAME {&FRAME-NAME}:MOVE-TO-TOP().
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
    DEF VAR wW AS widget NO-UNDO.
  
    RUN lockwindowupdate(FRAME {&FRAME-NAME}:hwnd).

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
    
    RUN lockwindowupdate(0).
    
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
  DEF VAR wWhere AS CHAR NO-UNDO.
  
  RUN SettWhereSats (OUTPUT wWhere).

  ASSIGN
    wAktivQString = wSortCol:PRIVATE-DATA
    wAktivQString = IF wWhere <> "" AND "{&QWhere}" <> "" 
                      THEN REPLACE(wAktivQString, "WHERE", wWhere)
                    ELSE IF wWhere <> ""
                      THEN REPLACE(wAktivQString, "NO-LOCK", wWhere)
                    ELSE wAktivQString.

/* message wAktivQString view-as alert-box. */

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
        IF wBlank = ? THEN
          REPOSITION {&BROWSE-NAME} TO ROWID rowid(b{&br-tabell}) NO-ERROR.
        ASSIGN wAktivFillIn:SCREEN-VALUE = "".
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEntry C-Win 
PROCEDURE SetEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.

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
  DEF OUTPUT PARAMETER wWhere AS CHAR NO-UNDO.

  IF FRAME DEFAULT-FRAME:visible = FALSE THEN
    RETURN.

  FRAME-SCOOP:
  DO WITH FRAME DEFAULT-FRAME:

  /* Bygger Where sats */
  ASSIGN
      wWhere = ""
      FI-Storl
      .
  RUN FiksStorl IN wLibHandle (INPUT-OUTPUT FI-Storl). /* Størrelse i standard SkoTex */
  ASSIGN
      FI-Storl:SCREEN-VALUE = FI-Storl
      .

  IF (CB-TTId <> wAlle AND CB-TTId <> "") THEN 
      ASSIGN
        wWhere = wWhere + " " + (IF wWhere = "" THEN "" ELSE "and ") + "TransLogg.TTId = " + entry(1,CB-TTId,':')
        .
  

  IF FI-Storl:SCREEN-VALUE <> "" THEN 
      ASSIGN
        wWhere = wWhere + " " + (IF wWhere = "" THEN "" ELSE "and ") + "TransLogg.Storl = ~'" + FI-Storl:SCREEN-VALUE + "~'"
              .
  
  IF int(FI-ButikkNr:SCREEN-VALUE) <> 0 THEN
      ASSIGN
          wWhere = wWhere + " " + (IF wWhere = "" THEN "" ELSE "and ") + "TransLogg.Butik = " + FI-ButikkNr:SCREEN-VALUE
      .
        
  IF int(FI-OvButik:SCREEN-VALUE) <> 0 THEN
      ASSIGN
        wWhere = wWhere + " " + (IF wWhere = "" THEN "" ELSE "and ") + "TransLogg.OvButik = " + FI-OvButik:SCREEN-VALUE
        .

  IF INPUT T-VisAlle = FALSE THEN
      ASSIGN
        wWhere = wWhere + " " + (IF wWhere = "" THEN "" ELSE "and ") + "TransLogg.Postert = false"
        .

  IF INPUT FI-FraDato <> ? THEN 
      ASSIGN
        wWhere = wWhere + " " + (IF wWhere = "" THEN "" ELSE "and ") + "TransLogg.Dato >= ~'" + FI-FraDato:SCREEN-VALUE + "~'"
              .
  IF INPUT FI-TilDato <> ? THEN 
      ASSIGN
        wWhere = wWhere + " " + (IF wWhere = "" THEN "" ELSE "and ") + "TransLogg.Dato <= ~'" + FI-TilDato:SCREEN-VALUE + "~'"
              .
  ASSIGN
    wWhere = IF wWhere <> "" AND "{&QWhere}" <> "" 
               THEN "where " + wWhere + " and "
             ELSE IF wWhere <> "" 
               THEN "NO-LOCK where " + wWhere 
             ELSE IF "{&QWhere}" <> ""
               THEN ""
             ELSE "NO-LOCK".
    
  END. /* FRAME SCOOP */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTmpChild C-Win 
PROCEDURE SlettTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tmpChild:
        IF VALID-HANDLE(tmpChild.wChild) THEN
            DELETE PROCEDURE tmpChild.wChild.
    END.

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
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = ROWID({&br-tabell}) NO-LOCK NO-ERROR.

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
  DEF VAR wKlokken AS CHAR NO-UNDO.
  DEF VAR wTid     AS INT  NO-UNDO.
  
  IF AVAILABLE BatchLogg THEN
    wTid = BatchLogg.RegistrertTid.
  ELSE
    wTid = 0.
  
  
  IF wTid <> 0 THEN
    wKlokken = string(wTid,"HH:MM:SS").
  ELSE
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

