&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tMedTrans NO-UNDO LIKE MedTrans.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 
  STEP 1: V�lj tabell i browsern
  STEP 2: Definiera de f�lt du �nskar i din browser
          Kryssa av enabled p� de f�lt du �nskar sortering
  STEP 3: G�r de andringar som beh�vs i alla scope.
          Sorttype skall ha lika m�nga entries som antal enablade f�lt.
              - Till�tna v�rden = "" (blank -> BY = default)
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
def input parameter wMedlemsRecid      as recid  no-undo.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Parameters Definitions ---                                           */
&scope br-tabell   tMedTrans
&scope Sorttype    ,,,
&scope BrowseIdx   MedTrans,OppslagDatoTid,ArtButStr,OppslagStr
&scope Sokvillkor  >=,>=,>=,>=
&scope InitIdx     MedTrans
&scope ip-felt     TransNr
/* Om du �nskar input parameter. Ger en startup-record */ 
/*
&scope ip-variabel w{&ip-felt}
*/

&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.TransNr) ~
                                                else "".
&scope QWhere '{&br-tabell}.MedlemsNr > 0'  + ' XWHERE '
/*   'Ordre.Ordrenr > ' + STRING({&ip-variabel}) */
&scope BrowseQ     FOR EACH {&br-tabell} NO-LOCK XWHERE XSORTTYPE XSORT INDEXED-REPOSITION
&scope BrowseSQ    FOR EACH b{&br-tabell} NO-LOCK WHERE XWHERE b{&br-tabell}.XFIELD XSOKV XFILL ~
                           USE-INDEX XIDX MAX-ROWS 1

/* Parameter Definisjoner ---                                           */
&IF LENGTH("{&ip-variabel}") > 0 &THEN

  &scope return-ip   ASSIGN {&ip-variabel} = {&br-tabell}.{&ip-felt}
  &scope init-phrase FIND b{&br-tabell} WHERE b{&br-tabell}.{&ip-felt} = ~
                        {&ip-variabel} USE-INDEX {&InitIdx} NO-LOCK NO-ERROR.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} INIT 6 NO-UNDO.
  &ELSE
    DEFINE INPUT-OUTPUT PARAMETER {&ip-variabel} LIKE {&br-tabell}.{&ip-felt} NO-UNDO.
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
DEFINE VAR wBlank            AS LOG         NO-UNDO.
define var wRetStatus        as log         no-undo.
def    var wTypeBeskr        as char format "x(20)" no-undo.
def    var wGruppeBeskr      as char format "x(20)" no-undo.
define var wExcEkstent       as char        no-undo.
DEF    VAR wTTIdListe        AS CHAR        NO-UNDO.
DEF    VAR wDb%              AS DEC  NO-UNDO.
DEF    VAR wRab%             AS DEC  NO-UNDO.
DEF    VAR wVAreKost         AS DEC  NO-UNDO.

{runlib.i}
DEF BUFFER bufMedlem FOR Medlem.

DEFINE TEMP-TABLE tmpMedlem2 LIKE Medlem  /* tmpMedlem opptatt i Medlemliste.i */
       FIELD Ordning AS INTE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-tMedTrans
&Scoped-define QUERY-NAME QUERY-Alle

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tMedTrans btMedTrans tmpMedlem2

/* Definitions for BROWSE BROWSE-tMedTrans                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-tMedTrans tMedTrans.MedlemsNr ~
tMedTrans.Butik tMedTrans.Dato wTypeBeskr tMedTrans.ArtikkelNr tMedTrans.Vg ~
tMedTrans.LopNr tMedTrans.Storl tMedTrans.Antall tMedTrans.Pris ~
tMedTrans.Mva tMedTrans.RabKr tMedTrans.KortNr tMedTrans.BatchNr ~
tMedTrans.TransNr wRab% tMedTrans.KassaNr tMedTrans.BongId ~
tMedTrans.BongLinjeNr wDb% tMedTrans.ForsNr tMedTrans.VVarekost ~
tMedTrans.LevNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-tMedTrans tMedTrans.Butik ~
tMedTrans.ArtikkelNr tMedTrans.LopNr tMedTrans.Storl 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-tMedTrans tMedTrans
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-tMedTrans tMedTrans
&Scoped-define QUERY-STRING-BROWSE-tMedTrans FOR EACH tMedTrans NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-tMedTrans OPEN QUERY BROWSE-tMedTrans FOR EACH tMedTrans NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-tMedTrans tMedTrans
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-tMedTrans tMedTrans


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for QUERY QUERY-Alle                                     */
&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define QUERY-STRING-QUERY-Alle FOR EACH btMedTrans NO-LOCK where   btMedTrans.MedlemsNr = Medlem.MedlemsNr BY btMedlem.MedlemsNr BY btMedlem.TransNr
&Scoped-define OPEN-QUERY-QUERY-Alle OPEN QUERY {&SELF-NAME} FOR EACH btMedTrans NO-LOCK where   btMedTrans.MedlemsNr = Medlem.MedlemsNr BY btMedlem.MedlemsNr BY btMedlem.TransNr.
&Scoped-define TABLES-IN-QUERY-QUERY-Alle btMedTrans
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Alle btMedTrans


/* Definitions for QUERY QUERY-tmp                                      */
&Scoped-define SELF-NAME QUERY-tmp
&Scoped-define OPEN-QUERY-QUERY-tmp /* OPEN QUERY {&SELF-NAME} FOR EACH tmpMedlem2 NO-LOCK BY tmpMedlem2.Ordning. */.
&Scoped-define TABLES-IN-QUERY-QUERY-tmp tmpMedlem2
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-tmp tmpMedlem2


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 RECT-52 B-VisTrans B-Oppdater ~
BUTTON-ArtKort FILL-IN-SOK-INTE FILL-IN-SOK-DECI FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DATE CB-TTId BUTTON-Sok BROWSE-tMedTrans 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-INTE FILL-IN-SOK-DECI ~
FILL-IN-SOK-CHAR FILL-IN-SOK-DATE CB-TTId 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Prep-Private-Data C-Win 
FUNCTION Prep-Private-Data RETURNS CHARACTER
  ( INPUT wQueryCol AS WIDGET,INPUT wQueryCol# AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Oppdater 
     LABEL "&Vis transaksjoner for medlem og koblede medlemmer" 
     SIZE 57 BY 1.14.

DEFINE BUTTON B-VisTrans 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.1.

DEFINE BUTTON BUTTON-ArtKort 
     LABEL "Arti&kkelkort..." 
     SIZE 17 BY 1.14.

DEFINE BUTTON BUTTON-Sok 
     LABEL "S�k" 
     SIZE 10.2 BY 1 TOOLTIP "S�k i AKTIV kollonne".

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

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 204 BY .1.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 204 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-tMedTrans FOR 
      tMedTrans SCROLLING.

DEFINE QUERY QUERY-Alle FOR 
      btMedTrans SCROLLING.

DEFINE QUERY QUERY-tmp FOR 
      tmpMedlem2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-tMedTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-tMedTrans C-Win _STRUCTURED
  QUERY BROWSE-tMedTrans NO-LOCK DISPLAY
      tMedTrans.MedlemsNr FORMAT ">>>>>>>>>>>>9":U
      tMedTrans.Butik FORMAT ">>>>>9":U
      tMedTrans.Dato FORMAT "99/99/9999":U
      wTypeBeskr COLUMN-LABEL "TransType" FORMAT "x(10)":U WIDTH 11
      tMedTrans.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      tMedTrans.Vg FORMAT "zzzzz9":U
      tMedTrans.LopNr FORMAT "->>>>>9":U
      tMedTrans.Storl FORMAT "x(10)":U
      tMedTrans.Antall FORMAT "-zz,zzz,zz9":U
      tMedTrans.Pris FORMAT "-zz,zzz,zz9.99":U
      tMedTrans.Mva FORMAT "->,>>>,>>9.99":U
      tMedTrans.RabKr FORMAT "->,>>>,>>9.99":U
      tMedTrans.KortNr FORMAT "X(22)":U
      tMedTrans.BatchNr FORMAT "zzzzzzzz9":U
      tMedTrans.TransNr FORMAT "zz,zzz,zz9":U
      wRab% COLUMN-LABEL "Rab%" FORMAT "->>9.9":U
      tMedTrans.KassaNr FORMAT "zzz9":U
      tMedTrans.BongId FORMAT "zz,zzz,zz9":U
      tMedTrans.BongLinjeNr FORMAT "zzzzz9":U
      wDb% COLUMN-LABEL "Db%" FORMAT "->>,>>>,>>9.99":U
      tMedTrans.ForsNr FORMAT ">>>>>9":U
      tMedTrans.VVarekost FORMAT "-z,zzz,zz9.99":U
      tMedTrans.LevNr FORMAT "zzzzz9":U
  ENABLE
      tMedTrans.Butik
      tMedTrans.ArtikkelNr
      tMedTrans.LopNr
      tMedTrans.Storl
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SIZE 204 BY 20.71 ROW-HEIGHT-CHARS .63 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-VisTrans AT ROW 1.24 COL 72
     B-Oppdater AT ROW 1.24 COL 77
     BUTTON-ArtKort AT ROW 1.24 COL 188
     FILL-IN-SOK-INTE AT ROW 1.33 COL 3 NO-LABEL
     FILL-IN-SOK-DECI AT ROW 1.33 COL 3 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.33 COL 3 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.33 COL 3 NO-LABEL
     CB-TTId AT ROW 1.33 COL 32 COLON-ALIGNED NO-LABEL
     BUTTON-Sok AT ROW 1.38 COL 22.4
     BROWSE-tMedTrans AT ROW 2.91 COL 1
     RECT-51 AT ROW 1.1 COL 1
     RECT-52 AT ROW 2.52 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.71
         SIZE 204.6 BY 22.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tMedTrans T "NEW SHARED" NO-UNDO skotex MedTrans
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Medlemstransaksjoner"
         HEIGHT             = 31.38
         WIDTH              = 206.6
         MAX-HEIGHT         = 39.19
         MAX-WIDTH          = 230.4
         VIRTUAL-HEIGHT     = 39.19
         VIRTUAL-WIDTH      = 230.4
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-tMedTrans BUTTON-Sok DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-tMedTrans:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 3
       BROWSE-tMedTrans:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 481
       BROWSE-tMedTrans:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-tMedTrans
/* Query rebuild information for BROWSE BROWSE-tMedTrans
     _TblList          = "Temp-Tables.tMedTrans"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   = Temp-Tables.tMedTrans.MedlemsNr
     _FldNameList[2]   > Temp-Tables.tMedTrans.Butik
"tMedTrans.Butik" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.tMedTrans.Dato
     _FldNameList[4]   > "_<CALC>"
"wTypeBeskr" "TransType" "x(10)" ? ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tMedTrans.ArtikkelNr
"tMedTrans.ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.tMedTrans.Vg
     _FldNameList[7]   > Temp-Tables.tMedTrans.LopNr
"tMedTrans.LopNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tMedTrans.Storl
"tMedTrans.Storl" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = Temp-Tables.tMedTrans.Antall
     _FldNameList[10]   = Temp-Tables.tMedTrans.Pris
     _FldNameList[11]   = Temp-Tables.tMedTrans.Mva
     _FldNameList[12]   = Temp-Tables.tMedTrans.RabKr
     _FldNameList[13]   = Temp-Tables.tMedTrans.KortNr
     _FldNameList[14]   = Temp-Tables.tMedTrans.BatchNr
     _FldNameList[15]   = Temp-Tables.tMedTrans.TransNr
     _FldNameList[16]   > "_<CALC>"
"wRab%" "Rab%" "->>9.9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = Temp-Tables.tMedTrans.KassaNr
     _FldNameList[18]   = Temp-Tables.tMedTrans.BongId
     _FldNameList[19]   = Temp-Tables.tMedTrans.BongLinjeNr
     _FldNameList[20]   > "_<CALC>"
"wDb%" "Db%" "->>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   = Temp-Tables.tMedTrans.ForsNr
     _FldNameList[22]   = Temp-Tables.tMedTrans.VVarekost
     _FldNameList[23]   = Temp-Tables.tMedTrans.LevNr
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-tMedTrans */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Alle
/* Query rebuild information for QUERY QUERY-Alle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH btMedTrans NO-LOCK where
  btMedTrans.MedlemsNr = Medlem.MedlemsNr BY btMedlem.MedlemsNr BY btMedlem.TransNr.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 1.71 , 134 )
*/  /* QUERY QUERY-Alle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-tmp
/* Query rebuild information for QUERY QUERY-tmp
     _START_FREEFORM
/* OPEN QUERY {&SELF-NAME} FOR EACH tmpMedlem2 NO-LOCK BY tmpMedlem2.Ordning. */
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 1 , 134 )
*/  /* QUERY QUERY-tmp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Medlemstransaksjoner */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Medlemstransaksjoner */
DO:
  if can-find(first tmpChild where
               valid-handle(tmpChild.wChild)) then
    do:
      wBekreft = false.
      message 'Det er startet andre programmer fra dette vinduet.' skip
              'Avsluttes dette vinduet, vil alle underliggende programmer' skip
              'ogs� bli avsluttet.'
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


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-Win
ON CHOOSE OF B-Oppdater IN FRAME DEFAULT-FRAME /* Vis transaksjoner for medlem og koblede medlemmer */
DO:
  /* T�mmer temp-file */ 
  FOR EACH tMedTrans:
    DELETE tMedTrans.
  END.

  RUN ByggTempFile.

  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans C-Win
ON CHOOSE OF B-VisTrans IN FRAME DEFAULT-FRAME /* Vis transaksjon */
DO:
  if not available tMedTrans then
    return no-apply.
    
  /* Kall til rutine for visning av transaksjon. */
  RUN gviskvittokopi.w (tMedTrans.Butik,1,tMedTrans.KassaNr,tMedTrans.Dato,tMedTrans.BongId).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-tMedTrans
&Scoped-define SELF-NAME BROWSE-tMedTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tMedTrans C-Win
ON ANY-PRINTABLE OF BROWSE-tMedTrans IN FRAME DEFAULT-FRAME
DO:
  if lastkey <> 32 then
    do:
      RUN SD-ANY-PRINTABLE.
      RETURN NO-APPLY.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tMedTrans C-Win
ON CURSOR-LEFT OF BROWSE-tMedTrans IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tMedTrans C-Win
ON CURSOR-RIGHT OF BROWSE-tMedTrans IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tMedTrans C-Win
ON HOME OF BROWSE-tMedTrans IN FRAME DEFAULT-FRAME
DO:
  APPLY "ENTRY" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tMedTrans C-Win
ON ROW-DISPLAY OF BROWSE-tMedTrans IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE tMedTrans THEN
  DO:
    ASSIGN
        wRab% = (tMedTrans.RabKr / tMedTrans.Pris) * 100
        .
    IF AVAILABLE Moms THEN RELEASE Moms.
    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = tMedTrans.Vg NO-ERROR.
    IF available VarGr THEN
        FIND Moms OF VarGr NO-LOCK NO-ERROR.

    IF tMedTrans.VVareKost <> 0 THEN
        wVareKost = tMedTrans.VVareKost.
    ELSE DO:
        FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = tMedTrans.ArtikkelNr AND
            Lager.Butik      = tMedTrans.Butik NO-ERROR.
        IF AVAILABLE Lager THEN
            wVareKost = Lager.VVareKost.
        ELSE
            wVareKost = 0.
    END.
    RUN FinnDb% IN wLibHandle (tMedTrans.Pris - tMedTrans.RabKr,
                               IF AVAILABLE Moms THEN Moms.MomsProc ELSE 0,
                               wVareKost).
    ASSIGN wDb% = DEC(RETURN-VALUE).

    ASSIGN
      wTypeBeskr = ENTRY(tMedTrans.TTId,wTTIdListe)
      .
  END.
  ELSE
    ASSIGN
      wTypeBeskr = ""
      wDb%       = 0
      wRab%      = 0
      .

  IF wDb% < 0 THEN
    ASSIGN
      wDb%:bgcolor in browse {&BROWSE-NAME} = 12.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tMedTrans C-Win
ON START-SEARCH OF BROWSE-tMedTrans IN FRAME DEFAULT-FRAME
DO:
  RUN lockwindowupdate(FRAME {&FRAME-NAME}:HWND).
  DEF VAR wSearchCol  AS WIDGET NO-UNDO.
  DEF VAR wQString    AS CHAR   NO-UNDO.
  DEF VAR wSortColIdx AS INTE   NO-UNDO.
  ASSIGN wSearchCol = SELF:CURRENT-COLUMN.
  def var wWhere as char no-undo.
  
  run SettWhereSats (output wWhere).
  
  /* Bytter kollonne. */                                                     
  IF wSortCol <> SELF:CURRENT-COLUMN AND
                   LOOKUP(wSearchCol:NAME,wSearchCols) > 0 THEN DO:
      ASSIGN wSortCol = SELF:CURRENT-COLUMN.
      RUN SortNyCol.

  END.
  /* Togler stigende/synkende sortering i kollonnen.          */
  /* Sortering endres hver gang det klikkes i samme kollonne. */
  ELSE IF LOOKUP(wSearchCol:NAME,wSearchCols) > 0 AND
          LOOKUP("USE-INDEX",wQ:PREPARE-STRING," ") = 0 THEN DO:
          
      ASSIGN wQString    = wQ:PREPARE-STRING
             wSortColIdx = LOOKUP("{&br-tabell}." + wSearchCol:NAME,wQString," ")
             wQString    = IF ENTRY(wSortColIdx + 1,wQString," ") = "DESCENDING" THEN
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME + " DESCENDING","{&br-tabell}." + wSearchCol:NAME)
                        ELSE
                 REPLACE(wQString,"{&br-tabell}." + wSearchCol:NAME, 
                           "{&br-tabell}." + wSearchCol:NAME + " DESCENDING")
             wQString = IF wWhere <> ""
                          THEN REPLACE(wQString, trim(wWhere), "XWHERE") /* Preper tilbake f�r private-data oppdateres. */
                          ELSE REPLACE(wQString, "BY", "XWHERE BY")
             wSearchCol:PRIVATE-DATA = wQString /* Prep av private data med XWHERE */
             wQString = REPLACE(wQString, "XWHERE",wWhere). /* Og preper igjen */
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


&Scoped-define SELF-NAME BUTTON-ArtKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ArtKort C-Win
ON CHOOSE OF BUTTON-ArtKort IN FRAME DEFAULT-FRAME /* Artikkelkort... */
DO:
  if available tMedTrans then
  DO:
      FIND ArtBas NO-LOCK WHERE
          ArtBAs.ArtikkelNr = tMedTrans.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN DO:
          ASSIGN wCurrent-Window:SENSITIVE = FALSE.
          run w-vartkor  (input recid(ArtBas), "ENDRE") NO-ERROR.
          ASSIGN wCurrent-Window:SENSITIVE = TRUE.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok C-Win
ON CHOOSE OF BUTTON-Sok IN FRAME DEFAULT-FRAME /* S�k */
DO:
   /* DEFINE VAR wBlank AS LOG  NO-UNDO.*/
   DEFINE VAR wSQStr AS CHAR NO-UNDO. 
   DEFINE VAR wChar AS  CHAR NO-UNDO.
   DEFINE VAR wXSokv AS CHAR NO-UNDO.
   DEFINE VAR wQWhere    AS CHAR NO-UNDO. 
   def    var wWhere as char no-undo.
  
   run SettWhereSats (output wWhere).
   wWhere = REPLACE(wWhere,"{&br-tabell}","b{&br-tabell}"). /* Endrer buffernavn */
   /*
  -------------- 
  assign
    wAktivQString = wSortCol:PRIVATE-DATA
    wAktivQString = REPLACE(wAktivQString, "XWHERE", wWhere).
  -------------------
  */ 
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
              wQWhere = REPLACE(wQWhere,"XWHERE",wWhere)
              wSQStr = REPLACE(wSQStr,"XWHERE",wQWhere)
              .

MESSAGE "Etter replace wSQStr:" wSQStr  SKIP(2)
        "wQWHere:" wQWhere
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              
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


&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define SELF-NAME QUERY-tmp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.                    

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  do:
    /*RUN SaveBrowseSettings.*/
    for each tmpChild:
      if valid-handle(tmpChild.wChild) then
        delete procedure tmpChild.wChild.
    end.
    if valid-handle(wParentHandle) then
      run SlettProg in wParentHandle.
    RUN disable_UI.
  end.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
{syspara.i 1 100 1 wAlle}

/* HotKeyS�k - DYYYYRT */
/*
on ALT-E of frame DEFAULT-FRAME anywhere 
  do:
    apply "CHOOSE":U to BUTTON-Endre in frame DEFAULT-FRAME.
  end.
*/

/* Henter medlemsposten */
FIND Medlem NO-LOCK WHERE
  RECID(Medlem) = wMedlemsRecid.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN InitVars. /* inkl open-query */
    IF RETURN-VALUE = "FEIL" THEN
       RETURN.
    RUN ByggTempFile.
/*    IF "Komnavn" <> wSortCol:NAME THEN
        RUN QueryCustomSettings ("Komnavn"). */
    RUN SD-QUERY-OPEN.

    IF RETURN-VALUE = "FEIL" THEN
        LEAVE MAIN-BLOCK.
    
    RUN InitType.

    RUN enable_UI.
    {lng.i} 

    /* {browsesettings.i {&BROWSE-NAME}}  Konflikt med filter.
    IF BROWSE {&BROWSE-NAME}:CURRENT-COLUMN <> wSortCol THEN DO:
        wSortCol = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
        RUN QueryCustomSettings (wSortCol:NAME).
    END.
    */
    
    RUN LabelColor.
    RUN Move-Fill-To-Top. 
    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    &ENDIF 
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END. 
    ELSE IF AVAILABLE {&br-tabell} THEN
      do:
        /*find MedlemsType of Medlem no-lock no-error.*/
        REPOSITION {&BROWSE-NAME} TO ROW 1.
      end.
    ASSIGN {&WINDOW-NAME}:HIDDEN = FALSE.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTempFile C-Win 
PROCEDURE ByggTempFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT AVAILABLE Medlem THEN
      RETURN NO-APPLY "AVBRYT".

  /* Bygger egne transaksjoner */
  FOR EACH MedTrans NO-LOCK WHERE
      MedTrans.MedlemsNr = Medlem.MedlemsNr:
      CREATE tMedTrans.
      BUFFER-COPY MedTrans TO tMedTrans.
  END.

  /* Bygger for koblede medlemmer. */
  FOR EACH bufMedlem NO-LOCK WHERE
      bufMedlem.HovedMedlemsNr = Medlem.MedlemsNr:

      FOR EACH MedTrans NO-LOCK WHERE
          MedTrans.MedlemsNr = bufMedlem.MedlemsNr:
          CREATE tMedTrans.
          BUFFER-COPY MedTrans TO tMedTrans.
      END.
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
  DEF INPUT PARAMETER wRecid AS RECID NO-UNDO.
  
  ASSIGN
    wMedlemsRecid = wRecid.
    
  /* Henter Kundesposten */
  FIND Medlem NO-LOCK WHERE
    RECID(Medlem) = wMedlemsRecid NO-ERROR.

  IF NOT AVAILABLE Medlem THEN 
    RETURN.
  ASSIGN CB-TTId:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,CB-TTId:LIST-ITEMS)
         CB-TTId.
  RUN InitVars.
  /* T�mmer temp-file */ 
  FOR EACH tMedTrans:
    DELETE tMedTrans.
  END.

  RUN ByggTempFile.

  RUN SD-CURSOR (" ").

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
  DISPLAY FILL-IN-SOK-INTE FILL-IN-SOK-DECI FILL-IN-SOK-CHAR FILL-IN-SOK-DATE 
          CB-TTId 
      WITH FRAME DEFAULT-FRAME.
  ENABLE RECT-51 RECT-52 B-VisTrans B-Oppdater BUTTON-ArtKort FILL-IN-SOK-INTE 
         FILL-IN-SOK-DECI FILL-IN-SOK-CHAR FILL-IN-SOK-DATE CB-TTId BUTTON-Sok 
         BROWSE-tMedTrans 
      WITH FRAME DEFAULT-FRAME.
  VIEW FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitType C-Win 
PROCEDURE InitType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign
    wTTIdListe = ""
    CB-TTId:LIST-ITEMS   IN FRAME default-frame = "."
    CB-TTId:list-items   in frame DEFAULT-FRAME = wAlle
    CB-TTId:screen-value in frame DEFAULT-FRAME = wAlle.

  for each TransType NO-LOCK:
    assign
     wTTIdListe = wTTIdListe +
                  (IF wTTIdListe <> ""
                     THEN ","
                     ELSE "") +  
                  TransType.Beskrivelse.
    IF CAN-DO("1,3,10",STRING(TransType.TTId)) THEN
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
        MESSAGE "Du m� 'enabla' minst ett felt" view-as alert-box.
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
                "Aktuellt v�rde: " + wSokvillkor
                 VIEW-AS ALERT-BOX ERROR.
        RETURN "FEIL".
    END.
    DO wIdx = 1 TO wAntSortCol:
        CASE ENTRY(wIdx,wSorttype):
            WHEN "" OR WHEN "BY" OR WHEN "BY DESCENDING" OR WHEN "USE-INDEX" THEN.
            OTHERWISE DO:
                          MESSAGE "Till�tna entries i 'scope Sort' = ''(tomt),BY,BY DESCENDING OCH USE-INDEX"
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

    /* Blanker Labelfelt - InitVars kan kj�res flere ganger.           */
    /* Det skjer n�r programmet kj�res fra et hovedprogram Persistent. */
    ASSIGN 
        wW = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(wW):
        IF LOOKUP(wW:NAME,wSearchCols) > 0 THEN
            ASSIGN 
              wW:LABEL         = trim(entry(1,wW:LABEL,"*"))
              wW:LABEL-BGCOLOR = IF wOrgBgCol <> 0 
                                   THEN wOrgBgCol
                                   ELSE wW:LABEL-BGCOLOR
              .
        ASSIGN wW = wW:NEXT-COLUMN.
    END.
    /* Ferdig med � blanke labler */

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
  
   IF FRAME Default-Frame:MOVE-TO-TOP() THEN.

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
    wAktivQString = REPLACE(wAktivQString, "XWHERE", wWhere).
  /*
  MESSAGE "SD-Query-Open:" SKIP
          "where:" wWHERE SKIP
          "wSortCol:PRIVATE-DATA:" wSortCol:PRIVATE-DATA SKIP
          "Ny query:" wAktivQString
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */

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
    wWhere = wWhere + (if input CB-TTId <> wAlle
               then (if "{&QWhere}" = "" then "" else " and ") + " tMedTrans.TTId = " + entry(1,CB-TTId:screen-value,":")
               else "") 
    /*
    wWhere = wWhere + " " +
             (if input CB-Gruppe <> wAlle
               then (if wWhere = "" then "" else "and ") + "Medlem.MedGruppe = " + entry(1,input CB-Gruppe,":")
               else "")
    wWhere = wWhere + " " +
             (if input FI-Navn <> "*"
               then (if wWhere = "" then "" else "and ") + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "Medlem.EtterNavn matches '"
                       else "Medlem.EtterNavn begins '") + 
                     input FI-Navn + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "*"
                       else "") + "'"
               else "")
    wWhere = if wWhere <> "" then "NO-LOCK where " + wWhere else "".
    */
    .
  end. /* FRAME SCOOP */       
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

