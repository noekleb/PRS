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
&scope br-tabell   LevBas
&scope Sorttype    BY,BY,BY,BY,BY
&scope BrowseIdx   LevIn,LevNamn,LevTel,LevAdr,LevPoNr
&scope Sokvillkor  >=,>=,>=,>=,>=
&scope InitIdx     LevIn  
&scope ip-felt     
/* Om du önskar input parameter. Ger en startup-record */ 
/*
&scope ip-variabel w{&ip-felt}
*/

&scope assign-retur-verdi ASSIGN retur-verdi = if available {&br-tabell} ~
                                                then STRING({&br-tabell}.LevNr) ~
                                                else "".

&scope BrowseQ     FOR EACH {&br-tabell} NO-LOCK XSORTTYPE XSORT INDEXED-REPOSITION
&scope BrowseSQ    FOR EACH b{&br-tabell} NO-LOCK WHERE b{&br-tabell}.XFIELD XSOKV XFILL ~
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
DEFINE VAR wBlank            AS LOG         NO-UNDO.
DEFINE VAR wRetStatus        AS LOG         NO-UNDO.
DEFINE VAR wPostadresse      AS CHAR LABEL "Postadresse" NO-UNDO.
DEFINE VAR wExcEkstent       AS CHAR        NO-UNDO.
DEF VAR cFileName        AS CHAR  NO-UNDO. 
DEF VAR cExcEkstent      AS CHAR  NO-UNDO.
DEF VAR cKunde           AS CHAR  NO-UNDO.
DEF VAR cSkoTex          AS CHAR  NO-UNDO.
DEFINE VARIABLE bSperrNy AS LOG NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.

{runlib.i}
/* {vargrliste.i &New="New"} */
DEFINE TEMP-TABLE tmpLevBas LIKE LevBas
       FIELD Ordning AS INTE.
DEF STREAM Eksport.
DEF STREAM sExportFile.
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-LevBas
&Scoped-define QUERY-NAME QUERY-Alle

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LevBas bLevBas tmpLevBas

/* Definitions for BROWSE BROWSE-LevBas                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-LevBas LevBas.levnr LevBas.levnamn ~
LevBas.KjedeAvtale LevBas.levtel LevBas.levadr LevBas.levponr ~
Postadr() @ wPostadresse LevBas.levland LevBas.valkod LevBas.RegistrertDato ~
LevBas.EDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-LevBas LevBas.levnr ~
LevBas.levnamn LevBas.levtel LevBas.levadr LevBas.levponr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-LevBas LevBas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-LevBas LevBas
&Scoped-define QUERY-STRING-BROWSE-LevBas FOR EACH LevBas NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-LevBas OPEN QUERY BROWSE-LevBas FOR EACH LevBas NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-LevBas LevBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-LevBas LevBas


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for QUERY QUERY-Alle                                     */
&Scoped-define SELF-NAME QUERY-Alle
&Scoped-define QUERY-STRING-QUERY-Alle FOR EACH bLevBas NO-LOCK BY LevNr
&Scoped-define OPEN-QUERY-QUERY-Alle OPEN QUERY {&SELF-NAME} FOR EACH bLevBas NO-LOCK BY LevNr.
&Scoped-define TABLES-IN-QUERY-QUERY-Alle bLevBas
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-Alle bLevBas


/* Definitions for QUERY QUERY-tmp                                      */
&Scoped-define SELF-NAME QUERY-tmp
&Scoped-define QUERY-STRING-QUERY-tmp FOR EACH tmpLevBas NO-LOCK BY tmpLevBas.Ordning
&Scoped-define OPEN-QUERY-QUERY-tmp OPEN QUERY {&SELF-NAME} FOR EACH tmpLevBas NO-LOCK BY tmpLevBas.Ordning.
&Scoped-define TABLES-IN-QUERY-QUERY-tmp tmpLevBas
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-tmp tmpLevBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 RECT-51 RECT-52 FILL-IN-SOK-DECI ~
FILL-IN-SOK-INTE FILL-IN-SOK-DATE FILL-IN-SOK-CHAR BUTTON-Ny BUTTON-Endre ~
BUTTON-Slett B-Excel B-Rapp B-Oppdater Btn_Help Btn_OK BUTTON-Sok ~
BROWSE-LevBas 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-DECI FILL-IN-SOK-INTE ~
FILL-IN-SOK-DATE FILL-IN-SOK-CHAR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Postadr C-Win 
FUNCTION Postadr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel":U
     LABEL "Excel..." 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter alle eller merkede linjer til Excel. Alt-X.".

DEFINE BUTTON B-Oppdater 
     LABEL "&Oppdater browser" 
     SIZE 26 BY 1.14.

DEFINE BUTTON B-Print 
     IMAGE-UP FILE "icon/e-print":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1 TOOLTIP "Utskrift av adresseetiketter".

DEFINE BUTTON B-Rapp 
     LABEL "Htm" 
     SIZE 4.6 BY 1.1.

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

DEFINE BUTTON BUTTON-Endre 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Endre.." 
     SIZE 4.6 BY 1.1 TOOLTIP "Endre  (Alt-E)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon/e-ny":U
     LABEL "&Ny.." 
     SIZE 4.6 BY 1.1 TOOLTIP "Opprette ny  (Alt-N)".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort (Alt-D)".

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1 TOOLTIP "Søk i AKTIV kollonne".

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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-LevBas FOR 
      LevBas SCROLLING.

DEFINE QUERY QUERY-Alle FOR 
      bLevBas SCROLLING.

DEFINE QUERY QUERY-tmp FOR 
      tmpLevBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-LevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-LevBas C-Win _STRUCTURED
  QUERY BROWSE-LevBas NO-LOCK DISPLAY
      LevBas.levnr COLUMN-LABEL "Levnr" FORMAT ">>>>>9":U
      LevBas.levnamn FORMAT "x(25)":U
      LevBas.KjedeAvtale FORMAT "*/":U COLUMN-FONT 6
      LevBas.levtel COLUMN-LABEL "Telefon" FORMAT "x(20)":U
      LevBas.levadr COLUMN-LABEL "Adresse" FORMAT "x(35)":U
      LevBas.levponr COLUMN-LABEL "Postnummer" FORMAT "x(6)":U
      Postadr() @ wPostadresse FORMAT "x(20)":U
      LevBas.levland COLUMN-LABEL "Land" FORMAT "x(15)":U
      LevBas.valkod COLUMN-LABEL "Valuta" FORMAT "x(3)":U
      LevBas.RegistrertDato FORMAT "99/99/9999":U
      LevBas.EDato FORMAT "99/99/9999":U
  ENABLE
      LevBas.levnr
      LevBas.levnamn
      LevBas.levtel
      LevBas.levadr
      LevBas.levponr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN MULTIPLE SIZE 155 BY 17.86 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-SOK-DECI AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-INTE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Ny AT ROW 1.48 COL 33
     BUTTON-Endre AT ROW 1.48 COL 38
     BUTTON-Slett AT ROW 1.48 COL 43
     B-Print AT ROW 1.48 COL 48
     B-Excel AT ROW 1.48 COL 53
     B-Rapp AT ROW 1.48 COL 58.2
     B-Oppdater AT ROW 1.48 COL 120
     Btn_Help AT ROW 1.48 COL 147
     Btn_OK AT ROW 1.48 COL 152
     BUTTON-Sok AT ROW 1.52 COL 21.4
     BROWSE-LevBas AT ROW 6.95 COL 2
     RECT-50 AT ROW 1 COL 1
     RECT-51 AT ROW 1.24 COL 1
     RECT-52 AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 157 BY 24.


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
         TITLE              = "Søkeliste leverandører"
         HEIGHT             = 23.91
         WIDTH              = 157
         MAX-HEIGHT         = 39.19
         MAX-WIDTH          = 230.4
         VIRTUAL-HEIGHT     = 39.19
         VIRTUAL-WIDTH      = 230.4
         MAX-BUTTON         = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-LevBas BUTTON-Sok DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Print IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-LevBas:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 2
       BROWSE-LevBas:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 481
       BROWSE-LevBas:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-LevBas
/* Query rebuild information for BROWSE BROWSE-LevBas
     _TblList          = "SkoTex.LevBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > SkoTex.LevBas.levnr
"LevBas.levnr" "Levnr" ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > SkoTex.LevBas.levnamn
"LevBas.levnamn" ? "x(25)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.LevBas.KjedeAvtale
"LevBas.KjedeAvtale" ? "*~~/" "logical" ? ? 6 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.LevBas.levtel
"LevBas.levtel" "Telefon" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > SkoTex.LevBas.levadr
"LevBas.levadr" "Adresse" "x(35)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > SkoTex.LevBas.levponr
"LevBas.levponr" "Postnummer" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Postadr() @ wPostadresse" ? "x(20)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > SkoTex.LevBas.levland
"LevBas.levland" "Land" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > SkoTex.LevBas.valkod
"LevBas.valkod" "Valuta" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = SkoTex.LevBas.RegistrertDato
     _FldNameList[11]   = SkoTex.LevBas.EDato
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-LevBas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-Alle
/* Query rebuild information for QUERY QUERY-Alle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH bLevBas NO-LOCK BY LevNr.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 3.14 , 96 )
*/  /* QUERY QUERY-Alle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-tmp
/* Query rebuild information for QUERY QUERY-tmp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tmpLevBas NO-LOCK BY tmpLevBas.Ordning.
     _END_FREEFORM
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 3.14 , 88 )
*/  /* QUERY QUERY-tmp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søkeliste leverandører */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søkeliste leverandører */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Søkeliste leverandører */
DO:
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Excel... */
DO:

    RUN Utskrift.
    /*RUN ExHtmRapp ("EX").*/
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-Win
ON CHOOSE OF B-Oppdater IN FRAME DEFAULT-FRAME /* Oppdater browser */
DO:
  RUN SD-CURSOR (" ").
  RETURN NO-APPLY.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Print C-Win
ON CHOOSE OF B-Print IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
/*
  def var wSvar as log no-undo.
  def var wLoop as int no-undo.
  
  /* Nullstiller logg */
  for each tmpVarGr: 
    delete tmpVarGr.
  end.
  /* EMPTY TEMP-TABLE tmpVarGr. */ /* [ NO-ERROR ] */
  wSvar = false.
  /* Bruker har valgt en eller flere linjer. */
  if BROWSE-VarGr:NUM-SELECTED-ROWS > 0 then
  do TRANSACTION:  
    message "Skal utskrift startes for de merkede varegruppene?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    if wSvar = false then 
      return no-apply.  

    {sww.i}
    do wLoop = 1 to BROWSE-VarGr:NUM-SELECTED-ROWS:
      wOk = BROWSE-VarGr:FETCH-SELECTED-ROW(wLoop).
      if not can-find(first tmpVarGr where
        tmpVarGr.VarGrRecid = recid(VarGr)) then
        do:
          create tmpVarGr.
          assign
            tmpVarGr.VarGrRecid = recid(VarGr).
        end.
    end.
    {swn.i}
  end.
  
  /* Ingen linjer er valgt. Alle linjer skal behandles. Bruker må bekrefte */
  else do:
    message "Skal utskrift startes for ALLE varegrupper?"
            view-as alert-box question buttons yes-no title "Bekreft utskrift"
            update wSvar.
    if wSvar = false then 
      return no-apply.  
    {sww.i}
    do:
      for each VarGr no-lock:
        if not can-find(first tmpVarGr where
          tmpVarGr.VarGrRecid = recid(VarGr)) then
          do:
            create tmpVarGr.
            assign
              tmpVarGr.VarGrRecid = recid(VarGr).
          end.
      end.  
    end.
    {swn.i}
  end. 
  
  if can-find(first tmpVarGr) then
    run vargrliste.p (300,3).
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapp C-Win
ON CHOOSE OF B-Rapp IN FRAME DEFAULT-FRAME /* Htm */
DO:
    RUN ExHtmRapp ("HTM").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-LevBas
&Scoped-define SELF-NAME BROWSE-LevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevBas C-Win
ON ANY-PRINTABLE OF BROWSE-LevBas IN FRAME DEFAULT-FRAME
DO:
  IF LASTKEY <> 32 THEN
    DO:
      RUN SD-ANY-PRINTABLE.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevBas C-Win
ON CURSOR-LEFT OF BROWSE-LevBas IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevBas C-Win
ON CURSOR-RIGHT OF BROWSE-LevBas IN FRAME DEFAULT-FRAME
DO:
  IF wAntSortCol < 2 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevBas C-Win
ON DEFAULT-ACTION OF BROWSE-LevBas IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Endre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevBas C-Win
ON END-MOVE OF BROWSE-LevBas IN FRAME DEFAULT-FRAME
DO:
  MESSAGE "gurre var her " VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevBas C-Win
ON HOME OF BROWSE-LevBas IN FRAME DEFAULT-FRAME
DO:
  APPLY "ENTRY" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevBas C-Win
ON START-SEARCH OF BROWSE-LevBas IN FRAME DEFAULT-FRAME
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
  SELF:SELECT-FOCUSED-ROW().
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
                     
  {&return-ip}
  .
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
      .
  &ELSE
      ASSIGN retur-verdi = "OK".
  &ENDIF
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME DEFAULT-FRAME /* Endre.. */
DO:
  DEF VAR wTmpChild AS HANDLE NO-UNDO.
  RUN w-vlevbas PERSISTENT SET wTmpChild (recid({&br-tabell}),"ENDRE").
  CREATE tmpChild.
  ASSIGN tmpChild.wChild = wTmpChild.
  RELEASE tmpChild.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny.. */
DO:
  IF bSperrNy = FALSE THEN 
    DO:
      MESSAGE 'Det er sperret for opprettelse av leverandører lokalt.' SKIP
              'Ditt system er koblet til et hovedkontor. Nyregistrering av leverandører skal skje der og overføres til din butikk.'
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
  DEF VAR wTmpChild AS HANDLE NO-UNDO.
  RUN w-vlevbas PERSISTENT SET wTmpChild (?,"NY").
  CREATE tmpChild.
  ASSIGN tmpChild.wChild = wTmpChild.
  RELEASE tmpChild.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Slette */
DO:
  DEF VAR wCurrRow AS INTE NO-UNDO.
  IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS > 0 AND 
     BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN DO:
      BROWSE {&BROWSE-NAME}:SCROLL-TO-SELECTED-ROW(1).
  END.
  IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS <> 1 THEN DO:
     IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 0 THEN
         MESSAGE "Du må velge 1 varegruppe." VIEW-AS ALERT-BOX INFORMATION
                      TITLE "Feil".
     ELSE DO:
         MESSAGE "Du kan bare velge 1 varegruppe." SKIP
                 "Vil du rense ditt utvalg?" VIEW-AS ALERT-BOX QUESTION
                 BUTTONS YES-NO TITLE "Feil" UPDATE wJa AS LOGI.
         IF wJa THEN
             BROWSE {&BROWSE-NAME}:DESELECT-ROWS().
         REPOSITION {&BROWSE-NAME} TO ROW 1.
     END.
     RETURN NO-APPLY.
  END.
  ASSIGN wCurrRow = BROWSE {&BROWSE-NAME}:FOCUSED-ROW.
  RUN w-vlevbas (recid({&br-tabell}),"SLETT").
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  wOk = BROWSE-{&br-tabell}:REFRESH().
  BROWSE-{&br-tabell}:SELECT-FOCUSED-ROW().
  RETURN NO-APPLY.  
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
   
   wBlank = FALSE.

   /*
   MESSAGE "wSearchCols:" wSearchCols SKIP
           "wAktivCol:" wAktivCol SKIP
           "wSokvillkor:" wSokvillkor SKIP
           "&BrowseSQ:" "{&BrowseSQ}" SKIP
           "wBrowseIdx:" wBrowseIdx SKIP
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   */
   IF wAktivFillIn:SCREEN-VALUE = "" THEN
       wBlank = TRUE.
   ELSE DO:
       ASSIGN wChar  = IF wAktivFillIn:DATA-TYPE BEGINS "CHAR" THEN '"' ELSE ''
              wSQStr = REPLACE("{&BrowseSQ}","XFIELD",ENTRY(wAktivCol,wSearchCols))
              wSQStr = REPLACE(wSQStr,"XSOKV",ENTRY(wAktivCol,wSokvillkor))
              wSQStr = IF ENTRY(wAktivCol,wBrowseIdx) <> "" THEN
                         REPLACE(wSQStr,"XIDX",ENTRY(wAktivCol,wBrowseIdx)) 
                       ELSE 
                           REPLACE(wSQStr,"USE-INDEX XIDX ","") 
              wSQStr = REPLACE(wSQStr,"XFILL",wChar + wAktivFillIn:SCREEN-VALUE + wChar).
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
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Søkeliste leverandører"
  &PreIClose      = "RUN SaveBrowseSettings."
  &PostIClose     = " "
  &PostDisable_ui = "RUN DelTmpChild.
                     &IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
                       return retur-verdi.
                     &else
                       message {&ip-variabel} retur-verdi view-as alert-box.
                     &endif"
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{syspara.i 16 2 3 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) THEN 
  bSperrNy = TRUE.

{syspara.i 1 100 1 wAlle}
{syspara.i 1 4 1 wExcEkstent}
wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.   

/* HotKeySøk - DYYYYRT */
ON ALT-N OF FRAME DEFAULT-FRAME ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Ny IN FRAME DEFAULT-FRAME.
  END.
ON ALT-E OF FRAME DEFAULT-FRAME ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Endre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-D OF FRAME DEFAULT-FRAME ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Slett IN FRAME DEFAULT-FRAME.
  END.
  /*
on ALT-P of frame DEFAULT-FRAME anywhere 
  do:
    apply "CHOOSE":U to B-Print in frame DEFAULT-FRAME.
  end.
*/

{syspara.i 1 4   1 cExcEkstent}
cExcEkstent = IF cExcEkstent = "" THEN "sdv" ELSE cExcEkstent.   
{syspara.i 1 1 100 cKunde}
{syspara.i 1 1 101 cSkoTex}

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
    RUN SD-QUERY-OPEN.

    IF RETURN-VALUE = "FEIL" THEN
        LEAVE MAIN-BLOCK.

    RUN InitResize. 
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
      DO:
        REPOSITION {&BROWSE-NAME} TO ROW 1.
      END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-Win 
PROCEDURE DelTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tmpChild:
        IF valid-handle(tmpChild.wChild) THEN DO:
            RUN DelTmpChild IN tmpChild.wChild NO-ERROR.
            DELETE PROCEDURE tmpChild.wChild.
        END.
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
  DISPLAY FILL-IN-SOK-DECI FILL-IN-SOK-INTE FILL-IN-SOK-DATE FILL-IN-SOK-CHAR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-50 RECT-51 RECT-52 FILL-IN-SOK-DECI FILL-IN-SOK-INTE 
         FILL-IN-SOK-DATE FILL-IN-SOK-CHAR BUTTON-Ny BUTTON-Endre BUTTON-Slett 
         B-Excel B-Rapp B-Oppdater Btn_Help Btn_OK BUTTON-Sok BROWSE-LevBas 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExHtmRapp C-Win 
PROCEDURE ExHtmRapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wDokTyp AS CHAR NO-UNDO.
  DEFINE VAR wtmpFileName AS CHAR NO-UNDO.
  DEFINE VAR wHead1Set    AS CHAR NO-UNDO.
  DEFINE VAR wColHead     AS CHAR NO-UNDO.
  DEFINE VAR wFields      AS CHAR NO-UNDO.
  DEFINE VAR wColHeadForm AS CHAR NO-UNDO.
  DEFINE VAR wTabell      AS CHAR NO-UNDO.
  DEFINE VAR wSep         AS CHAR INIT ";" NO-UNDO.
  DEFINE VAR wQY          AS CHAR NO-UNDO.
  DEFINE VAR hQuery       AS HANDLE NO-UNDO.
  DEFINE VAR hLevBas      AS HANDLE NO-UNDO.
  DEFINE VAR wLblHdl      AS HANDLE NO-UNDO.
  DEFINE VAR wTotWidthC   AS DECI NO-UNDO.
  DEFINE VAR wColWidthC   AS CHAR NO-UNDO.
  DEFINE VAR wCount       AS INTE NO-UNDO.
  RUN SkapaTmpLevBas.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  {sww.i}
 DO WITH FRAME {&FRAME-NAME}:
  IF valid-handle(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle (wDokTyp, IF wDokTyp = "HTM" THEN "HTM" ELSE wExcEkstent, OUTPUT wtmpFileName). 
  IF RETURN-VALUE = "TMP" THEN DO:
     ASSIGN hQuery = QUERY QUERY-tmp:HANDLE.
     OPEN QUERY QUERY-tmp FOR EACH tmpLevBas NO-LOCK BY tmpLevBas.Ordning.
  END.
  ELSE DO:
      ASSIGN hQuery = QUERY QUERY-Alle:HANDLE
             hLevBas = {&BROWSE-NAME}:QUERY.
      hQuery:QUERY-PREPARE(REPLACE(hLevBas:PREPARE-STRING,"LevBas","bLevBas")).
      hQuery:QUERY-OPEN().
  END.
  DO:
    ASSIGN wLblHdl = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(wLblHdl).
      IF wLblHdl:label = "" THEN LEAVE.
      ASSIGN wColHead = wColHead + (IF wColHead <> "" THEN wSep ELSE "") + 
                    TRIM(REPLACE(wLblHdl:label,"*",""))
             wFields  = wFields + (IF wFields <> "" THEN wSep ELSE "") + 
                    TRIM(wLblHdl:NAME)
             wTotWidthC = wTotWidthC + wLblHdl:WIDTH-CHARS
             wColHeadForm = wColHeadForm + (if wColHeadForm = "" then "" else wSep) +
                          (IF LENGTH(wLblHdl:FORMAT) = 1 THEN "C" ELSE (IF
                          wLblHdl:DATA-TYPE BEGINS "INTE" OR wLblHdl:DATA-TYPE BEGINS "DECI" THEN "R" ELSE "L"))
             wColWidthC = wColWidthC + (if wColWidthC = "" then "" else wSep) + string(INT(wLblHdl:WIDTH-CHARS))
             wLblHdl = wLblHdl:NEXT-COLUMN.
        
    END.
    DO wCount = 1 TO NUM-ENTRIES(wColHeadForm,wSep):
      ENTRY(wCount,wColHeadForm,wSep) = ENTRY(wCount,wColHeadForm,wSep) + "," + String(INT(DECI(ENTRY(wCount,wColWidthC,wSep)) / wTotWidthC * 100)) + "%".
    END.
  END.
  ASSIGN wTabell = "Leverandører"
         wHead1Set    = "100%,,1,0,2," + STRING(NUM-ENTRIES(wFields,wSep))
         wQY          = hQuery:PREPARE-STRING.
  RUN w-bhtml.p(wDokTyp,wSep,wHead1Set,wColHead,wFields,wColHeadForm,wTabell,wQY,hQuery,wtmpFileName).
  IF valid-handle(wLibHandle) THEN DO:
    IF wDokTyp = "HTM" THEN
        RUN OpenWeb IN wLibHandle (wtmpFileName).
    ELSE RUN OpenExcelDocument IN wLibHandle (wtmpFileName," ").
  END.
  {swn.i}
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitResize C-Win 
PROCEDURE InitResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Cho */ 
    DYNAMIC-FUNCTION("setnoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME DEFAULT-FRAME:HANDLE,"RECT-50,RECT-51,RECT-52").          
    DYNAMIC-FUNCTION("setnoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME DEFAULT-FRAME:HANDLE,"RECT-50,RECT-51,RECT-52").          

    DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,728,380,0,0).
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
    wAktivQString = REPLACE(wAktivQString, "NO-LOCK", wWhere).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWhereSats C-Win 
PROCEDURE SettWhereSats :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wWhere AS CHAR NO-UNDO.
/*
  if frame DEFAULT-FRAME:visible = false then
    return.

  FRAME-SCOOP:
  do with frame DEFAULT-FRAME:
  
  /* Bygger where sats */
  assign
/*
    wWhere = (if input CB-Type <> wAlle
               then (if wWhere = "" then "" else "and ") + "Kund.TypeId = " + entry(1,input CB-Type,":")
               else "") 

    wWhere = wWhere + " " +
             (if input CB-Gruppe <> wAlle
               then (if wWhere = "" then "" else "and ") + "VarGr.Hg = " + entry(1,input CB-Gruppe,":")
               else "")
    wWhere = wWhere + " " +
             (if input FI-Navn <> "*"
               then (if wWhere = "" then "" else "and ") + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "VarGHr.VgBeskr matches '"
                       else "VarGHr.VgBeskr begins '") + 
                     input FI-Navn + 
                     (if substring(input FI-Navn,1,1) = "*"
                       then "*"
                       else "") + "'"
               else "")
*/
    wWhere = if wWhere <> "" then "NO-LOCK where " + wWhere else "".
    
  end. /* FRAME SCOOP */       
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpLevBas C-Win 
PROCEDURE SkapaTmpLevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSvar AS LOG NO-UNDO.
  DEF VAR wLoop AS INT NO-UNDO.
    EMPTY TEMP-TABLE tmpLevBas. /* [ NO-ERROR ] */
    wSvar = FALSE.
    IF BROWSE BROWSE-LevBas:NUM-SELECTED-ROWS > 1 THEN DO:  
      MESSAGE "Skal rapport startes for de merkede leverandørene?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft utskrift"
              UPDATE wSvar.
      IF wSvar = TRUE THEN DO:
          DO wLoop = 1 TO BROWSE BROWSE-LevBas:NUM-SELECTED-ROWS:
              IF BROWSE BROWSE-LevBas:FETCH-SELECTED-ROW(wLoop) THEN DO:
                  CREATE tmpLevBas.
                  ASSIGN tmpLevBas.LevNr   = LevBas.LevNr
                         tmpLevBas.Ordning = wLoop.
                  RELEASE tmpLevBas.
              END.
          END.
          RETURN "TMP".
      END.
      ELSE
        RETURN "AVBRYT".  
    END.
    ELSE DO:
        MESSAGE "Skal utskrift startes for ALLE leverandører?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft utskrift"
              UPDATE wSvar.
        RETURN IF wSvar = TRUE THEN "ALLE" ELSE "AVBRYT".  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Win 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR piAntPoster AS INT  NO-UNDO.
  DEF VAR cIntervall  AS CHAR NO-UNDO.

  {sww.i}
  
  /* Finner temporært filnavn. */
  IF valid-handle(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle ("LevBas", cExcEkstent, OUTPUT cFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(cFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "LEVERANDØRLISTE"
    ""
    ""
    ""
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    "LevNr"
    "Navn"
    "Telefon"
    "Adresse"
    "Postadresse"
    "PostNr"
    "PostSted"
    "Land"
    "Kontakt"
    "Telefaks"
    "Komentar"
    "Valuta"
    SKIP.                                 
                                  
  /* Eksporterer data */
  EKSPORT:
  FOR EACH LevBas NO-LOCK:

    ASSIGN
        piAntPoster = piAntPoster + 1
        .

    FIND Post NO-LOCK WHERE
        Post.PostNr = LevBas.LevPoNr NO-ERROR.

    EXPORT STREAM sExportFile DELIMITER ";"
      /* A */ LevBas.LevNr     
      /* B */ LevBas.LevNamn
      /* C */ LevBas.LevTel
      /* D */ LevBas.LevAdr
      /* E */ LevBas.LevPAdr
      /* F */ LevBas.LevPoNr
      /* G */ IF AVAILABLE Post THEN Post.Beskrivelse ELSE " "
      /* H */ LevBas.LevLand
      /* I */ LevBas.LevKon
      /* J */ LevBas.TeleFax
      /* K */ LevBas.Kommentar
      /* L */ LevBas.ValKod
      .
                                
  END. /* EKSPORT */
                                  
  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:L2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:L2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("B:B"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/

  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("A:A"):NumberFormat = "# ##0".
  /*
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  */

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:B1"):Merge().
  chWorkSheets:Range("A1:B1"):HorizontalAlignment = 3.
  /*      
  chWorkSheets:Range("C2:C2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F2:F2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("H2:H2"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("J2:J2"):HorizontalAlignment = 4.   
  */

  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:O"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:X2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier - <Blank>".
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(piAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = cKunde.
  chWorkSheets:PageSetup:RightFooter    = cSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:L".
  chWorkSheets:PageSetup:Orientation    = 2.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("C3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = TRUE.
  
  /* Legger inn sumlinjer. */                                        
  /* Excel macro som gjør jobben.
  Range("A4").Select
    Selection.Subtotal GroupBy:=1, Function:=xlSum, TotalList:=Array(5, 7, 9), _
        Replace:=True, PageBreaks:=True, SummaryBelowData:=True  
  */
  STATUS DEFAULT "Setter summeringer...".
  /*chWorkSheets:Range("E4:M50"):Subtotal(1 ,1 ,"5 , 7, 9" ,TRUE ,TRUE ,TRUE ).*/   
  
  chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.

  STATUS DEFAULT "".

  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Postadr C-Win 
FUNCTION Postadr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Post WHERE Post.PostNr = LevBas.LevPadr NO-LOCK NO-ERROR.
  RETURN IF AVAIL Post THEN Post.Beskrivelse ELSE "".   /* Function return value. */

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
          wQStr = REPLACE(wQStr,"XSORT",wXSort).
  RETURN wQStr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

