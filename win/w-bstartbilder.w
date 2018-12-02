&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE tBild NO-UNDO LIKE Bild.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  Forfatter:
  Beskrivelse:  Template før søkprogram
  Parametere:
  Endringer:

  STEP 1: Definiera din QUERY
          Kryssa av Indexed-Reposition
          Kryssa av Sort-ByPhrase under Option
  STEP 2: Definiera de fælt du ønskar i din browser
          Kryssa av enabled på de fælt du ønskar sortering

   Programmet ær førberett før 4 søkkolonner.
   Gør de andringar som behøvs i alla scoped-define och følj
   beskrivning nedan.

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wListerRecid as recid NO-UNDO.
  def    var wNumRecords  as int   no-undo.
  def    var wPArent      as handle no-undo.
&ELSE
  DEFINE input PARAMETER wListerRecid as recid NO-UNDO.
  def    input parameter wNumRecords  as int   no-undo.
  def    input parameter wParent      as handle no-undo.
&ENDIF

DEFINE VAR wCellNr LIKE tBild.CellNr NO-UNDO.

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell   tBild
&scoped-define OptWhere 

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  CellNr
&scoped-define sok-field1-def  INT /* INT STRING DATE */
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} /* DESCENDING */
&scoped-define index-col1  CellNr
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Kolonne 2 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field2  VgLopNr
&scoped-define sok-field2-def  STRING
&scoped-define sortby-col2 {&br-tabell}.{&sok-field2}
&scoped-define sortby-phrase2 {&br-tabell}.{&sok-field2} /* DESCENDING */
&scoped-define index-col2  VgLopNr
&scoped-define sok-phrase2 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field2} >= ~
               INPUT FILL-IN-SOK-{&sok-field2-def} USE-INDEX {&index-col2} NO-LOCK NO-ERROR.

/* Kolonne 3 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field3  LevArtNr
&scoped-define sok-field3-def  STRING
&scoped-define sortby-col3 {&br-tabell}.{&sok-field3}
&scoped-define sortby-phrase3 {&br-tabell}.{&sok-field3} /* DESCENDING */
&scoped-define index-col3  LevArtNr
&scoped-define sok-phrase3 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field3} >= ~
               INPUT FILL-IN-SOK-{&sok-field3-def} USE-INDEX {&index-col3} NO-LOCK NO-ERROR.

&scoped-define ip-variabel wCellNr
&scoped-define return-ip   wCellNr = if available {&br-tabell}  ~
                                       then {&br-tabell}.{&sok-field1} ~
                                       else  {&sok-field1-def}("").
&scoped-define assign-retur-verdi retur-verdi = if available {&br-tabell} ~
                                                  then string(recid({&br-tabell})) ~
                                                  else "".
&scoped-define SletteMelding " "
&scoped-define KanSlettes 
&scoped-define ant-sok-field   3
&scoped-define init-datatype   INT /* CHAR, INT, DATE */
&scoped-define VedlikeholdsRutine d-v{&br-tabell}.w

/* Om du ønskar en startup-record annars fjern. */
&scoped-define init-phrase  FIND b{&br-tabell} WHERE ~
                           {&OptWhere} ~
                           b{&br-tabell}.{&sok-field1} = ~
                           {&ip-variabel} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Denna måste finnas før riktig sortering vid uppstart */
&scoped-define init-sortby-phrase {&br-tabell}.{&sok-field1} /* DESCENDING */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.

/* Lokale variabler ---                                                 */
def var wRecid      as recid                 no-undo.
def var wOk         as log                   no-undo.
def var retur-verdi as char initial "AVBRYT" no-undo.
def var wFarge      as char format "x(11)"   no-undo.
def var wTyper      as char                  no-undo.
def var wListeType  as char                  no-undo.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VAR wChild AS HANDLE NO-UNDO.
DEFINE VAR wMenu  AS CHAR NO-UNDO.
DEFINE VAR wScreenSize AS INTE INIT 1 NO-UNDO.
DEFINE VAR wColor AS INTE EXTENT 3 INIT [5197823,65280,16711680] NO-UNDO.

DEF VAR cVareTekst AS CHAR NO-UNDO.
DEF VAR lArtikkelNr AS DEC FORMAT "ZZZZZZZZZZZZZZ" NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-tBild

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tBild

/* Definitions for BROWSE BROWSE-tBild                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-tBild tBild.CellNr tBild.VgLopNr ~
tBild.ArtikkelNr cVareTekst tBild.LevNr tBild.LevArtNr tBild.BestNr ~
tBild.OrdreNr tBild.LevTid tBild.Valutapris tBild.InnkjPris tBild.VareKost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-tBild tBild.CellNr ~
tBild.VgLopNr tBild.ArtikkelNr tBild.LevArtNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-tBild tBild
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-tBild tBild
&Scoped-define QUERY-STRING-BROWSE-tBild FOR EACH tBild ~
      WHERE tBild.CellNr <> ? NO-LOCK ~
    BY tBild.CellNr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-tBild OPEN QUERY BROWSE-tBild FOR EACH tBild ~
      WHERE tBild.CellNr <> ? NO-LOCK ~
    BY tBild.CellNr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-tBild tBild
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-tBild tBild


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-tBild}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-SOK-STRING FILL-IN-SOK-DATE ~
FILL-IN-SOK-INT BUTTON-Sok BUTTON-5 BUTTON-2 BROWSE-tBild B-Exit Btn_Help-2 ~
RECT-10 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-STRING FILL-IN-SOK-DATE ~
FILL-IN-SOK-INT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON Btn_Help-2 DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     LABEL "&Angre ta bort linjer" 
     SIZE 22 BY 1.1.

DEFINE BUTTON BUTTON-3  NO-FOCUS
     LABEL "&Ny linje..." 
     SIZE 16 BY 1.1.

DEFINE BUTTON BUTTON-5 
     LABEL "&Ta bort linjer" 
     SIZE 20 BY 1.1.

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INT AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-STRING AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY .1.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-tBild FOR 
      tBild SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-tBild
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-tBild C-Win _STRUCTURED
  QUERY BROWSE-tBild NO-LOCK DISPLAY
      tBild.CellNr COLUMN-LABEL "CellNr *" FORMAT "zzz9":U
      tBild.VgLopNr COLUMN-LABEL "Vg/LøpeNr *" FORMAT "X(10)":U
      tBild.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      cVareTekst COLUMN-LABEL "Varetekst" WIDTH 30
      tBild.LevNr FORMAT "zzzzz9":U
      tBild.LevArtNr COLUMN-LABEL "Lev.ArtikkelNr *" FORMAT "X(30)":U
            WIDTH 36
      tBild.BestNr FORMAT "zzzzz9":U
      tBild.OrdreNr FORMAT "zzzzz9":U
      tBild.LevTid FORMAT "X(6)":U
      tBild.Valutapris FORMAT "-zz,zzz,zz9":U
      tBild.InnkjPris COLUMN-LABEL "InnkjPris" FORMAT "-zz,zzz,zz9":U
            WIDTH 10
      tBild.VareKost FORMAT "-zz,zzz,zz9":U WIDTH 11.8
  ENABLE
      tBild.CellNr
      tBild.VgLopNr
      tBild.ArtikkelNr
      tBild.LevArtNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 125 BY 19.52 ROW-HEIGHT-CHARS .52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-3 AT ROW 21.48 COL 54 NO-TAB-STOP 
     FILL-IN-SOK-STRING AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-INT AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 1.48 COL 21.6
     BUTTON-5 AT ROW 1.48 COL 53
     BUTTON-2 AT ROW 1.48 COL 73
     BROWSE-tBild AT ROW 2.91 COL 2
     B-Exit AT ROW 1.48 COL 122
     Btn_Help-2 AT ROW 1.48 COL 117
     RECT-10 AT ROW 2.67 COL 1
     RECT-9 AT ROW 1.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126.2 BY 21.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tBild T "SHARED" NO-UNDO Temp-DB Bild
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Utvalg kollesjonsliste"
         HEIGHT             = 21.62
         WIDTH              = 126.2
         MAX-HEIGHT         = 25.48
         MAX-WIDTH          = 193.8
         VIRTUAL-HEIGHT     = 25.48
         VIRTUAL-WIDTH      = 193.8
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
/* BROWSE-TAB BROWSE-tBild BUTTON-2 DEFAULT-FRAME */
ASSIGN 
       tBild.CellNr:COLUMN-READ-ONLY IN BROWSE BROWSE-tBild = TRUE
       tBild.VgLopNr:COLUMN-READ-ONLY IN BROWSE BROWSE-tBild = TRUE
       tBild.ArtikkelNr:COLUMN-READ-ONLY IN BROWSE BROWSE-tBild = TRUE
       tBild.LevArtNr:COLUMN-READ-ONLY IN BROWSE BROWSE-tBild = TRUE.

/* SETTINGS FOR BUTTON BUTTON-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-STRING IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-tBild
/* Query rebuild information for BROWSE BROWSE-tBild
     _TblList          = "Temp-Tables.tBild"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tBild.CellNr|yes"
     _Where[1]         = "tBild.CellNr <> ?"
     _FldNameList[1]   > Temp-Tables.tBild.CellNr
"CellNr" "CellNr *" "zzz9" "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[2]   > Temp-Tables.tBild.VgLopNr
"VgLopNr" "Vg/LøpeNr *" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[3]   > Temp-Tables.tBild.ArtikkelNr
"ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"cVareTekst" "Varetekst" ? ? ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.tBild.LevNr
     _FldNameList[6]   > Temp-Tables.tBild.LevArtNr
"LevArtNr" "Lev.ArtikkelNr *" ? "character" ? ? ? ? ? ? yes ? no no "36" yes no yes "U" "" ""
     _FldNameList[7]   = Temp-Tables.tBild.BestNr
     _FldNameList[8]   = Temp-Tables.tBild.OrdreNr
     _FldNameList[9]   = Temp-Tables.tBild.LevTid
     _FldNameList[10]   = Temp-Tables.tBild.Valutapris
     _FldNameList[11]   > Temp-Tables.tBild.InnkjPris
"InnkjPris" "InnkjPris" ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[12]   > Temp-Tables.tBild.VareKost
"VareKost" ? ? "decimal" ? ? ? ? ? ? no ? no no "11.8" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-tBild */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Utvalg kollesjonsliste */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Utvalg kollesjonsliste */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define BROWSE-NAME BROWSE-tBild
&Scoped-define SELF-NAME BROWSE-tBild
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tBild C-Win
ON ANY-PRINTABLE OF BROWSE-tBild IN FRAME DEFAULT-FRAME
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tBild C-Win
ON CURSOR-LEFT OF BROWSE-tBild IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tBild C-Win
ON CURSOR-RIGHT OF BROWSE-tBild IN FRAME DEFAULT-FRAME
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tBild C-Win
ON DEFAULT-ACTION OF BROWSE-tBild IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO B-Exit.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tBild C-Win
ON ROW-DISPLAY OF BROWSE-tBild IN FRAME DEFAULT-FRAME
DO:
  cVareTekst = "".
  IF AVAILABLE tBild THEN
  DO: 
       FIND ArtBas NO-LOCK WHERE
           ArtBas.ArtikkelNr = tBild.ArtikkelNr NO-ERROR.
       IF AVAILABLE ArtBas THEN
           ASSIGN
           cVareTekst = ArtBas.Beskr.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-tBild C-Win
ON START-SEARCH OF BROWSE-tBild IN FRAME DEFAULT-FRAME
DO:
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help-2 C-Win
ON CHOOSE OF Btn_Help-2 IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Angre ta bort linjer */
DO:
  def var wCellnr as INTE no-undo.
  IF NOT CAN-FIND(FIRST tBild WHERE tBild.Cellnr = ?) THEN 
      RETURN NO-APPLY.

  FIND LAST tBild WHERE tBild.Cellnr <> ?.
  wCellnr  = tBild.Cellnr + 1.
  FIND FIRST tBild WHERE tBild.Cellnr = ?.
  tBild.Cellnr = wCellnr.
  RUN BrowseRefresh.
  IF VALID-HANDLE(wChild) THEN
      RUN LesInnBild IN wChild ("").
  
  wNumrecords = 0.  
  for each tBild where 
    tBild.CellNr <> ?
    by tBild.CellNr:
    
    assign
      wNumRecords  = wNumRecords + 1.
  end.
  RUN WinTittel.
      
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Ny linje... */
DO:
   DEFINE VAR wSist AS INTE NO-UNDO.
   FIND LAST tBild WHERE tBild.Cellnr <> ?.
   ASSIGN wSist = tBild.Cellnr.
   CREATE tBild.
   Assign Cellnr  = wSist + 1
          Bild    = "bilder\" + STRING(wSist MOD 9 + 1) + ".bmp"
          Bildtxt = Bild.

  IF VALID-HANDLE(wChild) THEN
      RUN LesInnBild IN wChild ("Ny").
  RUN BrowseRefresh.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Ta bort linjer */
DO:
  if BROWSE-tBild:num-selected-rows in frame DEFAULT-FRAME = 0 then
    return.
    
  run TaBortMerkedeBilder.
  IF VALID-HANDLE(wChild) THEN
    RUN StartRestart.
  RUN WinTittel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok C-Win
ON CHOOSE OF BUTTON-Sok IN FRAME DEFAULT-FRAME /* Søk */
DO:
   DEFINE VAR wBlank AS LOG NO-UNDO.
   CASE wAktivCol:
       WHEN 1 THEN DO:
           IF FILL-IN-SOK-{&sok-field1-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase1}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field1-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &IF DEFINED(sok-phrase2) &THEN 
       WHEN 2 THEN DO:
           IF FILL-IN-SOK-{&sok-field2-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase2}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field2-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &ENDIF
       &IF DEFINED(sok-phrase3) &THEN 
       WHEN 3 THEN DO:
           IF FILL-IN-SOK-{&sok-field3-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase3}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field3-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &ENDIF
       &IF DEFINED(sok-phrase4) &THEN 
       WHEN 4 THEN DO:
           IF FILL-IN-SOK-{&sok-field4-def}:SCREEN-VALUE = "" THEN
               wBlank = TRUE.
           ELSE DO:
               {&sok-phrase4}
               IF NOT AVAIL b{&br-tabell} THEN DO:
                   APPLY "ENTRY" TO FILL-IN-SOK-{&sok-field4-def}.
                   RETURN NO-APPLY.         
               END.
           END.
       END.
       &ENDIF
   END CASE.
   IF wBlank THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
   ELSE
       RUN SD-Reposition.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-DATE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE C-Win
ON LEAVE OF FILL-IN-SOK-DATE IN FRAME DEFAULT-FRAME
DO:
    DEFINE VAR wDate AS DATE.
    wDate = DATE(FILL-IN-SOK-DATE:SCREEN-VALUE) NO-ERROR.
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


&Scoped-define SELF-NAME FILL-IN-SOK-INT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-INT C-Win
ON RETURN OF FILL-IN-SOK-INT IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-STRING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-STRING C-Win
ON RETURN OF FILL-IN-SOK-STRING IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
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
/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Start bildegrid"
  &PreIClose      = "run TaBortPoster.
                     run SettCelleNr in wParent.
                    "
}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

find Lister no-lock where
  recid(Lister) = wListerRecid no-error.
if not available Lister then
  do:
    message "Ukjent liste!" view-as alert-box error title "Feil".
    return "AVBRYT".
  end.
  
/* Bygger en liste over lsitetyper. */  
wTyper = "".  
for each SysPara no-lock where
  SysPara.SysHId = 7 and
  SysPara.SysGr  = 1:
  
  wTyper = wTyper + 
           (if wTyper = ""
              then ""
              else ",") + 
           SysPara.Parameter1.   
end.    

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  wOrgBgCol = {&sortby-col1}:Label-bgcolor in browse {&BROWSE-NAME}.
  RUN LabelColor.
  RUN Move-Fill-To-Top.
  
  /* Legger listens navn inn i tittel. */
  run WinTittel.
    
  RUN enable_UI.
  {lng.i} 

  IF {&ant-sok-field} > 1 THEN
      RUN Init-Read-Only.
      
  &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
  
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END.
  &ENDIF

  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.

  view frame DEFAULT-FRAME.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseRefresh C-Win 
PROCEDURE BrowseRefresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {&OPEN-QUERY-BROWSE-tBild}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytSortering C-Win 
PROCEDURE BytSortering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wNyCol AS INT NO-UNDO.
  DEFINE VAR             wDataType AS CHAR NO-UNDO.
  IF wNyCol = wAktivCol THEN
      RETURN NO-APPLY.
  ASSIGN wAktivCol = wNyCol.
  CASE wNyCol:
    WHEN 1 THEN DO:
        &scope SORTBY-PHRASE BY {&sortby-phrase1}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col1}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
    END.
    &IF DEFINED(sortby-phrase2) &THEN
    WHEN 2 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase2}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col2}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase3) &THEN
    WHEN 3 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase3}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col3}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
    &IF DEFINED(sortby-phrase4) &THEN
    WHEN 4 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase4}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col4}:DATA-TYPE IN BROWSE {&BROWSE-NAME}.
     END.
     &ENDIF
  END CASE.
  CASE wDataType:
      WHEN "INTEGER" THEN
          wAktivDataType = "INT".
      WHEN "CHARACTER" THEN
          wAktivDataType = "CHAR".
      WHEN "DATE" THEN
          wAktivDataType = "DATE".
  END CASE.
  &scope SORTBY-PHRASE BY {&sortby-phrase1}
  RUN Move-Fill-To-Top.
  RUN LabelColor.
  RETURN NO-APPLY.
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
  DISPLAY FILL-IN-SOK-STRING FILL-IN-SOK-DATE FILL-IN-SOK-INT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FILL-IN-SOK-STRING FILL-IN-SOK-DATE FILL-IN-SOK-INT BUTTON-Sok 
         BUTTON-5 BUTTON-2 BROWSE-tBild B-Exit Btn_Help-2 RECT-10 RECT-9 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Read-Only C-Win 
PROCEDURE Init-Read-Only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
    {&sortby-col1}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &IF DEFINED(sortby-col2) &THEN
    {&sortby-col2}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    &IF DEFINED(sortby-col3) &THEN
    {&sortby-col3}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    &IF DEFINED(sortby-col4) &THEN
    {&sortby-col4}:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    &ENDIF
    .
    RETURN NO-APPLY.
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
  ASSIGN
   {&sortby-col1}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 1 THEN wSortBgCol ELSE wOrgBgCol
   &IF DEFINED(sortby-col2) &THEN
   {&sortby-col2}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 2 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col3) &THEN
   {&sortby-col3}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 3 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col4) &THEN
   {&sortby-col4}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 4 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   .
   RETURN NO-APPLY.
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
        CASE wAktivDataType:
             WHEN "INT" THEN
                 IF FILL-IN-SOK-INT:MOVE-TO-TOP() THEN.
             WHEN "CHAR" THEN
                 IF FILL-IN-SOK-STRING:MOVE-TO-TOP() THEN.
             WHEN "DATE" THEN
                 IF FILL-IN-SOK-DATE:MOVE-TO-TOP() THEN.
        END CASE.
    END.
    RETURN NO-APPLY.
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
    CASE wAktivDataType:
        WHEN "INT" OR WHEN "DATE" THEN DO:
            IF key-function(lastkey) < "0" OR key-function(lastkey) > "9" THEN
                RETURN NO-APPLY.
        END.
    END CASE.
    CASE wAktivDataType:
        WHEN "INT" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-INT.
            APPLY LASTKEY. 
            FILL-IN-SOK-INT:CURSOR-OFFSET = 2.
        END.
        WHEN "CHAR" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-STRING.
            APPLY LASTKEY.
        END.
        WHEN "DATE" THEN DO:
            APPLY "ENTRY" TO FILL-IN-SOK-DATE.
            APPLY LASTKEY.
        END.
    END CASE.
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
    CASE wLeft-Right:
        WHEN "LEFT" THEN
            IF wAktivCol = 1 THEN
                RUN SortNyCol({&ant-sok-field}).
            ELSE
                RUN SortNyCol(wAktivCol - 1).
        WHEN "RIGHT" THEN
            IF wAktivCol = {&ant-sok-field} THEN
                RUN SortNyCol(1).
            ELSE
                RUN SortNyCol(wAktivCol + 1).
    END CASE.
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
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
        REPOSITION {&BROWSE-NAME} TO ROWID rowid(b{&br-tabell}) NO-ERROR.
        CASE wAktivDataType:
            WHEN "INT" THEN
                ASSIGN FILL-IN-SOK-INT:SCREEN-VALUE = "".
            WHEN "CHAR" THEN
                ASSIGN FILL-IN-SOK-STRING:SCREEN-VALUE = "".
            WHEN "DATE" THEN
                ASSIGN FILL-IN-SOK-DATE:SCREEN-VALUE = "".
        END CASE.
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Start-Search C-Win 
PROCEDURE SD-Start-Search :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR h-Curr-Col as widget.
    h-Curr-Col = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
    APPLY "end-search" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.  
    CASE h-Curr-Col:NAME:
        WHEN "{&sok-field1}" THEN
            IF wAktivCol <> 1 THEN DO:
                RUN SortNyCol(1).
                RETURN NO-APPLY.
            END.
        &IF DEFINED(sok-field2) &THEN 
        WHEN "{&sok-field2}" THEN
            IF wAktivCol <> 2 THEN DO:
                RUN SortNyCol(2).
                RETURN NO-APPLY.
            END.
        &ENDIF
        &IF DEFINED(sok-field3) &THEN 
        WHEN "{&sok-field3}" THEN
            IF wAktivCol <> 3 THEN DO:
                RUN SortNyCol(3).
                RETURN NO-APPLY.
            END.
        &ENDIF
        &IF DEFINED(sok-field4) &THEN 
        WHEN "{&sok-field4}" THEN
            IF wAktivCol <> 4 THEN DO:
                RUN SortNyCol(4).
                RETURN NO-APPLY.
            END.
        &ENDIF
    END CASE.
   
    RUN SD-Reposition.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkoReposition C-Win 
PROCEDURE SkoReposition :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wRowId AS ROWID NO-UNDO.
    RUN BrowseRefresh.
    REPOSITION {&BROWSE-NAME} TO ROWID wRowId NO-ERROR.

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
    DEFINE INPUT PARAMETER wNyCol     AS INT NO-UNDO.
    DEFINE VAR             wRowID     AS ROWID NO-UNDO.
    DEFINE VAR             wBrowseRow AS INT   NO-UNDO.
    wRowId = ROWID({&br-tabell}).
    wBrowseRow = {&BROWSE-NAME}:FOCUSED-ROW IN FRAME {&FRAME-NAME}.
    FIND b{&br-tabell} WHERE ROWID(b{&br-tabell}) = wRowId NO-LOCK.
    RUN BytSortering (wNyCol).
    RUN SD-Reposition.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartRestart C-Win 
PROCEDURE StartRestart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wMenuAction as char no-undo.
  
  wMenuAction = "INIT".
  case lookup(Lister.ListeType,wTyper):
     when 1 then run kollgridmeny.p (wMenuAction, "", output wMenu).
     when 2 then run artikkelgridmeny.p (wMenuAction, "", output wMenu).
     otherwise
       do:
         message "Ukjent listetype!" view-as alert-box.
         return "AVBRYT".
       end.
  end case.

  IF VALID-HANDLE(wChild) THEN
      APPLY "CLOSE" TO wChild.

  IF NOT VALID-HANDLE(wChild) THEN
      RUN w-bildegrid.w PERSISTENT 
          SET wChild (THIS-PROCEDURE:HANDLE,wMenu,wScreenSize,wNumRecords)
          (wListerRecid).          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortMerkedeBilder C-Win 
PROCEDURE TaBortMerkedeBilder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk   as log no-undo.
  def var wLoop as int no-undo.

  SLETTE_LOOP:
  do wLoop = BROWSE-tBild:NUM-SELECTED-ROWS in frame DEFAULT-FRAME to 1 by -1:
    wOk = BROWSE-tBild:FETCH-SELECTED-ROW(wLoop) in frame DEFAULT-FRAME.

    assign 
      tBild.CellNr = ?.
  end. /* SLETTE_LOOP */  

  /* Renumrerer radene slik at cellenummeret blir sammenhengende. */
  assign
    wLoop       = 0
    wNumRecords = 0.  
  for each tBild where 
    tBild.CellNr <> ?
    by tBild.CellNr:
    
    assign
      tBild.CellNr = wLoop
      wLoop        = wLoop + 1
      wNumRecords  = wNumRecords + 1.
  end.

  wOk = BROWSE-tBild:DELETE-SELECTED-ROWS( ) in frame DEFAULT-FRAME.  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinTittel C-Win 
PROCEDURE WinTittel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign
    C-Win:title = "Utvalg kolleksjonsliste: " + Lister.Beskrivelse +  
                  " (Antall poster " + string(wNumRecords) + ")".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

