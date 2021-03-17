&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wFarg LIKE Farg.Farg INIT 31 NO-UNDO.
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER wFarg LIKE Farg.Farg NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell   Farg

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  Farg
&scoped-define sok-field1-def  INT
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} /* DESCENDING */
&scoped-define index-col1  FargIn
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Kolonne 2 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field2  KFarge
&scoped-define sok-field2-def  CHAR
&scoped-define sortby-col2 {&br-tabell}.{&sok-field2}
&scoped-define sortby-phrase2 {&br-tabell}.{&sok-field2} /* DESCENDING */
&scoped-define index-col2  FarBeskr
&scoped-define sok-phrase2 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field2} >= ~
               INPUT FILL-IN-SOK-{&sok-field2-def} USE-INDEX {&index-col2} NO-LOCK NO-ERROR.

/* Kolonne 3 som det kan sorteras på, fjern om du bara vill ha 2 */
&scoped-define sok-field3  FarBeskr
&scoped-define sok-field3-def  CHAR
&scoped-define sortby-col3 {&br-tabell}.{&sok-field3}
&scoped-define sortby-phrase3 {&br-tabell}.{&sok-field3} /* DESCENDING */
&scoped-define index-col3  FarBeskr
&scoped-define sok-phrase3 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field3} >= ~
               INPUT FILL-IN-SOK-{&sok-field3-def} USE-INDEX {&index-col3} NO-LOCK NO-ERROR.

/* Om du vill sortera på fler kolonner:
   Kopiera kolonnedefinition 1 ovan och ændra talet 1 till næsta ikke anvænda.
   Ændra sedan alla fælt.
   Ændra ant-sok-field nedan till det antall som du vill sortera på.
   Ændra init-datatype nedan till datatyp før den sorteringskolonne du startar på
   Ændra æven alla andra scoped-define før att passa ditt program
   Ændra wAktivCol nedan till INIT ønskad startkolonne.
*/

&scoped-define ip-variabel wFarg
&scoped-define return-ip   wFarg = Farg.Farg.
&scoped-define assign-retur-verdi retur-verdi = string(recid(Farg)).
&scoped-define ant-sok-field   3
&scoped-define init-datatype   INT /* CHAR, INT, DATE */

&scoped-define SletteMelding "Skall posten tas bort?"
&scoped-define KanSlettes ~
  if can-find(first ArtBas where ArtBas.farg = farg.farg) then ~
    do: ~
      message "Det finns artiklar med denna färgkoden" ~
      view-as alert-box title "Fel vid borttagning". ~
      return no-apply. ~
    end.

&scoped-define VedlikeholdsRutine d-vfarg.w


/* Om du ønskar en startup-record annars fjern. */
&scoped-define init-phrase  FIND b{&br-tabell} WHERE b{&br-tabell}.{&sok-field1} = ~
                           {&ip-variabel} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Denna måste finnas før riktig sortering vid uppstart */
&scoped-define init-sortby-phrase {&br-tabell}.{&sok-field1} /* DESCENDING */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.

/* Lokale variabler ---                                                 */

def var retur-verdi as char initial "AVBRYT" no-undo.

DEFINE VAR wAktivCol         AS INT INIT 1   NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT          NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15  NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEFINE VAR wRecid            AS RECID        NO-UNDO.
DEFINE VAR wOk               AS LOG          NO-UNDO.
DEFINE VAR iIkkeArtBas       AS INTE INIT 11 NO-UNDO. /* bgfärg farg.farg */
DEF VAR cFileName            AS CHAR  NO-UNDO. 
DEF VAR cExcEkstent          AS CHAR  NO-UNDO.
DEF VAR cKunde               AS CHAR  NO-UNDO.
DEF VAR cSkoTex              AS CHAR  NO-UNDO.
DEF VAR lSok                 AS LOGI  NO-UNDO.
DEF VAR iTilgang           AS INT    NO-UNDO.

{runlib.i}
DEF STREAM sExportFile.
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Farg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Farg

/* Definitions for BROWSE BROWSE-Farg                                   */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Farg Farg.Farg Farg.KFarge ~
Farg.FarBeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Farg Farg.Farg Farg.KFarge ~
Farg.FarBeskr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Farg Farg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Farg Farg
&Scoped-define QUERY-STRING-BROWSE-Farg FOR EACH Farg NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Farg OPEN QUERY BROWSE-Farg FOR EACH Farg NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Farg Farg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Farg Farg


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Farg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-SOK-INT FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DATE BUTTON-Sok BROWSE-Farg Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-INT FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DATE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-KonvTbl 
     LABEL "Konverteringstabell..." 
     SIZE 22 BY 1.1.

DEFINE BUTTON B-Utskrift 
     LABEL "Utskrift..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Bytkode 
     LABEL "&Bytt kode..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Endre 
     LABEL "&Endre..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Flyttkode 
     LABEL "&Flytt kode..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "&Ny..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Slett" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INT AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Farg FOR 
      Farg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Farg Dialog-Frame _STRUCTURED
  QUERY BROWSE-Farg NO-LOCK DISPLAY
      Farg.Farg COLUMN-LABEL "Farge *" FORMAT "zzzz9":U WIDTH 9.5
      Farg.KFarge COLUMN-LABEL "Kortnavn *" FORMAT "X(5)":U
      Farg.FarBeskr COLUMN-LABEL "Beskrivelse *" FORMAT "x(30)":U
            WIDTH 37
  ENABLE
      Farg.Farg
      Farg.KFarge
      Farg.FarBeskr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 60 BY 10.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-SOK-INT AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 1.48 COL 21.6
     B-KonvTbl AT ROW 1.48 COL 39.6
     BROWSE-Farg AT ROW 2.76 COL 2
     BUTTON-Ny AT ROW 2.91 COL 62.4
     BUTTON-Endre AT ROW 4.24 COL 62.4
     BUTTON-Bytkode AT ROW 5.52 COL 62.4
     BUTTON-Flyttkode AT ROW 6.86 COL 62.4
     BUTTON-Slett AT ROW 8.38 COL 62.4
     B-Utskrift AT ROW 9.95 COL 62.2
     Btn_OK AT ROW 13.57 COL 2
     Btn_Cancel AT ROW 13.57 COL 14.4
     Btn_Help AT ROW 13.57 COL 46.8
     SPACE(16.79) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Søkeliste fargekoder"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-Farg B-KonvTbl Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-KonvTbl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       B-KonvTbl:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON B-Utskrift IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       B-Utskrift:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Bytkode IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Bytkode:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Endre IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Endre:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Flyttkode IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Flyttkode:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Ny IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Ny:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Slett IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-Slett:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Farg
/* Query rebuild information for BROWSE BROWSE-Farg
     _TblList          = "SkoTex.Farg"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > SkoTex.Farg.Farg
"Farg" "Farge *" "zzzz9" "integer" ? ? ? ? ? ? yes ? no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > SkoTex.Farg.KFarge
"KFarge" "Kortnavn *" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > SkoTex.Farg.FarBeskr
"FarBeskr" "Beskrivelse *" ? "character" ? ? ? ? ? ? yes ? no no "37" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-Farg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Søkeliste fargekoder */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KonvTbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvTbl Dialog-Frame
ON CHOOSE OF B-KonvTbl IN FRAME Dialog-Frame /* Konverteringstabell... */
DO:
  if not available Farg then
    return.
  run d-bimpkonv.w (1504,
                    string(Farg.Farg),
                    "Kobling av farger"
                    ).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Utskrift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Utskrift Dialog-Frame
ON CHOOSE OF B-Utskrift IN FRAME Dialog-Frame /* Utskrift... */
DO:
  RUN Utskrift.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Farg
&Scoped-define SELF-NAME BROWSE-Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Farg Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-Farg IN FRAME Dialog-Frame
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Farg Dialog-Frame
ON CURSOR-LEFT OF BROWSE-Farg IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Farg Dialog-Frame
ON CURSOR-RIGHT OF BROWSE-Farg IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Farg Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-Farg IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO Btn_OK.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Farg Dialog-Frame
ON ROW-DISPLAY OF BROWSE-Farg IN FRAME Dialog-Frame
DO:
  ASSIGN Farg.Farg:BGCOLOR IN BROWSE {&BROWSE-NAME} = 
      IF CAN-FIND(FIRST ArtBas WHERE ArtBas.Farg = Farg.Farg) THEN ? ELSE iIkkeArtBas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Farg Dialog-Frame
ON START-SEARCH OF BROWSE-Farg IN FRAME Dialog-Frame
DO:
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Farg Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Farg IN FRAME Dialog-Frame
DO:
  IF NOT lSok THEN
  DO:
    ASSIGN BUTTON-Slett:SENSITIVE     = Farg.Farg:BGCOLOR IN BROWSE {&BROWSE-NAME} = iIkkeArtBas
           BUTTON-Flyttkode:SENSITIVE = NOT BUTTON-Slett:SENSITIVE.
    IF iTilgang = 1 THEN
    assign
      BUTTON-Ny:sensitive        = FALSE 
      BUTTON-Bytkode:sensitive   = FALSE
      BUTTON-Slett:sensitive     = FALSE
      BUTTON-Flyttkode:sensitive = FALSE
    .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  {&return-ip}
  
  &IF DEFINED(assign-retur-verdi) &THEN
      {&assign-retur-verdi}
  &ELSE
      retur-verdi = "OK".
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Bytkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Bytkode Dialog-Frame
ON CHOOSE OF BUTTON-Bytkode IN FRAME Dialog-Frame /* Bytt kode... */
DO:
  assign
    wRecid = recid({&br-tabell}).
  run {&VedlikeholdsRutine} (input-output wRecid,"Bytkode").
  if return-value = "AVBRYT" then
    return no-apply.
  RUN SortNyCol(1).
/*   {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame} */
  find b{&br-tabell} no-lock where
    recid(b{&br-tabell}) = wRecid no-error.
  RUN SD-Reposition.
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre Dialog-Frame
ON CHOOSE OF BUTTON-Endre IN FRAME Dialog-Frame /* Endre... */
DO:
  assign
    wRecid = recid({&br-tabell}).
  run {&VedlikeholdsRutine} (input-output wRecid,"Endre").
  if return-value = "AVBRYT" then
    return no-apply.
  wOk = BROWSE-{&br-tabell}:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Flyttkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Flyttkode Dialog-Frame
ON CHOOSE OF BUTTON-Flyttkode IN FRAME Dialog-Frame /* Flytt kode... */
DO:
  assign
    wRecid = recid({&br-tabell}).
  run {&VedlikeholdsRutine} (input-output wRecid,"Flyttkode").
  if return-value = "AVBRYT" then
    return no-apply.
  RUN SortNyCol(1).
/*   {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame} */
  find b{&br-tabell} no-lock where
    recid(b{&br-tabell}) = wRecid no-error.
  RUN SD-Reposition.
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny Dialog-Frame
ON CHOOSE OF BUTTON-Ny IN FRAME Dialog-Frame /* Ny... */
DO:
  assign 
    wRecid = ?.
  run {&VedlikeholdsRutine} (input-output wRecid,"Ny").
  if return-value = "AVBRYT" then
    return no-apply.
  RUN SortNyCol(1).
  /* {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}*/
  find b{&br-tabell} no-lock where
    recid(b{&br-tabell}) = wRecid no-error.
  RUN SD-Reposition.
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett Dialog-Frame
ON CHOOSE OF BUTTON-Slett IN FRAME Dialog-Frame /* Slett */
DO:
  message {&SletteMelding}
  view-as alert-box buttons yes-no-cancel title "Bekreftelse" 
  update wOk.
  if wOk = true then
    do transaction:
      {&KanSlettes}
      find b{&br-tabell} exclusive-lock where
        recid(b{&br-tabell}) = recid({&br-tabell}) no-error.
      if available b{&br-tabell} then
        do:
          delete b{&br-tabell}.
          wOk = BROWSE-{&br-tabell}:DELETE-CURRENT-ROW( ).
        end.
    end.
  else return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sok Dialog-Frame
ON CHOOSE OF BUTTON-Sok IN FRAME Dialog-Frame /* Søk */
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
   APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-CHAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-CHAR Dialog-Frame
ON RETURN OF FILL-IN-SOK-CHAR IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SOK-DATE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE Dialog-Frame
ON LEAVE OF FILL-IN-SOK-DATE IN FRAME Dialog-Frame
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-DATE Dialog-Frame
ON RETURN OF FILL-IN-SOK-DATE IN FRAME Dialog-Frame
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SOK-INT Dialog-Frame
ON RETURN OF FILL-IN-SOK-INT IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO BUTTON-Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 1 4   1 cExcEkstent}
cExcEkstent = if cExcEkstent = "" then "sdv" else cExcEkstent.   
{syspara.i 1 1 100 cKunde}
{syspara.i 1 1 101 cSkoTex}
{syspara.i 16 15 2 iTilgang INT}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    wOrgBgCol = {&sortby-col1}:Label-bgcolor in browse {&BROWSE-NAME}.
    RUN LabelColor.
    RUN Move-Fill-To-Top.
    IF NOT (NUM-ENTRIES(PROGRAM-NAME(2)," ") = 2 AND
             ENTRY(2,PROGRAM-NAME(2)," ") BEGINS "d-vFarg") THEN DO:
        ASSIGN B-KonvTbl:SENSITIVE        = TRUE
               B-Utskrift:SENSITIVE       = TRUE
               BUTTON-Bytkode:SENSITIVE   = TRUE
               BUTTON-Endre:SENSITIVE     = TRUE
               BUTTON-Flyttkode:SENSITIVE = TRUE
               BUTTON-Ny:SENSITIVE        = TRUE
               BUTTON-Slett:SENSITIVE     = TRUE
               B-KonvTbl:HIDDEN           = FALSE
               B-Utskrift:HIDDEN          = FALSE
               BUTTON-Bytkode:HIDDEN      = FALSE
               BUTTON-Endre:HIDDEN        = FALSE
               BUTTON-Flyttkode:HIDDEN    = FALSE
               BUTTON-Ny:HIDDEN           = FALSE
               BUTTON-Slett:HIDDEN        = FALSE.
    END.
    ELSE
        ASSIGN lSok = TRUE
               FRAME {&FRAME-NAME}:WIDTH-PIXELS = 
               FRAME {&FRAME-NAME}:WIDTH-PIXELS - B-Utskrift:WIDTH-PIXELS - 5
            .
    {lng.i} RUN enable_UI.
    IF {&ant-sok-field} > 1 THEN
        RUN Init-Read-Only.
    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END.
    &ENDIF
    
    IF iTilgang = 1 THEN
    assign
      BUTTON-Ny:sensitive        = FALSE 
      BUTTON-Bytkode:sensitive   = FALSE
      BUTTON-Slett:sensitive     = FALSE
      BUTTON-Flyttkode:sensitive = FALSE
    .


    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytSortering Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-SOK-INT FILL-IN-SOK-CHAR FILL-IN-SOK-DATE 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-SOK-INT FILL-IN-SOK-CHAR FILL-IN-SOK-DATE BUTTON-Sok 
         BROWSE-Farg Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Read-Only Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LabelColor Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Fill-To-Top Dialog-Frame 
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
                 IF FILL-IN-SOK-CHAR:MOVE-TO-TOP() THEN.
             WHEN "DATE" THEN
                 IF FILL-IN-SOK-DATE:MOVE-TO-TOP() THEN.
        END CASE.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-ANY-PRINTABLE Dialog-Frame 
PROCEDURE SD-ANY-PRINTABLE :
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
            APPLY "ENTRY" TO FILL-IN-SOK-CHAR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-CURSOR Dialog-Frame 
PROCEDURE SD-CURSOR :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-Reposition Dialog-Frame 
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
                ASSIGN FILL-IN-SOK-CHAR:SCREEN-VALUE = "".
            WHEN "DATE" THEN
                ASSIGN FILL-IN-SOK-DATE:SCREEN-VALUE = "".
        END CASE.
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    END.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SD-START-SEARCH Dialog-Frame 
PROCEDURE SD-START-SEARCH :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortNyCol Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift Dialog-Frame 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR piAntPoster AS INT NO-UNDO.
  {sww.i}
  
  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("Farg", cExcEkstent, output cFileName). 
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(cFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"   
    "FARGELISTE"
    ""
    ""
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    "Farge"
    "Beskrivelse"
    "I bruk"
    SKIP.                                 
                                  
  /* Eksporterer data */
  EKSPORT:
  FOR EACH Farg no-lock:
    ASSIGN
        piAntPoster = piAntPoster + 1
        .

    EXPORT STREAM sExportFile DELIMITER ";"
      /* A */ Farg.Farg     
      /* B */ Farg.FarBeskr        
      /* C */ STRING(CAN-FIND(FIRST ArtBas WHERE ArtBas.Farg = Farg.Farg),"/Nei")        
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
  chWorkSheets:Range("A1:C2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:C2"):Font:Italic = TRUE.

  /*
  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("C:C"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/
  */

  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("A:A"):NumberFormat = "# ##0".
  /*
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  */

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:C1"):Merge().
  chWorkSheets:Range("A1:C1"):HorizontalAlignment = 3.
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
  chWorkSheets:Columns("A:C"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:X2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier - <Blank>".
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(piAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = cKunde.
  chWorkSheets:PageSetup:RightFooter    = cSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:C".
  chWorkSheets:PageSetup:Orientation    = 1.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("A3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  
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

  STATUS DEFAULT "".

  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

