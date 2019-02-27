&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tBestHode NO-UNDO LIKE BestHode.


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

------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR IO-Liste AS CHAR  NO-UNDO.
  define var wLevNr   as int   no-undo.
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER IO-Liste AS CHAR  NO-UNDO.
  DEFINE INPUT        PARAMETER wLevNr   AS int   NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define DB-BROWSER BROWSE-BestHode

&scoped-define br-tabell BestHode

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  BestNr
&scoped-define sok-field1-def  INT
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define where-phrase1 {&br-tabell}.{&sok-field1}
&scoped-define index-col1  BestNr
&scoped-define sok-phrase1 ~
  FIND FIRST b{&br-tabell} WHERE ~
  b{&br-tabell}.LevNr = wLevNr and ~
  b{&br-tabell}.{&sok-field1} >= ~
     INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

&scoped-define ant-sok-field   1
&scoped-define init-datatype   INT /* CHAR, INT, DATE */

&scoped-define init-sel-phrase ~
  FIND b{&br-tabell} where ~
  b{&br-tabell}.LevNr = wLevNr and ~
  b{&br-tabell}.{&sok-field1} = INT(ENTRY(wAntEntries,IO-Liste)) no-lock no-error.

&scoped-define Assign-IO-Liste STRING(t{&br-tabell}.{&sok-field1})

/* om man i options på query før varje browser klickar på fields-used
   måste man definiera BUFFER-COPY-USING USING felt1 felt2 ... */
&scoped-define BUFFER-COPY-USING USING Bestillingsdato ArtikkelNr

/* Denna måste finnas før riktig sortering vid uppstart */
&scoped-define init-sortby-phrase {&br-tabell}.{&sok-field1} /* DESCENDING */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.

/* Lokale variabler ---                                                 */
def var wBeskrivelse as char format "x(30)"   no-undo.
def var wAntValgt    as int                   no-undo.
def var retur-verdi  as char initial "AVBRYT" no-undo.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEFINE VAR wFirstRowId       AS ROWID       NO-UNDO.
DEFINE VAR wAntEntries       AS INT         NO-UNDO.
DEFINE VAR wRecidListe       AS CHAR        NO-UNDO.
DEFINE VAR wRowIdListe       AS CHAR        NO-UNDO.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-BestHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BestHode tBestHode

/* Definitions for BROWSE BROWSE-BestHode                               */
&Scoped-define FIELDS-IN-QUERY-BROWSE-BestHode BestHode.BestNr ~
BestHode.BestillingsDato BestHode.ArtikkelNr BestHode.LevKod HentFarve() 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-BestHode BestHode.BestNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-BestHode BestHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-BestHode BestHode
&Scoped-define QUERY-STRING-BROWSE-BestHode FOR EACH BestHode ~
      WHERE BestHode.LevNr = wLevNr and ~
BestHode.BestStat >= 2 and ~
BestHode.BestStat <= 3 NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-BestHode OPEN QUERY BROWSE-BestHode FOR EACH BestHode ~
      WHERE BestHode.LevNr = wLevNr and ~
BestHode.BestStat >= 2 and ~
BestHode.BestStat <= 3 NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-BestHode BestHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-BestHode BestHode


/* Definitions for BROWSE BROWSE-Tag                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Tag tBestHode.BestNr ~
tBestHode.BestillingsDato tBestHode.ArtikkelNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Tag tBestHode.BestNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Tag tBestHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Tag tBestHode
&Scoped-define QUERY-STRING-BROWSE-Tag FOR EACH tBestHode NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Tag OPEN QUERY BROWSE-Tag FOR EACH tBestHode NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Tag tBestHode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Tag tBestHode


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-BestHode}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-BestHode BROWSE-Tag Btn_OK Btn_Cancel ~
Btn_Help BUTTON-Sok RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-INT FILL-IN-SOK-CHAR ~
FILL-IN-SOK-DATE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentFarve Dialog-Frame 
FUNCTION HentFarve RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SORTLIST Dialog-Frame 
FUNCTION SORTLIST RETURNS CHARACTER
  ( INPUT wSortString AS CHARACTER, INPUT wDelimiter AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VgLopNr Dialog-Frame 
FUNCTION VgLopNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Hjelp" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-AlleFra 
     LABEL "<< &Alle fra" 
     SIZE 15 BY 1.1.

DEFINE BUTTON BUTTON-LeggTil 
     LABEL "Legg til &>>" 
     SIZE 15 BY 1.1.

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1.

DEFINE BUTTON BUTTON-TrekkFra 
     LABEL "&<< Trekk fra" 
     SIZE 15 BY 1.1.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INT AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94.4 BY 19.52.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 19.52.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-BestHode FOR 
      BestHode
    FIELDS(BestHode.BestNr
      BestHode.BestillingsDato
      BestHode.ArtikkelNr
      BestHode.LevKod) SCROLLING.

DEFINE QUERY BROWSE-Tag FOR 
      tBestHode
    FIELDS(tBestHode.BestNr
      tBestHode.BestillingsDato
      tBestHode.ArtikkelNr) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-BestHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-BestHode Dialog-Frame _STRUCTURED
  QUERY BROWSE-BestHode NO-LOCK DISPLAY
      BestHode.BestNr FORMAT ">>>>>>>9":U
      BestHode.BestillingsDato FORMAT "99/99/9999":U
      BestHode.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      BestHode.LevKod FORMAT "x(30)":U
      HentFarve() COLUMN-LABEL "Färg" WIDTH 17
  ENABLE
      BestHode.BestNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 92.4 BY 18.81.

DEFINE BROWSE BROWSE-Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Tag Dialog-Frame _STRUCTURED
  QUERY BROWSE-Tag NO-LOCK DISPLAY
      tBestHode.BestNr FORMAT ">>>>>>>9":U
      tBestHode.BestillingsDato FORMAT "99/99/9999":U
      tBestHode.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
  ENABLE
      tBestHode.BestNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 45 BY 18.81 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-BestHode AT ROW 2.91 COL 2.6
     BUTTON-LeggTil AT ROW 6.95 COL 97
     BUTTON-TrekkFra AT ROW 8.14 COL 97
     BUTTON-AlleFra AT ROW 9.33 COL 97
     BROWSE-Tag AT ROW 2.91 COL 114
     Btn_OK AT ROW 22.43 COL 1.6
     Btn_Cancel AT ROW 22.43 COL 14.6
     Btn_Help AT ROW 22.43 COL 146.8
     FILL-IN-SOK-INT AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.48 COL 1.6 NO-LABEL
     BUTTON-Sok AT ROW 1.48 COL 22.6
     RECT-4 AT ROW 2.67 COL 1.6
     RECT-5 AT ROW 2.67 COL 113
     SPACE(0.79) SKIP(1.34)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg bestillinger"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tBestHode T "?" NO-UNDO skotex BestHode
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
/* BROWSE-TAB BROWSE-BestHode 1 Dialog-Frame */
/* BROWSE-TAB BROWSE-Tag BUTTON-AlleFra Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-AlleFra IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-LeggTil IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-TrekkFra IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-BestHode
/* Query rebuild information for BROWSE BROWSE-BestHode
     _TblList          = "skotex.BestHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "BestHode.LevNr = wLevNr and
BestHode.BestStat >= 2 and
BestHode.BestStat <= 3"
     _FldNameList[1]   > skotex.BestHode.BestNr
"BestNr" ? ">>>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = skotex.BestHode.BestillingsDato
     _FldNameList[3]   = skotex.BestHode.ArtikkelNr
     _FldNameList[4]   = skotex.BestHode.LevKod
     _FldNameList[5]   > "_<CALC>"
"HentFarve()" "Färg" ? ? ? ? ? ? ? ? no ? no no "17" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-BestHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Tag
/* Query rebuild information for BROWSE BROWSE-Tag
     _TblList          = "Temp-Tables.tBestHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > Temp-Tables.tBestHode.BestNr
"BestNr" ? ">>>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.tBestHode.BestillingsDato
     _FldNameList[3]   = Temp-Tables.tBestHode.ArtikkelNr
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-Tag */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg bestillinger */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-BestHode
&Scoped-define SELF-NAME BROWSE-BestHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode Dialog-Frame
ON CURSOR-LEFT OF BROWSE-BestHode IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode Dialog-Frame
ON CURSOR-RIGHT OF BROWSE-BestHode IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-BestHode IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO BUTTON-LeggTil.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode Dialog-Frame
ON START-SEARCH OF BROWSE-BestHode IN FRAME Dialog-Frame
DO:
  IF BROWSE BROWSE-Tag:NUM-SELECTED-ROWS > 0 THEN DO:
      IF BROWSE BROWSE-Tag:DESELECT-ROWS() THEN.
      BUTTON-TrekkFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  END.
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-BestHode Dialog-Frame
ON VALUE-CHANGED OF BROWSE-BestHode IN FRAME Dialog-Frame
DO:
    IF BROWSE BROWSE-Tag:NUM-SELECTED-ROWS > 0 THEN DO:
        IF BROWSE BROWSE-Tag:DESELECT-ROWS() THEN.
        ASSIGN BUTTON-TrekkFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    END.
    ASSIGN BUTTON-LeggTil:SENSITIVE   IN FRAME {&FRAME-NAME} = SELF:NUM-SELECTED-ROWS > 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Tag
&Scoped-define SELF-NAME BROWSE-Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Tag Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-Tag IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO BUTTON-TrekkFra.
  RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Tag Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Tag IN FRAME Dialog-Frame
DO:
    IF {&DB-BROWSER}:NUM-SELECTED-ROWS > 0 THEN DO:
        IF {&DB-BROWSER}:DESELECT-ROWS() THEN.
        ASSIGN BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.
    END.
    ASSIGN BUTTON-TrekkFra:SENSITIVE  IN FRAME {&FRAME-NAME} = SELF:NUM-SELECTED-ROWS > 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
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
    RUN FyllSelectList.
    RETUR-VERDI = wRecidListe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AlleFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AlleFra Dialog-Frame
ON CHOOSE OF BUTTON-AlleFra IN FRAME Dialog-Frame /* << Alle fra */
DO:
    Run TilFra("AlleFra").
    ASSIGN BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} = {&DB-BROWSER}:NUM-SELECTED-ROWS > 0
           BUTTON-TrekkFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           SELF:SENSITIVE = NO.
    APPLY "ENTRY" TO {&DB-BROWSER}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-LeggTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-LeggTil Dialog-Frame
ON CHOOSE OF BUTTON-LeggTil IN FRAME Dialog-Frame /* Legg til >> */
DO:
     RUN TilFra("Til").
     ASSIGN SELF:SENSITIVE = {&DB-BROWSER}:SELECT-NEXT-ROW()
            BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    APPLY "ENTRY" TO BROWSE {&DB-BROWSER}.
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
       IF BROWSE-Tag:NUM-SELECTED-ROWS > 0 THEN
           APPLY "ENTRY" TO BROWSE-Tag.
       ELSE
           APPLY "ENTRY" TO {&DB-BROWSER}.
   ELSE DO:
       RUN SD-Reposition.
       ASSIGN BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-TrekkFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-TrekkFra Dialog-Frame
ON CHOOSE OF BUTTON-TrekkFra IN FRAME Dialog-Frame /* << Trekk fra */
DO:
     RUN TilFra("Fra").
     IF BROWSE-Tag:FOCUSED-ROW <> ? THEN DO:
         ASSIGN SELF:SENSITIVE = BROWSE-Tag:SELECT-FOCUSED-ROW()
                BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
         APPLY "ENTRY" TO BROWSE-Tag.
     END.
     ELSE DO:
         ASSIGN SELF:SENSITIVE = NO
                BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} = 
                                  {&DB-BROWSER}:NUM-SELECTED-ROWS > 0.
         APPLY "ENTRY" TO {&DB-BROWSER}.
     END.
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


&Scoped-define BROWSE-NAME BROWSE-BestHode
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON ANY-PRINTABLE OF Btn_OK,Btn_Cancel,Btn_Help,BUTTON-LeggTil,
                    BUTTON-TrekkFra,BUTTON-AlleFra,BUTTON-SOK,
                    BROWSE-Tag,{&DB-BROWSER} DO:
  RUN SD-ANY-PRINTABLE.
  APPLY "VALUE-CHANGED" TO BROWSE {&DB-BROWSER}.
  RETURN NO-APPLY.
END.    
ON " " OF {&DB-BROWSER},BROWSE-Tag DO:
   RETURN NO-APPLY.
END.   

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    wOrgBgCol = {&sortby-col1}:LABEL-BGCOLOR IN BROWSE {&DB-BROWSER}.
    RUN LabelColor.
    RUN Move-Fill-To-Top.
    IF IO-Liste <> "" THEN
        RUN InitSelected.
    RUN enable_UI.
    {lng.i}
    REPOSITION {&DB-BROWSER} TO ROW 1.
    RUN Init-Read-Only.
    IF CAN-FIND(FIRST t{&br-tabell}) THEN DO:
        {&OPEN-QUERY-BROWSE-Tag}
        BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    END.
    APPLY "ENTRY" TO BROWSE {&DB-BROWSER}.
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
        &scope WHERE-PHRASE {&where-phrase1}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col1}:DATA-TYPE IN BROWSE {&DB-BROWSER}.
    END.
    &IF DEFINED(where-phrase2) &THEN
    WHEN 2 THEN DO:  
        &scope WHERE-PHRASE {&where-phrase2}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
        wDataType = {&sortby-col2}:DATA-TYPE IN BROWSE {&DB-BROWSER}.
     END.
     &ENDIF
  END CASE.
  IF NOT wDataType BEGINS wAktivDataType THEN DO:
      CASE wDataType:
          WHEN "INTEGER" THEN
              wAktivDataType = "INT".
          WHEN "CHARACTER" THEN
              wAktivDataType = "CHAR".
          WHEN "DATE" THEN
              wAktivDataType = "DATE".
      END CASE.
      RUN Move-Fill-To-Top.
  END.
  &scope WHERE-PHRASE {&where-phrase1}
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
  ENABLE BROWSE-BestHode BROWSE-Tag Btn_OK Btn_Cancel Btn_Help BUTTON-Sok 
         RECT-4 RECT-5 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FyllSelectList Dialog-Frame 
PROCEDURE FyllSelectList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR wAntSel AS INT NO-UNDO.
    ASSIGN IO-Liste    = ""
           wRecidListe = "".
    FOR EACH t{&br-tabell}:
        ASSIGN IO-Liste = IO-Liste + 
                            IF IO-Liste <> "" 
                              THEN "," + 
                          {&Assign-IO-Liste}
                   ELSE {&Assign-IO-Liste}
               wRecidListe = wRecidListe + IF wRecidListe <> "" THEN "," + STRING(RECID(t{&br-tabell}))
                   ELSE STRING(RECID(t{&br-tabell})).
    END.
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
    {&sortby-col1}:READ-ONLY IN BROWSE {&DB-BROWSER} = YES
    &IF DEFINED(sortby-col2) &THEN
    {&sortby-col2}:READ-ONLY IN BROWSE {&DB-BROWSER} = YES
    &ENDIF
    t{&sortby-col1}:READ-ONLY IN BROWSE BROWSE-Tag = YES
    .
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSelected Dialog-Frame 
PROCEDURE InitSelected :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO wAntEntries = 1 TO NUM-ENTRIES(IO-Liste) WITH FRAME {&FRAME-NAME}:

        /* {&init-sel-phrase} */

        FIND b{&br-tabell} where 
          b{&br-tabell}.{&sok-field1} = int(ENTRY(wAntEntries,IO-Liste)) no-lock no-error.
        
        IF AVAIL b{&br-tabell} THEN DO:
            CREATE t{&br-tabell}.
            assign
              wAntValgt                  = wAntValgt + 1            
              t{&br-tabell}.{&sok-field1} = int(entry(wAntEntries,IO-Liste)).
            BUFFER-COPY b{&br-tabell} {&BUFFER-COPY-USING} TO t{&br-tabell}.
        END.
    END.
    RELEASE t{&br-tabell}.
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
   {&sortby-col1}:Label-bgcolor IN BROWSE {&DB-BROWSER} = 
                                   IF wAktivCol = 1 THEN wSortBgCol ELSE wOrgBgCol
   &IF DEFINED(sortby-col2) &THEN
   {&sortby-col2}:Label-bgcolor IN BROWSE {&DB-BROWSER} = 
                                   IF wAktivCol = 2 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col3) &THEN
   {&sortby-col3}:Label-bgcolor IN BROWSE {&DB-BROWSER} = 
                                   IF wAktivCol = 3 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col4) &THEN
   {&sortby-col4}:Label-bgcolor IN BROWSE {&DB-BROWSER} = 
                                   IF wAktivCol = 4 THEN wSortBgCol ELSE wOrgBgCol
   &ENDIF
   t{&br-tabell}.{&sok-field1}:Label-bgcolor IN BROWSE BROWSE-Tag = wSortBgCol
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
             WHEN "INT" THEN  DO:
                 ASSIGN FILL-IN-SOK-INT:SENSITIVE  = YES
                        FILL-IN-SOK-CHAR:SENSITIVE = NO
                        FILL-IN-SOK-DATE:SENSITIVE = NO.
                 ASSIGN FILL-IN-SOK-INT:SCREEN-VALUE = "".
                 IF FILL-IN-SOK-INT:MOVE-TO-TOP() THEN.
             END.
             WHEN "CHAR" THEN DO:
                 ASSIGN FILL-IN-SOK-INT:SENSITIVE  = NO
                        FILL-IN-SOK-CHAR:SENSITIVE = YES
                        FILL-IN-SOK-DATE:SENSITIVE = NO.
                 ASSIGN FILL-IN-SOK-CHAR:SCREEN-VALUE = "".
                 IF FILL-IN-SOK-CHAR:MOVE-TO-TOP() THEN.
             END.
             WHEN "DATE" THEN DO:
                 ASSIGN FILL-IN-SOK-INT:SENSITIVE  = NO
                        FILL-IN-SOK-CHAR:SENSITIVE = NO
                        FILL-IN-SOK-DATE:SENSITIVE = YES.
                 ASSIGN FILL-IN-SOK-DATE:SCREEN-VALUE = "".
                 IF FILL-IN-SOK-DATE:MOVE-TO-TOP() THEN.
             END.
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
    APPLY "ENTRY" TO BROWSE {&DB-BROWSER}.
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
        REPOSITION {&DB-BROWSER} TO ROWID rowid(b{&br-tabell}) NO-ERROR.
        CASE wAktivDataType:
            WHEN "INT" THEN
                ASSIGN FILL-IN-SOK-INT:SCREEN-VALUE = "".
            WHEN "CHAR" THEN
                ASSIGN FILL-IN-SOK-CHAR:SCREEN-VALUE = "".
            WHEN "DATE" THEN
                ASSIGN FILL-IN-SOK-DATE:SCREEN-VALUE = "".
        END CASE.
        APPLY "ENTRY" TO BROWSE {&DB-BROWSER}.
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
    DEFINE VAR wAntSel AS INTE NO-UNDO.
    DEFINE VAR h-Curr-Col as widget.
    ASSIGN h-Curr-Col = BROWSE {&DB-BROWSER}:CURRENT-COLUMN
           BUTTON-LeggTil:Sensitive IN FRAME {&FRAME-NAME} = NO.
    APPLY "end-search" TO {&DB-BROWSER} IN FRAME {&FRAME-NAME}.  
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
    END CASE.
   
/*    RUN SD-Reposition. */
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
    RUN BytSortering (wNyCol).
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilFra Dialog-Frame 
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
    
    def buffer bufVgKat for VgKat.
    
    CASE wType:
        WHEN "Til" THEN DO:
            DO wAntSel = 1 TO BROWSE {&DB-BROWSER}:NUM-SELECTED-ROWS:
                IF BROWSE {&DB-BROWSER}:FETCH-SELECTED-ROW (wAntsel) THEN
                  FIND t{&br-tabell} WHERE t{&br-tabell}.{&sok-field1} = {&br-tabell}.{&sok-field1} NO-ERROR.
                IF NOT AVAIL t{&br-tabell} THEN DO:
                   CREATE t{&br-tabell}.
                   assign
                     wAntValgt                   = wAntValgt + 1
                     t{&br-tabell}.{&sok-field1} = {&br-tabell}.{&sok-field1}.
                   BUFFER-COPY {&br-tabell} {&BUFFER-COPY-USING} TO t{&br-tabell}.
                   wOpen-Query = YES.
                END.
            END.
        END.
        WHEN "Fra" THEN DO:
            IF BROWSE BROWSE-Tag:NUM-SELECTED-ROWS > 1 THEN DO:
                SLETT:
                DO wAntSel = 1 TO BROWSE BROWSE-Tag:NUM-SELECTED-ROWS:
                    IF BROWSE BROWSE-Tag:FETCH-SELECTED-ROW (wAntSel) THEN
                      do:
                        do transaction:
                          DELETE t{&br-tabell}.
                          assign
                            wAntValgt = wAntValgt - 1.
                        end. /* TRANSACTION */
                      end.
                END. /* SLETT */
                wOpen-Query = YES.
            END.
            ELSE DO:
                DO WHILE BROWSE BROWSE-Tag:NUM-SELECTED-ROWS > 0:
                    IF BROWSE BROWSE-Tag:FETCH-SELECTED-ROW (1) THEN
                      SLETT:
                      do:
                        do transaction:
                          DELETE t{&br-tabell}.
                          assign
                            wAntValgt = wAntValgt - 1.
                        end. /* TRANSACTION */
                      end. /* SLETT */
                    IF BROWSE BROWSE-Tag:DELETE-SELECTED-ROW (1) THEN.
                END.
                wOpen-Query = NO.
            END.
        END.
        WHEN "AlleFra" THEN DO:
            FOR EACH t{&br-tabell}:
                DELETE t{&br-tabell}.
            END.
            wOpen-Query = YES.
       END.
    END CASE.
    IF wOpen-Query THEN DO:
        {&OPEN-QUERY-BROWSE-Tag}
        REPOSITION BROWSE-Tag TO ROW 1 NO-ERROR.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HentFarve Dialog-Frame 
FUNCTION HentFarve RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = BestHode.ArtikkelNr NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  RETURN IF AVAIL Farg THEN Farg.FarBeskr ELSE "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SORTLIST Dialog-Frame 
FUNCTION SORTLIST RETURNS CHARACTER
  ( INPUT wSortString AS CHARACTER, INPUT wDelimiter AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wSortItems AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT
     SIZE-PIXELS 1 BY 1 NO-UNDO.
  FORM wSortItems NO-LABEL WITH FRAME {&FRAME-NAME}.

  ASSIGN wSortItems:DELIMITER  = wDelimiter
         wSortItems:LIST-ITEMS = wSortString. 

  RETURN wSortItems:LIST-ITEMS.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VgLopNr Dialog-Frame 
FUNCTION VgLopNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  def var wVgLopNr as char no-undo.

  if available BestHode then
    do:
      find ArtBas no-lock where
        ArtBas.ArtikkelNr = BestHode.ArtikkelNr.
      wVgLopNr = string(ArtBas.Vg,"9999") + "/" + string(ArtBas.LopNr,"9999").
    end.
  else
    wVgLopNr = "".

  RETURN wVgLopNr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

