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
  DEFINE VAR wBildNr LIKE BildeRegister.BildNr INIT 1 NO-UNDO.
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER wBildNr LIKE BildeRegister.BildNr NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell   BildeRegister

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  BildNr
&scoped-define sok-field1-def  INT
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} /* DESCENDING */
&scoped-define index-col1  Bilde
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Kolonne 2 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field2  Tekst
&scoped-define sok-field2-def  CHAR
&scoped-define sortby-col2 {&br-tabell}.{&sok-field2}
&scoped-define sortby-phrase2 {&br-tabell}.{&sok-field2} /* DESCENDING */
&scoped-define index-col2  Tekst
&scoped-define sok-phrase2 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field2} >= ~
               INPUT FILL-IN-SOK-{&sok-field2-def} USE-INDEX {&index-col2} NO-LOCK NO-ERROR.

/* Kolonne 3 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field3  Dato
&scoped-define sok-field3-def  DATE
&scoped-define sortby-col3 {&br-tabell}.{&sok-field3}
&scoped-define sortby-phrase3 {&br-tabell}.{&sok-field3} /* DESCENDING */
&scoped-define index-col3  Dato
&scoped-define sok-phrase3 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field3} >= ~
               INPUT FILL-IN-SOK-{&sok-field3-def} USE-INDEX {&index-col3} NO-LOCK NO-ERROR.

/* Kolonne 4 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field4  LevNr
&scoped-define sok-field4-def  INT
&scoped-define sortby-col4 {&br-tabell}.{&sok-field4}
&scoped-define sortby-phrase4 {&br-tabell}.{&sok-field4} /* DESCENDING */
&scoped-define index-col4  Leverandor2
&scoped-define sok-phrase4 FIND FIRST b{&br-tabell} WHERE b{&br-tabell}.{&sok-field4} >= ~
               INPUT FILL-IN-SOK-{&sok-field4-def} USE-INDEX {&index-col4} NO-LOCK NO-ERROR.

/* Om du vill sortera på fler kolonner:
   Kopiera kolonnedefinition 1 ovan och ændra talet 1 till næsta ikke anvænda.
   Ændra sedan alla fælt.
   Ændra ant-sok-field nedan till det antall som du vill sortera på.
   Ændra init-datatype nedan till datatyp før den sorteringskolonne du startar på
   Ændra æven alla andra scoped-define før att passa ditt program
   Ændra wAktivCol nedan till INIT ønskad startkolonne.
*/

&scoped-define ip-variabel wBildNr
&scoped-define return-ip   wBildNr = BildeRegister.BildNr.
&scoped-define assign-retur-verdi retur-verdi = string(recid(BildeRegister)).
&scoped-define ant-sok-field   4
&scoped-define init-datatype   INT /* CHAR, INT, DATE */

/* Om du ønskar en startup-record annars fjern. */
&scoped-define init-phrase  FIND b{&br-tabell} WHERE b{&br-tabell}.{&sok-field1} = ~
                           {&ip-variabel} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Denna måste finnas før riktig sortering vid uppstart */
&scoped-define init-sortby-phrase {&br-tabell}.{&sok-field1} /* DESCENDING */

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.

/* Lokale variabler ---                                                 */
def var wOk as log no-undo.

def var retur-verdi as char initial "AVBRYT" no-undo.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Bilder

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Bilderegister levbas

/* Definitions for BROWSE BROWSE-Bilder                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Bilder Bilderegister.BildNr ~
Bilderegister.Tekst Bilderegister.Dato Bilderegister.LevNr ~
Bilderegister.LevArtNr Bilderegister.RegistrertDato Bilderegister.Sted ~
Bilderegister.Merknad Bilderegister.FilNavn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Bilder Bilderegister.BildNr ~
Bilderegister.Tekst Bilderegister.Dato Bilderegister.LevNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Bilder Bilderegister
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Bilder Bilderegister
&Scoped-define OPEN-QUERY-BROWSE-Bilder OPEN QUERY BROWSE-Bilder FOR EACH Bilderegister NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Bilder Bilderegister
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Bilder Bilderegister


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame LevBas.levnamn 
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Bilder}
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH levbas SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame levbas
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame levbas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-SOK-INT FILL-IN-SOK-DATE ~
FILL-IN-SOK-CHAR BUTTON-Sok BROWSE-Bilder BT-Zoom Btn_OK Btn_Cancel RECT-14 ~
RECT-15 
&Scoped-Define DISPLAYED-FIELDS Bilderegister.Dato Bilderegister.Sted ~
Bilderegister.BildNr Bilderegister.Tekst Bilderegister.LevArtNr ~
Bilderegister.LevNr LevBas.levnamn Bilderegister.Merknad ~
Bilderegister.RegistrertAv Bilderegister.RegistrertDato ~
Bilderegister.BrukerID Bilderegister.EDato 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-INT FILL-IN-SOK-DATE ~
FILL-IN-SOK-CHAR FI-RTid FI-ETid 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BT-Zoom 
     LABEL "Zoom..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Sok 
     LABEL "Søk" 
     SIZE 10.2 BY 1.1.

DEFINE VARIABLE FI-ETid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-RTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK-INT AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 10.19.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 8.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Bilder FOR 
      Bilderegister SCROLLING.

DEFINE QUERY Dialog-Frame FOR 
      levbas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Bilder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Bilder Dialog-Frame _STRUCTURED
  QUERY BROWSE-Bilder NO-LOCK DISPLAY
      Bilderegister.BildNr COLUMN-LABEL "Bilde *" FORMAT "zzzzz9":U
      Bilderegister.Tekst COLUMN-LABEL "Billedtekst *" FORMAT "X(30)":U
      Bilderegister.Dato COLUMN-LABEL "Fotografert *" FORMAT "99/99/9999":U
      Bilderegister.LevNr COLUMN-LABEL "Lev *" FORMAT "zzzzz9":U
      Bilderegister.LevArtNr COLUMN-LABEL "Artikkelnummer" FORMAT "X(20)":U
      Bilderegister.RegistrertDato COLUMN-LABEL "Registrert" FORMAT "99/99/9999":U
      Bilderegister.Sted FORMAT "X(20)":U
      Bilderegister.Merknad FORMAT "X(40)":U
      Bilderegister.FilNavn FORMAT "X(50)":U
  ENABLE
      Bilderegister.BildNr
      Bilderegister.Tekst
      Bilderegister.Dato
      Bilderegister.LevNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 84 BY 19.52 ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-SOK-INT AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 1.48 COL 23
     BROWSE-Bilder AT ROW 2.91 COL 2
     Bilderegister.Dato AT ROW 13.86 COL 102 COLON-ALIGNED
          LABEL "Fotografert"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     Bilderegister.Sted AT ROW 13.86 COL 118.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28.2 BY 1
     Bilderegister.BildNr AT ROW 14.91 COL 102 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Bilderegister.Tekst AT ROW 14.91 COL 113.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     Bilderegister.LevArtNr AT ROW 15.95 COL 102 COLON-ALIGNED
          LABEL "Lev.Art.Nr"
          VIEW-AS FILL-IN 
          SIZE 44.4 BY 1
     Bilderegister.LevNr AT ROW 16.95 COL 102 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     LevBas.levnamn AT ROW 16.95 COL 112.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33.8 BY 1
     Bilderegister.Merknad AT ROW 17.95 COL 102 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44.4 BY 1
     Bilderegister.RegistrertAv AT ROW 19.95 COL 102 COLON-ALIGNED
          LABEL "Opprettet"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     Bilderegister.RegistrertDato AT ROW 19.95 COL 118.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     FI-RTid AT ROW 19.95 COL 134.6 COLON-ALIGNED NO-LABEL
     Bilderegister.BrukerID AT ROW 21 COL 102 COLON-ALIGNED
          LABEL "Endret"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
     Bilderegister.EDato AT ROW 21 COL 118.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     FI-ETid AT ROW 21 COL 134.6 COLON-ALIGNED NO-LABEL
     BT-Zoom AT ROW 22.71 COL 89.2
     Btn_OK AT ROW 22.71 COL 124.8
     Btn_Cancel AT ROW 22.71 COL 137
     RECT-14 AT ROW 2.91 COL 89.2
     RECT-15 AT ROW 13.67 COL 89.2
     "Billedinformasjon" VIEW-AS TEXT
          SIZE 19.2 BY .62 AT ROW 13.1 COL 90
     SPACE(40.59) SKIP(10.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Billederegister"
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
                                                                        */
/* BROWSE-TAB BROWSE-Bilder BUTTON-Sok Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Bilderegister.BildNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-Bilder:NUM-LOCKED-COLUMNS IN FRAME Dialog-Frame     = 1.

/* SETTINGS FOR FILL-IN Bilderegister.BrukerID IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Bilderegister.Dato IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Bilderegister.EDato IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ETid IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-RTid IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Bilderegister.LevArtNr IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.LevNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.Merknad IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.RegistrertAv IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Bilderegister.RegistrertDato IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.Sted IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.Tekst IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Bilder
/* Query rebuild information for BROWSE BROWSE-Bilder
     _TblList          = "SkoTex.Bilderegister"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > SkoTex.Bilderegister.BildNr
"Bilderegister.BildNr" "Bilde *" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > SkoTex.Bilderegister.Tekst
"Bilderegister.Tekst" "Billedtekst *" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > SkoTex.Bilderegister.Dato
"Bilderegister.Dato" "Fotografert *" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > SkoTex.Bilderegister.LevNr
"Bilderegister.LevNr" "Lev *" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > SkoTex.Bilderegister.LevArtNr
"Bilderegister.LevArtNr" "Artikkelnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > SkoTex.Bilderegister.RegistrertDato
"Bilderegister.RegistrertDato" "Registrert" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   = SkoTex.Bilderegister.Sted
     _FldNameList[8]   = SkoTex.Bilderegister.Merknad
     _FldNameList[9]   = SkoTex.Bilderegister.FilNavn
     _Query            is OPENED
*/  /* BROWSE BROWSE-Bilder */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.levbas"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 3.29
       COLUMN          = 92.6
       HEIGHT          = 9.71
       WIDTH           = 54.4
       HIDDEN          = no
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      CtrlFrame:MOVE-AFTER(BROWSE-Bilder:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Billederegister */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Bilder
&Scoped-define SELF-NAME BROWSE-Bilder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bilder Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-Bilder IN FRAME Dialog-Frame
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bilder Dialog-Frame
ON CURSOR-LEFT OF BROWSE-Bilder IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bilder Dialog-Frame
ON CURSOR-RIGHT OF BROWSE-Bilder IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bilder Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-Bilder IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO Btn_OK.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bilder Dialog-Frame
ON START-SEARCH OF BROWSE-Bilder IN FRAME Dialog-Frame
DO:
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Bilder Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Bilder IN FRAME Dialog-Frame
DO:  
  run VisInfo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-Zoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-Zoom Dialog-Frame
ON CHOOSE OF BT-Zoom IN FRAME Dialog-Frame /* Zoom... */
DO:
  run d-visbil.w (input recid(BildeRegister)).
  return no-apply.
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
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame Dialog-Frame OCX.DblClick
PROCEDURE CtrlFrame.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  run d-visbil.w (input recid(BildeRegister)).
  return no-apply.  



END PROCEDURE.

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

{runlib.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    wOrgBgCol = {&sortby-col1}:Label-bgcolor in browse {&BROWSE-NAME}.
    RUN LabelColor.
    RUN Move-Fill-To-Top.
    RUN enable_UI.
    IF {&ant-sok-field} > 1 THEN
        RUN Init-Read-Only.
    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END.
    &ENDIF
    
    run VisInfo.
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
IF VALID-HANDLE(CtrlFrame) THEN
    DELETE OBJECT CtrlFrame NO-ERROR.
IF VALID-HANDLE(chCtrlFrame) THEN
    RELEASE OBJECT chCtrlFrame.
ASSIGN chCtrlFrame = ?.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "c-bbilder.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "c-bbilder.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY FILL-IN-SOK-INT FILL-IN-SOK-DATE FILL-IN-SOK-CHAR FI-RTid FI-ETid 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Bilderegister THEN 
    DISPLAY Bilderegister.Dato Bilderegister.Sted Bilderegister.BildNr 
          Bilderegister.Tekst Bilderegister.LevArtNr Bilderegister.LevNr 
          Bilderegister.Merknad Bilderegister.RegistrertAv 
          Bilderegister.RegistrertDato Bilderegister.BrukerID 
          Bilderegister.EDato 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnamn 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-SOK-INT FILL-IN-SOK-DATE FILL-IN-SOK-CHAR BUTTON-Sok 
         BROWSE-Bilder BT-Zoom Btn_OK Btn_Cancel RECT-14 RECT-15 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisInfo Dialog-Frame 
PROCEDURE VisInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wFilNavn as char no-undo.
  
  find first LevBas no-lock where
    LevBas.LevNr = BildeRegister.LevNr no-error.

  display
    Bilderegister.BildNr 
    Bilderegister.LevArtNr 
    Bilderegister.LevNr 
    Bilderegister.Merknad 
    Bilderegister.Sted 
    Bilderegister.Tekst 
    BildeRegister.Dato
    BildeRegister.RegistrertDato
    BildeRegister.EDato
    BildeRegister.BrukerID
    BildeRegister.RegistrertAv
    levbas.levnamn when available LevBas  
    string(BildeRegister.ETid,"HH:MM:SS") @ FI-ETid
    string(BildeRegister.RegistrertTid,"HH:MM:SS") @ FI-RTid
  with frame Dialog-Frame.

  if valid-handle(wLibHandle) then
    do:
      run HentBildePeker in wLibHandle (BildeRegister.BildNr,1,BildeRegister.FilNavn,output wFilNavn).
      assign
        chCtrlFrame:Picbuf:filename  = search(wFilNavn)
        chCtrlFrame:Picbuf:AutoScale = True.        
      wOk = chCtrlFrame:Picbuf:load.
    end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

