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
  DEFINE VAR wLapTopNr LIKE LapTop.LapTopNr INIT 31 NO-UNDO.
&ELSE
  DEFINE INPUT-OUTPUT PARAMETER wLapTopNr LIKE LapTop.LapTopNr NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell   LapTop
&scoped-define OptWhere 

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  LapTopNr
&scoped-define sok-field1-def  INT
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} /* DESCENDING */
&scoped-define index-col1  LapTop
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE ~
               {&OptWhere} ~
                b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Kolonne 2 som det kan sorteras på, fjern om du bara vill ha 1 */
&scoped-define sok-field2  Navn
&scoped-define sok-field2-def  CHAR
&scoped-define sortby-col2 {&br-tabell}.{&sok-field2}
&scoped-define sortby-phrase2 {&br-tabell}.{&sok-field2} /* DESCENDING */
&scoped-define index-col2  Navn
&scoped-define sok-phrase2 FIND FIRST b{&br-tabell} WHERE ~
               {&OptWhere} ~
               b{&br-tabell}.{&sok-field2} >= ~
               INPUT FILL-IN-SOK-{&sok-field2-def} USE-INDEX {&index-col2} NO-LOCK NO-ERROR.

/* Om du vill sortera på fler kolonner:
   Kopiera kolonnedefinition 1 ovan och ændra talet 1 till næsta ikke anvænda.
   Ændra sedan alla fælt.
   Ændra ant-sok-field nedan till det antall som du vill sortera på.
   Ændra init-datatype nedan till datatyp før den sorteringskolonne du startar på
   Ændra æven alla andra scoped-define før att passa ditt program
   Ændra wAktivCol nedan till INIT ønskad startkolonne.
*/

&scoped-define ip-variabel wLapTopNr
&scoped-define return-ip   wLapTopNr = if available {&br-tabell}  ~
                                       then {&br-tabell}.{&sok-field1} ~
                                       else  {&sok-field1-def}("").
&scoped-define assign-retur-verdi retur-verdi = if available {&br-tabell} ~
                                                  then string(recid({&br-tabell})) ~
                                                  else "".
&scoped-define SletteMelding " "
&scoped-define KanSlettes 
&scoped-define ant-sok-field   2
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

def var retur-verdi as char initial "AVBRYT" no-undo.

DEFINE VAR wAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR wAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEFINE VAR wRecid            AS recid       NO-UNDO.
DEFINE VAR wOk               AS log         NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-LapTop

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LapTop

/* Definitions for BROWSE BROWSE-LapTop                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-LapTop LapTop.LapTopNr LapTop.Navn ~
LapTop.LokDB-Katalog LapTop.LokDB-Navn LapTop.Merknad LapTop.LD-SentralDB ~
LapTop.OppkoblParam LapTop.ServerDBKopi LapTop.DatoSisOverf ~
LapTop.TidSisOverf LapTop.BrukerSisOverf LapTop.DatoSisteKopi ~
LapTop.TidSistKopi LapTop.BrukerSistKopi LapTop.EDato LapTop.BrukerID ~
LapTop.RegistrertAv LapTop.RegistrertDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-LapTop LapTop.LapTopNr ~
LapTop.Navn 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-LapTop LapTop
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-LapTop LapTop
&Scoped-define OPEN-QUERY-BROWSE-LapTop OPEN QUERY BROWSE-LapTop FOR EACH LapTop NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-LapTop LapTop
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-LapTop LapTop


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-LapTop}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-SOK-DATE FILL-IN-SOK-INT ~
FILL-IN-SOK-CHAR BUTTON-Sok BROWSE-LapTop BUTTON-Ny BUTTON-Endre ~
BUTTON-Slett Btn_Help Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-DATE FILL-IN-SOK-INT ~
FILL-IN-SOK-CHAR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
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

DEFINE BUTTON BUTTON-Endre 
     LABEL "&Endre..." 
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
DEFINE QUERY BROWSE-LapTop FOR 
      LapTop SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-LapTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-LapTop Dialog-Frame _STRUCTURED
  QUERY BROWSE-LapTop NO-LOCK DISPLAY
      LapTop.LapTopNr COLUMN-LABEL "LapTop *"
      LapTop.Navn COLUMN-LABEL "Navn *"
      LapTop.LokDB-Katalog
      LapTop.LokDB-Navn
      LapTop.Merknad
      LapTop.LD-SentralDB
      LapTop.OppkoblParam
      LapTop.ServerDBKopi
      LapTop.DatoSisOverf
      LapTop.TidSisOverf
      LapTop.BrukerSisOverf
      LapTop.DatoSisteKopi
      LapTop.TidSistKopi
      LapTop.BrukerSistKopi
      LapTop.EDato
      LapTop.BrukerID
      LapTop.RegistrertAv
      LapTop.RegistrertDato
  ENABLE
      LapTop.LapTopNr
      LapTop.Navn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 142 BY 19.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-INT AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 1.48 COL 21.6
     BROWSE-LapTop AT ROW 2.91 COL 2
     BUTTON-Ny AT ROW 2.91 COL 145
     BUTTON-Endre AT ROW 4.24 COL 145
     BUTTON-Slett AT ROW 5.57 COL 145
     Btn_Help AT ROW 22.91 COL 144.8
     Btn_OK AT ROW 23 COL 2
     Btn_Cancel AT ROW 23 COL 14.4
     SPACE(133.79) SKIP(0.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Søkeliste LapTop register"
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
/* BROWSE-TAB BROWSE-LapTop BUTTON-Sok Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-LapTop:NUM-LOCKED-COLUMNS IN FRAME Dialog-Frame = 2.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-LapTop
/* Query rebuild information for BROWSE BROWSE-LapTop
     _TblList          = "SkoTex.LapTop"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > SkoTex.LapTop.LapTopNr
"LapTopNr" "LapTop *" ? "integer" ? ? ? ? ? ? yes ? no no ?
     _FldNameList[2]   > SkoTex.LapTop.Navn
"Navn" "Navn *" ? "character" ? ? ? ? ? ? yes ? no no ?
     _FldNameList[3]   = SkoTex.LapTop.LokDB-Katalog
     _FldNameList[4]   = SkoTex.LapTop.LokDB-Navn
     _FldNameList[5]   = SkoTex.LapTop.Merknad
     _FldNameList[6]   = SkoTex.LapTop.LD-SentralDB
     _FldNameList[7]   = SkoTex.LapTop.OppkoblParam
     _FldNameList[8]   = SkoTex.LapTop.ServerDBKopi
     _FldNameList[9]   = SkoTex.LapTop.DatoSisOverf
     _FldNameList[10]   = SkoTex.LapTop.TidSisOverf
     _FldNameList[11]   = SkoTex.LapTop.BrukerSisOverf
     _FldNameList[12]   = SkoTex.LapTop.DatoSisteKopi
     _FldNameList[13]   = SkoTex.LapTop.TidSistKopi
     _FldNameList[14]   = SkoTex.LapTop.BrukerSistKopi
     _FldNameList[15]   = SkoTex.LapTop.EDato
     _FldNameList[16]   = SkoTex.LapTop.BrukerID
     _FldNameList[17]   = SkoTex.LapTop.RegistrertAv
     _FldNameList[18]   = SkoTex.LapTop.RegistrertDato
     _Query            is OPENED
*/  /* BROWSE BROWSE-LapTop */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Søkeliste LapTop register */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-LapTop
&Scoped-define SELF-NAME BROWSE-LapTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LapTop Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-LapTop IN FRAME Dialog-Frame
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LapTop Dialog-Frame
ON CURSOR-LEFT OF BROWSE-LapTop IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LapTop Dialog-Frame
ON CURSOR-RIGHT OF BROWSE-LapTop IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LapTop Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-LapTop IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO Btn_OK.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LapTop Dialog-Frame
ON START-SEARCH OF BROWSE-LapTop IN FRAME Dialog-Frame
DO:
  RUN SD-START-SEARCH.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny Dialog-Frame
ON CHOOSE OF BUTTON-Ny IN FRAME Dialog-Frame /* Ny... */
DO:
  assign 
    wRecid = ?.
  run {&VedlikeholdsRutine} (input-output wRecid,"Ny").
  if return-value = "AVBRYT" then
    return no-apply.

  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  find b{&br-tabell} no-lock where
    recid(b{&br-tabell}) = wRecid no-error.
  RUN SD-Reposition.
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


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    wOrgBgCol = {&sortby-col1}:Label-bgcolor in browse {&BROWSE-NAME}.
    RUN LabelColor.
    RUN Move-Fill-To-Top.
    {lng.i} RUN enable_UI.
    IF {&ant-sok-field} > 1 THEN
        RUN Init-Read-Only.
    &IF DEFINED(init-phrase) &THEN
    {&init-phrase}
    IF AVAILABLE b{&br-tabell} THEN DO:
        RUN SD-Reposition.
    END.
    &ENDIF
    APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
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
  DISPLAY FILL-IN-SOK-DATE FILL-IN-SOK-INT FILL-IN-SOK-CHAR 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-SOK-DATE FILL-IN-SOK-INT FILL-IN-SOK-CHAR BUTTON-Sok 
         BROWSE-LapTop BUTTON-Ny BUTTON-Endre BUTTON-Slett Btn_Help Btn_OK 
         Btn_Cancel 
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

