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
DEFINE VAR cEDB-System LIKE ImpKonv.EDB-System NO-UNDO.
DEFINE VAR cTabell     LIKE ImpKonv.Tabell     NO-UNDO.

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR iTabell     AS   INT                NO-UNDO.
  DEFINE VAR cInterntID  LIKE ImpKonv.InterntID  NO-UNDO.
  DEFINE VAR cTittel     AS   CHAR               NO-UNDO.
  ASSIGN
      cEDB-System = "FLEXICON,TEST"
      iTabell     = 0
      cInterntID  = "45"
      cTittel     = "Kobling av leverandør"
      .
&ELSE
  DEFINE INPUT PARAMETER iTabell     AS   INT                NO-UNDO.
  DEFINE INPUT PARAMETER cInterntID  LIKE ImpKonv.InterntID  NO-UNDO.
  DEFINE INPUT PARAMETER cTittel     AS   CHAR               NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell   ImpKonv

/* Kolonne 1 som det kan sorteres på */
&scoped-define sok-field1  EksterntId
&scoped-define sok-field1-def  CHAR
&scoped-define sortby-col1 {&br-tabell}.{&sok-field1}
&scoped-define sortby-phrase1 {&br-tabell}.{&sok-field1} /* DESCENDING */
&scoped-define index-col1  EksterntId
&scoped-define sok-phrase1 FIND FIRST b{&br-tabell} WHERE ~
                b{&br-tabell}.EDB-System = cSystem AND ~
                b{&br-tabell}.Tabell     = cTabell AND ~
                b{&br-tabell}.InterntId  = cInterntID AND ~
                b{&br-tabell}.{&sok-field1} >= ~
                INPUT FILL-IN-SOK-{&sok-field1-def} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Om du vill sortera på fler kolonner:
   Kopiera kolonnedefinition 1 ovan och ændra talet 1 till næsta ikke anvænda.
   Ændra sedan alla fælt.
   Ændra ant-sok-field nedan till det antall som du vill sortera på.
   Ændra init-datatype nedan till datatyp før den sorteringskolonne du startar på
   Ændra æven alla andra scoped-define før att passa ditt program
   Ændra iAktivCol nedan till INIT ønskad startkolonne.
*/

&scoped-define ip-variabel cInterntId
&scoped-define return-ip   cInterntId = IF AVAILABLE ImpKonv THEN ImpKonv.InterntId ELSE "".
&scoped-define assign-retur-verdi retur-verdi = string(rowid(ImpKonv)).
&scoped-define ant-sok-field   1
&scoped-define init-datatype   char /* CHAR, INT, DATE */

/* Om du ønskar en startup-record annars fjern. */
&scoped-define init-phrase  FIND b{&br-tabell} WHERE ~
                           b{&br-tabell}.EDB-System = cSystem AND ~
                           b{&br-tabell}.Tabell     = cTabell AND ~
                           b{&br-tabell}.InterntId  = cInterntID AND ~
                           b{&br-tabell}.{&sok-field1} = ~
                           {&ip-variabel} USE-INDEX {&index-col1} NO-LOCK NO-ERROR.

/* Denna måste finnas før riktig sortering vid uppstart */
&scoped-define init-sortby-phrase {&br-tabell}.{&sok-field1} /* DESCENDING */

&scoped-define SletteMelding "Skall posten tas bort?"
&scoped-define KanSlettes

&scoped-define VedlikeholdsRutine d-vimpkonv.w

/* Buffer og Temp-Table Definisjoner ---                                */
DEFINE BUFFER b{&br-tabell} FOR {&br-tabell}.

/* Lokale variabler ---                                                 */

def var retur-verdi as char initial "AVBRYT" no-undo.

DEFINE VAR cSystem           AS CHAR        NO-UNDO.
DEFINE VAR iAktivCol         AS INT INIT 1  NO-UNDO.
DEFINE VAR iOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR iSortBgCol        AS INT INIT 15 NO-UNDO.
DEFINE VAR cAktivDataType    AS CHAR INIT "{&init-datatype}" NO-UNDO.
DEFINE VAR rRowId            AS ROWID       NO-UNDO.
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
&Scoped-define BROWSE-NAME BROWSE-ImpKonv

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ImpKonv

/* Definitions for BROWSE BROWSE-ImpKonv                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-ImpKonv ImpKonv.InterntID ~
ImpKonv.EksterntID ImpKonv.Merknad 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-ImpKonv 
&Scoped-define QUERY-STRING-BROWSE-ImpKonv FOR EACH ImpKonv ~
      WHERE ImpKonv.EDB-System = cSystem and ~
ImpKonv.Tabell     = cTabell and ~
ImpKonv.InterntId  = cInterntId NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-ImpKonv OPEN QUERY BROWSE-ImpKonv FOR EACH ImpKonv ~
      WHERE ImpKonv.EDB-System = cSystem and ~
ImpKonv.Tabell     = cTabell and ~
ImpKonv.InterntId  = cInterntId NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-ImpKonv ImpKonv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-ImpKonv ImpKonv


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-SOK-DATE FILL-IN-SOK-INT ~
FILL-IN-SOK-CHAR BUTTON-Sok CB-EdbSystem BROWSE-ImpKonv BUTTON-Ny ~
BUTTON-Endre BUTTON-Slett Btn_Help Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOK-DATE FILL-IN-SOK-INT ~
FILL-IN-SOK-CHAR CB-EdbSystem 

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

DEFINE VARIABLE CB-EdbSystem AS CHARACTER FORMAT "X(256)":U 
     LABEL "EDB-System" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

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
DEFINE QUERY BROWSE-ImpKonv FOR 
      ImpKonv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-ImpKonv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-ImpKonv Dialog-Frame _STRUCTURED
  QUERY BROWSE-ImpKonv NO-LOCK DISPLAY
      ImpKonv.InterntID FORMAT "X(12)":U
      ImpKonv.EksterntID FORMAT "X(12)":U WIDTH 11.4
      ImpKonv.Merknad FORMAT "X(40)":U WIDTH 55.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 85 BY 7.38 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-SOK-DATE AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-INT AT ROW 1.48 COL 2 NO-LABEL
     FILL-IN-SOK-CHAR AT ROW 1.48 COL 2 NO-LABEL
     BUTTON-Sok AT ROW 1.48 COL 21.6
     CB-EdbSystem AT ROW 1.48 COL 49 COLON-ALIGNED
     BROWSE-ImpKonv AT ROW 2.91 COL 2
     BUTTON-Ny AT ROW 2.91 COL 88
     BUTTON-Endre AT ROW 4.24 COL 88
     BUTTON-Slett AT ROW 5.57 COL 88
     Btn_Help AT ROW 10.62 COL 88
     Btn_OK AT ROW 10.81 COL 2.2
     Btn_Cancel AT ROW 10.81 COL 14.6
     SPACE(77.19) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Søkeliste importkonvertering"
         CANCEL-BUTTON Btn_Cancel.


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
/* BROWSE-TAB BROWSE-ImpKonv CB-EdbSystem Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK-CHAR IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-DATE IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOK-INT IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-ImpKonv
/* Query rebuild information for BROWSE BROWSE-ImpKonv
     _TblList          = "SkoTex.ImpKonv"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "ImpKonv.EDB-System = cSystem and
ImpKonv.Tabell     = cTabell and
ImpKonv.InterntId  = cInterntId"
     _FldNameList[1]   = SkoTex.ImpKonv.InterntID
     _FldNameList[2]   > SkoTex.ImpKonv.EksterntID
"ImpKonv.EksterntID" ? ? "character" ? ? ? ? ? ? no ? no no "11.4" yes no no "U" "" ""
     _FldNameList[3]   > SkoTex.ImpKonv.Merknad
"ImpKonv.Merknad" ? ? "character" ? ? ? ? ? ? no ? no no "55.8" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-ImpKonv */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Søkeliste importkonvertering */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-ImpKonv
&Scoped-define SELF-NAME BROWSE-ImpKonv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-ImpKonv Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-ImpKonv IN FRAME Dialog-Frame
DO:
  RUN SD-ANY-PRINTABLE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-ImpKonv Dialog-Frame
ON CURSOR-LEFT OF BROWSE-ImpKonv IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("LEFT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-ImpKonv Dialog-Frame
ON CURSOR-RIGHT OF BROWSE-ImpKonv IN FRAME Dialog-Frame
DO:
  IF {&ant-sok-field} = 1 THEN
    RETURN NO-APPLY.
  RUN SD-CURSOR ("RIGHT").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-ImpKonv Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-ImpKonv IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE" TO Btn_OK.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-ImpKonv Dialog-Frame
ON START-SEARCH OF BROWSE-ImpKonv IN FRAME Dialog-Frame
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
    rRowId = rowid({&br-tabell}).
  run {&VedlikeholdsRutine} (cSystem,
                             cTabell,
                             cInterntId,
                             cTittel,
                             input-output rRowId,
                             "Endre").
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
    rRowId = ?.
  run {&VedlikeholdsRutine} (cSystem,
                             cTabell,
                             cInterntId,
                             cTittel,
                             input-output rRowId,
                             "Ny").
  if return-value = "AVBRYT" then
    return no-apply.

  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  find b{&br-tabell} no-lock where
    ROWID(b{&br-tabell}) = rRowId no-error.
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
  view-as alert-box buttons yes-no-cancel title "Bekräfta" 
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
   CASE iAktivCol:
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


&Scoped-define SELF-NAME CB-EdbSystem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-EdbSystem Dialog-Frame
ON VALUE-CHANGED OF CB-EdbSystem IN FRAME Dialog-Frame /* EDB-System */
DO:
  ASSIGN
      cSystem = CB-EDBSystem:SCREEN-VALUE
      .
  {&OPEN-QUERY-{&BROWSE-NAME}}
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

/* Henter parametre for konvertering. */
/*{syspara.i 1 2 iTabell cEDB-System}*/
FOR EACH EkstVPILev NO-LOCK:
    ASSIGN
        cEDB-System = cEDB-System + 
                      (IF cEDB-System = ""
                         THEN ""
                         ELSE ",") + 
                      EkstVPILev.KortNavn.
END.
/* {syspar2.i 1 2 iTabell cTabell} */
{getvpitabell.i &LevNr = 1 /*Alltid HK */
                &Nr    = iTabell
                &Felt  = cTabell}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    iOrgBgCol = {&sortby-col1}:Label-bgcolor in browse {&BROWSE-NAME}.
    RUN LabelColor.
    RUN Move-Fill-To-Top.
    
    ASSIGN
        CB-EDBSystem              = entry(1,cEDB-System)
        CB-EDBSystem:LIST-ITEMS   = cEDB-System
        CB-EDBsystem:SCREEN-VALUE = ENTRY(1,cEDB-System) 
        cSystem                   = ENTRY(1,cEDB-System)
        FRAME Dialog-Frame:TITLE = cTittel + " " + cInterntId
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
    
    {&OPEN-QUERY-{&BROWSE-NAME}}

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
  IF wNyCol = iAktivCol THEN
      RETURN NO-APPLY.
  ASSIGN iAktivCol = wNyCol.
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
          cAktivDataType = "INT".
      WHEN "CHARACTER" THEN
          cAktivDataType = "CHAR".
      WHEN "DATE" THEN
          cAktivDataType = "DATE".
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
  DISPLAY FILL-IN-SOK-DATE FILL-IN-SOK-INT FILL-IN-SOK-CHAR CB-EdbSystem 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-SOK-DATE FILL-IN-SOK-INT FILL-IN-SOK-CHAR BUTTON-Sok 
         CB-EdbSystem BROWSE-ImpKonv BUTTON-Ny BUTTON-Endre BUTTON-Slett 
         Btn_Help Btn_OK Btn_Cancel 
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
                                   IF iAktivCol = 1 THEN iSortBgCol ELSE iOrgBgCol
   &IF DEFINED(sortby-col2) &THEN
   {&sortby-col2}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF iAktivCol = 2 THEN iSortBgCol ELSE iOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col3) &THEN
   {&sortby-col3}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF iAktivCol = 3 THEN iSortBgCol ELSE iOrgBgCol
   &ENDIF
   &IF DEFINED(sortby-col4) &THEN
   {&sortby-col4}:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF iAktivCol = 4 THEN iSortBgCol ELSE iOrgBgCol
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
        CASE cAktivDataType:
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
    CASE cAktivDataType:
        WHEN "INT" OR WHEN "DATE" THEN DO:
            IF key-function(lastkey) < "0" OR key-function(lastkey) > "9" THEN
                RETURN NO-APPLY.
        END.
    END CASE.
    CASE cAktivDataType:
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
            IF iAktivCol = 1 THEN
                RUN SortNyCol({&ant-sok-field}).
            ELSE
                RUN SortNyCol(iAktivCol - 1).
        WHEN "RIGHT" THEN
            IF iAktivCol = {&ant-sok-field} THEN
                RUN SortNyCol(1).
            ELSE
                RUN SortNyCol(iAktivCol + 1).
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
        CASE cAktivDataType:
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
            IF iAktivCol <> 1 THEN DO:
                RUN SortNyCol(1).
                RETURN NO-APPLY.
            END.
        &IF DEFINED(sok-field2) &THEN 
        WHEN "{&sok-field2}" THEN
            IF iAktivCol <> 2 THEN DO:
                RUN SortNyCol(2).
                RETURN NO-APPLY.
            END.
        &ENDIF
        &IF DEFINED(sok-field3) &THEN 
        WHEN "{&sok-field3}" THEN
            IF iAktivCol <> 3 THEN DO:
                RUN SortNyCol(3).
                RETURN NO-APPLY.
            END.
        &ENDIF
        &IF DEFINED(sok-field4) &THEN 
        WHEN "{&sok-field4}" THEN
            IF iAktivCol <> 4 THEN DO:
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

