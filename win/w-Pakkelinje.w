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
DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cStorl      AS CHAR FORMAT "x(6)" NO-UNDO.
DEFINE VARIABLE iVg         LIKE ArtBas.Vg    LABEL "Vg"     NO-UNDO.
DEFINE VARIABLE iLopnr      LIKE ArtBas.Lopnr LABEL "Löpenr" FORMAT "zzzzz9" NO-UNDO.
DEFINE VARIABLE iLevnr      LIKE ArtBas.Levnr FORMAT "zzzzz9" NO-UNDO.
DEFINE VARIABLE cLevNamn    LIKE LevBas.LevNamn NO-UNDO.
DEFINE VARIABLE cLevFargKod LIKE ArtBas.LevFargKod NO-UNDO.
DEFINE VARIABLE cLevkod     AS CHARACTER      FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE cBeskr      AS CHARACTER      FORMAT "x(25)" LABEL "Beskrivelse" NO-UNDO.
DEFINE VARIABLE cPakkeBeskr AS CHAR FORMAT "x(30)" LABEL "Pakkebeskrivelse"  NO-UNDO.
DEFINE VARIABLE cEmptyField AS CHARACTER LABEL "" NO-UNDO.
DEFINE VARIABLE lPakke      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lIKasse     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE wOk AS LOGICAL    NO-UNDO.
DEFINE VARIABLE hJmfRutine  AS HANDLE     NO-UNDO.
define temp-table tmpChild
  field wChild as handle.

{runlib.i} /* Starter procedurebibloteket. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-Pakkelinje
&Scoped-define BROWSE-NAME BROWSE-IPakke

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PakkeLinje ArtBas

/* Definitions for BROWSE BROWSE-IPakke                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-IPakke PakkeLinje.Pakkenr ~
PakkeLinje.ArtikkelNr cLevKod ArtBas.Beskr cLevFargKod ArtBas.Vg ~
ArtBas.LopNr cStorl PakkeLinje.Antall cLevNamn "" @ cEmptyField 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-IPakke 
&Scoped-define QUERY-STRING-BROWSE-IPakke FOR EACH PakkeLinje ~
      WHERE PakkeLinje.PkArtikkelNr = dArtikkelNr NO-LOCK, ~
      EACH ArtBas OF PakkeLinje NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-IPakke OPEN QUERY BROWSE-IPakke FOR EACH PakkeLinje ~
      WHERE PakkeLinje.PkArtikkelNr = dArtikkelNr NO-LOCK, ~
      EACH ArtBas OF PakkeLinje NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-IPakke PakkeLinje ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-IPakke PakkeLinje
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-IPakke ArtBas


/* Definitions for BROWSE BROWSE-Pakkelinje                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Pakkelinje PakkeLinje.PkArtikkelNr ~
cLevkod cBeskr cLevFargKod iVg iLopNr cStorl PakkeLinje.Antall cLevNamn ~
"" @ cEmptyField 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Pakkelinje 
&Scoped-define QUERY-STRING-BROWSE-Pakkelinje FOR EACH PakkeLinje ~
      WHERE PakkeLinje.ArtikkelNr = dArtikkelNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Pakkelinje OPEN QUERY BROWSE-Pakkelinje FOR EACH PakkeLinje ~
      WHERE PakkeLinje.ArtikkelNr = dArtikkelNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Pakkelinje PakkeLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Pakkelinje PakkeLinje


/* Definitions for FRAME FRAME-Pakkelinje                               */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-57 BROWSE-IPakke BROWSE-Pakkelinje ~
BUTTON-Ny BUTTON-Endre BUTTON-Slett BUTTON-TilPakke B-ByttArtBas B-Jamfor ~
FI-IPakkeTxt FI-PakkeTxt 
&Scoped-Define DISPLAYED-OBJECTS FI-IPakkeTxt FI-PakkeTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ArtBasBeskr C-Win 
FUNCTION ArtBasBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuLopNr C-Win 
FUNCTION fuLopNr RETURNS INTEGER
  ( INPUT lArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OKreg C-Win 
FUNCTION OKreg RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PakkeBeskr C-Win 
FUNCTION PakkeBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-ByttArtBas 
     LABEL "&Gå til artikkel" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-Jamfor 
     LABEL "Sa&mmenlign..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Endre 
     LABEL "Endre..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "Ny..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "Slett" 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-TilPakke 
     LABEL "Pakkemedlem..." 
     SIZE 22 BY 1.14.

DEFINE VARIABLE FI-IPakkeTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Medlem av pakke" 
      VIEW-AS TEXT 
     SIZE 156.2 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Pakkenr AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Pakkenr" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-PakkeTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Pakkens innhold" 
      VIEW-AS TEXT 
     SIZE 156.2 BY .62
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-IPakke FOR 
      PakkeLinje, 
      ArtBas SCROLLING.

DEFINE QUERY BROWSE-Pakkelinje FOR 
      PakkeLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-IPakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-IPakke C-Win _STRUCTURED
  QUERY BROWSE-IPakke NO-LOCK DISPLAY
      PakkeLinje.Pakkenr FORMAT "ZZZZ":U
      PakkeLinje.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      cLevKod COLUMN-LABEL "Levkod" FORMAT "x(25)":U WIDTH 20
      ArtBas.Beskr FORMAT "x(40)":U WIDTH 50
      cLevFargKod COLUMN-LABEL "Farge" FORMAT "x(25)":U WIDTH 20
      ArtBas.Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U
      ArtBas.LopNr COLUMN-LABEL "Løpenr" FORMAT "zzzzz9":U
      cStorl COLUMN-LABEL "Str"
      PakkeLinje.Antall FORMAT "->>,>>9":U
      cLevNamn COLUMN-LABEL "Leverandør" WIDTH 20
      "" @ cEmptyField COLUMN-LABEL "." WIDTH 10.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 164.2 BY 19.62 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-Pakkelinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Pakkelinje C-Win _STRUCTURED
  QUERY BROWSE-Pakkelinje NO-LOCK DISPLAY
      PakkeLinje.PkArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      cLevkod COLUMN-LABEL "Levkode" FORMAT "x(25)":U WIDTH 20
      cBeskr COLUMN-LABEL "Beskrivelse" FORMAT "x(40)":U WIDTH 50
      cLevFargKod COLUMN-LABEL "Farge" FORMAT "x(25)":U WIDTH 20
      iVg COLUMN-LABEL "Vg"
      iLopNr COLUMN-LABEL "Løpenr" FORMAT "zzzzz9":U
      cStorl COLUMN-LABEL "Størrelse"
      PakkeLinje.Antall FORMAT "->>,>>9":U
      cLevNamn COLUMN-LABEL "Leverandør" WIDTH 20
      "" @ cEmptyField
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 164.2 BY 19.62 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Pakkelinje
     FI-Pakkenr AT ROW 1.33 COL 12 COLON-ALIGNED
     BROWSE-IPakke AT ROW 3.24 COL 1.8
     BROWSE-Pakkelinje AT ROW 3.29 COL 1.8
     BUTTON-Ny AT ROW 8.76 COL 173.2
     BUTTON-Endre AT ROW 10.05 COL 173.2
     BUTTON-Slett AT ROW 11.33 COL 173.2
     BUTTON-TilPakke AT ROW 12.62 COL 173.2
     B-ByttArtBas AT ROW 13.91 COL 173.2
     B-Jamfor AT ROW 15.19 COL 173.2
     FI-IPakkeTxt AT ROW 2.62 COL 1.8 NO-LABEL
     FI-PakkeTxt AT ROW 2.62 COL 1.8 NO-LABEL
     RECT-57 AT ROW 3.48 COL 171.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 201.8 BY 23.05.


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
         TITLE              = ""
         HEIGHT             = 31.95
         WIDTH              = 201.8
         MAX-HEIGHT         = 31.95
         MAX-WIDTH          = 201.8
         VIRTUAL-HEIGHT     = 31.95
         VIRTUAL-WIDTH      = 201.8
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
/* SETTINGS FOR FRAME FRAME-Pakkelinje
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-IPakke FI-Pakkenr FRAME-Pakkelinje */
/* BROWSE-TAB BROWSE-Pakkelinje BROWSE-IPakke FRAME-Pakkelinje */
/* SETTINGS FOR FILL-IN FI-IPakkeTxt IN FRAME FRAME-Pakkelinje
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Pakkenr IN FRAME FRAME-Pakkelinje
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-Pakkenr:HIDDEN IN FRAME FRAME-Pakkelinje           = TRUE.

/* SETTINGS FOR FILL-IN FI-PakkeTxt IN FRAME FRAME-Pakkelinje
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-IPakke
/* Query rebuild information for BROWSE BROWSE-IPakke
     _TblList          = "skotex.PakkeLinje,skotex.ArtBas OF skotex.PakkeLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.PakkeLinje.PkArtikkelNr = dArtikkelNr"
     _FldNameList[1]   = skotex.PakkeLinje.Pakkenr
     _FldNameList[2]   = skotex.PakkeLinje.ArtikkelNr
     _FldNameList[3]   > "_<CALC>"
"cLevKod" "Levkod" "x(25)" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > skotex.ArtBas.Beskr
"ArtBas.Beskr" ? "x(40)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"cLevFargKod" "Farge" "x(25)" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > skotex.ArtBas.Vg
"ArtBas.Vg" "Vg" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > skotex.ArtBas.LopNr
"ArtBas.LopNr" "Løpenr" "zzzzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"cStorl" "Str" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = skotex.PakkeLinje.Antall
     _FldNameList[10]   > "_<CALC>"
"cLevNamn" "Leverandør" ? ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
""""" @ cEmptyField" "." ? ? ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-IPakke */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Pakkelinje
/* Query rebuild information for BROWSE BROWSE-Pakkelinje
     _TblList          = "SkoTex.PakkeLinje"
     _Options          = "NO-LOCK"
     _Where[1]         = "SkoTex.PakkeLinje.ArtikkelNr = dArtikkelNr"
     _FldNameList[1]   = SkoTex.PakkeLinje.PkArtikkelNr
     _FldNameList[2]   > "_<CALC>"
"cLevkod" "Levkode" "x(25)" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"cBeskr" "Beskrivelse" "x(40)" ? ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"cLevFargKod" "Farge" "x(25)" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"iVg" "Vg" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"iLopNr" "Løpenr" "zzzzz9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"cStorl" "Størrelse" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = SkoTex.PakkeLinje.Antall
     _FldNameList[9]   > "_<CALC>"
"cLevNamn" "Leverandør" ? ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
""""" @ cEmptyField" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-Pakkelinje */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Pakkelinje
/* Query rebuild information for FRAME FRAME-Pakkelinje
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Pakkelinje */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME FRAME-Pakkelinje:HANDLE
       ROW             = 3.86
       COLUMN          = 172.8
       HEIGHT          = 4.19
       WIDTH           = 23.4
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Image-Sko:MOVE-AFTER(BROWSE-Pakkelinje:HANDLE IN FRAME FRAME-Pakkelinje).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ByttArtBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ByttArtBas C-Win
ON CHOOSE OF B-ByttArtBas IN FRAME FRAME-Pakkelinje /* Gå til artikkel */
DO:
    RUN HentVis IN wParentHandle (?,IF NOT lPakke THEN PakkeLinje.ArtikkelNr
                                   ELSE PakkeLinje.PkArtikkelNr).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Jamfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Jamfor C-Win
ON CHOOSE OF B-Jamfor IN FRAME FRAME-Pakkelinje /* Sammenlign... */
DO:
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cArtikkelnr AS CHARACTER  NO-UNDO.
    DEFINE BUFFER bPakkeLinje FOR PakkeLinje.
    DEFINE BUFFER bArtBas     FOR ArtBas.
    IF NOT AVAIL ArtBas THEN
        RETURN.
    IF lPakke = TRUE THEN DO:
        ASSIGN iCount = 0.
        FOR EACH bPakkeLinje WHERE bPakkeLinje.ArtikkelNr = dArtikkelNr NO-LOCK:

             ASSIGN iCount      = iCount + 1.
                    cArtikkelNr = cArtikkelNr +
                  (IF cArtikkelNr = "" THEN "" ELSE ",") + STRING(bPakkelinje.PkArtikkelNr).
             IF iCount = 5 THEN
                 LEAVE.
        END.
    END.
    ELSE DO:
        ASSIGN iCount = 0.
        FOR EACH bPakkeLinje WHERE bPakkeLinje.PkArtikkelNr = dArtikkelNr NO-LOCK,
                 EACH bArtBas OF bPakkeLinje NO-LOCK:
             ASSIGN iCount      = iCount + 1.
                    cArtikkelNr = cArtikkelNr +
                  (IF cArtikkelNr = "" THEN "" ELSE ",") + STRING(bArtBas.ArtikkelNr).
             IF iCount = 5 THEN
                 LEAVE.
        END.
    END.
    IF NOT VALID-HANDLE(hJmfRutine) THEN DO:
        RUN w-bildejmf.w PERSISTENT SET hJmfRutine.
        create tmpChild.
        ASSIGN tmpChild.wChild = hJmfRutine.
        RUN NyArtBas IN hJmfRutine (dArtikkelNr).
        DO iCount = 1 TO NUM-ENTRIES(cArtikkelNr):
            RUN NyArtBas IN hJmfRutine (DECI(ENTRY(iCount,cArtikkelNr))).
        END.
    END.
    ELSE
        RUN NyArtBas IN hJmfRutine (Erstattningsvare.ArtikkelNr).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-IPakke
&Scoped-define SELF-NAME BROWSE-IPakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-IPakke C-Win
ON ROW-DISPLAY OF BROWSE-IPakke IN FRAME FRAME-Pakkelinje
DO:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-LOCK NO-ERROR.
  FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = PakkeLinje.StrKode NO-ERROR.
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
      ASSIGN iVg     = ArtBas.Vg     
             iLopnr  = ArtBas.Lopnr  
             iLevnr  = ArtBas.LevNr  
             cLevkod = ArtBas.Levkod 
             cBeskr  = ArtBas.Beskr
             cLevFargKod = ArtBas.LevFargKod
             cLevNamn = IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE ""
             cStorl   = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ""
             .
  ELSE
      ASSIGN
          iVg     = 0     
          iLopnr  = 0  
          iLevnr  = 0  
          cLevkod = "" 
          cBeskr  = ""
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-IPakke C-Win
ON VALUE-CHANGED OF BROWSE-IPakke IN FRAME FRAME-Pakkelinje
DO:
    IF BROWSE BROWSE-IPakke:FOCUSED-ROW <> ? THEN
        RUN VisBilde2(1).
    ELSE
        chImage-Sko:Picbuf:CLEAR(2).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Pakkelinje
&Scoped-define SELF-NAME BROWSE-Pakkelinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Pakkelinje C-Win
ON DEFAULT-ACTION OF BROWSE-Pakkelinje IN FRAME FRAME-Pakkelinje
DO:
  APPLY "CHOOSE" TO BUTTON-Endre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Pakkelinje C-Win
ON ROW-DISPLAY OF BROWSE-Pakkelinje IN FRAME FRAME-Pakkelinje
DO:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-LOCK NO-ERROR.
  FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = PakkeLinje.StrKode NO-ERROR.
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
      ASSIGN iVg     = ArtBas.Vg     
             iLopnr  = ArtBas.Lopnr  
             iLevnr  = ArtBas.LevNr  
             cLevkod = ArtBas.Levkod 
             cBeskr  = ArtBas.Beskr
             cLevFargKod = ArtBas.LevFargKod
             cLevNamn = IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE ""
             cStorl   = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ""
             .
  ELSE
      ASSIGN
          iVg     = 0     
          iLopnr  = 0  
          iLevnr  = 0  
          cLevkod = "" 
          cBeskr  = ""
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Pakkelinje C-Win
ON VALUE-CHANGED OF BROWSE-Pakkelinje IN FRAME FRAME-Pakkelinje
DO:
    IF BROWSE BROWSE-PakkeLinje:FOCUSED-ROW <> ? THEN
        RUN VisBilde(1).
    ELSE
        chImage-Sko:Picbuf:CLEAR(2).
  
    ASSIGN BUTTON-Endre:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
           BUTTON-Slett:SENSITIVE = BUTTON-Endre:SENSITIVE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME FRAME-Pakkelinje /* Endre... */
DO:
  IF NOT OKreg() THEN
    RETURN NO-APPLY.
  RUN d-pakkelinje.w (ROWID(PakkeLinje),dArtikkelNr,THIS-PROCEDURE).
  BROWSE BROWSE-Pakkelinje:REFRESH().
  APPLY "ENTRY" TO BROWSE BROWSE-Pakkelinje.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME FRAME-Pakkelinje /* Ny... */
DO:
  IF NOT OKreg() THEN
      RETURN NO-APPLY.
  IF NOT CAN-FIND(FIRST StrekKode WHERE StrekKode.Artikkelnr = dArtikkelNr) THEN DO:
      MESSAGE "Pakke " FI-Pakkenr:SCREEN-VALUE " mangler strekkode."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN d-pakkelinje.w (?,dArtikkelNr,THIS-PROCEDURE).
  ASSIGN B-ByttArtBas:SENSITIVE = BROWSE BROWSE-Pakkelinje:FOCUSED-ROW <> ?
         BUTTON-Endre:SENSITIVE = B-ByttArtBas:SENSITIVE
         BUTTON-Slett:SENSITIVE = B-ByttArtBas:SENSITIVE
         B-ByttArtBas:SENSITIVE = B-ByttArtBas:SENSITIVE.
  IF BROWSE-Pakkelinje:FOCUSED-ROW <> ? THEN
      APPLY "VALUE-CHANGED" TO BROWSE-Pakkelinje.
  APPLY "ENTRY" TO BROWSE BROWSE-Pakkelinje. 
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME FRAME-Pakkelinje /* Slett */
DO:
  IF NOT OKreg() THEN
      RETURN NO-APPLY.
  MESSAGE "Vill du slette pakkeposten? " VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO UPDATE wSvar AS LOGI.
  IF wSvar THEN DO:
      FIND CURRENT PakkeLinje EXCLUSIVE.
      DELETE PakkeLinje.
      BROWSE BROWSE-Pakkelinje:DELETE-CURRENT-ROW().
  END.
  IF BROWSE BROWSE-Pakkelinje:FOCUSED-ROW <> ? THEN
      APPLY "ENTRY" TO BROWSE BROWSE-Pakkelinje.
  ELSE
      ASSIGN B-ByttArtBas:SENSITIVE = FALSE.
  APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Pakkelinje.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-TilPakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-TilPakke C-Win
ON CHOOSE OF BUTTON-TilPakke IN FRAME FRAME-Pakkelinje /* Pakkemedlem... */
DO:
    DEFINE VARIABLE cBildeFilNavn AS CHARACTER NO-UNDO.
/*   if not available ArtBas then                                              */
/*     return no-apply.                                                        */
/*   create tmpChild.                                                          */
/*   run w-barttranslogg.w PERSISTENT set tmpChild.wChild (ArtBas.ArtikkelNr). */
    RUN getBildeFilNavn IN wParentHandle (OUTPUT cBildeFilNavn).
    RUN d-tilpakke.w (dArtikkelNr,cBildeFilNavn).
    IF RETURN-VALUE <> "AVBRYT" THEN 
        {&OPEN-QUERY-BROWSE-IPakke}
    APPLY "ENTRY" TO BROWSE BROWSE-IPakke.
    ASSIGN B-ByttArtBas:SENSITIVE = BROWSE BROWSE-IPakke:FOCUSED-ROW <> ?.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-IPakke
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.             

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
  ON CLOSE OF THIS-PROCEDURE DO:
  /*    RUN SaveBrowseSettings. */
      RUN SlettTmpChild.
      IF VALID-HANDLE(wParentHandle) THEN
          RUN SlettProg in wParentHandle.
      RUN disable_UI.
      IF VALID-HANDLE(chImage-Sko) THEN
          RELEASE OBJECT chImage-Sko NO-ERROR.
      IF VALID-HANDLE(Image-Sko) THEN
          DELETE OBJECT Image-Sko NO-ERROR.
      ASSIGN Image-Sko   = ?
             chImage-Sko = ?.
  END.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  ASSIGN lPakke   = ArtBas.Pakke.
  RUN enable_UI.
  {lng.i}
  SUBSCRIBE TO "ByttObjekt" IN wParentHandle NO-ERROR.
  RUN ByttObjekt IN THIS-PROCEDURE (ArtBas.ArtikkelNr).
/*   IF ArtBas.Pakke = TRUE THEN DO:                                               */
/*       ASSIGN FI-Pakkenr:SCREEN-VALUE = STRING(ArtBas.Pakkenr)                   */
/*              FI-Pakkenr:HIDDEN = FALSE                                          */
/*              lIKasse           = ArtBas.iKasse.                                 */
/*       FI-PakkeTxt:MOVE-TO-TOP().                                                */
/*       BROWSE BROWSE-Pakkelinje:MOVE-TO-TOP().                                   */
/*       {&OPEN-QUERY-BROWSE-Pakkelinje}                                           */
/*       APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Pakkelinje.                        */
/*       APPLY "ENTRY" TO BROWSE BROWSE-Pakkelinje.                                */
/*       ASSIGN B-ByttArtBas:SENSITIVE = BROWSE BROWSE-Pakkelinje:FOCUSED-ROW <> ? */
/*              BUTTON-TilPakke:HIDDEN = TRUE.                                     */
/*   END.                                                                          */
/*   ELSE DO:                                                                      */
/*       ASSIGN BUTTON-Ny:HIDDEN       = TRUE                                      */
/*              BUTTON-Endre:HIDDEN    = TRUE                                      */
/*              BUTTON-Slett:HIDDEN    = TRUE                                      */
/*              BUTTON-TilPakke:HIDDEN = FALSE.                                    */
/*       FI-IPakkeTxt:MOVE-TO-TOP().                                               */
/*       BROWSE BROWSE-IPakke:MOVE-TO-TOP().                                       */
/*       {&OPEN-QUERY-BROWSE-IPakke}                                               */
/*       APPLY "VALUE-CHANGED" TO BROWSE BROWSE-IPakke.                            */
/*       APPLY "ENTRY" TO BROWSE BROWSE-IPakke.                                    */
/*       ASSIGN B-ByttArtBas:SENSITIVE = BROWSE BROWSE-IPakke:FOCUSED-ROW <> ?.    */
/*   END.                                                                          */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK.

  /*FRAME FRAME-Pakkelinje:SENSITIVE = artbas.sanertdato = ?.*/

/*   DISP {&FIELDS-IN-QUERY-FRAME-Pakkelinje} WITH FRAME FRAME-Pakkelinje. */
  {sww.i}  
  DO WITH FRAME {&FRAME-NAME}:
    IF VALID-HANDLE(hJmfRutine) THEN
        APPLY "CLOSE" TO hJmfRutine.
    ASSIGN dArtikkelNr             = ipArtikkelnr
           lPakke                  = ArtBas.Pakke
           lIKasse                 = ArtBas.iKasse
           FI-Pakkenr:SCREEN-VALUE = STRING(ArtBas.Pakkenr)
           FI-Pakkenr:HIDDEN       = NOT ArtBas.Pakke.
/*            FI-Pakkenr:HIDDEN       = NOT ArtBas.Pakkenr > 0. */

    IF ArtBas.Pakke = TRUE THEN DO:
        ASSIGN BUTTON-Ny:HIDDEN = FALSE
               BUTTON-Endre:HIDDEN = FALSE
               BUTTON-Slett:HIDDEN = FALSE
               BUTTON-TilPakke:HIDDEN = TRUE.

        FI-PakkeTxt:MOVE-TO-TOP().
        BROWSE BROWSE-Pakkelinje:MOVE-TO-TOP().
        CLOSE QUERY BROWSE-IPakke.
        {&OPEN-QUERY-BROWSE-Pakkelinje}
        APPLY "VALUE-CHANGED" TO BROWSE BROWSE-Pakkelinje.
        ASSIGN B-ByttArtBas:SENSITIVE = BROWSE BROWSE-Pakkelinje:FOCUSED-ROW <> ?
               B-Jamfor:SENSITIVE = B-ByttArtBas:SENSITIVE.
    END.
    ELSE DO:
        ASSIGN BUTTON-Ny:HIDDEN = TRUE
               BUTTON-Endre:HIDDEN = TRUE
               BUTTON-Slett:HIDDEN = TRUE
               BUTTON-TilPakke:HIDDEN = FALSE
               BUTTON-TilPakke:SENSITIVE = ArtBas.OPris = FALSE.
        FI-IPakkeTxt:MOVE-TO-TOP().
        BROWSE BROWSE-IPakke:MOVE-TO-TOP().
        CLOSE QUERY BROWSE-Pakkelinje.
        {&OPEN-QUERY-BROWSE-IPakke}
        APPLY "VALUE-CHANGED" TO BROWSE BROWSE-IPakke.
        ASSIGN B-ByttArtBas:SENSITIVE = BROWSE BROWSE-IPakke:FOCUSED-ROW <> ?
               B-Jamfor:SENSITIVE = B-ByttArtBas:SENSITIVE.
    END.
  END.
  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-Pakkelinje.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    Image-Sko:NAME = "Image-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-Pakkelinje.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  HIDE FRAME FRAME-Pakkelinje.
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
  RUN control_load.
  DISPLAY FI-IPakkeTxt FI-PakkeTxt 
      WITH FRAME FRAME-Pakkelinje.
  ENABLE RECT-57 BROWSE-IPakke BROWSE-Pakkelinje BUTTON-Ny BUTTON-Endre 
         BUTTON-Slett BUTTON-TilPakke B-ByttArtBas B-Jamfor FI-IPakkeTxt 
         FI-PakkeTxt 
      WITH FRAME FRAME-Pakkelinje.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Pakkelinje}
  VIEW C-Win.
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
   IF FRAME FRAME-PakkeLinje:MOVE-TO-TOP() THEN.
   IF lPakke = TRUE THEN
       APPLY "ENTRY" TO BROWSE BROWSE-Pakkelinje.
   ELSE
       APPLY "ENTRY" TO BROWSE BROWSE-IPakke.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryPakkelinje C-Win 
PROCEDURE OpenQueryPakkelinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     IF lPakke = TRUE THEN
         {&OPEN-QUERY-BROWSE-Pakkelinje}
     ELSE
         {&OPEN-QUERY-BROWSE-IPakke}
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
    IF lPakke = TRUE THEN
        APPLY "ENTRY" TO BROWSE BROWSE-Pakkelinje.
    ELSE
        APPLY "ENTRY" TO BROWSE BROWSE-IPakke.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Win 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     Visar bild på pakkemedlem
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bArtBas FOR ArtBas.
  FIND bArtBas WHERE bArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-LOCK NO-ERROR.
  if not available bArtBas then
    return.
  {visbilde.i
   &BldOcx = chImage-Sko
   &BildNr = "bArtBas.BildNr"
  }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde2 C-Win 
PROCEDURE VisBilde2 :
/*------------------------------------------------------------------------------
  Purpose:     Visar bild på utifrån pakkeidentitet
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bArtBas FOR ArtBas.
  FIND bArtBas WHERE bArtBas.ArtikkelNr = PakkeLinje.ArtikkelNr NO-LOCK NO-ERROR.
  if not available bArtBas then
    return.
  {visbilde.i
   &BldOcx = chImage-Sko
   &BildNr = "bArtBas.BildNr"
  }

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ArtBasBeskr C-Win 
FUNCTION ArtBasBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN (IF ArtBas.Beskr <> "" THEN ArtBas.Beskr
                             ELSE ArtBas.BongTekst) ELSE "Artikkel finnes ikke".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuLopNr C-Win 
FUNCTION fuLopNr RETURNS INTEGER
  ( INPUT lArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR iLopNr AS INT NO-UNDO.

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
      iLopNr = ArtBas.LopNr.
  ELSE
      iLopNr = ?.

  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OKreg C-Win 
FUNCTION OKreg RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF lIKasse THEN
      MESSAGE "Vare i kasse er satt på pakken." SKIP
              "For endring av pakkens innhold, ta bort markering."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN lIKasse = FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PakkeBeskr C-Win 
FUNCTION PakkeBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = PakkeLinje.ArtikkelNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN (IF ArtBas.Beskr <> "" THEN ArtBas.Beskr
                             ELSE ArtBas.BongTekst) ELSE "Artikkel finnes ikke".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

