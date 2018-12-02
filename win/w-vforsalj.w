&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-ButikKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-ButikKort 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR wInputRecid AS RECID NO-UNDO.
  DEF VAR wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
  ASSIGN 
    wModus = "ENDRE". /* Default */
 FIND FIRST Forsalj NO-LOCK NO-ERROR.
  IF AVAILABLE Forsalj THEN
    ASSIGN wInputRecid = recid(Forsalj).
&ELSE
  DEF INPUT PARAMETER wInputRecid AS RECID NO-UNDO.
  DEF INPUT PARAMETER wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR wNyVg              AS INT    NO-UNDO.
DEF VAR wOk                AS LOG    NO-UNDO.
DEF VAR wOldRecid          AS RECID  NO-UNDO.
DEF VAR hHandle            AS HANDLE NO-UNDO.
DEF VAR hLabel             AS HANDLE NO-UNDO.
DEF VAR wBildNr            AS INT    NO-UNDO.
DEF VAR wFilNavn           AS CHAR   NO-UNDO.
DEF VAR wFilExt            AS CHAR   NO-UNDO.
DEF VAR wButikk            AS CHAR FORMAT "x(6)" NO-UNDO.
DEF VAR wVisLager          AS RECID  NO-UNDO.
DEF VAR wSjekkStreng       AS CHAR   NO-UNDO.
DEF VAR wDirektePrisOppdat AS LOG    NO-UNDO.
DEF VAR wBestHodeRecid     AS RECID  NO-UNDO.
DEF VAR wRS-Vis            AS RECID  NO-UNDO.
DEF VAR wDataObjekt        AS CHAR   NO-UNDO.
DEF VAR wPerId             AS CHAR   NO-UNDO.
DEF VAR wStTypeId          AS CHAR   NO-UNDO.
DEF VAR  wCl               AS INT    NO-UNDO.
DEF VAR wPerLinTxt         AS CHAR   NO-UNDO.
DEF VAR wSubWin1            AS HANDLE NO-UNDO.
DEF VAR wLapTop            AS LOG    NO-UNDO.
DEF VAR wBekreft           AS LOG    NO-UNDO.
DEF VAR wFeil              AS LOG    NO-UNDO.
DEF VAR cTekst             AS CHAR   NO-UNDO.
DEF VAR iBrukerType        AS INT NO-UNDO.
DEF VAR iButikkNr          AS INT NO-UNDO.

/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab1Side        AS COM-HANDLE NO-UNDO.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.
DEFINE VAR wTabTekst         AS CHAR INIT "Vedlikehold" NO-UNDO.
DEFINE VAR wTabHnd           AS HANDLE EXTENT 1 NO-UNDO.
DEFINE VAR wBrowseHandle     AS HANDLE NO-UNDO.
/* Buffere */
DEF BUFFER bForsalj FOR Forsalj.
    DEF BUFFER bButikkForsalj FOR ButikkForsalj.
DEF TEMP-TABLE tmpChild 
  FIELD wChild AS HANDLE.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME B-butikkfsg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES butikkforsalj Butiker

/* Definitions for BROWSE B-butikkfsg                                   */
&Scoped-define FIELDS-IN-QUERY-B-butikkfsg butikkforsalj.Butik ~
Butiker.KortNavn Butiker.ButNamn butikkforsalj.KassererId ~
butikkforsalj.BrukerID butikkforsalj.EDato VisTid(butikkforsalj.ETid) ~
butikkforsalj.RegistrertAv butikkforsalj.RegistrertDato ~
VisTid(butikkforsalj.RegistrertTid) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-butikkfsg 
&Scoped-define QUERY-STRING-B-butikkfsg FOR EACH butikkforsalj ~
      WHERE butikkforsalj.ForsNr = Forsalj.ForsNr NO-LOCK, ~
      EACH Butiker OF butikkforsalj NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-butikkfsg OPEN QUERY B-butikkfsg FOR EACH butikkforsalj ~
      WHERE butikkforsalj.ForsNr = Forsalj.ForsNr NO-LOCK, ~
      EACH Butiker OF butikkforsalj NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-butikkfsg butikkforsalj Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-B-butikkfsg butikkforsalj
&Scoped-define SECOND-TABLE-IN-QUERY-B-butikkfsg Butiker


/* Definitions for FRAME FRAME-1                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-1 ~
    ~{&OPEN-QUERY-B-butikkfsg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 RECT-28 CB-Sort BUTTON-Angre ~
BUTTON-Kopier BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev BUTTON-Slett ~
Btn_Help BUTTON-Ok 
&Scoped-Define DISPLAYED-OBJECTS CB-Sort FILL-IN-ForsNr FILL-IN-FoNamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Modifierad C-ButikKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VisTid C-ButikKort 
FUNCTION VisTid RETURNS CHARACTER
  ( INPUT iTid AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-ButikKort AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon\e-undo":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON BUTTON-Kopier 
     IMAGE-UP FILE "icon\e-copy":U NO-FOCUS FLAT-BUTTON
     LABEL "&Kopiera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Kopier post  (Alt-K)".

DEFINE BUTTON BUTTON-Lagre 
     IMAGE-UP FILE "icon\e-save":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon\e-pilned":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon\e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon\e-pilopp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon\e-del":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON BUTTON-SokSelger-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL " 1: Vg/LpNr" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS " 1: Selger/Nr"," 2: Selger/Namn" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Velg sorteringsordning som skal gjelde for blaing med pil Opp/Ned." NO-UNDO.

DEFINE VARIABLE FILL-IN-FoNamn AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FILL-IN-ForsNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Kasserer" 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 203 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 203 BY .1.

DEFINE BUTTON B-BytKasserId 
     LABEL "Endre" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Koble 
     LABEL "Koble..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Sokeknapp 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Sokeknapp-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokSelger 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-PostSted1 AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Kasserer i butikk" 
      VIEW-AS TEXT 
     SIZE 48 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-EndretInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Endret informasjon" 
      VIEW-AS TEXT 
     SIZE 130 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 132 BY 20.71.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 20.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-butikkfsg FOR 
      butikkforsalj, 
      Butiker SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-butikkfsg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-butikkfsg C-ButikKort _STRUCTURED
  QUERY B-butikkfsg NO-LOCK DISPLAY
      butikkforsalj.Butik FORMAT ">>>>>9":U WIDTH 13.2
      Butiker.KortNavn FORMAT "X(8)":U
      Butiker.ButNamn FORMAT "x(20)":U WIDTH 19.8
      butikkforsalj.KassererId FORMAT ">>9":U WIDTH 8.8
      butikkforsalj.BrukerID FORMAT "X(10)":U
      butikkforsalj.EDato FORMAT "99/99/9999":U
      VisTid(butikkforsalj.ETid) COLUMN-LABEL "ETid"
      butikkforsalj.RegistrertAv FORMAT "X(10)":U
      butikkforsalj.RegistrertDato FORMAT "99/99/9999":U
      VisTid(butikkforsalj.RegistrertTid) COLUMN-LABEL "Regtid"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 58 BY 17.14 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokSelger-2 AT ROW 3 COL 34.6
     CB-Sort AT ROW 1.33 COL 34.2 COLON-ALIGNED NO-LABEL
     FILL-IN-ForsNr AT ROW 3 COL 22 COLON-ALIGNED HELP
          "Selgers nummer"
     FILL-IN-FoNamn AT ROW 3 COL 37.2 COLON-ALIGNED HELP
          "Navn" NO-LABEL
     BUTTON-Angre AT ROW 1.33 COL 20.8 NO-TAB-STOP 
     BUTTON-Kopier AT ROW 1.33 COL 11.4 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 1.33 COL 6.8 NO-TAB-STOP 
     BUTTON-Next AT ROW 1.33 COL 30.6 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.33 COL 2 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 26 NO-TAB-STOP 
     BUTTON-Slett AT ROW 1.33 COL 16 NO-TAB-STOP 
     Btn_Help AT ROW 1.33 COL 194.4 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 199.4 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204 BY 30.29.

DEFINE FRAME FRAME-1
     Forsalj.ForsNr AT ROW 1.62 COL 24.6 COLON-ALIGNED
          LABEL "Kasserer" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Forsalj.FoForNavn AT ROW 2.62 COL 24.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 54.4 BY 1
     BUTTON-Sokeknapp-2 AT ROW 15.76 COL 121
     Forsalj.FoNamn AT ROW 3.62 COL 24.6 COLON-ALIGNED
          LABEL "Etternavn" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 54.4 BY 1
     Forsalj.navnikasse AT ROW 4.62 COL 24.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.passord AT ROW 5.62 COL 24.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     Forsalj.FoPersNr AT ROW 6.62 COL 24.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.FodtDato AT ROW 7.62 COL 24.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.FoAdr AT ROW 8.62 COL 24.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 54.2 BY 1
     Forsalj.FoAdr2 AT ROW 9.62 COL 24.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 54.2 BY 1
     Forsalj.FoPadr AT ROW 10.62 COL 24.8 COLON-ALIGNED
          LABEL "Postboks"
          VIEW-AS FILL-IN 
          SIZE 54.2 BY 1
     Forsalj.FoPoNr AT ROW 11.62 COL 24.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.FoTel AT ROW 12.62 COL 24.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.Rabatt AT ROW 14.76 COL 25 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.Prisendring AT ROW 15.71 COL 25 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.Retur AT ROW 16.67 COL 25 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.slettTidligere AT ROW 17.62 COL 25 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.SlettBong AT ROW 18.57 COL 25 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.SletteForste AT ROW 19.52 COL 25 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.ForsaljAktiv AT ROW 1.71 COL 43
          VIEW-AS TOGGLE-BOX
          SIZE 28 BY .81
     FI-PostSted1 AT ROW 11.62 COL 47 COLON-ALIGNED NO-LABEL
     Forsalj.FoAnstNr AT ROW 1.62 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.AnsattDato AT ROW 2.57 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.SluttetDato AT ROW 3.57 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.ArbeidsProsent AT ROW 4.57 COL 101 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.Jobbtittel AT ROW 5.57 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 201 BY 21.19.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     Forsalj.LonnProfil AT ROW 6.52 COL 101 COLON-ALIGNED FORMAT "X(25)"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1","Item 1"
          DROP-DOWN-LIST
          SIZE 30 BY 1
     Forsalj.TimeLonn AT ROW 7.52 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.FastLonn AT ROW 8.52 COL 101 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.BrukerId2 AT ROW 14.76 COL 101 COLON-ALIGNED
          LABEL "Brukerid nettbutikk"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Forsalj.ButikkNr AT ROW 15.76 COL 101 COLON-ALIGNED
          LABEL "Butikktilhørighet"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     B-butikkfsg AT ROW 2.91 COL 139
     B-BytKasserId AT ROW 20.52 COL 166
     B-Koble AT ROW 20.52 COL 182.8
     BUTTON-SokSelger AT ROW 1.62 COL 37
     BUTTON-Sokeknapp AT ROW 11.62 COL 44.6
     FILL-IN-EndretInfo AT ROW 21 COL 3 NO-LABEL
     FILL-IN-1 AT ROW 1.81 COL 146.8 COLON-ALIGNED NO-LABEL
     RECT-46 AT ROW 1.24 COL 2
     RECT-47 AT ROW 1.24 COL 134
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 201 BY 21.19.

DEFINE FRAME FRAME-2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.


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
  CREATE WINDOW C-ButikKort ASSIGN
         HIDDEN             = YES
         TITLE              = "Kasserer"
         HEIGHT             = 30.29
         WIDTH              = 204
         MAX-HEIGHT         = 30.29
         MAX-WIDTH          = 204
         VIRTUAL-HEIGHT     = 30.29
         VIRTUAL-WIDTH      = 204
         MAX-BUTTON         = no
         RESIZE             = no
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-ButikKort
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-1:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-2:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SokSelger-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FoNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ForsNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-1
   L-To-R,COLUMNS                                                       */
/* BROWSE-TAB B-butikkfsg ButikkNr FRAME-1 */
/* SETTINGS FOR FILL-IN Forsalj.ArbeidsProsent IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Forsalj.BrukerId2 IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Forsalj.ButikkNr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FI-PostSted1 IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-EndretInfo IN FRAME FRAME-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Forsalj.FoNamn IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Forsalj.FoPadr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Forsalj.ForsNr IN FRAME FRAME-1
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR COMBO-BOX Forsalj.LonnProfil IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FRAME FRAME-2
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ButikKort)
THEN C-ButikKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-butikkfsg
/* Query rebuild information for BROWSE B-butikkfsg
     _TblList          = "skotex.butikkforsalj,skotex.Butiker OF skotex.butikkforsalj"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.butikkforsalj.ForsNr = Forsalj.ForsNr"
     _FldNameList[1]   > skotex.butikkforsalj.Butik
"butikkforsalj.Butik" ? ? "integer" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = skotex.Butiker.KortNavn
     _FldNameList[3]   > skotex.Butiker.ButNamn
"Butiker.ButNamn" ? ? "character" ? ? ? ? ? ? no ? no no "19.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > skotex.butikkforsalj.KassererId
"butikkforsalj.KassererId" ? ? "integer" ? ? ? ? ? ? no ? no no "8.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = skotex.butikkforsalj.BrukerID
     _FldNameList[6]   = skotex.butikkforsalj.EDato
     _FldNameList[7]   > "_<CALC>"
"VisTid(butikkforsalj.ETid)" "ETid" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = skotex.butikkforsalj.RegistrertAv
     _FldNameList[9]   = skotex.butikkforsalj.RegistrertDato
     _FldNameList[10]   > "_<CALC>"
"VisTid(butikkforsalj.RegistrertTid)" "Regtid" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE B-butikkfsg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-1
/* Query rebuild information for FRAME FRAME-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-2
/* Query rebuild information for FRAME FRAME-2
     _Query            is NOT OPENED
*/  /* FRAME FRAME-2 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 8.14
       COLUMN          = 1
       HEIGHT          = 23.1
       WIDTH           = 204
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(FILL-IN-FoNamn:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-ButikKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ButikKort C-ButikKort
ON END-ERROR OF C-ButikKort /* Kasserer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ButikKort C-ButikKort
ON WINDOW-CLOSE OF C-ButikKort /* Kasserer */
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
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME B-BytKasserId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BytKasserId C-ButikKort
ON CHOOSE OF B-BytKasserId IN FRAME FRAME-1 /* Endre */
DO:
  IF BROWSE B-butikkfsg:FOCUSED-ROW <> ? THEN DO:
      RUN d-BytKasserId (ROWID(ButikkForsalj)).
      BROWSE B-butikkfsg:REFRESH().
      APPLY "ENTRY" TO B-butikkfsg.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Koble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koble C-ButikKort
ON CHOOSE OF B-Koble IN FRAME FRAME-1 /* Koble... */
DO:
  DEF VAR cAktButListe   AS CHAR NO-UNDO.
  DEF VAR cIOButListe    AS CHAR NO-UNDO.

  FOR EACH butikkforsalj WHERE butikkforsalj.ForsNr = Forsalj.ForsNr NO-LOCK.
      ASSIGN cAktButListe = cAktButListe + (IF cAktButListe = "" THEN "" ELSE ",")  +
          STRING(butikkforsalj.Butik).
  END.
  ASSIGN cIOButListe = cAktButListe.
  RUN d-tagbutiker.w (INPUT-OUTPUT cIOButListe).
  /*
  IF iButikkNr = 0 THEN
      RUN d-tagbutiker.w (INPUT-OUTPUT cIOButListe).
  ELSE                             
      cIOButListe = STRING(iButikkNr).
  */
  IF cIOButListe <> cAktButListe THEN DO:
      RUN FixKobling (cIOButListe).
  END.
  {&OPEN-QUERY-B-butikkfsg}
  APPLY "ENTRY" TO B-butikkfsg.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-ButikKort
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  RUN WinHlp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-ButikKort
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  IF wModus = "NY" THEN
     ASSIGN Forsalj.ForsNr:SENSITIVE IN FRAME FRAME-1 = NO.
            wModus = "ENDRE".
  RUN VisPost.
  RUN BUTTONEnaDis.
  RUN KassererENaDis (2).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier C-ButikKort
ON CHOOSE OF BUTTON-Kopier IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
  DO WITH FRAME FRAME-1:
    ASSIGN 
      wModus   = "NY"
      Forsalj.ForsNr:SENSITIVE = TRUE
      Forsalj.ForsNr:SCREEN-VALUE = "0".
    RUN BUTTONEnaDis.
    RUN KassererENaDis (1).

    APPLY "ENTRY":U TO Forsalj.ForsNr IN FRAME FRAME-1.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-ButikKort
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  RUN LagrePost (0).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  RUN BUTTONEnaDis.
  RUN KassererENaDis (2).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-ButikKort
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  RUN Bytpost("Next").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-ButikKort
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  CLOSE QUERY B-butikkfsg.
  RUN KassererENaDis (1).
  RUN BUTTON-Ny.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-ButikKort
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
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
  IF Modifierad() THEN DO: 
    RUN LagrePost (0).
    IF RETURN-VALUE <> "OK" THEN
     DO:
       READKEY PAUSE 0.
       RETURN NO-APPLY.
    END.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
  RETURN wModus + "," + string(wInputRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-ButikKort
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  RUN Bytpost("Prev").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-ButikKort
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  RUN BUTTON-Slett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME BUTTON-Sokeknapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp C-ButikKort
ON CHOOSE OF BUTTON-Sokeknapp IN FRAME FRAME-1 /* ... */
OR "F10" OF Forsalj.FoPoNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    Forsalj.FoPoNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        Forsalj.FoPoNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        FI-PostSted1:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sokeknapp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp-2 C-ButikKort
ON CHOOSE OF BUTTON-Sokeknapp-2 IN FRAME FRAME-1 /* ... */
OR "F10" OF Forsalj.ButikkNr DO:
  DEF VAR cButikerFieldList    AS CHAR NO-UNDO.
  DEF VAR bOk                  AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "Butiker"
                     + ";Butik"
                     + ";ButNamn"
                     ,
                   "WHERE true"
                    ,""
                    ,"Butik,ButNamn",
                    OUTPUT cButikerFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cButikerFieldList NE "" THEN DO:
    ASSIGN 
       Forsalj.ButikkNr:SCREEN-VALUE      = ENTRY(1,cButikerFieldList,"|")
       .
    APPLY "any-printable" TO Forsalj.ButikkNr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSelger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSelger C-ButikKort
ON CHOOSE OF BUTTON-SokSelger IN FRAME FRAME-1 /* ... */
DO:
  DEF VAR wForsNR LIKE Forsalj.ForsNr NO-UNDO.
      IF wModus = "ENDRE" AND Modifierad() THEN DO:
        MESSAGE "Vill du lagra posten?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO
        TITLE "" UPDATE wChoice AS LOGICAL.
        IF wChoice = TRUE THEN
           RUN LagrePost(0).
           IF RETURN-VALUE = "AVBRYT" THEN DO:
               RETURN NO-APPLY.
        END.
        RUN VisPost.
      END.
      ASSIGN wForsnr = IF wModus = "ENDRE" THEN Forsalj.ForsNr ELSE ?.
      RUN d-bforsalj.w (INPUT-OUTPUT wForsnr).
      IF wModus = "NY" THEN DO:
         APPLY "ENTRY" TO Forsalj.ForsNr IN FRAME FRAME-1.
         RETURN NO-APPLY.
      END.
      IF RETURN-VALUE <> "AVBRYT" THEN DO:
        FIND Forsalj WHERE Forsalj.ForsNr = wForsnr NO-LOCK.
        ASSIGN wInputRecid = RECID(Forsalj).
        RUN VisPost.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokSelger-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSelger-2 C-ButikKort
ON CHOOSE OF BUTTON-SokSelger-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
    APPLY "CHOOSE" TO BUTTON-SokSelger IN FRAME FRAME-1.
    IF wAktivFlip = 2 AND RETURN-VALUE <> "AVBRYT" THEN DO:
        RUN ByttObjekt IN wSubWin1 (string(Forsalj.ForsNr,"9999999999999")).  
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Forsalj.FoPoNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Forsalj.FoPoNr C-ButikKort
ON LEAVE OF Forsalj.FoPoNr IN FRAME FRAME-1 /* Postnummer */
OR "RETURN":U OF Forsalj.FoPoNr OR "TAB":U OF Forsalj.FoPoNr
DO:
  DO WITH FRAME frame-1:
    FIND Post NO-LOCK WHERE Post.PostNr = INPUT Forsalj.FoPoNr NO-ERROR.
    ASSIGN FI-PostSted1:SCREEN-VALUE = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Forsalj.ForsaljAktiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Forsalj.ForsaljAktiv C-ButikKort
ON VALUE-CHANGED OF Forsalj.ForsaljAktiv IN FRAME FRAME-1 /* Aktiv */
DO:
  IF SELF:CHECKED AND INPUT Forsalj.Navnikasse = "" THEN DO:
      MESSAGE "Registrer navn i kasse"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN SELF:CHECKED = FALSE.
      APPLY "ENTRY" TO Forsalj.navnikasse.
      RETURN NO-APPLY.
  END.
/*   IF SELF:CHECKED AND INPUT Forsalj.FodtDato = ? THEN DO: */
/*       MESSAGE "Registrer fødselsdato"                     */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.              */
/*       ASSIGN SELF:CHECKED = FALSE.                        */
/*       APPLY "ENTRY" TO Forsalj.FodtDato.                  */
/*       RETURN NO-APPLY.                                    */
/*   END.                                                    */
  IF NOT SELF:CHECKED AND CAN-FIND(FIRST ButikkForsalj 
                  WHERE ButikkForsalj.Forsnr = INPUT Forsalj.Forsnr) THEN DO:
      MESSAGE "Kasserer er koblet til butikk."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN SELF:CHECKED = TRUE.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-ButikKort OCX.Click
PROCEDURE TabStrip.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN DO:
  IF wModus = "NY" THEN DO:
      MESSAGE "Posten må lagres först." VIEW-AS ALERT-BOX INFORMATION TITLE "Feil".
              ASSIGN wFeil = TRUE
                     chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER)
                     chTabStrip:SelectedItem = chTab1Side.
  END.
  ELSE IF wAktivFlip = 1 AND Modifierad() THEN DO:
      MESSAGE "Vill du lagra posten?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO
      TITLE "" UPDATE wChoice AS LOGICAL.
      IF wChoice = TRUE THEN
          RUN LagrePost(0).
          IF RETURN-VALUE = "AVBRYT" THEN DO:
              ASSIGN wFeil = TRUE
                     chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER)
                     chTabStrip:SelectedItem = chTab1Side.
              RETURN NO-APPLY.
      END.
      ELSE DO:
          IF wModus = "NY" THEN
              ASSIGN wModus = "ENDRE".
      END.
      RUN VisPost.
  END.
  IF NOT wFeil THEN DO:
    ASSIGN wAktivFlip = INT(chTabStrip:SelectedItem:Index).
    RUN ByttFrame. /* Bytter tab */
    RUN ButtonEnaDis.
  END.
  IF wAktivFlip = 1 AND NOT wFeil THEN
      APPLY "ENTRY" TO Forsalj.FoNamn IN FRAME FRAME-1.
  ELSE IF wAktivFlip <> 1 THEN
      APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
 END.
 ELSE DO:
    IF NOT wFeil THEN DO:
     IF wAktivFlip = 1 THEN
            APPLY "ENTRY" TO Forsalj.FoNamn IN FRAME FRAME-1.
     ELSE
       APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
    END.
    ELSE
        wFeil = FALSE.
 END.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B-butikkfsg
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-ButikKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Butikkkort"
  &PostIClose    = "if valid-handle(wSubWin1) then
                       do:
                         /* RUN SaveBrowseSettings in wHistorikk. */
                         delete procedure wSubWin1 no-error.
                       end.
                     RUN DelTmpChild.
                     IF VALID-HANDLE(chTabStrip) THEN
                         RELEASE OBJECT chTabStrip NO-ERROR.
                     IF VALID-HANDLE(chTabs) THEN
                         RELEASE OBJECT chTabs NO-ERROR.
                     IF VALID-HANDLE(chTab) THEN
                         RELEASE OBJECT chTab NO-ERROR.
                     IF VALID-HANDLE(chTab1Side) THEN
                         RELEASE OBJECT chTab1Side NO-ERROR.
                     IF VALID-HANDLE(TabStrip) THEN
                         DELETE OBJECT TabStrip NO-ERROR.
                     ASSIGN TabStrip    = ?
                            chTabStrip  = ?
                            chTabs      = ?
                            chTab       = ?
                            chTab1Side  = ?.
                         "                     
  &PostDisable_ui = "wModus = 'AVBRYT'." 
}


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
ON ALT-N OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Ny IN FRAME DEFAULT-FRAME.
  END.
ON ALT-L OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Lagre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-K OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Kopier IN FRAME DEFAULT-FRAME.
  END.
ON ALT-D OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Slett IN FRAME DEFAULT-FRAME.
  END.
ON ALT-A OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-angre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-CURSOR-UP OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Prev IN FRAME DEFAULT-FRAME.
  END.
ON ALT-CURSOR-DOWN OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Next IN FRAME DEFAULT-FRAME.
  END.

ON "CTRL-TAB":U ANYWHERE
  DO:
   chTab1Side = chTabs:Item ((IF wAktivFlip = NUM-ENTRIES(wTabTekst)
                      THEN 1 ELSE wAktivFlip + 1) BY-VARIANT-POINTER).
   chTabStrip:SelectedItem = chTab1Side.
    RETURN NO-APPLY.
  END.

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
IF valid-handle(wLibHandle) THEN
  RUN SjekkLapTop IN wLibHandle (OUTPUT wLapTop).

/* Setter sentrallager butikk */
{syspara.i 5 1 1 wCl INT}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF wInputRecid = ? AND wModus = "ENDRE" THEN DO:
      MESSAGE "Recid = ? och wModus = 'ENDRE' --> FEL !!" VIEW-AS ALERT-BOX ERROR.
      RETURN "AVBRYT".
  END.

  FIND Bruker NO-LOCK WHERE
      Bruker.BrukerId = USERID("SkoTex") NO-ERROR.
  IF AVAILABLE Bruker THEN
  DO:
      ASSIGN
          iBrukerType = Bruker.BrukerType
          iButikkNr   = Bruker.Butik
          .
  END.

  RUN InitCombos.
  RUN enable_UI.
  {lng.i} /* Oversettelse */
  
  RUN InitCB.
  ASSIGN {&WINDOW-NAME}:HIDDEN = NO
         CB-Sort:SCREEN-VALUE = ENTRY(1,CB-Sort:LIST-ITEMS)
         wTabHnd[1] = FRAME FRAME-1:HANDLE.
/*          wTabHnd[2] = FRAME FRAME-2:HANDLE. */

  RUN ByttFrame. /* Legger opp f›rste fane. */

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  ASSIGN chTab1Side = chTabStrip. /* COM-Handl 1. side */
  

  IF wModus <> "NY" THEN DO:
      FIND Forsalj WHERE recid(Forsalj) = wInputRecid NO-LOCK NO-ERROR.
      RUN VisPost.
  END.
  RUN ByttFrame. /* Legger opp f›rste fane. */ 

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  ASSIGN chTab1Side = chTabStrip. /* COM-Handl 1. side */
  
  IF wModus = "NY" THEN 
    DO:
      APPLY "CHOOSE":U TO BUTTON-Ny.
    END.
  ELSE IF wModus = "SLETT" THEN DO:
     APPLY "CHOOSE" TO BUTTON-Slett.
     RETURN RETURN-VALUE.
  END.

  IF iBrukertype = 2 THEN
      ASSIGN
        Forsalj.ButikkNr:SENSITIVE = FALSE
        BUTTON-Sokeknapp-2:SENSITIVE = FALSE
        .
  /*  */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  IF wModus <> "NY" THEN
    FIND Forsalj NO-LOCK WHERE 
      recid(Forsalj) = wInputRecid NO-ERROR.
      
  /* Retur verdi */  
  IF AVAILABLE Forsalj AND wModus <> "NY" THEN
    RETURN string(recid(Forsalj)).
  ELSE 
    RETURN "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Ny C-ButikKort 
PROCEDURE BUTTON-Ny :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSort AS INT NO-UNDO.
  DEFINE VARIABLE rForsalj AS ROWID      NO-UNDO.
  DO WITH FRAME FRAME-1:
     IF Modifierad() THEN DO:
         RUN LagrePost(0).
         IF RETURN-VALUE = "AVBRYT" THEN
             RETURN NO-APPLY.
     END.
     CLEAR FRAME FRAME-1.
     DO TRANSACTION:
         IF AVAIL Forsalj THEN
             ASSIGN rForsalj = ROWID(Forsalj).
         CREATE Forsalj.
         /*
         ASSIGN
           Forsalj.RAbatt         = 2
           Forsalj.Prisendring    = 2
           Forsalj.Retur          = 2
           Forsalj.slettTidligere = 2
           Forsalj.SlettBong      = 2
           Forsalj.SletteForste   = 2
           .
           */
         DISPLAY Forsalj.ForsNr
                 Forsalj.FoNamn
                 Forsalj.ForsaljAktiv
                 Forsalj.FoAnstNr
                 Forsalj.Passord
                 Forsalj.FoPersNr
                 Forsalj.FoAdr
                 Forsalj.FoPadr
                 Forsalj.FoPoNr
                 Forsalj.FoTel
                 Forsalj.navnikasse
                 Forsalj.FodtDato
                 Forsalj.Rabatt
                 Forsalj.Prisendring
                 Forsalj.Retur
                 Forsalj.slettTidligere
                 Forsalj.SlettBong
                 Forsalj.SletteForste 
                 Forsalj.BrukerId2 
           WITH FRAME FRAME-1.
         DELETE Forsalj.
         IF rForsalj <> ? THEN
             FIND Forsalj WHERE ROWID(Forsalj) = rForsalj NO-LOCK NO-ERROR.
     END.
     ASSIGN wModus = "NY"
            /*FILL-IN-ForsNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""*/ 
            FILL-IN-FoNamn:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
            /*Forsalj.ForsNr:SENSITIVE = TRUE*/
            /*Forsalj.ForsNr:SCREEN-VALUE = "0"*/.

    IF iBrukerType = 2 THEN
        Forsalj.ButikkNr:SCREEN-VALUE = STRING(iButikkNr).
    RUN BUTTONEnaDis.
    APPLY "ENTRY" TO Forsalj.ForsNr. 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Slett C-ButikKort 
PROCEDURE BUTTON-Slett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wOk AS LOG FORMAT "Ja/Nei" NO-UNDO.
  DEF VAR wNesteRecid AS RECID NO-UNDO.
  
  IF NOT AVAILABLE Forsalj THEN
      RETURN.

  /* Kassereren ka slettes hvis han ikke er koblet mot butikk. */
  IF CAN-FIND(FIRST ButikkForsalj WHERE
              ButikkForsalj.ForsNr = Forsalj.ForsNr) THEN
  DO:
      MESSAGE "Kassereren er koblet til en eller flere butikker." SKIP
              "Kasserer som er koblet til butikk kan ikke slettes. Koblingene må tas bort manuelt først."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  
  FIND bForsalj NO-LOCK WHERE
    recid(bForsalj) = recid(Forsalj) NO-ERROR.
  FIND NEXT bForsalj NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bForsalj THEN
    FIND FIRST bForsalj.
  IF AVAILABLE bForsalj THEN
    ASSIGN
      wNesteRecid = recid(bForsalj).

  ASSIGN wOk = FALSE.
  MESSAGE "Skal kassereren slettes?" VIEW-AS ALERT-BOX 
    QUESTION BUTTONS YES-NO
    TITLE "Bekreftelse"
    UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY "AVBRYT".  
  ELSE DO:
    FOR EACH stLinje EXCLUSIVE where
             StLinje.StType = "SELGERSTAT" and
             StLinje.DataObjekt = string(Forsalj.ForsNr,"9999999999999"):
       DELETE stLinje.
    END.
    FIND CURRENT Forsalj EXCLUSIVE.
    DELETE Forsalj.
    IF wModus = "SLETT" THEN
        RETURN "OK".
    ASSIGN 
          wInputRecid   = wNesteRecid
          wModus         = "ENDRE".
    FIND Forsalj NO-LOCK WHERE RECID(Forsalj) = wInputRecid.
    RUN VisPost.
    RUN BUTTONEnaDis.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-ButikKort 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:  
  ASSIGN
    BUTTON-Ny:sensitive     = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-Kopier:sensitive = NOT wModus = "Ny" AND wAktivFlip = 1
    BUTTON-Slett:sensitive  = NOT wModus = "Ny"  AND wAktivFlip = 1
    BUTTON-Prev:sensitive   = NOT wModus = "Ny"
    BUTTON-Next:sensitive   = NOT wModus = "Ny"
    BUTTON-Ok:sensitive     = NOT wModus = "Ny"
    BUTTON-Lagre:sensitive  = wAktivFlip = 1
    BUTTON-Angre:sensitive  = wAktivFlip = 1 AND wInputRecid <> ?
    BUTTON-SokSelger-2:sensitive = wAktivFlip <> 1.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bytpost C-ButikKort 
PROCEDURE Bytpost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wRettning AS CHAR NO-UNDO.
  DEF VAR wSort AS INT NO-UNDO.
  ASSIGN
    wSort = int(entry(1,CB-Sort:screen-value IN FRAME DEFAULT-FRAME,":")).
  IF Modifierad() THEN
    RUN LagrePost (0).

  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ELSE DO:
    ASSIGN wModus = "ENDRE".
    
    CASE wSort:
      WHEN 1 THEN
        DO:
          IF wRettning = "Next" THEN DO:
              FIND NEXT Forsalj NO-LOCK USE-INDEX Forsalin NO-ERROR.    
              IF NOT AVAILABLE Forsalj THEN
                 FIND LAST Forsalj NO-LOCK USE-INDEX Forsalin  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV Forsalj NO-LOCK USE-INDEX Forsalin NO-ERROR.    
              IF NOT AVAILABLE Forsalj THEN
                 FIND FIRST Forsalj NO-LOCK USE-INDEX Forsalin  NO-ERROR.
          END.
        END.
      WHEN 2 THEN
        DO:
          IF wRettning = "Next" THEN DO:
              FIND NEXT Forsalj NO-LOCK USE-INDEX FoNamn NO-ERROR.    
              IF NOT AVAILABLE Forsalj THEN
                 FIND LAST Forsalj NO-LOCK USE-INDEX FoNamn  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV Forsalj NO-LOCK USE-INDEX FoNamn NO-ERROR.    
              IF NOT AVAILABLE Forsalj THEN
                 FIND FIRST Forsalj NO-LOCK USE-INDEX FoNamn  NO-ERROR.
          END.
        END.
    END CASE.  
    IF AVAILABLE Forsalj THEN
      DO:
        ASSIGN
          wInputRecid = recid(Forsalj).
        RUN VisPost.
        IF wAktivFlip = 2 THEN
          DO:
            RUN ByttObjekt IN wSubWin1 (string(Forsalj.ForsNr,"9999999999999")).  
          END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrame C-ButikKort 
PROCEDURE ByttFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FRAME DEFAULT-FRAME:MOVE-TO-TOP().
     wTabHnd[wAktivFlip]:MOVE-TO-TOP().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-ButikKort  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-vforsalj.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTabStrip = TabStrip:COM-HANDLE
    UIB_S = chTabStrip:LoadControls( OCXFile, "TabStrip":U)
    TabStrip:NAME = "TabStrip":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-vforsalj.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-ButikKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-ButikKort  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ButikKort)
  THEN DELETE WIDGET C-ButikKort.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-ButikKort  _DEFAULT-ENABLE
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
  DISPLAY CB-Sort FILL-IN-ForsNr FILL-IN-FoNamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  ENABLE RECT-27 RECT-28 CB-Sort BUTTON-Angre BUTTON-Kopier BUTTON-Lagre 
         BUTTON-Next BUTTON-Ny BUTTON-Prev BUTTON-Slett Btn_Help BUTTON-Ok 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-PostSted1 FILL-IN-EndretInfo FILL-IN-1 
      WITH FRAME FRAME-1 IN WINDOW C-ButikKort.
  IF AVAILABLE Forsalj THEN 
    DISPLAY Forsalj.ForsNr Forsalj.FoForNavn Forsalj.FoNamn Forsalj.navnikasse 
          Forsalj.passord Forsalj.FoPersNr Forsalj.FodtDato Forsalj.FoAdr 
          Forsalj.FoAdr2 Forsalj.FoPadr Forsalj.FoPoNr Forsalj.FoTel 
          Forsalj.Rabatt Forsalj.Prisendring Forsalj.Retur 
          Forsalj.slettTidligere Forsalj.SlettBong Forsalj.SletteForste 
          Forsalj.ForsaljAktiv Forsalj.FoAnstNr Forsalj.AnsattDato 
          Forsalj.SluttetDato Forsalj.ArbeidsProsent Forsalj.Jobbtittel 
          Forsalj.LonnProfil Forsalj.TimeLonn Forsalj.FastLonn Forsalj.BrukerId2 
          Forsalj.ButikkNr 
      WITH FRAME FRAME-1 IN WINDOW C-ButikKort.
  ENABLE RECT-46 RECT-47 Forsalj.FoForNavn BUTTON-Sokeknapp-2 Forsalj.FoNamn 
         Forsalj.navnikasse Forsalj.passord Forsalj.FoPersNr Forsalj.FodtDato 
         Forsalj.FoAdr Forsalj.FoAdr2 Forsalj.FoPadr Forsalj.FoPoNr 
         Forsalj.FoTel Forsalj.Rabatt Forsalj.Prisendring Forsalj.Retur 
         Forsalj.slettTidligere Forsalj.SlettBong Forsalj.SletteForste 
         Forsalj.ForsaljAktiv Forsalj.FoAnstNr Forsalj.AnsattDato 
         Forsalj.SluttetDato Forsalj.ArbeidsProsent Forsalj.Jobbtittel 
         Forsalj.LonnProfil Forsalj.TimeLonn Forsalj.FastLonn Forsalj.BrukerId2 
         Forsalj.ButikkNr B-butikkfsg B-BytKasserId B-Koble BUTTON-SokSelger 
         BUTTON-Sokeknapp FILL-IN-EndretInfo FILL-IN-1 
      WITH FRAME FRAME-1 IN WINDOW C-ButikKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-1}
  VIEW FRAME FRAME-2 IN WINDOW C-ButikKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKobling C-ButikKort 
PROCEDURE FixKobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cIOButListe  AS CHARACTER  NO-UNDO.
    DEFINE       VARIABLE  iCount       AS INTEGER    NO-UNDO.

    DEF VAR piKassererId AS INT NO-UNDO.

    FOR EACH butikkforsalj WHERE butikkforsalj.ForsNr = Forsalj.ForsNr AND
                             NOT CAN-DO(cIOButListe,STRING(butikkforsalj.Butik)):
        DELETE butikkforsalj.
    END.
    DO iCount = 1 TO NUM-ENTRIES(cIOButListe):
        IF NOT CAN-FIND(butikkforsalj WHERE butikkforsalj.ForsNr = Forsalj.ForsNr AND
                         butikkforsalj.Butik = INT(ENTRY(iCount,cIOButListe))) THEN 
        DO:
            FIND LAST butikkforsalj NO-LOCK WHERE
                butikkforsalj.Butik = INT(ENTRY(iCount,cIOButListe))
                USE-INDEX ButikkForsKassererId NO-ERROR.
            IF AVAILABLE ButikkForsalj THEN DO:
                 IF NOT ButikkForsalj.KassererId + 1 > 999 THEN
                     piKassererId = ButikkForsalj.KassererId + 1.
                 ELSE DO piKassererId = 1 TO 999:
                     IF NOT CAN-FIND(bButikkForsalj WHERE bButikkForsalj.Butik = butikkforsalj.Butik AND
                                     bbutikkforsalj.ForsNr = butikkforsalj.ForsNr AND
                                     butikkforsalj.KassererId = piKassererId) THEN
                         LEAVE.
                 END.
            END.
            ELSE
                piKassererId = 1.
            CREATE butikkforsalj.
            ASSIGN butikkforsalj.ForsNr = Forsalj.ForsNr
                   butikkforsalj.Butik = INT(ENTRY(iCount,cIOButListe))
                   butikkforsalj.KassererId = piKassererId
                   .
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-ButikKort 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
    
  ASSIGN
      pcTekst = Tx(" 1: Nummer, 2: Navn",1)
      .

  DO WITH FRAME Default-Frame:
      ASSIGN
          CB-Sort:LIST-ITEMS = pcTekst
          CB-Sort:SCREEN-VALUE = ENTRY(1,pcTekst)
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombos C-ButikKort 
PROCEDURE InitCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME FRAME-1:
       ASSIGN Forsalj.Rabatt:LIST-ITEM-PAIRS = "Ja,2,Passord/kort,1,Nei,0"
              Forsalj.Prisendring:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.Retur:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.slettTidligere:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.SlettBong:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.SletteForste:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS.




       cTekst = '<Ikke angitt>|'.
       Forsalj.LonnProfil:DELIMITER = '|'.
       FOR EACH SysPara EXCLUSIVE-LOCK WHERE
         SysPara.SysHId =  50 AND
         SysPara.SysGr  =  21 AND
         SysPara.ParaNr >= 100 AND
         SysPara.ParaNr <= 110 AND
         SysPara.Parameter1 > '' AND
         SysPara.Parameter2 > '':
         cTekst =   cTekst 
                  + (IF cTekst <> '' THEN '|' ELSE '') 
                  + SysPara.Parameter1 + '|' + SysPara.Parameter1.
       END.
       Forsalj.LonnProfil:LIST-ITEM-PAIRS = cTekst.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-Controls C-ButikKort 
PROCEDURE initialize-Controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wCount AS INTE NO-UNDO.
  ASSIGN chTabStrip = chTabStrip:TabStrip
         chTabs     = chTabStrip:Tabs
         chTabs:Item(1):Caption = ENTRY(1,wTabTekst).
  DO wCount = 2 TO NUM-ENTRIES(wTabTekst):
      chTabs:Add(wCount,,ENTRY(wCount,wTabTekst)).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KassererENaDis C-ButikKort 
PROCEDURE KassererENaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER TogleEnaDis AS INT NO-UNDO.

DO WITH FRAME Frame-1:
    CASE TogleEnaDis:
        WHEN 1 THEN ASSIGN
                      B-BytKasserId:SENSITIVE = FALSE
                      B-Koble:SENSITIVE       = FALSE.
        WHEN 2 THEN ASSIGN
                      B-BytKasserId:SENSITIVE = TRUE
                      B-Koble:SENSITIVE       = TRUE.
    END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-ButikKort 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBekreft AS INT NO-UNDO.
  DEF VAR ipStatus AS CHAR INIT "AVBRYT" NO-UNDO.
  DEFINE VARIABLE cButik AS CHARACTER NO-UNDO.
  DEF BUFFER bButiker FOR Butiker.
  DO WITH FRAME FRAME-1:
    IF wModus = "NY" THEN DO:
      /* Sjekker input */
      IF INPUT Forsalj.ForsNr = 0 THEN
        DO:
          MESSAGE "Kasserernummer må være større enn 0"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".
          APPLY "ENTRY":U TO Forsalj.ForsNr.
          RETURN ipStatus.
        END.
        /*
        if input Forsalj.ForsNr > 999 then
          do:
            message "Kasserernummer kan ikke være større enn 999"
            view-as alert-box title "Lagringsfeil".
            apply "ENTRY":U to Forsalj.ForsNr.
            return ipStatus.
          end.
        */
      IF CAN-FIND(Forsalj WHERE
                  Forsalj.ForsNr = int(Forsalj.ForsNr:screen-value)) THEN
        DO:
          MESSAGE "Kasserer finnes allerede med nr:" Forsalj.ForsNr:screen-value
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          RETURN ipStatus.
        END.
    END.
    IF INPUT Forsalj.navnikasse = "" AND INPUT Forsalj.ForsaljAktiv = TRUE THEN DO:
        MESSAGE "Aktiv kasserer må ha navn i kasse registrert"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO Forsalj.navnikasse.
        RETURN ipStatus.
    END.
/*     IF INPUT Forsalj.FodtDato = ? AND INPUT Forsalj.ForsaljAktiv = TRUE THEN DO: */
/*         MESSAGE "Aktiv kasserer må ha fødselsdato registrert"                    */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
/*         APPLY "ENTRY" TO Forsalj.ForsaljAktiv.                                   */
/*         RETURN ipStatus.                                                         */
/*     END.                                                                         */
    IF NOT CAN-FIND(Post WHERE Post.PostNr = INPUT Forsalj.FoPoNr) THEN
    DO:
      MESSAGE "Ukjent postnummer!"
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
          APPLY "ENTRY":U TO Forsalj.FoPoNr.
          RETURN ipStatus.
    END.  
    LAGRE_FORSALJ:
      DO TRANSACTION:
        IF wModus = "NY" THEN
          DO:
            RELEASE Forsalj. /* Slipper gammel post. */
            CREATE Forsalj.
            ASSIGN
               wInputRecid = recid(Forsalj)
               Forsalj.ForsNr.
            /* Om butiker är tillgänglig betyder det att vi har endast 1 butik i systemet */
            /* och då skall vi automatiskt skapa en koppling, SE nedan 'RUN FixKobling' */
            FIND bButiker NO-LOCK NO-ERROR.
            IF AVAIL bButiker THEN
                ASSIGN cButik = STRING(bButiker.butik).
          END.
        ELSE DO:
          FIND CURRENT Forsalj EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF LOCKED Forsalj THEN
            DO:
                MESSAGE "Kassererer oppdateres fra en annen terminal" SKIP
                  "Forsök å lagre en gang til" VIEW-AS ALERT-BOX 
               WARNING TITLE "Lagringsfeil".
           RETURN NO-APPLY ipStatus.
          END.
        END.
        ASSIGN
             wModus       = "ENDRE"
             Forsalj.ForsNr:SENSITIVE = FALSE
             Forsalj.FoNamn
             Forsalj.ForsaljAktiv
             Forsalj.Passord
             Forsalj.FoAnstNr
             Forsalj.FoPersNr
             Forsalj.FoAdr
             Forsalj.FoPadr
             Forsalj.FoPoNr
             Forsalj.FoTel
             Forsalj.navnikasse
             Forsalj.FodtDato
             Forsalj.Rabatt
             Forsalj.Prisendring
             Forsalj.Retur
             Forsalj.slettTidligere
             Forsalj.SlettBong
             Forsalj.SletteForste
             Forsalj.ButikkNr
             Forsalj.Brukerid2

             Forsalj.FoForNavn
             Forsalj.FoAdr2
             Forsalj.AnsattDato
             Forsalj.SluttetDato
             Forsalj.ArbeidsProsent
             Forsalj.JobbTittel
             Forsalj.Lonnprofil
             Forsalj.TimeLonn
             Forsalj.FastLonn
             .
         cButik = STRING(Forsalj.ButikkNr).
         FIND CURRENT Forsalj NO-LOCK.
         IF INT(cButik) <> 0 THEN
             RUN FixKobling (cButik).
         RUN VisPost.
         RUN BUTTONEnaDis.
      END.
  END.
  RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyObjekt C-ButikKort 
PROCEDURE NyObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModified C-ButikKort 
PROCEDURE SettModified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wModified AS LOGI NO-UNDO.
  DO WITH FRAME FRAME-1:
    ASSIGN Forsalj.ForsNr:MODIFIED = wModified
           Forsalj.FoNamn:MODIFIED = wModified
           Forsalj.FoAnstNr:MODIFIED = wModified
           Forsalj.FoPersNr:MODIFIED = wModified
           Forsalj.FoAdr:MODIFIED = wModified
           Forsalj.FoPadr:MODIFIED = wModified
           Forsalj.FoPoNr:MODIFIED = wModified
           Forsalj.FoTel:MODIFIED = wModified
           Forsalj.navnikasse:MODIFIED = wModified
           Forsalj.FodtDato:MODIFIED = wModified
           Forsalj.Rabatt:MODIFIED = wModified
           Forsalj.Prisendring:MODIFIED = wModified
           Forsalj.Retur:MODIFIED = wModified
           Forsalj.slettTidligere:MODIFIED = wModified
           Forsalj.SlettBong:MODIFIED = wModified
           Forsalj.SletteForste:MODIFIED = wModified
           Forsalj.BrukerId2:MODIFIED = wModified

           Forsalj.FoForNavn:MODIFIED = wModified
           Forsalj.FoAdr2:MODIFIED = wModified
           Forsalj.AnsattDato:MODIFIED = wModified
           Forsalj.SluttetDato:MODIFIED = wModified
           Forsalj.ArbeidsProsent:MODIFIED = wModified
           Forsalj.JobbTittel:MODIFIED = wModified
           Forsalj.Lonnprofil:MODIFIED = wModified
           Forsalj.TimeLonn:MODIFIED = wModified
           Forsalj.FastLonn:MODIFIED = wModified

           .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpChild C-ButikKort 
PROCEDURE SkapaTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipChild AS HANDLE NO-UNDO.
    IF VALID-HANDLE(ipChild) THEN DO:
        CREATE tmpChild.
        ASSIGN tmpChild.wChild = ipChild.
        RELEASE tmpChild.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-ButikKort 
PROCEDURE SlettProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF valid-handle(wSubWin1) THEN
    DELETE PROCEDURE wSubWin1 NO-ERROR.
  APPLY "CLOSE":U TO THIS-PROCEDURE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisEndretInfo C-ButikKort 
PROCEDURE VisEndretInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE Forsalj THEN
    DO:
      ASSIGN
        Fill-In-EndretInfo = "Opprettet " + 
                             (IF Forsalj.RegistrertDato <> ? 
                               THEN string(Forsalj.RegistrertDato)
                               ELSE "        ") + " " +
                             (IF Forsalj.RegistrertTid <> 0
                               THEN string(Forsalj.RegistrertTid,"HH:MM:SS")
                               ELSE "        ") + " av " + 
                             Forsalj.RegistrertAv + "    Endret " +
                             (IF Forsalj.EDato <> ?
                               THEN string(Forsalj.EDato)
                               ELSE "        ") + " " +
                             (IF Forsalj.ETid <> 0
                               THEN string(Forsalj.ETid,"HH:MM:SS")
                               ELSE "        ") + " av " +
                             Forsalj.BrukerId.
    END.
  ELSE DO:
    ASSIGN
      FILL-IN-EndretInfo = "".
  END.
  DISPLAY 
    FILL-IN-EndretInfo
  WITH FRAME FRAME-1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-ButikKort 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wButPoststed AS CHAR NO-UNDO.
  DEF VAR wLevPoststed AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN FILL-IN-ForsNr:SCREEN-VALUE = STRING(Forsalj.ForsNr)
           FILL-IN-FoNamn:SCREEN-VALUE = Forsalj.FoNamn.
  END.
  FIND Post WHERE Post.PostNr = Forsalj.FoPoNr NO-LOCK NO-ERROR.
  ASSIGN FI-PostSted1 = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  DO WITH FRAME FRAME-1:

    DISPLAY Forsalj.ForsNr
            Forsalj.FoNamn
            Forsalj.ForsaljAktiv
            Forsalj.FoAnstNr
            Forsalj.Passord
            Forsalj.FoPersNr 
            Forsalj.FoAdr
            Forsalj.FoPadr
            Forsalj.FoPoNr
            FI-PostSted1
            Forsalj.FoTel
            Forsalj.navnikasse
            Forsalj.FodtDato
            Forsalj.Rabatt
            Forsalj.Prisendring
            Forsalj.Retur
            Forsalj.slettTidligere
            Forsalj.SlettBong
            Forsalj.SletteForste
            Forsalj.ButikkNr
            Forsalj.BrukerId2

            Forsalj.FoForNavn
            Forsalj.FoAdr2
            Forsalj.AnsattDato
            Forsalj.SluttetDato
            Forsalj.ArbeidsProsent
            Forsalj.JobbTittel
            Forsalj.TimeLonn
            Forsalj.FastLonn
            .
    FIND Bruker NO-LOCK WHERE
      Bruker.Brukerid = userid('SkoTex') NO-ERROR.
    IF NOT AVAILABLE Bruker OR Bruker.BrukerType > 1 THEN 
    DO:
      ASSIGN
          Forsalj.LonnProfil:HIDDEN     = TRUE
          Forsalj.ArbeidsProsent:HIDDEN = TRUE
          Forsalj.TimeLonn:HIDDEN       = TRUE
          Forsalj.FastLonn:HIDDEN       = TRUE
          .
    END.
    ASSIGN
        Forsalj.LonnProfil:SCREEN-VALUE IN FRAME Frame-1 = TRIM(Forsalj.LonnProfil).

    /* TN 15/6-11 Lønnsprofil oppdateres ikke i skjerm ved bytte av record??????
    MESSAGE forsalj.lonnprofil forsalj.lonnprofil:SCREEN-VALUE
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
  END.
  RUN VisEndretInfo.
  RUN SettModified(FALSE).
  IF wAktivFlip = 1 THEN
     APPLY "ENTRY" TO Forsalj.FoNamn IN FRAME FRAME-1.
  {&OPEN-QUERY-B-butikkfsg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-ButikKort 
PROCEDURE WinHlp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {winhlp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Modifierad C-ButikKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-1:
    RETURN IF Forsalj.Forsnr:MODIFIED = TRUE OR
              Forsalj.FoNamn:MODIFIED = TRUE OR
              Forsalj.FoAnstNr:MODIFIED = TRUE OR
              Forsalj.FoPersNr:MODIFIED = TRUE OR
              Forsalj.FoAdr:MODIFIED = TRUE OR
              Forsalj.FoPadr:MODIFIED = TRUE OR
              Forsalj.FoPoNr:MODIFIED = TRUE OR
              Forsalj.FoTel:MODIFIED = TRUE OR 

              Forsalj.navnikasse:MODIFIED = TRUE OR
              Forsalj.FodtDato:MODIFIED = TRUE OR
              Forsalj.Rabatt:MODIFIED = TRUE OR
              Forsalj.Prisendring:MODIFIED = TRUE OR
              Forsalj.Retur:MODIFIED = TRUE OR
              Forsalj.slettTidligere:MODIFIED = TRUE OR
              Forsalj.SlettBong:MODIFIED = TRUE OR
              Forsalj.SletteForste:MODIFIED = TRUE OR
              Forsalj.BrukerId2:MODIFIED = TRUE OR

              Forsalj.FoForNavn:MODIFIED = TRUE OR
              Forsalj.FoAdr2:MODIFIED = TRUE OR
              Forsalj.AnsattDato:MODIFIED = TRUE OR
              Forsalj.SluttetDato:MODIFIED = TRUE OR
              Forsalj.ArbeidsProsent:MODIFIED = TRUE OR
              Forsalj.JobbTittel:MODIFIED = TRUE OR
              Forsalj.Lonnprofil:MODIFIED = TRUE OR
              Forsalj.TimeLonn:MODIFIED = TRUE OR
              Forsalj.FastLonn:MODIFIED = TRUE

        THEN TRUE ELSE FALSE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VisTid C-ButikKort 
FUNCTION VisTid RETURNS CHARACTER
  ( INPUT iTid AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN STRING(iTid,"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

