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
 FIND FIRST Butiker NO-LOCK NO-ERROR.
  IF AVAILABLE Butiker THEN
    ASSIGN wInputRecid = RECID(Butiker).
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
DEF VAR wSubWin            AS HANDLE NO-UNDO.
DEF VAR wLapTop            AS LOG    NO-UNDO.
DEF VAR wBekreft           AS LOG    NO-UNDO.
DEF VAR wFeil              AS LOGI   NO-UNDO.
DEF VAR cTekst             AS CHAR   NO-UNDO.
DEF VAR hSkjema            AS HANDLE     NO-UNDO.
/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab1Side        AS COM-HANDLE NO-UNDO.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.
DEFINE VAR wTabTekst         AS CHAR INIT "Vedlikehold,Kasserere" NO-UNDO.
DEFINE VAR wTabHnd           AS HANDLE EXTENT 3 NO-UNDO.
DEFINE VAR wBrowseHandle     AS HANDLE NO-UNDO.
DEFINE VAR lKjButFinns       AS LOGICAL    NO-UNDO.
/* Buffere */
DEF BUFFER bButiker FOR Butiker.
DEF TEMP-TABLE tmpChild 
  FIELD wChild AS HANDLE.
&SCOPED-DEFINE SORTBY-PHRASE BY ApnSkjema.Ar DESCENDING
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
&Scoped-define INTERNAL-TABLES butikkforsalj Forsalj ApnSkjema

/* Definitions for BROWSE B-butikkfsg                                   */
&Scoped-define FIELDS-IN-QUERY-B-butikkfsg butikkforsalj.ForsNr ~
butikkforsalj.KassererId Forsalj.navnikasse Forsalj.FoNamn ~
butikkforsalj.BrukerID butikkforsalj.EDato VisTid(butikkforsalj.ETid) ~
butikkforsalj.RegistrertAv butikkforsalj.RegistrertDato ~
VisTid(butikkforsalj.RegistrertTid) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-butikkfsg 
&Scoped-define QUERY-STRING-B-butikkfsg FOR EACH butikkforsalj ~
      WHERE butikkforsalj.Butik = Butiker.Butik NO-LOCK, ~
      EACH Forsalj OF butikkforsalj NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-butikkfsg OPEN QUERY B-butikkfsg FOR EACH butikkforsalj ~
      WHERE butikkforsalj.Butik = Butiker.Butik NO-LOCK, ~
      EACH Forsalj OF butikkforsalj NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-butikkfsg butikkforsalj Forsalj
&Scoped-define FIRST-TABLE-IN-QUERY-B-butikkfsg butikkforsalj
&Scoped-define SECOND-TABLE-IN-QUERY-B-butikkfsg Forsalj


/* Definitions for BROWSE BROWSE-ApnSkjema                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-ApnSkjema ApnSkjema.Ar ~
ApnSkjema.Ukelengde ApnSkjema.Beskrivelse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-ApnSkjema 
&Scoped-define QUERY-STRING-BROWSE-ApnSkjema FOR EACH ApnSkjema ~
      WHERE ApnSkjema.ButikkNr = Butiker.Butik NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-ApnSkjema OPEN QUERY BROWSE-ApnSkjema FOR EACH ApnSkjema ~
      WHERE ApnSkjema.ButikkNr = Butiker.Butik NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-ApnSkjema ApnSkjema
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-ApnSkjema ApnSkjema


/* Definitions for FRAME FRAME-1                                        */

/* Definitions for FRAME FRAME-2                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-2 ~
    ~{&OPEN-QUERY-B-butikkfsg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 RECT-28 B-KonvTbl CB-Sort ~
BUTTON-Angre BUTTON-Kopier BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev ~
BUTTON-Slett Btn_Help BUTTON-Ok 
&Scoped-Define DISPLAYED-OBJECTS CB-Sort FILL-IN-Butik FILL-IN-ButNamn 

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

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-ArtKort MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE TabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTabStrip AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-KonvTbl 
     LABEL "Konverteringstabell..." 
     SIZE 22 BY 1.1.

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

DEFINE BUTTON BUTTON-SokBut-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL " 1: Vg/LpNr" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS " 1: Butik/Nr"," 2: Butik/Namn" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Velg sorteringsordning som skal gjelde for blaing med pil Opp/Ned." NO-UNDO.

DEFINE VARIABLE FILL-IN-Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN-ButNamn AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 203 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 203 BY .1.

DEFINE BUTTON B-EndreSkjema 
     IMAGE-UP FILE "icon/e-detail.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON B-EtiLayout 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON B-NySkjema 
     IMAGE-UP FILE "icon/e-ny.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON B-SlettSkjema 
     IMAGE-UP FILE "icon/e-del.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON B-SokKunde  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokMvaFG  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokMvaPG  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokPlukk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON B-SokPlukk-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-Sokeknapp 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Sokeknapp-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Sokeknapp-3 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokFakturatekst 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i fakturatekstregister".

DEFINE BUTTON BUTTON-SokPostEtikettSkriver 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-SokPostEtikettSkriver-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-SokRapportSkriver 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-SokSkriver 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-SokSkriverEti 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-SokSkriverRapp 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE BUTTON BUTTON-SokSkriverRapp-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE VARIABLE FI-MvaTxtFG AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MvaTxtPG AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Plukklager AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Securmark AS CHARACTER FORMAT "X(256)":U INITIAL "(Securmark)" 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U INITIAL " Åpningsskjema" 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-22 AS CHARACTER FORMAT "X(256)":U INITIAL "EOD Rapporter" 
      VIEW-AS TEXT 
     SIZE 19 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-EndretInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Endret informasjon" 
      VIEW-AS TEXT 
     SIZE 83.2 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-LevPoststed AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19.2 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Poststed AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 200 BY 26.43.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 6.14.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 200 BY .1.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 2.38.

DEFINE VARIABLE T-Kampanje AS LOGICAL INITIAL no 
     LABEL "Kan kjøre kampanje" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE T-Statistikk AS LOGICAL INITIAL yes 
     LABEL "Statistikk." 
     VIEW-AS TOGGLE-BOX
     SIZE 25.2 BY .81 NO-UNDO.

DEFINE VARIABLE T-VPI AS LOGICAL INITIAL no 
     LABEL "Kan ta imot VPI" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE BUTTON B-Koble 
     LABEL "Koble..." 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Kasserere" 
      VIEW-AS TEXT 
     SIZE 48 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 10.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-butikkfsg FOR 
      butikkforsalj, 
      Forsalj SCROLLING.

DEFINE QUERY BROWSE-ApnSkjema FOR 
      ApnSkjema SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-butikkfsg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-butikkfsg C-ButikKort _STRUCTURED
  QUERY B-butikkfsg NO-LOCK DISPLAY
      butikkforsalj.ForsNr COLUMN-LABEL "Kasserer" FORMAT "zzzzzzz9":U
            WIDTH 9.2
      butikkforsalj.KassererId FORMAT ">>9":U
      Forsalj.navnikasse FORMAT "X(15)":U
      Forsalj.FoNamn FORMAT "x(30)":U WIDTH 32.4
      butikkforsalj.BrukerID FORMAT "X(10)":U
      butikkforsalj.EDato FORMAT "99/99/9999":U
      VisTid(butikkforsalj.ETid) COLUMN-LABEL "Etid"
      butikkforsalj.RegistrertAv FORMAT "X(10)":U
      butikkforsalj.RegistrertDato FORMAT "99/99/9999":U
      VisTid(butikkforsalj.RegistrertTid) COLUMN-LABEL "Regtid"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 63 BY 6.67 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-ApnSkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-ApnSkjema C-ButikKort _STRUCTURED
  QUERY BROWSE-ApnSkjema NO-LOCK DISPLAY
      ApnSkjema.Ar FORMAT ">>>9":U
      ApnSkjema.Ukelengde FORMAT "9":U
      ApnSkjema.Beskrivelse FORMAT "X(30)":U WIDTH 29.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 43 BY 4.52 ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokBut-2 AT ROW 3 COL 29.2 NO-TAB-STOP 
     B-KonvTbl AT ROW 1.24 COL 171
     CB-Sort AT ROW 1.33 COL 34.2 COLON-ALIGNED NO-LABEL
     FILL-IN-Butik AT ROW 3 COL 16 COLON-ALIGNED HELP
          "Butikknummer"
     FILL-IN-ButNamn AT ROW 3 COL 32 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
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
         SIZE 203.6 BY 31.

DEFINE FRAME FRAME-1
     Butiker.webbutik AT ROW 1.57 COL 110 HELP
          ""
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     FI-Securmark AT ROW 25.29 COL 40 COLON-ALIGNED NO-LABEL
     Butiker.Butik AT ROW 1.48 COL 17.2 COLON-ALIGNED FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     Butiker.ButNamn AT ROW 1.48 COL 31.4 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 73.8 BY 1
     Butiker.KortNavn AT ROW 2.48 COL 17.2 COLON-ALIGNED
          LABEL "K.Navn/Ekst.id"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1 TOOLTIP "Kort navn på butikken."
     Butiker.EksterntId AT ROW 2.48 COL 31.6 COLON-ALIGNED HELP
          "Eksternt id på butikken" NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 23.6 BY 1 TOOLTIP "Eksternt id. F.eks fra regnskapssystem."
     Butiker.BuKon AT ROW 3.48 COL 17.2 COLON-ALIGNED
          LABEL "Butikksjef" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     Butiker.OrganisasjonsNr AT ROW 4.48 COL 17.2 COLON-ALIGNED FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     BUTTON-SokPostEtikettSkriver AT ROW 23 COL 192.6 NO-TAB-STOP 
     Butiker.BuAdr AT ROW 5.48 COL 17.2 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     Butiker.BuPadr AT ROW 6.48 COL 17.2 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     Butiker.BuPonr AT ROW 7.48 COL 17.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Butiker.ButLand AT ROW 8.52 COL 17.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     Butiker.BuTel AT ROW 9.52 COL 17.2 COLON-ALIGNED
          LABEL "Telefon/faks"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Butiker.Telefaks AT ROW 9.52 COL 37.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     Butiker.ButFirmanavn AT ROW 2.48 COL 67 COLON-ALIGNED HELP
          "Firmanavn (Brukes på fakturautskrift istedenfor butikknavn)"
          LABEL "Firmanavn" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 38.2 BY 1 TOOLTIP "Firmanavn (Brukes på fakturautskrift istedenfor butikknavn)"
     Butiker.LevKontakt AT ROW 3.48 COL 67 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 38.2 BY 1
     Butiker.LevAdresse1 AT ROW 5.48 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38.2 BY 1
     B-EtiLayout AT ROW 24 COL 157.4 NO-TAB-STOP 
     Butiker.LevAdresse2 AT ROW 6.48 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38.2 BY 1
     Butiker.LevPostBoks AT ROW 7.48 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38.2 BY 1
     Butiker.LevPostNr AT ROW 8.48 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Butiker.LevTelefon AT ROW 9.48 COL 67 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 38.2 BY 1
     Butiker.ProfilNr AT ROW 11.05 COL 17 COLON-ALIGNED FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     Butiker.LevMerknad AT ROW 12.05 COL 17 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 59.8 BY 1
     Butiker.ApningsDato AT ROW 15.05 COL 17 COLON-ALIGNED
          LABEL "Åpnet/system"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 5.24
         SIZE 201.6 BY 26.48.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     Butiker.Sentrallager AT ROW 13.24 COL 19
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 NO-TAB-STOP 
     Butiker.NedlagtDato AT ROW 16.05 COL 17 COLON-ALIGNED
          LABEL "Nedlagt"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Butiker.ButMalNr AT ROW 17.14 COL 17 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 32 BY 1
     BUTTON-SokFakturatekst AT ROW 12 COL 177.2 NO-TAB-STOP 
     Butiker.KommisjonsdatoStart AT ROW 19.57 COL 114 COLON-ALIGNED FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Butiker.Minusbutikk AT ROW 13.24 COL 61
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     Butiker.DirFakturautskrift AT ROW 11.24 COL 116
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .81
     Butiker.Fakturaskriver AT ROW 13.62 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28.6 BY 1
     Butiker.LanButikk AT ROW 13.24 COL 40
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .81 TOOLTIP "Butikken er tilgjengelig via lokalt nettverk." NO-TAB-STOP 
     Butiker.FakturaLayout AT ROW 14.57 COL 114 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 32 BY 1
     Butiker.KundeNr AT ROW 15.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Prisprofil.KortNavn AT ROW 11.05 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     FILL-IN-Poststed AT ROW 7.48 COL 37.2 COLON-ALIGNED NO-LABEL
     Butiker.IntFaktOverforing AT ROW 12.76 COL 116
          VIEW-AS TOGGLE-BOX
          SIZE 28.8 BY .81
     BUTTON-SokSkriverEti AT ROW 22.05 COL 147 NO-TAB-STOP 
     Butiker.FaktKopiRappSkriver AT ROW 12 COL 116
          VIEW-AS TOGGLE-BOX
          SIZE 33.4 BY .81
     Prisprofil.Beskrivelse AT ROW 11.05 COL 50 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26.8 BY 1
     Butiker.clButikkNr AT ROW 14.05 COL 17 COLON-ALIGNED HELP
          "Tilhører sentrallager"
          LABEL "Tilh. sentrallager"
          VIEW-AS FILL-IN NATIVE 
          SIZE 13.2 BY 1 TOOLTIP "Tilhører sentrallager"
     Butiker.PlukkButikk AT ROW 17.57 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
     Butiker.PrioPlukket AT ROW 18.57 COL 114 COLON-ALIGNED
          LABEL "Plukkliste prioritet"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1 TOOLTIP "Prioritering som benyttes ved bygging av plukkliste"
     Butiker.Fakturagebyr AT ROW 15.05 COL 169 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Butiker.Purregebyr AT ROW 16.05 COL 169 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Butiker.StdVeksel AT ROW 24.1 COL 17 COLON-ALIGNED
          LABEL "Std.vekselbeløp"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Butiker.harButikksystem AT ROW 15.14 COL 33
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 5.24
         SIZE 201.6 BY 26.48.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     FI-Plukklager AT ROW 17.57 COL 130.6 COLON-ALIGNED NO-LABEL
     Butiker.FakturaKopi AT ROW 11 COL 169 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     Butiker.FaktTekstNr AT ROW 12 COL 169 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     FILL-IN-LevPoststed AT ROW 8.48 COL 86 COLON-ALIGNED NO-LABEL
     Butiker.FGMomsKod AT ROW 13 COL 169 COLON-ALIGNED
          LABEL "Mva fakturagebyr"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     Butiker.PGMomsKod AT ROW 14 COL 169 COLON-ALIGNED
          LABEL "Mva. purregebyr"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     BUTTON-SokSkriverRapp AT ROW 23.05 COL 147 NO-TAB-STOP 
     FI-MvaTxtFG AT ROW 13 COL 178.2 COLON-ALIGNED NO-LABEL
     FI-MvaTxtPG AT ROW 14 COL 178.2 COLON-ALIGNED NO-LABEL
     Butiker.KasseNr AT ROW 21 COL 149 COLON-ALIGNED
          LABEL "Kasse kundeordre"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Ingen kasse definert",0
          DROP-DOWN-LIST
          SIZE 42 BY 1
     Butiker.EtikettPrinterKOrdre AT ROW 22 COL 149 COLON-ALIGNED
          LABEL "Etikett kundeordre"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Butiker.PostEtikettPrinter AT ROW 23 COL 149 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Butiker.BELayout AT ROW 24 COL 149 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     Butiker.BEPrinter AT ROW 25 COL 149 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Butiker.RAPPrinter AT ROW 26 COL 149 COLON-ALIGNED FORMAT "X(250)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     BROWSE-ApnSkjema AT ROW 3.14 COL 156
     Butiker.VaarREf AT ROW 18.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     B-SokPlukk-2 AT ROW 14 COL 32.4 NO-TAB-STOP 
     Butiker.BankKonto AT ROW 19.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Butiker.Postgiro AT ROW 20.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Butiker.ePostAdresse AT ROW 21.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Butiker.URLAdresse AT ROW 22.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Butiker.BEAktiv AT ROW 24.1 COL 162.4
          LABEL "Etikettskriver"
          VIEW-AS TOGGLE-BOX
          SIZE 17.6 BY .81
     Butiker.BETerminalklient AT ROW 24.05 COL 181
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     Butiker.FalckMedlNr AT ROW 25.1 COL 17 COLON-ALIGNED
          LABEL "ForhandlerID" FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Butiker.EODRapporter AT ROW 8.81 COL 156.2
          VIEW-AS TOGGLE-BOX
          SIZE 19.8 BY .81 NO-TAB-STOP 
     BUTTON-SokSkriverRapp-2 AT ROW 22 COL 192.6 NO-TAB-STOP 
     Butiker.EODFinansrapport AT ROW 9.76 COL 156
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 NO-TAB-STOP 
     T-Kampanje AT ROW 18.14 COL 171 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 5.24
         SIZE 201.6 BY 26.48.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     T-VPI AT ROW 19.05 COL 171 NO-TAB-STOP 
     Butiker.EODBokforingsbilag AT ROW 8.81 COL 178
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 NO-TAB-STOP 
     Butiker.EDOJournal AT ROW 9.76 COL 178
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 NO-TAB-STOP 
     T-Statistikk AT ROW 20 COL 171 HELP
          "Bygg statistikker fra brutikkens varetransaksjoner" NO-TAB-STOP 
     B-EndreSkjema AT ROW 2.05 COL 161.8 NO-TAB-STOP 
     B-NySkjema AT ROW 2.05 COL 156.4 NO-TAB-STOP 
     B-SlettSkjema AT ROW 2.05 COL 167.4 NO-TAB-STOP 
     B-SokMvaFG AT ROW 13 COL 175.8
     B-SokMvaPG AT ROW 14 COL 175.8
     B-SokPlukk AT ROW 17.57 COL 128.6 NO-TAB-STOP 
     BUTTON-SokBut AT ROW 1.48 COL 29 NO-TAB-STOP 
     BUTTON-Sokeknapp AT ROW 11.1 COL 30.6 NO-TAB-STOP 
     BUTTON-Sokeknapp-2 AT ROW 7.48 COL 35 NO-TAB-STOP 
     BUTTON-Sokeknapp-3 AT ROW 8.48 COL 83.8 NO-TAB-STOP 
     BUTTON-SokSkriver AT ROW 13.62 COL 144.8 NO-TAB-STOP 
     B-SokKunde AT ROW 15.57 COL 136.4
     FILL-IN-EndretInfo AT ROW 26.52 COL 2.8 NO-LABEL
     FILL-IN-21 AT ROW 1.24 COL 167 COLON-ALIGNED NO-LABEL
     FILL-IN-22 AT ROW 8.05 COL 166 COLON-ALIGNED NO-LABEL
     BUTTON-SokRapportSkriver AT ROW 26.05 COL 193 NO-TAB-STOP 
     BUTTON-SokPostEtikettSkriver-2 AT ROW 25 COL 193 NO-TAB-STOP 
     "Leveringsadresse" VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 4.81 COL 69
          FONT 6
     RECT-46 AT ROW 1 COL 1
     RECT-55 AT ROW 1.71 COL 154
     RECT-56 AT ROW 10.81 COL 1.8
     RECT-57 AT ROW 8.33 COL 154
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.4 ROW 5.24
         SIZE 201.6 BY 26.48.

DEFINE FRAME FRAME-2
     B-butikkfsg AT ROW 3.67 COL 9
     B-Koble AT ROW 10.76 COL 57
     FILL-IN-1 AT ROW 2.91 COL 7.8 COLON-ALIGNED NO-LABEL
     RECT-47 AT ROW 1.71 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 5.29
         SIZE 201 BY 26.43.


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
         TITLE              = "Butik"
         HEIGHT             = 31
         WIDTH              = 203.6
         MAX-HEIGHT         = 33.71
         MAX-WIDTH          = 204.4
         VIRTUAL-HEIGHT     = 33.71
         VIRTUAL-WIDTH      = 204.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-ArtKort:HANDLE.
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

/* SETTINGS FOR BUTTON BUTTON-SokBut-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Butik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ButNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-1
   Custom                                                               */
/* BROWSE-TAB BROWSE-ApnSkjema RAPPrinter FRAME-1 */
/* SETTINGS FOR FILL-IN Butiker.ApningsDato IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Butiker.BEAktiv IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.BELayout IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Prisprofil.Beskrivelse IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Butiker.BuAdr IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.BuKon IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Butiker.BuPadr IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.BuTel IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.ButFirmanavn IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Butiker.Butik IN FRAME FRAME-1
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Butiker.ButNamn IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.clButikkNr IN FRAME FRAME-1
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
ASSIGN 
       Butiker.clButikkNr:READ-ONLY IN FRAME FRAME-1        = TRUE.

ASSIGN 
       Butiker.EDOJournal:HIDDEN IN FRAME FRAME-1           = TRUE.

/* SETTINGS FOR FILL-IN Butiker.EksterntId IN FRAME FRAME-1
   EXP-HELP                                                             */
ASSIGN 
       Butiker.EODBokforingsbilag:HIDDEN IN FRAME FRAME-1           = TRUE.

ASSIGN 
       Butiker.EODFinansrapport:HIDDEN IN FRAME FRAME-1           = TRUE.

/* SETTINGS FOR FILL-IN Butiker.EtikettPrinterKOrdre IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.Fakturaskriver IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Butiker.FalckMedlNr IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Butiker.FGMomsKod IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FI-MvaTxtFG IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MvaTxtPG IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Plukklager IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Securmark IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-EndretInfo IN FRAME FRAME-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-LevPoststed IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Poststed IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Butiker.harButikksystem IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Butiker.KasseNr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.KommisjonsdatoStart IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Prisprofil.KortNavn IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Butiker.KortNavn IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.LevAdresse2 IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.LevKontakt IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.LevMerknad IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.LevTelefon IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.NedlagtDato IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.OrganisasjonsNr IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.PGMomsKod IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.PrioPlukket IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Butiker.ProfilNr IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.RAPPrinter IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Butiker.StdVeksel IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Butiker.webbutik IN FRAME FRAME-1
   EXP-HELP                                                             */
/* SETTINGS FOR FRAME FRAME-2
                                                                        */
/* BROWSE-TAB B-butikkfsg RECT-47 FRAME-2 */
/* SETTINGS FOR BUTTON B-Koble IN FRAME FRAME-2
   NO-ENABLE                                                            */
ASSIGN 
       B-Koble:HIDDEN IN FRAME FRAME-2           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ButikKort)
THEN C-ButikKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-butikkfsg
/* Query rebuild information for BROWSE B-butikkfsg
     _TblList          = "skotex.butikkforsalj,skotex.Forsalj OF skotex.butikkforsalj"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.butikkforsalj.Butik = Butiker.Butik"
     _FldNameList[1]   > skotex.butikkforsalj.ForsNr
"butikkforsalj.ForsNr" "Kasserer" "zzzzzzz9" "integer" ? ? ? ? ? ? no ? no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = skotex.butikkforsalj.KassererId
     _FldNameList[3]   = skotex.Forsalj.navnikasse
     _FldNameList[4]   > skotex.Forsalj.FoNamn
"Forsalj.FoNamn" ? ? "character" ? ? ? ? ? ? no ? no no "32.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = skotex.butikkforsalj.BrukerID
     _FldNameList[6]   = skotex.butikkforsalj.EDato
     _FldNameList[7]   > "_<CALC>"
"VisTid(butikkforsalj.ETid)" "Etid" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = skotex.butikkforsalj.RegistrertAv
     _FldNameList[9]   = skotex.butikkforsalj.RegistrertDato
     _FldNameList[10]   > "_<CALC>"
"VisTid(butikkforsalj.RegistrertTid)" "Regtid" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE B-butikkfsg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-ApnSkjema
/* Query rebuild information for BROWSE BROWSE-ApnSkjema
     _TblList          = "skotex.ApnSkjema"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "skotex.ApnSkjema.ButikkNr = Butiker.Butik"
     _FldNameList[1]   = skotex.ApnSkjema.Ar
     _FldNameList[2]   = skotex.ApnSkjema.Ukelengde
     _FldNameList[3]   > skotex.ApnSkjema.Beskrivelse
"ApnSkjema.Beskrivelse" ? ? "character" ? ? ? ? ? ? no ? no no "29.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-ApnSkjema */
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
       ROW             = 4.1
       COLUMN          = 1.2
       HEIGHT          = 27.86
       WIDTH           = 203
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(FILL-IN-ButNamn:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-ButikKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ButikKort C-ButikKort
ON END-ERROR OF C-ButikKort /* Butik */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ButikKort C-ButikKort
ON WINDOW-CLOSE OF C-ButikKort /* Butik */
DO:

  IF CAN-FIND(FIRST tmpChild WHERE
               VALID-HANDLE(tmpChild.wChild)) THEN
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
&Scoped-define SELF-NAME Butiker.ApningsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.ApningsDato C-ButikKort
ON DELETE-CHARACTER OF Butiker.ApningsDato IN FRAME FRAME-1 /* Åpnet/system */
DO:
  ASSIGN
    SELF:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EndreSkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EndreSkjema C-ButikKort
ON CHOOSE OF B-EndreSkjema IN FRAME FRAME-1
DO:
    DEFINE VARIABLE rApnSkjema AS ROWID      NO-UNDO.
    IF NOT AVAIL ApnSkjema OR VALID-HANDLE(hSkjema) THEN
        RETURN NO-APPLY.
    ASSIGN rApnSkjema = ROWID(ApnSkjema).
    /* ?,?,?,"" är parametrar vid nyreg */
    RUN w-ButOppetMal.w PERSISTENT SET hSkjema (INPUT-OUTPUT rApnSkjema,?,?,?,"").
    IF RETURN-VALUE <> "AVBRYT" THEN DO:
        FIND CURRENT ApnSkjema NO-LOCK.
        BROWSE BROWSE-ApnSkjema:REFRESH().
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EtiLayout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EtiLayout C-ButikKort
ON CHOOSE OF B-EtiLayout IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.BELayout
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLayoutListe   AS CHARACTER  NO-UNDO.
  DEFINE BUFFER bSyspara FOR Syspara.

  DO WITH FRAME FRAME-1:
      FOR EACH Syspara WHERE SysPara.SysHId = 5 AND SysPara.SysGr = 21 AND 
                                                    SysPara.Parameter2 BEGINS "AVAIL" NO-LOCK:
          FIND bSysPara WHERE bSysPara.SysHId = 5 AND bSysPara.SysGr = 20 AND 
                                                      bSysPara.ParaNr = SysPara.ParaNr NO-LOCK.
          ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                  bSysPara.Parameter2 + "," + STRING(bSysPara.ParaNr).
      END.
      ASSIGN cValgtVerdi = IF INT(Butiker.BELayout:SCREEN-VALUE) > 0 THEN Butiker.BELayout:SCREEN-VALUE ELSE "".
      RUN d-VelgGenerellCombo.w ("Velg layout",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.BELayout:SCREEN-VALUE = cValgtVerdi.
                 Butiker.BELayout:MODIFIED = TRUE.
          APPLY "TAB" TO Butiker.BELayout.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-2
&Scoped-define SELF-NAME B-Koble
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Koble C-ButikKort
ON CHOOSE OF B-Koble IN FRAME FRAME-2 /* Koble... */
DO:

/* detta tas om hand i kassererregistreringen */

/*   DEF VAR cAktForsListe   AS CHAR NO-UNDO.                                               */
/*   DEF VAR cIOForsListe    AS CHAR NO-UNDO.                                               */
/*                                                                                          */
/*   FOR EACH butikkforsalj WHERE butikkforsalj.Butik = Butiker.Butik NO-LOCK.              */
/*       ASSIGN cAktForsListe = cAktForsListe + (IF cAktForsListe = "" THEN "" ELSE ",")  + */
/*           STRING(butikkforsalj.ForsNr).                                                  */
/*   END.                                                                                   */
/*   ASSIGN cIOForsListe = cAktForsListe.                                                   */
/*   RUN d-tagforsalj.w (INPUT-OUTPUT cIOForsListe).                                        */
/*   IF cIOForsListe <> cAktForsListe THEN DO:                                              */
/*       RUN FixKobling (cIOForsListe).                                                     */
/*   END.                                                                                   */
/*   {&OPEN-QUERY-B-butikkfsg}                                                              */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME B-KonvTbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvTbl C-ButikKort
ON CHOOSE OF B-KonvTbl IN FRAME DEFAULT-FRAME /* Konverteringstabell... */
DO:
  IF NOT AVAILABLE Butiker THEN
    RETURN.
  RUN d-bimpkonv.w (1500,
                    string(Butiker.Butik),
                    "Kobling av butikk"
                    ).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME B-NySkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-NySkjema C-ButikKort
ON CHOOSE OF B-NySkjema IN FRAME FRAME-1
DO:
    DEFINE VARIABLE iAar         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntdar      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cOpenClosed  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE rNyRowId       AS ROWID      NO-UNDO.
    DEFINE VARIABLE cDummy1      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBeskrivelse AS CHARACTER  NO-UNDO.
    IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = INPUT Butiker.Butik) THEN
        RETURN NO-APPLY.
    IF Butiker.ApningsDato = ? THEN DO:
        MESSAGE "Registrer butikkens åpningsdato."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    RUN d-NySkjema.w (INPUT-OUTPUT iAar, INPUT-OUTPUT iAntDar, OUTPUT cBeskrivelse).
    IF RETURN-VALUE <> "AVBRYT" THEN DO:
        IF YEAR(Butiker.ApningsDato) > iAar THEN DO:
            MESSAGE "Butikken ikke åpnet ønsket år (" + STRING(iAar) + ")."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        ELSE IF YEAR(Butiker.NedlagtDato) < iAar THEN DO:
            MESSAGE "Butikken nedlagd " STRING(Butiker.NedlagtDato) ". Ønsket år: (" + STRING(iAar) + ")."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        ELSE IF CAN-FIND(FIRST ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik
                        AND ApnSkjema.Ar = iAar) THEN DO:
            MESSAGE "Skjema for " iAar " allerede registrert!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        RUN w-ButOppetMal.w (INPUT-OUTPUT rNyRowId,Butiker.Butik,iAar,iAntDar,cBeskrivelse).
        IF rNyRowId <> ? THEN DO:
          {&OPEN-QUERY-BROWSE-ApnSkjema}
          REPOSITION BROWSE-ApnSkjema TO ROWID rNyRowId.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettSkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettSkjema C-ButikKort
ON CHOOSE OF B-SlettSkjema IN FRAME FRAME-1
DO:
  IF BROWSE BROWSE-ApnSkjema:FOCUSED-ROW = ? THEN
      RETURN NO-APPLY.
  MESSAGE "Slette skjema?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSlett AS LOGICAL.
  IF NOT lSlett THEN
      RETURN.
  FIND CURRENT ApnSkjema EXCLUSIVE.
  DELETE ApnSkjema.
  BROWSE BROWSE-ApnSkjema:DELETE-CURRENT-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokKunde C-ButikKort
ON CHOOSE OF B-SokKunde IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.FGMomsKod
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "KundeNr;Navn".
  RUN JBoxDLookup.w ("Kunde;KundeNr;Navn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Butiker.KundeNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
/*     FI-MvaTxtFG:SCREEN-VALUE = entry(2,cLookupValue,"|"). */
    APPLY "TAB" TO Butiker.KundeNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokMvaFG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokMvaFG C-ButikKort
ON CHOOSE OF B-SokMvaFG IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.FGMomsKod
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "MomsKod;Beskrivelse".
  RUN JBoxDLookup.w ("Moms;MomsKod;MomsProc;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Butiker.FGMomsKod:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    FI-MvaTxtFG:SCREEN-VALUE = ENTRY(2,cLookupValue,"|").
    APPLY "TAB" TO Butiker.FGMomsKod.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokMvaPG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokMvaPG C-ButikKort
ON CHOOSE OF B-SokMvaPG IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.PGMomsKod
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "MomsKod;Beskrivelse".
  RUN JBoxDLookup.w ("Moms;MomsKod;MomsProc;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Butiker.PGMomsKod:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    FI-MvaTxtPG:SCREEN-VALUE = ENTRY(2,cLookupValue,"|").
    APPLY "TAB" TO Butiker.PGMomsKod.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokPlukk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokPlukk C-ButikKort
ON CHOOSE OF B-SokPlukk IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.PlukkButik
DO:
    DEF VAR cLookupValue AS CHAR NO-UNDO.

    /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
    /*          Param2: <Where sats> m/Join                                              */
    /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
    /* Kalkulerte felt kan også benyttes, label, format o.l..       */
    cLookupValue = "Butik;ButNamn".
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn","where true",INPUT-OUTPUT cLookupValue).

    IF cLookupValue NE "" THEN 
    DO:
      Butiker.PlukkButik:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokPlukk-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokPlukk-2 C-ButikKort
ON CHOOSE OF B-SokPlukk-2 IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.clButikkNr
DO:
    DEF VAR cLookupValue AS CHAR NO-UNDO.

    /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
    /*          Param2: <Where sats> m/Join                                              */
    /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
    /* Kalkulerte felt kan også benyttes, label, format o.l..       */
    cLookupValue = "Butik;ButNamn".
    RUN JBoxDLookup.w ("Butiker;Butik;ButNamn","where Butiker.Sentrallager = true",INPUT-OUTPUT cLookupValue).

    IF cLookupValue NE "" THEN 
    DO:
      Butiker.clButikkNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.BETerminalklient
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.BETerminalklient C-ButikKort
ON VALUE-CHANGED OF Butiker.BETerminalklient IN FRAME FRAME-1 /* Terminalklient */
DO:
    ASSIGN Butiker.BEPrinter:SENSITIVE = SELF:CHECKED.
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


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME Butiker.BuPonr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.BuPonr C-ButikKort
ON LEAVE OF Butiker.BuPonr IN FRAME FRAME-1 /* Postnummer */
OR "RETURN":U OF Butiker.BuPoNr OR "TAB":U OF Butiker.BuPoNr
DO:
  FIND Post NO-LOCK WHERE 
    Post.PostNr = INPUT Butiker.BuPoNr NO-ERROR.
  IF AVAILABLE Post THEN
    DO:
      DISPLAY 
        Post.Beskrivelse @ FILL-IN-Poststed
      WITH FRAME Frame-1.
    END.
  ELSE DO:
    FILL-IN-Poststed:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.ButNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.ButNamn C-ButikKort
ON TAB OF Butiker.ButNamn IN FRAME FRAME-1 /* Butikknavn */
OR "RETURN":U OF Butiker.ButNamn
DO:
    APPLY "ENTRY":U TO Butiker.KortNavn.
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-ButikKort
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  IF wModus = "NY" THEN
     ASSIGN Butiker.Butik:SENSITIVE IN FRAME FRAME-1 = NO.
            wModus = "ENDRE".
  RUN VisPost.
  RUN BUTTONEnaDis.
  RETURN NO-APPLY.
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
      Butiker.Butik:SENSITIVE = TRUE
      Butiker.Butik:SCREEN-VALUE = "0".
    RUN BUTTONEnaDis.
    APPLY "ENTRY":U TO Butiker.Butik.
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
               VALID-HANDLE(tmpChild.wChild)) THEN
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
&Scoped-define SELF-NAME BUTTON-SokBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut C-ButikKort
ON CHOOSE OF BUTTON-SokBut IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.Butik
DO:
  DO WITH FRAME FRAME-1:
     DEF VAR wButik LIKE Butiker.Butik NO-UNDO.
     DEF VAR rRowId AS ROWID NO-UNDO.
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
      ASSIGN wButik = IF wModus = "ENDRE" THEN Butiker.Butik ELSE ?.
      IF lKjButFinns AND wModus = "NY" THEN DO:
          RUN d-bkjedensbutikker.w (INPUT-OUTPUT rRowId).
          IF rRowId = ? THEN DO:
              IF AVAIL butiker THEN
                  APPLY "CHOOSE" TO BUTTON-Angre IN FRAME DEFAULT-FRAME.
              ELSE
                  APPLY "CLOSE" TO THIS-PROCEDURE.
              RETURN NO-APPLY.
          END.
      END.
      IF wModus = "NY" THEN DO:
         IF lKjButFinns THEN DO:
             RELEASE Kjedensbutikker.
             IF rRowId <> ? THEN
                 FIND KjedensButikker WHERE ROWID(KjedensButikker) = rRowId NO-LOCK NO-ERROR.
             IF NOT AVAIL KjedensButikker THEN DO:
                 APPLY "CHOOSE" TO BUTTON-Angre IN FRAME DEFAULT-FRAME.
                 RETURN NO-APPLY.
             END.
             FIND Post WHERE Post.Postnr = Kjedensbutikker.PostNr NO-LOCK NO-ERROR.
             DO WITH FRAME FRAME-1:
                 ASSIGN Butiker.BuAdr:SCREEN-VALUE = KjedensButikker.Adresse1 
                        Butiker.BuKon:SCREEN-VALUE = KjedensButikker.DagligLeder
                        Butiker.BuPonr:SCREEN-VALUE = KjedensButikker.PostNr
                        Butiker.BuTel:SCREEN-VALUE   = KjedensButikker.Telefon
                        Butiker.Butik:SCREEN-VALUE = STRING(KjedensButikker.ButikkNr)
                        Butiker.ButNamn:SCREEN-VALUE = KjedensButikker.ButikkNavn 
                        Butiker.LevKontakt:SCREEN-VALUE = KjedensButikker.Kontaktperson
                        Butiker.OrganisasjonsNr:SCREEN-VALUE = KjedensButikker.OrganisasjonsNr 
                        FILL-IN-Poststed:SCREEN-VALUE = Post.Beskrivelse.
                 APPLY "ENTRY" TO Butiker.KortNavn.
             END.
         END.
         ELSE DO:
             APPLY "ENTRY" TO Butiker.Butik IN FRAME FRAME-1.
             RETURN NO-APPLY.
         END.
      END.
      ELSE DO:
         RUN d-bbutiker.w (INPUT-OUTPUT wButik).
         IF RETURN-VALUE <> "AVBRYT" THEN DO:
           FIND Butiker WHERE Butiker.Butik = wButik NO-LOCK.
           ASSIGN wInputRecid = RECID(Butiker).
           RUN VisPost.
        END.
        ELSE DO:
            APPLY "ENTRY" TO Butiker.ButNamn IN FRAME FRAME-1.
            RETURN NO-APPLY.
        END.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokBut-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut-2 C-ButikKort
ON CHOOSE OF BUTTON-SokBut-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
    APPLY "CHOOSE" TO BUTTON-SokBut IN FRAME FRAME-1.
    IF wAktivFlip = 2 AND RETURN-VALUE <> "AVBRYT" THEN
      DO:
        RUN ByttObjekt IN wSubWin (STRING(Butiker.Butik,"999999")).  
      END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME BUTTON-Sokeknapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp C-ButikKort
ON CHOOSE OF BUTTON-Sokeknapp IN FRAME FRAME-1 /* ... */
DO:
  
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ProfilNr;KortNavn;Beskrivelse".
  RUN JBoxDLookup.w ("Prisprofil;ProfilNr;KortNavn;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Butiker.ProfilNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    Prisprofil.KortNavn:SCREEN-VALUE = ENTRY(2,cLookupValue,"|").
    Prisprofil.Beskrivelse:SCREEN-VALUE = ENTRY(3,cLookupValue,"|").
    APPLY "TAB" TO Butiker.ProfilNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sokeknapp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp-2 C-ButikKort
ON CHOOSE OF BUTTON-Sokeknapp-2 IN FRAME FRAME-1 /* ... */
OR "F10":U OF Butiker.BuPoNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    Butiker.BuPoNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        Butiker.BuPoNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        FILL-IN-PostSted:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sokeknapp-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp-3 C-ButikKort
ON CHOOSE OF BUTTON-Sokeknapp-3 IN FRAME FRAME-1 /* ... */
OR "F10":U OF Butiker.LevPostNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    Butiker.LevPostNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        Butiker.LevPostNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        FILL-IN-LevPostSted:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFakturatekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFakturatekst C-ButikKort
ON CHOOSE OF BUTTON-SokFakturatekst IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.FaktTekstNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "FaktTekstNr".
  RUN JBoxDLookup.w ("FakturaTekst;FaktTekstNr;FaktTekst","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    Butiker.FaktTekstNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
    APPLY "TAB" TO Butiker.FaktTekstNr.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokPostEtikettSkriver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokPostEtikettSkriver C-ButikKort
ON CHOOSE OF BUTTON-SokPostEtikettSkriver IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.EtikettPrinterKOrdre
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = Butiker.Fakturaskriver:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.PostEtikettPrinter:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS()).
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokPostEtikettSkriver-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokPostEtikettSkriver-2 C-ButikKort
ON CHOOSE OF BUTTON-SokPostEtikettSkriver-2 IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.BEPrinter
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.
  
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = Butiker.BEPrinter:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.BEPrinter:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS()).
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokRapportSkriver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokRapportSkriver C-ButikKort
ON CHOOSE OF BUTTON-SokRapportSkriver IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.RapPrinter
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.

  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = Butiker.RapPrinter:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.RapPrinter:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS()).
          APPLY "TAB" TO Butiker.ButNamn.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSkriver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSkriver C-ButikKort
ON CHOOSE OF BUTTON-SokSkriver IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.Fakturaskriver
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = Butiker.Fakturaskriver:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.Fakturaskriver:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS()).
          APPLY "TAB" TO Butiker.FakturaKopi.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSkriverEti
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSkriverEti C-ButikKort
ON CHOOSE OF BUTTON-SokSkriverEti IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.BEPrinter
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = Butiker.BEPrinter:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.BEPrinter:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS()).
          APPLY "TAB" TO Butiker.BEPrinter.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSkriverRapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSkriverRapp C-ButikKort
ON CHOOSE OF BUTTON-SokSkriverRapp IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.RAPPrinter
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = Butiker.Fakturaskriver:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.RAPPrinter:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS()).
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSkriverRapp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSkriverRapp-2 C-ButikKort
ON CHOOSE OF BUTTON-SokSkriverRapp-2 IN FRAME FRAME-1 /* ... */
OR F10 OF Butiker.EtikettPrinterKOrdre
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = Butiker.Fakturaskriver:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN Butiker.EtikettPrinterKOrdre:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS()).
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.EODRapporter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.EODRapporter C-ButikKort
ON VALUE-CHANGED OF Butiker.EODRapporter IN FRAME FRAME-1 /* EOD Rapporter */
DO:
  ASSIGN
      Butiker.EODFinansrapport:SENSITIVE   = Butiker.EODRapporter:CHECKED
      Butiker.EODBokforingsbilag:SENSITIVE = Butiker.EODRapporter:CHECKED
      Butiker.EDOJournal:SENSITIVE         = Butiker.EODRapporter:CHECKED
      .
  IF Butiker.EODRapporter:CHECKED= FALSE THEN
      ASSIGN
      Butiker.EODFinansrapport:CHECKED   = FALSE
      Butiker.EODBokforingsbilag:CHECKED = FALSE
      Butiker.EDOJournal:CHECKED         = FALSE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.KortNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.KortNavn C-ButikKort
ON TAB OF Butiker.KortNavn IN FRAME FRAME-1 /* K.Navn/Ekst.id */
OR "RETURN":U OF Butiker.KortNavn
DO:
    APPLY "ENTRY":U TO Butiker.EksterntId.
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.LanButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.LanButikk C-ButikKort
ON RETURN OF Butiker.LanButikk IN FRAME FRAME-1 /* LAN butikk */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.LevPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.LevPostNr C-ButikKort
ON LEAVE OF Butiker.LevPostNr IN FRAME FRAME-1 /* PostNr */
OR "RETURN":U OF Butiker.LevPostNr OR "TAB":U OF Butiker.LevPostNr
DO:
  FIND Post NO-LOCK WHERE 
    Post.PostNr = INPUT Butiker.LevPostNr NO-ERROR.
  IF AVAILABLE Post THEN
    DO:
      DISPLAY 
        Post.Beskrivelse @ FILL-IN-LevPoststed
      WITH FRAME Frame-1.
    END.
  ELSE DO:
     ASSIGN FILL-IN-LevPoststed:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.NedlagtDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.NedlagtDato C-ButikKort
ON DELETE-CHARACTER OF Butiker.NedlagtDato IN FRAME FRAME-1 /* Nedlagt */
DO:
  ASSIGN
    SELF:SCREEN-VALUE = "".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.PlukkButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.PlukkButikk C-ButikKort
ON LEAVE OF Butiker.PlukkButikk IN FRAME FRAME-1 /* Plukklager */
DO:
  IF INPUT Butiker.PlukkButik > 0 THEN
  DO:
      FIND bButiker WHERE bButiker.Butik = INPUT Butiker.PlukkButik NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bButiker THEN
      DO:
        MESSAGE 
        "Ugyldig butikknummer"          
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
      END.
      ASSIGN
          Butiker.PlukkButik:SCREEN-VALUE = STRING(bButiker.butik)
          FI-PlukkLager:SCREEN-VALUE = bButiker.KortNAvn
          .
      APPLY LASTKEY.
  END.
  ELSE DO:
      ASSIGN
          Butiker.PlukkButik:SCREEN-VALUE = ""
          FI-PlukkLager:SCREEN-VALUE = ""
          .
      APPLY LASTKEY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.ProfilNr C-ButikKort
ON LEAVE OF Butiker.ProfilNr IN FRAME FRAME-1 /* Prisprofil */
OR "RETURN":U OF Butiker.ProfilNr OR "TAB":U OF Butiker.ProfilNr
DO:
  FIND Prisprofil NO-LOCK WHERE 
    Prisprofil.ProfilNr = INPUT Butiker.ProfilNr NO-ERROR.
  IF AVAILABLE Prisprofil THEN
    DO:
      DISPLAY 
        Prisprofil.Beskrivelse 
        Prisprofil.KortNavn
      WITH FRAME Frame-1.
    END.
  ELSE DO:
      ASSIGN  Prisprofil.Beskrivelse:SCREEN-VALUE = ""
              Prisprofil.KortNavn:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butiker.Sentrallager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butiker.Sentrallager C-ButikKort
ON RETURN OF Butiker.Sentrallager IN FRAME FRAME-1 /* Sentrallager */
DO:
  APPLY "TAB" TO SELF.
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
      APPLY "ENTRY" TO Butiker.ButNamn IN FRAME FRAME-1.
  ELSE IF wAktivFlip <> 1 THEN
      APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
 END.
 ELSE DO:
    IF NOT wFeil THEN DO:
     IF wAktivFlip = 1 THEN
            APPLY "ENTRY" TO Butiker.ButNamn IN FRAME FRAME-1.
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
  &PostIClose    = "if valid-handle(wSubWin) then
                       do:
                         /* RUN SaveBrowseSettings in wHistorikk. */
                         delete procedure wSubWin no-error.
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
IF VALID-HANDLE(wLibHandle) THEN
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
  RUN InitCB.
  ASSIGN lKjButFinns = CAN-FIND(FIRST Kjedensbutikker).
  RUN enable_UI.
  
  {lng.i} /* Oversettelse */
  
  ASSIGN {&WINDOW-NAME}:HIDDEN = NO
         CB-Sort:SCREEN-VALUE = ENTRY(1,CB-Sort:LIST-ITEMS)
         wTabHnd[1] = FRAME FRAME-1:HANDLE
         wTabHnd[2] = FRAME FRAME-2:HANDLE.

  RUN ByttFrame. /* Legger opp f›rste fane. */

/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  ASSIGN chTab1Side = chTabStrip. /* COM-Handl 1. side */
  

  IF wModus <> "NY" THEN DO:
      FIND Butiker WHERE RECID(Butiker) = wInputRecid NO-LOCK NO-ERROR.
      RUN InitCB2.
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
  /*  */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  IF wModus <> "NY" THEN
    FIND Butiker NO-LOCK WHERE 
      RECID(Butiker) = wInputRecid NO-ERROR.
      
  /* Retur verdi */  
  IF AVAILABLE Butiker AND wModus <> "NY" THEN
    RETURN STRING(RECID(Butiker)).
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
  DO WITH FRAME FRAME-1:
     IF Modifierad() THEN DO:
         RUN LagrePost(0).
         IF RETURN-VALUE = "AVBRYT" THEN
             RETURN NO-APPLY.
     END.
     ASSIGN wModus = "NY"
         T-Kampanje = FALSE
         T-VPI = FALSE
         T-Statistikk = TRUE
         FILL-IN-Butik:SCREEN-VALUE IN FRAME DEFAULT-FRAME = "" 
         FILL-IN-ButNamn:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
         Butiker.Butik:SENSITIVE = NOT lKjButFinns
         Butiker.Butik:SCREEN-VALUE = "0"
         Butiker.ButNamn:SCREEN-VALUE = ""
         
         Butiker.KortNavn:SCREEN-VALUE = ""
         Butiker.EksterntId:SCREEN-VALUE = ""
         Butiker.BuKon:SCREEN-VALUE = ""
         Butiker.BuAdr:SCREEN-VALUE = ""
         Butiker.BuPadr:SCREEN-VALUE = ""
         Butiker.BuPoNr:SCREEN-VALUE = ""
         FILL-IN-PostSted:SCREEN-VALUE = ""
         Butiker.BuTel:SCREEN-VALUE = ""
         Butiker.ProfilNr:SCREEN-VALUE = ""
         PrisProfil.KortNavn:SCREEN-VALUE = ""
         Butiker.LevMerknad:SCREEN-VALUE = ""
         Butiker.LevKontakt:SCREEN-VALUE = ""
         Butiker.LevAdresse1:SCREEN-VALUE = ""
         Butiker.LevAdresse2:SCREEN-VALUE = ""
         Butiker.LevPostBoks:SCREEN-VALUE = ""
         Butiker.LevPostNr:SCREEN-VALUE = ""
         FILL-IN-LevPostSted:SCREEN-VALUE = ""
         Butiker.LevTelefon:SCREEN-VALUE = ""
         Butiker.ApningsDato:SCREEN-VALUE = ""
         Butiker.NedlagtDato:SCREEN-VALUE = ""
         Butiker.StdVeksel:SCREEN-VALUE = ""
         Butiker.FalckMedlNr:SCREEN-VALUE = ""
         Butiker.FakturaGebyr:SCREEN-VALUE = ""
         Butiker.ButLand:SCREEN-VALUE = ""
         Butiker.Telefaks:SCREEN-VALUE = ""
         Butiker.Purregebyr:SCREEN-VALUE = ""
         Butiker.VaarRef:SCREEN-VALUE = ""
         Butiker.BankKonto:SCREEN-VALUE = ""
         Butiker.PostGiro:SCREEN-VALUE = ""
         Butiker.ePostAdresse:SCREEN-VALUE = ""
         Butiker.URLAdresse:SCREEN-VALUE = ""
         Butiker.Minusbutik:SCREEN-VALUE = "no"
         Butiker.EODRapporter:SCREEN-VALUE = "no"
         Butiker.EODFinansrapport:SCREEN-VALUE = "no"
         Butiker.EODBokforingsbilag:SCREEN-VALUE = "no"
         Butiker.EDOJournal:SCREEN-VALUE = "no"
         Butiker.FaktKopiRappSkriver:SCREEN-VALUE = "no"
         Butiker.KasseNr:SCREEN-VALUE = "0"
         Butiker.EtikettPrinterKOrdre:SCREEN-VALUE = ""
         Butiker.PostEtikettPrinter:SCREEN-VALUE = ""
         Butiker.KommisjonsdatoStart:SCREEN-VALUE = ""
         Butiker.ButFirmanavn:SCREEN-VALUE = ""
         Butiker.webbutik:SCREEN-VALUE = "no"
         .
     DISPLAY 
         T-Kampanje
         T-VPI
         T-Statistikk
         .
    CLOSE QUERY BROWSE-ApnSkjema.
    RUN BUTTONEnaDis.
    IF NOT lKjButFinns THEN
       APPLY "ENTRY" TO Butiker.Butik.
    ELSE DO:
        APPLY "CHOOSE" TO BUTTON-SokBut IN FRAME FRAME-1.
    END.
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
  
  FIND bButiker NO-LOCK WHERE
    RECID(bButiker) = recid(Butiker) NO-ERROR.
  FIND NEXT bButiker NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bButiker THEN
    FIND FIRST bButiker.
  IF AVAILABLE bButiker THEN
    ASSIGN
      wNesteRecid = RECID(bButiker).

  IF CAN-FIND(FIRST ArtLag OF Butiker) THEN ~
    DO: ~
      MESSAGE "Det finnes artikkler med lager på denne butikken!" SKIP ~
              "Posten kan ikke slettes." ~
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding". ~
      IF wModus = "SLETT" THEN
          RETURN "AVBRYT".
       ELSE
          RETURN NO-APPLY.
    END.
  ASSIGN wOk = FALSE.
  MESSAGE "Skal butikken slettes?" VIEW-AS ALERT-BOX 
    QUESTION BUTTONS YES-NO
    TITLE "Bekreftelse"
    UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY "AVBRYT".  
  ELSE DO:
    FOR EACH stLinje EXCLUSIVE where
             StLinje.StType = "BUTSTAT" and
             StLinje.DataObjekt = string(Butiker.Butik,"999999"):
       DELETE stLinje.
    END.
    FOR EACH ButikkTilgang OF Butiker:
        DELETE ButikkTilgang.
    END.
    FOR EACH ButikkKobling OF Butiker:
        DELETE ButikkKobling.
    END.
    FIND CURRENT Butiker EXCLUSIVE.
    DELETE Butiker.
    IF wModus = "SLETT" THEN
        RETURN "OK".
    ASSIGN 
          wInputRecid   = wNesteRecid
          wModus         = "ENDRE".
    FIND Butiker NO-LOCK WHERE RECID(Butiker) = wInputRecid.
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
    BUTTON-SokBut-2:sensitive = wAktivFlip <> 1
    BUTTON-SokBut:SENSITIVE IN FRAME FRAME-1  = NOT lKjButFinns.
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
  IF VALID-HANDLE(hSkjema) THEN
      APPLY "CLOSE" TO hSkjema.
  ASSIGN
    wSort = int(ENTRY(1,CB-Sort:screen-value IN FRAME DEFAULT-FRAME,":")).
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
              FIND NEXT Butiker NO-LOCK USE-INDEX ButikIn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND LAST Butiker NO-LOCK USE-INDEX ButikIn  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV Butiker NO-LOCK USE-INDEX ButikIn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND FIRST Butiker NO-LOCK USE-INDEX ButikIn  NO-ERROR.
          END.
        END.
      WHEN 2 THEN
        DO:
          IF wRettning = "Next" THEN DO:
              FIND NEXT Butiker NO-LOCK USE-INDEX ButNamn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND LAST Butiker NO-LOCK USE-INDEX ButNamn  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV Butiker NO-LOCK USE-INDEX ButNamn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND FIRST Butiker NO-LOCK USE-INDEX ButNamn  NO-ERROR.
          END.
        END.
    END CASE.  
    IF AVAILABLE Butiker THEN
      DO:
        ASSIGN
          wInputRecid = RECID(Butiker).
        RUN InitCB2.
        RUN VisPost.
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

OCXFile = SEARCH( "w-vbutiker.wrx":U ).
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
ELSE MESSAGE "w-vbutiker.wrx":U SKIP(1)
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
        IF VALID-HANDLE(tmpChild.wChild) THEN DO:
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
  DISPLAY CB-Sort FILL-IN-Butik FILL-IN-ButNamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  ENABLE RECT-27 RECT-28 B-KonvTbl CB-Sort BUTTON-Angre BUTTON-Kopier 
         BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev BUTTON-Slett Btn_Help 
         BUTTON-Ok 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Securmark FILL-IN-Poststed FI-Plukklager FILL-IN-LevPoststed 
          FI-MvaTxtFG FI-MvaTxtPG T-Kampanje T-VPI T-Statistikk 
          FILL-IN-EndretInfo FILL-IN-21 FILL-IN-22 
      WITH FRAME FRAME-1 IN WINDOW C-ButikKort.
  IF AVAILABLE Butiker THEN 
    DISPLAY Butiker.webbutik Butiker.Butik Butiker.ButNamn Butiker.KortNavn 
          Butiker.EksterntId Butiker.BuKon Butiker.OrganisasjonsNr Butiker.BuAdr 
          Butiker.BuPadr Butiker.BuPonr Butiker.ButLand Butiker.BuTel 
          Butiker.Telefaks Butiker.ButFirmanavn Butiker.LevKontakt 
          Butiker.LevAdresse1 Butiker.LevAdresse2 Butiker.LevPostBoks 
          Butiker.LevPostNr Butiker.LevTelefon Butiker.ProfilNr 
          Butiker.LevMerknad Butiker.ApningsDato Butiker.Sentrallager 
          Butiker.NedlagtDato Butiker.ButMalNr Butiker.KommisjonsdatoStart 
          Butiker.Minusbutikk Butiker.DirFakturautskrift Butiker.Fakturaskriver 
          Butiker.LanButikk Butiker.FakturaLayout Butiker.KundeNr 
          Butiker.IntFaktOverforing Butiker.FaktKopiRappSkriver 
          Butiker.clButikkNr Butiker.PlukkButikk Butiker.PrioPlukket 
          Butiker.Fakturagebyr Butiker.Purregebyr Butiker.StdVeksel 
          Butiker.harButikksystem Butiker.FakturaKopi Butiker.FaktTekstNr 
          Butiker.FGMomsKod Butiker.PGMomsKod Butiker.KasseNr 
          Butiker.EtikettPrinterKOrdre Butiker.PostEtikettPrinter 
          Butiker.BELayout Butiker.BEPrinter Butiker.RAPPrinter Butiker.VaarREf 
          Butiker.BankKonto Butiker.Postgiro Butiker.ePostAdresse 
          Butiker.URLAdresse Butiker.BEAktiv Butiker.BETerminalklient 
          Butiker.FalckMedlNr Butiker.EODRapporter Butiker.EODFinansrapport 
          Butiker.EODBokforingsbilag Butiker.EDOJournal 
      WITH FRAME FRAME-1 IN WINDOW C-ButikKort.
  IF AVAILABLE Prisprofil THEN 
    DISPLAY Prisprofil.KortNavn Prisprofil.Beskrivelse 
      WITH FRAME FRAME-1 IN WINDOW C-ButikKort.
  ENABLE Butiker.webbutik Butiker.ButNamn Butiker.KortNavn Butiker.EksterntId 
         Butiker.BuKon Butiker.OrganisasjonsNr BUTTON-SokPostEtikettSkriver 
         Butiker.BuAdr Butiker.BuPadr Butiker.BuPonr Butiker.ButLand 
         Butiker.BuTel Butiker.Telefaks Butiker.ButFirmanavn Butiker.LevKontakt 
         Butiker.LevAdresse1 B-EtiLayout Butiker.LevAdresse2 
         Butiker.LevPostBoks Butiker.LevPostNr Butiker.LevTelefon 
         Butiker.ProfilNr Butiker.LevMerknad Butiker.ApningsDato 
         Butiker.Sentrallager Butiker.NedlagtDato Butiker.ButMalNr 
         BUTTON-SokFakturatekst Butiker.KommisjonsdatoStart Butiker.Minusbutikk 
         Butiker.DirFakturautskrift Butiker.LanButikk Butiker.FakturaLayout 
         Butiker.KundeNr Butiker.IntFaktOverforing BUTTON-SokSkriverEti 
         Butiker.FaktKopiRappSkriver Butiker.PlukkButikk Butiker.PrioPlukket 
         Butiker.Fakturagebyr Butiker.Purregebyr Butiker.StdVeksel 
         Butiker.harButikksystem Butiker.FakturaKopi Butiker.FaktTekstNr 
         Butiker.FGMomsKod Butiker.PGMomsKod BUTTON-SokSkriverRapp 
         Butiker.KasseNr Butiker.EtikettPrinterKOrdre 
         Butiker.PostEtikettPrinter Butiker.BEPrinter Butiker.RAPPrinter 
         BROWSE-ApnSkjema Butiker.VaarREf B-SokPlukk-2 Butiker.BankKonto 
         Butiker.Postgiro Butiker.ePostAdresse Butiker.URLAdresse 
         Butiker.BEAktiv Butiker.BETerminalklient Butiker.FalckMedlNr 
         Butiker.EODRapporter BUTTON-SokSkriverRapp-2 Butiker.EODFinansrapport 
         T-Kampanje T-VPI Butiker.EODBokforingsbilag Butiker.EDOJournal 
         T-Statistikk B-EndreSkjema B-NySkjema B-SlettSkjema B-SokMvaFG 
         B-SokMvaPG B-SokPlukk BUTTON-SokBut BUTTON-Sokeknapp 
         BUTTON-Sokeknapp-2 BUTTON-Sokeknapp-3 BUTTON-SokSkriver B-SokKunde 
         FILL-IN-EndretInfo FILL-IN-21 FILL-IN-22 BUTTON-SokRapportSkriver 
         BUTTON-SokPostEtikettSkriver-2 RECT-46 RECT-55 RECT-56 RECT-57 
      WITH FRAME FRAME-1 IN WINDOW C-ButikKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-1}
  DISPLAY FILL-IN-1 
      WITH FRAME FRAME-2 IN WINDOW C-ButikKort.
  ENABLE RECT-47 B-butikkfsg FILL-IN-1 
      WITH FRAME FRAME-2 IN WINDOW C-ButikKort.
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
  DEFINE INPUT  PARAMETER cIOForsListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEF VAR piKassererId AS INT NO-UNDO.
  FOR EACH butikkforsalj WHERE butikkforsalj.Butik = Butiker.Butik AND
                           NOT CAN-DO(cIOForsListe,STRING(butikkforsalj.ForsNr)):
      DELETE butikkforsalj.
  END.
  MESSAGE cIOForsListe
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  DO iCount = 1 TO NUM-ENTRIES(cIOForsListe):
      IF NOT CAN-FIND(butikkforsalj WHERE butikkforsalj.Butik = Butiker.Butik AND
                       butikkforsalj.ForsNr = INT(ENTRY(iCount,cIOForsListe))) THEN DO:
          CREATE butikkforsalj.
          ASSIGN butikkforsalj.Butik = Butiker.Butik
                 butikkforsalj.ForsNr = INT(ENTRY(iCount,cIOForsListe)).
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
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      ASSIGN cListItemPairs = "Ukjent,0".
      FOR EACH SysPara WHERE SysPara.SysHId = 19 AND
                             SysPara.SysGr  = 12 NO-LOCK: 
          ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    SysPara.Beskrivelse + "," + STRING(SysPara.ParaNr).
      END.
      ASSIGN Butiker.FakturaLayout:LIST-ITEM-PAIRS = cListItemPairs.

      ASSIGN cListItemPairs = "Ikke satt,0".
      FOR EACH ButMalHode NO-LOCK: 
          ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    ButMalHode.ButMalNavn + "," + STRING(butMalHode.ButMalNr).
      END.
      ASSIGN Butiker.ButMalNr:LIST-ITEM-PAIRS = cListItemPairs.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB2 C-ButikKort 
PROCEDURE InitCB2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      
      IF NOT AVAILABLE Butiker THEN
      DO:
          MESSAGE "Ingen butikk tilgjengelig."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
      ELSE DO:
          ASSIGN cListItemPairs = "Ingen kasse definert,0".
          FOR EACH Kasse WHERE 
              Kasse.ButikkNr = Butiker.Butik AND
              Kasse.GruppeNr = 1 /*AND
              Kasse.ModellNr = 10*/ NO-LOCK: 
              ASSIGN 
                  cListItemPairs = cListItemPairs + 
                                   (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                   STRING(Kasse.KasseNr) + " " + Kasse.Navn + "," + STRING(Kasse.KasseNr).
          END.
          ASSIGN 
              Butiker.KasseNr:LIST-ITEM-PAIRS = cListItemPairs
              Butiker.KasseNr:SCREEN-VALUE    = IF Butiker.KasseNr = ? THEN '0' ELSE STRING(Butiker.KasseNr)
              .
      END.
      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-ButikKort 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBekreft AS INT NO-UNDO.
  DEF VAR ipStatus AS CHAR INIT "AVBRYT" NO-UNDO.
  DEF VAR bSvar    AS LOG NO-UNDO.

  DO WITH FRAME FRAME-1:
    IF wModus = "NY" THEN DO:
      /* Sjekker input */
      IF INPUT Butiker.Butik = 0 THEN
        DO:
          MESSAGE "Butikknummer må være større enn 0"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".
          APPLY "ENTRY":U TO Butiker.Butik.
          RETURN ipStatus.
        END.
      IF CAN-FIND(Butiker WHERE
                  Butiker.Butik = int(Butiker.Butik:screen-value)) THEN
        DO:
          MESSAGE "Butikk finnes allerede med nr:" Butiker.Butik:screen-value
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Butiker.Butik.
          RETURN ipStatus.
        END.
    END.
    IF NOT CAN-FIND(Post NO-LOCK WHERE 
         Post.PostNr = INPUT Butiker.BuPoNr) THEN
        DO:
          MESSAGE "Postnr finnes ikke"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Butiker.BuPoNr.
          RETURN ipStatus.
        END.
    IF NOT CAN-FIND(Prisprofil NO-LOCK WHERE 
                       Prisprofil.ProfilNr = INPUT Butiker.ProfilNr) THEN
        DO:
          MESSAGE "Prisprofil finnes ikke"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Butiker.ProfilNr.
          RETURN ipStatus.
        END.
    IF NOT CAN-FIND(Post NO-LOCK WHERE 
         Post.PostNr = INPUT Butiker.LevPostNr) THEN
        DO:
          MESSAGE "Postnr finnes ikke"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Butiker.LevPostNr.
          RETURN ipStatus.
        END.
    IF NOT CAN-FIND(Moms WHERE 
         Moms.MomsKod = INPUT Butiker.FGMomsKod) THEN
        DO:
          MESSAGE "Moms finnes ikke"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Butiker.FGMomsKod.
          RETURN ipStatus.
        END.
    IF NOT CAN-FIND(Moms WHERE 
         Moms.MomsKod = INPUT Butiker.PGMomsKod) THEN
        DO:
          MESSAGE "Moms finnes ikke"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Butiker.PGMomsKod.
          RETURN ipStatus.
        END.
    IF INPUT Butiker.FaktTekstNr <> 0 AND NOT CAN-FIND(FakturaTekst WHERE 
         FakturaTekst.FaktTekstNr = INPUT Butiker.FaktTekstNr) THEN
        DO:
          MESSAGE "Fakturatekst finnes ikke"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Butiker.FaktTekstNr.
          RETURN ipStatus.
        END.
    IF INPUT Butiker.BEAktiv:CHECKED AND (Butiker.BELayout:SCREEN-VALUE = "0" OR
             Butiker.BEPrinter:SCREEN-VALUE = "") THEN DO:
        MESSAGE "Registrer etikettlayout og etikettskriver"
            VIEW-AS ALERT-BOX TITLE "Lagringsfeil".
        RETURN ipstatus.
    END.
    IF Butiker.EODRapporter:CHECKED = TRUE THEN
    DO:
        IF INPUT Butiker.RapPrinter = "" THEN
        DO:
            MESSAGE "Når EOD rapporter er enablet, må rapportskriver også angis."
            VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
            APPLY "ENTRY":U TO Butiker.FaktTekstNr.
            RETURN ipStatus.
        END.
    END.
    IF INPUT Butiker.Plukkbutik > 0 THEN
    PLUKKLAGER:
    DO:
       IF NOT CAN-FIND(bButiker WHERE
                       bButiker.Butik = INPUT Butiker.Plukkbutik) THEN
       DO:
           MESSAGE 
           "Ugyldig plukkbutikk angitt." 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN ipStatus.
       END.
       IF Butiker.Butik = INPUT Butiker.Plukkbutik THEN
       DO:
           MESSAGE 
           "Plukkbutikk og butikk kan ikke være samme butikk." 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN ipStatus.
       END.
       IF CAN-FIND(FIRST bButiker WHERE
                   bButiker.PlukkButikk = Butiker.Butik AND
                   bButiker.Butik       <> Butiker.Butik) THEN
       DO:
           MESSAGE 
           "Denne butikken er oppsatt som plukkbutikk på en annen butikk. " SKIP
           "En butikk kan ikke være plukkbutikk samtidig som den settes opp med plukkbutikk."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN ipStatus.
       END.
    END. /* PLUKKLAGER */

    IF T-Statistikk:CHECKED = FALSE THEN
    DO:
        bSvar = FALSE.
        MESSAGE "Det er ikke markert for oppdatering av statistikk." SKIP
                "Er dette riktig?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bSvar .
        IF bSvar = FALSE THEN
            RETURN ipStatus.
    END.

    LAGRE_BUTIKER:
      DO TRANSACTION:
        IF wModus = "NY" THEN
          DO:
            RELEASE Butiker. /* Slipper gammel post. */
            CREATE Butiker.
            ASSIGN
               wInputRecid = RECID(Butiker)
               Butiker.Butik.
          END.
        ELSE DO:
          FIND CURRENT Butiker EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF LOCKED Butiker THEN
            DO:
                MESSAGE "Butikken oppdateres fra en annen terminal" SKIP
                  "Forsök å lagre en gang til" VIEW-AS ALERT-BOX 
               WARNING TITLE "Lagringsfeil".
           RETURN NO-APPLY ipStatus.
          END.
        END.
        ASSIGN
             wModus       = "ENDRE"
             Butiker.Butik:SENSITIVE = FALSE
             Butiker.ButNamn
             Butiker.KortNavn
             Butiker.EksterntId
             Butiker.BuKon
             Butiker.BuAdr
             Butiker.BuPadr
             Butiker.BuPonr
             Butiker.BuTel
             Butiker.ProfilNr
             Butiker.OrganisasjonsNr
             Butiker.LevMerknad
             Butiker.LevKontakt
             Butiker.LevAdresse1
             Butiker.LevAdresse2
             Butiker.LevPostBoks
             Butiker.LevPostNr
             Butiker.LevTelefon
             Butiker.Sentrallager
             Butiker.LanButikk
             Butiker.harButikksystem
             butiker.ApningsDato
             Butiker.NedlagtDato
             Butiker.StdVeksel
             Butiker.FalckMedlNr
             Butiker.FakturaGebyr
             Butiker.PlukkButik
             Butiker.ButLand
             Butiker.Telefaks
             Butiker.Purregebyr
             Butiker.VaarRef
             Butiker.BankKonto
             Butiker.PostGiro
             Butiker.ePostAdresse
             Butiker.URLAdresse
             Butiker.FGMomsKod
             Butiker.PGMomsKod
             Butiker.DirFakturautskrift
             Butiker.Fakturaskriver
             Butiker.FakturaKopi
             Butiker.FakturaLayout
             Butiker.ButMalNr
             Butiker.FaktTekstNr
             Butiker.Minusbutikk
             Butiker.BEAktiv
             Butiker.BELayout
             Butiker.BEPrinter
             Butiker.BETerminalklient
             Butiker.IntFaktOverforing
             Butiker.KundeNr
             Butiker.RAPPrinter
             Butiker.EODRapporter
             Butiker.EODFinansrapport
             Butiker.EODBokforingsbilag
             Butiker.EDOJournal
             Butiker.FaktKopiRappSkriver
             Butiker.KasseNr
             Butiker.EtikettPrinterKOrdre
             Butiker.PostEtikettPrinter
             Butiker.KommisjonsdatoStart
             Butiker.PrioPlukket
             Butiker.CL
             Butiker.ButFirmanavn
             Butiker.Kampanje = IF T-Kampanje:SCREEN-VALUE = "no"
                                  THEN 0
                                  ELSE 1
             Butiker.VPI      = IF T-VPI:SCREEN-VALUE = "no"
                                  THEN 0
                                  ELSE 1
             Butiker.StatistikkOppdatering = IF T-Statistikk:SCREEN-VALUE = "no"
                                               THEN FALSE
                                               ELSE TRUE
             Butiker.webbutik
             .
         FIND CURRENT Butiker NO-LOCK.
         RUN VisPost.
         RUN BUTTONEnaDis.
      END.
  END.
  RETURN "OK".
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
    ASSIGN Butiker.Butik:MODIFIED = wModified
           Butiker.ButNamn:MODIFIED = wModified
           Butiker.BuKon:MODIFIED = wModified
           Butiker.BuAdr:MODIFIED = wModified
           Butiker.BuPadr:MODIFIED = wModified
           Butiker.BuPonr:MODIFIED = wModified
           Butiker.BuTel:MODIFIED = wModified
           Butiker.OrganisasjonsNr:MODIFIED = wModified
           Butiker.ProfilNr:MODIFIED = wModified
           Butiker.LevMerknad:MODIFIED = wModified
           Butiker.LevKontakt:MODIFIED = wModified
           Butiker.LevAdresse1:MODIFIED = wModified
           Butiker.LevAdresse2:MODIFIED = wModified
           Butiker.LevPostBoks:MODIFIED = wModified
           Butiker.LevPostNr:MODIFIED = wModified
           Butiker.LevTelefon:MODIFIED = wModified.
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
  IF VALID-HANDLE(wSubWin) THEN
    DELETE PROCEDURE wSubWin NO-ERROR.
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
  IF AVAILABLE Butiker THEN
    DO:
      ASSIGN
        Fill-In-EndretInfo = "Opprettet " + 
                             (IF Butiker.RegistrertDato <> ? 
                               THEN STRING(Butiker.RegistrertDato)
                               ELSE "        ") + " " +
                             (IF Butiker.RegistrertTid <> 0
                               THEN STRING(Butiker.RegistrertTid,"HH:MM:SS")
                               ELSE "        ") + " av " + 
                             Butiker.RegistrertAv + "    Endret " +
                             (IF Butiker.EDato <> ?
                               THEN STRING(Butiker.EDato)
                               ELSE "        ") + " " +
                             (IF Butiker.ETid <> 0
                               THEN STRING(Butiker.ETid,"HH:MM:SS")
                               ELSE "        ") + " av " +
                             Butiker.BrukerId.
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
    ASSIGN FILL-IN-Butik:SCREEN-VALUE = STRING(Butiker.Butik)
           FILL-IN-ButNamn:SCREEN-VALUE = Butiker.ButNamn.
  END.
  FIND PrisProfil OF Butiker NO-LOCK NO-ERROR.
  FIND Post WHERE Post.PostNr = Butiker.BuPoNr NO-LOCK NO-ERROR.
  ASSIGN wButPoststed = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  FIND Post WHERE Post.PostNr = Butiker.LevPostNr NO-LOCK NO-ERROR.
  ASSIGN wLevPoststed = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  FIND Moms WHERE Moms.MomsKod = Butiker.FGMomsKod NO-LOCK NO-ERROR.
  ASSIGN FI-MvaTxtFG = IF AVAIL Moms THEN Moms.Beskrivelse ELSE "Ukjent".
  FIND Moms WHERE Moms.MomsKod = Butiker.PGMomsKod NO-LOCK NO-ERROR.
  ASSIGN FI-MvaTxtPG = IF AVAIL Moms THEN Moms.Beskrivelse ELSE "Ukjent".
  IF Butiker.Plukkbutik > 0 THEN
  DO:
      FIND bButiker NO-LOCK WHERE
          bButiker.Butik = Butiker.PlukkButik NO-ERROR.
      IF AVAILABLE bButiker THEN
          FI-Plukklager = bButiker.KortNAvn.
      ELSE
          FI-Plukklager = "".
  END.
  ELSE ASSIGN
      FI-PlukkLager = "".

  DO WITH FRAME FRAME-1:
     ASSIGN
         T-Kampanje = IF Butiker.Kampanje = 0
                        THEN FALSE
                        ELSE TRUE
         T-VPI      = IF Butiker.VPI      = 0
                        THEN FALSE
                        ELSE TRUE
         T-Statistikk = Butiker.StatistikkOppdatering
         .
     DISPLAY 
         T-Kampanje
         T-VPI
         T-Statistikk
         Butiker.Butik 
         Butiker.ButNamn
         Butiker.BuKon
         Butiker.BuAdr
         Butiker.BuPadr
         wButPoststed @ FILL-IN-Poststed
         Butiker.BuPonr
         Butiker.BuTel
         Butiker.ProfilNr
         Butiker.KortNavn
         Butiker.EksterntId
         Butiker.OrganisasjonsNr
         (IF AVAIL PrisProfil THEN Prisprofil.Beskrivelse ELSE "") @ Prisprofil.Beskrivelse 
         (IF AVAIL PrisProfil THEN Prisprofil.KortNavn ELSE "") @ Prisprofil.KortNavn 
         Butiker.LevMerknad
         Butiker.LevKontakt
         Butiker.LevAdresse1
         Butiker.LevAdresse2
         Butiker.LevPostBoks
         Butiker.LevPostNr
         wLevPoststed @ FILL-IN-LevPoststed
         Butiker.LevTelefon
         Butiker.Sentrallager
         Butiker.LanButikk
         Butiker.harButikksystem
         Butiker.ApningsDato
         Butiker.NedlagtDato
         Butiker.StdVeksel
         Butiker.FalckMedlNr
         Butiker.FakturaGebyr
         Butiker.clButik
         Butiker.PlukkButik
         FI-Plukklager
         Butiker.ButLand
         Butiker.Telefaks
         Butiker.Purregebyr
         Butiker.VaarRef
         Butiker.BankKonto
         Butiker.PostGiro
         Butiker.ePostAdresse
         Butiker.URLAdresse
         Butiker.FGMomsKod
         Butiker.PGMomsKod
         Butiker.ButFirmanavn
         FI-MvaTxtFG
         FI-MvaTxtPG
         Butiker.DirFakturautskrift 
         Butiker.Fakturaskriver
         Butiker.FakturaKopi
         Butiker.FakturaLayout
         Butiker.ButMalNr
         Butiker.FaktTekstNr
         Butiker.MinusButikk
         Butiker.BEAktiv
         Butiker.BELayout
         Butiker.BEPrinter
         Butiker.BETerminalklient
         Butiker.IntFaktOverforing
         Butiker.KundeNr
         Butiker.RAPPrinter
         Butiker.PrioPlukket
         Butiker.EODRapporter
         Butiker.EODFinansrapport
         Butiker.EODBokforingsbilag
         Butiker.EDOJournal
         Butiker.FaktKopiRappSkriver
         Butiker.KasseNr
         Butiker.EtikettPrinterKOrdre
         Butiker.PostEtikettPrinter
         Butiker.KommisjonsdatoStart
         Butiker.webbutik
         .
     IF Butiker.EksterntId:SCREEN-VALUE BEGINS 'Ekstern id' THEN Butiker.EksterntId:SCREEN-VALUE = ''.
  END.
  ASSIGN
      Butiker.EODFinansrapport:SENSITIVE   = Butiker.EODRapporter:CHECKED
      Butiker.EODBokforingsbilag:SENSITIVE = Butiker.EODRapporter:CHECKED
      Butiker.EDOJournal:SENSITIVE         = Butiker.EODRapporter:CHECKED
      .
  RUN VisEndretInfo.
  RUN SettModified(FALSE).
  IF wAktivFlip = 1 THEN
     APPLY "ENTRY" TO Butiker.ButNamn IN FRAME FRAME-1.
  APPLY "VALUE-CHANGED" TO Butiker.BEPrinter.
  {&OPEN-QUERY-BROWSE-ApnSkjema}
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
    RETURN IF Butiker.Butik:MODIFIED = TRUE OR
              Butiker.ButNamn:MODIFIED = TRUE OR
              Butiker.BuKon:MODIFIED = TRUE OR
              Butiker.BuAdr:MODIFIED = TRUE OR
              Butiker.BuPadr:MODIFIED = TRUE OR
              Butiker.BuPonr:MODIFIED = TRUE OR
              Butiker.BuTel:MODIFIED = TRUE OR
              Butiker.ProfilNr:MODIFIED = TRUE OR
              Butiker.OrganisasjonsNr:MODIFIED = TRUE OR
              Butiker.LevMerknad:MODIFIED = TRUE OR
              Butiker.LevKontakt:MODIFIED = TRUE OR
              Butiker.LevAdresse1:MODIFIED = TRUE OR
              Butiker.LevAdresse2:MODIFIED = TRUE OR
              Butiker.LevPostBoks:MODIFIED = TRUE OR
              Butiker.LevPostNr:MODIFIED = TRUE OR
              Butiker.LevTelefon:MODIFIED = TRUE THEN TRUE ELSE FALSE.
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

