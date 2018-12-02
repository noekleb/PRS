&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-LevBasKort


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tmpSysPara NO-UNDO LIKE SysPara.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-LevBasKort 
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
  DEF VAR wInputRecid  AS RECID NO-UNDO.
  DEF VAR wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
  ASSIGN 
    wModus = "ENDRE". /* Default */
 FIND FIRST LevBas NO-LOCK NO-ERROR.
  IF AVAILABLE LevBas THEN
    ASSIGN wInputRecid = recid(LevBas).
&ELSE
  DEF INPUT PARAMETER wInputRecid AS RECID NO-UNDO.
  DEF INPUT PARAMETER wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
&ENDIF

/*
  DEF VAR wInputRecid  AS RECID NO-UNDO.
  DEF VAR wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
  ASSIGN 
    wModus = "ENDRE". /* Default */

FIND FIRST LevBas NO-LOCK NO-ERROR.
  IF AVAILABLE LevBas THEN
    ASSIGN wInputRecid = recid(LevBas).
*/

/* Local Variable Definitions ---                                       */
DEF VAR wNyVg              AS INT    NO-UNDO.
DEF VAR wOk                AS LOG    NO-UNDO.
DEF VAR wOldRecid          AS RECID  NO-UNDO.
DEF VAR hHandle            AS HANDLE NO-UNDO.
DEF VAR hLabel             AS HANDLE NO-UNDO.
DEF VAR wBildNr            AS INT    NO-UNDO.
DEFINE VARIABLE bHk    AS LOG NO-UNDO.
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
DEF VAR wSubWin1           AS HANDLE NO-UNDO.
DEF VAR wSubWin2           AS HANDLE NO-UNDO.
DEF VAR wSubWin3           AS HANDLE NO-UNDO.
DEF VAR wSubWin4           AS HANDLE NO-UNDO.
DEF VAR wSubWin5           AS HANDLE NO-UNDO.
DEF VAR wLapTop            AS LOG    NO-UNDO.
DEF VAR wBekreft           AS LOG    NO-UNDO.
DEF VAR wFeil              AS LOG    NO-UNDO.
DEF VAR cTabell            AS CHAR   NO-UNDO.
DEF VAR cTekst             AS CHAR   NO-UNDO.
DEF VAR iLavesteNr         AS INT    NO-UNDO.
DEF VAR cDivInfo           AS CHAR   NO-UNDO.
DEFINE VARIABLE bSperrNy AS LOG NO-UNDO.

/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR chTabs            AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab             AS COM-HANDLE NO-UNDO.
DEFINE VAR chTab1Side        AS COM-HANDLE NO-UNDO.
DEFINE VAR wAktivFlip        AS INT        INIT 1 NO-UNDO. /* !! parameter !! */
DEFINE VAR wAktivFrame       AS INTE INIT  1.
DEFINE VAR wTabTekst         AS CHAR INIT "Vedlikehold,Sortiment,Bestillinger,Ordre,Oppsett ordre" NO-UNDO.
DEFINE VAR wTabHnd           AS HANDLE EXTENT 5 NO-UNDO.
DEFINE VAR wBrowseHandle     AS HANDLE NO-UNDO.
/* Buffere */
DEF BUFFER bLevBas FOR LevBas.
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
&Scoped-define BROWSE-NAME B-LevKontakt

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LevKontakt tmpSysPara

/* Definitions for BROWSE B-LevKontakt                                  */
&Scoped-define FIELDS-IN-QUERY-B-LevKontakt LevKontakt.KontNr ~
LevKontakt.Navn LevKontakt.Telefon LevKontakt.Mobiltelefon ~
LevKontakt.E_MailKontakt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B-LevKontakt 
&Scoped-define QUERY-STRING-B-LevKontakt FOR EACH LevKontakt ~
      WHERE LevKontakt.LevNr = input FILL-IN-levnr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B-LevKontakt OPEN QUERY B-LevKontakt FOR EACH LevKontakt ~
      WHERE LevKontakt.LevNr = input FILL-IN-levnr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B-LevKontakt LevKontakt
&Scoped-define FIRST-TABLE-IN-QUERY-B-LevKontakt LevKontakt


/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 tmpSysPara.ParaNr ~
tmpSysPara.Parameter1 tmpSysPara.Parameter2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4 tmpSysPara.Parameter2 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-4 tmpSysPara
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-4 tmpSysPara
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH tmpSysPara NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY BROWSE-4 FOR EACH tmpSysPara NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 tmpSysPara
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 tmpSysPara


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-B-LevKontakt}

/* Definitions for FRAME FRAME-7                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-7 ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS LevBas.Notat 
&Scoped-define ENABLED-TABLES LevBas
&Scoped-define FIRST-ENABLED-TABLE LevBas
&Scoped-Define ENABLED-OBJECTS B-LevKontakt BT-LevKontakt B-KonvTbl CB-Sort ~
BUTTON-Angre BUTTON-Kopier BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev ~
BUTTON-Slett Btn_Help BUTTON-Ok RECT-27 RECT-28 
&Scoped-Define DISPLAYED-FIELDS LevBas.Notat 
&Scoped-define DISPLAYED-TABLES LevBas
&Scoped-define FIRST-DISPLAYED-TABLE LevBas
&Scoped-Define DISPLAYED-OBJECTS CB-Sort FILL-IN-levnr FILL-IN-levnamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Modifierad C-LevBasKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-LevBasKort AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON BT-LevKontakt  NO-FOCUS
     LABEL "Lev.kontakt..." 
     SIZE 15 BY 1.1.

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

DEFINE BUTTON BUTTON-SokLev-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL " 2: Lev/Navn" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS " 1: Lev/Nr"," 2: Lev/Navn" 
     DROP-DOWN-LIST
     SIZE 35 BY 1 TOOLTIP "Velg sorteringsordning som skal gjelde for blaing med pil Opp/Ned." NO-UNDO.

DEFINE VARIABLE FILL-IN-levnamn AS CHARACTER FORMAT "x(200)" 
     VIEW-AS FILL-IN 
     SIZE 51.8 BY 1.

DEFINE VARIABLE FILL-IN-levnr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 10.8 BY 1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.2 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 159.4 BY .1.

DEFINE BUTTON btnSokReklPost  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Post-3 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Post1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Post2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLev 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE fi-ReklPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-EndretInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Endret informasjon" 
      VIEW-AS TEXT 
     SIZE 92.8 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 1.48.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 4.52.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59.8 BY 5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59.6 BY 7.91.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 4.52.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 8.86.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59.4 BY 1.33.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 1.33.

DEFINE BUTTON B-Oppdater 
     LABEL "&Oppdater leverandørs artikkler" 
     SIZE 44 BY 1.14.

DEFINE BUTTON BUTTON-SokSprak 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Visningsmaske grunninformasjon" 
      VIEW-AS TEXT 
     SIZE 38 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst2 AS CHARACTER FORMAT "X(256)":U INITIAL "Vedlikehold av tekster" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 16.19.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 16.19.

DEFINE VARIABLE T-1 AS LOGICAL INITIAL no 
     LABEL "Sesong" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-10 AS LOGICAL INITIAL no 
     LABEL "Fargekode" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-11 AS LOGICAL INITIAL no 
     LABEL "Varegruppe" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE T-12 AS LOGICAL INITIAL no 
     LABEL "Bestillingsnummer(e)" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE T-2 AS LOGICAL INITIAL no 
     LABEL "Material" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-3 AS LOGICAL INITIAL no 
     LABEL "Hæl" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-4 AS LOGICAL INITIAL no 
     LABEL "For" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-5 AS LOGICAL INITIAL no 
     LABEL "Overdel" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-6 AS LOGICAL INITIAL no 
     LABEL "Slitesåle" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-7 AS LOGICAL INITIAL no 
     LABEL "Læst" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-8 AS LOGICAL INITIAL no 
     LABEL "Bruksområde" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE T-9 AS LOGICAL INITIAL no 
     LABEL "Notat" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B-LevKontakt FOR 
      LevKontakt SCROLLING.

DEFINE QUERY BROWSE-4 FOR 
      tmpSysPara SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B-LevKontakt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B-LevKontakt C-LevBasKort _STRUCTURED
  QUERY B-LevKontakt NO-LOCK DISPLAY
      LevKontakt.KontNr FORMAT ">>9":U
      LevKontakt.Navn FORMAT "X(40)":U
      LevKontakt.Telefon FORMAT "X(15)":U
      LevKontakt.Mobiltelefon FORMAT "X(15)":U WIDTH 12
      LevKontakt.E_MailKontakt FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 77.6 BY 4.86 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-LevBasKort _STRUCTURED
  QUERY BROWSE-4 NO-LOCK DISPLAY
      tmpSysPara.ParaNr COLUMN-LABEL "Nr" FORMAT ">>>9":U WIDTH 8.2
      tmpSysPara.Parameter1 COLUMN-LABEL "Standardspråk" FORMAT "X(33)":U
      tmpSysPara.Parameter2 COLUMN-LABEL "Valgt språk" FORMAT "X(50)":U
            WIDTH 54.4
  ENABLE
      tmpSysPara.Parameter2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103 BY 14.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-LevKontakt AT ROW 3 COL 82.4
     BT-LevKontakt AT ROW 1.24 COL 135
     LevBas.Notat AT ROW 4.14 COL 13.4 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 68 BY 3.81 NO-TAB-STOP 
     B-KonvTbl AT ROW 1.24 COL 75
     BUTTON-SokLev-2 AT ROW 3 COL 24.6
     CB-Sort AT ROW 1.33 COL 34.2 COLON-ALIGNED NO-LABEL
     FILL-IN-levnr AT ROW 3 COL 11.4 COLON-ALIGNED
     FILL-IN-levnamn AT ROW 3 COL 27.6 COLON-ALIGNED NO-LABEL
     BUTTON-Angre AT ROW 1.33 COL 20.8 NO-TAB-STOP 
     BUTTON-Kopier AT ROW 1.33 COL 11.4 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 1.33 COL 6.8 NO-TAB-STOP 
     BUTTON-Next AT ROW 1.33 COL 30.6 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.33 COL 2 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 26 NO-TAB-STOP 
     BUTTON-Slett AT ROW 1.33 COL 16 NO-TAB-STOP 
     Btn_Help AT ROW 1.33 COL 150.4 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.33 COL 155.4 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.6 BY 26.43.

DEFINE FRAME FRAME-1
     LevBas.levnr AT ROW 1.43 COL 14.4 COLON-ALIGNED
          LABEL "Leverandør" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 10.8 BY 1
     LevBas.levnamn AT ROW 1.43 COL 30.8 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 43.2 BY 1
     LevBas.levadr AT ROW 3.33 COL 14.4 COLON-ALIGNED
          LABEL "Adresse"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     BUTTON-SokLev AT ROW 1.38 COL 27.6
     LevBas.levponr AT ROW 4.33 COL 14.4 COLON-ALIGNED
          LABEL "Postnummer"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     LevBas.levtel AT ROW 5.33 COL 14.4 COLON-ALIGNED
          LABEL "Telefon" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.telefax AT ROW 6.33 COL 14.4 COLON-ALIGNED
          LABEL "Telefaks" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.telex AT ROW 7.33 COL 14.4 COLON-ALIGNED
          LABEL "Mobil" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.E_MailLev AT ROW 8.33 COL 14.4 COLON-ALIGNED
          LABEL "E-Mail"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.levland AT ROW 9.33 COL 14.4 COLON-ALIGNED
          LABEL "Land" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.EgetKundeNrHosLev AT ROW 10.33 COL 14.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.kommentar[1] AT ROW 12.95 COL 14 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.kommentar[2] AT ROW 13.95 COL 14 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.KjedeAvtale AT ROW 3.38 COL 65
          LABEL "&Kjedeavtale"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81 NO-TAB-STOP 
     LevBas.kommentar[3] AT ROW 14.95 COL 14 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.kommentar[4] AT ROW 15.95 COL 14 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     BUTTON-Post-3 AT ROW 16.95 COL 116.4
     LevBas.Rab1% AT ROW 12.95 COL 79 COLON-ALIGNED HELP
          "Avtalt rabatt med denne leverandøren (Suppleringsrabatt)."
          LABEL "Rabatt%"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1 TOOLTIP "Avtalt rabatt med denne leverandøren (Suppleringsrabatt)."
     LevBas.Frakt% AT ROW 13.95 COL 79 COLON-ALIGNED HELP
          "Påslag for å dekke fraktkostnader fra denne leverandøren"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1 TOOLTIP "Påslag for å dekke fraktkostnader fra denne leverandøren"
     LevBas.DivKost% AT ROW 14.95 COL 79 COLON-ALIGNED HELP
          "Kostnader som forventes ved håndtering av varer fra denne lever"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1 TOOLTIP "Kostnader som forventes ved håndtering av varer fra denne leverandøren."
     LevBas.Rab2% AT ROW 15.95 COL 79 COLON-ALIGNED HELP
          "DB% som forventes på varer fra denne leverandøren"
          LABEL "Db%"
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1 TOOLTIP "DB% som forventes på varer fra denne leverandøren"
     LevBas.levkon AT ROW 1.43 COL 112 COLON-ALIGNED
          LABEL "Navn" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SCROLLABLE SIZE 156 BY 17.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     LevBas.koadr AT ROW 3.33 COL 111.8 COLON-ALIGNED
          LABEL "Adresse" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.koponr AT ROW 4.33 COL 111.8 COLON-ALIGNED
          LABEL "Postnummer"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     LevBas.kopadr AT ROW 4.33 COL 128.6 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 25.4 BY 1
     LevBas.levpadr AT ROW 4.33 COL 31 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 25.4 BY 1
     LevBas.kotel AT ROW 5.33 COL 111.8 COLON-ALIGNED
          LABEL "Telefon" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.kotelefax AT ROW 6.33 COL 111.8 COLON-ALIGNED
          LABEL "Telefaks" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.kotelex AT ROW 7.33 COL 111.8 COLON-ALIGNED
          LABEL "Mobil" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     BUTTON-Post2 AT ROW 4.33 COL 125.6
     LevBas.E_MailKontakt AT ROW 8.33 COL 111.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.koland AT ROW 9.33 COL 111.8 COLON-ALIGNED
          LABEL "Land" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.ReklAdresse1 AT ROW 11.95 COL 112 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.ReklAdresse2 AT ROW 12.95 COL 112 COLON-ALIGNED FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.ReklPostBoks AT ROW 13.95 COL 112 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.ReklPostNr AT ROW 14.95 COL 112 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     fi-ReklPostSted AT ROW 14.95 COL 132.4 COLON-ALIGNED NO-LABEL
     LevBas.valkod AT ROW 16.95 COL 105 COLON-ALIGNED
          LABEL "Kode"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     Valuta.ValKurs AT ROW 16.95 COL 125 COLON-ALIGNED
          LABEL "Kurs"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     Valuta.ValDatum AT ROW 16.95 COL 140.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     BUTTON-Post1 AT ROW 4.33 COL 28.2
     FILL-IN-EndretInfo AT ROW 17.43 COL 3 NO-LABEL
     btnSokReklPost AT ROW 14.95 COL 129.8 NO-TAB-STOP 
     "Kontoradresse" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 2.62 COL 3
          FONT 6
     "Kontaktens adresse" VIEW-AS TEXT
          SIZE 27.4 BY .62 AT ROW 2.57 COL 98.6
          FONT 6
     "Merknad" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 12.1 COL 3
          FONT 6
     "Fakturaadresse reklamasjoner" VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 10.76 COL 98
          FONT 6
     "Valuta" VIEW-AS TEXT
          SIZE 14.4 BY .62 AT ROW 16.24 COL 98
          FONT 6
     "Kalkylemal" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 12.19 COL 65
          FONT 6
     RECT-2 AT ROW 16.71 COL 97
     RECT-3 AT ROW 12.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SCROLLABLE SIZE 156 BY 17.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-1
     RECT-4 AT ROW 11.24 COL 97
     RECT-5 AT ROW 2.86 COL 97
     RECT-7 AT ROW 2.86 COL 2
     RECT-8 AT ROW 1.24 COL 97.2
     RECT-9 AT ROW 1.24 COL 2
     RECT-59 AT ROW 12.67 COL 62
     RECT-63 AT ROW 2.91 COL 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SCROLLABLE SIZE 156 BY 17.33.

DEFINE FRAME FRAME-2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.86
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.81
         SIZE 156 BY 17.34.

DEFINE FRAME FRAME-7
     BUTTON-SokSprak AT ROW 2.33 COL 25
     LevBas.Lng AT ROW 2.33 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     sprak.Beskrivelse AT ROW 2.33 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     T-11 AT ROW 2.43 COL 121
     T-12 AT ROW 3.38 COL 121
     BROWSE-4 AT ROW 3.62 COL 4
     T-1 AT ROW 4.33 COL 121
     T-10 AT ROW 5.29 COL 121
     T-2 AT ROW 6.24 COL 121
     T-3 AT ROW 7.19 COL 121
     T-4 AT ROW 8.14 COL 121
     T-5 AT ROW 9.1 COL 121
     T-6 AT ROW 10.05 COL 121
     T-7 AT ROW 11 COL 121
     T-8 AT ROW 11.95 COL 121
     T-9 AT ROW 12.91 COL 121
     B-Oppdater AT ROW 16.71 COL 111
     FI-Tekst2 AT ROW 1.57 COL 2 COLON-ALIGNED NO-LABEL
     FI-Tekst1 AT ROW 1.67 COL 112 COLON-ALIGNED NO-LABEL
     RECT-55 AT ROW 1.95 COL 110
     RECT-56 AT ROW 1.95 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.86
         SIZE 156 BY 17.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tmpSysPara T "?" NO-UNDO SkoTex SysPara
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-LevBasKort ASSIGN
         HIDDEN             = YES
         TITLE              = "Leverandörer"
         HEIGHT             = 26.43
         WIDTH              = 159.6
         MAX-HEIGHT         = 33.71
         MAX-WIDTH          = 204.4
         VIRTUAL-HEIGHT     = 33.71
         VIRTUAL-WIDTH      = 204.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-ArtKort:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-LevBasKort 
/* ************************* Included-Libraries *********************** */

{incl\DevMode.i}
{incl\CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-LevBasKort
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-1:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-2:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-3:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-4:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-5:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-6:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-7:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB B-LevKontakt 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SokLev-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-levnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-levnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-1
   Custom                                                               */
ASSIGN 
       FRAME FRAME-1:HEIGHT           = 17.33
       FRAME FRAME-1:WIDTH            = 156.

/* SETTINGS FOR FILL-IN LevBas.DivKost% IN FRAME FRAME-1
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN LevBas.E_MailLev IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-ReklPostSted IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-EndretInfo IN FRAME FRAME-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN LevBas.Frakt% IN FRAME FRAME-1
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX LevBas.KjedeAvtale IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.koadr IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.koland IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kommentar[1] IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kommentar[2] IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.kommentar[3] IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.kommentar[4] IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.kopadr IN FRAME FRAME-1
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.koponr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.kotel IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kotelefax IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kotelex IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levadr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.levkon IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levland IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.levnr IN FRAME FRAME-1
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN LevBas.levpadr IN FRAME FRAME-1
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levponr IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.levtel IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.Rab1% IN FRAME FRAME-1
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN LevBas.Rab2% IN FRAME FRAME-1
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN LevBas.ReklAdresse2 IN FRAME FRAME-1
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.telefax IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.telex IN FRAME FRAME-1
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Valuta.ValDatum IN FRAME FRAME-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevBas.valkod IN FRAME FRAME-1
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Valuta.ValKurs IN FRAME FRAME-1
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME FRAME-2
                                                                        */
/* SETTINGS FOR FRAME FRAME-3
                                                                        */
/* SETTINGS FOR FRAME FRAME-4
                                                                        */
/* SETTINGS FOR FRAME FRAME-5
                                                                        */
/* SETTINGS FOR FRAME FRAME-6
                                                                        */
/* SETTINGS FOR FRAME FRAME-7
                                                                        */
/* BROWSE-TAB BROWSE-4 T-12 FRAME-7 */
/* SETTINGS FOR FILL-IN sprak.Beskrivelse IN FRAME FRAME-7
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FI-Tekst1 IN FRAME FRAME-7
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst2 IN FRAME FRAME-7
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-LevBasKort)
THEN C-LevBasKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B-LevKontakt
/* Query rebuild information for BROWSE B-LevKontakt
     _TblList          = "skotex.LevKontakt"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "LevKontakt.LevNr = input FILL-IN-levnr"
     _FldNameList[1]   = skotex.LevKontakt.KontNr
     _FldNameList[2]   = skotex.LevKontakt.Navn
     _FldNameList[3]   = skotex.LevKontakt.Telefon
     _FldNameList[4]   > skotex.LevKontakt.Mobiltelefon
"LevKontakt.Mobiltelefon" ? ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = skotex.LevKontakt.E_MailKontakt
     _Query            is OPENED
*/  /* BROWSE B-LevKontakt */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _TblList          = "Temp-Tables.tmpSysPara"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tmpSysPara.ParaNr
"tmpSysPara.ParaNr" "Nr" ? "integer" ? ? ? ? ? ? no ? no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tmpSysPara.Parameter1
"tmpSysPara.Parameter1" "Standardspråk" "X(33)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tmpSysPara.Parameter2
"tmpSysPara.Parameter2" "Valgt språk" ? "character" ? ? ? ? ? ? yes ? no no "54.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-3
/* Query rebuild information for FRAME FRAME-3
     _Query            is NOT OPENED
*/  /* FRAME FRAME-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-4
/* Query rebuild information for FRAME FRAME-4
     _Query            is NOT OPENED
*/  /* FRAME FRAME-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-5
/* Query rebuild information for FRAME FRAME-5
     _Query            is NOT OPENED
*/  /* FRAME FRAME-5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-6
/* Query rebuild information for FRAME FRAME-6
     _Query            is NOT OPENED
*/  /* FRAME FRAME-6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-7
/* Query rebuild information for FRAME FRAME-7
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-7 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TabStrip ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 8.38
       COLUMN          = 1
       HEIGHT          = 19.05
       WIDTH           = 159
       HIDDEN          = no
       SENSITIVE       = yes.
/* TabStrip OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      TabStrip:MOVE-AFTER(FILL-IN-levnamn:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-LevBasKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-LevBasKort C-LevBasKort
ON END-ERROR OF C-LevBasKort /* Leverandörer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-LevBasKort C-LevBasKort
ON WINDOW-CLOSE OF C-LevBasKort /* Leverandörer */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-LevBasKort C-LevBasKort
ON WINDOW-RESIZED OF C-LevBasKort /* Leverandörer */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KonvTbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvTbl C-LevBasKort
ON CHOOSE OF B-KonvTbl IN FRAME DEFAULT-FRAME /* Konverteringstabell... */
DO:
  IF NOT AVAILABLE LevBas THEN
    RETURN.
  RUN d-bimpkonv.w (1500,
                    string(LevBas.LevNr),
                    "Kobling av leverandør"
                    ).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-7
&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater C-LevBasKort
ON CHOOSE OF B-Oppdater IN FRAME FRAME-7 /* Oppdater leverandørs artikkler */
DO:
  IF NOT AVAILABLE LevBas THEN
      RETURN NO-APPLY.
  DO WITH FRAME frame-7:
      cDivInfo = string(T-1:screen-value ) + ',' +
                 string(T-2:screen-value ) + ',' +
                 string(T-3:screen-value ) + ',' +
                 string(T-4:screen-value ) + ',' +
                 string(T-5:screen-value ) + ',' +
                 string(T-6:screen-value ) + ',' +
                 string(T-7:screen-value ) + ',' +
                 string(T-8:screen-value ) + ',' +
                 string(T-9:screen-value ) + ',' +
                 string(T-10:screen-value) + ',' +
                 string(T-11:screen-value) + ',' +
                 string(T-12:screen-value).
  END.
  RUN SettMaskePaArtikkler.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BT-LevKontakt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-LevKontakt C-LevBasKort
ON CHOOSE OF BT-LevKontakt IN FRAME DEFAULT-FRAME /* Lev.kontakt... */
DO:
  IF CAN-FIND(LevBas WHERE
              LevBas.LevNr = INPUT FILL-IN-LevNr) THEN
  DO:
      {&WINDOW-NAME}:SENSITIVE = FALSE.
      RUN levkontakt.w (INPUT INPUT FILL-IN-LevNr).
      {&WINDOW-NAME}:SENSITIVE = TRUE.
      {&OPEN-QUERY-B-LevKontakt}
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME btnSokReklPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokReklPost C-LevBasKort
ON CHOOSE OF btnSokReklPost IN FRAME FRAME-1 /* ... */
OR F10 OF ReklPostNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "PostNr".
  RUN JBoxDLookup.w ("Post;PostNr;Beskrivelse","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ReklPostNr:SCREEN-VALUE = cLookupValue.
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","Post;Beskrivelse","WHERE PostNr = '" + ReklPostNr:SCREEN-VALUE + "'").
    fi-ReklPostSted:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-LevBasKort
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  RUN WinHlp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-LevBasKort
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  IF wModus = "NY" THEN
      ASSIGN LevBas.LevNr:SENSITIVE IN FRAME FRAME-1 = NO
             wModus = "ENDRE".
  RUN VisPost.
  RUN InitSprak (INPUT LevBas.Lng).
  RUN BUTTONEnaDis.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier C-LevBasKort
ON CHOOSE OF BUTTON-Kopier IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
  DO WITH FRAME FRAME-1:
    ASSIGN 
      wModus   = "NY"
      LevBas.LevNr:SENSITIVE = TRUE
      LevBas.LevNr:SCREEN-VALUE = "0".
    RUN BUTTONEnaDis.
    APPLY "ENTRY":U TO LevBas.LevNr.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-LevBasKort
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  RUN LagrePost (0).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  {&OPEN-QUERY-B-LevKontakt}
  RUN BUTTONEnaDis.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-LevBasKort
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  RUN Bytpost("Next").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-LevBasKort
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  IF bSperrNy = FALSE THEN 
    DO:
      MESSAGE 'Det er sperret for opprettelse av leverandører lokalt.' SKIP
              'Ditt system er koblet til et hovedkontor. Nyregistrering av leverandører skal skje der og overføres til din butikk.'
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
  RUN BUTTON-Ny.
  {&OPEN-QUERY-B-LevKontakt}
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-LevBasKort
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  DO WITH FRAME frame-7:
      IF cDivInfo <> string(T-1:screen-value ) + ',' +
                 string(T-2:screen-value ) + ',' +
                 string(T-3:screen-value ) + ',' +
                 string(T-4:screen-value ) + ',' +
                 string(T-5:screen-value ) + ',' +
                 string(T-6:screen-value ) + ',' +
                 string(T-7:screen-value ) + ',' +
                 string(T-8:screen-value ) + ',' +
                 string(T-9:screen-value ) + ',' +
                 string(T-10:screen-value) + ',' +
                 string(T-11:screen-value) + ',' +
                 string(T-12:screen-value)
     THEN DO:
         MESSAGE "Masken for visning av informasjon på ordre er endret, men artikklene er ikke opdpatert." SKIP
                 "Artiklene oppdateres ved å trykke på knappen 'Oppdater leverandørs artikler'." SKIP(1)
                 "Ønsker du alikevel å avslutte?" 
                 UPDATE wBekreft
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO.
         IF wBekreft <> TRUE THEN
             RETURN NO-APPLY.
     END.
  END.

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


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME BUTTON-Post-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Post-3 C-LevBasKort
ON CHOOSE OF BUTTON-Post-3 IN FRAME FRAME-1 /* ... */
OR F10 OF LevBas.ValKod
DO:

  DO WITH FRAME FRAME-ArtInfo:
      cTekst = "ValKod".
      RUN JBoxDLookup.w ("Valuta;ValKod;ValNavn;ValKurs;ValDatum;ValLand", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Valuta NO-LOCK WHERE
        Valuta.ValKod = (cTekst) NO-ERROR.
      IF AVAILABLE Valuta THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            LevBas.ValKod:SCREEN-VALUE   = cTekst
            Valuta.ValKurs:SCREEN-VALUE = string(Valuta.ValKurs)
            Valuta.ValDatum:SCREEN-VALUE = string(Valuta.ValDatum)
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            LevBas.ValKod:SCREEN-VALUE    = ''
            Valuta.ValKurs:SCREEN-VALUE  = ''
            Valuta.ValDatum:SCREEN-VALUE = ''
            .
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Post1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Post1 C-LevBasKort
ON CHOOSE OF BUTTON-Post1 IN FRAME FRAME-1 /* ... */
OR F10 OF LevBas.LevPoNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    LevBas.LevPoNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        LevBas.LevPoNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        LevBas.LevPadr:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Post2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Post2 C-LevBasKort
ON CHOOSE OF BUTTON-Post2 IN FRAME FRAME-1 /* ... */
OR F10 OF LevBas.KoPoNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    LevBas.KoPoNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        LevBas.KoPoNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        LevBas.KoPadr:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-LevBasKort
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  RUN Bytpost("Prev").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-LevBasKort
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  RUN BUTTON-Slett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME BUTTON-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev C-LevBasKort
ON CHOOSE OF BUTTON-SokLev IN FRAME FRAME-1 /* ... */
OR F10 OF LevBas.LevNr
DO:
  DO WITH FRAME FRAME-1:
     DEF VAR wLevNr LIKE LevBas.LevNr NO-UNDO.
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
      ASSIGN wLevNr = IF wModus = "ENDRE" THEN LevBas.LevNr ELSE ?.
      RUN d-blevbas.w (INPUT-OUTPUT wLevNr).
      IF wModus = "NY" THEN DO:
         APPLY "ENTRY" TO LevBas.LevNr IN FRAME FRAME-1.
         RETURN NO-APPLY.
      END.
      IF RETURN-VALUE <> "AVBRYT" THEN DO:
        FIND LevBas WHERE LevBas.LevNr = wLevNr NO-LOCK.
        ASSIGN wInputRecid = RECID(LevBas).
        RUN VisPost.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BUTTON-SokLev-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev-2 C-LevBasKort
ON CHOOSE OF BUTTON-SokLev-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
    APPLY "CHOOSE" TO BUTTON-SokLev IN FRAME FRAME-1.
    IF RETURN-VALUE <> "AVBRYT" THEN DO:
        RUN ByttFrame.
        /*
        if wAktivFlip > 1 and wAktivFlip < 6 then do:
            run ByttObjekt in wSubWin1 (wInputRecid).
        end.
        else if wAktivFlip = 6 then
          do:
            run ByttObjekt in wSubWin1 (string(LevBas.LevNr,"999999")).  
          end.
        */
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-7
&Scoped-define SELF-NAME BUTTON-SokSprak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSprak C-LevBasKort
ON CHOOSE OF BUTTON-SokSprak IN FRAME FRAME-7 /* ... */
OR F10 OF LevBas.Lng    
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = LevBas.Lng
    &Program     = d-bsprak.w
    &ParamType   = "INPUT"
    &ExtraParam  = "'O'"
    &Frame       = FRAME-7
    &PostRun     = "find Sprak no-lock where
                    recid(Sprak) = INT(return-value) no-error.
                    RUN InitSprak (IF AVAILABLE Sprak THEN Sprak.Lng ELSE INPUT LevBas.Lng)."
    &OptDisp     = "Sprak.Lng when available Sprak @ LevBas.Lng 
                    Sprak.Beskrivelse when available Sprak"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME LevBas.koponr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.koponr C-LevBasKort
ON LEAVE OF LevBas.koponr IN FRAME FRAME-1 /* Postnummer */
OR "RETURN":U OF LevBas.KoPoNr OR "TAB":U OF LevBas.KoPoNr
DO:
  DO WITH FRAME frame-1:
    FIND Post NO-LOCK WHERE
      Post.PostNr = INPUT LevBas.KoPoNr NO-ERROR.
    ASSIGN LevBas.KoPAdr:SCREEN-VALUE = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevBas.levponr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.levponr C-LevBasKort
ON LEAVE OF LevBas.levponr IN FRAME FRAME-1 /* Postnummer */
OR "RETURN":U OF LevBas.LevPonr OR "TAB":U OF LevBas.LevPonr
DO:
  DO WITH FRAME frame-1:
      FIND Post NO-LOCK WHERE
           Post.PostNr = INPUT LevBas.LevPonr NO-ERROR.
      ASSIGN LevBas.LevPadr:SCREEN-VALUE = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-7
&Scoped-define SELF-NAME LevBas.Lng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.Lng C-LevBasKort
ON DELETE-CHARACTER OF LevBas.Lng IN FRAME FRAME-7 /* Språkkode */
DO:
  ASSIGN
      LevBas.Lng:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.Lng C-LevBasKort
ON TAB OF LevBas.Lng IN FRAME FRAME-7 /* Språkkode */
OR "return" OF LevBas.Lng
DO:
  IF INPUT LevBas.Lng <> "" THEN
  DO:
    FIND Sprak NO-LOCK WHERE
      Sprak.Lng = INPUT LevBas.Lng NO-ERROR.
    IF AVAILABLE Sprak THEN
    DO:
      /* Setter opp browser for språk */
      RUN InitSprak (Sprak.Lng).
      DISPLAY
          Sprak.Lng WHEN AVAILABLE Sprak @ LevBas.Lng 
          Sprak.Beskrivelse WHEN AVAILABLE Sprak
      WITH FRAME FRAME-7.
    END.
    ELSE DO:
        DISPLAY
            "** Ukjent språk **" @ Sprak.Beskrivelse
        WITH FRAME FRAME-7.
        /* Setter opp browser for språk */
        RUN InitSprak ("DES").
    END.
  END.
  ELSE DO:
      DISPLAY
          "<Standard>" @ Sprak.Beskrivelse
      WITH FRAME FRAME-7.
      RUN InitSprak ("DES").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME LevBas.ReklPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.ReklPostNr C-LevBasKort
ON RETURN OF LevBas.ReklPostNr IN FRAME FRAME-1 /* PostNr */
OR "TAB" OF ReklPostNr
DO:
  IF SELF:MODIFIED THEN
  DO:
      ReklPostNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post",
                                              "WHERE PostNr = '" + ReklPostNr:SCREEN-VALUE + "'","fi-ReklPostSted").  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME TabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TabStrip C-LevBasKort OCX.Click
PROCEDURE TabStrip.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
IF wAktivFlip <> INT(chTabStrip:SelectedItem:Index) THEN DO:
  IF wModus = "NY" THEN DO:
      MESSAGE "Posten må lagres først." VIEW-AS ALERT-BOX INFORMATION TITLE "Feil".
              ASSIGN wFeil = TRUE
                     chTab1Side = chTabs:Item (wAktivFlip BY-VARIANT-POINTER)
                     chTabStrip:SelectedItem = chTab1Side.
  END.
  ELSE IF wAktivFlip = 1 AND Modifierad() THEN DO:
      MESSAGE "Vil du lagre posten?" VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO
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
      APPLY "ENTRY" TO LevBas.levnamn IN FRAME FRAME-1.
  ELSE IF wAktivFlip <> 1 THEN
      APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
 END.
 ELSE DO:
    IF NOT wFeil THEN DO:
     IF wAktivFlip = 1 THEN
            APPLY "ENTRY" TO LevBas.levnamn IN FRAME FRAME-1.
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


&Scoped-define FRAME-NAME FRAME-1
&Scoped-define SELF-NAME LevBas.valkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.valkod C-LevBasKort
ON LEAVE OF LevBas.valkod IN FRAME FRAME-1 /* Kode */
OR "RETURN":U OF LevBas.ValKod OR "TAB":U OF LevBas.ValKod
DO:
  DO WITH FRAME FRAME-1:
    FIND Valuta NO-LOCK WHERE
      Valuta.ValKod = INPUT LevBas.ValKod NO-ERROR.
    IF AVAILABLE Valuta THEN
      DISPLAY Valuta.ValDatum
              Valuta.ValKurs.
    ELSE 
      ASSIGN Valuta.ValDatum:SCREEN-VALUE = ""
             Valuta.ValKurs:SCREEN-VALUE = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME B-LevKontakt
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-LevBasKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

/* Laveste tillatte leverandørnummer ved nyregistrering */
{syspara.i 16 2 1 iLavesteNr INT}
IF iLavesteNr = 0 THEN iLavesteNr = 1.

{syspara.i 16 2 3 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) THEN 
  bSperrNy = TRUE.

{syspara.i 1 1 18 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) THEN 
  bHK = TRUE.


/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "Leverandørregister"
  &PostIClose    = "if valid-handle(wSubWin1) then
                         delete procedure wSubWin1 no-error.
                    if valid-handle(wSubWin2) then
                         delete procedure wSubWin2 no-error.
                    if valid-handle(wSubWin3) then
                         delete procedure wSubWin3 no-error.
                    if valid-handle(wSubWin4) then
                         delete procedure wSubWin4 no-error.
                    if valid-handle(wSubWin5) then
                         delete procedure wSubWin5 no-error.
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

{syspara.i 16 2 3 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) THEN 
  bSperrNy = TRUE.

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

ON ALT-S OF {&WINDOW-NAME} ANYWHERE 
  DO: /* SOKTRIGGER */
    RUN d-levbassok.w.
    IF RETURN-VALUE = "AVBRYT" THEN 
        RETURN NO-APPLY.
    FIND LevBas NO-LOCK WHERE
        LevBas.LevNr = INT(RETURN-VALUE) NO-ERROR.
    IF AVAILABLE LevBas THEN
    DO:
      ASSIGN
        wInputRecid = recid(LevBas).
      RUN VisPost.
      RUN ByttFrame.
    END.
  END. /* SOKTRIGGER */

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

/* Henter parametre for konvertering. */
{syspar2.i 1 2 1000 cTabell}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF wInputRecid = ? AND wModus = "ENDRE" THEN DO:
      MESSAGE "Recid = ? och wModus = 'ENDRE' --> FEL !!" VIEW-AS ALERT-BOX ERROR.
      RETURN "AVBRYT".
  END.
  RUN InitResize. 
  RUN enable_UI.
  {lng.i} /* Oversettelse */

  /* Setter opp browser for språk */
  RUN InitSprak ("DES").
  RUN InitCB.

  ASSIGN {&WINDOW-NAME}:HIDDEN = NO
         CB-Sort:SCREEN-VALUE = ENTRY(2,CB-Sort:LIST-ITEMS)
         wTabHnd[1] = FRAME FRAME-1:HANDLE
         /*wTabHnd[2] = FRAME FRAME-2:HANDLE*/
         wTabHnd[2] = FRAME FRAME-3:HANDLE
         wTabHnd[3] = FRAME FRAME-4:HANDLE
         wTabHnd[4] = FRAME FRAME-5:HANDLE
         wTabHnd[5] = FRAME FRAME-7:HANDLE.
  IF wModus <> "NY" THEN DO:
      FIND LevBas WHERE recid(LevBas) = wInputRecid NO-LOCK NO-ERROR.
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

  /* Direkte oppdatering av priser fra kalkyle. */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  IF wModus <> "NY" THEN
    FIND LevBas NO-LOCK WHERE 
      recid(LevBas) = wInputRecid NO-ERROR.
      
  /* Retur verdi */  
  IF AVAILABLE LevBas AND wModus <> "NY" THEN
    RETURN string(recid(LevBas)).
  ELSE 
    RETURN "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Ny C-LevBasKort 
PROCEDURE BUTTON-Ny :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-1:
     /* Denne sjekken skal ikke gjøres ved NY post. 
     IF Modifierad() THEN DO:
         RUN LagrePost(0).
         IF RETURN-VALUE  = "AVBRYT" THEN
             RETURN NO-APPLY.
     END.
     */
     RUN SettModified(TRUE).
     ASSIGN wModus  = "NY"
            FILL-IN-Levnr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
            FILL-IN-Levnamn:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
            LevBas.LevNr:SENSITIVE  = TRUE
            LevBas.Levnr:SCREEN-VALUE  = "0"
            LevBas.levnamn:SCREEN-VALUE = ""
            LevBas.levkon:SCREEN-VALUE = "" 
            LevBas.levadr:SCREEN-VALUE = ""
            LevBas.koadr:SCREEN-VALUE = ""
            LevBas.levponr:SCREEN-VALUE = ""
            LevBas.levpadr:SCREEN-VALUE = ""
            LevBas.koponr:SCREEN-VALUE = ""
            LevBas.kopadr:SCREEN-VALUE = ""
            LevBas.levtel:SCREEN-VALUE = ""
            LevBas.kotel:SCREEN-VALUE = ""
            LevBas.telefax:SCREEN-VALUE = ""
            LevBas.kotelefax:SCREEN-VALUE = ""
            LevBas.telex:SCREEN-VALUE = ""
            LevBas.kotelex:SCREEN-VALUE = ""
            LevBas.levland:SCREEN-VALUE = ""
            LevBas.koland:SCREEN-VALUE = ""
            LevBas.kommentar[1]:SCREEN-VALUE = ""
            LevBas.kommentar[2]:SCREEN-VALUE = ""
            LevBas.kommentar[3]:SCREEN-VALUE = ""
            LevBas.kommentar[4]:SCREEN-VALUE = ""
            LevBas.Notat:SCREEN-VALUE = ""
            LevBas.valkod:SCREEN-VALUE = ""
            Valuta.valkurs:SCREEN-VALUE = ""
            Valuta.valdatum:SCREEN-VALUE = ""
            LevBas.E_MailLev:SCREEN-VALUE = ""
            LevBas.E_MailKontakt:SCREEN-VALUE = ""
            LevBas.KjedeAvtale:SCREEN-VALUE = IF bHK THEN 'yes' ELSE 'no'
            LevBas.ReklAdresse1:SCREEN-VALUE = ""
            LevBas.ReklAdresse2:SCREEN-VALUE = ""
            LevBas.ReklPostNr:SCREEN-VALUE = ""
            LevBas.ReklPostBoks:SCREEN-VALUE = ""
            fi-ReklPostSted:SCREEN-VALUE = ""
            LevBas.Rab1%:SCREEN-VALUE = ""
            LevBas.Rab2%:SCREEN-VALUE = ""
            LevBas.Frakt%:SCREEN-VALUE = ""
            LevBas.DivKost%:SCREEN-VALUE = ""
         .
    RUN BUTTONEnaDis.
    APPLY "ENTRY" TO LevBas.LevNr. 
  END.
  DO WITH FRAME FRAME-7:
      ASSIGN
          LevBas.Lng:SCREEN-VALUE = ""
          T-1        = FALSE
          T-2        = FALSE
          T-3        = FALSE
          T-4        = FALSE
          T-5        = FALSE
          T-6        = FALSE
          T-7        = FALSE
          T-8        = FALSE
          T-9        = FALSE
          T-10       = FALSE
          T-11       = FALSE
          T-12       = FALSE
          .
      DISPLAY
          T-1       
          T-2       
          T-3       
          T-4       
          T-5       
          T-6       
          T-7       
          T-8       
          T-9       
          T-10
          T-11
          T-12
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Slett C-LevBasKort 
PROCEDURE BUTTON-Slett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wOk AS LOG FORMAT "Ja/Nei" NO-UNDO.
  DEF VAR wNesteRecid AS RECID NO-UNDO.
  
  DO WITH FRAME Frame-1:
    IF (INPUT LevBas.LevNr < iLavesteNr) AND (INPUT LevBas.LevNr <> 0) THEN
    DO:
      MESSAGE "Leverandør med leverandørnummer mindre enn " + STRING(iLavesteNr) + " kan ikke slettes."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
    END.
  END.

  FIND bLevBas NO-LOCK WHERE
    recid(bLevBas) = recid(LevBas) NO-ERROR.
  FIND NEXT bLevBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bLevBas THEN
    FIND FIRST bLevBas.
  IF AVAILABLE bLevBas THEN
    ASSIGN
      wNesteRecid = recid(bLevBas).

  IF CAN-FIND(FIRST ArtBas OF LevBas) THEN
    DO:
      MESSAGE "Det finnes artikkler med denne leverantören. Den kan ikke slettes!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      IF wModus = "SLETT" THEN
          RETURN "AVBRYT".
       ELSE
          RETURN NO-APPLY.
    END.
  IF CAN-FIND(FIRST Ordre OF LevBas) THEN
    DO:
      MESSAGE "Det finnes bestillinger med denne leverantören. Den kan ikke slettes!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  IF CAN-FIND(FIRST Ordre OF LevBas) THEN
    DO:
      MESSAGE "Det finnes artikkler med denne leverantören. Den kan ikke slettes!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
  ASSIGN wOk = FALSE.
  MESSAGE "Skal leverantören slettes?" VIEW-AS ALERT-BOX 
    QUESTION BUTTONS YES-NO
    TITLE "Bekreftelse"
    UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY "AVBRYT".  
  ELSE DO:
    /* Leverandørstatistikk */
    FOR EACH stLinje EXCLUSIVE where
             StLinje.StType = "LEVERAN" and
             StLinje.DataObjekt = string(LevBas.LevNr,"999999"):
       DELETE stLinje.
    END.
    /* Leverandørsortiment */
    FOR EACH LevSort OF LevBas:
        FOR EACH LevSAnt OF LevSort:
            DELETE LevSAnt.
        END.
        DELETE LEvSort.
    END.
    /* Kontaktpersoner. */
    FOR EACH LevKontakt OF LevBas:
        DELETE LevKontakt.
    END.
    FOR EACH LevLager OF LevBas:
        DELETE LEvLager.
    END.
    FIND CURRENT LevBas EXCLUSIVE.
    DELETE LevBas.
    IF wModus = "SLETT" THEN
        RETURN "OK".
    ASSIGN 
          wInputRecid   = wNesteRecid
          wModus         = "ENDRE".
    FIND LevBas NO-LOCK WHERE RECID(LevBas) = wInputRecid.
    RUN VisPost.
    RUN BUTTONEnaDis.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-LevBasKort 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:  
  ASSIGN
    BUTTON-Ny:sensitive       = NOT wModus  = "Ny" AND can-do("1",string(wAktivFlip))
    BUTTON-Kopier:sensitive   = NOT wModus  = "Ny" AND can-do("1",string(wAktivFlip))
    BUTTON-Slett:sensitive    = NOT wModus  = "Ny" AND can-do("1",string(wAktivFlip))
    BUTTON-Prev:sensitive     = NOT wModus  = "Ny"
    BUTTON-Next:sensitive     = NOT wModus  = "Ny"
    BUTTON-Ok:sensitive       = NOT wModus  = "Ny"
    BUTTON-Lagre:sensitive    = can-do("1,7",string(wAktivFlip))
    BUTTON-Angre:sensitive    = can-do("1,7",string(wAktivFlip)) AND wInputRecid <> ?
    BUTTON-SokLev-2:sensitive = NOT can-do("1,7",string(wAktivFlip)).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bytpost C-LevBasKort 
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
              FIND NEXT LevBas NO-LOCK USE-INDEX levin NO-ERROR.    
              IF NOT AVAILABLE LevBas THEN
                 FIND LAST LevBas NO-LOCK USE-INDEX levin  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV LevBas NO-LOCK USE-INDEX levin NO-ERROR.    
              IF NOT AVAILABLE LevBas THEN
                 FIND FIRST LevBas NO-LOCK USE-INDEX levin  NO-ERROR.
          END.
        END.
      WHEN 2 THEN
        DO:
          IF wRettning = "Next" THEN DO:
              FIND NEXT LevBas NO-LOCK USE-INDEX levnamn NO-ERROR.    
              IF NOT AVAILABLE LevBas THEN
                 FIND LAST LevBas NO-LOCK USE-INDEX levnamn  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV LevBas NO-LOCK USE-INDEX levnamn NO-ERROR.    
              IF NOT AVAILABLE LevBas THEN
                 FIND FIRST LevBas NO-LOCK USE-INDEX levnamn  NO-ERROR.
          END.
        END.
    END CASE.  
    IF AVAILABLE LevBas THEN
      DO:
        ASSIGN
          wInputRecid = recid(LevBas).
        RUN VisPost.
        RUN ByttFrame.
        /*
        if wAktivFlip = 1 then 
            run ByttObjekt in wSubWin1 (wInputRecid).
        ELSE if wAktivFlip = 2 then 
            run ByttObjekt in wSubWin2 (wInputRecid).
        ELSE if wAktivFlip = 3 then 
            run ByttObjekt in wSubWin3 (wInputRecid).
        ELSE if wAktivFlip = 4 then 
            run ByttObjekt in wSubWin4 (wInputRecid).
        ELSE if wAktivFlip = 5 then 
            run ByttObjekt in wSubWin5 (wInputRecid).
        else if wAktivFlip = 6 then
          do:
            run ByttObjekt in wSubWin1 (string(LevBas.LevNr,"999999")).  
          end.
        */
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttFrame C-LevBasKort 
PROCEDURE ByttFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FRAME DEFAULT-FRAME:MOVE-TO-TOP().
    wTabHnd[wAktivFlip]:MOVE-TO-TOP().
    IF wAktivFlip = 2 THEN DO:
        IF VALID-HANDLE(wSubWin2) THEN
        DO:
          RUN ByttObjekt IN wSubWin2 (wInputRecid).  
          RUN MoveToTopp IN wSubWin2.
        END.
        ELSE
          RUN w-LevBasLevSort.w PERSISTENT SET wSubWin2 (wInputRecid,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
    END.
    ELSE IF wAktivFlip = 3 THEN DO:
        IF VALID-HANDLE(wSubWin3) THEN
        DO:
          RUN ByttObjekt IN wSubWin3 (wInputRecid).  
          RUN MoveToTopp IN wSubWin3.
        END.
        ELSE
          RUN w-LevBasBestHode.w PERSISTENT SET wSubWin3 (wInputRecid,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
    END.
    ELSE IF wAktivFlip = 4 THEN DO:
        IF VALID-HANDLE(wSubWin4) THEN
        DO:
          RUN ByttObjekt IN wSubWin4 (wInputRecid).  
          RUN MoveToTopp IN wSubWin4.
        END.
        ELSE
          RUN w-LevBasOrdre.w PERSISTENT SET wSubWin4 (wInputRecid,THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE).
    END.
    ELSE IF wAktivFlip = 6 THEN
    DO:
        RUN InitSprak (INPUT LevBas.Lng).
    END.

    /* DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize",""). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-LevBasKort  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-vlevbas.wrx":U ).
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
ELSE MESSAGE "w-vlevbas.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-LevBasKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-LevBasKort  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-LevBasKort)
  THEN DELETE WIDGET C-LevBasKort.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-LevBasKort  _DEFAULT-ENABLE
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
  DISPLAY CB-Sort FILL-IN-levnr FILL-IN-levnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-LevBasKort.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.Notat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-LevBasKort.
  ENABLE B-LevKontakt BT-LevKontakt LevBas.Notat B-KonvTbl CB-Sort BUTTON-Angre 
         BUTTON-Kopier BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev 
         BUTTON-Slett Btn_Help BUTTON-Ok RECT-27 RECT-28 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-LevBasKort.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-ReklPostSted FILL-IN-EndretInfo 
      WITH FRAME FRAME-1 IN WINDOW C-LevBasKort.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnr LevBas.levnamn LevBas.levadr LevBas.levponr LevBas.levtel 
          LevBas.telefax LevBas.telex LevBas.E_MailLev LevBas.levland 
          LevBas.EgetKundeNrHosLev LevBas.kommentar[1] LevBas.kommentar[2] 
          LevBas.KjedeAvtale LevBas.kommentar[3] LevBas.kommentar[4] 
          LevBas.Rab1% LevBas.Frakt% LevBas.DivKost% LevBas.Rab2% LevBas.levkon 
          LevBas.koadr LevBas.koponr LevBas.kopadr LevBas.levpadr LevBas.kotel 
          LevBas.kotelefax LevBas.kotelex LevBas.E_MailKontakt LevBas.koland 
          LevBas.ReklAdresse1 LevBas.ReklAdresse2 LevBas.ReklPostBoks 
          LevBas.ReklPostNr LevBas.valkod 
      WITH FRAME FRAME-1 IN WINDOW C-LevBasKort.
  IF AVAILABLE Valuta THEN 
    DISPLAY Valuta.ValKurs Valuta.ValDatum 
      WITH FRAME FRAME-1 IN WINDOW C-LevBasKort.
  ENABLE LevBas.levnamn LevBas.levadr BUTTON-SokLev LevBas.levponr 
         LevBas.levtel LevBas.telefax LevBas.telex LevBas.E_MailLev 
         LevBas.levland LevBas.EgetKundeNrHosLev LevBas.kommentar[1] 
         LevBas.kommentar[2] LevBas.KjedeAvtale LevBas.kommentar[3] 
         LevBas.kommentar[4] BUTTON-Post-3 LevBas.Rab1% LevBas.Frakt% 
         LevBas.DivKost% LevBas.Rab2% LevBas.levkon LevBas.koadr LevBas.koponr 
         LevBas.kotel LevBas.kotelefax LevBas.kotelex BUTTON-Post2 
         LevBas.E_MailKontakt LevBas.koland LevBas.ReklAdresse1 
         LevBas.ReklAdresse2 LevBas.ReklPostBoks LevBas.ReklPostNr 
         LevBas.valkod BUTTON-Post1 FILL-IN-EndretInfo btnSokReklPost RECT-2 
         RECT-3 RECT-4 RECT-5 RECT-7 RECT-8 RECT-9 RECT-59 RECT-63 
      WITH FRAME FRAME-1 IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-1}
  VIEW FRAME FRAME-3 IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-3}
  VIEW FRAME FRAME-4 IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-4}
  VIEW FRAME FRAME-5 IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-5}
  VIEW FRAME FRAME-6 IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-6}
  VIEW FRAME FRAME-2 IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-2}
  DISPLAY T-11 T-12 T-1 T-10 T-2 T-3 T-4 T-5 T-6 T-7 T-8 T-9 FI-Tekst2 FI-Tekst1 
      WITH FRAME FRAME-7 IN WINDOW C-LevBasKort.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.Lng 
      WITH FRAME FRAME-7 IN WINDOW C-LevBasKort.
  IF AVAILABLE sprak THEN 
    DISPLAY sprak.Beskrivelse 
      WITH FRAME FRAME-7 IN WINDOW C-LevBasKort.
  ENABLE BUTTON-SokSprak RECT-55 RECT-56 LevBas.Lng T-11 T-12 BROWSE-4 T-1 T-10 
         T-2 T-3 T-4 T-5 T-6 T-7 T-8 T-9 B-Oppdater 
      WITH FRAME FRAME-7 IN WINDOW C-LevBasKort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-7}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-LevBasKort 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
    
  ASSIGN
      pcTekst = Tx(" 1: Leverandørnummer, 2: Leverandørnavn",1)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-Controls C-LevBasKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitResize C-LevBasKort 
PROCEDURE InitResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
IF AVAILABLE LevBas THEN
DO:
    IF NOT VALID-HANDLE(wSubWin2) THEN
        RUN w-LevBasLevSort.w PERSISTENT SET wSubWin2 (wInputRecid,?,THIS-PROCEDURE).

    IF NOT VALID-HANDLE(wSubWin3) THEN
        RUN w-LevBasBestHode.w PERSISTENT SET wSubWin3 (wInputRecid,?,THIS-PROCEDURE).

    IF NOT VALID-HANDLE(wSubWin4) THEN
        RUN w-LevBasOrdre.w PERSISTENT SET wSubWin4 (wInputRecid,?,THIS-PROCEDURE).

END.
    
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME DEFAULT-FRAME:HANDLE,
    "notat").

    DYNAMIC-FUNCTION("setNoResizey",THIS-PROCEDURE:CURRENT-WINDOW,FRAME DEFAULT-FRAME:HANDLE,
    "RECT-27,RECT-28,Notat,b-levkontakt").
         

     DYNAMIC-FUNCTION("setnoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME FRAME-1:HANDLE,   
    "kopadr,button-post2,fi-reklpoststed,btnsokreklpost,button-post-3,valkurs,valdatum").         

     DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME FRAME-1:HANDLE,   
    "RECT-9,RECT-8,RECT-7,RECT-63,RECT-5,RECT-3,RECT-59,RECT-4,RECT-2").         

     DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME FRAME-1:HANDLE,   
    "RECT-9,RECT-8,RECT-7,RECT-63,RECT-5,RECT-3,RECT-59,RECT-4,RECT-2").         

     DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW, FRAME FRAME-1:HANDLE,   
     "rect-3,rect-59,rect-4").       

     DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME FRAME-7:HANDLE,"RECT-55").         
     DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME FRAME-7:HANDLE,
                      "RECT-55,t-1,t-2,t-3,t-4,t-5,t-6,t-7,t-8,t-9,t-10,t-11,t-12,fi-tekst1,b-oppdater").         

     DYNAMIC-FUNCTION("setAddMovey",THIS-PROCEDURE:CURRENT-WINDOW,FRAME FRAME-7:HANDLE,
                      "b-oppdater").         
     
     IF VALID-HANDLE(wSubWin2) THEN
     DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("GetResizeFrameHandle" IN wSubWin2),
                      "b-konvtbl,button-ny,button-endre,button-slett") .         
     
     IF VALID-HANDLE(wSubWin3) THEN
     DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("GetResizeFrameHandle" IN wSubWin3),
                      "button-detaljer,button-oppdater-2") .     

     IF VALID-HANDLE(wSubWin4) THEN
     DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("GetResizeFrameHandle" IN wSubWin4),
                      "button-detaljer,button-oppdater-2") .         
     
    DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,730,550,0,0).
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSprak C-LevBasKort 
PROCEDURE InitSprak :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cLng AS CHAR NO-UNDO.

  DEF VAR iSysGr AS INT NO-UNDO.

  /* Tømmer tabellen */
  FOR EACH tmpSysPara:
      DELETE tmpSysPara.
  END.
  
  DO WITH FRAME FRAME-7:
    /* Standardspråk.                                           */
    /* Her skal browser og tmpTabellen tømmes.                  */
    /* Standard tekster fra rapportens parametre skal benyttes. */
    IF cLng = "" OR cLng = "DES" THEN
      ASSIGN
          BROWSE-4:SENSITIVE = FALSE.

    /* Bygger tmpTable for valgt språk. */
    ELSE DO:
        ASSIGN
            BROWSE-4:SENSITIVE = TRUE.

        /* Sjekker at det er lagt opp tekster for valgt språk.          */
        /* Er det ikke det, skal tekstene kopieres fra standard srpråk. */
        FIND FIRST SysGruppe NO-LOCK WHERE
          SysGruppe.SysHId = 5 AND
          SysGruppe.Beskrivelse = cLng NO-ERROR.

        IF NOT AVAILABLE SysGruppe THEN
        DO TRANSACTION:
          FIND LAST SysGruppe NO-LOCK WHERE
             SysGruppe.SysHId = 5 AND
             SysGruppe.SysGr >= 201 AND
             SysGruppe.SysGr <= 249 NO-ERROR.
          IF AVAILABLE SysGruppe THEN
            ASSIGN
              iSysGr = SysGruppe.SysGr + 1.
          ELSE
            ASSIGN
              iSysGr = 201.
          CREATE SysGruppe.
          ASSIGN
            SysGruppe.SysHId      = 5
            SysGruppe.SysGr       = iSysGr
            SysGruppe.Beskrivelse = cLng.
        END. /* TRANSACTION */
        ELSE
            ASSIGN iSysGr = SysGruppe.SysGr.
        
        /* Bygger opp fra default språk. */
        FOR EACH SysPara NO-LOCK WHERE
            SysPara.SysHId = 6 AND
            SysPara.SysGr  = 105:
            BUFFER-COPY SysPara TO tmpSysPara
                ASSIGN
                  tmpSysPara.SysHId = 5
                  tmpSysPara.SysGr  = iSysGr.
            RELEASE tmpSysPara.
        END.
        
        /* Tar bort overflødige tekster i valgt språk            */
        /* samtidig som at tekstene kopieres inn i tmp tabellen. */
        FOR EACH SysPara EXCLUSIVE-LOCK WHERE
            SysPara.SysHId = 5 AND
            SysPara.SysGr  = iSysGr TRANSACTION:
          FIND tmpSysPara WHERE
            tmpSysPara.SysHId = SysPara.SysHId AND
            tmpSysPara.SysGr  = SysPara.SysGr AND
            tmpSysPara.ParaNr = SysPara.ParaNr NO-ERROR.
          IF NOT AVAILABLE tmpSysPara THEN
            DELETE SysPara.
          ELSE 
            ASSIGN
              tmpSysPara.Parameter2 = SysPara.Parameter1.
        END.
    END.

    {&OPEN-QUERY-BROWSE-4}
  END. /* Frame scoop */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-LevBasKort 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBekreft AS INT NO-UNDO.
  DEF VAR ipStatus AS CHAR INIT "AVBRYT" NO-UNDO.
  DO WITH FRAME frame-7:
      IF INPUT LevBas.Lng <> "" THEN
      DO:
          FIND Sprak NO-LOCK WHERE
              Sprak.Lng = INPUT LevBas.Lng NO-ERROR.
          IF NOT AVAILABLE Sprak THEN
          DO:
              MESSAGE "Ugyldig språkkode!"
                  VIEW-AS ALERT-BOX ERROR TITLE "Feil".
              RETURN ipStatus.
          END.

      END.
      /* Lagre språk for ordreutskrift. */
      IF INPUT LevBas.Lng <> "" AND
         INPUT LevBas.Lng <> "DES" THEN
        RUN LagreSprak (INPUT LevBas.Lng).
  END.

  DO WITH FRAME FRAME-1:
    IF wModus = "NY" THEN DO:
      IF INPUT LevBas.LevNr < iLavesteNr THEN
      DO:
        MESSAGE "Laveste tillatte leverandørnummer er " + STRING(iLavesteNr) + "."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ipStatus.
      END.
      /* Sjekker input */
      IF INPUT LevBas.LevNr = 0 THEN
        DO:
          MESSAGE "Leverandørnummer må være større enn 0"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".
          APPLY "ENTRY":U TO LevBas.LevNr.
          RETURN ipStatus.
        END.
      IF CAN-FIND(LevBas WHERE
                  LevBas.LevNr = int(LevBas.LevNr:screen-value)) THEN
        DO:
          MESSAGE "Leverantör finnes allerede med nr:" LevBas.LevNr:screen-value
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO LevBas.LevNr.
          RETURN ipStatus.
        END.
    END.
    FIND Post NO-LOCK WHERE
         Post.PostNr = INPUT LevBas.LevPonr NO-ERROR.
    IF NOT AVAIL Post THEN
      DO:
          MESSAGE "Felaktigt postnummer"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".
          APPLY "ENTRY" TO LevBas.LevPonr.
          RETURN ipStatus.
    END.
    FIND Post NO-LOCK WHERE
         Post.PostNr = INPUT LevBas.KoPonr NO-ERROR.
    IF NOT AVAIL Post THEN
      DO:
          MESSAGE "Felaktigt postnummer"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY" TO LevBas.KoPonr.
          RETURN ipStatus.
    END.
    FIND Valuta NO-LOCK WHERE
      Valuta.ValKod = INPUT LevBas.ValKod NO-ERROR.
    IF NOT AVAIL Valuta THEN
      DO:
          MESSAGE "Felaktig valutakod"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY" TO LevBas.ValKod.
          RETURN ipStatus.
    END.
    LAGRE_LevBas:
      DO TRANSACTION:
        IF wModus = "NY" THEN
          DO:
            RELEASE LevBas. /* Slipper gammel post. */
            CREATE LevBas.
            ASSIGN
               wInputRecid = recid(LevBas)
               LevBas.LevNr.
          END.
        ELSE DO:
          FIND CURRENT LevBas EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF LOCKED LevBas THEN
            DO:
                MESSAGE "Leverantören oppdateres fra en annen termianl" SKIP
                  "Forsök å lagre en gang til" VIEW-AS ALERT-BOX 
               WARNING TITLE "Lagringsfeil".
           RETURN NO-APPLY ipStatus.
          END.
        END.

        ASSIGN FRAME Default-Frame
            LevBas.Notat
            .

        ASSIGN
             wModus       = "ENDRE"
             LevBas.LevNr:SENSITIVE = FALSE
             LevBas.levnamn
             LevBas.levkon
             LevBas.levadr
             LevBas.koadr
             LevBas.levponr
             LevBas.levpadr
             LevBas.koponr
             LevBas.kopadr
             LevBas.levtel
             LevBas.kotel
             LevBas.telefax
             LevBas.kotelefax
             LevBas.telex
             LevBas.kotelex
             LevBas.levland
             LevBas.koland
             LevBas.kommentar[1]
             LevBas.kommentar[2]
             LevBas.kommentar[3]
             LevBas.kommentar[4]
             LevBas.valkod
             LevBas.Lng
             LevBas.E_MailLev
             LevBas.E_MailKontakt
             LevBas.KjedeAvtale
             LevBAs.ReklAdresse1
             LevBAs.ReklAdresse2
             LevBAs.ReklPostNr
             LevBAs.ReklPostBoks
             LevBas.VisDivInfo[ 1] = INPUT T-1
             LevBas.VisDivInfo[ 2] = INPUT T-2
             LevBas.VisDivInfo[ 3] = INPUT T-3
             LevBas.VisDivInfo[ 4] = INPUT T-4
             LevBas.VisDivInfo[ 5] = INPUT T-5
             LevBas.VisDivInfo[ 6] = INPUT T-6
             LevBas.VisDivInfo[ 7] = INPUT T-7
             LevBas.VisDivInfo[ 8] = INPUT T-8
             LevBas.VisDivInfo[ 9] = INPUT T-9
             LevBas.VisDivInfo[10] = INPUT T-10
             LevBas.VisDivInfo[11] = INPUT T-11
             LevBas.VisDivInfo[12] = INPUT T-12
             LevBas.Rab1%
             LevBas.Rab2%
             LevBas.Frakt%
             LevBas.DivKost%
             .

         FIND CURRENT LevBas NO-LOCK.
         RUN VisPost.
         RUN BUTTONEnaDis.
      END.
  END.
  RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreSprak C-LevBasKort 
PROCEDURE LagreSprak :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cLng AS CHAR NO-UNDO.

  DEF VAR iSysGr AS INT NO-UNDO.

  /* Er det noe å lagre? */
  FIND FIRST tmpSysPara NO-ERROR.
  IF AVAILABLE tmpSysPara THEN
      ASSIGN
        iSysGr = tmpSysPara.SysGr.
  ELSE
      RETURN.

  /* Standardspråk. Her gjør vi ingenting.    */
  IF cLng = "" OR cLng = "DES" THEN
    RETURN.
  
  /* Skriver tekstene tilbake i databasen. */
  FOR EACH tmpSysPara EXCLUSIVE-LOCK TRANSACTION:
    FIND SysPara EXCLUSIVE-LOCK WHERE
      SysPara.SysHId = tmpSysPara.SysHId AND
      SysPara.SysGr  = tmpSysPara.SysGr AND
      SysPara.ParaNr = tmpSysPara.ParaNr NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
    DO:
        CREATE SysPara.
        ASSIGN
            SysPAra.SysHId = tmpSysPAra.SysHId
            SysPara.SysGr  = tmpSysPara.SysGr
            SysPara.ParaNr = tmpSysPara.ParaNr.
    END.
    ASSIGN
      SysPara.Beskrivelse = tmpSysPara.Beskrivelse
      SysPara.Parameter1  = tmpSysPara.Parameter2.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettMaskePaArtikkler C-LevBasKort 
PROCEDURE SettMaskePaArtikkler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lSvar  AS LOG NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  DO WITH FRAME FRAME-7:
    ASSIGN 
        lSvar = FALSE.
    MESSAGE "Skal leverandørens artikkler oppdateres med ny maske?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO TITLE "Bekreft"
        UPDATE lSvar.
    IF lSvar = FALSE THEN
        RETURN NO-APPLY.

    DO TRANSACTION:
        FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
            ArtBas.LevNr = LevBas.LevNr:
            ASSIGN
                iCount = iCount + 1
                ArtBas.VisDivInfo[ 1] = INPUT T-1
                ArtBas.VisDivInfo[ 2] = INPUT T-2
                ArtBas.VisDivInfo[ 3] = INPUT T-3
                ArtBas.VisDivInfo[ 4] = INPUT T-4
                ArtBas.VisDivInfo[ 5] = INPUT T-5
                ArtBas.VisDivInfo[ 6] = INPUT T-6
                ArtBas.VisDivInfo[ 7] = INPUT T-7
                ArtBas.VisDivInfo[ 8] = INPUT T-8
                ArtBas.VisDivInfo[ 9] = INPUT T-9
                ArtBas.VisDivInfo[10] = INPUT T-10
                ArtBas.VisDivInfo[11] = INPUT T-11
                ArtBas.VisDivInfo[12] = INPUT T-12
                .
        END.

    END. /* TRANSACTION */

    MESSAGE iCount "artikkler er oppdatert med ny maske." 
        VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".

  END. /* Frame Scoop */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModified C-LevBasKort 
PROCEDURE SettModified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wModified AS LOGI NO-UNDO.
  DO WITH FRAME Default-Frame:
      ASSIGN
          LevBas.Notat:MODIFIED = wModified
          .
  END.
  DO WITH FRAME FRAME-1:
    ASSIGN LevBas.levnr:MODIFIED = wModified
           LevBas.levnamn:MODIFIED = wModified
           LevBas.levkon:MODIFIED = wModified
           LevBas.levadr:MODIFIED = wModified
           LevBas.koadr:MODIFIED = wModified
           LevBas.levponr:MODIFIED = wModified
           LevBas.levpadr:MODIFIED = wModified
           LevBas.koponr:MODIFIED = wModified
           LevBas.kopadr:MODIFIED = wModified
           LevBas.levtel:MODIFIED = wModified
           LevBas.kotel:MODIFIED = wModified
           LevBas.telefax:MODIFIED = wModified
           LevBas.kotelefax:MODIFIED = wModified
           LevBas.telex:MODIFIED = wModified
           LevBas.kotelex:MODIFIED = wModified
           LevBas.levland:MODIFIED = wModified
           LevBas.koland:MODIFIED = wModified
           LevBas.kommentar[1]:MODIFIED = wModified
           LevBas.kommentar[2]:MODIFIED = wModified
           LevBas.kommentar[3]:MODIFIED = wModified
           LevBas.kommentar[4]:MODIFIED = wModified
           LevBas.valkod:MODIFIED = wModified
           LevBAs.ReklAdresse1:MODIFIED = wModified
           LevBAs.ReklAdresse2:MODIFIED = wModified
           LevBAs.ReklPostNr:MODIFIED = wModified
           LevBAs.ReklPostBoks:MODIFIED = wModified
           LevBas.Rab1%:MODIFIED = wModified
           LevBas.Rab2%:MODIFIED = wModified
           LevBas.Frakt%:MODIFIED = wModified
           LevBas.DivKost%:MODIFIED = wModified
           .
  END.
  DO WITH FRAME FRAME-7:
      ASSIGN
          LevBas.Lng:MODIFIED = wModified
          T-1:MODIFIED = wModified
          T-2:MODIFIED = wModified
          T-3:MODIFIED = wModified
          T-4:MODIFIED = wModified
          T-5:MODIFIED = wModified
          T-6:MODIFIED = wModified
          T-7:MODIFIED = wModified
          T-8:MODIFIED = wModified
          T-9:MODIFIED = wModified
          T-10:MODIFIED = wModified
          T-11:MODIFIED = wModified
          T-12:MODIFIED = wModified
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpChild C-LevBasKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-LevBasKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokLng C-LevBasKort 
PROCEDURE SokLng :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartGridord C-LevBasKort 
PROCEDURE StartGridord :
/*------------------------------------------------------------------------------
  Purpose:     Startar program från childbrowser
  Parameters:  <none>
  Notes:       Gäller för w-gridord.w,
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipArtBasRecid   AS RECID NO-UNDO.
    DEFINE INPUT PARAMETER ipBestHodeRecid AS RECID NO-UNDO.
    DEFINE INPUT PARAMETER ipModus         AS CHAR NO-UNDO.
    DEFINE VAR wPrgHandle AS HANDLE NO-UNDO.
    RUN w-gridord.w PERSISTENT SET wPrgHandle (ipArtBasRecid,INPUT-OUTPUT ipBestHodeRecid,ipModus).
    RUN SkapaTmpChild (wPrgHandle). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartVartkor C-LevBasKort 
PROCEDURE StartVartkor :
/*------------------------------------------------------------------------------
  Purpose:     Startar program från childbrowser
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipArtBasRecid   AS RECID NO-UNDO.
    DEFINE INPUT PARAMETER ipModus         AS CHAR NO-UNDO.
    DEFINE VAR wPrgHandle AS HANDLE NO-UNDO.

    RUN w-vartkor.w PERSISTENT SET wPrgHandle (ipArtBasRecid,ipModus).
    RUN SkapaTmpChild (wPrgHandle). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartVordre C-LevBasKort 
PROCEDURE StartVordre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipOrdreRecid   AS RECID NO-UNDO.
    DEFINE INPUT PARAMETER ipModus         AS CHAR NO-UNDO.
    DEFINE VAR wPrgHandle AS HANDLE NO-UNDO.

    RUN d-vordre.w PERSISTENT SET wPrgHandle (INPUT-OUTPUT ipOrdreRecid,ipModus).
    RUN SkapaTmpChild (wPrgHandle). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisEndretInfo C-LevBasKort 
PROCEDURE VisEndretInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAILABLE LevBas THEN
    DO:
      ASSIGN
        Fill-In-EndretInfo = "Opprettet " + 
                             (IF LevBas.RegistrertDato <> ? 
                               THEN string(LevBas.RegistrertDato)
                               ELSE "        ") + " " +
                             (IF LevBas.RegistrertTid <> 0
                               THEN string(LevBas.RegistrertTid,"HH:MM:SS")
                               ELSE "        ") + " av " + 
                             LevBas.RegistrertAv + "    Endret " +
                             (IF LevBas.EDato <> ?
                               THEN string(LevBas.EDato)
                               ELSE "        ") + " " +
                             (IF LevBas.ETid <> 0
                               THEN string(LevBas.ETid,"HH:MM:SS")
                               ELSE "        ") + " av " +
                             LevBas.BrukerId.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-LevBasKort 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN FILL-IN-LevNr:SCREEN-VALUE = STRING(LevBas.Levnr)
           FILL-IN-Levnamn:SCREEN-VALUE = LevBas.Levnamn.
  END.

  DO WITH FRAME FRAME-1:
    APPLY "ENTRY" TO CB-Sort IN FRAME DEFAULT-FRAME.
    FIND Valuta NO-LOCK WHERE
      Valuta.ValKod = LevBas.ValKod NO-ERROR.
    ASSIGN
      T-1  = LevBas.VisDivInfo[ 1]
      T-2  = LevBas.VisDivInfo[ 2]
      T-3  = LevBas.VisDivInfo[ 3]
      T-4  = LevBas.VisDivInfo[ 4]
      T-5  = LevBas.VisDivInfo[ 5]
      T-6  = LevBas.VisDivInfo[ 6]
      T-7  = LevBas.VisDivInfo[ 7]
      T-8  = LevBas.VisDivInfo[ 8]
      T-9  = LevBas.VisDivInfo[ 9]
      T-10 = LevBas.VisDivInfo[10]
      T-11 = LevBas.VisDivInfo[11]
      T-12 = LevBas.VisDivInfo[12]
      cDivInfo = string(LevBas.VisDivInfo[ 1]) + ',' +
                 string(LevBas.VisDivInfo[ 2]) + ',' +
                 string(LevBas.VisDivInfo[ 3]) + ',' +
                 string(LevBas.VisDivInfo[ 4]) + ',' +
                 string(LevBas.VisDivInfo[ 5]) + ',' +
                 string(LevBas.VisDivInfo[ 6]) + ',' +
                 string(LevBas.VisDivInfo[ 7]) + ',' +
                 string(LevBas.VisDivInfo[ 8]) + ',' +
                 string(LevBas.VisDivInfo[ 9]) + ',' +
                 string(LevBas.VisDivInfo[10]) + ',' +
                 string(LevBas.VisDivInfo[11]) + ',' +
                 string(LevBas.VisDivInfo[12])
        .
    DISPLAY
        LevBas.Notat
    WITH FRAME Default-Frame.
    DISPLAY LevBas.Levnr
            LevBas.levnamn
            LevBas.levkon 
            LevBas.levadr
            LevBas.koadr
            LevBas.levponr
            LevBas.levpadr
            LevBas.koponr
            LevBas.kopadr
            LevBas.levtel
            LevBas.kotel
            LevBas.telefax
            LevBas.kotelefax
            LevBas.telex
            LevBas.kotelex
            LevBas.levland
            LevBas.koland
            LevBas.E_MailLev
            LevBas.E_MailKontakt
            LevBas.kommentar[1]
            LevBas.kommentar[2]
            LevBas.kommentar[3]
            LevBas.kommentar[4]
            LevBas.valkod
            LevBas.Kjedeavtale
            LevBas.ReklAdresse1
            LevBas.ReklAdresse2
            LevBAs.ReklPostNr
            LevBAs.ReklPostBoks
            LevBas.Rab1%
            LevBas.Rab2%
            LevBas.Frakt%
            LevBas.DivKost%
            .
    FIND Sprak NO-LOCK WHERE
        Sprak.Lng = LevBas.Lng NO-ERROR.
    DISPLAY    
    LevBas.Lng
    "** Ukjent språk **" WHEN NOT AVAILABLE Sprak @ Sprak.Beskrivelse
    "<Standard>" WHEN LevBas.Lng = "" @ Sprak.Beskrivelse
    T-1
    T-2
    T-3
    T-4
    T-5
    T-6
    T-7
    T-8
    T-9
    T-10
    T-11
    T-12
    WITH FRAME FRAME-7.
    
    IF AVAILABLE Valuta THEN
      DISPLAY Valuta.ValDatum
              Valuta.ValKurs.
    ELSE 
      ASSIGN Valuta.ValDatum:SCREEN-VALUE = ""
             Valuta.ValKurs:SCREEN-VALUE = "".

     RUN SettModified(FALSE).

   FIND Post NO-LOCK WHERE
       Post.PostNr = LevBas.ReklPostNr NO-ERROR.
   IF AVAILABLE Post THEN
       fi-ReklPostSted:SCREEN-VALUE = Post.Beskrivelse.
   ELSE
       fi-ReklPostSted = "".

  END.
  {&OPEN-QUERY-B-LevKontakt}
  RUN VisEndretInfo.
  IF wAktivFlip = 1 THEN
      APPLY "ENTRY" TO LevBas.levNamn IN FRAME FRAME-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-LevBasKort 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Modifierad C-LevBasKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lRetValue AS LOG NO-UNDO.
DO WITH FRAME Default-Frame:
    ASSIGN
        LevBas.Notat:MODIFIED = TRUE
        .

END.

DO WITH FRAME FRAME-1:
    ASSIGN
        lRetValue = IF (LevBas.levnr:MODIFIED = TRUE OR
               LevBas.levnamn:MODIFIED = TRUE OR
               LevBas.levkon:MODIFIED = TRUE OR
               LevBas.levadr:MODIFIED = TRUE OR
               LevBas.koadr:MODIFIED = TRUE OR
               LevBas.levponr:MODIFIED = TRUE OR
               LevBas.levpadr:MODIFIED = TRUE OR
               LevBas.koponr:MODIFIED = TRUE OR
               LevBas.kopadr:MODIFIED = TRUE OR
               LevBas.levtel:MODIFIED = TRUE OR
               LevBas.kotel:MODIFIED = TRUE OR
               LevBas.telefax:MODIFIED = TRUE OR
               LevBas.kotelefax:MODIFIED = TRUE OR
               LevBas.telex:MODIFIED = TRUE OR
               LevBas.kotelex:MODIFIED = TRUE OR
               LevBas.levland:MODIFIED = TRUE OR
               LevBas.koland:MODIFIED = TRUE OR
               LevBas.kommentar[1]:MODIFIED = TRUE OR
               LevBas.kommentar[2]:MODIFIED = TRUE OR
               LevBas.kommentar[3]:MODIFIED = TRUE OR
               LevBas.kommentar[4]:MODIFIED = TRUE OR
               LevBas.valkod:MODIFIED = TRUE  OR
               LevBas.ReklAdresse1:MODIFIED = TRUE  OR
               LevBas.ReklAdresse2:MODIFIED = TRUE  OR
               LevBAs.ReklPostNr:MODIFIED = TRUE OR 
               LevBAs.ReklPostBoks:MODIFIED = TRUE  OR
               LevBas.Rab1%:MODIFIED = TRUE OR 
               LevBas.Rab2%:MODIFIED = TRUE OR 
               LevBas.Frakt%:MODIFIED = TRUE OR 
               LevBas.DivKost%:MODIFIED = TRUE
              )
              THEN TRUE ELSE FALSE
              .
  END.

  DO WITH FRAME FRAME-7:
    ASSIGN 
      lRetValue = IF (LevBas.Lng:MODIFIED = TRUE OR
                      T-1:MODIFIED = TRUE OR
                      T-2:MODIFIED = TRUE OR
                      T-3:MODIFIED = TRUE OR
                      T-4:MODIFIED = TRUE OR
                      T-5:MODIFIED = TRUE OR
                      T-6:MODIFIED = TRUE OR
                      T-7:MODIFIED = TRUE OR
                      T-8:MODIFIED = TRUE OR
                      T-9:MODIFIED = TRUE OR
                      T-10:MODIFIED = TRUE
                     ) THEN TRUE ELSE FALSE
                     .
  END.

  IF lRetValue THEN
      RETURN TRUE.
  ELSE
      RETURN FALSE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

