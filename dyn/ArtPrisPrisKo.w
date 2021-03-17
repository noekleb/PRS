&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE bOk              AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hBrowse          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBuffer          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldPris       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldAktivPris  AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldPrisKoPris AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldOpris      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldUtgatt     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldDB%        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hFieldAktivDB%   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hColPris         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hColPrisKoPris   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hColAktivPris    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hColDB%          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hColArtnr        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hColAktivDB%     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hPrisKoPris      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hPrisKoPris2     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hPrisKoInnPris   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hToolbar         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hViewRecCnt      AS HANDLE      NO-UNDO.
DEFINE VARIABLE ix               AS INTEGER     NO-UNDO.
DEF VAR iCurrTab                 AS INTEGER     NO-UNDO.
DEF VAR hCurrTabQuery            AS HANDLE      NO-UNDO.
DEF VAR hCurrTabProc             AS HANDLE      NO-UNDO.
DEF VAR hTabFolder               AS HANDLE      NO-UNDO.
DEF VAR hArtikkelkort            AS HANDLE      NO-UNDO.
DEF VAR iReturn                  AS INT         NO-UNDO.
DEF VAR iBrukerType              AS INT         NO-UNDO.
DEF VAR cSprak                   AS CHAR        NO-UNDO.
DEF VAR cSprakLst                AS CHAR  INITIAL 'SE,SVE' NO-UNDO.

DEF VAR cHuvGrAvdelingList     AS CHAR   NO-UNDO.
DEF VAR cVarGrHuvGrList        AS CHAR   NO-UNDO.
DEF VAR cLevBasRowIdList       AS CHAR   NO-UNDO.
DEF VAR cLevBasIdList          AS CHAR   NO-UNDO.
DEF VAR cAvdelingRowIdList     AS CHAR   NO-UNDO.
DEF VAR cAvdelingIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrIdList           AS CHAR   NO-UNDO.
DEF VAR cVarGrRowIdList        AS CHAR   NO-UNDO.
DEF VAR cVarGrIdList           AS CHAR   NO-UNDO.
DEF VAR cProdRowIdList         AS CHAR   NO-UNDO.
DEF VAR cVaremerkeIdList       AS CHAR   NO-UNDO.
DEF VAR cVaremerkeRowIdList    AS CHAR   NO-UNDO.
DEF VAR cProdIdList            AS CHAR   NO-UNDO.
DEF VAR iSelectorSourceCount   AS INT    NO-UNDO.
DEF VAR cCurrSelectBuffer      AS CHAR   NO-UNDO.
DEF VAR hArtBilde              AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame         AS HANDLE NO-UNDO.
DEF VAR cCL                    AS CHAR   NO-UNDO.
DEF VAR cQueryJoin             AS CHAR   NO-UNDO.
DEF VAR cSmallQueryJoin        AS CHAR   NO-UNDO.
DEF VAR cBuffersAndFlds        AS CHAR   NO-UNDO.
DEF VAR cSmallBuffersAndFlds   AS CHAR   NO-UNDO.
DEF VAR cSmallBufferList       AS CHAR   NO-UNDO.
DEF VAR cBufferList            AS CHAR   NO-UNDO.
DEF VAR bEditInnPris           AS LOG    NO-UNDO.
DEFINE VARIABLE iHKProfilnr AS INTE      NO-UNDO.
DEFINE VARIABLE iCLBut AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwArtpris KoFolder brwSearchField ~
rectFilter ArtikkelBilde tbArtPris tbEgneVarer tbVarerLokPris tbVarerPrisko ~
tbAvansertFilter cmbSaSong btnfiArtikkelNr cmbPrisProfil fiLevKod fiBeskr ~
fiLevFargKod cmbButikk sokLevNr sokProdNr sokVMId sokLevNamn sokProdusent ~
sokVaremerke rsArtPLU btnLev2 btnProdusent btnVaremerke sokAvdelingNr sokHg ~
sokVg sokAvdelingNavn sokHgBeskr sokVgBeskr btnClearFilter btnAvdeling ~
btnHuvGr btnVarGr 
&Scoped-Define DISPLAYED-OBJECTS tbEgneVarer tbVarerLokPris tbVarerPrisko ~
tbAvansertFilter cmbSaSong cmbPrisProfil fiLevKod fiBeskr fiLevFargKod ~
cmbButikk sokLevNr sokProdNr sokVMId sokLevNamn sokProdusent sokVaremerke ~
rsArtPLU sokAvdelingNr sokHg sokVg sokAvdelingNavn sokHgBeskr sokVgBeskr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnLev2 btnProdusent btnVaremerke btnAvdeling ~
btnHuvGr btnVarGr 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildORlist C-Win 
FUNCTION BuildORlist RETURNS CHARACTER
  ( INPUT icField     AS CHAR,
    INPUT icValueList AS CHAR,
    INPUT icFunction  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ChangeQueryJoin C-Win 
FUNCTION ChangeQueryJoin RETURNS LOGICAL
  ( INPUT ibFullJoin AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButikkNr C-Win 
FUNCTION getButikkNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrisKoBufferNum C-Win 
FUNCTION getPrisKoBufferNum RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProfilnr C-Win 
FUNCTION getProfilnr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VisArtBilde C-Win 
FUNCTION VisArtBilde RETURNS LOGICAL
  ( INPUT iiBildeNr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk alle avdelinger".

DEFINE BUTTON btnClearFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnfiArtikkelNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnHuvGr 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i alle hovedgrupper".

DEFINE BUTTON btnLev2 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk hele leverandør-register".

DEFINE BUTTON btnProdusent 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk hele leverandør-register".

DEFINE BUTTON btnVaremerke 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk hele leverandør-register".

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i alle varegrupper".

DEFINE VARIABLE cmbButikk AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 40
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cmbPrisProfil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prisprofil" 
     VIEW-AS COMBO-BOX INNER-LINES 40
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cmbSaSong AS CHARACTER FORMAT "X(256)" INITIAL "1" 
     LABEL "Sesong" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 TOOLTIP "Sesong".

DEFINE VARIABLE fiBeskr AS CHARACTER FORMAT "x(20)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Kort beskrivelse av artikkelen".

DEFINE VARIABLE fiLevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "Lev.farge" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Leverandørens fargekode".

DEFINE VARIABLE fiLevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "Lev.artnr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Leverandørens artikkelnummer".

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Hovedgr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokHgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokProdNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Produsent".

DEFINE VARIABLE sokProdusent AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokVaremerke AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokVg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokVgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE sokVMId AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1 TOOLTIP "Varemerke (~"Brand~").".

DEFINE VARIABLE rsArtPLU AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Artikkelregister", 1,
"Åpen pris", 2,
"Pant", 3
     SIZE 45.2 BY .67 NO-UNDO.

DEFINE RECTANGLE ArtikkelBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 4.24.

DEFINE RECTANGLE brwArtpris
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 24.38.

DEFINE RECTANGLE brwSearchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY .91.

DEFINE RECTANGLE KoFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 24.38.

DEFINE RECTANGLE rectFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 200.8 BY 4.76.

DEFINE RECTANGLE tbArtPris
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE tbAvansertFilter AS LOGICAL INITIAL no 
     LABEL "Vis avansert filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tbEgneVarer AS LOGICAL INITIAL no 
     LABEL "Vis kun varer med salg i egen butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .81 NO-UNDO.

DEFINE VARIABLE tbVarerLokPris AS LOGICAL INITIAL no 
     LABEL "Vis kun varer med lokal pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 TOOLTIP "Det vil si varer som har registrert pris for valgt prisprofil" NO-UNDO.

DEFINE VARIABLE tbVarerPrisko AS LOGICAL INITIAL no 
     LABEL "Vis kun varer med poster i priskø" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 TOOLTIP "Det vil si varer som har registrert pris for valgt prisprofil" NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 1.4 BY 24.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbEgneVarer AT ROW 1.19 COL 3
     tbVarerLokPris AT ROW 2.05 COL 3
     tbVarerPrisko AT ROW 2.91 COL 3
     tbAvansertFilter AT ROW 3.71 COL 7
     cmbSaSong AT ROW 4.57 COL 11.2 COLON-ALIGNED
     btnfiArtikkelNr AT ROW 4.57 COL 35.2 NO-TAB-STOP 
     cmbPrisProfil AT ROW 1.24 COL 53.2 COLON-ALIGNED
     fiLevKod AT ROW 2.48 COL 53.2 COLON-ALIGNED
     fiBeskr AT ROW 3.52 COL 53.2 COLON-ALIGNED
     fiLevFargKod AT ROW 4.57 COL 53.2 COLON-ALIGNED
     cmbButikk AT ROW 1.24 COL 88.2 COLON-ALIGNED
     sokLevNr AT ROW 2.48 COL 88.2 COLON-ALIGNED HELP
          "Leverandør - trykk F3 for å søke blant alle leverandører"
     sokProdNr AT ROW 3.52 COL 88.2 COLON-ALIGNED
     sokVMId AT ROW 4.57 COL 88.2 COLON-ALIGNED
     sokLevNamn AT ROW 2.48 COL 99.2 COLON-ALIGNED NO-LABEL
     sokProdusent AT ROW 3.52 COL 99.2 COLON-ALIGNED NO-LABEL
     sokVaremerke AT ROW 4.57 COL 99.2 COLON-ALIGNED NO-LABEL
     rsArtPLU AT ROW 1.43 COL 118.6 NO-LABEL
     btnLev2 AT ROW 2.48 COL 125.6 NO-TAB-STOP 
     btnProdusent AT ROW 3.52 COL 125.6 NO-TAB-STOP 
     btnVaremerke AT ROW 4.57 COL 125.6 NO-TAB-STOP 
     sokAvdelingNr AT ROW 2.48 COL 138.2 COLON-ALIGNED HELP
          "Varegruppe"
     sokHg AT ROW 3.52 COL 138.2 COLON-ALIGNED HELP
          "Varegruppe"
     sokVg AT ROW 4.57 COL 138.2 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 2.48 COL 149 COLON-ALIGNED NO-LABEL
     sokHgBeskr AT ROW 3.52 COL 149 COLON-ALIGNED NO-LABEL
     sokVgBeskr AT ROW 4.57 COL 149 COLON-ALIGNED NO-LABEL
     btnClearFilter AT ROW 1.24 COL 164.6
     btnAvdeling AT ROW 2.48 COL 175.2 NO-TAB-STOP 
     btnHuvGr AT ROW 3.48 COL 175.2 NO-TAB-STOP 
     btnVarGr AT ROW 4.52 COL 175.2 NO-TAB-STOP 
     brwArtpris AT ROW 7.19 COL 2
     KoFolder AT ROW 7.19 COL 118
     brwSearchField AT ROW 5.95 COL 2
     rectFilter AT ROW 1.1 COL 1.4
     ArtikkelBilde AT ROW 1.29 COL 180
     tbArtPris AT ROW 5.91 COL 23.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 201.4 BY 30.67.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1.05 COL 67.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 50 ROW 7.24
         SIZE 89 BY 24.33.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Insert window title>"
         HEIGHT             = 30.71
         WIDTH              = 201.2
         MAX-HEIGHT         = 40.19
         MAX-WIDTH          = 288
         VIRTUAL-HEIGHT     = 40.19
         VIRTUAL-WIDTH      = 288
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frmSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME L-To-R,COLUMNS                                            */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 30.67
       FRAME DEFAULT-FRAME:WIDTH            = 201.4.

/* SETTINGS FOR BUTTON btnAvdeling IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnHuvGr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnLev2 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnProdusent IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnVaremerke IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnVarGr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FRAME frmSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frmSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmSplitBarX
/* Query rebuild information for FRAME frmSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frmSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdeling C-Win
ON CHOOSE OF btnAvdeling IN FRAME DEFAULT-FRAME /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                      "Avdeling;AvdelingNr;AvdelingNavn",
                      "where true",
                      INPUT-OUTPUT cAvdelingRowIdList,
                      "AvdelingNr",
                      INPUT-OUTPUT cAvdelingIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cAvdelingRowidList) > 1 THEN 
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = "0"
             sokAvdelingNavn:SCREEN-VALUE = STRING(NUM-ENTRIES(cAvdelingRowidList)) + " av " +
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = cAvdelingIdList
             sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearFilter C-Win
ON CHOOSE OF btnClearFilter IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
  ASSIGN fiBeskr:SCREEN-VALUE = ""
         fiLevFargKod:SCREEN-VALUE = ""
         fiLevKod:SCREEN-VALUE = ""
         sokAvdelingNavn:SCREEN-VALUE = ""
         sokAvdelingNr:SCREEN-VALUE = ""
         sokHg:SCREEN-VALUE = ""
         sokHgBeskr:SCREEN-VALUE = "" 
         sokLevNamn:SCREEN-VALUE = ""
         sokLevNr:SCREEN-VALUE = ""
         sokProdNr:SCREEN-VALUE = ""
         sokProdusent:SCREEN-VALUE = ""
         sokVaremerke:SCREEN-VALUE = ""
         sokVg:SCREEN-VALUE = ""
         sokVgBeskr:SCREEN-VALUE = ""
         sokVMId:SCREEN-VALUE = ""
         tbEgneVarer:CHECKED = NO
         .
  DYNAMIC-FUNCTION("ClearComboBox",cmbSaSong:HANDLE).
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfiArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfiArtikkelNr C-Win
ON CHOOSE OF btnfiArtikkelNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR ocValue  AS CHAR NO-UNDO.

  RUN JBoxDSimpleSelectList.w ("Alle|0|Aktive|1|Ikke aktive|2",?,OUTPUT ocValue).

  IF ocValue NE "" THEN
    ASSIGN cmbSaSong:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList",
                                                                           "SaSong;SasBeskr;SaSong",
                                                                           "WHERE " 
                                                                         + (IF ocValue = "0" THEN "true"
                                                                            ELSE IF ocValue = "1" THEN "Aktiv"
                                                                            ELSE "NOT Aktiv"))
                                                  ,"|")
           cmbSaSong:SCREEN-VALUE = "0"
           .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHuvGr C-Win
ON CHOOSE OF btnHuvGr IN FRAME DEFAULT-FRAME /* ... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn",
                      "where true, first Avdeling OF HuvGr NO-LOCK " + 
                         (IF cAvdelingRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                          ELSE ""),
                      INPUT-OUTPUT cHuvGrRowIdList,
                      "Hg",
                      INPUT-OUTPUT cHuvGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.


  IF bOk THEN DO:
    IF NUM-ENTRIES(cHuvGrRowidList) > 1 THEN 
      ASSIGN sokHg:SCREEN-VALUE   = "0"
             sokHgBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cHuvGrRowidList)) + " av " +
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokHg:SCREEN-VALUE   = cHuvGrIdList
             sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev2 C-Win
ON CHOOSE OF btnLev2 IN FRAME DEFAULT-FRAME /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn",
                      "where true",
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cLevBasRowidList) > 1 THEN 
      ASSIGN sokLevNr:SCREEN-VALUE   = "0"
             sokLevNamn:SCREEN-VALUE = STRING(NUM-ENTRIES(cLevBasRowidList)) + " av " +
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokLevNr:SCREEN-VALUE   = ENTRY(1,cLevBasIdList,"|")
             sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE)
             .
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProdusent C-Win
ON CHOOSE OF btnProdusent IN FRAME DEFAULT-FRAME /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Produsent;prodnr;beskrivelse",
                      "where true",
                      INPUT-OUTPUT cProdRowIdList,
                      "Prodnr",
                      INPUT-OUTPUT cProdIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cProdRowidList) > 1 THEN 
      ASSIGN sokProdNr:SCREEN-VALUE   = "0"
             sokProdusent:SCREEN-VALUE = STRING(NUM-ENTRIES(cProdRowidList)) + " av " +
                                         STRING(iSelectorSourceCount)
             .
    ELSE
      ASSIGN sokProdNr:SCREEN-VALUE    = ENTRY(1,cProdIdList,"|")
             sokProdusent:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Produsent;Beskrivelse","WHERE ProdNr = " + sokProdNr:SCREEN-VALUE)
             .
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frmSplitBarX /* Button 1 */
DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
  RUN MoveToTop IN hCurrTabProc NO-ERROR.  
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVaremerke C-Win
ON CHOOSE OF btnVaremerke IN FRAME DEFAULT-FRAME /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Varemerke;VMId;beskrivelse",
                      "where true",
                      INPUT-OUTPUT cVaremerkeRowIdList,
                      "VMId",
                      INPUT-OUTPUT cVaremerkeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    IF NUM-ENTRIES(cVaremerkeRowidList) > 1 THEN 
      ASSIGN sokVMId:SCREEN-VALUE   = "0"
             sokVaremerke:SCREEN-VALUE = STRING(NUM-ENTRIES(cVaremerkeRowidList)) + " av " +
                                         STRING(iSelectorSourceCount)
             .
    ELSE
      ASSIGN sokVMId:SCREEN-VALUE    = ENTRY(1,cVaremerkeIdList,"|")
             sokVaremerke:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Varemerke;Beskrivelse","WHERE VMid = " + sokVMid:SCREEN-VALUE)
             .
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr C-Win
ON CHOOSE OF btnVarGr IN FRAME DEFAULT-FRAME /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VarGr;Vg;VgBeskr,HuvGr;Hg;HgBeskr",
                      "where true, first HuvGr OF VarGr NO-LOCK " + 
                         (IF cHuvGrRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cHuvGrRowIdList + "',STRING(ROWID(HuvGr)))"
                          ELSE ""),
                      INPUT-OUTPUT cVarGrRowIdList,
                      "Vg",
                      INPUT-OUTPUT cVarGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.


  IF bOk THEN DO:
    IF NUM-ENTRIES(cVarGrRowidList) > 1 THEN 
      ASSIGN sokVG:SCREEN-VALUE      = "0"
             sokVGBeskr:SCREEN-VALUE = STRING(NUM-ENTRIES(cVarGrRowidList)) + " av " +
                                       STRING(iSelectorSourceCount)
                                       .
    ELSE
      ASSIGN sokVG:SCREEN-VALUE   = cVarGrIdList
             sokVGBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVG:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbButikk C-Win
ON VALUE-CHANGED OF cmbButikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:  
  DEF VAR cCurrProfilnr AS CHAR NO-UNDO.
  DEF VAR cProfilnr     AS CHAR NO-UNDO.
/*   ASSIGN cCurrProfilnr = cmbPrisProfil:SCREEN-VALUE */
  ASSIGN cCurrProfilnr = STRING(cmbPrisProfil)
         cProfilNr = DYNAMIC-FUNCTION("getFieldValues","butiker","WHERE butik = " + cmbButikk:SCREEN-VALUE,"ProfilNr").
  IF cProfilNr NE ? THEN
    cmbPrisProfil:SCREEN-VALUE = cProfilnr.
  ELSE DO:
    IF CAN-DO(cSprakLst,cSprak) THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,"Butik saknar prisprofil","","").
    ELSE 
        DYNAMIC-FUNCTION("DoMessage",0,0,"Butikk er ikke satt opp med prisprofil","","").
    RETURN NO-APPLY.
  END.
  ASSIGN INPUT cmbPrisProfil.
  IF cProfilnr NE cCurrProfilnr THEN DO:
      FIND butiker WHERE butiker.butik = INT(cmbButikk:SCREEN-VALUE) NO-LOCK NO-ERROR.
      IF Butiker.Sentrallager = FALSE THEN DO:
          iCLBut = Butiker.clButikkNr.
          FIND butiker WHERE butiker.butik = iCLBut NO-LOCK NO-ERROR.
          IF iHKProfilnr <> butiker.profilnr THEN DO:
              iHKProfilnr = butiker.profilnr.
              DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE ProfilNr = " + STRING(iHKProfilnr)).
          END.
      END.
/*      FI-X:SCREEN-VALUE = STRING(iHKProfilnr). */
    RUN InvokeMethod(hBrowse,"OpenQuery").
    PUBLISH "ProfilnrChanged" (cProfilnr).
  END.
  PUBLISH "ButikknrChanged" (cmbButikk:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbPrisProfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbPrisProfil C-Win
ON VALUE-CHANGED OF cmbPrisProfil IN FRAME DEFAULT-FRAME /* Prisprofil */
DO:  
  DEF VAR cButForProfil AS CHAR NO-UNDO.

  cButForProfil = DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik",
                                   "WHERE " + (IF cmbPrisProfil:SCREEN-VALUE = "1" THEN "true" ELSE "Profilnr = " + cmbPrisProfil:SCREEN-VALUE) + " BY butik").

  IF cButForProfil NE "" THEN
    ASSIGN cmbButikk:LIST-ITEM-PAIRS = cButForProfil
           cmbButikk:SCREEN-VALUE    = (IF cmbPrisProfil:SCREEN-VALUE = "1" THEN cCl ELSE ENTRY(2,cButForProfil,"|"))
           .
  ELSE DO:
   IF CAN-DO(cSprakLst,cSprak) THEN
       DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen butik använder denna prisprofil","","").
   ELSE
       DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen butikker benytter denne prisprofilen","","").
    RETURN NO-APPLY.
  END.

  RUN InvokeMethod(hBrowse,"OpenQuery").
  PUBLISH "ProfilnrChanged" (cmbPrisProfil:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbSaSong C-Win
ON VALUE-CHANGED OF cmbSaSong IN FRAME DEFAULT-FRAME /* Sesong */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeskr C-Win
ON RETURN OF fiBeskr IN FRAME DEFAULT-FRAME /* Varetekst */
DO:
  IF SELF:MODIFIED THEN RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevFargKod C-Win
ON RETURN OF fiLevFargKod IN FRAME DEFAULT-FRAME /* Lev.farge */
DO:
  IF SELF:MODIFIED THEN RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevKod C-Win
ON RETURN OF fiLevKod IN FRAME DEFAULT-FRAME /* Lev.artnr */
DO:
  IF SELF:MODIFIED THEN RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsArtPLU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsArtPLU C-Win
ON VALUE-CHANGED OF rsArtPLU IN FRAME DEFAULT-FRAME
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNavn C-Win
ON RETURN OF sokAvdelingNavn IN FRAME DEFAULT-FRAME
DO:
  IF sokAvdelingNavn:MODIFIED THEN DO: 
    ASSIGN cAvdelingRowIdList = ""
           cAvdelingIdList    = ""
           .    
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON F3 OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
DO:
  APPLY "choose" TO btnAvdeling.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON RETURN OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
DO:
  IF sokAvdelingNr:MODIFIED  THEN DO:
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON TAB OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
DO:
  IF SELF:MODIFIED THEN sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON F3 OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgr */
DO:
  APPLY "choose" TO btnHuvGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON RETURN OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgr */
DO:
  IF sokHg:MODIFIED THEN DO: 
    sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHg C-Win
ON TAB OF sokHg IN FRAME DEFAULT-FRAME /* Hovedgr */
DO:
  IF SELF:MODIFIED THEN sokHgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","HuvGr;HgBeskr","WHERE Hg = " + sokHg:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokHgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokHgBeskr C-Win
ON RETURN OF sokHgBeskr IN FRAME DEFAULT-FRAME
DO:
  IF sokHgBeskr:MODIFIED THEN DO:
    ASSIGN cHuvGrRowIdList = ""
           cHuvGrIdList    = ""
           .
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNamn C-Win
ON RETURN OF sokLevNamn IN FRAME DEFAULT-FRAME
DO:
  IF sokLevNamn:MODIFIED THEN DO: 
    ASSIGN cLevBasRowIdList = ""
           cLevBasIdList    = ""
           .
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON F3 OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
DO:
  APPLY "choose" TO btnLev2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON TAB OF sokLevNr IN FRAME DEFAULT-FRAME /* Lev */
DO:
  IF SELF:MODIFIED THEN sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokProdNr C-Win
ON F3 OF sokProdNr IN FRAME DEFAULT-FRAME /* Produsent */
DO:
  APPLY "choose" TO btnProdusent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokProdNr C-Win
ON RETURN OF sokProdNr IN FRAME DEFAULT-FRAME /* Produsent */
DO:
  IF SELF:MODIFIED THEN DO:
    sokProdusent:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Produsent;Beskrivelse","WHERE ProdNr = " + SELF:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").  
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokProdNr C-Win
ON TAB OF sokProdNr IN FRAME DEFAULT-FRAME /* Produsent */
DO:
  IF SELF:MODIFIED THEN sokProdusent:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Produsent;Beskrivelse","WHERE ProdNr = " + SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokProdusent C-Win
ON RETURN OF sokProdusent IN FRAME DEFAULT-FRAME
DO:
  IF sokProdusent:MODIFIED THEN DO: 
    ASSIGN cProdRowIdList = ""
           cProdIdList    = ""
           .
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVaremerke C-Win
ON RETURN OF sokVaremerke IN FRAME DEFAULT-FRAME
DO:
  IF sokVaremerke:MODIFIED THEN DO: 
    ASSIGN cVaremerkeRowIdList = ""
           cVaremerkeIdList    = ""
           .
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON F3 OF sokVg IN FRAME DEFAULT-FRAME /* Varegr */
DO:
  APPLY "choose" TO btnVarGr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON RETURN OF sokVg IN FRAME DEFAULT-FRAME /* Varegr */
DO:
  IF sokVg:MODIFIED THEN DO: 
    sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVg C-Win
ON TAB OF sokVg IN FRAME DEFAULT-FRAME /* Varegr */
DO:
  IF SELF:MODIFIED THEN sokVgBeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","VarGr;VgBeskr","WHERE Vg = " + sokVg:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVgBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVgBeskr C-Win
ON RETURN OF sokVgBeskr IN FRAME DEFAULT-FRAME
DO:
  IF sokVgBeskr:MODIFIED THEN DO:
    ASSIGN cVarGrRowIdList = ""
           cVarGrIdList    = ""
           .
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokVMId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVMId C-Win
ON F3 OF sokVMId IN FRAME DEFAULT-FRAME /* Varemerke */
DO:
  APPLY "choose" TO btnVaremerke.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVMId C-Win
ON RETURN OF sokVMId IN FRAME DEFAULT-FRAME /* Varemerke */
DO:
  IF SELF:MODIFIED THEN DO:
    sokVaremerke:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Varemerke;Beskrivelse","WHERE VMid = " + SELF:SCREEN-VALUE).  
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokVMId C-Win
ON TAB OF sokVMId IN FRAME DEFAULT-FRAME /* Varemerke */
DO:
  IF SELF:MODIFIED THEN sokVaremerke:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Varemerke;Beskrivelse","WHERE VMid = " + SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvansertFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvansertFilter C-Win
ON VALUE-CHANGED OF tbAvansertFilter IN FRAME DEFAULT-FRAME /* Vis avansert filter */
DO:
  ASSIGN btnAvdeling:HIDDEN     = NOT tbAvansertFilter:CHECKED
         btnClearFilter:HIDDEN  = NOT tbAvansertFilter:CHECKED
         btnfiArtikkelNr:HIDDEN = NOT tbAvansertFilter:CHECKED
         btnHuvGr:HIDDEN        = NOT tbAvansertFilter:CHECKED
         btnLev2:HIDDEN         = NOT tbAvansertFilter:CHECKED
         btnProdusent:HIDDEN    = NOT tbAvansertFilter:CHECKED
         btnVaremerke:HIDDEN    = NOT tbAvansertFilter:CHECKED
         btnVarGr:HIDDEN        = NOT tbAvansertFilter:CHECKED
         cmbSaSong:HIDDEN       = NOT tbAvansertFilter:CHECKED
         fiBeskr:HIDDEN         = NOT tbAvansertFilter:CHECKED
         fiLevFargKod:HIDDEN    = NOT tbAvansertFilter:CHECKED
         fiLevKod:HIDDEN        = NOT tbAvansertFilter:CHECKED
         sokAvdelingNavn:HIDDEN = NOT tbAvansertFilter:CHECKED
         sokAvdelingNr:HIDDEN   = NOT tbAvansertFilter:CHECKED
         sokHg:HIDDEN           = NOT tbAvansertFilter:CHECKED
         sokHgBeskr:HIDDEN      = NOT tbAvansertFilter:CHECKED
         sokLevNamn:HIDDEN      = NOT tbAvansertFilter:CHECKED
         sokLevNr:HIDDEN        = NOT tbAvansertFilter:CHECKED
         sokProdNr:HIDDEN       = NOT tbAvansertFilter:CHECKED
         sokProdusent:HIDDEN    = NOT tbAvansertFilter:CHECKED
         sokVaremerke:HIDDEN    = NOT tbAvansertFilter:CHECKED
         sokVg:HIDDEN           = NOT tbAvansertFilter:CHECKED
         sokVgBeskr:HIDDEN      = NOT tbAvansertFilter:CHECKED
         sokVMid:HIDDEN         = NOT tbAvansertFilter:CHECKED
         .
  IF NOT PROGRAM-NAME(2) BEGINS "InitializeObject" THEN
    APPLY "END-MOVE" TO btnSplitBarX IN FRAME frmSplitBarX.

  ChangeQueryJoin(tbAvansertFilter:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbEgneVarer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbEgneVarer C-Win
ON VALUE-CHANGED OF tbEgneVarer IN FRAME DEFAULT-FRAME /* Vis kun varer med salg i egen butikk */
DO:
/*   IF tbEgneVarer:CHECKED AND cmbButikk:SCREEN-VALUE = cCL THEN DO:                             */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en butikk for å filtrere spørring på salg","","").  */
/*     tbEgneVarer:CHECKED = NO.                                                                  */
/*     RETURN NO-APPLY.                                                                           */
/*   END.                                                                                         */
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbVarerLokPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbVarerLokPris C-Win
ON VALUE-CHANGED OF tbVarerLokPris IN FRAME DEFAULT-FRAME /* Vis kun varer med lokal pris */
DO:
  IF tbVarerLokPris:CHECKED AND cmbButikk:SCREEN-VALUE = cCL THEN DO:
      IF CAN-DO(cSprakLst,cSprak) THEN
          DYNAMIC-FUNCTION("DoMessage",0,0,"Välj butik (ej centrallager) för att filtrera på lokala priser","","").
      ELSE
          DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en butikk (forskjellig fra sentrallager) for å filtrere spørring på lokalepriser","","").
    tbVarerLokPris:CHECKED = NO.
    RETURN NO-APPLY.
  END.
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbVarerPrisko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbVarerPrisko C-Win
ON VALUE-CHANGED OF tbVarerPrisko IN FRAME DEFAULT-FRAME /* Vis kun varer med poster i priskø */
DO:
  IF tbVarerLokPris:CHECKED AND cmbButikk:SCREEN-VALUE = cCL THEN DO:
      IF CAN-DO(cSprakLst,cSprak) THEN
          DYNAMIC-FUNCTION("DoMessage",0,0,"Välj butik (ej centrallager) för att filtrera på lokala priser","","").
      ELSE 
          DYNAMIC-FUNCTION("DoMessage",0,0,"Velg en butikk (forskjellig fra sentrallager) for å filtrere spørring på lokalepriser","","").
    tbVarerLokPris:CHECKED = NO.
    RETURN NO-APPLY.
  END.
  RUN InvokeMethod(hBrowse,"OpenQuery").
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
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  IF VALID-HANDLE(hArtBilde) THEN APPLY "close" TO hArtBilde.
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    DEF VAR cDummy AS CHAR NO-UNDO.
    RUN InitStringTranslation IN hCurrTabProc (THIS-PROCEDURE:CURRENT-WINDOW,INPUT-OUTPUT cDummy).
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


ON "WINDOW-RESIZED" OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW) NO-ERROR.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtBasEndret C-Win 
PROCEDURE ArtBasEndret :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM crArtbas AS CHAR NO-UNDO.

DEF VAR cBufferList AS CHAR   NO-UNDO.

cBufferList = DYNAMIC-FUNCTION("getAttribute",hBrowse,"bufferList").

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersAndFields") = cSmallBuffersAndFlds THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferList","cSmallBufferList").

IF hBuffer:AVAIL THEN
  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent" + getPrisKoBufferNum()):BUFFER-VALUE).

DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferList",cBufferList).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iArtBasBufferPos AS INT  NO-UNDO.
DEF VAR cBuffsAndFlds    AS CHAR NO-UNDO. 

  IF iBrukerType > 1 /* Butikkbruker */ THEN
  DO:
      IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
        cBuffsAndFlds = DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersAndFields").
        DO iArtBasBufferPos = 1 TO NUM-ENTRIES(cBuffsAndFlds):
          IF ENTRY(1,ENTRY(iArtBasBufferPos,cBuffsAndFlds),";") = "ArtBas" THEN LEAVE.
        END.
        RUN ArtBasVisTime.w   PERSIST SET hArtikkelkort (THIS-PROCEDURE,hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
      END.
      ELSE
        RUN ByttArtikkel IN hArtikkelkort (hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
  END.
  ELSE DO:
      IF NOT VALID-HANDLE(hArtikkelkort) THEN DO:
        cBuffsAndFlds = DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersAndFields").
        DO iArtBasBufferPos = 1 TO NUM-ENTRIES(cBuffsAndFlds):
          IF ENTRY(1,ENTRY(iArtBasBufferPos,cBuffsAndFlds),";") = "ArtBas" THEN LEAVE.
        END.
        RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",hBuffer:BUFFER-FIELD("RowIdent" + STRING(iArtBasBufferPos)):BUFFER-VALUE), "ENDRE," + STRING(THIS-PROCEDURE)).
        SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "ArtBasEndret" IN hArtikkelKort.
      END.
      ELSE
        RUN ByttArtikkel IN hArtikkelkort (hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
  END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR rRepos  AS ROWID NO-UNDO.
DEFINE VAR bFreeze AS LOG   NO-UNDO.

IF NOT LAST-EVENT:LABEL BEGINS "cursor-" THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","yes").
  bFreeze = YES.
END.

RUN SUPER.

RUN VisMiniBilde IN hArtBilde 
    (IF hBuffer:AVAIL THEN hBuffer:BUFFER-FIELD("BildNr"):BUFFER-VALUE 
     ELSE 0).

IF hBuffer:AVAILABLE THEN DO:
  IF VALID-HANDLE(hCurrTabQuery) AND NOT hCurrTabQuery:NAME BEGINS "brwStrekkode" THEN DO:
    hCurrTabQuery:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE ArtikkelNr = " + STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)) NO-ERROR.
    IF hCurrTabQuery:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
      rRepos = hCurrTabQuery:QUERY:GET-BUFFER-HANDLE(1):ROWID.
      hCurrTabQuery:DESELECT-ROWS() NO-ERROR. 
      hCurrTabQuery:SET-REPOSITIONED-ROW(10,"conditional").
      hCurrTabQuery:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        hCurrTabQuery:SELECT-ROW(hCurrTabQuery:FOCUSED-ROW).
    END.
  END.
  ELSE APPLY "entry" TO hBrowse.
  IF VALID-HANDLE(hArtikkelkort) THEN
    RUN ByttArtikkel IN hArtikkelkort (IF hBuffer:AVAIL THEN hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                       ELSE 0).

  hPrisKoPris:SENSITIVE = NOT hBuffer:BUFFER-FIELD("Opris"):BUFFER-VALUE AND hBuffer:BUFFER-FIELD("LokPris"):BUFFER-VALUE AND NOT hBuffer:BUFFER-FIELD("Pant"):BUFFER-VALUE.
  IF VALID-HANDLE(hPrisKoInnPris) THEN
    hPrisKoInnPris:SENSITIVE = hPrisKoPris:SENSITIVE AND bEditInnPris.
END.

IF bFreeze THEN DO:
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

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
  DISPLAY tbEgneVarer tbVarerLokPris tbVarerPrisko tbAvansertFilter cmbSaSong 
          cmbPrisProfil fiLevKod fiBeskr fiLevFargKod cmbButikk sokLevNr 
          sokProdNr sokVMId sokLevNamn sokProdusent sokVaremerke rsArtPLU 
          sokAvdelingNr sokHg sokVg sokAvdelingNavn sokHgBeskr sokVgBeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwArtpris KoFolder brwSearchField rectFilter ArtikkelBilde tbArtPris 
         tbEgneVarer tbVarerLokPris tbVarerPrisko tbAvansertFilter cmbSaSong 
         btnfiArtikkelNr cmbPrisProfil fiLevKod fiBeskr fiLevFargKod cmbButikk 
         sokLevNr sokProdNr sokVMId sokLevNamn sokProdusent sokVaremerke 
         rsArtPLU btnLev2 btnProdusent btnVaremerke sokAvdelingNr sokHg sokVg 
         sokAvdelingNavn sokHgBeskr sokVgBeskr btnClearFilter btnAvdeling 
         btnHuvGr btnVarGr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EntryOfWidget C-Win 
PROCEDURE EntryOfWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hWidget AS HANDLE NO-UNDO.

hWidget = DYNAMIC-FUNCTION("getCurrentObject").
  

    IF CAN-DO(cSprakLst,cSprak) THEN
    DO:
    END.
      IF hWidget:NAME = "ArtikkelNr" THEN
        hWidget:HELP = "Søk Streckod / Art.nr".
    ELSE DO:
        IF hWidget:NAME = "ArtikkelNr" THEN
          hWidget:HELP = "Søk Strekkode / Art.nr".
    END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn         AS INT    NO-UNDO.
DEF VAR cFileName       AS CHAR   NO-UNDO.
DEF VAR cTmpBaseQuery   AS CHAR   NO-UNDO.
DEF VAR cTmpCalcProc    AS CHAR   NO-UNDO.
DEF VAR cLabelList      AS CHAR   NO-UNDO.
DEF VAR cColumnList     AS CHAR   NO-UNDO.
DEF VAR hColumn         AS HANDLE NO-UNDO.

DEF VAR chExcelApplication      AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet             AS COM-HANDLE NO-UNDO.
DEF VAR chInterior              AS COM-HANDLE NO-UNDO.
DEF VAR chBorder                AS COM-HANDLE NO-UNDO.
DEF VAR chBefore                AS COM-HANDLE NO-UNDO.
DEF VAR iCount                  AS INTEGER NO-UNDO.
DEF VAR iNumCols                AS INT NO-UNDO INIT 14.
DEF VAR cRange                  AS CHAR NO-UNDO.

IF cmbPrisProfil:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1" THEN DO:
  IF CAN-DO(cSprakLst,cSprak) THEN
      iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,"Vis prisrapport för artiklar med avvikende pris?" + CHR(10) + "(inom urval)","","").
  ELSE
      iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,"Vis prisrapport for artikler med avvikende priser?" + CHR(10) + "(innenfor gjeldende utvalg)","","").

  IF iReturn <> 6 THEN RETURN.
  ELSE DO:
    ASSIGN cTmpBaseQuery = DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery")
           cTmpCalcProc  = DYNAMIC-FUNCTION("getAttribute",hBrowse,"calcFieldProc").
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE TRUE"). */
/*     DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","").  */
    DO ix = 1 TO hBrowse:NUM-COLUMNS:
      ASSIGN hColumn = hBrowse:GET-BROWSE-COLUMN(ix)
             NO-ERROR.
      IF VALID-HANDLE(hColumn) AND hColumn:VISIBLE THEN
        ASSIGN cLabelList  = cLabelList + REPLACE(DYNAMIC-FUNCTIO("getStrippedSortLabel",hColumn),"!"," ") + ";"
               cColumnList = cColumnList + hColumn:NAME + ","
               .
    END.
    IF DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"artpris_profilpris_rapport.p",cLabelList + "|" + cColumnList) THEN DO:
      cFileName = DYNAMIC-FUNCTION("getTransactionMessage").

      chExcelApplication = DYNAMIC-FUNCTION("getExcelHandle").
      IF NOT VALID-HANDLE(chExcelApplication) THEN DO:
        IF DYNAMIC-FUNCTION("setWebDoc","Open",cFileName) NE "" THEN
        DO:
            IF CAN-DO(cSprakLst,cSprak) THEN
                MESSAGE "Kan ikke åpne fil: " cFileName VIEW-AS ALERT-BOX ERROR.
            ELSE
                MESSAGE "Kan inte öppna fil: " cFileName VIEW-AS ALERT-BOX ERROR.
        END.
        RETURN.
      END.

      chExcelApplication:Workbooks:OpenText(cFileName,2,,,,,TRUE).

      chExcelApplication:ScreenUpdating = FALSE.

      ASSIGN chWorkbook                           = chExcelApplication:WorkBooks:ITEM(1)
             chWorkSheet                          = chExcelApplication:Sheets:ITEM(1)
             chWorkSheet:NAME                     = "Prisavvik"

             chWorkSheet:Rows(1):FONT:Bold        = TRUE
             chWorkSheet:PageSetup:PrintTitleRows = "$1:$1"
             chWorkSheet:Range("A:A"):NumberFormat = "##0"
             chWorkSheet:Range("G:I"):NumberFormat = "# ##0,00"
             chWorkSheet:Range("D:D"):columnwidth  = chWorkSheet:Range("C:C"):columnwidth * 3
             .             
      chWorkSheet:Range("A2"):Select().
      ASSIGN chExcelApplication:ActiveWindow:FreezePanes = TRUE.

      chExcelApplication:ScreenUpdating = YES.
      chExcelApplication:VISIBLE = TRUE.

    END.

  END.
  /*ELSE RUN SUPER.*/
END.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSelectorAttributes C-Win 
PROCEDURE getSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  ihSelectorSource AS HANDLE NO-UNDO.
DEF INPUT PARAM  ihSelectorTarget AS HANDLE NO-UNDO.
DEF INPUT PARAM  icDeSelRowidList AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiReturn         AS INT NO-UNDO.

DEF VAR cTmpHuvGrList    AS CHAR NO-UNDO.
DEF VAR cTmpVarGrList    AS CHAR NO-UNDO.

cCurrSelectBuffer = ihSelectorSource:QUERY:GET-BUFFER-HANDLE(1):NAME.
iSelectorSourceCount = INT(DYNAMIC-FUNCTION("getAttribute",ihSelectorSource,"totalcount")).

/* Håndtering av avhengighet Avdeling/HuvGr: */
IF cCurrSelectBuffer = "HuvGr" THEN DO WITH FRAME {&FRAME-NAME}:
  cHuvGrAvdelingList = "".
  ihSelectorTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT ihSelectorTarget:QUERY:QUERY-OFF-END:
    cHuvGrAvdelingList = cHuvGrAvdelingList + STRING(ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("AvdelingNr"):BUFFER-VALUE) + ",".
    ihSelectorTarget:QUERY:GET-NEXT().
  END.  
  cHuvGrAvdelingList = TRIM(cHuvGrAvdelingList,",").

  IF cVarGrHuvGrList NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cVarGrRowIdList):
      bOK = ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE Hg = " + ENTRY(ix,cVarGrHuvGrList)) NO-ERROR.
      IF bOk THEN
        cTmpVarGrList = cTmpVarGrList + ENTRY(ix,cVarGrRowIdList) + ",".
    END.
    IF NUM-ENTRIES(TRIM(cTmpVarGrList,",")) NE NUM-ENTRIES(cVarGrRowIdList) THEN DO:
      ASSIGN cVarGrRowIdList         = ""
             cVarGrIdList            = ""
             sokVg:SCREEN-VALUE      = "0"
             sokVgBeskr:SCREEN-VALUE = ""
             .
    END.
  END.
END.
ELSE IF cCurrSelectBuffer = "Avdeling" AND cHuvGrAvdelingList NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cHuvGrRowIdList):
    bOK = ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE AvdelingNr = " + ENTRY(ix,cHuvGrAvdelingList)) NO-ERROR.
    IF bOk THEN
      cTmpHuvGrList = cTmpHuvGrList + ENTRY(ix,cHuvGrRowIdList) + ",".
  END.
  IF NUM-ENTRIES(TRIM(cTmpHuvGrList,",")) NE NUM-ENTRIES(cHuvGrRowIdList) THEN DO:
    ASSIGN cHuvGrRowIdList         = ""
           cHuvGrIdList            = ""
           cVarGrRowIdList         = ""
           cVarGrIdList            = ""
           sokHg:SCREEN-VALUE      = "0"
           sokHgBeskr:SCREEN-VALUE = ""
           sokVg:SCREEN-VALUE      = "0"
           sokVgBeskr:SCREEN-VALUE = ""
           .
  END.
END.

/* Håndtering av avhengighet HuvGr/VarGr: */
ELSE IF cCurrSelectBuffer = "VarGr" THEN DO:
  cVarGrHuvGrList = "".
  ihSelectorTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT ihSelectorTarget:QUERY:QUERY-OFF-END:
    cVarGrHuvGrList = cVarGrHuvGrList + STRING(ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Hg"):BUFFER-VALUE) + ",".
    ihSelectorTarget:QUERY:GET-NEXT().
  END.  
  cVarGrHuvGrList = TRIM(cVarGrHuvGrList,",").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR hSearchField AS HANDLE      NO-UNDO.
DEF VAR cButNr       AS CHARACTER   NO-UNDO.
DEF VAR cProfilNr    AS CHARACTER   NO-UNDO.
DEF VAR iBrGrpNr     AS INT         NO-UNDO.
RUN enable_UI.

DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

iBrGrpNr = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrGrpNr").
cCL      = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").
IF CAN-DO('1,J,Ja,Yes,True',STRING(DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 2 and SysGr = 20 and ParaNr = 1","Parameter1"))) 
          THEN bEditInnPris = YES.
ELSE bEditInnPris = FALSE.

/* Brynjar: Har ikke satt parameter for bEditInnPris
bEditInnPris = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1") = "1".
*/


DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

hViewRecCnt = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"RecordCountWidget")) NO-ERROR.

cButNr      = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr").
iBrukerType = int(DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType")).
cSprak      = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","Lng").
/* TN 21/2-10 Bruker som ikke er koblet til butikk, har tilgang til alle butikker. Eller mot sentrallager. 
IF cButNr = ? OR cButNr = "0" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Bruker er ikke satt opp mot butikk","","").
  APPLY "close" TO THIS-PROCEDURE.
  RETURN.
END.
*/
DO WITH FRAME {&FRAME-NAME}:

  IF cButNr = ? OR cButNr = "0" OR NOT CAN-FIND(Butiker WHERE Butiker.Butik = INT(cButNr)) THEN 
    ASSIGN cButNr = cCL
           tbEgneVarer:CHECKED = NO.
  
  cProfilNr = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE butik = " + cButNr,"ProfilNr").

  iHKProfilnr = 1.
  FIND butiker WHERE butiker.butik = INT(cButNr) NO-LOCK NO-ERROR.
  IF Butiker.Sentrallager = FALSE THEN DO:
      iCLBut = Butiker.clButikkNr.
      FIND butiker WHERE butiker.butik = iCLBut NO-LOCK NO-ERROR.
      iHKProfilnr = butiker.profilnr.
  END.


  IF cProfilNr = ? OR cProfilnr = "0" THEN DO:
      IF CAN-DO(cSprakLst,cSprak) THEN
          DYNAMIC-FUNCTION("DoMessage",0,0,"Butik " + cButNr + " saknar prisprofil","","").
      ELSE
          DYNAMIC-FUNCTION("DoMessage",0,0,"Butikk " + cButNr + " mangler prisprofil","","").

    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.

  ASSIGN cmbSaSong:DELIMITER = "|"
         cmbSaSong:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","SaSong;SasBeskr;SaSong","WHERE Aktiv AND SasBeskr NE ''"),"|")
         cmbSaSong:SCREEN-VALUE = "0"

         cmbPrisProfil:DELIMITER = "|"
         /*
         cmbPrisProfil:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Prisprofil;Profilnr|Beskrivelse;Profilnr",
                                                          "WHERE " + (IF cProfilnr NE "1" THEN "Profilnr = '" + cProfilnr + "'" ELSE "true") + " BY Profilnr")
         
         */
         cmbPrisProfil:LIST-ITEM-PAIRS = RIGHT-TRIM(DYNAMIC-FUNCTION("getFieldList","Butikktilgang,Butiker,PrisProfil;ProfilNr|Beskrivelse;ProfilNr"
                                            ,"WHERE BrGrpnr = " + STRING(iBrGrpNr) + ", FIRST Butiker where Butiker.Butik = ButikkTilgang.Butik, FIRST PrisProfil where PrisProfil.ProfilNr = Butiker.ProfilNr"),"|")


         cmbPrisProfil:SCREEN-VALUE = cProfilnr
         cmbPrisProfil:SENSITIVE = IF ((NUM-ENTRIES(cmbPrisProfil:LIST-ITEM-PAIRS,'|') > 2) OR (cProfilnr = "1"))
                                     THEN TRUE
                                     ELSE FALSE
         cmbPrisProfil:BGCOLOR = 15

         cmbButikk:DELIMITER = "|"
         /*
         cmbButikk:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Butiker;butik|butnamn;butik",
                                                          "WHERE " + (IF cButNr = cCL THEN "true" ELSE "butik = '" + cButNr + "'") + " BY butik")
         */
         cmbButikk:LIST-ITEM-PAIRS = RIGHT-TRIM(DYNAMIC-FUNCTION("getFieldList","Butikktilgang,Butiker;ButNamn|Butik;Butik"
                                               ,"WHERE BrGrpnr = " + STRING(iBrGrpNr) + ", FIRST Butiker where Butiker.Butik = ButikkTilgang.Butik"),"|")

         cmbButikk:SCREEN-VALUE = cButNr
         cmbButikk:SENSITIVE = IF ((NUM-ENTRIES(cmbButikk:LIST-ITEM-PAIRS,'|') > 2) OR (cButNr = cCL))
                                 THEN TRUE
                                 ELSE FALSE
         cmbButikk:BGCOLOR  = 15
         INPUT cmbButikk.

  RUN VisMiniBilde.w PERSIST SET hArtBilde.
  RUN InitializeObject IN hArtBilde (ArtikkelBilde:HANDLE).
  hArtBildeFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hArtBilde).

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwArtpris:HANDLE
          ,200
          ,"multiple"
          ,"ArtPris"
          + ";ArtikkelNr|Art.nr"
          + ";+AktivPris|DECIMAL|->>><>>9.99|aktivpris(rowid)|Aktiv pris"
          + ";+AktivDB%|DECIMAL|->>>>9.99|aktivdb%(rowid)|DB% aktiv"
          + ";+PrisKoPris|DECIMAL|->>><>>9.99|priskopris(rowid)|Priskø-pris"
          + ";Pris[1]|HK pris"
          + ";+PrisKoInnpris|DECIMAL|->>><>>9.99|priskoinnpris(rowid)|Priskø-innpris"
/*          + ";+PrisKoVarekost|DECIMAL|->>><>>9.99|priskovarekost(rowid)|Priskø-varekost" */
          + ";Varekost[1]|HK v.kost"
          + ";DB%[1]|DB% HK"
          + ";!ProfilNr"
/*           + ";+DatoSisteSalg|DATE|99/99/99|datosistesalg(rowid)|Sist solgt" */
          + ";!+VisAktivPrisFilter|CHARACTER|xx|aktiv_pris_filter(ROWID)|Lok.pris filter"
        + ",ArtBas"
          + ";Beskr|Beskrivelse|X(255)@3"
          + ";LevKod|Lev.artnr@2"
          + ";LevFargKod|Lev.farge@4"
          + ";LevNr|Lev.nr"
          + ";ProdNr"
          + ";VMid|V.merke"
          + ";Vg"
          + ";!LopNr"
          + ";!BildNr"
          + ";!Opris"
          + ";!LokPris"
          + ";!+OprisFilter|CHARACTER|x|oprisfilter(rowid)"
          + ";!Pant"
          + ";!Utgatt"
        + ",Produsent"
          + ";Beskrivelse|Produsent@" + (IF bEditInnPris THEN "17" ELSE "16")
        + ",Varemerke"
          + ";Beskrivelse|Varemerketekst@" + (IF bEditInnPris THEN "19" ELSE "18")
        + ",LevBas"
          + ";LevNamn@13"
        + ",SaSong"
          + ";SasBeskr|Sesong@14"
        + ",VarGr"
          + ";VgBeskr|Varegruppe"
          + ";Hg"
        + ",HuvGr"
          + ";HgBeskr|Hovedgruppe"
          + ";AvdelingNr"
        + ",Avdeling"
          + ";AvdelingNavn|Avdeling"
        + ",Strekkode"
          + ";!kode"
          ,"WHERE false"
         + ",FIRST ArtBas NO-LOCK OF ArtPris" /* WHERE NOT Opris NE no" */
         + ",FIRST Produsent NO-LOCK OF ArtBas OUTER-JOIN"
         + ",FIRST Varemerke NO-LOCK OF ArtBas OUTER-JOIN"
         + ",FIRST LevBas NO-LOCK OF ArtBas"
         + ",FIRST SaSong NO-LOCK OF ArtBas OUTER-JOIN"
         + ",FIRST VarGr NO-LOCK OF ArtBas"
         + ",FIRST HuvGr NO-LOCK OF VarGr"
         + ",FIRST Avdeling NO-LOCK OF HuvGr"
         + ",FIRST Strekkode NO-LOCK OF ArtBas OUTER-JOIN"
          ,"").

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  ASSIGN cQueryJoin      = DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryJoin")
         cSmallQueryJoin = "," + ENTRY(2,cQueryJoin)
         cBuffersAndFlds = DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersAndFields")
         cBufferList     = DYNAMIC-FUNCTION("getAttribute",hBrowse,"bufferList")
         .
  DO ix = 1 TO 2:
    ASSIGN cSmallBuffersAndFlds = cSmallBuffersAndFlds + (IF cSmallBuffersAndFlds NE "" THEN "," ELSE "") + ENTRY(ix,cBuffersAndFlds)
           cSmallBufferList     = cSmallBufferList + (IF cSmallBufferList NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,cBuffersAndFlds),";")
           .
  END.

  APPLY "value-changed" TO tbAvansertFilter.

/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE ProfilNr = 1"). */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE ProfilNr = " + STRING(iHKProfilnr)).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","artpris_prisko_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamPrisKoPris",cProfilNr).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamAktivPris",cProfilNr).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamAktivDB%",cProfilNr).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"altPrimaryBufferList","VarGr,ArtBas").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"checkForIndexBeforeSort","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanLimit","100000").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryVargr","EACH ArtBas NO-LOCK OF VarGr,EACH ArtPris NO-LOCK OF ArtBas").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryHuvgr","EACH VarGr NO-LOCK OF HuvGr,EACH ArtBas NO-LOCK OF VarGr,EACH ArtPris NO-LOCK OF ArtBas").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryAvdeling","EACH HuvGr NO-LOCK OF Avdeling,EACH VarGr NO-LOCK OF HuvGr,EACH ArtBas NO-LOCK OF VarGr,EACH ArtPris NO-LOCK OF ArtBas").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryProdusent","EACH ArtBas NO-LOCK OF Produsent,EACH ArtPris NO-LOCK OF ArtBas").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryVaremerke","EACH ArtBas NO-LOCK OF Varemerke,EACH ArtPris NO-LOCK OF ArtBas").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryLevBas","EACH ArtBas NO-LOCK OF LevBas,EACH ArtPris NO-LOCK OF ArtBas").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryStrekkode","EACH ArtBas NO-LOCK OF Strekkode,EACH ArtPris NO-LOCK OF ArtBas").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"altPrimaryBufferList","ArtBas,LevBas").

  ASSIGN hFieldPris       = hBuffer:BUFFER-FIELD("Pris[1]")
         hFieldPrisKoPris = hBuffer:BUFFER-FIELD("PrisKoPris")
         hFieldAktivPris  = hBuffer:BUFFER-FIELD("AktivPris")
         hFieldOpris      = hBuffer:BUFFER-FIELD("Opris")
         hFieldUtgatt     = hBuffer:BUFFER-FIELD("Utgatt")
         hFieldDB%        = hBuffer:BUFFER-FIELD("DB%[1]")
         hFieldAktivDB%   = hBuffer:BUFFER-FIELD("AktivDB%")
         hColPrisKoPris   = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"PrisKoPris")
         hColPris         = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"Pris[1]")
         hColAktivPris    = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"AktivPris")
         hColDB%          = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"DB%[1]")
         hColAktivDB%     = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"AktivDB%")
         hColArtnr        = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"ArtikkelNr")
         .

  hPrisKoPris  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"PriskoPris","PriskoPris"
                 ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hPrisKoPris,"PriskoPris").
/*   DYNAMIC-FUNCTION("setAttribute",hPrisKoPris,"refreshRow","yes").  */
  hPrisKoPris:HELP = "Registrer egen pris for overføring av artikkel til priskø".

  IF bEditInnPris THEN DO:
    hPrisKoInnPris = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"PriskoInnPris","PriskoInnPris"
                  ,"","","","").
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hPrisKoInnPris,"PriskoInnPris").
    hPrisKoInnPris:HELP = "Brutto innpris for overføring av artikkel til priskø".
  END.

  DYNAMIC-FUNCTION("setSortString",hBrowse,"ArtikkelNr").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",brwSearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
          ,tbArtpris:HANDLE
          ,""
          ,"Refresh,TilEtikett;Legg til etikettkø,ScannTilKo;Scann for eitkettkø"
        + ",browseConfig;Kolonneoppsett"
        + (IF iBrukerType <> 2 THEN ",excel" ELSE "")
        + ";Eksporter til E&xcel"
          ,"maxborder").
          
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                  ,"MultiSortBrowse;Sorter på flere kolonner"
                  ,"").

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",KoFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

/*   IF SEARCH("controls.dll") NE ? THEN                                                                      */
/*     DYNAMIC-FUNCTION("setImageList" IN hTabFolder,"newbmp\user_green.bmp;application_add.bmp;bullet.bmp"). */

  IF CAN-DO(cSprakLst,cSprak) THEN
    DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Priskö|prisko_brw.w|Etikettkö|etikett_brw.w|Streckkoder|strekkode_brw.w",hBrowse).
  ELSE 
    DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Priskø|prisko_brw.w|Etikettkø|etikett_brw.w|Strekkoder|strekkode_brw.w",hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).


  RUN InvokeMethod(hBrowse,"OpenQuery").
END.


DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

InitializeResize().

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,200,0,0).
/* FI-X:SCREEN-VALUE = STRING(iHKProfilnr). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO.

IF ihFillIn = hPrisKoPris OR ihFillIn = hPrisKoInnPris THEN DO WITH FRAME {&FRAME-NAME}:

  obOk = DYNAMIC-FUNCTION("runProc","prisko_create.p",
                          /* Iom at buffer-rekkefølgen kan snus for effektiv sortering (uten søkekriterier) må rowid for Artpris hentes fra rett sted: */
                          DYNAMIC-FUNCTION("getPrimaryRowIdent",hBrowse) 
                  + "|" + ihFillIn:SCREEN-VALUE + "|" + cmbPrisProfil:SCREEN-VALUE + "|" + ihFillIn:NAME,?).
  IF NOT obOk AND DYNAMIC-FUNCTION("getTransactionMessage") NE "" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    IF ihFillIn = hPrisKoPris THEN
      ihBuffer:BUFFER-FIELD("PrisKoPris"):BUFFER-VALUE = DECIMAL(ihFillIn:SCREEN-VALUE).
    ELSE IF ihFillIn = hPrisKoInnPris THEN
      ihBuffer:BUFFER-FIELD("PrisKoInnPris"):BUFFER-VALUE = DECIMAL(ihFillIn:SCREEN-VALUE).
    RUN InvokeMethod(hCurrTabQuery,"OpenQuery").
  END.
END.
ELSE 
  obOk = DYNAMIC-FUNCTION("DoUpdate",ihBuffer:NAME,
            "",
            "",
            ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
            DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
            ihFillIn:SCREEN-VALUE,
            YES).
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       For å optimere filter på Opris (som er kanskje 10% av artiklene) så brukes 2 metoder:
            1: Vi ønsker å vise Opris varer:
               Kriteriet legges inn som prescan for artbas. 
            2: Vi vil ikke vise Opris varer:
               Filtrer ut Opris ved å bruke et kalkulert felt
------------------------------------------------------------------------------*/
DEF VAR cPrescanArtbas      AS CHAR NO-UNDO.
DEF VAR cPrescanStrekkode   AS CHAR NO-UNDO.
DEF VAR cPrescanVargr       AS CHAR NO-UNDO.
DEF VAR cPrescanHuvgr       AS CHAR NO-UNDO.
DEF VAR cPrescanAvd         AS CHAR NO-UNDO.
DEF VAR cPrescanLev         AS CHAR NO-UNDO.
DEF VAR cPrescanProd        AS CHAR NO-UNDO.
DEF VAR cPrescanVaremerke   AS CHAR NO-UNDO.
DEF VAR cBaseQuery          AS CHAR NO-UNDO.
DEF VAR cPrescanArtpris     AS CHAR NO-UNDO.
DEF VAR cPrescanStlinje     AS CHAR NO-UNDO.
DEF VAR cPrescanPrisko      AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fiBeskr fiLevKod fiLevFargKod rsArtPLU.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamPrisKoPris",cmbPrisProfil:SCREEN-VALUE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamAktivPris",cmbPrisProfil:SCREEN-VALUE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamAktivDB%",cmbPrisProfil:SCREEN-VALUE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamDatoSisteSalg",cmbButikk:SCREEN-VALUE + (IF tbEgneVarer:CHECKED THEN "¤yes" ELSE "")).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamVisAktivPrisFilter",IF tbVarerLokPris:CHECKED THEN cmbPrisProfil:SCREEN-VALUE ELSE "").

  IF rsArtPLU = 1 THEN
    /*DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamOprisFilter","no").*/
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamOprisFilter","").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamOprisFilter","").

  ASSIGN  cPrescanArtbas = (IF fiBeskr NE "" THEN
                              " AND Beskr "
                            + (IF INDEX(fiBeskr,"*") > 0 AND INDEX(fiBeskr,"*") NE LENGTH(fiBeskr) THEN "MATCHES '" ELSE "BEGINS '")
                            + fiBeskr + "'"
                            ELSE "")
                         + (IF fiLevKod NE "" THEN
                            " AND LevKod "
                            + (IF INDEX(fiLevKod,"*") > 0 AND INDEX(fiLevKod,"*") NE LENGTH(fiLevKod) THEN "MATCHES '" ELSE "BEGINS '")
                            + fiLevKod + "'"
                            ELSE "")
                         + (IF fiLevFargKod NE "" THEN
                            " AND LevFargKod "
                            + (IF INDEX(fiLevFargKod,"*") > 0 AND INDEX(fiLevFargKod,"*") NE LENGTH(fiLevFargKod) THEN "MATCHES '" ELSE "BEGINS '")
                            + fiLevFargKod + "'"
                            ELSE "")
                         + (IF sokVg:SCREEN-VALUE NE "0" THEN
                             " AND Vg = " + sokVg:SCREEN-VALUE
                            ELSE IF sokVgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVarGrRowIdList) > 1 THEN
                             " AND " + BuildORlist("ArtBas.Vg",cVarGrIdList,"") 
                            ELSE "")
                         + (IF cmbSaSong:SCREEN-VALUE NE "0" AND cmbSaSong:SCREEN-VALUE NE ? THEN
                             " AND SaSong = " + cmbSaSong:SCREEN-VALUE
                            ELSE "")
                         + (IF sokLevNr:SCREEN-VALUE NE "0" THEN
                             " AND ArtBas.LevNr = " + sokLevNr:SCREEN-VALUE 
                            ELSE IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) > 1 THEN
                            " AND " + BuildORlist("ArtBas.LevNr",cLevBasIdList,"") 
                            ELSE "")
                         + (IF sokProdNr:SCREEN-VALUE NE "0" THEN
                             " AND ArtBas.ProdNr = " + sokProdNr:SCREEN-VALUE
                            ELSE IF sokProdusent:SCREEN-VALUE NE "" AND NUM-ENTRIES(cProdRowIdList) > 1 THEN
                            " AND " + BuildORlist("ArtBas.ProdNr",cProdIdList,"") 
                           ELSE "")
                         + (IF sokVMId:SCREEN-VALUE NE "0" THEN
                             " AND VMid = " + sokVMId:SCREEN-VALUE
                            ELSE IF sokVaremerke:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVaremerkeRowIdList) > 1 THEN
                            " AND " + BuildORlist("ArtBas.VMid",cVaremerkeIdList,"") 
                           ELSE "")
                         + (IF rsArtPLU = 2 THEN
                             " AND Opris"
                            ELSE IF rsArtPLU = 3 THEN
                             " AND Pant"
                            ELSE "")
          cPrescanVargr = (IF sokHg:SCREEN-VALUE NE "0" THEN
                            " AND Hg = " + sokHg:SCREEN-VALUE
                           ELSE IF sokHgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cHuvGrRowIdList) > 1 THEN
                            " AND " + BuildORlist("Vargr.Hg",cHuvGrIdList,"") 
                           ELSE "")
                        + (IF sokVgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVarGrRowIdList) < 2 THEN
                           " AND " +
                            (IF INDEX(sokVgBeskr:SCREEN-VALUE,"*") > 0 AND INDEX(sokVgBeskr:SCREEN-VALUE,"*") NE LENGTH(sokVgBeskr:SCREEN-VALUE) THEN
                              "VgBeskr MATCHES '" + sokVgBeskr:SCREEN-VALUE + "*'"
                             ELSE
                              "VgBeskr BEGINS '" + TRIM(sokVgBeskr:SCREEN-VALUE,"*") + "'")
                           ELSE "") 
          cPrescanHuvgr = (IF sokAvdelingNr:SCREEN-VALUE NE "0" AND cPrescanVargr = "" THEN
                            " AND HuvGr.AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE
                           ELSE IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) > 1 THEN
                            " AND " + BuildORlist("Huvgr.AvdelingNr",cAvdelingIdList,"") 
                           ELSE "")
                        + (IF sokHgBeskr:SCREEN-VALUE NE "" AND NUM-ENTRIES(cHuvGrRowIdList) < 2 AND cPrescanVargr = "" THEN
                           " AND " +
                            (IF INDEX(sokHgBeskr:SCREEN-VALUE,"*") > 0 AND INDEX(sokHgBeskr:SCREEN-VALUE,"*") NE LENGTH(sokHgBeskr:SCREEN-VALUE) THEN
                              "HgBeskr MATCHES '" + sokHgBeskr:SCREEN-VALUE + "*'"
                             ELSE
                              "HgBeskr BEGINS '" + TRIM(sokHgBeskr:SCREEN-VALUE,"*") + "'")
                           ELSE "") 
          cPrescanAvd   = (IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cHuvGrRowIdList) < 2 AND cPrescanHuvgr = "" THEN
                           " AND " +
                            (IF INDEX(sokAvdelingNavn:SCREEN-VALUE,"*") > 0 AND INDEX(sokAvdelingNavn:SCREEN-VALUE,"*") NE LENGTH(sokAvdelingNavn:SCREEN-VALUE) THEN
                              "Avdeling.AvdelingNavn MATCHES '" + sokAvdelingNavn:SCREEN-VALUE + "*'"
                             ELSE
                              "Avdeling.AvdelingNavn BEGINS '" + TRIM(sokAvdelingNavn:SCREEN-VALUE,"*") + "'")
                           ELSE "") 
          cPrescanLev   = (IF sokLevNamn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cLevBasRowIdList) < 2 AND sokLevNr:SCREEN-VALUE = "0" THEN
                           " AND " +
                            (IF INDEX(sokLevNamn:SCREEN-VALUE,"*") > 0 AND INDEX(sokLevNamn:SCREEN-VALUE,"*") NE LENGTH(sokLevNamn:SCREEN-VALUE) THEN
                              "LevNamn MATCHES '" + sokLevNamn:SCREEN-VALUE + "*'"
                             ELSE
                              "LevNamn BEGINS '" + TRIM(sokLevNamn:SCREEN-VALUE,"*") + "'")
                           ELSE "") 
          cPrescanProd  = (IF sokProdusent:SCREEN-VALUE NE "" AND NUM-ENTRIES(cProdRowIdList) < 2 AND sokProdNr:SCREEN-VALUE = "0" THEN
                           " AND " +
                            (IF INDEX(sokProdusent:SCREEN-VALUE,"*") > 0 AND INDEX(sokProdusent:SCREEN-VALUE,"*") NE LENGTH(sokProdusent:SCREEN-VALUE) THEN
                              "Produsent.Beskrivelse MATCHES '" + sokProdusent:SCREEN-VALUE + "*'"
                             ELSE
                              "Produsent.Beskrivelse BEGINS '" + TRIM(sokProdusent:SCREEN-VALUE,"*") + "'")
                           ELSE "") 
          cPrescanVaremerke  = (IF sokVaremerke:SCREEN-VALUE NE "" AND NUM-ENTRIES(cVaremerkeRowIdList) < 2 AND sokVMId:SCREEN-VALUE = "0" THEN
                           " AND " +
                            (IF INDEX(sokVaremerke:SCREEN-VALUE,"*") > 0 AND INDEX(sokVaremerke:SCREEN-VALUE,"*") NE LENGTH(sokVaremerke:SCREEN-VALUE) THEN
                              "Varemerke.Beskrivelse MATCHES '" + sokVaremerke:SCREEN-VALUE + "*'"
                             ELSE
                              "Varemerke.Beskrivelse BEGINS '" + TRIM(sokVaremerke:SCREEN-VALUE,"*") + "'")
                           ELSE "") 
          cPrescanArtpris = (IF tbVarerLokPris:CHECKED THEN
                              "buf1_ArtPris WHERE buf1_Artpris.Profilnr = " + cmbPrisProfil:SCREEN-VALUE 
                            + ",FIRST ArtPris NO-LOCK WHERE ArtPris.ArtikkelNr = buf1_ArtPris.ArtikkelNr AND ArtPris.Profilnr = 1 AND ArtPris.Pris[1] NE buf1_ArtPris.Pris[1]"
                             ELSE "")
          cPrescanStlinje = (IF tbEgneVarer:CHECKED THEN
                               "Stlinje WHERE Stlinje.butik = " + cmbButikk:SCREEN-VALUE + " AND Stlinje.StTypeId = 'artikkel' AND Stlinje.Perid = 'aar',FIRST ArtPris NO-LOCK WHERE ArtPris.ArtikkelNr = DECIMAL(Stlinje.DataObjekt) AND Artpris.Profilnr = 1"
                             ELSE "")
          cPrescanPrisko = (IF tbVarerPrisko:CHECKED THEN
                              "Prisko WHERE Prisko.Profilnr = " + cmbPrisProfil:SCREEN-VALUE + ",FIRST ArtPris NO-LOCK WHERE ArtPris.ArtikkelNr = Prisko.Artikkelnr AND Artpris.Profilnr = 1"
                            ELSE "")
                           .


  /* Dersom buffer-rekkefølgen er snudd for effektiv sortering (uten søkekriterier) må den settes tilbake orginal rekkefølge for at prescan skal fungere: */
  IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgBaseQuery") NE "" THEN
    DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",hBrowse,"","").
  
  cBaseQuery    = " " + DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery").

  IF cPrescanArtbas NE "" THEN
    cPrescanArtbas = "ArtBas WHERE true" + cPrescanArtbas + ",EACH Artpris OF ArtBas NO-LOCK" + cBaseQuery.

  IF cPrescanVargr NE "" THEN
    cPrescanVargr = "VarGr WHERE true" + cPrescanVargr + ",EACH ArtBas NO-LOCK OF VarGr,EACH ArtPris OF ArtBas NO-LOCK" + cBaseQuery.
  
  IF cPrescanHuvgr NE "" THEN
    cPrescanHuvgr = "HuvGr WHERE true" + cPrescanHuvgr + ",EACH VarGr NO-LOCK OF HuvGr,EACH ArtBas NO-LOCK OF VarGr,EACH ArtPris OF ArtBas NO-LOCK" + cBaseQuery.

  IF cPrescanAvd NE "" THEN
    cPrescanAvd = "Avdeling WHERE true" + cPrescanAvd + ",EACH HuvGr NO-LOCK OF Avdeling,EACH VarGr NO-LOCK OF HuvGr,EACH ArtBas NO-LOCK OF VarGr,EACH ArtPris OF ArtBas NO-LOCK" + cBaseQuery.
  
  IF cPrescanLev NE "" THEN
    cPrescanLev = "LevBas WHERE true" + cPrescanLev + ",EACH ArtBas NO-LOCK OF LevBas,EACH ArtPris OF ArtBas NO-LOCK" + cBaseQuery.
  
  IF cPrescanProd NE "" THEN
    cPrescanProd = "Produsent WHERE true" + cPrescanProd + ",EACH ArtBas NO-LOCK OF Produsent,EACH ArtPris OF ArtBas NO-LOCK" + cBaseQuery.

  IF cPrescanVaremerke NE "" THEN
    cPrescanVaremerke = "Varemerke WHERE true" + cPrescanVaremerke + ",EACH ArtBas NO-LOCK OF Varemerke,EACH ArtPris OF ArtBas NO-LOCK" + cBaseQuery.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                   TRIM(cPrescanArtbas 
                     + "|" + cPrescanVargr
                     + "|" + cPrescanHuvgr 
                     + "|" + cPrescanAvd 
                     + "|" + cPrescanLev 
                     + "|" + cPrescanProd 
                     + "|" + cPrescanVaremerke 
                     + "|" + cPrescanArtpris
                     + "|" + cPrescanStlinje
                     + "|" + cPrescanPrisko
                   ,"|")
                   ).
                   
END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icDirection AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",icDirection) THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","yes").
  hBrowse:SELECT-FOCUSED-ROW().
  CASE icDirection:
    WHEN "Prev" THEN
        hBrowse:SELECT-PREV-ROW().
    WHEN "Next" THEN
      hBrowse:SELECT-NEXT-ROW().
  END CASE.
  APPLY "value-changed" TO hBrowse.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowseRecord C-Win 
PROCEDURE RefreshBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN RefreshRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCurrRowid1 AS CHAR   NO-UNDO.
DEF VAR iReposRow   AS INT    NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR cRowIdList   AS CHAR   NO-UNDO.
DEF VAR cPrisKoNum   AS CHAR   NO-UNDO.
DEF VAR hQueryBuff   AS HANDLE NO-UNDO.
DEF VAR cBufferList  AS CHAR   NO-UNDO.
DEF VAR iRowCnt      AS INT    NO-UNDO.
DEF VAR bTooManyRows AS LOG    NO-UNDO.

cBufferList = DYNAMIC-FUNCTION("getAttribute",hBrowse,"bufferList").

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersAndFields") = cSmallBuffersAndFlds THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferList","cSmallBufferList").

IF tbVarerLokPris:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    ASSIGN cCurrRowid1 = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
           iReposRow   = hBrowse:FOCUSED-ROW.
  
  RUN InvokeMethod(hBrowse,"OpenQuery").
  
  IF cCurrRowId1 NE "" THEN DO:
    bOk = hBrowse:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE RowIdent1 = '" + cCurrRowId1 + "'") NO-ERROR.
    IF bOk THEN DO:
      hBrowse:SET-REPOSITIONED-ROW(iReposRow,"conditional").
      hBrowse:QUERY:REPOSITION-TO-ROWID(hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
      APPLY "value-changed" TO hBrowse.
    END.
  END.
END.
ELSE DO:
  cPrisKoNum = getPrisKoBufferNum().
  
  CREATE BUFFER hQueryBuff FOR TABLE hBuffer.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hQueryBuff).
  hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuff:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    cRowIdList = cRowIdList + (IF cRowIdList NE "" THEN "," ELSE "") + hQueryBuff:BUFFER-FIELD("RowIdent" + cPrisKoNum):BUFFER-VALUE.
    hQuery:GET-NEXT().
    iRowCnt = iRowCnt + 1.
    IF iRowCnt > 100 THEN DO:
      bTooManyRows = YES.
      LEAVE.
    END.
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hQueryBuff NO-ERROR.
  
  IF NOT bTooManyRows THEN
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,cRowIdList).
  ELSE DO:
    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
      ASSIGN cCurrRowid1 = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
             iReposRow   = hBrowse:FOCUSED-ROW.

    RUN InvokeMethod(hBrowse,"OpenQuery").

    IF cCurrRowId1 NE "" THEN DO:
      bOk = hBrowse:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE RowIdent1 = '" + cCurrRowId1 + "'") NO-ERROR.
      IF bOk THEN DO:
        hBrowse:SET-REPOSITIONED-ROW(iReposRow,"conditional").
        hBrowse:QUERY:REPOSITION-TO-ROWID(hBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
        APPLY "value-changed" TO hBrowse.
      END.
    END.
  END.
END.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferList",cBufferList).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO: 
  
  IF hFieldOpris:BUFFER-VALUE THEN
    ASSIGN hColPris:BGCOLOR       = 8
           hColPrisKoPris:BGCOLOR = 8
           hColAktivPris:BGCOLOR  = 8
           hColDB%:BGCOLOR        = 8
           hColAktivDB%:BGCOLOR   = 8
           .
  ELSE DO:
    IF hFieldAktivDB%:BUFFER-VALUE > hFieldDB%:BUFFER-VALUE THEN
      hColAktivPris:BGCOLOR = 18.
    ELSE IF hFieldAktivDB%:BUFFER-VALUE < hFieldDB%:BUFFER-VALUE THEN
      hColAktivPris:BGCOLOR = 17.
    ELSE 
      hColAktivPris:BGCOLOR = ?.

/*
    IF hFieldPrisKoPris:BUFFER-VALUE NE 0 THEN DO:
      IF hFieldPrisKoPris:BUFFER-VALUE > hFieldAktivPris:BUFFER-VALUE THEN 
        hColPrisKoPris:BGCOLOR = 17 NO-ERROR.
      ELSE IF hFieldPrisKoPris:BUFFER-VALUE < hFieldAktivPris:BUFFER-VALUE THEN 
        hColPrisKoPris:BGCOLOR = 18 NO-ERROR.
      ELSE
        hColPrisKoPris:BGCOLOR = ?.
    END.
    IF hFieldAktivPris:BUFFER-VALUE > hFieldPris:BUFFER-VALUE THEN 
      hColAktivPris:BGCOLOR = 17 NO-ERROR.
    ELSE IF hFieldAktivPris:BUFFER-VALUE < hFieldPris:BUFFER-VALUE THEN 
      hColAktivPris:BGCOLOR = 18 NO-ERROR.
    ELSE
      hColAktivPris:BGCOLOR = ?.
*/      
  END.

  IF hFieldUtgatt:BUFFER-VALUE THEN
    hColArtnr:BGCOLOR = 17.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScannTilKoRecord C-Win 
PROCEDURE ScannTilKoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,2).

RUN ScannTilKoRecord IN hCurrTabProc (cmbPrisProfil:SCREEN-VALUE IN FRAME {&FRAME-NAME},cmbButikk:SCREEN-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME = "VarGr" THEN DO WITH FRAME frmLinje:
  IF sokAvdelingNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "0" THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"prescanqueryfilter","HuvGr WHERE HuvGr.AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE + ",EACH VarGr OF HuvGr NO-LOCK").
  ELSE IF cAvdelingIdList NE "" THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"prescanqueryfilter","HuvGr WHERE CAN-DO('" + REPLACE(cAvdelingIdList,"|",",") + "',STRING(HuvGr.AvdelingNr)),EACH VarGr OF HuvGr NO-LOCK").
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"basequery","WHERE "
                 + (IF sokHg:SCREEN-VALUE NE "0" THEN 
                      "Hg = " + sokHg:SCREEN-VALUE
                    ELSE IF cHuvGrIdList NE "" THEN
                      "CAN-DO('" + REPLACE(cHuvGrIdList,"|",",") + "',STRING(VarGr.Hg))"
                    ELSE "true")
                 + " AND "
                 + (IF sokVg:SCREEN-VALUE NE "0" THEN 
                      "Vg = " + sokVg:SCREEN-VALUE
                    ELSE IF cVarGrIdList NE "" THEN
                      "CAN-DO('" + REPLACE(cVarGrIdList,"|",",") + "',STRING(VarGr.Vg))"
                    ELSE "true")
                   ).
  RUN InvokeMethod(ihBrowse,"OpenQuery").
  APPLY "entry" TO ihBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSortSearch C-Win 
PROCEDURE StartSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hSearchField AS HANDLE NO-UNDO. 
DEF VAR cArtNr       AS CHAR   NO-UNDO.
DEF VAR cEAN AS CHAR NO-UNDO.

hSearchField = DYNAMIC-FUNCTION("getCurrentObject").

/*
IF hSearchField:NAME = "ArtikkelNr" THEN DO:
  cArtNr = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Kode = '" + hSearchField:SCREEN-VALUE + "'","Artikkelnr").
  IF cArtNr NE ? THEN
    hSearchField:SCREEN-VALUE = cArtNr.
END.
*/
IF hSearchField:NAME = "ArtikkelNr" THEN DO:
  ASSIGN 
     cEAN = TRIM(hSearchField:SCREEN-VALUE).
  RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
  ASSIGN
      hSearchField:SCREEN-VALUE = cEAN.

  cArtNr = DYNAMIC-FUNCTION("getFieldValues","Strekkode",
                             "WHERE Kode = '" + FILL("0",13 - LENGTH(hSearchField:SCREEN-VALUE))
                             + hSearchField:SCREEN-VALUE + "'",
                             "Artikkelnr").
  IF cArtNr NE ? THEN
    hSearchField:SCREEN-VALUE = cArtNr.
END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilEtikettRecord C-Win 
PROCEDURE TilEtikettRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cValue AS CHAR NO-UNDO.

  IF CAN-DO(cSprakLst,cSprak) THEN DO:
      RUN JBoxBrowseMsgUpdSelVal.w ("Bekräfta flyttning av artiklar till etikettkö",
                                     hBrowse:NUM-SELECTED-ROWS,
                                     0,
                                     "",
                                     OUTPUT cValue,
                                     OUTPUT iReturn).
  END.
  ELSE DO:
      RUN JBoxBrowseMsgUpdSelVal.w ("Bekreft flytting av artikler til etikettkø",
                                     hBrowse:NUM-SELECTED-ROWS,
                                     0,
                                     "",
                                     OUTPUT cValue,
                                     OUTPUT iReturn).
  END.


IF iReturn = 2 THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"RefreshProcessedRows","no").
  DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"etikettartpris_send_til.p",cmbPrisProfil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"RefreshProcessedRows","").
  IF iCurrTab NE 2 THEN
    DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,2).
  ELSE RUN InvokeMethod(hCurrTabQuery,"OpenQuery").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 77
       ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60
       ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 120
       ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 60
       ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 60
       ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 50
       ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 60
       ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS = 60
       .
IF bEditInnPris THEN
  ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 65.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildORlist C-Win 
FUNCTION BuildORlist RETURNS CHARACTER
  ( INPUT icField     AS CHAR,
    INPUT icValueList AS CHAR,
    INPUT icFunction  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cORlist AS CHAR NO-UNDO INIT "(".
DEF VAR ix      AS INT  NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icValueList,"|"):
  cORlist = cORlist 
          + (IF cORlist NE "(" THEN " OR " ELSE "")
          + icField + " = " + (IF icFunction NE "'" AND icFunction NE "" THEN 
                                 icFunction + "('" 
                               ELSE icFunction) 
                            + ENTRY(ix,icValueList,"|").
END.

cORlist = cORlist + ")".

RETURN cORlist.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ChangeQueryJoin C-Win 
FUNCTION ChangeQueryJoin RETURNS LOGICAL
  ( INPUT ibFullJoin AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cJoinedFields     AS CHAR NO-UNDO.
DEF VAR cNewBuffAndFlds   AS CHAR NO-UNDO.
DEF VAR hColumn           AS HANDLE NO-UNDO.

DEF VAR ix AS INT NO-UNDO.
DEF VAR iy AS INT NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","YES").
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).

DO ix = 3 TO NUM-ENTRIES(cBufferList):
  cJoinedFields = DYNAMIC-FUNCTION("getAttribute",hBrowse,"bufferFields" + ENTRY(ix,cBufferList)).
  DO iy = 1 TO NUM-ENTRIES(cJoinedFields):
    hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,ENTRY(iy,cJoinedFields)
                             /* Spesialregel pga like feltnavn */
                             +(IF ix = 4 AND ENTRY(iy,cJoinedFields) = "Beskrivelse" THEN "4" ELSE "")).
    IF VALID-HANDLE(hColumn) THEN
      hColumn:VISIBLE = ibFullJoin.
  END.
END.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"buffersAndFields",(IF ibFullJoin THEN cBuffersAndFlds ELSE cSmallBuffersAndFlds)).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryJoin",(IF ibFullJoin THEN cQueryJoin ELSE cSmallQueryJoin)).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"bufferList",(IF ibFullJoin THEN cBufferList ELSE cSmallBufferList)).

IF ibFullJoin THEN
  RUN InvokeMethod(hBrowse,"RefreshBrowseRecord").

DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButikkNr C-Win 
FUNCTION getButikkNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cmbButikk:SCREEN-VALUE IN FRAME {&FRAME-NAME}. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrisKoBufferNum C-Win 
FUNCTION getPrisKoBufferNum RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR cBuffAndFlds AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgBuffersAndFields") NE "" THEN DO:
  cBuffAndFlds = DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersAndFields").
  DO ix = 1 TO NUM-ENTRIES(cBuffAndFlds):
    IF ENTRY(1,ENTRY(ix,cBuffAndFlds),";") = "ArtPris" THEN LEAVE.
  END.
END.
ELSE ix = 1.

RETURN STRING(ix).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProfilnr C-Win 
FUNCTION getProfilnr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cmbPrisProfil:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX, 
                    DYNAMIC-FUNCTION("getWidgetsByLasso",brwArtpris:HANDLE IN FRAME {&FRAME-NAME},0,"frame,control-frame,rectangle,browse") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",KoFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor,button,toggle-box,combo-box,fill-in,radio-set")
                    ).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "brwArtpris," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rectFilter").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                   "btnAvdeling,btnHuvGr,btnLev2,btnVarGr,sokAvdelingNavn,sokAvdelingNr,sokHg,sokHgBeskr,sokLevNamn,sokVg,sokVgBeskr,btnProdusent,btnVaremerke,sokProdusent,sokVaremerke,rsArtPLU").

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,2000,1200,0,550).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: We don't want to sync all tabfolders when changing the tab-folder
           Therefore we delete and re-establish links accordingly 
------------------------------------------------------------------------------*/

IF iCurrTab NE 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

/*
IF iiTab = 1 THEN
  DYNAMIC-FUNCTION("CreateOneToOneLink",hCurrTabQuery,hBrowse,"CustNum").
ELSE 
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"CustNum").
*/

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

IF hCurrTabQuery:NAME BEGINS "brwStrekkode" THEN DO:
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"ArtikkelNr").
  RUN InvokeMethod(hBrowse,"DisplayRecord").
END.
ELSE IF hCurrTabQuery:NAME BEGINS "brwPrisko" THEN
  RUN ProfilnrChanged IN hCurrTabProc (cmbPrisProfil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
ELSE IF hCurrTabQuery:NAME BEGINS "brwEtikett" THEN
  RUN ButikkNrChanged IN hCurrTabProc (cmbButikk:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
ELSE  
  RUN InvokeMethod(hCurrTabQuery,"OpenQuery").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VisArtBilde C-Win 
FUNCTION VisArtBilde RETURNS LOGICAL
  ( INPUT iiBildeNr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RUN VisMiniBilde IN hArtBilde (iiBildeNr).
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

