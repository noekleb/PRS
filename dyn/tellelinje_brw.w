&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

update_varebok_from_artbas.p
vareboklinjeDvelgfelter.w           
vareboklinje_refresh_all.p           
           
           
           
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

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowse-2         AS HANDLE NO-UNDO.
DEF VAR hFilterWindow     AS HANDLE NO-UNDO.

DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.
DEF VAR hDetail           AS HANDLE NO-UNDO.
DEF VAR cRowidList        AS CHAR   NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.

DEF VAR hfiAntallTalt     AS HANDLE NO-UNDO.
DEF VAR hfcMerknad        AS HANDLE NO-UNDO.

DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hBuffer-2         AS HANDLE NO-UNDO.
DEF VAR hArtikkelkort     AS HANDLE NO-UNDO.
DEF VAR hEtikettVindu     AS HANDLE NO-UNDO.

/*Valg av poster*/
DEF VAR cDato   AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.

/*Browse stuff*/
DEF VAR hbcAntallTalt    AS HANDLE NO-UNDO.
DEF VAR hbfAntallTalt    AS HANDLE NO-UNDO.
DEF VAR hbcAntallDiff    AS HANDLE NO-UNDO.
DEF VAR hbfAntallDiff    AS HANDLE NO-UNDO.
DEF VAR hbfOpprAntalTalt AS HANDLE NO-UNDO.

DEF VAR bFlag  AS LOG NO-UNDO.
DEF VAR bFlag2 AS LOG NO-UNDO.

/* DEF VAR iFontWingdings    AS INT    NO-UNDO.                                                    */
/* iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR. */

{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar btnStartUtvalg ~
searchField rectBrowse-2 RECT-67 fraEdato tilEdato SaSong vg fraEtid ~
tilEtid Levkod Beskr LevFargKod btnToday strekkode LevNr Storl btnSaSong ~
artikkelnr btnBlank btnCalFraDato btnCalTilDato btnLevNr btnVg 
&Scoped-Define DISPLAYED-OBJECTS fraEdato tilEdato SaSong vg fraEtid ~
tilEtid Levkod Beskr LevFargKod strekkode LevNr Storl artikkelnr ~
FI-StrekkodeTekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillTime C-Win 
FUNCTION fillTime RETURNS CHARACTER
  (INPUT iphCombo         AS HANDLE,
   INPUT ipcTimeRangeFrom AS CHAR,
   INPUT ipcTimeRangeTo   AS CHAR,
   INPUT ipiTimeStep      AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SkrivEtikett C-Win 
FUNCTION SkrivEtikett RETURNS LOGICAL
  ( INPUT icListe AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlank 
     LABEL "Blank filter" 
     SIZE 15 BY 1.

DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSaSong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStartUtvalg  NO-FOCUS
     LABEL "Start søk" 
     SIZE 13 BY 1.

DEFINE BUTTON btnToday  NO-FOCUS
     LABEL "Dagens dato" 
     SIZE 14 BY 1 TOOLTIP "Dagens dato".

DEFINE BUTTON btnVg  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE artikkelnr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(200)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.

DEFINE VARIABLE FI-StrekkodeTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Søk på strekkode" 
      VIEW-AS TEXT 
     SIZE 40 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fraEdato AS DATE FORMAT "99/99/99":U 
     LABEL "Endret fra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fraEtid AS CHARACTER FORMAT "xx:xx":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(255)" 
     LABEL "Lev.fargekode" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.

DEFINE VARIABLE Levkod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.artnr." 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE SaSong AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Storl AS CHARACTER FORMAT "x(15)" 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE strekkode AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tilEdato AS DATE FORMAT "99/99/99":U 
     LABEL "Endret til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tilEtid AS CHARACTER FORMAT "xx:xx":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE vg AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 1.43.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 186 BY 10.48.

DEFINE RECTANGLE rectBrowse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 5.95.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnStartUtvalg AT ROW 7.24 COL 118 NO-TAB-STOP 
     fraEdato AT ROW 2.43 COL 13 COLON-ALIGNED
     tilEdato AT ROW 3.38 COL 13 COLON-ALIGNED
     SaSong AT ROW 4.38 COL 13 COLON-ALIGNED
     vg AT ROW 5.38 COL 13 COLON-ALIGNED
     fraEtid AT ROW 2.43 COL 31 COLON-ALIGNED NO-LABEL
     tilEtid AT ROW 3.38 COL 31 COLON-ALIGNED NO-LABEL
     Levkod AT ROW 2.43 COL 71 COLON-ALIGNED
     Beskr AT ROW 3.38 COL 71 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     LevFargKod AT ROW 4.33 COL 71 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     btnToday AT ROW 2.43 COL 46
     strekkode AT ROW 6.71 COL 71 COLON-ALIGNED NO-LABEL
     LevNr AT ROW 2.43 COL 118 COLON-ALIGNED
     Storl AT ROW 3.38 COL 118 COLON-ALIGNED HELP
          "Størrelse"
     btnSaSong AT ROW 4.38 COL 29 NO-TAB-STOP 
     artikkelnr AT ROW 4.38 COL 118 COLON-ALIGNED
     btnBlank AT ROW 7.24 COL 131
     btnCalFraDato AT ROW 2.43 COL 29
     btnCalTilDato AT ROW 3.43 COL 29
     btnLevNr AT ROW 2.43 COL 135.8 NO-TAB-STOP 
     btnVg AT ROW 5.38 COL 25 NO-TAB-STOP 
     FI-StrekkodeTekst AT ROW 5.86 COL 69 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 8.38 COL 1
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 7.43 COL 1
     rectBrowse-2 AT ROW 2.43 COL 147
     RECT-67 AT ROW 6.48 COL 70
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 186.2 BY 18.


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
         TITLE              = "Tellelinje"
         HEIGHT             = 18
         WIDTH              = 186.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME L-To-R,COLUMNS                                            */
/* SETTINGS FOR FILL-IN FI-StrekkodeTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Tellelinje */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tellelinje */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank C-Win
ON CHOOSE OF btnBlank IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
  RUN clearFilterRecord.
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraEdato
DO:
  RUN Cal.w (fraEdato:HANDLE).
/*   IF fraEdato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato C-Win
ON CHOOSE OF btnCalTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilEdato
DO:
  RUN Cal.w (tilEdato:HANDLE).
/*   IF tilEdato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevNr C-Win
ON CHOOSE OF btnLevNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF LevNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "LevNr;LevNamn".

  RUN JBoxDLookup.w ("LevBas;LevNr;LevNamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      LevNr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
/*     IF LevNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  */
  END.
  ELSE APPLY "entry" TO LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaSong C-Win
ON CHOOSE OF btnSaSong IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF SaSong
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Sasong;SasBeskr".

  RUN JBoxDLookup.w ("Sasong;Sasong;SasBeskr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      Sasong:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
/*     IF SaSong:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  */
  END.
  ELSE APPLY "entry" TO SaSong.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartUtvalg C-Win
ON CHOOSE OF btnStartUtvalg IN FRAME DEFAULT-FRAME /* Start søk */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToday
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToday C-Win
ON CHOOSE OF btnToday IN FRAME DEFAULT-FRAME /* Dagens dato */
DO:
  ASSIGN 
    fraEdato:SCREEN-VALUE = STRING(TODAY,"99/99/9999")
    tilEdato:SCREEN-VALUE = STRING(TODAY,"99/99/9999")
  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVg C-Win
ON CHOOSE OF btnVg IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF vg
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "vg;vgbeskr".

  RUN JBoxDLookup.w ("vargr;vg;vgbeskr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      vg:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
/*     IF LevNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  */
  END.
  ELSE APPLY "entry" TO vg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraEdato C-Win
ON LEAVE OF fraEdato IN FRAME DEFAULT-FRAME /* Endret fra */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraEtid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraEtid C-Win
ON ANY-PRINTABLE OF fraEtid IN FRAME DEFAULT-FRAME
DO:  
  DEFINE VARIABLE C AS CHARACTER NO-UNDO.
  DEFINE VARIABLE K AS CHARACTER NO-UNDO.
  K = CHR(LASTKEY).
  /* If not a digit, ignore */
  IF K < '0' OR K > '9' THEN RETURN NO-APPLY.
  C = REPLACE(SELF:SCREEN-VALUE, ':', '').
  C = REPLACE(C, ' ', '').
  /* Determine what digit you are placing */
  CASE LENGTH(C):
    WHEN 0 THEN IF K > '2' THEN RETURN NO-APPLY.
    WHEN 1 THEN IF K > '3' AND C = '2' THEN RETURN NO-APPLY.
    WHEN 2 THEN IF K > '5' THEN RETURN NO-APPLY.
    WHEN 4 THEN IF K > '5' THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Levkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Levkod C-Win
ON LEAVE OF Levkod IN FRAME DEFAULT-FRAME /* Lev.artnr. */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SaSong C-Win
ON LEAVE OF SaSong IN FRAME DEFAULT-FRAME /* Sesong */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Storl C-Win
ON LEAVE OF Storl IN FRAME DEFAULT-FRAME /* Størrelse */
DO:
  DYNAMIC-FUNCTION("runproc","bibl_fixstorl.p",storl:SCREEN-VALUE,?).
  SELF:SCREEN-VALUE = CAPS(DYNAMIC-FUNCTION("getTransactionMessage")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL strekkode C-Win
ON TAB OF strekkode IN FRAME DEFAULT-FRAME
OR 'RETURN' OF StrekKode
DO:
  IF TRIM(strekkode:SCREEN-VALUE) <> '' THEN
  DO:
      RUN StrekkodeOppslag.
      RUN ClearFilterRecord.
      APPLY 'entry' TO hBrowse.
      hBrowse:SELECT-FOCUSED-ROW().
      APPLY 'entry' TO hfiAntallTalt.
      RETURN NO-APPLY.
  END.
  ELSE
      APPLY LASTKEY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilEdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilEdato C-Win
ON LEAVE OF tilEdato IN FRAME DEFAULT-FRAME /* Endret til */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilEtid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilEtid C-Win
ON ANY-PRINTABLE OF tilEtid IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE C AS CHARACTER NO-UNDO.
  DEFINE VARIABLE K AS CHARACTER NO-UNDO.
  K = CHR(LASTKEY).
  /* If not a digit, ignore */
  IF K < '0' OR K > '9' THEN RETURN NO-APPLY.
  C = REPLACE(SELF:SCREEN-VALUE, ':', '').
  C = REPLACE(C, ' ', '').
  /* Determine what digit you are placing */
  CASE LENGTH(C):
    WHEN 0 THEN IF K > '2' THEN RETURN NO-APPLY.
    WHEN 1 THEN IF K > '3' AND C = '2' THEN RETURN NO-APPLY.
    WHEN 2 THEN IF K > '5' THEN RETURN NO-APPLY.
    WHEN 4 THEN IF K > '5' THEN RETURN NO-APPLY.
  END.
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
  PUBLISH "InvalidateHandle".
  IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
  DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.
  
  RUN enable_UI.
  DYNAMIC-FUNCTION("setParent",SOURCE-PROCEDURE).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AntTaltLikAntParRecord C-Win 
PROCEDURE AntTaltLikAntParRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Sett antall talt lik antall på lager ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_AntTaltLikAntPar.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_AntTaltLikAntPar.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AntTaltLikNullRecord C-Win 
PROCEDURE AntTaltLikNullRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Sett antall talt til null ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_AntTaltLikNull.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_AntTaltLikNull.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkelKortRecord C-Win 
PROCEDURE ArtikkelKortRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR fArtNr AS DEC NO-UNDO.

    ASSIGN 
      fArtNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.
      fArtNr = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtBas.ArtikkelNr = " 
                              + STRING(fArtNr),"ArtikkelNr").
    IF fArtNr LE 0 OR fArtNr = ? THEN
    DO:
      MESSAGE 'Kunne ikke finne ønsket post i artikkel register'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      UNDO, LEAVE.
    END.

  IF NOT VALID-HANDLE(hArtikkelkort) THEN
    RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtNr),"ROWID")), "ENDRE," + STRING(THIS-PROCEDURE)).
  ELSE
    RUN ByttArtikkel IN hArtikkelkort (fArtNr).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilterRecord C-Win 
PROCEDURE ClearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          FraEDato:SCREEN-VALUE    = ?
          TilEDato:SCREEN-VALUE    = ?
          Sasong:SCREEN-VALUE      = ''
          LevKod:SCREEN-VALUE      = ''
          Beskr:SCREEN-VALUE       = ''
          LevFargKod:SCREEN-VALUE  = ''
          Storl:SCREEN-VALUE       = ''
          LevNr:SCREEN-VALUE       = ''
          ArtikkelNr:SCREEN-VALUE  = ''
          StrekKode:SCREEN-VALUE   = ''
          vg:SCREEN-VALUE          = ''
          fraEtid:SCREEN-VALUE     = ''
          tilEtid:SCREEN-VALUE     = ''
          .
      DYNAMIC-FUNCTION("setSortString",hBrowse,"").
  END.
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
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN    
    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
      RUN ArtikkelKortRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Slette linje(r) ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_delete.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
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
  HIDE FRAME DEFAULT-FRAME.
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
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "New;&Ny,delete;Slette,Print;S&kriv,rule,excel;Eksporter til E&xcel" 
                     + ",Linjeregistrering;Linjeregistrering,ArtikkelKort;Artikkelkort"
                     + ",HTexport;Varefil til håndterminal,HTsetup;Håndterminal oppsett"
                     + ",HentIkkeTalt;Hent alle størrelser på artiklene,Strekkode;Strekkoder"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */                /*action;label;tooltip;Method;image*/
 "AntTaltLikNull;Sett antall talt = 0,AntTaltLikAntPar;Sett antall talt lik antal par,TaBortTommeLinjer;Ta bort linjer med 0 i lager og antall talt,TaBortPosLinjer;Ta bort linjer med pos. differanse,TaBortNegLinjer;Ta bort linjer med neg. differanse"

------------------------------------------------------------------------------*/
  DEF VAR cDisabledEvents    AS CHAR NO-UNDO.

  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN    
  DO WITH FRAME {&FRAME-NAME}:
    IF hBuffer:AVAILABLE AND hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):avail THEN
    DO:
      DYNAMIC-FUNCTION("CreateParentLink",hBrowse-2,hBrowse,"KobletTilTelleNr;tellenr").
      DYNAMIC-FUNCTION("setAttribute",hBrowse-2,'QueryJoin',",each buf1_tellelinje WHERE buf1_tellelinje.tellenr = buf1_tellehode.tellenr "  
                       + " AND buf1_tellelinje.artikkelnr = " + STRING(hBuffer:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE) 
                       + " AND buf1_tellelinje.storl = " + QUOTER(hBuffer:BUFFER-FIELD('storl'):BUFFER-VALUE) 
                       + " AND buf1_tellelinje.AntallTalt gt 0").

      /* --- Knappestyring ------ */
      IF (hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Oppdatert"):BUFFER-VALUE <> ?) THEN 
        cDisabledEvents = 'New,Delete,LinjeRegistrering,HentIkkeTalt,AntTaltLikNull,AntTaltLikAntPar,TaBortTommeLinjer,TaBortPosLinjer,TaBortNegLinjer,KobleTelleliste,LesInnFil'.
      ELSE
        cDisabledEvents = ''.
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"DisabledEvents",cDisabledEvents).
    END.
    IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleType'):BUFFER-VALUE = 1 
      AND INDEX('HentLokasjon',cDisabledEvents) LE 0 THEN
        cDisabledEvents = cDisabledEvents + ',HentLokasjon'.
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"DisabledEvents",cDisabledEvents).
  END.

  RUN SUPER.
  IF VALID-HANDLE(hArtikkelkort) AND hBuffer:avail THEN
    RUN ByttArtikkel IN hArtikkelkort (IF hBuffer:AVAIL THEN hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                       ELSE 0).
  
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
  DISPLAY fraEdato tilEdato SaSong vg fraEtid tilEtid Levkod Beskr LevFargKod 
          strekkode LevNr Storl artikkelnr FI-StrekkodeTekst 
      WITH FRAME DEFAULT-FRAME.
  ENABLE rectBrowse rectToolBar btnStartUtvalg searchField rectBrowse-2 RECT-67 
         fraEdato tilEdato SaSong vg fraEtid tilEtid Levkod Beskr LevFargKod 
         btnToday strekkode LevNr Storl btnSaSong artikkelnr btnBlank 
         btnCalFraDato btnCalTilDato btnLevNr btnVg 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTelleNr AS INTEGER NO-UNDO.
  
  ASSIGN 
    iTelleNr = INT(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR 
    THEN RETURN.
  ELSE RUN Varetelling_Til_Excel.p (iTelleNr).
    
  /* RUN SUPER.*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelSheetParams C-Win 
PROCEDURE ExcelSheetParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
PUBLISH "ExcelSheetParams" (ihObject,chExcelInstance,chWorkbook,chWorksheet,iCount - 1).
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipObject        AS HANDLE     NO-UNDO.
DEF INPUT PARAMETER ipExcelInstance AS CHAR       NO-UNDO.
DEF INPUT PARAMETER ipWorkbook      AS COM-HANDLE NO-UNDO.
DEF INPUT PARAMETER ipWorksheet     AS COM-HANDLE NO-UNDO.
DEF INPUT PARAMETER ipI             AS INT        NO-UNDO.

  ipWorkSheet:Range("D:F"):NumberFormat = "@".
  ipWorkSheet:Range("P:P"):NumberFormat = "@".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentIkkeTaltRecord C-Win 
PROCEDURE HentIkkeTaltRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   MESSAGE                                                                                                                                                     */
/*     "Henter alle størrelser for alle artikkler i listen." SKIP                                                                                                */
/*     "Fortsette ?"                                                                                                                                             */
/*     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk AS LOGICAL.                                                                                          */
/*   IF NOT bOk THEN RETURN NO-APPLY.                                                                                                                            */
/*                                                                                                                                                               */
/*   IF VALID-HANDLE(hParentBrowse) THEN                                                                                                                         */
/*   DO:                                                                                                                                                         */
/*     /*SESSION:SET-WAIT-STATE("general"). */                                                                                                                   */
/*                                                                                                                                                               */
/*     DYNAMIC-FUNCTION("runproc","tellelinje_adderstr.p",'RowId|' + string(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE),?). */
/*     RUN InvokeMethod(hBrowse,"OpenQuery").                                                                                                                    */
/*                                                                                                                                                               */
/*     /* SESSION:SET-WAIT-STATE(""). */                                                                                                                         */
/*   END.                                                                                                                                                        */
/*                                                                                                                                                               */


  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEF VAR iReturn     AS INT  NO-UNDO.
  DEF VAR ocValue     AS CHAR NO-UNDO.
  DEF VAR cStatusList AS CHAR NO-UNDO.
  DEF VAR cParam      AS CHAR NO-UNDO.

    RUN JBoxBrowseMsgUpdateVal.w ("Opprette linjer på manglende størrelser for valgte/alle artikler ?",
                                  hBrowse:NUM-SELECTED-ROWS,
                                  IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                    INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                  ELSE 99999,
                                  "",
                                  OUTPUT ocValue, 
                                  OUTPUT iReturn).

  IF iReturn = 1 THEN
    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"art_to_telling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE)).
  ELSE IF iReturn = 2 THEN
  DO:
    cRowIdList = ''.
    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
        cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
    END.
    IF cRowIdList <> '' THEN cRowIdList = 'ROWID,' + cRowIdList. 
    bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"art_to_telling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE) + ',' + cRowIdList).
  END.
  ELSE
    LEAVE.
  IF NOT bOK THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

  RUN InvokeMethod (hBrowse,'OpenQuery').
  /*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HTexportRecord C-Win 
PROCEDURE HTexportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTypeId AS INTEGER    NO-UNDO.
  DEFINE VARIABLE bOk     AS LOG        NO-UNDO.

  MESSAGE "Husk at tellelisten bare inneholder tellelinjer på de størrelser hvor lagerteller <> 0." SKIP
          "Den listen med varianter som nå sendes til håndterminalen, inneholder de størrelser som ligger i tellelisten." SKIP(1)
          "For å bygge en komplett artikkelfil til håndterminalen, skal dette gjøres via første flik 'Telleliste', eller via artikkelutvalget." SKIP(1)
          "Ønsker du alikevel å legge ut en artikkelfil basert på registrerte tellelinjer?"
      VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE bOk.
  IF bOk <> TRUE THEN
      RETURN.

  RUN d-velgHT.w (OUTPUT iTypeId).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  
  SESSION:SET-WAIT-STATE("general").
  RUN htvarefilFraTelling.p (hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE,iTypeId).
  SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  /* TN 7/1-10 Dette slå ar spørsmål på sesjonsninvå. Skal ikke gjøres. Spørsmål skal heller ikke slås av. */
  /*DYNAMIC-FUNCTION("setAttribute",SESSION,'DropExcelWarning','YES').*/

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "tellelinje"
                               + ";linjeNr|Linjenr|->>>>>>>9"
                               + ";VgLopNr|VgLopNr|x(12)"
                               + ";ArtikkelNr"
                               + ";LevKod|Lev.art.nr|x(30)"
                               + ";Beskr|Varetekst|x(40)"
                               + ";LevFargKod|Lev.farge|x(30)"
                               + ";Storl"
                               + ";AntallPar|Ant.lager|->>>>>>9" 
                               + ";OpprAntalTalt|Oppr.talt|->>>>>>9"
                               + ";AntallTalt|Ant.talt|->>>>>>9"
                               + ";AntallDiff|Ant.dif|->>>>>>9" 
                               + ";OpprVerdi"
                               + ";OpptVerdi"
                               + ";VerdiDiff"
                               + ";VVareKost"
                               + ";Oppdatert"                               
                               + ";Kode"
                               + ";Merknad|Merknad|x(200)"
                               + ";Vg"
                               + ";Sasong"
                               + ";LevNr"
                               + ";RegistrertDato"
                               + ";!RegistrertTid"
                               + ";+rTid|character|x(6)|rTid(ROWID)|Kl"
                               + ";RegistrertAv"
                               + ";EDato"
                               + ";!ETid"
                               + ";+endretTid|character|x(6)|endretTid(ROWID)|Kl"
                               + ";BrukerID"
                               + ";Butik"
                               + ";!TelleNr"
                               + ";!SeqNr"
                               + ";!RabKr"
                               + ";!Nedskrevet"
                               + ";!MatKod"
                               + ";!LopNr"
                               + ";!Farg"
                             ,"WHERE false"
                             ,"").
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=delval_tellelinje.p").

  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','tellelinje_brwcalc.p').
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "New;&Ny,delete;Slette,Print;S&kriv,rule,excel;Eksporter til E&xcel" 
                     + ",rule,RowsToBatch;Ant. rader i resultatsett¤enable"
                     /*+ ",Linjeregistrering;Linjeregistrering"*/
                     + ",HentIkkeTalt;Hent alle størrelser,Strekkode;Etikett"
                     + ",OppdaterLagerantall;Oppdater lagerantall"
                     /*+ ",KobleTelleliste;Koble til telleliste"*/
                     /*+ ",LesInnFil;Manuell innlesning av tellefil¤ENABLED"*/
                     + ",rule"
                     + ",HTexport;Varefil til håndterminal¤ENABLED"
                     + ",ArtikkelKort;Artikkelkort"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */                /*action;label;tooltip;Method;image*/

  DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowse,  /* parent widget */
                  "AntTaltLikNull;Sett antall talt = 0,AntTaltLikAntPar;Sett antall talt lik antal par,TaBortTommeLinjer;Ta bort linjer med 0 i lager og antall talt,TaBortPosLinjer;Ta bort linjer med pos. differanse,TaBortNegLinjer;Ta bort linjer med neg. differanse,OppdVVarekostPaLinjer;Oppdater varekost på linje(r)"
                 ,"").   

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
  
/*   fillTime(fraEtid:HANDLE,'00:00','23:59',15).       */
/*   tilETid:LIST-ITEM-PAIRS = fraETid:LIST-ITEM-PAIRS. */

  /* Oppdaterbar felt i browse */
  hfiAntallTalt = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "AntallTalt",     
                      "AntallTalt",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiAntallTalt,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiAntallTalt,"AntallTalt").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customUpdateValProc","=updval_tellelinje.p").
    
  /* Oppdaterbar felt i browse */
  hfcMerknad = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "Merknad",     
                    "Merknad",     
                    "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfcMerknad,"refreshrow","yes").          /* Refresh the row after update */
  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfcMerknad,"Merknad").

  hBrowse-2 = DYNAMIC-FUNCTION("NewBrowse"
              ,rectBrowse-2:HANDLE
              ,100
              ,""
              ,"buf1_TelleHode"
               + ";Tellenr|Lok.liste nr;!KobletTilTellenr"
               + ",buf1_tellelinje" 
                + ";AntallTalt;Merknad;ArtikkelNr;Storl"
              ,"WHERE false"
               + ", each buf1_tellelinje WHERE buf1_tellelinje.tellenr = buf1_TelleHode.tellenr " 
               + " AND buf1_tellelinje.antallTalt = 0 AND buf1_tellelinje.storl = ''" 
               + " AND buf1_tellelinje.AntallTalt GT 0"
              ,"").


END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION('setCurrentObject',hBrowse).  
DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
hChild = ?.
  APPLY 'value-changed' TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleTellelisteRecord C-Win 
PROCEDURE KobleTellelisteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn    AS CHAR  NO-UNDO.
DEF VAR rRowid             AS ROWID NO-UNDO.

bOk = FALSE.
/* THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE. */
DO WITH FRAME {&FRAME-NAME}:
/*   RUN JBoxSelector.w (THIS-PROCEDURE,0,                                                                                                */
/*       "Tellehode;TelleNr;Beskrivelse;TTId;Oppdatert;ButikkListe"                                                                       */
/*       ,"where TTId = 9 and Oppdatert = ? and and TelleType = 1 and TelleNr <> " + string(hBuffer:BUFFER-FIELD("TelleNr"):BUFFER-VALUE) */
/*       + " and ButikkListe = '" + hBuffer:BUFFER-FIELD("Butikkliste"):BUFFER-VALUE + "'",                                               */
/*       INPUT-OUTPUT cLokasjonRowIdList,                                                                                                 */
/*       "TelleNr",                                                                                                                       */
/*       INPUT-OUTPUT cLokasjonIdList,                                                                                                    */
/*       "","",                                                                                                                           */
/*       OUTPUT bOK).                                                                                                                     */
/*     THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.                                                                                    */

  cReturn = DYNAMIC-FUNCTION("getFieldValues","Tellehode"
                   ,"WHERE tellehode.ttid = 9 AND tellehode.oppdatert = ? AND tellehode.telletype = 1 AND tellenr <> "  
                             + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE)
                             + " AND butikkliste = " + QUOTER(STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ButikkListe"):BUFFER-VALUE))
                   ,"tellenr,beskrivelse").    

    IF cReturn = ? THEN
    DO:
      MESSAGE "Ingen åpen telleliste funnet"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.        
    END.
    ELSE
    MESSAGE "Skal det kobles til listen " + ENTRY(1,cReturn,'|') + ' ' + ENTRY(2,cReturn,'|') + "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.

    IF NOT bOk THEN
        RETURN.

    IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Butikkliste"):BUFFER-VALUE = '' THEN 
    DO:        
      MESSAGE 'Butikkliste ikke satt !!! '
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
    END.

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    
    RUN oppdatlokasjon.p (INPUT int(ENTRY(1,cReturn,'|')),
                          INPUT int(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Butikkliste"):BUFFER-VALUE),
                          INPUT STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("TelleNr"):BUFFER-VALUE)).

    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

/*     SESSION:SET-WAIT-STATE("GENERAL"). */

    rRowid = TO-ROWID(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    DYNAMIC-FUNCTION("runproc","tellehode_oppdatersum.p",STRING(rRowid),?).
    RUN InvokeMethod(hBrowse,"OpenQuery").
    DYNAMIC-FUNCTION("RefreshRowids",hParentBrowse,STRING(rRowId)).
    rRowid = TO-ROWID(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,STRING(rRowId)).

/*     SESSION:SET-WAIT-STATE(""). */

  END.

 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFilRecord C-Win 
PROCEDURE LesInnFilRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wAntTyper AS INT  NO-UNDO.
  DEF VAR wTypeId   AS INT  NO-UNDO.
  DEF VAR wKatalog  AS CHAR NO-UNDO.
  DEF VAR wFilNavn  AS CHAR NO-UNDO.
  
  {syspara.i 1 1 52 wKatalog}
  
  ASSIGN
  wFilNavn = wKatalog + '\' + 'inv.dat'.
    
  IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('butikkliste'):BUFFER-VALUE = '' THEN
    DO:
      MESSAGE "Det er ikke satt opp butikker på tellelisten!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN NO-APPLY.
    END.
 
  wTypeId = int(DYNAMIC-FUNCTION("getFieldValues","ht-type","WHERE true","typeid")).
  IF wTypeId = ? OR wTypeId = 0 THEN
  DO:
    MESSAGE "Det er ikke satt opp noen håndterminalsdefinisjoner!"
            VIEW-AS ALERT-BOX ERROR TITLE "Feil".
    RETURN NO-APPLY.
  END.
  
  /* Hvis det ligger en Inv.Dat, skal denne først konverteres til PPT880 formatet.            */
  /* vpiartbas_importer_pricat_fil.p aller opp xbxmobileinvinnles.p som gjør konverteringen. */
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_importer_pricat_fil.p"
                        ,'892;' + wFilNavn).
      
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN w-bht-filhode.w (INPUT hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE, INPUT wTypeId, 0).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  DYNAMIC-FUNCTION("runproc","tellehode_oppdatersum.p",STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE),?).
  
  RUN InvokeMethod(hBrowse,"OpenQuery").  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinjeregistreringRecord C-Win 
PROCEDURE LinjeregistreringRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN NewRecord.
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wRecid AS CHAR NO-UNDO.
  wRecid = ?.
  /* trim(entry(2,CB-Butikk:screen-value,":")) = wStdTxt */
  RUN w-vtellelinje.w PERSISTENT SET hDetail (INPUT-OUTPUT wRecid,"Endre", INT(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('butikkliste'):BUFFER-VALUE), TO-ROWID(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE), THIS-PROCEDURE:HANDLE,FALSE).
  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyLinje C-Win 
PROCEDURE NyLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  DUMMY PROCEDURE for å få gammelt program til å virke     
------------------------------------------------------------------------------*/
DEF INPUT PARAM ipRowid AS ROWID NO-UNDO.
/*   RUN InvokeMethod(hBrowse,"OpenQuery").                      */
/*   DYNAMIC-FUNCTION("RefreshRowids",hBrowse,ipRowid) NO-ERROR. */
  RUN RefreshBrowser (ipRowid).
  RETURN "Ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cWhere   AS CHAR NO-UNDO.
DEF VAR cVPIDato AS CHAR NO-UNDO.

DEF VAR iFraETid AS INT NO-UNDO.
DEF VAR iTilETid AS INT NO-UNDO.

  IF NOT bFLag THEN
  DO WITH FRAME {&FRAME-NAME}:
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').

    cWhere = buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,Beskr:HANDLE,'Beskr','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,LevFargKod:HANDLE,'LevFargKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,Storl:HANDLE,'Storl','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraEdato:HANDLE,'Edato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilEdato:HANDLE,'Edato','LE').
    cWhere = cWhere + buildFilter(cWhere,SaSong:HANDLE,'SaSong','EQ').
    cWhere = cWhere + buildFilter(cWhere,LevNr:HANDLE,'LevNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,ArtikkelNr:HANDLE,'ArtikkelNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,vg:HANDLE,'vg','EQ').
    
    IF NUM-ENTRIES(fraETid:SCREEN-VALUE,':') > 1 THEN
        iFraETid = int(ENTRY(1,fraEtid:SCREEN-VALUE,':')) * 3600 + int(ENTRY(2,fraEtid:SCREEN-VALUE,':')) * 60.
    IF NUM-ENTRIES(tilETid:SCREEN-VALUE,':') > 1 THEN
        iTilETid = int(ENTRY(1,tilEtid:SCREEN-VALUE,':')) * 3600 + int(ENTRY(2,tilEtid:SCREEN-VALUE,':')) * 60.

    IF iFraETid > 0 THEN
    DO:
        IF cWhere BEGINS ' WHERE' THEN
          cWhere = cWhere + ' AND '.
        ELSE
          cWhere = cWhere + ' WHERE '.
        cWhere = cWhere + ' eTid GE ' + STRING(iFraETid). 
    END.
    IF iTilETid > 0 THEN
    DO:
        IF cWhere BEGINS ' WHERE' THEN
          cWhere = cWhere + ' AND '.
        ELSE
          cWhere = cWhere + ' WHERE '.
        cWhere = cWhere + ' eTid LE ' + STRING(iTilETid). 
    END.

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).
    
    ASSIGN
      artikkelnr:MODIFIED    = FALSE
      fraEdato:MODIFIED      = FALSE
      tilEdato:MODIFIED      = FALSE
      fraEtid:MODIFIED       = FALSE
      tilEtid:MODIFIED       = FALSE
      Levkod:MODIFIED        = FALSE
      LevFargKod:MODIFIED    = FALSE
      Beskr:MODIFIED         = FALSE
      SaSong:MODIFIED        = FALSE
      LevNr:MODIFIED         = FALSE
      vg:MODIFIED            = FALSE 
    .
/*     hParentBrowse = DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"parent","from").                                           */
/*     IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND                                                                  */
/*        Strekkode:SCREEN-VALUE NE "" THEN                                                                                   */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",                                                        */
/*                        "VPIstrekkode WHERE VPIStrekkode.EkstVPILevNr = "                                                   */
/*                        + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE)        */
/*                        + " and VPIstrekkode.Kode = '" + Strekkode:SCREEN-VALUE + "'"                                       */
/*                        + ",FIRST VPIartBas OF VPIstrekkode NO-LOCK").                                                      */
/*     ELSE                                                                                                                   */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter","").                                                    */
/*                                                                                                                            */
/*                                                                                                                            */
  END.  
  bFlag = FALSE.

  RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterLagerantallRecord C-Win 
PROCEDURE OppdaterLagerantallRecord :
/*------------------------------------------------------------------------------
  Purpose: Oppdaterer lagerantall på alle valgte linjer eller alle linjer i utvalget.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iReturn     AS INT  NO-UNDO.
  DEF VAR ocValue     AS CHAR NO-UNDO.
  DEF VAR cStatusList AS CHAR NO-UNDO.
  DEF VAR cParam      AS CHAR NO-UNDO.

  bOK = FALSE.
  MESSAGE 
      "Oppdatering av lagerantall på linjene i tellelisten, er bare aktuelt å gjøre dersom man vet at tellelisten ble 'bygget' " + 
      "på feil tidspunkt, og at lagerantallet på linjene må oppdateres med aktuelt lagerantall." SKIP(1)
      "Er du sikker på at det er dette du vil gjøre?" skip(1)
      "På linjer som oppdateres, vil lagerantall bli satt til aktuelt lagerantall og eventuell differanse regnet ut på nytt. " +
      "Du vil få et nytt spørsmål om det er alle linjer i utvalget eller bare markerte linjer som skal oppdateres. " + 
      "Oppdatering av tellelisten vil ikke bli gjort før du har besvart neste spørsmål."
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
  IF bOk = FALSE THEN
      RETURN.

  RUN JBoxBrowseMsgUpdateVal.w ("Oppdater lagerantall på valgte/alle artikler ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"oppdater_lager_til_telling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE)).
ELSE IF iReturn = 2 THEN
DO:
  cRowIdList = ''.
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  IF cRowIdList <> '' THEN cRowIdList = 'ROWID,' + RIGHT-TRIM(cRowIdList,',').
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"oppdater_lager_til_telling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE) + '|' + cRowIdList).
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
  /*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdVVarekostPaLinjerRecord C-Win 
PROCEDURE OppdVVarekostPaLinjerRecord :
/*------------------------------------------------------------------------------
                        Purpose: Oppdaterer linje(r) med vektet varekost og regner om summer.                                                                                                                                     
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Oppdater varekost på linje(r)?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_OppdVVarekost.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_OppdVVarekost.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

PUBLISH 'VaretellingOppdaterSumRecord'.
RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
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
DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",cRettning) THEN DO:
  hBrowse:SELECT-FOCUSED-ROW().
  CASE cRettning:
      WHEN "Prev" THEN
          hBrowse:SELECT-PREV-ROW().
      WHEN "Next" THEN
        hBrowse:SELECT-NEXT-ROW().
  END CASE.
  APPLY "value-changed" TO hBrowse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
IF iReturn = 1 THEN  
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_report.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_report.p",'').
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
  
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
DEF VAR cList   AS CHAR NO-UNDO.

hBrowse:SELECT-FOCUSED-ROW() NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN.
RUN JBoxBrowseSelectMsg.w ("Sende valgte poster til rapport?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn).  /*1=Alle,2=Valgte*/

/* MESSAGE 'Programmerer må ferdigstille listen...' */
/*   VIEW-AS ALERT-BOX INFO BUTTONS OK.             */
/* LEAVE.                                           */
/* MESSAGE hParentBrowse:QUERY:PREPARE-STRING */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
IF iReturn = 1 THEN
  RUN printTellelinjeX.p ('Tellehode|' + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE) + '|Tellelinje|*',"").
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cList = cList + ',' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Rowident1'):BUFFER-VALUE).
  END.
  cList = TRIM(cList,',').
  RUN printTellelinjeX.p ('Tellehode|' + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE) +  '|Tellelinje|' + cList,"").    
END.
ELSE 
  LEAVE.


  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowser C-Win 
PROCEDURE RefreshBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  DUMMY PROCEDURE for å tilfredsstille gammelt program      
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipRowid AS ROWID NO-UNDO.
  DEF VAR iArtNr AS INT NO-UNDO.

  DEF VAR bOk AS LOG NO-UNDO.

  
  DO WITH FRAME Default-Frame:
      /*
      FIND TelleLinje NO-LOCK WHERE
          ROWID(TelleLinje) = to-rowid(STRING(ipRowid)) NO-ERROR.
      */
      /*
      iArtNr = int(DYNAMIC-FUNCTION("getFieldValues","Tellelinje","WHERE rowid(tellelinje) = to-rowid(" + quoter(STRING(ipRowid)) + ')',"ArtikkelNr")).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",' AND ArtikkelNr = ' + STRING(iArtNr)).
      */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",' AND ArtikkelNr = ' + STRING(TelleLinje.ArtikkelNr)). */

      bFlag = TRUE. /*Check openquery*/

      RUN InvokeMethod(hBrowse,"OpenQuery").

      IF hBuffer:AVAIL THEN
          DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  END.

  RETURN "Ok".

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
  DEF VAR iGreen  AS INT INIT 10 NO-UNDO.
  DEF VAR iRed    AS INT INIT 12 NO-UNDO.
  DEF VAR iYellow AS INT INIT 14 NO-UNDO.

  hbcAntallDiff:BGCOLOR =  IF hbfAntallDiff:BUFFER-VALUE < 0 THEN iGreen 
                           ELSE IF hbfAntallDiff:BUFFER-VALUE > 0 THEN iRed
                           ELSE ?.
  hbcAntallTalt:BGCOLOR =  IF hbfOpprAntalTalt:BUFFER-VALUE <> hbfAntallTalt:BUFFER-VALUE THEN iYellow ELSE ?. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowsToBatchRecord C-Win 
PROCEDURE RowsToBatchRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowsToBatch AS CHAR NO-UNDO.

RUN JBoxDSelectRowsToBatch.w (DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowsToBatch"), OUTPUT cRowsToBatch).

IF cRowsToBatch NE "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"rowsToBatch",cRowsToBatch).
  DYNAMIC-FUNCTION("setCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW,"<StartRowsToBatch>|brwLinje|" + cRowsToBatch + "|<EndRowsToBatch>").
  RUN INVOKEMETHOD(hBrowse,'OpenQuery').
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkodeOppslag C-Win 
PROCEDURE StrekkodeOppslag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cReturn  AS CHAR NO-UNDO.
  DEF VAR cReturn2 AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:    
    cReturn  = DYNAMIC-FUNCTION("getFieldValues","strekkode","WHERE kode = " + QUOTER(StrekKode:SCREEN-VALUE),"ArtikkelNr,StrKode").
    IF NUM-ENTRIES(cReturn,'|') GT 0 THEN
    DO:       
      artikkelnr:SCREEN-VALUE = ENTRY(1,cReturn,'|').
      RUN InvokeMethod(hBrowse,"OpenQuery").
      cReturn2 = DYNAMIC-FUNCTION("getFieldValues","strkonv","WHERE strkode = " + QUOTER(ENTRY(2,cReturn,'|')),'storl').
      IF NUM-ENTRIES(cReturn2,'|') GT 0 THEN
      DO:
        hBuffer:FIND-FIRST('WHERE artikkelnr = ' + ENTRY(1,cReturn,'|') + ' AND storl = ' + QUOTER(ENTRY(1,cReturn2,'|')),NO-LOCK).
        hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID) NO-ERROR.
        RUN InvokeMethod(hBrowse,"EndResizeBrowseColumn").
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkodeRecord C-Win 
PROCEDURE StrekkodeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iStartEtikett AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLayoutValg   AS INTEGER    NO-UNDO.

  /*
  RUN d-skrivervalg.w (OUTPUT iLayoutValg,OUTPUT iStartEtikett).
  IF iLayoutValg = 0 THEN
      RETURN.
  RUN batchEtikettTelling.p (hBrowse:QUERY:get-buffer-handle(1):BUFFER-FIELD('tellenr'):BUFFER-VALUE,iLayoutValg,"",FALSE,"TELLING")).
  */

  iReturn = 0.
  RUN JBoxBrowseSelectMsg.w ("Etikettutskrift valgte poster?",
      hBrowse:NUM-SELECTED-ROWS,
      IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
      ELSE 99999,
      OUTPUT iReturn).  /*1=Alle,2=Valgte*/

IF iReturn = 0 THEN RETURN NO-APPLY.

  IF iReturn = 1 OR hBrowse:NUM-SELECTED-ROWS = 0 THEN /* Alle poster */
    DO:
      IF DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"varetelling_etiketter.p","") THEN
         DYNAMIC-FUNCTION("SkrivEtikett", DYNAMIC-FUNCTION("getTransactionMessage")).
    END.
  ELSE DO: /* Valgte poster */
      IF DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"varetelling_etiketter.p","") THEN
          DYNAMIC-FUNCTION("SkrivEtikett", DYNAMIC-FUNCTION("getTransactionMessage")).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortNegLinjerRecord C-Win 
PROCEDURE TaBortNegLinjerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Ta bort linje(r) med negativ differanse ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_delnegdiff.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_delnegdiff.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortPosLinjerRecord C-Win 
PROCEDURE TaBortPosLinjerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Ta bort linje(r) med positiv differanse ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_delposdiff.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_delposdiff.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaBortTommeLinjerRecord C-Win 
PROCEDURE TaBortTommeLinjerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Ta bort linje(r) med null i lager og antall talt ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_delnullinjer.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_delnullinjer.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF icBrowseName = 'RectBrowse' THEN
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'AntallTalt' THEN 
        ASSIGN
          hbcAntallTalt = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfAntallTalt = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AntallTalt')                                                                            
        .
      WHEN 'AntallDiff' THEN 
        ASSIGN
          hbcAntallDiff = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfAntallDiff = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AntallDiff')
        .
      WHEN 'OpprAntalTalt' THEN 
        ASSIGN
          hbfOpprAntalTalt = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('OpprAntalTalt')                  
        .
    END CASE.
  END.

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillTime C-Win 
FUNCTION fillTime RETURNS CHARACTER
  (INPUT iphCombo         AS HANDLE,
   INPUT ipcTimeRangeFrom AS CHAR,
   INPUT ipcTimeRangeTo   AS CHAR,
   INPUT ipiTimeStep      AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iTimeFrom AS INT NO-UNDO.
DEF VAR iTimeTo   AS INT NO-UNDO.
DEF VAR iTime     AS INT NO-UNDO.
DEF VAR iTimeStep AS INT NO-UNDO.

/*Bygges om for å kunne legge inn faste klokkeslett som 00:00*/
ASSIGN 
  iTimeFrom = 0
  iTimeTo   = (23 * 3600) + (59 * 60)
  iTimeStep = ipiTimeStep * 60.
.
DO iTime = 1 TO iTimeTo:
  IF iTime GT iTimeFrom AND (iTime MOD iTimeStep = 0 OR iTime = 1) THEN 
    iphCombo:ADD-LAST(STRING(iTime,'hh:mm'),iTime).
   /*do: DISP string(iTime,'hh:mm') '....' iTimeFrom '...' iTimeTo. PAUSE 1. END.*/
END.
iphCombo:DELETE(1).
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hBrowse.

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

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse-2").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SkrivEtikett C-Win 
FUNCTION SkrivEtikett RETURNS LOGICAL
  ( INPUT icListe AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cArtikkelEti    AS CHAR NO-UNDO.
DEF VAR cEtiketter      AS CHAR NO-UNDO.
DEF VAR cAntallEti      AS CHAR NO-UNDO.
DEF VAR cIndividNr      AS CHAR NO-UNDO.
DEF VAR cPris           AS CHAR NO-UNDO.
DEF VAR iCount          AS INT  NO-UNDO.
DEF VAR cArtEANlist     AS CHAR NO-UNDO.
DEF VAR cArtANTlist     AS CHAR NO-UNDO.
DEF VAR cArtINDlist     AS CHAR NO-UNDO.
DEF VAR cArtPRISList    AS CHAR NO-UNDO.
DEF VAR iTotAnt         AS INT  NO-UNDO.

IF NUM-ENTRIES(icListe,"|") < 5 THEN RETURN FALSE.

ASSIGN icListe      = SUBSTR(icListe,8)
       cArtikkelEti = ENTRY(1,icListe,"|")
       cEtiketter   = ENTRY(2,icListe,"|")
       cAntallEti   = ENTRY(3,icListe,"|")
       cIndividNr   = ENTRY(4,icListe,"|")
       cPris        = ENTRY(5,icListe,"|")
       .

/* MESSAGE                                */
/*     "cArtikkelEti"  cArtikkelEti SKIP  */
/*     "cEtiketter  "  cEtiketter   SKIP  */
/*     "cAntallEti  "  cAntallEti   SKIP  */
/*     "cIndividNr  "  cIndividNr   SKIP  */
/*     "cPris"         cPris SKIP         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF cArtikkelEti <> "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cArtikkelEti,CHR(1)):
    ASSIGN cArtEANlist  = ENTRY(ix,cEtiketter,CHR(1))
           cArtANTlist  = ENTRY(ix,cAntallEti,CHR(1))
           cArtINDlist  = ENTRY(ix,cIndividNr,CHR(1))
           cArtPRISList = ENTRY(ix,cPris,CHR(1))
           .
    IF cArtEANlist <> "" THEN 
      DO iCount = 1 TO NUM-ENTRIES(cArtANTlist):
        IF ENTRY(iCount,cArtEANlist) <> "" AND INT(ENTRY(iCount,cArtANTlist)) > 0 THEN
          iTotAnt = iTotAnt + INT(ENTRY(iCount,cArtANTlist)).
      END.
  END.

  IF iTotAnt LE 0 THEN RETURN FALSE.

  DO ix = 1 TO NUM-ENTRIES(cArtikkelEti,CHR(1)):
    ASSIGN cArtEANlist  = ENTRY(ix,cEtiketter,CHR(1))
           cArtANTlist  = ENTRY(ix,cAntallEti,CHR(1))
           cArtINDlist  = ENTRY(ix,cIndividNr,CHR(1))
           cArtPRISList = ENTRY(ix,cPris,CHR(1))
           .
    IF cArtEANlist <> "" THEN DO:
      IF NOT VALID-HANDLE(hEtikettVindu) THEN
          RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (THIS-PROCEDURE:CURRENT-WINDOW).
      IF VALID-HANDLE(hEtikettVindu) THEN 
        DO iCount = 1 TO NUM-ENTRIES(cArtANTlist):
          IF ENTRY(iCount,cArtEANlist) <> "" AND INT(ENTRY(iCount,cArtANTlist)) > 0 THEN
            RUN NyEtikettPakkseddel IN hEtikettVindu (
                ENTRY(iCount,cArtEANlist),
                INT(ENTRY(iCount,cArtANTlist)),
                INT(ENTRY(iCount,cArtINDlist)),
                INT(ENTRY(iCount,cArtPRISList))
                ).
        END.
    END.
  END.
END.


RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

