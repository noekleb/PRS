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

DEF VAR bOk              AS LOG  NO-UNDO.
DEF VAR ix               AS INT  NO-UNDO.
DEF VAR cTekst           AS CHAR NO-UNDO.
                         
DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR hBrowse          AS HANDLE NO-UNDO.
 
DEF VAR hParent          AS HANDLE NO-UNDO.
DEF VAR hParentBrowse    AS HANDLE NO-UNDO.

DEF VAR hChild           AS HANDLE NO-UNDO.

DEF VAR VarebokNr        AS INT NO-UNDO.
DEF VAR cAlle            AS CHAR NO-UNDO.

DEF VAR hbcChkArtReg     AS HANDLE NO-UNDO.
DEF VAR hbfChkArtReg     AS HANDLE NO-UNDO.
DEF VAR hbcChkVPIinfo    AS HANDLE NO-UNDO.
DEF VAR hbfChkVPIinfo    AS HANDLE NO-UNDO.
DEF VAR hbcChkPris       AS HANDLE NO-UNDO.
DEF VAR hbfChkPris       AS HANDLE NO-UNDO.
DEF VAR hbcChkArtInfo    AS HANDLE NO-UNDO.
DEF VAR hbfChkArtInfo    AS HANDLE NO-UNDO.
DEF VAR hbcChkStrek      AS HANDLE NO-UNDO.
DEF VAR hbfChkStrek      AS HANDLE NO-UNDO.
DEF VAR hArtikkelKort    AS HANDLE NO-UNDO.
DEF VAR hSearchField     AS HANDLE NO-UNDO.
DEF VAR hbcChkKontroll   AS HANDLE NO-UNDO.
DEF VAR hbfChkKontroll   AS HANDLE NO-UNDO.
DEF VAR iBrukerType      AS INT    NO-UNDO.
DEF VAR cBrukerId        AS CHAR   NO-UNDO.
DEF VAR bSkjulFilter     AS LOG    NO-UNDO.
DEF VAR bVisEnkelToolbar AS LOG    NO-UNDO.
DEF VAR iInitBehStatus   AS INT    NO-UNDO.
DEF VAR bUndertrykkFilter AS LOG NO-UNDO.
DEF VAR bAvgrensRAvd     AS LOG NO-UNDO.
DEF VAR cSprak                   AS CHAR        NO-UNDO.
DEF VAR cSprakLst                AS CHAR  INITIAL 'SE,SVE' NO-UNDO.

DEF VAR hbcChkAnonseArtikkel AS HANDLE NO-UNDO.
DEF VAR hbfChkAnonseArtikkel AS HANDLE NO-UNDO.

DEF VAR hbcInnkjopsPris#1 AS HANDLE NO-UNDO.
DEF VAR hbfInnkjopsPris#1 AS HANDLE NO-UNDO.
DEF VAR hbfAktivInnkjopsPris AS HANDLE NO-UNDO.

DEF VAR hbcRab1%#1 AS HANDLE NO-UNDO.
DEF VAR hbfRab1%#1 AS HANDLE NO-UNDO.
DEF VAR hbfAktivRabatt% AS HANDLE NO-UNDO.

DEF VAR hbcDB%#1 AS HANDLE NO-UNDO.
DEF VAR hbfDB%#1 AS HANDLE NO-UNDO.
DEF VAR hbfAktivDB% AS HANDLE NO-UNDO.

DEF VAR hbcPris#1 AS HANDLE NO-UNDO.
DEF VAR hbfPris#1 AS HANDLE NO-UNDO.
DEF VAR hbfAktivPris AS HANDLE NO-UNDO.

DEF VAR hbcStrekkodeListe AS HANDLE NO-UNDO.
DEF VAR hbfStrekkodeListe AS HANDLE NO-UNDO.
DEF VAR hbfAktivStrekkodeListe AS HANDLE NO-UNDO.

DEF VAR hbcBestillingsnr AS HANDLE NO-UNDO.
DEF VAR hbfBestillingsnr AS HANDLE NO-UNDO.
DEF VAR hbfAktivBestillingsnr AS HANDLE NO-UNDO.

DEF VAR hbcLevKod AS HANDLE NO-UNDO.
DEF VAR hbfLevKod AS HANDLE NO-UNDO.
DEF VAR hbfAktivLevKod AS HANDLE NO-UNDO.

DEF VAR hbcBeskr AS HANDLE NO-UNDO.
DEF VAR hbfBeskr AS HANDLE NO-UNDO.
DEF VAR hbfAktivBeskr AS HANDLE NO-UNDO.

DEF VAR hbcLevFargKod AS HANDLE NO-UNDO.
DEF VAR hbfLevFargKod AS HANDLE NO-UNDO.
DEF VAR hbfAktivLevFargKod AS HANDLE NO-UNDO.

DEF VAR cRowIdList       AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS TG-Filter cbRAvdNr LevNr btnSaSong SaSong ~
tbAvvikVPIinfo tbAvvikArtBas btnCalFraDato tbAvvikStrekKode tbAvvikPris ~
tbAvvikArtInfo btnCalTilDato VareTekst Levkod fraVPIDato tilVPIDato ~
Utvidetsok Strekkode behStatus tbAnonseArtikkel cbcKontroll btnStartUtvalg ~
btnLevNr Vg btnLevNr-2 cbGrunnsortiment btnCalFraDato-2 btnCalTilDato-2 ~
fraOpprettet tilOpprettet rectBrowse rectToolBar searchField 
&Scoped-Define DISPLAYED-OBJECTS TG-Filter cbRAvdNr LevNr SaSong ~
tbAvvikVPIinfo tbAvvikArtBas tbAvvikStrekKode tbAvvikPris tbAvvikArtInfo ~
VareTekst Levkod fraVPIDato tilVPIDato Utvidetsok Strekkode behStatus ~
tbAnonseArtikkel cbcKontroll Vg cbGrunnsortiment fraOpprettet tilOpprettet 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtikkelNr C-Win 
FUNCTION getArtikkelNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVarebokNr C-Win 
FUNCTION getVarebokNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadRowsToBatchSettings C-Win 
FUNCTION LoadRowsToBatchSettings RETURNS LOGICAL
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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilDato-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLevNr-2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSaSong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStartUtvalg  NO-FOCUS
     LABEL "Start søk" 
     SIZE 13.4 BY 1.

DEFINE VARIABLE behStatus AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE cbcKontroll AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "(R7) Kontroll" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE cbGrunnsortiment AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "Grunnsortiment" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",no
     DROP-DOWN-LIST
     SIZE 24 BY 1 TOOLTIP "Filter på grunnsortiment" NO-UNDO.

DEFINE VARIABLE cbRAvdNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vareområde" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fraOpprettet AS DATE FORMAT "99/99/99":U 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fraVPIDato AS DATE FORMAT "99/99/99":U 
     LABEL "VPIdato fra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Levkod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.Artnr" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE SaSong AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Sessong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tilOpprettet AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tilVPIDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Utvidetsok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utvidetsøk" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE VareTekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE Vg AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 182 BY 10.71.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.

DEFINE VARIABLE tbAnonseArtikkel AS LOGICAL INITIAL no 
     LABEL "(R6) Annonse artikkel" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikArtBas AS LOGICAL INITIAL no 
     LABEL "(R1) Finnes ikke i artikkelregister" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "Artikkel finnes ikke i artikkelregister" NO-UNDO.

DEFINE VARIABLE tbAvvikArtInfo AS LOGICAL INITIAL no 
     LABEL "(R4) Avvik artikkel info" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikPris AS LOGICAL INITIAL no 
     LABEL "(R3) Avvik i priser" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikStrekKode AS LOGICAL INITIAL no 
     LABEL "(R5) Avvik strek kode" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikVPIinfo AS LOGICAL INITIAL no 
     LABEL "(R2) Avvik i VPI info" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Filter AS LOGICAL INITIAL no 
     LABEL "Filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TG-Filter AT ROW 2.43 COL 171 WIDGET-ID 2
     cbRAvdNr AT ROW 5.71 COL 144.2 COLON-ALIGNED
     LevNr AT ROW 3.48 COL 149.6 COLON-ALIGNED
     btnSaSong AT ROW 2.43 COL 165.8 NO-TAB-STOP 
     SaSong AT ROW 2.43 COL 149.6 COLON-ALIGNED
     tbAvvikVPIinfo AT ROW 4.57 COL 10
     tbAvvikArtBas AT ROW 3.48 COL 10
     btnCalFraDato AT ROW 4.52 COL 108
     tbAvvikStrekKode AT ROW 3.48 COL 46.4
     tbAvvikPris AT ROW 5.71 COL 10
     tbAvvikArtInfo AT ROW 2.43 COL 46.4
     btnCalTilDato AT ROW 4.57 COL 126
     VareTekst AT ROW 2.43 COL 92 COLON-ALIGNED
     Levkod AT ROW 3.48 COL 92 COLON-ALIGNED
     fraVPIDato AT ROW 4.57 COL 92 COLON-ALIGNED
     tilVPIDato AT ROW 4.57 COL 110 COLON-ALIGNED NO-LABEL
     Utvidetsok AT ROW 6.91 COL 92 COLON-ALIGNED
     Strekkode AT ROW 6.86 COL 44 COLON-ALIGNED
     behStatus AT ROW 2.43 COL 8 COLON-ALIGNED
     tbAnonseArtikkel AT ROW 4.57 COL 46.4
     cbcKontroll AT ROW 5.71 COL 44.4 COLON-ALIGNED
     btnStartUtvalg AT ROW 6.76 COL 170.8 NO-TAB-STOP 
     btnLevNr AT ROW 3.48 COL 165.8 NO-TAB-STOP 
     Vg AT ROW 4.57 COL 149.6 COLON-ALIGNED
     btnLevNr-2 AT ROW 4.57 COL 165.8 NO-TAB-STOP 
     cbGrunnsortiment AT ROW 6.81 COL 144.2 COLON-ALIGNED
     btnCalFraDato-2 AT ROW 5.62 COL 108
     btnCalTilDato-2 AT ROW 5.67 COL 126
     fraOpprettet AT ROW 5.67 COL 92 COLON-ALIGNED
     tilOpprettet AT ROW 5.67 COL 110 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 8.14 COL 2
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 6.91 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 183.4 BY 18.05.


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
         TITLE              = "VPI artikkler"
         HEIGHT             = 18.05
         WIDTH              = 183.4
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
   FRAME-NAME Custom                                                    */
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
ON END-ERROR OF C-Win /* VPI artikkler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* VPI artikkler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME behStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL behStatus C-Win
ON VALUE-CHANGED OF behStatus IN FRAME DEFAULT-FRAME /* Status */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraVPIDato
DO:
  RUN Cal.w (fraVPIdato:HANDLE).
  IF fraVPIdato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-2 C-Win
ON CHOOSE OF btnCalFraDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraOpprettet
DO:
  RUN Cal.w (fraOpprettet:HANDLE).
  IF fraOpprettet:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato C-Win
ON CHOOSE OF btnCalTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilVPIDato
DO:
  RUN Cal.w (tilVPIdato:HANDLE).
  IF tilVPIdato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato-2 C-Win
ON CHOOSE OF btnCalTilDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilOpprettet
DO:
  RUN Cal.w (tilOpprettet:HANDLE).
  IF tilOpprettet:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  
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
    IF LevNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevNr-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevNr-2 C-Win
ON CHOOSE OF btnLevNr-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Vg
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Vg;VgBeskr".

  RUN JBoxDLookup.w ("VarGr;Vg;VgBeskr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      Vg:SCREEN-VALUE = ENTRY(1,cLookupValue,"|")
    .
/*       SasBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
    IF Vg:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO Vg.
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
    IF SaSong:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO SaSong.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartUtvalg C-Win
ON CHOOSE OF btnStartUtvalg IN FRAME DEFAULT-FRAME /* Start søk */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbcKontroll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbcKontroll C-Win
ON VALUE-CHANGED OF cbcKontroll IN FRAME DEFAULT-FRAME /* (R7) Kontroll */
DO:
/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraOpprettet C-Win
ON LEAVE OF fraOpprettet IN FRAME DEFAULT-FRAME /* Opprettet */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraVPIDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraVPIDato C-Win
ON LEAVE OF fraVPIDato IN FRAME DEFAULT-FRAME /* VPIdato fra */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Levkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Levkod C-Win
ON LEAVE OF Levkod IN FRAME DEFAULT-FRAME /* Lev.Artnr */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SaSong C-Win
ON LEAVE OF SaSong IN FRAME DEFAULT-FRAME /* Sessong */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAnonseArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAnonseArtikkel C-Win
ON VALUE-CHANGED OF tbAnonseArtikkel IN FRAME DEFAULT-FRAME /* (R6) Annonse artikkel */
DO:
/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikArtBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikArtBas C-Win
ON VALUE-CHANGED OF tbAvvikArtBas IN FRAME DEFAULT-FRAME /* (R1) Finnes ikke i artikkelregister */
DO:
/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikArtInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikArtInfo C-Win
ON VALUE-CHANGED OF tbAvvikArtInfo IN FRAME DEFAULT-FRAME /* (R4) Avvik artikkel info */
DO:
/*     RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikPris C-Win
ON VALUE-CHANGED OF tbAvvikPris IN FRAME DEFAULT-FRAME /* (R3) Avvik i priser */
DO:
/*     RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikStrekKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikStrekKode C-Win
ON VALUE-CHANGED OF tbAvvikStrekKode IN FRAME DEFAULT-FRAME /* (R5) Avvik strek kode */
DO:
/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikVPIinfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikVPIinfo C-Win
ON VALUE-CHANGED OF tbAvvikVPIinfo IN FRAME DEFAULT-FRAME /* (R2) Avvik i VPI info */
DO:
/*     RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Filter C-Win
ON VALUE-CHANGED OF TG-Filter IN FRAME DEFAULT-FRAME /* Filter */
DO:
  RUN HideViewFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilOpprettet C-Win
ON LEAVE OF tilOpprettet IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilVPIDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilVPIDato C-Win
ON LEAVE OF tilVPIDato IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VareTekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VareTekst C-Win
ON LEAVE OF VareTekst IN FRAME DEFAULT-FRAME /* Varetekst */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
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
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
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

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

IF iBrukerType <= 1 THEN
DO:
    IF NOT VALID-HANDLE(hArtikkelkort) THEN
      RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtNr),"ROWID")), "ENDRE," + STRING(THIS-PROCEDURE)).
    ELSE
      RUN ByttArtikkel IN hArtikkelkort (fArtNr).
END.
ELSE DO:
    IF NOT VALID-HANDLE(hArtikkelkort) THEN
        RUN ArtBasVisTime.w   PERSIST SET hArtikkelkort (THIS-PROCEDURE,STRING(fArtNr)).
    ELSE
      RUN ByttArtikkel IN hArtikkelkort (fArtNr).    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearFilterRecord C-Win 
PROCEDURE clearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    tbAvvikArtBas:CHECKED    = FALSE 
    tbAvvikArtInfo:CHECKED   = FALSE
    tbAvvikPris:CHECKED      = FALSE
    tbAvvikStrekKode:CHECKED = FALSE
    tbAvvikVPIinfo:CHECKED   = FALSE
    fraVPIDato:SCREEN-VALUE  = '' 
    tilVPIDato:SCREEN-VALUE  = ''  
    Levkod:SCREEN-VALUE      = ''  
    SaSong:SCREEN-VALUE      = ''    
    Utvidetsok:SCREEN-VALUE  = ''  
    LevNr:SCREEN-VALUE       = ''    
    Vg:SCREEN-VALUE          = ''    
    VareTekst:SCREEN-VALUE   = '' 
    tbAnonseArtikkel:CHECKED = FALSE
    behStatus:SCREEN-VALUE   = ''
    
    fraVPIDato:MODIFIED      = FALSE 
    tilVPIDato:MODIFIED      = FALSE  
    Levkod:MODIFIED          = FALSE  
    SaSong:MODIFIED          = FALSE    
    Utvidetsok:MODIFIED      = FALSE 
    LevNr:MODIFIED           = FALSE    
    Vg:MODIFIED              = FALSE    
    VareTekst:MODIFIED       = FALSE 
    tbAnonseArtikkel:MODIFIED= FALSE
    behStatus:MODIFIED       = FALSE
  .
  
  RUN InvokeMethod (hBrowse,'OpenQuery').
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
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    RUN openVisAvvik.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord C-Win 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR icParam     AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Slette artikler ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
DO:
    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_delete.p",'').
END.
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
/*   MESSAGE 'Hvis innpris avviker fra aktiv innpris, skal da innpris legges over i aktiv kalkyle før postene slettes?' SKIP */
/*           'Ja-Innpris, men ikke utpris vil bli overført til artikkelregister' SKIP                                        */
/*           'Nei-Ingen priser vil bli overført'                                                                             */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.                                                               */
  ASSIGN icParam = ''.
/*       icParam = IF bOk THEN 'Yes' ELSE '' */
/*       bOk     = FALSE.                    */
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_delete.p",icParam).
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
------------------------------------------------------------------------------*/
RUN SUPER.
  IF VALID-HANDLE(hArtikkelkort) THEN
    RUN ByttArtikkel IN hArtikkelkort (IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                       ELSE 0).
  
  DO WITH FRAME {&FRAME-NAME}:
    RUN HideViewFilter.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    RUN openVisAvvik.  
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
  DISPLAY TG-Filter cbRAvdNr LevNr SaSong tbAvvikVPIinfo tbAvvikArtBas 
          tbAvvikStrekKode tbAvvikPris tbAvvikArtInfo VareTekst Levkod 
          fraVPIDato tilVPIDato Utvidetsok Strekkode behStatus tbAnonseArtikkel 
          cbcKontroll Vg cbGrunnsortiment fraOpprettet tilOpprettet 
      WITH FRAME DEFAULT-FRAME.
  ENABLE TG-Filter cbRAvdNr LevNr btnSaSong SaSong tbAvvikVPIinfo tbAvvikArtBas 
         btnCalFraDato tbAvvikStrekKode tbAvvikPris tbAvvikArtInfo 
         btnCalTilDato VareTekst Levkod fraVPIDato tilVPIDato Utvidetsok 
         Strekkode behStatus tbAnonseArtikkel cbcKontroll btnStartUtvalg 
         btnLevNr Vg btnLevNr-2 cbGrunnsortiment btnCalFraDato-2 
         btnCalTilDato-2 fraOpprettet tilOpprettet rectBrowse rectToolBar 
         searchField 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideViewFilter C-Win 
PROCEDURE HideViewFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
  ASSIGN
      tbAvvikArtBas:HIDDEN = TG-Filter:CHECKED
      tbAvvikVPIinfo:HIDDEN = TG-Filter:CHECKED
      tbAvvikPris:HIDDEN = TG-Filter:CHECKED
      tbAvvikArtInfo:HIDDEN = TG-Filter:CHECKED
      tbAvvikStrekKode:HIDDEN = TG-Filter:CHECKED
      tbAnonseArtikkel:HIDDEN = TG-Filter:CHECKED
      cbcKontroll:HIDDEN = TG-Filter:CHECKED
      Strekkode:HIDDEN = TG-Filter:CHECKED
      VareTekst:HIDDEN = TG-Filter:CHECKED
      Levkod:HIDDEN = TG-Filter:CHECKED
      fraVPIDato:HIDDEN = TG-Filter:CHECKED
      btnCalFraDato:HIDDEN = TG-Filter:CHECKED
      tilVPIDato:HIDDEN = TG-Filter:CHECKED
      btnCalTilDato:HIDDEN = TG-Filter:CHECKED
      fraOpprettet:HIDDEN = TG-Filter:CHECKED
      btnCalFraDato-2:HIDDEN = TG-Filter:CHECKED
      tilOpprettet:HIDDEN = TG-Filter:CHECKED
      btnCalTilDato-2:HIDDEN = TG-Filter:CHECKED
      SaSong:HIDDEN = TG-Filter:CHECKED
      btnSaSong:HIDDEN = TG-Filter:CHECKED
      LevNr:HIDDEN = TG-Filter:CHECKED
      btnLevNr:HIDDEN = TG-Filter:CHECKED
      Vg:HIDDEN = TG-Filter:CHECKED
      btnLevNr-2:HIDDEN = TG-Filter:CHECKED
      cbRAvdNr:HIDDEN = TG-Filter:CHECKED
      cbGrunnsortiment:HIDDEN = TG-Filter:CHECKED
      .
END.
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

  cBrukerId   = DYNAMIC-FUNCTION("getASuserId").
  iBrukerType = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + cBrukerId + "'","BrukerType").
  cAlle       = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 1  and SysGr = 100 and ParaNr = 1","Parameter1").
  cTekst      = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 1","Parameter1").
  IF CAN-DO('1,Ja,True,Yes,J,Y',cTekst) THEN 
      bSkjulFilter = TRUE.
  cTekst      = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 3","Parameter1").
  IF CAN-DO('1,Ja,True,Yes,J,Y',cTekst) THEN 
      bVisEnkelToolbar = TRUE.
  IF cAlle = "" THEN ASSIGN cAlle = "[Alle]".

  iInitBehStatus = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 5","Parameter1")).
  cSprak      = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","Lng").

  cTekst      = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 6","Parameter1").
  IF CAN-DO('1,Ja,True,Yes,J,Y',cTekst) THEN 
      bUndertrykkFilter = TRUE.

  cTekst      = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 7","Parameter1").
  IF CAN-DO('1,Ja,True,Yes,J,Y',cTekst) THEN 
      bAvgrensRAvd = TRUE.

  IF iBrukerType <= 1 THEN
      ASSIGN
      bSkjulFilter      = FALSE
      bVisEnkelToolbar  = FALSE
      iInitBehStatus    = ?
      bUndertrykkFilter = FALSE
      .

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "VPIArtBas"
                             + ";ArtikkelNr|Art.nr|>>>>>>>>>>>>>>9@2"
                             + ";LevKod;Beskr|Beskrivelse|x(50)"
                             + ";LevFargKod"
                             + ";RAvdNr|Vareomr."
                             + ";+chkArtReg|logical|*/|chkArtReg(ROWID)|R1"
                             + ";+chkVPIinfo|logical|*/|chkVPIinfo(ROWID)|R2"
                             + ";+chkPris|logical|*/|chkPris(ROWID)|R3"
                             + ";+chkArtInfo|logical|*/|chkArtInfo(ROWID)|R4"
                             + ";+chkStrek|logical|*/|chkStrek(ROWID)|R5"
                             + ";+chkAnonseArtikkel|logical|*/|chkAnonseArtikkel(ROWID)|R6"
                             + ";+chkKontroll|logical|*/|chkKontroll(ROWID)|R7"
                             + ";+InnkjopsPris#1|decimal|>>>><>>9.99|InnkjopsPris#1|Innkj.pris"
                             + ";+Rab1%#1|decimal|->>>><>>9.99|Rab1%#1|Rabatt1%"
                             + ";+DB%#1|decimal|->>>><>>9.99|DB%#1|DB%"
                             + ";+Pris#1|decimal|>>>><>>9.99|Pris#1|Pris"
                             + ";+KatalogPris#1|decimal|>>>><>>9.99|KatalogPris#1|Katalogpris"
                             + ";AnbefaltPris|Markedspris"
                             + ";+forhRab%#1|decimal|->>>><>>9.99|forhRab%#1|Forh.rab"
                             + ";+suppRab%#1|decimal|->>>><>>9.99|suppRab%#1|Sup.Rabatt"
                             + ";+StrekkodeListe|char|x(18)|StrekkodeListe(ROWID)|Strekkode@22"
                             + ";VPIDato@23"
                             + ";LinkVareNr|LinkVare@25"
                             + ";behStatus@28"
                             + ";KjedeValutaPris|Innkj.pris valuta"
                             + ";KjedeProdusent|Kjedens produsent"
                             
                             + ";!+AktivInnkjopsPris|decimal|>>>><>>9.99|AktivInnkjopsPris(ROWID)|Aktiv innkj.pris"
                             + ";!+AktivRabatt%|decimal|>>>><>>9.99|AktivRabatt%(ROWID)|Aktiv rabatt%"
                             + ";!+AktivDb%|decimal|>>>><>>9.99|AktivDb%(ROWID)|Aktiv db%"
                             + ";!+AktivPris|decimal|>>>><>>9.99|AktivPris(ROWID)|Aktiv pris"
                             
                             + ";!+AktivStrekkodeListe|char|x(18)|AktivStrekkodeListe(ROWID)|Aktiv strekkode"
                             + ";+Bestillingsnr|char|x(18)|Bestillingsnr(ROWID)|Bestillingsnr"
                             + ";!+AktivBestillingsnr|char|x(18)|AktivBestillingsnr(ROWID)|Aktiv bestillingsnr"
                             + ";!+AktivLevKod|char|x(18)|AktivLevKod(ROWID)|Aktiv lev.art.nr"
                             + ";!+AktivBeskr|char|x(50)|AktivBeskr(ROWID)|Aktiv beskrivelse"
                             + ";!+AktivLevFargKod|char|x(18)|AktivLevFargKod(ROWID)|Aktiv lev.fragekode"

                             + ";!Gjennomfaktureres;!AntIPkn;!StrKode2;!StrKode1;!GarantiKl;!BestForslag;!Pant;!IndividType;!ArtSlag;!ManRabIKas;!Mengde;!VPIBildeKode;!LevDato4;!LevDato3;!Linjemerknad;!KjedeVare;!HovedModellFarge;!PrisGrpNr;!SentralBestilling;!ModellFarge;!LokArtikkelNr;!Oppdatert;!SalgsEnhet;!Etikett;!HandKode;!Pakkenr;!BehKode;!KjentPaHK;!HKVareId;!IKasse;!LokPris;!HkStyrt;!Alder;!Pakke;!BildeIKasse;!OLLager;!OPris;!SattPaKampanje;!VisDivInfo;!DivInfo;!LevDato2;!LevDato1;!Storrelser;!RegistrertAv;!RegistrertTid;!RegistrertDato;!BrukerID;!ETid;!EDato;!ProdNr;!StrTypeID;!VgKat;!AnonseArtikkel;!BongTekst;!Notat;!VMId;!Lager;!ValKod;!ProvKod;!RabKod;!Anv-Id;!Slit-Id;!Inner-Id;!Foder-Id;!Last-Id;!Ov-Id;!Kommentar;!Tilv-Land;!BildNr;!MatKod;!Klack;!Farg;!LopNr;!Vg;!Hg"
                             + ";!EkstVPILevNr;!LevVareTekst;!KundeRabatt;!VareNr;!UtvidetSok"
                             + ";+ArtBasRegistrertDato|date|99/99/99|ArtBasRegistrertDato(ROWID)|Opprettet"
                             + ",VPIArtPris;Profilnr;AktivFraDato|Aktiveres@24"
                             + ";!+InnkjopsPris#1|decimal|>>>><>>9.99|Innkjopspris%#1|Innkj.pris"
                             + ";!+Varekost#1|decimal|>>>><>>9.99|Varekost%#1|Varekost"
                             + ",SaSong;SaSong;SasBeskr"
                             + ",LevBas;LevNr@26;LevNamn@27"
                             + ",sysPara;Parameter1|Status@1"
                             ,"WHERE false "
                             + ", FIRST VPIArtPris OUTER-JOIN WHERE VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr " 
                                                            + " AND VPIArtPris.VareNr       = VPIArtBas.VareNr NO-LOCK "
                             + ", FIRST SaSong OUTER-JOIN OF VPIArtBas NO-LOCK "
                             + ", FIRST LevBas OUTER-JOIN OF VPIArtBas NO-LOCK "
                             + ", FIRST sysPara OUTER-JOIN WHERE sysPara.sysHid = 21 AND sysPara.sysGr = 200 AND sysPara.ParaNr = VPIArtBas.behStatus NO-LOCK"
                             ,"").
 
  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"chkArtReg,chkVPIinfo,chkPris,chkArtInfo,chkStrek,chkAnonseArtikkel,chkKontroll").
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'QuerySort','ArtikkelNr').
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanLimit","100000").
  
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpiartbas_artbas_brwcalc.p').
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  ASSIGN 
    behStatus:DELIMITER              = "|"
    behStatus:LIST-ITEM-PAIRS        = "||" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 21 and sysGr = 200")
    cbGrunnsortiment:DELIMITER       = "|"
    cbGrunnsortiment:LIST-ITEM-PAIRS = "||Nei|No|Ja|Yes".

  /* Setter tilgang til vareområde. */
  IF iBrukerType <= 1 OR bAvgrensRAvd = FALSE THEN
    ASSIGN
    cbRAvdNr:DELIMITER = "|"
    cbRAvdNr:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("getFieldList","Regnskapsavdeling;RAvdNr|RAvdBeskrivelse;RAvdNr","where true")
    . 
  ELSE DO:
      ASSIGN
      cbRAvdNr:DELIMITER = "|"
      cTekst             = DYNAMIC-FUNCTION("getFieldList","Regnskapsavdeling;RAvdNr|RAvdBeskrivelse;RAvdNr","where OmradeAnsvarlig =  '" + cBrukerId + "'")
      .
      IF cTekst <> '' THEN cbRAvdNr:LIST-ITEM-PAIRS = cTekst.
      ELSE cbRAvdNr:LIST-ITEM-PAIRS = " |0". 
      cbRAvdNr:SCREEN-VALUE = ENTRY(2,cbRAvdNr:LIST-ITEM-PAIRS,'|').
  END.
  
  ASSIGN 
    cbcKontroll:DELIMITER = "|"
    cbcKontroll:LIST-ITEM-PAIRS = "||Feilkoblet EAN/str kode|Rule6_EAN|Feilkoblet EAN kode|Rule5_EAN|Ingen EAN kode|Rule1_EAN|Likt navn/ulikt modellnummer|Rule2_LevKod|Likt modellnummer/ulik tekst|Rule3_Beskr|Ingen bilde|Rule4_bilderef" 
  . 

  IF bVisEnkelToolbar THEN 
    hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                      rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                      "Fil",                         /* Corresponding menu label - no menu if blank */
                      "excel;Eksporter til E&xcel,Edit;&Endre,Delete;Slette,TilEtikett;Legg til etikettkø,sendPriskoRegister;&Send til priskø,clearFilter;Blank &filter¤ENABLE"
                      ,"maxborder").                  /* Misc - enable, maxborder.. */
  ELSE 
      hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                        rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                        "Fil",                         /* Corresponding menu label - no menu if blank */
                        "excel;Eksporter til E&xcel,BrowseConfig,Edit;&Endre,Delete;Slette,Print;S&kriv,rule,sendArtikkelRegister;&Send til artikkelregister,sendPriskoRegister;&Send til priskø,sendButikk;Send til b&utikk,ArtikkelKort;&Artikkel kort,clearFilter;Blank &filter¤ENABLE"
                        + ",RowsToBatch;Antall &rader i resultatsett¤enable"
                        ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  LoadRowsToBatchSettings().

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrowse,  /* parent widget */
                    "setStatus;Endre status,setRegnskapsAvdeling;Endre vareområdeset,setPantLink;&Link til pant"
                   ,""). 
  
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  

  ASSIGN TG-Filter:CHECKED IN FRAME DEFAULT-FRAME = bSkjulFilter.
  IF iInitBehStatus <> ? THEN
      ASSIGN behStatus:SCREEN-VALUE = STRING(iInitBehStatus) NO-ERROR.
  ELSE 
      ASSIGN behStatus:SCREEN-VALUE = ? NO-ERROR.

  RUN HideViewFilter.

/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cWhere            AS CHAR NO-UNDO.
DEF VAR cVPIDato          AS CHAR NO-UNDO.
DEF VAR cOpprettetDato    AS CHAR NO-UNDO.
DEF VAR cPrescanStrekkode AS CHAR NO-UNDO.
DEF VAR cPrescanProfil    AS CHAR NO-UNDO.

  
  
  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN
        cVPIDato = '¤' + 
                   (IF INPUT FraVPIDato <> ? THEN STRING(INPUT FraVPIDato) ELSE '') + '¤' +
                   (IF INPUT TilVPIDato <> ? THEN STRING(INPUT TilVPIDato) ELSE '')
        cOpprettetDato =  
                   (IF INPUT FraOpprettet <> ? THEN STRING(INPUT FraOpprettet) ELSE '') + '¤' +
                   (IF INPUT TilOpprettet <> ? THEN STRING(INPUT TilOpprettet) ELSE '')
        .
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtReg",tbAvvikArtBas:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkVPIinfo",tbAvvikVPIinfo:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkPris",tbAvvikPris:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtInfo",tbAvvikArtInfo:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkStrek",tbAvvikStrekKode:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkAnonseArtikkel",tbAnonseArtikkel:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkKontroll",IF cbcKontroll:SCREEN-VALUE = ? 
                                                                     THEN '' 
                                                                     ELSE (cbcKontroll:SCREEN-VALUE + cVPIDato)  ).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamArtBasRegistrertDato",cOpprettetDato).

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
    cWhere = buildFilter(cWhere,varetekst:HANDLE,'beskr','BEGINS').      
    cWhere = cWhere + buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraVPIdato:HANDLE,'VPIdato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilVPIdato:HANDLE,'VPIdato','LE').
    cWhere = cWhere + buildFilter(cWhere,SaSong:HANDLE,'SaSong','EQ').
    cWhere = cWhere + buildFilter(cWhere,cbRAvdNr:HANDLE,'RAvdNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,LevNr:HANDLE,'LevNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,Vg:HANDLE,'Vg','EQ').
    IF behStatus:SCREEN-VALUE <> ? THEN
    DO:
        IF INT(INPUT behStatus) = 0 THEN
        DO:
           IF cWhere MATCHES 'WHERE' THEN
             cWhere = ' AND ' + 'behStatus' + ' ' + '<' + ' ' + '90'.
           ELSE
             cWhere = ' WHERE ' + 'behStatus' + ' ' + '<' + ' ' + '90'.
        END.
        ELSE
            cWhere = cWhere + buildFilter(cWhere,behStatus:HANDLE,'behStatus','EQ').
    END.
    cWhere = cWhere + buildFilter(cWhere,Utvidetsok:HANDLE,'Utvidetsok','CONTAINS').

    IF cbGrunnsortiment:SCREEN-VALUE <> "" AND cbGrunnsortiment:SCREEN-VALUE <> ? THEN 
        cWhere = cWhere + buildFilter(cWhere,cbGrunnsortiment:HANDLE,'Grunnsortiment','EQ').

/*     cWhere = cWhere + buildFilter(cWhere,tbAnonseArtikkel:HANDLE,'AnonseArtikkel','EQ'). */

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

    ASSIGN
      fraVPIDato:MODIFIED      = FALSE 
      tilVPIDato:MODIFIED      = FALSE  
      Levkod:MODIFIED          = FALSE  
      SaSong:MODIFIED          = FALSE   
      LevNr:MODIFIED           = FALSE
      VareTekst:MODIFIED       = FALSE 
      Utvidetsok:MODIFIED      = FALSE 
      cbRAvdNr:MODIFIED        = FALSE
    .

    hParentBrowse = DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"parent","from").
    IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND
       Strekkode:SCREEN-VALUE NE "" THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
      cPrescanStrekkode = "VPIstrekkode WHERE VPIStrekkode.EkstVPILevNr = " 
                        + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) 
                        + " and VPIstrekkode.Kode = '" + Strekkode:SCREEN-VALUE + "'"
                        + ",FIRST VPIartBas OF VPIstrekkode NO-LOCK").

/*     IF INTEGER(profilnr:SCREEN-VALUE) > 1 AND profilnr:SCREEN-VALUE NE ? THEN                               */
/*       cPrescanProfil = "VPIArtPris WHERE VPIArtPris.profilnr = " + profilnr:SCREEN-VALUE                    */
/*                      + ",FIRST VPIartBas WHERE VPIartBas.ArtikkelNr = VPIArtPris.ArtikkelNr NO-LOCK"        */
/*                      + REPLACE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery"),"WHERE "," AND ") + " " */
/*                      + REPLACE(cWhere,"WHERE "," AND ").                                                    */

      
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                     TRIM(cPrescanStrekkode + "|" + cPrescanProfil,"|")
                     ).

  END.

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openVisAvvik C-Win 
PROCEDURE openVisAvvik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*   DEF VAR fArtNr AS INT NO-UNDO.                                                                                                */
  /*                                                                                                                                 */
  /*   fArtNr = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtBas.ArtikkelNr = "                                              */
  /*                             + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE),"ArtikkelNr"). */
  /*   IF fArtNr GT 0 THEN RUN Artikkelkort.                                                                                         */
  IF NOT VALID-HANDLE(hChild) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN vpiartbas_artikkeldetalj.w PERSIST SET hChild.
  END.
  APPLY "value-changed" TO hBrowse.

  IF VALID-HANDLE(hChild) THEN
  DO:
      DYNAMIC-FUNCTION('setColour' IN hChild).
      RUN MoveToTop IN hChild.
      SUBSCRIBE TO 'InvalidateHandle' IN hChild.
  END.
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
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.

RUN JBoxBrowseSelectMsg.w ("Sende valgte poster til rapport?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn).  /*1=Alle,2=Valgte*/
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

  hbcInnkjopsPris#1:BGCOLOR =  IF hbfInnkjopsPris#1:BUFFER-VALUE < hbfAktivInnkjopsPris:BUFFER-VALUE THEN iGreen 
                               ELSE IF hbfInnkjopsPris#1:BUFFER-VALUE > hbfAktivInnkjopsPris:BUFFER-VALUE THEN iRed
                               ELSE ?.
  
  hbcRab1%#1:BGCOLOR        =  IF hbfRab1%#1:BUFFER-VALUE < hbfAktivRabatt%:BUFFER-VALUE THEN iGreen 
                               ELSE IF hbfRab1%#1:BUFFER-VALUE > hbfAktivRabatt%:BUFFER-VALUE THEN iRed
                               ELSE ?.

  hbcDB%#1:BGCOLOR          =  IF hbfDB%#1:BUFFER-VALUE < hbfAktivDB%:BUFFER-VALUE THEN iGreen 
                               ELSE IF hbfDB%#1:BUFFER-VALUE > hbfAktivDB%:BUFFER-VALUE THEN iRed
                               ELSE ?.

  hbcPris#1:BGCOLOR         =  IF hbfPris#1:BUFFER-VALUE < hbfAktivPris:BUFFER-VALUE THEN iGreen 
                               ELSE IF hbfPris#1:BUFFER-VALUE > hbfAktivPris:BUFFER-VALUE THEN iRed
                               ELSE ?.

  hbcStrekkodeListe:BGCOLOR =  IF hbfStrekkodeListe:BUFFER-VALUE <> hbfAktivStrekkodeListe:BUFFER-VALUE THEN iYellow ELSE ?. 
  hbcBestillingsnr:BGCOLOR =  IF hbfBestillingsnr:BUFFER-VALUE <> hbfAktivBestillingsnr:BUFFER-VALUE THEN iYellow ELSE ?. 
  hbcLevKod:BGCOLOR =  IF hbfLevKod:BUFFER-VALUE <> hbfAktivLevKod:BUFFER-VALUE THEN iYellow ELSE ?. 
  hbcBeskr:BGCOLOR =  IF hbfBeskr:BUFFER-VALUE <> hbfAktivBeskr:BUFFER-VALUE THEN iYellow ELSE ?. 
  hbcLevFargKod:BGCOLOR =  IF hbfLevFargKod:BUFFER-VALUE <> hbfAktivLevFargKod:BUFFER-VALUE THEN iYellow ELSE ?. 
                                                                                                              
/*   IF hbfChkArtReg:BUFFER-VALUE = FALSE THEN */
/*     ASSIGN                                  */
/*       hbcChkArtReg:BGCOLOR = iGreen         */
/*     .                                       */

/*   IF VALID-HANDLE(hbcChkArtReg) THEN                     */
/*   ASSIGN                                                 */
/*     hbcChkArtReg:FONT      = iFontWingdings              */
/*     hbcChkArtReg:FORMAT    = CHR(254) + "/"  + CHR(168)  */
/*   .                                                      */
/*   IF VALID-HANDLE(hbcChkVarebok) THEN                    */
/*   ASSIGN                                                 */
/*     hbcChkVarebok:FONT      = iFontWingdings             */
/*     hbcChkVarebok:FORMAT    = CHR(254) + "/"  + CHR(168) */
/*   .                                                      */
/*   IF VALID-HANDLE(hbcChkPris) THEN                       */
/*   ASSIGN                                                 */
/*     hbcChkPris:FONT      = iFontWingdings                */
/*     hbcChkPris:FORMAT    = CHR(254) + "/"  + CHR(168)    */
/*   .                                                      */
/*   IF VALID-HANDLE(hbcChkArtInfo) THEN                    */
/*   ASSIGN                                                 */
/*     hbcChkArtInfo:FONT      = iFontWingdings             */
/*     hbcChkArtInfo:FORMAT    = CHR(254) + "/"  + CHR(168) */
/*   .                                                      */
/*   IF VALID-HANDLE(hbcChkStrek) THEN                      */
/*   ASSIGN                                                 */
/*     hbcChkStrek:FONT      = iFontWingdings               */
/*     hbcChkStrek:FORMAT    = CHR(254) + "/"  + CHR(168)   */
/*   .                                                      */

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
END.
RUN InvokeMethod (hBrowse,'OpenQuery').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendArtikkelRegisterRecord C-Win 
PROCEDURE sendArtikkelRegisterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn    AS INT NO-UNDO.
DEF VAR cVareNr AS CHAR NO-UNDO.

RUN velgFelterArtBas_artbas_artpris.w (OUTPUT opcFieldList, OUTPUT opcLabelList, OUTPUT opiReturn).
IF opiReturn = 0 THEN
    RETURN NO-APPLY.

IF opcFieldList = '' THEN
DO:
  MESSAGE 'Ingen felter valgt, avbryter overføring...' 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  UNDO, LEAVE.
END.

RUN JBoxBrowseSelectMsg.w ("Send valgte poster?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 0 THEN RETURN NO-APPLY.

IF iReturn = 1 THEN
  DO:
    bOK = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_behstatus_kontroll.p",'').
    IF NOT bOk THEN
    DO:
        MESSAGE "Det finnes artikler som har status >= 10 i utvalget. Disse må behandles og få endret sin status før de kan overføres." SKIP
                "Bruk filer for å få frem bare ubehandlede poster slik at disse kan overføres."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"artbas_new_update.p"
                          ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) 
                           + ";" + opcFieldList
                           ).
  END.
  ELSE DO:
      bOK = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_behstatus_kontroll.p",'').
      IF NOT bOk THEN
      DO:
          MESSAGE "Det finnes artikler som har status >= 10 i utvalget. Disse må behandles og få endret sin status før de kan overføres." SKIP
                  "Bruk filer for å få frem bare ubehandlede poster slik at disse kan overføres."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"artbas_new_update.p"
                             ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) 
                              + ";" + opcFieldList
                             ).
  END.

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i endring av linformasjon ",""). 
ELSE DO: 
    IF iReturn = 1 THEN
    DO:
        MESSAGE 'Skal alle oversendte VPI poster slettes fra VPI registeret?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
        IF bOK THEN DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_delete.p",'').

        /* Er detaljvisning oppe, skal denne avsluttes. */
        IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
        IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
        
        RUN InvokeMethod (hBrowse,'OpenQuery'). 
    END.
    ELSE DO:
        MESSAGE 'Skal de markerte oversendte VPI poster slettes fra VPI registeret?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
        IF bOK THEN 
        DO:
            DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_delete.p",'').
            RUN InvokeMethod (hBrowse,'OpenQuery'). 
        END.
        ELSE
            DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
    END.
    /* Er detaljvisning oppe, skal informasjonen i denne friskes opp */
    IF VALID-HANDLE(hChild) THEN
        RUN openVisAvvik.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendButikkRecord C-Win 
PROCEDURE sendButikkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn    AS INT    NO-UNDO.
DEF VAR cVareNr    AS CHAR   NO-UNDO.
DEF VAR cDummy     AS CHAR   NO-UNDO.
DEF VAR cButList   AS CHAR   NO-UNDO.

                                                                                                                                                       
RUN d-setvpiogsend.w ("Send valgte poster til butikkene?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn,    /*1=Alle,2=Valgte*/
                              OUTPUT cButList). 

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_send_til_butikk.p"
                        ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + cButList).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_send_til_butikk.p"
                         ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + cButList).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendPriskoRegisterRecord C-Win 
PROCEDURE sendPriskoRegisterRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.
DEFINE VARIABLE pcValue AS CHARACTER NO-UNDO.

DEF VAR iReturn     AS INT NO-UNDO.

RUN d-setpriskodato.w ("Send valgte poster til priskø?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn,    /*1=Alle,2=Valgte*/
                              OUTPUT pcValue). 
IF iReturn = 0 THEN 
  RETURN.
                                
/* Er visning av filer undertrykket, settes det slik at all informasjon overføres. */
IF bUndertrykkFilter = FALSE THEN 
DO:
    /* Er det også valgt åoppdatere artikkelinformasjonen, skal maske angis. */
    IF CAN-DO('1,J,yes,true,y',ENTRY(5,pcValue,'|')) THEN
    DO:
      RUN velgFelterArtBas_artbas_artpris.w (OUTPUT opcFieldList, OUTPUT opcLabelList, OUTPUT opiReturn).
      IF opiReturn = 0 THEN
          RETURN NO-APPLY.
      IF opcFieldList = '' THEN
      DO:
        MESSAGE 'Ingen felter valgt, avbryter overføring og priskøoppdatering...' 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
      END.
    END.
END.
ELSE opcFieldList = {tbchooseAll.i}.

IF iReturn = 1 THEN DO:
    bOK = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_behstatus_kontroll.p",'').
    IF NOT bOk THEN
      DO:
        MESSAGE "Det finnes artikler som har status >= 10 i utvalget. Disse må behandles og få endret sin status før de kan overføres." SKIP
                "Bruk filer for å få frem bare ubehandlede poster slik at disse kan overføres."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
      END.

    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_sendtilpris.p",pcValue + ';' + opcFieldList).
END.
ELSE
DO:
  bOK = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_behstatus_kontroll.p",'').
  IF NOT bOk THEN
    DO:
      MESSAGE "Det finnes artikler som har status >= 10 i utvalget. Disse må behandles og få endret sin status før de kan overføres." SKIP
              "Bruk filer for å få frem bare ubehandlede poster slik at disse kan overføres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_sendtilpris.p",pcValue + ';' + opcFieldList).
END.


IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i endring av linformasjon ",""). 
ELSE DO: 
    IF iReturn = 1 THEN
    DO:
        IF iBrukerType > 1 THEN
            DO:
                MESSAGE 'Skal det legges opp etiketter på oversendte VPI poster?'
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
                IF bOK THEN RUN TilEtikettRecord.
            END.

        MESSAGE 'Skal alle oversendte VPI poster slettes fra VPI registeret?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
        IF bOK THEN DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_delete.p",'').

        /* Er detaljvisning oppe, skal denne avsluttes. */
        IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
        IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
        
        RUN InvokeMethod (hBrowse,'OpenQuery'). 
    END.
    ELSE DO:
        IF iBrukerType > 1 THEN
            DO:
                MESSAGE 'Skal det legges opp etiketter på oversendte VPI poster?'
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
                IF bOK THEN RUN TilEtikettRecord.
            END.
        
        MESSAGE 'Skal de markerte oversendte VPI poster slettes fra VPI registeret?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
        IF bOK THEN 
        DO:
            DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_delete.p",'').
            RUN InvokeMethod (hBrowse,'OpenQuery'). 
        END.
        ELSE DO:
            DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
        END.
    END.
    /* Er detaljvisning oppe, skal informasjonen i denne friskes opp */
    IF VALID-HANDLE(hChild) THEN
        RUN openVisAvvik.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPantLinkRecord C-Win 
PROCEDURE setPantLinkRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cList AS CHAR NO-UNDO.

RUN d-settPantLink.w ("Sett link til pant på vare(r)?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn,    /*1=Alle,2=Valgte*/
                              OUTPUT ocValue).                     
IF iReturn = 0 THEN 
  RETURN.

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_setpantlink.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_setpantlink.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + ocValue).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRegnskapsAvdelingRecord C-Win 
PROCEDURE setRegnskapsAvdelingRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cList AS CHAR NO-UNDO.

IF iBrukerType <= 1 THEN
    ASSIGN 
      cList = DYNAMIC-FUNCTION("getFieldList","Regnskapsavdeling;RAvdNr|RAvdBeskrivelse;RAvdNr","WHERE TRUE")
      cList = REPLACE(cList,'|',',').
ELSE
    ASSIGN 
      cList = DYNAMIC-FUNCTION("getFieldList","Regnskapsavdeling;RAvdNr|RAvdBeskrivelse;RAvdNr","where OmradeAnsvarlig =  '" + cBrukerId + "'")
      cList = REPLACE(cList,'|',',').

RUN JBoxBrowseMsgUpdateVal.w ("Velg ønsket vareområde for endring av poster",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999
                              ,'integer|>>9|Vareområde|COMBO-BOX|,0,' + cList
                              ,OUTPUT ocValue
                              ,OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_setregnskapsavdeling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_setregnskapsavdeling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + ocValue).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setStatusRecord C-Win 
PROCEDURE setStatusRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

ASSIGN 
  cStatusList = DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 21 and sysGr = 200")
  cStatusList = REPLACE(cStatusList,'|',',')
.

RUN JBoxBrowseMsgUpdateVal.w ("Velg ønsket status for endring av poster",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999
                              ,'integer|>>9|Status|COMBO-BOX|,0,' + cStatusList
                              ,OUTPUT ocValue
                              ,OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_setstatus.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + 'behStatus' + ';' + ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_setstatus.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + 'behStatus' + ';' + ocValue).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).


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
DEF VAR iReturn AS INT NO-UNDO.


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

IF iReturn = 2 AND hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
  DYNAMIC-FUNCTION("ProcessSelectedRows",
                   hBrowse,
                   "etikettvpiartpris_send_til.p",
                   hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ProfilNr"):BUFFER-VALUE
                   ).
  MESSAGE 'Etiketter er lagt til etikettkø i prisregister. De skrives ut på vanlig måte via prisregisteret.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

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
  /*
  ihBrowse:MOVE-COLUMN(27,1).
  ihBrowse:MOVE-COLUMN(26,22).
  ihBrowse:MOVE-COLUMN(27,23).
  
  ihBrowse:MOVE-COLUMN(28,1).
  ihBrowse:MOVE-COLUMN(27,22).
  ihBrowse:MOVE-COLUMN(28,23).
  */
  
  ASSIGN
    /*
    ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 60
    ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 90
    ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 50
    ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 150
    ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 100
    ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 50
    ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS = 20
    ihBrowse:GET-BROWSE-COLUMN(11):WIDTH-PIXELS = 20
    ihBrowse:GET-BROWSE-COLUMN(12):WIDTH-PIXELS = 20
    ihBrowse:GET-BROWSE-COLUMN(13):WIDTH-PIXELS = 20
    */
    
    hbcChkArtReg         = ihBrowse:GET-BROWSE-COLUMN(7)
    hbfChkArtReg         = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtReg')
    hbcChkVPIinfo        = ihBrowse:GET-BROWSE-COLUMN(8)
    hbfChkVPIinfo        = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkVPIinfo')
    hbcChkPris           = ihBrowse:GET-BROWSE-COLUMN(9)
    hbfChkPris           = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkPris')
    hbcChkArtInfo        = ihBrowse:GET-BROWSE-COLUMN(10)
    hbfChkArtInfo        = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtInfo')
    hbcChkStrek          = ihBrowse:GET-BROWSE-COLUMN(11)
    hbfChkStrek          = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrek')
    hbcChkAnonseArtikkel = ihBrowse:GET-BROWSE-COLUMN(12)
    hbfChkAnonseArtikkel = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAnonseArtikkel')
    hbcChkKontroll    = ihBrowse:GET-BROWSE-COLUMN(13)
    hbfChkKontroll    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkKontroll')
    
    hbcChkArtReg:COLUMN-FONT            = 8
    hbcChkVPIinfo:COLUMN-FONT           = 8
    hbcChkPris:COLUMN-FONT              = 8
    hbcChkArtInfo:COLUMN-FONT           = 8
    hbcChkStrek:COLUMN-FONT             = 8
    hbcChkAnonseArtikkel:COLUMN-FONT    = 8
    hbcChkKontroll:COLUMN-FONT          = 8
  
    hbcInnkjopsPris#1    = ihBrowse:GET-BROWSE-COLUMN(14) 
    hbfInnkjopsPris#1    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('InnkjopsPris#1')
    hbfAktivInnkjopsPris = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivInnkjopsPris')

    hbcRab1%#1      = ihBrowse:GET-BROWSE-COLUMN(15) 
    hbfRab1%#1      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Rab1%#1')
    hbfAktivRabatt% = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivRabatt%')
                   
    hbcDB%#1        = ihBrowse:GET-BROWSE-COLUMN(16) 
    hbfDB%#1        = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DB%#1')
    hbfAktivDB%     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivDb%')

    hbcPris#1       = ihBrowse:GET-BROWSE-COLUMN(17) 
    hbfPris#1       = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Pris#1')
    hbfAktivPris    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivPris')

    hbcStrekkodeListe       = ihBrowse:GET-BROWSE-COLUMN(22) 
    hbfStrekkodeListe       = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('StrekkodeListe')
    hbfAktivStrekkodeListe  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivStrekkodeListe')

    hbcBestillingsnr      = ihBrowse:GET-BROWSE-COLUMN(31) 
    hbfBestillingsnr      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Bestillingsnr')
    hbfAktivBestillingsnr = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivBestillingsnr')

    hbcLevKod      = ihBrowse:GET-BROWSE-COLUMN(3) 
    hbfLevKod      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('LevKod')
    hbfAktivLevKod = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivLevKod')

    hbcBeskr      = ihBrowse:GET-BROWSE-COLUMN(4) 
    hbfBeskr      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Beskr')
    hbfAktivBeskr = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivBeskr')

    hbcLevFargKod      = ihBrowse:GET-BROWSE-COLUMN(5) 
    hbfLevFargKod      = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('LevFargKod')
    hbfAktivLevFargKod = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('AktivLevFargKod')
    .

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtikkelNr C-Win 
FUNCTION getArtikkelNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    RETURN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE.   /* Function return value. */
  ELSE 
    RETURN 0.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVarebokNr C-Win 
FUNCTION getVarebokNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN VarebokNr.   /* Function return value. */

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

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadRowsToBatchSettings C-Win 
FUNCTION LoadRowsToBatchSettings RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR iy           AS INT NO-UNDO.
  DEF VAR cWinSettings AS CHAR NO-UNDO.
  
  cWinSettings = DYNAMIC-FUNCTION("getCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW).
  
  DO ix = 1 TO NUM-ENTRIES(cWinSettings,"|"):
    IF ENTRY(ix,cWinSettings,"|") = "<StartRowsToBatch>" THEN
      DO iy = ix + 1 TO NUM-ENTRIES(cWinSettings,"|") BY 2:
        IF ENTRY(iy,cWinSettings,"|") = "<EndRowsToBatch>" THEN LEAVE.
        ELSE DYNAMIC-FUNCTION("setAttribute",hBrowse,"rowsToBatch",ENTRY(iy + 1,cWinSettings,"|")).
      END.
  END.
                         
  RETURN TRUE.   /* Function return value. */
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

