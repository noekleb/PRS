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
DEF VAR hFieldMap         AS HANDLE NO-UNDO.

DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.

DEF VAR VarebokNr         AS INT NO-UNDO.

DEF VAR hbcChkPris        AS HANDLE NO-UNDO.
DEF VAR hbfChkPris        AS HANDLE NO-UNDO.
DEF VAR hbcChkArtInfo     AS HANDLE NO-UNDO.
DEF VAR hbfChkArtInfo     AS HANDLE NO-UNDO.
DEF VAR hArtikkelKort     AS HANDLE NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.
DEF VAR hArtBasSok        AS HANDLE NO-UNDO.
DEF VAR hbcPris           AS HANDLE NO-UNDO.
DEF VAR hbfPris           AS HANDLE NO-UNDO.
DEF VAR hbcInnkjopspris   AS HANDLE NO-UNDO. 
DEF VAR hbfInnkjopspris   AS HANDLE NO-UNDO. 
DEF VAR hbcRab1%          AS HANDLE NO-UNDO.
DEF VAR hbfRab1%          AS HANDLE NO-UNDO.
DEF VAR hbcDB%            AS HANDLE NO-UNDO.
DEF VAR hbfDB%            AS HANDLE NO-UNDO.
DEF VAR hbcMva%           AS HANDLE NO-UNDO.
DEF VAR hbfMva%           AS HANDLE NO-UNDO.
DEF VAR hbcVarekost       AS HANDLE NO-UNDO.
DEF VAR hbfVarekost       AS HANDLE NO-UNDO.

DEF VAR hbcPris_ArtPris         AS HANDLE NO-UNDO.
DEF VAR hbfPris_ArtPris         AS HANDLE NO-UNDO.
DEF VAR hbcInnkjopspris_ArtPris AS HANDLE NO-UNDO. 
DEF VAR hbfInnkjopspris_ArtPris AS HANDLE NO-UNDO. 
DEF VAR hbcRab1%_ArtPris        AS HANDLE NO-UNDO.
DEF VAR hbfRab1%_ArtPris        AS HANDLE NO-UNDO.
DEF VAR hbcDB%_ArtPris          AS HANDLE NO-UNDO.
DEF VAR hbfDB%_ArtPris          AS HANDLE NO-UNDO.
DEF VAR hbcVarekost_ArtPris     AS HANDLE NO-UNDO.
DEF VAR hbfVarekost_ArtPris     AS HANDLE NO-UNDO.
DEF VAR hbcMva%_ArtPris         AS HANDLE NO-UNDO.
DEF VAR hbfMva%_ArtPris         AS HANDLE NO-UNDO.

DEF VAR hbcOrgPris        AS HANDLE NO-UNDO.
DEF VAR hbfOrgPris        AS HANDLE NO-UNDO.    

DEF VAR hBrw_pris         AS HANDLE NO-UNDO.

DEF VAR cRowIdList  AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS behStatus behType btnCalFraDato tbAvvikPris ~
tbAvvikArtInfo btnCalTilDato VareTekst Levkod fraAktiveresDato ~
tilAktiveresDato SaSong btnSaSong btnStartUtvalg LevNr btnLevNr Vg btnVg ~
rectBrowse rectToolBar searchField rectView rectWinToolbar rectView-2 ~
ProfilNr 
&Scoped-Define DISPLAYED-OBJECTS lblHeader behStatus behType tbAvvikPris ~
tbAvvikArtInfo VareTekst Levkod fraAktiveresDato tilAktiveresDato SaSong ~
OrgPris OrgGyldigTilDato OrgDB% OrgAktiveresDato lblHeader-2 ~
Innkjopspris_ArtPris DB%_ArtPris Rab1%_ArtPris varekost_ArtPris ~
mva%_ArtPris Pris_ArtPris LevNr Vg ProfilNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getColumnIndex C-Win 
FUNCTION getColumnIndex RETURNS INTEGER
  (INPUT ihBrowse AS HANDLE,
   INPUT icColumnName AS CHAR)  FORWARD.

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

DEFINE BUTTON btnVg  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE behStatus AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE behType AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Pris profilnr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE DB%_ArtPris AS DECIMAL FORMAT "->>,>>9.9<":U INITIAL 0 
     LABEL "DB%" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fraAktiveresDato AS DATE FORMAT "99/99/99":U 
     LABEL "Aktiveres fra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Innkjopspris_ArtPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE lblHeader AS CHARACTER FORMAT "X(256)":U INITIAL "Original data" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE lblHeader-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Aktiv kalkyle" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE Levkod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.Artnr" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE mva%_ArtPris AS DECIMAL FORMAT "->>,>>9.9<":U INITIAL 0 
     LABEL "Mva %" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE OrgAktiveresDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Aktiveres dato" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE OrgDB% AS DECIMAL FORMAT "->>,>>9.9<":U INITIAL 0 
     LABEL "DB%" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE OrgGyldigTilDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Gyldig til dato" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE OrgPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Pris_ArtPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Rab1%_ArtPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1%" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE SaSong AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Sessong" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE tilAktiveresDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE varekost_ArtPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE VareTekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE Vg AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157.8 BY 25.24.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 5.95.

DEFINE RECTANGLE rectView-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 18.57.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY .95.

DEFINE VARIABLE tbAvvikArtInfo AS LOGICAL INITIAL no 
     LABEL "(R2) Avvik artikkel info" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikPris AS LOGICAL INITIAL no 
     LABEL "(R1) Avvik i priser" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     lblHeader AT ROW 6.95 COL 161 COLON-ALIGNED NO-LABEL
     behStatus AT ROW 2.43 COL 17 COLON-ALIGNED
     behType AT ROW 3.57 COL 17 COLON-ALIGNED
     btnCalFraDato AT ROW 2.43 COL 185
     tbAvvikPris AT ROW 4.57 COL 120
     tbAvvikArtInfo AT ROW 5.29 COL 120
     btnCalTilDato AT ROW 2.43 COL 203
     VareTekst AT ROW 2.43 COL 118 COLON-ALIGNED
     Levkod AT ROW 3.48 COL 118 COLON-ALIGNED
     fraAktiveresDato AT ROW 2.43 COL 169 COLON-ALIGNED
     tilAktiveresDato AT ROW 2.43 COL 187 COLON-ALIGNED NO-LABEL
     SaSong AT ROW 4.71 COL 88 COLON-ALIGNED
     OrgPris AT ROW 7.91 COL 178 COLON-ALIGNED
     OrgGyldigTilDato AT ROW 9 COL 178.2 COLON-ALIGNED
     OrgDB% AT ROW 10.1 COL 178 COLON-ALIGNED
     btnSaSong AT ROW 4.81 COL 102 NO-TAB-STOP 
     OrgAktiveresDato AT ROW 11.19 COL 178 COLON-ALIGNED
     btnStartUtvalg AT ROW 5.05 COL 194 NO-TAB-STOP 
     lblHeader-2 AT ROW 13.62 COL 161 COLON-ALIGNED NO-LABEL
     Innkjopspris_ArtPris AT ROW 14.33 COL 178 COLON-ALIGNED
     DB%_ArtPris AT ROW 17.52 COL 178 COLON-ALIGNED
     Rab1%_ArtPris AT ROW 15.43 COL 178 COLON-ALIGNED
     varekost_ArtPris AT ROW 16.48 COL 178 COLON-ALIGNED
     mva%_ArtPris AT ROW 18.62 COL 178 COLON-ALIGNED
     Pris_ArtPris AT ROW 19.57 COL 178 COLON-ALIGNED
     LevNr AT ROW 2.43 COL 88 COLON-ALIGNED
     btnLevNr AT ROW 2.43 COL 102 NO-TAB-STOP 
     Vg AT ROW 3.62 COL 88 COLON-ALIGNED
     btnVg AT ROW 3.62 COL 102 NO-TAB-STOP 
     ProfilNr AT ROW 4.71 COL 17 COLON-ALIGNED WIDGET-ID 2
     rectBrowse AT ROW 7.19 COL 2.2
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 6.24 COL 2
     rectView AT ROW 7.19 COL 161
     rectWinToolbar AT ROW 1.24 COL 196.8
     rectView-2 AT ROW 13.86 COL 161
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 207 BY 31.52.


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
         TITLE              = "Prisendringer"
         HEIGHT             = 31.52
         WIDTH              = 207
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN DB%_ArtPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Innkjopspris_ArtPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lblHeader IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lblHeader-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mva%_ArtPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OrgAktiveresDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OrgDB% IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OrgGyldigTilDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN OrgPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Pris_ArtPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Rab1%_ArtPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN varekost_ArtPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

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
ON END-ERROR OF C-Win /* Prisendringer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prisendringer */
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


&Scoped-define SELF-NAME behType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL behType C-Win
ON VALUE-CHANGED OF behType IN FRAME DEFAULT-FRAME /* Type */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraAktiveresDato
DO:
  RUN Cal.w (fraAktiveresDato:HANDLE).
/*   IF fraAktiveresDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilDato C-Win
ON CHOOSE OF btnCalTilDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilAktiveresDato
DO:
  RUN Cal.w (tilAktiveresDato:HANDLE).
/*   IF tilAktiveresDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevNr C-Win
ON CHOOSE OF btnLevNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF SaSong
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
/*       LevNamn:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
/*     IF LevNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
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
/*     IF SaSong:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
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


&Scoped-define SELF-NAME btnVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVg C-Win
ON CHOOSE OF btnVg IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF SaSong
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Vg;VgBeskr".

  RUN JBoxDLookup.w ("VarGr;Vg;VgBeskr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      Vg:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
/*       VgBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|"). */
/*     IF Vg:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
  END.
  ELSE APPLY "entry" TO Vg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraAktiveresDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraAktiveresDato C-Win
ON LEAVE OF fraAktiveresDato IN FRAME DEFAULT-FRAME /* Aktiveres fra */
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


&Scoped-define SELF-NAME LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr C-Win
ON LEAVE OF LevNr IN FRAME DEFAULT-FRAME /* Leverandør */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ProfilNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProfilNr C-Win
ON VALUE-CHANGED OF ProfilNr IN FRAME DEFAULT-FRAME /* Pris profilnr */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
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


&Scoped-define SELF-NAME tbAvvikArtInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikArtInfo C-Win
ON VALUE-CHANGED OF tbAvvikArtInfo IN FRAME DEFAULT-FRAME /* (R2) Avvik artikkel info */
DO:
/*     RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikPris C-Win
ON VALUE-CHANGED OF tbAvvikPris IN FRAME DEFAULT-FRAME /* (R1) Avvik i priser */
DO:
/*     RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilAktiveresDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilAktiveresDato C-Win
ON LEAVE OF tilAktiveresDato IN FRAME DEFAULT-FRAME
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


&Scoped-define SELF-NAME Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Vg C-Win
ON LEAVE OF Vg IN FRAME DEFAULT-FRAME /* Varegruppe */
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
  IF VALID-HANDLE(hChild)        THEN APPLY "close" TO hChild.
  IF VALID-HANDLE(hArtBasSok)    THEN APPLY "close" TO hArtBasSok.
  IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
  
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle".
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
  
  DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
  DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.

  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


/* {incl/supptrigg.i hBrowse} */

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

IF NOT VALID-HANDLE(hArtikkelkort) THEN
  RUN w-vartkor.w  PERSIST SET hArtikkelkort (DYNAMIC-FUNCTION("getRecId","ArtBas",DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + STRING(fArtNr),"ROWID")), "ENDRE," + STRING(THIS-PROCEDURE)).
ELSE
  RUN ByttArtikkel IN hArtikkelkort (fArtNr).

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
    tbAvvikArtInfo:CHECKED         = FALSE
    tbAvvikPris:CHECKED            = FALSE
    fraAktiveresDato:SCREEN-VALUE  = '' 
    tilAktiveresDato:SCREEN-VALUE  = ''  
    Levkod:SCREEN-VALUE            = ''  
    SaSong:SCREEN-VALUE            = ''    
    VareTekst:SCREEN-VALUE         = '' 
    behStatus:SCREEN-VALUE         = ''
    
    fraAktiveresDato:MODIFIED      = FALSE 
    tilAktiveresDato:MODIFIED      = FALSE  
    Levkod:MODIFIED                = FALSE  
    SaSong:MODIFIED                = FALSE    
    VareTekst:MODIFIED             = FALSE 
    behStatus:MODIFIED             = FALSE
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
  Notes:       STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + 'KorrStatus' + ';' + ocValue
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Slette artikler ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).
                                                                     MESSAGE program-name(1) iReturn
                                                                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpimottak_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpimottak_delete.p",'').
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
RUN SUPER.
  IF VALID-HANDLE(hArtikkelkort) THEN
    RUN ByttArtikkel IN hArtikkelkort (IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                       ELSE 0).
  
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
  DISPLAY lblHeader behStatus behType tbAvvikPris tbAvvikArtInfo VareTekst 
          Levkod fraAktiveresDato tilAktiveresDato SaSong OrgPris 
          OrgGyldigTilDato OrgDB% OrgAktiveresDato lblHeader-2 
          Innkjopspris_ArtPris DB%_ArtPris Rab1%_ArtPris varekost_ArtPris 
          mva%_ArtPris Pris_ArtPris LevNr Vg ProfilNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE behStatus behType btnCalFraDato tbAvvikPris tbAvvikArtInfo 
         btnCalTilDato VareTekst Levkod fraAktiveresDato tilAktiveresDato 
         SaSong btnSaSong btnStartUtvalg LevNr btnLevNr Vg btnVg rectBrowse 
         rectToolBar searchField rectView rectWinToolbar rectView-2 ProfilNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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

/*   DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container"). */
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "VPIMottak"
                             + ";LevKod;Beskr;LevFargKod"
                             + ";+chkPris|logical|*/|chkPris(ROWID)|R1"
                             + ";+chkArtInfo|logical|*/|chkArtInfo(ROWID)|R2"
                             + ";InnkjopsPris;Rab1%" 
                             + ";+Db%|decimal|->>>><>>9.99|DB%|DB%"
                             + ";varekost;mva%;Pris"
                             + ";AktiveresDato;GyldigTilDato"
                             + ";OrgGyldigTilDato|OrgGyldig til;OrgAktiveresDato;OrgDB%;OrgPris|Org.pris"
                             + ";!ArtikkelNr;!VPIType;!VPIMottakId;!Vg;!SaSong;!RegistrertTid;!RegistrertDato;!RegistrertAv;!ProfilNr;!MomsKod;!GyldigTilTid;!EtikettSkrevet;!ETid;!EDato;!BrukerID;!AktiveresTid"
                             + ",ArtBas;!Artikkelnr"
                             + ";!+InnkjopsPris_artbas|decimal|>>>><>>9.99|Innkjopspris_artbas|Innkj.pris"
                             + ";!+Varekost_artbas|decimal|>>>><>>9.99|Varekost_artbas|Varekost"
                             + ",ArtPris;!ArtikkelNr;!ProfilNr" /*;!innkjopspris;!Rab1%;!Db%;!varekost;!mva%;!Pris*/
                             + ";!+InnkjopsPris_artpris|decimal|>>>><>>9.99|Innkjopspris_artpris|Innkj.pris"
                             + ";!+Rab1%_artpris|decimal|>>>><>>9.99|Rab1%_artpris|Rabatt1"
                             + ";!+DB%_artpris|decimal|>>>><>>9.99|DB%_artpris|DB%"
                             + ";!+Varekost_artpris|decimal|>>>><>>9.99|Varekost_artpris|Varekost"
                             + ";!+Mva%_artpris|decimal|>>>><>>9.99|Mva%_artpris|Mva%"
                             + ";!+Pris_artpris|decimal|>>>><>>9.99|Pris_artpris|Pris"
                             + ",SaSong;SaSong;SasBeskr"
                             + ",LevBas;LevNr|LevNr;LevNamn"
                             + ",sysPara;Parameter1|Status@1"
                             + ",buf2_sysPara;Parameter1|Type@2"
                             ,"WHERE FALSE "
                             + ", FIRST ArtBas  OUTER-JOIN WHERE ArtBas.artikkelnr   = VPIMottak.Artikkelnr "
                             + ", FIRST ArtPris OUTER-JOIN WHERE ArtPris.artikkelnr  = VPIMottak.Artikkelnr "
                                                          + " AND ArtPris.ProfilNR   = VPIMottak.ProfilNr "
                             + ", FIRST SaSong OUTER-JOIN OF VPIMottak NO-LOCK "
                             + ", FIRST LevBas OUTER-JOIN OF VPIMottak NO-LOCK "
                             + ", FIRST sysPara OUTER-JOIN WHERE sysPara.sysHid = 2 AND sysPara.sysGr = 10 AND sysPara.ParaNr = VPIMottak.behStatus NO-LOCK"
                             + ", FIRST buf2_sysPara OUTER-JOIN WHERE buf2_sysPara.sysHid = 2 " 
                                                              + " AND buf2_sysPara.sysGr = 9 "
                                                              + " AND buf2_sysPara.ParaNr = VPIMottak.VPIType NO-LOCK"
                             ,"").
  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"chkPris,chkArtInfo,chkStrek").
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'QuerySort','ArtikkelNr').
  
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpimottak_brwcalc.p').
/*   DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE). */
  
  ASSIGN 
    behStatus:DELIMITER = "|"
    behStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 2 and sysGr = 10")
  . 
  ASSIGN 
    behType:DELIMITER = "|"
    behType:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 2 and sysGr = 9")
  . 
  
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                      "",
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                      "OrgPris,OrgGyldigTilDato,OrgDB%,OrgAktiveresDato,Innkjopspris_ArtPris,Rab1%_ArtPris,DB%_ArtPris,Mva%_ArtPris,Pris_ArtPris",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "").                            /* other input widgets and lookup buttons for update fill-ins */
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "excel;Eksporter til E&xcel,BrowseConfig,Edit;&Endre,delete;&Slette,print;Utskrift,RowsToBatch;Antall &rader i resultatsett¤enable,rule,ArtikkelKort;&Artikkel kort,clearFilter;Blank &filter¤ENABLE"
                    + ",SendTilPrisRegister;Til &pris kø"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  hBrw_Pris = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "Pris",     
                    "Pris",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hBrw_Pris,"Pris").
  DYNAMIC-FUNCTION("setAttribute",hBrw_Pris,"refreshrow","yes"). 

  LoadRowsToBatchSettings().

  DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowse,  /* parent widget */
                  "setStatus;Endre status,setPris;Endre pris,setAktiveringsdato;Endre aktiveringsdato"
                 ,"").   
  
  DYNAMIC-FUNCTION("NewToolBar",
                  rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                  "Fil",                         /* Corresponding menu label - no menu if blank */
                  "close",
                  "right,enable").                      /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  

  RUN InvokeMethod (hBrowse,'OpenQuery').
  InitializeResize().
  
END.
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectView,rectView-2").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectView,rectView-2").
  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "btnCalFraDato,btnCalTilDato,fraAktiveresDato,tilAktiveresDato,btnStartUtvalg").
/*   DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,IMAGE-Sko").            */
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,650,850,0,250).
  
  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  
/*  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kobleArtikkelRecord C-Win 
PROCEDURE kobleArtikkelRecord :
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

  IF VALID-HANDLE(hArtBasSok) THEN 
    APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  IF VALID-HANDLE(hArtBasSok) THEN
  DO:
    RUN InitializeObject IN hArtBasSok.
    RUN MoveToTop IN hArtBasSok.
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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
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
DEF VAR cWhere AS CHAR NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkPris",tbAvvikPris:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtInfo",tbAvvikArtInfo:SCREEN-VALUE).

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
    cWhere = buildFilter(cWhere,varetekst:HANDLE,'beskr','BEGINS').      
    cWhere = cWhere + buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraAktiveresDato:HANDLE,'AktiveresDato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilAktiveresDato:HANDLE,'AktiveresDato','LE').
    cWhere = cWhere + buildFilter(cWhere,behStatus:HANDLE,'behStatus','EQ').
    cWhere = cWhere + buildFilter(cWhere,behType:HANDLE,'VPItype','EQ').
    cWhere = cWhere + buildFilter(cWhere,SaSong:HANDLE,'SaSong','EQ').
    cWhere = cWhere + buildFilter(cWhere,LevNr:HANDLE,'LevNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,Vg:HANDLE,'Vg','EQ').
/*     cWhere = cWhere + buildFilter(cWhere,tbAnonseArtikkel:HANDLE,'AnonseArtikkel','EQ'). */

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

    ASSIGN
      fraAktiveresDato:MODIFIED = FALSE 
      tilAktiveresDato:MODIFIED = FALSE  
      Levkod:MODIFIED           = FALSE  
      SaSong:MODIFIED           = FALSE    
      LevNr:MODIFIED            = FALSE    
      Vg:MODIFIED               = FALSE    
      VareTekst:MODIFIED        = FALSE 

    .

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
    RUN VPIMottak_artikkeldetalj.w PERSIST SET hChild.
  END.
  APPLY "value-changed" TO hBrowse.
  DYNAMIC-FUNCTION('setColour' IN hChild).

  RUN MoveToTop IN hChild.
  SUBSCRIBE TO 'InvalidateHandle' IN hChild.
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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpimottak_report.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpimottak_report.p",'').
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
  DEF VAR iGreen        AS INT INIT 10 NO-UNDO.
  DEF VAR iLightBlue    AS INT INIT 11 NO-UNDO.
  DEF VAR iRed          AS INT INIT 12 NO-UNDO.
  DEF VAR iYellow       AS INT INIT 14 NO-UNDO.
  DEF VAR iLightGreen   AS INT INIT 18 NO-UNDO.
  DEF VAR iLightYellow  AS INT INIT 16 NO-UNDO.
  DEF VAR iLightRed     AS INT INIT 17 NO-UNDO.

/*   IF hbfPris:BUFFER-VALUE NE hbfOrgPris:BUFFER-VALUE THEN */
/*     ASSIGN                                                */
/*       hbcPris:BGCOLOR = iLightBlue                        */
/*     .                                                     */
  IF hbfInnkjopsPris:BUFFER-VALUE NE hbfinnkjopspris_artpris:BUFFER-VALUE THEN
    ASSIGN
      hbcInnkjopsPris:BGCOLOR = IF hbfInnkjopsPris:BUFFER-VALUE GT hbfinnkjopspris_artpris:BUFFER-VALUE THEN iLightBlue ELSE iLightRed
    .
  IF hbfRab1%:BUFFER-VALUE NE hbfRab1%_artpris:BUFFER-VALUE THEN
   ASSIGN
     hbcRab1%:BGCOLOR = IF hbfRab1%:BUFFER-VALUE GT hbfRab1%_artpris:BUFFER-VALUE THEN iLightBlue ELSE iLightRed
   .
  IF hbfDB%:BUFFER-VALUE NE hbfDB%_artpris:BUFFER-VALUE THEN
  ASSIGN
    hbcDB%:BGCOLOR = IF hbfDB%:BUFFER-VALUE GT hbfDB%_artpris:BUFFER-VALUE THEN iLightBlue ELSE iLightRed
  .
  IF hbfVarekost:BUFFER-VALUE NE hbfVarekost_artpris:BUFFER-VALUE THEN
   ASSIGN
     hbcVarekost:BGCOLOR = IF hbfVarekost:BUFFER-VALUE GT hbfVarekost_artpris:BUFFER-VALUE THEN iLightBlue ELSE iLightRed
   .
  IF hbfMva%:BUFFER-VALUE NE hbfMva%_artpris:BUFFER-VALUE THEN
   ASSIGN
     hbcMva%:BGCOLOR = IF hbfMva%:BUFFER-VALUE GT hbfMva%_artpris:BUFFER-VALUE THEN iLightBlue ELSE iLightRed
   .
  IF hbfPris:BUFFER-VALUE NE hbfPris_artpris:BUFFER-VALUE THEN
   ASSIGN
     hbcPris:BGCOLOR = IF hbfPris:BUFFER-VALUE GT hbfPris_artpris:BUFFER-VALUE THEN iLightBlue ELSE iLightRed
   .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendKorreksjonRecord C-Win 
PROCEDURE sendKorreksjonRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  IF NOT behStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '10' THEN
  DO:
    MESSAGE 'Status må settes til "Koblet", for å kunne utføre Sending.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    UNDO, RETURN.
  END.

  RUN JBoxBrowseSelectMsg.w ("Send valgte poster?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                OUTPUT iReturn).  /*1=Alle,2=Valgte*/
  IF iReturn = 1 THEN
    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"korrVPIMottak_send.p"
                          ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE)                          
                           ).
  ELSE
    bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"korrVPIMottak_send.p"
                           ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) 
                           ).
  
  IF NOT bOK THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 
  ELSE 
    RUN InvokeMethod (hBrowse,'OpenQuery').
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTilPrisRegisterRecord C-Win 
PROCEDURE SendTilPrisRegisterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn     AS INT NO-UNDO.

RUN JBoxBrowseSelectMsg.w ("Send valgte poster?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpimottak_sendtilpris.p",'').
ELSE
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpimottak_sendtilpris.p",'').
END.

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i endring av linformasjon ",""). 
ELSE 
  IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAktiveringsdatoRecord C-Win 
PROCEDURE setAktiveringsdatoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ("Velg ønsket dato for endring av poster",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999
                              ,'DATE|99/99/9999|' + STRING(TODAY)
                              ,OUTPUT ocValue
                              ,OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpimottak_setaktiveringsdato.p",ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpimottak_setaktiveringsdato.p",ocValue).
END.
ELSE
  LEAVE.
IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPrisRecord C-Win 
PROCEDURE setPrisRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w ("Velg ønsket pris for endring av poster",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999
                              ,'DECIMAL|>>>>9.99'
                              ,OUTPUT ocValue
                              ,OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 1 THEN
DO:
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpimottak_setpris.p",ocValue).
  RUN InvokeMethod (hBrowse,'OpenQuery').
END.
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpimottak_setpris.p",ocValue).
END.
ELSE
  LEAVE.
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
  cStatusList = DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 2 and sysGr = 10 and Parameter2 = ''")
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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpimottak_setstatus.p",ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpimottak_setstatus.p",ocValue).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    INPUT icAction     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR bReturnFocus  AS LOG   NO-UNDO.
  DEF VAR cStrList      AS CHAR  NO-UNDO.
  DEF VAR bOk           AS LOG   NO-UNDO.
  DEF VAR iArtnr        AS INT   NO-UNDO.
  DEF VAR fArtikkelPris AS DEC   NO-UNDO.
  DEF VAR bCurrentFlag  AS LOG   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    MESSAGE 
      'Ønsker du å koble VPI artikkel ' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
    + ' mot artikkel ' + STRING(ifArtikkelNr)
    VIEW-AS ALERT-BOX INFO BUTTONS OK-CANCEL UPDATE bOk.
  IF bOk THEN
  DO:
    bok =  DYNAMIC-FUNCTION('runProc','korrVPIMottak_koble.p',STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE)
                                                                + ';' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
                                                                + ';' + STRING(ifArtikkelNr)
                           ,?).
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
    APPLY "value-changed" TO hBrowse.

    RETURN bOk.    
  END.
END. /*FRAME*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ihBrowse:MOVE-COLUMN(21,17).
  ihBrowse:MOVE-COLUMN(22,18).
  ihBrowse:MOVE-COLUMN(23,19).
  ihBrowse:MOVE-COLUMN(24,20).
  ASSIGN
    ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 70
    ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 70
    ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 70
    ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 120
    ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 70
    ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Pris')):WIDTH-PIXELS  = 80
    
    hbcPris           = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Pris'))
    hbfPris           = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Pris')
    hbcInnkjopspris   = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Innkjopspris'))
    hbfInnkjopspris   = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Innkjopspris')   
    hbcRab1%          = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Rab1%')) 
    hbfRab1%          = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Rab1%')    
    hbcDB%            = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'DB%')) 
    hbfDB%            = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DB%')    
    hbcVarekost       = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Varekost')) 
    hbfVarekost       = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Varekost')    
    hbcMva%           = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Mva%')) 
    hbfMva%           = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Mva%')    
    
/*     hbcInnkjopspris_ArtPris = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Innkjopspris_ArtPris')) */
    hbfInnkjopspris_ArtPris = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Innkjopspris_ArtPris')   
/*     hbcRab1%_ArtPris        = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Rab1%_ArtPris')) */
    hbfRab1%_ArtPris        = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Rab1%_ArtPris')    
/*     hbcDB%_ArtPris          = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'DB%_ArtPris')) */
    hbfDB%_ArtPris          = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DB%_ArtPris')    
/*     hbcVarekost_ArtPris     = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Varekost_ArtPris')) */
    hbfVarekost_ArtPris     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Varekost_ArtPris')    
/*     hbcMva%_ArtPris         = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Mva%_ArtPris')) */
    hbfMva%_ArtPris         = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Mva%_ArtPris')    
/*     hbcPris_ArtPris         = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'Pris_ArtPris')) */
    hbfPris_ArtPris         = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Pris_ArtPris')

    hbcOrgPris        = ihBrowse:GET-BROWSE-COLUMN(getColumnIndex(ihBrowse,'OrgPris'))
    hbfOrgPris        = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('OrgPris')
                      
    hbcChkPris        = ihBrowse:GET-BROWSE-COLUMN(6)
    hbfChkPris        = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkPris')
    hbcChkArtInfo     = ihBrowse:GET-BROWSE-COLUMN(7)
    hbfChkArtInfo     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtInfo')

    hbcChkPris:COLUMN-FONT              = 8
    hbcChkArtInfo:COLUMN-FONT           = 8
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getColumnIndex C-Win 
FUNCTION getColumnIndex RETURNS INTEGER
  (INPUT ihBrowse AS HANDLE,
   INPUT icColumnName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ix      AS INT NO-UNDO.

  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = icColumnName THEN 
      RETURN ix.
  END.
  RETURN ?.
  

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

