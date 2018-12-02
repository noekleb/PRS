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
 
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBrowse      AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.

DEF VAR VarebokNr         AS INT NO-UNDO.

DEF VAR hbcChkArtReg     AS HANDLE NO-UNDO.
DEF VAR hbfChkArtReg     AS HANDLE NO-UNDO.
DEF VAR hbcChkPris       AS HANDLE NO-UNDO.
DEF VAR hbfChkPris       AS HANDLE NO-UNDO.
DEF VAR hbcChkArtInfo    AS HANDLE NO-UNDO.
DEF VAR hbfChkArtInfo    AS HANDLE NO-UNDO.
DEF VAR hbcChkStrek      AS HANDLE NO-UNDO.
DEF VAR hbfChkStrek      AS HANDLE NO-UNDO.
DEF VAR hArtikkelKort    AS HANDLE NO-UNDO.
DEF VAR hSearchField     AS HANDLE NO-UNDO.
DEF VAR hArtBasSok       AS HANDLE NO-UNDO.

DEF VAR cRowIdList       AS CHAR   NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnCalFraDato btnCalTilDato SaSong ~
tbAvvikArtBas tbAvvikStrekKode tbAvvikPris tbAvvikArtInfo VareTekst Levkod ~
fraVPIDato tilVPIDato btnSaSong KorrStatus btnStartUtvalg rectBrowse ~
rectToolBar searchField 
&Scoped-Define DISPLAYED-OBJECTS SaSong tbAvvikArtBas tbAvvikStrekKode ~
tbAvvikPris tbAvvikArtInfo VareTekst Levkod fraVPIDato tilVPIDato ~
KorrStatus 

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

DEFINE BUTTON btnSaSong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStartUtvalg  NO-FOCUS
     LABEL "Start søk" 
     SIZE 13 BY 1.

DEFINE VARIABLE KorrStatus AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fraVPIDato AS DATE FORMAT "99/99/99":U 
     LABEL "VPIdato fra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Levkod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.Artnr" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE SaSong AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Sessong" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE tilVPIDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE VareTekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 168.8 BY 11.67.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 15 BY .95.

DEFINE VARIABLE tbAvvikArtBas AS LOGICAL INITIAL NO 
     LABEL "(R1) Finnes ikke i artikkelregister" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 TOOLTIP "Artikkel finnes ikke i artikkelregister" NO-UNDO.

DEFINE VARIABLE tbAvvikArtInfo AS LOGICAL INITIAL NO 
     LABEL "(R3) Avvik artikkel info" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikPris AS LOGICAL INITIAL NO 
     LABEL "(R2) Avvik i priser" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tbAvvikStrekKode AS LOGICAL INITIAL NO 
     LABEL "(R4) Avvik strek kode" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCalFraDato AT ROW 4.62 COL 108
     btnCalTilDato AT ROW 4.52 COL 126
     SaSong AT ROW 5.71 COL 92 COLON-ALIGNED
     tbAvvikArtBas AT ROW 3.57 COL 10
     tbAvvikStrekKode AT ROW 4.38 COL 48
     tbAvvikPris AT ROW 4.38 COL 10
     tbAvvikArtInfo AT ROW 3.57 COL 48
     VareTekst AT ROW 2.38 COL 92 COLON-ALIGNED
     Levkod AT ROW 3.43 COL 92 COLON-ALIGNED
     fraVPIDato AT ROW 4.52 COL 92 COLON-ALIGNED
     tilVPIDato AT ROW 4.52 COL 110 COLON-ALIGNED NO-LABEL
     btnSaSong AT ROW 5.71 COL 100 NO-TAB-STOP 
     KorrStatus AT ROW 2.38 COL 8 COLON-ALIGNED
     btnStartUtvalg AT ROW 5.95 COL 118 NO-TAB-STOP 
     rectBrowse AT ROW 7.67 COL 2
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 6.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170.2 BY 18.52.


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
         HEIGHT             = 18.52
         WIDTH              = 170.2
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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


&Scoped-define SELF-NAME fraVPIDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraVPIDato C-Win
ON LEAVE OF fraVPIDato IN FRAME DEFAULT-FRAME /* VPIdato fra */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KorrStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KorrStatus C-Win
ON VALUE-CHANGED OF KorrStatus IN FRAME DEFAULT-FRAME /* Status */
DO:
  RUN InvokeMethod (hBrowse,'OpenQuery').
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
ON VALUE-CHANGED OF tbAvvikArtInfo IN FRAME DEFAULT-FRAME /* (R3) Avvik artikkel info */
DO:
/*     RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikPris C-Win
ON VALUE-CHANGED OF tbAvvikPris IN FRAME DEFAULT-FRAME /* (R2) Avvik i priser */
DO:
/*     RUN InvokeMethod (hBrowse,'OpenQuery'). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikStrekKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikStrekKode C-Win
ON VALUE-CHANGED OF tbAvvikStrekKode IN FRAME DEFAULT-FRAME /* (R4) Avvik strek kode */
DO:
/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */
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
  IF VALID-HANDLE(hChild)        THEN APPLY "close" TO hChild.
  IF VALID-HANDLE(hArtBasSok)    THEN APPLY "close" TO hArtBasSok.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtBasEndret C-Win 
PROCEDURE ArtBasEndret :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

  DEF VAR piEkstVPILevNr AS INT NO-UNDO.

  DEF BUFFER bvpiArtBas    FOR VPIArtBas.
  DEF BUFFER bvpiArtPris   FOR VPIArtPris.
  DEF BUFFER bvpiStrekkode FOR VPIStrekkode.
  DEF BUFFER bArtBas       FOR ArtBas.
  DEF BUFFER bArtPris      FOR ArtPris.
  DEF BUFFER bStrekkode    FOR Strekkode.

  ASSIGN 
    piEkstVPILevNr = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE.

  DO TRANSACTION:
      FIND bVPIArtBas EXCLUSIVE-LOCK WHERE
          bVPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
          bVPIArtBas.VareNr       = STRING(plArtikkelNr) NO-ERROR.
      FIND bArtBas EXCLUSIVE-LOCK WHERE
          bArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
      IF (AVAILABLE bVPIartBas AND
          AVAILABLE bArtBas) THEN DO:
          BUFFER-COPY bArtBas EXCEPT Katalogpris ForhRab%
              TO bVPIArtBas
              ASSIGN
              bVPIArtBas.KatalogPris[1] = bArtBas.KatalogPris
              bVPIArtBas.ForhRab%[1]    = bArtBas.ForhRab%.

          RELEASE bVPIArtBas.
          RELEASE bArtBas.
      END.

      FIND FIRST bVPIArtPris EXCLUSIVE-LOCK WHERE
          bVPIArtPris.EkstVPILevNr = piEkstVPILevNr AND
          bVPIArtPris.VareNr       = STRING(plArtikkelNr) NO-ERROR.
      FIND FIRST bArtPris EXCLUSIVE-LOCK WHERE
          bArtPris.ArtikkelNr = plArtikkelNr NO-ERROR.
      IF (AVAILABLE bVPIartPris AND
          AVAILABLE bArtPris) THEN DO:
          BUFFER-COPY bArtPris
              TO bVPIArtPris.

          RELEASE bVPIArtPris.
          RELEASE bArtPris.
      END.

  END. /* Transaction */

  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

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
    fraVPIDato:SCREEN-VALUE  = '' 
    tilVPIDato:SCREEN-VALUE  = ''  
    Levkod:SCREEN-VALUE      = ''  
    SaSong:SCREEN-VALUE      = ''    
    VareTekst:SCREEN-VALUE   = '' 
    KorrStatus:SCREEN-VALUE  = ''
    
    fraVPIDato:MODIFIED      = FALSE 
    tilVPIDato:MODIFIED      = FALSE  
    Levkod:MODIFIED          = FALSE  
    SaSong:MODIFIED          = FALSE    
    VareTekst:MODIFIED       = FALSE 
    KorrStatus:MODIFIED      = FALSE
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

  RUN JBoxBrowseMsgUpdateVal.w ("Slette artikler ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_delete.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_delete.p",'').
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
  DISPLAY SaSong tbAvvikArtBas tbAvvikStrekKode tbAvvikPris tbAvvikArtInfo 
          VareTekst Levkod fraVPIDato tilVPIDato KorrStatus 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnCalFraDato btnCalTilDato SaSong tbAvvikArtBas tbAvvikStrekKode 
         tbAvvikPris tbAvvikArtInfo VareTekst Levkod fraVPIDato tilVPIDato 
         btnSaSong KorrStatus btnStartUtvalg rectBrowse rectToolBar searchField 
      WITH FRAME DEFAULT-FRAME.
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

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "VPIArtBas"
                             + ";!KorrStatus;ArtikkelNr|Art.nr;LevKod;Beskr;LevFargKod"
                             + ";+chkArtReg|logical|*/|chkArtReg(ROWID)|R1"
                             + ";+chkPris|logical|*/|chkPris(ROWID)|R2"
                             + ";+chkArtInfo|logical|*/|chkArtInfo(ROWID)|R3"
                             + ";+chkStrek|logical|*/|chkStrek(ROWID)|R4"
                             + ";+InnkjopsPris#1|decimal|>>>><>>9.99|InnkjopsPris#1|Innkj.pris"
                             + ";+Rab1%#1|decimal|->>>><>>9.99|Rab1%#1|Rabatt1%"
                             + ";+DB%#1|decimal|->>>><>>9.99|DB%#1|DB%"
                             + ";+Pris#1|decimal|>>>><>>9.99|Pris#1|Pris"
                             + ";KorrArtikkelNr;!Gjennomfaktureres;!AntIPkn;!StrKode2;!StrKode1;!GarantiKl;!BestForslag;!Pant;!IndividType;!ArtSlag;!ManRabIKas;!Mengde;!LinkVareNr;!VPIBildeKode;!VPIDato;!LevDato4;!LevDato3;!Linjemerknad;!KjedeVare;!HovedModellFarge;!PrisGrpNr;!SentralBestilling;!ModellFarge;!LokArtikkelNr;!Oppdatert;!SalgsEnhet;!Etikett;!HandKode;!Pakkenr;!BehKode;!KjentPaHK;!HKVareId;!IKasse;!LokPris;!HkStyrt;!Alder;!Pakke;!BildeIKasse;!OLLager;!OPris;!SattPaKampanje;!VisDivInfo;!DivInfo;!LevDato2;!LevDato1;!Storrelser;!RegistrertAv;!RegistrertTid;!RegistrertDato;!BrukerID;!ETid;!EDato;!ProdNr;!StrTypeID;!VgKat;!AnonseArtikkel;!BongTekst;!Notat;!VMId;!Lager;!ValKod;!ProvKod;!RabKod;!Anv-Id;!Slit-Id;!Inner-Id;!Foder-Id;!Last-Id;!Ov-Id;!Kommentar;!Tilv-Land;!BildNr;!MatKod;!Klack;!Farg;!LopNr;!Vg;!Hg"
                             + ";!EkstVPILevNr;!LevVareTekst;!KundeRabatt;!VareNr"
                             + ",VPIArtPris;!Profilnr|ProfilNr|>>>>>>9"
                             + ",SaSong;SaSong;SasBeskr"
                             + ",LevBas;LevNr;LevNamn"
                             + ",sysPara;Parameter1|Status"
                             ,"WHERE FALSE "
                             + ", FIRST VPIArtPris OUTER-JOIN WHERE VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr " 
                                                            + " AND VPIArtPris.VareNr       = VPIArtBas.VareNr "
                                                            + " AND VPIArtPris.ProfilNr     = 1 NO-LOCK "
                             + ", FIRST SaSong OUTER-JOIN OF VPIArtBas NO-LOCK "
                             + ", FIRST LevBas OUTER-JOIN OF VPIArtBas NO-LOCK "
                             + ", FIRST sysPara OUTER-JOIN WHERE sysPara.sysHid = 21 AND sysPara.sysGr = 201 AND sysPara.ParaNr = VPIArtBas.KorrStatus NO-LOCK"
                             ,"").
  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"chkArtReg,chkPris,chkArtInfo,chkStrek").
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'QuerySort','ArtikkelNr').
  
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpiartbas_artbas_brwcalc.p').
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  ASSIGN 
    KorrStatus:DELIMITER = "|"
    KorrStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 21 and sysGr = 201")
  . 
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "excel;Eksporter til E&xcel,BrowseConfig,Edit;&Endre,delete;&Slette,Print;S&kriv,rule,kobleArtikkel;Koble artikkel,ArtikkelKort;&Artikkel kort,sendKorreksjon;Send &korreksjon,clearFilter;Blank &filter¤ENABLE"
                    + ",RowsToBatch;Antall &rader i resultatsett¤enable"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  LoadRowsToBatchSettings().

  DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowse,  /* parent widget */
                  "setStatus;Endre status"
                 ,"").   
  
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  

/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.
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
    DYNAMIC-FUNCTION("setHideStr" IN hArtBasSok).
    RUN InitializeObject IN hArtBasSok.
    DYNAMIC-FUNCTION('setCloseOnSelect' IN hArtBasSok,YES).
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
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtReg",tbAvvikArtBas:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkPris",tbAvvikPris:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtInfo",tbAvvikArtInfo:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkStrek",tbAvvikStrekKode:SCREEN-VALUE).

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
    cWhere = buildFilter(cWhere,varetekst:HANDLE,'beskr','BEGINS').      
    cWhere = cWhere + buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraVPIdato:HANDLE,'VPIdato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilVPIdato:HANDLE,'VPIdato','LE').
    cWhere = cWhere + buildFilter(cWhere,SaSong:HANDLE,'SaSong','EQ').
    cWhere = cWhere + buildFilter(cWhere,KorrStatus:HANDLE,'KorrStatus','EQ').
/*     cWhere = cWhere + buildFilter(cWhere,tbAnonseArtikkel:HANDLE,'AnonseArtikkel','EQ'). */

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

    ASSIGN
      fraVPIDato:MODIFIED      = FALSE 
      tilVPIDato:MODIFIED      = FALSE  
      Levkod:MODIFIED          = FALSE  
      SaSong:MODIFIED          = FALSE    
      VareTekst:MODIFIED       = FALSE 

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
    /*RUN korrvpiartbas_artikkeldetalj.w PERSIST SET hChild.*/
    RUN vpiartbas_artikkeldetalj.w PERSIST SET hChild.
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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"korrvpiartbas_report.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"korrvpiartbas_report.p",'').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendKorreksjonRecord C-Win 
PROCEDURE sendKorreksjonRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
DEF VAR cRowIdList AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF NOT KorrStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '10' THEN
  DO:
    MESSAGE 'Status i FILTER må settes til "Koblet", for å kunne utføre Sending.' SKIP
            'Kun poster som har status "Koblet" vil bli sendt.'
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
    bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"korrvpiartbas_send.p"
                          ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE)                          
                           ).
  ELSE IF iReturn = 2 THEN
  DO:
    DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
        cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
    END.
    bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"korrvpiartbas_send.p"
                           ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) 
                           ).
  END.
  ELSE
    LEAVE.
  
  IF NOT bOK THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 
  
  IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).


  
END.
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
DEF VAR cRowIdList  AS CHAR NO-UNDO.

ASSIGN 
  cStatusList = DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 21 and sysGr = 201 and Parameter2 = ''")
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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_setStatus.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + 'KorrStatus' + ';' + ocValue).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_setStatus.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) + ';' + 'KorrStatus' + ';' + ocValue).
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
  Purpose: Koble artikkel. Kalles fra artbassok.w 
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
    bok =  DYNAMIC-FUNCTION('runProc','korrvpiartbas_koble.p',STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE)
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
  ihBrowse:MOVE-COLUMN(18,1).  /* Statustekst legges først */
  /* ihBrowse:MOVE-COLUMN(14,18). */

  ASSIGN
    ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 70
    ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 50
    ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS  = 50
    ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS  = 150
    ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS  = 100
    ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS  = 20
    ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS  = 20
    
    hbcChkArtReg   = ihBrowse:GET-BROWSE-COLUMN(6)
    hbfChkArtReg   = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtReg')
    hbcChkPris     = ihBrowse:GET-BROWSE-COLUMN(7)
    hbfChkPris     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkPris')
    hbcChkArtInfo  = ihBrowse:GET-BROWSE-COLUMN(8)
    hbfChkArtInfo  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtInfo')
    hbcChkStrek    = ihBrowse:GET-BROWSE-COLUMN(9)
    hbfChkStrek    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrek')
    
    hbcChkArtReg:COLUMN-FONT            = 8
    hbcChkPris:COLUMN-FONT              = 8
    hbcChkArtInfo:COLUMN-FONT           = 8
    hbcChkStrek:COLUMN-FONT             = 8
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

