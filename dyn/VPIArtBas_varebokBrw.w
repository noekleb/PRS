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

DEF VAR VarebokNr         AS dec NO-UNDO.

DEF VAR hbcChkArtReg         AS HANDLE NO-UNDO.
DEF VAR hbfChkArtReg         AS HANDLE NO-UNDO.
DEF VAR hbcChkVarebok        AS HANDLE NO-UNDO.
DEF VAR hbfChkVarebok        AS HANDLE NO-UNDO.
DEF VAR hbcChkPris           AS HANDLE NO-UNDO.
DEF VAR hbfChkPris           AS HANDLE NO-UNDO.
DEF VAR hbcChkArtInfo        AS HANDLE NO-UNDO.
DEF VAR hbfChkArtInfo        AS HANDLE NO-UNDO.
DEF VAR hbcChkStrek          AS HANDLE NO-UNDO.
DEF VAR hbfChkStrek          AS HANDLE NO-UNDO.
DEF VAR hArtikkelKort        AS HANDLE NO-UNDO.
DEF VAR hSearchField         AS HANDLE NO-UNDO.
DEF VAR hbcChkAnonseArtikkel AS HANDLE NO-UNDO.
DEF VAR hbfChkAnonseArtikkel AS HANDLE NO-UNDO.
DEF VAR hbcChkKontroll       AS HANDLE NO-UNDO.
DEF VAR hbfChkKontroll       AS HANDLE NO-UNDO.

DEF VAR cRowidList           AS CHAR   NO-UNDO.

/*Valg av poster*/
DEF VAR cDato   AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS behStatus btnLevNr SaSong btnSaSong ~
tbAvvikVarebok tbAvvikArtBas tbAvvikStrekKode btnCalFraDato tbAvvikPris ~
btnCalTilDato tbAvvikArtInfo VareTekst Levkod Utvidetsok Strekkode ~
tbAnonseArtikkel LevNr fraVPIDato tilVPIDato cbcKontroll btnLev ~
btnStartUtvalg rectBrowse rectToolBar searchField 
&Scoped-Define DISPLAYED-OBJECTS behStatus Varebokbeskrivelse SaSong ~
Varebok tbAvvikVarebok tbAvvikArtBas tbAvvikStrekKode tbAvvikPris ~
tbAvvikArtInfo VareTekst Levkod Utvidetsok Strekkode tbAnonseArtikkel LevNr ~
fraVPIDato tilVPIDato cbcKontroll 

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
FUNCTION getVarebokNr RETURNS DECIMAL
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

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSaSong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStartUtvalg  NO-FOCUS
     LABEL "Start søk" 
     SIZE 13 BY 1.

DEFINE VARIABLE behStatus AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cbcKontroll AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "(R7) Kontroll" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

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

DEFINE VARIABLE SaSong AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Sessong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tilVPIDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Utvidetsok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utvidetsøk" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE Varebok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varebok" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE Varebokbeskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE VareTekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185 BY 10.48.

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

DEFINE VARIABLE tbAvvikVarebok AS LOGICAL INITIAL no 
     LABEL "(R2) Finnes ikke i varebok" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     behStatus AT ROW 2.43 COL 91 COLON-ALIGNED
     btnLevNr AT ROW 3.62 COL 180.8 NO-TAB-STOP 
     Varebokbeskrivelse AT ROW 2.43 COL 33.4 COLON-ALIGNED NO-LABEL
     SaSong AT ROW 2.43 COL 164.6 COLON-ALIGNED
     btnSaSong AT ROW 2.43 COL 180.8 NO-TAB-STOP 
     Varebok AT ROW 2.43 COL 12 COLON-ALIGNED
     tbAvvikVarebok AT ROW 3.62 COL 14.2
     tbAvvikArtBas AT ROW 3.62 COL 45.2
     tbAvvikStrekKode AT ROW 4.38 COL 45.2
     btnCalFraDato AT ROW 4.91 COL 163
     tbAvvikPris AT ROW 4.43 COL 14.2
     btnCalTilDato AT ROW 4.81 COL 181
     tbAvvikArtInfo AT ROW 5.24 COL 14.2
     VareTekst AT ROW 3.62 COL 91 COLON-ALIGNED
     Levkod AT ROW 4.67 COL 91 COLON-ALIGNED
     Utvidetsok AT ROW 5.76 COL 91 COLON-ALIGNED
     Strekkode AT ROW 6.95 COL 91 COLON-ALIGNED
     tbAnonseArtikkel AT ROW 5.29 COL 45.2
     LevNr AT ROW 3.62 COL 164.6 COLON-ALIGNED
     fraVPIDato AT ROW 4.81 COL 147 COLON-ALIGNED
     tilVPIDato AT ROW 4.81 COL 165 COLON-ALIGNED NO-LABEL
     cbcKontroll AT ROW 6.24 COL 43 COLON-ALIGNED
     btnLev AT ROW 2.43 COL 31.2 NO-TAB-STOP 
     btnStartUtvalg AT ROW 7.19 COL 172 NO-TAB-STOP 
     rectBrowse AT ROW 8.38 COL 1
     rectToolBar AT ROW 1.24 COL 2
     searchField AT ROW 7.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 186.2 BY 18.05.


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
         WIDTH              = 186.2
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
/* SETTINGS FOR FILL-IN Varebok IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varebok:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Varebokbeskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Varebokbeskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "VarebokHode"
                     + ";VarebokNr|Vareboknr|->>>>>>>>>>9;MesseNr;VarebokBeskrivelse;RegistrertDato;RegistrertAv"
                     ,"WHERE true"
                    ,""
                    ,"VarebokNr,VarebokBeskrivelse",
                    OUTPUT cReturnList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnList NE "" THEN DO:
    ASSIGN 
      VarebokNr                       = DEC(ENTRY(1,cReturnList,"|"))
      Varebok:SCREEN-VALUE            = ENTRY(1,cReturnList,"|")
      VarebokBeskrivelse:SCREEN-VALUE = ENTRY(2,cReturnList,"|")
    .
    RUN InvokeMethod (hBrowse,'OpenQuery').
  END.
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
/*   IF varebok:SCREEN-VALUE NE '' THEN                    */
/*     RUN InvokeMethod (hBrowse,'OpenQuery').             */
/*   ELSE                                                  */
/*   DO:                                                   */
/*     MESSAGE 'Varebok må angies for å filtrer med denne' */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
/*     SELF:CHECKED = FALSE.                               */
/*   END.                                                  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAvvikPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikPris C-Win
ON VALUE-CHANGED OF tbAvvikPris IN FRAME DEFAULT-FRAME /* (R3) Avvik i priser */
DO:
/*   IF varebok:SCREEN-VALUE NE '' THEN                    */
/*     RUN InvokeMethod (hBrowse,'OpenQuery').             */
/*   ELSE                                                  */
/*   DO:                                                   */
/*     MESSAGE 'Varebok må angies for å filtrer med denne' */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
/*     SELF:CHECKED = FALSE.                               */
/*   END.                                                  */
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


&Scoped-define SELF-NAME tbAvvikVarebok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAvvikVarebok C-Win
ON VALUE-CHANGED OF tbAvvikVarebok IN FRAME DEFAULT-FRAME /* (R2) Finnes ikke i varebok */
DO:
/*   IF varebok:SCREEN-VALUE NE '' THEN                    */
/*     RUN InvokeMethod (hBrowse,'OpenQuery').             */
/*   ELSE                                                  */
/*   DO:                                                   */
/*     MESSAGE 'Varebok må angies for å filtrer med denne' */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
/*     SELF:CHECKED = FALSE.                               */
/*   END.                                                  */
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
    tbAvvikArtBas:CHECKED           = FALSE 
    tbAvvikArtInfo:CHECKED          = FALSE
    tbAvvikPris:CHECKED             = FALSE
    tbAvvikStrekKode:CHECKED        = FALSE
    tbAvvikVarebok:CHECKED          = FALSE
    Varebok:SCREEN-VALUE            = ''
    VarebokNr                       = 0
    fraVPIDato:SCREEN-VALUE         = '' 
    tilVPIDato:SCREEN-VALUE         = ''  
    Levkod:SCREEN-VALUE             = ''  
    Utvidetsok:SCREEN-VALUE         = ''  
    SaSong:SCREEN-VALUE             = ''    
    LevNr:SCREEN-VALUE              = ''    
    VareTekst:SCREEN-VALUE          = ''
    VarebokBeskrivelse:SCREEN-VALUE = ''
    behStatus:SCREEN-VALUE          = ''

    tbAnonseArtikkel:CHECKED        = FALSE
    
    fraVPIDato:MODIFIED             = FALSE 
    tilVPIDato:MODIFIED             = FALSE  
    Levkod:MODIFIED                 = FALSE  
    SaSong:MODIFIED                 = FALSE    
    Utvidetsok:MODIFIED             = FALSE 
    LevNr:MODIFIED                  = FALSE    
    VareTekst:MODIFIED              = FALSE 
    VarebokBeskrivelse:MODIFIED     = FALSE
    tbAnonseArtikkel:MODIFIED       = FALSE
    behStatus:MODIFIED              = FALSE
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
DYNAMIC-FUNCTION('setCurrentObject',hBrowse).
RUN SUPER.
  IF VALID-HANDLE(hArtikkelkort) THEN
    RUN ByttArtikkel IN hArtikkelkort (IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
                                       ELSE 0).
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      tbAvvikArtInfo:SENSITIVE = varebok:SCREEN-VALUE <> ''
      tbAvvikPris:SENSITIVE    = varebok:SCREEN-VALUE <> '' 
      tbAvvikVarebok:SENSITIVE = varebok:SCREEN-VALUE <> ''
    .
  
  END.
  IF VALID-HANDLE(hChild) THEN DYNAMIC-FUNCTION('setColour' IN hChild).
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
  DISPLAY behStatus Varebokbeskrivelse SaSong Varebok tbAvvikVarebok 
          tbAvvikArtBas tbAvvikStrekKode tbAvvikPris tbAvvikArtInfo VareTekst 
          Levkod Utvidetsok Strekkode tbAnonseArtikkel LevNr fraVPIDato 
          tilVPIDato cbcKontroll 
      WITH FRAME DEFAULT-FRAME.
  ENABLE behStatus btnLevNr SaSong btnSaSong tbAvvikVarebok tbAvvikArtBas 
         tbAvvikStrekKode btnCalFraDato tbAvvikPris btnCalTilDato 
         tbAvvikArtInfo VareTekst Levkod Utvidetsok Strekkode tbAnonseArtikkel 
         LevNr fraVPIDato tilVPIDato cbcKontroll btnLev btnStartUtvalg 
         rectBrowse rectToolBar searchField 
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
                             + ";ArtikkelNr|Art.nr"
                             + ";LevKod"
                             + ";Beskr|Beskrivelse|x(50)"
                             + ";LevFargKod"
                             + ";RAvdNr|Område.nr"
                             + ";+chkArtReg|logical|*/|chkArtReg(ROWID)|R1"
                             + ";+chkVarebok|logical|*/|chkVarebok(ROWID)|R2"
                             + ";+chkPris|logical|*/|chkPris(ROWID)|R3"
                             + ";+chkArtInfo|logical|*/|chkArtInfo(ROWID)|R4"
                             + ";+chkStrek|logical|*/|chkStrek(ROWID)|R5"
                             + ";+chkAnonseArtikkel|logical|*/|chkAnonseArtikkel(ROWID)|R6"
                             + ";+chkKontroll|logical|*/|chkKontroll(ROWID)|R7"
                             + ";+KatalogPris#1|decimal|>>>><>>9.99|KatalogPris#1|Engros"
                             + ";+forhRab%#1|decimal|>>>><>>9.99|forhRab%#1|Rab.forh"
                             + ";+VarekostForh|decimal|>>>><>>9.99|VarekostForh|Nto.Innpr forh"
                             + ";+suppRab%#1|decimal|>>>><>>9.99|suppRab%#1|Rab.sup"
                             + ";+VarekostSup|decimal|>>>><>>9.99|VarekostSup|Nto.Innpr sup"
                             + ";AnbefaltPris|Markedspris"
                             + ";KjedeValutaPris|Innkj.pris valuta"
                             + ";KjedeProdusent|Kjedens produsent"
                             /* ----------- Felt som ikke vises ---------------- */
                             + ";!behStatus"
                             + ";!Gjennomfaktureres;!AntIPkn;!StrKode2;!StrKode1;!GarantiKl;!BestForslag;!Pant;!IndividType;!ArtSlag;!ManRabIKas"
                             + ";!Mengde;!LinkVareNr;!VPIBildeKode;!VPIDato;!LevDato4;!LevDato3;!Linjemerknad;!KjedeVare;!HovedModellFarge"
                             + ";!PrisGrpNr;!SentralBestilling;!ModellFarge;!LokArtikkelNr;!Oppdatert;!SalgsEnhet;!Etikett;!HandKode;!Pakkenr"
                             + ";!BehKode;!KjentPaHK;!HKVareId;!IKasse;!LokPris;!HkStyrt;!Alder;!Pakke;!BildeIKasse;!OLLager;!OPris;!SattPaKampanje"
                             + ";!VisDivInfo;!DivInfo;!LevDato2;!LevDato1;!Storrelser;!RegistrertAv;!RegistrertTid;!RegistrertDato;!BrukerID;!ETid"
                             + ";!EDato;!ProdNr;!StrTypeID;!VgKat;!AnonseArtikkel;!BongTekst;!Notat;!VMId;!Lager;!ValKod;!ProvKod;!RabKod;!Anv-Id"
                             + ";!Slit-Id;!Inner-Id;!Foder-Id;!Last-Id;!Ov-Id;!Kommentar;!Tilv-Land;!BildNr;!MatKod;!Klack;!Farg;!LopNr;!Vg;!Hg"
                             + ";!EkstVPILevNr;!LevVareTekst;!KundeRabatt;!VareNr;!Katalogpris;!Utvidetsok"
                             /* ----------------------------------------------- */
                             + ",VPIArtPris;!Profilnr"
                             + ";!+InnkjopsPris#1|decimal|>>>><>>9.99|Innkjopspris%#1|Innkj.pris"
                             + ";!+Varekost#1|decimal|>>>><>>9.99|Varekost%#1|Varekost"
                             + ",SaSong;SaSong;SasBeskr"
                             + ",LevBas;LevNr;LevNamn"
                             + ",sysPara;Parameter1|Status"
                             ," WHERE TRUE "
                             + ", FIRST VPIArtPris OUTER-JOIN WHERE VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr " 
                                                            + " AND VPIArtPris.VareNr       = VPIArtBas.VareNr "
                                                            + " AND VPIArtPris.ProfilNr     = 1 NO-LOCK "
                             + ", FIRST SaSong OUTER-JOIN OF VPIArtBas NO-LOCK "
                             + ", FIRST LevBas OUTER-JOIN OF VPIArtBas NO-LOCK "
                             + ", FIRST sysPara OUTER-JOIN WHERE sysPara.sysHid = 21 AND sysPara.sysGr = 200 AND sysPara.ParaNr = VPIArtBas.behStatus NO-LOCK"
                             ,"").
  DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,"chkArtReg,chkVarebok,chkPris,chkArtInfo,chkStrek,chkAnonseArtikkel,chkKontroll").

  DYNAMIC-FUNCTION('setAttribute',hBrowse,'QuerySort','ArtikkelNr').
  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','vpiartbas_varebok_brwcalc.p').
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  ASSIGN 
    behStatus:DELIMITER = "|"
    behStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 21 and sysGr = 200")
  . 
  ASSIGN 
    cbcKontroll:DELIMITER = "|"
    cbcKontroll:LIST-ITEM-PAIRS = "||Feilkoblet EAN/str kode|Rule6_EAN|Feilkoblet EAN kode|Rule5_EAN|Ingen EAN kode|Rule1_EAN|Likt navn/ulikt modellnummer|Rule2_LevKod|Likt modellnummer/ulik tekst|Rule3_Beskr|Ingen bilde|Rule4_bilderef" 
  . 

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "excel;Eksporter til E&xcel,BrowseConfig,Edit;&Endre,delete;Slette,Print;S&kriv,rule,sendVarebok;Send til &varebok,sendButikk;Send til b&utikk,ArtikkelKort;&Artikkel kort,clearFilter;Blank &filter¤ENABLE"
                    + ",RowsToBatch;Antall &rader i resultatsett¤enable"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowse,  /* parent widget */
                  "setStatus;Endre status,setRegnskapsAvdeling;Endre vareområde"
                 ,"").   
  LoadRowsToBatchSettings().
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  

/*   RUN InvokeMethod (hBrowse,'OpenQuery'). */

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
DEF VAR cWhere   AS CHAR NO-UNDO.
DEF VAR cVPIDato AS CHAR NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        cVPIDato = '¤' + 
                   (IF INPUT FraVPIDato <> ? THEN STRING(INPUT FraVPIDato) ELSE '') + '¤' +
                   (IF INPUT TilVPIDato <> ? THEN STRING(INPUT TilVPIDato) ELSE '')
        .

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtReg",tbAvvikArtBas:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkVarebok",STRING(VarebokNr) + "¤" + tbAvvikVarebok:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkPris",STRING(VarebokNr) + "¤" + tbAvvikPris:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkArtInfo",STRING(VarebokNr) + "¤" + tbAvvikArtInfo:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkStrek",tbAvvikStrekKode:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkAnonseArtikkel",tbAnonseArtikkel:SCREEN-VALUE).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcparamChkKontroll",IF cbcKontroll:SCREEN-VALUE = ? 
                                                                     THEN '' 
                                                                     ELSE (cbcKontroll:SCREEN-VALUE + cVPIDato) ).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
    cWhere = buildFilter(cWhere,varetekst:HANDLE,'beskr','BEGINS').      
    cWhere = cWhere + buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraVPIdato:HANDLE,'VPIdato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilVPIdato:HANDLE,'VPIdato','LE').
    cWhere = cWhere + buildFilter(cWhere,SaSong:HANDLE,'SaSong','EQ').
    cWhere = cWhere + buildFilter(cWhere,LevNr:HANDLE,'LevNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,behStatus:HANDLE,'behStatus','EQ').
    cWhere = cWhere + buildFilter(cWhere,Utvidetsok:HANDLE,'Utvidetsok','CONTAINS').
/*     cWhere = cWhere + buildFilter(cWhere,tbAnonseArtikkel:HANDLE,'AnonseArtikkel','EQ'). */

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

    ASSIGN
      fraVPIDato:MODIFIED      = FALSE 
      tilVPIDato:MODIFIED      = FALSE  
      Levkod:MODIFIED          = FALSE  
      SaSong:MODIFIED          = FALSE    
      LevNr:MODIFIED           = FALSE    
      VareTekst:MODIFIED       = FALSE .

    hParentBrowse = DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"parent","from").
    IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND
       Strekkode:SCREEN-VALUE NE "" THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter",
                       "VPIstrekkode WHERE VPIStrekkode.EkstVPILevNr = " 
                       + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE) 
                       + " and VPIstrekkode.Kode = '" + Strekkode:SCREEN-VALUE + "'"
                       + ",FIRST VPIartBas OF VPIstrekkode NO-LOCK").
    ELSE
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryFilter","").


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
  IF vareboknr GT 0 THEN
  DO:
    RUN vpiartbas_varebokdetalj.w PERSIST SET hChild.
  END.   
  ELSE
  DO:
    MESSAGE 'Varebok må velges før en kan se på avviks detaljene'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVarebokRecord C-Win 
PROCEDURE sendVarebokRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  IF iReturn = 1 THEN 
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowseLinje,"varebehlinje_justermange.p",icField + "|" + icValue).
ELSE
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowseLinje,"varebehlinje_justermange.p",icField + "|" + icValue).

IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i endring av " + icField,""). 
ELSE IF cRowIdList NE "" THEN DO:
  DYNAMIC-FUNCTION("refreshRowids",hBrowseLinje,REPLACE(cRowIdList,"|",",")).
  APPLY "value-changed" TO hBrowseLinje.
END.
ELSE RUN OpenQuery.

RETURN bOk. 

------------------------------------------------------------------------------*/
DEF VAR opcFieldList AS CHAR NO-UNDO.
DEF VAR opcLabelList AS CHAR NO-UNDO.
DEF VAR opiReturn    AS INT  NO-UNDO.

DEF VAR iReturn    AS INT  NO-UNDO.
DEF VAR cVareNr    AS CHAR NO-UNDO.
DEF VAR cRowIdList AS CHAR NO-UNDO.

RUN velgFelterArtBas_Vareboklinje.w (OUTPUT opcFieldList, OUTPUT opcLabelList, OUTPUT opiReturn).
IF opiReturn = 0 THEN
    RETURN NO-APPLY.

IF opcFieldList = '' THEN
DO:
  MESSAGE 'Ingen felter valgt, avbryter overføring...' 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  UNDO, LEAVE.

END.
IF varebok:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
DO:
  MESSAGE "Varebok må være valgt for å kunne benytte denne funksjon."
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
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vareboklinje_new_update.p"
                        ,STRING(Vareboknr)
                       + ";" + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE)
                       + ";" + opcFieldList 
                          ).
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vareboklinje_new_update.p"
                        ,STRING(Vareboknr)
                       + ";" + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("EkstVPILevNr"):BUFFER-VALUE)
                       + ";" + opcFieldList
                         ).
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i endring av linformasjon ",""). 
ELSE DO:
      IF valid-handle(hChild) THEN
          RUN openVisAvvik.
  END.

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
DEF VAR cList       AS CHAR NO-UNDO.
DEF VAR cRowIdList  AS CHAR NO-UNDO.

ASSIGN 
  cList = DYNAMIC-FUNCTION("getFieldList","Regnskapsavdeling;RAvdNr|RAvdBeskrivelse;RAvdNr","WHERE TRUE")
  cList = REPLACE(cList,'|',',')
.

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
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i endring av linformasjon ",""). 

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
DEF VAR cRowIdList  AS CHAR NO-UNDO.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ihBrowse:MOVE-COLUMN(25,1).
  ihBrowse:MOVE-COLUMN(24,20).
  ihBrowse:MOVE-COLUMN(25,21).

  ASSIGN
    ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS  = 80
    ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS  = 50
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
    
    hbcChkArtReg   = ihBrowse:GET-BROWSE-COLUMN(7)
    hbfChkArtReg   = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtReg')
    hbcChkVarebok  = ihBrowse:GET-BROWSE-COLUMN(8)
    hbfChkVarebok  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkVarebok')
    hbcChkPris     = ihBrowse:GET-BROWSE-COLUMN(9)
    hbfChkPris     = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkPris')
    hbcChkArtInfo  = ihBrowse:GET-BROWSE-COLUMN(10)
    hbfChkArtInfo  = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkArtInfo')
    hbcChkStrek    = ihBrowse:GET-BROWSE-COLUMN(11)
    hbfChkStrek    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkStrek')
    hbcChkAnonseArtikkel    = ihBrowse:GET-BROWSE-COLUMN(12)
    hbfChkAnonseArtikkel    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkAnonseArtikkel')
    hbcChkKontroll    = ihBrowse:GET-BROWSE-COLUMN(13)
    hbfChkKontroll    = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ChkKontroll')
    
    hbcChkArtReg:COLUMN-FONT         = 8
    hbcChkVarebok:COLUMN-FONT        = 8
    hbcChkPris:COLUMN-FONT           = 8
    hbcChkArtInfo:COLUMN-FONT        = 8
    hbcChkStrek:COLUMN-FONT          = 8
    hbcChkAnonseArtikkel:COLUMN-FONT = 8
    hbcChkKontroll:COLUMN-FONT       = 8
  .

  RETURN TRUE.
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
FUNCTION getVarebokNr RETURNS DECIMAL
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

