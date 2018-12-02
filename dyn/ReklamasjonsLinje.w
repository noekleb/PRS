&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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

DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hParentBrowse   AS HANDLE NO-UNDO.
DEF VAR hParent         AS HANDLE NO-UNDO.
DEF VAR hChild          AS HANDLE NO-UNDO.
DEF VAR hHurtigRegister AS HANDLE NO-UNDO.
DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.
DEF VAR cRowIdList       AS CHAR NO-UNDO.
DEF VAR lKostnad        AS LOG  NO-UNDO.

{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ReklamasjonsLinje

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ReklamasjonsLinje SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ReklamasjonsLinje SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ReklamasjonsLinje
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ReklamasjonsLinje


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TG-Kost Butik ReklamStatus ReklamasjonsNr ~
fraFrist-dato tilFrist-dato fraRegistrertDato tilRegistrertDato ~
fraFakturertDato tilFakturertDato fraOppdatertDato tilOppdatertDato LevNr ~
btnCalTilFakturertDato Kundenr Kundenavn cbOppdLager cbFakturert cbTTId ~
Strekkode btnCalTilOppdatertDato LevKod Varetekst LevFargKod Artikkelnr ~
Storl btnCalFraFaktuturertDato bBlank bStartSok btnCalFraOppdatertDato ~
btnCalFraDato-3 btnCalFraDato-4 btnLevNr btnCalFraDato btnCalFraDato-2 ~
btnKundeNr btnKundeNr-2 rectToolbar rectBrowse rect-1 RECT-2 rectWinToolbar 
&Scoped-Define DISPLAYED-OBJECTS TG-Kost Butik ReklamStatus ReklamasjonsNr ~
fraFrist-dato tilFrist-dato fraRegistrertDato tilRegistrertDato ~
fraFakturertDato tilFakturertDato fraOppdatertDato tilOppdatertDato LevNr ~
Kundenr Kundenavn cbOppdLager cbFakturert cbTTId Strekkode LevKod Varetekst ~
LevFargKod Artikkelnr Storl lblLevNamn lblKundeNavn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD adjustBrowseColumns C-Win 
FUNCTION adjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Kostnadmodus C-Win 
FUNCTION Kostnadmodus RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bBlank 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bStartSok 
     LABEL "Start søk" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-4 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraFaktuturertDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraOppdatertDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilFakturertDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalTilOppdatertDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnKundeNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnKundeNr-2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cbFakturert AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Fakturert" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cbOppdLager AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Oppd.lager" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cbTTId AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Reklam.type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE ReklamStatus AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE Artikkelnr AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Artikkelnr" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE Butik AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fraFakturertDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fakturert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fraFrist-dato AS DATE FORMAT "99/99/99":U 
     LABEL "Frist" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fraOppdatertDato AS DATE FORMAT "99/99/99":U 
     LABEL "Oppdatert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fraRegistrertDato AS DATE FORMAT "99/99/99":U 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Kundenavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kunde navn" 
     VIEW-AS FILL-IN 
     SIZE 35.6 BY 1 NO-UNDO.

DEFINE VARIABLE Kundenr AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lblKundeNavn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE lblLevNamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.Fargekode" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.ArtNr" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE ReklamasjonsNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Rek.nr" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE Storl AS CHARACTER FORMAT "x(10)" 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 35.8 BY 1 NO-UNDO.

DEFINE VARIABLE tilFakturertDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tilFrist-dato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tilOppdatertDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tilRegistrertDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Varetekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vare tekst" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 1.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185 BY 7.38.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185 BY 20.95.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE TG-Kost AS LOGICAL INITIAL no 
     LABEL "Utan kostnader" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ReklamasjonsLinje SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TG-Kost AT ROW 4.81 COL 112
     Butik AT ROW 6.48 COL 116 COLON-ALIGNED
     ReklamStatus AT ROW 2.67 COL 13 COLON-ALIGNED
     ReklamasjonsNr AT ROW 3.71 COL 13 COLON-ALIGNED
     fraFrist-dato AT ROW 4.71 COL 13 COLON-ALIGNED
     tilFrist-dato AT ROW 4.76 COL 31.2 COLON-ALIGNED NO-LABEL
     fraRegistrertDato AT ROW 5.76 COL 13 COLON-ALIGNED
     tilRegistrertDato AT ROW 5.76 COL 31.2 COLON-ALIGNED NO-LABEL
     fraFakturertDato AT ROW 6.81 COL 13 COLON-ALIGNED
     tilFakturertDato AT ROW 6.81 COL 31.2 COLON-ALIGNED NO-LABEL
     fraOppdatertDato AT ROW 7.86 COL 13 COLON-ALIGNED
     tilOppdatertDato AT ROW 7.86 COL 31.2 COLON-ALIGNED NO-LABEL
     LevNr AT ROW 2.67 COL 62.4 COLON-ALIGNED
     btnCalTilFakturertDato AT ROW 6.81 COL 47
     Kundenr AT ROW 3.71 COL 62.4 COLON-ALIGNED
     Kundenavn AT ROW 4.67 COL 62.4 COLON-ALIGNED
     cbOppdLager AT ROW 5.67 COL 62.4 COLON-ALIGNED
     cbFakturert AT ROW 6.71 COL 62.4 COLON-ALIGNED
     cbTTId AT ROW 7.71 COL 62.4 COLON-ALIGNED
     Strekkode AT ROW 8.24 COL 121.2 COLON-ALIGNED
     btnCalTilOppdatertDato AT ROW 7.86 COL 47
     LevKod AT ROW 2.67 COL 148.6 COLON-ALIGNED
     Varetekst AT ROW 3.62 COL 148.6 COLON-ALIGNED
     LevFargKod AT ROW 4.57 COL 148.6 COLON-ALIGNED
     Artikkelnr AT ROW 5.62 COL 148.6 COLON-ALIGNED
     Storl AT ROW 6.62 COL 148.6 COLON-ALIGNED HELP
          "Størrelse"
     btnCalFraFaktuturertDato AT ROW 6.81 COL 29.2
     bBlank AT ROW 7.14 COL 170.2
     bStartSok AT ROW 8.29 COL 170.2
     btnCalFraOppdatertDato AT ROW 7.86 COL 29.2
     btnCalFraDato-3 AT ROW 4.76 COL 47.2
     btnCalFraDato-4 AT ROW 5.76 COL 47
     btnLevNr AT ROW 2.67 COL 79.4 NO-TAB-STOP 
     btnCalFraDato AT ROW 4.71 COL 29
     btnCalFraDato-2 AT ROW 5.76 COL 29.2
     btnKundeNr AT ROW 3.67 COL 79.4 NO-TAB-STOP 
     lblLevNamn AT ROW 2.67 COL 82.2 COLON-ALIGNED NO-LABEL
     lblKundeNavn AT ROW 3.67 COL 82 COLON-ALIGNED NO-LABEL
     btnKundeNr-2 AT ROW 6.48 COL 132 NO-TAB-STOP 
     rectToolbar AT ROW 1.24 COL 1.6
     rectBrowse AT ROW 10.05 COL 1
     rect-1 AT ROW 7.91 COL 111
     RECT-2 AT ROW 2.43 COL 1
     rectWinToolbar AT ROW 1.24 COL 176.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 185.2 BY 30.


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
         TITLE              = "Reklamasjons søk"
         HEIGHT             = 30
         WIDTH              = 185.2
         MAX-HEIGHT         = 51.43
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 51.43
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/app16.ico":U) THEN
    MESSAGE "Unable to load icon: ico/app16.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
/* SETTINGS FOR FILL-IN lblKundeNavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblKundeNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblLevNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblLevNamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "skotex.ReklamasjonsLinje"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Reklamasjons søk */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Reklamasjons søk */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Artikkelnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Artikkelnr C-Win
ON LEAVE OF Artikkelnr IN FRAME DEFAULT-FRAME /* Artikkelnr */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bBlank C-Win
ON CHOOSE OF bBlank IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
  RUN BlankFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStartSok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStartSok C-Win
ON CHOOSE OF bStartSok IN FRAME DEFAULT-FRAME /* Start søk */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF frafrist-dato
DO:
  RUN Cal.w (frafrist-dato:HANDLE).
/*   IF frist-dato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-2 C-Win
ON CHOOSE OF btnCalFraDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraRegistrertDato
DO:
  RUN Cal.w (fraRegistrertDato:HANDLE).
/*   IF RegistrertDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-3 C-Win
ON CHOOSE OF btnCalFraDato-3 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilfrist-dato
DO:
  RUN Cal.w (tilfrist-dato:HANDLE).
/*   IF frist-dato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-4 C-Win
ON CHOOSE OF btnCalFraDato-4 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilRegistrertDato
DO:
  RUN Cal.w (tilRegistrertDato:HANDLE).
/*   IF RegistrertDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraFaktuturertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraFaktuturertDato C-Win
ON CHOOSE OF btnCalFraFaktuturertDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraFakturertDato
DO:
  RUN Cal.w (fraFakturertDato:HANDLE).
/*   IF RegistrertDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraOppdatertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraOppdatertDato C-Win
ON CHOOSE OF btnCalFraOppdatertDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF fraOppdatertDato
DO:
  RUN Cal.w (fraOppdatertDato:HANDLE).
/*   IF RegistrertDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilFakturertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilFakturertDato C-Win
ON CHOOSE OF btnCalTilFakturertDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilFakturertDato
DO:
  RUN Cal.w (tilFakturertDato:HANDLE).
/*   IF RegistrertDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalTilOppdatertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalTilOppdatertDato C-Win
ON CHOOSE OF btnCalTilOppdatertDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF tilOppdatertDato
DO:
  RUN Cal.w (tilOppdatertDato:HANDLE).
/*   IF RegistrertDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundeNr C-Win
ON CHOOSE OF btnKundeNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF kundenr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KundeNr;Navn".

  RUN JBoxDLookup.w ("Kunde;Kundenr;Navn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      kundenr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
    IF kundenr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO kundenr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundeNr-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundeNr-2 C-Win
ON CHOOSE OF btnKundeNr-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF kundenr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Butik;ButNamn".

  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      Butik:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
    .
    IF Butik:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN StartQuery.
  END.
  ELSE APPLY "entry" TO Butik.

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
    IF LevNr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').   
  END.
  ELSE APPLY "entry" TO LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butik C-Win
ON LEAVE OF Butik IN FRAME DEFAULT-FRAME /* Butikk */
DO:
   IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFakturert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFakturert C-Win
ON VALUE-CHANGED OF cbFakturert IN FRAME DEFAULT-FRAME /* Fakturert */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbOppdLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbOppdLager C-Win
ON VALUE-CHANGED OF cbOppdLager IN FRAME DEFAULT-FRAME /* Oppd.lager */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTTId C-Win
ON VALUE-CHANGED OF cbTTId IN FRAME DEFAULT-FRAME /* Reklam.type */
DO:
    IF SELF:SCREEN-VALUE = "0" THEN DO:
        TG-Kost:SENSITIVE = TRUE.
    END.
    ELSE DO:
        TG-Kost:CHECKED = FALSE.
        TG-Kost:SENSITIVE = FALSE.
    END.
   IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraFakturertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraFakturertDato C-Win
ON LEAVE OF fraFakturertDato IN FRAME DEFAULT-FRAME /* Fakturert */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraFrist-dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraFrist-dato C-Win
ON LEAVE OF fraFrist-dato IN FRAME DEFAULT-FRAME /* Frist */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraOppdatertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraOppdatertDato C-Win
ON LEAVE OF fraOppdatertDato IN FRAME DEFAULT-FRAME /* Oppdatert */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fraRegistrertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fraRegistrertDato C-Win
ON LEAVE OF fraRegistrertDato IN FRAME DEFAULT-FRAME /* Registrert */
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kundenavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kundenavn C-Win
ON LEAVE OF Kundenavn IN FRAME DEFAULT-FRAME /* Kunde navn */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Kundenr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Kundenr C-Win
ON LEAVE OF Kundenr IN FRAME DEFAULT-FRAME /* Kundenr */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevFargKod C-Win
ON LEAVE OF LevFargKod IN FRAME DEFAULT-FRAME /* Lev.Fargekode */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevKod C-Win
ON LEAVE OF LevKod IN FRAME DEFAULT-FRAME /* Lev.ArtNr */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevNr C-Win
ON LEAVE OF LevNr IN FRAME DEFAULT-FRAME /* Lev.nr */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ReklamasjonsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ReklamasjonsNr C-Win
ON LEAVE OF ReklamasjonsNr IN FRAME DEFAULT-FRAME /* Rek.nr */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ReklamStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ReklamStatus C-Win
ON VALUE-CHANGED OF ReklamStatus IN FRAME DEFAULT-FRAME /* Status */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Storl C-Win
ON LEAVE OF Storl IN FRAME DEFAULT-FRAME /* Størrelse */
DO:
  IF SELF:MODIFIED THEN DO:
      DYNAMIC-FUNCTION("runproc","bibl_fixstorl.p",storl:SCREEN-VALUE,?).
      SELF:SCREEN-VALUE = caps(DYNAMIC-FUNCTION("getTransactionMessage")).
      RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Strekkode C-Win
ON LEAVE OF Strekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
    DEF VAR iArtNr       AS DEC NO-UNDO.
    DEF VAR cLookupValue AS CHAR NO-UNDO.
    DEF VAR iStrKode     AS INT NO-UNDO.
    DEF VAR cStorl       AS CHAR NO-UNDO.

  IF SELF:MODIFIED THEN
  DO:
      iArtNr   = DYNAMIC-FUNCTION('getFieldValues','Strekkode','WHERE StrekKode.Kode = ' + QUOTER(strekkode:SCREEN-VALUE),'ArtikkelNr').
      iStrKode = DYNAMIC-FUNCTION('getFieldValues','Strekkode','WHERE StrekKode.Kode = ' + QUOTER(strekkode:SCREEN-VALUE),'StrKode').
      cStorl   = DYNAMIC-FUNCTION('getFieldValues','StrKonv','WHERE StrKonv.StrKode = ' + QUOTER(string(iStrKode)),'Storl').

      IF iArtNr NE 0 AND iArtNr NE ? THEN 
      DO:
        ASSIGN 
            Artikkelnr:SCREEN-VALUE = string(iArtNr)
            Storl:SCREEN-VALUE      = cStorl
            Strekkode:SCREEN-VALUE  = ''.
        IF strekkode:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN StartQuery.   
      END.
      ELSE DO:
        MESSAGE 'Kunne ikke finne artikkel med angitt strekkode, vennligst oppgi riktig strekkode og prøv igjen.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

        APPLY "entry" TO Strekkode.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Kost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Kost C-Win
ON VALUE-CHANGED OF TG-Kost IN FRAME DEFAULT-FRAME /* Utan kostnader */
DO:
  IF cbTTId:SCREEN-VALUE = '62' THEN
      RETURN NO-APPLY.
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilFakturertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilFakturertDato C-Win
ON LEAVE OF tilFakturertDato IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilFrist-dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilFrist-dato C-Win
ON LEAVE OF tilFrist-dato IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilOppdatertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilOppdatertDato C-Win
ON LEAVE OF tilOppdatertDato IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tilRegistrertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tilRegistrertDato C-Win
ON LEAVE OF tilRegistrertDato IN FRAME DEFAULT-FRAME
DO:
/*   IF SELF:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
    IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Varetekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Varetekst C-Win
ON LEAVE OF Varetekst IN FRAME DEFAULT-FRAME /* Vare tekst */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
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
  IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hHurtigRegister) THEN DELETE OBJECT hHurtigRegister.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
FIND bruker WHERE Bruker.BrukerID = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
    cSprak = Bruker.Lng.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankFilter C-Win 
PROCEDURE BlankFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          ReklamStatus:SCREEN-VALUE      = '0'
          cbOppdLager:SCREEN-VALUE       = '0'
          cbFakturert:SCREEN-VALUE       = '0'
          cbTTId:SCREEN-VALUE            = '0'
          ReklamasjonsNr:SCREEN-VALUE    = ''
          fraFrist-dato:SCREEN-VALUE     = ?
          TilFrist-dato:SCREEN-VALUE     = ?
          fraRegistrertdato:SCREEN-VALUE = ?
          TilRegistrertdato:SCREEN-VALUE = ?
          fraFakturertdato:SCREEN-VALUE  = ?
          TilFakturertdato:SCREEN-VALUE  = ?
          fraOppdatertdato:SCREEN-VALUE  = ?
          TilOppdatertdato:SCREEN-VALUE  = ?
          LevNr:SCREEN-VALUE             = ''
          KundeNr:SCREEN-VALUE           = ''
          KundeNavn:SCREEN-VALUE         = ''          
          LevKod:SCREEN-VALUE            = ''
          Varetekst:SCREEN-VALUE         = ''
          LevFargKod:SCREEN-VALUE        = ''
          ArtikkelNr:SCREEN-VALUE        = ''
          Storl:SCREEN-VALUE             = ''
          Strekkode:SCREEN-VALUE         = ''
          .
      RUN StartQuery.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")). */
/*   DYNAMIC-FUNCTION("setCurrentObject",hBrowse).                                                                             */
/*   RUN OpenQuery.                                                                                                            */

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
  DEF VAR hChildQuery AS HANDLE NO-UNDO.

  IF NOT VALID-HANDLE(hChild) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN Reklamasjonsbehandling.w PERSIST SET hChild.
    RUN InitializeObject IN hChild.
    hChildQuery = DYNAMIC-FUNCTION('getQuery' IN hChild).
    DYNAMIC-FUNCTION("setAttribute",hChildQuery,'QueryFilter','').
    DYNAMIC-FUNCTION("CreateOneToOneLink",hChildQuery,hBrowse,"Reklamasjonsnr"). 
    APPLY 'value-changed' TO hBrowse.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN DefaultActionBrowse.
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY TG-Kost Butik ReklamStatus ReklamasjonsNr fraFrist-dato tilFrist-dato 
          fraRegistrertDato tilRegistrertDato fraFakturertDato tilFakturertDato 
          fraOppdatertDato tilOppdatertDato LevNr Kundenr Kundenavn cbOppdLager 
          cbFakturert cbTTId Strekkode LevKod Varetekst LevFargKod Artikkelnr 
          Storl lblLevNamn lblKundeNavn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TG-Kost Butik ReklamStatus ReklamasjonsNr fraFrist-dato tilFrist-dato 
         fraRegistrertDato tilRegistrertDato fraFakturertDato tilFakturertDato 
         fraOppdatertDato tilOppdatertDato LevNr btnCalTilFakturertDato Kundenr 
         Kundenavn cbOppdLager cbFakturert cbTTId Strekkode 
         btnCalTilOppdatertDato LevKod Varetekst LevFargKod Artikkelnr Storl 
         btnCalFraFaktuturertDato bBlank bStartSok btnCalFraOppdatertDato 
         btnCalFraDato-3 btnCalFraDato-4 btnLevNr btnCalFraDato btnCalFraDato-2 
         btnKundeNr btnKundeNr-2 rectToolbar rectBrowse rect-1 RECT-2 
         rectWinToolbar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FakturerRecord C-Win 
PROCEDURE FakturerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cFilNavn    AS CHAR NO-UNDO.

ASSIGN 
  cStatusList = DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 1")
  cStatusList = replace(cStatusList,'|',',').
.

RUN JBoxBrowseMsgUpdateVal.w (IF cSprak = "SE" THEN "Utskrift av reklamationer og markera dem som fakturerade" ELSE "Utskrift reklamasjoner og marker dem som fakturert",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999
                              ,'integer|>>9|Status|COMBO-BOX|,0,' + cStatusList
                              ,OUTPUT ocValue
                              ,OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 1 THEN DO:
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"reklamasjonslogg_reklamasjonsliste.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE) + ';' + ocValue).
  IF bOK THEN DO:
      cFilNavn = DYNAMIC-FUNCTION("getTransactionMessage").
      IF SEARCH(cFilNavn) <> ? THEN DO:
          RUN visPDF.w (cFilNavn,"REKLAMASJON").
          OS-DELETE VALUE(cFilNavn).
      END.
  END.
  MESSAGE "Skal reklamasjonslinjene markeres som fakturert?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk THEN DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"reklamasjonslinje_skriv_og_fakturer.p",'').
END.
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"reklamasjonslogg_reklamasjonsliste.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE) + ';' + ocValue).
  IF bOK THEN DO:
      cFilNavn = DYNAMIC-FUNCTION("getTransactionMessage").
      IF SEARCH(cFilNavn) <> ? THEN DO:
          RUN visPDF.w (cFilNavn,"REKLAMASJON").
          OS-DELETE VALUE(cFilNavn).
      END.
  END.
  MESSAGE IF cSprak = "SE" THEN "Skall reklamationslinjerna markeras som fakturerade?" ELSE "Skal reklamasjonslinjene markeres som fakturert?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk THEN DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"reklamasjonslinje_skriv_og_fakturer.p",'').
END.
ELSE
  LEAVE.

/*                                                                                                                                                                                             */
/* IF NOT bOK THEN                                                                                                                                                                             */
/*   DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),IF cSprak = "SE" THEN "Fel i utskrift av reklamationslista " ELSE "Feil i utskrift av reklamasjonsliste ",""). */

/* IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HurtigRecord C-Win 
PROCEDURE HurtigRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT valid-handle(hHurtigRegister) THEN 
  DO:
    RUN reklamasjonslogg_hurtig.w PERSISTENT SET hHurtigRegister.
    RUN initializeObject IN hHurtigRegister.
    SUBSCRIBE TO 'refreshByRowid' IN hHurtigRegister.

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
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "Reklamasjonslinje"  
                             + ";ReklamasjonsNr|Rekl.nr@3"
                              + ";!TTId|Rekltype"
                              + ";!LinjeNr"
                              + ";Butik|But.nr@2"
                              + ";FeilKode"
                              + ";ArtikkelNr|Art.nr"
                              + ";Vg"
                              + ";LopNr|Løpnr|>>>>>9"
                              + ";LevKod|Vev.best.nr"
                              + ";Varetekst"
                              + ";LevFargKod|Lev.fargekode"
                              + ";Storl|Str"
                              + ";Antall"
                              + ";VVArekost"
                              + ";Pris"
                              + ";RabKr"
                              + ";ReklamUtgifter|Rekl.utgifter"
                              + ";ReklamVerdi|Sum reklam"                              
                              + ";ReklamTotal|Sum kostnad"                                                            
                              + ";+cReklamType|CHARACTER|x(15)|reklamasjonslinje_reklamtype.p|Rekl.type@4"
                             + ",Reklamasjonslogg"
                              + ";!ReklamasjonsNr"
                              + ";KundeNr|Kunde@10"
                              + ";KundeNavn|Navn@11"
                              + ";KundeTelefon|Telefon@12"
                              + ";KundeMobil|Mobil@13"
                              + ";KundeAdresse@14"
                              + ";RegistrertDato|Registrert|99/99/99@6"
                              + ";Frist-Dato|Frist@7"
                              + ";!OppdLager|Oppd"
                              + ";OppdLagerDato|Oppdat.lager@8"
                              + ";!OppdLager|Oppd.av"
                              + ";!Fakturert|Fakt"
                              + ";FakturertDato|Fakturert@9"
                              + ";!FakturertAv|Fakt.av"
                              + ";ReklamUtgifter|Tot utgifter"
                              + ";ReklamVerdi|Tot reklam"
                              + ";ReklamTotal|Tot kostnad"
                              + ";+cReklamStatus|CHARACTER|x(15)|reklamasjonslogg_reklamstatus.p|Status@1"
                             + ",ArtBas;!ArtikkelNr"
                              + ";LevNr|Lev.nr@5"
                             ,"WHERE FALSE"
                             + ", FIRST ReklamasjonsLogg OF ReklamasjonsLinje NO-LOCK OUTER-JOIN"
                             + ", FIRST ArtBas WHERE ArtBas.ArtikkelNr = ReklamasjonsLinje.ArtikkelNr NO-LOCK OUTER-JOIN" 
                             ,"SORT|Reklamasjonsnr;DESC").
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryReklamasjonsLogg",'EACH ReklamasjonsLinje OF Reklamasjonslogg NO-LOCK').
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryArtBas",'EACH Reklamasjonslinje NO-LOCK WHERE ReklamasjonsLinje.ArtikkelNr = ArtBas.ArtikkelNr').
  
  ASSIGN 
      ReklamStatus:DELIMITER = "|"
      ReklamStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 1")
      cbOppdLager:DELIMITER = "|"
      cbOppdLager:LIST-ITEM-PAIRS = "|0|" +  DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 10")
      cbFakturert:DELIMITER = "|"
      cbFakturert:LIST-ITEM-PAIRS = "|0|" +  DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 11")
      cbTTId:DELIMITER = "|"
      cbTTId:LIST-ITEM-PAIRS = "|0|" +  DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 12")
      . 
   
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                              rectToolbar:HANDLE,
                              "File",
                              "Edit,BrowseConfig,Print,Rule,Excel,Hurtig;Hurtig registrer¤ENABLED,Kostnad;Hurtig kostnad¤ENABLED,OppdLager;Oppdater lager,Fakturer;Skriv ut og fakturer"
                             ,"maxborder").
  DYNAMIC-FUNCTION("NewToolBar",
                  rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                  "Fil,",                         /* Corresponding menu label - no menu if blank */
                  "close",
                  "right,enable").                      /* Misc - enable, maxborder.. */

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  RUN invokemethod(hBrowse,'openquery'). 
END.

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).


DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rect-1").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar,rect-1").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,800,200,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KostnadRecord C-Win 
PROCEDURE KostnadRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT VALID-HANDLE(hHurtigRegister) THEN 
  DO:
      IF NOT CAN-DO(REPLACE(cbTTId:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME},"|",","),"62") THEN DO:
          MESSAGE "Registertypen inte definierad"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
      lKostnad = TRUE.
    RUN reklamasjonslogg_hurtig.w PERSISTENT SET hHurtigRegister.
    RUN initializeObject IN hHurtigRegister.
    SUBSCRIBE TO 'refreshByRowid' IN hHurtigRegister.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdLagerRecord C-Win 
PROCEDURE OppdLagerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR iReturn    AS INT    NO-UNDO.

RUN JBoxBrowseMsgUpdateVal.w (IF cSprak = "SE" THEN "Uppdatera lager og statistik" ELSE "Oppdatere lager og statistikker",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              "",
                              OUTPUT ocValue, 
                              OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"reklamasjonslinje_oppdater_lager_og_statistikk.p",'').
ELSE IF iReturn = 2 THEN
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"reklamasjonslinje_oppdater_lager_og_statistikk.p",'').
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),IF cSprak = "SE" THEN "Fel vid uppdatering av lager og statistik" ELSE "Feil ved oppdatering av lager og statistikker",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).

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

MESSAGE "NexPrev"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.
DEF VAR cFilNavn    AS CHAR NO-UNDO.

ASSIGN 
  cStatusList = DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 1")
  cStatusList = replace(cStatusList,'|',',').
.

RUN JBoxBrowseMsgUpdateVal.w (IF cSprak = "SE" THEN "Utskrift av reklamationslista" ELSE "Utskrift av reklamasjonslista",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999
                              ,'integer|>>9|Status|COMBO-BOX|,0,' + cStatusList
                              ,OUTPUT ocValue
                              ,OUTPUT iReturn).  /*1=Alle,2=Valgte*/
IF iReturn = 1 THEN DO:
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"reklamasjonslogg_reklamasjonsliste.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE) + ';' + ocValue).
END.
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"reklamasjonslogg_reklamasjonsliste.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE) + ';' + ocValue).
END.
ELSE
  LEAVE.

IF bOK THEN DO:
    cFilNavn = DYNAMIC-FUNCTION("getTransactionMessage").
    IF SEARCH(cFilNavn) <> ? THEN DO:
        RUN visPDF.w (cFilNavn,"REKLAMASJON").
        OS-DELETE VALUE(cFilNavn).
    END.
END.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),IF cSprak = "SE" THEN "Fel i utskrift av reklamationslista " ELSE "Feil i utskrift av reklamasjonsliste ",""). 

/* IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refreshByRowid C-Win 
PROCEDURE refreshByRowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cRowidList AS CHARACTER NO-UNDO. /*Reklamasjonslogg '|' ReklamasjonsLinje*/
  
  RUN InvokeMethod(hBrowse,"OpenQuery").
  /*hBrowse:QUERY:REPOSITION-TO-ROWID(TO-ROWID(ENTRY(1,cRowidList,'|'))).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartQuery C-Win 
PROCEDURE StartQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cWhere AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
    cWhere = buildFilter(cWhere,ReklamasjonsNr:HANDLE,'ReklamasjonsNr','EQ').   
    cWhere = cWhere + buildFilter(cWhere,LevKod:HANDLE,'LevKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,vareTekst:HANDLE,'VareTekst','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,LevFargKod:HANDLE,'LevFargKod','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,Storl:HANDLE,'Storl','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,ArtikkelNr:HANDLE,'ArtikkelNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,cbTTId:HANDLE,'TTId','EQ').      
    cWhere = cWhere + buildFilter(cWhere,Butik:HANDLE,'Butik','EQ').

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 
    
    cWhere = ''.
    cWhere = cWhere + buildFilter(cWhere,LevNr:HANDLE,'LevNr','EQ').
    cWhere = cWhere + buildFilter(cWhere,ReklamStatus:HANDLE,'ReklamStatus','EQ').      
    cWhere = cWhere + buildFilter(cWhere,Kundenr:HANDLE,'kundenr','EQ').
    cWhere = cWhere + buildFilter(cWhere,kundenavn:HANDLE,'kundenavn','BEGINS').
    cWhere = cWhere + buildFilter(cWhere,fraFrist-dato:HANDLE,'Frist-dato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilFrist-dato:HANDLE,'Frist-dato','LE'). 
    cWhere = cWhere + buildFilter(cWhere,fraRegistrertDato:HANDLE,'RegistrertDato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilRegistrertDato:HANDLE,'RegistrertDato','LE').
    cWhere = cWhere + buildFilter(cWhere,fraFakturertDato:HANDLE,'FakturertDato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilFakturertDato:HANDLE,'FakturertDato','LE'). 
    cWhere = cWhere + buildFilter(cWhere,fraOppdatertDato:HANDLE,'OppdLagerDato','GE').
    cWhere = cWhere + buildFilter(cWhere,tilOppdatertDato:HANDLE,'OppdLagerDato','LE'). 
    IF TG-Kost:CHECKED THEN
        cWhere = cWhere + (IF cWhere = '' THEN ' where ' ELSE ' and ') + 'ReklamUtgifter = 0' .
/*     ELSE                                                                                        */
/*         cWhere = cWhere + (IF cWhere = '' THEN ' where ' ELSE ' and ') + 'ReklamUtgifter = 0' . */

    IF INPUT cbOppdLager = 1 THEN
        cWhere = cWhere + (IF cWhere = '' THEN ' where ' ELSE ' and ') + 'OppdLager = false' .
    ELSE IF INPUT cbOppdLager = 2 THEN
        cWhere = cWhere + (IF cWhere = '' THEN ' where ' ELSE ' and ') + 'OppdLager = true' .
    IF INPUT cbFakturert = 1 THEN
        cWhere = cWhere + (IF cWhere = '' THEN ' where ' ELSE ' and ') + 'Fakturert = false' .
    ELSE IF INPUT cbFakturert = 2 THEN
        cWhere = cWhere + (IF cWhere = '' THEN ' where ' ELSE ' and ') + 'Fakturert = true' .

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryfilter",'Reklamasjonslogg '+ cWhere + ',' + DYNAMIC-FUNCTION('getAttribute',hBrowse,'prescanqueryReklamasjonsLogg')).
    
  END.
  RUN invokemethod(hBrowse,'OpenQuery'). 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION adjustBrowseColumns C-Win 
FUNCTION adjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Hook fra NewBrowse slik at kolonner kan flyttes regelbasert.
           Gjør også justeringer av kolonnebredder her slik at disse 
           blir tatt vare på.
    Notes:  
------------------------------------------------------------------------------*/
  ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100.
  ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60.
  ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60.
  RETURN TRUE.
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

  RETURN hBrowse.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Kostnadmodus C-Win 
FUNCTION Kostnadmodus RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF lKostnad THEN DO:
      lKostnad = FALSE.
      RETURN TRUE.
  END.
  ELSE
      RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

