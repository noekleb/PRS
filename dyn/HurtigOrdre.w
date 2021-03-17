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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR cAvdelingRowIdList  AS CHAR NO-UNDO.
DEF VAR cAvdelingIdList     AS CHAR NO-UNDO.
DEF VAR iSelectorSourcCount AS INT  NO-UNDO.
DEF VAR hDetaljer           AS HANDLE NO-UNDO.
DEF VAR bCloseDet           AS LOG    NO-UNDO INIT YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ButikkNr btnLeveringsdatoTil ForsNr ~
sokAvdelingNr sokAvdelingNavn btnRegistrertDatoTil btnLeveringsdatoFra ~
Varetekst LeveringsdatoFra LeveringsdatoTil btnRegistrertDatoFra ~
RegistrertDatoFra RegistrertDatoTil Navn KOrdre_Id btnAvdeling LevStatus ~
brnOpenQuery brnClearFilter SvarFristFra SvarFristTil btnSvarFristFra ~
btnSvarFristTil KOrdreNavBrowse tbKasseOrdre 
&Scoped-Define DISPLAYED-OBJECTS ButikkNr ForsNr sokAvdelingNr ~
sokAvdelingNavn Varetekst LeveringsdatoFra LeveringsdatoTil ~
RegistrertDatoFra RegistrertDatoTil Navn KOrdre_Id LevStatus SvarFristFra ~
SvarFristTil 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDetailWinPos C-Win 
FUNCTION setDetailWinPos RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDetaljerHandle C-Win 
FUNCTION setDetaljerHandle RETURNS LOGICAL
  ( INPUT ihDetaljer AS HANDLE,
    INPUT ihDetQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON brnClearFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON brnOpenQuery 
     LABEL "&Start søk" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnAvdeling 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i avdelinger som finnes i vareboken".

DEFINE BUTTON btnLeveringsdatoFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnLeveringsdatoTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnRegistrertDatoFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnRegistrertDatoTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnSvarFristFra 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnSvarFristTil 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE VARIABLE ButikkNr AS CHARACTER FORMAT "X(256)" 
     LABEL "&Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 TOOLTIP "Trykk ENTER for å skifte butikk" NO-UNDO.

DEFINE VARIABLE ForsNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasserer" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE LevStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 23.6 BY 1 NO-UNDO.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Internt faktura id. Tildeles autmatisk av systemet.".

DEFINE VARIABLE LeveringsdatoFra AS DATE FORMAT "99/99/9999" 
     LABEL "Fra lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Leveringsdato".

DEFINE VARIABLE LeveringsdatoTil AS DATE FORMAT "99/99/9999" 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Leveringsdato".

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Kundenavn" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 TOOLTIP "Navn eller firmanavn".

DEFINE VARIABLE RegistrertDatoFra AS DATE FORMAT "99/99/9999" 
     LABEL "Fra reg.dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Dato da posten ble registrert i registeret".

DEFINE VARIABLE RegistrertDatoTil AS DATE FORMAT "99/99/9999" 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Dato da posten ble registrert i registeret".

DEFINE VARIABLE sokAvdelingNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.8 BY 1 NO-UNDO.

DEFINE VARIABLE sokAvdelingNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE VARIABLE SvarFristFra AS DATE FORMAT "99/99/9999" 
     LABEL "Fra svarfrist" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE SvarFristTil AS DATE FORMAT "99/99/9999" 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE Varetekst AS CHARACTER FORMAT "X(30)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 32.2 BY 1 TOOLTIP "Varetekst".

DEFINE RECTANGLE KOrdreNavBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 126 BY 11.33.

DEFINE RECTANGLE tbKasseOrdre
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButikkNr AT ROW 2.76 COL 11 COLON-ALIGNED HELP
          "Trykk ENTER for å skifte butikk"
     btnLeveringsdatoTil AT ROW 3.81 COL 105.8 NO-TAB-STOP 
     ForsNr AT ROW 3.81 COL 11 COLON-ALIGNED
     sokAvdelingNr AT ROW 4.91 COL 11 COLON-ALIGNED HELP
          "Varegruppe"
     sokAvdelingNavn AT ROW 4.91 COL 17.4 COLON-ALIGNED NO-LABEL
     btnRegistrertDatoTil AT ROW 4.86 COL 105.8 NO-TAB-STOP 
     btnLeveringsdatoFra AT ROW 3.81 COL 82 NO-TAB-STOP 
     Varetekst AT ROW 5.95 COL 11 COLON-ALIGNED
     LeveringsdatoFra AT ROW 3.81 COL 64 COLON-ALIGNED
     LeveringsdatoTil AT ROW 3.81 COL 87.8 COLON-ALIGNED
     btnRegistrertDatoFra AT ROW 4.86 COL 82 NO-TAB-STOP 
     RegistrertDatoFra AT ROW 4.86 COL 64 COLON-ALIGNED
     RegistrertDatoTil AT ROW 4.86 COL 87.8 COLON-ALIGNED
     Navn AT ROW 5.95 COL 64 COLON-ALIGNED
     KOrdre_Id AT ROW 7 COL 64 COLON-ALIGNED
     btnAvdeling AT ROW 4.91 COL 45.2 NO-TAB-STOP 
     LevStatus AT ROW 7 COL 88.4 COLON-ALIGNED
     brnOpenQuery AT ROW 2.76 COL 112.2
     brnClearFilter AT ROW 3.95 COL 112.2
     SvarFristFra AT ROW 2.76 COL 64 COLON-ALIGNED
     SvarFristTil AT ROW 2.76 COL 87.8 COLON-ALIGNED
     btnSvarFristFra AT ROW 2.76 COL 82 NO-TAB-STOP 
     btnSvarFristTil AT ROW 2.76 COL 105.8 NO-TAB-STOP 
     KOrdreNavBrowse AT ROW 8.14 COL 2
     tbKasseOrdre AT ROW 1.43 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 127.8 BY 18.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Søk Hurtig-ordre"
         HEIGHT             = 18.67
         WIDTH              = 128
         MAX-HEIGHT         = 25.1
         MAX-WIDTH          = 145.2
         VIRTUAL-HEIGHT     = 25.1
         VIRTUAL-WIDTH      = 145.2
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 18.76
       FRAME DEFAULT-FRAME:WIDTH            = 127.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søk Hurtig-ordre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søk Hurtig-ordre */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME brnClearFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brnClearFilter C-Win
ON CHOOSE OF brnClearFilter IN FRAME DEFAULT-FRAME /* Blank filter */
DO:
  ASSIGN ButikkNr:SCREEN-VALUE = "0"
         LevStatus:SCREEN-VALUE = "0"
         KOrdre_Id:SCREEN-VALUE = "0"
         SvarfristFra:SCREEN-VALUE = ?
         SvarfristTil:SCREEN-VALUE = ?
         LeveringsdatoFra:SCREEN-VALUE = ?
         LeveringsdatoTil:SCREEN-VALUE = ?
         Navn:SCREEN-VALUE = ""
         RegistrertDatoFra:SCREEN-VALUE = ?
         RegistrertDatoTil:SCREEN-VALUE = ?
         sokAvdelingNavn:SCREEN-VALUE = ""
         sokAvdelingNr:SCREEN-VALUE = "0"
         .
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME brnOpenQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brnOpenQuery C-Win
ON CHOOSE OF brnOpenQuery IN FRAME DEFAULT-FRAME /* Start søk */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAvdeling C-Win
ON CHOOSE OF btnAvdeling IN FRAME DEFAULT-FRAME /* ... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
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
                                       STRING(iSelectorSourcCount)
                                       .
    ELSE
      ASSIGN sokAvdelingNr:SCREEN-VALUE   = cAvdelingIdList
             sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
    IF NOT DYNAMIC-FUNCTION("getAttribute",hBrowse,"uselocaldata") = "yes" THEN
      RUN StartQuery.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLeveringsdatoFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLeveringsdatoFra C-Win
ON CHOOSE OF btnLeveringsdatoFra IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (LeveringsdatoFra:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLeveringsdatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLeveringsdatoTil C-Win
ON CHOOSE OF btnLeveringsdatoTil IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (LeveringsdatoTil:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRegistrertDatoFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRegistrertDatoFra C-Win
ON CHOOSE OF btnRegistrertDatoFra IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (RegistrertDatoFra:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRegistrertDatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRegistrertDatoTil C-Win
ON CHOOSE OF btnRegistrertDatoTil IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (RegistrertDatoTil:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSvarFristFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSvarFristFra C-Win
ON CHOOSE OF btnSvarFristFra IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (SvarFristFra:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSvarFristTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSvarFristTil C-Win
ON CHOOSE OF btnSvarFristTil IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (SvarFristTil:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON VALUE-CHANGED OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  ASSIGN ForsNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" 
                                + DYNAMIC-FUNCTION("getFieldList",
                                                   "butikkforsalj,forsalj;forsnr|Fonamn;forsnr",
                                                   "where " 
                                                 + (IF ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN
                                                     "butik = " + ButikkNr:SCREEN-VALUE 
                                                    ELSE "true")
                                                 + ",first forsalj NO-LOCK of butikkforsalj")
                                            ,"|")
         ForsNr:SCREEN-VALUE = "0"
         .                                                
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ForsNr C-Win
ON VALUE-CHANGED OF ForsNr IN FRAME DEFAULT-FRAME /* Kasserer */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KOrdre_Id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KOrdre_Id C-Win
ON RETURN OF KOrdre_Id IN FRAME DEFAULT-FRAME /* Ordrenr */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LeveringsdatoFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LeveringsdatoFra C-Win
ON RETURN OF LeveringsdatoFra IN FRAME DEFAULT-FRAME /* Fra lev.dato */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LeveringsdatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LeveringsdatoTil C-Win
ON RETURN OF LeveringsdatoTil IN FRAME DEFAULT-FRAME /* Til */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevStatus C-Win
ON VALUE-CHANGED OF LevStatus IN FRAME DEFAULT-FRAME /* Status */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Navn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Navn C-Win
ON RETURN OF Navn IN FRAME DEFAULT-FRAME /* Kundenavn */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RegistrertDatoFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RegistrertDatoFra C-Win
ON RETURN OF RegistrertDatoFra IN FRAME DEFAULT-FRAME /* Fra reg.dato */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RegistrertDatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RegistrertDatoTil C-Win
ON RETURN OF RegistrertDatoTil IN FRAME DEFAULT-FRAME /* Til */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNavn C-Win
ON RETURN OF sokAvdelingNavn IN FRAME DEFAULT-FRAME
OR TAB OF sokAvdelingNavn DO:
  IF sokAvdelingNavn:MODIFIED THEN DO: 
    ASSIGN cAvdelingRowIdList = ""
           cAvdelingIdList    = ""
           .    
    RUN InvokeMethod(hBrowse,"OpenQuery").
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokAvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON F3 OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
OR f10 OF sokAvdelingNr DO:
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
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokAvdelingNr C-Win
ON TAB OF sokAvdelingNr IN FRAME DEFAULT-FRAME /* Avdeling */
DO:
  IF sokAvdelingNr:MODIFIED  THEN 
    sokAvdelingNavn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNavn","WHERE AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Varetekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Varetekst C-Win
ON RETURN OF Varetekst IN FRAME DEFAULT-FRAME /* Varetekst */
DO:
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
  IF VALID-HANDLE(hDetaljer) AND bCloseDet THEN APPLY "close" TO hDetaljer.
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
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackTabFromBrowse C-Win 
PROCEDURE BackTabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setWidgetEnter",LevStatus:HANDLE IN FRAME {&FRAME-NAME}).
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
RUN EditRecord.

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
DEF VAR hDetQuery AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(hDetaljer) THEN DO:
  RUN HurtigOrdreDetaljer.w PERSIST SET hDetaljer.
  DYNAMIC-FUNCTION("setNoSearchBtn" IN hDetaljer,YES).
  RUN InitializeObject IN hDetaljer.
  hDetQuery = DYNAMIC-FUNCTION("GetLinkedObject",hDetaljer,"query","from").
  DYNAMIC-FUNCTION("CreateOneToOneLink",hDetQuery,hBrowse,"KOrdre_id").
  RUN InvokeMethod(hBrowse,"DisplayRecord").
  setDetailWinPos().
END.
RUN MoveToTop IN hDetaljer.

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
  DISPLAY ButikkNr ForsNr sokAvdelingNr sokAvdelingNavn Varetekst 
          LeveringsdatoFra LeveringsdatoTil RegistrertDatoFra RegistrertDatoTil 
          Navn KOrdre_Id LevStatus SvarFristFra SvarFristTil 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE ButikkNr btnLeveringsdatoTil ForsNr sokAvdelingNr sokAvdelingNavn 
         btnRegistrertDatoTil btnLeveringsdatoFra Varetekst LeveringsdatoFra 
         LeveringsdatoTil btnRegistrertDatoFra RegistrertDatoFra 
         RegistrertDatoTil Navn KOrdre_Id btnAvdeling LevStatus brnOpenQuery 
         brnClearFilter SvarFristFra SvarFristTil btnSvarFristFra 
         btnSvarFristTil KOrdreNavBrowse tbKasseOrdre 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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

iSelectorSourcCount = INT(DYNAMIC-FUNCTION("getAttribute",ihSelectorSource,"totalcount")).

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
DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DEFINE VARIABLE iBrGrpNr AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setAttribute",SESSION,"SeButikkNr",
                   DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")).
  iBrGrpNr = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrGrpNr").

  ButikkNr:DELIMITER = "|".
  IF DYNAMIC-FUNCTION("getAttribute",SESSION,"SeButikkNr") NE "" THEN 
  DO:
    ASSIGN ButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","butikktilgang,butiker;butnamn|butik;butik"
                                               ,"WHERE brgrpnr = " + STRING(iBrGrpNr) + ", first Butiker where Butiker.Butik = ButikkTilgang.Butik"),"|")
           ButikkNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",SESSION,"SeButikkNr").
  END.
  ELSE DO: 
    ASSIGN ButikkNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE true BY ButNamn"),"|")
           ButikkNr:SCREEN-VALUE = DYNAMIC-FUNCTION("getAttribute",SESSION,"ButikkNr").           
  END.

  ASSIGN LevStatus:DELIMITER = "|"
         LevStatus:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 19 and SysGr = 1"),"|")
         ForsNr:DELIMITER = "|"
         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    KOrdreNavBrowse:HANDLE,   
                    100,                 
                    "",                  
                    "KOrdreHode"
                    + ";KOrdre_id|Nr"
                    + ";LevStatus|Status|99"
                    + ";RegistrertDato"
                    + ";RegistrertAv"
                    + ";ForsNr|Kasserer"
                    + ";SvarFrist"
                    + ";Navn|Kundenavn"
                    + ";Telefon"
                    + ";Adresse1"
                    + ";MobilTlf"
                    + ";ButikkNr"
                  + ",SysPara"
                    + ";Parameter1||x(12)@3"
                  + ",Avdeling"
                    + ";AvdelingNavn|Avdeling@9"
                    ,"WHERE false"
                   + ",FIRST SysPara NO-LOCK WHERE SysPara.SysHId = 19 and SysPara.SysGr = 1 AND SysPara.ParaNr = INT(KOrdreHode.LevStatus) OUTER-JOIN"
                   + ",FIRST Avdeling NO-LOCK WHERE Avdeling.AvdelingNr = KOrdreHode.AvdelingNr OUTER-JOIN"
                    ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE Opphav = 5").
  DYNAMIC-FUNCTION("setSortString",hBrowse,ENTRY(2,DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields"))).
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterdropdownvaluelist_LevStatus",DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 19 and SysGr = 1")).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
           ,tbKasseOrdre:HANDLE
           ,"Fil"
           ,"new;Ny,Edit;Endre,BrowseConfig;Kolonneoppsett,Print,excel;Eksporter til E&xcel"
           ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  APPLY "value-changed" TO ButikkNr.

END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,550,200,0,0).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hDetQuery    AS HANDLE NO-UNDO.
DEF VAR hDetToolbar  AS HANDLE NO-UNDO.
DEF VAR hDetFieldMap AS HANDLE NO-UNDO.
DEF VAR cUpdFields   AS CHAR   NO-UNDO.
DEF VAR cUpdWidgets  AS CHAR   NO-UNDO.
DEF VAR hField       AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(hDetaljer) THEN DO WITH FRAME {&FRAME-NAME}:
  RUN HurtigOrdreDetaljer.w PERSIST SET hDetaljer.
  DYNAMIC-FUNCTION("setNoSearchBtn" IN hDetaljer,YES).
  RUN InitializeObject IN hDetaljer.
  ASSIGN hDetQuery    = DYNAMIC-FUNCTION("GetLinkedObject",hDetaljer,"query","from")
         hDetToolbar  = DYNAMIC-FUNCTION("GetLinkedObject",hDetQuery,"toolbar","from")
         hDetFieldMap = DYNAMIC-FUNCTION("GetLinkedObject",hDetQuery,"fieldMap","from")
         cUpdFields   = DYNAMIC-FUNCTION("getAttribute",hDetFieldMap,"ScreenUpdateFields")
         cUpdWidgets  = DYNAMIC-FUNCTION("getAttribute",hDetFieldMap,"ScreenUpdateWidgets")
         .
  DYNAMIC-FUNCTION("CreateOneToOneLink",hDetQuery,hBrowse,"KOrdre_id").

  RUN InvokeMethod(hDetToolbar,"NewRecord").

  hField = WIDGET-HANDLE(ENTRY(LOOKUP("ButikkNr",cUpdFields),cUpdWidgets)) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN DO:
    IF ButikkNr:SCREEN-VALUE NE ? AND ButikkNr:SCREEN-VALUE NE "0" THEN 
      hField:SCREEN-VALUE = ButikkNr:SCREEN-VALUE NO-ERROR.
    ELSE 
      hField:SCREEN-VALUE = ButikkNr:ENTRY(2) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
      APPLY "value-changed" TO hField.
      IF Forsnr:SCREEN-VALUE NE ? AND ForsNr:SCREEN-VALUE NE "0" THEN
        ASSIGN hField = WIDGET-HANDLE(ENTRY(LOOKUP("ForsNr",cUpdFields),cUpdWidgets))
               hField:SCREEN-VALUE = ForsNr:SCREEN-VALUE
               NO-ERROR.
    END.
  END.
  IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN 
    ASSIGN hField = WIDGET-HANDLE(ENTRY(LOOKUP("AvdelingNr",cUpdFields),cUpdWidgets))
           hField:SCREEN-VALUE = sokAvdelingNr:SCREEN-VALUE
           NO-ERROR.
  
  setDetailWinPos().
END.
RUN MoveToTop IN hDetaljer.

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
DEF VAR cVaretekst       AS CHAR NO-UNDO.
DEF VAR cNavn            AS CHAR NO-UNDO.
DEF VAR cPrescanAvdeling AS CHAR NO-UNDO.
DEF VAR cPrescanVare     AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cNavn      = IF INDEX(Navn:SCREEN-VALUE,"*") = LENGTH(Navn:SCREEN-VALUE) THEN RIGHT-TRIM(Navn:SCREEN-VALUE,"*") ELSE Navn:SCREEN-VALUE
         cVareTekst = IF INDEX(VareTekst:SCREEN-VALUE,"*") = LENGTH(VareTekst:SCREEN-VALUE) THEN RIGHT-TRIM(VareTekst:SCREEN-VALUE,"*") ELSE VareTekst:SCREEN-VALUE
         SvarfristFra SvarfristTil LeveringsdatoFra LeveringsdatoTil RegistrertDatoFra RegistrertDatoTil 
         .

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter",
                    (IF ButikkNr:SCREEN-VALUE NE ? AND ButikkNr:SCREEN-VALUE NE "0" THEN
                      " AND ButikkNr = " + ButikkNr:SCREEN-VALUE
                     ELSE "")
                  + (IF ForsNr:SCREEN-VALUE NE ? AND ForsNr:SCREEN-VALUE NE "0" THEN
                      " AND ForsNr = " + ForsNr:SCREEN-VALUE 
                     ELSE "")
                  + (IF LevStatus:SCREEN-VALUE NE ? AND LevStatus:SCREEN-VALUE NE "0" THEN
                      " AND LevStatus = '" + LevStatus:SCREEN-VALUE + "'"
                     ELSE "")
                  + (IF SvarfristFra NE ? THEN
                       " AND Svarfrist GE DATE('" + SvarfristFra:SCREEN-VALUE + "')"
                     ELSE "")
                  + (IF SvarfristTil NE ? THEN
                       " AND Svarfrist LE DATE('" + SvarfristTil:SCREEN-VALUE + "')"
                     ELSE "")
                  + (IF LeveringsDatoFra NE ? THEN
                       " AND LeveringsDato GE DATE('" + LeveringsDatoFra:SCREEN-VALUE + "')"
                     ELSE "")
                  + (IF LeveringsDatoTil NE ? THEN
                       " AND LeveringsDato LE DATE('" + LeveringsDatoTil:SCREEN-VALUE + "')"
                     ELSE "")
                  + (IF RegistrertDatoFra NE ? THEN
                       " AND RegistrertDato GE DATE('" + RegistrertDatoFra:SCREEN-VALUE + "')"
                     ELSE "")
                  + (IF RegistrertDatoTil NE ? THEN
                       " AND RegistrertDato LE DATE('" + RegistrertDatoTil:SCREEN-VALUE + "')"
                     ELSE "")
                  + (IF cNavn NE "" THEN
                      (IF INDEX(cNavn,"*") > 0 THEN
                        " AND Navn MATCHES '" + cNavn + "'"
                       ELSE 
                         " AND Navn BEGINS '" + cNavn + "'")
                     ELSE "")
                  + (IF KOrdre_Id:SCREEN-VALUE NE "0" THEN 
                      " AND KOrdre_Id = " + KOrdre_Id:SCREEN-VALUE
                     ELSE "")
                  + (IF sokAvdelingNr:SCREEN-VALUE NE "0" THEN
                      " AND AvdelingNr = " + sokAvdelingNr:SCREEN-VALUE
                     ELSE "")
                    ).

  ASSIGN cPrescanAvdeling = IF sokAvdelingNr:SCREEN-VALUE = "0" AND sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) < 2 THEN
                             " WHERE " +
                             (IF INDEX(sokAvdelingNavn:SCREEN-VALUE,"*") > 0 THEN 
                               "Avdeling.AvdelingNavn MATCHES '" + sokAvdelingNavn:SCREEN-VALUE + "*'"
                              ELSE
                               "Avdeling.AvdelingNavn BEGINS '" + sokAvdelingNavn:SCREEN-VALUE + "'")
                            ELSE IF sokAvdelingNavn:SCREEN-VALUE NE "" AND NUM-ENTRIES(cAvdelingRowIdList) > 1 THEN
                             " WHERE CAN-DO('" + REPLACE(cAvdelingIdList,"|",",") + "',STRING(Avdeling.AvdelingNr))"
                            ELSE ""
         cPrescanVare     = IF VareTekst:SCREEN-VALUE NE "" THEN
                             (IF INDEX(cVaretekst,"*") > 0 THEN
                               " WHERE VareTekst MATCHES '" + cVaretekst + "'"
                              ELSE 
                                " WHERE VareTekst BEGINS '" + cVaretekst + "'")
                            ELSE "" 
         .
                    
  IF cPrescanAvdeling NE "" THEN
    cPrescanAvdeling = "Avdeling " + cPrescanAvdeling
                     + ",FIRST KordreHode NO-LOCK WHERE KordreHode.AvdelingNr = Avdeling.AvdelingNr "
                     + REPLACE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery"),"WHERE","AND")
                     + DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryFilter")
                     .
/*   cPrescanAvdeling = "buf1_KordreHode NO-LOCK "                                                */
/*                    + DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery")                      */
/*                    + DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryFilter")                    */
/*                    + ",EACH KordreLinje NO-LOCK OF buf1_KordreHode"                            */
/*                    + ",FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = DEC(KordreLinje.Varenr)" */
/*                    + ",FIRST VarGr NO-LOCK OF ArtBas"                                          */
/*                    + ",FIRST HuvGr NO-LOCK OF VarGr"                                           */
/*                    + ",FIRST Avdeling NO-LOCK OF HuvGr " + cPrescanAvdeling                    */
/*                    + ",FIRST KordreHode NO-LOCK OF KordreLinje".                               */
  IF cPrescanVare NE "" THEN
    cPrescanVare = "KordreLinje " + cPrescanVare 
                 + ",FIRST KordreHode NO-LOCK WHERE KOrdreHode.KOrdre_Id = KordreLinje.KOrdre_Id "
                 + REPLACE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"baseQuery"),"WHERE","AND")
                 + DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryFilter")
                 .


  DYNAMIC-FUNCTION("setAttribute",hBrowse,"PrescanQueryFilter",
                   cPrescanAvdeling + "|" + cPrescanVare).
END.

RUN SUPER.

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
RUN skrivkundeordre.p (STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|full",
                       NO,"",1,"","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDetailWinPos C-Win 
FUNCTION setDetailWinPos RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hDetWin AS HANDLE NO-UNDO.
DEF VAR hMyWin  AS HANDLE NO-UNDO.

ASSIGN hDetWin = hDetaljer:CURRENT-WINDOW
       hMyWin  = THIS-PROCEDURE:CURRENT-WINDOW
       .
IF VALID-HANDLE(hDetWin) THEN DO:
  IF hMyWin:X + hMyWin:WIDTH-PIXELS + hDetWin:WIDTH-CHARS < SESSION:WORK-AREA-WIDTH-PIXELS - 5 AND
     hMyWin:Y + hMyWin:HEIGHT-PIXELS + hDetWin:HEIGHT-PIXELS < SESSION:WORK-AREA-HEIGHT-PIXELS - 5  THEN
    ASSIGN hDetWin:X = hMyWin:X + hMyWin:WIDTH-PIXELS + 5
           hDetWin:Y = hMyWin:Y
           .
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDetaljerHandle C-Win 
FUNCTION setDetaljerHandle RETURNS LOGICAL
  ( INPUT ihDetaljer AS HANDLE,
    INPUT ihDetQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hDetaljer = ihDetaljer
       bCloseDet = NO.

DYNAMIC-FUNCTION("CreateOneToOneLink",ihDetQuery,hBrowse,"KOrdre_id").

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

