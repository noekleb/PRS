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

DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR hToolbar     AS HANDLE NO-UNDO.
DEF VAR hFieldMap    AS HANDLE NO-UNDO.
DEF VAR hSearchField AS HANDLE NO-UNDO.
DEF VAR hTabFolder   AS HANDLE NO-UNDO.

DEF VAR hbcAktiv    AS HANDLE NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR.

DEF VAR hCurrTabProc    AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame   AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery   AS HANDLE NO-UNDO.
DEF VAR iCurrTab        AS INT NO-UNDO.

DEF VAR iButNr       AS INT NO-UNDO.
DEF VAR iBrGrpNr     AS INT NO-UNDO.
DEF VAR iBrukerType  AS INT NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnSendtDato rectToolbar rectBrowse ~
rectWinToolbar rectFolder searchField rectFieldMap FI-KampEierID ~
TG-Ejavslutade KampID KampNavn KampEierID KampKlar KampStartDato ~
KampStartTid KampSluttDato KampSluttTid KampSendtDato MinTid KampanjeNotat ~
btnSluttDato btnStartDato 
&Scoped-Define DISPLAYED-OBJECTS FI-KampEierID TG-Ejavslutade KampID ~
KampNavn KampEierID KampKlar KampStartDato KampStartTid KampSluttDato ~
KampSluttTid KampSendtDato MinTid KampanjeNotat 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetKampid C-Win 
FUNCTION GetKampid RETURNS DECIMAL
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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSendtDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSluttDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStartDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-KampEierID AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Eier" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE KampEierID AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Eier" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE KampSluttTid AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "00:00",0
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE KampStartTid AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "00:00",0
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE KampanjeNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 39 BY 3.81 NO-UNDO.

DEFINE VARIABLE KampID AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE KampNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE KampSendtDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Sendt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE KampSluttDato AS DATE FORMAT "99/99/99":U 
     LABEL "Slutt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE KampStartDato AS DATE FORMAT "99/99/99":U 
     LABEL "Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE MinTid AS CHARACTER FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 137.8 BY 30.19.

DEFINE RECTANGLE rectFieldMap
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 12.24.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY 17.91.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE VARIABLE KampKlar AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Ejavslutade AS LOGICAL INITIAL yes 
     LABEL "Ej avslutade" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1 BY 21.43
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSendtDato AT ROW 9.33 COL 173.8 NO-TAB-STOP 
     FI-KampEierID AT ROW 1.24 COL 56 COLON-ALIGNED WIDGET-ID 2
     TG-Ejavslutade AT ROW 1.38 COL 26
     KampID AT ROW 2.67 COL 156 COLON-ALIGNED
     KampNavn AT ROW 3.76 COL 156 COLON-ALIGNED
     KampEierID AT ROW 4.81 COL 156 COLON-ALIGNED
     KampKlar AT ROW 6 COL 158
     KampStartDato AT ROW 6.95 COL 156 COLON-ALIGNED
     KampStartTid AT ROW 6.95 COL 176 COLON-ALIGNED NO-LABEL
     KampSluttDato AT ROW 8.14 COL 156 COLON-ALIGNED
     KampSluttTid AT ROW 8.14 COL 176 COLON-ALIGNED NO-LABEL
     KampSendtDato AT ROW 9.33 COL 156 COLON-ALIGNED
     MinTid AT ROW 9.33 COL 176 COLON-ALIGNED NO-LABEL
     KampanjeNotat AT ROW 10.52 COL 158 NO-LABEL
     btnSluttDato AT ROW 8.14 COL 173.8 NO-TAB-STOP 
     btnStartDato AT ROW 6.95 COL 173.8 NO-TAB-STOP 
     rectToolbar AT ROW 1.24 COL 93.8
     rectBrowse AT ROW 2.48 COL 1.2
     rectWinToolbar AT ROW 1.24 COL 192
     rectFolder AT ROW 14.76 COL 146
     searchField AT ROW 1.24 COL 2.2
     rectFieldMap AT ROW 2.48 COL 146
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.6 BY 31.86.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 140 ROW 2.62
         SIZE 5 BY 30.05.


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
         TITLE              = "Kampanje"
         HEIGHT             = 31.86
         WIDTH              = 202.6
         MAX-HEIGHT         = 31.86
         MAX-WIDTH          = 202.6
         VIRTUAL-HEIGHT     = 31.86
         VIRTUAL-WIDTH      = 202.6
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
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       rectFieldMap:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kampanje */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kampanje */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSendtDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSendtDato C-Win
ON CHOOSE OF btnSendtDato IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF btnSendtDato 
DO:
  RUN Cal.w (KampSendtDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSluttDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSluttDato C-Win
ON CHOOSE OF btnSluttDato IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF btnSluttDato
DO:
  RUN Cal.w (KampSluttDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnStartDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartDato C-Win
ON CHOOSE OF btnStartDato IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF btnStartDato
DO:
  RUN Cal.w (KampStartDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-KampEierID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-KampEierID C-Win
ON VALUE-CHANGED OF FI-KampEierID IN FRAME DEFAULT-FRAME /* Eier */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Ejavslutade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Ejavslutade C-Win
ON VALUE-CHANGED OF TG-Ejavslutade IN FRAME DEFAULT-FRAME /* Ej avslutade */
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
  RUN disable_UI.
END.

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
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}
{incl/conttrigg.i hCurrTabQuery}

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  RUN MoveToTop IN hCurrTabProc NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnKvitteringRecord C-Win 
PROCEDURE btnKvitteringRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN KampSendMotta.w.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnSendRecord C-Win 
PROCEDURE btnSendRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME Default-Frame:
    IF KampKlar:CHECKED = FALSE THEN
    DO:
        MESSAGE 'Kampanjen er ikke aktivert. Den må aktiveres før den kan sendes.'
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN.
    END.
END.

MESSAGE "Skall kampanjen sändas?" VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lSend AS LOGICAL.
IF NOT lSend THEN
    RETURN NO-APPLY.

bok =  DYNAMIC-FUNCTION('runProc','kampanjemixmatch_send.p',STRING(hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE),?).
IF NOT bOk THEN MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* DYNAMIC-FUNCTION('setCurrentObject',hBrowse).           */
/* DYNAMIC-FUNCTION('applyEvent',hBrowse,'value-changed'). */
DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
APPLY "value-changed" TO hBrowse. /* <- For å få kjørt DisplayRecord */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnTeamRecord C-Win 
PROCEDURE btnTeamRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN d-bbutikkteam.w (1,3,"v").
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
  DISPLAY FI-KampEierID TG-Ejavslutade KampID KampNavn KampEierID KampKlar 
          KampStartDato KampStartTid KampSluttDato KampSluttTid KampSendtDato 
          MinTid KampanjeNotat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSendtDato rectToolbar rectBrowse rectWinToolbar rectFolder 
         searchField rectFieldMap FI-KampEierID TG-Ejavslutade KampID KampNavn 
         KampEierID KampKlar KampStartDato KampStartTid KampSluttDato 
         KampSluttTid KampSendtDato MinTid KampanjeNotat btnSluttDato 
         btnStartDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeHead C-Win 
PROCEDURE InitializeHead :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/* Er brukeren brukertype 2 og det er satt butikknr. på bruker, skal restriksjoner tre i kraft. */
ASSIGN iBrGrpNr    = INT(DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrGrpNr")) NO-ERROR.
ASSIGN iButNr      = INT(DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")) NO-ERROR.
ASSIGN iBrukerType = INT(DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrukerType")) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  
  ASSIGN 
    KampEierId:DELIMITER = "|"
    FI-KampEierId:DELIMITER = "|".

  IF iButNr > 0  AND iBrukertype = 2 THEN
  DO:
      cTekst = DYNAMIC-FUNCTION("getFieldList","KampanjeEier;KampEierId|KampEierNavn;KampEierId","where KampanjeEier.ButikkNr = '" + STRING(iButNr) + "'" ). 
      IF cTekst = '' THEN KampEierId:LIST-ITEM-PAIRS = "|0".
      ELSE KampEierId:LIST-ITEM-PAIRS = /*"|0|" + */ cTekst.
      FI-KampEierId:LIST-ITEM-PAIRS = KampEierId:LIST-ITEM-PAIRS.
      FI-KampEierId:SCREEN-VALUE    = ENTRY(2,FI-KampEierId:LIST-ITEM-PAIRS,'|').
  END.
  ELSE DO:
      cTekst = DYNAMIC-FUNCTION("getFieldList","KampanjeEier;KampEierId|KampEierNavn;KampEierId","where true"). 
      IF cTekst = '' THEN KampEierId:LIST-ITEM-PAIRS = "|0".
      ELSE KampEierId:LIST-ITEM-PAIRS = /*"|0|" + */ cTekst.
      FI-KampEierId:LIST-ITEM-PAIRS = "|0|" + KampEierId:LIST-ITEM-PAIRS.
      FI-KampEierId:SCREEN-VALUE    = ENTRY(2,FI-KampEierId:LIST-ITEM-PAIRS,'|').
  END.
  
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "",
                              "kampanjemixmatch"
                             + ";!KampEierId"
                             + ";KampId;KampNavn;KampStartDato;KampSluttDato;KampKlar|Aktiv"
                             + ";!KampStartTid;!KampSluttTid;!KampSendtDato"
                             + ";!KampSendtTid;!KampanjeNotat"
                             + ";!+MinTid|CHARACTER|x(5)|jb_hhmm(KampSendtTid)"
                             + ",KampanjeEier" 
                             + ";!KampEierId;KampEierNavn|EIER"
                             ,"WHERE FALSE"
                               + " ,FIRST KampanjeEier OF KampanjeMixMatch NO-LOCK",
                             "sort|KampId desc").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"flatviewjointype","outer-join").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
  
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "KampNavn,KampKlar,KampEierId,KampStartDato,KampStartTid,KampSluttDato,KampSluttTid,KampanjeNotat",   /* Update columns in buffer */
                      "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */
                    "KampId,KampSendtDato,MinTid",        /* Additional buffer and displ.fields - not updateable*/
                      "",                           /* Corresponding fill-ins */
                    "btnStartDato,btnSluttDato").                            /* other input widgets and lookup buttons for update fill-ins */

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customcreateproc","kampanjemixmatch_create.p").

  
  &SCOPED-DEFINE FilterTable KampanjeEier
  &SCOPED-DEFINE FilterField1 KampEierId
  &SCOPED-DEFINE FilterField2 KampEierNavn
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterdropdownfields_{&FilterField}","{&FilterTable};{&FilterField};{&FilterField}"). */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterdropdownquery_{&FilterField}","where true").                                    */
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_{&FilterField2}","{&FilterTable};{&FilterField1};{&FilterField2}").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_{&FilterField2}","where false").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_{&FilterField2}","{&FilterField2}").

  fillTime(KampStartTid:HANDLE,'00:00','23:59',30).
  KampSluttTid:LIST-ITEM-PAIRS = KampStartTid:LIST-ITEM-PAIRS.
/*   KampSendtTid:LIST-ITEM-PAIRS = KampStartTid:LIST-ITEM-PAIRS. */
  btnSendtDato:SENSITIVE       = FALSE. /*Vil alltid være dette*/

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "new,undo,delete,save"
                    + ",btnSend;S&end,Print"
                    + ",Filter,excel;Eksporter til E&xcel,flatview;Flat&view,BrowseConfig"
                    + ",btnTeam;Team,btnKvittering;Kvitto"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */
  
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "File",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */
  RUN InvokeMethod(hBrowse,"OpenQuery").
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
  RUN InitializeHead.
  
  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Tilbud|KampanjeTilbudBrw.w|Butikker|KampanjeButikkerBrw.w",hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  InitializeResize().

  APPLY "entry" TO hSearchField.

  rectFieldMap:HIDDEN = YES.

  APPLY "value-changed" TO hBrowse.
END.

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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

  RUN SUPER.

  DO WITH FRAME Default-Frame:
      KampEierId:SCREEN-VALUE = ENTRY(2,KampEierId:LIST-ITEM-PAIRS,'|').
      KampEierId = DEC(ENTRY(2,KampEierId:LIST-ITEM-PAIRS,'|')).
  END.


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
DEF VAR cOptWhere AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      IF TG-Ejavslutade:CHECKED THEN
          cOptWhere = " AND KampanjeMixMatch.KampSluttDato >= " + STRING(TODAY).
    
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",'').
    IF DEC(FI-KampEierID:SCREEN-VALUE) > 0  THEN
    DO:
        cWhere = buildFilter(cWhere,FI-KampEierID:HANDLE,'KampEierID','EQ').
        cWhere = cWhere + cOptWhere.
    END.
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere).
    ASSIGN
      FI-KampEierID:MODIFIED = FALSE.
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
    RUN skrivKampMM.p (hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE).
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
  IF VALID-HANDLE(hbcAktiv) THEN
  ASSIGN 
    hbcAktiv:FONT      = iFontWingdings
    hbcAktiv:FORMAT    = CHR(254) + "/"  + CHR(168)
  .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN "KampKlar" THEN 
    DO:
      IF KampKlar:CHECKED IN FRAME {&FRAME-NAME} THEN 
      DO:
        MESSAGE "Dette vil medføre at kampanjen kan overføres til butikk."
          SKIP "Er du sikker?"
          SKIP 1 "Bruk 'Send' funksjonen for å sende kampanjen til butikkene etter at den er aktivert."
          VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOk.
        IF NOT bOk THEN 
          KampKlar:CHECKED IN FRAME {&FRAME-NAME} = NOT KampKlar:CHECKED.
      END.
    END.
  END CASE.
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
ihBrowse:MOVE-COLUM(6,5). 
hbcAktiv      = ihBrowse:GET-BROWSE-COLUMN(6).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetKampid C-Win 
FUNCTION GetKampid RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKId AS DECIMAL    NO-UNDO.
  dKid = hFieldMap:BUFFER-FIELD('KampId'):BUFFER-VALUE NO-ERROR.   /* Function return value. */
  RETURN dKid.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  - Create the splitbar and grab the widgets that should follow it's move.
            - Set resize rules for this frame 
            - Set resize rules for window (and load previous settings for size and pos)
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectBrowse:HANDLE IN FRAME {&FRAME-NAME},0,"frame,control-frame,rectangle,browse") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor,button,toggle-box,combo-box,fill-in") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectFieldMap:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor,button,toggle-box,combo-box,fill-in")
                    ).


  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse,searchField," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar,KampanjeNotat").
  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, DYNAMIC-FUNCTION("getToolbarNames",rectWinToolBar:HANDLE,"")).

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

hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab).

DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"KampId").

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

