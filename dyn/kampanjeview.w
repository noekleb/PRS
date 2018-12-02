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
DEF VAR hToolbar    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSendtDato rectToolbar KampID KampNavn ~
KampEierID KampKlar KampStartDato KampStartTid KampSluttDato KampSluttTid ~
btnSluttDato KampSendtDato KampSendtTid btnStartDato 
&Scoped-Define DISPLAYED-OBJECTS KampID KampNavn KampEierID KampKlar ~
KampStartDato KampStartTid KampSluttDato KampSluttTid KampSendtDato ~
KampSendtTid 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE KampEierID AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Eier" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE KampSendtTid AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "00:00",0
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE KampSluttTid AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "00:00",0
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE KampStartTid AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "00:00",0
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE KampID AS DECIMAL FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE KampNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE KampSendtDato AS DATE FORMAT "99/99/99":U 
     LABEL "Sendt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE KampSluttDato AS DATE FORMAT "99/99/99":U 
     LABEL "Slutt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE KampStartDato AS DATE FORMAT "99/99/99":U 
     LABEL "Start" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE VARIABLE KampKlar AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSendtDato AT ROW 9.81 COL 35 NO-TAB-STOP 
     KampID AT ROW 3.14 COL 19 COLON-ALIGNED 
     KampNavn AT ROW 4.24 COL 19 COLON-ALIGNED 
     KampEierID AT ROW 5.29 COL 19 COLON-ALIGNED 
     KampKlar AT ROW 6.48 COL 21
     KampStartDato AT ROW 7.43 COL 19 COLON-ALIGNED
     KampStartTid AT ROW 7.43 COL 38 COLON-ALIGNED NO-LABEL 
     KampSluttDato AT ROW 8.62 COL 19 COLON-ALIGNED 
     KampSluttTid AT ROW 8.62 COL 38 COLON-ALIGNED NO-LABEL
     btnSluttDato AT ROW 8.62 COL 35 NO-TAB-STOP 
     KampSendtDato AT ROW 9.81 COL 19 COLON-ALIGNED 
     KampSendtTid AT ROW 9.81 COL 38 COLON-ALIGNED NO-LABEL 
     btnStartDato AT ROW 7.43 COL 35 NO-TAB-STOP 
     rectToolbar AT ROW 1.24 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 61.2 BY 11.62.


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
         TITLE              = "JukeBox template container"
         HEIGHT             = 11.71
         WIDTH              = 61.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeBox template container */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeBox template container */
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


&Scoped-define SELF-NAME btnStartDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartDato C-Win
ON CHOOSE OF btnStartDato IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF btnStartDato
DO:
  RUN Cal.w (KampStartDato:HANDLE).
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
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY KampID KampNavn KampEierID KampKlar KampStartDato KampStartTid 
          KampSluttDato KampSluttTid KampSendtDato KampSendtTid 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSendtDato rectToolbar KampID KampNavn KampEierID KampKlar 
         KampStartDato KampStartTid KampSluttDato KampSluttTid btnSluttDato 
         KampSendtDato KampSendtTid btnStartDato 
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
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  /*
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "",
                              "<buffer>"
                             + ";field1>"
                             ,"WHERE ..."
                             "").
  
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  */
   
  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                              rectToolbar:HANDLE,
                              "File",
                              "New,Save,Delete"
                             ,"maxborder").

/*   DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar). */
END.

/*   DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).  */

/* When multi-company db, subscribe to the ChangeCompany event: 
SUBSCRIBE TO "ChangeCompany" ANYWHERE.

*/

DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

