&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

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
def input-output parameter wOvORdreId like OvOrdre.OvOrdreId no-undo.
def input        parameter wModus     as char no-undo.

/* Local Variable Definitions ---                                       */
def var wOk as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-OvOrdreId FI-Merknad FI-OverforesDato ~
EDITOR-1 Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FI-OvOrdreId FI-Merknad FI-OverforesDato ~
EDITOR-1 FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 77 BY 5 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 108 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Merknad AS CHARACTER FORMAT "X(200)" 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1.

DEFINE VARIABLE FI-OverforesDato AS DATE FORMAT "99/99/99" 
     LABEL "Overføres" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE FI-OvOrdreId AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "OvOrdre" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-OvOrdreId AT ROW 1.95 COL 12 COLON-ALIGNED HELP
          "Overføringsordrenummer"
     FI-Merknad AT ROW 1.95 COL 33 COLON-ALIGNED HELP
          "Kort merknad til overøfirngsordren" NO-LABEL
     FI-OverforesDato AT ROW 3 COL 12 COLON-ALIGNED HELP
          "Dato da overføringen ble oppdatert."
     EDITOR-1 AT ROW 5.29 COL 4 NO-LABEL
     Btn_OK AT ROW 12.43 COL 2
     FI-Info AT ROW 16.24 COL 2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.6 BY 16
         DEFAULT-BUTTON Btn_OK.


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
         TITLE              = "Overføringsordre"
         HEIGHT             = 16
         WIDTH              = 113.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 113.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 113.6
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       EDITOR-1:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

/* SETTINGS FOR FILL-IN FI-Info IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Overføringsordre */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Overføringsordre */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  run LagreOvOrdre.
 
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

if wOvORdreId <> ? then
  do:
    find OvOrdre no-lock where
      OvOrdre.OvOrdreId = wOvOrdreId no-error.
    if not available OvOrdre then
      do:
        message "Ukjent overføringsordre (" + string(wOvOrdreId) + ")." 
          view-as alert-box error title "Feil".
        return "AVBRYT".
      end.
  end.
  
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  if wModus = "NY" then
    do:
      current-window:title = "Ny overføringsordre".
      FIND LAST OvOrdre USE-INDEX OvOrdre NO-LOCK NO-ERROR.
      IF AVAILABLE OvOrdre then
        wOvOrdreId = OvOrdre.OvOrdreId + 1.
      ELSE
        wOvOrdreId = 1.      
      FI-OvOrdreId = wOvOrdreId.
    end.
  else if wModus = "ENDRE" then
    current-window:title = "Endre overføringsordre".
  else if wModus = "KOBLE" then
    current-window:title = "Koble overføringsordre".
  else
    current-window:title = "Ukjent modus " + wModus.

  RUN enable_UI.
  {lng.i}
  assign
    current-window:hidden = false.
  if available OvOrdre then
    run VisOvOrdre.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FI-OvOrdreId FI-Merknad FI-OverforesDato EDITOR-1 FI-Info 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-OvOrdreId FI-Merknad FI-OverforesDato EDITOR-1 Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreOvOrdre C-Win 
PROCEDURE LagreOvOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOldOvORdreId like OvOrdre.OvOrdreId no-undo.
  def var wData         as char                no-undo.
  def var wOkStatus     as char                no-undo.
  
  assign frame DEFAULT-FRAME
    FI-Merknad
    FI-OverforesDato
    EDITOR-1
    wData = string(month(FI-OverforesDato),"99")  + "/" +
            string(day(FI-OverforesDato),"99")    + "/" +
            string(year(FI-OverforesDato),"9999") + "£" +
            FI-Merknad                            + "£" +
            EDITOR-1.            
  
  run ovordre.p ("NY","£",wData,output wOvOrdreId,output wOkStatus).
  
  if wOvOrdreId <> wOldOvOrdreId then
    do:
      message "Overføringsordren er tildelt OvOrdreId: " + string(wOvOrdreId) + "."
        view-as alert-box message Title "Melding".
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisOvOrdre C-Win 
PROCEDURE VisOvOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if wModus = "NY" then
    do:
      assign
        EDITOR-1 = "".
      display
        wOvOrdreId @ FI-OvOrdreId
         today      @ FI-OverforesDato
        ""         @ FI-Merknad
        EDITOR-1
      with frame DEFAULT-FRAME.
    end.
  
  else do:
    assign
      EDITOR-1 = OvOrdre.Notat.
    Display
      OvOrdre.OvOrdreId @ FI-OvOrdreId
      OvOrdre.Overfores @ FI-OverforesDato
      OvOrdre.Merknad   @ FI-Merknad
      EDITOR-1
    with frame DEFAULT-FRAME.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

