&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Leverantor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Leverantor 
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
/*          This .W file was created with the Progress UIB.             */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Lev

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES levbas valuta

/* Definitions for BROWSE BROWSE-Lev                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Lev levbas.levnr levbas.levnamn levbas.levadr levbas.levpadr levbas.levponr levbas.levtel levbas.telefax levbas.valkod valuta.valkurs levbas.levland levbas.levkon levbas.levsal levbas.kopadr levbas.koponr levbas.kotel levbas.kotelefax   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Lev   
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-Lev
&Scoped-define SELF-NAME BROWSE-Lev
&Scoped-define OPEN-QUERY-BROWSE-Lev OPEN QUERY {&SELF-NAME} FOR EACH levbas NO-LOCK where       levbas.levnamn matches FILL-IN-Lev:screen-value + "*", ~
             EACH valuta OF levbas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Lev levbas valuta
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Lev levbas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-Lev}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-Lev BUTTON-Blank BROWSE-Lev ~
Btn_Avbryt Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Lev 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Leverantor AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Avbryt AUTO-END-KEY DEFAULT 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Blank 
     LABEL "&Blank" 
     SIZE 8.2 BY 1.14 TOOLTIP "BLANKAR filter och öppnar söklistan igen".

DEFINE VARIABLE FILL-IN-Lev AS CHARACTER FORMAT "X(256)":U INITIAL "*" 
     LABEL "Namnsök" 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1 TOOLTIP "Skriv tecken för att söka på namn" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Lev FOR 
      levbas, 
      valuta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Lev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Lev C-Leverantor _FREEFORM
  QUERY BROWSE-Lev NO-LOCK DISPLAY
      levbas.levnr COLUMN-LABEL "LevNr"
      levbas.levnamn COLUMN-LABEL "Namn"
      levbas.levadr COLUMN-LABEL "Adress"
      levbas.levpadr COLUMN-LABEL "Postadress"
      levbas.levponr COLUMN-LABEL "PostNr"
      levbas.levtel COLUMN-LABEL "Telefon"
      levbas.telefax COLUMN-LABEL "Telefax"
      levbas.valkod COLUMN-LABEL "ValKod" 
      valuta.valkurs COLUMN-LABEL "Kurs" format "zzz9.99999"
      levbas.levland COLUMN-LABEL "Land"
      levbas.levkon COLUMN-LABEL "Kontakt"
      levbas.levsal COLUMN-LABEL "Saldo"
      levbas.kopadr COLUMN-LABEL "K-Postadress"
      levbas.koponr COLUMN-LABEL "Ko-PostNr"
      levbas.kotel COLUMN-LABEL "Ko-Telefon"
      levbas.kotelefax COLUMN-LABEL "Ko-Telefax"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 79.4 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-Lev AT ROW 1.29 COL 9.8 COLON-ALIGNED
     BUTTON-Blank AT ROW 1.29 COL 36.4
     BROWSE-Lev AT ROW 2.52 COL 1.2
     Btn_Avbryt AT ROW 15.67 COL 48.2
     Btn_OK AT ROW 15.67 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Avbryt.


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
  CREATE WINDOW C-Leverantor ASSIGN
         HIDDEN             = YES
         TITLE              = "Sök i leverantörsregistret"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Leverantor
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-Lev BUTTON-Blank DEFAULT-FRAME */
ASSIGN 
       BROWSE-Lev:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME = 2
       BROWSE-Lev:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME     = 1000.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Leverantor)
THEN C-Leverantor:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Lev
/* Query rebuild information for BROWSE BROWSE-Lev
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH levbas NO-LOCK where
      levbas.levnamn matches FILL-IN-Lev:screen-value + "*",
      EACH valuta OF levbas NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE BROWSE-Lev */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Leverantor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Leverantor C-Leverantor
ON END-ERROR OF C-Leverantor /* Sök i leverantörsregistret */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Leverantor C-Leverantor
ON WINDOW-CLOSE OF C-Leverantor /* Sök i leverantörsregistret */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Lev
&Scoped-define SELF-NAME BROWSE-Lev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Lev C-Leverantor
ON MOUSE-SELECT-DBLCLICK OF BROWSE-Lev IN FRAME DEFAULT-FRAME
DO:
  apply "CHOOSE":U to Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Avbryt C-Leverantor
ON CHOOSE OF Btn_Avbryt IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Leverantor
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Blank C-Leverantor
ON CHOOSE OF BUTTON-Blank IN FRAME DEFAULT-FRAME /* Blank */
DO:
  assign
    FILL-IN-Lev:screen-value in frame {&FRAME-NAME} = "*".
  run StartBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Lev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Lev C-Leverantor
ON RETURN OF FILL-IN-Lev IN FRAME DEFAULT-FRAME /* Namnsök */
DO:
  run StartBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Leverantor 


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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  apply "ENTRY":U to BROWSE-Lev.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  return string(recid(LevBAs)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Leverantor _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Leverantor)
  THEN DELETE WIDGET C-Leverantor.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Leverantor _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-Lev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Leverantor.
  ENABLE FILL-IN-Lev BUTTON-Blank BROWSE-Lev Btn_Avbryt Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Leverantor.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Leverantor.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartBrowse C-Leverantor 
PROCEDURE StartBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign
    FILL-IN-Lev = FILL-IN-Lev:screen-value in frame {&FRAME-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  apply "ENTRY":U to BROWSE-Lev.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


