&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-HuvGr 
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
def var wOk           as log no-undo.
def var wLoop         as int no-undo.
def var wHuvGrRecid   as recid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES HuvGr

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 HuvGr.Hg HuvGr.HgBeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 HuvGr.HgBeskr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 HuvGr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 HuvGr
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH HuvGr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 HuvGr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 HuvGr


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-3 BUTTON-Detalj Btn_Avbryt Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-HuvGr AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Avbryt AUTO-END-KEY DEFAULT 
     LABEL "&Avbryt" 
     SIZE 9 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 9 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Detalj 
     LABEL "&Detalj" 
     SIZE 9 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      HuvGr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-HuvGr _STRUCTURED
  QUERY BROWSE-3 DISPLAY
      HuvGr.Hg COLUMN-LABEL "Nr" FORMAT "zz9":U
      HuvGr.HgBeskr COLUMN-LABEL "Beskrivning" FORMAT "x(30)":U
  ENABLE
      HuvGr.HgBeskr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 38 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-3 AT ROW 1.24 COL 2
     BUTTON-Detalj AT ROW 9.81 COL 2
     Btn_Avbryt AT ROW 9.81 COL 22
     Btn_OK AT ROW 9.81 COL 31
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 39.8 BY 10.14.


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
  CREATE WINDOW C-HuvGr ASSIGN
         HIDDEN             = YES
         TITLE              = "Sök Huvudgrupp"
         HEIGHT             = 10.14
         WIDTH              = 39.4
         MAX-HEIGHT         = 17.29
         MAX-WIDTH          = 104.4
         VIRTUAL-HEIGHT     = 17.29
         VIRTUAL-WIDTH      = 104.4
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
/* SETTINGS FOR WINDOW C-HuvGr
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-3 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-HuvGr)
THEN C-HuvGr:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "SkoTex.HuvGr"
     _FldNameList[1]   > SkoTex.HuvGr.Hg
"HuvGr.Hg" "Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > SkoTex.HuvGr.HgBeskr
"HuvGr.HgBeskr" "Beskrivning" "x(30)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-HuvGr C-HuvGr
ON END-ERROR OF C-HuvGr /* Sök Huvudgrupp */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-HuvGr C-HuvGr
ON WINDOW-CLOSE OF C-HuvGr /* Sök Huvudgrupp */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 C-HuvGr
ON MOUSE-SELECT-DBLCLICK OF BROWSE-3 IN FRAME DEFAULT-FRAME
DO:
  apply "CHOOSE":U to Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 C-HuvGr
ON RETURN OF BROWSE-3 IN FRAME DEFAULT-FRAME
DO:
  apply "CHOOSE":U to Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Avbryt C-HuvGr
ON CHOOSE OF Btn_Avbryt IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-HuvGr
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Detalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Detalj C-HuvGr
ON CHOOSE OF BUTTON-Detalj IN FRAME DEFAULT-FRAME /* Detalj */
DO:
  run v-huvgr.w (input string(recid(HuvGr)), "ENDRE").
  if Return-Value = "AVBRYT" then 
    return no-apply.
  else if return-value = "SLETTET" then
    do:
      wOk = BROWSE-3:DELETE-SELECTED-ROW(1).
      return no-apply.
    end.
  find HuvGr no-lock where
    recid(HuvGr) = int(entry(2,Return-Value)).
  assign wHuvGrRecid = recid(HuvGr).

  if entry(1,Return-Value) = "NY" then
    do:
      if BROWSE-3:FOCUSED-ROW < BROWSE-3:DOWN then
        assign wLoop = BROWSE-3:FOCUSED-ROW + 1.
      else 
        assign wLoop = BROWSE-3:FOCUSED-ROW.  
      wOk = BROWSE-3:insert-row("AFTER").
      wOk = BROWSE-3:CREATE-RESULT-LIST-ENTRY().
      wOk = BROWSE-3:SELECT-ROW(wLoop).
      
      find HuvGr no-lock where
        recid(HuvGr) = wHuvGrRecid.    
    end.
  display 
    HuvGr.Hg
    HuvGr.HgBeskr
  with browse BROWSE-3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-HuvGr 


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

  assign
    HuvGr.HgBeskr:read-only = true.
    
  apply "ENTRY":U to BROWSE-3.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
       
  if available HuvGr 
    then return string(recid(HuvGr)).
    else return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-HuvGr  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-HuvGr)
  THEN DELETE WIDGET C-HuvGr.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-HuvGr  _DEFAULT-ENABLE
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
  ENABLE BROWSE-3 BUTTON-Detalj Btn_Avbryt Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-HuvGr.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-HuvGr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

