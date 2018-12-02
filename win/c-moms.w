&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Moms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Moms 
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
def var wOk   as log no-undo.
def var wLoop as int no-undo.

/* Buffere */
def buffer bMoms for Moms.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Moms

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 Moms.MomsKod Moms.MomsProc 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 Moms.MomsKod Moms.MomsProc 
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-2~
 ~{&FP1}MomsKod ~{&FP2}MomsKod ~{&FP3}~
 ~{&FP1}MomsProc ~{&FP2}MomsProc ~{&FP3}
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 Moms
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 Moms
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH Moms NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Moms
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Moms


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 BUTTON-Ny BUTTON-Endre BUTTON-Slett ~
Btn_Avbryt Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Moms AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Avbryt AUTO-END-KEY DEFAULT 
     LABEL "&Avbryt" 
     SIZE 9 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 9 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Endre 
     LABEL "&Ändra" 
     SIZE 9 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "&Ny" 
     SIZE 9 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "&Radera" 
     SIZE 9 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Moms SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Moms _STRUCTURED
  QUERY BROWSE-2 DISPLAY
      Moms.MomsKod COLUMN-LABEL "Kod"
      Moms.MomsProc COLUMN-LABEL "Procent"
  ENABLE
      Moms.MomsKod
      Moms.MomsProc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 17.8 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-2 AT ROW 1.1 COL 2
     BUTTON-Ny AT ROW 6.05 COL 2
     BUTTON-Endre AT ROW 6.05 COL 11
     BUTTON-Slett AT ROW 6.05 COL 20
     Btn_Avbryt AT ROW 6.05 COL 31.6
     Btn_OK AT ROW 6.05 COL 40.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.2 BY 6.43.


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
  CREATE WINDOW C-Moms ASSIGN
         HIDDEN             = YES
         TITLE              = "Sök i Momsregister"
         HEIGHT             = 6.52
         WIDTH              = 50.8
         MAX-HEIGHT         = 18.71
         MAX-WIDTH          = 106.6
         VIRTUAL-HEIGHT     = 18.71
         VIRTUAL-WIDTH      = 106.6
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
/* SETTINGS FOR WINDOW C-Moms
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-2 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Moms)
THEN C-Moms:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "SkoTex.Moms"
     _FldNameList[1]   > SkoTex.Moms.MomsKod
"MomsKod" "Kod" ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[2]   > SkoTex.Moms.MomsProc
"MomsProc" "Procent" ? "decimal" ? ? ? ? ? ? yes ?
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Moms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Moms C-Moms
ON END-ERROR OF C-Moms /* Sök i Momsregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Moms C-Moms
ON WINDOW-CLOSE OF C-Moms /* Sök i Momsregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Moms
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
  APPLY "CHOOSE":U to Btn_OK in frame {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Moms
ON RETURN OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
  apply "CHOOSE":U to Btn_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Moms
ON ROW-LEAVE OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
  if {&BROWSE-NAME}:new-row in frame {&FRAME-NAME} then
    do:
      do on error undo, return no-apply:
        create Moms.
        assign 
          Moms.MomsKod  = input browse {&BROWSE-NAME} Moms.MomsKod
          Moms.MomsProc = input Browse {&BROWSE-NAME} Moms.MomsProc.
        wOk = {&BROWSE-NAME}:create-result-list-entry().
      end.
    end.
  assign
    Moms.MomsKod:read-only  in browse {&BROWSE-NAME} = true
    Moms.MomsProc:read-only in browse {&BROWSE-NAME} = true.  
  wOk = {&BROWSE-NAME}:SELECT-FOCUSED-ROW( ).  
  apply "ENTRY":U to Moms.MomsKod in browse {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Avbryt C-Moms
ON CHOOSE OF Btn_Avbryt IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Moms
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Moms
ON CHOOSE OF BUTTON-Endre IN FRAME DEFAULT-FRAME /* Ändra */
DO:
  assign
    Moms.MomsProc:read-only in browse {&BROWSE-NAME} = false.
  apply "ENTRY":U to {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Moms
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  assign
    Moms.MomsKod:read-only  in browse {&BROWSE-NAME} = false
    Moms.MomsProc:read-only in browse {&BROWSE-NAME} = false.
  wOk = {&BROWSE-NAME}:insert-row("AFTER").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Moms
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  assign wOk = false.

  message "Skal momskoden slettes?" view-as alert-box 
    QUESTION BUTTONS YES-NO
    title "Bekreftelse"
    update wOk.
  if wOk = false then
    return no-apply "AVBRYT".  

  do wLoop = {&BROWSE-NAME}:num-selected-rows to 1 by -1:
    wOk = {&BROWSE-NAME}:fetch-selected-row(wLoop).
    get current {&BROWSE-NAME} exclusive-lock.
    
    if can-find(first VarGr where
                  VarGr.MomsKod = Moms.MomsKod) then
      do:
        message "Momskoden er i bruk p† en eller flere varegrupper!"
          view-as alert-box
          title "Advarsel".
        return no-apply.
      end.   
    
    delete Moms.
  end.
  wOk = {&BROWSE-NAME}:DELETE-SELECTED-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Moms 


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

find first bMoms no-lock no-error.
if not available bMoms then
  do transaction:
    create bMoms.
    assign bMoms.MomsKod  = 1
           bMoms.MomsProc = 23. 
  end. /* TRANSACTION */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  assign
    Moms.MomsKod:read-only = true
    Moms.MomsProc:read-only = true.

  apply "ENTRY":U to BROWSE-2.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

  if available Moms
    then return string(recid(Moms)).
  else
    return "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Moms _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Moms)
  THEN DELETE WIDGET C-Moms.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Moms _DEFAULT-ENABLE
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
  ENABLE BROWSE-2 BUTTON-Ny BUTTON-Endre BUTTON-Slett Btn_Avbryt Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Moms.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Moms.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


