&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
def input parameter wLevBasRecid        as recid  no-undo.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-Ordre
&Scoped-define BROWSE-NAME BROWSE-Ordre

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ordre

/* Definitions for BROWSE BROWSE-Ordre                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Ordre Ordre.OrdreNr Ordre.Merknad ~
Ordre.Notat Ordre.OrdreStatus 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Ordre 
&Scoped-define QUERY-STRING-BROWSE-Ordre FOR EACH Ordre ~
      WHERE Ordre.LevNr = LevBas.LevNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Ordre OPEN QUERY BROWSE-Ordre FOR EACH Ordre ~
      WHERE Ordre.LevNr = LevBas.LevNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Ordre Ordre
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Ordre Ordre


/* Definitions for FRAME FRAME-Ordre                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Ordre ~
    ~{&OPEN-QUERY-BROWSE-Ordre}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-Ordre BUTTON-Detaljer ~
BUTTON-Oppdater-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getResizeFrameHandle C-Win 
FUNCTION getResizeFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Detaljer 
     LABEL "Detaljer..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Oppdater-2 
     LABEL "Oppdater" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Ordre FOR 
      Ordre SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Ordre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Ordre C-Win _STRUCTURED
  QUERY BROWSE-Ordre NO-LOCK DISPLAY
      Ordre.OrdreNr FORMAT ">>>>>>>9":U
      Ordre.Merknad FORMAT "X(40)":U
      Ordre.Notat FORMAT "X(40)":U
      Ordre.OrdreStatus FORMAT ">9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137 BY 16.43 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Ordre
     BROWSE-Ordre AT ROW 1.48 COL 2
     BUTTON-Detaljer AT ROW 1.48 COL 140
     BUTTON-Oppdater-2 AT ROW 2.91 COL 140
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.9
         SIZE 154.4 BY 17.38.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 28.62
         WIDTH              = 170.8
         MAX-HEIGHT         = 28.62
         MAX-WIDTH          = 170.8
         VIRTUAL-HEIGHT     = 28.62
         VIRTUAL-WIDTH      = 170.8
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-Ordre
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-Ordre 1 FRAME-Ordre */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Ordre
/* Query rebuild information for BROWSE BROWSE-Ordre
     _TblList          = "SkoTex.Ordre"
     _Options          = "NO-LOCK"
     _Where[1]         = "SkoTex.Ordre.LevNr = LevBas.LevNr"
     _FldNameList[1]   = SkoTex.Ordre.OrdreNr
     _FldNameList[2]   = SkoTex.Ordre.Merknad
     _FldNameList[3]   > SkoTex.Ordre.Notat
"Ordre.Notat" ? "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = SkoTex.Ordre.OrdreStatus
     _Query            is OPENED
*/  /* BROWSE BROWSE-Ordre */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Ordre
/* Query rebuild information for FRAME FRAME-Ordre
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Ordre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Ordre
&Scoped-define SELF-NAME BROWSE-Ordre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Ordre C-Win
ON DEFAULT-ACTION OF BROWSE-Ordre IN FRAME FRAME-Ordre
DO:
  APPLY "CHOOSE" TO BUTTON-Detaljer.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Ordre C-Win
ON VALUE-CHANGED OF BROWSE-Ordre IN FRAME FRAME-Ordre
DO:
  ASSIGN BUTTON-Detaljer:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Detaljer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Detaljer C-Win
ON CHOOSE OF BUTTON-Detaljer IN FRAME FRAME-Ordre /* Detaljer... */
DO:
  DEF VAR wOrdreRecid AS RECID NO-UNDO.
  DEFINE VARIABLE hHandle AS HANDLE     NO-UNDO.
  ASSIGN wOrdreRecid = RECID(Ordre).
/*   RUN d-vordre.w (input-output wOrdreRecid,"ENDRE"). */
  RUN gvordre.w  (Ordre.OrdreNr).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Oppdater-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Oppdater-2 C-Win
ON CHOOSE OF BUTTON-Oppdater-2 IN FRAME FRAME-Ordre /* Oppdater */
DO:
    BROWSE {&BROWSE-NAME}:REFRESH().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
IF VALID-HANDLE(wCurrent-Window) THEN
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.             


/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  do:
/*    RUN SaveBrowseSettings. */
    if valid-handle(wParentHandle) then
      run SlettProg in wParentHandle.
    RUN disable_UI.
  end.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND LevBas WHERE RECID(LevBas) = wLevBasRecid NO-LOCK.
  RUN enable_UI.
  {lng.i}
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wRecid AS RECID NO-UNDO.

  FIND LevBas WHERE RECID(LevBas) = wRecid NO-LOCK.
  assign
    wLevBasRecid = RECID(LevBas).
  {sww.i}  
  {&OPEN-QUERY-{&BROWSE-NAME}}
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  {swn.i}

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
  HIDE FRAME FRAME-Ordre.
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
  ENABLE BROWSE-Ordre BUTTON-Detaljer BUTTON-Oppdater-2 
      WITH FRAME FRAME-Ordre.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Ordre}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTopp C-Win 
PROCEDURE MoveToTopp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   IF FRAME FRAME-Ordre:MOVE-TO-TOP() THEN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getResizeFrameHandle C-Win 
FUNCTION getResizeFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN FRAME FRAME-Ordre:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

