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
def input parameter wAvdelingRecid        as recid  no-undo.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Local Variable Definitions ---                                       */

DEFINE VAR wMomsProc LIKE Moms.MomsProc NO-UNDO.
DEFINE VAR wDummy AS CHAR LABEl ""     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-Avdeling
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES HuvGr

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 HuvGr.Hg HuvGr.HgBeskr wDummy 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH HuvGr ~
      WHERE HuvGr.AvdelingNr = Avdeling.AvdelingNr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 HuvGr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 HuvGr


/* Definitions for FRAME FRAME-Avdeling                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Avdeling ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 BUTTON-Detaljer BUTTON-Oppdater-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Moms C-Win 
FUNCTION Moms RETURNS CHARACTER
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
DEFINE QUERY BROWSE-2 FOR 
      HuvGr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      HuvGr.Hg FORMAT ">>>9":U
      HuvGr.HgBeskr FORMAT "x(40)":U
      wDummy
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137 BY 16.43 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Avdeling
     BROWSE-2 AT ROW 1.48 COL 2
     BUTTON-Detaljer AT ROW 1.67 COL 140
     BUTTON-Oppdater-2 AT ROW 3.1 COL 140
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.91
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
         TITLE              = "Hovedgrupper i avdeling"
         HEIGHT             = 27.48
         WIDTH              = 156.4
         MAX-HEIGHT         = 27.48
         MAX-WIDTH          = 156.4
         VIRTUAL-HEIGHT     = 27.48
         VIRTUAL-WIDTH      = 156.4
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
/* SETTINGS FOR FRAME FRAME-Avdeling
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-2 1 FRAME-Avdeling */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "SkoTex.HuvGr"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "SkoTex.HuvGr.AvdelingNr = Avdeling.AvdelingNr"
     _FldNameList[1]   = SkoTex.HuvGr.Hg
     _FldNameList[2]   > SkoTex.HuvGr.HgBeskr
"HgBeskr" ? "x(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > "_<CALC>"
"wDummy" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Avdeling
/* Query rebuild information for FRAME FRAME-Avdeling
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Avdeling */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Hovedgrupper i avdeling */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Hovedgrupper i avdeling */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME FRAME-Avdeling
DO:
  APPLY "CHOOSE" TO BUTTON-Detaljer.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME FRAME-Avdeling
DO:
  ASSIGN BUTTON-Detaljer:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Detaljer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Detaljer C-Win
ON CHOOSE OF BUTTON-Detaljer IN FRAME FRAME-Avdeling /* Detaljer... */
DO:
  DEFINE VAR wPrgHandle AS HANDLE NO-UNDO.
  RUN w-vhuvgr.w PERSISTENT SET wPrgHandle (RECID(huvgr),"ENDRE").
  RUN SkapaTmpChild IN wParentHandle (wPrgHandle).
/*  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Oppdater-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Oppdater-2 C-Win
ON CHOOSE OF BUTTON-Oppdater-2 IN FRAME FRAME-Avdeling /* Oppdater */
DO:
    BROWSE {&BROWSE-NAME}:REFRESH().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  FIND Avdeling WHERE RECID(Avdeling) = wAvdelingRecid NO-LOCK.
  RUN enable_UI.
  {lng.i} /* Oversettelse */
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

  FIND Avdeling WHERE RECID(Avdeling) = wRecid NO-LOCK.
  assign
    wAvdelingRecid = RECID(Avdeling).
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
  HIDE FRAME FRAME-Avdeling.
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
  ENABLE BROWSE-2 BUTTON-Detaljer BUTTON-Oppdater-2 
      WITH FRAME FRAME-Avdeling.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Avdeling}
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
  
   IF FRAME FRAME-Avdeling:MOVE-TO-TOP() THEN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Moms C-Win 
FUNCTION Moms RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

