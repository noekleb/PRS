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
&Scoped-define FRAME-NAME FRAME-LevSort
&Scoped-define BROWSE-NAME BROWSE-LevSort

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LevSort

/* Definitions for BROWSE BROWSE-LevSort                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-LevSort LevSort.SortID ~
LevSort.Beskrivelse LevSort.StrTypeID intervall() fordeling() 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-LevSort 
&Scoped-define QUERY-STRING-BROWSE-LevSort FOR EACH LevSort ~
      WHERE LevSort.LevNr = LevBas.LevNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-LevSort OPEN QUERY BROWSE-LevSort FOR EACH LevSort ~
      WHERE LevSort.LevNr = LevBas.LevNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-LevSort LevSort
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-LevSort LevSort


/* Definitions for FRAME FRAME-LevSort                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-LevSort ~
    ~{&OPEN-QUERY-BROWSE-LevSort}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-LevSort B-KonvTbl BUTTON-Ny ~
BUTTON-Endre BUTTON-Slett 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Fordeling C-Win 
FUNCTION Fordeling RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetResizeFrameHandle C-Win 
FUNCTION GetResizeFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Intervall C-Win 
FUNCTION Intervall RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-KonvTbl 
     LABEL "Konverteringstabell..." 
     SIZE 22 BY 1.1.

DEFINE BUTTON BUTTON-Endre 
     LABEL "Endre..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Ny 
     LABEL "Ny..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "Slett" 
     SIZE 22 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-LevSort FOR 
      LevSort SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-LevSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-LevSort C-Win _STRUCTURED
  QUERY BROWSE-LevSort NO-LOCK DISPLAY
      LevSort.SortID FORMAT "X(12)":U
      LevSort.Beskrivelse FORMAT "X(30)":U
      LevSort.StrTypeID FORMAT "zzzzz9":U WIDTH 8
      intervall() COLUMN-LABEL "Inndeling" FORMAT "x(15)":U
      fordeling() COLUMN-LABEL "Fordeling" FORMAT "x(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 130 BY 16.43 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-LevSort
     BROWSE-LevSort AT ROW 1.48 COL 2
     B-KonvTbl AT ROW 1.48 COL 133
     BUTTON-Ny AT ROW 4.1 COL 133
     BUTTON-Endre AT ROW 5.38 COL 133
     BUTTON-Slett AT ROW 6.71 COL 133
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
         TITLE              = "<insert window title>"
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-LevSort
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-LevSort 1 FRAME-LevSort */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-LevSort
/* Query rebuild information for BROWSE BROWSE-LevSort
     _TblList          = "SkoTex.LevSort"
     _Options          = "NO-LOCK"
     _Where[1]         = "SkoTex.LevSort.LevNr = LevBas.LevNr"
     _FldNameList[1]   = SkoTex.LevSort.SortID
     _FldNameList[2]   = SkoTex.LevSort.Beskrivelse
     _FldNameList[3]   > SkoTex.LevSort.StrTypeID
"LevSort.StrTypeID" ? ? "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"intervall()" "Inndeling" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"fordeling()" "Fordeling" "x(50)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-LevSort */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-LevSort
/* Query rebuild information for FRAME FRAME-LevSort
     _Query            is NOT OPENED
*/  /* FRAME FRAME-LevSort */
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


&Scoped-define SELF-NAME B-KonvTbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvTbl C-Win
ON CHOOSE OF B-KonvTbl IN FRAME FRAME-LevSort /* Konverteringstabell... */
DO:
  if not available LevSort then
    return.
  run d-bimpkonv.w (1009,
                    string(LevSort.LevNr) + ";" + string(LevSort.SortId),
                    "Kobling av leverandørsinndeling"
                    ).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-LevSort
&Scoped-define SELF-NAME BROWSE-LevSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevSort C-Win
ON DEFAULT-ACTION OF BROWSE-LevSort IN FRAME FRAME-LevSort
DO:
  APPLY "CHOOSE" TO BUTTON-Endre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-LevSort C-Win
ON VALUE-CHANGED OF BROWSE-LevSort IN FRAME FRAME-LevSort
DO:
  ASSIGN BUTTON-Endre:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME FRAME-LevSort /* Endre... */
DO:
  DEFINE VAR wPrgHandle AS HANDLE NO-UNDO.
  DEFINE VAR wLevSort   AS RECID NO-UNDO.
  ASSIGN wLevSort = RECID(LevSort).
  /* Parameter 4 används vid registrering från w-gridord,
     för att ange StrType */
  RUN d-vlevsort.w (INPUT-OUTPUT wLevSort,"ENDRE",RECID(LevBas),?).
  BROWSE {&BROWSE-NAME}:REFRESH().
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Win
ON CHOOSE OF BUTTON-Ny IN FRAME FRAME-LevSort /* Ny... */
DO:
  DEFINE VAR wPrgHandle AS HANDLE NO-UNDO.
  DEFINE VAR wLevSort   AS RECID NO-UNDO.
  /* Parameter 4 används vid registrering från w-gridord,
     för att ange StrType */
  RUN d-vlevsort.w (INPUT-OUTPUT wLevSort,"NY",RECID(LevBas),?).
  IF wLevSort <> ? THEN DO:
      {&OPEN-QUERY-{&BROWSE-NAME}}
      FIND LevSort NO-LOCK WHERE RECID(LevSort) = wLevSort.
      REPOSITION {&BROWSE-NAME} TO RECID wLevSort.
  END.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}. 
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME FRAME-LevSort /* Slett */
DO:
  IF CAN-FIND(FIRST VareBehLinjeTrans WHERE
              VareBehLinjeTrans.Kode = LevSort.SortId) THEN
  DO:
      MESSAGE "Leverandørsortimentet er i bruk på en messeordre." SKIP
              "Det kan ikke slettes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  MESSAGE "Vill du slette sortimentsposten? " VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO UPDATE wSvar AS LOGI.
  IF wSvar THEN DO:
      FIND CURRENT LevSort EXCLUSIVE.
      FOR EACH LevSAnt OF LevSort:
          DELETE LevSAnt.
      END.
      DELETE LevSort.
      BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
  END.                
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

IF VALID-HANDLE(wCurrent-Window)  THEN
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.             


DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.


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
  HIDE FRAME FRAME-LevSort.
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
  ENABLE BROWSE-LevSort B-KonvTbl BUTTON-Ny BUTTON-Endre BUTTON-Slett 
      WITH FRAME FRAME-LevSort.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LevSort}
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
  
   IF FRAME FRAME-LevSort:MOVE-TO-TOP() THEN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Fordeling C-Win 
FUNCTION Fordeling RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wTekst as char no-undo.
  if LevSort.Fri = false then
  for each LevSAnt of LevSort no-lock
    break by LevSant.LevNr
          by LevSAnt.SortId 
          by LevSAnt.SeqNr:
    wTekst = wTekst + 
             (if wTekst = "" 
                then ""
                else " ") + 
             string(LevSAnt.SoAnt).
  end.

  RETURN wTekst.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetResizeFrameHandle C-Win 
FUNCTION GetResizeFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 
  RETURN FRAME Frame-LevSort:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Intervall C-Win 
FUNCTION Intervall RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wTekst as char no-undo.

  for each LevSAnt of LevSort no-lock
    break by LevSant.LevNr
          by LevSAnt.SortId 
          by LevSAnt.SeqNr:
    if first(LevSAnt.SeqNr) then
      wTekst = LevSAnt.SoStorl.
    if last(LevSAnt.SeqNr) then
      wTekst = wTekst + " - " + LevSAnt.SoStorl.
  end.

  RETURN wTekst.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

