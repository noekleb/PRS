&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

DEFINE VAR ch_Grid AS COM-HANDLE NO-UNDO.
DEFINE VAR wMDrow  AS INTE       NO-UNDO.
DEFINE VAR wMDcol  AS INTE       NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE Overfor
        FIELD FraButik AS CHAR LABEL "Från butik"
        FIELD TilButik AS CHAR LABEL "Till butik"
        FIELD FraStrl  AS CHAR LABEL "Från Strl"
        FIELD TilStrl  AS CHAR LABEL "Till Strl"
        FIELD Antal    AS INTE LABEL "Antal"
        INDEX FraButik IS PRIMARY FraButik ASCENDING
                                  FraStrl  ASCENDING
        INDEX TilButik TilButik ASCENDING
                       TilStrl  ASCENDING.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "adeicon\brws":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Lista överföringar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-1 AT ROW 2.91 COL 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 19.48.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 19.48
         WIDTH              = 89.6
         MAX-HEIGHT         = 19.48
         MAX-WIDTH          = 89.6
         VIRTUAL-HEIGHT     = 19.48
         VIRTUAL-WIDTH      = 89.6
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 2.67
       COLUMN       = 3
       HEIGHT       = 14.76
       WIDTH        = 63
       HIDDEN       = no
       SENSITIVE    = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {C5DE3F86-3376-11d2-BAA4-04F205C10000} type: vsFlexGrid */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  RUN d-boverfor.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.MouseDown
PROCEDURE CtrlFrame.vsFlexGrid.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.

ASSIGN wMDRow = ch_grid:MouseRow
       wMDCol = ch_grid:MouseCol
       ch_Grid:MousePointer = 
       IF wMDRow = 0 OR wMDCol = 0 OR INT(ch_grid:Cell(0,wMDRow,wMDCol,wMDRow,wMDCol)) = 0 then 0 else 5.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.MouseUp
PROCEDURE CtrlFrame.vsFlexGrid.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.

DEFINE VAR             wMUrow   AS INTEGER NO-UNDO.
DEFINE VAR             wMUcol   AS INTEGER NO-UNDO.
DEFINE VAR             w-Antal  AS INTEGER NO-UNDO.
DEFINE VAR             wFraButik  AS CHAR    NO-UNDO.
DEFINE VAR             wFraStrl AS CHAR    NO-UNDO.
DEFINE VAR             wTilButik  AS CHAR    NO-UNDO.
DEFINE VAR             wTilStrl AS CHAR    NO-UNDO.
ASSIGN wMURow = ch_grid:MouseRow
       wMUCol = ch_grid:MouseCol.
    IF (wMDRow = wMURow and wMDCol = wMUCol) OR 
                 ch_Grid:MousePointer = 0 OR
                 wMUCol = 0 OR wMURow = 0 OR
                 ch_grid:Cell(0,wMURow,wMUCol,wMURow,wMUCol) = "" THEN DO:
           ASSIGN ch_Grid:MousePointer = 0.
           RETURN NO-APPLY.
    END.
    ASSIGN w-Antal = INT(ch_grid:Cell(0,wMDRow,wMDCol,wMDRow,wMDCol))
           wFraButik  = ch_grid:Cell(0,wMDRow,0,wMDRow,0)
           wFraStrl = ch_grid:Cell(0,0,wMDCol,0,wMDCol)
           wTilButik  = ch_grid:Cell(0,wMURow,0,wMURow,0)
           wTilStrl = ch_grid:Cell(0,0,wMUCol,0,wMUCol).
    IF wFraStrl <> wTilStrl AND wFraButik <> wTilButik THEN DO.
        MESSAGE "Överföring från en storlek till en annan" skip
                "mellan två butiker är inte tillåtet!" VIEW-AS ALERT-BOX.
        ASSIGN ch_Grid:MousePointer = 0.
        RETURN NO-APPLY.
    END.    
    RUN d-antal.w (INPUT-OUTPUT w-Antal,wFraButik,wFraStrl,wTilButik,wTilStrl).
    IF RETURN-VALUE = "<Avbryt>" OR w-Antal = 0 THEN DO:
        ASSIGN ch_Grid:MousePointer = 0.
        RETURN NO-APPLY.
    END.
    ASSIGN ch_grid:Cell(0,wMURow,wMUCol,wMURow,wMUCol) =
           STRING(INT(ch_grid:Cell(0,wMURow,wMUCol,wMURow,wMUCol)) + w-Antal)
           ch_grid:Cell(0,wMDRow,wMDCol,wMDRow,wMDCol) = string(int(ch_grid:Cell(0,wMDRow,wMDCol,wMDRow,wMDCol)) - w-Antal)
           ch_Grid:MousePointer = 0.
    IF wFraButik = wTilButik THEN DO:
        FIND Overfor WHERE Overfor.FraButik = wFraButik      AND
                           Overfor.TilButik = wTilButik      AND 
                           (Overfor.FraStrl  = wFraStrl   OR
                             Overfor.FraStrl  = wTilStrl)  AND
                           (Overfor.TilStrl   = wFraStrl    OR
                             Overfor.TilStrl  = wTilStrl) NO-ERROR.
        IF AVAIL Overfor THEN DO:
            IF Overfor.FraStrl = wFraStrl THEN DO:
                ASSIGN Overfor.Antal = Overfor.Antal + w-Antal.
            END.
            ELSE DO:
                IF Overfor.Antal = w-Antal THEN
                    DELETE Overfor.
                ELSE IF Overfor.Antal > w-Antal THEN
                    ASSIGN Overfor.Antal = Overfor.Antal - w-Antal.
                ELSE
                    ASSIGN Overfor.FraStrl = wFraStrl
                           Overfor.TilStrl = wTilStrl
                           Overfor.Antal = w-Antal - Overfor.Antal.
            END.
        END.
        ELSE DO:
            CREATE Overfor.
            ASSIGN Overfor.FraButik = wFraButik
                   Overfor.FraStrl  = wFraStrl
                   Overfor.TilButik = wTilButik
                   Overfor.TilStrl  = wTilStrl
                   Overfor.Antal    = w-Antal.
        END.
    END.
    ELSE DO:
        FIND Overfor WHERE (Overfor.FraButik = wFraButik  OR
                            Overfor.FraButik = wTilButik)  AND
                           (Overfor.TilButik = wTilButik  OR 
                            Overfor.TilButik = wFraButik)  AND 
                           Overfor.FraStrl  = wFraStrl AND
                           Overfor.TilStrl  = wTilStrl NO-ERROR.
        IF AVAIL Overfor THEN DO:
            IF Overfor.FraButik = wFraButik THEN DO:
                ASSIGN Overfor.Antal = Overfor.Antal + w-Antal.
            END.
            ELSE DO:
                IF Overfor.Antal = w-Antal THEN
                    DELETE Overfor.
                ELSE IF Overfor.Antal > w-Antal THEN
                    ASSIGN Overfor.Antal = Overfor.Antal - w-Antal.
                ELSE
                    ASSIGN Overfor.FraButik = wFraButik
                           Overfor.TilButik = wTilButik
                           Overfor.Antal = w-Antal - Overfor.Antal.
            END.
        END.
        ELSE DO:
            CREATE Overfor.
            ASSIGN Overfor.FraButik = wFraButik
                   Overfor.FraStrl  = wFraStrl
                   Overfor.TilButik = wTilButik
                   Overfor.TilStrl  = wTilStrl
                   Overfor.Antal    = w-Antal.
        END.
    END.
END PROCEDURE.

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
{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
{genlib.i
  &NoLibCall      = "Nei"
  &WindowName     = "Lager2"
  &PreIClose      = " "
  &PostIClose     = " "
  &PostDisable_ui = " "
}


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-lager2.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-lager2.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  ENABLE BUTTON-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
  ch_grid = chCtrlFrame:vsFlexGrid
/*  ch_Grid:AllowBigSelection = FALSE */
  ch_Grid:AllowSelection = FALSE
  ch_grid:Rows = 10
  ch_grid:Cols = 3.
  ch_grid:Cell(0,1,0,1,0) = 100.
  ch_grid:Cell(0,2,0,2,0) = 200.
  ch_grid:Cell(0,3,0,3,0) = 300.
  ch_grid:Cell(0,1,1,1,1) = 12.
  ch_grid:Cell(0,2,1,2,1) = 22.
  ch_grid:Cell(0,3,1,3,1) = 34.
  ch_grid:Cell(0,1,2,1,2) = 1.
  ch_grid:Cell(0,2,2,2,2) = 2.
  ch_grid:Cell(0,3,2,3,2) = 3.
  ch_grid:Cell(2,0,1,0,2) = 4.
  ch_grid:Cell(0,0,1,0,1) = "34.5".
  ch_grid:Cell(0,0,2,0,2) = "35".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

