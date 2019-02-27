&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
                          
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnLaunchFileName iJBoxMenuId cMenuType ~
cMenuLabel cAccelerator cLaunch iJBoxProgramId cLaunchtype iSeq cParameter ~
bMultiple 
&Scoped-Define DISPLAYED-OBJECTS iJBoxMenuId cMenuType cMenuLabel ~
cAccelerator cLaunch iJBoxProgramId cLaunchtype iSeq cParameter bMultiple 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnLaunchFileName 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Klient filnavn" 
     SIZE 4 BY 1.

DEFINE VARIABLE cLaunchtype AS CHARACTER FORMAT "X(256)":U INITIAL "START-WINDOW" 
     LABEL "Launch type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "PROCEDURE","START-WINDOW","THIS-PROCEDURE","DATA-BROWSE" 
     DROP-DOWN-LIST
     SIZE 27.2 BY 1 NO-UNDO.

DEFINE VARIABLE cAccelerator AS CHARACTER FORMAT "x(8)" 
     LABEL "Accelerator" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cLaunch AS CHARACTER FORMAT "x(40)" 
     LABEL "Launch" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "MENU-ITEM: Program file or internal procedure to run. MENUBAR: Menu program".

DEFINE VARIABLE cMenuLabel AS CHARACTER FORMAT "x(40)" 
     LABEL "Menu label" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cMenuType AS CHARACTER FORMAT "x(12)" 
     LABEL "Menu type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE cParameter AS CHARACTER FORMAT "x(30)" 
     LABEL "Parameter" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1.

DEFINE VARIABLE iJBoxMenuId AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Menu id" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE iJBoxProgramId AS INTEGER FORMAT "->>>9" INITIAL 0 
     LABEL "Prog.Id" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE iSeq AS INTEGER FORMAT "->>9":U INITIAL 0 
     LABEL "Sequence" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Display sequence" NO-UNDO.

DEFINE VARIABLE bMultiple AS LOGICAL INITIAL no 
     LABEL "Allow multiple instances" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnLaunchFileName AT ROW 5.19 COL 57
     iJBoxMenuId AT ROW 1.48 COL 69 COLON-ALIGNED
     cMenuType AT ROW 1.71 COL 12.8 COLON-ALIGNED
     cMenuLabel AT ROW 2.91 COL 12.8 COLON-ALIGNED
     cAccelerator AT ROW 4.05 COL 12.8 COLON-ALIGNED
     cLaunch AT ROW 5.19 COL 12.8 COLON-ALIGNED
     iJBoxProgramId AT ROW 5.19 COL 68.4 COLON-ALIGNED
     cLaunchtype AT ROW 6.33 COL 12.8 COLON-ALIGNED
     iSeq AT ROW 7.48 COL 12.8 COLON-ALIGNED
     cParameter AT ROW 8.57 COL 12.8 COLON-ALIGNED
     bMultiple AT ROW 8.67 COL 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SCROLLABLE SIZE 80 BY 9.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer"
         HEIGHT             = 9.52
         WIDTH              = 80
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 9.48
       FRAME DEFAULT-FRAME:WIDTH            = 80.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLaunchFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLaunchFileName C-Win
ON CHOOSE OF btnLaunchFileName IN FRAME DEFAULT-FRAME /* Klient filnavn */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Source files" "*.w *.r" 
                MUST-EXIST
                UPDATE bOk.
  IF bOK THEN DO:  
    cLaunch:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\").
    IF SUBSTR(cLaunch:SCREEN-VALUE,LENGTH(cLaunch:SCREEN-VALUE) - 1) = ".r" THEN
      cLaunch:SCREEN-VALUE = SUBSTR(cLaunch:SCREEN-VALUE,LENGTH(cLaunch:SCREEN-VALUE) - 1) + ".w".
    APPLY "any-printable" TO cLaunch.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
  PUBLISH "InvalidateHandle".
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

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hFieldMap}

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY iJBoxMenuId cMenuType cMenuLabel cAccelerator cLaunch iJBoxProgramId 
          cLaunchtype iSeq cParameter bMultiple 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnLaunchFileName iJBoxMenuId cMenuType cMenuLabel cAccelerator 
         cLaunch iJBoxProgramId cLaunchtype iSeq cParameter bMultiple 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bParameterInstalled AS LOG NO-UNDO.
DEF VAR bMultipleInstalled  AS LOG NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  bParameterInstalled = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxMenu","cParameter").
  bMultipleInstalled = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxMenu","bMultiple").

  IF NOT bParameterInstalled THEN
    cParameter:HIDDEN = YES.
  IF NOT bMultipleInstalled THEN
    bMultiple:HIDDEN = YES.

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME {&FRAME-NAME}:HANDLE,
                             "cMenuLabel,cAccelerator,cLaunch,cLaunchType,iSeq"
                          + (IF bParameterInstalled THEN ",cParameter" ELSE "")
                          + (IF bMultipleInstalled THEN ",bMultiple" ELSE "")
                            ,"", 
                             "cMenuType,iJBoxProgramId,iJBoxMenuId","",
                             "btnLaunchFileName").          

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,THIS-PROCEDURE).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

