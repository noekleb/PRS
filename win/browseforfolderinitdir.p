&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:          folder.w
  Description:   BrowseForFolder demo
  Author:        Cyril O'Floinn

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
{proextra.i}

&Scoped-Define MAX_PATH 260

&Scoped-Define BIF_RETURNONLYFSDIRS   1  /* For finding a folder to start document searching */
&Scoped-Define BIF_DONTGOBELOWDOMAIN  2  /* For starting the Find Computer */
&Scoped-Define BIF_STATUSTEXT         4
&Scoped-Define BIF_RETURNFSANCESTORS  8
&Scoped-Define BIF_BROWSEFORCOMPUTER  4096  /* Browsing for Computers */
&Scoped-Define BIF_BROWSEFORPRINTER   8192  /* Browsing for Printers */
&Scoped-Define BIF_BROWSEINCLUDEFILES 16384 /* Browsing for Everything */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS gchr-Title Btn_Done btn-Browse gchr-Folder ~
Flags-Rect 
&Scoped-Define DISPLAYED-OBJECTS gchr-Title gchr-Folder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetBrowseFlags C-Win 
FUNCTION GetBrowseFlags RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SelectFolder C-Win 
FUNCTION SelectFolder RETURNS CHARACTER
  (Input int-hwndOwner As Int, Input chr-Title As Char, Input int-Flags As Int, Input chr-InitialDir As Char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-Browse 
     LABEL "&Browse" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Close" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE gchr-Folder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE gchr-Title AS CHARACTER FORMAT "X(256)":U INITIAL "Choose Folder" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE Flags-Rect
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 7.86.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL yes 
     LABEL "BIF_RETURNONLYFSDIRS" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "BIF_DONTGOBELOWDOMAIN" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "BIF_STATUSTEXT" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL no 
     LABEL "BIF_RETURNFSANCESTORS" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-5 AS LOGICAL INITIAL no 
     LABEL "BIF_BROWSEFORCOMPUTER" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-6 AS LOGICAL INITIAL no 
     LABEL "BIF_BROWSEFORPRINTER" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-7 AS LOGICAL INITIAL no 
     LABEL "BIF_BROWSEINCLUDEFILES" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     gchr-Title AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABEL
     Btn_Done AT ROW 2.19 COL 64
     btn-Browse AT ROW 3.62 COL 64
     gchr-Folder AT ROW 4.1 COL 3 NO-LABEL
     "Title:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 3
     "Folder:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 3.38 COL 3
     " Flags:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 5.52 COL 5
     Flags-Rect AT ROW 5.76 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106 BY 16.29
         DEFAULT-BUTTON Btn_Done.

DEFINE FRAME FRAME-FLAGS
     TOGGLE-1 AT ROW 1.24 COL 3
     TOGGLE-2 AT ROW 2.19 COL 3
     TOGGLE-3 AT ROW 3.14 COL 3
     TOGGLE-4 AT ROW 4.1 COL 3
     TOGGLE-5 AT ROW 5.05 COL 3
     TOGGLE-6 AT ROW 6 COL 3
     TOGGLE-7 AT ROW 6.95 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 6.24
         SIZE 40 BY 7.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
	Last change:  JD   29 Nov 98    8:51 pm
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "BrowseForFolder test"
         HEIGHT             = 16.29
         WIDTH              = 81.8
         MAX-HEIGHT         = 16.29
         MAX-WIDTH          = 108.4
         VIRTUAL-HEIGHT     = 16.29
         VIRTUAL-WIDTH      = 108.4
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-FLAGS:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN gchr-Folder IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-FLAGS
                                                                        */
ASSIGN 
       TOGGLE-1:PRIVATE-DATA IN FRAME FRAME-FLAGS     = 
                "{&BIF_RETURNONLYFSDIRS}".

ASSIGN 
       TOGGLE-2:PRIVATE-DATA IN FRAME FRAME-FLAGS     = 
                "{&BIF_DONTGOBELOWDOMAIN}".

ASSIGN 
       TOGGLE-3:PRIVATE-DATA IN FRAME FRAME-FLAGS     = 
                "{&BIF_STATUSTEXT}".

ASSIGN 
       TOGGLE-4:PRIVATE-DATA IN FRAME FRAME-FLAGS     = 
                "{&BIF_RETURNFSANCESTORS}".

ASSIGN 
       TOGGLE-5:PRIVATE-DATA IN FRAME FRAME-FLAGS     = 
                "{&BIF_BROWSEFORCOMPUTER}".

ASSIGN 
       TOGGLE-6:PRIVATE-DATA IN FRAME FRAME-FLAGS     = 
                "{&BIF_BROWSEFORPRINTER}".

ASSIGN 
       TOGGLE-7:PRIVATE-DATA IN FRAME FRAME-FLAGS     = 
                "{&BIF_BROWSEINCLUDEFILES}".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-FLAGS
/* Query rebuild information for FRAME FRAME-FLAGS
     _Query            is NOT OPENED
*/  /* FRAME FRAME-FLAGS */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ProUtil Test */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ProUtil Test */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Browse C-Win
ON CHOOSE OF btn-Browse IN FRAME DEFAULT-FRAME /* Browse */
DO:
    Run BrowseFolder In This-Procedure.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Close */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseFolder C-Win 
PROCEDURE BrowseFolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    Def Var chr-Folder As Char No-UnDo.
    
    Assign Frame {&FRAME-NAME} gchr-Title gchr-Folder.
    
    /* Allow the user to select a folder */                    
    chr-Folder = SelectFolder (
        {&Window-Name}:Hwnd , 
        gchr-Title, 
        GetBrowseFlags(),
        gchr-Folder
    ).
    
    /* check did the user select anything */    
    If chr-Folder <> ? Then Do:
        Assign gchr-Folder = Chr-Folder.
        Display gchr-Folder With Frame {&FRAME-NAME}.
    End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY gchr-Title gchr-Folder 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE gchr-Title Btn_Done btn-Browse gchr-Folder Flags-Rect 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY TOGGLE-1 TOGGLE-2 TOGGLE-3 TOGGLE-4 TOGGLE-5 TOGGLE-6 TOGGLE-7 
      WITH FRAME FRAME-FLAGS IN WINDOW C-Win.
  ENABLE TOGGLE-1 TOGGLE-2 TOGGLE-3 TOGGLE-4 TOGGLE-5 TOGGLE-6 TOGGLE-7 
      WITH FRAME FRAME-FLAGS IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FLAGS}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetBrowseFlags C-Win 
FUNCTION GetBrowseFlags RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    Def Var int-BrowseFlags As Int    No-UnDo.
    Def Var wh-Temp         As Widget No-UnDo.
    
    Assign
        int-BrowseFlags = 0
        wh-Temp = Frame Frame-Flags:Handle
        wh-Temp = wh-Temp:First-Child  /* first field group */
        wh-Temp = wh-Temp:First-Child. /* first toggle-box */
        
    Do While Valid-Handle(wh-Temp):
        If wh-Temp:Type = "TOGGLE-BOX":U And wh-Temp:Checked Then
            int-BrowseFlags = int-BrowseFlags + Int(wh-Temp:Private-Data).
        wh-Temp = wh-Temp:Next-Sibling.
    End.   

    Return int-BrowseFlags.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SelectFolder C-Win 
FUNCTION SelectFolder RETURNS CHARACTER
  (Input int-hwndOwner As Int, Input chr-Title As Char, Input int-Flags As Int, Input chr-InitialDir As Char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    Def Var chr-SelectFolder As Char No-UnDo.
    Def Var chr-FolderName   As Char No-UnDo.
    Def Var int-BoolResult   As Int  No-UnDo.
    
    Assign 
        chr-SelectFolder = ?.
        chr-FolderName   = Fill(" ", {&MAX_PATH}).
    
    Run BrowseForFolder In hpExtra  (
        Input  int-hwndOwner,
        Input  chr-Title,
        Input  int-Flags,
        Input  chr-InitialDir,
        Output chr-FolderName,
        Output int-BoolResult
    ).
    
    If int-BoolResult <> 0 Then
        Assign chr-SelectFolder = Trim(chr-FolderName).
        
    Return chr-SelectFolder.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
