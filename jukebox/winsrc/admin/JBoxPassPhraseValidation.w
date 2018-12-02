&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwPassPhrase tbPassPhrase ~
cPassPhraseResolver cDescription bUseUniqueKey iLimitUniqKeySearch ~
cPassPhraseGeneration dCreated cCreatedBy dModified cModifiedBy 
&Scoped-Define DISPLAYED-OBJECTS cPassPhraseResolver cDescription ~
bUseUniqueKey iLimitUniqKeySearch cPassPhraseGeneration dCreated cCreatedBy ~
dModified cModifiedBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cPassPhraseGeneration AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 96 BY 4.05.

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cDescription AS CHARACTER FORMAT "x(40)" 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cModifiedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cPassPhraseResolver AS CHARACTER FORMAT "x(40)" 
     LABEL "Pass.phrase reslolver" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dModified AS DATE FORMAT "99/99/9999" 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE iLimitUniqKeySearch AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Limit for unique key" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1 TOOLTIP "Limit for unique key search in days (last n days..)".

DEFINE RECTANGLE brwPassPhrase
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY 8.1.

DEFINE RECTANGLE tbPassPhrase
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE bUseUniqueKey AS LOGICAL INITIAL no 
     LABEL "Unique key" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.6 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cPassPhraseResolver AT ROW 11.1 COL 22.2 COLON-ALIGNED WIDGET-ID 14
     cDescription AT ROW 12.38 COL 22 COLON-ALIGNED WIDGET-ID 8
     bUseUniqueKey AT ROW 13.57 COL 24.4 WIDGET-ID 26
     iLimitUniqKeySearch AT ROW 13.57 COL 60.4 COLON-ALIGNED WIDGET-ID 24
     cPassPhraseGeneration AT ROW 14.81 COL 24 NO-LABEL WIDGET-ID 20
     dCreated AT ROW 19.14 COL 22.2 COLON-ALIGNED WIDGET-ID 16
     cCreatedBy AT ROW 19.14 COL 43.2 COLON-ALIGNED WIDGET-ID 6
     dModified AT ROW 19.14 COL 83 COLON-ALIGNED WIDGET-ID 18
     cModifiedBy AT ROW 19.14 COL 104.2 COLON-ALIGNED WIDGET-ID 10
     brwPassPhrase AT ROW 2.67 COL 2 WIDGET-ID 2
     tbPassPhrase AT ROW 1.24 COL 2.6 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 126.8 BY 19.33 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Insert window title>"
         HEIGHT             = 19.33
         WIDTH              = 126.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 127.4
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 127.4
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
       FRAME DEFAULT-FRAME:HEIGHT           = 19.33
       FRAME DEFAULT-FRAME:WIDTH            = 126.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
cPassPhraseResolver:READ-ONLY IN FRAME {&FRAME-NAME} = YES.
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
  DISPLAY cPassPhraseResolver cDescription bUseUniqueKey iLimitUniqKeySearch 
          cPassPhraseGeneration dCreated cCreatedBy dModified cModifiedBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwPassPhrase tbPassPhrase cPassPhraseResolver cDescription 
         bUseUniqueKey iLimitUniqKeySearch cPassPhraseGeneration dCreated 
         cCreatedBy dModified cModifiedBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwPassPhrase:HANDLE
          ,100
          ,""
          ,"JBoxPassPhraseValidation"
          + ";cDescription"
          + ";cPassPhraseResolver"
         	+ ";cPassPhraseGeneration"
          + ";iLimitUniqKeySearch"
          + ";bUseUniqueKey"
        	+ ";dCreated|Created"
        	+ ";cCreatedBy|Cre.by"
        	+ ";dModified|Modified"
        	+ ";cModifiedBy|Mod.by"
          ,"WHERE false"
          ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"cPassPhraseResolver,cDescription,cPassPhraseGeneration,bUseUniqueKey,iLimitUniqKeySearch",""
          ,"cCreatedBy,cModifiedBy,dCreated,dModified",""
          ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
          ,tbPassPhrase:HANDLE
          ,"Fil"
          ,"new,copy,undo,delete,save,excel"
          ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  RUN InvokeMethod(hBrowse,"OpenQuery").

END.


DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "brwPassPhrase," + hBrowse:NAME).
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,300,350,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

