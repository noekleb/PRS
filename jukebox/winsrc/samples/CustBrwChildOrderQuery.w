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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR hToolbar     AS HANDLE NO-UNDO.
DEF VAR hTabFolder   AS HANDLE NO-UNDO.
DEF VAR iCurrTab     AS INT    NO-UNDO INIT 1.

DEF VAR hDebug       AS HANDLE NO-UNDO.
DEF VAR hCurrTabProc AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwCustomer tbCustomer CustomerFolder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 119 BY 9.52.

DEFINE RECTANGLE CustomerFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 119 BY 20.

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwCustomer AT ROW 2.67 COL 3
     tbCustomer AT ROW 1.48 COL 3
     CustomerFolder AT ROW 12.67 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122.2 BY 31.95.


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
         HEIGHT             = 31.95
         WIDTH              = 122.2
         MAX-HEIGHT         = 31.95
         MAX-WIDTH          = 137
         VIRTUAL-HEIGHT     = 31.95
         VIRTUAL-WIDTH      = 137
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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

{incl/conttrigg.i hBrowse}
{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ON "WINDOW-RESIZED" OF {&WINDOW-NAME} DO:    
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
  RUN MoveToTop IN hCurrTabProc.
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
  ENABLE brwCustomer tbCustomer CustomerFolder 
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
DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
  ,BrwCustomer:HANDLE
  ,100
  ,""
  ,"Customer"
   + ";CustNum"
   + ";Name"
   + ";Address"
   + ";Country"
   + ";Phone"
   + ";Fax"
  ,"WHERE false"
  ,"").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
  ,tbCustomer:HANDLE
  ,"File"
  ,"filter,excel"
  ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).


  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",CustomerFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Customer|CustView.w|Orders|OrderBrw.w|Order|OrderView2.w",hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).
END.

InitializeResize().

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

RUN InvokeMethod(hBrowse,"OpenQuery").

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,1000,100,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hPageFrame AS HANDLE NO-UNDO.
DEF VAR hPageQuery AS HANDLE NO-UNDO.

/* Frames in the tab-folder will be adjusted according to the tab-folder rectangle so it is very important to set the rule for it: */
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"CustomerFolder").

/* Also the tab-folder must here be told how to behave when moved in Y-direction: */
DYNAMIC-FUNCTION("setTabMoveY" IN hTabFolder).

/* hPageFrame = DYNAMIC-FUNCTION("getAnyWidget",THIS-PROCEDURE:CURRENT-WINDOW,0,"frmTabFrame"). */
/* MESSAGE PROGRAM-NAME(1) SKIP                                                                 */
/*         hPageFrame:NAME                                                                      */
/*         VIEW-AS ALERT-BOX.                                                                   */

/* DYNAMIC-FUNCTION("setWidgetWatch",DYNAMIC-FUNCTION("getAnyWidget",THIS-PROCEDURE:CURRENT-WINDOW,0,"frmTabFrame"),"c:\temp\tabwatch3.log").  */

/* Also the frames on each tab must have their rules set: 
   Note the debug option */
DO ix = 1 TO 3:
  hPageFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,ix).
  DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, hPageFrame,
                   DYNAMIC-FUNCTION("getSiblingNames",hPageFrame:FIRST-CHILD:FIRST-CHILD,"","","")).

  IF ix = 1 THEN       
    DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, hPageFrame,
                     "Comments," + hPageFrame:NAME).
  ELSE IF ix = 2 THEN DO:
    hPageQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2).  
    DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW,hPageFrame,hPageFrame:NAME + "," + hPageQuery:NAME).
    DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, hPageFrame,hPageQuery:NAME).
  END.
  ELSE IF ix = 3 THEN DO:
    DYNAMIC-FUNCTION("setWidgetWatch",DYNAMIC-FUNCTION("getAnyWidget",THIS-PROCEDURE:CURRENT-WINDOW,0,"frmOrderBrw"),"c:\temp\tabwatch3.log").
    DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW,hPageFrame,"edWarehouse," + hPageQuery:NAME).
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTabQuery     AS HANDLE NO-UNDO.

hTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab).

DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hTabQuery).

hTabQuery    = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab).
hCurrTabProc = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab).

IF iiTab = 1 THEN
  DYNAMIC-FUNCTION("CreateOneToOneLink",hTabQuery,hBrowse,"custnum").
ELSE 
  DYNAMIC-FUNCTION("CreateParentLink",hTabQuery,hBrowse,"custnum").

iCurrTab = iiTab.

APPLY "value-changed" TO hBrowse.



RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

