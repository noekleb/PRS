&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Sample for button panel

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           June 2008

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
/*&SCOPED-DEFINE AdvGuiWin*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hPanelHor   AS HANDLE NO-UNDO.
DEF VAR hPanelVert  AS HANDLE NO-UNDO.
DEF VAR iStatus     AS INT    NO-UNDO.
DEF VAR bMarkRow    AS LOG    NO-UNDO.
DEF VAR iMarkColor  AS INT    NO-UNDO.

/* Include these if need to set properties directly on the panel, see setPanelProperties function */
{JukeBoxControlsGeneral.i}
{JukeBoxPanel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Panel BrwCustomer Toolbar HorizontalPanel ~
CustNum Name 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPanelProperties C-Win 
FUNCTION setPanelProperties RETURNS LOGICAL
  ( INPUT ihPanel     AS HANDLE,   /* Handle to panel proc */
    INPUT ihPanelRect AS HANDLE,   /* Handle to rectangle */
    INPUT iiPanel     AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Please enter a customer number.".

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter a name.".

DEFINE RECTANGLE BrwCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 117 BY 6.91.

DEFINE RECTANGLE HorizontalPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.4 BY 1.05.

DEFINE RECTANGLE Panel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 5.48.

DEFINE RECTANGLE Toolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CustNum AT ROW 9.86 COL 12 COLON-ALIGNED HELP
          "Please enter a customer number."
     Name AT ROW 10.91 COL 12 COLON-ALIGNED HELP
          "Please enter a name."
     Panel AT ROW 2.43 COL 120
     BrwCustomer AT ROW 2.43 COL 2
     Toolbar AT ROW 1.33 COL 2.6
     HorizontalPanel AT ROW 1.29 COL 38.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 147.6 BY 11.38.


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
         TITLE              = "Sample code for use of panel buttons"
         HEIGHT             = 11.33
         WIDTH              = 147.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 147.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 147.6
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
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 147.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sample code for use of panel buttons */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sample code for use of panel buttons */
DO:
  /* This event will close the window and terminate the procedure.  */
  MESSAGE 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
  DISPLAY CustNum Name 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Panel BrwCustomer Toolbar HorizontalPanel CustNum Name 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HorizButt1Record C-Win 
PROCEDURE HorizButt1Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        
        VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HorizButt2Record C-Win 
PROCEDURE HorizButt2Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        
        VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Define button panel for additional functions 
  Parameters:  <none>
  Notes:       The button panel can also be used for standard buttons (new,delete..)
               but then the panel must be linked to the browse like a toolbar would.
               A disadvantage with this usage is that the panel has no function 
               for disabling buttons.
------------------------------------------------------------------------------*/
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DYNAMIC-FUNCTION("setAttribute",SESSION,"copyToolbarToBrowse","yes").

DO WITH FRAME {&FRAME-NAME}:
  hToolbar = DYNAMIC-FUNCTION("NewToolbar"
            ,ToolBar:HANDLE
            ,"File"
            ,"new,copy,undo,save,delete,filter,excel,rule,browseconfig,rule"
            ,"maxborder").

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,BrwCustomer:HANDLE
          ,200
          ,"multiple,bgcolor|" + STRING(DYNAMIC-FUNCTION("getColorNum",RGB-VALUE(250,250,250)))
          ,"Customer"
           + ";custnum;name"
          ,"WHERE false"
          ,"").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsBrowse","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"name",""
          ,"custnum",""
          ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  /* The panel buttons can have images of type bmp or ico (requires controls.dll)
    NOTE: The size (bytes) of the first button must be GE any of the next buttons 
    These two panels uses different methods to call procedures: */

  /* Call different procdures for each button: HorizButt1Record, HorizButt2Record: */
  hPanelHor = DYNAMIC-FUNCTION("NewPanel"
            ,HorizontalPanel:HANDLE
            ,"File"  /* Append menu to File menu */
            ,"HorizButt1;&Button 1;My buttons tooltip;;bmp\links16.bmp"
           + ",HorizButt2;B&utton 2;My 2. button tooltip;;bmp\root.bmp"
            ,70,22  /* Button width, height - width needed for horizontal buttons */
            ,"").
  /* To have the buttons added to browse popup-menu */
  DYNAMIC-FUNCTION("CreateObjectLink",hPanelHor,hBrowse).

  /* Add the buttons as rigth-click functions on the browse: */
/*   DYNAMIC-FUNCTION("NewMenuBand",hBrowse,"","addfirst;" + STRING(hPanelHor)). */

  /* Call PanelAction from each button */
  hPanelVert = DYNAMIC-FUNCTION("NewPanel"
            ,Panel:HANDLE
            ,"Vertial buttons" /* New sub-menu (blank for none) */ 
            ,"VertAction1;&My button;My buttons tooltip;PanelAction;ico\important_note.ico"
           + ",VertAction2;M&y 2nd button;My 2. button tooltip;PanelAction;ico\calendar2.ico"
            ,0,36     
            ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hPanelVert,hBrowse).


  DYNAMIC-FUNCTION("NewToolbar"
            ,ToolBar:HANDLE
            ,"File"  /* Append to File menu */
            ,"close;E&xit¤menu"
            ,"").

END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,100,400,300).

RUN InvokeMethod(hBrowse,"OpenQuery").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PanelAction C-Win 
PROCEDURE PanelAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE PROGRAM-NAME(1) SKIP
        DYNAMIC-FUNCTION("getPanelAction" IN hPanelVert)
        VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPanelProperties C-Win 
FUNCTION setPanelProperties RETURNS LOGICAL
  ( INPUT ihPanel     AS HANDLE,   /* Handle to panel proc */
    INPUT ihPanelRect AS HANDLE,   /* Handle to rectangle */
    INPUT iiPanel     AS INT ) :   /* Handle to panel itself */
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: To work with the panel handle include JukeBoxPanel.i in def. 
------------------------------------------------------------------------------*/

IF ihPanelRect:NAME = "HorizontalPanel" THEN 
  RUN PanelSetProperty (iiPanel,{&PANEL_SEPARATORS},{&GEN_ENABLE}).
ELSE
  DYNAMIC-FUNCTION("InitPanelFont" IN ihPanel,"MS Sans Serif",8).


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

