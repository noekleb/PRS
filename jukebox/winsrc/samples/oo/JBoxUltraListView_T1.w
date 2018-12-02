&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:PDFViewer.w

  Description: Procedure for show of pdf's within 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            

  Created:           13. Jan 2014

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
/* Uncomment to enable use of .Net components: */
&SCOPED-DEFINE AdvGuiWin 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEFINE VARIABLE oToolbar      AS JBoxToolbar       NO-UNDO.
DEFINE VARIABLE oJboxUltraListView     AS uc.JboxUltraListView      NO-UNDO.
DEFINE VARIABLE oGrid     AS uc.JboxIGGrid      NO-UNDO.

DEFINE VARIABLE oStatusDate   AS JBoxDevExDateEdit NO-UNDO.
/*DEFINE  VARIABLE fpSpreadDesigner1 AS FarPoint.Win.Spread.Design.FpSpreadDesigner NO-UNDO.*/

/*** Start instance property definitions for JBoxQuery object oQryCustomer ***/
DEFINE VARIABLE oQryDummy          AS JBoxQuery NO-UNDO.
DEFINE VARIABLE gcAllSites         AS CHARACTER NO-UNDO INITIAL "** All sites".
DEFINE VARIABLE hTabFolder         AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghttPatientProcess AS HANDLE    NO-UNDO.
DEFINE VARIABLE gcSiteId           AS CHARACTER NO-UNDO.
def var iZoom as int no-undo. 
def var iZoomFactor as int init 1 no-undo. 


DEFINE TEMP-TABLE FILELIST NO-UNDO
  FIELD FILELIST_NAME AS CHAR LABEL "Name"
  FIELD FILELIST_DESCRIPTION AS CHAR label "Description"
  FIELD FILELIST_DATEUPDATED as datetime label "Date updated"
  .

DEFINE TEMP-TABLE ttDummy NO-UNDO 
  FIELD cField AS CHARACTER. 
  

/*
FUNCTION getTableHandleQryCustomer RETURNS HANDLE().
  RETURN BUFFER ttPatientProcess:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQryCustomer RETURNS CHARACTER().
  RETURN 
      'Customer'.
END FUNCTION.
FUNCTION getQueryJoinQryCustomer RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
*/
/*** End instance property settings for JBoxQuery object oQryCustomer ***/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectTabFolder tbToolbar COMBO-BOX-1 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoCleanUpObjects C-Win 
FUNCTION DoCleanUpObjects RETURNS LOGICAL
  ( INPUT cWin AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Details","Icons","List","Tiles","Thumbnails" 
     DROP-DOWN-LIST
     SIZE 47 BY 1 NO-UNDO.

DEFINE RECTANGLE rectTabFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 137 BY 20.95.

DEFINE RECTANGLE tbToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     COMBO-BOX-1 AT ROW 1.24 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     "View" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.48 COL 83 WIDGET-ID 6
     rectTabFolder AT ROW 2.67 COL 3 WIDGET-ID 2
     tbToolbar AT ROW 1.24 COL 4 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 139.4 BY 22.76 WIDGET-ID 100.


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
         TITLE              = "PDF Viewer"
         HEIGHT             = 23.38
         WIDTH              = 141
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 200.8
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 200.8
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
       FRAME DEFAULT-FRAME:HEIGHT           = 22.76
       FRAME DEFAULT-FRAME:WIDTH            = 139.4.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* PDF Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* PDF Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
/*  fpSpreadDesigner1:dispose().              */
/*  DELETE OBJECT  fpSpreadDesigner1 NO-ERROR.*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-1 C-Win
ON VALUE-CHANGED OF COMBO-BOX-1 IN FRAME DEFAULT-FRAME
DO:
  oJboxUltraListView:layout( SELF:SCREEN-VALUE). 
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
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DesignReportRecord C-Win 
PROCEDURE DesignReportRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*    if valid-object(oJboxUltraListView) then*/
/*          oJboxUltraListView:ShowDialogSpreadDesigner().*/

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
  DISPLAY COMBO-BOX-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectTabFolder tbToolbar COMBO-BOX-1 
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
  
  DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",1,YES,?).
  
  DO WITH FRAME {&FRAME-NAME}:
    
    DYNAMIC-FUNCTION("NewObject",
                     THIS-PROCEDURE:CURRENT-WINDOW,
                     THIS-PROCEDURE,
                     "container").
    
    DYNAMIC-FUNCTION("setMinXYmove",2000,1200).


create filelist. filelist_name = "firstfile1". FILELIST_DESCRIPTION = "desc". FILELIST_DATEUPDATED = today.
create filelist. filelist_name = "firstfile2". FILELIST_DESCRIPTION = "desc".
create filelist. filelist_name = "firstfile3". FILELIST_DESCRIPTION = "desc".
create filelist. filelist_name = "firstfile4". FILELIST_DESCRIPTION = "desc".
create filelist. filelist_name = "firstfile5". FILELIST_DESCRIPTION = "desc".
create filelist. filelist_name = "firstfile6". FILELIST_DESCRIPTION = "desc".


 oJboxUltraListView = NEW uc.JboxUltraListView (THIS-PROCEDURE, rectTabFolder:HANDLE,BUFFER filelist:TABLE-HANDLE).
       
 oJboxUltraListView:RegisterWithJukeBox(no).
 oJboxUltraListView:Layout("details").
 oJboxUltraListView:dock.
 oJboxUltraListView:BringToFront().
 
 
    oToolbar = NEW JBoxToolbar(tbToolbar:HANDLE,"File"). 
    oToolbar:designWidget = tbToolbar:handle.
    oToolbar:AddTool("Print" ,"Print",true).
    oToolbar:AddTool("AddForm"  ,"Add Form",true).
    


  END.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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
  DYNAMIC-FUNCTION("DoLockWindow",?).
  
  IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
    RUN ShowForm ("").                                                       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     oJboxUltraListView:printPreview().
  */      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ZoomInOut C-Win 
PROCEDURE ZoomInOut :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   def input param ZoomType as char no-undo. 
   
   iZoom = IF ZoomTYpe = "in" THEN iZoom + iZoomFactor ELSE iZoom - iZoomFactor .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ZoomInRecord C-Win 
PROCEDURE ZoomInRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  RUN ZoomInOut("in").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ZoomOutRecord C-Win 
PROCEDURE ZoomOutRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
run ZoomInout ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoCleanUpObjects C-Win 
FUNCTION DoCleanUpObjects RETURNS LOGICAL
  ( INPUT cWin AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF VALID-OBJECT(oJboxUltraListView) THEN 
      DELETE OBJECT oJboxUltraListView NO-ERROR. 

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

