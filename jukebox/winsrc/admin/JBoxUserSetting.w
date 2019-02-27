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
                     
DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hToolbar     AS HANDLE NO-UNDO.
DEF VAR hFieldMap    AS HANDLE NO-UNDO.

DEF VAR hSettingName AS HANDLE NO-UNDO.
DEF VAR hContext     AS HANDLE NO-UNDO.
DEF VAR hSourceFile  AS HANDLE NO-UNDO.
DEF VAR hObjectName  AS HANDLE NO-UNDO.
DEF VAR hSetting     AS HANDLE NO-UNDO.
DEF VAR hJBoxUserId  AS HANDLE NO-UNDO.
DEF VAR cJBoxUserId  AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwUserSettings tbUserSettings BrowseSearch 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitOverlays C-Win 
FUNCTION InitOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setJBoxUserId C-Win 
FUNCTION setJBoxUserId RETURNS LOGICAL
  ( INPUT icUserId AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE BrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY .91.

DEFINE RECTANGLE brwUserSettings
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152 BY 23.81.

DEFINE RECTANGLE tbUserSettings
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwUserSettings AT ROW 2.67 COL 2
     tbUserSettings AT ROW 1.24 COL 20.2
     BrowseSearch AT ROW 1.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 153.6 BY 25.67.


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
         TITLE              = "User settings"
         HEIGHT             = 25.67
         WIDTH              = 153.6
         MAX-HEIGHT         = 25.67
         MAX-WIDTH          = 153.6
         VIRTUAL-HEIGHT     = 25.67
         VIRTUAL-WIDTH      = 153.6
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User settings */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User settings */
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
  IF cJBoxUserId = DYNAMIC-FUNCTION("getASuserId") THEN
    DYNAMIC-FUNCTION("BuildTableCache","JBoxUserSetting|WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'").
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
    setJBoxUserId("bha").
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseColumnLookup C-Win 
PROCEDURE BrowseColumnLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturnValues   AS CHAR NO-UNDO.
DEF VAR bOk             AS LOG  NO-UNDO.

  /* See also procedure setLookupAttributes */

IF DYNAMIC-FUNCTION("getCurrentObject") = hSettingName THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "temp-table ttSettingName"
                    + ";SettingName|CHARACTER|x(30)||Setting"
                    + ";SettingDesc|CHARACTER|x(80)||Description"
                   ,"WHERE false"
                    ,""                                                  
                    ,"SettingName",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
  IF bOk AND cReturnValues NE "" THEN 
    hSettingName:SCREEN-VALUE = cReturnValues.
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hSetting THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "temp-table ttSettingName"
                    + ";SettingValue|CHARACTER|x(30)||Setting value"
                    + ";SettingValueDesc|CHARACTER|x(80)||Description"
                   ,"WHERE false"
                    ,""                                                  
                    ,"SettingValue",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
  IF bOk AND cReturnValues NE "" THEN 
    hSetting:SCREEN-VALUE = cReturnValues.
END.

ELSE RUN SUPER.

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
  ENABLE brwUserSettings tbUserSettings BrowseSearch 
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
DEF VAR hSearchField    AS HANDLE NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
  ,brwUserSettings:HANDLE
  ,100
  ,"MULTIPLE"
  ,"JBoxUserSetting"
    + ";cSettingName"
    + ";cContext"
    + ";cSourceFile|Source file or SESSION"
    + ";cObjectName"
    + ";cSetting"
    + ";cJBoxUserId"
    + ";dCreated"
    + ";cCreatedBy"
    + ";dModified"
    + ";cModifiedBy"
    + ";iJBoxUserSettingId"
  + ",JBoxUser"
    + ";cUserName@7"
  ,"WHERE false"
  + ",FIRST JBoxUser NO-LOCK OF JBoxUserSetting OUTER-JOIN"
  ,"").

  DYNAMIC-FUNCTION("setSortString",hBrowse,"cSettingName").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableOnDblClick","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"setReadOnlyOnReturn","yes").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",BrowseSearch:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
  ,tbUserSettings:HANDLE
  ,"Fil"
  ,"new;copy,delete,Filter,excel"
  ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  InitOverlays().

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
hJBoxUserId:SCREEN-VALUE = cJBoxUserId.
APPLY "back-tab" TO hJBoxUserId.
hSourceFile:SCREEN-VALUE = "SESSION".
APPLY  "back-tab" TO hSourceFile.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTBrect  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihOther   AS HANDLE NO-UNDO.

DEF VAR httBuffer AS HANDLE NO-UNDO.

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):NAME = "SettingName" THEN DO:
  httBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).
  httBuffer:BUFFER-CREATE().
  ASSIGN httBuffer::SettingName = "MainMenuType"
         httBuffer::SettingDesc = "Different users may have different menu types"
         .
  RUN InvokeMethod(ihBrowse,"OpenQuery").
END.
ELSE IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(1):NAME = "SettingValue" THEN DO:
  CASE hSettingName:SCREEN-VALUE:
    WHEN "MainMenuType" THEN DO:
      httBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).
      httBuffer:BUFFER-CREATE().
      ASSIGN httBuffer::SettingValue = "Ribbon"
             httBuffer::SettingValueDesc = "Office 2007 Ribbon menu with MDI tabs as window containers"   
             .
      httBuffer:BUFFER-CREATE().
      ASSIGN httBuffer::SettingValue = "Nav-bar"
             httBuffer::SettingValueDesc = "Outlook style menu with individual window containers"   
             .
      httBuffer:BUFFER-CREATE().
      ASSIGN httBuffer::SettingValue = "Plain"
             httBuffer::SettingValueDesc = "Simple drop menu with optional button panel. Individual window containers"   
             .
    END.
  END CASE.
  RUN InvokeMethod(ihBrowse,"OpenQuery").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitOverlays C-Win 
FUNCTION InitOverlays RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hSettingName  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"cSettingName","cSettingName"
              ,"temp-table","","","").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSettingName,"cSettingName").

hContext  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"cContext","cContext"
          ,"","","","").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hContext,"cContext").

hSourceFile  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"cSourceFile","cSourceFile"
              ,"","","","").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSourceFile,"cSourceFile").

hObjectName  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"cObjectName","cObjectName"
              ,"","","","").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hObjectName,"cObjectName").

hSetting  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"cSetting","cSetting"
              ,"temp-table","","","").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hSetting,"cSetting").

hJBoxUserId  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"cJBoxUserId","cJBoxUserId"
              ,"JBoxUser;cUserName;cJBoxUserId;bActive;bSuperUser","WHERE TRUE","cJBoxUserId","").
DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hJBoxUserId,"cJBoxUserId").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setJBoxUserId C-Win 
FUNCTION setJBoxUserId RETURNS LOGICAL
  ( INPUT icUserId AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE cJBoxUserId = '" + icUserId + "'").
THIS-PROCEDURE:CURRENT-WINDOW:TITLE = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Innstillinger: " ELSE "Settings: ") + icUserId.
RUN InvokeMethod(hBrowse,"OpenQuery").
cJBoxUserId = icUserId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

