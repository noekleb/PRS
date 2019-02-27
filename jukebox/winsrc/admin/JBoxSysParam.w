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
&Scoped-Define ENABLED-OBJECTS brwSysParam tbSysParam rectBrowseSearch ~
bActive iJBoxCompanyId cUsage cSysParamName cSysParamText ~
cSysParamCharValue iSysParamIntValue dCreated cCreatedBy ~
fSysParamDecimalValue dSysParamDateValue cModifiedBy dModified 
&Scoped-Define DISPLAYED-OBJECTS bActive iJBoxCompanyId cUsage ~
cSysParamName cSysParamText cSysParamCharValue iSysParamIntValue dCreated ~
cCreatedBy fSysParamDecimalValue dSysParamDateValue cModifiedBy dModified 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cSysParamName AS CHARACTER FORMAT "x(30)" 
     LABEL "Parameter name" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE iJBoxCompanyId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "CompanyId" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE cUsage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 92 BY 4.29.

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cModifiedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cSysParamCharValue AS CHARACTER FORMAT "x(256)" 
     LABEL "Sys param char value" 
     VIEW-AS FILL-IN 
     SIZE 136 BY 1.

DEFINE VARIABLE cSysParamText AS CHARACTER FORMAT "x(256)" 
     LABEL "Sys param text" 
     VIEW-AS FILL-IN 
     SIZE 136 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Created" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dModified AS DATE FORMAT "99/99/9999" 
     LABEL "Modified" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dSysParamDateValue AS DATE FORMAT "99/99/9999" 
     LABEL "Sys param date value" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fSysParamDecimalValue AS DECIMAL FORMAT "->>,>>>,>>9.9999" INITIAL 0 
     LABEL "Sys param decimal value" 
     VIEW-AS FILL-IN 
     SIZE 24.4 BY 1.

DEFINE VARIABLE iSysParamIntValue AS INTEGER FORMAT "->>,>>>,>>9" INITIAL 0 
     LABEL "Sys.param integer value" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1.

DEFINE RECTANGLE brwSysParam
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 162.4 BY 12.14.

DEFINE RECTANGLE rectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY 1.05.

DEFINE RECTANGLE tbSysParam
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11.8 BY .95.

DEFINE VARIABLE bActive AS LOGICAL INITIAL yes 
     LABEL "Active" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bActive AT ROW 16 COL 149.4
     iJBoxCompanyId AT ROW 16.14 COL 24 COLON-ALIGNED
     cUsage AT ROW 17.19 COL 69 NO-LABEL
     cSysParamName AT ROW 17.24 COL 24 COLON-ALIGNED
     cSysParamText AT ROW 21.81 COL 24 COLON-ALIGNED
     cSysParamCharValue AT ROW 22.91 COL 24 COLON-ALIGNED
     iSysParamIntValue AT ROW 24 COL 24 COLON-ALIGNED
     dCreated AT ROW 25 COL 125 COLON-ALIGNED
     cCreatedBy AT ROW 25 COL 146.2 COLON-ALIGNED
     fSysParamDecimalValue AT ROW 25.05 COL 24 COLON-ALIGNED
     dSysParamDateValue AT ROW 26.1 COL 24 COLON-ALIGNED
     cModifiedBy AT ROW 26.1 COL 146.2 COLON-ALIGNED
     dModified AT ROW 26.14 COL 125 COLON-ALIGNED
     brwSysParam AT ROW 3.62 COL 1.6
     tbSysParam AT ROW 1.24 COL 2.2
     rectBrowseSearch AT ROW 2.43 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 164.6 BY 26.52.


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
         TITLE              = "System parameters"
         HEIGHT             = 26.52
         WIDTH              = 164.6
         MAX-HEIGHT         = 26.52
         MAX-WIDTH          = 164.6
         VIRTUAL-HEIGHT     = 26.52
         VIRTUAL-WIDTH      = 164.6
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
       FRAME DEFAULT-FRAME:HEIGHT           = 26.52
       FRAME DEFAULT-FRAME:WIDTH            = 164.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* System parameters */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* System parameters */
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
  DISPLAY bActive iJBoxCompanyId cUsage cSysParamName cSysParamText 
          cSysParamCharValue iSysParamIntValue dCreated cCreatedBy 
          fSysParamDecimalValue dSysParamDateValue cModifiedBy dModified 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwSysParam tbSysParam rectBrowseSearch bActive iJBoxCompanyId cUsage 
         cSysParamName cSysParamText cSysParamCharValue iSysParamIntValue 
         dCreated cCreatedBy fSysParamDecimalValue dSysParamDateValue 
         cModifiedBy dModified 
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
DEF VAR hSearchField AS HANDLE NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN iJBoxCompanyId:DELIMITER       = "|"
         iJBoxCompanyId:LIST-ITEM-PAIRS = RIGHT-TRIM("Global|0|"
                                    +  DYNAMIC-FUNCTION("getFieldList",
                                      "JBoxCompany;cCompanyName;iJBoxCompanyId",
                                      "WHERE true BY cCompanyName"),"|") 
         cSysParamName:DELIMITER        = "|"
         cSysParamName:LIST-ITEMS       = DYNAMIC-FUNCTION("getFieldList","JBoxSysParamNames;cSysParamName","WHERE true")
         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
  ,brwSysParam:HANDLE
  ,100
  ,""
  ,"JBoxSysParam"
    + ";iJBoxCompanyId"
    + ";iJBoxSysParamId"
    + ";cSysParamName"
    + ";cSysParamText"
    + ";cSysParamCharValue"
    + ";dSysParamDateValue"
    + ";iSysParamIntValue"
    + ";fSysParamDecimalValue"
    + ";bActive"
    + ";dCreated"
    + ";cCreatedBy"
    + ";dModified"
    + ";cModifiedBy"
 + ",JBoxCompany"
    + ";cCompanyName@2"
 + ",JBoxSysParamNames"
    + ";cUsage"
  ,"WHERE false"
 + ",FIRST JBoxCompany NO-LOCK OF JBoxSysParam OUTER-JOIN" 
 + ",FIRST JBoxSysParamNames NO-LOCK OF JBoxSysParam" 
  ,"").

  DYNAMIC-FUNCTION("setSortString",hBrowse,"cSysParamName,iJBoxCompanyId").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrowse,4).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
    ,hBrowse:QUERY
    ,FRAME {&FRAME-NAME}:HANDLE
    ,"iJBoxCompanyId,cSysParamCharValue,cSysParamText,cSysParamName,dSysParamDateValue,iSysParamIntValue,fSysParamDecimalValue,bActive",""
    ,"cCreatedBy,cModifiedBy,dCreated,dModified,cUsage",""
    ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
    ,TbSysParam:HANDLE
    ,"File"
    ,"new,copy,undo,delete,save,filter,excel"
   + ",ParNames;Edit parameter names"
   + ",resetGlobal;Reset global attributes"
    ,"maxborder").
  
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).

  RUN InvokeMethod(hBrowse,"OpenQuery").

END.

DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "cSysParamCharValue,cSysParamText").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"cUsage").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,200,0,0).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParNamesRecord C-Win 
PROCEDURE ParNamesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.
RUN JBoxSysParNames.w.
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.
cSysParamName:LIST-ITEMS IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getFieldList","JBoxSysParamNames;cSysParamName","WHERE true").
RUN InvokeMethod(hBrowse,"RefreshBrowseRecord").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetGlobalRecord C-Win 
PROCEDURE resetGlobalRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("DoMessage",0,4,"Reset global attributes (variables - attached to SESSION:FIRST-PROCEDURE)?","","") = 6 THEN
  DYNAMIC-FUNCTION("msetAttribute",SESSION:FIRST-PROCEDURE,"*","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icField AS CHAR NO-UNDO.
IF icField = "cSysParamName" THEN
  cUsage:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getFieldList","JBoxSysParamNames;cUsage","WHERE cSysParamName = '" + SELF:SCREEN-VALUE + "'").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

