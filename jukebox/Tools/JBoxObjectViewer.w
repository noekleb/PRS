&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/*&SCOPED-DEFINE AdvGuiWin*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/*&SCOPED-DEFINE PureABLWin 1*/

DEF VAR ix                AS INT NO-UNDO.
DEF VAR bOK               AS LOG NO-UNDO.
DEF VAR hBrowseObject     AS HANDLE NO-UNDO.
DEF VAR hBrowseEvent      AS HANDLE NO-UNDO.
DEF VAR hBrowseAttribute  AS HANDLE NO-UNDO.
DEF VAR hBrowseObjectLink AS HANDLE NO-UNDO.
DEF VAR hBufferObject     AS HANDLE NO-UNDO.
DEF VAR hQueryObject      AS HANDLE NO-UNDO.
DEF VAR hMenu             AS HANDLE NO-UNDO.
DEF VAR hProjectView      AS HANDLE NO-UNDO.
DEF VAR hResizeView       AS HANDLE NO-UNDO.
DEF VAR hTTlist           AS HANDLE NO-UNDO.
DEF VAR httResize         AS HANDLE NO-UNDO.
DEF VAR hSessProc         AS HANDLE NO-UNDO.
DEF VAR hAppComp          AS HANDLE NO-UNDO.
DEF VAR hLogMethods       AS HANDLE NO-UNDO.
DEF VAR hTranMan          AS HANDLE NO-UNDO.
DEF VAR hLinkList         AS HANDLE NO-UNDO.
DEF VAR hABLattr          AS HANDLE NO-UNDO.
DEF VAR hIe               AS HANDLE NO-UNDO.
DEF VAR hAttrList         AS HANDLE NO-UNDO.
DEF VAR oJboxInternetExplorer AS uc.JBoxInternetExplorer NO-UNDO.


DEF VAR cJBoxUIlibHTML    AS CHAR NO-UNDO.
DEF VAR cJBoxASlibHTML    AS CHAR NO-UNDO.
DEF VAR cJukeBoxRoot      AS CHAR NO-UNDO.
DEF VAR bInputParam       AS LOG  NO-UNDO.
DEF VAR cCallsTo          AS CHAR NO-UNDO.
DEF VAR cTotalInvoked     AS CHAR NO-UNDO.
DEF VAR cHTMLfileList     AS CHAR NO-UNDO.
DEF VAR ghMainAppBuilderWindow AS HANDLE NO-UNDO.

DEF VAR hDebugEventSrcProc AS HANDLE NO-UNDO.
DEF VAR hDebugEventObj     AS HANDLE NO-UNDO.
DEF VAR cDebugEventMethod  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttWindow 
    FIELD cWinName     AS CHAR 
    FIELD cMenuTitle   AS CHAR 
    FIELD hWindow      AS HANDLE
    FIELD hWinMenuItem AS HANDLE
    .
DEF VAR hWinMenu AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttProjectFiles
    FIELD cId             AS CHAR
    FIELD cParentId       AS CHAR
    FIELD iLevel          AS INT
    FIELD cFileName       AS CHAR
    .
DEF VAR httProjectFiles AS HANDLE.
httProjectFiles = BUFFER ttProjectFiles:HANDLE.

DEF TEMP-TABLE ttCallsFrom
    FIELD cCall           AS CHAR
    FIELD cCallsFrom      AS CHAR
    .

DEF TEMP-TABLE ttFrames 
    FIELD hFrame      AS HANDLE
    FIELD cSourceProc AS CHAR.

DEF STREAM sProgramFile.
DEF STREAM sMethodFile.

/* &IF DEFINED (ttDataDictDefined) = 0 &THEN        */
/* &SCOPED-DEFINE ttDataDictDefined                 */
/* {incl/ttDataDict.i NEW}   /* For query module */ */
/* &ENDIF                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY rectBrwObject rectBrwAttribute ~
rectBrwObjectLink rectBrwEvent rectJukeBox RectBrowseSearch cmbWindow ~
cAttrName cAttrValue fiAttributeLabel 
&Scoped-Define DISPLAYED-OBJECTS cmbWindow cAttrName cAttrValue ~
fiAttributeLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildWindowList C-Win 
FUNCTION BuildWindowList RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convInvoked C-Win 
FUNCTION convInvoked RETURNS LOGICAL
  ( INPUT icInvoked  AS CHAR,
    INPUT icParentId AS CHAR,
    INPUT iiLevel    AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convToHTML C-Win 
FUNCTION convToHTML RETURNS CHARACTER
  ( INPUT icFileName         AS CHAR,
    INPUT ibReturnMethodFile AS LOG,
    INPUT ibJukeBoxFile      AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillObjectTable C-Win 
FUNCTION FillObjectTable RETURNS LOGICAL
  ( INPUT icTable  AS CHAR,
    INPUT ihBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProcNameForWindow C-Win 
FUNCTION getProcNameForWindow RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTransSourceProc C-Win 
FUNCTION getTransSourceProc RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewWindowSource C-Win 
FUNCTION ViewWindowSource RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 2" 
     SIZE 146 BY .43.

DEFINE VARIABLE cmbWindow AS CHARACTER FORMAT "X(256)":U 
     LABEL "Container" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE cAttrValue AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 96 BY 3.67 NO-UNDO.

DEFINE VARIABLE cAttrName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53 BY .86 NO-UNDO.

DEFINE VARIABLE fiAttributeLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Attribute setting:" 
      VIEW-AS TEXT 
     SIZE 15.6 BY .62 NO-UNDO.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.6 BY .91.

DEFINE RECTANGLE rectBrwAttribute
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.8 BY 9.52.

DEFINE RECTANGLE rectBrwEvent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.4 BY 5.86.

DEFINE RECTANGLE rectBrwObject
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 8.67.

DEFINE RECTANGLE rectBrwObjectLink
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.6 BY 5.86.

DEFINE RECTANGLE rectJukeBox
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 11.33 COL 1.4
     cmbWindow AT ROW 1.33 COL 12.6 COLON-ALIGNED
     cAttrName AT ROW 17.86 COL 65.8 COLON-ALIGNED NO-LABEL
     cAttrValue AT ROW 18.86 COL 51 NO-LABEL
     fiAttributeLabel AT ROW 17.91 COL 50 COLON-ALIGNED NO-LABEL
     rectBrwObject AT ROW 2.57 COL 2
     rectBrwAttribute AT ROW 13.14 COL 2.2
     rectBrwObjectLink AT ROW 11.91 COL 99.4
     rectBrwEvent AT ROW 11.91 COL 50.6
     rectJukeBox AT ROW 1.14 COL 2
     RectBrowseSearch AT ROW 11.91 COL 2.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 147.2 BY 21.67.


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
         TITLE              = "JukeBox Object Viewer"
         HEIGHT             = 21.67
         WIDTH              = 147
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 52.38
         VIRTUAL-WIDTH      = 320
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
       FRAME DEFAULT-FRAME:HEIGHT           = 21.67
       FRAME DEFAULT-FRAME:WIDTH            = 147.2.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JukeBox Object Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JukeBox Object Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* JukeBox Object Viewer */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
  DYNAMIC-FUNCTION("SetSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE:HANDLE IN FRAME {&FRAME-NAME},NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbWindow C-Win
ON VALUE-CHANGED OF cmbWindow IN FRAME DEFAULT-FRAME /* Container */
DO:
  IF cmbWindow:SCREEN-VALUE NE ? THEN DO:
    IF cmbWindow:SCREEN-VALUE = STRING(SESSION) THEN
      DYNAMIC-FUNCTION("setAttribute",hBrowseObject,"querywhere","WHERE cObjectName = 'SESSION'").
    ELSE
      DYNAMIC-FUNCTION("setAttribute",hBrowseObject,"querywhere","WHERE hWindow = WIDGET-HANDLE('" + cmbWindow:SCREEN-VALUE + "')").
  END.
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowseObject,"querywhere","WHERE false").

  DYNAMIC-FUNCTION("setCurrentObject",hBrowseObject).
  RUN OpenQuery.

  IF cmbWindow:SCREEN-VALUE NE ? AND VALID-HANDLE(WIDGET-HANDLE(cmbWindow:SCREEN-VALUE)) 
     AND WIDGET-HANDLE(cmbWindow:SCREEN-VALUE) NE SESSION AND WIDGET-HANDLE(cmbWindow:SCREEN-VALUE) NE SESSION:FIRST-PROCEDURE THEN
    ViewWindowSource(WIDGET-HANDLE(cmbWindow:SCREEN-VALUE)).
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
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF VALID-HANDLE(hProjectView) THEN APPLY "close" TO hProjectView.
  IF VALID-HANDLE(hResizeView) THEN APPLY "close" TO hResizeView.
  IF VALID-HANDLE(hTTlist) THEN APPLY "close" TO hTTlist.
  IF VALID-HANDLE(hSessProc) THEN APPLY "close" TO hSessProc.
  IF VALID-HANDLE(hAppComp) THEN APPLY "close" TO hAppComp.
  IF VALID-HANDLE(hLogMethods) THEN APPLY "close" TO hLogMethods.
  IF VALID-HANDLE(hTranMan) THEN APPLY "close" TO hTranMan.
  IF VALID-HANDLE(hLinkList) THEN APPLY "close" TO hLinkList.
  IF VALID-HANDLE(hABLattr) THEN APPLY "close" TO hABLattr.
  IF VALID-HANDLE(hIe) THEN APPLY "close" TO hIe.
  IF VALID-HANDLE(hAttrList) THEN APPLY "close" TO hAttrList.
  DELETE OBJECT httResize NO-ERROR.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  SESSION:SET-WAIT-STATE("general").

  RUN enable_UI.

  RUN JBoxLoadLib.p ("JBoxASlib.p,JBoxUIlib.p,ResizeLib.p" + IF PROVERSION BEGINS "1" THEN ",JBoxFUlib.p" ELSE "").

  RUN InitWindow.
    
  SESSION:SET-WAIT-STATE("").
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ABLattributesRecord C-Win 
PROCEDURE ABLattributesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hABLattr) THEN DO:
  RUN ABLattributes.w PERSIST SET hABLattr.
  RUN InitializeObject IN hABLattr.
END.
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseObject THEN
  DYNAMIC-FUNCTION("setObjectHandle" IN hABLattr,hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hObject"):BUFFER-VALUE).
ELSE
  DYNAMIC-FUNCTION("setObjectHandle" IN hABLattr,hBrowseEvent:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hWidget"):BUFFER-VALUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppCompRecord C-Win 
PROCEDURE AppCompRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hAppComp) THEN 
  RUN AppComp.w PERSIST SET hAppComp.

hAppComp:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AttributeListRecord C-Win 
PROCEDURE AttributeListRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN DO:
  MESSAGE "Select a window first"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

IF NOT VALID-HANDLE(hLinkList) THEN DO:
  RUN WindowAttributeList.w PERSIST SET hAttrList.
  RUN InitializeObject IN hAttrList.
END.
DYNAMIC-FUNCTION("setWindowHandle" IN hAttrList,cmbWindow:SCREEN-VALUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CallURLRecord C-Win 
PROCEDURE CallURLRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cURL AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.

RUN JBoxAskForValue.w ("Enter URL","CHARACTER|x(320)",INPUT-OUTPUT cURL,OUTPUT iReturn).
IF iReturn = 2 THEN 
  DYNAMIC-FUNCTION("setWebDoc","",cURL).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CmdLineRecord C-Win 
PROCEDURE CmdLineRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OS-COMMAND.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContentToExcelRecord C-Win 
PROCEDURE ContentToExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cObjectType AS CHAR  NO-UNDO.
DEF VAR hObject     AS HANDLE NO-UNDO.
ASSIGN cObjectType = hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cObjectType"):BUFFER-VALUE
       hObject     = hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hObject"):BUFFER-VALUE.

IF CAN-DO("fieldMap,query,browse,temp-table",cObjectType) THEN 
  CASE cObjectType:
    WHEN "fieldMap"   THEN DYNAMIC-FUNCTION("ToExcelViaFile",hObject,0).
    WHEN "query"      THEN DYNAMIC-FUNCTION("ToExcelViaFile",hObject:GET-BUFFER-HANDLE(1),0).
    WHEN "browse"     THEN DYNAMIC-FUNCTION("ToExcelViaFile",hObject:QUERY:GET-BUFFER-HANDLE(1),0).
    WHEN "temp-table" THEN DYNAMIC-FUNCTION("ToExcelViaFile",hObject:DEFAULT-BUFFER-HANDLE,0).
  END CASE.
ELSE MESSAGE "Export of content is available for browsers, queries, fieldMaps and temp-tables"
             VIEW-AS ALERT-BOX INFORMATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebugAttributeRecord C-Win 
PROCEDURE DebugAttributeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT  NO-UNDO.
DEF VAR cAttr   AS CHAR NO-UNDO.

RUN JBoxAskForValue.w ("Enter attribute name","CHARACTER|x(30)",INPUT-OUTPUT cAttr,OUTPUT iReturn).
IF iReturn = 2 THEN 
  DYNAMIC-FUNCTION("setAttribute",SESSION,"debugAttribute",cAttr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebugEventRecord C-Win 
PROCEDURE DebugEventRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
ASSIGN cDebugEventMethod = hBrowseEvent:QUERY:GET-BUFFER-HANDLE(1)::cMethod
       hDebugEventObj    = hBrowseEvent:QUERY:GET-BUFFER-HANDLE(1)::hObject
       .
       
MESSAGE "The Object Viewer now subscribes to the evnent handler for " hDebugEventObj:NAME SKIP
        "Method: " cDebugEventMethod SKIP(1)
        "Keep the Object Viewer running when initiating the event" 
VIEW-AS ALERT-BOX.       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebugResize C-Win 
PROCEDURE DebugResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihProcOrWin AS HANDLE NO-UNDO.

DEF VAR hResizeBuffer AS HANDLE NO-UNDO.
DEF VAR hResizeQuery  AS HANDLE NO-UNDO.
DEF VAR hDesignObject AS HANDLE NO-UNDO.
DEF VAR hWindow       AS HANDLE NO-UNDO.
DEF VAR cStep         AS CHAR   NO-UNDO.
DEF VAR cType         AS CHAR   NO-UNDO.
DEF VAR hResizeFrame  AS HANDLE NO-UNDO.

IF ihProcOrWin:TYPE = "procedure" THEN
  ASSIGN hWindow = ihProcOrWin:CURRENT-WINDOW
         cStep   = ihProcOrWin:FILE-NAME.
ELSE
  hWindow = ihProcOrWin.

DELETE OBJECT httResize NO-ERROR.
EMPTY TEMP-TABLE ttFrames.
hBrowseObject:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrowseObject:QUERY:QUERY-OFF-END:
  hDesignObject = hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hDesignObject"):BUFFER-VALUE.
  IF VALID-HANDLE(hDesignObject) THEN DO:
    CREATE ttFrames.
    ttFrames.cSourceProc = hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cSourceProc"):BUFFER-VALUE.
    IF hDesignObject:TYPE = "frame" THEN
      ttFrames.hFrame = hDesignObject.
    ELSE
      ttFrames.hFrame = hDesignObject:FRAME NO-ERROR.
  END.
  hBrowseObject:QUERY:GET-NEXT().
END.

httResize = DYNAMIC-FUNCTION("getResizeSettings",hWindow).

ASSIGN hResizeBuffer = httResize:DEFAULT-BUFFER-HANDLE.

CREATE QUERY hResizeQuery.
hResizeQuery:SET-BUFFERS(hResizeBuffer).
hResizeQuery:QUERY-PREPARE("FOR EACH " + hResizeBuffer:NAME).

hResizeQuery:QUERY-OPEN().
hResizeQuery:GET-FIRST().
REPEAT WHILE NOT hResizeQuery:QUERY-OFF-END:
  hResizeFrame  = hResizeBuffer:BUFFER-FIELD("hFrame"):BUFFER-VALUE.

  FIND FIRST ttFrames 
       WHERE ttFrames.hFrame = hResizeBuffer:BUFFER-FIELD("hFrame"):BUFFER-VALUE NO-ERROR.
  IF AVAIL ttFrames THEN
    hResizeBuffer:BUFFER-FIELD("cSourceProc"):BUFFER-VALUE = ttFrames.cSourceProc.
  
  hResizeBuffer:BUFFER-FIELD("cFrameName"):BUFFER-VALUE = hResizeFrame:NAME.
/*   hResizeBuffer:BUFFER-FIELD("cFrameName"):BUFFER-VALUE = hResizeBuffer:BUFFER-FIELD("hFrame"):BUFFER-VALUE:NAME. */
  hResizeQuery:GET-NEXT().
END.

DELETE OBJECT hResizeQuery NO-ERROR.

IF NOT VALID-HANDLE(hResizeView) THEN
  RUN ResizeViewer.w PERSIST SET hResizeView.
ELSE RUN RefreshRecord IN hResizeView.

hResizeView:CURRENT-WINDOW:TITLE = cStep.
IF cStep NE "" THEN
  CURRENT-WINDOW = ihProcOrWin:CURRENT-WINDOW.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpJlwTreeViewRecord C-Win 
PROCEDURE DumpJlwTreeViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "JlwTreeViewToExcel".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpStaticASlibTablesRecord C-Win 
PROCEDURE DumpStaticASlibTablesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DumpStaticASlibTables").
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
  DISPLAY cmbWindow cAttrName cAttrValue fiAttributeLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY rectBrwObject rectBrwAttribute rectBrwObjectLink 
         rectBrwEvent rectJukeBox RectBrowseSearch cmbWindow cAttrName 
         cAttrValue fiAttributeLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecFunctionRecord C-Win 
PROCEDURE ExecFunctionRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFunctionName AS CHAR NO-UNDO.
DEF VAR cParameter    AS CHAR NO-UNDO.
DEF VAR iReturn       AS INT  NO-UNDO.
DEF VAR cReturn       AS CHAR NO-UNDO.

RUN JBoxAskForValue.w ("Function name","CHARACTER|x(40)|getAttribute",INPUT-OUTPUT cFunctionName,OUTPUT iReturn).
IF iReturn = 2 THEN DO: 
  RUN JBoxAskForValue.w ("Parameter","CHARACTER|x(40)",INPUT-OUTPUT cParameter,OUTPUT iReturn).
  cReturn = DYNAMIC-FUNCTION(cFunctionName) NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
    cReturn = DYNAMIC-FUNCTION(cFunctionName,cParameter) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
      cReturn = DYNAMIC-FUNCTION(cFunctionName,hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):HANDLE) NO-ERROR.
      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN 
        cReturn = DYNAMIC-FUNCTION(cFunctionName,hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):HANDLE,cParameter) NO-ERROR.
    END.
  END.
  IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN
    MESSAGE "Return value: " SKIP 
            cReturn
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ELSE
    MESSAGE ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.

END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExploreTmpRecord C-Win 
PROCEDURE ExploreTmpRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE oServer AS COM-HANDLE NO-UNDO.

CREATE 'Shell.Application' oServer.

/* Invoke the Windows Explorer on the C:\WINNT folder               */

NO-RETURN-VALUE oServer:Explore(SESSION:TEMP-DIR).

/* Release the object references                                    */

RELEASE OBJECT oServer.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findAppBuilder C-Win 
PROCEDURE findAppBuilder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* 05-JAN-2007 sla: We will probably need this guy soon to add a penguin button on it */
DEFINE VARIABLE hCheckFirstMenuItem    AS HANDLE     NO-UNDO.
ghMainAppBuilderWindow = SESSION:FIRST-CHILD.

DO WHILE ghMainAppBuilderWindow <> ?:
    /* 14-SEP-2007 sla: as asked by Jan Keirse, some people change the title of the AppBuilder, so let's be more flexible' */
    /* IF ghMainAppBuilderWindow:TITLE = "AppBuilder" THEN DO: */
    IF ghMainAppBuilderWindow:TITLE MATCHES "*AppBuilder" THEN DO:
        
        /* 14-SEP-2007 sla: safer test afterward */
        /* hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR. */
        /*IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:FIRST-CHILD NO-ERROR. */
        /*IF   VALID-HANDLE(hCheckFirstMenuItem)                                                                    */
        /* AND hCheckFirstMenuItem:LABEL = "&File"                                                                  */
        /* THEN LEAVE.                                                                                              */
    
        hCheckFirstMenuItem = ghMainAppBuilderWindow:MENU-BAR NO-ERROR.
        IF VALID-HANDLE(hCheckFirstMenuItem) THEN hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.
        
        /* 06-DEC-2007 bot: Make sure we have the "&Help" menu, since this is not always the last one with modified appbuilders */
        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL <> "&Help":U THEN DO WHILE hCheckFirstMenuItem <> ?:
           ASSIGN hCheckFirstMenuItem = hCheckFirstMenuItem:PREV-SIBLING NO-ERROR. /*Walk back from the last menuitem*/
           IF   VALID-HANDLE(hCheckFirstMenuItem)
            AND hCheckFirstMenuItem:LABEL = "&Help":U
              THEN LEAVE. /* We found the correct menuitem*/
         END.
        /* 06-DEC-2007 bot: now return to the "About AppBuilder" check*/
        
        IF   VALID-HANDLE(hCheckFirstMenuItem)
         AND hCheckFirstMenuItem:LABEL = "&Help" THEN DO:
            hCheckFirstMenuItem = hCheckFirstMenuItem:LAST-CHILD NO-ERROR.
            
            IF VALID-HANDLE(hCheckFirstMenuItem)
             AND hCheckFirstMenuItem:TYPE = "MENU-ITEM"
             AND hCheckFirstMenuItem:LABEL = "About AppBuilder"
             THEN LEAVE.  /* OK , found a reliable way */
         END.
    
    END.
    ghMainAppBuilderWindow = ghMainAppBuilderWindow:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTranslationRecord C-Win 
PROCEDURE InitTranslationRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN DO: 
  MESSAGE "Select a window first"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

PUBLISH "BuildScreenObjects" (WIDGET-HANDLE(cmbWindow:SCREEN-VALUE)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLibPath      AS CHAR    NO-UNDO.
DEF VAR cHTMLcat      AS CHAR    NO-UNDO.
DEF VAR cTmpFileName  AS CHAR    NO-UNDO.
DEF VAR idx           AS INT     NO-UNDO.
DEF VAR hFieldMap     AS HANDLE  NO-UNDO.
DEF VAR hSearchField  AS HANDLE  NO-UNDO.
DEF VAR cIeFrameList  AS CHAR    NO-UNDO.
DEF VAR cIeFrameNames AS CHAR    NO-UNDO.
DEF VAR hIeFrame      AS HANDLE  NO-UNDO.
DEF VAR hCol          AS HANDLE  NO-UNDO.

RUN findAppBuilder.

DO WITH FRAM {&FRAME-NAME}:
  ASSIGN cJBoxUIlibHTML = convToHTML("JBoxUIlib.p",TRUE,FALSE)
         cJBoxASlibHTML = convToHTML("JBoxASlib.p",TRUE,FALSE)
         cLibPath       = SEARCH("JBoxUIlib.p")
         THIS-PROCEDURE:CURRENT-WINDOW:TITLE = THIS-PROCEDURE:CURRENT-WINDOW:TITLE + " (" + PROVERSION + " " + PROGRESS + ")"
         .
  IF cLibPath = ? THEN cLibPath = SEARCH("JBoxUIlib.r").

  DO ix = 1 TO NUM-ENTRIES(cLibPath,"\") - 2:
    cJukeBoxRoot = cJukeBoxRoot + ENTRY(ix,cLibPath,"\") + "\".
  END.

  SUBSCRIBE TO "InvalidateHandle" ANYWHERE.
  
  SUBSCRIBE TO "StartJBoxEvent" ANYWHERE.

  hBufferObject = WIDGET-HANDLE(DYNAMIC-FUNCTION("getObjectTableHandle","ttObject")).
  hBufferObject = hBufferObject:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY hQueryObject.
  hQueryObject:SET-BUFFERS(hBufferObject).
  hQueryObject:QUERY-PREPARE("FOR EACH ttObject BY hWindow").
  cmbWindow:DELIMITER = "|".

  hBrowseObject = DYNAMIC-FUNCTION("NewBrowse",
                                   rectBrwObject:HANDLE,
                                   1000,
                                   "",
                                   "temp-table"
                                   + ";+cObjectName|CHARACTER|x(30)||ObjectName"
                                   + ";+cObjectType|CHARACTER|x(25)||Type"
                                   + ";+cInitProc|CHARACTER|x(35)||Inst.by"
                                   + ";+cGenProc|CHARACTER|x(35)||Inst.proc"
                                   + ";+cSourceProc|CHARACTER|x(70)||Source proc"
                                   + ";+cState|CHARACTER|x(10)||Object state"
                                   + ";+hObject|HANDLE|||Handle"
                                   + ";+ValidObjectHandle|LOGICAL|||Valid"
                                   + ";+!hWindow|HANDLE|||Window"
                                   + ";+!hDesignObject|HANDLE|||Design object"
                                   ,
                                   "where false",
                                   "sort|cObjectType").
  hBrowseObject:NAME = "brwObject".
  DYNAMIC-FUNCTION("NewMenuBand",hBrowseObject
                  ,"MultiSortBrowse;Sort on multiple columns"
                 + (IF VALID-HANDLE(ghMainAppBuilderWindow) THEN ",OpenFile;Open source file in AppBuilder using ABhack method (doesn't) need ABhack to run)" ELSE "")
                 + ",ABLattributes;(Some) ABL attributes for object"
                 + ",Excel,ContentToExcel;Content to Excel","").

  hBrowseEvent = DYNAMIC-FUNCTION("NewBrowse",
                                   rectBrwEvent:HANDLE,
                                   1000,
                                   "",
                                   "temp-table"
                                   + ";+cName|CHARACTER|x(40)||Event"
                                   + ";+cAction|CHARACTER|x(25)||Action"
                                   + ";+cMethod|CHARACTER|x(25)||Method"
                                   + ";+cLabel|CHARACTER|x(25)||Label"
                                   + ";+cWidgetName|CHARACTER|x(40)||Widget name"
                                   + ";+hWidget|HANDLE|||Widget-handle"
                                   + ";+!hObject|HANDLE|||Handle"
                                   + ";+!hWindow|HANDLE|||Window"
                                   ,
                                   "where false",
                                   "sort|cName").
  hBrowseEvent:NAME = "brwEvent".
  DYNAMIC-FUNCTION("NewMenuBand",hBrowseEvent,
                   "Excel"
                 + ",ABLattributes;(Some) ABL attributes for object"
                 + ",DebugEvent;Debug event"
                   ,"").
  hCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseEvent,"cName").
  hCol:WIDTH-CHARS = 20.
  hCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowseEvent,"cWidgetName").
  hCol:WIDTH-CHARS = 20.

  hBrowseAttribute = DYNAMIC-FUNCTION("NewBrowse",
                                   rectBrwAttribute:HANDLE,
                                   1000,
                                   "!FIT-LAST-COLUMN",
                                   "temp-table"
                                   + ";+cName|CHARACTER|x(256)||Attribute"
                                   + ";+cValue|CHARACTER|x(320)||Value"
                                   + ";+!hObject|HANDLE|||Handle"
                                   + ";+!hWindow|HANDLE|||Window"
                                   ,
                                   "where false",
                                   "sort|cName").
  hBrowseAttribute:NAME = "brwAttribute".
  hBrowseAttribute:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100.
  hBrowseAttribute:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 2500.
  DYNAMIC-FUNCTION("setAttribute",hBrowseAttribute,"searchdefault","filter").
  DYNAMIC-FUNCTION("NewMenuBand",hBrowseAttribute,"Excel","").


  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrowseAttribute,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowseAttribute,hSearchField).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                               hBrowseAttribute:QUERY,
                               FRAME {&FRAME-NAME}:HANDLE,
                               "","",
                               "cName,cValue","cAttrName,cAttrValue",
                               "").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowseAttribute).

  hBrowseObjectLink = DYNAMIC-FUNCTION("NewBrowse",
                                   rectBrwObjectLink:HANDLE,
                                   1000,
                                   "",
                                   "temp-table"
                                   + ";+!cFromObject|CHARACTER|x(20)||From object"
                                   + ";+cLinktype|CHARACTER|x(20)||Linktype"
                                   + ";+cLinkInfo|CHARACTER|x(50)||Linkinfo"
                                   + ";+cToObject|CHARACTER|x(50)||To object"
                                   + ";+cToObjectType|CHARACTER|x(20)||To object type"
                                   + ";+cInitProc|CHARACTER|x(70)||Created by"
                                   + ";+!cFromObjectType|CHARACTER|x(20)||From object type"
                                   + ";+!hFromObject|HANDLE|||Handle"
                                   + ";+!hToObject|HANDLE|||Handle"
                                   ,
                                   "where false",
                                   "sort|cLinkType").
  hBrowseObjectLink:NAME = "brwObjectLink".
  DYNAMIC-FUNCTION("NewMenuBand",hBrowseObjectLink,"Excel","").

  DYNAMIC-FUNCTION("CreateParentLink",hBrowseAttribute,hBrowseObject,"hObject").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseObjectLink,hBrowseObject,"hFromObject;hObject").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseEvent,hBrowseObject,"hObject").


  hMenu = DYNAMIC-FUNCTION("NewMenuBand",
                    THIS-PROCEDURE:CURRENT-WINDOW,
                    "Refresh;Refresh"
                  + ",ResizeSettings;Resize settings for window"
                  + ",WidgetList;Widget list for window"
                  + ",LinkList;Object links for window"
                  + ",AttributeList;All attributes for window"
                  + ",DebugAttribute;Debug attribute setting"
                  + ",TTList;Dynamic temp-tables and buffers"
                  + (IF SEARCH("protools/_ppmgr.r") NE ? AND PROGRESS = "full" THEN
                     ",ProcMgr;Procedure manager"
                   + ",Session;Session info"
                     ELSE "")
                  + ",PidCmd;Pid and command line and workdir"   
                  + (IF PROGRESS = "full" THEN 
                      ",AppComp;Application compiler"
                    + ",ExecFunction;Excecute function"
                    + ",DumpStaticAsLibTables;Dump static ASlib tables"
                     ELSE "")
                  + ",SetDebugAlert;Set DEBUG-ALERT flag"   
                  + ",SetSessionAttr;Set session attribute"   
                  + ",LogMethods;Log methods"
                  + ",LogServer;Log queries and transactions on server"
                  + ",ViewMem;View memory usage"
                  + ",ViewPropath;View PROPATH"
                  + ",SearchPropath;Search for file in PROPATH"
                  + ",CmdLine;Command line"
                  + ",TranMan;Start translation program"
                  + ",InitTranslation;Init translation for window"
                  + ",ExploreTmp;Explore temp-dir"
                  + ",ViewBehaviour;View behaviour settings"
                  + ",ViewFieldCache;View field cache"
                  + ",ViewSettingsCache;View user settings cache"
                  + ",DumpJlwTreeView;Dump JLW treeview table"
                  + ",WidgetAttributes;Attributes for widget"
                  + ",CallURL;Call URL"
                  + ",RunProc;Run procedure on client"
                  + ",RunServerProc;Run procedure on server"
                  + ",rule"
                  + ",Close;E&xit",
                    "File").

  FillObjectTable("ttObject",hBrowseObject:QUERY:GET-BUFFER-HANDLE(1)).
  FillObjectTable("ttEvent",hBrowseEvent:QUERY:GET-BUFFER-HANDLE(1)).
  FillObjectTable("ttAttribute",hBrowseAttribute:QUERY:GET-BUFFER-HANDLE(1)).
  FillObjectTable("ttObjectLink",hBrowseObjectLink:QUERY:GET-BUFFER-HANDLE(1)).

  BuildWindowList().

/*
  cIeFrameList = DYNAMIC-FUNCTION("getFrameHandleList" IN hIe).
  DO ix = 1 TO NUM-ENTRIES(cIeFrameList):
    hIeFrame = WIDGET-HANDLE(ENTRY(ix,cIeFrameList)) NO-ERROR.
    IF VALID-HANDLE(hIeFrame) THEN
      cIeFrameNames = cIeFrameNames + (IF cIeFrameNames NE "" THEN "," ELSE "") + hIeFrame:NAME.
  END.
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, DYNAMIC-FUNCTION("getFrameHandle" IN hIe),cIeFrameNames).
  */

  DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"RectBrowseSearch," + hSearchField:NAME).

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrwObject,brwObject,rectIe,rectCurrWin,rectJukeBox,RectBrowseSearch,cAttrValue").
/*  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "edHelp").*/
  DYNAMIC-FUNCTION("setResizeXgroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33.33, 
                                      "rectBrwAttribute,brwAttribute,rectBrwEvent,brwEvent,rectBrwObjectLink,brwObjectLink,cmbWindow,cmbCatalog,cmbView,rectCurrWin").
  DYNAMIC-FUNCTION("setResizeXgroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,66.66,"cAttrValue").
  DYNAMIC-FUNCTION("setMoveXgroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33.33, 
                                      "rectBrwEvent,brwEvent,cmbCatalog,rsHelpType,cAttrValue,fiAttributeLabel,cAttrName").
  DYNAMIC-FUNCTION("setMoveXgroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,66.66, 
                                      "rectBrwObjectLink,brwObjectLink,cmbView").


  DYNAMIC-FUNCTION("SetSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE:HANDLE IN FRAME {&FRAME-NAME},NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
                   STRING(rectBrwObject:HANDLE) + "," +
                   STRING(hBrowseObject) + "," +
/*                   DYNAMIC-FUNCTION("getFrameHandleList" IN hIe) + "," +*/
                   STRING(rectBrwAttribute:HANDLE) + "," +
                   STRING(hBrowseAttribute) + "," +
                   STRING(rectBrwObjectLink:HANDLE) + "," +
                   STRING(hBrowseObjectLink) + "," +
                   STRING(rectBrwEvent:HANDLE) + "," +
                   STRING(hBrowseEvent) + "," +
                   STRING(RectBrowseSearch:HANDLE) + "," +
                   STRING(hSearchField)
                   ).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},100,200).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,550,250,0,0).

  SUBSCRIBE TO "DebugResize" ANYWHERE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.

FIND FIRST ttWindow 
     WHERE ttWindow.hWindow = ihWindow NO-ERROR.
IF AVAIL ttWindow THEN DO:
  DELETE OBJECT ttWindow.hWinMenuItem.
  DELETE ttWindow.
  RUN RefreshRecord.
END.

IF ihWindow = hIe THEN APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkListRecord C-Win 
PROCEDURE LinkListRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN DO:
  MESSAGE "Select a window first"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

IF NOT VALID-HANDLE(hLinkList) THEN DO:
  RUN WindowObjectLinks.w PERSIST SET hLinkList.
  RUN InitializeObject IN hLinkList.
END.
DYNAMIC-FUNCTION("setWindowHandle" IN hLinkList,cmbWindow:SCREEN-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LogMethodsRecord C-Win 
PROCEDURE LogMethodsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hLogMethods) THEN 
    RUN LogMethods.w PERSIST SET hLogMethods.

hLogMethods:CURRENT-WINDOW:MOVE-TO-TOP().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LogServerRecord C-Win 
PROCEDURE LogServerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bServerLogs AS LOG NO-UNDO.
MESSAGE "Log queries and transactions to ascii-files on server" 
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bServerLogs.

IF bServerLogs THEN
  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|ServerQuery.log," +   
                    "TransLogFile|ServerTrans.log").
ELSE
  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|," +   
                    "TransLogFile|"). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenFileRecord C-Win 
PROCEDURE OpenFileRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hOpenFile AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  RUN protools/abhack/openFileInAB.p 
       PERSISTENT SET hOpenFile
      (hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cSourceProc"):BUFFER-VALUE, ghMainAppBuilderWindow).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PidCmdRecord C-Win 
PROCEDURE PidCmdRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("DoMessage",0,20,"PID: " + DYNAMIC-FUNCTION("getMyPid") 
                                 + CHR(10) + "Cmd.line: " + DYNAMIC-FUNCTION("getMyCommandLine")
                                 + CHR(10) + "Workdir:  " + System.Environment:CurrentDirectory
                                 ,"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcMgrRecord C-Win 
PROCEDURE ProcMgrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF NOT VALID-HANDLE(hSessProc) THEN
  RUN protools/_ppmgr.w PERSIST SET hSessProc.
    
hSessProc:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cWinHandle AS CHAR   NO-UNDO.
DEF VAR hCurrObj   AS HANDLE NO-UNDO.
DEF VAR iReposRow  AS INT    NO-UNDO.

ASSIGN cWinHandle = cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       iReposRow  = hBrowseObject:FOCUSED-ROW
       hCurrObj   = IF hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
                      hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("hObject"):BUFFER-VALUE
                    ELSE ?.

hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().
hBrowseObjectLink:QUERY:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().
hBrowseEvent:QUERY:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().
hBrowseAttribute:QUERY:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().


FillObjectTable("ttObject",hBrowseObject:QUERY:GET-BUFFER-HANDLE(1)).
FillObjectTable("ttEvent",hBrowseEvent:QUERY:GET-BUFFER-HANDLE(1)).
FillObjectTable("ttAttribute",hBrowseAttribute:QUERY:GET-BUFFER-HANDLE(1)).
FillObjectTable("ttObjectLink",hBrowseObjectLink:QUERY:GET-BUFFER-HANDLE(1)).

BuildWindowList().

cmbWindow:SCREEN-VALUE = cWinHandle NO-ERROR.
APPLY "value-changed" TO cmbWindow IN FRAME {&FRAME-NAME}.

IF hCurrObj NE ? THEN DO:
  bOk = hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE hObject = WIDGET-HANDLE('" + STRING(hCurrObj) + "')") NO-ERROR.
  IF bOK THEN DO:
    hBrowseObject:SET-REPOSITIONED-ROW(iReposRow,"conditional").
    hBrowseObject:QUERY:REPOSITION-TO-ROWID(hBrowseObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      APPLY "value-changed" TO hBrowseObject.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResizeSettingsRecord C-Win 
PROCEDURE ResizeSettingsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hWindow       AS HANDLE NO-UNDO.

IF cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN DO: 
  MESSAGE "Select a window first"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

hWindow = WIDGET-HANDLE(cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
IF ERROR-STATUS:ERROR OR hWindow:TYPE NE "window" THEN RETURN.

RUN DebugResize(hWindow).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProcRecord C-Win 
PROCEDURE RunProcRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cProc AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.

RUN JBoxAskForValue.w ("Enter procedure name","CHARACTER|x(320)",INPUT-OUTPUT cProc,OUTPUT iReturn).
IF iReturn = 2 THEN DO:
  IF SEARCH(cProc) NE ? THEN 
    RUN VALUE(cProc).
  ELSE 
    MESSAGE "Invalid procedure name (not in PROPATH)"
             VIEW-AS ALERT-BOX.
END.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunServerProcRecord C-Win 
PROCEDURE RunServerProcRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cProc   AS CHAR NO-UNDO.
DEF VAR cParam  AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.

RUN JBoxAskForValue.w ("Enter procedure name","CHARACTER|x(320)",INPUT-OUTPUT cProc,OUTPUT iReturn).
IF iReturn = 2 THEN DO:
  RUN JBoxAskForValue.w ("Enter parameter","CHARACTER|x(320)",INPUT-OUTPUT cParam,OUTPUT iReturn).
  
  IF NOT DYNAMIC-FUNCTION("runProc",cProc,cParam,?) THEN
    MESSAGE DYNAMIC-FUNCTION("getTransactionMessage")
    VIEW-AS ALERT-BOX.
  ELSE DO:
    cParam = DYNAMIC-FUNCTION("getTransactionMessage").
    IF cParam NE "" THEN 
      DYNAMIC-FUNCTION("DoMessage",0,20,cParam,"","").
  END.    
END.     


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SearchPropathRecord C-Win 
PROCEDURE SearchPropathRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.
DEF VAR iReturn   AS INT  NO-UNDO.
RUN JBoxAskForValue.w ("File name","CHARACTER|X(50)",INPUT-OUTPUT cFileName,OUTPUT iReturn).
IF iReturn > 0 AND SEARCH(cFileName) NE ? THEN DO:
  FILE-INFO:FILE-NAME = SEARCH(cFileName).
  DYNAMIC-FUNCTION("DoMessage",0,20,FILE-INFO:FULL-PATHNAME,"","").
END.
ELSE IF iReturn > 0 AND SEARCH(cFileName) = ? THEN
 MESSAGE "File " cFileName " not found"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SessionRecord C-Win 
PROCEDURE SessionRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN protools/_session.w NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDebugAlertRecord C-Win 
PROCEDURE SetDebugAlertRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
SESSION:DEBUG-ALERT = NOT SESSION:DEBUG-ALERT.
MESSAGE "DEBUG-ALERT is now" STRING(SESSION:DEBUG-ALERT,"ON/OFF")
        VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSessionAttrRecord C-Win 
PROCEDURE SetSessionAttrRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cAttrName  AS CHAR NO-UNDO.
DEF VAR cAttrValue AS CHAR NO-UNDO.
DEF VAR iReturn    AS INT  NO-UNDO.

RUN JBoxAskForValue.w ("Attribute name","CHARACTER|x(50)",INPUT-OUTPUT cAttrName,OUTPUT iReturn).
IF iReturn = 2 THEN DO:
  cAttrValue = DYNAMIC-FUNCTION("getAttribute",SESSION,cAttrName). 
  RUN JBoxAskForValue.w ("Attribute value","CHARACTER|x(50)",INPUT-OUTPUT cAttrValue,OUTPUT iReturn).
  IF iReturn = 2 THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,cAttrName,cAttrValue).
END.  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartJBoxEvent C-Win 
PROCEDURE StartJBoxEvent :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihCurrObject AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCurrSourceProc AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCurrWidget AS HANDLE NO-UNDO.
DEF INPUT PARAM icCurrMethod AS CHAR NO-UNDO.

IF cDebugEventMethod NE "" 
   AND cDebugEventMethod = icCurrMethod
   AND ihCurrObject = hDebugEventObj THEN 
  MESSAGE "Object name: " ihCurrObject:NAME SKIP 
          "Method: " icCurrMethod
          VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartWindow C-Win 
PROCEDURE StartWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icWinName AS CHAR NO-UNDO.
DEF INPUT PARAM icRunFile AS CHAR NO-UNDO.

DEF VAR hMenuItm AS HANDLE NO-UNDO.

FIND FIRST ttWindow 
     WHERE ttWindow.cWinName   = icWinName
       AND ttWindow.cMenuTitle = icRunFile
     NO-ERROR.
IF NOT AVAIL ttWindow THEN DO:
  CREATE ttWindow.
  ASSIGN ttWindow.cWinName     = icWinName
         ttWindow.cMenuTitle   = icRunFile
         .
  
  RUN VALUE(icWinName) PERSIST SET ttWindow.hWindow.

  IF NOT VALID-HANDLE(hWinMenu) THEN DO:
    CREATE SUB-MENU hWinMenu
           ASSIGN PARENT = THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR
                  LABEL = "Vindu"
                  NAME  = "m_Vindu"
                  .
    DYNAMIC-FUNCTION("InitTranslation",hWinMenu).
  END.
  CREATE MENU-ITEM ttWindow.hWinMenuItem
         ASSIGN TOGGLE-BOX = TRUE
                PARENT     = hWinMenu
                LABEL      = icRunFile
                NAME       = "m_" + icRunFile
                TRIGGERS:
                  ON VALUE-CHANGED PERSISTENT RUN StartWindow IN THIS-PROCEDURE (ttWindow.cWinName,icRunFile).
                END TRIGGERS
                .
END.
ttWindow.hWindow:CURRENT-WINDOW:TITLE = ttWindow.cMenuTitle.
DYNAMIC-FUNCTION("setCompanyHeader",ttWindow.hWindow:CURRENT-WINDOW).
IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"InitializeObject") THEN 
  RUN InitializeObject IN ttWindow.hWindow NO-ERROR.
IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"MoveToTop") THEN 
  RUN MoveToTop IN ttWindow.hWindow NO-ERROR.
ELSE DO:
  ttWindow.hWindow:CURRENT-WINDOW:WINDOW-STATE = 3.
  ttWindow.hWindow:CURRENT-WINDOW:MOVE-TO-TOP().
END.
hMenuItm = hWinMenu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMenuItm):
  hMenuItm:CHECKED = IF hMenuItm = ttWindow.hWinMenuItem THEN TRUE ELSE FALSE.
  hMenuItm = hMenuItm:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TranManRecord C-Win 
PROCEDURE TranManRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hTranMan) THEN 
  RUN JBoxTranMan.w PERSIST SET hTranMan.

hTranMan:CURRENT-WINDOW:MOVE-TO-TOP().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TTListRecord C-Win 
PROCEDURE TTListRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hTTlist) THEN
  RUN TTViewer.w PERSIST SET hTTlist.
ELSE RUN RefreshRecord IN hTTlist.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewBehaviourRecord C-Win 
PROCEDURE ViewBehaviourRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoMessage",0,20,DYNAMIC-FUNCTION("getBehaviour"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewFieldCacheRecord C-Win 
PROCEDURE ViewFieldCacheRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",DYNAMIC-FUNCTION("getFieldCacheBuffer"),0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewMemRecord C-Win 
PROCEDURE ViewMemRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("dumpMemInfo").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewPropathRecord C-Win 
PROCEDURE ViewPropathRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoMessage",0,20,PROPATH,"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewSettingsCacheRecord C-Win 
PROCEDURE ViewSettingsCacheRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hSettingsCache AS HANDLE NO-UNDO.

hSettingsCache = DYNAMIC-FUNCTION("getCacheBufferHandle","JBoxUserSetting").
IF VALID-HANDLE(hSettingsCache) THEN
  DYNAMIC-FUNCTION("ToExcelViaFile",hSettingsCache,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WidgetAttributesRecord C-Win 
PROCEDURE WidgetAttributesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cWidget AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.

RUN JBoxAskForValue.w ("Enter widget-handle","INTEGER|>>>>>9",INPUT-OUTPUT cWidget,OUTPUT iReturn).
IF iReturn = 2 THEN DO: 
  IF NOT VALID-HANDLE(hABLattr) THEN DO:
    RUN ABLattributes.w PERSIST SET hABLattr.
    RUN InitializeObject IN hABLattr.
  END.
  DYNAMIC-FUNCTION("setObjectHandle" IN hABLattr,WIDGET-HANDLE(cWidget)).
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WidgetListRecord C-Win 
PROCEDURE WidgetListRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.

IF cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN DO:
  MESSAGE "Select a window first"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.
SESSION:SET-WAIT-STATE("general").
cFileName = SESSION:TEMP-DIR + "widglist_" + cmbWindow:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "_" + STRING(TIME) + ".csv".
DYNAMIC-FUNCTION("ShowMeTheWidgets",WIDGET-HANDLE(cmbWindow:SCREEN-VALUE),0,"top-down",cFileName).
SESSION:SET-WAIT-STATE("").
DYNAMIC-FUNCTION("setWebDoc","open",cFileName).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildWindowList C-Win 
FUNCTION BuildWindowList RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTmpWindow  AS HANDLE NO-UNDO.
DEF VAR cWindowList AS CHAR NO-UNDO INIT "|0|".
DEF VAR cTitle      AS CHAR NO-UNDO.
DEF VAR cProc       AS CHAR NO-UNDO.

hQueryObject:QUERY-OPEN().
hQueryObject:GET-FIRST().
REPEAT WHILE NOT hQueryObject:QUERY-OFF-END:
  IF hBufferObject:BUFFER-FIELD("hWindow"):BUFFER-VALUE NE hTmpWindow THEN DO:
    hTmpWindow = WIDGET-HANDLE(hBufferObject:BUFFER-FIELD("hWindow"):BUFFER-VALUE) NO-ERROR.
    IF VALID-HANDLE(hTmpWindow) THEN DO:
      IF CAN-QUERY(hTmpWindow,"TITLE") THEN
        ASSIGN cTitle = hTmpWindow:TITLE
               cProc  = getProcNameForWindow(hTmpWindow).
      ELSE DO:
        ASSIGN cTitle = hBufferObject:BUFFER-FIELD("cObjectName"):BUFFER-VALUE
               cProc  = "".
        IF cTitle = ? THEN cTitle = hBufferObject:BUFFER-FIELD("cObjectType"):BUFFER-VALUE.
      END.
      IF cTitle NE ? THEN
        cWindowList = cWindowList + cTitle + " (" + cProc + ")|" +
                                    STRING(hBufferObject:BUFFER-FIELD("hWindow"):BUFFER-VALUE) + "|".
    END.
  END.
  hTmpWindow = hBufferObject:BUFFER-FIELD("hWindow"):BUFFER-VALUE.
  hQueryObject:GET-NEXT().
END.
cmbWindow:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = RIGHT-TRIM(cWindowList,"|").

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convInvoked C-Win 
FUNCTION convInvoked RETURNS LOGICAL
  ( INPUT icInvoked  AS CHAR,
    INPUT icParentId AS CHAR,
    INPUT iiLevel    AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR idx AS INT NO-UNDO.

iiLevel = iiLevel + 1.

IF iiLevel < 5 THEN DO idx = 1 TO NUM-ENTRIES(icInvoked):
  IF NOT CAN-DO(cTotalInvoked,ENTRY(idx,icInvoked)) THEN DO:
    cTotalInvoked = cTotalInvoked + ENTRY(idx,icInvoked) + ",".
    CREATE ttProjectFiles.
    ASSIGN ttProjectFiles.cFileName       = SEARCH(ENTRY(idx,icInvoked))
           ttProjectFiles.cParentId       = icParentId
           ttProjectFiles.iLevel          = iiLevel
           ttProjectFiles.cId             = STRING(iiLevel) + "|" + ENTRY(idx,icInvoked)
           .
    convToHTML(ENTRY(idx,icInvoked),TRUE,FALSE).
    IF cCallsTo NE "" THEN
      convInvoked(cCallsTo,ttProjectFiles.cId,iiLevel).
  END.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convToHTML C-Win 
FUNCTION convToHTML RETURNS CHARACTER
  ( INPUT icFileName         AS CHAR,
    INPUT ibReturnMethodFile AS LOG,
    INPUT ibJukeBoxFile      AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFullProgramFileName AS CHAR NO-UNDO.
DEF VAR cOutProgramFileName  AS CHAR NO-UNDO.
DEF VAR cOutMethodFileName   AS CHAR NO-UNDO.
DEF VAR cInput               AS CHAR NO-UNDO.
DEF VAR cMethod              AS CHAR NO-UNDO.
DEF VAR cLink                AS CHAR NO-UNDO.
DEF VAR cMethodLink          AS CHAR NO-UNDO.
DEF VAR bNoInpParam          AS LOG  NO-UNDO.
DEF VAR cNextLineIs          AS CHAR NO-UNDO.
DEF VAR cTmpString           AS CHAR NO-UNDO.
DEF VAR idx                  AS INT  NO-UNDO.
DEF VAR idx2                 AS INT  NO-UNDO.
DEF VAR bUIBisRunning        AS LOG  NO-UNDO.

IF icFileName = '"' THEN RETURN "".
/*
DO WITH FRAME {&FRAME-NAME}:
  cCallsTo = "".
  EMPTY TEMP-TABLE ttCallsFrom.

  IF ibJukeBoxFile THEN
    cFullProgramFileName = cJukeBoxRoot + cmbCatalog:SCREEN-VALUE + "\" + cmbView:SCREEN-VALUE.
  ELSE 
    cFullProgramFileName = SEARCH(icFileName).
  IF cFullProgramFileName NE ? THEN DO:
    ASSIGN cOutProgramFileName = SESSION:TEMP-DIR + REPLACE(REPLACE(SUBSTR(icFileName,1,LENGTH(icFileName) - 1) + "html","/","__"),"\","__")
           cOutMethodFileName  = SESSION:TEMP-DIR + REPLACE(REPLACE(SUBSTR(icFileName,1,LENGTH(icFileName) - 2) + "Methods.html","/","__"),"\","__").
    OUTPUT STREAM sProgramFile TO VALUE(cOutProgramFileName).

    OUTPUT STREAM sMethodFile  TO VALUE(cOutMethodFileName).
    PUT STREAM sProgramFile UNFORMATTED "<html><head></head><body><pre>".
    IF NOT ibReturnMethodFile THEN
      PUT STREAM sProgramFile  UNFORMATTED "<strong>" + '<a href="' + cOutProgramFileName + "#" + cFullProgramFileName + '">Contents of</a>' + " " + cFullProgramFileName + "</strong>".
    PUT STREAM sProgramFile SKIP(1).
    PUT STREAM sMethodFile  UNFORMATTED "<html><head></head><body><pre><strong>" + '<a href="' + cOutMethodFileName + "#" + cFullProgramFileName + '">Contents of</a>' + " " + cFullProgramFileName + "</strong>" SKIP(1).
    INPUT FROM VALUE(cFullProgramFileName).
    ASSIGN bInputParam = FALSE
           ix = 0. 
    REPEAT:
      IMPORT UNFORMATTED cInput.
      ASSIGN ix         = ix + 1
             cTmpString = "".

      IF bUIBisRunning AND cInput MATCHES "*&ENDIF*" THEN
        bUIBisRunning = FALSE.

      IF cInput MATCHES "*&IF DEFINED(UIB_is_Running)*" THEN 
        bUIBisRunning = TRUE.
      
      IF bUIBisRunning THEN NEXT.

      IF cNextLineIs NE "" THEN DO:
        IF cNextLineIs = "UItrigger" THEN
          cMethod = ENTRY(1,cInput," ") + " " + ENTRY(2,cInput," ") + " " + ENTRY(3,cInput," ") + " " + ENTRY(4,cInput," ").
        ELSE 
          cMethod = cInput.
        ASSIGN cLink       = '<a name="' + cMethod + '"></a>' + cOutMethodFileName
               cMethodLink = '<a href="' + cOutMethodFileName + "#" + cMethod + '">' + cMethod + '</a>'.
        PUT STREAM sProgramFile UNFORMATTED '<a name="' + cMethod + '"></a>' + cFullProgramFileName SKIP.
        PUT STREAM sMethodFile UNFORMATTED cMethodLink SKIP.
        cNextLineIs = "".
      END.
      ELSE IF cInput MATCHES "*runproc*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"runproc"))
               cTmpString = IF NUM-ENTRIES(cTmpString) > 1 THEN TRIM(ENTRY(2,cTmpString),'"') ELSE "".
      END.
      ELSE IF cInput MATCHES "*gettemptable*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"gettemptable"))
               cTmpString = IF NUM-ENTRIES(cTmpString) > 1 THEN TRIM(ENTRY(2,cTmpString),'"') ELSE "".
      END.
      ELSE IF cInput MATCHES "*newviewer*" AND NUM-ENTRIES(cInput) GE 4 THEN 
        cTmpString = ENTRY(2,ENTRY(4,cInput),'"').
      ELSE IF cInput MATCHES "*run *.p*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"run")).
               cTmpString = IF NUM-ENTRIES(cTmpString," ") > 1 THEN TRIM(ENTRY(2,cTmpString," ")) ELSE "".
        IF cTmpString NE "" AND cTmpString MATCHES "*(*" THEN cTmpString = SUBSTR(cTmpString,1,INDEX(cTmpString,"(") - 1).
      END.
      ELSE IF cInput MATCHES "*run *.w*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"run")).
               cTmpString = IF NUM-ENTRIES(cTmpString," ") > 1 THEN TRIM(ENTRY(2,cTmpString," ")) ELSE "".
        IF cTmpString NE "" AND cTmpString MATCHES "*(*" THEN cTmpString = SUBSTR(cTmpString,1,INDEX(cTmpString,"(") - 1).
      END.
      ELSE IF cInput MATCHES "*|*|*|*.p*" THEN DO:
        DO idx = R-INDEX(cInput,".p") TO 1 BY -1:
          IF SUBSTR(cInput,idx,1) = "|" THEN LEAVE.
        END.
        cTmpString = TRIM(SUBSTR(cInput,idx + 1,R-INDEX(cInput,".p") - idx + 1)).
      END.
      ELSE IF cInput MATCHES "*|*.w*" THEN DO:
        DO idx = 2 TO NUM-ENTRIES(cInput,"|"):
          IF ENTRY(idx,cInput,"|") MATCHES "*.w*" THEN DO:
            DO idx2 = R-INDEX(ENTRY(idx,cInput,"|"),".w") TO 1 BY -1:
              IF SUBSTR(cInput,idx,1) = "|" THEN LEAVE.
            END.
            cTmpString = cTmpString + TRIM(SUBSTR(ENTRY(idx,cInput,"|"),idx2 + 1,R-INDEX(ENTRY(idx,cInput,"|"),".w") - idx2 + 1)) + ",".
          END.
        END.
      END.
      ELSE IF cInput MATCHES "*CustomCreateProc*.p*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"customcreateproc"))
               cTmpString = IF NUM-ENTRIES(cTmpString) > 1 THEN TRIM(ENTRY(2,cTmpString),'"') ELSE ""
               cTmpString = IF cTmpString NE "" THEN SUBSTR(cTmpString,1,R-INDEX(cTmpString,".p") + 1) ELSE "".
      END.
      ELSE IF cInput MATCHES "*CustomUpdValProc*.p*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"CustomUpdValProc"))
               cTmpString = IF NUM-ENTRIES(cTmpString) > 1 THEN TRIM(ENTRY(2,cTmpString),'"') ELSE ""
               cTmpString = IF cTmpString NE "" THEN SUBSTR(cTmpString,1,R-INDEX(cTmpString,".p") + 1) ELSE "".
      END.
      ELSE IF cInput MATCHES "*customDeleteValProc*.p*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"customDeleteValProc"))
               cTmpString = IF NUM-ENTRIES(cTmpString) > 1 THEN TRIM(ENTRY(2,cTmpString),'"') ELSE ""
               cTmpString = IF cTmpString NE "" THEN SUBSTR(cTmpString,1,R-INDEX(cTmpString,".p") + 1) ELSE "".
      END.
      ELSE IF cInput MATCHES "*postUpdateProc*.p*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"postUpdateProc"))
               cTmpString = IF NUM-ENTRIES(cTmpString) > 1 THEN TRIM(ENTRY(2,cTmpString),'"') ELSE ""
               cTmpString = IF cTmpString NE "" THEN SUBSTR(cTmpString,1,R-INDEX(cTmpString,".p") + 1) ELSE "".
      END.
      ELSE IF cInput MATCHES "*CalcFieldProc*.p*" THEN DO:
        ASSIGN cTmpString = SUBSTR(cInput,INDEX(cInput,"CalcFieldProc"))
               cTmpString = IF NUM-ENTRIES(cTmpString) > 1 THEN TRIM(ENTRY(2,cTmpString),'"') ELSE ""
               cTmpString = IF cTmpString NE "" THEN SUBSTR(cTmpString,1,R-INDEX(cTmpString,".p") + 1) ELSE "".
      END.
      ELSE IF cInput MATCHES "*" + CHR(123) + "*.i*" THEN DO:
        DO idx = INDEX(cInput,CHR(123)) + 1 TO LENGTH(cInput):
          cTmpString = cTmpString + SUBSTR(cInput,idx,1).
        END.
        cTmpString = SUBSTR(cTmpString,1,R-INDEX(cTmpString,CHR(125)) - 1).
      END.

      cTmpString = TRIM(cTmpString,",").
      DO idx = 1 TO NUM-ENTRIES(cTmpString):
        IF ENTRY(idx,cTmpString) NE "" AND SEARCH(ENTRY(idx,cTmpString)) NE ? AND NOT ENTRY(idx,cTmpString) MATCHES "*<*" THEN DO:
          IF NOT CAN-DO(cCallsTo,ENTRY(idx,cTmpString)) THEN cCallsTo = cCallsTo + ENTRY(idx,cTmpString) + ",".
          cInput = REPLACE(cInput,ENTRY(idx,cTmpString),'<a href="' + REPLACE(REPLACE(SUBSTR(ENTRY(idx,cTmpString),1,LENGTH(ENTRY(idx,cTmpString)) - 2),"/","__"),"\","__") + 'Methods.html' + '">' + ENTRY(idx,cTmpString) + '</a>').
          IF cMethodLink NE "" THEN DO:
            CREATE ttCallsFrom.
            ASSIGN ttCallsFrom.cCall      = ENTRY(idx,cTmpString)
                   ttCallsFrom.cCallsFrom = cMethodLink.
          END.
        END.
      END.

      PUT STREAM sProgramFile UNFORMATTED cInput SKIP.

      IF (cInput BEGINS "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION" AND NOT cInput BEGINS "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD") 
         OR cInput BEGINS "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE" 
         THEN DO:
        ASSIGN cMethod     = ENTRY(4,cInput," ")
               cLink       = '<a name="' + cMethod + '"></a>' + cOutMethodFileName
               cMethodLink = '<a href="' + cOutMethodFileName + "#" + cMethod + '">' 
                                         + (IF ENTRY(3,cInput," ") BEGINS "_" THEN 
                                              SUBSTR(ENTRY(3,cInput," "),2) 
                                            ELSE ENTRY(3,cInput," "))
                                         + "  " + cMethod 
                                         + '</a>'
               .
        PUT STREAM sProgramFile UNFORMATTED '<a name="' + cMethod + '"></a>' + cFullProgramFileName SKIP.
        PUT STREAM sMethodFile UNFORMATTED cMethodLink SKIP.
      END.
      ELSE IF cInput BEGINS "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL" THEN DO:
        IF cInput MATCHES "*OCX*" THEN 
          cNextLineIs = "OCXtrigger".
        ELSE 
          cNextLineIs = "UItrigger".
      END.
      ELSE IF cInput BEGINS "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK" THEN DO:
        ASSIGN cMethod     = "MAIN BLOCK"
               cLink       = '<a name="' + cMethod + '"></a>' + cOutMethodFileName
               cMethodLink = '<a href="' + cOutMethodFileName + "#" + cMethod + '">' + cMethod + '</a>'
               .
        PUT STREAM sProgramFile UNFORMATTED '<a name="' + cMethod + '"></a>' + cFullProgramFileName SKIP.
        PUT STREAM sMethodFile UNFORMATTED cMethodLink SKIP.
      END.

      IF ix < 100 THEN DO:
        IF cInput BEGINS "PROCEDURE" AND NOT bInputParam THEN
          bNoInpParam = TRUE.
        IF cInput MATCHES "*INPUT*PARAM*AS*" THEN
          bInputParam = TRUE.
      END.
    END.
    INPUT CLOSE.
    PUT STREAM sProgramFile UNFORMATTED "</pre></body></html>".
    OUTPUT STREAM sProgramFile CLOSE.
    cCallsTo = TRIM(cCallsTo,",").
    DO ix = 1 TO NUM-ENTRIES(cCallsTo):
      IF ENTRY(ix,cCallsTo) MATCHES "*.i" THEN DO:
        PUT STREAM sMethodFile UNFORMATTED
            '<a href="' + REPLACE(REPLACE(SUBSTR(ENTRY(ix,cCallsTo),1,LENGTH(ENTRY(ix,cCallsTo)) - 2) + 'Methods.html',"/","__"),"\","__") + '">INCLUDE LIBRARY ' + ENTRY(ix,cCallsTo) + '</a>'.

        FOR EACH ttCallsFrom
            WHERE ttCallsFrom.cCall = ENTRY(ix,cCallsTo)
            BREAK BY ttCallsFrom.cCallsFrom:
          IF FIRST-OF(ttCallsFrom.cCallsFrom) THEN
            PUT STREAM sMethodFile UNFORMATTED "  " + ttCallsFrom.cCallsFrom.
        END.
        PUT STREAM sMethodFile SKIP.
      END.
    END.
    DO ix = 1 TO NUM-ENTRIES(cCallsTo):
      IF NOT ENTRY(ix,cCallsTo) MATCHES "*.i" THEN DO:
        PUT STREAM sMethodFile UNFORMATTED
            '<a href="' + REPLACE(REPLACE(SUBSTR(ENTRY(ix,cCallsTo),1,LENGTH(ENTRY(ix,cCallsTo)) - 2) + 'Methods.html',"/","__"),"\","__") + '">EXTERNAL PROCEDURE ' + ENTRY(ix,cCallsTo) + '</a>'.

        FOR EACH ttCallsFrom
            WHERE ttCallsFrom.cCall = ENTRY(ix,cCallsTo)
            BREAK BY ttCallsFrom.cCallsFrom:
          IF FIRST-OF(ttCallsFrom.cCallsFrom) THEN
            PUT STREAM sMethodFile UNFORMATTED "  " + ttCallsFrom.cCallsFrom.
        END.
        PUT STREAM sMethodFile SKIP.
      END.
    END.

    PUT STREAM sMethodFile UNFORMATTED "</pre></body></html>".
    OUTPUT STREAM sMethodFile  CLOSE.

    OS-APPEND VALUE(cOutProgramFileName) VALUE(cOutMethodFileName).
    IF ibReturnMethodFile THEN 
      OS-DELETE VALUE(cOutProgramFileName).
    ELSE 
      OS-DELETE VALUE(cOutMethodFileName).
  END.
END.
*/
IF bNoInpParam THEN bInputParam = FALSE.
IF ibReturnMethodFile THEN 
  RETURN cOutMethodFileName.
ELSE 
  RETURN cOutProgramFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillObjectTable C-Win 
FUNCTION FillObjectTable RETURNS LOGICAL
  ( INPUT icTable  AS CHAR,
    INPUT ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR htt     AS HANDLE NO-UNDO.
DEF VAR hb      AS HANDLE NO-UNDO.
DEF VAR hq      AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hObject AS HANDLE NO-UNDO.
DEF VAR hWidget AS HANDLE NO-UNDO.

DEF VAR hbObject AS HANDLE NO-UNDO.

IF icTable = "ttResize" THEN
  htt = httResize.
ELSE
  htt = DYNAMIC-FUNCTION("getObjectTableHandle",icTable).
hb  = htt:DEFAULT-BUFFER-HANDLE.

IF CAN-DO("ttAttribute,ttObjectLink",icTable) THEN DO:
  htt = DYNAMIC-FUNCTION("getObjectTableHandle","ttObject").
  hbObject = htt:DEFAULT-BUFFER-HANDLE.
END.

CREATE QUERY hq.
hq:SET-BUFFERS(hb).
hq:QUERY-PREPARE("FOR EACH " + hb:NAME).
hq:QUERY-OPEN().

hq:GET-FIRST().
REPEAT WHILE NOT hq:QUERY-OFF-END:
  ihBuffer:BUFFER-CREATE().
  ihBuffer:BUFFER-COPY(hb).
  IF icTable = "ttObjectLink" THEN DO:
    bOk = hbObject:FIND-FIRST("WHERE hObject = WIDGET-HANDLE('" + STRING(hb:BUFFER-FIELD("hFromObject"):BUFFER-VALUE) + "')") NO-ERROR.
    IF bOk THEN
      ASSIGN ihBuffer:BUFFER-FIELD("cFromObject"):BUFFER-VALUE = hbObject:BUFFER-FIELD("cObjectName"):BUFFER-VALUE
             ihBuffer:BUFFER-FIELD("cFromObjectType"):BUFFER-VALUE = hbObject:BUFFER-FIELD("cObjectType"):BUFFER-VALUE.
    bOk = hbObject:FIND-FIRST("WHERE hObject = WIDGET-HANDLE('" + STRING(hb:BUFFER-FIELD("hToObject"):BUFFER-VALUE) + "')") NO-ERROR.
    IF bOk THEN
      ASSIGN ihBuffer:BUFFER-FIELD("cToObject"):BUFFER-VALUE = hbObject:BUFFER-FIELD("cObjectName"):BUFFER-VALUE
             ihBuffer:BUFFER-FIELD("cToObjectType"):BUFFER-VALUE = hbObject:BUFFER-FIELD("cObjectType"):BUFFER-VALUE.
  END.
  ELSE IF icTable = "ttObject" THEN DO:
    hbObject = hb:BUFFER-FIELD("hSourceProc"):BUFFER-VALUE.
    IF VALID-HANDLE(hbObject) THEN
      ihBuffer:BUFFER-FIELD("cSourceProc"):BUFFER-VALUE = SEARCH(hbObject:FILE-NAME).
    hObject = ihBuffer:BUFFER-FIELD("hObject"):BUFFER-VALUE.
    ihBuffer:BUFFER-FIELD("ValidObjectHandle"):BUFFER-VALUE = VALID-HANDLE(hObject).
  END.
  ELSE IF icTable = "ttEvent" THEN DO:
    hWidget = ihBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE.
    IF VALID-HANDLE(hWidget) THEN DO:
      IF CAN-QUERY(hWidget,"name") THEN
        ihBuffer:BUFFER-FIELD("cWidgetName"):BUFFER-VALUE = hWidget:NAME.
      IF CAN-QUERY(hWidget,"label") THEN
        ihBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE = hWidget:LABEL.
    END.
  END.
  ELSE IF icTable = "ttAttribute" THEN DO:
    bOk = hbObject:FIND-FIRST("WHERE hObject = WIDGET-HANDLE('" + STRING(hb:BUFFER-FIELD("hObject"):BUFFER-VALUE) + "')") NO-ERROR.
    IF bOk THEN
      ASSIGN ihBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE = hbObject:BUFFER-FIELD("cObjectName"):BUFFER-VALUE
             ihBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = hbObject:BUFFER-FIELD("cObjectType"):BUFFER-VALUE 
             ihBuffer:BUFFER-FIELD("chWindow"):BUFFER-VALUE = STRING(hbObject:BUFFER-FIELD("hWindow"):BUFFER-VALUE) 
             NO-ERROR.
  END.
  hq:GET-NEXT().
END.

DELETE OBJECT hq.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProcNameForWindow C-Win 
FUNCTION getProcNameForWindow RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hProc     AS HANDLE NO-UNDO.
DEF VAR cFileName AS CHAR NO-UNDO.

hProc = SESSION:FIRST-PROCEDURE.
REPEAT WHILE VALID-HANDLE(hProc):
  IF hProc:CURRENT-WINDOW = ihWindow THEN DO:
    cFileName = hProc:FILE-NAME.
    LEAVE.
  END.
  hProc = hProc:NEXT-SIBLING.
END.
RETURN cFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTransSourceProc C-Win 
FUNCTION getTransSourceProc RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSourceProcFile AS CHAR NO-UNDO.

cSourceProcFile = getProcNameForWindow(ihWindow).

IF cSourceProcFile MATCHES "*.ab" THEN
  cSourceProcFile = ENTRY(2,SUBSTR(cSourceProcFile,1,LENGTH(cSourceProcFile) - 3),"_") + ".w".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewWindowSource C-Win 
FUNCTION ViewWindowSource RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.
DEF VAR cMenuFile AS CHAR NO-UNDO.

SESSION:SET-WAIT-STATE("general").
cFileName = getProcNameForWindow(ihWindow).
cMenuFile = convToHTML(cFileName,TRUE,FALSE).
cTotalInvoked = "".
EMPTY TEMP-TABLE ttProjectFiles.
CREATE ttProjectFiles.
ASSIGN ttProjectFiles.cFileName = SEARCH(cFileName).
       ttProjectFiles.cId       = "0|" + ttProjectFiles.cFileName
       .     
/* IF cCallsTo NE "" THEN DO:                                   */
/*   convInvoked(cCallsTo,ttProjectFiles.cId,0).                */
/*   IF NOT VALID-HANDLE(hProjectView) THEN DO:                 */
/*     RUN JBoxProjectView.w PERSIST SET hProjectView.          */
/*     RUN InitializeObject IN hProjectView (httProjectFiles).  */
/*   END.                                                       */
/*   RUN FillTreeView IN hProjectView.                          */
/* END.                                                         */
/* ELSE IF VALID-HANDLE(hProjectView) THEN                      */
/*   RUN FillTreeView IN hProjectView.                          */

IF cMenuFile NE "" AND cMenuFile NE ? THEN
  oJboxInternetExplorer:setURI(cMenuFile).

/*DYNAMIC-FUNCTION("NavigateToURL" IN hIe,cMenuFile).*/
SESSION:SET-WAIT-STATE("").

RUN MoveToTop IN hIe NO-ERROR.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

