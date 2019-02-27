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
DEF VAR hBrwProcess  AS HANDLE NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hTbProcess   AS HANDLE NO-UNDO.
DEF VAR hBrwProRel   AS HANDLE NO-UNDO.
DEF VAR hTbProRel    AS HANDLE NO-UNDO.
DEF VAR hBrwProStep  AS HANDLE NO-UNDO.
DEF VAR hTbProStep   AS HANDLE NO-UNDO.
DEF VAR hBrwStepPar  AS HANDLE NO-UNDO.
DEF VAR hTbStepPar   AS HANDLE NO-UNDO.
DEF VAR hBrwStepLog  AS HANDLE NO-UNDO.
DEF VAR hStepLogCol  AS HANDLE NO-UNDO.

/* Overlay fields: */
DEF VAR hProDesc     AS HANDLE NO-UNDO.
DEF VAR hProRelCon   AS HANDLE NO-UNDO.
DEF VAR hProRelEntId AS HANDLE NO-UNDO.
DEF VAR hProStepSeq  AS HANDLE NO-UNDO.
DEF VAR hProStepDesc AS HANDLE NO-UNDO.
DEF VAR hProStepProg AS HANDLE NO-UNDO.
DEF VAR hProStepLog  AS HANDLE NO-UNDO.
DEF VAR hStepParSeq  AS HANDLE NO-UNDO.
DEF VAR hStepParName AS HANDLE NO-UNDO.
DEF VAR hStepParTrg  AS HANDLE NO-UNDO.
DEF VAR hStepParText AS HANDLE NO-UNDO.
DEF VAR hStepParDate AS HANDLE NO-UNDO.
DEF VAR hStepParDec  AS HANDLE NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbProcess brwProcess tbProStep ~
brwProcessStep tbProcessItemParam brwProcessStepParam brwProcessRel ~
brwProcessStepLog tbProcessRel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwProcess
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 6.67.

DEFINE RECTANGLE brwProcessRel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 6.62.

DEFINE RECTANGLE brwProcessStep
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 8.1.

DEFINE RECTANGLE brwProcessStepLog
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 175 BY 7.86.

DEFINE RECTANGLE brwProcessStepParam
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.1.

DEFINE RECTANGLE tbProcess
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 1.19.

DEFINE RECTANGLE tbProcessItemParam
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY 1.1.

DEFINE RECTANGLE tbProcessRel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 1.19.

DEFINE RECTANGLE tbProStep
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbProcess AT ROW 1.24 COL 2
     brwProcess AT ROW 2.67 COL 2
     tbProStep AT ROW 9.57 COL 2
     brwProcessStep AT ROW 11 COL 2
     tbProcessItemParam AT ROW 9.57 COL 82
     brwProcessStepParam AT ROW 11 COL 81
     brwProcessRel AT ROW 2.67 COL 81
     brwProcessStepLog AT ROW 19.57 COL 2
     tbProcessRel AT ROW 1.24 COL 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 177.6 BY 26.67.


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
         HEIGHT             = 26.67
         WIDTH              = 177.6
         MAX-HEIGHT         = 26.67
         MAX-WIDTH          = 177.6
         VIRTUAL-HEIGHT     = 26.67
         VIRTUAL-WIDTH      = 177.6
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
       FRAME DEFAULT-FRAME:HEIGHT           = 26.67
       FRAME DEFAULT-FRAME:WIDTH            = 177.6.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseColumnLookup C-Win 
PROCEDURE BrowseColumnLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hLookup           AS HANDLE NO-UNDO.
DEF VAR cBuffersAndFields AS CHAR   NO-UNDO.
DEF VAR cQueryCrit        AS CHAR   NO-UNDO.
DEF VAR cReturnField      AS CHAR   NO-UNDO.

hLookup = DYNAMIC-FUNCTION("getCurrentObject").

IF hLookup = hProRelEntId THEN DO:
  CASE hProRelCon:SCREEN-VALUE:
    WHEN "JBoxEmailAccount" THEN
      ASSIGN cBuffersAndFields = "JBoxEmailAccount;cEmailAddress;cAccountName;iJBoxEmailAccountId"
             cQueryCrit        = "WHERE true"
             cReturnField      = "iJBoxEmailAccountId"
             .
    WHEN "JBoxEventLogType" THEN
      ASSIGN cBuffersAndFields = "JBoxEventLogType;cEventLogType;cEventLogTypeText"
             cQueryCrit        = "WHERE true"
             cReturnField      = "cEventLogType"
             .
  END CASE.
  IF cBuffersAndFields NE "" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hLookup,"viewbuffersandfields",cBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",hLookup,"buffersandfields",cBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",hLookup,"querycriteria",cQueryCrit).
    DYNAMIC-FUNCTION("setAttribute",hLookup,"lookupattributes",cReturnField).
  END.
  ELSE RETURN.
END.

RUN SUPER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwStepPar THEN
  hStepParTrg:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + 
                                DYNAMIC-FUNCTION("getFieldList",
                                                 "JBoxProcessRel;+ProcessTrigger|jbadmin_jbprocessrel_desc.p;iJBoxProcessRelId",
                                                 "WHERE iJBoxProcessId = " + 
                                                        STRING(hBrwProcess:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxProcessId"):BUFFER-VALUE))
                                           ,"|").
RUN SUPER.

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
  ENABLE tbProcess brwProcess tbProStep brwProcessStep tbProcessItemParam 
         brwProcessStepParam brwProcessRel brwProcessStepLog tbProcessRel 
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

iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

DO WITH FRAME {&FRAME-NAME}:

  hBrwProcess = DYNAMIC-FUNCTION("NewBrowse"
    ,brwProcess:HANDLE
    ,100
    ,"TITLE|Workflow"
    ,"JBoxProcess"
      + ";cProcessDescription"
      + ";dCreated|Created"
      + ";cCreatedBy|By"
      + ";dModified|Modified"
      + ";cModifiedBy|Mod By"
      + ";iJBoxProcessId"
    ,"WHERE false"
    ,"").

  hBrwProcess:TOOLTIP = "Doubleclick to edit".

  DYNAMIC-FUNCTION("setSortString",hBrwProcess,"cProcessDescription").

  hProDesc = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwProcess,"cProcessDescription","cProcessDescription"
    ,"","","","").  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwProcess,hProDesc,"cProcessDescription").
  DYNAMIC-FUNCTION("setAttribute",hBrwProcess,"enableOnDblClick","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwProcess,"setReadOnlyOnReturn","yes").

  hTbProcess = DYNAMIC-FUNCTION("NewToolBar"
    ,tbProcess:HANDLE
    ,"File"
    ,"new,delete,excel"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrwProcess,hTbProcess).

  /* Process triggers / relations */

  hBrwProRel = DYNAMIC-FUNCTION("NewBrowse"
    ,brwProcessRel:HANDLE
    ,100
    ,"TITLE|Process triggers,MULTIPLE"
    ,"JBoxProcessRel"
     + ";cContext|Entity"
     + ";cEntityId"
     + ";+ProcessTrigger|CHARACTER|x(50)|jbadmin_jbprocessrel_desc.p|Description"
     + ";!iJBoxProcessRelId"
    ,"WHERE false"
    ,"").

  hBrwProRel:TOOLTIP = "Doubleclick to edit".


  DYNAMIC-FUNCTION("setSortString",hBrwProRel,"cContext").

  DYNAMIC-FUNCTION("setAttribute",hBrwProRel,"enableOnDblClick","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwProRel,"setReadOnlyOnReturn","yes").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwProRel,hBrwProcess,"iJBoxProcessId").

  hProRelCon = DYNAMIC-FUNCTION("NewBrowseDropDown",hBrwProRel,"cContext","cContext","","","JBoxEmailAccount|JBoxEmailAccount|JBoxEventLogType|JBoxEventLogType").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwProRel,hProRelCon,"cContext").
  DYNAMIC-FUNCTION("setAttribute",hProRelCon,"refreshRow","yes").

  hProRelEntId  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwProRel,"cEntityId","cEntityId"
                ,"lookup","WHERE false",""
                ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwProRel,hProRelEntId,"cEntityId").
  DYNAMIC-FUNCTION("setAttribute",hProRelEntId,"refreshRow","yes").

  hTbProRel = DYNAMIC-FUNCTION("NewToolBar"
    ,tbProcessRel:HANDLE
    ,"File"
    ,"new,delete,excel"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrwProRel,hTbProRel).

  /* Process steps */

  hBrwProStep = DYNAMIC-FUNCTION("NewBrowse"
    ,brwProcessStep:HANDLE
    ,100
    ,"TITLE|Process steps"
    ,"JBoxProcessStep"
    + ";iSeq"
    + ";cProcessStepDesc"
    + ";cProgramName"
    + ";bLogProcessStep"
    + ";dCreated|Created"
    + ";cCreatedBy|By"
    + ";dModified|Modified"
    + ";cModifiedBy|Mod By"
    + ";iJBoxProcessStepId"
    + ";iJBoxProcessId"
    ,"WHERE false"
    ,"").

  hBrwProStep:TOOLTIP = "Doubleclick to edit".

  DYNAMIC-FUNCTION("setSortString",hBrwProStep,"iSeq").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwProStep,hBrwProcess,"iJBoxProcessId").

  DYNAMIC-FUNCTION("setAttribute",hBrwProStep,"enableOnDblClick","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwProStep,"setReadOnlyOnReturn","yes").

  hProStepSeq = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwProStep,"iSeq","iSeq"
    ,"","","","").  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwProStep,hProStepSeq,"iSeq").

  hProStepDesc = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwProStep,"cProcessStepDesc","cProcessStepDesc"
    ,"","","","").  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwProStep,hProStepDesc,"cProcessStepDesc").

  hProStepProg = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwProStep,"cProgramName","cProgramName"
    ,"","","","").  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwProStep,hProStepProg,"cProgramName").

  hStepLogCol  = DYNAMIC-FUNCTION("getBrowseColumn",hBrwProStep,"bLogProcessStep").
/*   hStepLogFld  = hBrwProStep:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("bLogProcessStep").  */
  hProStepLog  = DYNAMIC-FUNCTION("NewBrowseToggle",hBrwProStep,"bLogProcessStep","bLogProcessStep","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwProStep,hProStepLog,"bLogProcessStep").
  hProStepLog:TOOLTIP = "Log process step".

  hTbProStep = DYNAMIC-FUNCTION("NewToolBar"
    ,tbProStep:HANDLE
    ,"File"
    ,"new,delete,excel"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hTbProStep,hBrwProStep).


  /* Process step parameters: */

  hBrwStepPar = DYNAMIC-FUNCTION("NewBrowse"
    ,brwProcessStepParam:HANDLE
    ,100
    ,"TITLE|Process step parameters"
    ,"JBoxProcessStepParam"
    + ";iSeq"
    + ";cParameterName"
    + ";cParameterText"
    + ";dParameterDate"
    + ";fParameterDec"
    + ";!iJBoxProcessRelId"
    + ";!iJBoxProcessStepId"
    + ";!iJBoxProcessStepParamId"
   + ",JBoxProcessRel"
    + ";+ProcessTrigger|CHARACTER|x(50)|jbadmin_jbprocessrel_desc.p|Process trigger@3"
    ,"WHERE false"
   + ",FIRST JBoxProcessRel NO-LOCK OF JBoxProcessStepParam OUTER-JOIN"
    ,"").

  hBrwStepPar:TOOLTIP = "Doubleclick to edit".

  DYNAMIC-FUNCTION("setSortString",hBrwStepPar,"iSeq").

  DYNAMIC-FUNCTION("setAttribute",hBrwStepPar,"enableOnDblClick","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwStepPar,"setReadOnlyOnReturn","yes").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwStepPar,hBrwProStep,"iJBoxProcessStepId").

  hStepParSeq = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwStepPar,"iSeq","iSeq"
               ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStepPar,hStepParSeq,"iSeq").

  hStepParName = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwStepPar,"cParameterName","cParameterName"
               ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStepPar,hStepParName,"cParameterName").

  hStepParTrg = DYNAMIC-FUNCTION("NewBrowseDropDown",hBrwStepPar,"ProcessTrigger","iJBoxProcessRelId","","","|0").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStepPar,hStepParTrg,"ProcessTrigger").

  hStepParText = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwStepPar,"cParameterText","cParameterText"
               ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStepPar,hStepParText,"cParameterText").

  hStepParDate = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwStepPar,"dParameterDate","dParameterDate"
               ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStepPar,hStepParDate,"dParameterDate").

  hStepParDec = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwStepPar,"fParameterDec","fParameterDec"
               ,"","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwStepPar,hStepParDec,"fParameterDec").


  hTbStepPar = DYNAMIC-FUNCTION("NewToolBar"
    ,tbProcessItemParam:HANDLE
    ,"File"
    ,"new,delete,excel"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrwStepPar,hTbStepPar).

  /* Process step log */

  hBrwStepLog = DYNAMIC-FUNCTION("NewBrowse"
    ,brwProcessStepLog:HANDLE
    ,100
    ,"TITLE|Process log,MULTIPLE"
    ,"JBoxProcessStepLog"
    + ";dCreated|Created"
    + ";+cCreTime|CHARACTER|x(8)|jb_hhmmss(iCreTime)|Time"
    + ";cCreatedBy|By"
    + ";cLogMessage"
  + ",JBoxProcessStep"
    + ";cProgramName"
  + ",JBoxProcess"
    + ";cProcessDescription"
    ,"WHERE false"
   + ",FIRST JBoxProcessStep NO-LOCK OF JBoxProcessStepLog"
   + ",FIRST JBoxProcess NO-LOCK OF JBoxProcessStep"
    ,"").

  DYNAMIC-FUNCTION("setSortString",hBrwStepLog,"dCreated;DESC,cCreTime;DESC").

  DYNAMIC-FUNCTION("setAttribute",hBrwStepLog,"allowDeleteKey","YES").
  DYNAMIC-FUNCTION("setAttribute",hBrwStepLog,"allowMultiDelete","YES").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwStepLog,hBrwProStep,"iJBoxProcessStepId").

  DYNAMIC-FUNCTION("NewMenuBand",hBrwStepLog
                  ,"MultiSortBrowse;Sort on multiple columns"
                 + ",Delete"
                  ,"").


  RUN InvokeMethod(hBrwProcess,"OpenQuery").
END.

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "brwProcess,brwProcessStep," + hBrwProcess:NAME + "," + hBrwProStep:NAME).
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "brwProcess,brwProcessStep,brwProcessRel,brwProcessStepParam,"
                + hBrwProcess:NAME + "," + hBrwProStep:NAME + "," + hBrwProRel:NAME + "," + hBrwStepPar:NAME).

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,400,0,0).

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
DEF VAR iNextSeq AS INT NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hTbProStep THEN
  iNextSeq = DYNAMIC-FUNCTION("getLocalQueryMaxValue",hBrwProStep,"iSeq",1).
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hTbStepPar THEN
  iNextSeq = DYNAMIC-FUNCTION("getLocalQueryMaxValue",hBrwStepPar,"iSeq",1).

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hTbProStep THEN DO:
  hProStepSeq:SCREEN-VALUE = STRING(iNextSeq).
  APPLY "tab" TO hProStepSeq.
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hTbStepPar THEN DO:
  hStepParSeq:SCREEN-VALUE = STRING(iNextSeq).
  APPLY "tab" TO hStepParSeq.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwProStep THEN
  ASSIGN hStepLogCol:FONT    = iFontWingdings
         hStepLogCol:FORMAT  = CHR(254) + "/"  + CHR(168)
         .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.

IF icBrowseName = "brwProcessStep" THEN DO:
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cProcessStepDesc").
  hColumn:WIDTH-PIXELS = 100.
END.
ELSE IF icBrowseName = "brwProcessStepParam" THEN DO:
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"ProcessTrigger").
  hColumn:WIDTH-PIXELS = 150.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cParameterText").
  hColumn:WIDTH-PIXELS = 150.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"dParameterDate").
  hColumn:WIDTH-PIXELS = 120.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

