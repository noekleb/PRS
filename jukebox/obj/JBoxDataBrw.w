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

DEF VAR bOk                  AS LOG    NO-UNDO.
DEF VAR ix                   AS INT    NO-UNDO.
DEF VAR iReturn              AS INT    NO-UNDO.
DEF VAR hParent              AS HANDLE NO-UNDO.
DEF VAR hParentBrowse        AS HANDLE NO-UNDO.  
DEF VAR hToolbar             AS HANDLE NO-UNDO.

DEF VAR hBrowse              AS HANDLE NO-UNDO.

DEF VAR cTotalMenu           AS CHAR   NO-UNDO.
DEF VAR cTotalFields         AS CHAR   NO-UNDO.
DEF VAR cTotalLabels         AS CHAR   NO-UNDO.
DEF VAR hTotalsMenu          AS HANDLE NO-UNDO.
DEF VAR bDecimalFields       AS LOG    NO-UNDO.
DEF VAR iRowsToBatch         AS INT    NO-UNDO.
DEF VAR bManualView          AS LOG    NO-UNDO.

DEF VAR cInitFilterFields    AS CHAR   NO-UNDO.
DEF VAR cInitFilterOperators AS CHAR   NO-UNDO.
DEF VAR cInitFilterValues    AS CHAR   NO-UNDO.
DEF VAR cInitCalcFieldProcs  AS CHAR   NO-UNDO.
DEF VAR cInitSort            AS CHAR   NO-UNDO.

DEF VAR cUseLocalData        AS CHAR   NO-UNDO.
DEF VAR cCustomFilter        AS CHAR   NO-UNDO.
DEF VAR cExcludeFields       AS CHAR   NO-UNDO.
DEF VAR bPrintButton         AS LOG    NO-UNDO.
DEF VAR bWordButton          AS LOG    NO-UNDO.
DEF VAR bEmailButton         AS LOG    NO-UNDO.
DEF VAR bSMSButton           AS LOG    NO-UNDO.
DEF VAR bAccumButton         AS LOG    NO-UNDO INIT TRUE.
DEF VAR bAccumFieldSort      AS LOG    NO-UNDO INIT TRUE.

DEF VAR cOrgWinTitle         AS CHAR   NO-UNDO.
DEF VAR cQueryTitle          AS CHAR   NO-UNDO.
DEF VAR cExcludedList        AS CHAR   NO-UNDO.
DEF VAR bDisableSaveFilter   AS LOG    NO-UNDO.
DEF VAR bEnableSaveQuery     AS LOG    NO-UNDO.
DEF VAR hMyQueriesMenu       AS HANDLE NO-UNDO.
DEF VAR cMyQueryList         AS CHAR   NO-UNDO.
DEF VAR cCurrQueryId         AS CHAR   NO-UNDO.
DEF VAR cMenuIdList          AS CHAR   NO-UNDO.
DEF VAR cMenuNameList        AS CHAR   NO-UNDO.
DEF VAR hBtnSaveQuery        AS HANDLE NO-UNDO.
DEF VAR hBtnDeleteQuery      AS HANDLE NO-UNDO.
DEF VAR hFilterBtn           AS HANDLE NO-UNDO.
DEF VAR cActiveFilterBtn     AS CHAR   NO-UNDO.
DEF VAR cPassiveFilterBtn    AS CHAR   NO-UNDO.
DEF VAR hAccumBtn            AS HANDLE NO-UNDO.
DEF VAR cActiveAccumBtn      AS CHAR   NO-UNDO.
DEF VAR cPassiveAccumBtn     AS CHAR   NO-UNDO.
DEF VAR hConfigBtn           AS HANDLE NO-UNDO.
DEF VAR cActiveConfigBtn     AS CHAR   NO-UNDO.
DEF VAR cPassiveConfigBtn    AS CHAR   NO-UNDO.
DEF VAR cQuerySettingList    AS CHAR   NO-UNDO.
DEF VAR cCurrQuerySettings   AS CHAR   NO-UNDO.
DEF VAR cOrgViewFields       AS CHAR   NO-UNDO.
DEF VAR hExcelGraph          AS HANDLE NO-UNDO.
DEF VAR cGraphMenu           AS CHAR   NO-UNDO.
DEF VAR cGraphLabels         AS CHAR   NO-UNDO.
DEF VAR cGraphFields         AS CHAR   NO-UNDO.
DEF VAR cCurrGraphField      AS CHAR   NO-UNDO.
DEF VAR iMaxGraph            AS INT    NO-UNDO INIT 50.
DEF VAR cCurrFlatDistinct    AS CHAR   NO-UNDO.
DEF VAR cCurrFlatAccum       AS CHAR   NO-UNDO.
DEF VAR bUseStdAppServer     AS LOG    NO-UNDO.
DEF VAR bNoDrillDown         AS LOG    NO-UNDO.
DEF VAR cAccumTotalFields    AS CHAR   NO-UNDO.
DEF VAR cTitlePrefixTotals   AS CHAR   NO-UNDO.
DEF VAR cTitlePrefixDetails  AS CHAR   NO-UNDO.
DEF VAR hMenuItemCountRows   AS HANDLE NO-UNDO.
DEF VAR oDynDataBrowse       AS JBoxDynDataBrowse NO-UNDO.

cQuerySettingList = "OperatorInUse_,GroupOperator_,FieldGroup_,FieldOperator_,OperatorInUse_,FieldOperator_,FilterValue_,HiddenGroupOperator_,queryfilter,prescanquery,calcfieldfilter,advancedfilter,getrecordcount"
/*                    + ",allaccumfields,allcalcfields,prescanmainquerycandidates_filter" /* <- to keep from clearing */  */
                   + ",currviewfields,widthpixellist,distinctcolumns,accumfields,accumsortfields,accumviewfields"
                   + ",querysort,localsort,querydesc,1stsortcolumn,1stsortcolumndesc,2ndsortcolumn,2ndsortcolumndesc,3rdsortcolumn,3rdsortcolumndesc,4thsortcolumn,4thsortcolumndesc"
                     .

DEF TEMP-TABLE ttChild
    FIELD hChild AS HANDLE.

/* {pdfprint.i}  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwFlatView searchField rectToolbar ~
rectWinToolbar fiCurrentQuery 
&Scoped-Define DISPLAYED-OBJECTS fiCurrentQuery 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearAttributes C-Win 
FUNCTION ClearAttributes RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearUseIndex C-Win 
FUNCTION ClearUseIndex RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateSearchField C-Win 
FUNCTION CreateSearchField RETURNS LOGICAL
  ( INPUT icInitSort AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseHandle C-Win 
FUNCTION getBrowseHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDynDataBrowseObject C-Win 
FUNCTION getDynDataBrowseObject RETURNS JBoxDynDataBrowse
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolbarHandle C-Win 
FUNCTION getToolbarHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReduceFields C-Win 
FUNCTION ReduceFields RETURNS CHARACTER
  ( INPUT icBuffersAndFields AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveQuery C-Win 
FUNCTION SaveQuery RETURNS LOGICAL
  ( INPUT icCurrQueryId AS CHAR,
    INPUT icName        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SeqToAlpha C-Win 
FUNCTION SeqToAlpha RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT,
    INPUT iCount  AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAccumButton C-Win 
FUNCTION setAccumButton RETURNS LOGICAL
  ( INPUT ibAccumButton AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAccumFieldSort C-Win 
FUNCTION setAccumFieldSort RETURNS LOGICAL
  ( INPUT ibAccumFieldSort AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCalcFieldProcs C-Win 
FUNCTION setCalcFieldProcs RETURNS LOGICAL
  ( INPUT icCalcFieldProcs AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCustomFilterWin C-Win 
FUNCTION setCustomFilterWin RETURNS LOGICAL
  ( INPUT icCustomFilter AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDisableSaveFilter C-Win 
FUNCTION setDisableSaveFilter RETURNS LOGICAL
  ( INPUT ibDisableSaveFilter AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEmailButton C-Win 
FUNCTION setEmailButton RETURNS LOGICAL
  ( INPUT ibEmailButton AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEnableSaveQuery C-Win 
FUNCTION setEnableSaveQuery RETURNS LOGICAL
  ( INPUT ibEnableSaveQuery AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setExcludeFields C-Win 
FUNCTION setExcludeFields RETURNS LOGICAL
  ( icExcludeFields AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setInitFilter C-Win 
FUNCTION setInitFilter RETURNS LOGICAL
  ( INPUT icInitFilterFields    AS CHAR,
    INPUT icInitFilterOperators AS CHAR,
    INPUT icInitFilterValues    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setManualView C-Win 
FUNCTION setManualView RETURNS LOGICAL
  ( INPUT ibManualView AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMyQueriesMenu C-Win 
FUNCTION setMyQueriesMenu RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoDrillDown C-Win 
FUNCTION setNoDrillDown RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPrintButton C-Win 
FUNCTION setPrintButton RETURNS LOGICAL
  ( INPUT ibPrintButton AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRowsToBatch C-Win 
FUNCTION setRowsToBatch RETURNS LOGICAL
  ( INPUT iiRowsToBatch AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSMSButton C-Win 
FUNCTION setSMSButton RETURNS LOGICAL
  ( INPUT ibSMSButton AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTotalFields C-Win 
FUNCTION setTotalFields RETURNS LOGICAL
  ( INPUT icTotalFields AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTotalFieldsMenu C-Win 
FUNCTION setTotalFieldsMenu RETURNS LOGICAL
  ( INPUT icViewType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUseLocalData C-Win 
FUNCTION setUseLocalData RETURNS LOGICAL
  ( ibUseLocalData AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUseStdAppserver C-Win 
FUNCTION setUseStdAppserver RETURNS LOGICAL
  ( INPUT ibUseStdAppServer AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWinTitle C-Win 
FUNCTION setWinTitle RETURNS LOGICAL
  ( INPUT icWinTitle AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWordButton C-Win 
FUNCTION setWordButton RETURNS LOGICAL
  ( INPUT ibWordButton AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StartQuery C-Win 
FUNCTION StartQuery RETURNS LOGICAL
  ( INPUT icFilterFields    AS CHAR,
    INPUT icFilterOperators AS CHAR,
    INPUT icFilterValues    AS CHAR,
    INPUT icInitSort        AS CHAR,
    INPUT icDistinctCols    AS CHAR,
    INPUT icAccumFields     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StartQueryCls C-Win 
FUNCTION StartQueryCls RETURNS LOGICAL
  ( INPUT icFilterFields    AS CHAR,
    INPUT icFilterOperators AS CHAR,
    INPUT icFilterValues    AS CHAR,
    INPUT icInitSort        AS CHAR,
    INPUT icDistinctCols    AS CHAR,
    INPUT icAccumFields     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiCurrentQuery AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 42 BY .86 NO-UNDO.

DEFINE RECTANGLE BrwFlatView
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129.4 BY 15.71.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY .91.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 4.2 BY .91.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.8 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiCurrentQuery AT ROW 1.38 COL 73 COLON-ALIGNED
     BrwFlatView AT ROW 2.67 COL 1.6
     searchField AT ROW 1.33 COL 2.2
     rectToolbar AT ROW 1.33 COL 20.8
     rectWinToolbar AT ROW 1.33 COL 126.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.6 BY 17.62.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 17.62
         WIDTH              = 130.6
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

{incl\jb_shared.i}
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
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* <insert window title> */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCurrentQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCurrentQuery C-Win
ON ANY-PRINTABLE OF fiCurrentQuery IN FRAME DEFAULT-FRAME /* Fill 1 */
DO:
  IF fiCurrentQuery:MODIFIED THEN
    hBtnSaveQuery:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCurrentQuery C-Win
ON BACKSPACE OF fiCurrentQuery IN FRAME DEFAULT-FRAME /* Fill 1 */
DO:
  hBtnSaveQuery:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCurrentQuery C-Win
ON DELETE-CHARACTER OF fiCurrentQuery IN FRAME DEFAULT-FRAME /* Fill 1 */
DO:
  hBtnSaveQuery:SENSITIVE = YES.
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
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  FOR EACH ttChild:
    IF VALID-HANDLE(ttChild.hChild) THEN 
      APPLY "close" TO ttChild.hChild.
  END.
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
/*     RUN InitializeObject ("Hovedbok","WHERE true","iHovedbokId",TRUE). */
    RUN InitializeObject ("FakturaLinje","WHERE ifakturalinjeid > 30","iFakturaHodeId",TRUE).
  &ENDIF



  SUBSCRIBE TO "InitStringTranslation" ANYWHERE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}

ON 'ctrl-s' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF cCurrQueryId NE "" THEN RUN SaveCurrentQuery.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeSetDynFilter C-Win 
PROCEDURE BeforeSetDynFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihQueryObject AS HANDLE NO-UNDO.

IF ihQueryObject NE hBrowse THEN RETURN.

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"uselocaldata") = "yes" AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"tempTableHandle") = "" THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyToClipboardRecord C-Win 
PROCEDURE CopyToClipboardRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("CopyBrowseToClipboard",hBrowse).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CountRecord C-Win 
PROCEDURE CountRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hMenuItemCountRows:CHECKED THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getRecordCount","yes").
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.
ELSE 
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getRecordCount","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:    If the browse is showing distinct values allow drill-down
              if the distinct columns is contained within the basetablefields 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cDistinctColumns       AS CHAR   NO-UNDO.
DEF VAR cBaseTableFilterFields AS CHAR   NO-UNDO.
DEF VAR hCurrFlatView          AS HANDLE NO-UNDO.
DEF VAR hCurrFlatBrw           AS HANDLE NO-UNDO.

ASSIGN cDistinctColumns = DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctcolumns")
       cBaseTableFilterFields = DYNAMIC-FUNCTION("getAttribute",hBrowse,"basetablefilterfields")
       .
       
IF cDistinctColumns NE "" THEN DO:
  hCurrFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"flatviewhandle")) NO-ERROR.
  IF VALID-HANDLE(hCurrFlatView) THEN DO:
    hCurrFlatBrw = DYNAMIC-FUNCTION("getBrowseHandle" IN hCurrFlatView).
    CREATE ttChild.
    ttChild.hChild = hCurrFlatView.

    ASSIGN cCurrFlatDistinct = DYNAMIC-FUNCTION("getAttribute",hCurrFlatBrw,"distinctcolumns")
           cCurrFlatAccum    = DYNAMIC-FUNCTION("getAttribute",hCurrFlatBrw,"accumfields").  
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"flatviewhandle","").
  END.
  ELSE DO: 
    bOk = NO.
    SjekkChild:
    FOR EACH ttChild:
      IF VALID-HANDLE(ttChild.hChild) THEN DO:
        hCurrFlatBrw = DYNAMIC-FUNCTION("getBrowseHandle" IN ttChild.hChild).
        ASSIGN cCurrFlatDistinct = DYNAMIC-FUNCTION("getAttribute",hCurrFlatBrw,"distinctcolumns")
               cCurrFlatAccum    = DYNAMIC-FUNCTION("getAttribute",hCurrFlatBrw,"accumfields").
        IF cCurrFlatDistinct NE "" THEN DO:
          bOk = YES.  
          LEAVE SjekkChild.
        END.
      END.
    END.
    IF NOT bOk THEN
      ASSIGN cCurrFlatDistinct = ""
             cCurrFlatAccum    = "".
  END.

  RUN FlatViewRecord.
END.
ELSE 
  PUBLISH "FlatViewDblClick" (ENTRY(1,ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields")),";"),
                                 hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteQuery C-Win 
PROCEDURE DeleteQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SaveQuery(cCurrQueryId,"delete_setting").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectRowRecord C-Win 
PROCEDURE DeselectRowRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hRecordSelectWidget AS HANDLE NO-UNDO.

hBrowse:DESELECT-ROWS().

hRecordSelectWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"RecordSelectWidget")) NO-ERROR.
IF VALID-HANDLE(hRecordSelectWidget) THEN 
  hRecordSelectWidget:SCREEN-VALUE = "".

IF VALID-HANDLE(hExcelGraph) THEN 
  hExcelGraph:SENSITIVE = INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")) < iMaxGraph AND NUM-ENTRIES(DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctcolumns")) = 1.


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
PUBLISH "BeforeDisplayRecord" (hBrowse,hToolbar).

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DynAccumDone C-Win 
PROCEDURE DynAccumDone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM icAction AS CHAR   NO-UNDO.

DEF VAR hFilter                  AS HANDLE NO-UNDO.
DEF VAR cPermanentDisabledEvents AS CHAR   NO-UNDO.
DEF VAR cTitlePrefix             AS CHAR   NO-UNDO.
DEF VAR cTitleSuffix             AS CHAR   NO-UNDO.
DEF VAR cDistinctCols            AS CHAR   NO-UNDO.
DEF VAR cButton                  AS CHAR   NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

cPermanentDisabledEvents = DYNAMIC-FUNCTION("getAttribute",hToolbar,"permanentdisabledevents").

IF icAction = "accum" THEN DO:
  ASSIGN cDistinctCols = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctcolumns")
         cButton = DYNAMIC-FUNCTION("getAttribute",SESSION,"activeaccumbutton").

  IF CAN-DO(cPermanentDisabledEvents,"filter") THEN DO:
    cTitlePrefix = cTitlePrefixTotals + " ".
/*     IF DYNAMIC-FUNCTION("Scandinavian") THEN */
/*       cTitlePrefix  = "Totalt pr ".          */
/*     ELSE                                     */
/*       cTitlePrefix  = "Totals pr ".          */
  END.
  ELSE DO:
    cTitleSuffix = " - " + cTitlePrefixTotals + " ".
/*     IF DYNAMIC-FUNCTION("Scandinavian") THEN */
/*       cTitleSuffix  = " - Totalt pr ".       */
/*     ELSE                                     */
/*       cTitleSuffix  = " - Totals pr ".       */
  END.

  DO ix = 1 TO NUM-ENTRIES(cDistinctCols):
    IF CAN-DO(cPermanentDisabledEvents,"filter") THEN 
      cTitlePrefix = cTitlePrefix + DYNAMIC-FUNCTION("getBrowseColumnLabel",ihBrowse,ENTRY(ix,cDistinctCols)) + 
                     IF ix = NUM-ENTRIES(cDistinctCols) THEN ". " ELSE ", ".
    ELSE
      cTitleSuffix = cTitleSuffix + DYNAMIC-FUNCTION("getBrowseColumnLabel",ihBrowse,ENTRY(ix,cDistinctCols)) + 
                     IF ix = NUM-ENTRIES(cDistinctCols) THEN ". " ELSE ", ".
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:TITLE = cTitlePrefix + (IF cQueryTitle NE "" THEN cQueryTitle ELSE cOrgWinTitle) + cTitleSuffix.

  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents",TRIM(cPermanentDisabledEvents + ",word,browseconfig",",")).
  hFilter = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"filterhandle")) NO-ERROR.
  IF VALID-HANDLE(hFilter) THEN APPLY "close" TO hFilter.
  setTotalFieldsMenu("accum").
END.
ELSE DO:
  THIS-PROCEDURE:CURRENT-WINDOW:TITLE = cOrgWinTitle.
  cButton = DYNAMIC-FUNCTION("getAttribute",SESSION,"passiveaccumbutton").
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents",cPermanentDisabledEvents).
  setTotalFieldsMenu("details").
END.

IF VALID-HANDLE(hAccumBtn) THEN 
  hAccumBtn:LOAD-IMAGE(cButton).
       
APPLY "value-changed" TO hBrowse.

APPLY "window-resized" TO {&WINDOW-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE emailRecord C-Win 
PROCEDURE emailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "FlatViewEmailRecord" (ENTRY(1,ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields")),";"),
                               hBrowse).

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
  DISPLAY fiCurrentQuery 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BrwFlatView searchField rectToolbar rectWinToolbar fiCurrentQuery 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelGraph C-Win 
PROCEDURE ExcelGraph :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hMenuItem         AS HANDLE NO-UNDO.
DEF VAR cDistinctField    AS CHAR   NO-UNDO.
DEF VAR iOrgPosDistinct   AS INT    NO-UNDO.
DEF VAR iOrgPosGraphField AS INT    NO-UNDO.

ASSIGN hMenuItem       = DYNAMIC-FUNCTION("getCurrentWidget")
       cCurrGraphField = ENTRY(LOOKUP(hMenuItem:LABEL,cGraphLabels),cGraphFields)
       cDistinctField  = DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctcolumns")
       .

DO ix = 1 TO hBrowse:NUM-COLUMNS:
  IF ix > 1 AND hBrowse:GET-BROWSE-COLUMN(ix):NAME = cDistinctField THEN DO:
    hBrowse:MOVE-COLUMN(ix,1).  
    iOrgPosDistinct = ix.
    LEAVE.
  END.
END.
DO ix = 1 TO hBrowse:NUM-COLUMNS:
  IF ix NE 2 AND hBrowse:GET-BROWSE-COLUMN(ix):NAME = cCurrGraphField THEN DO:
    hBrowse:MOVE-COLUMN(ix,2).  
    iOrgPosGraphField = ix.
    LEAVE.
  END.
END.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN ExcelRecord.

cCurrGraphField = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"useExcelViewer") = "yes" THEN
  PUBLISH "ExcelDataReady" (hBrowse).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelSheetParams C-Win 
PROCEDURE ExcelSheetParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse           AS HANDLE     NO-UNDO.
DEF INPUT PARAM chExcelApplication AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorkbook         AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM chWorksheet        AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM iCount             AS INT        NO-UNDO.

DEF VAR chChart              AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheetRange     AS COM-HANDLE NO-UNDO.
DEF VAR cRange               AS CHAR       NO-UNDO.
DEF VAR cAccumFields         AS CHAR       NO-UNDO.
DEF VAR cDistinctFields      AS CHAR       NO-UNDO.
DEF VAR cYtext               AS CHAR       NO-UNDO.
DEF VAR iRange               AS INT        NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

IF cCurrGraphField = "" THEN RETURN.

IF VALID-HANDLE(hParent) AND CAN-DO(hParent:INTERNAL-ENTRIES,"ExcelSheetParams") AND hParent:FILE-NAME NE THIS-PROCEDURE:FILE-NAME THEN RETURN.

cRange = "A1:B" + STRING(iCount).

chWorksheetRange = chWorksheet:Range(cRange).
chWorksheet:ChartObjects:Add(10,150,425,300):Activate.
chExcelApplication:ActiveChart:ChartWizard(chWorksheetRange, 3, 1, 2, 1, 1, TRUE,
    IF NOT fiCurrentQuery:HIDDEN IN FRAME {&FRAME-NAME} THEN fiCurrentQuery:SCREEN-VALUE ELSE THIS-PROCEDURE:CURRENT-WINDOW:TITLE,
    "",
    "").

chExcelApplication:VISIBLE = TRUE.
chExcelApplication:ActiveChart:PrintPreview(0).

/* release com-handles */
RELEASE OBJECT chChart NO-ERROR.
RELEASE OBJECT chWorksheetRange NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFlatView  AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG    NO-UNDO.

DEF VAR hNewBrwPlaceHolder  AS HANDLE NO-UNDO.
DEF VAR cBaseQuery          AS CHAR   NO-UNDO.
DEF VAR cQueryFilter        AS CHAR   NO-UNDO.
DEF VAR cBrowseColumn       AS CHAR   NO-UNDO.
DEF VAR cBufferColumn       AS CHAR   NO-UNDO.
DEF VAR cDistinctCols       AS CHAR   NO-UNDO.
DEF VAR cCalcFields         AS CHAR   NO-UNDO.
DEF VAR cBuffDistinct       AS CHAR   NO-UNDO.
DEF VAR cCalcDistinct       AS CHAR   NO-UNDO.
DEF VAR cBufferList         AS CHAR   NO-UNDO.
DEF VAR cFieldBuffer        AS CHAR   NO-UNDO.
DEF VAR cDBfield            AS CHAR   NO-UNDO.
DEF VAR cCalcFieldFilter    AS CHAR   NO-UNDO.
DEF VAR cBuffPreScanQry     AS CHAR   NO-UNDO EXTENT 20.
DEF VAR cPreScanValue       AS CHAR   NO-UNDO.
DEF VAR cCurrPreScanQry     AS CHAR   NO-UNDO.
DEF VAR cPrescanQuery       AS CHAR   NO-UNDO.
DEF VAR cPreScanQueryFilter AS CHAR   NO-UNDO.
DEF VAR cWindowTitle        AS CHAR   NO-UNDO.
DEF VAR hNewBrowse          AS HANDLE NO-UNDO.
DEF VAR hNewFilterButton    AS HANDLE NO-UNDO.
DEF VAR hNewToolbar         AS HANDLE NO-UNDO.
DEF VAR cActiveFilterButton AS CHAR   NO-UNDO.
DEF VAR cCalcFilterTitle    AS CHAR   NO-UNDO.
DEF VAR iz                  AS INT    NO-UNDO.
DEF VAR cFilterExclude      AS CHAR   NO-UNDO.
DEF VAR cOperatorAttributes AS CHAR   NO-UNDO.
DEF VAR cFieldsForOperators AS CHAR   NO-UNDO.
DEF VAR cFieldsInUse        AS CHAR   NO-UNDO.
DEF VAR cOperatorsInUse     AS CHAR   NO-UNDO.
DEF VAR hColumn             AS HANDLE NO-UNDO.
DEF VAR cLastPreScan        AS CHAR   NO-UNDO.
DEF VAR cModBuffsAndFlds    AS CHAR   NO-UNDO.
DEF VAR cModQueryJoin       AS CHAR   NO-UNDO.
DEF VAR cModBaseQuery       AS CHAR   NO-UNDO.
DEF VAR cModSkipUnique      AS CHAR   NO-UNDO.
DEF VAR cNewCurrFields      AS CHAR   NO-UNDO.
DEF VAR cModCurrFields      AS CHAR   NO-UNDO.
DEF VAR cCurrViewFields     AS CHAR   NO-UNDO.

IF bNoDrillDown THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                     "Drill-down er ikke tilgjengelig for denne spørringen"
                    ELSE
                     "Drill-down not available for this query"),"","").
  obOk = NO.
  RETURN.
END.

cCurrViewFields = DYNAMIC-FUNCTION("getAttribute",hBrowse,"currviewfields").

/* If the buffer sequence has been modified for query speed, swap back to design sequence here: */
IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgbuffersandfields") NE "" THEN DO:
  ASSIGN cModBuffsAndFlds = DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields")
         cModQueryJoin    = DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryjoin")
         cModBaseQuery    = DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery")
         cModSkipUnique   = DYNAMIC-FUNCTION("getAttribute",hBrowse,"skipuniquerows")
         .
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"SkipUniqueRows","").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"buffersandfields",DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgbuffersandfields")).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryjoin",DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgqueryjoin")).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery",DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgbasequery")).
END.

ASSIGN hNewBrwPlaceHolder = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView)
       cBaseQuery         = DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery")
       cQueryFilter       = DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter")
       cDistinctCols      = DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctcolumns")
       cCalcFields        = DYNAMIC-FUNCTION("getAttribute",hBrowse,"allcalcfields")
       cBufferList        = DYNAMIC-FUNCTION("getAttribute",hBrowse,"bufferlist")
       cBuffDistinct      = IF cBaseQuery MATCHES "*WHERE *" THEN " AND" ELSE " WHERE"
       cFilterExclude     = DYNAMIC-FUNCTION("getAttribute",hBrowse,"filterexcludefields")
       .

DO ix = 1 TO hBrowse:NUM-COLUMNS:
  ASSIGN
    cBrowseColumn = hBrowse:GET-BROWSE-COLUMN(ix):NAME
    cBufferColumn = IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgdbfield" + hBrowse:GET-BROWSE-COLUMN(ix):NAME) NE "" THEN 
                      DYNAMIC-FUNCTION("getAttribute",hBrowse,"orgdbfield" + hBrowse:GET-BROWSE-COLUMN(ix):NAME)
                    ELSE
                      cBrowseColumn
    cFieldBuffer  = DYNAMIC-FUNCTION("getAttribute",hBrowse,"fieldbuffer" + cBrowseColumn)
                    .
  IF CAN-DO(cDistinctCols,cBrowseColumn) AND NOT CAN-DO(cCalcFields,cBrowseColumn) AND LOOKUP(cFieldBuffer,cBufferList) = 1 THEN DO:
    ASSIGN cBuffDistinct  = cBuffDistinct + " " + cBrowseColumn + " = "
           cFilterExclude = cFilterExclude + "," + cBrowseColumn.
    CASE hBrowse:GET-BROWSE-COLUMN(ix):DATA-TYPE:
      WHEN "CHARACTER" THEN cBuffDistinct = cBuffDistinct + QUOTER(hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE).
      WHEN "DATE"      THEN cBuffDistinct = cBuffDistinct + "DATE('" + hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE + "')".
      WHEN "DECIMAL"   THEN cBuffDistinct = cBuffDistinct + "DECIMAL(" + hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE + ")".
      WHEN "INTEGER"   THEN cBuffDistinct = cBuffDistinct + hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE.
      WHEN "LOGICAL"   THEN cBuffDistinct = cBuffDistinct + "LOGICAL(" + hBrowse:GET-BROWSE-COLUMN(ix):INPUT-VALUE + ")".
    END CASE.
    cBuffDistinct = cBuffDistinct + " AND".
  END.
  ELSE IF CAN-DO(cDistinctCols,cBrowseColumn) AND CAN-DO(cCalcFields,cBrowseColumn) THEN 
    ASSIGN cCalcFieldFilter = cCalcFieldFilter + cBrowseColumn + "¤=¤" + REPLACE(hBrowse:GET-BROWSE-COLUMN(ix):INPUT-VALUE,",",CHR(1)) + "|"
           cFilterExclude   = cFilterExclude + "," + cBrowseColumn
           cCalcFilterTitle = cCalcFilterTitle + " AND " +
                       (IF cBrowseColumn BEGINS "jb_month_" THEN "MONTH(" + SUBSTR(cBrowseColumn,10) + ")"
                        ELSE IF cBrowseColumn BEGINS "jb_quarter_" THEN "QUARTER(" + SUBSTR(cBrowseColumn,12) + ")"
                        ELSE IF cBrowseColumn BEGINS "jb_week_" THEN "WEEK(" + SUBSTR(cBrowseColumn,9) + ")"
                        ELSE IF cBrowseColumn BEGINS "jb_year_" THEN "YEAR(" + SUBSTR(cBrowseColumn,9) + ")"
                        ELSE cBrowseColumn) + " = " +
                        hBrowse:GET-BROWSE-COLUMN(ix):INPUT-VALUE.
  ELSE IF CAN-DO(cDistinctCols,cBrowseColumn) AND LOOKUP(cFieldBuffer,cBufferList) > 1 THEN DO:
    cFilterExclude = cFilterExclude + "," + cBrowseColumn.
    CASE hBrowse:GET-BROWSE-COLUMN(ix):DATA-TYPE:
      WHEN "CHARACTER" THEN cPreScanValue = QUOTER(REPLACE(hBrowse:GET-BROWSE-COLUMN(ix):INPUT-VALUE,",",CHR(1))).
      WHEN "DATE"      THEN cPreScanValue = "DATE('" + hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE + "')".
      WHEN "DECIMAL"   THEN cPreScanValue = "DECIMAL(" + hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE + ")".
      WHEN "INTEGER"   THEN cPreScanValue = hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE.
      WHEN "LOGICAL"   THEN cPreScanValue = "LOGICAL(" + hBrowse:GET-BROWSE-COLUMN(ix):INPUT-VALUE + ")".
    END CASE.
    cBuffPreScanQry[LOOKUP(cFieldBuffer,cBufferList)] = cBuffPreScanQry[LOOKUP(cFieldBuffer,cBufferList)] +
       (IF cBuffPreScanQry[LOOKUP(cFieldBuffer,cBufferList)] NE "" THEN " AND " ELSE cFieldBuffer + ".") +
       cBufferColumn + " = " + REPLACE(cPreScanValue,",",CHR(1)).
  END.

  IF CAN-DO(cDistinctCols,cBrowseColumn) THEN
    cWindowTitle = cWindowTitle + DYNAMIC-FUNCTION("getBrowseColumnLabel",hBrowse,cBrowseColumn) + " = " + hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE + ", ". 
  
END.
IF LENGTH(cBuffDistinct) > 6 THEN
  cBuffDistinct = RIGHT-TRIM(cBuffDistinct," AND").
ELSE cBuffDistinct = "".

DO ix = 1 TO 20:
  IF cBuffPreScanQry[ix] NE "" THEN DO:
    IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"prescanquery" + ENTRY(ix,cBufferList)) NE "" THEN DO:
      cPrescanQuery = DYNAMIC-FUNCTION("getAttribute",hBrowse,"prescanquery" + ENTRY(ix,cBufferList)).
      DO iz = 1 TO 5:
        cPreScanQuery = REPLACE(cPreScanQuery,"  "," ").
      END.

      ASSIGN cPreScanQuery   = REPLACE(cPreScanQuery,",FIRST","¤FIRST")
             cPreScanQuery   = REPLACE(cPreScanQuery,", FIRST","¤FIRST")
             cPreScanQuery   = REPLACE(cPreScanQuery,",LAST","¤LAST")
             cPreScanQuery   = REPLACE(cPreScanQuery,", LAST","¤LAST")
             cPreScanQuery   = REPLACE(cPreScanQuery,",EACH","¤EACH")
             cPreScanQuery   = REPLACE(cPreScanQuery,", EACH","¤EACH")
             cPrescanQuery = ENTRY(ix,cBufferList) + " WHERE " + cBuffPreScanQry[ix] + "," + cPrescanQuery
             .
      DO iz = 1 TO NUM-ENTRIES(cPreScanQuery,"¤"):
        IF iz < NUM-ENTRIES(cPreScanQuery,"¤") THEN
          cCurrPreScanQry = cCurrPreScanQry + ENTRY(iz,cPreScanQuery,"¤") + ",".
        ELSE
          cPreScanQuery = ENTRY(iz,cPreScanQuery,"¤").
      END.
      ASSIGN cLastPreScan  = cPrescanQuery
             cPreScanQuery = cPrescanQuery + " " + (IF cBaseQuery   BEGINS "WHERE " AND cLastPreScan MATCHES "* WHERE *" THEN "AND " + SUBSTR(cBaseQuery,7) ELSE cBaseQuery)  
             cPreScanQuery = cPrescanQuery + " " + (IF cQueryFilter BEGINS "WHERE " AND cLastPreScan MATCHES "* WHERE *" THEN "AND " + SUBSTR(cQueryFilter,7) ELSE cQueryFilter)  
             cCurrPreScanQry = cCurrPreScanQry + cPrescanQuery + CHR(28).
    END.
    ELSE 
      ASSIGN cPrescanQuery = ENTRY(ix,cBufferList) + " WHERE " + cBuffPreScanQry[ix] + ",EACH " + DYNAMIC-FUNCTION("getAttribute",hBrowse,"basetable") + " NO-LOCK OF " + ENTRY(ix,cBufferList)
             cCurrPreScanQry = cCurrPreScanQry + cPrescanQuery + " " + DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") + DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") + CHR(28)
             .

/*     cWindowTitle    = cWindowTitle + ENTRY(1,cPrescanQuery) + ", ". */
  END.
END.

cOperatorAttributes = DYNAMIC-FUNCTION("getAttributeList",hBrowse,"operatorInUse_","",NO).
IF cOperatorAttributes NE "|" THEN DO:
  ASSIGN cFieldsForOperators = ENTRY(1,cOperatorAttributes,"|")
         cOperatorAttributes = ENTRY(2,cOperatorAttributes,"|").
  DO ix = 1 TO NUM-ENTRIES(cFieldsForOperators):
    cWindowTitle = cWindowTitle + DYNAMIC-FUNCTION("getBrowseColumnLabel",hBrowse,
                                                   SUBSTR(ENTRY(ix,cFieldsForOperators),R-INDEX(ENTRY(ix,cFieldsForOperators),"_") + 1))
                                + " " + ENTRY(ix,cOperatorAttributes,CHR(1))
                                + " " + DYNAMIC-FUNCTION("getAttribute",hBrowse,"filtervalue" +
                                                         SUBSTR(ENTRY(ix,cFieldsForOperators),INDEX(ENTRY(ix,cFieldsForOperators),"_")))
                                + ", ".
  END.
  cWindowTitle = REPLACE(TRIM(cWindowTitle,", "),CHR(1),",").
END.
ELSE
  cWindowTitle = TRIM(
                 TRIM(cWindowTitle + DYNAMIC-FUNCTION("getAttribute",hBrowse,"prescanqueryfilter"),",")
                    + DYNAMIC-FUNCTION("getAttribute",hBrowse,"basetable") + " "
                    + (IF TRIM(DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") + DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter")) = "where true" THEN " WHERE "
                       ELSE DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") + DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") + " ")
                    + TRIM(cBuffDistinct,"AND ") + " "
                    + cCalcFilterTitle
                 ,"where ")
                  .

DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"prescanqueryfilter",TRIM(cCurrPreScanQry + CHR(28) + DYNAMIC-FUNCTION("getAttribute",hBrowse,"prescanqueryfilter"),CHR(28))).
DO ix = 1 TO NUM-ENTRIES(cBufferList):
  DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"prescanquery" + ENTRY(ix,cBufferList),DYNAMIC-FUNCTION("getAttribute",hBrowse,"prescanquery" + ENTRY(ix,cBufferList))).
END.

DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"availdistinctcolumns",DYNAMIC-FUNCTION("getAttribute",hBrowse,"availdistinctcolumns")).
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"availaccumfields",DYNAMIC-FUNCTION("getAttribute",hBrowse,"availaccumfields")).
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"accumdatatypes",DYNAMIC-FUNCTION("getAttribute",hBrowse,"accumdatatypes")).
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"distinctdatatypes",DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctdatatypes")).
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"calcfieldfilter",TRIM(cCalcFieldFilter + "|" + DYNAMIC-FUNCTION("getAttribute",hBrowse,"calcfieldfilter"),"|")).

DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"originaltemptable",DYNAMIC-FUNCTION("getAttribute",hBrowse,"originaltemptable")).
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"distinctcolumns",cCurrFlatDistinct).
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"accumfields",cCurrFlatAccum).
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"filterexcludefields",TRIM(cFilterExclude,",")).

DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FieldGroup_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FieldOperator_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FilterLookupFields_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FilterLookupQuery_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FilterLookupReturnField_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FilterValue_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FilterDropdownFields_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FilterDropdownQuery_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"FilterDropDownValueList_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"GroupOperator_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"HiddenGroupOperator_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"OperatorInUse_").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"localsort").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"disabledrilldownfilter").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"usersettingcontext").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"prescanlimit").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"calcparam").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"calcphrase").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"shadedRows").
DYNAMIC-FUNCTION("CopyAttributes",hBrowse,hNewBrwPlaceHolder,"WindowsBrowse").

ihFlatView:CURRENT-WINDOW:TITLE = cTitlePrefixDetails + " "
/*         (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Detaljer: " ELSE "Details: ") */
        + cWindowTitle.

DYNAMIC-FUNCTION("setWordButton" IN ihFlatView,bWordButton).
DYNAMIC-FUNCTION("setExcludeFields" IN ihFlatView,cDistinctCols).

DYNAMIC-FUNCTION("setRowsToBatch" IN ihFlatView,iRowsToBatch).

DYNAMIC-FUNCTION("setUseStdAppserver" IN ihFlatView,YES).

/* No load of userfilter when doing drill-down.. */
DYNAMIC-FUNCTION("setAttribute",hNewBrwPlaceHolder,"usersettingcontext","-").

RUN InitializeObject IN ihFlatView (DYNAMIC-FUNCTION("getAttribute",hBrowse,"viewbuffersandfields"), 
                                    ClearUseIndex(
                                      DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") + cBuffDistinct + " " +
                                      DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") +
                                      DYNAMIC-FUNCTION("getAttribute",hBrowse,"querywhere") +
                                      DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryjoin")
                                     ),"", 
                                   TRUE).

hNewBrowse       = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).

hNewBrowse:SEPARATORS = hBrowse:SEPARATORS.

hNewBrowse:HELP = ihFlatView:CURRENT-WINDOW:TITLE.

DYNAMIC-FUNCTION("RemoveUseIndex",hNewBrowse).
hNewFilterButton = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",DYNAMIC-FUNCTION("getLinkedObject",hNewBrowse,"toolbar","from"),"buttonFilter")).
hNewToolbar      = DYNAMIC-FUNCTION("getLinkedObject",hNewBrowse,"toolbar","from").

DYNAMIC-FUNCTION("setAttribute",hNewBrowse,"filterexcludefields",TRIM(cFilterExclude,",")).

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryfilter") NE "" OR 
   DYNAMIC-FUNCTION("getAttribute",hBrowse,"prescanqueryfilter") NE "" OR 
   DYNAMIC-FUNCTION("getAttribute",hBrowse,"calcfieldfilter") NE "" THEN DO:
  cActiveFilterButton = DYNAMIC-FUNCTION("getAttribute",SESSION,"ActiveFilterButton").
  IF VALID-HANDLE(hNewFilterButton) AND cActiveFilterButton NE "" THEN
    hNewFilterButton:LOAD-IMAGE(cActiveFilterButton) NO-ERROR.
END.

DYNAMIC-FUNCTION("setAttribute",hNewToolbar,"disabledevents","browseconfig,filter").
DYNAMIC-FUNCTION("setAttribute",hNewToolbar,"permanentdisabledevents","browseconfig,filter").

IF cCurrFlatDistinct NE "" THEN DO:
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hNewBrowse,"jbCountDistinct").
  IF VALID-HANDLE(hColumn) THEN hColumn:VISIBLE = YES.

  IF cCurrFlatAccum NE "" AND NUM-ENTRIES(cCurrFlatAccum) = 1 THEN DO:
    hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hNewBrowse,"jbAverage").
    IF VALID-HANDLE(hColumn) THEN hColumn:VISIBLE = YES.
  END.

  DYNAMIC-FUNCTION("setBrowseColumns",hNewBrowse,
                   cCurrFlatDistinct + (IF cCurrFlatAccum NE "" AND NUM-ENTRIES(cCurrFlatAccum) = 1 THEN 
                                          "," + cCurrFlatAccum + ",jbCountDistinct,jbAverage" 
                                        ELSE IF cCurrFlatAccum NE "" THEN
                                          "," + cCurrFlatAccum + ",jbCountDistinct" 
                                        ELSE ",jbCountDistinct"),NO).    
  RUN DynAccumDone IN ihFlatView (hNewBrowse,"accum").
  DYNAMIC-FUNCTION("setAttribute",hNewBrowse,"uselocaldata","yes").
END.

APPLY "value-changed" TO hNewBrowse.

DYNAMIC-FUNCTION("DeleteObject",DYNAMIC-FUNCTION("getLinkedObject",hNewBrowse,"browse-search-field","from")).

IF DYNAMIC-FUNCTION("getAttribute",hNewBrowse,"currviewfields") = "" THEN 
  DYNAMIC-FUNCTION("setBrowseColumns",hNewBrowse,DYNAMIC-FUNCTION("getAttribute",hBrowse,"allviewfields"),NO).
ELSE DO:
  cNewCurrFields = DYNAMIC-FUNCTION("getAttribute",hNewBrowse,"currviewfields").
  DO ix = 1 TO NUM-ENTRIES(cCurrViewFields):
    IF CAN-DO(cNewCurrFields,ENTRY(ix,cCurrViewFields)) AND 
       NOT CAN-DO(cDistinctCols,ENTRY(ix,cCurrViewFields)) AND
       NOT ENTRY(ix,cCurrViewFields) BEGINS "jbph2_"
       THEN
      cModCurrFields = cModCurrFields + ENTRY(ix,cCurrViewFields) + ",".
  END.
  cModCurrFields = TRIM(cModCurrFields,",").
  IF cModCurrFields NE "" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hNewBrowse,"currviewfields",cModCurrFields).
    DYNAMIC-FUNCTION("setBrowseColumns",hNewBrowse,cModCurrFields,NO).
  END.
END.
DYNAMIC-FUNCTION("setAttribute",hNewBrowse,"allviewfields",DYNAMIC-FUNCTION("getAttribute",hBrowse,"allviewfields")).

DYNAMIC-FUNCTION("swapObjectHandle",hNewBrwPlaceHolder,hNewBrowse).

PUBLISH "SizeToFitTab" (hNewBrowse:WINDOW).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraStartSearch C-Win 
PROCEDURE ExtraStartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihBrowse      AS HANDLE NO-UNDO.
DEF INPUT  PARAM icColumnName  AS CHAR   NO-UNDO.
DEF INPUT  PARAM ibDesc        AS LOG    NO-UNDO.
DEF OUTPUT PARAM obOk          AS LOG    NO-UNDO INIT YES.

PUBLISH "DataBrwExtraStartSearch" (ihBrowse,icColumnName,ibDesc,OUTPUT obOk).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilterRecord C-Win 
PROCEDURE FilterRecord :
/*------------------------------------------------------------------------------
  Purpose:    To enable the filter to assign the correct value for source-buffer for the fields 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bLocal AS LOG NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"uselocaldata") = "yes" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","").
  bLocal = YES.
END.

RUN SUPER.

IF bLocal THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").


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
DEF INPUT PARAM icBuffersAndFields AS CHAR NO-UNDO.
DEF INPUT PARAM icWhereString      AS CHAR NO-UNDO.
DEF INPUT PARAM icInitSort         AS CHAR NO-UNDO.
DEF INPUT PARAM ibRecordCount      AS LOG  NO-UNDO.

RUN enable_UI.

/* IF THIS-PROCEDURE:CURRENT-WINDOW:ICON = "" THEN                        */
/*   THIS-PROCEDURE:CURRENT-WINDOW:LOAD-ICON("ico\admin%.ico") NO-ERROR.  */

DEF VAR cNoColumnSearch   AS CHAR   NO-UNDO.
DEF VAR hBrwMenu          AS HANDLE NO-UNDO.
DEF VAR cInitFilter       AS CHAR   NO-UNDO.
DEF VAR cNavMenuLabel     AS CHAR   NO-UNDO.
DEF VAR cEditMenuLabel    AS CHAR   NO-UNDO.
DEF VAR hWinToolbar       AS HANDLE NO-UNDO.
DEF VAR cDefaultFrameFont AS CHAR   NO-UNDO.
DEF VAR cDefaultBrwFont   AS CHAR   NO-UNDO.
DEF VAR hAppServer        AS HANDLE NO-UNDO.
DEF VAR cQueryAppS        AS CHAR   NO-UNDO.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont").
  IF cDefaultFrameFont NE "" THEN
    FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.

  cDefaultBrwFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultDataBrowseFont").
  IF cDefaultBrwFont = "" THEN
    cDefaultBrwFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultBrowseFont").

  IF ERROR-STATUS:ERROR THEN cDefaultBrwFont = "".

  ASSIGN fiCurrentQuery:LABEL   = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Spørring" ELSE "Query")
         fiCurrentQuery:HIDDEN  = YES
         fiCurrentQuery:SIDE-LABEL-HANDLE:Y = fiCurrentQuery:SIDE-LABEL-HANDLE:Y - 2
         cInitSort              = icInitSort
         cOrgWinTitle           = IF cOrgWinTitle = "" THEN THIS-PROCEDURE:CURRENT-WINDOW:TITLE ELSE cOrgWinTitle
         icInitSort             = IF NUM-ENTRIES(icInitSort,".") > 1 THEN TRIM(ENTRY(2,icInitSort,".")) ELSE TRIM(icInitSort)
         cTitlePrefixTotals     = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Totalt pr:" ELSE "Totals pr:"
         cTitlePrefixDetails    = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Detaljer:"  ELSE "Details:"
         .
  IF cOrgWinTitle BEGINS "Details: " THEN
    cQueryTitle = SUBSTR(cOrgWinTitle,10).
  ELSE IF cOrgWinTitle BEGINS "Detaljer: " THEN
    cQueryTitle = SUBSTR(cOrgWinTitle,11).

  IF NUM-ENTRIES(ENTRY(1,icBuffersAndFields),";") > 1 THEN
    icBuffersAndFields = ReduceFields(icBuffersAndFields).

  IF cInitFilterFields NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cInitFilterFields):
      cInitFilter = cInitFilter + " AND "
                    + ENTRY(ix,cInitFilterFields) + " "
                    + ENTRY(ix,cInitFilterOperators) + " " 
                    + ENTRY(ix,cInitFilterValues,"|") 
                    .
    END.
  END.

  IF VALID-HANDLE(hParent) AND DYNAMIC-FUNCTION("getAttribute",BrwFlatView:HANDLE,"usersettingcontext") = "" THEN
    DYNAMIC-FUNCTION("setAttribute",BrwFlatView:HANDLE,"usersettingcontext",hParent:FILE-NAME).
  ELSE IF NOT VALID-HANDLE(hParent) AND DYNAMIC-FUNCTION("getAttribute",BrwFlatView:HANDLE,"usersettingcontext") = "" THEN
    DYNAMIC-FUNCTION("setAttribute",BrwFlatView:HANDLE,"disablesavefilter","yes").

  IF iRowsToBatch = 0 THEN
    iRowsToBatch = IF cUseLocalData = "yes" THEN 60000 ELSE 200.

  IF NOT bUseStdAppServer OR CAN-DO(DYNAMIC-FUNCTION("getAttribute",SESSION,"use_query_appservice"),THIS-PROCEDURE:FILE-NAME) THEN DO:
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"query_appservice") = "" THEN DO:
      cQueryAppS = DYNAMIC-FUNCTION("getAttribute",SESSION,"query_appservice_connect").
      IF cQueryAppS NE "" THEN DO:
        CREATE SERVER hAppServer.
        hAppServer:CONNECT(cQueryAppS) NO-ERROR.
        IF hAppServer:CLIENT-CONNECTION-ID NE "" THEN 
          DYNAMIC-FUNCTION("setAttribute",SESSION,"query_appservice",STRING(hAppServer)).
      END.
    END.      
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"query_appservice") NE "" THEN
      DYNAMIC-FUNCTION("setAttribute",BrwFlatView:HANDLE,"useAppserver","query_appservice").
  END.

  IF icInitSort NE "" THEN
    DYNAMIC-FUNCTION("setAttribute",BrwFlatView:HANDLE,"InitialSortString",icInitSort).

  /* Create the browse: */
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           BrwFlatView:HANDLE, 
                           iRowsToBatch,
                           "MULTIPLE,!FIT-LAST-COLUMN" 
                         + (IF cDefaultBrwFont NE "" THEN "FONT|" + cDefaultBrwFont ELSE ""), 
                           icBuffersAndFields,  
                           icWhereString + " " + cInitFilter,
                           (IF ibRecordCount AND DYNAMIC-FUNCTION("getAttribute",BrwFlatView:HANDLE,"getrecordcount") NE "no" THEN ",getrecordcount" ELSE "") + 
                           cInitCalcFieldProcs).

  hBrowse:NAME = "JBoxDataBrw".
  
  /* If any field are excluded from the browse they should also be excluded from the filter: */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterexcludefields",cExcludeFields).

  IF icWhereString NE "where false" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery",DYNAMIC-FUNCTION("getAttribute",hBrowse,"querywhere")).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere","").
  END.

  IF ibRecordCount THEN DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
  IF DYNAMIC-FUNCTION("getAttribute",SESSION,"useDataBrwRowShade") = "YES" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","YES").

  IF cExcludedList NE "" THEN DYNAMIC-FUNCTION("setAttribute",hBrowse,"excludedfieldposlist",cExcludedList).

  IF bDisableSaveFilter THEN DYNAMIC-FUNCTION("setAttribute",hBrowse,"disablesavefilter","yes").

  /* If the extract should be treated as a temp-table after first retrieval from the database: */
  IF cUseLocalData = "yes" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata",cUseLocalData).
  ELSE DO:
    IF NOT bAccumFieldSort THEN
      DYNAMIC-FUNCTION("setNoColumnSort",hBrowse,DYNAMIC-FUNCTION("getAttribute",hBrowse,"allcalcfields")).
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"nocolumnsearch",
                     TRIM(DYNAMIC-FUNCTION("getAttribute",hBrowse,"alljoinviewfields") + "," + DYNAMIC-FUNCTION("getAttribute",hBrowse,"allcalcfields"),",")).
  END.

  IF cInitFilterFields NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cInitFilterFields):
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"FilterField_" + ENTRY(ix,cInitFilterFields),TRIM(ENTRY(ix,cInitFilterValues,"|"),"'")).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"FieldOperator_" + ENTRY(ix,cInitFilterFields),"AND").
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"operatorInUse_" + ENTRY(ix,cInitFilterFields),ENTRY(ix,cInitFilterOperators)).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"filtervalue_" + ENTRY(ix,cInitFilterFields),TRIM(ENTRY(ix,cInitFilterValues,"|"),"'")).
    END.
  END.

  IF icInitSort NE "" THEN 
    CreateSearchField(ENTRY(1,icInitSort," ")).
  ELSE searchField:HIDDEN = TRUE.

  hBrwMenu = DYNAMIC-FUNCTION("NewMenuBand",
                   hBrowse,                                      /* Parent widget */
                   "MultiSortBrowse;"
                   + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sorter på flere kolonner" ELSE "Sort on multiple columns")
                 + ",|" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Summér markerte rader" ELSE "Totals selected rows") 
                 + ",CopyToClipboard;"
                    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kopier markerte rader" ELSE "Copy seleced rows")
                 + ",DeselectRow;"
                    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fjern markering av rad" ELSE "Deselect rows")
                 + ",|" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Excel graf" ELSE "Excel graph")
                 + ",SetQueryWhere;"
                    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sett egne spørrekriterier" ELSE "Set query crit") 
                 + ",RowsToBatch;"
                    + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sett antall rader i resultatsett" ELSE "Set rows to batch") 
                   ,"").
  hTotalsMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"placeholder1")).
  hExcelGraph = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hBrowse,"placeholder2")).
  hExcelGraph:SENSITIVE = NO.
  setTotalFieldsMenu("details").

  ASSIGN cNavMenuLabel  = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Naviger" ELSE "Navigate"
         cEditMenuLabel = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rediger" ELSE "Edit".

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,      
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",      
                             (IF bEnableSaveQuery AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "" AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "JBoxDataBrw.w" THEN
                               "SaveNewQuery;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Lagre som ny spørring ctrl-n" ELSE "Save as new query ctrl-n") + ";;SaveNewQuery¤menu,"
                              ELSE "")
                           + (IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "" AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "JBoxDataBrw.w" THEN 
                               "BrowseConfig|" + cEditMenuLabel + ";" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kolonneoppsett" ELSE "Browse config") + "," 
                              ELSE "")
                           + "excel" + (IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"useExcelViewer") = "yes" THEN
                                          ";Dump to dataview;;;" + 
                                           (IF DYNAMIC-FUNCTION("getAttribute",SESSION,"btnImg_excelViewer") NE "" THEN
                                              DYNAMIC-FUNCTION("getAttribute",SESSION,"btnImg_excelViewer")
                                            ELSE "bmp\spread16.bmp")
                                        ELSE "")
                           + ",filter|" + cEditMenuLabel
                           + (IF bAccumButton THEN 
                               ",accum|" + cEditMenuLabel + ";" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Akkumuler" ELSE "Accumulate") 
                              ELSE "")
                           + (IF bWordButton THEN ",word" ELSE "")
                           + (IF bPrintButton THEN ",print" ELSE "")
                           + (IF bEmailButton THEN ",email" ELSE "")
                           + (IF bSMSButton THEN ",SMS" ELSE "")
                           + (IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "" AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "JBoxDataBrw.w" THEN 
                               ",Count|" + cEditMenuLabel + ";" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vis antall rader" ELSE "View record count") + ";toggle¤menu enable" 
                              ELSE "")
/*                            + ",rule"  */
                           + ",first|" + cNavMenuLabel + (IF DYNAMIC-FUNCTION("Scandinavian") THEN ";Første" ELSE "")
                              + ",prev|" + cNavMenuLabel + (IF DYNAMIC-FUNCTION("Scandinavian") THEN ";F&orrige" ELSE "")
                              + ",next|" + cNavMenuLabel + (IF DYNAMIC-FUNCTION("Scandinavian") THEN ";Neste" ELSE "")
                              + ",last|" + cNavMenuLabel + (IF DYNAMIC-FUNCTION("Scandinavian") THEN ";Siste" ELSE "")
                           + (IF bEnableSaveQuery AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "" AND DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext") NE "JBoxDataBrw.w" THEN
                               ",|" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Mine spørringer" ELSE "My queries") + "¤menu"
                              ELSE "")
/*                            + ",rule" */
                            ,"maxborder").
                            
  DYNAMIC-FUNCTION("setObjectSourceFileHandle",hBrowse,THIS-PROCEDURE).                            

  hMyQueriesMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"placeholder1")) NO-ERROR.
  IF VALID-HANDLE(hMyQueriesMenu) THEN SetMyQueriesMenu().
  ASSIGN hFilterBtn = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFilter"))
         cActiveFilterBtn  = DYNAMIC-FUNCTION("getAttribute",SESSION,"activefilterbutton") /*DYNAMIC-FUNCTION("getActiveFilterButtonFile")*/
         cPassiveFilterBtn = DYNAMIC-FUNCTION("getAttribute",SESSION,"passivefilterbutton")
         hAccumBtn         = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonAccum"))
         cActiveAccumBtn   = DYNAMIC-FUNCTION("getAttribute",SESSION,"activeaccumbutton")
         cPassiveAccumBtn  = DYNAMIC-FUNCTION("getAttribute",SESSION,"passiveaccumbutton")
         hConfigBtn        = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonBrowseConfig"))
         cActiveConfigBtn  = DYNAMIC-FUNCTION("getAttribute",SESSION,"activeconfigbutton")
         cPassiveConfigBtn = DYNAMIC-FUNCTION("getAttribute",SESSION,"passiveconfigbutton")
         .

  hMenuItemCountRows = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"menu-itemCount")) NO-ERROR.
  IF VALID-HANDLE(hMenuItemCountRows) THEN
    hMenuItemCountRows:CHECKED = ibRecordCount.
  
  /* If the filter values should be set in custom .w: */
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"filterwindow",cCustomFilter).

  IF cInitFilterFields NE "" THEN 
    hFilterBtn:LOAD-IMAGE(cActiveFilterBtn).

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File",
                             "Close;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Avsluttalt-a" ELSE "Exitalt-x")
                           + ",Delete|;&;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett spørring" ELSE "Delete current query&")
                                      + ";DeleteQuery"
                           + ",Save|;Savectrl-s;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Lagre spørring" ELSE "Save current query")
                                      + ";SaveCurrentQuery"
                            ,"right,enable").  
  ASSIGN hBtnSaveQuery = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hWinToolbar,"buttonSave"))
         hBtnSaveQuery:HIDDEN = YES
         hBtnDeleteQuery = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hWinToolbar,"buttonDelete"))
         hBtnDeleteQuery:HIDDEN = YES.

  /* Link the browse and the toolbar */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  SUBSCRIBE TO "ExcelSheetParams" ANYWHERE.
  SUBSCRIBE TO "DynAccumDone" ANYWHERE.
  SUBSCRIBE TO "BeforeSetDynFilter" ANYWHERE.

  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectWinToolbar").
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,350,150,0,150).

  APPLY "value-changed" TO hBrowse.
  APPLY "entry" TO hBrowse.

  SESSION:SET-WAIT-STATE("").

END.

IF NOT bManualView THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF icWhereString BEGINS "where false" THEN DO:
    THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS + 1.
    APPLY "window-resized" TO {&WINDOW-NAME}.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitQuery C-Win 
PROCEDURE InitQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFilterFields    AS CHAR NO-UNDO.
DEF INPUT PARAM icFilterOperators AS CHAR NO-UNDO.
DEF INPUT PARAM icFilterValues    AS CHAR NO-UNDO.
DEF INPUT PARAM icInitSort        AS CHAR NO-UNDO.
DEF INPUT PARAM icDistinctCols    AS CHAR NO-UNDO.
DEF INPUT PARAM icAccumFields     AS CHAR NO-UNDO.

DEF VAR hColumn AS HANDLE NO-UNDO.

RUN InitWithUserQuery(OUTPUT bOk).

IF bOk THEN RETURN.
ELSE 
  StartQuery(icFilterFields,
             icFilterOperators,
             icFilterValues,
             icInitSort,
             icDistinctCols,
             icAccumFields).

/*
DYNAMIC-FUNCTION("setAttribute",hBrowse,"distinctcolumns",icDistinctCols).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"accumfields",icAccumFields).

IF icInitSort NE "" AND icDistinctCols = "" THEN DO:
  DYNAMIC-FUNCTION("setSortString",hBrowse,icInitSort).
  CreateSearchField(ENTRY(1,icInitSort)).
END.
ELSE IF icDistinctCols NE "" THEN DO:
  DYNAMIC-FUNCTION("setSortString",hBrowse,icDistinctCols).
  CreateSearchField(ENTRY(1,icDistinctCols)).
END.

IF icFilterFields NE "" THEN
  DYNAMIC-FUNCTION("InitDynFilter",hBrowse
                   ,icFilterFields
                   ,icFilterOperators
                   ,icFilterValues
                   ,"").

ELSE 
  RUN InvokeMethod(hBrowse,"OpenQuery").

IF icDistinctCols NE "" THEN DO:    
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"jbCountDistinct").
  IF VALID-HANDLE(hColumn) THEN hColumn:VISIBLE = YES.

  IF icAccumFields NE "" AND NUM-ENTRIES(icAccumFields) = 1 THEN DO:
    hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"jbAverage").
    IF VALID-HANDLE(hColumn) THEN hColumn:VISIBLE = YES.
  END.

  DYNAMIC-FUNCTION("setBrowseColumns",hBrowse,
                   icDistinctCols    + (IF icAccumFields NE "" AND NUM-ENTRIES(icAccumFields) = 1 THEN 
                                          "," + icAccumFields + ",jbCountDistinct,jbAverage" 
                                        ELSE IF icAccumFields NE "" THEN
                                          "," + icAccumFields + ",jbCountDistinct"
                                        ELSE ",jbCountDistinct"),NO).    

  RUN DynAccumDone (hBrowse,"accum").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").
END.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStringTranslation C-Win 
PROCEDURE InitStringTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

/* For translation: Init special translation type and corresponding values: */
IF NOT CAN-DO(iocTypeList,"TitlePrefix-Details") THEN
  iocTypeList = iocTypeList + ",TitlePrefix-Details".
IF NOT CAN-DO(iocTypeList,"TitlePrefix-Totals") THEN
  iocTypeList = iocTypeList + ",TitlePrefix-Totals".

cTitlePrefixDetails = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,
                                       "TitlePrefix-Details",cTitlePrefixDetails).
cTitlePrefixTotals  = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,
                                       "TitlePrefix-Totals",cTitlePrefixTotals).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWithUserQuery C-Win 
PROCEDURE InitWithUserQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obOk AS LOG    NO-UNDO.

DEF VAR cMenuDef      AS CHAR   NO-UNDO.
DEF VAR cMenuIdList   AS CHAR   NO-UNDO.
DEF VAR cMyQueryList  AS CHAR   NO-UNDO.
DEF VAR cMenuNameList AS CHAR   NO-UNDO.
DEF VAR hMenuItem     AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("setUserSettingSort","cSetting").

cMenuDef = DYNAMIC-FUNCTION("getUserSetting",THIS-PROCEDURE:FILE-NAME,
                            "UserQuery",
                            DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext"),
                            "*").
IF cMenuDef NE "" THEN DO:
  IF NUM-ENTRIES(cMenuDef,"|") > 1 THEN DO:
    ASSIGN cMenuIdList   = ENTRY(1,cMenuDef,"|")
           cMenuNameList = REPLACE(ENTRY(2,cMenuDef,"|"),",",CHR(3))           
           .
    DO ix = 1 TO NUM-ENTRIES(cMenuIdList):
      cMyQueryList = cMyQueryList + (IF cMyQueryList NE "" THEN "|" ELSE "") + ENTRY(ix,cMenuNameList,CHR(1)) + "|" + ENTRY(ix,cMenuIdList).
    END.
  END.
  ELSE 
    ASSIGN cMenuIdList   = DYNAMIC-FUNCTION("getTransactionMessage")
           cMenuNameList = cMenuDef
           cMyQueryList  = cMenuNameList + "|" + cMenuIdList.

  cMenuDef = "".
  RUN JBoxDSimpleSelectList.w (cMyQueryList,?,OUTPUT cMenuDef).
  IF cMenuDef NE ? THEN DO:
    hMenuItem = DYNAMIC-FUNCTION("getEventWidget",
                                 hToolbar,
                                 cMenuDef,
                                 "menu-item").
    hMenuItem:CHECKED = YES.
    DYNAMIC-FUNCTION("setCurrentWidget",hMenuItem).
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"lasttoolbaraction",cMenuDef).
    RUN "StartUserQuery".
    obOk = YES.
  END.

END.

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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:TOP-ONLY = YES.
IF VALID-HANDLE(hBrowse) THEN
  APPLY "entry" TO hBrowse.
THIS-PROCEDURE:CURRENT-WINDOW:TOP-ONLY = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bOpenQuery AS LOG NO-UNDO INIT YES.

PUBLISH "OpenQueryFlatView" (THIS-PROCEDURE,hBrowse,OUTPUT bOpenQuery).

IF bOpenQuery THEN DO:
  RUN SUPER.

  IF VALID-HANDLE(hExcelGraph) THEN 
    hExcelGraph:SENSITIVE = INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")) < iMaxGraph AND NUM-ENTRIES(DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctcolumns")) = 1.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PdfPrint C-Win 
PROCEDURE PdfPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  FIND FIRST TTPDF.                                                    */
/*                                                                       */
/*  ASSIGN TTPDF.creporttitle = {&WINDOW-NAME}:TITLE                     */
/*         TTPDF.hbrowse = hBrowse                                       */
/*         TTPDF.loAltColours = NO                                       */
/* /*         TTPDF.deAltColour[1] = DEC(fillin_iRed:SCREEN-VALUE)   */  */
/* /*         TTPDF.deAltColour[2] = DEC(fillin_iGreen:SCREEN-VALUE) */  */
/* /*         TTPDF.deAltColour[3] = DEC(fillin_iBlue:SCREEN-VALUE)  */  */
/*         TTPDF.cFunction[4] = THIS-PROCEDURE:CURRENT-WINDOW:TITLE.     */
/*                                                                       */
/*  RUN pdflibrary.p (INPUT-OUTPUT TABLE TTPDF).                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printRecord C-Win 
PROCEDURE printRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "FlatViewPrintRecord" (ENTRY(1,ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields")),";"),
                               hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
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
PUBLISH "RowDisplayDataBrowse" (hBrowse).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowEntry C-Win 
PROCEDURE RowEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF VALID-HANDLE(hExcelGraph) THEN
  hExcelGraph:SENSITIVE = hBrowse:NUM-SELECTED-ROWS < iMaxGraph AND NUM-ENTRIES(DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctcolumns")) = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveCurrentQuery C-Win 
PROCEDURE SaveCurrentQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SaveQuery(cCurrQueryId,IF fiCurrentQuery:MODIFIED IN FRAME {&FRAME-NAME} THEN REPLACE(fiCurrentQuery:SCREEN-VALUE,","," ") ELSE "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveNewQuery C-Win 
PROCEDURE SaveNewQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cName   AS CHAR NO-UNDO.
DEF VAR iReturn AS INT  NO-UNDO.
DEF VAR iQryIx  AS INT  NO-UNDO.

RUN JBoxAskForValue.w (IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                         "Angi navn for ny spørring"
                       ELSE
                        "Enter name for new query instance"
                       ,"CHARACTER|x(100)",INPUT-OUTPUT cName,OUTPUT iReturn).
IF iReturn > 0 AND cName NE "" THEN DO:
  IF cMenuIdList NE "" THEN
    cCurrQueryId = "query" + STRING(NUM-ENTRIES(cMenuIdList) + 1).
  ELSE cCurrQueryId = "query1".

  SaveQuery(cCurrQueryId,REPLACE(cName,","," ")).
END.
ELSE IF iReturn > 0 AND cName = "" THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Du må angi et navn for spørringen"
                                   ELSE "You must enter a name for the query","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetQueryWhereRecord C-Win 
PROCEDURE SetQueryWhereRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cQuery AS CHAR NO-UNDO.

cQuery = DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryWhere").

RUN JBoxDSmallEditor.w (INPUT-OUTPUT cQuery,"Set query string",NO,OUTPUT bOk).

IF bOK THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryWhere",cQuery).
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SMSRecord C-Win 
PROCEDURE SMSRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "FlatViewSMSRecord" (ENTRY(1,ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields")),";"),
                               hBrowse).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartUserQuery C-Win 
PROCEDURE StartUserQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cQueryAtt         AS CHAR   NO-UNDO.
DEF VAR hCurrMenuId       AS HANDLE NO-UNDO.
DEF VAR cCurrFilterValues AS CHAR   NO-UNDO.
DEF VAR bActiveFilter     AS LOG    NO-UNDO.
DEF VAR cAccumViewFields  AS CHAR   NO-UNDO.
DEF VAR cCurrViewFields   AS CHAR   NO-UNDO.
DEF VAR hAccum            AS HANDLE NO-UNDO.
DEF VAR hFilter           AS HANDLE NO-UNDO.
DEF VAR hConfig           AS HANDLE NO-UNDO.
DEF VAR bViewAvg          AS LOG    NO-UNDO.
DEF VAR bRecordCount      AS LOG    NO-UNDO INIT ?.
DEF VAR bLocal            AS LOG    NO-UNDO.
          
bLocal = DYNAMIC-FUNCTION("getAttribute",hBrowse,"useLocalData") = "yes".
IF bLocal THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"useLocalData","").

hCurrMenuId = DYNAMIC-FUNCTION("getCurrentWidget").

IF hCurrMenuId:CHECKED THEN DO WITH FRAME {&FRAME-NAME}:
  cCurrQueryId = DYNAMIC-FUNCTION("getAttribute",hToolbar,"lasttoolbaraction").
  
  cQueryAtt = DYNAMIC-FUNCTION("getUserSetting",THIS-PROCEDURE:FILE-NAME,
                               cCurrQueryId,
                               DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext"),
                               "*").
  IF cQueryAtt NE "" THEN DO:
    ASSIGN fiCurrentQuery:SCREEN-VALUE = ENTRY(LOOKUP(cCurrQueryId,cMenuIdList),cMenuNameList,CHR(1))
           fiCurrentQuery:HIDDEN       = NO           
           fiCurrentQuery:MODIFIED     = NO
           hBtnSaveQuery:HIDDEN        = NO
           hBtnSaveQuery:SENSITIVE     = NO
           hBtnDeleteQuery:HIDDEN      = NO
           cCurrQuerySettings          = ENTRY(1,cQueryAtt,"|")
           cCurrFilterValues           = REPLACE(ENTRY(2,cQueryAtt,"|"),CHR(3),"|").

    hAccum = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"accumhandle")) NO-ERROR.
    IF VALID-HANDLE(hAccum) THEN 
      APPLY "close" TO hAccum.
    hFilter = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"filterhandle")) NO-ERROR.
    IF VALID-HANDLE(hFilter) THEN 
      APPLY "close" TO hFilter.
    hConfig = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"browseconfighandle")) NO-ERROR.
    IF VALID-HANDLE(hConfig) THEN 
      APPLY "close" TO hConfig.

    DO ix = 1 TO NUM-ENTRIES(cQuerySettingList):
      DYNAMIC-FUNCTION("msetAttribute",hBrowse,ENTRY(ix,cQuerySettingList),"").
    END.
    DO ix = 1 TO NUM-ENTRIES(cCurrQuerySettings):
      DYNAMIC-FUNCTION("setAttribute",hBrowse,ENTRY(ix,cCurrQuerySettings),ENTRY(ix,cCurrFilterValues,CHR(1))).
      IF ENTRY(ix,cCurrQuerySettings) BEGINS "filterValue_" AND ENTRY(ix,cCurrFilterValues,CHR(1)) NE "" THEN 
        bActiveFilter = YES.
      IF ENTRY(ix,cCurrQuerySettings) = "accumviewfields" THEN 
        ASSIGN cAccumViewFields = ENTRY(ix,cCurrFilterValues,CHR(1))
               bViewAvg         = CAN-DO(cAccumViewFields,"jbAverage").
      IF ENTRY(ix,cCurrQuerySettings) = "currviewfields" THEN 
        cCurrViewFields = ENTRY(ix,cCurrFilterValues,CHR(1)).
      IF ENTRY(ix,cCurrQuerySettings) = "getRecordCount" THEN 
        bRecordCount = ENTRY(ix,cCurrFilterValues,CHR(1)) = "yes".
    END.

    IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"localsort") NE "" AND
       DYNAMIC-FUNCTION("GetLinkedObject",hBrowse,"browse-search-field","from") = ? THEN
      CreateSearchField(ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"localsort"))).

    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.

    IF cAccumViewFields NE "" THEN DO:
      DYNAMIC-FUNCTION("ViewCountDistinctColumn",hBrowse,YES).
      IF bViewAvg THEN
        DYNAMIC-FUNCTION("ViewAverageColumn",hBrowse,YES).
      DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
      DYNAMIC-FUNCTION("setBrowseColumns",hBrowse,cAccumViewFields,YES).
      RUN DynAccumDone (hBrowse,"accum").
      DYNAMIC-FUNCTION("DoLockWindow",?).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"currviewfields",cOrgViewFields).
      hAccumBtn:LOAD-IMAGE(cActiveAccumBtn).
      IF VALID-HANDLE(hConfigBtn) THEN
        hConfigBtn:LOAD-IMAGE(cPassiveConfigBtn).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION("ViewCountDistinctColumn",hBrowse,NO).
      DYNAMIC-FUNCTION("ViewAverageColumn",hBrowse,NO).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"currviewfields",cCurrViewFields).
      DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
      DYNAMIC-FUNCTION("setBrowseColumns",hBrowse,cCurrViewFields,NO).
      RUN DynAccumDone (hBrowse,"clear").
      DYNAMIC-FUNCTION("DoLockWindow",?).
      IF VALID-HANDLE(hAccumBtn) THEN
        hAccumBtn:LOAD-IMAGE(cPassiveAccumBtn).
      IF VALID-HANDLE(hConfigBtn) THEN DO:  
        IF cCurrViewFields NE cOrgViewFields THEN
          hConfigBtn:LOAD-IMAGE(cActiveConfigBtn).
        ELSE
          hConfigBtn:LOAD-IMAGE(cPassiveConfigBtn).
      END.
    END.

    IF bActiveFilter THEN
      hFilterBtn:LOAD-IMAGE(cActiveFilterBtn).
    ELSE
      hFilterBtn:LOAD-IMAGE(cPassiveFilterBtn).
    
  END.
END.
ELSE ClearAttributes().
    
IF bLocal THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"useLocalData","yes").

DYNAMIC-FUNCTION("setToolbarToggles",hToolbar,NO,cCurrQueryId).

IF bRecordCount NE ? AND VALID-HANDLE(hMenuItemCountRows) THEN
  hMenuItemCountRows:CHECKED = bRecordCount.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SumSelected C-Win 
PROCEDURE SumSelected :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hMenuItem AS HANDLE NO-UNDO.
DEF VAR fTotal    AS DEC NO-UNDO FORMAT "->>>,>>>,>>9.99".
DEF VAR iCount    AS INT NO-UNDO.

hMenuItem = DYNAMIC-FUNCTION("getCurrentWidget").


DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
    ASSIGN fTotal = fTotal + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(LOOKUP(hMenuItem:LABEL,cTotalLabels),
                                                                                   (IF cAccumTotalFields NE "" THEN cAccumTotalFields ELSE cTotalFields))):BUFFER-VALUE
           iCount = iCount + 1.
END.
IF DYNAMIC-FUNCTION("DoMessage",0,4,(IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sum " ELSE "Total ") + hMenuItem:LABEL + ": " + STRING(fTotal,"->>>,>>>,>>9.99") + CHR(10)
                                  + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Ant " ELSE "Count ") + hMenuItem:LABEL + ": " + STRING(iCount,"->>>,>>>,>>9")
                    ,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kopier sum til utklippstavle?" ELSE "Copy total to clipboard?","") = 6 THEN
  CLIPBOARD:VALUE = STRING(fTotal).

RUN MoveToTop.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wordRecord C-Win 
PROCEDURE wordRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "FlatViewWordRecord" (ENTRY(1,ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields")),";"),
                               hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.
DEF VAR hField  AS HANDLE NO-UNDO.

hColumn = ihBrowse:FIRST-COLUMN.
REPEAT WHILE VALID-HANDLE(hColumn).
  hColumn:WIDTH-PIXELS = MIN(hColumn:WIDTH-PIXELS,120).
  hField = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hColumn:NAME) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND hField:DATA-TYPE = "DECIMAL" THEN
    bDecimalFields = YES.
  IF hColumn:VISIBLE THEN
    cOrgViewFields = cOrgViewFields + (IF cOrgViewFields NE "" THEN "," ELSE "") + hColumn:NAME.
  hColumn = hColumn:NEXT-COLUMN.
END.

RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearAttributes C-Win 
FUNCTION ClearAttributes RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hFilter       AS HANDLE NO-UNDO.
DEF VAR hAccum        AS HANDLE NO-UNDO.
DEF VAR hConfig       AS HANDLE NO-UNDO.
DEF VAR bRecordCount  AS LOG    NO-UNDO INIT ?.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cCurrQueryId           = ""
         fiCurrentQuery:HIDDEN  = YES
         hBtnSaveQuery:HIDDEN   = YES
         hBtnDeleteQuery:HIDDEN = YES
         cCurrQuerySettings    = ""
         .

  hAccum = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"accumhandle")) NO-ERROR.
  IF VALID-HANDLE(hAccum) THEN 
    APPLY "close" TO hAccum.
  hConfig = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"browseconfighandle")) NO-ERROR.
  IF VALID-HANDLE(hConfig) THEN 
    APPLY "close" TO hConfig.


  hFilter = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"filterhandle")) NO-ERROR.
  IF VALID-HANDLE(hFilter) THEN
    APPLY "close" TO hFilter.
  DO ix = 1 TO NUM-ENTRIES(cQuerySettingList):
    IF NOT CAN-DO("prescanquery,allaccumfields,allcalcfields,prescanmainquerycandidates_filter",ENTRY(ix,cQuerySettingList)) THEN
      DYNAMIC-FUNCTION("msetAttribute",hBrowse,ENTRY(ix,cQuerySettingList),"").

    IF ENTRY(ix,cQuerySettingList) = "getRecordCount" THEN 
      bRecordCount = NO.
  END.
  DYNAMIC-FUNCTION("msetAttribute",hBrowse,"prescanqueryfilter","").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"currviewfields",cOrgViewFields).  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querysort",cInitSort).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"localsort",cInitSort).
  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  DYNAMIC-FUNCTION("ViewCountDistinctColumn",hBrowse,NO).
  DYNAMIC-FUNCTION("ViewAverageColumn",hBrowse,NO).
  DYNAMIC-FUNCTION("setBrowseColumns",hBrowse,cOrgViewFields,NO).
  RUN DynAccumDone (hBrowse,"clear").

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.

  hFilterBtn:LOAD-IMAGE(cPassiveFilterBtn) NO-ERROR.
  IF VALID-HANDLE(hAccumBtn) THEN
    hAccumBtn:LOAD-IMAGE(cPassiveAccumBtn).
  IF VALID-HANDLE(hConfigBtn) THEN
    hConfigBtn:LOAD-IMAGE(cPassiveConfigBtn).

  IF bRecordCount = NO AND VALID-HANDLE(hMenuItemCountRows) THEN hMenuItemCountRows:CHECKED = NO.

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearUseIndex C-Win 
FUNCTION ClearUseIndex RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRevQuery AS CHAR NO-UNDO.
DEF VAR iy        AS INT  NO-UNDO.
DEF VAR bRemoved  AS LOG  NO-UNDO.

IF icQueryString MATCHES "*USE-INDEX*" THEN DO:
  DO iy = 2 TO NUM-ENTRIES(icQueryString," "):
    IF ENTRY(iy,icQueryString," ") = "USE-INDEX" OR ENTRY(iy - 1,icQueryString," ") = "USE-INDEX" THEN NEXT.
    cRevQuery = cRevQuery + ENTRY(iy,icQueryString," ") + " ".
  END.
  RETURN cRevQuery.
END.
RETURN icQueryString.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateSearchField C-Win 
FUNCTION CreateSearchField RETURNS LOGICAL
  ( INPUT icInitSort AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.

IF DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browse-search-field","from") NE ? THEN RETURN NO.

IF icInitSort NE "" THEN DO WITH FRAME {&FRAME-NAME}:
  bOK = FALSE.
  DO ix = 1 TO hBrowse:NUM-COLUMNS:
    IF TRIM(ENTRY(1,icInitSort)) = hBrowse:GET-BROWSE-COLUMN(ix):NAME THEN DO:
      bOk = TRUE.
      LEAVE.
    END.
  END.
  IF bOK AND ix > 0 THEN DO:
    hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,ix).
    IF VALID-HANDLE(hSearchField) THEN DO:
      DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).
      IF NUM-ENTRIES(icInitSort) > 1 THEN
        hSearchField:HIDDEN = YES.
    END.
  END.
  ELSE searchField:HIDDEN = TRUE.
END.
ELSE searchField:HIDDEN = TRUE.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseHandle C-Win 
FUNCTION getBrowseHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hBrowse) THEN
  RETURN hBrowse.
ELSE 
  RETURN BrwFlatView:HANDLE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDynDataBrowseObject C-Win 
FUNCTION getDynDataBrowseObject RETURNS JBoxDynDataBrowse
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RETURN oDynDataBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolbarHandle C-Win 
FUNCTION getToolbarHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hToolbar.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReduceFields C-Win 
FUNCTION ReduceFields RETURNS CHARACTER
  ( INPUT icBuffersAndFields AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewList   AS CHAR NO-UNDO.
DEF VAR iy         AS INT  NO-UNDO.
DEF VAR iFieldIdx  AS INT  NO-UNDO.
DEF VAR bReduced   AS LOG  NO-UNDO.

cExcludedList = "".

DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields):
  bReduced = NO.
  IF NUM-ENTRIES(ENTRY(ix,icBuffersAndFields),";") > 1 THEN
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icBuffersAndFields),";"):
      IF iy = 2 THEN
        cNewList = cNewList + ENTRY(iy - 1,ENTRY(ix,icBuffersAndFields),";") + ";".
  
      iFieldIdx = iFieldIdx + 1.
      IF NOT CAN-DO(cExcludeFields,TRIM(ENTRY(1,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|"),"!"))
         THEN DO:
        cNewList = cNewList + ENTRY(iy,ENTRY(ix,icBuffersAndFields),";") + ";".
      END.
      ELSE ASSIGN cExcludedList = cExcludedList + (IF cExcludedList NE "" THEN "," ELSE "") + STRING(iFieldIdx)
                  bReduced      = YES.
    END.
  ELSE cNewList = cNewList + ENTRY(ix,icBuffersAndFields).

/*   IF NUM-ENTRIES(ENTRY(ix,cNewList),";") = 2 AND ENTRY(2,ENTRY(ix,cNewList),";") = "" AND bReduced THEN  */
/*     cNewList = cNewList + ",".                                                                           */
/*   ELSE                                                                                                   */
    cNewList = RIGHT-TRIM(cNewList,";") + ",".
END.

RETURN TRIM(cNewList,","). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveQuery C-Win 
FUNCTION SaveQuery RETURNS LOGICAL
  ( INPUT icCurrQueryId AS CHAR,
    INPUT icName        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNameList  AS CHAR   NO-UNDO.
DEF VAR cValueList AS CHAR   NO-UNDO.
DEF VAR cAttrList  AS CHAR   NO-UNDO.

IF icName NE "" THEN DO:
  DYNAMIC-FUNCTION("setUserSetting",THIS-PROCEDURE:FILE-NAME,
                   "userquery",
                   DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext"),
                   icCurrQueryId,
                   icName).
  SetMyQueriesMenu().
  
  DYNAMIC-FUNCTION("setUserSetting",THIS-PROCEDURE:FILE-NAME,
                   icCurrQueryId,
                   DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext"),
                   cCurrQuerySettings,
                   "delete_setting").

  IF icName = "delete_setting" THEN DO:
    ClearAttributes().
    DYNAMIC-FUNCTION("setToolbarToggles",hToolbar,NO,"").
    RETURN YES.
  END.
END.

DO ix = 1 TO NUM-ENTRIES(cQuerySettingList):
  cAttrList  = DYNAMIC-FUNCTION("getAttributeList",hBrowse
                                ,ENTRY(ix,cQuerySettingList)
                                ,""
                                ,YES).
  IF ENTRY(1,cAttrList,"|") NE "" THEN
    ASSIGN cNameList  = cNameList  + ENTRY(1,cAttrList,"|") + ","
           cValueList = cValueList + ENTRY(2,cAttrList,"|") + CHR(1).
END.
ASSIGN cValueList = SUBSTR(cValueList,1,LENGTH(cValueList) - 1)
       cNameList  = SUBSTR(cNameList,1,LENGTH(cNameList) - 1).

DYNAMIC-FUNCTION("setUserSetting",THIS-PROCEDURE:FILE-NAME,
                 icCurrQueryId,
                 DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext"),
                 cNameList,
                 cValueList).
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SeqToAlpha C-Win 
FUNCTION SeqToAlpha RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT,
    INPUT iCount  AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRange AS CHAR NO-UNDO INIT "Z".

IF iiSeqNo < 0 OR iiSeqNo > 80 THEN cRange = "Z".
ELSE IF iiSeqNo < 27 THEN
  cRange = CHR(64 + iiSeqNo).
ELSE IF iiSeqNo < 54 THEN
  cRange = "A" + CHR(64 - 26 + iiSeqNo).
ELSE IF iiSeqNo < 81 THEN
  cRange = "B" + CHR(64 - 26 + iiSeqNo).

cRange = cRange + STRING(iCount).

RETURN cRange.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAccumButton C-Win 
FUNCTION setAccumButton RETURNS LOGICAL
  ( INPUT ibAccumButton AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

bAccumButton = ibAccumButton.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAccumFieldSort C-Win 
FUNCTION setAccumFieldSort RETURNS LOGICAL
  ( INPUT ibAccumFieldSort AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Switch on/off sorting on accumulated fields 
    Notes: Can be called before/after the browse is created. Default is YES
------------------------------------------------------------------------------*/
bAccumFieldSort = ibAccumFieldSort.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCalcFieldProcs C-Win 
FUNCTION setCalcFieldProcs RETURNS LOGICAL
  ( INPUT icCalcFieldProcs AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cInitCalcFieldProcs = "," + REPLACE(TRIM(icCalcFieldProcs,","),",",";").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCustomFilterWin C-Win 
FUNCTION setCustomFilterWin RETURNS LOGICAL
  ( INPUT icCustomFilter AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Must be set prior to InitializeObject. Use ExtraFlatViewRecord 
------------------------------------------------------------------------------*/
cCustomFilter = icCustomFilter.
RETURN FALSE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDisableSaveFilter C-Win 
FUNCTION setDisableSaveFilter RETURNS LOGICAL
  ( INPUT ibDisableSaveFilter AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bDisableSaveFilter = ibDisableSaveFilter.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEmailButton C-Win 
FUNCTION setEmailButton RETURNS LOGICAL
  ( INPUT ibEmailButton AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bEmailButton = ibEmailButton.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEnableSaveQuery C-Win 
FUNCTION setEnableSaveQuery RETURNS LOGICAL
  ( INPUT ibEnableSaveQuery AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bEnableSaveQuery = ibEnableSaveQuery.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setExcludeFields C-Win 
FUNCTION setExcludeFields RETURNS LOGICAL
  ( icExcludeFields AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cExcludeFields = icExcludeFields.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setInitFilter C-Win 
FUNCTION setInitFilter RETURNS LOGICAL
  ( INPUT icInitFilterFields    AS CHAR,
    INPUT icInitFilterOperators AS CHAR,
    INPUT icInitFilterValues    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN cInitFilterFields    = icInitFilterFields
       cInitFilterOperators = icInitFilterOperators
       cInitFilterValues    = icInitFilterValues
       .

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setManualView C-Win 
FUNCTION setManualView RETURNS LOGICAL
  ( INPUT ibManualView AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bManualView = ibManualView.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMyQueriesMenu C-Win 
FUNCTION setMyQueriesMenu RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hMenuItem AS HANDLE NO-UNDO.
DEF VAR hNextItem AS HANDLE NO-UNDO.

DEF VAR cMenuDef      AS CHAR NO-UNDO.

IF NOT VALID-HANDLE(hMyQueriesMenu) THEN RETURN NO.

hMenuItem = hMyQueriesMenu:FIRST-CHILD NO-ERROR.
REPEAT WHILE VALID-HANDLE(hMenuItem):
  hNextItem = hMenuItem:NEXT-SIBLING.
  DELETE OBJECT hMenuItem.
  hMenuItem = hNextItem.
END.
DYNAMIC-FUNCTION("setUserSettingSort","cSetting").
cMenuDef = DYNAMIC-FUNCTION("getUserSetting",THIS-PROCEDURE:FILE-NAME,
                            "UserQuery",
                            DYNAMIC-FUNCTION("getAttribute",hBrowse,"usersettingcontext"),
                            "*").
IF cMenuDef NE "" THEN DO:
  IF NUM-ENTRIES(cMenuDef,"|") > 1 THEN DO:
    ASSIGN cMenuIdList   = ENTRY(1,cMenuDef,"|")
           cMenuNameList = REPLACE(ENTRY(2,cMenuDef,"|"),",",CHR(3))           
           cMenuDef      = "".
    DO ix = 1 TO NUM-ENTRIES(cMenuIdList):
      ASSIGN cMenuDef = cMenuDef + ENTRY(ix,cMenuIdList) + ";" + ENTRY(ix,cMenuNameList,CHR(1)) + "&;StartUserQuery;toggle¤enable,"
             cMyQueryList = cMyQueryList + ENTRY(ix,cMenuIdList) + "|" + ENTRY(ix,cMenuNameList,CHR(1)) + ";".
    END.
    cMyQueryList = TRIM(cMyQueryList,";").
  END.
  ELSE 
    ASSIGN cMenuIdList   = DYNAMIC-FUNCTION("getTransactionMessage")
           cMenuNameList = cMenuDef
           cMenuDef      = cMenuIdList + ";" + cMenuDef + "&;StartUserQuery;toggle¤enable,"
           cMyQueryList  = cMenuIdList + "|" + cMenuDef.

  DYNAMIC-FUNCTION("NewMenuBand",hMyQueriesMenu,
                   TRIM(cMenuDef,","),"").

  DO ix = 1 TO NUM-ENTRIES(cMenuIdList):
    hMenuItem = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"menu-itemquery" + STRING(ix))) NO-ERROR.
    IF VALID-HANDLE(hMenuItem) THEN
      hMenuItem:LABEL = REPLACE(hMenuItem:LABEL,CHR(3),",").
  END.
  cMenuNameList = REPLACE(cMenuNameList,CHR(3),",").

  hMyQueriesMenu:SENSITIVE = YES.

  IF LOOKUP(cCurrQueryId,cMenuIdList) > 0 THEN 
    ASSIGN fiCurrentQuery:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(LOOKUP(cCurrQueryId,cMenuIdList),cMenuNameList,CHR(1))
           fiCurrentQuery:HIDDEN   = NO
           fiCurrentQuery:MODIFIED = NO
           hBtnSaveQuery:HIDDEN    = NO
           hBtnSaveQuery:SENSITIVE = NO
           hBtnDeleteQuery:HIDDEN  = NO
           hMenuItem               = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"menu-item" + cCurrQueryId))
           hMenuItem:CHECKED       = YES
           NO-ERROR.
END.
ELSE 
  ASSIGN hMyQueriesMenu:SENSITIVE                     = NO
         fiCurrentQuery:HIDDEN IN FRAME {&FRAME-NAME} = YES
         hBtnSaveQuery:HIDDEN                         = YES
         hBtnDeleteQuery:HIDDEN                       = YES
         NO-ERROR.

PUBLISH "RebuildActionMenu".

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoDrillDown C-Win 
FUNCTION setNoDrillDown RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

bNoDrillDown = YES.

RETURN YES.

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
  IF NOT VALID-HANDLE(ihParent) THEN RETURN FALSE.
  
  hParent = ihParent.

  IF NOT VALID-OBJECT(oDynDataBrowse) THEN DO WITH FRAME {&FRAME-NAME}:
    oDynDataBrowse = NEW JBoxDynDataBrowse(THIS-PROCEDURE,hParent,BrwFlatView:HANDLE,rectToolbar:HANDLE,searchField:HANDLE).
  END.      
    
  IF CAN-DO(hParent:INTERNAL-ENTRIES,"myDatabrowseObject") THEN
    RUN myDatabrowseObject IN hParent (oDynDataBrowse). 

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPrintButton C-Win 
FUNCTION setPrintButton RETURNS LOGICAL
  ( INPUT ibPrintButton AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bPrintButton = ibPrintButton.

RETURN FALSE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRowsToBatch C-Win 
FUNCTION setRowsToBatch RETURNS LOGICAL
  ( INPUT iiRowsToBatch AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

iRowsToBatch = iiRowsToBatch.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSMSButton C-Win 
FUNCTION setSMSButton RETURNS LOGICAL
  ( INPUT ibSMSButton AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bSMSButton = ibSMSButton.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTotalFields C-Win 
FUNCTION setTotalFields RETURNS LOGICAL
  ( INPUT icTotalFields AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Must be set prior to InitializeObject. Use ExtraFlatViewRecord
------------------------------------------------------------------------------*/
cTotalFields = icTotalFields.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTotalFieldsMenu C-Win 
FUNCTION setTotalFieldsMenu RETURNS LOGICAL
  ( INPUT icViewType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hMenuItem  AS HANDLE NO-UNDO.
DEF VAR hNextItem  AS HANDLE NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.
DEF VAR cLabel     AS CHAR   NO-UNDO.
DEF VAR hColumn    AS HANDLE NO-UNDO.

hMenuItem = hTotalsMenu:FIRST-CHILD NO-ERROR.
REPEAT WHILE VALID-HANDLE(hMenuItem):
  hNextItem = hMenuItem:NEXT-SIBLING.
  DELETE OBJECT hMenuItem.
  hMenuItem = hNextItem.
END.

hMenuItem = hExcelGraph:FIRST-CHILD NO-ERROR.
REPEAT WHILE VALID-HANDLE(hMenuItem):
  hNextItem = hMenuItem:NEXT-SIBLING.
  DELETE OBJECT hMenuItem.
  hMenuItem = hNextItem.
END.

ASSIGN cTotalLabels      = ""
       cTotalMenu        = ""
       cAccumTotalFields = "".

IF icViewType = "details" THEN DO:

  IF cTotalFields NE "none" AND cTotalFields NE "" THEN
    DO ix = 1 TO hBrowse:NUM-COLUMNS:
      cLabel = DYNAMIC-FUNCTION("getStrippedSortLabel",hBrowse:GET-BROWSE-COLUMN(ix)).
      IF hBrowse:GET-BROWSE-COLUMN(ix):VISIBLE AND CAN-DO(cTotalFields,hBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN
        ASSIGN cTotalLabels = cTotalLabels + cLabel + ","
               cTotalMenu   = cTotalMenu   + "TotalOf;" + cLabel + ";SumSelected" + ",".
    END.
  ELSE 
    DO ix = 1 TO hBrowse:NUM-COLUMNS:
      cLabel = DYNAMIC-FUNCTION("getStrippedSortLabel",hBrowse:GET-BROWSE-COLUMN(ix)).
      hField = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hBrowse:GET-BROWSE-COLUMN(ix):NAME) NO-ERROR.
      IF hBrowse:GET-BROWSE-COLUMN(ix):VISIBLE AND NOT ERROR-STATUS:ERROR AND hField:DATA-TYPE = "DECIMAL" THEN
        ASSIGN cTotalLabels = cTotalLabels + cLabel + ","
               cTotalFields = cTotalFields + hField:NAME + ","
               cTotalMenu   = cTotalMenu   + "TotalOf;" + cLabel + ";SumSelected" + ",".
    END.
END.
ELSE DO:
  ASSIGN cGraphFields      = ""
         cGraphLabels      = ""
         cGraphMenu        = "".

  IF cTotalFields NE "none" AND cTotalFields NE "" THEN 
    DO ix = 1 TO NUM-ENTRIES(cTotalFields) + 2:
      IF ix LE NUM-ENTRIES(cTotalFields) THEN
        hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,ENTRY(ix,cTotalFields)).
      ELSE IF ix = NUM-ENTRIES(cTotalFields) + 1 THEN
        hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"jbCountDistinct").
      ELSE IF ix = NUM-ENTRIES(cTotalFields) + 2 THEN
        hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"jbAverage").
      ELSE hColumn = ?.

      IF VALID-HANDLE(hColumn) AND hColumn:VISIBLE THEN DO:
        IF ix LE NUM-ENTRIES(cTotalFields) + 1 THEN
          cAccumTotalFields = cAccumTotalFields +
                              (IF cAccumTotalFields NE "" THEN "," ELSE "") + 
                              (IF ix LE NUM-ENTRIES(cTotalFields) THEN ENTRY(ix,cTotalFields)
                               ELSE "jbCountDistinct").

        cLabel = DYNAMIC-FUNCTION("getStrippedSortLabel",hColumn).

        IF (CAN-DO(cTotalFields,hColumn:NAME) AND NOT CAN-DO("jbAverage",hColumn:NAME))
          OR hColumn:NAME = "jbCountDistinct" THEN
          ASSIGN cTotalLabels = cTotalLabels + cLabel + ","
                 cTotalMenu   = cTotalMenu   + "TotalOf;" + cLabel + ";SumSelected" + ",".

        IF (CAN-DO(cTotalFields,hColumn:NAME) OR CAN-DO("jbAverage,jbCountDistinct",hColumn:NAME)) THEN
          ASSIGN cGraphLabels = cGraphLabels +  cLabel + ","
                 cGraphFields = cGraphFields + hColumn:NAME + "," 
                 cGraphMenu   = cGraphMenu   + "GraphOf;" +  cLabel + ";ExcelGraph" + ",".
      END.
    END.
  ELSE 
    DO ix = 1 TO hBrowse:NUM-COLUMNS:
      IF NOT hBrowse:GET-BROWSE-COLUMN(ix):VISIBLE THEN NEXT.
      cLabel = DYNAMIC-FUNCTION("getStrippedSortLabel",hBrowse:GET-BROWSE-COLUMN(ix)).

      hField = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hBrowse:GET-BROWSE-COLUMN(ix):NAME) NO-ERROR.
      IF (NOT ERROR-STATUS:ERROR AND hField:DATA-TYPE = "DECIMAL" AND NOT CAN-DO("jbAverage",hBrowse:GET-BROWSE-COLUMN(ix):NAME))
         OR hBrowse:GET-BROWSE-COLUMN(ix):NAME = "jbCountDistinct" THEN
        ASSIGN cTotalLabels = cTotalLabels + cLabel + ","
               cTotalMenu   = cTotalMenu   + "TotalOf;" + cLabel + ";SumSelected" + ",".

      IF (NOT ERROR-STATUS:ERROR AND hField:DATA-TYPE = "DECIMAL") OR CAN-DO("jbAverage,jbCountDistinct",hBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN
        ASSIGN cGraphLabels = cGraphLabels +  cLabel + ","
               cGraphFields = cGraphFields + hBrowse:GET-BROWSE-COLUMN(ix):NAME + "," 
               cGraphMenu   = cGraphMenu   + "GraphOf;" +  cLabel + ";ExcelGraph" + ",".
    END.

  ASSIGN cGraphLabels = TRIM(cGraphLabels,",")
         cGraphFields = TRIM(cGraphFields,",")
         cGraphMenu   = TRIM(cGraphMenu,",")
         .
  IF VALID-HANDLE(hExcelGraph) AND cGraphMenu NE "" AND NUM-ENTRIES(DYNAMIC-FUNCTION("getAttribute",hBrowse,"distinctcolumns")) = 1 THEN DO:
    DYNAMIC-FUNCTION("setObjectSourceProc",THIS-PROCEDURE).
    DYNAMIC-FUNCTION("NewMenuBand",hExcelGraph,cGraphMenu,"").
    hExcelGraph:SENSITIVE = INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")) < iMaxGraph.
  END.
END.

ASSIGN cTotalLabels = TRIM(cTotalLabels,",")
       cTotalFields = TRIM(cTotalFields,",")
       cTotalMenu   = TRIM(cTotalMenu,",")
       .

IF cTotalMenu NE "" THEN DO:
  DYNAMIC-FUNCTION("setObjectSourceProc",THIS-PROCEDURE).
  DYNAMIC-FUNCTION("NewMenuBand",hTotalsMenu,cTotalMenu,"").
END.  
ELSE 
  hTotalsMenu:SENSITIVE = NO.


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUseLocalData C-Win 
FUNCTION setUseLocalData RETURNS LOGICAL
  ( ibUseLocalData AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Must be set prior to InitializeObject. Use ExtraFlatViewRecord
------------------------------------------------------------------------------*/
IF ibUseLocalData THEN
  cUseLocalData = "yes".
ELSE
  cUseLocalData = "".

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUseStdAppserver C-Win 
FUNCTION setUseStdAppserver RETURNS LOGICAL
  ( INPUT ibUseStdAppServer AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bUseStdAppServer = ibUseStdAppServer.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWinTitle C-Win 
FUNCTION setWinTitle RETURNS LOGICAL
  ( INPUT icWinTitle AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN cOrgWinTitle = icWinTitle
       THIS-PROCEDURE:CURRENT-WINDOW:TITLE = icWinTitle.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWordButton C-Win 
FUNCTION setWordButton RETURNS LOGICAL
  ( INPUT ibWordButton AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bWordButton = ibWordButton.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StartQuery C-Win 
FUNCTION StartQuery RETURNS LOGICAL
  ( INPUT icFilterFields    AS CHAR,
    INPUT icFilterOperators AS CHAR,
    INPUT icFilterValues    AS CHAR,
    INPUT icInitSort        AS CHAR,
    INPUT icDistinctCols    AS CHAR,
    INPUT icAccumFields     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.
DEF VAR bLocal            AS LOG    NO-UNDO.
          
bLocal = DYNAMIC-FUNCTION("getAttribute",hBrowse,"useLocalData") = "yes".
IF bLocal THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"useLocalData","").

DYNAMIC-FUNCTION("setAttribute",hBrowse,"distinctcolumns",icDistinctCols).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"accumfields",icAccumFields).

IF icInitSort NE "" AND icDistinctCols = "" THEN DO:
  DYNAMIC-FUNCTION("setSortString",hBrowse,icInitSort).
  CreateSearchField(ENTRY(1,icInitSort)).
END.
ELSE IF icDistinctCols NE "" THEN DO:
  DYNAMIC-FUNCTION("setSortString",hBrowse,icDistinctCols).
  CreateSearchField(ENTRY(1,icDistinctCols)).
END.
ELSE DO:
  icInitSort = ENTRY(1,DYNAMIC-FUNCTION("getAttribute",hBrowse,"allViewFields")).
  DYNAMIC-FUNCTION("setSortString",hBrowse,icInitSort).
  CreateSearchField(icInitSort).
END.

IF icFilterFields NE "" THEN
  DYNAMIC-FUNCTION("InitDynFilter",hBrowse
                   ,icFilterFields
                   ,icFilterOperators
                   ,icFilterValues
                   ,"").

ELSE DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
END.

IF icDistinctCols NE "" THEN DO:    
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"jbCountDistinct").
  IF VALID-HANDLE(hColumn) THEN hColumn:VISIBLE = YES.

  IF icAccumFields NE "" AND NUM-ENTRIES(icAccumFields) = 1 THEN DO:
    hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"jbAverage").
    IF VALID-HANDLE(hColumn) THEN hColumn:VISIBLE = YES.
  END.

  DYNAMIC-FUNCTION("setBrowseColumns",hBrowse,
                   icDistinctCols    + (IF icAccumFields NE "" AND NUM-ENTRIES(icAccumFields) = 1 THEN 
                                          "," + icAccumFields + ",jbCountDistinct,jbAverage" 
                                        ELSE IF icAccumFields NE "" THEN
                                          "," + icAccumFields + ",jbCountDistinct"
                                        ELSE ",jbCountDistinct"),NO).    

  RUN DynAccumDone (hBrowse,"accum").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").
END.
ELSE IF bLocal THEN
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"useLocalData","yes").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StartQueryCls C-Win 
FUNCTION StartQueryCls RETURNS LOGICAL
  ( INPUT icFilterFields    AS CHAR,
    INPUT icFilterOperators AS CHAR,
    INPUT icFilterValues    AS CHAR,
    INPUT icInitSort        AS CHAR,
    INPUT icDistinctCols    AS CHAR,
    INPUT icAccumFields     AS CHAR) :
/*------------------------------------------------------------------------------
 Purpose: Enable the JBoxDynDataBrowse to use the method name StartQuery
 Notes:
------------------------------------------------------------------------------*/
        RETURN StartQuery(icFilterFields,icFilterOperators,icFilterValues,icInitSort,icDistinctCols,icAccumFields).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* MESSAGE PROGRAM-NAME(1) SKIP                                              */
/*         DYNAMIC-FUNCTION("getAttribute",ihBrowse,"QueryStatFieldValues")  */
/*         VIEW-AS ALERT-BOX.                                                */

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

