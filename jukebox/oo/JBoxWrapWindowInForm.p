&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : JBoxWrapWindowInForm.p 
    Purpose     : Wrap ABL window in ABL form

    Syntax      :

    Description :

    Author(s)   : brynjar@chemistry.no
    Created     : 01.01.11
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING System.Windows.Forms.* FROM ASSEMBLY.
USING Progress.Lang.* FROM ASSEMBLY.
USING Progress.Windows.* FROM ASSEMBLY. 
USING System.Windows.Forms.IWindowTarget FROM ASSEMBLY.

DEFINE INPUT PARAM ihProc   AS HANDLE NO-UNDO.
DEFINE INPUT PARAM ihWindow AS HANDLE NO-UNDO.

DEFINE VARIABLE oForm             AS JBoxWrapperForm NO-UNDO. 
DEFINE VARIABLE oChildForm        AS MDIChildForm NO-UNDO.
DEFINE VARIABLE oWindowContainer  AS Progress.Windows.WindowContainer NO-UNDO.
DEFINE VARIABLE bEmbedInChildForm AS LOG    NO-UNDO.
DEFINE VARIABLE ixNode            AS INT    NO-UNDO INIT 1000.
DEFINE VARIABLE hCurrTabFolder    AS HANDLE NO-UNDO.
DEFINE VARIABLE iCurrTabPage      AS INT    NO-UNDO.
DEFINE VARIABLE ix                AS INT    NO-UNDO.
DEFINE VARIABLE bHideTabs         AS LOG    NO-UNDO.
DEFINE VARIABLE bFocusToUC        AS LOG    NO-UNDO.
DEFINE VARIABLE hFocusToWidget    AS HANDLE NO-UNDO.
DEFINE VARIABLE bEnableReturn     AS LOG    NO-UNDO.
DEFINE VARIABLE hTimer            AS HANDLE NO-UNDO.
DEFINE VARIABLE hCurrJBoxObject   AS HANDLE NO-UNDO.
DEFINE VARIABLE hCurrUserContr    AS HANDLE NO-UNDO.
DEFINE VARIABLE cCurrFieldName    AS CHAR   NO-UNDO.
DEFINE VARIABLE bCurrSwitch       AS LOG    NO-UNDO.
DEF VAR         iWinContainerX    AS INT    NO-UNDO.
DEF VAR         iWinContainerY    AS INT    NO-UNDO.
DEF VAR         iDeltaWinY        AS INT    NO-UNDO.

DEF TEMP-TABLE ttUserControls
    FIELD oUserControl     AS Progress.Lang.Object
    FIELD oWidget          AS Progress.Lang.Object
    FIELD cUserControlType AS CHAR 
    FIELD cWidgetType      AS CHAR 
    FIELD hPlaceHolder     AS HANDLE /* Design widget, i.e rectangle, editor, date field, etc */
    FIELD bVisible         AS LOG
    FIELD hTabFolder       AS HANDLE
    FIELD iTabPage         AS INT
    FIELD hSourceProc      AS HANDLE
    FIELD hSuperProc       AS HANDLE
    FIELD hFrame           AS HANDLE
    FIELD cMethodSuffix    AS CHAR 
    FIELD iDeltaWidth      AS INTEGER
    FIELD iDeltaHeight     AS INTEGER
    FIELD cPosition        AS CHAR
    .

DEF VAR hbUserControls AS HANDLE NO-UNDO.
hbUserControls = BUFFER ttUserControls:HANDLE.

DEF TEMP-TABLE ttWinMenu
    FIELD hMenuItem     AS HANDLE
    FIELD iNodeIndex    AS INT
    FIELD oMenuItem     AS Progress.Lang.OBJECT    
    FIELD hTrigWidget   AS HANDLE
    FIELD cAccelerator  AS CHAR
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-buildWindowMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildWindowMenu Procedure 
FUNCTION buildWindowMenu RETURNS LOGICAL
  ( INPUT ihMenu       AS HANDLE,
    INPUT ioParent     AS CLASS Progress.Lang.Object )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentUserControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentUserControl Procedure 
FUNCTION getCurrentUserControl RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrTabFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrTabFolder Procedure 
FUNCTION getCurrTabFolder RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrTabPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrTabPage Procedure 
FUNCTION getCurrTabPage RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsMDIchild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsMDIchild Procedure 
FUNCTION getIsMDIchild RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserControlsBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUserControlsBuffer Procedure 
FUNCTION getUserControlsBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWindowForm Procedure 
FUNCTION getWindowForm RETURNS Progress.Lang.Object
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowSuper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWindowSuper Procedure 
FUNCTION getWindowSuper RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 27.95
         WIDTH              = 71.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
PUBLISH "EmbedInChildForm" (ihProc,OUTPUT bEmbedInChildForm). /* Target: JBoxRibbonMenu.p */

IF NOT bEmbedInChildForm AND ihWindow NE ? THEN DO:
  oForm = NEW JBoxWrapperForm(ihProc,THIS-PROCEDURE).

  iDeltaWinY = INT(DYNAMIC-FUNCTION("getAttribute",ihWindow,"DeltaWinY")) NO-ERROR.
  IF iDeltaWinY = ? THEN iDeltaWinY = 0.

  ASSIGN oWindowContainer = oForm:WindowContainer
         oWindowContainer:EmbeddedWindow = ihWindow
         oForm:Size = NEW System.Drawing.Size(ihWindow:WIDTH-PIXELS + 17,ihWindow:HEIGHT-PIXELS + 36 + iDeltaWinY)
         oWindowContainer:Size = NEW System.Drawing.Size(ihWindow:WIDTH-PIXELS,ihWindow:HEIGHT-PIXELS + iDeltaWinY)    
         .
    
  SUBSCRIBE TO "SetWinMenuActionState" ANYWHERE.
END.
ELSE IF bEmbedInChildForm THEN
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "SetChildWrapperForm" ANYWHERE.
ELSE IF bEmbedInChildForm = ? THEN
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "SetWrapperForm" ANYWHERE.
  
    
DYNAMIC-FUNCTION("NewObject",ihWindow,THIS-PROCEDURE,"super-procedure") NO-ERROR.

SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "IsWindowWrapped"        ANYWHERE.  /* If multiple calls to this program for same window (from suppredded windows) */
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "AdjustFormHeight"       ANYWHERE.  /* Publish from CreateStatusBar in ResizeLib */
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "ResizeFormComponents"   ANYWHERE.  /* Publish from DoWidgetResize in ResizeLib */
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "CloseForm"              ANYWHERE.
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "CurrentTabFolderPage"   ANYWHERE.  /* Published from tabfolder when a new page is added */
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "ViewHideTabUserControl" ANYWHERE.  /* Published from tabfolder when change of page */
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetDisplay"          ANYWHERE.  /* Published from DisplayFieldMap and DispBrwOverlayWidgets in jboxobjlib */*/
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetEnable"           ANYWHERE.  /* Published from setFieldMapState in jboxobjlib */*/
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetVisible"          ANYWHERE.  /* Published from setFieldMapState in jboxobjlib */*/
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetUndo"             ANYWHERE.  /* Published from UndoRecord in jboxuilib */       */
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetSave"             ANYWHERE.  /* Published from SaveRecord in jboxuilib */       */
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetPreSave"          ANYWHERE.  /* Published from SaveRecord in jboxuilib */       */
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetNew"              ANYWHERE.  /* Published from NewRecord in jboxuilib */        */
/*SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetFocus"            ANYWHERE.  /* Published from EntryOfWidget and EnterBrowseFillIn in jboxuilib */*/
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "ViewHideUserControl"    ANYWHERE.  /* Published from jbouilib.EndResizeBrowseColumn  */
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "ViewHideAllControls"    ANYWHERE.  /* Published from setLockWindowUpdate in resizelib.p */

SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "dotNetMethod"           ANYWHERE.  /* Published from libraries */

/*
IF SEARCH("JBoxJlwTimer.w") NE ? OR SEARCH("JBoxJlwTimer.r") NE ? THEN DO:
  RUN JBoxJlwTimer.w PERSIST SET hTimer ("CheckTimerEvent",200).
  DYNAMIC-FUNCTION("SetTimerName" IN hTimer,"DotNetFocusTimer").
END.  
*/

ON 'close' OF THIS-PROCEDURE DO:
  FOR EACH ttUserControls:
    DELETE OBJECT oUserControl NO-ERROR.
  END.

  IF VALID-HANDLE(hTimer) THEN
    APPLY "close" TO hTimer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AdjustFormHeight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdjustFormHeight Procedure 
PROCEDURE AdjustFormHeight :
/*------------------------------------------------------------------------------
  Purpose:     Publish from CreateStatusBar in ResizeLib. If a statusbar is added the size of the form must be adjusted accordingly
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihAdjustWin AS HANDLE NO-UNDO.
DEF INPUT PARAM iiDeltaY    AS INT    NO-UNDO.

IF NOT bEmbedInChildForm AND ihAdjustWin = ihWindow AND VALID-OBJECT(oForm) THEN 
  ASSIGN oForm:iDeltaYStatusBar = iiDeltaY
         oForm:Size = NEW System.Drawing.Size(ihWindow:WIDTH-PIXELS + 17,ihWindow:HEIGHT-PIXELS + 36 + iiDeltaY) 
         .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApplyWinMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyWinMenu Procedure 
PROCEDURE ApplyWinMenu :
/*------------------------------------------------------------------------------
  Purpose:     When a .net window menu is selected apply the corresponding (hidden) ABL menu-item 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icMenuTag AS CHAR NO-UNDO.

FIND FIRST ttWinMenu
     WHERE ttWinMenu.iNodeIndex = INTEGER(icMenuTag)
     NO-ERROR.

IF AVAIL ttWinMenu AND VALID-HANDLE(ttWinMenu.hTrigWidget) THEN DO:
  IF ttWinMenu.hTrigWidget:TOGGLE-BOX THEN DO:
    ttWinMenu.hTrigWidget:CHECKED = NOT ttWinMenu.hTrigWidget:CHECKED.
    RUN SetWinMenuActionState.
  END.
  ELSE
    APPLY "choose" TO ttWinMenu.hTrigWidget.      
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckTimerEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckTimerEvent Procedure 
PROCEDURE CheckTimerEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR bMyEnableReturn AS LOG NO-UNDO INIT YES.


IF bFocusToUC AND AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).

  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetFocus") THEN
    RUN dotNetFocus IN ttUserControls.hSourceProc.
  ELSE 
    DYNAMIC-INVOKE(ttUserControls.oUserControl,"dotNetFocus") NO-ERROR.

   ASSIGN bMyEnableReturn = NO
          bEnableReturn   = YES.
   
END.
bFocusToUC = NO.

IF VALID-HANDLE(hFocusToWidget) THEN DO:
  APPLY "entry" TO hFocusToWidget.
  bMyEnableReturn = NO.
END.

hFocusToWidget = ?.

/* Enable return on next timer tick to avoid double enter-action: */
IF bEnableReturn AND bMyEnableReturn AND AVAIL ttUserControls THEN DO:
  DYNAMIC-INVOKE(ttUserControls.oUserControl,"setEnableReturn",YES) NO-ERROR.
  bEnableReturn = NO.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseForm Procedure 
PROCEDURE CloseForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER ihCurrProc   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM     obFormClosed AS LOG    NO-UNDO.
IF ihCurrProc = ihProc THEN DO:
  oForm:Close().
  DELETE OBJECT oForm NO-ERROR.
  obFormClosed = YES.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CurrentTabFolderPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CurrentTabFolderPage Procedure 
PROCEDURE CurrentTabFolderPage :
/*------------------------------------------------------------------------------
  Purpose:     Published from tabfolder when a new page is added 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM ihCurrTabFolder  AS HANDLE NO-UNDO.
DEFINE INPUT PARAM iiCurrTabPage    AS INT NO-UNDO.

ASSIGN hCurrTabFolder = ihCurrTabFolder
       iCurrTabPage   = iiCurrTabPage
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetDisplay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetDisplay Procedure 
PROCEDURE dotNetDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       published from jboxobjlib:DisplayFieldMap and DispBrwOverlayWidgets
------------------------------------------------------------------------------*/
FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).
  
  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetDisplay") AND ENTRY(2,PROGRAM-NAME(2)," ") NE ttUserControls.hSourceProc:FILE-NAME THEN
    RUN dotNetDisplay IN ttUserControls.hSourceProc NO-ERROR.
  ELSE do:
    IF ttUserControls.cUserControlType = "JBoxDevExDateEdit" THEN
      CAST(ttUserControls.oUserControl,"JBoxDevExDateEdit"):dotNetDisplay() NO-ERROR.
    ELSE  
      DYNAMIC-INVOKE(ttUserControls.oUserControl,"dotNetDisplay") NO-ERROR.
  end.  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetEnable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetEnable Procedure 
PROCEDURE dotNetEnable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       published from jboxobjlib:setFieldMapState
------------------------------------------------------------------------------*/
/*DEF INPUT PARAM ihFieldMap  AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM ihUserContr AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM icFieldName AS CHAR   NO-UNDO.*/
/*DEF INPUT PARAM ibEnable    AS LOG    NO-UNDO.*/

FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).

  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetEnable") AND ENTRY(2,PROGRAM-NAME(2)," ") NE ttUserControls.hSourceProc:FILE-NAME THEN
    RUN dotNetEnable IN ttUserControls.hSourceProc (bCurrSwitch).
  ELSE
    DYNAMIC-INVOKE(ttUserControls.oUserControl,"dotNetEnable",bCurrSwitch) NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetFocus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetFocus Procedure 
PROCEDURE dotNetFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       published from jboxobjlib:EntryOfWidget
------------------------------------------------------------------------------*/
/*DEF INPUT PARAM ihFieldMap  AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM ihUserContr AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM icFieldName AS CHAR   NO-UNDO.*/

FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.

IF AVAIL ttUserControls AND NOT ttUserControls.cPosition BEGINS "right" THEN 
  bFocusToUC = YES. /* <- instruction for CheckTimerEvent */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetMethod Procedure 
PROCEDURE dotNetMethod :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icMethodName      AS CHAR   NO-UNDO.
DEF INPUT PARAM ihCurrJBoxObject  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCurrUserContr   AS HANDLE NO-UNDO.
DEF INPUT PARAM icCurrFieldName   AS CHAR   NO-UNDO.
DEF INPUT PARAM ibCurrSwitch      AS LOG    NO-UNDO. 

ASSIGN hCurrJBoxObject = ihCurrJBoxObject
       hCurrUserContr  = ihCurrUserContr
       cCurrFieldName  = icCurrFieldName
       bCurrSwitch     = ibCurrSwitch
       .

RUN VALUE(icMethodName) IN THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetNew) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetNew Procedure 
PROCEDURE dotNetNew :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      published from JboxUiLib:NewRecord 
------------------------------------------------------------------------------*/
/*DEF INPUT PARAM ihFieldMap  AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM ihUserContr AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM icFieldName AS CHAR   NO-UNDO.*/

FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).

  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetNew") AND ENTRY(2,PROGRAM-NAME(2)," ") NE ttUserControls.hSourceProc:FILE-NAME THEN
    RUN dotNetNew IN ttUserControls.hSourceProc.
  ELSE 
    DYNAMIC-INVOKE(ttUserControls.oUserControl,ENTRY(1,PROGRAM-NAME(1)," ")) NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetPreSave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetPreSave Procedure 
PROCEDURE dotNetPreSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*DEF INPUT PARAM ihFieldMap  AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM ihUserContr AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM icFieldName AS CHAR   NO-UNDO.*/


FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).

  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetPreSave") AND ENTRY(2,PROGRAM-NAME(2)," ") NE ttUserControls.hSourceProc:FILE-NAME THEN
    RUN dotNetPreSave IN ttUserControls.hSourceProc.
  ELSE 
    DYNAMIC-INVOKE(ttUserControls.oUserControl,"dotNetPreSave") NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetSave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetSave Procedure 
PROCEDURE dotNetSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*DEF INPUT PARAM ihFieldMap  AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM ihUserContr AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM icFieldName AS CHAR   NO-UNDO.*/


FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).

  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetSave") AND ENTRY(2,PROGRAM-NAME(2)," ") NE ttUserControls.hSourceProc:FILE-NAME THEN
    RUN dotNetSave IN ttUserControls.hSourceProc.
  ELSE 
    DYNAMIC-INVOKE(ttUserControls.oUserControl,"dotNetSave").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetUndo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetUndo Procedure 
PROCEDURE dotNetUndo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      published from JboxUiLib:UndoRecord 
------------------------------------------------------------------------------*/
/*DEF INPUT PARAM ihFieldMap  AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM ihUserContr AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM icFieldName AS CHAR   NO-UNDO.*/


FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).

  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetUndo") AND ENTRY(2,PROGRAM-NAME(2)," ") NE ttUserControls.hSourceProc:FILE-NAME THEN
    RUN dotNetUndo IN ttUserControls.hSourceProc.
  ELSE 
    DYNAMIC-INVOKE(ttUserControls.oUserControl,ENTRY(1,PROGRAM-NAME(1)," ")) NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetVisible) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetVisible Procedure 
PROCEDURE dotNetVisible :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       published from jboxobjlib:setFieldMapState
------------------------------------------------------------------------------*/
/*DEF INPUT PARAM ihFieldMap  AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM ihUserContr AS HANDLE NO-UNDO.*/
/*DEF INPUT PARAM icFieldName AS CHAR   NO-UNDO.*/
/*DEF INPUT PARAM ibVisible   AS LOG    NO-UNDO.*/

FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceHolder = hCurrUserContr
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  DYNAMIC-FUNCTION("setCurrentObject",ttUserControls.hPlaceHolder).

  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,"dotNetVisible") AND ENTRY(2,PROGRAM-NAME(2)," ") NE ttUserControls.hSourceProc:FILE-NAME THEN
    RUN dotNetVisible IN ttUserControls.hSourceProc (bCurrSwitch).
  ELSE
    DYNAMIC-INVOKE(ttUserControls.oUserControl,"dotNetVisible",bCurrSwitch) NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFormObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFormObject Procedure 
PROCEDURE getFormObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ooForm AS JBoxWrapperForm NO-UNDO.

ooForm = oForm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InvokeMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvokeMethod Procedure 
PROCEDURE InvokeMethod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihObject AS HANDLE NO-UNDO.
DEF INPUT PARAM icMethod AS CHAR   NO-UNDO.

FIND FIRST ttUserControls WHERE ttUserControls.hPlaceHolder = ihObject
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  IF NOT icMethod MATCHES "*" + ttUserControls.cMethodSuffix THEN
    icMethod = icMethod + ttUserControls.cMethodSuffix.
  IF CAN-DO(ttUserControls.hSourceProc:INTERNAL-ENTRIES,icMethod) THEN
    RUN VALUE(icMethod) IN ttUserControls.hSourceProc.
  ELSE 
    DYNAMIC-INVOKE(ttUserControls.oUserControl,icMethod).
END.
ELSE IF VALID-HANDLE(ihObject) THEN RUN SUPER (ihObject,icMethod).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsWindowWrapped) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IsWindowWrapped Procedure 
PROCEDURE IsWindowWrapped :
/*------------------------------------------------------------------------------
  Purpose:     Check if the window super-procedure (this procedure) ia already running.
               If so, assign it as super also to the additional procedure(s) running within the
               window container (typically suppressed windows)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihWrapWindow AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ohSuper      AS HANDLE NO-UNDO.
 
IF ihWrapWindow = ihWindow THEN
  ohSuper = THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RegisterWithJukeBox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegisterWithJukeBox Procedure 
PROCEDURE RegisterWithJukeBox :
/*------------------------------------------------------------------------------
  Purpose:     Register the design 4GL widget as JukeBox object:
  Parameters:  <none>
  Notes:       The user control must contain the "getDotNetWidget" method
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihPlaceHolder     AS HANDLE NO-UNDO.
DEF INPUT PARAM ioUserControl     AS Progress.Lang.Object NO-UNDO.
DEF INPUT PARAM icUserControlType AS CHAR NO-UNDO.
DEF INPUT PARAM ihSourceProc      AS HANDLE NO-UNDO.
DEF INPUT PARAM icMethodSuffix    AS CHAR NO-UNDO.

DYNAMIC-FUNCTION("NewObject",ihPlaceHolder:WINDOW,ihPlaceHolder,icUserControlType).

CREATE ttUserControls.
ASSIGN ttUserControls.oUserControl     = ioUserControl
       ttUserControls.cUserControlType = icUserControlType
       ttUserControls.oWidget          = DYNAMIC-INVOKE(ioUserControl,"getDotNetWidget")
       ttUserControls.hPlaceHolder     = ihPlaceHolder
       ttUserControls.hTabFolder       = hCurrTabFolder
       ttUserControls.iTabPage         = iCurrTabPage
       ttUserControls.hSourceProc      = IF VALID-HANDLE(ihSourceProc) THEN ihSourceProc ELSE SOURCE-PROCEDURE
       ttUserControls.cMethodSuffix    = icMethodSuffix
       .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResizeFormComponents) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResizeFormComponents Procedure 
PROCEDURE ResizeFormComponents :
/*------------------------------------------------------------------------------
  Purpose:     After window resize set the user control size and position according to their design widget
  Parameters:  <none>
  Notes:       Published from ResizeLib.p
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM ihResizeWin AS HANDLE NO-UNDO.

DEFINE VARIABLE oUserControlInstance AS System.Windows.Forms.Control NO-UNDO.
DEFINE VARIABLE oWidgetInstance      AS System.Windows.Forms.Control NO-UNDO.
DEF VAR iDeltaX       AS INT NO-UNDO.
DEF VAR iDeltaY       AS INT NO-UNDO.

IF ihResizeWin = ihWindow THEN
  FOR EACH ttUserControls:
    IF VALID-HANDLE(ttUserControls.hPlaceHolder) THEN DO:
      oUserControlInstance = DYNAMIC-CAST(ttUserControls.oUserControl,ttUserControls.cUserControlType).
      oWidgetInstance      = CAST(ttUserControls.oWidget,"System.Windows.Forms.Control").
  
      oUserControlInstance:Location = NEW System.Drawing.Point(iWinContainerX + DYNAMIC-FUNCTION("getAbsPosition",ttUserControls.hPlaceHolder,"x")
                                                            + (IF ttUserControls.cPosition = "right-of" THEN ttUserControls.hPlaceHolder:WIDTH-PIXELS - 2 
                                                               ELSE IF ttUserControls.cPosition = "right" THEN ttUserControls.hPlaceHolder:WIDTH-PIXELS - oUserControlInstance:Width
                                                               ELSE 0),
                                                               iWinContainerY + DYNAMIC-FUNCTION("getAbsPosition",ttUserControls.hPlaceHolder,"y") 
                                                            + (IF NOT bEmbedInChildForm THEN oForm:iDeltaYWinMenu ELSE 0)
                                                            + iDeltaWinY).
            
      IF NOT ttUserControls.cPosition BEGINS "right"  
         AND (oUserControlInstance:WIDTH NE ttUserControls.hPlaceHolder:WIDTH-PIXELS + ttUserControls.iDeltaWidth
              OR oUserControlInstance:HEIGHT NE ttUserControls.hPlaceHolder:HEIGHT-PIXELS + ttUserControls.iDeltaHeight) THEN DO:
                
        ASSIGN iDeltaX = ttUserControls.hPlaceHolder:WIDTH-PIXELS - oWidgetInstance:Width
               iDeltaY = ttUserControls.hPlaceHolder:HEIGHT-PIXELS - oWidgetInstance:Height
               oUserControlInstance:WIDTH = ttUserControls.hPlaceHolder:WIDTH-PIXELS + ttUserControls.iDeltaWidth 
               oUserControlInstance:HEIGHT = ttUserControls.hPlaceHolder:HEIGHT-PIXELS               
               oWidgetInstance:Width = ttUserControls.hPlaceHolder:WIDTH-PIXELS + ttUserControls.iDeltaWidth
               oWidgetInstance:Height = ttUserControls.hPlaceHolder:HEIGHT-PIXELS + ttUserControls.iDeltaHeight
               .
        IF ttUserControls.cUserControlType = "JBoxMsTabs" THEN /* AND NOT PROGRAM-NAME(2) BEGINS "setSplitBar" THEN */ 
          PUBLISH "ResizeTabWindows" (ttUserControls.hPlaceHolder,oUserControlInstance:Left,oUserControlInstance:Top,iDeltaX,iDeltaY).
      END.         
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetChildControlProperty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetChildControlProperty Procedure 
PROCEDURE SetChildControlProperty :
/*------------------------------------------------------------------------------
  Purpose:     Set property for .net control added by CreateChildControl
  Parameters:  <none>
  Notes:       Download from PSDN. Written by Mike Fechner
------------------------------------------------------------------------------*/
DEF INPUT PARAM poControl AS System.Windows.Forms.Control NO-UNDO.
DEF INPUT PARAM pcPropertyName AS CHARACTER NO-UNDO.
DEF INPUT PARAM poPropertyValue AS System.Object NO-UNDO.
  
DEFINE VARIABLE oType     AS System.Type                    NO-UNDO .
DEFINE VARIABLE oProperty AS System.Reflection.PropertyInfo NO-UNDO .  
  
IF NOT VALID-OBJECT (poControl) THEN 
    UNDO, THROW NEW AppError ("No valid control!", 0) .
      
oType = poControl:GetType().

oProperty = oType:GetProperty (pcPropertyName) .
    
IF VALID-OBJECT(oProperty) THEN 
  oProperty:SetValue (poControl, poPropertyValue, ?).           
ELSE
  UNDO, THROW NEW AppError ("Invalid Property Name " + pcPropertyName, 0) .             

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetChildWrapperForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetChildWrapperForm Procedure 
PROCEDURE SetChildWrapperForm :
/*------------------------------------------------------------------------------
  Purpose:     Published from the menu (JBoxRibbonMenu.p) to set the childform if the window is
               shown in a MDI container
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiWrapProc  AS HANDLE NO-UNDO.
DEF INPUT PARAM ioMdiChildForm AS MDIChildForm NO-UNDO.

IF iiWrapProc = ihProc THEN
  oChildForm = ioMdiChildForm
  .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWinContainerXY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWinContainerXY Procedure
PROCEDURE SetWinContainerXY:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiWinContainerX AS INT NO-UNDO.
DEF INPUT PARAM iiWinContainerY AS INT NO-UNDO.

ASSIGN iWinContainerX = iiWinContainerX
       iWinContainerY = iiWinContainerY.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-SetWinMenuActionState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWinMenuActionState Procedure 
PROCEDURE SetWinMenuActionState :
/*------------------------------------------------------------------------------
  Purpose:     Enable/disable .net window menu items according to the latest (user) action              
  Parameters:  <none>
  Notes:       Published from jboxobjlib.p, setToolbar 
------------------------------------------------------------------------------*/
DEF VAR oMenuItem AS "System.Windows.Forms.ToolStripMenuItem" NO-UNDO.
DEF VAR hCorrBtn  AS HANDLE NO-UNDO.

FOR EACH ttWinMenu
    WHERE ttWinMenu.iNodeIndex > 1000
    :

  IF VALID-HANDLE(ttWinMenu.hTrigWidget) THEN DO:
    oMenuItem = CAST(ttWinMenu.oMenuItem,"System.Windows.Forms.ToolStripMenuItem").
    IF ttWinMenu.hTrigWidget:PRIVATE-DATA NE "" THEN DO:
      hCorrBtn = WIDGET-HANDLE(ttWinMenu.hTrigWidget:PRIVATE-DATA) NO-ERROR.
      IF VALID-HANDLE(hCorrBtn) AND SEARCH(hCorrBtn:IMAGE) NE ? THEN   
        oMenuItem:Image = System.Drawing.Image:FromFile(SEARCH(hCorrBtn:IMAGE)).        
    END.    
    oMenuItem:Enabled = ttWinMenu.hTrigWidget:SENSITIVE.
    IF ttWinMenu.hTrigWidget:TOGGLE-BOX THEN
      oMenuItem:Checked = ttWinMenu.hTrigWidget:Checked.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetWrapperForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWrapperForm Procedure
PROCEDURE SetWrapperForm:
/*------------------------------------------------------------------------------
  Purpose:     Published from a tab-folder (JBoxMsTabs.cls) to set the form if the window is
               not shown in a MDI container
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiWrapProc  AS HANDLE NO-UNDO.
DEF INPUT PARAM ioForm      AS Progress.Windows.Form NO-UNDO.

IF iiWrapProc = ihProc THEN
  oForm = CAST(ioForm,"JBoxWrapperForm").
  .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-ShowForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowForm Procedure 
PROCEDURE ShowForm :
/*------------------------------------------------------------------------------
  Purpose:    Show the form and optionally build the .net window menu - if the window is not 
              embedded in a child form
  Parameters:  <none>
  Notes:      icParam currently not in use 
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM icParam AS CHAR NO-UNDO.

DEFINE VARIABLE oUserControlInstance AS System.Windows.Forms.Control NO-UNDO.
DEFINE VARIABLE hMenu                AS HANDLE NO-UNDO.

IF NOT bEmbedInChildForm THEN DO:
  oForm:Text = ihWindow:TITLE.

  IF oForm:menuStrip1 = ? THEN DO:
    hMenu = ihWindow:MENU-BAR NO-ERROR.
    IF NOT VALID-HANDLE(hMenu) THEN
      hMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",ihWindow,"MENUBAR")) NO-ERROR.
  
    IF VALID-HANDLE(hMenu) THEN DO:
      oForm:menuStrip1 = NEW System.Windows.Forms.MenuStrip().
      oForm:menuStrip1:SuspendLayout().
  
      buildWindowMenu(hMenu:FIRST-CHILD,?).
  
      oForm:Controls:Add(oForm:menuStrip1).
      oForm:MainMenuStrip = oForm:menuStrip1.
      oForm:menuStrip1:ResumeLayout(FALSE).
      oForm:menuStrip1:PerformLayout().
  
  
      oForm:bSkipResize = YES.
  
      ASSIGN oForm:iDeltaYWinMenu      = 22
             oForm:Size = NEW System.Drawing.Size(ihWindow:WIDTH-PIXELS + 17,
                                                  ihWindow:HEIGHT-PIXELS + 36 + oForm:iDeltaYWinMenu /* + oForm:iDeltaYStatusBar */).
             oWindowContainer:Location = NEW System.Drawing.Point(0, oForm:iDeltaYWinMenu).
             .
  
      RUN ResizeFormComponents(ihWindow).
  
      RUN SetWinMenuActionState IN TARGET-PROCEDURE.
  
      oForm:ResumeLayout(FALSE).
      oForm:PerformLayout().
      oForm:menuStrip1:BringToFront().
  
    END.
  END.
  
  oForm:Show().   

  DEF VAR hSfr AS HANDLE NO-UNDO.
  hSfr = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",ihWindow,"StatusBarFrame")) NO-ERROR.
  IF VALID-HANDLE(hSfr) THEN DO:
    hSfr:Y = ihWindow:HEIGHT-PIXELS - hsfr:HEIGHT-PIXELS NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
      MESSAGE ERROR-STATUS:GET-MESSAGE(1)
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
      hSfr:MOVE-TO-TOP().
      oForm:Height = oForm:Height + 1.
    END.
  END.

END.
ELSE IF VALID-OBJECT(oChildForm) THEN oChildForm:BringToFront().

/*
IF icPage NE "" THEN
  FOR EACH ttUserControls:
    oUserControlInstance = DYNAMIC-CAST(ttUserControls.oUserControl,ttUserControls.cUserControlType).
    IF ttUserControls.cPage = icPage THEN
      oUserControlInstance:BringToFront().
    ELSE
      oUserControlInstance:SendToBack().
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewHideAllControls) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewHideAllControls Procedure 
PROCEDURE ViewHideAllControls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Published from resizelib.setLockWindowUpdate
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihCurrWindow  AS HANDLE NO-UNDO.
DEF INPUT PARAM ibView        AS LOG    NO-UNDO.

IF ihCurrWindow NE ihWindow THEN RETURN.

DEF VAR oUserControlInstance AS System.Windows.Forms.Control NO-UNDO.

IF NOT PROGRAM-NAME(4) BEGINS "DisplayRecord" AND NOT PROGRAM-NAME(4) BEGINS "DoFill" AND NOT PROGRAM-NAME(4) BEGINS "StartWindow" THEN 
  FOR EACH ttUserControls
      WHERE ttUserControls.bVisible
      :
    oUserControlInstance = DYNAMIC-CAST(ttUserControls.oUserControl,ttUserControls.cUserControlType).
    IF ibView THEN 
      oUserControlInstance:BringToFront().
    ELSE
      oUserControlInstance:SendToBack().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewHideTabUserControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewHideTabUserControl Procedure 
PROCEDURE ViewHideTabUserControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM ihCurrTabFolder  AS HANDLE NO-UNDO.
DEFINE INPUT PARAM iiCurrTabPage    AS INT NO-UNDO.

DEF VAR oUserControlInstance AS System.Windows.Forms.Control NO-UNDO.
DEF VAR cChildTabs           AS CHAR   NO-UNDO.
DEF VAR hChildTab            AS HANDLE NO-UNDO.
DEF VAR cTabProcList         AS CHAR   NO-UNDO.
DEF VAR ix                   AS INT    NO-UNDO.

IF VALID-HANDLE(ihCurrTabFolder) THEN DO:
  FOR EACH ttUserControls
      WHERE ttUserControls.hTabFolder = ihCurrTabFolder
      :
    oUserControlInstance = DYNAMIC-CAST(ttUserControls.oUserControl,ttUserControls.cUserControlType).
    IF ttUserControls.iTabPage = iiCurrTabPage AND NOT bHideTabs AND NOT ttUserControls.hPlaceHolder:HIDDEN THEN DO:
      oUserControlInstance:BringToFront().
      ttUserControls.bVisible = YES.
    END.
    ELSE DO:
      oUserControlInstance:SendToBack().
      ttUserControls.bVisible = NO.
    END.
  END.
  cChildTabs = DYNAMIC-FUNCTION("getAttribute",ihCurrTabFolder,"childTabFolders").
  IF cChildTabs NE "" THEN DO:
    cTabProcList = DYNAMIC-FUNCTION("getAttribute",ihCurrTabFolder,"tabProcList").
    DO ix = 1 TO NUM-ENTRIES(cChildTabs): /* In case of multiple child tabs on same parent page, see initPages in JBoxJlwTabFolder.w */
      hChildTab = WIDGET-HANDLE(ENTRY(ix,cChildTabs)) NO-ERROR.
      IF VALID-HANDLE(hChildTab) THEN DO:
        bHideTabs = STRING(iiCurrTabPage) NE DYNAMIC-FUNCTION("getAttribute",hChildTab,"parentTabPage").
        RUN ViewHideTabUserControl (hChildTab,DYNAMIC-FUNCTION("getCurrentTab" IN hChildTab)).
      END.
    END.
    bHideTabs = NO.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewHideUserControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewHideUserControl Procedure 
PROCEDURE ViewHideUserControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Published from jbouilib.EndResizeBrowseColumn 
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihPlaceholder AS HANDLE NO-UNDO.
DEF INPUT PARAM ibView        AS LOG    NO-UNDO.

DEF VAR oUserControlInstance AS System.Windows.Forms.Control NO-UNDO.
DEF VAR oWidgetInstance      AS System.Windows.Forms.Control NO-UNDO.

FIND FIRST ttUserControls
     WHERE ttUserControls.hPlaceholder = ihPlaceholder
     NO-ERROR.
IF AVAIL ttUserControls THEN DO:
  oUserControlInstance = DYNAMIC-CAST(ttUserControls.oUserControl,ttUserControls.cUserControlType).
  IF (ihPlaceHolder:HIDDEN AND ibView = ?) OR NOT ibView THEN DO:
    oUserControlInstance:SendToBack().
    ttUserControls.bVisible = NO.
  END.
  ELSE DO:
    oWidgetInstance      = CAST(ttUserControls.oWidget,"System.Windows.Forms.Control").

    oUserControlInstance:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",ttUserControls.hPlaceHolder,"x")
                                                          + (IF ttUserControls.cPosition = "right-of" THEN ttUserControls.hPlaceHolder:WIDTH-PIXELS - 2 
                                                             ELSE IF ttUserControls.cPosition = "right" THEN ttUserControls.hPlaceHolder:WIDTH-PIXELS - oUserControlInstance:Width
                                                             ELSE 0),
                                                             DYNAMIC-FUNCTION("getAbsPosition",ttUserControls.hPlaceHolder,"y") 
                                                             + (IF NOT bEmbedInChildForm THEN oForm:iDeltaYWinMenu ELSE 0)
                                                             + iDeltaWinY).
    IF NOT ttUserControls.cPosition BEGINS "right" THEN                                                         
      ASSIGN oUserControlInstance:Width = ttUserControls.hPlaceHolder:WIDTH-PIXELS + ttUserControls.iDeltaWidth
             oUserControlInstance:Height = ttUserControls.hPlaceHolder:HEIGHT-PIXELS.

    oWidgetInstance:Width = ttUserControls.hPlaceHolder:WIDTH-PIXELS.
    oWidgetInstance:Height = ttUserControls.hPlaceHolder:HEIGHT-PIXELS.
    oUserControlInstance:BringToFront().
    ttUserControls.bVisible = YES.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WaitForForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WaitForForm Procedure 
PROCEDURE WaitForForm :
/*------------------------------------------------------------------------------
  Purpose:     When running a window from the AppBuilder RUN this procedure to replace the AppBuilder wait-for
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
WAIT-FOR System.Windows.Forms.Application:RUN(oForm).
QUIT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-widgetFocus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetFocus Procedure 
PROCEDURE widgetFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFocusToWidget AS HANDLE NO-UNDO.

hFocusToWidget = ihFocusToWidget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-buildWindowMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildWindowMenu Procedure 
FUNCTION buildWindowMenu RETURNS LOGICAL
  ( INPUT ihMenu       AS HANDLE,
    INPUT ioParent     AS CLASS Progress.Lang.Object ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hChild        AS HANDLE NO-UNDO.
DEF VAR cDummy        AS CHAR   NO-UNDO.
DEF VAR bFirstInGroup AS LOG    NO-UNDO.
DEF VAR hCorrBtn      AS HANDLE NO-UNDO.
DEF VAR cImage        AS CHAR   NO-UNDO.

DEF BUFFER ttWinMenu FOR ttWinMenu.

hChild = ihMenu.

REPEAT WHILE VALID-HANDLE(hChild):
  CASE hChild:TYPE:
    WHEN "SUB-MENU" THEN DO:
      CREATE ttWinMenu.
      ASSIGN ttWinMenu.iNodeIndex = ixNode
             ttWinMenu.oMenuItem = oForm:AddSubMenu(
                                    ioParent,ttWinMenu.iNodeIndex,hChild:LABEL,"","",bFirstInGroup)
             ixNode               = ixNode + 1
             bFirstInGroup        = NO
             .
      IF VALID-HANDLE(hChild:FIRST-CHILD) THEN
        buildWindowMenu(hChild:FIRST-CHILD,ttWinMenu.oMenuItem).
    END.
    WHEN "MENU-ITEM" THEN DO:
      IF hChild:SUBTYPE = "RULE" THEN
        bFirstInGroup = YES.
      ELSE DO:
        cImage = "".
        IF hChild:PRIVATE-DATA NE "" THEN DO:
          hCorrBtn = WIDGET-HANDLE(hChild:PRIVATE-DATA) NO-ERROR.
          IF VALID-HANDLE(hCorrBtn) THEN
            cImage = hCorrBtn:IMAGE.               
        END.
        CREATE ttWinMenu.
        ASSIGN ttWinMenu.iNodeIndex   = ixNode
               ttWinMenu.oMenuItem    = oForm:AddMenuItem(ioParent,ttWinMenu.iNodeIndex,hChild:LABEL,
                                                          cImage,
                                                          hChild:ACCELERATOR,bFirstInGroup)
               ttWinMenu.hTrigWidget  = hChild
               ttWinMenu.cAccelerator = hChild:ACCELERATOR
               ixNode                 = ixNode + 1
               bFirstInGroup          = NO
               .
      END.
    END.
  END CASE.
  hChild = hChild:NEXT-SIBLING.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentUserControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentUserControl Procedure 
FUNCTION getCurrentUserControl RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF AVAIL ttUserControls THEN
  RETURN ttUserControls.hPlaceHolder.
ELSE
  RETURN hCurrUserContr. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrTabFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrTabFolder Procedure 
FUNCTION getCurrTabFolder RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hCurrTabFolder.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrTabPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrTabPage Procedure 
FUNCTION getCurrTabPage RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCurrTabPage.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsMDIchild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsMDIchild Procedure 
FUNCTION getIsMDIchild RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RETURN bEmbedInChildForm.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserControlsBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUserControlsBuffer Procedure 
FUNCTION getUserControlsBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hbUserControls.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWindowForm Procedure 
FUNCTION getWindowForm RETURNS Progress.Lang.Object
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF bEmbedInChildForm THEN 
  RETURN CAST(oChildForm,"Progress.Lang.Object").
ELSE 
  RETURN CAST(oForm,"Progress.Lang.Object").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowSuper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWindowSuper Procedure 
FUNCTION getWindowSuper RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN THIS-PROCEDURE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

