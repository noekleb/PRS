&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Brynjar Hasle
    Created     : May 2010
    Notes       : iNodeIndex is created from the menu database but is also used for designated
                  programs:
                  -1000  : Alarm window
                  -10000 : Start window (panel)
                  -1..   : Tab groups generated from sub-menus in the menu definition
                  -1001 to -9999 : Assigned to new tabs published from the application (StartTabWindow)
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING Progress.Windows.*.
USING Infragistics.Win.UltraWinToolbars.*.

DEF VAR bOk              AS LOG    NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR hParent          AS HANDLE NO-UNDO.
DEF VAR iMainMenuId      AS INT    NO-UNDO.
DEF VAR httMenu          AS HANDLE NO-UNDO.
DEF VAR httMenuBuffer    AS HANDLE NO-UNDO.
DEF VAR hParameterField  AS HANDLE NO-UNDO.
DEF VAR hMultiple        AS HANDLE NO-UNDO.
DEF VAR iTimerInterval   AS INT    NO-UNDO.
DEF VAR hTimer           AS HANDLE NO-UNDO.
DEF VAR iCurrMenuStyle   AS INT    NO-UNDO.
DEF VAR cTimerServerProc AS CHAR   NO-UNDO.
DEF VAR cTimerClientProc AS CHAR   NO-UNDO.
DEF VAR vObject          AS CLASS Progress.Lang.Object NO-UNDO.
DEF VAR cObjectType      AS CHAR   NO-UNDO.
DEF VAR goJBoxMainForm   AS JBoxMainForm NO-UNDO.
DEF VAR hWin             AS HANDLE NO-UNDO.
DEF VAR hEmbedded        AS HANDLE NO-UNDO.
DEF VAR iDesignHeight    AS INTEGER NO-UNDO.
DEF VAR iDesignWidth     AS INTEGER NO-UNDO.
DEF VAR vGrpWinMenu      AS CLASS Progress.Lang.Object NO-UNDO.
DEF VAR ixNode           AS INT NO-UNDO INIT 1000.
DEF VAR vCurrRibbonTab   AS CLASS Progress.Lang.Object NO-UNDO.
DEF VAR oChildForm       AS Progress.Windows.Form   NO-UNDO.
DEF VAR bWinStatusArea   AS LOG NO-UNDO.
DEF VAR hStatusFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrNodeKey     AS INT NO-UNDO.
DEF VAR iClientXsize     AS INT NO-UNDO INIT 1200.
DEF VAR iClientYsize     AS INT NO-UNDO INIT 800.
DEF VAR cWinMenuGrpLabel AS CHAR NO-UNDO INIT "Behandle".
DEF VAR bDontQuit        AS LOG  NO-UNDO.
DEF VAR bEmbeddedForm    AS LOG  NO-UNDO.
DEF VAR bRibbonMinimized AS LOG  NO-UNDO.
DEF VAR cWinTitle        AS CHAR NO-UNDO.
DEF VAR cStyleSheet      AS CHAR NO-UNDO.
DEF VAR bIsChildWindow   AS LOG  NO-UNDO.
DEF VAR bCreateStatusBar AS LOG  NO-UNDO INIT YES.
DEF VAR iLastMenuId      AS INT  NO-UNDO.
DEF VAR bLogProgramExec  AS LOG  NO-UNDO.
DEF VAR hMainMenu        AS HANDLE NO-UNDO.
DEF VAR cTimerContext    AS CHAR   NO-UNDO.
DEF VAR cLastTimerEvent  AS CHAR   NO-UNDO.
DEF VAR cPanelURL        AS CHAR   NO-UNDO.
DEF VAR iNewTabWinIdx    AS INT    NO-UNDO INIT -1001.
DEF VAR oLastMdiTab      AS Infragistics.Win.UltraWinTabbedMdi.MdiTab NO-UNDO.
DEF VAR bCancelFormClose AS LOG    NO-UNDO.
DEF VAR cCustomTabGroupTitle AS CHAR NO-UNDO.
DEF VAR cCustomProgramList   AS CHAR NO-UNDO.
DEF VAR cHiddenSubMenuLabels AS CHAR NO-UNDO.
DEF VAR cHiddenMenuItemParams AS CHAR NO-UNDO.
DEF VAR cInitQuickAccess      AS CHAR NO-UNDO.
DEF VAR cWinStartParam        AS CHAR NO-UNDO.
DEF VAR cProgramMultiInstanceList AS CHAR NO-UNDO.
DEF VAR bSizedToFit           AS LOG NO-UNDO.

DEF TEMP-TABLE ttProgram
    FIELD oRibbonObj   AS CLASS Progress.Lang.Object
    FIELD oChildForm   AS CLASS Progress.Lang.Object
    FIELD oMdiTab      AS CLASS Progress.Lang.Object
    FIELD iNodeIndex   AS INT
    FIELD cObjectType  AS CHAR
    FIELD cProcName    AS CHAR
    FIELD hWinProc     AS HANDLE
    FIELD iFormTag     AS INT
    FIELD cMenuTitle   AS CHAR 
    FIELD hWinMenuItem AS HANDLE
    FIELD hParent      AS HANDLE
    FIELD bChildWin    AS LOG
    FIELD hTrigWidget  AS HANDLE 
    FIELD cMenuLabel   AS CHAR 
    FIELD cLaunchType  AS CHAR
    FIELD bEmbed       AS LOG 
    FIELD cAccelerator AS CHAR
    FIELD cImage       AS CHAR
    FIELD bUseTimer    AS LOG 
    FIELD iJBoxMenuId  AS INT
    FIELD cParameter   AS CHAR
    FIELD cMenuType    AS CHAR
    FIELD hWindow      AS HANDLE
    FIELD oContainer   AS CLASS Progress.Lang.Object
    .
DEF VAR httProgram AS HANDLE NO-UNDO.
httProgram = BUFFER ttProgram:HANDLE.

DEF TEMP-TABLE ttMenuAccelerators
   FIELD cAccelerator    AS CHAR 
   FIELD bTriggerDefined AS LOG
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

&IF DEFINED(EXCLUDE-BuildActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildActionRibbonGroup Procedure 
FUNCTION BuildActionRibbonGroup RETURNS LOGICAL
  (INPUT ihMenu       AS HANDLE,
   INPUT ioParent     AS CLASS Progress.Lang.Object,
   INPUT icParentType AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearActionRibbonGroup Procedure 
FUNCTION ClearActionRibbonGroup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmbedMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EmbedMe Procedure 
FUNCTION EmbedMe RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsWindowActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsWindowActive Procedure 
FUNCTION getIsWindowActive RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParameter Procedure 
FUNCTION getParameter RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProgramMultiInstanceList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProgramMultiInstanceList Procedure 
FUNCTION getProgramMultiInstanceList RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStartPanelURL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStartPanelURL Procedure 
FUNCTION getStartPanelURL RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTtProgramHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTtProgramHandle Procedure 
FUNCTION getTtProgramHandle RETURNS HANDLE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWindowFrames Procedure 
FUNCTION getWindowFrames RETURNS HANDLE
  ( INPUT hWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveQuickAccess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD saveQuickAccess Procedure 
FUNCTION saveQuickAccess RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppStyleSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppStyleSheet Procedure 
FUNCTION setAppStyleSheet RETURNS LOGICAL
  ( INPUT icStyleSheet AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setClientSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setClientSize Procedure 
FUNCTION setClientSize RETURNS LOGICAL
  ( INPUT iiClientXsize AS INT,
    INPUT iiClientYsize AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCreateStatusBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCreateStatusBar Procedure 
FUNCTION setCreateStatusBar RETURNS LOGICAL
  ( INPUT ibCreateStatusBar AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCustomMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCustomMenu Procedure 
FUNCTION setCustomMenu RETURNS LOGICAL
  ( INPUT icTabGroupTitle AS CHAR,
    INPUT icProgramList   AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setIsChildWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setIsChildWindow Procedure 
FUNCTION setIsChildWindow RETURNS LOGICAL
  ( INPUT ibIsChildWindow AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMenuBgImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuBgImage Procedure 
FUNCTION setMenuBgImage RETURNS LOGICAL
  ( INPUT icBgImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setParentHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParentHandle Procedure 
FUNCTION setParentHandle RETURNS LOGICAL
  (INPUT ihParent AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setProgramMultiInstanceList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setProgramMultiInstanceList Procedure 
FUNCTION setProgramMultiInstanceList RETURNS LOGICAL
  ( INPUT icProgramMultiInstanceList AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRibbonIconImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRibbonIconImage Procedure 
FUNCTION setRibbonIconImage RETURNS LOGICAL
  ( INPUT icImageName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRibbonMinimized) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRibbonMinimized Procedure 
FUNCTION setRibbonMinimized RETURNS LOGICAL
  ( INPUT ibRibbonMinimized AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setStartPanelURL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setStartPanelURL Procedure 
FUNCTION setStartPanelURL RETURNS LOGICAL
  ( INPUT icPanelURL AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setStatusText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setStatusText Procedure 
FUNCTION setStatusText RETURNS LOGICAL
  ( INPUT icStatusText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTimerProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTimerProperties Procedure 
FUNCTION setTimerProperties RETURNS LOGICAL
  ( INPUT iiTimerInterval   AS INT,
    INPUT icTimerContext    AS CHAR,
    INPUT icTimerServerProc AS CHAR,
    INPUT icTimerClientProc AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWindowTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWindowTitle Procedure 
FUNCTION setWindowTitle RETURNS LOGICAL
  ( INPUT icWinTitle AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWinMenuGroupLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWinMenuGroupLabel Procedure 
FUNCTION setWinMenuGroupLabel RETURNS LOGICAL
  ( INPUT icWinMenuGrpLabel AS CHAR )  FORWARD.

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
         HEIGHT             = 31.67
         WIDTH              = 61.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

hParent = SOURCE-PROCEDURE.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  RUN InitializeObject(0).
&ENDIF

ON 'CLOSE':U OF THIS-PROCEDURE DO:
  RUN InvalidateHandle(?).
  DELETE OBJECT goJBoxMainForm NO-ERROR.
  DELETE PROCEDURE THIS-PROCEDURE NO-ERROR.
  RETURN.
END.


/* ON "alt-s" ANYWHERE PERSISTENT RUN AltKeyFocus IN THIS-PROCEDURE.  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ActivateWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActivateWindow Procedure 
PROCEDURE ActivateWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiNodeKey AS INT NO-UNDO.

DEF VAR hMenu          AS HANDLE NO-UNDO.
DEF VAR cDummy         AS CHAR   NO-UNDO.
DEF VAR cRibbonTabType AS CHAR   NO-UNDO INIT "Infragistics.Win.UltraWinToolbars.RibbonTab".
DEF VAR hFirstFrame    AS HANDLE NO-UNDO.

iCurrNodeKey = iiNodeKey.

DEF BUFFER ttProgram FOR ttProgram.

ClearActionRibbonGroup().
FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = iiNodeKey
     NO-ERROR.
IF AVAIL ttProgram AND VALID-HANDLE(ttProgram.hWinProc) THEN DO:
  PUBLISH "SuspendNamedTimer" ("RefreshStatusBarAndMenu",NOT ttProgram.bUseTimer).

  iLastMenuId = ttProgram.iJBoxMenuId.

  hMenu = ttProgram.hWinProc:CURRENT-WINDOW:MENU-BAR NO-ERROR.
  IF NOT VALID-HANDLE(hMenu) THEN
    hMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",ttProgram.hWinProc:CURRENT-WINDOW,"MENUBAR")) NO-ERROR.
  IF NOT VALID-HANDLE(hMenu) THEN
    hMenu = ttProgram.hTrigWidget.
  IF VALID-HANDLE(hMenu) THEN DO:
    vGrpWinMenu = goJBoxMainForm:AddRibbonGroup(vCurrRibbonTab,cRibbonTabType,ixNode,cWinMenuGrpLabel,OUTPUT cDummy).
    ixNode = ixNode + 1.
    BuildActionRibbonGroup(hMenu:FIRST-CHILD,vGrpWinMenu,"Infragistics.Win.UltraWinToolbars.RibbonGroup").
  END.
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"MoveToTop") THEN
    RUN MoveToTop IN ttProgram.hWinProc.
  ELSE DO:
    hFirstFrame = ttProgram.hWinProc:CURRENT-WINDOW:FIRST-CHILD NO-ERROR.
    IF VALID-HANDLE(hFirstFrame) THEN
      APPLY "ENTRY" TO hFirstFrame.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AltKeyFocus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AltKeyFocus Procedure 
PROCEDURE AltKeyFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  MESSAGE "alt-s" SKIP FOCUS:NAME SKIP FOCUS:TYPE
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApplyAccelerator) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyAccelerator Procedure 
PROCEDURE ApplyAccelerator :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icAccelerator AS CHAR NO-UNDO.

FIND FIRST ttProgram
     WHERE ttProgram.iNodeIndex > 1000
       AND ttProgram.cAccelerator = icAccelerator
NO-ERROR.
IF AVAIL ttProgram THEN DO:
  APPLY "CHOOSE" TO ttProgram.hTrigWidget.
  RETURN "NO-APPLY".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildMenu Procedure 
PROCEDURE BuildMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMenuId     AS INT NO-UNDO.
DEF INPUT PARAM ivParent     AS Progress.Lang.Object NO-UNDO.
DEF INPUT PARAM iiParentNode AS INT  NO-UNDO.
DEF INPUT PARAM icParentType AS CHAR NO-UNDO.

DEF VAR hQuery         AS HANDLE NO-UNDO.
DEF VAR hBuffer        AS HANDLE NO-UNDO.
DEF VAR hMenuObject    AS HANDLE NO-UNDO.
DEF VAR hBuffParamFld  AS HANDLE NO-UNDO.
DEF VAR bLarge         AS LOG    NO-UNDO.
DEF VAR bFirstInGroup  AS LOG    NO-UNDO.
DEF VAR cImage         AS CHAR   NO-UNDO.
DEF VAR cProgramLaunch AS CHAR   NO-UNDO.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE(httMenuBuffer).
hBuffParamFld = hBuffer:BUFFER-FIELD("cParameter") NO-ERROR.

hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH JBoxMenu WHERE JBoxMenu.iParentNodeIndex = " + STRING(iiParentNode) + " BY iNodeIndex").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF (hBuffer::cMenuType = "sub-menu" AND 
      LOOKUP(hBuffer::cMenuLabel,cHiddenSubMenuLabels,";") > 0) OR
     (hBuffer::cMenuType = "menu-item" AND cHiddenMenuItemParams NE "" AND VALID-HANDLE(hBuffParamFld) AND
      LOOKUP(hBuffParamFld:BUFFER-VALUE,cHiddenMenuItemParams,";") > 0) 
       THEN DO:
    hQuery:GET-NEXT().
    NEXT.
  END.     

  IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "menubar" THEN DO:      
    iCurrMenuStyle = 0.

    CREATE ttProgram.
    ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE     
           ttProgram.oRibbonObj = goJBoxMainForm:getToolbarHandle(OUTPUT ttProgram.cObjectType)
           ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
           ttProgram.cMenuType  = hBuffer::cMenuType
           .
    IF VALID-HANDLE(hBuffParamFld) THEN
      ttProgram.cParameter = hBuffParamFld:BUFFER-VALUE.

    RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                   ttProgram.oRibbonObj,
                   hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE,
                   ttProgram.cObjectType).


  END.
  ELSE IF hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "tv-nav-bar" THEN DO:      
    iCurrMenuStyle = 1.

    RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
                   ?,
                   hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE,
                   "").
  END.

  IF hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE BEGINS "JBoxTimer" THEN DO:
    iTimerInterval  = INT(hBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR OR iTimerInterval = 0 THEN iTimerInterval = 500000.
    cTimerServerProc = ENTRY(1,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
    IF cTimerServerProc = "" THEN cTimerServerProc = "jbserv_checkbroadcastmessage.p".
    IF NUM-ENTRIES(hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE) > 1 THEN
      cTimerClientProc = ENTRY(2,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE).
    ELSE cTimerClientProc = "JBoxGetBroadCast.w".
    
    hQuery:GET-NEXT().
    NEXT.
  END.

  CASE STRING(hBuffer:BUFFER-FIELD("cMenuType"):BUFFER-VALUE):
    WHEN "SUB-MENU" THEN DO:
        IF hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE NE "" THEN
          cImage = hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.
        ELSE IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
          cImage = hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.
        ELSE cImage = "".  
        
      IF iCurrMenuStyle > 0 THEN DO:
        CASE INTEGER(hBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE):
          WHEN 3 THEN DO:
            CREATE ttProgram.
            ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE * -1 
                   ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonTab(ttProgram.iNodeIndex,
                                                                      hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                      OUTPUT ttProgram.cObjectType)
                   ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ttProgram.cMenuType  = hBuffer::cMenuType
                   .
            IF vCurrRibbonTab = ? THEN vCurrRibbonTab = ttProgram.oRibbonObj.
            IF hBuffer:BUFFER-FIELD("bHasChildMenuItem"):BUFFER-VALUE OR hBuffer:BUFFER-FIELD("bHasChildSubMenu"):BUFFER-VALUE THEN DO:
              ASSIGN vObject     = ttProgram.oRibbonObj
                     cObjectType = ttProgram.cObjectType
                     .
              CREATE ttProgram.
              ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE     
                     ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonGroup(vObject,
                                                                          cObjectType,
                                                                          ttProgram.iNodeIndex,
                                                                          hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                          OUTPUT ttProgram.cObjectType)
                     ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                     ttProgram.cMenuType  = hBuffer::cMenuType
                     .
              IF VALID-HANDLE(hBuffParamFld) THEN
                ttProgram.cParameter = hBuffParamFld:BUFFER-VALUE.
            END.
          END.

          OTHERWISE DO:
            CREATE ttProgram.
            ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE
                   ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
                   ttProgram.cMenuType  = hBuffer::cMenuType
                   ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(ivParent,
                                                                                icParentType,
                                                                                ttProgram.iNodeIndex,
                                                                                hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                                cImage, /* Image */
                                                                                "", /* Accelerator */
                                                                                NO, /* Large */
                                                                                NO, /* First in group */
                                                                                OUTPUT ttProgram.cObjectType)
                   .
            IF VALID-HANDLE(hBuffParamFld) THEN
              ttProgram.cParameter = hBuffParamFld:BUFFER-VALUE.
          END.
        END CASE.
      END.
      ELSE DO:
        CREATE ttProgram.
        ASSIGN ttProgram.iNodeIndex = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE
               ttProgram.cMenuLabel = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
               ttProgram.cMenuType  = hBuffer::cMenuType
               ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(ivParent,
                                                                            icParentType,
                                                                            ttProgram.iNodeIndex,
                                                                            hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                            "", /* Image */
                                                                            "", /* Accelerator */
                                                                            NO, /* Large */
                                                                            NO, /* First in group */
                                                                            OUTPUT ttProgram.cObjectType)
               .
        IF VALID-HANDLE(hBuffParamFld) THEN
          ttProgram.cParameter = hBuffParamFld:BUFFER-VALUE.
      END.
      RUN BuildMenu (hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE,
               ttProgram.oRibbonObj,
               hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE,
               ttProgram.cObjectType).
    END.
    WHEN "MENU-ITEM" THEN DO:

      IF hBuffer::cLaunchType = "START-WINDOW" AND
         SEARCH(hBuffer::cLaunch) = ? AND SEARCH(SUBSTR(hBuffer::cLaunch,1,LENGTH(hBuffer::cLaunch) - 1) + "r") = ? THEN DO:
        cProgramLaunch = REPLACE(hBuffer::cLaunch,"/","\").
        cProgramLaunch = ENTRY(NUM-ENTRIES(cProgramLaunch,"\"),cProgramLaunch,"\").
        IF SEARCH(cProgramLaunch) = ? AND SEARCH(SUBSTR(cProgramLaunch,1,LENGTH(cProgramLaunch) - 1) + "r") = ? THEN cProgramLaunch = "".
      END.
      ELSE cProgramLaunch = hBuffer::cLaunch.
      
      IF NOT (bIsChildWindow AND hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE = "THIS-PROCEDURE") 
         AND NOT (hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE = "THIS-PROCEDURE" AND NOT CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE))
         AND (IF CAN-DO("START-WINDOW,PROCEDURE",hBuffer::cLaunchType) THEN cProgramLaunch NE "" ELSE TRUE)
         THEN DO:
        IF hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE NE "" THEN
          cImage = hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.
/*           cImage = ENTRY(NUM-ENTRIES(hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE,"\"),hBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE,"\"). */
        ELSE IF hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE NE "" THEN
          cImage = hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE.
/*           cImage = ENTRY(NUM-ENTRIES(hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE,"\"),hBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE,"\"). */
        ELSE cImage = "".  
        bLarge = cImage NE "".
  
        CREATE ttProgram.
        ASSIGN ttProgram.iNodeIndex  = hBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE
               ttProgram.cMenuType   = hBuffer::cMenuType
               ttProgram.oRibbonObj  = goJBoxMainForm:AddRibbonButtonTool(ivParent,icParentType,ttProgram.iNodeIndex,hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE,
                                                                         IF bLarge /* AND iCurrMenuStyle > 0 */ THEN cImage ELSE "",
  /*                                                                        "j0431631.png", */
                                                                         "", /* Acelerator */
                                                                         IF iCurrMenuStyle > 0 THEN bLarge ELSE NO, /* Large */
                                                                         bFirstInGroup, /* First in group */
                                                                         OUTPUT ttProgram.cObjectType)
               ttProgram.cProcName   = cProgramLaunch /* hBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE */
               ttProgram.cMenuLabel  = hBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE
               ttProgram.cLaunchType = hBuffer:BUFFER-FIELD("cLaunchType"):BUFFER-VALUE
               ttProgram.bEmbed      = NOT hBuffer:BUFFER-FIELD("bConfigurable"):BUFFER-VALUE
               ttProgram.iJBoxMenuId = hBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE
               bFirstInGroup         = NO 
               .
        IF VALID-HANDLE(hBuffParamFld) THEN
          ttProgram.cParameter = hBuffParamFld:BUFFER-VALUE.
      END.
    END.
    WHEN "RULE" THEN bFirstInGroup = YES.

  END CASE.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CancelFormClosing) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CancelFormClosing Procedure 
PROCEDURE CancelFormClosing :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibCancelFormClose AS LOG NO-UNDO.

bCancelFormClose = ibCancelFormClose.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChangeCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany Procedure 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iCurrCompany     AS INT    NO-UNDO.
DEF VAR iMenuChoice      AS INT    NO-UNDO.
DEF VAR iDummy           AS INT    NO-UNDO.
DEF VAR hDummyWin        AS HANDLE NO-UNDO.

FIND FIRST ttProgram WHERE ttProgram.hWinMenuItem = SELF NO-ERROR.

iCurrCompany = DYNAMIC-FUNCTION("getCompanyId").

DYNAMIC-FUNCTION("setMenuRestart",YES).
RUN JBoxDummyWin.w PERSIST SET hDummyWin.
RUN JBoxDSelectCompany.w (TRUE, INPUT-OUTPUT iDummy).
DELETE PROCEDURE hDummyWin.
DYNAMIC-FUNCTION("setMenuRestart",NO).

IF DYNAMIC-FUNCTION("getCompanyId") NE iCurrCompany AND
   CAN-DO(hParent:INTERNAL-ENTRIES,"RestartMenu")
   THEN DO:
  RUN SelectRestartMenu (OUTPUT iMenuChoice).
  RUN CloseMenu.
  RUN RestartMenu IN hParent (iMenuChoice).
  RUN InvalidateHandle IN hParent (THIS-PROCEDURE).
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
DEF VAR cCurrTimerEvent AS CHAR NO-UNDO.
DEF VAR bViewBtnEvents  AS LOG  NO-UNDO.

IF cTimerServerProc NE "" THEN DO:
  DYNAMIC-FUNCTION("setShowHourGlass",FALSE).
  IF DYNAMIC-FUNCTION("runproc",cTimerServerProc,cTimerContext,?) THEN DO:
    cCurrTimerEvent = DYNAMIC-FUNCTION("getTransactionMessage").
    IF cCurrTimerEvent NE cLastTimerEvent THEN DO:
      IF cTimerClientProc NE "" THEN DO:
        IF cTimerClientProc = "setViewBtnEvents" THEN DO:
          bViewBtnEvents = LOGICAL(cCurrTimerEvent) NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN
            RUN VALUE(cTimerClientProc) (bViewBtnEvents).
        END.
        ELSE RUN VALUE(cTimerClientProc) (cCurrTimerEvent).
      END.
      cLastTimerEvent = cCurrTimerEvent.
    END.
  END.
  DYNAMIC-FUNCTION("setShowHourGlass",TRUE).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseAndExit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseAndExit Procedure 
PROCEDURE CloseAndExit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
APPLY "close" TO THIS-PROCEDURE.
RUN InvalidateHandle IN hParent (?) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseChildWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseChildWindow Procedure 
PROCEDURE CloseChildWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiNodeKey AS INT  NO-UNDO.
DEF OUTPUT PARAM ocReturn  AS CHAR NO-UNDO.


FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = iiNodeKey
     NO-ERROR.
IF AVAIL ttProgram THEN DO: 
  IF VALID-HANDLE(ttProgram.hWinProc) THEN 
    APPLY "CLOSE" TO ttProgram.hWinProc.
  ocReturn = RETURN-VALUE.
  IF ocReturn NE "cancel" AND NOT bCancelFormClose THEN DO:
    IF AVAIL ttProgram THEN DO:
/*       oChildForm = CAST(ttProgram.oChildForm,"Progress.Windows.Form") NO-ERROR. */
/*       IF NOT ERROR-STATUS:ERROR THEN                                            */
/*         oChildForm:CLOSE().                                                     */
/*       IF VALID-OBJECT(ttProgram.oChildForm) THEN                           */
/*         DELETE OBJECT CAST(ttProgram.oChildForm,"Progress.Windows.Form").  */
      ttProgram.oChildForm = ?.
    END.
    ClearActionRibbonGroup().
  END.
  ELSE ocReturn = "cancel".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseMenu Procedure 
PROCEDURE CloseMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttProgram:
  IF VALID-HANDLE(ttProgram.hWinProc) THEN DO:
    APPLY "close" TO ttProgram.hWinProc.
    IF AVAIL ttProgram AND VALID-HANDLE(ttProgram.hWinProc) THEN 
      RETURN.
  END.
END.
DELETE OBJECT httMenuBuffer NO-ERROR.
DELETE OBJECT httMenu NO-ERROR.

bDontQuit = YES. /* DELETE OBJECT causes a FormClosed event... */
DELETE OBJECT goJBoxMainForm.
bDontQuit = NO.

/*PUBLISH "InvalidateHandle" (THIS-PROCEDURE).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseTabs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseTabs Procedure 
PROCEDURE CloseTabs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihExcept        AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obCloseComplete AS LOG NO-UNDO INIT YES.

FOR EACH ttProgram WHERE ttProgram.iNodeIndex < 30000 AND ttProgram.hWinProc NE ihExcept:
  IF VALID-HANDLE(ttProgram.hWinProc) THEN DO:
    APPLY "close" TO ttProgram.hWinProc.
    IF AVAIL ttProgram AND VALID-HANDLE(ttProgram.hWinProc) THEN DO:
      obCloseComplete = NO.
      RETURN.
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseWindow Procedure 
PROCEDURE CloseWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bCloseComplete AS LOG NO-UNDO.
IF DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = STRING(THIS-PROCEDURE) OR DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = "" THEN DO:
  RUN CloseTabs(?,OUTPUT bCloseComplete).
  IF NOT bCloseComplete THEN RETURN.
  saveQuickAccess().    
  APPLY "close" TO THIS-PROCEDURE.
/*   QUIT. */
END.
ELSE RUN CloseMenu.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateMenuTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateMenuTT Procedure 
PROCEDURE CreateMenuTT :
/*------------------------------------------------------------------------------
 Purpose: To enable the menu to be loaded from a temp-table (or file/xml..)
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cJBoxMenuFlds AS CHAR NO-UNDO 
    INIT "cMenuType;character,cMenuLabel;character,iJBoxMenuId;integer,iJBoxProgramId;integer,dCreated;date,cCreatedBy;character,dModified;date,cModifiedBy;character,cLaunch;character,cAccelerator;character,cLaunchType;character,bConfigurable;logical,cImage;character,cSelImage;character,cStateImage;character,cTextColor;character,cFontStyle;character,cMenuTooltip;character,cMenuNumber;character,iTvNavBarStyle;integer,iImageSize;integer,iSelImageSize;integer,iStateImageSize;integer,cParameter;character,bLimitToCompanyAdmins;logical,bLimitToSuperUsers;logical,bDefault;logical"
         .
         
CREATE TEMP-TABLE httMenu.

DO ix = 1 TO NUM-ENTRIES(cJBoxMenuFlds):
  httMenu:ADD-NEW-FIELD(ENTRY(1,ENTRY(ix,cJBoxMenuFlds),";"),ENTRY(2,ENTRY(ix,cJBoxMenuFlds),";")).
END.
httMenu:ADD-NEW-FIELD("iParentMenuId","INTEGER").
httMenu:ADD-NEW-FIELD("iRootMenuId","INTEGER").
httMenu:ADD-NEW-FIELD("iLevel","INTEGER").
httMenu:ADD-NEW-FIELD("iSeq","INTEGER").
httMenu:ADD-NEW-FIELD("iNodeIndex","INTEGER").
httMenu:ADD-NEW-FIELD("iColourCode","INTEGER").
httMenu:ADD-NEW-FIELD("RowIdent1","CHARACTER").
httMenu:ADD-NEW-FIELD("bDisabled","LOGICAL").
httMenu:ADD-NEW-FIELD("hMenuItem","HANDLE").
httMenu:ADD-NEW-FIELD("bMultiple","LOGICAL").
httMenu:ADD-NEW-FIELD("cParentMenuType","CHARACTER").
httMenu:ADD-NEW-FIELD("bHasChildSubMenu","LOGICAL").
httMenu:ADD-NEW-FIELD("cParentImage","CHARACTER").
httMenu:ADD-NEW-FIELD("iParentNodeIndex","INTEGER").
httMenu:ADD-NEW-FIELD("bHasChildMenuItem","LOGICAL").

httMenu:TEMP-TABLE-PREPARE("JBoxMenu").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmbedInChildForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmbedInChildForm Procedure 
PROCEDURE EmbedInChildForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAM ihProc  AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAM obEmbed AS LOG NO-UNDO.

/* IF ttProgram.hWinProc = ? THEN */
/*   obEmbed = ttProgram.bEmbed. */

ASSIGN obEmbed       = bEmbeddedForm
       bEmbeddedForm = NO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndJBoxEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndJBoxEvent Procedure 
PROCEDURE EndJBoxEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihCurrObject     AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCurrSourceProc AS HANDLE NO-UNDO.
DEF INPUT PARAM ihCurrWidget     AS HANDLE NO-UNDO.
DEF INPUT PARAM icCurrMethod     AS CHAR   NO-UNDO.

DEF VAR hContainer AS HANDLE NO-UNDO.

IF CAN-DO("deleteRecord,defaultActionBrowse,startSearch,JboxWindowClose,browseColumnLookup",icCurrMethod) THEN DO:
/* IF CAN-DO("deleteRecord,lookup",icCurrMethod) THEN DO: */
  DEF BUFFER ttProgram FOR ttProgram.
    
  hContainer = DYNAMIC-FUNCTION("getContainerHandle",ihCurrSourceProc).

  FIND FIRST ttProgram
       WHERE ttProgram.hWinProc = hContainer
       NO-ERROR.

  IF AVAIL ttProgram AND ttProgram.oChildForm NE ? THEN DO:
    oChildForm = CAST(ttProgram.oChildForm,"Progress.Windows.Form").
    oChildForm:BringToFront().
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EventHandler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EventHandler Procedure 
PROCEDURE EventHandler :
/*------------------------------------------------------------------------------
  Purpose: Handles all events from the form                                                                                                                                       
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icEventName AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER poSender    AS System.Object  NO-UNDO.
DEFINE INPUT PARAMETER poArgs      AS System.EventArgs NO-UNDO.

DEF VAR cRetVal         AS CHAR NO-UNDO.
DEF VAR hDummy          AS HANDLE NO-UNDO.

CASE icEventName:
  /* pass menu item stuff through */
  WHEN 'ToolClick' THEN DO:
    IF TYPE-OF(poSender, UltraToolbarsManager) THEN DO:
      FIND FIRST ttProgram 
           WHERE ttProgram.iNodeIndex = INT(cast(poArgs, ToolClickEventArgs):Tool:KEY)
           NO-ERROR.
      IF AVAIL ttProgram THEN DO: 
        IF ttProgram.oChildForm NE ? THEN DO:
          IF CAN-DO(cProgramMultiInstanceList,ttProgram.cProcName) THEN
            RUN StartTabWindow (ttProgram.cProcName,ttProgram.cMenuLabel,?,YES,NO,OUTPUT hDummy).
          ELSE DO:            
            iLastMenuId = ttProgram.iJBoxMenuId.
            CAST(ttProgram.oChildForm,"Progress.Windows.MDIChildForm"):BringToFront().
          END.  
        END.
        ELSE IF VALID-HANDLE(ttProgram.hTrigWidget) THEN DO:
          IF ttProgram.hTrigWidget:TOGGLE-BOX THEN DO:
            IF NOT PROGRAM-NAME(3) BEGINS "setStatusMenuItem" THEN DO:
              ttProgram.hTrigWidget:CHECKED = NOT ttProgram.hTrigWidget:CHECKED.
              APPLY "VALUE-CHANGED" TO ttProgram.hTrigWidget.
            END.
          END.
          ELSE
            APPLY "CHOOSE" TO ttProgram.hTrigWidget.
          RUN SetWinMenuActionState.
        END.
        ELSE DO:
          ttProgram.hParent = ?. 
          RUN StartWindow (cast(poArgs, ToolClickEventArgs):Tool:KEY).
        END.  
      END.
    END.
  END.    
  WHEN 'FormClosed' THEN DO:
  
    IF NOT bDontQuit AND (DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = STRING(THIS-PROCEDURE) OR DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle") = "") THEN DO:
      saveQuickAccess() NO-ERROR.
      APPLY "close" TO THIS-PROCEDURE.
      RUN InvalidateHandle IN hParent (?) NO-ERROR.
      /* In effect:  QUIT. */
      IF ERROR-STATUS:ERROR THEN QUIT.
    END.
  END.
    
  WHEN 'FormClosing' THEN DO:
    bCancelFormClose = NO. 
    RUN CloseChildWindow (INT(STRING(CAST(poSender, Form):Tag)),OUTPUT cRetVal).
  END. 
   
  WHEN 'FormActivated' THEN 
    RUN ActivateWindow (INT(STRING(CAST(poSender, Form):Tag))).
  WHEN 'TabSelected' THEN 
    RUN SetCurrTab (cast(poArgs, RibbonTabEventArgs):TAB:KEY).
  WHEN 'TabInitialized' THEN
    oLastMdiTab = cast(poArgs,Infragistics.Win.UltraWinTabbedMdi.MdiTabEventArgs):Tab.

END CASE.

RETURN cRetVal.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsProcedureActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getIsProcedureActive Procedure 
PROCEDURE getIsProcedureActive :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icProcName AS CHAR NO-UNDO.
DEF OUTPUT PARAM obRunning  AS LOG  NO-UNDO. 

DEF BUFFER bttProgram FOR ttProgram.

FOR EACH bttProgram 
    WHERE bttProgram.cProcName = icProcName:
  IF VALID-HANDLE(bttProgram.hWindow) THEN obRunning = YES.
END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMainFormObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMainFormObject Procedure 
PROCEDURE getMainFormObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ooMainForm AS JBoxMainForm NO-UNDO.

ooMainForm = goJBoxMainForm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowContainerHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getWindowContainerHandle Procedure 
PROCEDURE getWindowContainerHandle :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihWindow AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ohProc   AS HANDLE NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.hWindow = ihWindow
     NO-ERROR.
IF AVAIL ttProgram THEN ohProc = ttProgram.hWinProc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowContainerObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getWindowContainerObject Procedure 
PROCEDURE getWindowContainerObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihWinProc     AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ohContainer   AS JBoxContainer NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.hWinProc = ihWinProc
     NO-ERROR.
IF AVAIL ttProgram THEN ohContainer = CAST(ttProgram.oContainer,"JBoxContainer").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Procedure 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMenuId    AS INT NO-UNDO.

DEF VAR iCompanyId       AS INT    NO-UNDO.
DEF VAR cReadOnlyUsers   AS CHAR   NO-UNDO.
DEF VAR cReadOnlyActions AS CHAR   NO-UNDO.
DEF VAR cImageList       AS CHAR   NO-UNDO.
DEF VAR cLargeImageList  AS CHAR   NO-UNDO.
DEF VAR cGoToList        AS CHAR   NO-UNDO.
DEF VAR hPHnavBarOptions AS HANDLE NO-UNDO.
DEF VAR iNodeIdx         AS INT    NO-UNDO.
DEF VAR cLogoImage       AS CHAR   NO-UNDO.
DEF VAR cCompanyList     AS CHAR   NO-UNDO.
DEF VAR cUserCompanyList AS CHAR   NO-UNDO.
DEF VAR iDummy           AS INT    NO-UNDO.
DEF VAR cAppImage        AS CHAR   NO-UNDO.
DEF VAR cPanelFile       AS CHAR   NO-UNDO.
DEF VAR hDummyWin        AS HANDLE NO-UNDO.
DEF VAR cQuickAccesList  AS CHAR   NO-UNDO.
DEF VAR cQuickAccesNodes AS CHAR   NO-UNDO.
DEF VAR cDummy           AS CHAR   NO-UNDO.
DEF VAR bSelectCompanyOrRole AS LOG NO-UNDO. /* Select company or role */
DEF VAR bSuper           AS LOG    NO-UNDO.

goJBoxMainForm = NEW JBoxMainForm(THIS-PROCEDURE).  
goJBoxMainForm:ClientSize = NEW System.Drawing.Size(iClientXsize,iClientYsize).
goJBoxMainForm:setRibbonMinimized (bRibbonMinimized).
/* goJBoxMainForm:setWindowTitle("OVF - Eiendom og kontraktsystem").  */
/* goJBoxMainForm:setBgImage("bg-green.bmp").                         */
/* goJBoxMainForm:setIconImage("RUN.bmp").                            */
/* goJBoxMainForm:setStatusText("Brynjar").                           */
/* goJBoxMainForm:setRibbonIconImage("j0433808.png").                 */
/* goJBoxMainForm:setAppStyleSheet("vs2008_test.isl").                */
/* goJBoxMainForm:setAppStyleSheet("JukeBoxGray.isl").  */

ASSIGN iCompanyId = DYNAMIC-FUNCTION("getCompanyId")
       cCompanyList = DYNAMIC-FUNCTION("getFieldList","JBoxCompany;iJBoxCompanyId","WHERE iJBoxCompanyId > 0") /* are there companies (or roles) in the database? */
       cUserCompanyList = DYNAMIC-FUNCTION("getFieldList","JBoxCompanyUser;iJBoxCompanyId","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "' BY iJBoxCompanyId")
       bSuper = (IF DYNAMIC-FUNCTION("getIsTableInstalled","JBoxUser") THEN LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","bSuperUser")) ELSE NO)
       cHiddenSubMenuLabels = DYNAMIC-FUNCTION("getAttribute",SESSION,"HiddenSubMenuLabels")
       cHiddenMenuItemParams = DYNAMIC-FUNCTION("getAttribute",SESSION,"HiddenMenuItemParams")
       .

IF VALID-HANDLE(httMenu) THEN
  bSelectCompanyOrRole = NO.
ELSE IF DYNAMIC-FUNCTION("getAttribute",SESSION,"CompanyIsRole") = "yes" THEN DO:
  IF NOT bSuper THEN
    bSelectCompanyOrRole = NUM-ENTRIES(cUserCompanyList,"|") > 1.
END.
ELSE IF iCompanyId = 0 OR DYNAMIC-FUNCTION("getAttribute",SESSION,"SelectCompanyOnStartup") = "yes" THEN DO:
  IF NUM-ENTRIES(cCompanyList,"|") = 1 THEN
    DYNAMIC-FUNCTION("setCompanyId",INT(cCompanyList)).
  ELSE IF NOT bSuper THEN DO:
    IF cUserCompanyList NE "" THEN DO:
      IF NUM-ENTRIES(cUserCompanyList,"|") = 1 THEN   
        DYNAMIC-FUNCTION("setCompanyId",INT(cUserCompanyList)).
      ELSE  
        bSelectCompanyOrRole = YES.
    END.
    ELSE DO:
      MESSAGE "Company/client access not assigned to user"
              VIEW-AS ALERT-BOX.
      QUIT.  
    END.    
  END.      
  ELSE bSelectCompanyOrRole = YES. 
END.  
            
IF bSelectCompanyOrRole THEN DO: 
  RUN JBoxDummyWin.w PERSIST SET hDummyWin.
  RUN JBoxDSelectCompany.w PERSIST (TRUE,INPUT-OUTPUT iDummy).
  DELETE PROCEDURE hDummyWin.
END.

IF iiMenuId = 0 THEN DO:
  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iiMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
  ELSE 
    iiMenuId = INT(DYNAMIC-FUNCTION("getFieldValues","JBoxMenu","WHERE cMenuType = 'menu'","iJBoxMenuId")).
END.

IF iiMenuId = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"No menu definitions found in database" + CHR(10)
                                 + DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  QUIT.
END.

/* Normally the session object is already created here.. */
DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").

SUBSCRIBE TO "SetWinMenuActionState" ANYWHERE.
SUBSCRIBE TO "RebuildActionMenu"    ANYWHERE.

SUBSCRIBE TO "RefreshMenu"          ANYWHERE.
SUBSCRIBE TO "getPlaceholderHandle" ANYWHERE.
SUBSCRIBE TO "getMenuImage"         ANYWHERE.
SUBSCRIBE TO "InvalidateHandle"     ANYWHERE.
SUBSCRIBE TO "ApplyMenu"            ANYWHERE.
SUBSCRIBE TO "setCompanyLogo"       ANYWHERE.
SUBSCRIBE TO "CancelFormClosing"    ANYWHERE.
SUBSCRIBE TO "CloseTabs"            ANYWHERE.
SUBSCRIBE TO "getWindowContainerHandle" ANYWHERE.
SUBSCRIBE TO "getWindowContainerObject" ANYWHERE.
SUBSCRIBE TO "setWindowContainerObject" ANYWHERE.
SUBSCRIBE TO "getIsProcedureActive" ANYWHERE.
  
hMainMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle")) NO-ERROR.
/* If there is only one menu instance there is no need to set the mainmenuhandle attribute.
   Otherwise only the main menu will be the only place to start child windows: */
IF NOT VALID-HANDLE(hMainMenu) OR hMainMenu = THIS-PROCEDURE THEN DO:
  SUBSCRIBE TO "UserNotification"    ANYWHERE.
  SUBSCRIBE TO "setViewBtnEvents"    ANYWHERE.
  SUBSCRIBE TO "setMenuPanelURL"     ANYWHERE.
  SUBSCRIBE TO "setEventMsgProg"     ANYWHERE.
  SUBSCRIBE TO "StartChildWindow"    ANYWHERE.
  SUBSCRIBE TO "StartTabWindow"      ANYWHERE.
  SUBSCRIBE TO "StartChildTab"       ANYWHERE.
  SUBSCRIBE TO "SizeToFitTab"        ANYWHERE.
  SUBSCRIBE TO "ViewTabProcHandle"   ANYWHERE.
  SUBSCRIBE TO "NewCustomMenuWindow" ANYWHERE.
  SUBSCRIBE TO "RestartMenu"         ANYWHERE.
  SUBSCRIBE TO "getMainFormObject"   ANYWHERE.
END.
  
SUBSCRIBE TO "EmbedInChildForm" ANYWHERE.

IF NOT VALID-HANDLE(httMenu) THEN DO:
  IF iiMenuId = -1 THEN  /* custom */
    httMenu = DYNAMIC-FUNCTION("getTempTable","jbsetup_getcustom_menu.p",
                                 "|" + (IF DYNAMIC-FUNCTION("getLanguageCode") NE DYNAMIC-FUNCTION("getBaseLanguageCode") THEN
                                           DYNAMIC-FUNCTION("getLanguageCode")
                                         ELSE "")
                               + "|no|" + DYNAMIC-FUNCTION("getAttribute",SESSION,"RestrictedUnlessAllowed")
                               + "|" + cCustomTabGroupTitle + "|" + cCustomProgramList,?).
  
  ELSE DO:
    iMainMenuId = iiMenuId.
    
    httMenu = DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                                ,"WHERE iJBoxMenuId = " + STRING(iMainMenuId)
                               + "|" + (IF DYNAMIC-FUNCTION("getLanguageCode") NE DYNAMIC-FUNCTION("getBaseLanguageCode") THEN
                                          DYNAMIC-FUNCTION("getLanguageCode")
                                        ELSE "")
                               + "|no|" + DYNAMIC-FUNCTION("getAttribute",SESSION,"RestrictedUnlessAllowed")
                                ,?).
  END.
END.  

httMenuBuffer = httMenu:DEFAULT-BUFFER-HANDLE.
hParameterField = httMenuBuffer:BUFFER-FIELD("cParameter") NO-ERROR.
hMultiple = httMenuBuffer:BUFFER-FIELD("bMultiple") NO-ERROR.

/* DYNAMIC-FUNCTION("toExcelViaFile",httMenuBuffer,0).  */

bOk = httMenuBuffer:FIND-FIRST("WHERE cMenuType = 'menu'") NO-ERROR.
IF bOk THEN DO:
  
  IF iiMenuId = -1 THEN iMainMenuId = httMenuBuffer::iJBoxMenuId. /* if this is a custom menu (f.ex from workflow) */
  
  IF SEARCH(httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE) NE ? THEN DO:
    cAppImage = httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.
  END.
  IF cAppImage NE "" THEN
    goJBoxMainForm:setRibbonIconImage(cAppImage).

  IF httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE NE "" AND SEARCH(httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE) NE ? THEN
    cLogoImage = httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE.

 ASSIGN
    cPanelURL    = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE cCodeType = 'URLs' AND cCodeValue = 'PanelURL' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompanyId") ,"cMisc1") 
    cPanelURL    = (if cPanelURL = "" OR cPanelURL = ? THEN httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE ELSE cPanelURL)
    cPanelFile   = (IF cPanelURL NE "" THEN cPanelURL ELSE cPanelFile)
    iNodeIdx     = httMenuBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE
    .

/*
  ASSIGN cPanelURL    = httMenuBuffer:BUFFER-FIELD("cLaunch"):BUFFER-VALUE
         cPanelFile   = (IF cPanelURL NE "" THEN cPanelURL ELSE cPanelFile)
         iNodeIdx     = httMenuBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE 
         .
*/         

  /* New window QAT btn: */
/*   goJBoxMainForm:AddRibbonButtonTool(?,"ultraToolbarsManager1:Ribbon:QuickAccessToolbar",2000,"Nytt vindu","fo.bmp","",NO,NO,OUTPUT cDummy).  */

  cQuickAccesList = DYNAMIC-FUNCTION("getFieldList","JBoxMenuFavorites;iJBoxMenuId","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "' AND cType = 'QAT'").
  DO ix = 1 TO NUM-ENTRIES(cQuickAccesList,"|"):
    bOk = httMenuBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + ENTRY(ix,cQuickAccesList,"|")) NO-ERROR.
    IF bOk THEN
      cQuickAccesNodes = cQuickAccesNodes + (IF cQuickAccesNodes NE "" THEN "," ELSE "") + STRING(httMenuBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
  END.
  
  cInitQuickAccess = REPLACE(DYNAMIC-FUNCTION("getAttribute",SESSION,"InitQuickAccessMenu"),";","|").
  
  DO ix = 1 TO NUM-ENTRIES(cInitQuickAccess,"|"):
    bOk = httMenuBuffer:FIND-FIRST("WHERE cMenuLabel = '" + ENTRY(ix,cInitQuickAccess + "'","|")) NO-ERROR.
    IF bOk THEN
      cQuickAccesNodes = cQuickAccesNodes + (IF cQuickAccesNodes NE "" THEN "," ELSE "") + STRING(httMenuBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE).
  END.  
  goJBoxMainForm:cQuickAccessNodes = cQuickAccesNodes.

END.
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Error in menu definition - no root (menu) node defined","","").
  QUIT.    
END.

RUN BuildMenu (iMainMenuId,?,iNodeIdx,"").

/* RUN getPlaceholderHandle("WinMenu",OUTPUT hWinMenu). */

/*   IF cLogoImage NE "" THEN                                  */
/*     DYNAMIC-FUNCTION("setNavBarImage" IN hTree,cLogoImage). */

/* {&WINDOW-NAME}:TITLE = DYNAMIC-FUNCTION("getAppTitle"). */

  DYNAMIC-FUNCTION("getFunctionRestrictions").    
/*   DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW). */

IF DYNAMIC-FUNCTION("getIsTableInstalled","JBoxUser") THEN DO:
  IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","bSuperUser")) THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","super").
  ELSE DO:
    bOk = LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxCompanyUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'" +
                                                                      "  AND iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")),"bSuperUserCompany")) NO-ERROR.
    IF bOk THEN
      DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","companysuper").
    ELSE DO:
  
      cReadOnlyUsers   = REPLACE(DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'ro-users'"),"|",",").
      cReadOnlyActions = REPLACE(DYNAMIC-FUNCTION("getFieldList","JBoxGenCode;cCodeValue","WHERE cCodeType = 'ro-actions'"),"|",",").
      IF cReadOnlyActions = "" THEN cReadOnlyActions = "new,edit,copy,delete,undo,save".
    
      IF CAN-DO(cReadOnlyUsers,DYNAMIC-FUNCTION("getASuserId")) THEN
        DYNAMIC-FUNCTION("setSecDisabledActions",cReadOnlyActions,"").
    END.
  END.
END.  

/* DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW). */

/* If the menu contains an entry for message publication (requires JukeBox messaging installed) 
   start the timer here: */

IF (NOT VALID-HANDLE(hMainMenu) OR hMainMenu = THIS-PROCEDURE) AND iTimerInterval > 0 THEN DO:
  RUN StartTimer.
  IF iTimerInterval > 1000 THEN
    RUN CheckTimerEvent.
END.

PUBLISH "setProgBarProperty" ("text"," ").
PUBLISH "setProgBarProperty" ("position","0").

goJBoxMainForm:Show().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InvalidateHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle Procedure 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWinProc AS HANDLE NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FOR EACH ttProgram
    WHERE (IF ihWinProc NE ? THEN ttProgram.hParent = ihWinProc ELSE TRUE):
  IF VALID-HANDLE(ttProgram.hWinProc) THEN 
    APPLY "close" TO ttProgram.hWinProc.
  IF AVAIL ttProgram THEN DO:
    IF ttProgram.oChildForm NE ? THEN DO:
      oChildForm = CAST(ttProgram.oChildForm,"Progress.Windows.Form") NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        oChildForm:CLOSE().
    END.
    DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.
    DELETE ttProgram.
  END.
END.

FIND FIRST ttProgram
     WHERE ttProgram.hWinProc = ihWinProc
     NO-ERROR.
IF AVAIL ttProgram THEN DO:
  IF ttProgram.oChildForm NE ? THEN DO:
    oChildForm = CAST(ttProgram.oChildForm,"Progress.Windows.Form") NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN 
      oChildForm:CLOSE().
  END.
  DELETE OBJECT ttProgram.hWinMenuItem NO-ERROR.

  IF ttProgram.bChildWin THEN DELETE ttProgram.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadFromXmlNodeTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFromXmlNodeTable Procedure 
PROCEDURE LoadFromXmlNodeTable :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihbttNodes AS HANDLE NO-UNDO.

DEF VAR hQuery  AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.

/*run toexcelviafile.p (ihbttNodes,0).*/

IF NOT VALID-HANDLE(httMenu) THEN RUN CreateMenuTT.

hBuffer = httMenu:DEFAULT-BUFFER-HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS (ihbttNodes).
hQuery:QUERY-PREPARE("FOR EACH " + ihbttNodes:NAME + " BY iNodeIdx").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
ix = 1.
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF ihbttNodes::cViewType = "block" THEN DO:
    hBuffer:BUFFER-CREATE().
    ASSIGN hBuffer::iJBoxMenuId      = ihbttNodes::iNodeIdx
           hBuffer::iNodeIndex       = ihbttNodes::iNodeIdx
           hBuffer::iParentMenuId    = ihbttNodes::iParentNodeIdx 
           hBuffer::iParentNodeIndex = ihbttNodes::iParentNodeIdx 
           hBuffer::cMenuType        = ihbttNodes::cElement
           hBuffer::iLevel           = ihbttNodes::iLevel + 1
           hBuffer::bHasChildSubMenu = YES
           ix = ix + 1
           .
  END.
  ELSE hBuffer:BUFFER-FIELD(ihbttNodes::cElement):BUFFER-VALUE = ihbttNodes::cNodeValue.
   
  hQuery:GET-NEXT ().
END.  

/*run toexcelviafile.p (hBuffer,0).*/

DELETE OBJECT hQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogProgramExecution) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LogProgramExecution Procedure 
PROCEDURE LogProgramExecution :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH("log-manager-report.p") = ? AND SEARCH("log-manager-report.r") = ? THEN DO:
  MESSAGE "Log manager report program not installed"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN.
END.

bLogProgramExec = NOT bLogProgramExec.

MESSAGE "Logging of program execution is now " STRING(bLogProgramExec,"On/Off")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewCustomMenuWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewCustomMenuWindow Procedure 
PROCEDURE NewCustomMenuWindow :
/*------------------------------------------------------------------------------
 Purpose: Create a new menu window containing one ribbon for a collection of menu items          
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icMenuTitle     AS CHAR NO-UNDO.
DEF INPUT PARAM icTabGroupTitle AS CHAR NO-UNDO.
DEF INPUT PARAM icMenuItems     AS CHAR NO-UNDO.

CREATE ttProgram.
ttProgram.iNodeIndex = 60000.

RUN JBoxRibbonMenu.p PERSIST SET ttProgram.hWinProc.

DYNAMIC-FUNCTION("setIsChildWindow" IN ttProgram.hWinProc,YES).

IF iClientXsize > 0 THEN
  DYNAMIC-FUNCTION("setClientSize" IN ttProgram.hWinProc,iClientXsize,iClientYsize).
    
DYNAMIC-FUNCTION("setRibbonMinimized" IN ttProgram.hWinProc,bRibbonMinimized).

DYNAMIC-FUNCTION("setCustomMenu" IN ttProgram.hWinProc,icTabGroupTitle,icMenuItems).

RUN InitializeObject IN ttProgram.hWinProc (-1).

DYNAMIC-FUNCTION("setCreateStatusBar" IN ttProgram.hWinProc,bCreateStatusBar).
DYNAMIC-FUNCTION("setAppStyleSheet" IN ttProgram.hWinProc,cStyleSheet).
DYNAMIC-FUNCTION("setWindowTitle" IN ttProgram.hWinProc,icMenuTitle).
DYNAMIC-FUNCTION("setStatusText" IN ttProgram.hWinProc,DYNAMIC-FUNCTION("getASuserName")).
DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN ttProgram.hWinProc,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewMenuWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewMenuWindow Procedure 
PROCEDURE NewMenuWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMenuId AS INT NO-UNDO.

CREATE ttProgram.
ttProgram.iNodeIndex = 30000.

RUN JBoxRibbonMenu.p PERSIST SET ttProgram.hWinProc.

DYNAMIC-FUNCTION("setIsChildWindow" IN ttProgram.hWinProc,YES).

IF iClientXsize > 0 THEN
  DYNAMIC-FUNCTION("setClientSize" IN ttProgram.hWinProc,iClientXsize,iClientYsize).
    
DYNAMIC-FUNCTION("setRibbonMinimized" IN ttProgram.hWinProc,bRibbonMinimized).

RUN InitializeObject IN ttProgram.hWinProc (IF iiMenuId = 0 THEN iMainMenuId ELSE iiMenuId).

DYNAMIC-FUNCTION("setCreateStatusBar" IN ttProgram.hWinProc,bCreateStatusBar).
DYNAMIC-FUNCTION("setAppStyleSheet" IN ttProgram.hWinProc,cStyleSheet).
DYNAMIC-FUNCTION("setWindowTitle" IN ttProgram.hWinProc,cWinTitle).
DYNAMIC-FUNCTION("setStatusText" IN ttProgram.hWinProc,DYNAMIC-FUNCTION("getASuserName")).
DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN ttProgram.hWinProc,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RebuildActionMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RebuildActionMenu Procedure 
PROCEDURE RebuildActionMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN ActivateWindow (iCurrNodeKey).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RestartMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RestartMenu Procedure 
PROCEDURE RestartMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SetRestart IN hParent (YES) NO-ERROR.
RUN CloseMenu.
RUN RestartMenu IN hParent (iMainMenuId).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectRestartMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectRestartMenu Procedure 
PROCEDURE SelectRestartMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM oiMenuChoice  AS INT NO-UNDO.

DEF VAR cCompanyMenu AS CHAR NO-UNDO.
DEF VAR cSuperMenu   AS CHAR NO-UNDO.
DEF VAR cReturn      AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getUserLevel") = "super" THEN DO:
  cCompanyMenu = DYNAMIC-FUNCTION("getFieldList",
                                  "JBoxCompanyMenu,JBoxMenu;cMenuLabel;iJBoxMenuId",
                                  "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                + ",FIRST JBoxMenu NO-LOCK OF JBoxCompanyMenu WHERE cMenuType = 'menu'"
                                  ).
  
  IF cCompanyMenu NE "" THEN DO:
    IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
      cSuperMenu = DYNAMIC-FUNCTION("getTransactionMessage").
    
    IF LOOKUP(cSuperMenu,cCompanyMenu,"|") = 0 THEN
      RUN JBoxDSimpleSelectList.w (cCompanyMenu + "|" 
                                 + DYNAMIC-FUNCTION("getFieldValues","JBoxMenu",
                                                    "WHERE iJBoxMenuId = " + cSuperMenu,"cMenuLabel,iJBoxMenuId")
                                  ,?
                                  ,OUTPUT cReturn).
    oiMenuChoice = INTEGER(cReturn).
  END.
END.
ELSE IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
  oiMenuChoice = INT(DYNAMIC-FUNCTION("getTransactionMessage")).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCurrTab) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCurrTab Procedure 
PROCEDURE SetCurrTab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiNodeKey AS INT NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = iiNodeKey
     NO-ERROR.
IF AVAIL ttProgram AND ttProgram.oRibbonObj NE ? THEN DO:
  vCurrRibbonTab = ttProgram.oRibbonObj.
/*    goJBoxMainForm:ActiveControl = ttProgram.oChildForm. */
/*   CAST(ttProgram.oChildForm:Activate().  */
  RUN ActivateWindow (iCurrNodeKey).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEventMsgProg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEventMsgProg Procedure 
PROCEDURE setEventMsgProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icEventMsgProc AS CHAR NO-UNDO.
DEF INPUT PARAM icLabel        AS CHAR NO-UNDO.

DEF BUFFER bttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.cProcName = icEventMsgProc
     NO-ERROR.
IF NOT AVAIL ttProgram THEN DO:
  FIND FIRST bttProgram 
       WHERE bttProgram.cMenuType = "menubar"
       NO-ERROR.
  IF AVAIL bttProgram THEN DO:
    CREATE ttProgram.
    ASSIGN ttProgram.iNodeIndex  = -1000
/*            ttProgram.oRibbonObj  = goJBoxMainForm:AddRibbonButtonTool(?,"ultraToolbarsManager1:Ribbon:QuickAccessToolbar",             */
/*                                                                      ttProgram.iNodeIndex,icLabel,                                     */
/*                                                                      IF SEARCH("bmp\dueledg.bmp") NE ? THEN "bmp\dueledg.bmp" ELSE "", */
/*                                                                      "", /* Acelerator */                                              */
/*                                                                      YES, /* Large */                                                  */
/*                                                                      NO, /* First in group */                                          */
/*                                                                      OUTPUT ttProgram.cObjectType)                                     */
           ttProgram.cProcName   = icEventMsgProc
           ttProgram.cMenuLabel  = icLabel
           ttProgram.cLaunchType = "start-window"
           ttProgram.bEmbed      = NO
           ttProgram.iJBoxMenuId = -1000
           ttProgram.cMenuType   = "menu-item"
           .
/*     IF VALID-HANDLE(hBuffParamFld) THEN                  */
/*       ttProgram.cParameter = hBuffParamFld:BUFFER-VALUE. */
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setViewBtnEvents) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setViewBtnEvents Procedure 
PROCEDURE setViewBtnEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibViewBtnAlarm AS LOG NO-UNDO.

FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = -1000
     NO-ERROR.

IF AVAIL ttProgram THEN DO:
  IF ibViewBtnAlarm AND ttProgram.oRibbonObj = ? THEN
    ttProgram.oRibbonObj  = goJBoxMainForm:AddRibbonButtonTool(?,"ultraToolbarsManager1:Ribbon:QuickAccessToolbar",
                                                              ttProgram.iNodeIndex,ttProgram.cMenuLabel,
                                                              IF SEARCH("bmp\dueledg.bmp") NE ? THEN "bmp\dueledg.bmp" ELSE "",
                                                              "", /* Acelerator */
                                                              YES, /* Large */
                                                              NO, /* First in group */
                                                              OUTPUT ttProgram.cObjectType)
                                                              .
  ELSE IF NOT ibViewBtnAlarm AND ttProgram.oRibbonObj NE ? THEN DO:
    goJBoxMainForm:RemoveRibbonButtonTool(ttProgram.oRibbonObj).
    ttProgram.oRibbonObj = ?.
  END.
END.

/*

IF NOT ibViewBtnAlarm THEN DO:
  goJBoxMainForm:DisableMenuItem (-1000).
/*   IF SEARCH("ico\alarm.ico") NE ? THEN                  */
/*     goJBoxMainForm:setRibbonIconImage("ico\alarm.ico"). */
END.
ELSE DO:
  goJBoxMainForm:EnableMenuItem (-1000).
/*   bOk = httMenuBuffer:FIND-FIRST("WHERE cMenuType = 'menu'") NO-ERROR.                */
/*   IF bOk AND SEARCH(httMenuBuffer:BUFFER-FIELD("cStateImage"):BUFFER-VALUE) NE ? THEN */
/*     goJBoxMainForm:setRibbonIconImage(httMenuBuffer::cStateImage).                    */
END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWindowContainerObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setWindowContainerObject Procedure 
PROCEDURE setWindowContainerObject :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWinProc   AS HANDLE        NO-UNDO.
DEF INPUT PARAM ioContainer AS JBoxContainer NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.hWinProc = ihWinProc
     NO-ERROR.
IF AVAIL ttProgram THEN ttProgram.oContainer = CAST(ioContainer,"Progress.Lang.Object").

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
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER ttProgram FOR ttProgram.
DEF VAR hCorrBtn AS HANDLE NO-UNDO.

FOR EACH ttProgram 
    WHERE ttProgram.iNodeIndex > 1000:
  IF VALID-HANDLE(ttProgram.hTrigWidget) THEN DO:
    IF ttProgram.hTrigWidget:PRIVATE-DATA NE "" THEN DO:
      hCorrBtn = WIDGET-HANDLE(ttProgram.hTrigWidget:PRIVATE-DATA) NO-ERROR.
      IF VALID-HANDLE(hCorrBtn) AND SEARCH(hCorrBtn:IMAGE) NE ? THEN   
        goJBoxMainForm:setMenuToolImage(ttProgram.iNodeIndex,hCorrBtn:IMAGE).
    END.    
      
    IF ttProgram.hTrigWidget:SENSITIVE THEN
      goJBoxMainForm:EnableMenuItem(ttProgram.iNodeIndex).
    ELSE
      goJBoxMainForm:DisableMenuItem(ttProgram.iNodeIndex).
    IF ttProgram.hTrigWidget:TOGGLE-BOX THEN
      goJBoxMainForm:SetStatusMenuItem(ttProgram.oRibbonObj,ttProgram.hTrigWidget:CHECKED).
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWinStartParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setWinStartParam Procedure 
PROCEDURE setWinStartParam :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icWinStartParam AS CHAR NO-UNDO.

cWinStartParam = icWinStartParam.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SizeToFitTab) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SizeToFitTab Procedure 
PROCEDURE SizeToFitTab :
/*------------------------------------------------------------------------------
 Purpose: For programmatically added tabs that do their own initialization
          F.ex FlatView, ie start of JBoxDataBrw.w from FlatViewRecord in JBoxUiLib.p
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWin AS HANDLE NO-UNDO.

IF NOT AVAIL ttProgram OR ttProgram.hWindow NE ihWin THEN
  FIND FIRST ttProgram
       WHERE ttProgram.hWindow = ihWin
       NO-ERROR.
IF NOT AVAIL ttProgram OR (AVAIL ttProgram AND NOT ttProgram.bEmbed) THEN RETURN.

bSizedToFit = YES.

/* One way to detect that resize setting are in place: */
IF DYNAMIC-FUNCTION("getWinMinXmove",ihWin,"X") > 0 THEN DO:
  hStatusFrame = ?.
  getWindowFrames(ihWin:FIRST-CHILD).

  /* The window is automatically sized to fit the available space so to get a delta for the frames to grow or shrink we need to re-assign design-size to current */
  DYNAMIC-FUNCTION("setCurrWidthHeight",ihWin,iDesignWidth - 3,iDesignHeight + (IF VALID-HANDLE(hStatusFrame) THEN hStatusFrame:HEIGHT-PIXELS ELSE 0)).
  /* The same values need to be assigned as org. win size if any suppressed windows that are not started on initialize are to be correctly adjusted */
  DYNAMIC-FUNCTION("setOrgWidthHeight",ihWin,iDesignWidth - 3,iDesignHeight + (IF VALID-HANDLE(hStatusFrame) THEN hStatusFrame:HEIGHT-PIXELS ELSE 0)).
  
  /* The status-bar frame is created after the window was resized in MDIChildForm and therefore it shouldn't be touched this one time: */
  DYNAMIC-FUNCTION("setOneTimeException",hStatusFrame).
  
  APPLY "WINDOW-RESIZED" TO ihWin.
END.

/*IF NOT AVAIL ttProgram OR ttProgram.hWindow NE ihWin THEN*/
/*  FIND FIRST ttProgram                                   */
/*       WHERE ttProgram.hWindow = ihWin                   */
/*       NO-ERROR.                                         */
IF AVAIL ttProgram THEN
  RUN ActivateWindow(ttProgram.iNodeIndex).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartChildTab) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartChildTab Procedure 
PROCEDURE StartChildTab :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icProcName     AS CHAR   NO-UNDO.
DEF INPUT  PARAM icWinTitle     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
DEF INPUT  PARAM ibKeepOpen     AS LOG    NO-UNDO.
DEF INPUT  PARAM icDirection    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.

DEF VAR hWin            AS HANDLE NO-UNDO.
DEF VAR bMoveToNewGroup AS LOG NO-UNDO.
/*
DEF VAR oGroup AS Infragistics.Win.UltraWinTabbedMdi.MdiTabGroup NO-UNDO.
DEF VAR oOrien AS System.Windows.Forms.Orientation NO-UNDO.

MoveToNewGroup(INPUT Infragistics.Win.UltraWinTabbedMdi.MdiTabGroup, INPUT
Infragistics.Win.RelativePosition, INPUT System.Windows.Forms.Orientation) - Infragistics.Win.UltraWinTabbedMdi.MdiTab
Returns Infragistics.Win.UltraWinTabbedMdi.MdiTabGroup  value.
*/

/*IF icDirection NE "" THEN                                                                           */
/*  goJBoxMainForm:TabOrientation = (IF CAN-DO("H,Hor,Horizontal",icDirection) THEN "Hor" ELSE "Ver").*/

IF NOT ibNew THEN DO:
  FIND FIRST ttProgram 
       WHERE ttProgram.cProcName = icProcName
         AND ttProgram.oChildForm NE ?
        NO-ERROR.
  IF NOT AVAIL ttProgram THEN DO:        
    FIND FIRST ttProgram 
         WHERE ttProgram.cProcName = icProcName NO-ERROR.
  
    IF NOT AVAIL ttProgram THEN  
      bMoveToNewGroup = icProcName NE "JBoxDataBrw.w".
    ELSE IF AVAIL ttProgram THEN DO:
      IF ttProgram.cProcName = "JBoxDataBrw.w" THEN bMoveToNewGroup = NO.
      ELSE IF ttProgram.oChildForm = ? THEN bMoveToNewGroup = YES.
    END.
  END.    
END. 
ELSE bMoveToNewGroup = YES.

RUN StartTabWindow(icProcName,icWinTitle,ihParent,bMoveToNewGroup,ibKeepOpen,OUTPUT ohWinHandle).
IF bMoveToNewGroup THEN DO:
  oLastMdiTab:MoveToNewGroup().

  /*IF icDirection NE "" THEN               */
  /*  goJBoxMainForm:TabOrientation = "Ver".*/
  
  hWin = ohWinHandle:CURRENT-WINDOW.
  
  APPLY "WINDOW-RESIZED" TO hWin.
  
  PUBLISH "SetStatusBarWidth" (hWin).
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartChildWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartChildWindow Procedure 
PROCEDURE StartChildWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  icProcName:   File name 
               icWinTitle:  If blank the window will inherit the child win title
               ihParent:    Calling procedure
               ibNew:       TRUE if multiple instances allowed
               ohWinHandle: Procedure handle to child
                
  Notes:       Invoke procedure "InitializeChild" in the calling procedure
               to set attributes in the child prior to InitializeObject
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icProcName     AS CHAR   NO-UNDO.
DEF INPUT  PARAM icWinTitle     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.

DEF VAR hMenuItm   AS HANDLE NO-UNDO.
DEF VAR bAlarmProc AS LOG    NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN DO:
  icProcName = ENTRY(NUM-ENTRIES(REPLACE(icProcName,"/","\"),"\"),"\").
  IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN DO:
    MESSAGE "Could not find" icProcName
            VIEW-AS ALERT-BOX.
    RETURN.
  END.  
END.

FIND FIRST ttProgram 
     WHERE ttProgram.cProcName = SOURCE-PROCEDURE:FILE-NAME NO-ERROR.
IF AVAIL ttProgram AND ttProgram.iNodeIndex = -1000 THEN
  bAlarmProc = YES.

FIND FIRST ttProgram 
     WHERE ttProgram.cProcName = icProcName NO-ERROR.

IF AVAIL ttProgram AND bAlarmProc THEN DO:
  RUN StartWindow(STRING(ttProgram.iNodeIndex)).
  RETURN.
END.

FIND FIRST ttProgram 
     WHERE ttProgram.cProcName = icProcName
       AND ttProgram.bChildWin
     NO-ERROR.

IF NOT AVAIL ttProgram OR ibNew OR (AVAIL ttProgram AND NOT VALID-HANDLE(ttProgram.hWinProc)) THEN DO:
  IF NOT AVAIL ttProgram THEN DO:
    CREATE ttProgram.
    ASSIGN ttProgram.cProcName     = icProcName
           ttProgram.cMenuTitle   = icWinTitle
           ttProgram.hParent      = ihParent
           ttProgram.bChildWin    = TRUE
           .
  END.         
  
  IF cWinStartParam NE "" THEN
    RUN VALUE(icProcName) PERSIST SET ttProgram.hWinProc (cWinStartParam).
  ELSE  
    RUN VALUE(icProcName) PERSIST SET ttProgram.hWinProc.
    
  IF icWinTitle NE "" THEN
    ttProgram.hWinProc:CURRENT-WINDOW:TITLE = ttProgram.cMenuTitle.
  
  ttProgram.hWindow = ttProgram.hWinProc:CURRENT-WINDOW.  
  
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"setParent") THEN 
    DYNAMIC-FUNCTION("setParent" IN ttProgram.hWinProc,ihParent).
  
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
    IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"InitializeChild") THEN
      RUN InitializeChild IN ihParent (ttProgram.hWinProc).
    RUN InitializeObject IN ttProgram.hWinProc.
  END.

  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"ChangeCompany") THEN
    DYNAMIC-FUNCTION("setCompanyHeader",ttProgram.hWinProc:CURRENT-WINDOW).
END.
ELSE IF AVAIL ttProgram THEN
  ASSIGN ttProgram.cProcName          = icProcName
         ttProgram.cMenuTitle         = icWinTitle
         ttProgram.hParent            = ihParent
         ttProgram.hWinProc:CURRENT-WINDOW:TITLE = IF icWinTitle NE "" THEN icWinTitle ELSE ttProgram.hWinProc:CURRENT-WINDOW:TITLE
         .

IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"MoveToTop") THEN 
  RUN MoveToTop IN ttProgram.hWinProc.
ELSE DO:
  ttProgram.hWinProc:CURRENT-WINDOW:WINDOW-STATE = 3.
  ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
END.

ohWinHandle = ttProgram.hWinProc.

FINALLY:
  SESSION:SET-WAIT-STATE("").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"isModal","").
  cWinStartParam = "".
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartMenuId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartMenuId Procedure 
PROCEDURE StartMenuId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMenuId AS INT NO-UNDO.

FIND FIRST ttProgram WHERE ttProgram.iJBoxMenuId = iiMenuId NO-ERROR.
IF AVAIL ttProgram THEN
  RUN StartWindow(STRING(ttProgram.iNodeIndex)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartMenuProcName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartMenuProcName Procedure 
PROCEDURE StartMenuProcName :
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icProcName AS CHAR   NO-UNDO.
DEF INPUT  PARAM ibEmbed    AS LOG    NO-UNDO.
DEF OUTPUT PARAM ohWinProc  AS HANDLE NO-UNDO.
DEF OUTPUT PARAM oiMenuX    AS INT    NO-UNDO.
DEF OUTPUT PARAM oiMenuY    AS INT    NO-UNDO.

DEF VAR bOrgEmbed AS LOG NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram WHERE ttProgram.cProcName = icProcName NO-ERROR.
IF AVAIL ttProgram THEN DO:
  IF VALID-HANDLE(ttProgram.hWinProc) AND VALID-HANDLE(ttProgram.hWinProc:CURRENT-WINDOW) THEN
    ttProgram.hWinProc:CURRENT-WINDOW:MOVE-TO-TOP().
  ELSE DO:       
    ASSIGN bOrgEmbed = ttProgram.bEmbed
           ttProgram.bEmbed = ibEmbed.
    RUN StartWindow(STRING(ttProgram.iNodeIndex)).
    ttProgram.bEmbed = bOrgEmbed.
    ASSIGN ohWinProc = ttProgram.hWinProc
           oiMenuX   = goJBoxMainForm:Left
           oiMenuY   = goJBoxMainForm:Top. 
    IF NOT ibEmbed THEN goJBoxMainForm:SendToBack().       
  END.  
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartTabWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartTabWindow Procedure 
PROCEDURE StartTabWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  icProcName:  File name 
               icTabTitle:  Mandatory (if blank the file name will be used)
               ihParent:    Calling procedure
               ohWinHandle: Procedure handle to child
                
  Notes:       Invoke procedure "InitializeChild" in the calling procedure
               to set attributes in the child prior to InitializeObject
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icProcName     AS CHAR   NO-UNDO.
DEF INPUT  PARAM icTabTitle     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihParent       AS HANDLE NO-UNDO.
DEF INPUT  PARAM ibNew          AS LOG    NO-UNDO.
DEF INPUT  PARAM ibKeepOpen     AS LOG    NO-UNDO.
DEF OUTPUT PARAM ohWinHandle    AS HANDLE NO-UNDO.

DEF VAR iMyTabIndex AS INT NO-UNDO.
DEF VAR hWin        AS HANDLE NO-UNDO.
DEF VAR rttProgram  AS ROWID NO-UNDO.

DEF BUFFER bttProgram  FOR ttProgram.
DEF BUFFER bbttProgram FOR ttProgram.

IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN DO:
  icProcName = ENTRY(NUM-ENTRIES(REPLACE(icProcName,"/","\"),"\"),"\").
  IF SEARCH(icProcName) = ? AND SEARCH(SUBSTR(icProcName,1,LENGTH(icProcName) - 1) + "r") = ? THEN DO:
    MESSAGE "Could not find" icProcName
            VIEW-AS ALERT-BOX.
    RETURN.
  END.  
END.

FIND FIRST ttProgram 
     WHERE ttProgram.cProcName = icProcName
       AND ttProgram.oChildForm NE ?
      NO-ERROR.
IF NOT AVAIL ttProgram THEN
  FIND FIRST ttProgram 
       WHERE ttProgram.cProcName = icProcName NO-ERROR.

IF AVAIL ttProgram AND ttProgram.cProcName NE "JBoxDataBrw.w" AND NOT ibNew THEN DO:
  IF NOT ibNew AND ttProgram.oChildForm NE ? THEN DO:
    ASSIGN iLastMenuId = ttProgram.iJBoxMenuId
           ohWinHandle = ttProgram.hWinProc
           CAST(ttProgram.oChildForm,"Progress.Windows.MDIChildForm"):Text = icTabTitle.
                  
    CAST(ttProgram.oChildForm,"Progress.Windows.MDIChildForm"):BringToFront().
  END.
  ELSE DO:
    ASSIGN ttProgram.hParent    = ihParent
           ttProgram.cMenuLabel = (IF icTabTitle NE "" THEN icTabTitle ELSE ttProgram.cMenuLabel)
           rttProgram           = ROWID(ttProgram).
    RUN StartWindow(STRING(ttProgram.iNodeIndex)).
    FIND ttProgram WHERE ROWID(ttProgram) = rttProgram NO-ERROR.
    IF AVAIL ttProgram THEN DO:
      ohWinHandle = ttProgram.hWinProc.
      IF ibKeepOpen THEN
        ttProgram.hParent = ?.
    END.
  END.  
  RETURN.
END.
ELSE DO:
  CREATE ttProgram.
  ASSIGN ttProgram.cProcName    = (IF icProcName = "JBoxDataBrw.w" THEN "" ELSE icProcName)
         ttProgram.cMenuTitle   = icTabTitle
         ttProgram.cMenuLabel   = icTabTitle
         ttProgram.cLaunchType  = (IF icProcName = "JBoxDataBrw.w" THEN "DATA-BROWSE" ELSE "START-WINDOW")
         ttProgram.bEmbed       = YES
         ttProgram.hParent      = ihParent
         ttProgram.bChildWin    = NO
         iNewTabWinIdx          = iNewTabWinIdx - 1
         ttProgram.iNodeIndex   = iNewTabWinIdx
         iMyTabIndex            = iNewTabWinIdx
         .
  RUN StartWindow(STRING(ttProgram.iNodeIndex)).
  
  FIND FIRST ttProgram
       WHERE ttProgram.iNodeIndex = iMyTabIndex
       NO-ERROR.
  IF AVAIL ttProgram THEN DO:
    ohWinHandle = ttProgram.hWinProc.
    IF ibKeepOpen THEN
      ttProgram.hParent = ?.
    ELSE IF icProcName = "JBoxDataBrw.w" THEN DO:
      FIND FIRST bttProgram 
           WHERE bttProgram.hParent   = ttProgram.hParent
             AND bttProgram.hWinProc  NE ttProgram.hWinProc 
             AND bttProgram.cProcName = "JBoxDataBrw.w"
           NO-ERROR.
/*      IF AVAIL bttProgram THEN DO:                                                                             */
/*        oLastMdiTab:MoveToGroup(CAST(bttProgram.oMdiTab,"Infragistics.Win.UltraWinTabbedMdi.MdiTab"):TabGroup).*/
/*      END.                                                                                                     */
/*      ELSE DO:                                                                                                 */
/*        goJBoxMainForm:TabOrientation = "Horizontal".                                                          */
      oLastMdiTab:MoveToNewGroup().
      hWin = ohWinHandle:CURRENT-WINDOW.
      APPLY "WINDOW-RESIZED" TO hWin.
      PUBLISH "SetStatusBarWidth" (ohWinHandle:CURRENT-WINDOW).
/*        goJBoxMainForm:TabOrientation = "Vertical".                                                            */
/*      END.                                                                                                     */
    END.  
  END.
END.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartTimer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartTimer Procedure 
PROCEDURE StartTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH("controls.dll") NE ? AND SEARCH("JBoxJLWTimer.r") NE ? THEN
  RUN JBoxJLWtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
ELSE IF SEARCH("JBoxABLtimer.r") NE ? AND PROPATH MATCHES "*webclient*" THEN
  RUN JBoxABLtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
ELSE IF SEARCH("rstimer.ocx") NE ? AND PROPATH MATCHES "*webclient*" THEN
  RUN JBoxRsTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
ELSE
  RUN JBoxTimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartWindow Procedure 
PROCEDURE StartWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       isModal can be used to control if other menu items can be started
               typically set in a PROCEDURE that calls a dialog
------------------------------------------------------------------------------*/
DEF INPUT PARAM icNodeKey      AS CHAR   NO-UNDO.

DEFINE VARIABLE hWinFrame     AS HANDLE  NO-UNDO.
DEFINE VARIABLE cInitProc     AS CHAR    NO-UNDO.
DEFINE VARIABLE hInitProc     AS HANDLE  NO-UNDO.
DEFINE VARIABLE cLogFile      AS CHAR    NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",SESSION,"isModal") NE "" THEN DO:
  IF VALID-HANDLE(CURRENT-WINDOW) THEN DO:
    CURRENT-WINDOW:WINDOW-STATE = 3.
    CURRENT-WINDOW:MOVE-TO-TOP().
    CURRENT-WINDOW:TOP-ONLY = YES.
  END.
  RETURN.
END. 

bSizedToFit = NO.

SESSION:SET-WAIT-STATE("general").

DEFINE BUFFER ttProgram FOR ttProgram.

FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = INT(icNodeKey)
     NO-ERROR.
IF NOT AVAIL ttProgram THEN RETURN.

IF ttProgram.cLaunchType     = "THIS-PROCEDURE" THEN DO:
  RUN VALUE(ttProgram.cProcName) (ttProgram.cParameter) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
    RUN VALUE(ttProgram.cProcName).

  SESSION:SET-WAIT-STATE("").
  RETURN.
END.
ELSE IF ttProgram.cLaunchType = "URL" THEN DO:
  DYNAMIC-FUNCTION("setWebDoc","",ttProgram.cProcName).  
  SESSION:SET-WAIT-STATE("").
  RETURN.
END.

IF ttProgram.cLaunchType     = "DATA-BROWSE" THEN
  ASSIGN cInitProc           = ttProgram.cProcName
         ttProgram.cProcName = "JBoxDataBrw.w"
         .

IF SEARCH(SUBSTR(ttProgram.cProcName,1,R-INDEX(ttProgram.cProcName,".")) + "r") = ? AND 
   SEARCH(SUBSTR(ttProgram.cProcName,1,R-INDEX(ttProgram.cProcName,".")) + "w") = ? AND 
   SEARCH(SUBSTR(ttProgram.cProcName,1,R-INDEX(ttProgram.cProcName,".")) + "p") = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Could not find program " + ttProgram.cProcName,"ERROR","").
  SESSION:SET-WAIT-STATE("").
  RETURN.
END.

bEmbeddedForm = ttProgram.bEmbed.

IF bLogProgramExec THEN DO:
  cLogFile = SESSION:TEMP-DIR + ttProgram.cProcName + ".log".
  LOG-MANAGER:LOGFILE-NAME    = cLogFile. 
  LOG-MANAGER:LOG-ENTRY-TYPES = "4gltrace:4".
  LOG-MANAGER:CLEAR-LOG().
  LOG-MANAGER:LOGGING-LEVEL   = 4.
END.

IF ttProgram.cProcName NE "JBoxDataBrw.w" AND ttProgram.cParameter NE "" THEN DO:
  RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc (ttProgram.cParameter) NO-ERROR.
  IF ERROR-STATUS:ERROR AND AVAIL ttProgram THEN 
    RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc NO-ERROR.
  IF ERROR-STATUS:ERROR OR NOT AVAIL ttProgram THEN 
    RETURN.
END.
ELSE IF cWinStartParam NE "" THEN
  RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc (cWinStartParam) NO-ERROR.
ELSE 
  RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  RUN VALUE(ttProgram.cProcName) PERSIST SET ttProgram.hWinProc ("") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Failed to start " ttProgram.cProcName SKIP 
            ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX ERROR.
    SESSION:SET-WAIT-STATE("").
    DELETE ttProgram.
    RETURN.
  END.
END.

IF ttProgram.cLaunchType = 'PROCEDURE' AND ((VALID-HANDLE(ttProgram.hWinProc) AND NOT VALID-HANDLE(hInitProc)) OR NOT VALID-HANDLE(ttProgram.hWinProc)) THEN DO:
  DELETE PROCEDURE ttProgram.hWinProc NO-ERROR.
  SESSION:SET-WAIT-STATE("").
  RETURN.
END.

iLastMenuId = ttProgram.iJBoxMenuId.

IF VALID-HANDLE(ttProgram.hWinProc:NEXT-SIBLING) AND ttProgram.hWinProc:NEXT-SIBLING:CURRENT-WINDOW NE ? THEN DO:
  ASSIGN hInitProc          = ttProgram.hWinProc
         ttProgram.hWinProc = ttProgram.hWinProc:NEXT-SIBLING
         hWin               = ttProgram.hWinProc:CURRENT-WINDOW
         .
  IF ttProgram.cLaunchType = "PROCEDURE" THEN 
    DELETE PROCEDURE hInitProc.
END.

IF ttProgram.bEmbed THEN DO:
  ttProgram.bEmbed = EmbedMe(ttProgram.hWinProc).                                                 
  IF ttProgram.bEmbed AND bWinStatusArea THEN DO:
    DYNAMIC-FUNCTION("CreateStatusBar",hWin,"",1,YES,?).
    ttProgram.bUseTimer = YES.
  END.
END.
ELSE IF VALID-HANDLE(ttProgram.hWinProc:CURRENT-WINDOW) AND hWin NE ttProgram.hWinProc:CURRENT-WINDOW THEN hWin = ttProgram.hWinProc:CURRENT-WINDOW.
ELSE IF NOT VALID-HANDLE(ttProgram.hWinProc:CURRENT-WINDOW) THEN DO:
  RETURN.
END.  

IF DYNAMIC-FUNCTION("getAttribute",SESSION,"useDesignWindowTitle") NE "yes" AND VALID-HANDLE(hWin) THEN
  hWin:TITLE = REPLACE(ttProgram.cMenuLabel,"&","").

ttProgram.hWindow = hWin.

IF ttProgram.cLaunchType = "DATA-BROWSE" THEN DO:
  IF cInitProc NE "" THEN DO:
    IF VALID-HANDLE(hParameterField) AND ttProgram.cParameter NE "" THEN
      RUN VALUE(cInitProc) PERSIST SET hInitProc (ttProgram.hWinProc,ttProgram.cParameter).
    ELSE
      RUN VALUE(cInitProc) PERSIST SET hInitProc (ttProgram.hWinProc).

    ttProgram.cProcName = cInitProc.
      
    IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"setParent") AND CAN-DO(hInitProc:INTERNAL-ENTRIES,"myDatabrowseObject") THEN DO:
      DYNAMIC-FUNCTION("setParent" IN ttProgram.hWinProc,hInitProc).
      IF CAN-DO(hInitProc:INTERNAL-ENTRIES,"InitializeObject") THEN
        RUN InitializeObject IN hInitProc.
    END.    
  
    IF VALID-HANDLE(hInitProc) AND NOT CAN-DO(hInitProc:INTERNAL-ENTRIES,"InvalidateHandle") THEN DELETE PROCEDURE hInitProc.
    ELSE 
      SUBSCRIBE PROCEDURE hInitProc TO "InvalidateHandle" IN ttProgram.hWinProc.
  END.    
  ELSE DYNAMIC-FUNCTION("setParent" IN hWinProc,ttProgram.hParent).
END.
ELSE IF VALID-HANDLE(hInitProc) AND CAN-DO(hInitProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
    RUN InitializeObject IN hInitProc NO-ERROR.
END.
ELSE IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"InitializeObject") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  IF VALID-HANDLE(ttProgram.hParent) THEN DO:
    IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"setParent") THEN 
      DYNAMIC-FUNCTION("setParent" IN ttProgram.hWinProc,ttProgram.hParent).
    IF CAN-DO(ttProgram.hParent:INTERNAL-ENTRIES,"InitializeChild") THEN
      RUN InitializeChild IN ttProgram.hParent (ttProgram.hWinProc).
  END.  
  RUN InitializeObject IN ttProgram.hWinProc NO-ERROR.
END.
ELSE IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"local-initialize") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  RUN local-initialize IN ttProgram.hWinProc NO-ERROR.
  ttProgram.bUseTimer = YES.
END.
ELSE IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"dispatch") THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hWin).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
  RUN dispatch IN ttProgram.hWinProc ("initialize") NO-ERROR.
  ttProgram.bUseTimer = YES.
END.
  
IF ttProgram.bEmbed THEN DO:
  RUN ActivateWindow(INT(icNodeKey)).
  RUN SetWinMenuActionState.
  
  oChildForm:Activated:Subscribe(goJBoxMainForm:OnMdiChildFormActivated).
  

  /* One way to detect that resize setting are in place: */
  IF DYNAMIC-FUNCTION("getWinMinXmove",hWin,"X") > 0 AND NOT bSizedToFit THEN DO:
    hStatusFrame = ?.
    getWindowFrames(hWin:FIRST-CHILD).
  
    /* The window is automatically sized to fit the available space so to get a delta for the frames to grow or shrink we need to re-assign design-size to current */
    DYNAMIC-FUNCTION("setCurrWidthHeight",hWin,iDesignWidth - 3,iDesignHeight + (IF VALID-HANDLE(hStatusFrame) THEN hStatusFrame:HEIGHT-PIXELS ELSE 0)).
    /* The same values need to be assigned as org. win size if any suppressed windows that are not started on initialize are to be correctly adjusted */
    DYNAMIC-FUNCTION("setOrgWidthHeight",hWin,iDesignWidth - 3,iDesignHeight + (IF VALID-HANDLE(hStatusFrame) THEN hStatusFrame:HEIGHT-PIXELS ELSE 0)).
        
    /* The status-bar frame is created after the window was resized in MDIChildForm and therefore it shouldn't be touched this one time: */
    DYNAMIC-FUNCTION("setOneTimeException",hStatusFrame).
    
    APPLY "WINDOW-RESIZED" TO hWin.
  END.
END.
ELSE DO:
  /*   IF httMenuBuffer:AVAIL THEN DO:                                                        */
  /*     IF httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE MATCHES "*.ico" THEN            */
  /*       hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cImage"):BUFFER-VALUE)).         */
  /*     ELSE IF httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE MATCHES "*.ico" THEN */
  /*       hWin:LOAD-ICON(STRING(httMenuBuffer:BUFFER-FIELD("cParentImage"):BUFFER-VALUE)).   */
  /*     ELSE IF hWin:ICON = "" AND cAppImage NE "" THEN                                      */
  /*       hWin:LOAD-ICON(cAppImage).                                                         */
  /*   END.                                                                                   */
    
END.

bSizedToFit = NO.

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?). 
SESSION:SET-WAIT-STATE("").

IF NOT VALID-HANDLE(ttProgram.hWinProc) THEN RETURN.

IF ttProgram.cProcName NE "JBoxDataBrw.w" THEN DO:
  IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"MoveToTop") THEN DO:
    RUN MoveToTop IN ttProgram.hWinProc.
  END.
  ELSE DO:
    hWinFrame = hWin:FIRST-CHILD NO-ERROR.
    IF ttProgram.hWinProc:ADM-DATA NE ? THEN DO:
      hWin:VISIBLE = NO.
      hWin:VISIBLE = YES.
    END.
    IF VALID-HANDLE(hStatusFrame) AND hStatusFrame:WINDOW = hWin THEN
      hStatusFrame:MOVE-TO-TOP().
    IF VALID-HANDLE(hWinFrame) THEN 
      APPLY "entry" TO hWinFrame.
  END.
END.

IF ttProgram.bEmbed THEN DO:
  oChildForm:BringToFront().
  goJBoxMainForm:ActiveControl = oChildForm.
  ttProgram.oMdiTab = CAST(oLastMdiTab,"Progress.Lang.Object").
END.

IF CAN-DO(ttProgram.hWinProc:INTERNAL-ENTRIES,"MoveToTop") THEN
  RUN MoveToTop IN ttProgram.hWinProc.

IF bLogProgramExec THEN DO:
  LOG-MANAGER:CLOSE-LOG().

  RUN log-manager-report.p (cLogFile,cLogFile + ".txt").
  OS-COMMAND NO-WAIT VALUE("notepad " + cLogFile + ".txt").
END.

FINALLY:
  SESSION:SET-WAIT-STATE("").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"isModal","").
  cWinStartParam = "".
END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToggleBtnEvents) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToggleBtnEvents Procedure 
PROCEDURE ToggleBtnEvents :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE "On/off"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
RUN setViewBtnEvents (bOk).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserNotification) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UserNotification Procedure 
PROCEDURE UserNotification :
/*------------------------------------------------------------------------------
  Purpose:    Display a user notification and return the response to calling program 
  Parameters:  <none>
  Notes:      If a handling proc is given the menu will try to start it 
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icShortMsg      AS CHAR NO-UNDO.
DEF INPUT  PARAM icLongMsg       AS CHAR NO-UNDO.
DEF INPUT  PARAM icHandlingProc  AS CHAR NO-UNDO.
DEF INPUT  PARAM icPublishTo     AS CHAR NO-UNDO.
DEF INPUT  PARAM icContext       AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocResponse      AS CHAR NO-UNDO.

IF VALID-HANDLE(hMainMenu) AND THIS-PROCEDURE NE hMainMenu THEN RETURN.

RUN JBoxNotifyUser.w PERSIST (icShortMsg,icLongMsg,icHandlingProc,icPublishTo,OUTPUT ocResponse).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewCurrTab) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewCurrTab Procedure 
PROCEDURE ViewCurrTab :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex = iCurrNodeKey
     NO-ERROR.
IF AVAIL ttProgram THEN 
  RUN ViewTabProcHandle(ttProgram.hWinProc).     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewStartPanel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewStartPanel Procedure 
PROCEDURE ViewStartPanel :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
IF (SEARCH("JBoxStartPanel.w") = ? AND SEARCH("JBoxStartPanel.r") = ?) OR NOT VALID-HANDLE(hParameterField) THEN RETURN.

FIND FIRST ttProgram 
     WHERE ttProgram.iNodeIndex  = -10000
     NO-ERROR.
IF NOT AVAIL ttProgram THEN DO:
  CREATE ttProgram.
  ASSIGN ttProgram.iNodeIndex  = -10000
         ttProgram.cProcName   = "JBoxStartPanel.w"
         ttProgram.cParameter  = cPanelURL
         ttProgram.cMenuLabel  = "Start"
         ttProgram.cLaunchType = "start-window"
         ttProgram.bEmbed      = yes
         ttProgram.iJBoxMenuId = -10000
         ttProgram.cMenuType   = "menu-item"
         .
END.
RUN StartWindow("-10000").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewTabProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewTabProcHandle Procedure 
PROCEDURE ViewTabProcHandle :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihTabProc AS HANDLE NO-UNDO.

FIND FIRST ttProgram 
     WHERE ttProgram.hWinProc = ihTabProc
     NO-ERROR.
IF AVAIL ttProgram AND ttProgram.oChildForm NE ? THEN DO:
  iLastMenuId = ttProgram.iJBoxMenuId.
  CAST(ttProgram.oChildForm,"Progress.Windows.MDIChildForm"):BringToFront().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-BuildActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildActionRibbonGroup Procedure 
FUNCTION BuildActionRibbonGroup RETURNS LOGICAL
  (INPUT ihMenu       AS HANDLE,
   INPUT ioParent     AS CLASS Progress.Lang.Object,
   INPUT icParentType AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hChild        AS HANDLE NO-UNDO.
DEF VAR cDummy        AS CHAR   NO-UNDO.
DEF VAR bFirstInGroup AS LOG    NO-UNDO.
DEF VAR hCorrBtn      AS HANDLE NO-UNDO.
DEF VAR cImage        AS CHAR   NO-UNDO.

DEF BUFFER ttProgram FOR ttProgram.

/* hChild = ihMenu:FIRST-CHILD. */
hChild = ihMenu.

REPEAT WHILE VALID-HANDLE(hChild):
  CASE hChild:TYPE:
    WHEN "SUB-MENU" THEN DO:
      CREATE ttProgram.
      ASSIGN ttProgram.iNodeIndex = ixNode
             ttProgram.oRibbonObj = goJBoxMainForm:AddRibbonPopupMenuTool(
                                    ioParent,icParentType,ttProgram.iNodeIndex,hChild:LABEL,"","",NO,bFirstInGroup,OUTPUT ttProgram.cObjectType)
             ixNode               = ixNode + 1
             bFirstInGroup        = NO
             .
      IF VALID-HANDLE(hChild:FIRST-CHILD) THEN
        BuildActionRibbonGroup(hChild:FIRST-CHILD,ttProgram.oRibbonObj,ttProgram.cObjectType).
    END.
    WHEN "MENU-ITEM" THEN DO:
      IF hChild:SUBTYPE = "RULE" THEN
        bFirstInGroup = YES.        
      ELSE DO:
        cImage = "".
        IF hChild:PRIVATE-DATA NE "" THEN DO:
          hCorrBtn = WIDGET-HANDLE(hChild:PRIVATE-DATA) NO-ERROR.
          IF VALID-HANDLE(hCorrBtn) AND SEARCH(hCorrBtn:IMAGE) NE ? THEN   
            cImage = SEARCH(hCorrBtn:IMAGE).
        END.    
          
        CREATE ttProgram.
        ASSIGN ttProgram.iNodeIndex   = ixNode
               ttProgram.oRibbonObj   = IF hChild:TOGGLE-BOX THEN
                                          goJBoxMainForm:AddRibbonStateButtonTool(
                                                 ioParent,icParentType,ttProgram.iNodeIndex,hChild:LABEL,
                                                 "",hChild:ACCELERATOR,NO,bFirstInGroup,OUTPUT ttProgram.cObjectType)
                                        ELSE
                                          goJBoxMainForm:AddRibbonButtonTool(
                                                 ioParent,icParentType,ttProgram.iNodeIndex,hChild:LABEL,
                                                 cImage,hChild:ACCELERATOR,NO,bFirstInGroup,OUTPUT ttProgram.cObjectType)
               ttProgram.hTrigWidget  = hChild
               ttProgram.cAccelerator = hChild:ACCELERATOR
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

&IF DEFINED(EXCLUDE-ClearActionRibbonGroup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearActionRibbonGroup Procedure 
FUNCTION ClearActionRibbonGroup RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF vGrpWinMenu NE ? THEN DO:
  goJBoxMainForm:RemoveRibbonGroup(vGrpWinMenu).
  FOR EACH ttProgram
      WHERE ttProgram.iNodeIndex > 1000:
    DELETE ttProgram.
  END.
  vGrpWinMenu = ?.
  RETURN YES.
END.
RETURN NO.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmbedMe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EmbedMe Procedure 
FUNCTION EmbedMe RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Might be called from the procedure itself. For ADM this call should
           be added to windowmn.i before create-objects:
            
    DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.
------------------------------------------------------------------------------*/
IF ihProc = hEmbedded THEN RETURN YES.

IF NOT VALID-HANDLE(ihProc:CURRENT-WINDOW) THEN RETURN NO.

IF NOT VALID-OBJECT(goJBoxMainForm) THEN RETURN NO.

ASSIGN hWin          = ihProc:CURRENT-WINDOW
       iDesignWidth  = hWin:WIDTH-PIXELS 
       iDesignHeight = hWin:HEIGHT-PIXELS   
       .

IF NOT AVAIL ttProgram OR NOT ttProgram.bEmbed THEN RETURN NO.

IF ttProgram.cProcName NE ihProc:FILE-NAME THEN RETURN NO.

IF VALID-HANDLE(hWin:MENUBAR) THEN
  ttProgram.hTrigWidget = hWin:MENUBAR.
bWinStatusArea = hWin:STATUS-AREA AND bCreateStatusBar.

oChildForm = NEW MDIChildForm(goJBoxMainForm, hWin) NO-ERROR.
IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
  DELETE OBJECT oChildForm.
  RETURN NO.
END.

oChildForm:Tag = ttProgram.iNodeIndex.
/*   oChildForm:Icon = goJBoxMainForm:Icon. */
oChildForm:Text = REPLACE(ttProgram.cMenuLabel,"&","").

oChildForm:FormClosing:Subscribe(goJBoxMainForm:OnMdiChildFormClosing).
oChildForm:Show().

PUBLISH "SetChildWrapperForm" (ihProc,oChildForm). /* Target WrapWindowInForm.p */

ttProgram.oChildForm = CAST(oChildForm,"Progress.Lang.Object").

hEmbedded = ihProc.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsWindowActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsWindowActive Procedure 
FUNCTION getIsWindowActive RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF BUFFER bttProgram FOR ttProgram.

FOR EACH bttProgram:
  IF VALID-HANDLE(bttProgram.hWindow) THEN RETURN YES.
END.
RETURN NO.    

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParameter Procedure 
FUNCTION getParameter RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF AVAIL ttProgram THEN
  RETURN ttProgram.cParameter.
/* IF VALID-HANDLE(hParameterField) THEN  */
/*   RETURN hParameterField:BUFFER-VALUE. */

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProgramMultiInstanceList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProgramMultiInstanceList Procedure 
FUNCTION getProgramMultiInstanceList RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RETURN cProgramMultiInstanceList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStartPanelURL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStartPanelURL Procedure 
FUNCTION getStartPanelURL RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
RETURN cPanelURL.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTtProgramHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTtProgramHandle Procedure 
FUNCTION getTtProgramHandle RETURNS HANDLE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RETURN httProgram.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWindowFrames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWindowFrames Procedure 
FUNCTION getWindowFrames RETURNS HANDLE
  ( INPUT hWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  List widgets to file
    Notes:  NB! Use level = 0 for initial call
------------------------------------------------------------------------------*/
REPEAT WHILE hWidget NE ?:
  IF hWidget:TYPE = "FRAME" THEN DO:
    IF hWidget:NAME = "StatusFrame" THEN 
      hStatusFrame = hWidget.
/*     hWidget:BGCOLOR = ?.  */
  END.
  IF CAN-QUERY(hWidget,'FIRST-CHILD') AND hWidget:FIRST-CHILD  <> ? THEN
    getWindowFrames (hWidget:FIRST-CHILD).

  hWidget = hWidget:NEXT-SIBLING.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveQuickAccess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION saveQuickAccess Procedure 
FUNCTION saveQuickAccess RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cRetVal     AS CHARACTER NO-UNDO.
DEF VAR cMenuIdList AS CHARACTER NO-UNDO.

IF NOT DYNAMIC-FUNCTION("getIsTableInstalled","JBoxMenuFavorites") THEN RETURN NO.

cRetVal = goJBoxMainForm:getQuickAccessToolsKeyList().
DO ix = 1 TO NUM-ENTRIES(cRetVal):
  bOk = httMenuBuffer:FIND-FIRST("WHERE iNodeIndex = " + ENTRY(ix,cRetVal)) NO-ERROR.
  IF httMenuBuffer:AVAIL THEN
    cMenuIdList = cMenuIdList + (IF cMenuIdList NE "" THEN "," ELSE "") + STRING(httMenuBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).
END.
DYNAMIC-FUNCTION("runProc","jbsetup_save_menu_favorites.p",cMenuIdList + ";QAT",?).
IF iLastMenuId = ? THEN iLastMenuId = 0.
ELSE DO:
  FIND FIRST ttProgram 
       WHERE ttProgram.iJBoxMenuId = iLastMenuId    
       NO-ERROR.
  IF AVAIL ttProgram AND ttProgram.oChildForm = ? THEN
    iLastMenuId = 0.
END. 
DYNAMIC-FUNCTION("runProc","jbsetup_save_menu_favorites.p",STRING(iLastMenuId) + ";LAST",?).
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppStyleSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppStyleSheet Procedure 
FUNCTION setAppStyleSheet RETURNS LOGICAL
  ( INPUT icStyleSheet AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cStyleSheet = icStyleSheet.
goJBoxMainForm:setAppStyleSheet(icStyleSheet).

/* goJBoxMainForm:setAppStyleSheet("vs2008_test.isl"). */
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setClientSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setClientSize Procedure 
FUNCTION setClientSize RETURNS LOGICAL
  ( INPUT iiClientXsize AS INT,
    INPUT iiClientYsize AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-OBJECT(goJBoxMainForm) THEN
  goJBoxMainForm:ClientSize = NEW System.Drawing.Size(iiClientXsize,iiClientYsize).
ELSE  
  ASSIGN iClientXsize = iiClientXsize
         iClientYsize = iiClientYsize.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCreateStatusBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCreateStatusBar Procedure 
FUNCTION setCreateStatusBar RETURNS LOGICAL
  ( INPUT ibCreateStatusBar AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bCreateStatusBar = ibCreateStatusBar.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCustomMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCustomMenu Procedure 
FUNCTION setCustomMenu RETURNS LOGICAL
  ( INPUT icTabGroupTitle AS CHAR,
    INPUT icProgramList   AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
ASSIGN cCustomTabGroupTitle = icTabGroupTitle
       cCustomProgramList   = icProgramList
       .
RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setIsChildWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setIsChildWindow Procedure 
FUNCTION setIsChildWindow RETURNS LOGICAL
  ( INPUT ibIsChildWindow AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bIsChildWindow = ibIsChildWindow.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMenuBgImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuBgImage Procedure 
FUNCTION setMenuBgImage RETURNS LOGICAL
  ( INPUT icBgImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setBgImage(icBgImage).

/* goJBoxMainForm:setBgImage("bg-green.bmp"). */
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setParentHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParentHandle Procedure 
FUNCTION setParentHandle RETURNS LOGICAL
  (INPUT ihParent AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setProgramMultiInstanceList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setProgramMultiInstanceList Procedure 
FUNCTION setProgramMultiInstanceList RETURNS LOGICAL
  ( INPUT icProgramMultiInstanceList AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
cProgramMultiInstanceList = icProgramMultiInstanceList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRibbonIconImage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRibbonIconImage Procedure 
FUNCTION setRibbonIconImage RETURNS LOGICAL
  ( INPUT icImageName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setRibbonIconImage(icImageName).

/* goJBoxMainForm:setRibbonIconImage("j0433808.png"). */
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRibbonMinimized) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRibbonMinimized Procedure 
FUNCTION setRibbonMinimized RETURNS LOGICAL
  ( INPUT ibRibbonMinimized AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bRibbonMinimized = ibRibbonMinimized.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setStartPanelURL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setStartPanelURL Procedure 
FUNCTION setStartPanelURL RETURNS LOGICAL
  ( INPUT icPanelURL AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   cPanelURL = icPanelURL.

   RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setStatusText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setStatusText Procedure 
FUNCTION setStatusText RETURNS LOGICAL
  ( INPUT icStatusText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
goJBoxMainForm:setStatusText(icStatusText).
/* goJBoxMainForm:setStatusText("Brynjar"). */

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTimerProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTimerProperties Procedure 
FUNCTION setTimerProperties RETURNS LOGICAL
  ( INPUT iiTimerInterval   AS INT,
    INPUT icTimerContext    AS CHAR,
    INPUT icTimerServerProc AS CHAR,
    INPUT icTimerClientProc AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN iTimerInterval    = iiTimerInterval  
       cTimerContext     = icTimerContext   
       cTimerServerProc  = icTimerServerProc
       cTimerClientProc  = icTimerClientProc
       .

IF NOT VALID-HANDLE(hTimer) THEN DO:
  RUN StartTimer.
  IF iTimerInterval > 1000 THEN
    RUN CheckTimerEvent.
END.

DYNAMIC-FUNCTION("setInterval" IN hTimer,iTimerInterval).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWindowTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWindowTitle Procedure 
FUNCTION setWindowTitle RETURNS LOGICAL
  ( INPUT icWinTitle AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cWinTitle = icWinTitle.
goJBoxMainForm:setWindowTitle(cWinTitle).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWinMenuGroupLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWinMenuGroupLabel Procedure 
FUNCTION setWinMenuGroupLabel RETURNS LOGICAL
  ( INPUT icWinMenuGrpLabel AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cWinMenuGrpLabel = icWinMenuGrpLabel.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

