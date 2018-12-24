&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : JBoxUIlib.p
    Purpose     : Library methods on UI objects

    Author(s)   : Not all functions are written by the Chemistry team. Whenever available, the
                  author is mentioned..
    Created     : Dec 2003 by brynjar@chemistry.no
    Notes       : 
    
    Changed     : Dec 2005 by brynjar@chemistry.no
                  - removed all functions for object manipulation to JBoxObjLib.p
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR bOK                       AS LOG NO-UNDO.
DEF VAR ix                        AS INT NO-UNDO.    
DEF VAR iReturn                   AS INT NO-UNDO.

/* (Historic) Behaviour variables: see setBehaviour */
DEF NEW SHARED VAR iDefaultSortFont          AS INT  NO-UNDO INIT 6.
DEF NEW SHARED VAR iDefaultSortColor         AS INT  NO-UNDO INIT 15.
DEF NEW SHARED VAR cBrowseSearchDefault      AS CHAR NO-UNDO INIT "goto".
DEF NEW SHARED VAR bTabOnReturn              AS LOG  NO-UNDO INIT TRUE.
DEF NEW SHARED VAR bSetSortLabel             AS LOG  NO-UNDO INIT TRUE.
DEF NEW SHARED VAR cDefActionList            AS CHAR NO-UNDO.
DEF NEW SHARED VAR cDefImageList             AS CHAR NO-UNDO.
DEF NEW SHARED VAR cCtrlHotkeyActions        AS CHAR NO-UNDO INIT "new,edit,delete,save,undo,filter".
DEF NEW SHARED VAR cCtrlHotkeys              AS CHAR NO-UNDO INIT "N,E,D,S,Z,F".
DEF NEW SHARED VAR cAltHotkeyActions         AS CHAR NO-UNDO.
DEF NEW SHARED VAR cAltHotkeys               AS CHAR NO-UNDO.
DEF NEW SHARED VAR cPassiveFilterButton      AS CHAR NO-UNDO.
DEF NEW SHARED VAR cActiveFilterButton       AS CHAR NO-UNDO INIT "gif/filterc.gif".
DEF NEW SHARED VAR bKeepExcel                AS LOG  NO-UNDO INIT TRUE. /* Keep excel running ant try to hook up next report to same instance */
DEF NEW SHARED VAR hCurrSourceProc           AS HANDLE NO-UNDO.
DEF NEW SHARED VAR hCurrWidget               AS HANDLE NO-UNDO.
DEF NEW SHARED VAR hCurrWindow               AS HANDLE NO-UNDO.
DEF NEW SHARED VAR hTmpObject                AS HANDLE NO-UNDO.
DEF NEW SHARED VAR cGlobSecDisabledActions   AS CHAR   NO-UNDO.
DEF NEW SHARED VAR cMarkAsc                  AS CHAR   NO-UNDO INIT " ^". 
DEF NEW SHARED VAR cMarkDesc                 AS CHAR   NO-UNDO INIT " v". 
DEF NEW SHARED VAR iEditBgColor              AS INT    NO-UNDO.
DEF NEW SHARED VAR hPrevWidget               AS HANDLE NO-UNDO.
DEF NEW SHARED VAR hPrevObject               AS HANDLE NO-UNDO.

DEF VAR bMyReturnNoApply          AS LOG    NO-UNDO.
DEF VAR hMyReturnNoApplyMethod    AS HANDLE NO-UNDO.
DEF VAR cMyReturnNoApplyMethod    AS CHAR   NO-UNDO.
DEF VAR iCursorOffset             AS INT    NO-UNDO.
DEF VAR bApplyLastkey             AS LOG    NO-UNDO.
DEF VAR cInputWidgetTypes         AS CHAR   NO-UNDO INIT "browse,button,combo-box,editor,fill-in,selection-list,slider,toggle-box,radio-set".
DEF VAR bFreezeEndResizeBrw       AS LOG    NO-UNDO.
DEF VAR hWidgetEnter              AS HANDLE NO-UNDO.
DEF VAR hWidgetEnd                AS HANDLE NO-UNDO.
DEF VAR hHelpWidget               AS HANDLE NO-UNDO.
DEF VAR bSetCurrHandles           AS LOG    NO-UNDO INIT TRUE.
DEF VAR iColorRowShade            AS INT    NO-UNDO.
DEF VAR cFunctionKeys             AS CHAR   NO-UNDO INIT "F5".
DEF VAR cFunctionKeyActions       AS CHAR   NO-UNDO INIT "refresh".
DEF VAR hCursorCurrColumn         AS HANDLE NO-UNDO.
DEF VAR cDefLabelList             AS CHAR   NO-UNDO.
DEF VAR iCancelNextEvents         AS INT    NO-UNDO.

DEF VAR httTable                  AS HANDLE NO-UNDO.
DEF VAR httTableBuffer            AS HANDLE NO-UNDO.
DEF VAR httTableQuery             AS HANDLE NO-UNDO.

/* Try if possible to keep one instance of Excel and Word running */
DEF VAR chExcelApplication        AS COM-HANDLE NO-UNDO.
DEF VAR chWordApplication         AS COM-HANDLE NO-UNDO.

/* Temp-tables to handle events and state for dynamic objects: */
DEF NEW SHARED TEMP-TABLE ttObject          /* Toolbar rectangle, browse, menu, buffer.. */
    FIELD hWindow                 AS HANDLE
    FIELD hObject                 AS HANDLE
    FIELD cObjectType             AS CHAR     /* browse, toolbar, menu.. */
    FIELD cState                  AS CHAR     /* Assigned by EventAction. F.ex "new" for toolbar, <sortcolumn>,<desc> for browse */
    FIELD hDesignObject           AS HANDLE
    FIELD hSourceProc             AS HANDLE 
    FIELD cObjectName             AS CHAR     /* Name taken from the name of the design object (most often) */
    FIELD cInitProc               AS CHAR
    FIELD cGenProc                AS CHAR
    INDEX idxObject   IS UNIQUE PRIMARY hObject 
    INDEX idxWindow   hWindow
    .
DEF BUFFER bttObject FOR ttObject.

DEF NEW SHARED TEMP-TABLE ttObjectLink
    FIELD hFromObject             AS HANDLE
    FIELD hToObject               AS HANDLE
    FIELD cLinkType               AS CHAR
    FIELD cLinkInfo               AS CHAR
    FIELD cInitProc               AS CHAR
    FIELD iSeq                    AS INT
    INDEX idxFrom    hFromObject
    INDEX idxTo      hToObject
    .
DEF BUFFER bttObjectLink FOR ttObjectLink.
DEF BUFFER bbttObjectLink FOR ttObjectLink.

DEF NEW SHARED TEMP-TABLE ttAttribute     
    FIELD hObject                 AS HANDLE
    FIELD cName                   AS CHAR    /* Display, Update, Input, SortColumn, Desc ... */
    FIELD cValue                  AS CHAR
    INDEX idxObject  hObject cName
    INDEX idxName    cName
    .
DEF BUFFER bttAttribute FOR ttAttribute.

DEF NEW SHARED TEMP-TABLE ttEvent           /* Button choose, menu item choose, start-search, etc */
    FIELD hWindow                 AS HANDLE
    FIELD hWidget                 AS HANDLE   /* handle to event widget, ie button, menu-item, browse.. */
    FIELD hObject                 AS HANDLE   /* Handle to event object, ie toolbar, browse, menu.. */
    FIELD cName                   AS CHAR     /* "Choose", "start-search", etc */
    FIELD cAction                 AS CHAR     /* Type of action. F.ex "new" for a toolbar event will cause delete, new and copy buttons to be disabled */
    FIELD cMethod                 AS CHAR     /* Name for procedure to excecute */
    FIELD cWidgetType             AS CHAR
    FIELD bReturnNoApply          AS LOG
    FIELD cLabel                  AS CHAR
    INDEX idxObject  hObject
    . 
DEF BUFFER bttEvent FOR ttEvent.

DEF TEMP-TABLE ttSort1 NO-UNDO
    FIELD iSeq   AS INT
    FIELD iSeq2  AS INT
    FIELD cText1 AS CHAR
    FIELD cText2 AS CHAR
    .
DEF BUFFER bttSort1 FOR ttSort1.

DEF TEMP-TABLE ttEventProcReturn
    FIELD hObject      AS HANDLE
    FIELD cMethod      AS CHAR
    FIELD cReturnValue AS CHAR
    FIELD iReturnCount AS INT.

DEF VAR iRetEvtCnt AS INT NO-UNDO.

DEF TEMP-TABLE ttBrowseColumns NO-UNDO
    FIELD hColumn   AS HANDLE
    FIELD iPos      AS INT
    INDEX idxPos IS PRIMARY iPos 
    .

PROCEDURE MessageBoxA EXTERNAL "user32.dll":  
  DEFINE INPUT PARAMETER hwnd    AS LONG.  
  DEFINE INPUT PARAMETER mbtext  AS CHARACTER.  
  DEFINE INPUT PARAMETER mbtitle AS CHARACTER.  
  DEFINE INPUT PARAMETER style   AS LONG.  
  DEFINE RETURN PARAMETER result AS LONG.
END.

PROCEDURE ShellExecuteA EXTERNAL "SHELL32.DLL" :
  DEFINE INPUT  PARAMETER hHandle       AS LONG.
  DEFINE INPUT  PARAMETER lpOperation   AS CHAR.
  DEFINE INPUT  PARAMETER lpFile        AS CHAR.
  DEFINE INPUT  PARAMETER lpParameters  AS CHAR.
  DEFINE INPUT  PARAMETER lpDirectory   AS CHAR.
  DEFINE INPUT  PARAMETER nShowCmd      AS LONG.
  DEFINE RETURN PARAMETER hInstance     AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AddColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddColor Procedure 
FUNCTION AddColor RETURNS LOGICAL
  ( INPUT icName  AS CHAR,
    INPUT iiRed   AS INT,
    INPUT iiGreen AS INT,
    INPUT iiBlue  AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddFont Procedure 
FUNCTION AddFont RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApplyEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ApplyEvent Procedure 
FUNCTION ApplyEvent RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE,
    INPUT icAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BrwOrQryToFMapTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BrwOrQryToFMapTranslation Procedure 
FUNCTION BrwOrQryToFMapTranslation RETURNS LOGICAL
  ( INPUT ihBrowseOrQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChangePrimarySearchBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ChangePrimarySearchBuffer Procedure 
FUNCTION ChangePrimarySearchBuffer RETURNS CHARACTER
  ( INPUT ihQueryObject    AS HANDLE,
    INPUT icPrimaryBuffer  AS CHAR,
    INPUT icPrimaryQuery   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckModified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckModified Procedure 
FUNCTION CheckModified RETURNS LOGICAL
  ( INPUT ihFrameOrFieldMap AS HANDLE,
    INPUT icFunction        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseAndDisableChilds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CloseAndDisableChilds Procedure 
FUNCTION CloseAndDisableChilds RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DisplayRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DisplayRow Procedure 
FUNCTION DisplayRow RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoLockWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoLockWindow Procedure 
FUNCTION DoLockWindow RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoMessage Procedure 
FUNCTION DoMessage RETURNS INTEGER
  ( INPUT iiMsgNo  AS INT,
    INPUT iiType   AS INT,
    INPUT icText   AS CHAR,
    INPUT icTitle  AS CHAR,
    INPUT icData   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryOperators) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixQueryOperators Procedure 
FUNCTION FixQueryOperators RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAttribute Procedure 
FUNCTION getAttribute RETURNS CHARACTER
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentAction Procedure 
FUNCTION getCurrentAction RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentObject Procedure 
FUNCTION getCurrentObject RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentSourceProc Procedure 
FUNCTION getCurrentSourceProc RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentWidget Procedure 
FUNCTION getCurrentWidget RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDropDownLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDropDownLabel Procedure 
FUNCTION getDropDownLabel RETURNS CHARACTER
  ( INPUT ihDropDownHandle AS HANDLE,
    INPUT icDelimiter      AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDropDownValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDropDownValue Procedure 
FUNCTION getDropDownValue RETURNS CHARACTER
  ( INPUT ihDropDownHandle AS HANDLE,
    INPUT icLabel          AS CHAR,
    INPUT icDelimiter      AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventProcReturnValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEventProcReturnValue Procedure 
FUNCTION getEventProcReturnValue RETURNS CHARACTER
  ( INPUT ihObject      AS HANDLE,
    INPUT icMethod      AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExcelHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getExcelHandle Procedure 
FUNCTION getExcelHandle RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMousePosition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMousePosition Procedure 
FUNCTION getMousePosition RETURNS INTEGER
  ( INPUT ihWinOrFrame AS HANDLE,
    INPUT icXY         AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryRowIdent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrimaryRowIdent Procedure 
FUNCTION getPrimaryRowIdent RETURNS CHARACTER
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryBufferList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQueryBufferList Procedure 
FUNCTION getQueryBufferList RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSelectionListLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectionListLabel Procedure 
FUNCTION getSelectionListLabel RETURNS CHARACTER
  ( INPUT ihSelListHandle AS HANDLE,
    INPUT iiEntry         AS INT,
    INPUT icDelimiter     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTmpObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTmpObject Procedure 
FUNCTION getTmpObject RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWordHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWordHandle Procedure 
FUNCTION getWordHandle RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadQueryFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadQueryFilter Procedure 
FUNCTION LoadQueryFilter RETURNS LOGICAL
  ( INPUT ihBrowseOrQuery  AS HANDLE,
    INPUT ibExcecuteFilter AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadWinIcon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadWinIcon Procedure 
FUNCTION LoadWinIcon RETURNS LOGICAL
  ( INPUT ihWindow   AS HANDLE,
    INPUT icIcon     AS CHAR,
    INPUT ibOverride AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NavigateFromBrowseOverlay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NavigateFromBrowseOverlay Procedure 
FUNCTION NavigateFromBrowseOverlay RETURNS LOGICAL
  ( INPUT ihOverlay AS HANDLE,
    INPUT ihBrowse  AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshParentRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RefreshParentRecord Procedure 
FUNCTION RefreshParentRecord RETURNS LOGICAL
  ( INPUT ihSourceQuery AS HANDLE,
    INPUT ihParentQuery AS HANDLE,
    INPUT icAction      AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshRowids) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RefreshRowids Procedure 
FUNCTION RefreshRowids RETURNS INTEGER
  ( INPUT ihQueryObject    AS HANDLE,
    INPUT icRowidList      AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseExcelHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReleaseExcelHandle Procedure 
FUNCTION ReleaseExcelHandle RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveUseIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RemoveUseIndex Procedure 
FUNCTION RemoveUseIndex RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveModified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveModified Procedure 
FUNCTION SaveModified RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setApplyLastKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setApplyLastKey Procedure 
FUNCTION setApplyLastKey RETURNS LOGICAL
  ( INPUT ibApplyLastkey AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAttribute Procedure 
FUNCTION setAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrowseSearchField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBrowseSearchField Procedure 
FUNCTION setBrowseSearchField RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT ihSortColumn AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrwOverlayBGcolNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBrwOverlayBGcolNum Procedure 
FUNCTION setBrwOverlayBGcolNum RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT iiColNum AS INT    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCancelNextEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCancelNextEvent Procedure
FUNCTION setCancelNextEvent RETURNS LOGICAL 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setCurrentObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCurrentObject Procedure 
FUNCTION setCurrentObject RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrentSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCurrentSourceProc Procedure 
FUNCTION setCurrentSourceProc RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrentWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCurrentWidget Procedure 
FUNCTION setCurrentWidget RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDynFilter Procedure 
FUNCTION setDynFilter RETURNS LOGICAL
  ( INPUT icAction      AS CHAR,
    INPUT ihFilter      AS HANDLE,
    INPUT ihQueryObject AS HANDLE,
    INPUT icSourceFile  AS CHAR,
    INPUT icUserContext AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEventProcReturnValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEventProcReturnValue Procedure 
FUNCTION setEventProcReturnValue RETURNS LOGICAL
  ( INPUT ihObject      AS HANDLE,
    INPUT icMethod      AS CHAR,
    INPUT icReturnValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoColumnSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoColumnSort Procedure 
FUNCTION setNoColumnSort RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icColumnList AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReturnNoApply) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setReturnNoApply Procedure 
FUNCTION setReturnNoApply RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReturnNoApplyMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setReturnNoApplyMethod Procedure 
FUNCTION setReturnNoApplyMethod RETURNS LOGICAL
  ( INPUT icMyReturnNoApplyMethod AS CHAR,
    INPUT ihMyReturnNoApplyMethod AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTmpObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTmpObject Procedure 
FUNCTION setTmpObject RETURNS LOGICAL
  ( INPUT ihTmpObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetCursor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWidgetCursor Procedure 
FUNCTION setWidgetCursor RETURNS LOGICAL
  ( INPUT ihWidgetCursor AS HANDLE,
    INPUT iiCursorOffset AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWidgetEnd Procedure 
FUNCTION setWidgetEnd RETURNS LOGICAL
  ( INPUT ihWidgetEnd AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetEnter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWidgetEnter Procedure 
FUNCTION setWidgetEnter RETURNS LOGICAL
  ( INPUT ihWidgetEnter AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StartWindow Procedure 
FUNCTION StartWindow RETURNS HANDLE
  ( INPUT icWindowName AS CHAR,
    INPUT icAction     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SwapPrescanToMainBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SwapPrescanToMainBuffer Procedure 
FUNCTION SwapPrescanToMainBuffer RETURNS LOGICAL
  ( INPUT ihQueryObject       AS HANDLE,
    INPUT icPreScanQueryList  AS CHAR )  FORWARD.

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
         HEIGHT             = 37.67
         WIDTH              = 66.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN JBoxLoadLib.p("JBoxObjLib.p").

IF cDefActionList = "" THEN
  cDefActionList = "accept,new,edit,close,help,copy,undo,delete,save"
                 + ",excel,word,print,filter,www,email,next,prev,first,last"
                 + ",commit,rollback,activate,flatview,refresh,accum,moveup,movedown,browseconfig,color,note,documents,insert,multiSortBrowse,copyBrowseRows,selectAllRows,deselectRow,rowsToBatch"
                   .
IF cDefImageList = "" THEN
  cDefImageList  = "bmp/accep16e.bmp,bmp/new16e.bmp,bmp/edit16e.bmp,bmp/e-exit.bmp,bmp/e-help.bmp,bmp/copy.bmp,bmp/undo16e.bmp,bmp/del16e.bmp,bmp/save.bmp" /* gif/saverec.gif" */
                 + ",gif/afexcel.gif,gif/afword.gif,bmp/print16e.bmp,gif/filter.gif,gif/afinternet.gif,gif/msngrWindow.gif,bmp/next.bmp,bmp/prev.bmp,bmp/first.bmp,bmp/last.bmp"
                 + ",bmp/commit.bmp,bmp/rollback.bmp,gif/active.gif,gif/sdogen16.gif,gif/refresh.gif,gif/statusu.gif,gif/moveup.gif,gif/movedown.gif,bmp/table.bmp,bmp/color.bmp,bmp/note16e.bmp,bmp/critino.bmp,bmp/add.bmp,bmp/bullet_triangle_green.bmp,bmp/copyrec.bmp,bmp/text1.bmp,bmp/text0.bmp,newbmp/yllpg16e.BMP"
                 .

IF cDefLabelList = "" THEN
  cDefLabelList = "Aksepter,Ny,Endre,Lukk,Hjelp,Kopier,Angre,Slett,Lagre,Excel,Word,Utskrift,Filter,www,E-post,Neste,Forrige,Første,Siste"
                + ",Commit,Rull tilbake,Aktiver,Drill-down visning,Oppdater visning,Akkumuler,Flytt opp,Flytt ned,Kolonneoppsett,Farge,Notat,Dokumenter,Sett inn,Sorter på flere kolonner,Kopier rad(er),Velg alle rader,Fjern markering av rad,Sett rader i resultatsett"
                .

DO ix = 1 TO NUM-ENTRIES(cDefActionList):
  IF SEARCH(ENTRY(ix,cDefImageList)) NE ? THEN
    setAttribute(SESSION,"BtnImg_" + ENTRY(ix,cDefActionList),ENTRY(ix,cDefImageList)).
END.

DO ix = 1 TO NUM-ENTRIES(cDefLabelList):
  setAttribute(SESSION,"BtnLabel_" + ENTRY(ix,cDefActionList),ENTRY(ix,cDefLabelList)).
END.

setAttribute(SESSION,"ActiveDocumentsButton","bmp/critiyes.bmp").
setAttribute(SESSION,"ActiveAccumButton","gif/statusc.gif").
setAttribute(SESSION,"PassiveAccumButton","gif/statusu.gif").
setAttribute(SESSION,"ActiveFilterButton","gif/filterc.gif").
setAttribute(SESSION,"PassiveFilterButton","gif/filter.gif").
setAttribute(SESSION,"ActiveConfigButton","bmp/tbls.bmp").
setAttribute(SESSION,"PassiveConfigButton","bmp/table.bmp").
setAttribute(SESSION,"ActiveNoteButton","bmp/notet16e.bmp").
setAttribute(SESSION,"PassiveNoteButton","bmp/note16e.bmp").
setAttribute(SESSION,"ActivePinButton","gif/pushin.gif").
setAttribute(SESSION,"PassivePinButton","gif/pushout.gif").
setAttribute(SESSION,"CtrlHotkeyActions",cCtrlHotkeyActions).
setAttribute(SESSION,"CtrlHotkeys",cCtrlHotkeys).
setAttribute(SESSION,"AltHotkeyActions",cAltHotkeyActions).
setAttribute(SESSION,"AltHotkeys",cAltHotkeys).
setAttribute(SESSION,"FunctionKeyActions",cFunctionKeyActions).
setAttribute(SESSION,"FunctionKeys",cFunctionKeys).

SUBSCRIBE TO "ResizeBrowseColumns" ANYWHERE.

CREATE WIDGET-POOL "localTT" NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AccumRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AccumRecord Procedure 
PROCEDURE AccumRecord :
/*------------------------------------------------------------------------------
  Purpose:     Pull up field selector for selecting distinct and accumulated fields
  Parameters:  <none>
  Notes:       Current object is the toolbar or browse.
------------------------------------------------------------------------------*/
DEF VAR hAccum          AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR cAccumWin       AS CHAR   NO-UNDO.
DEF VAR hCurrSourceProc AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.

IF ttObject.cObjectType NE "browse" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "browse" 
       NO-ERROR.
  
  IF NOT AVAIL ttObjectLink THEN RETURN.
  ELSE hBrowse = ttObjectLink.hToObject.
END.
ELSE hBrowse = ttObject.hObject.

ASSIGN hCurrObject     = ttObject.hObject
       hCurrSourceProc = ttObject.hSourceProc.

hAccum = WIDGET-HANDLE(getAttribute(hCurrObject,"accumhandle")).

IF NOT VALID-HANDLE(hAccum) THEN DO:
  cAccumWin = getAttribute(hCurrObject,"accumwindow").
  IF cAccumWin = "" THEN cAccumWin = "JBoxDynAccum.w".
  RUN VALUE(cAccumWin) PERSIST SET hAccum 
      (hCurrSourceProc,
       hBrowse,
       IF hCurrWidget:TYPE = "button" THEN hCurrWidget ELSE ?). /* Accum button */
  
  setAttribute(hCurrObject,"accumhandle",STRING(hAccum)).
END.
RUN MoveToTop IN hAccum.

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AltCursorDownBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AltCursorDownBrowse Procedure 
PROCEDURE AltCursorDownBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCol        AS HANDLE NO-UNDO.
DEF VAR hCurrColumn AS HANDLE NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

hBrowse = ttObject.hObject.

IF getAttribute(hBrowse,"localsort") NE "" THEN DO:
  hCursorCurrColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,getAttribute(hBrowse,"localsort")).
  IF VALID-HANDLE(hCursorCurrColumn) THEN
    RUN StartSearch.
  hCursorCurrColumn = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AltCursorDownSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AltCursorDownSortSearch Procedure 
PROCEDURE AltCursorDownSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hBrowse AS HANDLE NO-UNDO.

hBrowse = DYNAMIC-FUNCTION("GetLinkedObject",ttObject.hObject,"browse","from").
IF VALID-HANDLE(hBrowse) THEN
  APPLY "alt-cursor-down" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AnyPrintableBrowseColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnyPrintableBrowseColumn Procedure 
PROCEDURE AnyPrintableBrowseColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER ttObjectLink FOR ttObjectLink.

FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse-search-field"
     NO-ERROR.
IF AVAIL ttObjectLink AND ttObjectLink.hToObject:VISIBLE AND LASTKEY NE 32 THEN DO:
  APPLY "entry" TO ttObjectLink.hToObject.
  ttObjectLink.hToObject:MODIFIED = FALSE.
  APPLY LASTKEY.
  IF CAN-DO("INTEGER,DECIMAL",ttObjectLink.hToObject:DATA-TYPE) THEN
    APPLY "cursor-right" TO ttObjectLink.hToObject.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AnyPrintableKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnyPrintableKey Procedure 
PROCEDURE AnyPrintableKey :
/*------------------------------------------------------------------------------
  Purpose:    Switch on undo-button when input field is modified 
  Parameters:  <none>
  Notes:      Current object is the field-map. Must navigate to toolbar as set toolbar-state 
------------------------------------------------------------------------------*/
DEF VAR cAvailWidgets  AS CHAR   NO-UNDO.
DEF VAR hWidget        AS HANDLE NO-UNDO.
DEF VAR hCurrObject    AS HANDLE NO-UNDO.
DEF VAR cTimeInputFlds AS CHAR   NO-UNDO.

IF CAN-QUERY(hCurrWidget,"READ-ONLY") AND hCurrWidget:READ-ONLY THEN RETURN.

DEF BUFFER ttObjectLink FOR ttObjectLink.

ASSIGN hCurrObject = ttObject.hObject
       cTimeInputFlds = DYNAMIC-FUNCTION("getAttribute",hCurrObject,"timeInputFields").

IF CAN-DO(cTimeInputFlds,hCurrWidget:NAME) THEN DO:
  CASE hCurrWidget:CURSOR-OFFSET: 
    WHEN 1 THEN IF CHR(LASTKEY) > '2' THEN bMyReturnNoApply = YES.
    WHEN 2 THEN IF CHR(LASTKEY) > '4' AND SUBSTR(hCurrWidget:SCREEN-VALUE,1,1) = "2" THEN bMyReturnNoApply = YES.
    WHEN 3 OR WHEN 4 THEN
      IF (SUBSTR(hCurrWidget:SCREEN-VALUE,1,2) = "24" AND CHR(LASTKEY) > '0') OR
         CHR(LASTKEY) > '5' THEN bMyReturnNoApply = YES.
    WHEN 5 OR WHEN 6 THEN IF SUBSTR(hCurrWidget:SCREEN-VALUE,1,2) = "24" AND CHR(LASTKEY) > '0' THEN bMyReturnNoApply = YES.
  END.
  IF bMyReturnNoApply THEN DO:
    ASSIGN hWidgetEnter  = hCurrWidget
           iCursorOffset = hCurrWidget:CURSOR-OFFSET. 
    RETURN.
  END.
END.

FOR EACH ttObjectLink
   WHERE ttObjectLink.hFromObject = hCurrObject
     AND ttObjectLink.cLinkType = "toolbar":
  IF NOT CAN-DO("new,modified",DYNAMIC-FUNCTION("getToolBarState",ttObjectLink.hToObject)) THEN DO:
    cAvailWidgets = getAttribute(ttObject.hObject,"recordavailwidgets").
    DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"modified").
    
    DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
      hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
      IF VALID-HANDLE(hWidget) THEN
        hWidget:SENSITIVE = FALSE.
    END.
  END.
END.

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AnyPrintableSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnyPrintableSortSearch Procedure 
PROCEDURE AnyPrintableSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY LASTKEY.

bApplyLastkey = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApplyBrowseFillInLookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyBrowseFillInLookup Procedure 
PROCEDURE ApplyBrowseFillInLookup :
/*------------------------------------------------------------------------------
  Purpose:     Check if a lookup event is defined for the overlay and if so, apply it
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST ttEvent 
     WHERE ttEvent.hObject = ttObject.hObject
       AND ttEvent.cMethod = "BrowseColumnLookup"
     NO-ERROR.

IF NOT AVAIL ttEvent THEN
  FIND FIRST ttEvent 
       WHERE ttEvent.hObject = ttObject.hObject
         AND ttEvent.cMethod = "BrowseDateLookup"
       NO-ERROR.

IF AVAIL ttEvent THEN
  APPLY "choose" TO ttEvent.hWidget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackSpaceSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackSpaceSortSearch Procedure 
PROCEDURE BackSpaceSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY LASTKEY.

bApplyLastkey = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackTabFromBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackTabFromBrowse Procedure 
PROCEDURE BackTabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTabChainOverlays AS CHAR   NO-UNDO.
DEF VAR hWidget           AS HANDLE NO-UNDO.

hWidgetEnter = ?.

IF getAttribute(ttObject.hObject,"prevTabItem") NE "" THEN 
  hWidgetEnter = WIDGET-HANDLE(getAttribute(ttObject.hObject,"prevTabItem")) NO-ERROR.

IF NOT VALID-HANDLE(hWidgetEnter) THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse-search-field"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN
    hWidgetEnter = ttObjectLink.hToObject.
  ELSE IF getAttribute(ttObject.hObject,"tabchainoverlays") NE "" 
       AND getAttribute(ttObject.hObject,"currentrowid") = getAttribute(ttObject.hObject,"lastrowid") THEN DO:
    cTabChainOverlays = getAttribute(ttObject.hObject,"tabchainoverlays").
    DO ix = NUM-ENTRIES(cTabChainOverlays) TO 1 BY -1:
      hWidget = WIDGET-HANDLE(ENTRY(ix,cTabChainOverlays)) NO-ERROR.
      IF VALID-HANDLE(hWidget) AND hWidget:VISIBLE THEN DO:
        hWidgetEnter = hWidget.
        LEAVE.
      END.
    END.
  END.
END.

IF NOT VALID-HANDLE(hWidgetEnter) THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    FOR EACH ttEvent
        WHERE ttEvent.hObject = ttObjectLink.hToObject:
      IF ttEvent.hWidget:SENSITIVE AND CAN-QUERY(ttEvent.hWidget,"tab-stop") AND ttEvent.hWidget:TAB-STOP THEN DO:
        hWidgetEnter = ttEvent.hWidget.
        LEAVE.
      END.
    END.
  END.
END.
IF NOT VALID-HANDLE(hWidgetEnter) THEN DO ix = 1 TO 10:
  hWidget = ttObject.hDesignObject:PREV-SIBLING NO-ERROR.
  IF VALID-HANDLE(hWidget) AND CAN-DO(cInputWidgetTypes,hWidget:TYPE) THEN DO:
    IF NOT CAN-QUERY(hWidget,"tab-stop") OR (CAN-QUERY(hWidget,"tab-stop") AND NOT hWidget:TAB-STOP) THEN NEXT.
    hWidgetEnter = hWidget.
    LEAVE.
  END.
END.

IF NOT VALID-HANDLE(hWidgetEnter) THEN
  hWidgetEnter = ttObject.hObject.     

bMyReturnNoApply = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackTabFromSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackTabFromSortSearch Procedure 
PROCEDURE BackTabFromSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
bMyReturnNoApply = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackTabOfWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackTabOfWidget Procedure 
PROCEDURE BackTabOfWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* MESSAGE hCurrWidget:NAME                */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BrowseColumnLookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseColumnLookup Procedure 
PROCEDURE BrowseColumnLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the lookup overlay-fillin
------------------------------------------------------------------------------*/
DEF VAR cInOut              AS CHAR   NO-UNDO.
DEF VAR hCurrObject         AS HANDLE NO-UNDO.
DEF VAR cReadOnlyOnReturn   AS CHAR   NO-UNDO.
DEF VAR cStayInFieldOnEnter AS CHAR   NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hTmpSourceProc      AS HANDLE NO-UNDO.
DEF VAR cFieldName          AS CHAR   NO-UNDO.

ASSIGN hCurrObject         = ttObject.hObject
       cInOut              = getAttribute(ttObject.hObject,"lookupattributes")
       hTmpSourceProc      = hCurrSourceProc
       hBrowse             = DYNAMIC-FUNCTION("getLinkedObject",hCurrObject,"browse","from")
       cReadOnlyOnReturn   = getAttribute(hBrowse,"setReadOnlyOnReturn")
       cStayInFieldOnEnter = getAttribute(hBrowse,"stayInFieldOnEnter")
       cFieldName          = hCurrObject:NAME
       .

setAttribute(hBrowse,"setReadOnlyOnReturn","").
setAttribute(hBrowse,"stayInFieldOnEnter","yes").
setAttribute(hCurrObject,"lastlookupreturnvalues","").

RUN JBoxDLookup.w (getAttribute(ttObject.hObject,"viewbuffersandfields"),getAttribute(ttObject.hObject,"querycriteria"),
                  INPUT-OUTPUT cInOut).

IF NOT VALID-HANDLE(hCurrObject) THEN
  hCurrObject = DYNAMIC-FUNCTION("getLinkedObjectByInfo",hBrowse,"browseOverlay",cFieldName).

setCurrentObject(hCurrObject).

IF cInOut NE "" THEN DO:
  hCurrObject:SCREEN-VALUE = ENTRY(1,cInOut,"|").
  setAttribute(hCurrObject,"lastlookupreturnvalues",cInOut).
  
  IF NOT VALID-HANDLE(hCurrSourceProc) THEN
    hCurrSourceProc = hTmpSourceProc.
  IF VALID-HANDLE(hCurrSourceProc) AND CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraBrowseColumnLookup") THEN
    RUN ExtraBrowseColumnLookup IN hCurrSourceProc (hCurrObject).

  setAttribute(hCurrObject,"last-event","tab").
  APPLY "tab" TO hCurrObject.  

  IF VALID-HANDLE(hCurrObject) THEN
    APPLY "entry" TO hCurrObject.

END.

setAttribute(hBrowse,"setReadOnlyOnReturn",cReadOnlyOnReturn).
setAttribute(hBrowse,"stayInFieldOnEnter",cStayInFieldOnEnter).

IF VALID-HANDLE(hCurrObject) THEN
  hCurrObject:WINDOW:MOVE-TO-TOP().
ELSE IF VALID-HANDLE(hCurrSourceProc) THEN
  hCurrSourceProc:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BrowseConfigRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseConfigRecord Procedure 
PROCEDURE BrowseConfigRecord :
/*------------------------------------------------------------------------------
  Purpose:     Pull up field selector for selecting browse columns to view
  Parameters:  <none>
  Notes:       Current object is the browse. Must navigate to the browse object
------------------------------------------------------------------------------*/
DEF VAR hConfig         AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR hCurrProc       AS HANDLE NO-UNDO.
DEF VAR cConfigWin      AS CHAR   NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hButton         AS HANDLE NO-UNDO.

IF ttObject.cObjectType NE "browse" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "browse" 
       NO-ERROR.
  
  IF NOT AVAIL ttObjectLink THEN RETURN.
  ELSE hBrowse = ttObjectLink.hToObject.

  IF hCurrWidget:TYPE = "menu-item" THEN
    hButton = DYNAMIC-FUNCTION("getEventWidget",ttObject.hObject,"browseconfig","button").
  ELSE hButton = hCurrWidget.
END.
ELSE DO:
  hBrowse = ttObject.hObject.
  hButton = DYNAMIC-FUNCTION("getEventWidget",DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"toolbar","from"),"browseconfig","button").
END.

hCurrObject     = ttObject.hObject.
IF NOT VALID-HANDLE(hCurrSourceProc) THEN
  hCurrProc = ttObject.hSourceProc.
ELSE hCurrProc = hCurrSourceProc.

hConfig = WIDGET-HANDLE(getAttribute(hCurrObject,"browseconfighandle")) NO-ERROR.

IF NOT VALID-HANDLE(hConfig) THEN DO:
  cConfigWin = getAttribute(hCurrObject,"browseconfigwindow").
  IF cConfigWin = "" THEN cConfigWin = "JBoxBrowseConfig.w".
  RUN VALUE(cConfigWin) PERSIST SET hConfig 
      (hCurrProc,
       hBrowse,
       hButton). 
  
  setAttribute(hCurrObject,"browseconfighandle",STRING(hConfig)).
END.
RUN MoveToTop IN hConfig.

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BrowseDateLookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseDateLookup Procedure 
PROCEDURE BrowseDateLookup :
/*------------------------------------------------------------------------------
  Purpose:    View calendar lookup in browse column 
  Parameters:  <none>
  Notes:      Current object is the overlay fill-in. 
------------------------------------------------------------------------------*/
DEF VAR hFillIn AS HANDLE NO-UNDO.
DEFINE VARIABLE hWin AS HANDLE NO-UNDO.

hFillIn = ttObject.hObject.
hWin = hFillIn:WINDOW.

RUN Cal.w (ttObject.hObject).

IF VALID-HANDLE(hFillIn) THEN DO:
  setAttribute(hFillIn,"lastlookupreturnvalues",hFillIn:SCREEN-VALUE).
  setWidgetEnter(hFillIn).
  APPLY "tab" TO hFillIn.  
    
END.
DYNAMIC-FUNCTION("DoLockWindow",?).  
hWin:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BrowseDateLookupDialog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseDateLookupDialog Procedure
PROCEDURE BrowseDateLookupDialog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR hFillIn AS HANDLE NO-UNDO.

hFillIn = ttObject.hObject.

RUN dCal.w (ttObject.hObject).

setAttribute(hFillIn,"lastlookupreturnvalues",hFillIn:SCREEN-VALUE).
setWidgetEnter(hFillIn).
APPLY "tab" TO hFillIn.  

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-ClearDateTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearDateTime Procedure 
PROCEDURE ClearDateTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hCurrWidget:SCREEN-VALUE = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearFilterRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilterRecord Procedure 
PROCEDURE ClearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hWidget       AS HANDLE NO-UNDO.

IF ttObject.cObjectType = "toolbar" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "filter" NO-ERROR.
  IF AVAIL ttObjectLink THEN
    FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN.
END.
IF NOT ttObject.cObjectType = "filter" THEN
  RETURN.
                                             
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse" NO-ERROR.
IF NOT AVAIL ttObjectLink THEN
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "query" NO-ERROR.

IF AVAIL ttObjectLink THEN DO:
  FOR EACH bttAttribute NO-LOCK OF ttObject
      WHERE bttAttribute.cName BEGINS "filterfield_":
  
    hWidget = WIDGET-HANDLE(getAttribute(ttObject.hObject,bttAttribute.cName)).

    setAttribute(IF ttObjectLink.hToObject:TYPE = "browse" THEN ttObjectLink.hToObject:QUERY ELSE ttObjectLink.hToObject,
                 "filtervalue_" + hWidget:NAME,"").
    hWidget:SCREEN-VALUE = "".
    IF hWidget:TYPE = "combo-box" THEN
      hWidget:SCREEN-VALUE = " " NO-ERROR.
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
DEFINE VAR bFormClosed AS LOG NO-UNDO.
IF VALID-HANDLE(hCurrSourceProc) THEN DO:
  PUBLISH "CloseForm"  (hCurrSourceProc,OUTPUT bFormClosed).
  IF NOT bFormClosed THEN
    APPLY "CLOSE" TO hCurrSourceProc.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CommitRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CommitRecord Procedure 
PROCEDURE CommitRecord :
/*------------------------------------------------------------------------------
  Purpose:    NOT IN USE: brynjar: 22.05.08  
  Parameters:  <none>
  Notes:      Current object is the toolbar 
------------------------------------------------------------------------------
DEF VAR hCurrObject  AS HANDLE NO-UNDO.
DEF VAR cInputFields AS CHAR NO-UNDO.
DEF VAR cInputValues AS CHAR NO-UNDO.
DEF VAR cRepos       AS CHAR NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
                                           
hCurrObject = ttObject.hObject.

RUN SaveRecord.

IF NOT CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraCommitRecord") THEN DO:
  DYNAMIC-FUNCTION("DoCommit",TRUE).
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = hCurrObject
         AND ttObjectLink.cLinkType   = "browse"
       NO-ERROR.
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = hCurrObject
         AND bttObjectLink.cLinkType   = "fieldMap"
       NO-ERROR.
  IF AVAIL ttObjectLink AND AVAIL bttObjectLink THEN DO:
    ASSIGN hBuffer = bttObjectLink.hToObject
           hBrowse = ttObjectLink.hToObject
           cInputFields = getAttribute(hBuffer,"bufferUpdateFields")
           .
    ttObjectLink.hToObject:QUERY:GET-FIRST().
    REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
      cInputValues = "".
      DO ix = 1 TO NUM-ENTRIES(cInputFields):
        cInputValues = cInputValues + STRING(hBuffer:BUFFER-FIELD(ENTRY(ix,cInputFields)):BUFFER-VALUE) + "|". 
      END.
      DYNAMIC-FUNCTION("DoCreate",hBuffer:NAME,
                    IF getAttribute(hBuffer,"customUpdateValProc") NE "" THEN
                      LEFT-TRIM(RIGHT-TRIM(getAttribute(hBuffer,"customUpdateValProc") + ",=" +
                                           getAttribute(hBuffer,"customCreateProc"),",="),",")
                    ELSE IF getAttribute(hBuffer,"customCreateProc") NE "" THEN
                      ",=" + getAttribute(hBuffer,"customCreateProc")
                    ELSE "",
                    cInputFields,
                    SUBSTR(cInputValues,1,LENGTH(cInputValues) - 1),
                    FALSE).
      hBrowse:QUERY:GET-NEXT().
    END.
    DYNAMIC-FUNCTION("DoCommit",TRUE).
    hBrowse:QUERY:GET-FIRST().
    REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
      cInputValues = "".
      DO ix = 1 TO NUM-ENTRIES(cInputFields):
        cInputValues = cInputValues + STRING(hBuffer:BUFFER-FIELD(ENTRY(ix,cInputFields)):BUFFER-VALUE) + "|". 
      END.
      DYNAMIC-FUNCTION("DoRefetchTrans",hBuffer,cInputFields,SUBSTR(cInputValues,1,LENGTH(cInputValues) - 1)).
      hBrowse:QUERY:GET-NEXT().
    END.
    hBrowse:QUERY:GET-FIRST().
    hBrowse:REFRESH().

    setAttribute(hBrowse,"uselocaldata","").
    setAttribute(hBuffer,"uselocaldata","").

    setAttribute(hCurrObject,"commitstate","off").
    DYNAMIC-FUNCTION("setToolbar",hCurrObject,"avail").

  END.
  ELSE DoMessage(0,0,"Commit handlinge requires both a browse and a fieldmap object" + CHR(10)
                + "Caller: " + PROGRAM-NAME(2) + CHR(10)
                + "Function: " + PROGRAM-NAME(1),
                "JukeBox programmers error","").
END.
ELSE DO:
  RUN ExtraCommitRecord IN hCurrSourceProc (OUTPUT bOK).
  IF bOK THEN DO:
    setCurrentObject(hCurrObject).
    /* Deletes the temporary data in the browse, resets transaction attributes on toolbar fieldmap and browse, and opens the query. 
       This can be done with good consious since the data are now committed */
    RUN RollbackRecord.
  END.
END.

setCurrentObject(hCurrObject).
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyBrowseRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyBrowseRows Procedure
PROCEDURE CopyBrowseRowsRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

IF ttObject.hObject:TYPE NE "browse" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN 
    DYNAMIC-FUNCTION("CopyBrowseToClipboard",ttObjectLink.hToObject).
END.
ELSE    
  DYNAMIC-FUNCTION("CopyBrowseToClipboard",ttObject.hObject).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-CopyRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord Procedure 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object is Toolbar (or menu) or browse (rigth-click)
              NB! The toolbar must exist even if the function is invoked by browse right-click (for now..)
------------------------------------------------------------------------------*/
DEF VAR cAvailWidgets       AS CHAR   NO-UNDO.
DEF VAR hWidget             AS HANDLE NO-UNDO.
DEF VAR hCurrObject         AS HANDLE NO-UNDO.
DEF VAR cWidgets            AS CHAR   NO-UNDO. 
DEF VAR hBuffer             AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR cBufferValues       AS CHAR   NO-UNDO.
DEF VAR cBufferFields       AS CHAR   NO-UNDO.
DEF VAR rRepos              AS ROWID  NO-UNDO.
DEF VAR cCopyHandles        AS CHAR   NO-UNDO.
DEF VAR hCopyWidget         AS HANDLE NO-UNDO.
DEF VAR bInsertBrowseRow    AS LOG    NO-UNDO.
DEF VAR cUpdateValProc      AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateFields  AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateValues  AS CHAR   NO-UNDO.
DEF VAR iCurrRow            AS INT    NO-UNDO.
DEF VAR cExcludeCopyFields  AS CHAR   NO-UNDO.
DEF VAR cModBufferFields    AS CHAR   NO-UNDO.


IF NOT DYNAMIC-FUNCTION("getIsEventAllowed",ttObject.hObject,"copy") THEN RETURN.

IF ttObject.hObject:TYPE = "browse" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    ASSIGN cCopyHandles = DYNAMIC-FUNCTION("getToolBarHandles",ttObjectLink.hToObject,"*copy*")
           hCopyWidget  = WIDGET-HANDLE(ENTRY(1,cCopyHandles)) NO-ERROR.
    IF VALID-HANDLE(hCopyWidget) AND hCopyWidget:SENSITIVE THEN
      FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject.
    ELSE RETURN.
  END.
END.
ELSE DO:
  IF VALID-HANDLE(hCurrWidget) AND CAN-DO("button,menu-item",hCurrWidget:TYPE) THEN
    hCopyWidget = hCurrWidget. 
  ELSE
    ASSIGN cCopyHandles = DYNAMIC-FUNCTION("getToolBarHandles",ttObject.hObject,"*copy*")
           hCopyWidget  = WIDGET-HANDLE(ENTRY(1,cCopyHandles)) NO-ERROR.
END. 

IF VALID-HANDLE(hCopyWidget) AND NOT hCopyWidget:SENSITIVE THEN RETURN.
ELSE IF NOT VALID-HANDLE(hCopyWidget) THEN RETURN.

hCurrObject = ttObject.hObject.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject 
       AND ttObjectLink.cLinkType   = "browse" 
     NO-ERROR.
IF AVAIL ttObjectLink THEN 
  bInsertBrowseRow = getAttribute(ttObjectLink.hToObject,"insertbrowserow") = "yes".

FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "fieldmap"
     NO-ERROR.

IF AVAIL ttObjectLink AND NOT bInsertBrowseRow THEN DO:
  DYNAMIC-FUNCTION("setToolbar",ttObject.hObject,"new").
  setAttribute(ttObject.hObject,"copyRecord","yes").
  cAvailWidgets = getAttribute(ttObjectLink.hToObject,"recordavailwidgets").
  DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
   hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
   IF VALID-HANDLE(hWidget) THEN
     hWidget:SENSITIVE = FALSE.
  END.

  FIND bttObject WHERE bttObject.hObject = ttObjectLink.hToObject.
  IF bttObject.cObjectType = "fieldmap" THEN DO:
    cWidgets = getAttribute(bttObject.hObject,"primaryKeyWidgets").
    IF cWidgets NE "" THEN 
      hWidget = WIDGET-HANDLE(ENTRY(1,cWidgets)) NO-ERROR.
    ELSE DO:
      cWidgets = TRIM(getAttribute(bttObject.hObject,"ScreenUpdateWidgets") + "," + 
                      (IF getAttribute(bttObject.hObject,"ExtraUpdateWidgets") NE "" THEN
                         getAttribute(bttObject.hObject,"ExtraUpdateWidgets") + ","
                       ELSE ""),",").
    
      hWidget = WIDGET-HANDLE(ENTRY(1,cWidgets)) NO-ERROR.
    END.  
    IF VALID-HANDLE(hWidget) THEN
      APPLY "entry" TO hWidget.
  END.
END.
ELSE DO:
  FIND FIRST ttObjectLink 
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse"
       NO-ERROR.

  /* Insert record to the browse: */
  IF AVAIL ttObjectLink THEN DO:
    ASSIGN hBrowse       = ttObjectLink.hToObject
           hBuffer       = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
           cBufferFields      = getAttribute(hBrowse,"basetablefields")
           cExcludeCopyFields = getAttribute(hBrowse,"excludeCopyFields").

    DO ix = 1 TO NUM-ENTRIES(cBufferFields):
      IF NOT CAN-DO(cExcludeCopyFields,ENTRY(ix,cBufferFields)) THEN
        cBufferValues = cBufferValues + (IF cBufferValues NE "" THEN "|" ELSE "") +
                        (IF hBuffer:BUFFER-FIELD(ENTRY(ix,cBufferFields)):BUFFER-VALUE NE ? THEN 
                           STRING(hBuffer:BUFFER-FIELD(ENTRY(ix,cBufferFields)):BUFFER-VALUE)
                         ELSE "").
    END.

    DYNAMIC-FUNCTION("setPostUpdProc",IF getAttribute(hBuffer,"postUpdateProc") NE "" THEN
                                        getAttribute(hBuffer,"postUpdateProc")
                                      ELSE getAttribute(hBrowse,"postUpdateProc")).

    ASSIGN cUpdateValProc = IF getAttribute(hBrowse,"customUpdateValProc") NE "" THEN 
                              LEFT-TRIM(RIGHT-TRIM(getAttribute(hBrowse,"customUpdateValProc") + ",=" +
                                                   getAttribute(hBrowse,"customCreateProc"),",="),",")

                            ELSE IF getAttribute(hBuffer,"customUpdateValProc") NE "" THEN
                              LEFT-TRIM(RIGHT-TRIM(getAttribute(hBuffer,"customUpdateValProc") + ",=" +
                                                   getAttribute(hBuffer,"customCreateProc"),",="),",")

                            ELSE IF getAttribute(hBrowse,"customCreateProc") NE "" THEN
                              ",=" + getAttribute(hBrowse,"customCreateProc")

                            ELSE IF getAttribute(hBuffer,"customCreateProc") NE "" THEN
                              ",=" + getAttribute(hBuffer,"customCreateProc")

                            ELSE ""

           cExtraUpdateFields = TRIM(getAttribute(hBrowse,"childlinkfields") + "," + 
                            (IF getAttribute(hBrowse,"bufferextrafields") NE "" THEN 
                               getAttribute(hBrowse,"bufferextrafields")
                             ELSE getAttribute(hBuffer,"bufferextrafields")),",")
           cExtraUpdateValues = TRIM(DYNAMIC-FUNCTION("getParentLinkValues",hBrowse) + "|" + 
                            (IF getAttribute(hBrowse,"bufferextravalues") NE "" THEN 
                               getAttribute(hBrowse,"bufferextravalues")
                             ELSE getAttribute(hBuffer,"bufferextravalues")),",")
                            .
    DO ix = 1 TO NUM-ENTRIES(cBufferFields):
      IF NOT CAN-DO(cExcludeCopyFields,ENTRY(ix,cBufferFields)) THEN DO:
        IF CAN-DO(cExtraUpdateFields,ENTRY(ix,cBufferFields)) THEN
          ENTRY(ix,cBufferValues,"|") = ENTRY(LOOKUP(ENTRY(ix,cBufferFields),cExtraUpdateFields),cExtraUpdateValues,"|").
        cModBufferFields = cModBufferFields + (IF cModBufferFields NE "" THEN "," ELSE "") + ENTRY(ix,cBufferFields).
      END.
    END.

    IF DYNAMIC-FUNCTION("DoCreate",hBuffer:NAME,
                  cUpdateValProc,
                  TRIM(cModBufferFields + "," + cExtraUpdateFields,","),
                  TRIM(cBufferValues + "|" + cExtraUpdateValues,"|"),
                  TRUE) THEN DO:

      IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraCopyRecord") THEN DO:
        RUN ExtraCopyRecord IN hCurrSourceProc (hBuffer,OUTPUT bOk).
        IF NOT bOk THEN RETURN.
      END.
  
      hBuffer:BUFFER-CREATE.
  
      iCurrRow = hBrowse:FOCUSED-ROW.

      rRepos = hBuffer:ROWID.
      DYNAMIC-FUNCTION("DoRefetchTrans",hBuffer,"FIRST","").
      DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
/*       DYNAMIC-FUNCTION("refreshRow",hBrowse,                                         */
/*                         DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields"), */
/*                         DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryjoin")).       */
  
      hBrowse:QUERY:QUERY-OPEN().
      hBrowse:SET-REPOSITIONED-ROW(iCurrRow + 1,"conditional").
/*       hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional"). */
      hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
  
      IF getAttribute(hBrowse,"enableondblclick") = "yes" OR getAttribute(hBrowse,"enableOnToolbarClick") = "yes" THEN 
        setAttribute(hBrowse,"enableupdate","yes").

      APPLY "value-changed" TO hBrowse.
      IF hBrowse:MULTIPLE THEN
        hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).

      setAttribute(hBrowse,"rowsadded",STRING(INT(getAttribute(hBrowse,"rowsadded")) + 1)).
      DYNAMIC-FUNCTION("ViewRecordCount",hBrowse).
    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setToolbar",hBuffer,"new").
    FIND FIRST ttObjectLink 
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query"
         NO-ERROR.
  END.
END.
IF getAttribute(hCurrObject,"CloseAndDisableChilds") = "" THEN
  CloseAndDisableChilds(hCurrObject).

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlBackTabSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlBackTabSortSearch Procedure 
PROCEDURE CtrlBackTabSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "PrevTab" (hCurrSourceProc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlCursorLeftBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlCursorLeftBrowse Procedure 
PROCEDURE CtrlCursorLeftBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCol        AS HANDLE NO-UNDO.
DEF VAR hCurrColumn AS HANDLE NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

hBrowse = ttObject.hObject.

EMPTY TEMP-TABLE ttBrowseColumns.

DO ix = 1 TO hBrowse:NUM-COLUMNS:
  hCol = hBrowse:GET-BROWSE-COLUMN(ix).
  IF CAN-DO(getAttribute(hBrowse,"NoColumnSort"),hCol:NAME) OR NOT hCol:VISIBLE THEN NEXT.
  IF hCol:NAME = getAttribute(hBrowse,"localsort") THEN
    hCurrColumn = hCol.
  CREATE ttBrowseColumns.
  ASSIGN ttBrowseColumns.hColumn = hCol
         ttBrowseColumns.iPos    = (IF hCol:X = -1 THEN -10000 + ix ELSE hCol:X)
         .
END.

IF NOT VALID-HANDLE(hCurrColumn) THEN DO:
  FIND LAST ttBrowseColumns NO-ERROR.
  IF AVAIL ttBrowseColumns THEN
    hCurrColumn = ttBrowseColumns.hColumn.  
END.
ELSE DO:
  FIND LAST ttBrowseColumns
       WHERE ttBrowseColumns.iPos < hCurrColumn:X
       NO-ERROR.
  IF AVAIL ttBrowseColumns THEN DO:
    hCurrColumn = ttBrowseColumns.hColumn.
    IF hCurrColumn:X < hBrowse:WIDTH-PIXELS THEN DO:
      ix = 0.
      REPEAT WHILE hCurrColumn:X < 5 AND ix < 50:
        APPLY "cursor-left" TO hBrowse.
        ix = ix + 1.
      END.
    END.
  END.
  ELSE DO:
    FIND LAST ttBrowseColumns NO-ERROR.
    IF AVAIL ttBrowseColumns THEN DO:
      ASSIGN hCurrColumn = ttBrowseColumns.hColumn
             ix          = 0.     
      REPEAT WHILE hCurrColumn:X + hCurrColumn:WIDTH-PIXELS > hBrowse:WIDTH-PIXELS AND hCurrColumn:X > 5 AND ix < 50:
        APPLY "cursor-right" TO hBrowse.
        ix = ix + 50.
      END.
    END.
  END.
END.


IF VALID-HANDLE(hCurrColumn) THEN DO:
  hCursorCurrColumn = hCurrColumn.
  RUN StartSearch.
  hCursorCurrColumn = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlCursorLeftSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlCursorLeftSortSearch Procedure 
PROCEDURE CtrlCursorLeftSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hBrowse AS HANDLE NO-UNDO.

hBrowse = DYNAMIC-FUNCTION("GetLinkedObject",ttObject.hObject,"browse","from").
IF VALID-HANDLE(hBrowse) THEN
  APPLY "ctrl-cursor-left" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlCursorRightBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlCursorRightBrowse Procedure 
PROCEDURE CtrlCursorRightBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCol        AS HANDLE NO-UNDO.
DEF VAR hCurrColumn AS HANDLE NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

hBrowse = ttObject.hObject.

EMPTY TEMP-TABLE ttBrowseColumns.

DO ix = 1 TO hBrowse:NUM-COLUMNS:
  hCol = hBrowse:GET-BROWSE-COLUMN(ix).
  IF CAN-DO(getAttribute(hBrowse,"NoColumnSort"),hCol:NAME) OR NOT hCol:VISIBLE THEN NEXT.
  IF hCol:NAME = getAttribute(hBrowse,"localsort") THEN
    hCurrColumn = hCol.
  CREATE ttBrowseColumns.
  ASSIGN ttBrowseColumns.hColumn = hCol
         ttBrowseColumns.iPos    = (IF hCol:X = -1 THEN -10000 + ix ELSE hCol:X)
         .
END.

IF NOT VALID-HANDLE(hCurrColumn) THEN DO:
  FIND FIRST ttBrowseColumns NO-ERROR.
  IF AVAIL ttBrowseColumns THEN
    hCurrColumn = ttBrowseColumns.hColumn.  
END.
ELSE DO:
  FIND FIRST ttBrowseColumns
       WHERE ttBrowseColumns.iPos > hCurrColumn:X
       NO-ERROR.
  IF AVAIL ttBrowseColumns THEN DO:
    hCurrColumn = ttBrowseColumns.hColumn.
    IF hCurrColumn:X + hCurrColumn:WIDTH-PIXELS > hBrowse:WIDTH-PIXELS THEN DO:
      ix = 0.
      REPEAT WHILE hCurrColumn:X + hCurrColumn:WIDTH-PIXELS > hBrowse:WIDTH-PIXELS AND hCurrColumn:X > 5 AND ix < 50 /* - MIN(hCurrColumn:WIDTH-PIXELS,hBrowse:WIDTH-PIXELS) */:
        APPLY "cursor-right" TO hBrowse.
        ix = ix + 1.
      END.
      IF hCurrColumn:X + hCurrColumn:WIDTH-PIXELS + 30 > hBrowse:WIDTH-PIXELS AND hCurrColumn:WIDTH-PIXELS < hBrowse:WIDTH-PIXELS - 50 THEN
        DO ix = 1 TO 4:
          APPLY "cursor-right" TO hBrowse.
        END.
    END.
  END.
  ELSE DO:
    FIND FIRST ttBrowseColumns NO-ERROR.
    IF AVAIL ttBrowseColumns THEN DO:
      hCurrColumn = ttBrowseColumns.hColumn.     
      REPEAT WHILE hCurrColumn:X < 0:
        APPLY "cursor-left" TO hBrowse.
      END.
    END.
  END.
END.

IF VALID-HANDLE(hCurrColumn) THEN DO:
  hCursorCurrColumn = hCurrColumn.
  RUN StartSearch.
  hCursorCurrColumn = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlCursorRightSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlCursorRightSortSearch Procedure 
PROCEDURE CtrlCursorRightSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hBrowse AS HANDLE NO-UNDO.

hBrowse = DYNAMIC-FUNCTION("GetLinkedObject",ttObject.hObject,"browse","from").
IF VALID-HANDLE(hBrowse) THEN
  APPLY "ctrl-cursor-right" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CtrlTabSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlTabSortSearch Procedure 
PROCEDURE CtrlTabSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "NextTab" (hCurrSourceProc).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DefaultActionBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse Procedure 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:    Enable update on browse if the attribute enableondblclick or enableonreturn is set
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iMousePos         AS INT    NO-UNDO.
DEF VAR cOverlayList      AS CHAR   NO-UNDO.
DEF VAR hOverlay          AS HANDLE NO-UNDO.
DEF VAR hCurrOverlay      AS HANDLE NO-UNDO.
DEF VAR hEnterHere        AS HANDLE NO-UNDO.
DEF VAR bClickedOnOverlay AS LOG    NO-UNDO.
DEF VAR hCurrObject       AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.

IF getAttribute(hCurrObject,"enableondblclick") = "yes" OR getAttribute(hCurrObject,"enableonreturn") = "yes" THEN DO:
  setAttribute(hCurrObject,"enableupdate","yes").

  DoLockWindow(hCurrObject:WINDOW).

  hCurrOverlay = WIDGET-HANDLE(getAttribute(hCurrObject,"currentoverlaywidget")) NO-ERROR.

  RUN EndResizeBrowseColumn.

  cOverlayList = getAttribute(hCurrObject,"tabchainoverlays").

  IF LAST-EVENT:LABEL = "enter" 
     AND getAttribute(hCurrObject,"enableonreturn") NE "no" 
     THEN DO:
    IF VALID-HANDLE(hCurrOverlay) THEN APPLY "entry" TO hCurrOverlay.
    ELSE DO:
      hEnterHere = WIDGET-HANDLE(ENTRY(1,cOverlayList)) NO-ERROR.
      IF VALID-HANDLE(hEnterHere) THEN DO: 
        APPLY "entry" TO hEnterHere.
        setAttribute(hCurrObject,"currentoverlaywidget",STRING(hEnterHere)).
      END.
    END.
    setAttribute(hCurrObject,"doubleclickenabledfield","").
  END.
  ELSE IF LAST-EVENT:LABEL NE "enter" THEN DO:
    iMousePos = DYNAMIC-FUNCTION("getMousePosition",hCurrObject:FRAME,"x").
  
    DO ix = 1 TO NUM-ENTRIES(cOverlayList):
      hOverlay = WIDGET-HANDLE(ENTRY(ix,cOverlayList)) NO-ERROR.
      IF VALID-HANDLE(hOverlay) AND hOverlay:X > iMousePos AND NOT VALID-HANDLE(hEnterHere) THEN 
        hEnterHere = WIDGET-HANDLE(ENTRY((IF ix > 1 THEN ix - 1 ELSE 1),cOverlayList)) NO-ERROR.
      ELSE IF VALID-HANDLE(hOverlay) AND hOverlay:X LE iMousePos AND hOverlay:X + hOverlay:WIDTH-PIXELS GE iMousePos THEN
        bClickedOnOverlay = YES.
    END.
    IF NOT bClickedOnOverlay THEN DO:
      setAttribute(hCurrObject,"doubleclickenabledfield",ENTRY(1,cOverlayList)).
      setAttribute(hCurrObject,"currentoverlaywidget",ENTRY(1,cOverlayList)).
      IF NOT VALID-HANDLE(hEnterHere) THEN
        hEnterHere = WIDGET-HANDLE(ENTRY(1,cOverlayList)) NO-ERROR.
      IF VALID-HANDLE(hEnterHere) THEN 
        APPLY "entry" TO hEnterHere.
    END.
    ELSE DO:
      IF NOT VALID-HANDLE(hEnterHere) THEN
        hEnterHere = WIDGET-HANDLE(ENTRY(NUM-ENTRIES(cOverlayList),cOverlayList)) NO-ERROR.
      IF VALID-HANDLE(hEnterHere) THEN DO:
        APPLY "entry" TO hEnterHere.
        setAttribute(hCurrObject,"currentoverlaywidget",STRING(hEnterHere)).
      END.
      setAttribute(hCurrObject,"doubleclickenabledfield","").
    END.
  END.

  DoLockWindow(?).
END.
setCurrentObject(hCurrObject).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteCharSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteCharSortSearch Procedure 
PROCEDURE DeleteCharSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY LASTKEY.

bApplyLastkey = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeletePropertyOverlayValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeletePropertyOverlayValue Procedure 
PROCEDURE DeletePropertyOverlayValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF CAN-DO("integer,decimal",ttObject.hObject:DATA-TYPE) AND INT(ttObject.hObject:SCREEN-VALUE) = 0 THEN
  ttObject.hObject:SCREEN-VALUE = "?".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord Procedure 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object is the toolbar (or browse when DELETE-CHARACTER). Must navigate to the buffer object.
              A fieldmap object is not mandatory and the attributes for delete msg, etc 
              may be attached to the browse (or query) object.
              When using the delete-character a toolbar with an enabled delete-button must be linked to the browse      
------------------------------------------------------------------------------*/
DEF VAR cFieldName     AS CHAR   NO-UNDO.
DEF VAR hBuffer        AS HANDLE NO-UNDO.
DEF VAR hBrowse        AS HANDLE NO-UNDO.
DEF VAR hQuery         AS HANDLE NO-UNDO.
DEF VAR bAvailFieldMap AS LOG    NO-UNDO INIT TRUE.
DEF VAR hSrcObject     AS HANDLE NO-UNDO.
DEF VAR hToolbar       AS HANDLE NO-UNDO.
DEF VAR rDelete        AS ROWID  NO-UNDO.
DEF VAR rRepos         AS ROWID  NO-UNDO.
DEF VAR cDeleteHandles AS CHAR   NO-UNDO.
DEF VAR hDeleteWidget  AS HANDLE NO-UNDO.
DEF VAR iNumDeletes    AS INT    NO-UNDO INIT 1.
DEF VAR cDelRowident   AS CHAR   NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR iy             AS INT    NO-UNDO.
DEF VAR cDeleteText    AS CHAR   NO-UNDO.

IF NOT DYNAMIC-FUNCTION("getIsEventAllowed",ttObject.hObject,"delete") THEN RETURN ERROR "error".

IF LAST-EVENT:LABEL = "del" AND ttObject.hObject:TYPE NE "browse" THEN 
  RETURN ERROR.

IF ttObject.hObject:TYPE = "browse" THEN DO:
  ASSIGN hBrowse = ttObject.hObject
         hQuery  = ttObject.hObject:QUERY.
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    IF NOT DYNAMIC-FUNCTION("getIsEventAllowed",ttObjectLink.hToObject,"delete") THEN RETURN ERROR "error".

    ASSIGN cDeleteHandles = DYNAMIC-FUNCTION("getToolBarHandles",ttObjectLink.hToObject,"*delete*")
           hDeleteWidget  = WIDGET-HANDLE(ENTRY(1,cDeleteHandles)).
    IF VALID-HANDLE(hDeleteWidget) AND hDeleteWidget:SENSITIVE THEN
      FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject.
    ELSE DO:
      FIND FIRST ttEvent 
           WHERE ttEvent.hObject = ttObject.hObject
             AND ttEvent.cAction = "delete"
           NO-ERROR.
      IF NOT AVAIL ttEvent THEN
        RETURN ERROR.
      ELSE
        FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject.
    END.
  END.
END.
ELSE IF CAN-DO("button,menu-item",hCurrWidget:TYPE) THEN hDeleteWidget = hCurrWidget.
ELSE 
  ASSIGN cDeleteHandles = DYNAMIC-FUNCTION("getToolBarHandles",ttObject.hObject,"*delete*")
         hDeleteWidget  = WIDGET-HANDLE(ENTRY(1,cDeleteHandles)).

IF (VALID-HANDLE(hDeleteWidget) AND NOT hDeleteWidget:SENSITIVE) OR
   (NOT VALID-HANDLE(hDeleteWidget) AND getAttribute(ttObject.hObject,"allowdeletekey") NE "yes")
   THEN RETURN.

IF CAN-DO("toolbar,panel,popupMenu",ttObject.cObjectType)
/*    ttObject.hObject:TYPE = "rectangle" */
   THEN DO:
  hToolbar = ttObject.hObject.

  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = hToolbar 
         AND ttObjectLink.cLinkType   = "fieldMap" 
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    ASSIGN hBuffer    = ttObjectLink.hToObject
           hSrcObject = hBuffer.
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject 
           AND bttObjectLink.cLinkType   = "browse" 
         NO-ERROR.
    IF AVAIL bttObjectLink THEN
      ASSIGN hQuery  = bttObjectLink.hToObject:QUERY
             hBrowse = bttObjectLink.hToObject
             hBuffer = bttObjectLink.hToObject:QUERY:GET-BUFFER-HANDLE(1)
             .
    ELSE DO:
      FIND FIRST bttObjectLink
           WHERE bttObjectLink.hFromObject = hToolbar 
             AND bttObjectLink.cLinkType   = "browse" 
           NO-ERROR.
      IF AVAIL bttObjectLink THEN
        ASSIGN hQuery  = bttObjectLink.hToObject:QUERY
               hBrowse = bttObjectLink.hToObject
               hBuffer = bttObjectLink.hToObject:QUERY:GET-BUFFER-HANDLE(1)
               .
      ELSE DO:
        FIND FIRST bttObjectLink
             WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject 
               AND bttObjectLink.cLinkType   = "query" 
             NO-ERROR.
        IF AVAIL bttObjectLink THEN
          hQuery = bttObjectLink.hToObject.
      END.
    END.
  END.
  ELSE DO:
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = hToolbar 
           AND ttObjectLink.cLinkType   = "browse" 
         NO-ERROR.
  
    IF AVAIL ttObjectLink THEN
      ASSIGN hBrowse    = ttObjectLink.hToObject
             hQuery     = hBrowse:QUERY
             hBuffer    = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
             hSrcObject = hBrowse
             bAvailFieldMap = FALSE
            .
  
    ELSE DO:
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = hToolbar 
             AND ttObjectLink.cLinkType   = "query" 
           NO-ERROR.
      IF AVAIL ttObjectLink THEN 
        ASSIGN hQuery  = ttObjectLink.hToObject
               hBuffer = hQuery:GET-BUFFER-HANDLE(1)
               hSrcObject = hQuery
               bAvailFieldMap = FALSE
              .
      ELSE RETURN ERROR.
    END.
  END.
END.
ELSE DO:
  hSrcObject = ttObject.hObject.
  IF ttObject.hObject:TYPE = "browse" THEN
    hBuffer    = ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1).
  ELSE
    hBuffer = ttObject.hObject:GET-BUFFER-HANDLE(1).
END.

IF (NOT bAvailFieldMap AND VALID-HANDLE(hBrowse) AND hBrowse:MULTIPLE AND hBrowse:NUM-SELECTED-ROWS > 1 AND getAttribute(hBrowse,"allowmultidelete") NE "no")
   OR (VALID-HANDLE(hBrowse) AND hBrowse:MULTIPLE AND hBrowse:NUM-SELECTED-ROWS > 1 AND getAttribute(hBrowse,"allowmultidelete") = "yes") THEN
  iNumDeletes = hBrowse:NUM-SELECTED-ROWS.

IF hBuffer:AVAIL THEN DO:
  IF getAttribute(hSrcObject,"nodeletewarning") = "" THEN DO:
    IF NOT CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"MyDeleteMessage") THEN DO:
      IF iNumDeletes > 1 THEN
        iReturn = DoMessage(0,4,
                            IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                              "Slett " + STRING(hBrowse:NUM-SELECTED-ROWS) + " forekomster" 
                            ELSE "Delete " + STRING(hBrowse:NUM-SELECTED-ROWS) + " records" 
                           ,"",""). 
      ELSE DO:
        IF VALID-HANDLE(hBrowse) AND NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN DO:
          hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN RETURN.
        END.

        IF getAttribute(hSrcObject,"fieldnamedeletewarning") NE "" THEN 
          iReturn = DoMessage(0,4,
                              IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett forekomst: &1" ELSE "Delete record: &1" 
                              ,"",
                              STRING(hBuffer:BUFFER-FIELD(getAttribute(hSrcObject,"fieldnamedeletewarning")):BUFFER-VALUE)). 
        ELSE DO:
          IF VALID-HANDLE(hBrowse) THEN
            DO ix = 1 TO hBrowse:NUM-COLUMNS:
              IF hBrowse:GET-BROWSE-COLUMN(ix):VISIBLE THEN
                ASSIGN cDeleteText = cDeleteText + hBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE + " - "
                       iy = iy + 1.
              IF iy = 2 THEN LEAVE.
            END.
          ELSE DO:
            DO ix = 1 TO hBuffer:NUM-FIELDS:
              ASSIGN cDeleteText = cDeleteText + STRING(hBuffer:BUFFER-FIELD(ix):BUFFER-VALUE) + " - "
                     iy = iy + 1.
              IF iy = 1 THEN LEAVE. /* For now */
            END.
          END.
          cDeleteText = RIGHT-TRIM(cDeleteText," - ").

          iReturn = DoMessage(0,4,
                              IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett forekomst: &1" ELSE "Delete record: &1" 
                              ,"",
                              cDeleteText). 
        END.
      END.
    END.
    ELSE DO:
      RUN MyDeleteMessage IN hCurrSourceProc (OUTPUT bOk).
      IF bOK THEN iReturn = 6. 
      ELSE iReturn = 0.
    END.
  END.
  ELSE iReturn = 6.
  IF iReturn = 6 THEN DO:
    IF getAttribute(hBrowse,"uselocaldata") NE "yes" AND
       getAttribute(hBuffer,"uselocaldata") NE "yes" AND
       getAttribute(hQuery,"uselocaldata") NE "yes" THEN
      DO ix = 1 TO iNumDeletes:
        IF iNumDeletes > 1 THEN DO: 
          bOk = hBrowse:FETCH-SELECTED-ROW(ix).
          IF bOK THEN cDelRowident = hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.
          ELSE DO:
            DoMessage(0,0,"Error when fetching selected rows. Operation cancelled","","").
            RETURN.
          END.
        END.
        ELSE cDelRowident = hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.
        DYNAMIC-FUNCTION("setPostUpdProc",IF getAttribute(hBuffer,"postUpdateProc") NE "" THEN
                                            getAttribute(hBuffer,"postUpdateProc")
                                          ELSE IF getAttribute(hBrowse,"postUpdateProc") NE "" THEN
                                            getAttribute(hBrowse,"postUpdateProc")
                                          ELSE getAttribute(hQuery,"postUpdateProc")).
        bOk = DYNAMIC-FUNCTION("DoDelete",getAttribute(hBuffer,"dbname") + hBuffer:NAME,
              getAttribute(hSrcObject,"customDeleteValProc"),
              "",
              cDelRowident,
              IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraDeleteRecord") OR iNumDeletes > 1 THEN FALSE ELSE TRUE).
        IF NOT bOk AND NOT (CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraDeleteRecord") OR iNumDeletes > 1) THEN DO:
          DoMessage(0,0,DYNAMIC-FUNCTION("getTransactionMessage"),
                   (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Feil i sletting" ELSE "Error when deleting record"),"").
          RETURN ERROR "error".
        END.
      END.
    ELSE bOk = TRUE.

    IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraDeleteRecord") THEN DO:
      setCurrentObject(hToolbar).
      RUN ExtraDeleteRecord IN hCurrSourceProc (OUTPUT bOk).
    END.
    ELSE IF iNumDeletes > 1 AND
            getAttribute(hBrowse,"uselocaldata") NE "yes" AND
            getAttribute(hBuffer,"uselocaldata") NE "yes" AND
            getAttribute(hQuery,"uselocaldata") NE "yes"
       THEN DO:        
      bOk = DYNAMIC-FUNCTION("DoCommit",NO).
      IF NOT bOk THEN DO:
        DoMessage(0,0,DYNAMIC-FUNCTION("getTransactionMessage"),
                 (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Feil i sletting" ELSE "Error when deleting record"),"").
        RETURN ERROR "error".
      END.
    END. 

    IF bOk THEN DO ix = 1 TO iNumDeletes:

      IF iNumDeletes > 1 THEN
        hBrowse:FETCH-SELECTED-ROW(ix).

      rDelete = hBuffer:ROWID.

      IF ix = iNumDeletes AND hQuery:IS-OPEN THEN DO:
        hQuery:GET-PREV().
        IF hBuffer:AVAIL THEN 
          rRepos = hBuffer:ROWID.
        ELSE DO:
          hQuery:GET-NEXT().
          hQuery:GET-NEXT().
          IF hBuffer:AVAIL THEN 
            rRepos = hBuffer:ROWID.
        END.
      END.

      FOR EACH bttObjectLink
          WHERE bttObjectLink.hFromObject = hQuery
            AND bttObjectLink.cLinkType   = "onetoone":
        RefreshParentRecord(hQuery,bttObjectLink.hToObject,"delete").
      END.

      hBuffer:FIND-BY-ROWID(rDelete).
      hBuffer:BUFFER-DELETE().
      IF bAvailFieldMap AND ix = iNumDeletes THEN DO:

        IF VALID-HANDLE(hBrowse) AND getAttribute(hToolbar,"commitstate") = "on" AND rRepos = ? THEN DO:
          setCurrentObject(ttObjectLink.hToObject).
          setAttribute(ttObjectLink.hToObject,"uselocaldata","").
          setAttribute(hBuffer,"uselocaldata","").
          setAttribute(hToolbar,"commitstate","off").
          RUN OpenQuery.
          ASSIGN bSetCurrHandles = FALSE.
                 hCurrWidget     = hBrowse
                 .
          setCurrentObject(hBrowse).
          RUN DoProcessEvent(0,"value-changed"). 
        END.
        ELSE IF VALID-HANDLE(hBrowse) THEN DO:
          hBrowse:REFRESH().
          IF rRepos NE ? THEN DO: 
            hBrowse:SET-REPOSITIONED-ROW(hBrowse:FOCUSED-ROW + 1,"conditional").
            hQuery:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
          END.
          ASSIGN bSetCurrHandles = FALSE.
                 hCurrWidget    = hBrowse
                 .
          setAttribute(hBrowse,"rowsdeleted",STRING(INT(getAttribute(hBrowse,"rowsdeleted")) + iNumDeletes)).
          DYNAMIC-FUNCTION("ViewRecordCount",hBrowse).
          setCurrentObject(hBrowse).
          RUN DoProcessEvent(0,"value-changed"). 
        END.
        ELSE DO:
          /* Deleted record is linked to a query: */
          FIND FIRST ttObjectLink
               WHERE ttObjectLink.hFromObject = hBuffer
                 AND ttObjectLink.cLinkType   = "query"
               NO-ERROR.
          IF AVAIL ttObjectLink THEN DO:
            ASSIGN bSetCurrHandles = FALSE.
                   hCurrWidget    = ttObjectLink.hToObject.
            setCurrentObject(ttObjectLink.hToObject).
            RUN DoProcessEvent(0,"value-changed"). 
          END.
        END.
      END.
      ELSE IF VALID-HANDLE(hBrowse) AND ix = iNumDeletes THEN DO:
        IF getAttribute(hBrowse,"enableondblclick") = "yes" OR getAttribute(hBrowse,"enableOnToolbarClick") = "yes" THEN 
          setAttribute(hBrowse,"enableupdate","").

        hBrowse:REFRESH().
        ASSIGN bSetCurrHandles = FALSE.
               hCurrWidget     = hBrowse
               .
        setCurrentObject(hBrowse).
        RUN DoProcessEvent(0,"value-changed"). 
        setAttribute(hBrowse,"rowsdeleted",STRING(INT(getAttribute(hBrowse,"rowsdeleted")) + iNumDeletes)).
        DYNAMIC-FUNCTION("ViewRecordCount",hBrowse).
      END.
    END.
    ELSE DO: 
      IF DYNAMIC-FUNCTION("getTransactionMessage") NE "" THEN
        DoMessage(0,0,DYNAMIC-FUNCTION("getTransactionMessage"),
                  (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Feil i sletting" ELSE "Error when deleting record"),"").
      RETURN ERROR.
    END.
  END.
  ELSE RETURN ERROR.
END.

IF VALID-HANDLE(hToolbar) THEN DO:
  setCurrentObject(hToolbar).
  PUBLISH "EndDeleteRecord" (hToolbar).
END.
ELSE DO:
  setCurrentObject(hBrowse).
  PUBLISH "EndDeleteRecord" (hBrowse).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeselectBrowseRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeselectBrowseRow Procedure
PROCEDURE DeselectRowRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR hRecordSelectWidget AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.

IF AVAIL ttObject THEN DO:
  IF ttObject.hObject:TYPE NE "browse" THEN DO:
    hBrowse = DYNAMIC-FUNCTION("getLinkedObject",ttObject.hObject,"browse","from").
    IF NOT VALID-HANDLE(hBrowse) OR hBrowse:TYPE NE "browse" THEN DO:
      MESSAGE "Missing association (link) from " ttObject.cObjectType " to browse" SKIP
              "Programmers mistake"
              VIEW-AS ALERT-BOX.
      RETURN.
    END.
  END.
  ELSE hBrowse = ttObject.hObject.
END.  
ELSE RETURN.
  
hBrowse:DESELECT-ROWS().
hRecordSelectWidget = WIDGET-HANDLE(getAttribute(hBrowse:WINDOW,"RecordSelectWidget")) NO-ERROR.

IF VALID-HANDLE(hRecordSelectWidget) THEN 
  IF hBrowse:NUM-SELECTED-ROWS > 0 THEN
    hRecordSelectWidget:SCREEN-VALUE = STRING(hBrowse:NUM-SELECTED-ROWS,"zzzzz9").
  ELSE
    hRecordSelectWidget:SCREEN-VALUE = "".



END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-DisplayRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord Procedure 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the browse (or a query). Must refresh any children and navigate to the buffer object and update browse fields
------------------------------------------------------------------------------*/
DEF VAR hBuffer         AS HANDLE NO-UNDO.
DEF VAR cQueryWhere     AS CHAR   NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR hMyTmpObject    AS HANDLE NO-UNDO.
DEF VAR inx             AS INT    NO-UNDO.        
DEF VAR hWidget         AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR rRowid          AS ROWID  NO-UNDO.
DEF VAR rCurrRowid      AS ROWID  NO-UNDO.
DEF VAR hExternalBrowse AS HANDLE NO-UNDO.
DEF VAR cAvailWidgets   AS CHAR   NO-UNDO.
DEF VAR hLinkFromBuffer AS HANDLE NO-UNDO.
DEF VAR hMySourceProc   AS HANDLE NO-UNDO.
DEF VAR cParentLinkType AS CHAR   NO-UNDO.
DEF VAR cIndexUsage     AS CHAR   NO-UNDO.
DEF VAR hChildObject    AS HANDLE NO-UNDO.
DEF VAR cChildLinkInfo  AS CHAR   NO-UNDO.
DEF VAR bWindowLock     AS LOG    NO-UNDO.
DEF VAR hRecordSelectWidget AS HANDLE NO-UNDO.

ASSIGN hBuffer     = IF ttObject.cObjectType = "browse" THEN ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1) ELSE ttObject.hObject:GET-BUFFER-HANDLE(1)
       hQuery      = IF ttObject.cObjectType = "browse" THEN ttObject.hObject:QUERY ELSE ttObject.hObject
       hBrowse     = IF ttObject.cObjectType = "browse" THEN ttObject.hObject ELSE ?
       hCurrObject = IF NOT VALID-HANDLE(hTmpObject) THEN ttObject.hObject ELSE hTmpObject
       cGlobSecDisabledActions = DYNAMIC-FUNCTION("getSecDisabledActions","")
       .

{incl/methodlog.i hCurrObject:NAME}

IF NOT VALID-HANDLE(hBuffer) THEN RETURN. /* <- query isn't open */

IF NOT CAN-FIND(FIRST ttObjectLink 
                WHERE ttObjectLink.hFromObject = hCurrObject
                  AND CAN-DO("parent,onetoone",ttObjectLink.cLinkType)) THEN 
                      
  SaveModified(hCurrObject).

IF hCurrObject:TYPE NE "query" AND 
   CAN-FIND(FIRST ttObjectLink 
            WHERE ttObjectLink.hToObject = hCurrObject
              AND CAN-DO("parent,onetoone",ttObjectLink.cLinkType)) THEN DO:
  bWindowLock = YES.  
  DoLockWindow(hCurrObject:WINDOW).
END.

IF hCurrObject:TYPE = "browse" THEN 
  setAttribute(hCurrObject,"currentrow",STRING(hCurrObject:FOCUSED-ROW)).

/* Update links to children (browsers and queries):, */
FOR EACH ttObjectLink
    WHERE ttObjectLink.hToObject = hCurrObject
      AND (ttObjectLink.cLinkType = "parent" OR ttObjectLink.cLinkType = "onetoone"):


  ASSIGN cParentLinkType = ttObjectLink.cLinkType
         cIndexUsage     = getAttribute(hChildObject,"use-index")
         hChildObject    = ttObjectLink.hFromObject
         cChildLinkInfo  = ttObjectLink.cLinkInfo
         .

  SaveModified(hChildObject).
  
  ASSIGN hTmpObject      = hChildObject
         hMyTmpObject    = hTmpObject
         .

  IF hTmpObject:TYPE = "browse" THEN
    hLinkFromBuffer = hChildObject:QUERY:GET-BUFFER-HANDLE(1).
  ELSE
    hLinkFromBuffer = hChildObject:GET-BUFFER-HANDLE(1).

  cQueryWhere = (IF cIndexUsage NE "" THEN " USE-INDEX " + cIndexUsage ELSE "") + " WHERE ".
  IF hBuffer:AVAIL THEN DO:
    IF ENTRY(1,cChildLinkInfo) = "parentlink" THEN
      cQueryWhere = getAttribute(hChildObject,"parentlink").
    ELSE DO ix = 1 TO NUM-ENTRIES(cChildLinkInfo):
      cQueryWhere = cQueryWhere + ENTRY(1,ENTRY(ix,cChildLinkInfo),";") + " = " +
                    (IF NUM-ENTRIES(ENTRY(ix,cChildLinkInfo),";") = 1 THEN
                      (IF hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'HANDLE' THEN 
                        'WIDGET-HANDLE("' + STRING(hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                       ELSE IF hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'CHARACTER' THEN 
                        '"' + hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE + '"'
                       ELSE
                        hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE + '("' +
                        STRING(hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                       )
                     ELSE
                      (IF hLinkFromBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'HANDLE' THEN 
                        'WIDGET-HANDLE("' + STRING(hBuffer:BUFFER-FIELD(ENTRY(2,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                       ELSE IF hLinkFromBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'CHARACTER' THEN 
                        '"' + hBuffer:BUFFER-FIELD(ENTRY(2,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE + '"'
                       ELSE
                        hLinkFromBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE + '("' +
                        STRING(hBuffer:BUFFER-FIELD(ENTRY(2,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                       )
                     )
                  + " AND ".
    END.
  END.
  ELSE cQueryWhere = cQueryWhere + "FALSE".
  
  hMySourceProc = hCurrSourceProc.
  IF cQueryWhere NE "WHERE " THEN DO:
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = hChildObject
           AND bttObjectLink.cLinkType   = "procedure" NO-LOCK NO-ERROR.
    IF AVAIL bttObjectLink THEN
      hCurrSourceProc = bttObjectLink.hToObject.
    ELSE DO:
      FIND FIRST bttObject
           WHERE bttObject.hObject = hChildObject.
      hCurrSourceProc = bttObject.hSourceProc.
    END.
    bSetCurrHandles = FALSE.
    hCurrWidget = hChildObject.
    setAttribute(hChildObject,"basequery",TRIM(RIGHT-TRIM(cQueryWhere," AND"))).
    IF cParentLinkType NE "onetoone" THEN
      setAttribute(hLinkFromBuffer,"checkmodified","no").

    IF getAttribute(hTmpObject,"enableondblclick") = "yes" THEN DO:
      IF getAttribute(hTmpObject,"doubleclickenabledfield") = "" THEN
        setAttribute(hTmpObject,"enableupdate","no").
    END.

    RUN DoProcessEvent(0,"open-query").
  END.

  IF cQueryWhere = "WHERE FALSE" THEN DO:
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = hMyTmpObject
           AND bttObjectLink.cLinkType   = "toolbar" NO-LOCK NO-ERROR.
    IF AVAIL bttObjectLink THEN
      DYNAMIC-FUNCTION("setToolbar",bttObjectLink.hToObject,IF cParentLinkType = "parent" THEN "disable" ELSE "not avail").
  END.
  hCurrSourceProc = hMySourceProc.

  hTmpObject = ?.
END.

/* It a fieldMap is linked, move to it to display it's fields: */
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = hCurrObject 
       AND ttObjectLink.cLinkType   = "fieldMap" 
     NO-ERROR.
IF AVAIL ttObjectLink AND getAttribute(ttObjectLink.hToObject,"fieldMapIsBrowse") NE "yes" THEN 
  DYNAMIC-FUNCTION("DisplayFieldMap",ttObjectLink.hToObject).  
ELSE
  FOR EACH ttObjectLink 
      WHERE ttObjectLink.hFromObject = hCurrObject
        AND ttObjectLink.cLinkType = "dotNetDisplay"
      :
    /* Target: JBoxWrapWindowInForm */
/*    PUBLISH "dotNetDisplay" (hCurrObject,ttObjectLink.hToObject,ttObjectLink.cLinkInfo).*/
    PUBLISH "dotNetMethod" ("dotNetDisplay",hCurrObject,ttObjectLink.hToObject,ttObjectLink.cLinkInfo,?).
  END.


IF NOT VALID-HANDLE(hCurrSourceProc) THEN hCurrSourceProc = SOURCE-PROCEDURE.
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraDisplayRecord") THEN 
  RUN ExtraDisplayRecord IN hCurrSourceProc (hCurrObject) NO-ERROR.

IF hBuffer:AVAIL THEN DO:
  setAttribute(hCurrObject,"currentrowid",STRING(hBuffer:ROWID)).
  FOR EACH ttObjectLink
      WHERE ttObjectLink.hFromObject = hCurrObject
        AND ttObjectLink.cLinkType   = "onetoone":
    RefreshParentRecord(hCurrObject,ttObjectLink.hToObject,"display").
  END.
END.

FOR EACH ttObjectLink
   WHERE ttObjectLink.hFromObject = hCurrObject
     AND CAN-DO("toolbar,panel,popupMenu",ttObjectLink.cLinkType):
  setAttribute(ttObjectLink.hToObject,"copyRecord","").
  IF hBuffer:AVAIL THEN DO:
    DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"avail").
    IF STRING(hBuffer:ROWID) = getAttribute(hCurrObject,"firstrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"first").
    ELSE IF STRING(hBuffer:ROWID) = getAttribute(hCurrObject,"lastrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"last").
  END.
  ELSE
    DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"not avail").
END.
IF hCurrObject:TYPE = "browse" THEN
  DYNAMIC-FUNCTION("setToolbar",hCurrObject,IF hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN "avail" ELSE "not avail").

IF hCurrObject:TYPE = "browse" AND hBuffer:AVAIL THEN 
  FOR EACH ttObjectLink
     WHERE ttObjectLink.hToObject = hCurrObject
       AND ttObjectLink.cLinkType = "onetoone"
    ,FIRST bttObjectLink
           WHERE bttObjectLink.hFromObject = ttObjectLink.hFromObject
             AND bttObjectLink.cLinkType = "toolbar":
    IF STRING(hBuffer:ROWID) = getAttribute(hBrowse,"firstrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",bttObjectLink.hToObject,"first").
    ELSE IF STRING(hBuffer:ROWID) = getAttribute(hBrowse,"lastrowid") THEN
      DYNAMIC-FUNCTION("setToolbar",bttObjectLink.hToObject,"last").
  END.

/* Disable any popup menu-items if the record is not available: */
cAvailWidgets  = getAttribute(hCurrObject,"recordavailmenu-items").
DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
  IF VALID-HANDLE(hWidget) THEN DO:
    hWidget:SENSITIVE = hBuffer:AVAIL.
    FIND FIRST ttEvent
         WHERE ttEvent.hWidget = hWidget NO-LOCK NO-ERROR.
    IF AVAIL ttEvent AND CAN-DO(cGlobSecDisabledActions,ttEvent.cAction) THEN
      hWidget:SENSITIVE = FALSE.
  END.
END.

IF hCurrObject:TYPE = "browse" THEN DO:
  setAttribute(hBrowse,"newrow","").
  IF hCurrObject:MULTIPLE AND hCurrObject:NUM-SELECTED-ROWS = 0
     AND hCurrObject:QUERY:NUM-RESULTS > 0 
     AND getAttribute(hCurrObject,"select1strow") = "yes" THEN DO:
    hCurrObject:SELECT-ROW(1).
    DisplayRow(hCurrObject).
  END.
  IF CAN-FIND(FIRST ttObjectLink
              WHERE ttObjectLink.hFromObject = hCurrObject
                AND ttObjectLink.cLinkType   = "browseoverlay") THEN
    hCurrObject:REFRESH() NO-ERROR.

  DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hCurrObject).
END.

IF hCurrObject:TYPE = "browse" AND hCurrObject:MULTIPLE THEN DO:
  hRecordSelectWidget = WIDGET-HANDLE(getAttribute(hCurrObject:WINDOW,"RecordSelectWidget")) NO-ERROR.

  IF VALID-HANDLE(hRecordSelectWidget) THEN 
    IF hCurrObject:NUM-SELECTED-ROWS > 0 THEN
      hRecordSelectWidget:SCREEN-VALUE = STRING(hCurrObject:NUM-SELECTED-ROWS,"zzzzz9").
    ELSE
      hRecordSelectWidget:SCREEN-VALUE = "".
END.

IF bWindowLock THEN
  DoLockWindow(?).

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoProcessEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoProcessEvent Procedure 
PROCEDURE DoProcessEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceProc  AS HANDLE NO-UNDO.
DEF INPUT PARAM icEvent       AS CHAR NO-UNDO.

DEF VAR bPass          AS LOG    NO-UNDO.
DEF VAR bReturnNoApply AS LOG    NO-UNDO.
DEF VAR hCurrObject    AS HANDLE NO-UNDO.
DEF VAR hMyEventWidget AS HANDLE NO-UNDO.
DEF VAR cCurrMethod    AS CHAR   NO-UNDO.

IF iCancelNextEvents > 0 THEN DO:
  iCancelNextEvents = iCancelNextEvents - 1.
  RETURN.
END.

bMyReturnNoApply = NO.

IF bSetCurrHandles OR NOT VALID-HANDLE(hCurrSourceProc) THEN DO:
  ASSIGN hCurrSourceProc = ihSourceProc
         hPrevWidget     = hCurrWidget
         hCurrWidget     = SELF.
  hCurrWindow = hCurrWidget:WINDOW NO-ERROR.

  IF ERROR-STATUS:ERROR THEN /* This would happen when a browse column initiates the event */
    hCurrWindow = hCurrWidget:PARENT:WINDOW.

END.

FIND FIRST ttEvent 
     WHERE ttEvent.hWidget = hCurrWidget 
       AND ttEvent.cName   = icEvent
     NO-ERROR.
IF NOT AVAIL ttEvent THEN
  FIND FIRST ttEvent 
       WHERE ttEvent.hWidget = hCurrWidget 
         AND ttEvent.cAction  = icEvent
       NO-ERROR.

IF AVAIL ttEvent THEN DO:
  ASSIGN bReturnNoApply = ttEvent.bReturnNoApply
         cCurrMethod    = ttEvent.cMethod.

  IF AVAIL ttObject THEN hPrevObject = ttObject.hObject.

  FIND ttObject WHERE ttObject.hObject = ttEvent.hObject.
  hCurrObject = ttObject.hObject.
  PUBLISH "StartJBoxEvent" (hCurrObject,hCurrSourceProc,hCurrWidget,cCurrMethod).

  IF ttObject.cObjectType = "toolbar" THEN DO:
    setAttribute(hCurrWindow,"current" + ttObject.cObjectType,STRING(ttObject.hObject)).
    setAttribute(ttObject.hObject,"lasttoolbaraction",ttEvent.cAction).

    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "browse"
         NO-LOCK NO-ERROR.
    IF NOT AVAIL ttObjectLink THEN
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = ttObject.hObject
             AND ttObjectLink.cLinkType   = "query"
           NO-LOCK NO-ERROR.
    IF AVAIL ttObjectLink THEN DO:
      FIND FIRST bttObjectLink
           WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
             AND CAN-DO("procedure,viewer",bttObjectLink.cLinkType)
           NO-LOCK NO-ERROR.
      IF AVAIL bttObjectLink AND CAN-DO(bttObjectLink.hToObject:INTERNAL-ENTRIES,ttEvent.cMethod) THEN DO:
        hCurrSourceProc = bttObjectLink.hToObject.
        RUN VALUE(ttEvent.cMethod) IN bttObjectLink.hToObject NO-ERROR.
        bPass = TRUE.
      END.
      ELSE DO:
        IF ttObjectLink.cLinkType = "query" THEN
          FIND FIRST ttObjectLink
               WHERE ttObjectLink.hFromObject = ttObject.hObject
                 AND ttObjectLink.cLinkType   = "browse"
               NO-LOCK NO-ERROR.
        ELSE
          FIND FIRST ttObjectLink
               WHERE ttObjectLink.hFromObject = ttObject.hObject
                 AND ttObjectLink.cLinkType   = "query"
               NO-LOCK NO-ERROR.
        IF AVAIL ttObjectLink THEN DO:
          FIND FIRST bttObjectLink
               WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
                  AND CAN-DO("procedure,viewer",bttObjectLink.cLinkType)
               NO-LOCK NO-ERROR.
          IF AVAIL bttObjectLink AND CAN-DO(bttObjectLink.hToObject:INTERNAL-ENTRIES,ttEvent.cMethod) THEN DO:
            hCurrSourceProc = bttObjectLink.hToObject.
            RUN VALUE(ttEvent.cMethod) IN bttObjectLink.hToObject NO-ERROR.
            bPass = TRUE.
          END.
          ELSE DO:
            FIND ttObjectLink
                 WHERE ttObjectLink.hFromObject = ttObject.hObject
                   AND ttObjectLink.cLinkType = "fieldmap"
                 NO-LOCK NO-ERROR.
            IF AVAIL ttObjectLink THEN DO:
              FIND FIRST bttObjectLink
                   WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
                     AND CAN-DO("procedure,viewer",bttObjectLink.cLinkType)
                   NO-LOCK NO-ERROR.
              IF AVAIL bttObjectLink AND CAN-DO(bttObjectLink.hToObject:INTERNAL-ENTRIES,ttEvent.cMethod) THEN DO:
                hCurrSourceProc = bttObjectLink.hToObject.
                RUN VALUE(ttEvent.cMethod) IN bttObjectLink.hToObject NO-ERROR.
                bPass = TRUE.
              END.
            END.
          END.
        END.
      END.
    END.
  END.

  IF NOT bPass THEN DO:

    IF VALID-HANDLE(ihSourceProc) AND ihSourceProc NE hCurrSourceProc AND CAN-DO(ihSourceProc:INTERNAL-ENTRIES,ttEvent.cMethod) THEN
      RUN VALUE(ttEvent.cMethod) IN ihSourceProc NO-ERROR.
    ELSE IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,ttEvent.cMethod) THEN 
      RUN VALUE(ttEvent.cMethod) IN hCurrSourceProc NO-ERROR.
    ELSE IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,ttEvent.cMethod) THEN 
      RUN VALUE(ttEvent.cMethod) NO-ERROR.
    ELSE MESSAGE "Method not found: " ttEvent.cMethod.

/*     IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,ttEvent.cMethod) THEN DO: */
/*       RUN VALUE(ttEvent.cMethod) IN ihSourceProc NO-ERROR.               */
/*       IF ERROR-STATUS:ERROR THEN                                         */
/*         RUN VALUE(ttEvent.cMethod) IN hCurrSourceProc NO-ERROR.          */
/*     END.                                                                 */
/*     ELSE IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES,ttEvent.cMethod) THEN */
/*       RUN VALUE(ttEvent.cMethod) NO-ERROR.                               */
/*     ELSE MESSAGE "Method not found: " ttEvent.cMethod.                   */

  END.
  IF ERROR-STATUS:ERROR THEN RETURN.
  setEventProcReturnValue(hCurrObject,cCurrMethod,RETURN-VALUE).
END.

bSetCurrHandles = TRUE.

IF bReturnNoApply OR bMyReturnNoApply OR getAttribute(hCurrObject,"return-no-apply") = "yes" THEN DO:
  IF VALID-HANDLE(hWidgetEnter) AND iCursorOffset NE 0 THEN DO:
    APPLY "entry" TO hWidgetEnter.
    hWidgetEnter:CURSOR-OFFSET = iCursorOffset.
  END.
  ELSE IF VALID-HANDLE(hWidgetEnd) THEN DO:
    APPLY "entry" TO hWidgetEnd.
    APPLY "end" TO hWidgetEnd.
  END.
  ELSE IF VALID-HANDLE(hWidgetEnter) THEN
    APPLY "entry" TO hWidgetEnter.
  ELSE IF cMyReturnNoApplyMethod NE "" THEN DO:
    IF VALID-HANDLE(hMyReturnNoApplyMethod) THEN
      RUN VALUE(cMyReturnNoApplyMethod) IN hMyReturnNoApplyMethod NO-ERROR.
    ELSE
      RUN VALUE(cMyReturnNoApplyMethod) NO-ERROR.
  END.
  ELSE IF VALID-HANDLE(hCurrWidget) AND NOT bApplyLastkey AND
     CAN-DO("Browse,button,combo-box,control-frame,editor,fill-in,frame,radio-set,selection-list,slider,toggle-box,window"
            ,hCurrWidget:TYPE) THEN APPLY "entry" TO hCurrWidget.

  ASSIGN bMyReturnNoApply = NO
         cMyReturnNoApplyMethod = ""
         hMyReturnNoApplyMethod = ?
         hWidgetEnter           = ?
         hWidgetEnd             = ?
         iCursorOffset          = 0
         bApplyLastKey          = NO.

  RETURN NO-APPLY.
END.
IF VALID-HANDLE(hCurrSourceProc) AND CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"PostProcessEvent") THEN
  RUN PostProcessEvent IN hCurrSourceProc (hCurrObject,hMyEventWidget).

PUBLISH "EndJBoxEvent" (hCurrObject,hCurrSourceProc,hCurrWidget,cCurrMethod).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EditRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord Procedure 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object is the toolbar 
------------------------------------------------------------------------------*/
DEF VAR hFirstWidget         AS HANDLE NO-UNDO.
DEF VAR hTmpWidget           AS HANDLE NO-UNDO.
DEF VAR hCurrObject          AS HANDLE NO-UNDO.
DEF VAR cPrimaryKeyFields    AS CHAR   NO-UNDO.
DEF VAR cScreenUpdateWidgets AS CHAR   NO-UNDO.
hCurrObject = ttObject.hObject.

IF NOT DYNAMIC-FUNCTION("getIsEventAllowed",hCurrObject,"edit") THEN RETURN.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = hCurrObject
       AND ttObjectLink.cLinkType   = "fieldmap"
     NO-ERROR.
IF AVAIL ttObjectLink THEN DO:
  FIND bttObject WHERE bttObject.hObject = ttObjectLink.hToObject.

  IF NOT DYNAMIC-FUNCTION("getActionPermission",
                          bttObject.hSourceProc:FILE-NAME,
                          DYNAMIC-FUNCTION("getObjectName",bttObject.hObject),
                          "Edit") THEN
    RETURN.
    
  ASSIGN cPrimaryKeyFields    = getAttribute(ttObjectLink.hToObject,"primaryKeyFields")
         cScreenUpdateWidgets = getAttribute(ttObjectLink.hToObject,"screenUpdateWidgets")
         .
    
  DO ix = 1 TO NUM-ENTRIES(cScreenUpdateWidgets):  
    hFirstWidget  = WIDGET-HANDLE(ENTRY(ix,cScreenUpdateWidgets)) NO-ERROR.
    IF NOT VALID-HANDLE(hFirstWidget) THEN RETURN.
    IF NOT CAN-DO(cPrimaryKeyFields,hFirstWidget:NAME) THEN LEAVE.
  END.

  IF getAttribute(bttObject.hObject,"fieldMapIsBrowse") = "YES" THEN DO:
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = bttObject.hObject
           AND bttObjectLink.cLinkType   = "browse"
         NO-ERROR.
    IF AVAIL bttObjectLink THEN DO: 
      setAttribute(bttObjectLink.hToObject,"enableupdate","yes").
      DYNAMIC-FUNCTION("DispBrwOverlayWidgets",bttObjectLink.hToObject).
    END.  
  END.
END.
ELSE DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = hCurrObject
         AND ttObjectLink.cLinkType   = "browse"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    FIND bttObject WHERE bttObject.hObject = ttObjectLink.hToObject.

    IF NOT DYNAMIC-FUNCTION("getActionPermission",
                            bttObject.hSourceProc:FILE-NAME,
                            DYNAMIC-FUNCTION("getObjectName",bttObject.hObject),
                            "Edit") THEN
      RETURN.

    APPLY "entry" TO bttObject.hObject.
    IF getAttribute(bttObject.hObject,"enableOnDblClick") = "YES" THEN
      RUN InvokeMethod (bttObject.hObject,"DefaultActionBrowse").
    ELSE
      bMyReturnNoApply = YES.

    RETURN.
  END.    
  ELSE RETURN.
END.

DYNAMIC-FUNCTION("setToolbar",hCurrObject,"edit").

IF VALID-HANDLE(hFirstWidget) THEN DO:
  hTmpWidget = hCurrWidget.
  APPLY "entry" TO hFirstWidget.
  hCurrWidget = hTmpWidget.
END.

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndErrorBrowseOverlay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndErrorBrowseOverlay Procedure 
PROCEDURE EndErrorBrowseOverlay :
/*------------------------------------------------------------------------------
  Purpose:    Handle ESC from an overlay field (go back to the browse) 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hBrowse AS HANDLE NO-UNDO.

setWidgetEnter(ttObject.hObject).

/* This won't do the trick: 
FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType = "browse"
     NO-ERROR.
IF AVAIL ttObjectLink THEN DO:
  hBrowse = ttObjectLink.hToObject.
  IF getAttribute(hBrowse,"enableondblclick") = "yes" OR getAttribute(hBrowse,"enableonreturn") = "yes" THEN DO:
    setAttribute(hBrowse,"enableupdate","").
    RUN EndResizeBrowseColumn.
    setWidgetEnter(hBrowse).
  END.
  ELSE setWidgetEnter(ttObject.hObject).
END.
*/
                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EndResizeBrowseColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndResizeBrowseColumn Procedure 
PROCEDURE EndResizeBrowseColumn :
/*------------------------------------------------------------------------------
  Purpose:     Resize any overlay fill-ins for resized browse column
  Parameters:  <none>
  Notes:       Current object is the browse, current event is generated by end-resize of browse column
------------------------------------------------------------------------------*/
/* Check if current column, the column before or after is resized */
DEF VAR cBrowseColumns   AS CHAR   NO-UNDO.
DEF VAR hWidget          AS HANDLE NO-UNDO.
DEF VAR hResizeWidget    AS HANDLE NO-UNDO.
DEF VAR iTmpXpixels      AS INT    NO-UNDO.
DEF VAR iTmpYpixels      AS INT    NO-UNDO.
DEF VAR hLastOverlay     AS HANDLE NO-UNDO.
DEF VAR hFirstOverlay    AS HANDLE NO-UNDO.
DEF VAR cTabChain        AS CHAR   NO-UNDO.
DEF VAR cWidthPixelsList AS CHAR   NO-UNDO.

IF NOT CAN-FIND(FIRST ttObjectLink
                WHERE ttObjectLink.hFromObject = ttObject.hObject
                  AND ttObjectLink.cLinkType   = "browseoverlay") THEN
  RETURN.

IF bFreezeEndResizeBrw THEN
  DoLockWindow(ttObject.hObject:WINDOW).

ASSIGN iTmpXpixels      = ttObject.hObject:FRAME:WIDTH-PIXELS
       iTmpYpixels      = ttObject.hObject:FRAME:HEIGHT-PIXELS
       cWidthPixelsList = getAttribute(ttObject.hObject,"widthpixelslist")
       ix               = (IF VALID-HANDLE(hCurrWidget) THEN LOOKUP(hCurrWidget:NAME,getAttribute(ttObject.hObject,"allviewfields")) ELSE 0)
       .

IF ix > 0 AND NUM-ENTRIES(cWidthPixelsList) GE ix THEN DO:
  ENTRY(ix,cWidthPixelsList) = STRING(hCurrWidget:WIDTH-PIXELS).
  DYNAMIC-FUNCTION("setAttribute",ttObject.hObject,"widthpixelslist",cWidthPixelsList).
END.

FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ttObject.hObject
      AND ttObjectLink.cLinkType   = "browseoverlay":

  IF NOT VALID-HANDLE(ttObjectLink.hToObject) THEN NEXT.

  hResizeWidget = ?.
  DO ix = 1 TO ttObject.hObject:NUM-COLUMNS:
    hWidget = ttObject.hObject:GET-BROWSE-COLUMN(ix).
    cBrowseColumns = cBrowseColumns + hWidget:NAME + ",".
    IF hWidget:NAME = ttObjectLink.cLinkInfo THEN
      hResizeWidget = hWidget.
  END.

  IF NOT VALID-HANDLE(hResizeWidget) THEN DO:
    ASSIGN ttObjectLink.hToObject:HIDDEN         = TRUE
           ttObjectLink.hToObject:X              = 1
           ttObjectLink.hToObject:Y              = 1
           .
    PUBLISH "ViewHideUserControl" (ttObjectLink.hToObject,NO).
    NEXT.
  END.

  IF NOT ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL
     OR hResizeWidget:X > ttObject.hObject:X + ttObject.hObject:WIDTH-PIXELS - 18
     OR hResizeWidget:X < 1 
     OR hResizeWidget:Y > ttObjectLink.hFromObject:Y + ttObjectLink.hFromObject:HEIGHT-PIXELS
     OR (LAST-EVENT:LABEL = "window-resized" AND hResizeWidget:Y < ttObjectLink.hFromObject:Y)
     OR getAttribute(ttObjectLink.hToObject,"visible") = "no"
     OR ((getAttribute(ttObject.hObject,"enableondblclick") = "yes" OR getAttribute(ttObject.hObject,"enableOnToolbarClick") = "yes") AND 
         getAttribute(ttObject.hObject,"enableupdate") NE "yes")
    THEN DO:
   ASSIGN ttObjectLink.hToObject:HIDDEN         = TRUE
          ttObjectLink.hToObject:X              = 1
          ttObjectLink.hToObject:Y              = 1
          .
   PUBLISH "ViewHideUserControl" (ttObjectLink.hToObject,NO).
  END.
  ELSE DO:
    ASSIGN ttObjectLink.hToObject:X              = IF ttObjectLink.hToObject:TYPE NE "toggle-box" THEN 
                                                     ttObject.hObject:X + hResizeWidget:X - 1
                                                   ELSE ttObject.hObject:X + hResizeWidget:X + 1
           ttObjectLink.hToObject:Y              = IF ttObjectLink.hToObject:TYPE NE "toggle-box" THEN ttObject.hObject:Y + hResizeWidget:Y - 1
                                                   ELSE ttObject.hObject:Y + hResizeWidget:Y
           ttObjectLink.hToObject:WIDTH-PIXELS   = IF hResizeWidget:X + hResizeWidget:WIDTH-PIXELS > ttObject.hObject:X + ttObject.hObject:WIDTH-PIXELS - 17 THEN
                                                     ttObject.hObject:X + ttObject.hObject:WIDTH-PIXELS - hResizeWidget:X - 18
                                                   ELSE
                                                     MIN(ttObject.hObject:WIDTH-PIXELS - 17,hResizeWidget:WIDTH-PIXELS + 4)
           ttObjectLink.hToObject:HIDDEN         = NO
           ttObjectLink.hToObject:SENSITIVE      = YES
           hLastOverlay                          = (IF ttObjectLink.hToObject:SENSITIVE AND 
                                                     (NOT VALID-HANDLE(hLastOverlay) OR 
                                                     (VALID-HANDLE(hLastOverlay) AND ttObjectLink.hToObject:X > hLastOverlay:X)) THEN
                                                     ttObjectLink.hToObject
                                                   ELSE hLastOverlay)
           hFirstOverlay                         = (IF ttObjectLink.hToObject:SENSITIVE AND
                                                     (NOT VALID-HANDLE(hFirstOverlay) OR 
                                                     (VALID-HANDLE(hFirstOverlay) AND ttObjectLink.hToObject:X < hFirstOverlay:X)) THEN
                                                     ttObjectLink.hToObject
                                                   ELSE hFirstOverlay)
           ttObjectLink.iSeq                     = ttObjectLink.hToObject:X
           NO-ERROR.
    IF ttObjectLink.hToObject:TYPE = "toggle-box" THEN ttObjectLink.hToObject:WIDTH-PIXELS = ttObjectLink.hToObject:WIDTH-PIXELS - 2 NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      ttObjectLink.hToObject:MOVE-TO-TOP().

    PUBLISH "ViewHideUserControl" (ttObjectLink.hToObject,YES).
  END.
  
  FIND FIRST ttEvent 
       WHERE ttEvent.hObject     = ttObjectLink.hToObject
         AND ttEvent.cWidgetType = "browse-lookup" 
       NO-ERROR.
  IF AVAIL ttEvent THEN DO:
    ASSIGN ttEvent.hWidget:X        = ttObjectLink.hToObject:X + ttObjectLink.hToObject:WIDTH-PIXELS - 18
           ttEvent.hWidget:Y        = ttObjectLink.hToObject:Y 
           ttEvent.hWidget:HIDDEN   = ttObjectLink.hToObject:HIDDEN
           NO-ERROR.
    IF NOT ttEvent.hWidget:HIDDEN AND NOT ERROR-STATUS:ERROR THEN
      ttEvent.hWidget:MOVE-TO-TOP().
  END.
END.
setAttribute(ttObject.hObject,"lastenabledoverlay",STRING(hLastOverlay)).
setAttribute(ttObject.hObject,"firstenabledoverlay",STRING(hFirstOverlay)).

/* Set tabulator sequence: */
ix = 0.
FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ttObject.hObject
      AND ttObjectLink.cLinkType   = "browseoverlay"
      BY ttObjectLink.iSeq:
  IF ttObjectLink.hToObject:HIDDEN OR NOT ttObjectLink.hToObject:SENSITIVE THEN 
    NEXT.
  ELSE
    ASSIGN ttObjectLink.hToObject:TAB-STOP = FALSE
           cTabChain = cTabChain + STRING(ttObjectLink.hToObject) + ","
            .
END.
setAttribute(ttObject.hObject,"tabchainoverlays",TRIM(cTabChain,",")).

IF VALID-HANDLE(ttObject.hObject) AND ttObject.hObject:TYPE = "browse" AND ttObject.hObject:FRAME:TYPE NE "dialog-box" THEN
  ASSIGN ttObject.hObject:FRAME:WIDTH-PIXELS  = iTmpXpixels
         ttObject.hObject:FRAME:HEIGHT-PIXELS = iTmpYpixels
         NO-ERROR.

IF bFreezeEndResizeBrw THEN DoLockWindow(?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EnterBrowseDropDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnterBrowseDropDown Procedure 
PROCEDURE EnterBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iBGcolNum  AS INT NO-UNDO.

IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "combo-box")  THEN RETURN.
ELSE DO:

  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse"
       NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN DO:
    MESSAGE "No link to browse from drop-down. Programmers mistake"
            VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
  END.
  ELSE DO:
    setAttribute(ttObjectLink.hToObject,"currentrow",STRING(ttObjectLink.hToObject:FOCUSED-ROW)).
    
    IF getAttribute(ttObjectLink.hToObject,"editoverlaybgcolnum") NE "" THEN
      iBGcolNum = INT(getAttribute(ttObjectLink.hToObject,"editoverlaybgcolnum")).
    IF getAttribute(ttObjectLink.hFromObject,"editoverlaybgcolnum") NE "" THEN
      iBGcolNum = INT(getAttribute(ttObjectLink.hFromObject,"editoverlaybgcolnum")).
    IF iBGcolNum = 0 THEN iBGcolNum = iEditBgColor.  
    IF iBGcolNum NE 0 THEN DO:
      FOR EACH bttObjectLink
          WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
            AND bttObjectLink.cLinkType   = "browseoverlay":
        bttObjectLink.hToObject:BGCOLOR = (IF iEditBgColor NE 0 THEN iEditBgColor ELSE ?).
      END.
      ttObject.hObject:BGCOLOR = iBGcolNum.
    END.

    hHelpWidget = WIDGET-HANDLE(getAttribute(hCurrWidget:WINDOW,"HelpTextWidget")) NO-ERROR.
    IF VALID-HANDLE(hHelpWidget) THEN
      hHelpWidget:SCREEN-VALUE = hCurrWidget:HELP.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EnterBrowseFillIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnterBrowseFillIn Procedure 
PROCEDURE EnterBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:    Keep the current row of the browse to be able to handle leave of fill-in later 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iBGcolNum    AS INT    NO-UNDO.

DEF BUFFER bttObjectLink FOR ttObjectLink.

IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "fill-in")  THEN RETURN.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN DO:
  MESSAGE "No link to browse from fill-in. Programmers mistake"
          VIEW-AS ALERT-BOX INFORMATION.
  RETURN.
END.
ELSE DO:
  setAttribute(ttObjectLink.hToObject,"currentrow",STRING(ttObjectLink.hToObject:FOCUSED-ROW)).
  setAttribute(ttObjectLink.hToObject,"currentOverlayWidget",STRING(ttObject.hObject)).
  
  IF getAttribute(ttObjectLink.hToObject,"editoverlaybgcolnum") NE "" THEN
    iBGcolNum = INT(getAttribute(ttObjectLink.hToObject,"editoverlaybgcolnum")).
  IF getAttribute(ttObjectLink.hFromObject,"editoverlaybgcolnum") NE "" THEN
    iBGcolNum = INT(getAttribute(ttObjectLink.hFromObject,"editoverlaybgcolnum")).
    
  IF iBGcolNum = 0 THEN iBGcolNum = iEditBgColor.
    
  IF iBGcolNum NE 0 THEN DO:
    FOR EACH bttObjectLink
        WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
          AND bttObjectLink.cLinkType   = "browseoverlay":
      bttObjectLink.hToObject:BGCOLOR = (IF iEditBgColor NE 0 THEN iEditBgColor ELSE ?).
    END.
    ttObject.hObject:BGCOLOR = iBGcolNum.
  END.

  hHelpWidget = WIDGET-HANDLE(getAttribute(hCurrWidget:WINDOW,"HelpTextWidget")) NO-ERROR.
  IF VALID-HANDLE(hHelpWidget) THEN
    hHelpWidget:SCREEN-VALUE = hCurrWidget:HELP.


  FOR EACH bttObjectLink 
      WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
        AND bttObjectLink.cLinkType   = "dotNetDisplay"
        AND bttObjectLink.cLinkInfo   = hCurrWidget:NAME 
      :
    /* Target: JBoxWrapWindowInForm */
   /* PUBLISH "dotNetFocus" (ttObjectLink.hToObject,bttObjectLink.hToObject,bttObjectLink.cLinkInfo). */
    PUBLISH "dotNetMethod" ("dotNetFocus",ttObjectLink.hToObject,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,?).
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EnterBrowseSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnterBrowseSearch Procedure 
PROCEDURE EnterBrowseSearch :
/*------------------------------------------------------------------------------
  Purpose:     enter the browse search field when user presses F8
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hSearchField AS HANDLE NO-UNDO.

IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "browse")  THEN RETURN.
ELSE DO:
  hSearchField = DYNAMIC-FUNCTION("GetLinkedObject",ttObject.hObject,"browse-search-field","from").
  IF VALID-HANDLE(hSearchField) THEN
    APPLY "entry" TO hSearchField.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EnterBrowseToggle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnterBrowseToggle Procedure 
PROCEDURE EnterBrowseToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iBGcolNum  AS INT NO-UNDO.

IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "toggle-box")  THEN RETURN.
ELSE DO:
  ttObject.hObject:BGCOLOR = ?.

  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse"
       NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN DO:
    MESSAGE "No link to browse from fill-in. Programmers mistake"
            VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
  END.
  ELSE DO:
    setAttribute(ttObjectLink.hToObject,"currentrow",STRING(ttObjectLink.hToObject:FOCUSED-ROW)).
    
    hHelpWidget = WIDGET-HANDLE(getAttribute(hCurrWidget:WINDOW,"HelpTextWidget")) NO-ERROR.
    IF VALID-HANDLE(hHelpWidget) THEN
      hHelpWidget:SCREEN-VALUE = hCurrWidget:HELP.

/*     IF getAttribute(ttObjectLink.hToObject,"editoverlaybgcolnum") NE "" THEN         */
/*       iBGcolNum = INT(getAttribute(ttObjectLink.hToObject,"editoverlaybgcolnum")).    */
/*     IF getAttribute(ttObjectLink.hFromObject,"editoverlaybgcolnum") NE "" THEN       */
/*       iBGcolNum = INT(getAttribute(ttObjectLink.hFromObject,"editoverlaybgcolnum")).  */
/*     IF iBGcolNum NE 0 THEN DO:                                                   */
/*       FOR EACH bttObjectLink                                                    */
/*           WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject              */
/*             AND bttObjectLink.cLinkType   = "browseoverlay":                    */
/*         bttObjectLink.hToObject:BGCOLOR = ?.                                    */
/*       END.                                                                      */
/*       ttObject.hObject:BGCOLOR = iBGcolNum.                                      */
/*     END.                                                                        */
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EntryOfWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EntryOfWidget Procedure 
PROCEDURE EntryOfWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCurrObject AS HANDLE NO-UNDO.
DEF VAR cLabel      AS CHAR   NO-UNDO.

hCurrObject = ttObject.hObject.

hHelpWidget = WIDGET-HANDLE(getAttribute(hCurrWidget:WINDOW,"HelpTextWidget")) NO-ERROR.

IF VALID-HANDLE(hHelpWidget) THEN DO:
  cLabel = hCurrWidget:LABEL NO-ERROR.
  hHelpWidget:SCREEN-VALUE = IF getAttribute(hCurrWidget,"MyHelpText") NE "" THEN getAttribute(hCurrWidget,"MyHelpText")
                             ELSE IF hCurrWidget:HELP NE ? THEN hCurrWidget:HELP 
                             ELSE IF hCurrWidget:TOOLTIP NE ? THEN REPLACE(hCurrWidget:TOOLTIP,CHR(10)," ")
                             ELSE IF cLabel NE ? THEN cLabel
                             ELSE IF hCurrWidget:TYPE = "browse" THEN hCurrWidget:QUERY:GET-BUFFER-HANDLE(1):NAME
                             ELSE "".
END.
  
IF hCurrObject:TYPE = "browse" THEN
    DYNAMIC-FUNCTION("viewRecordCount",hCurrObject).
ELSE IF CAN-DO("fill-in,editor",hCurrWidget:TYPE) THEN DO:
  FOR EACH ttObjectLink 
      WHERE ttObjectLink.hFromObject = hCurrObject
        AND ttObjectLink.cLinkType = "dotNetDisplay"
        AND ttObjectLink.cLinkInfo = hCurrWidget:NAME 
      :
    /* Target: JBoxWrapWindowInForm */
  /*  PUBLISH "dotNetFocus" (hCurrObject,ttObjectLink.hToObject,ttObjectLink.cLinkInfo). */
    PUBLISH "dotNetMethod" ("dotNetFocus",hCurrObject,ttObjectLink.hToObject,ttObjectLink.cLinkInfo,?).
  END.
END.

setCurrentObject(hCurrObject).

RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExcelRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord Procedure 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the toolbar. Must navigate to the browse object
               To export a table (without browse) ovverride this by referring the table objec
------------------------------------------------------------------------------*/
DEF VAR iNewBatch    AS INT NO-UNDO.
DEF VAR iMaxCount    AS INT NO-UNDO.
DEF VAR hQryOrBuffer AS HANDLE NO-UNDO.
DEF VAR hCurrObject  AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.

IF ttObject.cObjectType = "browse" THEN 
  hQryOrBuffer = ttObject.hObject.
ELSE DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "browse" 
       NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject 
           AND ttObjectLink.cLinkType   = "fieldmap" 
         NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject 
           AND ttObjectLink.cLinkType   = "query" 
         NO-ERROR.
  IF AVAIL ttObjectLink THEN 
    FIND FIRST ttObject 
         WHERE ttObject.hObject = ttObjectLink.hToObject
         NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN.
  hQryOrBuffer = ttObject.hObject.  
END.

IF hQryOrBuffer:TYPE = "BROWSE" 
   AND getAttribute(hQryOrBuffer,"firstrowid") = "" 
   AND getAttribute(hQryOrBuffer,"uselocaldata") NE "yes" 
   AND hQryOrBuffer:NUM-SELECTED-ROWS < 2  
   THEN DO:
  iMaxCount = DYNAMIC-FUNCTION("FillBrowse",hQryOrBuffer,
                      INT(getAttribute(hQryOrBuffer,"rowstobatch")),
                      INT(getAttribute(hQryOrBuffer,"querystart")),
                      getAttribute(hQryOrBuffer,"buffersandfields"),
                      getAttribute(hQryOrBuffer,"basequery") + 
                        getAttribute(hQryOrBuffer,"queryfilter") + 
                        getAttribute(hQryOrBuffer,"querywhere") + 
                       (IF getAttribute(hQryOrBuffer,"use-index") NE "" THEN
                         " USE-INDEX " + getAttribute(hQryOrBuffer,"use-index")
                        ELSE "") +
/*                        (IF getAttribute(hQryOrBuffer,"orgqueryjoin") = "" THEN */
                          getAttribute(hQryOrBuffer,"queryjoin"),
/*                         ELSE ""), */
                      getAttribute(hQryOrBuffer,"querysort"),
                      IF getAttribute(hQryOrBuffer,"querydesc") = "desc" THEN TRUE ELSE FALSE).
  setAttribute(hQryOrBuffer,"querystart",STRING(iMaxCount)).
END.


IF hQryOrBuffer:TYPE = "BROWSE" 
   AND getAttribute(hQryOrBuffer,"lastrowid") = "" 
   AND getAttribute(hQryOrBuffer,"uselocaldata") NE "yes" 
   AND hQryOrBuffer:NUM-SELECTED-ROWS < 2  
   THEN DO:
    
  IF (getAttribute(SESSION,"DropExcelWarning") NE "YES" AND
      getAttribute(hQryOrBuffer,"DropExcelWarning") NE "NO")
     OR getAttribute(hQryOrBuffer,"DropExcelWarning") = "YES" THEN
    RUN dAskForQueryBatch.w (OUTPUT iNewBatch).

  IF iNewBatch > 0 THEN DO:
    iMaxCount = DYNAMIC-FUNCTION("FillBrowse",hQryOrBuffer,
                        iNewBatch,
                        INT(getAttribute(hQryOrBuffer,"querystart")),
                        getAttribute(hQryOrBuffer,"buffersandfields"),
                        getAttribute(hQryOrBuffer,"basequery") +
                          getAttribute(hQryOrBuffer,"queryfilter") +
                          getAttribute(hQryOrBuffer,"querywhere") +
                         (IF getAttribute(hQryOrBuffer,"use-index") NE "" THEN
                           " USE-INDEX " + getAttribute(hQryOrBuffer,"use-index")
                          ELSE "") +
/*                          (IF getAttribute(hQryOrBuffer,"orgqueryjoin") = "" THEN */
                            getAttribute(hQryOrBuffer,"queryjoin"),
/*                           ELSE ""), */
                        getAttribute(hQryOrBuffer,"querysort"),
                        getAttribute(hQryOrBuffer,"querydesc") = "desc").
    setAttribute(hQryOrBuffer,"querystart",STRING(iMaxCount)).
  END.
  ELSE IF iNewBatch < 0 THEN RETURN.
END.

IF getAttribute(hQryOrBuffer,"useExcelViewer") NE "yes" THEN
  DYNAMIC-FUNCTION("ToExcelViaFile",hQryOrBuffer,0).

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-F3Widget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE F3Widget Procedure 
PROCEDURE F3Widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the fieldMap object
------------------------------------------------------------------------------*/
DEF VAR hButton     AS HANDLE NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.

hButton = WIDGET-HANDLE(DYNAMIC-FUNCTION("getFieldMapWidgets",ttObject.hObject,"btn" + hCurrWidget:NAME)) NO-ERROR.
IF VALID-HANDLE(hButton) THEN
  APPLY "choose" TO hButton.

setCurrentObject(hCurrObject).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FilterRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilterRecord Procedure 
PROCEDURE FilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       If current object is the toolbar we must navigate to the browse object
------------------------------------------------------------------------------*/
DEF VAR hFilter         AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR cFilterWin      AS CHAR   NO-UNDO.
DEF VAR hCurrSourceProc AS HANDLE NO-UNDO.
DEF VAR hBrowseOrQuery  AS HANDLE NO-UNDO.
DEF VAR hFilterButton   AS HANDLE NO-UNDO.
DEF VAR bDialog         AS LOG    NO-UNDO.

IF ttObject.cObjectType NE "browse" THEN DO:
  hFilterButton = hCurrWidget.
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "browse" 
       NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject 
           AND ttObjectLink.cLinkType   = "query" 
         NO-ERROR.
  
  IF NOT AVAIL ttObjectLink THEN RETURN.
  ELSE hBrowseOrQuery = ttObjectLink.hToObject.
       
  bDialog = getAttribute(ttObject.hObject,"filterdialogbox") = "yes".
  IF NOT bDialog THEN
    bDialog = getAttribute(hBrowseOrQuery,"filterdialogbox") = "yes".
END.
ELSE DO:
  hBrowseOrQuery = ttObject.hObject.
  bDialog = getAttribute(ttObject.hObject,"filterdialogbox") = "yes".
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    hFilterButton = DYNAMIC-FUNCTION("getEventWidget",ttObjectLink.hToObject,"filter","").
    IF NOT bDialog THEN
      bDialog = getAttribute(ttObjectLink.hToObject,"filterdialogbox") = "yes".
  END.
  ELSE hFilterButton = DYNAMIC-FUNCTION("getEventWidget",hBrowseOrQuery,"filter","menu-item").
END.

ASSIGN hCurrObject     = ttObject.hObject
       hCurrSourceProc = ttObject.hSourceProc.

IF VALID-HANDLE(hCurrObject:WINDOW) THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",hCurrObject:WINDOW).
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","yes").
END.

hFilter = WIDGET-HANDLE(getAttribute(hCurrObject,"filterhandle")).

IF NOT VALID-HANDLE(hFilter) THEN DO:
  cFilterWin = getAttribute(hCurrObject,"filterwindow").
  IF cFilterWin = "" THEN DO:
    cFilterWin = DYNAMIC-FUNCTION("getUserSetting",
                                  DYNAMIC-FUNCTION("getObjectSourceFile",hCurrObject),
                                  DYNAMIC-FUNCTION("getObjectName",hCurrObject),
                                  DYNAMIC-FUNCTION("getAttribute",hCurrObject,"usersettingcontext"),
                                  "filterwindow").
    IF cFilterWin NE "" THEN
      setAttribute(hCurrObject,"filterwindow",cFilterWin).
    ELSE DO:
      cFilterWin = getAttribute(hCurrObject,"customfilterwindow").
      IF cFilterWin = "" THEN DO:
        cFilterWin = getAttribute(SESSION,"filterwindow").
        IF cFilterWin = "" THEN DO:
          cFilterWin = DYNAMIC-FUNCTION("getUserSetting",
                                        "session","","",
                                        "filterwindow").
          IF cFilterWin = "" THEN
            cFilterWin = "JBoxDynFilter.w".
          setAttribute(SESSION,"filterwindow",cFilterWin).
        END.
      END.
    END.
  END.
  RUN VALUE(cFilterWin) PERSIST SET hFilter 
      (hCurrSourceProc,
       hBrowseOrQuery,
       hFilterButton,
       bDialog). 
  
  setAttribute(hCurrObject,"filterhandle",STRING(hFilter)).
  setAttribute(hBrowseOrQuery,"filterhandle",STRING(hFilter)).
END.
RUN MoveToTop IN hFilter.

DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FirstRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FirstRecord Procedure 
PROCEDURE FirstRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is browse/query or toolbar 
------------------------------------------------------------------------------*/
DEF VAR iMaxCount       AS INT    NO-UNDO.
DEF VAR hExternalBrowse AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.

IF ttObject.cObjectType = "toolbar" THEN DO:
  hToolbar = ttObject.hObject.
  FIND FIRST ttObjectLink 
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse" 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink 
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query" 
         NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    hExternalBrowse = WIDGET-HANDLE(getAttribute(ttObjectLink.hToObject,"ExternalBrowse")) NO-ERROR.     
    IF VALID-HANDLE(hExternalBrowse) THEN DO:
      hExternalBrowse:QUERY:GET-FIRST().
      hExternalBrowse:QUERY:REPOSITION-TO-ROWID(hExternalBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID).
      APPLY "value-changed" TO hExternalBrowse.
      DYNAMIC-FUNCTION("setToolbar",ttObject.hObject,"first").
      RETURN.
    END.
  END.
  IF ttObjectLink.cLinkType = "query" AND 
     CAN-FIND(FIRST bttObjectLink WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
                                    AND bttObjectLink.cLinkType = "onetoone") THEN DO:
    FIND bttObjectLink 
         WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
           AND bttObjectLink.cLinkType = "onetoone".
    FIND ttObject WHERE ttObject.hObject = bttObjectLink.hToObject NO-ERROR.
  END.
  ELSE FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN.
END.

ASSIGN hCurrObject     = ttObject.hObject
       hCurrSourceProc = ttObject.hSourceProc.
IF hCurrObject:TYPE = "browse" THEN
  hQuery = hCurrObject:QUERY.
ELSE IF hCurrObject:TYPE = "query" THEN
  hQuery = hCurrObject.

IF getAttribute(hCurrObject,"firstrowid") NE "" THEN DO:
  IF hCurrObject:TYPE = "query" THEN hQuery:GET-FIRST().
  ELSE hQuery:REPOSITION-TO-ROWID(TO-ROWID(getAttribute(hCurrObject,"firstrowid"))).
  IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    IF hCurrObject:TYPE = "browse" THEN DO:        
      hCurrObject:DESELECT-ROWS().
      hCurrObject:SELECT-FOCUSED-ROW().
    END.
    ASSIGN bSetCurrHandles = FALSE
           hCurrWidget    = hCurrObject
           .
    RUN DoProcessEvent(0,"value-changed").
    IF VALID-HANDLE(hToolbar) THEN
      DYNAMIC-FUNCTION("setToolbar",hToolbar,"first").
    ELSE DO:
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = hCurrObject 
             AND ttObjectLink.cLinkType   = "toolbar"
           NO-ERROR.
      IF AVAIL ttObjectLink THEN
        DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"first").
    END.
  END.
  RETURN.
END.

setAttribute(hCurrObject,"querystart","0").
setAttribute(hCurrObject,"querydir","").
DYNAMIC-FUNCTION("setQueryStatFields",""). 

IF hCurrObject:TYPE = "browse" THEN DO:
  iMaxCount = DYNAMIC-FUNCTION("fillBrowse",hCurrObject,
                      INT(getAttribute(hCurrObject,"rowstobatch")),
                      INT(getAttribute(hCurrObject,"querystart")),"","","",
                      getAttribute(hCurrObject,"querydesc") = "desc").
  setAttribute(hCurrObject,"querystart",STRING(iMaxCount)).

  IF hCurrObject:NUM-ITERATIONS > 0 THEN DO:
    hQuery:GET-FIRST().
    IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
      hCurrObject:SELECT-FOCUSED-ROW().
      ASSIGN bSetCurrHandles = FALSE
             hCurrWidget    = hCurrObject
             .
      RUN DoProcessEvent(0,"value-changed").
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"first").
      ELSE DO:
        FIND FIRST ttObjectLink
             WHERE ttObjectLink.hFromObject = hCurrObject 
               AND ttObjectLink.cLinkType   = "toolbar"
             NO-ERROR.
        IF AVAIL ttObjectLink THEN
          DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"first").
      END.
    END.
  END.
END.
ELSE IF VALID-HANDLE(hQuery) THEN DO:
  iMaxCount = DYNAMIC-FUNCTION("fillQuery",hCurrObject,
                      INT(getAttribute(hCurrObject,"rowstobatch")),
                      INT(getAttribute(hCurrObject,"querystart")),"","").

  setAttribute(hCurrObject,"querystart",STRING(iMaxCount)).
  IF hCurrObject:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    ASSIGN bSetCurrHandles  = FALSE
           hCurrWidget      = hCurrObject
           .
    RUN DoProcessEvent(0,"value-changed").    
    IF VALID-HANDLE(hToolbar) THEN
      DYNAMIC-FUNCTION("setToolbar",hToolbar,"first").
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FlatViewRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord Procedure 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:    View multiple linked queries (browsers) as one browse 
  Parameters:  <none>
  Notes:      - Current object is the toolbar - or it can be the browse (when doing a drill-down, f.ex)
              - Note that the attribute "flatviewjointype" can be set on each linked browse (query - coming).
                If set at the top-level query it is passed on to all childs unless overridden at child level.
                If not set a INNER-JOIN is assumed.
------------------------------------------------------------------------------*/
DEF VAR hCurrObject           AS HANDLE NO-UNDO.
DEF VAR hFlatView             AS HANDLE NO-UNDO.
DEF VAR cBuffersAndFields     AS CHAR   NO-UNDO.
DEF VAR cQueryCrit            AS CHAR   NO-UNDO.
DEF VAR cQuerySort            AS CHAR   NO-UNDO.
DEF VAR cCalcFieldProcs       AS CHAR   NO-UNDO.
DEF VAR hPlaceHolder          AS HANDLE NO-UNDO.
DEF VAR hSourceQuery          AS HANDLE NO-UNDO.
DEF VAR hFlatBrw              AS HANDLE NO-UNDO.
DEF VAR hFilterButton         AS HANDLE NO-UNDO.
DEF VAR cFieldReposList       AS CHAR   NO-UNDO.
DEF VAR cExcludedList         AS CHAR   NO-UNDO.
DEF VAR iReduceMoveFrom       AS INT    NO-UNDO.
DEF VAR iReduceMoveTo         AS INT    NO-UNDO.
DEF VAR iy                    AS INT    NO-UNDO.
DEF VAR bEmbedded             AS LOG    NO-UNDO.
DEF VAR cTitle                AS CHAR   NO-UNDO.
DEF VAR bUseDataBrowseObject  AS LOG    NO-UNDO.             

hCurrObject = ttObject.hObject.

bUseDataBrowseObject = PROVERSION > "10".

IF ttObject.cObjectType NE "browse" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse"
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query"
         NO-LOCK NO-ERROR.
  
  IF NOT AVAIL ttObjectLink THEN RETURN.
  ELSE FIND FIRST ttObject WHERE ttObject.hObject = ttObjectLink.hToObject.
END.
hSourceQuery = ttObject.hObject.

ASSIGN cBuffersAndFields = IF getAttribute(hSourceQuery,"flatviewbuffersandfields") NE "" THEN
                             getAttribute(hSourceQuery,"flatviewbuffersandfields")
                           ELSE IF ttObject.cObjectType = "query" THEN 
                             getAttribute(hSourceQuery,"buffersandfields")
                           ELSE
                             getAttribute(hSourceQuery,"viewbuffersandfields")
       cQueryCrit        = /* getAttribute(hSourceQuery,"basequery") + 
                           getAttribute(hSourceQuery,"queryfilter") +
                           getAttribute(hSourceQuery,"querywhere") + */
                           "where false" 
                         + (IF getAttribute(hSourceQuery,"flatviewqueryjoin") = "-" THEN ""
                            ELSE IF getAttribute(hSourceQuery,"flatviewqueryjoin") NE "" THEN
                              getAttribute(hSourceQuery,"flatviewqueryjoin")
                            ELSE getAttribute(hSourceQuery,"queryjoin"))
       cQuerySort        = TRIM(REPLACE(REPLACE(getAttribute(hSourceQuery,"localsort")," desc ",";desc ")," BY ",",") + ";" +
                           getAttribute(hSourceQuery,"querydesc"),";")
       cCalcFieldProcs   = IF getAttribute(hSourceQuery,"calcfieldproc") NE "" THEN ",calcfieldproc|" + getAttribute(hSourceQuery,"calcfieldproc") ELSE ""
       cFieldReposList   = getAttribute(hSourceQuery,"fieldreposlist")
       cTitle            = getAttribute(hSourceQuery,"flatViewTitle")
                           .

/* hFlatView = WIDGET-HANDLE(getAttribute(hCurrObject,"flatviewhandle")) NO-ERROR. */

IF NOT VALID-HANDLE(hFlatView) THEN DO:
  PUBLISH "StartTabWindow" ("JBoxDataBrw.w",
           TRIM((IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rapport" ELSE "Report") + " " + cTitle), 
           hCurrSourceProc, /* parent */
           YES,     /* (always) new instance */
           NO,     /* keep the tab open after close of this window */
           OUTPUT hFlatView). 
  IF NOT VALID-HANDLE(hFlatView) THEN          
    RUN JBoxDataBrw.w PERSIST SET hFlatView.
  ELSE bEmbedded = YES.

  hPlaceHolder = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).

  RUN getChildFldsAndCrit (hSourceQuery,
                           getAttribute(hSourceQuery,"flatviewjointype"),
                           getAttribute(hSourceQuery,"allviewfields"),
                           hPlaceHolder,
                           INPUT-OUTPUT cBuffersAndFields,
                           INPUT-OUTPUT cQueryCrit,
                           INPUT-OUTPUT cCalcFieldProcs,
                           INPUT-OUTPUT cFieldReposList).

  DYNAMIC-FUNCTION("setCalcFieldProcs" IN hFlatView,TRIM(cCalcFieldProcs,";")).
  DYNAMIC-FUNCTION("setParent" IN hFlatView,hCurrSourceProc).
  DYNAMIC-FUNCTION("setDisableSaveFilter" IN hFlatView,YES).
  DYNAMIC-FUNCTION("setAttribute",hPlaceHolder,"disablesavebrowseconfig","yes").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"FieldGroup_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"FieldOperator_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"FilterLookupFields_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"FilterLookupQuery_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"FilterLookupReturnField_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"FilterValue_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"GroupOperator_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"HiddenGroupOperator_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"OperatorInUse_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"filterdropdownvaluelist_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"filterdropdownfields_").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"filterdropdownquery_").
  DYNAMIC-FUNCTION("AppendAttribute",hPlaceHolder,"calcfieldfilter",getAttribute(hSourceQuery,"calcFieldFilter"),"|").
/*   DYNAMIC-FUNCTION("AppendAttribute",hPlaceHolder,"prescanqueryfilter",DYNAMIC-FUNCTION("getAttribute",hSourceQuery,"prescanqueryfilter"),"|"). */
/*   DYNAMIC-FUNCTION("AppendAttribute",hPlaceHolder,"calcfieldfilter",DYNAMIC-FUNCTION("getAttribute",hSourceQuery,"calcfieldfilter"),"|").       */
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"localsort").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"myQuerySort").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"myLocalSort").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"noColumnSort").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"use-index").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"availaccumfields").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"accumdatatypes").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"availaccumfields").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"accumdatatypes").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"availdistinctcolumns").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"distinctdatatypes").
  DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"prescanquery*").
  IF getAttribute(hSourceQuery,"flatviewbuffersandfields") = "" THEN
    DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hPlaceHolder,"tempTableHandle").
  DYNAMIC-FUNCTION("msetAttribute",hPlaceHolder,"prescanquerywhere","").


  setAttribute(hPlaceHolder,"disabledrilldownfilter","yes").

  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraFlatViewRecord") THEN
    RUN ExtraFlatViewRecord IN hCurrSourceProc (hFlatView,OUTPUT bOK).
  ELSE bOk = YES.
  
  IF bOK THEN DO:
    
    IF getAttribute(hPlaceHolder,"buffersAndFields") NE "" THEN
      ASSIGN cBuffersAndFields = getAttribute(hPlaceHolder,"buffersAndFields")
             cQueryCrit        = "where false" 
                               + getAttribute(hPlaceHolder,"queryjoin")
             cQuerySort        = getAttribute(hPlaceHolder,"querysort")
             hSourceQuery      = hPlaceHolder  /* <- NOTE - flatview is now used for a new query, (maybe) different from original source query */                   
                               .
    IF getAttribute(hSourceQuery,"flatViewTitle") NE "" THEN
      cTitle = getAttribute(hSourceQuery,"flatViewTitle").                           
      

    hFlatView:CURRENT-WINDOW:TITLE =                  
            (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rapport: " ELSE "Report: ")  
         +  (IF cTitle NE "" THEN cTitle
             ELSE REPLACE(
                    TRIM(
                      (IF DYNAMIC-FUNCTION("getAttribute",hPlaceHolder,"prescanqueryfilter") NE "" THEN
                         DYNAMIC-FUNCTION("getAttribute",hPlaceHolder,"prescanqueryfilter")
                       ELSE DYNAMIC-FUNCTION("getAttribute",hSourceQuery,"queryfilter")) + 
                       TRIM(REPLACE(DYNAMIC-FUNCTION("getAttribute",hPlaceHolder,"calcfieldfilter"),"¤"," "),"|")
                        ),
                  CHR(1),",")
             ).

    IF LENGTH(hFlatView:CURRENT-WINDOW:TITLE) < 10 THEN
      hFlatView:CURRENT-WINDOW:TITLE =                  
              (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Rapport: " ELSE "Report: ") + hCurrSourceProc:CURRENT-WINDOW:TITLE NO-ERROR.

    IF DYNAMIC-FUNCTION("getAttribute",hCurrObject,"tempTableHandle") NE "" THEN
      cBuffersAndFields = "temp-table " + cBuffersAndFields.

    IF bUseDataBrowseObject THEN
      RUN InitFlatViewDataBrwObj.p (hFlatView,
                                    cBuffersAndFields,cQueryCrit,cQuerySort,
                                    getAttribute(hFlatView,"viewRecordCount") NE "no" AND getAttribute(hPlaceHolder,"getRecordCount") NE "no" AND getAttribute(hSourceQuery,"flatViewRecordCount") NE "no").
    ELSE                                    
      RUN InitializeObject IN hFlatView (cBuffersAndFields,
                                         cQueryCrit, 
                                         cQuerySort,
                                         getAttribute(hFlatView,"viewRecordCount") NE "no" AND getAttribute(hPlaceHolder,"getRecordCount") NE "no" AND getAttribute(hSourceQuery,"flatViewRecordCount") NE "no").

    hFlatBrw = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).
    
    DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hFlatBrw,"baseQuery").
    DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hFlatBrw,"queryFilter").
    DYNAMIC-FUNCTION("CopyAttributes",hSourceQuery,hFlatBrw,"queryWhere").

    RUN InvokeMethod(hFlatBrw,"OpenQuery").  

    hFlatBrw:HELP = hFlatView:CURRENT-WINDOW:TITLE.

    IF getAttribute(hSourceQuery,"queryfilter") NE "" OR getAttribute(hSourceQuery,"prescanqueryfilter") NE "" OR getAttribute(hSourceQuery,"calcfieldfilter") NE "" THEN DO:
/*      setAttribute(DYNAMIC-FUNCTION("getLinkedObject",hFlatBrw,"toolbar","from"),"configdisabledevents","filter").*/
      hFilterButton = WIDGET-HANDLE(getAttribute(DYNAMIC-FUNCTION("getLinkedObject",hFlatBrw,"toolbar","from"),"buttonFilter")).
      &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
        IF cActiveFilterButton NE "" THEN
          hFilterButton:LOAD-IMAGE(cActiveFilterButton) NO-ERROR.
      &ENDIF    
      APPLY "value-changed" TO hFlatBrw.
    END.

    cExcludedList = getAttribute(hFlatBrw,"excludedfieldposlist").
   
    DO ix = 1 TO NUM-ENTRIES(cFieldReposList):
      ASSIGN bOK = YES
             iReduceMoveFrom = 0
             iReduceMoveTo   = 0.
      DO iy = 1 TO NUM-ENTRIES(cExcludedList):
        IF INT(ENTRY(iy,cExcludedList)) < INT(ENTRY(2,ENTRY(ix,cFieldReposList),";")) THEN
          iReduceMoveFrom = iReduceMoveFrom + 1.
        IF INT(ENTRY(iy,cExcludedList)) < INT(ENTRY(3,ENTRY(ix,cFieldReposList),";")) THEN
          iReduceMoveTo = iReduceMoveTo + 1.
        IF INT(ENTRY(iy,cExcludedList)) = INT(ENTRY(2,ENTRY(ix,cFieldReposList),";")) THEN DO:
          bOk = NO.
          LEAVE.
        END.
      END.
      IF bOK THEN hFlatBrw:MOVE-COLUMN(INT(ENTRY(2,ENTRY(ix,cFieldReposList),";")) - iReduceMoveFrom,INT(ENTRY(3,ENTRY(ix,cFieldReposList),";")) - iReduceMoveTo) NO-ERROR.
    END.
    
  END.
  setAttribute(hCurrObject,"flatviewhandle",IF VALID-HANDLE(hFlatView) THEN STRING(hFlatView) ELSE "").
  
  IF NOT bUseDataBrowseObject THEN
    PUBLISH "SizeToFitTab" (hWin).
END.
ELSE PUBLISH "ViewTabProcHandle" (hFlatView:CURRENT-WINDOW).

RUN MoveToTop IN hFlatView NO-ERROR.

setCurrentObject(hCurrObject).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getChildFldsAndCrit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getChildFldsAndCrit Procedure 
PROCEDURE getChildFldsAndCrit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT        PARAM ihObject            AS HANDLE NO-UNDO.
DEF INPUT        PARAM icJoinType          AS CHAR   NO-UNDO.
DEF INPUT        PARAM icViewFields        AS CHAR   NO-UNDO.
DEF INPUT        PARAM ihPlaceHolder       AS HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM iocBuffersAndFields AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM iocQueryCrit        AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM iocCalcFieldProcs   AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM iocFieldReposList   AS CHAR   NO-UNDO.

DEF VAR cBuffAndFlds      AS CHAR   NO-UNDO.
DEF VAR hbParent          AS HANDLE NO-UNDO.
DEF VAR hbChild           AS HANDLE NO-UNDO.
DEF VAR cThisJoinType     AS CHAR   NO-UNDO.
DEF VAR cQueryJoin        AS CHAR   NO-UNDO.
DEF VAR cModQueryJoin     AS CHAR   NO-UNDO.
DEF VAR cCalcProc         AS CHAR   NO-UNDO.
DEF VAR iy                AS INT    NO-UNDO.
DEF VAR cModBuffAndFlds   AS CHAR   NO-UNDO.
DEF VAR bBufferFieldInUse AS LOG    NO-UNDO.
DEF VAR cMatchField       AS CHAR   NO-UNDO.
DEF VAR cIndexUsage       AS CHAR   NO-UNDO.
DEF VAR cDbFieldReposList AS CHAR   NO-UNDO.
DEF VAR cFieldReposList   AS CHAR   NO-UNDO.
DEF VAR iInputNumBuffers  AS INT    NO-UNDO.
DEF VAR cBufferList       AS CHAR   NO-UNDO.
DEF VAR cFieldBuffer      AS CHAR   NO-UNDO.

hbParent = IF ihObject:TYPE = "browse" THEN ihObject:QUERY:GET-BUFFER-HANDLE(1) ELSE ihObject:GET-BUFFER-HANDLE(1).  

FOR EACH ttObjectLink
    WHERE ttObjectLink.hToObject = ihObject
      AND ttObjectLink.cLinkType = "parent"
      AND ttObjectLink.cLinkInfo NE "parentlink": /* Indicates that the link query is set contitionally in DisplayRecord - not suitable for automatic drill-down */
  
  IF getAttribute(ttObjectLink.hFromObject,"excludeFromFlatView") = "yes" THEN NEXT.
  
  cBuffAndFlds = IF getAttribute(ttObjectLink.hFromObject,"flatviewbuffersandfields") NE "" THEN
                   getAttribute(ttObjectLink.hFromObject,"flatviewbuffersandfields")
                 ELSE IF ttObjectLink.hFromObject:TYPE = "query" THEN  
                   getAttribute(ttObjectLink.hFromObject,"buffersandfields")
                 ELSE
                   getAttribute(ttObjectLink.hFromObject,"viewbuffersandfields").
  IF DYNAMIC-FUNCTION("getAttribute",ttObjectLink.hFromObject,"hideflatviewduplicates") = "yes" THEN
    DO ix = 1 TO NUM-ENTRIES(cBuffAndFlds):
      ASSIGN cModBuffAndFlds   = cModBuffAndFlds + ENTRY(1,ENTRY(ix,cBuffAndFlds),";") + ";"
             bBufferFieldInUse = NO.
      DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,cBuffAndFlds),";"):      
        ASSIGN cMatchField = ENTRY(1,ENTRY(iy,ENTRY(ix,cBuffAndFlds),";"),"|") +
                             (IF INDEX(ENTRY(iy,ENTRY(ix,cBuffAndFlds),";"),"|") = 0 THEN ";" ELSE "") 
               cModBuffAndFlds = cModBuffAndFlds 
             + (IF iocBuffersAndFields MATCHES "*;" + cMatchField + "*" THEN "!" ELSE "")
             + ENTRY(iy,ENTRY(ix,cBuffAndFlds),";") + ";"
               bBufferFieldInUse = YES.
      END.
      IF bBufferFieldInUse THEN
        cModBuffAndFlds = TRIM(cModBuffAndFlds,";").
      cModBuffAndFlds = cModBuffAndFlds + ",".
    END.
  ELSE cModBuffAndFlds = cBuffAndFlds.

  ASSIGN iInputNumBuffers    = NUM-ENTRIES(iocBuffersAndFields)
         iocBuffersAndFields = iocBuffersAndFields + "," + TRIM(cModBuffAndFlds,",")
         cIndexUsage         = getAttribute(ttObjectLink.hFromObject,"use-index")
         iocQueryCrit        = iocQueryCrit + ",EACH " + ENTRY(1,ENTRY(1,cBuffAndFlds),";") 
                             + (IF cIndexUsage NE "" THEN " USE-INDEX " + cIndexUsage ELSE "")
                             + " WHERE "
         hbChild             = IF ttObjectLink.hFromObject:TYPE = "browse" THEN 
                                 ttObjectLink.hFromObject:QUERY:GET-BUFFER-HANDLE(1) 
                               ELSE ttObjectLink.hFromObject:GET-BUFFER-HANDLE(1)
         cCalcProc           = getAttribute(ttObjectLink.hFromObject,"calcfieldproc")
         iocCalcFieldProcs   = (IF iocCalcFieldProcs = "" AND cCalcProc NE "" THEN ",calcfieldproc|" 
                                ELSE IF iocCalcFieldProcs NE "" AND cCalcProc NE "" THEN iocCalcFieldProcs + ";"
                                ELSE iocCalcFieldProcs)
         iocCalcFieldProcs   = iocCalcFieldProcs + (IF cCalcProc NE "" THEN cCalcProc ELSE "")
         cFieldReposList     = getAttribute(ttObjectLink.hFromObject,"fieldreposlist")
         cDbFieldReposList   = getAttribute(ttObjectLink.hFromObject,"dbfieldreposlist")
         cBufferList         = getAttribute(ttObjectLink.hFromObject,"bufferlist")
         .
  
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"FieldGroup_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"FieldOperator_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"FilterLookupFields_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"FilterLookupQuery_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"FilterLookupReturnField_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"FilterValue_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"GroupOperator_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"HiddenGroupOperator_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"OperatorInUse_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"filterdropdownvaluelist_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"filterdropdownfields_").
  DYNAMIC-FUNCTION("CopyAttributes",ttObjectLink.hFromObject,ihPlaceHolder,"filterdropdownquery_").
  
  DYNAMIC-FUNCTION("AppendAttribute",ihPlaceHolder,"calcfieldfilter",getAttribute(ttObjectLink.hFromObject,"calcFieldFilter"),"|").

  DO ix = 1 TO NUM-ENTRIES(cDbFieldReposList):
    ASSIGN cFieldBuffer      = getAttribute(ttObjectLink.hFromObject,"fieldbuffer" + ENTRY(1,ENTRY(ix,cFieldReposList),";"))
           iocFieldReposList = iocFieldReposList 
                             + (IF iocFieldReposList NE "" THEN "," ELSE "")
                             + ENTRY(1,ENTRY(ix,cDbFieldReposList),";")
                             + (IF CAN-DO(icViewFields,ENTRY(1,ENTRY(ix,cDbFieldReposList),";")) THEN
                                  STRING(iInputNumBuffers + LOOKUP(cFieldBuffer,cBufferList))
                                ELSE "")
                             .
    DO iy = 2 TO 3:
      iocFieldReposList = iocFieldReposList + ";" + STRING(INT(ENTRY(iy,ENTRY(ix,cFieldReposList),";")) + NUM-ENTRIES(icViewFields)).
    END.
  END.

  IF ENTRY(1,ttObjectLink.cLinkInfo) = "parentlink" THEN
    iocQueryCrit = iocQueryCrit + getAttribute(ttObjectLink.hFromObject,"parentlink").
  ELSE DO ix = 1 TO NUM-ENTRIES(ttObjectLink.cLinkInfo):
    iocQueryCrit = iocQueryCrit + ENTRY(1,ENTRY(1,getAttribute(ttObjectLink.hFromObject,"buffersandfields"),";")) + "." 
                                + ENTRY(1,ENTRY(ix,ttObjectLink.cLinkInfo),";") + " = " 
                + (IF NUM-ENTRIES(ENTRY(ix,ttObjectLink.cLinkInfo),";") = 1 THEN  
                    (IF hbChild:BUFFER-FIELD(ENTRY(ix,ttObjectLink.cLinkInfo)):DATA-TYPE = "HANDLE" THEN
                       "WIDGET-HANDLE(" + ENTRY(ix,ttObjectLink.cLinkInfo) + ")"
                     ELSE IF hbChild:BUFFER-FIELD(ENTRY(ix,ttObjectLink.cLinkInfo)):DATA-TYPE = "CHARACTER" THEN
                       "STRING(" + hbParent:NAME + "." + ENTRY(ix,ttObjectLink.cLinkInfo) + ")"
                     ELSE
                       hbChild:BUFFER-FIELD(ENTRY(ix,ttObjectLink.cLinkInfo)):DATA-TYPE + "(" +
                       hbParent:NAME + "." + ENTRY(ix,ttObjectLink.cLinkInfo) + ")"
                     )
                   ELSE  
                    (IF hbChild:BUFFER-FIELD(ENTRY(1,ENTRY(ix,ttObjectLink.cLinkInfo),";")):DATA-TYPE = "HANDLE" THEN
                       "WIDGET-HANDLE(" + ENTRY(2,ENTRY(ix,ttObjectLink.cLinkInfo),";") + ")"
                     ELSE IF hbChild:BUFFER-FIELD(ENTRY(1,ENTRY(ix,ttObjectLink.cLinkInfo),";")):DATA-TYPE = "CHARACTER" THEN
                       "STRING(" + hbParent:NAME + "." + ENTRY(2,ENTRY(ix,ttObjectLink.cLinkInfo),";") + ")"
                     ELSE
                       hbChild:BUFFER-FIELD(ENTRY(ix,ttObjectLink.cLinkInfo)):DATA-TYPE + "(" +
                       hbParent:NAME + "." + ENTRY(2,ENTRY(ix,ttObjectLink.cLinkInfo),";") + ")"
                     )
                   ).
    IF ix < NUM-ENTRIES(ttObjectLink.cLinkInfo) THEN
      iocQueryCrit = iocQueryCrit + " AND ".
    IF ix = NUM-ENTRIES(ttObjectLink.cLinkInfo) THEN 
      iocQueryCrit = iocQueryCrit + getAttribute(ttObjectLink.hFromObject,"queryfilter").
  END.  
  cThisJoinType = getAttribute(ttObjectLink.hFromObject,"flatviewjointype").
  IF cThisJoinType = "" THEN cThisJoinType = icJoinType.

  cQueryJoin = getAttribute(ttObjectLink.hFromObject,"queryjoin").
  /* If the child join is outer all joins from the child must be the same: */
  IF cThisJoinType = "OUTER-JOIN" AND cQueryJoin NE "," THEN DO:
    cModQueryJoin = ",".
    DO ix = 2 TO NUM-ENTRIES(cQueryJoin):
      cModQueryJoin = cModQueryJoin + ENTRY(ix,cQueryJoin) +
                      (IF NOT ENTRY(ix,cQueryJoin) MATCHES "*outer-join*" THEN
                        " OUTER-JOIN"
                       ELSE "") +
                      ",".
    END.
    cQueryJoin = RIGHT-TRIM(cModQueryJoin,",").
  END.
  ELSE cQueryJoin = RIGHT-TRIM(cQueryJoin,",").

  iocQueryCrit = iocQueryCrit + " NO-LOCK " + cThisJoinType + cQueryJoin.
  RUN getChildFldsAndCrit(ttObjectLink.hFromObject,icJointype,
                          icViewFields + "," + getAttribute(ttObjectLink.hFromObject,"allviewfields"),
                          ihPlaceHolder,
                          INPUT-OUTPUT iocBuffersAndFields,
                          INPUT-OUTPUT iocQueryCrit,
                          INPUT-OUTPUT iocCalcFieldProcs,
                          INPUT-OUTPUT iocFieldReposList).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Help) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Help Procedure 
PROCEDURE Help :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InvalidateHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle Procedure 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     Close the parent procedure when a viewer object is closed
  Parameters:  <none>
  Notes:       If the viewer is invoked by the tab-folder object the tab-folder handles closing
------------------------------------------------------------------------------*/
FIND FIRST bttObject
     WHERE bttObject.hObject = SOURCE-PROCEDURE
     NO-ERROR.
IF AVAIL bttObject THEN DO:
  FIND FIRST bttObjectLink 
       WHERE bttObjectLink.hFromObject = bttObject.hObject
         AND bttObjectLink.cLinkType   = "parent"
         AND bttObjectLink.cLinkInfo   = "Viewer"
       NO-ERROR.
  IF AVAIL bttObjectLink THEN
    APPLY "close" TO bttObjectLink.hToObject.
END.

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

DEF BUFFER bttObject FOR ttObject.

FIND FIRST ttEvent WHERE ttEvent.hObject = ihObject AND ttEvent.cMethod = icMethod NO-LOCK NO-ERROR.
IF AVAIL ttEvent THEN DO:
  FIND ttObject OF ttEvent.
  ASSIGN bSetCurrHandles = FALSE
         hCurrWidget     = ttEvent.hWidget
         hCurrSourceProc = ttObject.hSourceProc
         hTmpObject      = ?.
  RUN DoProcessEvent(0,ttEvent.cAction).
  bSetCurrHandles = YES.
END.
ELSE DO:
  IF VALID-HANDLE(ihObject) THEN
    MESSAGE "Invalid method reference for " ihObject:NAME ": " icMethod SKIP
            "Caller: " PROGRAM-NAME(2)
            VIEW-AS ALERT-BOX ERROR.
  ELSE
    MESSAGE "Invalid object reference for InvokeMethod. Method: " icMethod SKIP
            "Caller: " PROGRAM-NAME(2)
            VIEW-AS ALERT-BOX ERROR.

  RETURN ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastRecord Procedure 
PROCEDURE LastRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the toolbar or the browse itself
------------------------------------------------------------------------------*/
DEF VAR iMaxCount       AS INT NO-UNDO.
DEF VAR hExternalBrowse AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.

IF ttObject.cObjectType = "toolbar" THEN DO:
  hToolbar = ttObject.hObject.
  FIND FIRST ttObjectLink 
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse" 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink 
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query" 
         NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    hExternalBrowse = WIDGET-HANDLE(getAttribute(ttObjectLink.hToObject,"ExternalBrowse")) NO-ERROR.     
    IF VALID-HANDLE(hExternalBrowse) THEN DO:
      hExternalBrowse:QUERY:GET-LAST().
      hExternalBrowse:SET-REPOSITIONED-ROW(hExternalBrowse:NUM-ITERATIONS,"conditional").
      hExternalBrowse:QUERY:REPOSITION-TO-ROWID(hExternalBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID).
      APPLY "value-changed" TO hExternalBrowse.
      DYNAMIC-FUNCTION("setToolbar",ttObject.hObject,"last").
      RETURN.
    END.
  END.
  IF ttObjectLink.cLinkType = "query" AND 
     CAN-FIND(FIRST bttObjectLink WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
                                    AND bttObjectLink.cLinkType = "onetoone") THEN DO:
    FIND bttObjectLink 
         WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
           AND bttObjectLink.cLinkType = "onetoone".
    FIND ttObject WHERE ttObject.hObject = bttObjectLink.hToObject NO-ERROR.
  END.
  ELSE FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject NO-ERROR.

  IF NOT AVAIL ttObject THEN RETURN.
END.

ASSIGN hCurrObject     = ttObject.hObject
       hCurrSourceProc = ttObject.hSourceProc.
IF hCurrObject:TYPE = "browse" THEN
  hQuery = hCurrObject:QUERY.
ELSE hQuery = hCurrObject.

IF getAttribute(hCurrObject,"lastrowid") NE "" THEN DO:
  IF hCurrObject:TYPE = "browse" THEN DO:      
    hCurrObject:SET-REPOSITIONED-ROW(hCurrObject:DOWN,"conditional").      
    hCurrObject:DESELECT-ROWS().
    hQuery:REPOSITION-TO-ROWID(TO-ROWID(getAttribute(hCurrObject,"lastrowid"))).
  END.
  ELSE hQuery:GET-LAST().
  IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    IF hCurrObject:TYPE = "browse" THEN DO:      
      hCurrObject:DESELECT-ROWS().
      hCurrObject:SELECT-ROW(hCurrObject:NUM-ITERATIONS).
      hCurrObject:FETCH-SELECTED-ROW(1).
    END.

    ASSIGN bSetCurrHandles = FALSE
           hCurrWidget    = hCurrObject
           .
    RUN DoProcessEvent(0,"value-changed").
    IF VALID-HANDLE(hToolbar) THEN
      DYNAMIC-FUNCTION("setToolbar",hToolbar,"last").
    ELSE DO:
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = hCurrObject 
             AND ttObjectLink.cLinkType   = "toolbar"
           NO-ERROR.
      IF AVAIL ttObjectLink THEN
        DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"last").
    END.
  END.
  RETURN.
END.

setAttribute(hCurrObject,"querystart","0").
setAttribute(hCurrObject,"querydir","desc").

DYNAMIC-FUNCTION("setQueryStatFields",""). 

IF hCurrObject:TYPE = "browse" THEN DO:
  DYNAMIC-FUNCTION("setQueryStatFields",""). 
  iMaxCount = DYNAMIC-FUNCTION("fillBrowse",hCurrObject,
                      INT(getAttribute(hCurrObject,"rowstobatch")),
                      INT(getAttribute(hCurrObject,"querystart")),"","","",
                      getAttribute(hCurrObject,"querydesc") = "desc").
  setAttribute(hCurrObject,"querystart",STRING(iMaxCount)).

  IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    hCurrObject:DESELECT-ROWS().
    hCurrObject:SET-REPOSITIONED-ROW(hCurrObject:DOWN,"conditional").
    hQuery:GET-BUFFER-HANDLE(1):FIND-BY-ROWID(TO-ROWID(getAttribute(hCurrObject,"lastrowid"))) NO-ERROR.
    IF hQuery:GET-BUFFER-HANDLE(1):AVAIL THEN
      hQuery:REPOSITION-TO-ROWID(hQuery:GET-BUFFER-HANDLE(1):ROWID).
    ELSE
      hCurrObject:SELECT-ROW(hCurrObject:NUM-ITERATIONS).
    hCurrObject:FETCH-SELECTED-ROW(1) NO-ERROR.
    ASSIGN bSetCurrHandles  = FALSE
           hCurrWidget      = hCurrObject
           .
    RUN DoProcessEvent(0,"value-changed").
    
    IF VALID-HANDLE(hToolbar) THEN
      DYNAMIC-FUNCTION("setToolbar",hToolbar,"last").
    ELSE DO:
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = hCurrObject 
             AND ttObjectLink.cLinkType   = "toolbar"
           NO-ERROR.
      IF AVAIL ttObjectLink THEN
        DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"last").
    END.
  END.
END.
ELSE DO:
  iMaxCount = DYNAMIC-FUNCTION("fillQuery",hCurrObject,
                      INT(getAttribute(hCurrObject,"rowstobatch")),
                      INT(getAttribute(hCurrObject,"querystart")),"","").

  setAttribute(hCurrObject,"querystart",STRING(iMaxCount)).

  IF hCurrObject:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    ASSIGN bSetCurrHandles  = FALSE
           hCurrWidget      = hCurrObject
           .
    RUN DoProcessEvent(0,"value-changed").    
    IF VALID-HANDLE(hToolbar) THEN
      DYNAMIC-FUNCTION("setToolbar",hToolbar,"last").
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeaveBrowseDropDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseDropDown Procedure 
PROCEDURE LeaveBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the combo
------------------------------------------------------------------------------*/
DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR hCombo               AS HANDLE NO-UNDO.

IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "combo-box")  THEN RETURN.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
hCombo = ttObject.hObject.
IF NOT AVAIL ttObjectLink THEN DO:
  MESSAGE "No link to browse from combo. Programmers mistake"
          VIEW-AS ALERT-BOX INFORMATION.
  RETURN.
END.
ELSE hBrowse = ttObjectLink.hToObject.

NavigateFromBrowseOverlay(hCombo,hBrowse).

IF NOT VALID-HANDLE(hCombo) THEN
  hCombo = WIDGET-HANDLE(getAttribute(hBrowse,"currentoverlaywidget")).

setCurrentObject(hCombo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeaveBrowseFillIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn Procedure 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the fill-in
------------------------------------------------------------------------------*/
DEF VAR hBuffer              AS HANDLE NO-UNDO.
DEF VAR cQueryWhere          AS CHAR   NO-UNDO. 
DEF VAR cValidate1           AS CHAR   NO-UNDO.
DEF VAR cValidate2           AS CHAR   NO-UNDO.
DEF VAR cValidate3           AS CHAR   NO-UNDO.
DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR cJoin                AS CHAR   NO-UNDO.
DEF VAR cJoinBuffers         AS CHAR   NO-UNDO.
DEF VAR hFillIn              AS HANDLE NO-UNDO.
DEF VAR cBuffersAndFields    AS CHAR   NO-UNDO.
DEF VAR bMyOK                AS LOG    NO-UNDO.
DEF VAR cUpdateValProc       AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateFields   AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateValues   AS CHAR   NO-UNDO.
DEF VAR iBGcolNum            AS INT    NO-UNDO INIT ?.
DEF VAR bNewRow              AS LOG    NO-UNDO.
DEF VAR cLastEvent           AS CHAR   NO-UNDO.
DEF VAR iFocusedRow          AS INT    NO-UNDO.

IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "fill-in")  THEN RETURN.

IF getAttribute(ttObject.hObject,"last-event") NE "" THEN
  cLastEvent = getAttribute(ttObject.hObject,"last-event").
ELSE
  cLastEvent = LAST-EVENT:LABEL.
    
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
hFillIn = ttObject.hObject.
IF NOT AVAIL ttObjectLink THEN DO:
  IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN  
    MESSAGE "No link to browse from fill-in." SKIP
            PROGRAM-NAME(1) SKIP
            "This could be a valid situation if the field should be changed to read-only" SKIP(1)
            "THIS MESSAGE IS ONLY SHOWN WHEN QUERY-LOGGING IS ON"
            VIEW-AS ALERT-BOX INFORMATION.
  RETURN "error".
END.
ELSE hBrowse = ttObjectLink.hToObject.

IF cLastEvent = "ctrl-s" AND getAttribute(hBrowse,"editWithFieldMap") NE "yes" THEN cLastEvent = "enter".
     
bNewRow = getAttribute(hBrowse,"newrow") = "yes".

hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

IF NOT hBuffer:AVAIL THEN RETURN.

IF hFillIn:MODIFIED THEN DO:

  iFocusedRow = hBrowse:FOCUSED-ROW.
  IF getAttribute(hFillIn,"validation") NE "" THEN DO:
    ASSIGN cValidate1 = ENTRY(1,getAttribute(hFillIn,"validation"))
           cValidate2 = IF NUM-ENTRIES(getAttribute(hFillIn,"validation")) > 1 THEN ENTRY(2,getAttribute(hFillIn,"validation")) ELSE ""
           cValidate3 = IF NUM-ENTRIES(getAttribute(hFillIn,"validation")) > 2 THEN ENTRY(3,getAttribute(hFillIn,"validation")) ELSE ""
           bMyOK        = TRUE
           .
    CASE hFillIn:DATA-TYPE:
      WHEN "DECIMAL" THEN DO:
        IF (DEC(hFillIn:SCREEN-VALUE) LT DEC(cValidate1) OR DEC(hFillIn:SCREEN-VALUE) GT DEC(cValidate2))
           AND DEC(hFillIn:SCREEN-VALUE) NE DEC(cValidate3) THEN
          bMyOK = FALSE.
      END.
      WHEN "INTEGER" THEN DO:
        IF (INT(hFillIn:SCREEN-VALUE) LT INT(cValidate1) OR INT(hFillIn:SCREEN-VALUE) GT INT(cValidate2)) 
          AND INT(hFillIn:SCREEN-VALUE) NE INT(cValidate3) THEN
          bMyOK = FALSE.
      END.
      WHEN "DATE" THEN DO:
        IF (DATE(hFillIn:SCREEN-VALUE) LT DATE(cValidate1) OR DATE(hFillIn:SCREEN-VALUE) GT DATE(cValidate2)) 
          AND DATE(hFillIn:SCREEN-VALUE) NE DATE(cValidate3) THEN
          bMyOK = FALSE.
      END.
      WHEN "CHARACTER" THEN DO:
        IF LOOKUP(hFillIn:SCREEN-VALUE,cValidate1) = 0 THEN
          bMyOK = FALSE.
      END.
    END CASE.
    IF NOT bMyOK THEN DO:
      IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"MyBrowseFillInValMsg") THEN
        RUN MyBrowseFillInValMsg IN hCurrSourceProc.
      ELSE 
        DoMessage(0,0,
                  (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Ugyldig verdi: " ELSE "Invalid value: ") + hFillIn:SCREEN-VALUE + CHR(10) + CHR(10) +
                  (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Gyldig: " ELSE "Valid: ") + cValidate1 +
                  (IF cValidate2 NE "" THEN " - " + cValidate2 ELSE "")
                   ,"","").

      CASE hFillIn:DATA-TYPE:
        WHEN "DECIMAL"   THEN hFillIn:SCREEN-VALUE = "0".
        WHEN "INTEGER"   THEN hFillIn:SCREEN-VALUE = "0".
        WHEN "DATE"      THEN hFillIn:SCREEN-VALUE = ?.
        WHEN "CHARACTER" THEN hFillIn:SCREEN-VALUE = "".
      END.
  
      APPLY "entry" TO hFillIn.
      RETURN.
    END.
  END.
  bMyOK = ?.
  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"MySaveBrowseFillIn") THEN
    RUN MySaveBrowseFillIn IN hCurrSourceProc (hFillIn,hBuffer,OUTPUT bMyOK).
  IF bMyOK = ? THEN DO:  
    IF NOT getAttribute(hBrowse,"uselocaldata") = "yes" AND NOT getAttribute(hBrowse,"editWithFieldMap") = "yes" THEN DO:
      DYNAMIC-FUNCTION("setPostUpdProc",IF getAttribute(hBuffer,"postUpdateProc") NE "" THEN
                                          getAttribute(hBuffer,"postUpdateProc")
                                        ELSE getAttribute(hBrowse,"postUpdateProc")).
      ASSIGN cUpdateValProc = IF getAttribute(hFillIn,"customUpdateValProc") NE "" THEN 
                                getAttribute(hFillIn,"customUpdateValProc")
                              ELSE IF getAttribute(hBuffer,"customUpdateValProc") NE "" THEN 
                                getAttribute(hBuffer,"customUpdateValProc")
                              ELSE getAttribute(hBrowse,"customUpdateValProc")
             cExtraUpdateFields = IF getAttribute(hFillIn,"bufferextrafields") NE "" THEN 
                                getAttribute(hFillIn,"bufferextrafields")
                              ELSE IF getAttribute(hBuffer,"bufferextrafields") NE "" THEN 
                                getAttribute(hBuffer,"bufferextrafields")
                              ELSE getAttribute(hBrowse,"bufferextrafields")
             cExtraUpdateValues = IF getAttribute(hFillIn,"bufferextravalues") NE "" THEN 
                                getAttribute(hFillIn,"bufferextravalues")
                              ELSE IF getAttribute(hBuffer,"bufferextravalues") NE "" THEN 
                                getAttribute(hBuffer,"bufferextravalues")
                              ELSE getAttribute(hBrowse,"bufferextravalues")
             cExtraUpdateValues = REPLACE(cExtraUpdateValues,"|",CHR(1))
                              .
                                                            
      IF DYNAMIC-FUNCTION("getIsAttributeSet",hBrowse,"serverTransInputParam") THEN
        DYNAMIC-FUNCTION("setServerTransInputParam",DYNAMIC-FUNCTION("getAttribute",hBrowse,"serverTransInputParam")).
        
      bMyOK = DYNAMIC-FUNCTION("DoUpdate",hBuffer:NAME,
                    cUpdateValProc,
                    "",
                    hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                    getAttribute(hFillIn,"buffercolumn") + (IF cExtraUpdateFields NE "" THEN "," + cExtraUpdateFields ELSE ""),
                    hFillIn:SCREEN-VALUE + (IF cExtraUpdateValues NE "" THEN CHR(1) + cExtraUpdateValues ELSE ""),
                    IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraLeaveBrowseFillIn") THEN FALSE ELSE TRUE).
                    
      setAttribute(hBrowse,"serverTransReturnParam",DYNAMIC-FUNCTION("getServerTransReturnParam")).
      setAttribute(hBrowse,"serverTransReturnMessage",DYNAMIC-FUNCTION("getTransactionMessage")).
    END.
    ELSE bMyOK = TRUE.

    IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraLeaveBrowseFillIn") THEN
      RUN ExtraLeaveBrowseFillIn IN hCurrSourceProc (hFillIn,hBuffer,OUTPUT bMyOK).

    IF NOT bMyOK THEN DO:
      DoMessage (0,0,DYNAMIC-FUNCTION("getTransactionMessage"),
                 IF DYNAMIC-FUNCTION("Scandinavian") THEN "Feil i oppdatering" ELSE "Error in update","").
      setWidgetEnter(hFillIn).
      RETURN.
    END.
    ELSE bMyOK = YES.
        
  END.

  IF bMyOK THEN DO:
    IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
      hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW) NO-ERROR.
    IF hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN DO:
      IF getAttribute(hFillIn,"refreshrow") = "yes" THEN DO:
        RefreshRowids (hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
        bFreezeEndResizeBrw = YES.
        IF getAttribute(hBrowse,"enableondblclick") = "yes" OR getAttribute(hBrowse,"enableonreturn") = "yes" THEN 
          setAttribute(hBrowse,"enableupdate","yes").
        APPLY "value-changed" TO hBrowse.
        bFreezeEndResizeBrw = NO.
      END.
      ELSE 
        hBuffer:BUFFER-FIELD(getAttribute(hFillIn,"browsecolumn")):BUFFER-VALUE = hFillIn:INPUT-VALUE.
    END.
  END.
  ELSE RETURN.
END.
ELSE bMyOK = TRUE.

setAttribute(hFillIn,"last-event",cLastEvent).

IF bNewRow THEN
  setAttribute(hBrowse,"newrow","yes").

setAttribute(hBrowse,"updateErrorRow","").
setAttribute(hBrowse,"updateErrorWidget","").
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"BeforeNavBrowseFillIn") THEN
  RUN BeforeNavBrowseFillIn IN hCurrSourceProc (hFillIn,hBuffer,OUTPUT bMyOK).

IF NOT bMyOK THEN DO:
  setAttribute(hBrowse,"updateErrorRow",STRING(iFocusedRow)).
  setAttribute(hBrowse,"updateErrorWidget",STRING(hFillIn)).
  hBrowse:SELECT-ROW(iFocusedRow) NO-ERROR.
  setWidgetEnter(hFillIn).
  RETURN.
END.
setAttribute(hBrowse,"updateErrorRow","").
setAttribute(hBrowse,"updateErrorWidget","").

IF cLastEvent = "ctrl-s" AND getAttribute(hBrowse,"editWithFieldMap") = "yes" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = hBrowse
         AND ttObjectLink.cLinkType   = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    RUN InvokeMethod(ttObjectLink.hToObject,"SaveRecord").
    RETURN.
  END.  
END.  

IF getAttribute(hBrowse,"overlaybgcolnum") NE "" THEN
  iBGcolNum = INT(getAttribute(hBrowse,"overlaybgcolnum")).
IF getAttribute(hFillIn,"overlaybgcolnum") NE "" THEN
  iBGcolNum = INT(getAttribute(hFillIn,"overlaybgcolnum")).

IF VALID-HANDLE(hFillIn) THEN
  hFillIn:BGCOLOR = iBGcolNum.
IF hBrowse:FOCUSED-ROW NE ? THEN 
  NavigateFromBrowseOverlay(hFillIn,hBrowse).

IF NOT VALID-HANDLE(hFillIn) THEN
  hFillIn = WIDGET-HANDLE(getAttribute(hBrowse,"currentoverlaywidget")).
IF VALID-HANDLE(hFillIn) THEN
  setCurrentObject(hFillIn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeaveBrowseToggle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseToggle Procedure 
PROCEDURE LeaveBrowseToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the toggle
------------------------------------------------------------------------------*/
DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR hToggle              AS HANDLE NO-UNDO.
DEF VAR iBGcolNum            AS INT    NO-UNDO INIT 15.

IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "toggle-box")  THEN RETURN.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
hToggle = ttObject.hObject.
IF NOT AVAIL ttObjectLink THEN DO:
  MESSAGE "No link to browse from toggle. Programmers mistake" SKIP
          PROGRAM-NAME(1) SKIP
          VIEW-AS ALERT-BOX INFORMATION.
  RETURN.
END.
ELSE hBrowse = ttObjectLink.hToObject.

NavigateFromBrowseOverlay(hToggle,hBrowse).

IF NOT VALID-HANDLE(hToggle) THEN
  hToggle = WIDGET-HANDLE(getAttribute(hBrowse,"currentoverlaywidget")).

IF VALID-HANDLE(hToggle) THEN DO:
  IF getAttribute(hBrowse,"overlaybgcolnum") NE "" THEN
    iBGcolNum = INT(getAttribute(hBrowse,"overlaybgcolnum")).
  IF getAttribute(hToggle,"overlaybgcolnum") NE "" THEN
    iBGcolNum = INT(getAttribute(hToggle,"overlaybgcolnum")).

  hToggle:BGCOLOR = iBGcolNum.
  
  setCurrentObject(hToggle).
END.
ELSE APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeaveSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveSortSearch Procedure 
PROCEDURE LeaveSortSearch :
/*------------------------------------------------------------------------------
  Purpose:    Sole purpose is to avoid error 630 (see PKB solution id P21972) 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ttObject.hObject:MODIFIED = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeaveWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveWidget Procedure 
PROCEDURE LeaveWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the fieldMap object
------------------------------------------------------------------------------*/
DEF VAR hTmpToolbar    AS HANDLE NO-UNDO.
DEF VAR cAvailWidgets  AS CHAR   NO-UNDO.
DEF VAR hWidget        AS HANDLE NO-UNDO.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType = "toolbar" NO-ERROR.

IF hCurrWidget:MODIFIED AND AVAIL ttObjectLink AND NOT CAN-DO("new,modified",DYNAMIC-FUNCTION("getToolBarState",ttObjectLink.hToObject)) THEN DO: 
  cAvailWidgets = getAttribute(ttObject.hObject,"recordavailwidgets").
  DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"modified").

  DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
   hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
   IF VALID-HANDLE(hWidget) THEN
     hWidget:SENSITIVE = FALSE.
  END.
END.
hWidget = hCurrWidget.

IF AVAIL ttObjectLink THEN
  hTmpToolbar = ttObjectLink.hToObject.

IF NOT VALID-HANDLE(hCurrSourceProc) THEN hCurrSourceProc = SOURCE-PROCEDURE.
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"LeaveOfField") THEN 
  RUN LeaveOfField IN hCurrSourceProc (hCurrWidget:NAME).    
ELSE IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"LeaveOfFieldHandle") THEN 
  RUN LeaveOfFieldHandle IN hCurrSourceProc (hCurrWidget).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MouseMenuDownBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MouseMenuDownBrowse Procedure 
PROCEDURE MouseMenuDownBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF ttObject.hObject:TYPE = "browse" THEN DO:
  IF ttObject.hObject:FOCUSED-ROW NE ? AND NOT ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    ttObject.hObject:SELECT-FOCUSED-ROW().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MultiSortBrowseRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MultiSortBrowseRecord Procedure 
PROCEDURE MultiSortBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:    Provide for multiple column sort in browse 
  Parameters:  <none>
  Notes:      There's no event on the browse itself to fire this event so a menu-band must be defined for the browse:
              (see NewMenuBand function). The menu item must be named "MultiSortBrowse".
              Current object is hence the menu-band and the browse handle is provided 
              by following the link from the menu-band to the browse. 
------------------------------------------------------------------------------*/
DEF VAR hBrowse   AS HANDLE NO-UNDO.
DEF VAR iMaxCount AS INT NO-UNDO.
DEF VAR hColumn   AS HANDLE NO-UNDO.
DEF VAR cSortMap  AS CHAR NO-UNDO.
DEF VAR iy        AS INT NO-UNDO.
DEF VAR bDesc     AS LOG NO-UNDO.

IF AVAIL ttObject THEN DO:
  IF ttObject.hObject:TYPE NE "browse" THEN DO:
    hBrowse = DYNAMIC-FUNCTION("getLinkedObject",ttObject.hObject,"browse","from").
    IF NOT VALID-HANDLE(hBrowse) OR hBrowse:TYPE NE "browse" THEN DO:
      MESSAGE "Missing association (link) from " ttObject.cObjectType " to browse" SKIP
              "Programmers mistake"
              VIEW-AS ALERT-BOX.
      RETURN.
    END.
  END.
  ELSE hBrowse = ttObject.hObject.
END.  
ELSE RETURN.

bOK = FALSE.
RUN JBoxDBrowseSort.w (hBrowse, OUTPUT bOk).
IF NOT bOk THEN RETURN.

setAttribute(hBrowse,"querystart","0").

bDesc = DYNAMIC-FUNCTION("ComposeSortString",hBrowse,YES).

IF DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browse-search-field","from") NE ? THEN DO:
  setAttribute(hBrowse,"querywhere","").
  setAttribute(hBrowse,"prescanquerywhere","").
END.

DYNAMIC-FUNCTION("setQueryStatFields",""). /* <-- Never count or retrieve statistics when data is sortet */

iMaxCount = DYNAMIC-FUNCTION("fillBrowse",hBrowse,
                    INT(getAttribute(hBrowse,"rowstobatch")),
                    INT(getAttribute(hBrowse,"querystart")),"","","",
                    bDesc).

setBrowseSearchField(hBrowse,DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,getAttribute(hBrowse,"1stSortColumn"))).

setAttribute(hBrowse,"querystart",STRING(iMaxCount)).

IF iMaxCount = 0 THEN
  hBrowse:QUERY:QUERY-CLOSE().

APPLY "value-changed":U TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord Procedure 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object is the toolbar (or browse, when pressing INSERT-MODE). Must navigate to the buffer (fieldMap) object.
              If no fieldmap is linked there can be a browse and we insert a row to it instead               
              When using the insert-mode a toolbar with an enabled new-button must be linked to the browse and current object will be the toolbar     
              (Current object may also be the fieldMap - set programmatically)
------------------------------------------------------------------------------*/
DEF VAR hFirstWidget        AS HANDLE NO-UNDO. 
DEF VAR hWidget             AS HANDLE NO-UNDO.
DEF VAR cWidgets            AS CHAR   NO-UNDO. 
DEF VAR hCurrObject         AS HANDLE NO-UNDO.
DEF VAR cAvailWidgets       AS CHAR   NO-UNDO.
DEF VAR hTmpWidget          AS HANDLE NO-UNDO.
DEF VAR hField              AS HANDLE NO-UNDO.
DEF VAR cBufferUpdFlds      AS CHAR   NO-UNDO.
DEF VAR cScreenUpdFlds      AS CHAR   NO-UNDO.
DEF VAR rRepos              AS ROWID  NO-UNDO.
DEF VAR hBuffer             AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR cNewHandles         AS CHAR   NO-UNDO.
DEF VAR hNewWidget          AS HANDLE NO-UNDO.
DEF VAR bInsertBrowseRow    AS LOG    NO-UNDO.
DEF VAR cUpdateValProc      AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateFields  AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateValues  AS CHAR   NO-UNDO.
DEF VAR cPrimaryRowid       AS CHAR   NO-UNDO.
DEF VAR cBuffersAndFields   AS CHAR   NO-UNDO.
DEF VAR cBufferList         AS CHAR   NO-UNDO.
DEF VAR cPrimaryKeyWidgets  AS CHAR   NO-UNDO.

IF NOT DYNAMIC-FUNCTION("getIsEventAllowed",ttObject.hObject,"new") THEN RETURN.

IF ttObject.cObjectType NE "fieldMap" THEN DO:
  IF CAN-DO("browse,query",ttObject.hObject:TYPE) THEN DO:
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "toolbar"
         NO-ERROR.
    IF AVAIL ttObjectLink THEN DO:
      ASSIGN cNewHandles = DYNAMIC-FUNCTION("getToolBarHandles",ttObjectLink.hToObject,"*new*")
             hNewWidget  = WIDGET-HANDLE(ENTRY(1,cNewHandles))
             hToolbar    = ttObjectLink.hToObject.

      IF NOT DYNAMIC-FUNCTION("getActionPermission",
                              ttObject.hSourceProc:FILE-NAME,
                              DYNAMIC-FUNCTION("getObjectName",hToolbar),
                              "New") THEN
        RETURN.
      IF VALID-HANDLE(hNewWidget) AND hNewWidget:SENSITIVE THEN
        FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject.
      ELSE RETURN.
    END.
    ELSE IF NOT getAttribute(ttObject.hObject,"allowInsertKey") = "yes" THEN RETURN.
  END.
  ELSE IF hCurrWidget:TYPE = "button" THEN
     ASSIGN hNewWidget = hCurrWidget
            hToolbar   = ttObject.hObject.
  ELSE IF CAN-DO("toolbar,panel",ttObject.cObjectType) THEN DO:
    ASSIGN cNewHandles = DYNAMIC-FUNCTION("getToolBarHandles",ttObject.hObject,"*new*")
           hNewWidget  = WIDGET-HANDLE(ENTRY(1,cNewHandles))
           hToolbar    = ttObject.hObject.
    IF NOT VALID-HANDLE(hNewWidget) OR NOT hNewWidget:SENSITIVE THEN RETURN.     
  END.
  ELSE RETURN.      
  
  IF VALID-HANDLE(hNewWidget) AND CAN-QUERY(hNewWidget,"sensitive") AND NOT hNewWidget:SENSITIVE THEN RETURN.
  
  hCurrObject = ttObject.hObject.
  IF hCurrObject:TYPE = "browse" THEN
    hBrowse = hCurrObject.
  
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "browse" 
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO: 
    bInsertBrowseRow = getAttribute(ttObjectLink.hToObject,"insertbrowserow") = "yes".
    IF getAttribute(ttObjectLink.hToObject,"editUsingOverlayFieldMap") = "yes" THEN 
      setAttribute(ttObjectLink.hToObject,"useLocalData","yes").  
  END.
  
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "fieldMap" 
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    FIND FIRST ttObject 
         WHERE ttObject.hObject = ttObjectLink.hToObject
         NO-ERROR.
    IF NOT AVAIL ttObject THEN DO: 
      DoMessage (0,0,"Missing fieldMap object","JukeBox programmers mistake","").
      RETURN ERROR.
    END.
    hBuffer = ttObject.hObject.
  END.
END.
ELSE DO:
  ASSIGN hCurrObject = ttObject.hObject
         hBuffer     = ttObject.hObject.
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:      
    hToolbar = ttObjectLink.hToObject.
    IF NOT DYNAMIC-FUNCTION("getActionPermission",
                            ttObject.hSourceProc:FILE-NAME,
                            DYNAMIC-FUNCTION("getObjectName",hToolbar),
                            "New") THEN
      RETURN.
  END.
  IF NOT DYNAMIC-FUNCTION("getActionPermission",
                          ttObject.hSourceProc:FILE-NAME,
                          ttObject.cObjectName,
                          "New") THEN
    RETURN.
END.

IF ttObject.cObjectType = "fieldmap" AND NOT bInsertBrowseRow THEN DO:
  ASSIGN cWidgets = TRIM(getAttribute(hBuffer,"ScreenUpdateWidgets") + "," + 
                         (IF getAttribute(hBuffer,"ExtraUpdateWidgets") NE "" THEN
                            getAttribute(hBuffer,"ExtraUpdateWidgets") + ","
                          ELSE "") + 
                         getAttribute(hBuffer,"ScreenDisplayWidgets"),",")
         cBufferUpdFlds = getAttribute(hBuffer,"BufferUpdateFields")
         cScreenUpdFlds = getAttribute(hBuffer,"ScreenUpdateFields")
         .
  
  DO ix = 1 TO NUM-ENTRIES(cWidgets):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cWidgets)).
    IF hWidget:TYPE NE "button" THEN DO:
      hField = hBuffer:BUFFER-FIELD(hWidget:NAME) NO-ERROR.
      IF ERROR-STATUS:GET-NUMBER(1) = 7351 THEN
        hField = hBuffer:BUFFER-FIELD(ENTRY(LOOKUP(hWidget:NAME,cScreenUpdFlds),cBufferUpdFlds)) NO-ERROR.
  
      IF hWidget:TYPE = "combo-box" AND VALID-HANDLE(hField) AND (hWidget:LOOKUP(STRING(hField:INITIAL)) = 0 OR hField:INITIAL = ? OR hField:INITIAL = "") THEN
        DYNAMIC-FUNCTION("ClearComboBox",hWidget).
      IF hWidget:PRIVATE-DATA NE ? AND hWidget:PRIVATE-DATA NE "" THEN DO: /* ref pkb article 47199 - initial value not accessible */
        IF hWidget:DATA-TYPE = "date" THEN DO:
          IF hWidget:PRIVATE-DATA BEGINS "today " AND NUM-ENTRIES(hWidget:PRIVATE-DATA," ") = 3 THEN DO:
            IF ENTRY(2,hWidget:PRIVATE-DATA," ") = "+" THEN
              hWidget:SCREEN-VALUE = STRING(TODAY + INT(ENTRY(3,hWidget:PRIVATE-DATA," "))) NO-ERROR.
            ELSE IF ENTRY(2,hWidget:PRIVATE-DATA," ") = "-" THEN  
              hWidget:SCREEN-VALUE = STRING(TODAY - INT(ENTRY(3,hWidget:PRIVATE-DATA," "))) NO-ERROR.
          END.          
          ELSE IF hWidget:PRIVATE-DATA = "today" THEN
            hWidget:SCREEN-VALUE = STRING(TODAY). 
          ELSE IF hWidget:PRIVATE-DATA = "getFirstDayOfMonth" THEN
            hWidget:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION("getFirstDayOfMonth",TODAY)).
          ELSE IF hWidget:PRIVATE-DATA = "getFirstDayOfPriorMonth" THEN
            hWidget:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION("getFirstDayOfPriorMonth",TODAY)).
        END.
        ELSE hWidget:SCREEN-VALUE = hWidget:PRIVATE-DATA NO-ERROR.
      END.  
      ELSE IF VALID-HANDLE(hField) THEN hWidget:SCREEN-VALUE = hField:INITIAL NO-ERROR.
   
      IF ERROR-STATUS:ERROR THEN
        CASE hWidget:DATA-TYPE:
          WHEN "date"    THEN hWidget:SCREEN-VALUE = ? NO-ERROR.
          WHEN "decimal" THEN hWidget:SCREEN-VALUE = STRING(0) NO-ERROR.
          WHEN "integer" THEN hWidget:SCREEN-VALUE = STRING(0) NO-ERROR.
          WHEN "logical" THEN hWidget:SCREEN-VALUE = STRING(NO) NO-ERROR.
          OTHERWISE hWidget:SCREEN-VALUE = "" NO-ERROR.
        END CASE.
      hWidget:MODIFIED = FALSE NO-ERROR.
      IF ix = 1 THEN hFirstWidget = hWidget.
    END.  
  END.
  
  cPrimaryKeyWidgets = getAttribute(hBuffer,"primaryKeyWidgets").
  IF cPrimaryKeyWidgets NE "" THEN
    hFirstWidget = WIDGET-HANDLE(ENTRY(1,cPrimaryKeyWidgets)) NO-ERROR.
  
  cAvailWidgets = getAttribute(hBuffer,"recordavailwidgets").
  DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
   hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
   IF VALID-HANDLE(hWidget) THEN 
     hWidget:SENSITIVE = FALSE NO-ERROR.
  END.

  FOR EACH ttObjectLink 
      WHERE ttObjectLink.hFromObject = ttObject.hObject
        AND ttObjectLink.cLinkType = "dotNetDisplay"
      :
    /* Target: JBoxWrapWindowInForm */
/*    PUBLISH "dotNetNew" (ttObject.hObject,ttObjectLink.hToObject,ttObjectLink.cLinkInfo). */
    PUBLISH "dotNetMethod" ("dotNetNew",ttObject.hObject,ttObjectLink.hToObject,ttObjectLink.cLinkInfo,?).
  END.

  /* Special handling for one commit of multiple (new) records: */
  IF getAttribute(hCurrObject,"commitstate") = "off" THEN DO:
    setAttribute(hBuffer,"uselocaldata","yes").
    setAttribute(hCurrObject,"commitstate","on").

    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = hCurrObject
           AND ttObjectLink.cLinkType   = "browse"
         NO-ERROR.
    IF NOT AVAIL ttObjectLink THEN DO:
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = hCurrObject
             AND ttObjectLink.cLinkType   = "query"
           NO-ERROR.
      IF AVAIL ttObjectLink THEN DO:
        setAttribute(ttObjectLink.hToObject,"uselocaldata","yes").
        ttObjectLink.hToObject:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().
        ttObjectLink.hToObject:QUERY-OPEN().
      END.
    END.
    ELSE DO:
      setAttribute(ttObjectLink.hToObject,"uselocaldata","yes").
      ttObjectLink.hToObject:QUERY:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().
      ttObjectLink.hToObject:QUERY:QUERY-OPEN().
    END.
    IF AVAIL ttObjectLink THEN DO:
      FIND bttObjectLink 
           WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
             AND bttObjectLink.cLinkType = "fieldMap"
           NO-ERROR.
      IF AVAIL bttObjectLink THEN
        setAttribute(bttObjectLink.hToObject,"uselocaldata","yes").
    END.
  END.
 
  DYNAMIC-FUNCTION("setToolbar",hToolbar,"new").

  IF VALID-HANDLE(hFirstWidget) THEN DO:
    hTmpWidget = hCurrWidget.
    APPLY "entry" TO hFirstWidget.
    hCurrWidget = hTmpWidget.
  END.
END.
ELSE DO:

  IF VALID-HANDLE(hBrowse) THEN hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
  ELSE DO:
    FIND FIRST ttObjectLink 
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "browse"
         NO-ERROR.
    /* Insert record to the browse: */
    IF AVAIL ttObjectLink THEN DO:
      ASSIGN hBrowse = ttObjectLink.hToObject
             hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
      FOR EACH bttObjectLink
          WHERE bttObjectLink.hFromObject = hBrowse
            AND bttObjectLink.cLinkType   = "browseoverlay":
        IF VALID-HANDLE(bttObjectLink.hToObject) AND bttObjectLink.hToObject:TYPE = "combo-box" THEN
          DYNAMIC-FUNCTION("ClearComboBox",bttObjectLink.hToObject).
      END.
    END.
  END.
  IF VALID-HANDLE(hBuffer) AND NOT getAttribute(hBrowse,"uselocaldata") = "yes" THEN DO:
    DYNAMIC-FUNCTION("setPostUpdProc",IF getAttribute(hBuffer,"postUpdateProc") NE "" THEN
                                        getAttribute(hBuffer,"postUpdateProc")
                                      ELSE getAttribute(hBrowse,"postUpdateProc")).

    ASSIGN cUpdateValProc = IF getAttribute(hBrowse,"customUpdateValProc") NE "" THEN 
                              LEFT-TRIM(RIGHT-TRIM(getAttribute(hBrowse,"customUpdateValProc") + ",=" +
                                                   getAttribute(hBrowse,"customCreateProc"),",="),",")

                            ELSE IF getAttribute(hBuffer,"customUpdateValProc") NE "" THEN
                              LEFT-TRIM(RIGHT-TRIM(getAttribute(hBuffer,"customUpdateValProc") + ",=" +
                                                   getAttribute(hBuffer,"customCreateProc"),",="),",")

                            ELSE IF getAttribute(hBrowse,"customCreateProc") NE "" THEN
                              ",=" + getAttribute(hBrowse,"customCreateProc")

                            ELSE IF getAttribute(hBuffer,"customCreateProc") NE "" THEN
                              ",=" + getAttribute(hBuffer,"customCreateProc")

                            ELSE ""

           cExtraUpdateFields = TRIM(getAttribute(hBrowse,"childlinkfields") + "," + 
                            (IF getAttribute(hBrowse,"bufferextrafields") NE "" THEN 
                               getAttribute(hBrowse,"bufferextrafields")
                             ELSE getAttribute(hBuffer,"bufferextrafields")),",")
           cExtraUpdateValues = DYNAMIC-FUNCTION("getParentLinkValues",hBrowse) +  
                            (IF getAttribute(hBrowse,"bufferextravalues") NE "" THEN 
                               "|" + getAttribute(hBrowse,"bufferextravalues")
                             ELSE IF getAttribute(hBuffer,"bufferextravalues") NE "" THEN 
                               "|" + getAttribute(hBuffer,"bufferextravalues")
                             ELSE "")
                            .

    IF DYNAMIC-FUNCTION("DoCreate",hBuffer:NAME,
                  cUpdateValProc,
                  cExtraUpdateFields,
                  cExtraUpdateValues,
                  TRUE) THEN DO:

      IF getAttribute(hBrowse,"orgbuffersandfields") NE "" THEN DO:
        ASSIGN cBuffersAndFields = getAttribute(hBrowse,"buffersandfields")
               cBufferList       = getAttribute(hBrowse,"bufferlist").
        DO ix = 1 TO NUM-ENTRIES(cBuffersAndFields):
          IF LOOKUP(ENTRY(1,ENTRY(ix,cBuffersAndFields),";"),cBufferList) = 1 THEN
            cPrimaryRowid = hBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)):BUFFER-VALUE.
        END.
        IF cPrimaryRowid = "" THEN DO:          
          MESSAGE PROGRAM-NAME(1) SKIP
                  "Cannot save record. The buffersequence is not equal original setting"
                  VIEW-AS ALERT-BOX.
          RETURN.
        END.
      END.
  
      hBuffer:BUFFER-CREATE.
  
      rRepos = hBuffer:ROWID.
      DYNAMIC-FUNCTION("DoRefetchTrans",hBuffer,"FIRST","").

      IF getAttribute(hBuffer,"calcfieldproc") NE "" THEN
        DYNAMIC-FUNCTION("setCalcFieldProc",getAttribute(hBuffer,"calcfieldproc")).
      ELSE IF getAttribute(hBrowse,"calcfieldproc") NE "" THEN
        DYNAMIC-FUNCTION("setCalcFieldProc",getAttribute(hBrowse,"calcfieldproc")).

      IF cPrimaryRowid NE "" THEN DO:
        DYNAMIC-FUNCTION("setCurrentRowid",cPrimaryRowid).
        DYNAMIC-FUNCTION("refreshRow",hBrowse,
                          getAttribute(hBrowse,"orgbuffersandfields"),
                          getAttribute(hBrowse,"orgqueryjoin")).
      END.
      ELSE
        DYNAMIC-FUNCTION("refreshRow",hBrowse,
                          DYNAMIC-FUNCTION("getAttribute",hBrowse,"buffersandfields"),
                          DYNAMIC-FUNCTION("getAttribute",hBrowse,"queryjoin")).
  
      IF hBrowse:QUERY:PREPARE-STRING = ? THEN
        hBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
      hBrowse:QUERY:QUERY-OPEN().
      hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
      hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
  
      IF getAttribute(hBrowse,"enableondblclick") = "yes" THEN 
        setAttribute(hBrowse,"enableupdate","yes").

      APPLY "value-changed" TO hBrowse.
      DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hBrowse).

      setAttribute(hBrowse,"newrow","yes").

      setAttribute(hBrowse,"rowsadded",STRING(INT(getAttribute(hBrowse,"rowsadded")) + 1)).
      DYNAMIC-FUNCTION("ViewRecordCount",hBrowse).

    END.
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  ELSE IF VALID-HANDLE(hBuffer) AND getAttribute(hBrowse,"uselocaldata") = "yes" THEN DO:
    hBuffer:BUFFER-CREATE.
    rRepos = hBuffer:ROWID.
    IF hBrowse:QUERY:PREPARE-STRING = ? THEN
      hBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
    hBrowse:QUERY:QUERY-OPEN().
    hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
    hBrowse:QUERY:REPOSITION-TO-ROWID(rRepos).
    IF getAttribute(hBrowse,"enableondblclick") = "yes" OR getAttribute(hBrowse,"enableOnToolbarClick") = "yes" THEN 
      setAttribute(hBrowse,"enableupdate","yes").
    APPLY "value-changed" TO hBrowse.
    setAttribute(hBrowse,"newrow","yes").
    IF getAttribute(hBrowse,"enableOnToolbarClick") = "yes" THEN DO:
      DYNAMIC-FUNCTION("setToolbar",hToolbar,"new").
      cWidgets = getAttribute(hBuffer,"ScreenUpdateWidgets").
      hFirstWidget  = WIDGET-HANDLE(ENTRY(1,cWidgets)) NO-ERROR.
      IF VALID-HANDLE(hFirstWidget) THEN DO:
        hTmpWidget = hCurrWidget.
        APPLY "entry" TO hFirstWidget.
        hCurrWidget = hTmpWidget.
      END.
    END.  
  END.
  ELSE DO:
    DYNAMIC-FUNCTION("setToolbar",hToolbar,"new").
    FIND FIRST ttObjectLink 
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query"
         NO-ERROR.
  END.
  
  IF AVAIL ttObjectLink THEN DO:
    cAvailWidgets = getAttribute(ttObjectLink.hToObject,"recordavailwidgets").
    DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
      hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
      IF VALID-HANDLE(hWidget) THEN
        hWidget:SENSITIVE = FALSE NO-ERROR.
    END.
  END.
END.


IF getAttribute(hCurrObject,"CloseAndDisableChilds") = "" THEN
  CloseAndDisableChilds(hCurrObject).

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NextRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NextRecord Procedure 
PROCEDURE NextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Current object is toolbar     
------------------------------------------------------------------------------*/
DEF VAR iTotRec         AS INT NO-UNDO.
DEF VAR hExternalBrowse AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.

IF ttObject.cObjectType = "toolbar" THEN DO:
  hToolbar = ttObject.hObject.

  FIND FIRST ttObjectLink 
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse" 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink 
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query" 
         NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    hExternalBrowse = WIDGET-HANDLE(getAttribute(ttObjectLink.hToObject,"ExternalBrowse")) NO-ERROR.     
    IF VALID-HANDLE(hExternalBrowse) THEN DO:
      hExternalBrowse:SELECT-NEXT-ROW().
      APPLY "value-changed" TO hExternalBrowse.
      RETURN.
    END.
  END.
  IF ttObjectLink.cLinkType = "query" AND 
     CAN-FIND(FIRST bttObjectLink WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
                                    AND bttObjectLink.cLinkType = "onetoone") THEN DO:
    FIND bttObjectLink 
         WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
           AND bttObjectLink.cLinkType = "onetoone".
    FIND ttObject WHERE ttObject.hObject = bttObjectLink.hToObject NO-ERROR.
  END.
  ELSE FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject NO-ERROR.
END.

ASSIGN hCurrObject     = ttObject.hObject
       hCurrSourceProc = ttObject.hSourceProc
       iTotRec = INT(getAttribute(hCurrObject,"totalcount"))
       .
setAttribute(hCurrObject,"querydir","").

IF hCurrObject:TYPE = "browse" AND hCurrObject:NUM-ITERATIONS > 0 THEN DO:

  hCurrObject:SELECT-FOCUSED-ROW().
  IF NOT hCurrObject:SELECT-NEXT-ROW() THEN DO:
    IF STRING(hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) NE getAttribute(hCurrObject,"lastrowid") THEN DO:
      ASSIGN bSetCurrHandles = FALSE
             hCurrWidget    = hCurrObject
             .
      RUN DoProcessEvent(0,"off-end").
      hCurrObject:SELECT-FOCUSED-ROW().
    END.
    IF STRING(hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(hCurrObject,"lastrowid") THEN DO:
      hCurrObject:SELECT-FOCUSED-ROW().
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"last").
      ELSE DO:
        FIND FIRST ttObjectLink
             WHERE ttObjectLink.hFromObject = hCurrObject
               AND ttObjectLink.cLinkType   = "toolbar"
             NO-ERROR.
        IF AVAIL ttObjectLink THEN
          DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"last").
      END.
    END.
  END.
  ELSE DO:
    ASSIGN bSetCurrHandles = FALSE
           hCurrWidget    = hCurrObject
           .
    RUN DoProcessEvent(0,"value-changed").
    IF STRING(hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(hCurrObject,"lastrowid") THEN DO:
      hCurrObject:SELECT-FOCUSED-ROW().
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"last").
      ELSE DO:
        FIND FIRST ttObjectLink
             WHERE ttObjectLink.hFromObject = hCurrObject
               AND ttObjectLink.cLinkType   = "toolbar"
             NO-ERROR.
        IF AVAIL ttObjectLink THEN
          DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"last").
      END.
    END.
  END.
END.
ELSE IF hCurrObject:TYPE NE "browse" THEN DO:    
  IF STRING(hCurrObject:GET-BUFFER-HANDLE(1):ROWID) NE getAttribute(hCurrObject,"lastrowid") THEN DO:      
    hCurrObject:GET-NEXT().

    IF NOT hCurrObject:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
      ASSIGN bSetCurrHandles = FALSE
             hCurrWidget    = hCurrObject
             .
      RUN DoProcessEvent(0,"off-end").        
    END.
    ELSE DO:
      ASSIGN bSetCurrHandles  = FALSE
             hCurrWidget      = hCurrObject
             .
      RUN DoProcessEvent(0,"value-changed").
    END.    
  END.
  IF STRING(hCurrObject:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(hCurrObject,"lastrowid") AND VALID-HANDLE(hToolbar) THEN
    DYNAMIC-FUNCTION("setToolbar",hToolbar,"last").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NoteRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NoteRecord Procedure 
PROCEDURE NoteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       If current object is the toolbar we must navigate to the browse object
               "notedialogbox" attribute is currently irrelevant cause we're only supporting dialog
               also hNote is (currently) meaningless
               
           NB! Usage is always via DisplayRecord (before SUPER) where you set the current note value
               as an attribute on the toolbar (or optionally the browse or query) containing the current note value:
               DYNAMIC-FUNCTION("setAttribute",hToolbar,"currentnotevalue",<some text>).
               To enable the toolbar to change image depending on wether there is a note value or not
               the note attribute must be set on the toolbar.
               You can also set the attribute on the query object for the note function to work 
------------------------------------------------------------------------------*/
DEF VAR hNote            AS HANDLE NO-UNDO.
DEF VAR hCurrObject      AS HANDLE NO-UNDO.
DEF VAR cNoteWin         AS CHAR   NO-UNDO.
DEF VAR hCurrSourceProc  AS HANDLE NO-UNDO.
DEF VAR hBrowseOrQuery   AS HANDLE NO-UNDO.
DEF VAR hNoteButton      AS HANDLE NO-UNDO.
DEF VAR bDialog          AS LOG    NO-UNDO.
DEF VAR cNote            AS CHAR   NO-UNDO.
DEF VAR hToolbar         AS HANDLE NO-UNDO.
DEF VAR cNoteImage       AS CHAR   NO-UNDO.
DEF VAR bReadOnly        AS LOG    NO-UNDO.
DEF VAR bSplitEditor     AS LOG    NO-UNDO.
DEF VAR bAddTimeStamp    AS LOG    NO-UNDO.
DEF VAR cEditorDirection AS CHAR   NO-UNDO.

IF ttObject.cObjectType = "toolbar" THEN DO:
  ASSIGN hNoteButton = hCurrWidget
         hToolbar    = ttObject.hObject.
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "browse" 
       NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject 
           AND ttObjectLink.cLinkType   = "query" 
         NO-ERROR.
  
  IF NOT AVAIL ttObjectLink THEN RETURN.
  ELSE hBrowseOrQuery = ttObjectLink.hToObject.
       
END.
ELSE DO:
  hBrowseOrQuery = ttObject.hObject.
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN 
    ASSIGN hToolbar    = ttObjectLink.hToObject
           hNoteButton = DYNAMIC-FUNCTION("getEventWidget",ttObjectLink.hToObject,"Note","").
END.

ASSIGN hCurrObject      = ttObject.hObject
       hCurrSourceProc  = ttObject.hSourceProc
       cNote            = getAttribute(hCurrObject,"NoteCurrentValue")
       bDialog          = getAttribute(hCurrObject,"NoteDialogBox") = "yes"
       bReadOnly        = getAttribute(hCurrObject,"NoteReadOnly") = "yes"
       bAddTimeStamp    = getAttribute(hCurrObject,"NoteAddTimeStamp") = "yes"
       bSplitEditor     = getAttribute(hCurrObject,"NoteSplitEditor") = "yes"
       cEditorDirection = getAttribute(hCurrObject,"NoteEditDirection") /* up / down (default) */
       .

IF hCurrObject = hBrowseOrQuery AND VALID-HANDLE(hNoteButton) THEN DO:
  IF getAttribute(hToolbar,"NoteCurrentValue") NE "" THEN
    cNote = getAttribute(hToolbar,"NoteCurrentValue").
  IF NOT bDialog THEN
    bDialog = getAttribute(hToolbar,"NoteDialogBox") = "yes".
  IF NOT bReadOnly THEN
    bReadOnly = getAttribute(hToolbar,"NoteReadOnly") = "yes".
  IF NOT bAddTimeStamp THEN
    bAddTimeStamp = getAttribute(hToolbar,"NoteAddTimeStamp") = "yes".
  IF NOT bSplitEditor THEN
    bSplitEditor = getAttribute(hToolbar,"NoteAddTimeStamp") = "yes".
  IF getAttribute(hToolbar,"NoteEditDirection") NE "" THEN
    cEditorDirection = getAttribute(hToolbar,"NoteEditDirection").
END.
ELSE IF hCurrObject = hBrowseOrQuery THEN DO:
  IF getAttribute(hBrowseOrQuery,"NoteCurrentValue") NE "" THEN
    cNote = getAttribute(hBrowseOrQuery,"NoteCurrentValue").
  IF NOT bDialog THEN
    bDialog = getAttribute(hBrowseOrQuery,"NoteDialogBox") = "yes".
  IF NOT bReadOnly THEN
    bReadOnly = getAttribute(hBrowseOrQuery,"NoteReadOnly") = "yes".
  IF NOT bAddTimeStamp THEN
    bAddTimeStamp = getAttribute(hBrowseOrQuery,"NoteAddTimeStamp") = "yes".
  IF NOT bSplitEditor THEN
    bSplitEditor = getAttribute(hBrowseOrQuery,"NoteAddTimeStamp") = "yes".
  IF getAttribute(hBrowseOrQuery,"NoteEditDirection") NE "" THEN
    cEditorDirection = getAttribute(hBrowseOrQuery,"NoteEditDirection").
END.

IF NOT VALID-HANDLE(hNote) THEN DO:
  cNoteWin = getAttribute(hCurrObject,"notewindow").
  IF cNoteWin = "" THEN cNoteWin = getAttribute(SESSION,"notewindow").
  IF cNoteWin = "" THEN cNoteWin = "JBoxDEditor.w".
  RUN VALUE(cNoteWin) 
      (INPUT-OUTPUT cNote,
       bReadOnly,
       bAddTimeStamp,
       bSplitEditor,
       cEditorDirection,
       OUTPUT bOk). 
  
  IF bOK THEN DO:
    IF VALID-HANDLE(hToolbar) THEN DO:
      setAttribute(hToolbar,"NoteCurrentValue",cNote).
      setAttribute(hToolbar,"NoteCurrentValueChanged","yes").
    END.
    ELSE DO:
      setAttribute(hBrowseOrQuery,"NoteCurrentValue",cNote).
      setAttribute(hBrowseOrQuery,"NoteCurrentValueChanged","yes").
    END.
  END.
  ELSE DO:
    IF VALID-HANDLE(hToolbar) THEN 
      setAttribute(hToolbar,"NoteCurrentCalueChanged","").
    ELSE
      setAttribute(hBrowseOrQuery,"NoteCurrentValueChanged","").
  END.
  IF VALID-HANDLE(hNoteButton) THEN DO:
    IF cNote NE "" THEN DO:
      IF getAttribute(hToolbar,"ActiveNoteButton") NE "" THEN
        cNoteImage = getAttribute(hToolbar,"ActiveNoteButton").
      ELSE  
        cNoteImage = getAttribute(SESSION,"ActiveNoteButton").
    END.  
    ELSE DO:
      IF getAttribute(hToolbar,"PassiveNoteButton") NE "" THEN
        cNoteImage = getAttribute(hToolbar,"PassiveNoteButton").
      ELSE  
        cNoteImage = getAttribute(SESSION,"PassiveNoteButton").
    END.  
        
    &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
      hNoteButton:LOAD-IMAGE(cNoteImage) NO-ERROR.
    &ENDIF  
  END.
END.

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OffEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffEnd Procedure 
PROCEDURE OffEnd :
/*------------------------------------------------------------------------------
  Purpose:    Current object is the browse 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iMaxCount   AS INT    NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.

IF getAttribute(ttObject.hObject,"lastrowid") NE "" 
   OR getAttribute(ttObject.hObject,"uselocaldata") = "yes" THEN RETURN.

DYNAMIC-FUNCTION("setQueryStatFields",""). /* <-- Never count or retrieve statistics when paging */

IF ttObject.cObjectType = "BROWSE" THEN DO:    
  IF (LAST-EVENT:LABEL = "scroll-notify" AND getAttribute(ttObject.hObject,"lastrowid") NE "") 
     OR STRING(ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(ttObject.hObject,"lastrowid") 
     OR getAttribute(ttObject.hObject,"uselocaldata") = "yes" THEN RETURN.

  iMaxCount = DYNAMIC-FUNCTION("fillBrowse",ttObject.hObject,
                      INT(getAttribute(ttObject.hObject,"rowstobatch")),
                      INT(getAttribute(ttObject.hObject,"querystart")),"","","",
                      getAttribute(ttObject.hObject,"querydesc") = "desc").
END.
ELSE DO:
  IF getAttribute(ttObject.hObject,"lastrowid") NE ""
     OR STRING(ttObject.hObject:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(ttObject.hObject,"lastrowid") 
     OR getAttribute(ttObject.hObject,"uselocaldata") = "yes" THEN RETURN.
    
  iMaxCount = DYNAMIC-FUNCTION("fillQuery",ttObject.hObject,
                      INT(getAttribute(ttObject.hObject,"rowstobatch")),
                      INT(getAttribute(ttObject.hObject,"querystart")),"","").
END.

setAttribute(ttObject.hObject,"querystart",STRING(iMaxCount)).

ASSIGN bSetCurrHandles = FALSE
       hCurrWidget    = ttObject.hObject
       .
RUN DoProcessEvent(0,"value-changed").

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OffHome) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffHome Procedure 
PROCEDURE OffHome :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iMaxCount   AS INT NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.


IF getAttribute(ttObject.hObject,"firstrowid") NE "" 
   OR getAttribute(ttObject.hObject,"uselocaldata") = "yes" THEN RETURN.

DYNAMIC-FUNCTION("setQueryStatFields",""). /* <-- Never count or retrieve statistics when paging */

IF ttObject.cObjectType = "browse" THEN DO:
  ttObject.hObject:QUERY:GET-FIRST().
  IF STRING(ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(ttObject.hObject,"firstrowid") THEN DO:
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = hCurrObject 
           AND ttObjectLink.cLinkType   = "toolbar"
         NO-ERROR.
    IF AVAIL ttObjectLink THEN
      DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"first").
    RETURN.
  END.
  
  setAttribute(ttObject.hObject,"querydir","desc").
  
  iMaxCount = DYNAMIC-FUNCTION("fillBrowse",ttObject.hObject,
                      INT(getAttribute(ttObject.hObject,"rowstobatch")),
                      INT(getAttribute(ttObject.hObject,"querystart")),"","","",
                      getAttribute(ttObject.hObject,"querydesc") = "desc").
END.
ELSE DO:
  ttObject.hObject:GET-FIRST().
  IF STRING(ttObject.hObject:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(ttObject.hObject,"firstrowid") THEN DO:
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = hCurrObject 
           AND ttObjectLink.cLinkType   = "toolbar"
         NO-ERROR.
    IF AVAIL ttObjectLink THEN
      DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"first").
    RETURN.
  END.
  
  setAttribute(ttObject.hObject,"querydir","desc").
    
  iMaxCount = DYNAMIC-FUNCTION("fillQuery",ttObject.hObject,
                      INT(getAttribute(ttObject.hObject,"rowstobatch")),
                      INT(getAttribute(ttObject.hObject,"querystart")),"","").
END.

setAttribute(ttObject.hObject,"querystart",STRING(iMaxCount)).

ASSIGN bSetCurrHandles = FALSE
       hCurrWidget    = ttObject.hObject
       .
RUN DoProcessEvent(0,"value-changed").

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpenQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery Procedure 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is (or must be set to) the browse object.
------------------------------------------------------------------------------*/
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR iMaxCount         AS INT    NO-UNDO.
DEF VAR bDesc             AS LOG    NO-UNDO.
DEF VAR cSortMap          AS CHAR   NO-UNDO.
DEF VAR iy                AS INT    NO-UNDO.
DEF VAR hMyTmpQueryObject AS HANDLE NO-UNDO.
DEF VAR cQueryStat        AS CHAR   NO-UNDO.

SaveModified(ttObject.hObject).

ix = 0.
IF hTmpObject = ? THEN hTmpObject = ttObject.hObject.

hMyTmpQueryObject = hTmpObject.

IF getAttribute(hTmpObject,"editUsingOverlayFieldMap") = "yes" THEN DO:
  setAttribute(hTmpObject,"useLocalData","").
  setAttribute(hTmpObject,"enableUpdate","").
END.

bDesc = DYNAMIC-FUNCTION("ComposeSortString",hTmpObject,NO).

setAttribute(hTmpObject,"querystart","0").

cQueryStat = TRIM((IF getAttribute(hTmpObject,"getrecordcount") = "yes" THEN "rowcount," ELSE "")
                  + getAttribute(hTmpObject,"querystatfields"),",").
IF cQueryStat NE "" THEN 
  DYNAMIC-FUNCTION("setQueryStatFields",cQueryStat).

IF NOT getAttribute(hTmpObject,"keepsearchvalue") = "yes" THEN
  setAttribute(hTmpObject,"prescanquerywhere","").

IF hTmpObject:TYPE = "browse" THEN DO:
  IF NOT hTmpObject:QUERY:IS-OPEN THEN
    hTmpObject:QUERY:QUERY-OPEN() NO-ERROR.

  cSortMap = getAttribute(hTmpObject,"sortmap").
  DO iy = 1 TO NUM-ENTRIES(cSortMap):
    IF ENTRY(1,ENTRY(iy,cSortMap),";") = getAttribute(hTmpObject,"querysort") THEN DO:
      setAttribute(hTmpObject,"querysort",ENTRY(2,ENTRY(iy,cSortMap),";")).
      LEAVE.
    END.
  END.

  IF getAttribute(hTmpObject,"distinctcolumns") NE "" AND getAttribute(hTmpObject,"uselocaldata") NE "yes" 
     AND getAttribute(hTmpObject,"GoLocalWhenSmallDistinctCount") NE "no" THEN 
    setAttribute(hTmpObject,"GoLocalWhenSmallDistinctCount","yes").
  ELSE IF getAttribute(hTmpObject,"GoLocalWhenSmallDistinctCount") = "yes" THEN
    setAttribute(hTmpObject,"uselocaldata","").
  
  iMaxCount = DYNAMIC-FUNCTION("fillBrowse",hTmpObject,
                      INT(getAttribute(hTmpObject,"rowstobatch")),
                      INT(getAttribute(hTmpObject,"querystart")),"","","",
                      bDesc).
END.
/* Query: */
ELSE DO:
  IF NOT hTmpObject:IS-OPEN THEN
    hTmpObject:QUERY-OPEN() NO-ERROR.
  
  iMaxCount = DYNAMIC-FUNCTION("fillQuery",hTmpObject,
                      INT(getAttribute(hTmpObject,"rowstobatch")),
                      INT(getAttribute(hTmpObject,"querystart")),"","").
END.

IF DYNAMIC-FUNCTION("getTransactionMessage") NE "" 
   AND NOT getAttribute(hTmpObject,"uselocaldata") = "yes" 
   AND NOT DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*(234)*"
   THEN DO:
  IF DYNAMIC-FUNCTION("getTransactionMessage") BEGINS "error" THEN    
    DoMessage (-1,0,DYNAMIC-FUNCTION("getTransactionMessage"),
               IF DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*warning*" THEN "Warning" 
               ELSE "JukeBox, Query error:"
               ,"").
  ELSE IF DYNAMIC-FUNCTION("getTransactionMessage") BEGINS "log" THEN DO:
    /* output to logfile */
  END.                
END.             

setAttribute(hTmpObject,"querystart",STRING(iMaxCount)).

IF iMaxCount = 0 THEN DO:
  IF hTmpObject:TYPE = "browse" THEN
    hTmpObject:QUERY:QUERY-CLOSE().
  ELSE
    hTmpObject:QUERY-CLOSE().
END.

FOR EACH ttEvent
    WHERE ttEvent.hObject     = hTmpObject
      AND ttEvent.cWidgetType = "menu-item"
      AND NOT CAN-DO("new,undo",ttEvent.cAction):
  IF VALID-HANDLE(ttEvent.hWidget) THEN
    ttEvent.hWidget:SENSITIVE = IF iMaxCount = 0 THEN FALSE ELSE TRUE.
END.

IF hTmpObject:TYPE = "browse" THEN 
  APPLY "value-changed":U TO hTmpObject.
ELSE DO:   
  ASSIGN bSetCurrHandles = FALSE
         hCurrWidget     = hTmpObject.
  RUN DoProcessEvent(0,"value-changed").
/*   ApplyEvent(hTmpObject,"value-changed"). */
END.

IF getAttribute(hMyTmpQueryObject,"getrecordcount") = "yes" AND hMyTmpQueryObject:TYPE = "browse" THEN
  APPLY "entry" TO hMyTmpQueryObject.

hTmpObject = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrevRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevRecord Procedure 
PROCEDURE PrevRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Current object is the toolbar      
------------------------------------------------------------------------------*/
DEF VAR iTotRec         AS INT NO-UNDO.
DEF VAR rCurrRowid      AS ROWID NO-UNDO.
DEF VAR hExternalBrowse AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.

IF ttObject.cObjectType = "toolbar" THEN DO:
  hToolbar = ttObject.hObject.
  FIND FIRST ttObjectLink 
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse" 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink 
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query" 
         NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    hExternalBrowse = WIDGET-HANDLE(getAttribute(ttObjectLink.hToObject,"ExternalBrowse")) NO-ERROR.     
    IF VALID-HANDLE(hExternalBrowse) THEN DO:
      hExternalBrowse:SELECT-PREV-ROW().
      APPLY "value-changed" TO hExternalBrowse.
      RETURN.
    END.
  END.
  IF ttObjectLink.cLinkType = "query" AND 
     CAN-FIND(FIRST bttObjectLink WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
                                    AND bttObjectLink.cLinkType = "onetoone") THEN DO:
    FIND bttObjectLink 
         WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
           AND bttObjectLink.cLinkType = "onetoone".
    FIND ttObject WHERE ttObject.hObject = bttObjectLink.hToObject NO-ERROR.
  END.
  ELSE FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject NO-ERROR.

  IF NOT AVAIL ttObject THEN RETURN.
END.

ASSIGN hCurrObject     = ttObject.hObject
       hCurrSourceProc = ttObject.hSourceProc
       iTotRec = INT(getAttribute(hCurrObject,"totalcount"))
       .
       
IF hCurrObject:TYPE = "browse" THEN
  setAttribute(hCurrObject,"querydir","desc").

IF hCurrObject:TYPE = "browse" AND hCurrObject:NUM-ITERATIONS > 0 THEN DO:
  hCurrObject:SELECT-FOCUSED-ROW().
  IF NOT hCurrObject:SELECT-PREV-ROW() THEN DO:
    IF STRING(hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) NE getAttribute(hCurrObject,"firstrowid") THEN DO:
      ASSIGN bSetCurrHandles = FALSE
             hCurrWidget    = hCurrObject
             rCurrRowid     = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID       
             .
      RUN DoProcessEvent(0,"off-home").

      hCurrObject:QUERY:REPOSITION-TO-ROWID(rCurrRowid). 
      hCurrObject:SELECT-FOCUSED-ROW().
      hCurrObject:SELECT-PREV-ROW().
    END.
    IF STRING(hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(hCurrObject,"firstrowid") THEN DO:
      hCurrObject:SELECT-FOCUSED-ROW().
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = hCurrObject
             AND ttObjectLink.cLinkType   = "toolbar"
           NO-ERROR.
      IF AVAIL ttObjectLink THEN
        DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"first").
    END.
  END.
  ELSE DO:
    ASSIGN bSetCurrHandles = FALSE
           hCurrWidget    = hCurrObject
           .
    RUN DoProcessEvent(0,"value-changed").
    IF STRING(hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(hCurrObject,"firstrowid") THEN DO:
      hCurrObject:SELECT-FOCUSED-ROW().
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"first").
      ELSE DO:
        FIND FIRST ttObjectLink
             WHERE ttObjectLink.hFromObject = hCurrObject
               AND ttObjectLink.cLinkType   = "toolbar"
             NO-ERROR.
        IF AVAIL ttObjectLink THEN
          DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"first").
      END.
    END.
  END.
END.
ELSE IF hCurrObject:TYPE NE "browse" THEN DO:
  IF STRING(hCurrObject:GET-BUFFER-HANDLE(1):ROWID) NE getAttribute(hCurrObject,"firstrowid") THEN DO:      
    hCurrObject:GET-PREV().

    IF NOT hCurrObject:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
      ASSIGN bSetCurrHandles = FALSE
             hCurrWidget    = hCurrObject
             .
      RUN DoProcessEvent(0,"off-home").        
    END.
    ELSE DO:
      ASSIGN bSetCurrHandles  = FALSE
             hCurrWidget      = hCurrObject
             .
      RUN DoProcessEvent(0,"value-changed").
    END.    
  END.
  IF STRING(hCurrObject:GET-BUFFER-HANDLE(1):ROWID) = getAttribute(hCurrObject,"firstrowid") AND VALID-HANDLE(hToolbar) THEN
    DYNAMIC-FUNCTION("setToolbar",hToolbar,"first").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord Procedure 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     Automatically transfer a fieldMap to print preview
  Parameters:  <none>
  Notes:       Current object is the toolbar
------------------------------------------------------------------------------*/
DEF VAR hPreview        AS HANDLE NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO INIT YES.


FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ttObject.hObject 
       AND ttObjectLink.cLinkType   = "fieldMap" 
     NO-ERROR.
IF AVAIL ttObjectLink THEN DO:
  ASSIGN hCurrObject     = ttObject.hObject
         hCurrSourceProc = ttObject.hSourceProc
         hFieldMap       = ttObjectLink.hToObject.
  
  hPreview = WIDGET-HANDLE(getAttribute(hCurrObject,"printPreviewHandle")).

  IF NOT VALID-HANDLE(hPreview) THEN DO:
    RUN JBoxTextFilePreview.w PERSIST SET hPreview.
    DYNAMIC-FUNCTION("setEnableExcel" IN hPreview,NO).
    RUN InitializeObject IN hPreview.
  END.

  DYNAMIC-FUNCTION("setWindowTitle" IN hPreview,"Preview " + hFieldMap:NAME).
  
  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraPrintRecord") THEN
    RUN ExtraPrintRecord IN hCurrSourceProc (hPreview,OUTPUT bOk).

  IF bOk THEN
    DYNAMIC-FUNCTION("LoadPreviewFromTT" IN hPreview,
                     DYNAMIC-FUNCTION("FieldMapFieldsToTT",
                                      hFieldMap,
                                      getAttribute(hFieldMap,"ExcludeFromPrint"))
                    ,""
                    ,YES).
  
  setAttribute(hCurrObject,"printPreviewHandle",STRING(hPreview)).

  RUN MoveToTop IN hPreview.
  setCurrentObject(hCurrObject).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshBrowseRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowseRecord Procedure 
PROCEDURE RefreshBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:    Refresh data for browse (F5) 
  Parameters:  <none>
  Notes:      Current object is the browse widget 
------------------------------------------------------------------------------*/
DEF VAR cCurrRowid1 AS CHAR   NO-UNDO.
DEF VAR iReposRow   AS INT    NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.
DEF VAR rReposRow   AS ROWID  NO-UNDO.

hCurrObject = ttObject.hObject.
IF getAttribute(hCurrObject,"useLocalData") NE "yes" AND hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  ASSIGN cCurrRowid1 = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
         iReposRow   = hCurrObject:FOCUSED-ROW.
ELSE IF hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  ASSIGN rReposRow = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID
         iReposRow = hCurrObject:FOCUSED-ROW.


RUN InvokeMethod(hCurrObject,"OpenQuery").

IF cCurrRowId1 NE "" THEN DO:
  bOk = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE RowIdent1 = '" + cCurrRowId1 + "'") NO-ERROR.
  IF bOk THEN DO:
    hCurrObject:SET-REPOSITIONED-ROW(iReposRow,"conditional").
    hCurrObject:QUERY:REPOSITION-TO-ROWID(hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
  END.
END.
ELSE IF rReposRow NE ? THEN DO:
  hCurrObject:SET-REPOSITIONED-ROW(iReposRow,"conditional").
  hCurrObject:QUERY:REPOSITION-TO-ROWID(rReposRow) NO-ERROR.
END.

APPLY "value-changed" TO hCurrObject.

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord Procedure 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:    Refresh data  
  Parameters:  <none>
  Notes:      Current object is the toolbar widget 
------------------------------------------------------------------------------*/
DEF VAR cCurrRowid1 AS CHAR   NO-UNDO.
DEF VAR iReposRow   AS INT    NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.
DEF VAR hBrwOrQuery AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.

FIND FIRST ttObjectLink NO-LOCK
     WHERE ttObjectLink.hFromObject = hCurrObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN DO:
  FIND FIRST ttObjectLink NO-LOCK
       WHERE ttObjectLink.hFromObject = hCurrObject
         AND ttObjectLink.cLinkType   = "query"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    hBrwOrQuery = ttObjectLink.hToObject.
    IF ttObjectLink.hToObject:GET-BUFFER-HANDLE(1):AVAIL THEN
      ASSIGN cCurrRowid1 = hBrwOrQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
             iReposRow   = -999.
  END.
  ELSE RETURN.
END.
ELSE DO:
  hBrwOrQuery = ttObjectLink.hToObject.
  IF ttObjectLink.hToObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    ASSIGN cCurrRowid1 = hBrwOrQuery:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
           iReposRow   = hBrwOrQuery:FOCUSED-ROW.
END.

RUN InvokeMethod(hBrwOrQuery,"OpenQuery").
/* setCurrentObject(hBrwOrQuery). */
/* RUN OpenQuery.                 */

IF cCurrRowId1 NE "" THEN DO:
  IF iReposRow NE -999 THEN DO:   /* Browse */
    bOk = hBrwOrQuery:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE RowIdent1 = '" + cCurrRowId1 + "'") NO-ERROR.
    IF bOk THEN DO:
      hBrwOrQuery:SET-REPOSITIONED-ROW(iReposRow,"conditional").
      hBrwOrQuery:QUERY:REPOSITION-TO-ROWID(hBrwOrQuery:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
    END.
  END.
  ELSE DO:
    bOk = hBrwOrQuery:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE RowIdent1 = '" + cCurrRowId1 + "'") NO-ERROR.
    IF bOk THEN 
      hBrwOrQuery:REPOSITION-TO-ROWID(hBrwOrQuery:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
  END.

  ASSIGN bSetCurrHandles = FALSE
         hCurrWidget     = hBrwOrQuery
         .
  RUN DoProcessEvent(0,"value-changed"). /* Note: With JukeBok the query has a 'value-changed' event (not a Progress event) */

END.
setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResizeBrowseColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResizeBrowseColumns Procedure 
PROCEDURE ResizeBrowseColumns :
/*------------------------------------------------------------------------------
  Purpose:     Do a resize of the first column of a browse containing an updateable column
               to ensure that the overlay fill-in(s) are properly displayed after window resize or move of splitbar.
  Parameters:  <none>
  Notes:       PUBLISH from ResizeLib: setWidgetResize, setSplitBarX, setSplitBarY
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.

DEF VAR hColumn AS HANDLE NO-UNDO.

FOR EACH bttObject 
    WHERE bttObject.cObjectType = "browse":
  IF CAN-FIND(FIRST bttObjectLink
              WHERE bttObjectLink.hFromObject = bttObject.hObject
                AND bttObjectLink.cLinkType   = "browseoverlay") THEN DO:
    hColumn = bttObject.hObject:GET-BROWSE-COLUMN(1) NO-ERROR.
    IF VALID-HANDLE(hColumn) THEN
      APPLY "end-resize" TO hColumn.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReturnOfWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturnOfWidget Procedure 
PROCEDURE ReturnOfWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hNextTabItem  AS HANDLE NO-UNDO.
DEF VAR hToolbar      AS HANDLE NO-UNDO.
DEF VAR hCurrObject   AS HANDLE NO-UNDO.
DEF VAR hSaveBtn      AS HANDLE NO-UNDO.
DEF VAR bSave         AS LOG    NO-UNDO.
DEF VAR cScreenUpdW   AS CHAR   NO-UNDO.

hCurrObject = ttObject.hObject.

IF DYNAMIC-FUNCTION("getAttribute",SESSION,"saveOnReturnOfLastField") = "yes" THEN DO:
  cScreenUpdW = getAttribute(hCurrObject,"ScreenUpdateWidgets").
  IF (VALID-HANDLE(hCurrWidget:NEXT-TAB-ITEM) AND 
      (NOT CAN-DO(cScreenUpdW,STRING(hCurrWidget:NEXT-TAB-ITEM)) OR STRING(hCurrWidget:NEXT-TAB-ITEM) = ENTRY(1,cScreenUpdW)))
     OR NOT VALID-HANDLE(hCurrWidget:NEXT-TAB-ITEM) THEN DO:
    hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hCurrObject,"toolbar","from"). 
    hSaveBtn = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"save","").
    IF VALID-HANDLE(hSaveBtn) AND hSaveBtn:SENSITIVE THEN DO:
      RUN InvokeMethod(hToolbar,"SaveRecord").
      bSave = YES.
    END.
  END.
END.

IF NOT bSave THEN DO:
  IF hCurrWidget:TYPE NE "fill-in" THEN DO:
    APPLY "tab" TO hCurrWidget.
    bMyReturnNoApply = YES.    
  END.
  ELSE DO:
    IF bTabOnReturn AND VALID-HANDLE(hCurrWidget) AND VALID-HANDLE(hCurrWidget:NEXT-TAB-ITEM) THEN DO:
      hNextTabItem = hCurrWidget:NEXT-TAB-ITEM.
      APPLY "entry" TO hNextTabItem.
      bMyReturnNoApply = YES.
    END.
  END.
END.

setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RollbackRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RollbackRecord Procedure 
PROCEDURE RollbackRecord :
/*------------------------------------------------------------------------------
  Purpose:    Delete all temporary created records in a commit type transaction 
  Parameters:  <none>
  Notes:      Current object is the toolbar 
              NOT IN USE: brynjar: 22.05.08 
-----------------------------------------------------------------------------
DEF VAR hCurrObject AS HANDLE.

hCurrObject = ttObject.hObject.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = hCurrObject
       AND ttObjectLink.cLinkType = "browse"
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN DO:
  DoMessage(0,0,"Missing object link from toolbar to browse" + CHR(10)
              + "Caller: " + PROGRAM-NAME(2) + CHR(10)
              + "Procedure: " + PROGRAM-NAME(1),"JukeBox Programmers error","").
  RETURN.
END.

ttObjectLink.hToObject:QUERY:GET-BUFFER-HANDLE(1):EMPTY-TEMP-TABLE().

RUN UndoRecord.

setCurrentObject(hCurrObject).
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RowDisplayBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse Procedure 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the browse
------------------------------------------------------------------------------*/
DEF VAR hRowCount AS HANDLE NO-UNDO.
IF AVAIL ttObject AND getAttribute(ttObject.hObject,"shadedRows") = "yes" 
   AND iColorRowShade NE 0  
   THEN DO:
  hRowCount = WIDGET-HANDLE(getAttribute(ttObject.hObject,"RowCountFieldHandle")) NO-ERROR.
  IF VALID-HANDLE(hRowCount) THEN
    FOR EACH ttEvent OF ttObject 
        WHERE ttEvent.cWidgetType = "browse-column":
      IF hRowCount:BUFFER-VALUE MOD 2 = 0 THEN
        ttEvent.hWidget:BGCOLOR = iColorRowShade.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RowEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowEntry Procedure 
PROCEDURE RowEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is always a browser
------------------------------------------------------------------------------*/
DEF VAR cScreenValue        AS CHAR   NO-UNDO.
DEF VAR hCurrObject         AS HANDLE NO-UNDO.
DEF VAR iPrevRow            AS INT    NO-UNDO.
DEF VAR iCurrRow            AS INT    NO-UNDO.
DEF VAR hFillIn             AS HANDLE NO-UNDO.
DEF VAR bOverlays           AS LOG    NO-UNDO.
DEF VAR hToggle             AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iNumToggles         AS INT    NO-UNDO.
DEF VAR cCurrBrowseToggles  AS CHAR   NO-UNDO.
DEF VAR hEnterHere          AS HANDLE NO-UNDO.
DEF VAR hRecordSelectWidget AS HANDLE NO-UNDO.

IF ttObject.hObject:TYPE NE "browse" THEN RETURN.

ASSIGN hCurrObject         = ttObject.hObject
       iPrevRow            = INT(getAttribute(ttObject.hObject,"currentrow"))
       bFreezeEndResizeBrw = YES
       .

IF getAttribute(hCurrObject,"enableOnToolbarClick") = "yes" AND getAttribute(ttObject.hObject,"enableupdate") = "yes" THEN DO:
  iCancelNextEvents = 2.  
  hCurrObject:SELECT-ROW(iPrevRow).
  hEnterHere = WIDGET-HANDLE(getAttribute(hCurrObject,"currentOverlayWidget")) NO-ERROR.
  IF NOT VALID-HANDLE(hEnterHere) THEN
    hEnterHere = WIDGET-HANDLE(getAttribute(hCurrObject,"firstEnabledOverlay")) NO-ERROR.  
  IF VALID-HANDLE(hEnterHere) THEN
    setWidgetEnter(hEnterHere).
  RETURN.
END.

IF hCurrObject:MULTIPLE THEN DO:
  hRecordSelectWidget = WIDGET-HANDLE(getAttribute(hCurrObject:WINDOW,"RecordSelectWidget")) NO-ERROR.

  IF VALID-HANDLE(hRecordSelectWidget) THEN 
    hRecordSelectWidget:SCREEN-VALUE = STRING(hCurrObject:NUM-SELECTED-ROWS,"zzzzz9").
END.

IF hCurrObject:NUM-ITERATIONS = 0 THEN RETURN.

IF getAttribute(ttObject.hObject,"enableondblclick") = "yes" THEN DO:
  IF getAttribute(ttObject.hObject,"doubleclickenabledfield") = "" THEN
    setAttribute(ttObject.hObject,"enableupdate","no").
  ELSE DO:
    hEnterHere = WIDGET-HANDLE(getAttribute(ttObject.hObject,"doubleclickenabledfield")) NO-ERROR.
    setAttribute(ttObject.hObject,"doubleclickenabledfield","").
  END.
END.

FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ttObject.hObject
      AND ttObjectLink.cLinktype   = "browseoverlay"
   ,FIRST bttObject 
          WHERE bttObject.hObject = ttObjectLink.hToObject
            AND CAN-DO("fill-in,toggle-box",bttObject.cObjectType):

  IF bttObject.cObjectType = "toggle-box" THEN DO:
    iNumToggles = iNumToggles + 1.
    hToggle[iNumToggles] = bttObject.hObject.
  END.
  bOverlays = TRUE.

  IF ttObjectLink.hToObject:MODIFIED AND iPrevRow NE 0 AND bttObject.cObjectType = "fill-in" THEN DO:

    DoLockWindow(hCurrObject:WINDOW).

    iCurrRow = hCurrObject:FOCUSED-ROW.
    hFillIn  = ttObjectLink.hToObject.
    cScreenValue = ttObjectLink.hToObject:SCREEN-VALUE.
    hCurrObject:SELECT-ROW(iPrevRow).
    hFillIn:SCREEN-VALUE = cScreenValue.
    APPLY "return" TO hFillIn.
    IF getAttribute(hCurrObject,"updateErrorRow") NE "" THEN DO: 
      DoLockWindow(?).
      hEnterHere = WIDGET-HANDLE(getAttribute(hCurrObject,"updateErrorWidget")) NO-ERROR.
      IF VALID-HANDLE(hEnterHere) THEN
        setWidgetEnter(hEnterHere).
      RETURN.
    END.
    IF hCurrObject:MULTIPLE THEN 
      hCurrObject:DESELECT-ROWS().
    hCurrObject:SELECT-ROW(iCurrRow).
    IF hCurrObject:MULTIPLE THEN 
      hCurrObject:SELECT-FOCUSED-ROW().
    DoLockWindow(?).
    
  END.
END.

/* If no overlays we don't want an extra value-change for every mouse-click and potential extra round-trips to fetch child records: */
IF bOverlays OR hCurrObject:MULTIPLE THEN DO:
/*  APPLY "value-changed" TO hCurrObject.*/
  setAttribute(hCurrObject,"newrow","").
  IF hCurrObject:MULTIPLE AND hCurrObject:NUM-SELECTED-ROWS = 0
     AND hCurrObject:QUERY:NUM-RESULTS > 0 
     AND getAttribute(hCurrObject,"select1strow") = "yes" THEN DO:
    hCurrObject:SELECT-ROW(1).
    DisplayRow(hCurrObject).
  END.
  IF CAN-FIND(FIRST ttObjectLink
              WHERE ttObjectLink.hFromObject = hCurrObject
                AND ttObjectLink.cLinkType   = "browseoverlay") THEN
    hCurrObject:REFRESH() NO-ERROR.

  DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hCurrObject).  
END.  

/* In the value-change we might have re-created the toggle overlays: */
IF getAttribute(hCurrObject,"currbrowsetoggles") NE "" THEN DO:
  cCurrBrowseToggles = getAttribute(hCurrObject,"currbrowsetoggles").
  DO ix = 1 TO iNumToggles:
    hToggle[ix] = WIDGET-HANDLE(ENTRY(ix,cCurrBrowseToggles)) NO-ERROR.
  END.
END.

setAttribute(hCurrObject,"currentrow",STRING(hCurrObject:FOCUSED-ROW)).

DO ix = 1 TO iNumToggles:
  IF VALID-HANDLE(hToggle[ix]) 
     AND LAST-EVENT:X GE hToggle[ix]:X 
     AND LAST-EVENT:X LE hToggle[ix]:X + hToggle[ix]:WIDTH-PIXELS   
     THEN DO:
    hToggle[ix]:CHECKED = NOT hToggle[ix]:CHECKED.
    APPLY "value-changed" TO hToggle[ix].
    LEAVE.
  END.
END.

IF VALID-HANDLE(hEnterHere) THEN APPLY "entry" TO hEnterHere.

bFreezeEndResizeBrw = NO.

/* To be able to set the widgetEnter / MyReturnNoApply (which was reset by value-changed over) */
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraRowEntry") THEN 
  RUN ExtraRowEntry IN hCurrSourceProc (hCurrObject).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RowsToBatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowsToBatch Procedure
PROCEDURE RowsToBatchRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cBatchSize  AS CHAR NO-UNDO.
DEF VAR hThisObject AS HANDLE NO-UNDO.
DEF VAR iReturn     AS INT NO-UNDO.

IF NOT CAN-DO("browse,query",ttObject.cObjectType) THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "browse" NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ttObject.hObject
           AND ttObjectLink.cLinkType   = "query" NO-ERROR.
  IF AVAIL ttObjectLink THEN
    hThisObject = ttObjectLink.hToObject.
END.
ELSE hThisObject = ttObject.hObject.
IF VALID-HANDLE(hThisObject) THEN DO:
  cBatchSize = getAttribute(hThisObject,"rowsToBatch").
  RUN JBoxAskForValue.w (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sett rader i resultatsett" ELSE "Set rows to batch",
                        "INTEGER|>>>>9|<opt:init.val>",INPUT-OUTPUT cBatchSize,OUTPUT iReturn).
  IF iReturn = 2 THEN 
    setAttribute(hThisObject,"rowsToBatch",cBatchSize).
END.  
   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-SaveFilterRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveFilterRecord Procedure 
PROCEDURE SaveFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:    Save filter settings to users ini-file for this window 
  Parameters:  <none>
  Notes:      Current object must be (set to) the filter object 
------------------------------------------------------------------------------*/
DEF VAR hWidget         AS HANDLE NO-UNDO.
DEF VAR hOperatorWidget AS HANDLE NO-UNDO.
DEF VAR cFilterString   AS CHAR NO-UNDO.
DEF VAR cOperator       AS CHAR NO-UNDO.

IF ttObject.cObjectType = "toolbar" OR ttObject.cObjectType = "popupMenu" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "filter" NO-ERROR.
  IF AVAIL ttObjectLink THEN
    FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN.
END.
IF NOT ttObject.cObjectType = "filter" THEN
  RETURN.
                                             
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse" NO-ERROR.
IF NOT AVAIL ttObjectLink THEN
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "query" NO-ERROR.

IF AVAIL ttObjectLink THEN DO:
  FOR EACH bttAttribute NO-LOCK OF ttObject
      WHERE bttAttribute.cName BEGINS "filterfield_":
  
    hWidget = WIDGET-HANDLE(getAttribute(ttObject.hObject,bttAttribute.cName)).


    IF hWidget:SCREEN-VALUE NE ? 
       AND hWidget:SCREEN-VALUE NE "" 
       AND IF hWidget:DATA-TYPE = "INTEGER" THEN INT(hWidget:SCREEN-VALUE) NE 0 ELSE TRUE
       AND IF hWidget:DATA-TYPE = "DECIMAL" THEN DEC(hWidget:SCREEN-VALUE) NE 0 ELSE TRUE
       THEN DO:

      ASSIGN cOperator       = ""
             hOperatorWidget = WIDGET-HANDLE(getAttribute(ttObject.hObject,"operatorField_" + hWidget:NAME)).
      IF VALID-HANDLE(hOperatorWidget) THEN
        cOperator = hOperatorWidget:SCREEN-VALUE.
      ELSE IF hWidget:DATA-TYPE = "CHARACTER" THEN
        cOperator = (IF getAttribute(ttObject.hObject,"operator_" + hWidget:NAME) NE "" THEN
                       getAttribute(ttObject.hObject,"operator_" + hWidget:NAME)
                     ELSE IF INDEX(hWidget:SCREEN-VALUE,"*") > 0 THEN
                       "MATCHES"
                     ELSE "BEGINS").
      ELSE                     
        cOperator = (IF getAttribute(ttObject.hObject,"operator_" + hWidget:NAME) NE "" THEN
                       getAttribute(ttObject.hObject,"operator_" + hWidget:NAME)
                     ELSE "=").

      IF cOperator NE "" THEN 
        cFilterString = cFilterString +
                        hWidget:NAME + "|" +
                        getAttribute(ttObject.hObject,"bufferfield_" + hWidget:NAME) + "|" +
                        cOperator + "|" + 
                        (IF hWidget:DATA-TYPE = "LOGICAL" THEN
                          hWidget:INPUT-VALUE
                         ELSE hWidget:SCREEN-VALUE) + "|".

    END.      

  END.
  
  IF cFilterString = "" THEN cFilterString = "|||".
  cFilterString = "<StartFilter>|" + cFilterString + "<EndFilter>".

  FIND bttObject WHERE bttObject.hObject = ttObjectLink.hToObject.
  DYNAMIC-FUNCTION("setCustomWinSettings",bttObject.hWindow,cFilterString).
END.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord Procedure 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object is the toolbar. Must navigate to the fieldmap object 
              (after getting the toolbar state)
------------------------------------------------------------------------------*/
DEF VAR cScreenUpdateWidgets AS CHAR   NO-UNDO.
DEF VAR cBufferUpdateFields  AS CHAR   NO-UNDO.
DEF VAR cInputValues         AS CHAR   NO-UNDO.
DEF VAR cInputFields         AS CHAR   NO-UNDO.
DEF VAR hWidget              AS HANDLE NO-UNDO.
DEF VAR cReturn              AS CHAR   NO-UNDO.
DEF VAR rRepos               AS ROWID  NO-UNDO.
DEF VAR cTBstate             AS CHAR   NO-UNDO.
DEF VAR cBuffersAndFields    AS CHAR   NO-UNDO.
DEF VAR cBufferList          AS CHAR   NO-UNDO.
DEF VAR hCurrFieldMap        AS HANDLE NO-UNDO.
DEF VAR hCurrObject          AS HANDLE NO-UNDO.
DEF VAR hRefreshObject       AS HANDLE NO-UNDO.
DEF VAR hMySourceProc        AS HANDLE NO-UNDO.
DEF VAR iReposRow            AS INT    NO-UNDO.
DEF VAR cBufferFields        AS CHAR   NO-UNDO.
DEF VAR hQueryObject         AS HANDLE NO-UNDO.
DEF VAR cPrimaryRowid        AS CHAR   NO-UNDO.
DEF VAR cParentLinkFields    AS CHAR   NO-UNDO.
DEF VAR cParentLinkValues    AS CHAR   NO-UNDO.
DEF VAR cModParentLinkFields AS CHAR   NO-UNDO.
DEF VAR cModParentLinkValues AS CHAR   NO-UNDO.
DEF VAR hFirstParent         AS HANDLE NO-UNDO.

IF NOT DYNAMIC-FUNCTION("getIsEventAllowed",ttObject.hObject,"save") THEN RETURN.

ASSIGN cTBstate      = ttObject.cState
       hCurrObject   = ttObject.hObject
       hMySourceProc = hCurrSourceProc.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject 
       AND ttObjectLink.cLinkType   = "query" 
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "browse" 
       NO-ERROR.
IF AVAIL ttObjectLink THEN
  hQueryObject = ttObjectLink.hToObject.

IF ttObject.cObjectType NE "fieldmap" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "fieldmap" 
       NO-ERROR.
  IF AVAIL ttObjectLink THEN
    FIND FIRST ttObject 
         WHERE ttObject.hObject = ttObjectLink.hToObject
         NO-ERROR.
  IF NOT AVAIL ttObject THEN DO:
    MESSAGE "Missing link from " ttObject.hObject:NAME " to fieldmap" SKIP
            "Programmers mistake"
            VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  ELSE hCurrFieldMap = ttObject.hObject.
END.
ELSE hCurrFieldMap = ttObject.hObject.

IF NOT VALID-HANDLE(hCurrSourceProc) THEN hCurrSourceProc = SOURCE-PROCEDURE.


FOR EACH ttObjectLink 
    WHERE ttObjectLink.hFromObject = hCurrFieldMap
      AND ttObjectLink.cLinkType = "dotNetDisplay"
    :
  /* Target: JBoxWrapWindowInForm */
/*  PUBLISH "dotNetPreSave" (hCurrFieldMap,ttObjectLink.hToObject,ttObjectLink.cLinkInfo). */
  PUBLISH "dotNetMethod" ("dotNetPreSave",hCurrFieldMap,ttObjectLink.hToObject,ttObjectLink.cLinkInfo,?).  
END.

ASSIGN cScreenUpdateWidgets = getAttribute(hCurrFieldMap,"ScreenUpdateWidgets")
       cBufferFields        = getAttribute(hCurrFieldMap,"BufferUpdateFields").

DO ix = 1 TO NUM-ENTRIES(cScreenUpdateWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cScreenUpdateWidgets)).

  IF (getAttribute(hCurrFieldMap,"saveonlymodified") = "yes" OR
      getAttribute(hQueryObject,"nodbreadaccesfields") NE "" OR
      getAttribute(hQueryObject,"nodbreadwritefields") NE "")
      AND NOT hWidget:MODIFIED THEN NEXT.

  IF hWidget:DATA-TYPE = "DATE" AND LENGTH(hWidget:SCREEN-VALUE) = 6 THEN
    hWidget:SCREEN-VALUE = hWidget:SCREEN-VALUE + STRING(YEAR(TODAY)).
/*   ELSE IF hWidget:DATA-TYPE = "CHARACTER" AND INDEX(hWidget:SCREEN-VALUE,"|") > 0 THEN DO:                                                       */
/*     DoMessage (0,0,                                                                                                                              */
/*                (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Ugyldig inputverdi: | i felt " ELSE "Invalid input value | in field ") + hWidget:LABEL */
/*               ,"","").                                                                                                                           */
/*     RETURN "error".                                                                                                                              */
/*   END.                                                                                                                                           */
  ASSIGN cInputFields = cInputFields + ENTRY(ix,cBufferFields) + ","
         cInputValues = cInputValues + (IF hWidget:INPUT-VALUE NE ? THEN 
                                          (IF hWidget:DATA-TYPE = "LOGICAL" THEN
                                            STRING(hWidget:INPUT-VALUE)
                                           ELSE IF hWidget:DATA-TYPE NE "DATE" THEN
                                             hWidget:INPUT-VALUE
                                           ELSE hWidget:SCREEN-VALUE)
                                        ELSE "?") + CHR(1).
END.
ASSIGN cInputValues = cInputValues + (IF getAttribute(hCurrFieldMap,"BufferExtraValues") NE ? THEN REPLACE(getAttribute(hCurrFieldMap,"BufferExtraValues"),"|",CHR(1)) ELSE "")
       cInputFields = TRIM(cInputFields + (IF getAttribute(hCurrFieldMap,"BufferExtraValues") NE ? THEN getAttribute(hCurrFieldMap,"BufferExtraFields") ELSE ""),",")
       cInputValues = IF SUBSTR(cInputValues,LENGTH(cInputValues)) = CHR(1) AND NUM-ENTRIES(cInputValues,CHR(1)) NE NUM-ENTRIES(cInputFields) THEN
                         SUBSTR(cInputValues,1,LENGTH(cInputValues) - 1) 
                      ELSE cInputValues
       .
       
IF (getAttribute(hCurrFieldMap,"uselocaldata") NE "yes" AND getAttribute(hQueryObject,"uselocaldata") NE "yes") OR getAttribute(hCurrFieldMap,"fieldMapIsBrowse") = "yes" THEN DO:
  DYNAMIC-FUNCTION("setPostUpdProc",getAttribute(hCurrFieldMap,"postUpdateProc")).
  IF DYNAMIC-FUNCTION("getIsAttributeSet",hCurrFieldMap,"serverTransInputParam") THEN
    DYNAMIC-FUNCTION("setServerTransInputParam",DYNAMIC-FUNCTION("getAttribute",hCurrFieldMap,"serverTransInputParam")).
    
  IF cTBstate NE "new" AND hCurrFieldMap:AVAIL THEN DO:
    IF getAttribute(hQueryObject,"orgbuffersandfields") NE "" THEN DO:
      ASSIGN cBuffersAndFields = getAttribute(hQueryObject,"buffersandfields")
             cBufferList       = getAttribute(hQueryObject,"bufferlist").
      DO ix = 1 TO NUM-ENTRIES(cBuffersAndFields):
        IF LOOKUP(ENTRY(1,ENTRY(ix,cBuffersAndFields),";"),cBufferList) = 1 THEN 
          cPrimaryRowid = hCurrFieldMap:BUFFER-FIELD("RowIdent" + STRING(ix)):BUFFER-VALUE.
      END.
      IF cPrimaryRowid NE "" THEN
        DYNAMIC-FUNCTION("setCurrentRowid",cPrimaryRowid).
      ELSE DO:          
        MESSAGE PROGRAM-NAME(1) SKIP
                "Cannot save record. The buffersequence is not equal original setting"
                VIEW-AS ALERT-BOX.
        RETURN ERROR "error".
      END.
    END.
    IF getAttribute(hCurrFieldMap,"fieldMapIsBrowse") NE "yes" THEN
      cReturn = DYNAMIC-FUNCTION("getCurrentChanged",hCurrFieldMap,cInputFields).
    IF cReturn NE "" THEN DO:
      iReturn = DoMessage(0,1,cReturn,
                          (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Skrive over data?" ELSE "Override data?"),"").
      IF iReturn = 2 THEN RETURN.
    END.

    bOk = DYNAMIC-FUNCTION("DoUpdate",getAttribute(hCurrFieldMap,"dbname") + hCurrFieldMap:NAME,
                  getAttribute(hCurrFieldMap,"customUpdateValProc"),
                  "",
                  IF cPrimaryRowid NE "" THEN cPrimaryRowid ELSE hCurrFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                  cInputFields,
                  cInputValues,
                  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraSaveRecord") THEN FALSE ELSE TRUE).
  END.
  ELSE IF cTBstate = "new" THEN DO:
    ASSIGN cParentLinkValues = DYNAMIC-FUNCTION("getParentLinkValues",hQueryObject)
           cParentLinkFields = DYNAMIC-FUNCTION("getParentLinkFields",hQueryObject).
    IF cParentLinkValues NE "" AND cParentLinkFields NE "parentLink" THEN DO ix = 1 TO NUM-ENTRIES(cParentLinkFields):
      IF NOT CAN-DO(cInputFields,ENTRY(ix,cParentLinkFields)) THEN
        ASSIGN cModParentLinkFields = cModParentLinkFields + "," + ENTRY(ix,cParentLinkFields)
               cModParentLinkValues = cModParentLinkValues + CHR(1) + ENTRY(ix,cParentLinkValues,CHR(1)).
    END.
    IF cModParentLinkValues NE "" THEN 
      ASSIGN cInputValues = cInputValues + cModParentLinkValues
             cInputFields = TRIM(cInputFields + cModParentLinkFields,",").
    
    bOk = DYNAMIC-FUNCTION("DoCreate",getAttribute(hCurrFieldMap,"dbname") + hCurrFieldMap:NAME,
                  IF getAttribute(hCurrFieldMap,"customUpdateValProc") NE "" THEN
                    LEFT-TRIM(RIGHT-TRIM(getAttribute(hCurrFieldMap,"customUpdateValProc") + ",=" +
                                         getAttribute(hCurrFieldMap,"customCreateProc"),",="),",")
                  ELSE IF getAttribute(hCurrFieldMap,"customCreateProc") NE "" THEN
                    ",=" + getAttribute(hCurrFieldMap,"customCreateProc")
                  ELSE "",
                  cInputFields,
                  cInputValues,
                  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraSaveRecord") THEN FALSE ELSE TRUE).
  END.
  ELSE RETURN ERROR "error".
  
  setAttribute(hCurrFieldMap,"serverTransReturnParam",DYNAMIC-FUNCTION("getServerTransReturnParam")).
  setAttribute(hCurrFieldMap,"serverTransReturnMessage",DYNAMIC-FUNCTION("getTransactionMessage")).
END.
ELSE bOK = TRUE.

/* When using this feature remember to commit the transaction yourself!! */
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraSaveRecord") THEN 
  RUN ExtraSaveRecord IN hCurrSourceProc (cTBstate,OUTPUT bOk).

IF NOT bOk THEN DO:
  IF DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*(7368)*" THEN
    DYNAMIC-FUNCTION("DoMessage",-1,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Database Security","").
  ELSE IF DYNAMIC-FUNCTION("getTransactionMessage") NE "" AND 
          NOT CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"SaveErrorHandler") 
          AND DYNAMIC-FUNCTION("getServerTransReturnParam") NE "confirm" THEN
    DoMessage (0,0,DYNAMIC-FUNCTION("getTransactionMessage"),
               IF DYNAMIC-FUNCTION("Scandinavian") THEN "Feil i registrering" ELSE "Error saving data","").
  ELSE IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"SaveErrorHandler") THEN
    RUN SaveErrorHandler IN hCurrSourceProc (cTBstate,OUTPUT bOk).

  IF NOT bOk THEN DO:
    DYNAMIC-FUNCTION("resetTransRecords").
  
    RETURN ERROR "error".
  END.
END.

IF cTBstate = "new" THEN DO:
  IF getAttribute(hCurrFieldMap,"fieldMapIsBrowse") NE "yes" THEN  
    hCurrFieldMap:BUFFER-CREATE.
  ELSE setAttribute(hQueryObject,"useLocalData","no").
    
  rRepos = hCurrFieldMap:ROWID.
END.

IF getAttribute(hCurrFieldMap,"uselocaldata") NE "yes" AND getAttribute(hQueryObject,"uselocaldata") NE "yes" THEN 
  DYNAMIC-FUNCTION("DoRefetchTrans",hCurrFieldMap,"FIRST","").
ELSE DO:
  cBufferUpdateFields = getAttribute(hCurrFieldMap,"BufferUpdateFields").

  DO ix = 1 TO NUM-ENTRIES(cBufferUpdateFields):
    IF VALID-HANDLE(hCurrFieldMap:BUFFER-FIELD(ENTRY(ix,cBufferUpdateFields))) THEN
      DYNAMIC-FUNCTION("assignStringValue",hCurrFieldMap:BUFFER-FIELD(ENTRY(ix,cBufferUpdateFields)),ENTRY(ix,cInputValues,CHR(1))).
  END.
END.

FOR EACH ttObjectLink 
    WHERE ttObjectLink.hFromObject = hCurrFieldMap
      AND ttObjectLink.cLinkType = "dotNetDisplay"
    :
  /* Target: JBoxWrapWindowInForm */
/*  PUBLISH "dotNetSave" (hCurrFieldMap,ttObjectLink.hToObject,ttObjectLink.cLinkInfo). */
  PUBLISH "dotNetMethod" ("dotNetSave",hCurrFieldMap,ttObjectLink.hToObject,ttObjectLink.cLinkInfo,?).
END.

/* Navigate to browse or query object for refresh and reposition: */
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = hCurrFieldMap
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN 
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = hCurrFieldMap
         AND ttObjectLink.cLinkType   = "query"
       NO-ERROR.

IF AVAIL ttObjectLink THEN DO:
  hRefreshObject = ttObjectLink.hToObject.

  IF NOT getAttribute(hCurrFieldMap,"uselocaldata") = "yes" AND NOT getAttribute(hQueryObject,"uselocaldata") = "yes" THEN DO:
    DYNAMIC-FUNCTION("setCalcFieldProc",getAttribute(hRefreshObject,"calcfieldproc")).
    IF cPrimaryRowid NE "" THEN DO:
      DYNAMIC-FUNCTION("setCurrentRowid",cPrimaryRowid).
      DYNAMIC-FUNCTION("refreshRow",hRefreshObject,
                        getAttribute(hRefreshObject,"orgbuffersandfields"),
                        getAttribute(hRefreshObject,"orgqueryjoin")).
    END.
    ELSE
      DYNAMIC-FUNCTION("refreshRow",hRefreshObject,
                        getAttribute(hRefreshObject,"buffersandfields"),
                        getAttribute(hRefreshObject,"queryjoin")).
  END.

  IF cTBstate = "new" THEN DO:
    IF ttObjectLink.cLinkType = "browse" THEN DO:
      IF hRefreshObject:QUERY:PREPARE-STRING = ? THEN
        hRefreshObject:QUERY:QUERY-PREPARE("FOR EACH " + hRefreshObject:QUERY:GET-BUFFER-HANDLE(1):NAME).
      hQueryObject = hRefreshObject:QUERY.
      hQueryObject:QUERY-OPEN().
      hRefreshObject:SET-REPOSITIONED-ROW(hRefreshObject:DOWN,"conditional") NO-ERROR.
      hRefreshObject:QUERY:REPOSITION-TO-ROWID(rRepos).
    END.
    ELSE DO:
      IF NOT hRefreshObject:IS-OPEN THEN DO:
        IF hRefreshObject:PREPARE-STRING = ? THEN
          hRefreshObject:QUERY-PREPARE("FOR EACH " + hRefreshObject:GET-BUFFER-HANDLE(1):NAME).
        hRefreshObject:QUERY-OPEN().
      END.
      hRefreshObject:QUERY-OPEN(). 
      hRefreshObject:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
      IF ERROR-STATUS:GET-NUMBER(1) = 7331 THEN DO:
        hRefreshObject:QUERY-PREPARE("FOR EACH " + hRefreshObject:GET-BUFFER-HANDLE(1):NAME
                                   + " WHERE ROWID(" + hRefreshObject:GET-BUFFER-HANDLE(1):NAME + ") = TO-ROWID('" + STRING(rRepos) + "')").
        hRefreshObject:QUERY-OPEN().
      END.
      hRefreshObject:GET-NEXT().
      hRefreshObject:GET-PREV().
      IF NOT hCurrFieldMap:AVAIL THEN
        setAttribute(hRefreshObject,"firstrowid",STRING(rRepos)).
      hRefreshObject:GET-NEXT().
      hRefreshObject:GET-NEXT().
      IF NOT hCurrFieldMap:AVAIL THEN
        setAttribute(hRefreshObject,"lastrowid",STRING(rRepos)).
      hRefreshObject:GET-PREV().
    END.
    setAttribute(hRefreshObject,"rowsadded",STRING(INT(getAttribute(hRefreshObject,"rowsadded")) + 1)).
    DYNAMIC-FUNCTION("ViewRecordCount",hRefreshObject).
    FOR EACH bttObjectLink
        WHERE bttObjectLink.hFromObject = hRefreshObject
          AND bttObjectLink.cLinkType   = "onetoone":
      IF hFirstParent = ? AND bttObjectLink.hToObject:TYPE = "browse" THEN
        hFirstParent = bttObjectLink.hToObject.
      RefreshParentRecord(hRefreshObject,bttObjectLink.hToObject,"new").
    END.
  END.
  ELSE DO:
    IF ttObjectLink.cLinkType = "browse" THEN DO:
      IF hRefreshObject:MULTIPLE THEN hRefreshObject:SELECT-ROW(hRefreshObject:FOCUSED-ROW).
      DO ix = 1 TO hRefreshObject:NUM-COLUMNS:
        hRefreshObject:GET-BROWSE-COLUMN(ix):SCREEN-VALUE = STRING(hCurrFieldMap:BUFFER-FIELD(hRefreshObject:GET-BROWSE-COLUMN(ix):NAME):BUFFER-VALUE).
      END.
      FIND FIRST bttObject WHERE bttObject.hObject = hRefreshObject NO-ERROR.
      IF AVAIL bttObject AND CAN-DO(bttObject.hSourceProc:INTERNAL-ENTRIES,"RowDisplayBrowse") THEN
        RUN RowDisplayBrowse IN bttObject.hSourceProc.   
    END.
    FOR EACH bttObjectLink
        WHERE bttObjectLink.hFromObject = hRefreshObject
          AND bttObjectLink.cLinkType   = "onetoone":
      IF hFirstParent = ? AND bttObjectLink.hToObject:TYPE = "browse" THEN
        hFirstParent = bttObjectLink.hToObject.
      RefreshParentRecord(hRefreshObject,bttObjectLink.hToObject,"display").
    END.
  END.

  setAttribute(hCurrFieldMap,"checkmodified","no").
  setCurrentObject(hRefreshObject).
  ASSIGN bSetCurrHandles = FALSE
         hCurrWidget     = hRefreshObject
         hCurrSourceProc = hMySourceProc
         .
  IF getAttribute(hCurrObject,"copyRecord") = "yes" THEN
    cTBstate = "copy".


  RUN DoProcessEvent(0,"value-changed"). /* Note: With JukeBok the query has a 'value-changed' event (not a Progress event) */

  hWidget = WIDGET-HANDLE(ENTRY(1,cScreenUpdateWidgets)) NO-ERROR.
  IF VALID-HANDLE(hWidget) AND hWidget:SENSITIVE AND getAttribute(hCurrFieldMap,"fieldMapIsBrowse") NE "yes" THEN APPLY "entry" TO hWidget.
  ELSE IF hRefreshObject:TYPE = "browse" THEN DO:
    IF getAttribute(hCurrFieldMap,"fieldMapIsBrowse") = "yes" THEN DO:
      setAttribute(hRefreshObject,"enableupdate","no").
      setCurrentObject(hRefreshObject).
      RUN EndResizeBrowseColumn.
    END.    
    APPLY "entry" TO hRefreshObject.
  END.  
  ELSE IF VALID-HANDLE(hFirstParent) THEN
    APPLY "entry" TO hFirstParent.

END.
setCurrentObject(hCurrObject).


PUBLISH "EndSaveRecord" (hCurrObject,cTBstate).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ScrollNotifyBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScrollNotifyBrowse Procedure 
PROCEDURE ScrollNotifyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     Resize any overlay fill-ins for resized browse column
  Parameters:  <none>
  Notes:       Current object is the browse, current event is generated by scroll-notify of browse column
------------------------------------------------------------------------------*/
DEF VAR iTmpXpixels     AS INT    NO-UNDO.
DEF VAR iTmpYpixels     AS INT    NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR rSelectionStart AS ROWID  NO-UNDO.
DEF VAR rRepos          AS ROWID  NO-UNDO.
DEF VAR iCurrRow        AS INT    NO-UNDO.
DEF VAR iNumSelected    AS INT    NO-UNDO.
DEF VAR bSelectAll      AS LOG    NO-UNDO.
DEF VAR bAdjRowSelected AS LOG    NO-UNDO.
DEF VAR bCurrRowSet     AS LOG    NO-UNDO.
DEF VAR cLastEvent      AS CHAR   NO-UNDO.

ASSIGN hCurrObject = ttObject.hObject
       cLastEvent  = LAST-EVENT:LABEL.

IF ((getAttribute(SESSION,"windowsbrowse") = "yes" AND getAttribute(ttObject.hObject,"windowsbrowse") NE "no") OR 
     getAttribute(ttObject.hObject,"windowsbrowse") = "yes") AND ttObject.hObject:MULTIPLE THEN 
   WindowsBrowse: DO:
          
  IF cLastEvent MATCHES "*cursor*" OR cLastEvent MATCHES "*page*" AND hCurrObject:FOCUSED-ROW NE ? THEN DO:
    IF hCurrObject:FOCUSED-ROW NE ? AND NOT hCurrObject:IS-ROW-SELECTED(hCurrObject:FOCUSED-ROW) THEN
      hCurrObject:SELECT-FOCUSED-ROW().
    ASSIGN rRepos   = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID
           iCurrRow = hCurrObject:FOCUSED-ROW.
    IF cLastEvent BEGINS "shift" AND hCurrObject:NUM-SELECTED-ROWS > 0 THEN DO:
      iNumSelected = hCurrObject:NUM-SELECTED-ROWS.
      IF hCurrObject:FETCH-SELECTED-ROW(1) THEN 
        rSelectionStart = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.
      hCurrObject:SELECT-ROW(iCurrRow) NO-ERROR.
      hCurrObject:FETCH-SELECTED-ROW(iNumSelected).
    END.
    /* down: */
    IF cLastEvent MATCHES "*down" THEN DO:
      IF cLastEvent BEGINS "shift" THEN DO:
        bAdjRowSelected = hCurrObject:IS-ROW-SELECTED(hCurrObject:FOCUSED-ROW - 1) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN LEAVE WindowsBrowse.
        IF hCurrObject:NUM-SELECTED-ROWS > 1 AND NOT bAdjRowSelected THEN DO:
          hCurrObject:DESELECT-FOCUSED-ROW().
          ASSIGN iCurrRow    = iCurrRow + 1
                 bCurrRowSet = YES.
        END.
        ELSE DO:
          IF hCurrObject:FOCUSED-ROW < hCurrObject:DOWN THEN
            hCurrObject:SELECT-ROW(hCurrObject:FOCUSED-ROW + 1) NO-ERROR.
          ELSE DO:
            IF hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
              rRepos = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.
            hCurrObject:QUERY:GET-NEXT().
            IF NOT hCurrObject:QUERY:QUERY-OFF-END THEN 
              hCurrObject:SCROLL-TO-CURRENT-ROW().
            ELSE DO:
              APPLY "off-end" TO hCurrObject.
              IF rRepos NE ? THEN DO:
                hCurrObject:QUERY:REPOSITION-TO-ROWID(rRepos).
                iCurrRow = hCurrObject:DOWN.
                hCurrObject:SELECT-ROW(iCurrRow) NO-ERROR.            
              END.
              bSelectAll = YES.
            END.
          END.
        END.
        IF NOT cLastEvent BEGINS "shift-page" THEN
          APPLY "value-changed" TO hCurrObject.
      END.
      ELSE DO:
        IF cLastEvent MATCHES "*cursor*" THEN DO:
          IF hCurrObject:QUERY:NUM-RESULTS > 0 THEN DO:
            IF NOT hCurrObject:SELECT-NEXT-ROW() THEN DO:
              APPLY "off-end" TO hCurrObject.
              IF hCurrObject:FOCUSED-ROW NE ? THEN 
                hCurrObject:SELECT-ROW(hCurrObject:FOCUSED-ROW).
            END.
            APPLY "value-changed" TO hCurrObject.
          END.
        END.
        ELSE DO:
          DoLockWindow(IF hCurrObject:FRAME:TYPE = "dialog-box" THEN hCurrObject:FRAME ELSE hCurrObject:WINDOW).
          DO ix = 1 TO hCurrObject:DOWN - 1:
            IF NOT hCurrObject:SELECT-NEXT-ROW() THEN DO:
              APPLY "off-end" TO hCurrObject.
              IF hCurrObject:FOCUSED-ROW NE ? THEN DO:
                hCurrObject:SELECT-ROW(hCurrObject:FOCUSED-ROW).
                LEAVE.
              END.
            END.
          END.
          APPLY "value-changed" TO hCurrObject.
          DoLockWindow(?).
          bMyReturnNoApply = YES.
          LEAVE WindowsBrowse.
        END.
      END.
    END.
    /* up: */
    ELSE DO:
      IF cLastEvent BEGINS "shift" THEN DO:
        bAdjRowSelected = hCurrObject:IS-ROW-SELECTED(hCurrObject:FOCUSED-ROW + 1) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN LEAVE WindowsBrowse.
        IF hCurrObject:NUM-SELECTED-ROWS > 1 AND NOT bAdjRowSelected THEN DO:
          hCurrObject:DESELECT-FOCUSED-ROW().
          ASSIGN iCurrRow    = iCurrRow - 1
                 bCurrRowSet = YES.
        END.
        ELSE DO:
          IF hCurrObject:FOCUSED-ROW > 1 THEN 
            hCurrObject:SELECT-ROW(hCurrObject:FOCUSED-ROW - 1) NO-ERROR.
          ELSE DO:
            IF hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
              rRepos = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.
            hCurrObject:QUERY:GET-PREV().
            IF NOT hCurrObject:QUERY:QUERY-OFF-END THEN 
              hCurrObject:SCROLL-TO-CURRENT-ROW().
            ELSE IF getAttribute(hCurrObject,"firstrowid") NE STRING(rRepos) THEN DO:
              APPLY "off-home" TO hCurrObject.
              IF rRepos NE ? THEN DO:
                hCurrObject:QUERY:REPOSITION-TO-ROWID(rRepos).
                iCurrRow = hCurrObject:FOCUSED-ROW.
                hCurrObject:SELECT-ROW(iCurrRow) NO-ERROR.            
              END.
              bSelectAll = YES.
            END.
          END.
        END.
        IF NOT cLastEvent BEGINS "shift-page" THEN
          APPLY "value-changed" TO hCurrObject.
      END.
      ELSE DO:
        IF cLastEvent MATCHES "*cursor*" THEN DO:
          IF hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
            rRepos = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.
          IF hCurrObject:QUERY:NUM-RESULTS > 0 AND NOT hCurrObject:SELECT-PREV-ROW() THEN DO:
            APPLY "off-home" TO hCurrObject.
            IF rRepos NE ? THEN DO:
              hCurrObject:QUERY:REPOSITION-TO-ROWID(rRepos).
              hCurrObject:SELECT-ROW(1) NO-ERROR.            
            END.
          END.
          APPLY "value-changed" TO hCurrObject.
        END.
        ELSE DO:
          DoLockWindow(IF hCurrObject:FRAME:TYPE = "dialog-box" THEN hCurrObject:FRAME ELSE hCurrObject:WINDOW).
          DO ix = 1 TO hCurrObject:DOWN - 1:
            IF hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
              rRepos = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.
            IF hCurrObject:QUERY:NUM-RESULTS > 0 AND NOT hCurrObject:SELECT-PREV-ROW() THEN DO:
              APPLY "off-home" TO hCurrObject.
              IF rRepos NE ? THEN DO:
                hCurrObject:QUERY:REPOSITION-TO-ROWID(rRepos).
                hCurrObject:SELECT-ROW(1) NO-ERROR.            
              END.
              LEAVE.
            END.
          END.
          APPLY "value-changed" TO hCurrObject.
          DoLockWindow(?).
          bMyReturnNoApply = YES.
          LEAVE WindowsBrowse.
        END.
      END.
    END.
    IF cLastEvent BEGINS "shift" AND rSelectionStart NE ? THEN DO:
      ASSIGN rRepos   = IF hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID ELSE rRepos
             iCurrRow = IF NOT bCurrRowSet THEN hCurrObject:FOCUSED-ROW ELSE iCurrRow.
      IF bSelectAll THEN
        hCurrObject:SELECT-ALL(rSelectionStart,rRepos) NO-ERROR.
      hCurrObject:SET-REPOSITIONED-ROW(iCurrRow,"always").
      hCurrObject:QUERY:REPOSITION-TO-ROWID(rRepos) NO-ERROR.
      hCurrObject:SELECT-ROW(iCurrRow) NO-ERROR.
    END.
    ELSE IF NOT cLastEvent BEGINS "shift" THEN 
      hCurrObject:SELECT-FOCUSED-ROW() NO-ERROR.
  END.
END.

ASSIGN iTmpXpixels = hCurrObject:FRAME:WIDTH-PIXELS
       iTmpYpixels = hCurrObject:FRAME:HEIGHT-PIXELS
       .

FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = hCurrObject
      AND ttObjectLink.cLinkType   = "browseoverlay":

  ttObjectLink.hToObject:VISIBLE = FALSE.
  FOR EACH ttEvent
      WHERE ttEvent.hObject = ttObjectLink.hToObject
        AND ttEvent.cName   = "lookup":
    ttEvent.hWidget:VISIBLE = NO.
  END.
END.

IF hCurrObject:FRAME:TYPE NE "dialog-box" THEN
  ASSIGN hCurrObject:FRAME:WIDTH-PIXELS  = iTmpXpixels
         hCurrObject:FRAME:HEIGHT-PIXELS = iTmpYpixels
         NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectBrowse Procedure 
PROCEDURE SelectAllRowsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hRecordSelectWidget AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.

IF AVAIL ttObject THEN DO:
  IF ttObject.hObject:TYPE NE "browse" THEN DO:
    hBrowse = DYNAMIC-FUNCTION("getLinkedObject",ttObject.hObject,"browse","from").
    IF NOT VALID-HANDLE(hBrowse) OR hBrowse:TYPE NE "browse" THEN DO:
      MESSAGE "Missing association (link) from " ttObject.cObjectType " to browse" SKIP
              "Programmers mistake"
              VIEW-AS ALERT-BOX.
      RETURN.
    END.
  END.
  ELSE hBrowse = ttObject.hObject.
END.  
ELSE RETURN.
  
hBrowse:SELECT-ALL().
hRecordSelectWidget = WIDGET-HANDLE(getAttribute(hBrowse:WINDOW,"RecordSelectWidget")) NO-ERROR.

IF VALID-HANDLE(hRecordSelectWidget) THEN 
  IF hBrowse:NUM-SELECTED-ROWS > 0 THEN
    hRecordSelectWidget:SCREEN-VALUE = STRING(hBrowse:NUM-SELECTED-ROWS,"zzzzz9").
  ELSE
    hRecordSelectWidget:SCREEN-VALUE = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectFilterRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectFilterRecord Procedure 
PROCEDURE SelectFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:    Allow the user to select filter window of choice 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFilterSelect AS HANDLE NO-UNDO.
hFilterSelect = StartWindow("JBoxSelectFilter.w","restart").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftCtrlCursorLeftBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftCtrlCursorLeftBrowse Procedure 
PROCEDURE ShiftCtrlCursorLeftBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCol        AS HANDLE NO-UNDO.
DEF VAR hCurrColumn AS HANDLE NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

hBrowse = ttObject.hObject.

EMPTY TEMP-TABLE ttBrowseColumns.

DO ix = 1 TO hBrowse:NUM-COLUMNS:
  hCol = hBrowse:GET-BROWSE-COLUMN(ix).
  IF CAN-DO(getAttribute(hBrowse,"NoColumnSort"),hCol:NAME) OR NOT hCol:VISIBLE THEN NEXT.
  IF hCol:NAME = getAttribute(hBrowse,"localsort") THEN
    hCurrColumn = hCol.
  CREATE ttBrowseColumns.
  ASSIGN ttBrowseColumns.hColumn = hCol
         ttBrowseColumns.iPos    = (IF hCol:X = -1 THEN -10000 + ix ELSE hCol:X)
         .
END.

IF NOT VALID-HANDLE(hCurrColumn) THEN DO:
  FIND LAST ttBrowseColumns NO-ERROR.
  IF AVAIL ttBrowseColumns THEN
    hCurrColumn = ttBrowseColumns.hColumn.  
END.
ELSE DO:
  FIND LAST ttBrowseColumns
       WHERE ttBrowseColumns.iPos < hCurrColumn:X
       NO-ERROR.
  IF AVAIL ttBrowseColumns THEN DO:
    hCurrColumn = ttBrowseColumns.hColumn.
    IF hCurrColumn:X < hBrowse:WIDTH-PIXELS THEN DO:
      REPEAT WHILE hCurrColumn:X < 5 AND ix < 50:
        APPLY "cursor-left" TO hBrowse.
        ix = ix + 1.
      END.
    END.
  END.
  ELSE DO:
    FIND LAST ttBrowseColumns NO-ERROR.
    IF AVAIL ttBrowseColumns THEN DO:
      ASSIGN hCurrColumn = ttBrowseColumns.hColumn
             ix          = 0.     
      REPEAT WHILE hCurrColumn:X + hCurrColumn:WIDTH-PIXELS > hBrowse:WIDTH-PIXELS AND hCurrColumn:X > 5 AND ix < 50:
        APPLY "cursor-right" TO hBrowse.
        ix = ix + 1.
      END.
    END.
  END.
END.

IF VALID-HANDLE(hCurrColumn) THEN DO:
  DYNAMIC-FUNCTION("setSortLabel",hBrowse,hCurrColumn:NAME,NO).
  DYNAMIC-FUNCTION("setSortString",hBrowse,hCurrColumn:NAME).
  setBrowseSearchField(hBrowse,hCurrColumn).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftCtrlCursorLeftSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftCtrlCursorLeftSortSearch Procedure 
PROCEDURE ShiftCtrlCursorLeftSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hBrowse AS HANDLE NO-UNDO.

hBrowse = DYNAMIC-FUNCTION("GetLinkedObject",ttObject.hObject,"browse","from").
IF VALID-HANDLE(hBrowse) THEN
  APPLY "shift-ctrl-cursor-left" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftCtrlCursorRightBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftCtrlCursorRightBrowse Procedure 
PROCEDURE ShiftCtrlCursorRightBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hCol        AS HANDLE NO-UNDO.
DEF VAR hCurrColumn AS HANDLE NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

hBrowse = ttObject.hObject.

IF DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browse-search-field","from") = ? THEN RETURN.

EMPTY TEMP-TABLE ttBrowseColumns.

DO ix = 1 TO hBrowse:NUM-COLUMNS:
  hCol = hBrowse:GET-BROWSE-COLUMN(ix).
  IF CAN-DO(getAttribute(hBrowse,"NoColumnSort"),hCol:NAME) OR NOT hCol:VISIBLE THEN NEXT.
  IF hCol:NAME = getAttribute(hBrowse,"localsort") THEN
    hCurrColumn = hCol.
  CREATE ttBrowseColumns.
  ASSIGN ttBrowseColumns.hColumn = hCol
         ttBrowseColumns.iPos    = (IF hCol:X = -1 THEN -10000 + ix ELSE hCol:X)
         .
END.

IF NOT VALID-HANDLE(hCurrColumn) THEN DO:
  FIND FIRST ttBrowseColumns NO-ERROR.
  IF AVAIL ttBrowseColumns THEN
    hCurrColumn = ttBrowseColumns.hColumn.  
END.
ELSE DO:
  FIND FIRST ttBrowseColumns
       WHERE ttBrowseColumns.iPos > hCurrColumn:X
       NO-ERROR.
  IF AVAIL ttBrowseColumns THEN DO:
    hCurrColumn = ttBrowseColumns.hColumn.
    IF hCurrColumn:X + hCurrColumn:WIDTH-PIXELS > hBrowse:WIDTH-PIXELS THEN DO:
      ix = 0.
      REPEAT WHILE hCurrColumn:X + hCurrColumn:WIDTH-PIXELS > hBrowse:WIDTH-PIXELS AND hCurrColumn:X > 5 AND ix < 50 /* - MIN(hCurrColumn:WIDTH-PIXELS,hBrowse:WIDTH-PIXELS) */:
        APPLY "cursor-right" TO hBrowse.
        ix = ix + 1.
      END.
      IF hCurrColumn:X + hCurrColumn:WIDTH-PIXELS + 30 > hBrowse:WIDTH-PIXELS AND hCurrColumn:WIDTH-PIXELS < hBrowse:WIDTH-PIXELS - 50 THEN
        DO ix = 1 TO 4:
          APPLY "cursor-right" TO hBrowse.
        END.
    END.
  END.
  ELSE DO:
    FIND FIRST ttBrowseColumns NO-ERROR.
    IF AVAIL ttBrowseColumns THEN DO:
      hCurrColumn = ttBrowseColumns.hColumn.     
      REPEAT WHILE hCurrColumn:X < 0:
        APPLY "cursor-left" TO hBrowse.
      END.
    END.
  END.
END.

IF VALID-HANDLE(hCurrColumn) THEN DO:
  DYNAMIC-FUNCTION("setSortLabel",hBrowse,hCurrColumn:NAME,NO).
  DYNAMIC-FUNCTION("setSortString",hBrowse,hCurrColumn:NAME).
  setBrowseSearchField(hBrowse,hCurrColumn).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftCtrlCursorRightSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftCtrlCursorRightSortSearch Procedure 
PROCEDURE ShiftCtrlCursorRightSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hBrowse AS HANDLE NO-UNDO.

hBrowse = DYNAMIC-FUNCTION("GetLinkedObject",ttObject.hObject,"browse","from").
IF VALID-HANDLE(hBrowse) THEN
  APPLY "shift-ctrl-cursor-right" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftCursorDownBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftCursorDownBrowse Procedure 
PROCEDURE ShiftCursorDownBrowse :
/*------------------------------------------------------------------------------
  Purpose:     To mimic Windows behaviour for selecting rows in a multi-select browse
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF ((getAttribute(SESSION,"windowsbrowse") = "yes" AND getAttribute(ttObject.hObject,"windowsbrowse") NE "no") OR 
     getAttribute(ttObject.hObject,"windowsbrowse") = "yes") AND ttObject.hObject:MULTIPLE THEN 
  APPLY "cursor-down" TO ttObject.hObject.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftCursorUpBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftCursorUpBrowse Procedure 
PROCEDURE ShiftCursorUpBrowse :
/*------------------------------------------------------------------------------
  Purpose:     To mimic Windows behaviour for selecting rows in a multi-select browse
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF ((getAttribute(SESSION,"windowsbrowse") = "yes" AND getAttribute(ttObject.hObject,"windowsbrowse") NE "no") OR 
     getAttribute(ttObject.hObject,"windowsbrowse") = "yes") AND ttObject.hObject:MULTIPLE THEN 
  APPLY "cursor-up" TO ttObject.hObject.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftEndBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftEndBrowse Procedure 
PROCEDURE ShiftEndBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR rFirstRow   AS ROWID  NO-UNDO.
DEF VAR rLastRow    AS ROWID  NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.

IF ((getAttribute(SESSION,"windowsbrowse") = "yes" AND getAttribute(hCurrObject,"windowsbrowse") NE "no") OR 
     getAttribute(hCurrObject,"windowsbrowse") = "yes") AND hCurrObject:MULTIPLE AND hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
         
  IF hCurrObject:NUM-SELECTED-ROWS > 0 THEN 
    hCurrObject:FETCH-SELECTED-ROW(1). 
  ELSE IF hCurrObject:FOCUSED-ROW NE ? AND NOT hCurrObject:IS-ROW-SELECTED(hCurrObject:FOCUSED-ROW) THEN
    hCurrObject:SELECT-FOCUSED-ROW().

  rFirstRow = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.

  hCurrObject:QUERY:GET-LAST().
  rLastRow = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.

  IF STRING(rLastRow) NE getAttribute(hCurrObject,"lastrowid") THEN DO:
    APPLY "off-end" TO hCurrObject.
    hCurrObject:QUERY:GET-LAST().
    rLastRow = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.
  END.

  hCurrObject:SELECT-ALL(rFirstRow,rLastRow).

  hCurrObject:SET-REPOSITIONED-ROW(hCurrObject:DOWN,"conditional").
  hCurrObject:QUERY:REPOSITION-TO-ROWID(rLastRow).

  IF STRING(rLastRow) NE getAttribute(hCurrObject,"lastrowid") THEN DO:
    IF DoMessage(0,4,IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                       "Ikke alle rader er hentet fra server. Vil du hente og markere neste batch"
                     ELSE
                       "Not all rows are fetched from server. Retrieve and select next batch?"
                    ,"","") = 6 THEN DO:
      bMyReturnNoApply = YES.
      RUN ShiftEndBrowse.
    END.
  END.

  bMyReturnNoApply = YES.
END.
APPLY "value-changed" TO hCurrObject.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftHomeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftHomeBrowse Procedure 
PROCEDURE ShiftHomeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR rFirstRow   AS ROWID  NO-UNDO.
DEF VAR rLastRow    AS ROWID  NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.


IF ((getAttribute(SESSION,"windowsbrowse") = "yes" AND getAttribute(hCurrObject,"windowsbrowse") NE "no") OR 
     getAttribute(hCurrObject,"windowsbrowse") = "yes") AND hCurrObject:MULTIPLE AND hCurrObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:

  IF hCurrObject:NUM-SELECTED-ROWS > 0 THEN 
    hCurrObject:FETCH-SELECTED-ROW(hCurrObject:NUM-SELECTED-ROWS). 
  ELSE IF hCurrObject:FOCUSED-ROW NE ? AND NOT hCurrObject:IS-ROW-SELECTED(hCurrObject:FOCUSED-ROW) THEN
    hCurrObject:SELECT-FOCUSED-ROW().

  rLastRow = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.

  hCurrObject:QUERY:GET-FIRST().
  rFirstRow = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.

  IF STRING(rFirstRow) NE getAttribute(hCurrObject,"firstrowid") THEN DO:
    APPLY "off-home" TO hCurrObject.
    hCurrObject:QUERY:GET-FIRST().
    rFirstRow = hCurrObject:QUERY:GET-BUFFER-HANDLE(1):ROWID.
  END.

  hCurrObject:SELECT-ALL(rFirstRow,rLastRow).

  hCurrObject:SET-REPOSITIONED-ROW(1,"always").
  hCurrObject:QUERY:REPOSITION-TO-ROWID(rFirstRow).

  IF STRING(rFirstRow) NE getAttribute(hCurrObject,"firstrowid") THEN DO:
    IF DoMessage(0,4,IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                       "Ikke alle rader er hentet fra server. Vil du hente og markere neste batch"
                     ELSE
                       "Not all rows are fetched from server. Retrieve and select next batch?"
                    ,"","") = 6 THEN DO:
      bMyReturnNoApply = YES.
      RUN ShiftHomeBrowse.
    END.
  END.

  bMyReturnNoApply = YES.
END.

APPLY "value-changed" TO hCurrObject.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftPageDownBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftPageDownBrowse Procedure 
PROCEDURE ShiftPageDownBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix          AS INT NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.
IF ((getAttribute(SESSION,"windowsbrowse") = "yes" AND getAttribute(hCurrObject,"windowsbrowse") NE "no") OR 
     getAttribute(hCurrObject,"windowsbrowse") = "yes") AND hCurrObject:MULTIPLE THEN DO:
  DoLockWindow(IF hCurrObject:FRAME:TYPE = "dialog-box" THEN hCurrObject:FRAME ELSE hCurrObject:WINDOW).
  setAttribute(SESSION,"windowislocked","yes").
  DO ix = 1 TO hCurrObject:DOWN - 1:
    RUN ScrollNotifyBrowse.
  END.
  APPLY "value-changed" TO hCurrObject.
  setAttribute(SESSION,"windowislocked","").
  DoLockWindow(?).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ShiftPageUpBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShiftPageUpBrowse Procedure 
PROCEDURE ShiftPageUpBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.
DEF VAR hCurrObject AS HANDLE NO-UNDO.

hCurrObject = ttObject.hObject.
IF getAttribute(SESSION,"windowsbrowse") = "yes" AND hCurrObject:MULTIPLE THEN DO:
  DoLockWindow(IF hCurrObject:FRAME:TYPE = "dialog-box" THEN hCurrObject:FRAME ELSE hCurrObject:WINDOW).
  setAttribute(SESSION,"windowislocked","yes").
  DO ix = 1 TO hCurrObject:DOWN - 1:
    RUN ScrollNotifyBrowse.
  END.
  APPLY "value-changed" TO hCurrObject.
  setAttribute(SESSION,"windowislocked","").
  DoLockWindow(?).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartDynFilterRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartDynFilterRecord Procedure 
PROCEDURE StartDynFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object must be (set to) the filter object 
------------------------------------------------------------------------------*/
DEF VAR hWidget          AS HANDLE  NO-UNDO.
DEF VAR cFilterString    AS CHAR    NO-UNDO.
DEF VAR hQuery           AS HANDLE  NO-UNDO.
DEF VAR hBuffer          AS HANDLE  NO-UNDO.
DEF VAR hCurrObject      AS HANDLE  NO-UNDO.
DEF VAR hLastWidget      AS HANDLE  NO-UNDO.
DEF VAR cInUseList       AS CHAR    NO-UNDO.
DEF VAR iNumInUse        AS INT     NO-UNDO.
DEF VAR cGroupList       AS CHAR    NO-UNDO.
DEF VAR cGroupOperList   AS CHAR    NO-UNDO.
DEF VAR bOkGroup         AS LOG     NO-UNDO INIT TRUE.
DEF VAR cCleanUpList     AS CHAR    NO-UNDO.
DEF VAR iPrevGroup       AS INT     NO-UNDO.
DEF VAR hQueryObject     AS HANDLE  NO-UNDO.
DEF VAR hDynFilter       AS HANDLE  NO-UNDO.
DEF VAR cBufferList      AS CHAR    NO-UNDO.
DEF VAR iy               AS INT     NO-UNDO.
DEF VAR cPrescanQuery    AS CHAR    NO-UNDO.
DEF VAR cCurrPreScanQry  AS CHAR    NO-UNDO.
DEF VAR cBaseTableFilter AS CHAR    NO-UNDO.
DEF VAR cAllCalcFields   AS CHAR    NO-UNDO.
DEF VAR cCalcFieldFilter AS CHAR    NO-UNDO.
DEF VAR bActiveFilter    AS LOG     NO-UNDO.
DEF VAR cBaseQuery       AS CHAR    NO-UNDO.
DEF VAR cQueryFilter     AS CHAR    NO-UNDO.
DEF VAR iz               AS INT     NO-UNDO.
DEF VAR ix               AS INT     NO-UNDO.
DEF VAR cGrpOper         AS CHAR    NO-UNDO.

ASSIGN hCurrObject = ttObject.hObject
       hLastWidget = FOCUS
       .

IF PROGRAM-NAME(2) MATCHES "*JBoxDynFilter.w" AND VALID-HANDLE(hLastWidget) AND hLastWidget:TYPE = "fill-in" THEN
  APPLY "return" TO hLastWidget.

IF ttObject.hObject NE hCurrObject THEN
  FIND FIRST ttObject WHERE ttObject.hObject = hCurrObject.

IF ttObject.cObjectType = "toolbar" THEN DO:
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = ttObject.hObject
         AND bttObjectLink.cLinkType   = "dynfilter" NO-ERROR.
  IF AVAIL bttObjectLink THEN
    FIND ttObject WHERE ttObject.hObject = bttObjectLink.hToObject NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN.
END.
IF NOT ttObject.cObjectType = "dynfilter" THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "DynFilter object not avail" SKIP
          "ttObject.cObjectType: " ttObject.cObjectType
          VIEW-AS ALERT-BOX.
  RETURN.
END.
hDynFilter = ttObject.hObject.
                                             
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = hDynFilter
       AND ttObjectLink.cLinkType   = "browse" NO-ERROR.
IF NOT AVAIL ttObjectLink THEN
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = hDynFilter
         AND ttObjectLink.cLinkType   = "query" NO-ERROR.

IF AVAIL ttObjectLink THEN DO:
  hQueryObject = ttObjectLink.hToObject.

  FOR EACH ttAttribute
      WHERE ttAttribute.hObject = hQueryObject:
    IF ttAttribute.cName BEGINS "OperatorInUse_"
      OR ttAttribute.cName BEGINS "FilterValue_"
      OR ttAttribute.cName BEGINS "GroupOperator_"
      OR ttAttribute.cName BEGINS "FieldGroup_"
      OR ttAttribute.cName BEGINS "FieldOperator_"
      OR ttAttribute.cName BEGINS "HiddenGroupOperator_"
      THEN
     DELETE ttAttribute.
  END.

  PUBLISH "BeforeSetDynFilter" (hQueryObject).

  bActiveFilter = setDynFilter("Execute",hDynFilter,hQueryObject,"","").
  IF bActiveFilter = ? THEN RETURN.

  PUBLISH "DynFilterDefined" (hQueryObject).

  hWidget = WIDGET-HANDLE(getAttribute(hDynFilter,"filterbutton")).
  IF VALID-HANDLE(hWidget) THEN DO:
    IF hWidget:TYPE NE "button" THEN DO:
      FIND FIRST ttEvent
           WHERE ttEvent.hWidget = hWidget NO-ERROR.
      IF AVAIL ttEvent THEN DO:
        FIND FIRST bttEvent
             WHERE bttEvent.hObject     = ttEvent.hObject
               AND bttEvent.cAction     = ttEvent.cAction
               AND bttEvent.cWidgetType = "button"
             NO-ERROR.
        IF AVAIL bttEvent AND VALID-HANDLE(bttEvent.hWidget) THEN
          hWidget = bttEvent.hWidget.
      END.
    END.
    &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
      IF cActiveFilterButton NE "" AND bActiveFilter THEN
        hWidget:LOAD-IMAGE(cActiveFilterButton) NO-ERROR.
      ELSE IF NOT bActiveFilter AND cPassiveFilterButton NE "" THEN
        hWidget:LOAD-IMAGE(cPassiveFilterButton) NO-ERROR.
    &ENDIF    
  END.

  IF getAttribute(hQueryObject,"executedynfilter") NE "no" THEN DO:
    ASSIGN bSetCurrHandles = FALSE
           hCurrWidget    = hQueryObject
           hTmpObject     = hCurrWidget
           .
  
    setCurrentObject(hQueryObject).
    setCurrentSourceProc(ttObject.hSourceProc).
    RUN DoProcessEvent(0,"open-query"). 
  END.
  setAttribute(hQueryObject,"executedynfilter","").
END.

IF getAttribute(SESSION,"UIB_is_Running") = "yes" AND (VALID-HANDLE(hWidget) OR hQueryObject:TYPE = "BROWSE") THEN
  DYNAMIC-FUNCTION("DoRedrawWindow",IF VALID-HANDLE(hWidget) THEN hWidget:WINDOW ELSE hQueryObject:WINDOW).
   
setCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartFilterRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartFilterRecord Procedure 
PROCEDURE StartFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object must be (set to) the filter object 
------------------------------------------------------------------------------*/
DEF VAR hWidget       AS HANDLE NO-UNDO.
DEF VAR cFilterString AS CHAR   NO-UNDO.
DEF VAR cOperator     AS CHAR   NO-UNDO.
DEF VAR hBrowse       AS HANDLE NO-UNDO.
DEF VAR cFieldList    AS CHAR   NO-UNDO.
DEF VAR cOperatorList AS CHAR   NO-UNDO.
DEF VAR cValueList    AS CHAR   NO-UNDO.
DEF VAR cValue        AS CHAR   NO-UNDO.

IF ttObject.cObjectType = "toolbar" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "filter" NO-ERROR.
  IF AVAIL ttObjectLink THEN
    FIND ttObject WHERE ttObject.hObject = ttObjectLink.hToObject NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN.
END.
IF NOT ttObject.cObjectType = "filter" THEN
  RETURN.
                                             
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse" NO-ERROR.
IF NOT AVAIL ttObjectLink THEN
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "query" NO-ERROR.
ELSE hBrowse = ttObjectLink.hToObject.

IF AVAIL ttObjectLink THEN DO:
  FOR EACH bttAttribute NO-LOCK OF ttObject
      WHERE bttAttribute.cName BEGINS "filterfield_":
  
    hWidget = WIDGET-HANDLE(getAttribute(ttObject.hObject,bttAttribute.cName)).

    setAttribute(ttObjectLink.hToObject,
                 "filtervalue_" + hWidget:NAME,IF hWidget:SCREEN-VALUE NE ? THEN hWidget:SCREEN-VALUE ELSE "").
    
    IF hWidget:SCREEN-VALUE NE ? 
       AND hWidget:SCREEN-VALUE NE "" 
       AND IF hWidget:DATA-TYPE = "INTEGER" THEN INT(hWidget:SCREEN-VALUE) NE 0 ELSE TRUE
       AND IF hWidget:DATA-TYPE = "DECIMAL" THEN DEC(hWidget:SCREEN-VALUE) NE 0 ELSE TRUE
       THEN DO:

      cOperator = "".
      IF hWidget:DATA-TYPE = "CHARACTER" THEN
        cOperator = (IF getAttribute(ttObject.hObject,"operator_" + hWidget:NAME) NE "" THEN
                       getAttribute(ttObject.hObject,"operator_" + hWidget:NAME)
                     ELSE IF INDEX(hWidget:SCREEN-VALUE,"*") > 0 THEN
                       "MATCHES"
                     ELSE "BEGINS").
      ELSE                     
        cOperator = (IF getAttribute(ttObject.hObject,"operator_" + hWidget:NAME) NE "" THEN
                       getAttribute(ttObject.hObject,"operator_" + hWidget:NAME)
                     ELSE "=").

      IF cOperator NE "" THEN 
        ASSIGN cValue = (IF hWidget:DATA-TYPE      = "CHARACTER" THEN " '" + hWidget:SCREEN-VALUE + "'"
                         ELSE IF hWidget:DATA-TYPE = "LOGICAL"   THEN " LOGICAL('" + hWidget:INPUT-VALUE + "')"
                         ELSE hWidget:DATA-TYPE + "('" + hWidget:SCREEN-VALUE + "')")
               cFilterString = cFilterString + getAttribute(ttObject.hObject,"bufferfield_" + hWidget:NAME) + " " + cOperator + cValue + " AND "
               cFieldList    = cFieldList + (IF cFieldList NE "" THEN "," ELSE "") + getAttribute(ttObject.hObject,"bufferfield_" + hWidget:NAME)
               cOperatorList = cOperatorList + (IF cOperatorList NE "" THEN "," ELSE "") + cOperator
               cValueList    = cValueList + (IF cValueList NE "" THEN "|" ELSE "") + hWidget:SCREEN-VALUE.
               .

      setAttribute(ttObjectLink.hToObject,"OperatorInUse_" + hWidget:NAME,cOperator).

    END.      
  END.
  DYNAMIC-FUNCTION("initDynFilter",ttObjectLink.hToObject,cFieldList,cOperatorList,cValueList,"static").  
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch Procedure 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is always the browse
------------------------------------------------------------------------------*/
DEF VAR bDesc           AS LOG    NO-UNDO.
DEF VAR cSortMap        AS CHAR   NO-UNDO.
DEF VAR cNoColumnSearch AS CHAR   NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR iMaxCount       AS INT    NO-UNDO.
DEF VAR hCurrColumn     AS HANDLE NO-UNDO.
DEF VAR cCurrSort       AS CHAR   NO-UNDO.
DEF VAR cCalcFields     AS CHAR   NO-UNDO.
DEF VAR cNewSort        AS CHAR   NO-UNDO.
DEF VAR cMappedSort     AS CHAR   NO-UNDO.
DEF VAR cCurrMappedSort AS CHAR   NO-UNDO.
DEF VAR cSortBuffer     AS CHAR   NO-UNDO.
DEF VAR bOkSort         AS LOG    NO-UNDO.
DEF VAR cExtentFlds     AS CHAR   NO-UNDO.
DEF VAR cAltExtFlds     AS CHAR   NO-UNDO.
DEF VAR cAltExtentField AS CHAR   NO-UNDO.

IF VALID-HANDLE(hCursorCurrColumn) THEN
  hCurrColumn = hCursorCurrColumn.
ELSE DO:
  IF NOT VALID-HANDLE(ttObject.hObject:CURRENT-COLUMN) THEN RETURN.
  
  IF CAN-DO(getAttribute(ttObject.hObject,"NoColumnSort"),ttObject.hObject:CURRENT-COLUMN:NAME) THEN DO:
    APPLY "end-search" TO ttObject.hObject.
    RETURN.
  END.
END.

ASSIGN bDesc       = getAttribute(ttObject.hObject,"querydesc") = "desc" 
       hCurrColumn = (IF VALID-HANDLE(hCursorCurrColumn) THEN hCursorCurrColumn ELSE ttObject.hObject:CURRENT-COLUMN)
       cNewSort = hCurrColumn:NAME
       cExtentFlds = getAttribute(ttObject.hObject,"extentFields")
       cAltExtFlds = getAttribute(ttObject.hObject,"altExtentFields") 
       cSortMap = getAttribute(ttObject.hObject,"sortmap")
       .

IF CAN-DO(cExtentFlds,cNewSort) THEN DO:
  cAltExtentField =  ENTRY(LOOKUP(cNewSort,cExtentFlds),cAltExtFlds).
  IF LOOKUP(cNewSort + ";" + cAltExtentField,cSortMap) = 0 AND SUBSTR(cNewSort,LENGTH(cNewSort)) NE "]" THEN DO:
    cSortMap = cSortMap + (IF cSortMap NE "" THEN "," ELSE "") 
             + cNewSort + ";" 
             + getAttribute(ttObject.hObject,"orgdbfield"
                         + SUBSTR(cNewSort,1,R-INDEX(cNewSort,"_") - 1)
                         + "[" + getAttribute(ttObject.hObject,"extent_" + cNewSort) + "]")
             .
    setAttribute(ttObject.hObject,"sortMap",cSortMap).
  END.
END.
cSortMap = getAttribute(ttObject.hObject,"sortmap").

DO ix = 1 TO NUM-ENTRIES(cSortMap):
  IF getAttribute(ttObject.hObject,"querysort") = ENTRY(2,ENTRY(ix,cSortMap),";") THEN 
    cCurrMappedSort = ENTRY(1,ENTRY(ix,cSortMap),";").
  IF cNewSort = ENTRY(1,ENTRY(ix,cSortMap),";") THEN
    cMappedSort = ENTRY(2,ENTRY(ix,cSortMap),";").
END.

IF cCurrMappedSort = "" AND getAttribute(ttObject.hObject,"localsort") = "" THEN
  cCurrMappedSort = getAttribute(ttObject.hObject,"querysort").

IF getAttribute(ttObject.hObject,"localsort") NE "" THEN
  cCurrSort = getAttribute(ttObject.hObject,"localsort").
ELSE
  cCurrSort = cCurrMappedSort.

IF cCurrSort EQ cNewSort THEN
  bDesc = NOT bDesc.
ELSE 
  bDesc = NO.

cSortBuffer = getAttribute(ttObject.hObject,"fieldbuffer" + (IF cMappedSort NE "" THEN cMappedSort ELSE cNewSort)).

DoLockWindow(ttObject.hObject:WINDOW).
setAttribute(SESSION,"userkeepswindowlocked","yes").

DYNAMIC-FUNCTION("setQueryStatFields",""). /* <-- Never count or retrieve statistics when data is sortet */

bOK = YES.
IF getAttribute(ttObject.hObject,"uselocaldata") NE "yes" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cSortMap):
    IF ENTRY(1,ENTRY(ix,cSortMap),";") = cNewSort THEN DO:
      setAttribute(ttObject.hObject,"querysort",ENTRY(2,ENTRY(ix,cSortMap),";")).
      setAttribute(ttObject.hObject,"localsort","").
      setAttribute(ttObject.hObject,"sortbuffer",getAttribute(ttObject.hObject,"fieldbuffer" + ENTRY(2,ENTRY(ix,cSortMap),";"))).
      bOK = FALSE.
    END.
  END.
  IF bOK THEN DO:
    cCalcFields = getAttribute(ttObject.hObject,"allcalcfields").
    setAttribute(ttObject.hObject,"querysort",
                 (IF NOT CAN-DO(cCalcFields,cNewSort) THEN
                    getAttribute(ttObject.hObject,"fieldbuffer" + cNewSort) + "." 
                  + getAttribute(ttObject.hObject,"orgdbfield" + cNewSort)
                  ELSE cNewSort)
                 ).
    setAttribute(ttObject.hObject,"localsort",cNewSort).
    setAttribute(ttObject.hObject,"sortbuffer",getAttribute(ttObject.hObject,"fieldbuffer" + cNewSort)).
  END.
END.
ELSE DO:
  setAttribute(ttObject.hObject,"querysort",cNewSort).
  setAttribute(ttObject.hObject,"localsort",cNewSort).
  setAttribute(ttObject.hObject,"sortbuffer",getAttribute(ttObject.hObject,"fieldbuffer" + cNewSort)).
END.

setAttribute(ttObject.hObject,"querydesc",IF bDesc THEN "desc" ELSE "").
bSetCurrHandles = FALSE.

setAttribute(ttObject.hObject,"querystart","0").

hBrowse = ttObject.hObject.

bOk = TRUE.

IF NOT VALID-HANDLE(hCurrSourceProc) THEN hCurrSourceProc = SOURCE-PROCEDURE.
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraStartSearch") THEN 
  RUN ExtraStartSearch IN hCurrSourceProc (ttObject.hObject,cNewSort,bDesc,OUTPUT bOk).

IF bOk THEN DO:
  IF (getAttribute(ttObject.hObject,"checkForIndexBeforeSort") = "yes" OR
      (getAttribute(ttObject.hObject,"checkForIndexBeforeSort") = "calcFields" AND CAN-DO(getAttribute(ttObject.hObject,"allCalcFields"),cNewSort)))
     AND (cSortBuffer NE "" OR CAN-DO(getAttribute(ttObject.hObject,"allCalcFields"),cNewSort))
     AND getAttribute(ttObject.hObject,"queryFilter") = "" 
     AND getAttribute(ttObject.hObject,"prescanBaseQuery") = ""
     AND getAttribute(ttObject.hObject,"prescanQueryFilter") = ""
     AND getAttribute(ttObject.hObject,"use-index") = ""
     AND getAttribute(ttObject.hObject,"useLocalData") NE "yes" 
     THEN DO:
    bOkSort = YES.
  
    IF cSortBuffer = getAttribute(ttObject.hObject,"baseTable") OR CAN-DO(getAttribute(ttObject.hObject,"altPrimaryBufferList"),cSortBuffer) THEN 
      bOkSort = DYNAMIC-FUNCTION("runProc","jbserv_is_field_indexed.p",
                                  cSortBuffer + "." + (IF cMappedSort NE "" THEN cMappedSort ELSE cNewSort)
                                 ,?).
    ELSE bOkSort = NO.                                                                                                 
  
    IF NOT bOkSort THEN DO:
      bOk = DoMessage (0,1,IF DYNAMIC-FUNCTION("Scandinavian") THEN
                          "Du er i ferd med å sortere på et ikke-indeksert felt eller" + CHR(10)
                        + "utvalget kan medføre ineffektiv bruk av indekser. Denne" + CHR(10)
                        + "operasjonen kan bli tidkrevende. Vil du fortsette?"
                        ELSE
                          "The sort-field is not indexed and could cause poor query performance" + CHR(10)
                        + "Continue?"
                       ,"","") = 1.
    END.
  END.
END.

IF bOK THEN DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",hBrowse,"browse-search-field","from") NE ? THEN DO:
    setAttribute(hBrowse,"querywhere","").
    setAttribute(hBrowse,"prescanquerywhere","").
  END.

  DYNAMIC-FUNCTION("ComposeSortString",hBrowse,NO).

  iMaxCount = DYNAMIC-FUNCTION("fillBrowse",hBrowse,
                      INT(getAttribute(hBrowse,"rowstobatch")),
                      INT(getAttribute(hBrowse,"querystart")),"","","",
                      bDesc).

  setAttribute(hBrowse,"querystart",STRING(iMaxCount)).

  IF iMaxCount = 0 THEN
    hBrowse:QUERY:QUERY-CLOSE().

  setCurrentObject(hBrowse).
  bSetCurrHandles = FALSE.
  RUN DoProcessEvent(0,"value-changed").
END.
ELSE setAttribute(hBrowse,"startsearch_done","no").

setBrowseSearchField(hBrowse,hCurrColumn).

setAttribute(hBrowse,"startsearch_done","").

setCurrentObject(hBrowse).
APPLY "end-search" TO hBrowse.


setAttribute(SESSION,"userkeepswindowlocked","").
DoLockWindow(?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSortSearch Procedure 
PROCEDURE StartSortSearch :
/*------------------------------------------------------------------------------
  Notes: Current object is the search fill-in. Must navigate to the browse      
------------------------------------------------------------------------------*/
DEF VAR hMyTmpFldObj    AS HANDLE NO-UNDO.
DEF VAR hMyTmpQryObj    AS HANDLE NO-UNDO.
DEF VAR iMaxCount       AS INT    NO-UNDO.
DEF VAR bDesc           AS LOG    NO-UNDO.
DEF VAR hCobj           AS HANDLE NO-UNDO.
DEF VAR bReposOk        AS LOG    NO-UNDO.
DEF VAR hTmpQry         AS HANDLE NO-UNDO.
DEF VAR cFieldBuffer    AS CHAR   NO-UNDO.
DEF VAR cPrescanQuery   AS CHAR   NO-UNDO.
DEF VAR cPrescanCrit    AS CHAR   NO-UNDO.
DEF VAR cDbFld          AS CHAR   NO-UNDO.
DEF VAR cClientFld      AS CHAR   NO-UNDO.
DEF VAR cBaseQuery      AS CHAR   NO-UNDO.
DEF VAR cQueryFilter    AS CHAR   NO-UNDO.
DEF VAR cPreScanBaseTbl AS CHAR   NO-UNDO.
DEF VAR cWhereAnd       AS CHAR   NO-UNDO.
DEF VAR cSearchDef      AS CHAR   NO-UNDO. /* goto or filter */
DEF VAR cMainQryCand    AS CHAR   NO-UNDO.
DEF VAR cOperator       AS CHAR   NO-UNDO.
DEF VAR cInpVal         AS CHAR   NO-UNDO.
DEF VAR cCalcFlfFilter  AS CHAR   NO-UNDO.
DEF VAR hRowCnt         AS HANDLE NO-UNDO.
DEF VAR cTemp           AS CHAR   NO-UNDO.

hCobj   = ttObject.hObject.
cInpVal = ttObject.hObject:INPUT-VALUE.

FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN DO:
  MESSAGE "Missing link information from " ttObject.hObject:NAME SKIP
          "Programmers mistake" VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

ASSIGN hTmpObject        = ttObjectLink.hToObject
       hMyTmpQryObj = hTmpObject
       hMyTmpFldObj = ttObject.hObject
       bOk               = TRUE
       bDesc             = getAttribute(hTmpObject,"querydesc") = "desc"
       .

hRowCnt = hTmpObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowCount") NO-ERROR. 

setAttribute(hTmpObject,"querystart","0").

/* Set search buffers to original sequence: */
ChangePrimarySearchBuffer(hTmpObject,"","").

IF NOT VALID-HANDLE(hCurrSourceProc) THEN hCurrSourceProc = SOURCE-PROCEDURE.
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraStartSortSearch") THEN 
  RUN ExtraStartSortSearch IN hCurrSourceProc (hCobj,hTmpObject,getAttribute(hTmpObject,"1stSortColumn"),
                                               OUTPUT bOk).

IF bOK THEN DO:

  IF CAN-DO(getAttribute(hTmpObject,"SearchToFilterDataTypes"),hMyTmpFldObj:DATA-TYPE) THEN DO:
    DYNAMIC-FUNCTION("ChangeDynFilter",hTmpObject,hMyTmpFldObj:NAME,
                     (IF hMyTmpFldObj:DATA-TYPE = "character" THEN
                       (IF INDEX(hMyTmpFldObj:INPUT-VALUE,"*") > 0 AND 
                           INDEX(hMyTmpFldObj:INPUT-VALUE,"*") NE LENGTH(hMyTmpFldObj:INPUT-VALUE) THEN 
                          "matches"
                        ELSE "begins")
                      ELSE "="),
                      hMyTmpFldObj:INPUT-VALUE,
                      "").
    RETURN.
  END.

  DoLockWindow(hTmpObject:WINDOW).
  setAttribute(SESSION,"userkeepswindowlocked","yes").

  RemoveUseIndex(hTmpObject).

  setAttribute(hTmpObject,"prescanquerywhere","").

  cSearchDef = getAttribute(hTmpObject,"searchdefault").
  IF cSearchDef = "" THEN
    cSearchDef = cBrowseSearchDefault.
                                                                                                                               
  ASSIGN cClientFld = getAttribute(hTmpObject,"1stSortColumn")
         cDbFld     = getAttribute(hTmpObject,"1stDbSortColumn").
  IF cClientFld = "" THEN
    cClientFld = IF getAttribute(hTmpObject,"localsort") NE "" THEN getAttribute(hTmpObject,"localsort") ELSE getAttribute(hTmpObject,"querysort").
  IF cDbFld = "" THEN
    cDbFld = getAttribute(hTmpObject,"fieldbuffer" + cClientFld) + "." + getAttribute(hTmpObject,"orgdbfield" + cClientFld).
                                                                                                                         
  cFieldBuffer = IF getAttribute(hTmpObject,"uselocaldata") = "yes" THEN getAttribute(hTmpObject,"basetable") ELSE getAttribute(hTmpObject,"fieldbuffer" + cClientFld).

  IF cFieldBuffer = getAttribute(hTmpObject,"basetable") THEN DO:
    IF getAttribute(hTmpObject,"basequery") NE "" OR getAttribute(hTmpObject,"queryfilter") NE "" THEN
      cWhereAnd = " AND ".
    ELSE cWhereAnd = " WHERE ".
  END.
  ELSE cWhereAnd = " WHERE ".

  IF getAttribute(hTmpObject,"searchoperator" + cClientFld) NE "" THEN
    cOperator = " " + getAttribute(hTmpObject,"searchoperator" + cClientFld) + " ".
  ELSE IF cSearchDef = "goto" THEN
    cOperator = IF getAttribute(hTmpObject,"querydesc") = "desc" THEN " LE " ELSE " GE ".
  ELSE cOperator = " = ".

  CASE hCobj:DATA-TYPE:
    WHEN "CHARACTER" THEN DO:
      IF hCobj:TYPE NE "radio-set" THEN DO:
        IF hCobj:INPUT-VALUE NE "" THEN DO:
          IF cDbFld = "vare.fritekst3" THEN DO:
            DO ix = 1 TO NUM-ENTRIES(cInpVal," "):
              IF ENTRY(ix, cInpVal,"") <> "" THEN 
                cTemp = cTemp + ENTRY(ix, cInpVal, "") + " ".
            END.     
            ASSIGN cInpVal    = TRIM(cTemp)
                   cInpVal    = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(cInpVal,"*",""),"&",""),"|",""),"^",""),"!","")
                   cInpVal    = REPLACE(TRIM(cInpVal)," ","* & ") + "*"
                   cSearchDef = "filter"
                   cOperator      = " CONTAINS "
                   cPrescanCrit   = " WHERE " + cDbFld + cOperator + "'" 
                                  + REPLACE(REPLACE(REPLACE(cInpVal,CHR(39), CHR(126) + CHR(39)),CHR(34), CHR(126) + CHR(34)),",",CHR(1)) + "'"
                   .

          END.
          ELSE IF cSearchDef BEGINS "match" AND INDEX(cInpVal,"*") = 0 THEN DO:
            ASSIGN cSearchDef   = "filter"
                   cOperator    = " MATCHES "
                   cPrescanCrit = " WHERE " + cDbFld + cOperator + "'" + REPLACE("*" + cInpVal + "*",",",CHR(1)) + "'".
            setAttribute(hTmpObject,"querywhere",cWhereAnd + cDbFld + cOperator + 
                                                             "'*" + hCobj:INPUT-VALUE + "*'").
          END.
          ELSE DO:
            cPreScanCrit = " WHERE " + cDbFld +
                           REPLACE(
                           (IF INDEX(cInpVal,"*") > 0 
                               AND INDEX(cInpVal,"*") NE LENGTH(cInpVal) THEN " MATCHES " 
                            ELSE " BEGINS ") +
                           "'" + (IF INDEX(cInpVal,"*") = LENGTH(cInpVal) THEN 
                                    RIGHT-TRIM(cInpVal,"*")
                                  ELSE cInpVal) + "'"
                           ,",",CHR(1)).
            IF INDEX(cPreScanCrit,"*") > 0 THEN
              cSearchDef = "filter".

            cOperator      = IF INDEX(cInpVal,"*") > 0 
                                AND INDEX(cInpVal,"*") NE LENGTH(cInpVal) THEN " MATCHES " ELSE " BEGINS ".
            setAttribute(hTmpObject,"querywhere",cWhereAnd + cDbFld + cOperator + 
                                                             "'" + (IF INDEX(cInpVal,"*") = LENGTH(cInpVal) THEN 
                                                                      RIGHT-TRIM(hCobj:INPUT-VALUE,"*")
                                                                    ELSE hCobj:INPUT-VALUE) + "'"
                                                           ).
          END.
        END.
        ELSE 
          setAttribute(hTmpObject,"querywhere","").
      END.
      ELSE DO:
        IF hCobj:INPUT-VALUE NE ? THEN
          setAttribute(hTmpObject,"querywhere",cWhereAnd + cDbFld + cOperator +
                                                           STRING(hCobj:INPUT-VALUE)
                                                           ).
        ELSE 
          setAttribute(hTmpObject,"querywhere","").
      END.
    END.
    WHEN "LOGICAL" THEN 
      setAttribute(hTmpObject,"querywhere",cWhereAnd + cDbFld + cOperator +
                                                       STRING(hCobj:INPUT-VALUE)
                                                       ).
    OTHERWISE DO:
      IF ttObject.hObject:DATA-TYPE BEGINS "date" THEN DO:
        IF hCobj:INPUT-VALUE = ? THEN
          setAttribute(hTmpObject,"querywhere","").
        ELSE 
          setAttribute(hTmpObject,"querywhere",cWhereAnd + cDbFld + cOperator +
                                                           ttObject.hObject:DATA-TYPE + "('" + hCobj:INPUT-VALUE + "')"
                                                          ).
      END.  
      ELSE IF hCobj:INPUT-VALUE = 0 THEN
        setAttribute(hTmpObject,"querywhere","").
      ELSE 
        setAttribute(hTmpObject,"querywhere",cWhereAnd + cDbFld + cOperator +
                                                         ttObject.hObject:DATA-TYPE + "('" + hCobj:INPUT-VALUE + "')"
                                                         ).
    END.
  END CASE.

  IF (getAttribute(hTmpObject,"uselocaldata") = "yes" OR cSearchDef = "goto") AND NOT
     (getAttribute(hTmpObject,"uselocaldata") = "yes" AND TRIM(cOperator) = "MATCHES")
     THEN DO:

    hTmpObject:QUERY:QUERY-PREPARE("PRESELECT EACH " + hTmpObject:QUERY:GET-BUFFER-HANDLE(1):NAME + " NO-LOCK "
                                 + (IF getAttribute(hTmpObject,"uselocaldata") = "yes" THEN
                                      getAttribute(hTmpObject,"baseQuery") + getAttribute(hTmpObject,"QueryFilter")
                                    ELSE "")
                                 + " BY " + getAttribute(hTmpObject,"localsort") 
                                 + (IF VALID-HANDLE(hRowCnt) THEN 
                                      (IF getAttribute(hTmpObject,"localsort") NE "" THEN " BY " ELSE "") + "RowCount" 
                                    ELSE "")
                                    ) NO-ERROR.
    IF ERROR-STATUS:GET-NUMBER(1) = 141 THEN
      hTmpObject:QUERY:QUERY-PREPARE("PRESELECT EACH " + hTmpObject:QUERY:GET-BUFFER-HANDLE(1):NAME + " NO-LOCK "
                                   + " BY SUBSTR('" + getAttribute(hTmpObject,"localsort") + "',1,50)" + (IF VALID-HANDLE(hRowCnt) THEN " BY RowCount" ELSE "")
                                     ) NO-ERROR.
    hTmpObject:QUERY:QUERY-OPEN().

    IF hTmpObject:QUERY:IS-OPEN AND getAttribute(hTmpObject,"querywhere") NE "" THEN DO:
      hTmpObject:QUERY:GET-FIRST().
      IF hTmpObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
        hTmpObject:SELECT-ROW(1).
        CREATE QUERY hTmpQry.
        hTmpQry:SET-BUFFERS(hTmpObject:QUERY:GET-BUFFER-HANDLE(1)).
        hTmpQry:QUERY-PREPARE("FOR EACH " + hTmpObject:QUERY:GET-BUFFER-HANDLE(1):NAME
                              + " NO-LOCK " 
                              + (IF getAttribute(hTmpObject,"uselocaldata") = "yes" THEN
                                   getAttribute(hTmpObject,"baseQuery") + getAttribute(hTmpObject,"QueryFilter")
                                 ELSE (IF TRIM(getAttribute(hTmpObject,"querywhere")) BEGINS "AND " THEN " WHERE TRUE " ELSE ""))
                              + REPLACE(getAttribute(hTmpObject,"querywhere"),cDbFld,cClientFld)
                              + " BY " + getAttribute(hTmpObject,"localsort") 
                              + (IF bDesc THEN " DESC" ELSE "")
                              + (IF VALID-HANDLE(hRowCnt) THEN 
                                  (IF getAttribute(hTmpObject,"localsort") NE "" THEN " BY " ELSE "") + "RowCount" 
                                 ELSE "")
                                 ) NO-ERROR.
        IF ERROR-STATUS:GET-NUMBER(1) = 141 THEN
          hTmpQry:QUERY-PREPARE("FOR EACH " + hTmpObject:QUERY:GET-BUFFER-HANDLE(1):NAME
                                + " NO-LOCK " 
                                + (IF getAttribute(hTmpObject,"uselocaldata") = "yes" THEN
                                     getAttribute(hTmpObject,"baseQuery") + getAttribute(hTmpObject,"QueryFilter")
                                   ELSE (IF TRIM(getAttribute(hTmpObject,"querywhere")) BEGINS "AND " THEN " WHERE TRUE " ELSE ""))
                                + REPLACE(getAttribute(hTmpObject,"querywhere"),cDbFld,cClientFld)
                                + " BY SUBSTR('" + getAttribute(hTmpObject,"localsort") + "',1,50)" 
                                + (IF bDesc THEN " DESC" ELSE "")
                                + (IF VALID-HANDLE(hRowCnt) THEN " BY RowCount" ELSE "")
                                  ) NO-ERROR.
        hTmpQry:QUERY-OPEN() NO-ERROR.
        IF NOT hTmpQry:IS-OPEN THEN
          hTmpQry:QUERY-OPEN() NO-ERROR.
  
        hTmpQry:GET-FIRST() NO-ERROR.
        bReposOk = hTmpObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL.
        DELETE OBJECT hTmpQry.
        IF bReposOk 
           AND getAttribute(hTmpObject,"uselocaldata") NE "yes"
           AND hCobj:DATA-TYPE NE "CHARACTER" 
           AND STRING(hTmpObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(cClientFld):BUFFER-VALUE) NE hCobj:INPUT-VALUE THEN DO:
          IF CAN-DO(getAttribute(hTmpObject,"allcalcfields"),cClientFld) THEN DO:
            setAttribute(hTmpObject,"calcfieldwhere",cClientFld + "¤=¤" + hCobj:INPUT-VALUE).
            setAttribute(hTmpObject,"querywhere","").
            cTemp = "".
          END.
          ELSE DO:
            cTemp = getAttribute(hTmpObject,"querywhere").
            setAttribute(hTmpObject,"querywhere",REPLACE(getAttribute(hTmpObject,"querywhere"),cOperator," = ")).
          END.
          bReposOk = NOT DYNAMIC-FUNCTION("ProcessQuery",hTmpObject,"jbserv_gettemptablejoin.p","").
          setAttribute(hTmpObject,"querywhere",cTemp).
        END.
      END.
    END.
  END.

  IF bReposOk THEN DO:
    hTmpObject:SET-REPOSITIONED-ROW(hTmpObject:DOWN,"conditional").
    hTmpObject:QUERY:REPOSITION-TO-ROWID(hTmpObject:QUERY:GET-BUFFER-HANDLE(1):ROWID) NO-ERROR.
    IF hTmpObject:MULTIPLE THEN DO:
      IF hTmpObject:NUM-SELECTED-ROWS > 0 THEN
        hTmpObject:DESELECT-ROWS().
      hTmpObject:SELECT-ROW(hTmpObject:FOCUSED-ROW).
    END.
  END.
  IF NOT bReposOk OR getAttribute(hTmpObject,"keepsearchvalue") = "yes" THEN DO:
    IF CAN-DO(DYNAMIC-FUNCTION("getAttribute",hTmpObject,"allCalcFields"),cClientFld) AND getAttribute(hTmpObject,"uselocaldata") NE "yes" THEN DO:
      IF getAttribute(hTmpObject,"keepsearchvalue") = "yes" THEN DO:
        cCalcFlfFilter = DYNAMIC-FUNCTION("getAttribute",hTmpObject,"calcFieldFilter").
        cCalcFlfFilter = cCalcFlfFilter + (IF cCalcFlfFilter NE "" THEN "|" ELSE "")
                         + cClientFld + "¤"
                         + cOperator + "¤"
                         + hCobj:INPUT-VALUE.
        DYNAMIC-FUNCTION("setAttribute",hTmpObject,"queryWhere","").
        DYNAMIC-FUNCTION("setAttribute",hTmpObject,"calcFieldFilter",cCalcFlfFilter).
      END.
    END.
    ELSE IF cFieldBuffer NE getAttribute(hTmpObject,"basetable") AND getAttribute(hTmpObject,"orgbuffersandfields") = "" THEN DO:  /* if orgbuffersandfields is set (by override), the buffer sequence has changed */
      IF getAttribute(hTmpObject,"querywhere") NE "" THEN DO:
        IF cPreScanCrit = "" THEN cPreScanCrit = getAttribute(hTmpObject,"querywhere").
        IF getAttribute(hTmpObject,"prescanquery" + cFieldBuffer) NE "" THEN DO:
          ASSIGN cPreScanQuery       = getAttribute(hTmpObject,"prescanquery" + cFieldBuffer)
                 cPreScanBaseTbl   = ENTRY(NUM-ENTRIES(cPreScanQuery),cPreScanQuery)
                 cPreScanQuery       = cFieldBuffer + " " + cPreScanCrit + "," + cPrescanQuery
                 cMainQryCand = cPreScanQuery
                 cBaseQuery          = getAttribute(hTmpObject,"basequery")
                 cQueryFilter        = getAttribute(hTmpObject,"queryfilter")
                 cPreScanQuery       = cPrescanQuery + " " + (IF cBaseQuery BEGINS "WHERE " AND cPreScanBaseTbl MATCHES "* WHERE *" THEN 
                                                                "AND " + SUBSTR(cBaseQuery,7)
                                                              ELSE IF cBaseQuery MATCHES "* where *" AND cPreScanBaseTbl MATCHES "* WHERE *" THEN 
                                                                REPLACE(cBaseQuery," WHERE "," AND ")
                                                              ELSE cBaseQuery)  
                 .
          setAttribute(hTmpObject,"prescanquerywhere",cPrescanQuery).
        END.
        ELSE DO:
          ASSIGN cPrescanQuery       = cFieldBuffer + " " + cPreScanCrit + ",EACH " + getAttribute(hTmpObject,"basetable") + " NO-LOCK OF " + cFieldBuffer
                 cMainQryCand = cPrescanQuery.        
          setAttribute(hTmpObject,"prescanquerywhere",cPrescanQuery + " " + getAttribute(hTmpObject,"basequery") + getAttribute(hTmpObject,"queryfilter")).
        END.
        setAttribute(hTmpObject,"querywhere","").
      END.

      IF getAttribute(hTmpObject,"keepsearchvalue") = "yes" THEN DO:
        setAttribute(hTmpObject,"prescanmainquerycandidate_query",cMainQryCand).
        SwapPrescanToMainBuffer(hTmpObject,"").
      END.
      ELSE SwapPrescanToMainBuffer(hTmpObject,cMainQryCand).
    END.
    /* Query buffer sequence was altered and this criteria doesn't apply: */
    ELSE DO:
      IF getAttribute(hTmpObject,"orgbuffersandfields") NE "" THEN 
        setAttribute(hTmpObject,"querywhere","").
      setAttribute(hTmpObject,"prescanmainquerycandidate_query","").
      SwapPrescanToMainBuffer(hTmpObject,"").
    END.

    IF NOT bReposOk THEN DO:
      IF getAttribute(hTmpObject,"getrecordcount") = "yes" THEN
        DYNAMIC-FUNCTION("setQueryStatFields","rowcount").

      IF CAN-DO(getAttribute(hTmpObject,"allcalcfields"),cClientFld) AND NOT getAttribute(hTmpObject,"uselocaldata") = "yes" THEN DO:
        setAttribute(hTmpObject,"calcfieldwhere",cClientFld + "¤" + TRIM(cOperator) + "¤" + hCobj:INPUT-VALUE).
        setAttribute(hTmpObject,"querywhere","").
      END.
  
      iMaxCount = DYNAMIC-FUNCTION("FillBrowse",hTmpObject,
                          INT(getAttribute(hTmpObject,"rowstobatch")),
                          INT(getAttribute(hTmpObject,"querystart")),"","","",
                          bDesc).
      
      setAttribute(hTmpObject,"querystart",STRING(iMaxCount)).
      setAttribute(hTmpObject,"calcfieldwhere","").
    
      IF DYNAMIC-FUNCTION("getTransactionMessage") NE "" 
         AND NOT getAttribute(hTmpObject,"uselocaldata") = "yes" THEN 
        DoMessage (-1,0,DYNAMIC-FUNCTION("getTransactionMessage"),
                   IF DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*warning*" THEN "Warning" 
                   ELSE "JukeBox, Query error:"
                   ,"").
  
      IF iMaxCount = 0 THEN
        hTmpObject:QUERY:QUERY-CLOSE().
    END.
  END.

  APPLY "value-changed":U TO hTmpObject.

  IF (cSearchDef = "goto" OR
      DYNAMIC-FUNCTION("getAttribute",hMyTmpQryObj,"lastrowid") NE "") AND
      getAttribute(hMyTmpQryObj,"keepsearchvalue") NE "yes" THEN 
    setAttribute(hMyTmpQryObj,"querywhere","").

  IF hMyTmpFldObj:DATA-TYPE NE "LOGICAL" AND getAttribute(hMyTmpQryObj,"keepsearchvalue") NE "yes" THEN
    hMyTmpFldObj:SCREEN-VALUE = "".

  IF getAttribute(hMyTmpQryObj,"getrecordcount") = "yes" THEN
    APPLY "entry" TO hMyTmpQryObj.
  ELSE IF VALID-HANDLE(hTmpObject) THEN APPLY "entry" TO hTmpObject.

  hTmpObject = ?.
  setCurrentObject(hCobj).
  setAttribute(SESSION,"userkeepswindowlocked","").
  DoLockWindow(?).
END.
ELSE DO:
  setAttribute(hTmpObject,"querywhere","").
  setAttribute(hTmpObject,"prescanquerywhere","").
  hTmpObject = ?.
END.

RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TabFromBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabFromBrowse Procedure 
PROCEDURE TabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:    Apply tab from a dynamically created browse 
  Parameters:  <none>
  Notes:      Current object is the browse 
------------------------------------------------------------------------------*/
DEF VAR cTabChainOverlays AS CHAR   NO-UNDO.
DEF VAR hWidget           AS HANDLE NO-UNDO.
DEF VAR cWidgets          AS CHAR   NO-UNDO.

hWidgetEnter = ?.

IF getAttribute(ttObject.hObject,"nextTabItem") NE "" THEN 
  hWidgetEnter = WIDGET-HANDLE(getAttribute(ttObject.hObject,"nextTabItem")) NO-ERROR.

IF NOT VALID-HANDLE(hWidgetEnter) AND getAttribute(ttObject.hObject,"tabchainoverlays") NE "" 
   AND getAttribute(ttObject.hObject,"currentrowid") NE getAttribute(ttObject.hObject,"lastrowid") THEN DO:
  cTabChainOverlays = getAttribute(ttObject.hObject,"tabchainoverlays").
  DO ix = 1 TO NUM-ENTRIES(cTabChainOverlays):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cTabChainOverlays)) NO-ERROR.
    IF VALID-HANDLE(hWidget) AND hWidget:VISIBLE AND hWidget:TYPE NE "toggle-box" THEN DO:
      hWidgetEnter = hWidget.
      LEAVE.
    END.
  END.
END.

IF NOT VALID-HANDLE(hWidgetEnter) THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType   = "fieldmap"
       NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN DO:
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hToObject = ttObject.hObject
           AND bttObjectLink.cLinkType = "onetoone"
         NO-ERROR.
    IF AVAIL bttObjectLink THEN
      FIND FIRST ttObjectLink
           WHERE ttObjectLink.hFromObject = bttObjectLink.hFromObject
             AND ttObjectLink.cLinkType   = "fieldmap"
           NO-ERROR.
  END.
  IF AVAIL ttObjectLink THEN DO:
    cWidgets = TRIM(getAttribute(ttObjectLink.hToObject,"ScreenUpdateWidgets") + "," + 
                             (IF getAttribute(ttObjectLink.hToObject,"ExtraUpdateWidgets") NE "" THEN
                                getAttribute(ttObjectLink.hToObject,"ExtraUpdateWidgets") + ","
                              ELSE "") + 
                             getAttribute(ttObjectLink.hToObject,"ScreenDisplayWidgets"),",").
    IF cWidgets NE "" THEN
      hWidgetEnter = WIDGET-HANDLE(ENTRY(1,cWidgets)) NO-ERROR.
  END.
END.

IF NOT VALID-HANDLE(hWidgetEnter) THEN DO ix = 1 TO 10:
  hWidget = ttObject.hDesignObject:NEXT-SIBLING NO-ERROR.
  IF VALID-HANDLE(hWidget) AND CAN-DO(cInputWidgetTypes,hWidget:TYPE) THEN DO:
    IF NOT CAN-QUERY(hWidget,"tab-stop") OR (CAN-QUERY(hWidget,"tab-stop") AND NOT hWidget:TAB-STOP) THEN NEXT.
    hWidgetEnter = hWidget.
    LEAVE.
  END.
END.
IF NOT VALID-HANDLE(hWidgetEnter) THEN
  hWidgetEnter = ttObject.hObject.

bMyReturnNoApply = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TabFromSortSearch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabFromSortSearch Procedure 
PROCEDURE TabFromSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
IF AVAIL ttObjectLink THEN
  hWidgetEnter = ttObjectLink.hToObject.
         
bMyReturnNoApply = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TodaysDateKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TodaysDateKey Procedure 
PROCEDURE TodaysDateKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hCurrWidget) AND hCurrWidget:TYPE = "fill-in" AND hCurrWidget:DATA-TYPE = "date" THEN DO:
  IF DATE(hCurrWidget:SCREEN-VALUE) = ? THEN
    hCurrWidget:SCREEN-VALUE = STRING(TODAY).
  IF LAST-EVENT:FUNCTION = "+" THEN
    hCurrWidget:SCREEN-VALUE = STRING(DATE(hCurrWidget:SCREEN-VALUE) + 1).
  ELSE IF LAST-EVENT:FUNCTION = "-" THEN
    hCurrWidget:SCREEN-VALUE = STRING(DATE(hCurrWidget:SCREEN-VALUE) - 1).
  ELSE
    hCurrWidget:SCREEN-VALUE = STRING(TODAY).

  RUN AnyPrintableKey.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UndoRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRecord Procedure 
PROCEDURE UndoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Current object is the toolbar. Must navigate to the browse/query object
------------------------------------------------------------------------------*/
DEF VAR hCurrObject  AS HANDLE NO-UNDO.
DEF VAR hQueryObject AS HANDLE NO-UNDO.
DEF VAR hFieldMap    AS HANDLE NO-UNDO.
DEF VAR hWidget      AS HANDLE NO-UNDO.

DEF BUFFER bttObjectLink FOR ttObjectLink.

hCurrObject = ttObject.hObject.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject 
       AND ttObjectLink.cLinkType   = "browse" 
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN 
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject 
         AND ttObjectLink.cLinkType   = "query" 
       NO-ERROR.

IF AVAIL ttObjectLink THEN
  FIND FIRST ttObject 
       WHERE ttObject.hObject = ttObjectLink.hToObject
       NO-ERROR.

IF NOT AVAIL ttObject THEN RETURN ERROR.
ELSE DO:
  hQueryObject = ttObject.hObject.

  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType = "fieldMap"
       NO-ERROR.
  IF AVAIL ttObjectLink THEN DO:
    setAttribute (ttObjectLink.hToObject,"checkmodified","no").

    hFieldMap = ttObjectLink.hToObject.

    IF getAttribute(hCurrObject,"commitstate") = "on" THEN DO:
      IF NOT ttObjectLink.hToObject:AVAIL THEN DO:
        setAttribute(hCurrObject,"commitstate","off").
        setAttribute(ttObjectLink.hToObject,"uselocaldata","").
        FIND FIRST bttObjectLink
             WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
               AND bttObjectLink.cLinkType = "browse"
             NO-ERROR.
        IF AVAIL bttObjectLink THEN DO:
          setAttribute(bttObjectLink.hToObject,"uselocaldata","").
          setCurrentObject(bttObjectLink.hToObject).
          RUN OpenQuery.
        END.
      END.
    END.
    IF getAttribute(hFieldMap,"fieldMapIsBrowse") = "yes" THEN DO:  
      IF getAttribute(hQueryObject,"useLocalData") = "yes" THEN DO:  
        IF DYNAMIC-FUNCTION("getToolbarState",hCurrObject) = "new" THEN
          hFieldMap:BUFFER-DELETE() NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          hQueryObject:REFRESH().
      END.    
      ELSE
        DYNAMIC-FUNCTION("RefreshRowids",hQueryObject,hFieldMap::RowIdent1).
        
      setAttribute(hQueryObject,"enableupdate","").
      setAttribute(hQueryObject,"useLocalData","").
    END.   
  END.

  IF hQueryObject:TYPE = "browse" THEN DO:
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hToObject = hQueryObject
           AND ttObjectLink.cLinkType = "onetoone"
         NO-ERROR.
    IF AVAIL ttObjectLink THEN DO:
      FIND FIRST bttObjectLink
           WHERE bttObjectLink.hFromObject = ttObjectLink.hFromObject
             AND bttObjectLink.cLinkType = "fieldMap"
           NO-ERROR.
      IF AVAIL bttObjectLink THEN
        setAttribute(bttObjectLink.hToObject,"checkmodified","no").
    END.

    setAttribute(hQueryObject,"checkmodified","no").

    ASSIGN bSetCurrHandles = FALSE
           hCurrWidget     = hQueryObject
           .
    RUN DoProcessEvent(0,"value-changed").
  END.
  ELSE DO:
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = hQueryObject
           AND ttObjectLink.cLinkType   = "fieldMap"
         NO-ERROR.
    IF AVAIL ttObjectLink THEN 
      setAttribute(ttObjectLink.hToObject,"checkmodified","no").

    setAttribute(hQueryObject,"checkmodified","no").
    ASSIGN bSetCurrHandles = FALSE
           hCurrWidget     = hQueryObject
           .
    RUN DoProcessEvent(0,"value-changed").
  END.
END.

IF VALID-HANDLE(hFieldMap) THEN DO:
  FOR EACH bttObjectLink 
      WHERE bttObjectLink.hFromObject = hFieldMap
        AND bttObjectLink.cLinkType = "dotNetDisplay"
      :
    /* Target: JBoxWrapWindowInForm */
/*    PUBLISH "dotNetUndo" (hFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo). */
    PUBLISH "dotNetMethod" ("dotNetUndo",hFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,?).
  END.

  hWidget = WIDGET-HANDLE(ENTRY(1,getAttribute(hFieldMap,"screenUpdateWidgets"))) NO-ERROR.
  IF VALID-HANDLE(hWidget) AND hWidget:SENSITIVE THEN APPLY "entry" TO hWidget.
  ELSE IF hQueryObject:TYPE = "browse" THEN
    APPLY "entry" TO hQueryObject.
END.

SetCurrentObject(hCurrObject).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValChngBrowseDropDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseDropDown Procedure 
PROCEDURE ValChngBrowseDropDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the combo. Must navigate to the browser
------------------------------------------------------------------------------*/
DEF VAR hBuffer              AS HANDLE NO-UNDO.
DEF VAR cQueryWhere          AS CHAR NO-UNDO. 
DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR hCombo               AS HANDLE NO-UNDO.
DEF VAR cUpdateValProc       AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateFields   AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateValues   AS CHAR   NO-UNDO.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
hCombo = ttObject.hObject.
IF NOT AVAIL ttObjectLink THEN DO:
  MESSAGE "No link to browse from combo. Programmers mistake"
          VIEW-AS ALERT-BOX INFORMATION.
  RETURN.
END.
ELSE hBrowse = ttObjectLink.hToObject.

hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"MySaveBrowseDropDown") THEN
  RUN MySaveBrowseDropDown IN hCurrSourceProc (hCombo,hBuffer,OUTPUT bOk).
ELSE DO:
  bOK = TRUE.
  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraValChngBrowseDropDown") THEN
    RUN ExtraValChngBrowseDropDown IN hCurrSourceProc (hCombo,hBuffer,OUTPUT bOk).

  IF bOk THEN DO:
    IF NOT getAttribute(hBrowse,"uselocaldata") = "yes" AND NOT getAttribute(hBrowse,"editWithFieldMap") = "yes" THEN DO:
      DYNAMIC-FUNCTION("setPostUpdProc",IF getAttribute(hBuffer,"postUpdateProc") NE "" THEN
                                          getAttribute(hBuffer,"postUpdateProc")
                                        ELSE getAttribute(hBrowse,"postUpdateProc")).
      ASSIGN cUpdateValProc = IF getAttribute(hCombo,"customUpdateValProc") NE "" THEN 
                                getAttribute(hCombo,"customUpdateValProc")
                              ELSE IF getAttribute(hBuffer,"customUpdateValProc") NE "" THEN 
                                getAttribute(hBuffer,"customUpdateValProc")
                              ELSE getAttribute(hBrowse,"customUpdateValProc")
             cExtraUpdateFields = IF getAttribute(hCombo,"bufferextrafields") NE "" THEN 
                                getAttribute(hCombo,"bufferextrafields")
                              ELSE IF getAttribute(hBuffer,"bufferextrafields") NE "" THEN 
                                getAttribute(hBuffer,"bufferextrafields")
                              ELSE getAttribute(hBrowse,"bufferextrafields")
             cExtraUpdateValues = IF getAttribute(hCombo,"bufferextravalues") NE "" THEN 
                                getAttribute(hCombo,"bufferextravalues")
                              ELSE IF getAttribute(hBuffer,"bufferextravalues") NE "" THEN 
                                getAttribute(hBuffer,"bufferextravalues")
                              ELSE getAttribute(hBrowse,"bufferextravalues")
             cExtraUpdateValues = REPLACE(cExtraUpdateValues,"|",CHR(1))
                              .
      IF DYNAMIC-FUNCTION("getIsAttributeSet",hBrowse,"serverTransInputParam") THEN
        DYNAMIC-FUNCTION("setServerTransInputParam",DYNAMIC-FUNCTION("getAttribute",hBrowse,"serverTransInputParam")).
      bOK = DYNAMIC-FUNCTION("DoUpdate",hBuffer:NAME,
                  cUpdateValProc,
                  "",
                  hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                  getAttribute(hCombo,"buffercolumn") + (IF cExtraUpdateFields NE "" THEN "," + cExtraUpdateFields ELSE ""),
                  (IF hCombo:SCREEN-VALUE = ? THEN "" ELSE hCombo:SCREEN-VALUE) + (IF cExtraUpdateValues NE "" THEN CHR(1) + cExtraUpdateValues ELSE ""),
                  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraValChngBrowseDropDown") THEN FALSE ELSE TRUE).
                  
      setAttribute(hBrowse,"serverTransReturnParam",DYNAMIC-FUNCTION("getServerTransReturnParam")).
      setAttribute(hBrowse,"serverTransReturnMessage",DYNAMIC-FUNCTION("getTransactionMessage")).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION("AssignStringValue",hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(getAttribute(hCombo,"buffercolumn")),
                       IF hCombo:SCREEN-VALUE = ? THEN "" ELSE hCombo:SCREEN-VALUE).
      RETURN.
    END.

    IF NOT bOk THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    END.
  END.
END.

IF bOK THEN DO:

  IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW) NO-ERROR.
  IF hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN DO:
    IF getAttribute(hCombo,"refreshrow") = "yes" THEN DO:
      RefreshRowids (hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hBrowse).
      IF CAN-FIND(FIRST ttObjectLink
                  WHERE ttObjectLink.hToObject = hBrowse
                    AND ttObjectLink.cLinkType = "parent"
                    AND ttObjectLink.cLinkInfo MATCHES "*" + getAttribute(hBrowse,"browsecolumn") + "*") THEN
        APPLY "value-changed" TO hBrowse.
    END.
    ELSE 
      hBuffer:BUFFER-FIELD(getAttribute(hCombo,"browsecolumn")):BUFFER-VALUE = getDropDownLabel(hCombo,"|").
  END.
END.

setCurrentObject(hCombo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValChngBrowseToggle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValChngBrowseToggle Procedure 
PROCEDURE ValChngBrowseToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the toggle. Must navigate to the browser
------------------------------------------------------------------------------*/
DEF VAR hBuffer              AS HANDLE NO-UNDO.
DEF VAR cQueryWhere          AS CHAR NO-UNDO. 
DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR cJoin                AS CHAR NO-UNDO.
DEF VAR cJoinBuffers         AS CHAR NO-UNDO.
DEF VAR hToggle              AS HANDLE NO-UNDO.
DEF VAR cBuffersAndFields    AS CHAR NO-UNDO.
DEF VAR cUpdateValProc       AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateFields   AS CHAR   NO-UNDO.
DEF VAR cExtraUpdateValues   AS CHAR   NO-UNDO.
DEF VAR iBGcolNum            AS INT    NO-UNDO INIT 15.

IF ttObject.hObject:HIDDEN THEN RETURN.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "browse"
     NO-ERROR.
hToggle = ttObject.hObject.
IF NOT AVAIL ttObjectLink THEN DO:
  MESSAGE "No link to browse from combo. Programmers mistake"
          VIEW-AS ALERT-BOX INFORMATION.
  RETURN.
END.
ELSE hBrowse = ttObjectLink.hToObject.

IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
 hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW) NO-ERROR.

hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"MySaveBrowseToggle") THEN
  RUN MySaveBrowseToggle IN hCurrSourceProc (hToggle,hBuffer,OUTPUT bOk).
ELSE DO:
  bOK = TRUE.
  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraValChngBrowseToggle") THEN
    RUN ExtraValChngBrowseToggle IN hCurrSourceProc (hToggle,hBuffer,OUTPUT bOk).

  IF bOk THEN DO:
    IF NOT getAttribute(hBrowse,"uselocaldata") = "yes" THEN DO:
      DYNAMIC-FUNCTION("setPostUpdProc",IF getAttribute(hBuffer,"postUpdateProc") NE "" THEN
                                          getAttribute(hBuffer,"postUpdateProc")
                                        ELSE getAttribute(hBrowse,"postUpdateProc")).
      ASSIGN cUpdateValProc = IF getAttribute(hToggle,"customUpdateValProc") NE "" THEN 
                                getAttribute(hToggle,"customUpdateValProc")
                              ELSE IF getAttribute(hBuffer,"customUpdateValProc") NE "" THEN 
                                getAttribute(hBuffer,"customUpdateValProc")
                              ELSE getAttribute(hBrowse,"customUpdateValProc")
             cExtraUpdateFields = IF getAttribute(hToggle,"bufferextrafields") NE "" THEN 
                                getAttribute(hToggle,"bufferextrafields")
                              ELSE IF getAttribute(hBuffer,"bufferextrafields") NE "" THEN 
                                getAttribute(hBuffer,"bufferextrafields")
                              ELSE getAttribute(hBrowse,"bufferextrafields")
             cExtraUpdateValues = IF getAttribute(hToggle,"bufferextravalues") NE "" THEN 
                                getAttribute(hToggle,"bufferextravalues")
                              ELSE IF getAttribute(hBuffer,"bufferextravalues") NE "" THEN 
                                getAttribute(hBuffer,"bufferextravalues")
                              ELSE getAttribute(hBrowse,"bufferextravalues")
             cExtraUpdateValues = REPLACE(cExtraUpdateValues,"|",CHR(1))
                              .
      IF DYNAMIC-FUNCTION("getIsAttributeSet",hBrowse,"serverTransInputParam") THEN
        DYNAMIC-FUNCTION("setServerTransInputParam",DYNAMIC-FUNCTION("getAttribute",hBrowse,"serverTransInputParam")).
      bOK = DYNAMIC-FUNCTION("DoUpdate",hBuffer:NAME,
                   cUpdateValProc,
                   "",
                   hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                   getAttribute(hToggle,"buffercolumn") + (IF cExtraUpdateFields NE "" THEN "," + cExtraUpdateFields ELSE ""),
                   hToggle:INPUT-VALUE + (IF cExtraUpdateValues NE "" THEN CHR(1) + cExtraUpdateValues ELSE ""),
                   IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraValChngBrowseToggle") THEN FALSE ELSE TRUE).

      setAttribute(hBrowse,"serverTransReturnParam",DYNAMIC-FUNCTION("getServerTransReturnParam")).
      setAttribute(hBrowse,"serverTransReturnMessage",DYNAMIC-FUNCTION("getTransactionMessage")).
    END.
    ELSE 
      hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(getAttribute(hToggle,"buffercolumn")):BUFFER-VALUE = hToggle:INPUT-VALUE NO-ERROR.
  END.
END.

IF bOK THEN DO:
  IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW) NO-ERROR.
  IF hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN DO:
    IF getAttribute(hToggle,"refreshrow") = "yes" THEN DO:
      RefreshRowids (hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      DYNAMIC-FUNCTION("DispBrwOverlayWidgets",hBrowse).
    END.
    ELSE 
      hBuffer:BUFFER-FIELD(getAttribute(hToggle,"browsecolumn")):BUFFER-VALUE = hToggle:INPUT-VALUE.
  END.
END.
ELSE RETURN.

IF getAttribute(hBrowse,"overlaybgcolnum") NE "" THEN
  iBGcolNum = INT(getAttribute(hBrowse,"overlaybgcolnum")).
IF getAttribute(hToggle,"overlaybgcolnum") NE "" THEN
  iBGcolNum = INT(getAttribute(hToggle,"overlaybgcolnum")).

hToggle:BGCOLOR = iBGcolNum.

setCurrentObject(hToggle).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValueChangedWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedWidget Procedure 
PROCEDURE ValueChangedWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Current object is the fieldMap object
------------------------------------------------------------------------------*/
DEF VAR cAvailWidgets AS CHAR   NO-UNDO.
DEF VAR hWidget       AS HANDLE NO-UNDO.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType = "toolbar" NO-ERROR.

IF AVAIL ttObjectLink AND NOT CAN-DO("new,modified",DYNAMIC-FUNCTION("getToolBarState",ttObjectLink.hToObject)) THEN DO:
  cAvailWidgets = getAttribute(ttObject.hObject,"recordavailwidgets").
  DYNAMIC-FUNCTION("setToolbar",ttObjectLink.hToObject,"modified").

  DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
   hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
   IF VALID-HANDLE(hWidget) THEN
     hWidget:SENSITIVE = FALSE.
  END.
END.

IF NOT VALID-HANDLE(hCurrSourceProc) THEN hCurrSourceProc = SOURCE-PROCEDURE.
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ValueChangedField") THEN 
  RUN ValueChangedField IN hCurrSourceProc (hCurrWidget:NAME).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AddColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddColor Procedure 
FUNCTION AddColor RETURNS LOGICAL
  ( INPUT icName  AS CHAR,
    INPUT iiRed   AS INT,
    INPUT iiGreen AS INT,
    INPUT iiBlue  AS INT) :
/*------------------------------------------------------------------------------
  Purpose: Make dynamic colors available as session attributes 
    Notes: Name "RowShade" is reserved for shading browse rows
------------------------------------------------------------------------------*/
DEF VAR iColNum AS INT NO-UNDO.

iColNum = DYNAMIC-FUNCTION("getColorNum",RGB-VALUE(iiRed,iiGreen,iiBlue)) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
  IF NOT PROVERSION BEGINS "9" THEN
    MESSAGE PROGRAM-NAME(1) SKIP
            "Error in getColorNum function"
            VIEW-AS ALERT-BOX ERROR.
  RETURN NO.
END.

DYNAMIC-FUNCTION("setAttribute",SESSION,"Color_" + icName,STRING(iColNum)).

IF icName = "RowShade" THEN iColorRowShade = iColNum.
IF icName = "EditBgColor" THEN iEditBgColor = iColNum.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddFont Procedure 
FUNCTION AddFont RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApplyEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ApplyEvent Procedure 
FUNCTION ApplyEvent RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE,
    INPUT icAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Apply a named event for an object 
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttEvent WHERE ttEvent.hObject = ihObject AND ttEvent.cAction = icAction NO-LOCK NO-ERROR.
IF AVAIL ttEvent THEN DO:
  ASSIGN bSetCurrHandles = FALSE
         hCurrWidget     = ttEvent.hWidget.
  RUN DoProcessEvent(0,ttEvent.cAction).
  RETURN TRUE.
END.
ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BrwOrQryToFMapTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BrwOrQryToFMapTranslation Procedure 
FUNCTION BrwOrQryToFMapTranslation RETURNS LOGICAL
  ( INPUT ihBrowseOrQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cScreenWidgets  AS CHAR   NO-UNDO.
DEF VAR cScreenFields   AS CHAR   NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hWidget         AS HANDLE NO-UNDO.
DEF VAR iFieldNum       AS INT    NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.

FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ihBrowseOrQuery
       AND ttObjectLink.cLinkType   = "fieldMap"
     NO-ERROR.
IF AVAIL ttObjectLink THEN DO:
  ASSIGN cScreenWidgets = TRIM(
                            getAttribute(ttObjectLink.hToObject,"ScreenDisplayWidgets") + ","
                          + getAttribute(ttObjectLink.hToObject,"ScreenUpdateWidgets")
                            ,",")
         cScreenFields  = TRIM(
                            getAttribute(ttObjectLink.hToObject,"ScreenDisplayFields") + ","
                          + getAttribute(ttObjectLink.hToObject,"ScreenUpdateFields")
                            ,",")
                            .
  IF ihBrowseOrQuery:TYPE = "browse" THEN
    DO ix = 1 TO ihBrowseOrQuery:NUM-COLUMNS:
      iFieldNum = LOOKUP(ihBrowseOrQuery:GET-BROWSE-COLUMN(ix):NAME,cScreenFields).
      IF iFieldNum = 0 AND ihBrowseOrQuery:GET-BROWSE-COLUMN(ix):NAME MATCHES "*]" THEN
        iFieldNum = LOOKUP(REPLACE(REPLACE(ihBrowseOrQuery:GET-BROWSE-COLUMN(ix):NAME,"[","_"),"]",""),cScreenFields).
      IF iFieldNum > 0 THEN DO:
        hWidget = WIDGET-HANDLE(ENTRY(iFieldNum,cScreenWidgets)) NO-ERROR.
        IF VALID-HANDLE(hWidget) THEN
          hWidget:LABEL = ihBrowseOrQuery:GET-BROWSE-COLUMN(ix):LABEL.
      END.
    END.
  ELSE DO:
    hBuffer = ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1).
    DO ix = 1 TO hBuffer:NUM-FIELDS:
      iFieldNum = LOOKUP(hBuffer:BUFFER-FIELD(ix):NAME,cScreenFields).
      IF iFieldNum = 0 AND hBuffer:BUFFER-FIELD(ix):NAME MATCHES "*]" THEN
        iFieldNum = LOOKUP(REPLACE(REPLACE(hBuffer:BUFFER-FIELD(ix):NAME,"[","_"),"]",""),cScreenFields).
      IF iFieldNum > 0 THEN DO:
        hWidget = WIDGET-HANDLE(ENTRY(ix,cScreenWidgets)) NO-ERROR.
        IF VALID-HANDLE(hWidget) THEN
          hWidget:LABEL = hBuffer:BUFFER-FIELD(ix):LABEL.
      END.
    END.
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChangePrimarySearchBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ChangePrimarySearchBuffer Procedure 
FUNCTION ChangePrimarySearchBuffer RETURNS CHARACTER
  ( INPUT ihQueryObject    AS HANDLE,
    INPUT icPrimaryBuffer  AS CHAR,
    INPUT icPrimaryQuery   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Swap buffer sequence for query 
    Notes: Alternative method to using prescan queries. Well suited if a resultset from prescan could be very large
------------------------------------------------------------------------------*/
DEF VAR cBufferList        AS CHAR NO-UNDO.
DEF VAR cPrimaryBuffers    AS CHAR NO-UNDO.
DEF VAR cCurrBuffAndFlds   AS CHAR NO-UNDO.
DEF VAR cNewBuffAndFlds    AS CHAR NO-UNDO.
DEF VAR cNewQueryJoin      AS CHAR NO-UNDO.
DEF VAR cNewBufferList     AS CHAR NO-UNDO.
DEF VAR cBaseQuery         AS CHAR NO-UNDO.
DEF VAR cQueryFilter       AS CHAR NO-UNDO.
DEF VAR cQueryWhere        AS CHAR NO-UNDO.

ASSIGN cBufferList     = getAttribute(ihQueryObject,"bufferlist")
       cPrimaryBuffers = TRIM(icPrimaryBuffer + "," + getQueryBufferList(icPrimaryQuery),",").

IF cPrimaryBuffers NE "" AND ENTRY(1,cBufferList) NE ENTRY(1,cPrimaryBuffers) THEN DO:
  IF getAttribute(ihQueryObject,"orgbuffersandfields") = "" THEN DO:
    setAttribute(ihQueryObject,"orgbuffersandfields",getAttribute(ihQueryObject,"buffersandfields")).
    setAttribute(ihQueryObject,"orgqueryjoin",getAttribute(ihQueryObject,"queryjoin")).
    setAttribute(ihQueryObject,"orgbasequery",getAttribute(ihQueryObject,"basequery")).
    cCurrBuffAndFlds = getAttribute(ihQueryObject,"buffersandfields").
  END.
  ELSE
    cCurrBuffAndFlds = getAttribute(ihQueryObject,"orgbuffersandfields").

  DO ix = 1 TO NUM-ENTRIES(cPrimaryBuffers):
    IF LOOKUP(ENTRY(ix,cPrimaryBuffers),cBufferList) > 0 THEN
      cNewBuffAndFlds = cNewBuffAndFlds + ENTRY(LOOKUP(ENTRY(ix,cPrimaryBuffers),cBufferList),cCurrBuffAndFlds) + ",".
    ELSE
      cNewBuffAndFlds = cNewBuffAndFlds + ENTRY(ix,cPrimaryBuffers) + ",".
  END.
  cNewBufferList = cPrimaryBuffers.

  DO ix = 1 TO NUM-ENTRIES(cCurrBuffAndFlds):
    IF NOT CAN-DO(cPrimaryBuffers,ENTRY(ix,cBufferList)) THEN DO:
      ASSIGN cNewBuffAndFlds = cNewBuffAndFlds + ENTRY(ix,cCurrBuffAndFlds) + ","
             cNewQueryJoin   = cNewQueryJoin + getAttribute(ihQueryObject,"bufferjoin" + ENTRY(ix,cBufferList))
             cNewBufferList  = cNewBufferList + "," + ENTRY(ix,cBufferList).
    END.
    IF ENTRY(ix,cBufferList) = getAttribute(ihQueryObject,"basetable") THEN 
      ASSIGN cBaseQuery    = getAttribute(ihQueryObject,"orgbasequery")
             cQueryFilter  = REPLACE(TRIM(getAttribute(ihQueryObject,"queryfilter")),CHR(1),",")
             cQueryWhere   = REPLACE(TRIM(getAttribute(ihQueryObject,"querywhere")),CHR(1),",")
             cNewQueryJoin = cNewQueryJoin + " " 
                                    + cBaseQuery
                                    + (IF cQueryFilter BEGINS "WHERE " AND cBaseQuery MATCHES "* WHERE *" THEN " AND" + SUBSTR(cQueryFilter,7)
                                       ELSE IF cQueryFilter BEGINS "AND " AND NOT cBaseQuery MATCHES "* WHERE *" THEN " WHERE" + SUBSTR(cQueryFilter,5)
                                       ELSE " " + cQueryFilter)
                                    + (IF cQueryWhere BEGINS "WHERE " AND cBaseQuery + " " + cQueryFilter MATCHES "* WHERE *" THEN " AND" + SUBSTR(cQueryWhere,7)
                                       ELSE IF cQueryWhere BEGINS "AND " AND NOT cBaseQuery + " " + cQueryFilter MATCHES "* WHERE *" THEN " WHERE" + SUBSTR(cQueryWhere,5)
                                       ELSE " " + cQueryWhere)
                                      .
  END.
  
  setAttribute(ihQueryObject,"buffersandfields",TRIM(cNewBuffAndFlds,",")).
  setAttribute(ihQueryObject,"modbufferlist",TRIM(cNewBufferList,",")).
  setAttribute(ihQueryObject,"queryjoin",cNewQueryJoin).
  setAttribute(ihQueryObject,"basequery",REPLACE(icPrimaryQuery,CHR(1),",")).
  setAttribute(ihQueryObject,"queryfilter","").
  setAttribute(ihQueryObject,"querywhere","").
  IF getAttribute(ihQueryObject,"uniquebuffer") NE "" THEN
    setAttribute(ihQueryObject,"SkipUniqueRows",STRING(LOOKUP(getAttribute(ihQueryObject,"uniquebuffer"),cNewBufferList))).
  ELSE
    setAttribute(ihQueryObject,"SkipUniqueRows",STRING(LOOKUP(getAttribute(ihQueryObject,"basetable"),cNewBufferList))).
END.
ELSE DO:
  setAttribute(ihQueryObject,"SkipUniqueRows","").
  IF getAttribute(ihQueryObject,"orgbuffersandfields") NE "" THEN DO:
    setAttribute(ihQueryObject,"buffersandfields",getAttribute(ihQueryObject,"orgbuffersandfields")).
    setAttribute(ihQueryObject,"queryjoin",getAttribute(ihQueryObject,"orgqueryjoin")).
    setAttribute(ihQueryObject,"basequery",getAttribute(ihQueryObject,"orgbasequery")).
    setAttribute(ihQueryObject,"orgbuffersandfields","").
    setAttribute(ihQueryObject,"orgqueryjoin","").
    setAttribute(ihQueryObject,"orgbasequery","").
    setAttribute(ihQueryObject,"modbufferlist","").
  END.
END.
 
RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckModified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckModified Procedure 
FUNCTION CheckModified RETURNS LOGICAL
  ( INPUT ihFrameOrFieldMap AS HANDLE,
    INPUT icFunction        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget       AS HANDLE NO-UNDO.
DEF VAR bModified     AS LOG    NO-UNDO.
DEF VAR cCheckFields  AS CHAR   NO-UNDO.

DEF BUFFER bttObject FOR ttObject.

{incl/methodlog.i}

FIND FIRST bttObject
     WHERE bttObject.hObject = ihFrameOrFieldMap
     NO-ERROR.

IF AVAIL bttObject AND bttObject.cObjectType = "fieldMap" THEN DO:

  IF getAttribute(bttObject.hObject,"checkmodified") = "no" THEN DO:
    setAttribute(bttObject.hObject,"checkmodified","").
    RETURN NO.
  END.

  cCheckFields = TRIM(DYNAMIC-FUNCTION("mgetAttribute",bttObject.hObject,"ScreenUpdateWidgets") + "," +
                      DYNAMIC-FUNCTION("mgetAttribute",bttObject.hObject,"ExtraUpdateWidgets"),",").

  {incl/methodlog.i cCheckFields}

  DO ix = 1 TO NUM-ENTRIES(cCheckFields):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cCheckFields)).

    IF NOT CAN-DO("FILL-IN,COMBO-BOX,EDITOR,RADIO-SET,SELECTION-LIST,SLIDER,TOGGLE-BOX",hWidget:TYPE) THEN NEXT.

    IF NOT hWidget:SENSITIVE THEN NEXT.

    IF NOT (CAN-DO("FILL-IN,EDITOR",hWidget:TYPE) AND hWidget:READ-ONLY) THEN DO:
      IF icFunction = "check" AND hWidget:MODIFIED THEN DO:
        bModified = TRUE.
        LEAVE.
      END.
      ELSE hWidget:MODIFIED = FALSE. 
    END.
  END.
END.
ELSE IF NOT AVAIL bttObject THEN DO:
  ASSIGN hWidget = ihFrameOrFieldMap:FIRST-CHILD:FIRST-CHILD.
  DO WHILE VALID-HANDLE (hWidget):
    IF CAN-DO("FILL-IN,COMBO-BOX,EDITOR,RADIO-SET,SELECTION-LIST,SLIDER,TOGGLE-BOX",hWidget:TYPE) 
       AND NOT CAN-FIND(FIRST ttObject WHERE ttObject.hObject = hWidget)
       AND hWidget:SENSITIVE THEN DO:
  
      IF NOT (CAN-DO("FILL-IN,EDITOR",hWidget:TYPE) AND hWidget:READ-ONLY) THEN DO:
        IF icFunction = "check" AND hWidget:MODIFIED THEN DO:
          bModified = TRUE.
          LEAVE.
        END.
        ELSE hWidget:MODIFIED = FALSE. 
      END.
    END.
    ASSIGN hWidget = hWidget:NEXT-SIBLING.               
  END.       
END.

RETURN bModified.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CloseAndDisableChilds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CloseAndDisableChilds Procedure 
FUNCTION CloseAndDisableChilds RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Close any child queries and disable their toolbars 
    Notes: Follows parent links from query/browse to query/browse 
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihParent NO-ERROR.
IF NOT AVAIL bttObject THEN RETURN FALSE.

IF NOT CAN-DO("browse,query",bttObject.cObjectType) THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ihParent 
         AND ttObjectLink.cLinkType = "browse"
       NO-ERROR.
  IF NOT AVAIL ttObjectLink THEN
    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ihParent 
           AND ttObjectLink.cLinkType = "query"
         NO-ERROR.
  IF AVAIL ttObjectLink THEN ihParent = ttObjectLink.hToObject.
  ELSE RETURN FALSE.
END.

FOR EACH ttObjectLink
    WHERE ttObjectLink.hToObject = ihParent
      AND CAN-DO("parent",ttObjectLink.cLinkType),
    FIRST bttObject WHERE bttObject.hObject = ttObjectLink.hFromObject:
  IF CAN-DO("browse,query",bttObject.cObjectType) THEN DO:
    IF bttObject.hObject:TYPE = "query" THEN bttObject.hObject:QUERY-CLOSE().
    ELSE DO:
      bttObject.hObject:QUERY:QUERY-CLOSE().
      FOR EACH bttObjectLink
          WHERE bttObjectLink.hFromObject = bttObject.hObject
            AND bttObjectLink.cLinkType = "browseoverlay":
        bttObjectLink.hToObject:HIDDEN = TRUE.

        FIND FIRST bttEvent 
             WHERE bttEvent.hObject = bttObjectLink.hToObject
               AND bttEvent.cName   = "lookup" 
             NO-ERROR.
        IF AVAIL bttEvent THEN
          bttEvent.hWidget:HIDDEN = YES.
      END.
    END.
    IF CAN-DO(bttObject.hSourceProc:INTERNAL-ENTRIES,"ExtraCloseAndDisableChilds") THEN
      RUN ExtraCloseAndDisableChilds IN bttObject.hSourceProc(bttObject.hObject).
    FIND bttObjectLink
         WHERE bttObjectLink.hFromObject = bttObject.hObject
           AND bttObjectLink.cLinkType   = "toolbar"
         NO-ERROR.
    IF AVAIL bttObjectLink THEN
      DYNAMIC-FUNCTION("setToolbar",bttObjectLink.hToObject,"disable").
    CloseAndDisableChilds(bttObject.hObject).
  END.
END.

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DisplayRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DisplayRow Procedure 
FUNCTION DisplayRow RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

IF NOT ihBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN RETURN NO.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  ihBrowse:GET-BROWSE-COLUMN(ix):SCREEN-VALUE = STRING(ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ihBrowse:GET-BROWSE-COLUMN(ix):NAME):BUFFER-VALUE).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoLockWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoLockWindow Procedure 
FUNCTION DoLockWindow RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Avoid flashing when Progress does operations on widgets
           The setLockWindowUpdate functions is used from resizelib.p
    Notes: The programmer can set attributes on two levels on the SESSION object to avoid flashing og windows:
           1: keepwindowlocked = "yes" 
              This attribute may also be used by standard methods and hence might be switched off in the midst of an operation
           2: userkeepswindowlocked = "yes"
              Use this setting to be totally sure that there is no change in the window locking state until you allow it again.
              It is IMPERATIVE that the programmer turns off userkeepswindowlocked attribute after use.
------------------------------------------------------------------------------*/
IF getAttribute(SESSION,"userkeepswindowlocked") = "yes" THEN RETURN NO.

DYNAMIC-FUNCTION("setLockWindow",ihWindow) NO-ERROR.

IF ihWindow NE ? AND getAttribute(SESSION,"windowislocked") NE "yes" THEN DO:
  DYNAMIC-FUNCTION("setLockWindowUpdate",TRUE) NO-ERROR.
  setAttribute(SESSION,"windowislocked","yes") NO-ERROR.
END.
ELSE IF ihWindow = ? AND getAttribute(SESSION,"keepwindowlocked") NE "yes" THEN DO:
  DYNAMIC-FUNCTION("setLockWindowUpdate",FALSE) NO-ERROR.
  setAttribute(SESSION,"windowislocked","") NO-ERROR.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoMessage Procedure 
FUNCTION DoMessage RETURNS INTEGER
  ( INPUT iiMsgNo  AS INT,
    INPUT iiType   AS INT,
    INPUT icText   AS CHAR,
    INPUT icTitle  AS CHAR,
    INPUT icData   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Get meaningful button texts..
    Notes:  Message types with corresponding buttons/return value:
            Type 0:   OK/1
                 1:   Ok/1  - Cancel/2
                 2    Abort/3 - Retry/4 - Ignore/5   
                 3:   Yes/6 - No/7 - Cancel/2
                 4:   Yes/6 - No/7
                 5:   Retry/4 - Cancel/2
                 6:   Cancel/2 - Try Again/10 - Continue/11
                20:   Notepad (textfile)
------------------------------------------------------------------------------*/
DEF VAR iMsgReturn    AS INT    NO-UNDO.
DEF VAR cFileName     AS CHAR   NO-UNDO.
DEF VAR hSourceWin    AS HANDLE NO-UNDO.
DEF VAR cMessAndTitle AS CHAR   NO-UNDO.

{incl/methodlog.i icText}

IF iiType = 2 OR (iiType > 4 AND iiType < 10) THEN DO:
  hSourceWin = SOURCE-PROCEDURE:CURRENT-WINDOW NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    hSourceWin = hCurrWindow.
  IF VALID-HANDLE(hSourceWin) THEN
    hSourceWin:SENSITIVE = NO.
END.

IF iiMsgNo NE 0 THEN DO:
  IF DYNAMIC-FUNCTION("runProc","jbserv_getusermessage.p",STRING(iiMsgNo) + "|" + icText + "|" + icTitle,?) THEN 
    ASSIGN cMessAndTitle = DYNAMIC-FUNCTION("getTransactionMessage")
           icText  = ENTRY(1,cMessAndTitle,"|")
           icTitle = ENTRY(2,cMessAndTitle,"|").
END.
ELSE IF NUM-ENTRIES(icText,"|") > 1 THEN 
  ASSIGN icTitle = ENTRY(2,icText,"|")
         icText  = ENTRY(1,icText,"|")
         .

IF icData NE "" THEN
  DO ix = 1 TO NUM-ENTRIES(icData,"|"):
    icText = REPLACE(icText,"&" + STRING(ix),ENTRY(ix,icData,"|")).
  END.

/* IF iiType < 10 THEN                                                */
/*   RUN MessageBoxA (0, icText, icTitle, iiType, OUTPUT iMsgReturn). */
IF iiType < 10 THEN DO:
  CASE iiType:
    WHEN 0 THEN DO:
        MESSAGE icText VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE icTitle.
    END.  
    WHEN 1 THEN DO:
       MESSAGE icText VIEW-AS ALERT-BOX INFO BUTTONS OK-CANCEL TITLE icTitle UPDATE bOK.
       iMsgReturn = IF bOk THEN 1 ELSE 2.
    END.
    WHEN 3 THEN DO:
       MESSAGE icText VIEW-AS ALERT-BOX INFO BUTTONS YES-NO-CANCEL TITLE icTitle UPDATE bOK.
       iMsgReturn = IF bOk THEN 6 ELSE IF NOT bOk THEN 7 ELSE 2.
    END.
    WHEN 4 THEN DO:
       MESSAGE icText VIEW-AS ALERT-BOX INFO BUTTONS YES-NO TITLE icTitle UPDATE bOK.
       iMsgReturn = IF bOk THEN 6 ELSE 7.
    END.
    OTHERWISE RUN MessageBoxA (0, icText, icTitle, iiType, OUTPUT iMsgReturn).
  END CASE.
END.

ELSE IF iiType = 20 THEN DO:
  cFileName = SESSION:TEMP-DIR + "msg" + "_" + STRING(TIME) + ".txt".
  OUTPUT TO VALUE(cFileName).
  IF icTitle NE "" THEN 
    PUT UNFORMATTED icTitle SKIP(1).
  PUT UNFORMATTED icText.
  OUTPUT CLOSE.
  DYNAMIC-FUNCTION("setWebDoc","open",cFileName).
END.

IF VALID-HANDLE(hSourceWin) THEN
  hSourceWin:SENSITIVE = YES.

IF iiMsgNo < 0 THEN CLIPBOARD:VALUE = icText.

RETURN iMsgReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryOperators) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixQueryOperators Procedure 
FUNCTION FixQueryOperators RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix         AS INT  NO-UNDO.
DEF VAR iy         AS INT  NO-UNDO.
DEF VAR ixWhere    AS INT  NO-UNDO.
DEF VAR cPart      AS CHAR NO-UNDO.
DEF VAR cNewPart   AS CHAR NO-UNDO.
DEF VAR cNewString AS CHAR NO-UNDO.

icQueryString = TRIM(icQueryString).

ASSIGN icQueryString = REPLACE(icQueryString,"where where ","WHERE ")
       icQueryString = REPLACE(icQueryString,"where  where ","WHERE ")
       icQueryString = REPLACE(icQueryString," where where "," WHERE ")
       icQueryString = REPLACE(icQueryString," where true where "," WHERE ")
       icQueryString = REPLACE(icQueryString," and and "," AND ")
       icQueryString = REPLACE(icQueryString," and ( and "," AND ( ")
       icQueryString = REPLACE(icQueryString," where ( and "," WHERE ( ")
       icQueryString = REPLACE(icQueryString," and ( where "," WHERE ( ")
       icQueryString = REPLACE(icQueryString," where and "," WHERE ( ")
       icQueryString = REPLACE(icQueryString," where  and "," WHERE ( ")
       icQueryString = REPLACE(icQueryString,"where and ","WHERE ")
       icQueryString = REPLACE(icQueryString,"where  and ","WHERE ")
       icQueryString = REPLACE(icQueryString," and or "," OR ")
       icQueryString = REPLACE(icQueryString," or and "," OR ")


       icQueryString = REPLACE(icQueryString,",   FIRST",",FIRST")
       icQueryString = REPLACE(icQueryString,",  FIRST",",FIRST")
       icQueryString = REPLACE(icQueryString,",   LAST",",LAST")
       icQueryString = REPLACE(icQueryString,",  LAST",",LAST")
       icQueryString = REPLACE(icQueryString,",   EACH",",EACH")
       icQueryString = REPLACE(icQueryString,",  EACH",",EACH")
       icQueryString = REPLACE(icQueryString,",FIRST",CHR(3) + "FIRST")
       icQueryString = REPLACE(icQueryString,", FIRST",CHR(3) + "FIRST")
       icQueryString = REPLACE(icQueryString,",LAST",CHR(3) + "LAST")
       icQueryString = REPLACE(icQueryString,", LAST",CHR(3) + "LAST")
       icQueryString = REPLACE(icQueryString,",EACH",CHR(3) + "EACH")
       icQueryString = REPLACE(icQueryString,", EACH",CHR(3) + "EACH")
       .

IF icQueryString BEGINS "AND " THEN
  icQueryString = "WHERE " + SUBSTR(icQueryString,5).

DO ix = 1 TO NUM-ENTRIES(icQueryString,CHR(3)):
  ASSIGN ixWhere  = 0
         cPart    = ENTRY(ix,icQueryString,CHR(3))
         cNewPart = "".
  DO iy = 1 TO NUM-ENTRIES(cPart," "):
    IF ENTRY(iy,cPart," ") = "where" THEN DO:
      ixWhere = ixWhere + 1.
      IF ixWhere < 2 THEN
        cNewPart = cNewPart + " " + ENTRY(iy,cPart," ") + " ".
      ELSE 
        cNewPart = cNewPart + " AND ".
    END.
    ELSE IF ENTRY(iy,cPart," ") NE "" THEN 
      cNewPart = cNewPart + ENTRY(iy,cPart," ") + " ".
    ELSE
      cNewPart = cNewPart + " ".
  END.
  cNewString = cNewString + TRIM(cNewPart) + ",".
END.

RETURN RIGHT-TRIM(cNewString,","). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAttribute Procedure 
FUNCTION getAttribute RETURNS CHARACTER
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttAttribute 
     WHERE ttAttribute.hObject = ihObject
       AND ttAttribute.cName   = icName
     NO-ERROR.
IF AVAIL ttAttribute THEN
  RETURN ttAttribute.cValue.
ELSE RETURN "".   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentAction Procedure 
FUNCTION getCurrentAction RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND ttEvent
     WHERE ttEvent.hWidget = hCurrWidget
     NO-ERROR.
IF AVAIL ttEvent THEN 
  RETURN ttEvent.cAction.
ELSE
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentObject Procedure 
FUNCTION getCurrentObject RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF hTmpObject NE ? THEN
  RETURN hTmpObject.
ELSE IF AVAIL ttObject THEN
  RETURN ttObject.hObject.
ELSE
  RETURN ?.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentSourceProc Procedure 
FUNCTION getCurrentSourceProc RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hCurrSourceProc.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentWidget Procedure 
FUNCTION getCurrentWidget RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hCurrWidget.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDropDownLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDropDownLabel Procedure 
FUNCTION getDropDownLabel RETURNS CHARACTER
  ( INPUT ihDropDownHandle AS HANDLE,
    INPUT icDelimiter      AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iIndexPlussOne AS INT NO-UNDO.

IF icDelimiter = "" THEN
  icDelimiter = ihDropDownHandle:DELIMITER.

DO ix = 2 TO NUM-ENTRIES(ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) BY 2:
  IF ENTRY(ix,ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) = ihDropDownHandle:SCREEN-VALUE THEN
    RETURN ENTRY(ix - 1,ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter).
END.

RETURN "".

/* iIndexPlussOne = LOOKUP(ihDropDownHandle:SCREEN-VALUE, ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) + 1.                                             */
/*                                                                                                                                                       */
/* IF ihDropDownHandle:SCREEN-VALUE = ? THEN                                                                                                             */
/*   RETURN "".                                                                                                                                          */
/* IF iIndexPlussOne LE NUM-ENTRIES(ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) AND                                                                    */
/*    ENTRY(LOOKUP(ihDropDownHandle:SCREEN-VALUE, ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) + 1,ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) =      */
/*    ENTRY(LOOKUP(ihDropDownHandle:SCREEN-VALUE, ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter),ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) THEN       */
/*   RETURN ihDropDownHandle:ENTRY(ihDropDownHandle:LOOKUP(ihDropDownHandle:SCREEN-VALUE)).                                                              */
/* ELSE                                                                                                                                                  */
/*   RETURN ENTRY(LOOKUP(ihDropDownHandle:SCREEN-VALUE, ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) - 1,ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter). */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDropDownValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDropDownValue Procedure 
FUNCTION getDropDownValue RETURNS CHARACTER
  ( INPUT ihDropDownHandle AS HANDLE,
    INPUT icLabel          AS CHAR,
    INPUT icDelimiter      AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.

IF icDelimiter = "" THEN
  icDelimiter = ",".

IF icLabel = "" THEN RETURN "".
ELSE IF icLabel = ? THEN RETURN "".
ELSE DO:
  cReturn = ENTRY(LOOKUP(icLabel, ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) + 1,ihDropDownHandle:LIST-ITEM-PAIRS,icDelimiter) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
    RETURN cReturn.
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventProcReturnValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEventProcReturnValue Procedure 
FUNCTION getEventProcReturnValue RETURNS CHARACTER
  ( INPUT ihObject      AS HANDLE,
    INPUT icMethod      AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Keep a stack of the last 10 RETURN values from event procedures 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cAllRetValues AS CHAR NO-UNDO.

IF ihObject NE ? AND icMethod NE "" THEN DO:
  FIND FIRST ttEventProcReturn
       WHERE ttEventProcReturn.hObject = ihObject
         AND ttEventProcReturn.cMethod = icMethod
       NO-ERROR.
  IF AVAIL ttEventProcReturn THEN
    RETURN ttEventProcReturn.cReturnValue.
END.
ELSE IF ihObject NE ? THEN DO:
  FIND FIRST ttEventProcReturn
       WHERE ttEventProcReturn.hObject = ihObject
       NO-ERROR.
  IF AVAIL ttEventProcReturn THEN
    RETURN ttEventProcReturn.cReturnValue.
END.
ELSE IF icMethod NE "" THEN DO:
  FIND FIRST ttEventProcReturn
       WHERE ttEventProcReturn.cMethod = icMethod
       NO-ERROR.
  IF AVAIL ttEventProcReturn THEN
    RETURN ttEventProcReturn.cReturnValue.
END.
ELSE FOR EACH ttEventProcReturn:
  cAllRetValues = cAllRetValues + ttEventProcReturn.cMethod + "," + ttEventProcReturn.cReturnValue + ",".
END.

RETURN TRIM(cAllRetValues,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getExcelHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getExcelHandle Procedure 
FUNCTION getExcelHandle RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Remember to set Excel visible yourself
------------------------------------------------------------------------------*/
DEF VAR chActiveWin     AS COM-HANDLE NO-UNDO.
DEF VAR chExcelInstance AS COM-HANDLE NO-UNDO.

IF bKeepExcel THEN DO:
  chActiveWin = chExcelApplication:ActiveWindow NO-ERROR.
  
  /* If there is no active Excel (known to JukeBox), create a new instance: */
  IF NOT VALID-HANDLE(chActiveWin) THEN DO:
    RELEASE OBJECT chExcelApplication NO-ERROR.
    chExcelApplication = ?.
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
    IF VALID-HANDLE(chExcelApplication) THEN
      chExcelApplication:VISIBLE = FALSE.
  END.
  RETURN chExcelApplication.
END.
ELSE DO:
  CREATE "Excel.Application" chExcelInstance NO-ERROR.
  IF VALID-HANDLE(chExcelApplication) THEN
    chExcelInstance:VISIBLE = FALSE.
  RETURN chExcelInstance.
END.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMousePosition) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMousePosition Procedure 
FUNCTION getMousePosition RETURNS INTEGER
  ( INPUT ihWinOrFrame AS HANDLE,
    INPUT icXY         AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* Ref http://oehive.org/node/435: */
/*----- memptr for use with API call -----*/
DEF VAR mpt_mousexy AS MEMPTR NO-UNDO.
/*----- define space for 2 long integers -----*/
set-size(mpt_mousexy) = 8.
/*----- result code from API call -----*/
DEF VAR rc AS INT NO-UNDO.

RUN GetCursorPos(INPUT GET-POINTER-VALUE(mpt_mousexy),
                 OUTPUT rc).

IF NOT VALID-HANDLE(ihWinOrFrame) THEN DO:
  IF icXY = "Y" THEN RETURN INT(STRING(GET-LONG(mpt_mousexy,5))).
  ELSE RETURN INT(STRING(GET-LONG(mpt_mousexy,1))).
END.

RUN ScreenToClient (ihWinOrFrame:HWND,
                    INPUT GET-POINTER-VALUE(mpt_mousexy),
                    OUTPUT rc).
IF icXY = "Y" THEN RETURN INT(STRING(GET-LONG(mpt_mousexy,5))).
ELSE RETURN INT(STRING(GET-LONG(mpt_mousexy,1))).

END FUNCTION.

PROCEDURE GetCursorPos EXTERNAL "user32.dll":
  DEFINE INPUT  PARAMETER  lpPoint     AS LONG. /* memptr */
  DEFINE RETURN PARAMETER  ReturnValue AS LONG.
END PROCEDURE.
PROCEDURE ScreenToClient EXTERNAL "user32" :
   DEFINE INPUT  PARAMETER hWnd        AS LONG.
   DEFINE INPUT  PARAMETER lpPoint     AS LONG. /* memptr */
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryRowIdent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrimaryRowIdent Procedure 
FUNCTION getPrimaryRowIdent RETURNS CHARACTER
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffer AS HANDLE NO-UNDO.

IF ihQueryObject:TYPE = "QUERY" THEN
  hBuffer = ihQueryObject:GET-BUFFER-HANDLE(1).
ELSE
  hBuffer = ihQueryObject:QUERY:GET-BUFFER-HANDLE(1).

IF NOT hBuffer:AVAIL THEN RETURN "".

IF getAttribute(ihQueryObject,"modbufferlist") NE "" THEN
  RETURN hBuffer:BUFFER-FIELD("RowIdent" + STRING(LOOKUP(ENTRY(1,getAttribute(ihQueryObject,"bufferlist")),getAttribute(ihQueryObject,"modbufferlist")))):BUFFER-VALUE.
ELSE
  RETURN hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryBufferList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQueryBufferList Procedure 
FUNCTION getQueryBufferList RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cBufferList AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.

icQueryString = TRIM(icQueryString).
DO ix = 1 TO 5:
  icQueryString = REPLACE(icQueryString,"  "," ").
END.

ASSIGN icQueryString = REPLACE(icQueryString,",FIRST","¤FIRST")
       icQueryString = REPLACE(icQueryString,", FIRST","¤FIRST")
       icQueryString = REPLACE(icQueryString,",LAST","¤LAST")
       icQueryString = REPLACE(icQueryString,", LAST","¤LAST")
       icQueryString = REPLACE(icQueryString,",EACH","¤EACH")
       icQueryString = REPLACE(icQueryString,", EACH","¤EACH")
       icQueryString = REPLACE(icQueryString,",","|")
       icQueryString = REPLACE(icQueryString,"¤FIRST",",FIRST")
       icQueryString = REPLACE(icQueryString,"¤LAST",",LAST")
       icQueryString = REPLACE(icQueryString,"¤EACH",",EACH")
       .

DO ix = 1 TO NUM-ENTRIES(icQueryString):
  IF ENTRY(ix,icQueryString) BEGINS "FIRST " 
    OR ENTRY(ix,icQueryString) BEGINS "LAST "
    OR ENTRY(ix,icQueryString) BEGINS "EACH " THEN
    cBufferList = cBufferList + ENTRY(2,ENTRY(ix,icQueryString)," ") + ",".
END.
  
RETURN TRIM(cBufferList,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSelectionListLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectionListLabel Procedure 
FUNCTION getSelectionListLabel RETURNS CHARACTER
  ( INPUT ihSelListHandle AS HANDLE,
    INPUT iiEntry         AS INT,
    INPUT icDelimiter     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icDelimiter = "" THEN
  icDelimiter = ",".

RETURN ENTRY(LOOKUP(ihSelListHandle:ENTRY(iiEntry),ihSelListHandle:LIST-ITEM-PAIRS,icDelimiter) - 1,ihSelListHandle:LIST-ITEM-PAIRS,icDelimiter).  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTmpObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTmpObject Procedure 
FUNCTION getTmpObject RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hTmpObject.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWordHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWordHandle Procedure 
FUNCTION getWordHandle RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Remember to set Word visible yourself
------------------------------------------------------------------------------*/
DEF VAR chActiveWin    AS COM-HANDLE NO-UNDO.
DEF VAR chWordInstance AS COM-HANDLE NO-UNDO.

/* chActiveWin = chWordApplication:ActiveWindow NO-ERROR.  */

/* If there is no active Word (known to JukeBox), create a new instance: */
IF NOT VALID-HANDLE(chWordApplication) THEN DO:
  RELEASE OBJECT chWordApplication NO-ERROR.
  chWordApplication = ?.
  CREATE "Word.Application" chWordApplication NO-ERROR.
  IF VALID-HANDLE(chWordApplication) THEN
    chWordApplication:VISIBLE = FALSE.
END.
RETURN chWordApplication.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadQueryFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadQueryFilter Procedure 
FUNCTION LoadQueryFilter RETURNS LOGICAL
  ( INPUT ihBrowseOrQuery  AS HANDLE,
    INPUT ibExcecuteFilter AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cWinSettings  AS CHAR NO-UNDO.
DEF VAR iy            AS INT NO-UNDO.
DEF VAR hWidget       AS HANDLE NO-UNDO.
DEF VAR bFilterActive AS LOG NO-UNDO.
DEF VAR cFilterString AS CHAR NO-UNDO.
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hField        AS HANDLE NO-UNDO.

FIND ttObject WHERE ttObject.hObject = ihBrowseOrQuery NO-ERROR.
IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "browse" AND ttObject.cObjectType NE "query") THEN DO:
  MESSAGE "Invalid object reference, LoadQueryFilter"
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".
  RETURN FALSE.
END.

hBuffer = IF ihBrowseOrQuery:TYPE = "browse" THEN ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1) 
          ELSE ihBrowseOrQuery:GET-BUFFER-HANDLE(1).

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ttObject.hObject
       AND ttObjectLink.cLinkType   = "filter"
     NO-ERROR.
FIND FIRST bttObjectLink
     WHERE bttObjectLink.hFromObject = ttObject.hObject
       AND bttObjectLink.cLinkType   = "toolbar"
     NO-ERROR.

cWinSettings = DYNAMIC-FUNCTION("getCustomWinSettings",ttObject.hWindow).

DO ix = 1 TO NUM-ENTRIES(cWinSettings,"|"):
  IF ENTRY(ix,cWinSettings,"|") = "<StartFilter>" THEN DO:
    DO iy = ix + 1 TO NUM-ENTRIES(cWinSettings,"|") BY 4:
      IF ENTRY(iy,cWinSettings,"|") = "<endfilter>" THEN LEAVE.
      ELSE IF ENTRY(iy,cWinSettings,"|") NE "" THEN DO:
        bFilterActive = TRUE.
        IF AVAIL ttObjectLink THEN DO:
          hWidget = WIDGET-HANDLE(getAttribute(ttObjectLink.hToObject,"filterfield_" + ENTRY(iy,cWinSettings,"|"))).
          IF VALID-HANDLE(hWidget) THEN
            hWidget:SCREEN-VALUE = ENTRY(iy + 3,cWinSettings,"|").

          hWidget = WIDGET-HANDLE(getAttribute(ttObjectLink.hToObject,"operatorfield_" + ENTRY(iy,cWinSettings,"|"))).
          IF VALID-HANDLE(hWidget) THEN
            hWidget:SCREEN-VALUE = ENTRY(iy + 2,cWinSettings,"|").
        END.
        setAttribute(ttObject.hObject,"filtervalue_"   + ENTRY(iy,cWinSettings,"|"),ENTRY(iy + 3,cWinSettings,"|")).
        setAttribute(ttObject.hObject,"operatorInUse_" + ENTRY(iy,cWinSettings,"|"),ENTRY(iy + 2,cWinSettings,"|")).

        hField = hBuffer:BUFFER-FIELD(ENTRY(iy + 1,cWinSettings,"|")).

        cFilterString = cFilterString + hField:NAME + " " + 
                        ENTRY(iy + 2,cWinSettings,"|") + 
                        (IF hField:DATA-TYPE      = "CHARACTER" THEN " '" + ENTRY(iy + 3,cWinSettings,"|") + "'"
                         ELSE IF hField:DATA-TYPE BEGINS "DATE" THEN " " + hField:DATA-TYPE + "('" + ENTRY(iy + 3,cWinSettings,"|") + "')"
                         ELSE IF hField:DATA-TYPE = "DECIMAL"   THEN " DEC('" + ENTRY(iy + 3,cWinSettings,"|") + "')"
                         ELSE " " + ENTRY(iy + 3,cWinSettings,"|")) +
                        " AND ".
      END.
    END.
    LEAVE.
  END.
END.
IF bFilterActive AND AVAIL bttObjectLink THEN DO:
  hWidget = DYNAMIC-FUNCTION("getEventWidget",bttObjectLink.hToObject,"filter","").
  &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
  IF VALID-HANDLE(hWidget) AND cActiveFilterButton NE "" THEN
    hWidget:LOAD-IMAGE(cActiveFilterButton).
  &ENDIF
  
  IF ibExcecuteFilter THEN DO:
    IF cFilterString NE "" THEN
      ASSIGN cFilterString = (IF getAttribute(ihBrowseOrQuery,"basequery") NE "" THEN " AND " ELSE "WHERE ") + cFilterString
             cFilterString = RIGHT-TRIM(cFilterString," AND ").

    setAttribute(ihBrowseOrQuery,"queryfilter",cFilterString).
    ASSIGN bSetCurrHandles = FALSE.
           hCurrWidget    = ihBrowseOrQuery
           .
    setCurrentObject(ihBrowseOrQuery).
    RUN DoProcessEvent(0,"open-query"). 
  END.

  RETURN TRUE.
END.
ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadWinIcon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadWinIcon Procedure 
FUNCTION LoadWinIcon RETURNS LOGICAL
  ( INPUT ihWindow   AS HANDLE,
    INPUT icIcon     AS CHAR,
    INPUT ibOverride AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icIcon NE "" AND
   (IF NOT ibOverride THEN ihWindow:ICON = "" ELSE TRUE)
   AND SEARCH(icIcon) NE ? THEN DO:
  ihWindow:LOAD-ICON(icIcon).
  RETURN YES.
END.

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NavigateFromBrowseOverlay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NavigateFromBrowseOverlay Procedure 
FUNCTION NavigateFromBrowseOverlay RETURNS LOGICAL
  ( INPUT ihOverlay AS HANDLE,
    INPUT ihBrowse  AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTabTo               AS HANDLE NO-UNDO.
DEF VAR hLastEnabledOverlay  AS HANDLE NO-UNDO.
DEF VAR hFirstEnabledOverlay AS HANDLE NO-UNDO.
DEF VAR cTabChainOverlays    AS CHAR   NO-UNDO.
DEF VAR cLastEvent           AS CHAR   NO-UNDO.
DEF VAR bNewRow              AS LOG    NO-UNDO.

hLastEnabledOverlay  = WIDGET-HANDLE(getAttribute(ihBrowse,"lastenabledoverlay")).
hFirstEnabledOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"firstenabledoverlay")).
bNewRow              = getAttribute(ihBrowse,"newrow") = "yes".

IF getAttribute(ihOverlay,"last-event") NE "" THEN DO:
  cLastEvent = getAttribute(ihOverlay,"last-event").
  setAttribute(ihOverlay,"last-event","").
END.
ELSE
  cLastEvent = LAST-EVENT:LABEL.

IF getAttribute(ihBrowse,"enableOnToolbarClick") = "yes" THEN DO:
  IF (ihOverlay = hLastEnabledOverlay AND CAN-DO("tab,enter",cLastEvent)) OR
     (ihOverlay = hFirstEnabledOverlay AND CAN-DO("shift-tab",cLastEvent)) OR
     CAN-DO("cursor-up,cursor-down,page-up,page-down",cLastEvent)  
     THEN DO:
    setWidgetEnter(ihOverlay).
    RETURN NO.
  END.  
  IF cLastEvent = "enter" THEN cLastEvent = "tab".
END.  

IF NOT ihBrowse:IS-ROW-SELECTED(ihBrowse:FOCUSED-ROW) THEN
  ihBrowse:SELECT-ROW(ihBrowse:FOCUSED-ROW) NO-ERROR.
IF ihBrowse:IS-ROW-SELECTED(ihBrowse:FOCUSED-ROW) THEN DO: 

  IF cLastEvent = "enter" THEN DO:
    IF getAttribute(ihBrowse,"stayInFieldOnEnter") NE "yes" THEN
      IF NOT bNewRow AND NOT ihBrowse:SELECT-NEXT-ROW() THEN APPLY "off-end" TO ihBrowse.

    IF getAttribute(ihBrowse,"enableondblclick") = "yes" AND
       (getAttribute(ihBrowse,"setReadOnlyOnReturn") = "yes" OR
       (getAttribute(ihBrowse,"setReadOnlyOnReturnOfLastField") = "yes" AND
        ihOverlay = hLastEnabledOverlay)) 
       THEN 
      setAttribute(ihBrowse,"enableupdate","no").

    APPLY "value-changed" TO ihBrowse.
    bFreezeEndResizeBrw = NO.

    IF NOT getAttribute(ihBrowse,"setreadonlyonreturn") = "yes" AND NOT
      (getAttribute(ihBrowse,"setReadOnlyOnReturnOfLastField") = "yes" AND
       ihOverlay = hLastEnabledOverlay)
      THEN DO:
      IF NOT VALID-HANDLE(ihOverlay) THEN
        ihOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"currentoverlaywidget")).
      IF VALID-HANDLE(ihOverlay) THEN DO:
        APPLY "entry" TO ihOverlay.
        setAttribute(ihBrowse,"currentoverlaywidget",STRING(ihOverlay)).
      END.
    END.
    ELSE DO:
      setWidgetEnter(ihBrowse).
      IF getAttribute(ihBrowse,"setReadOnlyOnReturnOfLastField") = "yes" AND VALID-HANDLE(hLastEnabledOverlay) THEN
        setAttribute(ihBrowse,"currentoverlaywidget",STRING(hLastEnabledOverlay)).
    END.
  END.
  ELSE IF CAN-DO("cursor-down,page-down",cLastEvent) THEN DO:
    IF cLastEvent = "cursor-down" THEN DO:
      IF NOT ihBrowse:SELECT-NEXT-ROW() THEN APPLY "off-end" TO ihBrowse.
    END.
    ELSE
      APPLY "page-down" TO ihBrowse.

    APPLY "value-changed" TO ihBrowse.

    IF NOT VALID-HANDLE(ihOverlay) THEN
      ihOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"currentoverlaywidget")).
    IF VALID-HANDLE(ihOverlay) THEN DO:
      APPLY "entry" TO ihOverlay.
      setAttribute(ihBrowse,"currentoverlaywidget",STRING(ihOverlay)).
    END.
    ELSE DO:
      setWidgetEnter(ihBrowse).
      IF getAttribute(ihBrowse,"enableondblclick") = "yes" THEN DO:
        IF getAttribute(ihBrowse,"doubleclickenabledfield") = "" THEN
          setAttribute(ihBrowse,"enableupdate","no").
      END.
    END.
  END.
  ELSE IF CAN-DO("cursor-up,page-up",cLastEvent) THEN DO:
/*     IF ihBrowse:SELECT-PREV-ROW() THEN DO: */
    IF cLastEvent = "cursor-up" THEN DO:
      IF NOT ihBrowse:SELECT-PREV-ROW() THEN APPLY "off-home" TO ihBrowse.
    END.
    ELSE
      APPLY "page-up" TO ihBrowse.

    APPLY "value-changed" TO ihBrowse.
    IF NOT VALID-HANDLE(ihOverlay) THEN
      ihOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"currentoverlaywidget")).
    IF VALID-HANDLE(ihOverlay) THEN DO:
      APPLY "entry" TO ihOverlay.
      setAttribute(ihBrowse,"currentoverlaywidget",STRING(ihOverlay)).
    END.
    ELSE DO:
      setWidgetEnter(ihBrowse).
      IF getAttribute(ihBrowse,"enableondblclick") = "yes" THEN DO:
        IF getAttribute(ihBrowse,"doubleclickenabledfield") = "" THEN
          setAttribute(ihBrowse,"enableupdate","no").
      END.
    END.
/*     END. */
  END.
  ELSE IF cLastEvent = "tab" AND ihOverlay = hLastEnabledOverlay THEN DO:
    IF getAttribute(ihBrowse,"currentrowid") NE getAttribute(ihBrowse,"lastrowid") THEN DO:
      IF NOT bNewRow AND NOT ihBrowse:SELECT-NEXT-ROW() THEN APPLY "off-end" TO ihBrowse.
      bFreezeEndResizeBrw = YES.
      APPLY "value-changed" TO ihBrowse.
      bFreezeEndResizeBrw = NO.
  
      IF NOT VALID-HANDLE(ihOverlay) THEN 
        ASSIGN hLastEnabledOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"currentoverlaywidget"))
               ihOverlay           = hLastEnabledOverlay.
      
      IF NOT VALID-HANDLE(hFirstEnabledOverlay) THEN
        hFirstEnabledOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"firstenabledoverlay")).

      IF VALID-HANDLE(ihOverlay) AND
        getAttribute(ihBrowse,"gotoLastEnabledFromTab") NE "yes" THEN DO:
        ASSIGN hFirstEnabledOverlay:TAB-STOP = TRUE
               hLastEnabledOverlay:TAB-STOP = TRUE
               .
        hFirstEnabledOverlay:MOVE-AFTER(hLastEnabledOverlay).
        IF hFirstEnabledOverlay = hLastEnabledOverlay THEN
          setAttribute(hLastEnabledOverlay,"return-no-apply","yes").
        ELSE
          setAttribute(hLastEnabledOverlay,"return-no-apply","").
        APPLY "entry" TO hFirstEnabledOverlay.
        setAttribute(ihBrowse,"currentoverlaywidget",STRING(hFirstEnabledOverlay)).
      END.
      ELSE IF VALID-HANDLE(ihOverlay) THEN DO:
        APPLY "entry" TO ihOverlay.
        setAttribute(ihBrowse,"currentoverlaywidget",STRING(ihOverlay)).
      END.
      ELSE DO:
        setWidgetEnter(ihBrowse).
        IF getAttribute(ihBrowse,"enableondblclick") = "yes" THEN DO:
          IF getAttribute(ihBrowse,"doubleclickenabledfield") = "" THEN
            setAttribute(ihBrowse,"enableupdate","no").
        END.
      END.
    END.
    ELSE DO:
      setWidgetEnter(ihBrowse).
      IF getAttribute(ihBrowse,"enableondblclick") = "yes" THEN DO:
        IF getAttribute(ihBrowse,"doubleclickenabledfield") = "" THEN
          setAttribute(ihBrowse,"enableupdate","no").
      END.
    END.
  END.
  ELSE IF cLastEvent = "shift-tab" AND ihOverlay = hFirstEnabledOverlay THEN DO:
    IF ihBrowse:SELECT-PREV-ROW() THEN DO:
      APPLY "value-changed" TO ihBrowse.
      bFreezeEndResizeBrw = NO.
      IF NOT VALID-HANDLE(ihOverlay) THEN
        ASSIGN hFirstEnabledOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"currentoverlaywidget"))
               ihOverlay           = hFirstEnabledOverlay.
      
      IF NOT VALID-HANDLE(hLastEnabledOverlay) THEN
        hLastEnabledOverlay = WIDGET-HANDLE(getAttribute(ihBrowse,"lastenabledoverlay")).

      IF VALID-HANDLE(hLastEnabledOverlay) AND
         VALID-HANDLE(hFirstEnabledOverlay) AND 
         getAttribute(ihBrowse,"gotoFirstEnabledFromBackTab") NE "yes" THEN DO:
        ASSIGN hFirstEnabledOverlay:TAB-STOP = TRUE
               hLastEnabledOverlay:TAB-STOP = TRUE
               .
        hFirstEnabledOverlay:MOVE-AFTER(hLastEnabledOverlay).
        IF hFirstEnabledOverlay = hLastEnabledOverlay THEN
          setAttribute(hLastEnabledOverlay,"return-no-apply","yes").
        ELSE
          setAttribute(hLastEnabledOverlay,"return-no-apply","").
        APPLY "entry" TO hLastEnabledOverlay.
        setAttribute(ihBrowse,"currentoverlaywidget",STRING(hLastEnabledOverlay)).
      END.
      ELSE IF VALID-HANDLE(ihOverlay) THEN DO:
        APPLY "entry" TO ihOverlay.
        setAttribute(ihBrowse,"currentoverlaywidget",STRING(ihOverlay)).
      END.
      ELSE DO:
        setWidgetEnter(ihBrowse).
        IF getAttribute(ihBrowse,"enableondblclick") = "yes" THEN DO:
          IF getAttribute(ihBrowse,"doubleclickenabledfield") = "" THEN
            setAttribute(ihBrowse,"enableupdate","no").
        END.
      END.
    END.
    ELSE setWidgetEnter(ihBrowse).
  END.
  ELSE IF cLastEvent = "tab" THEN DO:
    cTabChainOverlays = getAttribute(ihBrowse,"tabchainoverlays").
    IF NUM-ENTRIES(cTabChainOverlays) > 1 THEN DO:
      ASSIGN hTabTo = WIDGET-HANDLE(ENTRY(LOOKUP(STRING(ihOverlay),cTabChainOverlays) + 1,cTabChainOverlays))
             hTabTo:TAB-STOP = TRUE
             ihOverlay:TAB-STOP = TRUE.

/*       IF DYNAMIC-FUNCTION("getEventWidget",ihOverlay,"choose","browse-lookup") NE "" AND hTabTo:TYPE = "combo-box" THEN DO:  */
/*         PUBLISH "SuspendNamedTimer" ("FocusTimer",NO).                                                                       */
/*         setAttribute(SESSION,"nextFocusWidget",STRING(hTabTo)).                                                              */
/*       END.                                                                                                                   */

      hTabTo:MOVE-AFTER(ihOverlay).
      setWidgetEnter(hTabTo).
      setAttribute(ihBrowse,"currentoverlaywidget",STRING(hTabTo)).
    END.
  END.
  ELSE IF cLastEvent = "shift-tab" THEN DO:
    cTabChainOverlays = getAttribute(ihBrowse,"tabchainoverlays").
    IF NUM-ENTRIES(cTabChainOverlays) > 1 THEN DO:
      ASSIGN hTabTo = WIDGET-HANDLE(ENTRY(LOOKUP(STRING(ihOverlay),cTabChainOverlays) - 1,cTabChainOverlays))
             hTabTo:TAB-STOP = TRUE
             ihOverlay:TAB-STOP = TRUE.

      hTabTo:MOVE-BEFORE(ihOverlay).
      setWidgetEnter(hTabTo).
      setAttribute(ihBrowse,"currentoverlaywidget",STRING(hTabTo)).
    END.
  END.

END.

IF bNewRow THEN
  setAttribute(ihBrowse,"newrow","yes").

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshParentRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RefreshParentRecord Procedure 
FUNCTION RefreshParentRecord RETURNS LOGICAL
  ( INPUT ihSourceQuery AS HANDLE,
    INPUT ihParentQuery AS HANDLE,
    INPUT icAction      AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Refresh a navigation browse/query after a record modification or refresh from database 
    Notes: SourceQuery is always a query linked to the parent (navigation) as onetoone
           If the source query definition doesn't match the parent (navigation browse) AND there
           are duplicate field names in a joined query this function will yield unpredictable results.
           You must in this case write your own copy procedure from source to target (parent) buffer:
           MyRefreshParentRecord (for parameters, see under)
------------------------------------------------------------------------------*/
DEF VAR rRepos          AS ROWID  NO-UNDO.
DEF VAR hParentBuffer   AS HANDLE NO-UNDO.
DEF VAR hChildBuffer    AS HANDLE NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR cFindString     AS CHAR   NO-UNDO.
DEF VAR cChildLinkInfo  AS CHAR   NO-UNDO.
DEF VAR iFocusedRow     AS INT    NO-UNDO.
DEF VAR hChildQueryObj  AS HANDLE NO-UNDO.

IF icAction = "" THEN RETURN FALSE.

IF ihParentQuery:TYPE = "query" THEN
  hParentBuffer = ihParentQuery:GET-BUFFER-HANDLE(1).
ELSE 
  hParentBuffer = ihParentQuery:QUERY:GET-BUFFER-HANDLE(1).
IF ihSourceQuery:TYPE = "query" THEN
  hChildBuffer = ihSourceQuery:GET-BUFFER-HANDLE(1).
ELSE 
  hChildBuffer = ihSourceQuery:QUERY:GET-BUFFER-HANDLE(1).

IF getAttribute(hParentBuffer,"RefreshParentRecord") = "no" THEN RETURN NO.

IF icAction = "delete" THEN DO:
  hParentBuffer:BUFFER-DELETE().
  setAttribute(ihParentQuery,"rowsdeleted",STRING(INT(getAttribute(ihParentQuery,"rowsdeleted")) + 1)).
  DYNAMIC-FUNCTION("ViewRecordCount",ihParentQuery).
  IF ihParentQuery:TYPE = "browse" THEN 
    ihParentQuery:REFRESH().
END.
ELSE DO:
  IF icAction = "new" THEN DO:
    hParentBuffer:BUFFER-CREATE().
    rRepos = hParentBuffer:ROWID.
    IF ihParentQuery:TYPE = "browse" THEN DO:
      IF ihParentQuery:QUERY:PREPARE-STRING = ? THEN
        ihParentQuery:QUERY:QUERY-PREPARE("FOR EACH " + ihParentQuery:QUERY:GET-BUFFER-HANDLE(1):NAME).
      ihParentQuery:QUERY:QUERY-OPEN().
      ihParentQuery:SET-REPOSITIONED-ROW(ihParentQuery:DOWN,"conditional").
      ihParentQuery:QUERY:REPOSITION-TO-ROWID(rRepos).
    END.
    ELSE IF ihParentQuery:IS-OPEN THEN DO:
      ihParentQuery:QUERY-OPEN(). 
      ihParentQuery:REPOSITION-TO-ROWID(rRepos).
      ihParentQuery:GET-NEXT().
    END.
    hChildQueryObj =  DYNAMIC-FUNCTION("GetLinkedObject",ihParentQuery,"parent","to").
    IF VALID-HANDLE(hChildQueryObj) THEN DO:
      IF hChildQueryObj:TYPE = "browse" THEN
        hChildQueryObj:QUERY:QUERY-CLOSE().
      ELSE
        hChildQueryObj:QUERY-CLOSE().
    END.
    setAttribute(ihParentQuery,"rowsadded",STRING(INT(getAttribute(ihParentQuery,"rowsadded")) + 1)).
    DYNAMIC-FUNCTION("ViewRecordCount",ihParentQuery).
  END.
  ELSE DO:
    rRepos = hParentBuffer:ROWID.
    FIND FIRST bttObjectLink 
         WHERE bttObjectLink.hToObject   = ihParentQuery
           AND bttObjectLink.hFromObject = ihSourceQuery
         NO-ERROR.
    IF AVAIL bttObjectLink THEN DO:
      cChildLinkInfo = bttObjectLink.cLinkInfo.
      DO ix = 1 TO NUM-ENTRIES(cChildLinkInfo):
        cFindString = cFindString + ENTRY((IF NUM-ENTRIES(cChildLinkInfo,";") = 2 THEN 2 ELSE 1),ENTRY(ix,cChildLinkInfo),";") + " = " +
                      (IF NUM-ENTRIES(ENTRY(ix,cChildLinkInfo),";") = 1 THEN
                        (IF hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'HANDLE' THEN 
                          'WIDGET-HANDLE("' + STRING(hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                         ELSE IF hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'CHARACTER' THEN 
                          '"' + hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE + '"'
                         ELSE
                          hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE + '("' +
                          STRING(hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                         )
                       ELSE
                        (IF hParentBuffer:BUFFER-FIELD(ENTRY(2,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'HANDLE' THEN 
                          'WIDGET-HANDLE("' + STRING(hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                         ELSE IF hParentBuffer:BUFFER-FIELD(ENTRY(2,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE = 'CHARACTER' THEN 
                          '"' + hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE + '"'
                         ELSE
                          hParentBuffer:BUFFER-FIELD(ENTRY(2,ENTRY(ix,cChildLinkInfo),";")):DATA-TYPE + '("' +
                          STRING(hChildBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,cChildLinkInfo),";")):BUFFER-VALUE) + '")'
                        )
                      )
                    + " AND ".
      END.
      cFindString = TRIM(RIGHT-TRIM(cFindString," AND")).
    END.
    IF cFindString NE "" THEN DO:
      bOK = hParentBuffer:FIND-FIRST("WHERE " + cFindString) NO-ERROR.
      IF NOT bOk THEN RETURN NO.
    END.
  END.

  IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"MyRefreshParentRecord") THEN DO:
    RUN MyRefreshParentRecord IN hCurrSourceProc (ihSourceQuery:GET-BUFFER-HANDLE(1),hParentBuffer,icAction,OUTPUT bOk).
    IF NOT bOk THEN RETURN FALSE.
  END.
  ELSE hParentBuffer:BUFFER-COPY(ihSourceQuery:GET-BUFFER-HANDLE(1),"RowCount").

  IF ihParentQuery:TYPE = "browse" THEN DO:
    iFocusedRow = ihParentQuery:FOCUSED-ROW.
    IF rRepos NE hParentBuffer:ROWID THEN
      ihParentQuery:QUERY:REPOSITION-TO-ROWID(hParentBuffer:ROWID).
    IF ihParentQuery:MULTIPLE THEN ihParentQuery:SELECT-ROW(ihParentQuery:FOCUSED-ROW).
    RefreshRowids(ihParentQuery,ihParentQuery:QUERY:GET-BUFFER-HANDLE():BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
/*     DO ix = 1 TO ihParentQuery:NUM-COLUMNS:                                                                                                         */
/*       ihParentQuery:GET-BROWSE-COLUMN(ix):SCREEN-VALUE = STRING(hParentBuffer:BUFFER-FIELD(ihParentQuery:GET-BROWSE-COLUMN(ix):NAME):BUFFER-VALUE). */
/*     END.                                                                                                                                            */
    FIND FIRST bttObject WHERE bttObject.hObject = ihParentQuery NO-ERROR.
    IF AVAIL bttObject AND CAN-DO(bttObject.hSourceProc:INTERNAL-ENTRIES,"RowDisplayBrowse") THEN
      RUN RowDisplayBrowse IN bttObject.hSourceProc.   

    IF rRepos NE hParentBuffer:ROWID THEN DO:
      ihParentQuery:SET-REPOSITIONED-ROW(iFocusedRow,"conditional").
      ihParentQuery:QUERY:REPOSITION-TO-ROWID(rRepos).
      ihParentQuery:SELECT-ROW(ihParentQuery:FOCUSED-ROW).
    END.
  END.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RefreshRowids) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RefreshRowids Procedure 
FUNCTION RefreshRowids RETURNS INTEGER
  ( INPUT ihQueryObject    AS HANDLE,
    INPUT icRowidList      AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  iStartRow = -1: Keep already read records in query
------------------------------------------------------------------------------*/
DEF VAR httTable          AS HANDLE NO-UNDO.
DEF VAR httTableBuffer    AS HANDLE NO-UNDO.
DEF VAR httTableQuery     AS HANDLE NO-UNDO.

DEF VAR cQueryString      AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR cFindString       AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR iNumRowIds        AS INT    NO-UNDO.
DEF VAR iCount            AS INT    NO-UNDO.
DEF VAR hField            AS HANDLE NO-UNDO.
DEF VAR cTemp             AS CHAR   NO-UNDO.
DEF VAR cModBuffsAndFlds  AS CHAR   NO-UNDO.
DEF VAR cModQueryJoin     AS CHAR   NO-UNDO.
DEF VAR cModBaseQuery     AS CHAR   NO-UNDO.
DEF VAR cModSkipUnique    AS CHAR   NO-UNDO.
DEF VAR cClientBufferList AS CHAR   NO-UNDO.
DEF VAR cServerBufferList AS CHAR   NO-UNDO.
DEF VAR cRowIdentIdxList  AS CHAR   NO-UNDO.
DEF VAR cPrimaryBuffer    AS CHAR   NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"uselocaldata") NE "yes" THEN DO:

  icRowIdList = TRIM(icRowIdList,",").
  IF icRowIdList = "" THEN RETURN 0.
  
  IF ihQueryObject:TYPE = "QUERY" THEN
    hBuffer = ihQueryObject:GET-BUFFER-HANDLE(1).
  ELSE
    hBuffer = ihQueryObject:QUERY:GET-BUFFER-HANDLE(1).
  
  /* If the buffer sequence has been modified for query speed, swap back to design sequence here: */
  IF getAttribute(ihQueryObject,"orgbuffersandfields") NE "" THEN DO:
    ASSIGN cModBuffsAndFlds = getAttribute(ihQueryObject,"buffersandfields")
           cModQueryJoin    = getAttribute(ihQueryObject,"queryjoin")
           cModBaseQuery    = getAttribute(ihQueryObject,"basequery")
           cModSkipUnique   = getAttribute(ihQueryObject,"skipuniquerows")
           .
    setAttribute(ihQueryObject,"SkipUniqueRows","").
    setAttribute(ihQueryObject,"buffersandfields",getAttribute(ihQueryObject,"orgbuffersandfields")).
    setAttribute(ihQueryObject,"queryjoin",getAttribute(ihQueryObject,"orgqueryjoin")).
    setAttribute(ihQueryObject,"basequery",getAttribute(ihQueryObject,"orgbasequery")).
  END.
  cPrimaryBuffer = ENTRY(1,ENTRY(1,getAttribute(ihQueryObject,"buffersandfields")),";").

  IF NUM-ENTRIES(icRowidList) LE 250  THEN DO:
    DO ix = 1 TO NUM-ENTRIES(icRowIdList):
      cQueryString = cQueryString + "ROWID(" + cPrimaryBuffer + ") = TO-ROWID('" + ENTRY(ix,icRowIdList) + "') OR ".
    END.
    cQueryString = SUBSTR(cQueryString,1,LENGTH(cQueryString) - 3).
  END.
  
  cTemp = DYNAMIC-FUNCTION("AddCalcParam",getAttribute(ihQueryObject,"BuffersAndFields"),ihQueryObject).
  DYNAMIC-FUNCTION("setCalcFieldProc",getAttribute(ihQueryObject,"calcfieldproc")).  
  httTable = DYNAMIC-FUNCTION("getTempTableJoin",10000,0,"",
                              cTemp,
                              (IF NUM-ENTRIES(icRowidList) > 250 THEN
                                "WHERE CAN-DO('" + TRIM(icRowidList,",") + "',STRING(ROWID(" + cPrimaryBuffer + ")))"
                               ELSE  
                                 cQueryString)
                             + getAttribute(ihQueryObject,"queryjoin")
                              ).
  
  IF DYNAMIC-FUNCTION("getTransactionMessage") NE "" THEN 
    DoMessage (0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"JukeBox, Query error:","").
  
  httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httTableQuery NO-ERROR.
  httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
  httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
  httTableQuery:QUERY-OPEN.
  IF getAttribute(ihQueryObject,"orgbuffersandfields") NE "" THEN 
    ASSIGN cClientBufferList = getAttribute(ihQueryObject,"modbufferlist")
           cServerBufferList = cClientBufferList
           .
  ELSE 
    ASSIGN cClientBufferList = getAttribute(ihQueryObject,"bufferlist")
           cServerBufferList = getAttribute(ihQueryObject,"bufferlist")
           .

  DO ix = 1 TO NUM-ENTRIES(cClientBufferList):
    IF getAttribute(ihQueryObject,"bufferjoin" + ENTRY(ix,cClientBufferList)) MATCHES "*OUTER-JOIN*" THEN LEAVE.
    cRowIdentIdxList = cRowIdentIdxList + (IF cRowIdentIdxList NE "" THEN "," ELSE "") + STRING(LOOKUP(ENTRY(ix,cClientBufferList),cServerBufferList)).
  END.
  
  httTableQuery:GET-FIRST().
  REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:
    cFindString   = "WHERE ".
  
    DO ix = 1 TO NUM-ENTRIES(cRowIdentIdxList):
      cFindString = cFindString + "RowIdent" + ENTRY(ix,cRowIdentIdxList) + " = '" + httTableBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)):BUFFER-VALUE + "' AND ".
    END.
    cFindString = RIGHT-TRIM(cFindString," AND ").
  
    bOK = FALSE.
    DO WHILE NOT bOK AND cFindString NE "WHERE":
      bOk = hBuffer:FIND-UNIQUE(cFindString) NO-ERROR.
      IF NOT bOK THEN DO:
        IF R-INDEX(cFindString,"AND") > 0 THEN
          cFindString = SUBSTR(cFindString,1,R-INDEX(cFindString,"AND") - 1).
        ELSE cFindString = "WHERE".
        IF cFindString NE "WHERE" THEN
          bOk = hBuffer:FIND-UNIQUE(cFindString) NO-ERROR.
      END.
    END.
    IF bOk THEN DO:
      hBuffer:BUFFER-COPY(httTableBuffer,"RowCount").
      iCount = iCount + 1.
    END.
    httTableQuery:GET-NEXT().
  END.

  DELETE OBJECT httTableQuery.
  DELETE OBJECT httTable.

END.
ELSE bOk = YES.

IF ihQueryObject:TYPE = "BROWSE" THEN DO:
  IF NUM-ENTRIES(icRowidList) > 1 THEN
    ihQueryObject:REFRESH().
  ELSE IF bOK THEN DO:
    IF ihQueryObject:MULTIPLE THEN DO:
      IF ihQueryObject:FOCUSED-ROW > 0 THEN
        ihQueryObject:SELECT-ROW(ihQueryObject:FOCUSED-ROW).
      ELSE RETURN 0.
    END.
    DisplayRow(ihQueryObject).
    FIND FIRST bttObject WHERE bttObject.hObject = ihQueryObject NO-ERROR.
    IF AVAIL bttObject AND CAN-DO(bttObject.hSourceProc:INTERNAL-ENTRIES,"RowDisplayBrowse") THEN
      RUN RowDisplayBrowse IN bttObject.hSourceProc.   
  END.
END.

IF cModBuffsAndFlds NE "" THEN DO:
  setAttribute(ihQueryObject,"buffersandfields",cModBuffsAndFlds).
  setAttribute(ihQueryObject,"queryjoin",cModQueryJoin).
  setAttribute(ihQueryObject,"basequery",cModBaseQuery).
  setAttribute(ihQueryObject,"skipuniquerows",cModSkipUnique).
END.

RETURN iCount.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseExcelHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReleaseExcelHandle Procedure 
FUNCTION ReleaseExcelHandle RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RELEASE OBJECT chExcelApplication NO-ERROR.
chExcelApplication = ?.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveUseIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RemoveUseIndex Procedure 
FUNCTION RemoveUseIndex RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cOldQuery AS CHAR NO-UNDO.
DEF VAR cRevQuery AS CHAR NO-UNDO.
DEF VAR iy        AS INT  NO-UNDO.

IF getAttribute(ihQueryObject,"keepuseindex") = "yes" THEN RETURN NO.

cOldQuery = getAttribute(ihQueryObject,"basequery").
IF cOldQuery MATCHES "*USE-INDEX*" THEN DO:
  DO iy = 2 TO NUM-ENTRIES(cOldQuery," "):
    IF ENTRY(iy,cOldQuery," ") = "USE-INDEX" OR ENTRY(iy - 1,cOldQuery," ") = "USE-INDEX" THEN NEXT.
    cRevQuery = cRevQuery + ENTRY(iy,cOldQuery," ") + " ".
  END.
  setAttribute(ihQueryObject,"basequery",ENTRY(1,cOldQuery," ") + " " + cRevQuery).
END.
cOldQuery = getAttribute(ihQueryObject,"queryfilter").
IF cOldQuery MATCHES "*USE-INDEX*" THEN DO:
  DO iy = 2 TO NUM-ENTRIES(cOldQuery," "):
    IF ENTRY(iy,cOldQuery," ") = "USE-INDEX" OR ENTRY(iy - 1,cOldQuery," ") = "USE-INDEX" THEN NEXT.
    cRevQuery = cRevQuery + ENTRY(iy,cOldQuery," ") + " ".
  END.
  setAttribute(ihQueryObject,"queryfilter",ENTRY(1,cOldQuery," ") + " " + cRevQuery).
END.
cOldQuery = getAttribute(ihQueryObject,"querywhere").
IF cOldQuery MATCHES "*USE-INDEX*" THEN DO:
  DO iy = 2 TO NUM-ENTRIES(cOldQuery," "):
    IF ENTRY(iy,cOldQuery," ") = "USE-INDEX" OR ENTRY(iy - 1,cOldQuery," ") = "USE-INDEX" THEN NEXT.
    cRevQuery = cRevQuery + ENTRY(iy,cOldQuery," ") + " ".
  END.
  setAttribute(ihQueryObject,"querywhere",ENTRY(1,cOldQuery," ") + " " + cRevQuery).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveModified) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveModified Procedure 
FUNCTION SaveModified RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Check if a fieldMap (viewer) is changed 
    Notes: Input parameter is the corresponding query (browse) 
------------------------------------------------------------------------------*/
DEF VAR rRowid            AS ROWID  NO-UNDO.
DEF VAR rCurrRowid        AS ROWID  NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hFieldMapModified AS HANDLE NO-UNDO.

DEF BUFFER buffToolbar    FOR ttObject.
DEF BUFFER bttObject      FOR ttObject.
DEF BUFFER bttObjectLink  FOR ttObjectLink.
DEF BUFFER bbttObjectLink FOR ttObjectLink.

FIND FIRST bttObjectLink
     WHERE bttObjectLink.hFromObject = ihObject
       AND bttObjectLink.cLinkType   = "fieldMap"
     NO-ERROR.

IF NOT AVAIL bttObjectLink THEN DO:
  FIND FIRST bbttObjectLink 
       WHERE bbttObjectLink.hToObject = ihObject
         AND bbttObjectLink.cLinkType = "onetoone"
       NO-ERROR.
  IF AVAIL bbttObjectLink THEN
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = bbttObjectLink.hFromObject
           AND bttObjectLink.cLinkType   = "fieldMap"
         NO-ERROR.
  ELSE DO:
    FIND FIRST bbttObjectLink 
         WHERE bbttObjectLink.hToObject = ihObject
           AND bbttObjectLink.cLinkType = "parent"
         NO-ERROR.
    IF AVAIL bbttObjectLink THEN DO:
      FIND FIRST bttObjectLink
           WHERE bttObjectLink.hFromObject = bbttObjectLink.hFromObject
             AND bttObjectLink.cLinkType   = "fieldMap"
           NO-ERROR.
      IF NOT AVAIL bttObjectLink THEN DO:
        FIND FIRST bbttObjectLink 
             WHERE bbttObjectLink.hToObject = ihObject
               AND bbttObjectLink.cLinkType = "parent"
             NO-ERROR.
        IF AVAIL bbttObjectLink THEN 
          FIND FIRST bttObjectLink
               WHERE bttObjectLink.hFromObject = bbttObjectLink.hFromObject
                 AND bttObjectLink.cLinkType   = "fieldMap"
               NO-ERROR.
      END.
    END.
  END.
END.
ELSE IF NOT CAN-FIND(FIRST bbttObjectLink 
                     WHERE bbttObjectLink.hFromObject = ihObject
                       AND bbttObjectLink.cLinkType = "onetoone") THEN DO:
  rRowid = TO-ROWID(getAttribute(ihObject,"currentrowid")) NO-ERROR.
  IF ihObject:TYPE = "browse" THEN
    ASSIGN hBuffer = ihObject:QUERY:GET-BUFFER-HANDLE(1)
           hBrowse = ihObject
           hQuery  = hBrowse:QUERY.
  ELSE IF ihObject:TYPE = "query" THEN
    ASSIGN hBuffer = ihObject:GET-BUFFER-HANDLE(1)
           hQuery  = ihObject.
  ELSE RETURN YES.

END.

IF AVAIL bttObjectLink 
   AND getAttribute(bttObjectLink.hToObject,"checkmodified") NE "never"
   AND getAttribute(bttObjectLink.hToObject,"checkmodified") NE "no" THEN DO:

  /* Set to be able to find correct fieldMap object in CheckModified */
  hFieldMapModified = bttObjectLink.hToObject.

  FIND bttObject WHERE bttObject.hObject = bttObjectLink.hToObject.
  FIND bttObjectLink 
       WHERE bttObjectLink.hFromObject = bttObject.hObject
         AND bttObjectLink.cLinkType = "toolbar"
         NO-ERROR.
  IF AVAIL bttObjectLink THEN
    FIND buffToolbar WHERE buffToolbar.hObject = bttObjectLink.hToObject.

  IF AVAIL bttObjectLink 
     AND (CheckModified(hFieldMapModified,"check") OR buffToolbar.cState = "modified") THEN DO:
     
    IF DoMessage (0,4,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Lagre endringer?" ELSE "Save changes?"
               ,"","") = 6 THEN DO: 

      IF rRowId NE ? THEN DO:
        rCurrRowid = hBuffer:ROWID.
        bOk = hBuffer:FIND-BY-ROWID(rRowid) NO-ERROR.
        IF bOk THEN DO: /* Reposition, save, reposition: */
          IF VALID-HANDLE(hBrowse) THEN DO:
            hBrowse:DESELECT-FOCUSED-ROW().
            hBrowse:SET-REPOSITIONED-ROW(hBrowse:FOCUSED-ROW,"conditional").
          END.
          hQuery:REPOSITION-TO-ROWID(rRowid).
          IF VALID-HANDLE(hBrowse) THEN 
            hBrowse:SELECT-FOCUSED-ROW().
  
          ApplyEvent(bttObjectLink.hToObject,"save").
  
          bOk = hBuffer:FIND-BY-ROWID(rCurrRowid) NO-ERROR.
          IF bOk THEN DO:
            IF VALID-HANDLE(hBrowse) THEN DO:
              hBrowse:DESELECT-FOCUSED-ROW().
              hBrowse:SET-REPOSITIONED-ROW(hBrowse:FOCUSED-ROW,"conditional").
            END.
            hQuery:REPOSITION-TO-ROWID(rCurrRowid).
            IF VALID-HANDLE(hBrowse) THEN 
              hBrowse:SELECT-FOCUSED-ROW().
          END.
        END.
      END.
      ELSE DO:
        ApplyEvent(bttObjectLink.hToObject,"save").
        setAttribute(bttObject.hObject,"checkmodified","no").
      END.
    END.
    ELSE DO:
      setAttribute(bttObject.hObject,"checkmodified","no").
      RETURN NO.
    END.
  END.
END.

hFieldMapModified = ?.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setApplyLastKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setApplyLastKey Procedure 
FUNCTION setApplyLastKey RETURNS LOGICAL
  ( INPUT ibApplyLastkey AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bApplyLastkey = ibApplyLastkey.
IF ibApplyLastkey THEN
  bMyReturnNoApply = YES.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAttribute Procedure 
FUNCTION setAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLogField AS CHAR NO-UNDO.
cLogField = (IF CAN-QUERY(ihObject,"name") THEN ihObject:NAME + " - " ELSE "") + icName + " - " + icValue.

{incl/methodlog.i cLogField}
IF getAttribute(SESSION,"debugattribute") = icName THEN
  MESSAGE "Attrbute " icName SKIP
          "Value " icValue SKIP
          "Stack: " PROGRAM-NAME(2) SKIP
          "Stack: " PROGRAM-NAME(3) SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

FIND FIRST ttAttribute 
     WHERE ttAttribute.hObject = ihObject
       AND ttAttribute.cName   = icName
     NO-ERROR.
IF NOT AVAIL ttAttribute THEN DO:
  CREATE ttAttribute.
  ASSIGN ttAttribute.hObject = ihObject
         ttAttribute.cName   = icName.
END.
IF icName = "querywhere" AND icValue NE ttAttribute.cValue THEN DO:
  ttAttribute.cValue = icValue.
  RETURN TRUE.
END.
ELSE IF icName = "buffersandfields" THEN DO:
  ttAttribute.cValue = REPLACE(icValue,"!","").
  RETURN TRUE.
END.

ttAttribute.cValue = icValue.

IF icName BEGINS "BtnImg_" OR icName MATCHES "Passive*Button" THEN
  DYNAMIC-FUNCTION("setDefaultButton",icName,icValue).


RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrowseSearchField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBrowseSearchField Procedure 
FUNCTION setBrowseSearchField RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT ihSortColumn AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Change browse search field according to current sort column 
    Notes: Search field doesn't apply when multiple sort columns 
------------------------------------------------------------------------------*/
/* DEF VAR bDesc           AS LOG NO-UNDO.  */
/* DEF VAR cSortMap        AS CHAR NO-UNDO. */
DEF VAR cNoColumnSearch AS CHAR   NO-UNDO.
DEF VAR hTmpObj-2       AS HANDLE NO-UNDO.
DEF VAR hTmpObj-3       AS HANDLE NO-UNDO.
DEF VAR hTmpXpixels     AS INT    NO-UNDO.
DEF VAR hTmpYpixels     AS INT    NO-UNDO.
DEF VAR iTmpXpixels     AS INT    NO-UNDO.
DEF VAR iTmpYpixels     AS INT    NO-UNDO.
DEF VAR hField          AS HANDLE NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR bNewSearchField AS LOG    NO-UNDO.

ASSIGN iTmpXpixels = ihBrowse:FRAME:WIDTH-PIXELS
       iTmpYpixels = ihBrowse:FRAME:HEIGHT-PIXELS
       .

cNoColumnSearch = getAttribute(ihBrowse,"NoColumnSearch").
FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ihBrowse
       AND ttObjectLink.cLinkType   = "browse-search-field"
     NO-ERROR.
IF AVAIL ttObjectLink AND VALID-HANDLE(ttObjectLink.hToObject) THEN DO:
  IF NOT VALID-HANDLE(ihSortColumn) THEN DO:
    ttObjectLink.hToObject:HIDDEN = YES.
    RETURN NO.
  END.
  
  IF ttObjectLink.hToObject NE ihSortColumn AND 
     getAttribute(ihBrowse,"keepsearchvalue") = "yes" THEN
    setAttribute(ihBrowse,"querywhere","").

  ttObjectLink.hToObject:HIDDEN = NO.
  
  hField = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ihSortColumn:NAME).

 IF (ttObjectLink.hToObject:TYPE = "radio-set" AND hField:DATA-TYPE NE "logical") OR
    (ttObjectLink.hToObject:TYPE NE "radio-set" AND hField:DATA-TYPE = "logical") THEN
   bNewSearchField = YES.

  DoLockWindow(ihBrowse:WINDOW).
  IF NOT CAN-DO(cNoColumnSearch,ihSortColumn:NAME) OR getAttribute(ihBrowse,"uselocaldata") = "yes" THEN DO:
    IF hField:DATA-TYPE NE ttObjectLink.hToObject:DATA-TYPE OR bNewSearchField THEN DO:
      FIND bttObject WHERE bttObject.hObject = ttObjectLink.hToObject.
      ASSIGN hTmpObj-2   = bttObject.hDesignObject
             hTmpXpixels = hTmpObj-2:FRAME:WIDTH-PIXELS
             hTmpYpixels = hTmpObj-2:FRAME:HEIGHT-PIXELS
             .
      IF VALID-HANDLE(hCurrSourceProc) THEN
        DYNAMIC-FUNCTION("setObjectSourceProc",hCurrSourceProc).
      ELSE
        DYNAMIC-FUNCTION("setObjectSourceProc",bttObject.hSourceProc).
      DYNAMIC-FUNCTION("DeleteObject",bttObject.hObject).
      DO ix = 1 TO ihBrowse:NUM-COLUMNS:
        IF ihBrowse:GET-BROWSE-COLUMN(ix):HANDLE = ihSortColumn THEN 
          hTmpObj-3 = DYNAMIC-FUNCTION("NewBrowseSearchField",hTmpObj-2,ihBrowse,ix).
      END.
      DYNAMIC-FUNCTION("CreateObjectLink",ihBrowse,hTmpObj-3).
      IF ihBrowse:FRAME:TYPE NE "dialog-box" THEN
        ASSIGN hTmpObj-2:FRAME:WIDTH-PIXELS  = hTmpXpixels
               hTmpObj-2:FRAME:HEIGHT-PIXELS = hTmpYpixels
               .
    END.
    ELSE DO:
      ASSIGN hTmpObj-3 = ttObjectLink.hToObject
/*              hTmpObj-3:FORMAT = ihSortColumn:FORMAT */
             hTmpObj-3:HELP   = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Søk " ELSE "Search ") 
                              + DYNAMIC-FUNCTION("getStrippedSortLabel",hTmpObj-3) 
             .        
      IF hTmpObj-3:DATA-TYPE NE "logical" AND hTmpObj-3:TYPE NE "radio-set" THEN
        ASSIGN hTmpObj-3:SCREEN-VALUE = ""
               hTmpObj-3:FORMAT = ihSortColumn:FORMAT
               .
      ELSE DO:
        IF hTmpObj-3:TYPE = "toggle-box" THEN
          ASSIGN hTmpObj-3:CHECKED = NO
                 hTmpObj-3:LABEL   = DYNAMIC-FUNCTION("getStrippedSortLabel",ihSortColumn).
        ELSE IF hTmpObj-3:TYPE = "radio-set" THEN
          hTmpObj-3:SCREEN-VALUE = ?.
      END.
    END.

    ASSIGN hTmpObj-3:VISIBLE   = TRUE
           hTmpObj-3:MODIFIED  = FALSE
           hTmpObj-3:NAME      = ihSortColumn:NAME
           NO-ERROR.

    DYNAMIC-FUNCTION("setSearchFieldLinkInfo",ihBrowse,ihSortColumn:NAME).

    IF CAN-DO(getAttribute(ihBrowse,"SearchToFilterDataTypes"),hTmpObj-3:DATA-TYPE) THEN DO:
      IF getAttribute(ihBrowse,"filtervalue_" + hTmpObj-3:NAME) NE "" THEN
        hTmpObj-3:SCREEN-VALUE = getAttribute(ihBrowse,"filtervalue_" + hTmpObj-3:NAME).
      ELSE
        hTmpObj-3:SCREEN-VALUE = getAttribute(ihBrowse,"filtervalue_2_" + hTmpObj-3:NAME).
    END.

    APPLY "entry" TO hTmpObj-3.
  END.
  ELSE
    ttObjectLink.hToObject:VISIBLE = FALSE.

  DoLockWindow(?).
END.
ELSE IF AVAIL ttObjectLink THEN DO:
/*   MESSAGE PROGRAM-NAME(1) SKIP  */
/*           PROGRAM-NAME(2)       */
/*           VIEW-AS ALERT-BOX.    */
  DYNAMIC-FUNCTION("DeleteObject",ttObjectLink.hToObject).
END.

IF ihBrowse:FRAME:TYPE NE "dialog-box" THEN
  ASSIGN ihBrowse:FRAME:WIDTH-PIXELS  = iTmpXpixels
         ihBrowse:FRAME:HEIGHT-PIXELS = iTmpYpixels.
  
IF VALID-HANDLE(hCurrWindow) AND getAttribute(ihBrowse,"startsearch_done") NE "no" THEN
  APPLY "end-resize" TO hCurrWindow.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrwOverlayBGcolNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBrwOverlayBGcolNum Procedure 
FUNCTION setBrwOverlayBGcolNum RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT iiColNum AS INT    ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ihBrowse
      AND ttObjectLink.cLinkType   = "browseoverlay":
  ttObjectLink.hToObject:BGCOLOR = iiColNum.
END.
setAttribute(ihBrowse,"overlaybgcolnum",STRING(iiColNum)).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCancelNextEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCancelNextEvent Procedure
FUNCTION setCancelNextEvent RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
iCancelNextEvents = iCancelNextEvents + 1.

RETURN YES.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setCurrentObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCurrentObject Procedure 
FUNCTION setCurrentObject RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttObject WHERE ttObject.hObject = ihObject NO-ERROR.
IF AVAIL ttObject THEN
  RETURN TRUE.
ELSE DO:
  MESSAGE "Unsuccessful setCurrentObject method. Object not available" SKIP(1)
           PROGRAM-NAME(1) SKIP
           PROGRAM-NAME(2) 
           VIEW-AS ALERT-BOX ERROR TITLE "JukeBox programmers error".
  RETURN FALSE.   
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrentSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCurrentSourceProc Procedure 
FUNCTION setCurrentSourceProc RETURNS LOGICAL
  ( INPUT ihProc AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hCurrSourceProc = ihProc.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrentWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCurrentWidget Procedure 
FUNCTION setCurrentWidget RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Force focus to a certain widget if the event has RETURN NO-APPLY 
    Notes: F.ex used to retain focus in a window if an external method is called after super in SaveRecord 
------------------------------------------------------------------------------*/
hCurrWidget = ihWidget.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDynFilter Procedure 
FUNCTION setDynFilter RETURNS LOGICAL
  ( INPUT icAction      AS CHAR,
    INPUT ihFilter      AS HANDLE,
    INPUT ihQueryObject AS HANDLE,
    INPUT icSourceFile  AS CHAR,
    INPUT icUserContext AS CHAR) :

DEF VAR cFltStr            AS CHAR    NO-UNDO.
DEF VAR hQuery             AS HANDLE  NO-UNDO.
DEF VAR hBuffer            AS HANDLE  NO-UNDO.
DEF VAR cInUseList         AS CHAR    NO-UNDO.
DEF VAR iNumInUse          AS INT     NO-UNDO.
DEF VAR cGroupList         AS CHAR    NO-UNDO.
DEF VAR cGroupOperList     AS CHAR    NO-UNDO.
DEF VAR cBufferList        AS CHAR    NO-UNDO.
DEF VAR iy                 AS INT     NO-UNDO.
DEF VAR cPrScQry           AS CHAR    NO-UNDO.
DEF VAR cCurrPreScanQry    AS CHAR    NO-UNDO.
DEF VAR cBaseTableFilter   AS CHAR    NO-UNDO.
DEF VAR cAllCalcFields     AS CHAR    NO-UNDO.
DEF VAR cCalcFieldFilter   AS CHAR    NO-UNDO.
DEF VAR bActiveFilter      AS LOG     NO-UNDO.
DEF VAR cBaseQuery         AS CHAR    NO-UNDO.
DEF VAR cQueryFilter       AS CHAR    NO-UNDO.
DEF VAR iz                 AS INT     NO-UNDO.
DEF VAR ix                 AS INT     NO-UNDO.
DEF VAR cGrpOper           AS CHAR    NO-UNDO.
DEF VAR cOperInUseList     AS CHAR    NO-UNDO.
DEF VAR cGrpOperList       AS CHAR    NO-UNDO.
DEF VAR cFldGrpLst         AS CHAR    NO-UNDO.
DEF VAR cFldOprLst         AS CHAR    NO-UNDO.
DEF VAR cHdnGrpOperLst     AS CHAR    NO-UNDO.
DEF VAR cFltValLst         AS CHAR    NO-UNDO.
DEF VAR cMainQryCandList   AS CHAR    NO-UNDO.
DEF VAR hNumInUseField     AS HANDLE  NO-UNDO.
DEF VAR cLastPreScan       AS CHAR    NO-UNDO.
DEF VAR bAppendPrescan     AS LOG     NO-UNDO.
DEF VAR bLocal             AS LOG     NO-UNDO.

FIND FIRST ttObject 
     WHERE ttObject.hObject = ihFilter
     NO-ERROR.
IF NOT AVAIL ttObject THEN RETURN ?.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ihQueryObject
       AND ttObjectLink.cLinkType   = "procedure"
     NO-ERROR.
IF AVAIL ttObjectLink THEN
  icSourceFile = ttObjectLink.hToObject:FILE-NAME.

cAllCalcFields = getAttribute(ihQueryObject,"allcalcfields").

ChangePrimarySearchBuffer(ihQueryObject,"","").

bLocal = getAttribute(ihQueryObject,"uselocaldata") = "yes".

IF NOT bLocal THEN 
  cBufferList = getAttribute(ihQueryObject,"bufferlist")
              + (IF getAttribute(ihQueryObject,"extraFilterBuffers") NE "" AND
                    NOT CAN-DO(getAttribute(ihQueryObject,"bufferlist"),getAttribute(ihQueryObject,"extraFilterBuffers")) THEN
                    "," + getAttribute(ihQueryObject,"extraFilterBuffers") ELSE "").
ELSE
  cBufferList = getAttribute(ihQueryObject,"basetable").

DO ix = 1 TO NUM-ENTRIES(cBufferList):
  ASSIGN cFltStr  = ""
         cGroupList     = ""
         cGroupOperList = ""
         cInUseList     = ""
         .

  CREATE QUERY hQuery.
  IF ttObject.hDesignObject:TYPE = "browse" THEN
    CREATE BUFFER hBuffer FOR TABLE ttObject.hDesignObject:QUERY:GET-BUFFER-HANDLE(1).
  ELSE
    CREATE BUFFER hBuffer FOR TABLE ttObject.hDesignObject:GET-BUFFER-HANDLE(1).

  hNumInUseField = hBuffer:BUFFER-FIELD("NumInUse") NO-ERROR.

  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE BufferName = '" + ENTRY(ix,cBufferList) + "' AND FieldGroup NE 0 BY FieldGroup BY GroupOperator DESC BY FieldOperator BY Seq").
  hQuery:QUERY-OPEN().

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    IF NOT CAN-DO(cAllCalcFields,hBuffer::ColumnName) THEN DO:
      IF hBuffer::GroupOperator NE "" THEN
        cGrpOper = hBuffer::GroupOperator.
      IF hBuffer::Operator NE "" AND NOT CAN-DO(cGroupList,STRING(hBuffer::FieldGroup)) THEN 
        ASSIGN cGroupList     = cGroupList + STRING(hBuffer::FieldGroup) + ","
               cGroupOperList = cGroupOperList + cGrpOper + ","
               .
    END.
    hQuery:GET-NEXT().
  END.
  ASSIGN cGroupList     = TRIM(cGroupList,",")
         cGroupOperList = TRIM(cGroupOperList,",")
         .

  DO iy = 1 TO NUM-ENTRIES(cGroupList):
    hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE FieldGroup = " + STRING(ENTRY(iy,cGroupList)) 
                       + " AND Operator NE ''"
                       + (IF getAttribute(ihQueryObject,"uselocaldata") NE "yes" THEN
                           " AND NOT CAN-DO('" + cAllCalcFields + "',ColumnName)"
                          ELSE "")
                         ).   
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    IF hBuffer:AVAIL THEN DO:
      cFltStr = cFltStr + " " + ENTRY(iy,cGroupOperList) + " (". 
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        IF hBuffer::Operator = "IN" AND NUM-ENTRIES(hBuffer::ColumnValue) < 20 THEN DO:
          cFltStr = cFltStr + " " + hBuffer::FieldOperator + " (". 
          DO iz = 1 TO NUM-ENTRIES(hBuffer::ColumnValue):
            cFltStr = cFltStr + (IF hBuffer::DataType NE "CHARACTER" THEN "STRING(" ELSE "")
                              + (IF bLocal THEN hBuffer::ColumnName 
                                 ELSE 
                                   getAttribute(ihQueryObject,"fieldbuffer" + hBuffer::ColumnName) + "."
                                 + getAttribute(ihQueryObject,"orgdbfield" + hBuffer::ColumnName))
                              + (IF hBuffer::DataType NE "CHARACTER" THEN ")" ELSE "")
                              + " = '" + ENTRY(iy,hBuffer::ColumnValue) + "'"
                              + (IF iz < NUM-ENTRIES(hBuffer::ColumnValue) THEN " OR " ELSE ")")
                              .
          END.                    
        END.                     
        ELSE
          cFltStr = cFltStr + " " 
                      + (IF hBuffer::Operator = "IN" THEN 
                           "CAN-DO('" + hBuffer::ColumnValue + "'," 
                         + (IF hBuffer::DataType NE "CHARACTER" THEN "STRING(" ELSE "")
                         ELSE "")
                      + (IF bLocal THEN hBuffer::ColumnName ELSE
                           getAttribute(ihQueryObject,"fieldbuffer" + hBuffer::ColumnName) + "."
                         + getAttribute(ihQueryObject,"orgdbfield" + hBuffer::ColumnName))
                      + (IF hBuffer::Operator NE "IN" THEN 
                           " " + hBuffer::Operator 
                        + (IF hBuffer::DataType = "CHARACTER" THEN " '" + hBuffer::ColumnValue + "'"
                           ELSE " " + hBuffer::DataType + "('" 
                            + (IF hBuffer::ColumnValue NE ? THEN 
                                (IF hBuffer::DataType = "logical" THEN
                                   (IF hBuffer::ColumnValue = hBuffer::CharValueTrue THEN "yes" ELSE "no")
                                 ELSE hBuffer::ColumnValue) 
                               ELSE "?") + "')"
                               )
                        ELSE (IF hBuffer::DataType NE "CHARACTER" THEN "))" ELSE ")")).
        hQuery:GET-NEXT().
        IF hBuffer:AVAIL THEN
          cFltStr = cFltStr + " " + hBuffer::FieldOperator.
      END.
      cFltStr = cFltStr + ")". 
    END.
  END.
  
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE BufferName = '" + ENTRY(ix,cBufferList) + "' BY FieldOperator BY Seq").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    iNumInUse = 0.
    IF CAN-DO(cInUseList,hBuffer::ColumnName) THEN DO:
      DO iy = 1 TO NUM-ENTRIES(cInUseList):
        IF ENTRY(iy,cInUseList) = hBuffer::ColumnName THEN
          iNumInUse = iNumInUse + 1.
      END.
      
      IF VALID-HANDLE(hNumInUseField) THEN
        iNumInUse = hNumInUseField:BUFFER-VALUE.

      IF icAction = "execute" THEN 
        setAttribute(ihQueryObject,
                     "filtervalue_" + STRING(iNumInUse) + "_" + hBuffer::ColumnName,
                     hBuffer::ColumnValue).
      ELSE IF hBuffer::Operator NE "" THEN
        cFltValLst = cFltValLst + STRING(iNumInUse) + "_" + hBuffer::ColumnName + "¤" + hBuffer::ColumnValue + "|".
    END.
    ELSE DO:
      IF icAction = "execute" THEN 
        setAttribute(ihQueryObject,
                     "filtervalue_" + hBuffer::ColumnName,
                     hBuffer::ColumnValue).
      ELSE IF hBuffer::Operator NE "" THEN
        cFltValLst = cFltValLst + hBuffer::ColumnName + "¤" + hBuffer::ColumnValue + "|".
    END.

    IF hBuffer::Operator NE "" AND 
       (hBuffer::FieldGroup = 0 OR CAN-DO(cAllCalcFields,hBuffer::ColumnName)) THEN DO:
      IF CAN-DO(cAllCalcFields,hBuffer::ColumnName) AND getAttribute(ihQueryObject,"uselocaldata") NE "yes" THEN
        cCalcFieldFilter = cCalcFieldFilter 
                         + hBuffer::ColumnName + "¤" 
                         + hBuffer::Operator + "¤"
                         + (IF hBuffer::ColumnValue NE ? THEN hBuffer::ColumnValue ELSE "?") + "|".
      ELSE IF hBuffer::FieldGroup = 0 THEN DO:
        IF hBuffer::Operator = "IN" AND NUM-ENTRIES(hBuffer::ColumnValue) < 20 THEN DO:
          cFltStr = cFltStr + " " + hBuffer::FieldOperator + " (". 
          DO iz = 1 TO NUM-ENTRIES(hBuffer::ColumnValue):
            cFltStr = cFltStr + (IF hBuffer::DataType NE "CHARACTER" THEN "STRING(" ELSE "")
                              + (IF bLocal THEN hBuffer::ColumnName 
                                 ELSE 
                                   getAttribute(ihQueryObject,"fieldbuffer" + hBuffer::ColumnName) + "."
                                 + getAttribute(ihQueryObject,"orgdbfield" + hBuffer::ColumnName))
                              + (IF hBuffer::DataType NE "CHARACTER" THEN ")" ELSE "")
                              + " = '" + ENTRY(iy,hBuffer::ColumnValue) + "'"
                              + (IF iz < NUM-ENTRIES(hBuffer::ColumnValue) THEN " OR " ELSE ")")
                              .
          END.                    
        END.                     
        ELSE
          cFltStr = cFltStr + " " + hBuffer::FieldOperator + " " 
                        + (IF hBuffer::Operator = "IN" THEN 
                             "CAN-DO('" + hBuffer::ColumnValue + "'," 
                           + (IF hBuffer::DataType NE "CHARACTER" THEN "STRING(" ELSE "")
                           ELSE "")
                        + (IF bLocal THEN hBuffer::ColumnName ELSE 
                             getAttribute(ihQueryObject,"fieldbuffer" + hBuffer::ColumnName) + "."
                           + getAttribute(ihQueryObject,"orgdbfield" + hBuffer::ColumnName))
                        + (IF hBuffer::Operator NE "IN" THEN 
                             " " + hBuffer::Operator 
                          + (IF hBuffer::DataType = "CHARACTER" THEN " '" + hBuffer::ColumnValue + "'"
                             ELSE " " + hBuffer::DataType + "('" 
                              + (IF hBuffer::ColumnValue NE ? THEN 
                                  (IF hBuffer::DataType = "logical" THEN
                                     (IF hBuffer::ColumnValue = hBuffer::CharValueTrue THEN "yes" ELSE "no")
                                   ELSE hBuffer::ColumnValue) 
                                 ELSE "?") + "')"
                                 )
                          ELSE (IF hBuffer::DataType NE "CHARACTER" THEN "))" ELSE ")")).
      END.                  
    END.

    IF icAction = "execute" THEN DO:
      setAttribute(ihQueryObject,"OperatorInUse_" + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") +
                   hBuffer::ColumnName,hBuffer::Operator).
      setAttribute(ihQueryObject,"GroupOperator_" + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") +
                   hBuffer::ColumnName,hBuffer::GroupOperator).
      setAttribute(ihQueryObject,"FieldGroup_" + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") +
                   hBuffer::ColumnName,hBuffer::FieldGroup).
      setAttribute(ihQueryObject,"FieldOperator_" + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") +
                   hBuffer::ColumnName,hBuffer::FieldOperator).
      setAttribute(ihQueryObject,"HiddenGroupOperator_" + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") +
                   hBuffer::ColumnName,hBuffer:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE).
    END.
    ELSE IF hBuffer::Operator NE "" THEN 
      ASSIGN 
        cOperInUseList = cOperInUseList + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") + hBuffer::ColumnName + "¤" + hBuffer::Operator + "|"
        cGrpOperList   = cGrpOperList   + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") + hBuffer::ColumnName + "¤" + hBuffer::GroupOperator + "|"
        cFldGrpLst     = cFldGrpLst     + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") + hBuffer::ColumnName + "¤" + hBuffer::FieldGroup + "|"
        cFldOprLst     = cFldOprLst     + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") + hBuffer::ColumnName + "¤" + hBuffer::FieldOperator + "|"
        cHdnGrpOperLst = cHdnGrpOperLst + (IF iNumInUse > 0 THEN STRING(iNumInUse) + "_" ELSE "") + hBuffer::ColumnName + "¤" + hBuffer::Operator + "|"
        .


    cInUseList = cInUseList + hBuffer::ColumnName + ",".
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffer.

  IF cFltStr BEGINS " and" THEN
    cFltStr = SUBSTR(cFltStr,5).
  ELSE IF cFltStr BEGINS " or" THEN DO:
    IF DYNAMIC-FUNCTION("DoMessage",0,4,"The expression:" + CHR(10) + cFltStr + CHR(10) + "for table " + ENTRY(ix,cBufferList) + " is meaningless. Sure you want to do this?","","") = 7 THEN RETURN ?.
    cFltStr = " TRUE" + cFltStr. 
  END.

  IF cFltStr NE "" OR cCalcFieldFilter NE "" THEN bActiveFilter = YES.
  
  IF ix = 1 THEN DO:
    IF icAction = "Execute" THEN
      RemoveUseIndex(ihQueryObject).
                
    IF cFltStr NE "" THEN
      cFltStr = (IF getAttribute(ihQueryObject,"basequery") NE "" THEN " AND (" + cFltStr + ")" ELSE "WHERE " + cFltStr).           
    ELSE
      cFltStr = getAttribute(ihFilter,"defaultfilter").
      
    cBaseTableFilter = cFltStr.
  END.
  ELSE DO:
    IF cFltStr NE "" THEN DO:
      ASSIGN cFltStr        = TRIM(REPLACE(cFltStr,",",CHR(1)))
             bAppendPrescan = getAttribute(ihQueryObject,"appendprescanquery" + ENTRY(ix,cBufferList)) NE "no"
             cBaseQuery     = IF bAppendPrescan THEN TRIM(getAttribute(ihQueryObject,"basequery")) ELSE ""
             cQueryFilter   = IF bAppendPrescan THEN TRIM(cBaseTableFilter) ELSE ""
             .
      IF getAttribute(ihQueryObject,"prescanquery" + ENTRY(ix,cBufferList)) NE "" THEN DO:
        cPrScQry = getAttribute(ihQueryObject,"prescanquery" + ENTRY(ix,cBufferList)).              

        DO iz = 1 TO 5:
          cPrScQry = REPLACE(cPrScQry,"  "," ").
        END.

        ASSIGN cPrScQry = REPLACE(cPrScQry,",FIRST","¤FIRST")
               cPrScQry = REPLACE(cPrScQry,", FIRST","¤FIRST")
               cPrScQry = REPLACE(cPrScQry,",LAST","¤LAST")
               cPrScQry = REPLACE(cPrScQry,", LAST","¤LAST")
               cPrScQry = REPLACE(cPrScQry,",EACH","¤EACH")
               cPrScQry = REPLACE(cPrScQry,", EACH","¤EACH")
               cPrScQry = ENTRY(ix,cBufferList) + " WHERE " + cFltStr + "," + cPrScQry
               .
        DO iz = 1 TO NUM-ENTRIES(cPrScQry,"¤"):
          IF iz < NUM-ENTRIES(cPrScQry,"¤") THEN
            ASSIGN cCurrPreScanQry = cCurrPreScanQry + ENTRY(iz,cPrScQry,"¤") + ","
                   cMainQryCandList = cMainQryCandList + ENTRY(iz,cPrScQry,"¤") + ",".
          ELSE
            cPrScQry = ENTRY(iz,cPrScQry,"¤").
        END.
        cLastPreScan  = cPrScQry.
        ASSIGN cMainQryCandList = cMainQryCandList + cPrScQry + "|"
               cPrScQry = cPrScQry + " " + (IF cBaseQuery   BEGINS "WHERE " AND cLastPreScan MATCHES "* WHERE *" THEN "AND " + SUBSTR(cBaseQuery,7) 
                                            ELSE IF cBaseQuery MATCHES "* WHERE *" AND cLastPreScan MATCHES "* WHERE *" THEN REPLACE(cBaseQuery," WHERE "," AND ")
                                            ELSE cBaseQuery)  
               cPrScQry = cPrScQry + " " + (IF cQueryFilter BEGINS "WHERE " AND cLastPreScan MATCHES "* WHERE *" THEN "AND " + SUBSTR(cQueryFilter,7) ELSE cQueryFilter)  
               .
      END.
      ELSE 
        ASSIGN cPrScQry = ENTRY(ix,cBufferList) + " WHERE " + cFltStr + ",EACH " + getAttribute(ihQueryObject,"basetable") + " NO-LOCK OF " + ENTRY(ix,cBufferList)
               cMainQryCandList = cMainQryCandList + cPrScQry + "|"
               cPrScQry   = cPrScQry + " " + cBaseQuery + " " + cQueryFilter
               .
        ASSIGN cPrScQry = TRIM(cPrScQry)
               cCurrPreScanQry = cCurrPreScanQry + cPrScQry + CHR(28) /* "|" */.
    END.
  END.
END.

IF icAction = "Execute" THEN DO:
  IF getAttribute(ihQueryObject,"viewqueryfilter") NE "" THEN 
    IF DoMessage(0,1,"     Basequery: " + getAttribute(ihQueryObject,"basequery") + CHR(10)
                   + "   Query-filter: " + cBaseTableFilter + CHR(10)
                   + "Query-where: " + getAttribute(ihQueryObject,"querywhere") + CHR(10) + CHR(10)
                   + "    Query-join: " + getAttribute(ihQueryObject,"queryjoin") + CHR(10) + CHR(10)
                   + "Pre-scan queries (filters on joined buffers): " + CHR(10) + REPLACE(cCurrPreScanQry,CHR(28),CHR(10)) + CHR(10) + CHR(10)
                   + "Filters on calculated fields:" + CHR(10) + REPLACE(REPLACE(cCalcFieldFilter,"|",CHR(10)),"¤"," ") 
                  ,"Querystring","") NE 1
       THEN RETURN ?.

  ASSIGN cCalcFieldFilter = REPLACE(cCalcFieldFilter,",",CHR(1))
         cCalcFieldFilter = REPLACE(cCalcFieldFilter,";",CHR(3))
         cCalcFieldFilter = TRIM(cCalcFieldFilter,"|")
         cCurrPreScanQry  = TRIM(cCurrPreScanQry,CHR(28))
         .

  setAttribute(ihQueryObject,"calcfieldfilter",cCalcFieldFilter).
  setAttribute(ihQueryObject,"queryfilter",cBaseTableFilter).
  setAttribute(ihQueryObject,"prescanmainquerycandidates_filter",TRIM(cMainQryCandList,"|")).

  IF SwapPrescanToMainBuffer(ihQueryObject,"") THEN DO:
    cCurrPreScanQry = cMainQryCandList.
    setAttribute(ihQueryObject,"queryfilter","").
    IF getAttribute(ihQueryObject,"viewqueryfilter") NE "" THEN 
      DoMessage(0,0,"NOTE: Analyse of prescan resulted in new main query." + CHR(10)
                  + "(Any other prescan criteria will be added to the main query on the server)" + CHR(10) + CHR(10)  
                  + "     Basequery: " + getAttribute(ihQueryObject,"basequery") + CHR(10)
                  + "   Query-filter: " + getAttribute(ihQueryObject,"queryfilter") + CHR(10)
                  + "Query-where: " + getAttribute(ihQueryObject,"querywhere") + CHR(10) + CHR(10)
                  + "    Query-join: " + getAttribute(ihQueryObject,"queryjoin") + CHR(10) + CHR(10)
                  + "Pre-scan queries (filters on joined buffers): " + CHR(10) + REPLACE(cCurrPreScanQry,CHR(28),CHR(10)) + CHR(10) + CHR(10)
                  + "Filters on calculated fields:" + CHR(10) + REPLACE(REPLACE(cCalcFieldFilter,"|",CHR(10)),"¤"," ") 
                 ,"Querystring","").
  END.
  ELSE IF getAttribute(ihQueryObject,"filtersortonlyonprimarybuffer") = "yes" 
          AND getAttribute(ihQueryObject,"sortbuffer") NE getAttribute(ihQueryObject,"basetable") THEN
    setAttribute(ihQueryObject,"querysort","").

  setAttribute(ihQueryObject,"prescanqueryfilter",cCurrPreScanQry).
  RETURN bActiveFilter.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setUserSetting",
                   icSourceFile,
                   DYNAMIC-FUNCTION("getObjectName",ihQueryObject),
                   getAttribute(ihQueryObject,"usersettingcontext"),
                   "filtervaluelist,operatorinuselist,groupoperatorlist,fieldgrouplist,fieldoperatorlist,hiddengroupoperatorlist,queryfilter,advancedfilter,calcfieldfilter,prescanqueryfilter",
                   IF (cFltValLst NE "" OR cCalcFieldFilter NE "" OR cCurrPreScanQry NE "") THEN
                     TRIM(cFltValLst,"|") + CHR(1) +
                     TRIM(cOperInUseList,"|") + CHR(1) +
                     TRIM(cGrpOperList,"|") + CHR(1) +
                     TRIM(cFldGrpLst,"|") + CHR(1) +
                     TRIM(cFldOprLst,"|") + CHR(1) +
                     TRIM(cHdnGrpOperLst,"|") + CHR(1) +
                     cBaseTableFilter + CHR(1) +
                     getAttribute(ihQueryObject,"advancedfilter") + CHR(1) +
                     cCalcFieldFilter + CHR(1) +
                     cCurrPreScanQry
                   ELSE "delete_setting"
                   ).
  RETURN ?.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEventProcReturnValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEventProcReturnValue Procedure 
FUNCTION setEventProcReturnValue RETURNS LOGICAL
  ( INPUT ihObject      AS HANDLE,
    INPUT icMethod      AS CHAR,
    INPUT icReturnValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Keep a stack of the last 10 RETURN values from event procedures 
    Notes:  
------------------------------------------------------------------------------*/
IF iRetEvtCnt > 50 THEN iRetEvtCnt = 1.
ELSE iRetEvtCnt = iRetEvtCnt + 1.

FIND ttEventProcReturn
     WHERE ttEventProcReturn.iReturnCount = iRetEvtCnt
     NO-ERROR.
IF NOT AVAIL ttEventProcReturn THEN
  CREATE ttEventProcReturn.

ASSIGN ttEventProcReturn.hObject      = ihObject
       ttEventProcReturn.cMethod      = icMethod
       ttEventProcReturn.cReturnValue = icReturnValue
       ttEventProcReturn.iReturnCount = iRetEvtCnt.
  
FIND CURRENT ttEventProcReturn NO-ERROR.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoColumnSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoColumnSort Procedure 
FUNCTION setNoColumnSort RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icColumnList AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Set font-label and indicator for direction 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn        AS HANDLE NO-UNDO.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  hColumn = ihBrowse:GET-BROWSE-COLUMN(ix).

  IF CAN-DO(icColumnList,hColumn:NAME) THEN 
    hColumn:LABEL-FGCOLOR = 7.
  ELSE
    hColumn:LABEL-FGCOLOR = ?.
END.
setAttribute(ihBrowse,"NoColumnSort",icColumnList).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReturnNoApply) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setReturnNoApply Procedure 
FUNCTION setReturnNoApply RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bMyReturnNoApply = YES.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReturnNoApplyMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setReturnNoApplyMethod Procedure 
FUNCTION setReturnNoApplyMethod RETURNS LOGICAL
  ( INPUT icMyReturnNoApplyMethod AS CHAR,
    INPUT ihMyReturnNoApplyMethod AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Set the method and optional handle for where it resides 
    Notes: Method must be a procedure
------------------------------------------------------------------------------*/
ASSIGN cMyReturnNoApplyMethod = icMyReturnNoApplyMethod
       hMyReturnNoApplyMethod = ihMyReturnNoApplyMethod
       bMyReturnNoApply       = YES
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTmpObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTmpObject Procedure 
FUNCTION setTmpObject RETURNS LOGICAL
  ( INPUT ihTmpObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Normally: Reset the temporary object handle
    Notes: Use with care. The temporary object handle is used to keep object information between recursive calls.
           Example of usage is when the relations between queries cannot be described with a parent link and
           you have to define and run multiple OpenQuery procedures within DisplayRecord. In this case you
           need to reset the temporary object handle between each OpenQuery 
------------------------------------------------------------------------------*/
hTmpObject = ihTmpObject.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetCursor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWidgetCursor Procedure 
FUNCTION setWidgetCursor RETURNS LOGICAL
  ( INPUT ihWidgetCursor AS HANDLE,
    INPUT iiCursorOffset AS INT) :
/*------------------------------------------------------------------------------
  Purpose: Use to force end to a widget by issuing a RETURN NO-APPLY to current trigger in process 
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hWidgetEnter     = ihWidgetCursor
       iCursorOffset    = iiCursorOffset
       bMyReturnNoApply = YES.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWidgetEnd Procedure 
FUNCTION setWidgetEnd RETURNS LOGICAL
  ( INPUT ihWidgetEnd AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Use to force end to a widget by issuing a RETURN NO-APPLY to current trigger in process 
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hWidgetEnd       = ihWidgetEnd
       bMyReturnNoApply = YES.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setWidgetEnter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWidgetEnter Procedure 
FUNCTION setWidgetEnter RETURNS LOGICAL
  ( INPUT ihWidgetEnter AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Use to force entry to a widget by issuing a RETURN NO-APPLY to current trigger in process 
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hWidgetEnter     = ihWidgetEnter
       bMyReturnNoApply = YES.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartWindow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StartWindow Procedure 
FUNCTION StartWindow RETURNS HANDLE
  ( INPUT icWindowName AS CHAR,
    INPUT icAction     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Check if window is running and take action as prescribed
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWindow AS HANDLE NO-UNDO.

hWindow = SESSION:FIRST-PROCEDURE.
REPEAT WHILE VALID-HANDLE(hWindow):
  IF hWindow:FILE-NAME = icWindowName THEN
    LEAVE.
  hWindow = hWindow:NEXT-SIBLING.
END.
IF VALID-HANDLE(hWindow) THEN 
  CASE icAction:
    WHEN "close"   THEN APPLY "close" TO hWindow.
    WHEN "restart" THEN DO:
      APPLY "close" TO hWindow.
      RUN VALUE(icWindowName) PERSIST SET hWindow.
    END.
    WHEN "move-to-top" OR WHEN "start" THEN DO:
      hWindow:CURRENT-WINDOW:MOVE-TO-TOP().
      hWindow:CURRENT-WINDOW:WINDOW-STATE = 3.
      RETURN hWindow.
    END.
  END CASE.
ELSE
  CASE icAction:
    WHEN "restart" OR WHEN "start" THEN
      RUN VALUE(icWindowName) PERSIST SET hWindow.
  END CASE.

IF VALID-HANDLE(hWindow) THEN DO: 
  IF CAN-DO(hWindow:INTERNAL-ENTRIES,"InitializeObject") THEN
    RUN InitializeObject IN hWindow.
  IF CAN-DO(hWindow:INTERNAL-ENTRIES,"MoveToTop") THEN
    RUN MoveToTop IN hWindow.
END.

RETURN hWindow.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SwapPrescanToMainBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SwapPrescanToMainBuffer Procedure 
FUNCTION SwapPrescanToMainBuffer RETURNS LOGICAL
  ( INPUT ihQueryObject       AS HANDLE,
    INPUT icPreScanQueryList  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Check if a prescan query should rather be used as primary search for the main query 
    Notes: Called from setDynFilter. Dependent of the attribute 'altprimarybufferlist' for the query object.
           Method is to try to reduce the prescan queries and then check if the remaining subselects
           still are candidates to be primary buffer for the main query.
  Let's explain this with an example from Sports2000:
      Query: FOR EACH order, EACH orderline OF order, FIRST item OF orderline
      Here orderline is a candidate to become the main buffer (if the programmer has put it in the alt.. list)     
      since it potentially might return a huge number of records (all 'sent' orderlines, f.ex)
      However, if there is also a subselect (prescan) on item it is probably more efficient to
      rather keep the prescans since they will be reduced to one query on the server:
      FOR EACH item WHERE .., EACH orderline OF item WHERE ..,EACH order OF orderline
------------------------------------------------------------------------------*/
DEF VAR cAltPrimaryBufferList AS CHAR NO-UNDO.
DEF VAR bIncluded             AS LOG  NO-UNDO.
DEF VAR cBufferList           AS CHAR NO-UNDO.
DEF VAR ix                    AS INT  NO-UNDO.
DEF VAR cPreScanQueryList     AS CHAR NO-UNDO.
DEF VAR bFromFilter           AS LOG  NO-UNDO.
DEF VAR cSortBuffer           AS CHAR NO-UNDO.

/* Candidate list for substitution of primary buffer + any existing prescan query candidates (from filter) */
ASSIGN cAltPrimaryBufferList = getAttribute(ihQueryObject,"altprimarybufferlist")
       cPreScanQueryList     = TRIM(icPreScanQueryList + "|" + getAttribute(ihQueryObject,"prescanmainquerycandidates_filter")
                                                       + "|" + getAttribute(ihQueryObject,"prescanmainquerycandidate_query")
                                    ,"|")
       bFromFilter           = PROGRAM-NAME(2) MATCHES "*filter*"
       cSortBuffer           = getAttribute(ihQueryObject,"sortbuffer")
       .

IF cAltPrimaryBufferList = "" OR getAttribute(ihQueryObject,"basequery") NE "" OR NUM-ENTRIES(cPreScanQueryList,"|") > 1 THEN RETURN NO.

IF getAttribute(ihQueryObject,"queryfilter") NE "" AND NOT getAttribute(ihQueryObject,"queryfilter") MATCHES "* MATCHES *" THEN RETURN NO.
IF getAttribute(ihQueryObject,"querywhere")  NE "" AND NOT getAttribute(ihQueryObject,"querywhere") MATCHES "* MATCHES *" THEN RETURN NO.

EMPTY TEMP-TABLE ttSort1.

DO ix = 1 TO NUM-ENTRIES(cPreScanQueryList,"|"):
  IF CAN-DO(cAltPrimaryBufferList,ENTRY(1,TRIM(ENTRY(ix,cPreScanQueryList,"|"))," ")) THEN DO:
    CREATE ttSort1.
    ASSIGN ttSort1.iSeq   = ix
           ttSort1.cText1 = ENTRY(ix,cPreScanQueryList,"|")   
           ttSort1.cText2 = ENTRY(1,TRIM(ENTRY(ix,cPreScanQueryList,"|"))," ") + "," + /* Primary buffer */
                            getQueryBufferList(ENTRY(ix,cPreScanQueryList,"|"))        /* Other buffers */
           ttSort1.iSeq2  = NUM-ENTRIES(ttSort1.cText2)                                /* Number of buffers */
           .         
  END.
END.

setAttribute(ihQueryObject,"trylocalsort","").

IF CAN-DO(cAltPrimaryBufferList,cSortBuffer) AND
   CAN-FIND(FIRST ttSort1 WHERE ttSort1.cText2 = ENTRY(LOOKUP(cSortBuffer,cAltPrimaryBufferList),cAltPrimaryBufferList)) THEN DO:
  FOR EACH ttSort1
      WHERE ENTRY(1,ttSort1.cText2) = ENTRY(LOOKUP(cSortBuffer,cAltPrimaryBufferList),cAltPrimaryBufferList):
    ChangePrimarySearchBuffer(ihQueryObject,ENTRY(1,ttSort1.cText2),TRIM(SUBSTR(ttSort1.cText1,INDEX(ttSort1.cText1," ")))).
    RETURN YES.
  END.
END.

DO ix = 1 TO NUM-ENTRIES(cAltPrimaryBufferList):
  FOR EACH ttSort1
      WHERE ENTRY(1,ttSort1.cText2) = ENTRY(ix,cAltPrimaryBufferList)
      :
    ChangePrimarySearchBuffer(ihQueryObject,ENTRY(1,ttSort1.cText2),TRIM(SUBSTR(ttSort1.cText1,INDEX(ttSort1.cText1," ")))).
    IF (bFromFilter OR getAttribute(ihQueryObject,"querywhere") = "") AND
       getAttribute(ihQueryObject,"filtersortonlyonprimarybuffer") = "yes"
       AND cSortBuffer NE ENTRY(ix,cAltPrimaryBufferList)
       THEN DO:
      setAttribute(ihQueryObject,"querysort","").
      setAttribute(ihQueryObject,"trylocalsort","yes").
    END.
    RETURN YES.
  END.
END.
  
ChangePrimarySearchBuffer(ihQueryObject,"","").
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

