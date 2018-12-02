&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : JBoxObjLib.p
    Purpose     : Object handling

    Author(s)   : brynjar@chemistry.no
    Created     : 08.12.05
    Notes       : The library was spawned from JBoxUIlib with the purpose
                  to hold functions manipulate the objects themselves                   
                  plus methods that applies to one single object
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR bOK                       AS LOG NO-UNDO.
DEF VAR ix                        AS INT NO-UNDO.    
DEF VAR iReturn                   AS INT NO-UNDO.

/* Behaviour variables: */
DEF SHARED VAR iDefaultSortFont          AS INT  NO-UNDO.
DEF SHARED VAR iDefaultSortColor         AS INT  NO-UNDO.
DEF SHARED VAR cBrowseSearchDefault      AS CHAR NO-UNDO INIT "goto".
DEF SHARED VAR bTabOnReturn              AS LOG  NO-UNDO.
DEF SHARED VAR bSetSortLabel             AS LOG  NO-UNDO.
DEF SHARED VAR cDefActionList            AS CHAR NO-UNDO.
DEF SHARED VAR cDefImageList             AS CHAR NO-UNDO.
DEF SHARED VAR cPassiveFilterButton      AS CHAR NO-UNDO.
DEF SHARED VAR cActiveFilterButton       AS CHAR NO-UNDO.
DEF SHARED VAR cCtrlHotkeyActions        AS CHAR NO-UNDO.
DEF SHARED VAR cCtrlHotkeys              AS CHAR NO-UNDO.
DEF SHARED VAR cAltHotkeyActions         AS CHAR NO-UNDO.
DEF SHARED VAR cAltHotkeys               AS CHAR NO-UNDO.
DEF SHARED VAR bKeepExcel                AS LOG  NO-UNDO INIT TRUE. /* Keep excel running ant try to hook up next report to same instance */
DEF SHARED VAR hCurrSourceProc           AS HANDLE NO-UNDO.
DEF SHARED VAR hCurrWidget               AS HANDLE NO-UNDO.
DEF SHARED VAR hCurrWindow               AS HANDLE NO-UNDO.
DEF SHARED VAR hTmpObject                AS HANDLE NO-UNDO.
DEF SHARED VAR cGlobSecDisabledActions   AS CHAR   NO-UNDO.
DEF SHARED VAR cMarkAsc                  AS CHAR   NO-UNDO INIT " ^". 
DEF SHARED VAR cMarkDesc                 AS CHAR   NO-UNDO INIT " v". 
DEF SHARED VAR iEditBgColor              AS INT    NO-UNDO.
DEF SHARED VAR hPrevWidget               AS HANDLE NO-UNDO.
DEF SHARED VAR hPrevObject               AS HANDLE NO-UNDO.

DEF VAR cBehaviour                AS CHAR   NO-UNDO.

DEF VAR hObjectSourceProc         AS HANDLE NO-UNDO.
DEF VAR hDebugObject              AS HANDLE NO-UNDO.
DEF VAR bSetUseBrowseColumnFormat AS LOG    NO-UNDO.

DEF VAR httTable                  AS HANDLE NO-UNDO.
DEF VAR httTableBuffer            AS HANDLE NO-UNDO.
DEF VAR httTableQuery             AS HANDLE NO-UNDO.

/* Temp-tables to handle events and state for dynamic objects: */
DEF SHARED TEMP-TABLE ttObject          /* Toolbar rectangle, browse, menu, buffer.. */
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
DEF BUFFER bttObject  FOR ttObject.
DEF BUFFER bSecObject FOR ttObject.

DEF SHARED TEMP-TABLE ttObjectLink
    FIELD hFromObject             AS HANDLE
    FIELD hToObject               AS HANDLE
    FIELD cLinkType               AS CHAR
    FIELD cLinkInfo               AS CHAR
    FIELD cInitProc               AS CHAR
    FIELD iSeq                    AS INT
    INDEX idxFrom    hFromObject
    INDEX idxTo      hToObject
    .
DEF BUFFER bttObjectLink  FOR ttObjectLink.
DEF BUFFER bbttObjectLink FOR ttObjectLink.

DEF SHARED TEMP-TABLE ttAttribute     
    FIELD hObject                 AS HANDLE
    FIELD cName                   AS CHAR    /* Display, Update, Input, SortColumn, Desc ... */
    FIELD cValue                  AS CHAR
    INDEX idxObject  hObject cName
    INDEX idxName    cName
    .
DEF BUFFER bttAttribute FOR ttAttribute.
DEF temp-table ttAttributeCopy LIKE ttAttribute.

DEF SHARED TEMP-TABLE ttEvent           /* Button choose, menu item choose, start-search, etc */
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

DEF TEMP-TABLE ttSort2 NO-UNDO
    FIELD iSeq   AS INT
    FIELD cText1 AS CHAR
    FIELD cText2 AS CHAR
    .

DEF TEMP-TABLE ttBufferDef NO-UNDO
    FIELD cBuffer     AS CHAR
    FIELD iBufferNum  AS INT
    FIELD cCriteria   AS CHAR
    INDEX idxBufferNum IS PRIMARY iBufferNum
    .
DEF TEMP-TABLE ttFieldDef NO-UNDO
    FIELD iBufferNum       AS INT
    FIELD cFieldDef        AS CHAR
    FIELD cFieldName       AS CHAR
    FIELD cFieldLabel      AS CHAR
    FIELD cClientFieldName AS CHAR
    FIELD iFieldPos        AS INT
    FIELD bCalculated      AS LOG
    FIELD bVisible         AS LOG
    FIELD iFieldSeq        AS INT
    INDEX idxField IS PRIMARY iBufferNum iFieldSeq
    .
DEF BUFFER bttFieldDef FOR ttFieldDef.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AddBrowseTriggers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddBrowseTriggers Procedure 
FUNCTION AddBrowseTriggers RETURNS LOGICAL PRIVATE
  ( INPUT ihBrowse    AS HANDLE,
    INPUT ihProcedure AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddCalcParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddCalcParam Procedure 
FUNCTION AddCalcParam RETURNS CHARACTER
  ( INPUT icBuffsAndFlds AS CHAR,
    INPUT ihBrwOrQry     AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddEditBtn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddEditBtn Procedure 
FUNCTION AddEditBtn RETURNS CHARACTER
  ( INPUT icActionList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddEvent Procedure 
FUNCTION AddEvent RETURNS LOGICAL
  ( INPUT ihObject     AS HANDLE,
    INPUT ihWidget     AS HANDLE,
    INPUT icAction     AS CHAR,
    INPUT icName       AS CHAR,
    INPUT icWidgetType AS CHAR,
    INPUT icLabel      AS CHAR,
    INPUT icMethod     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AppendAttribute Procedure 
FUNCTION AppendAttribute RETURNS LOGICAL
  ( INPUT ihObject      AS HANDLE,
    INPUT icName        AS CHAR,
    INPUT icValue       AS CHAR,
    INPUT icDelimiter   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AppendFieldMap Procedure 
FUNCTION AppendFieldMap RETURNS LOGICAL
  ( INPUT ihFieldMap            AS HANDLE,
    INPUT ihFrame               AS HANDLE,
    INPUT icBufferUpdateFields  AS CHAR,
    INPUT icScreenUpdateFields  AS CHAR,
    INPUT icBufferDisplayFields AS CHAR,
    INPUT icScreenDisplayFields AS CHAR,
    INPUT icOtherProp           AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-appendList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendList Procedure 
FUNCTION appendList RETURNS CHARACTER
  (INPUT icList AS CHAR,INPUT icItem AS CHAR,INPUT icDelim AS CHAR  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendToolbar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AppendToolbar Procedure 
FUNCTION AppendToolbar RETURNS LOGICAL
      ( INPUT ihToolbar     AS HANDLE,
        INPUT ihRectangle   AS HANDLE,
        INPUT icMenu        AS CHAR,
        INPUT icActionList  AS CHAR,
        INPUT icOtherProp   AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChangeDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ChangeDynFilter Procedure 
FUNCTION ChangeDynFilter RETURNS LOGICAL
  ( INPUT ihQueryObject   AS HANDLE,
    INPUT icFieldList     AS CHAR,
    INPUT icOperatorList  AS CHAR,
    INPUT icValueList     AS CHAR,
    INPUT icAction        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearComboBox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearComboBox Procedure 
FUNCTION ClearComboBox RETURNS LOGICAL
  ( INPUT ihCombo AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearQueryFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearQueryFilter Procedure 
FUNCTION ClearQueryFilter RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ComposeSortString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ComposeSortString Procedure 
FUNCTION ComposeSortString RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE,
    INPUT ibReplace     AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CopyAttributes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CopyAttributes Procedure 
FUNCTION CopyAttributes RETURNS LOGICAL
  ( INPUT ihFrom AS HANDLE,
    INPUT ihTo   AS HANDLE,
    INPUT icMask AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateDotNetDisplayLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateDotNetDisplayLink Procedure 
FUNCTION CreateDotNetDisplayLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateObjectLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateObjectLink Procedure 
FUNCTION CreateObjectLink RETURNS LOGICAL
  ( INPUT ihObject1  AS HANDLE,
    INPUT ihObject2  AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateOneToOneLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateOneToOneLink Procedure 
FUNCTION CreateOneToOneLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateOverlayLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateOverlayLink Procedure 
FUNCTION CreateOverlayLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateParentLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateParentLink Procedure 
FUNCTION CreateParentLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteLinksFrom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteLinksFrom Procedure 
FUNCTION DeleteLinksFrom RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteObject Procedure 
FUNCTION DeleteObject RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteObjectLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteObjectLink Procedure 
FUNCTION DeleteObjectLink RETURNS LOGICAL
  ( INPUT ihObject1  AS HANDLE,
    INPUT ihObject2  AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DispBrwOverlayWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DispBrwOverlayWidgets Procedure 
FUNCTION DispBrwOverlayWidgets RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DisplayFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DisplayFieldMap Procedure 
FUNCTION DisplayFieldMap RETURNS LOGICAL
  ( INPUT ihFieldMap AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCleanUpObjects) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoCleanUpObjects Procedure 
FUNCTION DoCleanUpObjects RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FieldMapFieldsToTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FieldMapFieldsToTT Procedure 
FUNCTION FieldMapFieldsToTT RETURNS HANDLE
  ( INPUT ihFieldMap   AS HANDLE,
    INPUT icExNameList AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillBrowse Procedure 
FUNCTION FillBrowse RETURNS INTEGER
  ( INPUT ihBrowse           AS HANDLE,
    INPUT iBatchSize         AS INT,
    INPUT iStartRow          AS INT,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icSortColumn       AS CHAR,
    INPUT ibDesc             AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillLocalBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillLocalBrowse Procedure 
FUNCTION FillLocalBrowse RETURNS INTEGER
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icSortColumn       AS CHAR,
    INPUT ibDesc             AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillLocalQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillLocalQuery Procedure 
FUNCTION FillLocalQuery RETURNS INTEGER
  ( INPUT ihQuery            AS HANDLE,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillQuery Procedure 
FUNCTION FillQuery RETURNS INTEGER
  ( INPUT ihQuery            AS HANDLE,
    INPUT iBatchSize         AS INT,
    INPUT iStartRow          AS INT,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixQuery Procedure 
FUNCTION FixQuery RETURNS CHARACTER
  ( INPUT icQueryCriteria AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixSortString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixSortString Procedure 
FUNCTION FixSortString RETURNS CHARACTER
  ( INPUT ihQueryObject  AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getActiveFilterButtonFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getActiveFilterButtonFile Procedure 
FUNCTION getActiveFilterButtonFile RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&IF DEFINED(EXCLUDE-getAttributeList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAttributeList Procedure 
FUNCTION getAttributeList RETURNS CHARACTER
  ( INPUT ihObject        AS HANDLE,
    INPUT icName          AS CHAR,
    INPUT icExceptionList AS CHAR,
    INPUT ibBlanks        AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBehaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBehaviour Procedure 
FUNCTION getBehaviour RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseColumn Procedure 
FUNCTION getBrowseColumn RETURNS HANDLE
  ( INPUT ihBrowse    AS HANDLE,
    INPUT icFieldName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseColumnLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseColumnLabel Procedure 
FUNCTION getBrowseColumnLabel RETURNS CHARACTER
  ( INPUT ihBrowse    AS HANDLE,
    INPUT icFieldName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseColumnNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseColumnNum Procedure 
FUNCTION getBrowseColumnNum RETURNS INTEGER
  ( INPUT ihBrowse    AS HANDLE,
    INPUT icFieldName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferFieldDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBufferFieldDataType Procedure 
FUNCTION getBufferFieldDataType RETURNS CHARACTER
  ( INPUT ihBuffer AS HANDLE,
    INPUT icField  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getColNumFromRGB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getColNumFromRGB Procedure 
FUNCTION getColNumFromRGB RETURNS INTEGER
  ( INPUT iRGBcolor AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContainerHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContainerHandle Procedure 
FUNCTION getContainerHandle RETURNS HANDLE
  ( INPUT ihProcedure    AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDesignObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDesignObject Procedure 
FUNCTION getDesignObject RETURNS HANDLE
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEventAction Procedure 
FUNCTION getEventAction RETURNS CHARACTER
  ( INPUT ihParent AS HANDLE,
    INPUT ihWidget AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEventMethod Procedure 
FUNCTION getEventMethod RETURNS CHARACTER
  ( INPUT ihParent AS HANDLE,
    INPUT ihWidget AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEventWidget Procedure 
FUNCTION getEventWidget RETURNS HANDLE
  ( INPUT ihParent AS HANDLE,
    INPUT icAction AS CHAR,
    INPUT icType   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldHandle Procedure 
FUNCTION getFieldHandle RETURNS HANDLE
  ( INPUT ihFieldMap  AS HANDLE,
    INPUT icFieldName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldMapWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldMapWidgets Procedure 
FUNCTION getFieldMapWidgets RETURNS CHARACTER
  ( INPUT ihFieldMap   AS HANDLE,
    INPUT icFieldNames AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsAttributeSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsAttributeSet Procedure
FUNCTION getIsAttributeSet RETURNS LOGICAL 
  (INPUT ihObject  AS HANDLE,
   INPUT icName    AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getIsEventAllowed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsEventAllowed Procedure 
FUNCTION getIsEventAllowed RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE,
    INPUT icEvent  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkedObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLinkedObject Procedure 
FUNCTION getLinkedObject RETURNS HANDLE
  ( INPUT ihObject    AS HANDLE,
    INPUT icLinktype  AS CHAR,
    INPUT icDirection AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkedObjectByInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLinkedObjectByInfo Procedure 
FUNCTION getLinkedObjectByInfo RETURNS HANDLE
  ( INPUT ihObject    AS HANDLE,
    INPUT icLinktype  AS CHAR,
    INPUT icLinkInfo  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLocalDistinctRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLocalDistinctRows Procedure 
FUNCTION getLocalDistinctRows RETURNS LOGICAL
  ( INPUT ihOrgBrowse AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectActionList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectActionList Procedure 
FUNCTION getObjectActionList RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectByAttribute Procedure 
FUNCTION getObjectByAttribute RETURNS HANDLE
  ( INPUT ihWindow    AS HANDLE,
    INPUT icName      AS CHAR,
    INPUT icValue     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectByEvent Procedure 
FUNCTION getObjectByEvent RETURNS HANDLE
  ( INPUT ihEventWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectByLinkInfo Procedure 
FUNCTION getObjectByLinkInfo RETURNS HANDLE
  ( INPUT ihObject    AS HANDLE,
    INPUT icLinkinfo  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectByName Procedure 
FUNCTION getObjectByName RETURNS HANDLE
  ( INPUT ihWindow     AS HANDLE,
    INPUT icObjectName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByNameAndType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectByNameAndType Procedure 
FUNCTION getObjectByNameAndType RETURNS HANDLE
  ( INPUT ihWindow     AS HANDLE,
    INPUT icObjectName AS CHAR,
    INPUT icObjectType AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectContainer Procedure 
FUNCTION getObjectContainer RETURNS HANDLE
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectExists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectExists Procedure 
FUNCTION getObjectExists RETURNS LOGICAL
  (INPUT ihObject AS HANDLE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectList Procedure 
FUNCTION getObjectList RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectName Procedure 
FUNCTION getObjectName RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectSourceFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectSourceFile Procedure 
FUNCTION getObjectSourceFile RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectSourceFileHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectSourceFileHandle Procedure 
FUNCTION getObjectSourceFileHandle RETURNS HANDLE
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectState Procedure 
FUNCTION getObjectState RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectTableHandle Procedure 
FUNCTION getObjectTableHandle RETURNS CHARACTER
  ( INPUT icTTname AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectType Procedure 
FUNCTION getObjectType RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPageObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPageObject Procedure 
FUNCTION getPageObject RETURNS HANDLE
  ( INPUT ihContainer AS HANDLE,
    INPUT icPage      AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentLinkFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentLinkFields Procedure 
FUNCTION getParentLinkFields RETURNS CHARACTER
  ( INPUT ihChild AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentLinkValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentLinkValues Procedure 
FUNCTION getParentLinkValues RETURNS CHARACTER
  ( INPUT ihChild AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRealSortField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRealSortField Procedure 
FUNCTION getRealSortField RETURNS CHARACTER
  ( INPUT ihQueryObject     AS HANDLE,
    INPUT icLocalFieldName  AS CHAR,
    INPUT icTarget          AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSecDisabledActions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSecDisabledActions Procedure 
FUNCTION getSecDisabledActions RETURNS CHARACTER
  ( INPUT icProgramFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStrippedSortLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStrippedSortLabel Procedure 
FUNCTION getStrippedSortLabel RETURNS CHARACTER
  ( INPUT ihColumn AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolbarButtonHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolbarButtonHandle Procedure 
FUNCTION getToolbarButtonHandle RETURNS HANDLE
  ( INPUT ihToolBar    AS HANDLE,
    INPUT icAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolBarHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolBarHandles Procedure 
FUNCTION getToolBarHandles RETURNS CHARACTER
  ( INPUT ihToolBar              AS HANDLE,
    INPUT icWidgetNameOrTypeList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolBarNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolBarNames Procedure 
FUNCTION getToolBarNames RETURNS CHARACTER
  ( INPUT ihToolBar    AS HANDLE,
    INPUT icWidgetName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolbarState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolbarState Procedure 
FUNCTION getToolbarState RETURNS CHARACTER
  ( INPUT ihToolbar  AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetSourceFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWidgetSourceFile Procedure 
FUNCTION getWidgetSourceFile RETURNS CHARACTER
  ( INPUT ihWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitDynFilter Procedure 
FUNCTION InitDynFilter RETURNS LOGICAL
  ( INPUT ihQueryObject   AS HANDLE,
    INPUT icFieldList     AS CHAR,
    INPUT icOperatorList  AS CHAR,
    INPUT icValueList     AS CHAR,
    INPUT icAction        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitToolbarWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitToolbarWidgets Procedure 
FUNCTION InitToolbarWidgets RETURNS LOGICAL
  ( INPUT ihToolbar      AS HANDLE,
    INPUT ihLinkedObject AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LinkAllObjects) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LinkAllObjects Procedure 
FUNCTION LinkAllObjects RETURNS LOGICAL
  ( INPUT ihWindow     AS HANDLE,
    INPUT bReplace     AS LOG,
    INPUT icExceptList AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadUserFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadUserFilter Procedure 
FUNCTION LoadUserFilter RETURNS LOGICAL
  ( INPUT ihObject     AS HANDLE,
    INPUT ihSourceProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MergeToolbars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MergeToolbars Procedure 
FUNCTION MergeToolbars RETURNS LOGICAL
  ( INPUT ihSourceTB AS HANDLE,
    INPUT ihTargetTB AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mgetAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD mgetAttribute Procedure 
FUNCTION mgetAttribute RETURNS CHARACTER
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoveBrowseColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MoveBrowseColumn Procedure 
FUNCTION MoveBrowseColumn RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icColumnName AS CHAR,
    INPUT iiToPos      AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-msetAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD msetAttribute Procedure 
FUNCTION msetAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewBrowse Procedure 
FUNCTION NewBrowse RETURNS HANDLE
  ( INPUT ihRectOrBrw        AS HANDLE,
    INPUT iiRowsToBatch      AS INT,
    INPUT icProperties       AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icOtherProp        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseDropDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewBrowseDropDown Procedure 
FUNCTION NewBrowseDropDown RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icList             AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseFillIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewBrowseFillIn Procedure 
FUNCTION NewBrowseFillIn RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icLookupAttrib     AS CHAR,
    INPUT icValidation       AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseSearchField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewBrowseSearchField Procedure 
FUNCTION NewBrowseSearchField RETURNS HANDLE
  ( INPUT ihRectSearchField  AS HANDLE,
    INPUT ihBrowse           AS HANDLE,
    INPUT iiColumn           AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseToggle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewBrowseToggle Procedure 
FUNCTION NewBrowseToggle RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icOtherProp        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewDynFilter Procedure 
FUNCTION NewDynFilter RETURNS HANDLE
  ( INPUT ihFilterQryObject      AS HANDLE,
    INPUT ihFilterButton         AS HANDLE,
    INPUT icOtherProp            AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewEvent Procedure 
FUNCTION NewEvent RETURNS LOGICAL
  ( INPUT ihObject     AS HANDLE,
    INPUT ihWidget     AS HANDLE,
    INPUT icAction     AS CHAR,
    INPUT icName       AS CHAR,
    INPUT icWidgetType AS CHAR,
    INPUT icMethod     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewFieldMap Procedure 
FUNCTION NewFieldMap RETURNS HANDLE
  ( INPUT ihQuery               AS HANDLE,
    INPUT ihFrame               AS HANDLE,
    INPUT icBufferUpdateFields  AS CHAR,
    INPUT icScreenUpdateFields  AS CHAR,
    INPUT icBufferDisplayFields AS CHAR,
    INPUT icScreenDisplayFields AS CHAR,
    INPUT icOtherProp           AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewFilter Procedure 
FUNCTION NewFilter RETURNS HANDLE
  ( INPUT ihQuery                AS HANDLE,
    INPUT ihFrame                AS HANDLE,
    INPUT icInputFieldList       AS CHAR,
    INPUT icBufferFieldList      AS CHAR,
    INPUT icOperatorList         AS CHAR,
    INPUT icOperatorFieldList    AS CHAR,
    INPUT ibSearchOnValueChanged AS LOG,
    INPUT ihFilterButton         AS HANDLE,
    INPUT icOtherProp            AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewMenuBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewMenuBand Procedure 
FUNCTION NewMenuBand RETURNS HANDLE
  ( INPUT ihParent     AS HANDLE,
    INPUT icActionList AS CHAR,
    INPUT icOtherProp  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewObject Procedure 
FUNCTION NewObject RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT ihObject AS HANDLE,
    INPUT icType   AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewPanel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewPanel Procedure 
FUNCTION NewPanel RETURNS HANDLE
  ( INPUT ihRectangle    AS HANDLE,
    INPUT icMenu         AS CHAR,
    INPUT icActionList   AS CHAR,
    INPUT iiButtonWidth  AS INT,
    INPUT iiButtonHeight AS INT,
    INPUT icOtherProp    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewPropertyFillIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewPropertyFillIn Procedure 
FUNCTION NewPropertyFillIn RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icDataType         AS CHAR,
    INPUT icFormat           AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icLookupAttrib     AS CHAR,
    INPUT icValidation       AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewQuery Procedure 
FUNCTION NewQuery RETURNS HANDLE
  ( INPUT iiRowsToBatch      AS INT,
    INPUT icProperties       AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icOtherProp        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewTabFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewTabFolder Procedure 
FUNCTION NewTabFolder RETURNS HANDLE
  ( INPUT ihRectangle            AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewTempTable Procedure 
FUNCTION NewTempTable RETURNS HANDLE
  ( INPUT ihContainer        AS HANDLE,
    INPUT icName             AS CHAR,
    INPUT ihSourceProc       AS HANDLE,
    INPUT icFieldDef         AS CHAR,
    INPUT icIndexList        AS CHAR,
    INPUT icOtherProp        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewToolBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewToolBar Procedure 
FUNCTION NewToolBar RETURNS HANDLE
  ( INPUT ihRect      AS HANDLE,
    INPUT icMenu      AS CHAR,
    INPUT icActList   AS CHAR,
    INPUT icOtherProp AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewViewer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NewViewer Procedure 
FUNCTION NewViewer RETURNS HANDLE
  ( INPUT ihRectangle   AS HANDLE,
    INPUT ihParentQuery AS HANDLE,
    INPUT icViewerProg  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProcessQuery Procedure 
FUNCTION ProcessQuery RETURNS LOGICAL
  ( INPUT ihQueryObject   AS HANDLE,
    INPUT icServerProgram AS CHAR,
    INPUT icParamList     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessSelectedRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProcessSelectedRows Procedure 
FUNCTION ProcessSelectedRows RETURNS LOGICAL
  ( INPUT ihBrowse        AS HANDLE,
    INPUT icServerProgram AS CHAR,
    INPUT icParamList     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReplaceObjectLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReplaceObjectLink Procedure 
FUNCTION ReplaceObjectLink RETURNS LOGICAL
  ( INPUT ihObject1  AS HANDLE,
    INPUT ihObject2  AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResetBufferSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ResetBufferSequence Procedure 
FUNCTION ResetBufferSequence RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResetMenuToggle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ResetMenuToggle Procedure 
FUNCTION ResetMenuToggle RETURNS LOGICAL
  ( INPUT ihMenu       AS HANDLE,
    INPUT ihExceptItem AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActionDbSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setActionDbSecurity Procedure 
FUNCTION setActionDbSecurity RETURNS LOGICAL
  ( INPUT ihMenuOwnerOrToolbar AS HANDLE,
    INPUT ihSourceQuery        AS HANDLE)  FORWARD.

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

&IF DEFINED(EXCLUDE-setAttributeList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAttributeList Procedure 
FUNCTION setAttributeList RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icList    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBehaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBehaviour Procedure 
FUNCTION setBehaviour RETURNS LOGICAL
  ( INPUT icBehaviour AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrowseColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBrowseColumns Procedure 
FUNCTION setBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse       AS HANDLE,
    INPUT icColumns      AS CHAR,
    INPUT ibHideOverlays AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrowseProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBrowseProperties Procedure 
FUNCTION setBrowseProperties RETURNS LOGICAL
  ( INPUT icProperties AS CHAR,
    INPUT ihBrowse     AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDebugObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDebugObject Procedure 
FUNCTION setDebugObject RETURNS LOGICAL
  ( INPUT ihDebugObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDefaultButton) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDefaultButton Procedure 
FUNCTION setDefaultButton RETURNS LOGICAL
  ( INPUT icAction  AS CHAR,
    INPUT icImage   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEditBgColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEditBgColor Procedure 
FUNCTION setEditBgColor RETURNS LOGICAL
  ( INPUT iiEditBgColor AS INT  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEventMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEventMethod Procedure 
FUNCTION setEventMethod RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE,
    INPUT ihWidget AS HANDLE,
    INPUT icMethod AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldMapState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldMapState Procedure 
FUNCTION setFieldMapState RETURNS LOGICAL
  ( INPUT ihFieldMap  AS HANDLE,
    INPUT icState     AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setListAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setListAttribute Procedure 
FUNCTION setListAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR,
    INPUT icDelim   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setObjectName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setObjectName Procedure 
FUNCTION setObjectName RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icNewName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setObjectSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setObjectSourceProc Procedure 
FUNCTION setObjectSourceProc RETURNS LOGICAL
  ( INPUT ihObjectSourceProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setObjectState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setObjectState Procedure 
FUNCTION setObjectState RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icState   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSearchFieldLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSearchFieldLinkInfo Procedure 
FUNCTION setSearchFieldLinkInfo RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icColumn AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSecDisabledActions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSecDisabledActions Procedure 
FUNCTION setSecDisabledActions RETURNS LOGICAL
  ( INPUT icSecDisabledActions AS CHAR,
    INPUT icProgramFile        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSortLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSortLabel Procedure 
FUNCTION setSortLabel RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icSortColumn AS CHAR,
    INPUT ibDesc       AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSortMarkers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSortMarkers Procedure 
FUNCTION setSortMarkers RETURNS LOGICAL
  ( INPUT icMarkAsc  AS CHAR,
    INPUT icMarkDesc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSortString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSortString Procedure 
FUNCTION setSortString RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE,
    INPUT icSortString  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSourceProc Procedure
FUNCTION setSourceProc RETURNS LOGICAL 
  (INPUT ihObject     AS HANDLE,
   INPUT ihSourceProc AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setToolbar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setToolbar Procedure 
FUNCTION setToolbar RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE,
    INPUT icState   AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setToolbarToggles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setToolbarToggles Procedure 
FUNCTION setToolbarToggles RETURNS LOGICAL
  ( INPUT ihToolbar       AS HANDLE,
    INPUT ibChecked       AS LOG,
    INPUT icExceptionList AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseBrowseColumnFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUseBrowseColumnFormat Procedure 
FUNCTION setUseBrowseColumnFormat RETURNS LOGICAL
  ( INPUT ibSetUseBrowseColumnFormat AS LOG ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-swapObjectHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD swapObjectHandle Procedure 
FUNCTION swapObjectHandle RETURNS LOGICAL
  ( INPUT ihFrom AS HANDLE,
    INPUT ihTo   AS HANDLE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewAverageColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewAverageColumn Procedure 
FUNCTION ViewAverageColumn RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT ibView   AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewCountDistinctColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewCountDistinctColumn Procedure 
FUNCTION ViewCountDistinctColumn RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT ibView   AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewHideFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewHideFieldMap Procedure 
FUNCTION ViewHideFieldMap RETURNS LOGICAL
  ( INPUT ihFieldMap AS HANDLE,
    INPUT ibView     AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewRecordCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewRecordCount Procedure 
FUNCTION ViewRecordCount RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE )  FORWARD.

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
         HEIGHT             = 43.76
         WIDTH              = 63.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ConstructBrowseOrQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConstructBrowseOrQuery Procedure 
PROCEDURE ConstructBrowseOrQuery :
/*------------------------------------------------------------------------------
  Purpose:    Decompose parameters to NewBrowse or NewQuery functions.
              For the browse these attributes can be set for the placeholder rectangle (design object) 
              before NewBrowse is called:
              - getrecordcoount         (if the initial query should return records)
              - querysort               (if the initial query should return records)
              - querydesc               (if the initial query should return records)
              - 1stSortColumn           (when returning data and multiple sort columns)
              - 1stSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - 2ndSortColumn           (when returning data and multiple sort columns)
              - 2ndSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - 3rdSortColumn           (when returning data and multiple sort columns)
              - 3rdSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - 4thSortColumn           (when returning data and multiple sort columns)
              - 4thSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - temptablehandle         (if the browse should be built on a local temp-table)
                If this attribute is set also remeber to set "uselocaldata","yes"
              - calcfieldproc           (name of server procedure containing calculated field procs)
              - periodfield<fieldname>,<periods>
                Example: wmqy (week month quarter year - if none is set the periode is assumed m (month))
                Usage is for enabling distinct queries (and totals) on periodes based on a date field in the database 
              - querystatfields         (comma-separated list of fields that should be accumulated on the server)
              - distinctcolumns         (comma-separated list of distinct columns)
              - accumfields             (comma-separated list of accumulated fields pr distinct row) 
              
              A query doesn't have a placeholder and hence any pre-create attributes must be set
              as a comma-separated list (see NewQuery)
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihObject            AS HANDLE NO-UNDO.
DEF INPUT  PARAM icBuffersAndFields  AS CHAR   NO-UNDO.
DEF INPUT  PARAM icQueryCriteria     AS CHAR   NO-UNDO.
DEF INPUT  PARAM icOtherProp         AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk                AS LOG    NO-UNDO.

DEF VAR iy                     AS INT    NO-UNDO.
DEF VAR cQueryJoin             AS CHAR   NO-UNDO.
DEF VAR cBuffersAndFields      AS CHAR   NO-UNDO.
DEF VAR cViewBuffersAndFields  AS CHAR   NO-UNDO.
DEF VAR cBufferFieldDefs       AS CHAR   NO-UNDO.
DEF VAR cBaseTableFilterFields AS CHAR   NO-UNDO.
DEF VAR cBaseTableFields       AS CHAR   NO-UNDO.
DEF VAR cAllViewFields         AS CHAR   NO-UNDO.
DEF VAR cAllViewFieldsReal     AS CHAR   NO-UNDO.
DEF VAR cAllCalcFields         AS CHAR   NO-UNDO.
DEF VAR cAllJoinViewFields     AS CHAR   NO-UNDO.
DEF VAR cNoDisplayFields       AS CHAR   NO-UNDO.
DEF VAR cCalcFieldProc         AS CHAR   NO-UNDO.
DEF VAR cSortMap               AS CHAR   NO-UNDO.
DEF VAR cBufferFields          AS CHAR   NO-UNDO.
DEF VAR cViewBufferFieldDefs   AS CHAR   NO-UNDO.
DEF VAR cBufferList            AS CHAR   NO-UNDO.
DEF VAR cFieldReposList        AS CHAR   NO-UNDO.
DEF VAR cDbFieldReposList      AS CHAR   NO-UNDO.
DEF VAR iFieldIx               AS INT    NO-UNDO.
DEF VAR iFldPos                AS INT    NO-UNDO.
DEF VAR cNotExistList          AS CHAR   NO-UNDO.
DEF VAR cDistinctList          AS CHAR   NO-UNDO.
DEF VAR cAccumList             AS CHAR   NO-UNDO.
DEF VAR cLocalCalcFieldList    AS CHAR   NO-UNDO.
DEF VAR hColumn                AS HANDLE NO-UNDO.

IF icQueryCriteria MATCHES "*(*Company)" OR icQueryCriteria MATCHES "*(Codemaster)" THEN DO:
  setAttribute(ihObject,"CompanyTag",SUBSTR(icQueryCriteria,R-INDEX(icQueryCriteria,"("))).
  icQueryCriteria = SUBSTR(icQueryCriteria,1,R-INDEX(icQueryCriteria,"(") - 1).
END.

IF NUM-ENTRIES(icQueryCriteria) < NUM-ENTRIES(icBuffersAndFields) THEN DO:
  MESSAGE "Invalid browse/query definition" SKIP
          "Number of buffers i query criteria is less that number of buffers in buffer/field def"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

DO ix = 1 TO LENGTH(icQueryCriteria):
  IF SUBSTR(icQueryCriteria,ix,6) = "first " OR
     SUBSTR(icQueryCriteria,ix,5) = "last " OR
     SUBSTR(icQueryCriteria,ix,5) = "each " 
     THEN DO:
    DO iy = ix TO MAX(1,ix - 10) BY -1:
      IF SUBSTR(icQueryCriteria,iy,1) = "," THEN LEAVE.  
    END.
    LEAVE.
  END.
END.
IF iy > 0 THEN DO:
  setAttribute(ihObject,"querywhere",TRIM(SUBSTR(icQueryCriteria,1,iy - 1))).
  setAttribute(ihObject,"queryjoin",TRIM(SUBSTR(icQueryCriteria,iy))).
END.
ELSE
  setAttribute(ihObject,"querywhere",TRIM(icQueryCriteria)).

/* Interprete initial comma-separated parameter string (for the browse the parameters can be set on the design object): */
DO ix = 1 TO NUM-ENTRIES(icOtherProp):
  IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "SORT" THEN DO:
    setAttribute(ihObject,"querysort",REPLACE(ENTRY(1,ENTRY(2,ENTRY(ix,icOtherProp),"|")," "),";"," ")).
    IF NUM-ENTRIES(ENTRY(2,ENTRY(ix,icOtherProp),"|")," ") > 1 THEN
      setAttribute(ihObject,"querydesc",REPLACE(ENTRY(2,ENTRY(2,ENTRY(ix,icOtherProp),"|")," "),";"," ")).
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "TEMP-TABLE" THEN DO:
    setAttribute(ihObject,"temptablehandle",ENTRY(2,ENTRY(ix,icOtherProp),"|")).
    setAttribute(ihObject,"uselocaldata","yes").
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "getrecordcount" THEN 
    setAttribute(ihObject,"getrecordcount","yes").
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "calcfieldproc" THEN DO:
    cCalcFieldProc = ENTRY(2,ENTRY(ix,icOtherProp),"|").
    setAttribute(ihObject,"calcfieldproc",cCalcFieldProc).
    DYNAMIC-FUNCTION("setCalcFieldProc",cCalcFieldProc).
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "periodfield" THEN 
    setAttribute(ihObject,"addperiod" + ENTRY(1,ENTRY(2,ENTRY(ix,icOtherProp),"|")," "),ENTRY(2,ENTRY(2,ENTRY(ix,icOtherProp),"|")," ")).
END.

IF getAttribute(ihObject,"querysort") NE "" THEN DO:
  setAttribute(ihObject,"1stSortColumn",ENTRY(1,getAttribute(ihObject,"querysort")," ")).
  setAttribute(ihObject,"1stSortColumnDesc",getAttribute(ihObject,"querydesc")).
END.

EMPTY TEMP-TABLE ttBufferDef.
EMPTY TEMP-TABLE ttFieldDef.

DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields):
  CREATE ttBufferDef.  
  ASSIGN ttBufferDef.cBuffer     = IF ihObject:TYPE = "rectangle" AND ENTRY(1,ENTRY(ix,icBuffersAndFields),";") = "temp-table" THEN ihObject:NAME 
                                   ELSE ENTRY(1,ENTRY(ix,icBuffersAndFields),";")
         ttBufferDef.iBufferNum  = ix
         ttBufferDef.cCriteria   = TRIM(ENTRY(ix,icQueryCriteria))
         .
  IF ttBufferDef.cBuffer BEGINS "NOT EXIST " THEN 
    ASSIGN ttBufferDef.cBuffer    = TRIM(SUBSTR(ttBufferDef.cBuffer,11))
           cNotExistList          = cNotExistList + ENTRY(1,ttBufferDef.cBuffer,";") + ",".

  DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icBuffersAndFields),";"):
    CREATE ttFieldDef.
    ASSIGN ttFieldDef.iBufferNum  = ix
           ttFieldDef.cFieldDef   = TRIM(ENTRY(1,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"@"))
           ttFieldDef.iFieldPos   = IF NUM-ENTRIES(ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"@") > 1 THEN 
                                      INT(ENTRY(2,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"@"))
                                    ELSE 0
           ttFieldDef.cFieldName  = IF ttFieldDef.cFieldDef BEGINS "!+" OR ttFieldDef.cFieldDef BEGINS "+!" THEN SUBSTR(ENTRY(1,ttFieldDef.cFieldDef,"|"),3)
                                    ELSE IF ttFieldDef.cFieldDef BEGINS "!" OR ttFieldDef.cFieldDef BEGINS "+" THEN SUBSTR(ENTRY(1,ttFieldDef.cFieldDef,"|"),2)
                                    ELSE ENTRY(1,ttFieldDef.cFieldDef,"|")
           ttFieldDef.bCalculated = ttFieldDef.cFieldDef BEGINS "+" OR
                                    ttFieldDef.cFieldDef BEGINS "!+"
           ttFieldDef.bVisible    = NOT ttFieldDef.cFieldDef BEGINS "!" AND
                                    NOT ttFieldDef.cFieldDef BEGINS "+!"
           iFieldIx               = iFieldIx + 10
           ttFieldDef.iFieldSeq   = iFieldIx
           .
    IF NUM-ENTRIES(ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") = 1 THEN
      ttFieldDef.cFieldLabel = ?. /* to be determined from browse if static */       
/*     IF R-INDEX(ttFieldDef.cFieldName,"[") > 0 THEN                                                     */
/*       ttFieldDef.cFieldName = SUBSTR(ttFieldDef.cFieldName,1,R-INDEX(ttFieldDef.cFieldName,"[") - 1).  */
    IF ttFieldDef.bCalculated AND NUM-ENTRIES(ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") GE 4 AND ENTRY(4,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") = "" THEN
      cLocalCalcFieldList = cLocalCalcFieldList + ttFieldDef.cFieldName + ",".
    ELSE IF ttFieldDef.bCalculated AND NUM-ENTRIES(ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") LT 4 THEN 
      cLocalCalcFieldList = cLocalCalcFieldList + ttFieldDef.cFieldName + ",".
    IF ttFieldDef.cFieldName BEGINS "distinct " THEN
      ASSIGN ttFieldDef.cFieldName = TRIM(SUBSTR(ttFieldDef.cFieldName,10))
             cDistinctList = cDistinctList + ttFieldDef.cFieldName + ",".
    IF ttFieldDef.cFieldName BEGINS "accum " THEN
      ASSIGN ttFieldDef.cFieldName = TRIM(SUBSTR(ttFieldDef.cFieldName,7))
             cAccumList = cAccumList + ttFieldDef.cFieldName + ",".
  END.
  IF ix > 1 THEN DO:
    setAttribute(ihObject,"bufferjoin" + ttBufferDef.cBuffer,"," + TRIM(ENTRY(ix,icQueryCriteria))).
    cQueryJoin = cQueryJoin + "," + TRIM(ENTRY(ix,icQueryCriteria)).
  END.
END.

IF ihObject:TYPE = "browse" THEN DO ix = 1 TO ihObject:NUM-COLUMNS:
  FIND FIRST ttFieldDef WHERE ttFieldDef.cFieldName = ihObject:GET-BROWSE-COLUMN(ix):NAME NO-ERROR.
  IF AVAIL ttFieldDef THEN DO:
    IF SESSION:DISPLAY-TYPE NE "TTY" AND NOT ihObject:GET-BROWSE-COLUMN (ix):VISIBLE THEN 
      ttFieldDef.bVisible = NO.
    IF ttFieldDef.cFieldLabel = ? THEN ttFieldDef.cFieldDef = ttFieldDef.cFieldDef + "|" + ihObject:GET-BROWSE-COLUMN(ix):LABEL.  
  END.  
END.  

IF cNotExistList NE "" THEN
  setAttribute(ihObject,"noexistbuffers",TRIM(cNotExistList,",")).
IF cDistinctList NE "" THEN
  setAttribute(ihObject,"distinctcolumns",TRIM(cDistinctList,",")).
IF cAccumList NE "" THEN
  setAttribute(ihObject,"accumfields",TRIM(cAccumList,",")).
IF cLocalCalcFieldList NE "" THEN
  setAttribute(ihObject,"localcalcfields",TRIM(cLocalCalcFieldList,",")).

cSortMap = getAttribute(ihObject,"sortmap").
FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihObject
      AND ttAttribute.cName   BEGINS "addperiod":
  FIND FIRST ttFieldDef
       WHERE ttFieldDef.cFieldName = SUBSTR(ttAttribute.cName,10)
         AND ttFieldDef.bVisible   
         AND NOT ttFieldDef.bCalculated
       NO-ERROR.
  IF AVAIL ttFieldDef THEN DO ix = 1 TO LENGTH(ttAttribute.cValue):
    IF CAN-DO("w,m,q,y",SUBSTR(ttAttribute.cValue,ix,1)) THEN DO:
      CREATE bttFieldDef.
      ASSIGN bttFieldDef.iBufferNum  = ttFieldDef.iBufferNum
             bttFieldDef.bVisible    = YES
             bttFieldDef.bCalculated = YES.
      CASE SUBSTR(ttAttribute.cValue,ix,1):
        WHEN "w" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 2
                 bttFieldDef.cFieldName  = "jb_week_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_weeknum(" + ttFieldDef.cFieldName + ")".
        WHEN "m" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 4
                 bttFieldDef.cFieldName  = "jb_month_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_month(" + ttFieldDef.cFieldName + ")".
        WHEN "q" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 6
                 bttFieldDef.cFieldName  = "jb_quarter_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_quarter(" + ttFieldDef.cFieldName + ")".
        WHEN "y" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 8
                 bttFieldDef.cFieldName  = "jb_year_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_year(" + ttFieldDef.cFieldName + ")".

      END CASE. 
/*       IF ix = 1 THEN cSortMap = cSortMap + "," + bttFieldDef.cFieldName + ";" + ttFieldDef.cFieldName + ",". */
    END.
  END.
END.
setAttribute(ihObject,"sortmap",TRIM(cSortMap,",")).

FOR EACH ttBufferDef:
  ASSIGN cBuffersAndFields     = cBuffersAndFields + ttBufferDef.cBuffer
         cViewBuffersAndFields = cViewBuffersAndFields + ttBufferDef.cBuffer
         cBufferFieldDefs      = ""
         cBufferFields         = ""
         cViewBufferFieldDefs  = ""
         cBufferList           = cBufferList + ttBufferDef.cBuffer + ",".

  IF ttBufferDef.iBufferNum = 1 THEN
    setAttribute(ihObject,"basetable",ttBufferDef.cBuffer).

  FOR EACH ttFieldDef 
      WHERE ttBufferDef.iBufferNum = ttFieldDef.iBufferNum
      BY ttFieldDef.iFieldSeq:

    ASSIGN cBufferFieldDefs = cBufferFieldDefs + ttFieldDef.cFieldDef + ";"
           cBufferFields    = cBufferFields + ttFieldDef.cFieldName + ",".
  
    IF CAN-DO(cAllViewFields,ttFieldDef.cFieldName) THEN DO:
      setAttribute(ihObject,"fieldbuffer" + ttFieldDef.cFieldName + STRING(ttFieldDef.iBufferNum)
                           ,ttBufferDef.cBuffer).
      setAttribute(ihObject,"orgdbfield" + ttFieldDef.cFieldName + STRING(ttFieldDef.iBufferNum),ttFieldDef.cFieldName).
      ttFieldDef.cClientFieldName = ttFieldDef.cFieldName + STRING(ttFieldDef.iBufferNum).
    END.
    ELSE DO:
      setAttribute(ihObject,"fieldbuffer" + ttFieldDef.cFieldName,ttBufferDef.cBuffer).
      setAttribute(ihObject,"orgdbfield" + ttFieldDef.cFieldName,ttFieldDef.cFieldName).
      ttFieldDef.cClientFieldName = ttFieldDef.cFieldName.
    END.
    IF SUBSTR(ttFieldDef.cFieldName,LENGTH(ttFieldDef.cFieldName)) = "]" THEN DO:
      setAttribute(ihObject,"orgDbField" + REPLACE(REPLACE(ttFieldDef.cFieldName,"[","_"),"]",""),ttFieldDef.cFieldName).
      setAttribute(ihObject,"fieldbuffer" + REPLACE(REPLACE(ttFieldDef.cFieldName,"[","_"),"]",""),ttBufferDef.cBuffer).
    END.

    IF ttFieldDef.bVisible AND NOT ttFieldDef.bCalculated AND ttBufferDef.iBufferNum = 1 THEN
      cBaseTableFilterFields = cBaseTableFilterFields + ttFieldDef.cFieldName + ",".
  
    IF ttBufferDef.iBufferNum = 1 AND NOT ttFieldDef.bCalculated THEN
      cBaseTableFields = cBaseTableFields + ttFieldDef.cFieldName + ",".

    IF ttFieldDef.bVisible THEN
      ASSIGN cAllViewFields       = cAllViewFields       + ttFieldDef.cFieldName + ","
             cViewBufferFieldDefs = cViewBufferFieldDefs + ttFieldDef.cFieldDef + ";"
             .
        
    IF ttFieldDef.bCalculated THEN DO:
      cAllCalcFields = cAllCalcFields + ttFieldDef.cFieldName + ",".

      IF NUM-ENTRIES(ttFieldDef.cFieldDef,"|") > 3 AND ENTRY(4,ttFieldDef.cFieldDef,"|") MATCHES "*(ROWID*" THEN
        setAttribute(ihObject,"calcphrase" + ttFieldDef.cFieldName,ENTRY(4,ttFieldDef.cFieldDef,"|")).
    END.
    
    IF NOT ttFieldDef.bVisible THEN
      cNoDisplayFields = cNoDisplayFields + ttFieldDef.cFieldName + ",".
    
    IF ttBufferDef.iBufferNum > 1 AND ttFieldDef.bVisible THEN
      cAllJoinViewFields = cAllJoinViewFields + ",".

  END.

  ASSIGN cBufferFieldDefs      = TRIM(cBufferFieldDefs,";")
         cViewBufferFieldDefs  = TRIM(cViewBufferFieldDefs,";")
         cBuffersAndFields     = TRIM(cBuffersAndFields + ";" + cBufferFieldDefs,";") + ","
         cViewBuffersAndFields = TRIM(cViewBuffersAndFields + ";" + cViewBufferFieldDefs,";") + ","
         .
  
  setAttribute(ihObject,"bufferfields" + ttBufferDef.cBuffer,TRIM(cBufferFields,",")).
END.

ix = 0.
EMPTY TEMP-TABLE ttSort1.
FOR EACH ttFieldDef 
    WHERE ttFieldDef.bVisible
    BY ttFieldDef.iFieldSeq:
  ix = ix + 1.
  IF ttFieldDef.iFieldPos > 0 THEN DO:
    CREATE ttSort1.
    ASSIGN ttSort1.iSeq   = ttFieldDef.iFieldPos            /* "To" position */
           ttSort1.iSeq2  = ix                              /* "From" position */
           ttSort1.cText1 = ttFieldDef.cClientFieldName + ";"
           ttSort1.cText2 = ttFieldDef.cFieldName + ";".
  END.
END.

FOR EACH ttSort1 
    BY ttSort1.iSeq DESC
    :
  ASSIGN cFieldReposList   = cFieldReposList + ttSort1.cText1 + STRING(ttSort1.iSeq2) + ";" + STRING(ttSort1.iSeq) + ","
         cDbFieldReposList = cDbFieldReposList + ttSort1.cText2 + STRING(ttSort1.iSeq2) + ";" + STRING(ttSort1.iSeq) + ",".
END.

setAttribute(ihObject,"buffersandfields",TRIM(cBuffersAndFields,",")).
setAttribute(ihObject,"viewbuffersandfields",TRIM(cViewBuffersAndFields,",")).
setAttribute(ihObject,"basetablefilterfields",TRIM(cBaseTableFilterFields,",")).
setAttribute(ihObject,"basetablefields",TRIM(cBaseTableFields,",")).
cAllCalcFields = TRIM(cAllCalcFields,",").
setAttribute(ihObject,"allcalcfields",TRIM(cAllCalcFields + ",jbCountDistinct,jbAverage",",")).
setAttribute(ihObject,"alljoinviewfields",TRIM(cAllJoinViewFields,",")).
IF getAttribute(ihObject,"noDisplayFields") NE "" THEN cNoDisplayFields = getAttribute(ihObject,"noDisplayFields").
setAttribute(ihObject,"nodisplayfields",TRIM(cNoDisplayFields,",")).
setAttribute(ihObject,"allviewfields",TRIM(cAllViewFields,",")).
setAttribute(ihObject,"bufferlist",TRIM(cBufferList,",")).
IF cFieldReposList NE "" THEN DO:
  setAttribute(ihObject,"FieldReposList",TRIM(cFieldReposList,",")).
  setAttribute(ihObject,"DbFieldReposList",TRIM(cDbFieldReposList,",")).
END.

IF getAttribute(ihObject,"initialsortstring") NE "" THEN DO:
  IF NUM-ENTRIES(cBuffersAndFields,";") = 1 THEN
    MESSAGE "The initialSortString attribute cannot be used without a detailed field spec"
            VIEW-AS ALERT-BOX WARNING.
  ELSE
    setSortString(ihObject,getAttribute(ihObject,"initialsortstring")).
END.
ELSE IF getAttribute(ihObject,"querysort") = "" THEN DO:
  setAttribute(ihObject,"querysort",
                 TRIM(getAttribute(ihObject,"1stSortColumn") + " " +
                 getAttribute(ihObject,"1stSortColumnDesc") +
                 (IF getAttribute(ihObject,"2ndSortColumn") NE "" THEN
                    (IF getAttribute(ihObject,"1stSortColumn") NE "" THEN " BY " ELSE "") +
                    getAttribute(ihObject,"2ndSortColumn") + " " +
                    getAttribute(ihObject,"2ndSortColumnDesc")
                  ELSE "") +
                 (IF getAttribute(ihObject,"3rdSortColumn") NE "" THEN
                    (IF getAttribute(ihObject,"1stSortColumn") NE "" OR getAttribute(ihObject,"2ndSortColumn") NE "" THEN " BY " ELSE "") +
                    getAttribute(ihObject,"3rdSortColumn") + " " +
                    getAttribute(ihObject,"3rdSortColumnDesc")
                  ELSE "") +
                 (IF getAttribute(ihObject,"4thSortColumn") NE "" THEN
                    (IF getAttribute(ihObject,"1stSortColumn") NE "" OR getAttribute(ihObject,"2ndSortColumn") NE "" OR getAttribute(ihObject,"3rdSortColumn") NE "" THEN " BY " ELSE "") +
                    getAttribute(ihObject,"4thSortColumn") + " " +
                    getAttribute(ihObject,"4thSortColumnDesc")
                  ELSE "")
                  )).
  setAttribute(ihObject,"querydesc","").

  setAttribute(ihObject,"sortbuffer",getAttribute(ihObject,"fieldbuffer" + TRIM(getAttribute(ihObject,"1stSortColumn")))).
END.
ELSE DO: 
  setAttribute(ihObject,"sortbuffer",getAttribute(ihObject,"fieldbuffer" + ENTRY(1,getAttribute(ihObject,"querysort")," "))).
  IF getAttribute(ihObject,"localsort") = "" THEN
    setAttribute(ihObject,"localsort",getAttribute(ihObject,"querysort")).
END.

obOk = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateBrowseAndEvents) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateBrowseAndEvents Procedure 
PROCEDURE CreateBrowseAndEvents :
/*------------------------------------------------------------------------------
  Purpose: Originally in NewBrowse. Moved here to reduce size of NewBrowse    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihRectangle  AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihQuery      AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihProcedure  AS HANDLE NO-UNDO.
DEF INPUT  PARAM icProperties AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ohBrowse     AS HANDLE NO-UNDO.

DEF VAR ix          AS INT  NO-UNDO.
DEF VAR cTitle      AS CHAR NO-UNDO INIT ?.
DEF VAR iTitleBgCol AS INT  NO-UNDO INIT ?.
DEF VAR iTitleFgCol AS INT  NO-UNDO INIT ?.
DEF VAR iTitleFont  AS INT  NO-UNDO INIT ?.

IF icProperties MATCHES "*title*" THEN
  DO ix = 1 TO NUM-ENTRIES(icProperties):
    CASE ENTRY(1,ENTRY(ix,icProperties),"|"):
      WHEN "title"         THEN cTitle = ENTRY(2,ENTRY(ix,icProperties),"|").
      WHEN "title-bgcolor" THEN iTitleBgCol = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
      WHEN "title-fgcolor" THEN iTitleFgCol = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
      WHEN "title-font"    THEN iTitleFont  = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    END CASE.
  END.

IF ihRectangle:TYPE NE "browse" THEN DO:
  CREATE BROWSE ohBrowse
    ASSIGN ROW-MARKERS      = FALSE
           SEPARATORS       = TRUE
           NAME             = ihRectangle:NAME + "_" + STRING(ohBrowse)
           FRAME            = ihRectangle:FRAME
           QUERY            = httTableQuery
           X                = ihRectangle:X + 1
           Y                = ihRectangle:Y + 1
           WIDTH-PIXELS     = ihRectangle:WIDTH-PIXELS - 2
           HEIGHT-PIXELS    = ihRectangle:HEIGHT-PIXELS - 2
           MULTIPLE         = IF CAN-DO(icProperties,"MULTIPLE") THEN TRUE ELSE FALSE
           ROW-MARKERS      = IF CAN-DO(icProperties,"ROW-MARKERS") THEN TRUE ELSE FALSE
           TITLE            = cTitle
  /*          TITLE-BGCOLOR    = iTitleBgCol  */
  /*          TITLE-FGCOLOR    = iTitleFgCol  */
  /*          TITLE-FONT       = iTitleFont   */
           VISIBLE          = YES
           SENSITIVE        = TRUE
           COLUMN-RESIZABLE = TRUE
           READ-ONLY        = YES
           TRIGGERS:
             ON START-SEARCH            PERSISTENT RUN DoProcessEvent (ihProcedure,"start-search").
             ON OFF-END                 PERSISTENT RUN DoProcessEvent (ihProcedure,"off-end").
             ON OFF-HOME                PERSISTENT RUN DoProcessEvent (ihProcedure,"off-home").
             ON VALUE-CHANGED           PERSISTENT RUN DoProcessEvent (ihProcedure,"value-changed").
             ON mouse-select-up         PERSISTENT RUN DoProcessEvent (ihProcedure,"row-entry").
             ON mouse-menu-down         PERSISTENT RUN DoProcessEvent (ihProcedure,"mouse-menu-down").
             ON ROW-LEAVE               PERSISTENT RUN DoProcessEvent (ihProcedure,"row-leave").
             ON ROW-DISPLAY             PERSISTENT RUN DoProcessEvent (ihProcedure,"row-display").
             ON END-MOVE                PERSISTENT RUN DoProcessEvent (ihProcedure,"view-overlay").
             ON SCROLL-NOTIFY           PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
             ON CURSOR-RIGHT            PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
             ON CURSOR-LEFT             PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
             ON CURSOR-UP               PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
             ON CURSOR-DOWN             PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
             ON shift-CURSOR-UP         PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-cursor-up").
             ON shift-CURSOR-DOWN       PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-cursor-down").
             ON PAGE-UP                 PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
             ON PAGE-DOWN               PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
             ON shift-PAGE-UP           PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-page-up").
             ON shift-PAGE-DOWN         PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-page-down").
             ON HOME                    PERSISTENT RUN DoProcessEvent (ihProcedure,"first-record").
             ON END                     PERSISTENT RUN DoProcessEvent (ihProcedure,"last-record").
             ON shift-HOME              PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-home").
             ON shift-END               PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-end").
             ON ANY-PRINTABLE           PERSISTENT RUN DoProcessEvent (ihProcedure,"any-printable").
             ON DEFAULT-ACTION          PERSISTENT RUN DoProcessEvent (ihProcedure,"default-action").
             ON INSERT-MODE             PERSISTENT RUN DoProcessEvent (ihProcedure,"new-record").
             ON DELETE-CHARACTER        PERSISTENT RUN DoProcessEvent (ihProcedure,"delete-record").
             ON F5                      PERSISTENT RUN DoProcessEvent (ihProcedure,"refresh-record"). 
             ON F8                      PERSISTENT RUN DoProcessEvent (ihProcedure,"enter-browse-search"). 
             ON TAB                     PERSISTENT RUN DoProcessEvent (ihProcedure,"tab").
             ON BACK-TAB                PERSISTENT RUN DoProcessEvent (ihProcedure,"back-tab").
             ON DROP-FILE-NOTIFY        PERSISTENT RUN DoProcessEvent (ihProcedure,"drop-file-notify").
             ON ctrl-CURSOR-RIGHT       PERSISTENT RUN DoProcessEvent (ihProcedure,"ctrl-cursor-right").
             ON ctrl-CURSOR-LEFT        PERSISTENT RUN DoProcessEvent (ihProcedure,"ctrl-cursor-left").
             ON shift-ctrl-CURSOR-RIGHT PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-ctrl-cursor-right").
             ON shift-ctrl-CURSOR-LEFT  PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-ctrl-cursor-left").
             ON alt-CURSOR-DOWN         PERSISTENT RUN DoProcessEvent (ihProcedure,"alt-cursor-down").
           END TRIGGERS.
  PUBLISH "setPageQueryHandle" (ohBrowse).
END.             
ELSE DO: 
  AddBrowseTriggers(ihRectangle,ihProcedure).
  ohBrowse = ihRectangle.
  ohBrowse:COLUMN-RESIZABLE = YES.
END.

IF getAttribute(SESSION,"shadedRows") = "yes" THEN
  setAttribute(ohBrowse,"shadedRows","yes").

IF getAttribute(ohBrowse:WINDOW,"HelpTextWidget") NE "" THEN DO:
  ON ENTRY OF ohBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"entry-of-widget").
  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ohBrowse
         ttEvent.hWidget       = ohBrowse
         ttEvent.cAction       = "entry-of-widget"
         ttEvent.cName         = "entry-of-widget"
         ttEvent.cWidgetType   = "browse"
         ttEvent.cMethod       = "EntryOfWidget".
END.

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "start-search"
       ttEvent.cName         = "start-search"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "StartSearch".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "off-end"
       ttEvent.cName         = "off-end"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "OffEnd".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "off-home"
       ttEvent.cName         = "off-home"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "OffHome".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "value-changed"
       ttEvent.cName         = "value-changed"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "DisplayRecord".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "row-entry"
       ttEvent.cName         = "row-entry"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "RowEntry".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "row-leave"
       ttEvent.cName         = "row-leave"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "RowLeave".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "open-query"
       ttEvent.cName         = "open-query"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "OpenQuery".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "view-overlay"
       ttEvent.cName         = "view-overlay"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "EndResizeBrowseColumn".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "scroll-notify"
       ttEvent.cName         = "scroll-notify"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ScrollNotifyBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "row-display"
       ttEvent.cName         = "row-display"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "RowDisplayBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "last-record"
       ttEvent.cName         = "last-record"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "LastRecord".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "first-record"
       ttEvent.cName         = "first-record"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "FirstRecord".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "any-printable"
       ttEvent.cName         = "any-printable"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "AnyPrintableBrowseColumn".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "default-action"
       ttEvent.cName         = "default-action"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "DefaultActionBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "new-record"
       ttEvent.cName         = "new-record"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "NewRecord".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "delete-record"
       ttEvent.cName         = "delete-record"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "DeleteRecord".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "refresh-record"
       ttEvent.cName         = "refresh-record"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "RefreshBrowseRecord".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "tab"
       ttEvent.cName         = "tab"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "TabFromBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "back-tab"
       ttEvent.cName         = "back-tab"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "BackTabFromBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "drop-file-notify"
       ttEvent.cName         = "drop-file-notify"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "DropFileNotifyBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-cursor-down"
       ttEvent.cName         = "shift-cursor-down"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftCursorDownBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-cursor-up"
       ttEvent.cName         = "shift-cursor-up"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftCursorUpBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-page-down"
       ttEvent.cName         = "shift-page-down"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftPageDownBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-page-up"
       ttEvent.cName         = "shift-page-up"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftPageUpBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-home"
       ttEvent.cName         = "shift-home"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftHomeBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-end"
       ttEvent.cName         = "shift-end"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftEndBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "mouse-menu-down"
       ttEvent.cName         = "mouse-menu-down"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "MouseMenuDownBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "ctrl-cursor-right"
       ttEvent.cName         = "ctrl-cursor-right"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "CtrlCursorRightBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "ctrl-cursor-left"
       ttEvent.cName         = "ctrl-cursor-left"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "CtrlCursorLeftBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-ctrl-cursor-right"
       ttEvent.cName         = "shift-ctrl-cursor-right"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftCtrlCursorRightBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "shift-ctrl-cursor-left"
       ttEvent.cName         = "shift-ctrl-cursor-left"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ShiftCtrlCursorLeftBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "alt-cursor-down"
       ttEvent.cName         = "alt-cursor-down"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "AltCursorDownBrowse".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "enter-browse-search"
       ttEvent.cName         = "enter-browse-search"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "EnterBrowseSearch".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ohBrowse
       ttEvent.hWidget       = ohBrowse
       ttEvent.cAction       = "excel"
       ttEvent.cName         = "excel"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ExcelRecord".

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFillBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoFillBrowse Procedure 
PROCEDURE DoFillBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse           AS HANDLE NO-UNDO.
DEF INPUT PARAM iBatchSize         AS INT    NO-UNDO.
DEF INPUT PARAM iStartRow          AS INT    NO-UNDO.
DEF INPUT PARAM icBuffersAndFields AS CHAR   NO-UNDO.
DEF INPUT PARAM icQueryCriteria    AS CHAR   NO-UNDO.
DEF INPUT PARAM icSortExpr       AS CHAR   NO-UNDO.
DEF INPUT PARAM ibDesc             AS LOG    NO-UNDO.
DEF OUTPUT PARAM iMaxCount         AS INT    NO-UNDO.

DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR rBuffer           AS ROWID  NO-UNDO.
DEF VAR cFindString       AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR iNumRowIds        AS INT    NO-UNDO.
DEF VAR iTotCount         AS INT    NO-UNDO.
DEF VAR iCount            AS INT    NO-UNDO.
DEF VAR cJoin             AS CHAR   NO-UNDO.
DEF VAR hField            AS HANDLE NO-UNDO.
DEF VAR cSkipUniqueRows   AS CHAR   NO-UNDO.
DEF VAR cSortMap          AS CHAR   NO-UNDO.
DEF VAR cLastRowid        AS CHAR   NO-UNDO.
DEF VAR cFirstRowid       AS CHAR   NO-UNDO.
DEF VAR iNumServerRec     AS INT    NO-UNDO.
DEF VAR iTmpStartRow      AS INT    NO-UNDO.
DEF VAR bWhereFound       AS LOG    NO-UNDO.
DEF VAR iy                AS INT    NO-UNDO.
DEF VAR hTmp              AS HANDLE NO-UNDO.
DEF VAR hWin              AS HANDLE NO-UNDO.
DEF VAR cCheckedRowIds    AS CHAR   NO-UNDO.
DEF VAR cDistinctColumns  AS CHAR   NO-UNDO.
DEF VAR cAccumFields      AS CHAR   NO-UNDO.
DEF VAR cStatValues       AS CHAR   NO-UNDO.
DEF VAR cDistBufFldList   AS CHAR   NO-UNDO.
DEF VAR cAccBufFldList    AS CHAR   NO-UNDO.
DEF VAR cCurrBuffer       AS CHAR   NO-UNDO.
                                    
DEF VAR cTmp                AS CHAR   NO-UNDO.
DEF VAR cTemp               AS CHAR   NO-UNDO.
DEF VAR cTemp2              AS CHAR   NO-UNDO.
DEF VAR cFieldDef           AS CHAR   NO-UNDO.
DEF VAR bCalcField          AS LOG    NO-UNDO.
DEF VAR cMatchQuery         AS CHAR   NO-UNDO.
DEF VAR hTmpQuery           AS HANDLE NO-UNDO.
DEF VAR hUseAppServer       AS HANDLE NO-UNDO.
DEF VAR hAltExtentFields    AS HANDLE NO-UNDO EXTENT 50.
DEF VAR hExtentFields       AS HANDLE NO-UNDO EXTENT 50.
DEF VAR cExtentFields       AS CHAR   NO-UNDO.
DEF VAR cAltExtFieldHandles AS CHAR   NO-UNDO.
DEF VAR iCntExtFields       AS INT    NO-UNDO.
DEF VAR cExcludeLocalFields AS CHAR   NO-UNDO.
DEF VAR cExcludedFields     AS CHAR   NO-UNDO. 

DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR httTable            AS HANDLE NO-UNDO.
DEF VAR httTableBuffer      AS HANDLE NO-UNDO.
DEF VAR httTableQuery       AS HANDLE NO-UNDO.
DEF VAR bBufferSwap         AS LOG    NO-UNDO.
DEF VAR cCompanyTag         AS CHAR   NO-UNDO.
DEF VAR cSortString         AS CHAR   NO-UNDO.
DEF VAR bAccumDistinct      AS LOG    NO-UNDO.
DEF VAR cMyQuerySort        AS CHAR   NO-UNDO.
DEF VAR cMyLocalSort        AS CHAR   NO-UNDO.
DEF VAR cModQuerySort       AS CHAR   NO-UNDO.
DEF VAR cModLocalSort       AS CHAR   NO-UNDO.
DEF VAR cQueryWhere         AS CHAR   NO-UNDO.
DEF VAR cQueryFilter        AS CHAR   NO-UNDO.
DEF VAR cBaseQuery          AS CHAR   NO-UNDO.

/*{JBoxObjectDef.i}*/
DEF BUFFER bbttObjectLink FOR ttObjectLink.

{incl/methodlog.i ihBrowse:NAME}

IF icQueryCriteria = "" THEN DO:
  PUBLISH "ControlBufferSwap" (ihBrowse,OUTPUT bBufferSwap).
  ASSIGN cBaseQuery   = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"baseQuery")
         cQueryFilter = TRIM(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryFilter"))
         cQueryWhere  = TRIM(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryWhere"))
         .
  IF cBaseQuery NE "" THEN DO:
    IF cQueryFilter BEGINS "WHERE " THEN DO:
      cQueryFilter = " AND" + SUBSTR(cQueryFilter,6).
      setAttribute(ihBrowse,"queryFilter",cQueryFilter).
    END.    
    IF cQueryWhere BEGINS "WHERE " THEN DO:
      cQueryWhere = " AND" + SUBSTR(cQueryWhere,6).
      setAttribute(ihBrowse,"queryWhere",cQueryWhere).
    END.    
  END.         
  IF (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryWhere") = "" 
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryFilter") = "" 
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanquerywhere") = ""
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter") = ""
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"uselocaldata") NE "yes" 
     AND NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"allcalcfields"),ENTRY(1,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querysort")," "))
     ) OR bBufferSwap
     THEN DO:
    IF CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"altPrimaryBufferList"),DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer")) THEN
      DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",ihBrowse,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer"),
                                IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"preScanQuery" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer")) NE "" THEN
                                  "," + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"preScanQuery" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer"))
                                ELSE "WHERE true,EACH " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"basetable") + " NO-LOCK OF " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer")
                                ).
    ELSE DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",ihBrowse,"","").
  END.
  
  ASSIGN icBuffersAndFields = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"buffersandfields")
         icQueryCriteria    = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"basequery") + 
                              DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryfilter") + 
                              DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querywhere") +
                             (IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index_" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn")) NE "" THEN
                                " USE-INDEX " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index_" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn"))
                              ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index") NE "" THEN
                                " USE-INDEX " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index")
                              ELSE "") +  
                              DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryjoin")
         icSortExpr       = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querysort")
         .

END.


ASSIGN cTemp = REPLACE(icBuffersAndFields,"!","")
       cTemp = REPLACE(cTemp,";RowCount","")
       cTemp = REPLACE(cTemp,";RowIdent1","")
       cTemp = REPLACE(cTemp,";RowIdent2","")
       cTemp = REPLACE(cTemp,";RowIdent3","")
       cTemp = REPLACE(cTemp,";RowIdent4","")
       cTemp = REPLACE(cTemp,";RowIdent5","")
       cTemp = REPLACE(cTemp,";RowIdent6","")
       cTemp = REPLACE(cTemp,";RowIdent7","")
       cTemp = REPLACE(cTemp,";RowIdent8","")
       cTemp = REPLACE(cTemp,";RowIdent9","")
       cTemp = REPLACE(cTemp,";distinct ",";")
       cTemp = REPLACE(cTemp,"+distinct ","+")
       cTemp = REPLACE(cTemp,";accum ",";")
       cTemp = REPLACE(cTemp,"+accum ","+")
       icQueryCriteria = DYNAMIC-FUNCTION("FixQuery",icQueryCriteria)
       .

IF icQueryCriteria MATCHES "*(*Company)" OR icQueryCriteria MATCHES "*(Codemaster)" THEN DO:
  ASSIGN cCompanyTag     = SUBSTR(icQueryCriteria,R-INDEX(icQueryCriteria,"("))
         icQueryCriteria = SUBSTR(icQueryCriteria,1,R-INDEX(icQueryCriteria,"(") - 1)
         cQueryWhere     = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryWhere").
  IF cQueryWhere MATCHES "*(*Company)" OR cQueryWhere MATCHES "*(Codemaster)" THEN
    cQueryWhere = SUBSTR(cQueryWhere,1,R-INDEX(cQueryWhere,"(") - 1).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"queryWhere",cQueryWhere).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"CompanyTag",cCompanyTag).
END.

hWin = ihBrowse:WINDOW NO-ERROR.
IF VALID-HANDLE(hWin) THEN
  DYNAMIC-FUNCTION("DoLockWindow",ihBrowse:WINDOW).

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryjoin") = "" THEN DO:
  DO ix = 1 TO LENGTH(icQueryCriteria):
    IF SUBSTR(icQueryCriteria,ix,6) = "first " OR
       SUBSTR(icQueryCriteria,ix,5) = "last " OR
       SUBSTR(icQueryCriteria,ix,5) = "each " 
       THEN DO:
      DO iy = ix TO ix - 10 BY -1:
        IF SUBSTR(icQueryCriteria,iy,1) = "," THEN LEAVE.
      END.
      LEAVE.
    END.
  END.
  IF iy > 0 THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"queryjoin",SUBSTR(icQueryCriteria,iy)).
END.

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"enableondblclick") = "yes" THEN 
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"enableupdate","no").

cDistinctColumns = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctcolumns").
cAccumFields = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumfields").

IF cDistinctColumns NE "" OR cAccumFields NE "" THEN DO:
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF CAN-DO(cDistinctColumns,ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN
      cDistBufFldList = cDistBufFldList + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldbuffer" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) 
                      + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgdbfield" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) + ",".
    ELSE IF CAN-DO(cAccumFields,ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN
      cAccBufFldList = cAccBufFldList + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldbuffer" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) 
                      + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgdbfield" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) + ",".
  END.
  ASSIGN cDistBufFldList = TRIM(cDistBufFldList,",")
         cAccBufFldList  = TRIM(cAccBufFldList,",").
  DO ix = 1 TO NUM-ENTRIES(cTemp):
    ASSIGN cTemp2 = cTemp2 + (IF cTemp2 NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,cTemp),";") + ";"
           cCurrBuffer = ENTRY(1,ENTRY(ix,cTemp),";"). 
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,cTemp),";"):
      ASSIGN cFieldDef = TRIM(ENTRY(1,ENTRY(iy,ENTRY(ix,cTemp),";"),"|"),"+")
             bCalcField = ENTRY(iy,ENTRY(ix,cTemp),";") BEGINS "+".
      IF cDistinctColumns NE "" AND 
         CAN-DO(cDistBufFldList,cCurrBuffer + cFieldDef) AND
         (CAN-DO(cDistinctColumns,cFieldDef) OR
          CAN-DO(cDistinctColumns,cFieldDef + STRING(ix))) THEN
        cTemp2 = cTemp2 + (IF bCalcField THEN "+" ELSE "") + "distinct " + TRIM(ENTRY(iy,ENTRY(ix,cTemp),";"),"+") + ";".
      ELSE IF cAccumFields NE "" AND 
         CAN-DO(cAccBufFldList,cCurrBuffer + cFieldDef) AND
         (CAN-DO(cAccumFields,cFieldDef) OR
          CAN-DO(cAccumFields,cFieldDef + STRING(ix))) THEN
        cTemp2 = cTemp2 + (IF bCalcField THEN "+" ELSE "") + "accum " + TRIM(ENTRY(iy,ENTRY(ix,cTemp),";"),"+") + ";".
      ELSE
        cTemp2 = cTemp2 + ENTRY(iy,ENTRY(ix,cTemp),";") + ";".
    END.
    cTemp2 = TRIM(cTemp2,";").
  END.
  cTemp = cTemp2.
END.
  
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"rowsadded","0").
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"rowsdeleted","0").

bAccumDistinct = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctColumns") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumFields") NE "".

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"uselocaldata") = "yes" AND NOT bAccumDistinct THEN DO:
  IF VALID-HANDLE(hCurrSourceProc) AND CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"FillLocalBrowse") THEN
    iMaxCount = DYNAMIC-FUNCTION("FillLocalBrowse" IN hCurrSourceProc,ihBrowse,cTemp,icQueryCriteria,icSortExpr,ibDesc).
  ELSE
    iMaxCount = DYNAMIC-FUNCTION("FillLocalBrowse",ihBrowse,cTemp,icQueryCriteria,icSortExpr,ibDesc).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"recordcount",STRING(iMaxCount)).
END.
ELSE DO:
  /* If we for some reason have a local copy, purge it now since we are getting a fresh load: */
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"originaltemptable") NE "" THEN DO:
    hTmp = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"originaltemptable")) NO-ERROR.
    IF VALID-HANDLE(hTmp) THEN DO:
      DYNAMIC-FUNCTION("DeleteObject",hTmp).
      DELETE OBJECT hTmp NO-ERROR.
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"originaltemptable","").
    END.
  END.

  cTemp = DYNAMIC-FUNCTION("AddCalcParam",cTemp,ihBrowse).
  
  hBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).
  iTmpStartRow = iStartRow.
  
  IF iStartRow = 0 AND ihBrowse:QUERY:IS-OPEN THEN 
    hBuffer:EMPTY-TEMP-TABLE().
  ELSE IF iStartRow > 0 THEN DO:
    ihBrowse:SET-REPOSITIONED-ROW(ihBrowse:DOWN,"Conditional").
    iTotCount   = INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"totalcount")).
    cFirstRowId = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid").
  END.
  
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepsearchvalue") = "yes" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter") NE "" AND
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"basetable") = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortbuffer") 
     THEN DO:
    FIND FIRST bbttObjectLink
         WHERE bbttObjectLink.hFromObject = ihBrowse
           AND bbttObjectLink.cLinkType   = "browse-search-field"
         NO-ERROR.
    IF AVAIL bbttObjectLink AND 
       DYNAMIC-FUNCTION("getBufferFieldDataType",ihBrowse:QUERY:GET-BUFFER-HANDLE(1),bbttObjectLink.cLinkInfo) = "character" AND
       INDEX(bbttObjectLink.hToObject:INPUT-VALUE,"*") > 0 THEN
      cMatchQuery = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortbuffer") + " WHERE " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortbuffer") + "." + bbttObjectLink.cLinkInfo
                  + " MATCHES '" + bbttObjectLink.hToObject:INPUT-VALUE + "'".
  END.
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanBaseQuery") NE "" OR 
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanQueryFilter") NE "" OR 
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanQueryWhere") NE "" THEN
    DYNAMIC-FUNCTION("setPreScanQuery",TRIM(REPLACE(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanBaseQuery"),"|",CHR(3)) + CHR(28) + 
                                            REPLACE(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter"),"|",CHR(3)) + CHR(28) + 
                                            REPLACE(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanquerywhere"),"|",CHR(3)) + CHR(28) +
                                            REPLACE(cMatchQuery,"|",CHR(3))
                                            ,"|")).
  ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter") NE "" THEN
    DYNAMIC-FUNCTION("setPreScanQuery",REPLACE(cMatchQuery,"|",CHR(3))).

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanlimit") NE "" THEN
    DYNAMIC-FUNCTION("setPreScanLimit",INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanlimit"))).
  DYNAMIC-FUNCTION("setCalcFieldProc",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldproc")).
  DYNAMIC-FUNCTION("setCalcFieldFilter",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldfilter")
                   + (IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldfilter") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldwhere") NE "" THEN "|" ELSE "")
                   + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldwhere")
                   ).
  DYNAMIC-FUNCTION("setNoExistBuffers",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"noexistbuffers")).

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"useAppserver") NE "" THEN DO:
    hUseAppServer = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",SESSION,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"useAppserver"))) NO-ERROR.
    IF VALID-HANDLE(hUseAppServer) THEN
      DYNAMIC-FUNCTION("setUseAppserver",hUseAppServer).
  END.

  DYNAMIC-FUNCTION("setUniqueBuffer",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"uniqueBuffer")).
  DYNAMIC-FUNCTION("setQueryStopAfter",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryStopAfter")).
  DYNAMIC-FUNCTION("setOrgBuffersAndFields",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgBuffersAndFields")).

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"NoMandatoryCalcFields") NE "" THEN 
    DYNAMIC-FUNCTION("setSkipCalcFields",ihBrowse).

  IF icSortExpr = "" THEN icSortExpr = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort").
  
  IF DYNAMIC-FUNCTION("getAttributeList",ihBrowse,"myQuerySort*","",NO) NE "|" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(icSortExpr," "):
      cTmp = ENTRY(ix,icSortExpr," ").
      cTmp = ENTRY(NUM-ENTRIES(cTmp,"."),cTmp,".").
    
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myQuerySort" + cTmp) NE "" THEN DO:
        cMyQuerySort = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myQuerySort" + cTmp).
        cMyQuerySort = REPLACE(cMyQuerySort," <desc> ",IF ibDesc OR (ix < NUM-ENTRIES(icSortExpr," ") AND ENTRY(ix + 1,icSortExpr," ") BEGINS "desc") THEN " desc " ELSE " ").
        IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myLocalSort" + cTmp) NE "" THEN
          ASSIGN cMyLocalSort = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myLocalSort" + cTmp)
                 cMyLocalSort = REPLACE(cMyLocalSort," <desc> ",IF ibDesc THEN " desc " ELSE " ").
        ELSE
          DO iy = 1 TO NUM-ENTRIES(cMyQuerySort," "):   
            cTemp2 = ENTRY(iy,cMyQuerySort," ").  
            cMyLocalSort = cMyLocalSort + (IF cMyLocalSort NE "" THEN " " ELSE "") + ENTRY(NUM-ENTRIES(cTemp2,"."),cTemp2,".").
          END.  
        ASSIGN cModQuerySort = cModQuerySort + cMyQuerySort + " "
               cModLocalSort = cModLocalSort + cMyLocalSort + " "
               .    
      END.
      ELSE ASSIGN cModQuerySort = cModQuerySort + ENTRY(ix,icSortExpr," ") + " "
                  cModLocalSort = cModLocalSort + cTmp + " "
                  .    
    END. 
    icSortExpr = cModQuerySort. 
  END.

  httTable = DYNAMIC-FUNCTION("getTempTableJoin",
                              iBatchSize,
                              IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"SmartComponentLibrary_QueryStart") NE "" THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"SmartComponentLibrary_QueryStart"))
                              ELSE iStartRow,
                              DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir"),
                              cTemp,
                              icQueryCriteria 
                            + (IF icSortExpr NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"skipServerSort") NE "yes" THEN
                                " BY " + icSortExpr + (IF ibDesc THEN " DESC" ELSE "")
                               ELSE "")
                            + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"CompanyTag")
                              ).
  
  httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httTableQuery NO-ERROR.
  httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
  httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
  httTableQuery:QUERY-OPEN.
  cJoin = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryjoin").
  iNumRowIds = NUM-ENTRIES(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"buffersandfields")).
  
  cSkipUniqueRows = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"SkipUniqueRows").
  DO ix = 1 TO NUM-ENTRIES(cSkipUniqueRows):
    hField = hBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)) NO-ERROR.
    IF NOT VALID-HANDLE(hField) THEN LEAVE.
    ELSE cCheckedRowIds = cCheckedRowIds + ENTRY(ix,cSkipUniqueRows) + ",".
  END.
  cSkipUniqueRows = TRIM(cCheckedRowIds,",").
  
  ASSIGN cExtentFields       = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"extentFields")
         cAltExtFieldHandles = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"AltExtentFieldHandles")
         cExcludeLocalFields = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localcalcfields").
  DO ix = 1 TO NUM-ENTRIES(cExtentFields):
    ASSIGN hAltExtentFields[ix] = WIDGET-HANDLE(ENTRY(ix,cAltExtFieldHandles))
/*            hExtentFields[ix]    = httTableBuffer:BUFFER-FIELD(ENTRY(ix,cExtentFields)) */
           iCntExtFields        = iCntExtFields + 1.
    hExtentFields[ix]    = httTableBuffer:BUFFER-FIELD(ENTRY(ix,cExtentFields)) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
      cFieldDef = ENTRY(ix,cExtentFields).
      hAltExtentFields[ix] = hBuffer:BUFFER-FIELD(cFieldDef).
      cFieldDef = SUBSTR(cFieldDef,1,R-INDEX(cFieldDef,"_") - 1) + "[" + SUBSTR(cFieldDef,R-INDEX(cFieldDef,"_") + 1) + "]".
      hExtentFields[ix] = httTableBuffer:BUFFER-FIELD(cFieldDef).
    END.
  END.
  DO ix = 1 TO NUM-ENTRIES(cExcludeLocalFields):
    IF ENTRY(ix,cExcludeLocalFields) MATCHES "*]" THEN DO:
      IF NOT CAN-DO(cExcludedFields,SUBSTR(ENTRY(ix,cExcludeLocalFields),1,R-INDEX(ENTRY(ix,cExcludeLocalFields),"[") - 1)) THEN
        cExcludedFields = cExcludedFields + (IF cExcludedFields NE "" THEN "," ELSE "") + SUBSTR(ENTRY(ix,cExcludeLocalFields),1,R-INDEX(ENTRY(ix,cExcludeLocalFields),"[") - 1).
    END.
    ELSE cExcludedFields = cExcludedFields + (IF cExcludedFields NE "" THEN "," ELSE "") + ENTRY(ix,cExcludeLocalFields).
  END.

  
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "" THEN
    httTableQuery:GET-FIRST().
  ELSE
    httTableQuery:GET-LAST().
  REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:
    ASSIGN cFindString   = "WHERE "
           iNumServerRec = iNumServerRec + 1
           .
    /*       
    IF cSkipUniqueRows NE "" THEN
      DO ix = 1 TO NUM-ENTRIES(cSkipUniqueRows):
        cFindString = cFindString + "RowIdent" + ENTRY(ix,cSkipUniqueRows) + " = '" + httTableBuffer:BUFFER-FIELD("RowIdent" + ENTRY(ix,cSkipUniqueRows)):BUFFER-VALUE + "' AND ".
      END.
    ELSE
    */
      DO ix = 1 TO iNumRowIds:
        cFindString = cFindString + "RowIdent" + STRING(ix) + " = '" + httTableBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)):BUFFER-VALUE + "' AND ".
      END.
    cFindString = RIGHT-TRIM(cFindString," AND ").
    bOk = hBuffer:FIND-UNIQUE(cFindString) NO-ERROR.
    IF NOT bOk THEN DO:
      hBuffer:BUFFER-CREATE().
      hBuffer:BUFFER-COPY(httTableBuffer).
      ASSIGN iTotCount   = iTotCount + 1
             iCount      = iCount    + 1
             cFirstRowid = IF cFirstRowid = "" THEN STRING(hBuffer:ROWID) ELSE cFirstRowid
             .
    END.
    ELSE hBuffer:BUFFER-COPY(httTableBuffer,cExcludedFields).  

    DO ix = 1 TO iCntExtFields:
      hAltExtentFields[ix]:BUFFER-VALUE = hExtentFields[ix]:BUFFER-VALUE.
    END.
  
    cLastRowid  = STRING(hBuffer:ROWID).
  
    IF iStartRow > 0 THEN DO:
      ihBrowse:SET-REPOSITIONED-ROW(ihBrowse:DOWN,"Conditional").
      rBuffer = hBuffer:ROWID.
      iStartRow = 0.
    END.
    iMaxCount = IF httTableBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE > iMaxCount THEN httTableBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE ELSE iMaxCount.
  
    IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "" THEN
      httTableQuery:GET-NEXT().
    ELSE 
      httTableQuery:GET-PREV().
  END.
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"totalcount",STRING(iTotCount)).
  
  IF iCount > 0 THEN DO:
  
    /* If we go from the start or we go from the end and retrieve all records, set firstrowid to cFirstRowid: */ 
    IF (iNumServerRec < INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"rowstobatch")) AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "") 
       OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "" AND iTmpStartRow = 0) THEN
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid",cFirstRowid).
    ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "" THEN
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid","").
  
    /* If we go from the end or we go from the start and retrieve all records, set lastrowid to cLastRowid: */ 
    IF iNumServerRec < INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"rowstobatch")) 
        OR DYNAMIC-FUNCTION("getAttribute",ihBrowse,"rowstobatch") = "0"
        OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "" AND iTmpStartRow = 0) THEN
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid",cLastRowid).
    ELSE
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid","").

    IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" THEN icSortExpr = DYNAMIC-FUNCTION("FixSortString",ihBrowse).
    
    IF cModLocalSort NE "" THEN 
      icSortExpr = cModLocalSort.
    
    ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME  
                               + (IF icSortExpr NE ""
                                    OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"lastrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "")
                                    OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "")
                                    THEN
                                   " BY " + icSortExpr
                                   + (IF ibDesc THEN " DESC" ELSE "")
                                  ELSE "")
                               + " BY RowCount"
                                 ) NO-ERROR.

    IF ERROR-STATUS:GET-NUMBER(1) = 7328 THEN
      ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " BY RowCount").

    hTmpQuery = ihBrowse:QUERY NO-ERROR.

    IF VALID-HANDLE(hTmpQuery) THEN
      bOk = hTmpQuery:QUERY-OPEN() NO-ERROR.
    ELSE DO: 
      IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN DO:
        MESSAGE PROGRAM-NAME(1) SKIP
                "Invalid query handle: " hTmpQuery SKIP
                VIEW-AS ALERT-BOX ERROR.
        iMaxCount = 0.
        RETURN.
      END.
    END.

    IF NOT bOk AND ERROR-STATUS:GET-NUMBER(1) = 141 THEN DO:
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stDbSortColumn")) = "CHARACTER" THEN
          cSortString = " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn") + ",1,30)".
        ELSE
          cSortString = " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumndDesc").
      END.
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndDbSortColumn")) = "CHARACTER" THEN
          cSortString = cSortString + " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumn") + ",1,30)".
        ELSE
          cSortString = cSortString + " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumndDesc").
      END.
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdDbSortColumn")) = "CHARACTER" THEN
          cSortString = cSortString + " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumn") + ",1,30)".
        ELSE
          cSortString = cSortString + " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumndDesc").
      END.
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thDbSortColumn")) = "CHARACTER" THEN
          cSortString = cSortString + " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumn") + ",1,30)".
        ELSE
          cSortString = cSortString + " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumndDesc").
      END.
      IF cSortString = "" THEN DO:
        ASSIGN cSortString  = "SUBSTR("
               icSortExpr = " BY " + icSortExpr.
        DO ix = 1 TO LENGTH(icSortExpr):
          cSortString = cSortString + IF SUBSTR(icSortExpr,ix,4) = " BY " THEN " BY SUBSTR("
                                      ELSE IF SUBSTR(icSortExpr,ix,1) = " " AND SUBSTR(icSortExpr,ix - 3,4) NE " BY " THEN ",1,50) "
                                      ELSE IF SUBSTR(icSortExpr,ix - 1,4) NE " BY " AND SUBSTR(icSortExpr,ix - 2,4) NE " BY " THEN SUBSTR(icSortExpr,ix,1)
                                      ELSE "".
        END.
        IF R-INDEX(icSortExpr,"DESC") = 0 OR R-INDEX(cSortString,"(") > R-INDEX(cSortString,")") THEN
          cSortString = cSortString + ",1,50) ".
      END.

      ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME 
                                 + (IF icSortExpr NE ""
                                      OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"lastrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "")
                                      OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "")
                                      THEN
                                    cSortString 
                                    ELSE "")
                                 + " BY RowCount"
                                   ).
      bOK = ihBrowse:QUERY:QUERY-OPEN() NO-ERROR.
    END.
    
    IF bSetSortLabel THEN DO:
      cSortMap = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortmap").
      DO ix = 1 TO NUM-ENTRIES(cSortMap):
        IF ENTRY(2,ENTRY(ix,cSortMap),";") = icSortExpr THEN 
          icSortExpr = ENTRY(1,ENTRY(ix,cSortMap),";").
      END.
      IF icSortExpr = "" AND NOT (
           (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"lastrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "") OR
           (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "")) THEN
        DYNAMIC-FUNCTION("setAttribute",ihBrowse,"localsort","").
      DYNAMIC-FUNCTION("setSortLabel",ihBrowse,(IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" THEN DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") ELSE icSortExpr),ibDesc).
    END.
  
    IF rBuffer NE ? THEN
      ihBrowse:QUERY:REPOSITION-TO-ROWID(rBuffer).
    ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "" THEN DO: 
      ihBrowse:QUERY:GET-LAST().
      IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
        DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid",STRING(ihBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)).
    END.
    ELSE DO:
      ihBrowse:QUERY:GET-FIRST().
      IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
        DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid",STRING(ihBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)).
    END.
  END.
  
  DELETE OBJECT httTableQuery.
  DELETE OBJECT httTable.
  
  IF VALID-HANDLE(hWin) THEN
    DYNAMIC-FUNCTION("DoLockWindow",?).

  IF DYNAMIC-FUNCTION("getQueryStatFieldValues") NE "" THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"recordcount",ENTRY(1,ENTRY(2,DYNAMIC-FUNCTION("getQueryStatFieldValues"),"|"),";")).
  ELSE DO:
    IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"getrecordcount") = "" THEN DYNAMIC-FUNCTION("setAttribute",ihBrowse,"recordcount","").
    IF NOT PROGRAM-NAME(2) BEGINS "StartSearch" AND NOT PROGRAM-NAME(2) BEGINS("MultiSortBrowse") THEN 
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"currentcount",STRING((IF cSkipUniqueRows NE "" THEN iTotCount ELSE iMaxCount))).
  END.

  IF INDEX(DYNAMIC-FUNCTION("getQueryStatFieldValues"),";") > 0 THEN DO:
    cStatValues = DYNAMIC-FUNCTION("getQueryStatFieldValues").
    DO ix = 2 TO NUM-ENTRIES(cStatValues,";"):
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"statvalue" + ENTRY(1,ENTRY(ix,cStatValues,";"),"|"),ENTRY(2,ENTRY(ix,cStatValues,";"),"|")).
    END.
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"QueryStatFieldValues",SUBSTR(DYNAMIC-FUNCTION("getQueryStatFieldValues"),INDEX(DYNAMIC-FUNCTION("getQueryStatFieldValues"),";") + 1)).
  END.

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"GoLocalWhenSmallDistinctCount") = "yes" AND 
     INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"recordcount")) < 3000 AND
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctcolumns") NE "" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"uselocaldata","yes").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid","").
  END.
END.

IF VALID-HANDLE(hCurrSourceProc) AND CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ViewRecordCount") THEN
  DYNAMIC-FUNCTION("ViewRecordCount" IN hCurrSourceProc,ihBrowse).
ELSE
  DYNAMIC-FUNCTION("ViewRecordCount",ihBrowse).

IF VALID-HANDLE(hCurrSourceProc) AND CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ViewQueryStat") THEN
  DYNAMIC-FUNCTION("ViewQueryStat" IN hCurrSourceProc,ihBrowse).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFillQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoFillQuery Procedure 
PROCEDURE DoFillQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihQuery            AS HANDLE NO-UNDO.
DEF INPUT  PARAM iBatchSize         AS INT    NO-UNDO.
DEF INPUT  PARAM iStartRow          AS INT    NO-UNDO.
DEF INPUT  PARAM icBuffersAndFields AS CHAR   NO-UNDO.
DEF INPUT  PARAM icQueryCriteria    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM iMaxCount          AS INT    NO-UNDO.

DEF VAR hBuffer             AS HANDLE NO-UNDO.
DEF VAR rBuffer             AS ROWID  NO-UNDO.
DEF VAR cFindString         AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR iNumRowIds          AS INT    NO-UNDO.
DEF VAR iTotCount           AS INT    NO-UNDO.
DEF VAR iCount              AS INT    NO-UNDO.
DEF VAR iy                  AS INT    NO-UNDO.
DEF VAR cJoin               AS CHAR   NO-UNDO.
DEF VAR hField              AS HANDLE NO-UNDO.
DEF VAR cSkipUniqueRows     AS CHAR   NO-UNDO.
DEF VAR cSortMap            AS CHAR   NO-UNDO.
DEF VAR cTemp               AS CHAR   NO-UNDO.
DEF VAR cFirstRowid         AS CHAR   NO-UNDO.
DEF VAR cLastRowid          AS CHAR   NO-UNDO.
DEF VAR iNumServerRec       AS INT    NO-UNDO.
DEF VAR iTmpStartRow        AS INT    NO-UNDO.
DEF VAR hTmpQuery           AS HANDLE NO-UNDO.
DEF VAR cSortColumn         AS CHAR   NO-UNDO.
DEF VAR hAltExtentFields    AS HANDLE NO-UNDO EXTENT 50.
DEF VAR hExtentFields       AS HANDLE NO-UNDO EXTENT 50.
DEF VAR cExtentFields       AS CHAR   NO-UNDO.
DEF VAR cAltExtFieldHandles AS CHAR   NO-UNDO.
DEF VAR iCntExtFields       AS INT    NO-UNDO.
DEF VAR cStatValues         AS CHAR   NO-UNDO.
DEF VAR cExcludeLocalFields AS CHAR   NO-UNDO.
DEF VAR cExcludedFields     AS CHAR   NO-UNDO.       
DEF VAR cCheckedRowIds      AS CHAR   NO-UNDO.
DEF VAR cFieldDef           AS CHAR   NO-UNDO.
DEF VAR cQueryWhere         AS CHAR   NO-UNDO.
DEF VAR cQueryFilter        AS CHAR   NO-UNDO.
DEF VAR cBaseQuery          AS CHAR   NO-UNDO.
DEF VAR cCompanyTag         AS CHAR   NO-UNDO.
DEF VAR hUseAppServer       AS HANDLE NO-UNDO.

IF icQueryCriteria = "" THEN DO:
  ASSIGN cBaseQuery   = DYNAMIC-FUNCTION("getAttribute",ihQuery,"baseQuery")
         cQueryFilter = TRIM(DYNAMIC-FUNCTION("getAttribute",ihQuery,"queryFilter"))
         cQueryWhere  = TRIM(DYNAMIC-FUNCTION("getAttribute",ihQuery,"queryWhere"))
         .
  IF cBaseQuery NE "" THEN DO:
    IF cQueryFilter BEGINS "WHERE " THEN DO:
      cQueryFilter = " AND" + SUBSTR(cQueryFilter,6).
      setAttribute(ihQuery,"queryFilter",cQueryFilter).
    END.    
    IF cQueryWhere BEGINS "WHERE " THEN DO:
      cQueryWhere = " AND" + SUBSTR(cQueryWhere,6).
      setAttribute(ihQuery,"queryWhere",cQueryWhere).
    END.    
  END.         
  
  IF getAttribute(ihQuery,"queryWhere") = "" 
     AND getAttribute(ihQuery,"queryFilter") = "" 
     AND getAttribute(ihQuery,"prescanquerywhere") = ""
     AND getAttribute(ihQuery,"prescanqueryfilter") = ""
     AND getAttribute(ihQuery,"uselocaldata") NE "yes" 
     AND NOT CAN-DO(getAttribute(ihQuery,"allcalcfields"),ENTRY(1,getAttribute(ihQuery,"querysort")," "))
     THEN DO:
    IF CAN-DO(getAttribute(ihQuery,"altPrimaryBufferList"),getAttribute(ihQuery,"sortBuffer")) THEN
      DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",ihQuery,getAttribute(ihQuery,"sortBuffer"),
                                IF getAttribute(ihQuery,"preScanQuery" + getAttribute(ihQuery,"sortBuffer")) NE "" THEN
                                  "," + getAttribute(ihQuery,"preScanQuery" + getAttribute(ihQuery,"sortBuffer"))
                                ELSE "WHERE true,EACH " + getAttribute(ihQuery,"basetable") + " NO-LOCK OF " + getAttribute(ihQuery,"sortBuffer")
                                ).
    ELSE DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",ihQuery,"","").
  END.
  
  ASSIGN icBuffersAndFields = getAttribute(ihQuery,"buffersandfields")
         icQueryCriteria    = getAttribute(ihQuery,"basequery") + 
                              getAttribute(ihQuery,"queryfilter") + 
                              getAttribute(ihQuery,"querywhere") + 
                             (IF getAttribute(ihQuery,"use-index") NE "" THEN
                                " USE-INDEX " + getAttribute(ihQuery,"use-index")
                              ELSE "") +
                              getAttribute(ihQuery,"queryjoin")
                              .
END.

{incl/methodlog.i}

ASSIGN cTemp = REPLACE(icBuffersAndFields,"!","")
       cTemp = REPLACE(cTemp,";RowCount","")
       cTemp = REPLACE(cTemp,";RowIdent1","")
       cTemp = REPLACE(cTemp,";RowIdent2","")
       cTemp = REPLACE(cTemp,";RowIdent3","")
       cTemp = REPLACE(cTemp,";RowIdent4","")
       cTemp = REPLACE(cTemp,";RowIdent5","")
       cTemp = REPLACE(cTemp,";RowIdent6","")
       cTemp = REPLACE(cTemp,";RowIdent7","")
       cTemp = REPLACE(cTemp,";RowIdent8","")
       cTemp = REPLACE(cTemp,";RowIdent9","")
       icQueryCriteria = DYNAMIC-FUNCTION("FixQuery",icQueryCriteria)
       .
       
IF icQueryCriteria MATCHES "*(*Company)" OR icQueryCriteria MATCHES "*(Codemaster)" THEN DO:
  ASSIGN cCompanyTag     = SUBSTR(icQueryCriteria,R-INDEX(icQueryCriteria,"("))
         icQueryCriteria = SUBSTR(icQueryCriteria,1,R-INDEX(icQueryCriteria,"(") - 1)
         cQueryWhere     = DYNAMIC-FUNCTION("getAttribute",ihQuery,"queryWhere").
  IF cQueryWhere MATCHES "*(*Company)" OR cQueryWhere MATCHES "*(Codemaster)" THEN
    cQueryWhere = SUBSTR(cQueryWhere,1,R-INDEX(cQueryWhere,"(") - 1).
  DYNAMIC-FUNCTION("setAttribute",ihQuery,"queryWhere",cQueryWhere).
  DYNAMIC-FUNCTION("setAttribute",ihQuery,"CompanyTag",cCompanyTag).
END.

IF getAttribute(ihQuery,"querysort") NE "" AND 
   (DYNAMIC-FUNCTION("getAttribute",ihQuery,"skipServerSort") NE "yes" AND DYNAMIC-FUNCTION("getAttribute",ihQuery,"useLocalData") NE "yes") THEN
  icQueryCriteria = icQueryCriteria + " BY " + getAttribute(ihQuery,"querysort")
                  + (IF getAttribute(ihQuery,"querydesc") = "desc" THEN " DESCENDING" ELSE "").

IF getAttribute(ihQuery,"queryjoin") = "" THEN DO:
  DO ix = 1 TO LENGTH(icQueryCriteria):
    IF SUBSTR(icQueryCriteria,ix,6) = "first " OR
       SUBSTR(icQueryCriteria,ix,5) = "last " OR
       SUBSTR(icQueryCriteria,ix,5) = "each " 
       THEN DO:
      DO iy = ix TO ix - 10 BY -1:
        IF SUBSTR(icQueryCriteria,iy,1) = "," THEN LEAVE.
      END.
      LEAVE.
    END.
  END.
  IF iy > 0 THEN 
    setAttribute(ihQuery,"queryjoin",SUBSTR(icQueryCriteria,iy)).
END.

hBuffer = ihQuery:GET-BUFFER-HANDLE(1).
iTmpStartRow = iStartRow.


IF getAttribute(ihQuery,"uselocaldata") = "yes" THEN DO:
  IF VALID-HANDLE(hCurrSourceProc) AND CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"FillLocalQuery") THEN
    iMaxCount = DYNAMIC-FUNCTION("FillLocalQuery" IN hCurrSourceProc,ihQuery,cTemp,icQueryCriteria).
  ELSE
    iMaxCount = FillLocalQuery(ihQuery,cTemp,icQueryCriteria).
  setAttribute(ihQuery,"recordcount",STRING(iMaxCount)).
END.
ELSE DO:

  IF iStartRow = 0 AND ihQuery:IS-OPEN THEN 
    hBuffer:EMPTY-TEMP-TABLE().
  ELSE IF iStartRow > 0 OR iStartRow = -1 THEN 
    iTotCount = INT(getAttribute(ihQuery,"totalcount")).
  
  DYNAMIC-FUNCTION("setQueryStopAfter",DYNAMIC-FUNCTION("getAttribute",ihQuery,"queryStopAfter")).
  DYNAMIC-FUNCTION("setOrgBuffersAndFields",DYNAMIC-FUNCTION("getAttribute",ihQuery,"orgBuffersAndFields")).
  
  cTemp = DYNAMIC-FUNCTION("AddCalcParam",cTemp,ihQuery).
  
  DYNAMIC-FUNCTION("setPreScanQuery",TRIM(REPLACE(DYNAMIC-FUNCTION("getAttribute",ihQuery,"prescanBaseQuery"),"|",CHR(3)) + CHR(28) + 
                                          REPLACE(DYNAMIC-FUNCTION("getAttribute",ihQuery,"prescanqueryfilter"),"|",CHR(3)) + CHR(28) + 
                                          REPLACE(DYNAMIC-FUNCTION("getAttribute",ihQuery,"prescanquerywhere"),"|",CHR(3))
                                          ,CHR(28))).

  IF DYNAMIC-FUNCTION("getAttribute",ihQuery,"prescanlimit") NE "" THEN
    DYNAMIC-FUNCTION("setPreScanLimit",INT(DYNAMIC-FUNCTION("getAttribute",ihQuery,"prescanlimit"))).
    
   DYNAMIC-FUNCTION("setNoExistBuffers",DYNAMIC-FUNCTION("getAttribute",ihQuery,"noexistbuffers")).

  IF DYNAMIC-FUNCTION("getAttribute",ihQuery,"useAppserver") NE "" THEN DO:
    hUseAppServer = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",SESSION,DYNAMIC-FUNCTION("getAttribute",ihQuery,"useAppserver"))) NO-ERROR.
    IF VALID-HANDLE(hUseAppServer) THEN
      DYNAMIC-FUNCTION("setUseAppserver",hUseAppServer).
  END.

  DYNAMIC-FUNCTION("setUniqueBuffer",DYNAMIC-FUNCTION("getAttribute",ihQuery,"uniqueBuffer")).
                                          
  DYNAMIC-FUNCTION("setCalcFieldProc",getAttribute(ihQuery,"calcfieldproc")).
  DYNAMIC-FUNCTION("setCalcFieldFilter",DYNAMIC-FUNCTION("getAttribute",ihQuery,"calcfieldfilter")
                 + (IF DYNAMIC-FUNCTION("getAttribute",ihQuery,"calcfieldfilter") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihQuery,"calcfieldwhere") NE "" THEN "|" ELSE "")
                 + DYNAMIC-FUNCTION("getAttribute",ihQuery,"calcfieldwhere")
                 ).
  
  httTable = DYNAMIC-FUNCTION("getTempTableJoin",iBatchSize,
                              IF DYNAMIC-FUNCTION("getAttribute",ihQuery,"SmartComponentLibrary_QueryStart") NE "" THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",ihQuery,"SmartComponentLibrary_QueryStart"))
                              ELSE IF iStartRow = -1 THEN 0 
                              ELSE iStartRow,
                              getAttribute(ihQuery,"querydir"),
                              cTemp,
                              icQueryCriteria
                            + DYNAMIC-FUNCTION("getAttribute",ihQuery,"CompanyTag")
                              ).
  
  httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httTableQuery NO-ERROR.
  httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
  httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
  httTableQuery:QUERY-OPEN.
  cJoin = getAttribute(ihQuery,"queryjoin").
  DO ix = 1 TO hBuffer:NUM-FIELDS:
    IF hBuffer:BUFFER-FIELD(ix):NAME BEGINS "RowIdent" THEN DO:
      iNumRowIds = iNumRowIds + 1.
      IF NUM-ENTRIES(cJoin) GE iNumRowIds + 1 AND ENTRY(iNumRowIds + 1,cJoin) MATCHES "*OUTER-JOIN*" THEN
        LEAVE.
    END.
  END.

  cSkipUniqueRows = getAttribute(ihQuery,"SkipUniqueRows").
  DO ix = 1 TO NUM-ENTRIES(cSkipUniqueRows):
    hField = hBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)) NO-ERROR.
    IF NOT VALID-HANDLE(hField) THEN LEAVE.
    ELSE cCheckedRowIds = cCheckedRowIds + ENTRY(ix,cSkipUniqueRows) + ",".
  END.
  cSkipUniqueRows = TRIM(cCheckedRowIds,",").
  
  ASSIGN cExtentFields       = getAttribute(ihQuery,"extentFields")
         cAltExtFieldHandles = getAttribute(ihQuery,"AltExtentFieldHandles")
         cExcludeLocalFields = getAttribute(ihQuery,"localcalcfields").
  DO ix = 1 TO NUM-ENTRIES(cExtentFields):
    ASSIGN hAltExtentFields[ix] = WIDGET-HANDLE(ENTRY(ix,cAltExtFieldHandles))
/*            hExtentFields[ix]    = httTableBuffer:BUFFER-FIELD(ENTRY(ix,cExtentFields)) */
           iCntExtFields        = iCntExtFields + 1.
    hExtentFields[ix]    = httTableBuffer:BUFFER-FIELD(ENTRY(ix,cExtentFields)) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
      cFieldDef = ENTRY(ix,cExtentFields).
      hAltExtentFields[ix] = hBuffer:BUFFER-FIELD(cFieldDef).
      cFieldDef = SUBSTR(cFieldDef,1,R-INDEX(cFieldDef,"_") - 1) + "[" + SUBSTR(cFieldDef,R-INDEX(cFieldDef,"_") + 1) + "]".
      hExtentFields[ix] = httTableBuffer:BUFFER-FIELD(cFieldDef).
    END.
  END.
  DO ix = 1 TO NUM-ENTRIES(cExcludeLocalFields):
    IF ENTRY(ix,cExcludeLocalFields) MATCHES "*]" THEN DO:
      IF NOT CAN-DO(cExcludedFields,SUBSTR(ENTRY(ix,cExcludeLocalFields),1,R-INDEX(ENTRY(ix,cExcludeLocalFields),"[") - 1)) THEN
        cExcludedFields = cExcludedFields + (IF cExcludedFields NE "" THEN "," ELSE "") + SUBSTR(ENTRY(ix,cExcludeLocalFields),1,R-INDEX(ENTRY(ix,cExcludeLocalFields),"[") - 1).
    END.
    ELSE cExcludedFields = cExcludedFields + (IF cExcludedFields NE "" THEN "," ELSE "") + ENTRY(ix,cExcludeLocalFields).
  END.
  
  IF getAttribute(ihQuery,"querydir") = "" THEN
    httTableQuery:GET-FIRST().
  ELSE
    httTableQuery:GET-LAST().
  REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:  
    ASSIGN cFindString   = "WHERE "
           iNumServerRec = iNumServerRec + 1
           .
    /*       
    IF cSkipUniqueRows NE "" THEN
      DO ix = 1 TO NUM-ENTRIES(cSkipUniqueRows):
        cFindString = cFindString + "RowIdent" + ENTRY(ix,cSkipUniqueRows) + " = '" + httTableBuffer:BUFFER-FIELD("RowIdent" + ENTRY(ix,cSkipUniqueRows)):BUFFER-VALUE + "' AND ".
      END.
    ELSE
    */
      DO ix = 1 TO iNumRowIds:
        cFindString = cFindString + "RowIdent" + STRING(ix) + " = '" + httTableBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)):BUFFER-VALUE + "' AND ".
      END.
    cFindString = RIGHT-TRIM(cFindString," AND ").

    bOk = hBuffer:FIND-UNIQUE(cFindString) NO-ERROR.
    IF NOT bOk THEN DO:
      hBuffer:BUFFER-CREATE().
      hBuffer:BUFFER-COPY(httTableBuffer).
      ASSIGN iTotCount   = iTotCount + 1
             iCount      = iCount    + 1
             cFirstRowid = IF cFirstRowid = "" THEN STRING(hBuffer:ROWID) ELSE cFirstRowid
             .
    END.
    ELSE hBuffer:BUFFER-COPY(httTableBuffer,cExcludedFields).
  
    cLastRowid  = STRING(hBuffer:ROWID).

    DO ix = 1 TO iCntExtFields:
      hAltExtentFields[ix]:BUFFER-VALUE = hExtentFields[ix]:BUFFER-VALUE.
    END.

    IF iStartRow NE 0 THEN DO:
      rBuffer = hBuffer:ROWID.
      iStartRow = 0.
    END.
    iMaxCount = IF httTableBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE > iMaxCount THEN httTableBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE ELSE iMaxCount.

    IF getAttribute(ihQuery,"querydir") = "" THEN
      httTableQuery:GET-NEXT().
    ELSE 
      httTableQuery:GET-PREV().
  END.
  setAttribute(ihQuery,"totalcount",STRING(iTotCount)).

  /* If we go from the start or we go from the end and retrieve all records, set firstrowid to cFirstRowid: */ 
  IF (iNumServerRec < INT(getAttribute(ihQuery,"rowstobatch")) AND getAttribute(ihQuery,"querydir") NE "") 
     OR (getAttribute(ihQuery,"querydir") = "" AND iTmpStartRow = 0) THEN
    setAttribute(ihQuery,"firstrowid",cFirstRowid).
  ELSE IF getAttribute(ihQuery,"querydir") NE "" THEN
    setAttribute(ihQuery,"firstrowid","").

  /* If we go from the end or we go from the start and retrieve all records, set lastrowid to cLastRowid: */ 
  IF iNumServerRec < INT(getAttribute(ihQuery,"rowstobatch")) 
      OR getAttribute(ihQuery,"rowstobatch") = "0"
      OR (getAttribute(ihQuery,"querydir") NE "" AND iTmpStartRow = 0) THEN
    setAttribute(ihQuery,"lastrowid",cLastRowid).
  ELSE
    setAttribute(ihQuery,"lastrowid","").

  IF getAttribute(ihQuery,"localsort") NE "" THEN cSortColumn = FixSortString(ihQuery).
  ihQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + 
                        (IF cSortColumn NE "" THEN 
                          " BY " + cSortColumn + " " + getAttribute(ihQuery,"querydesc")
                         ELSE "")) NO-ERROR.
  IF ERROR-STATUS:GET-NUMBER(1) = 7328 THEN
    ihQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).

  hTmpQuery = ihQuery NO-ERROR.

  IF VALID-HANDLE(hTmpQuery) THEN
    bOk = hTmpQuery:QUERY-OPEN() NO-ERROR.
  ELSE DO: 
    IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN DO:
      MESSAGE PROGRAM-NAME(1) SKIP
              "Invalid query handle: " hTmpQuery SKIP
              VIEW-AS ALERT-BOX ERROR.
      iMaxCount = 0.
      RETURN.
    END.
  END.

  IF NOT bOk AND ERROR-STATUS:GET-NUMBER(1) = 141 THEN DO:
    cSortColumn = "SUBSTR(" + getAttribute(ihQuery,"localsort") + ",1,50)".
    ihQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + 
                          " BY " + cSortColumn 
                        + " " + getAttribute(ihQuery,"querydesc")).
    bOK = ihQuery:QUERY:QUERY-OPEN() NO-ERROR.
  END.

  IF rBuffer NE ? THEN DO:
    ihQuery:REPOSITION-TO-ROWID(rBuffer) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      ihQuery:GET-NEXT().
  END.
  ELSE IF getAttribute(ihQuery,"querydir") NE "" THEN DO: 
    ihQuery:GET-LAST().
    IF ihQuery:GET-BUFFER-HANDLE(1):AVAIL THEN
      setAttribute(ihQuery,"lastrowid",STRING(ihQuery:GET-BUFFER-HANDLE(1):ROWID)).
  END.
  ELSE DO:
    ihQuery:GET-FIRST().
    IF ihQuery:GET-BUFFER-HANDLE(1):AVAIL THEN
      setAttribute(ihQuery,"firstrowid",STRING(ihQuery:GET-BUFFER-HANDLE(1):ROWID)).
  END.
  
  DELETE OBJECT httTableQuery.
  DELETE OBJECT httTable.
  
  IF DYNAMIC-FUNCTION("getQueryStatFieldValues") NE "" THEN 
    setAttribute(ihQuery,"recordcount",ENTRY(1,ENTRY(2,DYNAMIC-FUNCTION("getQueryStatFieldValues"),"|"),";")).
  ELSE DO:
    IF (getAttribute(ihQuery,"querydir") = "" AND
        getAttribute(ihQuery,"lastrowid") NE "" AND iStartRow = 0) OR
       (getAttribute(ihQuery,"querydir") NE "" AND
        getAttribute(ihQuery,"firstrowid") NE "" AND iStartRow = 0) OR
        getAttribute(ihQuery,"viewrecordcount") NE ""
      THEN    
      setAttribute(ihQuery,"recordcount",STRING(MAX(iTotCount,ihQuery:NUM-RESULTS))).
    ELSE IF getAttribute(ihQuery,"getrecordcount") = "" THEN setAttribute(ihQuery,"recordcount","").

    IF NOT PROGRAM-NAME(3) BEGINS "StartSearch" AND NOT PROGRAM-NAME(3) BEGINS("MultiSortBrowse") AND 
       NOT PROGRAM-NAME(3) BEGINS "FirstRecord" AND NOT PROGRAM-NAME(3) BEGINS("LastRecord") THEN 
      setAttribute(ihQuery,"currentcount",STRING((IF cSkipUniqueRows NE "" THEN iTotCount ELSE iMaxCount))).
  END.
  
  IF INDEX(DYNAMIC-FUNCTION("getQueryStatFieldValues"),";") > 0 THEN DO:
    cStatValues = DYNAMIC-FUNCTION("getQueryStatFieldValues").
    DO ix = 2 TO NUM-ENTRIES(cStatValues,";"):
      setAttribute(ihQuery,"statvalue" + ENTRY(1,ENTRY(ix,cStatValues,";"),"|"),ENTRY(2,ENTRY(ix,cStatValues,";"),"|")).
    END.
    setAttribute(ihQuery,"QueryStatFieldValues",SUBSTR(DYNAMIC-FUNCTION("getQueryStatFieldValues"),INDEX(DYNAMIC-FUNCTION("getQueryStatFieldValues"),";") + 1)).
  END.
END.

DYNAMIC-FUNCTION("ViewRecordCount",ihQuery).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSetAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSetAttribute Procedure 
PROCEDURE getSetAttribute :
/*------------------------------------------------------------------------------
 Purpose: Get/set attribute from classes - workaround for bug using dynamic-function
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihObject        AS HANDLE NO-UNDO.
DEF INPUT PARAM icAttrName      AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM iocValue AS CHAR NO-UNDO. 

IF iocValue = ? THEN iocValue = getAttribute(ihObject,icAttrName).
ELSE setAttribute(ihObject,icAttrName,iocValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AddBrowseTriggers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddBrowseTriggers Procedure 
FUNCTION AddBrowseTriggers RETURNS LOGICAL PRIVATE
  ( INPUT ihBrowse    AS HANDLE,
    INPUT ihProcedure AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ON START-SEARCH            OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"start-search").
ON OFF-END                 OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"off-end").
ON OFF-HOME                OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"off-home").
ON VALUE-CHANGED           OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"value-changed").
ON mouse-select-up         OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"row-entry").
ON mouse-menu-down         OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"mouse-menu-down").
/*ON ROW-LEAVE               OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"row-leave").*/
ON ROW-DISPLAY             OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"row-display").
ON END-MOVE                OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"view-overlay").
ON SCROLL-NOTIFY           OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
ON CURSOR-RIGHT            OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
ON CURSOR-LEFT             OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
ON CURSOR-UP               OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
ON CURSOR-DOWN             OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
ON shift-CURSOR-UP         OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-cursor-up").
ON shift-CURSOR-DOWN       OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-cursor-down").
ON PAGE-UP                 OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
ON PAGE-DOWN               OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"scroll-notify").
ON shift-PAGE-UP           OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-page-up").
ON shift-PAGE-DOWN         OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-page-down").
ON HOME                    OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"first-record").
ON END                     OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"last-record").
ON shift-HOME              OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-home").
ON shift-END               OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-end").
ON ANY-PRINTABLE           OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"any-printable").
ON DEFAULT-ACTION          OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"default-action").
ON INSERT-MODE             OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"new-record").
ON DELETE-CHARACTER        OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"delete-record").
ON F5                      OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"refresh-record"). 
ON F8                      OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"enter-browse-search"). 
ON TAB                     OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"tab").
ON BACK-TAB                OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"back-tab").
ON DROP-FILE-NOTIFY        OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"drop-file-notify").
ON ctrl-CURSOR-RIGHT       OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"ctrl-cursor-right").
ON ctrl-CURSOR-LEFT        OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"ctrl-cursor-left").
ON shift-ctrl-CURSOR-RIGHT OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-ctrl-cursor-right").
ON shift-ctrl-CURSOR-LEFT  OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"shift-ctrl-cursor-left").
ON alt-CURSOR-DOWN         OF ihBrowse PERSISTENT RUN DoProcessEvent (ihProcedure,"alt-cursor-down").

 
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddCalcParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddCalcParam Procedure 
FUNCTION AddCalcParam RETURNS CHARACTER
  ( INPUT icBuffsAndFlds AS CHAR,
    INPUT ihBrwOrQry     AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Replace parameter string for BuffersAndFields to contain current parameter to calculated fields 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewCalcPhrase AS CHAR NO-UNDO.
DEF VAR cTemp          AS CHAR NO-UNDO.

cTemp = icBuffsAndFlds.

FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihBrwOrQry
      AND ttAttribute.cName BEGINS "calcparam":
  FIND FIRST bttAttribute
       WHERE bttAttribute.hObject = ihBrwOrQry
         AND bttAttribute.cName = "calcphrase" + SUBSTR(ttAttribute.cName,10) NO-ERROR.
  IF AVAIL bttAttribute THEN 
    ASSIGN ix             = INDEX(bttAttribute.cValue,"(ROWID")
           cNewCalcPhrase = SUBSTR(bttAttribute.cValue,1,ix) + "ROWID" + REPLACE(ttAttribute.cValue,",",CHR(1)) + ")"
           cTemp          = REPLACE(cTemp,bttAttribute.cValue,cNewCalcPhrase)
           . 
END.

RETURN cTemp.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddEditBtn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddEditBtn Procedure 
FUNCTION AddEditBtn RETURNS CHARACTER
  ( INPUT icActionList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix       AS INT NO-UNDO.
DEF VAR cNewList AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",SESSION,"appendEditBtn") = "yes" 
   AND ENTRY(1,ENTRY(1,icActionList),";") = "new" 
   AND NUM-ENTRIES(icActionList) > 1  
   THEN DO:
  cNewList = ENTRY(1,icActionList) + ",edit;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Endre" ELSE "Edit") + "ctrl-e". 
  DO ix = 2 TO NUM-ENTRIES(icActionList):
    IF ENTRY(1,ENTRY(ix,icActionList),";") = "edit" THEN DO:
      cNewList = icActionList.
      LEAVE.
    END.
    cNewList = cNewList + "," + ENTRY(ix,icActionList).
  END.
END.
ELSE cNewList = icActionList.

RETURN cNewList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AddEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddEvent Procedure 
FUNCTION AddEvent RETURNS LOGICAL
  ( INPUT ihObject     AS HANDLE,
    INPUT ihWidget     AS HANDLE,
    INPUT icAction     AS CHAR,
    INPUT icName       AS CHAR,
    INPUT icWidgetType AS CHAR,
    INPUT icLabel      AS CHAR,
    INPUT icMethod     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: 
------------------------------------------------------------------------------*/
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ihObject
       ttEvent.hWidget       = ihWidget
       ttEvent.cAction       = icAction
       ttEvent.cName         = icName
       ttEvent.cWidgetType   = icWidgetType
       ttEvent.cLabel        = icLabel
       ttEvent.cMethod       = icMethod.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AppendAttribute Procedure 
FUNCTION AppendAttribute RETURNS LOGICAL
  ( INPUT ihObject      AS HANDLE,
    INPUT icName        AS CHAR,
    INPUT icValue       AS CHAR,
    INPUT icDelimiter   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST bttAttribute
     WHERE bttAttribute.hObject = ihObject 
       AND bttAttribute.cName   = icName
     NO-ERROR.
IF AVAIL bttAttribute THEN DO:
  IF icDelimiter NE "" AND bttAttribute.cValue NE "" THEN
    bttAttribute.cValue = bttAttribute.cValue + icDelimiter.
  bttAttribute.cValue = bttAttribute.cValue + icValue.
END.
ELSE setAttribute(ihObject,icName,icValue).

RETURN YES.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AppendFieldMap Procedure 
FUNCTION AppendFieldMap RETURNS LOGICAL
  ( INPUT ihFieldMap            AS HANDLE,
    INPUT ihFrame               AS HANDLE,
    INPUT icBufferUpdateFields  AS CHAR,
    INPUT icScreenUpdateFields  AS CHAR,
    INPUT icBufferDisplayFields AS CHAR,
    INPUT icScreenDisplayFields AS CHAR,
    INPUT icOtherProp           AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Create object to hold links between query buffer fields and corr. input and displ fields
    Notes: Handle to field map is the buffer handle 
------------------------------------------------------------------------------*/
DEF VAR hWidget             AS HANDLE NO-UNDO.
DEF VAR cInpWidgets         AS CHAR NO-UNDO.
DEF VAR cDispWidgets        AS CHAR NO-UNDO.
DEF VAR cExtraUpdateFields  AS CHAR NO-UNDO.
DEF VAR cExtraUpdateWidgets AS CHAR NO-UNDO.
DEF VAR cAllUpdateFields    AS CHAR NO-UNDO.
DEF VAR cTimeInputFlds      AS CHAR NO-UNDO.
DEF VAR cPrimaryKeyFields   AS CHAR NO-UNDO.
DEF VAR cPrimaryKeyWidgets  AS CHAR NO-UNDO.

hObjectSourceProc = (IF hObjectSourceProc = ? THEN SOURCE-PROCEDURE ELSE hObjectSourceProc).

IF VALID-HANDLE(ihFieldMap) THEN DO:
  FIND FIRST ttObject WHERE ttObject.hObject = ihFieldMap NO-ERROR.
  IF NOT AVAIL ttObject THEN DO:
    MESSAGE "The FieldMap object doesn't exist" SKIP(1)
            PROGRAM-NAME(1) SKIP
            PROGRAM-NAME(2)
            VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".
    RETURN FALSE.
  END.
END.
ELSE DO:
  ihFieldMap = ihFrame.
  FIND FIRST ttObject WHERE ttObject.hObject = ihFieldMap NO-ERROR.
  IF NOT AVAIL ttObject THEN DO:
    CREATE ttObject.
    ASSIGN ttObject.hWindow       = ihFrame:WINDOW
           ttObject.hObject       = ihFieldMap
           ttObject.cObjectType   = "fieldMap"
           ttObject.cObjectName   = ihFieldMap:NAME
           ttObject.hDesignObject = ihFrame
           ttObject.hSourceProc   = hObjectSourceProc
           ttObject.cInitProc     = PROGRAM-NAME(2)
           ttObject.cGenProc      = PROGRAM-NAME(1)
           .
  
    CREATE ttEvent.
    ASSIGN ttEvent.hObject       = ttObject.hObject
           ttEvent.hWidget       = ttObject.hObject
           ttEvent.cAction       = "new-record"
           ttEvent.cName         = "new-record"
           ttEvent.cWidgetType   = "object"
           ttEvent.cMethod       = "NewRecord".
    CREATE ttEvent.
    ASSIGN ttEvent.hObject       = ttObject.hObject
           ttEvent.hWidget       = ttObject.hObject
           ttEvent.cAction       = "save-record"
           ttEvent.cName         = "save-record"
           ttEvent.cWidgetType   = "object"
           ttEvent.cMethod       = "SaveRecord".
  END. 
END.    

ASSIGN cExtraUpdateFields    = ENTRY(1,icOtherProp,";")
       icScreenUpdateFields  = IF icScreenUpdateFields NE "" THEN icScreenUpdateFields ELSE icBufferUpdateFields
       icScreenDisplayFields = IF icScreenDisplayFields NE "" THEN icScreenDisplayFields ELSE icBufferDisplayFields
       cTimeInputFlds        = getAttribute(ihFieldMap,"TimeInputFields")
       .
IF NUM-ENTRIES(icOtherProp,";") > 1 THEN DO:
  cPrimaryKeyFields = ENTRY(2,icOtherProp,";").
  setAttribute(ihFieldMap,"PrimaryKeyFields",cPrimaryKeyFields).
END.         

setAttribute(ihFieldMap,"BufferUpdateFields" + hObjectSourceProc:FILE-NAME,icBufferUpdateFields).
setAttribute(ihFieldMap,"ScreenUpdateFields" + hObjectSourceProc:FILE-NAME,icScreenUpdateFields).
setAttribute(ihFieldMap,"BufferDisplayFields" + hObjectSourceProc:FILE-NAME,icBufferDisplayFields).
setAttribute(ihFieldMap,"ScreenDisplayFields" + hObjectSourceProc:FILE-NAME,icScreenDisplayFields).
setAttribute(ihFieldMap,"ExtraUpdateFields" + hObjectSourceProc:FILE-NAME,cExtraUpdateFields).

setAttribute(ihFieldMap,"BufferUpdateFields",TRIM(getAttribute(ihFieldMap,"BufferUpdateFields") + "," + icBufferUpdateFields,",")).
setAttribute(ihFieldMap,"ScreenUpdateFields",TRIM(getAttribute(ihFieldMap,"ScreenUpdateFields") + "," + icScreenUpdateFields,",")).
setAttribute(ihFieldMap,"BufferDisplayFields",TRIM(getAttribute(ihFieldMap,"BufferDisplayFields") + "," + icBufferDisplayFields,",")).
setAttribute(ihFieldMap,"ScreenDisplayFields",TRIM(getAttribute(ihFieldMap,"ScreenDisplayFields") + "," + icScreenDisplayFields,",")).
setAttribute(ihFieldMap,"ExtraUpdateFields",TRIM(getAttribute(ihFieldMap,"ExtraUpdateFields") + "," + cExtraUpdateFields,",")).
setAttribute(ihFieldMap,"checkmodified","no").

cAllUpdateFields = TRIM(icScreenUpdateFields + "," + cExtraUpdateFields,",").

EMPTY TEMP-TABLE ttSort1.
EMPTY TEMP-TABLE ttSort2.
DO ix = 1 TO NUM-ENTRIES(icScreenUpdateFields):
  CREATE ttSort1.
  ASSIGN ttSort1.iSeq   = ix
         ttSort1.cText1 = ENTRY(ix,icScreenUpdateFields).
END.
DO ix = 1 TO NUM-ENTRIES(icScreenDisplayFields):
  CREATE ttSort2.
  ASSIGN ttSort2.iSeq   = ix
         ttSort2.cText1 = ENTRY(ix,icScreenDisplayFields).
END.

hWidget = ihFrame:FIRST-CHILD:FIRST-CHILD.
REPEAT WHILE hWidget NE ?:
  IF CAN-FIND(FIRST ttObject WHERE ttObject.hObject = hWidget AND ttObject.cObjectType = "browse-search-field") THEN DO:
    IF CAN-QUERY(hWidget,"next-sibling") THEN 
      hWidget = hWidget:NEXT-SIBLING.
    ELSE hWidget = ?.
    NEXT.
  END.
  
  IF CAN-DO(cAllUpdateFields,hWidget:NAME) THEN DO:

    IF CAN-DO(icScreenUpdateFields,hWidget:NAME) THEN DO:
      FIND FIRST ttSort1 WHERE ttSort1.cText1 = hWidget:NAME.
      ttSort1.cText2 = STRING(hWidget).
    END.

    IF hWidget:TYPE = "fill-in" OR (CAN-QUERY(hWidget,"SUBTYPE") AND hWidget:SUBTYPE = "drop-down") THEN DO:
      ON ANY-PRINTABLE    OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
      ON LEAVE            OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave-widget").
      IF hWidget:TYPE = "fill-in" THEN DO:
        IF hWidget:DATA-TYPE BEGINS "DATE" THEN DO:
          ON BACKSPACE OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"clear-date-time").
          ON DELETE-CHARACTER OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"clear-date-time").
          ON " " OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"clear-date-time").
          ON "t" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "d" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "+" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "-" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          CREATE ttEvent.
          ASSIGN ttEvent.hObject     = ttObject.hObject
                 ttEvent.hWidget     = hWidget
                 ttEvent.cAction     = "todays-date-key"
                 ttEvent.cName       = "todays-date-key"
                 ttEvent.cWidgetType = "fill-in"
                 ttEvent.cMethod     = "TodaysDateKey".
        END.
        ELSE DO:
          ON BACKSPACE        OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
          ON DELETE-CHARACTER OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        END.  
        
        /*
        ON BACKSPACE        OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        ON DELETE-CHARACTER OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        ON ctrl-v           OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        IF hWidget:DATA-TYPE = "date" THEN DO:
          ON "t" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "d" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "+" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "-" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          CREATE ttEvent.
          ASSIGN ttEvent.hObject       = ttObject.hObject
                 ttEvent.hWidget       = hWidget
                 ttEvent.cAction       = "todays-date-key"
                 ttEvent.cName         = "todays-date-key"
                 ttEvent.cWidgetType   = "fill-in"
                 ttEvent.cMethod       = "TodaysDateKey".
        END.
        */
      END.
      ELSE DO:
        ON VALUE-CHANGED OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        hWidget:BGCOLOR = 15.
      END.
      
      IF hWidget:DATA-TYPE BEGINS "date" THEN DO:
        CREATE ttEvent.
        ASSIGN ttEvent.hObject       = ttObject.hObject
               ttEvent.hWidget       = hWidget
               ttEvent.cAction       = "clear-date-time"
               ttEvent.cName         = "clear-date-time"
               ttEvent.cWidgetType   = "fill-in"
               ttEvent.cMethod       = "ClearDateTime".
      END.
    
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "any-printable"
             ttEvent.cName         = "any-printable"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "AnyPrintableKey".
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "leave-widget"
             ttEvent.cName         = "leave-widget"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "LeaveWidget".
    
      IF hWidget:FORMAT = "99:99" OR hWidget:FORMAT = "99:99:99" THEN
        cTimeInputFlds = cTimeInputFlds + (IF cTimeInputFlds NE "" THEN "," ELSE "") + hWidget:NAME.
    
      IF CAN-DO(cExtraUpdateFields,"btn" + hWidget:NAME) THEN DO:
        ON F3 OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"f3-widget").
        CREATE ttEvent.
        ASSIGN ttEvent.hObject       = ttObject.hObject
               ttEvent.hWidget       = hWidget
               ttEvent.cAction       = "f3-widget"
               ttEvent.cName         = "f3-widget"
               ttEvent.cWidgetType   = "fill-in"
               ttEvent.cMethod       = "F3Widget".
      END.
    END.
    ELSE DO:
      ON VALUE-CHANGED OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"value-changed-widget").
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "value-changed-widget"
             ttEvent.cName         = "value-changed-widget"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "ValueChangedWidget".
      IF hWidget:TYPE = "combo-box" THEN
        hWidget:BGCOLOR = 15.
    END.
    
    IF bTabOnReturn AND hWidget:TYPE NE "editor" THEN DO:
      ON RETURN OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"return-of-widget").
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "return-of-widget"
             ttEvent.cName         = "return-of-widget"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "ReturnOfWidget".
    END.
    
    IF getAttribute(hWidget:WINDOW,"HelpTextWidget") NE "" THEN DO:
      ON ENTRY OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"entry-of-widget").
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "entry-of-widget"
             ttEvent.cName         = "entry-of-widget"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "EntryOfWidget".
    END.

  END.
  ELSE IF hWidget:NAME BEGINS "btn"
       AND LENGTH(hWidget:NAME) > 3
       AND CAN-DO(icScreenUpdateFields,SUBSTR(hWidget:NAME,4)) THEN
    cExtraUpdateFields = cExtraUpdateFields + (IF cExtraUpdateFields NE "" THEN "," ELSE "") + hWidget:NAME.

  IF CAN-DO(icScreenDisplayFields,hWidget:NAME) THEN DO:
    IF CAN-DO("editor,fill-in",hWidget:TYPE) THEN
      ASSIGN hWidget:SENSITIVE = TRUE
             hWidget:READ-ONLY = TRUE.
    ELSE hWidget:SENSITIVE = FALSE.

    hWidget:TAB-STOP  = FALSE.
    FIND FIRST ttSort2 WHERE ttSort2.cText1 = hWidget:NAME.
    ttSort2.cText2 = STRING(hWidget).
  END.
  ELSE IF CAN-DO(cExtraUpdateFields,hWidget:NAME) THEN 
    cExtraUpdateWidgets = cExtraUpdateWidgets + STRING(hWidget) + ",".
  IF CAN-DO(cPrimaryKeyFields,hWidget:NAME) THEN
    cPrimaryKeyWidgets = cPrimaryKeyWidgets + STRING(hWidget) + ",".  
  IF CAN-QUERY(hWidget,"next-sibling") THEN 
    hWidget = hWidget:NEXT-SIBLING.
  ELSE hWidget = ?.
END.

FOR EACH ttSort1:
  cInpWidgets = cInpWidgets + ttSort1.cText2 + ",".
END.
FOR EACH ttSort2:
  cDispWidgets = cDispWidgets + ttSort2.cText2 + ",".
END.

ASSIGN cInpWidgets = TRIM(cInpWidgets,",")
       cDispWidgets = TRIM(cDispWidgets,",").

setAttribute(ihFieldMap,"ScreenUpdateWidgets" + hObjectSourceProc:FILE-NAME,cInpWidgets).
setAttribute(ihFieldMap,"ScreenDisplayWidgets" + hObjectSourceProc:FILE-NAME,cDispWidgets).
IF cExtraUpdateWidgets NE "" THEN DO:
  setAttribute(ihFieldMap,"ExtraUpdateFields" + hObjectSourceProc:FILE-NAME,TRIM(cExtraUpdateFields,",")).
  setAttribute(ihFieldMap,"ExtraUpdateWidgets" + hObjectSourceProc:FILE-NAME,TRIM(cExtraUpdateWidgets,",")).
END.
IF cTimeInputFlds NE "" THEN
  setAttribute(ihFieldMap,"TimeInputFields",cTimeInputFlds).


setAttribute(ihFieldMap,"ScreenUpdateWidgets",TRIM(getAttribute(ihFieldMap,"ScreenUpdateWidgets") + "," + cInpWidgets,",")).
setAttribute(ihFieldMap,"ScreenDisplayWidgets",TRIM(getAttribute(ihFieldMap,"ScreenDisplayWidgets") + "," + cDispWidgets,",")).
IF cExtraUpdateWidgets NE "" THEN
  setAttribute(ihFieldMap,"ExtraUpdateWidgets",TRIM(getAttribute(ihFieldMap,"ExtraUpdateWidgets") + "," + cExtraUpdateWidgets,",")).
IF cPrimaryKeyWidgets NE "" THEN
  setAttribute(ihFieldMap,"PrimaryKeyWidgets",TRIM(getAttribute(ihFieldMap,"PrimaryKeyWidgets") + "," + cPrimaryKeyWidgets,",")).

IF NUM-ENTRIES(cInpWidgets) NE NUM-ENTRIES(icBufferUpdateFields) THEN 
  MESSAGE "Number of update fields in frame doesn't match number of update fields in buffer" SKIP
          "(Could also be the spelling of first screen field)" SKIP(1)
          "Input widgets: " SKIP cInpWidgets SKIP(1)
          "Buffer update widgets: " SKIP icBufferUpdateFields SKIP
          PROGRAM-NAME(1) SKIP
          PROGRAM-NAME(2)
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".
ELSE IF NUM-ENTRIES(cDispWidgets) NE NUM-ENTRIES(icBufferDisplayFields) THEN
  MESSAGE "Number of display fields in frame doesn't match number of display fields in buffer" SKIP
          "(Could also be the spelling of first screen field)" SKIP(1)
          "Display widgets: " SKIP cDispWidgets SKIP(1)
          "Buffer display widgets: " SKIP icBufferDisplayFields SKIP(1)
          PROGRAM-NAME(1) SKIP
          PROGRAM-NAME(2)
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".

hObjectSourceProc = ?.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-appendList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendList Procedure 
FUNCTION appendList RETURNS CHARACTER
  (INPUT icList AS CHAR,INPUT icItem AS CHAR,INPUT icDelim AS CHAR  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cDelim   AS CHAR NO-UNDO INIT ",".
DEF VAR cNewList AS CHAR NO-UNDO.

IF icDelim NE "" THEN cDelim = icDelim.

IF icItem NE "" AND LOOKUP(icItem,icList,cDelim) = 0 THEN
  cNewList = icList + (IF icList NE "" THEN cDelim ELSE "") + icItem.
ELSE
  cNewList = icList.

RETURN cNewList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendToolbar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AppendToolbar Procedure 
FUNCTION AppendToolbar RETURNS LOGICAL
      ( INPUT ihToolbar     AS HANDLE,
        INPUT ihRectangle   AS HANDLE,
        INPUT icMenu        AS CHAR,
        INPUT icActionList  AS CHAR,
        INPUT icOtherProp   AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc = SOURCE-PROCEDURE.
IF NewToolbar (ihRectangle,icMenu,icActionList,"append_toolbar," + STRING(ihToolbar) + "," + icOtherProp) = ? THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Error when appending toolbar"
          VIEW-AS ALERT-BOX.
  hObjectSourceProc = ?.
  RETURN NO.
END.
hObjectSourceProc = ?.
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChangeDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ChangeDynFilter Procedure 
FUNCTION ChangeDynFilter RETURNS LOGICAL
  ( INPUT ihQueryObject   AS HANDLE,
    INPUT icFieldList     AS CHAR,
    INPUT icOperatorList  AS CHAR,
    INPUT icValueList     AS CHAR,
    INPUT icAction        AS CHAR ) :

/*------------------------------------------------------------------------------
  Purpose: Modify current dynamic filter settings  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cCurrFields     AS CHAR   NO-UNDO.
DEF VAR cCurrValues     AS CHAR   NO-UNDO.
DEF VAR cCurrOperators  AS CHAR   NO-UNDO.
DEF VAR cCurrRealFields AS CHAR   NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR bDynamicsFilter AS LOG    NO-UNDO.
DEF VAR cRealFieldName  AS CHAR   NO-UNDO.

DEF BUFFER bttAttribute FOR ttAttribute.

hToolbar = DYNAMIC-FUNCTION("getLinkedObject",ihQueryObject,"toolbar","from").
IF getAttribute(hToolbar,"filterwindow") = "JBoxDynamicsFilter.w" OR
   (getAttribute(hToolbar,"filterwindow") = "" AND getAttribute(SESSION,"filterwindow") = "JBoxDynamicsFilter.w") THEN
  bDynamicsFilter = YES.

FOR EACH bttAttribute NO-LOCK
    WHERE bttAttribute.hObject = ihQueryObject
      AND bttAttribute.cName BEGINS "OperatorInUse_"
      AND bttAttribute.cValue NE "":

  IF bDynamicsFilter AND SUBSTR(bttAttribute.cName,16,1) = "_" THEN 
    cRealFieldName = SUBSTR(bttAttribute.cName,17).
  ELSE
    cRealFieldName = SUBSTR(bttAttribute.cName,15).

  IF NOT CAN-DO(icFieldList,cRealFieldName) THEN
    ASSIGN cCurrFields = cCurrFields + (IF cCurrFields NE "" THEN "," ELSE "") + SUBSTR(bttAttribute.cName,15)
           cCurrRealFields = cCurrRealFields + (IF cCurrRealFields NE "" THEN "," ELSE "") + cRealFieldName.
  ELSE
    bttAttribute.cValue = "".
END.

DO ix = 1 TO NUM-ENTRIES(cCurrFields):
  ASSIGN cCurrOperators = cCurrOperators + (IF cCurrOperators NE "" THEN "," ELSE "") 
                        + getAttribute(ihQueryObject,"operatorInUse_" + ENTRY(ix,cCurrFields))
         cCurrValues    = cCurrValues + (IF cCurrValues NE "" THEN "|" ELSE "")
                        + getAttribute(ihQueryObject,"filterValue_" + ENTRY(ix,cCurrFields))
                        .
END.

DO ix = 1 TO NUM-ENTRIES(icFieldList):
  IF ENTRY(ix,icValueList,"|") NE "" THEN
    ASSIGN cCurrRealFields = cCurrRealFields + (IF cCurrRealFields NE "" THEN "," ELSE "") + ENTRY(ix,icFieldList)
           cCurrOperators  = cCurrOperators + (IF cCurrOperators NE "" THEN "," ELSE "") + ENTRY(ix,icOperatorList)
           cCurrValues     = cCurrValues + (IF cCurrValues NE "" THEN "|" ELSE "") + ENTRY(ix,icValueList,"|")
           .
END.


RETURN InitDynfilter(ihQueryObject,cCurrRealFields,cCurrOperators,cCurrValues,icAction).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearComboBox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearComboBox Procedure 
FUNCTION ClearComboBox RETURNS LOGICAL
  ( INPUT ihCombo AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDummy     AS CHAR NO-UNDO.
DEF VAR iDummy     AS INT  NO-UNDO.
DEF VAR fDummy     AS DEC  NO-UNDO.
DEF VAR dDummy     AS DATE NO-UNDO.
DEF VAR cDelim     AS CHAR NO-UNDO.
DEF VAR iEntries   AS INT  NO-UNDO.
DEF VAR bListItems AS LOG  NO-UNDO.

IF ihCombo:DATA-TYPE = "character" AND ihCombo:SUBTYPE = "drop-down" THEN DO:
  ihCombo:SCREEN-VALUE = ?.
  RETURN YES.  
END.

ASSIGN cDummy   = STRING(TODAY) + "_abc_" + STRING(TIME)
       cDelim   = ihCombo:DELIMITER
       .

IF ihCombo:DATA-TYPE = "character" OR ihCombo:SUBTYPE = "simple" THEN DO:
  IF ihCombo:SUBTYPE = "simple" THEN
    ihCombo:ADD-FIRST(cDummy).
  ELSE DO:
    ihCombo:ADD-FIRST("",cDummy) NO-ERROR.
    IF ERROR-STATUS:GET-NUMBER(1) = 8590 THEN DO:
      ihCombo:DELETE(1).  
      ihCombo:ADD-FIRST("") NO-ERROR.
      ihCombo:SCREEN-VALUE = "".
      ihCombo:DELETE(1).
    END.
    ELSE IF ihCombo:ENTRY(1) = "" AND PROVERSION GT "11" THEN
      ihCombo:SCREEN-VALUE = " " NO-ERROR.
    ELSE DO:
      ihCombo:SCREEN-VALUE = cDummy NO-ERROR.
      ihCombo:DELETE(cDummy) NO-ERROR.
    END.           
  END.  
END.
ELSE DO:
  cDummy = ihCombo:LIST-ITEM-PAIRS NO-ERROR.
  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN bListItems = YES.

  IF bListItems THEN
    iEntries = NUM-ENTRIES(ihCombo:LIST-ITEMS,cDelim).
  ELSE
    iEntries = NUM-ENTRIES(ihCombo:LIST-ITEM-PAIRS,cDelim) / 2.
  CASE ihCombo:DATA-TYPE:
    WHEN "integer" THEN DO:
      iDummy = INTEGER(ihCombo:ENTRY(iEntries)) + 1.
      IF bListItems THEN 
        ihCombo:ADD-FIRST(STRING(iDummy)) NO-ERROR.
      ELSE
        ihCombo:ADD-FIRST("",iDummy) NO-ERROR.
      ihCombo:SCREEN-VALUE = STRING(iDummy) NO-ERROR.
      ihCombo:DELETE(STRING(iDummy)) NO-ERROR.
    END.
    WHEN "decimal" THEN DO:
      fDummy = DECIMAL(ihCombo:ENTRY(iEntries)) + 1.
      IF bListItems THEN
        ihCombo:ADD-FIRST(STRING(fDummy)) NO-ERROR.
      ELSE
        ihCombo:ADD-FIRST("",fDummy) NO-ERROR.
      ihCombo:SCREEN-VALUE = STRING(fDummy) NO-ERROR.
      ihCombo:DELETE(STRING(fDummy)) NO-ERROR.
    END.
    WHEN "date" THEN DO:
      dDummy = DATE(ihCombo:ENTRY(iEntries)) + 1.
      IF bListItems THEN
        ihCombo:ADD-FIRST(STRING(dDummy)) NO-ERROR.
      ELSE
        ihCombo:ADD-FIRST("",dDummy) NO-ERROR.
      ihCombo:SCREEN-VALUE = STRING(dDummy) NO-ERROR.
      ihCombo:DELETE(STRING(dDummy)) NO-ERROR.
    END.
  END CASE.
END.

/*
IF ihCombo:LOOKUP("") = 0 THEN DO:
  IF ihCombo:SUBTYPE = "simple" THEN
    ihCombo:ADD-FIRST("").
  ELSE
    ihCombo:ADD-FIRST("","") NO-ERROR.
  ihCombo:SCREEN-VALUE = " " NO-ERROR.
  ihCombo:DELETE("") NO-ERROR.
  NEXT.
END.
ELSE ihCombo:SCREEN-VALUE = " " NO-ERROR.
*/  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearQueryFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearQueryFilter Procedure 
FUNCTION ClearQueryFilter RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Clear all filter settings for the query object (query or browse) 
    Notes: Typically used if the user is allowed to search outside the current filter setting 
------------------------------------------------------------------------------*/
DEF VAR hFilter        AS HANDLE NO-UNDO.
DEF VAR hFilterButton  AS HANDLE NO-UNDO.

FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihQueryObject:
  IF ttAttribute.cName BEGINS "OperatorInUse_"
    OR ttAttribute.cName BEGINS "FilterValue_" 
    OR ttAttribute.cName BEGINS "GroupOperator_" 
    OR ttAttribute.cName BEGINS "FieldGroup_" 
    OR ttAttribute.cName BEGINS "FieldOperator_" 
    OR ttAttribute.cName BEGINS "HiddenGroupOperator_" 
    THEN
   DELETE ttAttribute.
 ELSE IF CAN-DO("calcfieldfilter,queryfilter,prescanqueryfilter",ttAttribute.cName) THEN
   ttAttribute.cValue = "".
END.

FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ihQueryObject
       AND ttObjectLink.cLinkType   = "toolbar"
     NO-ERROR.
IF AVAIL ttObjectLink THEN DO:
  hFilterButton = DYNAMIC-FUNCTION("getEventWidget",ttObjectLink.hToObject,"filter","").
  &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
    IF VALID-HANDLE(hFilterButton) AND cPassiveFilterButton NE "" THEN
      hFilterButton:LOAD-IMAGE(cPassiveFilterButton) NO-ERROR.
  &ENDIF    
END.

hFilter = WIDGET-HANDLE(getAttribute(ihQueryObject,"filterhandle")) NO-ERROR.
IF VALID-HANDLE(hFilter) AND CAN-DO(hFilter:INTERNAL-ENTRIES,"ClearFilterRecord") THEN
  RUN ClearFilterRecord IN hFilter.

ResetBufferSequence(ihQueryObject).

RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ComposeSortString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ComposeSortString Procedure 
FUNCTION ComposeSortString RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE,
    INPUT ibReplace     AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Interpret current attribute settings for sorting and set the "querysort" attribute 
    Notes: Return value indicates if this is a DESC sort (to cover old dirt) 
------------------------------------------------------------------------------*/
DEF VAR bDesc          AS LOG  NO-UNDO.
DEF VAR cCurrSort      AS CHAR NO-UNDO.
DEF VAR cCurrLocalSort AS CHAR NO-UNDO.
DEF VAR cSortBuffer    AS CHAR NO-UNDO.
DEF VAR cDesc          AS CHAR NO-UNDO.

ASSIGN cCurrSort      = getAttribute(ihQueryObject,"querysort")
       cCurrLocalSort = getAttribute(ihQueryObject,"localsort")
       .

IF NOT ibReplace AND getAttribute(ihQueryObject,"querysort") NE "" THEN DO:
  setAttribute(ihQueryObject,"1stDbSortColumn",ENTRY(1,cCurrSort," ")).
  setAttribute(ihQueryObject,"1stSortColumn",
               IF NUM-ENTRIES(ENTRY(1,cCurrLocalSort," "),".") > 2 THEN
                 ENTRY(3,ENTRY(1,cCurrSort," "),".")
               ELSE IF NUM-ENTRIES(ENTRY(1,cCurrLocalSort," "),".") > 1 THEN
                 ENTRY(2,ENTRY(1,cCurrLocalSort," "),".")
               ELSE ENTRY(1,cCurrLocalSort," ")).
  IF NUM-ENTRIES(cCurrSort," ") > 1 THEN
    cDesc = ENTRY(2,cCurrSort," ").
  ELSE 
    cDesc = getAttribute(ihQueryObject,"querydesc").
  setAttribute(ihQueryObject,"1stSortColumnDesc",cDesc).
  setAttribute(ihQueryObject,"sortbuffer",getAttribute(ihQueryObject,"fieldbuffer" + getAttribute(ihQueryObject,"1stSortColumn"))).
  RETURN getAttribute(ihQueryObject,"querydesc") = "desc".
END.

setAttribute(ihQueryObject,"querysort",
               TRIM(getAttribute(ihQueryObject,"1stDbSortColumn") + " " +
               getAttribute(ihQueryObject,"1stSortColumnDesc") +
               (IF getAttribute(ihQueryObject,"2ndSortColumn") NE "" THEN
                  (IF getAttribute(ihQueryObject,"1stSortColumn") NE "" THEN " BY " ELSE "") +
                  getAttribute(ihQueryObject,"2ndDbSortColumn") + " " +
                  getAttribute(ihQueryObject,"2ndSortColumnDesc")
                ELSE "") +
               (IF getAttribute(ihQueryObject,"3rdSortColumn") NE "" THEN
                  (IF getAttribute(ihQueryObject,"1stSortColumn") NE "" OR getAttribute(ihQueryObject,"2ndSortColumn") NE "" THEN " BY " ELSE "") +
                  getAttribute(ihQueryObject,"3rdDbSortColumn") + " " +
                  getAttribute(ihQueryObject,"3rdSortColumnDesc")
                ELSE "") +
               (IF getAttribute(ihQueryObject,"4thSortColumn") NE "" THEN
                  (IF getAttribute(ihQueryObject,"1stSortColumn") NE "" OR getAttribute(ihQueryObject,"2ndSortColumn") NE "" OR getAttribute(ihQueryObject,"3rdSortColumn") NE "" THEN " BY " ELSE "") +
                  getAttribute(ihQueryObject,"4thDbSortColumn") + " " +
                  getAttribute(ihQueryObject,"4thSortColumnDesc")
                ELSE "")
                )).

setAttribute(ihQueryObject,"localsort",
               TRIM(getAttribute(ihQueryObject,"1stSortColumn") + " " +
               getAttribute(ihQueryObject,"1stSortColumnDesc") +
               (IF getAttribute(ihQueryObject,"2ndSortColumn") NE "" THEN
                  (IF getAttribute(ihQueryObject,"1stSortColumn") NE "" THEN " BY " ELSE "") +
                  getAttribute(ihQueryObject,"2ndSortColumn") + " " +
                  getAttribute(ihQueryObject,"2ndSortColumnDesc")
                ELSE "") +
               (IF getAttribute(ihQueryObject,"3rdSortColumn") NE "" THEN
                  (IF getAttribute(ihQueryObject,"1stSortColumn") NE "" OR getAttribute(ihQueryObject,"2ndSortColumn") NE "" THEN " BY " ELSE "") +
                  getAttribute(ihQueryObject,"3rdSortColumn") + " " +
                  getAttribute(ihQueryObject,"3rdSortColumnDesc")
                ELSE "") +
               (IF getAttribute(ihQueryObject,"4thSortColumn") NE "" THEN
                  (IF getAttribute(ihQueryObject,"1stSortColumn") NE "" OR getAttribute(ihQueryObject,"2ndSortColumn") NE "" OR getAttribute(ihQueryObject,"3rdSortColumn") NE "" THEN " BY " ELSE "") +
                  getAttribute(ihQueryObject,"4thSortColumn") + " " +
                  getAttribute(ihQueryObject,"4thSortColumnDesc")
                ELSE "")
                )).

IF getAttribute(ihQueryObject,"querysort") NE "" THEN DO:
  
  /* If number of search columns are only one */
  IF NUM-ENTRIES(getAttribute(ihQueryObject,"querysort")," ") = 2 AND ENTRY(2,getAttribute(ihQueryObject,"querysort")," ") = "desc" THEN DO:
    bDesc = YES.
    setAttribute(ihQueryObject,"querydesc",TRIM(ENTRY(2,getAttribute(ihQueryObject,"querysort")," "))).
    setAttribute(ihQueryObject,"querysort",TRIM(ENTRY(1,getAttribute(ihQueryObject,"querysort")," "))).
    setAttribute(ihQueryObject,"localsort",TRIM(ENTRY(1,getAttribute(ihQueryObject,"localsort")," "))).
  END.
  ELSE setAttribute(ihQueryObject,"querydesc","").

  setAttribute(ihQueryObject,"sortbuffer",getAttribute(ihQueryObject,"fieldbuffer" + getAttribute(ihQueryObject,"1stSortColumn"))).

  RETURN bDesc.
END.
ELSE DO:
  setAttribute(ihQueryObject,"querysort",cCurrSort).

  IF cCurrSort = "" AND getAttribute(ihQueryObject,"trylocalsort") = "yes" THEN
    setAttribute(ihQueryObject,"localsort",cCurrLocalSort).
  RETURN getAttribute(ihQueryObject,"querydesc") = "desc".
END.  

setAttribute(ihQueryObject,"manualsort","").

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CopyAttributes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CopyAttributes Procedure 
FUNCTION CopyAttributes RETURNS LOGICAL
  ( INPUT ihFrom AS HANDLE,
    INPUT ihTo   AS HANDLE,
    INPUT icMask AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH bttAttribute
    WHERE bttAttribute.hObject = ihFrom
      AND (IF INDEX(icMask,"*") > 0 THEN 
             bttAttribute.cName MATCHES icMask
           ELSE
             bttAttribute.cName BEGINS icMask):
  setAttribute(ihTo,bttAttribute.cName,bttAttribute.cValue).
END.

RETURN YES.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateDotNetDisplayLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateDotNetDisplayLink Procedure 
FUNCTION CreateDotNetDisplayLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  "From" is fieldMap. To is the design object (rectangle placeholder) for .net object (i.e richtextbox)
            Linkinfo is name of field
------------------------------------------------------------------------------*/
IF NOT CAN-FIND(FIRST ttObjectLink
                WHERE ttObjectLink.hFromObject = ihFromObject
                  AND ttObjectLink.hToObject   = ihToObject 
                  AND ttObjectLink.cLinktype   = "dotNet") THEN DO TRANSACTION:

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihFromObject
         ttObjectLink.hToObject   = ihToObject  
         ttObjectLink.cLinkType   = "dotNetDisplay"
         ttObjectLink.cLinkInfo   = icLinkInfo
         ttObjectLink.cInitProc   = PROGRAM-NAME(2).
END.
IF NOT CAN-FIND(FIRST ttObjectLink
                WHERE ttObjectLink.hFromObject = ihToObject
                  AND ttObjectLink.hToObject   = ihFromObject
                  AND ttObjectLink.cLinktype   = "fieldMap") THEN DO TRANSACTION:

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihToObject
         ttObjectLink.hToObject   = ihFromObject
         ttObjectLink.cLinkType   = "fieldMap"
         ttObjectLink.cLinkInfo   = icLinkInfo
         ttObjectLink.cInitProc   = PROGRAM-NAME(2).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateObjectLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateObjectLink Procedure 
FUNCTION CreateObjectLink RETURNS LOGICAL
  ( INPUT ihObject1  AS HANDLE,
    INPUT ihObject2  AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  Create a double-chained link between object1 and two.
    Notes:  The linktype is inherited from the object-type you link to
            (It's like naming a bridge to an island)
------------------------------------------------------------------------------*/
DO TRANSACTION:
  FIND ttObject WHERE ttObject.hObject = ihObject1 NO-ERROR.
  FIND bttObject WHERE bttObject.hObject = ihObject2 NO-ERROR.
  IF AVAIL ttObject AND AVAIL bttObject THEN DO: 
    IF NOT CAN-FIND(ttObjectLink WHERE ttObjectLink.hFromObject = ihObject1 
                                   AND ttObjectLink.hToObject   = ihObject2 
                                   AND ttObjectLink.cLinkType   = bttObject.cObjectType) THEN DO:
      CREATE ttObjectLink.
      ASSIGN ttObjectLink.hFromObject = ihObject1
             ttObjectLink.hToObject   = ihObject2  
             ttObjectLink.cLinkType   = bttObject.cObjectType
             ttObjectLink.cInitProc   = PROGRAM-NAME(2). 
    END.
    IF NOT CAN-FIND(ttObjectLink WHERE ttObjectLink.hFromObject = ihObject2 
                                   AND ttObjectLink.hToObject   = ihObject1 
                                   AND ttObjectLink.cLinkType   = ttObject.cObjectType) THEN DO:
      CREATE ttObjectLink.
      ASSIGN ttObjectLink.hFromObject = ihObject2
             ttObjectLink.hToObject   = ihObject1  
             ttObjectLink.cLinkType   = ttObject.cObjectType 
             ttObjectLink.cInitProc   = PROGRAM-NAME(2).
    END.
    IF (getAttribute(SESSION,"copytoolbartobrowse") = "yes" AND getAttribute(SOURCE-PROCEDURE:CURRENT-WINDOW,"copytoolbartobrowse") NE "no") 
       OR getAttribute(SOURCE-PROCEDURE:CURRENT-WINDOW,"copytoolbartobrowse") = "yes" 
       OR (ttObject.cObjectType = "browse" AND getAttribute(ttObject.hObject,"copytoolbartobrowse") = "yes")
       OR (bttObject.cObjectType = "browse" AND getAttribute(bttObject.hObject,"copytoolbartobrowse") = "yes")
       THEN DO:
      IF ttObject.cObjectType = "browse" AND CAN-DO("toolbar,panel",bttObject.cObjectType) AND getAttribute(ttObject.hObject,"copytoolbartobrowse") NE "no" THEN 
        NewMenuBand(ttObject.hObject,"","addfirst;" + STRING(bttObject.hObject)).
      ELSE IF bttObject.cObjectType = "browse" AND CAN-DO("toolbar,panel",ttObject.cObjectType) AND getAttribute(bttObject.hObject,"copytoolbartobrowse") NE "no" THEN 
        NewMenuBand(bttObject.hObject,"","addfirst;" + STRING(ttObject.hObject)).
    END.

    FIND ttObject  WHERE ttObject.hObject  = ihObject1.
    FIND bttObject WHERE bttObject.hObject = ihObject2.

    IF ttObject.cObjectType = "toolbar" THEN
      InitToolbarWidgets(ttObject.hObject,bttObject.hObject).
    ELSE IF bttObject.cObjectType = "toolbar" THEN
      InitToolbarWidgets(bttObject.hObject,ttObject.hObject).

    FIND ttObject  WHERE ttObject.hObject  = ihObject1.
    FIND bttObject WHERE bttObject.hObject = ihObject2.

    IF ttObject.cObjectType = "browse" AND bttObject.cObjectType = "browse-search-field" AND
      ((getAttribute(ttObject.hObject,"localsort") NE "" AND
        NOT CAN-DO(getAttribute(ttObject.hObject,"currviewfields"),ENTRY(1,getAttribute(ttObject.hObject,"localsort")," "))) OR  
       (getAttribute(ttObject.hObject,"1stSortColumn") NE "" AND
        NOT CAN-DO(getAttribute(ttObject.hObject,"currviewfields"),getAttribute(ttObject.hObject,"1stSortColumn")))) 
       THEN
      bttObject.hObject:HIDDEN = YES.
    ELSE IF bttObject.cObjectType = "browse" AND ttObject.cObjectType = "browse-search-field" AND
      ((getAttribute(bttObject.hObject,"localsort") NE "" AND
        NOT CAN-DO(getAttribute(bttObject.hObject,"currviewfields"),ENTRY(1,getAttribute(bttObject.hObject,"localsort")," "))) OR  
       (getAttribute(ttObject.hObject,"1stSortColumn") NE "" AND
        NOT CAN-DO(getAttribute(bttObject.hObject,"currviewfields"),getAttribute(bttObject.hObject,"1stSortColumn")))) 
       THEN
      ttObject.hObject:HIDDEN = YES.

    IF ttObject.cObjectType = "browse" AND bttObject.cObjectType = "fieldmap" THEN
      CopyAttributes(ttObject.hObject,bttObject.hObject,"nodb*accessfields").
    ELSE IF ttObject.cObjectType = "fieldmap" AND bttObject.cObjectType = "browse" THEN
      CopyAttributes(bttObject.hObject,ttObject.hObject,"nodb*accessfields").
    ELSE IF ttObject.cObjectType = "query" AND bttObject.cObjectType = "fieldmap" THEN
      CopyAttributes(ttObject.hObject,bttObject.hObject,"nodb*accessfields").
    ELSE IF ttObject.cObjectType = "fieldmap" AND bttObject.cObjectType = "query" THEN
      CopyAttributes(bttObject.hObject,ttObject.hObject,"nodb*accessfields").

    IF ttObject.cObjectType = "browse" AND bttObject.cObjectType = "toolbar" THEN
      setActionDbSecurity(bttObject.hObject,ttObject.hObject).
    ELSE IF ttObject.cObjectType = "toolbar" AND bttObject.cObjectType = "browse" THEN
      setActionDbSecurity(ttObject.hObject,bttObject.hObject).
    ELSE IF ttObject.cObjectType = "query" AND bttObject.cObjectType = "toolbar" THEN
      setActionDbSecurity(bttObject.hObject,ttObject.hObject).
    ELSE IF ttObject.cObjectType = "toolbar" AND bttObject.cObjectType = "query" THEN
      setActionDbSecurity(ttObject.hObject,bttObject.hObject).

  END.
  ELSE MESSAGE "Invalid sourceobject for linking:" + CHR(10) + 
               (IF NOT AVAIL ttObject AND VALID-HANDLE(ihObject1) THEN ihObject1:NAME 
                ELSE IF NOT AVAIL ttObject AND NOT VALID-HANDLE(ihObject1) THEN "Invalid handle for parameter 1" 
                ELSE IF NOT AVAIL bttObject AND VALID-HANDLE(ihObject2) THEN ihObject2:NAME 
                ELSE "Invalid handle for parameter 2" 
                ) SKIP(1)
                PROGRAM-NAME(1) SKIP
                PROGRAM-NAME(2)
                VIEW-AS ALERT-BOX ERROR TITLE "JukeBox programmers mistake".
END.
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateOneToOneLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateOneToOneLink Procedure 
FUNCTION CreateOneToOneLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Use to link FROM detail record query to navigation browse or query 
            The link will enable data entry in detail window if no record is avail in nav.query (not so with parent link)
            The nav.buffer is automatically refreshed from detail record
    Notes:  Linkinfo: Foreign key(s):
            - Equal names:   <keyfield1>[,keyfield2..]
            - Diff. names:   <child keyfield1>;<parent keyfield1>[,<child keyfield2>;<parent keyfield2>..]
------------------------------------------------------------------------------*/
IF ihFromObject:TYPE NE "query" THEN DO:
  MESSAGE "Source object must be of type QUERY" SKIP
          "The link is intended for keeping a navigation query in sync when a record is modified"
          VIEW-AS ALERT-BOX ERROR.
  RETURN FALSE.
END.
IF NOT CAN-FIND(FIRST ttObjectLink
                WHERE ttObjectLink.hFromObject = ihFromObject
                  AND ttObjectLink.hToObject   = ihToObject 
                  AND ttObjectLink.cLinktype   = "onetoone") THEN DO:

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihFromObject
         ttObjectLink.hToObject   = ihToObject  
         ttObjectLink.cLinkType   = "onetoone"
         ttObjectLink.cLinkInfo   = icLinkInfo
         ttObjectLink.cInitProc   = PROGRAM-NAME(2).
  setAttribute(ihFromObject,"parentlinkinfo",icLinkInfo).

  RETURN TRUE.
END.
ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateOverlayLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateOverlayLink Procedure 
FUNCTION CreateOverlayLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  "From" is browse. Linkinfo is name of browse column
------------------------------------------------------------------------------*/
IF NOT CAN-FIND(FIRST ttObjectLink
                WHERE ttObjectLink.hFromObject = ihFromObject
                  AND ttObjectLink.hToObject   = ihToObject 
                  AND ttObjectLink.cLinktype   = "browseoverlay") THEN DO TRANSACTION:

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihFromObject
         ttObjectLink.hToObject   = ihToObject  
         ttObjectLink.cLinkType   = "browseoverlay"
         ttObjectLink.cLinkInfo   = icLinkInfo
         ttObjectLink.cInitProc   = PROGRAM-NAME(2).
END.
IF NOT CAN-FIND(FIRST ttObjectLink
                WHERE ttObjectLink.hFromObject = ihToObject
                  AND ttObjectLink.hToObject   = ihFromObject
                  AND ttObjectLink.cLinktype   = "browse") THEN DO TRANSACTION:

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihToObject
         ttObjectLink.hToObject   = ihFromObject
         ttObjectLink.cLinkType   = "browse"
         ttObjectLink.cLinkInfo   = icLinkInfo
         ttObjectLink.cInitProc   = PROGRAM-NAME(2).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateParentLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateParentLink Procedure 
FUNCTION CreateParentLink RETURNS LOGICAL
  ( INPUT ihFromObject  AS HANDLE,
    INPUT ihToObject    AS HANDLE,
    INPUT icLinkInfo    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Link FROM child browse or query to parent
    Notes:  Linkinfo: Foreign key(s):
            - Equal names:   <keyfield1>[,keyfield2..]
            - Diff. names:   <child keyfield1>;<parent keyfield1>[,<child keyfield2>;<parent keyfield2>..]
------------------------------------------------------------------------------*/
DEF VAR cChildLinkFields    AS CHAR NO-UNDO.
DEF VAR cParentLinkFields   AS CHAR NO-UNDO.

IF ihFromObject = ? THEN DO:
  MESSAGE "'From object' handle is invalid" SKIP(1)
          PROGRAM-NAME(1) SKIP
          PROGRAM-NAME(2)
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox (or JukeBox programmers) error".
  RETURN FALSE.
END.
IF ihToObject = ? THEN DO:
  MESSAGE "'To object' handle is invalid" SKIP(1)
          PROGRAM-NAME(1) SKIP
          PROGRAM-NAME(2)
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox (or JukeBox programmers) error".
  RETURN FALSE.
END.

IF NOT CAN-FIND(FIRST ttObjectLink
                WHERE ttObjectLink.hFromObject = ihFromObject
                  AND ttObjectLink.hToObject   = ihToObject 
                  AND ttObjectLink.cLinktype   = "parent") THEN DO:

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihFromObject
         ttObjectLink.hToObject   = ihToObject  
         ttObjectLink.cLinkType   = "parent"
         ttObjectLink.cLinkInfo   = icLinkInfo
         ttObjectLink.cInitProc   = PROGRAM-NAME(2).
  setAttribute(ihFromObject,"parentlinkinfo",icLinkInfo).
  DO ix = 1 TO NUM-ENTRIES(icLinkInfo):
    ASSIGN cChildLinkFields  = cChildLinkFields + ENTRY(1,ENTRY(ix,icLinkInfo),";") + ","
           cParentLinkFields = cParentLinkFields + (IF NUM-ENTRIES(ENTRY(ix,icLinkInfo),";") > 1 THEN ENTRY(2,ENTRY(ix,icLinkInfo),";") ELSE ENTRY(ix,icLinkInfo)) + ",".
  END.
  setAttribute(ihFromObject,"childlinkfields",TRIM(cChildLinkFields,",")).
  setAttribute(ihFromObject,"parentlinkfields",TRIM(cParentLinkFields,",")).
END.
ELSE RETURN FALSE.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteLinksFrom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteLinksFrom Procedure 
FUNCTION DeleteLinksFrom RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Delete all links from an object 
    Notes: Useful for tabfolders, f.ex, on tab-changed and you have a commmon toolbar 
------------------------------------------------------------------------------*/
FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ihObject:
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
         AND bttObjectLink.hToObject   = ttObjectLink.hFromObject
       NO-ERROR.
  IF AVAIL bttObjectLink THEN DELETE bttObjectLink.
  DELETE ttObjectLink.
END.

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteObject Procedure 
FUNCTION DeleteObject RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTmpFieldMap    AS HANDLE NO-UNDO.
DEF VAR bDynamicWidget  AS LOG    NO-UNDO.
DEF VAR hFrame          AS HANDLE NO-UNDO EXTENT 100.
DEF VAR iFrameIdx       AS INT    NO-UNDO.
DEF VAR bHideFrames     AS LOG    NO-UNDO.
DEF VAR cThisObjectType AS CHAR   NO-UNDO.
DEF VAR hCurrWindow     AS HANDLE NO-UNDO.
DEF VAR hDeleteObject   AS HANDLE NO-UNDO. /* Used to prevent crash in 9.1d */

FIND FIRST ttObject 
     WHERE ttObject.hObject= ihObject
    NO-ERROR.
IF AVAIL ttObject THEN
  cThisObjectType = ttObject.cObjectType.
ELSE RETURN NO.

IF ttObject.cObjectType = "fieldMap" 
   AND getAttribute(ttObject.hObject,"checkmodified") NE "never" 
   AND getAttribute(ttObject.hObject,"checkmodified") NE "no"
       THEN DO:

  bOk = DYNAMIC-FUNCTION("CheckModified",ttObject.hDesignObject,"check").
  hTmpFieldMap = ttObject.hObject.
  setAttribute(ttObject.hObject,"checkmodified","no").

  IF bOK THEN 
    iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Lagre endringer?" ELSE "Save changes?","","").
  IF iReturn = 6 THEN DO:
    FIND ttObjectLink 
      WHERE ttObjectLink.hFromObject = ttObject.hObject
        AND ttObjectLink.cLinkType = "toolbar"
        NO-ERROR.
    IF AVAIL ttObjectLink THEN DO:
      DYNAMIC-FUNCTION("ApplyEvent",ttObjectLink.hToObject,"save").
      setAttribute(hTmpFieldMap,"checkmodified","no").
    END.
  END.
  ELSE IF iReturn = 2 THEN 
    RETURN FALSE.
END.

FOR EACH ttObject 
    WHERE ttObject.hObject = ihObject:

  FOR EACH ttObjectLink 
      WHERE ttObjectLink.hFromObject = ttObject.hObject:
    DELETE ttObjectLink.
  END.
  FOR EACH ttObjectLink 
      WHERE ttObjectLink.hToObject = ttObject.hObject:
    DELETE ttObjectLink.
  END.
  bHideFrames = getAttribute(ihObject,"hideframewhendeleteobject") = "yes" OR cThisObjectType = "browse-search-field".
  FOR EACH ttAttribute OF ttObject:
    DELETE ttAttribute.
  END.
/*   FOR EACH ttAttribute WHERE ttAttribute.hObject = ttObject.hDesignObject:  */
/*     DELETE ttAttribute.                                                     */
/*   END.                                                                      */
  FOR EACH ttEvent OF ttObject:
    IF VALID-HANDLE(ttEvent.hWidget) THEN DO:
      bDynamicWidget = ttEvent.hWidget:DYNAMIC NO-ERROR.
      IF bDynamicWidget THEN DO:
        IF bHideFrames THEN DO:
          DYNAMIC-FUNCTION("DoLockWindow",ttEvent.hWidget:WINDOW).
          iFrameIdx = iFrameIdx + 1.
          hFrame[iFrameIdx] = ttEvent.hWidget:FRAME NO-ERROR.
          IF VALID-HANDLE(hFrame[iFrameIdx]) AND NOT hFrame[iFrameIdx]:HIDDEN AND cThisObjectType NE "browse-search-field" AND cThisObjectType NE "toggle-box" THEN
            hFrame[iFrameIdx]:HIDDEN = YES.
        END.
        IF VALID-HANDLE(ttEvent.hWidget) THEN DO:
          IF ttEvent.hWidget:TYPE = "browse" THEN DO:
            hDeleteObject = ttEvent.hWidget:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
            IF VALID-HANDLE(hDeleteObject) AND NOT DYNAMIC-FUNCTION("getIsTableCached",hDeleteObject) THEN
              DELETE OBJECT hDeleteObject NO-ERROR.
            IF VALID-HANDLE(ttEvent.hWidget) THEN DO:
              hDeleteObject = ttEvent.hWidget:QUERY NO-ERROR.
              IF VALID-HANDLE(hDeleteObject) THEN
                DELETE OBJECT hDeleteObject NO-ERROR.
            END.
          END.
          ELSE IF ttEvent.hWidget:TYPE = "query" THEN DO:
            hDeleteObject = ttEvent.hWidget:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
            IF VALID-HANDLE(hDeleteObject) AND NOT DYNAMIC-FUNCTION("getIsTableCached",hDeleteObject) THEN
              DELETE OBJECT hDeleteObject NO-ERROR.
          END.

          IF NOT CAN-FIND(FIRST bttEvent 
                                WHERE bttEvent.hWidget = ttEvent.hWidget
                                  AND bttEvent.hObject NE ttEvent.hObject) THEN
            DELETE OBJECT ttEvent.hWidget NO-ERROR.
        END.
      END.
    END.
    DELETE ttEvent NO-ERROR.
  END.
  IF AVAIL ttObject AND VALID-HANDLE(ttObject.hObject) THEN DO:
    IF CAN-DO("query,browse,fieldMap,temp-table",ttObject.cObjectType) THEN DO:
      CASE ttObject.cObjectType:
/*         WHEN "fieldMap" THEN DO:                                   */
/*           hDeleteObject = ttObject.hObject:TABLE-HANDLE NO-ERROR.  */
/*           IF VALID-HANDLE(hDeleteObject) THEN                      */
/*             DELETE OBJECT ttObject.hObject:TABLE-HANDLE NO-ERROR.  */
/*         END.                                                       */
        WHEN "browse" THEN DO:
          hDeleteObject = ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
          IF VALID-HANDLE(hDeleteObject) THEN
            DELETE OBJECT hDeleteObject NO-ERROR.
          IF VALID-HANDLE(ttObject.hObject) THEN DO:
            hDeleteObject = ttObject.hObject:QUERY NO-ERROR.
            IF VALID-HANDLE(hDeleteObject) THEN
              DELETE OBJECT hDeleteObject NO-ERROR.
          END.
        END.
        WHEN "query" THEN DO:
          hDeleteObject = ttObject.hObject:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
          IF VALID-HANDLE(hDeleteObject) THEN
            DELETE OBJECT hDeleteObject NO-ERROR.
        END.
      END CASE.
      DELETE OBJECT ttObject.hObject NO-ERROR.
    END.
    ELSE IF CAN-DO("tabfolder,container,procedure",ttObject.cObjectType) THEN DO:
      PUBLISH "InvalidateHandle" (ttObject.hObject).
      IF AVAIL ttObject THEN DO:
        IF VALID-HANDLE(ttObject.hObject) THEN APPLY "close" TO ttObject.hObject.
        DELETE PROCEDURE ttObject.hObject NO-ERROR.
      END.
    END.
    ELSE IF ttObject.cObjectType = "toolbar" THEN 
      DELETE OBJECT ttObject.hDesignObject NO-ERROR.
  END.
  IF ttObject.cObjectType = "DynFilter" AND VALID-HANDLE(ttObject.hDesignObject) THEN DO:
    IF ttObject.hDesignObject:TYPE = "query" THEN
      DELETE OBJECT ttObject.hDesignObject:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
    ELSE
      DELETE OBJECT ttObject.hDesignObject:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
    DELETE OBJECT ttObject.hDesignObject NO-ERROR.
  END.
  DELETE ttObject NO-ERROR.
END.

DO ix = 1 TO iFrameIdx:
  IF VALID-HANDLE(hFrame[ix]) THEN
    ASSIGN hFrame[ix]:WIDTH-PIXELS  = hFrame[ix]:WIDTH-PIXELS
           hFrame[ix]:HEIGHT-PIXELS = hFrame[ix]:HEIGHT-PIXELS
           hFrame[ix]:HIDDEN        = NO
           .
END.
IF bHideFrames THEN
  DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteObjectLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteObjectLink Procedure 
FUNCTION DeleteObjectLink RETURNS LOGICAL
  ( INPUT ihObject1  AS HANDLE,
    INPUT ihObject2  AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTmpObject AS HANDLE NO-UNDO.

FIND FIRST ttObjectLink WHERE ttObjectLink.hFromObject = ihObject1 
                          AND ttObjectLink.hToObject   = ihObject2 NO-ERROR.
IF AVAIL ttObjectLink THEN DELETE ttObjectLink.

FIND FIRST ttObjectLink WHERE ttObjectLink.hFromObject = ihObject2 
                          AND ttObjectLink.hToObject   = ihObject1 NO-ERROR.
IF AVAIL ttObjectLink THEN DELETE ttObjectLink.

hTmpObject = DYNAMIC-FUNCTION("getTmpObject").

IF hTmpObject = ihObject1 OR hTmpObject = ihObject2 THEN
  DYNAMIC-FUNCTION("setTmpObject",?).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DispBrwOverlayWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DispBrwOverlayWidgets Procedure 
FUNCTION DispBrwOverlayWidgets RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Display overlay widgets linked to the browse 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget       AS HANDLE NO-UNDO.
DEF VAR cColumnName   AS CHAR   NO-UNDO.
DEF VAR hBrowseColumn AS HANDLE NO-UNDO.

IF ihBrowse:TYPE NE "browse" THEN RETURN NO.

FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ihBrowse
      AND ttObjectLink.cLinkType = "browseoverlay":

  hWidget = ?.
  cColumnName = getAttribute(ttObjectLink.hToObject,"browsecolumn").
  hWidget = WIDGET-HANDLE(getAttribute(ttObjectLink.hToObject,"browsecolumnhandle")) NO-ERROR.
  IF NOT VALID-HANDLE(hWidget) THEN
    DO ix = 1 TO ihBrowse:NUM-COLUMNS:
      hWidget = ihBrowse:GET-BROWSE-COLUMN(ix).
      IF hWidget:NAME = cColumnName THEN
        LEAVE.
    END.
  IF VALID-HANDLE(hWidget) AND ihBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN DO:
    IF ttObjectLink.hToObject:TYPE = "combo-box" THEN DO:
      IF hWidget:SCREEN-VALUE NE "" THEN
        ttObjectLink.hToObject:SCREEN-VALUE = DYNAMIC-FUNCTION("getDropDownValue",ttObjectLink.hToObject,hWidget:SCREEN-VALUE,"|") NO-ERROR.
      ELSE IF LOOKUP("0",ttObjectLink.hToObject:LIST-ITEM-PAIRS,"|") > 0 THEN
        ttObjectLink.hToObject:SCREEN-VALUE = "0".
      ELSE IF LOOKUP("",ttObjectLink.hToObject:LIST-ITEM-PAIRS,"|") > 0 THEN 
        ttObjectLink.hToObject:SCREEN-VALUE = " " NO-ERROR.
      ELSE
        ttObjectLink.hToObject:SCREEN-VALUE = ? NO-ERROR.
               
    END.
    ELSE IF ttObjectLink.hToObject:TYPE = "fill-in" THEN 
      ttObjectLink.hToObject:SCREEN-VALUE = hWidget:SCREEN-VALUE NO-ERROR.
    ELSE IF ttObjectLink.hToObject:TYPE = "toggle-box" THEN DO:
      IF ihBrowse:MULTIPLE THEN ihBrowse:SELECT-ROW(ihBrowse:FOCUSED-ROW) NO-ERROR.
      ttObjectLink.hToObject:SCREEN-VALUE = hWidget:INPUT-VALUE NO-ERROR.
    END.

    ttObjectLink.hToObject:MODIFIED = NO.

    PUBLISH "dotNetMethod" ("dotNetDisplay",ihBrowse,ttObjectLink.hToObject,"",?).
  END.
  ELSE IF VALID-HANDLE(hWidget) THEN DO:
    ASSIGN ttObjectLink.hToObject:HIDDEN         = TRUE
           ttObjectLink.hToObject:X              = 1
           ttObjectLink.hToObject:Y              = 1
           .
    FIND FIRST bttEvent 
         WHERE bttEvent.hObject = ttObjectLink.hToObject
           AND bttEvent.cName   = "lookup" 
         NO-ERROR.
    IF AVAIL bttEvent THEN
      bttEvent.hWidget:HIDDEN = YES.

  END.
END.

IF SESSION:DISPLAY-TYPE NE "TTY" THEN DO:
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    hWidget = ihBrowse:GET-BROWSE-COLUMN(ix).
    IF hWidget:VISIBLE THEN
      LEAVE.
  END.
  APPLY "end-resize" TO hWidget.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DisplayFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DisplayFieldMap Procedure 
FUNCTION DisplayFieldMap RETURNS LOGICAL
  ( INPUT ihFieldMap AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: View the content of a record buffer (the fieldMap handle) using
           the associated lists of widget handles   
    Notes: The widget handles might span multiple frames 
  Changes: Added check for dotNetDisplay links
           If browse overlays are the fieldMap this function doesn't apply
------------------------------------------------------------------------------*/
DEF VAR hWidget                 AS HANDLE NO-UNDO.
DEF VAR cScreenWidgets          AS CHAR   NO-UNDO. 
DEF VAR cBufferFields           AS CHAR   NO-UNDO.
DEF VAR cAvailWidgets           AS CHAR   NO-UNDO.
DEF VAR cPrimaryKeyWidgets      AS CHAR   NO-UNDO.
DEF VAR hField                  AS HANDLE NO-UNDO.
DEF VAR cField                  AS CHAR   NO-UNDO.

DEF BUFFER bttObjectLink FOR ttObjectLink.

IF getAttribute(ihFieldMap,"fieldMapIsBrowse") = "yes" THEN RETURN YES.

ASSIGN cScreenWidgets          = TRIM(getAttribute(ihFieldMap,"ScreenUpdateWidgets") + "," + getAttribute(ihFieldMap,"ScreenDisplayWidgets"),",")
       cBufferFields           = TRIM(getAttribute(ihFieldMap,"BufferUpdateFields")  + "," + getAttribute(ihFieldMap,"BufferDisplayFields"),",")
       cAvailWidgets           = getAttribute(ihFieldMap,"recordavailwidgets") + "," + getAttribute(ihFieldMap,"recordmodifywidgets")
       cPrimaryKeyWidgets      = getAttribute(ihFieldMap,"primaryKeyWidgets")
       .

IF getAttribute(ihFieldMap,"checkmodified") NE "never" THEN
  setAttribute(ihFieldMap,"checkmodified","check").  

IF ihFieldMap:AVAIL THEN DO:
  setAttribute(ihFieldMap,"currentrowid",STRING(ihFieldMap:ROWID)).
  /* Used when external query (and hence one-record-at-the-time handling): */
  setAttribute(ihFieldMap,"currentdbrowid",ihFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
END.

DO ix = 1 TO NUM-ENTRIES(cScreenWidgets):
  IF ENTRY(ix,cScreenWidgets) = "" AND ix > 1 THEN DO:
    hWidget = WIDGET-HANDLE(ENTRY(ix - 1,cScreenWidgets)).
    MESSAGE "Field reference after " hWidget:NAME " is invalid" SKIP
            "(Doesn't exist in frame)"
            VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, FieldMap definition error".
    LEAVE.
  END.

  hWidget = WIDGET-HANDLE(ENTRY(ix,cScreenWidgets)) NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Field reference " ENTRY(ix,cScreenWidgets) " is invalid" SKIP
            "(Doesn't exist in frame)"
            VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".

  IF ihFieldMap:AVAIL THEN DO:
    cField = ENTRY(ix,cBufferFields).
    hField = ihFieldMap:BUFFER-FIELD(cField) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" AND cField MATCHES "*]" THEN DO:
      cField = REPLACE(REPLACE(cField,"[","_"),"]","").
      hField = ihFieldMap:BUFFER-FIELD(cField) NO-ERROR. 
    END.
    IF VALID-HANDLE(hField) THEN DO:  
      IF hWidget:TYPE = "combo-box" THEN 
        ClearComboBox(hWidget).
      IF hWidget:DATA-TYPE = "CHARACTER" THEN DO:
        hWidget:SCREEN-VALUE = STRING(hField:BUFFER-VALUE) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND hWidget:TYPE = "combo-box" AND hWidget:SCREEN-VALUE = ? AND hWidget:ENTRY(1) = "'?'" 
           AND hField:BUFFER-VALUE = "" THEN 
          hWidget:SCREEN-VALUE = hWidget:ENTRY(1).
      END.  
        
      ELSE IF hWidget:DATA-TYPE = "LOGICAL" THEN 
        hWidget:SCREEN-VALUE = IF hField:BUFFER-VALUE THEN STRING ("yes") 
                               ELSE IF hField:BUFFER-VALUE = ? THEN ?
                               ELSE STRING("no") NO-ERROR.
      ELSE
        hWidget:SCREEN-VALUE = STRING(hField:BUFFER-VALUE) NO-ERROR.
    END.
  END.
  ELSE DO:
    IF hWidget:TYPE = "combo-box" THEN 
      ClearComboBox(hWidget).
    ELSE CASE hWidget:DATA-TYPE:
      WHEN "date"    THEN hWidget:SCREEN-VALUE = ?.
      WHEN "decimal" THEN hWidget:SCREEN-VALUE = STRING(0) NO-ERROR.
      WHEN "integer" THEN hWidget:SCREEN-VALUE = STRING(0) NO-ERROR.
      WHEN "logical" THEN hWidget:SCREEN-VALUE = STRING(NO).
      OTHERWISE hWidget:SCREEN-VALUE = "".
    END CASE.
  END.

  hWidget:MODIFIED = FALSE.
END.

/* disable any widgets linked to the fieldMap that depend on record availability: */
DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
  IF VALID-HANDLE(hWidget) THEN 
    hWidget:SENSITIVE = ihFieldMap:AVAIL.
END.

/* Set primary key fields to read-only/not sensitive */
DO ix = 1 TO NUM-ENTRIES(cPrimaryKeyWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cPrimaryKeyWidgets)) NO-ERROR.
  IF VALID-HANDLE(hWidget) THEN DO:
    IF hWidget:TYPE = "fill-in" THEN
      hWidget:READ-ONLY = YES.
    ELSE 
      hWidget:SENSITIVE = NO.
  END.  
END.

FOR EACH bttObjectLink 
    WHERE bttObjectLink.hFromObject = ihFieldMap
      AND bttObjectLink.cLinkType = "dotNetDisplay"
    :
  /* Target: JBoxWrapWindowInForm */
  PUBLISH "dotNetMethod" ("dotNetDisplay",ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,?).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCleanUpObjects) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoCleanUpObjects Procedure 
FUNCTION DoCleanUpObjects RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  - Checks first if any modified widgets in the window that might be saved..
            - Closes automatically any persistent filter windows
------------------------------------------------------------------------------*/
DEF VAR hFilterWindow    AS HANDLE NO-UNDO.
DEF VAR hFlatViewWindow  AS HANDLE NO-UNDO.
DEF VAR hAccumWindow     AS HANDLE NO-UNDO.
DEF VAR hPrintPreviewWin AS HANDLE NO-UNDO.
DEF VAR hBrwConfigWindow AS HANDLE NO-UNDO.
DEF VAR cDummyFile       AS CHAR   NO-UNDO.
DEF VAR bDynamicWidget   AS LOG    NO-UNDO.
DEF VAR hDeleteObject    AS HANDLE NO-UNDO. /* Used to prevent crash in 9.1d */
DEF VAR cLogText         AS CHAR   NO-UNDO.
DEF VAR cDeleteSeq       AS CHAR   NO-UNDO INIT "query,browse,fieldmap,temp-table,browse-search-field,toolbar,fill-in,combo-box,toggle-box,viewer,procedure,tabfolder,container,super-procedure,*".
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR hParentProc      AS HANDLE NO-UNDO.
DEF VAR hParentWin       AS HANDLE NO-UNDO.
    
IF NOT VALID-HANDLE(ihWindow) THEN RETURN YES.

EMPTY TEMP-TABLE ttAttributeCopy.

FOR EACH ttObject
    WHERE ttObject.hWindow = ihWindow
      AND ttObject.cObjectType = "fieldMap"
   :
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ttObject.hObject
         AND ttObjectLink.cLinkType = "toolbar"
       NO-ERROR.
  IF AVAIL ttObjectLink AND
     getAttribute(ttObject.hObject,"checkmodified") NE "never" AND
     getAttribute(ttObject.hObject,"checkmodified") NE "no" THEN DO:

   iReturn = 0.
   IF DYNAMIC-FUNCTION("CheckModified",ttObject.hObject,"check") THEN 
     iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
                          IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                            "Lagre endringer?"
                          ELSE 
                            "Save changes?"
                          ,"","").
    IF iReturn = 6 THEN
      DYNAMIC-FUNCTION("ApplyEvent",ttObjectLink.hToObject,"save").
    ELSE IF iReturn = 2 THEN DO:
      PUBLISH "CancelFormClosing" (yes).
      RETURN FALSE.
    END.  
  END.
END.

hParentProc = ihWindow:INSTANTIATING-PROCEDURE:INSTANTIATING-PROCEDURE.

ihWindow:VISIBLE = NO.

IF VALID-HANDLE(hParentProc) AND VALID-HANDLE(hParentProc:CURRENT-WINDOW) THEN 
  PUBLISH "EndJBoxEvent" (?,hParentProc,?,"JboxWindowClose").

DO ix = 1 TO NUM-ENTRIES(cDeleteSeq):
  FOR EACH ttObject 
      WHERE ttObject.hWindow = ihWindow
        AND CAN-DO(ENTRY(ix,cDeleteSeq),ttObject.cObjectType):
  
    cLogText = "Deleting: " + ttObject.cObjectType + " " + ttObject.cObjectName + "  - Valid: " + STRING(VALID-HANDLE(ttObject.hObject)).
    {incl/methodlog.i cLogText}
  
    FOR EACH ttObjectLink 
        WHERE ttObjectLink.hFromObject = ttObject.hObject:
      DELETE ttObjectLink.
    END.
    FOR EACH ttObjectLink 
        WHERE ttObjectLink.hToObject = ttObject.hObject:
      DELETE ttObjectLink.
    END.
    FOR EACH ttEvent OF ttObject:
      IF VALID-HANDLE(ttEvent.hWidget) THEN DO:
        bDynamicWidget = ttEvent.hWidget:DYNAMIC NO-ERROR.
        IF bDynamicWidget THEN DO:
          IF VALID-HANDLE(ttEvent.hWidget) THEN DO:
            cLogText = "  Deleting event widget: " + ttEvent.cName + " Dynamic object: " + ttEvent.hWidget:TYPE + ", " + ttEvent.hWidget:NAME.
            {incl/methodlog.i cLogText}
            IF ttEvent.hWidget:TYPE = "browse" THEN DO:
              hDeleteObject = ttEvent.hWidget:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
              IF VALID-HANDLE(hDeleteObject) AND NOT DYNAMIC-FUNCTION("getIsTableCached",hDeleteObject) THEN
                DELETE OBJECT hDeleteObject NO-ERROR.
              IF VALID-HANDLE(ttEvent.hWidget) THEN DO:
                hDeleteObject = ttEvent.hWidget:QUERY NO-ERROR.
                IF VALID-HANDLE(hDeleteObject) THEN
                  DELETE OBJECT hDeleteObject NO-ERROR.
              END.
            END.
            ELSE IF ttEvent.hWidget:TYPE = "query" THEN DO:
              hDeleteObject = ttEvent.hWidget:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
              IF VALID-HANDLE(hDeleteObject) AND NOT DYNAMIC-FUNCTION("getIsTableCached",hDeleteObject) THEN
                DELETE OBJECT hDeleteObject NO-ERROR.
            END.
            IF NOT CAN-FIND(FIRST bttEvent 
                                  WHERE bttEvent.hWidget = ttEvent.hWidget
                                    AND bttEvent.hObject NE ttEvent.hObject) THEN
              DELETE OBJECT ttEvent.hWidget NO-ERROR.
          END.
        END.
      END.
      DELETE ttEvent.
    END.
    FOR EACH ttAttribute OF ttObject:
      IF ttAttribute.cName = "filterhandle" THEN
        hFilterWindow = WIDGET-HANDLE(getAttribute(ttObject.hObject,"filterhandle")).
      IF ttAttribute.cName = "accumhandle" THEN
        hAccumWindow = WIDGET-HANDLE(getAttribute(ttObject.hObject,"accumhandle")).
      IF ttAttribute.cName = "flatviewhandle" THEN
        hFlatViewWindow = WIDGET-HANDLE(getAttribute(ttObject.hObject,"flatviewhandle")).
      IF ttAttribute.cName = "browseconfighandle" THEN
        hBrwConfigWindow = WIDGET-HANDLE(getAttribute(ttObject.hObject,"browseconfighandle")).
      IF ttAttribute.cName = "printPreviewHandle" THEN
        hPrintPreviewWin = WIDGET-HANDLE(getAttribute(ttObject.hObject,"printPreviewHandle")).
      DELETE ttAttribute.
    END.
  
    IF AVAIL ttObject AND VALID-HANDLE(ttObject.hObject) THEN DO:
      IF CAN-DO("query,browse,fieldMap,temp-table",ttObject.cObjectType) THEN DO:
        CASE ttObject.cObjectType:
          WHEN "fieldMap" THEN DO:
            hDeleteObject = ttObject.hObject:TABLE-HANDLE NO-ERROR.
            IF VALID-HANDLE(hDeleteObject) AND NOT DYNAMIC-FUNCTION("getIsTableCached",hDeleteObject) THEN
              DELETE OBJECT ttObject.hObject:TABLE-HANDLE NO-ERROR.
          END.
          WHEN "browse" THEN DO:
/*             hDeleteObject = ttEvent.hWidget:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR. */
            hDeleteObject = ttObject.hObject:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
            IF VALID-HANDLE(hDeleteObject) AND NOT DYNAMIC-FUNCTION("getIsTableCached",hDeleteObject) THEN
              DELETE OBJECT hDeleteObject NO-ERROR.
/*             IF VALID-HANDLE(ttEvent.hWidget) THEN DO: */
              hDeleteObject = ttObject.hObject:QUERY NO-ERROR.
/*               hDeleteObject = ttEvent.hWidget:QUERY NO-ERROR. */
              IF VALID-HANDLE(hDeleteObject) THEN
                DELETE OBJECT hDeleteObject NO-ERROR.
/*             END. */
          END.
          WHEN "query" THEN DO:
            hDeleteObject = ttObject.hObject:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.
            IF VALID-HANDLE(hDeleteObject) AND NOT DYNAMIC-FUNCTION("getIsTableCached",hDeleteObject) THEN
              DELETE OBJECT hDeleteObject NO-ERROR.
          END.
        END CASE.
        DELETE OBJECT ttObject.hObject NO-ERROR.
      END.
      ELSE IF CAN-DO("tabfolder,container,procedure,panel,super-procedure",ttObject.cObjectType) THEN DO:
        PUBLISH "InvalidateHandle" (ttObject.hObject).
        IF AVAIL ttObject THEN DO:
          IF VALID-HANDLE(ttObject.hObject) THEN APPLY "close" TO ttObject.hObject.
          DELETE PROCEDURE ttObject.hObject NO-ERROR.
        END.
      END.
    END.
  /*   IF AVAIL ttObject AND ttObject.cObjectType = "DynFilter" AND VALID-HANDLE(ttObject.hDesignObject) THEN DO:  */
  /*     IF ttObject.hDesignObject:TYPE = "query" THEN DO:                                                         */
  /*       hDeleteObject = ttObject.hDesignObject:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.                      */
  /*       IF VALID-HANDLE(hDeleteObject) THEN                                                                     */
  /*         DELETE OBJECT hDeleteObject NO-ERROR.                                                                 */
  /* /*       DELETE OBJECT ttObject.hDesignObject:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR. */                  */
  /*     END.                                                                                                      */
  /*     ELSE DO:                                                                                                  */
  /*       hDeleteObject = ttObject.hDesignObject:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.                */
  /*       IF VALID-HANDLE(hDeleteObject) THEN                                                                     */
  /*         DELETE OBJECT hDeleteObject NO-ERROR.                                                                 */
  /* /*       DELETE OBJECT ttObject.hDesignObject:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE NO-ERROR.  */           */
  /*     END.                                                                                                      */
  /*     DELETE OBJECT ttObject.hDesignObject NO-ERROR.                                                            */
  /*   END.                                                                                                        */
  
    DELETE ttObject NO-ERROR.
  
    IF VALID-HANDLE(hFilterWindow) THEN
      APPLY "close" TO hFilterWindow.
    IF VALID-HANDLE(hAccumWindow) THEN
      APPLY "close" TO hAccumWindow.
    IF VALID-HANDLE(hFlatViewWindow) THEN
      APPLY "close" TO hFlatViewWindow.
    IF VALID-HANDLE(hBrwConfigWindow) THEN
      APPLY "close" TO hBrwConfigWindow.
    IF VALID-HANDLE(hPrintPreviewWin) THEN
      APPLY "close" TO hPrintPreviewWin.
  END.
END.

FOR EACH ttAttribute WHERE ttAttribute.hObject = ihWindow:
  DELETE ttAttribute.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FieldMapFieldsToTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FieldMapFieldsToTT Procedure 
FUNCTION FieldMapFieldsToTT RETURNS HANDLE
  ( INPUT ihFieldMap   AS HANDLE,
    INPUT icExNameList AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bttObject FOR ttObject.
FIND bttObject WHERE bttObject.hObject = ihFieldMap NO-ERROR.
IF AVAIL bttObject THEN
  RETURN DYNAMIC-FUNCTION("FrameFieldsToTT",bttObject.hDesignObject,
                          TRIM(getAttribute(ihFieldMap,"screenUpdateFields") + "," + getAttribute(ihFieldMap,"screenDisplayFields"),","),
                          icExNameList).
ELSE
  RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillBrowse Procedure 
FUNCTION FillBrowse RETURNS INTEGER
  ( INPUT ihBrowse           AS HANDLE,
    INPUT iBatchSize         AS INT,
    INPUT iStartRow          AS INT,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icSortColumn       AS CHAR,
    INPUT ibDesc             AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iMaxCount        AS INT    NO-UNDO.

/*RUN DoFillBrowse.p (ihBrowse,iBatchSize,iStartRow,icBuffersAndFields,icQueryCriteria,icSortColumn,ibDesc,hCurrSourceProc,OUTPUT iMaxCount).*/
RUN DoFillBrowse(ihBrowse,iBatchSize,iStartRow,icBuffersAndFields,icQueryCriteria,icSortColumn,ibDesc,OUTPUT iMaxCount). 

RETURN iMaxCount.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillLocalBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillLocalBrowse Procedure 
FUNCTION FillLocalBrowse RETURNS INTEGER
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icSortColumn       AS CHAR,
    INPUT ibDesc             AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  Called when the browse has all data and doesn't need to go back to server for search, sort, etc
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR cSortMap          AS CHAR   NO-UNDO.
DEF VAR httBuffer         AS HANDLE NO-UNDO.
DEF VAR iRecordCount      AS INT    NO-UNDO.
DEF VAR cStatFields       AS CHAR   NO-UNDO.
DEF VAR cStatFieldValues  AS CHAR   NO-UNDO.
DEF VAR fStatValues       AS DEC    NO-UNDO EXTENT 100.
DEF VAR hOrgTable         AS HANDLE NO-UNDO.
DEF VAR hOrgBuffer        AS HANDLE NO-UNDO.
DEF VAR hOrgQuery         AS HANDLE NO-UNDO.
DEF VAR cLocalSort        AS CHAR   NO-UNDO.
DEF VAR bSortMap          AS LOG    NO-UNDO.
DEF VAR hRowCount         AS HANDLE NO-UNDO.
DEF VAR cError            AS CHAR   NO-UNDO.
DEF VAR cOrgLocalSort     AS CHAR   NO-UNDO.

SESSION:SET-WAIT-STATE("general").

IF getAttribute(ihBrowse,"temptablebuffer") NE "" THEN 
  hBuffer = WIDGET-HANDLE(getAttribute(ihBrowse,"temptablebuffer")).
ELSE
  hBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).

ASSIGN cLocalSort    = IF getAttribute(ihBrowse,"localsort") NE "" THEN getAttribute(ihBrowse,"localsort") ELSE icSortColumn
       cOrgLocalSort = cLocalSort.

IF cLocalSort NE "" AND SUBSTR(cLocalSort,LENGTH(cLocalSort)) = "]" THEN DO:
  cLocalSort = "jbextent_" + RIGHT-TRIM(SUBSTR(cLocalSort,R-INDEX(cLocalSort,"[") + 1),"]") + "_" + SUBSTR(cLocalSort,1,R-INDEX(cLocalSort,"[") - 1) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "Sort on " cLocalSort SKIP
            ERROR-STATUS:GET-MESSAGE(1)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

bOk = ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " " + 
                                   getAttribute(ihBrowse,"basequery") + 
                                     getAttribute(ihBrowse,"queryfilter") + 
                                     getAttribute(ihBrowse,"querywhere")  
                                  + (IF cLocalSort NE "" THEN 
                                      " BY " + cLocalSort + (IF ibDesc THEN " DESC" ELSE "") 
                                     ELSE "")
                                  + (IF VALID-HANDLE(hRowCount) THEN " BY RowCount" ELSE "")) NO-ERROR.

IF NOT bOk OR ERROR-STATUS:NUM-MESSAGES NE 0 THEN DO:
  IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN DO:
    cError = ERROR-STATUS:GET-MESSAGE(1).
    MESSAGE PROGRAM-NAME(1) SKIP
            "Query prepare failed: " SKIP
            "FOR EACH " + hBuffer:NAME + " " + 
                              getAttribute(ihBrowse,"basequery") + 
                                getAttribute(ihBrowse,"queryfilter") + 
                                getAttribute(ihBrowse,"querywhere")  
                           + (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF ibDesc THEN " DESC" ELSE "") ELSE "") SKIP(1)
            cError SKIP(2)
            "It will be automatically changed to:" SKIP
            "FOR EACH " + hBuffer:NAME + " " +
                             (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF ibDesc THEN " DESC" ELSE "") ELSE "") SKIP(1)
            "(the failing query is copied to the clipboard)" SKIP(1)
            "TIP: It might be that you are setting a criteria on a field that is not included in the field specification" SKIP(1)
            "THIS MESSAGE IS ONLY SHOWN WHEN QUERY-LOGGING IS ON"
            VIEW-AS ALERT-BOX WARNING.
    CLIPBOARD:VALUE = "FOR EACH " + hBuffer:NAME + " " + 
                              getAttribute(ihBrowse,"basequery") + 
                                getAttribute(ihBrowse,"queryfilter") + 
                                getAttribute(ihBrowse,"querywhere")  
                           + (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF ibDesc THEN " DESC" ELSE "") ELSE "").
  END.
  ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " " +
                             (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF ibDesc THEN " DESC" ELSE "") ELSE "")).
END.

IF getAttribute(ihBrowse,"distinctcolumns") NE "" AND getAttribute(ihBrowse,"originaltemptable") = "" THEN DO:
  CREATE TEMP-TABLE hOrgTable IN WIDGET-POOL "localTT".
  hOrgTable:CREATE-LIKE(hBuffer).
  hOrgTable:TEMP-TABLE-PREPARE("org_" + hBuffer:NAME).
  hOrgBuffer = hOrgTable:DEFAULT-BUFFER-HANDLE.
  DYNAMIC-FUNCTION("NewObject",ihBrowse:WINDOW,hOrgTable,"").
  setAttribute(hOrgTable,"currentdistinctcolumns",getAttribute(ihBrowse,"distinctcolumns")).
  setAttribute(ihBrowse,"originaltemptable",STRING(hOrgTable)).
  CREATE QUERY hOrgQuery.
  hOrgQuery:SET-BUFFERS(hBuffer).
  hOrgQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
  hOrgQuery:QUERY-OPEN().
  hOrgQuery:GET-FIRST().
  ix = 0.
  REPEAT WHILE NOT hOrgQuery:QUERY-OFF-END:
    hOrgBuffer:BUFFER-CREATE().
    hOrgBuffer:BUFFER-COPY(hBuffer).
    hOrgQuery:GET-NEXT().
    ix = ix + 1.
  END.

  getLocalDistinctRows(ihBrowse).
END.
ELSE IF getAttribute(ihBrowse,"originaltemptable") NE "" THEN DO:
  hOrgTable = WIDGET-HANDLE(getAttribute(ihBrowse,"originaltemptable")) NO-ERROR.

  IF VALID-HANDLE(hOrgTable) THEN DO:
    hBuffer:EMPTY-TEMP-TABLE().
    hOrgBuffer = hOrgTable:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY hOrgQuery.
    hOrgQuery:SET-BUFFERS(hOrgBuffer).
    hOrgQuery:QUERY-PREPARE("FOR EACH " + hOrgBuffer:NAME).
    hOrgQuery:QUERY-OPEN().
    hOrgQuery:GET-FIRST().
    ix = 0.
    REPEAT WHILE NOT hOrgQuery:QUERY-OFF-END:
      hBuffer:BUFFER-CREATE().
      hBuffer:BUFFER-COPY(hOrgBuffer).
      hOrgQuery:GET-NEXT().
      ix = ix + 1.
    END.
    DELETE OBJECT hOrgQuery.
    IF getAttribute(ihBrowse,"distinctcolumns") NE "" THEN
      getLocalDistinctRows(ihBrowse).
    ELSE DO: 
      DYNAMIC-FUNCTION("DeleteObject",hOrgTable).
      DELETE OBJECT hOrgTable NO-ERROR.
      setAttribute(ihBrowse,"originaltemptable","").
    END.
  END.
END.

bOK = ihBrowse:QUERY:QUERY-OPEN NO-ERROR.
IF NOT bOk AND ERROR-STATUS:GET-NUMBER(1) = 141 THEN DO:  /* Index field too long */
  cLocalSort = " BY SUBSTR(" + cLocalSort + ",1,50)".
  ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + cLocalSort + (IF ibDesc THEN " DESC" ELSE "")
                             + (IF VALID-HANDLE(hRowCount) THEN " BY RowCount" ELSE "")).
  bOK = ihBrowse:QUERY:QUERY-OPEN NO-ERROR.
END.

IF bSetSortLabel THEN DO:
  cSortMap = getAttribute(ihBrowse,"sortmap").
  DO ix = 1 TO NUM-ENTRIES(cSortMap):
    IF ENTRY(2,ENTRY(ix,cSortMap),";") = icSortColumn THEN 
      ASSIGN icSortColumn = ENTRY(1,ENTRY(ix,cSortMap),";")
             bSortMap     = YES.
  END.
  DYNAMIC-FUNCTION("setSortLabel",ihBrowse,IF bSortMap THEN icSortColumn ELSE cOrgLocalSort,ibDesc).
END.

ihBrowse:QUERY:GET-FIRST().
setAttribute(ihBrowse,"firstrowid",STRING(ihBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)).

cStatFields = getAttribute(ihBrowse,"querystatfields").

DYNAMIC-FUNCTION("DoLockWindow",ihBrowse:WINDOW).

hRowCount = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowCount") NO-ERROR.

REPEAT WHILE NOT ihBrowse:QUERY:QUERY-OFF-END:
  iRecordCount = iRecordCount + 1.

  IF VALID-HANDLE(hRowCount) THEN
    hRowCount:BUFFER-VALUE = iRecordCount.

  IF cStatFields NE "" THEN DO ix = 1 TO NUM-ENTRIES(cStatFields):
    fStatValues[ix] = fStatValues[ix] + hBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
  END.

  setAttribute(ihBrowse,"lastrowid",STRING(ihBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)).
  ihBrowse:QUERY:GET-NEXT().
END.

ihBrowse:QUERY:QUERY-OPEN().
/* ihBrowse:QUERY:GET-FIRST(). */

DYNAMIC-FUNCTION("DoLockWindow",?).

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  setAttribute(ihBrowse,"statvalue" + ENTRY(ix,cStatFields),STRING(fStatValues[ix])).
  cStatFieldValues = cStatFieldValues + ENTRY(ix,cStatFields) + "|" + STRING(fStatValues[ix]) + ";".
END.
setAttribute(ihBrowse,"querystatfieldvalues",cStatFieldValues).

SESSION:SET-WAIT-STATE("").

setAttribute(ihBrowse,"recordcount",STRING(iRecordCount)).

RETURN iRecordCount. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillLocalQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillLocalQuery Procedure 
FUNCTION FillLocalQuery RETURNS INTEGER
  ( INPUT ihQuery            AS HANDLE,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Called when the browse has all data and doesn't need to go back to server for search, sort, etc
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR cSortMap          AS CHAR   NO-UNDO.
DEF VAR httBuffer         AS HANDLE NO-UNDO.
DEF VAR iRecordCount      AS INT    NO-UNDO.
DEF VAR cStatFields       AS CHAR   NO-UNDO.
DEF VAR cStatFieldValues  AS CHAR   NO-UNDO.
DEF VAR fStatValues       AS DEC    NO-UNDO EXTENT 100.
DEF VAR hOrgTable         AS HANDLE NO-UNDO.
DEF VAR hOrgBuffer        AS HANDLE NO-UNDO.
DEF VAR hOrgQuery         AS HANDLE NO-UNDO.
DEF VAR cLocalSort        AS CHAR   NO-UNDO.
DEF VAR bSortMap          AS LOG    NO-UNDO.
DEF VAR bDesc             AS LOG    NO-UNDO.

IF getAttribute(ihQuery,"temptablebuffer") NE "" THEN 
  hBuffer = WIDGET-HANDLE(getAttribute(ihQuery,"temptablebuffer")).
ELSE
  hBuffer = ihQuery:GET-BUFFER-HANDLE(1).

ASSIGN cLocalSort = getAttribute(ihQuery,"localsort")
       bDesc      = getAttribute(ihQuery,"querydesc") = "desc".

bOk = ihQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " " + 
                                   getAttribute(ihQuery,"basequery") + 
                                     getAttribute(ihQuery,"queryfilter") + 
                                     getAttribute(ihQuery,"querywhere")  
                             + (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF bDesc THEN " DESC" ELSE "") ELSE "")) NO-ERROR.


IF NOT bOk OR ERROR-STATUS:NUM-MESSAGES NE 0 THEN DO:
  IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN DO:
    MESSAGE PROGRAM-NAME(1) SKIP
            "Query prepare failed: " SKIP
            "FOR EACH " + hBuffer:NAME + " " + 
                              getAttribute(ihQuery,"basequery") + 
                                getAttribute(ihQuery,"queryfilter") + 
                                getAttribute(ihQuery,"querywhere")  
                           + (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF bDesc THEN " DESC" ELSE "") ELSE "") SKIP(1)
            ERROR-STATUS:GET-MESSAGE(1) SKIP(2)
            "It will be automatically changed to:" SKIP
            "FOR EACH " + hBuffer:NAME + " " +
                             (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF bDesc THEN " DESC" ELSE "") ELSE "") SKIP(1)
            "(the failing query is copied to the clipboard)"
            VIEW-AS ALERT-BOX WARNING.
    CLIPBOARD:VALUE = "FOR EACH " + hBuffer:NAME + " " + 
                              getAttribute(ihQuery,"basequery") + 
                                getAttribute(ihQuery,"queryfilter") + 
                                getAttribute(ihQuery,"querywhere")  
                           + (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF bDesc THEN " DESC" ELSE "") ELSE "").
  END.
  ihQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " " +
                             (IF cLocalSort NE "" THEN " BY " + cLocalSort + (IF bDesc THEN " DESC" ELSE "") ELSE "")).
END.

bOK = ihQuery:QUERY-OPEN NO-ERROR.
IF NOT bOk AND ERROR-STATUS:GET-NUMBER(1) = 141 THEN DO:  /* Index field too long */
  cLocalSort = "SUBSTR(" + cLocalSort + ",1,50)".
  ihQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + cLocalSort + (IF bDesc THEN " DESC" ELSE "")).
  bOK = ihQuery:QUERY-OPEN NO-ERROR.
END.

ihQuery:GET-FIRST().
setAttribute(ihQuery,"firstrowid",STRING(ihQuery:GET-BUFFER-HANDLE(1):ROWID)).

cStatFields = getAttribute(ihQuery,"querystatfields").

REPEAT WHILE NOT ihQuery:QUERY-OFF-END:
  iRecordCount = iRecordCount + 1.

  IF cStatFields NE "" THEN DO ix = 1 TO NUM-ENTRIES(cStatFields):
    fStatValues[ix] = fStatValues[ix] + hBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
  END.

  setAttribute(ihQuery,"lastrowid",STRING(ihQuery:GET-BUFFER-HANDLE(1):ROWID)).
  ihQuery:GET-NEXT().
END.

ihQuery:GET-FIRST().

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  setAttribute(ihQuery,"statvalue" + ENTRY(ix,cStatFields),STRING(fStatValues[ix])).
  cStatFieldValues = cStatFieldValues + ENTRY(ix,cStatFields) + "|" + STRING(fStatValues[ix]) + ";".
END.
setAttribute(ihQuery,"querystatfieldvalues",cStatFieldValues).

setAttribute(ihQuery,"recordcount",STRING(iRecordCount)).

RETURN iRecordCount. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillQuery Procedure 
FUNCTION FillQuery RETURNS INTEGER
  ( INPUT ihQuery            AS HANDLE,
    INPUT iBatchSize         AS INT,
    INPUT iStartRow          AS INT,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  iStartRow = -1: Keep already read records in query            
------------------------------------------------------------------------------*/
DEF VAR iMaxCount           AS INT    NO-UNDO.

RUN DoFillQuery (ihQuery,iBatchSize,iStartRow,icBuffersAndFields,icQueryCriteria,OUTPUT iMaxCount).

RETURN iMaxCount.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixQuery Procedure 
FUNCTION FixQuery RETURNS CHARACTER
  ( INPUT icQueryCriteria AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQuery    AS CHAR NO-UNDO.
DEF VAR cJoin     AS CHAR NO-UNDO.
DEF VAR iy        AS INT  NO-UNDO.
DEF VAR cTemp1    AS CHAR NO-UNDO.
DEF VAR cTemp2    AS CHAR NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icQueryCriteria):
  IF ix = 1 THEN DO:
    IF ENTRY(ix,icQueryCriteria) NE "" THEN 
      cTemp1 = ENTRY(ix,icQueryCriteria).
    ELSE cTemp1 = "WHERE true".
  END.
  ELSE IF NOT (ix > 2 AND ENTRY(ix,icQueryCriteria) = ENTRY(ix - 1,icQueryCriteria)) THEN
    cTemp1 = cTemp1 + "," + ENTRY(ix,icQueryCriteria).
END.
IF cTemp1 NE icQueryCriteria THEN
  icQueryCriteria = cTemp1.

icQueryCriteria = DYNAMIC-FUNCTION("FixQueryOperators",icQueryCriteria).

DO ix = 1 TO LENGTH(icQueryCriteria):
  IF SUBSTR(icQueryCriteria,ix,6) = "first " OR
     SUBSTR(icQueryCriteria,ix,5) = "last " OR
     SUBSTR(icQueryCriteria,ix,5) = "each " 
     THEN DO:
    DO iy = ix TO MAX(1,ix - 10) BY -1:
      IF SUBSTR(icQueryCriteria,iy,1) = "," THEN LEAVE.
    END.
    LEAVE.
  END.
END.

IF iy > 1 THEN 
  ASSIGN cQuery = SUBSTR(icQueryCriteria,1,iy - 1)
         cJoin  = SUBSTR(icQueryCriteria,iy)
         .
ELSE
  RETURN icQueryCriteria.

ASSIGN cTemp1 = SUBSTR(cQuery,1,INDEX(cQuery,"where") + 4)
       cTemp2 = SUBSTR(cQuery,INDEX(cQuery,"where") + 5)
       icQueryCriteria = cTemp1 + REPLACE(cTemp2,"where"," and") + cJoin.

RETURN icQueryCriteria.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixSortString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixSortString Procedure 
FUNCTION FixSortString RETURNS CHARACTER
  ( INPUT ihQueryObject  AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSortString AS CHAR NO-UNDO.
DEF VAR cNewString  AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR cElement    AS CHAR NO-UNDO.
DEF VAR cExtFields  AS CHAR NO-UNDO.
DEF VAR cAltExtFld  AS CHAR NO-UNDO.

cSortString = getAttribute(ihQueryObject,"localsort").

IF INDEX(cSortString,"[") > 0 THEN DO:
  ASSIGN cExtFields = getAttribute(ihQueryObject,"ExtentFields")
         cAltExtFld = getAttribute(ihQueryObject,"altExtentFields").  
  DO ix = 1 TO NUM-ENTRIES(cSortString," "):
    cElement = ENTRY(ix,cSortString," ").
    IF CAN-DO(cExtFields,cElement) THEN
      cNewString = cNewString + ENTRY(LOOKUP(cElement,cExtFields),cAltExtFld) + " ".
    ELSE
      cNewString = cNewString + cElement + " ".
  END.
END.
ELSE cNewString = cSortString.

RETURN TRIM(cNewString).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getActiveFilterButtonFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getActiveFilterButtonFile Procedure 
FUNCTION getActiveFilterButtonFile RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cActiveFilterButton.  

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

&IF DEFINED(EXCLUDE-getAttributeList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAttributeList Procedure 
FUNCTION getAttributeList RETURNS CHARACTER
  ( INPUT ihObject        AS HANDLE,
    INPUT icName          AS CHAR,
    INPUT icExceptionList AS CHAR,
    INPUT ibBlanks        AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Return all attribute names and values where attribute name begins or matches.. 
    Notes: Returns values corresponding to the user getUserSetting function: 
           <name,name,..|<value>CHR1<value> 
------------------------------------------------------------------------------*/
DEF VAR ocReturn     AS CHAR NO-UNDO.
DEF VAR cSettingList AS CHAR NO-UNDO.

IF ihObject NE ? THEN DO:
  IF INDEX(icName,"*") > 0 THEN
    FOR EACH ttAttribute 
        WHERE ttAttribute.hObject = ihObject
          AND (IF icExceptionList NE "" THEN NOT CAN-DO(icExceptionList,ttAttribute.cValue) ELSE YES)
          AND (IF NOT ibBlanks THEN ttAttribute.cValue NE "" ELSE YES)
          AND ttAttribute.cName NE ""
          AND ttAttribute.cName MATCHES icName:  
      ASSIGN cSettingList = cSettingList + ttAttribute.cName + ","    
             ocReturn     = ocReturn + ttAttribute.cValue + CHR(1).
    END.
  ELSE
    FOR EACH ttAttribute 
        WHERE ttAttribute.hObject = ihObject
          AND (IF icExceptionList NE "" THEN NOT CAN-DO(icExceptionList,ttAttribute.cValue) ELSE YES)
          AND (IF NOT ibBlanks THEN ttAttribute.cValue NE "" ELSE YES)
          AND ttAttribute.cName NE ""
          AND ttAttribute.cName BEGINS icName:  
      ASSIGN cSettingList = cSettingList + ttAttribute.cName + ","
             ocReturn     = ocReturn + ttAttribute.cValue + CHR(1).
    END.
END.
ELSE DO:
  IF INDEX(icName,"*") > 0 THEN
    FOR EACH ttAttribute 
        WHERE (IF icExceptionList NE "" THEN NOT CAN-DO(icExceptionList,ttAttribute.cValue) ELSE YES)
          AND (IF NOT ibBlanks THEN ttAttribute.cValue NE "" ELSE YES)
          AND ttAttribute.cName NE ""
          AND ttAttribute.cName MATCHES icName:  
      ASSIGN cSettingList = cSettingList + ttAttribute.cName + ","
             ocReturn     = ocReturn + ttAttribute.cValue + CHR(1).
    END.
  ELSE
    FOR EACH ttAttribute 
        WHERE (IF icExceptionList NE "" THEN NOT CAN-DO(icExceptionList,ttAttribute.cValue) ELSE YES)
          AND (IF NOT ibBlanks THEN ttAttribute.cValue NE "" ELSE YES)
          AND ttAttribute.cName NE ""
          AND ttAttribute.cName BEGINS icName:  
      ASSIGN cSettingList = cSettingList + ttAttribute.cName + ","
             ocReturn     = ocReturn + ttAttribute.cValue + CHR(1).
    END.
END.

ASSIGN ocReturn     = REPLACE(SUBSTR(ocReturn,1,LENGTH(ocReturn) - 1),"|",CHR(3))
       cSettingList = TRIM(cSettingList,","). 

ocReturn = cSettingList + "|" + ocReturn.

RETURN ocReturn.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBehaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBehaviour Procedure 
FUNCTION getBehaviour RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cBehaviour.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseColumn Procedure 
FUNCTION getBrowseColumn RETURNS HANDLE
  ( INPUT ihBrowse    AS HANDLE,
    INPUT icFieldName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix      AS INT NO-UNDO.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = icFieldName THEN 
    RETURN ihBrowse:GET-BROWSE-COLUMN(ix).
END.
  
RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseColumnLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseColumnLabel Procedure 
FUNCTION getBrowseColumnLabel RETURNS CHARACTER
  ( INPUT ihBrowse    AS HANDLE,
    INPUT icFieldName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix      AS INT NO-UNDO.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = icFieldName THEN 
    RETURN getStrippedSortLabel(ihBrowse:GET-BROWSE-COLUMN(ix)).
END.
  
RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseColumnNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseColumnNum Procedure 
FUNCTION getBrowseColumnNum RETURNS INTEGER
  ( INPUT ihBrowse    AS HANDLE,
    INPUT icFieldName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix      AS INT NO-UNDO.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = icFieldName THEN 
    RETURN ix.
END.

RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferFieldDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBufferFieldDataType Procedure 
FUNCTION getBufferFieldDataType RETURNS CHARACTER
  ( INPUT ihBuffer AS HANDLE,
    INPUT icField  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix      AS INT NO-UNDO.

IF VALID-HANDLE(ihBuffer) THEN
  DO ix = 1 TO ihBuffer:NUM-FIELDS:
    IF ihBuffer:BUFFER-FIELD(ix):NAME = icField THEN
      RETURN ihBuffer:BUFFER-FIELD(ix):DATA-TYPE.
  END.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getColNumFromRGB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getColNumFromRGB Procedure 
FUNCTION getColNumFromRGB RETURNS INTEGER
  ( INPUT iRGBcolor AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy AS INT NO-UNDO.

IF iRGBcolor = 0 THEN RETURN 15.

IF iRGBcolor NE 0 THEN DO iy = 0 TO COLOR-TABLE:NUM-ENTRIES:
  IF COLOR-TABLE:GET-RGB-VALUE(iy) = iRGBcolor THEN RETURN iy.
END.

IF iy = 256 THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          "Max user configurable colors reached. Reusing latest color" SKIP
          VIEW-AS ALERT-BOX WARNING.
ELSE
  ASSIGN iy = COLOR-TABLE:NUM-ENTRIES
         COLOR-TABLE:NUM-ENTRIES = iy + 1.

COLOR-TABLE:SET-DYNAMIC(iy, yes).
COLOR-TABLE:SET-RGB-VALUE(iy,iRGBcolor).

RETURN iy.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContainerHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContainerHandle Procedure 
FUNCTION getContainerHandle RETURNS HANDLE
  ( INPUT ihProcedure    AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hContainer  AS HANDLE NO-UNDO.
DEF VAR hProc       AS HANDLE NO-UNDO.

hProc = ihProcedure.

DO ix = 1 TO 4:
  FOR FIRST ttObjectLink
      WHERE ttObjectLink.hFromObject = hProc
        AND ttObjectLink.cLinkType   = "parent"
     ,FIRST ttObject
            WHERE ttObject.hObject = ttObjectLink.hToObject
      :
    IF ttObject.cObjectType = "container" THEN
      hContainer = ttObject.hObject.
    ELSE 
      hProc = ttObject.hObject.
  END.
  IF hContainer NE ? THEN LEAVE.
END.    

IF hContainer = ? THEN hContainer = ihProcedure.

RETURN hContainer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDesignObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDesignObject Procedure 
FUNCTION getDesignObject RETURNS HANDLE
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihObject NO-LOCK NO-ERROR.
IF AVAIL bttObject THEN
  RETURN bttObject.hDesignObject.
ELSE
  RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventAction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEventAction Procedure 
FUNCTION getEventAction RETURNS CHARACTER
  ( INPUT ihParent AS HANDLE,
    INPUT ihWidget AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Parent might be anything (toolbar, buffer, popup..) 
           To distinguish corresoponding menu-items and buttons, use type (button / menu-item) - assumes button
------------------------------------------------------------------------------*/
FIND FIRST ttEvent 
     WHERE ttEvent.hObject     = ihParent
       AND ttEvent.hWidget     = ihWidget
     NO-ERROR.
IF AVAIL ttEvent THEN
  RETURN ttEvent.cAction.
ELSE
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEventMethod Procedure 
FUNCTION getEventMethod RETURNS CHARACTER
  ( INPUT ihParent AS HANDLE,
    INPUT ihWidget AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Parent might be anything (toolbar, buffer, popup..) 
           To distinguish corresoponding menu-items and buttons, use type (button / menu-item) - assumes button
------------------------------------------------------------------------------*/
FIND FIRST ttEvent 
     WHERE ttEvent.hObject     = ihParent
       AND ttEvent.hWidget     = ihWidget
     NO-ERROR.
IF AVAIL ttEvent THEN
  RETURN ttEvent.cMethod.
ELSE
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getEventWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEventWidget Procedure 
FUNCTION getEventWidget RETURNS HANDLE
  ( INPUT ihParent AS HANDLE,
    INPUT icAction AS CHAR,
    INPUT icType   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Parent might be anything (toolbar, buffer, popup..) 
           To distinguish corresoponding menu-items and buttons, use type (button / menu-item) - assumes button
------------------------------------------------------------------------------*/
FIND FIRST ttEvent 
     WHERE ttEvent.hObject     = ihParent
       AND ttEvent.cAction     = icAction
       AND ttEvent.cWidgetType = (IF icType = "" THEN "button" ELSE icType) 
     NO-ERROR.
IF AVAIL ttEvent THEN
  RETURN ttEvent.hWidget.
ELSE IF icType = "" THEN DO:
  FIND FIRST ttEvent 
       WHERE ttEvent.hObject     = ihParent
         AND ttEvent.cAction     = icAction
         AND ttEvent.cWidgetType = "menu-item" 
       NO-ERROR.
  IF AVAIL ttEvent THEN
    RETURN ttEvent.hWidget.
  ELSE
    RETURN ?. 
END.  
ELSE
  RETURN ?.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldHandle Procedure 
FUNCTION getFieldHandle RETURNS HANDLE
  ( INPUT ihFieldMap  AS HANDLE,
    INPUT icFieldName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Return handle to a widget in a fieldmap
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFields        AS CHAR NO-UNDO.
DEF VAR cWidgets       AS CHAR NO-UNDO.
DEF VAR hWidget        AS HANDLE NO-UNDO.
DEF VAR ix             AS INT NO-UNDO.

cFields = getAttribute(ihFieldMap,"ScreenUpdateFields").
IF CAN-DO(cFields,icFieldName) THEN DO:
  cWidgets = getAttribute(ihFieldMap,"ScreenUpdateWidgets").
  DO ix = 1 TO NUM-ENTRIES(cWidgets):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cWidgets)) NO-ERROR.
    IF VALID-HANDLE(hWidget) AND hWidget:NAME = icFieldName THEN
      RETURN hWidget.
  END.
END.
cFields = getAttribute(ihFieldMap,"ScreenDisplayFields").
IF CAN-DO(cFields,icFieldName) THEN DO:
  cWidgets = getAttribute(ihFieldMap,"ScreenDisplayWidgets").
  DO ix = 1 TO NUM-ENTRIES(cWidgets):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cWidgets)) NO-ERROR.
    IF VALID-HANDLE(hWidget) AND hWidget:NAME = icFieldName THEN
      RETURN hWidget.
  END.
END.
cFields = getAttribute(ihFieldMap,"ExtraUpdateFields").
IF CAN-DO(cFields,icFieldName) THEN DO:
  cWidgets = getAttribute(ihFieldMap,"ExtraUpdateWidgets").
  DO ix = 1 TO NUM-ENTRIES(cWidgets):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cWidgets)) NO-ERROR.
    IF VALID-HANDLE(hWidget) AND hWidget:NAME = icFieldName THEN
      RETURN hWidget.
  END.
END.

RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldMapWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldMapWidgets Procedure 
FUNCTION getFieldMapWidgets RETURNS CHARACTER
  ( INPUT ihFieldMap   AS HANDLE,
    INPUT icFieldNames AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Return a (list of) handle(s) to widgets in a fieldmap (viewer)
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cWidgets        AS CHAR NO-UNDO.
DEF VAR cReturnWidgets  AS CHAR NO-UNDO.
DEF VAR hWidget         AS HANDLE NO-UNDO.

cWidgets = getAttribute(ihFieldMap,"ScreenUpdateWidgets").
DO ix = 1 TO NUM-ENTRIES(cWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cWidgets)).
  IF CAN-DO(icFieldNames,hWidget:NAME) THEN
    cReturnWidgets = cReturnWidgets + ENTRY(ix,cWidgets) + ",".
END.

cWidgets = getAttribute(ihFieldMap,"ScreenDisplayWidgets").
DO ix = 1 TO NUM-ENTRIES(cWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cWidgets)).
  IF CAN-DO(icFieldNames,hWidget:NAME) THEN
    cReturnWidgets = cReturnWidgets + ENTRY(ix,cWidgets) + ",".
END.

cWidgets = getAttribute(ihFieldMap,"ExtraUpdateWidgets").
DO ix = 1 TO NUM-ENTRIES(cWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cWidgets)).
  IF CAN-DO(icFieldNames,hWidget:NAME) THEN
    cReturnWidgets = cReturnWidgets + ENTRY(ix,cWidgets) + ",".
END.

RETURN TRIM(cReturnWidgets,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsAttributeSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsAttributeSet Procedure
FUNCTION getIsAttributeSet RETURNS LOGICAL 
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
RETURN AVAIL ttAttribute.   

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getIsEventAllowed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsEventAllowed Procedure 
FUNCTION getIsEventAllowed RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE,
    INPUT icEvent  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Pass on call to ASlib to request access for an event
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cObjectName AS CHAR NO-UNDO.

FIND FIRST bSecObject
     WHERE bSecObject.hObject = ihObject  
     NO-ERROR.
IF AVAIL bSecObject THEN DO:
  IF bSecObject.cObjectType = "browse" THEN
    cObjectName = SUBSTR(bSecObject.cObjectName,1,R-INDEX(bSecObject.cObjectName,"_") - 1).
  ELSE cObjectName = bSecObject.cObjectName.

  RETURN DYNAMIC-FUNCTION("getActionPermission",bSecObject.hSourceProc:FILE-NAME,cObjectName,icEvent).
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkedObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLinkedObject Procedure 
FUNCTION getLinkedObject RETURNS HANDLE
  ( INPUT ihObject    AS HANDLE,
    INPUT icLinktype  AS CHAR,
    INPUT icDirection AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icDirection = "from" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ihObject
         AND ttObjectLink.cLinkType   = icLinkType 
       NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN
    RETURN ttObjectLink.hToObject.
  ELSE RETURN ?.
END.
ELSE IF icDirection = "to" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hToObject = ihObject
         AND ttObjectLink.cLinkType = icLinkType 
       NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN
    RETURN ttObjectLink.hFromObject.
  ELSE RETURN ?.
END.
ELSE DO:
  MESSAGE PROGRAM-NAME(1) SKIP(1)
          "Illegal request. Specify 'to' or 'from' as link-direction"
          VIEW-AS ALERT-BOX ERROR.
  RETURN ?.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinkedObjectByInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLinkedObjectByInfo Procedure 
FUNCTION getLinkedObjectByInfo RETURNS HANDLE
  ( INPUT ihObject    AS HANDLE,
    INPUT icLinktype  AS CHAR,
    INPUT icLinkInfo  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDirection AS CHAR NO-UNDO.
IF NUM-ENTRIES(icLinkInfo,"|") > 1 THEN
  ASSIGN cDirection = ENTRY(2,icLinkInfo,"|")
         icLinkInfo = ENTRY(1,icLinkInfo,"|").

IF cDirection = "to" THEN DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hToObject = ihObject
         AND ttObjectLink.cLinkType = icLinkType 
         AND ttObjectLink.cLinkInfo = icLinkInfo
       NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN
    RETURN ttObjectLink.hFromObject.
END.
ELSE DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hFromObject = ihObject
         AND ttObjectLink.cLinkType   = icLinkType 
         AND ttObjectLink.cLinkInfo   = icLinkInfo
       NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN
    RETURN ttObjectLink.hToObject.
END.
RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLocalDistinctRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLocalDistinctRows Procedure 
FUNCTION getLocalDistinctRows RETURNS LOGICAL
  ( INPUT ihOrgBrowse AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Run through a local temptable an extract the distinct values. Also add up
            the accumulated fields 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iCountDistinct      AS INT    NO-UNDO.
DEF VAR hDistinctFields     AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iNumDistinctFields  AS INT    NO-UNDO.
DEF VAR hOrgAccumFields     AS HANDLE NO-UNDO EXTENT 20.
DEF VAR hPreScanAccumFields AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iNumAccumFields     AS INT    NO-UNDO.
DEF VAR cDistinctValue      AS CHAR   NO-UNDO.
DEF VAR hPreScanDistinctCol AS HANDLE NO-UNDO.
DEF VAR hPreScanRowIdCol    AS HANDLE NO-UNDO.
DEF VAR cAccumFields        AS CHAR   NO-UNDO.
DEF VAR cDistinctFields     AS CHAR   NO-UNDO.
DEF VAR hOrgQuery           AS HANDLE NO-UNDO.
DEF VAR hOrgBuffer          AS HANDLE NO-UNDO.
DEF VAR httPreScan          AS HANDLE NO-UNDO.
DEF VAR hPreScanBuffer      AS HANDLE NO-UNDO.
DEF VAR hPreScanQuery       AS HANDLE NO-UNDO.
DEF VAR cOrgPrepareString   AS CHAR   NO-UNDO.

ASSIGN cAccumFields        = getAttribute(ihOrgBrowse,"accumfields")
       cDistinctFields     = getAttribute(ihOrgBrowse,"distinctcolumns")
       iNumDistinctFields  = NUM-ENTRIES(cDistinctFields)
       iNumAccumFields     = NUM-ENTRIES(cAccumFields)
       hOrgBuffer          = ihOrgBrowse:QUERY:GET-BUFFER-HANDLE(1)
       .
DO ix = 1 TO iNumDistinctFields:
  hDistinctFields[ix] = hOrgBuffer:BUFFER-FIELD(ENTRY(ix,cDistinctFields)).
END.

CREATE TEMP-TABLE httPreScan.

DO ix = 1 TO iNumAccumFields:
  hOrgAccumFields[ix] = hOrgBuffer:BUFFER-FIELD(ENTRY(ix,cAccumFields)).
  httPreScan:ADD-NEW-FIELD(hOrgAccumFields[ix]:NAME,hOrgAccumFields[ix]:DATA-TYPE).
END.
httPreScan:ADD-NEW-FIELD("rRelPreScan","ROWID"). 
httPreScan:ADD-NEW-FIELD("cDistinctValue","CHARACTER").  
httPreScan:ADD-NEW-INDEX("idxDistinct").
httPreScan:ADD-INDEX-FIELD("idxDistinct","cDistinctValue").
httPreScan:TEMP-TABLE-PREPARE("ttPreScan") NO-ERROR.

ASSIGN hPreScanBuffer      = httPreScan:DEFAULT-BUFFER-HANDLE
       hPreScanDistinctCol = hPreScanBuffer:BUFFER-FIELD("cDistinctValue")
       hPreScanRowIdCol    = hPreScanBuffer:BUFFER-FIELD("rRelPreScan").
DO ix = 1 TO iNumAccumFields:
  hPreScanAccumFields[ix] = hPreScanBuffer:BUFFER-FIELD(ENTRY(ix,cAccumFields)).
/*   cAccumBuffers[ix] = getAttribute(ihOrgBrowse,"fieldBuffer" + ENTRY(ix,cAccumFields)). */
END.

CREATE QUERY hOrgQuery.
hOrgQuery:SET-BUFFERS(hOrgBuffer).
cOrgPrepareString = ihOrgBrowse:QUERY:PREPARE-STRING.
/* Don't need sorting and definetly don't want error 141 caused by to long character sortfield: */
IF INDEX(cOrgPrepareString," BY ") > 0 THEN
  cOrgPrepareString = SUBSTR(cOrgPrepareString,1,INDEX(cOrgPrepareString," BY ")).
hOrgQuery:QUERY-PREPARE(cOrgPrepareString).
hOrgQuery:QUERY-OPEN().

hOrgQuery:GET-FIRST().

PRESCANLOOP:
REPEAT WHILE NOT hOrgQuery:QUERY-OFF-END:
  cDistinctValue = "".

  bOk = FALSE.
  DO ix = 1 TO iNumDistinctFields:
    IF hDistinctFields[ix]:BUFFER-VALUE NE ? THEN
      cDistinctValue = cDistinctValue + STRING(hDistinctFields[ix]:BUFFER-VALUE).
  END.

  bOk = hPreScanBuffer:FIND-FIRST("WHERE cDistinctValue = '" + cDistinctValue + "'") NO-ERROR.

  IF NOT bOK THEN DO:   /* If distinct value doesn't exist or there is no check on distinct */
    hPreScanBuffer:BUFFER-CREATE().
    ASSIGN hPreScanRowIdCol:BUFFER-VALUE = hOrgBuffer:ROWID
           hPreScanDistinctCol:BUFFER-VALUE = cDistinctValue
           iCountDistinct = iCountDistinct + 1.
  END.
  DO ix = 1 TO iNumAccumFields:
    hPreScanAccumFields[ix]:BUFFER-VALUE = hPreScanAccumFields[ix]:BUFFER-VALUE + hOrgAccumFields[ix]:BUFFER-VALUE.
  END.

  IF bOk THEN
    hOrgBuffer:BUFFER-DELETE().
  
  hOrgQuery:GET-NEXT().
END.

CREATE QUERY hPreScanQuery.
hPreScanQuery:SET-BUFFERS(hPreScanBuffer,hOrgBuffer).
hPreScanQuery:QUERY-PREPARE("FOR EACH " + hPreScanBuffer:NAME + 
                            ",FIRST " + hOrgBuffer:NAME + " WHERE ROWID(" + hOrgBuffer:NAME + ") = rRelPreScan").
hPreScanQuery:QUERY-OPEN().
hPreScanQuery:GET-FIRST().
REPEAT WHILE NOT hPreScanQuery:QUERY-OFF-END:
  DO ix = 1 TO iNumAccumFields:
    hOrgAccumFields[ix]:BUFFER-VALUE = hPreScanAccumFields[ix]:BUFFER-VALUE.
  END.
  hPreScanQuery:GET-NEXT().
END.

DELETE OBJECT hOrgQuery NO-ERROR.
DELETE OBJECT hPreScanQuery NO-ERROR.
DELETE OBJECT httPreScan NO-ERROR.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectActionList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectActionList Procedure 
FUNCTION getObjectActionList RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cActionList AS CHAR NO-UNDO.
FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
IF AVAIL bttObject THEN 
  FOR EACH ttEvent OF bttObject NO-LOCK
      WHERE ttEvent.cAction NE "no action":
    cActionList = cActionList + ttEvent.cAction + "|".
  END.

RETURN TRIM(cActionList,"|").  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectByAttribute Procedure 
FUNCTION getObjectByAttribute RETURNS HANDLE
  ( INPUT ihWindow    AS HANDLE,
    INPUT icName      AS CHAR,
    INPUT icValue     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH bttAttribute
    WHERE bttAttribute.cName  = icName
      AND bttAttribute.cValue = icValue,
    FIRST bttObject OF bttAttribute
          WHERE bttObject.hWindow = ihWindow:
  RETURN bttObject.hObject.
END.
RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectByEvent Procedure 
FUNCTION getObjectByEvent RETURNS HANDLE
  ( INPUT ihEventWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttEvent 
     WHERE ttEvent.hWidget = ihEventWidget
     NO-ERROR.
IF AVAIL ttEvent THEN
  RETURN ttEvent.hObject.
ELSE
  RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectByLinkInfo Procedure 
FUNCTION getObjectByLinkInfo RETURNS HANDLE
  ( INPUT ihObject    AS HANDLE,
    INPUT icLinkinfo  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hFromObject = ihObject
       AND ttObjectLink.cLinkInfo   = icLinkInfo 
     NO-LOCK NO-ERROR.
IF AVAIL ttObjectLink THEN
  RETURN ttObjectLink.hToObject.
ELSE DO:
  FIND FIRST ttObjectLink
       WHERE ttObjectLink.hToObject = ihObject
         AND ttObjectLink.cLinkInfo = icLinkInfo
       NO-LOCK NO-ERROR.
  IF AVAIL ttObjectLink THEN
    RETURN ttObjectLink.hFromObject.
  ELSE RETURN ?.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectByName Procedure 
FUNCTION getObjectByName RETURNS HANDLE
  ( INPUT ihWindow     AS HANDLE,
    INPUT icObjectName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST bttObject
     WHERE bttObject.hWindow     = ihWindow
       AND bttObject.cObjectName = icObjectName
     NO-ERROR.
IF AVAIL bttObject THEN RETURN bttObject.hObject.
ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectByNameAndType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectByNameAndType Procedure 
FUNCTION getObjectByNameAndType RETURNS HANDLE
  ( INPUT ihWindow     AS HANDLE,
    INPUT icObjectName AS CHAR,
    INPUT icObjectType AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
FIND FIRST bttObject
     WHERE bttObject.hWindow     = ihWindow
       AND bttObject.cObjectName = icObjectName
       AND bttObject.cObjectType = icObjectType
     NO-ERROR.
IF AVAIL bttObject THEN RETURN bttObject.hObject.
ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectContainer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectContainer Procedure 
FUNCTION getObjectContainer RETURNS HANDLE
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
IF AVAIL bttObject  THEN
  RETURN bttObject.hWindow.
ELSE
  RETURN ?.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectExists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectExists Procedure 
FUNCTION getObjectExists RETURNS LOGICAL
  (INPUT ihObject AS HANDLE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RETURN CAN-FIND(FIRST ttObject WHERE ttObject.hObject = ihObject).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectList Procedure 
FUNCTION getObjectList RETURNS CHARACTER
  ( INPUT ihWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cObjList AS CHAR NO-UNDO.
FOR EACH bttObject
    WHERE bttObject.hWindow = ihWindow:
  cObjList = cObjList + STRING(bttObject.hObject) + ",".
END.


RETURN TRIM(cObjList,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectName Procedure 
FUNCTION getObjectName RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
IF AVAIL bttObject THEN
  RETURN bttObject.cObjectName.
ELSE IF CAN-QUERY(ihObject,"NAME") THEN
  RETURN ihObject:NAME.
ELSE
  RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectSourceFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectSourceFile Procedure 
FUNCTION getObjectSourceFile RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
IF AVAIL bttObject AND VALID-HANDLE(bttObject.hSourceProc) THEN
  RETURN bttObject.hSourceProc:FILE-NAME.
ELSE
  RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectSourceFileHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectSourceFileHandle Procedure 
FUNCTION getObjectSourceFileHandle RETURNS HANDLE
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
IF AVAIL bttObject THEN
  RETURN bttObject.hSourceProc.
ELSE
  RETURN ?.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectState Procedure 
FUNCTION getObjectState RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
IF AVAIL bttObject THEN
  RETURN bttObject.cState.
ELSE
  RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectTableHandle Procedure 
FUNCTION getObjectTableHandle RETURNS CHARACTER
  ( INPUT icTTname AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* IF NOT SOURCE-PROCEDURE:FILE-NAME BEGINS "JBox" THEN RETURN "".  */

CASE icTTname:
  WHEN "ttObject"     THEN RETURN STRING(BUFFER ttObject:HANDLE:TABLE-HANDLE).
  WHEN "ttEvent"      THEN RETURN STRING(BUFFER ttEvent:HANDLE:TABLE-HANDLE).
  WHEN "ttAttribute"  THEN DO:
    EMPTY TEMP-TABLE ttAttributeCopy.
    FOR EACH ttAttribute:
      CREATE ttAttributeCopy.
      BUFFER-COPY ttAttribute TO ttAttributeCopy.
    END.
    RETURN STRING(BUFFER ttAttributeCopy:HANDLE:TABLE-HANDLE).  
  END. 
  WHEN "ttObjectLink" THEN RETURN STRING(BUFFER ttObjectLink:HANDLE:TABLE-HANDLE).
END CASE.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectType Procedure 
FUNCTION getObjectType RETURNS CHARACTER
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
IF AVAIL bttObject THEN
  RETURN bttObject.cObjectType.
ELSE
  RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPageObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPageObject Procedure 
FUNCTION getPageObject RETURNS HANDLE
  ( INPUT ihContainer AS HANDLE,
    INPUT icPage      AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttObjectLink
     WHERE ttObjectLink.hToObject = ihContainer
       AND ttObjectLink.cLinkInfo = "page" + icPage 
     NO-LOCK NO-ERROR.
IF AVAIL ttObjectLink THEN
  RETURN ttObjectLink.hFromObject.
ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentLinkFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentLinkFields Procedure 
FUNCTION getParentLinkFields RETURNS CHARACTER
  ( INPUT ihChild AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Get the current values of the link fields in a parent record 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hPartner       AS HANDLE NO-UNDO.

FIND FIRST bttObjectLink
     WHERE bttObjectLink.hFromObject = ihChild
       AND bttObjectLink.cLinkType   = "parent"
     NO-ERROR.
IF NOT AVAIL bttObjectLink THEN DO:
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = ihChild
         AND bttObjectLink.cLinkType   = "onetoone"
       NO-ERROR.
  IF AVAIL bttObjectLink THEN DO:
    hPartner = bttObjectLink.hToObject.
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = hPartner
           AND bttObjectLink.cLinkType   = "parent"
         NO-ERROR.
    IF AVAIL bttObjectLink THEN
      RETURN getAttribute(hPartner,"childlinkfields").
  END.
END.
ELSE RETURN getAttribute(ihChild,"childlinkfields").

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentLinkValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentLinkValues Procedure 
FUNCTION getParentLinkValues RETURNS CHARACTER
  ( INPUT ihChild AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Get the current values of the link fields in a parent record 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cParentValues  AS CHAR   NO-UNDO.
DEF VAR cParentFields  AS CHAR   NO-UNDO.
DEF VAR hBuffer        AS HANDLE NO-UNDO.
DEF VAR hPartner       AS HANDLE NO-UNDO.

FIND FIRST bttObjectLink
     WHERE bttObjectLink.hFromObject = ihChild
       AND bttObjectLink.cLinkType   = "parent"
     NO-ERROR.
IF NOT AVAIL bttObjectLink THEN DO:
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = ihChild
         AND bttObjectLink.cLinkType   = "onetoone"
       NO-ERROR.
  IF AVAIL bttObjectLink THEN DO:
    hPartner = bttObjectLink.hToObject.
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = hPartner
           AND bttObjectLink.cLinkType   = "parent"
         NO-ERROR.
    IF AVAIL bttObjectLink THEN
      cParentFields = getAttribute(hPartner,"parentlinkfields").
  END.
END.
ELSE cParentFields = getAttribute(ihChild,"parentlinkfields").

IF cParentFields NE "parentLink" AND AVAIL bttObjectLink THEN DO:
  IF bttObjectLink.hToObject:TYPE = "browse" THEN
    hBuffer = bttObjectLink.hToObject:QUERY:GET-BUFFER-HANDLE(1).
  ELSE IF bttObjectLink.hToObject:TYPE = "query" THEN
    hBuffer = bttObjectLink.hToObject:GET-BUFFER-HANDLE(1).
  ELSE RETURN "". /* <- should never happen.. */

  IF NOT hBuffer:AVAIL THEN DO:
    IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN
      MESSAGE "Unable to retrieve parent link values for object: " bttObjectLink.hToObject:NAME SKIP
              "Record is not available" SKIP(1)
              "THIS MESSAGE IS ONLY SHOWN WHEN QUERY-LOGGING IS ON"
               VIEW-AS ALERT-BOX ERROR.
    RETURN "".
  END.

  DO ix = 1 TO NUM-ENTRIES(cParentFields):
    cParentValues = cParentValues + STRING(hBuffer:BUFFER-FIELD(ENTRY(ix,cParentFields)):BUFFER-VALUE) + CHR(1).
  END.
END.

IF cParentValues NE "" THEN
  RETURN SUBSTR(cParentValues,1,LENGTH(cParentValues) - 1).
ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRealSortField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRealSortField Procedure 
FUNCTION getRealSortField RETURNS CHARACTER
  ( INPUT ihQueryObject     AS HANDLE,
    INPUT icLocalFieldName  AS CHAR,
    INPUT icTarget          AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Get the real sortfield either for local or db purpose 
    Notes: Takes in account the mapping of the sort field (typical: calc field for time mapped to integer time)
           Re-mapping of duplicate fields only works for the first instance 
------------------------------------------------------------------------------*/
DEF VAR cCalcFields AS CHAR NO-UNDO.
DEF VAR cSortMap    AS CHAR NO-UNDO.
DEF VAR cMapTo      AS CHAR NO-UNDO.
DEF VAR iy          AS INT  NO-UNDO.
DEF VAR cReturn     AS CHAR NO-UNDO.

ASSIGN cCalcFields      = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allcalcfields")
       cSortMap         = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"sortmap")
       icLocalFieldName = TRIM(icLocalFieldName)
       .

DO iy = 1 TO NUM-ENTRIES(cSortMap):
  IF ENTRY(1,ENTRY(iy,cSortMap),";") = icLocalFieldName THEN
    RETURN ENTRY(2,ENTRY(iy,cSortMap),";").
END.

IF icTarget = "db" THEN DO:
  cReturn = IF NOT CAN-DO(cCalcFields,icLocalFieldName) THEN
              getAttribute(ihQueryObject,"fieldbuffer" + icLocalFieldName) + "." 
            + getAttribute(ihQueryObject,"orgdbfield" + icLocalFieldName)
            ELSE icLocalFieldName.
  IF cReturn = "." THEN DO:
    cReturn = ENTRY(1,getAttribute(ihQueryObject,"buffersAndFields")) + "." + icLocalFieldName.

    IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN 
      MESSAGE PROGRAM-NAME(1) SKIP
              "Could not retrieve database sortfield. Probably caused by missing field specifications for the query (browse) object." SKIP
              "Assumes table name to be first entry in query specification." SKIP(1)
              "THIS MESSAGE IS ONLY SHOWN WHEN QUERY-LOGGING IS ON"
              VIEW-AS ALERT-BOX WARNING.
  END.

  RETURN cReturn.
END.
ELSE
  RETURN icLocalFieldName.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSecDisabledActions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSecDisabledActions Procedure 
FUNCTION getSecDisabledActions RETURNS CHARACTER
  ( INPUT icProgramFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Get the list of disabled actions. 
    Notes: If no program file is given the global list is returned 
------------------------------------------------------------------------------*/
IF icProgramFile = "" THEN
  RETURN cGlobSecDisabledActions.
ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStrippedSortLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStrippedSortLabel Procedure 
FUNCTION getStrippedSortLabel RETURNS CHARACTER
  ( INPUT ihColumn AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iMarkerLength AS INT NO-UNDO.
iMarkerLength = LENGTH(cMarkAsc).

IF ihColumn:LABEL NE "" THEN DO:
  IF LENGTH(ihColumn:LABEL) > iMarkerLength - 1 AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - (iMarkerLength - 1)) = cMarkAsc THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - iMarkerLength).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkAsc + "1" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkAsc + "2" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkAsc + "3" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkAsc + "4" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength - 1 AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - (iMarkerLength - 1)) = cMarkDesc THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - iMarkerLength).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkDesc + "1" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkDesc + "2" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkDesc + "3" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE IF LENGTH(ihColumn:LABEL) > iMarkerLength AND SUBSTR(ihColumn:LABEL,LENGTH(ihColumn:LABEL) - iMarkerLength) = cMarkDesc + "4" THEN
    RETURN SUBSTR(ihColumn:LABEL,1,LENGTH(ihColumn:LABEL) - (iMarkerLength + 1)).
  ELSE RETURN ihColumn:LABEL.
END.

RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolbarButtonHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolbarButtonHandle Procedure 
FUNCTION getToolbarButtonHandle RETURNS HANDLE
  ( INPUT ihToolBar    AS HANDLE,
    INPUT icAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: handle for toolbar button
------------------------------------------------------------------------------*/
FIND FIRST ttEvent 
     WHERE ttEvent.hObject = ihToolBar
       AND ttEvent.cName   = icAction
     NO-ERROR.
IF NOT AVAILABLE ttEvent THEN 
  FIND FIRST ttEvent 
       WHERE ttEvent.hObject = ihToolBar
         AND ttEvent.cName   = "choose"
         AND ttEvent.cAction = icAction
       NO-ERROR.
IF NOT AVAILABLE ttEvent THEN 
  FIND FIRST ttEvent 
       WHERE ttEvent.hObject = ihToolBar
         AND ttEvent.cAction = icAction
       NO-ERROR.

IF AVAIL ttEvent THEN RETURN ttEvent.hWidget.
ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolBarHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolBarHandles Procedure 
FUNCTION getToolBarHandles RETURNS CHARACTER
  ( INPUT ihToolBar              AS HANDLE,
    INPUT icWidgetNameOrTypeList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Retrieve one or many handles to buttons and/or menu items defined for a toolbar object
           (or any widget being the parent when defining a menu-band) 
    Notes: The widget names generated by JukeBox for buttons contains the string-value of their handles to 
           make sure that button names are unique for the container.
           To retrieve the handles by name hence makes a challenge.
    Use of icWidgetNameOrTypeList: 
       - To retrieve handles to all buttons: ""
       - To retrieve all handles for a widget type: ",widget-type" (ie "menu-item")
       - to retrieve handles matching a name (handles for menu-item and button are returned): "*action*"
       - To retrieve a spesific menu-item: "action" (menu items are named equal to their action)
------------------------------------------------------------------------------*/
DEF VAR cHandles AS CHAR NO-UNDO.

FOR EACH ttEvent 
    WHERE ttEvent.hObject = ihToolBar:
  IF icWidgetNameOrTypeList NE "" AND ttEvent.hWidget:NAME = icWidgetNameOrTypeList THEN
    RETURN STRING(ttEvent.hWidget).
  ELSE IF INDEX(icWidgetNameOrTypeList,"*") > 0 AND ttEvent.hWidget:NAME MATCHES icWidgetNameOrTypeList THEN 
    cHandles = cHandles + STRING(ttEvent.hWidget) + ",".
  ELSE IF NUM-ENTRIES(icWidgetNameOrTypeList) > 1 AND CAN-DO(icWidgetNameOrTypeList,ttEvent.cWidgetType) THEN
    cHandles = cHandles + STRING(ttEvent.hWidget) + ",".
  ELSE IF ttEvent.hWidget:TYPE = "BUTTON" AND INDEX(icWidgetNameOrTypeList,"*") = 0 THEN
    cHandles = cHandles + STRING(ttEvent.hWidget) + ",".
END.
RETURN TRIM(cHandles,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolBarNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolBarNames Procedure 
FUNCTION getToolBarNames RETURNS CHARACTER
  ( INPUT ihToolBar    AS HANDLE,
    INPUT icWidgetName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNames AS CHAR NO-UNDO.

FOR EACH ttEvent 
    WHERE ttEvent.hObject = ihToolBar:
  IF icWidgetName NE "" AND ttEvent.hWidget:NAME = icWidgetName THEN
    RETURN ttEvent.hWidget:NAME.
  ELSE IF INDEX(icWidgetName,"*") > 0 AND ttEvent.hWidget:NAME MATCHES icWidgetName THEN 
    cNames = cNames + ttEvent.hWidget:NAME + ",".
  ELSE IF ttEvent.hWidget:TYPE = "BUTTON" AND INDEX(icWidgetName,"*") = 0 THEN
    cNames = cNames + ttEvent.hWidget:NAME + ",".
END.
RETURN TRIM(cNames,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolbarState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolbarState Procedure 
FUNCTION getToolbarState RETURNS CHARACTER
  ( INPUT ihToolbar  AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST bttObject WHERE bttObject.hObject = ihToolbar NO-ERROR.
IF AVAIL bttObject THEN
  RETURN bttObject.cState.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWidgetSourceFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWidgetSourceFile Procedure 
FUNCTION getWidgetSourceFile RETURNS CHARACTER
  ( INPUT ihWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSourceFile AS CHAR NO-UNDO.
FOR FIRST bttEvent
    WHERE bttEvent.hWidget = ihWidget
   ,FIRST bttObject OF bttEvent
          WHERE VALID-HANDLE(bttObject.hSourceProc)
  :
  cSourceFile = bttObject.hSourceProc:FILE-NAME.
/*   IF cSourceFile MATCHES "*.ab" THEN                                                  */
/*     cSourceFile = ENTRY(2,SUBSTR(cSourceFile,1,LENGTH(cSourceFile) - 3),"_") + ".w".  */
  RETURN cSourceFile.
END.
RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitDynFilter Procedure 
FUNCTION InitDynFilter RETURNS LOGICAL
  ( INPUT ihQueryObject   AS HANDLE,
    INPUT icFieldList     AS CHAR,
    INPUT icOperatorList  AS CHAR,
    INPUT icValueList     AS CHAR,
    INPUT icAction        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set initial values for dynamic filter. Returns TRUE if rows were retrieved
    Notes: Cannot do "advanced", ie grouping with and/or
           A toolbar with filter button must be linked to the browse
           icAction: "try": Execute filter but clears the filter if no result is retrieved
------------------------------------------------------------------------------*/
DEF VAR cFilterFields   AS CHAR   NO-UNDO.
DEF VAR iy              AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hFilter         AS HANDLE NO-UNDO.
DEF VAR hFilterButton   AS HANDLE NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.
DEF VAR hNewBuff        AS HANDLE NO-UNDO.
DEF VAR cOperator       AS CHAR   NO-UNDO.
DEF VAR cInUseFieldList AS CHAR   NO-UNDO.
DEF VAR cDuplicateList  AS CHAR   NO-UNDO.
DEF VAR cDuplOperList   AS CHAR   NO-UNDO.
DEF VAR cDuplValueList  AS CHAR   NO-UNDO.
DEF VAR iGroupCount     AS INT    NO-UNDO.

DEF VAR cExtraFilter    AS CHAR   NO-UNDO.
DEF VAR cExtraBuffers   AS CHAR   NO-UNDO.
DEF VAR cExtraFields    AS CHAR   NO-UNDO.
DEF VAR cExtraFieldDef  AS CHAR   NO-UNDO.
DEF VAR cExtraFieldLab  AS CHAR   NO-UNDO.
DEF VAR cExtraFieldForm AS CHAR   NO-UNDO.
DEF VAR cExtraFieldType AS CHAR   NO-UNDO.
DEF VAR cExtraFieldAttr AS CHAR   NO-UNDO.
DEF VAR cExtraDbFldAttr AS CHAR   NO-UNDO.
DEF VAR bDynamicsFilter AS LOG    NO-UNDO.
DEF VAR hFilterWin      AS HANDLE NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR hCurrObject     AS HANDLE NO-UNDO.
DEF VAR cValue          AS CHAR   NO-UNDO. 
DEF VAR bValue          AS LOG    NO-UNDO.
DEF VAR cFormat         AS CHAR   NO-UNDO.

IF AVAIL ttObject THEN
  hCurrObject = ttObject.hObject.
ELSE
  hCurrObject = ihQueryObject.

hToolbar = DYNAMIC-FUNCTION("getLinkedObject",ihQueryObject,"toolbar","from").
IF NOT VALID-HANDLE(hToolbar) AND NOT icAction MATCHES "*static*" THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Error when trying to initialize filter." SKIP
          "Query object is not linked to toolbar"
          VIEW-AS ALERT-BOX.
  RETURN FALSE.
END.
hFilterButton = WIDGET-HANDLE(getAttribute(hToolbar,"buttonFilter")) NO-ERROR.
IF NOT VALID-HANDLE(hFilterButton) AND NOT icAction MATCHES "*static*" THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Error when trying to initialize filter." SKIP
          "Toolbar linked to the query object must contain a filter button"
          VIEW-AS ALERT-BOX.
  RETURN FALSE.
END.

IF getAttribute(hToolbar,"filterwindow") = "JBoxDynamicsFilter.w" OR
   (getAttribute(hToolbar,"filterwindow") = "" AND getAttribute(SESSION,"filterwindow") = "JBoxDynamicsFilter.w") THEN
  bDynamicsFilter = YES.

msetAttribute(ihQueryObject,"filterValue_","").
msetAttribute(ihQueryObject,"fieldOperator_","").
msetAttribute(ihQueryObject,"fieldGroup_","").
msetAttribute(ihQueryObject,"groupOperator_","").
msetAttribute(ihQueryObject,"hiddenGroupOperator_","").
msetAttribute(ihQueryObject,"operatorInUse_","").
msetAttribute(ihQueryObject,"*rowid","").

DO ix = 1 TO NUM-ENTRIES(icFieldList):  
  cOperator = ENTRY(ix,icOperatorList).
/*   setAttribute(ihQueryObject,"filtervalue_" + (IF CAN-DO(cInUseFieldList,ENTRY(ix,icFieldList)) THEN "1_" */
/*                                                ELSE "")                                                   */
/*                                             + ENTRY(ix,icFieldList),ENTRY(ix,icValueList,"|")).           */
  CASE cOperator:
    WHEN "GT" THEN cOperator = ">".
    WHEN "GE" THEN cOperator = ">=".
    WHEN "LT" THEN cOperator = "<".
    WHEN "LE" THEN cOperator = "<=".
    WHEN "NE" THEN cOperator = "<>".
    WHEN "EQ" THEN cOperator = "=".
  END CASE.
  IF CAN-DO(cInUseFieldList,ENTRY(ix,icFieldList)) THEN DO:
    iGroupCount = iGroupCount + 1.
    setAttribute(ihQueryObject,"OperatorInUse_" + STRING(iGroupCount) + "_" + ENTRY(ix,icFieldList),cOperator).
    setAttribute(ihQueryObject,"FieldGroup_" + STRING(iGroupCount) + "_" + ENTRY(ix,icFieldList),"0"). /* org: 1 */
    setAttribute(ihQueryObject,"FieldOperator_" + STRING(iGroupCount) + "_" + ENTRY(ix,icFieldList),
                 (IF cOperator = "=" THEN "OR" ELSE "AND")).
    setAttribute(ihQueryObject,"FilterValue_" + STRING(iGroupCount) + "_" + ENTRY(ix,icFieldList),ENTRY(ix,icValueList,"|")).
  END.
  ELSE DO:
    setAttribute(ihQueryObject,"OperatorInUse_" + ENTRY(ix,icFieldList),cOperator).
    setAttribute(ihQueryObject,"FilterValue_" + ENTRY(ix,icFieldList),ENTRY(ix,icValueList,"|")).
    iGroupCount = 0.
  END.

  cInUseFieldList = cInUseFieldList + (IF cInUseFieldList NE "" THEN "," ELSE "") + ENTRY(ix,icFieldList).
END.

hFilterWin = WIDGET-HANDLE(getAttribute(hToolbar,"filterhandle")) NO-ERROR.
IF VALID-HANDLE(hFilterWin) AND CAN-DO(hFilterWin:INTERNAL-ENTRIES,"setFilter") THEN DO:
  RUN SetFilter IN hFilterWin (OUTPUT bOk) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND bOk THEN RETURN YES.
  ELSE IF ERROR-STATUS:ERROR THEN DO:
    RUN SetFilter IN hFilterWin NO-ERROR.
    RETURN YES.
  END.
END.


DYNAMIC-FUNCTION("setObjectSourceProc",SOURCE-PROCEDURE).
hQuery = DYNAMIC-FUNCTION("NewQuery",1,"",
                           "temp-table"
                         + ";+BufferName|CHARACTER"
                         + ";+GroupOperator|CHARACTER"
                         + ";+FieldGroup|INTEGER"
                         + ";+FieldOperator|CHARACTER"
                         + ";+ColumnLabel|CHARACTER"
                         + ";+Operator|CHARACTER" 
                         + ";+ColumnValue|CHARACTER"
                         + ";+!ColumnName|CHARACTER" 
                         + ";+!DataType|CHARACTER" 
                         + ";+!Format|CHARACTER" 
                         + ";+!Seq|INTEGER" 
                         + ";+!HiddenGroupOperator|CHARACTER" 
                         + ";+!DuplicateRow|LOGICAL"
                         + ";+!CharValueTrue|CHARACTER"
                         + ";+!CharValueFalse|CHARACTER"
                         + (IF bDynamicsFilter THEN ";!NumInUse|INTEGER" ELSE "")
                           ,
                           "where false",
                           "").

hBuffer = hQuery:GET-BUFFER-HANDLE(1).
CREATE BUFFER hNewBuff FOR TABLE hBuffer.

cFilterFields = getAttribute(ihQueryObject,"allviewfields")      
              + (IF getAttribute(ihQueryObject,"nodisplayfields") NE "" THEN
                  "," + getAttribute(ihQueryObject,"nodisplayfields")
                 ELSE "").

cExtraFilter = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"extraFilterFields").
DO ix = 1 TO NUM-ENTRIES(cExtraFilter):
  cExtraDbFldAttr = "".
  IF NUM-ENTRIES(ENTRY(ix,cExtraFilter),".") NE 2 THEN DO:
    MESSAGE "Error in definition of extra filter fields. Must be defined as <table>.<field>"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
    LEAVE.
  END.
  cExtraFieldDef = ENTRY(2,ENTRY(ix,cExtraFilter),".").
  IF NUM-ENTRIES(cExtraFieldDef,"|") > 1 THEN DO:
    cExtraFieldLab = ENTRY(2,cExtraFieldDef,"|").
    IF cExtraFieldLab = "" THEN DO:
      MESSAGE "Error in definition of extra filter fields. No label defined for field: " cExtraFieldDef SKIP
              "Complete attr.setting for entry in extraFilterFields with label override: <table>.<field>|<label>"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
      LEAVE.
    END.
  END.
  ELSE cExtraFieldLab = "".
  IF NUM-ENTRIES(cExtraFieldDef,"|") > 2 THEN DO:
    cExtraFieldForm = ENTRY(3,cExtraFieldDef,"|").
    IF cExtraFieldForm = "" THEN DO:
      MESSAGE "Error in definition of extra filter fields. No format defined for field: " cExtraFieldDef SKIP
              "Complete attr.setting for entry in extraFilterFields with label and format override: <table>.<field>|<label>|<format>"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
      LEAVE.
    END.
  END.
  ELSE cExtraFieldForm = "".
  IF NUM-ENTRIES(cExtraFieldDef,"|") > 3 THEN DO:
    cExtraFieldType = ENTRY(4,cExtraFieldDef,"|").
    IF cExtraFieldType = "" THEN DO:
      MESSAGE "Error in definition of extra filter fields. No type defined for field: " cExtraFieldDef SKIP
              "Complete attr.setting for entry in extraFilterFields with label, format and type override: <table>.<field>|<label>|<format>|<type>"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
      LEAVE.
    END.
    ELSE cExtraDbFldAttr = cExtraFieldType + "|" + cExtraFieldForm + "|" + cExtraFieldLab.
  END.
  ELSE cExtraFieldType = "".

  cExtraFieldDef = ENTRY(1,cExtraFieldDef,"|").

  DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"fieldBuffer" + cExtraFieldDef,ENTRY(1,ENTRY(ix,cExtraFilter),".")).
  DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"orgdbfield" + cExtraFieldDef,cExtraFieldDef).
  cExtraFields    = cExtraFields + (IF cExtraFields NE "" THEN "," ELSE "") + cExtraFieldDef.
  IF cExtraDbFldAttr = "" THEN DO:
    cExtraDbFldAttr = DYNAMIC-FUNCTION("getFieldList","_file;,_Field;_data-type;_format;_label" 
                       ,"WHERE _file-name = '" + ENTRY(1,ENTRY(ix,cExtraFilter),".") + "'"
                      + ",FIRST _Field OF _File NO-LOCK WHERE _field-name = '" + cExtraFieldDef + "'").

    IF cExtraDbFldAttr = ? THEN
      cExtraDbFldAttr = DYNAMIC-FUNCTION("getFieldList","_file;,_Field;_data-type;_format" 
                     ,"WHERE _file-name = '" + ENTRY(1,ENTRY(ix,cExtraFilter),".") + "'"
                    + ",FIRST _Field OF _File NO-LOCK WHERE _field-name = '" + cExtraFieldDef + "'").
  
    IF cExtraDbFldAttr = "" THEN
      MESSAGE "Could not obtain information from database on extra filter field " cExtraFieldDef SKIP
              "Either the db/field names are wrong or it could be that the field comes from another db" SKIP
              "You could try to add type as last parameter to the definition:" SKIP
              "<table>.<field>|<label>|<format>|<type>"
              VIEW-AS ALERT-BOX WARNING.
    ELSE DO:
      IF cExtraFieldForm NE "" AND NUM-ENTRIES(cExtraDbFldAttr,"|") > 1 THEN
        ENTRY(2,cExtraDbFldAttr,"|") = cExtraFieldForm.
      IF cExtraFieldLab NE "" AND NUM-ENTRIES(cExtraDbFldAttr,"|") > 2 THEN
        ENTRY(3,cExtraDbFldAttr,"|") = cExtraFieldLab.
      ELSE IF cExtraFieldLab NE "" THEN
        cExtraDbFldAttr = cExtraDbFldAttr + "|" + cExtraFieldLab.
      ELSE 
        cExtraDbFldAttr = cExtraDbFldAttr + "|".
    END.
  END.


  IF cExtraDbFldAttr NE "" THEN DO:
    ASSIGN cFilterFields = cFilterFields + (IF cFilterFields NE "" THEN "," ELSE "") + cExtraFieldDef
           cExtraFieldAttr = cExtraFieldAttr + (IF cExtraFieldAttr NE "" THEN "," ELSE "") + cExtraDbFldAttr.
    IF NOT CAN-DO(cExtraBuffers,ENTRY(1,ENTRY(ix,cExtraFilter),".")) THEN
      cExtraBuffers = cExtraBuffers + (IF cExtraBuffers NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,cExtraFilter),".").
  END.
END.
IF cExtraBuffers NE "" THEN 
  DYNAMIC-FUNCTION("setAttribute",ihQueryObject,"extraFilterBuffers",cExtraBuffers).

DO ix = 1 TO NUM-ENTRIES(cFilterFields):
  hBuffer:BUFFER-CREATE().
  ASSIGN hBuffer:BUFFER-FIELD("BufferName"):BUFFER-VALUE = 
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"uselocaldata") = "yes" THEN
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"basetable")
                 ELSE DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"fieldbuffer" + ENTRY(ix,cFilterFields))
         hBuffer:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE =            
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GroupOperator_" + ENTRY(ix,cFilterFields)) NE "" THEN
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"GroupOperator_" + ENTRY(ix,cFilterFields))
                 ELSE ""
         hBuffer:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldGroup_" + ENTRY(ix,cFilterFields)) NE "" THEN
                   INT(DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldGroup_" + ENTRY(ix,cFilterFields)))
                 ELSE 0
         hBuffer:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE =            
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + ENTRY(ix,cFilterFields)) NE "" THEN
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"FieldOperator_" + ENTRY(ix,cFilterFields))
                 ELSE "AND"
         hBuffer:BUFFER-FIELD("ColumnName"):BUFFER-VALUE = ENTRY(ix,cFilterFields)
         hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = 
                 DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"operatorInUse_" + ENTRY(ix,cFilterFields))           
         hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = 
                 DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + ENTRY(ix,cFilterFields))
         hBuffer:BUFFER-FIELD("DataType"):BUFFER-VALUE =
                 IF CAN-DO(cExtraFields,ENTRY(ix,cFilterFields)) THEN
                   ENTRY(1,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtraFields),cExtraFieldAttr),"|")
                 ELSE IF ihQueryObject:TYPE = "browse" THEN
                   ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):DATA-TYPE
                 ELSE
                   ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):DATA-TYPE
         hBuffer:BUFFER-FIELD("Format"):BUFFER-VALUE =
                 IF CAN-DO(cExtraFields,ENTRY(ix,cFilterFields)) THEN
                   ENTRY(2,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtraFields),cExtraFieldAttr),"|")
                 ELSE IF ihQueryObject:TYPE = "browse" THEN
                   ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):FORMAT
                 ELSE
                   ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):FORMAT
         hBuffer:BUFFER-FIELD("ColumnLabel"):BUFFER-VALUE =
                 IF CAN-DO(cExtraFields,ENTRY(ix,cFilterFields)) THEN
                   ENTRY(3,ENTRY(LOOKUP(ENTRY(ix,cFilterFields),cExtraFields),cExtraFieldAttr),"|")
                 ELSE IF ihQueryObject:TYPE = "browse" THEN
                   ihQueryObject:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):LABEL
                 ELSE
                   ihQueryObject:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ENTRY(ix,cFilterFields)):LABEL
         hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE = ix + 100 * ix
         hBuffer:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE =            
                 IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"HiddenGroupOperator_" + ENTRY(ix,cFilterFields)) NE "" THEN
                   DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"HiddenGroupOperator_" + ENTRY(ix,cFilterFields))
                 ELSE ""
         .
         
  IF hBuffer::DataType = "LOGICAL" THEN DO:
    cValue = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filterDropdownValuelist_" + ENTRY(ix,cFilterFields)).
    IF cValue NE "" THEN
      ASSIGN hBuffer::CharValueTrue  = ENTRY(NUM-ENTRIES(cValue,"|") - 2,cValue,"|")
             hBuffer::CharValueFalse = ENTRY(NUM-ENTRIES(cValue,"|"),cValue,"|")
             . 
    ELSE IF NUM-ENTRIES(cFormat,"/") = 2 AND ENTRY(1,cFormat,"/") NE "" AND ENTRY(2,cFormat,"/") NE "" THEN        
      ASSIGN hBuffer::CharValueTrue  = ENTRY(1,cFormat,"/")
             hBuffer::CharValueFalse = ENTRY(2,cFormat,"/")
             . 
    ELSE         
      ASSIGN hBuffer::CharValueTrue  = "yes"
             hBuffer::CharValueFalse = "no"
             . 
    IF hBuffer::ColumnValue NE "" THEN DO:        
      bValue = LOGICAL(hBuffer::ColumnValue) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND bValue NE ? THEN DO:
        IF bValue THEN
          hBuffer::ColumnValue = hBuffer::CharValueTrue.
        ELSE         
          hBuffer::ColumnValue = hBuffer::CharValueFalse.
      END.
    END.
  END.  
         
         
  DO iy = 1 TO 100:
    IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"OperatorInUse_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields)) NE "" 
       THEN DO:
      hNewBuff:BUFFER-CREATE().
      hNewBuff:BUFFER-COPY(hBuffer).
      ASSIGN hNewBuff:BUFFER-FIELD("GroupOperator"):BUFFER-VALUE = 
               getAttribute(ihQueryObject,"GroupOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
             hNewBuff:BUFFER-FIELD("FieldGroup"):BUFFER-VALUE = 
               getAttribute(ihQueryObject,"FieldGroup_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
             hNewBuff:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE = 
               getAttribute(ihQueryObject,"FieldOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
             hNewBuff:BUFFER-FIELD("Operator"):BUFFER-VALUE = 
               getAttribute(ihQueryObject,"OperatorInUse_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
             hNewBuff:BUFFER-FIELD("FieldOperator"):BUFFER-VALUE = 
               getAttribute(ihQueryObject,"FieldOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
             hNewBuff:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = 
               getAttribute(ihQueryObject,"filtervalue_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
             hNewBuff:BUFFER-FIELD("Seq"):BUFFER-VALUE = ix + 100 * ix + iy
             hNewBuff:BUFFER-FIELD("HiddenGroupOperator"):BUFFER-VALUE = 
               getAttribute(ihQueryObject,"HiddenGroupOperator_" + STRING(iy) + "_" + ENTRY(ix,cFilterFields))
             .
      IF bDynamicsFilter THEN
        hNewBuff:BUFFER-FIELD("NumInUse"):BUFFER-VALUE = iy.
    END.
    ELSE LEAVE.
  END.

  IF bDynamicsFilter AND CAN-DO("begins,matches",hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE) THEN DO:
    hNewBuff:BUFFER-CREATE().
    hNewBuff:BUFFER-COPY(hBuffer).
    ASSIGN hNewBuff:BUFFER-FIELD("NumInUse"):BUFFER-VALUE = 2
           hNewBuff:BUFFER-FIELD("Seq"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("Seq"):BUFFER-VALUE + 2
           hBuffer:BUFFER-FIELD("ColumnValue"):BUFFER-VALUE = ""
           hBuffer:BUFFER-FIELD("Operator"):BUFFER-VALUE = ""
           .
  END.
END.

DYNAMIC-FUNCTION("setObjectSourceProc",SOURCE-PROCEDURE).
hFilter = DYNAMIC-FUNCTION("NewDynFilter",
                           hQuery,
                           hFilterButton,                                                             
                           "").

IF VALID-HANDLE(hToolbar) THEN
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFilter).
DYNAMIC-FUNCTION("CreateObjectLink",ihQueryObject,hFilter).
  
DELETE OBJECT hNewBuff.


IF icAction MATCHES "*noexec*" THEN setAttribute(ihQueryObject,"executeDynFilter","NO").

DYNAMIC-FUNCTION("setCurrentObject",hFilter).

RUN StartDynFilterRecord.

IF icAction MATCHES "*try*" AND getAttribute(ihQueryObject,"firstrowid") = "" THEN 
  DYNAMIC-FUNCTION("ClearQueryFilter",ihQueryObject).

DYNAMIC-FUNCTION("DeleteObject",hQuery).
DYNAMIC-FUNCTION("DeleteObject",hFilter).
DELETE OBJECT hBuffer:TABLE-HANDLE NO-ERROR.
DELETE OBJECT hBuffer NO-ERROR.

IF VALID-HANDLE(hCurrObject) THEN 
  DYNAMIC-FUNCTION("setCurrentObject",hCurrObject).

RETURN getAttribute(ihQueryObject,"firstrowid") NE "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitToolbarWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitToolbarWidgets Procedure 
FUNCTION InitToolbarWidgets RETURNS LOGICAL
  ( INPUT ihToolbar      AS HANDLE,
    INPUT ihLinkedObject AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Set the initial buttons for the toolbar based on the settigs for a linked object
           Ex: The linked browse has a active filter and the filter button should reflect that 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget   AS HANDLE NO-UNDO.
DEF VAR cButton   AS CHAR   NO-UNDO.

FIND ttObject WHERE ttObject.hObject = ihLinkedObject NO-ERROR.
IF NOT AVAIL ttObject THEN RETURN NO.

CASE ttObject.cObjectType:
  WHEN "browse" THEN DO:
    hWidget = DYNAMIC-FUNCTION("getEventWidget",ihToolbar,"browseconfig","button").
    IF VALID-HANDLE(hWidget) THEN DO:
      IF getAttribute(ttObject.hObject,"allviewfields") NE getAttribute(ttObject.hObject,"currviewfields") 
         OR getAttribute(ttObject.hObject,"noeditfields") NE "" THEN 
        cButton = getAttribute(SESSION,"activeconfigbutton").
      ELSE
        cButton = getAttribute(SESSION,"passiveconfigbutton").        
      &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
        hWidget:LOAD-IMAGE(cButton).
      &ENDIF  
    END.
    hWidget = DYNAMIC-FUNCTION("getEventWidget",ihToolbar,"browseconfig","menu-item").
    IF VALID-HANDLE(hWidget) AND hWidget:TOGGLE-BOX THEN 
      hWidget:CHECKED = getAttribute(ttObject.hObject,"allviewfields") NE getAttribute(ttObject.hObject,"currviewfields") 
                        OR getAttribute(ttObject.hObject,"noeditfields") NE "". 

    hWidget = DYNAMIC-FUNCTION("getEventWidget",ihToolbar,"filter","button").
    IF VALID-HANDLE(hWidget) THEN DO:
      IF getAttribute(ttObject.hObject,"queryfilter") NE "" 
         OR getAttribute(ttObject.hObject,"prescanqueryfilter") NE "" 
         OR getAttribute(ttObject.hObject,"calcfieldfilter") NE "" THEN 
        cButton = getAttribute(SESSION,"activefilterbutton").
      ELSE
        cButton = getAttribute(SESSION,"passivefilterbutton").      
      &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
        hWidget:LOAD-IMAGE(cButton).
      &ENDIF  
    END.
    hWidget = DYNAMIC-FUNCTION("getEventWidget",ihToolbar,"filter","menu-item").
    IF VALID-HANDLE(hWidget) AND hWidget:TOGGLE-BOX THEN 
      hWidget:CHECKED = getAttribute(ttObject.hObject,"queryfilter") NE "". 
  END.
END CASE.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LinkAllObjects) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LinkAllObjects Procedure 
FUNCTION LinkAllObjects RETURNS LOGICAL
  ( INPUT ihWindow     AS HANDLE,
    INPUT bReplace     AS LOG,
    INPUT icExceptList AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Automatically link all objects 
    Notes: 
------------------------------------------------------------------------------*/
IF bReplace THEN DO:
  FOR EACH ttObject 
      WHERE ttObject.hWindow = ihWindow:
    FOR EACH ttObjectLink 
        WHERE ttObjectLink.hFromObject = ttObject.hObject:
      DELETE ttObjectLink.
    END.
    FOR EACH ttObjectLink 
        WHERE ttObjectLink.hToObject = ttObject.hObject:
      DELETE ttObjectLink.
    END.
  END.
END.
FOR EACH ttObject 
    WHERE ttObject.hWindow = ihWindow
      AND NOT CAN-DO(icExceptList,STRING(ttObject.hObject)),
     EACH bttObject
          WHERE bttObject.hWindow = ihWindow
            AND bttObject.hObject NE ttObject.hObject
            AND NOT CAN-DO(icExceptList,STRING(bttObject.hObject)):
  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ttObject.hObject
         ttObjectLink.hToObject   = bttObject.hObject
         ttObjectLink.cLinkType   = bttObject.cObjectType.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadUserFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadUserFilter Procedure 
FUNCTION LoadUserFilter RETURNS LOGICAL
  ( INPUT ihObject     AS HANDLE,
    INPUT ihSourceProc AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFilterSetting AS CHAR NO-UNDO.
DEF VAR bUseResizeLib AS LOG NO-UNDO.

bUseResizeLib = DYNAMIC-FUNCTION("getUseResizeLibWithJukebox") NO-ERROR.
IF NOT bUseResizeLib THEN RETURN NO.

IF ihSourceProc = ? THEN ihSourceProc = getObjectSourceFileHandle(ihObject).

cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                 ihSourceProc:FILE-NAME,
                 getObjectName(ihObject),
                 getAttribute(ihObject,"usersettingcontext"),
                 "queryfilter").
IF cFilterSetting NE "" THEN
  setAttribute(ihObject,"queryfilter",cFilterSetting).

cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                 ihSourceProc:FILE-NAME,
                 getObjectName(ihObject),
                 getAttribute(ihObject,"usersettingcontext"),
                 "calcfieldfilter").
IF cFilterSetting NE "" THEN
  setAttribute(ihObject,"calcfieldfilter",cFilterSetting).

cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                 ihSourceProc:FILE-NAME,
                 getObjectName(ihObject),
                 getAttribute(ihObject,"usersettingcontext"),
                 "prescanqueryfilter").
IF cFilterSetting NE "" THEN
  setAttribute(ihObject,"prescanqueryfilter",cFilterSetting).

IF getAttribute(ihObject,"queryfilter") NE "" OR getAttribute(ihObject,"calcfieldfilter") NE "" OR getAttribute(ihObject,"prescanqueryfilter") NE "" THEN DO:
  cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                                  ihSourceProc:FILE-NAME,
                                  getObjectName(ihObject),
                                  getAttribute(ihObject,"usersettingcontext"),
                                  "filtervaluelist").
  DO ix = 1 TO NUM-ENTRIES(cFilterSetting,"|"):
    setAttribute(ihObject,"filtervalue_" + ENTRY(1,ENTRY(ix,cFilterSetting,"|"),"¤"),ENTRY(2,ENTRY(ix,cFilterSetting,"|"),"¤")).
  END.

  cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                                  ihSourceProc:FILE-NAME,
                                  getObjectName(ihObject),
                                  getAttribute(ihObject,"usersettingcontext"),
                                  "operatorinuselist").
  DO ix = 1 TO NUM-ENTRIES(cFilterSetting,"|"):
    setAttribute(ihObject,"OperatorInUse_" + ENTRY(1,ENTRY(ix,cFilterSetting,"|"),"¤"),ENTRY(2,ENTRY(ix,cFilterSetting,"|"),"¤")).
  END.

  cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                                  ihSourceProc:FILE-NAME,
                                  getObjectName(ihObject),
                                  getAttribute(ihObject,"usersettingcontext"),
                                  "groupoperatorlist").
  DO ix = 1 TO NUM-ENTRIES(cFilterSetting,"|"):
    setAttribute(ihObject,"GroupOperator_" + ENTRY(1,ENTRY(ix,cFilterSetting,"|"),"¤"),ENTRY(2,ENTRY(ix,cFilterSetting,"|"),"¤")).
  END.

  cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                                  ihSourceProc:FILE-NAME,
                                  getObjectName(ihObject),
                                  getAttribute(ihObject,"usersettingcontext"),
                                  "fieldgrouplist").
  DO ix = 1 TO NUM-ENTRIES(cFilterSetting,"|"):
    setAttribute(ihObject,"FieldGroup_" + ENTRY(1,ENTRY(ix,cFilterSetting,"|"),"¤"),ENTRY(2,ENTRY(ix,cFilterSetting,"|"),"¤")).
  END.  

  cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                                  ihSourceProc:FILE-NAME,
                                  getObjectName(ihObject),
                                  getAttribute(ihObject,"usersettingcontext"),
                                  "fieldoperatorlist").
  DO ix = 1 TO NUM-ENTRIES(cFilterSetting,"|"):
    setAttribute(ihObject,"FieldOperator_" + ENTRY(1,ENTRY(ix,cFilterSetting,"|"),"¤"),ENTRY(2,ENTRY(ix,cFilterSetting,"|"),"¤")).
  END.
  
  cFilterSetting = DYNAMIC-FUNCTION("getUserSetting",
                                  ihSourceProc:FILE-NAME,
                                  getObjectName(ihObject),
                                  getAttribute(ihObject,"usersettingcontext"),
                                  "hiddengroupoperatorlist").
  DO ix = 1 TO NUM-ENTRIES(cFilterSetting,"|"):
    setAttribute(ihObject,"HiddenGroupOperator_" + ENTRY(1,ENTRY(ix,cFilterSetting,"|"),"¤"),ENTRY(2,ENTRY(ix,cFilterSetting,"|"),"¤")).
  END.

  setAttribute(ihObject,"advancedfilter",
               DYNAMIC-FUNCTION("getUserSetting",
                                ihSourceProc:FILE-NAME,
                                getObjectName(ihObject),
                                getAttribute(ihObject,"usersettingcontext"),
                                "advancedfilter")
               ).

  RETURN YES.
END.

ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MergeToolbars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MergeToolbars Procedure 
FUNCTION MergeToolbars RETURNS LOGICAL
  ( INPUT ihSourceTB AS HANDLE,
    INPUT ihTargetTB AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Merge two toolbars to one by copying attributes and events from source to target and deleting links to/from source 
           NB: Programmer must restore any links for source TB when target toolbar is deleted
    Notes: Sometimes neccessary cause we only support one toolbar (record) link.
           Since all triggers are already created this will not affect where overrides will be run
           Typical usage is when details for a record is shown in a popup-window for maintenance
------------------------------------------------------------------------------*/
FIND FIRST ttObject 
     WHERE ttObject.hObject = ihSourceTB
     NO-ERROR.
IF NOT AVAIL ttObject OR (AVAIL ttObject AND ttObject.cObjectType NE "toolbar") THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Invalid source toolbar reference"
          VIEW-AS ALERT-BOX.
  RETURN NO.
END.
FIND FIRST bttObject 
     WHERE bttObject.hObject = ihTargetTB
     NO-ERROR.
IF NOT AVAIL bttObject OR (AVAIL bttObject AND bttObject.cObjectType NE "toolbar") THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Invalid target toolbar reference"
          VIEW-AS ALERT-BOX.
  RETURN NO.
END.

FOR EACH ttEvent
    WHERE ttEvent.hObject = ttObject.hObject:
  CREATE bttEvent.
  BUFFER-COPY ttEvent TO bttEvent.
  ASSIGN bttEvent.hObject = bttObject.hObject.
END.
FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ttObject.hObject:
  CREATE bttAttribute.
  BUFFER-COPY ttAttribute TO bttAttribute.
  ASSIGN bttAttribute.hObject = bttObject.hObject.
END.
FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ihSourceTB:
  DELETE ttObjectLink.
END.
FOR EACH ttObjectLink
    WHERE ttObjectLink.hToObject = ihSourceTB:
  DELETE ttObjectLink.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mgetAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION mgetAttribute Procedure 
FUNCTION mgetAttribute RETURNS CHARACTER
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Return all attribute values where attribute name begins or matches.. 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocReturn AS CHAR NO-UNDO.

IF ihObject NE ? THEN DO:
  IF INDEX(icName,"*") > 0 THEN
    FOR EACH ttAttribute 
        WHERE ttAttribute.hObject = ihObject
          AND ttAttribute.cName MATCHES icName:  
      ocReturn = ocReturn + ttAttribute.cValue + ",".
    END.
  ELSE
    FOR EACH ttAttribute 
        WHERE ttAttribute.hObject = ihObject
          AND ttAttribute.cName BEGINS icName:  
      ocReturn = ocReturn + ttAttribute.cValue + ",".
    END.
END.
ELSE DO:
  IF INDEX(icName,"*") > 0 THEN
    FOR EACH ttAttribute 
        WHERE ttAttribute.cName MATCHES icName:  
      ocReturn = ocReturn + ttAttribute.cValue + ",".
    END.
  ELSE
    FOR EACH ttAttribute 
        WHERE ttAttribute.cName BEGINS icName:  
      ocReturn = ocReturn + ttAttribute.cValue + ",".
    END.
END.

RETURN TRIM(ocReturn,",").   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoveBrowseColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MoveBrowseColumn Procedure 
FUNCTION MoveBrowseColumn RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icColumnName AS CHAR,
    INPUT iiToPos      AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix              AS INT  NO-UNDO.
DEF VAR iy              AS INT  NO-UNDO.
DEF VAR cFieldReposList AS CHAR NO-UNDO.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  IF NOT ihBrowse:GET-BROWSE-COLUMN(ix):VISIBLE THEN NEXT.
  iy = iy + 1.
  IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = icColumnName THEN DO:
    ihBrowse:MOVE-COLUMN(iy,iiToPos).
    LEAVE.
  END.
END.
/* IF iy > iiToPos THEN DO:                                                                                              */
/*   cFieldReposList = getAttribute(ihBrowse,"fieldreposlist").                                                          */
/*   DO ix = 1 TO NUM-ENTRIES(cFieldReposList):                                                                          */
/*     IF INT(ENTRY(3,ENTRY(ix,cFieldReposList),";")) > iiToPos THEN                                                     */
/*       MoveBrowseColumn(ihBrowse,ENTRY(1,ENTRY(ix,cFieldReposList),";"),INT(ENTRY(3,ENTRY(ix,cFieldReposList),";"))).  */
/*   END.                                                                                                                */
/* END.                                                                                                                  */
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-msetAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION msetAttribute Procedure 
FUNCTION msetAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set all attribute values where attribute name begins or matches.. 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocReturn AS CHAR NO-UNDO.

IF ihObject NE ? THEN DO:
  IF INDEX(icName,"*") > 0 THEN
    FOR EACH ttAttribute 
        WHERE ttAttribute.hObject = ihObject
          AND ttAttribute.cName MATCHES icName:  
      ttAttribute.cValue = icValue.
    END.
  ELSE
    FOR EACH ttAttribute 
        WHERE ttAttribute.hObject = ihObject
          AND ttAttribute.cName BEGINS icName:  
      ttAttribute.cValue = icValue.
    END.
END.
ELSE DO:
  IF INDEX(icName,"*") > 0 THEN
    FOR EACH ttAttribute 
        WHERE ttAttribute.cName MATCHES icName:  
      ttAttribute.cValue = icValue.
    END.
  ELSE
    FOR EACH ttAttribute 
        WHERE ttAttribute.cName BEGINS icName:  
      ttAttribute.cValue = icValue.
    END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewBrowse Procedure 
FUNCTION NewBrowse RETURNS HANDLE
  ( INPUT ihRectOrBrw        AS HANDLE,
    INPUT iiRowsToBatch      AS INT,
    INPUT icProperties       AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icOtherProp        AS CHAR) :

DEF VAR hBrowse                AS HANDLE NO-UNDO.
DEF VAR hField                 AS HANDLE NO-UNDO.
DEF VAR iCount                 AS INT    NO-UNDO.
DEF VAR cLastRowid             AS CHAR   NO-UNDO.
DEF VAR cFirstRowid            AS CHAR   NO-UNDO.
DEF VAR iViewFieldCount        AS INT    NO-UNDO.
DEF VAR iStartCountPeriodFld   AS INT    NO-UNDO.
DEF VAR hBaseField             AS HANDLE NO-UNDO.
DEF VAR cBaseFieldLabel        AS CHAR   NO-UNDO.
DEF VAR cAllViewFields         AS CHAR   NO-UNDO.
DEF VAR cAllViewFieldsReal     AS CHAR   NO-UNDO.
DEF VAR cNoDisplayFields       AS CHAR   NO-UNDO.
DEF VAR cReturnField           AS CHAR   NO-UNDO.
DEF VAR cStatValues            AS CHAR   NO-UNDO.
DEF VAR cFieldReposList        AS CHAR   NO-UNDO.
DEF VAR cCurrViewFields        AS CHAR   NO-UNDO.
DEF VAR cWidthPixelsList       AS CHAR   NO-UNDO.
DEF VAR cUserWidthPixels       AS CHAR   NO-UNDO.
DEF VAR iy                     AS INT    NO-UNDO.
DEF VAR bUserFilter            AS LOG    NO-UNDO.
DEF VAR cBufferFieldSpec       AS CHAR   NO-UNDO.       
DEF VAR cBufferDbFieldSpec     AS CHAR   NO-UNDO.  
DEF VAR hMySourceProc          AS HANDLE NO-UNDO.
DEF VAR cTTfieldspec           AS CHAR   NO-UNDO.
DEF VAR cExtentFields          AS CHAR   NO-UNDO.
DEF VAR cAltExtentFields       AS CHAR   NO-UNDO.
DEF VAR cExtentFieldHandles    AS CHAR   NO-UNDO.
DEF VAR cAltExtentFieldHandles AS CHAR   NO-UNDO.
DEF VAR cExtent                AS CHAR   NO-UNDO.
DEF VAR cOrgExtentField        AS CHAR   NO-UNDO.
DEF VAR cOrgViewFields         AS CHAR   NO-UNDO.

{incl/methodlog.i ihRectOrBrw:NAME}

RUN ConstructBrowseOrQuery(ihRectOrBrw,icBuffersAndFields,icQueryCriteria,icOtherProp,OUTPUT bOk).
IF NOT bOK THEN RETURN ?.

ASSIGN hObjectSourceProc = (IF NOT VALID-HANDLE(hObjectSourceProc) THEN SOURCE-PROCEDURE ELSE hObjectSourceProc)
       hMySourceProc = hObjectSourceProc
       bUserFilter = LoadUserFilter(ihRectOrBrw,hObjectSourceProc)
       .

IF ENTRY(1,icBuffersAndFields,";") BEGINS "temp-table" THEN DO:
  IF LEFT-TRIM(SUBSTR(ENTRY(1,icBuffersAndFields,";"),12,5)) BEGINS "like " THEN
    cTTfieldSpec = TRIM(SUBSTR(icBuffersAndFields,INDEX(icBuffersAndFields," like ") + 6)).
  httTable = NewTempTable(ihRectOrBrw:WINDOW,
                         (IF LEFT-TRIM(SUBSTR(ENTRY(1,icBuffersAndFields,";"),12,5)) BEGINS "like " THEN
                            LEFT-TRIM(SUBSTR(ENTRY(1,icBuffersAndFields,";"),12))
                          ELSE IF NUM-ENTRIES(ENTRY(1,icBuffersAndFields,";")," ") > 1 THEN
                            ENTRY(2,ENTRY(1,icBuffersAndFields,";")," ")
                          ELSE
                            ihRectOrBrw:NAME),
                          hObjectSourceProc,
                          IF cTTfieldSpec NE "" THEN
                            cTTfieldSpec
                          ELSE
                            SUBSTR(icBuffersAndFields,INDEX(icBuffersAndFields,";") + 1),
                          "",
                          IF cTTfieldSpec NE "" THEN icQueryCriteria
                          ELSE "").
  setAttribute(ihRectOrBrw,"uselocaldata","yes").
  hObjectSourceProc = hMySourceProc.
END.
ELSE IF getAttribute(ihRectOrBrw,"temptablehandle") = "" AND ihRectOrBrw:TYPE NE "browse" THEN DO:
  DYNAMIC-FUNCTION("setIndexOnRowids",YES).
  DYNAMIC-FUNCTION("setReturnQueryInfo",YES).
  httTable = DYNAMIC-FUNCTION("getTempTableJoin",iiRowsToBatch,0,"",
                              getAttribute(ihRectOrBrw,"buffersandfields"),
                              "where false" + getAttribute(ihRectOrBrw,"queryjoin")).
  
  IF DYNAMIC-FUNCTION("getTransactionMessage") NE "" THEN DO:
    IF DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*(233)*" OR
       DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*(234)*" THEN DO:
      IF getAttribute(SESSION,"showsecuritywarnings") NE "no" THEN 
        IF DYNAMIC-FUNCTION("DoMessage",-1,4,DYNAMIC-FUNCTION("getTransactionMessage") + CHR(10) + CHR(10) +
                            "Continue to show warnings when I try query restricted database tables?" 
                            ,"Database Security Warning","") = 7 THEN
          setAttribute(SESSION,"showsecuritywarnings","no").
    END.
    ELSE DO:
      DYNAMIC-FUNCTION("DoMessage",-1,0,
                 DYNAMIC-FUNCTION("getTransactionMessage") + CHR(10)
                 + "Function: " + PROGRAM-NAME(1) + CHR(10) 
                 + "Caller: " + PROGRAM-NAME(2)
                ,"JukeBox, browse definition error:","").
      QUIT.
    END.
  END.
END.
ELSE IF getAttribute(ihRectOrBrw,"temptablehandle") NE "" THEN DO:
  httTable = WIDGET-HANDLE(getAttribute(ihRectOrBrw,"temptablehandle")).
  setAttribute(ihRectOrBrw,"uselocaldata","yes").
END.
ELSE IF ihRectOrBrw:TYPE = "browse" THEN DO:
  ASSIGN ihRectOrBrw:READ-ONLY = YES
         httTableBuffer = ihRectOrBrw:QUERY:GET-BUFFER-HANDLE(1)
         .
  /* Check for db access here..*/
/* ELSE httTable = ihRectOrBrw:QUERY:GET-BUFFER-HANDLE(1):TABLE-HANDLE. */
END.
ELSE DO:
  MESSAGE "Invalid parameters to NewBrowse function"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN ?.
END.

IF NOT VALID-HANDLE(httTable) AND ihRectOrBrw:TYPE NE "browse" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage") + CHR(10)
                                 + "Function: " + PROGRAM-NAME(1) + CHR(10) 
                                 + "Caller: " + PROGRAM-NAME(2)
                   ,"JukeBox, browse definition error:","").
  RETURN ?.
END.

IF ihRectOrBrw:TYPE NE "browse" THEN DO:
  IF getAttribute(ihRectOrBrw,"temptablebuffer") NE "" THEN 
    httTableBuffer = WIDGET-HANDLE(getAttribute(ihRectOrBrw,"temptablebuffer")).
  ELSE
    httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httTableQuery NO-ERROR.
  httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
END.

RUN CreateBrowseAndEvents(ihRectOrBrw,httTableQuery,hObjectSourceProc,icProperties,OUTPUT hBrowse).
CREATE ttObject.
ASSIGN ttObject.hWindow       = ihRectOrBrw:WINDOW
       ttObject.hObject       = hBrowse
       ttObject.cObjectType   = "browse"
       ttObject.hDesignObject = ihRectOrBrw
       ttObject.cObjectName   = ihRectOrBrw:NAME
       ttObject.hSourceProc   = hObjectSourceProc
       ttObject.cInitProc     = PROGRAM-NAME(2)
       ttObject.cGenProc      = PROGRAM-NAME(1)
       .
FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihRectOrBrw:
  ttAttribute.hObject = hBrowse.
END.

setAttribute(hBrowse,"placeholderHandle",STRING(ihRectOrBrw)).
setAttribute(hBrowse,"placeholderName",ihRectOrBrw:NAME).

setAttribute(hBrowse,"nodbreadaccessfields",DYNAMIC-FUNCTION("getNoDbReadAccessFields")).
setAttribute(hBrowse,"nodbwriteaccessfields",DYNAMIC-FUNCTION("getNoDbWriteAccessFields")).
setAttribute(hBrowse,"nodbreadaccessbuffers",DYNAMIC-FUNCTION("getNoDbReadAccessBuffers")).
setAttribute(hBrowse,"nodbwriteaccessbuffers",DYNAMIC-FUNCTION("getNoDbWriteAccessBuffers")).
setAttribute(hBrowse,"nodbcreateaccessbuffers",DYNAMIC-FUNCTION("getNoDbCreateAccessBuffers")).
setAttribute(hBrowse,"nodbdeleteaccessbuffers",DYNAMIC-FUNCTION("getNoDbDeleteAccessBuffers")).

cBufferFieldSpec = DYNAMIC-FUNCTION("getBufferFieldSpec").
IF cBufferFieldSpec NE "" THEN DO:
  cBufferDbFieldSpec = DYNAMIC-FUNCTION("getBufferDbFieldSpec").
  DO ix = 1 TO NUM-ENTRIES(cBufferFieldSpec):
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,cBufferFieldSpec),"|"):
      setAttribute(hBrowse,"fieldbuffer" + ENTRY(iy,ENTRY(ix,cBufferFieldSpec),"|"),ENTRY(1,ENTRY(ix,cBufferFieldSpec),"|")).
      setAttribute(hBrowse,"orgdbfield" + ENTRY(iy,ENTRY(ix,cBufferFieldSpec),"|"),ENTRY(iy,ENTRY(ix,cBufferDbFieldSpec),"|")).
    END.
  END.
END.
ELSE IF getAttribute(hBrowse,"uselocaldata") = "yes" THEN DO ix = 1 TO httTable:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
  setAttribute(hBrowse,"fieldbuffer" + httTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ix):NAME,httTable:DEFAULT-BUFFER-HANDLE:NAME).
  setAttribute(hBrowse,"orgdbfield" + httTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ix):NAME,httTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ix):NAME).
END.

setAttribute(hBrowse,"rowstobatch",STRING(iiRowsToBatch)).
IF getAttribute(hBrowse,"querywhere") BEGINS "where false" THEN
  setAttribute(hBrowse,"querywhere","").

hField = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowCount") NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
  setAttribute(hBrowse,"RowCountFieldHandle",STRING(hField)).

ASSIGN cNoDisplayFields = getAttribute(hBrowse,"nodisplayfields")
       cAllViewFields   = getAttribute(hBrowse,"allviewfields").
DO ix = 1 TO httTableBuffer:NUM-FIELDS:
  IF (NOT httTableBuffer:BUFFER-FIELD(ix):NAME BEGINS "RowIdent" AND
      httTableBuffer:BUFFER-FIELD(ix):NAME NE "RowCount" AND
      LOOKUP(httTableBuffer:BUFFER-FIELD(ix):NAME,cNoDisplayFields) = 0) OR
      LOOKUP(httTableBuffer:BUFFER-FIELD(ix):NAME,cAllViewFields) > 0
     THEN DO:

    IF httTableBuffer:BUFFER-FIELD(ix):DATA-TYPE = "blob" THEN NEXT.

    cReturnField = httTableBuffer:BUFFER-FIELD(ix):NAME.
    IF CAN-DO("2,3,4,5,6,7,8,9",SUBSTR(cReturnField,LENGTH(cReturnField))) AND
       LOOKUP(SUBSTR(cReturnField,1,LENGTH(cReturnField) - 1),cNoDisplayFields) > 0
       THEN NEXT.

    IF cReturnField BEGINS "jbextent_" THEN DO:
      ASSIGN cOrgExtentField        = SUBSTR(cReturnField,INDEX(cReturnField,"_",10) + 1)
             cExtent                = SUBSTR(cReturnField,10,INDEX(cReturnField,"_",10) - 10)
             .
      setAttribute(hBrowse,"orgDbField" + cOrgExtentField + "_" + cExtent,cOrgExtentField + "[" + cExtent + "]").

      ASSIGN cOrgExtentField        = cOrgExtentField + (IF ihRectOrBrw:TYPE NE "browse" THEN "[" + cExtent + "]" ELSE "_" + cExtent)
             cExtentFields          = cExtentFields + cOrgExtentField + ","
             cAltExtentFields       = cAltExtentFields + cReturnField + ","
             cExtentFieldHandles    = cExtentFieldHandles + STRING(httTableBuffer:BUFFER-FIELD(cOrgExtentField)) + ",".
             cAltExtentFieldHandles = cAltExtentFieldHandles + STRING(httTableBuffer:BUFFER-FIELD(ix)) + ",".
      setAttribute(hBrowse,"extent_" + cReturnField,cExtent).
      setAttribute(hBrowse,"extent_" + cOrgExtentField,cExtent).
      NEXT.
    END.
    
    IF cReturnField NE "jbCountDistinct" THEN
      iViewFieldCount = iViewFieldCount + 1.

    IF ihRectOrBrw:TYPE NE "browse" THEN DO:
      hField = hBrowse:ADD-LIKE-COLUMN(httTableBuffer:BUFFER-FIELD(ix)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        MESSAGE PROGRAM-NAME(1) SKIP
                ERROR-STATUS:GET-MESSAGE(1) 
                VIEW-AS ALERT-BOX ERROR.
    END.
    ELSE DO:
      hField = getBrowseColumn(hBrowse,httTableBuffer:BUFFER-FIELD(ix):NAME) NO-ERROR.
      IF NOT VALID-HANDLE(hField) THEN NEXT.
    END.

    ON END-RESIZE    OF hField PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"end-resize").
    CREATE ttEvent.
    ASSIGN ttEvent.hObject       = hBrowse
           ttEvent.hWidget       = hField
           ttEvent.cAction       = "end-resize"
           ttEvent.cName         = "end-resize"
           ttEvent.cWidgetType   = "browse-column"
           ttEvent.cMethod       = "EndResizeBrowseColumn".
    IF NOT SESSION:DISPLAY-TYPE = "TTY" AND bSetSortLabel AND hField:WIDTH - LENGTH(hField:LABEL) < 4 THEN
      hField:WIDTH = hField:WIDTH + 5 - MAX(0,hField:WIDTH - LENGTH(hField:LABEL)).

    IF hField:LABEL BEGINS "jb_" AND INDEX(hField:LABEL,"_",4) > 0 THEN DO:
      ASSIGN iStartCountPeriodFld = iStartCountPeriodFld + 1
             hBaseField           = hBrowse:GET-BROWSE-COLUMN(iViewFieldCount - iStartCountPeriodFld)
             cBaseFieldLabel      = hBaseField:LABEL.
             
      CASE SUBSTR(hField:LABEL,1,8):
        WHEN "jb_week_" THEN hField:LABEL = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Uke[" ELSE "Week[") + cBaseFieldLabel + "]".
        WHEN "jb_month" THEN hField:LABEL = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Mnd[" ELSE "Month[") + cBaseFieldLabel + "]".
        WHEN "jb_quart" THEN hField:LABEL = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Kv[" ELSE "Q[") + cBaseFieldLabel + "]".
        WHEN "jb_year_" THEN hField:LABEL = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "År[" ELSE "Year[") + cBaseFieldLabel + "]".
      END CASE.
      hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hField:NAME):LABEL = hField:LABEL.
    END.
    ELSE iStartCountPeriodFld = 0.

    IF cReturnField = "jbCountDistinct" THEN 
      ASSIGN hField:VISIBLE = NO
             hField:LABEL   = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Antall" ELSE "Count"
             NO-ERROR.
    IF cReturnField = "jbAverage" THEN 
      ASSIGN hField:VISIBLE = NO
             hField:LABEL   = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Gj.snitt" ELSE "Average"
             NO-ERROR.
  END.
END.
setAttribute(hBrowse,"extentFields",TRIM(cExtentFields,",")).
setAttribute(hBrowse,"altExtentFields",TRIM(cAltExtentFields,",")).
setAttribute(hBrowse,"extentFieldHandles",TRIM(cExtentFieldHandles,",")).
setAttribute(hBrowse,"altExtentFieldHandles",TRIM(cAltExtentFieldHandles,",")).
setBrowseProperties(icProperties,hBrowse).
cFieldReposList = getAttribute(hBrowse,"fieldreposlist").

EMPTY TEMP-TABLE ttSort1.
DO ix = 1 TO NUM-ENTRIES(cFieldReposList):
  CREATE ttSort1.
  ASSIGN ttSort1.cText1 = ENTRY(1,ENTRY(ix,cFieldReposList),";")
         ttSort1.iSeq   = INT(ENTRY(2,ENTRY(ix,cFieldReposList),";"))
         ttSort1.iSeq2  = INT(ENTRY(3,ENTRY(ix,cFieldReposList),";"))
         .
END.
DO ix = 1 TO NUM-ENTRIES(cFieldReposList):
  FOR EACH ttSort1 BY iSeq:
    MoveBrowseColumn(hBrowse,ttSort1.cText1,ttSort1.iSeq2).
  END.
  FOR EACH ttSort1 BY iSeq2 DESC:
    MoveBrowseColumn(hBrowse,ttSort1.cText1,ttSort1.iSeq2).
  END.
END.

IF CAN-DO(ttObject.hSourceProc:INTERNAL-ENTRIES,"AdjustBrowseColumns") THEN
  DYNAMIC-FUNCTION("AdjustBrowseColumns" IN ttObject.hSourceProc,ttObject.hObject,ttObject.cObjectName).

IF SESSION:DISPLAY-TYPE NE "TTY" THEN 
  DO ix = 1 TO hBrowse:NUM-COLUMNS:
    IF hBrowse:GET-BROWSE-COLUMN(ix):VISIBLE THEN
      ASSIGN cAllViewFieldsReal = cAllViewFieldsReal + hBrowse:GET-BROWSE-COLUMN(ix):NAME + ","
             cWidthPixelsList   = cWidthPixelsList + STRING(hBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS) + ",".
  END.
ELSE cAllViewFieldsReal = cAllViewFields.

cAllViewFieldsReal = TRIM(cAllViewFieldsReal,",").
setAttribute(hBrowse,"allviewfields",cAllViewFieldsReal).
setAttribute(hBrowse,"orgwidthpixelslist",cWidthPixelsList).
IF getAttribute(hBrowse,"basetablefilterfields") = "" THEN
  setAttribute(hBrowse,"basetablefilterfields",cAllViewFieldsReal).

IF bSetSortLabel AND getAttribute(hBrowse,"querysort") NE "" THEN 
  DYNAMIC-FUNCTION("setSortLabel",hBrowse,getAttribute(hBrowse,"querysort"),IF getAttribute(hBrowse,"querydesc") NE "" THEN TRUE ELSE FALSE).

FIND ttObject WHERE ttObject.hObject = hBrowse.
setAttribute(ttObject.hObject,
             "noeditfields",
             DYNAMIC-FUNCTION("getUserSetting",
                              IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc:FILE-NAME ELSE ttObject.hSourceProc:FILE-NAME,
                               ttObject.cObjectName,
                               getAttribute(hBrowse,"usersettingcontext"),
                               "noeditfields")).
cUserWidthPixels = DYNAMIC-FUNCTION("getUserSetting",
                                   IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc:FILE-NAME ELSE ttObject.hSourceProc:FILE-NAME,
                                   ttObject.cObjectName,
                                   getAttribute(hBrowse,"usersettingcontext"),
                                   "widthpixelslist").
IF cUserWidthPixels NE "" THEN
  DO ix = 1 TO NUM-ENTRIES(cUserWidthPixels):
    iy = LOOKUP(ENTRY(1,ENTRY(ix,cUserWidthPixels),";"),cAllViewFieldsReal).
    IF iy > 0 THEN
      ENTRY(iy,cWidthPixelsList) = ENTRY(2,ENTRY(ix,cUserWidthPixels),";").
  END.
setAttribute(hBrowse,"widthpixelslist",TRIM(cWidthPixelsList,",")).
IF getAttribute(hBrowse,"currviewfields") = "" THEN DO:
  cCurrViewFields = DYNAMIC-FUNCTION("getUserSetting",
                                     IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc:FILE-NAME ELSE ttObject.hSourceProc:FILE-NAME,
                                     ttObject.cObjectName,
                                     getAttribute(hBrowse,"usersettingcontext"),
                                     "currviewfields").
  cOrgViewFields = DYNAMIC-FUNCTION("getUserSetting",
                                     IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc:FILE-NAME ELSE ttObject.hSourceProc:FILE-NAME,
                                     ttObject.cObjectName,
                                     getAttribute(hBrowse,"usersettingcontext"),
                                     "orgviewfields").
  IF cOrgViewFields NE "" THEN DO:
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"IssueWarningWhenBrowseHasChanged") = "" THEN
      cCurrViewFields = DYNAMIC-FUNCTION("syncLists",cCurrViewFields,cAllViewFieldsReal,",").
    ELSE IF NOT DYNAMIC-FUNCTION("equalLists",cOrgViewFields,cAllViewFieldsReal,",") THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       IF DYNAMIC-FUNCTION("Scandinavian") THEN
                         "Definisjon av standard kolonner er endret siden sist du lagret overstyring av kolonneoppsett" + CHR(10)
                       + "Det anbefales at du sletter kolonneoppsettet du har lagret"
                       ELSE
                         "The definition of standards columns has changed since your last save of column setup" + CHR(10)
                       + "It is recommended that you delete the column setup override"
                      ,"","").
  END. 
END.
ELSE 
  cCurrViewFields = getAttribute(hBrowse,"currviewfields").
setAttribute(hBrowse,"overlaybgcolor",DYNAMIC-FUNCTION("getUserSetting",
                                      IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc:FILE-NAME ELSE ttObject.hSourceProc:FILE-NAME,
                                      ttObject.cObjectName,
                                      getAttribute(hBrowse,"usersettingcontext"),
                                      "overlaybgcolor")).
IF getAttribute(hBrowse,"overlaybgcolor") NE "" THEN
  setAttribute(hBrowse,"overlaybgcolnum",STRING(DYNAMIC-FUNCTION("getColNumFromRGB",INT(getAttribute(hBrowse,"overlaybgcolor"))))) NO-ERROR.

IF cCurrViewFields = "" THEN cCurrViewFields = cAllViewFieldsReal.
  setAttribute(hBrowse,"currviewfields",cCurrViewFields).
IF cCurrViewFields NE cAllViewFieldsReal 
   OR cUserWidthPixels NE "" 
   OR getAttribute(hBrowse,"nodbreadaccessfields") NE "" 
   OR getAttribute(hBrowse,"nodbwriteaccessfields") NE "" 
   OR getAttribute(hBrowse,"nodbwriteaccessbuffers") NE "" 
   THEN 
  setBrowseColumns(hBrowse,cCurrViewFields,NO).

IF SESSION:DISPLAY-TYPE NE "TTY" THEN DO:  
  IF LOOKUP("!FIT-LAST-COLUMN",icProperties) = 0 THEN
    hBrowse:FIT-LAST-COLUMN = TRUE NO-ERROR.
  ELSE 
    hBrowse:FIT-LAST-COLUMN = FALSE NO-ERROR.
  DYNAMIC-FUNCTION("setFollowPlaceHolder",hBrowse:WINDOW,hBrowse,ihRectOrBrw) NO-ERROR.
END.  

IF NOT LEFT-TRIM(icQueryCriteria) BEGINS "where false" THEN DO:
  hCurrSourceProc = hObjectSourceProc.
  IF getAttribute(hBrowse,"getrecordcount") = "yes" THEN
    DYNAMIC-FUNCTION("setQueryStatFields","rowcount").
  FillBrowse(hBrowse,iiRowsToBatch,0,
             getAttribute(hBrowse,"buffersandfields"),
             getAttribute(hBrowse,"basequery") +
             getAttribute(hBrowse,"querywhere") +
            (IF getAttribute(hBrowse,"use-index_" + getAttribute(hBrowse,"1stsortcolumn")) NE "" THEN
               " USE-INDEX " + getAttribute(hBrowse,"use-index_" + getAttribute(hBrowse,"1stsortcolumn"))
             ELSE IF getAttribute(hBrowse,"use-index") NE "" THEN
               " USE-INDEX " + getAttribute(hBrowse,"use-index")
             ELSE "") +
            (IF (bUserFilter AND getAttribute(hBrowse,"startwithuserfilter") = "no")
                OR getAttribute(hBrowse,"querywhere") BEGINS "where false" THEN ""
             ELSE getAttribute(hBrowse,"queryfilter")) +
             getAttribute(hBrowse,"queryjoin"),
             getAttribute(hBrowse,"querysort"),
             getAttribute(hBrowse,"querydesc") = "desc"
            ).
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
END.
ELSE hBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME).

hObjectSourceProc = ?.

RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseDropDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewBrowseDropDown Procedure 
FUNCTION NewBrowseDropDown RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icList             AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Fields prefixed with ! are added to the buffer but not to the grid  
------------------------------------------------------------------------------*/
DEF VAR hCombo            AS HANDLE NO-UNDO.
DEF VAR hBrowseColumn     AS HANDLE NO-UNDO.
DEF VAR hTmpXpixels       AS INT    NO-UNDO.
DEF VAR hTmpYpixels       AS INT    NO-UNDO.
DEF VAR iBrwRowHeight     AS INT    NO-UNDO.
DEF VAR iWidthPix         AS INT    NO-UNDO.
DEF VAR cNoDbWriteFields  AS CHAR   NO-UNDO.
DEF VAR cNoDbWriteBuffers AS CHAR   NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",ihBrowse:WINDOW).

ASSIGN hTmpXpixels = ihBrowse:FRAME:WIDTH-PIXELS
       hTmpYpixels = ihBrowse:FRAME:HEIGHT-PIXELS
       hObjectSourceProc = (IF hObjectSourceProc = ? THEN SOURCE-PROCEDURE ELSE hObjectSourceProc)
       .

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  hBrowseColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
  IF hBrowseColumn:NAME = icBrowseColumn THEN
    LEAVE.
END.
IF icBufferColumn = "" THEN icBufferColumn = icBrowseColumn.

IF hBrowseColumn:VISIBLE THEN 
  iWidthPix = MIN(ihBrowse:WIDTH-PIXELS - hBrowseColumn:WIDTH-PIXELS,hBrowseColumn:WIDTH-PIXELS + 4).
IF iWidthPix LE 0 THEN iWidthPix = 10.

CREATE COMBO-BOX hCombo 
  ASSIGN DELIMITER        = "|"
         DATA-TYPE        = "CHARACTER"
         FORMAT           = "x(256)"
         NAME             = icBrowseColumn
         SUBTYPE          = "DROP-DOWN-LIST"
         LIST-ITEM-PAIRS  = IF icList NE "||" AND icList NE "" THEN icList ELSE icList + DYNAMIC-FUNCTION("getFieldList",icBuffersAndFields,icQueryCriteria)
         INNER-LINES      = 50
         FRAME            = ihBrowse:FRAME
         X                = ihBrowse:X + (IF NOT hBrowseColumn:VISIBLE THEN 2 ELSE hBrowseColumn:X) - 1
         Y                = ihBrowse:Y + (IF NOT hBrowseColumn:VISIBLE THEN 1 ELSE hBrowseColumn:Y)
         WIDTH-PIXELS     = iWidthPix
         VISIBLE          = NO
         SENSITIVE        = TRUE
         HELP             = hBrowseColumn:HELP
         TOOLTIP          = hBrowseColumn:HELP
         TRIGGERS:
           ON VALUE-CHANGED PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"value-changed").
           ON TAB           PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON BACK-TAB      PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON RETURN        PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON ENTRY         PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"entry").
           ON END-ERROR     PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"end-error").
         END TRIGGERS.

iBrwRowHeight = INT(getAttribute(SESSION,"adjustRowHeightPixels")).

IF iBrwRowHeight NE 0 THEN
  ihBrowse:ROW-HEIGHT-PIXELS = iBrwRowHeight.
ELSE
  ihBrowse:ROW-HEIGHT-CHARS = hCombo:HEIGHT-CHARS - .25. 

CREATE ttObject.
ASSIGN ttObject.hWindow      = ihBrowse:WINDOW
       ttObject.hObject      = hCombo
       ttObject.cObjectName  = icBrowseColumn
       ttObject.cObjectType  = "combo-box"
       ttObject.hSourceProc  = hObjectSourceProc
       ttObject.cInitProc    = PROGRAM-NAME(2)
       ttObject.cGenProc     = PROGRAM-NAME(1)
       .
setAttribute(ttObject.hObject,"browsecolumnhandle",STRING(hBrowseColumn)).
setAttribute(ttObject.hObject,"browsecolumn",icBrowseColumn).
setAttribute(ttObject.hObject,"buffercolumn",icBufferColumn).
IF NOT CAN-DO(getAttribute(ihBrowse,"currviewfields"),icBrowseColumn) 
  OR CAN-DO(getAttribute(ihBrowse,"noeditfields"),icBrowseColumn) THEN
  setAttribute(ttObject.hObject,"visible","no").

CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hCombo
       ttEvent.cAction        = "value-changed"
       ttEvent.cName          = "value-changed"
       ttEvent.cWidgetType    = "browse-dropdown"
       ttEvent.cMethod        = "ValChngBrowseDropDown"
       ttEvent.bReturnNoApply = YES.
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hCombo
       ttEvent.cAction        = "leave"
       ttEvent.cName          = "leave"
       ttEvent.cWidgetType    = "browse-dropdown"
       ttEvent.cMethod        = "LeaveBrowseDropDown".
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hCombo
       ttEvent.cAction        = "entry"
       ttEvent.cName          = "entry"
       ttEvent.cWidgetType    = "browse-dropdown"
       ttEvent.cMethod        = "EnterBrowseDropDown".
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hCombo
       ttEvent.cAction        = "end-error"
       ttEvent.cName          = "end-error"
       ttEvent.cWidgetType    = "browse-dropdown"
       ttEvent.cMethod        = "EndErrorBrowseOverlay".

FOR EACH ttObjectLink
    WHERE ttObjectLink.hFromObject = ihBrowse
      AND ttObjectLink.cLinkType   = "browseoverlay"
   ,FIRST bttObject WHERE bttObject.hObject = ttObjectLink.hToObject:
  IF bttObject.cObjectType = "fill-in" THEN
    bttObject.hObject:HEIGHT-PIXELS = ihBrowse:ROW-HEIGHT-PIXELS + 5.
  ELSE IF bttObject.cObjectType = "toggle-box" THEN
    bttObject.hObject:HEIGHT-PIXELS = ihBrowse:ROW-HEIGHT-PIXELS + 3.
END.

ASSIGN ihBrowse:FRAME:WIDTH-PIXELS  = hTmpXpixels
       ihBrowse:FRAME:HEIGHT-PIXELS = hTmpYpixels
       .

IF getAttribute(ihBrowse,"overlaybgcolnum") NE "" THEN 
  ttObject.hObject:BGCOLOR = INT(getAttribute(ihBrowse,"overlaybgcolnum")).


ASSIGN cNoDbWriteFields  = getAttribute(ihBrowse,"nodbwriteaccessfields")
       cNoDbWriteBuffers = getAttribute(ihBrowse,"nodbwriteaccessbuffers")
       .
IF CAN-DO(cNoDbWriteFields,icBufferColumn) 
   OR CAN-DO(cNoDbWriteBuffers,getAttribute(ihBrowse,"fieldbuffer" + icBufferColumn))
  THEN
  setAttribute(ttObject.hObject,"visible","no").

DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN hCombo.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseFillIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewBrowseFillIn Procedure 
FUNCTION NewBrowseFillIn RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icLookupAttrib     AS CHAR,
    INPUT icValidation       AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Fields prefixed with ! are added to the buffer but not to the grid  
------------------------------------------------------------------------------*/
DEF VAR hFillIn           AS HANDLE NO-UNDO.
DEF VAR hButton           AS HANDLE NO-UNDO.
DEF VAR hBrowseColumn     AS HANDLE NO-UNDO.
DEF VAR iTmpXpixels       AS INT    NO-UNDO.
DEF VAR iTmpYpixels       AS INT    NO-UNDO.
DEF VAR cNoDbWriteFields  AS CHAR   NO-UNDO.
DEF VAR cNoDbWriteBuffers AS CHAR   NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",ihBrowse:WINDOW).

IF icBufferColumn = "" THEN icBufferColumn = icBrowseColumn.

ASSIGN iTmpXpixels = ihBrowse:FRAME:WIDTH-PIXELS
       iTmpYpixels = ihBrowse:FRAME:HEIGHT-PIXELS
       hObjectSourceProc = (IF hObjectSourceProc = ? THEN SOURCE-PROCEDURE ELSE hObjectSourceProc)
       .

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  hBrowseColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
  IF hBrowseColumn:NAME = icBrowseColumn THEN
    LEAVE.
END.
IF NOT VALID-HANDLE(hBrowseColumn) THEN DO:
  DYNAMIC-FUNCTION("DoLockWindow",?).
  RETURN ?.
END.
IF ihBrowse:X = ? THEN
  MESSAGE PROGRAM-NAME(1) SKIP
          hObjectSourceProc:FILE-NAME SKIP
          VIEW-AS ALERT-BOX.

CREATE FILL-IN hFillIn 
  ASSIGN FRAME            = ihBrowse:FRAME
         NAME             = icBrowseColumn
         DATA-TYPE        = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hBrowseColumn:NAME):DATA-TYPE
         FORMAT           = (IF icBuffersAndFields MATCHES "*9:9*" THEN icBuffersAndFields
                             ELSE IF bSetUseBrowseColumnFormat THEN hBrowseColumn:FORMAT 
                             ELSE ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hBrowseColumn:NAME):FORMAT)
         X                = ihBrowse:X + (IF NOT hBrowseColumn:VISIBLE THEN 2 ELSE hBrowseColumn:X) - 1
         Y                = ihBrowse:Y + (IF NOT hBrowseColumn:VISIBLE THEN 4 ELSE hBrowseColumn:Y) - 3
         WIDTH-PIXELS     = (IF NOT hBrowseColumn:VISIBLE THEN 1 ELSE hBrowseColumn:WIDTH-PIXELS) + 4
         HEIGHT-PIXELS    = MAX(10,ihBrowse:ROW-HEIGHT-PIXELS + 5) /* hBrowseColumn:HEIGHT-PIXELS + 1  */
         VISIBLE          = NO
         SENSITIVE        = TRUE
         TAB-STOP         = FALSE
         HELP             = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hBrowseColumn:NAME):HELP
         TOOLTIP          = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(hBrowseColumn:NAME):HELP
         TRIGGERS:
           ON ENTRY       PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"entry").
           ON LEAVE       RETURN NO-APPLY.
           ON RETURN      PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON "ctrl-s"    PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON CURSOR-DOWN PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON CURSOR-UP   PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON PAGE-DOWN   PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON PAGE-UP     PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON TAB         PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON BACK-TAB    PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON F3          PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"F3").
           ON F10         PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"F3").
           ON END-ERROR   PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"end-error").
         END TRIGGERS.

bSetUseBrowseColumnFormat = NO.

CREATE ttObject.
ASSIGN ttObject.hWindow       = ihBrowse:WINDOW
       ttObject.hObject       = hFillIn
       ttObject.cObjectType   = "fill-in"
       ttObject.hDesignObject = ihBrowse
       ttObject.cObjectName   = icBrowseColumn
       ttObject.hSourceProc   = hObjectSourceProc
       ttObject.cInitProc     = PROGRAM-NAME(2)
       ttObject.cGenProc      = PROGRAM-NAME(1)
       .
setAttribute(ttObject.hObject,"browsecolumn",icBrowseColumn).
setAttribute(ttObject.hObject,"browsecolumnhandle",STRING(hBrowseColumn)).
setAttribute(ttObject.hObject,"buffercolumn",icBufferColumn).
setAttribute(ttObject.hObject,"validation",icValidation).
IF NOT CAN-DO(getAttribute(ihBrowse,"currviewfields"),icBrowseColumn) 
  OR CAN-DO(getAttribute(ihBrowse,"noeditfields"),icBrowseColumn) THEN
  setAttribute(ttObject.hObject,"visible","no").

IF hFillIn:DATA-TYPE = "date" THEN DO:
  ON "t" OF hFillIn PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
  ON "d" OF hFillIn PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
  ON "+" OF hFillIn PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
  ON "-" OF hFillIn PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = hFillIn
         ttEvent.cAction       = "todays-date-key"
         ttEvent.cName         = "todays-date-key"
         ttEvent.cWidgetType   = "fill-in"
         ttEvent.cMethod       = "TodaysDateKey".

END.
/*ELSE IF icBuffersAndFields MATCHES "*9:9*" THEN DO:*/
  ON ANY-PRINTABLE OF hFillIn PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"any-printable").
  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = hFillIn
         ttEvent.cAction       = "any-printable"
         ttEvent.cName         = "any-printable"
         ttEvent.cWidgetType   = "fill-in"
         ttEvent.cMethod       = "AnyPrintableKey"
         .
 IF icBuffersAndFields MATCHES "*9:9*" THEN        
   icBuffersAndFields    = "".
/*END.*/

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "leave"
       ttEvent.cName         = "leave"
       ttEvent.cWidgetType   = "browse-fillin"
       ttEvent.cMethod       = "LeaveBrowseFillIn".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "entry"
       ttEvent.cName         = "entry"
       ttEvent.cWidgetType   = "browse-fillin"
       ttEvent.cMethod       = "EnterBrowseFillIn".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "F3"
       ttEvent.cName         = "apply lookup"
       ttEvent.cWidgetType   = "browse-fillin"
       ttEvent.cMethod       = "ApplyBrowseFillInLookup".

CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "end-error"
       ttEvent.cName          = "end-error"
       ttEvent.cWidgetType    = "browse-fillin"
       ttEvent.cMethod        = "EndErrorBrowseOverlay".

IF icBuffersAndFields NE "" AND NOT CAN-DO("Cal.w,dCal.w",icBuffersAndFields) THEN DO:
  CREATE BUTTON hButton
         ASSIGN FRAME         = ihBrowse:FRAME
                NAME          = "btnBrw_" + icBrowseColumn
                WIDTH-PIXELS  = 17
                TAB-STOP      = FALSE
                NO-FOCUS      = TRUE
                HEIGHT-PIXELS = MAX(10,ihBrowse:ROW-HEIGHT-PIXELS + 5)
                X             = hFillIn:X + hBrowseColumn:WIDTH-PIXELS - 18 
                Y             = hFillIn:Y + 2
                LABEL         = "..."
                SENSITIVE     = TRUE
                HIDDEN        = TRUE
         TRIGGERS:
           ON CHOOSE PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"lookup").
         END TRIGGERS.

  setAttribute(ttObject.hObject,"viewbuffersandfields",icBuffersAndFields).
  setAttribute(ttObject.hObject,"buffersandfields",icBuffersAndFields).
  setAttribute(ttObject.hObject,"querycriteria",icQueryCriteria).
  setAttribute(ttObject.hObject,"lookupattributes",icLookupAttrib).

  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = hButton
         ttEvent.cAction       = "choose"
         ttEvent.cName         = "lookup"
         ttEvent.cWidgetType   = "browse-lookup"
         ttEvent.cMethod       = "BrowseColumnLookup".
END.

ELSE IF CAN-DO("Cal.w,dCal.w",icBuffersAndFields) AND hFillIn:DATA-TYPE = "date" THEN DO:
  CREATE BUTTON hButton
         ASSIGN FRAME         = ihBrowse:FRAME
                WIDTH-PIXELS  = 17
                TAB-STOP      = FALSE
                NO-FOCUS      = TRUE
                HEIGHT-PIXELS = MAX(10,ihBrowse:ROW-HEIGHT-PIXELS + 5) 
                X             = hFillIn:X + hBrowseColumn:WIDTH-PIXELS - 18 
                Y             = hFillIn:Y + 2
                LABEL         = "..."
                SENSITIVE     = TRUE
                HIDDEN        = TRUE
         TRIGGERS:
           ON CHOOSE PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"lookup").
         END TRIGGERS.

  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = hButton
         ttEvent.cAction       = "choose"
         ttEvent.cName         = "lookup"
         ttEvent.cWidgetType   = "browse-lookup"
         ttEvent.cMethod       = "BrowseDateLookup" + (IF icBuffersAndFields = "dCal.w" THEN "Dialog" ELSE "").
END.

ASSIGN ihBrowse:FRAME:WIDTH-PIXELS  = iTmpXpixels
       ihBrowse:FRAME:HEIGHT-PIXELS = iTmpYpixels
       .

IF getAttribute(ihBrowse,"overlaybgcolnum") NE "" THEN 
  ttObject.hObject:BGCOLOR = INT(getAttribute(ihBrowse,"overlaybgcolnum")).


ASSIGN cNoDbWriteFields  = getAttribute(ihBrowse,"nodbwriteaccessfields")
       cNoDbWriteBuffers = getAttribute(ihBrowse,"nodbwriteaccessbuffers")
       .
IF CAN-DO(cNoDbWriteFields,icBufferColumn) 
   OR CAN-DO(cNoDbWriteBuffers,getAttribute(ihBrowse,"fieldbuffer" + icBufferColumn))
  THEN
  setAttribute(ttObject.hObject,"visible","no").

DYNAMIC-FUNCTION("DoLockWindow",?).

hObjectSourceProc = ?.

RETURN hFillIn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseSearchField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewBrowseSearchField Procedure 
FUNCTION NewBrowseSearchField RETURNS HANDLE
  ( INPUT ihRectSearchField  AS HANDLE,
    INPUT ihBrowse           AS HANDLE,
    INPUT iiColumn           AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Create new browse search field object and link it to the browse  
------------------------------------------------------------------------------*/
DEF VAR hFillIn     AS HANDLE NO-UNDO.
DEF VAR hField      AS HANDLE NO-UNDO.
DEF VAR hColumn     AS HANDLE NO-UNDO.
DEF VAR iVirtWidth  AS INT    NO-UNDO.
DEF VAR iVirtHeight AS INT    NO-UNDO.
DEF VAR iTmpXpixels AS INT    NO-UNDO.
DEF VAR iTmpYpixels AS INT    NO-UNDO.

ASSIGN ihRectSearchField:HIDDEN = TRUE
       iVirtWidth  = ihRectSearchField:WINDOW:VIRTUAL-WIDTH-PIXELS
       iVirtHeight = ihRectSearchField:WINDOW:VIRTUAL-HEIGHT-PIXELS
       .

ASSIGN iTmpXpixels      = ihRectSearchField:FRAME:WIDTH-PIXELS
       iTmpYpixels      = ihRectSearchField:FRAME:HEIGHT-PIXELS
       .

IF iiColumn NE 0 THEN DO:
  hColumn = ihBrowse:GET-BROWSE-COLUMN(iiColumn) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ?.
  hField = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD(ihBrowse:GET-BROWSE-COLUMN(iiColumn):NAME) NO-ERROR.
END.
ELSE RETURN ?.

IF VALID-HANDLE(hField) AND hField:DATA-TYPE = "LOGICAL" THEN DO:

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldTypeForLogicalSearchFields") = "radio-set" OR
    (DYNAMIC-FUNCTION("getAttribute",SESSION,"fieldTypeForLogicalSearchFields") = "radio-set" AND
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldTypeForLogicalSearchFields") = "")
     THEN DO:

    CREATE RADIO-SET hFillIn 
      ASSIGN DELIMITER        = "|"
             RADIO-BUTTONS    = "?|?|" + ENTRY(1,hField:FORMAT,"/") + "|true|" + ENTRY(2,hField:FORMAT,"/") + "|false"    
             HORIZONTAL       = YES
             FRAME            = ihRectSearchField:FRAME
             X                = ihRectSearchField:X /* + 2 */
             Y                = ihRectSearchField:Y + 2 
             WIDTH-PIXELS     = ihRectSearchField:WIDTH-PIXELS 
             VISIBLE          = TRUE
             SENSITIVE        = TRUE
             HELP             = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Søk " ELSE "Search on ") 
                              + DYNAMIC-FUNCTION("getStrippedSortLabel",hColumn) 
             TRIGGERS:
               ON VALUE-CHANGED           PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"return").
               ON TAB                     PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"tab").
               ON ctrl-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-tab").
               ON BACK-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"back-tab").
               ON ENTRY                   PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"entry-of-widget").
               ON ctrl-CURSOR-RIGHT       PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-right").
               ON ctrl-CURSOR-LEFT        PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-left").
               ON shift-ctrl-CURSOR-RIGHT PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-right").
               ON shift-ctrl-CURSOR-LEFT  PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-left").
               ON alt-CURSOR-DOWN         PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"alt-cursor-down").
             END TRIGGERS.
    hFillIn:SCREEN-VALUE = ? NO-ERROR.
  END.
  ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldTypeForLogicalSearchFields") = "combo-box" OR
    (DYNAMIC-FUNCTION("getAttribute",SESSION,"fieldTypeForLogicalSearchFields") = "combo-box" AND
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldTypeForLogicalSearchFields") = "")
     THEN DO:

    CREATE COMBO-BOX hFillIn 
      ASSIGN DELIMITER        = "|"
             DATA-TYPE        = "LOGICAL"
             SUBTYPE          = "DROP-DOWN-LIST"
             LIST-ITEM-PAIRS  = ENTRY(1,hField:FORMAT,"/") + "|true|" + ENTRY(2,hField:FORMAT,"/") + "|false"
             INNER-LINES      = 3
             FRAME            = ihRectSearchField:FRAME
             X                = ihRectSearchField:X
             Y                = ihRectSearchField:Y
             WIDTH-PIXELS     = ihRectSearchField:WIDTH-PIXELS 
             VISIBLE          = TRUE
             SENSITIVE        = TRUE
             HELP             = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Søk " ELSE "Search on ") 
                              + DYNAMIC-FUNCTION("getStrippedSortLabel",hColumn) 
             TRIGGERS:
               ON VALUE-CHANGED           PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"return").
               ON TAB                     PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"tab").
               ON ctrl-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-tab").
               ON BACK-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"back-tab").
               ON ENTRY                   PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"entry-of-widget").
               ON ctrl-CURSOR-RIGHT       PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-right").
               ON ctrl-CURSOR-LEFT        PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-left").
               ON shift-ctrl-CURSOR-RIGHT PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-right").
               ON shift-ctrl-CURSOR-LEFT  PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-left").
               ON alt-CURSOR-DOWN         PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"alt-cursor-down").
             END TRIGGERS.
    hFillIn:SCREEN-VALUE = "false" NO-ERROR.
  END.
  ELSE DO:
    CREATE TOGGLE-BOX hFillIn 
    ASSIGN FRAME            = ihRectSearchField:FRAME
           X                = ihRectSearchField:X + 2
           Y                = ihRectSearchField:Y + 2
           WIDTH-PIXELS     = ihRectSearchField:WIDTH-PIXELS 
           FORMAT           = hField:FORMAT
           LABEL            = hField:LABEL
           VISIBLE          = TRUE
           SENSITIVE        = TRUE
           HELP             = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Søk " ELSE "Search on ") 
                            + DYNAMIC-FUNCTION("getStrippedSortLabel",hColumn) 
           TRIGGERS:
             ON VALUE-CHANGED           PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"return").
             ON TAB                     PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"tab").
             ON ctrl-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-tab").
             ON BACK-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"back-tab").
             ON ENTRY                   PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"entry-of-widget").
             ON ctrl-CURSOR-RIGHT       PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-right").
             ON ctrl-CURSOR-LEFT        PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-left").
             ON shift-ctrl-CURSOR-RIGHT PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-right").
             ON shift-ctrl-CURSOR-LEFT  PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-left").
             ON alt-CURSOR-DOWN         PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"alt-cursor-down").
           END TRIGGERS.
    hFillIn:SCREEN-VALUE = STRING(FALSE) NO-ERROR.
  END.      
END.
ELSE DO:           
  CREATE FILL-IN hFillIn 
    ASSIGN DATA-TYPE         = IF iiColumn NE 0 THEN hField:DATA-TYPE ELSE "CHARACTER"
           FRAME             = ihRectSearchField:FRAME
           X                 = ihRectSearchField:X
           Y                 = ihRectSearchField:Y
           WIDTH-PIXELS      = ihRectSearchField:WIDTH-PIXELS 
           FORMAT            = IF iiColumn NE 0 THEN hField:FORMAT ELSE "x(50)"
           VISIBLE           = YES
           SENSITIVE         = YES
           HELP              = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Søk " ELSE "Search on ")
                             + DYNAMIC-FUNCTION("getStrippedSortLabel",hColumn) 
           TRIGGERS:
             ON RETURN                  PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"return").
             ON TAB                     PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"tab").
             ON ctrl-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-tab").
             ON ctrl-shift-TAB          PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-back-tab").
             ON BACK-TAB                PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"back-tab").
             ON ANY-PRINTABLE           PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"any-printable").
             ON BACKSPACE               PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"backspace").
             ON DELETE-CHARACTER        PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"delete-character").
             ON ENTRY                   PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"entry-of-widget").
             ON ctrl-CURSOR-RIGHT       PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-right").
             ON ctrl-CURSOR-LEFT        PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"ctrl-cursor-left").
             ON shift-ctrl-CURSOR-RIGHT PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-right").
             ON shift-ctrl-CURSOR-LEFT  PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"shift-ctrl-cursor-left").
             ON alt-CURSOR-DOWN         PERSISTENT RUN DoProcessEvent (IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE,"alt-cursor-down").
           END TRIGGERS.


  hFillIn:SCREEN-VALUE = "".
END.
hFillIn:MOVE-BEFORE(ihBrowse).

hFillIn:NAME = hField:NAME.

IF ihRectSearchField:FRAME:TYPE NE "dialog-box" THEN
  ASSIGN ihRectSearchField:FRAME:WIDTH-PIXELS  = iTmpXpixels
         ihRectSearchField:FRAME:HEIGHT-PIXELS = iTmpYpixels
         NO-ERROR.

PUBLISH "NeverResize" (hFillIn,"X").
IF getAttribute(ihRectSearchField,"allowMoveX") NE "yes" THEN DO:
  PUBLISH "NeverMove" (ihRectSearchField,"X"). 
  PUBLISH "NeverMove" (hFillIn,"X").
END.
IF getAttribute(ihRectSearchField,"allowMoveY") NE "yes" THEN DO:
  PUBLISH "NeverMove" (ihRectSearchField,"Y"). 
  PUBLISH "NeverMove" (hFillIn,"Y").
END.
PUBLISH "NeverResize" (ihRectSearchField,"X").

CREATE ttObject.
ASSIGN ttObject.hWindow       = ihRectSearchField:WINDOW
       ttObject.hObject       = hFillIn
       ttObject.cObjectType   = "browse-search-field"
       ttObject.hDesignObject = ihRectSearchField
       ttObject.cObjectName   = ihRectSearchField:NAME
       ttObject.hSourceProc   = IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE
       ttObject.cInitProc     = PROGRAM-NAME(2)
       ttObject.cGenProc      = PROGRAM-NAME(1)
       .

IF hFillIn:DATA-TYPE = "date" THEN DO:
  ON "t" OF hFillIn PERSISTENT  RUN DoProcessEvent (SOURCE-PROCEDURE,"todays-date-key").
  ON "d" OF hFillIn PERSISTENT  RUN DoProcessEvent (SOURCE-PROCEDURE,"todays-date-key").
  ON "+" OF hFillIn PERSISTENT  RUN DoProcessEvent (SOURCE-PROCEDURE,"todays-date-key").
  ON "-" OF hFillIn PERSISTENT  RUN DoProcessEvent (SOURCE-PROCEDURE,"todays-date-key").
  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = hFillIn
         ttEvent.cAction       = "todays-date-key"
         ttEvent.cName         = "todays-date-key"
         ttEvent.cWidgetType   = "fill-in"
         ttEvent.cMethod       = "TodaysDateKey".
END.

CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "return"
       ttEvent.cName          = "return"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "StartSortSearch"
       ttEvent.bReturnNoApply = TRUE
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "tab"
       ttEvent.cName          = "tab"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "TabFromSortSearch"
       ttEvent.bReturnNoApply = TRUE
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "back-tab"
       ttEvent.cName          = "back-tab"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "BackTabFromSortSearch"
       ttEvent.bReturnNoApply = TRUE
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "any-printable"
       ttEvent.cName          = "any-printable"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "AnyPrintableSortSearch"
       ttEvent.bReturnNoApply = YES
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "backspace"
       ttEvent.cName          = "backspace"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "BackSpaceSortSearch"
       ttEvent.bReturnNoApply = YES
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "delete-character"
       ttEvent.cName          = "delete-character"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "DeleteCharSortSearch"
       ttEvent.bReturnNoApply = YES
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "ctrl-tab"
       ttEvent.cName          = "ctrl-tab"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "CtrlTabSortSearch"
       ttEvent.bReturnNoApply = TRUE
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject        = ttObject.hObject
       ttEvent.hWidget        = hFillIn
       ttEvent.cAction        = "ctrl-back-tab"
       ttEvent.cName          = "ctrl-back-tab"
       ttEvent.cWidgetType    = "browse-search-field"
       ttEvent.cMethod        = "CtrlBackTabSortSearch"
       ttEvent.bReturnNoApply = TRUE
       .
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "entry-of-widget"
       ttEvent.cName         = "entry-of-widget"
       ttEvent.cWidgetType   = "button"
       ttEvent.cMethod       = "EntryOfWidget".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "ctrl-cursor-right"
       ttEvent.cName         = "ctrl-cursor-right"
       ttEvent.cWidgetType   = "browse-search-field"
       ttEvent.cMethod       = "CtrlCursorRightSortSearch".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "ctrl-cursor-left"
       ttEvent.cName         = "ctrl-cursor-left"
       ttEvent.cWidgetType   = "browse-search-field"
       ttEvent.cMethod       = "CtrlCursorLeftSortSearch".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "shift-ctrl-cursor-right"
       ttEvent.cName         = "shift-ctrl-cursor-right"
       ttEvent.cWidgetType   = "browse-search-field"
       ttEvent.cMethod       = "ShiftCtrlCursorRightSortSearch".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "shift-ctrl-cursor-left"
       ttEvent.cName         = "shift-ctrl-cursor-left"
       ttEvent.cWidgetType   = "browse-search-field"
       ttEvent.cMethod       = "ShiftCtrlCursorLeftSortSearch".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "alt-cursor-down"
       ttEvent.cName         = "alt-cursor-down"
       ttEvent.cWidgetType   = "browse-search-field"
       ttEvent.cMethod       = "AltCursorDownSortSearch".
hObjectSourceProc = ?.

RETURN hFillIn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewBrowseToggle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewBrowseToggle Procedure 
FUNCTION NewBrowseToggle RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icOtherProp        AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Fields prefixed with ! are added to the buffer but not to the grid  
------------------------------------------------------------------------------*/
DEF VAR hToggleBox        AS HANDLE NO-UNDO.
DEF VAR hBrowseColumn     AS HANDLE NO-UNDO.
DEF VAR iTmpXpixels       AS INT NO-UNDO.
DEF VAR iTmpYpixels       AS INT NO-UNDO.
DEF VAR cNoDbWriteFields  AS CHAR   NO-UNDO.
DEF VAR cNoDbWriteBuffers AS CHAR   NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",ihBrowse:WINDOW).

ASSIGN iTmpXpixels = ihBrowse:FRAME:WIDTH-PIXELS
       iTmpYpixels = ihBrowse:FRAME:HEIGHT-PIXELS
       hObjectSourceProc = (IF hObjectSourceProc = ? THEN SOURCE-PROCEDURE ELSE hObjectSourceProc)
       .

IF icBufferColumn = "" THEN icBufferColumn = icBrowseColumn.

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  hBrowseColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
  IF hBrowseColumn:NAME = icBrowseColumn THEN
    LEAVE.
END.

CREATE TOGGLE-BOX hToggleBox 
  ASSIGN FRAME            = ihBrowse:FRAME
         LABEL            = ""
         NAME             = icBrowseColumn
         X                = ihBrowse:X + (IF NOT hBrowseColumn:VISIBLE THEN 1 ELSE hBrowseColumn:X) 
         Y                = ihBrowse:Y + (IF NOT hBrowseColumn:VISIBLE THEN 1 ELSE hBrowseColumn:Y)
         WIDTH-PIXELS     = (IF NOT hBrowseColumn:VISIBLE THEN 10 ELSE hBrowseColumn:WIDTH-PIXELS - 1) 
         HEIGHT-PIXELS    = ihBrowse:ROW-HEIGHT-PIXELS + 3 /* hBrowseColumn:HEIGHT-PIXELS - 1 */
         VISIBLE          = NO
         SENSITIVE        = TRUE
         TAB-STOP         = FALSE
         BGCOLOR          = 15
         HELP             = hBrowseColumn:HELP
         TOOLTIP          = hBrowseColumn:HELP
         TRIGGERS:
           ON VALUE-CHANGED PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"value-changed").
           ON CURSOR-DOWN   PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON CURSOR-UP     PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON TAB           PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON BACK-TAB      PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON RETURN        PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave").
           ON ENTRY         PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"entry").
         END TRIGGERS.

CREATE ttObject.
ASSIGN ttObject.hWindow       = ihBrowse:WINDOW
       ttObject.hObject       = hToggleBox
       ttObject.cObjectType   = "toggle-box"
       ttObject.hDesignObject = ihBrowse
       ttObject.cObjectName   = icBrowseColumn
       ttObject.hSourceProc   = hObjectSourceProc
       ttObject.cInitProc     = PROGRAM-NAME(2)
       ttObject.cGenProc      = PROGRAM-NAME(1)
       .
setAttribute(ttObject.hObject,"browsecolumn",icBrowseColumn).
setAttribute(ttObject.hObject,"buffercolumn",icBufferColumn).
IF NOT CAN-DO(getAttribute(ihBrowse,"currviewfields"),icBrowseColumn) 
  OR CAN-DO(getAttribute(ihBrowse,"noeditfields"),icBrowseColumn) THEN
  setAttribute(ttObject.hObject,"visible","no").

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hToggleBox
       ttEvent.cAction       = "value-changed"
       ttEvent.cName         = "value-changed"
       ttEvent.cWidgetType   = "browse-toggle"
       ttEvent.cMethod       = "ValChngBrowseToggle".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hToggleBox
       ttEvent.cAction       = "leave"
       ttEvent.cName         = "leave"
       ttEvent.cWidgetType   = "browse-toggle"
       ttEvent.cMethod       = "LeaveBrowseToggle".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hToggleBox
       ttEvent.cAction       = "entry"
       ttEvent.cName         = "entry"
       ttEvent.cWidgetType   = "browse-toggle"
       ttEvent.cMethod       = "EnterBrowseToggle".

ASSIGN ihBrowse:FRAME:WIDTH-PIXELS  = iTmpXpixels
       ihBrowse:FRAME:HEIGHT-PIXELS = iTmpYpixels
       .

IF getAttribute(ihBrowse,"overlaybgcolnum") NE "" THEN 
  ttObject.hObject:BGCOLOR = INT(getAttribute(ihBrowse,"overlaybgcolnum")).


ASSIGN cNoDbWriteFields  = getAttribute(ihBrowse,"nodbwriteaccessfields")
       cNoDbWriteBuffers = getAttribute(ihBrowse,"nodbwriteaccessbuffers")
       .
IF CAN-DO(cNoDbWriteFields,icBufferColumn) 
   OR CAN-DO(cNoDbWriteBuffers,getAttribute(ihBrowse,"fieldbuffer" + icBufferColumn))
  THEN
  setAttribute(ttObject.hObject,"visible","no").

DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN hToggleBox.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewDynFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewDynFilter Procedure 
FUNCTION NewDynFilter RETURNS HANDLE
  ( INPUT ihFilterQryObject      AS HANDLE,
    INPUT ihFilterButton         AS HANDLE,
    INPUT icOtherProp            AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Create an object to hold information on filter fields and their corresponding values.
  Notes:   - The values are stored as attributes on the corresponding query 
           - The filterbutton is hence just a placeholder for the filter while the filter window is active
------------------------------------------------------------------------------*/
DEF VAR hFilter   AS HANDLE NO-UNDO.
DEF VAR hQuery    AS HANDLE NO-UNDO.
DEF VAR hWindow   AS HANDLE NO-UNDO.
DEF VAR cInitProc AS CHAR   NO-UNDO.

hFilter = ihFilterButton.

IF ihFilterQryObject:TYPE = "query" THEN
  ASSIGN hQuery    = ihFilterQryObject
         hWindow   = IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc:CURRENT-WINDOW ELSE WIDGET-HANDLE(getAttribute(SESSION,"current-window"))
         cInitProc = PROGRAM-NAME(3).
ELSE
  ASSIGN hQuery    = ihFilterQryObject:QUERY
         hWindow   = ihFilterQryObject:WINDOW
         cInitProc = PROGRAM-NAME(2).

FIND FIRST ttObject WHERE ttObject.hObject = hFilter NO-ERROR.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = hWindow
         ttObject.hObject       = hFilter
         ttObject.cObjectType   = "DynFilter"
         ttObject.hDesignObject = ihFilterQryObject
         ttObject.hSourceProc   = IF VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc ELSE SOURCE-PROCEDURE
         ttObject.cObjectName   = ihFilterQryObject:NAME
         ttObject.cInitProc     = cInitProc
         ttObject.cGenProc      = PROGRAM-NAME(1)
         .
END.

IF VALID-HANDLE(ihFilterButton) THEN
  setAttribute(hFilter,"filterbutton",STRING(ihFilterButton)).

hObjectSourceProc = ?.

RETURN hFilter.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewEvent Procedure 
FUNCTION NewEvent RETURNS LOGICAL
  ( INPUT ihObject     AS HANDLE,
    INPUT ihWidget     AS HANDLE,
    INPUT icAction     AS CHAR,
    INPUT icName       AS CHAR,
    INPUT icWidgetType AS CHAR,
    INPUT icMethod     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: The name of this function is REALLY misguiding 
------------------------------------------------------------------------------*/
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ihObject
       ttEvent.hWidget       = ihWidget
       ttEvent.cAction       = icAction
       ttEvent.cName         = icName
       ttEvent.cWidgetType   = icWidgetType
       ttEvent.cMethod       = icMethod.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewFieldMap Procedure 
FUNCTION NewFieldMap RETURNS HANDLE
  ( INPUT ihQuery               AS HANDLE,
    INPUT ihFrame               AS HANDLE,
    INPUT icBufferUpdateFields  AS CHAR,
    INPUT icScreenUpdateFields  AS CHAR,
    INPUT icBufferDisplayFields AS CHAR,
    INPUT icScreenDisplayFields AS CHAR,
    INPUT icOtherProp           AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Create object to hold links between query buffer fields and corr. input and displ fields
    Notes: Handle to field map is the buffer handle 
           FieldMap can also be used for browse overlays to enable save of multiple values:
               ENTRY(2,icOtherProp,";") MATCHES "*UseForBrwOverlays*"
               In this case triggers are created for the individual fields
 ------------------------------------------------------------------------------*/
DEF VAR hFieldMap           AS HANDLE NO-UNDO.
DEF VAR hWidget             AS HANDLE NO-UNDO.
DEF VAR cInpWidgets         AS CHAR NO-UNDO.
DEF VAR cDispWidgets        AS CHAR NO-UNDO.
DEF VAR cExtraUpdateFields  AS CHAR NO-UNDO.
DEF VAR cExtraUpdateWidgets AS CHAR NO-UNDO.
DEF VAR cAllUpdateFields    AS CHAR NO-UNDO.
DEF VAR cTimeInputFlds      AS CHAR NO-UNDO.
DEF VAR bUseForBrwOverlays  AS LOG  NO-UNDO.

IF hObjectSourceProc = ? THEN hObjectSourceProc = SOURCE-PROCEDURE.

IF VALID-HANDLE(ihQuery) THEN DO:
  IF ihQuery:TYPE = "browse" THEN ihQuery = ihQuery:QUERY.
  
  hFieldMap = ihQuery:GET-BUFFER-HANDLE(1).
END.
ELSE hFieldMap = ihFrame.  /* To be able to set define the fieldmap without knowledge of the query */

FIND FIRST ttObject WHERE ttObject.hObject = hFieldMap NO-ERROR.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihFrame:WINDOW
         ttObject.hObject       = hFieldMap
         ttObject.cObjectType   = "fieldMap"
         ttObject.cObjectName   = hFieldMap:NAME
         ttObject.hDesignObject = ihFrame
         ttObject.hSourceProc   = hObjectSourceProc
         ttObject.cInitProc     = PROGRAM-NAME(2)
         ttObject.cGenProc      = PROGRAM-NAME(1)
         .

  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = ttObject.hObject
         ttEvent.cAction       = "new-record"
         ttEvent.cName         = "new-record"
         ttEvent.cWidgetType   = "object"
         ttEvent.cMethod       = "NewRecord".
  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = ttObject.hObject
         ttEvent.cAction       = "save-record"
         ttEvent.cName         = "save-record"
         ttEvent.cWidgetType   = "object"
         ttEvent.cMethod       = "SaveRecord".
END.

ASSIGN cExtraUpdateFields    = ENTRY(1,icOtherProp,";")
       icScreenUpdateFields  = IF icScreenUpdateFields NE "" THEN icScreenUpdateFields ELSE icBufferUpdateFields
       icScreenDisplayFields = IF icScreenDisplayFields NE "" THEN icScreenDisplayFields ELSE icBufferDisplayFields
       bUseForBrwOverlays    = NUM-ENTRIES(icOtherProp,";") > 1 AND ENTRY(2,icOtherProp,";") MATCHES "*UseForBrwOverlays*"
       .

setAttribute(hFieldMap,"BufferUpdateFields",icBufferUpdateFields).
setAttribute(hFieldMap,"ScreenUpdateFields",icScreenUpdateFields).
setAttribute(hFieldMap,"BufferDisplayFields",icBufferDisplayFields).
setAttribute(hFieldMap,"ScreenDisplayFields",icScreenDisplayFields).
setAttribute(hFieldMap,"ExtraUpdateFields",cExtraUpdateFields).
setAttribute(hFieldMap,"checkmodified","no").

cAllUpdateFields = TRIM(icScreenUpdateFields + "," + cExtraUpdateFields,",").

EMPTY TEMP-TABLE ttSort1.
EMPTY TEMP-TABLE ttSort2.
DO ix = 1 TO NUM-ENTRIES(icScreenUpdateFields):
  CREATE ttSort1.
  ASSIGN ttSort1.iSeq   = ix
         ttSort1.cText1 = ENTRY(ix,icScreenUpdateFields).
END.
DO ix = 1 TO NUM-ENTRIES(icScreenDisplayFields):
  CREATE ttSort2.
  ASSIGN ttSort2.iSeq   = ix
         ttSort2.cText1 = ENTRY(ix,icScreenDisplayFields).
END.

hWidget = ihFrame:FIRST-CHILD:FIRST-CHILD.
REPEAT WHILE hWidget NE ?:
  
  IF CAN-FIND(FIRST ttObject WHERE ttObject.hObject = hWidget AND ttObject.cObjectType = "browse-search-field") THEN DO:
    IF CAN-QUERY(hWidget,"next-sibling") THEN 
      hWidget = hWidget:NEXT-SIBLING.
    ELSE hWidget = ?.
    NEXT.
  END.

  IF CAN-DO(cAllUpdateFields,hWidget:NAME) THEN DO:

    IF CAN-DO(icScreenUpdateFields,hWidget:NAME) THEN DO:
      FIND FIRST ttSort1 WHERE ttSort1.cText1 = hWidget:NAME.
      ttSort1.cText2 = STRING(hWidget).
    END.
 
    IF hWidget:TYPE = "fill-in" OR (CAN-QUERY(hWidget,"SUBTYPE") AND hWidget:SUBTYPE = "drop-down") THEN DO:
      ON ANY-PRINTABLE    OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
      IF NOT bUseForBrwOverlays THEN
        ON LEAVE OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave-widget").
      ELSE  
        ON LEAVE OF hWidget RETURN NO-APPLY.
        
      IF hWidget:TYPE = "fill-in" THEN DO:
        ON ctrl-v           OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        IF hWidget:DATA-TYPE BEGINS "DATE" THEN DO:
          ON BACKSPACE OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"clear-date-time").
          ON DELETE-CHARACTER OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"clear-date-time").
          ON " " OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"clear-date-time").
          ON "t" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "d" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "+" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          ON "-" OF hWidget PERSISTENT  RUN DoProcessEvent (hObjectSourceProc,"todays-date-key").
          CREATE ttEvent.
          ASSIGN ttEvent.hObject     = ttObject.hObject
                 ttEvent.hWidget     = hWidget
                 ttEvent.cAction     = "todays-date-key"
                 ttEvent.cName       = "todays-date-key"
                 ttEvent.cWidgetType = "fill-in"
                 ttEvent.cMethod     = "TodaysDateKey".
        END.
        ELSE DO:
          ON BACKSPACE        OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
          ON DELETE-CHARACTER OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        END.  
      END.
      ELSE IF NOT bUseForBrwOverlays THEN DO:
        ON VALUE-CHANGED OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"any-printable").
        hWidget:BGCOLOR = 15.
      END.

      IF hWidget:DATA-TYPE BEGINS "date" THEN DO:
        CREATE ttEvent.
        ASSIGN ttEvent.hObject       = ttObject.hObject
               ttEvent.hWidget       = hWidget
               ttEvent.cAction       = "clear-date-time"
               ttEvent.cName         = "clear-date-time"
               ttEvent.cWidgetType   = "fill-in"
               ttEvent.cMethod       = "ClearDateTime".
      END.

      IF NOT bUseForBrwOverlays THEN DO:       
        CREATE ttEvent.
        ASSIGN ttEvent.hObject       = ttObject.hObject
               ttEvent.hWidget       = hWidget
               ttEvent.cAction       = "any-printable"
               ttEvent.cName         = "any-printable"
               ttEvent.cWidgetType   = "fill-in"
               ttEvent.cMethod       = "AnyPrintableKey".
        CREATE ttEvent.
        ASSIGN ttEvent.hObject       = ttObject.hObject
               ttEvent.hWidget       = hWidget
               ttEvent.cAction       = "leave-widget"
               ttEvent.cName         = "leave-widget"
               ttEvent.cWidgetType   = "fill-in"
               ttEvent.cMethod       = "LeaveWidget".
      END.         

      IF hWidget:FORMAT = "99:99" OR hWidget:FORMAT = "99:99:99" THEN
        cTimeInputFlds = cTimeInputFlds + (IF cTimeInputFlds NE "" THEN "," ELSE "") + hWidget:NAME.

      IF CAN-DO(cExtraUpdateFields,"btn" + hWidget:NAME) THEN DO:
        ON F3 OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"f3-widget").
        CREATE ttEvent.
        ASSIGN ttEvent.hObject       = ttObject.hObject
               ttEvent.hWidget       = hWidget
               ttEvent.cAction       = "f3-widget"
               ttEvent.cName         = "f3-widget"
               ttEvent.cWidgetType   = "fill-in"
               ttEvent.cMethod       = "F3Widget".
      END.
    END.
    ELSE IF NOT bUseForBrwOverlays THEN DO:
      ON VALUE-CHANGED OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"value-changed-widget").
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "value-changed-widget"
             ttEvent.cName         = "value-changed-widget"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "ValueChangedWidget".
      IF hWidget:TYPE = "combo-box" THEN
        hWidget:BGCOLOR = 15.
      IF hWidget:TYPE = "editor" THEN DO:
        ON LEAVE OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"leave-widget").
        CREATE ttEvent.
        ASSIGN ttEvent.hObject       = ttObject.hObject
               ttEvent.hWidget       = hWidget
               ttEvent.cAction       = "leave-widget"
               ttEvent.cName         = "leave-widget"
               ttEvent.cWidgetType   = "fill-in"
               ttEvent.cMethod       = "LeaveWidget".
      END.
    END.

    IF NOT bUseForBrwOverlays AND bTabOnReturn AND hWidget:TYPE NE "editor" THEN DO:
      ON RETURN OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"return-of-widget").
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "return-of-widget"
             ttEvent.cName         = "return-of-widget"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "ReturnOfWidget".
    END.
  
    IF getAttribute(SESSION,"NoFieldEntryTriggers") NE "yes" AND NOT bUseForBrwOverlays THEN DO:
/*    IF NOT bUseForBrwOverlays AND getAttribute(hWidget:WINDOW,"HelpTextWidget") NE "" THEN DO:*/
      ON ENTRY OF hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"entry-of-widget").
      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "entry-of-widget"
             ttEvent.cName         = "entry-of-widget"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "EntryOfWidget".
    END.
  END.

  IF CAN-DO(icScreenDisplayFields,hWidget:NAME) THEN DO:
    IF CAN-DO("editor,fill-in",hWidget:TYPE) THEN
      ASSIGN hWidget:SENSITIVE = TRUE
             hWidget:READ-ONLY = TRUE.
    ELSE hWidget:SENSITIVE = FALSE.

    hWidget:TAB-STOP  = FALSE.
    FIND FIRST ttSort2 WHERE ttSort2.cText1 = hWidget:NAME.
    ttSort2.cText2 = STRING(hWidget).
  END.
  ELSE IF CAN-DO(cExtraUpdateFields,hWidget:NAME) THEN 
    cExtraUpdateWidgets = cExtraUpdateWidgets + STRING(hWidget) + ",".
  IF CAN-QUERY(hWidget,"next-sibling") THEN 
    hWidget = hWidget:NEXT-SIBLING.
  ELSE hWidget = ?.
END.

FOR EACH ttSort1:
  cInpWidgets = cInpWidgets + ttSort1.cText2 + ",".
END.
FOR EACH ttSort2:
  cDispWidgets = cDispWidgets + ttSort2.cText2 + ",".
END.

ASSIGN cInpWidgets = TRIM(cInpWidgets,",")
       cDispWidgets = TRIM(cDispWidgets,",").
setAttribute(hFieldMap,"ScreenUpdateWidgets",cInpWidgets).
setAttribute(hFieldMap,"ScreenDisplayWidgets",cDispWidgets).
IF cExtraUpdateWidgets NE "" THEN
  setAttribute(hFieldMap,"ExtraUpdateWidgets",TRIM(cExtraUpdateWidgets,",")).
IF cTimeInputFlds NE "" THEN
  setAttribute(hFieldMap,"TimeInputFields",cTimeInputFlds).

IF NUM-ENTRIES(cInpWidgets) NE NUM-ENTRIES(icBufferUpdateFields) THEN 
  MESSAGE "Number of update fields in frame doesn't match number of update fields in buffer" SKIP
          "(Could also be the spelling of first screen field)" SKIP(1)
          "Input widgets: " SKIP cInpWidgets SKIP(1)
          "Buffer update widgets: " SKIP icBufferUpdateFields
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".
ELSE IF NUM-ENTRIES(cDispWidgets) NE NUM-ENTRIES(icBufferDisplayFields) THEN
  MESSAGE "Number of display fields in frame doesn't match number of display fields in buffer" SKIP
          "(Could also be the spelling of first screen field)" SKIP(1)
          "Display widgets: " SKIP cDispWidgets SKIP(1)
          "Buffer display widgets: " SKIP icBufferDisplayFields
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".

hObjectSourceProc = ?.

RETURN hFieldMap.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewFilter Procedure 
FUNCTION NewFilter RETURNS HANDLE
  ( INPUT ihQuery                AS HANDLE,
    INPUT ihFrame                AS HANDLE,
    INPUT icInputFieldList       AS CHAR,
    INPUT icBufferFieldList      AS CHAR,
    INPUT icOperatorList         AS CHAR,
    INPUT icOperatorFieldList    AS CHAR,
    INPUT ibSearchOnValueChanged AS LOG,
    INPUT ihFilterButton         AS HANDLE,
    INPUT icOtherProp            AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Create an object to hold information on filter fields and their corresponding values.
  Notes:   - The values are stored as attributes on the corresponding query 
           - If the bufferfield list is empty (or entries in the list are empty)
             the bufferfield are considered the same as the input fields
           - If the operatorlist is empty (or entries in the list are empty)
             the operator is assumed based on the datatype of the INPUT field
------------------------------------------------------------------------------*/
DEF VAR hFilter             AS HANDLE NO-UNDO.
DEF VAR hWidget             AS HANDLE NO-UNDO.

hFilter = ihFrame.

FIND FIRST ttObject WHERE ttObject.hObject = hFilter NO-ERROR.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihFrame:WINDOW
         ttObject.hObject       = hFilter
         ttObject.cObjectType   = "Filter"
         ttObject.hDesignObject = ihFrame
         ttObject.cObjectName   = ihFrame:NAME
         ttObject.hSourceProc   = SOURCE-PROCEDURE
         ttObject.cInitProc     = PROGRAM-NAME(2)
         ttObject.cGenProc      = PROGRAM-NAME(1)
         .
END.

IF icOperatorList NE "" AND NUM-ENTRIES(icOperatorList) NE NUM-ENTRIES(icInputFieldList) THEN DO:
  MESSAGE "Number of entries in operatorlist doesn't match number of fields in filter definition" SKIP(1)
          "Field list: " SKIP icInputFieldList SKIP(1)
          "Operator list: " SKIP icOperatorList
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".
  RETURN ?.
END.
IF icBufferFieldList NE "" AND NUM-ENTRIES(icBufferFieldList) NE NUM-ENTRIES(icInputFieldList) THEN DO:
  MESSAGE "Number of entries in bufferfield list doesn't match number of fields in filter definition" SKIP(1)
          "Field list: " SKIP icInputFieldList SKIP(1)
          "Operator list: " SKIP icBufferFieldList
          VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".
  RETURN ?.
END.

hWidget = ihFrame:FIRST-CHILD:FIRST-CHILD.
REPEAT WHILE hWidget NE ?:
  IF CAN-DO(icInputFieldList,hWidget:NAME) AND NOT CAN-FIND(FIRST ttObjectLink WHERE ttObjectLink.hFromObject = hWidget AND ttObjectLink.cLinkType = "browse") THEN DO:
    hWidget:SCREEN-VALUE = getAttribute(ihQuery,"filtervalue_" + hWidget:NAME).
    setAttribute(ttObject.hObject,"filterfield_" + hWidget:NAME,STRING(hWidget)).
    setAttribute(ttObject.hObject,"bufferfield_" + hWidget:NAME,
                 IF icBufferFieldList = "" THEN hWidget:NAME 
                 ELSE ENTRY(LOOKUP(hWidget:NAME,icInputFieldList),icBufferFieldList)).
    setAttribute(ttObject.hObject,"operator_" + hWidget:NAME,
                 IF icOperatorList = "" THEN "" 
                 ELSE ENTRY(LOOKUP(hWidget:NAME,icInputFieldList),icOperatorList)).

    IF ibSearchOnValueChanged THEN DO:
      IF hWidget:TYPE = "fill-in" THEN 
        ON RETURN    OF hWidget PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"execute-filter").  
      ELSE 
        ON VALUE-CHANGED OF hWidget PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"execute-filter").

      CREATE ttEvent.
      ASSIGN ttEvent.hObject       = ttObject.hObject
             ttEvent.hWidget       = hWidget
             ttEvent.cAction       = "execute-filter"
             ttEvent.cName         = "execute-filter"
             ttEvent.cWidgetType   = "fill-in"
             ttEvent.cMethod       = "StartFilterRecord".
    END.
  END.
  ELSE IF CAN-DO(icOperatorFieldList,hWidget:NAME) THEN DO:
    hWidget:SCREEN-VALUE = getAttribute(ihQuery,"operatorInUse_" + ENTRY(LOOKUP(hWidget:NAME,icOperatorFieldList),icInputFieldList)).
    setAttribute(ihQuery,"operatorField_" + ENTRY(LOOKUP(hWidget:NAME,icOperatorFieldList),icInputFieldList),STRING(hWidget)).
  END.

  IF CAN-QUERY(hWidget,"next-sibling") THEN 
    hWidget = hWidget:NEXT-SIBLING.
  ELSE hWidget = ?.
END.

IF VALID-HANDLE(ihFilterButton) THEN DO:
  setAttribute(hFilter,"activefilterbutton",getAttribute(SESSION,"ActiveFilterButton")).
  setAttribute(hFilter,"passivefilterbutton",getAttribute(SESSION,"PassiveFilterButton")).
  setAttribute(hFilter,"filterbutton",STRING(ihFilterButton)).
END.

RETURN hFilter.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewMenuBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewMenuBand Procedure 
FUNCTION NewMenuBand RETURNS HANDLE
  ( INPUT ihParent     AS HANDLE,
    INPUT icActionList AS CHAR,
    INPUT icOtherProp  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Add a series of actions as a popup-menu, extending a sub-menu or as a new window menu
    Notes:  To create a new window menu, ihParent must be the window handle and icOtherProp the menu label.
            A sub-menu can also inherit the actions from another menu-object (typically a toolbar
            
            NB: IF YOU ADD AN ACCELERATOR TO A POPUP-MENU PROGRESS WILL DIE
------------------------------------------------------------------------------*/
DEF VAR cAction         AS CHAR   NO-UNDO.
DEF VAR cMethod         AS CHAR   NO-UNDO.
DEF VAR cLabel          AS CHAR   NO-UNDO.
DEF VAR hMenuBand       AS HANDLE NO-UNDO.
DEF VAR hRule           AS HANDLE NO-UNDO.
DEF VAR hPlaceHold      AS HANDLE NO-UNDO.
DEF VAR iNumPlaceHold   AS INT    NO-UNDO.
DEF VAR cMenuList       AS CHAR   NO-UNDO.
DEF VAR cPlaceHoldList  AS CHAR   NO-UNDO.
DEF VAR cBtnMenu        AS CHAR   NO-UNDO.
DEF VAR bToggle         AS LOG    NO-UNDO.
DEF VAR bTglChecked     AS LOG    NO-UNDO.
DEF VAR hMenu           AS HANDLE NO-UNDO.
DEF VAR cAvailList      AS CHAR   NO-UNDO.
DEF VAR cWidgetList     AS CHAR   NO-UNDO.
DEF VAR hInheritFrom    AS HANDLE NO-UNDO.
DEF VAR cActionDef      AS CHAR   NO-UNDO.
DEF VAR cEnabledList    AS CHAR   NO-UNDO.
DEF VAR cAccelerator    AS CHAR   NO-UNDO.
DEF VAR bAlreadyAdded   AS LOG    NO-UNDO.
DEF VAR bPopupMenu      AS LOG    NO-UNDO.
DEF VAR hParentToParent AS HANDLE NO-UNDO.

ASSIGN cCtrlHotkeyActions = getAttribute(SESSION,"CtrlHotkeyActions")
       cCtrlHotkeys       = getAttribute(SESSION,"CtrlHotkeys")
       cAltHotkeyActions  = getAttribute(SESSION,"AltHotkeyActions")
       cAltHotkeys        = getAttribute(SESSION,"AltHotkeys")
       .

IF NOT VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc = SOURCE-PROCEDURE.

IF ihParent:TYPE = "menu" THEN DO:
  hMenuBand = ihParent.
  FIND FIRST ttObject WHERE ttObject.hObject = hMenuBand NO-ERROR.
  
  IF NOT AVAIL ttObject THEN DO:
    CREATE ttObject.
    ASSIGN ttObject.hWindow       = IF VALID-HANDLE(hObjectSourceProc:CURRENT-WINDOW) THEN hObjectSourceProc:CURRENT-WINDOW ELSE ?
           ttObject.hObject       = hMenuBand
           ttObject.cObjectType   = "popupMenu"
           ttObject.hDesignObject = ?
           ttObject.hSourceProc   = hObjectSourceProc
           ttObject.cObjectName   = "PopupMenu"
           ttObject.cInitProc     = PROGRAM-NAME(2)
           ttObject.cGenProc      = PROGRAM-NAME(1)
           .
  END.  
END.
ELSE IF ihParent:TYPE NE "window" THEN DO:
  hParentToParent = ihParent. 
  REPEAT WHILE VALID-HANDLE(hParentToParent):
    IF NOT CAN-DO("menu,sub-menu",hParentToParent:TYPE) THEN DO:
      bPopupMenu = YES.
      LEAVE.
    END.
    ELSE IF hParentToParent:TYPE = "menu" THEN DO:
      IF hParentToParent:POPUP-ONLY THEN DO:
        bPopupMenu = YES.
        LEAVE.
      END.
    END.
    hParentToParent = hParentToParent:PARENT NO-ERROR.
  END.

  IF VALID-HANDLE(ihParent) AND ihParent:TYPE = "SUB-MENU" THEN DO:
    FIND FIRST ttEvent WHERE ttEvent.hWidget = ihParent NO-ERROR.
    IF AVAIL ttEvent THEN
      FIND FIRST ttObject WHERE ttObject.hObject = ttEvent.hObject NO-ERROR.
    IF NOT AVAIL ttObject THEN
      FIND FIRST ttObject WHERE ttObject.hObject = ihParent NO-ERROR.
  END.
  ELSE FIND FIRST ttObject WHERE ttObject.hObject = ihParent NO-ERROR.
  IF NOT AVAIL ttObject THEN DO:
    FIND FIRST ttEvent WHERE ttEvent.hWidget = ihParent NO-ERROR.
    IF AVAIL ttEvent THEN DO:
      iNumPlaceHold = 10.
      FIND FIRST ttObject OF ttEvent NO-ERROR.
    END.
  
    IF NOT AVAIL ttObject THEN DO:
      CREATE ttObject.
      ASSIGN ttObject.hWindow       = ihParent:WINDOW
             ttObject.hObject       = ihParent
             ttObject.cObjectType   = "popupMenu"
             ttObject.hDesignObject = ihParent
             ttObject.hSourceProc   = hObjectSourceProc
             ttObject.cObjectName   = ihParent:NAME
             ttObject.cInitProc     = PROGRAM-NAME(2)
             ttObject.cGenProc      = PROGRAM-NAME(1)
             .
    END.
  END.
  
  IF ihParent:TYPE = "SUB-MENU" THEN 
    hMenuBand = ihParent.
  ELSE IF ihParent:POPUP-MENU = ? THEN DO:
    CREATE MENU hMenuBand
           ASSIGN POPUP-ONLY = TRUE
                  .
    ihParent:POPUP-MENU = hMenuBand.
  END.
  ELSE DO:
    hMenuBand = ihParent:POPUP-MENU.

    IF VALID-HANDLE(hMenuBand:LAST-CHILD) THEN 
      CREATE MENU-ITEM hRule
             ASSIGN SUBTYPE = "rule"
                    PARENT  = hMenuBand
                    .
  END.
  DO ix = 1 TO NUM-ENTRIES(icOtherProp,"|"):
    IF ENTRY(1,ENTRY(ix,icOtherProp,"|"),";") = "addfirst" THEN DO:
      hInheritFrom = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icOtherProp,"|"),";")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        MESSAGE "Error when adding inherited menu actions: Invalid handle"
                VIEW-AS ALERT-BOX ERROR.
      ELSE DO:
        FIND bttObject WHERE bttObject.hObject = hInheritFrom NO-ERROR.
        IF AVAIL bttObject THEN DO:
          FOR EACH bttEvent 
              WHERE bttEvent.hObject = hInheritFrom
                AND CAN-DO("button,panel-button",bttEvent.cWidgetType):
            FIND FIRST ttEvent  
                 WHERE ttEvent.hObject = ttObject.hObject
                   AND ttEvent.cAction  = bttEvent.cAction
                 NO-ERROR.
            IF AVAIL ttEvent THEN DO:
              bAlreadyAdded = YES.
              LEAVE.
            END.
          END.
          IF NOT bAlreadyAdded THEN FOR EACH bttEvent 
              WHERE bttEvent.hObject = hInheritFrom
                AND CAN-DO("button,panel-button,rule",bttEvent.cWidgetType)
                AND NOT CAN-DO("undo,save",bttEvent.cAction)
                BREAK BY bttEvent.hObject BY bttEvent.hWidget:

            IF LAST-OF(bttEvent.hObject) AND bttEvent.cWidgetType = "rule" THEN LEAVE.

            CREATE ttEvent.
            ASSIGN ttEvent.hObject     = ttObject.hObject
                   ttEvent.cAction     = bttEvent.cAction
                   ttEvent.cName       = "choose"
                   ttEvent.cWidgetType = IF bttEvent.cWidgetType MATCHES "*button" THEN "menu-item" ELSE "rule"
                   ttEvent.cMethod     = bttEvent.cMethod
                   ttEvent.cLabel      = bttEvent.cLabel
                   .
            IF bttEvent.cWidgetType MATCHES "*button" THEN DO:
              CREATE MENU-ITEM ttEvent.hWidget
                    ASSIGN PARENT     = hMenuBand
                           LABEL      = bttEvent.cLabel
                           NAME       = bttEvent.cAction 
                           TRIGGERS:
                             ON CHOOSE PERSISTENT RUN DoProcessEvent (bttObject.hSourceProc,"choose").
                           END TRIGGERS.
      
              setAttribute(ttObject.hObject,"menu-item" + cAction,STRING(ttEvent.hWidget)).
      
              cWidgetList = cWidgetList + STRING(ttEvent.hWidget) + ",".
              IF ihParent:TYPE = "browse" AND NUM-ENTRIES(ENTRY(1,ENTRY(ix,icActionList),";"),"|") > 1 THEN
                cAvailList = cAvailList + STRING(ttEvent.hWidget) + ",".
            END.
            ELSE CREATE MENU-ITEM hRule
                     ASSIGN SUBTYPE = "rule"
                            PARENT  = hMenuBand
                            .
          END.
        END.
      END.
    END.
  END.
END.
ELSE IF icOtherProp NE "" THEN DO:
  CREATE MENU hMenu.
  ihParent:MENUBAR = hMenu.  
  setAttribute(ihParent,"MENUBAR",STRING(hMenu)).
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihParent
         ttObject.hObject       = hMenu
         ttObject.cObjectType   = "Menu"
         ttObject.cObjectName   = icOtherProp
         ttObject.hDesignObject = ?
         ttObject.hSourceProc   = hObjectSourceProc
         ttObject.cInitProc     = PROGRAM-NAME(2)
         ttObject.cGenProc      = PROGRAM-NAME(1)
         .
  CREATE SUB-MENU hMenuBand
    ASSIGN PARENT = hMenu
           LABEL = icOtherProp
           NAME  = icOtherProp
           .
END.
ELSE DO:
  MESSAGE PROGRAM-NAME(1) SKIP "Invalid menu spesification" VIEW-AS ALERT-BOX ERROR.
END.


ASSIGN cWidgetList        = getAttribute(ttObject.hObject,"menubandwidgetlist")
       cWidgetList        = cWidgetList + (IF cWidgetList NE "" THEN "," ELSE "")
       cAvailList         = getAttribute(ttObject.hObject,"recordavailmenu-items")
       cAvailList         = cAvailList + (IF cAvailList NE "" THEN "," ELSE "")
       cEnabledList       = getAttribute(ttObject.hObject,"configenabledevents")
       cEnabledList       = cEnabledList + (IF cEnabledList NE "" THEN "," ELSE "")
       .

IF icActionList NE "" THEN DO ix = 1 TO NUM-ENTRIES(icActionList):
  ASSIGN cActionDef  = ENTRY(1,ENTRY(ix,icActionList),"¤")
         bToggle     = NO
         bTglChecked = NO
         cAction     = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
         cMethod     = cAction + "Record"
         cLabel      = IF getAttribute(SESSION,"BtnLabel_" + cAction) NE "" THEN
                         getAttribute(SESSION,"BtnLabel_" + cAction)
                       ELSE
                         CAPS(SUBSTR(cAction,1,1)) + SUBSTR(cAction,2)
         .

  IF NUM-ENTRIES(cActionDef,";") = 2 THEN 
    cLabel  = ENTRY(2,cActionDef,";").
  ELSE IF NUM-ENTRIES(cActionDef,";") = 3 THEN 
    ASSIGN cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
           cMethod  = IF ENTRY(3,cActionDef,";") NE "" AND ENTRY(3,cActionDef,";") NE "toggle" THEN ENTRY(3,cActionDef,";") ELSE cMethod
           bToggle  = ENTRY(3,cActionDef,";") = "toggle"
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") = 4 THEN 
    ASSIGN cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
           cMethod  = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cMethod
           bToggle  = ENTRY(4,cActionDef,";") = "toggle"
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") = 5 THEN 
    ASSIGN cLabel      = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
           cMethod     = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cMethod
           bToggle     = ENTRY(4,cActionDef,";") = "toggle"
           bTglChecked = LOGICAL(ENTRY(5,cActionDef,";"))
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") > 5 THEN DO:
    MESSAGE "Error in menu-band definition for element " SKIP
            cActionDef SKIP
            "Menu-bands are defined as a comma-separated list like this: " SKIP(1)
            "[<Action>] | [|<Label for placeholder] [;<Label>;<Method>],[<Action>] | [|<Label for placeholder] [;<Label>;<Method>].." SKIP(1)
            "If label for placeholder equals - (dash) it's considered a rule"
            VIEW-AS ALERT-BOX ERROR.
    RETURN ?.
  END.
  ASSIGN cLabel       = REPLACE(cLabel,CHR(1),",")    
         cAccelerator = IF INDEX(cLabel,"ctrl-") > 0 THEN
                          "CTRL-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"ctrl-") + 5,1))
                        ELSE IF INDEX(cLabel,"alt-") > 0 THEN
                          "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"alt-") + 4,1))
                        ELSE IF INDEX(cLabel,"&") > 0 AND R-INDEX(cLabel,"&") NE LENGTH(cLabel) THEN
                         "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"&") + 1,1))
                        ELSE IF INDEX(cLabel,"&") > 0 THEN ""
                        ELSE IF CAN-DO(cCtrlHotkeyActions,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) THEN 
                         "CTRL-" + ENTRY(LOOKUP(cAction,cCtrlHotkeyActions),cCtrlHotkeys) 
                        ELSE IF CAN-DO(cAltHotkeyActions,cAction) THEN 
                         "ALT-" + ENTRY(LOOKUP(cAction,cAltHotkeyActions),cAltHotkeys) 
                        ELSE "ALT-" + CAPS(SUBSTR(cLabel,1,1)) + ")"
         cLabel       = IF INDEX(cLabel,"ctrl-") > 0 THEN 
                          TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"ctrl-") - 1))
                        ELSE IF INDEX(cLabel,"alt-") > 0 THEN 
                          TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"alt-") - 1))
                        ELSE IF R-INDEX(cLabel,"&") = LENGTH(cLabel) THEN 
                          REPLACE(cLabel,"&","")
                        ELSE cLabel.

  cLabel = REPLACE(cLabel,CHR(1),",").    
         
  IF NUM-ENTRIES(ENTRY(ix,icActionList),"¤") > 1 THEN DO: 
    IF ENTRY(2,ENTRY(ix,icActionList),"¤") MATCHES "*enable*" THEN
      cEnabledList = cEnabledList + "," + cAction.
  END.

  cBtnMenu = IF NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 THEN ENTRY(2,ENTRY(1,cActionDef,";"),"|") 
             ELSE IF CAN-DO("-,rule",cAction) THEN cAction
             ELSE "".

  IF cBtnMenu NE "" AND (NOT CAN-DO(cMenuList,cBtnMenu) OR cBtnMenu = "-" OR cBtnMenu = "rule") THEN DO:

    IF CAN-DO("-,rule",cBtnMenu) THEN 
      CREATE MENU-ITEM hRule
             ASSIGN SUBTYPE = "rule"
                    PARENT  = hMenuBand
                    .
    ELSE DO:
      CREATE SUB-MENU hPlaceHold
             ASSIGN PARENT = hMenuBand
                    LABEL = cBtnMenu
                    NAME  = IF cAction NE "" THEN cAction ELSE REPLACE(cBtnMenu," ","_")
                    .
      ASSIGN cPlaceHoldList = cPlaceHoldList + "," + STRING(hPlaceHold)
             cMenuList      = cMenuList + (IF cMenuList NE "" THEN "," ELSE "") + cBtnMenu.
             iNumPlaceHold = iNumPlaceHold + 1.
      setAttribute(ttObject.hObject,"placeholder" + STRING(iNumPlaceHold),STRING(hPlaceHold)).
      setAttribute(ttObject.hObject,"sub-menu" + cBtnMenu,STRING(hPlaceHold)).
    END.
    NEXT.
  END.

  IF cMethod = "CloseRecord" THEN cMethod = "CloseWindow".
  ELSE IF cMethod = "HelpRecord" THEN cMethod = "Help".

  CREATE ttEvent.
  ASSIGN ttEvent.hObject     = ttObject.hObject
         ttEvent.cAction     = cAction
         ttEvent.cName       = "choose"
         ttEvent.cWidgetType = "menu-item"
         ttEvent.cMethod     = cMethod.
  IF bToggle THEN DO:
    IF cAccelerator NE "" AND hMenuBand:TYPE = "sub-menu" AND NOT bPopupMenu THEN
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN TOGGLE-BOX  = TRUE
                   PARENT      = hMenuBand
                   LABEL       = cLabel
                   NAME        = (IF VALID-HANDLE(ihParent) AND CAN-QUERY(ihParent,"name") AND ihParent:NAME NE ? THEN ihParent:NAME + "_" ELSE "") + "mitm_" + cAction 
                   ACCELERATOR = TRIM(cAccelerator,")")  
                   TRIGGERS:
                     ON VALUE-CHANGED PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
                   END TRIGGERS.
    ELSE
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN TOGGLE-BOX  = TRUE
                   PARENT      = hMenuBand
                   LABEL       = cLabel
                   NAME        = (IF VALID-HANDLE(ihParent) AND CAN-QUERY(ihParent,"name") AND ihParent:NAME NE ? THEN ihParent:NAME + "_" ELSE "") + "mitm_" + cAction 
                   TRIGGERS:
                     ON VALUE-CHANGED PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
                   END TRIGGERS.
      ttEvent.hWidget:CHECKED = bTglChecked.
  END.
  ELSE DO:
    IF cAccelerator NE "" AND hMenuBand:TYPE = "sub-menu" AND NOT bPopupMenu THEN
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN PARENT      = hMenuBand
                   LABEL       = cLabel
                   NAME        = (IF VALID-HANDLE(ihParent) AND CAN-QUERY(ihParent,"name") AND ihParent:NAME NE ? THEN ihParent:NAME + "_" ELSE "") + "mitm_" + cAction 
                   ACCELERATOR = TRIM(cAccelerator,")")  
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
                   END TRIGGERS.
    ELSE
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN PARENT      = hMenuBand
                   LABEL       = cLabel
                   NAME        = (IF VALID-HANDLE(ihParent) AND CAN-QUERY(ihParent,"name") AND ihParent:NAME NE ? THEN ihParent:NAME + "_" ELSE "") + "mitm_" + cAction 
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
                   END TRIGGERS.
  END.

  setAttribute(ttObject.hObject,"menu-item" + cAction,STRING(ttEvent.hWidget)).
  setAttribute(ttObject.hObject,"menu-item" + cLabel,STRING(ttEvent.hWidget)).

  cWidgetList = cWidgetList + STRING(ttEvent.hWidget) + ",".
  IF ihParent:TYPE = "browse" AND NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 THEN
    cAvailList = cAvailList + STRING(ttEvent.hWidget) + ",".
END.

setListAttribute(ihParent,"menuBandWidgetList",TRIM(cWidgetList,","),",").
IF cAvailList NE "" THEN 
  setListAttribute(ihParent,"recordAvailMenu-items",TRIM(cAvailList,","),",").
IF cEnabledList NE "" THEN 
  setListAttribute(ttObject.hObject,"configEnabledEvents",TRIM(cEnabledList,","),",").

setListAttribute(ttObject.hObject,"sub-menu-handles",TRIM(cPlaceHoldList,","),",").  
setListAttribute(ttObject.hObject,"sub-menuList",cMenuList,",").

IF NOT PROGRAM-NAME(2) MATCHES "New* jbox*" THEN
  hObjectSourceProc = ?.

IF ihParent:TYPE NE "window" THEN
  RETURN hMenuBand.
ELSE
  RETURN hMenu.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewObject Procedure 
FUNCTION NewObject RETURNS LOGICAL
  ( INPUT ihWindow AS HANDLE,
    INPUT ihObject AS HANDLE,
    INPUT icType   AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Instantiate an object to use for general purposes. (Could f.ex be used just to 
           make sure that a dynamically created object is deleted when the window is closed)
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cName AS CHAR NO-UNDO.

IF ihWindow = ? THEN ihWindow = CURRENT-WINDOW.

IF CAN-FIND(FIRST ttObject WHERE ttObject.hObject = ihObject) THEN RETURN FALSE.

IF hObjectSourceProc = ? THEN hObjectSourceProc = SOURCE-PROCEDURE.

CREATE ttObject.
ASSIGN ttObject.hWindow      = ihWindow
       ttObject.hObject      = ihObject
       ttObject.cObjectType  = IF icType = "" THEN ihObject:TYPE ELSE icType
       ttObject.hSourceProc  = hObjectSourceProc
       ttObject.cInitProc    = PROGRAM-NAME(2)
       ttObject.cGenProc     = PROGRAM-NAME(1)
       ttObject.cObjectName  = IF CAN-DO("container,procedure",icType) THEN ihObject:FILE-NAME 
                               ELSE IF ihObject:NAME NE ? AND ihObject:NAME NE "" THEN ihObject:NAME
                               ELSE ttObject.cObjectType
       NO-ERROR.

hObjectSourceProc = ?.

IF NOT ERROR-STATUS:ERROR THEN DO:
  {incl/methodlog.i ttObject.cObjectName}
/*   IF CAN-DO("container,procedure",icType) THEN                                                    */
/*     AddEvent(ttObject.hObject,ttObject.hObject,"read-only","read-only","window","read-only","").  */
  RETURN TRUE.
END.
ELSE DO:
  cName = ihObject:NAME NO-ERROR.
  MESSAGE "Error when creating object " + cName SKIP(1)
          PROGRAM-NAME(1) SKIP
          PROGRAM-NAME(2) VIEW-AS ALERT-BOX ERROR.
  RETURN FALSE. 
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewPanel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewPanel Procedure 
FUNCTION NewPanel RETURNS HANDLE
  ( INPUT ihRectangle    AS HANDLE,
    INPUT icMenu         AS CHAR,
    INPUT icActionList   AS CHAR,
    INPUT iiButtonWidth  AS INT,
    INPUT iiButtonHeight AS INT,
    INPUT icOtherProp    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Run the panel object specified in the previously defined attributes for the rectangle
  Notes:   The rectangle object is just a placeholder for the tabfolder object
           All predefined attributes are transeferred to the tabfolder when it is generated
           Every attribute has its default.
------------------------------------------------------------------------------*/
DEF VAR hPanel          AS HANDLE NO-UNDO.
DEF VAR cPanelProg      AS CHAR NO-UNDO INIT "JBoxJlwPanel.w".

IF SEARCH("controls.dll") = ? THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Controls.dll not found"
          VIEW-AS ALERT-BOX.
  RETURN ?.
END.

IF getAttribute(SESSION,"PanelProg") NE "" THEN 
  cPanelProg = getAttribute(SESSION,"PanelProg").
IF getAttribute(ihRectangle,"PanelProg") NE "" THEN 
  cPanelProg = getAttribute(ihRectangle,"PanelProg").

RUN VALUE(cPanelProg) PERSIST SET hPanel.
DYNAMIC-FUNCTION("setParent" IN hPanel,SOURCE-PROCEDURE).

FIND FIRST ttObject WHERE ttObject.hObject = hPanel NO-ERROR.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihRectangle:WINDOW
         ttObject.hObject       = hPanel
         ttObject.cObjectType   = "Panel"
         ttObject.hDesignObject = ihRectangle
         ttObject.hSourceProc   = SOURCE-PROCEDURE
         ttObject.cGenProc      = PROGRAM-NAME(1)
         ttObject.cInitProc     = PROGRAM-NAME(2)
         ttObject.cObjectName   = ihRectangle:NAME
         .
END.

FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihRectangle:
  ttAttribute.hObject = hPanel.
END.
IF icActionList NE "" THEN
  RUN InitializeObject IN hPanel (ihRectangle,
                                  icMenu,
                                  icActionList,
                                  iiButtonWidth,
                                  iiButtonHeight,
                                  icOtherProp,
                                  SOURCE-PROCEDURE).

RETURN hPanel.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewPropertyFillIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewPropertyFillIn Procedure 
FUNCTION NewPropertyFillIn RETURNS HANDLE
  ( INPUT ihBrowse           AS HANDLE,
    INPUT icBrowseColumn     AS CHAR,
    INPUT icBufferColumn     AS CHAR,
    INPUT icDataType         AS CHAR,
    INPUT icFormat           AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icLookupAttrib     AS CHAR,
    INPUT icValidation       AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Fields prefixed with ! are added to the buffer but not to the grid  
------------------------------------------------------------------------------*/
DEF VAR hFillIn         AS HANDLE NO-UNDO.
DEF VAR hButton         AS HANDLE NO-UNDO.
DEF VAR hBrowseColumn   AS HANDLE NO-UNDO.
DEF VAR iTmpXpixels     AS INT NO-UNDO.
DEF VAR iTmpYpixels     AS INT NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",ihBrowse:WINDOW).

ASSIGN iTmpXpixels = ihBrowse:FRAME:WIDTH-PIXELS
       iTmpYpixels = ihBrowse:FRAME:HEIGHT-PIXELS
       .

DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  hBrowseColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
  IF hBrowseColumn:NAME = icBrowseColumn THEN
    LEAVE.
END.

CREATE FILL-IN hFillIn 
  ASSIGN FRAME            = ihBrowse:FRAME
         NAME             = icBrowseColumn
         DATA-TYPE        = icDataType
         FORMAT           = icFormat
         X                = ihBrowse:X + hBrowseColumn:X - 1
         Y                = ihBrowse:Y + hBrowseColumn:Y - 3
         WIDTH-PIXELS     = hBrowseColumn:WIDTH-PIXELS + 4
         HEIGHT-PIXELS    = hBrowseColumn:HEIGHT-PIXELS + 1
         VISIBLE          = NO
         SENSITIVE        = TRUE
         TAB-STOP         = FALSE
         TRIGGERS:
           ON ENTRY            PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"entry").
           ON RETURN           PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").
           ON CURSOR-DOWN      PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").
           ON CURSOR-UP        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").
           ON TAB              PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").
           ON BACK-TAB         PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"leave").
           ON BACKSPACE        PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"delete").
           ON DELETE-CHARACTER PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"delete").
           ON F3               PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"F3").
           ON F10              PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"F3").
         END TRIGGERS.

CREATE ttObject.
ASSIGN ttObject.hWindow       = ihBrowse:WINDOW
       ttObject.hObject       = hFillIn
       ttObject.cObjectType   = "fill-in"
       ttObject.hDesignObject = ihBrowse
       ttObject.hSourceProc   = SOURCE-PROCEDURE
       ttObject.cInitProc    = PROGRAM-NAME(2)
       ttObject.cGenProc     = PROGRAM-NAME(1)
       ttObject.cObjectName  = icBrowseColumn
       .
setAttribute(ttObject.hObject,"browsecolumn",icBrowseColumn).
setAttribute(ttObject.hObject,"buffercolumn",icBufferColumn).
setAttribute(ttObject.hObject,"validation",icValidation).

IF CAN-DO("date,datetime",hFillIn:DATA-TYPE) THEN DO:
  ON "t" OF hFillIn PERSISTENT  RUN DoProcessEvent (SOURCE-PROCEDURE,"todays-date-key").
  ON "d" OF hFillIn PERSISTENT  RUN DoProcessEvent (SOURCE-PROCEDURE,"todays-date-key").
  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = hFillIn
         ttEvent.cAction       = "todays-date-key"
         ttEvent.cName         = "todays-date-key"
         ttEvent.cWidgetType   = "fill-in"
         ttEvent.cMethod       = "TodaysDateKey".
END.

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "leave"
       ttEvent.cName         = "leave"
       ttEvent.cWidgetType   = "browse-fillin"
       ttEvent.cMethod       = "LeaveBrowseFillIn".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "entry"
       ttEvent.cName         = "entry"
       ttEvent.cWidgetType   = "browse-fillin"
       ttEvent.cMethod       = "EnterBrowseFillIn".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "F3"
       ttEvent.cName         = "apply lookup"
       ttEvent.cWidgetType   = "browse-fillin"
       ttEvent.cMethod       = "ApplyBrowseFillInLookup".

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hFillIn
       ttEvent.cAction       = "delete"
       ttEvent.cName         = "delete"
       ttEvent.cWidgetType   = "browse-fillin"
       ttEvent.cMethod       = "DeletePropertyOverlayValue".

IF icBuffersAndFields NE "" OR CAN-DO("date,datetime",icDataType) THEN DO:
  CREATE BUTTON hButton
         ASSIGN FRAME         = ihBrowse:FRAME
                WIDTH-PIXELS  = 17
                TAB-STOP      = FALSE
                NO-FOCUS      = TRUE
                HEIGHT-PIXELS = hBrowseColumn:HEIGHT-PIXELS 
                X             = hFillIn:X + hBrowseColumn:WIDTH-PIXELS - 18 
                Y             = hFillIn:Y + 2
                LABEL         = "..."
                SENSITIVE     = TRUE
                HIDDEN        = TRUE
         TRIGGERS:
           ON CHOOSE PERSISTENT RUN DoProcessEvent (SOURCE-PROCEDURE,"lookup").
         END TRIGGERS.

  IF icBuffersAndFields NE "" THEN DO:
    IF icLookupAttrib = "" THEN icLookupAttrib = icBrowseColumn.
    setAttribute(ttObject.hObject,"buffersandfields",icBuffersAndFields).
    setAttribute(ttObject.hObject,"viewbuffersandfields",icBuffersAndFields).
    setAttribute(ttObject.hObject,"querycriteria",icQueryCriteria).
    setAttribute(ttObject.hObject,"lookupattributes",icLookupAttrib).
  END.

  CREATE ttEvent.
  ASSIGN ttEvent.hObject       = ttObject.hObject
         ttEvent.hWidget       = hButton
         ttEvent.cAction       = "choose"
         ttEvent.cName         = "lookup"
         ttEvent.cWidgetType   = "browse-lookup"
         ttEvent.cMethod       = IF icBuffersAndFields NE "" THEN "BrowseColumnLookup" ELSE "BrowseDateLookup".
END.

ASSIGN ihBrowse:FRAME:WIDTH-PIXELS  = iTmpXpixels
       ihBrowse:FRAME:HEIGHT-PIXELS = iTmpYpixels
       .
DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN hFillIn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewQuery Procedure 
FUNCTION NewQuery RETURNS HANDLE
  ( INPUT iiRowsToBatch      AS INT,
    INPUT icProperties       AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icOtherProp        AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Fields prefixed with ! are added to the buffer but not to the grid  
------------------------------------------------------------------------------*/
DEF VAR httTable               AS HANDLE NO-UNDO.
DEF VAR httTableBuffer         AS HANDLE NO-UNDO.
DEF VAR httTableQuery          AS HANDLE NO-UNDO.
DEF VAR hQuery                 AS HANDLE NO-UNDO.
DEF VAR hField                 AS HANDLE NO-UNDO.
DEF VAR iy                     AS INT    NO-UNDO.
DEF VAR cInitSort              AS CHAR   NO-UNDO.
DEF VAR cInitSortDesc          AS CHAR   NO-UNDO.
DEF VAR bTempTable             AS LOG    NO-UNDO.
DEF VAR cBaseTableFilterFields AS CHAR   NO-UNDO.
DEF VAR cAllViewFields         AS CHAR   NO-UNDO.
DEF VAR cCalcField             AS CHAR   NO-UNDO.
DEF VAR cAllCalcFields         AS CHAR   NO-UNDO.
DEF VAR cAllJoinViewFields     AS CHAR   NO-UNDO.
DEF VAR cCalcFieldProc         AS CHAR   NO-UNDO.
DEF VAR bRecordCount           AS LOG    NO-UNDO.
DEF VAR cBufferFieldSpec       AS CHAR   NO-UNDO.       
DEF VAR cBufferDbFieldSpec     AS CHAR   NO-UNDO. 
DEF VAR hWindow                AS HANDLE NO-UNDO.
DEF VAR cLogText               AS CHAR   NO-UNDO.
DEF VAR hMySourceProc          AS HANDLE NO-UNDO.
DEF VAR cTTfieldspec           AS CHAR   NO-UNDO.
DEF VAR cExtentFields          AS CHAR   NO-UNDO.
DEF VAR cAltExtentFields       AS CHAR   NO-UNDO.
DEF VAR cExtentFieldHandles    AS CHAR   NO-UNDO.
DEF VAR cAltExtentFieldHandles AS CHAR   NO-UNDO.
DEF VAR cExtent                AS CHAR   NO-UNDO.
DEF VAR cOrgExtentField        AS CHAR   NO-UNDO.
DEF VAR cReturnField           AS CHAR   NO-UNDO.

IF hObjectSourceProc = ? THEN hObjectSourceProc = SOURCE-PROCEDURE.

hWindow = hObjectSourceProc:CURRENT-WINDOW.

cLogText = "query for " + hObjectSourceProc:FILE-NAME.
{incl/methodlog.i cLogText}

CREATE QUERY hQuery NO-ERROR.

IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"setInitialQueryAttributes") THEN
  DYNAMIC-FUNCTION("setInitialQueryAttributes" IN SOURCE-PROCEDURE,hQuery).

RUN ConstructBrowseOrQuery(hQuery,icBuffersAndFields,icQueryCriteria,icOtherProp,OUTPUT bOk).
IF NOT bOK THEN DO:
  DELETE OBJECT hQuery NO-ERROR.
  RETURN ?.
END.

IF NOT PROGRAM-NAME(2) BEGINS "propSet_" THEN
  PUBLISH "setPageQueryHandle" (hQuery).

IF ENTRY(1,icBuffersAndFields,";") = "temp-table" THEN DO:
  IF LEFT-TRIM(SUBSTR(ENTRY(1,icBuffersAndFields,";"),12,5)) BEGINS "like " THEN
    cTTfieldSpec = TRIM(SUBSTR(icBuffersAndFields,INDEX(icBuffersAndFields," like ") + 6)).

  httTable = NewTempTable(hWindow,
                          (IF LEFT-TRIM(SUBSTR(ENTRY(1,icBuffersAndFields,";"),12,5)) BEGINS "like " THEN
                             LEFT-TRIM(SUBSTR(ENTRY(1,icBuffersAndFields,";"),12))
                           ELSE IF NUM-ENTRIES(ENTRY(1,icBuffersAndFields,";")," ") > 1 THEN
                              ENTRY(2,ENTRY(1,icBuffersAndFields,";")," ")
                           ELSE
                            "QueryTT_" + STRING(TIME)),
                          hObjectSourceProc,
                          IF cTTfieldSpec NE "" THEN
                            cTTfieldSpec
                          ELSE
                            SUBSTR(icBuffersAndFields,INDEX(icBuffersAndFields,";") + 1),
                          "",
                          IF cTTfieldSpec NE "" THEN icQueryCriteria
                          ELSE "").

  setAttribute(hQuery,"uselocaldata","yes").
  hObjectSourceProc = hMySourceProc.
END.
ELSE IF getAttribute(hQuery,"temptablehandle") = "" THEN DO:
  IF getAttribute(hQuery,"getrecordcount") = "yes" THEN
    DYNAMIC-FUNCTION("setQueryStatFields","rowcount").
  DYNAMIC-FUNCTION("setIndexOnRowids",YES).
  DYNAMIC-FUNCTION("setReturnQueryInfo",YES).

  httTable = DYNAMIC-FUNCTION("getTempTableJoin",iiRowsToBatch,0,"",
                              getAttribute(hQuery,"buffersandfields"),
                              "where false" + getAttribute(hQuery,"queryjoin")).
  
  IF DYNAMIC-FUNCTION("getTransactionMessage") NE "" THEN DO:
    IF DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*(233)*" OR
       DYNAMIC-FUNCTION("getTransactionMessage") MATCHES "*(234)*" THEN DO:
      IF getAttribute(SESSION,"showsecuritywarnings") NE "no" THEN 
        IF DYNAMIC-FUNCTION("DoMessage",-1,4,DYNAMIC-FUNCTION("getTransactionMessage") + CHR(10) + CHR(10) +
                            "Continue to show warnings when I try query restricted database tables?" 
                            ,"Database Security Warning","") = 7 THEN
          setAttribute(SESSION,"showsecuritywarnings","no").
    END.
    ELSE DO:
      DYNAMIC-FUNCTION("DoMessage",-1,0,
                 DYNAMIC-FUNCTION("getTransactionMessage") + CHR(10)
                 + "Function: " + PROGRAM-NAME(1) + CHR(10) 
                 + "Caller: " + PROGRAM-NAME(2)
                ,"JukeBox, browse definition error:","").
      QUIT.
    END.
  END.
END.
ELSE DO:
  httTable = WIDGET-HANDLE(getAttribute(hQuery,"temptablehandle")).
  DYNAMIC-FUNCTION("setAttribute",hQuery,"useLocalData","yes").
END. 

FOR EACH ttAttribute
    WHERE ttAttribute.hObject = httTable:
  ttAttribute.hObject = hQuery.
END.

httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.

hQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
hQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

CREATE ttObject.
ASSIGN ttObject.hWindow      = hWindow
       ttObject.hObject      = hQuery
       ttObject.cObjectType  = "query"
       ttObject.hSourceProc  = hObjectSourceProc
       ttObject.cGenProc     = PROGRAM-NAME(1)
       ttObject.cInitProc    = PROGRAM-NAME(2)
       ttObject.cObjectName  = httTableBuffer:NAME
       .

setAttribute(hQuery,"nodbreadaccessfields",DYNAMIC-FUNCTION("getNoDbReadAccessFields")).
setAttribute(hQuery,"nodbwriteaccessfields",DYNAMIC-FUNCTION("getNoDbWriteAccessFields")).
setAttribute(hQuery,"nodbreadaccessbuffers",DYNAMIC-FUNCTION("getNoDbReadAccessBuffers")).
setAttribute(hQuery,"nodbwriteaccessbuffers",DYNAMIC-FUNCTION("getNoDbWriteAccessBuffers")).
setAttribute(hQuery,"nodbcreateaccessbuffers",DYNAMIC-FUNCTION("getNoDbCreateAccessBuffers")).
setAttribute(hQuery,"nodbdeleteaccessbuffers",DYNAMIC-FUNCTION("getNoDbDeleteAccessBuffers")).

IF PROGRAM-NAME(2) BEGINS "propSet_" AND icBuffersAndFields NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields):
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icBuffersAndFields),";"):
      IF getAttribute(hQuery,"fieldbuffer" + ENTRY(1,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|")) = "" THEN DO:
        setAttribute(hQuery,"fieldbuffer" + ENTRY(1,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|"),ENTRY(1,ENTRY(ix,icBuffersAndFields),";")).
        setAttribute(hQuery,"orgdbfield" + ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),ENTRY(iy,ENTRY(ix,icBuffersAndFields),";")).
      END.  
    END.
  END.
END.
ELSE DO:
  /* If there wasn't a field spec (for some buffers) this is returned from the server: */
  cBufferFieldSpec = DYNAMIC-FUNCTION("getBufferFieldSpec").
  
  IF cBufferFieldSpec NE "" THEN DO:
    cBufferDbFieldSpec = DYNAMIC-FUNCTION("getBufferDbFieldSpec").
    DO ix = 1 TO NUM-ENTRIES(cBufferFieldSpec):
      DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,cBufferFieldSpec),"|"):
        setAttribute(hQuery,"fieldbuffer" + ENTRY(iy,ENTRY(ix,cBufferFieldSpec),"|"),ENTRY(1,ENTRY(ix,cBufferFieldSpec),"|")).
        setAttribute(hQuery,"orgdbfield" + ENTRY(iy,ENTRY(ix,cBufferFieldSpec),"|"),ENTRY(iy,ENTRY(ix,cBufferDbFieldSpec),"|")).
      END.
    END.
  END.
  ELSE IF getAttribute(hQuery,"uselocaldata") = "yes" THEN DO ix = 1 TO httTable:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
    setAttribute(hQuery,"fieldbuffer" + httTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ix):NAME,httTable:DEFAULT-BUFFER-HANDLE:NAME).
    setAttribute(hQuery,"orgdbfield" + httTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ix):NAME,httTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(ix):NAME).
  END.
END.  

DO ix = 1 TO httTableBuffer:NUM-FIELDS:
  cReturnField = httTableBuffer:BUFFER-FIELD(ix):NAME.
  IF cReturnField BEGINS "jbextent_" THEN DO:
    ASSIGN cOrgExtentField        = SUBSTR(cReturnField,INDEX(cReturnField,"_",10) + 1)
           cExtent                = SUBSTR(cReturnField,10,INDEX(cReturnField,"_",10) - 10)
           cOrgExtentField        = cOrgExtentField + (IF getAttribute(hQuery,"tempTableHandle") = "" THEN "[" + cExtent + "]" ELSE "_" + cExtent)
           cExtentFields          = cExtentFields + cOrgExtentField + ","
           cAltExtentFields       = cAltExtentFields + cReturnField + ","
           cExtentFieldHandles    = cExtentFieldHandles + STRING(httTableBuffer:BUFFER-FIELD(cOrgExtentField)) + ",".
           cAltExtentFieldHandles = cAltExtentFieldHandles + STRING(httTableBuffer:BUFFER-FIELD(ix)) + ",".
    setAttribute(hQuery,"extent_" + cReturnField,cExtent).
    setAttribute(hQuery,"extent_" + cOrgExtentField,cExtent).
  END.
END.
setAttribute(hQuery,"extentFields",TRIM(cExtentFields,",")).
setAttribute(hQuery,"altExtentFields",TRIM(cAltExtentFields,",")).
setAttribute(hQuery,"extentFieldHandles",TRIM(cExtentFieldHandles,",")).
setAttribute(hQuery,"altExtentFieldHandles",TRIM(cAltExtentFieldHandles,",")).

setAttribute(hQuery,"rowstobatch",STRING(iiRowsToBatch)).
IF getAttribute(hQuery,"querywhere") BEGINS "where false" THEN
  setAttribute(hQuery,"querywhere","").

CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hQuery
       ttEvent.cAction       = "value-changed"
       ttEvent.cName         = "value-changed"
       ttEvent.cWidgetType   = "query"
       ttEvent.cMethod       = "DisplayRecord".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hQuery
       ttEvent.cAction       = "open-query"
       ttEvent.cName         = "open-query"
       ttEvent.cWidgetType   = "query"
       ttEvent.cMethod       = "OpenQuery".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hQuery
       ttEvent.cAction       = "off-end"
       ttEvent.cName         = "off-end"
       ttEvent.cWidgetType   = "query"
       ttEvent.cMethod       = "OffEnd".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hQuery
       ttEvent.cAction       = "off-home"
       ttEvent.cName         = "off-home"
       ttEvent.cWidgetType   = "query"
       ttEvent.cMethod       = "OffHome".
CREATE ttEvent.
ASSIGN ttEvent.hObject       = ttObject.hObject
       ttEvent.hWidget       = hQuery
       ttEvent.cAction       = "excel"
       ttEvent.cName         = "excel"
       ttEvent.cWidgetType   = "browse"
       ttEvent.cMethod       = "ExcelRecord".

DO ix = 1 TO httTableBuffer:NUM-FIELDS: 
  IF (NOT httTableBuffer:BUFFER-FIELD(ix):NAME BEGINS "RowIdent" AND
      httTableBuffer:BUFFER-FIELD(ix):NAME NE "RowCount" AND
      NOT icBuffersAndFields MATCHES "*!" + httTableBuffer:BUFFER-FIELD(ix):NAME + "*") 
     THEN 
  cAllViewFields = cAllViewFields + httTableBuffer:BUFFER-FIELD(ix):NAME + ",".
END.
setAttribute(ttObject.hObject,"allviewfields",TRIM(cAllViewFields,",")).
IF cBaseTableFilterFields = "" THEN
  setAttribute(ttObject.hObject,"basetablefilterfields",TRIM(cAllViewFields,",")).

DO ix = 1 TO NUM-ENTRIES(icProperties):
  CASE ENTRY(1,ENTRY(ix,icProperties),"|"):
    WHEN "PRIVATE-DATA"                 THEN hQuery:PRIVATE-DATA = ENTRY(2,ENTRY(ix,icProperties),"|").
  END CASE.
END.

IF NOT LEFT-TRIM(icQueryCriteria) BEGINS "where false" 
   THEN DO:
  FillQuery(hQuery,iiRowsToBatch,0,"","").

  DYNAMIC-FUNCTION("setCurrentObject",hQuery).
END.

hObjectSourceProc = ?.

RETURN hQuery.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewTabFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewTabFolder Procedure 
FUNCTION NewTabFolder RETURNS HANDLE
  ( INPUT ihRectangle            AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Run the tabfolder object specified in the previously defined attributes for the rectangle
  Notes:   The rectangle object is just a placeholder for the tabfolder object
           All predefined attributes are transeferred to the tabfolder when it is generated
           Every attribute has its default.
------------------------------------------------------------------------------*/
DEF VAR hTabFolder          AS HANDLE NO-UNDO.
DEF VAR cTabFolderProg      AS CHAR NO-UNDO INIT "JBoxTabFolder.w".

IF getAttribute(SESSION,"TabFolderProg") NE "" THEN 
  cTabFolderProg = getAttribute(SESSION,"TabFolderProg").
IF getAttribute(ihRectangle,"TabFolderProg") NE "" THEN 
  cTabFolderProg = getAttribute(ihRectangle,"TabFolderProg").

IF cTabFolderProg = "JBoxJlwTabFolder.w" AND SEARCH("controls.dll") = ? THEN
  cTabFolderProg = "JBoxTabFolder.w".

IF hObjectSourceProc = ? THEN hObjectSourceProc = SOURCE-PROCEDURE.

RUN VALUE(cTabFolderProg) PERSIST SET hTabFolder.
DYNAMIC-FUNCTION("setParent" IN hTabFolder,hObjectSourceProc).
DYNAMIC-FUNCTION("setTabFolderPosition" IN hTabFolder,ihRectangle).

FIND FIRST ttObject WHERE ttObject.hObject = hTabFolder NO-ERROR.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihRectangle:WINDOW
         ttObject.hObject       = hTabFolder
         ttObject.cObjectType   = "TabFolder"
         ttObject.hDesignObject = ihRectangle
         ttObject.hSourceProc   = SOURCE-PROCEDURE
         ttObject.cGenProc      = PROGRAM-NAME(1)
         ttObject.cInitProc     = PROGRAM-NAME(2)
         ttObject.cObjectName   = ihRectangle:NAME
         .
END.

FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihRectangle:
  ttAttribute.hObject = hTabFolder.
END.

hObjectSourceProc = ?.

RETURN hTabFolder.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewTempTable Procedure 
FUNCTION NewTempTable RETURNS HANDLE
  ( INPUT ihContainer        AS HANDLE,
    INPUT icName             AS CHAR,
    INPUT ihSourceProc       AS HANDLE,
    INPUT icFieldDef         AS CHAR,
    INPUT icIndexList        AS CHAR,
    INPUT icOtherProp        AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Create a client temp-table 
    Notes: When the table is defined LIKE (icName begins LIKE ..) the last param
           can be used as query crit  
------------------------------------------------------------------------------*/
DEF VAR httTable               AS HANDLE NO-UNDO.
DEF VAR iy                     AS INT    NO-UNDO.
DEF VAR iz                     AS INT    NO-UNDO.
DEF VAR cExtraField            AS CHAR   NO-UNDO.
DEF VAR cExtraDataType         AS CHAR   NO-UNDO.
DEF VAR cExtraFormat           AS CHAR   NO-UNDO.
DEF VAR cExtraInit             AS CHAR   NO-UNDO. 
DEF VAR cExtraLabel            AS CHAR   NO-UNDO.
DEF VAR cIndexName             AS CHAR   NO-UNDO.

IF icName BEGINS "LIKE " THEN DO:
  httTable = DYNAMIC-FUNCTION("getTempTableJoin",0,0,"",
                              icFieldDef,
                              IF icOtherProp NE "" THEN icOtherProp ELSE "WHERE false").
  icName = TRIM(SUBSTR(icName,6)).
END.
ELSE DO:
  CREATE TEMP-TABLE httTable.

  ASSIGN icFieldDef = REPLACE(icFieldDef,"+!","")
         icFieldDef = REPLACE(icFieldDef,"!+","")
         icFieldDef = REPLACE(icFieldDef,";+",";")
         icFieldDef = REPLACE(icFieldDef,"!","")
         .
  IF icFieldDef BEGINS "+" THEN
    icFieldDef = SUBSTR(icFieldDef,2).
  
  DO iy = 1 TO NUM-ENTRIES(icFieldDef,";"):
    DO iz = 1 TO NUM-ENTRIES(ENTRY(iy,icFieldDef,";"),"|"):
      CASE iz:
        WHEN 1 THEN cExtraField    = ENTRY(iz,ENTRY(iy,icFieldDef,";"),"|").
        WHEN 2 THEN cExtraDataType = ENTRY(iz,ENTRY(iy,icFieldDef,";"),"|").
        WHEN 3 THEN cExtraFormat   = REPLACE(ENTRY(iz,ENTRY(iy,icFieldDef,";"),"|"),"<",",").
        WHEN 4 THEN cExtraInit     = ENTRY(iz,ENTRY(iy,icFieldDef,";"),"|").
        WHEN 5 THEN cExtraLabel    = ENTRY(iz,ENTRY(iy,icFieldDef,";"),"|").
      END CASE.
    END.
    iz = iz - 1.
    IF iz < 2 THEN DO:
      MESSAGE PROGRAM-NAME(1) SKIP
              "Missing data type for extra field " + cExtraField
              VIEW-AS ALERT-BOX.
      RETURN ?.
    END.
    CASE iz:
      WHEN 2 THEN httTable:ADD-NEW-FIELD(cExtraField,cExtraDataType).
      WHEN 3 THEN httTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat).
      WHEN 4 THEN DO:
        IF cExtraInit = "" THEN
          httTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat).
        ELSE
          httTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat,cExtraInit).
      END.
      WHEN 5 THEN 
        httTable:ADD-NEW-FIELD(cExtraField,
                                  cExtraDataType,0,
                                  cExtraFormat,
                                  cExtraInit,
                                  cExtraLabel,
                                  IF cExtraLabel MATCHES "*" + CHR(10) + "*" THEN
                                    REPLACE(cExtraLabel,CHR(10),"!")
                                  ELSE ?) NO-ERROR.
    END CASE.
    cExtraInit = "".
    
  END. 
  
  httTable:ADD-NEW-FIELD("RowIdent1","CHARACTER").
  
  IF icName = "" THEN
    icName = ENTRY(1,ENTRY(1,icFieldDef),";").
  
  DO iy = 1 TO NUM-ENTRIES(icIndexList):
    cIndexName = "idx" + STRING(iy) + "_" + ENTRY(1,ENTRY(iy,icIndexList),";").
    httTable:ADD-NEW-INDEX(cIndexName).
    DO iz = 1 TO NUM-ENTRIES(ENTRY(iy,icIndexList),";"):
      httTable:ADD-INDEX-FIELD(cIndexName,ENTRY(iz,ENTRY(iy,icIndexList),";")).
    END.
  END.
  
  httTable:TEMP-TABLE-PREPARE(icName) NO-ERROR.
  IF ERROR-STATUS:ERROR OR ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
    MESSAGE PROGRAM-NAME(1) SKIP
            ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX.
    RETURN ?.
  END.
END.
  
CREATE ttObject.
ASSIGN ttObject.hWindow      = ihContainer
       ttObject.hObject      = httTable
       ttObject.cObjectType  = "temp-table"
       ttObject.hSourceProc  = IF VALID-HANDLE(ihSourceProc) THEN ihSourceProc ELSE SOURCE-PROCEDURE
       ttObject.cGenProc     = PROGRAM-NAME(1)
       ttObject.cInitProc    = PROGRAM-NAME(2)
       ttObject.cObjectName  = icName
       .

hObjectSourceProc = ?.

RETURN httTable. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewToolBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewToolBar Procedure 
FUNCTION NewToolBar RETURNS HANDLE
  ( INPUT ihRect      AS HANDLE,
    INPUT icMenu      AS CHAR,
    INPUT icActList   AS CHAR,
    INPUT icOtherProp AS CHAR) :

DEF VAR cAction        AS CHAR   NO-UNDO.
DEF VAR cMethod        AS CHAR   NO-UNDO.
DEF VAR cLabel         AS CHAR   NO-UNDO.
DEF VAR cTTip          AS CHAR   NO-UNDO.
DEF VAR cImg           AS CHAR   NO-UNDO.
DEF VAR hMenu          AS HANDLE NO-UNDO.
DEF VAR hPlaceHold     AS HANDLE NO-UNDO.
DEF VAR hRule          AS HANDLE NO-UNDO.
DEF VAR cHotKey        AS CHAR   NO-UNDO.
DEF VAR cMnuLst        AS CHAR   NO-UNDO.
DEF VAR cBtnMenu       AS CHAR   NO-UNDO.
DEF VAR iBtnGap        AS INT    NO-UNDO.
DEF VAR iNumPlaceHold  AS INT    NO-UNDO.
DEF VAR bRightAdjust   AS LOG    NO-UNDO.
DEF VAR bNoMenu        AS LOG    NO-UNDO.
DEF VAR cAccel         AS CHAR   NO-UNDO.
DEF VAR cFunKeyActs    AS CHAR   NO-UNDO.
DEF VAR cFunKeys       AS CHAR   NO-UNDO.
DEF VAR cBtnNames      AS CHAR   NO-UNDO.
DEF VAR cBtnHdls       AS CHAR   NO-UNDO.
DEF VAR cRuleHdls      AS CHAR   NO-UNDO.
DEF VAR hParentSubMenu AS HANDLE NO-UNDO.
DEF VAR hLastBtn       AS HANDLE NO-UNDO.
DEF VAR cActionDef     AS CHAR   NO-UNDO.
DEF VAR cEnablLst      AS CHAR   NO-UNDO.
DEF VAR cPholdLst      AS CHAR   NO-UNDO.
DEF VAR bMenuOnly      AS LOG    NO-UNDO.
DEF VAR bTgl           AS LOG    NO-UNDO.
DEF VAR hCurrObj       AS HANDLE NO-UNDO.
DEF VAR cActionList    AS CHAR   NO-UNDO.
DEF VAR iNumMenuOnly   AS INT    NO-UNDO.
DEF VAR bAppend        AS LOG    NO-UNDO.
DEF VAR iStartX        AS INT    NO-UNDO.
DEF VAR hButton        AS HANDLE NO-UNDO.
DEF VAR ihButtonY      AS INT    NO-UNDO.
DEF VAR ihButtonWidth  AS INT    NO-UNDO.
DEF VAR ihButtonHeight AS INT    NO-UNDO.
DEF VAR bhButtonFlat   AS LOG    NO-UNDO.
DEF VAR cPanelHeight   AS CHAR   NO-UNDO.

/*GOO - resize toolbar*/
def var iNewPanelHeight    as int init 40 no-undo. /*Size 48x48*/
def var iOldPanelHeight    as int init 20 no-undo.
def var iPanelHeight       as int init 20 no-undo.

IF getAttribute(ihRect,"rectangleSet") NE "yes" THEN DO: /* not a static toolbar */
  cPanelHeight = getAttribute(ihRect,"btnPanelHeight").
  IF cPanelHeight = "" THEN 
    cPanelHeight = getAttribute(SESSION,"btnPanelHeight").
    
  IF cPanelHeight NE "" THEN 
    CASE cPanelHeight:
      WHEN "Small"  THEN iPanelHeight = 22.
      WHEN "Medium" THEN iPanelHeight = 36.   
      WHEN "Large"  THEN iPanelHeight = 52.
      OTHERWISE     iPanelHeight = INT(cPanelHeight).
    END.   
  ELSE IF VALID-HANDLE(ihRect) THEN
    ASSIGN  
      iPanelHeight         = IF ihRect:height-pixels >= 36 then iNewPanelHeight else iOldPanelHeight
      ihRect:height-pixels = iPanelHeight.
END.

ASSIGN cCtrlHotkeyActions = getAttribute(SESSION,"CtrlHotkeyActions")
       cCtrlHotkeys       = getAttribute(SESSION,"CtrlHotkeys")
       cAltHotkeyActions  = getAttribute(SESSION,"AltHotkeyActions")
       cAltHotkeys        = getAttribute(SESSION,"AltHotkeys")
       cFunKeys           = getAttribute(SESSION,"FunctionKeys")
       cFunKeyActs        = getAttribute(SESSION,"FunctionKeyActions")
       .

IF icOtherProp MATCHES "*right*" THEN 
  bRightAdjust = TRUE.

IF NOT VALID-HANDLE(hObjectSourceProc) THEN hObjectSourceProc = SOURCE-PROCEDURE.

IF icOtherProp BEGINS "append_toolbar" THEN DO:
  FIND FIRST ttObject WHERE ttObject.hObject = WIDGET-HANDLE(ENTRY(2,icOtherProp)) NO-ERROR.
  IF NOT AVAIL ttObject THEN RETURN ?.
  ASSIGN bAppend = YES
         icOtherProp = ENTRY(3,icOtherProp)
         cPholdLst = getAttribute(ttObject.hObject,"sub-menu-handles")
         cMnuLst   = getAttribute(ttObject.hObject,"sub-menuList") 
         .
  IF ihRect = ? OR ihRect = ttObject.hDesignObject THEN DO:
    ASSIGN iStartX  = INT(getAttribute(ttObject.hObject,"startX")) 
           ihRect   = ttObject.hDesignObject.
           
    ihButtonY = INT(getAttribute(ttObject.hObject,"firstStaticButtonY")) NO-ERROR.
    IF ihButtonY NE 0 THEN  /* This run extends a static toolbar */         
      ASSIGN bhButtonFlat   = LOGICAL(getAttribute(ttObject.hObject,"firstStaticButtonFlat"))  
             ihButtonWidth  = INT(getAttribute(ttObject.hObject,"firstStaticButtonWidth"))  
             ihButtonHeight = INT(getAttribute(ttObject.hObject,"firstStaticButtonHeight"))
             iStartX = iStartX - 3
             NO-ERROR.
  END.         
END.
ELSE DO:
  FIND FIRST ttObject WHERE ttObject.hObject = ihRect NO-ERROR.
  icActList = AddEditBtn(icActList).
END.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihRect:WINDOW
         ttObject.hObject       = ihRect
         ttObject.cObjectType   = "toolbar"
         ttObject.hDesignObject = ihRect
         ttObject.cObjectName   = ihRect:NAME
         ttObject.hSourceProc   = hObjectSourceProc
         ttObject.cGenProc      = PROGRAM-NAME(1)
         ttObject.cInitProc     = PROGRAM-NAME(2).
END.
hCurrObj = ttObject.hObject.

/*
IF ihButtonWidth = 0 THEN 
  ihButtonWidth = IF icMenu = "" THEN 23 ELSE 20.
GOO: */    
IF ihButtonWidth = 0 THEN 
  ihButtonWidth = IF icMenu = "" THEN iPanelHeight + 3 ELSE iPanelHeight.  

IF NOT bRightAdjust AND iStartX = 0 THEN iStartX = ihRect:X.

IF ihRect:TYPE = "rectangle" THEN
  PUBLISH "NeverResize" (ihRect,"Y").

IF icMenu NE "" THEN DO:
  hMenu = WIDGET-HANDLE(getAttribute(ihRect:WINDOW,"MENUBAR")) NO-ERROR.
  IF NOT VALID-HANDLE(ihRect:WINDOW:MENUBAR) AND NOT VALID-HANDLE(hMenu) THEN DO:
    CREATE MENU hMenu.
    ihRect:WINDOW:MENUBAR = hMenu.
    setAttribute(ihRect:WINDOW,"MENUBAR",STRING(hMenu)).
  END.
  ELSE DO:
    ix = 1.
    hPlaceHold = WIDGET-HANDLE(ENTRY(ix,cPholdLst)) NO-ERROR.
    REPEAT WHILE VALID-HANDLE(hPlaceHold):
      IF hPlaceHold:LABEL = icMenu THEN DO:
        IF VALID-HANDLE(hPlaceHold:LAST-CHILD) AND hPlaceHold:LAST-CHILD:TYPE NE "sub-menu" AND hPlaceHold:LAST-CHILD:SUBTYPE NE "rule" AND (NUM-ENTRIES(icActList) > 1 OR CAN-DO("rule,-",ENTRY(1,icActList))) THEN DO:
          CREATE MENU-ITEM hRule
                ASSIGN SUBTYPE = "rule"
                       PARENT  = hPlaceHold.
          AddEvent(hCurrObj,hRule,"no action","rule","menurule","","").
        END.
        LEAVE.
      END.
      ix = ix + 1.
      hPlaceHold = WIDGET-HANDLE(ENTRY(ix,cPholdLst)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN hPlaceHold = ?.
    END.
    /* Look for corresp. sub-menu for the window: */
    IF NOT VALID-HANDLE(hPlaceHold) THEN DO:
      hPlaceHold = hMenu:FIRST-CHILD.
      REPEAT WHILE VALID-HANDLE(hPlaceHold):
        IF hPlaceHold:LABEL = icMenu THEN DO:
          IF VALID-HANDLE(hPlaceHold:LAST-CHILD) AND hPlaceHold:LAST-CHILD:TYPE NE "sub-menu" AND hPlaceHold:LAST-CHILD:SUBTYPE NE "rule" THEN DO:
            CREATE MENU-ITEM hRule
                  ASSIGN SUBTYPE = "rule"
                         PARENT  = hPlaceHold.
            AddEvent(hCurrObj,hRule,"no action","rule","menurule","","").
          END.
          LEAVE.
        END.
        hPlaceHold = hPlaceHold:NEXT-SIBLING.
      END.
    END.
  END.
  IF NOT VALID-HANDLE(hPlaceHold) THEN DO:
    CREATE SUB-MENU hPlaceHold
           ASSIGN PARENT = hMenu
                  LABEL = icMenu
                  NAME  = icMenu.
    AddEvent(hCurrObj,hPlacehold,"no action",icMenu,"sub-menu",icMenu,"").
    setListAttribute(hCurrObj,"sub-menu" + cLabel,STRING(hPlaceHold),",").
    ASSIGN cMnuLst   = appendList(cMnuLst,icMenu,"")
           cPholdLst = appendList(cPholdLst,STRING(hPlaceHold),",").
  END.
END.

DO ix = 1 TO NUM-ENTRIES(icActList):
  ASSIGN cImg       = ""
         bNoMenu      = NO
         bMenuOnly    = NO
         cAccel       = ""    
         cActionDef   = ENTRY(1,ENTRY(ix,icActList),"¤")
         cAction      = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
         cLabel       = IF NUM-ENTRIES(cActionDef,";") > 1 AND ENTRY(2,cActionDef,";") NE "" THEN
                          ENTRY(2,cActionDef,";")         
                        ELSE IF getAttribute(SESSION,"BtnLabel_" + cAction) NE "" THEN
                          getAttribute(SESSION,"BtnLabel_" + cAction)
                        ELSE
                          CAPS(SUBSTR(cAction,1,1)) + SUBSTR(cAction,2)
         cTTip        = cLabel
         cMethod      = cAction + "Record"
         .
  IF NUM-ENTRIES(cActionDef,";") = 2 THEN 
    ASSIGN cLabel   = ENTRY(2,cActionDef,";")
           cTTip    = cLabel.
  ELSE IF NUM-ENTRIES(cActionDef,";") = 3 THEN 
    ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
           cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
           cTTip    = ENTRY(3,cActionDef,";")
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") = 4 THEN
    ASSIGN cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
           cTTip    = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cTTip
           cMethod  = ENTRY(4,cActionDef,";")
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") = 5 THEN 
    ASSIGN cLabel   = IF ENTRY(2,cActionDef,";") NE "" THEN ENTRY(2,cActionDef,";") ELSE cLabel
           cTTip    = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cTTip
           cMethod  = IF ENTRY(4,cActionDef,";") NE "" THEN ENTRY(4,cActionDef,";") ELSE cMethod
           cImg     = ENTRY(5,cActionDef,";")
           .
  ELSE IF NUM-ENTRIES(cActionDef,";") > 5 THEN DO:
    MESSAGE "Error in TB def for element " cActionDef 
            VIEW-AS ALERT-BOX.
    RETURN ?.
  END.

  ASSIGN cLabel       = REPLACE(cLabel,CHR(1),",")    
         cTTip        = REPLACE(cTTip,CHR(1),",")    
         bTgl         = NUM-ENTRIES(cActionDef,";") > 2 AND ENTRY(3,cActionDef,";") = "toggle"
         cAccel       = IF INDEX(cLabel,"ctrl-") > 0 THEN
                          "CTRL-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"ctrl-") + 5,1))
                        ELSE IF INDEX(cLabel,"alt-") > 0 THEN
                          "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"alt-") + 4,1))
                        ELSE IF INDEX(cLabel,"&") > 0 AND R-INDEX(cLabel,"&") NE LENGTH(cLabel) THEN
                         "ALT-" + CAPS(SUBSTR(cLabel,INDEX(cLabel,"&") + 1,1))
                        ELSE IF INDEX(cLabel,"&") > 0 THEN ""
                        ELSE IF CAN-DO(cFunKeyActs,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) THEN 
                          ENTRY(LOOKUP(cAction,cFunKeyActs),cFunKeys)
                        ELSE IF CAN-DO(cCtrlHotkeyActions,cAction) AND NOT CAN-DO(cAltHotkeyActions,cAction) THEN 
                         "CTRL-" + ENTRY(LOOKUP(cAction,cCtrlHotkeyActions),cCtrlHotkeys) 
                        ELSE IF CAN-DO(cAltHotkeyActions,cAction) THEN 
                         "ALT-" + ENTRY(LOOKUP(cAction,cAltHotkeyActions),cAltHotkeys) 
                        ELSE "ALT-" + CAPS(SUBSTR(cLabel,1,1))
         cLabel       = (IF INDEX(cLabel,"ctrl-") > 0 THEN 
                          TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"ctrl-") - 1))
                         ELSE IF INDEX(cLabel,"alt-") > 0 THEN 
                          TRIM(SUBSTR(cLabel,1,INDEX(cLabel,"alt-") - 1))
                         ELSE IF R-INDEX(cLabel,"&") = LENGTH(cLabel) THEN 
                          REPLACE(cLabel,"&","")
                         ELSE cLabel)
         cTTip        = RIGHT-TRIM(cTTip + " (" + cAccel," (") + (IF cAccel NE "" THEN ")" ELSE "")
         cActionList  = cActionList + (IF cActionList NE "" THEN "," ELSE "") + cAction
         hButton      = ?.

  IF NUM-ENTRIES(ENTRY(ix,icActList),"¤") > 1 THEN DO: 
    IF ENTRY(2,ENTRY(ix,icActList),"¤") MATCHES "*enable*" THEN
      cEnablLst = cEnablLst + "," + cAction.
    IF ENTRY(2,ENTRY(ix,icActList),"¤") MATCHES "*menu*" THEN 
      ASSIGN bMenuOnly = YES
             iNumMenuOnly = iNumMenuOnly + 1.
    IF NUM-ENTRIES(ENTRY(ix,icActList),"¤") > 3 THEN 
      hButton = WIDGET-HANDLE(ENTRY(4,ENTRY(ix,icActList),"¤")).
    ELSE IF NUM-ENTRIES(ENTRY(ix,icActList),"¤") > 2 THEN 
      hButton = WIDGET-HANDLE(ENTRY(3,ENTRY(ix,icActList),"¤")).
  END.

  IF (cAction = "rule" OR cAction = "-") AND ihRect:WIDTH-PIXELS > 20 AND NOT bMenuOnly THEN DO:
    CREATE RECTANGLE hRule
           ASSIGN FRAME         = ihRect:FRAME
                  GRAPHIC-EDGE  = TRUE
                  FILLED        = FALSE
                  X             = IF bRightAdjust THEN 
/*                                    ihRect:X + iStartX + ihRect:WIDTH-PIXELS - iBtnGap - (ix - 1 - iNumPlaceHold) * (IF icMenu = "" THEN 23 ELSE 20 ) + 2 */
                                    ihRect:X + iStartX + ihRect:WIDTH-PIXELS - iBtnGap - (ix - 1 - iNumPlaceHold) * (IF icMenu = "" THEN iPanelHeight + 3 ELSE iPanelHeight ) + 2
                                  ELSE
                                    ihRect:X + iStartX + iBtnGap + (ix - 1 - iNumPlaceHold) * ihButtonWidth + 5
/*                                    ihRect:X + iStartX + iBtnGap + (ix - 1 - iNumPlaceHold) * (IF icMenu = "" THEN 23 ELSE 20 ) + 5 */
                  Y             = ihRect:Y - (IF getAttribute(ihRect,"rectangleSetY") = "yes" THEN 0 ELSE 3)
                  EDGE-PIXELS   = 2
                  WIDTH-PIXELS  = 2
                  HEIGHT-PIXELS = iPanelHeight + 5
/*                  HEIGHT-PIXELS = 25 */
                  HIDDEN        = FALSE
                  NAME          = "rect_" + STRING(hCurrObj).
    cRuleHdls = cRuleHdls + STRING(hRule) + ",".

    AddEvent(hCurrObj,hRule,"no action","rule","rule","rule","").
    iBtnGap = iBtnGap - 13.
    setAttribute(hCurrObj,"startX",STRING(hRule:X + (IF bRightAdjust THEN 0 ELSE hRule:WIDTH-PIXELS + iBtnGap) - ihRect:X)).
    NEXT.
  END.
  ELSE IF (cAction = "rule" OR cAction = "-") AND bMenuOnly AND VALID-HANDLE(hPlaceHold) THEN DO:
    CREATE MENU-ITEM hRule
          ASSIGN SUBTYPE = "rule"
                 PARENT  = hPlaceHold
                 .
    NEXT.
  END.

  cBtnMenu = IF NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 THEN ENTRY(2,ENTRY(1,cActionDef,";"),"|") ELSE "".

  IF cBtnMenu NE "" AND cAction NE "sub-menu" AND NOT CAN-DO(cMnuLst,cBtnMenu) THEN DO:
    IF NOT VALID-HANDLE(hMenu) THEN 
      hMenu = WIDGET-HANDLE(getAttribute(ihRect:WINDOW,"MENUBAR")) NO-ERROR.
    IF NOT VALID-HANDLE(hMenu) THEN DO:
      CREATE MENU hMenu.
      ihRect:WINDOW:MENUBAR = hMenu.
    END.
    ELSE DO:
      hPlaceHold = hMenu:FIRST-CHILD.
      REPEAT WHILE VALID-HANDLE(hPlaceHold):
        IF hPlaceHold:LABEL = cBtnMenu THEN DO:
          IF VALID-HANDLE(hPlaceHold:LAST-CHILD) AND hPlaceHold:LAST-CHILD:SUBTYPE NE "rule" THEN DO:
            CREATE MENU-ITEM hRule
                  ASSIGN SUBTYPE = "rule"
                         PARENT  = hPlaceHold.
            AddEvent(hCurrObj,hRule,"no action","rule","menurule","rule","").
          END.
          LEAVE.
        END.
        hPlaceHold = hPlaceHold:NEXT-SIBLING.
      END.
    END.
    IF NOT VALID-HANDLE(hPlaceHold) THEN DO:
      CREATE SUB-MENU hPlaceHold
             ASSIGN PARENT = hMenu
                    LABEL = cBtnMenu
                    NAME  = IF cAction NE "" THEN cAction ELSE REPLACE(cBtnMenu," ","").
      AddEvent(hCurrObj,hPlaceHold,cAction,cBtnMenu,"sub-menu",cBtnMenu,"").
      setListAttribute(hCurrObj,"sub-menu" + cBtnMenu,STRING(hPlaceHold),",").
    END.
    ASSIGN cMnuLst   = appendList(cMnuLst,cBtnMenu,",") /* cMnuLst + "," + cBtnMenu */
           cPholdLst = appendList(cPholdLst,STRING(hPlaceHold),",").
  END.
  ELSE IF cAction = "sub-menu" THEN DO:
    IF NOT VALID-HANDLE(hMenu) THEN 
      hMenu = WIDGET-HANDLE(getAttribute(ihRect:WINDOW,"MENUBAR")) NO-ERROR.
    IF NOT VALID-HANDLE(hMenu) THEN 
      hMenu = ihRect:WINDOW:MENUBAR.
    hParentSubMenu = hMenu:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hParentSubMenu):
      IF hParentSubMenu:LABEL = cBtnMenu THEN DO:
        IF VALID-HANDLE(hParentSubMenu:LAST-CHILD) AND hParentSubMenu:LAST-CHILD:TYPE NE "sub-menu" AND hParentSubMenu:LAST-CHILD:SUBTYPE NE "rule" THEN DO:
          CREATE MENU-ITEM hRule
                ASSIGN SUBTYPE = "rule"
                       PARENT  = hParentSubMenu.
          AddEvent(hCurrObj,hRule,"no action","rule","menurule","rule","").
        END.
        CREATE SUB-MENU hPlaceHold
               ASSIGN PARENT = hParentSubMenu
                      LABEL = cLabel
                      NAME  = REPLACE(cLabel," ","").
        AddEvent(hCurrObj,hPlaceHold,cLabel,cLabel,"sub-menu",cLabel,"").
        setListAttribute(hCurrObj,"sub-menu" + cLabel,STRING(hPlaceHold),",").
        ASSIGN cMnuLst   = cMnuLst + "," + cLabel
               cPholdLst = appendList(cPholdLst,STRING(hPlaceHold),",").
        LEAVE.
      END.
      hParentSubMenu = hParentSubMenu:NEXT-SIBLING.
    END.
  END.
  ELSE IF NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 AND cBtnMenu = "" THEN 
    bNoMenu = TRUE. 
  ELSE IF cBtnMenu NE "" AND CAN-DO(cMnuLst,cBtnMenu) THEN
    hPlaceHold = WIDGET-HANDLE(ENTRY(LOOKUP(cBtnMenu,cMnuLst),cPholdLst)).
  ELSE IF cMnuLst NE "" THEN DO:
    IF LOOKUP(icMenu,cMnuLst) > 0 THEN
      hPlaceHold = WIDGET-HANDLE(ENTRY(LOOKUP(icMenu,cMnuLst),cPholdLst)).
    ELSE  
      hPlaceHold = WIDGET-HANDLE(ENTRY(1,cPholdLst)).
  END.  

  IF cAction = "" OR cAction = "sub-menu" THEN DO:
    iNumPlaceHold = iNumPlaceHold + 1.
    setAttribute(hCurrObj,"placeholder" + STRING(iNumPlaceHold),STRING(hPlaceHold)).
    NEXT.
  END.

  IF cAction = "commit" THEN
    setAttribute(hCurrObj,"commitstate","off").

  IF cMethod = "CloseRecord" THEN cMethod = "CloseWindow".
  ELSE IF cMethod = "HelpRecord" THEN 
    ASSIGN cMethod = "Help"
           cAccel = "F1"
           cTTip     = cLabel.

  IF ihRect:WIDTH-PIXELS > 20 AND NOT bMenuOnly AND NOT bTgl THEN DO:
    CREATE ttEvent.
    ASSIGN ttEvent.hObject        = hCurrObj
           ttEvent.cAction        = cAction
           ttEvent.cName          = "choose" 
           ttEvent.cWidgetType    = "button"
           ttEvent.cMethod        = cMethod
           ttEvent.cLabel         = cLabel
           .
    IF NOT VALID-HANDLE(hButton) THEN DO:
      CREATE BUTTON ttEvent.hWidget ASSIGN
           FRAME         = (IF ihRect:TYPE = "rectangle" THEN ihRect:FRAME ELSE ihRect)
           AUTO-RESIZE   = TRUE
           FLAT-BUTTON   = IF ihButtonY NE 0 THEN bhButtonFlat 
                           ELSE (IF icMenu NE "" OR cBtnMenu NE "" OR icOtherProp MATCHES "*flat*" THEN TRUE ELSE FALSE)
           X             = IF bRightAdjust THEN 
                             ihRect:X + iStartX + ihRect:WIDTH-PIXELS - iPanelHeight + 2 - iBtnGap - (ix - 1 - iNumPlaceHold - iNumMenuOnly) * 23 
/*                             ihRect:X + iStartX + ihRect:WIDTH-PIXELS - 22 - iBtnGap - (ix - 1 - iNumPlaceHold - iNumMenuOnly) * 23 */ 
                           ELSE
                           /*  ihRect:X + */ iStartX + iBtnGap + MAX(0,ix - 1 - iNumPlaceHold - iNumMenuOnly) * ihButtonWidth + 3 
/*                             (IF icMenu = "" OR (ihButtonY NE 0 AND NOT bhButtonFlat) THEN 23 ELSE 20 ) + 3 */
           Y             = IF ihButtonY NE 0 THEN ihButtonY 
                           ELSE (
                             ihRect:Y + 
                             (IF icMenu NE "" OR cBtnMenu NE "" OR icOtherProp MATCHES "*flat*" THEN 1 ELSE 0) +
                             (IF getAttribute(ihRect,"rectangleSetY") = "yes" THEN 3 ELSE 0))
           TOOLTIP       = cTTip
           WIDTH-PIXELS  = IF getAttribute(SESSION,"BtnImg_" + cAction) NE "" OR cImg NE "" THEN 
                             (IF ihButtonY NE 0 THEN ihButtonWidth ELSE IF icMenu = "" THEN iPanelHeight + 3 ELSE iPanelheight ) 
/*                             (IF ihButtonY NE 0 THEN ihButtonWidth ELSE IF icMenu = "" THEN 23 ELSE 20 ) */ 
                           ELSE FONT-TABLE:GET-TEXT-WIDTH-PIXELS(cLabel + ":":U, ihRect:FRAME:FONT) + 10
           HEIGHT-PIXELS = IF ihButtonY NE 0 THEN ihButtonHeight
                           ELSE (IF icMenu = "" THEN iPanelHeight + 3 ELSE iPanelHeight )
/*                           ELSE (IF icMenu = "" THEN 23 ELSE 20 ) */
           VISIBLE       = TRUE
           SENSITIVE     = TRUE
           LABEL         = IF cLabel MATCHES "*&*" OR cAccel BEGINS "ctrl-" THEN cLabel 
                           ELSE IF cAccel BEGINS "alt-" THEN "&" + cLabel
                           ELSE cLabel
           NAME          = (IF ihRect:NAME NE ? THEN ihRect:NAME ELSE "fr") + "_btn_" + cMethod 
        TRIGGERS:
          ON CHOOSE PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
        END TRIGGERS.
      hButton = ttEvent.hWidget.   
    END.    
    ELSE DO:
      ttEvent.hWidget = hButton.
      IF ihButtonY = 0 AND getAttribute(SESSION,"BtnImg_" + cAction) NE "" OR cImg NE "" THEN DO:
        setAttribute(hCurrObj,"firstStaticButtonY",STRING(hButton:Y)).  
        setAttribute(hCurrObj,"firstStaticButtonFlat",STRING(hButton:FLAT-BUTTON)).  
        setAttribute(hCurrObj,"firstStaticButtonWidth",STRING(hButton:WIDTH-PIXELS)).  
        setAttribute(hCurrObj,"firstStaticButtonHeight",STRING(hButton:HEIGHT-PIXELS)).  
      END.         
      IF hButton:LABEL BEGINS "Button-" OR hButton:LABEL BEGINS "btn-" THEN
        hButton:LABEL = IF cLabel MATCHES "*&*" OR cAccel BEGINS "ctrl-" THEN cLabel 
                        ELSE IF cAccel BEGINS "alt-" THEN "&" + cLabel
                        ELSE cLabel.
      IF hButton:TOOLTIP = "" THEN hButton:TOOLTIP = cTTip.
                        
      ON CHOOSE OF hButton PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
    END.
    
    IF NOT ttEvent.hWidget:FLAT-BUTTON AND getAttribute(ttEvent.hWidget:WINDOW,"HelpTextWidget") NE "" THEN DO:
      ON ENTRY OF ttEvent.hWidget PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"entry-of-widget").
      CREATE bttEvent.
      ASSIGN bttEvent.hObject       = hCurrObj
             bttEvent.hWidget       = ttEvent.hWidget
             bttEvent.cAction       = "entry-of-widget"
             bttEvent.cName         = "entry-of-widget"
             bttEvent.cWidgetType   = "button"
             bttEvent.cMethod       = "EntryOfWidget".
    END.

    ASSIGN cBtnNames = cBtnNames + ttEvent.hWidget:NAME + ","
           cBtnHdls  = cBtnHdls + STRING(ttEvent.hWidget) + ","
           hLastBtn  = ttEvent.hWidget 
           ttEvent.hWidget:TAB-STOP = NOT ttEvent.hWidget:FLAT-BUTTON
           .
    /* GOO: */       
    IF cImg = '' then 
    do: 
      cImg = if can-do('Note,filter,BrowseConfig',cAction) then getAttribute(SESSION,'inSensitive' + cAction + 'Button') else getAttribute(SESSION,'Passive' + cAction + 'Button') .
      if cImg ne '' or cImg ne ? or length(cImg) gt 0 then
      do:
        &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
          bOk = ttEvent.hWidget:LOAD-IMAGE-INSENSITIVE(cImg) no-error.
          IF ERROR-STATUS:ERROR or not bOk THEN
           MESSAGE "Could not find image: " cImg  SKIP(1) length(cImg) skip
                 VIEW-AS ALERT-BOX ERROR TITLE "Programmers error".
        &ENDIF         
      end.
      cImg = ''.
    end.                  
           
    IF cImg = "" THEN cImg = getAttribute(SESSION,"BtnImg_" + cAction).
    IF cImg NE "" THEN DO:
      &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
        ttEvent.hWidget:LOAD-IMAGE(cImg) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          MESSAGE "Couldn't find image: " + cImg SKIP(1)
                  VIEW-AS ALERT-BOX ERROR.
      &ENDIF  
      IF cAction = "filter" THEN DO:
        setAttribute(SESSION,"passivefilterbutton",cImg).
        cPassiveFilterButton = cImg.
      END.
    END.
    ELSE iBtnGap = iBtnGap + FONT-TABLE:GET-TEXT-WIDTH-PIXELS(cLabel + ":":U, ihRect:FRAME:FONT) - (IF icMenu = "" THEN 15 ELSE 12 ) + 3. 

/*    MESSAGE hButton:label skip "iBtnGap" iBtnGap
    VIEW-AS ALERT-BOX. */
    setAttribute(hCurrObj,"startX",STRING(ttEvent.hWidget:X + (IF bRightAdjust THEN 0 ELSE ttEvent.hWidget:WIDTH-PIXELS + iBtnGap) /* - ihRect:X */)).

    setAttribute(hCurrObj,"button" + cAction,STRING(ttEvent.hWidget)).
  END.

  IF (icMenu NE "" OR cBtnMenu NE "") AND NOT bNoMenu AND cLabel NE "" THEN DO:

    IF VALID-HANDLE(hPlaceHold:LAST-CHILD) AND hPlaceHold:LAST-CHILD:TYPE = "sub-menu" THEN DO:
      CREATE MENU-ITEM hRule
        ASSIGN SUBTYPE = "rule"
               PARENT  = hPlaceHold
               .
      AddEvent(hCurrObj,hRule,"no action","rule","menurule","rule","").
    END.
    CREATE ttEvent.
    ASSIGN ttEvent.hObject        = hCurrObj
           ttEvent.cAction        = cAction
           ttEvent.cName          = "choose"
           ttEvent.cWidgetType    = "menu-item"
           ttEvent.cMethod        = cMethod
           ttEvent.cLabel         = cLabel
           .
    IF bTgl THEN
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN TOGGLE-BOX = TRUE
                   PARENT     = hPlaceHold
                   LABEL      = cLabel
                   NAME       = ihRect:NAME + "_mitm_" + cAction
                   TRIGGERS:
                     ON VALUE-CHANGED PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
                   END TRIGGERS.
    ELSE 
      CREATE MENU-ITEM ttEvent.hWidget
            ASSIGN PARENT      = hPlaceHold
                   LABEL       = cLabel
                   NAME        = (IF ihRect:NAME NE ? THEN ihRect:NAME ELSE cAction) + "_mitm_" + cAction 
                   ACCELERATOR = TRIM(cAccel,")")
                   PRIVATE-DATA = IF VALID-HANDLE(hButton) THEN STRING(hButton) ELSE ""
                   TRIGGERS:
                     ON CHOOSE PERSISTENT RUN DoProcessEvent (hObjectSourceProc,"choose").
                  END TRIGGERS.
    setAttribute(hCurrObj,"menu-item" + cAction,STRING(ttEvent.hWidget)).
    setAttribute(hCurrObj,"menu-item" + cLabel,STRING(ttEvent.hWidget)).
  END.
END.

setListAttribute(hCurrObj,"button-names",TRIM(cBtnNames,","),",").
setListAttribute(hCurrObj,"button-handles",TRIM(cBtnHdls,","),",").
setListAttribute(hCurrObj,"rule-handles",TRIM(cRuleHdls,","),",").
setListAttribute(hCurrObj,"actionList",cActionList,",").
setListAttribute(hCurrObj,"sub-menu-handles",cPholdLst,",").
setListAttribute(hCurrObj,"sub-menuList",cMnuLst,",").
IF cEnablLst NE "" THEN 
  setListAttribute(hCurrObj,"configenabledevents",TRIM(cEnablLst,","),",").

IF getAttribute(ihRect,"rectangleSet") NE "yes" AND ihRect:TYPE NE "window" THEN DO:
/*IF cBtnNames NE "" AND NOT bAppend THEN DO:*/
  IF icOtherProp MATCHES "*border*" THEN DO:
    ASSIGN ihRect:HIDDEN = FALSE
           ihRect:X = IF CAN-DO(icOtherProp,"maxborder") THEN 2 ELSE ihRect:X - 2
           ihRect:Y = ihRect:Y - 3
           ihRect:WIDTH-PIXELS = IF CAN-DO(icOtherProp,"maxborder") THEN ihRect:FRAME:WIDTH-PIXELS - 2
                                      ELSE IF CAN-DO(icOtherProp,"leftborder") THEN 3
                                      ELSE hLastBtn:X + hLastBtn:WIDTH-PIXELS - ihRect:X + 2
/*           ihRect:HEIGHT-PIXELS = (IF icMenu = "" THEN 29 ELSE 26 ). */
           ihRect:HEIGHT-PIXELS = (IF icMenu = "" THEN iPanelHeight + 9 ELSE iPanelHeight + 6 ).
    setAttribute(ihRect,"rectangleSetY","yes").       
    IF NOT CAN-DO(icOtherProp,"maxborder") THEN 
      PUBLISH "NeverResize" (ihRect,"X").
    ELSE  
      setAttribute(ihRect,"rectangleSet","yes").
  END.
  ELSE 
    ihRect:HIDDEN = TRUE.
END.
ELSE IF ihRect:TYPE NE "window" AND icOtherProp MATCHES "*hide*" THEN 
  ihRect:HIDDEN = TRUE.

IF NOT icOtherProp MATCHES "*enable*" THEN 
  DYNAMIC-FUNCTION("setToolbar",ihRect,"not avail").

hMenu = WIDGET-HANDLE(getAttribute(hCurrObj,"buttonFilter")) NO-ERROR.
IF VALID-HANDLE(hMenu) THEN
  NewMenuBand(hMenu,"SelectFilter;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Endre filtervindu" ELSE "Change filter window") + "¤enable","").
hObjectSourceProc = ?.

IF CAN-DO("yes,true",getAttribute(SESSION,"NoToolbarRectangle")) THEN ihRect:HIDDEN = YES.

RETURN ihRect.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewViewer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NewViewer Procedure 
FUNCTION NewViewer RETURNS HANDLE
  ( INPUT ihRectangle   AS HANDLE,
    INPUT ihParentQuery AS HANDLE,
    INPUT icViewerProg  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Run the viewer object 
  Notes:   The rectangle object is just a placeholder.
           The viewer object has no standard methods. It's only purpose is to
           set the coordinates and provide automatic clean-up
------------------------------------------------------------------------------*/
DEF VAR hViewer          AS HANDLE NO-UNDO.
DEF VAR hViewerFrame     AS HANDLE NO-UNDO.
DEF VAR iDeltaX          AS INT    NO-UNDO.
DEF VAR iDeltaY          AS INT    NO-UNDO.
DEF VAR hParent          AS HANDLE NO-UNDO.
DEF VAR cPPathCheck      AS CHAR   NO-UNDO.

hParent = SOURCE-PROCEDURE.

IF SEARCH(icViewerProg) = ? AND SEARCH(SUBSTR(icViewerProg,1,LENGTH(icViewerProg) - 1) + "r") = ? THEN DO:
  cPPathCheck = REPLACE(icViewerProg,"/","~\").
  cPPathCheck = ENTRY(NUM-ENTRIES(cPPathCheck,"~\"),cPPathCheck,"~\").
  IF SEARCH(cPPathCheck) NE ? OR SEARCH(SUBSTR(cPPathCheck,1,LENGTH(cPPathCheck) - 1) + "r") NE ? THEN
    icViewerProg = cPPathCheck.
  ELSE DO:
    MESSAGE "Could not find viewer procedure " icViewerProg
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN ?.
  END.
END.

RUN VALUE(icViewerProg) PERSIST SET hViewer.
IF CAN-DO(hViewer:INTERNAL-ENTRIES,"setParent") THEN
  DYNAMIC-FUNCTION("setParent" IN hViewer,hParent).
ELSE DO:
  MESSAGE "A viewer procedure must contain the setParent(parent-procedure handle) function" VIEW-AS ALERT-BOX ERROR.
  RETURN ?.
END.
IF CAN-DO(hViewer:INTERNAL-ENTRIES,"getFrameHandle") THEN
  hViewerFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hViewer) NO-ERROR.
ELSE DO:
  MESSAGE "A viewer procedure must contain the getFrameHandle function" SKIP
          "(that should contain RETURN FRAME {&FRAME-NAME}:HANDLE)"
          VIEW-AS ALERT-BOX ERROR.
  RETURN ?.
END.

FIND FIRST ttObject WHERE ttObject.hObject = hViewer NO-ERROR.
IF NOT AVAIL ttObject THEN DO:
  CREATE ttObject.
  ASSIGN ttObject.hWindow       = ihRectangle:WINDOW
         ttObject.hObject       = hViewer
         ttObject.cObjectType   = "Viewer"
         ttObject.hDesignObject = ihRectangle
         ttObject.hSourceProc   = hParent
         ttObject.cGenProc      = PROGRAM-NAME(1)
         ttObject.cInitProc     = PROGRAM-NAME(2)
         ttObject.cObjectName   = icViewerProg
         .
END.

FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihRectangle:
  ttAttribute.hObject = hViewer.
END.

ASSIGN hViewerFrame:X             = DYNAMIC-FUNCTION("getAbsPosition",ihRectangle,"X")
       hViewerFrame:Y             = DYNAMIC-FUNCTION("getAbsPosition",ihRectangle,"Y")
       iDeltaX      = ihRectangle:WIDTH-PIXELS - hViewerFrame:WIDTH-PIXELS
       iDeltaY      = ihRectangle:HEIGHT-PIXELS - hViewerFrame:HEIGHT-PIXELS
       hViewerFrame:WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS
       hViewerFrame:HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS
       hViewerFrame:VIRTUAL-WIDTH-PIXELS  = ihRectangle:WIDTH-PIXELS
       hViewerFrame:VIRTUAL-HEIGHT-PIXELS = ihRectangle:HEIGHT-PIXELS
       .

DYNAMIC-FUNCTION("CreateParentLink",hViewer,hParent,"viewer").

IF CAN-DO(hViewer:INTERNAL-ENTRIES,"setParent") THEN
  DYNAMIC-FUNCTION("setParent" IN hViewer,hParent) NO-ERROR.
IF CAN-DO(hViewer:INTERNAL-ENTRIES,"getFrameHandle") THEN 
  DYNAMIC-FUNCTION("setAttribute",hParent,"viewerframe",
                   STRING(DYNAMIC-FUNCTION("getFrameHandle" IN hViewer))).

IF CAN-DO(hParent:INTERNAL-ENTRIES,"InvalidateHandle") THEN
  SUBSCRIBE PROCEDURE hParent TO "InvalidateHandle" IN hViewer.
IF CAN-DO(hParent:INTERNAL-ENTRIES,"InitializeViewer") THEN DO:
  DYNAMIC-FUNCTION("InitializeViewer" IN hParent,hViewer) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    DYNAMIC-FUNCTION("InitializeViewer" IN hParent) NO-ERROR.
END.

IF CAN-DO(hViewer:INTERNAL-ENTRIES,"InitializeResize") THEN
  DYNAMIC-FUNCTION("InitializeResize" IN hViewer) NO-ERROR.

DYNAMIC-FUNCTION("setDeltaX",iDeltaX).
DYNAMIC-FUNCTION("setDeltaY",iDeltaY).
IF VALID-HANDLE(hViewerFrame:FIRST-CHILD:FIRST-CHILD) THEN
  DYNAMIC-FUNCTION("DoWidgetResize",hViewerFrame:FIRST-CHILD:FIRST-CHILD). 
DYNAMIC-FUNCTION("setDeltaX",0).
DYNAMIC-FUNCTION("setDeltaY",0).

IF VALID-HANDLE(ihParentQuery) THEN DO:
  IF CAN-DO(hViewer:INTERNAL-ENTRIES,"setQuery") THEN 
    DYNAMIC-FUNCTION("setQuery" IN hViewer,ihParentQuery) NO-ERROR.
  ELSE  
    MESSAGE "Function to set the query ('setQuery') missing in viewer procedure " + icViewerProg
            VIEW-AS ALERT-BOX ERROR.
END.

IF CAN-DO(hViewer:INTERNAL-ENTRIES,"InitializeObject") THEN
  RUN InitializeObject IN hViewer NO-ERROR.
ELSE  
  MESSAGE "Procedure 'InitializeObject' missing in viewer procedure " icViewerProg
          VIEW-AS ALERT-BOX ERROR.

RETURN hViewer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProcessQuery Procedure 
FUNCTION ProcessQuery RETURNS LOGICAL
  ( INPUT ihQueryObject   AS HANDLE,
    INPUT icServerProgram AS CHAR,
    INPUT icParamList     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Grab the various attributes for a query object (browse or query)
           and pass them on to a standard server routine that will produce the
           same resultset and pass on the buffer to a custom handling routine 
    Notes: Corresponds to ProcessSelectedRows
------------------------------------------------------------------------------*/
DEF VAR cQueryCriteria   AS CHAR   NO-UNDO.
DEF VAR cTemp            AS CHAR   NO-UNDO.
DEF VAR cTemp2           AS CHAR   NO-UNDO.
DEF VAR cDistinctColumns AS CHAR   NO-UNDO.
DEF VAR cAccumFields     AS CHAR   NO-UNDO.
DEF VAR cDistBufFldList  AS CHAR   NO-UNDO.
DEF VAR cCurrBuffer      AS CHAR   NO-UNDO.
DEF VAR cFieldDef        AS CHAR   NO-UNDO.
DEF VAR bCalcField       AS LOG    NO-UNDO.
DEF VAR cMatchQuery      AS CHAR   NO-UNDO.
DEF VAR cAccBufFldList   AS CHAR   NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR iy               AS INT    NO-UNDO.
DEF VAR httCheck         AS HANDLE NO-UNDO.
DEF VAR bOK              AS LOG    NO-UNDO.

IF NOT VALID-HANDLE(ihQueryObject) THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Invalid browse or query handle" SKIP
          VIEW-AS ALERT-BOX.
  RETURN NO.
END.

ASSIGN cTemp = REPLACE(getAttribute(ihQueryObject,"buffersandfields"),"!","")
       cTemp = REPLACE(cTemp,";RowCount","")
       cTemp = REPLACE(cTemp,";RowIdent1","")
       cTemp = REPLACE(cTemp,";RowIdent2","")
       cTemp = REPLACE(cTemp,";RowIdent3","")
       cTemp = REPLACE(cTemp,";RowIdent4","")
       cTemp = REPLACE(cTemp,";RowIdent5","")
       cTemp = REPLACE(cTemp,";RowIdent6","")
       cTemp = REPLACE(cTemp,";RowIdent7","")
       cTemp = REPLACE(cTemp,";RowIdent8","")
       cTemp = REPLACE(cTemp,";RowIdent9","")
       cTemp = REPLACE(cTemp,";distinct ",";")
       cTemp = REPLACE(cTemp,"+distinct ","+")
       cTemp = REPLACE(cTemp,";accum ",";")
       cTemp = REPLACE(cTemp,"+accum ","+")
       cQueryCriteria = DYNAMIC-FUNCTION("FixQuery",
                                         getAttribute(ihQueryObject,"basequery") + 
                                         getAttribute(ihQueryObject,"queryfilter") + 
                                         getAttribute(ihQueryObject,"querywhere") + 
                                         (IF getAttribute(ihQueryObject,"use-index") NE "" THEN
                                           " USE-INDEX " + getAttribute(ihQueryObject,"use-index")
                                          ELSE "") +
                                         getAttribute(ihQueryObject,"queryjoin"))
       cDistinctColumns = getAttribute(ihQueryObject,"distinctcolumns")
       cAccumFields = getAttribute(ihQueryObject,"accumfields")
       .


IF ihQueryObject:TYPE = "browse" AND (cDistinctColumns NE "" OR cAccumFields NE "") THEN DO:
  DO ix = 1 TO ihQueryObject:NUM-COLUMNS:
    IF CAN-DO(cDistinctColumns,ihQueryObject:GET-BROWSE-COLUMN(ix):NAME) THEN
      cDistBufFldList = cDistBufFldList + getAttribute(ihQueryObject,"fieldbuffer" + ihQueryObject:GET-BROWSE-COLUMN(ix):NAME) 
                      + getAttribute(ihQueryObject,"orgdbfield" + ihQueryObject:GET-BROWSE-COLUMN(ix):NAME) + ",".
    ELSE IF CAN-DO(cAccumFields,ihQueryObject:GET-BROWSE-COLUMN(ix):NAME) THEN
      cAccBufFldList = cAccBufFldList + getAttribute(ihQueryObject,"fieldbuffer" + ihQueryObject:GET-BROWSE-COLUMN(ix):NAME) 
                      + getAttribute(ihQueryObject,"orgdbfield" + ihQueryObject:GET-BROWSE-COLUMN(ix):NAME) + ",".
  END.
  ASSIGN cDistBufFldList = TRIM(cDistBufFldList,",")
         cAccBufFldList  = TRIM(cAccBufFldList,",").
  DO ix = 1 TO NUM-ENTRIES(cTemp):
    ASSIGN cTemp2 = cTemp2 + (IF cTemp2 NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,cTemp),";") + ";"
           cCurrBuffer = ENTRY(1,ENTRY(ix,cTemp),";"). 
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,cTemp),";"):
      ASSIGN cFieldDef = TRIM(ENTRY(1,ENTRY(iy,ENTRY(ix,cTemp),";"),"|"),"+")
             bCalcField = ENTRY(iy,ENTRY(ix,cTemp),";") BEGINS "+".
      IF cDistinctColumns NE "" AND 
         CAN-DO(cDistBufFldList,cCurrBuffer + cFieldDef) AND
         (CAN-DO(cDistinctColumns,cFieldDef) OR
          CAN-DO(cDistinctColumns,cFieldDef + STRING(ix))) THEN
        cTemp2 = cTemp2 + (IF bCalcField THEN "+" ELSE "") + "distinct " + TRIM(ENTRY(iy,ENTRY(ix,cTemp),";"),"+") + ";".
      ELSE IF cAccumFields NE "" AND 
         CAN-DO(cAccBufFldList,cCurrBuffer + cFieldDef) AND
         (CAN-DO(cAccumFields,cFieldDef) OR
          CAN-DO(cAccumFields,cFieldDef + STRING(ix))) THEN
        cTemp2 = cTemp2 + (IF bCalcField THEN "+" ELSE "") + "accum " + TRIM(ENTRY(iy,ENTRY(ix,cTemp),";"),"+") + ";".
      ELSE
        cTemp2 = cTemp2 + ENTRY(iy,ENTRY(ix,cTemp),";") + ";".
    END.
    cTemp2 = TRIM(cTemp2,";").
  END.
  cTemp = cTemp2.
END.
  
cTemp = DYNAMIC-FUNCTION("AddCalcParam",cTemp,ihQueryObject).
    
IF getAttribute(ihQueryObject,"keepsearchvalue") = "yes" AND getAttribute(ihQueryObject,"prescanqueryfilter") NE "" AND
   getAttribute(ihQueryObject,"basetable") = getAttribute(ihQueryObject,"sortbuffer") 
   THEN DO:
  FIND FIRST bbttObjectLink
       WHERE bbttObjectLink.hFromObject = ihQueryObject
         AND bbttObjectLink.cLinkType   = "browse-search-field"
       NO-ERROR.
  IF AVAIL bbttObjectLink AND 
     DYNAMIC-FUNCTION("getBufferFieldDataType",ihQueryObject:QUERY:GET-BUFFER-HANDLE(1),bbttObjectLink.cLinkInfo) = "character" AND
     INDEX(bbttObjectLink.hToObject:INPUT-VALUE,"*") > 0 THEN
    cMatchQuery = getAttribute(ihQueryObject,"sortbuffer") + " WHERE " + getAttribute(ihQueryObject,"sortbuffer") + "." + bbttObjectLink.cLinkInfo
                + " MATCHES '" + bbttObjectLink.hToObject:INPUT-VALUE + "'".
END.
IF getAttribute(ihQueryObject,"prescanBaseQuery") NE "" OR 
   getAttribute(ihQueryObject,"prescanqueryfilter") NE "" OR 
   getAttribute(ihQueryObject,"prescanquerywhere") NE "" THEN
  DYNAMIC-FUNCTION("setPreScanQuery",TRIM(getAttribute(ihQueryObject,"prescanBaseQuery") + "|" + 
                                          getAttribute(ihQueryObject,"prescanqueryfilter") + "|" + 
                                          getAttribute(ihQueryObject,"prescanquerywhere") + "|" +
                                          cMatchQuery
                                          ,"|")).
ELSE IF getAttribute(ihQueryObject,"prescanqueryfilter") NE "" THEN
  DYNAMIC-FUNCTION("setPreScanQuery",cMatchQuery).

IF getAttribute(ihQueryObject,"prescanlimit") NE "" THEN
  DYNAMIC-FUNCTION("setPreScanLimit",INT(getAttribute(ihQueryObject,"prescanlimit"))).
DYNAMIC-FUNCTION("setCalcFieldProc",getAttribute(ihQueryObject,"calcfieldproc")).
DYNAMIC-FUNCTION("setCalcFieldProc",getAttribute(ihQueryObject,"calcfieldproc")).
DYNAMIC-FUNCTION("setCalcFieldFilter",DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"calcfieldfilter")
                 + (IF DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"calcfieldfilter") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"calcfieldwhere") NE "" THEN "|" ELSE "")
                 + DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"calcfieldwhere")
                 ).

IF icServerProgram = "jbserv_gettemptablejoin.p" THEN DO:
  httCheck = DYNAMIC-FUNCTION("getTempTableJoin",1,0,"",cTemp,cQueryCriteria).
  httCheck:DEFAULT-BUFFER-HANDLE:FIND-FIRST("WHERE true") NO-ERROR.
  bOK = httCheck:DEFAULT-BUFFER-HANDLE:AVAIL.
  DELETE OBJECT httCheck.
  RETURN bOk.
END.
ELSE DO:
  IF NOT DYNAMIC-FUNCTION("getTTJoinOnServer",getAttribute(ihQueryObject,"querydir"),
                           cTemp,
                           cQueryCriteria +
                           IF getAttribute(ihQueryObject,"querysort") NE "" THEN
                            " BY " + getAttribute(ihQueryObject,"querysort") 
                          + (IF getAttribute(ihQueryObject,"querydesc") NE "" THEN " DESC" ELSE "")
                           ELSE "",
                          icServerProgram,
                          icParamList) THEN DO:
    IF getAttribute(ihQueryObject,"viewErrorsFromProcessRows") NE "no" THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    RETURN NO.
  END.
  ELSE RETURN YES. 
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessSelectedRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProcessSelectedRows Procedure 
FUNCTION ProcessSelectedRows RETURNS LOGICAL
  ( INPUT ihBrowse        AS HANDLE,
    INPUT icServerProgram AS CHAR,
    INPUT icParamList     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch the selected rows from a browse and pass them as temp-table
           to the server. 
    Notes: Corresponds to ProcessQuery that will take the entire resultset
------------------------------------------------------------------------------*/
DEF VAR hTempTable    AS HANDLE NO-UNDO.
DEF VAR hSourceBuffer AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer AS HANDLE NO-UNDO.
DEF VAR hRefreshQuery AS HANDLE NO-UNDO.
DEF VAR iCnt          AS INT    NO-UNDO.
DEF VAR cRowIdList    AS CHAR   NO-UNDO.
DEF VAR cTransMsg     AS CHAR   NO-UNDO.
DEF VAR iNumRowIds    AS INT    NO-UNDO.
DEF VAR cFindString   AS CHAR   NO-UNDO.

DEF BUFFER ttObject FOR ttObject. 

IF NOT VALID-HANDLE(ihBrowse) THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          "Invalid browse handle" SKIP
          VIEW-AS ALERT-BOX.
  RETURN NO.
END.

FIND ttObject WHERE ttObject.hObject = ihBrowse.

hSourceBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).

CREATE TEMP-TABLE hTempTable. 
hTempTable:CREATE-LIKE(hSourceBuffer).
hTempTable:TEMP-TABLE-PREPARE(hSourceBuffer:NAME).

hTargetBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
       
DO ix = 1 TO ihBrowse:NUM-SELECTED-ROWS:
  IF ihBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
    hTargetBuffer:BUFFER-CREATE().  
    hTargetBuffer:BUFFER-COPY(hSourceBuffer).
  END.
END.

IF NOT DYNAMIC-FUNCTION("runproc",icServerProgram,icParamList,hTempTable) THEN DO:    
  IF getAttribute(ihBrowse,"viewErrorsFromProcessRows") NE "no" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  DELETE OBJECT hTempTable.  
  RETURN NO.
END.
ELSE DO: 
  IF getAttribute(ihBrowse,"useLocalData") = "yes" THEN DO:
    iNumRowIds = NUM-ENTRIES(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"buffersandfields")).
    hTempTable = DYNAMIC-FUNCTION("getRunProcReturnTable",?).
    hTargetBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY hRefreshQuery.
    hRefreshQuery:SET-BUFFERS(hTargetBuffer).
    hRefreshQuery:QUERY-PREPARE("FOR EACH " + hTargetBuffer:NAME).
    hRefreshQuery:QUERY-OPEN().
    hRefreshQuery:GET-FIRST().
    REPEAT WHILE NOT hRefreshQuery:QUERY-OFF-END:
      cFindString = "WHERE ".
      DO ix = 1 TO iNumRowIds:
        cFindString = cFindString + "RowIdent" + STRING(ix) + " = '" + hTargetBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)):BUFFER-VALUE + "' AND ".
      END.
      cFindString = RIGHT-TRIM(cFindString," AND ").
      bOk = hSourceBuffer:FIND-UNIQUE(cFindString) NO-ERROR.
      IF bOk THEN 
        hSourceBuffer:BUFFER-COPY(hTargetBuffer).  
      hRefreshQuery:GET-NEXT().
    END.
    ihBrowse:REFRESH().  
    DELETE OBJECT hRefreshQuery NO-ERROR.
    DELETE OBJECT hTempTable.  
  END.  
  ELSE IF CAN-DO(ttObject.hSourceProc:INTERNAL-ENTRIES,"fetchProcessedRows") THEN DO:
    DYNAMIC-FUNCTION("getRunProcReturnTable",hSourceBuffer). 
    DYNAMIC-FUNCTION("fetchProcessedRows" IN ttObject.hSourceProc,hSourceBuffer).
  END.
  ELSE DO:
    IF getAttribute(ihBrowse,"RefreshProcessedRows") NE "no" THEN DO:
      cTransMsg = DYNAMIC-FUNCTION("getTransactionMessage").
      CREATE QUERY hRefreshQuery.
      hRefreshQuery:SET-BUFFERS(hTargetBuffer).
      hRefreshQuery:QUERY-PREPARE("FOR EACH " + hTargetBuffer:NAME).
      hRefreshQuery:QUERY-OPEN().
      hRefreshQuery:GET-FIRST().
      REPEAT WHILE NOT hRefreshQuery:QUERY-OFF-END:
        cRowIdList = cRowIdList + hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + ",".
        IF ix MOD 100 = 0 THEN DO:
          DYNAMIC-FUNCTION("RefreshRowids",ihBrowse,TRIM(cRowIdList,",")).
          cRowIdList = "".
        END.
        hRefreshQuery:GET-NEXT().
      END.
      IF cRowIdList NE "" THEN
        DYNAMIC-FUNCTION("RefreshRowids",ihBrowse,TRIM(cRowIdList,",")).
      
      DELETE OBJECT hRefreshQuery NO-ERROR.
      DYNAMIC-FUNCTION("setTransactionMessage",cTransMsg).
    END.
    DELETE OBJECT hTempTable.  
  END. 

  RETURN YES. 
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReplaceObjectLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReplaceObjectLink Procedure 
FUNCTION ReplaceObjectLink RETURNS LOGICAL
  ( INPUT ihObject1  AS HANDLE,
    INPUT ihObject2  AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Replace the links between any of the input objects to any other object of the same
           type as the new pair 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hObject3  AS HANDLE NO-UNDO.

FIND ttObject WHERE ttObject.hObject = ihObject1 NO-ERROR.
FIND bttObject WHERE bttObject.hObject = ihObject2 NO-ERROR.
IF AVAIL ttObject AND AVAIL bttObject THEN DO: 
  FIND FIRST ttObjectLink WHERE ttObjectLink.hFromObject = ihObject1 
                            AND ttObjectLink.cLinkType   = bttObject.cObjectType NO-ERROR.

  IF AVAIL ttObjectLink THEN DO: 
    hObject3 = ttObjectLink.hToObject.
    DELETE ttObjectLink.
  END.
  ELSE DO:
    FIND FIRST ttObjectLink WHERE ttObjectLink.hFromObject = ihObject2 
                              AND ttObjectLink.cLinkType   = ttObject.cObjectType NO-ERROR.
    IF AVAIL ttObjectLink THEN DO: 
      hObject3 = ttObjectLink.hToObject.
      DELETE ttObjectLink.
    END.
  END.

  FIND FIRST ttObjectLink WHERE ttObjectLink.hFromObject = hObject3 
                            AND ttObjectLink.cLinkType   = ttObject.cObjectType NO-ERROR.
  IF AVAIL ttObjectLink THEN DELETE ttObjectLink.
  ELSE DO:
    FIND FIRST ttObjectLink WHERE ttObjectLink.hFromObject = hObject3 
                              AND ttObjectLink.cLinkType   = bttObject.cObjectType NO-ERROR.
    IF AVAIL ttObjectLink THEN 
      DELETE ttObjectLink.
  END.

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihObject1
         ttObjectLink.hToObject   = ihObject2  
         ttObjectLink.cLinkType   = bttObject.cObjectType. 

  CREATE ttObjectLink.
  ASSIGN ttObjectLink.hFromObject = ihObject2
         ttObjectLink.hToObject   = ihObject1  
         ttObjectLink.cLinkType   = ttObject.cObjectType. 

  IF ttObject.cObjectType = "toolbar" THEN
    InitToolbarWidgets(ttObject.hObject,bttObject.hObject).
  ELSE IF bttObject.cObjectType = "toolbar" THEN
    InitToolbarWidgets(bttObject.hObject,ttObject.hObject).

/*   IF ttObject.cObjectType = "browse" AND bttObject.cObjectType = "fieldmap" THEN       */
/*     CopyAttributes(ttObject.hObject,bttObject.hObject,"nodb*accessfields").            */
/*   ELSE IF ttObject.cObjectType = "fieldmap" AND bttObject.cObjectType = "browse" THEN  */
/*     CopyAttributes(bttObject.hObject,ttObject.hObject,"nodb*accessfields").            */
/*   ELSE IF ttObject.cObjectType = "query" AND bttObject.cObjectType = "fieldmap" THEN   */
/*     CopyAttributes(ttObject.hObject,bttObject.hObject,"nodb*accessfields").            */
/*   ELSE IF ttObject.cObjectType = "fieldmap" AND bttObject.cObjectType = "query" THEN   */
/*     CopyAttributes(bttObject.hObject,ttObject.hObject,"nodb*accessfields").            */
/*                                                                                        */
  IF ttObject.cObjectType = "browse" AND bttObject.cObjectType = "toolbar" THEN
    setActionDbSecurity(bttObject.hObject,ttObject.hObject).
  ELSE IF ttObject.cObjectType = "toolbar" AND bttObject.cObjectType = "browse" THEN
    setActionDbSecurity(ttObject.hObject,bttObject.hObject).
  ELSE IF ttObject.cObjectType = "query" AND bttObject.cObjectType = "toolbar" THEN
    setActionDbSecurity(bttObject.hObject,ttObject.hObject).
  ELSE IF ttObject.cObjectType = "toolbar" AND bttObject.cObjectType = "query" THEN
    setActionDbSecurity(ttObject.hObject,bttObject.hObject).

  RETURN TRUE.
END.
ELSE MESSAGE "Invalid sourceobject for linking:" + CHR(10) +
             (IF NOT AVAIL ttObject AND VALID-HANDLE(ihObject1) THEN ihObject1:NAME 
              ELSE IF NOT AVAIL ttObject AND NOT VALID-HANDLE(ihObject1) THEN "Invalid handle for parameter 1" 
              ELSE IF NOT AVAIL bttObject AND VALID-HANDLE(ihObject2) THEN ihObject2:NAME 
              ELSE "Invalid handle for parameter 2") +
              (IF VALID-HANDLE(ihObject1) THEN CHR(10) + "Parameter 1: " + ihObject1:NAME ELSE "") +
              (IF VALID-HANDLE(ihObject2) THEN CHR(10) + "Parameter 2: " + ihObject2:NAME ELSE "") SKIP(1)
               PROGRAM-NAME(1) SKIP
               PROGRAM-NAME(2) VIEW-AS ALERT-BOX ERROR TITLE 
               "JukeBox programmers mistake".

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResetBufferSequence) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ResetBufferSequence Procedure 
FUNCTION ResetBufferSequence RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF getAttribute(ihQueryObject,"orgbuffersandfields") NE "" THEN DO:
  setAttribute(ihQueryObject,"SkipUniqueRows","").
  setAttribute(ihQueryObject,"buffersandfields",getAttribute(ihQueryObject,"orgbuffersandfields")).
  setAttribute(ihQueryObject,"queryjoin",getAttribute(ihQueryObject,"orgqueryjoin")).
  setAttribute(ihQueryObject,"basequery",getAttribute(ihQueryObject,"orgbasequery")).
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ResetMenuToggle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ResetMenuToggle Procedure 
FUNCTION ResetMenuToggle RETURNS LOGICAL
  ( INPUT ihMenu       AS HANDLE,
    INPUT ihExceptItem AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Reset toggle for a menu-item or a sub-menu 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hMenuItem AS HANDLE NO-UNDO.

IF ihMenu NE ihExceptItem AND ihMenu:TYPE = "menu-item" AND ihMenu:TOGGLE-BOX THEN DO:
  ihMenu:CHECKED = NO.
  RETURN YES.
END.
ELSE IF ihMenu:TYPE NE "menu-item" THEN DO:
  hMenuItem = ihMenu:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hMenuItem):
    IF hMenuItem NE ihExceptItem AND hMenuItem:TYPE = "menu-item" AND hMenuItem:TOGGLE-BOX THEN
      hMenuItem:CHECKED = NO.
    hMenuItem = hMenuItem:NEXT-SIBLING.
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActionDbSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setActionDbSecurity Procedure 
FUNCTION setActionDbSecurity RETURNS LOGICAL
  ( INPUT ihMenuOwnerOrToolbar AS HANDLE,
    INPUT ihSourceQuery        AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose: Check for database restrictions and disable toolbar and menu actions and accordingly 
    Notes: Invoked automatically when a browse or query is linked to the toolbar or when a pop-up menu is 
           created for a browse.
           (Attributes set on ihMenuOwnerOrToolbar can be overriden)   
------------------------------------------------------------------------------*/
DEF VAR cCurrDbSecDisabled       AS CHAR NO-UNDO.
DEF VAR cNoDbReadAccessBuffers   AS CHAR NO-UNDO.
DEF VAR cNoDbWriteAccessBuffers  AS CHAR NO-UNDO.
DEF VAR cNoDbCreateAccessBuffers AS CHAR NO-UNDO.
DEF VAR cNoDbDeleteAccessBuffers AS CHAR NO-UNDO.
DEF VAR cBaseTable               AS CHAR NO-UNDO.

ASSIGN cCurrDbSecDisabled       = getAttribute(ihMenuOwnerOrToolbar,"dbsecdisbledactions")
       cNoDbReadAccessBuffers   = getAttribute(ihSourceQuery,"nodbreadaccessbuffers")
       cNoDbWriteAccessBuffers  = getAttribute(ihSourceQuery,"nodbwriteaccessbuffers")
       cNoDbCreateAccessBuffers = getAttribute(ihSourceQuery,"nodbcreateaccessbuffers")
       cNoDbDeleteAccessBuffers = getAttribute(ihSourceQuery,"nodbdeleteaccessbuffers")
       cBaseTable               = getAttribute(ihSourceQuery,"basetable")
       .

IF cBaseTable = "" THEN RETURN YES.

IF CAN-DO(cNoDbReadAccessBuffers,cBaseTable) THEN 
  setAttribute(ihMenuOwnerOrToolbar,"dbsecdisabledactions","*").
ELSE IF CAN-DO(cNoDbWriteAccessBuffers,cBaseTable) THEN 
  setAttribute(ihMenuOwnerOrToolbar,"dbsecdisabledactions","new,copy,delete,save").
ELSE DO:
  IF CAN-DO(cNoDbCreateAccessBuffers,cBaseTable) THEN 
    setAttribute(ihMenuOwnerOrToolbar,"dbsecdisabledactions","new,copy").
  IF CAN-DO(cNoDbDeleteAccessBuffers,cBaseTable) THEN 
    setAttribute(ihMenuOwnerOrToolbar,"dbsecdisabledactions","delete").
  ELSE
    setAttribute(ihMenuOwnerOrToolbar,"dbsecdisabledactions","").
END.

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
DEF VAR hBuffer   AS HANDLE NO-UNDO.
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
  setDefaultButton(icName,icValue).

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAttributeList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAttributeList Procedure 
FUNCTION setAttributeList RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icList    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set attributes corresponding to user settings 
    Notes: Accepts values corresponding to the user getUserSetting function: 
           <name,name,..|<value>CHR1<value> 
------------------------------------------------------------------------------*/
DEF VAR cValueList   AS CHAR NO-UNDO.
DEF VAR cSettingList AS CHAR NO-UNDO.

ASSIGN cSettingList = ENTRY(1,icList,"|")
       cValueList   = ENTRY(2,icList,"|").

DO ix = 1 TO NUM-ENTRIES(cSettingList):
  IF INDEX(ENTRY(ix,cSettingList),"_") > 0 THEN
    msetAttribute(ihObject,SUBSTR(ENTRY(ix,cSettingList),1,INDEX(ENTRY(ix,cSettingList),"_")),"").
END.
DO ix = 1 TO NUM-ENTRIES(cSettingList):
  FIND FIRST ttAttribute
       WHERE ttAttribute.hObject = ihObject
         AND ttAttribute.cName   = ENTRY(ix,cSettingList)
       NO-ERROR.
  IF NOT AVAIL ttAttribute THEN DO:
    CREATE ttAttribute.
    ASSIGN ttAttribute.hObject = ihObject
           ttAttribute.cName   = ENTRY(ix,cSettingList).
  END.
  ttAttribute.cValue = ENTRY(ix,cValueList,CHR(1)).
END.

RETURN YES.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBehaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBehaviour Procedure 
FUNCTION setBehaviour RETURNS LOGICAL
  ( INPUT icBehaviour AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Set session preferences
    Notes:  Here for historic reasons (from before the existence of SESSION in ttObject).
            Further on, use setAttribute(SESSION...
------------------------------------------------------------------------------*/
cBehaviour = icBehaviour.

DO ix = 1 TO NUM-ENTRIES(cBehaviour):
  CASE ENTRY(1,ENTRY(ix,cBehaviour),"|"):
    WHEN "DefaultSortFont"     THEN DO:
      iDefaultSortFont     = INT(ENTRY(2,ENTRY(ix,cBehaviour),"|")).
      setAttribute(SESSION,"DefaultSortFont",STRING(iDefaultSortFont)).
    END.
    WHEN "DefaultSortColor"    THEN DO:
      iDefaultSortColor    = INT(ENTRY(2,ENTRY(ix,cBehaviour),"|")).
      setAttribute(SESSION,"DefaultSortColor",STRING(iDefaultSortColor)).
    END.
    WHEN "BrowseSearchDefault" THEN DO:
      cBrowseSearchDefault = ENTRY(2,ENTRY(ix,cBehaviour),"|").
      setAttribute(SESSION,"BrowseSearchDefault",cBrowseSearchDefault).
    END.
    WHEN "TabOnReturn"         THEN DO:
      bTabOnReturn = IF ENTRY(2,ENTRY(ix,cBehaviour),"|") = "yes" THEN TRUE ELSE FALSE.
      setAttribute(SESSION,"TabOnReturn",STRING(bTabOnReturn)).
    END.
    WHEN "SetSortLabel"        THEN DO:
      bSetSortLabel        = IF ENTRY(2,ENTRY(ix,cBehaviour),"|") = "yes" THEN TRUE ELSE FALSE.
      setAttribute(SESSION,"SetSortLabel",STRING(bSetSortLabel)).
    END.
    WHEN "DefaultActionList"   THEN DO:
      cDefActionList       = ENTRY(2,ENTRY(ix,cBehaviour),"|").
      setAttribute(SESSION,"DefActionList",cDefActionList).
    END.
    WHEN "ActiveFilterButton" THEN DO:
      cActiveFilterButton       = ENTRY(2,ENTRY(ix,cBehaviour),"|").
      setAttribute(SESSION,"ActiveFilterButton",cActiveFilterButton).
    END.
    WHEN "DefaultImageList"    THEN DO:
      cDefImageList        = REPLACE(ENTRY(2,ENTRY(ix,cBehaviour),"|"),";",",").
      setAttribute(SESSION,"DefImageList",cDefImageList).
    END.
    WHEN "CtrlHotkeyActions"   THEN DO:
      cCtrlHotkeyActions   = REPLACE(ENTRY(2,ENTRY(ix,cBehaviour),"|"),";",",").
      setAttribute(SESSION,"CtrlHotkeyActions",cCtrlHotkeyActions).
    END.
    WHEN "CtrlHotkeys"         THEN DO:
      cCtrlHotkeys         = REPLACE(ENTRY(2,ENTRY(ix,cBehaviour),"|"),";",",").
      setAttribute(SESSION,"CtrlHotkeys",cCtrlHotkeys).
    END.
    WHEN "KeepExcel"         THEN DO:
      bKeepExcel = IF ENTRY(2,ENTRY(ix,cBehaviour),"|") = "yes" THEN TRUE ELSE FALSE.
      setAttribute(SESSION,"KeepExcel",STRING(bKeepExcel)).
    END.
    OTHERWISE DO:

    END.
  END CASE.
END.
DO ix = 1 TO MIN(NUM-ENTRIES(cDefActionList),NUM-ENTRIES(cDefImageList)):
  IF SEARCH(ENTRY(ix,cDefImageList)) NE ? THEN
    setAttribute(SESSION,"BtnImg_" + ENTRY(ix,cDefActionList),ENTRY(ix,cDefImageList)).
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrowseColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBrowseColumns Procedure 
FUNCTION setBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse       AS HANDLE,
    INPUT icColumns      AS CHAR,
    INPUT ibHideOverlays AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cAllViewFields    AS CHAR   NO-UNDO.
DEF VAR bResize           AS LOG    NO-UNDO.
DEF VAR hColumn           AS HANDLE NO-UNDO.
DEF VAR hWindow           AS HANDLE NO-UNDO.
DEF VAR cNoEditFields     AS CHAR   NO-UNDO.
DEF VAR cWidthPixelsList  AS CHAR   NO-UNDO.
DEF VAR cColumnName       AS CHAR   NO-UNDO.
DEF VAR cNoDbReadAccess   AS CHAR   NO-UNDO.
DEF VAR cNoDbWriteFields  AS CHAR   NO-UNDO.
DEF VAR cNoDbWriteBuffers AS CHAR   NO-UNDO.

IF icColumns NE "" 
   OR getAttribute(ihBrowse,"nodbreadaccessfields") NE "" 
   OR getAttribute(ihBrowse,"nodbwriteaccessfields") NE "" 
   OR getAttribute(ihBrowse,"nodbwriteaccessbuffers") NE "" 
   THEN DO:

  ASSIGN cAllViewFields    = getAttribute(ihBrowse,"allviewfields")
         cNoEditFields     = getAttribute(ihBrowse,"noeditfields")
         cWidthPixelsList  = getAttribute(ihBrowse,"widthpixelslist")
         cNoDbReadAccess   = getAttribute(ihBrowse,"nodbreadaccessfields")
         cNoDbWriteFields  = getAttribute(ihBrowse,"nodbwriteaccessfields")
         cNoDbWriteBuffers = getAttribute(ihBrowse,"nodbwriteaccessbuffers")
         .
         
  IF icColumns = "*" THEN icColumns = cAllViewFields.
  ELSE ihBrowse:FIT-LAST-COLUMN = NO.

  EMPTY TEMP-TABLE ttSort1.
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    cColumnName = ihBrowse:GET-BROWSE-COLUMN(ix):NAME.

    FIND FIRST ttObjectLink
         WHERE ttObjectLink.hFromObject = ihBrowse
           AND ttObjectLink.cLinkType   = "browseoverlay"
           AND ttObjectLink.cLinkInfo   = cColumnName
         NO-ERROR.
    IF NOT CAN-DO(icColumns,cColumnName) 
       OR CAN-DO(cNoDbReadAccess,cColumnName) 
       THEN DO:
      ihBrowse:GET-BROWSE-COLUMN(ix):VISIBLE = NO.
      IF AVAIL ttObjectLink THEN 
        setAttribute(ttObjectLink.hToObject,"visible","no").
    END.
    ELSE IF CAN-DO(cAllViewFields,cColumnName) THEN DO:
      IF LOOKUP(cColumnName,cAllViewFields) LE NUM-ENTRIES(cWidthPixelsList) THEN
        ihBrowse:GET-BROWSE-COLUMN(ix):WIDTH-PIXELS = INT(ENTRY(LOOKUP(cColumnName,cAllViewFields),cWidthPixelsList)).
      ihBrowse:GET-BROWSE-COLUMN(ix):VISIBLE = YES.
      IF AVAIL ttObjectLink THEN
        setAttribute(ttObjectLink.hToObject,"visible","yes").
      CREATE ttSort1.
      ASSIGN ttSort1.iSeq   = LOOKUP(cColumnName,icColumns)
             ttSort1.iSeq2  = ix.
    END.

    IF AVAIL ttObjectLink AND 
      (ibHideOverlays 
       OR CAN-DO(cNoEditFields,cColumnName) 
       OR CAN-DO(cNoDbWriteFields,cColumnName)
       OR CAN-DO(cNoDbWriteBuffers,getAttribute(ihBrowse,"fieldbuffer" + cColumnName))
       ) THEN
      setAttribute(ttObjectLink.hToObject,"visible","no").
  END.
  FOR EACH ttSort1 BY ttSort1.iSeq:
    IF ttSort1.iSeq2 NE ttSort1.iSeq THEN DO:
      ihBrowse:MOVE-COLUMN(ttSort1.iSeq2,ttSort1.iSeq) NO-ERROR.
      FOR EACH bttSort1 
          WHERE bttSort1.iSeq2 GE ttSort1.iSeq
            AND bttSort1.iSeq2 LT ttSort1.iSeq2:
        bttSort1.iSeq2 = bttSort1.iSeq2 + 1.
      END.
    END.
  END.  
    
  hColumn = ihBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
 
  RETURN YES.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBrowseProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBrowseProperties Procedure 
FUNCTION setBrowseProperties RETURNS LOGICAL
  ( INPUT icProperties AS CHAR,
    INPUT ihBrowse     AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Originally in NewBrowse. Moved here to reduce size of NewBrowse
    Notes: Not all of these properties have not been tested for post-create of browse.
           If any property fails here they have to be set at create-time (moved to NewBrowse)
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(icProperties):
  CASE ENTRY(1,ENTRY(ix,icProperties),"|"):
    WHEN "!FIT-LAST-COLUMN"             THEN ihBrowse:FIT-LAST-COLUMN = FALSE.   
    WHEN "!REFRESHABLE"                 THEN ihBrowse:REFRESHABLE = FALSE.       
    WHEN "!DROP-TARGET"                 THEN ihBrowse:DROP-TARGET = FALSE.       
    WHEN "!LABELS"                      THEN ihBrowse:LABELS = FALSE.     
    WHEN "!MOVABLE"                     THEN ihBrowse:MOVABLE = FALSE.
    WHEN "!RESIZABLE"                   THEN ihBrowse:RESIZABLE = FALSE.
    WHEN "!SEPARATORS"                  THEN ihBrowse:SEPARATORS = FALSE.        
    WHEN "!SENSITIVE"                   THEN ihBrowse:SENSITIVE = FALSE. 
    WHEN "!SELECTABLE"                  THEN ihBrowse:SELECTABLE = FALSE.        
    WHEN "!SCROLLBAR-VERTICAL"          THEN ihBrowse:SCROLLBAR-VERTICAL = FALSE.        
    WHEN "!ROW-RESIZABLE"               THEN ihBrowse:ROW-RESIZABLE = FALSE.     
    WHEN "!NO-EMPTY-SPACE"              THEN ihBrowse:NO-EMPTY-SPACE = FALSE.    
    WHEN "!DYNAMIC"                     THEN ihBrowse:DYNAMIC = FALSE.   
    WHEN "!HIDDEN"                      THEN ihBrowse:HIDDEN = FALSE.    
    WHEN "!EXPANDABLE"                  THEN ihBrowse:EXPANDABLE = FALSE.        
    WHEN "!VIEW-FIRST-COLUMN-ON-REOPEN" THEN ihBrowse:VIEW-FIRST-COLUMN-ON-REOPEN = FALSE.       
    WHEN "!TAB-STOP"                    THEN ihBrowse:TAB-STOP = FALSE.  
    WHEN "!READ-ONLY"                   THEN ihBrowse:READ-ONLY = FALSE.
    WHEN "!VISIBLE"                     THEN ihBrowse:VISIBLE = FALSE.
    WHEN "ALLOW-COLUMN-SEARCHING"       THEN ihBrowse:ALLOW-COLUMN-SEARCHING = TRUE.     
    WHEN "BGCOLOR"                      THEN ihBrowse:BGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "COLUMN-MOVABLE"               THEN ihBrowse:COLUMN-MOVABLE = TRUE.     
    WHEN "COLUMN-RESIZABLE"             THEN ihBrowse:COLUMN-RESIZABLE = TRUE.   
    WHEN "COLUMN-SCROLLING"             THEN ihBrowse:COLUMN-SCROLLING = TRUE.       
    WHEN "CONTEXT-HELP-ID"              THEN ihBrowse:CONTEXT-HELP-ID = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).        
    WHEN "COLUMN"                       THEN ihBrowse:COLUMN = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "CURRENT-COLUMN"               THEN ihBrowse:CURRENT-COLUMN = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).       
    WHEN "DROP-TARGET"                  THEN ihBrowse:DROP-TARGET = TRUE.        
    WHEN "DYNAMIC"                      THEN ihBrowse:DYNAMIC = TRUE.    
    WHEN "DOWN"                         THEN ihBrowse:DOWN = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).   
    WHEN "EXPANDABLE"                   THEN ihBrowse:EXPANDABLE = TRUE. 
    WHEN "FIT-LAST-COLUMN"              THEN ihBrowse:FIT-LAST-COLUMN = TRUE.    
    WHEN "FGCOLOR"                      THEN ihBrowse:FGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "FONT"                         THEN ihBrowse:FONT = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "FRAME"                        THEN ihBrowse:FRAME = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).        
    WHEN "HELP"                         THEN ihBrowse:HELP = ENTRY(2,ENTRY(ix,icProperties),"|").
    WHEN "HEIGHT-PIXELS"                THEN ihBrowse:HEIGHT-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "HEIGHT-CHARS"                 THEN ihBrowse:HEIGHT-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "HIDDEN"                       THEN ihBrowse:HIDDEN = TRUE.     
    WHEN "LABELS"                       THEN ihBrowse:LABELS = TRUE.     
    WHEN "MOVABLE"                      THEN ihBrowse:MOVABLE = TRUE.
    WHEN "MIN-COLUMN-WIDTH-PIXELS"      THEN ihBrowse:MIN-COLUMN-WIDTH-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).        
    WHEN "MENU-MOUSE"                   THEN ihBrowse:MENU-MOUSE = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).     
    WHEN "MIN-COLUMN-WIDTH-CHARS"       THEN ihBrowse:MIN-COLUMN-WIDTH-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")). 
    WHEN "MENU-KEY"                     THEN ihBrowse:MENU-KEY = ENTRY(2,ENTRY(ix,icProperties),"|").    
    WHEN "MAX-DATA-GUESS"               THEN ihBrowse:MAX-DATA-GUESS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")). 
    WHEN "NUM-LOCKED-COLUMNS"           THEN ihBrowse:NUM-LOCKED-COLUMNS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).     
    WHEN "NAME"                         THEN ihBrowse:NAME = ENTRY(2,ENTRY(ix,icProperties),"|").
    WHEN "NO-EMPTY-SPACE"               THEN ihBrowse:NO-EMPTY-SPACE = TRUE.     
    WHEN "PRIVATE-DATA"                 THEN ihBrowse:PRIVATE-DATA = ENTRY(2,ENTRY(ix,icProperties),"|").
    WHEN "PARENT"                       THEN ihBrowse:PARENT = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).       
    WHEN "POPUP-MENU"                   THEN ihBrowse:POPUP-MENU = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).   
    WHEN "ROW-HEIGHT-CHARS"             THEN ihBrowse:ROW-HEIGHT-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).   
    WHEN "RESIZABLE"                    THEN ihBrowse:RESIZABLE = TRUE.
    WHEN "ROW-RESIZABLE"                THEN ihBrowse:ROW-RESIZABLE = TRUE.      
    WHEN "ROW-HEIGHT-PIXELS"            THEN ihBrowse:ROW-HEIGHT-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).      
    WHEN "REFRESHABLE"                  THEN ihBrowse:REFRESHABLE = TRUE.        
    WHEN "READ-ONLY"                    THEN ihBrowse:READ-ONLY = TRUE.
    WHEN "SELECTABLE"                   THEN ihBrowse:SELECTABLE = TRUE. 
    WHEN "SCROLLBAR-VERTICAL"           THEN ihBrowse:SCROLLBAR-VERTICAL = TRUE. 
    WHEN "SENSITIVE"                    THEN ihBrowse:SENSITIVE = TRUE.  
    WHEN "SEPARATORS"                   THEN ihBrowse:SEPARATORS = TRUE. 
    WHEN "SEPARATOR-FGCOLOR"            THEN ihBrowse:SEPARATOR-FGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).      
/*     WHEN "TITLE-BGCOLOR"                THEN ihBrowse:TITLE-BGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).  */
/*     WHEN "TITLE-FGCOLOR"                THEN ihBrowse:TITLE-FGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).  */
/*     WHEN "TITLE-FONT"                   THEN ihBrowse:TITLE-FONT = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).     */
    WHEN "TOOLTIP"                      THEN ihBrowse:TOOLTIP = ENTRY(2,ENTRY(ix,icProperties),"|").     
    WHEN "TAB-STOP"                     THEN ihBrowse:TAB-STOP = TRUE.       
    WHEN "TAB-POSITION"                 THEN ihBrowse:TAB-POSITION = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).   
/*     WHEN "TITLE"                        THEN ihBrowse:TITLE = ENTRY(2,ENTRY(ix,icProperties),"|").  */
    WHEN "VIEW-FIRST-COLUMN-ON-REOPEN"  THEN ihBrowse:VIEW-FIRST-COLUMN-ON-REOPEN= TRUE. 
    WHEN "VISIBLE"                      THEN ihBrowse:VISIBLE = TRUE.
    WHEN "WIDTH-PIXELS"                 THEN ihBrowse:WIDTH-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "WIDTH-CHARS"                  THEN ihBrowse:WIDTH-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "X"                            THEN ihBrowse:X = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
  END CASE.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDebugObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDebugObject Procedure 
FUNCTION setDebugObject RETURNS LOGICAL
  ( INPUT ihDebugObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hDebugObject = ihDebugObject.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDefaultButton) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDefaultButton Procedure 
FUNCTION setDefaultButton RETURNS LOGICAL
  ( INPUT icAction  AS CHAR,
    INPUT icImage   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cAction AS CHAR NO-UNDO.

IF icAction MATCHES "Passive*Button" THEN DO:
  cAction = SUBSTR(icAction,8,R-INDEX(icAction,"Button") - 1).
  IF cAction = "Config" THEN cAction = "Browse" + cAction.
END.
ELSE cAction = SUBSTR(icAction,8).

IF SEARCH(icImage) = ? THEN RETURN NO.

IF CAN-DO(cDefActionList,cAction) THEN
  ENTRY(LOOKUP(cAction,cDefActionList),cDefImageList) = icImage NO-ERROR.
ELSE 
  ASSIGN cDefActionList = cDefActionList + "," + cAction
         cDefImageList  = cDefImageList  + "," + icImage.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEditBgColor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEditBgColor Procedure 
FUNCTION setEditBgColor RETURNS LOGICAL
  ( INPUT iiEditBgColor AS INT  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
iEditBgColor = iiEditBgColor.

RETURN yes.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEventMethod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEventMethod Procedure 
FUNCTION setEventMethod RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE,
    INPUT ihWidget AS HANDLE,
    INPUT icMethod AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Parent might be anything (toolbar, buffer, popup..) 
           To distinguish corresoponding menu-items and buttons, use type (button / menu-item) - assumes button
------------------------------------------------------------------------------*/
FIND FIRST ttEvent 
     WHERE ttEvent.hObject     = ihParent
       AND ttEvent.hWidget     = ihWidget
     NO-ERROR.
IF AVAIL ttEvent THEN
  ttEvent.cMethod = icMethod.
ELSE
  RETURN NO.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldMapState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldMapState Procedure 
FUNCTION setFieldMapState RETURNS LOGICAL
  ( INPUT ihFieldMap  AS HANDLE,
    INPUT icState     AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set the field state for a fieldmap according to the current user action 
    Notes: Main usage is from setToolbar 
           Uses the same convention for "state" as the toolbar, i.e new,save,avail,etc   
           Also applies any database security restrictions        
  Changes: If browse overlays are the fieldMap this function doesn't apply
------------------------------------------------------------------------------*/
DEF VAR hWidget                 AS HANDLE NO-UNDO.
DEF VAR cUpdateWidgets          AS CHAR   NO-UNDO.
DEF VAR cDisabledFields         AS CHAR   NO-UNDO.
DEF VAR cReadOnlyFields         AS CHAR   NO-UNDO.
DEF VAR cDisabledTBevents       AS CHAR   NO-UNDO. 
DEF VAR cBufferFields           AS CHAR   NO-UNDO.
DEF VAR cScreenWidgets          AS CHAR   NO-UNDO. 
DEF VAR cNoDbReadAccessFields   AS CHAR   NO-UNDO.
DEF VAR cNoDbWriteAccessFields  AS CHAR   NO-UNDO.
DEF VAR cNoDbWriteAccessBuffers AS CHAR   NO-UNDO.
DEF VAR bCheckParent            AS LOG    NO-UNDO. 
DEF VAR bEditBtn                AS LOG    NO-UNDO.
DEF VAR cPrimaryKeyFields       AS CHAR   NO-UNDO.
DEF VAR cEditBgCol              AS CHAR   NO-UNDO.
DEF VAR iEditBgCol              AS INT    NO-UNDO.
DEF VAR bQueryOnlyFieldMap      AS LOG    NO-UNDO.

IF getAttribute(ihFieldMap,"fieldMapIsBrowse") = "yes" THEN RETURN YES.

ASSIGN cUpdateWidgets     = TRIM(getAttribute(ihFieldMap,"ScreenUpdateWidgets") + "," +
                                 getAttribute(ihFieldMap,"ExtraUpdateWidgets"),",")
       cDisabledFields    = getAttribute(ihFieldMap,"DisabledFields")
       cReadOnlyFields    = getAttribute(ihFieldMap,"ReadOnlyFields")
       cScreenWidgets     = TRIM(getAttribute(ihFieldMap,"ScreenUpdateWidgets") + "," + getAttribute(ihFieldMap,"ScreenDisplayWidgets"),",")
       cBufferFields      = TRIM(getAttribute(ihFieldMap,"BufferUpdateFields")  + "," + getAttribute(ihFieldMap,"BufferDisplayFields"),",")
       cPrimaryKeyFields  = getAttribute(ihFieldMap,"primaryKeyFields")
       cEditBgCol         = getAttribute(ihFieldMap,"editBgColor")
       bQueryOnlyFieldMap = getAttribute(ihFieldMap,"queryOnly") = "yes"
       .
IF cEditBgCol = "?" THEN
  iEditBgCol = ?.
ELSE IF cEditBgCol NE "" THEN
  iEditBgCol = INT(cEditBgCol).
ELSE iEditBgCol = iEditBgColor.         

FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ihFieldMap
       AND ttObjectLink.cLinkType   = "toolbar"
     NO-ERROR.
IF AVAIL ttObjectLink THEN
  ASSIGN cDisabledTBevents = TRIM(getAttribute(ttObjectLink.hToObject,"disabledevents") 
                             + "," + getAttribute(ttObjectLink.hToObject,"disabledActions") 
                             + "," + getAttribute(ttObjectLink.hToObject,"configdisabledevents") + ","
                             + (IF NOT getIsEventAllowed(ttObjectLink.hToObject,"save") THEN "save" ELSE "")
                              ,",")
         bEditBtn          = CAN-DO(getAttribute(ttObjectLink.hToObject,"actionlist"),"edit").

FIND FIRST ttObjectLink 
     WHERE ttObjectLink.hFromObject = ihFieldMap
       AND ttObjectLink.cLinkType   = "query"
     NO-ERROR.
IF NOT AVAIL ttObjectLink THEN
  FIND FIRST ttObjectLink 
       WHERE ttObjectLink.hFromObject = ihFieldMap
         AND ttObjectLink.cLinkType   = "browse"
       NO-ERROR.
ELSE bCheckParent = YES.
IF AVAIL ttObjectLink THEN DO:
  ASSIGN cNoDbReadAccessFields   = getAttribute(ttObjectLink.hToObject,"nodbreadaccessfields")
         cNoDbWriteAccessFields  = getAttribute(ttObjectLink.hToObject,"nodbwriteaccessfields")
         cNoDbWriteAccessBuffers = getAttribute(ttObjectLink.hToObject,"nodbwriteaccessbuffers").
  IF getEventWidget(ttObjectLink.hToObject,"new","menu-item") NE ? THEN
    cDisabledTBevents = TRIM(cDisabledTBevents + (IF NOT getIsEventAllowed(ttObjectLink.hToObject,"new") THEN ",save" ELSE ""),",").
END.
IF bCheckParent THEN DO:
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
         AND bttObjectLink.cLinkType   = "parent"
       NO-ERROR.
  IF NOT AVAIL bttObjectLink THEN
    FIND FIRST bttObjectLink
         WHERE bttObjectLink.hFromObject = ttObjectLink.hToObject
           AND bttObjectLink.cLinkType   = "onetoone"
         NO-ERROR.
  IF AVAIL bttObjectLink THEN DO:
    IF getEventWidget(bttObjectLink.hToObject,"new","menu-item") NE ? THEN
      cDisabledTBevents = TRIM(cDisabledTBevents + (IF NOT getIsEventAllowed(bttObjectLink.hToObject,"new") THEN ",save" ELSE ""),",").
  END.
END.

DO ix = 1 TO NUM-ENTRIES(cUpdateWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cUpdateWidgets)) NO-ERROR.
  IF VALID-HANDLE(hWidget) THEN DO:
    IF iEditBgCol NE 0 AND icState NE "modified" THEN
      hWidget:BGCOLOR = ?.

    IF (CAN-DO(cGlobSecDisabledActions,"save") OR CAN-DO(cDisabledTBevents,"save") OR
        (icState = "avail" AND getAttribute(ihFieldMap,"enableOnEdit") NE "no" AND bEditBtn))
       AND NOT bQueryOnlyFieldMap 
       THEN DO:
      IF CAN-DO("combo-box,toggle-box,slider,radio-set,button",hWidget:TYPE) THEN
        hWidget:SENSITIVE = FALSE.
      ELSE DO:
        hWidget:READ-ONLY = TRUE NO-ERROR.

        FIND FIRST bttObjectLink 
             WHERE bttObjectLink.hFromObject = ihFieldMap
               AND bttObjectLink.cLinkType   = "dotNetDisplay"
               AND bttObjectLink.cLinkInfo   = hWidget:NAME
             NO-ERROR.
        IF AVAIL bttObjectLink THEN
          /* Target: JBoxWrapWindowInForm */
        /*  PUBLISH "dotNetEnable" (ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO). */
          PUBLISH "dotNetMethod" ("dotNetEnable",ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO).
      END.
      NEXT.
    END.

    IF CAN-DO("avail,modified,new,edit",icState) THEN DO:
      hWidget:SENSITIVE = TRUE.
      IF NOT CAN-DO("combo-box,toggle-box,slider,radio-set",hWidget:TYPE) THEN DO:
        hWidget:READ-ONLY = FALSE NO-ERROR.
        IF iEditBgCol NE 0 THEN 
          IF icState = "avail" OR (CAN-DO("edit,modified",icState) AND CAN-DO(cPrimaryKeyFields,hWidget:NAME)) THEN hWidget:BGCOLOR = ?. 
          ELSE hWidget:BGCOLOR = iEditBgCol.
        FIND FIRST bttObjectLink 
             WHERE bttObjectLink.hFromObject = ihFieldMap
               AND bttObjectLink.cLinkType   = "dotNetDisplay"
               AND bttObjectLink.cLinkInfo   = hWidget:NAME
             NO-ERROR.
        IF AVAIL bttObjectLink THEN
          /* Target: JBoxWrapWindowInForm */
        /*  PUBLISH "dotNetEnable" (ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,YES). */
          PUBLISH "dotNetMethod" ("dotNetEnable",ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,YES).
      END.
    END.

    IF CAN-DO(cDisabledFields,hWidget:NAME) THEN
      hWidget:SENSITIVE = FALSE.
    ELSE IF CAN-DO(cReadOnlyFields,hWidget:NAME) OR (CAN-DO(cPrimaryKeyFields,hWidget:NAME) AND icState NE "new") THEN DO:
      IF CAN-DO("combo-box,toggle-box,slider,radio-set,button",hWidget:TYPE) THEN
        hWidget:SENSITIVE = FALSE.
      ELSE 
        hWidget:READ-ONLY = TRUE NO-ERROR.
      FIND FIRST bttObjectLink 
           WHERE bttObjectLink.hFromObject = ihFieldMap
             AND bttObjectLink.cLinkType   = "dotNetDisplay"
             AND bttObjectLink.cLinkInfo   = hWidget:NAME
           NO-ERROR.
      IF AVAIL bttObjectLink THEN
        /* Target: JBoxWrapWindowInForm */
      /*  PUBLISH "dotNetEnable" (ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO). */
        PUBLISH "dotNetMethod" ("dotNetEnable",ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO).
    END.
    ELSE IF CAN-DO("disable,not avail",icState) THEN DO:
      IF CAN-DO("combo-box,toggle-box,slider,radio-set,button",hWidget:TYPE) THEN
        hWidget:SENSITIVE = FALSE.
      ELSE 
        hWidget:READ-ONLY = TRUE NO-ERROR.
      
/*      hWidget:SENSITIVE = FALSE.*/
    END.  
  END.
END.

IF cNoDbReadAccessFields NE "" OR cNoDbWriteAccessFields NE "" OR cNoDbWriteAccessBuffers NE "" THEN
  DO ix = 1 TO NUM-ENTRIES(cScreenWidgets):
    hWidget = WIDGET-HANDLE(ENTRY(ix,cScreenWidgets)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN DO:
      IF CAN-DO(cNoDbReadAccessFields,ENTRY(ix,cBufferFields)) THEN DO:
        hWidget:HIDDEN = YES.
        FIND FIRST bttObjectLink 
             WHERE bttObjectLink.hFromObject = ihFieldMap
               AND bttObjectLink.cLinkType   = "dotNetDisplay"
               AND bttObjectLink.cLinkInfo   = hWidget:NAME
             NO-ERROR.
        IF AVAIL bttObjectLink THEN
          /* Target: JBoxWrapWindowInForm */
         /* PUBLISH "dotNetVisible" (ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO). */
          PUBLISH "dotNetMethod" ("dotNetVisible",ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO).
      END.

      ELSE IF CAN-DO(cNoDbWriteAccessFields,ENTRY(ix,cBufferFields)) 
           OR CAN-DO(cNoDbWriteAccessBuffers,getAttribute(ttObjectLink.hToObject,"fieldbuffer" + ENTRY(ix,cBufferFields))) 
        THEN DO:
        IF CAN-DO("combo-box,toggle-box,slider,radio-set",hWidget:TYPE) THEN
          hWidget:SENSITIVE = FALSE.
        ELSE DO:
          hWidget:READ-ONLY = TRUE NO-ERROR.
          FIND FIRST bttObjectLink 
               WHERE bttObjectLink.hFromObject = ihFieldMap
                 AND bttObjectLink.cLinkType   = "dotNetDisplay"
                 AND bttObjectLink.cLinkInfo   = hWidget:NAME
               NO-ERROR.
          IF AVAIL bttObjectLink THEN
            /* Target: JBoxWrapWindowInForm */
          /*  PUBLISH "dotNetEnable" (ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO). */            
            PUBLISH "dotNetMethod" ("dotNetEnable",ihFieldMap,bttObjectLink.hToObject,bttObjectLink.cLinkInfo,NO).
        END.
      END.
    END.
  END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setListAttribute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setListAttribute Procedure 
FUNCTION setListAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR,
    INPUT icDelim   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Make sure that an attribute that contains a list holds 
           only unique (non-empty) entries
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffer   AS HANDLE NO-UNDO.
DEF VAR cLogField AS CHAR   NO-UNDO.
DEF VAR ix        AS INT    NO-UNDO.
DEF VAR cNewValue AS CHAR   NO-UNDO.
DEF VAR cDelim    AS CHAR   NO-UNDO INIT ",".

IF icDelim NE "" THEN cDelim = icDelim.

cLogField = (IF CAN-QUERY(ihObject,"name") THEN ihObject:NAME + " - " ELSE "") + icName + " - " + icValue.

{incl/methodlog.i cLogField}

DO ix = 1 TO NUM-ENTRIES(icValue,cDelim):
  IF ENTRY(ix,icValue,cDelim) NE "" AND LOOKUP(ENTRY(ix,icValue,cDelim),cNewValue,cDelim) = 0 THEN
    cNewValue = cNewValue + (IF cNewValue NE "" THEN cDelim ELSE "") + ENTRY(ix,icValue,cDelim).
END.

FIND FIRST ttAttribute 
     WHERE ttAttribute.hObject = ihObject
       AND ttAttribute.cName   = icName
     NO-ERROR.
IF NOT AVAIL ttAttribute THEN DO:
  CREATE ttAttribute.
  ASSIGN ttAttribute.hObject = ihObject
         ttAttribute.cName   = icName
         ttAttribute.cValue  = cNewValue.
END.
ELSE DO ix = 1 TO NUM-ENTRIES(cNewValue,cDelim):
  IF LOOKUP(ENTRY(ix,cNewValue,cDelim),ttAttribute.cValue,cDelim) = 0 THEN
    ttAttribute.cValue = ttAttribute.cValue + (IF ttAttribute.cValue NE "" THEN cDelim ELSE "") + ENTRY(ix,cNewValue,cDelim).
END.

RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setObjectName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setObjectName Procedure 
FUNCTION setObjectName RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icNewName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: 
    Notes: 
------------------------------------------------------------------------------*/
FIND FIRST bttObject
     WHERE bttObject.hObject     = ihObject
     NO-ERROR.
IF AVAIL bttObject THEN DO:
  bttObject.cObjectName = icNewName.
  RETURN YES.
END.
ELSE RETURN NO.
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setObjectSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setObjectSourceProc Procedure 
FUNCTION setObjectSourceProc RETURNS LOGICAL
  ( INPUT ihObjectSourceProc AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hObjectSourceProc = ihObjectSourceProc.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setObjectState) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setObjectState Procedure 
FUNCTION setObjectState RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icState   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Usage is f.ex when you want to save a fieldMap directly (not via a user action).
           In this case the fieldMap must have a state corresponding to the action (new/change).
    Notes: 
------------------------------------------------------------------------------*/
FIND FIRST bttObject
     WHERE bttObject.hObject     = ihObject
     NO-ERROR.
IF AVAIL bttObject THEN DO:
  bttObject.cState = icState.
  RETURN YES.
END.
ELSE RETURN NO.
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSearchFieldLinkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSearchFieldLinkInfo Procedure 
FUNCTION setSearchFieldLinkInfo RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icColumn AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST bbttObjectLink
     WHERE bbttObjectLink.hFromObject = ihBrowse
       AND bbttObjectLink.cLinkType = "browse-search-field"
     NO-ERROR.
IF AVAIL bbttObjectLink THEN
  bbttObjectLink.cLinkInfo = icColumn.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSecDisabledActions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSecDisabledActions Procedure 
FUNCTION setSecDisabledActions RETURNS LOGICAL
  ( INPUT icSecDisabledActions AS CHAR,
    INPUT icProgramFile        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Disable a set of actions. Useful to assign read-only,f.ex to a user
    Notes: If no program file is given the action-list is global  
------------------------------------------------------------------------------*/
IF icProgramFile = "" THEN
  cGlobSecDisabledActions = icSecDisabledActions.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSortLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSortLabel Procedure 
FUNCTION setSortLabel RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icSortColumn AS CHAR,
    INPUT ibDesc       AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Set font-label and indicator for direction 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn        AS HANDLE NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.
DEF VAR cSortMap       AS CHAR NO-UNDO.
DEF VAR cColumnName    AS CHAR NO-UNDO.

ihBrowse:CLEAR-SORT-ARROWS().
IF NUM-ENTRIES(icSortColumn,"") > 1 THEN DO:
  cSortMap = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"sortmap").
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    hColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
  
    hColumn:LABEL-FONT = IF ihBrowse:PARENT:PARENT:FONT NE ? THEN 
                           ihBrowse:PARENT:PARENT:FONT
                         ELSE ?.
    hColumn:LABEL-BGCOLOR = IF ihBrowse:PARENT:PARENT:BGCOLOR NE ? THEN 
                              ihBrowse:PARENT:PARENT:BGCOLOR
                            ELSE ?.

    cColumnName = hColumn:NAME.
    IF SUBSTR(cColumnName,LENGTH(cColumnName)) = "]" THEN
      cColumnName = "jbextent_" + RIGHT-TRIM(SUBSTR(cColumnName,R-INDEX(cColumnName,"[") + 1),"]") + "_" + SUBSTR(cColumnName,1,R-INDEX(cColumnName,"[") - 1).  
    
    IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumn") THEN 
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumnDesc") = "desc" THEN NO ELSE YES,1).
    ELSE IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumn") THEN
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumnDesc") = "desc" THEN NO ELSE YES,2).
    ELSE IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumn") THEN
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumnDesc") = "desc" THEN NO ELSE YES,3).
    ELSE IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumn") THEN
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumnDesc") = "desc" THEN NO ELSE YES,4).
    ELSE IF cSortMap NE "" THEN
      DO iy = 1 TO NUM-ENTRIES(cSortMap):
        IF hColumn:NAME = ENTRY(1,ENTRY(iy,cSortMap),";") THEN DO:
          IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumnDesc") = "desc" THEN NO ELSE YES,1).
          ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumnDesc") = "desc" THEN NO ELSE YES,2).
          ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumnDesc") = "desc" THEN NO ELSE YES,3).
          ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumnDesc") = "desc" THEN NO ELSE YES,4).
         LEAVE.
       END.
     END.
  END.
END.

ELSE DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  hColumn = ihBrowse:GET-BROWSE-COLUMN(ix).

/*   hColumn:LABEL-FONT = IF ihBrowse:PARENT:PARENT:FONT NE ? THEN       */
/*                          ihBrowse:PARENT:PARENT:FONT                  */
/*                        ELSE ?.                                        */
/*   hColumn:LABEL-BGCOLOR = IF ihBrowse:PARENT:PARENT:BGCOLOR NE ? THEN */
/*                             ihBrowse:PARENT:PARENT:BGCOLOR            */
/*                           ELSE ?.                                     */

  IF hColumn:NAME = icSortColumn THEN DO:
    ihBrowse:SET-SORT-ARROW(ix,NOT ibDesc).
    DYNAMIC-FUNCTION("setSearchFieldLinkInfo",ihBrowse,icSortColumn).
  END.
END.


/*
IF PROVERSION BEGINS "1" AND PROVERSION > "10.1a" THEN
  RUN setSortLabel.p (ihBrowse,icSortColumn,ibDesc).
ELSE DO:
  IF NUM-ENTRIES(icSortColumn,"") > 1 THEN DO:
    cSortMap = getAttribute(ihBrowse,"sortmap").
    DO ix = 1 TO ihBrowse:NUM-COLUMNS:
      hColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
    
      hColumn:LABEL-FONT = IF ihBrowse:FONT NE ? THEN
                             ihBrowse:FONT
                           ELSE IF ihBrowse:PARENT:PARENT:FONT NE ? THEN 
                             ihBrowse:PARENT:PARENT:FONT
                           ELSE ?.
      hColumn:LABEL-BGCOLOR = IF ihBrowse:PARENT:PARENT:BGCOLOR NE ? THEN 
                                ihBrowse:PARENT:PARENT:BGCOLOR
                              ELSE ?.
    
      hColumn:LABEL = DYNAMIC-FUNCTION("getStrippedSortLabel",hColumn).

      cColumnName = hColumn:NAME.
      IF SUBSTR(cColumnName,LENGTH(cColumnName)) = "]" THEN
        cColumnName = "jbextent_" + RIGHT-TRIM(SUBSTR(cColumnName,R-INDEX(cColumnName,"[") + 1),"]") + "_" + SUBSTR(cColumnName,1,R-INDEX(cColumnName,"[") - 1).  
    
      IF cColumnName = getAttribute(ihBrowse,"1stSortColumn") THEN
        ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
               hColumn:LABEL-BGCOLOR = iDefaultSortColor
               hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"1stSortColumnDesc") = "desc" THEN cMarkDesc + "1" ELSE cMarkAsc + "1").
      ELSE IF cColumnName = getAttribute(ihBrowse,"2ndSortColumn") THEN
        ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
               hColumn:LABEL-BGCOLOR = iDefaultSortColor
               hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"2ndSortColumnDesc") = "desc" THEN cMarkDesc + "2" ELSE cMarkAsc + "2").
      ELSE IF cColumnName = getAttribute(ihBrowse,"3rdSortColumn") THEN
        ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
               hColumn:LABEL-BGCOLOR = iDefaultSortColor
               hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"3rdSortColumnDesc") = "desc" THEN cMarkDesc + "3" ELSE cMarkAsc + "3").
      ELSE IF cColumnName = getAttribute(ihBrowse,"4thSortColumn") THEN
        ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
               hColumn:LABEL-BGCOLOR = iDefaultSortColor
               hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"4thSortColumnDesc") = "desc" THEN cMarkDesc + "4" ELSE cMarkAsc + "4").
     ELSE IF cSortMap NE "" THEN
       DO iy = 1 TO NUM-ENTRIES(cSortMap):
         IF hColumn:NAME = ENTRY(1,ENTRY(iy,cSortMap),";") THEN DO:
           IF ENTRY(2,ENTRY(iy,cSortMap),";") = getAttribute(ihBrowse,"1stSortColumn") THEN
             ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
                    hColumn:LABEL-BGCOLOR = iDefaultSortColor
                    hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"1stSortColumnDesc") = "desc" THEN cMarkDesc + "1" ELSE cMarkAsc + "1").
           ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = getAttribute(ihBrowse,"2ndSortColumn") THEN
             ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
                    hColumn:LABEL-BGCOLOR = iDefaultSortColor
                    hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"2ndSortColumnDesc") = "desc" THEN cMarkDesc + "2" ELSE cMarkAsc + "2").
           ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = getAttribute(ihBrowse,"3rdSortColumn") THEN
             ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
                    hColumn:LABEL-BGCOLOR = iDefaultSortColor
                    hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"3rdSortColumnDesc") = "desc" THEN cMarkDesc + "3" ELSE cMarkAsc + "3").
           ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = getAttribute(ihBrowse,"4thSortColumn") THEN
             ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
                    hColumn:LABEL-BGCOLOR = iDefaultSortColor
                    hColumn:LABEL = hColumn:LABEL + (IF getAttribute(ihBrowse,"4thSortColumnDesc") = "desc" THEN cMarkDesc + "4" ELSE cMarkAsc + "4").
           LEAVE.
         END.
       END.
    END.
  END.
  
  ELSE DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    hColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
  
    hColumn:LABEL-FONT = IF ihBrowse:FONT NE ? THEN
                             ihBrowse:FONT
                         ELSE IF ihBrowse:PARENT:PARENT:FONT NE ? THEN 
                           ihBrowse:PARENT:PARENT:FONT
                         ELSE ?.
    hColumn:LABEL-BGCOLOR = IF ihBrowse:PARENT:PARENT:BGCOLOR NE ? THEN 
                              ihBrowse:PARENT:PARENT:BGCOLOR
                            ELSE ?.
  
    hColumn:LABEL = DYNAMIC-FUNCTION("getStrippedSortLabel",hColumn).
  
    IF hColumn:NAME = icSortColumn THEN DO:
      ASSIGN hColumn:LABEL-FONT    = iDefaultSortFont
             hColumn:LABEL-BGCOLOR = iDefaultSortColor
             hColumn:LABEL = hColumn:LABEL + (IF ibDesc THEN cMarkDesc ELSE cMarkAsc).
      DYNAMIC-FUNCTION("setSearchFieldLinkInfo",ihBrowse,icSortColumn).
    END.
  END.
END.
*/
IF icSortColumn = "" THEN DO:
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = ihBrowse
         AND bttObjectLink.cLinkType   = "browse-search-field"
       NO-ERROR.
  IF AVAIL bttObjectLink THEN
    bttObjectLink.hToObject:HIDDEN = YES.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSortMarkers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSortMarkers Procedure 
FUNCTION setSortMarkers RETURNS LOGICAL
  ( INPUT icMarkAsc  AS CHAR,
    INPUT icMarkDesc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN cMarkAsc  = icMarkAsc
       cMarkDesc = icMarkDesc.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSortString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSortString Procedure 
FUNCTION setSortString RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE,
    INPUT icSortString  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Simplify initialization of the sort expression
Parameter: icSortString: <field1>[;desc],<field2>[;desc]..
    Notes: The sortmap attribute is taken in account so it must be set before the function is called
           If there are duplicate field names in the database query refere to the duplicate
           by adding the buffer number as a suffix to the field name. 
           NB! This does not apply to the first instance of the duplicates (1st buffer where the fieldname occurs)       
------------------------------------------------------------------------------*/

setAttribute(ihQueryObject,"1stSortColumn","").
setAttribute(ihQueryObject,"1stDbSortColumn","").
setAttribute(ihQueryObject,"1stSortColumnDesc","").
setAttribute(ihQueryObject,"2ndSortColumn","").
setAttribute(ihQueryObject,"2ndDbSortColumn","").
setAttribute(ihQueryObject,"2ndSortColumnDesc","").
setAttribute(ihQueryObject,"3rdSortColumn","").
setAttribute(ihQueryObject,"3rdDbSortColumn","").
setAttribute(ihQueryObject,"3rdSortColumnDesc","").
setAttribute(ihQueryObject,"4thSortColumn","").
setAttribute(ihQueryObject,"4thDbSortColumn","").
setAttribute(ihQueryObject,"4thSortColumnDesc","").

IF NUM-ENTRIES(icSortString) = 1 THEN DO:
  setAttribute(ihQueryObject,"querysort",getRealSortField(ihQueryObject,ENTRY(1,icSortString,";"),"db")).
  setAttribute(ihQueryObject,"localsort",getRealSortField(ihQueryObject,ENTRY(1,icSortString,";"),"")).
  setAttribute(ihQueryObject,"sortbuffer",getAttribute(ihQueryObject,"fieldbuffer" + ENTRY(1,icSortString,";"))).

  DYNAMIC-FUNCTION("setSearchFieldLinkInfo",ihQueryObject,getAttribute(ihQueryObject,"localsort")).
  IF NUM-ENTRIES(icSortString,";") > 1 THEN DO:
    IF ENTRY(2,icSortString,";") = "desc" THEN
      setAttribute(ihQueryObject,"querydesc",ENTRY(2,icSortString,";")).
    ELSE
      setAttribute(ihQueryObject,"querydesc","").
  END.
  setAttribute(ihQueryObject,"1stDbSortColumn",getAttribute(ihQueryObject,"querysort")).
  setAttribute(ihQueryObject,"1stSortColumn",getAttribute(ihQueryObject,"localsort")).
  setAttribute(ihQueryObject,"1stSortColumnDesc",getAttribute(ihQueryObject,"querydesc")).
  RETURN getAttribute(ihQueryObject,"querydesc") = "desc".
END.

ELSE DO ix = 1 TO NUM-ENTRIES(icSortString):
  CASE ix:
    WHEN 1 THEN DO:
      setAttribute(ihQueryObject,"1stSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"")).
      setAttribute(ihQueryObject,"1stDbSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"db")).
      IF NUM-ENTRIES(ENTRY(ix,icSortString),";") > 1 THEN DO:
        IF ENTRY(2,ENTRY(ix,icSortString),";") = "desc" THEN
          setAttribute(ihQueryObject,"1stSortColumnDesc",ENTRY(2,ENTRY(ix,icSortString),";")).
        ELSE
          setAttribute(ihQueryObject,"1stSortColumnDesc","").
      END.
    END.
    WHEN 2 THEN DO:
      setAttribute(ihQueryObject,"2ndSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"")).
      setAttribute(ihQueryObject,"2ndDbSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"db")).
      IF NUM-ENTRIES(ENTRY(ix,icSortString),";") > 1 THEN DO:
        IF ENTRY(2,ENTRY(ix,icSortString),";") = "desc" THEN
          setAttribute(ihQueryObject,"2ndSortColumnDesc",ENTRY(2,ENTRY(ix,icSortString),";")).
        ELSE
          setAttribute(ihQueryObject,"2ndSortColumnDesc","").
      END.
    END.
    WHEN 3 THEN DO:
      setAttribute(ihQueryObject,"3rdSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"")).
      setAttribute(ihQueryObject,"3rdDbSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"db")).
      IF NUM-ENTRIES(ENTRY(ix,icSortString),";") > 1 THEN DO:
        IF ENTRY(2,ENTRY(ix,icSortString),";") = "desc" THEN
          setAttribute(ihQueryObject,"3rdSortColumnDesc",ENTRY(2,ENTRY(ix,icSortString),";")).
        ELSE
          setAttribute(ihQueryObject,"3rdSortColumnDesc","").
      END.
    END.
    WHEN 4 THEN DO:
      setAttribute(ihQueryObject,"4thSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"")).
      setAttribute(ihQueryObject,"4thDbSortColumn",getRealSortField(ihQueryObject,ENTRY(1,ENTRY(ix,icSortString),";"),"db")).
      IF NUM-ENTRIES(ENTRY(ix,icSortString),";") > 1 THEN DO:
        IF ENTRY(2,ENTRY(ix,icSortString),";") = "desc" THEN
          setAttribute(ihQueryObject,"4thSortColumnDesc",ENTRY(2,ENTRY(ix,icSortString),";")).
        ELSE
          setAttribute(ihQueryObject,"4thSortColumnDesc","").
      END.
    END.
  END CASE.
END.

RETURN ComposeSortString(ihQueryObject,YES).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSourceProc Procedure
FUNCTION setObjectSourceFileHandle RETURNS LOGICAL 
  ( INPUT ihObject     AS HANDLE,
    INPUT ihSourceProc AS HANDLE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
FIND FIRST ttObject 
     WHERE ttObject.hObject = ihObject
     NO-ERROR.
IF AVAIL ttObject THEN 
  ttObject.hSourceProc = ihSourceProc.
ELSE RETURN NO.

RETURN YES.         

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setToolbar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setToolbar Procedure 
FUNCTION setToolbar RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE,
    INPUT icState   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDisabledEvents       AS CHAR   NO-UNDO.
DEF VAR cEnabledEvents        AS CHAR   NO-UNDO.
DEF VAR cRecordAvailWidgets   AS CHAR   NO-UNDO.
DEF VAR hCorrBrowseObj        AS HANDLE NO-UNDO.
DEF VAR bActiveNote           AS LOG    NO-UNDO.
DEF VAR cNoteImage            AS CHAR   NO-UNDO.
DEF VAR cDbSecDisabledActions AS CHAR   NO-UNDO.
DEF VAR bActiveFilter         AS LOG    NO-UNDO.
DEF VAR bEnableOnEdit         AS LOG    NO-UNDO.
DEF VAR bOneToOne             AS LOG    NO-UNDO.
DEF VAR bActiveUdf            AS LOG    NO-UNDO.
DEF VAR cUdfImage             AS CHAR   NO-UNDO.
DEF VAR bActiveDoc            AS LOG    NO-UNDO.
DEF VAR cDocImage             AS CHAR   NO-UNDO.

DEF BUFFER bbttObjectLink FOR ttObjectLink.

ASSIGN cDisabledEvents        = TRIM(getAttribute(ihToolbar,"disabledevents")
                                   + "," + getAttribute(ihToolbar,"disabledActions")
                                   + "," + getAttribute(ihToolbar,"configdisabledevents"),",")
       cEnabledEvents         = TRIM(getAttribute(ihToolbar,"enabledevents") 
                                   + "," + getAttribute(ihToolbar,"enabledActions")
                                   + "," + getAttribute(ihToolbar,"configenabledevents"),",")
       cDbSecDisabledActions  = getAttribute(ihToolbar,"dbsecdisabledactions")
       bActiveNote            = getAttribute(ihToolbar,"NoteCurrentValue") NE ""
       bActiveUdf             = getAttribute(ihToolbar,"UdfCurrentValue") = "yes"
       bActiveDoc             = getAttribute(ihToolbar,"DocCurrentValue") = "yes"
       .

FIND FIRST bttObjectLink
     WHERE bttObjectLink.hFromObject = ihToolbar
       AND bttObjectLink.cLinkType = "browse"
     NO-LOCK NO-ERROR.
IF AVAIL bttObjectLink THEN DO:
  hCorrBrowseObj = bttObjectLink.hToObject.
  ASSIGN cDisabledEvents = TRIM(cDisabledEvents 
                              + "," + getAttribute(hCorrBrowseObj,"disabledevents")
                              + "," + getAttribute(hCorrBrowseObj,"disabledActions"),",")
         cEnabledEvents  = TRIM(cEnabledEvents 
                              + "," + getAttribute(hCorrBrowseObj,"enabledevents") 
                              + "," + getAttribute(hCorrBrowseObj,"enabledActions") 
                              + "," + getAttribute(hCorrBrowseObj,"configenabledevents"),",")
         bActiveFilter   = getAttribute(hCorrBrowseObj,"queryfilter") NE "" OR getAttribute(hCorrBrowseObj,"prescanqueryfilter") NE "" OR getAttribute(hCorrBrowseObj,"calcfieldfilter") NE ""
         .
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = hCorrBrowseObj
         AND bttObjectLink.cLinkType   = "parent"
       NO-ERROR.
  IF AVAIL bttObjectLink THEN DO:
    FIND bttObject 
         WHERE bttObject.hObject = bttObjectLink.hToObject
         NO-ERROR.
    IF AVAIL bttObject AND 
       (bttObject.cObjectType = "browse" AND NOT bttObject.hObject:QUERY:GET-BUFFER-HANDLE(1):AVAIL OR  
        bttObject.cObjectType = "query" AND NOT bttObject.hObject:GET-BUFFER-HANDLE(1):AVAIL) AND
        NOT CAN-DO(cEnabledEvents,"new")
       THEN
      cDisabledEvents = TRIM(cDisabledEvents + ",new",",").
  END.
END.

FIND FIRST bttObjectLink
     WHERE bttObjectLink.hFromObject = ihToolbar
       AND bttObjectLink.cLinkType = "query"
     NO-LOCK NO-ERROR.
IF AVAIL bttObjectLink THEN DO:
  bActiveFilter   = getAttribute(bttObjectLink.hToObject,"queryfilter") NE "" OR getAttribute(bttObjectLink.hToObject,"prescanqueryfilter") NE "" OR getAttribute(bttObjectLink.hToObject,"calcfieldfilter") NE "".
  FIND FIRST bbttObjectLink NO-LOCK
       WHERE bbttObjectLink.hFromObject = bttObjectLink.hToObject
         AND bbttObjectLink.cLinkType   = "oneToOne"
       NO-ERROR.
  IF AVAIL bbttObjectLink THEN 
    bOneToOne = YES.
END.

FIND FIRST bttObjectLink
     WHERE bttObjectLink.hFromObject = ihToolbar
       AND bttObjectLink.cLinkType = "fieldmap"
     NO-LOCK NO-ERROR.
IF NOT AVAIL bttObjectLink THEN
  FIND FIRST bttObjectLink
       WHERE bttObjectLink.hFromObject = ihToolbar
         AND bttObjectLink.cLinkType = "browse"
       NO-LOCK NO-ERROR.
IF AVAIL bttObjectLink THEN
  ASSIGN cRecordAvailWidgets = getAttribute(bttObjectLink.hToObject,"recordavailwidgets")
         bEnableOnEdit       = getAttribute(bttObjectLink.hToObject,"enableOnEdit") NE "no" AND 
                               CAN-DO(getAttribute(ihToolbar,"actionlist"),"edit").

FOR EACH ttEvent
    WHERE (ttEvent.hObject = ihToolbar
        OR ttEvent.hObject = hCorrBrowseObj)
      AND ttEvent.cAction NE "entry-of-widget"
      :
    
  IF NOT VALID-HANDLE(ttEvent.hWidget) OR 
     (VALID-HANDLE(ttEvent.hWidget) AND ttEvent.hWidget:TYPE NE "button" AND ttEvent.hWidget:TYPE NE "menu-item") THEN
    NEXT.

  IF ttEvent.cAction = "note" AND ttEvent.cWidgetType = "button" THEN DO:
    IF bActiveNote THEN DO:
      IF getAttribute(ihToolbar,"ActiveNoteButton") NE "" THEN
        cNoteImage = getAttribute(ihToolbar,"ActiveNoteButton").
      ELSE  
        cNoteImage = getAttribute(SESSION,"ActiveNoteButton").
    END.  
    ELSE DO:
      IF getAttribute(ihToolbar,"PassiveNoteButton") NE "" THEN
        cNoteImage = getAttribute(ihToolbar,"PassiveNoteButton").
      ELSE  
        cNoteImage = getAttribute(SESSION,"PassiveNoteButton").
    END.  
    &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
      IF cNoteImage NE "" THEN
        ttEvent.hWidget:LOAD-IMAGE(cNoteImage).
    &ENDIF    
  END.

  IF ttEvent.cAction = "udf" AND ttEvent.cWidgetType = "button" THEN DO:
    IF bActiveUdf THEN
      cUdfImage = getAttribute(SESSION,"ActiveUdfButton").
    ELSE
      cUdfImage = getAttribute(SESSION,"PassiveUdfButton").
    &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
      IF cUdfImage NE "" THEN
        ttEvent.hWidget:LOAD-IMAGE(cUdfImage).
    &ENDIF    
  END.
  
  IF ttEvent.cAction = "documents" AND ttEvent.cWidgetType = "button" THEN DO:
    IF bActiveDoc THEN
      cDocImage = getAttribute(SESSION,"ActiveDocumentsButton").
    ELSE
      cDocImage = getAttribute(SESSION,"BtnImg_Documents").
    &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN   
      IF cDocImage NE "" THEN
        ttEvent.hWidget:LOAD-IMAGE(cDocImage).
    &ENDIF    
  END.

  IF CAN-DO(cDisabledEvents,ttEvent.cAction) OR CAN-DO(cDbSecDisabledActions,ttEvent.cAction)
     THEN DO:
    ttEvent.hWidget:SENSITIVE = FALSE.
    NEXT.
  END.

  IF CAN-DO(cGlobSecDisabledActions,ttEvent.cAction) THEN DO:
    ttEvent.hWidget:SENSITIVE = FALSE.
    NEXT.
  END.
  
  IF ttEvent.cAction = "delfilter" THEN DO:
    ttEvent.hWidget:SENSITIVE = bActiveFilter.
    NEXT.
  END.

  IF NOT getIsEventAllowed(ihToolbar,ttEvent.cAction) THEN DO:
    ttEvent.hWidget:SENSITIVE = FALSE.
    NEXT.
  END.

  IF CAN-DO(cEnabledEvents,ttEvent.cAction) THEN DO:
    ttEvent.hWidget:SENSITIVE = TRUE.
    NEXT.
  END.

  IF getAttribute(ihToolbar,"commitstate") = "off"
    AND CAN-DO("commit,rollback",ttEvent.cAction) THEN DO:
    ttEvent.hWidget:SENSITIVE = FALSE.
    NEXT.
  END.

  IF CAN-DO("first,prev,next,last",ttEvent.cAction) AND bOneToOne THEN DO:
    ttEvent.hWidget:SENSITIVE = YES.
    NEXT.
  END.

  IF cRecordAvailWidgets NE "" AND 
     CAN-DO("new,copy,modified",icState) AND 
     CAN-DO(cRecordAvailWidgets,STRING(ttEvent.hWidget)) THEN DO:
    ttEvent.hWidget:SENSITIVE = FALSE.
    NEXT.
  END.

  IF icState = "disable" THEN
    ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE IF (icState = "new" OR (icState = "edit" AND bEnableOnEdit)) AND
     NOT CAN-DO("save,undo",ttEvent.cAction) THEN ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE IF icState = "not avail" AND
     NOT CAN-DO("filter,new,browseconfig,refresh,close,insert",ttEvent.cAction) THEN ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE IF icState = "avail" AND
     CAN-DO("undo,save",ttEvent.cAction) THEN ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE IF icState = "avail" AND bOneToOne AND
     CAN-DO("first,prev,next,last",ttEvent.cAction) THEN ttEvent.hWidget:SENSITIVE = YES.
  ELSE IF icState = "modified" AND
    NOT CAN-DO("save,undo",ttEvent.cAction) THEN ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE IF icState = "last" AND
    CAN-DO("next,last",ttEvent.cAction) THEN ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE IF icState = "last" AND
    NOT CAN-DO("next,last",ttEvent.cAction) THEN NEXT.
  ELSE IF icState = "first" AND
    CAN-DO("first,prev",ttEvent.cAction) THEN ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE IF icState = "first" AND
    NOT CAN-DO("first,prev",ttEvent.cAction) THEN NEXT.
  ELSE IF icState = "no parent" THEN
     ttEvent.hWidget:SENSITIVE = FALSE.
  ELSE
    ttEvent.hWidget:SENSITIVE = TRUE.
END.

FIND FIRST ttObject WHERE ttObject.hObject = ihToolbar NO-ERROR.
IF NOT AVAIL ttObject THEN RETURN FALSE.

ttObject.cState = icState.

IF AVAIL bttObjectLink AND NOT CAN-DO("first,last,prev,next,save,undo",icState) THEN 
  setFieldMapState(bttObjectLink.hToObject,icState).

IF NOT VALID-HANDLE(hCurrSourceProc) THEN hCurrSourceProc = SOURCE-PROCEDURE.
IF CAN-DO(hCurrSourceProc:INTERNAL-ENTRIES,"ExtraSetToolbar") THEN 
  RUN ExtraSetToolbar IN hCurrSourceProc (icState).

FOR EACH bttObjectLink
    WHERE bttObjectLink.hToObject = ihToolBar
      AND bttObjectLink.cLinkType = "parent":

  IF CAN-DO("new,copy,modified,not avail",icState) THEN
    SetToolbar(bttObjectLink.hFromObject,"disable").
END.

PUBLISH "SetWinMenuActionState".

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setToolbarToggles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setToolbarToggles Procedure 
FUNCTION setToolbarToggles RETURNS LOGICAL
  ( INPUT ihToolbar       AS HANDLE,
    INPUT ibChecked       AS LOG,
    INPUT icExceptionList AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH ttEvent
    WHERE ttEvent.hObject = ihToolbar
      AND (IF icExceptionList NE "" THEN NOT CAN-DO(icExceptionList,ttEvent.cAction) ELSE TRUE)
      :
  IF VALID-HANDLE(ttEvent.hWidget) AND ttEvent.hWidget:TYPE = "menu-item" AND ttEvent.hWidget:TOGGLE-BOX THEN
    ttEvent.hWidget:CHECKED = ibChecked.
END.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseBrowseColumnFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUseBrowseColumnFormat Procedure 
FUNCTION setUseBrowseColumnFormat RETURNS LOGICAL
  ( INPUT ibSetUseBrowseColumnFormat AS LOG ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
bSetUseBrowseColumnFormat = ibSetUseBrowseColumnFormat.
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-swapObjectHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION swapObjectHandle Procedure 
FUNCTION swapObjectHandle RETURNS LOGICAL
  ( INPUT ihFrom AS HANDLE,
    INPUT ihTo   AS HANDLE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE result AS LOGICAL NO-UNDO INIT YES.

IF ihFrom = ihTo THEN RETURN NO.

FOR EACH ttObject
    WHERE ttObject.hObject = ihFrom
    :
  FOR EACH ttObjectLink
      WHERE ttObjectLink.hFromObject = ihFrom
      :
    IF NOT CAN-FIND(FIRST bttObjectLink
                          WHERE bttObjectLink.hFromObject = ihTo 
                            AND bttObjectLink.hToObject = ttObjectLink.hToObject
                            AND bttObjectLink.cLinkType = ttObjectLink.cLinkType) THEN    
      ttObjectLink.hFromObject = ihTo.
  END.
  FOR EACH ttObjectLink
      WHERE ttObjectLink.hToObject = ihFrom
      :
    IF NOT CAN-FIND(FIRST bttObjectLink
                          WHERE bttObjectLink.hToObject = ihTo 
                            AND bttObjectLink.hFromObject = ttObjectLink.hFromObject
                            AND bttObjectLink.cLinkType = ttObjectLink.cLinkType) THEN    
      ttObjectLink.hToObject = ihTo.
  END.
  FOR EACH ttEvent
      WHERE ttEvent.hObject = ihFrom
      :
    FIND FIRST bttEvent 
         WHERE bttEvent.hObject = ihTo
           AND bttEvent.cName   = ttEvent.cName
           AND bttEvent.hWidget = ttEvent.hWidget
         NO-ERROR.
    IF AVAIL bttEvent THEN DELETE bttEvent.    
    ttEvent.hObject = ihTo.
  END.
  
  FIND FIRST bttObject
       WHERE bttObject.hObject = ihTo
       NO-ERROR.
  IF AVAIL bttObject THEN 
    DELETE bttObject.
  
  ttObject.hObject = ihTo.
END.

FOR EACH ttAttribute 
    WHERE ttAttribute.hObject = ihFrom    
    :
  FIND FIRST bttAttribute
       WHERE bttAttribute.hObject = ihTo
         AND bttAttribute.cName   = ttAttribute.cName
       NO-ERROR.
  IF AVAIL bttAttribute THEN DELETE bttAttribute.    
  ttAttribute.hObject = ihTo.
END.

RETURN result.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewAverageColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewAverageColumn Procedure 
FUNCTION ViewAverageColumn RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT ibView   AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = "jbAverage" THEN DO:
    ihBrowse:GET-BROWSE-COLUMN(ix):VISIBLE = ibView.
    RETURN YES.
  END.
END.

RETURN FALSE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewCountDistinctColumn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewCountDistinctColumn Procedure 
FUNCTION ViewCountDistinctColumn RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT ibView   AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  IF ihBrowse:GET-BROWSE-COLUMN(ix):NAME = "jbCountDistinct" THEN DO:
    ihBrowse:GET-BROWSE-COLUMN(ix):VISIBLE = ibView.
    RETURN YES.
  END.
END.

RETURN FALSE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewHideFieldMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewHideFieldMap Procedure 
FUNCTION ViewHideFieldMap RETURNS LOGICAL
  ( INPUT ihFieldMap AS HANDLE,
    INPUT ibView     AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Hide the content of a record buffer (the fieldMap handle) using
           the associated lists of widget handles   
    Notes: The widget handles might span multiple frames 
------------------------------------------------------------------------------*/
DEF VAR hWidget                 AS HANDLE NO-UNDO.
DEF VAR cScreenWidgets          AS CHAR   NO-UNDO. 
DEF VAR cBufferFields           AS CHAR   NO-UNDO.
DEF VAR cAvailWidgets           AS CHAR   NO-UNDO.

ASSIGN cScreenWidgets          = TRIM(getAttribute(ihFieldMap,"ScreenUpdateWidgets") + "," + getAttribute(ihFieldMap,"ScreenDisplayWidgets"),",")
       cBufferFields           = TRIM(getAttribute(ihFieldMap,"BufferUpdateFields")  + "," + getAttribute(ihFieldMap,"BufferDisplayFields"),",")
       cAvailWidgets           = getAttribute(ihFieldMap,"recordavailwidgets") + "," + getAttribute(ihFieldMap,"recordmodifywidgets")
       .

DO ix = 1 TO NUM-ENTRIES(cScreenWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cScreenWidgets)) NO-ERROR.
  IF VALID-HANDLE(hWidget) THEN hWidget:HIDDEN = NOT ibView.
  ELSE
    MESSAGE "Field reference " ENTRY(ix,cScreenWidgets) " is invalid" SKIP
            "(Doesn't exist in frame)"
            VIEW-AS ALERT-BOX ERROR TITLE "JukeBox, Design Error".

END.

/* disable any widgets linked to the fieldMap that depend on record availability: */
IF NOT ibView THEN DO ix = 1 TO NUM-ENTRIES(cAvailWidgets):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cAvailWidgets)) NO-ERROR.
  IF VALID-HANDLE(hWidget) THEN 
    hWidget:SENSITIVE = ihFieldMap:AVAIL.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewRecordCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewRecordCount Procedure 
FUNCTION ViewRecordCount RETURNS LOGICAL
  ( INPUT ihObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: 
------------------------------------------------------------------------------*/
DEF VAR hRecordCountWidget AS HANDLE NO-UNDO.
DEF VAR iNumZ              AS INT    NO-UNDO.
DEF VAR cCount             AS CHAR   NO-UNDO.
DEF VAR cPlus              AS CHAR   NO-UNDO.

IF NOT VALID-HANDLE(ihObject) OR getAttribute(ihObject,"viewRecordCount") = "no" THEN RETURN FALSE.

IF getAttribute(ihObject,"uselocaldata") = "yes" 
   OR getAttribute(ihObject,"recordcount") NE "" THEN
 cCount = STRING(INT(getAttribute(ihObject,"recordcount")) + INT(getAttribute(ihObject,"rowsadded")) - INT(getAttribute(ihObject,"rowsdeleted"))).
ELSE IF ihObject:TYPE = "browse" THEN
 ASSIGN cCount = STRING(INT(getAttribute(ihObject,"currentcount")) + INT(getAttribute(ihObject,"rowsadded")) - INT(getAttribute(ihObject,"rowsdeleted"))) 
        cPlus  = IF getAttribute(ihObject,"lastrowid") = "" AND INT(getAttribute(ihObject,"currentcount")) NE 0 THEN "+" ELSE "".
ELSE RETURN NO.      

IF INTEGER(cCount) < 0 THEN cCount = "0".

setAttribute(ihObject,"recordcount",cCount).
setAttribute(ihObject,"currentcount",cCount).
setAttribute(ihObject,"rowsadded","").
setAttribute(ihObject,"rowsdeleted","").

IF ihObject:TYPE = "browse" THEN 
  hRecordCountWidget = WIDGET-HANDLE(getAttribute(ihObject:WINDOW,"RecordCountWidget")) NO-ERROR.
ELSE DO:
  FIND bttObject WHERE bttObject.hObject = ihObject NO-ERROR.
  IF AVAIL bttObject AND VALID-HANDLE(bttObject.hWindow) AND NOT bttObject.hWindow:STATUS-AREA THEN 
    hRecordCountWidget = WIDGET-HANDLE(getAttribute(bttObject.hWindow,"RecordCountWidget")) NO-ERROR.
END.
  
IF VALID-HANDLE(hRecordCountWidget) THEN 
  ASSIGN iNumZ = (hRecordCountWidget:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS("..9",hRecordCountWidget:FONT)) 
               / FONT-TABLE:GET-TEXT-WIDTH-PIXELS("z",hRecordCountWidget:FONT)
         hRecordCountWidget:SCREEN-VALUE = STRING(INT(cCount),FILL("z",iNumZ) + "9") + cPlus.
ELSE DO:
  cCount = (IF DYNAMIC-FUNCTION("Scandinavian") THEN "[Ant: " ELSE "[Count: ") + cCount + "]".
  IF ihObject:TYPE = "browse" THEN
    ihObject:HELP = cCount + cPlus.
  ELSE IF AVAIL bttObject AND VALID-HANDLE(bttObject.hWindow) AND bttObject.hWindow:STATUS-AREA THEN
    STATUS INPUT cCount IN WINDOW bttObject.hWindow.
END.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

