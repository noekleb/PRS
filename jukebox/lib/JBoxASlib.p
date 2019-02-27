&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : ChemistryLib.p
    Purpose     : Gather all kinds of handy functions for the client (and the server)

    Syntax      :

    Description :

    Author(s)   : Not all functions are written by the Chemistry team. Whenever available, the
                  author is mentioned..
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR bOK                       AS LOG NO-UNDO.
DEF VAR ix                        AS INT NO-UNDO.
DEF VAR iDefaultSortFont          AS INT INIT 6.    /* Moved to UI_lib */
DEF VAR bSetSortLabel             AS LOG INIT TRUE. /* Moved to UI_lib */
DEF VAR cAppTitle                 AS CHAR NO-UNDO.

DEF VAR iCompanyId                AS INT  NO-UNDO.
DEF VAR cCompanyName              AS CHAR NO-UNDO.
DEF VAR cCompanyLogo              AS CHAR NO-UNDO.
DEF VAR cCodeMasterCompanyId      AS CHAR NO-UNDO.
DEF VAR cParentCompanyId          AS CHAR NO-UNDO.
DEF VAR cUserLevel                AS CHAR NO-UNDO.
DEF VAR bMenuRestart              AS LOG  NO-UNDO.

/* Behaviour settings: */
DEF VAR cTransLogFile             AS CHAR NO-UNDO.
DEF VAR cQueryLogFile             AS CHAR NO-UNDO.
DEF VAR cMemInfoLogFile           AS CHAR NO-UNDO.
DEF VAR bLoadDataDictOnStartup    AS LOG  NO-UNDO.
DEF VAR bShowHourGlass            AS LOG  NO-UNDO INIT TRUE.
DEF VAR cUserSettingSortExp       AS CHAR NO-UNDO.

/* Language control: */
DEF VAR cLanguage                 AS CHAR NO-UNDO INIT "NO".
DEF VAR cBaseLanguage             AS CHAR NO-UNDO INIT "NO".
DEF VAR cAppLanguages             AS CHAR NO-UNDO INIT "NO".
DEF VAR httTranslation            AS HANDLE NO-UNDO.
DEF VAR cTransSourceFile           AS CHAR NO-UNDO.
DEF VAR hBuffTranslation          AS HANDLE NO-UNDO.
DEF VAR cWidgetType               AS CHAR NO-UNDO.
DEF VAR cWidgetName               AS CHAR NO-UNDO.
DEF VAR cLabelList                AS CHAR NO-UNDO
    INIT "BROWSE-COLUMN,BUTTON,COMBO-BOX,DBFIELD,FILL-IN,MENU-ITEM,RADIO-SET,SUB-MENU,TOGGLE-BOX,TEXT-FILL-IN".
DEF VAR cToolTipList              AS CHAR NO-UNDO
    INIT "BROWSE,BUTTON,COMBO-BOX,DBFIELD,EDITOR,FILL-IN,RADIO-SET,SELECTION-LIST,SLIDER,TOGGLE-BOX".
DEF VAR cTitleList                AS CHAR NO-UNDO
    INIT "DIALOG-BOX,FRAME,WINDOW".
DEF VAR cTranslationReportFile    AS CHAR   NO-UNDO.
DEF VAR hTranManBuffer            AS HANDLE NO-UNDO.
DEF VAR hTranManProc              AS HANDLE NO-UNDO.
DEF VAR cTransObjectTypeList      AS CHAR   NO-UNDO.
DEF VAR bDynamicsTranslation      AS LOG    NO-UNDO.                                  

/* Appserver communication: */
DEF VAR cASUserId                 AS CHAR   NO-UNDO.
DEF VAR cASUserName               AS CHAR   NO-UNDO.
DEF VAR hAppServer                AS HANDLE NO-UNDO.
DEF VAR hUseAppServer             AS HANDLE NO-UNDO.
DEF VAR cAppServiceId             AS CHAR   NO-UNDO.
DEF VAR cAppServiceConnectString  AS CHAR   NO-UNDO.
DEF VAR cSessionId                AS CHAR   NO-UNDO.   
DEF VAR cQueryStatFields          AS CHAR   NO-UNDO.  /* Must be set for each run */
DEF VAR cQueryStatFieldValues     AS CHAR   NO-UNDO.  /* Must be retrieved for each run */
DEF VAR bIndexOnRowids            AS LOG    NO-UNDO.  /* Add index on all rowid's in result set from gettemptablejoin */
DEF VAR cPreScanQuery             AS CHAR   NO-UNDO.
DEF VAR iPreScanLimit             AS INT    NO-UNDO.  /* Max num records for prescan */
DEF VAR httRunProc                AS HANDLE NO-UNDO.  /* Handle to tt returned from runProc */
DEF VAR hdsRunProc                AS HANDLE NO-UNDO.  /* Handle to ds returned from runProcDs */
def var iReConnectPause as int init ? no-undo. /*initiate with ? so that 0 can be used*/
def var iReConnectAttempts as int init ? no-undo.  /*initiate with ? so that 0 can be used*/
DEF VAR cQueryStopAfter           AS CHAR   NO-UNDO.
DEF VAR cOrgBuffersAndFields            AS CHAR   NO-UNDO.

/* Temp-tables and variables for Transaction.engine: */
DEF TEMP-TABLE ttTransAction NO-UNDO
    FIELD iTransActionId          AS INT
    FIELD cTransActionBuffer      AS CHAR
    FIELD cTransAction            AS CHAR
    FIELD cTransActionCriteria    AS CHAR /* "ROWID", "UNIQUE" or query criteria */
    FIELD cTransActionErrorHandl  AS CHAR /* "IGNORE"(ref.int check), "AVAIL"(process when avail), "RETRY"(not impl). Default blank (undo,abort) */
    FIELD cTransActionIdFields    AS CHAR 
    FIELD cTransActionValueFields AS CHAR
    FIELD cTransActionValidation  AS CHAR /* NO: no val. =myval.p: only myval.p. +myval.p add.valproc myval.p */ 
    FIELD cTransActionPostUpdProc AS CHAR
    INDEX idxTransActionId        IS PRIMARY iTransActionId 
    .
/* ROWID:  Rowid's for records to delete / update must be given as cTransRecordIdValue 
   UNIQUE: Id columns set in cTransActionIdFields must be matched by corresponding values
           in cTransRecordIdValue
   query criteria: WHERE criteria for a record(set) that should be updated by values
           in the ONE ttTransRecord table that is linked to this ttTransAction
*/

DEF TEMP-TABLE ttTransRecord NO-UNDO
    FIELD iTransActionId          AS INT
    FIELD iTransRecordId          AS INT
    FIELD cTransRecordIdValues    AS CHAR        
    FIELD cTransRecordValues      AS CHAR
    FIELD cTransRecordRowid       AS CHAR   
    FIELD cReturnExtraFields      AS CHAR
    FIELD cReturnExtraValues      AS CHAR
    INDEX idxTransRecordId        IS PRIMARY iTransRecordId 
    INDEX idxTransActionId        iTransActionId
    .

DEF VAR ixTransActionId           AS INT NO-UNDO.
DEF VAR ixTransRecordId           AS INT NO-UNDO.
DEF VAR httTransAction            AS HANDLE NO-UNDO.
DEF VAR httTransRecord            AS HANDLE NO-UNDO.
httTransAction = BUFFER ttTransAction:HANDLE:TABLE-HANDLE.
httTransrecord = BUFFER ttTransRecord:HANDLE:TABLE-HANDLE.
DEF VAR cReturnTrans              AS CHAR NO-UNDO.
DEF VAR cPostUpdProc              AS CHAR NO-UNDO.
DEF VAR cCalcFieldProc            AS CHAR NO-UNDO.
DEF VAR cCalcFieldFilter          AS CHAR NO-UNDO.
DEF VAR cNoDbReadAccessFields     AS CHAR NO-UNDO.
DEF VAR cNoDbWriteAccessFields    AS CHAR NO-UNDO.
DEF VAR cNoDbReadAccessBuffers    AS CHAR NO-UNDO.
DEF VAR cNoDbWriteAccessBuffers   AS CHAR NO-UNDO.
DEF VAR cNoDbCreateAccessBuffers  AS CHAR NO-UNDO.
DEF VAR cNoDbDeleteAccessBuffers  AS CHAR NO-UNDO.
DEF VAR bReturnQueryInfo          AS LOG  NO-UNDO.
DEF VAR cBufferFieldSpec          AS CHAR NO-UNDO.
DEF VAR cBufferDbFieldSpec        AS CHAR NO-UNDO.
DEF VAR cNoExistBuffers           AS CHAR NO-UNDO.
DEF VAR bOpenUnlessRestricted     AS LOG  NO-UNDO INIT YES.
DEF VAR cCurrentRowid             AS CHAR NO-UNDO.
DEF VAR cServerReturnParam        AS CHAR NO-UNDO.
DEF VAR cServerTransReturnParam   AS CHAR NO-UNDO.
DEF VAR cServerTransInputParam    AS CHAR NO-UNDO.
DEF VAR cSkipCalcFields           AS CHAR NO-UNDO.
DEF VAR cAppendWinCompanyTitle    AS CHAR NO-UNDO.
DEF VAR cUniqueBuffer             AS CHAR NO-UNDO.

/* Query module: */
/* &IF DEFINED (ttDataDictDefined) = 0 &THEN  */
/* &SCOPED-DEFINE ttDataDictDefined           */
/* {incl/ttDataDict.i}                        */
/* &ENDIF                                     */
DEF VAR cDBFilter                 AS CHAR NO-UNDO INIT "*".
DEF VAR cTableFilter              AS CHAR NO-UNDO INIT "WHERE NOT cFileName BEGINS 'JBox'".
DEF VAR cFieldFilter              AS CHAR NO-UNDO INIT "  AND NOT CAN-DO('iJBoxCompanyId',cFieldName)".

DEF TEMP-TABLE ttCache
    FIELD cTableName AS CHAR
    FIELD hTempTable AS HANDLE
    FIELD hBuffer    AS HANDLE
    INDEX idxTableName cTableName
    INDEX idxTT        hTempTable
    INDEX idxBuff      hBuffer.

DEF VAR hBuffCache AS HANDLE NO-UNDO.
hBuffCache = BUFFER ttCache:HANDLE.

DEF TEMP-TABLE ttFunctionAccess
    FIELD cClientFileName AS CHAR
    FIELD bMenu           AS LOG
    FIELD cObjectName     AS CHAR
    FIELD cAction         AS CHAR
    FIELD bAccess         AS LOG
    INDEX idxCOA  cClientFileName cObjectName cAction
    INDEX idxMenu bMenu.
    
DEF VAR hBuffClientAccess AS HANDLE NO-UNDO.
DEF VAR httClientAccess   AS HANDLE NO-UNDO.
hBuffClientAccess = BUFFER ttFunctionAccess:HANDLE.
httClientAccess = hBuffClientAccess:TABLE-HANDLE.

DEF TEMP-TABLE tt_field
    FIELD Db-Name          AS CHAR
    FIELD Table-Name       AS CHAR 
    FIELD Field-Name       AS CHAR
    FIELD Field-Type       AS CHAR
    FIELD Field-Label      AS CHAR
    FIELD Field-Format     AS CHAR
    FIELD Field-Initial    AS CHAR
    FIELD Field-Help       AS CHAR
    FIELD Field-Order      AS INT
    FIELD Field-Extent     AS INT
    FIELD Table-Can-Read   AS CHAR
    FIELD Table-Can-Write  AS CHAR
    FIELD Table-Can-Create AS CHAR
    FIELD Table-Can-Delete AS CHAR
    FIELD Field-Can-Read   AS CHAR
    FIELD Field-Can-Write  AS CHAR
    INDEX idxFileField Table-Name Field-Name
    INDEX idxField Field-Name
    .

DEF VAR htt_field         AS HANDLE NO-UNDO.
htt_field = BUFFER tt_field:HANDLE.

DEF TEMP-TABLE ttDefaultTranslation
    FIELD cLanguage   AS CHARACTER
    FIELD cFieldName  AS CHAR 
    FIELD cFieldLabel AS CHAR
    FIELD cFieldHelp  AS CHAR
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

&IF DEFINED(EXCLUDE-assignStringValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignStringValue Procedure 
FUNCTION assignStringValue RETURNS LOGICAL
  ( INPUT ihField       AS HANDLE,
    INPUT icUpdateValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildTableCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BuildTableCache Procedure 
FUNCTION BuildTableCache RETURNS LOGICAL
  ( INPUT icCacheList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckReturnValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckReturnValue Procedure 
FUNCTION CheckReturnValue RETURNS LOGICAL
  ( INPUT icReturnTrans AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearTableCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ClearTableCache Procedure 
FUNCTION ClearTableCache RETURNS LOGICAL
  ( INPUT icCacheList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateDataSet Procedure 
FUNCTION CreateDataSet RETURNS HANDLE
  ( INPUT icBufferHandleList AS CHAR,
    INPUT icRelationFields   AS CHAR,
    INPUT icSourceKeyList    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateExtentCalcField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateExtentCalcField Procedure 
FUNCTION CreateExtentCalcField RETURNS CHARACTER
  ( INPUT ohTempTable  AS HANDLE,
    INPUT icFieldName  AS CHAR,
    INPUT icDataType   AS CHAR,
    INPUT icFormat     AS CHAR,
    INPUT icLabel      AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deleteRows Procedure 
FUNCTION deleteRows RETURNS CHARACTER
  ( INPUT cBufferName AS CHAR,
    INPUT cRowIdList  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCommit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoCommit Procedure 
FUNCTION DoCommit RETURNS LOGICAL
  ( INPUT ibReturnAll AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoCreate Procedure 
FUNCTION DoCreate RETURNS LOGICAL
  ( INPUT icBuffer       AS CHAR,
    INPUT icInstructions AS CHAR,  /* References to hooks for create, field validation and post update */
    INPUT icValueFields  AS CHAR,
    INPUT icValues       AS CHAR,
    INPUT ibCommit       AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoDelete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoDelete Procedure 
FUNCTION DoDelete RETURNS LOGICAL
  ( INPUT icBuffer       AS CHAR,
    INPUT icInstructions AS CHAR,  /* Directions delete validate and post delete */
    INPUT icIdFldsOrCrit AS CHAR,  /* Comma sep. field names for UNIQUE match or blank when icIdValues contains a ROWID */
    INPUT icIdValues     AS CHAR,  /* Acutal ROWID or CHR(1)-delimited) value(s) for corresp id fields */ 
    INPUT ibCommit       AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoRefetchTrans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoRefetchTrans Procedure 
FUNCTION DoRefetchTrans RETURNS LOGICAL
  ( INPUT ihBuffer        AS HANDLE,
    INPUT icFieldsToMatch AS CHAR,
    INPUT icValuesToMatch AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoRefetchValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoRefetchValues Procedure
FUNCTION DoRefetchValues RETURNS CHARACTER 
  (INPUT icFieldList     AS CHAR,
   INPUT icFieldsToMatch AS CHAR,
   INPUT icValuesToMatch AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-DoUpdate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoUpdate Procedure 
FUNCTION DoUpdate RETURNS LOGICAL
  ( INPUT icBuffer       AS CHAR,
    INPUT icInstructions AS CHAR,  /* References to hooks for field validation and post update */
    INPUT icIdFldsOrCrit AS CHAR,  /* Comma sep. field names for UNIQUE match or blank when icIdValues contains a ROWID */ 
    INPUT icIdValues     AS CHAR,  /* Acutal ROWID or CHR(1)-delimited) value(s) for corresp id fields */                  
    INPUT icValueFields  AS CHAR,
    INPUT icValues       AS CHAR,
    INPUT ibCommit       AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dumpMemInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dumpMemInfo Procedure 
FUNCTION dumpMemInfo RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DumpStaticASlibTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DumpStaticASlibTables Procedure 
FUNCTION DumpStaticASlibTables RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getActionPermission) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getActionPermission Procedure 
FUNCTION getActionPermission RETURNS LOGICAL
  ( INPUT icClientFileName AS CHAR,
    INPUT icObjectName     AS CHAR,
    INPUT icAction         AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppserviceHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAppserviceHandle Procedure 
FUNCTION getAppserviceHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppserviceId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAppserviceId Procedure 
FUNCTION getAppserviceId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAppTitle Procedure 
FUNCTION getAppTitle RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASlibBeaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASlibBeaviour Procedure 
FUNCTION getASlibBeaviour RETURNS CHARACTER
  ( INPUT icBehaviour AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASUserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASUserId Procedure 
FUNCTION getASUserId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASuserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASuserName Procedure 
FUNCTION getASuserName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBaseLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBaseLanguageCode Procedure 
FUNCTION getBaseLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferDbFieldSpec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBufferDbFieldSpec Procedure 
FUNCTION getBufferDbFieldSpec RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferFieldSpec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBufferFieldSpec Procedure 
FUNCTION getBufferFieldSpec RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-getCacheBufferHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCacheBufferHandle Procedure 
FUNCTION getCacheBufferHandle RETURNS HANDLE
  ( INPUT icCacheTable AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCompany Procedure 
FUNCTION getCompany RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCompanyId Procedure 
FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyLogo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCompanyLogo Procedure 
FUNCTION getCompanyLogo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCompanyName Procedure 
FUNCTION getCompanyName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentChanged Procedure 
FUNCTION getCurrentChanged RETURNS CHARACTER
  ( INPUT ihBuffer AS HANDLE,
    INPUT icFields AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDataDict) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDataDict Procedure 
FUNCTION getDataDict RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDate Procedure 
FUNCTION getDate RETURNS DATE
  (INPUT icDate AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldCacheBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldCacheBuffer Procedure 
FUNCTION getFieldCacheBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldDataType Procedure 
FUNCTION getFieldDataType RETURNS CHARACTER
  ( INPUT icDbField AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldList Procedure 
FUNCTION getFieldList RETURNS CHARACTER
  ( INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldsForTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldsForTable Procedure 
FUNCTION getFieldsForTable RETURNS CHARACTER
  ( INPUT icDbTable AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldValues Procedure 
FUNCTION getFieldValues RETURNS CHARACTER
  ( INPUT icBufferName    AS CHAR,
    INPUT icQueryCriteria AS CHAR,
    INPUT icColumns       AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFunctionRestrictions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFunctionRestrictions Procedure 
FUNCTION getFunctionRestrictions RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsBufferCached) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsBufferCached Procedure 
FUNCTION getIsBufferCached RETURNS LOGICAL
  ( INPUT ihBufferHandle AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsFieldInTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsFieldInTable Procedure 
FUNCTION getIsFieldInTable RETURNS LOGICAL
  ( INPUT icDbField AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsTableCached) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsTableCached Procedure 
FUNCTION getIsTableCached RETURNS LOGICAL
  ( INPUT ihTableHandle AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsTableInstalled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsTableInstalled Procedure 
FUNCTION getIsTableInstalled RETURNS LOGICAL
  ( INPUT icDbTable AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsUserSettingInstalled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsUserSettingInstalled Procedure 
FUNCTION getIsUserSettingInstalled RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLanguageCode Procedure 
FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLanguages Procedure 
FUNCTION getLanguages RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMenuPermission) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuPermission Procedure 
FUNCTION getMenuPermission RETURNS LOGICAL
  ( INPUT icAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMyDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyDataSet Procedure
FUNCTION getMyDataSet RETURNS LOGICAL 
  (INPUT icServerProgram  AS CHAR,
   INPUT icParamList      AS CHAR,
   INPUT ihTargetDS       AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-getMyTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyTempTable Procedure 
FUNCTION getMyTempTable RETURNS LOGICAL
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetBuffer   AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbCreateAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoDbCreateAccessBuffers Procedure 
FUNCTION getNoDbCreateAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbDeleteAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoDbDeleteAccessBuffers Procedure 
FUNCTION getNoDbDeleteAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbReadAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoDbReadAccessBuffers Procedure 
FUNCTION getNoDbReadAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbReadAccessFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoDbReadAccessFields Procedure 
FUNCTION getNoDbReadAccessFields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbWriteAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoDbWriteAccessBuffers Procedure 
FUNCTION getNoDbWriteAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbWriteAccessFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoDbWriteAccessFields Procedure 
FUNCTION getNoDbWriteAccessFields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentCompany Procedure 
FUNCTION getParentCompany RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryStatFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQueryStatFields Procedure 
FUNCTION getQueryStatFields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryStatFieldValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQueryStatFieldValues Procedure 
FUNCTION getQueryStatFieldValues RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRecId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRecId Procedure 
FUNCTION getRecId RETURNS RECID
  ( INPUT icBuffer   AS CHAR,
    INPUT icRowId    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRecordCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRecordCount Procedure 
FUNCTION getRecordCount RETURNS INTEGER
  ( INPUT icBuffer         AS CHAR,
    INPUT icQueryCriteria  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReturnedTransValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReturnedTransValue Procedure 
FUNCTION getReturnedTransValue RETURNS CHARACTER
  ( INPUT icFieldName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRole) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRole Procedure 
FUNCTION getRole RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRoleId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRoleId Procedure 
FUNCTION getRoleId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowIdList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowIdList Procedure 
FUNCTION getRowIdList RETURNS CHARACTER
  ( INPUT icBufferList         AS CHAR,
    INPUT icBufferReturnList   AS CHAR,
    INPUT icQueryCriteria      AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRunProcReturnDs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRunProcReturnDs Procedure 
FUNCTION getRunProcReturnDs RETURNS HANDLE
  ( INPUT ihTargetDs   AS HANDLE,
    INPUT ibAppend     AS LOG,
    INPUT ibReplace    AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRunProcReturnTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRunProcReturnTable Procedure 
FUNCTION getRunProcReturnTable RETURNS HANDLE
  ( INPUT ihTargetBuffer   AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getServerReturnParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getServerReturnParam Procedure 
FUNCTION getServerReturnParam RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getServerTransReturnParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getServerTransReturnParam Procedure 
FUNCTION getServerTransReturnParam RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSessionContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSessionContext Procedure 
FUNCTION getSessionContext RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSessionData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSessionData Procedure 
FUNCTION getSessionData RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSessionId Procedure 
FUNCTION getSessionId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStringTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStringTranslation Procedure 
FUNCTION getStringTranslation RETURNS CHARACTER
  ( INPUT icFileName    AS CHAR,
    INPUT icObjectType  AS CHAR,
    INPUT icObjectNames AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTempTable Procedure 
FUNCTION getTempTable RETURNS HANDLE
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetBuffer   AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTempTableJoin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTempTableJoin Procedure 
FUNCTION getTempTableJoin RETURNS HANDLE
  ( INPUT iiBatchSize        AS INT,
    INPUT iiStartRow         AS INT,
    INPUT icDirection        AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotal Procedure
FUNCTION getTotal RETURNS DECIMAL 
  (INPUT icBuffer         AS CHAR,
   INPUT icQueryCriteria  AS CHAR,
   INPUT icFieldName      AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getTransactionMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTransactionMessage Procedure 
FUNCTION getTransactionMessage RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransactionRowIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTransactionRowIds Procedure 
FUNCTION getTransactionRowIds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTranslation Procedure 
FUNCTION getTranslation RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE,
    INPUT iiLevel  AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTranslationBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTranslationBuffer Procedure 
FUNCTION getTranslationBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransObjectTypeList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTransObjectTypeList Procedure 
FUNCTION getTransObjectTypeList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransRecordBufferHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTransRecordBufferHandle Procedure 
FUNCTION getTransRecordBufferHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTJoinOnServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTTJoinOnServer Procedure 
FUNCTION getTTJoinOnServer RETURNS LOGICAL
  ( INPUT icDirection        AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icServerProgram    AS CHAR,
    INPUT icParamList        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserLevel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUserLevel Procedure 
FUNCTION getUserLevel RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserSetting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUserSetting Procedure 
FUNCTION getUserSetting RETURNS CHARACTER
  ( INPUT icSourceFile     AS CHAR,
    INPUT icObjectName     AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icSettingName    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initTranslation Procedure 
FUNCTION initTranslation RETURNS LOGICAL
  ( INPUT ihWidget  AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsFieldNameInTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD IsFieldNameInTable Procedure 
FUNCTION IsFieldNameInTable RETURNS LOGICAL
  ( INPUT cBufferName  AS CHAR,
    INPUT cColumn      AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakeEventLogEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MakeEventLogEntry Procedure 
FUNCTION MakeEventLogEntry RETURNS LOGICAL
  ( INPUT iiEventId        AS INT,
    INPUT icDescription    AS CHAR,
    INPUT icGroup          AS CHAR,
    INPUT icType           AS CHAR,
    INPUT icSource         AS CHAR,
    INPUT icCategory       AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OnlyCalcFldsInBufferDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OnlyCalcFldsInBufferDef Procedure 
FUNCTION OnlyCalcFldsInBufferDef RETURNS LOGICAL
  ( INPUT icBufferDef AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD putTempTable Procedure 
FUNCTION putTempTable RETURNS CHARACTER
  ( INPUT icServerProgram  AS CHAR,
    INPUT TABLE-HANDLE     ihTempTable,
    INPUT icParamList      AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-QueryUsesIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD QueryUsesIndex Procedure 
FUNCTION QueryUsesIndex RETURNS LOGICAL
  ( INPUT icBufferList  AS CHAR,
    INPUT icQueryString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReconnectServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReconnectServer Procedure 
FUNCTION ReconnectServer RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-refreshFromTransRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD refreshFromTransRecord Procedure 
FUNCTION refreshFromTransRecord RETURNS LOGICAL
  ( INPUT ihBuffer           AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-refreshRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD refreshRow Procedure 
FUNCTION refreshRow RETURNS LOGICAL
  ( INPUT ihObject           AS HANDLE,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryJoin        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resetTransRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD resetTransRecords Procedure 
FUNCTION resetTransRecords RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-runProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD runProc Procedure 
FUNCTION runProc RETURNS LOGICAL
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT httTable         AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-runProcDs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD runProcDs Procedure 
FUNCTION runProcDs RETURNS LOGICAL
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihDataset        AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Scandinavian) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Scandinavian Procedure 
FUNCTION Scandinavian RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppendWinCompanyTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppendWinCompanyTitle Procedure 
FUNCTION setAppendWinCompanyTitle RETURNS LOGICAL
  ( INPUT icAppendWinCompanyTitle AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppserviceConnectString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppserviceConnectString Procedure 
FUNCTION setAppserviceConnectString RETURNS LOGICAL
  ( INPUT icAppServiceConnectString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppserviceHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppserviceHandle Procedure 
FUNCTION setAppserviceHandle RETURNS LOGICAL
  ( INPUT ihAppserver AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppserviceId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppserviceId Procedure 
FUNCTION setAppserviceId RETURNS LOGICAL
  ( INPUT icAppServiceId AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setAppTitle Procedure 
FUNCTION setAppTitle RETURNS LOGICAL
  ( INPUT icAppTitle AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setASlibBehaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setASlibBehaviour Procedure 
FUNCTION setASlibBehaviour RETURNS LOGICAL
  ( INPUT icBehaviour AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setASUserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setASUserId Procedure 
FUNCTION setASUserId RETURNS LOGICAL
  ( INPUT icASUserId   AS CHAR,
    INPUT icASUserName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBaseLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBaseLanguageCode Procedure 
FUNCTION setBaseLanguageCode RETURNS LOGICAL
  ( INPUT icBaseLanguage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCacheFieldFormat Procedure 
FUNCTION setCacheFieldFormat RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icFormat     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCacheFieldHelp Procedure 
FUNCTION setCacheFieldHelp RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icHelp     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldInitValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCacheFieldInitValue Procedure 
FUNCTION setCacheFieldInitValue RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icInitValue     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCacheFieldLabel Procedure 
FUNCTION setCacheFieldLabel RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icLabel     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCalcFieldFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCalcFieldFilter Procedure 
FUNCTION setCalcFieldFilter RETURNS LOGICAL
  ( INPUT icCalcFieldFilter AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCalcFieldProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCalcFieldProc Procedure 
FUNCTION setCalcFieldProc RETURNS LOGICAL
  ( INPUT icCalcFieldProc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-setCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCodeMaster Procedure 
FUNCTION setCodeMaster RETURNS LOGICAL
  ( INPUT icCodeMasterCompanyId AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCompanyHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCompanyHeader Procedure 
FUNCTION setCompanyHeader RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCompanyId Procedure 
FUNCTION setCompanyId RETURNS LOGICAL
  ( INPUT iiCompanyId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrentRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCurrentRowid Procedure 
FUNCTION setCurrentRowid RETURNS LOGICAL
  ( INPUT icCurrentRowid AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDataDictFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDataDictFilter Procedure 
FUNCTION setDataDictFilter RETURNS LOGICAL
  ( INPUT icDBFilter    AS CHAR,
    INPUT icTableFilter AS CHAR,
    INPUT icFieldFilter AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDefaultTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDefaultTranslation Procedure 
FUNCTION setDefaultTranslation RETURNS LOGICAL
  ( INPUT icLanguageCode AS CHAR,
    INPUT icFieldName    AS CHAR,
    INPUT icFieldLabel   AS CHAR,
    INPUT icFieldHelp    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setIndexOnRowids) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setIndexOnRowids Procedure 
FUNCTION setIndexOnRowids RETURNS LOGICAL
  ( INPUT ibIndexOnRowids AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLanguageCode Procedure 
FUNCTION setLanguageCode RETURNS LOGICAL
  ( INPUT icLanguage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLanguages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLanguages Procedure 
FUNCTION setLanguages RETURNS LOGICAL
  ( INPUT icAppLanguages AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoadDataDictOnStartup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLoadDataDictOnStartup Procedure 
FUNCTION setLoadDataDictOnStartup RETURNS LOGICAL
  ( INPUT ibLoadDataDictOnStartup AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMenuRestart) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMenuRestart Procedure 
FUNCTION setMenuRestart RETURNS LOGICAL
  ( INPUT ibMenuRestart AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoExistBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoExistBuffers Procedure 
FUNCTION setNoExistBuffers RETURNS LOGICAL
  ( INPUT icNoExistBuffers AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOrgBufferList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOrgBufferList Procedure
FUNCTION setOrgBuffersAndFields RETURNS LOGICAL 
  (INPUT icOrgBuffersAndFields AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setPostUpdProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPostUpdProc Procedure 
FUNCTION setPostUpdProc RETURNS LOGICAL
  ( INPUT icPostUpdProc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreScanLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPreScanLimit Procedure 
FUNCTION setPreScanLimit RETURNS LOGICAL
  ( INPUT iiPreScanLimit AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreScanQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPreScanQuery Procedure 
FUNCTION setPreScanQuery RETURNS LOGICAL
  ( INPUT icPreScanQuery AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setQueryStatFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQueryStatFields Procedure 
FUNCTION setQueryStatFields RETURNS LOGICAL
  ( INPUT icQueryStatFields AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setQueryStatValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQueryStatValues Procedure 
FUNCTION setQueryStatValues RETURNS LOGICAL
  ( INPUT icQueryStatFieldValues AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setQueryStopAfter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQueryStopAfter Procedure
FUNCTION setQueryStopAfter RETURNS LOGICAL 
  (INPUT icQueryStopAfter AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setReturnQueryInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setReturnQueryInfo Procedure 
FUNCTION setReturnQueryInfo RETURNS LOGICAL
  ( INPUT ibReturnQueryInfo AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setServerTransInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setServerTransInputParam Procedure 
FUNCTION setServerTransInputParam RETURNS LOGICAL
  ( INPUT icServerTransInputParam AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSessionContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSessionContext Procedure 
FUNCTION setSessionContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSessionId Procedure 
FUNCTION setSessionId RETURNS LOGICAL
  ( INPUT icSessionId AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setShowHourGlass) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setShowHourGlass Procedure 
FUNCTION setShowHourGlass RETURNS LOGICAL
  ( INPUT ibShowHourGlass AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSkipCalcFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSkipCalcFields Procedure 
FUNCTION setSkipCalcFields RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSourceFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSourceFileName Procedure 
FUNCTION setSourceFileName RETURNS LOGICAL
  ( icTransSourceFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTranManBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTranManBuffer Procedure 
FUNCTION setTranManBuffer RETURNS LOGICAL
  ( INPUT ihTranManBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTranManProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTranManProc Procedure 
FUNCTION setTranManProc RETURNS LOGICAL
  ( INPUT ihTranManProc AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTransactionMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTransactionMessage Procedure 
FUNCTION setTransactionMessage RETURNS LOGICAL
  ( INPUT icReturnTrans AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTranslationRepFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTranslationRepFile Procedure 
FUNCTION setTranslationRepFile RETURNS LOGICAL
  ( INPUT icTranslationReportFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUniqueBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUniqueBuffer Procedure 
FUNCTION setUniqueBuffer RETURNS LOGICAL
  ( INPUT icUniqueBuffer AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUseAppserver Procedure 
FUNCTION setUseAppserver RETURNS LOGICAL
  ( INPUT ihUseAppServer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseDynamicsTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUseDynamicsTranslation Procedure 
FUNCTION setUseDynamicsTranslation RETURNS LOGICAL
  ( INPUT ibDynamicsTranslation AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUserSetting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUserSetting Procedure 
FUNCTION setUserSetting RETURNS LOGICAL
  ( INPUT icSourceFile     AS CHAR,
    INPUT icObjectName     AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icSettingName    AS CHAR,
    INPUT icSetting        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUserSettingSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUserSettingSort Procedure
FUNCTION setUserSettingSort RETURNS LOGICAL 
  (INPUT icUserSettingSortExp AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setWidgetTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWidgetTranslation Procedure 
FUNCTION setWidgetTranslation RETURNS LOGICAL
  ( INPUT ihTranslationBuffer AS HANDLE,
    INPUT ihWidget            AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TranManInitBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TranManInitBrowse Procedure 
FUNCTION TranManInitBrowse RETURNS HANDLE
  ( INPUT ihFrame            AS HANDLE,
    INPUT ihRect             AS HANDLE,
    INPUT icProperties       AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR)  FORWARD.

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
         HEIGHT             = 35.19
         WIDTH              = 85.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/demo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "dCreated"
       ttDefaultTranslation.cFieldLabel = "Created"
       ttDefaultTranslation.cFieldHelp  = "Created date"
       .
CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "dModified"
       ttDefaultTranslation.cFieldLabel = "Modified"
       ttDefaultTranslation.cFieldHelp  = "Modified date"
       .
CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "cCreatedBy"
       ttDefaultTranslation.cFieldLabel = "By"
       ttDefaultTranslation.cFieldHelp  = "Created by"
       .
CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "cModifiedBy"
       ttDefaultTranslation.cFieldLabel = "By"
       ttDefaultTranslation.cFieldHelp  = "Modified by"
       .
CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "cJBoxUserId"
       ttDefaultTranslation.cFieldLabel = "Userid"
       ttDefaultTranslation.cFieldHelp  = "Userid"
       .
CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "cUserName"
       ttDefaultTranslation.cFieldLabel = "User name"
       ttDefaultTranslation.cFieldHelp  = "User name"
       .
CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "cUserName"
       ttDefaultTranslation.cFieldLabel = "User name"
       ttDefaultTranslation.cFieldHelp  = "User name"
       .
CREATE ttDefaultTranslation.
ASSIGN ttDefaultTranslation.cLanguage   = "EN"
       ttDefaultTranslation.cFieldName  = "cCompanyName"
       ttDefaultTranslation.cFieldLabel = "Name"
       ttDefaultTranslation.cFieldHelp  = "Company name"
       .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreateQueryFromCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateQueryFromCache Procedure 
PROCEDURE CreateQueryFromCache :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icBuffersAndFields AS CHAR   NO-UNDO.
DEF INPUT PARAM icQueryCriteria    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ohTempTable       AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ocReturn          AS CHAR   NO-UNDO.

DEF VAR cBufferDef         AS CHAR   NO-UNDO.
DEF VAR cDbName            AS CHAR   NO-UNDO.
DEF VAR cBufferName        AS CHAR   NO-UNDO.
DEF VAR cField             AS CHAR   NO-UNDO.
DEF VAR cTTname            AS CHAR   NO-UNDO.
DEF VAR bFieldSpec         AS LOG    NO-UNDO.
DEF VAR cRealFieldSpec     AS CHAR   NO-UNDO.
DEF VAR cRealDbFieldSpec   AS CHAR   NO-UNDO.
DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR iy                 AS INT    NO-UNDO.
DEF VAR iz                 AS INT    NO-UNDO.
DEF VAR iBufferCount       AS INT    NO-UNDO.
DEF VAR bOnlyCalcFieldsDef AS LOG    NO-UNDO.

DEF VAR cAddedFields       AS CHAR   NO-UNDO.            /* All fields added to temp-table */
DEF VAR cDuplicateFields   AS CHAR   NO-UNDO EXTENT 100. /* Duplicate fields pr buffer num */
DEF VAR cExtraField        AS CHAR   NO-UNDO.
DEF VAR cExtraDataType     AS CHAR   NO-UNDO.
DEF VAR cExtraFormat       AS CHAR   NO-UNDO.
DEF VAR cExtraInit         AS CHAR   NO-UNDO. 
DEF VAR cExtraParam        AS CHAR   NO-UNDO.
DEF VAR cExtraLabel        AS CHAR   NO-UNDO.
DEF VAR cFieldName         AS CHAR   NO-UNDO.
DEF VAR cBufferList        AS CHAR   NO-UNDO.  
DEF VAR cOverrideLabel     AS CHAR   NO-UNDO.
DEF VAR cOverrideFormat    AS CHAR   NO-UNDO.
DEF VAR cReturnOKmessage   AS CHAR   NO-UNDO.
DEF VAR bExtent            AS LOG    NO-UNDO.
DEF VAR iExtent            AS INT    NO-UNDO.
DEF VAR httBuffer          AS HANDLE NO-UNDO.
DEF VAR hField             AS HANDLE NO-UNDO.

DEF VAR cNotCanReadFlds    AS CHAR   NO-UNDO.
DEF VAR cNotCanWriteFlds   AS CHAR   NO-UNDO.
DEF VAR cNotCanReadBuffs   AS CHAR   NO-UNDO.
DEF VAR cNotCanCreateBuffs AS CHAR   NO-UNDO.
DEF VAR cNotCanWriteBuffs  AS CHAR   NO-UNDO.
DEF VAR cNotCanDeleteBuffs AS CHAR   NO-UNDO.

CREATE TEMP-TABLE ohTempTable.

iBufferCount = NUM-ENTRIES(icBuffersAndFields).

DO ix = 1 TO iBufferCount:
  ASSIGN cBufferDef       = ENTRY(ix,icBuffersAndFields)
         cBufferName      = ENTRY(1,cBufferDef,";").
  IF NUM-ENTRIES(cBufferName,".") > 1 THEN
    ASSIGN cDbName      = ENTRY(1,cBufferName,".")
           cBufferName  = ENTRY(2,cBufferName,".")
           .
  ELSE cDbName = "".

  IF cBufferName MATCHES "buf*_*" THEN cBufferName = SUBSTR(cBufferName,6).

  bOnlyCalcFieldsDef = OnlyCalcFldsInBufferDef(cBufferDef).

  IF NUM-ENTRIES(cBufferDef,";") > 1 AND ENTRY(2,cBufferDef,";") NE "" THEN DO:
    IF NOT bFieldSpec AND NOT bOnlyCalcFieldsDef THEN
      bFieldSpec = YES.
/*     bFieldSpec = YES. */
    DO iy = 2 TO NUM-ENTRIES(cBufferDef,";"):
      /* Calculated field (or just an extra field in the buffer): */
      bExtent = NO.
      IF ENTRY(iy,cBufferDef,";") BEGINS "+" OR SUBSTR(ENTRY(1,ENTRY(iy,cBufferDef,";"),"|"),LENGTH(ENTRY(1,ENTRY(iy,cBufferDef,";"),"|")),1) = "]" THEN DO:
        DO iz = 1 TO NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|"):
          CASE iz:
            WHEN 1 THEN DO:
              IF SUBSTR(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),LENGTH(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|")),1) = "]" THEN                
                ASSIGN bExtent     = YES
                       cExtraField = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|")
                       cExtraFormat = ""
                       cExtraLabel  = ""
                       .
              ELSE cExtraField  = SUBSTR(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),2).

              IF TRIM(cExtraField) BEGINS "ACCUM " THEN 
                cExtraField  = TRIM(SUBSTR(cExtraField,INDEX(cExtraField,"accum ") + 6)).
              ELSE IF TRIM(cExtraField) BEGINS "DISTINCT " THEN
                cExtraField = TRIM(SUBSTR(cExtraField,INDEX(cExtraField,"distinct ") + 8)).
            END.
            WHEN 2 THEN DO:
              IF bExtent AND NOT cExtraField BEGINS "+" THEN 
                cExtraLabel = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
              ELSE                  
                cExtraDataType = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
            END.
            WHEN 3 THEN cExtraFormat   = REPLACE(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),"<",",").
            WHEN 5 THEN cExtraLabel    = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
          END CASE.
        END.
        iz = iz - 1.
        IF iz < 2 AND NOT bExtent THEN DO:
          ocReturn = "Missing data type for extra field " + cExtraField.
          NEXT.
        END.
        IF NOT bExtent THEN CASE iz:
            WHEN 2 THEN ohTempTable:ADD-NEW-FIELD(cExtraField,cExtraDataType).
            WHEN 3 THEN ohTempTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat).
            WHEN 4 THEN ohTempTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat).
            WHEN 5 THEN 
              ohTempTable:ADD-NEW-FIELD(cExtraField,
                                        cExtraDataType,0,
                                        cExtraFormat,
                                        IF cExtraDataType = "CHARACTER" THEN ""
                                        ELSE IF cExtraDataType BEGINS "DATE" THEN "?"
                                        ELSE IF cExtraDataType = "LOGICAL" THEN "no"
                                        ELSE "0",
                                        cExtraLabel,
                                        IF cExtraLabel MATCHES "*" + CHR(10) + "*" THEN
                                          REPLACE(cExtraLabel,CHR(10),"!")
                                        ELSE ?).
          END CASE.
        ELSE DO:
          IF NOT cExtraField BEGINS "+" THEN DO:
            FIND FIRST tt_field 
                 WHERE tt_field.Table-Name = cBufferName
                   AND tt_field.Field-Name = SUBSTR(cExtraField,1,R-INDEX(cExtraField,"[") - 1)
                   AND (IF cDbName NE "" THEN tt_field.Db-name = cDbName ELSE TRUE)
                 NO-ERROR.
            
            IF NOT AVAIL tt_field THEN DO:
              ocReturn = "Field " + cExtraField + CHR(10) + 
                         "doesn't exist in buffer " + cBufferName.
              RETURN.
            END.
            cExtraDataType = tt_field.Field-Type.
            IF cExtraFormat = "" THEN
              cExtraFormat = tt_field.Field-format.
            IF cExtraLabel = "" THEN
              cExtraLabel = tt_field.Field-label.
          END.

          ocReturn = CreateExtentCalcField(ohTempTable,cExtraField,cExtraDataType,cExtraFormat,cExtraLabel).
          IF ocReturn NE "" THEN RETURN.
        END.
        cExtraInit = "".        
      END. /* Calculated field */

      /* Database buffer field: */
      ELSE DO:
        cField = ENTRY(1,ENTRY(iy,cBufferDef,";"),"|").

        IF cField BEGINS "accum " THEN
          cField = TRIM(SUBSTR(cField,7)).
        ELSE IF cField BEGINS "distinct " THEN 
          cField = TRIM(SUBSTR(cField,10)).

        FIND FIRST tt_field 
             WHERE tt_field.Table-Name = cBufferName
               AND tt_field.Field-Name = cField
               AND (IF cDbName NE "" THEN tt_field.Db-name = cDbName ELSE TRUE)
             NO-ERROR.

        IF NOT AVAIL tt_field THEN DO:
          ocReturn = "Field " + ENTRY(1,ENTRY(iy,cBufferDef,";"),"|") + CHR(10) + 
                     "doesn't exist in buffer " + cBufferName.
          NEXT.
        END.

        IF NOT CAN-DO(cAddedFields,cField) THEN DO:
          /* Check for overrides of format and label: */
          IF NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|") > 1 THEN DO:
            DO iz = 2 TO NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|"):
              IF iz = 2 THEN DO:
                IF ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|") NE "=" THEN 
                  cOverrideLabel = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
                ELSE 
                  cOverrideLabel = tt_field.Field-Label.
              END.
              ELSE IF iz = 3 THEN
                cOverrideFormat = REPLACE(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),"<",",").
            END.
            IF iz = 3 THEN /* No override on format */
              cOverrideFormat = tt_field.Field-format.

            ohTempTable:ADD-NEW-FIELD(tt_field.Field-name,tt_field.Field-Type,0,cOverrideFormat,
                                     tt_field.Field-Initial,
                                     cOverrideLabel,
                                     IF cOverrideLabel MATCHES "*" + CHR(10) + "*" THEN
                                       REPLACE(cOverrideLabel,CHR(10),"!")
                                     ELSE ?)
                        NO-ERROR.
            IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
              ocReturn = "Error in create of field " + ENTRY(1,ENTRY(iy,cBufferDef,";"),"|") + CHR(10) + 
                         ERROR-STATUS:GET-MESSAGE(1).
              NEXT.
            END.

          END.
          ELSE DO: 
            ohTempTable:ADD-NEW-FIELD(tt_field.Field-name,tt_field.Field-Type,0,tt_field.Field-Format,
                                      tt_field.Field-Initial,
                                      tt_field.Field-Label,
                                      ?)
                        NO-ERROR.
            IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
              ocReturn = "Error in create of field " + ENTRY(1,ENTRY(iy,cBufferDef,";"),"|") + CHR(10) + 
                         ERROR-STATUS:GET-MESSAGE(1).
              NEXT.
            END.
          END.

          cAddedFields = cAddedFields + tt_field.Field-name + ",".

        END.
        /* The field name already exists in the temp-table buffer. Add a suffix to make it unique: */
        ELSE DO:
          /* Check for overrides of format and label also here: */
          IF NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|") > 1 THEN DO:
            DO iz = 2 TO NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|"):
              IF iz = 2 THEN DO:
                IF ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|") NE "=" THEN 
                  cOverrideLabel = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
                ELSE 
                  cOverrideLabel = tt_field.Field-Label.
              END.
              ELSE IF iz = 3 THEN
                cOverrideFormat = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
            END.
            IF iz = 3 THEN
              cOverrideFormat = tt_field.Field-format. /* No override on format */

            ohTempTable:ADD-NEW-FIELD(tt_field.Field-Name + STRING(ix),tt_field.Field-Type,0,cOverrideFormat,
                                      tt_field.Field-Initial,
                                      cOverrideLabel,
                                      IF cOverrideLabel MATCHES "*" + CHR(10) + "*" THEN
                                        REPLACE(cOverrideLabel,CHR(10),"!")
                                      ELSE ?).
          END.
          ELSE DO:
            ohTempTable:ADD-NEW-FIELD(tt_field.Field-name + STRING(ix),tt_field.Field-Type,0,tt_field.Field-Format,
                                      tt_field.Field-Initial,
                                      tt_field.Field-Label,
                                      ?)
                        NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
              ocReturn = "Error in create of field " + ENTRY(1,ENTRY(iy,cBufferDef,";"),"|") + CHR(10) + 
                         ERROR-STATUS:GET-MESSAGE(1).
              NEXT.
            END.
          END.

          cDuplicateFields[ix] = cDuplicateFields[ix] + tt_field.Field-Name + ",".
        END. /* Duplicate field */

      END. /* Database buffer field */
    END.

  END. /* Field list for buffer */

  /* No field list is given (except calculated fields) and we grab all fields from the buffer and add to the temp-table  */
  IF ((ix = 1 OR bFieldSpec = NO) AND NUM-ENTRIES(cBufferDef,";") = 1) OR (bOnlyCalcFieldsDef AND NOT bFieldSpec) THEN DO:
/*   ELSE IF (ix = 1 OR bFieldSpec = NO) AND NUM-ENTRIES(cBufferDef,";") = 1 THEN DO: */
    ASSIGN cRealFieldSpec   = cBufferDef + "|"
           cRealDbFieldSpec = cBufferDef + "|".

    FOR EACH tt_field NO-LOCK
        WHERE tt_field.Table-Name = ENTRY(1,cBufferDef,";")
          AND (IF cDbName NE "" THEN tt_field.Db-name = cDbName ELSE TRUE)
           BY Field-Order:

      /* Goo: To be able to get by RefreshRow method in _AS lib */
      cFieldName = tt_field.Field-Name.
      IF INDEX(cFieldName,"blob") > 0 OR INDEX(cFieldName,"clob") > 0 THEN NEXT.

      DO iz = 0 TO tt_field.Field-Extent:
        IF tt_field.Field-Extent > 0 AND iz = 0 THEN NEXT.

        IF tt_field.Field-Extent > 0 THEN cFieldName = tt_field.Field-Name + "[" + STRING(iz) + "]".

        cRealDbFieldSpec = cRealDbFieldSpec + tt_field.Field-Name + "|".
  
        IF tt_field.Field-Extent = 0 THEN DO:
          IF NOT CAN-DO(cAddedFields,tt_field.Field-Name) THEN DO:
            ohTempTable:ADD-NEW-FIELD(tt_field.Field-name,tt_field.Field-Type,0,tt_field.Field-Format,
                                      tt_field.Field-Initial,
                                      tt_field.Field-Label,
                                      ?).
            ASSIGN cAddedFields   = cAddedFields + tt_field.Field-Name + ","
                   cRealFieldSpec = cRealFieldSpec + tt_field.Field-Name + "|".
          END.
          ELSE DO:
            ohTempTable:ADD-NEW-FIELD(tt_field.Field-name + STRING(ix),tt_field.Field-Type,0,tt_field.Field-Format,
                                      tt_field.Field-Initial,
                                      tt_field.Field-Label,
                                     ?).
            ASSIGN cDuplicateFields[ix] = cDuplicateFields[ix] + tt_field.Field-Name + ","
                   cRealFieldSpec       = cRealFieldSpec + tt_field.Field-Name + STRING(ix) + "|".
          END.
        END.
        ELSE DO:
          ocReturn = CreateExtentCalcField(ohTempTable,tt_field.Field-Name,tt_field.Field-Type,tt_field.Field-Format,
                                           tt_field.Field-Label + "[" + STRING(iz) + "]").  
          IF ocReturn NE "" THEN RETURN.
          cRealFieldSpec = cRealFieldSpec + tt_field.Field-Name + "|".
        END.
      END.
    END.

    IF bReturnQueryInfo THEN
      cReturnOKmessage = cReturnOKmessage
                       + RIGHT-TRIM(",bufferfieldspec;" + cRealFieldSpec,"|")
                       + RIGHT-TRIM(",bufferdbfieldspec;" + cRealDbFieldSpec,"|")
                       .
  END.

  cDuplicateFields[ix] = TRIM(cDuplicateFields[ix],",").

  ohTempTable:ADD-NEW-FIELD("RowIdent" + STRING(ix),"CHARACTER").

  cBufferList = cBufferList + ENTRY(1,cBufferDef,";") + ",".
  
END.

ohTempTable:ADD-NEW-FIELD("RowCount","INTEGER").

ohTempTable:ADD-NEW-FIELD("jbCountDistinct","INTEGER",0,">>>,>>>,>>9",1).

ohTempTable:ADD-NEW-FIELD("jbAverage","DECIMAL",0,"->>>,>>>,>>9.99",0).

IF bIndexOnRowids THEN DO: /* Done when the browse or query is defined to speed up duplicate-check on client */
  ohTempTable:ADD-NEW-INDEX("idxRowids").
  DO ix = 1 TO iBufferCount:
    ohTempTable:ADD-INDEX-FIELD("idxRowids","RowIdent" + STRING(ix)).
  END.
END.

cBufferList       = TRIM(cBufferList,",").

IF INDEX(ENTRY(1,ENTRY(1,icBuffersAndFields),";"),".") > 0 THEN
  cTTname = ENTRY(2,ENTRY(1,ENTRY(1,icBuffersAndFields),";"),".").
ELSE
  cTTname = ENTRY(1,ENTRY(1,icBuffersAndFields),";").

ohTempTable:TEMP-TABLE-PREPARE(cTTname) NO-ERROR.
IF ERROR-STATUS:ERROR OR ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
  ocReturn = ERROR-STATUS:GET-MESSAGE(1).
  RETURN.
END.
httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE.

DO ix = 1 TO iBufferCount:
  iy = 0.
  FOR EACH tt_field NO-LOCK
      WHERE tt_field.Table-Name = ENTRY(ix,cBufferList):
      
    iy = iy + 1.

    hField = httBuffer:BUFFER-FIELD(tt_field.Field-Name + STRING(ix)) NO-ERROR.
    IF NOT VALID-HANDLE(hField) THEN
      hField = httBuffer:BUFFER-FIELD(tt_field.Field-Name) NO-ERROR.
    IF VALID-HANDLE(hField) THEN
      hField:HELP = tt_field.Field-Help.

    IF NOT CAN-DO(tt_field.Field-Can-Read,cASuserId) THEN 
      cNotCanReadFlds = cNotCanReadFlds + tt_field.Field-Name + ",".

    IF NOT CAN-DO(tt_field.Field-Can-Write,cASuserId) THEN 
      cNotCanWriteFlds = cNotCanWriteFlds + tt_field.Field-Name + ",".

    IF iy = 1 THEN DO:
      IF NOT CAN-DO(tt_field.Table-Can-Read,cASuserId) THEN 
        cNotCanReadBuffs = cNotCanReadBuffs + tt_field.Table-Name + ",".
      IF NOT CAN-DO(tt_field.Table-Can-Create,cASuserId) THEN 
        cNotCanCreateBuffs = cNotCanCreateBuffs + tt_field.Table-Name + ",".
      IF NOT CAN-DO(tt_field.Table-Can-Write,cASuserId) THEN 
        cNotCanWriteBuffs = cNotCanWriteBuffs + tt_field.Table-Name + ",".
      IF NOT CAN-DO(tt_field.Table-Can-Delete,cASuserId) THEN 
        cNotCanDeleteBuffs = cNotCanDeleteBuffs + tt_field.Table-Name + ",".
    END.
  END.
END.

ASSIGN cNotCanReadFlds    = RIGHT-TRIM(cNotCanReadFlds,",")
       cNotCanWriteFlds   = RIGHT-TRIM(cNotCanWriteFlds,",")
       cNotCanReadBuffs   = RIGHT-TRIM(cNotCanReadBuffs,",")
       cNotCanWriteBuffs  = RIGHT-TRIM(cNotCanWriteBuffs,",")
       cNotCanCreateBuffs = RIGHT-TRIM(cNotCanCreateBuffs,",")
       cNotCanDeleteBuffs = RIGHT-TRIM(cNotCanDeleteBuffs,",")
       cReturnOKmessage = cReturnOKmessage 
       + RIGHT-TRIM(",nodbreadaccessfields;"    + REPLACE(cNotCanReadFlds,",","|"),";")
       + RIGHT-TRIM(",nodbwriteaccessfields;"   + REPLACE(cNotCanWriteFlds,",","|"),";")
       + RIGHT-TRIM(",nodbreadaccessbuffers;"   + REPLACE(cNotCanReadBuffs,",","|"),";")
       + RIGHT-TRIM(",nodbwriteaccessbuffers;"  + REPLACE(cNotCanWriteBuffs,",","|"),";") 
       + RIGHT-TRIM(",nodbcreateaccessbuffers;" + REPLACE(cNotCanCreateBuffs,",","|"),";") 
       + RIGHT-TRIM(",nodbdeleteaccessbuffers;" + REPLACE(cNotCanDeleteBuffs,",","|"),";") 
       .

IF cNotCanReadFlds NE "" THEN
  ocReturn = "** Insufficient access privilege for Field " + cNotCanReadFlds + ". (233)".
IF cNotCanReadBuffs NE "" THEN
  ocReturn = (IF ocReturn NE "" THEN CHR(10) ELSE "") + "** Insufficient access privilege for Table " + cNotCanReadBuffs + ". (234)".

ocReturn = ocReturn + "¤" + TRIM(cReturnOKmessage,",").
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectAppLanguage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectAppLanguage Procedure 
PROCEDURE SelectAppLanguage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLangList AS CHAR NO-UNDO.
DEF VAR cReturn   AS CHAR NO-UNDO.
DEF VAR ix        AS INT  NO-UNDO.

IF NUM-ENTRIES(cAppLanguages,"|") > 1 THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cAppLanguages,"|"):
    cLangList = cLangList + (IF cLangList NE "" THEN "|" ELSE "") 
              + ENTRY(ix,cAppLanguages,"|") + "|" + ENTRY(ix,cAppLanguages,"|").
  END.
  RUN JBoxDSimpleSelectList.w (cLangList,?,OUTPUT cReturn).
  IF cReturn NE "" THEN
    setLanguageCode(cReturn).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-assignStringValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignStringValue Procedure 
FUNCTION assignStringValue RETURNS LOGICAL
  ( INPUT ihField       AS HANDLE,
    INPUT icUpdateValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bError AS LOG NO-UNDO.

CASE ihField:DATA-TYPE:
  WHEN "character" THEN
    ihField:BUFFER-VALUE = icUpdateValue NO-ERROR.
  WHEN "date" THEN
    ihField:BUFFER-VALUE = DATE(icUpdateValue) NO-ERROR.
  WHEN "decimal" THEN
    ihField:BUFFER-VALUE = DECIMAL(icUpdateValue) NO-ERROR.
  WHEN "integer" THEN
    ihField:BUFFER-VALUE = INTEGER(icUpdateValue) NO-ERROR.
  WHEN "logical" THEN
    ihField:BUFFER-VALUE = LOGICAL(icUpdateValue) NO-ERROR. /* (IF icUpdateValue = "yes" OR icUpdateValue = "true" THEN TRUE ELSE FALSE)  NO-ERROR. */
  OTHERWISE DO:
    IF SEARCH("assignStringValue.p") NE ? THEN
      RUN assignStringValue.p (ihField,icUpdateValue,OUTPUT bError).
    ELSE bError = YES.
  END.
END CASE.

IF ERROR-STATUS:ERROR THEN
  bError = YES.   
 
RETURN NOT bError.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildTableCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BuildTableCache Procedure 
FUNCTION BuildTableCache RETURNS LOGICAL
  ( INPUT icCacheList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Get local copies of database tables
           Param: <table pr serverprog>|<criteria>;<table or serverprog>|<criteria>..
    Notes: criteria and serverprogram are optional 
------------------------------------------------------------------------------*/
DEF VAR cTable      AS CHAR   NO-UNDO.
DEF VAR cCriteria   AS CHAR   NO-UNDO.
DEF VAR cServerProg AS CHAR   NO-UNDO.
DEF VAR httDummy    AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.

IF icCacheList = "" THEN RETURN NO.

DO ix = 1 TO NUM-ENTRIES(icCacheList,";"):
  cTable = "".

  IF ENTRY(1,ENTRY(ix,icCacheList,";"),"|") MATCHES "*.p" THEN
    cServerProg = ENTRY(1,ENTRY(ix,icCacheList,";"),"|").
  ELSE
    cTable = ENTRY(1,ENTRY(ix,icCacheList,";"),"|").
  IF NUM-ENTRIES(ENTRY(ix,icCacheList,";"),"|") > 1 THEN
    cCriteria = ENTRY(2,ENTRY(ix,icCacheList,";"),"|").
  ELSE 
    cCriteria = "WHERE true".

  IF cTable = "" AND cServerProg = "" THEN NEXT.

  IF cTable NE "" THEN
    FIND FIRST ttCache
         WHERE ttCache.cTableName = cTable 
         NO-ERROR.
  ELSE IF NOT CAN-DO("jbserv_getfieldcache.p",cServerProg) THEN
    FIND FIRST ttCache
         WHERE ttCache.cTableName = cServerProg 
         NO-ERROR.
  ELSE DO:
    IF cServerProg = "jbserv_getfieldcache.p" THEN DO:
      FIND FIRST tt_field NO-ERROR.
      httDummy = getTempTable(cServerProg,cCriteria,htt_field).

      FOR EACH ttDefaultTranslation
          WHERE ttDefaultTranslation.cLanguage = cLanguage
            AND ttDefaultTranslation.cFieldLabel NE ""
          :

        FOR EACH tt_field 
            WHERE tt_field.Field-Name = ttDefaultTranslation.cFieldName
            :
          ASSIGN tt_field.Field-Label = ttDefaultTranslation.cFieldLabel
                 tt_field.Field-Help  = ttDefaultTranslation.cFieldHelp
                 .
        END.
      END.
/*       OUTPUT TO c:\temp\datadict.csv.   */
/*       FOR EACH tt_field:                */
/*         EXPORT DELIMITER ";" tt_field.  */
/*       END.                              */
/*       OUTPUT CLOSE.                     */
    END.
    NEXT.
  END.

  IF AVAIL ttCache THEN DO:
    IF VALID-HANDLE(ttCache.hBuffer) THEN
      ttCache.hBuffer:EMPTY-TEMP-TABLE().
  END.
  ELSE DO:
    CREATE ttCache.
    ASSIGN ttCache.cTableName = ENTRY(1,ENTRY(ix,icCacheList,";"),"|").
  END.
  ttCache.hTempTable = getTempTable(cServerProg,cTable + "|" + cCriteria,?).
  IF VALID-HANDLE(ttCache.hTempTable) THEN DO:
    ttCache.hBuffer = ttCache.hTempTable:DEFAULT-BUFFER-HANDLE.
    IF ttCache.hBuffer:NAME = "JBoxTranslation" THEN
      ASSIGN hBuffTranslation   = ttCache.hBuffer
             ttCache.cTableName = ttCache.hBuffer:NAME.
  END.
  ELSE DO:
    IF cQueryLogFile NE "" AND NOT cTable = "JBoxUserSetting" THEN
      MESSAGE "Failed to retrieve cache for table " + cTable SKIP
              "Criteria: " cCriteria SKIP(1)
              cReturnTrans
              VIEW-AS ALERT-BOX ERROR.
    DELETE ttCache.
  END.
END.

RETURN YES.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckReturnValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckReturnValue Procedure 
FUNCTION CheckReturnValue RETURNS LOGICAL
  ( INPUT icReturnTrans AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icReturnTrans BEGINS "Invalid session" THEN DO:
  MESSAGE icReturnTrans VIEW-AS ALERT-BOX.
  QUIT.
END.
ELSE IF NUM-ENTRIES(icReturnTrans,"¤") > 1 THEN DO:
  cServerReturnParam = ENTRY(2,icReturnTrans,"¤").
  DO ix = 1 TO NUM-ENTRIES(cServerReturnParam):
    IF NUM-ENTRIES(ENTRY(ix,cServerReturnParam),";") > 1 THEN
      CASE ENTRY(1,ENTRY(ix,cServerReturnParam),";"):
        WHEN "nodbreadaccessfields" THEN
          cNoDbReadAccessFields = REPLACE(ENTRY(2,ENTRY(ix,cServerReturnParam),";"),"|",",").
        WHEN "nodbwriteaccessfields" THEN
          cNoDbWriteAccessFields = REPLACE(ENTRY(2,ENTRY(ix,cServerReturnParam),";"),"|",",").
        WHEN "nodbreadaccessbuffers" THEN
          cNoDbReadAccessBuffers = REPLACE(ENTRY(2,ENTRY(ix,cServerReturnParam),";"),"|",",").
        WHEN "nodbwriteaccessbuffers" THEN
          cNoDbWriteAccessBuffers = REPLACE(ENTRY(2,ENTRY(ix,cServerReturnParam),";"),"|",",").
        WHEN "nodbcreateaccessbuffers" THEN
          cNoDbCreateAccessBuffers = REPLACE(ENTRY(2,ENTRY(ix,cServerReturnParam),";"),"|",",").
        WHEN "nodbdeleteaccessbuffers" THEN
          cNoDbDeleteAccessBuffers = REPLACE(ENTRY(2,ENTRY(ix,cServerReturnParam),";"),"|",",").
        WHEN "bufferfieldspec" THEN
          cBufferFieldSpec = ENTRY(2,ENTRY(ix,cServerReturnParam),";").
        WHEN "bufferdbfieldspec" THEN
          cBufferDbFieldSpec = ENTRY(2,ENTRY(ix,cServerReturnParam),";").
      END CASE.
  END.
  cReturnTrans = ENTRY(1,cReturnTrans,"¤").
END.
ELSE cServerReturnParam = "".

RETURN FALSE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ClearTableCache) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ClearTableCache Procedure 
FUNCTION ClearTableCache RETURNS LOGICAL
  ( INPUT icCacheList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Clear local copies of database tables
           Param: <table>|<criteria>;<table>|<criteria>..
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTable    AS CHAR NO-UNDO.
DEF VAR cCriteria AS CHAR NO-UNDO.

IF icCacheList = "*" THEN
  FOR EACH ttCache:
    IF VALID-HANDLE(ttCache.hBuffer) THEN
      ttCache.hBuffer:EMPTY-TEMP-TABLE().
    DELETE ttCache.
  END.

ELSE DO ix = 1 TO NUM-ENTRIES(icCacheList,";"):
  cTable = ENTRY(1,ENTRY(ix,icCacheList,";"),"|").
  FIND FIRST ttCache
       WHERE ttCache.cTableName = cTable 
       NO-ERROR.
  IF AVAIL ttCache THEN DO:
    IF VALID-HANDLE(ttCache.hBuffer) THEN
      ttCache.hBuffer:EMPTY-TEMP-TABLE().
    DELETE ttCache.
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateDataSet Procedure 
FUNCTION CreateDataSet RETURNS HANDLE
  ( INPUT icBufferHandleList AS CHAR,
    INPUT icRelationFields   AS CHAR,
    INPUT icSourceKeyList    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE hDataSet    AS HANDLE  NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER NO-UNDO.
DEFINE VARIABLE hDataSource AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBuffer     AS HANDLE  NO-UNDO EXTENT 10.
DEFINE VARIABLE hQuery      AS HANDLE  NO-UNDO.

CREATE DATASET hDataSet.
DO ix = 1 TO NUM-ENTRIES(icBufferHandleList):
  hBuffer[ix] = WIDGET-HANDLE(ENTRY(ix, icBufferHandleList)).
  hDataSet:ADD-BUFFER(hBuffer[ix]).
END.
  
DO ix = 2 TO NUM-ENTRIES(icRelationFields):
  hDataSet:ADD-RELATION(hDataSet:GET-BUFFER-HANDLE(1),
  hDataSet:GET-BUFFER-HANDLE(ix),
  ENTRY(1,icRelationFields) + "," + ENTRY(ix,icRelationFields)).
END.


RETURN hDataSet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateExtentCalcField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateExtentCalcField Procedure 
FUNCTION CreateExtentCalcField RETURNS CHARACTER
  ( INPUT ohTempTable  AS HANDLE,
    INPUT icFieldName  AS CHAR,
    INPUT icDataType   AS CHAR,
    INPUT icFormat     AS CHAR,
    INPUT icLabel      AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField  AS HANDLE NO-UNDO.
DEF VAR iExtent AS INT    NO-UNDO.

iExtent = INT(SUBSTR(icFieldName,R-INDEX(icFieldName,"[") + 1,R-INDEX(icFieldName,"]") - R-INDEX(icFieldName,"[") - 1)) NO-ERROR.

IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN RETURN "Invalid definition of extent field: " + icFieldName + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
IF NOT icFieldName BEGINS "+" THEN DO:
  ohTempTable:ADD-NEW-FIELD(icFieldName,
                            icDataType,0,
                            icFormat,
                            "",
                            icLabel,
                            IF icLabel MATCHES "*" + CHR(10) + "*" THEN
                              REPLACE(icLabel,CHR(10),"!")
                            ELSE ?) NO-ERROR.

  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN RETURN "Error when creating output tt field for extent field: " + icFieldName + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).

  ohTempTable:ADD-NEW-FIELD("jbextent_" + STRING(iExtent) + "_" + SUBSTR(icFieldName,1,R-INDEX(icFieldName,"[") - 1),icDataType).
END.
ELSE DO:
  icFieldName = SUBSTR(icFieldName,2,INDEX(icFieldName,"[") - 2).

  ohTempTable:ADD-NEW-FIELD(icFieldName,
                            icDataType,iExtent,
                            icFormat,
                            "",
                            icLabel,
                            IF icLabel MATCHES "*" + CHR(10) + "*" THEN
                              REPLACE(icLabel,CHR(10),"!")
                            ELSE ?) NO-ERROR.


  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN RETURN "Error when creating output tt field for extent field: " + icFieldName + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
END.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deleteRows Procedure 
FUNCTION deleteRows RETURNS CHARACTER
  ( INPUT cBufferName AS CHAR,
    INPUT cRowIdList  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Unconditional delete of comma-separated list of rowid's.
    Notes:  Use with care!
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.

IF getAppServiceHandle() NE ? THEN
  RUN deleterows.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      cBufferName,
      cRowIdList,
      OUTPUT cReturn)
      .
ELSE 
  RUN deleterows.p
     (DYNAMIC-FUNCTION("getSessionId"),
      cBufferName,
      cRowIdList,
      OUTPUT cReturn)
      .

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCommit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoCommit Procedure 
FUNCTION DoCommit RETURNS LOGICAL
  ( INPUT ibReturnAll AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: If ibReturnAll = TRUE all transaction records will be returned (with completion) from AppServer.
           When false only the 1st transaction row is returned
------------------------------------------------------------------------------*/
DEF VAR obOK AS LOG NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_servertrans.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      ibReturnAll,
      cServerTransInputParam,
      TABLE-HANDLE httTransAction,
      INPUT-OUTPUT TABLE-HANDLE httTransRecord,
      OUTPUT cReturnTrans,
      OUTPUT obOK,
      cTransLogFile)
      NO-ERROR.

  IF ERROR-STATUS:ERROR AND ERROR-STATUS:GET-NUMBER(1) = 3230 THEN 
    RUN jbserv_servertrans.p
        ON getAppServiceHandle()
       (DYNAMIC-FUNCTION("getSessionId"),
        ibReturnAll,
        TABLE-HANDLE httTransAction,
        INPUT-OUTPUT TABLE-HANDLE httTransRecord,
        OUTPUT cReturnTrans,
        OUTPUT obOK,
        cTransLogFile)
        NO-ERROR.

  IF ERROR-STATUS:ERROR THEN
    IF ReconnectServer() THEN 
      obOK = DoCommit (ibReturnAll).
END.
ELSE
  RUN jbserv_servertrans.p
     (DYNAMIC-FUNCTION("getSessionId"),
      ibReturnAll,
      cServerTransInputParam,
      TABLE-HANDLE httTransAction,
      INPUT-OUTPUT TABLE-HANDLE httTransRecord,
      OUTPUT cReturnTrans,
      OUTPUT obOK,
      cTransLogFile).

ASSIGN ixTransActionId        = 0
       ixTransRecordId        = 0
       cServerTransInputParam = "".

bOK = SESSION:SET-WAIT-STATE("").

IF NUM-ENTRIES(cReturnTrans,"¤") > 1 THEN
  cServerTransReturnParam = ENTRY(2,cReturnTrans,"¤").
ELSE
  cServerTransReturnParam = "".

CheckReturnValue(cReturnTrans).

RETURN obOK.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoCreate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoCreate Procedure 
FUNCTION DoCreate RETURNS LOGICAL
  ( INPUT icBuffer       AS CHAR,
    INPUT icInstructions AS CHAR,  /* References to hooks for create, field validation and post update */
    INPUT icValueFields  AS CHAR,
    INPUT icValues       AS CHAR,
    INPUT ibCommit       AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  Prepare transaction to be sent to appserver
    Notes:  
------------------------------------------------------------------------------*/
IF ixTransRecordId = 0 THEN DO:
  EMPTY TEMP-TABLE ttTransAction.
  EMPTY TEMP-TABLE ttTransRecord.
END.

IF NUM-ENTRIES(icValues,"|") = NUM-ENTRIES(icValueFields) THEN
  icValues = REPLACE(icValues,"|",CHR(1)).
ELSE IF INDEX(icValues,"|") > 0 THEN DO:
  MESSAGE "The data values contain the | (pipe) character but | is also used as a delimiter for data" SKIP 
          "This is an invalid situtation and the transaction will not be committed" SKIP(1) 
          "The cause of this error could also be that the number of data field names don't match the number of data values"
          VIEW-AS ALERT-BOX ERROR.
  ixTransRecordId = 0.
  RETURN NO.
END.

FIND FIRST ttTransAction
     WHERE ttTransAction.cTransActionBuffer      = icBuffer
       AND ttTransAction.cTransAction            = "create"
     NO-ERROR.
IF NOT AVAIL ttTransAction THEN DO:
  ixTransActionId = ixTransActionId + 1.
  CREATE ttTransAction.
  ASSIGN ttTransAction.iTransActionId          = ixTransActionId
         ttTransAction.cTransActionBuffer      = icBuffer
         ttTransAction.cTransAction            = "create"
         ttTransAction.cTransActionValueFields = icValueFields
         ttTransAction.cTransActionPostUpdProc = cPostUpdProc
         .
  IF NUM-ENTRIES(icInstructions) > 1 AND ENTRY(2,icInstructions) NE "" THEN DO:
    IF CAN-DO("+,=",SUBSTR(ENTRY(1,icInstructions),1,1)) OR ENTRY(1,icInstructions) = "NO" THEN
      ttTransAction.cTransActionValidation = ENTRY(1,icInstructions) + "," + 
                                             IF ENTRY(2,icInstructions) BEGINS "=" THEN 
                                               ENTRY(2,icInstructions) 
                                             ELSE "=" + ENTRY(2,icInstructions).
    ELSE 
      ASSIGN ttTransAction.cTransActionErrorHandl = ENTRY(1,icInstructions)
             ttTransAction.cTransActionValidation = "," + 
                                                    IF ENTRY(2,icInstructions) BEGINS "=" THEN 
                                                      ENTRY(2,icInstructions) 
                                                    ELSE "=" + ENTRY(2,icInstructions).
  END.
  ELSE DO:
    IF CAN-DO("+,=",SUBSTR(icInstructions,1,1)) OR icInstructions = "NO" THEN
      ttTransAction.cTransActionValidation = icInstructions.
    ELSE
      ttTransAction.cTransActionErrorHandl  = icInstructions.
  END.

  IF NUM-ENTRIES(icInstructions) > 2 THEN 
    ttTransAction.cTransActionPostUpdProc = ENTRY(3,icInstructions).

END.

CREATE ttTransRecord.
ASSIGN ixTransRecordId                    = ixTransRecordId + 1
       ttTransRecord.iTransActionId       = ttTransAction.iTransActionId
       ttTransRecord.iTransRecordId       = ixTransRecordId
       ttTransRecord.cTransRecordValues   = icValues
       cPostUpdProc                       = ""
       .

IF ibCommit THEN
  RETURN DoCommit(TRUE).
ELSE RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoDelete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoDelete Procedure 
FUNCTION DoDelete RETURNS LOGICAL
  ( INPUT icBuffer       AS CHAR,
    INPUT icInstructions AS CHAR,  /* Directions delete validate and post delete */
    INPUT icIdFldsOrCrit AS CHAR,  /* Comma sep. field names for UNIQUE match or blank when icIdValues contains a ROWID */
    INPUT icIdValues     AS CHAR,  /* Acutal ROWID or CHR(1)-delimited) value(s) for corresp id fields */ 
    INPUT ibCommit       AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  Prepare transaction to be sent to appserver
    Notes:  
------------------------------------------------------------------------------*/
IF ixTransRecordId = 0 THEN DO:
  EMPTY TEMP-TABLE ttTransAction.
  EMPTY TEMP-TABLE ttTransRecord.
END.

IF icIdFldsOrCrit NE "" AND icIdValues NE "" THEN DO:
  IF NUM-ENTRIES(icIdValues,"|") = NUM-ENTRIES(icIdFldsOrCrit) THEN
    icIdValues = REPLACE(icIdValues,"|",CHR(1)).
  ELSE IF INDEX(icIdValues,"|") > 0 THEN DO:
    MESSAGE "The identification values contain the | (pipe) character but | is also used as a delimiter for data" SKIP 
            "This is an invalid situtation and the transaction will not be committed" SKIP(1)
            "The cause of this error could also be that the number of data field names don't match the number of data values"
            VIEW-AS ALERT-BOX ERROR.
    ixTransRecordId = 0.
    RETURN NO.
  END.
END. 

FIND FIRST ttTransAction
     WHERE ttTransAction.cTransActionBuffer    = icBuffer
       AND ttTransAction.cTransAction          = "delete"
     NO-ERROR.
IF NOT AVAIL ttTransAction THEN DO:
  ixTransActionId = ixTransActionId + 1.
  CREATE ttTransAction.
  ASSIGN ttTransAction.iTransActionId          = ixTransActionId
         ttTransAction.cTransActionBuffer      = icBuffer
         ttTransAction.cTransAction            = "delete"
         ttTransAction.cTransActionCriteria    = 
/*                                                  IF icIdFldsOrCrit BEGINS "WHERE" THEN icIdFldsOrCrit ELSE */
                                                 IF icIdFldsOrCrit = "" THEN "ROWID"
                                                 ELSE "UNIQUE"
         ttTransAction.cTransActionIdFields    = icIdFldsOrCrit
         ttTransAction.cTransActionPostUpdProc = cPostUpdProc
         .
  IF CAN-DO("+,=",SUBSTR(icInstructions,1,1)) THEN
    cTransActionValidation = ENTRY(1,icInstructions).
  ELSE 
    ttTransAction.cTransActionErrorHandl  = ENTRY(1,icInstructions).

  IF NUM-ENTRIES(icInstructions) > 1 THEN 
    ttTransAction.cTransActionPostUpdProc = ENTRY(2,icInstructions).
END.

ixTransRecordId = ixTransRecordId + 1.
CREATE ttTransRecord.
ASSIGN ttTransRecord.iTransActionId       = ttTransAction.iTransActionId
       ttTransRecord.iTransRecordId       = ixTransRecordId
       ttTransRecord.cTransRecordIdValue  = IF icIdFldsOrCrit NE "" THEN icIdValues ELSE ""
       ttTransRecord.cTransRecordRowid    = IF icIdFldsOrCrit = "" THEN icIdValues ELSE ""
       cPostUpdProc                       = ""
       .

IF ibCommit THEN
  RETURN DoCommit(FALSE). 
ELSE RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoRefetchTrans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoRefetchTrans Procedure 
FUNCTION DoRefetchTrans RETURNS LOGICAL
  ( INPUT ihBuffer        AS HANDLE,
    INPUT icFieldsToMatch AS CHAR,
    INPUT icValuesToMatch AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch returned record from update transaction and refresh buffer
    Notes: Fields and values to match are optional - needed when more than on receipt record pr buffer/action.
           icFieldsToMatch can be RowIdent* with corresponding value.
           Fields should be delimited by comma, values by |.
           NB! Only the first rowid (RowIdent1) will be updated. If the buffer is a joined set use the "refreshQueryRow" method (in addition)
------------------------------------------------------------------------------*/
DEF VAR inx          AS INT NO-UNDO.
DEF VAR bUseRecord   AS LOG NO-UNDO.
DEF VAR hField       AS HANDLE NO-UNDO.
DEF VAR cUpdateValue AS CHAR NO-UNDO.

FOR EACH ttTransRecord
    WHERE (IF icFieldsToMatch MATCHES "RowIdent*" THEN
             ttTransRecord.cTransRecordRowid = STRING(ihBuffer:BUFFER-FIELD(icFieldsToMatch):BUFFER-VALUE)
           ELSE TRUE),
    FIRST ttTransAction 
          WHERE ttTransAction.iTransActionId = ttTransRecord.iTransActionId
            AND (IF icFieldsToMatch MATCHES "RowIdent*" THEN 
                   TRUE 
                 ELSE ttTransAction.cTransActionBuffer = ihBuffer:NAME 
                      AND ttTransAction.cTransAction NE "delete")
    :
  
  bUseRecord = TRUE.
  IF icFieldsToMatch NE "" AND NOT icFieldsToMatch MATCHES "RowIdent*" AND NOT icFieldsToMatch = "FIRST" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(icFieldsToMatch):
      IF CAN-DO(cTransActionIdFields,ENTRY(ix,icFieldsToMatch)) THEN DO:
        IF ENTRY(LOOKUP(ENTRY(ix,icFieldsToMatch),ttTransAction.cTransActionIdFields),
                 ttTransRecord.cTransRecordIdValues,CHR(1)) NE ENTRY(ix,icValuesToMatch,"|") THEN
          bUseRecord = FALSE.
      END.
      ELSE DO:
        IF ENTRY(LOOKUP(ENTRY(ix,icFieldsToMatch),ttTransAction.cTransActionValueFields),
                 ttTransRecord.cTransRecordValues,CHR(1)) NE ENTRY(ix,icValuesToMatch,"|") THEN
          bUseRecord = FALSE.
      END.
    END.
  END.
  IF bUseRecord THEN DO:
    DO ix = 1 TO ihBuffer:NUM-FIELDS:
      hField = ihBuffer:BUFFER-FIELD(ix).

      IF NOT VALID-HANDLE(hField) THEN NEXT. /* Might be if "BufferExtraField is used */

      inx = LOOKUP(hField:NAME,ttTransAction.cTransActionValueFields).
      IF inx NE 0 THEN 
        cUpdateValue = ENTRY(inx,ttTransRecord.cTransRecordValues,CHR(1)).
      ELSE DO:
        inx = LOOKUP(hField:NAME,ttTransRecord.cReturnExtraFields).
        IF inx NE 0 THEN 
          cUpdateValue = ENTRY(inx,ttTransRecord.cReturnExtraValues,"|").
      END.
      IF inx NE 0 THEN
        bOk = assignStringValue(hField,cUpdateValue).
      ELSE IF hField:NAME = "RowIdent1" THEN 
        hField:BUFFER-VALUE = ttTransRecord.cTransRecordRowid NO-ERROR.
    END.
  END. 
  IF icFieldsToMatch = "FIRST" THEN LEAVE.
END.
IF bUseRecord THEN
  RETURN TRUE.
ELSE
  RETURN FALSE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoRefetchValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoRefetchValues Procedure
FUNCTION DoRefetchValues RETURNS CHARACTER 
  ( INPUT icFieldList     AS CHAR,
    INPUT icFieldsToMatch AS CHAR,
    INPUT icValuesToMatch AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch returned values from update transaction and refresh buffer
    Notes: Fields and values to match are optional - needed when more than on receipt record pr buffer/action.
           icFieldsToMatch can be RowIdent* with corresponding value.
           Fields should be delimited by comma, values by |.
           NB! Only the first rowid (RowIdent1) will be updated. If the buffer is a joined set use the "refreshQueryRow" method (in addition)
------------------------------------------------------------------------------*/
DEF VAR inx          AS INT NO-UNDO.
DEF VAR bUseRecord   AS LOG NO-UNDO.
DEF VAR hField       AS HANDLE NO-UNDO.
DEF VAR cUpdateValue AS CHAR NO-UNDO.
DEF VAR cReturn      AS CHAR NO-UNDO.

FOR EACH ttTransRecord
    WHERE (IF icFieldsToMatch MATCHES "RowIdent*" THEN
             ttTransRecord.cTransRecordRowid = icFieldsToMatch
           ELSE TRUE),
    FIRST ttTransAction 
          WHERE ttTransAction.iTransActionId = ttTransRecord.iTransActionId
    :
  
  bUseRecord = TRUE.
  IF icFieldsToMatch NE "" AND NOT icFieldsToMatch MATCHES "RowIdent*" AND NOT icFieldsToMatch = "FIRST" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(icFieldsToMatch):
      IF CAN-DO(cTransActionIdFields,ENTRY(ix,icFieldsToMatch)) THEN DO:
        IF ENTRY(LOOKUP(ENTRY(ix,icFieldsToMatch),ttTransAction.cTransActionIdFields),
                 ttTransRecord.cTransRecordIdValues,CHR(1)) NE ENTRY(ix,icValuesToMatch,"|") THEN
          bUseRecord = FALSE.
      END.
      ELSE DO:
        IF ENTRY(LOOKUP(ENTRY(ix,icFieldsToMatch),ttTransAction.cTransActionValueFields),
                 ttTransRecord.cTransRecordValues,CHR(1)) NE ENTRY(ix,icValuesToMatch,"|") THEN
          bUseRecord = FALSE.
      END.
    END.
  END.
  IF bUseRecord THEN DO:
    DO ix = 1 TO NUM-ENTRIES(icFieldList):

      inx = LOOKUP(ENTRY(ix,icFieldList),ttTransAction.cTransActionValueFields).
      IF inx NE 0 THEN 
        cUpdateValue = ENTRY(inx,ttTransRecord.cTransRecordValues,CHR(1)).
      ELSE DO:
        inx = LOOKUP(ENTRY(ix,icFieldList),ttTransRecord.cReturnExtraFields).
        IF inx NE 0 THEN 
          cUpdateValue = ENTRY(inx,ttTransRecord.cReturnExtraValues,"|").
        ELSE IF ENTRY(ix,icFieldList) = "RowIdent1" THEN
          cUpdateValue = ttTransRecord.cTransRecordRowid.   
      END.
      cReturn = cReturn + (IF cReturn NE "" THEN "|" ELSE "") + cUpdateValue.
    END.
  END. 
  IF icFieldsToMatch = "FIRST" THEN LEAVE.
END.

RETURN cReturn.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-DoUpdate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoUpdate Procedure 
FUNCTION DoUpdate RETURNS LOGICAL
  ( INPUT icBuffer       AS CHAR,
    INPUT icInstructions AS CHAR,  /* References to hooks for field validation and post update */
    INPUT icIdFldsOrCrit AS CHAR,  /* Comma sep. field names for UNIQUE match or blank when icIdValues contains a ROWID */ 
    INPUT icIdValues     AS CHAR,  /* Acutal ROWID or CHR(1)-delimited) value(s) for corresp id fields */                  
    INPUT icValueFields  AS CHAR,
    INPUT icValues       AS CHAR,
    INPUT ibCommit       AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  Prepare transaction to be sent to appserver
    Notes:  
------------------------------------------------------------------------------*/
IF ixTransRecordId = 0 THEN DO:
  EMPTY TEMP-TABLE ttTransAction.
  EMPTY TEMP-TABLE ttTransRecord.
END.

IF icIdFldsOrCrit NE "" AND icIdValues NE "" THEN DO:
  IF NUM-ENTRIES(icIdValues,"|") = NUM-ENTRIES(icIdFldsOrCrit) THEN
    icIdValues = REPLACE(icIdValues,"|",CHR(1)).
  ELSE IF INDEX(icIdValues,"|") > 0 THEN DO:
    MESSAGE "The identification values contain the | (pipe) character but | is also used as a delimiter for data" SKIP 
            "This is an invalid situtation and the transaction will not be committed" SKIP(1)
            "The cause of this error could also be that the number of data field names don't match the number of data values"
            VIEW-AS ALERT-BOX ERROR.
    ixTransRecordId = 0.
    RETURN NO.
  END.
END. 

IF NUM-ENTRIES(icValues,"|") = NUM-ENTRIES(icValueFields) THEN
  icValues = REPLACE(icValues,"|",CHR(1)).
ELSE IF INDEX(icValues,"|") > 0 THEN DO:
  MESSAGE "The data values contain the | (pipe) character but | is also used as a delimiter for data" SKIP 
          "This is an invalid situtation and the transaction will not be committed" SKIP(1) 
          "The cause of this error could also be that the number of data field names don't match the number of data values"
          VIEW-AS ALERT-BOX ERROR.
  ixTransRecordId = 0.
  RETURN NO.
END.


FIND FIRST ttTransAction
     WHERE ttTransAction.cTransActionBuffer      = icBuffer
       AND ttTransAction.cTransAction            = "update"
     NO-ERROR.
IF NOT AVAIL ttTransAction THEN DO:
  ixTransActionId = ixTransActionId + 1.
  CREATE ttTransAction.
  ASSIGN ttTransAction.iTransActionId          = ixTransActionId
         ttTransAction.cTransActionBuffer      = icBuffer
         ttTransAction.cTransAction            = "update"
         ttTransAction.cTransActionCriteria    = 
/*                                                  IF icIdFldsOrCrit BEGINS "WHERE" THEN icIdFldsOrCrit ELSE */
                                                 IF icIdFldsOrCrit = "" THEN "ROWID"
                                                 ELSE "UNIQUE"
         ttTransAction.cTransActionIdFields    = icIdFldsOrCrit
         ttTransAction.cTransActionValueFields = icValueFields
         ttTransAction.cTransActionPostUpdProc = cPostUpdProc
         .
   IF CAN-DO("+,=",SUBSTR(icInstructions,1,1)) THEN
     cTransActionValidation = ENTRY(1,icInstructions).
   ELSE 
     ttTransAction.cTransActionErrorHandl  = ENTRY(1,icInstructions).

   IF NUM-ENTRIES(icInstructions) > 1 THEN 
     ttTransAction.cTransActionPostUpdProc = ENTRY(2,icInstructions).
END.

CREATE ttTransRecord.
ASSIGN ixTransRecordId                    = ixTransRecordId + 1
       ttTransRecord.iTransActionId       = ttTransAction.iTransActionId
       ttTransRecord.iTransRecordId       = ixTransRecordId
       ttTransRecord.cTransRecordIdValue  = IF icIdFldsOrCrit NE "" THEN icIdValues ELSE ""
       ttTransRecord.cTransRecordValue    = icValues
       ttTransRecord.cTransRecordRowid    = IF icIdFldsOrCrit = "" THEN icIdValues ELSE ""
       cPostUpdProc                       = ""
       .
IF ibCommit THEN
  RETURN DoCommit(TRUE).
ELSE RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dumpMemInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dumpMemInfo Procedure 
FUNCTION dumpMemInfo RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Log memory use on AppServer (or client)
    Notes:  getappsmeminfo1.p written by Peter Van Dam (Netsetup)
------------------------------------------------------------------------------*/

IF SEARCH("getappsmeminfo1.p") NE ? OR SEARCH("getappsmeminfo1.r") NE ? THEN DO:
  IF cMemInfoLogFile = "" THEN cMemInfoLogFile = SESSION:TEMP-DIR + DYNAMIC-FUNCTION("getAppTitle") + "_meminfo.log".
  RUN getappsmeminfo1.p (cMemInfoLogFile).
  DYNAMIC-FUNCTION("setWebDoc","open",cMemInfoLogFile).
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DumpStaticASlibTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DumpStaticASlibTables Procedure 
FUNCTION DumpStaticASlibTables RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",hBuffClientAccess,0).
DYNAMIC-FUNCTION("ToExcelViaFile",htt_field,0).

FOR EACH ttCache:
  DYNAMIC-FUNCTION("ToExcelViaFile",ttCache.hBuffer,0).
END.


RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getActionPermission) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getActionPermission Procedure 
FUNCTION getActionPermission RETURNS LOGICAL
  ( INPUT icClientFileName AS CHAR,
    INPUT icObjectName     AS CHAR,
    INPUT icAction         AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF cUserLevel = "super" THEN RETURN YES.

FIND FIRST ttFunctionAccess
     WHERE ttFunctionAccess.cClientFileName = icClientFileName
       AND ttFunctionAccess.cObjectName     = icObjectName
       AND CAN-DO(ttFunctionAccess.cAction,icAction)
     NO-ERROR.
IF NOT AVAIL ttFunctionAccess THEN DO:
  FIND FIRST ttFunctionAccess
       WHERE ttFunctionAccess.cClientFileName = icClientFileName
         AND ttFunctionAccess.cObjectName     = ""
         AND CAN-DO(ttFunctionAccess.cAction,icAction)
       NO-ERROR.
  IF NOT AVAIL ttFunctionAccess THEN 
    FIND FIRST ttFunctionAccess
         WHERE ttFunctionAccess.cClientFileName = ""
           AND ttFunctionAccess.cObjectName     = ""
           AND CAN-DO(ttFunctionAccess.cAction,icAction)
         NO-ERROR.
END.

IF AVAIL ttFunctionAccess THEN 
  RETURN ttFunctionAccess.bAccess.
ELSE
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppserviceHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAppserviceHandle Procedure 
FUNCTION getAppserviceHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hReturnValue AS HANDLE NO-UNDO.

IF VALID-HANDLE(hUseAppServer) THEN DO:
  hReturnValue = hUseAppServer.
  hUseAppServer = ?.
END.
ELSE
  hReturnValue = hAppServer.

RETURN hReturnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppserviceId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAppserviceId Procedure 
FUNCTION getAppserviceId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF PROGRAM-NAME(2) MATCHES "*jbox*" THEN
  RETURN cAppServiceId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAppTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAppTitle Procedure 
FUNCTION getAppTitle RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cAppTitle. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASlibBeaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getASlibBeaviour Procedure 
FUNCTION getASlibBeaviour RETURNS CHARACTER
  ( INPUT icBehaviour AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icBehaviour):
  CASE ENTRY(ix,icBehaviour):
    WHEN "TransLogFile"        THEN cReturn = cReturn + cTransLogFile + ",".
    WHEN "QueryLogFile"        THEN cReturn = cReturn + cQueryLogFile + ",".
    WHEN "MemInfoLogFile"      THEN cReturn = cReturn + cMemInfoLogFile + ",".
  END CASE.
END.

RETURN TRIM(cReturn,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASUserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getASUserId Procedure 
FUNCTION getASUserId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cASUserId.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASuserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getASuserName Procedure 
FUNCTION getASuserName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cASuserName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBaseLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBaseLanguageCode Procedure 
FUNCTION getBaseLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cBaseLanguage.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferDbFieldSpec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBufferDbFieldSpec Procedure 
FUNCTION getBufferDbFieldSpec RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: In cases where no fieldspec was given for a query this is returned 
           from the server procedure and will be retrieve from the NewBrowse / NewQuery here 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFieldSpec AS CHAR NO-UNDO.

ASSIGN cFieldSpec         = cBufferDbFieldSpec
       cBufferDbFieldSpec = "".

RETURN cFieldSpec.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBufferFieldSpec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBufferFieldSpec Procedure 
FUNCTION getBufferFieldSpec RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: In cases where no fieldspec was given for a query this is returned 
           from the server procedure and will be retrieve from the NewBrowse / NewQuery here 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFieldSpec AS CHAR NO-UNDO.

ASSIGN cFieldSpec       = cBufferFieldSpec
       cBufferFieldSpec = "".

RETURN cFieldSpec.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-getCacheBufferHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCacheBufferHandle Procedure 
FUNCTION getCacheBufferHandle RETURNS HANDLE
  ( INPUT icCacheTable AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttCache
     WHERE ttCache.cTableName = cTable 
     NO-ERROR.
IF AVAIL ttCache AND VALID-HANDLE(ttCache.hBuffer) THEN
  RETURN ttCache.hBuffer.

RETURN ?.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCodeMasterCompanyId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCompany Procedure 
FUNCTION getCompany RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN STRING(iCompanyId).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCompanyId Procedure 
FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCompanyId.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyLogo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCompanyLogo Procedure 
FUNCTION getCompanyLogo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCompanyLogo.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCompanyName Procedure 
FUNCTION getCompanyName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCompanyName.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCurrentChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentChanged Procedure 
FUNCTION getCurrentChanged RETURNS CHARACTER
  ( INPUT ihBuffer AS HANDLE,
    INPUT icFields AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Compare contents of client temp-table buffer (ihBuffer) with current db values
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocFieldsChanged AS CHAR NO-UNDO.
DEF VAR cMyOrgValues    AS CHAR NO-UNDO.
DEF VAR rRowid          AS ROWID NO-UNDO.
DEF VAR hField          AS HANDLE NO-UNDO.
DEF VAR cReturn         AS CHAR NO-UNDO.

IF cCurrentRowid NE "" THEN
  ASSIGN rRowid        = TO-ROWID(cCurrentRowid)
         cCurrentRowid = "".
ELSE
  rRowid = TO-ROWID(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN "".

DO ix = 1 TO NUM-ENTRIES(icFields):
  hField = ihBuffer:BUFFER-FIELD(ENTRY(ix,icFields)) NO-ERROR.
  IF VALID-HANDLE(hField) THEN
    cMyOrgValues = cMyOrgValues + (IF hField:BUFFER-VALUE NE ? THEN STRING(hField:BUFFER-VALUE) ELSE "") + "|".
  ELSE RETURN "".
END.
cMyOrgValues = SUBSTR(cMyOrgValues,1,LENGTH(cMyOrgValues) - 1).


IF DYNAMIC-FUNCTION("getAppServiceHandle") NE ? THEN DO:
  RUN jbserv_checkcurrentchanged.p ON DYNAMIC-FUNCTION("getAppServiceHandle")
      (ihBuffer:NAME,
       rRowid,
       icFields,
       cMyOrgValues,
       OUTPUT ocFieldsChanged) 
       NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      cReturn = getCurrentChanged (ihBuffer,icFields).
  END.
END.
ELSE 
  RUN jbserv_checkcurrentchanged.p
      (ihBuffer:NAME,
       rRowid,
       icFields,
       cMyOrgValues,
       OUTPUT ocFieldsChanged).

IF ocFieldsChanged NE "" THEN DO:
/*   cReturn = "Følgende endringer er foretatt av andre etter at du leste forekomsten:" + CHR(10) + CHR(10). */
  cReturn = "Changes have been done by another user since you retrieved the record:" + CHR(10) + CHR(10).
  DO ix = 1 TO NUM-ENTRIES(ocFieldsChanged,CHR(1)):
    cReturn = cReturn + ENTRY(3,ENTRY(ix,ocFieldsChanged,CHR(1)),CHR(3)) + "  ("
                      + ENTRY(1,ENTRY(ix,ocFieldsChanged,CHR(1)),CHR(3)) + ")  FROM   "
                      + ENTRY(2,ENTRY(ix,ocFieldsChanged,CHR(1)),CHR(3)) + "   TO   "
                      + ENTRY(4,ENTRY(ix,ocFieldsChanged,CHR(1)),CHR(3)) + CHR(10).
  END.
  cReturn = cReturn + CHR(10).
END.

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDataDict) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDataDict Procedure 
FUNCTION getDataDict RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Retrieve data dictionary definitions 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffer AS HANDLE NO-UNDO.

/* hBuffer = BUFFER tt_file:HANDLE.                                                                           */
/* getTempTable ("jbquery_getdatadict.p","_file;" + cDBfilter + ";" + cTableFilter,hBuffer).                  */
/* hBuffer = BUFFER tt_field:HANDLE.                                                                          */
/* getTempTable ("jbquery_getdatadict.p","_field;" + cDBfilter + ";" + cTableFilter + cFieldFilter,hBuffer).  */

RETURN TRUE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDate Procedure 
FUNCTION getDate RETURNS DATE
  (INPUT icDate AS CHAR):

icDate = TRIM(icDate,"/").
IF icDate NE ? AND icDate NE "" THEN DO:
  IF SESSION:DATE-FORMAT = "dmy" THEN
    RETURN DATE(INT(SUBSTR(icDate,4,2)),INT(SUBSTR(icDate,1,2)),INT(SUBSTR(icDate,7))).
  ELSE 
    RETURN DATE(INT(SUBSTR(icDate,1,2)),INT(SUBSTR(icDate,4,2)),INT(SUBSTR(icDate,7))).
END.
ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldCacheBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldCacheBuffer Procedure 
FUNCTION getFieldCacheBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN htt_field.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldDataType Procedure 
FUNCTION getFieldDataType RETURNS CHARACTER
  ( INPUT icDbField AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDbName    AS CHAR NO-UNDO.
DEF VAR cTableName AS CHAR NO-UNDO.
DEF VAR cFieldName AS CHAR NO-UNDO.
DEF VAR cDataType  AS CHAR NO-UNDO.

IF NUM-ENTRIES(icDbField,".") = 3 THEN DO:
  ASSIGN cDbName    = ENTRY(1,icDbField,".")
         cTableName = ENTRY(2,icDbField,".")
         cFieldName = ENTRY(3,icDbField,".")
         .

  FIND FIRST tt_field 
       WHERE tt_field.Db-Name    = cDbName
         AND tt_field.Table-Name = cTableName
         AND tt_field.Field-Name = cFieldName
       NO-ERROR.
END.
ELSE IF NUM-ENTRIES(icDbField,".") = 2 THEN DO:
  ASSIGN cTableName = ENTRY(1,icDbField,".")
         cFieldName = ENTRY(2,icDbField,".")
         .
  FIND FIRST tt_field 
       WHERE tt_field.Table-Name = cTableName
         AND tt_field.Field-Name = cFieldName
       NO-ERROR.
END.
ELSE DO:
  cFieldName = icDbField.
  FIND FIRST tt_field 
       WHERE tt_field.Field-Name = cFieldName
       NO-ERROR.
END.

IF AVAIL tt_field THEN RETURN tt_field.Field-Type.
ELSE DO:
  IF getAppServiceHandle() NE ? THEN
    RUN jbserv_getfielddatatype.p
        ON getAppServiceHandle()
       (DYNAMIC-FUNCTION("getSessionId"),
        cDbName,
        cTableName,
        cFieldName,
        OUTPUT cDataType)
        .
  ELSE 
    RUN jbserv_getfielddatatype.p
       (DYNAMIC-FUNCTION("getSessionId"),
        cDbName,
        cTableName,
        cFieldName,
        OUTPUT cDataType)
        .
  RETURN cDataType.
END.

RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldList Procedure 
FUNCTION getFieldList RETURNS CHARACTER
  ( INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR) :
/*------------------------------------------------------------------------------
   Retrieve a list of fields from a query.
   - Supports calculated fields and joined buffers
   F.ex to get a list of customer names for salesreps in a region:
      BuffersAndFields:  SalesRep,Customer;CustName
      QueryCriteria:     WHERE Region = 'NY',EACH Customer OF SalesRep NO-LOCK
--------------------------------------------------------------------------------------*/
DEF VAR cFieldPairs AS CHAR NO-UNDO.

IF icQueryCriteria = ? THEN 
  RETURN ?.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getfieldlist.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffersAndFields,
      icQueryCriteria,
      OUTPUT cFieldPairs,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      cFieldPairs = getFieldList (icBuffersAndFields,icQueryCriteria).
  END.
END.
ELSE 
  RUN jbserv_getfieldlist.p
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffersAndFields,
      icQueryCriteria,
      OUTPUT cFieldPairs,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

IF cReturnTrans NE "" THEN 
  MESSAGE cReturnTrans VIEW-AS ALERT-BOX.

CheckReturnValue(cReturnTrans).

RETURN cFieldPairs.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldsForTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldsForTable Procedure 
FUNCTION getFieldsForTable RETURNS CHARACTER
  ( INPUT icDbTable AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cDbName    AS CHAR NO-UNDO.
DEF VAR cTableName AS CHAR NO-UNDO.
DEF VAR cFields    AS CHAR NO-UNDO.

IF NUM-ENTRIES(icDbTable,".") = 2 THEN 
  ASSIGN cDbName    = ENTRY(1,icDbTable,".")
         cTableName = ENTRY(2,icDbTable,".")
         .

ELSE 
  cTableName = icDbTable.

IF cTableName MATCHES "buf*_*" THEN cTableName = SUBSTR(cTableName,6).

FOR EACH tt_field 
    WHERE tt_field.Table-Name = cTableName
      AND (IF cDbName NE "" THEN tt_field.Db-Name = cDbName ELSE TRUE)
    :
  IF tt_field.Field-Extent = 0 THEN    
    cFields = cFields + (IF cFields NE "" THEN ";" ELSE "") + tt_field.Field-Name.
  ELSE DO ix = 1 TO tt_field.Field-Extent:
      cFields = cFields + (IF cFields NE "" THEN ";" ELSE "") + tt_field.Field-Name + "[" + STRING(ix) + "]".
  END.
END.
IF cFields NE "" THEN RETURN cFields.  
ELSE DO:
  IF getAppServiceHandle() NE ? THEN
    RUN jbserv_getfieldsfortable.p
        ON getAppServiceHandle()
       (DYNAMIC-FUNCTION("getSessionId"),
        cDbName,
        cTableName,
        OUTPUT cFields)
        .
  ELSE 
    RUN jbserv_getfieldsfortable.p
       (DYNAMIC-FUNCTION("getSessionId"),
        cDbName,
        cTableName,
        OUTPUT cFields)
        .
  RETURN cFields.
END.

RETURN "".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldValues Procedure 
FUNCTION getFieldValues RETURNS CHARACTER
  ( INPUT icBufferName    AS CHAR,
    INPUT icQueryCriteria AS CHAR,
    INPUT icColumns       AS CHAR ) :
/* ------------------------------------------------------------------------------
   Retrieve values for one or more fields, or the ROWID for a buffer (table).
   - To get value from extent, add the extent number to the field-list:
     <column>;<extent>,<column>..  
   - To get the rowid, use ROWID as column name
------------------------------------------------------------------------------*/
DEF VAR cValues AS CHAR NO-UNDO.

IF icQueryCriteria = ? THEN 
  RETURN ?.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getfieldvalues.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      icBufferName,
      icQueryCriteria,
      icColumns,
      OUTPUT cValues,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      cValues = getFieldValues (icBufferName,icQueryCriteria,icColumns).
  END.
END.
ELSE 
  RUN jbserv_getfieldvalues.p
     (DYNAMIC-FUNCTION("getSessionId"),
      icBufferName,
      icQueryCriteria,
      icColumns,
      OUTPUT cValues,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

IF cReturnTrans NE "" THEN 
  MESSAGE cReturnTrans VIEW-AS ALERT-BOX.

CheckReturnValue(cReturnTrans).

RETURN cValues.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFunctionRestrictions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFunctionRestrictions Procedure 
FUNCTION getFunctionRestrictions RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocUserLevel AS CHAR NO-UNDO.
IF cUserLevel = "" THEN DO:
  IF getAppServiceHandle() NE ? THEN
    RUN jbadmin_getuserlevel.p ON getAppServiceHandle() (getSessionId(),iCompanyId,OUTPUT ocUserLevel,OUTPUT bOK).
  ELSE 
    RUN jbadmin_getuserlevel.p (getSessionId(),iCompanyId,OUTPUT ocUserLevel,OUTPUT bOK).
  IF bOk THEN cUserLevel = ocUserLevel.
END.  

hBuffClientAccess:EMPTY-TEMP-TABLE().
IF runproc("jbadmin_getclient_restrictions.p","",httClientAccess) THEN DO:
  getRunProcReturnTable(hBuffClientAccess).  
  RETURN YES.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsBufferCached) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsBufferCached Procedure 
FUNCTION getIsBufferCached RETURNS LOGICAL
  ( INPUT ihBufferHandle AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttCache 
     WHERE ttCache.hBuffer = ihBufferHandle
     NO-ERROR.
RETURN AVAIL ttCache.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsFieldInTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsFieldInTable Procedure 
FUNCTION getIsFieldInTable RETURNS LOGICAL
  ( INPUT icDbField AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDbName    AS CHAR NO-UNDO.
DEF VAR cTableName AS CHAR NO-UNDO.
DEF VAR cFieldName AS CHAR NO-UNDO.
DEF VAR cDataType  AS CHAR NO-UNDO.

IF NUM-ENTRIES(icDbField,".") = 3 THEN DO:
  ASSIGN cDbName    = ENTRY(1,icDbField,".")
         cTableName = ENTRY(2,icDbField,".")
         cFieldName = ENTRY(3,icDbField,".")
         .

  FIND FIRST tt_field 
       WHERE tt_field.Db-Name    = cDbName
         AND tt_field.Table-Name = cTableName
         AND tt_field.Field-Name = cFieldName
       NO-ERROR.
END.
ELSE IF NUM-ENTRIES(icDbField,".") = 2 THEN DO:
  ASSIGN cTableName = ENTRY(1,icDbField,".")
         cFieldName = ENTRY(2,icDbField,".")
         .
  FIND FIRST tt_field 
       WHERE tt_field.Table-Name = cTableName
         AND tt_field.Field-Name = cFieldName
       NO-ERROR.
END.
ELSE DO:
  cFieldName = icDbField.
  FIND FIRST tt_field 
       WHERE tt_field.Field-Name = cFieldName
       NO-ERROR.
END.

IF AVAIL tt_field THEN RETURN YES.
ELSE IF NOT CAN-FIND(FIRST tt_field) THEN
  RETURN IsFieldNameInTable(cTableName,cFieldName).

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsTableCached) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsTableCached Procedure 
FUNCTION getIsTableCached RETURNS LOGICAL
  ( INPUT ihTableHandle AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttCache 
     WHERE ttCache.hTempTable = ihTableHandle
     NO-ERROR.
RETURN AVAIL ttCache.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsTableInstalled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsTableInstalled Procedure 
FUNCTION getIsTableInstalled RETURNS LOGICAL
  ( INPUT icDbTable AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDbName    AS CHAR NO-UNDO.
DEF VAR cTableName AS CHAR NO-UNDO.
DEF VAR bOk        AS LOG  NO-UNDO.

IF NUM-ENTRIES(icDbTable,".") = 2 THEN DO:
  ASSIGN cDbName = ENTRY(1,icDbTable,".")
         cTableName = ENTRY(2,icDbTable,".")
         .
  FIND FIRST tt_field 
       WHERE tt_field.Db-Name    = cDbName
         AND tt_field.Table-Name = cTableName
       NO-ERROR.
END.
ELSE DO:
  cTableName = icDbTable.
  FIND FIRST tt_field 
       WHERE tt_field.Table-Name = cTableName
       NO-ERROR.
END.

IF AVAIL tt_field THEN RETURN YES.
ELSE DO:
  IF getAppServiceHandle() NE ? THEN
    RUN jbserv_getistableinstalled.p
        ON getAppServiceHandle()
       (DYNAMIC-FUNCTION("getSessionId"),
        cDbName,
        cTableName,
        OUTPUT bOk)
        .
  ELSE 
    RUN jbserv_getistableinstalled.p
       (DYNAMIC-FUNCTION("getSessionId"),
        cDbName,
        cTableName,
        OUTPUT bOk)
        .
END.

RETURN bOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsUserSettingInstalled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsUserSettingInstalled Procedure 
FUNCTION getIsUserSettingInstalled RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSettingList AS CHAR NO-UNDO.

FIND FIRST ttCache
     WHERE ttCache.cTableName = "JBoxUserSetting"
     NO-ERROR.
IF AVAIL ttCache THEN RETURN YES.

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getusersetting.p
      ON getAppServiceHandle()
     (getSessionId(),
      "dummy",
      "dummy",
      "dummy",
      "dummy",
      OUTPUT cSettingList)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF NOT ERROR-STATUS:GET-NUMBER(1) = 293 AND ReconnectServer() THEN 
      cSettingList = getUserSetting ("dummy","dummy","dummy","dummy").
  END.
END.
ELSE 
  RUN jbserv_getusersetting.p
     (getSessionId(),
      "dummy",
      "dummy",
      "dummy",
      "dummy",
      OUTPUT cSettingList)
      NO-ERROR.

IF RETURN-VALUE BEGINS "No setting" THEN
  RETURN NO.
ELSE RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLanguageCode Procedure 
FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cLanguage.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLanguages Procedure 
FUNCTION getLanguages RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cAppLanguages.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMenuPermission) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuPermission Procedure 
FUNCTION getMenuPermission RETURNS LOGICAL
  ( INPUT icAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttFunctionAccess
     WHERE ttFunctionAccess.cAction = icAction
       AND ttFunctionAccess.bMenu
     NO-ERROR.

IF AVAIL ttFunctionAccess THEN
  RETURN ttFunctionAccess.bAccess.
ELSE
  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMyDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyDataSet Procedure
FUNCTION getMyDataSet RETURNS LOGICAL 
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetDS       AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Use a custom-made procedure to fill temp-table defined on the client 
------------------------------------------------------------------------------*/
DEF VAR obOk           AS LOG    NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_runproc_ds.p
      ON getAppServiceHandle()
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT DATASET-HANDLE ihTargetDS,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      obOK = getMyDataSet(icServerProgram,icParamList,ihTargetDS).
  END.
END.
ELSE 
  RUN jbserv_runproc_ds.p
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT DATASET-HANDLE ihTargetDS,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      .

SESSION:SET-WAIT-STATE("").

CheckReturnValue(cReturnTrans).

RETURN obOk.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-getMyTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyTempTable Procedure 
FUNCTION getMyTempTable RETURNS LOGICAL
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetBuffer   AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Use a custom-made procedure to fill temp-table defined on the client 
------------------------------------------------------------------------------*/
DEF VAR httTable       AS HANDLE NO-UNDO.
DEF VAR httTableBuffer AS HANDLE NO-UNDO.
DEF VAR httTableQuery  AS HANDLE NO-UNDO.
DEF VAR obOk           AS LOG    NO-UNDO.

IF ihTargetBuffer:TYPE = "buffer" THEN
  httTable = ihTargetBuffer:TABLE-HANDLE.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_runproc.p
      ON getAppServiceHandle()
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT TABLE-HANDLE httTable,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      obOK = getMyTempTable(icServerProgram,icParamList,ihTargetBuffer).
  END.
END.
ELSE 
  RUN jbserv_runproc.p
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT TABLE-HANDLE httTable,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      .

SESSION:SET-WAIT-STATE("").

CheckReturnValue(cReturnTrans).

RETURN obOk.
/*
httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
MESSAGE PROGRAM-NAME(1) SKIP
        httTableBuffer SKIP
        ihTargetBuffer 
        VIEW-AS ALERT-BOX.
CREATE QUERY httTableQuery NO-ERROR.
httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
httTableQuery:QUERY-OPEN.
httTableQuery:GET-FIRST().
REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:
/*   ihTargetBuffer:BUFFER-CREATE().             */
/*   ihTargetBuffer:BUFFER-COPY(httTableBuffer). */
  MESSAGE PROGRAM-NAME(1) SKIP
          httTableBuffer:buffer-field("instnr"):buffer-value
          VIEW-AS ALERT-BOX.
  httTableQuery:GET-NEXT().
END.
DELETE OBJECT httTableQuery.
/* DELETE OBJECT httTable.  */
RETURN YES.
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbCreateAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoDbCreateAccessBuffers Procedure 
FUNCTION getNoDbCreateAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch additional security info after a request for data
    Notes: This function must be called subsequently after a request for data (getTempTableJoin)            
------------------------------------------------------------------------------*/
DEF VAR cNoCreateBuffers AS CHAR NO-UNDO.

ASSIGN cNoCreateBuffers         = cNoDbCreateAccessBuffers
       cNoDbCreateAccessBuffers = "".

RETURN cNoCreateBuffers.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbDeleteAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoDbDeleteAccessBuffers Procedure 
FUNCTION getNoDbDeleteAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch additional security info after a request for data
    Notes: This function must be called subsequently after a request for data (getTempTableJoin)            
------------------------------------------------------------------------------*/
DEF VAR cNoDeleteBuffers AS CHAR NO-UNDO.

ASSIGN cNoDeleteBuffers         = cNoDbDeleteAccessBuffers
       cNoDbDeleteAccessBuffers = "".

RETURN cNoDeleteBuffers.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbReadAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoDbReadAccessBuffers Procedure 
FUNCTION getNoDbReadAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch additional security info after a request for data
    Notes: This function must be called subsequently after a request for data (getTempTableJoin)            
------------------------------------------------------------------------------*/
DEF VAR cNoReadBuffers AS CHAR NO-UNDO.

ASSIGN cNoReadBuffers         = cNoDbReadAccessBuffers
       cNoDbReadAccessBuffers = "".

RETURN cNoReadBuffers.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbReadAccessFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoDbReadAccessFields Procedure 
FUNCTION getNoDbReadAccessFields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch additional security info after a request for data
    Notes: This function must be called subsequently after a request for data (getTempTableJoin)            
------------------------------------------------------------------------------*/
DEF VAR cNoReadFields AS CHAR NO-UNDO.

ASSIGN cNoReadFields         = cNoDbReadAccessFields
       cNoDbReadAccessFields = "".

RETURN cNoReadFields.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbWriteAccessBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoDbWriteAccessBuffers Procedure 
FUNCTION getNoDbWriteAccessBuffers RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch additional security info after a request for data
    Notes: This function must be called subsequently after a request for data (getTempTableJoin)            
------------------------------------------------------------------------------*/
DEF VAR cNoWriteBuffers AS CHAR NO-UNDO.

ASSIGN cNoWriteBuffers         = cNoDbWriteAccessBuffers
       cNoDbWriteAccessBuffers = "".

RETURN cNoWriteBuffers.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoDbWriteAccessFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoDbWriteAccessFields Procedure 
FUNCTION getNoDbWriteAccessFields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Fetch additional security info after a request for data
    Notes: This function must be called subsequently after a request for data (getTempTableJoin)            
------------------------------------------------------------------------------*/
DEF VAR cNoWriteFields AS CHAR NO-UNDO.

ASSIGN cNoWriteFields         = cNoDbWriteAccessFields
       cNoDbWriteAccessFields = "".

RETURN cNoWriteFields.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentCompany Procedure 
FUNCTION getParentCompany RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cParentCompanyId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryStatFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQueryStatFields Procedure 
FUNCTION getQueryStatFields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cQueryStatFields.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryStatFieldValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQueryStatFieldValues Procedure 
FUNCTION getQueryStatFieldValues RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cQueryStatFieldValues.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRecId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRecId Procedure 
FUNCTION getRecId RETURNS RECID
  ( INPUT icBuffer   AS CHAR,
    INPUT icRowId    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Get a old-fashion rec-id from buffername and rowid 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR rRecId AS RECID NO-UNDO.

IF icRowId = ? THEN 
  RETURN ?.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getrecid.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffer,
      icRowId,
      OUTPUT rRecId,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      rRecId = getRecId (icBuffer,icRowId).
  END.
END.
ELSE 
  RUN jbserv_getrecid.p
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffer,
      icRowId,
      OUTPUT rRecId,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

RETURN rRecId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRecordCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRecordCount Procedure 
FUNCTION getRecordCount RETURNS INTEGER
  ( INPUT icBuffer         AS CHAR,
    INPUT icQueryCriteria  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iRecCount AS INT NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getrecordcount.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffer,
      icQueryCriteria,
      OUTPUT iRecCount,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      iRecCount = getRecordCount (icBuffer,icQueryCriteria).
  END.
END.
ELSE 
  RUN jbserv_getrecordcount.p
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffer,
      icQueryCriteria,
      OUTPUT iRecCount,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

RETURN iRecCount.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReturnedTransValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReturnedTransValue Procedure 
FUNCTION getReturnedTransValue RETURNS CHARACTER
  ( INPUT icFieldName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cReturnValue  AS CHAR NO-UNDO.
FIND FIRST ttTransRecord NO-ERROR.
IF AVAIL ttTransRecord THEN DO:
  DO ix = 1 TO NUM-ENTRIES(ttTransRecord.cReturnExtraFields):
    IF ENTRY(ix,ttTransRecord.cReturnExtraFields) = icFieldName THEN
      cReturnValue = ENTRY(ix,ttTransRecord.cReturnExtraValues,"|").
  END.
  IF cReturnValue = "" THEN DO:
    FIND FIRST ttTransAction 
         WHERE ttTransAction.iTransActionId = ttTransRecord.iTransActionId.
    DO ix = 1 TO NUM-ENTRIES(ttTransAction.cTransActionValueFields):
      IF ENTRY(ix,ttTransAction.cTransActionValueFields) = icFieldName THEN
        cReturnValue = ENTRY(ix,ttTransRecord.cTransRecordValues,"|").
    END.
  END.

END.

RETURN cReturnValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRole) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRole Procedure 
FUNCTION getRole RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN STRING(iCompanyId).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRoleId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRoleId Procedure 
FUNCTION getRoleId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowIdList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowIdList Procedure 
FUNCTION getRowIdList RETURNS CHARACTER
  ( INPUT icBufferList         AS CHAR,
    INPUT icBufferReturnList   AS CHAR,
    INPUT icQueryCriteria      AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Fetch comma-separated list of rowid's from 1 to many buffers
    Notes: If icBufferReturnList is blank, the last element in icBufferList is assumed to be the return-buffer (for rowid's)
------------------------------------------------------------------------------*/
DEF VAR cRowIdList AS CHAR NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getrowidlist.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      icBufferList,
      icBufferReturnList,
      icQueryCriteria,
      OUTPUT cRowIdList,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      cRowIdList = getRowIdList (icBufferList,icBufferReturnList,icQueryCriteria).
  END.
END.
ELSE 
  RUN jbserv_getrowidlist.p
     (DYNAMIC-FUNCTION("getSessionId"),
      icBufferList,
      icBufferReturnList,
      icQueryCriteria,
      OUTPUT cRowIdList,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

CheckReturnValue(cReturnTrans).

IF cReturnTrans NE "" THEN DO:
  MESSAGE PROGRAM-NAME(1) SKIP
          cReturnTrans SKIP
          VIEW-AS ALERT-BOX.
  IF NUM-ENTRIES(cReturnTrans,CHR(10)) > 1 THEN
    CLIPBOARD:VALUE = ENTRY(1,cReturnTrans,CHR(10)).
END.

RETURN cRowIdList.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRunProcReturnDs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRunProcReturnDs Procedure 
FUNCTION getRunProcReturnDs RETURNS HANDLE
  ( INPUT ihTargetDs   AS HANDLE,
    INPUT ibAppend     AS LOG,
    INPUT ibReplace    AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Retrieve temp-table data returned from last runProc function call
     Note: It's the programmers responsibility to delete the temp-table.
           The handle will also be reused..
------------------------------------------------------------------------------*/
DEF VAR httTableBuffer AS HANDLE NO-UNDO.
DEF VAR httTableQuery  AS HANDLE NO-UNDO.

IF ihTargetDs = ? THEN DO:
  RETURN hdsRunProc.
END.
ELSE DO:
  ihTargetDs:COPY-DATASET(hdsRunProc,ibAppend,ibReplace).
  DELETE OBJECT hdsRunProc.
  RETURN ?.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRunProcReturnTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRunProcReturnTable Procedure 
FUNCTION getRunProcReturnTable RETURNS HANDLE
  ( INPUT ihTargetBuffer   AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Retrieve temp-table data returned from last runProc function call
     Note: It's the programmers responsibility to delete the temp-table.
           The handle will also be reused..
------------------------------------------------------------------------------*/
DEF VAR httTableBuffer AS HANDLE NO-UNDO.
DEF VAR httTableQuery  AS HANDLE NO-UNDO.

IF ihTargetBuffer = ? THEN DO:
  RETURN httRunProc.
END.
ELSE DO:
  ihTargetBuffer:EMPTY-TEMP-TABLE().
  httTableBuffer = httRunProc:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httTableQuery NO-ERROR.
  httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
  httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
  httTableQuery:QUERY-OPEN().
  httTableQuery:GET-FIRST().
  REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:
    ihTargetBuffer:BUFFER-CREATE().
    ihTargetBuffer:BUFFER-COPY(httTableBuffer).
    httTableQuery:GET-NEXT().
  END.
  DELETE OBJECT httTableQuery.
  DELETE OBJECT httRunProc.
  RETURN ?.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getServerReturnParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getServerReturnParam Procedure 
FUNCTION getServerReturnParam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Get any output parameters from a server validation hook procedure  
    Notes: Can be used for any server routine that returns a string that is examined by
           the CheckReturnValue function. 
           To pass an output parameter in addition to / rather than an error message 
           add a separator to the return value: <msg (blank)>¤<output param>
------------------------------------------------------------------------------*/

RETURN cServerReturnParam. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getServerTransReturnParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getServerTransReturnParam Procedure 
FUNCTION getServerTransReturnParam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Return any parameter info from the latest call to jbserv_servertrans.p
    Notes:  Normally used to enable output parameters from 
            validation and post_update hooks.
            To pass an output parameter in addition to / rather than an error message 
            add a separator to the return value: <msg (blank)>¤<output param>
------------------------------------------------------------------------------*/

RETURN cServerTransReturnParam.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSessionContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSessionContext Procedure 
FUNCTION getSessionContext RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN DYNAMIC-FUNCTION("getFieldValues","JBoxLoginSession",
                        "WHERE cSessionId = '" + DYNAMIC-FUNCTION("getSessionId") + "'",
                        "cContext").


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSessionData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSessionData Procedure 
FUNCTION getSessionData RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NUM-ENTRIES(cAppLanguages,"|") > 1 THEN DO:
  IF bJukeBoxDemo THEN DO:
    IF SEARCH("jboxtranslation.dmp") NE ? THEN DO:
      DEF VAR VariJBoxTranslationId AS INT NO-UNDO.
      DEF VAR VarcObjectName        AS CHAR NO-UNDO.
      DEF VAR VarcObjectType        AS CHAR NO-UNDO.
      DEF VAR VarcLabel             AS CHAR NO-UNDO.
      DEF VAR VarcColumnLabel       AS CHAR NO-UNDO.
      DEF VAR VarcTooltip           AS CHAR NO-UNDO.
      DEF VAR VarcHelpText          AS CHAR NO-UNDO.
      DEF VAR VarcInitValue         AS CHAR NO-UNDO.
      DEF VAR VarcHelpTag           AS CHAR NO-UNDO.
      DEF VAR VarcLanguage          AS CHAR NO-UNDO.
      DEF VAR VarcFileName          AS CHAR NO-UNDO.
      DEF VAR VariMsgNo             AS INT NO-UNDO.
      DEF VAR VarcMessage           AS CHAR NO-UNDO.
      DEF VAR VariMsgDataPos        AS INT NO-UNDO.
      DEF VAR VarcTitle             AS CHAR NO-UNDO.
      DEF VAR VariMsgButtonType     AS INT NO-UNDO.
      DEF VAR VardCreated           AS DATE NO-UNDO.
      DEF VAR VarcCreatedBy         AS CHAR NO-UNDO.
      DEF VAR VardModified          AS DATE NO-UNDO.
      DEF VAR VarcModifiedBy        AS CHAR NO-UNDO.

      INPUT FROM VALUE(SEARCH("jboxtranslation.dmp")).
      CREATE TEMP-TABLE httTranslation.
      httTranslation:ADD-NEW-FIELD("iJBoxTranslationId","INTEGER").
      httTranslation:ADD-NEW-FIELD("cObjectName","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cObjectType","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cLabel","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cColumnLabel","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cTooltip","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cHelpText","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cInitValue","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cHelpTag","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cLanguage","CHARACTER").
      httTranslation:ADD-NEW-FIELD("cFileName","CHARACTER").
      httTranslation:ADD-NEW-FIELD("iMsgNo","INTEGER").
      httTranslation:ADD-NEW-FIELD("cMessage","CHARACTER").
      httTranslation:ADD-NEW-FIELD("iMsgDataPos","INTEGER").
      httTranslation:ADD-NEW-FIELD("cTitle","CHARACTER").
      httTranslation:ADD-NEW-FIELD("iMsgButtonType","INTEGER").
      httTranslation:ADD-NEW-FIELD("dCreated","DATE").
      httTranslation:ADD-NEW-FIELD("cCreatedBy","CHARACTER").
      httTranslation:ADD-NEW-FIELD("dModified","DATE").
      httTranslation:ADD-NEW-FIELD("cModifiedBy","CHARACTER").
      httTranslation:TEMP-TABLE-PREPARE("JBoxTranslation").
      hBuffTranslation = httTranslation:DEFAULT-BUFFER-HANDLE.
      REPEAT:
        IMPORT DELIMITER ";" VariJBoxTranslationId
                             VarcObjectName       
                             VarcObjectType       
                             VarcLabel            
                             VarcColumnLabel      
                             VarcTooltip          
                             VarcHelpText         
                             VarcInitValue        
                             VarcHelpTag          
                             VarcLanguage         
                             VarcFileName         
                             VariMsgNo            
                             VarcMessage          
                             VariMsgDataPos       
                             VarcTitle            
                             VariMsgButtonType    
                             VardCreated          
                             VarcCreatedBy        
                             VardModified         
                             VarcModifiedBy.       
        IF VarcLanguage = cLanguage THEN DO:
          hBuffTranslation:BUFFER-CREATE().
          ASSIGN hBuffTranslation:BUFFER-FIELD("iJBoxTranslationId"):BUFFER-VALUE = VariJBoxTranslationId
                 hBuffTranslation:BUFFER-FIELD("cObjectName"):BUFFER-VALUE =        VarcObjectName       
                 hBuffTranslation:BUFFER-FIELD("cObjectType"):BUFFER-VALUE =        VarcObjectType       
                 hBuffTranslation:BUFFER-FIELD("cLabel"):BUFFER-VALUE =             VarcLabel            
                 hBuffTranslation:BUFFER-FIELD("cColumnLabel"):BUFFER-VALUE =       VarcColumnLabel      
                 hBuffTranslation:BUFFER-FIELD("cTooltip"):BUFFER-VALUE =           VarcTooltip          
                 hBuffTranslation:BUFFER-FIELD("cHelpText"):BUFFER-VALUE =          VarcHelpText         
                 hBuffTranslation:BUFFER-FIELD("cInitValue"):BUFFER-VALUE =         VarcInitValue        
                 hBuffTranslation:BUFFER-FIELD("cHelpTag"):BUFFER-VALUE =           VarcHelpTag          
                 hBuffTranslation:BUFFER-FIELD("cLanguage"):BUFFER-VALUE =          VarcLanguage         
                 hBuffTranslation:BUFFER-FIELD("cFileName"):BUFFER-VALUE =          VarcFileName         
                 hBuffTranslation:BUFFER-FIELD("iMsgNo"):BUFFER-VALUE =             VariMsgNo            
                 hBuffTranslation:BUFFER-FIELD("cMessage"):BUFFER-VALUE =           VarcMessage          
                 hBuffTranslation:BUFFER-FIELD("iMsgDataPos"):BUFFER-VALUE =        VariMsgDataPos       
                 hBuffTranslation:BUFFER-FIELD("cTitle"):BUFFER-VALUE =             VarcTitle            
                 hBuffTranslation:BUFFER-FIELD("iMsgButtonType"):BUFFER-VALUE =     VariMsgButtonType    
                 hBuffTranslation:BUFFER-FIELD("dCreated"):BUFFER-VALUE =           VardCreated          
                 hBuffTranslation:BUFFER-FIELD("cCreatedBy"):BUFFER-VALUE =         VarcCreatedBy        
                 hBuffTranslation:BUFFER-FIELD("dModified"):BUFFER-VALUE =          VardModified         
                 hBuffTranslation:BUFFER-FIELD("cModifiedBy"):BUFFER-VALUE =        VarcModifiedBy.      
        END.
      END.
      INPUT CLOSE.
    END.
  END.
  ELSE DO:
    IF BuildTableCache("JBoxTranslation|WHERE cLanguage = '" + cLanguage + "' AND iMsgNo = 0") THEN DO:
      FIND FIRST ttCache
           WHERE ttCache.cTableName = "JBoxTranslation"
           NO-ERROR.
      IF AVAIL ttCache THEN
        hBuffTranslation = ttCache.hBuffer.
    END.
  END.
END.

BuildTableCache("JBoxUserSetting|WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'"
                 + ";jbserv_getfieldcache.p").

/* IF NOT CAN-FIND(FIRST tt_file) AND bLoadDataDictOnStartup THEN  */
/*   getDataDict().                                                */

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSessionId Procedure 
FUNCTION getSessionId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF PROGRAM-NAME(2) MATCHES "*jbox*" THEN
  RETURN cSessionId.
ELSE RETURN ENCODE("zzz01/01/1900" + STRING(TIME)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStringTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStringTranslation Procedure 
FUNCTION getStringTranslation RETURNS CHARACTER
  ( INPUT icFileName    AS CHAR,
    INPUT icObjectType  AS CHAR,
    INPUT icObjectNames AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bTranslated   AS LOG    NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR cReturnString AS CHAR   NO-UNDO.
DEF VAR cObjectName   AS CHAR   NO-UNDO.
DEF VAR hTmpTransBuff AS HANDLE NO-UNDO.
DEF VAR hWidget       AS HANDLE NO-UNDO.
DEF VAR hWidgetRect   AS HANDLE NO-UNDO.
DEF VAR cWidgetType   AS CHAR   NO-UNDO.
DEF VAR bAvailWidget  AS LOG    NO-UNDO.

IF VALID-HANDLE(hTranManBuffer) THEN DO:    
  CREATE BUFFER hTmpTransBuff FOR TABLE hTranManBuffer.
  bAvailWidget = hTmpTransBuff:FIND-FIRST("WHERE cFileName   = '" + icFileName + "'" +
                                          "  AND cObjectName = '" + icObjectType + "'" +
                                          "  AND chWidget    NE ''") NO-ERROR.
END.

DO ix = 1 TO NUM-ENTRIES(icObjectNames,"|"):
  cObjectName = ENTRY(ix,icObjectNames,"|").
  bTranslated = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cObjectName + "'" +
                                    " AND cFileName   = '" + icFileName + "'" +
                                    " AND cObjectType = '" + icObjectType + "'") NO-ERROR.
  
  IF cTranslationReportFile NE "" THEN 
    PUT UNFORMATTED 
        cObjectName  ";"  
        (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cFileName"):BUFFER-VALUE ELSE "") ";" 
        icObjectType ";"
        STRING(bTranslated) ";"
        cObjectName ";" 
        (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cLabel"):BUFFER-VALUE ELSE "") ";" 
        SKIP.
  
  ELSE IF VALID-HANDLE(hTranManBuffer) THEN DO:
    bOk = hTranManBuffer:FIND-FIRST("WHERE cObjectName = '" + cObjectName + "'" +
                                     " AND cFileName   = '" + icFileName + "'" +
                                     " AND cObjectType = '" + icObjectType + "'") NO-ERROR.
    IF NOT bOk THEN DO:
      hTranManBuffer:BUFFER-CREATE().
      ASSIGN hTranManBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE      = cObjectName
             hTranManBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE = cObjectName
             hTranManBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = icObjectType
             hTranManBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE      = bTranslated
             hTranManBuffer:BUFFER-FIELD("cLanguage"):BUFFER-VALUE   = DYNAMIC-FUNCTION("getBaseLanguageCode")
             hTranManBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE   = icFileName
             .
    END.
    ELSE
      hTranManBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = bTranslated.

    IF bAvailWidget THEN DO:
      ASSIGN hTranManBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE = hTmpTransBuff:BUFFER-FIELD("hWidget"):BUFFER-VALUE
             hTranManBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE = hTmpTransBuff:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE
             hTranManBuffer:BUFFER-FIELD("bPopupDefined"):BUFFER-VALUE = YES
             hWidget = hTmpTransBuff:BUFFER-FIELD("hWidget"):BUFFER-VALUE.

      IF hWidget:PARENT NE ? AND hWidget:PARENT:TYPE = "BROWSE" THEN 
        cWidgetType = "BROWSE-COLUMN".
      ELSE
        cWidgetType = hWidget:TYPE.

      IF bTranslated THEN DO:
        IF cWidgetType = "button" THEN DO:
          hWidgetRect = hTranManBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE.
          IF NOT VALID-HANDLE(hWidgetRect) THEN DO:
            CREATE RECTANGLE hWidgetRect
                   ASSIGN NAME          = "TranslationMarker_" + hWidget:NAME
                          FRAME         = hWidget:FRAME
                          X             = hWidget:X
                          Y             = hWidget:Y + hWidget:HEIGHT-PIXELS
                          WIDTH-PIXELS  = hWidget:WIDTH-PIXELS
                          HEIGHT-PIXELS = IF hWidget:FRAME:HEIGHT-PIXELS - (hWidget:Y + hWidget:HEIGHT-PIXELS) < 6 THEN
                                            MAX(2,hWidget:FRAME:HEIGHT-PIXELS - (hWidget:Y + hWidget:HEIGHT-PIXELS))
                                          ELSE 6
                          EDGE-PIXELS   = 1
                          GRAPHIC-EDGE  = YES
                          FILLED        = YES
                          BGCOLOR       = 10
                          VISIBLE       = YES
                          .
            ASSIGN hTranManBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE     = hWidgetRect
                   hTranManBuffer:BUFFER-FIELD("iOrgRectBgColor"):BUFFER-VALUE = 10.
            DYNAMIC-FUNCTION("addTranslationMarkerRule",hWidgetRect,hWidget).
          END.
          ELSE ASSIGN hWidgetRect:BGCOLOR = 10
                      hWidgetRect:VISIBLE = YES.
        END.
        ELSE IF cWidgetType = "browse-column" THEN 
          hWidget:LABEL-BGCOLOR = 10.
        ELSE IF CAN-QUERY(hWidget,"bgcolor") THEN
          hWidget:BGCOLOR = 10.
      END.
      ELSE DO:
        IF cWidgetType = "button" THEN DO:
          hWidgetRect = hTranManBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE.
          IF VALID-HANDLE(hWidgetRect) THEN DELETE OBJECT hWidgetRect NO-ERROR.
        END.
        ELSE IF cWidgetType = "browse-column" THEN 
          hWidget:LABEL-BGCOLOR = hTranManBuffer:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE.
        ELSE IF CAN-QUERY(hWidget,"bgcolor") THEN
          hWidget:BGCOLOR = hTranManBuffer:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE.
      END.
    END.
  END.
  cReturnString = cReturnString + (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cLabel"):BUFFER-VALUE ELSE cObjectName) + "|".
END.

IF VALID-HANDLE(hTmpTransBuff) THEN DO:
  IF bAvailWidget THEN DO:      
    hWidgetRect = hTmpTransBuff:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE.
    IF VALID-HANDLE(hWidgetRect) THEN DELETE OBJECT hWidgetRect NO-ERROR.
    hWidget = hWidget:POPUP-MENU NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN DELETE OBJECT hWidget.
    hTmpTransBuff:BUFFER-DELETE().
  END.
  DELETE OBJECT hTmpTransBuff.
END.

IF cReturnString NE "" THEN
  cReturnString = SUBSTR(cReturnString,1,LENGTH(cReturnString) - 1).

IF cReturnString NE "" THEN
  RETURN cReturnString.
ELSE IF NUM-ENTRIES(icObjectNames,"|") = 1 THEN
  RETURN icObjectNames.
ELSE 
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTempTable Procedure 
FUNCTION getTempTable RETURNS HANDLE
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetBuffer   AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieve custom queries
    Notes: If icServerProgram is blank a standard routine is assumed
           
   icParamList: entry(1,icParam,"|"): Table name
                entry(2,icParam,"|"): Query string (optional)
------------------------------------------------------------------------------*/
DEF VAR httTable       AS HANDLE NO-UNDO.
DEF VAR httTableBuffer AS HANDLE NO-UNDO.
DEF VAR httTableQuery  AS HANDLE NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF icServerProgram = "" THEN icServerProgram = "jbserv_gettemptable.p".

IF getAppServiceHandle() NE ? THEN DO:
  RUN VALUE(icServerProgram)
      ON getAppServiceHandle()
     (getSessionId(),
      icParamList,
      OUTPUT TABLE-HANDLE httTable,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      httTable = getTempTable (icServerProgram,icParamList,?).
  END.
END.
ELSE 
  RUN VALUE(icServerProgram)
     (getSessionId(),
      icParamList,
      OUTPUT TABLE-HANDLE httTable,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

CheckReturnValue(cReturnTrans).

IF VALID-HANDLE(httTable) THEN DO:
  cServerTransReturnParam = cReturnTrans.
  IF ihTargetBuffer = ? THEN
    RETURN httTable.
  ELSE DO:
    httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY httTableQuery NO-ERROR.
    httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
    httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
    httTableQuery:QUERY-OPEN.
    httTableQuery:GET-FIRST().
    REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:
      ihTargetBuffer:BUFFER-CREATE().
      ihTargetBuffer:BUFFER-COPY(httTableBuffer).
      httTableQuery:GET-NEXT().
    END.
    DELETE OBJECT httTableQuery.
    DELETE OBJECT httTable.
    RETURN ?.
  END.
END.
ELSE DO:
  IF cReturnTrans = "" THEN cReturnTrans = "Error in " + icServerProgram.
  cServerTransReturnParam = "". 
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTempTableJoin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTempTableJoin Procedure 
FUNCTION getTempTableJoin RETURNS HANDLE
  ( INPUT iiBatchSize        AS INT,
    INPUT iiStartRow         AS INT,
    INPUT icDirection        AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTempTable     AS HANDLE NO-UNDO.
DEF VAR cParamString   AS CHAR   NO-UNDO.
DEF VAR hTmpAppService AS HANDLE NO-UNDO.

IF icQueryCriteria = ? THEN 
  icQueryCriteria = "WHERE FALSE".

IF cQueryLogFile  NE "" THEN cParamString = "logfile;" + cQueryLogFile.
IF cCalcFieldProc NE "" THEN cParamString = TRIM(cParamString + ",calcfieldproc;" + REPLACE(cCalcFieldProc,",",";"),",").
IF bIndexOnRowids THEN 
  cParamString   = TRIM(cParamString + ",indexonrowids;yes",",").
IF cPreScanQuery NE "" THEN
  cParamString = TRIM(cParamString + ",prescanquery;" + REPLACE(cPreScanQuery,",","¤"),",").
IF cCalcFieldFilter NE "" THEN
  cParamString = TRIM(cParamString + ",calcfieldfilter;" + cCalcFieldFilter).
IF iPreScanLimit NE 0 THEN
  cParamString = TRIM(cParamString + ",prescanlimit;" + STRING(iPreScanLimit)).
IF bReturnQueryInfo THEN
  cParamString = TRIM(cParamString + ",returnqueryinfo;yes",",").
IF cNoExistBuffers NE "" THEN
  cParamString = TRIM(cParamString + ",noexistbuffers;" + REPLACE(cNoExistBuffers,",","¤"),",").
IF cSkipCalcFields NE "" THEN
  cParamString = TRIM(cParamString + ",skipcalcfields;" + cSkipCalcFields,",").
IF cUniqueBuffer NE "" THEN
  cParamString = TRIM(cParamString + ",uniqueBuffer;" + cUniqueBuffer,",").
IF cOrgBuffersAndFields NE "" THEN  
  cParamString = TRIM(cParamString + ",orgBuffersAndFields;" + REPLACE(REPLACE(cOrgBuffersAndFields,",","¤"),";",CHR(1)),",").
IF cQueryStopAfter ne "" THEN
  cParamString = TRIM(cParamString + ",queryStopAfter;" + cQueryStopAfter,",").

bOK = SESSION:SET-WAIT-STATE("general").

IF NOT icQueryCriteria BEGINS "where false" THEN
  PUBLISH "setProgBarProperty" ("startQuery","").

hTmpAppService = getAppServiceHandle().

IF CAN-FIND(FIRST tt_field) 
   AND icQueryCriteria BEGINS "where false" 
/*    AND bIndexOnRowids                */
/*    AND bReturnQueryInfo              */
/*    AND PROGRAM-NAME(2) BEGINS "New"  */
   AND NOT icBuffersAndFields BEGINS "_fi"
   THEN 
  RUN CreateQueryFromCache
     (icBuffersAndFields,
      icQueryCriteria,
      OUTPUT hTempTable,
      OUTPUT cReturnTrans)
      NO-ERROR.
ELSE IF hTmpAppService NE ? THEN DO:
  RUN jbserv_gettemptablejoin.p
      ON hTmpAppService
/*       ON getAppServiceHandle() */
     (getSessionId(),
      iiBatchSize,
      iiStartRow,
      icDirection,
      icBuffersAndFields,
      icQueryCriteria,
      cQueryStatFields,
      cParamString,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cQueryStatFieldValues,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      hTempTable = getTempTableJoin (iiBatchSize,iiStartRow,icDirection,icBuffersAndFields,icQueryCriteria).
  END.
END.
ELSE
  RUN jbserv_gettemptablejoin.p
     (getSessionId(),
      iiBatchSize,
      iiStartRow,
      icDirection,
      icBuffersAndFields,
      icQueryCriteria,
      cQueryStatFields,
      cParamString,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cQueryStatFieldValues,
      OUTPUT cReturnTrans)
      NO-ERROR.

bOK = SESSION:SET-WAIT-STATE("").
IF NOT icQueryCriteria BEGINS "where false" THEN
  PUBLISH "setProgBarProperty" ("endQuery","").

CheckReturnValue(cReturnTrans).

ASSIGN cQueryStatFields = ""
       cCalcFieldProc   = ""
       cPreScanQuery    = ""
       cCalcFieldFilter = ""
       iPreScanLimit    = 0
       bReturnQueryInfo = NO
       cNoExistBuffers  = ""
       bIndexOnRowids   = NO
       cSkipCalcFields  = ""
       cUniqueBuffer    = ""
       cQueryStopAfter  = ""
       .

RETURN hTempTable.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotal Procedure
FUNCTION getTotal RETURNS DECIMAL 
  ( INPUT icBuffer         AS CHAR,
    INPUT icQueryCriteria  AS CHAR,
    INPUT icFieldName      AS CHAR) :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR fTotal AS DECIMAL NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_gettotal.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffer,
      icQueryCriteria,
      icFieldName,
      OUTPUT fTotal,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      fTotal = getTotal (icBuffer,icQueryCriteria,icFieldName).
  END.
END.
ELSE 
  RUN jbserv_gettotal.p
     (DYNAMIC-FUNCTION("getSessionId"),
      icBuffer,
      icQueryCriteria,
      icFieldName,
      OUTPUT fTotal,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

RETURN fTotal.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getTransactionMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTransactionMessage Procedure 
FUNCTION getTransactionMessage RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cReturnTrans.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransactionRowIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTransactionRowIds Procedure 
FUNCTION getTransactionRowIds RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Pass rowids created in previous DoCommit 
    Notes: N/A for update/delete 
------------------------------------------------------------------------------*/
DEF VAR cRowIds AS CHAR NO-UNDO.

FOR EACH ttTransRecord NO-LOCK:
  cRowIds = cRowIds + ttTransRecord.cTransRecordRowid + ",".
END.
RETURN TRIM(cRowIds,","). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTranslation Procedure 
FUNCTION getTranslation RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE,
    INPUT iiLevel  AS INT):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bTranslated       AS LOG    NO-UNDO.
DEF VAR cWidgetSourceFile AS CHAR   NO-UNDO.
DEF VAR hWidgetRect       AS HANDLE NO-UNDO.
DEF VAR cModWidgetName    AS CHAR   NO-UNDO.

REPEAT WHILE ihWidget NE ?:
  IF CAN-DO("BROWSE,BUTTON,COMBO-BOX,DIALOG-BOX,FILL-IN,EDITOR,FRAME,MENU-ITEM,RADIO-SET,SELECTION-LIST,SLIDER,SUB-MENU,TEXT,TOGGLE-BOX,WINDOW",ihWidget:TYPE) 
     AND (IF ihWidget:TYPE NE "window" THEN ihWidget:NAME NE ? ELSE TRUE) THEN DO:

    IF ihWidget:PARENT NE ? AND ihWidget:PARENT:TYPE = "BROWSE" THEN 
      cWidgetType = "BROWSE-COLUMN".
    ELSE IF ihWidget:TYPE = "text" AND ihWidget:SIDE-LABEL-HANDLE NE ? THEN
      cWidgetType = "TEXT-FILL-IN".
    ELSE
      cWidgetType = ihWidget:TYPE.

    cWidgetSourceFile = DYNAMIC-FUNCTION("getWidgetSourceFile",ihWidget).
    IF cWidgetSourceFile = "" THEN
      cWidgetSourceFile = cTransSourceFile.

    IF ihWidget:TYPE = "BUTTON" AND SUBSTR(ihWidget:NAME,1,4) = "btn_" AND R-INDEX(ihWidget:NAME,"_") > 4 THEN
      cWidgetName = SUBSTR(ihWidget:NAME,1,R-INDEX(ihWidget:NAME,"_") - 1).
    ELSE IF ihWidget:TYPE = "window" THEN 
      cWidgetName = cTransSourceFile.
    ELSE
      cWidgetName = ihWidget:NAME.

    IF bDynamicsTranslation THEN DO:
      IF cWidgetType <> "BROWSE" THEN 
        DYNAMIC-FUNCTION("setDynamicsTranslation",ihWidget,cWidgetName,
                         (IF cWidgetType = "BROWSE-COLUMN" THEN "BROWSE" ELSE cWidgetType),
                         cWidgetSourceFile,
                         (IF cWidgetType = "browse-column" THEN DYNAMIC-FUNCTION("getAttribute",ihWidget:PARENT,"fieldbuffer" + ihWidget:NAME) ELSE "")).    
    END.
    ELSE IF NOT cWidgetName BEGINS "TranslationMarker_" AND NOT (cWidgetName = "Translate" AND cWidgetSourceFile = "JBoxTranMan.w") THEN DO:
      
      bTranslated = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
                                          " AND cFileName   = '" + cWidgetSourceFile + "'" +
                                          " AND cObjectType = '" + cWidgetType + "'") NO-ERROR.
      IF NOT bTranslated THEN
        bTranslated = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
                                            " AND cFileName   = ''" +
                                            " AND cObjectType = '" + cWidgetType + "'") NO-ERROR.
      IF NOT bTranslated THEN
        bTranslated = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
                                            " AND cFileName   = ''" +
                                            " AND cObjectType = ''") NO-ERROR.
      IF NOT bTranslated THEN
        bTranslated = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cWidgetName + "'" +
                                            " AND cObjectType = 'DBFIELD'") NO-ERROR.
    
      IF NOT bTranslated AND ((cWidgetType = "button" AND INDEX(cWidgetName,"_btn_") > 0) OR (cWidgetType = "menu-item" AND INDEX(cWidgetName,"_mitm_") > 0)) THEN DO:
        IF cWidgetType = "button" THEN
          cModWidgetName = SUBSTR(cWidgetName,INDEX(cWidgetName,"btn_")).
        ELSE
          cModWidgetName = SUBSTR(cWidgetName,INDEX(ihWidget:NAME,"mitm_")).
          
        bTranslated = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cModWidgetName + "'" +
                                              " AND cFileName   = ''" +
                                              " AND cObjectType = '" + cWidgetType + "'") NO-ERROR.
        IF NOT bTranslated THEN
          bTranslated = hBuffTranslation:FIND-FIRST("WHERE cObjectName = '" + cModWidgetName + "'" +
                                              " AND cFileName   = ''" +
                                              " AND cObjectType = ''") NO-ERROR.
      END.

      IF cTranslationReportFile NE "" THEN DO:
        IF NOT ((cWidgetType = "menu-item" AND cWidgetName = "Translate") OR CAN-DO("jbCountDistinc,jbAccumValue",cWidgetName) OR cWidgetName BEGINS "TranslationMarker_") THEN
          PUT UNFORMATTED 
              cWidgetName  ";"  
              (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cFileName"):BUFFER-VALUE ELSE "") ";" 
              cWidgetType ";"
              STRING(bTranslated) ";"
              (IF CAN-QUERY(ihWidget,"label") AND ihWidget:LABEL NE ? THEN ihWidget:LABEL ELSE "") ";" 
              (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cLabel"):BUFFER-VALUE ELSE "") ";" 
              (IF CAN-QUERY(ihWidget,"tooltip") AND ihWidget:TOOLTIP NE ? THEN ihWidget:TOOLTIP ELSE "") ";" 
              (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cTooltip"):BUFFER-VALUE ELSE "") ";" 
              (IF CAN-QUERY(ihWidget,"help") AND ihWidget:HELP NE ? THEN ihWidget:HELP ELSE "") ";" 
              (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cHelpText"):BUFFER-VALUE ELSE "") ";" 
              (IF CAN-QUERY(ihWidget,"title") AND ihWidget:TITLE NE ? THEN ihWidget:TITLE ELSE "") ";" 
              (IF bTranslated THEN hBuffTranslation:BUFFER-FIELD("cTitle"):BUFFER-VALUE ELSE "") ";" 
              SKIP.
      END.
      ELSE IF VALID-HANDLE(hTranManBuffer) THEN DO:
        bOk = hTranManBuffer:FIND-FIRST("WHERE chWidget = '" + STRING(ihWidget) + "'") NO-ERROR.
        IF NOT bOk THEN DO:
          hTranManBuffer:BUFFER-CREATE().
          ASSIGN hTranManBuffer:BUFFER-FIELD("cToolTip"):BUFFER-VALUE    = IF CAN-QUERY(ihWidget,"TOOLTIP") THEN ihWidget:TOOLTIP ELSE ""
                 hTranManBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE      = IF ihWidget:TYPE = "RADIO-SET" THEN ihWidget:RADIO-BUTTONS
                                                                           ELSE IF cWidgetType = "BROWSE-COLUMN" THEN
                                                                             DYNAMIC-FUNCTION("getStrippedSortLabel",ihWidget)
                                                                           ELSE IF CAN-QUERY(ihWidget,"LABEL") THEN ihWidget:LABEL                                                            
                                                                           ELSE ""
                 hTranManBuffer:BUFFER-FIELD("cTitle"):BUFFER-VALUE      = IF CAN-QUERY(ihWidget,"TITLE") THEN 
                                                                             IF ihWidget:TITLE MATCHES "*[*" THEN SUBSTR(ihWidget:TITLE,1,INDEX(ihWidget:TITLE,"[") - 2)
                                                                             ELSE ihWidget:TITLE
                                                                           ELSE ""
                 hTranManBuffer:BUFFER-FIELD("cInitValue"):BUFFER-VALUE  = IF cWidgetType = "TEXT" THEN ihWidget:SCREEN-VALUE 
                                                                           ELSE ""
                 hTranManBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE = cWidgetName
                 hTranManBuffer:BUFFER-FIELD("cObjectType"):BUFFER-VALUE = cWidgetType
                 hTranManBuffer:BUFFER-FIELD("chWidget"):BUFFER-VALUE    = STRING(ihWidget)
                 hTranManBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE      = NO
                 hTranManBuffer:BUFFER-FIELD("cLanguage"):BUFFER-VALUE   = DYNAMIC-FUNCTION("getBaseLanguageCode")
                 hTranManBuffer:BUFFER-FIELD("hWidget"):BUFFER-VALUE     = ihWidget
                 hTranManBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE   = cWidgetSourceFile
                 hTranManBuffer:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE = ihWidget:BGCOLOR                                                                            
                 .
        END.
        ELSE IF NOT bTranslated THEN
          setWidgetTranslation(hTranManBuffer,ihWidget).
    
        IF bTranslated THEN DO:
          hTranManBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = YES.
          IF cWidgetType = "button" THEN DO:
            hWidgetRect = hTranManBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE.
            IF NOT VALID-HANDLE(hWidgetRect) THEN DO:
              CREATE RECTANGLE hWidgetRect
                     ASSIGN NAME          = "TranslationMarker_" + ihWidget:NAME
                            FRAME         = ihWidget:FRAME
                            X             = ihWidget:X
                            Y             = ihWidget:Y + ihWidget:HEIGHT-PIXELS
                            WIDTH-PIXELS  = ihWidget:WIDTH-PIXELS
                            HEIGHT-PIXELS = IF ihWidget:FRAME:HEIGHT-PIXELS - (ihWidget:Y + ihWidget:HEIGHT-PIXELS) < 6 THEN
                                              MAX(2,ihWidget:FRAME:HEIGHT-PIXELS - (ihWidget:Y + ihWidget:HEIGHT-PIXELS))
                                            ELSE 6
                            EDGE-PIXELS   = 1
                            GRAPHIC-EDGE  = YES
                            FILLED        = YES
                            BGCOLOR       = 10
                            VISIBLE       = YES
                            .
              ASSIGN hTranManBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE     = hWidgetRect
                     hTranManBuffer:BUFFER-FIELD("iOrgRectBgColor"):BUFFER-VALUE = 10.
              DYNAMIC-FUNCTION("addTranslationMarkerRule",hWidgetRect,ihWidget).
            END.
            ELSE ASSIGN hWidgetRect:BGCOLOR = 10
                        hWidgetRect:VISIBLE = YES.
          END.
          ELSE IF cWidgetType = "browse-column" THEN 
            ihWidget:LABEL-BGCOLOR = 10.
          ELSE IF CAN-QUERY(ihWidget,"bgcolor") THEN
            ihWidget:BGCOLOR = 10.
  
          setWidgetTranslation(hBuffTranslation,ihWidget).
        END.
        ELSE DO:
          hTranManBuffer:BUFFER-FIELD("bExist"):BUFFER-VALUE = NO.
          IF cWidgetType = "button" THEN DO:
            hWidgetRect = hTranManBuffer:BUFFER-FIELD("hWidgetRect"):BUFFER-VALUE.
            IF VALID-HANDLE(hWidgetRect) THEN DELETE OBJECT hWidgetRect NO-ERROR.
          END.
          ELSE IF cWidgetType = "browse-column" THEN 
            ihWidget:LABEL-BGCOLOR = hTranManBuffer:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE NO-ERROR.
          ELSE IF CAN-QUERY(ihWidget,"bgcolor") THEN
            ihWidget:BGCOLOR = hTranManBuffer:BUFFER-FIELD("iOrgBgColor"):BUFFER-VALUE NO-ERROR.
        END.
    
        IF NOT hTranManBuffer:BUFFER-FIELD("bPopupDefined"):BUFFER-VALUE AND 
           CAN-QUERY(ihWidget,"popup-menu") AND NOT CAN-DO("window,frame",cWidgetType) THEN DO:
          DYNAMIC-FUNCTION("setObjectSourceProc",hTranManProc).
          DYNAMIC-FUNCTION("NewMenuBand",ihWidget,"Translate","").
          hTranManBuffer:BUFFER-FIELD("bPopupDefined"):BUFFER-VALUE = YES.
        END.
    
        IF NOT CAN-DO(cTransObjectTypeList,cWidgetType) THEN
          cTransObjectTypeList = cTransObjectTypeList + "," + cWidgetType.
    
      END.
      ELSE IF bTranslated THEN
        setWidgetTranslation(hBuffTranslation,ihWidget).
    
    END.

    IF CAN-QUERY(ihWidget,'MENU-BAR') AND ihWidget:MENU-BAR NE ? THEN
      getTranslation(ihWidget:MENU-BAR:FIRST-CHILD,iiLevel + 1).
    ELSE IF CAN-QUERY(ihWidget,'MENU-BAR') THEN DO:
      hWidgetRect = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",ihWidget,"MENUBAR")) NO-ERROR.
      IF VALID-HANDLE(hWidgetRect) THEN        
        getTranslation(hWidgetRect:FIRST-CHILD,iiLevel + 1).
    END.

    IF CAN-QUERY(ihWidget,'FIRST-COLUMN') AND ihWidget:FIRST-COLUMN NE ? THEN
      getTranslation(ihWidget:FIRST-COLUMN,iiLevel + 1).
    IF CAN-QUERY(ihWidget,'NEXT-COLUMN') AND ihWidget:NEXT-COLUMN NE ? THEN
      getTranslation(ihWidget:NEXT-COLUMN,iiLevel + 1).
  END.
      
  IF CAN-QUERY(ihWidget,'FIRST-CHILD') AND ihWidget:FIRST-CHILD  <> ? THEN
    getTranslation(ihWidget:FIRST-CHILD,iiLevel + 1).

  IF CAN-QUERY(ihWidget,'POPUP-MENU') AND ihWidget:POPUP-MENU NE ? THEN
    getTranslation(ihWidget:POPUP-MENU:FIRST-CHILD,iiLevel + 1).

  IF iiLevel > 0 AND CAN-QUERY(ihWidget,'NEXT-SIBLING') THEN ihWidget = ihWidget:NEXT-SIBLING.
  ELSE LEAVE.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTranslationBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTranslationBuffer Procedure 
FUNCTION getTranslationBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBuffTranslation.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransObjectTypeList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTransObjectTypeList Procedure 
FUNCTION getTransObjectTypeList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cTransObjectTypeList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransRecordBufferHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTransRecordBufferHandle Procedure 
FUNCTION getTransRecordBufferHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN BUFFER ttTransRecord:HANDLE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTTJoinOnServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTTJoinOnServer Procedure 
FUNCTION getTTJoinOnServer RETURNS LOGICAL
  ( INPUT icDirection        AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR,
    INPUT icServerProgram    AS CHAR,
    INPUT icParamList        AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Start a server routine that will query the database first and
           then pass the result set (by buffer-handle) to a BL routine using 
           same parameters as runproc  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTempTable    AS HANDLE NO-UNDO.
DEF VAR obOK          AS LOG    NO-UNDO. 
DEF VAR cParamString  AS CHAR   NO-UNDO.

IF icQueryCriteria = ? THEN 
  icQueryCriteria = "WHERE FALSE".

IF cQueryLogFile  NE "" THEN cParamString = "logfile;" + cQueryLogFile.
IF cCalcFieldProc NE "" THEN cParamString = TRIM(cParamString + ",calcfieldproc;" + cCalcFieldProc,",").
IF bIndexOnRowids THEN 
  ASSIGN cParamString   = TRIM(cParamString + ",indexonrowids;yes",",")
         bIndexOnRowids = NO.
IF cPreScanQuery NE "" THEN
  cParamString = TRIM(cParamString + ",prescanquery;" + REPLACE(cPreScanQuery,",","¤"),",").
IF cCalcFieldFilter NE "" THEN
  cParamString = TRIM(cParamString + ",calcfieldfilter;" + cCalcFieldFilter).
IF iPreScanLimit NE 0 THEN
  cParamString = TRIM(cParamString + ",prescanlimit;" + STRING(iPreScanLimit)).
IF bReturnQueryInfo THEN
  cParamString = TRIM(cParamString + ",returnqueryinfo;yes",",").

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO: 
  RUN jbserv_processquery.p
      ON getAppServiceHandle()
     (getSessionId(),
      icDirection,
      icBuffersAndFields,
      icQueryCriteria,
      cQueryStatFields,
      cParamString,
      icServerProgram,
      icParamList,
      OUTPUT cQueryStatFieldValues,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      obOk = getTTJoinOnServer (icDirection,icBuffersAndFields,icQueryCriteria,icServerProgram,icParamList).
  END.
END.
ELSE 
  RUN jbserv_processquery.p
     (getSessionId(),
      icDirection,
      icBuffersAndFields,
      icQueryCriteria,
      cQueryStatFields,
      cParamString,
      icServerProgram,
      icParamList,
      OUTPUT cQueryStatFieldValues,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      NO-ERROR.

bOK = SESSION:SET-WAIT-STATE("").

CheckReturnValue(cReturnTrans).

ASSIGN cQueryStatFields = ""
       cCalcFieldProc   = ""
       cPreScanQuery    = ""
       cCalcFieldFilter = ""
       iPreScanLimit    = 0
       bReturnQueryInfo = NO
       .

RETURN obOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserLevel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUserLevel Procedure 
FUNCTION getUserLevel RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cUserLevel.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUserSetting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUserSetting Procedure 
FUNCTION getUserSetting RETURNS CHARACTER
  ( INPUT icSourceFile     AS CHAR,
    INPUT icObjectName     AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icSettingName    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieve user setting / attribute
------------------------------------------------------------------------------*/
DEF VAR cSettingList       AS CHAR   NO-UNDO.
DEF VAR ocReturn           AS CHAR   NO-UNDO.
DEF VAR hQuery             AS HANDLE NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

FIND FIRST ttCache
     WHERE ttCache.cTable = "JBoxUserSetting"
     NO-ERROR.
IF AVAIL ttCache THEN DO:
  IF NOT VALID-HANDLE(ttCache.hBuffer) THEN DO:
    IF NOT BuildTableCache("JBoxUserSetting|WHERE cJBoxUserId = '" + cASuserId + "'") THEN
      MESSAGE "Failed to rebuild cache for user settings before read"
              VIEW-AS ALERT-BOX WARNING.
    ELSE FIND CURRENT ttCache.
  END.
  IF VALID-HANDLE(ttCache.hBuffer) THEN DO:
    IF icSourceFile = "" AND icObjectName = "" AND icContext = "" AND icSettingName = "" THEN
      RETURN "Invalid qualifier".

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(ttCache.hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ttCache.hBuffer:NAME
                        + " WHERE cSourceFile  = '" + icSourceFile + "'"
                        + "   AND cObjectName  = '" + icObjectName + "'"
                        + "   AND cContext     = '" + icContext + "'"
                        + "   AND CAN-DO('" + icSettingName + "',cSettingName)"
                        + "   AND cJBoxUserId  = '" + cASUserId + "'"
                        + (IF cUserSettingSortExp NE "" THEN ' BY ' + LEFT-TRIM(cUserSettingSortExp,'BY ') ELSE '')
                          ) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN DO:
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST().
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        ASSIGN ocReturn     = ocReturn + ttCache.hBuffer:BUFFER-FIELD("cSetting"):BUFFER-VALUE + CHR(1)
               cSettingList = cSettingList + ttCache.hBuffer:BUFFER-FIELD("cSettingName"):BUFFER-VALUE + ",".
        hQuery:GET-NEXT().
      END.
    END.
    ELSE bOk = NO.
    
    ASSIGN ocReturn     = SUBSTR(ocReturn,1,LENGTH(ocReturn) - 1)
           cSettingList = TRIM(cSettingList,",")
           cUserSettingSortExp = "". 
    
    IF hQuery:NUM-RESULTS > 1 THEN 
      ocReturn = cSettingList + "|" + ocReturn.
    ELSE 
      cReturnTrans = cSettingList.

    DELETE OBJECT hQuery.

    IF ocReturn NE "" THEN
      RETURN ocReturn.
  END.
END.

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getusersetting.p
      ON getAppServiceHandle()
     (getSessionId(),
      icSourceFile,
      icObjectName,
      icContext,
      icSettingName,
      OUTPUT cSettingList)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF NOT ERROR-STATUS:GET-NUMBER(1) = 293 AND ReconnectServer() THEN 
      cSettingList = getUserSetting (icSourceFile,icObjectName,icContext,icSettingName).
  END.
END.
ELSE 
  RUN jbserv_getusersetting.p
     (getSessionId(),
      icSourceFile,
      icObjectName,
      icContext,
      icSettingName,
      OUTPUT cSettingList)
      NO-ERROR.

bOK = SESSION:SET-WAIT-STATE("").

cReturnTrans = RETURN-VALUE.

RETURN cSettingList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initTranslation Procedure 
FUNCTION initTranslation RETURNS LOGICAL
  ( INPUT ihWidget  AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Fields prefixed with ! are added to the buffer but not to the grid  
------------------------------------------------------------------------------*/
DEF VAR httDummy            AS HANDLE NO-UNDO.
DEF VAR cCustomFileNameList AS CHAR   NO-UNDO.
DEF VAR cCustomObjTypeList  AS CHAR   NO-UNDO.
DEF VAR cCustomObjNameList  AS CHAR   NO-UNDO.
DEF VAR cCustomObjLabelList AS CHAR   NO-UNDO.

IF cTransSourceFile = "" THEN
  cTransSourceFile = SOURCE-PROCEDURE:FILE-NAME.

IF cTransSourceFile MATCHES "*.ab" THEN
    cTransSourceFile = ENTRY(2,SUBSTR(cTransSourceFile,1,LENGTH(cTransSourceFile) - 3),"_") + ".w".

IF VALID-HANDLE(hTranManProc) THEN DO:
  hTranManBuffer = DYNAMIC-FUNCTION("getTranManBuffer" IN hTranManProc). 
  hTranManBuffer:EMPTY-TEMP-TABLE().
  SOURCE-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
END.

bOK = SESSION:SET-WAIT-STATE("general").

IF bDynamicsTranslation THEN DO:    
  bOk = DYNAMIC-FUNCTION("initDynamicsTranslation") NO-ERROR.
  IF bOk THEN DO:      
    getTranslation(ihWidget,0).
    DYNAMIC-FUNCTION("getDynamicsTranslation").
  END.
END.
ELSE DO:    
  /* Only to speed up things when running from AppBuilder (the translation buffer needs to be defined): */
  IF NOT VALID-HANDLE(hBuffTranslation) AND NUM-ENTRIES(cAppLanguages,"|") > 1 THEN DO:
    IF BuildTableCache("JBoxTranslation|WHERE cLanguage = '" + cLanguage + "' AND iMsgNo = 0") THEN DO:
      FIND FIRST ttCache 
           WHERE ttCache.cTableName = "JBoxTranslation" 
           NO-ERROR.
      IF AVAIL ttCache THEN
        hBuffTranslation = ttCache.hBuffer.
      ELSE 
        MESSAGE "Missing table JBoxTranslation" SKIP
                "If the application is configured for multiple languages this table must be installed" SKIP
                "(Otherwise check the setLanguages function in your startup procedure)"
                VIEW-AS ALERT-BOX ERROR.
    END.
    ELSE
      MESSAGE "Failed to build cache for table JBoxTranslation" SKIP
              "If the application is configured for multiple languages this table must be installed" SKIP
              "(Otherwise check the setLanguages function in your startup procedure)"
              VIEW-AS ALERT-BOX ERROR.
  END.
  
  IF NUM-ENTRIES(cAppLanguages,"|") > 1 AND VALID-HANDLE(hBuffTranslation) THEN DO:
    IF cTranslationReportFile NE "" THEN DO:
      OUTPUT TO VALUE(cTranslationReportFile).
      PUT UNFORMATTED 
          "Object name;"  
          "File name;" 
          "Object type;"
          "Translated;"
          "Org.label;" 
          "Trans.label;"
          "Org.tooltip;"
          "Trans.tooltip;"
          "Org.helptext;"
          "Org.title;"
          "Trans.title;"
          SKIP.
    END.
    cTransObjectTypeList = ",DBFIELD".
  
    getTranslation(ihWidget,0).
  
    PUBLISH "InitStringTranslation" (ihWidget:WINDOW,INPUT-OUTPUT cTransObjectTypeList). 
  
    IF cTranslationReportFile NE "" THEN DO:
      cTranslationReportFile = "".
      OUTPUT CLOSE.
    END.
    ELSE IF VALID-HANDLE(hTranManProc) THEN
      RUN BuildScreenObjects IN hTranManProc (SOURCE-PROCEDURE:CURRENT-WINDOW).
  
    ASSIGN hTranManBuffer   = ?
           cTransSourceFile = ""
           hTranManProc     = ?.
  END.
END.

bOK = SESSION:SET-WAIT-STATE("").

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IsFieldNameInTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION IsFieldNameInTable Procedure 
FUNCTION IsFieldNameInTable RETURNS LOGICAL
  ( INPUT cBufferName  AS CHAR,
    INPUT cColumn      AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bExist AS LOG NO-UNDO.

IF getAppServiceHandle() NE ? THEN
  RUN jbserv_getisfieldintable.p
      ON getAppServiceHandle()
     (DYNAMIC-FUNCTION("getSessionId"),
      cBufferName,
      cColumn,
      OUTPUT bExist)
      .
ELSE 
  RUN jbserv_getisfieldintable.p
     (DYNAMIC-FUNCTION("getSessionId"),
      cBufferName,
      cColumn,
      OUTPUT bExist)
      .

RETURN bExist.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MakeEventLogEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MakeEventLogEntry Procedure 
FUNCTION MakeEventLogEntry RETURNS LOGICAL
  ( INPUT iiEventId        AS INT,
    INPUT icDescription    AS CHAR,
    INPUT icGroup          AS CHAR,
    INPUT icType           AS CHAR,
    INPUT icSource         AS CHAR,
    INPUT icCategory       AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Create an event log entry
------------------------------------------------------------------------------*/

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_seteventline.p
      ON getAppServiceHandle()
     (getSessionId(),
      iiEventId,    
      icDescription,
      icGroup,      
      icType,       
      icSource,     
      icCategory)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF NOT ERROR-STATUS:GET-NUMBER(1) = 293 AND ReconnectServer() THEN 
      RETURN MakeEventLogEntry (iiEventId,    
                                icDescription,
                                icGroup,      
                                icType,       
                                icSource,     
                                icCategory).
  END.
END.
ELSE 
  RUN jbserv_seteventline.p
     (getSessionId(),
      iiEventId,    
      icDescription,
      icGroup,      
      icType,       
      icSource,     
      icCategory)
      NO-ERROR.

bOK = SESSION:SET-WAIT-STATE("").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OnlyCalcFldsInBufferDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OnlyCalcFldsInBufferDef Procedure 
FUNCTION OnlyCalcFldsInBufferDef RETURNS LOGICAL
  ( INPUT icBufferDef AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Check if the only fields in a definition for a query buffer are calculated
           If so, also all other fields should be added to the returned temp-table 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

IF NUM-ENTRIES(icBufferDef,";") = 1 THEN RETURN NO.

DO ix = 2 TO NUM-ENTRIES(icBufferDef,";"):
  IF NOT (ENTRY(ix,icBufferDef,";") BEGINS "+" OR SUBSTR(ENTRY(1,ENTRY(ix,icBufferDef,";"),"|"),LENGTH(ENTRY(1,ENTRY(ix,icBufferDef,";"),"|")),1) = "]") THEN 
    RETURN NO.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION putTempTable Procedure 
FUNCTION putTempTable RETURNS CHARACTER
  ( INPUT icServerProgram  AS CHAR,
    INPUT TABLE-HANDLE     ihTempTable,
    INPUT icParamList      AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieve custom queries
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cOutParam  AS CHAR NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN VALUE(icServerProgram)
      ON getAppServiceHandle()
     (getSessionId(),
      icParamList,
      TABLE-HANDLE ihTempTable,
      OUTPUT cOutParam,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      cOutParam = putTempTable (icServerProgram,TABLE-HANDLE ihTempTable,icParamList).
  END.
END.
ELSE 
  RUN VALUE(icServerProgram)
     (getSessionId(),
      icParamList,
      TABLE-HANDLE ihTempTable,
      OUTPUT cOutParam,
      OUTPUT cReturnTrans)
      .

bOK = SESSION:SET-WAIT-STATE("").

RETURN cOutParam.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-QueryUsesIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION QueryUsesIndex Procedure 
FUNCTION QueryUsesIndex RETURNS LOGICAL
  ( INPUT icBufferList  AS CHAR,
    INPUT icQueryString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bIndex  AS LOG NO-UNDO.

IF icQueryString = "" THEN RETURN ?.

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_queryindexusage.p
      ON getAppServiceHandle()
     (getSessionId(),
      icBufferList,
      icQueryString,
      OUTPUT bIndex,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      bIndex = QueryUsesIndex (icBufferList,icQueryString).
  END.
END.
ELSE 
  RUN jbserv_queryindexusage.p
     (getSessionId(),
      icBufferList,
      icQueryString,
      OUTPUT bIndex,
      OUTPUT cReturnTrans)
      NO-ERROR.

CheckReturnValue(cReturnTrans).

IF cReturnTrans NE "" THEN
  RETURN ?.
ELSE RETURN bIndex.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReconnectServer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReconnectServer Procedure 
FUNCTION ReconnectServer RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:   
    Notes:  GOO Jan 2017: reconnectPause/Attempts
------------------------------------------------------------------------------*/
DEF VAR hServer AS HANDLE NO-UNDO.
DEFINE VARIABLE iCount  AS INTEGER NO-UNDO.

ASSIGN
    iReConnectPause    = IF iReConnectPause = ? THEN int(DYNAMIC-FUNCTION('getAttribute',SESSION,'ReConnectPause')) ELSE iReConnectPause
    iReConnectAttempts = IF iReConnectAttempts = ? THEN int(DYNAMIC-FUNCTION('getAttribute',SESSION,'ReConnectAttempts')) ELSE iReConnectAttempts
    no-error.
if error-status:error then
do:
ASSIGN
  iReConnectPause    = 0
  iReConnectAttempts = 1
  no-error.
end.

CREATE SERVER hServer.
IF cAppServiceConnectString NE "" THEN
  hServer:CONNECT(cAppServiceConnectString) NO-ERROR.
ELSE
  hServer:CONNECT(ENTRY(1,SESSION:PARAM,";")) NO-ERROR.
IF hServer:CLIENT-CONNECTION-ID = "" THEN DO:
  DO iCount = 1 TO iReConnectAttempts:
    PAUSE iReConnectPause.
    hServer:CONNECT(ENTRY(1,SESSION:PARAM,";")) NO-ERROR.
    IF hServer:CLIENT-CONNECTION-ID NE "" THEN DO:
      DYNAMIC-FUNCTION("setAppserviceHandle",hServer).
      DYNAMIC-FUNCTION("setAppserviceId",ENTRY(NUM-ENTRIES(hServer:CLIENT-CONNECTION-ID,":"),hServer:CLIENT-CONNECTION-ID,":")).
      RETURN TRUE.
    END.
  END.
  MESSAGE "Server connection lost when trying to re-connect. Try re-login"
    VIEW-AS ALERT-BOX ERROR.
  QUIT.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAppserviceHandle",hServer).
  DYNAMIC-FUNCTION("setAppserviceId",ENTRY(NUM-ENTRIES(hServer:CLIENT-CONNECTION-ID,":"),hServer:CLIENT-CONNECTION-ID,":")).
  RETURN TRUE.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-refreshFromTransRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION refreshFromTransRecord Procedure 
FUNCTION refreshFromTransRecord RETURNS LOGICAL
  ( INPUT ihBuffer           AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  Refresh the query buffer from the transaction buffer 
    Notes:  Typical use: After mass update from a browse.
            NB! this method is not sufficient when need of recalculation of calc. field or the 
            updated values are foreign keys (and the query has a join with the key).
            If this is needed, re-open the query (against the server)
------------------------------------------------------------------------------*/
DEF VAR hField AS HANDLE NO-UNDO.

FOR EACH ttTransAction
    WHERE ttTransAction.cTransActionBuffer = ihBuffer:NAME,
    EACH ttTransRecord WHERE ttTransRecord.iTransActionId = ttTransAction.iTransActionId:

  IF ttTransAction.cTransAction = "create" THEN DO:
    ihBuffer:BUFFER-CREATE().
    DO ix = 1 TO NUM-ENTRIES(cTransActionIdFields):
      assignStringValue(ihBuffer:BUFFER-FIELD(ENTRY(ix,cTransActionIdFields)),ENTRY(ix,cTransRecordIdValues,"|")). 
    END.
  END.
  ELSE bOK = ihBuffer:FIND-FIRST("WHERE RowIdent1 = '" + cTransRecordRowid + "'") NO-ERROR.

  IF bOK THEN DO:
    IF ttTransAction.cTransAction NE "delete" THEN DO:
      DO ix = 1 TO NUM-ENTRIES(ttTransAction.cTransActionValueFields):
        assignStringValue(ihBuffer:BUFFER-FIELD(ENTRY(ix,cTransActionValueFields)),ENTRY(ix,cTransRecordValues,"|")). 
      END.
      DO ix = 1 TO NUM-ENTRIES(ttTransRecord.cReturnExtraFields):
        hField = ihBuffer:BUFFER-FIELD(ENTRY(ix,cReturnExtraFields)) NO-ERROR.
        IF VALID-HANDLE(hField) THEN
          assignStringValue(hField,ENTRY(ix,cReturnExtraValues,"|")). 
      END.
    END.
    ELSE ihBuffer:BUFFER-DELETE().
  END.
  ELSE RETURN FALSE.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-refreshRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION refreshRow Procedure 
FUNCTION refreshRow RETURNS LOGICAL
  ( INPUT ihObject           AS HANDLE,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryJoin        AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Refresh query-row, browse-row or buffer 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR httTable       AS HANDLE NO-UNDO.
DEF VAR httTableBuffer AS HANDLE NO-UNDO.
DEF VAR httTableQuery  AS HANDLE NO-UNDO.

DEF VAR hBuffer             AS HANDLE NO-UNDO.
DEF VAR cQueryCriteria      AS CHAR NO-UNDO.
DEF VAR cTemp               AS CHAR NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR cFieldDef           AS CHAR   NO-UNDO.
DEF VAR hAltExtentFields    AS HANDLE NO-UNDO EXTENT 50.
DEF VAR hExtentFields       AS HANDLE NO-UNDO EXTENT 50.
DEF VAR cExtentFields       AS CHAR   NO-UNDO.
DEF VAR cAltExtFieldHandles AS CHAR   NO-UNDO.
DEF VAR iCntExtFields       AS INT    NO-UNDO.
DEF VAR cExcludeLocalFields AS CHAR   NO-UNDO.
DEF VAR cExcludedFields     AS CHAR   NO-UNDO. 

IF icBuffersAndFields = "" THEN RETURN FALSE.

CASE ihObject:TYPE:
  WHEN "buffer" THEN hBuffer = ihObject.
  WHEN "query"  THEN hBuffer = ihObject:GET-BUFFER-HANDLE(1).
  WHEN "browse" THEN hBuffer = ihObject:QUERY:GET-BUFFER-HANDLE(1).
  OTHERWISE RETURN FALSE.
END CASE.

cQueryCriteria = "WHERE ROWID(" + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + ") = TO-ROWID('" + 
                 (IF cCurrentRowid NE "" THEN cCurrentRowid
                  ELSE STRING(hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)) + "')" +
                 icQueryJoin                                          
                 .

httTable = DYNAMIC-FUNCTION("getTempTableJoin",1,0,"",
                            icBuffersAndFields,
                            cQueryCriteria).

httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.

ASSIGN cExtentFields       = DYNAMIC-FUNCTION("getAttribute",ihObject,"extentFields")
       cAltExtFieldHandles = DYNAMIC-FUNCTION("getAttribute",ihObject,"AltExtentFieldHandles")
       cExcludeLocalFields = DYNAMIC-FUNCTION("getAttribute",ihObject,"localcalcfields").
DO ix = 1 TO NUM-ENTRIES(cExtentFields):
  ASSIGN hAltExtentFields[ix] = WIDGET-HANDLE(ENTRY(ix,cAltExtFieldHandles))
         iCntExtFields        = iCntExtFields + 1.
  hExtentFields[ix]    = httTableBuffer:BUFFER-FIELD(ENTRY(ix,cExtentFields)) NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
    cFieldDef = ENTRY(ix,cExtentFields).
    hAltExtentFields[ix] = hBuffer:BUFFER-FIELD(cFieldDef).
    cFieldDef = SUBSTR(cFieldDef,1,R-INDEX(cFieldDef,"_") - 1) + "[" + SUBSTR(cFieldDef,R-INDEX(cFieldDef,"_") + 1) + "]".
    hExtentFields[ix] = httTableBuffer:BUFFER-FIELD(cFieldDef).
  END.
END.


httTableBuffer:FIND-FIRST() NO-ERROR.

IF httTableBuffer:AVAIL THEN DO:
  hBuffer:BUFFER-COPY(httTableBuffer).
  DO ix = 1 TO iCntExtFields:
    hAltExtentFields[ix]:BUFFER-VALUE = hExtentFields[ix]:BUFFER-VALUE.
  END.
END.  

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-resetTransRecords) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION resetTransRecords Procedure 
FUNCTION resetTransRecords RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ixTransRecordId = 0.

EMPTY TEMP-TABLE ttTransAction.
EMPTY TEMP-TABLE ttTransRecord.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-runProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION runProc Procedure 
FUNCTION runProc RETURNS LOGICAL
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT httTable         AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieve custom queries
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR obOk          AS LOG NO-UNDO.
DEF VAR httBuffer     AS HANDLE NO-UNDO.
DEF VAR httQuery      AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer AS HANDLE NO-UNDO.
                                       
IF bShowHourGlass THEN SESSION:SET-WAIT-STATE("general").

DELETE OBJECT httRunProc NO-ERROR.

IF VALID-HANDLE(httTable) THEN DO:
  IF httTable:TYPE = "buffer" THEN
    httTable = httTable:TABLE-HANDLE.

  httBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  CREATE TEMP-TABLE httRunProc.
  httRunProc:CREATE-LIKE(httBuffer).
  httRunProc:TEMP-TABLE-PREPARE(httBuffer:NAME).
  hTargetBuffer = httRunProc:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httQuery.
  httQuery:SET-BUFFERS(httBuffer).
  httQuery:QUERY-PREPARE("FOR EACH " + httBuffer:NAME).
  httQuery:QUERY-OPEN().
  httQuery:GET-FIRST().
  REPEAT WHILE NOT httQuery:QUERY-OFF-END:
    hTargetBuffer:BUFFER-CREATE().
    hTargetBuffer:BUFFER-COPY(httBuffer).
    httQuery:GET-NEXT().
  END.
  DELETE OBJECT httQuery.
END.

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_runproc.p
      ON getAppServiceHandle()
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT TABLE-HANDLE httRunProc,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      obOK = runProc(icServerProgram,icParamList,httRunProc).
  END.
END.
ELSE 
  RUN jbserv_runproc.p
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT TABLE-HANDLE httRunProc,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      .

SESSION:SET-WAIT-STATE("").

IF obOk THEN cServerTransReturnParam = cReturnTrans.

RETURN obOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-runProcDs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION runProcDs Procedure 
FUNCTION runProcDs RETURNS LOGICAL
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihDataset        AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieve custom queries
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR obOk          AS LOG NO-UNDO.
DEF VAR httBuffer     AS HANDLE NO-UNDO.
DEF VAR httQuery      AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer AS HANDLE NO-UNDO.
                                       
IF bShowHourGlass THEN SESSION:SET-WAIT-STATE("general").

DELETE OBJECT hdsRunProc NO-ERROR.

IF VALID-HANDLE(ihDataset) THEN DO:
  CREATE DATASET hdsRunProc.
  hdsRunProc:CREATE-LIKE(ihDataset).
  hdsRunProc:COPY-DATASET(ihDataset).
END.

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_runproc_ds.p
      ON getAppServiceHandle()
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT DATASET-HANDLE hdsRunProc,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF ReconnectServer() THEN 
      obOK = runProc(icServerProgram,icParamList,hdsRunProc).
  END.
END.
ELSE 
  RUN jbserv_runproc_ds.p
     (getSessionId(),
      icServerProgram,
      icParamList,
      INPUT-OUTPUT DATASET-HANDLE hdsRunProc,
      OUTPUT cReturnTrans,
      OUTPUT obOk)
      .

SESSION:SET-WAIT-STATE("").

IF obOk THEN cServerTransReturnParam = cReturnTrans.

RETURN obOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Scandinavian) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Scandinavian Procedure 
FUNCTION Scandinavian RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF CAN-DO("da,dk,no,sv,se,nn,",cLanguage) THEN
  RETURN TRUE.
ELSE
  RETURN FALSE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppendWinCompanyTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppendWinCompanyTitle Procedure 
FUNCTION setAppendWinCompanyTitle RETURNS LOGICAL
  ( INPUT icAppendWinCompanyTitle AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cAppendWinCompanyTitle = icAppendWinCompanyTitle.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppserviceConnectString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppserviceConnectString Procedure 
FUNCTION setAppserviceConnectString RETURNS LOGICAL
  ( INPUT icAppServiceConnectString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

cAppServiceConnectString = icAppServiceConnectString.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppserviceHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppserviceHandle Procedure 
FUNCTION setAppserviceHandle RETURNS LOGICAL
  ( INPUT ihAppserver AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hAppserver = ihAppserver.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppserviceId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppserviceId Procedure 
FUNCTION setAppserviceId RETURNS LOGICAL
  ( INPUT icAppServiceId AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cAppServiceId = icAppServiceId.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setAppTitle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setAppTitle Procedure 
FUNCTION setAppTitle RETURNS LOGICAL
  ( INPUT icAppTitle AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cAppTitle = icAppTitle.
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setASlibBehaviour) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setASlibBehaviour Procedure 
FUNCTION setASlibBehaviour RETURNS LOGICAL
  ( INPUT icBehaviour AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(icBehaviour):
  CASE ENTRY(1,ENTRY(ix,icBehaviour),"|"):
    WHEN "TransLogFile"        THEN cTransLogFile        = ENTRY(2,ENTRY(ix,icBehaviour),"|").
    WHEN "QueryLogFile"        THEN cQueryLogFile        = ENTRY(2,ENTRY(ix,icBehaviour),"|").
    WHEN "MemInfoLogFile"      THEN cMemInfoLogFile      = ENTRY(2,ENTRY(ix,icBehaviour),"|").
  END CASE.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setASUserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setASUserId Procedure 
FUNCTION setASUserId RETURNS LOGICAL
  ( INPUT icASUserId   AS CHAR,
    INPUT icASUserName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cASUserId = icASUserId.
IF icASuserName NE "" THEN
  cASuserName = icASuserName.
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBaseLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBaseLanguageCode Procedure 
FUNCTION setBaseLanguageCode RETURNS LOGICAL
  ( INPUT icBaseLanguage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cBaseLanguage = icBaseLanguage.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCacheFieldFormat Procedure 
FUNCTION setCacheFieldFormat RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icFormat     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF INDEX(icTableName,"*") = 0 THEN DO:
  IF icDbName NE "" THEN
    FIND FIRST tt_field 
         WHERE tt_field.Db-name    = icDbName
           AND tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  ELSE
    FIND FIRST tt_field 
         WHERE tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  IF AVAIL tt_field THEN
    tt_field.Field-Format = icFormat.
  ELSE RETURN NO.
END.
ELSE 
  FOR EACH tt_field 
      WHERE (IF icDbName NE "" THEN tt_field.Db-Name = icDbName ELSE TRUE)
        AND tt_field.Table-Name MATCHES icTableName
        AND tt_field.Field-Name = icFieldName
      :
    tt_field.Field-Format = icFormat.
  END.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCacheFieldHelp Procedure 
FUNCTION setCacheFieldHelp RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icHelp     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF INDEX(icTableName,"*") = 0 THEN DO:
  IF icDbName NE "" THEN
    FIND FIRST tt_field 
         WHERE tt_field.Db-name    = icDbName
           AND tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  ELSE
    FIND FIRST tt_field 
         WHERE tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  IF AVAIL tt_field THEN
    tt_field.Field-Help = icHelp.
  ELSE RETURN NO.
END.
ELSE 
  FOR EACH tt_field 
      WHERE (IF icDbName NE "" THEN tt_field.Db-Name = icDbName ELSE TRUE)
        AND tt_field.Table-Name MATCHES icTableName
        AND tt_field.Field-Name = icFieldName
      :
    tt_field.Field-Help = icHelp.
  END.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldInitValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCacheFieldInitValue Procedure 
FUNCTION setCacheFieldInitValue RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icInitValue     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF INDEX(icTableName,"*") = 0 THEN DO:
  IF icDbName NE "" THEN
    FIND FIRST tt_field 
         WHERE tt_field.Db-name    = icDbName
           AND tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  ELSE
    FIND FIRST tt_field 
         WHERE tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  IF AVAIL tt_field THEN
    tt_field.Field-Initial = icInitValue.
  ELSE RETURN NO.
END.
ELSE 
  FOR EACH tt_field 
      WHERE (IF icDbName NE "" THEN tt_field.Db-Name = icDbName ELSE TRUE)
        AND tt_field.Table-Name MATCHES icTableName
        AND tt_field.Field-Name = icFieldName
      :
    tt_field.Field-Initial = icInitValue.
  END.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCacheFieldLabel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCacheFieldLabel Procedure 
FUNCTION setCacheFieldLabel RETURNS LOGICAL
  ( INPUT icDbName     AS CHAR,
    INPUT icTableName  AS CHAR,
    INPUT icFieldName  AS CHAR,
    INPUT icLabel     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF INDEX(icTableName,"*") = 0 THEN DO:
  IF icDbName NE "" THEN
    FIND FIRST tt_field 
         WHERE tt_field.Db-name    = icDbName
           AND tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  ELSE
    FIND FIRST tt_field 
         WHERE tt_field.Table-Name = icTableName
           AND tt_field.Field-Name = icFieldName
         NO-ERROR.
  IF AVAIL tt_field THEN
    tt_field.Field-Label = icLabel.
  ELSE RETURN NO.
END.
ELSE 
  FOR EACH tt_field 
      WHERE (IF icDbName NE "" THEN tt_field.Db-Name = icDbName ELSE TRUE)
        AND tt_field.Table-Name MATCHES icTableName
        AND tt_field.Field-Name = icFieldName
      :
    tt_field.Field-Label = icLabel.
  END.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCalcFieldFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCalcFieldFilter Procedure 
FUNCTION setCalcFieldFilter RETURNS LOGICAL
  ( INPUT icCalcFieldFilter AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

cCalcFieldFilter = icCalcFieldFilter.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCalcFieldProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCalcFieldProc Procedure 
FUNCTION setCalcFieldProc RETURNS LOGICAL
  ( INPUT icCalcFieldProc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Set name of server procedure containing all calculated fields 
            for getTempTableJoin -> jbserv_gettemptablejoin.p 
    Notes:  
------------------------------------------------------------------------------*/
cCalcFieldProc = icCalcFieldProc.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-setCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCodeMaster Procedure 
FUNCTION setCodeMaster RETURNS LOGICAL
  ( INPUT icCodeMasterCompanyId AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
cCodeMasterCompanyId = icCodeMasterCompanyId.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCompanyHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCompanyHeader Procedure 
FUNCTION setCompanyHeader RETURNS LOGICAL
  ( INPUT hWindow AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iCompanyId NE 0 AND cCompanyName NE "" THEN DO:
  IF hWindow:TITLE MATCHES "*[*" THEN
    hWindow:TITLE = SUBSTR(hWindow:TITLE,1,INDEX(hWindow:TITLE,"[")) + cCompanyName
                  + (IF cAppendWinCompanyTitle NE "" THEN " " + cAppendWinCompanyTitle ELSE "")
                  + "]" NO-ERROR.
  ELSE 
    hWindow:TITLE = hWindow:TITLE + " [" + cCompanyName 
                  + (IF cAppendWinCompanyTitle NE "" THEN " " + cAppendWinCompanyTitle ELSE "")
                  + "]" NO-ERROR.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCompanyId Procedure 
FUNCTION setCompanyId RETURNS LOGICAL
  ( INPUT iiCompanyId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hProc          AS HANDLE NO-UNDO.
DEF VAR ocUserLevel    AS CHAR   NO-UNDO.
DEF VAR cNewCodeMaster AS CHAR   NO-UNDO.
DEF VAR cNewParent     AS CHAR   NO-UNDO.
DEF VAR cCompanyParam  AS CHAR   NO-UNDO.

bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_getcompanyname.p ON getAppServiceHandle() (getSessionId(),
                                                        iiCompanyId, 
                                                        OUTPUT cCompanyName,
                                                        OUTPUT cNewCodeMaster,
                                                        OUTPUT cNewParent,
                                                        OUTPUT cCompanyLogo,
                                                        OUTPUT cCompanyParam,
                                                        OUTPUT bOK) 
      NO-ERROR.
  IF ERROR-STATUS:ERROR AND (ERROR-STATUS:GET-NUMBER(1) = 3230 OR ERROR-STATUS:GET-NUMBER(1) = 3234) THEN 
    RUN jbserv_getcompanyname.p ON getAppServiceHandle() (getSessionId(),
                                                          iiCompanyId, 
                                                          OUTPUT cCompanyName, 
                                                          OUTPUT bOK). 
END.
ELSE DO:
  RUN jbserv_getcompanyname.p (getSessionId(),
                               iiCompanyId, 
                               OUTPUT cCompanyName, 
                               OUTPUT cNewCodeMaster,
                               OUTPUT cNewParent,
                               OUTPUT cCompanyLogo,
                               OUTPUT cCompanyParam,
                               OUTPUT bOK)
      NO-ERROR.
  IF ERROR-STATUS:ERROR AND (ERROR-STATUS:GET-NUMBER(1) = 3230 OR ERROR-STATUS:GET-NUMBER(1) = 3234) THEN 
    RUN jbserv_getcompanyname.p (getSessionId(),
                                 iiCompanyId, 
                                 OUTPUT cCompanyName, 
                                 OUTPUT bOK). 
END.

IF bOK THEN DO:
  iCompanyId = iiCompanyId.

  IF getAppServiceHandle() NE ? THEN
    RUN jbadmin_getuserlevel.p ON getAppServiceHandle() (getSessionId(),iiCompanyId,OUTPUT ocUserLevel,OUTPUT bOK).
  ELSE 
    RUN jbadmin_getuserlevel.p (getSessionId(),iiCompanyId,OUTPUT ocUserLevel,OUTPUT bOK).
  IF bOk THEN cUserLevel = ocUserLevel.

  getFunctionRestrictions().

  IF NUM-ENTRIES(cCompanyName,"|") > 1 THEN DO:      
    DO ix = 2 TO NUM-ENTRIES(cCompanyName,"|"):
      CASE ENTRY(1,ENTRY(ix,cCompanyName,"|"),"¤"):
        WHEN "codemaster" THEN
          cNewCodeMaster = ENTRY(2,ENTRY(ix,cCompanyName,"|"),"¤").
        WHEN "parent" THEN
          cNewParent = ENTRY(2,ENTRY(ix,cCompanyName,"|"),"¤").
      END CASE.
    END.
    cCompanyName = ENTRY(1,cCompanyName,"|").
  END.
  IF cNewCodeMaster NE "" THEN cCodeMasterCompanyId = cNewCodeMaster.
  ELSE cCodeMasterCompanyId = STRING(iCompanyId).
  IF cNewParent NE "" THEN cParentCompanyId = cNewParent.
  ELSE cParentCompanyId = STRING(iCompanyId).

  hProc = SESSION:FIRST-PROCEDURE.
  REPEAT WHILE hProc NE ?:
    IF VALID-HANDLE(hProc:CURRENT-WINDOW) AND CAN-DO(hProc:INTERNAL-ENTRIES,"ChangeCompany") THEN
      setCompanyHeader(hProc:CURRENT-WINDOW).
    hProc = hProc:NEXT-SIBLING.
  END.
  
  IF NOT bMenuRestart THEN PUBLISH "ChangeCompany".
  bMenuRestart = NO.

  PUBLISH "setCompanyLogo" (cCompanyLogo).
END.
ELSE MESSAGE PROGRAM-NAME(1) SKIP
             "Couldn't get company name. (Probably due to invalid session id)"
             VIEW-AS ALERT-BOX ERROR.

bOK = SESSION:SET-WAIT-STATE("").

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCurrentRowid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCurrentRowid Procedure 
FUNCTION setCurrentRowid RETURNS LOGICAL
  ( INPUT icCurrentRowid AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Swap rowid for save and refresh if the original buffer sequence was changed 
    Notes:  
------------------------------------------------------------------------------*/
cCurrentRowid = icCurrentRowid.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDataDictFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDataDictFilter Procedure 
FUNCTION setDataDictFilter RETURNS LOGICAL
  ( INPUT icDBFilter    AS CHAR,
    INPUT icTableFilter AS CHAR,
    INPUT icFieldFilter AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set filter criteria for retrieval of data dictionary queries 
    Notes: Initial values:
    DEF VAR cDBFilter                 AS CHAR NO-UNDO INIT "*".
    DEF VAR cTableFilter              AS CHAR NO-UNDO INIT "WHERE NOT cFileName BEGINS 'JBox'".
    DEF VAR cFieldFilter              AS CHAR NO-UNDO INIT "  AND NOT CAN-DO('iJBoxCompanyId',cFieldName)".

------------------------------------------------------------------------------*/
ASSIGN cDBFilter    = (IF icDBFilter    NE "" THEN icDBFilter    ELSE cDBFilter)
       cTableFilter = (IF icTableFilter NE "" THEN icTableFilter ELSE cTableFilter)
       cFieldFilter = (IF icFieldFilter NE "" THEN icFieldFilter ELSE cFieldFilter). 

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDefaultTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDefaultTranslation Procedure 
FUNCTION setDefaultTranslation RETURNS LOGICAL
  ( INPUT icLanguageCode AS CHAR,
    INPUT icFieldName    AS CHAR,
    INPUT icFieldLabel   AS CHAR,
    INPUT icFieldHelp    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Set default translation for standard fields (see also main block)
            Used to translate dictionary cache (in BuildTableCache)
    Notes:  To cancel a translation set "" as label
------------------------------------------------------------------------------*/
FIND FIRST ttDefaultTranslation
     WHERE ttDefaultTranslation.cLanguage  = icLanguageCode
       AND ttDefaultTranslation.cFieldName = icFieldName
     NO-ERROR.

IF NOT AVAIL ttDefaultTranslation THEN DO:
  CREATE ttDefaultTranslation.
  ASSIGN ttDefaultTranslation.cLanguage   = icLanguageCode
         ttDefaultTranslation.cFieldName  = icFieldName
         .
END.
 
ASSIGN ttDefaultTranslation.cFieldLabel = icFieldLabel  
       ttDefaultTranslation.cFieldHelp  = icFieldHelp
       .
        

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setIndexOnRowids) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setIndexOnRowids Procedure 
FUNCTION setIndexOnRowids RETURNS LOGICAL
  ( INPUT ibIndexOnRowids AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: The first time a resultset for a browse is retrieved an index is 
           defined on all rowid columns to speed up check for duplicates
           when subsequent fills are done to the same browse (or query)  
    Notes:  
------------------------------------------------------------------------------*/
bIndexOnRowids = ibIndexOnRowids.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLanguageCode Procedure 
FUNCTION setLanguageCode RETURNS LOGICAL
  ( INPUT icLanguage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cDefActionList AS CHAR NO-UNDO.
DEF VAR cDefLabelList  AS CHAR NO-UNDO.

IF icLanguage = "EN" THEN DO:
  ASSIGN cDefActionList = "accept,new,edit,close,help,copy,undo,delete,save"
                        + ",excel,word,print,filter,www,email,next,prev,first,last"
                        + ",commit,rollback,activate,flatview,refresh,accum,moveup,movedown,browseconfig,color,note,documents"
         cDefLabelList  = "Accept,New,Edit,Close,Help,Copy,Undo,Delete,Save"
                        + ",Excel,Word,Print,Filter,www,Email,Next,Prev,First,Last"
                        + ",Commit,Rollback,Activate,Drill-down view,Refresh,Accumulate,Move Up,Move Down,Column setup,Color,Note,Documents"
         .                              
  
  DO ix = 1 TO NUM-ENTRIES(cDefLabelList):
    DYNAMIC-FUNCTION("setAttribute",SESSION,"BtnLabel_" + ENTRY(ix,cDefActionList),ENTRY(ix,cDefLabelList)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Could not set default button labels for language " + icLanguage SKIP
              "Probably due to wrong sequence of loading libraries"
              VIEW-AS ALERT-BOX ERROR.
      LEAVE.
    END.
  END.
END.

cLanguage = icLanguage.

RETURN DoUpdate("JBoxLoginSession","",
                "cSessionId",
                getSessionId(),
                "cLanguage",
                cLanguage,
                YES).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLanguages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLanguages Procedure 
FUNCTION setLanguages RETURNS LOGICAL
  ( INPUT icAppLanguages AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cAppLanguages = REPLACE(icAppLanguages,",","|").
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoadDataDictOnStartup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLoadDataDictOnStartup Procedure 
FUNCTION setLoadDataDictOnStartup RETURNS LOGICAL
  ( INPUT ibLoadDataDictOnStartup AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  NOT IN USE
------------------------------------------------------------------------------*/
bLoadDataDictOnStartup = ibLoadDataDictOnStartup.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMenuRestart) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMenuRestart Procedure 
FUNCTION setMenuRestart RETURNS LOGICAL
  ( INPUT ibMenuRestart AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bMenuRestart = ibMenuRestart.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNoExistBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoExistBuffers Procedure 
FUNCTION setNoExistBuffers RETURNS LOGICAL
  ( INPUT icNoExistBuffers AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cNoExistBuffers = icNoExistBuffers.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOrgBufferList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOrgBufferList Procedure
FUNCTION setOrgBuffersAndFields RETURNS LOGICAL 
  ( INPUT icOrgBuffersAndFields AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  cOrgBuffersAndFields = icOrgBuffersAndFields.

  RETURN YES.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setPostUpdProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPostUpdProc Procedure 
FUNCTION setPostUpdProc RETURNS LOGICAL
  ( INPUT icPostUpdProc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Set the post transaction update procedure
    Notes:  cPostUpdProc is cleared after use either in DoCreate or DoUpdate
            (Does not apply for DoDelete)
------------------------------------------------------------------------------*/
cPostUpdProc = icPostUpdProc.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreScanLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPreScanLimit Procedure 
FUNCTION setPreScanLimit RETURNS LOGICAL
  ( INPUT iiPreScanLimit AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

iPreScanLimit = iiPreScanLimit.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPreScanQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPreScanQuery Procedure 
FUNCTION setPreScanQuery RETURNS LOGICAL
  ( INPUT icPreScanQuery AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cPreScanQuery = icPreScanQuery.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setQueryStatFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQueryStatFields Procedure 
FUNCTION setQueryStatFields RETURNS LOGICAL
  ( INPUT icQueryStatFields AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cQueryStatFields = icQueryStatFields.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setQueryStatValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQueryStatValues Procedure 
FUNCTION setQueryStatValues RETURNS LOGICAL
  ( INPUT icQueryStatFieldValues AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Used for emergancy overrides
------------------------------------------------------------------------------*/
cQueryStatFieldValues = icQueryStatFieldValues.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setQueryStopAfter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQueryStopAfter Procedure
FUNCTION setQueryStopAfter RETURNS LOGICAL 
  ( INPUT icQueryStopAfter AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
cQueryStopAfter = icQueryStopAfter.

RETURN YES.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setReturnQueryInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setReturnQueryInfo Procedure 
FUNCTION setReturnQueryInfo RETURNS LOGICAL
  ( INPUT ibReturnQueryInfo AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Used to control parameter to getTempTableJoin if it should retrieve additional query info 
    Notes:  
------------------------------------------------------------------------------*/

bReturnQueryInfo = ibReturnQueryInfo.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setServerTransInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setServerTransInputParam Procedure 
FUNCTION setServerTransInputParam RETURNS LOGICAL
  ( INPUT icServerTransInputParam AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cServerTransInputParam = icServerTransInputParam.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSessionContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSessionContext Procedure 
FUNCTION setSessionContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bOk = runProc("jbserv_setsessioncontext.p",icContext,?).
IF NOT bOk THEN DO:
  MESSAGE getTransactionMessage() VIEW-AS ALERT-BOX.
  QUIT.
END.
RETURN bOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSessionId Procedure 
FUNCTION setSessionId RETURNS LOGICAL
  ( INPUT icSessionId AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cSessionId = icSessionId.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setShowHourGlass) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setShowHourGlass Procedure 
FUNCTION setShowHourGlass RETURNS LOGICAL
  ( INPUT ibShowHourGlass AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bShowHourGlass = ibShowHourGlass.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSkipCalcFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSkipCalcFields Procedure 
FUNCTION setSkipCalcFields RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNoMandCalcFields AS CHAR NO-UNDO.
DEF VAR cCalcFields       AS CHAR NO-UNDO.
DEF VAR cCurrViewFields   AS CHAR NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.

ASSIGN cNoMandCalcFields = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"noMandatoryCalcFields")
       cCalcFields       = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"allCalcFields")
       cCurrViewFields   = DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"currViewFields")
       .

DO ix = 1 TO NUM-ENTRIES(cNoMandCalcFields):
  IF NOT CAN-DO(cCurrViewFields,ENTRY(ix,cNoMandCalcFields))
     AND DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"filtervalue_" + ENTRY(ix,cNoMandCalcFields)) = ""
     AND LOOKUP(ENTRY(ix,cNoMandCalcFields),DYNAMIC-FUNCTION("getAttribute",ihQueryObject,"localsort")," ") = 0
     THEN
    cSkipCalcFields = (IF cSkipCalcFields NE "" THEN "¤" ELSE "") + ENTRY(ix,cNoMandCalcFields).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSourceFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSourceFileName Procedure 
FUNCTION setSourceFileName RETURNS LOGICAL
  ( icTransSourceFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cTransSourceFile = icTransSourceFile.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTranManBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTranManBuffer Procedure 
FUNCTION setTranManBuffer RETURNS LOGICAL
  ( INPUT ihTranManBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hTranManBuffer = ihTranManBuffer.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTranManProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTranManProc Procedure 
FUNCTION setTranManProc RETURNS LOGICAL
  ( INPUT ihTranManProc AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hTranManProc = ihTranManProc.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTransactionMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTransactionMessage Procedure 
FUNCTION setTransactionMessage RETURNS LOGICAL
  ( INPUT icReturnTrans AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cReturnTrans = icReturnTrans.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTranslationRepFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTranslationRepFile Procedure 
FUNCTION setTranslationRepFile RETURNS LOGICAL
  ( INPUT icTranslationReportFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

cTranslationReportFile = icTranslationReportFile.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUniqueBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUniqueBuffer Procedure 
FUNCTION setUniqueBuffer RETURNS LOGICAL
  ( INPUT icUniqueBuffer AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Passed on to jbserv_gettemptablejoin as a tuning option:
            When accumulating subtotals the procedure checks if the value has already been processed.
            This check is unneccessary for the unique buffer.
    Notes:  
------------------------------------------------------------------------------*/
cUniqueBuffer = icUniqueBuffer.

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseAppserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUseAppserver Procedure 
FUNCTION setUseAppserver RETURNS LOGICAL
  ( INPUT ihUseAppServer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hUseAppServer = ihUseAppServer.

RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseDynamicsTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUseDynamicsTranslation Procedure 
FUNCTION setUseDynamicsTranslation RETURNS LOGICAL
  ( INPUT ibDynamicsTranslation AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bDynamicsTranslation = ibDynamicsTranslation.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUserSetting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUserSetting Procedure 
FUNCTION setUserSetting RETURNS LOGICAL
  ( INPUT icSourceFile     AS CHAR,
    INPUT icObjectName     AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icSettingName    AS CHAR,
    INPUT icSetting        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Store user setting / attribute
------------------------------------------------------------------------------*/
bOK = SESSION:SET-WAIT-STATE("general").

IF getAppServiceHandle() NE ? THEN DO:
  RUN jbserv_setusersetting.p
      ON getAppServiceHandle()
     (getSessionId(),
      icSourceFile,
      icObjectName,
      icContext,
      icSettingName,
      icSetting)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF NOT ERROR-STATUS:GET-NUMBER(1) = 293 AND ReconnectServer() THEN 
      RETURN setUserSetting (icSourceFile,icObjectName,icContext,icSettingName,icSetting).
  END.
END.
ELSE 
  RUN jbserv_setusersetting.p
     (getSessionId(),
      icSourceFile,
      icObjectName,
      icContext,
      icSettingName,
      icSetting)
      NO-ERROR.

IF RETURN-VALUE = "" THEN DO:
  FIND FIRST ttCache
       WHERE ttCache.cTable = "JBoxUserSetting"
       NO-ERROR.
  IF AVAIL ttCache THEN DO:
    IF VALID-HANDLE(ttCache.hBuffer) THEN DO ix = 1 TO NUM-ENTRIES(icSettingName):
      bOk = ttCache.hBuffer:FIND-FIRST("WHERE cSourceFile  = '" + icSourceFile + "'"
                              + " AND cObjectName  = '" + icObjectName + "'"
                              + " AND cContext     = '" + icContext + "'"
                              + " AND cSettingName = '" + ENTRY(ix,icSettingName) + "'"
                              + " AND cJBoxUserId  = '" + cASUserId + "'") NO-ERROR.
      IF bOk AND icSetting = "delete_setting" THEN
        ttCache.hBuffer:BUFFER-DELETE().
      ELSE IF icSetting NE "delete_setting" THEN DO:
        IF NOT bOk THEN DO:
          ttCache.hBuffer:BUFFER-CREATE().
          ASSIGN ttCache.hBuffer:BUFFER-FIELD("cSourceFile"):BUFFER-VALUE        = icSourceFile
                 ttCache.hBuffer:BUFFER-FIELD("cObjectName"):BUFFER-VALUE        = icObjectName
                 ttCache.hBuffer:BUFFER-FIELD("cContext"):BUFFER-VALUE           = icContext
                 ttCache.hBuffer:BUFFER-FIELD("cSettingName"):BUFFER-VALUE       = ENTRY(ix,icSettingName)
                 ttCache.hBuffer:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE        = cASUserId
                 ttCache.hBuffer:BUFFER-FIELD("dCreated"):BUFFER-VALUE           = TODAY
                 ttCache.hBuffer:BUFFER-FIELD("cCreatedBy"):BUFFER-VALUE         = cASUserId
                 ttCache.hBuffer:BUFFER-FIELD("iJBoxUserSettingId"):BUFFER-VALUE 
                         = INT(getFieldValues("JBoxUserSetting",
                               "WHERE cSourceFile  = '" + icSourceFile + "'"
                              + " AND cObjectName  = '" + icObjectName + "'"
                              + " AND cContext     = '" + icContext + "'"
                              + " AND cSettingName = '" + ENTRY(ix,icSettingName) + "'"
                              + " AND cJBoxUserId  = '" + cASUserId + "'"
                               ,"iJBoxUserSettingId"))
                 .
        END.
        ELSE ASSIGN
          ttCache.hBuffer:BUFFER-FIELD("dModified"):BUFFER-VALUE     = TODAY
          ttCache.hBuffer:BUFFER-FIELD("cModifiedBy"):BUFFER-VALUE   = cASUserId.
        
        ttCache.hBuffer:BUFFER-FIELD("cSetting"):BUFFER-VALUE = ENTRY(ix,icSetting,CHR(1)).
      END.
    END.
    ELSE DO:
      IF NOT BuildTableCache("JBoxUserSetting|WHERE cJBoxUserId = '" + cASuserId + "'") THEN
        MESSAGE "Failed to rebuild cache for user settings after save"
                VIEW-AS ALERT-BOX WARNING.
    END.
  END.
END.

bOK = SESSION:SET-WAIT-STATE("").
  
cReturnTrans = RETURN-VALUE.
IF LENGTH(TRIM(cReturnTrans)) LE 2 THEN cReturnTrans = "".

RETURN cReturnTrans = "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUserSettingSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUserSettingSort Procedure
FUNCTION setUserSettingSort RETURNS LOGICAL 
  ( INPUT icUserSettingSortExp AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
cUserSettingSortExp = icUserSettingSortExp.

RETURN YES.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setWidgetTranslation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWidgetTranslation Procedure 
FUNCTION setWidgetTranslation RETURNS LOGICAL
  ( INPUT ihTranslationBuffer AS HANDLE,
    INPUT ihWidget            AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cSortInd AS CHAR NO-UNDO.
DEF VAR cAcc     AS CHAR NO-UNDO.

IF ihWidget:PARENT NE ? AND ihWidget:PARENT:TYPE = "BROWSE" THEN DO:
  cWidgetType = "BROWSE-COLUMN".
  IF LENGTH(ihWidget:LABEL) > 1 AND CAN-DO(" v, ^",SUBSTR(ihWidget:LABEL,LENGTH(ihWidget:LABEL) - 1)) THEN
    cSortInd = SUBSTR(ihWidget:LABEL,LENGTH(ihWidget:LABEL) - 1).
  ELSE IF LENGTH(ihWidget:LABEL) > 2 AND CAN-DO(" <<, >>",SUBSTR(ihWidget:LABEL,LENGTH(ihWidget:LABEL) - 2)) THEN
    cSortInd = SUBSTR(ihWidget:LABEL,LENGTH(ihWidget:LABEL) - 2).
END.
ELSE IF ihWidget:TYPE = "text" AND ihWidget:SIDE-LABEL-HANDLE NE ? THEN
  cWidgetType = "TEXT-FILL-IN".
ELSE
  cWidgetType = ihWidget:TYPE.

IF VALID-HANDLE(ihWidget) THEN DO:
  IF CAN-DO(cTitleList,cWidgetType) THEN
    ihWidget:TITLE = ihTranslationBuffer:BUFFER-FIELD("cTitle"):BUFFER-VALUE + 
                    IF ihWidget:TITLE MATCHES "*[*" THEN SUBSTR(ihWidget:TITLE,INDEX(ihWidget:TITLE,"[") - 1) ELSE "" NO-ERROR.
  ELSE IF cWidgetType = "text" THEN
    ihWidget:SCREEN-VALUE = ihTranslationBuffer:BUFFER-FIELD("cInitValue"):BUFFER-VALUE.
  ELSE IF cWidgetType = "RADIO-SET" THEN ihWidget:RADIO-BUTTONS = ihTranslationBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE.
/*   ELSE IF cWidgetType = "COMBO-BOX" AND ihWidget:SUBTYPE = "SIMPLE" THEN ihWidget:LIST-ITEMS = ihTranslationBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE. */
/*   ELSE IF cWidgetType = "COMBO-BOX" THEN ihWidget:LIST-ITEM-PAIRS = ihTranslationBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE.                            */
  ELSE DO:
    IF CAN-DO(cToolTipList,cWidgetType) THEN ihWidget:TOOLTIP = ihTranslationBuffer:BUFFER-FIELD("cToolTip"):BUFFER-VALUE.
    IF CAN-DO(cToolTipList,cWidgetType) THEN ihWidget:HELP    = ihTranslationBuffer:BUFFER-FIELD("cHelpText"):BUFFER-VALUE.
    IF CAN-DO(cLabelList,cWidgetType) THEN /* DO: */
      ihWidget:LABEL     = ihTranslationBuffer:BUFFER-FIELD("cLabel"):BUFFER-VALUE + cSortInd NO-ERROR.  
/*       IF cWidgetType = "menu-item" THEN                                          */
/*         MESSAGE PROGRAM-NAME(1) SKIP                                             */
/*                 CAN-QUERY(ihWidget,"accelerator") SKIP                           */
/*                 ihTranslationBuffer:BUFFER-FIELD("cHelpText"):BUFFER-VALUE SKIP  */
/*                 VIEW-AS ALERT-BOX.                                               */
/*       IF cWidgetType = "menu-item" AND CAN-QUERY(ihWidget,"accelerator") AND ihTranslationBuffer:BUFFER-FIELD("cHelpText"):BUFFER-VALUE NE "" THEN DO: */
/*         cAcc = ihTranslationBuffer:BUFFER-FIELD("cHelpText"):BUFFER-VALUE.                                                                             */
/*         MESSAGE PROGRAM-NAME(1) SKIP                                                                                                                   */
/*                 ihWidget:PARENT:TYPE                                                                                                                   */
/*                 VIEW-AS ALERT-BOX.                                                                                                                     */
/* /*         ihWidget:ACCELERATOR = cAcc NO-ERROR.  */                                                                                                   */
/*       END.                                                                                                                                             */
/*     END.                                                                                                                                               */
  END.
END.

RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TranManInitBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TranManInitBrowse Procedure 
FUNCTION TranManInitBrowse RETURNS HANDLE
  ( INPUT ihFrame            AS HANDLE,
    INPUT ihRect             AS HANDLE,
    INPUT icProperties       AS CHAR,
    INPUT icBuffersAndFields AS CHAR,
    INPUT icQueryCriteria    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Fields prefixed with ! are added to the buffer but not to the grid  
------------------------------------------------------------------------------*/
DEF VAR httTable       AS HANDLE NO-UNDO.
DEF VAR httTableBuffer AS HANDLE NO-UNDO.
DEF VAR httTableQuery  AS HANDLE NO-UNDO.
DEF VAR hBrowse        AS HANDLE NO-UNDO.
DEF VAR hField         AS HANDLE NO-UNDO.
DEF VAR cTranslation   AS CHAR NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.
DEF VAR cTemp          AS CHAR NO-UNDO.

cTemp = REPLACE(icBuffersAndFields,"!","").
cTemp = REPLACE(cTemp,";RowCount","").
cTemp = REPLACE(cTemp,";RowIdent1","").
cTemp = REPLACE(cTemp,";RowIdent2","").
cTemp = REPLACE(cTemp,";RowIdent3","").
httTable = DYNAMIC-FUNCTION("getTempTableJoin",0,0,"",
                            cTemp,
                            icQueryCriteria).

httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY httTableQuery NO-ERROR.
httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.

CREATE BROWSE hBrowse
  ASSIGN ROW-MARKERS      = FALSE
         SEPARATORS       = TRUE
         FRAME            = ihFrame
         QUERY            = httTableQuery
         MULTIPLE         = IF CAN-DO(icProperties,"MULTIPLE") THEN TRUE ELSE FALSE
         ROW-MARKERS      = IF CAN-DO(icProperties,"ROW-MARKERS") THEN TRUE ELSE FALSE
         VISIBLE          = YES
         SENSITIVE        = TRUE
         COLUMN-RESIZABLE = TRUE
         READ-ONLY   = YES
         .

IF VALID-HANDLE(ihRect) THEN 
  ASSIGN hBrowse:X             = ihRect:X + 1
         hBrowse:Y             = ihRect:Y + 1
         hBrowse:WIDTH-PIXELS  = ihRect:WIDTH-PIXELS - 2
         hBrowse:HEIGHT-PIXELS = ihRect:HEIGHT-PIXELS - 2.

DO ix = 1 TO httTableBuffer:NUM-FIELDS:
  IF (NOT httTableBuffer:BUFFER-FIELD(ix):NAME BEGINS "Row" AND
      NOT icBuffersAndFields MATCHES "*!" + httTableBuffer:BUFFER-FIELD(ix):NAME + "*") 
     THEN 
    hField = hBrowse:ADD-LIKE-COLUMN(httTableBuffer:BUFFER-FIELD(ix)) NO-ERROR.
    IF bSetSortLabel AND hField:WIDTH - LENGTH(hField:LABEL) < 4 THEN 
      hField:WIDTH = hField:WIDTH + 5 - MAX(0,hField:WIDTH - LENGTH(hField:LABEL)).
END.

DO ix = 1 TO NUM-ENTRIES(icProperties):
  CASE ENTRY(1,ENTRY(ix,icProperties),"|"):
    WHEN "!FIT-LAST-COLUMN"             THEN hBrowse:FIT-LAST-COLUMN = FALSE.   
    WHEN "!REFRESHABLE"                 THEN hBrowse:REFRESHABLE = FALSE.       
    WHEN "!DROP-TARGET"                 THEN hBrowse:DROP-TARGET = FALSE.       
    WHEN "!LABELS"                      THEN hBrowse:LABELS = FALSE.     
    WHEN "!MOVABLE"                     THEN hBrowse:MOVABLE = FALSE.
    WHEN "!RESIZABLE"                   THEN hBrowse:RESIZABLE = FALSE.
    WHEN "!SEPARATORS"                  THEN hBrowse:SEPARATORS = FALSE.        
    WHEN "!SENSITIVE"                   THEN hBrowse:SENSITIVE = FALSE. 
    WHEN "!SELECTABLE"                  THEN hBrowse:SELECTABLE = FALSE.        
    WHEN "!SCROLLBAR-VERTICAL"          THEN hBrowse:SCROLLBAR-VERTICAL = FALSE.        
    WHEN "!ROW-RESIZABLE"               THEN hBrowse:ROW-RESIZABLE = FALSE.     
    WHEN "!NO-EMPTY-SPACE"              THEN hBrowse:NO-EMPTY-SPACE = FALSE.    
    WHEN "!DYNAMIC"                     THEN hBrowse:DYNAMIC = FALSE.   
    WHEN "!HIDDEN"                      THEN hBrowse:HIDDEN = FALSE.    
    WHEN "!EXPANDABLE"                  THEN hBrowse:EXPANDABLE = FALSE.        
    WHEN "!VIEW-FIRST-COLUMN-ON-REOPEN" THEN hBrowse:VIEW-FIRST-COLUMN-ON-REOPEN = FALSE.       
    WHEN "!TAB-STOP"                    THEN hBrowse:TAB-STOP = FALSE.  
    WHEN "!READ-ONLY"                   THEN hBrowse:READ-ONLY = FALSE.
    WHEN "!VISIBLE"                     THEN hBrowse:VISIBLE = FALSE.
    WHEN "ALLOW-COLUMN-SEARCHING"       THEN hBrowse:ALLOW-COLUMN-SEARCHING = TRUE.     
    WHEN "BGCOLOR"                      THEN hBrowse:BGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "COLUMN-MOVABLE"               THEN hBrowse:COLUMN-MOVABLE = TRUE.     
    WHEN "COLUMN-RESIZABLE"             THEN hBrowse:COLUMN-RESIZABLE = TRUE.   
    WHEN "COLUMN-SCROLLING"             THEN hBrowse:COLUMN-SCROLLING = TRUE.       
    WHEN "CONTEXT-HELP-ID"              THEN hBrowse:CONTEXT-HELP-ID = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).        
    WHEN "COLUMN"                       THEN hBrowse:COLUMN = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "CURRENT-COLUMN"               THEN hBrowse:CURRENT-COLUMN = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).       
    WHEN "DROP-TARGET"                  THEN hBrowse:DROP-TARGET = TRUE.        
    WHEN "DYNAMIC"                      THEN hBrowse:DYNAMIC = TRUE.    
    WHEN "DOWN"                         THEN hBrowse:DOWN = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).   
    WHEN "EXPANDABLE"                   THEN hBrowse:EXPANDABLE = TRUE. 
    WHEN "FIT-LAST-COLUMN"              THEN hBrowse:FIT-LAST-COLUMN = TRUE.    
    WHEN "FGCOLOR"                      THEN hBrowse:FGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "FONT"                         THEN hBrowse:FONT = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "FRAME"                        THEN hBrowse:FRAME = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).        
    WHEN "HELP"                         THEN hBrowse:HELP = ENTRY(2,ENTRY(ix,icProperties),"|").
    WHEN "HEIGHT-PIXELS"                THEN hBrowse:HEIGHT-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "HEIGHT-CHARS"                 THEN hBrowse:HEIGHT-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "HIDDEN"                       THEN hBrowse:HIDDEN = TRUE.     
    WHEN "LABELS"                       THEN hBrowse:LABELS = TRUE.     
    WHEN "MOVABLE"                      THEN hBrowse:MOVABLE = TRUE.
    WHEN "MIN-COLUMN-WIDTH-PIXELS"      THEN hBrowse:MIN-COLUMN-WIDTH-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).        
    WHEN "MENU-MOUSE"                   THEN hBrowse:MENU-MOUSE = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).     
    WHEN "MIN-COLUMN-WIDTH-CHARS"       THEN hBrowse:MIN-COLUMN-WIDTH-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")). 
    WHEN "MENU-KEY"                     THEN hBrowse:MENU-KEY = ENTRY(2,ENTRY(ix,icProperties),"|").    
    WHEN "MAX-DATA-GUESS"               THEN hBrowse:MAX-DATA-GUESS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")). 
    WHEN "NUM-LOCKED-COLUMNS"           THEN hBrowse:NUM-LOCKED-COLUMNS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).     
    WHEN "NAME"                         THEN hBrowse:NAME = ENTRY(2,ENTRY(ix,icProperties),"|").
    WHEN "NO-EMPTY-SPACE"               THEN hBrowse:NO-EMPTY-SPACE = TRUE.     
    WHEN "PRIVATE-DATA"                 THEN hBrowse:PRIVATE-DATA = ENTRY(2,ENTRY(ix,icProperties),"|").
    WHEN "PARENT"                       THEN hBrowse:PARENT = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).       
    WHEN "POPUP-MENU"                   THEN hBrowse:POPUP-MENU = WIDGET-HANDLE(ENTRY(2,ENTRY(ix,icProperties),"|")).   
    WHEN "ROW-HEIGHT-CHARS"             THEN hBrowse:ROW-HEIGHT-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).   
    WHEN "RESIZABLE"                    THEN hBrowse:RESIZABLE = TRUE.
    WHEN "ROW-RESIZABLE"                THEN hBrowse:ROW-RESIZABLE = TRUE.      
    WHEN "ROW-HEIGHT-PIXELS"            THEN hBrowse:ROW-HEIGHT-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).      
    WHEN "REFRESHABLE"                  THEN hBrowse:REFRESHABLE = TRUE.        
    WHEN "READ-ONLY"                    THEN hBrowse:READ-ONLY = TRUE.
    WHEN "SELECTABLE"                   THEN hBrowse:SELECTABLE = TRUE. 
    WHEN "SCROLLBAR-VERTICAL"           THEN hBrowse:SCROLLBAR-VERTICAL = TRUE. 
    WHEN "SENSITIVE"                    THEN hBrowse:SENSITIVE = TRUE.  
    WHEN "SEPARATORS"                   THEN hBrowse:SEPARATORS = TRUE. 
    WHEN "SEPARATOR-FGCOLOR"            THEN hBrowse:SEPARATOR-FGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).      
    WHEN "TITLE-BGCOLOR"                THEN hBrowse:TITLE-BGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).  
    WHEN "TITLE-FGCOLOR"                THEN hBrowse:TITLE-FGCOLOR = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).  
    WHEN "TITLE-FONT"                   THEN hBrowse:TITLE-FONT = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).     
    WHEN "TOOLTIP"                      THEN hBrowse:TOOLTIP = ENTRY(2,ENTRY(ix,icProperties),"|").     
    WHEN "TAB-STOP"                     THEN hBrowse:TAB-STOP = TRUE.       
    WHEN "TAB-POSITION"                 THEN hBrowse:TAB-POSITION = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).   
    WHEN "TITLE"                        THEN hBrowse:TITLE = ENTRY(2,ENTRY(ix,icProperties),"|").       
    WHEN "VIEW-FIRST-COLUMN-ON-REOPEN"  THEN hBrowse:VIEW-FIRST-COLUMN-ON-REOPEN= TRUE. 
    WHEN "VISIBLE"                      THEN hBrowse:VISIBLE = TRUE.
    WHEN "WIDTH-PIXELS"                 THEN hBrowse:WIDTH-PIXELS = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "WIDTH-CHARS"                  THEN hBrowse:WIDTH-CHARS = DEC(ENTRY(2,ENTRY(ix,icProperties),"|")).
    WHEN "X"                            THEN hBrowse:X = INT(ENTRY(2,ENTRY(ix,icProperties),"|")).
  END CASE.
END.

IF NOT CAN-DO(icProperties,"!FIT-LAST-COLUMN") THEN
  hBrowse:FIT-LAST-COLUMN = TRUE.

httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
httTableQuery:QUERY-OPEN.

RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

