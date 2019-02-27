&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : jbserv_api_for_server.p
    Purpose     :

    Syntax      :

    Description : Replicate functions in the client-side appserver interface
                  for the server to enable a subset of the calls.

    Author(s)   : brynjar@chemistry.no
    Created     : 20.12.10
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT PARAM icSessionId  AS CHAR NO-UNDO.

DEF VAR ocReturn                AS CHAR   NO-UNDO.
DEF VAR bOK                     AS LOG    NO-UNDO.
DEF VAR cExtraFieldList         AS CHAR   NO-UNDO.
DEF VAR cExtraValuesList        AS CHAR   NO-UNDO.
DEF VAR hBuffer                 AS HANDLE NO-UNDO.
DEF VAR httPreTrans             AS HANDLE NO-UNDO.
DEF VAR hBuffPreTrans           AS HANDLE NO-UNDO.
DEF VAR cCurrAction             AS CHAR   NO-UNDO.
DEF VAR ix                      AS INT    NO-UNDO.
DEF VAR cReturnTrans            AS CHAR   NO-UNDO.
DEF VAR cViewFileNamePrefix     AS CHAR   NO-UNDO.
DEF VAR httRunProc              AS HANDLE NO-UNDO.  /* Handle to tt returned from runProc */
DEF VAR hdsRunProc              AS HANDLE NO-UNDO.  /* Handle to ds returned from runProcDs */
DEF VAR cReturnParam            AS CHAR   NO-UNDO.
DEF VAR cInputParam             AS CHAR   NO-UNDO.
DEF VAR cContext                AS CHAR   NO-UNDO.
DEF VAR cReportFileName         AS CHAR   NO-UNDO.
DEF VAR cDocFileDates           AS CHAR   NO-UNDO.
DEF VAR cDocFileDescs           AS CHAR   NO-UNDO.
DEF VAR cDocFileIds             AS CHAR   NO-UNDO.
DEF VAR cDocFileTypes           AS CHAR   NO-UNDO.
DEF VAR dDocLoadFileDate        AS DATE   NO-UNDO.
DEF VAR iDocLoadFileTime        AS INT    NO-UNDO.
DEF VAR cDocSeqList             AS CHAR   NO-UNDO.
DEF VAR cNoCompressTypes        AS CHAR NO-UNDO INIT "bmp,gif,jpg,jpeg,mp3,tiff,tif,rar,gz,zip,wma,wmv".
DEF VAR bCompressFiles          AS LOG  NO-UNDO INIT TRUE.
DEF VAR cGZIP                   AS CHAR NO-UNDO.
DEF VAR cOutParam               AS CHAR NO-UNDO.
DEF VAR cTmpDocFileNames        AS CHAR NO-UNDO.
DEF VAR cOrgDocFileNames        AS CHAR NO-UNDO.
DEF VAR bUseOrgFileName         AS LOG  NO-UNDO.
DEF VAR bSupprOpenDocMsg        AS LOG  NO-UNDO.
DEF VAR cSupprOpenDocMsg        AS CHAR NO-UNDO.
DEF VAR cSaveDir                AS CHAR NO-UNDO.
DEF VAR cOpenFileErrorList      AS CHAR NO-UNDO.

&IF "{&OPSYS}" = "WIN32" &THEN
  DEF VAR oCmd AS JBoxCmd NO-UNDO.
&ENDIF

{incl/validatesession.i}

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

DEF VAR cServerTransInputParam    AS CHAR NO-UNDO.
DEF VAR cTransLogFile             AS CHAR NO-UNDO.
DEF VAR cServerTransReturnParam   AS CHAR NO-UNDO.
DEF VAR cPostUpdProc              AS CHAR NO-UNDO.
DEF VAR cAppTitle                 AS CHAR NO-UNDO.
DEF VAR cASuserName               AS CHAR NO-UNDO.

/* Doc handling: */
DEF VAR httDoc          AS HANDLE NO-UNDO.
DEF TEMP-TABLE ttDoc NO-UNDO
    FIELD cFileName       AS CHAR
    FIELD cFullPathName   AS CHAR
    FIELD cFileType       AS CHAR
    FIELD cDescription    AS CHAR
    FIELD iDocSize        AS INT
    FIELD blDocument      AS BLOB
    FIELD dFileCreateDate AS DATE
    FIELD iFileCreateTime AS INT
    FIELD dFileModDate    AS DATE
    FIELD iFileModTime    AS INT
    FIELD cCreatedBy      AS CHAR
    FIELD dCreated        AS DATE
    FIELD cContext        AS CHAR
    FIELD cEntityId       AS CHAR
    FIELD iSeq            AS INT
    .
DEF VAR cDocLoadParam  AS CHAR NO-UNDO.

httDoc = BUFFER ttDoc:HANDLE:TABLE-HANDLE.

DEF STREAM strmDoc.
DEF STREAM strmFile.

DEF VAR bChunkInProgress AS LOG    NO-UNDO.
DEF VAR httDocInfoBuf    AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttDocInfo NO-UNDO
    FIELD iJBoxDocumentId AS INT
    FIELD iDocSize        AS INT
    FIELD cFileName       AS CHAR
    FIELD cFileType       AS CHAR
    FIELD cDescription    AS CHAR
    FIELD dFileModDate    AS DATE
    FIELD cCat            AS CHAR
    FIELD blChunk         AS BLOB
    .
httDocInfoBuf = BUFFER ttDocInfo:HANDLE.

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

&IF DEFINED(EXCLUDE-CheckReturnValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckReturnValue Procedure 
FUNCTION CheckReturnValue RETURNS LOGICAL
  ( INPUT icReturnTrans AS CHAR )  FORWARD.

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
  ( INPUT icFieldList     AS CHAR,
    INPUT icFieldsToMatch AS CHAR,
    INPUT icValuesToMatch AS CHAR )  FORWARD.

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

&IF DEFINED(EXCLUDE-ExportMemptrDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExportMemptrDoc Procedure 
FUNCTION ExportMemptrDoc RETURNS LOGICAL
  ( INPUT impDocument AS MEMPTR,
    INPUT ibOpen      AS LOG,
    INPUT icOutFile   AS CHAR,
    INPUT icSaveDir   AS CHAR,
    INPUT icFileName  AS CHAR )  FORWARD.

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

&IF DEFINED(EXCLUDE-getASlibHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASlibHandle Procedure 
FUNCTION getASlibHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASuserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASuserId Procedure 
FUNCTION getASuserId RETURNS CHARACTER
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

&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodeMasterId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodeMasterId Procedure 
FUNCTION getCodeMasterId RETURNS INTEGER
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

&IF DEFINED(EXCLUDE-getContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContext Procedure 
FUNCTION getContext RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
  (INPUT icBuffersAndFields AS CHAR,
   INPUT icQueryCriteria    AS CHAR) FORWARD.

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
  (INPUT icBufferName    AS CHAR,
   INPUT icQueryCriteria AS CHAR,
   INPUT icColumns       AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayOfMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFirstDayOfMonth Procedure 
FUNCTION getFirstDayOfMonth RETURNS DATE
  ( input ipDate as date ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayOfMonthSomeMonthsBack) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFirstDayOfMonthSomeMonthsBack Procedure 
FUNCTION getFirstDayOfMonthSomeMonthsBack RETURNS DATE
  ( input ipDate as date, ipMonthsBack as integer ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayOfPriorMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFirstDayOfPriorMonth Procedure 
FUNCTION getFirstDayOfPriorMonth RETURNS DATE
  ( input ipDate as date ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInputParam Procedure 
FUNCTION getInputParam RETURNS CHARACTER
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

&IF DEFINED(EXCLUDE-getMyDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyDataSet Procedure 
FUNCTION getMyDataSet RETURNS LOGICAL
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetDS       AS HANDLE )  FORWARD.

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

&IF DEFINED(EXCLUDE-getNoCompressTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoCompressTypes Procedure 
FUNCTION getNoCompressTypes RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrgDocFileNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrgDocFileNames Procedure 
FUNCTION getOrgDocFileNames RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOutParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOutParam Procedure 
FUNCTION getOutParam RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOutputParam Procedure 
FUNCTION getOutputParam RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPreTransBuffer Procedure 
FUNCTION getPreTransBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  ( INPUT ihBuffer AS HANDLE )  FORWARD.

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

&IF DEFINED(EXCLUDE-getReportFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReportFileName Procedure 
FUNCTION getReportFileName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&IF DEFINED(EXCLUDE-getTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTempTable Procedure 
FUNCTION getTempTable RETURNS HANDLE
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetBuffer   AS HANDLE ) FORWARD.

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

&IF DEFINED(EXCLUDE-getUniqueFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUniqueFileName Procedure 
FUNCTION getUniqueFileName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue Procedure 
FUNCTION getValue RETURNS CHARACTER
  ( INPUT icField AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekDiff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekDiff Procedure 
FUNCTION getWeekDiff RETURNS INTEGER
  ( INPUT iiStartWeek AS INT,
    INPUT iiEndWeek   AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekNum Procedure 
FUNCTION getWeekNum RETURNS INTEGER
  ( INPUT idSomeDate     AS DATE,
    INPUT iiOutputLength AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayInMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LastDayInMonth Procedure 
FUNCTION LastDayInMonth RETURNS DATE
  ( INPUT iiYear  AS INT,
    INPUT iiMonth AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadDocs Procedure 
FUNCTION LoadDocs RETURNS LOGICAL
  ( INPUT icFileNames   AS CHAR,
    INPUT icContext     AS CHAR,
    INPUT icEntityId    AS CHAR,
    INPUT icDescription AS CHAR )  FORWARD.

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

&IF DEFINED(EXCLUDE-setContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setContext Procedure 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadFileDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocLoadFileDate Procedure 
FUNCTION setDocLoadFileDate RETURNS LOGICAL
  ( INPUT idDocLoadFileDate AS DATE,
    INPUT iiDocLoadFileTime AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocLoadParam Procedure 
FUNCTION setDocLoadParam RETURNS LOGICAL
  ( INPUT icDocLoadParam AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocLoadProc Procedure 
FUNCTION setDocLoadProc RETURNS LOGICAL
  ( INPUT icDocUploadProc    AS CHAR,
    INPUT ibSaveDocFileToDeb AS LOG,
    INPUT icDocLoadParam     AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadSeqList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocLoadSeqList Procedure 
FUNCTION setDocLoadSeqList RETURNS LOGICAL
  ( INPUT icDocSeqList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setExtraFieldsAndValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setExtraFieldsAndValues Procedure 
FUNCTION setExtraFieldsAndValues RETURNS LOGICAL
  ( INPUT icExtraFieldList  AS CHAR,
    INPUT icExtraValuesList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setInputParam Procedure 
FUNCTION setInputParam RETURNS LOGICAL
  ( INPUT icInputParam AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setOutputParam Procedure 
FUNCTION setOutputParam RETURNS LOGICAL
  ( INPUT ocReturnParam AS CHAR )  FORWARD.

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

&IF DEFINED(EXCLUDE-setPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPreTransBuffer Procedure 
FUNCTION setPreTransBuffer RETURNS LOGICAL
  ( INPUT ihCurrBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReportFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setReportFileName Procedure 
FUNCTION setReportFileName RETURNS LOGICAL
  ( INPUT icReportFileName AS CHAR )  FORWARD.

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

&IF DEFINED(EXCLUDE-setSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSessionId Procedure 
FUNCTION setSessionId RETURNS LOGICAL
  (INPUT icNewSessionId AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startASlib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startASlib Procedure 
FUNCTION startASlib RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewDocs Procedure 
FUNCTION ViewDocs RETURNS LOGICAL
  ( INPUT icContext  AS CHAR,
    INPUT icEntityId AS CHAR,
    INPUT ibOpen     AS LOG,   
    INPUT icSaveDir  AS CHAR)  FORWARD.

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
         HEIGHT             = 24.67
         WIDTH              = 73.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF ocReturn = "Invalid session" THEN DELETE PROCEDURE THIS-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InvokeCreateProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvokeCreateProc Procedure 
PROCEDURE InvokeCreateProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icCreateProc  AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer      AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

IF icCreateProc NE "" THEN DO:
  cCurrAction = "create".

  IF SEARCH(icCreateProc) = ? AND SEARCH(SUBSTR(icCreateProc,1,LENGTH(icCreateProc) - 1) + "r") = ? THEN 
    ocReturn = "Couldn't find server create program: " + icCreateProc.
  ELSE 
    RUN VALUE(icCreateProc) (ihBuffer,cExtraFieldList,cExtraValuesList,icSessionId,OUTPUT ocReturn). 
END.

ASSIGN cExtraFieldList  = ""
       cExtraValuesList = ""
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InvokeDeleteValProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvokeDeleteValProc Procedure 
PROCEDURE InvokeDeleteValProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icDeleteValProc AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

DEF VAR cBufferName AS CHAR NO-UNDO.

IF icDeleteValProc NE "" THEN DO:
  ASSIGN cBufferName = ihBuffer:NAME
         hBuffer     = ihBuffer
         cCurrAction = "delete"
         .
  setPreTransBuffer(ihBuffer).

  IF SEARCH(icDeleteValProc) = ? AND SEARCH(SUBSTR(icDeleteValProc,1,LENGTH(icDeleteValProc) - 1) + "r") = ? THEN 
    ocReturn = "Couldn't find server delete validation program: " + icDeleteValProc.
  ELSE 
    RUN VALUE(icDeleteValProc) (cBufferName,STRING(ihBuffer:ROWID),icSessionId,OUTPUT ocReturn).
END.

ASSIGN cExtraFieldList  = ""
       cExtraValuesList = ""
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InvokePostUpdateProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvokePostUpdateProc Procedure 
PROCEDURE InvokePostUpdateProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icPostUpdateProc AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer         AS HANDLE NO-UNDO.
DEF INPUT  PARAM icCurrAction     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn         AS CHAR NO-UNDO.

IF icPostUpdateProc NE "" THEN DO:

  IF icCurrAction = "" THEN icCurrAction = cCurrAction.

  IF SEARCH(icPostUpdateProc) = ? AND SEARCH(SUBSTR(icPostUpdateProc,1,LENGTH(icPostUpdateProc) - 1) + "r") = ? THEN 
    ocReturn = "Couldn't find server postUpdate program: " + icPostUpdateProc.
  ELSE 
    RUN VALUE(icPostUpdateProc) (IF cCurrAction = "DELETE" THEN hBuffPreTrans ELSE ihBuffer,icCurrAction,icSessionId,OUTPUT ocReturn). 
END.

ASSIGN cExtraFieldList  = ""
       cExtraValuesList = ""
       .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InvokeUpdateValProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvokeUpdateValProc Procedure 
PROCEDURE InvokeUpdateValProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icUpdateValProc AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

DEF VAR cBufferName AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR cFieldList  AS CHAR NO-UNDO.
DEF VAR cValueList  AS CHAR NO-UNDO.

IF icUpdateValProc NE "" AND ihBuffer:AVAIL THEN DO:
  ASSIGN cBufferName = ihBuffer:NAME
         hBuffer     = ihBuffer
         cCurrAction = "update"
         .
  setPreTransBuffer(ihBuffer).

  DO ix = 1 TO ihBuffer:NUM-FIELDS:
    ASSIGN cFieldList = cFieldList + (IF cFieldList NE "" THEN "," ELSE "") + ihBuffer:BUFFER-FIELD(ix):NAME
           cValueList = cValueList + (IF cValueList NE "" THEN "|" ELSE "") 
                      + (IF ihBuffer:BUFFER-FIELD(ix):BUFFER-VALUE NE ? THEN STRING(ihBuffer:BUFFER-FIELD(ix):BUFFER-VALUE) ELSE "")
                      .
  END.

  IF SEARCH(icUpdateValProc) = ? AND SEARCH(SUBSTR(icUpdateValProc,1,LENGTH(icUpdateValProc) - 1) + "r") = ? THEN 
    ocReturn = "Couldn't find server update validation program: " + icUpdateValProc.
  ELSE 
    RUN VALUE(icUpdateValProc) (STRING(ihBuffer:ROWID),cFieldList,cValueList,icSessionId,OUTPUT ocReturn).

END.

ASSIGN cExtraFieldList  = ""
       cExtraValuesList = ""
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WeekNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WeekNum Procedure 
PROCEDURE WeekNum :
/************************************************************************************
        PROCEDURE: weeknum.p

        PURPOSE:   Calculates the week-number for a given date

        SYNTAX:    RUN samples/weeknum.p (INPUT in, OUTPUT out).

        REMARKS:   This code calculates the week-number for the date given.
                   The format is YYYYWW

        PARAMETERS:
            INPUT:  date
            OUTPUT: week number

        AUTHORS:   Judy Rothermal
        DATE:      February 1993

        LAST INSPECTED:
        INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.         */
 
/*Code_Start*/

/* Assumptions:                                                     */
/* 1. Weeks start on MONDAYS                                        */
/* 2. If January 1st falls on Friday, Saturday, Sunday or Monday    */
/*    then week 1 for this year will start on the first Monday      */
/*    the same year. If not, week 1 will start on the last Monday   */
/*    previous year.                                                */
/*    (In other words: At least 4 of the seven days of week 1 for   */
/*     a given year must fall into this year)                       */


DEFINE INPUT  PARAMETER indate   AS DATE.  /* Input date , eg 10/17/90 */
DEFINE OUTPUT PARAMETER yyyyww   AS INT.   /* Output week, eg 9042     */

DEFINE VARIABLE yr   AS INT.  /* Year of indate, eg 1990      */
DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
                              /* (01/01/90 is a Monday)      */
DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
DEFINE VARIABLE wn   AS INT.  /* Week number , eg 45         */

ASSIGN
  yr   = YEAR(indate)
  d1   = WEEKDAY(DATE( 1 , 1 , yr))
  dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                          DATE(1, 10, yr) - d1 )
  wn   = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
ASSIGN
  yr     = yr - 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                            DATE(1, 10, yr) - d1 )
  wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
ASSIGN
  yr     = yr + 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
              THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.

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
DEF VAR bOk AS LOG init true NO-UNDO.

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
      RUN jbserv_assignstringvalue.p (ihField,0,icUpdateValue,OUTPUT bOk).
    ELSE bOk = False.
  END.
END CASE.

IF ERROR-STATUS:ERROR THEN bOk = false.   

RETURN bOk.

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
IF NUM-ENTRIES(icReturnTrans,"¤") > 1 THEN DO:
  ASSIGN cReturnParam = ENTRY(2,icReturnTrans,"¤")
         cReturnTrans = ENTRY(1,cReturnTrans,"¤").
END.
ELSE cReturnParam = "".

RETURN FALSE. 

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

&IF DEFINED(EXCLUDE-ExportMemptrDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExportMemptrDoc Procedure 
FUNCTION ExportMemptrDoc RETURNS LOGICAL
  ( INPUT impDocument AS MEMPTR,
    INPUT ibOpen      AS LOG,
    INPUT icOutFile   AS CHAR,
    INPUT icSaveDir   AS CHAR,
    INPUT icFileName  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Export a memptr variable to a given catalog and file name
           and decompress the file if it is compressed 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cOpenDocMsg AS CHAR NO-UNDO.
DEF VAR oServer AS COM-HANDLE NO-UNDO.
DEF VAR cDocType AS CHAR NO-UNDO.
DEF VAR cOpenUsing AS CHAR NO-UNDO.

OUTPUT STREAM strmDoc TO VALUE(icOutFile) NO-MAP BINARY NO-CONVERT.
EXPORT STREAM strmDoc impDocument.
OUTPUT STREAM strmDoc CLOSE.

IF bCompressFiles AND cGZIP NE ? AND icOutFile MATCHES "*.gz" THEN DO:
/*  OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' -df "' + icOutFile + '"').*/
/*  oCMD = NEW jboxcmd(SEARCH("gzip.exe"),'-df ' + quoter(icOutFile)).*/
  &IF "{&OPSYS}" = "WIN32" &then
    oCMD = NEW JBoxCmd(SEARCH("gzip.exe"),'-df ' + QUOTER(icOutFile)).
  &else
    OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' -df "' + icOutFile + '"').
  &ENDIF
  
  IF icOutFile NE icSaveDir + icFileName AND
     SEARCH(icSaveDir + RIGHT-TRIM(icFileName,".gz")) NE ? THEN DO:
    OS-DELETE VALUE(icOutFile).
    icOutFile = icSaveDir + RIGHT-TRIM(icFileName,".gz").
  END.
  ELSE IF icOutFile MATCHES "*.gz" THEN icOutFile = SUBSTR(icOutFile,1,LENGTH(icOutFile) - 3).
END.

IF ibOpen THEN DO:
  cDocType = ENTRY(NUM-ENTRIES(icOutFile,"."),icOutFile,".").
  cOpenUsing = DYNAMIC-FUNCTION("getAttribute",SESSION,"ProgramToOpenFilType" + cDocType).
  IF cOpenUsing NE "" AND SEARCH(cOpenUsing) NE ? THEN DO:
    FILE-INFO:FILE-NAME = SEARCH(cOpenUsing).
    OS-COMMAND NO-WAIT VALUE(FILE-INFO:FULL-PATHNAME + " " + icOutFile).
  END.
  ELSE DO: 
    cOpenDocMsg = DYNAMIC-FUNCTION('setWebDoc','open',icOutFile).
    IF NOT bSupprOpenDocMsg THEN DO:
      IF cOpenDocMsg BEGINS "There is no" THEN DO:
        IF DYNAMIC-FUNCTION("DoMessage",0,4,cOpenDocMsg + CHR(10) + "Filename: " + icOutFile + CHR(10)
                                      + "Open in default text editor?"
                                       ,"","") = 6 THEN DO:
          OS-COPY VALUE(icOutFile) VALUE(icOutFile + ".txt").
          DYNAMIC-FUNCTION('setWebDoc','open',icOutFile + ".txt").
        END.
        ELSE IF icSaveDir NE "" THEN DO:
          CREATE 'Shell.Application' oServer.
          NO-RETURN-VALUE oServer:Explore(icSaveDir).
          RELEASE OBJECT oServer.
        END.    
      END.
      ELSE IF cOpenDocMsg NE "" THEN
        DYNAMIC-FUNCTION("DoMessage",0,0,cOpenDocMsg + CHR(10) + "Filename: " + icOutFile,"","").
    END.
    ELSE DO:
      IF cSupprOpenDocMsg = "" THEN 
        ASSIGN cSupprOpenDocMsg = cOpenDocMsg
               cSaveDir = icSaveDir.
      cOpenFileErrorList = cOpenFileErrorList + (IF cOpenFileErrorList NE "" THEN CHR(10) ELSE "") + ENTRY(NUM-ENTRIES(icOutFile,"\"),icOutFile,"\").         
    END.
  END.                 
END.

RETURN YES.

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

&IF DEFINED(EXCLUDE-getASlibHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getASlibHandle Procedure 
FUNCTION getASlibHandle RETURNS HANDLE
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

&IF DEFINED(EXCLUDE-getASuserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getASuserId Procedure 
FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cCurrUserId. 

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

&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN STRING(iCodeMaster).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCodeMasterId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodeMasterId Procedure 
FUNCTION getCodeMasterId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCodeMaster.

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

RETURN STRING(iCurrCompanyId).

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

RETURN iCurrCompanyId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContext Procedure 
FUNCTION getContext RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* DEF VAR cContext AS CHAR NO-UNDO.                              */
/*                                                                */
/* RUN jbserv_getsessioncontext.p (icSessionId,OUTPUT cContext).  */

RETURN cContext.

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

RETURN cCurrAction.

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

IF NUM-ENTRIES(icDbField,".") = 3 THEN 
  ASSIGN cDbName    = ENTRY(1,icDbField,".")
         cTableName = ENTRY(2,icDbField,".")
         cFieldName = ENTRY(3,icDbField,".")
         .

ELSE IF NUM-ENTRIES(icDbField,".") = 2 THEN
  ASSIGN cTableName = ENTRY(1,icDbField,".")
         cFieldName = ENTRY(2,icDbField,".")
         .
ELSE 
  cFieldName = icDbField.

RUN jbserv_getfielddatatype.p
   (DYNAMIC-FUNCTION("getSessionId"),
    cDbName,
    cTableName,
    cFieldName,
    OUTPUT cDataType).
  
RETURN cDataType.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldList Procedure 
FUNCTION getFieldList RETURNS CHARACTER
  (INPUT icBuffersAndFields AS CHAR,
   INPUT icQueryCriteria    AS CHAR):
  
DEF VAR cReturnTrans AS CHAR NO-UNDO.
DEF VAR cFieldPairs  AS CHAR NO-UNDO.

RUN jbserv_getfieldlist.p
   (icSessionId,
    icBuffersAndFields,
    icQueryCriteria,
    OUTPUT cFieldPairs,
    OUTPUT cReturnTrans).

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

RUN jbserv_getfieldsfortable.p
   (icSessionId,
    cDbName,
    cTableName,
    OUTPUT cFields).

RETURN cFields.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldValues Procedure 
FUNCTION getFieldValues RETURNS CHARACTER
  (INPUT icBufferName    AS CHAR,
   INPUT icQueryCriteria AS CHAR,
   INPUT icColumns       AS CHAR):
  
  DEF VAR cReturnTrans AS CHAR NO-UNDO.
  DEF VAR cValues      AS CHAR NO-UNDO.

  RUN jbserv_getfieldvalues.p
     (icSessionId,
      icBufferName,
      icQueryCriteria,
      icColumns,
      OUTPUT cValues,
      OUTPUT cReturnTrans)
      .

  RETURN cValues.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayOfMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFirstDayOfMonth Procedure 
FUNCTION getFirstDayOfMonth RETURNS DATE
  ( input ipDate as date ):
/* Greg Higgins ghiggins@ultramain.com */

  define variable tmpResult as date no-undo.

  assign
    tmpResult = ipDate - day ( ipDate ) + 1
    .
  return tmpResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayOfMonthSomeMonthsBack) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFirstDayOfMonthSomeMonthsBack Procedure 
FUNCTION getFirstDayOfMonthSomeMonthsBack RETURNS DATE
  ( input ipDate as date, ipMonthsBack as integer ):
/* Greg Higgins ghiggins@ultramain.com */
  define variable tmpResult as date no-undo.

  assign
    ipMonthsBack = abs ( ipMonthsBack )
    tmpResult    = ipDate
    .
  do while ipMonthsBack ge 0 :
    assign
      tmpResult    = getFirstDayOfMonth ( tmpResult )
      tmpResult    = tmpResult - 1 when ipMonthsBack gt 0 
      ipMonthsBack = ipMonthsBack - 1
      .
  end.
  return tmpResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayOfPriorMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFirstDayOfPriorMonth Procedure 
FUNCTION getFirstDayOfPriorMonth RETURNS DATE
  ( input ipDate as date ):
/* Greg Higgins ghiggins@ultramain.com */
  define variable tmpResult as date no-undo.

  assign
    tmpResult = getFirstDayOfMonth ( ipDate )
    tmpResult = getFirstDayOfMonth ( tmpResult - 1 )
    .
  return tmpResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInputParam Procedure 
FUNCTION getInputParam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cInputParam.

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

RETURN cCurrLanguage.

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

RUN jbserv_runproc_ds.p
   (icSessionId,
    icServerProgram,
    icParamList,
    INPUT-OUTPUT DATASET-HANDLE ihTargetDS,
    OUTPUT cReturnTrans,
    OUTPUT obOk)
    .

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

httTable = ihTargetBuffer:TABLE-HANDLE.

RUN jbserv_runproc.p
   (icSessionId,
    icServerProgram,
    icParamList,
    INPUT-OUTPUT TABLE-HANDLE httTable,
    OUTPUT cReturnTrans,
    OUTPUT obOk)
    .

CheckReturnValue(cReturnTrans).

RETURN obOk.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNoCompressTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoCompressTypes Procedure 
FUNCTION getNoCompressTypes RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cNoCompressTypes.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrgDocFileNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrgDocFileNames Procedure 
FUNCTION getOrgDocFileNames RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Return original file name(s) for last retrieved document(s) 
    Notes:  
------------------------------------------------------------------------------*/

RETURN cOrgDocFileNames.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOutParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOutParam Procedure 
FUNCTION getOutParam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Returns the output parameter from last function call. 
    Notes:  
------------------------------------------------------------------------------*/

RETURN cOutParam.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOutputParam Procedure 
FUNCTION getOutputParam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cReturnParam.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPreTransBuffer Procedure 
FUNCTION getPreTransBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBuffPreTrans.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  ( INPUT ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPKfields      AS CHAR NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.

DO ix = 1 TO 100:
  IF ihBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
    IF ENTRY(2,ihBuffer:INDEX-INFORMATION(ix)) = "1" AND ENTRY(3,ihBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
      DO iy = 5 TO NUM-ENTRIES(ihBuffer:INDEX-INFORMATION(ix)) BY 2:
        cPKfields = cPKfields + ENTRY(iy,ihBuffer:INDEX-INFORMATION(ix)) + ",".
      END.
      LEAVE.
    END.
  END.
  ELSE LEAVE.
END.  
IF cPKfields = "" THEN
  DO ix = 1 TO 100:
    IF ihBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
      IF ENTRY(2,ihBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
        DO iy = 5 TO NUM-ENTRIES(ihBuffer:INDEX-INFORMATION(ix)) BY 2:
          cPKfields = cPKfields + ENTRY(iy,ihBuffer:INDEX-INFORMATION(ix)) + ",".
        END.
        LEAVE.
      END.
    END.
    ELSE LEAVE.
  END.  


RETURN TRIM(cPKfields,",").

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

  RUN jbserv_getrecordcount.p
     (icSessionId,
      icBuffer,
      icQueryCriteria,
      OUTPUT iRecCount,
      OUTPUT cReturnTrans)
      .

RETURN iRecCount.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getReportFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReportFileName Procedure 
FUNCTION getReportFileName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cReportFileName.

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

RETURN STRING(iCurrCompanyId).

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

RETURN iCurrCompanyId.

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

RUN jbserv_getrowidlist.p
     (icSessionId,
      icBufferList,
      icBufferReturnList,
      icQueryCriteria,
      OUTPUT cRowIdList,
      OUTPUT cReturnTrans)
      .


CheckReturnValue(cReturnTrans).

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

RETURN cReturnParam.

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
RETURN cCurrContext.
/*
RETURN DYNAMIC-FUNCTION("getFieldValues","JBoxLoginSession",
                        "WHERE cSessionId = '" + DYNAMIC-FUNCTION("getSessionId") + "'",
                        "cContext").
*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTempTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTempTable Procedure 
FUNCTION getTempTable RETURNS HANDLE
  ( INPUT icServerProgram  AS CHAR,
    INPUT icParamList      AS CHAR,
    INPUT ihTargetBuffer   AS HANDLE ):
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

RUN VALUE(icServerProgram)
   (icSessionId,
    icParamList,
    OUTPUT TABLE-HANDLE httTable,
    OUTPUT cReturnTrans)
    .

bOK = SESSION:SET-WAIT-STATE("").

CheckReturnValue(cReturnTrans).

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

&IF DEFINED(EXCLUDE-getUniqueFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUniqueFileName Procedure 
FUNCTION getUniqueFileName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN SESSION:TEMP-DIRECTORY + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(RANDOM(1,10000)) + STRING(TIME) + ".tmp".   

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
DEF VAR cUserLevel AS CHAR NO-UNDO.
RUN jbadmin_getuserlevel.p (icSessionId,iCurrCompanyId,OUTPUT cUserLevel,OUTPUT bOK).
  
RETURN cUserLevel. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue Procedure 
FUNCTION getValue RETURNS CHARACTER
  ( INPUT icField AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF LOOKUP(icField,cExtraFieldList) > 0 THEN
  RETURN ENTRY(LOOKUP(icField,cExtraFieldList),cExtraValuesList,"|").
ELSE IF VALID-HANDLE(hBuffer) AND hBuffer:AVAIL AND VALID-HANDLE(hBuffer:BUFFER-FIELD(icField)) THEN
  RETURN STRING(hBuffer:BUFFER-FIELD(icField):BUFFER-VALUE).
ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekDiff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekDiff Procedure 
FUNCTION getWeekDiff RETURNS INTEGER
  ( INPUT iiStartWeek AS INT,
    INPUT iiEndWeek   AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: Calculate number of weeks between weeks 
    Notes: Requires weeknum as yyyyww 
------------------------------------------------------------------------------*/
DEF VAR iStartYear AS INT NO-UNDO.
DEF VAR iEndYear   AS INT NO-UNDO.
DEF VAR iCount     AS INT NO-UNDO.

IF iiStartWeek = 0 AND iiEndWeek NE 0 THEN
  iiStartWeek = iiEndWeek.
ELSE IF iiEndWeek = 0 AND iiStartWeek NE 0 THEN
  iiEndWeek = iiStartWeek.
ELSE IF iiStartWeek = 0 AND iiEndWeek = 0 THEN
  RETURN 0.

ASSIGN iStartYear = INT(SUBSTR(STRING(iiStartWeek),1,4))
       iEndYear   = INT(SUBSTR(STRING(iiEndWeek),1,4))
       .

DO ix = iStartYear TO iEndYear:
  IF ix < iEndYear THEN DO:
    IF ix > iStartYear THEN
      iCount = iCount + (getWeekNum(LastDayInMonth(ix,12),6) - getWeekNum(DATE(1,1,ix),6)).
    ELSE
      iCount = iCount + (getWeekNum(LastDayInMonth(ix,12),6) - iiStartWeek).
  END.
  ELSE DO:
    IF ix > iStartYear THEN 
      iCount = iCount + (iiEndWeek - INT(STRING(ix) + "00")).
    ELSE
      iCount = iCount + (iiEndWeek - iiStartWeek).
  END.
/*   MESSAGE PROGRAM-NAME(1) SKIP             */
/*           "iiStartWeek: " iiStartWeek SKIP */
/*           "iiEndWeek: " iiEndWeek SKIP     */
/*           ix SKIP                          */
/*           iCount                           */
/*           VIEW-AS ALERT-BOX.               */
END.

RETURN iCount.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekNum Procedure 
FUNCTION getWeekNum RETURNS INTEGER
  ( INPUT idSomeDate     AS DATE,
    INPUT iiOutputLength AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iWeekNum AS INT NO-UNDO.

RUN WeekNum (idSomeDate, OUTPUT iWeekNum).

IF iWeekNum NE ? THEN
  CASE iiOutputLength:
    WHEN 2 THEN RETURN INT(SUBSTR(STRING(iWeekNum),5)).
    WHEN 4 THEN RETURN INT(SUBSTR(STRING(iWeekNum),3)).
    OTHERWISE RETURN iWeekNum.
  END CASE.
ELSE RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LastDayInMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LastDayInMonth Procedure 
FUNCTION LastDayInMonth RETURNS DATE
  ( INPUT iiYear  AS INT,
    INPUT iiMonth AS INT):

 DEF VAR ixD      AS DATE NO-UNDO.
 DEF VAR iLastDay AS INT NO-UNDO.

 IF iiMonth NE 12 THEN DO ixD = DATE(iiMonth,28,iiYear) TO DATE(iiMonth,28,iiYear) + 4:
   IF MONTH(ixD) NE iiMonth THEN DO:
     RETURN ixD - 1.
   END.
 END.
 ELSE RETURN DATE(12,31,iiYear).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadDocs Procedure 
FUNCTION LoadDocs RETURNS LOGICAL
  ( INPUT icFileNames   AS CHAR,
    INPUT icContext     AS CHAR,
    INPUT icEntityId    AS CHAR,
    INPUT icDescription AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTemp     AS CHAR NO-UNDO.
DEF VAR crBilde   AS CHAR NO-UNDO.
DEF VAR cFileName AS CHAR NO-UNDO.
DEF VAR bCompress AS LOG  NO-UNDO.

&IF "{&OPSYS}" = "WIN32" &THEN
  DEF VAR oCmd AS JBoxCmd NO-UNDO.
&ENDIF

IF NUM-ENTRIES(icDescription,"|") NE NUM-ENTRIES(icFileNames,";") AND icDescription MATCHES "*|*" THEN RETURN NO.

bOK = TRUE.

EMPTY TEMP-TABLE ttDoc.

icFileNames = TRIM(icFileNames,";").
DO ix = 1 TO NUM-ENTRIES(icFileNames,";"):
  FILE-INFO:FILE-NAME = ENTRY(ix,icFileNames,";").

  CREATE ttDoc.
  ASSIGN ttDoc.cFileName       = SUBSTR(ENTRY(ix,icFileNames,";"),R-INDEX(ENTRY(ix,icFileNames,";"),"\") + 1)
         ttDoc.cFullPathName   = FILE-INFO:FULL-PATHNAME
         ttDoc.cFileType       = SUBSTR(ENTRY(ix,icFileNames,";"),R-INDEX(ENTRY(ix,icFileNames,";"),".") + 1)
         ttDoc.cDescription    = (IF NUM-ENTRIES(icDescription,"|") = NUM-ENTRIES(icFileNames,";") THEN ENTRY(ix,icDescription,"|") ELSE icDescription)
         ttDoc.iDocSize        = FILE-INFO:FILE-SIZE
         ttDoc.dFileCreateDate = IF dDocLoadFileDate NE ? THEN dDocLoadFileDate ELSE FILE-INFO:FILE-CREATE-DATE
         ttDoc.iFileCreateTime = IF iDocLoadFileTime NE 0 THEN iDocLoadFileTime ELSE FILE-INFO:FILE-CREATE-TIME
         ttDoc.dFileModDate    = IF dDocLoadFileDate NE ? THEN dDocLoadFileDate ELSE FILE-INFO:FILE-MOD-DATE
         ttDoc.iFileModTime    = IF iDocLoadFileTime NE 0 THEN iDocLoadFileTime ELSE FILE-INFO:FILE-MOD-TIME
         ttDoc.cCreatedBy      = DYNAMIC-FUNCTION("getASuserId")
         ttDoc.dCreated        = TODAY
         ttDoc.iSeq            = IF NUM-ENTRIES(cDocSeqList) = NUM-ENTRIES(icFileNames,";") THEN
                                   INTEGER(ENTRY(ix,cDocSeqList))
                                 ELSE ix
                               
         ttDoc.cContext        = icContext
         ttDoc.cEntityId       = icEntityId
         bCompress             = FALSE
         .

  IF bCompressFiles AND cGZIP NE ? AND NOT CAN-DO(cNoCompressTypes,SUBSTR(ENTRY(ix,icFileNames,";"),R-INDEX(ENTRY(ix,icFileNames,";"),".") + 1)) THEN DO:
    cFileName = SESSION:TEMP-DIR + ttDoc.cFileName.
    OS-COPY VALUE(FILE-INFO:FULL-PATHNAME) VALUE(cFileName).
    
    &IF "{&OPSYS}" = "WIN32" &THEN
      oCmd = NEW JBoxCmd(SEARCH("gzip.exe"),' ' + QUOTER(cFileName)).
    &ELSE
      OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' "' + cFileName + '"').
    &ENDIF

    cFileName = cFileName + ".gz".
    IF SEARCH(cFileName) NE ? THEN DO:
      FILE-INFO:FILE-NAME = cFileName.
      ASSIGN bCompress = TRUE
             ttDoc.cFileName = ttDoc.cFileName + ".gz"
             ttDoc.iDocSize  = FILE-INFO:FILE-SIZE
             .
    END.
    ELSE cFileName = FILE-INFO:FULL-PATHNAME.
  END.
  ELSE cFileName = FILE-INFO:FULL-PATHNAME.

  COPY-LOB FROM FILE cFileName TO OBJECT ttDoc.blDocument.
  IF bCompress THEN
    OS-DELETE VALUE(cFileName).
END.
ASSIGN iDocLoadFileTime = 0
       dDocLoadFileDate = ?
       cDocSeqList      = ""
       .

RUN jbdoc_savedoc.p
   (icSessionId,
    TABLE-HANDLE httDoc,
    cDocLoadParam,
    OUTPUT bOk,
    OUTPUT cOutParam)
    .

ASSIGN cDocLoadParam  = "".
  
RETURN bOk.

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
DEF VAR obOk          AS LOG    NO-UNDO.
DEF VAR httBuffer     AS HANDLE NO-UNDO.
DEF VAR httQuery      AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer AS HANDLE NO-UNDO.
DEF VAR bAccess       AS LOG    NO-UNDO INIT YES.
                                       
IF SEARCH("jbserv_check_runproc_access.p") NE ? OR SEARCH("jbserv_check_runproc_access.r") NE ? THEN
  RUN jbserv_check_runproc_access.p (icSessionId,icServerProgram,iCurrCompanyId,cCurrUserId,OUTPUT bAccess) NO-ERROR.
IF NOT bAccess THEN DO:
  cReturnTrans = "You don't have access to run this procedure: " + icServerProgram.
  RETURN NO.
END.

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

RUN jbserv_runproc.p
   (icSessionId,
    icServerProgram,
    icParamList,
    INPUT-OUTPUT TABLE-HANDLE httRunProc,
    OUTPUT cReturnTrans,
    OUTPUT obOk)
    .

IF obOk THEN cReturnParam = cReturnTrans.

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
DEF VAR bAccess       AS LOG    NO-UNDO INIT YES.
                                       
IF SEARCH("jbserv_check_runproc_access.p") NE ? OR SEARCH("jbserv_check_runproc_access.r") NE ? THEN
  RUN jbserv_check_runproc_access.p (icSessionId,icServerProgram,iCurrCompanyId,cCurrUserId,OUTPUT bAccess) NO-ERROR.
IF NOT bAccess THEN DO:
  cReturnTrans = "You don't have access to run this procedure: " + icServerProgram.
  RETURN NO.
END.
                                       
DELETE OBJECT hdsRunProc NO-ERROR.

IF VALID-HANDLE(ihDataset) THEN DO:
  CREATE DATASET hdsRunProc.
  hdsRunProc:CREATE-LIKE(ihDataset).
  hdsRunProc:COPY-DATASET(ihDataset).
END.

RUN jbserv_runproc_ds.p
   (icSessionId,
    icServerProgram,
    icParamList,
    INPUT-OUTPUT DATASET-HANDLE hdsRunProc,
    OUTPUT cReturnTrans,
    OUTPUT obOk)
    .

IF obOk THEN cReturnParam = cReturnTrans.

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
IF CAN-DO("da,dk,no,sv,se,nn,",cCurrLanguage) THEN
  RETURN TRUE.
ELSE
  RETURN FALSE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setContext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setContext Procedure 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cContext = icContext.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadFileDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocLoadFileDate Procedure 
FUNCTION setDocLoadFileDate RETURNS LOGICAL
  ( INPUT idDocLoadFileDate AS DATE,
    INPUT iiDocLoadFileTime AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN dDocLoadFileDate = idDocLoadFileDate
       iDocLoadFileTime = iiDocLoadFileTime
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocLoadParam Procedure 
FUNCTION setDocLoadParam RETURNS LOGICAL
  ( INPUT icDocLoadParam AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Used to assign a load procedure to pass to the appserver (or run on the client)
           in conjunction with loading a file to the database
    Notes: The load procedure decides wether the document itself should be loaded to the database
------------------------------------------------------------------------------*/
cDocLoadParam = icDocLoadParam.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocLoadProc Procedure 
FUNCTION setDocLoadProc RETURNS LOGICAL
  ( INPUT icDocUploadProc    AS CHAR,
    INPUT ibSaveDocFileToDeb AS LOG,
    INPUT icDocLoadParam     AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
ASSIGN cDocLoadParam = "docUploadProc" + (IF ibSaveDocFileToDeb THEN "SaveDocToDb" ELSE "")
                     + CHR(1) + icDocUploadProc 
                     + CHR(1) + icDocLoadParam
                     .
                      
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDocLoadSeqList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocLoadSeqList Procedure 
FUNCTION setDocLoadSeqList RETURNS LOGICAL
  ( INPUT icDocSeqList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cDocSeqList = icDocSeqList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setExtraFieldsAndValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setExtraFieldsAndValues Procedure 
FUNCTION setExtraFieldsAndValues RETURNS LOGICAL
  ( INPUT icExtraFieldList  AS CHAR,
    INPUT icExtraValuesList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN cExtraFieldList  = icExtraFieldList
       cExtraValuesList = icExtraValuesList
       .

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setInputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setInputParam Procedure 
FUNCTION setInputParam RETURNS LOGICAL
  ( INPUT icInputParam AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cInputParam = icInputParam.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOutputParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setOutputParam Procedure 
FUNCTION setOutputParam RETURNS LOGICAL
  ( INPUT ocReturnParam AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cReturnParam = ocReturnParam.

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

&IF DEFINED(EXCLUDE-setPreTransBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPreTransBuffer Procedure 
FUNCTION setPreTransBuffer RETURNS LOGICAL
  ( INPUT ihCurrBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hBuffPreTrans) THEN DO:
  DELETE OBJECT hBuffPreTrans NO-ERROR.
  DELETE OBJECT httPreTrans NO-ERROR.
END.

IF VALID-HANDLE(ihCurrBuffer) THEN DO:
  CREATE TEMP-TABLE httPreTrans.
  httPreTrans:CREATE-LIKE(ihCurrBuffer).
  httPreTrans:TEMP-TABLE-PREPARE(ihCurrBuffer:NAME).
  hBuffPreTrans = httPreTrans:DEFAULT-BUFFER-HANDLE.
  hBuffPreTrans:BUFFER-CREATE().
  hBuffPreTrans:BUFFER-COPY(ihCurrBuffer).
  
  RETURN YES.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReportFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setReportFileName Procedure 
FUNCTION setReportFileName RETURNS LOGICAL
  ( INPUT icReportFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cReportFileName = icReportFileName.

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
cInputParam = icServerTransInputParam.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSessionId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSessionId Procedure 
FUNCTION setSessionId RETURNS LOGICAL
  (INPUT icNewSessionId AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
IF icNewSessionId NE icSessionId THEN DO:
  DELETE PROCEDURE THIS-PROCEDURE NO-ERROR.
  RETURN NO.
END.
ELSE RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startASlib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startASlib Procedure 
FUNCTION startASlib RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(THIS-PROCEDURE).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewDocs Procedure 
FUNCTION ViewDocs RETURNS LOGICAL
  ( INPUT icContext  AS CHAR,
    INPUT icEntityId AS CHAR,
    INPUT ibOpen     AS LOG,   
    INPUT icSaveDir  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hTempTable  AS HANDLE NO-UNDO.
DEF VAR hTempBuffer AS HANDLE NO-UNDO.
DEF VAR hTempQuery  AS HANDLE NO-UNDO.
DEF VAR cOutFile    AS CHAR   NO-UNDO.
DEF VAR mpDocument  AS MEMPTR NO-UNDO.
DEF VAR cOpenDocMsg AS CHAR   NO-UNDO.
DEF VAR bUseOrgName AS LOG    NO-UNDO.
DEF VAR cCompFile   AS CHAR   NO-UNDO.
DEF VAR bChunk      AS LOG    NO-UNDO.
DEF VAR bChunkOk    AS LOG    NO-UNDO.

ASSIGN cTmpDocFileNames = ""
       cOrgDocFileNames = ""
       cDocFileDates    = ""
       cDocFileDescs    = ""
       cDocFileIds      = ""
       cDocFileTypes    = ""
       cReturnTrans     = ""
       .

RUN jbdoc_getdoc.p
   (icSessionId,
    icContext + "|" + icEntityId,
    OUTPUT TABLE-HANDLE hTempTable,
    OUTPUT cOutParam,
    OUTPUT bOk).

IF bOk THEN DO:    

  IF icSaveDir = "" THEN icSaveDir = SESSION:TEMP-DIR.
  ELSE 
    ASSIGN icSaveDir   = TRIM(icSaveDir,CHR(92)) + CHR(92)
           bUseOrgName = TRUE.

  IF bUseOrgFileName THEN bUseOrgName = YES.           

  hTempBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY hTempQuery.
  hTempQuery:SET-BUFFERS(hTempBuffer).
  hTempQuery:QUERY-PREPARE("FOR EACH " + hTempBuffer:NAME).
  hTempQuery:QUERY-OPEN().
  hTempQuery:GET-FIRST().

  REPEAT WHILE NOT hTempQuery:QUERY-OFF-END:
    ASSIGN cOutFile = icSaveDir +
                      (IF bUseOrgName THEN 
                         REPLACE(hTempBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE,"\","_")
                       ELSE IF cViewFileNamePrefix NE "" THEN
                         cViewFileNamePrefix
                                   + "-" + STRING(hTempBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) 
                                   + (IF icContext = "JBoxDocRev" THEN "-" + TRIM(ENTRY(2,icEntityId,"|")) ELSE "")
                                   + "-" + DYNAMIC-FUNCTION('getASuserId') + "." + hTempBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE
                                   + (IF hTempBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE NE "gz" AND hTempBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE MATCHES "*.gz" THEN ".gz" ELSE "")
                       ELSE
                         icContext + "-" + STRING(hTempBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) 
                                   + (IF icContext = "JBoxDocRev" THEN "-" + TRIM(ENTRY(2,icEntityId,"|")) ELSE "")
                                   + "-" + DYNAMIC-FUNCTION('getASuserId') + "." + hTempBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE
                                   + (IF hTempBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE NE "gz" AND hTempBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE MATCHES "*.gz" THEN ".gz" ELSE ""))
           cTmpDocFileNames = cTmpDocFileNames + cOutFile + "|"
           cOrgDocFileNames = cOrgDocFileNames + hTempBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE + "|"
           cDocFileDates    = cDocFileDates    + STRING(hTempBuffer:BUFFER-FIELD("dFileModDate"):BUFFER-VALUE) + ","
           cDocFileDescs    = cDocFileDescs    + STRING(hTempBuffer:BUFFER-FIELD("cDescription"):BUFFER-VALUE) + "|"
           cDocFileIds      = cDocFileIds      + STRING(hTempBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + ","
           cDocFileTypes    = cDocFileTypes    + hTempBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE + ","
           .
    SET-SIZE(mpDocument) = hTempBuffer:BUFFER-FIELD('iDocSize'):BUFFER-VALUE.
    COPY-LOB FROM OBJECT hTempBuffer:BUFFER-FIELD('blDocument'):BUFFER-VALUE TO OBJECT mpDocument.

    ExportMemptrDoc (mpDocument,
                     ibOpen,
                     cOutFile,
                     icSaveDir,
                     hTempBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE).

    hTempQuery:GET-NEXT().
  END.

  IF cGZIP NE ? THEN
    cTmpDocFileNames = REPLACE(cTmpDocFileNames,".gz|","|").

  ASSIGN cTmpDocFileNames    = TRIM(cTmpDocFileNames,"|")
         cOrgDocFileNames    = TRIM(cOrgDocFileNames,"|")
         cDocFileDates       = TRIM(cDocFileDates,",")
         cDocFileDescs       = SUBSTR(cDocFileDescs,1,LENGTH(cDocFileDescs) - 1)
         cDocFileIds         = TRIM(cDocFileIds,",")
         cDocFileTypes       = TRIM(cDocFileTypes,",")
         cViewFileNamePrefix = ""
         .
  IF cTmpDocFileNames = "" THEN 
    cReturnTrans = "No files found for entity reference".       

  DELETE OBJECT hTempQuery.
  DELETE OBJECT hTempTable.
  SET-SIZE(mpDocument) = 0.
END.

bUseOrgFileName = NO.           

RETURN cTmpDocFileNames NE "". 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

