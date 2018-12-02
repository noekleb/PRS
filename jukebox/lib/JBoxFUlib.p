&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ChemistryFU_lib.p
    Purpose     : Function library

    Syntax      :

    Description :

    Author(s)   : Not all functions are written by the Chemistry team. Whenever available, the
                  author is mentioned..
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{incl/Excel_1.3.i}

DEF VAR bOK                 AS LOG NO-UNDO.
DEF VAR ix                  AS INT NO-UNDO.

DEF VAR cOutParam           AS CHAR NO-UNDO.
DEF VAR cTmpDocFileNames    AS CHAR NO-UNDO.
DEF VAR cOrgDocFileNames    AS CHAR NO-UNDO.
DEF VAR cDocFileDates       AS CHAR NO-UNDO.
DEF VAR cDocFileDescs       AS CHAR NO-UNDO.
DEF VAR cDocFileIds         AS CHAR NO-UNDO.
DEF VAR cDocFileTypes       AS CHAR NO-UNDO.
DEF VAR cGZIP               AS CHAR NO-UNDO.
DEF VAR cNoCompressTypes    AS CHAR NO-UNDO INIT "bmp,gif,jpg,jpeg,mp3,tiff,tif,rar,gz,zip,wma,wmv".
DEF VAR bCompressFiles      AS LOG  NO-UNDO INIT TRUE.
DEF VAR cReturnTrans        AS CHAR NO-UNDO.
DEF VAR bLoadDocsAsync      AS LOG  NO-UNDO.
DEF VAR dDocLoadFileDate    AS DATE NO-UNDO.
DEF VAR iDocLoadFileTime    AS INT  NO-UNDO.
DEF VAR cViewFileNamePrefix AS CHAR NO-UNDO.
DEF VAR cDocSeqList         AS CHAR NO-UNDO.
DEF VAR bUseOrgFileName     AS LOG  NO-UNDO.
DEF VAR bSupprOpenDocMsg    AS LOG  NO-UNDO.
DEF VAR cSupprOpenDocMsg    AS CHAR NO-UNDO.
DEF VAR cOpenFileErrorList  AS CHAR NO-UNDO.
DEF VAR cSaveDir            AS CHAR NO-UNDO.
DEF VAR hClassSourceProc    AS HANDLE NO-UNDO.
DEF VAR cDocUploadProc      AS CHAR NO-UNDO.
DEF VAR iPid                AS INT  NO-UNDO.
DEF VAR cIniFile            AS CHAR NO-UNDO INIT "progress.ini".
DEF VAR cExeFile            AS CHAR NO-UNDO.
DEF VAR cSubProcessWorkDir  AS CHAR NO-UNDO.

&IF "{&OPSYS}" = "WIN32" &THEN
  DEF VAR oCmd AS JBoxCmd NO-UNDO.
&ENDIF

/* Open multiple files: */
&GLOBAL-DEFINE OFN_OVERWRITEPROMPT  2
&GLOBAL-DEFINE OFN_HIDEREADONLY     4
&GLOBAL-DEFINE OFN_NOCHANGEDIR      8
&GLOBAL-DEFINE OFN_ALLOWMULTISELECT 512
&GLOBAL-DEFINE OFN_PATHMUSTEXIST    2048
&GLOBAL-DEFINE OFN_FILEMUSTEXIST    4096
&GLOBAL-DEFINE OFN_NOREADONLYRETURN 32768
&GLOBAL-DEFINE OFN_EXPLORER         524288
 
PROCEDURE GetOpenFileNameA EXTERNAL "comdlg32.dll" :
  DEFINE INPUT  PARAMETER lpOfn   AS LONG.
  DEFINE RETURN PARAMETER pReturn AS LONG.
END PROCEDURE.

PROCEDURE GetCurrentProcessId EXTERNAL "KERNEL32.DLL":
  DEFINE RETURN PARAMETER intProcessHandle AS LONG.
END PROCEDURE.

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


DEF TEMP-TABLE ttFrame
    FIELD iRow        AS INT 
    FIELD iCol        AS INT
    FIELD hWidget     AS HANDLE
    FIELD iWidthChars AS INT
    FIELD iWidthPix   AS INT
    FIELD iMasterRow  AS INT
    FIELD cText       AS CHAR 
    INDEX idxRow AS PRIMARY iRow
    .
DEF BUFFER bttFrame FOR ttFrame.
DEF VAR hBuffFrame AS HANDLE NO-UNDO.
hBuffFrame = BUFFER ttFrame:HANDLE.

DEF TEMP-TABLE ttEditor
    FIELD iRow       AS INT
    FIELD iCol       AS INT
    FIELD iMasterRow AS INT
    FIELD iWidth     AS INT
    FIELD cText      AS CHAR
    INDEX idxRow AS PRIMARY iRow
    .
DEF BUFFER bttEditor FOR ttEditor.

DEF TEMP-TABLE ttStartup
    FIELD cParam  AS CHAR
    FIELD cValue  AS CHAR
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

&IF DEFINED(EXCLUDE-ConvHtmlToText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvHtmlToText Procedure 
FUNCTION ConvHtmlToText RETURNS CHARACTER
  ( INPUT icSourceText AS CHAR,
    INPUT icSourceType AS CHAR,    /* text/file */
    INPUT icReturnType AS CHAR,    /* text/file */
    INPUT iCharsPrLine AS INT,
    INPUT icConfigType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvXlsToCsv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvXlsToCsv Procedure 
FUNCTION ConvXlsToCsv RETURNS CHARACTER
  ( INPUT icXlsFile AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteListItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteListItem Procedure 
FUNCTION DeleteListItem RETURNS CHARACTER
  ( INPUT icList      AS CHAR,
    INPUT icEntry     AS CHAR,
    INPUT iiEntry     AS INT,
    INPUT icDelimiter AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-equalLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD equalLists Procedure 
FUNCTION equalLists RETURNS LOGICAL
  ( INPUT icList1     AS CHAR,
    INPUT icList2     AS CHAR,
    INPUT icDelimiter AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExploreDirectory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExploreDirectory Procedure 
FUNCTION ExploreDirectory RETURNS LOGICAL
  ( INPUT icDirectory AS CHAR )  FORWARD.

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

&IF DEFINED(EXCLUDE-extractEmailFromString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD extractEmailFromString Procedure 
FUNCTION extractEmailFromString RETURNS CHARACTER
  ( INPUT icString AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FrameFieldsToTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FrameFieldsToTT Procedure 
FUNCTION FrameFieldsToTT RETURNS HANDLE
  ( INPUT ihFrame      AS HANDLE,
    INPUT icNameList   AS CHAR,
    INPUT icExNameList AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAlphaSeqNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAlphaSeqNo Procedure 
FUNCTION getAlphaSeqNo RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCharFileSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCharFileSize Procedure 
FUNCTION getCharFileSize RETURNS CHARACTER
  ( INPUT dSize AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getColorNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getColorNum Procedure 
FUNCTION getColorNum RETURNS INTEGER
  ( INPUT iRGBcolor AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getConvToIntTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getConvToIntTime Procedure 
FUNCTION getConvToIntTime RETURNS CHARACTER
  ( INPUT icTime AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileDates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDocFileDates Procedure 
FUNCTION getDocFileDates RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileDescs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDocFileDescs Procedure 
FUNCTION getDocFileDescs RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDocFileIds Procedure 
FUNCTION getDocFileIds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDocFileTypes Procedure 
FUNCTION getDocFileTypes RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&IF DEFINED(EXCLUDE-getIsOfficeComHandleActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsOfficeComHandleActive Procedure 
FUNCTION getIsOfficeComHandleActive RETURNS LOGICAL
  ( INPUT ichOfficeComHandle AS COM-HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLocalQueryMaxValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLocalQueryMaxValue Procedure 
FUNCTION getLocalQueryMaxValue RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icField         AS CHAR,
    INPUT ifIncrement     AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLocalQueryStat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLocalQueryStat Procedure 
FUNCTION getLocalQueryStat RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMyCommandLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyCommandLine Procedure
FUNCTION getMyCommandLine RETURNS CHARACTER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getMyExeFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyExeFile Procedure
FUNCTION getMyExeFile RETURNS CHARACTER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getMyIniFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyIniFile Procedure
FUNCTION getMyIniFile RETURNS CHARACTER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getMyPid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyPid Procedure
FUNCTION getMyPid RETURNS INTEGER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getSubProcessWorkDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSubProcessWorkDir Procedure
FUNCTION getSubProcessWorkDir RETURNS CHARACTER 
  (  ) FORWARD.

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

&IF DEFINED(EXCLUDE-getProcedureHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProcedureHandle Procedure 
FUNCTION getProcedureHandle RETURNS HANDLE
  ( INPUT icProcName AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStartupParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStartupParam Procedure
FUNCTION getStartupParam RETURNS CHARACTER 
  (INPUT icParameter AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getTmpDocFileNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTmpDocFileNames Procedure 
FUNCTION getTmpDocFileNames RETURNS CHARACTER
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

&IF DEFINED(EXCLUDE-getUserConfirmation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUserConfirmation Procedure 
FUNCTION getUserConfirmation RETURNS LOGICAL
  ( INPUT icPwd            AS CHAR,
    INPUT icUserList       AS CHAR,
    INPUT iiResourceId     AS INT )  FORWARD.

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

&IF DEFINED(EXCLUDE-LoadUniqueDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadUniqueDocs Procedure 
FUNCTION LoadUniqueDocs RETURNS LOGICAL
  ( INPUT icFileNames   AS CHAR,
    INPUT icContext     AS CHAR,
    INPUT icEntityId    AS CHAR,
    INPUT icDescription AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectFileNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SelectFileNames Procedure 
FUNCTION SelectFileNames RETURNS CHARACTER (
  INPUT FilterList       AS CHARACTER,
  INPUT InitialDirectory AS CHARACTER,
  INPUT DialogTitle      AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendOutlookMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sendOutlookMail Procedure 
FUNCTION sendOutlookMail RETURNS CHARACTER
  ( INPUT icTO      AS CHAR,
    INPUT icCC      AS CHAR,
    INPUT icBCC     AS CHAR,
    INPUT icSubject AS CHAR,
    INPUT icBody    AS CHAR,
    INPUT icAttach  AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setClassSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setClassSourceProc Procedure 
FUNCTION setClassSourceProc RETURNS LOGICAL
  ( INPUT ihClassSourceProc AS HANDLE ) FORWARD.

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
  (INPUT icDocUploadProc    AS CHAR,
   INPUT ibSaveDocFileToDeb AS LOG,
   INPUT icDocLoadParam     AS CHAR) FORWARD.

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

&IF DEFINED(EXCLUDE-setFileCompression) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFileCompression Procedure 
FUNCTION setFileCompression RETURNS LOGICAL
  ( INPUT ibCompressFiles AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFrameFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFrameFields Procedure 
FUNCTION setFrameFields RETURNS LOGICAL
  ( INPUT ihFrame     AS HANDLE,
    INPUT icFieldList AS CHAR,
    INPUT icValueList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoadDocsAsync) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLoadDocsAsync Procedure 
FUNCTION setLoadDocsAsync RETURNS LOGICAL
  ( INPUT ibLoadDocsAsync AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMyIniFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMyIniFile Procedure
FUNCTION setMyIniFile RETURNS LOGICAL 
  (INPUT icIniFile AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setMyWorDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMyWorDir Procedure
FUNCTION setSubProcessWorkDir RETURNS LOGICAL 
  ( INPUT icSubProcessWorkDir AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setNoCompressTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoCompressTypes Procedure 
FUNCTION setNoCompressTypes RETURNS LOGICAL
  ( INPUT icNoCompressTypes AS CHAR,
    INPUT ibAppend          AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSuppressOpenDocError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSuppressOpenDocError Procedure 
FUNCTION setSuppressOpenDocError RETURNS LOGICAL
  ( INPUT ibSupprOpenDocMsg AS LOG ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseOrgFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUseOrgFileName Procedure 
FUNCTION setUseOrgFileName RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setViewFileNamePrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setViewFileNamePrefix Procedure 
FUNCTION setViewFileNamePrefix RETURNS LOGICAL
  ( INPUT icViewFileNamePrefix AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SplitText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SplitText Procedure 
FUNCTION SplitText RETURNS CHARACTER
  ( INPUT icText             AS CHAR,
    INPUT iiLength           AS INT,
    INPUT ibSplitOnLineshift AS LOG,
    INPUT icDelimiter        AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-syncLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD syncLists Procedure 
FUNCTION syncLists RETURNS CHARACTER
  ( INPUT icList1        AS CHAR,
    INPUT icList2        AS CHAR,
    INPUT icDelimiter    AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TextToFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TextToFileName Procedure 
FUNCTION TextToFileName RETURNS CHARACTER
  ( INPUT icText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ttToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ttToFile Procedure
FUNCTION ttToFile RETURNS LOGICAL 
  (INPUT ihBuffer     AS HANDLE,
   INPUT icFileName   AS CHAR,
   INPUT icFieldList  AS CHAR,
   INPUT icQueryWhere AS CHAR,
   INPUT icSortBy     AS CHAR,
   INPUT ibOpenFile   AS LOG) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-ValidCharTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidCharTime Procedure 
FUNCTION ValidCharTime RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE )  FORWARD.

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

&IF DEFINED(EXCLUDE-ViewOpenDocError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewOpenDocError Procedure 
FUNCTION ViewOpenDocError RETURNS CHARACTER
  ( INPUT ibView AS LOG ) FORWARD.

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
         HEIGHT             = 37.65
         WIDTH              = 61.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

cGZIP = SEARCH("gzip.exe").
RUN GetCurrentProcessId (OUTPUT iPid).

DEF VAR cWmicFile AS CHAR NO-UNDO.
DEF VAR cProcess  AS CHAR NO-UNDO.
cWmicFile = getUniqueFileName().

OS-COMMAND SILENT VALUE("WMIC /OUTPUT:" + cWmicFile + " path win32_process get Caption,Processid,Commandline").
INPUT FROM VALUE(cWmicFile).
REPEAT:
  IMPORT UNFORMATTED cProcess.
  cProcess = TRIM(cProcess).
  IF cProcess BEGINS "prow" AND cProcess MATCHES "*" + STRING(iPid) THEN DO:
    
    REPEAT WHILE cProcess MATCHES "*  *":
      cProcess = REPLACE(cProcess,"  "," ").
    END.    
    cExeFile = ENTRY(2,cProcess," ").
    DO ix = 3 TO NUM-ENTRIES(cProcess," ") - 1:
      IF ENTRY(ix,cProcess," ") BEGINS "-" THEN DO:
        CREATE ttStartup.
        ttStartup.cParam = ENTRY(ix,cProcess," ").
        IF NOT ENTRY(ix + 1,cProcess," ") BEGINS "-" THEN
          ttStartup.cValue = ENTRY(ix + 1,cProcess," ").
        IF ttStartup.cParam BEGINS "-ini" THEN
          cIniFile = ttStartup.cValue.  
      END.
    END.
  END.
END.
INPUT CLOSE.
OS-DELETE VALUE(cWmicFile).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ChunkDownload) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChunkDownload Procedure 
PROCEDURE ChunkDownload :
/*------------------------------------------------------------------------------
  Purpose:     Analyse a download of docs if files need to be chunked or batched in a number of files at a time  
  Parameters:  <none>
  Notes:       Done to get past timeout limits and bugs in the java classes used by Aia
------------------------------------------------------------------------------*/
DEF INPUT PARAM  icContext  AS CHAR NO-UNDO.
DEF INPUT PARAM  icEntityId AS CHAR NO-UNDO.
DEF INPUT PARAM  ibOpenDoc  AS LOG  NO-UNDO.
DEF INPUT PARAM  icSaveDir  AS CHAR NO-UNDO.
DEF OUTPUT PARAM obChunk    AS LOG  NO-UNDO.
DEF OUTPUT PARAM obChunkOk  AS LOG  NO-UNDO.

DEF VAR hTempTable     AS HANDLE NO-UNDO.
DEF VAR hTempBuffer    AS HANDLE NO-UNDO.
DEF VAR hTempQuery     AS HANDLE NO-UNDO.
DEF VAR iTotSmallSize  AS INT    NO-UNDO.
DEF VAR iTotSize       AS INT    NO-UNDO.
DEF VAR iSizeLimit     AS INT    NO-UNDO INIT 1000000.
DEF VAR iCount         AS INT    NO-UNDO.
DEF VAR iStartDocBatch AS INT    NO-UNDO.
DEF VAR cOutFile       AS CHAR   NO-UNDO.
DEF VAR bUseOrgName    AS LOG    NO-UNDO.
DEF VAR mpTarget       AS MEMPTR NO-UNDO.
DEF VAR iDocsize       AS INT    NO-UNDO.
DEF VAR iStartChunk    AS INT    NO-UNDO.
DEF VAR iChunkSize     AS INT    NO-UNDO.
DEF VAR cChunkFileName AS CHAR   NO-UNDO.

IF DYNAMIC-FUNCTION("getAttribute",SESSION,"doc_chunk_limit") NE "" THEN
  iSizeLimit = INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"doc_chunk_limit")) NO-ERROR.
IF iSizeLimit LT 100000 OR iSizeLimit = ? THEN RETURN.

SESSION:SET-WAIT-STATE("general").

IF DYNAMIC-FUNCTION("getAppServiceHandle") NE ? THEN DO:
  RUN jbdoc_getdocinfo.p
      ON DYNAMIC-FUNCTION("getAppServiceHandle")
     (DYNAMIC-FUNCTION("getSessionId"),
      icContext + "|" + icEntityId,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cOutParam,
      OUTPUT bOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF DYNAMIC-FUNCTION("ReconnectServer") THEN 
      RUN jbdoc_getdocinfo.p
          ON DYNAMIC-FUNCTION("getAppServiceHandle")
         (DYNAMIC-FUNCTION("getSessionId"),
          icContext + "|" + icEntityId,
          OUTPUT TABLE-HANDLE hTempTable,
          OUTPUT cOutParam,
          OUTPUT bOk).
  END.
END.
ELSE 
  RUN jbdoc_getdocinfo.p
     (DYNAMIC-FUNCTION("getSessionId"),
      icContext + "|" + icEntityId,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cOutParam,
      OUTPUT bOk).

EMPTY TEMP-TABLE ttDocInfo.

hTempBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
CREATE QUERY hTempQuery.
hTempQuery:SET-BUFFERS(hTempBuffer).
hTempQuery:QUERY-PREPARE("FOR EACH " + hTempBuffer:NAME).
hTempQuery:QUERY-OPEN().
hTempQuery:GET-FIRST().

REPEAT WHILE NOT hTempQuery:QUERY-OFF-END:
  CREATE ttDocInfo.
  httDocInfoBuf:BUFFER-COPY(hTempBuffer).

  IF hTempBuffer:BUFFER-FIELD("iDocSize"):BUFFER-VALUE > iSizeLimit THEN
    ttDocInfo.cCat = "big".

  ASSIGN iTotSize = iTotSize + hTempBuffer:BUFFER-FIELD("iDocSize"):BUFFER-VALUE
         iCount   = iCount + 1.

  hTempQuery:GET-NEXT().
END.

/* Now we know if the total download exceeds max in one call
   and the single files that exceeds max. */

IF iTotSize > iSizeLimit THEN DO:
  ASSIGN bChunkInProgress = YES
         obChunk          = YES
         .
  IF icSaveDir = "" THEN icSaveDir = SESSION:TEMP-DIR.
  ELSE 
    ASSIGN icSaveDir   = TRIM(icSaveDir,"\") + "\"
           bUseOrgName = TRUE.
           
  IF bUseOrgFileName THEN bUseOrgName = YES.           

  /* First process the files under size limit in batches that are less than limit - 
     Approximately: Includes the current doc that is the one that pushes over the limit: */  

  FOR EACH ttDocInfo
      WHERE ttDocInfo.cCat NE "big"
      BY ttDocInfo.iJBoxDocumentId:
    iTotSmallSize = iTotSmallSize + ttDocInfo.iDocSize.

    IF iTotSmallSize > iSizeLimit THEN DO:
      bChunkInProgress = YES.
      obChunkOk = ViewDocs(icContext,
                  icEntityId
                + (IF NUM-ENTRIES(icEntityId,"|") = 1 THEN "||" ELSE "|")
                + STRING(iStartDocBatch) + "¤" + STRING(ttDocInfo.iJBoxDocumentId) + "¤" + STRING(iSizeLimit),
                  ibOpenDoc,
                  icSaveDir).      
      ASSIGN iStartDocBatch = ttDocInfo.iJBoxDocumentId
             iTotSmallSize  = 0.
    END.
  END.
  IF iTotSmallSize > 0 THEN DO:      
    bChunkInProgress = YES.
    obChunkOk = ViewDocs(icContext,
                icEntityId
              + (IF NUM-ENTRIES(icEntityId,"|") = 1 THEN "||" ELSE "|")
              + STRING(iStartDocBatch) + "¤999999999¤" + STRING(iSizeLimit),
                ibOpenDoc,
                icSaveDir).      
  END.

  /* Now process the documents exceeding max size indivdually by chunks: */

  DELETE OBJECT hTempTable NO-ERROR.  
  FOR EACH ttDocInfo
      WHERE ttDocInfo.cCat = "big"
      BY ttDocInfo.iJBoxDocumentId:

    ASSIGN cOutFile = icSaveDir +
                      (IF bUseOrgName THEN 
                         REPLACE(ttDocInfo.cFileName,"\","_")
                       ELSE IF cViewFileNamePrefix NE "" THEN
                         cViewFileNamePrefix
                                   + "-" + STRING(ttDocInfo.iJBoxDocumentId) 
                                   + (IF icContext = "JBoxDocRev" THEN "-" + TRIM(ENTRY(2,icEntityId,"|")) ELSE "")
                                   + "-" + DYNAMIC-FUNCTION('getASuserId')
                                   + "." + ttDocInfo.cFileType
                                   + (IF ttDocInfo.cFileType NE "gz" AND ttDocInfo.cFileName MATCHES "*.gz" THEN ".gz" ELSE "")
                       ELSE
                         icContext + "-" + STRING(ttDocInfo.iJBoxDocumentId) 
                                   + (IF icContext = "JBoxDocRev" THEN "-" + TRIM(ENTRY(2,icEntityId,"|")) ELSE "")
                                   + "-" + DYNAMIC-FUNCTION('getASuserId')
                                   + "." + ttDocInfo.cFileType
                                   + (IF ttDocInfo.cFileType NE "gz" AND ttDocInfo.cFileName MATCHES "*.gz" THEN ".gz" ELSE ""))

           cTmpDocFileNames = cTmpDocFileNames + cOutFile + "|"
           cOrgDocFileNames = cOrgDocFileNames + ttDocInfo.cFileName + "|"
           cDocFileDates    = cDocFileDates    + STRING(ttDocInfo.dFileModDate) + ","
           cDocFileDescs    = cDocFileDescs    + STRING(ttDocInfo.cDescription) + "|"
           cDocFileIds      = cDocFileIds      + STRING(ttDocInfo.iJBoxDocumentId) + ","
           cDocFileTypes    = cDocFileTypes    + ttDocInfo.cFileType + ","
           iChunkSize       = iSizeLimit
           iStartChunk      = 1
           ix               = 0
           .

    SET-SIZE(mpTarget) = 0.
    SET-SIZE(mpTarget) = ttDocInfo.iDocSize.
    
    GetChunks:
    REPEAT:
      ASSIGN iChunkSize = MIN(ttDocInfo.iDocSize - iStartChunk + 1,iChunkSize)
             ix = ix + 1.
      IF DYNAMIC-FUNCTION("getAppServiceHandle") NE ? THEN DO:
        IF icContext NE "JBoxDocRev" THEN
          RUN jbdoc_getdocchunk.p
              ON DYNAMIC-FUNCTION("getAppServiceHandle")
             (DYNAMIC-FUNCTION("getSessionId"),
              ttDocInfo.iJBoxDocumentId,
              iChunkSize,
              iStartChunk,
              OUTPUT TABLE-HANDLE hTempTable,
              OUTPUT iDocsize,
              OUTPUT cChunkFileName,  /* Same as ttDocInfo.cFileName */
              OUTPUT cOutParam,
              OUTPUT bOk)
              NO-ERROR.
        ELSE
          RUN jbdoc_getdocchunk_rev.p
              ON DYNAMIC-FUNCTION("getAppServiceHandle")
             (DYNAMIC-FUNCTION("getSessionId"),
              ttDocInfo.iJBoxDocumentId,
              INTEGER(ENTRY(2,icEntityId,"|")),
              iChunkSize,
              iStartChunk,
              OUTPUT TABLE-HANDLE hTempTable,
              OUTPUT iDocsize,
              OUTPUT cChunkFileName,  /* Same as ttDocInfo.cFileName */
              OUTPUT cOutParam,
              OUTPUT bOk)
              NO-ERROR.
      END.
      ELSE DO:
        IF icContext NE "JBoxDocRev" THEN
          RUN jbdoc_getdocchunk.p
             (DYNAMIC-FUNCTION("getSessionId"),
              ttDocInfo.iJBoxDocumentId,
              iChunkSize,
              iStartChunk,
              OUTPUT TABLE-HANDLE hTempTable,
              OUTPUT iDocsize,
              OUTPUT cChunkFileName,
              OUTPUT cOutParam,
              OUTPUT bOk).
        ELSE
          RUN jbdoc_getdocchunk_rev.p
             (DYNAMIC-FUNCTION("getSessionId"),
              ttDocInfo.iJBoxDocumentId,
              INTEGER(ENTRY(2,icEntityId,"|")),
              iChunkSize,
              iStartChunk,
              OUTPUT TABLE-HANDLE hTempTable,
              OUTPUT iDocsize,
              OUTPUT cChunkFileName,
              OUTPUT cOutParam,
              OUTPUT bOk).
      END.

      IF ix > 100 OR NOT bOk THEN DO:
        IF NOT bOk THEN
          MESSAGE PROGRAM-NAME(1) SKIP
                  cOutParam VIEW-AS ALERT-BOX ERROR.
        DELETE OBJECT hTempTable NO-ERROR.  
        SET-SIZE(mpTarget) = 0.
        RETURN.
      END.

      hTempBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
      bOk = hTempBuffer:FIND-FIRST() NO-ERROR.
      IF bOk THEN DO:
        COPY-LOB FROM OBJECT hTempBuffer:BUFFER-FIELD("blChunk"):BUFFER-VALUE                 
                 TO OBJECT mpTarget
                 OVERLAY AT iStartChunk.

        iStartChunk = iStartChunk + iChunkSize.

        IF iStartChunk > iDocSize THEN DO:
          ExportMemptrDoc (mpTarget,
                           ibOpenDoc,
                           cOutFile,
                           icSaveDir,
                           ttDocInfo.cFileName).
          DELETE OBJECT hTempTable NO-ERROR.  
          LEAVE GetChunks.
        END.
      END.
    END.
    DELETE OBJECT hTempTable NO-ERROR.  
  END.
  SET-SIZE(mpTarget) = 0.

  ASSIGN obChunkOk = bOk
         bChunkInProgress = NO.
  IF NOT obChunkOk THEN
    DYNAMIC-FUNCTION("setTransactionMessage","Error when retrieving file (in chunks)").
         
END.

ELSE obChunk = NO.

cViewFileNamePrefix = "".

SESSION:SET-WAIT-STATE("").

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

&IF DEFINED(EXCLUDE-ConvHtmlToText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvHtmlToText Procedure 
FUNCTION ConvHtmlToText RETURNS CHARACTER
  ( INPUT icSourceText AS CHAR,
    INPUT icSourceType AS CHAR,    /* text/file */
    INPUT icReturnType AS CHAR,    /* text/file */
    INPUT iCharsPrLine AS INT,
    INPUT icConfigType AS CHAR ) : /* Configuration types: Standard/.. */
/*------------------------------------------------------------------------------
  Purpose: Use the HtmlAsText utility from www.nirsoft.net to convert
           either a html file or text to plain text and return the result 
           either as text or in a file 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTmpFile    AS CHAR NO-UNDO.
DEF VAR cConfigFile AS CHAR NO-UNDO.
DEF VAR cConvFile   AS CHAR NO-UNDO.
DEF VAR cConvResult AS CHAR NO-UNDO.
DEF VAR cConvExe    AS CHAR NO-UNDO.

IF SEARCH("htmlastext.exe") = ? THEN RETURN icSourceText.

FILE-INFO:FILE-NAME = SEARCH("htmlastext.exe").
cConvExe = FILE-INFO:FULL-PATHNAME.

ASSIGN cConfigFile = SESSION:TEMP-DIRECTORY + "ConvHtmlToTxt_" + DYNAMIC-FUNCTION("getASuserId") + ".cfg"
       cConvFile   = SESSION:TEMP-DIRECTORY + "ConvHtmlToTxt_" + DYNAMIC-FUNCTION("getASuserId") + ".tmp"
       .

IF iCharsPrLine = 0 THEN iCharsPrLine = 75.

IF icSourceType = "text" THEN DO:
  cTmpFile = getUniqueFileName().
  OUTPUT STREAM strmFile TO VALUE(cTmpFile).    
  PUT STREAM strmFile UNFORMATTED icSourceText.
  OUTPUT STREAM strmFile CLOSE.
END.
ELSE DO:
  IF SEARCH(icSourceText) = ? THEN
    RETURN "".
  FILE-INFO:FILE-NAME = icSourceText.
  cTmpFile = FILE-INFO:FULL-PATHNAME.
END. 


OUTPUT STREAM strmFile TO VALUE(cConfigFile).
PUT STREAM strmFile UNFORMATTED 
                "[Config]" 
    + CHR(10) + "OpenInNotepad=0"
    + CHR(10) + "CharsPerLine=" + STRING(iCharsPrLine) 
    + CHR(10) + "Source=" + cTmpFile
    + CHR(10) + "Dest=" + cConvFile
    + CHR(10) + "SkipTitleText=0"
    + CHR(10) + "AddLineUnderHeader=0"
    + CHR(10) + "SkipTableHeaderText=0"
    + CHR(10) + "TableCellDelimit=1"
    + CHR(10) + "HeadingLineChars======="
    + CHR(10) + "HorRuleChar=="
    + CHR(10) + "ListChars=*o-@#"
    + CHR(10) + "ConvertMode=1"
    + CHR(10) + "AllowCenterText=0"
    + CHR(10) + "AllowRightText=0"
    + CHR(10) + "DLSpc=8"
    + CHR(10) + "LinksDisplayFormat=%T"
    + CHR(10) + "EncloseBoldCharsStart=<<"
    + CHR(10) + "EncloseBoldCharsEnd=>>"
    + CHR(10) + "EncloseBold=0"
    + CHR(10) + "SubFolders=0"
    + CHR(10)
    .
OUTPUT STREAM strmFile CLOSE.

&IF "{&OPSYS}" = "WIN32" &THEN
  oCMD = new jboxcmd(cConvExe,' /run' + cConfigFile).
&ELSE
  OS-COMMAND SILENT VALUE(cConvExe + " /run " + cConfigFile).
&ENDIF

/*OS-COMMAND SILENT VALUE(cConvExe + " /run " + cConfigFile).*/
/*oCMD = new jboxcmd(cConvExe,' /run' + cConfigFile).*/

OS-DELETE VALUE(cConfigFile).

IF icReturnType = "text" THEN DO:
  INPUT STREAM strmFile FROM VALUE(cConvFile).
  REPEAT:
    IMPORT STREAM strmFile UNFORMATTED cTmpFile.
    cConvResult = cConvResult + cTmpFile.
  END.
  INPUT STREAM strmFile CLOSE.
  OS-DELETE VALUE(cTmpFile).
  OS-DELETE VALUE(cConvFile).
  RETURN cConvResult.
END.
ELSE RETURN cConvFile.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvXlsToCsv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvXlsToCsv Procedure 
FUNCTION ConvXlsToCsv RETURNS CHARACTER
  ( INPUT icXlsFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chExcelInstance AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook      AS COM-HANDLE NO-UNDO.
DEF VAR cModName        AS CHAR       NO-UNDO.


/* chExcelInstance = DYNAMIC-FUNCTION("getExcelHandle") NO-ERROR. */

CREATE "Excel.Application" chExcelInstance NO-ERROR.

IF NOT VALID-HANDLE(chExcelInstance) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   IF DYNAMIC-FUNCTION("Scandinavian") THEN
                     "Kunne ikke konvertere filen fra Excel format til csv. (Er Excel installert på maskinen?)"
                   ELSE
                     "Could not convert from Excel format to csv. (Is Excel installed on the machine?)"
                   ,"","").
  RETURN "".
END.
    
chExcelInstance:VISIBLE = FALSE.

chExcelInstance:DisplayAlerts = NO.

chExcelInstance:Workbooks:OpenText(icXlsFile,2,,,,,TRUE).

chWorkbook = chExcelInstance:WorkBooks:ITEM(1).
       
cModName = RIGHT-TRIM(icXlsFile,".xls") + "_" + STRING(TIME) + ".csv".

cModName = SESSION:TEMP-DIR + ENTRY(NUM-ENTRIES(cModName,"\"),cModName,"\").

NO-RETURN-VALUE chWorkbook:SaveAs(cModName,{&xlCSVMSDOS},,,,,,) NO-ERROR.

chExcelInstance:ActiveWindow:Close().

RELEASE OBJECT chWorkbook NO-ERROR.
RELEASE OBJECT chExcelInstance NO-ERROR.
/* chExcelInstance:DisplayAlerts = YES. */

RETURN cModName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteListItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteListItem Procedure 
FUNCTION DeleteListItem RETURNS CHARACTER
  ( INPUT icList      AS CHAR,
    INPUT icEntry     AS CHAR,
    INPUT iiEntry     AS INT,
    INPUT icDelimiter AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Remove an entry from a list 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewList AS CHAR NO-UNDO.

IF icDelimiter = "" THEN icDelimiter = ",".

DO ix = 1 TO NUM-ENTRIES(icList,icDelimiter):
  IF ix = iiEntry THEN NEXT.
  ELSE IF iiEntry = 0 AND ENTRY(ix,icList,icDelimiter) = icEntry THEN NEXT.
  ELSE cNewList = cNewList + ENTRY(ix,icList,icDelimiter) + icDelimiter.
END.

RETURN SUBSTR(cNewList,1,LENGTH(cNewList) - 1).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-equalLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION equalLists Procedure 
FUNCTION equalLists RETURNS LOGICAL
  ( INPUT icList1     AS CHAR,
    INPUT icList2     AS CHAR,
    INPUT icDelimiter AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

IF icDelimiter = "" THEN icDelimiter = ",".

IF NUM-ENTRIES(icList1,icDelimiter) = NUM-ENTRIES(icList2,icDelimiter) THEN
  DO ix = 1 TO NUM-ENTRIES(icList1,icDelimiter):
    IF ENTRY(ix,icList1,icDelimiter) NE ENTRY(ix,icList2,icDelimiter) THEN 
      RETURN NO.
  END.
ELSE RETURN NO.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExploreDirectory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExploreDirectory Procedure 
FUNCTION ExploreDirectory RETURNS LOGICAL
  ( INPUT icDirectory AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE oServer AS COM-HANDLE NO-UNDO.

IF icDirectory = "" THEN icDirectory = SESSION:TEMP-DIRECTORY.

CREATE 'Shell.Application' oServer.

/* Invoke the Windows Explorer on the C:\WINNT folder               */

NO-RETURN-VALUE oServer:Explore(icDirectory).

/* Release the object references                                    */

RELEASE OBJECT oServer.


RETURN YES.

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

&IF DEFINED(EXCLUDE-extractEmailFromString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION extractEmailFromString Procedure 
FUNCTION extractEmailFromString RETURNS CHARACTER
  ( INPUT icString AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR ix      AS INT  NO-UNDO.
DEF VAR iAlfaK  AS INT  NO-UNDO.
DEF VAR iStart  AS INT  NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.

iAlfaK = INDEX(icString,"@").
IF iAlfaK > 0 THEN DO:
  DO ix = iAlfaK TO 1 BY -1:
    IF SUBSTR(icString,ix,1) = " " THEN LEAVE.
  END.
  iStart = ix.
  DO ix = iAlfaK TO LENGTH(icString):
    IF SUBSTR(icString,ix,1) = " " THEN LEAVE.
  END.


  RETURN SUBSTR(icString,iStart,ix - iStart).
END.
RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FrameFieldsToTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FrameFieldsToTT Procedure 
FUNCTION FrameFieldsToTT RETURNS HANDLE
  ( INPUT ihFrame      AS HANDLE,
    INPUT icNameList   AS CHAR,
    INPUT icExNameList AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTxt         AS CHAR   NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iz           AS INT    NO-UNDO.
DEF VAR cSplitTxt    AS CHAR   NO-UNDO.
DEF VAR bMoveDown    AS LOG    NO-UNDO.
DEF VAR iMaxRow      AS INT    NO-UNDO.
DEF VAR iMasterCnt   AS INT    NO-UNDO.
DEF VAR bAccMastCnt  AS LOG    NO-UNDO INIT YES.
DEF VAR iOverFlowCnt AS INT    NO-UNDO.
DEF VAR cLabel       AS CHAR   NO-UNDO.

EMPTY TEMP-TABLE ttFrame.
EMPTY TEMP-TABLE ttEditor.

DYNAMIC-FUNCTION("getOrgWigetPos",ihFrame,
                 "fill-in,combo-box,editor,toggle-box,text,radio-set",
                 icNameList,icExNameList,?,hBuffFrame).


FOR EACH ttFrame
    BREAK BY ttFrame.iRow BY ttFrame.iCol
    :

  IF ttFrame.hWidget:HIDDEN THEN NEXT.

  IF iMaxRow = 0 THEN iMaxRow = ttFrame.iRow.

  IF ttFrame.hWidget:LABEL NE ? THEN
    cLabel = ttFrame.hWidget:LABEL + ": ".
  ELSE cLabel = "".

  CASE ttFrame.hWidget:TYPE:
    WHEN "text" THEN 
      cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cTxt)) + ttFrame.hWidget:SCREEN-VALUE.
    WHEN "fill-in" THEN DO:
      cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cLabel) - LENGTH(cTxt)) + cLabel.

      IF ttFrame.hWidget:DATA-TYPE = "CHARACTER" AND FONT-TABLE:GET-TEXT-WIDTH-PIXELS(ttFrame.hWidget:SCREEN-VALUE,3) > ttFrame.iWidthPix THEN DO:
        cSplitTxt = DYNAMIC-FUNCTION("SplitText",ttFrame.hWidget:SCREEN-VALUE,ttFrame.iWidthChars,YES,"|").
        IF bAccMastCnt THEN 
          ASSIGN iMasterCnt  = iMasterCnt + 1
                 bAccMastCnt = NO.
        DO ix = 1 TO NUM-ENTRIES(cSplitTxt,"|"):
          IF ix = 1 THEN
            cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cTxt)) + ENTRY(ix,cSplitTxt,"|").
          ELSE DO:
            CREATE ttEditor.  
            ASSIGN ttEditor.iRow       = ttFrame.iRow + ix - 1
                   ttEditor.iCol       = ttFrame.iCol
                   ttEditor.iMasterRow = iMasterCnt
                   ttEditor.iWidth     = ttFrame.iWidthChars
                   ttEditor.cText      = FILL(" ",ttFrame.iCol) + ENTRY(ix,cSplitTxt,"|")
                   iMaxRow             = iMaxRow + 1
                   .
          END.
        END.
      END.
      ELSE IF ttFrame.hWidget:SCREEN-VALUE NE ? THEN
        cTxt = cTxt + ttFrame.hWidget:SCREEN-VALUE.
    END.
    WHEN "toggle-box" THEN DO:
      IF ttFrame.iCol - LENGTH(cTxt) > LENGTH(cLabel) THEN
        cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cLabel) - LENGTH(cTxt)) + cLabel + (IF LOGICAL(ttFrame.hWidget:INPUT-VALUE) THEN "X" ELSE "-").
      ELSE
        cTxt = cTxt + FILL(" ",LENGTH(cTxt) - ttFrame.iCol) + cLabel + ttFrame.hWidget:SCREEN-VALUE.
    END.
    WHEN "combo-box" THEN DO:
      cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cLabel) - LENGTH(cTxt)) + cLabel 
           + (IF ttFrame.hWidget:SUBTYPE = "drop-down-list" THEN DYNAMIC-FUNCTION("getDropDownLabel",ttFrame.hWidget,"") ELSE ttFrame.hWidget:SCREEN-VALUE).
    END.
    WHEN "radio-set" THEN DO:
      cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cLabel) - LENGTH(cTxt)).
      DO ix = 1 TO NUM-ENTRIES(ttFrame.hWidget:RADIO-BUTTONS,ttFrame.hWidget:DELIMITER) BY 2:
        IF ttFrame.hWidget:SCREEN-VALUE = ENTRY(ix + 1,ttFrame.hWidget:RADIO-BUTTONS,ttFrame.hWidget:DELIMITER) THEN
          cTxt = cTxt + (IF ix > 1 THEN "-" ELSE "") + "[" + ENTRY(ix,ttFrame.hWidget:RADIO-BUTTONS,ttFrame.hWidget:DELIMITER) + "]".
        ELSE
          cTxt = cTxt + (IF ix > 1 THEN "-" ELSE "") + ENTRY(ix,ttFrame.hWidget:RADIO-BUTTONS,ttFrame.hWidget:DELIMITER).
      END. 
    END.
    WHEN "editor" THEN DO:
      IF FONT-TABLE:GET-TEXT-WIDTH-PIXELS(ttFrame.hWidget:SCREEN-VALUE,3) > ttFrame.iWidthPix THEN DO:
        cSplitTxt = DYNAMIC-FUNCTION("SplitText",ttFrame.hWidget:SCREEN-VALUE,ttFrame.iWidthChars,YES,"|").
        IF bAccMastCnt THEN 
          ASSIGN iMasterCnt  = iMasterCnt + 1
                 bAccMastCnt = NO.
        DO ix = 1 TO NUM-ENTRIES(cSplitTxt,"|"):
          IF ix = 1 THEN
            cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cTxt)) + ENTRY(ix,cSplitTxt,"|").
          ELSE DO:
            CREATE ttEditor.  
            ASSIGN ttEditor.iRow       = ttFrame.iRow + ix - 1
                   ttEditor.iCol       = ttFrame.iCol
                   ttEditor.iMasterRow = iMasterCnt
                   ttEditor.iWidth     = ttFrame.iWidthChars
                   ttEditor.cText      = FILL(" ",ttFrame.iCol) + ENTRY(ix,cSplitTxt,"|")
                   iMaxRow             = iMaxRow + 1
                   .
          END.
        END.
      END.
      ELSE
        cTxt = cTxt + FILL(" ",ttFrame.iCol - LENGTH(cTxt) + 2) + ttFrame.hWidget:SCREEN-VALUE.
    END.
  END CASE.
    
  IF LAST-OF(ttFrame.iRow) THEN 
    ASSIGN ttFrame.cText = cTxt
           cTxt          = ""
           bAccMastCnt   = YES
           ttFrame.iMasterRow = iMasterCnt
           cSplitTxt     = ""
           iMaxRow       = iMaxRow + 1
           .
END.
FOR EACH ttFrame 
    WHERE ttFrame.cText = "":
  DELETE ttFrame.
END.
FOR EACH ttEditor
    WHERE ttEditor.cText = "":
  DELETE ttEditor.
END.

/* OUTPUT TO c:\temp\printsan.txt.       */
/*                                       */
/* PUT "iMaxRow: " iMaxRow SKIP(1).      */
/*                                       */
/* FOR EACH ttFrame:                     */
/*   DISP ttFrame.iRow                   */
/*        ttFrame.iCol                   */
/*        ttFrame.iMasterRow             */
/*        ttFrame.cText FORMAT "x(80)"   */
/*        WITH WIDTH 120 STREAM-IO.      */
/* END.                                  */
/* FOR EACH ttEditor:                    */
/*   DISP ttEditor.iRow                  */
/*        ttEditor.iCol                  */
/*        ttEditor.iMasterRow            */
/*        ttEditor.cText FORMAT "x(80)"  */
/*        WITH WIDTH 120 STREAM-IO.      */
/* END.                                  */
/* OUTPUT CLOSE.                         */
/*                                       */


DO ix = 1 TO iMaxRow:
  FIND FIRST ttFrame
       WHERE ttFrame.iRow = ix
       NO-ERROR.
  ASSIGN bMoveDown    = NO
         cTxt         = ""
         . 

  EditField:
  FOR EACH ttEditor
      WHERE ttEditor.iRow = ix:

    IF AVAIL ttFrame THEN DO:
/*       IF ttEditor.iMasterRow = ttFrame.iMasterRow AND TRIM(SUBSTR(ttFrame.cText,ttEditor.iCol,ttEditor.iWidth)) = "" THEN DO: */
/*         SUBSTR(ttFrame.cText,ttEditor.iCol,ttEditor.iWidth) = SUBSTR(ttEditor.cText,ttEditor.iCol,ttEditor.iWidth).           */
/*         NEXT EditField.                                                                                                       */
/*       END.                                                                                                                    */
/*       ELSE                                                                                                                    */
      bMoveDown = YES.
    END.
    cTxt = cTxt + FILL(" ",ttEditor.iCol - LENGTH(cTxt)) + TRIM(ttEditor.cText).
  
    IF bMoveDown THEN DO:
      FOR EACH bttFrame
         WHERE bttFrame.iRow GE ttEditor.iRow
         BREAK BY bttFrame.iMasterRow DESC
               BY bttFrame.iRow DESC:
        bttFrame.iRow = bttFrame.iRow + 1.
        IF LAST-OF(bttFrame.iMasterRow) AND bttFrame.iMasterRow NE ttEditor.iMasterRow THEN DO:
          ASSIGN iOverFlowCnt = 0
                 iz           = 0.
          FOR EACH bttEditor
             WHERE bttEditor.iMasterRow = bttFrame.iMasterRow:
            iOverFlowCnt = iOverFlowCnt + 1.
          END.
          FOR EACH bttEditor
             WHERE bttEditor.iMasterRow = bttFrame.iMasterRow
                BY bttEditor.iRow DESC:
            ASSIGN bttEditor.iRow = bttFrame.iRow + iOverFlowCnt - iz
                   iz = iz + 1.
          END.
        END.
      END.
    END.
    CREATE ttFrame.
    BUFFER-COPY ttEditor TO ttFrame.
    ttFrame.cText = cTxt.
  END.
END.
  
RETURN hBuffFrame.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAlphaSeqNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAlphaSeqNo Procedure 
FUNCTION getAlphaSeqNo RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: Return corresponding letter for a sequence number 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.

IF iiSeqNo < 0 OR iiSeqNo > 80 THEN RETURN "".
ELSE IF iiSeqNo < 27 THEN
  ocValue = CHR(64 + iiSeqNo).
ELSE IF iiSeqNo < 54 THEN
  ocValue = "A" + CHR(64 - 26 + iiSeqNo).
ELSE IF iiSeqNo < 81 THEN
  ocValue = "B" + CHR(64 - 26 + iiSeqNo).

RETURN ocValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCharFileSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCharFileSize Procedure 
FUNCTION getCharFileSize RETURNS CHARACTER
  ( INPUT dSize AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  kudos: Jon Brock jrbrock@gmail.com
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFactor AS CHARACTER NO-UNDO INITIAL "B,KB,MB,GB,TB,PB".
  DEFINE VARIABLE iFactor AS INTEGER   NO-UNDO INITIAL 1.

  /* Converts a size in bytes to a human form, 9737728 => 9.287 MB
   * See http://en.wikipedia.org/wiki/Byte for correct units
   */

  IF dSize = 0 THEN RETURN "0 B".
  iFactor = TRUNCATE(LOG(ABSOLUTE(dSize)) / LOG(1024), 0) + 1.
  IF iFactor > NUM-ENTRIES(cFactor) THEN iFactor = NUM-ENTRIES(cFactor).
  dSize = dSize / EXP(1024, iFactor - 1).

  RETURN SUBSTITUTE("&1 &2", INTEGER(dSize * 1000) / 1000,ENTRY(iFactor,cFactor)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getColorNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getColorNum Procedure 
FUNCTION getColorNum RETURNS INTEGER
  ( INPUT iRGBcolor AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR iy AS INT NO-UNDO.

IF iRGBcolor = 0 THEN RETURN 15.

IF iRGBcolor NE 0 THEN DO iy = 0 TO COLOR-TABLE:NUM-ENTRIES:
 IF COLOR-TABLE:GET-RGB-VALUE(iy) = iRGBcolor THEN RETURN iy.
END.


ASSIGN iy = COLOR-TABLE:NUM-ENTRIES
      COLOR-TABLE:NUM-ENTRIES = iy + 1.


IF iy = 256 THEN
 MESSAGE PROGRAM-NAME(1) SKIP
         256 SKIP
         VIEW-AS ALERT-BOX.


COLOR-TABLE:SET-DYNAMIC(iy, yes).
COLOR-TABLE:SET-RGB-VALUE(iy,iRGBcolor).


RETURN iy.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getConvToIntTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getConvToIntTime Procedure 
FUNCTION getConvToIntTime RETURNS CHARACTER
  ( INPUT icTime AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iTime AS INT NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icTime,":"):
  IF ix = 1 THEN iTime = INT(ENTRY(ix,icTime,":")) * 3600.
  ELSE IF ix = 2 THEN iTime = iTime + INT(ENTRY(ix,icTime,":")) * 60.
  ELSE iTime = iTime + INT(ENTRY(ix,icTime,":")).
END.

RETURN STRING(iTime).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileDates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDocFileDates Procedure 
FUNCTION getDocFileDates RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cDocFileDates. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileDescs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDocFileDescs Procedure 
FUNCTION getDocFileDescs RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cDocFileDescs. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileIds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDocFileIds Procedure 
FUNCTION getDocFileIds RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cDocFileIds. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDocFileTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDocFileTypes Procedure 
FUNCTION getDocFileTypes RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cDocFileTypes.

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

&IF DEFINED(EXCLUDE-getIsOfficeComHandleActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsOfficeComHandleActive Procedure 
FUNCTION getIsOfficeComHandleActive RETURNS LOGICAL
  ( INPUT ichOfficeComHandle AS COM-HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chTestActiveWin AS COM-HANDLE NO-UNDO.


chTestActiveWin = ichOfficeComHandle:ActiveWindow NO-ERROR.
  
RETURN VALID-HANDLE(chTestActiveWin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLocalQueryMaxValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLocalQueryMaxValue Procedure 
FUNCTION getLocalQueryMaxValue RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icField         AS CHAR,
    INPUT ifIncrement     AS DEC ) : 
/*------------------------------------------------------------------------------
  Purpose:  Get max value of a field in a local query
    Notes:  Field must be type INT or DEC
------------------------------------------------------------------------------*/
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hQueryBuffer  AS HANDLE NO-UNDO.
DEF VAR fMax          AS DEC    NO-UNDO.

IF ihBrowseOrQuery:TYPE = "browse" THEN
  hBuffer = ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hBuffer = ihBrowseOrQuery:GET-BUFFER-HANDLE(1).

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME + " BY " + icField).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE hQueryBuffer:AVAIL:
  fMax = hQueryBuffer:BUFFER-FIELD(icField):BUFFER-VALUE.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

RETURN STRING(fMax + ifIncrement).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLocalQueryStat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLocalQueryStat Procedure 
FUNCTION getLocalQueryStat RETURNS CHARACTER
  ( INPUT ihBrowseOrQuery AS HANDLE,
    INPUT icStatFields    AS CHAR ) : 
/*------------------------------------------------------------------------------
  Purpose:  Sum up listed fields in a local query
    Notes:  If no fields are specified go and get them from the querystatfields attribute
            and then store the result back in the querystatfieldvalues
------------------------------------------------------------------------------*/
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hQueryBuffer  AS HANDLE NO-UNDO.
DEF VAR cStatFields   AS CHAR   NO-UNDO.
DEF VAR fStatValues   AS DEC    NO-UNDO EXTENT 100.
DEF VAR cReturnString AS CHAR   NO-UNDO.
DEF VAR iCount        AS INT    NO-UNDO.

IF icStatFields = "" THEN
  cStatFields = DYNAMIC-FUNCTION("getAttribute",ihBrowseOrQuery,"querystatfields").
ELSE cStatFields = icStatFields.

IF ihBrowseOrQuery:TYPE = "browse" THEN
  hBuffer = ihBrowseOrQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hBuffer = ihBrowseOrQuery:GET-BUFFER-HANDLE(1).

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  DO ix = 1 TO NUM-ENTRIES(cStatFields):
    fStatValues[ix] = fStatValues[ix] + hQueryBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
  END.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  cReturnString = cReturnString + ENTRY(ix,cStatFields) + "|" + STRING(fStatValues[ix]) + ";".
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"localstatvalue" + ENTRY(ix,cStatFields),STRING(fStatValues[ix])).
END.

IF icStatFields = "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"querystatfieldvalues",TRIM(cReturnString,";")).
  DYNAMIC-FUNCTION("setAttribute",ihBrowseOrQuery,"recordcount",STRING(iCount)).
  DYNAMIC-FUNCTION("ViewRecordCount",ihBrowseOrQuery).
END.
ELSE 
  cReturnString = "rowcount|" + STRING(iCount) + ";" + cReturnString.

RETURN TRIM(cReturnString,";").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMyCommandLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyCommandLine Procedure
FUNCTION getMyCommandLine RETURNS CHARACTER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cCmdLine AS CHAR NO-UNDO.

cCmdLine = cExeFile.
FOR EACH ttStartup:
  cCmdLine = cCmdLine + " " + ttStartup.cParam + (IF ttStartup.cValue NE "" THEN " " + ttStartup.cValue ELSE "").
END.
		
RETURN cCmdLine.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getMyExeFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyExeFile Procedure
FUNCTION getMyExeFile RETURNS CHARACTER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
RETURN cExeFile.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getMyIniFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyIniFile Procedure
FUNCTION getMyIniFile RETURNS CHARACTER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
RETURN cIniFile.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getMyPid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyPid Procedure
FUNCTION getMyPid RETURNS INTEGER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
RETURN iPid.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getSubProcessWorkDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSubProcessWorkDir Procedure
FUNCTION getSubProcessWorkDir RETURNS CHARACTER 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RETURN cSubProcessWorkDir.

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

&IF DEFINED(EXCLUDE-getProcedureHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProcedureHandle Procedure 
FUNCTION getProcedureHandle RETURNS HANDLE
  ( INPUT icProcName AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.

IF INDEX(icProcName," ") > 0 THEN
  icProcName = substr(icProcName,R-INDEX(icProcName," ") + 1).
hProc = SESSION:LAST-PROCEDURE.
REPEAT WHILE VALID-HANDLE(hProc):
  IF hProc:FILE-NAME = icProcName THEN LEAVE.
  hProc = hProc:PREV-SIBLING.
END.  
IF hProc = ? THEN hProc = hClassSourceProc. /* class invoked from non-persistent proc prior to V11 - must be set manually */
RETURN hProc.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getStartupParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStartupParam Procedure
FUNCTION getStartupParam RETURNS CHARACTER 
  ( INPUT icParameter AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
FIND FIRST ttStartup 
     WHERE ttStartup.cParam = icParameter
     NO-ERROR.
IF AVAIL ttStartup AND ttStartup.cValue NE "" THEN RETURN ttStartup.cParam + " " + ttStartup.cValue.
ELSE IF AVAIL ttStartup THEN RETURN ttStartup.cParam.
ELSE IF LENGTH(icParameter) > 2 THEN DO:
  FIND FIRST ttStartup 
       WHERE ttStartup.cParam BEGINS icParameter
       NO-ERROR.
  IF AVAIL ttStartup AND ttStartup.cValue NE "" THEN RETURN ttStartup.cParam + " " + ttStartup.cValue.
  ELSE IF AVAIL ttStartup THEN RETURN ttStartup.cParam.
END.
RETURN "".     

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getTmpDocFileNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTmpDocFileNames Procedure 
FUNCTION getTmpDocFileNames RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Return temporary file name(s) for last retrieved document(s) 
    Notes:  
------------------------------------------------------------------------------*/

RETURN cTmpDocFileNames. 

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

&IF DEFINED(EXCLUDE-getUserConfirmation) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUserConfirmation Procedure 
FUNCTION getUserConfirmation RETURNS LOGICAL
  ( INPUT icPwd            AS CHAR,
    INPUT icUserList       AS CHAR,
    INPUT iiResourceId     AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Get user password confirmation (before an action is allowed)
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR rawKey AS RAW NO-UNDO.
DEF VAR rawPwd AS RAW NO-UNDO.
DEF VAR mpPwd  AS MEMPTR NO-UNDO.
rawKey = GENERATE-RANDOM-KEY.
mpPwd = ENCRYPT(icPwd,rawKey).
rawPwd = mpPwd.

IF DYNAMIC-FUNCTION("getAppServiceHandle") NE ? THEN DO:
  RUN jbserv_confirmuser.p
      ON DYNAMIC-FUNCTION("getAppServiceHandle")
     (DYNAMIC-FUNCTION("getSessionId"),
      DYNAMIC-FUNCTION("getASUserId"),
      rawPwd,
      rawKey,
      icUserList,
      iiResourceId,
      OUTPUT bOK,
      OUTPUT cReturnTrans)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF DYNAMIC-FUNCTION("ReconnectServer") THEN 
      bOk = getUserConfirmation (icPwd,icUserList,iiResourceId).
  END.
END.
ELSE 
  RUN jbserv_confirmuser.p
    (DYNAMIC-FUNCTION("getSessionId"),
     DYNAMIC-FUNCTION("getASUserId"),
     rawPwd,
     rawKey,
     icUserList,
     iiResourceId,
     OUTPUT bOK,
     OUTPUT cReturnTrans)
     .

IF cReturnTrans BEGINS "dblist;" THEN
  DYNAMIC-FUNCTION("setAttribute",SESSION,"dblist",ENTRY(2,cReturnTrans,";")).
ELSE
  DYNAMIC-FUNCTION("CheckReturnValue",cReturnTrans).

RETURN bOk. 

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

IF NOT bLoadDocsAsync THEN
  SESSION:SET-WAIT-STATE("general").

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

IF DYNAMIC-FUNCTION("getAppServiceHandle") NE ? THEN DO:
  IF bLoadDocsAsync THEN DO:
    RUN jbdoc_savedoc.p
        ON DYNAMIC-FUNCTION("getAppServiceHandle") ASYNCHRONOUS
       (DYNAMIC-FUNCTION("getSessionId"),
        TABLE-HANDLE httDoc,
        cDocLoadParam,
        OUTPUT bOk,
        OUTPUT cOutParam)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      IF DYNAMIC-FUNCTION("ReconnectServer") THEN 
        RUN jbdoc_savedoc.p
            ON DYNAMIC-FUNCTION("getAppServiceHandle") ASYNCHRONOUS
           (DYNAMIC-FUNCTION("getSessionId"),
            TABLE-HANDLE httDoc,
            cDocLoadParam,
            OUTPUT bOk,
            OUTPUT cOutParam).
    END.
  END.
  ELSE DO:
    RUN jbdoc_savedoc.p
        ON DYNAMIC-FUNCTION("getAppServiceHandle") 
       (DYNAMIC-FUNCTION("getSessionId"),
        TABLE-HANDLE httDoc,
        cDocLoadParam,
        OUTPUT bOk,
        OUTPUT cOutParam)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      IF DYNAMIC-FUNCTION("ReconnectServer") THEN 
        RUN jbdoc_savedoc.p
            ON DYNAMIC-FUNCTION("getAppServiceHandle")
           (DYNAMIC-FUNCTION("getSessionId"),
            TABLE-HANDLE httDoc,
            cDocLoadParam,
            OUTPUT bOk,
            OUTPUT cOutParam).
    END.
  END.
END.
ELSE 
  RUN jbdoc_savedoc.p
     (DYNAMIC-FUNCTION("getSessionId"),
      TABLE-HANDLE httDoc,
      cDocLoadParam,
      OUTPUT bOk,
      OUTPUT cOutParam)
      .

ASSIGN cDocLoadParam  = ""
       bLoadDocsAsync = NO.

SESSION:SET-WAIT-STATE("").
  
RETURN bOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadUniqueDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadUniqueDocs Procedure 
FUNCTION LoadUniqueDocs RETURNS LOGICAL
  ( INPUT icFileNames   AS CHAR,
    INPUT icContext     AS CHAR,
    INPUT icEntityId    AS CHAR,
    INPUT icDescription AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Make sure that each relation between entity and document becomes unique
    Notes:  When using this method the join from an entity to documents must use BEGINS, eg:
            ,EACH JBoxDocRel WHERE cContext = "<some context, normally the table name>" AND cEntityId BEGINS "<string value of the primary key>"
------------------------------------------------------------------------------*/
DEF VAR idx AS INT NO-UNDO.

DO idx = 1 TO NUM-ENTRIES(icFileNames,";"):
  LoadDocs(ENTRY(idx,icFileNames,";"),
           icContext,
           icEntityId  + "_" + STRING(TODAY) + "_" + DYNAMIC-FUNCTION("getASuserId") + "_" + STRING(TIME) + STRING(NUM-ENTRIES(icFileNames,";") * -1 - idx),
           icDescription).
END.
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelectFileNames) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SelectFileNames Procedure 
FUNCTION SelectFileNames RETURNS CHARACTER (
  INPUT FilterList       AS CHARACTER,
  INPUT InitialDirectory AS CHARACTER,
  INPUT DialogTitle      AS CHARACTER):

/*------------------------------------------------------------------------------
  Purpose:     Replaces the SYSTEM-DIALOG-GET-FILE common dialog,
               supports multiselect.
               by Scott Anderson, Stuart Morris and Jurjen
               
  Parameters:  Filterlist could be f.ex:
                                   "Word Documents (*.doc,*.rtf)|*.doc;*.rtf" + "|" +
                                   "Excel Worksheets (*.xls)|*.xls"           + "|" +
                                   "Access Databases (*.mdb)|*.mdb"           + "|" +
                                   "All (doc,rtf,xls,mdb,ppt)|*.doc;*.rtf;*.xls;*.mdb;*.ppt"  
------------------------------------------------------------------------------*/
  DEF VAR FileNames        AS CHARACTER NO-UNDO.

  IF PROVERSION GE "11" THEN
    RUN SelectFileNames.p (FilterList,InitialDirectory,DialogTitle,OUTPUT FileNames).
  ELSE DO:
      

    DEF VAR iOK              AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE Flags           AS INTEGER NO-UNDO.
    DEFINE VARIABLE lpOfn           AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE lpstrFilter     AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE lpstrTitle      AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE lpstrInitialDir AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE lpstrFile       AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE offset          AS INTEGER NO-UNDO.
 
    IF VALID-HANDLE(SOURCE-PROCEDURE:CURRENT-WINDOW) THEN
      CURRENT-WINDOW = SOURCE-PROCEDURE:CURRENT-WINDOW.

    /* Flags controls the behaviour and appearance of the dialog-box. 
             There is much room for experiments. This combination works nice: */
    Flags = {&OFN_ALLOWMULTISELECT} + 
            {&OFN_EXPLORER} + 
            {&OFN_NOCHANGEDIR}.
 
    /* convert the "|"-separated list of filters to a CHR(0)-separated 
       list and make sure it's terminated with a double CHR(0): */
    FilterList = TRIM(FilterList,"|") + "|". /* this will cause the double CHR(0) */
    SET-SIZE(lpstrFilter)      = LENGTH(FilterList) + 1.
    PUT-STRING(lpstrFilter, 1) = FilterList.
    DO offset=1 TO GET-SIZE(lpstrFilter) :
       IF GET-BYTE(lpstrFilter,offset)=124 /* =ASC("|") */ THEN 
          PUT-BYTE(lpstrFilter,offset)=0.
    END.
 
    /* get memory-pointers to the string parameters: */
    SET-SIZE(lpstrFile)   = 1024. /* room for a couple of files...     */
    PUT-BYTE(lpstrFile,1) = 0.    /* don't initialize dialog to a file */
   
    SET-SIZE(lpstrTitle) = LENGTH(DialogTitle) + 1.
    PUT-STRING(lpstrTitle,1) = DialogTitle.
   
    IF InitialDirectory NE ? THEN DO:
       SET-SIZE(lpstrInitialDir) = LENGTH(InitialDirectory) + 1.
       PUT-STRING(lpstrInitialDir,1) = InitialDirectory.
    END.
 
    /* create and initialize an OPENFILENAME structure: */
    SET-SIZE(lpOfn) = 76. /* = {&OPENFILENAME_SIZE_VERSION_400} 
                               to be used in NT4 and Windows 95/98. 
                               Windows 2000 supports a couple more fields. */
 
    /* size */              PUT-LONG (lpOfn, 1) = GET-SIZE(lpOfn).
    /* hwndOwner */         PUT-LONG (lpOfn, 5) = CURRENT-WINDOW:HWND.
    /* hInstance */         PUT-LONG (lpOfn, 9) = 0.
    /* lpstrFilter */       PUT-LONG (lpOfn,13) = GET-POINTER-VALUE(lpstrFilter).
    /* lpstrCustomFilter */ PUT-LONG (lpOfn,17) = 0.
    /* nMaxCustFilter */    PUT-LONG (lpOfn,21) = 0.
    /* nFilterIndex */      PUT-LONG (lpOfn,25) = 0.
    /* lpstrFile */         PUT-LONG (lpOfn,29) = GET-POINTER-VALUE(lpstrFile).
    /* nMaxFile */          PUT-LONG (lpOfn,33) = GET-SIZE(lpstrFile).
    /* lpstrFileTitle */    PUT-LONG (lpOfn,37) = 0.
    /* nMaxFileTitle */     PUT-LONG (lpOfn,41) = 0.
    /* lpstrInitialDir */   PUT-LONG (lpOfn,45) = GET-POINTER-VALUE(lpstrInitialDir).
    /* lpstrTitle */        PUT-LONG (lpOfn,49) = GET-POINTER-VALUE(lpstrTitle).
    /* flags */             PUT-LONG (lpOfn,53) = Flags.
     
    /* nFileOffset */       PUT-SHORT(lpOfn,57) = 0.
    /* nFileExtension */    PUT-SHORT(lpOfn,59) = 0.
    /* lpstrDefExt */       PUT-LONG (lpOfn,61) = 0.
    /* lCustData */         PUT-LONG (lpOfn,65) = 0.
    /* lpfnHook */          PUT-LONG (lpOfn,69) = 0.
    /* lpTemplateName */    PUT-LONG (lpOfn,73) = 0.
   
    /* run the dialog: */
    RUN GetOpenFileNameA (GET-POINTER-VALUE(lpOfn), OUTPUT iOK).
 
    /* release memory: */
    SET-SIZE(lpstrFilter)     = 0.
    SET-SIZE(lpOfn)           = 0.
    SET-SIZE(lpstrTitle)      = 0.
    SET-SIZE(lpstrInitialDir) = 0.
 
    /* lpstrFilter now contains a path, followed by CHR(0), followed 
       by a CHR(0)-separated list of filenames, terminated by a double CHR(0). 
       Unless the user selected only one file: then lpstrFilter will simply
       contain the fully-qualified filename.
       Either way, let's convert the result to a comma-separated list of 
       fully-qualified filenames: */
 
    IF iOK NE 0 THEN DO:
      DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cList AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
   
      ASSIGN cPath  = GET-STRING(lpstrFile,1)
             offset = LENGTH(cPath) + 2.
   
      REPEAT:
        cFile = GET-STRING(lpstrFile, offset).
        IF cFile = "" THEN LEAVE.
        ASSIGN cList  = cList + ';' + cPath +  '\' + cFile
               offset = offset + LENGTH(cFile) + 1.
      END.
      ASSIGN cList     = TRIM(cList, ";")
             FileNames = IF cList = "" THEN cPath ELSE cList.
    END.
   
    SET-SIZE(lpstrFile) = 0.
  END.

  RETURN FileNames.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendOutlookMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sendOutlookMail Procedure 
FUNCTION sendOutlookMail RETURNS CHARACTER
  ( INPUT icTO      AS CHAR,
    INPUT icCC      AS CHAR,
    INPUT icBCC     AS CHAR,
    INPUT icSubject AS CHAR,
    INPUT icBody    AS CHAR,
    INPUT icAttach  AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR chOutlook  AS COM-HANDLE NO-UNDO.
DEF VAR hItem      AS COM-HANDLE NO-UNDO.
DEF VAR hAttach    AS COM-HANDLE NO-UNDO.
DEF VAR iAttchFile AS INT NO-UNDO.
DEF VAR bAtt       AS LOG NO-UNDO.

CREATE "outlook.application" chOutlook.

ASSIGN hItem = chOutlook:CreateItem(0).

DO ix = 1 TO NUM-ENTRIES(icAttach,";"):
  ASSIGN hAttach = hItem:Attachments:ADD(ENTRY(ix,icAttach,";"))
         bAtt    = TRUE.
END.

ASSIGN hItem:Subject = icSubject
       hItem:body    = icBody
       .

IF icTO NE "" THEN
  hItem:TO  = icTO.
IF icCC NE "" THEN
  hItem:CC  = icCC.
IF icBCC NE "" THEN
  hItem:BCC = icBCC.


hItem:SAVE().
hItem:SEND().

RELEASE OBJECT chOutlook.
IF bAtt THEN RELEASE OBJECT hAttach.
RELEASE OBJECT hItem.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setClassSourceProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setClassSourceProc Procedure 
FUNCTION setClassSourceProc RETURNS LOGICAL
  ( INPUT ihClassSourceProc AS HANDLE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
hClassSourceProc = ihClassSourceProc.
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

&IF DEFINED(EXCLUDE-setFileCompression) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFileCompression Procedure 
FUNCTION setFileCompression RETURNS LOGICAL
  ( INPUT ibCompressFiles AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bCompressFiles = ibCompressFiles.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFrameFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFrameFields Procedure 
FUNCTION setFrameFields RETURNS LOGICAL
  ( INPUT ihFrame     AS HANDLE,
    INPUT icFieldList AS CHAR,
    INPUT icValueList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWdgt AS HANDLE NO-UNDO.          
hWdgt = ihFrame:FIRST-CHILD:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hWdgt):
  IF CAN-DO(icFieldList,hWdgt:NAME) THEN
    hWdgt:SCREEN-VALUE = ENTRY(LOOKUP(hWdgt:NAME,icFieldList),icValueList,"|").
  hWdgt = hWdgt:NEXT-SIBLING.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLoadDocsAsync) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLoadDocsAsync Procedure 
FUNCTION setLoadDocsAsync RETURNS LOGICAL
  ( INPUT ibLoadDocsAsync AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bLoadDocsAsync = ibLoadDocsAsync.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMyIniFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMyIniFile Procedure
FUNCTION setMyIniFile RETURNS LOGICAL 
  ( INPUT icIniFile AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
cIniFile = icIniFile.

RETURN YES.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setMyWorDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMyWorDir Procedure
FUNCTION setSubProcessWorkDir RETURNS LOGICAL 
  ( INPUT icSubProcessWorkDir AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

cSubProcessWorkDir = icSubProcessWorkDir.

RETURN YES.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setNoCompressTypes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoCompressTypes Procedure 
FUNCTION setNoCompressTypes RETURNS LOGICAL
  ( INPUT icNoCompressTypes AS CHAR,
    INPUT ibAppend          AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ibAppend THEN
  cNoCompressTypes = TRIM(cNoCompressTypes,",") + "," + icNoCompressTypes.
ELSE
  cNoCompressTypes = icNoCompressTypes.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSuppressOpenDocError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSuppressOpenDocError Procedure 
FUNCTION setSuppressOpenDocError RETURNS LOGICAL
  ( INPUT ibSupprOpenDocMsg AS LOG ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
bSupprOpenDocMsg = ibSupprOpenDocMsg.
IF bSupprOpenDocMsg THEN
  ASSIGN cSupprOpenDocMsg = ""
         cOpenFileErrorList = ""
         cSaveDir = "".

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setUseOrgFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUseOrgFileName Procedure 
FUNCTION setUseOrgFileName RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
bUseOrgFileName = YES.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setViewFileNamePrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setViewFileNamePrefix Procedure 
FUNCTION setViewFileNamePrefix RETURNS LOGICAL
  ( INPUT icViewFileNamePrefix AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Must be set before each call of ViewDocs 
------------------------------------------------------------------------------*/
cViewFileNamePrefix = icViewFileNamePrefix.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SplitText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SplitText Procedure 
FUNCTION SplitText RETURNS CHARACTER
  ( INPUT icText             AS CHAR,
    INPUT iiLength           AS INT,
    INPUT ibSplitOnLineshift AS LOG,
    INPUT icDelimiter        AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Split a text in multiple parts
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPartText AS CHAR NO-UNDO.
DEF VAR iSplit    AS INT  NO-UNDO.
DEF VAR ix        AS INT  NO-UNDO.

IF icText = "" THEN
  RETURN "".
IF icDelimiter = "" THEN
  icDelimiter = "|".

IF ibSplitOnLineshift THEN
  iSplit = R-INDEX(icText,CHR(10),iiLength).
IF iSplit = 0 THEN
  DO ix = iiLength TO 1 BY -1:
    IF SUBSTR(icText,ix,1) = " " THEN DO:
      iSplit = ix.
      LEAVE.
    END.
  END.

IF iSplit = 0 THEN iSplit = iiLength.

cPartText = REPLACE(SUBSTR(icText,1,iSplit),CHR(10)," ").

RETURN cPartText +
       (IF cPartText NE "" THEN
         icDelimiter + SplitText(SUBSTR(icText,iSplit + 1),iiLength,ibSplitOnLineshift,icDelimiter)
        ELSE "").


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-syncLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION syncLists Procedure 
FUNCTION syncLists RETURNS CHARACTER
  ( INPUT icList1        AS CHAR,
    INPUT icList2        AS CHAR,
    INPUT icDelimiter    AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Sync list1 with list2. Not existing elements in list2 are removed,
           any new elements are added at the end of list1
    Notes: 
------------------------------------------------------------------------------*/
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR cReturnList AS CHAR NO-UNDO.

IF icDelimiter = "" THEN icDelimiter = ",".

DO ix = 1 TO NUM-ENTRIES(icList1,icDelimiter):
  IF LOOKUP(ENTRY(ix,icList1,icDelimiter),icList2,icDelimiter) > 0 THEN 
    cReturnList = cReturnList + (IF cReturnList NE "" THEN icDelimiter ELSE "") + ENTRY(ix,icList1,icDelimiter).
END.
DO ix = 1 TO NUM-ENTRIES(icList2,icDelimiter):
  IF LOOKUP(ENTRY(ix,icList2,icDelimiter),icList1,icDelimiter) = 0 THEN 
    cReturnList = cReturnList + (IF cReturnList NE "" THEN icDelimiter ELSE "") + ENTRY(ix,icList2,icDelimiter).
END.

RETURN cReturnList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TextToFileName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TextToFileName Procedure 
FUNCTION TextToFileName RETURNS CHARACTER
  ( INPUT icText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Remove illegal characters in file names from text
    Notes:  
------------------------------------------------------------------------------*/
icText = REPLACE(icText,"?","").
icText = REPLACE(icText,":","").
icText = REPLACE(icText,"/","").
icText = REPLACE(icText,"\","").
icText = REPLACE(icText,"*","").
icText = REPLACE(icText,"<","").
icText = REPLACE(icText,">","").
icText = REPLACE(icText,"|","").

RETURN icText.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ttToFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ttToFile Procedure
FUNCTION ttToFile RETURNS LOGICAL 
  ( INPUT ihBuffer     AS HANDLE,
    INPUT icFileName   AS CHAR,
    INPUT icFieldList  AS CHAR,
    INPUT icQueryWhere AS CHAR,
    INPUT icSortBy     AS CHAR,
    INPUT ibOpenFile   AS LOG):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR cSort       AS CHAR   NO-UNDO.
DEF VAR cQueryWhere AS CHAR   NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

ASSIGN icSortBy      = TRIM(icSortBy)
       icQueryWhere  = TRIM(icQueryWhere).

IF icQueryWhere NE "" THEN
  cQueryWhere = (IF icQueryWhere BEGINS "WHERE " THEN " " ELSE " WHERE ") + icQueryWhere.
  
IF icSortBy NE "" THEN 
  cSort = (IF icSortBy BEGINS "BY " THEN " " ELSE " BY ") + icSortBy.
    
IF icFieldList = "" THEN     
  DO ix = 1 TO ihBuffer:NUM-FIELDS:
    icFieldList = icFieldList + (IF icFieldList NE "" THEN "," ELSE "") + ihBuffer:BUFFER-FIELD (ix):NAME.
  END.  

OUTPUT TO VALUE(icFileName).

CREATE QUERY hQuery.

hQuery:SET-BUFFERS (ihBuffer).
hQuery:QUERY-PREPARE ("FOR EACH " + ihBuffer:NAME + cQueryWhere + cSort).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  DO ix = 1 TO NUM-ENTRIES(icFieldList):
    PUT UNFORMATTED ihBuffer:BUFFER-FIELD(ENTRY(ix,icFieldList)):BUFFER-VALUE.
    IF ix = NUM-ENTRIES(icFieldList) THEN
      PUT " ".
  END. 
  PUT SKIP. 
  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

IF ibOpenFile THEN DYNAMIC-FUNCTION("setWebDoc","",icFileName).

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-ValidCharTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidCharTime Procedure 
FUNCTION ValidCharTime RETURNS LOGICAL
  ( INPUT ihWidget AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Validates a character variable used for time input (colon in format)
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTString AS CHAR NO-UNDO.
DEF VAR iHrs     AS INT  NO-UNDO.
DEF VAR iMin     AS INT  NO-UNDO.
DEF VAR iSec     AS INT  NO-UNDO.

cTString = ihWidget:SCREEN-VALUE.

iHrs = INTEGER(ENTRY(1,cTString,":")) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN NO.

iMin = INTEGER(ENTRY(2,cTString,":")) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN NO.

IF NUM-ENTRIES(cTString,":") > 2 THEN DO:
  iSec = INTEGER(ENTRY(3,cTString,":")) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO.
END.

IF iHrs > 24 THEN RETURN NO.
IF iMin > 59 THEN RETURN NO.
IF iSec > 59 THEN RETURN NO.

IF iHrs = 24 AND (iMin > 0 OR iSec > 0)  THEN RETURN NO.

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

DYNAMIC-FUNCTION ("setTransactionMessage",""). /* Borrows the message mechanism from ASlib so the message needs to be cleared first.. */       

IF NOT bChunkInProgress THEN DO:
  ASSIGN cTmpDocFileNames = ""
         cOrgDocFileNames = ""
         cDocFileDates    = ""
         cDocFileDescs    = ""
         cDocFileIds      = ""
         cDocFileTypes    = ""
         .
  RUN ChunkDownload(icContext,icEntityId,ibOpen,icSaveDir,OUTPUT bChunk,OUTPUT bChunkOk).
  IF bChunk THEN DO:      
    IF cGZIP NE ? THEN
      cTmpDocFileNames = REPLACE(cTmpDocFileNames,".gz|","|").

    ASSIGN cTmpDocFileNames = TRIM(cTmpDocFileNames,"|")
           cOrgDocFileNames = TRIM(cOrgDocFileNames,"|")
           cDocFileDates    = TRIM(cDocFileDates,",")
           cDocFileDescs    = SUBSTR(cDocFileDescs,1,LENGTH(cDocFileDescs) - 1)
           cDocFileIds      = TRIM(cDocFileIds,",")
           cDocFileTypes    = TRIM(cDocFileTypes,",")
           bUseOrgFileName  = NO           
           .

    RETURN bChunkOk.
  END.
END.

SESSION:SET-WAIT-STATE("general").

IF DYNAMIC-FUNCTION("getAppServiceHandle") NE ? THEN DO:
  RUN jbdoc_getdoc.p
      ON DYNAMIC-FUNCTION("getAppServiceHandle")
     (DYNAMIC-FUNCTION("getSessionId"),
      icContext + "|" + icEntityId,
      OUTPUT TABLE-HANDLE hTempTable,
      OUTPUT cOutParam,
      OUTPUT bOk)
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    IF DYNAMIC-FUNCTION("ReconnectServer") THEN 
      RUN jbdoc_getdoc.p
          ON DYNAMIC-FUNCTION("getAppServiceHandle")
         (DYNAMIC-FUNCTION("getSessionId"),
          icContext + "|" + icEntityId,
          OUTPUT TABLE-HANDLE hTempTable,
          OUTPUT cOutParam,
          OUTPUT bOk).
  END.
END.
ELSE 
  RUN jbdoc_getdoc.p
     (DYNAMIC-FUNCTION("getSessionId"),
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

  IF NOT bChunkInProgress THEN DO: /* ViewDocs can be called from ChunkDownload */
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
      DYNAMIC-FUNCTION ("setTransactionMessage","No files found for entity reference").       
  END.

  DELETE OBJECT hTempQuery.
  DELETE OBJECT hTempTable.
  SET-SIZE(mpDocument) = 0.
END.
SESSION:SET-WAIT-STATE("").

bChunkInProgress = NO.
bUseOrgFileName = NO.           

RETURN cTmpDocFileNames NE "". 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewOpenDocError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewOpenDocError Procedure 
FUNCTION ViewOpenDocError RETURNS CHARACTER
  ( INPUT ibView AS LOG ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR oServer AS COM-HANDLE NO-UNDO.

IF ibView AND cSupprOpenDocMsg NE "" THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,cSupprOpenDocMsg + CHR(10) + "Filename(s) " + cOpenFileErrorList,"","").
  IF cSaveDir NE "" THEN DO:
    CREATE 'Shell.Application' oServer.
    NO-RETURN-VALUE oServer:Explore(cSaveDir).
    RELEASE OBJECT oServer.
  END.    
  ELSE IF cSupprOpenDocMsg NE "" THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,cSupprOpenDocMsg + CHR(10) + "Filename(s): " + cOpenFileErrorList,"","").  
END.  

bSupprOpenDocMsg = NO.

RETURN cSupprOpenDocMsg.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

