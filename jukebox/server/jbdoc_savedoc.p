DEF INPUT PARAM  icSessionId AS CHAR NO-UNDO.
DEF INPUT PARAM TABLE-HANDLE hTempTable.
DEF INPUT PARAM  icParam     AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR hDocBuffer       AS HANDLE NO-UNDO.
DEF VAR hDocRelBuffer    AS HANDLE NO-UNDO.
DEF VAR bRegister        AS LOG    NO-UNDO INIT TRUE.
DEF VAR ocTargetDBtable  AS CHAR   NO-UNDO.
DEF VAR ocFirstKeyString AS CHAR   NO-UNDO.
DEF VAR ocLastKeyString  AS CHAR   NO-UNDO.
DEF VAR rBreak           AS ROWID  NO-UNDO.
DEF VAR cDocIdList       AS CHAR   NO-UNDO.
DEF VAR bCreateNewDoc    AS LOG    NO-UNDO.
DEF VAR cImportLogIdList AS CHAR   NO-UNDO.
DEF VAR iImportLogId     AS INT    NO-UNDO.
DEF VAR cImportHeaderId  AS CHAR   NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR bSmallDoc        AS LOG    NO-UNDO.
DEF VAR iReplaceDocId    AS INT    NO-UNDO.
DEF VAR hDocRevBuffer    AS HANDLE NO-UNDO.
DEF VAR hSeq             AS HANDLE NO-UNDO.
DEF VAR hDocMetaBuffer   AS HANDLE NO-UNDO.
DEF VAR cDocMetaReturn   AS CHAR   NO-UNDO.
DEF VAR cPrevImgId       AS CHAR   NO-UNDO.
DEF VAR cPrevImgFile     AS CHAR   NO-UNDO.
DEF VAR cUploadProc      AS CHAR   NO-UNDO.
DEF VAR cUploadParam     AS CHAR   NO-UNDO.

{incl/validatesession.i}

DEF TEMP-TABLE ttFileList
    FIELD cFileName AS CHAR 
    FIELD bSmallImg AS LOG
    FIELD iDocId    AS INT
    FIELD iPairId   AS INT
    .

DEF TEMP-TABLE ttImportDataLog NO-UNDO
    FIELD iImportHeaderId   AS INT 
    FIELD cLogType          AS CHARACTER
    FIELD cLogName          AS CHAR
    FIELD cLogDesc          AS CHAR
    FIELD iLineNum          AS INT
    FIELD cLogText          AS CHAR
    .
DEF VAR httImportDataLog    AS HANDLE NO-UNDO.
DEF VAR hBuffImportDataLog  AS HANDLE NO-UNDO.
httImportDataLog   = BUFFER ttImportDataLog:TABLE-HANDLE.
hBuffImportDataLog = httImportDataLog:DEFAULT-BUFFER-HANDLE.

FUNCTION CreImportDataLog RETURNS LOGICAL (INPUT icLogType AS CHAR,INPUT icLogName AS CHAR,INPUT icLogDesc AS CHAR, INPUT iiLineNum AS INT,icLogText AS CHAR):    
  CREATE ttImportDataLog.
  ASSIGN ttImportDataLog.cLogType = icLogType
         ttImportDataLog.cLogName = icLogName
         ttImportDataLog.cLogDesc = icLogDesc
         ttImportDataLog.iLineNum = iiLineNum
         ttImportDataLog.cLogText = icLogText
         ttImportDataLog.iImportHeaderId = INTEGER(hBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE)
         .
END FUNCTION.

FUNCTION getASuserId     RETURNS CHARACTER FORWARD.
FUNCTION getCompanyId    RETURNS INTEGER   FORWARD.
FUNCTION getCodeMaster   RETURNS INTEGER   FORWARD.
FUNCTION getLanguageCode RETURNS CHARACTER FORWARD.
FUNCTION Scandinavian    RETURNS LOGICAL   FORWARD.
FUNCTION getSysParam     RETURNS CHARACTER
       ( INPUT icParamName AS CHAR,
         INPUT icValueType AS CHAR ) FORWARD.
FUNCTION getGenCode RETURNS CHARACTER
  ( INPUT icCodeType  AS CHAR,
    INPUT icValueDesc AS CHAR ) FORWARD.

hDocBuffer = BUFFER JBoxDocument:HANDLE.
hDocRelBuffer = BUFFER JBoxDocRel:HANDLE.
hBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.
hSeq = hBuffer:BUFFER-FIELD("iSeq") NO-ERROR.

CREATE BUFFER hDocMetaBuffer FOR TABLE "JBoxDocMeta" NO-ERROR.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).

hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + (IF VALID-HANDLE(hSeq) THEN " BY iSeq" ELSE "")).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF icParam MATCHES "*replacedocpair*" THEN DO:   
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ix = ix + 1.
    IF STRING(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE) BEGINS "small" THEN bSmallDoc = YES.
    hQuery:GET-NEXT().
  END.
  IF ix NE 2 OR NOT bSmallDoc THEN DO:
    ocReturn = "Invalid combination of load parameter 'replaceDocPair' and number of loaded files (must be 2, where one is 'small')".
    DELETE OBJECT hQuery.
    RETURN.
  END.
  ELSE hQuery:GET-FIRST().
END.

IF ENTRY(1,icParam,CHR(1)) BEGINS "docUploadProc" THEN DO:   
  ASSIGN cUploadProc  = ENTRY(2,icParam,CHR(1))
         cUploadParam = ENTRY(3,icParam,CHR(1)).
   
  bRegister = ENTRY(1,icParam,CHR(1)) MATCHES "*saveDocToDb".
         
  IF SEARCH(cUploadProc) = ? AND SEARCH(SUBSTR(cUploadProc,1,LENGTH(cUploadProc) - 1) + "r") = ? THEN DO:
    ASSIGN ocReturn = "Could not find procedure for document upload: " + cUploadProc
           obOk     = NO.
    RETURN.              
  END.   
  IF NOT bRegister THEN DO: 
    RUN VALUE(cUploadProc) (cUploadParam,hBuffer,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
    RETURN.
  END.   
END.            

REPEAT TRANSACTION ON ERROR UNDO, LEAVE WHILE NOT hQuery:QUERY-OFF-END:

  IF hBuffer:BUFFER-FIELD("cContext"):BUFFER-VALUE = "JBoxFileImportHeader" THEN DO: 
    RUN jbdoc_loadfile.p (hBuffer,
                          icSessionId,
                          hBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE,
                          icParam,
                          OUTPUT bRegister,
                          OUTPUT ocTargetDBtable,
                          OUTPUT ocFirstKeyString,
                          OUTPUT ocLastKeyString,
                          OUTPUT ocReturn,
                          OUTPUT obOk).
    IF NOT obOk OR icParam MATCHES "*test*" THEN DO:
      IF ocReturn = "" AND icParam MATCHES "*test*" AND NOT CAN-FIND(FIRST ttImportDataLog) THEN
        ocReturn = "File import test successful".
      ELSE rBreak = hBuffer:ROWID.
      UNDO, LEAVE.
    END.
    ELSE DO:
      cImportHeaderId = hBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE.
      RUN jbdoc_logfileimport.p (hBuffer,
                                 ocReturn,
                                 ocTargetDBtable,
                                 ocFirstKeyString,
                                 ocLastKeyString,
                                 OUTPUT iImportLogId).
      cImportLogIdList = cImportLogIdList + (IF cImportLogIdList NE "" THEN ";" ELSE "") 
                       + cImportHeaderId + "|" + STRING(iImportLogId).
    END.
  END.

  IF bRegister THEN DO:
    bCreateNewDoc = YES.
    IF icParam MATCHES "*replacedocpair*" THEN DO:     
      FOR EACH JBoxDocRel NO-LOCK
          WHERE JBoxDocRel.cContext  = STRING(hBuffer:BUFFER-FIELD("cContext"):BUFFER-VALUE)
            AND JBoxDocRel.cEntityId = STRING(hBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE) 
         ,FIRST JBoxDocument EXCLUSIVE-LOCK
                OF JBoxDocRel
                WHERE (IF STRING(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE) BEGINS "small" THEN
                         JBoxDocument.cFileName BEGINS "small"
                       ELSE IF NOT STRING(hBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE) BEGINS "small" THEN
                         NOT JBoxDocument.cFileName BEGINS "small"
                       ELSE FALSE):
        hDocBuffer:BUFFER-COPY(hBuffer,"iJBoxDocumentId").
        cDocIdList = cDocIdList + STRING(hDocBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + ",".
        bCreateNewDoc = NO.
      END.
    END.
    ELSE IF icParam MATCHES "*replacedoc*" THEN DO:
      IF hBuffer:BUFFER-FIELD("cContext"):BUFFER-VALUE NE "" THEN DO:
        FIND FIRST JBoxDocRel NO-LOCK 
             WHERE JBoxDocRel.cContext  = STRING(hBuffer:BUFFER-FIELD("cContext"):BUFFER-VALUE)
               AND JBoxDocRel.cEntityId = STRING(hBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE) 
             NO-ERROR.
        IF AVAIL JBoxDocRel THEN 
          FIND FIRST JBoxDocument EXCLUSIVE-LOCK 
               OF JBoxDocRel
               NO-WAIT NO-ERROR.
      END.
      ELSE DO:
        iReplaceDocId = INTEGER(hBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          FIND JBoxDocument EXCLUSIVE-LOCK 
               WHERE JBoxDocument.iJBoxDocumentId = iReplaceDocId
               NO-WAIT NO-ERROR.
      END.
      IF AVAIL JBoxDocument THEN DO:
        bCreateNewDoc = NO.
        IF icParam MATCHES "*saveoldversion*" THEN DO:
          CREATE BUFFER hDocRevBuffer FOR TABLE "JBoxDocRev" NO-ERROR.
          IF VALID-HANDLE(hDocRevBuffer) THEN
            RUN jbdoc_savedocasrevision.p ("",hDocBuffer,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
        END.
        IF ocReturn = "" THEN DO:
          hDocBuffer:BUFFER-COPY(hBuffer,"iJBoxDocumentId").
          cDocIdList = cDocIdList + STRING(hDocBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + ",".
        END.
      END.
    END.
    IF bCreateNewDoc THEN DO:
      CREATE JBoxDocument.
      hDocBuffer:BUFFER-COPY(hBuffer).
      CREATE JBoxDocRel.
      hDocRelBuffer:BUFFER-COPY(hBuffer).
      ASSIGN hDocRelBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE = hDocBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE
             cDocIdList = cDocIdList + STRING(hDocBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + ",".

      CREATE ttFileList.
      ASSIGN ttFileList.cFileName = IF hDocBuffer::cFileName BEGINS "small_" THEN SUBSTR(hDocBuffer::cFileName,7) ELSE hDocBuffer::cFileName
             ttFileList.bSmallImg = hDocBuffer::cFileName BEGINS "small_"
             ttFileList.iDocId    = hDocBuffer::iJBoxDocumentId AS INT
/*              ttFileList.iPairId   AS INT */
             .

    END.
    IF VALID-HANDLE(hDocMetaBuffer) THEN
      RUN jbdoc_post_update.p (hDocBuffer,IF bCreateNewDoc THEN "create" ELSE "update",icSessionId,OUTPUT cDocMetaReturn).
      
    IF cUploadProc NE "" THEN  
      RUN VALUE(cUploadProc) (cUploadParam,hBuffer,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
  END.
    
  hQuery:GET-NEXT().
END.

IF ocReturn = "" THEN 
  ASSIGN ocReturn = TRIM(cDocIdList,",")
         obOk     = YES.
ELSE IF ENTRY(1,ocReturn,"|") = "returnvalue" THEN obOk = TRUE.

IF rBreak NE ? THEN DO TRANSACTION:
  hBuffer:FIND-BY-ROWID(rBreak).
  cImportHeaderId = hBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE.
  RUN jbdoc_logfileimport.p (hBuffer,
                             ocReturn,
                             ocTargetDBtable,
                             "",
                             "",
                             OUTPUT iImportLogId).
  cImportLogIdList = cImportLogIdList + (IF cImportLogIdList NE "" THEN ";" ELSE "") + cImportHeaderId + "|" + STRING(iImportLogId).
END.

IF CAN-FIND(FIRST ttImportDataLog) THEN
  RUN jbdoc_cre_fileimport_datalog.p (cImportLogIdList,hBuffImportDataLog).

IF cDocIdList NE "" AND obOK AND icParam MATCHES "*linkdocs*" THEN
  RUN jbdoc_cre_doclink.p (cDocIdList).

ix = 0.
FOR EACH ttFileList 
    BREAK BY ttFileList.cFileName
          BY ttFileList.bSmallImg:
  ix = ix + 1.
  IF LAST-OF(ttFileList.cFileName) THEN DO:
    IF ix = 2 AND ttFileList.bSmallImg THEN 
      RUN jbdoc_cre_doclink.p (cPrevImgId + "," + STRING(ttFileList.iDocId)).
    ix = 0.
  END.
  ASSIGN cPrevImgFile = ttFileList.cFileName
         cPrevImgId   = STRING(ttFileList.iDocId).
END.

FINALLY:
  DELETE OBJECT hQuery.
  DELETE OBJECT hTempTable.
  DELETE OBJECT hDocMetaBuffer NO-ERROR.
  DELETE OBJECT hDocRevBuffer NO-ERROR.
END.  

FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrUserId.

END FUNCTION.

FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCurrCompanyId. 

END FUNCTION.

FUNCTION getCodeMaster RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCodeMaster. 

END FUNCTION.

FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrLanguage.

END FUNCTION.

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

FUNCTION getSysParam RETURNS CHARACTER
  ( INPUT icParamName AS CHAR,
    INPUT icValueType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cParamValue       AS CHAR   NO-UNDO.
DEF VAR hBuffJboxSysParam AS HANDLE NO-UNDO.

CREATE BUFFER hBuffJboxSysParam FOR TABLE "JBoxSysParam" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN ?.
DELETE OBJECT hBuffJboxSysParam.

IF SEARCH("jbadmin_getsysparam.p") NE ? OR SEARCH("jbadmin_getsysparam.r") NE ? THEN
  RUN jbadmin_getsysparam.p (icParamName,icValueType,iCurrCompanyId,iCodeMaster,OUTPUT cParamValue).
ELSE RETURN ?.

RETURN cParamValue.

END FUNCTION.

FUNCTION getGenCode RETURNS CHARACTER
  ( INPUT icCodeType  AS CHAR,
    INPUT icValueDesc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cCodeValue        AS CHAR   NO-UNDO.
DEF VAR hBuffJboxGenCode  AS HANDLE NO-UNDO.

CREATE BUFFER hBuffJboxGenCode FOR TABLE "JBoxGenCode" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN ?.
DELETE OBJECT hBuffJboxGenCode.

IF SEARCH("jbadmin_getgeneralcode.p") NE ? OR SEARCH("jbadmin_getgeneralcode.r") NE ? THEN
  RUN jbadmin_getgeneralcode.p (icCodeType,icValueDesc,iCurrCompanyId,iCodeMaster,OUTPUT cCodeValue).
ELSE RETURN ?.

RETURN cCodeValue.

END FUNCTION.
