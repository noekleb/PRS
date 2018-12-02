/* jbdoc_getexportfile.p 
   Purpose:    Produce an export temp-table
   Parameters: iJBoxFileExportHeaderId
               
-------------------------------------------------------------------------*/                  
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR iFileExportId   AS INT    NO-UNDO.
DEF VAR cBufferName     AS CHAR   NO-UNDO.
DEF VAR cCriteria       AS CHAR   NO-UNDO.
DEF VAR cBufferCopyFlds AS CHAR   NO-UNDO.
DEF VAR hFileSpecBuffer AS HANDLE NO-UNDO.
DEF VAR hHeaderBuffer   AS HANDLE NO-UNDO.
DEF VAR iRecordCount    AS INT    NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.
DEF VAR httBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hField          AS HANDLE NO-UNDO.
DEF VAR cCompanyId      AS CHAR   NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hExpQuery       AS HANDLE NO-UNDO.
DEF VAR cFileName       AS CHAR   NO-UNDO.
DEF VAR cDelimiter      AS CHAR   NO-UNDO.
DEF VAR cTmpDelim       AS CHAR   NO-UNDO.
DEF VAR cTransMsg       AS CHAR   NO-UNDO.

DEF STREAM sExport.

{incl/validatesession.i}

iFileExportId = INT(icParam) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Invalid identified for file export header: " + icParam.
  RETURN.
END.
FIND JBoxFileExportHeader 
     WHERE JBoxFileExportHeader.iJBoxFileExportHeaderId = iFileExportId
     NO-LOCK NO-ERROR.
IF NOT AVAIL JBoxFileExportHeader THEN DO:
  ocReturn = "Couldn't find export header: " + icParam.
  RETURN.
END.

ASSIGN cBufferName = JBoxFileExportHeader.cSourceDBtable
       cCriteria   = JBoxFileExportHeader.cFilter
       .

IF JBoxFileExportHeader.cExportProg NE "" AND 
   SEARCH(JBoxFileExportHeader.cExportProg) = ? AND 
   SEARCH(SUBSTR(JBoxFileExportHeader.cExportProg,1,LENGTH(JBoxFileExportHeader.cExportProg) - 1) + "r") = ? THEN DO:
  ocReturn = "Couldn't find export program on server: " + JBoxFileExportHeader.cExportProg.
  RETURN.
END.

CREATE BUFFER hFileSpecBuffer FOR TABLE "JBoxFileExportSpec".
CREATE BUFFER hHeaderBuffer   FOR TABLE "JBoxFileExportHeader".
CREATE BUFFER hBuffer         FOR TABLE cBufferName NO-ERROR.

CREATE TEMP-TABLE ohTempTable.
FOR EACH JBoxFileExportSpec OF JBoxFileExportHeader 
    NO-LOCK BY JBoxFileExportSpec.iSeq:
  IF VALID-HANDLE(hBuffer) THEN
    hField = hBuffer:BUFFER-FIELD(JBoxFileExportSpec.cDBfieldName) NO-ERROR.

  IF VALID-HANDLE(hField) THEN DO:
    ohTempTable:ADD-NEW-FIELD(JBoxFileExportSpec.cFieldName,hField:DATA-TYPE).
    cBufferCopyFlds = cBufferCopyFlds 
                    + JBoxFileExportSpec.cFieldName + ","
                    + JBoxFileExportSpec.cDBfieldName + ","
                      .
  END.
  ELSE ohTempTable:ADD-NEW-FIELD(JBoxFileExportSpec.cFieldName,"CHARACTER").
END.

ohTempTable:TEMP-TABLE-PREPARE(cBufferName) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Failed to create export table. Examine export spec".
  RETURN.
END.

ASSIGN httBuffer       = ohTempTable:DEFAULT-BUFFER-HANDLE
       cBufferCopyFlds = TRIM(cBufferCopyFlds,",").

IF cBufferCopyFlds NE "" THEN DO:
  hField = hBuffer:BUFFER-FIELD("iJBoxCompanyId") NO-ERROR.
  IF VALID-HANDLE(hField) THEN DO:
    RUN jbserv_getcompanyid.p (icSessionId,OUTPUT cCompanyId).
    IF cCompanyId NE "" THEN
      cCriteria = cCriteria + " AND iJBoxCompanyId = " + cCompanyId.
  END.

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " NO-LOCK " + cCriteria).
  hQuery:QUERY-OPEN().
  
  hQuery:GET-FIRST().
  
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    iRecordCount = iRecordCount + 1.

    httBuffer:BUFFER-CREATE().  
    httBuffer:BUFFER-COPY(hBuffer,?,cBufferCopyFlds) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      ocReturn = "Error in export at record no " + STRING(iRecordCount) + CHR(10)
               + ERROR-STATUS:GET-MESSAGE(1).
      LEAVE.
    END.
    
    IF JBoxFileExportHeader.cExportProg NE "" THEN DO:
      RUN VALUE(JBoxFileExportHeader.cExportProg) (icParam,httBuffer,hBuffer,icSessionId,
                                                   OUTPUT ocReturn,OUTPUT bOK).
      IF NOT bOk THEN DO:
        ocReturn = "Error in custom export function at record no " + STRING(iRecordCount) + CHR(10) + ocReturn.
        LEAVE.
      END.
    END.

    hQuery:GET-NEXT().
  END.
  
  DELETE OBJECT hQuery.
END.
ELSE DO:
  RUN VALUE(JBoxFileExportHeader.cExportProg) (icParam,httBuffer,icSessionId,
                                               OUTPUT ocReturn,OUTPUT bOK).
  IF NOT bOk THEN 
    ocReturn = "Error in custom export function: " + CHR(10) + ocReturn.
END.

CREATE JBoxFileExportLog.
BUFFER-COPY JBoxFileExportHeader TO JBoxFileExportLog.
ASSIGN JBoxFileExportLog.dCreated                     = TODAY
       JBoxFileExportLog.iCreTime                     = TIME
       JBoxFileExportLog.cErrorMessage                = ocReturn
       .

IF JBoxFileExportHeader.bLogFilesInDB THEN DO:
  ASSIGN cFileName = SESSION:TEMP-DIR + JBoxFileExportHeader.cSourceDBtable + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + STRING(TIME) + ".exp"
         cDelimiter = JBoxFileExportHeader.cDelimiter.
  
  CREATE QUERY hExpQuery.
  hExpQuery:SET-BUFFERS(httBuffer).
  hExpQuery:QUERY-PREPARE("FOR EACH " + httBuffer:NAME).
  hExpQuery:QUERY-OPEN().

  OUTPUT STREAM sExport TO VALUE(cFileName).
  IF JBoxFileExportHeader.bHeaderLines THEN DO:
    DO ix = 1 TO httBuffer:NUM-FIELDS:
      IF ix = httBuffer:NUM-FIELDS THEN
        cTmpDelim = "".
      ELSE 
        cTmpDelim = cDelimiter.
      PUT STREAM sExport UNFORMATTED httBuffer:BUFFER-FIELD(ix):NAME cTmpDelim.
    END.
    PUT STREAM sExport SKIP.
  END.
  
  hExpQuery:GET-FIRST().
  REPEAT WHILE NOT hExpQuery:QUERY-OFF-END:
    DO ix = 1 TO httBuffer:NUM-FIELDS:
      IF ix = httBuffer:NUM-FIELDS THEN
        cTmpDelim = "".
      ELSE 
        cTmpDelim = cDelimiter.
      PUT STREAM sExport UNFORMATTED httBuffer:BUFFER-FIELD(ix):BUFFER-VALUE cTmpDelim. 
    END.
    hExpQuery:GET-NEXT().
  END.
  DELETE OBJECT hExpQuery.
  OUTPUT STREAM sExport CLOSE.

  RUN jbdoc_loggserverdoc.p (cFileName,
                             "JBoxFileExportLog",
                             STRING(JBoxFileExportLog.iJBoxFileExportLogId),
                             icSessionId,
                             OUTPUT ocReturn,
                             OUTPUT bOk).
  OS-DELETE VALUE(cFileName).
END.

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hFileSpecBuffer.
DELETE OBJECT hHeaderBuffer.
DELETE OBJECT ohTempTable.

IF ocReturn = "" THEN
  ocReturn = "logrowid|" + STRING(ROWID(JBoxFileExportLog)).
