/* Transfer a document blob to a temp-table for database import
   Caller: jbdoc_savedoc.p 
   
   Created: 15.02.05  By BHa
   Modifed: 22.12.09  By Bha:
                      Imported data are always copied to temp-table buffer to be able to give more control to the 
                      custom import routine. 
                      Import-routine will be persistent if it containse the procedure ProcessImportedRow
---------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer         AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId      AS CHAR   NO-UNDO.
DEF INPUT  PARAM icImportHeaderId AS CHAR   NO-UNDO.
DEF INPUT  PARAM icParam          AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obLoad           AS LOG    NO-UNDO. /* INIT TRUE. */
DEF OUTPUT PARAM ocTargetDBtable  AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocFirstKeyString AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocLastKeyString  AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn         AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK             AS LOG    NO-UNDO.

DEF VAR mpDocument      AS MEMPTR NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR iByteCount      AS INT    NO-UNDO.
DEF VAR cLine           AS CHAR   NO-UNDO.
DEF VAR cChar           AS CHAR   NO-UNDO.
                        
DEF VAR iLineCount          AS INT    NO-UNDO.
DEF VAR iRecordCount        AS INT    NO-UNDO.
DEF VAR fTotal              AS DEC    NO-UNDO.
DEF VAR httImport           AS HANDLE NO-UNDO.
DEF VAR hbImport            AS HANDLE NO-UNDO.
DEF VAR hqImport            AS HANDLE NO-UNDO.
DEF VAR httTarget           AS HANDLE NO-UNDO.
DEF VAR hbTtTarget          AS HANDLE NO-UNDO.
DEF VAR hbDbTarget          AS HANDLE NO-UNDO.
DEF VAR hbSourceFields      AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hbTargetFields      AS HANDLE NO-UNDO EXTENT 100.
DEF VAR cInpFldIndexLst     AS CHAR   NO-UNDO.
DEF VAR cBuffCpyFldLst      AS CHAR   NO-UNDO.
DEF VAR iNumImpFields       AS INT    NO-UNDO.
DEF VAR bTest               AS LOG    NO-UNDO.
DEF VAR iTestCount          AS INT    NO-UNDO.
DEF VAR cBufferCopyImpFlds  AS CHAR   NO-UNDO.
DEF VAR cBufferCopyTtDbFlds AS CHAR   NO-UNDO.
DEF VAR cBufferCopyExclude  AS CHAR   NO-UNDO.
DEF VAR cDelimiter          AS CHAR   NO-UNDO.
DEF VAR hCompIdFld          AS HANDLE NO-UNDO.
DEF VAR cJBCompanyId        AS CHAR   NO-UNDO.
DEF VAR iJBCompanyId        AS INT    NO-UNDO.
DEF VAR rLastOkRow          AS ROWID  NO-UNDO.
DEF VAR httTest             AS HANDLE NO-UNDO.
DEF VAR cTransContext       AS CHAR   NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR cFileLabelList      AS CHAR   NO-UNDO.
DEF VAR hImportProg         AS HANDLE NO-UNDO.
DEF VAR bDeleteTarget       AS LOG    NO-UNDO.
DEF VAR bImportLogFile      AS LOG    NO-UNDO.
DEF VAR hFldCreatedDate     AS HANDLE NO-UNDO.
DEF VAR hFldCreatedBy       AS HANDLE NO-UNDO.
DEF VAR cUserId             AS CHAR   NO-UNDO.
DEF VAR hJbAPI              AS HANDLE NO-UNDO.
DEF VAR hParamfld           AS HANDLE NO-UNDO.
DEF VAR bParamFldError      AS LOG    NO-UNDO.

DEF TEMP-TABLE ttFlagForLabelSet
    FIELD cLogName AS CHAR.

hParent = SOURCE-PROCEDURE.
          
cUserId = DYNAMIC-FUNCTION("getASuserId" IN hParent).

FUNCTION setContext RETURNS LOGICAL (INPUT icTransContext AS CHAR):
  cTransContext = icTransContext.
END FUNCTION.

FUNCTION getContext RETURNS CHARACTER ( ):    
  RETURN cTransContext.
END FUNCTION.

FUNCTION getFileDelimiter RETURNS CHARACTER ( ):
  IF AVAIL JBoxFileImportHeader THEN RETURN JBoxFileImportHeader.cDelimiter.
  ELSE RETURN ";".
END.

FUNCTION getCurrentFileLineNum RETURNS INTEGER ( ):
  IF AVAIL JBoxFileImportHeader THEN RETURN JBoxFileImportHeader.iHeaderLines + iRecordCount.
  ELSE RETURN iRecordCount.
END.

FUNCTION startASlib RETURNS LOGICAL ():
  IF NOT VALID-HANDLE(hJbAPI) THEN
    RUN jbserv_api_for_server.p PERSIST SET hJbAPI (icSessionId).
  SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(hJbAPI).
END.

FUNCTION getASlibHandle RETURNS HANDLE ():
  RETURN hJbAPI.
END FUNCTION.

FUNCTION CreImportDataLog RETURNS LOGICAL (INPUT icLogType AS CHAR,INPUT icLogName AS CHAR,INPUT icLogDesc AS CHAR,icLogText AS CHAR):   
  DEF VAR cSourceData AS CHAR NO-UNDO.
  DEF VAR ix          AS INT  NO-UNDO.

  IF NOT AVAIL JBoxFileImportHeader THEN RETURN NO.

  bImportLogFile = YES.

  IF icLogType = "sourcedata" THEN DO:
    IF NOT CAN-FIND(FIRST ttFlagForLabelSet WHERE ttFlagForLabelSet.cLogName = icLogName) THEN DO:
      DYNAMIC-FUNCTION("CreImportDataLog" IN hParent,icLogType,icLogName,icLogDesc,1,cFileLabelList).
      CREATE ttFlagForLabelSet.
      ttFlagForLabelSet.cLogName = icLogName.
      DO ix = 2 TO JBoxFileImportHeader.iHeaderLines:
        DYNAMIC-FUNCTION("CreImportDataLog" IN hParent,icLogType,icLogName,icLogDesc,ix,"").
      END.
    END.
    DO ix = 1 TO NUM-ENTRIES(cFileLabelList,";"): /* hbImport:NUM-FIELDS: */
      cSourceData = cSourceData + (IF cSourceData NE "" THEN JBoxFileImportHeader.cDelimiter ELSE "") + STRING(hbImport:BUFFER-FIELD(ENTRY(ix,cFileLabelList,";")):BUFFER-VALUE).
    END.
    IF icLogText NE "" THEN
      cSourceData = cSourceData + JBoxFileImportHeader.cDelimiter + icLogText.

    DYNAMIC-FUNCTION("CreImportDataLog" IN hParent,icLogType,icLogName,icLogDesc,
                     iRecordCount + JBoxFileImportHeader.iHeaderLines,
                     cSourceData).
  END.
  ELSE  
    DYNAMIC-FUNCTION("CreImportDataLog" IN hParent,icLogType,icLogName,icLogDesc,
                     IF NOT PROGRAM-NAME(2) MATCHES "*CleanUp*" THEN iRecordCount + JBoxFileImportHeader.iHeaderLines ELSE 0,
                     icLogText).
END FUNCTION.

DO ix = 1 TO NUM-ENTRIES(icParam,"|"):
  IF ENTRY(1,ENTRY(ix,icParam,"|"),"|") = "test" THEN DO:
    bTest = TRUE.
    IF NUM-ENTRIES(ENTRY(ix,icParam,"|"),"¤") > 1 THEN
      iTestCount = INT(ENTRY(2,ENTRY(ix,icParam,"|"),"¤")) NO-ERROR.
  END.
END.

FUNCTION ProcessLine RETURNS CHARACTER
  (INPUT icLine AS CHAR) FORWARD.

FUNCTION GetPrimaryKeyValues RETURNS CHARACTER
  (INPUT ihBuffer AS HANDLE) FORWARD.

FIND FIRST JBoxFileImportHeader
     WHERE JBoxFileImportHeader.iJBoxFileImportHeaderId = INT(icImportHeaderId)
     NO-LOCK NO-ERROR.
IF NOT AVAIL JBoxFileImportHeader THEN DO:
  ocReturn = "Couldn't find file import description " + icImportHeaderId.
  RETURN.
END.

ASSIGN obLoad          = JBoxFileImportHeader.bLogFilesInDB
       ocTargetDBtable = JBoxFileImportHeader.cTargetDBtable
       cDelimiter      = JBoxFileImportHeader.cDelimiter
       .

IF cDelimiter = "" THEN DO:
  ocReturn = "File delimiter not specified".
  RETURN.
END.

IF JBoxFileImportHeader.cImportProg NE "" 
   AND SEARCH(JBoxFileImportHeader.cImportProg) = ?
   AND SEARCH(SUBSTR(JBoxFileImportHeader.cImportProg,1,LENGTH(JBoxFileImportHeader.cImportProg) - 1) + "r") = ? THEN DO:
  ocReturn = "Couldn't find import specified import program " + JBoxFileImportHeader.cImportProg.
  RETURN.
END.


IF JBoxFileImportHeader.cTargetDBtable NE "" THEN DO:
  CREATE TEMP-TABLE httTarget.
/*   httTarget:CREATE-LIKE(JBoxFileImportHeader.cTargetDBtable) NO-ERROR. */
  IF ERROR-STATUS:ERROR THEN DO:
    ocReturn = "Invalid table reference for import " + JBoxFileImportHeader.cTargetDBtable.
    RETURN.
  END.

  CREATE BUFFER hbDbTarget FOR TABLE JBoxFileImportHeader.cTargetDBtable NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    ocReturn = "Invalid table reference for import " + JBoxFileImportHeader.cTargetDBtable.
    DELETE OBJECT httTarget NO-ERROR.
    DELETE OBJECT hbTtTarget NO-ERROR.
    RETURN.
  END.

  httTarget:ADD-FIELDS-FROM(hbDbTarget).
  httTarget:TEMP-TABLE-PREPARE("tt" + JBoxFileImportHeader.cTargetDBtable).
  CREATE BUFFER hbTtTarget FOR TABLE httTarget.
END.

/* Create the temp-table definition and, if defined source table, obtain field handles for corresponding db table fields */
CREATE TEMP-TABLE httImport.
FOR EACH JBoxFileImportSpec NO-LOCK 
    OF JBoxFileImportHeader
    BY JBoxFileImportSpec.iSeq:

  ASSIGN iNumImpFields = iNumImpFields + 1 
         cFileLabelList = cFileLabelList + (IF cFileLabelList NE "" THEN JBoxFileImportHeader.cDelimiter ELSE "") + JBoxFileImportSpec.cFieldName.

  httImport:ADD-NEW-FIELD(JBoxFileImportSpec.cFieldName,JBoxFileImportSpec.cDataType).
  IF VALID-HANDLE(hbTtTarget) AND JBoxFileImportSpec.cDBfieldName NE "" THEN DO:
    hbTargetFields[iNumImpFields] = hbTtTarget:BUFFER-FIELD(JBoxFileImportSpec.cDBfieldName) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      ocReturn = "Invalid target field reference for import: " 
                + JBoxFileImportSpec.cDBfieldName.
    ELSE 
      cBuffCpyFldLst = cBuffCpyFldLst + (IF cBuffCpyFldLst NE "" THEN "," ELSE "") + STRING(iNumImpFields).
  END.
  cInpFldIndexLst = cInpFldIndexLst + (IF cInpFldIndexLst NE "" THEN "," ELSE "") + STRING(JBoxFileImportSpec.iSeq).
END.

httImport:TEMP-TABLE-PREPARE("ttImport").
hbImport = httImport:DEFAULT-BUFFER-HANDLE.

/* After the temp-table is created (prepared), retrieve corresponding source field handles to db-field handles: */
IF VALID-HANDLE(hbTtTarget) THEN DO:
  ix = 0.
  FOR EACH JBoxFileImportSpec NO-LOCK 
      OF JBoxFileImportHeader
      BY JBoxFileImportSpec.iSeq:
  
    ix = ix + 1.
    IF JBoxFileImportSpec.cDBfieldName NE "" THEN DO:
      hbSourceFields[ix] = hbImport:BUFFER-FIELD(JBoxFileImportSpec.cFieldName) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        ocReturn = "Invalid source field reference for import: " 
                  + JBoxFileImportSpec.cFieldName.
    END.
  END.
END.

IF ocReturn = "" THEN DO:

  /* Import the file to temp-table: */
  CREATE QUERY hqImport.
  hqImport:SET-BUFFERS(hbImport).
    
  SET-SIZE(mpDocument) = ihBuffer:BUFFER-FIELD('iDocSize'):BUFFER-VALUE.
  COPY-LOB FROM OBJECT ihBuffer:BUFFER-FIELD('blDocument'):BUFFER-VALUE TO OBJECT mpDocument.
  
  REPEAT:  
    iByteCount = iByteCount + 1.
    cChar = GET-STRING(mpDocument,iByteCount,1).
    IF cChar = CHR(10) THEN DO:
      IF LENGTH(cLine) > 1 THEN DO:
        iLineCount = iLineCount + 1.
/*         IF iLineCount < 5 THEN                            */
/*           MESSAGE "cLine" cLine SKIP                      */
/*                   "cInpFldIndexLst " cInpFldIndexLst SKIP */
/*                   "iNumImpFields " iNumImpFields          */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.          */
        IF iLineCount > JBoxFileImportHeader.iHeaderLines THEN DO:
          IF iLineCount = JBoxFileImportHeader.iHeaderLines + 1 THEN DO:
            IF NUM-ENTRIES(cLine,cDelimiter) < ix THEN DO:
              ocReturn = "Num input columns when split by " + cDelimiter + " doesn't match column specification:" + CHR(10) + cLine.
              LEAVE.
            END.  
          END.  
          ocReturn = ProcessLine(cLine).
        END.  
        IF ocReturn NE "" THEN LEAVE.
      END.
      cLine = "".
    END.
    ELSE cLine = cLine + cChar.
    
    IF (bTEST AND iTestCount NE 0 AND iLineCount > iTestCount) OR iByteCount GE GET-SIZE(mpDocument) THEN LEAVE.
  END.

  /* Process the temp-table. Assign fields according to file-spec and/or run an assign function */
  IF ocReturn = "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cBuffCpyFldLst):
      cBufferCopyImpFlds = cBufferCopyImpFlds + 
                           hbTargetFields[INT(ENTRY(ix,cBuffCpyFldLst))]:NAME + "," +
                           hbSourceFields[INT(ENTRY(ix,cBuffCpyFldLst))]:NAME + "," NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        ocReturn = "Error in field assign definition" + CHR(10)
                 + "Index: " + STRING(ix) + ". Source field index: " + ENTRY(ix,cBuffCpyFldLst) + ". Field number list: " + cBuffCpyFldLst + CHR(10)
                 + ERROR-STATUS:GET-MESSAGE(1) + CHR(10)
                 + "VALID-HANDLE(hbSourceFields[INT(ENTRY(ix,cBuffCpyFldLst))]): " + STRING(VALID-HANDLE(hbSourceFields[INT(ENTRY(ix,cBuffCpyFldLst))])) + CHR(10)
                 + "VALID-HANDLE(hbTargetFields[INT(ENTRY(ix,cBuffCpyFldLst))]): " + STRING(VALID-HANDLE(hbTargetFields[INT(ENTRY(ix,cBuffCpyFldLst))]))
                 .
        LEAVE.
      END.
      cBufferCopyTtDbFlds = cBufferCopyTtDbFlds +
                            hbTargetFields[INT(ENTRY(ix,cBuffCpyFldLst))]:NAME + "," +
                            hbTargetFields[INT(ENTRY(ix,cBuffCpyFldLst))]:NAME + ",".
    END.

    IF ocReturn = "" THEN DO:
      ASSIGN cBufferCopyImpFlds  = TRIM(cBufferCopyImpFlds,",")
             cBufferCopyTtDbFlds = TRIM(cBufferCopyTtDbFlds,",").
  
      IF VALID-HANDLE(hbDbTarget) THEN DO:
        hCompIdFld = hbDbTarget:BUFFER-FIELD("iJBoxCompanyId") NO-ERROR.
        IF VALID-HANDLE(hCompIdFld) THEN DO:
          RUN jbserv_getcompanyid.p (icSessionId,OUTPUT cJBcompanyId).
          IF cJBcompanyId NE "" THEN
            iJBcompanyId = INT(cJBcompanyId).
        END.
        hFldCreatedBy    = hbDbTarget:BUFFER-FIELD("cCreatedBy") NO-ERROR.
        hFldCreatedDate  = hbDbTarget:BUFFER-FIELD("dCreated") NO-ERROR.

        DO ix = 1 TO hbDbTarget:NUM-FIELDS:
          IF NOT CAN-DO(cBufferCopyTtDbFlds,hbDbTarget:BUFFER-FIELD(ix):NAME) THEN
            cBufferCopyExclude = cBufferCopyExclude + (IF cBufferCopyExclude NE "" THEN "," ELSE "") + hbDbTarget:BUFFER-FIELD(ix):NAME.
        END.
      END.
  
      /* Check if the custom import routine should be persistent while importing rows.
         To be stay persistent the import routine must contain the procedure ProcessImportRow */
      IF JBoxFileImportHeader.cImportProg NE "" THEN DO:
        RUN VALUE(JBoxFileImportHeader.cImportProg) PERSIST SET hImportProg (icParam,hbImport,hbTtTarget,icSessionId,OUTPUT ocReturn,OUTPUT obOK).
        IF NOT CAN-DO(hImportProg:INTERNAL-ENTRIES,"ProcessImportedRow") THEN 
          DELETE PROCEDURE hImportProg.
      END.
      ELSE IF NUM-ENTRIES(icParam,";") > 1 AND VALID-HANDLE(hbDbTarget) THEN DO:
        hParamfld = hbDbTarget:BUFFER-FIELD(ENTRY(1,icParam,";")) NO-ERROR.
        IF VALID-HANDLE(hParamFld) THEN icParam = ENTRY(2,icParam,";").
        ELSE ocReturn = "Invalid spesification of parameter field: " + ENTRY(1,icParam,";") + ". Field does not exist in table " + hbDbTarget:NAME.
      END.
  
      IF ocReturn = "" THEN DO:
        hqImport:QUERY-PREPARE("FOR EACH " + hbImport:NAME).
        hqImport:QUERY-OPEN().
        REPEAT:
          hqImport:GET-NEXT().
          ASSIGN iRecordCount  = iRecordCount + 1
                 bDeleteTarget = NO
                 .
          IF hqImport:QUERY-OFF-END THEN LEAVE.
    
          IF VALID-HANDLE(hbTtTarget) THEN DO:

            IF cBufferCopyImpFlds NE "" THEN DO:
              hbTtTarget:BUFFER-COPY(hbImport,?,cBufferCopyImpFlds) NO-ERROR.
              IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
                IF NOT (ERROR-STATUS:GET-NUMBER(1) = 132 AND JBoxFileImportHeader.bIgnoreDuplicates) THEN DO:
                  ASSIGN ocReturn = "Error in record creation at line " + STRING(iRecordCount + JBoxFileImportHeader.iHeaderLines) + CHR(10)
                                  + ERROR-STATUS:GET-MESSAGE(1)
                        obOK      = NO.
                  LEAVE.
                END.
                ELSE bDeleteTarget = YES. 
              END.
            END.
             
            IF JBoxFileImportHeader.cImportProg NE "" THEN DO:
              IF VALID-HANDLE(hImportProg) THEN
                RUN ProcessImportedRow IN hImportProg (icParam,hbImport,hbTtTarget,icSessionId,OUTPUT ocReturn,OUTPUT obOK).
              ELSE
                RUN VALUE(JBoxFileImportHeader.cImportProg) (icParam,hbImport,hbTtTarget,icSessionId,OUTPUT ocReturn,OUTPUT obOK).
      
              IF NOT CAN-DO("skip,skiprow",ocReturn) THEN DO: /* skip(row): The import-program handled database update */
                IF NOT obOk THEN DO:
                  ocReturn = (IF bTest THEN "TEST: " ELSE "") +
                             "Error for line " + STRING(iRecordCount + JBoxFileImportHeader.iHeaderLines) + CHR(10) + ocReturn.
                  IF NOT bImportLogFile OR NOT bTest THEN DO:
                    obOK = NO.
                    LEAVE.
                  END.
                END.
              END.
              ELSE IF hbTtTarget:AVAIL THEN hbTtTarget:BUFFER-DELETE().
            END.
            ELSE IF bDeleteTarget AND hbTtTarget:AVAIL THEN hbTtTarget:BUFFER-DELETE().

            IF CAN-DO("skip,skiprow",ocReturn) THEN ocReturn = "".

            IF hbTtTarget:AVAIL AND cBufferCopyTtDbFlds NE "" AND NOT bTest THEN DO:

              hbDbTarget:BUFFER-CREATE() NO-ERROR.
              IF NOT (ERROR-STATUS:ERROR AND ERROR-STATUS:GET-NUMBER(1) = 132) THEN DO:
                IF cBufferCopyTtDbFlds NE "" THEN DO:
                  hbDbTarget:BUFFER-COPY(hbTtTarget,cBufferCopyExclude,cBufferCopyTtDbFlds) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN DO:
                    IF NOT (ERROR-STATUS:GET-NUMBER(1) = 132 AND JBoxFileImportHeader.bIgnoreDuplicates) THEN DO:
                      ocReturn = "Error in record creation at line " + STRING(iRecordCount + JBoxFileImportHeader.iHeaderLines) + CHR(10)
                               + ERROR-STATUS:GET-MESSAGE(1).
                      obOK = NO.
                      LEAVE.
                    END.
                    ELSE hbTtTarget:BUFFER-DELETE().
                  END.
                END.
                IF hbTtTarget:AVAIL AND hbDbTarget:AVAIL THEN DO:
                  rLastOkRow = hbDbTarget:ROWID.
            
                  IF iJBcompanyId NE 0 THEN
                    hCompIdFld:BUFFER-VALUE = iJBcompanyId.
                  IF VALID-HANDLE(hFldCreatedDate) THEN hFldCreatedDate:BUFFER-VALUE = TODAY.
                  IF VALID-HANDLE(hFldCreatedBy) THEN hFldCreatedBy:BUFFER-VALUE = cUserId.
                  IF VALID-HANDLE(hParamfld) THEN RUN jbserv_assignstringvalue.p (hParamfld,0,icParam,OUTPUT bParamFldError).
                  IF bParamFldError THEN ocReturn = "Error when assigning parameter value " + icParam.
  
                  IF NOT bTest AND ocFirstKeyString = "" THEN 
                    ocFirstKeyString = GetPrimaryKeyValues(hbDbTarget).
        
                  IF NOT bTest AND iRecordCount = iLineCount - JBoxFileImportHeader.iHeaderLines THEN DO:
                    IF hbDbTarget:AVAIL THEN
                      ocLastKeyString = GetPrimaryKeyValues(hbDbTarget).
                    ELSE IF rLastOkRow NE ? THEN DO:
                      IF hbDbTarget:FIND-BY-ROWID(rLastOkRow,NO-LOCK) THEN
                        ocLastKeyString = GetPrimaryKeyValues(hbDbTarget).
                    END.
                    ELSE ocReturn = "No new rows imported".
                  END.
                END.
              END.
            END.
          END.
          ELSE IF JBoxFileImportHeader.cImportProg NE "" THEN DO:
            /* No target table is defined. The import program must handle everything: */
            IF VALID-HANDLE(hImportProg) THEN
              RUN ProcessImportedRow IN hImportProg (icParam,hbImport,?,icSessionId,OUTPUT ocReturn,OUTPUT obOK).
            ELSE
              RUN VALUE(JBoxFileImportHeader.cImportProg) (icParam,hbImport,?,icSessionId,OUTPUT ocReturn,OUTPUT obOK).
          END.
        END.
      END.
    END.
  END.  
END.

IF VALID-HANDLE(hImportProg) THEN DO:
  RUN CleanUp IN hImportProg NO-ERROR.
  DELETE PROCEDURE hImportProg NO-ERROR.
END.

IF VALID-HANDLE(hJbAPI) THEN DO:
  DYNAMIC-FUNCTION("setPreTransBuffer" IN hJbAPI,?).  
  DELETE PROCEDURE hJbAPI.  
END. 


DELETE OBJECT hbTtTarget  NO-ERROR.
DELETE OBJECT hbDbTarget  NO-ERROR.
DELETE OBJECT hqImport    NO-ERROR.
DELETE OBJECT httImport   NO-ERROR.
DELETE OBJECT httTarget   NO-ERROR.

IF ocReturn = "" OR ocReturn = "No new rows imported" THEN obOk = TRUE.

FUNCTION ProcessLine RETURNS CHARACTER
  (INPUT icLine AS CHAR):

  DEF VAR idx      AS INT  NO-UNDO.
  DEF VAR iFldIdx  AS INT  NO-UNDO.
  DEF VAR cValue   AS CHAR NO-UNDO.
  DEF VAR bContent AS LOG  NO-UNDO.

  DO idx = 1 TO NUM-ENTRIES(icLine,cDelimiter):
    IF ENTRY(idx,icLine,cDelimiter) NE "" THEN DO:
      bContent = YES.
      LEAVE.
    END.
  END.
  
  IF NOT bContent THEN RETURN "".

  hbImport:BUFFER-CREATE() NO-ERROR.

  DO idx = 1 TO NUM-ENTRIES(icLine,cDelimiter):

    IF CAN-DO(cInpFldIndexLst,STRING(idx)) THEN DO:
      ASSIGN iFldIdx = iFldIdx + 1
             cValue  = TRIM(ENTRY(idx,icLine,cDelimiter)).
      IF hbImport:BUFFER-FIELD(iFldIdx):DATA-TYPE NE "CHARACTER" THEN DO: 
        REPEAT WHILE INDEX(cValue," ") > 0:
          cValue = REPLACE(cValue," ","").
        END.
        IF hbImport:BUFFER-FIELD(iFldIdx):DATA-TYPE = "DECIMAL" AND INDEX(cValue,".") > INDEX(cValue,",") THEN cValue = REPLACE(cValue,",","").
      END.  
      ELSE
        ASSIGN cValue = REPLACE(cValue,CHR(155),"ø")
               cValue = REPLACE(cValue,CHR(155),"ø")
               cValue = REPLACE(cValue,CHR(134),"å")
               cValue = REPLACE(cValue,CHR(134),"å")
               .        
         
      CASE hbImport:BUFFER-FIELD(iFldIdx):DATA-TYPE:
        WHEN "DATE"     THEN hbImport:BUFFER-FIELD(iFldIdx):BUFFER-VALUE = DATE(cValue) NO-ERROR.
        WHEN "DECIMAL"  THEN hbImport:BUFFER-FIELD(iFldIdx):BUFFER-VALUE = DECIMAL(REPLACE(cValue,".",",")) NO-ERROR.
        WHEN "INTEGER"  THEN hbImport:BUFFER-FIELD(iFldIdx):BUFFER-VALUE = INTEGER(cValue) NO-ERROR. 
        WHEN "LOGICAL"  THEN hbImport:BUFFER-FIELD(iFldIdx):BUFFER-VALUE = LOGICAL(cValue) NO-ERROR.
        OTHERWISE            hbImport:BUFFER-FIELD(iFldIdx):BUFFER-VALUE = cValue NO-ERROR.
      END CASE.
      IF ERROR-STATUS:ERROR THEN 
        RETURN "Error in file import at line " + STRING(iLineCount) + CHR(10) 
              + "File column#: " + STRING(idx) + ", Db field name: " +  hbImport:BUFFER-FIELD(iFldIdx):NAME + ", Type: " + hbImport:BUFFER-FIELD(iFldIdx):DATA-TYPE + CHR(10) 
              + "Value: " + cValue + CHR(10) + CHR(10)
              + ERROR-STATUS:GET-MESSAGE(1) + CHR(10) + CHR(10) 
              + icLine.
    END.
  END.

  RETURN "".

END FUNCTION.

FUNCTION GetPrimaryKeyValues RETURNS CHARACTER
  (INPUT ihKeyBuffer AS HANDLE):

DEF VAR cPKfields      AS CHAR NO-UNDO.
DEF VAR cKeyString     AS CHAR NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.

DO ix = 1 TO 100:
  IF ihKeyBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
    IF ENTRY(2,ihKeyBuffer:INDEX-INFORMATION(ix)) = "1" AND ENTRY(3,ihKeyBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
      DO iy = 5 TO NUM-ENTRIES(ihKeyBuffer:INDEX-INFORMATION(ix)) BY 2:
        cPKfields = cPKfields + ENTRY(iy,ihKeyBuffer:INDEX-INFORMATION(ix)) + ",".
      END.
      LEAVE.
    END.
  END.
  ELSE LEAVE.
END.  

DO ix = 1 TO NUM-ENTRIES(cPKfields) - 1:
 cKeyString = ihKeyBuffer:BUFFER-FIELD(ENTRY(ix,cPKfields)):NAME + "|" + STRING(ihKeyBuffer:BUFFER-FIELD(ENTRY(ix,cPKfields)):BUFFER-VALUE) + "|".
END.

IF cKeyString NE "" THEN
  RETURN SUBSTR(cKeyString,1,LENGTH(cKeyString) - 1).
ELSE RETURN "".

END FUNCTION.

FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  (INPUT ihBuffer AS HANDLE):

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

FUNCTION getSourceTableFieldHandle RETURNS HANDLE
  (INPUT icTargetFieldName AS CHAR):
  
  DEF VAR cSourceFieldName AS CHAR   NO-UNDO.
  DEF VAR hSourceField     AS HANDLE NO-UNDO.

  IF LOOKUP(icTargetFieldName,cBufferCopyImpFlds) > 0 THEN DO:
    cSourceFieldName = ENTRY(LOOKUP(icTargetFieldName,cBufferCopyImpFlds) + 1,cBufferCopyImpFlds) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hSourceField = hbImport:BUFFER-FIELD(cSourceFieldName) NO-ERROR.
  END.

  RETURN hSourceField.
END FUNCTION.
