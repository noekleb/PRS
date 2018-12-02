/* Delete a document and all references (also a corresponding thumbnail doc)
   Parameters:  DocumentId 
         
   Created:  24.05.05 by Brynjar Hasle  
   Modified: 13.02.15 by Brynjar
             Delete meta data               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT   NO-UNDO.
DEF VAR hbJboxDocMeta AS HANDLE NO-UNDO.

DEF BUFFER bDocument FOR JBoxDocument.

CREATE BUFFER hbJBoxDocMeta FOR TABLE "JBoxDocMeta" NO-ERROR.

FIND FIRST JBoxDocument
     WHERE JBoxDocument.iJBoxDocumentId = INT(icParam)
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL JBoxDocument THEN DO:
  IF JBoxDocument.cFileName BEGINS "small_" THEN
    FIND FIRST bDocument
         WHERE bDocument.cFileName  = LEFT-TRIM(JBoxDocument.cFileName,"small_")
           AND bDocument.dCreated   = JBoxDocument.dCreated
           AND bDocument.cCreatedBy = JBoxDocument.cCreatedBy
           AND bDocument.iJBoxDocumentId GE JBoxDocument.iJBoxDocumentId - 10
           AND bDocument.iJBoxDocumentId LE JBoxDocument.iJBoxDocumentId + 10
         EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL bDocument THEN DO:
    FIND FIRST bDocument
         WHERE bDocument.cFileName  = "small_" + JBoxDocument.cFileName
           AND bDocument.dCreated   = JBoxDocument.dCreated
           AND bDocument.cCreatedBy = JBoxDocument.cCreatedBy
           AND bDocument.iJBoxDocumentId GE JBoxDocument.iJBoxDocumentId - 10
           AND bDocument.iJBoxDocumentId LE JBoxDocument.iJBoxDocumentId + 10
         EXCLUSIVE-LOCK NO-ERROR.
  END.
  IF AVAIL bDocument THEN DO:
    FOR EACH JBoxDocRel EXCLUSIVE-LOCK
        WHERE JBoxDocRel.iJBoxDocumentId = bDocument.iJBoxDocumentId:
      DELETE JBoxDocRel.
    END.
    RUN jbdoc_del_docrev.p (STRING(bDocument.iJBoxDocumentId)).
    RUN jbdoc_del_doclink.p (STRING(bDocument.iJBoxDocumentId)).
    IF VALID-HANDLE(hbJboxDocMeta) THEN DO:
      hbJboxDocMeta:FIND-FIRST("WHERE iJBoxDocumentId = " + STRING(bDocument.iJBoxDocumentId),EXCLUSIVE-LOCK) NO-ERROR.
      IF hbJboxDocMeta:AVAIL THEN
        hbJboxDocMeta:BUFFER-DELETE().
    END.
    DELETE bDocument.
  END.
  FOR EACH JBoxDocRel EXCLUSIVE-LOCK
      WHERE JBoxDocRel.iJBoxDocumentId = JBoxDocument.iJBoxDocumentId:
    DELETE JBoxDocRel.
  END.
  IF AVAIL JBoxDocument THEN DO:
    RUN jbdoc_del_docrev.p (STRING(JBoxDocument.iJBoxDocumentId)).
    RUN jbdoc_del_doclink.p (STRING(JBoxDocument.iJBoxDocumentId)).
    IF VALID-HANDLE(hbJboxDocMeta) THEN DO:
      hbJboxDocMeta:FIND-FIRST("WHERE iJBoxDocumentId = " + STRING(JBoxDocument.iJBoxDocumentId),EXCLUSIVE-LOCK) NO-ERROR.
      IF hbJboxDocMeta:AVAIL THEN
        hbJboxDocMeta:BUFFER-DELETE().
    END.
    DELETE JBoxDocument.  
  END. 
END.

IF ocReturn = "" THEN obOk = TRUE.

