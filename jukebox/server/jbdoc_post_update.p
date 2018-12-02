/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap | hBrowse,"PostUpdateProc","<procedure>").
   If there's no fieldmap (viewer) set the attribute on the browse object
   
   NOTE: If the action is DELETE the buffer handle is for a temp-table copy of the record that was deleted
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Delete, Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR iCompId    AS INT NO-UNDO.

iCompId = DYNAMIC-FUNCTION("getCompanyId" IN SOURCE-PROCEDURE).

IF icAction NE "delete" THEN DO:
  FIND FIRST JBoxDocMeta EXCLUSIVE-LOCK
       WHERE JBoxDocMeta.iJBoxDocumentId = ihBuffer::iJBoxDocumentId
       NO-ERROR.
  IF NOT AVAIL JBoxDocMeta THEN DO:
    CREATE JBoxDocMeta.
    JBoxDocMeta.iJBoxDocumentId = ihBuffer::iJBoxDocumentId.
  END.
  IF JBoxDocMeta.cKeyword NE ihBuffer::cDescription 
     AND (ihBuffer::cDescription NE "" OR (ihBuffer::cDescription = "" AND icAction = "update")) THEN
    ASSIGN JBoxDocMeta.dtKeywordModified  = NOW
           JBoxDocMeta.cKeywordModifiedBy = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE).

  ASSIGN JBoxDocMeta.cKeyword       = ihBuffer::cDescription
         JBoxDocMeta.cFileName      = ihBuffer::cFileName
         JBoxDocMeta.cFileType      = ihBuffer::cFileType
         JBoxDocMeta.dtFileCreated  = DATETIME(STRING(ihBuffer::dFileCreateDate) + " " + STRING(ihBuffer::iFileCreateTime,"hh:mm:ss"))
         JBoxDocMeta.dtFileModified = DATETIME(STRING(ihBuffer::dFileModDate) + " " + STRING(ihBuffer::iFileModTime,"hh:mm:ss"))
         JBoxDocMeta.iJBoxCompanyId = iCompId
         .
END.
ELSE DO:
  FIND FIRST JBoxDocMeta EXCLUSIVE-LOCK
       WHERE JBoxDocMeta.iJBoxDocumentId = ihBuffer::iJBoxDocumentId
       NO-ERROR.
  IF AVAIL JBoxDocMeta THEN DELETE JBoxDocMeta.
END.
