/* Delete document relation and corr.document
   Parameters:  iJBoxDocRelId
         
   Created: 16.06.05 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

FIND FIRST JBoxDocRel EXCLUSIVE-LOCK
     WHERE JBoxDocRel.iJBoxDocRelId = INT(icParam)
     NO-ERROR.
IF AVAIL JBoxDocRel THEN DO:
  FIND FIRST JBoxDocument OF JBoxDocRel EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL JBoxDocument THEN DO:
    RUN jbdoc_del_doclink.p (STRING(JBoxDocument.iJBoxDocumentId)).
    RUN jbdoc_del_docrev.p (STRING(JBoxDocument.iJBoxDocumentId)).
    DELETE JBoxDocument.    
    DELETE JBoxDocRel.
  END.
  ELSE ocReturn = "Document not available (for delete)".
END.
ELSE ocReturn = "Document relation not available (for delete)".

IF ocReturn = "" THEN obOk = TRUE.

