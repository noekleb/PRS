/* Delete references to a document and the document itself if noone else references it
   Parameters:  Context|ContextId 
         
   Created:  19.05.05 by Brynjar Hasle     
   Modified: 13.02.15 by Brynjar
             Deletes doc.meta also             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hbJboxDocMeta AS HANDLE NO-UNDO.

DEF BUFFER bDocRel FOR JBoxDocRel.

CREATE BUFFER hbJBoxDocMeta FOR TABLE "JBoxDocMeta" NO-ERROR.

DO TRANSACTION:
  FOR EACH JBoxDocRel EXCLUSIVE-LOCK
      WHERE JBoxDocRel.cContext   = ENTRY(1,icParam,"|")
        AND JBoxDocRel.cEntityId  = ENTRY(2,icParam,"|")
      :
    IF NOT CAN-FIND(FIRST bDocRel
                    WHERE bDocRel.iJBoxDocumentId = JBoxDocRel.iJBoxDocumentId
                      AND bDocRel.cContext        NE ENTRY(1,icParam,"|")) THEN DO:
      FIND JBoxDocument
           WHERE JBoxDocument.iJBoxDocumentId = JBoxDocRel.iJBoxDocumentId
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL JBoxDocument THEN DO:
        DELETE JBoxDocument.
        IF VALID-HANDLE(hbJboxDocMeta) THEN DO:
          hbJboxDocMeta:FIND-FIRST("WHERE iJBoxDocumentId = " + STRING(JBoxDocRel.iJBoxDocumentId),EXCLUSIVE-LOCK) NO-ERROR.
          IF hbJboxDocMeta:AVAIL THEN
            hbJboxDocMeta:BUFFER-DELETE().
        END.
      END. 
      ELSE DO:
        ocReturn = "Failed to delete document(s)".
        UNDO,LEAVE.
      END.
    END.
    DELETE JBoxDocRel.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

