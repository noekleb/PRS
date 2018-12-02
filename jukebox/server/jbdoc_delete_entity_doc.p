/* Delete document relation and corr.document for an entity
   The document is only deleted if current entity is the only entity linked to the doc
   Parameters:  Context|ContextId  (entityid)
         
   Created:  16.06.05 by Brynjar Hasle                  
   Modified: 02.01.11 by Brynjar Hasle 
           - Allows "strong" or "weak" deletes: 
               "Strong":  Delete even if other entities reference the document
               "Weak":    If other entities reference the doc only delete this reference
               "Default" (or blank): Delete only when no other entities links to the document
           - Deletes document to document links 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR icEntityId    AS CHAR NO-UNDO.
DEF VAR icContext     AS CHAR NO-UNDO.
DEF VAR cDeleteMode   AS CHAR NO-UNDO.
DEF VAR cOtherRefList AS CHAR NO-UNDO.

DEF BUFFER bJBoxDocRel FOR JBoxDocRel.

IF NUM-ENTRIES(icParam,"|") LT 2 THEN DO:
  ocReturn = "Invalid parameter for jbdoc_delete_entity_doc.p".
  RETURN.
END.

ASSIGN icContext  = ENTRY(1,icParam,"|")
       icEntityId = ENTRY(2,icParam,"|")
       .
IF NUM-ENTRIES(icParam,"|") > 2 THEN
  cDeleteMode = ENTRY(3,icParam,"|").

DO TRANSACTION ON ERROR UNDO,LEAVE:
  FOR EACH JBoxDocRel EXCLUSIVE-LOCK
      WHERE JBoxDocRel.cEntityId = icEntityId 
        AND JBoxDocRel.cContext  = icContext:
  
    IF (cDeleteMode = "strong" OR 
        NOT CAN-FIND(FIRST bJBoxDocRel
                     WHERE bJBoxDocRel.iJBoxDocumentId = JBoxDocRel.iJBoxDocumentId
                       AND (bJBoxDocRel.cEntityId      NE icEntityId
                        OR bJBoxDocRel.cContext        NE icContext))) OR
       (cDeleteMode = "weak" AND 
        NOT CAN-FIND(FIRST bJBoxDocRel
                     WHERE bJBoxDocRel.iJBoxDocumentId = JBoxDocRel.iJBoxDocumentId
                       AND (bJBoxDocRel.cEntityId      NE icEntityId
                        OR bJBoxDocRel.cContext        NE icContext)))
       THEN DO:
      FIND FIRST JBoxDocument OF JBoxDocRel EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL JBoxDocument THEN DO:
        RUN jbdoc_delete_one_doc.p (STRING(JBoxDocRel.iJBoxDocumentId),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
        IF NOT obOK THEN UNDO,LEAVE.
      END.
      ELSE DO:
        ocReturn = "Document not available (for delete)".
        UNDO,LEAVE.
      END.
    END.
    ELSE IF cDeleteMode = "" OR cDeleteMode = "default" THEN DO:
      FOR EACH bJBoxDocRel NO-LOCK
          WHERE bJBoxDocRel.iJBoxDocumentId = JBoxDocRel.iJBoxDocumentId
            AND (bJBoxDocRel.cEntityId       NE icEntityId
              OR bJBoxDocRel.cContext        NE icContext)
           BREAK BY bJBoxDocRel.cEntityId:
        IF FIRST-OF(bJBoxDocRel.cEntityId) THEN
          cOtherRefList = cOtherRefList + (IF cOtherRefList NE "" THEN "," ELSE "") + bJBoxDocRel.cEntityId.
      END. 
      IF cOtherRefList NE "" THEN DO:
        ocReturn = "Document is referenced by other entities and cannot be deleted:" + CHR(10) + cOtherRefList.
        UNDO,LEAVE.
      END.
    END.
    DELETE JBoxDocRel NO-ERROR.
  END.
END.

obOk = ocReturn = "".

