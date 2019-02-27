/* Check if other entities references a document
   Parameters:  Context|ContextId  (entityid)
         
   Created:  02.01.11 by Brynjar Hasle 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obExist     AS LOG NO-UNDO.

DEF VAR icEntityId    AS CHAR NO-UNDO.
DEF VAR icContext     AS CHAR NO-UNDO.
DEF VAR cOtherRefList AS CHAR NO-UNDO.
DEF VAR iCount        AS INT  NO-UNDO.

DEF BUFFER bJBoxDocRel FOR JBoxDocRel.

IF NUM-ENTRIES(icParam,"|") LT 2 THEN DO:
  ocReturn = "Invalid parameter for jbdoc_other_entity_links.p".
  RETURN.
END.

ASSIGN icContext  = ENTRY(1,icParam,"|")
       icEntityId = ENTRY(2,icParam,"|")
       .


FIND FIRST JBoxDocRel NO-LOCK
     WHERE JBoxDocRel.cEntityId = icEntityId 
       AND JBoxDocRel.cContext  = icContext
     NO-ERROR.
IF AVAIL JBoxDocRel THEN
  FOR EACH bJBoxDocRel NO-LOCK
      WHERE bJBoxDocRel.iJBoxDocumentId = JBoxDocRel.iJBoxDocumentId
        AND (bJBoxDocRel.cEntityId      NE icEntityId
          OR bJBoxDocRel.cContext       NE icContext)
       BREAK BY bJBoxDocRel.cEntityId:
    iCount = iCount + 1.
    IF FIRST-OF(bJBoxDocRel.cEntityId) THEN DO:
      IF DYNAMIC-FUNCTION("runProc","jbserv_find_record_from_idvalue.p",
                          bJBoxDocRel.cContext + "¤" + bJBoxDocRel.cEntityId,?) THEN
        cOtherRefList = cOtherRefList + (IF cOtherRefList NE "" THEN CHR(10) ELSE "") + DYNAMIC-FUNCTION("getTransactionMessage") + " (Id:" + bJBoxDocRel.cEntityId + ")".
      ELSE
        cOtherRefList = cOtherRefList + (IF cOtherRefList NE "" THEN CHR(10) ELSE "") + bJBoxDocRel.cContext + " (Id:" + bJBoxDocRel.cEntityId + ")".
    END.
  END. 
IF cOtherRefList NE "" THEN DO:
  IF iCount > 1 THEN
    ocReturn = (IF DYNAMIC-FUNCTION("Scandinavian" IN SOURCE-PROCEDURE) THEN
                  "Dokumenter er knyttet til andre begreper i databasen:"
                ELSE
                  "The documents are referenced from other entities in the database:")
             + CHR(10) + cOtherRefList.
  ELSE
    ocReturn = (IF DYNAMIC-FUNCTION("Scandinavian" IN SOURCE-PROCEDURE) THEN
                  "Dokument er knyttet til et annet begrep i databasen:"
                ELSE
                  "The document is referenced from an other entity in the database:")
             + CHR(10) + cOtherRefList.
END.

obExist = ocReturn NE "".

