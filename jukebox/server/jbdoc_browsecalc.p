/* In order to speed up processing of calculated fields it can be crucial
   to place the procedures in a single persistent procedure that is loaded
   once before processing the records.
   To invoke:
   
   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","orderline_browsecalc.p").
   
   Usage in browse (the name must not end on .p) :
   
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total|Total"
   
   (vs, when calling a .p each time - : 
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total.p|Total"
   )
------------------------------------------------------------------------*/   
DEFINE BUFFER bDocRel FOR JBoxDocRel.

DEF VAR cDocRelKeys AS CHAR NO-UNDO.
                         
PROCEDURE find_masterdoc_id:
  /* Find out if the input documentid is the master document */
  DEF INPUT PARAM  iiDocumentId AS INT  NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  FIND FIRST JBoxDocLink NO-LOCK
       WHERE JBoxDocLink.iFromDocumentId = iiDocumentId
       NO-ERROR.
  IF AVAIL JBoxDocLink AND JBoxDocLink.iToDocumentId NE JBoxDocLink.iFromDocumentId THEN
    ocReturn = "skiprow".
  ELSE
   ocReturn = STRING(iiDocumentId).
END PROCEDURE.

PROCEDURE doclink_filename:
  /* Find the filename for a linked document with optional 
     capability to filter out the master document.
     Purpose is to retrieve the file name from JBoxDocument
     without having to read the blob field from the database 
     as would be the case when using a standard query for JBoxDocument
   */
  DEF INPUT  PARAM irDocLink    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR  NO-UNDO.

  FIND JBoxDocLink NO-LOCK
       WHERE ROWID(JBoxDocLink) = irDocLink
       NO-ERROR.
  IF AVAIL JBoxDocLink THEN DO:
    IF icParam = "skipmaster" AND JBoxDocLink.iToDocumentId = JBoxDocLink.iFromDocumentId THEN
      ocReturn = "skiprow".
    ELSE DO:
      FOR EACH JBoxDocument 
               FIELDS (cFileName)
               NO-LOCK               
          WHERE JBoxDocument.iJBoxDocumentId = JBoxDocLink.iFromDocumentId
          :
        ocReturn = JBoxDocument.cFileName.       
      END.
    END.
  END.

END PROCEDURE.


PROCEDURE other_docrel:
  /* List relations to tables other than the viewed relation.
     Can also filter on the condition that other links exist/don't exist.
   */
  DEF INPUT  PARAM irDocRel     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR  NO-UNDO.

  cDocRelKeys = "".

  FIND JBoxDocRel NO-LOCK
       WHERE ROWID(JBoxDocRel) = irDocRel
       NO-ERROR.
  IF AVAIL JBoxDocRel THEN DO:
    FOR EACH bDocRel NO-LOCK
        WHERE bDocRel.iJBoxDocumentId = JBoxDocRel.iJBoxDocumentId
          AND bDocRel.cContext NE JBoxDocRel.cContext
        :
      IF NOT CAN-DO(ocReturn,bDocRel.cContext) THEN 
        ASSIGN ocReturn    = ocReturn + (IF ocReturn NE "" THEN "," ELSE "") + bDocRel.cContext
               cDocRelKeys = cDocRelKeys + (IF cDocRelKeys NE "" THEN "¤" ELSE "") + bDocRel.cEntityId
               .
    END.
    IF icParam = "skipWhenOtherRel" AND ocReturn NE "" THEN
      ocReturn = "skiprow".
    IF icParam = "skipWhenNoOtherRel" AND ocReturn = "" THEN
      ocReturn = "skiprow".
  END.

END PROCEDURE.

PROCEDURE other_docrel_keys:
  /* Dependent on other_docrel */
  DEF INPUT  PARAM irDocRel     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  ocReturn = cDocRelKeys.

END PROCEDURE.

PROCEDURE documentrel_list:
  /* Find list of documents for one relation */
  DEF INPUT  PARAM irDocRel     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR  NO-UNDO.

  FIND JBoxDocRel NO-LOCK
       WHERE ROWID(JBoxDocRel) = irDocRel
       NO-ERROR.

  IF AVAIL JBoxDocRel THEN DO:
    FOR EACH bDocRel NO-LOCK
        WHERE bDocRel.cContext  = JBoxDocRel.cContext
          AND bDocRel.cEntityId = JBoxDocRel.cEntityId
       ,FIRST JBoxDocument 
              FIELDS (cFileName)
              NO-LOCK
              WHERE JBoxDocument.iJBoxDocumentId = bDocRel.iJBoxDocumentId
        :
      ocReturn = ocReturn + (IF ocReturn NE "" THEN ", " ELSE "") + JBoxDocument.cFileName.
    END.
  END.
END PROCEDURE.

PROCEDURE documentrel_idlist:
  /* Find list of document ids for one relation */
  DEF INPUT  PARAM irDocRel     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  FIND JBoxDocRel NO-LOCK
       WHERE ROWID(JBoxDocRel) = irDocRel
       NO-ERROR.

  IF AVAIL JBoxDocRel THEN DO:
    FOR EACH bDocRel NO-LOCK
        WHERE bDocRel.cContext  = JBoxDocRel.cContext
          AND bDocRel.cEntityId = JBoxDocRel.cEntityId
       ,FIRST JBoxDocument 
              FIELDS (iJBoxDocumentId)
              NO-LOCK
              WHERE JBoxDocument.iJBoxDocumentId = bDocRel.iJBoxDocumentId
        :
      ocReturn = ocReturn + (IF ocReturn NE "" THEN "," ELSE "") + STRING(JBoxDocument.iJBoxDocumentId).
    END.
  END.
END PROCEDURE.
