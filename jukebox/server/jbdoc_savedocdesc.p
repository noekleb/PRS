DEF INPUT  PARAM icDocRelRowid   AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields        AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues        AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

DEF VAR hDocRelBuff AS HANDLE NO-UNDO.
DEF VAR bOK         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

hDocRelBuff = BUFFER JBoxDocRel:HANDLE.

bOK = hDocRelBuff:FIND-BY-ROWID(TO-ROWID(icDocRelRowid),EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
IF bOk THEN DO:
  IF icFields = "cDescription" THEN DO:
    FIND FIRST jboxdocument WHERE JBoxDocument.iJBoxDocumentId = INT(hDocRelBuff:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL JBoxDocument THEN
      JBoxDocument.cDescription = icValues.
    ELSE ocReturn = "Document not available for update".
    RETURN.
  END.
END.
ELSE 
  ocReturn = "Document not available for update".

