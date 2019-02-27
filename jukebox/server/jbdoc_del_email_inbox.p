DEF INPUT PARAM  icBuffer    AS CHAR NO-UNDO.
DEF INPUT PARAM  icRowid     AS CHAR NO-UNDO.
DEF INPUT PARAM  icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

DEF VAR bOk           AS LOG  NO-UNDO.
DEF VAR cDeleteParam  AS CHAR NO-UNDO.

FIND JBoxEmailInbox WHERE ROWID(JBoxEmailInbox) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.

cDeleteParam = DYNAMIC-FUNCTION("getInputParam" IN SOURCE-PROCEDURE).
IF cDeleteParam = "" THEN cDeleteParam = "weak".

IF AVAIL JBoxEmailInbox THEN
  RUN jbdoc_delete_entity_doc.p (icBuffer + "|" 
                              + STRING(JBoxEmailInbox.iJBoxEmailInboxId) + "|"
                              + cDeleteParam,
                               ?,
                               icSessionId,
                               OUTPUT ocError,
                               OUTPUT bOk
                                ).
