/* Session validation */
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocCurrUserId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM oiCurrCompanyId AS INT    NO-UNDO.
DEF OUTPUT PARAM ocCurrLanguage  AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocContext       AS CHAR   NO-UNDO.

DEF VAR hJBoxLoginSession AS HANDLE NO-UNDO.
DEF VAR bJbLsFound        AS LOG    NO-UNDO.

CREATE BUFFER hJBoxLoginSession FOR TABLE "JBoxLoginSession" NO-ERROR.
IF NOT VALID-HANDLE(hJBoxLoginSession) THEN RETURN.

bJbLsFound = hJBoxLoginSession:FIND-FIRST("WHERE JBoxLoginSession.cSessionId = '" + icSessionId + "'") NO-ERROR.
IF bJbLsFound AND icSessionId NE "" THEN
  ASSIGN SESSION:DATE-FORMAT    = hJBoxLoginSession:BUFFER-FIELD("cDateFormat"):BUFFER-VALUE
         SESSION:NUMERIC-FORMAT = hJBoxLoginSession:BUFFER-FIELD("cNumFormat"):BUFFER-VALUE
         ocCurrUserId           = hJBoxLoginSession:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE
         oiCurrCompanyId        = hJBoxLoginSession:BUFFER-FIELD("iJBoxCompanyID"):BUFFER-VALUE
         ocCurrLanguage         = hJBoxLoginSession:BUFFER-FIELD("cLanguage"):BUFFER-VALUE
         ocContext              = hJBoxLoginSession:BUFFER-FIELD("cContext"):BUFFER-VALUE
         .
ELSE 
  ocReturn = "Invalid session. Either expired or reset due to system upgrade. Try new login".
         
DELETE OBJECT hJBoxLoginSession.

