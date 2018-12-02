/* Get session context */
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocContext       AS CHAR   NO-UNDO.

DEF VAR hJBoxLoginSession AS HANDLE NO-UNDO.
DEF VAR bJbLsFound        AS LOG    NO-UNDO.

CREATE BUFFER hJBoxLoginSession FOR TABLE "JBoxLoginSession" NO-ERROR.
IF NOT VALID-HANDLE(hJBoxLoginSession) THEN RETURN.

bJbLsFound = hJBoxLoginSession:FIND-FIRST("WHERE JBoxLoginSession.cSessionId = '" + icSessionId + "'") NO-ERROR.
IF bJbLsFound THEN
  ocContext = hJBoxLoginSession:BUFFER-FIELD("cContext"):BUFFER-VALUE.
         
DELETE OBJECT hJBoxLoginSession.

