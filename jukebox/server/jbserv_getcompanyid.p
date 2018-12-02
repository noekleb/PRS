/* Retrieve login company for session. 
   Used f ex for to automatically append company selection criteria for query
*/

DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCompanyId AS CHAR NO-UNDO.

FIND FIRST JBoxLoginSession WHERE JBoxLoginSession.cSessionId = icSessionId NO-LOCK NO-ERROR.
IF AVAIL JBoxLoginSession THEN 
  ocCompanyId = STRING(JBoxLoginSession.iJBoxCompanyId).
