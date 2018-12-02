/* Retrieve login company for session. 
   Used f ex for to automatically append company selection criteria for query
*/

DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCompanyId AS CHAR NO-UNDO.

/*FIND FIRST JBoxLoginSession WHERE JBoxLoginSession.cSessionId = icSessionId NO-LOCK NO-ERROR.*/
/*IF AVAIL JBoxLoginSession THEN                                                               */
/*  ocCompanyId = STRING(JBoxLoginSession.iJBoxCompanyId).                                     */

FIND FIRST JBoxCompany NO-LOCK NO-ERROR.
IF AVAILABLE JBoxCompany THEN 
    ocCompanyId = STRING(JBoxCompany.iJBoxCompanyId).
ELSE 
    ocCompanyId = '1'.
