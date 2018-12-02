/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=update_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   Dynamic (automatic) validation is done before custom val (ref doc/html/fieldMap.html for rules).
   
   If there's no fieldmap (viewer) set the attribute on the browse object
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

DEF VAR iCodemasterCompany AS INT NO-UNDO.
DEF VAR iParentCompany     AS INT NO-UNDO.

FIND JBoxCompany WHERE ROWID(JBoxCompany) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL JBoxCompany THEN DO:  
  iCodeMasterCompany = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"codemaster")).
  FIND FIRST JBoxCompanyToCompany OF JBoxCompany EXCLUSIVE-LOCK
       WHERE JBoxCompanyToCompany.cCompanyRole = "codemaster"
       NO-ERROR.
  IF AVAIL JBoxCompanyToCompany THEN DO:
    IF iCodemasterCompany = 0 THEN DELETE JBoxCompanyToCompany.
    ELSE JBoxCompanyToCompany.iJBoxToCompanyId = iCodeMasterCompany.
  END.
  ELSE IF iCodeMasterCompany NE 0 THEN DO:
    CREATE JBoxCompanyToCompany.
    ASSIGN JBoxCompanyToCompany.iJBoxCompanyId   = JBoxCompany.iJBoxCompanyId
           JBoxCompanyToCompany.iJBoxToCompanyId = iCodeMasterCompany
           JBoxCompanyToCompany.cCompanyRole     = "codemaster".
  END.

  iParentCompany = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"parent")).
  FIND FIRST JBoxCompanyToCompany OF JBoxCompany EXCLUSIVE-LOCK
       WHERE JBoxCompanyToCompany.cCompanyRole = "parent"
       NO-ERROR.
  IF AVAIL JBoxCompanyToCompany THEN DO:
    IF iParentCompany = 0 THEN DELETE JBoxCompanyToCompany.
    ELSE JBoxCompanyToCompany.iJBoxToCompanyId = iParentCompany.
  END.
  ELSE IF iParentCompany NE 0 THEN DO:
    CREATE JBoxCompanyToCompany.
    ASSIGN JBoxCompanyToCompany.iJBoxCompanyId   = JBoxCompany.iJBoxCompanyId
           JBoxCompanyToCompany.iJBoxToCompanyId = iParentCompany
           JBoxCompanyToCompany.cCompanyRole     = "parent".
  END.
END.


