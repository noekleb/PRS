DEF INPUT PARAM  irJBoxCompany  AS ROWID NO-UNDO.
DEF INPUT PARAM  icMenuItem     AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId    AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn       AS CHAR  NO-UNDO INIT "no".

FIND JBoxCompany WHERE ROWID(JBoxCompany) = irJBoxCompany NO-LOCK NO-ERROR.
IF AVAIL JBoxCompany THEN DO:
  FIND FIRST JBoxCompanyMenu NO-LOCK
       WHERE JBoxCompanyMenu.iJBoxMenuId = INT(icMenuItem)
         AND JBoxCompanyMenu.iJBoxCompanyId = JBoxCompany.iJBoxCompanyId
       NO-ERROR.
  IF AVAIL JBoxCompanyMenu THEN
    ocReturn = STRING(JBoxCompanyMenu.bWriteAccess).
END.
