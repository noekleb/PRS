DEF INPUT  PARAM irBuffer      AS ROWID NO-UNDO.
DEF INPUT  PARAM icParam       AS CHAR  NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

FIND JBoxUser WHERE ROWID(JBoxUser) = irBuffer NO-LOCK NO-ERROR.

IF AVAIL JBoxUser THEN 
  FOR EACH JBoxCompanyUser OF JBoxUser NO-LOCK
     ,FIRST JBoxCompany OF JBoxCompanyUser NO-LOCK:
    IF icParam = "" OR JBoxCompanyUser.iJBoxCompanyId = INT(icParam) THEN
      ocReturn = ocReturn + JBoxCompany.cCompanyName + ", ".
  END.
IF icParam NE "" AND ocReturn = "" THEN
  ocReturn = "skiprow".
ELSE
  ocReturn = TRIM(ocReturn,", ").
