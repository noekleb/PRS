/* get system parameter (from server procs) */
DEF INPUT PARAM  icParamName  AS CHAR NO-UNDO.
DEF INPUT PARAM  icValueType  AS CHAR NO-UNDO.
DEF INPUT PARAM  iiCompanyId  AS INT  NO-UNDO.
DEF INPUT PARAM  iiCodeMaster AS INT  NO-UNDO.
DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

IF iiCodeMaster NE 0 THEN 
  FIND FIRST JBoxSysParam NO-LOCK
       WHERE JBoxSysParam.iJBoxCompanyId = iiCodeMaster
         AND JBoxSysParam.cSysParamName  = icParamName
         AND JBoxSysParam.bActive
       NO-ERROR.

IF NOT AVAIL JBoxSysParam AND iiCompanyId NE 0 THEN
  FIND FIRST JBoxSysParam NO-LOCK
       WHERE JBoxSysParam.iJBoxCompanyId = iiCompanyId
         AND JBoxSysParam.cSysParamName  = icParamName
         AND JBoxSysParam.bActive
       NO-ERROR.

IF NOT AVAIL JBoxSysParam THEN
  FIND FIRST JBoxSysParam NO-LOCK
       WHERE JBoxSysParam.iJBoxCompanyId = 0
         AND JBoxSysParam.cSysParamName  = icParamName
         AND JBoxSysParam.bActive
       NO-ERROR.

IF AVAIL JBoxSysParam THEN 
  CASE icValueType:
    WHEN "date"    THEN ocValue = STRING(JBoxSysParam.dSysParamDateValue).
    WHEN "integer" THEN ocValue = STRING(JBoxSysParam.iSysParamIntValue).
    WHEN "decimal" THEN ocValue = STRING(JBoxSysParam.fSysParamDecimalValue).
    WHEN "text"    THEN ocValue = JBoxSysParam.cSysParamText.
    OTHERWISE           ocValue = JBoxSysParam.cSysParamCharValue.
  END CASE.
ELSE ocValue = ?.
