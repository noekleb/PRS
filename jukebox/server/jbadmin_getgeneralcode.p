/* get system parameter (from server procs) */
DEF INPUT PARAM  icCodeType   AS CHAR NO-UNDO.
DEF INPUT PARAM  icValueDesc  AS CHAR NO-UNDO. /* description or value */
DEF INPUT PARAM  iiCompanyId  AS INT  NO-UNDO.
DEF INPUT PARAM  iiCodeMaster AS INT  NO-UNDO.
DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

IF iiCodeMaster NE 0 THEN DO:
  FIND FIRST JBoxGenCode NO-LOCK
       WHERE JBoxGenCode.iJBoxCompanyId = iiCodeMaster
         AND JBoxGenCode.cCodeType      = icCodeType
         AND JBoxGenCode.cDescription   = icValueDesc
       NO-ERROR.
  IF AVAIL JBoxGenCode THEN
    ocValue = JBoxGenCode.cCodeValue.
  ELSE
    FIND FIRST JBoxGenCode NO-LOCK
         WHERE JBoxGenCode.iJBoxCompanyId = iiCodeMaster
           AND JBoxGenCode.cCodeType      = icCodeType
           AND JBoxGenCode.cCodeValue     = icValueDesc
         NO-ERROR.
  IF AVAIL JBoxGenCode THEN
    ocValue = JBoxGenCode.cDescription.
END. 

IF NOT AVAIL JBoxGenCode AND iiCompanyId NE 0 THEN DO:
  FIND FIRST JBoxGenCode NO-LOCK
       WHERE JBoxGenCode.iJBoxCompanyId = iiCompanyId
         AND JBoxGenCode.cCodeType      = icCodeType
         AND JBoxGenCode.cDescription   = icValueDesc
       NO-ERROR.
  IF AVAIL JBoxGenCode THEN
    ocValue = JBoxGenCode.cCodeValue.
  ELSE
    FIND FIRST JBoxGenCode NO-LOCK
         WHERE JBoxGenCode.iJBoxCompanyId = iiCompanyId
           AND JBoxGenCode.cCodeType      = icCodeType
           AND JBoxGenCode.cCodeValue     = icValueDesc
         NO-ERROR.
  IF AVAIL JBoxGenCode THEN
    ocValue = JBoxGenCode.cDescription.
END.

IF NOT AVAIL JBoxGenCode THEN DO:
  FIND FIRST JBoxGenCode NO-LOCK
       WHERE JBoxGenCode.iJBoxCompanyId = 0
         AND JBoxGenCode.cCodeType      = icCodeType
         AND JBoxGenCode.cDescription   = icValueDesc
       NO-ERROR.
  IF AVAIL JBoxGenCode THEN
    ocValue = JBoxGenCode.cCodeValue.
  ELSE
    FIND FIRST JBoxGenCode NO-LOCK
         WHERE JBoxGenCode.iJBoxCompanyId = 0
           AND JBoxGenCode.cCodeType      = icCodeType
           AND JBoxGenCode.cCodeValue     = icValueDesc
         NO-ERROR.
  IF AVAIL JBoxGenCode THEN
    ocValue = JBoxGenCode.cDescription.
END.

