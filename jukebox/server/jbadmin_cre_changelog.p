DEF INPUT  PARAM iiChangeLogHeaderId AS INT  NO-UNDO.
DEF INPUT  PARAM iiCompanyId         AS INT  NO-UNDO.
DEF INPUT  PARAM icChangeType        AS CHAR NO-UNDO.
DEF INPUT  PARAM icUserId            AS CHAR NO-UNDO.
DEF INPUT  PARAM icEntityIdFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icEntityIdValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icTableName         AS CHAR NO-UNDO.
DEF INPUT  PARAM icFieldsChanged     AS CHAR NO-UNDO.
DEF INPUT  PARAM icFromValues        AS CHAR NO-UNDO.
DEF INPUT  PARAM icToValues          AS CHAR NO-UNDO.
DEF INPUT  PARAM icExtra             AS CHAR NO-UNDO.

FIND FIRST JBoxChangeLogType NO-LOCK
     WHERE JBoxChangeLogType.cChangeLogTypeId = icChangeType
     NO-ERROR.
IF NOT AVAIL JBoxChangeLogType THEN DO:
  CREATE JBoxChangeLogType.
  ASSIGN JBoxChangeLogType.cChangeLogTypeId   = icChangeType
         JBoxChangeLogType.cChangeLogTypeText = icChangeType
         JBoxChangeLogType.dCreated           = TODAY
         JBoxChangeLogType.cCreatedBy         = icUserId
         .
END.

CREATE JBoxChangeLog.
ASSIGN JBoxChangeLog.iJBoxChangeLogHeaderId = iiChangeLogHeaderId
       JBoxChangeLog.iJBoxCompanyId         = iiCompanyId
       JBoxChangeLog.cChangeLogTypeId       = icChangeType
       JBoxChangeLog.cEntityIdFields        = icEntityIdFields
       JBoxChangeLog.cEntityIdValues        = icEntityIdValues
       JBoxChangeLog.cTableChanged          = icTableName
       JBoxChangeLog.cFieldsChanged         = icFieldsChanged
       JBoxChangeLog.cFromValues            = icFromValues
       JBoxChangeLog.cToValues              = icToValues
       JBoxChangeLog.dCreated               = TODAY
       .

