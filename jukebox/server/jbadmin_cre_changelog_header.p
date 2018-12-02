DEF INPUT  PARAM iiCompanyId         AS INT  NO-UNDO.
DEF INPUT  PARAM icText              AS CHAR NO-UNDO.
DEF INPUT  PARAM icUserId            AS CHAR NO-UNDO.
DEF INPUT  PARAM idFrom              AS DATE NO-UNDO.
DEF INPUT  PARAM iiFromTime          AS INT  NO-UNDO.
DEF INPUT  PARAM icExtra             AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiChangeLogHeaderId AS INT  NO-UNDO.

CREATE JBoxChangeLogHeader.
ASSIGN JBoxChangeLogHeader.cChangeLogHeaderText = icText
       JBoxChangeLogHeader.cCreatedBy           = icUserId
       JBoxChangeLogHeader.dChange              = idFrom
       JBoxChangeLogHeader.iChangeTime          = iiFromTime
       JBoxChangeLogHeader.iJBoxCompanyId       = iiCompanyId
       JBoxChangeLogHeader.dCreated             = TODAY
       .
oiChangeLogHeaderId = JBoxChangeLogHeader.iJBoxChangeLogHeaderId.

