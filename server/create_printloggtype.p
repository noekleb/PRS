DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

FIND FIRST PrintLoggType
     WHERE PrintLoggType.LoggType = ENTRY(LOOKUP("LoggType",icFields),icValues,"|")
     NO-LOCK NO-ERROR.
IF NOT AVAIL PrintLoggType THEN DO:
  CREATE PrintLoggType.
  ASSIGN PrintLoggType.LoggType    = ENTRY(LOOKUP("LoggType",icFields),icValues,"|")
         PrintLoggType.Beskrivelse = ENTRY(LOOKUP("LoggType",icFields),icValues,"|")
         .
END.

