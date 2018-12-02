DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iVareBehType  AS INT NO-UNDO.

DEF BUFFER bVareBehType FOR VareBehType.
FIND LAST bVareBehType
     NO-LOCK NO-ERROR.
IF AVAIL bVareBehType THEN
  iVareBehType = bVareBehType.VareBehType + 1.
ELSE iVareBehType = 1.

hBuffer:BUFFER-FIELD("VareBehType"):BUFFER-VALUE = iVareBehType.

