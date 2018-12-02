DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iLgId  AS INT NO-UNDO.

DEF BUFFER bLokalGruppering FOR LokalGruppering.
FIND LAST bLokalGruppering
     NO-LOCK NO-ERROR.
IF AVAIL bLokalGruppering THEN
  iLgId = bLokalGruppering.LgId + 1.
ELSE iLgId = 1.

hBuffer:BUFFER-FIELD("LgId"):BUFFER-VALUE = iLgId.

