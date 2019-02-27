DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iVarebokType  AS INT NO-UNDO.

DEF BUFFER bVarebokType FOR VarebokType.
FIND LAST bVarebokType
     NO-LOCK NO-ERROR.
IF AVAIL bVarebokType THEN
  iVarebokType = bVarebokType.VarebokType + 1.
ELSE iVarebokType = 1.

hBuffer:BUFFER-FIELD("VarebokType"):BUFFER-VALUE = iVarebokType.

