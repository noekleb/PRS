DEF INPUT  PARAM irVarebokLinje AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

FIND VarebokLinje WHERE ROWID(VarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
IF AVAIL VarebokLinje THEN
  ocValue = STRING(ETid,"HH:MM:SS").
