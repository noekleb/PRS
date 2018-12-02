DEF INPUT  PARAM irVarebehBestHode AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

FIND VarebehBestHode WHERE ROWID(VarebehBestHode) = irVarebehBestHode NO-LOCK NO-ERROR.
IF AVAIL VarebehBestHode THEN DO:
  FIND FIRST BestHode OF VarebehBestHode NO-LOCK NO-ERROR.
  IF AVAIL BestHode THEN
    ocValue = STRING(BestHode.LevDato).
  ELSE 
    ocValue = STRING(VarebehBestHode.LevDato).
END.
