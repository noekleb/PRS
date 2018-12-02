DEF INPUT  PARAM irVarebehBestHode AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

FIND VarebehBestHode WHERE ROWID(VarebehBestHode) = irVarebehBestHode NO-LOCK NO-ERROR.
IF AVAIL VarebehBestHode AND VarebehBestHode.BestNr NE 0 THEN DO:
  FIND BestHode OF VarebehBestHode NO-LOCK NO-ERROR.
  IF AVAIL BestHode THEN
    ocValue = STRING(BestHode.BestStat).
  ELSE 
    ocValue = "6".
END.
