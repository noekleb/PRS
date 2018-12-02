DEF INPUT  PARAM irVarebehBestHode AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

FIND VarebehBestHode WHERE ROWID(VarebehBestHode) = irVarebehBestHode NO-LOCK NO-ERROR.
IF AVAIL VarebehBestHode AND VarebehBestHode.BestNr NE 0 
   AND CAN-FIND(FIRST TransLogg WHERE TransLogg.BatchNr = VarebehBestHode.BestNr) THEN 
  ocValue = STRING(VarebehBestHode.BestNr).
