DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

FIND KOrdreLinje NO-LOCK
     WHERE ROWID(KOrdreLinje) = irKOrdreLinje
     NO-ERROR.

IF AVAIL KOrdreLinje THEN 
  ocValue = STRING(KOrdreLinje.NettoLinjeSum - KOrdreLinje.MvaKr).

