DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

FIND Salesrep WHERE ROWID(Salesrep) = irBuffer NO-LOCK NO-ERROR.
IF AVAIL Salesrep THEN
  ocReturn = "distinct¤" + Region.
