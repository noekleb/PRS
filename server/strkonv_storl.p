DEF INPUT PARAM  iiStrKode     AS INT   NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

FIND StrKonv WHERE StrKonv.StrKode = iiStrKode NO-LOCK NO-ERROR.
IF AVAIL StrKonv THEN 
  ocReturn = TRIM(StrKonv.Storl).
