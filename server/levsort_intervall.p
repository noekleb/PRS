DEF INPUT  PARAM irLevSort   AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR  NO-UNDO.

DEF VAR cLastStr  AS CHAR NO-UNDO.

FIND LevSort WHERE ROWID(LevSort) = irLevSort NO-LOCK NO-ERROR.
IF AVAIL LevSort THEN DO:
  FOR EACH LevSAnt FIELDS(SoStorl) NO-LOCK
      OF LevSort
      BY SeqNr:
    IF ocValue = "" THEN ocValue = SoStorl.
    cLastStr = SoStorl.
  END.
  ocValue = TRIM(ocValue) + " - " + TRIM(cLastStr).
END.
