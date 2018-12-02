DEF INPUT  PARAM irLevSort   AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR  NO-UNDO.

FIND LevSort WHERE ROWID(LevSort) = irLevSort NO-LOCK NO-ERROR.
IF AVAIL LevSort THEN 
  FOR EACH LevSAnt FIELDS(SoAnt) NO-LOCK
      OF LevSort
      BY SeqNr:
    ocValue = ocValue + STRING(SoAnt) + ",".
  END.
  
ocValue = TRIM(ocValue,",").
