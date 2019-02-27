DEF INPUT  PARAM rRowid   AS ROWID NO-UNDO.
DEF INPUT  PARAM cParam   AS CHAR  NO-UNDO.
DEF INPUT  PARAM cSession AS CHAR  NO-UNDO.
DEF OUTPUT PARAM cList    AS CHAR  NO-UNDO.

DEF VAR cBrukerId AS CHAR NO-UNDO.

FIND butiker WHERE ROWID(butiker) = rRowid NO-LOCK NO-ERROR.
IF AVAIL butiker THEN
DO:
  FOR EACH ButikkKobling WHERE BrGrpNr                  GT 0
                           AND ButikkKobling.TeamTypeId EQ 2
                           AND ButikkKobling.TeamNr     GT 0
                           AND ButikkKobling.butik      EQ butiker.butik NO-LOCK
     ,FIRST ButikkTeam OF ButikkKobling 
                       NO-LOCK:
    cList = cList + ',' + STRING(ROWID(ButikkTeam)).
  END.
END.
cList = TRIM(cList,',').

