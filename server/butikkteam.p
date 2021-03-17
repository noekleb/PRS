DEF INPUT  PARAM rRowid   AS ROWID NO-UNDO.
DEF INPUT  PARAM cParam   AS CHAR  NO-UNDO.
DEF INPUT  PARAM cSession AS CHAR  NO-UNDO.
DEF OUTPUT PARAM cList    AS CHAR  NO-UNDO.

DEF VAR cBrukerId AS CHAR NO-UNDO.
DEFINE VARIABLE iTeamType AS INTEGER NO-UNDO.

cBrukerId = ENTRY(1,cParam,'¤').

IF NUM-ENTRIES(cParam,'¤') > 1 THEN 
  iTeamType = INT(ENTRY(2,cParam,'¤')).
ELSE
  iTeamType = 3.

FIND butiker WHERE ROWID(butiker) = rRowid NO-LOCK NO-ERROR.
IF AVAIL butiker THEN
DO:
  FIND Bruker WHERE Bruker.BrukerId = cBrukerId NO-LOCK NO-ERROR.
  IF AVAIL bruker THEN
  DO:
    IF CAN-FIND(butikktilgang WHERE ButikkTilgang.BrGrpNr = Bruker.BrGrpNr
                                AND ButikkTilgang.Butik   = Butiker.Butik) THEN
    DO:
      FOR EACH ButikkKobling WHERE ButikkKobling.BrGrpNr    = Bruker.BrGrpNr
                               AND ButikkKobling.TeamTypeId = iTeamType
                               AND ButikkKobling.TeamNr     GT 0
                               AND ButikkKobling.butik      = butiker.butik NO-LOCK
         ,FIRST ButikkTeam OF ButikkKobling 
                           NO-LOCK:
        cList = cList + ',' + STRING(ROWID(ButikkTeam)).
      END.
    END.
  END.
END.
cList = TRIM(cList,',').

