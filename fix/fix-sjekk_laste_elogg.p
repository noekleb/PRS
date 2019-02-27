DEF VAR iAnt AS INT NO-UNDO.
CURRENT-WINDOW:WIDTH = 300.

  DEF BUFFER tmpELogg FOR Elogg.

  FOR EACH ELogg NO-LOCK:
    FIND tmpelogg WHERE RECID(tmpELogg) = RECID(ELogg) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF LOCKED tmpELogg THEN 
    DO:
      iAnt = iAnt + 1.
      FIND tmpELogg WHERE RECID(tmpELogg) = RECID(ELogg) NO-LOCK NO-ERROR.
      IF AVAILABLE tmpElogg THEN
        DISPLAY tmpElogg WITH WIDTH 300.
    END.

  END.
  MESSAGE iANt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

