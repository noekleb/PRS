DEF INPUT  PARAM iiFunctionId AS INT  NO-UNDO.
DEF INPUT  PARAM icUserId     AS CHAR NO-UNDO.
DEF OUTPUT PARAM obAccess     AS LOG  NO-UNDO.


FIND FIRST JBoxFunction NO-LOCK
     WHERE JBoxFunction.iJBoxFunctionId = iiFunctionId 
     NO-ERROR.
IF AVAIL JBoxFunction THEN DO:
  FIND FIRST JBoxFunctionAccess NO-LOCK
       OF JBoxFunction
       WHERE JBoxFunctionAccess.cJBoxUserId = icUserId
       NO-ERROR.
  IF NOT AVAIL JBoxFunctionAccess THEN 
    FOR EACH JBoxUserGroupMember NO-LOCK
        WHERE JBoxUserGroupMember.cJBoxUserId = icUserId
        :
      FIND FIRST JBoxFunctionAccess NO-LOCK
           OF JBoxFunction
           WHERE JBoxFunctionAccess.cJBoxUserId = icUserId
           NO-ERROR.
      IF AVAIL JBoxFunctionAccess THEN DO:
        obAccess = YES.
        LEAVE.
      END.
    END.
  ELSE obAccess = YES.
END.
ELSE obAccess = YES.

