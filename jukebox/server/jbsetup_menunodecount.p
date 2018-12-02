DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iCount      AS INT    NO-UNDO.
DEF VAR cCountList  AS CHAR   NO-UNDO.

DO ON ERROR UNDO, LEAVE:
  RUN ChildCount (INT(icParam)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    UNDO, LEAVE.
  ELSE obOK = TRUE.
END.

ocReturn = STRING(iCount).

PROCEDURE ChildCount:
  DEF INPUT PARAM iiJBoxMenuId       AS INT NO-UNDO.

  FOR EACH JBoxMenuToMenu NO-LOCK
      WHERE JBoxMenuToMenu.iToMenuId = iiJBoxMenuId:
    RUN ChildCount (JBoxMenuToMenu.iFromMenuId).
  END.
  IF NOT CAN-DO(cCountList,STRING(iiJBoxMenuId)) THEN
    ASSIGN iCount     = iCount + 1
           cCountList = cCountList + (IF cCountList NE "" THEN "," ELSE "") + STRING(iiJBoxMenuId).
END PROCEDURE.
