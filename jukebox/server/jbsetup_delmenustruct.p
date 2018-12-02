DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix               AS INT NO-UNDO.
DEF VAR bOK              AS LOG NO-UNDO.
DEF VAR hBuffMenu        AS HANDLE NO-UNDO.
DEF VAR httBuffer        AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.

DEF BUFFER bJBoxMenu      FOR JBoxMenu.

DO TRANSACTION ON ERROR UNDO, LEAVE:
  FOR EACH JBoxMenuToMenu EXCLUSIVE-LOCK
      WHERE JBoxMenuToMenu.iFromMenuId = INT(icParam):
    DELETE JBoxMenuToMenu.
  END.
  RUN DeleteMenu (INT(icParam)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    UNDO, LEAVE.
  ELSE obOK = TRUE.
END.

PROCEDURE DeleteMenu:
  DEF INPUT PARAM iiMenuId       AS INT NO-UNDO.

  DEF BUFFER bJBoxMenuToMenu FOR JBoxMenuToMenu.

  FOR EACH JBoxMenuToMenu EXCLUSIVE-LOCK
      WHERE JBoxMenuToMenu.iToMenuId = iiMenuId:
    FIND FIRST bJBoxMenuToMenu NO-LOCK
         WHERE bJBoxMenuToMenu.iFromMenuId = JBoxMenuToMenu.iFromMenuId
           AND bJBoxMenuToMenu.iToMenuId NE JBoxMenuToMenu.iToMenuId
         NO-ERROR.
    IF NOT AVAIL bJBoxMenuToMenu THEN
      RUN DeleteMenu (JBoxMenuToMenu.iFromMenuId).
    IF AVAIL JBoxMenuToMenu THEN 
      DELETE JBoxMenuToMenu.
  END.
  FIND JBoxMenu WHERE JBoxMenu.iJBoxMenuId = iiMenuId
       EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  IF NOT AVAIL JBoxMenu THEN DO:
    ocReturn = "Menu not available for delete".
    UNDO,RETURN ERROR.
  END.
  DELETE JBoxMenu.
END PROCEDURE.
