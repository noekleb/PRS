/* Clean up potential errors in menu from dump/load etc   
   Created 09 apr 10 by brynjar@chemistry.no
-----------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iNext AS INT NO-UNDO.
DEF VAR hBuffUserMenu AS HANDLE NO-UNDO.

CREATE BUFFER hBuffUserMenu FOR TABLE "JBoxUserMenu" NO-ERROR.

FIND LAST JBoxMenu NO-LOCK NO-ERROR.
IF AVAIL JBoxMenu THEN DO:
  IF JBoxMenu.iJBoxMenuId > CURRENT-VALUE(seqJBoxMenuId) THEN
    CURRENT-VALUE(seqJBoxMenuId) = JBoxMenu.iJBoxMenuId.

  FIND FIRST JBoxMenu WHERE JBoxMenu.iJBoxMenuId = 0 NO-ERROR.
  IF AVAIL JBoxMenu THEN DO:
    iNext = NEXT-VALUE(seqJboxMenuId).
    JBoxMenu.iJBoxMenuId = iNext.
    FOR EACH JBoxMenuToMenu
        WHERE JBoxMenuToMenu.iFromMenuId = 0:
      JBoxMenuToMenu.iFromMenuId = iNext.
    END.
    FOR EACH JBoxMenuToMenu
        WHERE JBoxMenuToMenu.iToMenuId = 0:
      JBoxMenuToMenu.iToMenuId = iNext.
    END.
  END.
END.

IF VALID-HANDLE(hBuffUserMenu) THEN DO:
  RUN jbsetup_fix_usermenu.p.
  DELETE OBJECT hBuffUserMenu.
END.
