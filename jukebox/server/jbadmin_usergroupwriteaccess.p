DEF INPUT PARAM  irJBoxUserGroup  AS ROWID NO-UNDO.
DEF INPUT PARAM  icMenuItem     AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId    AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn       AS CHAR  NO-UNDO INIT "no".

FIND JBoxUserGroup WHERE ROWID(JBoxUserGroup) = irJBoxUserGroup NO-LOCK NO-ERROR.
IF AVAIL JBoxUserGroup THEN DO:
  FIND FIRST JBoxUserMenu NO-LOCK
       WHERE JBoxUserMenu.iJBoxMenuId = INT(icMenuItem)
         AND JBoxUserMenu.iJBoxUserGroupId = JBoxUserGroup.iJBoxUserGroupId
       NO-ERROR.
  IF AVAIL JBoxUserMenu THEN
    ocReturn = STRING(JBoxUserMenu.bWriteAccess).
END.
