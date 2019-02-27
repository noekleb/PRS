DISABLE TRIGGERS FOR LOAD OF JBoxMenu.

DEF VAR iFirstMenu AS INT NO-UNDO.
FOR EACH JBoxMenu 
    BY JBoxMenu.iJBoxMenuId DESC
    :
  FOR EACH JBoxMenuToMenu
      WHERE JBoxMenuToMenu.iFromMenuId = JBoxMenu.iJBoxMenuId
      :
    JBoxMenuToMenu.iFromMenuId = JBoxMenuToMenu.iFromMenuId + 1.
  END.
  FOR EACH JBoxMenuToMenu
      WHERE JBoxMenuToMenu.iToMenuId = JBoxMenu.iJBoxMenuId
      :
    JBoxMenuToMenu.iToMenuId = JBoxMenuToMenu.iToMenuId + 1.
  END.
  JBoxMenu.iJBoxMenuId = JBoxMenu.iJBoxMenuId + 1.
END.

FIND FIRST JBoxMenu.
iFirstMenu = JBoxMenu.iJBoxMenuId.

CREATE JBoxMenu.
ASSIGN JBoxMenu.iJBoxMenuId = 1
       JBoxMenu.cMenuType = "MENU"
       .
CREATE JBoxMenuToMenu.
ASSIGN JBoxMenuToMenu.iToMenuId = 1
       JBoxMenuToMenu.iFromMenuId = iFirstMenu.

FIND LAST JBoxMenu.
CURRENT-VALUE(seqJboxMenuId) = JBoxMenu.iJBoxMenuId.
