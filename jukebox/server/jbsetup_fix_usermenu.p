FIND LAST JBoxUserMenu NO-LOCK 
     USE-INDEX idxUserMenuId
     NO-ERROR.
IF AVAIL JBoxUserMenu THEN
  CURRENT-VALUE(seqJboxUserMenuId) = JBoxUserMenu.iJBoxUserMenuId.

  
