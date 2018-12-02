DEF TEMP-TABLE ttMenu
    FIELD cMenuType    AS CHARACTER
    FIELD cMenuLabel   AS CHARACTER
    FIELD cParentMenu  AS CHARACTER
    FIELD cLaunch      AS CHARACTER
    FIELD bPersistent  AS LOG
    FIELD iMenuId      AS INTEGER
    .
DEF BUFFER bttMenu FOR ttMenu.

DEF VAR iSeq    AS INT NO-UNDO.
DEF VAR iParent AS INT NO-UNDO.
DEF VAR ix      AS INT NO-UNDO.

INPUT FROM .\menydump.txt.
REPEAT:
  CREATE ttMenu.
  IMPORT DELIMITER "|" ttMenu.
END.
INPUT CLOSE.

DELETE FROM JBoxMenu WHERE JBoxMenu.iJBoxMenuId > 2.
DELETE FROM JBoxMenuToMenu WHERE JBoxMenuToMenu.iFromMenuId > 2.

FIND LAST JBoxMenu NO-LOCK NO-ERROR.
IF AVAIL JBoxMenu THEN
  CURRENT-VALUE(seqJBoxMenuId) = JBoxMenu.iJBoxMenuId.


FOR EACH ttMenu:
  IF ttMenu.cMenuType = "rule" AND ttMenu.cParentMenu = "" THEN NEXT.
  CREATE JBoxMenu.
  BUFFER-COPY ttMenu TO JBoxMenu.
  IF ttMenu.cLaunch NE ""  THEN DO:
    IF ttMenu.bPersistent THEN
      JBoxMenu.cLaunchType = "start-window".
    ELSE
      JBoxMenu.cLaunchType = "procedure".
  END. 
  ttMenu.iMenuId = JBoxMenu.iJBoxMenuId.

  FIND FIRST bttMenu
       WHERE bttMenu.cMenuLabel = ttMenu.cParentMenu
         AND bttMenu.cMenuLabel NE ""
       NO-ERROR.

  CREATE JBoxMenuToMenu.
  ASSIGN JBoxMenuToMenu.iFromMenuId = JBoxMenu.iJBoxMenuId
         JBoxMenuToMenu.iToMenuId   = (IF AVAIL bttMenu THEN bttMenu.iMenuId ELSE IF JBoxMenu.iJBoxMenuId > 4 THEN 3 ELSE 2)
         JBoxMenuToMenu.iSeq        = iSeq
         JBoxMenuToMenu.dCreated    = TODAY
         iSeq                       = iSeq + 2
         ix                         = ix + 1
         .
END.
CREATE JBoxMenu.
ASSIGN JBoxMenu.cMenuLabel = "Jukebox menyoppsett"
       JBoxMenu.cLaunch    = "JBoxJlwMenuMaint.w"
       JBoxMenu.cLaunchType = "start-window"
       JBoxMenu.cMenuType   = "menu-item"
       .
CREATE JBoxMenuToMenu.
ASSIGN JBoxMenuToMenu.iFromMenuId = JBoxMenu.iJBoxMenuId
       JBoxMenuToMenu.iToMenuId   = 2
       JBoxMenuToMenu.iSeq        = iSeq.


FOR EACH JBoxMenu:
  JBoxMenu.cMenuLabel = REPLACE(JBoxMenu.cMenuLabel,"&","").
END.
