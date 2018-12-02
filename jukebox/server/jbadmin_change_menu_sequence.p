/* Update menu sequence number
   Parameters:  <drag-node>|<drag-parent-node>|<drop-node>|<action> 
   
   Created: 05.05.08 by brynjar@chemistry.no                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iDragNode   AS INT  NO-UNDO.
DEF VAR iDragParent AS INT  NO-UNDO.
DEF VAR iDropNode   AS INT  NO-UNDO.
DEF VAR cUserId     AS CHAR NO-UNDO.
DEF VAR iDropSeq    AS INT  NO-UNDO.

DEF BUFFER bJBoxMenuToMenu FOR JBoxMenuToMenu.

ASSIGN iDragNode    = INT(ENTRY(1,icParam,"|"))
       iDragParent  = INT(ENTRY(2,icParam,"|"))
       iDropNode    = INT(ENTRY(3,icParam,"|"))
       cUserId      = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE)
       .

DO TRANSACTION ON ERROR UNDO,LEAVE:
  FIND JBoxMenuToMenu NO-LOCK
       WHERE JBoxMenuToMenu.iToMenuId   = iDragParent
         AND JBoxMenuToMenu.iFromMenuId = iDropNode
       NO-ERROR.
  IF AVAIL JBoxMenuToMenu THEN
    iDropSeq = JBoxMenuToMenu.iSeq.
  ELSE DO:
    ocReturn = "Couldn't _find drop node. Contact system admin: " + PROGRAM-NAME(1).
    RETURN.
  END.

  DO PRESELECT EACH JBoxMenuToMenu EXCLUSIVE-LOCK 
      WHERE JBoxMenuToMenu.iToMenuId = iDragParent
        AND JBoxMenuToMenu.iSeq > iDropSeq:
    FIND NEXT JBoxMenuToMenu NO-ERROR.
    IF AVAIL JBoxMenuToMenu THEN
      ASSIGN JBoxMenuToMenu.iSeq        = JBoxMenuToMenu.iSeq + 1
             JBoxMenuToMenu.cModifiedBy = cUserId
             JBoxMenuToMenu.dModified   = TODAY
             .
  END.

  FIND JBoxMenuToMenu EXCLUSIVE-LOCK 
       WHERE JBoxMenuToMenu.iToMenuId   = iDragParent
         AND JBoxMenuToMenu.iFromMenuId = iDragNode
       NO-ERROR.
  IF AVAIL JBoxMenuToMenu THEN
    ASSIGN JBoxMenuToMenu.iSeq        = iDropSeq + 1
           JBoxMenuToMenu.cModifiedBy = cUserId
           JBoxMenuToMenu.dModified   = TODAY
           .
  ELSE DO:
    ocReturn = "Couldn't _find drag node. Contact system admin: " + PROGRAM-NAME(1).
    RETURN.
  END.
END.

obOk = ocReturn = "".

