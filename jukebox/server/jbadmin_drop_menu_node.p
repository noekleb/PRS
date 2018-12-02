/* Update menu to menu link
   Parameters:  <drag-node>|<drag-parent-node>|<drop-node>|<action> 
   
   Created: 18.04.08 by brynjar@chemistry.no                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iDragNode   AS INT  NO-UNDO.
DEF VAR iDragParent AS INT  NO-UNDO.
DEF VAR iDropNode   AS INT  NO-UNDO.
DEF VAR cAction     AS CHAR NO-UNDO.
DEF VAR cUserId     AS CHAR NO-UNDO.
DEF VAR bValidDrop  AS LOG  NO-UNDO INIT YES.

ASSIGN iDragNode    = INT(ENTRY(1,icParam,"|"))
       iDragParent  = INT(ENTRY(2,icParam,"|"))
       iDropNode    = INT(ENTRY(3,icParam,"|"))
       cAction      = ENTRY(4,icParam,"|")
       cUserId      = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE)
       .


FUNCTION ValidDropNode RETURNS LOGICAL (INPUT iiDropId AS INT,INPUT iiDragId AS INT):

  FOR EACH JBoxMenuToMenu NO-LOCK
      WHERE JBoxMenuToMenu.iFromMenuId = iiDropId
      :

    IF JBoxMenuToMenu.iToMenuId = iiDragId THEN DO:
      bValidDrop = NO.
      RETURN FALSE.
    END.
    ValidDropNode(JBoxMenuToMenu.iToMenuId,iiDragId).
  END.

  RETURN TRUE. 
END FUNCTION.

ValidDropNode(iDropNode,iDragNode).
IF NOT bValidDrop THEN 
  ocReturn = "Invalid drop-node (child in same branch)".  

ELSE DO TRANSACTION:

  IF cAction = "move" THEN DO:
    FIND JBoxMenuToMenu EXCLUSIVE-LOCK 
         WHERE JBoxMenuToMenu.iToMenuId   = iDragParent
           AND JBoxMenuToMenu.iFromMenuId = iDragNode
         NO-ERROR.
    IF AVAIL JBoxMenuToMenu THEN
      DELETE JBoxMenuToMenu.
/*     ELSE DO:                                         */
/*       ocReturn = "Current menu-link not available".  */
/*       UNDO, LEAVE.                                   */
/*     END.                                             */
  END.

  FIND JBoxMenuToMenu EXCLUSIVE-LOCK 
       WHERE JBoxMenuToMenu.iToMenuId   = iDropNode
         AND JBoxMenuToMenu.iFromMenuId = iDragNode
       NO-ERROR.
  IF NOT AVAIL JBoxMenuToMenu THEN DO:
    CREATE JBoxMenuToMenu.
    ASSIGN JBoxMenuToMenu.iToMenuId   = iDropNode
           JBoxMenuToMenu.iFromMenuId = iDragNode
           JBoxMenuToMenu.cCreatedBy  = cUserId
           JBoxMenuToMenu.dCreated    = TODAY
           .
  END.
END.

obOk = ocReturn = "".

