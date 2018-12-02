/* Create link from menu-items to 
   Parameters:  <menuid>|<grouplist> 
      
   Created: 01.05.08 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT    NO-UNDO.
DEF VAR iToMenuId     AS INT    NO-UNDO.
DEF VAR cUserId       AS CHAR   NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hBuffMenToMen AS HANDLE NO-UNDO.

ASSIGN iToMenuId     = INT(ENTRY(1,icParam,";"))
       cUserId       = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE)
       hBuffMenToMen = BUFFER JBoxMenuToMenu:HANDLE
       .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  CREATE JBoxMenuToMenu.
  hBuffMenToMen:BUFFER-COPY(ihBuffer,"iToMenuId").
  ASSIGN JBoxMenuToMenu.iToMenuId  = iToMenuId
         JBoxMenuToMenu.dCreated   = TODAY
         JBoxMenuToMenu.cCreatedBy = cUserId
         .
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

obOk = ocReturn = "".

