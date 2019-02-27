/* Create link from menu-items to 
   Parameters:  <menuid>|<grouplist> 
      
   Created: 01.05.08 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND JBoxMenuToMenu EXCLUSIVE-LOCK 
       WHERE JBoxMenuToMenu.iToMenuId = ihBuffer:BUFFER-FIELD("iToMenuId"):BUFFER-VALUE 
         AND JBoxMenuToMenu.iFromMenuId = ihBuffer:BUFFER-FIELD("iFromMenuId"):BUFFER-VALUE 
       NO-WAIT NO-ERROR.
  IF AVAIL JBoxMenuToMenu THEN
    DELETE JBoxMenuToMenu.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

obOk = ocReturn = "".

