/* Get liked users and groups for menu
   Parameters:  <menuid>
      
   Created:  01.05.08 by Brynjar Hasle                  
   Modified: 26.06.13 by Brynjar
             Support of bWriteAccess field in JBoxUserMenu table        
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO INIT YES.

DEF VAR iMenuId            AS INT    NO-UNDO.
DEF VAR hBuffJBoxUserMenu  AS HANDLE NO-UNDO.
DEF VAR hBuffJBoxUserGroup AS HANDLE NO-UNDO.
DEF VAR hBuffJBoxUser      AS HANDLE NO-UNDO.
DEF VAR hQuery             AS HANDLE NO-UNDO.
DEF VAR hWriteAccess       AS HANDLE NO-UNDO.

iMenuId = INT(ENTRY(1,icParam,";")).

CREATE BUFFER hBuffJBoxUserMenu FOR TABLE "JBoxUserMenu" NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  obOk = NO.  
  RETURN.
END.

CREATE BUFFER hBuffJBoxUser      FOR TABLE "JBoxUser".
CREATE BUFFER hBuffJBoxUserGroup FOR TABLE "JBoxUserGroup".

IF NOT ERROR-STATUS:ERROR THEN DO:

  hWriteAccess = hBuffJBoxUserMenu:BUFFER-FIELD("bWriteAccess") NO-ERROR.

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffJBoxUserMenu,hBuffJBoxUserGroup).
  hQuery:QUERY-PREPARE("FOR EACH JBoxUserMenu NO-LOCK WHERE JBoxUserMenu.iJBoxMenuId = " 
                     + icParam + ",FIRST JBoxUserGroup OF JBoxUserMenu NO-LOCK").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    ocReturn = ocReturn + hBuffJBoxUserGroup:BUFFER-FIELD("cUserGroupName"):BUFFER-VALUE + " (Group)"
             + (IF VALID-HANDLE(hWriteAccess) THEN
                 (IF hWriteAccess:BUFFER-VALUE THEN " (Write)" ELSE " (Read)")
                ELSE "")
             + CHR(10).
    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffJBoxUserGroup.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffJBoxUserMenu,hBuffJBoxUser).
hQuery:QUERY-PREPARE("FOR EACH JBoxUserMenu NO-LOCK WHERE JBoxUserMenu.iJBoxMenuId = " 
                   + icParam + ",FIRST JBoxUser OF JBoxUserMenu NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ocReturn = ocReturn + hBuffJBoxUser:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + " - " 
           + hBuffJBoxUser:BUFFER-FIELD("cUserName"):BUFFER-VALUE + " (User)" 
           + (IF VALID-HANDLE(hWriteAccess) THEN
               (IF hWriteAccess:BUFFER-VALUE THEN " (Write)" ELSE " (Read)")
              ELSE "")
           + CHR(10).
  hQuery:GET-NEXT().
END.
DELETE OBJECT hQuery.
DELETE OBJECT hBuffJBoxUser.
DELETE OBJECT hBuffJBoxUserMenu.



