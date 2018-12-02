/* Determine startup menu for user:
   Usage:
   
  RUN JBoxJlwDynMenu.w PERSISTENT SET hDynMenu.
  DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",hDynMenu).

  IF DYNAMIC-FUNCTION("runProc","jbsetup_getuser_mainmenu.p","",?) THEN 
    iMenuId = INT(DYNAMIC-FUNCTION("getTransactionMessage")).
  ELSE DO:
/*     DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen meny er definert for bruker","","").  */
/*     QUIT. */
  END.

  RUN InitializeObject IN hDynMenu (iMenuId).
  
  Created 01 may 08 by brynjar@chemistry.no
-----------------------------------------------------------------------*/
                                                                         
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR iMenuId            AS INT    NO-UNDO.
DEF VAR cUserId            AS CHAR   NO-UNDO.
DEF VAR iCompanyId         AS INT    NO-UNDO.
DEF VAR hBuffMenu          AS HANDLE NO-UNDO.
DEF VAR hBuffUserMenu      AS HANDLE NO-UNDO.
DEF VAR hBuffUserGroupMem  AS HANDLE NO-UNDO.
DEF VAR hBuffCompanyMenu   AS HANDLE NO-UNDO.
DEF VAR hBuffCompanyUser   AS HANDLE NO-UNDO.
DEF VAR hQueryUser         AS HANDLE NO-UNDO.
DEF VAR hQueryGroup        AS HANDLE NO-UNDO.
DEF VAR hBuffUser          AS HANDLE NO-UNDO.
DEF VAR bSuperUser         AS LOG    NO-UNDO.
DEF VAR bCompanyAdmin      AS LOG    NO-UNDO.
DEF VAR hLimitToCompAdm    AS HANDLE NO-UNDO.
DEF VAR hLimitToSuperUser  AS HANDLE NO-UNDO.
DEF VAR hDefault           AS HANDLE NO-UNDO.
DEF VAR bRestUnlessAllowed AS LOG    NO-UNDO.

ASSIGN cUserId    = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       iCompanyId = DYNAMIC-FUNCTION("getCompanyId" IN SOURCE-PROCEDURE)
       bRestUnlessAllowed = icParam = "yes"
       .

CREATE BUFFER hBuffCompanyMenu FOR TABLE "JBoxCompanyMenu" NO-ERROR.
CREATE BUFFER hBuffMenu FOR TABLE "JBoxMenu".
hLimitToCompAdm   = hBuffMenu:BUFFER-FIELD("bLimitToCompanyAdmins") NO-ERROR.
hLimitToSuperUser = hBuffMenu:BUFFER-FIELD("bLimitToCompanyAdmins") NO-ERROR.
hDefault          = BUFFER JBoxMenu:BUFFER-FIELD("bDefault") NO-ERROR.

FUNCTION CompanyAccess RETURNS LOGICAL (INPUT iiMenuId AS INT ): 
  IF bSuperUser THEN RETURN YES.

  IF VALID-HANDLE(hLimitToCompAdm) OR VALID-HANDLE(hLimitToSuperUser) THEN DO:
    hBuffMenu:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.
    IF hBuffMenu:AVAIL THEN DO:
      IF hLimitToSuperUser:BUFFER-VALUE THEN RETURN NO.
      ELSE IF hLimitToCompAdm:BUFFER-VALUE THEN 
        RETURN bCompanyAdmin.
    END.
  END.

  IF VALID-HANDLE(hBuffCompanyMenu) THEN DO:
    hBuffCompanyMenu:FIND-FIRST("WHERE iJBoxMenuId   = " + STRING(iiMenuId)
                              + " AND iJBoxCompanyId = " + STRING(iCompanyId),NO-LOCK) NO-ERROR.
    IF NOT hBuffCompanyMenu:AVAIL THEN DO:
      hBuffCompanyMenu:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.
      IF NOT bRestUnlessAllowed THEN
        RETURN NOT hBuffCompanyMenu:AVAIL.
      ELSE
        RETURN NO.
    END.
    ELSE RETURN YES.
  END.
  ELSE RETURN YES.
END FUNCTION.

CREATE BUFFER hBuffUser FOR TABLE "JBoxUser" NO-ERROR.
IF VALID-HANDLE(hBuffUser) THEN DO:
  hBuffUser:FIND-FIRST("WHERE cJBoxUserId = '" + cUserId + "'",NO-LOCK) NO-ERROR.  
  IF hBuffUser:AVAIL THEN
    bSuperUser = hBuffUser:BUFFER-FIELD("bSuperUser"):BUFFER-VALUE.
END.

FOR EACH JBoxMenu NO-LOCK
    WHERE JBoxMenu.cMenuType = "menu"
       BY JBoxMenu.iJBoxMenuId DESC:
  ASSIGN ix      = ix + 1
         iMenuId = JBoxMenu.iJBoxMenuId
         .
END.

IF ix LE 1 OR cUserId = "" OR bSuperUser THEN DO:
  ASSIGN ocReturn = STRING(iMenuId)
         obOk     = YES.
  DELETE OBJECT hBuffCompanyMenu NO-ERROR.
  DELETE OBJECT hBuffUser NO-ERROR.
  RETURN.
END.

CREATE BUFFER hBuffUserMenu FOR TABLE "JBoxUserMenu" NO-ERROR.
IF NOT VALID-HANDLE(hBuffUserMenu) THEN DO:
  DELETE OBJECT hBuffCompanyMenu NO-ERROR.
  DELETE OBJECT hBuffUser NO-ERROR.
  RETURN.  
END. 

CREATE BUFFER hBuffCompanyUser FOR TABLE "JBoxCompanyUser" NO-ERROR.
IF VALID-HANDLE(hBuffCompanyUser) THEN DO:
  hBuffCompanyUser:FIND-FIRST("WHERE cJBoxUserId = '" + cUserId + "' AND iJBoxCompanyId = " + STRING(iCompanyId),NO-LOCK) NO-ERROR.  
  IF hBuffCompanyUser:AVAIL THEN
    bCompanyAdmin = hBuffCompanyUser:BUFFER-FIELD("bSuperUserCompany"):BUFFER-VALUE.
END.

CREATE QUERY hQueryUser.
hQueryUser:SET-BUFFERS(hBuffUserMenu,BUFFER JBoxMenu:HANDLE).
hQueryUser:QUERY-PREPARE("FOR EACH JBoxUserMenu NO-LOCK WHERE JBoxUserMenu.cJBoxUserId = '" + cUserId + "'"
                   + ",FIRST JBoxMenu OF JBoxUserMenu NO-LOCK WHERE JBoxMenu.cMenuType = 'menu'").
hQueryUser:QUERY-OPEN().
hQueryUser:GET-FIRST().
IF hBuffUserMenu:AVAIL AND CompanyAccess(hBuffUserMenu:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) THEN 
  ASSIGN ocReturn = hBuffUserMenu:BUFFER-FIELD("iJBoxMenuId"):STRING-VALUE
         obOk     = YES.
ELSE DO:
  CREATE BUFFER hBuffUserGroupMem FOR TABLE "JBoxUserGroupMembers" NO-ERROR.
  IF VALID-HANDLE(hBuffUserGroupMem) THEN DO:
    CREATE QUERY hQueryGroup.
    hQueryGroup:SET-BUFFERS(hBuffUserGroupMem,hBuffUserMenu,BUFFER JBoxMenu:HANDLE).
    hQueryGroup:QUERY-PREPARE("FOR EACH JBoxUserGroupMembers NO-LOCK WHERE JBoxUserGroupMembers.cJBoxUserId = '" + cUserId  + "'"
                       + ",EACH JBoxUserMenu NO-LOCK WHERE JBoxUserMenu.iJboxUserGroupId = JBoxUserGroupMembers.iJboxUserGroupId"
                       + ",FIRST JBoxMenu OF JBoxUserMenu WHERE JBoxMenu.cMenuType = 'menu' NO-LOCK BY JBoxUserMenu.iJBoxMenuId").
    hQueryGroup:QUERY-OPEN().
    hQueryGroup:GET-FIRST().
    REPEAT WHILE NOT hQueryGroup:QUERY-OFF-END:
      IF hBuffUserMenu:AVAIL AND CompanyAccess(hBuffUserMenu:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) THEN DO:
        ASSIGN ocReturn = hBuffUserMenu:BUFFER-FIELD("iJBoxMenuId"):STRING-VALUE
               obOk     = YES.
        LEAVE.
      END.
      hQueryGroup:GET-NEXT().
    END.
  END.
END.

IF NOT obOk THEN
  FOR EACH JBoxMenu NO-LOCK
      WHERE JBoxMenu.cMenuType = "menu"
         BY JBoxMenu.iJBoxMenuId
        :

    IF bRestUnlessAllowed THEN DO:
      IF CompanyAccess(JBoxMenu.iJBoxMenuId) THEN DO:
        ASSIGN ocReturn = STRING(JBoxMenu.iJBoxMenuId)
               obOk     = YES.
        LEAVE.
      END.
    END.
    ELSE DO:
      IF VALID-HANDLE(hBuffUserMenu) THEN DO:
        hBuffUserMenu:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(JBoxMenu.iJBoxMenuId)) NO-ERROR.  
        IF hBuffUserMenu:AVAILABLE AND NOT CompanyAccess(JBoxMenu.iJBoxMenuId) THEN NEXT.
      END.
      IF CompanyAccess(JBoxMenu.iJBoxMenuId) THEN DO:
        ASSIGN ocReturn = STRING(JBoxMenu.iJBoxMenuId)
               obOk     = YES.
        IF VALID-HANDLE(hDefault) AND hDefault:BUFFER-VALUE THEN LEAVE.
      END.
    END.
  END.

DELETE OBJECT hQueryUser NO-ERROR.
DELETE OBJECT hQueryGroup NO-ERROR.
DELETE OBJECT hBuffMenu NO-ERROR.
DELETE OBJECT hBuffUserMenu NO-ERROR.
DELETE OBJECT hBuffUserGroupMem NO-ERROR.
DELETE OBJECT hBuffUser NO-ERROR.
DELETE OBJECT hBuffCompanyMenu NO-ERROR.
DELETE OBJECT hBuffCompanyUser NO-ERROR.


