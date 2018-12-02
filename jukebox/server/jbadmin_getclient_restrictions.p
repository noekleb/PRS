/* jbadmin_getclient_restrictions.p 
   Purpose: Get restricted actions for user
   Conventions: 
   - SysAdmins are not cheched
   - Company admins are only checked for menu access (typically so they can't reach the sys.adm menu)
   - All functions that are defined in the JBoxFunction table are restricted
   - 4 levels a user can be granted access:
     1. CompanyId and userid exist in the JBoxFunctionAccess table
     2. CompanyId 0 (zero) and userid exist
     3. CompanyId and user group exist
     4. CompanyId 0 and user group exist
-------------------------------------------------------------------------*/               
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR cUserId             AS CHAR   NO-UNDO.
DEF VAR cUserGroupList      AS CHAR   NO-UNDO.
DEF VAR cFunctionId         AS CHAR   NO-UNDO.
DEF VAR bCompanySuper       AS LOG    NO-UNDO.
DEF VAR cCompanyId          AS CHAR   NO-UNDO.
DEF VAR hBuffFunction       AS HANDLE NO-UNDO.
DEF VAR hBuffUser           AS HANDLE NO-UNDO.
DEF VAR hBuffFunctionAccess AS HANDLE NO-UNDO.
DEF VAR hBuffUserGroupMem   AS HANDLE NO-UNDO.
DEF VAR hBuffCompanyUser    AS HANDLE NO-UNDO.
DEF VAR hQueryFunction      AS HANDLE NO-UNDO.
DEF VAR hQueryUser          AS HANDLE NO-UNDO.
DEF VAR hQueryUserGroupMem  AS HANDLE NO-UNDO.
DEF VAR hAllowCompanyAdmins AS HANDLE NO-UNDO.

CREATE BUFFER hBuffFunction FOR TABLE "JBoxFunction" NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

hAllowCompanyAdmins = hBuffFunction:BUFFER-FIELD("bAllowCompanyAdmins") NO-ERROR.

CREATE BUFFER hBuffFunctionAccess FOR TABLE "JBoxFunctionAccess" NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

CREATE BUFFER hBuffUser FOR TABLE "JBoxUser" NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

ASSIGN cUserId    = DYNAMIC-FUNCTION("getASUserId" IN SOURCE-PROCEDURE)
       cCompanyId = DYNAMIC-FUNCTION("getCompanyId" IN SOURCE-PROCEDURE).

bOK = hBuffUser:FIND-FIRST("WHERE cJBoxUserId = '" + cUserId + "'",NO-LOCK) NO-ERROR.
IF bOk AND hBuffUser:BUFFER-FIELD("bSuperUser"):BUFFER-VALUE THEN DO:
  obOK = YES.
  DELETE OBJECT hBuffFunction NO-ERROR.
  DELETE OBJECT hBuffUser NO-ERROR.
  DELETE OBJECT hBuffFunctionAccess NO-ERROR.
  RETURN.
END.
ELSE IF NOT bOk THEN DO:
  CREATE QUERY hQueryFunction.
  hQueryFunction:SET-BUFFERS(hBuffFunction).
  hQueryFunction:QUERY-PREPARE("FOR EACH JBoxFunction NO-LOCK").
  hQueryFunction:QUERY-OPEN().

  hQueryFunction:GET-FIRST().
  REPEAT WHILE NOT hQueryFunction:QUERY-OFF-END:
    ihBuffer:BUFFER-CREATE().
    ihBuffer:BUFFER-COPY(hBuffFunction).
    ihBuffer:BUFFER-FIELD("bAccess"):BUFFER-VALUE = NO.

    hQueryFunction:GET-NEXT().
  END.    
  DELETE OBJECT hBuffFunction       NO-ERROR.
  DELETE OBJECT hQueryFunction      NO-ERROR.
  DELETE OBJECT hBuffUser NO-ERROR.
  DELETE OBJECT hBuffFunctionAccess NO-ERROR.
  obOK = ocReturn = "".
  RETURN.
END.

CREATE BUFFER hBuffCompanyUser FOR TABLE "JBoxCompanyUser" NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN DO:    
  bOK = hBuffCompanyUser:FIND-FIRST("WHERE cJBoxUserId = '" + cUserId + "' AND iJBoxCompanyId = " + cCompanyId,NO-LOCK) NO-ERROR.
  bCompanySuper = bOk AND hBuffCompanyUser:BUFFER-FIELD("bSuperUserCompany"):BUFFER-VALUE.
END.

CREATE BUFFER hBuffUserGroupMem FOR TABLE "JBoxUserGroupMembers" NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN DO:
  CREATE QUERY hQueryUserGroupMem.
  hQueryUserGroupMem:SET-BUFFERS(hBuffUserGroupMem).
  hQueryUserGroupMem:QUERY-PREPARE("FOR EACH JBoxUserGroupMembers NO-LOCK WHERE JBoxUserGroupMembers.cJBoxUserId = '" + cUserId + "'").
  hQueryUserGroupMem:QUERY-OPEN().
  hQueryUserGroupMem:GET-FIRST().
  REPEAT WHILE NOT hQueryUserGroupMem:QUERY-OFF-END:
    cUserGroupList = cUserGroupList + STRING(hBuffUserGroupMem:BUFFER-FIELD("iJBoxUserGroupId"):BUFFER-VALUE) + ",".
    hQueryUserGroupMem:GET-NEXT().
  END.
  cUserGroupList = TRIM(cUserGroupList,",").
END.


CREATE QUERY hQueryFunction.
hQueryFunction:SET-BUFFERS(hBuffFunction).
hQueryFunction:QUERY-PREPARE("FOR EACH JBoxFunction NO-LOCK"). /* + IF bCompanySuper THEN " WHERE bMenu" ELSE ""). */
hQueryFunction:QUERY-OPEN().

hQueryFunction:GET-FIRST().
REPEAT WHILE NOT hQueryFunction:QUERY-OFF-END:
  bOk = bCompanySuper AND VALID-HANDLE(hAllowCompanyAdmins) AND hAllowCompanyAdmins:BUFFER-VALUE.
  IF NOT bOk THEN DO:
    cFunctionId = (hBuffFunction:BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE).
    bOk = hBuffFunctionAccess:FIND-FIRST("WHERE JBoxFunctionAccess.iJBoxFunctionId = " + cFunctionId
                                        + " AND JBoxFunctionAccess.cJBoxUserId = '" + cUserId + "'"
                                        + " AND JBoxFunctionAccess.iJBoxCompanyId = " + cCompanyId,NO-LOCK) NO-ERROR.
    IF NOT bOk THEN DO:
      bOk = hBuffFunctionAccess:FIND-FIRST("WHERE JBoxFunctionAccess.iJBoxFunctionId = " + cFunctionId
                                          + " AND JBoxFunctionAccess.cJBoxUserId = '" + cUserId + "'"
                                          + " AND JBoxFunctionAccess.iJBoxCompanyId = 0",NO-LOCK) NO-ERROR.
      IF NOT bOk AND cUserGroupList NE "" THEN DO:
        bOk = hBuffFunctionAccess:FIND-FIRST("WHERE JBoxFunctionAccess.iJBoxFunctionId = " + cFunctionId
                                            + (IF NUM-ENTRIES(cUserGroupList) > 1 THEN 
                                                " AND CAN-DO('" + cUserGroupList + "',STRING(JBoxFunctionAccess.iJBoxUserGroupId))"
                                               ELSE
                                                " AND JBoxFunctionAccess.iJboxUserGroupId = " + cUserGroupList)
                                            + " AND JBoxFunctionAccess.iJBoxCompanyId = " + cCompanyId,NO-LOCK) NO-ERROR.        
        IF NOT bOk THEN 
          bOk = hBuffFunctionAccess:FIND-FIRST("WHERE JBoxFunctionAccess.iJBoxFunctionId = " + cFunctionId
                                              + (IF NUM-ENTRIES(cUserGroupList) > 1 THEN 
                                                  " AND CAN-DO('" + cUserGroupList + "',STRING(JBoxFunctionAccess.iJBoxUserGroupId))"
                                                 ELSE
                                                  " AND JBoxFunctionAccess.iJboxUserGroupId = " + cUserGroupList)
                                              + " AND JBoxFunctionAccess.iJBoxCompanyId = 0",NO-LOCK) NO-ERROR.
      END.
    END.
  END.

  ihBuffer:BUFFER-CREATE().
  ihBuffer:BUFFER-COPY(hBuffFunction).
  ihBuffer:BUFFER-FIELD("bAccess"):BUFFER-VALUE = bOk.

  hQueryFunction:GET-NEXT().
END.

DELETE OBJECT hBuffFunction       NO-ERROR.
DELETE OBJECT hBuffUser           NO-ERROR.
DELETE OBJECT hBuffFunctionAccess NO-ERROR.
DELETE OBJECT hBuffUserGroupMem   NO-ERROR.
DELETE OBJECT hBuffCompanyUser    NO-ERROR.
DELETE OBJECT hQueryFunction      NO-ERROR.
DELETE OBJECT hQueryUser          NO-ERROR.
DELETE OBJECT hQueryUserGroupMem  NO-ERROR.

obOK = ocReturn = "".
