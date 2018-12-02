/* jbadmin_getfunction_access.p 
   Purpose: Get users and groups granted function access
   Parameters: FunctionId
-------------------------------------------------------------------------*/               
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

FOR EACH JBoxFunctionAccess NO-LOCK
    WHERE JBoxFunctionAccess.iJBoxFunctionId = INT(icParam)
      AND (JBoxFunctionAccess.cJBoxUserId NE "" OR JBoxFunctionAccess.iJboxUserGroupId NE 0)
    :
  ihBuffer:BUFFER-CREATE().
  ihBuffer:BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE = INT(icParam).

  IF JBoxFunctionAccess.cJBoxUserId NE "" THEN DO:
    FIND FIRST JBoxUser OF JBoxFunctionAccess NO-LOCK
         NO-ERROR.
    IF AVAIL JBoxUser THEN
      ihBuffer:BUFFER-FIELD("cUserOrGroup"):BUFFER-VALUE = JBoxUser.cUserName + " (" + JBoxUser.cJBoxUserId + ")".
  END.
  ELSE IF JBoxFunctionAccess.iJboxUserGroupId NE 0 THEN DO:
    FIND FIRST JBoxUserGroup OF JBoxFunctionAccess NO-LOCK
         NO-ERROR.
    IF AVAIL JBoxUserGroup THEN
      ihBuffer:BUFFER-FIELD("cUserOrGroup"):BUFFER-VALUE = JBoxUserGroup.cUserGroupName.
  END.
END.

obOK = ocReturn = "".
