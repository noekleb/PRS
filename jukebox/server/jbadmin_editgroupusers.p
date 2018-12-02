/* Update group-users link
   Parameters:  <grooupid>|<useridlist> 
   
   Created:  21.10.04 by Brynjar Hasle     
   Modified: 25.10.14 bt Brynjar
           - If UG is defined for company only users with access to company can be member           
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR iGroupId    AS INT    NO-UNDO.
DEF VAR hCompId     AS HANDLE NO-UNDO.
DEF VAR bOkUserComp AS LOG    NO-UNDO.

hCompId = BUFFER JBoxUserGroup:BUFFER-FIELD("iJBoxCompanyId") NO-ERROR.

iGroupId = INT(ENTRY(1,icParam,"|")).
icParam = SUBSTR(icParam,INDEX(icParam,"|") + 1).

FIND FIRST JBoxUserGroup NO-LOCK
     WHERE JBoxUserGroup.iJboxUserGroupId = iGroupId
     NO-ERROR.


IF AVAIL JBoxUserGroup THEN
  UpdateGroup:
  DO TRANSACTION:
  FOR EACH JBoxUserGroupMembers EXCLUSIVE-LOCK
      WHERE JBoxUserGroupMembers.iJBoxUserGroupId = iGroupId:
    IF LOOKUP(JBoxUserGroupMembers.cJBoxUserId,icParam,"|") = 0 THEN
      DELETE JBoxUserGroupMembers.
  END.
  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    FIND JBoxUserGroupMembers 
         WHERE JBoxUserGroupMembers.iJBoxUserGroupId = iGroupId
           AND JBoxUserGroupMembers.cJBoxUserId      = ENTRY(ix,icParam,"|")
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxUserGroupMembers THEN DO:
      FIND FIRST JBoxUser NO-LOCK
           WHERE JBoxUser.cJBoxUserId = ENTRY(ix,icParam,"|")
           NO-ERROR.
      IF AVAIL JBoxUser THEN DO:
        bOkUserComp = YES.
        IF NOT JBoxUser.bSuperUser AND VALID-HANDLE(hCompId) AND hCompId:BUFFER-VALUE NE 0 THEN DO:
          FIND FIRST JBoxCompanyUser NO-LOCK
               WHERE JBoxCompanyUser.iJBoxCompanyId = hCompId:BUFFER-VALUE
                 AND JBoxCompanyUser.cJBoxUserId = ENTRY(ix,icParam,"|")
               NO-ERROR.
          IF NOT AVAIL JBoxCompanyUser THEN bOkUserComp = NO.
        END.
        IF bOkUserComp THEN DO:
          CREATE JBoxUserGroupMembers.
          ASSIGN JBoxUserGroupMembers.iJBoxUserGroupId   = iGroupId
                 JBoxUserGroupMembers.cJBoxUserId        = ENTRY(ix,icParam,"|")
                 .
        END.
        ELSE DO:
          ocReturn = "User " + ENTRY(ix,icParam,"|") + " has no relation to company".
          UNDO, LEAVE UpdateGroup.
        END.
      END.
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

