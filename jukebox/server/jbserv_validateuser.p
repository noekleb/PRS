DEF INPUT PARAM icUserId     AS CHAR NO-UNDO.
DEF INPUT PARAM icPwd        AS CHAR NO-UNDO.
DEF INPUT PARAM icDateFormat AS CHAR NO-UNDO.
DEF INPUT PARAM icNumFormat  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocUserName  AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiCompanyId AS INT NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hLoginTime      AS HANDLE NO-UNDO.
DEF VAR hActive         AS HANDLE NO-UNDO.

DEF BUFFER bJBoxLoginSession FOR JBoxLoginSession.

IF icPwd NE CHR(3) THEN
  obOk = SETUSERID(icUserId,icPwd,LDBNAME(1)). 
ELSE obOk = YES.

IF obOk THEN DO:
  FIND JBoxUser
       WHERE JBoxUser.cJBoxUserId = icUserId
       NO-LOCK NO-ERROR.
  IF AVAIL JBoxUser THEN DO:

    hActive = BUFFER JBoxUser:BUFFER-FIELD("bActive") NO-ERROR.
    IF VALID-HANDLE(hActive) AND NOT hActive:BUFFER-VALUE THEN DO:
      obOk = NO.
      RETURN.
    END.

    IF NOT JBoxUser.bSuperUser THEN
      FOR EACH JBoxCompanyUser OF JBoxUser NO-LOCK 
          BY JBoxCompanyUser.bSuperUserCompany DESC:
        ix = ix + 1.
        IF ix = 1 THEN
          oiCompanyId = JBoxCompanyUser.iJBoxCompanyId.
      END.
    ELSE DO:
      FOR EACH JBoxCompany NO-LOCK:
        ASSIGN ix = ix + + 1
               oiCompanyId = JBoxCompany.iJBoxCompanyId.
      END.
      IF ix > 1 THEN
        oiCompanyId = 0.
    END.
  
    FOR EACH JBoxLoginSession
        WHERE JBoxLoginSession.cSessionId > ""
          AND JBoxLoginSession.dCreated < TODAY - 2
        NO-LOCK:
      FIND bJBoxLoginSession EXCLUSIVE-LOCK
           WHERE ROWID(bJBoxLoginSession) = ROWID(JBoxLoginSession)
           NO-WAIT NO-ERROR.
      IF AVAIL bJBoxLoginSession THEN
        bJBoxLoginSession.cSessionId = "".
    END.

    CREATE JBoxLoginSession.
    ASSIGN JBoxLoginSession.cSessionId  = ENCODE(icUserId + STRING(TODAY) + STRING(TIME))
           JBoxLoginSession.cJBoxUserId = icUserId
           JBoxLoginSession.cDateFormat = icDateFormat
           JBoxLoginSession.cNumFormat  = icNumFormat
           JBoxLoginSession.dCreated    = TODAY
           ocSessionId                  = JBoxLoginSession.cSessionId
           ocUserName                   = JBoxUser.cUserName
           .
    hLoginTime = BUFFER JBoxLoginSession:HANDLE:BUFFER-FIELD("iLoginTime") NO-ERROR.
    IF VALID-HANDLE(hLoginTime) THEN
      hLoginTime:BUFFER-VALUE = TIME.

  END.
  ELSE obOk = FALSE.
END.
