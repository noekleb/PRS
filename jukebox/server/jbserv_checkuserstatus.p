DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT YES.

DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR cUserId         AS CHAR   NO-UNDO.
DEF VAR hPwdInterval    AS HANDLE NO-UNDO.
DEF VAR hLastPwdChange  AS HANDLE NO-UNDO.
DEF VAR dPwdChanged     AS DATE   NO-UNDO.
DEF VAR hBuffJBoxUser   AS HANDLE NO-UNDO.
DEF VAR hChangePwd      AS HANDLE NO-UNDO.

cUserId = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE).

FIND FIRST _User NO-LOCK
     WHERE _User._Userid = cUserId
     NO-ERROR.
IF NOT AVAIL _User THEN RETURN.

IF ENCODE(cUserId) = _User._Password THEN DO:
  ocReturn = "yes|".  
  RETURN.
END. 

ASSIGN hPwdInterval   = BUFFER JBoxUser:HANDLE:BUFFER-FIELD("iPwdInterval") 
       hLastPwdChange = BUFFER JBoxUser:HANDLE:BUFFER-FIELD("dLastPwdChange")
       hBuffJBoxUser  = BUFFER JBoxUser:HANDLE
       hChangePwd     = BUFFER JBoxUser:HANDLE:BUFFER-FIELD("bChangePwd")
       NO-ERROR.

IF VALID-HANDLE(hPwdInterval) AND VALID-HANDLE(hLastPwdChange) THEN DO:
  bOk = hBuffJBoxUser:FIND-FIRST("WHERE JBoxUser.cJBoxUserId = '" + cUserId + "'"
                                ,NO-LOCK) NO-ERROR.
  IF bOk THEN DO:
    
    IF VALID-HANDLE(hChangePwd) AND hChangePwd:BUFFER-VALUE = YES THEN DO:
      ocReturn = "yes|".  
      RETURN.
    END. 
    
    IF hPwdInterval:BUFFER-VALUE > 0 THEN DO:
      dPwdChanged = IF hLastPwdChange:BUFFER-VALUE NE ? THEN hLastPwdChange:BUFFER-VALUE ELSE hBuffJBoxUser:BUFFER-FIELD("dCreated"):BUFFER-VALUE.
      IF dPwdChanged = ? THEN DO:
        hBuffJBoxUser:FIND-CURRENT(EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
        IF hBuffJBoxUser:AVAIL THEN
          hLastPwdChange:BUFFER-VALUE = TODAY.
      END.
      ELSE DO:
        IF TODAY - hPwdInterval:BUFFER-VALUE GT dPwdChanged THEN
          ocReturn = "yes|".
        ELSE IF TODAY - hPwdInterval:BUFFER-VALUE GT dPwdChanged - 10 THEN
          ocReturn = "yes|" + STRING(ABS(TODAY - dPwdChanged - hPwdInterval:BUFFER-VALUE)).
      END.
    END.
  END.
END.

