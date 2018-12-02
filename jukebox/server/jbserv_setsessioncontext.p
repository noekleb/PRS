/* Set server session context
   
   Created: 18.10.12 by brynjar@chemistry.no
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.


FIND FIRST JBoxLoginSession EXCLUSIVE-LOCK
     WHERE JBoxLoginSession.cSessionId = icSessionId
     NO-ERROR.
IF AVAIL JBoxLoginSession THEN DO:
  IF icParam BEGINS "+|" THEN
    JBoxLoginSession.cContext = JBoxLoginSession.cContext + SUBSTR(icParam,3).
  ELSE
    JBoxLoginSession.cContext = icParam.
END.
ELSE ocReturn = "Login session not available".

obOK = ocReturn = "".
