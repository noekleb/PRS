/* Purpose:   Create process log entry (if logtype is defined to do so).
      Note:   If a JBoxAppProcessUserLogType record exists the logical flags override 
              settings in JBoxAppProcessLogType if they are set (ne ?)

   Created:   22.05.12 by brynjar@chemistry.no
-------------------------------------------------------------------------*/                  
DEF INPUT  PARAM icApplicationId     AS CHAR NO-UNDO. 
DEF INPUT  PARAM icUserId            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF INPUT  PARAM icProcessType       AS CHAR NO-UNDO.
DEF INPUT  PARAM icLogType           AS CHAR NO-UNDO.
DEF INPUT  PARAM icLogText           AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk                AS LOG  NO-UNDO.

DEF VAR bCreLogEntry  AS LOG NO-UNDO INIT ?.
DEF VAR bReturnStop   AS LOG NO-UNDO INIT ?.

IF icSessionId NE "" THEN DO:
  {incl/validatesession.i}
END.

FIND FIRST JBoxApplication NO-LOCK
     WHERE JBoxApplication.iJBoxApplicationId = INTEGER(icApplicationId)
     NO-ERROR.
IF NOT AVAIL JBoxApplication THEN DO:
  ocReturn = "Invalid application id: " + icApplicationId.
  RETURN.
END.

FIND FIRST JBoxUser NO-LOCK
     WHERE JBoxUser.cJBoxUserId = icUserId
     NO-ERROR.
IF NOT AVAIL JBoxUser THEN DO:
  ocReturn = "Invalid user id: " + icUserId.
  RETURN.
END.

FIND FIRST JBoxAppProcessType NO-LOCK
     WHERE JBoxAppProcessType.cProcessType = icProcessType
     NO-ERROR.
IF NOT AVAIL JBoxAppProcessType THEN DO:
  ocReturn = "Invalid process type: " + icProcessType.
  RETURN.
END.

FIND FIRST JBoxAppProcessLogType NO-LOCK
     WHERE JBoxAppProcessLogType.cLogType = icLogType
     NO-ERROR.
IF NOT AVAIL JBoxAppProcessLogType THEN DO:
  ocReturn = "Invalid process log type: " + icLogType.
  RETURN.
END.

FIND FIRST JBoxAppProcess EXCLUSIVE-LOCK
     WHERE JBoxAppProcess.iJBoxApplicationId     = JBoxApplication.iJBoxApplicationId
       AND JBoxAppProcess.cJBoxUserId            = JBoxUser.cJBoxUserId
       AND JBoxAppProcess.iJBoxAppProcessTypeId  = JBoxAppProcessType.iJBoxAppProcessTypeId
    NO-WAIT NO-ERROR.
IF NOT AVAIL JBoxAppProcess THEN DO:
  CREATE JBoxAppProcess.
  ASSIGN JBoxAppProcess.iJBoxApplicationId    = JBoxApplication.iJBoxApplicationId
         JBoxAppProcess.cJBoxUserId           = JBoxUser.cJBoxUserId
         JBoxAppProcess.iJBoxAppProcessTypeId = JBoxAppProcessType.iJBoxAppProcessTypeId
         JBoxAppProcess.dCreated              = TODAY
         JBoxAppProcess.cCreatedBy            = JBoxUser.cJBoxUserId
         .
END.
JBoxAppProcess.bRunning = YES.

FIND FIRST JBoxAppProcessUserLogType NO-LOCK
     WHERE JBoxAppProcessUserLogType.cJBoxUserId              = JBoxUser.cJBoxUserId
       AND JBoxAppProcessUserLogType.iJBoxAppProcessLogTypeId = JBoxAppProcessLogType.iJBoxAppProcessLogTypeId
     NO-ERROR.

IF AVAIL JBoxAppProcessUserLogType THEN
  ASSIGN bCreLogEntry = JBoxAppProcessUserLogType.bCreateLogEntry
/*          bReturnStop  = JBoxAppProcessUserLogType.bReturnStopSignal <- not sure if this should be configurable as an override */
         .
/* IF bReturnStop = ? THEN                                  */
  bReturnStop = JBoxAppProcessLogType.bReturnStopSignal.

IF bCreLogEntry = ? THEN
  bCreLogEntry = JBoxAppProcessLogType.bCreateLogEntry.

IF JBoxAppProcessLogType.bFlagAsNotRunning THEN 
  JBoxAppProcess.bRunning = NO.

IF bCreLogEntry THEN DO:
  CREATE JBoxAppProcessLog.
  ASSIGN JBoxAppProcessLog.iJBoxAppProcessId        = JBoxAppProcess.iJBoxAppProcessId
         JBoxAppProcessLog.dtTime                   = NOW
         JBoxAppProcessLog.iJBoxAppProcessLogTypeId = JBoxAppProcessLogType.iJBoxAppProcessLogTypeId
         JBoxAppProcessLog.cLogText                 = icLogText
         .
END.


obOk = ocReturn = "".
IF bReturnStop THEN ocReturn = "STOP".
