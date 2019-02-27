/* Purpose:    Check status of application processes.
               Processes are configuered pr user and application.
               Usage: Check process status before (batch) download of application updates
   Parameters: Application id
               username 
   Created:   22.05.12 by brynjar@chemistry.no
-------------------------------------------------------------------------*/                  
DEF INPUT  PARAM icApplicationId     AS CHAR NO-UNDO. 
DEF INPUT  PARAM icComputerUserName  AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocRunningProcesses  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocStoppedProcesses  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.

{incl/validatesession.i}

FOR EACH JBoxAppProcess NO-LOCK
    WHERE JBoxAppProcess.iJBoxApplicationId = INTEGER(icApplicationId)
      AND JBoxAppProcess.cJBoxUserId        = icComputerUserName
      AND JBoxAppProcess.bActive
    :
  FIND FIRST JBoxAppProcessType NO-LOCK
       OF JBoxAppProcess
       NO-ERROR.
  IF JBoxAppProcess.bRunning THEN
    ocRunningProcesses = ocRunningProcesses +(IF ocRunningProcesses NE "" THEN ";" ELSE "") 
                       + (IF AVAILABLE JBoxAppProcessType THEN JBoxAppProcessType.cProcessType ELSE STRING(JBoxAppProcess.iJBoxAppProcessId)).
  ELSE
    ocStoppedProcesses = ocStoppedProcesses +(IF ocStoppedProcesses NE "" THEN ";" ELSE "") 
                       + (IF AVAILABLE JBoxAppProcessType THEN JBoxAppProcessType.cProcessType ELSE STRING(JBoxAppProcess.iJBoxAppProcessId)).
END.

