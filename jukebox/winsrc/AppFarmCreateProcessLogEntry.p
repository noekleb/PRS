/* Purpose:  Create process log entry.
             Can be called with an appserver handle and session-id or userid.             
                
   Created:  23.05.2012 by brynjar@chemistry.no
--------------------------------------------------------------------*/
DEF INPUT  PARAM hServer        AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF INPUT  PARAM icUserId       AS CHAR   NO-UNDO.
DEF INPUT  PARAM icProcessName  AS CHAR   NO-UNDO.
DEF INPUT  PARAM icLogType      AS CHAR   NO-UNDO.
DEF INPUT  PARAM icLogText      AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn       AS CHAR   NO-UNDO.

DEF VAR ix                AS INT    NO-UNDO.
DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR cRoot             AS CHAR   NO-UNDO INIT ".\".
DEF VAR cSignature        AS CHAR   NO-UNDO.

FUNCTION ConnectAppfarm RETURNS LOGICAL ( ):    
  {incl/startupconnect.i}  
END FUNCTION.

IF NOT VALID-HANDLE(hServer) OR (CAN-QUERY(hServer,"CLIENT-CONNECTION-ID") AND hServer:CLIENT-CONNECTION-ID = "") THEN DO:
  IF VALID-HANDLE(SOURCE-PROCEDURE) AND CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"getAppServiceHandle") THEN
    hServer = DYNAMIC-FUNCTION("getAppServiceHandle" IN SOURCE-PROCEDURE).

  IF NOT VALID-HANDLE(hServer) OR hServer:CLIENT-CONNECTION-ID = "" THEN DO:
    IF NOT ConnectAppfarm() THEN DO:
      ocReturn = "Failed to connect appfarm database".
      RETURN.
    END.
  END.
END.


IF VALID-HANDLE(SOURCE-PROCEDURE) THEN DO:
  IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"getRootDir") THEN
    cRoot = DYNAMIC-FUNCTION("getRootDir" IN SOURCE-PROCEDURE).
END.
  

IF SEARCH(cRoot + "signature.txt") NE ? THEN DO:

  INPUT FROM VALUE(cRoot + "signature.txt").
  IMPORT UNFORMATTED cSignature.
  INPUT CLOSE.

  bOK = SESSION:SET-WAIT-STATE("general").
  
  IF VALID-HANDLE(hServer) THEN DO:
    RUN jbappfarm_log_process_activity.p
        ON hServer
       (ENTRY(1,cSignature,"|"),
        icUserId,
        icSessionId,
        icProcessName,
        icLogType,
        icLogText,
        OUTPUT ocReturn,
        OUTPUT bOk)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      ocReturn = "Could not connect to appserver".
  END.
  ELSE 
    RUN jbappfarm_log_process_activity.p
       (ENTRY(1,cSignature,"|"),
        icUserId,
        icSessionId,
        icProcessName,
        icLogType,
        icLogText,
        OUTPUT ocReturn,
        OUTPUT bOk)
        .
  
  bOK = SESSION:SET-WAIT-STATE("").
END.

ELSE ocReturn = "ERROR: Application unavailable (missing signature file)".
