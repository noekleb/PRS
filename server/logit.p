/* Procedures for handing Logging  */
DEF STREAM strLogit.

DEF VAR bLogIt AS LOG NO-UNDO.

DEFINE TEMP-TABLE LogMessage NO-UNDO
        FIELD LogText AS CHARACTER
        FIELD type AS CHAR .
/*
&GLOBAL-DEFINE LOG-MANAGER_LOGGING-LEVEL LOG-MANAGER:LOGGING-LEVEL
&GLOBAL-DEFINE LOG-MANAGER_LOGFILE-NAME  LOG-MANAGER:LOGFILE-NAME
&GLOBAL-DEFINE LOG-MANAGER_WRITE-MESSAGE LOG-MANAGER:WRITE-MESSAGE
*/

FUNCTION AddLogMessage RETURNS LOGICAL 
  (INPUT ipcMessage AS CHAR,
   INPUT ipcLogType AS CHAR):
    
    DEFINE VARIABLE cDateTimeStamp AS CHAR NO-UNDO. 
    cDateTimeStamp = 
       SUBSTRING(STRING(YEAR(TODAY),'9999'),3,2) + '-' +
       STRING(MONTH(TODAY),'99') + '-' +
       STRING(DAY(TODAY),'99') + ' ' + STRING(TIME,'HH:MM:SS').
    CREATE LogMessage. 
    LogMessage.LogText = cDateTimeStamp + CHR(9) + ipcLogType + CHR(9) + ipcMessage.  
    LogMessage.Type    = ipcLogType. 
END.

FUNCTION LOG-EXT RETURNS LOGICAL (INPUT ipcMessage AS CHAR):
/*     IF {&LOG-MANAGER_LOGGING-LEVEL} GE 4 THEN                    */
/*     DO:                                                          */
/*         {&LOG-MANAGER_WRITE-MESSAGE}(ipcMessage,"LOG") NO-ERROR. */
/*         AddLogMessage(ipcMessage,'LOG').                         */
/*     END.                                                         */
   IF bLogit THEN AddLogMessage(ipcMessage,'LOG').        
    RETURN TRUE.
END.
FUNCTION LOG-VER RETURNS LOGICAL (INPUT ipcMessage AS CHAR):
/*     IF {&LOG-MANAGER_LOGGING-LEVEL} GE 3 THEN                    */
/*     DO:                                                          */
/*         {&LOG-MANAGER_WRITE-MESSAGE}(ipcMessage,"LOG") NO-ERROR. */
/*         AddLogMessage(ipcMessage,'LOG').                         */
/*     END.                                                         */
    IF bLogit THEN AddLogMessage(ipcMessage,'LOG').        
    RETURN TRUE.
END.
FUNCTION LOG RETURNS LOGICAL (INPUT ipcMessage AS CHAR):
/*     IF {&LOG-MANAGER_LOGGING-LEVEL} GE 2 THEN                    */
/*     DO:                                                          */
/*         {&LOG-MANAGER_WRITE-MESSAGE}(ipcMessage,"LOG") NO-ERROR. */
/*         AddLogMessage(ipcMessage,'LOG').                         */
/*     END.                                                         */
  IF bLogit THEN AddLogMessage(ipcMessage,'LOG').        
    RETURN TRUE.
END.
FUNCTION INF RETURNS LOGICAL (INPUT ipcMessage AS CHAR):
/*     {&LOG-MANAGER_WRITE-MESSAGE}(ipcMessage,"LOG") NO-ERROR. */
    IF bLogit THEN AddLogMessage(ipcMessage,'INF').        
    RETURN TRUE.
END.
FUNCTION ERROR RETURNS LOGICAL (INPUT ipcMessage AS CHAR):
/*     IF {&LOG-MANAGER_LOGGING-LEVEL} GE 1 THEN                   */
/*        {&LOG-MANAGER_WRITE-MESSAGE}(ipcMessage,"ERR") NO-ERROR. */
    IF bLogit THEN AddLogMessage(ipcMessage,'ERR').        
    RETURN TRUE.
END.

FUNCTION printLog RETURNS LOGICAL (INPUT ipcFileName AS CHAR):
  OUTPUT STREAM strLogit TO VALUE(ipcFileName).
  PUT STREAM strLogit UNFORMATTED 'Tidspunkt' chr(9) 'Loggtype' CHR(9) 'Hendelse' SKIP.
  FOR EACH logMessage:
    PUT STREAM strLogit UNFORMATTED Logmessage.LogText SKIP.
  END.
  OUTPUT STREAM strLogit CLOSE.
END FUNCTION.

FUNCTION clearLog RETURNS LOGICAL ():
  EMPTY TEMP-TABLE LogMessage.
END FUNCTION.

FUNCTION setLogging RETURNS LOGICAL (INPUT ipbLogging AS LOG):
  bLogIt = ipbLogging.
END FUNCTION.

