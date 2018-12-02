/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomDeleteValProc","=delete_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   Dynamic (automatic) validation is done before custom val (ref doc/html/fieldMap.html for rules).
   
   If there's no fieldmap (viewer) set the attribute on the browse or query object
   
   Note the function-call SuppressDynDelete: When schema delete validation the record must be
   deleted with a DELETE statement. The main routine will issue a BUFFER-DELETE and fail if this
   function-call isn't issued
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icBufferName        AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

FIND JBoxFileImportLog WHERE ROWID(JBoxFileImportLog) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL JBoxFileImportLog THEN DO:
  FOR EACH JBoxFileImportLogFileHeader OF JBoxFileImportLog
      EXCLUSIVE-LOCK:
    FOR EACH JBoxFileImportLogFile EXCLUSIVE-LOCK 
        OF JBoxFileImportLogFileHeader:
      DELETE JBoxFileImportLogFile.
    END.
    DELETE JBoxFileImportLogFileHeader.
  END.
  DELETE JBoxFileImportLog NO-ERROR.
END.



