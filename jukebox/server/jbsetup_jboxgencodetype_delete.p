/* To invoke (standard): 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomDeleteValProc","=delete_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   
   If there's no fieldmap (viewer) set the attribute on the browse or query object
   
   HERE: The routine is invoked directly on a DoDelete call in JBoxGenCode.w,DeleteType
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icBufferName        AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

FIND JBoxGenCodeType WHERE ROWID(JBoxGenCodeType) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL JBoxGenCodeType THEN DO:
  FOR EACH JBoxGenCode EXCLUSIVE-LOCK 
      WHERE JBoxGenCode.cCodeType = JBoxGenCodeType.cCodeType
      :
    DELETE JBoxGenCode.
  END.
END.


