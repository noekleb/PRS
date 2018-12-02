/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=update_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   Dynamic (automatic) validation is done before custom val (ref doc/html/fieldMap.html for rules).
   
   If there's no fieldmap (viewer) set the attribute on the browse object
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

DEF VAR iKontoNr AS INT NO-UNDO.

FIND Innloser WHERE ROWID(Innloser) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL Innloser THEN DO:
  iKontoNr = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KontoNr")).
  IF iKontoNr > 0 THEN 
  DO:  
      FIND FIRST KontoTabell NO-LOCK
           WHERE KontoTabell.KontoNr = iKontoNr
           NO-ERROR.
      IF NOT AVAIL Kontotabell THEN
        ocReturn = "Ugyldig kontonummer. Angi 0 eller et gyldig kontonummer.".
  END.
END.


