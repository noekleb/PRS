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

DEF VAR iButikkNr AS INT  NO-UNDO.


FIND PlListeHode WHERE ROWID(PlListeHode) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL PlListeHode THEN DO:
  iButikkNr = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"FraButikkNr")).  

  FIND FIRST Butiker NO-LOCK 
       WHERE Butiker.Butik = iButikkNr
       NO-ERROR.
  IF NOT AVAIL Butiker THEN DO:
    ocReturn = "Finner ikke butikk".
    RETURN.
  END.
END.


