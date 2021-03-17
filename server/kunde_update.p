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

DEFINE VARIABLE cPostNr AS CHARACTER NO-UNDO.

DEFINE BUFFER bKunde FOR Kunde.

DO FOR bKunde TRANSACTION:
  FIND bKunde WHERE ROWID(bKunde) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF AVAILABLE bKunde AND NOT LOCKED bKunde THEN 
  DO:
      cPostNr = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"PostNr").
      IF NOT CAN-FIND(Post WHERE 
                      Post.PostNr = cPostNr) THEN
      DO: 
        ocReturn = '* Ugyldig postnummer på kundens adresse.'.
        RELEASE bKunde.
        RETURN.
      END.  
      cPostNr = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"LevPostNr").
      IF NOT CAN-FIND(Post WHERE 
                      Post.PostNr = cPostNr) THEN
      DO: 
        ocReturn = '* Ugyldig postnummer på leveringsadressen.'.
        RELEASE bKunde.
        RETURN.
      END.  
      cPostNr = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"FaktPostNr").
      IF NOT CAN-FIND(Post WHERE 
                      Post.PostNr = cPostNr) THEN
      DO: 
        ocReturn = '* Ugyldig postnummer på faktura adressen.'.
        RELEASE bKunde.
        RETURN.
      END.  
    
  END.
  ELSE DO: 
    ocReturn = '* Kunde record er lås fra en annen terminal. Kan ikke oppdateres.'.
    IF AVAILABLE bKunde THEN 
      RELEASE bKunde.
    RETURN.
  END.  
  
END. /* TRANSACTION */
