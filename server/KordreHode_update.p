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

FIND KOrdreHode WHERE ROWID(KOrdreHode) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE KOrdreHode THEN 
DO:
    cPostNr = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"PostNr").
    IF NOT CAN-FIND(Post WHERE 
                    Post.PostNr = cPostNr) THEN
    DO: 
      ocReturn = '* Ugyldig postnummer på KOrdreHodens adresse.'.
      RETURN.
    END.  
    cPostNr = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"LevPostNr").
    IF NOT CAN-FIND(Post WHERE 
                    Post.PostNr = cPostNr) THEN
    DO: 
      ocReturn = '* Ugyldig postnummer på leveringsadressen.'.
      RETURN.
    END.  
    cPostNr = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"FaktPostNr").
    IF NOT CAN-FIND(Post WHERE 
                    Post.PostNr = cPostNr) THEN
    DO: 
      ocReturn = '* Ugyldig postnummer på faktura adressen.'.
      RETURN.
    END.  
  
END.

