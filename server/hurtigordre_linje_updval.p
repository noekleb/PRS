/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=update_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   Dynamic (automatic) validation is done before custom val.
   
   If there's no fieldmap (viewer) set the attribute on the browse object or the browse overlay object
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

DEF VAR cProfilnr AS CHAR NO-UNDO.
DEF VAR cVarenr   AS CHAR NO-UNDO.

FIND KOrdreLinje WHERE ROWID(KOrdreLinje) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL KOrdreLinje THEN DO:
  ASSIGN cProfilnr = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"cProfilnr")
         cVarenr   = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Varenr"). 
  IF cVarenr NE KOrdreLinje.VareNr OR DYNAMIC-FUNCTION("getCurrentAction" IN SOURCE-PROCEDURE) = "create" THEN DO:
    FIND FIRST ArtBas NO-LOCK
         WHERE ArtBas.ArtikkelNr = DECIMAL(cVarenr)
         NO-ERROR.
    IF AVAIL ArtBas THEN DO:
      KOrdreLinje.LevFargKod = ArtBas.LevFargKod.
      FIND FIRST ArtPris NO-LOCK
           OF ArtBas
           WHERE ArtPris.ProfilNr = INTEGER(cProfilnr)
           NO-ERROR.
      IF AVAIL ArtPris THEN
        KOrdreLinje.VareKost = ArtPris.VareKost[1].
    END.
    ELSE
      ASSIGN KOrdreLinje.LevFargKod = ""
             KOrdreLinje.VareKost   = 0
             .
  END.
END.


