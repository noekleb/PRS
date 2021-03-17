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

DEF VAR fArtikkelnr AS DEC  NO-UNDO.
DEF VAR iLnr        AS INT  NO-UNDO INIT 1.
DEF VAR cStorl      AS CHAR NO-UNDO.
DEF VAR iStr        AS INT  NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR iAntall     AS INT  NO-UNDO.

DEF BUFFER bPlListeLinje FOR PlListeLinje.

FIND PlListeLinje WHERE ROWID(PlListeLinje) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL PlListeLinje AND DYNAMIC-FUNCTION("getCurrentAction" IN SOURCE-PROCEDURE) = "Create" THEN DO:
  fArtikkelnr = DEC(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"ArtikkelNr")).  
  IF fArtikkelnr = ? THEN RETURN.
  ASSIGN cStorl  = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Storl")
         iAntall = INTEGER(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Antall"))
         .

  FIND FIRST PlListeHode NO-LOCK 
       OF PlListeLinje
       NO-ERROR.
  IF NOT AVAIL PlListeHode THEN DO:
    ocReturn = "Finner ikke ordreforslagshode".
    RETURN.
  END.

  FIND FIRST ArtBas NO-LOCK
       WHERE ArtBas.ArtikkelNr = fArtikkelnr
       NO-ERROR.
  IF NOT AVAIL ArtBas THEN
    ocReturn = "Ugyldig artikkelnr".
  ELSE DO:
    FOR EACH bPlListeLinje NO-LOCK
        WHERE bPlListeLinje.PlListeId = PlListeHode.PlListeId
        BY bPlListeLinje.PlLinjenr DESC:
      iLnr = bPlListeLinje.PlLinjenr + 1.
      LEAVE.
    END.

    FIND FIRST StrType NO-LOCK 
         OF ArtBas
         NO-ERROR.
    IF NOT AVAIL StrType THEN DO:
      ocReturn = "Artikkel mangler størrelsestype".
      RETURN.
    END.

    DO ix = 0 TO 3:
      iStr = INTEGER(ENTRY(LOOKUP(FILL(" ",ix) + cStorl,StrType.AlfaFordeling),StrType.Fordeling)) NO-ERROR.
      IF iStr NE 0 THEN LEAVE.
    END.
            
    BUFFER-COPY ArtBas TO PlListeLinje.

    ASSIGN PlListeLinje.StrKode = iStr
           PlListeLinje.PlLinjenr = iLnr
           PlListeLinje.AntallPlukket = iAntall
           .

  END.
END.


