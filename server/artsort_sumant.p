DEF INPUT  PARAM irArtSort   AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR  NO-UNDO.

DEF VAR iAnt    AS INT NO-UNDO.

FIND ArtSort WHERE ROWID(ArtSort) = irArtSort NO-LOCK NO-ERROR.
IF AVAIL ArtSort THEN DO: 
  FIND FIRST LevSort OF ArtSort NO-LOCK NO-ERROR.
  IF AVAIL LevSort THEN DO:
    FOR EACH LevSAnt FIELDS(SoAnt) NO-LOCK
        OF LevSort
        BY SeqNr:
      iAnt = iAnt + SoAnt.
    END.
    
    ocValue = STRING(iAnt).
  END.
END.
