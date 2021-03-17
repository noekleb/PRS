/* Samling av alle prosedyrer som kalkulerer verdier for varebok
   Opprettet: 16.06.09 av TN
-------------------------------------------------------------------*/   

DEFINE VARIABLE iCL       AS INTEGER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.
       
  iCL = INTEGER((DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1"))).
  IF iCL > 0 THEN
    iProfilNr = INTEGER((DYNAMIC-FUNCTION("getFieldValues","Butiker",
                            "WHERE Butik = '" + STRING(iCL) + "'","ProfilNr"))).
  IF iProfilNr = ? OR iProfilNr = 0 THEN iProfilNr = 1.                                                                
                                 
PROCEDURE artbas_sok_vglopnr:
  DEF INPUT PARAM irArtBas       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  DEF VARIABLE iVg    AS INTEGER NO-UNDO.
  DEF VARIABLE iLopNr AS INTEGER NO-UNDO.

  FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
    ocReturn = ''.
  END.
  ELSE DO:
    ASSIGN
      iVg    = ArtBas.Vg
      iLopNr = ArtBAs.LopNr.
    
    IF iLopNr = ? THEN iLopNr = 0.
    ocReturn = STRING(iVg) + '/' + STRING(iLopNr).
  END.

  IF ocReturn = ? THEN ocReturn = "".
END PROCEDURE.

PROCEDURE artbas_sok_avdeling:
  DEF INPUT PARAM irArtBas       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  IF NOT AVAILABLE ArtBas AND ROWID(ArtBAs) <> irArtBas THEN
    FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
    ocReturn = ''.
  END.
  ELSE DO:
    FIND HuvGr OF ArtBAs NO-LOCK NO-ERROR.
    IF AVAILABLE HuvGr THEN
      FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
    ocReturn = (IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE '').
  END.

  IF ocReturn = ? THEN ocReturn = "".
END PROCEDURE.

PROCEDURE artpris_varekost:
  DEF INPUT PARAM irArtBas       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  IF NOT AVAILABLE ArtBas AND ROWID(ArtBas) <> irArtBas THEN
    FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
    ocReturn = ''.
  END.
  ELSE DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = iProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
      FIND FIRST ArtPris OF ArtBas NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocReturn = STRING(ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1]).
      
  END.

  IF ocReturn = ? THEN ocReturn = "".
END PROCEDURE.

PROCEDURE artpris_pris:
  DEF INPUT PARAM irArtBas       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  IF NOT AVAILABLE ArtBas AND ROWID(ArtBas) <> irArtBas THEN
    FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
    ocReturn = ''.
  END.
  ELSE DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = iProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
      FIND FIRST ArtPris OF ArtBas NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocReturn = STRING(ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]).
      
  END.

  IF ocReturn = ? THEN ocReturn = "".
END PROCEDURE.

PROCEDURE artpris_tilbud:
  DEF INPUT PARAM irArtBas       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  IF NOT AVAILABLE ArtBas AND ROWID(ArtBas) <> irArtBas THEN
    FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
    ocReturn = ''.
  END.
  ELSE DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = iProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
      FIND FIRST ArtPris OF ArtBas NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocReturn = IF ArtPris.Tilbud THEN '*' ELSE ''.
      
  END.

  IF ocReturn = ? THEN ocReturn = "".
END PROCEDURE.

PROCEDURE artbas_sanertdato:
  DEF INPUT PARAM irArtBas       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  IF NOT AVAILABLE ArtBas AND ROWID(ArtBas) <> irArtBas THEN
    FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
    ocReturn = ''.
  END.
  ELSE DO:
      ocReturn = IF ArtBas.SanertDato <> ? THEN string(ArtBas.SanertDato) ELSE ''.
  END.

  IF ocReturn = ? THEN ocReturn = "".
END PROCEDURE.
