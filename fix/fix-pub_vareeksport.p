DEFINE VAR cLanButiker     AS CHARACTER  NO-UNDO.
DEFINE VAR cFtpButiker     AS CHARACTER  NO-UNDO.
DEFINE VAR cVareFiler      AS CHARACTER  NO-UNDO.
DEFINE VAR cMixFiler       AS CHARACTER  NO-UNDO.
DEFINE VAR iAntVarer       AS INTEGER    NO-UNDO.
DEFINE VAR iAntPakker      AS INTEGER    NO-UNDO.
DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
  
  LOOPEN:
  FOR EACH ArtBas /*WHERE ArtBas.LevNr = 10*/:
    FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    IF ArtPris.VareKost[1] = 0 OR ArtPris.VareKost[1] = ? THEN NEXT.
    IF ArtPris.Pris[1] = 0 OR ArtPris.Pris[1] = ? THEN NEXT.

    piAnt = piAnt + 1.
    IF piAnt > 500 THEN LEAVE LOOPEN.
  
    /* Flagger artikkelen for utlegg. */
    ArtBas.ETid = TIME.
  END. /* LOOPEN */

ASSIGN
  cLanButiker = '1'
  cFtpButiker = ''.
RUN eksportPUBArtBas.p (INPUT cLanButiker,INPUT cFtpButiker,OUTPUT cVareFiler,OUTPUT cMixFiler,OUTPUT iAntVarer,OUTPUT iAntPakker).
