DEFINE VARIABLE iCl AS INT NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/*{syspara.i 5 1 1 iCl INT}.           */
/*FIND Butiker NO-LOCK WHERE           */
/*  Butiker.Butik = iCl NO-ERROR.      */

{syspara.i 50 65 4 cButLst}
{syspar2.i 50 65 4 cTekst}
IF cTekst <> '' THEN 
  cButLst = cButLst + ',' + cTekst.

PROCEDURE ArtBas_HarLager:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  ocValue = 'FALSE'.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    LAGERLOOP: 
    FOR EACH Lager OF ArtBas NO-LOCK:
      IF NOT CAN-DO(cButLst,STRING(Lager.Butik)) THEN 
        NEXT.
      IF Lager.LagAnt > 0 THEN
        DO:
          ocValue = 'TRUE'.
          LEAVE LAGERLOOP.
        END.
    END.  /* LAGERLOOP  */
  END.
  
END PROCEDURE. 
