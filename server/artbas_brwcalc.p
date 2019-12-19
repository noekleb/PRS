DEFINE VARIABLE iCl AS INT NO-UNDO.
/*DEFINE VARIABLE iUtvidetStatus AS INTEGER NO-UNDO.*/
DEFINE VARIABLE obOk AS LOG NO-UNDO.

/*{syspara.i 19 9 4 iUtvidetStatus INT}*/
/*{syspara.i 5 1 1 iCl INT}.           */
/*FIND Butiker NO-LOCK WHERE           */
/*  Butiker.Butik = iCl NO-ERROR.      */

PROCEDURE ArtBas_HarLager:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO: 
    IF CAN-FIND(FIRST Lager OF ArtBAs WHERE 
      Lager.LagAnt > 0) THEN 
      ocValue = 'TRUE'.
    ELSE 
      ocValue = 'FALSE'.
  END.  
  ELSE ocValue = ''.

END PROCEDURE. 
