/*
  TN 7/10-03
  rutine som importerer varestruktur fra StorePoint.
*/

DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cFelt1 AS CHAR NO-UNDO.
DEF VAR cFelt2 AS CHAR NO-UNDO.
DEF VAR cFelt3 AS CHAR NO-UNDO.
DEF VAR cFelt4 AS CHAR NO-UNDO.
DEF VAR cFelt5 AS CHAR NO-UNDO.
DEF VAR cFelt6 AS CHAR NO-UNDO.

DEF VAR piLoop AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.

DEF STREAM InnFil.
DEF STREAM UtFil.

/* Renser alle leverandører */
FOR EACH LevBas:
    DELETE LevBas.
END.

INPUT STREAM InnFil FROM VALUE("LeverandorStorePoint.csv") NO-ECHO.
LESFRAFIL:
REPEAT :
  IMPORT STREAM InnFil DELIMITER ";" 
      cFelt1 
      cFelt2 
      .

  /* Sjekk på gyldig avdeling */
  IF INT(cFelt1) = 0 THEN
  DO:
      MESSAGE 
          cFelt1 
          cFelt2 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      NEXT LESFRAFIL.
  END.
  /* Oppretter Avdeling */
  FIND LevBas NO-LOCK WHERE
      LevBas.LevNr = INT(cFelt1) NO-ERROR.
  IF NOT AVAILABLE LevBas THEN
  DO:
      CREATE LevBas.
      ASSIGN
          LevBas.LevNr   = INT(cFelt1)
          LevBas.LevNamn = cFelt2
          .
  END.

END. /* LESFRAFIL */

OUTPUT STREAM UtFil CLOSE.
INPUT STREAM InnFil CLOSE.
