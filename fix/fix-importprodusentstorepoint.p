/*
  TN 7/10-03
  rutine som importerer produsenter fra StorePoint.
*/

DEF VAR cLinje  AS CHAR NO-UNDO.
DEF VAR cFelt1  AS CHAR NO-UNDO.
DEF VAR cFelt2  AS CHAR NO-UNDO.
DEF VAR cFelt3  AS CHAR NO-UNDO.
DEF VAR cFelt4  AS CHAR NO-UNDO.

DEF VAR piLoop    AS INT NO-UNDO.
DEF VAR piMvaKode AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.

DEF STREAM InnFil.
DEF STREAM UtFil.

/* Renser alle produsent */
FOR EACH Produsent:
    DELETE Produsent.
END.

INPUT STREAM InnFil FROM VALUE("c:\home\lindbak\ankommet\Underprodusent_StorePoint.sdv") NO-ECHO.
LESFRAFIL:
REPEAT :
  IMPORT STREAM InnFil DELIMITER ";" 
      cFelt1 
      cFelt2 
      cFelt3 
      cFelt4 
      .
  ASSIGN
      cFelt3 = TRIM(cFelt3)
      .

  /* Oppretter Produsent */
  FIND Produsent EXCLUSIVE-LOCK WHERE
      Produsent.ProdNr = INT(cFelt1) NO-ERROR.
  IF NOT AVAILABLE Produsent THEN
  DO:
      CREATE Produsent.
      ASSIGN
          Produsent.ProdNr = INT(cFelt1)
          .
  END.
  ASSIGN
      Produsent.Beskrivelse = cFelt3
      .
END. /* LESFRAFIL */

OUTPUT STREAM UtFil CLOSE.
INPUT STREAM InnFil CLOSE.
