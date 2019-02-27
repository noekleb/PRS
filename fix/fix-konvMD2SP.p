/*
  TN 7/10-03
  rutine som konverterer til brukbart csv format.
*/

DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cFelt1 AS CHAR NO-UNDO.
DEF VAR cFelt2 AS CHAR NO-UNDO.
DEF VAR cFelt3 AS CHAR NO-UNDO.
DEF VAR cFelt4 AS CHAR NO-UNDO.
DEF VAR cFelt5 AS CHAR NO-UNDO.
DEF VAR cFelt6 AS CHAR NO-UNDO.

DEF VAR cTekst1 AS CHAR NO-UNDO.
DEF VAR cTekst2 AS CHAR NO-UNDO.
DEF VAR cTekst3 AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.

DEF STREAM InnFil.
DEF STREAM UtFil.

INPUT STREAM InnFil FROM VALUE("ImpKonv-DivPluStrekkode.csv") NO-ECHO.
OUTPUT STREAM UtFil TO VALUE("ImpKonv-DivPluStrekkode.d") NO-ECHO.
LESFRAFIL:
REPEAT :
  IMPORT STREAM InnFil DELIMITER ";" 
      cTekst1 
      cTekst2
      cTekst3
      .
  ASSIGN
      cFelt1 = trim(cTekst1)
      cFelt2 = trim(cTekst2)
      cFelt3 = trim(cTekst3)
      .

  EXPORT STREAM UtFil
      ?
      0
      " "
      ?
      0
      " "
      "MegaDisk"
      "Strekkode"
      cFelt1 
      cFelt2 
      cFelt3
      .
/* ? 0 " " ? 0 " " "MegaDisk" "Strekkode" "1020" "31" "SL 30 DGR HELT PRIS" */

END. /* LESFRAFIL */

OUTPUT STREAM UtFil CLOSE.
INPUT STREAM InnFil CLOSE.
