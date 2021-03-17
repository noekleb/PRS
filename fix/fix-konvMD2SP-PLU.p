/*
  TN 7/10-03
  rutine som konverterer PLU liste til brukbart csv format.
*/

DEF VAR cLinje  AS CHAR NO-UNDO.
DEF VAR cFelt1  AS CHAR NO-UNDO.
DEF VAR cFelt2  AS CHAR NO-UNDO.
DEF VAR cFelt3  AS CHAR NO-UNDO.
DEF VAR cFelt4  AS CHAR NO-UNDO.
DEF VAR cFelt5  AS CHAR NO-UNDO.
DEF VAR cFelt6  AS CHAR NO-UNDO.

DEF VAR cTekst1 AS CHAR NO-UNDO.
DEF VAR cTekst2 AS CHAR NO-UNDO.
DEF VAR cTekst3 AS CHAR NO-UNDO.

DEF VAR piLoop  AS INT  NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.

DEF STREAM InnFil.
DEF STREAM UtFil.

INPUT STREAM InnFil FROM VALUE("pressbyran\MapPLU-org-konv.csv") NO-ECHO.
OUTPUT STREAM UtFil TO VALUE("pressbyran\MapPLU-org-konv.d") NO-ECHO.
LESFRAFIL:
REPEAT :
  IMPORT STREAM InnFil UNFORMATTED cLinje. 

  IF NUM-ENTRIES(cLinje,";") >= 3 THEN
  UTLEGG:
  DO piLoop = 3 TO NUM-ENTRIES(cLinje,";"):
      ASSIGN
          cFelt1 = trim(ENTRY(1,cLinje,";"))
          cFelt2 = trim(ENTRY(piLoop,cLinje,";"))
          cFelt3 = trim(ENTRY(2,cLinje,";"))
          .

      IF cFelt2 <> "" AND cFelt1 <> "" AND (cFelt1 <> cFelt2) THEN
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
  END. /* UTLEGG */
END. /* LESFRAFIL */

OUTPUT STREAM UtFil CLOSE.
INPUT STREAM InnFil CLOSE.
