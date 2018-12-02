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

INPUT STREAM InnFil FROM VALUE("VgSP.csv") NO-ECHO.
OUTPUT STREAM UtFil TO VALUE("VgSP1.csv") NO-ECHO.
LESFRAFIL:
REPEAT :
  IMPORT STREAM InnFil DELIMITER ";" cTekst1 cTekst2 cTekst3.
  ASSIGN
      cFelt1 = entry(1,cTekst1," ")
      cFelt2 = substring(cTekst1,INDEX(cTekst1," ") + 1)
      cFelt3 = entry(1,cTekst2," ")
      cFelt4 = substring(cTekst2,INDEX(cTekst2," ") + 1)
      cFelt5 = entry(1,cTekst3," ")
      cFelt6 = substring(cTekst3,INDEX(cTekst3," ") + 1)
      .

  PUT STREAM UtFil UNFORMATTED  
      cFelt1 ";"
      cFelt2 ";"
      cFelt3 ";"
      cFelt4 ";"
      cFelt5 ";"
      cFelt6      
      SKIP.

END. /* LESFRAFIL */

OUTPUT STREAM UtFil CLOSE.
INPUT STREAM InnFil CLOSE.
