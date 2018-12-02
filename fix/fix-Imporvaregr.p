/*
  
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
DEF VAR piHg    AS INT  NO-UNDO.
DEF VAR pcTekst AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

DEF STREAM InnFil.

INPUT STREAM InnFil FROM VALUE("C:\DB\mxold\Vg_mx-sport.csv") NO-ECHO.

LESFRAFIL:
REPEAT :
  IMPORT STREAM InnFil DELIMITER ";" 
      ^
      ^
      pcTekst
      piHg
      . 

  IF NOT CAN-FIND(huvGr WHERE
                  HuvGr.Hg = piHg) THEN
  DO:
      CREATE HuvGr.
      ASSIGN
          HuvGr.Hg         = piHg
          HuvGr.HgBesk     = pcTekst
          HuvGr.AvdelingNr = int(SUBstring(STRING(HuvGr.Hg,"99"),1,1))
          NO-ERROR.

      DISPLAY
          HuvGr.Hg
          HuvGr.HgBeskr
          WITH WIDTH 248 USE-TEXT.
  END.
END. /* LESFRAFIL */

INPUT STREAM InnFil CLOSE.

