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

CURRENT-WINDOW:WIDTH = 250.

DEF STREAM InnFil.

INPUT STREAM InnFil FROM VALUE("C:\DB\mxold\Vg_mx-sport.csv") NO-ECHO.

LESFRAFIL:
REPEAT :
  CREATE VarGr.
  IMPORT STREAM InnFil DELIMITER ";" 
      ^
      ^
      ^
      ^
      VarGr.VgBeskr
      ^
      VarGr.Vg
      . 

  ASSIGN
      VarGr.Hg = int(SUBstring(STRING(VarGr.Vg,"999"),1,2))
      VarGr.MomsKod = 1
      VarGr.Kost_Proc = 65
      .

  DISPLAY
      VarGr.Vg
      VarGr.VgBeskr
      VarGr.Hg
      
      WITH WIDTH 248 USE-TEXT.

END. /* LESFRAFIL */

INPUT STREAM InnFil CLOSE.

