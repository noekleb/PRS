/*
  TN 5/11-03
  Import av gammel varegruppetabell fra Pressbyrån.
  Det eneste som skal inn er kost_proc. Allt annet skal ligge som før.
  Importen må imidlertid gå via konverteringsregisteret.
  
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

DEF TEMP-TABLE tmpVarGr 
    FIELD Hg AS CHAR
    FIELD Vg AS CHAR
    FIELD VgTekst AS CHAR 
    .

INPUT STREAM InnFil FROM VALUE("C:\ArkivDokument\LRS\Kunder & Prospects\Norge\Kunder\Hjem & Hobby\DataOppsettAvDB\Varegrupper.csv") NO-ECHO.

LESFRAFIL:
REPEAT :
  CREATE tmpVarGr.
  IMPORT STREAM InnFil DELIMITER ";" tmpVarGr. 

  CREATE VarGr.
  ASSIGN
      VarGr.Hg        = INT(tmpVarGr.Hg)
      VarGr.Vg        = (INT(tmpVarGr.Hg) * 100) + int(tmpVarGr.Vg)
      VarGr.VgBeskr   = tmpVarGr.VgTekst
      VarGr.Kost_proc = 63.00
      VarGr.MomsKod   = 1
      .
END. /* LESFRAFIL */

INPUT STREAM InnFil CLOSE.

