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

DEF TEMP-TABLE tmpVarGr LIKE VarGr
    FIELD InterntId AS CHAR 
    INDEX Internt Vg
    .

INPUT STREAM InnFil FROM VALUE("pressbyran\vargrOLD.d") NO-ECHO.

LESFRAFIL:
REPEAT :
  CREATE tmpVarGr.
  IMPORT STREAM InnFil tmpVarGr. 

  /* Sjekker konvertering */
  FIND FIRST ImpKonv NO-LOCK WHERE
      ImpKonv.EDB-System = "MegaDisk" AND
      ImpKonv.Tabell     = "VarGr"     AND
      ImpKonv.EksterntId = string(tmpVarGr.Vg) NO-ERROR.
  IF AVAILABLE impKonv THEN
  DO:
      tmpVarGr.InterntId = ImpKonv.InterntId.
      FIND VarGr WHERE
          VarGr.Vg = INT(ImpKonv.InterntId) EXCLUSIVE-LOCK NO-ERROR.
  END.
  ELSE
      FIND VarGr EXCLUSIVE-LOCK WHERE
          VarGr.Vg = tmpVarGr.Vg NO-ERROR.

  DISPLAY
      tmpVarGr.Vg COLUMN-LABEL "OldVg"
      tmpVarGr.VgBeskr
      tmpVarGr.InterntId COLUMN-LABEL "NyVg"
      VarGr.VgBesk WHEN AVAILABLE VarGr
      "Ja" WHEN AVAILABLE VarGr
      "Nei" WHEN NOT AVAILABLE VarGr
      tmpVarGr.Kost_Proc
      WITH WIDTH 248 USE-TEXT.

  IF AVAILABLE VarGr THEN
      VarGr.Kost_Proc = tmpVarGr.Kost_Proc.

END. /* LESFRAFIL */

INPUT STREAM InnFil CLOSE.

