DEF VAR cinnFilNavn AS CHAR NO-UNDO.
DEF VAR cutFilNavn  AS CHAR NO-UNDO.

DEF VAR cLinje AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR iAntRader AS INT NO-UNDO.
DEF VAR iAntRens  AS INT NO-UNDO.
DEF VAR iVarGr AS INT FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTabell     AS CHARACTER NO-UNDO. 

DEF STREAM inn.
DEF STREAM ut.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN 
  cEDB-System = 'AXFOOD'
  cTabell     = 'VarGr'
  cinnFilNavn = 'C:\Home\Lindbak\ANKOMMET\Varugrupp Axfood-Preem HG VG Visma 101125.csv'
  cutFilNavn  = 'C:\Home\Lindbak\ANKOMMET\Varugrupp Axfood-Preem HG VG Visma 101125_Error.csv'
  .

INPUT stream inn from value(cinnFilNavn).
OUTPUT stream ut to value(cutFilNavn).

LOOPEN:
REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje.
  iAntRader = iAntRader + 1.
  IF iAntRader = 1 THEN NEXT.
  
 
  IF iAntRader MODULO 10 = 0 THEN
  DO:
    PAUSE 0.
    DISPLAY iAntRader iAntRens WITH FRAME g.
  END.
  
  /* Sjekker gyldighet på data - skipper ugyldige linjer*/
  ASSIGN
    iVarGr = int(ENTRY(3,cLinje,";"))
    no-error.
  IF ERROR-STATUS:ERROR THEN NEXT LOOPEN.
  IF iVarGr = 0 THEN NEXT LOOPEN.
  
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = iVarGr NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
      PUT STREAM Ut cLinje SKIP.
  
  FIND FIRST impKonv WHERE
    ImpKonv.EDB-System = cEDB-System AND
    ImpKonv.Tabell     = cTabell     AND
    ImpKonv.InterntID  = STRING(iVarGr,'>>9999')
    NO-ERROR.
  IF NOT AVAILABLE impKonv THEN
  DO:
    CREATE impKonv.
    ASSIGN 
    ImpKonv.EDB-System = cEDB-System 
    ImpKonv.Tabell     = cTabell
    ImpKonv.InterntID  = TRIM(STRING(iVarGr,'>>9999'))
    ImpKonv.EksterntID = TRIM(ENTRY(10,cLinje,";"))
                         .
    
    iAntRens = iAntRens + 1.
    PUT STREAM ut UNFORMATTE cLinje SKIP.
  END.
  ELSE 
    ASSIGN
      ImpKonv.InterntID  = TRIM(STRING(iVarGr,'>>9999'))
      ImpKonv.EksterntID = TRIM(ENTRY(10,cLinje,";"))
      .
END. /* LOOPEN */

OUTPUT stream ut close.
INPUT stream inn close.


MESSAGE iAntRens iAntRader VIEW-AS ALERT-BOX.
