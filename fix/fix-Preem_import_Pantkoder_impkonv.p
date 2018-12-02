DEF VAR cinnFilNavn AS CHAR NO-UNDO.
DEF VAR cutFilNavn  AS CHAR NO-UNDO.

DEF VAR cLinje AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR iAntRader AS INT NO-UNDO.
DEF VAR iAntRens  AS INT NO-UNDO.
DEF VAR cPantKode AS INT FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTabell     AS CHARACTER NO-UNDO. 

DEF STREAM inn.
DEF STREAM ut.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN 
  cEDB-System = 'AXFOOD'
  cTabell     = 'Pant'
  cinnFilNavn = 'C:\appdir\Pantartikler_01.csv'
  cutFilNavn  = 'C:\appdir\Pantartikler_01_Error.csv'
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
    cPantKode = TRIM(ENTRY(1,cLinje,";"))
    no-error.
  IF ERROR-STATUS:ERROR THEN NEXT LOOPEN.
  IF cPantKode = '' THEN NEXT LOOPEN.
  
  FIND FIRST impKonv WHERE
    ImpKonv.EDB-System = cEDB-System AND
    ImpKonv.Tabell     = cTabell     AND
    ImpKonv.InterntID  = '' AND 
    ImpKonv.EksterntId = cPantKode
    NO-ERROR.
  IF NOT AVAILABLE impKonv THEN
  DO:
    CREATE impKonv.
    ASSIGN 
    ImpKonv.EDB-System = cEDB-System 
    ImpKonv.Tabell     = cTabell
    ImpKonv.InterntID  = ''
    ImpKonv.EksterntID = cPantKode.
    
    iAntRens = iAntRens + 1.
    PUT STREAM ut UNFORMATTE cLinje SKIP.
  END.
END. /* LOOPEN */

OUTPUT stream ut close.
INPUT stream inn close.


MESSAGE iAntRens iAntRader VIEW-AS ALERT-BOX.
