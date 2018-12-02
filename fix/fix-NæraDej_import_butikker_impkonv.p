DEF VAR cinnFilNavn AS CHAR NO-UNDO.
DEF VAR cutFilNavn  AS CHAR NO-UNDO.

DEF VAR cLinje AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR iAntRader AS INT NO-UNDO.
DEF VAR iAntRens  AS INT NO-UNDO.
DEF VAR iButikkNr AS INT FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTabell     AS CHARACTER NO-UNDO. 

DEF STREAM inn.
DEF STREAM ut.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN 
  cEDB-System = 'NæraDej'
  cTabell     = 'Butiker'
  cinnFilNavn = 'C:\Home\Lindbak\ANKOMMET\KundelistaNæraDej25052012.csv'
  cutFilNavn  = 'C:\Home\Lindbak\ANKOMMET\KundelistaNæraDej25052012_log.csv'
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
    iButikkNr = int(ENTRY(2,cLinje,";"))
    no-error.
  IF ERROR-STATUS:ERROR THEN NEXT LOOPEN.
  IF iButikkNr = 0 THEN NEXT LOOPEN.
  
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButikkNr NO-ERROR.
  IF NOT AVAILABLE Butiker THEN
  DO:
      PUT STREAM Ut cLinje SKIP.
      CREATE Butiker.
      ASSIGN
          Butiker.Butik = INT(ENTRY(2,cLinje,";"))
          Butiker.ButNamn = ENTRY(3,cLinje,";")
          Butiker.ButFirmaNavn = ENTRY(5,cLinje,";")
          Butiker.EksterntId = ENTRY(4,cLinje,";")
          Butiker.BuAdr = ENTRY(6,cLinje,";")
          Butiker.BuPoNr = ENTRY(7,cLinje,";")
          Butiker.BuPAdr = ENTRY(8,cLinje,";")
          Butiker.OrganisasjonsNr = ENTRY(9,cLinje,";")
          Butiker.BuKon = ENTRY(10,cLinje,";")
          Butiker.BuTel = ENTRY(11,cLinje,";")
          Butiker.ePostAdresse = ENTRY(12,cLinje,";")
          Butiker.ApningsDato = 01/01/2011
          Butiker.harButikksystem = TRUE
          Butiker.CL = 100
          .
  END.
  
  FIND FIRST impKonv WHERE
    ImpKonv.EDB-System = cEDB-System AND
    ImpKonv.Tabell     = cTabell     AND
    ImpKonv.InterntID  = STRING(iButikkNr,'>>9999')
    NO-ERROR.
  IF NOT AVAILABLE impKonv THEN
  DO:
    CREATE impKonv.
    ASSIGN 
    ImpKonv.EDB-System = cEDB-System 
    ImpKonv.Tabell     = cTabell
    ImpKonv.InterntID  = TRIM(STRING(iButikkNr,'>>9999'))
    ImpKonv.EksterntID = TRIM(ENTRY(4,cLinje,";"))
                         .
    
    iAntRens = iAntRens + 1.
    PUT STREAM ut UNFORMATTE cLinje SKIP.
  END.
  ELSE 
    ASSIGN
      ImpKonv.InterntID  = TRIM(STRING(iButikkNr,'>>9999'))
      ImpKonv.EksterntID = TRIM(ENTRY(4,cLinje,";"))
      .
END. /* LOOPEN */

OUTPUT stream ut close.
INPUT stream inn close.


MESSAGE iAntRens iAntRader VIEW-AS ALERT-BOX.
