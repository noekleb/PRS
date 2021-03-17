def var cinnFilNavn as char no-undo.
def var cutFilNavn  as char no-undo.

def var cLinje as char format "x(200)" no-undo.
def var iAntRader as int no-undo.
def var iAntRens  as int no-undo.
def var iLevNr    as int format ">>>>>9" no-undo.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTabell     AS CHARACTER NO-UNDO. 

def stream inn.
def stream ut.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN 
  cEDB-System = 'MxKjede'
  cTabell     = 'levbas'
  cinnFilNavn = 'C:\Polygon\Kundejobber\mxSport\Innlesing av mappingtabell\Leverandorer_MxLaguneparken_2.csv'
  cutFilNavn  = 'C:\Polygon\Kundejobber\mxSport\Innlesing av mappingtabell\Leverandorer_MxLaguneparken_2_err.csv'
  .

input stream inn from value(cinnFilNavn).
output stream ut to value(cutFilNavn).

LOOPEN:
repeat:
  import stream Inn unformatted cLinje.
  iAntRader = iAntRader + 1.
  if iAntRader = 1 then next.
  IF NUM-ENTRIES(cLinje,';') < 3 THEN NEXT.
  IF TRIM(ENTRY(3,cLinje,";")) = '' THEN NEXT.
  
 
  if iAntRader modulo 10 = 0 then
  do:
    pause 0.
    display iAntRader iAntRens with frame g.
  end.
  
  /* Sjekker gyldighet på data - skipper ugyldige linjer*/
  assign
    iLevNr = int(entry(1,cLinje,";"))
    no-error.
  IF ERROR-STATUS:ERROR THEN NEXT LOOPEN.
  IF iLevNr = 0 THEN NEXT LOOPEN.
  
  FIND LevBas NO-LOCK WHERE
    LevBas.LevNr = iLevNr NO-ERROR.
  IF NOT AVAILABLE LevBas THEN
      PUT STREAM Ut cLinje SKIP.
  
  find first impKonv where
    ImpKonv.EDB-System = cEDB-System AND
    ImpKonv.Tabell     = cTabell     AND
    ImpKonv.InterntID  = STRING(iLevNr,'>>>>>9')
    no-error.
  if not available impKonv then
  do:
    create impKonv.
    assign 
    ImpKonv.EDB-System = cEDB-System 
    ImpKonv.Tabell     = cTabell     
    ImpKonv.InterntID  = TRIM(STRING(iLevNr,'>>>>>9'))
    ImpKonv.EksterntID = TRIM(ENTRY(3,cLinje,";"))
                         .
    
    iAntRens = iAntRens + 1.
    put stream ut unformatte cLinje skip.
  end.
end. /* LOOPEN */

output stream ut close.
input stream inn close.

message iAntRens iAntRader view-as alert-box.
