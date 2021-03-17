def var cinnFilNavn as char no-undo.
def var cutFilNavn  as char no-undo.

def var cLinje as char format "x(200)" no-undo.
def var iAntRader as int no-undo.
def var iAntRens  as int no-undo.
DEFINE VARIABLE cEkstStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIntStr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTabell     AS CHARACTER NO-UNDO. 

def stream inn.
def stream ut.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN 
  cEDB-System = 'Sport'
  cTabell     = 'strkonv'
  cinnFilNavn = 'C:\home\lindbak\ankommet\Sport1_StrKonv.csv'
  cutFilNavn  = 'C:\home\lindbak\ankommet\Sport1_StrKonv_err.csv'
  .

input stream inn from value(cinnFilNavn).
output stream ut to value(cutFilNavn).

LOOPEN:
repeat:
  import stream Inn unformatted cLinje.
  iAntRader = iAntRader + 1.
  if iAntRader = 1 then next.
  IF NUM-ENTRIES(cLinje,';') < 2 THEN NEXT.
  IF TRIM(ENTRY(2,cLinje,";")) = '' THEN NEXT.
 
  if iAntRader modulo 10 = 0 then
  do:
    pause 0.
    display iAntRader iAntRens with frame g.
  end.
  
  find first impKonv where
    ImpKonv.EDB-System = cEDB-System AND
    ImpKonv.Tabell     = cTabell     AND
    ImpKonv.InterntID  = TRIM(ENTRY(2,cLinje,";"))
    no-error.
  if not available impKonv then
  do:
    create impKonv.
    assign 
    ImpKonv.EDB-System = cEDB-System 
    ImpKonv.Tabell     = cTabell     
    ImpKonv.InterntID  = TRIM(ENTRY(2,cLinje,";"))
    ImpKonv.EksterntID = TRIM(ENTRY(1,cLinje,";")).
    
    iAntRens = iAntRens + 1.
    put stream ut unformatte cLinje skip.
  end.
end. /* LOOPEN */

output stream ut close.
input stream inn close.

message iAntRens iAntRader view-as alert-box.
