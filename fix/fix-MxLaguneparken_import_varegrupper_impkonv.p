def var cinnFilNavn as char no-undo.
def var cutFilNavn  as char no-undo.

def var cLinje as char format "x(200)" no-undo.
def var iAntRader as int no-undo.
def var iAntRens  as int no-undo.
def var iVg as int format ">>>>>9" no-undo.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTabell     AS CHARACTER NO-UNDO. 

def stream inn.
def stream ut.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN 
  cEDB-System = 'MxKjede'
  cTabell     = 'vargr'
  cinnFilNavn = 'C:\Polygon\Kundejobber\mxSport\Innlesing av mappingtabell\Varegrupper_MxLaguneparken_2.csv'
  cutFilNavn  = 'C:\Polygon\Kundejobber\mxSport\Innlesing av mappingtabell\Varegrupper_MxLaguneparken_2_err.csv'
  .

input stream inn from value(cinnFilNavn).
output stream ut to value(cutFilNavn).

LOOPEN:
repeat:
  import stream Inn unformatted cLinje.
  iAntRader = iAntRader + 1.
  if iAntRader = 1 then next.
  IF TRIM(ENTRY(7,cLinje,";")) = '' THEN NEXT.
  
 
  if iAntRader modulo 10 = 0 then
  do:
    pause 0.
    display iAntRader iAntRens with frame g.
  end.
  
  /* Sjekker gyldighet på data - skipper ugyldige linjer*/
  assign
    iVg = int(entry(5,cLinje,";"))
    no-error.
  IF ERROR-STATUS:ERROR THEN NEXT LOOPEN.
  IF iVg = 0 THEN NEXT LOOPEN.
  
  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = iVg NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
      PUT STREAM Ut cLinje SKIP.
  
  find first impKonv where
    ImpKonv.EDB-System = cEDB-System AND
    ImpKonv.Tabell     = cTabell     AND
    ImpKonv.InterntID  = STRING(iVg,'>>9999')
    no-error.
  if not available impKonv then
  do:
    create impKonv.
    assign 
    ImpKonv.EDB-System = cEDB-System 
    ImpKonv.Tabell     = cTabell     
    ImpKonv.InterntID  = TRIM(STRING(iVg,'>>9999'))
    ImpKonv.EksterntID = TRIM(ENTRY(7,cLinje,";"))
                         .
    
    iAntRens = iAntRens + 1.
    put stream ut unformatte cLinje skip.
  end.
end. /* LOOPEN */

output stream ut close.
input stream inn close.

message iAntRens iAntRader view-as alert-box.
