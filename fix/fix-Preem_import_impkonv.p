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
  cEDB-System = 'Menigo'
  cTabell     = 'vargr'
  cinnFilNavn = 'C:\ArkivDokument\Polygon Software\Kunder & Prospects\Sverige\Kunder\Preem AB\Prosjekter\Prosjekt VPI håndtering\Varegruppe konvertering\Menigo varuklasser+InfoPos Vgr.csv'
  cutFilNavn  = 'C:\ArkivDokument\Polygon Software\Kunder & Prospects\Sverige\Kunder\Preem AB\Prosjekter\Prosjekt VPI håndtering\Varegruppe konvertering\Menigo_mangler_varegruppe.csv'
  .

input stream inn from value(cinnFilNavn).
output stream ut to value(cutFilNavn).

LOOPEN:
repeat:
  import stream Inn unformatted cLinje.
  iAntRader = iAntRader + 1.
  if iAntRader = 1 then next.
  
 
  if iAntRader modulo 10 = 0 then
  do:
    pause 0.
    display iAntRader iAntRens with frame g.
  end.
  
  /* Sjekker gyldighet på data - skipper ugyldige linjer*/
  assign
    iVg = int(entry(6,cLinje,";"))
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
    ImpKonv.EksterntID = TRIM(ENTRY(1,cLinje,";") + 
                         string(int(ENTRY(2,cLinje,';')),'999') +
                         string(int(ENTRY(3,cLinje,';')),'99') +
                         string(int(ENTRY(4,cLinje,';')),'99'))
                         .
    
    iAntRens = iAntRens + 1.
    put stream ut unformatte cLinje skip.
  end.
end. /* LOOPEN */

output stream ut close.
input stream inn close.

message iAntRens iAntRader view-as alert-box.
