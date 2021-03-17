def var cinnFilNavn as char no-undo.
def var cutFilNavn  as char no-undo.

def var cLinje as char format "x(200)" no-undo.
def var iAntRader as int no-undo.
def var iAntRens  as int no-undo.
def var fArtikkelNr as dec format ">>>>>>>>>>>>9" no-undo.

def stream inn.
def stream ut.

current-window:width = 50.

assign
  cinnFilNavn = 'C:\Polygon\PRS\Anton_Artikkler_sport1_Ulik.csv'
  cutFilNavn  = 'C:\Polygon\PRS\Anton_Artikkler_sport1_Ulik_renset.csv'
  .

def temp-table tmpArtikkel
  field ArtikkelNr like ArtBas.ArtikkelNr
  index Art ArtikkelNr.
  
input stream inn from value(cinnFilNavn).
output stream ut to value(cutFilNavn).

repeat:
  import stream Inn unformatted cLinje.
  iAntRader = iAntRader + 1.
  if iAntRader = 1 then next.
  
 
  if iAntRader modulo 1000 = 0 then
  do:
    pause 0.
    display iAntRader iAntRens with frame g.
  end.
  
  assign
    fArtikkelNr = dec(entry(1,cLinje,";")).
  
  find first tmpArtikkel where
    tmpArtikkel.ArtikkelNr = fArtikkelNr no-error.
  if not available tmpArtikkel then
  do:
    create tmpArtikkel.
    assign tmpArtikkel.ArtikkelNr = fArtikkelNr.
    iAntRens = iAntRens + 1.
    put stream ut unformatte cLinje skip.
  end.
end.

output stream ut close.
input stream inn close.

message iAntRens iAntRader view-as alert-box.
