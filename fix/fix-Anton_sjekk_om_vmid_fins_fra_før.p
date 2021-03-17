def var cinnFil as char no-undo.
def var iVmId1 as int no-undo.
def var iVmId2 as int no-undo.
def var cLevNamn as char format "x(30)" no-undo.

current-window:width = 250.
assign
  cinnFil = 'C:\appdir\AntonKonv\Mapping av regsitre fra David\Anton Leverandører.csv'
  .

def stream Inn.
def stream Ut.

input stream Inn from value(cInnFil).
output stream Ut to value('tn.txt').

repeat:
  import stream Inn delimiter ';'
    iVmId1
    iVmId2
    .
    
  display
    iVmId1
    iVmId2
    with width 250.
    
  put stream Ut unformatted
    'run ByttVmId(' + string(iVmId2) + ', ' + string(iVmId1) + ').'
    skip.  
end.

output stream Ut close.
input stream Inn close.
