def var cinnFil as char no-undo.
def var iLevNr1 as int no-undo.
def var iLevNr2 as int no-undo.
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
    iLevNr1
    iLevNr2
    cLevNamn
    .
    
  find LevBas no-lock where
    LevBas.LevNr = iLevNr1 no-error.
      
  display
    iLevNr1
    iLevNr2
    levbas.LevNamn when available LevBas
    cLevNamn
    with width 250.
    
  put stream Ut unformatted
    'run ByttLevNr(' + string(iLevNr2) + ', ' + string(iLevNr1) + ').'
    skip.  
end.

output stream Ut close.
input stream Inn close.
