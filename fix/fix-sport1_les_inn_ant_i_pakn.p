current-window:width = 250.

def var cFilNavn as char no-undo.
def var cLinje   as char format "x(100)" no-undo.

def var dArtikkelNr as dec format "9999999999999" no-undo.
def var dAntIPakn as dec no-undo.

def stream Inn.


assign
  cFilNavn = 'c:\polygon\prs\Antipakn.csv'.
  
input stream Inn from value(cFilNavn) no-echo.

repeat:
  import stream Inn unformatted cLinje.
  
  assign
    dArtikkelNr = dec(entry(2,cLinje,';'))
    dAntIPakn   = dec(entry(8,cLinje,';'))
    no-error.
  if error-status:error then next.
  
  find ArtBas exclusive-lock where
    ArtBas.ArtikkelNr = dArtikkelNr no-error.
  if available ArtBas then
    ArtBas.AntIPakn = dAntIPakn.
  
  display 
    ArtBas.ArtikkelNr when available ArtBas
    dArtikkelNr
    dAntIPakn
    cLinje
  with width 250.
end.


input stream Inn close.
