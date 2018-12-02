def var cFilNavn as char no-undo.
def var cLinje as char no-undo.
def var cEAN as char format "x(20)"no-undo.
def var cEAN2 as char format "x(20)" no-undo.
def var iAntTreff as int no-undo.
def var iAntFeil as int no-undo.

def stream Inn.
def stream Ut.

current-window:width = 250.

input stream Inn from value('Ean_lager_utpris.txt') no-echo.
output stream Ut to value('Varhaug_manko.txt').

repeat:
  import stream Inn unformatted cLinje . 
  
  assign
    cEAN = entry(1,clinje,';')
    cEAN2 = entry(1,cLinje,';')
    .
    
  run bibl_chkean.p (input-output cEAN2).

  if can-find(first VPIStrekkode where 
               VPIStrekkode.Kode = cEAN2)
    then iAntTreff = iAntTreff + 1.
  else do:
    iAntFeil = iAntFeil + 1.
    put stream Ut unformatted cLinje skip.
  end.

  /*
  display 
    cEAN length(cEAN) cEAN2 length(cEAN2) 
    can-find(first VPIStrekkode where 
             VPIStrekkode.Kode = cEAN2)
    with width 250.  
  */  
end.
output stream Ut close.
input stream Inn close.

display iAntTreff iAntFeil.
