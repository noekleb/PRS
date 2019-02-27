def var cFilNavn as char no-undo.

def stream Ut.

output stream Ut to value("plufile_5.rpt").

for each InfoPOS.Vare no-lock:
  put stream Ut unformatted
    InfoPOS.Vare.EAN
    skip.
    
end.
    
output stream Ut close.    
