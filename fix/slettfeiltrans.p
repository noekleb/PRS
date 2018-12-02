def var x as int.
def stream ut.
output stream ut to translogg.err append.

for each Translogg where
  
  postert = false:
  
  x = x + 1.
  pause 0.
  display x with frame g.
  
  export stream ut translogg.
  delete translogg.
  
end.  
output stream ut close.
display x.
