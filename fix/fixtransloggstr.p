/* Fikser størrelsesplassering med space. */
for each TransLogg exclusive-lock:

 assign
    TransLogg.Storl = trim(TransLogg.Storl)
    TransLogg.Storl = if (length(TransLogg.Storl) = 1 or 
                          length(TransLogg.Storl) = 3
                         ) 
                        then " " + TransLogg.Storl
                        else TransLogg.Storl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(TransLogg.Storl,",") <> 0 then
    OVERLAY(TransLogg.Storl, index(TransLogg.Storl,","), 1, "CHARACTER") = ".".
end.                        
