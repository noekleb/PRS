/* Fikser størrelsesplassering med space. */
for each StrTStr exclusive-lock:

 assign
    StrTStr.SoStorl = trim(StrTStr.SoStorl)
    StrTStr.SoStorl = if (length(StrTStr.SoStorl) = 1 or 
                          length(StrTStr.SoStorl) = 3
                         ) 
                        then " " + StrTStr.SoStorl
                        else StrTStr.SoStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(StrTStr.SoStorl,",") <> 0 then
    OVERLAY(StrTStr.SoStorl, index(StrTStr.SoStorl,","), 1, "CHARACTER") = ".".
end.                        
