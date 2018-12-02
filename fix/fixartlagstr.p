/* Fikser størrelsesplassering med space. */
for each ArtLag where
  ArtLag.Vg = 1 and
  ArtLAg.LopNr = 2221:

 assign
    Artlag.Storl = trim(Artlag.Storl)
    Artlag.Storl = if (length(Artlag.Storl) = 1 or 
                       length(Artlag.Storl) = 3
                      ) 
                     then " " + Artlag.Storl
                     else Artlag.Storl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(Artlag.Storl,",") <> 0 then
    OVERLAY(Artlag.Storl, index(Artlag.Storl,","), 1, "CHARACTER") = ".".
end.                        
