
for each StrTStr:
  run FiksStorl(input-output StrTStr.SoStorl).

end.



/* Fikser størrelsen */
PROCEDURE FiksStorl:
  def INPUT-output parameter wStorl as char NO-UNDO.

 assign
    wStorl = trim(wStorl)
    wStorl = caps(wStorl)
    wStorl = if (length(wStorl) = 1 or 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.
