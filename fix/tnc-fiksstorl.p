FOR EACH KundeTrans:
    IF INDEX(KundeTrans.Storl,",") <> 0 THEN
    DO:
        RUN FixStorl(INPUT-OUTPUT KundeTrans.Storl).
        DISPLAY KundeTrans.Storl.
    END.
END.

PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

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

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  if NUM-ENTRIES(wStorl,".") = 2 then
    DO:
      if NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) then
        wStorl = ENTRY(1,wStorl,".").
    END.

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

