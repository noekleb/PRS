TRIGGER PROCEDURE FOR CREATE OF MedRabSjekk.

  def var trgRabSjekkId as DEC no-undo.

  {trg/c_w_trg.i &Type="C" &Fil="MedRabSjekk"}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genRabSjekkId.p (OUTPUT trgRabSjekkId).
      assign
        MedRabSjekk.RabSjekkId = trgRabSjekkId
        NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE LOOPEN.
  END.


