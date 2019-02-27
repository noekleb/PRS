TRIGGER PROCEDURE FOR CREATE OF SkoTex.Ordre.
  
  def var trgOrdreNr as INTE no-undo.

  {trg/c_w_trg.i &Type="C" &Fil="SkoTex.Ordre"}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genordrenr.p (OUTPUT trgOrdreNr).
      assign
        Ordre.OrdreNr = trgOrdreNr
        NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE LOOPEN.
  END.


