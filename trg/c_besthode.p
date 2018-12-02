TRIGGER PROCEDURE FOR CREATE OF SkoTex.BestHode.
  
  def var trgBestNr as INTE no-undo.

  {trg/c_w_trg.i &Type="C" &Fil="SkoTex.BestHode"}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genbestnr.p (OUTPUT trgBestNr).
      assign
        BestHode.BestNr = trgBestNr
        NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE LOOPEN.
  END.


