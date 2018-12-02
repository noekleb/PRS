TRIGGER PROCEDURE FOR CREATE OF Medlem.



  DEF VAR trgMedlemsNr AS DEC NO-UNDO.

  {trg/c_w_trg.i &Type="C" &Fil="SkoTex.Medlem"}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genmedlemsnr.p (OUTPUT trgMedlemsNr).
      ASSIGN
        Medlem.MedlemsNr = trgMedlemsNr
        NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE LOOPEN.
  END.



