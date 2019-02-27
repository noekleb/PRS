TRIGGER PROCEDURE FOR CREATE OF SkoTex.Forsalj.

  def var trgForsNr as DEC no-undo.

  {trg\c_w_trg.i &Fil=SkoTex.Forsalj &Type="C"}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genforsnr.p (OUTPUT trgForsNr).
      IF CAN-FIND(Forsalj WHERE
                  Forsalj.ForsNr = int(trgForsNr)) THEN
          NEXT loopen.
      ELSE DO:
          assign
            Forsalj.ForsNr = trgForsNr
            NO-ERROR.
          LEAVE LOOPEN.
      END.
  END.


