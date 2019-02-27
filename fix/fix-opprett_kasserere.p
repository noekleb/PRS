/* Opprettelse av kasserere */

FOR EACH Kasse NO-LOCK:
  IF NOT CAN-FIND(FIRST Forsalj WHERE
                  Forsalj.ButikkNr = Kasse.ButikkNr) THEN
  DO:
      CREATE forsalj.
      ASSIGN
          Forsalj.ButikkNr     = Kasse.ButikkNr
          Forsalj.FoNamn       = 'Kasse ' + STRING(Kasse.KasseNr) + '/' + STRING(Kasse.ButikkNr)
          Forsalj.navnikasse   = 'Kasse ' + STRING(Kasse.KasseNr) + '/' + STRING(Kasse.ButikkNr)
          Forsalj.Passord      = Kasse.ButikkNr
          Forsalj.ForsaljAktiv = TRUE
          Forsalj.ButikkNr     = Kasse.ButikkNr
          .
  END.
  ELSE FIND FIRST Forsalj NO-LOCK WHERE
                  Forsalj.ButikkNr = Kasse.ButikkNr.

  FIND FIRST ButikkForsalj NO-LOCK WHERE
      ButikkForsalj.ForsNr = Forsalj.ForsNr AND
      ButikkForsalj.Butik  = Forsalj.ButikkNr NO-ERROR.
  IF NOT AVAILABLE ButikkForsalj THEN
  DO:
      CREATE ButikkForsalj.
      ASSIGN
          ButikkForsalj.ForsNr     = Forsalj.ForsNr
          ButikkForsalj.Butik      = Forsalj.ButikkNr
          ButikkForsalj.KassererId = Forsalj.ButikkNr
          .
  END.
END.
