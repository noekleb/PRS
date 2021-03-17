FOR EACH KassererOppgj:
    FIND ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.Butik = KassererOppgj.ButikkNr AND
        ButikkForsalj.KassererId = KassererOppgj.KassererNr NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN
        KassererOppgj.KassererNr = ButikkForsalj.ForsNr.
END.

FOR EACH KassererBilag:
    FIND ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.Butik = KassererBilag.ButikkNr AND
        ButikkForsalj.KassererId = KassererBilag.KassererNr NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN
        KassererBilag.KassererNr = ButikkForsalj.ForsNr.
END.

FOR EACH KassererKontanter:
    FIND ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.Butik = KassererKontanter.ButikkNr AND
        ButikkForsalj.KassererId = KassererKontanter.KassererNr NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN
        KassererKontanter.KassererNr = ButikkForsalj.ForsNr.
END.

FOR EACH KassererValuta:
    FIND ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.Butik = KassererValuta.ButikkNr AND
        ButikkForsalj.KassererId = KassererValuta.KassererNr NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN
        KassererValuta.KassererNr = ButikkForsalj.ForsNr.
END.
