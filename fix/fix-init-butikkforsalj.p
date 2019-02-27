FOR EACH Forsalj:
    FOR EACH Butiker WHERE Butiker.Butik < 20:
        CREATE ButikkForsalj.
        ASSIGN
            ButikkForsalj.Butik      = butiker.Butik
            ButikkForsalj.ForsNr     = Forsalj.ForsNr
            ButikkForsalj.KassererId = Forsalj.ForsNr
            .
    END.
END.
