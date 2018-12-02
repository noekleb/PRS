FOR EACH ArtBas:
    FIND ArtPris WHERE 
         ArtPris.ArtikkelNr = ArtBas.ARtikkelNr and
         ArtPris.ProfilNR = 1 NO-LOCK NO-ERROR.
    IF ArtPris.TilbudFraDato <> ? THEN
        ASSIGN
          ArtBas.SattPaKampanje = ArtPris.TilbudFraDato.

    FIND LAST HPrisKo NO-LOCK WHERE
        HPrisKo.ArtikkelNR = ArtBas.ArtikkelNr AND
        HPrisKo.ProfilNR = 1 and
        HPrisKo.TYPE = 2 /* Tilbud */ NO-ERROR.
    IF AVAILABLE HPrisKo THEN
    DO:
        IF HPrisKo.AktiveresDato <> ? THEN
        DO:
            IF ArtBas.SattPaKampanje = ?  THEN
                 ArtBas.SattPaKampanje = HPrisKo.AktiveresDato.
            ELSE
             IF HPrisKo.AktiveresDato < ArtBas.SattPaKampanje THEN
                 ArtBas.SattPaKampanje = HPrisKo.AktiveresDato.
        END.
    END.

END.
