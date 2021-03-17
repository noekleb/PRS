FOR EACH ArtBAs WHERE
    ArtBAs.LevNr = 38 AND
    ArtBAs.Vg = 4220:

    IF ArtBas.BildNR <> 0 THEN
    DO:
        IF ArtBAs.BildNR <> INT(ArtBas.ArtikkelNr) THEN
        BLOKK1:
        DO:
            FIND BildeRegister WHERE
                BildeRegister.BildNr = ArtBas.BildNr NO-ERROR.
            IF NOT AVAILABLE BildeRegister THEN
            DO:
                ArtBas.BildNr = 0.
                LEAVE BLOKK1.
            END.
            ELSE DO:
                FOR EACH BildeData OF BildeRegister:
                    BildeData.BildNr = int(ArtBas.ArtikkelNr).
                END.
                BildeRegister.BildNr = int(ArtBas.BildNr).
                ArtBas.BildNr = int(ArtBAs.ArtikkelNr).
                DISPLAY
                    ArtBAs.ArtikkelNr
                    ArtBAs.BildNr
                    .
            END.
            

        END.
    END.
END.
