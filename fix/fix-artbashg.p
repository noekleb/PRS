FOR EACH ArtBAs:
    FIND VarGr OF ArtBas.
    IF (ArtBAs.HG <> VarGr.Hg) THEN
    DO:
        DISPLAY
            ArtBas.ArtikkelNr
            ArtBas.Beskr
            ArtBas.LevFargKod
            ArtBas.Vg
            VarGr.Hg
            .

        ArtBas.Hg = VarGr.Hg.
    END.
END.
