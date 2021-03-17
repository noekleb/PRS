FOR EACH ArtBAs:
    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = ArtBAs.Vg NO-ERROR.
    IF AVAILABLE VarGr AND ArtBas.Hg <> VarGr.Hg THEN
    DO:
        ArtBas.Hg = VarGr.Hg.

        DISPLAY
            ArtBAs.ArtikkelNR
            ArtBAs.Beskr
            ArtBAs.Vg
            ArtBAs.Hg.
    END.
END.
