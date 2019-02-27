FOR EACH ArtBAs:
    FIND ArtPris OF ArtBas WHERE
        ArtPRis.ProfilNr = 1 NO-ERROR.
    IF AVAILABLE ArtPris THEN
    DO:
        ASSIGN
            ArtBas.PRodNr = ArtBAs.LEvNr
            ArtBas.KatalogPris = ArtPris.InnkjopsPris[1]
            ArtBAs.ForhRab%    = ArtPris.Rab1%[1]
            ArtBAs.SupRab%     = ArtPris.Rab1%[1]
            .
    END.
END.
