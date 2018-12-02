FOR EACH ArtBAs NO-LOCK WHERE
    ArtBas.ArtikkelNr >= 900000 AND ArtBas.ArtikkelNr <= 900199:

    IF NOT CAN-FIND(Strekkode WHERE
                    Strekkode.Kode = STRING(ArtBas.ArtikkelNr)) THEN
    DO:
        CREATE Strekkode.
        ASSIGN
            Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
            Strekkode.Kode       = STRING(ArtBas.ArtikkelNr)
            Strekkode.VareId     = ArtBas.ArtikkelNr
            Strekkode.HovedNr    = TRUE
            Strekkode.iKasse     = FALSE
            .
    END.
END.
