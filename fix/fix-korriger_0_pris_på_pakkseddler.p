CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Ut.

DEF BUFFER bufArtPris FOR ArtPris.

OUTPUT STREAM Ut TO VALUE('konv\pksdler_ute_pris13082019.csv').

PUT STREAM Ut UNFORMATTED
        'PkSdlNr;'
        'PkSdlId;'
        'ButikkNr;'
        'ArtikkelNr;'
        'NyPris'
    SKIP.

FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10, 
    EACH PkSdlPris OF PkSdlHode WHERE 
             PkSdlPris.NyPris = 0,
    FIRST PkSdlLinje OF PkSdlHode:

    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = PkSdlLinje.butikkNr.
    FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
        ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
    FIND bufArtPris NO-LOCK WHERE 
        bufArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
        bufArtPris.ProfilNr = 1 NO-ERROR.

    IF ArtPris.Pris[1] > 1 THEN
    DO:
        DISPLAY
            PkSdlHode.PkSdlNr
            PkSdlHode.PkSdlId
            PkSdlLinje.ButikkNr
            PkSdlLinje.ArtikkelNr
            PkSdlPris.NyPris
            '|'
            ArtPris.Pris[1]
            ArtPris.VareKost[1]
            '|'
            bufArtPris.Pris[1]
            bufArtPris.VareKost[1]
            WITH WIDTH 350.

        ASSIGN 
            PkSdlPris.NyPris         = ArtPris.Pris[1]
            PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]
            PkSdlPris.NyRab1%        = ArtPris.Rab1%[1]
            PkSdlPris.NyVarekost     = ArtPris.VareKost[1]
            PkSdlPris.NyDb%          = ArtPris.Db%[1]
            .
    END.
    PUT STREAM Ut UNFORMATTED
        PkSdlHode.PkSdlNr ';'
        PkSdlHode.PkSdlId ';'
        PkSdlLinje.ButikkNr ';'
        PkSdlLinje.ArtikkelNr ';'
        PkSdlPris.NyPris
        SKIP.
END.
