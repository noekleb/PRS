CURRENT-WINDOW:WIDTH = 300.
FOR EACH kampanjehode NO-LOCK WHERE
    KampanjeHode.KampanjeId = 128,
    EACH KampanjeLinje OF KampanjeHode NO-LOCK,
    EACH ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr,
    EACH PrisKo EXCLUSIVE-LOCK WHERE
        Prisko.ArtikkelNr = KampanjeLinje.ArtikkelNr AND
        PrisKo.Tilbud = TRUE AND
        PrisKo.TYPE       = 3 AND 
        PrisKo.AktiveresDato = 05/20/2012:

    ASSIGN
        PrisKo.AktiveresDato = TODAY
        PrisKo.AktiveresTid  = 0.
        .
    /*
    DISPLAY
        KampanjeHode.ProfilNr
        KampanjeHode.KampanjeId
        KampanjeHode.Beskrivelse
        KampanjeLinje.ArtikkelNr
        ArtBas.Beskr
        PrisKo.TYPE
        PrisKo.Tilbud
        PrisKo.AktiveresDato
        STRING(PrisKo.AktiveresTid,"HH:MM")
        PrisKo.GyldigTilDato
        WITH WIDTH 300.
    */
END.
