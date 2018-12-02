FOR EACH ArtBAs NO-LOCK /*WHERE
    ArtBas.ArtikkelNR = 10800*/:

    RUN genStrekkode.p (ArtBas.ARtikkelNr, 1,"").
END.
