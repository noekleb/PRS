FOR EACH Strekkode WHERE Kode = '7325701492750',
    FIRST StrKonv OF StrekKode NO-LOCK,
    FIRST ArtLag WHERE ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND ArtLag.butik = 4 AND ArtLag.storl = StrKonv.Storl,
    FIRST Lager WHERE Lager.ArtikkelNr = StrekKode.ArtikkelNr AND Lager.Butik = ArtLag.butik:

    DISPLAY
        Strekkode.Kode
        Lager.Lagant
        ArtLag.Lagant
        .

END.
