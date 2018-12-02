FOR EACH Butiker NO-LOCK:
    FIND KjedensButikker EXCLUSIVE-LOCK WHERE
        KjedensButikker.ButikkNr = Butiker.Butik NO-ERROR.
    IF NOT AVAILABLE KjedensButikker THEN
    DO:
        CREATE KjedensButikker.
        ASSIGN
            Kjedensbutikker.ButikkNr   = Butiker.Butik
            KjedensButikker.KjedeNr    = 1
            KjedensButikker.RegionNr   = 1
            KjedensButikker.DistriktNr = 1
            .
    END.
    ASSIGN
      Kjedensbutikker.ButikkNavn         = Butiker.ButNamn
      
      Kjedensbutikker.Kontaktperson      = ""
      Kjedensbutikker.E-Mail             = Butiker.ePostAdresse
      Kjedensbutikker.Telefon            = Butiker.BuTel
      Kjedensbutikker.Telefaks           = Butiker.Telefaks
      Kjedensbutikker.PostNr             = Butiker.BuPoNr
      Kjedensbutikker.Firmanavn          = Butiker.ButFirmanavn
      Kjedensbutikker.DagligLeder        = Butiker.BuKon
      
      Kjedensbutikker.Adresse1           = Butiker.BuAdr
      Kjedensbutikker.OrganisasjonsNr    = Butiker.OrganisasjonsNr
      Kjedensbutikker.OppstartButikkdata = Butiker.ApningsDato
      Kjedensbutikker.UtmeldtDato        = Butiker.NedlagtDato
      
      Kjedensbutikker.FakturaAdresse1    = Butiker.FakturaAdresse1
      Kjedensbutikker.FakturaAdresse2    = Butiker.FakturaAdresse2
      Kjedensbutikker.FakturaPostNr      = Butiker.FakturaPostNr
      Kjedensbutikker.FakturaPostBoks    = Butiker.FakturaPostBoks
        .
END.
