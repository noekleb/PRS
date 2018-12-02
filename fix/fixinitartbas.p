for each artbas:
  assign
    /* ArtBAs.ArtikkelNr     = NEXT-VALUE(ArtikkelNr) */
    ArtBas.aktivert       = true
    artbas.aktivdato      = (if ArtBAs.Ny_dato <> ? then ArtBAs.Ny_Dato else today)
    ArtBas.RegistrertDato = (if ArtBAs.Ny_dato <> ? then ArtBAs.Ny_Dato else today)
    ArtBas.RegistrertTid  = time
    ArtBas.RegistrertAV   = userid("dictdb")
    ArtBas.EDato          = (if ArtBAs.Ny_dato <> ? then ArtBAs.Ny_Dato else today)
    ArtBas.ETid           = time
    ArtBas.BrukerId       = userid("dictdb")
    ArtBas.LAger          = true
    ArtBas.Storrelser     = true
    .

end.
