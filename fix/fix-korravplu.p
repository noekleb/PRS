FOR EACH ArtBas WHERE ArtBas.ArtikkelNr < 10000:
  ASSIGN
    ArtBas.OPris = TRUE
    ArtBas.Lager = FALSE
    ArtBas.Storrelser = TRUE
    ArtBAs.StrTypeId = 2
    .
END.
