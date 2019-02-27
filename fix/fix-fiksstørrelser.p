FOR EACH ArtBas WHERE ArtBas.LAger = TRUE AND
  ArtBas.Storrelser = FALSE AND 
  ArtBas.OPRis = FALSE:

  IF ArtBas.StrTypeId > 1 THEN
    ArtBas.Storrelser = TRUE.

  DISPLAY
    ArtBas.ArtikkelNr
    ArtBAs.OPris
    ArtBas.Lager
    ArtBas.Storrelser
    ArtBas.StrTypeId
    .
END.
