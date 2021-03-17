FOR EACH ArtBas WHERE ArtBas.ArtikkelNr < 10000:
  FOR EACH Strekkode OF ArtBas WHERE
    LENGTH(TRIM(Strekkode.Kode)) > 5:
    DELETE strekkode.
  END.

END.
