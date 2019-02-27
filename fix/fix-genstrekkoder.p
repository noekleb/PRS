FOR EACH ArtBas NO-LOCK WHERE
  ArtBas.ArtikkelNr = 3117:
    RUN genStrekkode (ArtBas.ArtikkelNr,1,"""").
END.

