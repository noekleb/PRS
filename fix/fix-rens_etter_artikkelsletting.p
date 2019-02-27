FOR EACH StrType WHERE StrTypeId > 999:
    DELETE StrType.
END.
FOR EACH EANNrListe WHERE EANNrListe.ArtikkelNr > 0:
    EANNrListe.ArtikkelNr = 0.
END.
FOR EACH ArtikkelNrSerie:
    DELETE ArtikkelNrSerie.
END.
