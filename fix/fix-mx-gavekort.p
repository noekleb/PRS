FOR EACH Gavekort WHERE
    GAvekort.RegistrertDato < 04/18/2005:
    DELETE Gavekort.
END.
FOR EACH Gavekort:
    Gavekort.IdentType = 1.
END.

FOR EACH Tilgode WHERE
    Tilgode.RegistrertDato < 04/18/2005:
    DELETE Tilgode.
END.
