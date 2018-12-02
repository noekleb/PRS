FOR EACH kundekort:
    CREATE medlemskort.
    BUFFER-COPY Kundekort TO MedlemsKort
        ASSIGN
        MedlemsKort.MedlemsNr = Kunde.KundeNr
        MedlemsKort.KortType  = 2 /* Kundekort. */
        .
END.
