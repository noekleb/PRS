FOR EACH Butiker NO-LOCK:
    PAUSE 0.
    DISPLAY
        Butiker.Butik
        .
    FOR EACH StLinje WHERE
        StLinje.Butik = Butiker.Butik AND
        StLinje.AarPerLinNr = 0:

    StLinje.Diverse = " " + StLinje.Diverse.
    StLinje.Diverse = TRIM(StLinje.Diverse).

    END.
END.
