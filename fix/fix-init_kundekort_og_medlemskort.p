DEF VAR X AS INT.
FOR EACH KundeKort:
    X = X + 1.
    KundeKort.InterntKKortId = X.
END.

DEF VAR X AS INT.
FOR EACH Medlemskort:
    X = X + 1.
    Medlemskort.InterntKKortId = X.
END.

