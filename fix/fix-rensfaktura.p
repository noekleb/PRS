DELETE FROM fakturahode.
DELETE FROM fakturalinje.
DELETE FROM kundereskontr.
DELETE FROM kundereskobling.
FOR EACH Kunde:
    ASSIGN
        Kundesaldo = 0
        ForsteKjop = ?
        Sistekjop = ?
        .
END.
