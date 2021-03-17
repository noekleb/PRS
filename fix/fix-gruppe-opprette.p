FOR EACH Butiker NO-LOCK:
    CREATE Gruppe.
    ASSIGN
        Gruppe.ButikkNr = Butiker.Butik
        Gruppe.GruppeNr = 1
        Gruppe.Navn     = "Gruppe 1"
        .
END.
