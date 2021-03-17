FOR EACH Selger:
    FOR EACH Butiker WHERE Butiker.Butik < 20:
        CREATE ButikkSelger.
        ASSIGN
            ButikkSelger.Butik      = butiker.Butik
            ButikkSelger.SelgerNr   = Selger.SelgerNr
            ButikkSelger.SelgerId = Selger.SelgerNr
            .
    END.
END.
