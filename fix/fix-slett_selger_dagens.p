FOR EACH Selger EXCLUSIVE-LOCK WHERE RegistrertDato = TODAY:
    DISPLAY
        Selger.SelgerNr
        Selger.Navn
        Selger.RegistrertDato
        .
    FOR EACH ButikkSelger OF Selger EXCLUSIVE-LOCK:
        DELETE ButikkSelger.
    END.

    DELETE Selger.
END.
