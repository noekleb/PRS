DEF BUFFER bKasse FOR Kasse.
/*
FOR EACH Kasse WHERE Kasse.butikkNr > 1:
    DELETE Kasse.
END.
*/
FOR EACH Kasse WHERE Kasse.KasseNr < 10 AND Kasse.ButikkNr = 1 AND
    Kasse.ButikkNr <= 800:
    DISPLAY 
        Kasse.KasseNR
        Kasse.butikkNr.
    IF NOT CAN-FIND(Butiker WHERE Butiker.butik = Kasse.butikkNr) THEN
        DELETE Kasse.
    FOR EACH butiker WHERE
        Butiker.butik > 1:
        CREATE bKasse.
        BUFFER-COPY Kasse TO bKasse
            ASSIGN 
            bKasse.ButikkNr     = Butiker.Butik
            bKasse.Navn         = "Kasse " + STRING(Kasse.KasseNr) + 
                                  " " + Butiker.KortNavn
            bKasse.ElJournalId  = string(Butiker.butik) + ";" + 
                                  STRING(Kasse.KasseNr) + ";"
            bKasse.ElJournal[2] = string(Butiker.butik)
            .
    END.
END.
